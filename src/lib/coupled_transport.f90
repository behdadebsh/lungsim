! Orchestration of Ruobing's surfactant-only and staged lymphatic/surfactant protocols.
module coupled_transport
  use precision, only: dp
  use arrays, only: num_units, num_elems, units, unit_field
  use indices, only: num_nu, nu_vol, nu_comp, nu_pe, nu_Pe_max, nu_Pe_min
  use parameter_types, only: V_params, solve_V_params, coupled_lymphatic_params
  use surfactant, only: surfactant_state, initialise_surface, advance_surface, validate_surface_parameters
  use coupled_lymphatics, only: fluid_state, initialise_fluid, advance_fluid, validate_fluid_parameters, &
       fluid_running, sample_fluid_convergence, fluid_stop_status
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: coupled_active, fluid_active, load_coupled_capillary, prepare_coupling, initialise_coupling
  public :: advance_coupled_surface, advance_coupled_fluid, add_surface_mechanics, release_coupling
  public :: settle_coupled_fluid, reset_coupled_surface, finish_coupling, record_coupled_stage
  public :: start_coupled_log, get_coupled_terminal, export_coupled
  logical, protected :: coupled_active = .false., fluid_active = .false.
  logical :: global_limit_reached = .false., results_ready = .false., log_open = .false.
  logical :: capillary_imported = .false.
  integer :: sample_steps = 0, log_unit
  type(surfactant_state), allocatable :: surface(:)
  type(fluid_state), allocatable :: fluid(:)
  real(dp), allocatable :: capillary(:,:) ! pressure Pa, sheet area mm^2, transit time s
  integer, allocatable :: input_units(:)

contains

  subroutine check_airway_layout()
    if (num_nu /= 12 .or. nu_vol /= 1 .or. nu_pe /= 7) error stop 'Set ventilation_indices before coupled ventilation'
    if (.not. allocated(units) .or. .not. allocated(unit_field)) error stop 'Load airway geometry and append_units first'
    if (num_units < 1 .or. size(unit_field,2) /= num_units .or. size(unit_field,1) /= num_nu) &
         error stop 'Inconsistent coupled ventilation unit allocation'
  end subroutine check_airway_layout

  subroutine load_coupled_capillary(capillary_file)
    character(len=*), intent(in) :: capillary_file
    integer :: io, ios, terminal, u
    integer, allocatable :: unit_by_element(:)
    logical, allocatable :: seen(:)
    real(dp) :: pressure, area, transit
    character(len=1024) :: line
    call release_coupling()
    call check_airway_layout()
    allocate(capillary(3,num_units),input_units(num_units),unit_by_element(num_elems),seen(num_units))
    input_units = units
    capillary = 0.0_dp
    unit_by_element = 0
    seen = .false.
    do u = 1,num_units
       unit_by_element(units(u)) = u
    enddo
    open(newunit=io,file=trim(capillary_file),status='old',action='read',iostat=ios)
    if (ios /= 0) error stop 'Cannot open mapped coupled capillary input'
    do
       read(io,'(a)',iostat=ios) line
       if (ios < 0) exit
       if (ios /= 0) error stop 'Cannot read mapped capillary input'
       line = adjustl(line)
       if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
       read(line,*,iostat=ios) terminal,pressure,area,transit
       if (ios /= 0) error stop 'Expected terminal_element pressure_Pa area_mm2 transit_s'
       if (terminal < 1 .or. terminal > num_elems) error stop 'Invalid capillary input terminal element'
       u = unit_by_element(terminal)
       if (u == 0) error stop 'Capillary input element is not an airway terminal'
       if (seen(u)) error stop 'Duplicate airway terminal in capillary input'
       if (.not. all(ieee_is_finite([pressure,area,transit]))) error stop 'Non-finite capillary input'
       if (area < 0.0_dp .or. transit < 0.0_dp) error stop 'Negative capillary area or transit time'
       if (area > 0.0_dp .and. transit <= 0.0_dp) error stop 'Exchanging units require positive transit time'
       capillary(:,u) = [pressure,area,transit]
       seen(u) = .true.
    enddo
    close(io)
    if (.not. all(seen)) error stop 'Capillary input must contain every airway terminal exactly once'
    capillary_imported = .true.
  end subroutine load_coupled_capillary

  subroutine prepare_coupling(model)
    character(len=*), intent(in) :: model
    call check_airway_layout()
    select case (trim(model))
    case ('surfactant')
       fluid_active = .false.
       capillary_imported = .false.
       if (allocated(capillary)) deallocate(capillary)
       if (allocated(input_units)) deallocate(input_units)
       allocate(capillary(3,num_units),input_units(num_units))
       capillary = 0.0_dp
       input_units = units
    case ('lymphatic_surfactant')
       fluid_active = .true.
       if (.not. capillary_imported) error stop 'Call import_coupled_capillary before lymphatic_surfactant'
       if (size(capillary,2) /= num_units) error stop 'Reimport capillary inputs after changing geometry'
       if (any(input_units /= units)) error stop 'Reimport capillary inputs after changing terminals'
    case default
       error stop 'Coupled model must be surfactant or lymphatic_surfactant'
    end select
    if (.not. all(ieee_is_finite([V_params%T_interval,solve_V_params%dt]))) error stop 'Non-finite ventilation timestep'
    if (solve_V_params%dt <= 0.0_dp .or. V_params%T_interval <= 0.0_dp .or. &
         solve_V_params%dt > V_params%T_interval .or. solve_V_params%num_breaths < 1) &
         error stop 'Invalid coupled ventilation duration or timestep'
    call validate_surface_parameters()
    if (fluid_active) call validate_fluid_parameters()
    if (allocated(surface)) deallocate(surface)
    if (allocated(fluid)) deallocate(fluid)
    allocate(surface(num_units),fluid(num_units))
    sample_steps = 0
    global_limit_reached = .false.
    results_ready = .false.
    coupled_active = .true.
  end subroutine prepare_coupling

  subroutine initialise_coupling()
    integer :: u
    call reset_coupled_surface()
    do u = 1,num_units
       if (fluid_active) call initialise_fluid(fluid(u),num_units)
    enddo
    results_ready = .true.
  end subroutine initialise_coupling

  subroutine reset_coupled_surface()
    integer :: u
    do u = 1,num_units
       call initialise_surface(surface(u),unit_field(nu_vol,u))
    enddo
  end subroutine reset_coupled_surface

  subroutine advance_coupled_surface(dt)
    real(dp), intent(in) :: dt
    integer :: u
    do u = 1,num_units
       call advance_surface(surface(u),unit_field(nu_vol,u),fluid(u)%alveolar > 0.0_dp,dt)
    enddo
  end subroutine advance_coupled_surface

  subroutine add_surface_mechanics(u)
    integer, intent(in) :: u
    ! Retained source prescription; not the full tangent of the dynamic surface law.
    unit_field(nu_comp,u) = 1.0_dp/(1.0_dp/unit_field(nu_comp,u)+1.0_dp/surface(u)%compliance)
    unit_field(nu_pe,u) = unit_field(nu_pe,u)+surface(u)%pressure
  end subroutine add_surface_mechanics

  subroutine advance_coupled_fluid(dt, time_start)
    real(dp), intent(in) :: dt, time_start
    real(dp) :: active_dt
    integer :: u
    if (.not. fluid_active .or. global_limit_reached) return
    do u = 1,num_units
       if (.not. fluid_running(fluid(u),capillary(3,u))) cycle
       active_dt = min(dt,coupled_lymphatic_params%maximum_transit_times*capillary(3,u)-fluid(u)%elapsed)
       if (active_dt <= 0.0_dp) cycle
       call advance_fluid(fluid(u),capillary(1,u),capillary(2,u), &
            unit_field(nu_Pe_max,u)-unit_field(nu_Pe_min,u),V_params%T_interval,active_dt,time_start)
       if (active_dt < dt) fluid(u)%elapsed = coupled_lymphatic_params%maximum_transit_times*capillary(3,u)
    enddo
    sample_steps = sample_steps+1
    if (sample_steps >= coupled_lymphatic_params%convergence_check_steps) then
       do u = 1,num_units
          call sample_fluid_convergence(fluid(u))
       enddo
       sample_steps = 0
    endif
  end subroutine advance_coupled_fluid

  logical function any_fluid_running()
    integer :: u
    any_fluid_running = .false.
    do u = 1,num_units
       if (fluid_running(fluid(u),capillary(3,u))) then
          any_fluid_running = .true.
          return
       endif
    enddo
  end function any_fluid_running

  subroutine settle_coupled_fluid(time)
    real(dp), intent(inout) :: time
    real(dp) :: limit, dt
    call record_coupled_stage('fluid_settling_start',time)
    limit = real(coupled_lymphatic_params%maximum_settling_breaths,dp)*V_params%T_interval
    do while (any_fluid_running())
       if (time >= limit) then
          global_limit_reached = .true.
          exit
       endif
       dt = min(solve_V_params%dt,limit-time)
       call advance_coupled_fluid(dt,time)
       time = time+dt
       if (sample_steps == 0) call record_coupled_stage('fluid_settling_progress',time)
    enddo
    call record_coupled_stage('fluid_settling_end',time)
  end subroutine settle_coupled_fluid

  subroutine start_coupled_log(filename)
    character(len=*), intent(in) :: filename
    integer :: ios
    if (log_open) close(log_unit)
    open(newunit=log_unit,file=trim(filename)//'.opcoupled',status='replace',action='write',iostat=ios)
    if (ios /= 0) error stop 'Cannot open coupled stage log'
    log_open = .true.
    write(log_unit,'(a)') '# stage absolute_time_s running saturation_converged transit_capped global_capped excluded'
  end subroutine start_coupled_log

  subroutine record_coupled_stage(stage, time)
    character(len=*), intent(in) :: stage
    real(dp), intent(in) :: time
    integer :: counts(0:4), u, status
    counts = 0
    if (fluid_active) then
       do u = 1,num_units
          status = fluid_stop_status(fluid(u),capillary(3,u))
          if (status == 0 .and. global_limit_reached) status = 3
          counts(status) = counts(status)+1
       enddo
    endif
    write(*,'(a,1x,f14.4,5(1x,i0))') trim(stage),time,counts
    if (log_open) then
       write(log_unit,'(a,1x,f14.4,5(1x,i0))') trim(stage),time,counts
       flush(log_unit)
    endif
  end subroutine record_coupled_stage

  subroutine get_coupled_terminal(u, values)
    integer, intent(in) :: u
    real(dp), intent(out) :: values(18)
    integer :: status
    if (.not. results_ready) error stop 'Run evaluate_vent_coupled before exporting coupled results'
    if (size(surface) /= num_units) error stop 'Coupled results geometry changed'
    if (input_units(u) /= units(u)) error stop 'Coupled results terminals changed'
    values = 0.0_dp
    values(1:5) = [surface(u)%gamma,surface(u)%tension,surface(u)%pressure,surface(u)%compliance,fluid(u)%alveolar]
    values(18) = -1.0_dp
    if (fluid_active) then
       values(6) = (fluid(u)%volume_a+fluid(u)%volume_b)/(fluid(u)%capacity_a+fluid(u)%capacity_b)
       values(7) = fluid(u)%elapsed
       if (fluid(u)%elapsed > 0.0_dp) values(8:9) = [fluid(u)%filtered,fluid(u)%drained]/fluid(u)%elapsed
       values(17) = fluid(u)%saturation_error
       status = fluid_stop_status(fluid(u),capillary(3,u))
       if (status == 0 .and. global_limit_reached) status = 3
       values(18) = real(status,dp)
    endif
    values(10:13) = [capillary(1,u),capillary(1,u)*coupled_lymphatic_params%pressure_multiplier,capillary(2:3,u)]
    values(14:16) = [fluid(u)%protein_a+fluid(u)%protein_b,fluid(u)%protein_alveolar,fluid(u)%protein_drained]
  end subroutine get_coupled_terminal

  subroutine export_coupled(filename)
    character(len=*), intent(in) :: filename
    integer :: io, ios, u
    real(dp) :: values(18)
    if (.not. results_ready) error stop 'Run evaluate_vent_coupled before exporting coupled results'
    open(newunit=io,file=trim(filename),status='replace',action='write',iostat=ios)
    if (ios /= 0) error stop 'Cannot open coupled CSV results'
    write(io,'(a)') 'terminal_element,gamma_g_cm2,tension_dyn_cm,surface_pressure_Pa,surface_compliance_mm3_Pa,'// &
         'alveolar_fluid_mm3,interstitial_saturation,elapsed_fluid_s,mean_filtration_mm3_s,mean_lymph_mm3_s,'// &
         'input_cap_pressure_Pa,effective_cap_pressure_Pa,cap_area_mm2,transit_time_s,'// &
         'interstitial_protein_mg,alveolar_protein_mg,drained_protein_mg,saturation_error,stop_status'
    do u = 1,num_units
       call get_coupled_terminal(u,values)
       write(io,'(i0,18(",",es24.16))') units(u),values
    enddo
    close(io)
  end subroutine export_coupled

  subroutine finish_coupling()
    ! Keep completed results for the usual explicit export calls.
    coupled_active = .false.
    if (log_open) close(log_unit)
    log_open = .false.
  end subroutine finish_coupling

  subroutine release_coupling()
    call finish_coupling()
    if (allocated(surface)) deallocate(surface)
    if (allocated(fluid)) deallocate(fluid)
    if (allocated(capillary)) deallocate(capillary)
    if (allocated(input_units)) deallocate(input_units)
    fluid_active = .false.
    results_ready = .false.
    capillary_imported = .false.
  end subroutine release_coupling
end module coupled_transport
