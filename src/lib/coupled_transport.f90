! Orchestration for the opt-in Ruobing-derived ventilation/fluid/surfactant model.
! State is independent of unit_field's perfusion/lymphatic indices.
module coupled_transport
  use precision, only: dp
  use arrays, only: num_units, num_elems, units, unit_field
  use indices, only: num_nu, nu_vol, nu_comp, nu_pe, nu_Pe_max, nu_Pe_min
  use parameter_types, only: V_params, solve_V_params, coupled_lymphatic_params
  use surfactant, only: surfactant_state, initialise_surface, advance_surface, validate_surface_parameters
  use coupled_lymphatics, only: fluid_state, initialise_fluid, advance_fluid, validate_fluid_parameters
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: coupled_active, prepare_coupling, initialise_coupling, advance_coupled_surface
  public :: advance_coupled_fluid, add_surface_mechanics, export_coupled, release_coupling
  logical, protected :: coupled_active = .false.
  logical :: fluid_active = .false.
  type(surfactant_state), allocatable :: surface(:)
  type(fluid_state), allocatable :: fluid(:)
  real(dp), allocatable :: capillary(:,:) ! pressure Pa, sheet area mm^2, transit time s

contains

  subroutine prepare_coupling(capillary_file)
    character(len=*), intent(in) :: capillary_file
    integer :: io, ios, terminal, u, nrows
    integer, allocatable :: unit_by_element(:)
    logical, allocatable :: seen(:)
    real(dp) :: pressure, area, transit
    character(len=1024) :: line
    call release_coupling()
    ! Only an airway model is supported. Do not reinterpret a perfusion array.
    if (num_nu /= 12 .or. nu_vol /= 1 .or. nu_pe /= 7) error stop 'Set ventilation_indices before coupled ventilation'
    if (.not. allocated(units) .or. .not. allocated(unit_field)) error stop 'Load airway geometry and append_units first'
    if (num_units < 1 .or. size(unit_field,2) /= num_units .or. size(unit_field,1) /= num_nu) &
         error stop 'Inconsistent coupled ventilation unit allocation'
    if (.not. all(ieee_is_finite([V_params%T_interval,solve_V_params%dt]))) error stop 'Non-finite ventilation timestep'
    if (solve_V_params%dt <= 0.0_dp .or. V_params%T_interval <= 0.0_dp .or. &
         solve_V_params%dt > V_params%T_interval .or. solve_V_params%num_breaths < 1) &
         error stop 'Invalid coupled ventilation duration or timestep'
    call validate_surface_parameters()
    fluid_active = len_trim(capillary_file) > 0
    if (fluid_active) call validate_fluid_parameters()
    allocate(surface(num_units),fluid(num_units),capillary(3,num_units))
    capillary = 0.0_dp
    if (fluid_active) then
       allocate(unit_by_element(num_elems),seen(num_units))
       unit_by_element = 0
       seen = .false.
       do u = 1,num_units
          unit_by_element(units(u)) = u
       enddo
       open(newunit=io,file=trim(capillary_file),status='old',action='read',iostat=ios)
       if (ios /= 0) error stop 'Cannot open mapped coupled capillary input'
       nrows = 0
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
          capillary(:,u) = [pressure,area,transit]
          seen(u) = .true.
          nrows = nrows+1
       enddo
       close(io)
       if (nrows /= num_units) error stop 'Capillary input must contain every airway terminal exactly once'
    endif
    coupled_active = .true.
  end subroutine prepare_coupling

  subroutine initialise_coupling()
    integer :: u
    do u = 1,num_units
       call initialise_surface(surface(u),unit_field(nu_vol,u))
       if (fluid_active) call initialise_fluid(fluid(u),num_units)
    enddo
  end subroutine initialise_coupling

  subroutine advance_coupled_surface(dt)
    real(dp), intent(in) :: dt
    integer :: u
    do u = 1,num_units
       call advance_surface(surface(u),unit_field(nu_vol,u),fluid(u)%alveolar > 0.0_dp,dt)
    enddo
  end subroutine advance_coupled_surface

  subroutine add_surface_mechanics(u)
    integer, intent(in) :: u
    ! Ruobing's constitutive choice: additive recoil pressure and reciprocal
    ! compliance. This is not a re-derivation of the full surfactant tangent.
    unit_field(nu_comp,u) = 1.0_dp/(1.0_dp/unit_field(nu_comp,u)+1.0_dp/surface(u)%compliance)
    unit_field(nu_pe,u) = unit_field(nu_pe,u)+surface(u)%pressure
  end subroutine add_surface_mechanics

  subroutine advance_coupled_fluid(dt)
    real(dp), intent(in) :: dt
    integer :: u
    if (.not. fluid_active) return
    do u = 1,num_units
       call advance_fluid(fluid(u),capillary(1,u),capillary(2,u), &
            unit_field(nu_Pe_max,u)-unit_field(nu_Pe_min,u),V_params%T_interval,dt)
    enddo
  end subroutine advance_coupled_fluid

  subroutine export_coupled(filename)
    character(len=*), intent(in) :: filename
    integer :: io, ios, u
    real(dp) :: saturation, mean_flux, mean_lymph, effective_pressure
    open(newunit=io,file=trim(filename)//'.coupled.csv',status='replace',action='write',iostat=ios)
    if (ios /= 0) error stop 'Cannot open coupled results file'
    write(io,'(a)') 'terminal_element,gamma_g_cm2,tension_dyn_cm,surface_pressure_Pa,surface_compliance_mm3_Pa,'// &
         'alveolar_fluid_mm3,interstitial_saturation,elapsed_fluid_s,mean_filtration_mm3_s,mean_lymph_mm3_s,'// &
         'input_cap_pressure_Pa,effective_cap_pressure_Pa,cap_area_mm2,transit_time_s,'// &
         'interstitial_protein_mg,alveolar_protein_mg,drained_protein_mg'
    do u = 1,num_units
       saturation = 0.0_dp
       mean_flux = 0.0_dp
       mean_lymph = 0.0_dp
       if (fluid_active) then
          saturation = (fluid(u)%volume_a+fluid(u)%volume_b)/(fluid(u)%capacity_a+fluid(u)%capacity_b)
          if (fluid(u)%elapsed > 0.0_dp) then
             mean_flux = fluid(u)%filtered/fluid(u)%elapsed
             mean_lymph = fluid(u)%drained/fluid(u)%elapsed
          endif
       endif
       effective_pressure = capillary(1,u)*coupled_lymphatic_params%pressure_multiplier
       write(io,'(i0,16(",",es24.16))') units(u),surface(u)%gamma,surface(u)%tension, &
            surface(u)%pressure,surface(u)%compliance,fluid(u)%alveolar,saturation,fluid(u)%elapsed, &
            mean_flux,mean_lymph,capillary(1,u),effective_pressure,capillary(2,u),capillary(3,u), &
            fluid(u)%protein_a+fluid(u)%protein_b,fluid(u)%protein_alveolar,fluid(u)%protein_drained
    enddo
    close(io)
  end subroutine export_coupled

  subroutine release_coupling()
    if (allocated(surface)) deallocate(surface)
    if (allocated(fluid)) deallocate(fluid)
    if (allocated(capillary)) deallocate(capillary)
    coupled_active = .false.
    fluid_active = .false.
  end subroutine release_coupling
end module coupled_transport
