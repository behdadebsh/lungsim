! Ruobing surfactant and coupled fluid/protein model.
! Surface equations derive from ven_surf a681432 and Lym_surf 143d819.
! Fluid/protein equations derive from Lym_surf 143d819, lymphatics.f90::alveolar_flux.
! Engineering orchestration is kept here so the complete opt-in model has one module.
module surfactant
  use precision, only: dp
  use arrays, only: num_units, num_elems, units, unit_field
  use indices, only: num_nu, nu_vol, nu_comp, nu_pe, nu_Pe_max, nu_Pe_min
  use parameter_types, only: V_params, solve_V_params, surfactant_params, lymphatic_params, coupled_lymphatic_params
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: surfactant_state, initialise_surface, advance_surface, validate_surface_parameters
  public :: surface_tension
  public :: fluid_state, initialise_fluid, advance_fluid, validate_fluid_parameters
  public :: fluid_running, sample_fluid_convergence, fluid_stop_status
  public :: coupled_active, fluid_active, load_coupled_capillary, prepare_coupling, initialise_coupling
  public :: advance_coupled_surface, advance_coupled_fluid, add_surface_mechanics, release_coupling
  public :: settle_coupled_fluid, reset_coupled_surface, finish_coupling, record_coupled_stage
  public :: start_coupled_log, get_coupled_terminal, export_coupled

  type :: surfactant_state
     real(dp) :: area = 0.0_dp         ! cm^2 per alveolus (hemisphere)
     real(dp) :: gamma = 0.0_dp        ! g/cm^2
     real(dp) :: tension = 0.0_dp      ! dyn/cm
     real(dp) :: pressure = 0.0_dp     ! Pa
     real(dp) :: compliance = 0.0_dp   ! mm^3/Pa per terminal unit
  end type surfactant_state


  ! Per-terminal state for Ruobing's fluid/protein transport equations.
  type :: fluid_state
     real(dp) :: capacity_a = 0.0_dp, capacity_b = 0.0_dp ! mm^3
     real(dp) :: volume_a = 0.0_dp, volume_b = 0.0_dp, alveolar = 0.0_dp ! mm^3
     real(dp) :: protein_a = 0.0_dp, protein_b = 0.0_dp, protein_alveolar = 0.0_dp ! mg
     real(dp) :: filtered = 0.0_dp, drained = 0.0_dp ! cumulative mm^3
     real(dp) :: protein_filtered = 0.0_dp, protein_drained = 0.0_dp ! cumulative mg
     real(dp) :: elapsed = 0.0_dp
     ! Source initial history deliberately prevents declaring convergence before
     ! enough samples exist. History is sampled every convergence_check_steps.
     real(dp) :: saturation_history(5) = [1.0_dp,2.0_dp,3.0_dp,4.0_dp,5.0_dp]
     real(dp) :: saturation_error = 0.5_dp
  end type fluid_state

  ! State used to coordinate the explicit surfactant and lymphatic_surfactant modes.
  logical, protected :: coupled_active = .false., fluid_active = .false.
  logical :: global_limit_reached = .false., results_ready = .false., log_open = .false.
  logical :: capillary_imported = .false.
  integer :: sample_steps = 0, log_unit
  type(surfactant_state), allocatable :: surface(:)
  type(fluid_state), allocatable :: fluid(:)
  real(dp), allocatable :: capillary(:,:) ! pressure Pa, sheet area mm^2, transit time s
  integer, allocatable :: input_units(:)

contains

  subroutine validate_surface_parameters()
    associate(p => surfactant_params)
      if (.not. all(ieee_is_finite([p%gamma_star,p%tension_clean,p%tension_hat,p%tension_min, &
           p%m2,p%adsorption_rate,p%desorption_rate,p%bulk_normal,p%bulk_flooded, &
           p%initial_gamma_ratio,p%alveoli_per_unit]))) error stop 'Non-finite surfactant parameter'
      if (p%gamma_star <= 0.0_dp .or. p%m2 <= 0.0_dp .or. p%alveoli_per_unit <= 0.0_dp) &
           error stop 'Surfactant concentration scale, slope and alveoli count must be positive'
      if (p%tension_min <= 0.0_dp .or. p%tension_hat < p%tension_min .or. &
           p%tension_clean < p%tension_hat) error stop 'Invalid surfactant tension limits'
      if (min(p%adsorption_rate,p%desorption_rate,p%bulk_normal,p%bulk_flooded,p%initial_gamma_ratio) < 0.0_dp) &
           error stop 'Negative surfactant kinetic parameter'
      if (p%initial_gamma_ratio > gamma_max()/p%gamma_star) error stop 'Initial gamma exceeds gamma_max'
    end associate
  end subroutine validate_surface_parameters

  real(dp) function gamma_max()
    associate(p => surfactant_params)
      gamma_max = p%gamma_star*(1.0_dp+(p%tension_hat-p%tension_min)/p%m2)
    end associate
  end function gamma_max

  real(dp) function surface_tension(gamma)
    real(dp), intent(in) :: gamma
    associate(p => surfactant_params)
      if (gamma < p%gamma_star) then
         surface_tension = (p%tension_hat-p%tension_clean)*max(0.0_dp,gamma)/p%gamma_star+p%tension_clean
      else
         surface_tension = -p%m2*min(gamma,gamma_max())/p%gamma_star+p%m2+p%tension_hat
      endif
    end associate
  end function surface_tension

  subroutine surface_geometry(volume, radius, area)
    real(dp), intent(in) :: volume
    real(dp), intent(out) :: radius, area
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    if (.not. ieee_is_finite(volume) .or. volume <= 0.0_dp) error stop 'Non-positive/non-finite acinar volume'
    radius = (3.0_dp*volume/(1000.0_dp*surfactant_params%alveoli_per_unit*2.0_dp*pi))**(1.0_dp/3.0_dp)
    area = 2.0_dp*pi*radius**2
  end subroutine surface_geometry

  subroutine initialise_surface(state, volume)
    type(surfactant_state), intent(out) :: state
    real(dp), intent(in) :: volume
    real(dp) :: radius
    call surface_geometry(volume,radius,state%area)
    state%gamma = surfactant_params%initial_gamma_ratio*surfactant_params%gamma_star
    call update_mechanics(state,volume,radius)
  end subroutine initialise_surface

  subroutine advance_surface(state, volume, flooded, dt)
    type(surfactant_state), intent(inout) :: state
    real(dp), intent(in) :: volume, dt
    logical, intent(in) :: flooded
    real(dp) :: radius, area, darea, bulk, rate, h, a, old_area
    integer :: i, steps
    if (.not. ieee_is_finite(dt) .or. dt <= 0.0_dp) error stop 'Surfactant dt must be positive and finite'
    call surface_geometry(volume,radius,area)
    old_area = state%area
    if (old_area <= 0.0_dp) error stop 'Initialise surfactant before stepping'
    darea = (area-old_area)/dt
    bulk = surfactant_params%bulk_normal
    if (flooded) bulk = surfactant_params%bulk_flooded
    ! Same explicit rate equation as the source. Substep fast adsorption/area
    ! changes to prevent negative concentrations at larger ventilation timesteps.
    rate = surfactant_params%adsorption_rate*bulk+surfactant_params%desorption_rate+ &
         abs(darea)/min(area,old_area)
    if (.not. ieee_is_finite(rate) .or. dt*rate > 1.0e5_dp) error stop 'Surfactant step too large'
    steps = max(1,ceiling(dt*rate/0.1_dp))
    h = dt/real(steps,dp)
    do i = 1,steps
       a = old_area+darea*h*real(i,dp)
       if (state%gamma < surfactant_params%gamma_star) then
          state%gamma = state%gamma+h*(surfactant_params%adsorption_rate*bulk* &
               (surfactant_params%gamma_star-state%gamma)-surfactant_params%desorption_rate*state%gamma- &
               state%gamma*darea/a)
       else
          state%gamma = state%gamma-h*state%gamma*darea/a
       endif
       state%gamma = max(0.0_dp,min(gamma_max(),state%gamma))
    enddo
    state%area = area
    call update_mechanics(state,volume,radius)
  end subroutine advance_surface

  subroutine update_mechanics(state, volume, radius)
    type(surfactant_state), intent(inout) :: state
    real(dp), intent(in) :: volume, radius
    state%tension = surface_tension(state%gamma)
    state%pressure = 2.0_dp*state%tension/radius/10.0_dp ! dyn/cm^2 -> Pa
    state%compliance = 3.0_dp*volume/state%pressure
  end subroutine update_mechanics

  ! Fluid/protein transport adapted from Ruobing Lym_surf::alveolar_flux.

  subroutine validate_fluid_parameters()
    associate(p => coupled_lymphatic_params, l => lymphatic_params)
      if (.not. all(ieee_is_finite([p%lung_mass_g,p%pressure_multiplier,p%conductivity_multiplier, &
           p%protein_reflection,p%protein_permeability,p%protein_convection_fraction,p%plasma_protein, &
           p%initial_interstitial_protein,p%oncotic_linear,p%oncotic_quadratic,p%initial_a_fraction, &
           p%exchange_resistance,p%minimum_volume,l%interstitial_capacity_ml_per_100g, &
           l%interstitial_compartment_a_fraction,l%initial_interstitial_saturation, &
           l%capillary_hydraulic_conductivity,l%interstitial_pressure_min_mmhg,l%interstitial_pressure_max_mmhg, &
           l%lymphatic_pressure_min_mmhg,l%lymphatic_pressure_max_mmhg,l%lymphatic_density, &
           l%lymphatic_saturation_threshold,l%lymphatic_baseline_conductivity_ratio, &
           l%lymphatic_conductivity_coefficient_1,l%lymphatic_conductivity_coefficient_2, &
           l%lymphatic_conductivity_coefficient_3,l%lymphatic_conductivity_coefficient_4, &
           l%lymphatic_conductivity_coefficient_5,l%lymphatic_conductivity_coefficient_6, &
           l%pressure_phase_offset_radians]))) error stop 'Non-finite coupled lymphatic parameter'
      if (min(p%lung_mass_g,p%minimum_volume,p%exchange_resistance,l%interstitial_capacity_ml_per_100g) <= 0.0_dp) &
           error stop 'Coupled lymphatic mass, capacity, volume floor and resistance must be positive'
      if (min(p%pressure_multiplier,p%conductivity_multiplier,p%protein_permeability,p%plasma_protein, &
           p%initial_interstitial_protein,p%oncotic_linear,p%oncotic_quadratic, &
           l%capillary_hydraulic_conductivity,l%lymphatic_density,l%lymphatic_baseline_conductivity_ratio) < 0.0_dp) &
           error stop 'Negative coupled transport parameter'
      if (p%fluid_substeps < 1) error stop 'fluid_substeps must be at least one'
      if (.not. all(ieee_is_finite([p%minimum_transit_times,p%maximum_transit_times,p%saturation_tolerance]))) &
           error stop 'Non-finite coupled stopping parameter'
      if (p%minimum_transit_times < 0.0_dp .or. p%maximum_transit_times <= p%minimum_transit_times .or. &
           p%saturation_tolerance <= 0.0_dp .or. p%convergence_check_steps < 1 .or. &
           p%maximum_settling_breaths < 1 .or. p%surfactant_equilibration_breaths < 0) &
           error stop 'Invalid coupled stopping parameter'
      if (min(p%protein_reflection,p%protein_convection_fraction) < 0.0_dp .or. &
           max(p%protein_reflection,p%protein_convection_fraction) > 1.0_dp) error stop 'Protein fractions outside [0,1]'
      if (l%interstitial_compartment_a_fraction <= 0.0_dp .or. l%interstitial_compartment_a_fraction >= 1.0_dp) &
           error stop 'Interstitial compartment fraction must be between zero and one'
      if (p%initial_a_fraction <= 0.0_dp .or. p%initial_a_fraction > l%interstitial_compartment_a_fraction .or. &
           l%initial_interstitial_saturation <= 0.0_dp .or. &
           l%initial_interstitial_saturation > 1.0_dp-l%interstitial_compartment_a_fraction) &
           error stop 'Invalid coupled initial fluid fractions'
    end associate
  end subroutine validate_fluid_parameters

  subroutine initialise_fluid(state, count)
    type(fluid_state), intent(out) :: state
    integer, intent(in) :: count
    real(dp) :: capacity
    if (count < 1) error stop 'Coupled model requires terminal units'
    state = fluid_state()
    capacity = lymphatic_params%interstitial_capacity_ml_per_100g* &
         coupled_lymphatic_params%lung_mass_g*10.0_dp/real(count,dp)
    state%capacity_a = lymphatic_params%interstitial_compartment_a_fraction*capacity
    state%capacity_b = capacity-state%capacity_a
    state%volume_a = coupled_lymphatic_params%initial_a_fraction*capacity
    state%volume_b = lymphatic_params%initial_interstitial_saturation*capacity
    if (min(state%volume_a,state%volume_b) <= coupled_lymphatic_params%minimum_volume) &
         error stop 'Coupled initial fluid volume is below minimum_volume'
    ! Source used concentration*mm^3 as scaled amounts. Explicit /1000 gives mg;
    ! the corresponding concentration and flux conversions preserve the algebra.
    state%protein_a = coupled_lymphatic_params%initial_interstitial_protein*state%volume_a/1000.0_dp
    state%protein_b = coupled_lymphatic_params%initial_interstitial_protein*state%volume_b/1000.0_dp
  end subroutine initialise_fluid

  real(dp) function oncotic(concentration)
    real(dp), intent(in) :: concentration
    oncotic = coupled_lymphatic_params%oncotic_linear*concentration+ &
         coupled_lymphatic_params%oncotic_quadratic*concentration**2
  end function oncotic

  real(dp) function interstitial_pressure(saturation, fluctuation, phase)
    real(dp), intent(in) :: saturation, fluctuation, phase
    real(dp) :: difference
    difference = lymphatic_params%interstitial_pressure_min_mmhg-lymphatic_params%interstitial_pressure_max_mmhg
    interstitial_pressure = fluctuation/2.0_dp*sin(phase)+(difference+fluctuation)* &
         (saturation**2-2.0_dp*saturation)+lymphatic_params%interstitial_pressure_min_mmhg+fluctuation/2.0_dp
  end function interstitial_pressure

  real(dp) function lymphatic_conductivity(saturation, conductivity)
    real(dp), intent(in) :: saturation, conductivity
    associate(l => lymphatic_params)
      if (saturation < l%lymphatic_saturation_threshold) then
         lymphatic_conductivity = l%lymphatic_baseline_conductivity_ratio*conductivity
      else
         lymphatic_conductivity = (((((l%lymphatic_conductivity_coefficient_1*saturation+ &
              l%lymphatic_conductivity_coefficient_2)*saturation+l%lymphatic_conductivity_coefficient_3)* &
              saturation+l%lymphatic_conductivity_coefficient_4)*saturation+ &
              l%lymphatic_conductivity_coefficient_5)*saturation+l%lymphatic_conductivity_coefficient_6)*conductivity
      endif
    end associate
    if (lymphatic_conductivity < 0.0_dp) error stop 'Negative lymphatic conductivity: review polynomial parameters'
  end function lymphatic_conductivity

  subroutine capillary_exchange(volume, protein, pressure, cap_pressure, area, conductivity, dt, dv, dq)
    real(dp), intent(inout) :: volume, protein
    real(dp), intent(in) :: pressure, cap_pressure, area, conductivity, dt
    real(dp), intent(out) :: dv, dq
    real(dp) :: c, donor_c
    c = 1000.0_dp*protein/volume
    dv = 0.0_dp
    ! Preserve the source's hydrostatic gate; net oncotic flow can be negative.
    if (cap_pressure > pressure) dv = 0.5_dp*conductivity*area* &
         (cap_pressure-pressure-coupled_lymphatic_params%protein_reflection* &
         (oncotic(coupled_lymphatic_params%plasma_protein)-oncotic(c)))*dt
    dv = max(dv,coupled_lymphatic_params%minimum_volume-volume)
    donor_c = c
    if (dv > 0.0_dp) donor_c = coupled_lymphatic_params%plasma_protein
    dq = (coupled_lymphatic_params%protein_permeability*area*0.5_dp* &
         (coupled_lymphatic_params%plasma_protein-c)*dt+ &
         dv*donor_c*coupled_lymphatic_params%protein_convection_fraction)/1000.0_dp
    dq = max(dq,-protein)
    volume = volume+dv
    protein = protein+dq
  end subroutine capillary_exchange

  subroutine transfer(volume_from, protein_from, volume_to, protein_to, requested)
    real(dp), intent(inout) :: volume_from, protein_from, volume_to, protein_to
    real(dp), intent(in) :: requested
    real(dp) :: amount, protein_amount
    amount = max(0.0_dp,min(requested,volume_from-coupled_lymphatic_params%minimum_volume))
    protein_amount = protein_from*amount/volume_from
    volume_from = volume_from-amount
    protein_from = protein_from-protein_amount
    volume_to = volume_to+amount
    protein_to = protein_to+protein_amount
  end subroutine transfer

  logical function fluid_running(state, transit)
    type(fluid_state), intent(in) :: state
    real(dp), intent(in) :: transit
    associate(p => coupled_lymphatic_params)
      fluid_running = (state%saturation_error > p%saturation_tolerance .or. &
           state%elapsed < p%minimum_transit_times*transit) .and. state%elapsed < p%maximum_transit_times*transit
    end associate
  end function fluid_running

  integer function fluid_stop_status(state, transit)
    ! 0 running; 1 saturation converged; 2 transit-time cap; 3 global cap (caller);
    ! 4 no transit time (zero-area input only); -1 surfactant-only (caller).
    type(fluid_state), intent(in) :: state
    real(dp), intent(in) :: transit
    fluid_stop_status = 0
    if (transit <= 0.0_dp) then
       fluid_stop_status = 4
    elseif (fluid_running(state,transit)) then
       return
    elseif (state%saturation_error <= coupled_lymphatic_params%saturation_tolerance .and. &
         state%elapsed >= coupled_lymphatic_params%minimum_transit_times*transit) then
       fluid_stop_status = 1
    else
       fluid_stop_status = 2
    endif
  end function fluid_stop_status

  subroutine sample_fluid_convergence(state)
    type(fluid_state), intent(inout) :: state
    real(dp) :: saturation
    saturation = (state%volume_a+state%volume_b)/(state%capacity_a+state%capacity_b)
    state%saturation_history(2:5) = state%saturation_history(1:4)
    state%saturation_history(1) = saturation
    state%saturation_error = abs(sum(state%saturation_history)/5.0_dp-saturation)
  end subroutine sample_fluid_convergence

  subroutine advance_fluid(state, pressure_pa, area_mm2, pe_range_pa, period, dt, time_start)
    type(fluid_state), intent(inout) :: state
    real(dp), intent(in) :: pressure_pa, area_mm2, pe_range_pa, period, dt
    real(dp), intent(in), optional :: time_start
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: h, phase, fluctuation, pressure, conductivity, pa, pb, pl, saturation
    real(dp) :: dva, dvb, dqa, dqb, excess, diffusion, drain, protein_drain
    integer :: i
    if (.not. all(ieee_is_finite([pressure_pa,area_mm2,pe_range_pa,period,dt]))) &
         error stop 'Non-finite coupled fluid input'
    if (dt <= 0.0_dp .or. period <= 0.0_dp .or. area_mm2 < 0.0_dp .or. pe_range_pa < 0.0_dp) &
         error stop 'Invalid coupled fluid timestep, area or recoil range'
    h = dt/real(coupled_lymphatic_params%fluid_substeps,dp)
    pressure = pressure_pa/133.32239_dp*coupled_lymphatic_params%pressure_multiplier
    fluctuation = pe_range_pa/133.32239_dp
    conductivity = lymphatic_params%capillary_hydraulic_conductivity*coupled_lymphatic_params%conductivity_multiplier
    do i = 1,coupled_lymphatic_params%fluid_substeps
       phase = 2.0_dp*pi*(state%elapsed+h)/period
       ! Absolute respiratory clock continues during frozen-fluid breaths.
       if (present(time_start)) phase = 2.0_dp*pi*(time_start+real(i,dp)*h)/period
       pa = interstitial_pressure(state%volume_a/state%capacity_a,fluctuation,phase)
       pb = interstitial_pressure(state%volume_b/state%capacity_b,fluctuation,phase)
       call capillary_exchange(state%volume_a,state%protein_a,pa,pressure,area_mm2,conductivity,h,dva,dqa)
       call capillary_exchange(state%volume_b,state%protein_b,pb,pressure,area_mm2,conductivity,h,dvb,dqb)
       state%filtered = state%filtered+dva+dvb
       state%protein_filtered = state%protein_filtered+dqa+dqb
       ! Ruobing's A overflow splits equally between B and alveoli. Transfer
       ! proteins with donor concentrations, never using stale excess variables.
       excess = max(0.0_dp,state%volume_a-state%capacity_a)
       call transfer(state%volume_a,state%protein_a,state%alveolar,state%protein_alveolar,excess/2.0_dp)
       call transfer(state%volume_a,state%protein_a,state%volume_b,state%protein_b,excess/2.0_dp)
       call transfer(state%volume_b,state%protein_b,state%alveolar,state%protein_alveolar, &
            max(0.0_dp,state%volume_b-state%capacity_b))
       diffusion = (state%volume_a/state%capacity_a-state%volume_b/state%capacity_b)* &
            h/coupled_lymphatic_params%exchange_resistance
       if (diffusion > 0.0_dp) then
          call transfer(state%volume_a,state%protein_a,state%volume_b,state%protein_b,diffusion)
       else
          call transfer(state%volume_b,state%protein_b,state%volume_a,state%protein_a,-diffusion)
       endif
       saturation = state%volume_b/state%capacity_b
       pl = fluctuation/2.0_dp*sin(phase+lymphatic_params%pressure_phase_offset_radians)+ &
            (lymphatic_params%lymphatic_pressure_max_mmhg-lymphatic_params%lymphatic_pressure_min_mmhg- &
            fluctuation)*saturation**2+lymphatic_params%lymphatic_pressure_min_mmhg+fluctuation/2.0_dp
       drain = lymphatic_conductivity(saturation,conductivity)*area_mm2*lymphatic_params%lymphatic_density* &
            max(0.0_dp,pb-pl)*h
       drain = min(drain,max(0.0_dp,state%volume_b-coupled_lymphatic_params%minimum_volume))
       protein_drain = state%protein_b*drain/state%volume_b
       state%volume_b = state%volume_b-drain
       state%protein_b = state%protein_b-protein_drain
       state%drained = state%drained+drain
       state%protein_drained = state%protein_drained+protein_drain
       state%elapsed = state%elapsed+h
       if (.not. all(ieee_is_finite([state%volume_a,state%volume_b,state%alveolar, &
            state%protein_a,state%protein_b,state%protein_alveolar,state%filtered,state%drained]))) &
            error stop 'Non-finite coupled transport state; reduce timestep and review parameters'
    enddo
  end subroutine advance_fluid

  ! Coupled protocol, terminal mapping, stopping and result access.

  subroutine check_airway_layout()
    if (num_nu /= 12 .or. nu_vol /= 1 .or. nu_pe /= 7) error stop 'Set ventilation_indices before coupled ventilation'
    if (.not. allocated(units) .or. .not. allocated(unit_field)) error stop 'Load airway geometry and append_units first'
    if (num_units < 1 .or. size(unit_field,2) /= num_units .or. size(unit_field,1) /= num_nu) &
         error stop 'Inconsistent coupled ventilation unit allocation'
  end subroutine check_airway_layout

  subroutine load_coupled_capillary(capillary_file)
    character(len=*), intent(in) :: capillary_file
    integer :: connector, io, ios, terminal, u
    integer, allocatable :: unit_by_element(:)
    logical, allocatable :: seen(:)
    logical :: format_set, raw_microflow
    real(dp) :: pressure, area, transit, xyz(3), pin, pout, before_area(4), after_transit(2)
    character(len=1024) :: line
    call release_coupling()
    call check_airway_layout()
    allocate(capillary(3,num_units),input_units(num_units),unit_by_element(num_elems),seen(num_units))
    input_units = units
    capillary = 0.0_dp
    unit_by_element = 0
    seen = .false.
    format_set = .false.
    raw_microflow = .false.
    do u = 1,num_units
       unit_by_element(units(u)) = u
    enddo
    open(newunit=io,file=trim(capillary_file),status='old',action='read',iostat=ios)
    if (ios /= 0) error stop 'Cannot open coupled capillary input'
    do
       read(io,'(a)',iostat=ios) line
       if (ios < 0) exit
       if (ios /= 0) error stop 'Cannot read mapped capillary input'
       line = adjustl(line)
       if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
       ! Standard micro_flow_unit.out contains 14 columns. Its capillary
       ! connector elements are created as 2*num_elems+u by add_matching_mesh,
       ! where u is the common airway/perfusion terminal-unit index.
       read(line,*,iostat=ios) connector,xyz,pin,pout,before_area,area,transit,after_transit
       if (ios == 0) then
          if (format_set .and. .not. raw_microflow) error stop 'Mixed coupled capillary input formats'
          raw_microflow = .true.
          format_set = .true.
          u = connector-2*num_elems
          if (u < 1 .or. u > num_units) &
               error stop 'micro_flow_unit connector does not match the airway element/unit numbering'
          terminal = units(u)
          pressure = 0.5_dp*(pin+pout)
       else
          if (format_set .and. raw_microflow) error stop 'Mixed coupled capillary input formats'
          raw_microflow = .false.
          format_set = .true.
          read(line,*,iostat=ios) terminal,pressure,area,transit
          if (ios /= 0) error stop 'Expected micro_flow_unit.out or terminal pressure_Pa area_mm2 transit_s'
          if (terminal < 1 .or. terminal > num_elems) error stop 'Invalid capillary input terminal element'
          u = unit_by_element(terminal)
          if (u == 0) error stop 'Capillary input element is not an airway terminal'
       endif
       if (seen(u)) error stop 'Duplicate airway terminal in capillary input'
       if (.not. all(ieee_is_finite([pressure,area,transit]))) error stop 'Non-finite capillary input'
       if (area < 0.0_dp .or. transit < 0.0_dp) error stop 'Negative capillary area or transit time'
       if (area > 0.0_dp .and. transit <= 0.0_dp) error stop 'Exchanging units require positive transit time'
       capillary(:,u) = [pressure,area,transit]
       seen(u) = .true.
    enddo
    close(io)
    if (.not. all(seen)) error stop 'Capillary input must contain every matching airway terminal exactly once'
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
end module surfactant
