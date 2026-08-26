! Experimental fluid/protein equations adapted from Ruobing-rl/Lym_surf 143d819.
! Deliberately separate from the validated published lymphatics module.
module coupled_lymphatics
  use precision, only: dp
  use parameter_types, only: lymphatic_params, coupled_lymphatic_params
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: fluid_state, initialise_fluid, advance_fluid, validate_fluid_parameters
  public :: fluid_running, sample_fluid_convergence, fluid_stop_status

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

contains

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
end module coupled_lymphatics
