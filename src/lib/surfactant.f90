! Adapted from Ruobing-rl/lungsim Lym_surf, 143d8192def1938e6eb781c451d411ba825bb0a6.
! Constitutive equations retained; unsafe standalone/C-array interfaces omitted.
module surfactant
  use precision, only: dp
  use parameter_types, only: surfactant_params
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: surfactant_state, initialise_surface, advance_surface, validate_surface_parameters
  public :: surface_tension

  type :: surfactant_state
     real(dp) :: area = 0.0_dp         ! cm^2 per alveolus (hemisphere)
     real(dp) :: gamma = 0.0_dp        ! g/cm^2
     real(dp) :: tension = 0.0_dp      ! dyn/cm
     real(dp) :: pressure = 0.0_dp     ! Pa
     real(dp) :: compliance = 0.0_dp   ! mm^3/Pa per terminal unit
  end type surfactant_state

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
end module surfactant
