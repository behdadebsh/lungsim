module gas_exchange
!*Brief Description:* This module is for simulating lung gas steady-state transfer and gas exchange.
!
!*LICENSE:*
!
!
!
!*Full Description:*
  !
  use arrays
  use indices
  use precision

  implicit none

  !Module parameters

  real(dp), parameter :: mmL_to_mlml = 0.022414_dp ! 1 mmol gas at STPD, not 25.452e-3_dp ! to convert mmol/L to mL/mL
  ! coefficients in the Kelman model for SHbO2
  real(dp), parameter :: A1 = -8.538889e+3_dp, A2 = 2.121401e+3_dp, A3 = -6.707399e+1_dp, &
       A4 = 9.359609e+5_dp, A5 = -3.134626e+4_dp, A6=2.396167e+3_dp, A7=-6.710441e+1_dp
  
  !Module types

  ! key global physiological variables that are carried over between calls to the module's functions
  type :: bloodgas_global
     ! global blood gas and acid-base variables. defining a type allows more than one instance.
     ! initialised here to reasonable values; updated by the models
     ! this should move to a general module if and when these are required by other modules
     real(dp) :: p_art_o2  = 100.0_dp
     real(dp) :: p_ven_o2  = 40.0_dp 
     real(dp) :: c_art_o2  = 0.2_dp
     real(dp) :: c_ven_o2  = 0.15_dp
     real(dp) :: p_art_co2 = 40.0_dp 
     real(dp) :: p_ven_co2 = 45.0_dp 
     real(dp) :: c_art_co2 = 0.47_dp
     real(dp) :: c_ven_co2 = 0.51_dp
     real(dp) :: pH_art = 7.4_dp
     real(dp) :: pH_ven = 7.37_dp
     real(dp) :: sat_art = 0.97_dp
     real(dp) :: sat_ven = 0.75_dp
  end type bloodgas_global

  type(bloodgas_global) :: bg_state ! used in the code, allowing for values to be updated
  
  !Module variables

  ! derived parameters - calculated once in update_derived (called from initialisation subroutines)
  real(dp) :: p_i_o2              ! mmHg, partial pressure of inspired O2
  real(dp) :: p_atm_dry           ! mmHg, atmospheric pressure less water vapour pressue - at 37 degrees
  real(dp) :: o2_cnv_pp2c         ! (mmol/mm^3)/mmHg, conversion from partial pressure (mmHg) to concentration (mmol/mm^3)
  real(dp) :: o2_cnv_c2pp         ! mmHg/(mmol/mm^3), conversion from concentration (mmol/mm^3) to partial pressure (mmHg)
  real(dp) :: Hb_conc             ! concentration of Hb, g/dL * 10 dL/L / (g/mol) --> mol/L
  real(dp) :: pH_a                ! pH of arterial blood
  real(dp) :: pH_v                ! pH of venous blood
  real(dp) :: Hb_g_dL             ! haemoglobin in g/dL
  real(dp) :: K_stpd              ! conversion from BTPS to STPD using K = P_B * T/(273.15)
  logical :: initialised_gastransfer = .false.
  
  !Interfaces

  public  initial_gasexchange
  public  steadystate_co2          ! calculation of steady-state CO2 in gx units based on mass balance
  public  steadystate_o2           ! calculation of steady-state O2 in gx units based on mass balance

  private initialise_gastransfer   ! allocate memory and initialise for s-s gas transfer problems
  private derived_gx_params        ! calculate variable values that have been declared above
  private unit_co2_steadystate     ! steady-state CO2 transfer for a single gx unit
  private function_co2             ! --helper: residual of CO2 mass balance
  private fdash_co2                ! --helper: derivative of CO2 balance residual wrt capillary PCO2
  private co2_content_from_pco2    ! calculate CO2 content for a given PCO2 (and PO2, pH, sat)
  private pco2_from_co2content     ! calculate PCO2 for a given CO2 content (and PO2)
  private unit_o2_steadystate      ! steady-state O2 transfer for a single gx unit
  private function_o2              ! --helper: residual of O2 mass balance
  private fdash_o2                 ! --helper: derivative of O2 balance residual wrt capillary PO2
  private fdash_o2_kelman          ! --option: Kelman model for Hb saturation slope
  private fdash_o2_dash            ! --option: Dash model for Hb saturation slope
  private o2_content_from_po2      ! calculate O2 content for a given PO2 (and PCO2, sat)
  private po2_from_o2content       ! calculate PO2 for a given O2 content (and PCO2, pH)
  private saturation_of_o2         ! saturation of O2 for a given PO2, c_CO2, PCO2, pH 
  private saturation_kelman        ! --option: accurate around normal PCO2, not good for extremes of PCO2
  private saturation_valsecchi     ! --option: empirical, very accurate at 37C and normal pH. no pH dependence
  private saturation_dash          ! --option: Bassingthwaigthe group. Expands to very detailed model (not here)
  private pH_funct_CO2             ! simple estimation of PH as a function of PCO2
  
contains
  
!!!##############################################################################
  
  subroutine initial_gasexchange(initial_concentration, surface_area, V_cap)

!!! Inputs
    real(dp),intent(in) :: initial_concentration
    real(dp), optional ::  surface_area, V_cap
!!! Locals
    integer :: nunit
    real(dp) :: Vcap_unit
    
!!! allocate memory for the gasex_field array, if not already allocated
    if(.not.allocated(gasex_field)) allocate(gasex_field(num_gx,num_units))
    
!!! initialiase nj_conc2 (for CO2 concentration); currently hardcoded to 40 mmHg
    node_field(nj_conc2,1:num_nodes) = 40.0_dp * o2_cnv_pp2c
    write(*,'('' Initialising Palv_CO2 to 40 mmHg'')')
    
!!! initialise the gas exchange field for o2 partial pressures
    gasex_field(ng_p_alv_o2,1:num_units) = initial_concentration * o2_cnv_c2pp
    gasex_field(ng_p_alv_co2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    gasex_field(ng_p_cap_o2,1:num_units) = initial_concentration * o2_cnv_c2pp
    gasex_field(ng_p_cap_co2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    
    unit_field(nu_conc1,1:num_units) = gasex_field(ng_p_alv_o2,1:num_units) * o2_cnv_pp2c
    unit_field(nu_conc2,1:num_units) = gasex_field(ng_p_alv_co2,1:num_units)* o2_cnv_pp2c
    
    if(present(surface_area))then
!!! initialise the time blood has been in capillaries
       gasex_field(ng_time,1:num_units) = 0.0_dp
       
!!! capillary volume per gas exchange unit = transit time * flow
       ! elem_units_below is the EFFECTIVE number of units, so this is correct
       !Note that these are calculated on a per unit basis in the perfusion model so can be read in for future iterations
       Vcap_unit = V_cap/elem_units_below(1) ! the capillary volume per gas exchange unit
       gasex_field(ng_Vc,1:num_units) = Vcap_unit
       gasex_field(ng_sa,1:num_units) = surface_area/elem_units_below(1)
       
!!! transit time through the gas exchange unit = capillary volume/flow
       forall (nunit=1:num_units) gasex_field(ng_tt,nunit) = &
            Vcap_unit/unit_field(nu_perf,nunit)
    endif
    
  end subroutine initial_gasexchange
  
!!!##############################################################################

  subroutine initialise_gastransfer()
    ! allocate memory as required and initialise array types for gas transfer
    
    use parameter_types, only: constants, gasex, gx_params
    
    call derived_gx_params()
  
!!! allocate memory for the gasex arrays, if not already allocated
    if(.not.allocated(gasex%p_alv_o2))  allocate(gasex%p_alv_o2(num_units))
    if(.not.allocated(gasex%p_cap_o2))  allocate(gasex%p_cap_o2(num_units))
    if(.not.allocated(gasex%c_cap_o2))  allocate(gasex%c_cap_o2(num_units))
    if(.not.allocated(gasex%conc_o2))   allocate(gasex%conc_o2(num_units))
    if(.not.allocated(gasex%p_alv_co2)) allocate(gasex%p_alv_co2(num_units))
    if(.not.allocated(gasex%p_cap_co2)) allocate(gasex%p_cap_co2(num_units))
    if(.not.allocated(gasex%c_cap_co2)) allocate(gasex%c_cap_co2(num_units))
    if(.not.allocated(gasex%conc_co2))  allocate(gasex%conc_co2(num_units))
    if(.not.allocated(gasex%ph_cap))    allocate(gasex%ph_cap(num_units))
    if(.not.allocated(gasex%sat_cap))   allocate(gasex%sat_cap(num_units))
    if(.not.allocated(gasex%Vdot))      allocate(gasex%Vdot(num_units))
    if(.not.allocated(gasex%Qdot))      allocate(gasex%Qdot(num_units))

    gasex%p_alv_o2  = bg_state%p_art_o2
    gasex%p_cap_o2  = bg_state%p_art_o2
    gasex%c_cap_o2  = bg_state%c_art_o2
    gasex%conc_o2   = gx_params%init_p_alv_o2 * o2_cnv_pp2c
    gasex%p_alv_co2 = bg_state%p_art_co2
    gasex%p_cap_co2 = bg_state%p_art_co2
    gasex%c_cap_co2 = bg_state%c_art_co2
    gasex%conc_co2  = gasex%p_alv_co2 * o2_cnv_pp2c
    gasex%ph_cap    = bg_state%ph_art 
    gasex%sat_cap   = bg_state%sat_art
    
    gasex%Vdot(:) = unit_field(nu_Vdot0,:)
    gasex%Qdot(:) = unit_field(nu_perf,:)

    node_field(nj_conc1,:) = bg_state%p_art_o2 * o2_cnv_pp2c
    node_field(nj_conc2,:) = bg_state%p_art_co2 * o2_cnv_pp2c  ! o2molvol is just molar volume for all gas species
    
    node_field(nj_conc1,1) = gx_params%FiO2 / constants%o2molvol_37deg !mmol/mm^3, inspired O2
    node_field(nj_conc2,1) = 0.0_dp ! inspired CO2; should make FiCO2 user-defined

  end subroutine initialise_gastransfer
  
!!!##############################################################################

  subroutine derived_gx_params()
    ! calculates values of variables (which become parameters) declared at top of the module

    use parameter_types, only: constants, gx_params

    p_atm_dry   = gx_params%press_atm - gx_params%press_h2o
    p_i_o2      = gx_params%FiO2 * p_atm_dry ! accounting for humidification by the upper airway
    o2_cnv_c2pp = constants%o2molvol_37deg * p_atm_dry
    o2_cnv_pp2c = 1.0_dp / o2_cnv_c2pp
    Hb_g_dl     = gx_params%Hb
    Hb_conc     = Hb_g_dL * 10.0_dp / constants%mw  ! concentration of Hb, g/dL * 10 dL/L / (g/mol) --> mol/L
    ! Hb_conc should be ~= 2.33e-3_dp for all species ! mM==mmol/L; haemoglobin concentration in whole blood (Guyton & Hall 1996)
    pH_a        = gx_params%pHa
    pH_v        = pH_a - 0.03_dp
    K_stpd      = gx_params%press_atm * (gx_params%body_temp + 273.15d0) / 273.15d0

  end subroutine derived_gx_params

!!!##############################################################################

  real(dp) function steadystate_CO2 (Vdot_alv) result(p_art_co2)
    ! Steadystate CO2 model following Kelman. Uses CO2 content<->PCO2 mapping with Haldane coupling:
    ! steady-state is reached when there is no change between current and previous p_ven_co2

    use parameter_types, only: gx_params, Q_params
!!! Inputs
    real(dp), intent(in) :: Vdot_alv
!!! Local parameters
    real(dp), parameter :: err_tol = 1.0e-3_dp
!!! Local variables
    integer :: counter, k, ne, np, nunit
    real(dp) :: cardiac_output, c_art_co2, c_cap_co2, c_cap_o2, c_ven_co2, fdash, fun_co2,  &
         p_art_co2_last, p_cap_co2, p_cap_o2, pH_c, p_ven_co2, &
         p_ven_co2_last, Q_total, RV_flow, sat_c, shunt_flow, unit_v, unit_q, VCO2, v_q
    logical :: continue

    ! call initialisation if not already done
    if(.not.initialised_gastransfer)then
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif

    ! get the key cardiac and metabolic variables
    VCO2 = gx_params%VCO2
    cardiac_output = Q_params%cardiac_output
    shunt_flow = Q_params%shunt_fraction * cardiac_output
    RV_flow = cardiac_output - shunt_flow

    c_ven_co2 = bg_state%c_ven_co2
    p_art_co2 = bg_state%p_art_co2
    p_ven_co2 = bg_state%p_ven_co2
    p_ven_co2_last = bg_state%p_ven_co2
    p_art_co2_last = bg_state%p_art_co2

    counter = 1
    continue = .true.

    do while (continue)

       Q_total   = 0.0_dp
       c_art_co2 = 0.0_dp
       
       do nunit = 1, num_units
          ne = units(nunit)
          ! Initialise to previous capillary value
          p_cap_co2 = gasex%p_cap_co2(nunit)
          p_cap_o2  = gasex%p_cap_o2(nunit)
          c_cap_co2 = gasex%c_cap_co2(nunit)
          c_cap_o2  = gasex%c_cap_o2(nunit)
          
          pH_c  = gasex%ph_cap(nunit)
          sat_c = gasex%sat_cap(nunit)
          
          unit_v = Vdot_alv * gasex%Vdot(nunit) / elem_field(ne_Vdot,1)
          unit_q = RV_flow * gasex%Qdot(nunit) / elem_field(ne_Qdot,1)
          if (abs(unit_q) < loose_tol) then
             v_q = 1.0e5_dp ! set to high enough, but not ridiculous, value
          else
             v_q = unit_v / unit_q
          endif

          p_cap_o2 = unit_o2_steadystate(c_cap_co2, c_cap_o2, p_cap_co2, p_cap_o2, &
               pH_c, v_q, sat_c)
          p_cap_co2 = unit_co2_steadystate(c_cap_co2, p_cap_co2, p_cap_o2, v_q)

          if(p_cap_co2 < 1.0e-2_dp)then
             c_cap_co2 = 0.0_dp
          else
             c_cap_co2 = gasex%c_cap_co2(nunit) ! previous c_cap_co2
            ! update unit pH for new p_cap_co2, using previous c_cap_co2
             pH_c = pH_funct_CO2(p_cap_co2, c_cap_co2) ! using previous iteration c_cap_co2
             sat_c = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH_c) ! using previous iteration c_cap_co2
             c_cap_co2 = co2_content_from_pco2(p_cap_co2, p_cap_o2, pH_c, sat_c)
             ! Flow-weighted sum of CO2 content
             Q_total   = Q_total + abs(gasex%Qdot(nunit)) * units_effective(nunit)
             c_art_co2 = c_art_co2 + units_effective(nunit) * (c_cap_co2 * abs(gasex%Qdot(nunit)))
          endif
          
          ! update the gas exchange unit variables
          gasex%p_cap_co2(nunit) = p_cap_co2
          gasex%p_alv_co2(nunit) = p_cap_co2
          gasex%ph_cap(nunit) = pH_c
          gasex%sat_cap(nunit) = sat_c
          gasex%c_cap_co2(nunit) = c_cap_co2

       end do ! nunit

       ! Normalise by total flow
       if (Q_total > 0.0_dp) then
          c_art_co2 = c_art_co2 / Q_total
       else
          c_art_co2 = 0.0_dp
       endif

       ! add shunt contribution
       c_art_co2 = (c_art_co2 * RV_flow + c_ven_co2 * shunt_flow) / cardiac_output
       p_art_co2 = pco2_from_co2content(c_art_co2, p_art_co2, bg_state%p_art_o2)
       
       ! update local c_ven_co2 via metabolic VCO2 addition
       c_ven_co2 = c_art_co2 + VCO2 / cardiac_output   ! units: (ml/ml)
       p_ven_co2 = pco2_from_co2content(c_ven_co2, p_ven_co2, bg_state%p_ven_o2)

       ! Convergence check
       if (counter > 1) then
          if (abs(p_ven_co2 - p_ven_co2_last) / max(zero_tol, abs(p_ven_co2_last)) < err_tol .and. &
               abs(p_art_co2 - p_art_co2_last) / max(zero_tol, abs(p_art_co2_last)) < err_tol) then
             continue = .false.
          else
             if (counter >= 200) continue = .false.
             counter = counter + 1
             p_ven_co2_last = p_ven_co2
             p_art_co2_last = p_art_co2
          endif
       else
          counter = counter + 1
          p_ven_co2_last = p_ven_co2
          p_art_co2_last = p_art_co2
       endif
    end do ! while continue

    do nunit = 1, num_units
       ne = units(nunit)
       np = elem_nodes(2, ne)
       node_field(nj_conc2, np) = gasex%p_cap_co2(nunit) * o2_cnv_pp2c
    end do

    gasex%conc_co2 = gasex%p_alv_co2 * o2_cnv_pp2c

    ! update the stored global blood gas acid-base variables
    bg_state%p_art_co2 = p_art_co2
    bg_state%p_ven_co2 = p_ven_co2
    bg_state%c_art_co2 = c_art_co2
    bg_state%c_ven_co2 = c_ven_co2
    bg_state%pH_art = pH_funct_CO2(p_art_co2, c_art_co2)
    bg_state%sat_art = saturation_of_o2(c_art_co2, p_art_co2, bg_state%p_art_o2, bg_state%pH_art)
    bg_state%pH_ven = pH_funct_CO2(p_ven_co2, c_ven_co2)
    bg_state%sat_ven = saturation_of_o2(c_ven_co2, p_ven_co2, bg_state%p_ven_o2, bg_state%pH_ven)
    
  end function steadystate_CO2
  

!!!##############################################################################

  real(dp) function unit_co2_steadystate(c_cap_co2_init, p_cap_co2_init, &
       p_cap_o2, v_q) result(p_cap_co2)
    ! steadystate CO2 for a single unit. function_co2 does mass balance and fdash_co2
    ! estimates derivative of pco2

!!! Inputs
    real(dp), intent(in) :: c_cap_co2_init, p_cap_co2_init, p_cap_o2, v_q
!!! Locals
    integer :: k
    real(dp) :: c_cap_co2, fdash, fun_co2, pH, sat

    if (abs(v_q) <= 1.0e-3_dp) then
       ! no ventilation: capillary CO2 tends to venous CO2
       p_cap_co2 = bg_state%p_ven_co2
    elseif (abs(v_q) > 100.0_dp) then
       ! essentially infinite ventilation: alveolar/capillary CO2 ~ 0
       p_cap_co2 = 0.0_dp
    else
       p_cap_co2 = p_cap_co2_init
       c_cap_co2 = c_cap_co2_init
       pH = pH_funct_CO2(p_cap_co2, c_cap_co2)
       sat = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH)

       k = 0
       do
          fun_co2 = function_co2(v_q, c_cap_co2, p_cap_co2)
          if (abs(fun_co2) < 1.0e-4_dp) exit
          if (k >= 200) exit
          fdash = fdash_co2(v_q, c_cap_co2, p_cap_co2, p_cap_o2)
          if (abs(fdash) < zero_tol) exit
          p_cap_co2 = p_cap_co2 - fun_co2/fdash
          pH = pH_funct_CO2(p_cap_co2, c_cap_co2) ! using previous iteration c_cap_co2
          sat = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH) ! using previous iteration c_cap_co2
          c_cap_co2 = co2_content_from_pco2(p_cap_co2, p_cap_o2, pH, sat)
          k = k + 1
       enddo
    endif
          
    p_cap_co2 = max(p_cap_co2, 0.0_dp)
          
  end function unit_co2_steadystate
  
!!!##############################################################################
  
  real(dp) function steadystate_O2 (Vdot_alv) result(p_art_o2)
    ! steadystate O2 model following Kelman
    
    use parameter_types, only: gx_params, Q_params
!!! Inputs
    real(dp),intent(in) :: Vdot_alv
!!! Locals
    integer :: counter, k, ne, np, nunit
    real(dp) :: cardiac_output, c_art_o2, c_cap_co2, c_cap_o2, c_ven_o2, fdash, fun_o2, &
         p_art_o2_last, p_cap_co2, p_cap_o2, pH_c, p_ven_o2, p_ven_o2_last, &
         Q_total, RV_flow, sat_c, shunt_flow, unit_v, unit_q, VO2, v_q, sum_o2, c_ven_co2
    logical :: continue
    
    ! call initialisation if not already done
    if(.not.initialised_gastransfer)then
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif
    
    ! get the key cardiac and metabolic variables
    VO2 = gx_params%VO2
    cardiac_output = Q_params%cardiac_output
    shunt_flow = Q_params%shunt_fraction * cardiac_output
    RV_flow = cardiac_output - shunt_flow

    c_ven_o2 = bg_state%c_ven_o2
    p_art_o2 = bg_state%p_art_o2
    p_ven_o2 = bg_state%p_ven_o2
    p_ven_o2_last = p_ven_o2
    
    counter = 1
    continue = .true.

    do while (continue)
       
       Q_total = 0.0_dp
       c_art_o2 = 0.0_dp

       do nunit = 1, num_units
          ne = units(nunit)
          ! Initialise to previous capillary value
          p_cap_o2  = gasex%p_cap_o2(nunit)
          p_cap_co2 = gasex%p_cap_co2(nunit)
          c_cap_o2  = gasex%c_cap_o2(nunit)
          c_cap_co2 = gasex%c_cap_co2(nunit)
          
          pH_c  = gasex%ph_cap(nunit)
          sat_c = gasex%sat_cap(nunit)
          
          unit_v = Vdot_alv * gasex%Vdot(nunit) / elem_field(ne_Vdot,1)
          unit_q = RV_flow * gasex%Qdot(nunit) / elem_field(ne_Qdot,1)
          if (unit_q < loose_tol) then
             v_q = 1.0e5_dp ! set to high enough, but not ridiculous, value
          else
             v_q = unit_v / unit_q
          endif

          p_cap_o2 = unit_o2_steadystate(c_cap_co2, c_cap_o2, p_cap_co2, p_cap_o2, &
               pH_c, v_q, sat_c)
          
          ! update saturation and content for new p_cap_o2
          sat_c = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH_c)
          c_cap_o2 = o2_content_from_po2(p_cap_co2, p_cap_o2, sat_c)
          ! Flow-weighted sum of O2 content
          Q_total   = Q_total + abs(gasex%Qdot(nunit)) * units_effective(nunit)
          c_art_o2 = c_art_o2 + units_effective(nunit) * &
               (c_cap_o2 * abs(gasex%Qdot(nunit)))
          
          ! update the gas exchange unit variables
          gasex%p_cap_o2(nunit) = p_cap_o2
          gasex%p_alv_o2(nunit) = p_cap_o2
          gasex%ph_cap(nunit) = pH_c
          gasex%sat_cap(nunit) = sat_c
          gasex%c_cap_o2(nunit) = c_cap_o2

       enddo !nunit

       ! Normalise by total flow
       if (Q_total > 0.0_dp) then
          c_art_o2 = c_art_o2 / Q_total
       else
          c_art_o2 = 0.0_dp
       endif

       ! just for output:
       c_cap_o2 = c_art_o2
       p_cap_o2 = po2_from_o2content(bg_state%c_art_co2, c_cap_o2, bg_state%p_art_co2, &
            bg_state%p_art_o2, bg_state%ph_art, bg_state%sat_art)
       
       ! add shunt contribution
       c_art_o2 = (c_art_o2 * RV_flow + c_ven_o2 * shunt_flow) / cardiac_output
       p_art_o2 = po2_from_o2content(bg_state%c_art_co2, c_art_o2, bg_state%p_art_co2, &
            bg_state%p_art_o2, bg_state%ph_art, bg_state%sat_art)

       ! Subtract metabolic consumption of O2 via VO2 to get c_ven_o2
       c_ven_o2 = c_art_o2 - VO2 / cardiac_output   ! units: (ml/ml)
       p_ven_o2 = po2_from_o2content(bg_state%c_ven_co2, c_ven_o2, bg_state%p_ven_co2, &
            bg_state%p_ven_o2, bg_state%ph_ven, bg_state%sat_ven)

       ! Convergence check
       if (counter > 1) then
          if (abs(p_ven_o2 - p_ven_o2_last) / max(zero_tol, abs(p_ven_o2_last)) < loose_tol .and. &
               abs(p_art_o2 - p_art_o2_last) / max(zero_tol, abs(p_art_o2_last)) < loose_tol) then
             continue = .false.
          else
             if (counter >= 200) continue = .false.
             counter = counter + 1
             p_ven_o2_last = p_ven_o2
             p_art_o2_last = p_art_o2
          endif !convergence check
       else
          counter = counter+1
          p_ven_o2_last = p_ven_o2
          p_art_o2_last = p_art_o2
       endif
    enddo !while continue
    
    do nunit =1,num_units
       ne = units(nunit)
       np = elem_nodes(2,ne)
       node_field(nj_conc1,np) = gasex%p_cap_o2(nunit) * o2_cnv_pp2c
    enddo

    gasex%conc_o2 = gasex%p_alv_o2 * o2_cnv_pp2c

    ! update the stored global blood gas acid-base variables
    bg_state%p_art_o2 = p_art_o2
    bg_state%p_ven_o2 = p_ven_o2
    bg_state%c_art_o2 = c_art_o2
    bg_state%c_ven_o2 = c_ven_o2
    bg_state%sat_art = saturation_of_o2(bg_state%c_art_co2, bg_state%p_art_co2, p_art_o2, bg_state%pH_art)
    bg_state%sat_ven = saturation_of_o2(bg_state%c_ven_co2, bg_state%p_ven_co2, p_ven_o2, bg_state%pH_ven)

  end function steadystate_O2

!!!##############################################################################

  real(dp) function unit_o2_steadystate(c_cap_co2, c_cap_o2_init, p_cap_co2, p_cap_o2_init, &
       pH_c, v_q, sat_c_init) result(p_cap_o2)
    ! steadystate O2 for a single unit. function_o2 does mass balance and fdash_o2
    ! estimates derivative of po2

!!! Inputs
    real(dp), intent(in) :: c_cap_co2, c_cap_o2_init, p_cap_co2, p_cap_o2_init, pH_c, v_q, sat_c_init
!!! Locals
    integer :: k
    real(dp) :: c_cap_o2, fdash, fun_o2, sat_c

    sat_c = sat_c_init
    
    if (abs(v_q) <= 1.0e-3_dp) then
       ! no ventilation: capillary O2 tends to venous O2
       p_cap_o2 = bg_state%p_ven_o2 ! use stored global value
    elseif(abs(v_q) > 100.0_dp)then
       ! essentially infinite ventilation: alveolar/capillary O2 ~ inspired value
       p_cap_o2 = p_i_o2
    else ! calculate the steady-state p_cap_o2
       p_cap_o2 = p_cap_o2_init
       c_cap_o2 = c_cap_o2_init
       k = 0
       do
          fun_o2 = function_o2(v_q, c_cap_o2, p_cap_o2)
          if (abs(fun_o2) < 1.0e-4_dp) exit
          if (k >= 200) exit
          fdash = fdash_o2(p_cap_co2, p_cap_o2, v_q, pH_c)
          if (abs(fdash) < zero_tol) exit
          p_cap_o2 = p_cap_o2 - fun_o2/fdash
          sat_c = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH_c)
          c_cap_o2 = o2_content_from_po2(p_cap_co2, p_cap_o2, sat_c)
          k = k + 1
       enddo
    endif
          
    ! including a limitation that p_cap_o2 cannot be less than p_ven_o2
    p_cap_o2 = max(p_cap_o2, bg_state%p_ven_o2)
          
  end function unit_o2_steadystate
  
!!!##############################################################################

  pure real(dp) function function_o2( v_q, c_cap_o2, p_cap_o2) result (fun_o2)
    ! O₂ flux mismatch across alveolar-capillary interface for given V/Q state
    ! calculates the residual of O2 mass balance across capillary
    
!!! Inputs
    real(dp),intent (in) :: c_cap_o2, p_cap_o2, v_q

    ! use K_stpd to convert BTPS on airside to STPD on blood side
    fun_o2 = v_q * (p_i_o2 - p_cap_o2) - K_stpd * (c_cap_o2 - bg_state%c_ven_o2)

  end function function_o2

!!!##############################################################################

  real(dp)  function fdash_o2 (p_co2, p_o2, v_q, pH_c) result (fdash)
    ! d/dPO₂ of O₂ flux mismatch using Hb saturation slope
    ! computes Jacobian term for O₂ balance equation using saturation model derivative
    ! (either Kelman or Dash), including dissolved and Hb-bound O₂ contributions
    
    use parameter_types
!!! Inputs
    real(dp),intent(in) :: p_co2, p_o2, v_q, pH_c
!!! Locals
    real(dp) :: C, dsat_o2_dp

    select case (trim(gx_params%sat_model))
    case('kelman')
       dsat_o2_dp = fdash_o2_kelman(p_co2, p_o2, pH_c)
    case('dash')
       dsat_o2_dp = fdash_o2_dash(p_co2, p_o2, pH_c)
    end select
    
    C = (constants%Wbl * constants%alphaO2 + 4.0_dp * Hb_conc * &
         dsat_o2_dp * (constants%o2molvol_stpd*1.0e-3_dp))
    
    fdash = -v_q - K_stpd * C

  end function fdash_o2

!!!##############################################################################

  real(dp) function fdash_o2_kelman (p_co2, p_o2, pH_c) result (dsat_o2_dp)

    use parameter_types, only: gx_params
!!! Inputs
    real(dp),intent(in) :: p_co2, p_o2, pH_c
!!! Locals
    real(dp) :: aa, bb, aa_dash, bb_dash, gamma, X
    
    gamma = 10.0_dp**(0.024_dp*(37.0_dp - gx_params%body_temp) + 0.4_dp * (pH_c - 7.4_dp) + &
         0.06_dp * (log10(40.0_dp) - log10(p_co2)))
    X = p_o2*gamma
    aa = (X*(X*(X*(X+A3)+A2)+A1))
    bb = (X*(X*(X*(X+A7)+A6)+A5)+A4)
    aa_dash = gamma*(4.0_dp*X**3 + 3.0_dp*A3*X**2 + 2.0_dp*A2*X+A1)
    bb_dash = gamma*(4.0_dp*X**3 + 3.0_dp*A7*X**2 + 2.0_dp*A6*X+A5)
    
    dsat_o2_dp = (aa_dash*bb-aa*bb_dash)/bb**2

  end function fdash_o2_kelman
  
!!!##############################################################################

  real(dp)  function fdash_o2_dash (p_co2, p_o2, pH_c) result (dsat_o2_dp)
    ! consistent with the Dash et al. model for O2 saturation
    
    use parameter_types, only: gx_params
!!! Inputs
    real(dp),intent(in) :: p_co2, p_o2, pH_c
!!! Locals
    real(dp) :: x, nH, dnH_dx, P50, P50_std
    real(dp) :: f_pH, f_CO2, f_T
    real(dp) :: r, logterm

    P50_std = 26.8_dp
    x = max(p_o2, 1.0e-6_dp)

    ! variable Hill coefficient
    nH = 2.82_dp - 1.20_dp * exp(-x / 29.25_dp)
    dnH_dx = (1.20_dp / 29.25_dp) * exp(-x / 29.25_dp)

    ! P50 model
    f_pH = exp( 1.10_dp*(7.40_dp - pH_c) + 0.05_dp*(7.40_dp - pH_c)**2 )
    f_CO2 = (p_co2 / 40.0_dp)**0.06_dp
    f_T   = exp( 0.02_dp * (gx_params%body_temp - 37.0_dp) )
    
    P50 = P50_std * f_pH * f_CO2 * f_T
    
    logterm = log(P50 / x)
    r = exp(nH * logterm)
    
    dsat_o2_dp = (r / (1.0_dp + r)**2) * (nH / x - dnH_dx * logterm)
    
    if (p_o2 <= 1.0e-6_dp) then
       dsat_o2_dp = 0.0_dp
    end if
    
  end function fdash_o2_dash

!!!##############################################################################

  real(dp) function o2_content_from_po2 (p_co2, p_o2, sat_o2) result(c_from_po2)
!!! Kelman method for calculating the content of O2 from partial pressure

    use parameter_types, only: contants
!!! Inputs
    real(dp) :: p_co2, p_o2, sat_o2

    ! call initialisation of gas exchange parameters
    if(.not.initialised_gastransfer)then
       call derived_gx_params()
    endif
    
    if(abs(p_o2).lt.zero_tol)then
       c_from_po2 = 0.0_dp
    else
!!! Calculate O2 content (convert from molar to ml O2 per ml blood)
!!! o2molvol is in units of mm^3/mmol; alphaO2 is mol/mmHg; content should be ml/ml
       c_from_po2 = (constants%Wbl * constants%alphaO2 * p_o2 + 4.0_dp * Hb_conc * sat_o2) * &
            (constants%o2molvol_stpd * 1.0e-3_dp)
    endif

    if(c_from_po2.LT.0.0_dp) c_from_po2=0.0_dp !curve fit behaves poorly at low PO2

  end function o2_content_from_po2

!!!##############################################################################

  pure real(dp) function saturation_of_o2 (c_co2, p_co2, p_o2, pH) result(sat_o2)

    use parameter_types, only: gx_params
!!! Inputs
    real(dp),intent(in) :: c_co2, p_co2, p_o2, pH

    if(abs(p_o2) <= zero_tol)then
       sat_o2 = 0.0_dp
       return
    endif
    
    select case (trim(gx_params%sat_model))
    case ('kelman')
       sat_o2 = saturation_kelman(p_co2, p_o2, ph)
    case ('dash')
       sat_o2 = saturation_dash(p_co2, p_o2, ph)
    case('valsecchi')
       sat_o2 = saturation_valsecchi(p_co2, p_o2)
    end select

    sat_o2 = max(0.0_dp, sat_o2)
    sat_o2 = min(1.0_dp, sat_o2)

  end function saturation_of_o2

!!!##############################################################################

  pure real(dp) function saturation_kelman(p_co2, p_o2, ph) result(sat)

    use parameter_types, only: gx_params
!!! Inputs
    real(dp),intent(in) :: p_co2, p_o2, ph
!!! Locals
    real(dp) :: X
    
    ! estimate effective PO2 (X) taking into account Bohr shift due to temperature (0.024/degC),
    ! pH (0.4/unit), pCO2 (0.06 x log10)
    X = p_o2 * 10.0_dp** (0.024_dp * (37.0_dp - gx_params%body_temp) + 0.4_dp * (pH - 7.4_dp) + &
         0.06_dp * (log10(dble(40.0_dp)) - log10(dble(p_co2))))
    ! estimate SHbO2 from 4th order rational polynomial fitted by Kelman. Horner's method for efficiency
    sat = (X*(X*(X*(X+A3)+A2)+A1))/(X*(X*(X*(X+A7)+A6)+A5)+A4)

  end function saturation_kelman
    
!!!##############################################################################

  pure real(dp) function saturation_valsecchi(pco2, po2) result(sat)
    ! empirical model from Valsecchi et al., Front Med 12 Jan 2026
    ! Sec Intensive Care Medicine and Anesthesiology
    ! volume 12, https://doi.org/10.3389/fmed.2025.1708274
    
!!! Inputs
    real(dp), intent(in) :: po2, pco2
!!! Local parameters
    real(dp), parameter :: a      = 97.1399_dp
    real(dp), parameter :: bfix   = 9.2308_dp
    real(dp), parameter :: bpCO2  = 0.1062_dp
    real(dp), parameter :: x0fix  = 11.1305_dp
    real(dp), parameter :: x0PCO2 = 0.1793_dp
    real(dp), parameter :: s2e    = 3.2723_dp
!!! Local variables
    real(dp) :: b, x0
    
    b  = bfix + bpCO2 * pco2
    x0 = x0fix + x0PCO2 * pco2
    sat  = a * exp(-exp(-(po2 - x0)/b))/100.0_dp
    
  end function saturation_valsecchi
  
!!!##############################################################################

  pure real(dp) function saturation_dash(pco2, po2, pH) result(sat)
    ! stripped-back model from Dash et al., Eur J Appl Physiol 2016
    ! volume 116(1):97-113
    ! note that the model is very dependent on pH
    
    use parameter_types, only: gx_params
!!! Inputs
    real(dp), intent(in) :: po2, pH, pco2
!!! Locals
    real(dp) :: nH, P50, P50_std
    real(dp) :: f_pH, f_CO2, f_T
    real(dp) :: po2c
    
    P50_std = 26.8_dp   ! mmHg
    po2c = max(po2, 1.0e-6_dp)
    
    ! variable Hill coefficient (Eq. 11)
    nH = 2.82_dp - 1.20_dp * exp(-po2c / 29.25_dp)
    
    ! P50 model (Eqs. 9 + 10 simplified)
    ! --- pH effect (dominant, includes curvature) ---
    f_pH = exp( 1.10_dp*(7.40_dp - pH) + 0.05_dp*(7.40_dp - pH)**2 )
    ! --- CO2 effect (secondary) ---
    f_CO2 = (pco2 / 40.0_dp)**0.06_dp
    ! --- temperature effect ---
    f_T = exp( 0.02_dp * (gx_params%body_temp - 37.0_dp) )
    
    P50 = P50_std * f_pH * f_CO2 * f_T
    
    ! saturation (Eq. 6)
    sat = po2c**nH / (po2c**nH + P50**nH)
    sat = max(0.0_dp, min(1.0_dp, sat))
    
  end function saturation_dash
  
!!!##############################################################################

  real(dp) function co2_content_from_pco2(p_co2, p_o2, pH, sat_o2) result(c_mlml)
    ! Implementation of Douglas et al., J Appl Physiol, 65(1): 473-477, 1985.
    ! returns content of CO2 in mL STPD/mL blood

    use parameter_types, only: constants
!!! Inputs
    real(dp), intent(in) :: p_co2, p_o2, sat_o2
!!! Local parameters
    real(dp), parameter :: conv_mM_mldl = 2.226_dp ! conversion factor from mM to mL/dL
    real(dp), parameter :: tol = 1.0e-5_dp
!!! Local variables
    integer :: k, max_itn
    real(dp) :: blood_factor, c_plasma_mldl, c_blood_mldl, pH, pkp

    !! Apparent dissociation constant for plasma CO2-bicarbonate system
    !! Eq. (5), which comes from Kelman, Respir Physiol, 3: 111-115, 1967.
    pkp = 6.086_dp + 0.042_dp*(7.4_dp - pH) + (38.0_dp - gx_params%body_temp) * &
         (0.0047_dp + 0.0014_dp * (7.4_dp - pH))
    
    !! From Eq. (1). Plasma CO2 content in mL STPD / dL. 2.226 converts mmol/L to mL STPD/dL
    !! Limitation: alphaCO2 (temperature dependent) is for T=37
    c_plasma_mldl = conv_mM_mldl * constants%alphaCO2 * p_co2 * (1.0_dp + 10.0_dp**(pH - pkp))
    
    ! From Eq. (6), Douglas whole-blood correction factor; so2_frac must be 0-1 here
    blood_factor = 1.0_dp - (0.0289_dp * Hb_g_dL) / &
         ((3.352_dp - 0.456_dp * sat_o2) * (8.142_dp - pH))
    
    c_blood_mldl = c_plasma_mldl * blood_factor  ! Whole-blood CO2 content in mL STPD / dL
    c_mlml = c_blood_mldl / 100.0_dp  ! Convert mL/dL -> mL/mL
    
  end function co2_content_from_pco2

!!!##############################################################################
  
  real(dp) function pco2_from_co2content(c_co2, p_co2_init, p_o2) result(p_co2)

!!! Inputs
    real(dp), intent(in) :: c_co2, p_co2_init, p_o2
!!! Local parameters
    integer,  parameter :: itmax = 80
    real(dp), parameter :: tol = 1.0e-8_dp
!!! Local variables
    integer :: it
    real(dp) :: c_x_co2, lo, hi, mid, f_lo, f_hi, f_mid, pH_mid, sat
    
    p_co2 = p_co2_init
    
    ! Initial bracket around the guess
    lo = max(0.1_dp, 0.5_dp * p_co2)
    hi = min(400.0_dp, 1.5_dp * p_co2)
    
    pH_mid = pH_funct_CO2(lo, c_co2)
    sat = saturation_of_o2(c_co2, p_co2, p_o2, pH_mid) ! using previous iteration c_cap_co2
    f_lo = co2_content_from_pco2(lo, p_o2, ph_mid, sat) - c_co2
    
    pH_mid = pH_funct_CO2(hi, c_co2)
    sat = saturation_of_o2(c_co2, p_co2, p_o2, pH_mid) ! using previous iteration c_cap_co2
    f_hi = co2_content_from_pco2(hi, p_o2, ph_mid, sat) - c_co2
    
    ! Expand bracket if needed
    do while (f_lo * f_hi > 0.0_dp)
       lo = max(0.1_dp, 0.5_dp * lo)
       hi = min(400.0_dp, 2.0_dp * hi)
       
       pH_mid = pH_funct_CO2(lo, c_co2)
       sat = saturation_of_o2(c_co2, p_co2, p_o2, pH_mid) ! using previous iteration c_cap_co2
       f_lo = co2_content_from_pco2(lo, p_o2, ph_mid, sat) - c_co2
       
       pH_mid = pH_funct_CO2(hi, c_co2)
       sat = saturation_of_o2(c_co2, p_co2, p_o2, pH_mid) ! using previous iteration c_cap_co2
       f_hi = co2_content_from_pco2(hi, p_o2, ph_mid, sat) - c_co2
       
       if (lo <= 0.1_dp .and. hi >= 400.0_dp) exit
    end do
    
    ! If still unbracketed, return the initial guess clipped to range
    if (f_lo * f_hi > 0.0_dp) then
       p_co2 = max(0.1_dp, min(400.0_dp, p_co2))
       return
    end if
    
    ! Bisection solve
    do it = 1, itmax
       mid = 0.5_dp * (lo + hi)
       pH_mid = pH_funct_CO2(mid, c_co2)
       sat = saturation_of_o2(c_co2, p_co2, p_o2, pH_mid) ! using previous iteration c_cap_co2
       f_mid = co2_content_from_pco2(mid, p_o2, ph_mid, sat) - c_co2
       
       if (abs(f_mid) < tol) exit
       
       if (f_lo * f_mid <= 0.0_dp) then
          hi = mid
          f_hi = f_mid
       else
          lo = mid
          f_lo = f_mid
       end if
    end do
    
    p_co2 = 0.5_dp * (lo + hi)
    
  end function pco2_from_co2content

!!!##############################################################################
  
  real(dp) function po2_from_o2content(c_co2, c_o2, p_co2, p_o2_init, pH, sat_o2_init) result(p_o2)

!!! Inputs
    real(dp), intent(in) :: c_co2, c_o2, p_co2, p_o2_init, pH, sat_o2_init
!!! Local parameters
    integer,  parameter :: itmax = 80
    real(dp), parameter :: tol = 1.0e-8_dp
!!! Local variables
    real(dp) :: lo, hi, mid, f_lo, f_hi, f_mid, sat
    integer :: it
    
    p_o2 = p_o2_init
    
    ! Initial bracket around the guess
    lo = max(0.1_dp, 0.5_dp * p_o2)
    hi = min(400.0_dp, 1.5_dp * p_o2)
    
    f_lo = o2_content_from_po2(p_co2, lo, sat_o2_init) - c_o2
    f_hi = o2_content_from_po2(p_co2, hi, sat_o2_init) - c_o2

    ! Expand bracket if needed
    do while (f_lo * f_hi > 0.0_dp)
       lo = max(0.1_dp, 0.5_dp * lo)
       hi = min(400.0_dp, 2.0_dp * hi)
       
       sat = saturation_of_o2(c_co2, p_co2, lo, pH) ! using previous iteration c_cap_o2
       f_lo = o2_content_from_po2(p_co2, lo, sat) - c_o2
       
       sat = saturation_of_o2(c_co2, p_co2, hi, pH) ! using previous iteration c_cap_o2
       f_hi = o2_content_from_po2(p_co2, hi, sat) - c_o2
       
       if (lo <= 0.1_dp .and. hi >= 400.0_dp) exit
    end do
    
    ! If still unbracketed, return the initial guess clipped to range
    if (f_lo * f_hi > 0.0_dp) then
       p_o2 = max(0.1_dp, min(400.0_dp, p_o2))
       return
    end if
    
    ! Bisection solve
    do it = 1, itmax
       mid = 0.5_dp * (lo + hi)
       sat = saturation_of_o2(c_co2, p_co2, mid, pH) ! using previous iteration c_cap_co2
       f_mid = o2_content_from_po2(p_co2, mid, sat) - c_o2
       
       if (abs(f_mid) < tol) exit
       
       if (f_lo * f_mid <= 0.0_dp) then
          hi = mid
          f_hi = f_mid
       else
          lo = mid
          f_lo = f_mid
       end if
    end do
    
    p_o2 = 0.5_dp * (lo + hi)
    
  end function po2_from_o2content

!!!##############################################################################

  real(dp) function function_co2(v_q, c_cap_co2, p_cap_co2) result (fun_co2)

!!! Inputs
    real(dp), intent(in) :: v_q, c_cap_co2, p_cap_co2
!!! Local parameters
    real(dp), parameter :: p_i_co2 = 0.0_dp
    
    ! use K_stpd to convert BTPS on airside to STPD on blood side
    fun_co2 = v_q * (p_cap_co2 - p_i_co2) - K_stpd * (bg_state%c_ven_co2 - c_cap_co2)

  end function function_co2
    
!!!##############################################################################

  real(dp) function fdash_co2(v_q, c_cap_co2, p_cap_co2, p_cap_o2) result(fdash_co2)
    ! Numerical derivative of content_co2 w.r.t. p_cap_co2

!!! Inputs
    real(dp),intent(in) :: v_q, c_cap_co2, p_cap_co2, p_cap_o2
!!! Local parameters
    real(dp), parameter :: delta = 0.1_dp ! mmHg perturbation — small enough for accuracy
!!! Local variables
    real(dp) :: dC_dP, c_plus, c_minus, pH, sat

    ! Numerical derivative of CO2 content w.r.t. PCO2
    pH = pH_funct_CO2(p_cap_co2 + delta, c_cap_co2) ! using previous iteration c_cap_co2
    sat = saturation_of_o2(c_cap_co2, p_cap_co2 + delta, p_cap_o2, pH) ! using previous iteration c_cap_co2
    c_plus  = co2_content_from_pco2(p_cap_co2 + delta, p_cap_o2, pH, sat)

    pH = pH_funct_CO2(p_cap_co2 - delta, c_cap_co2) ! using previous iteration c_cap_co2
    sat = saturation_of_o2(c_cap_co2, p_cap_co2 - delta, p_cap_o2, pH) ! using previous iteration c_cap_co2
    c_minus = co2_content_from_pco2(p_cap_co2 - delta, p_cap_o2, pH, sat)

    dC_dP   = (c_plus - c_minus) / (2.0_dp * delta)

    ! use K_stpd to convert BTPS on airside to STPD on blood side
    fdash_co2 = v_q + K_stpd * dC_dP

  end function fdash_co2

!!!##############################################################################

  real(dp) function pH_funct_CO2(p_co2, c_co2) result(pH)
    ! using the simplest approximation. more complicated ones don't
    ! work well in this current framework because of the interdependence
    ! between content_CO2, pH, sat etc.

!!! Inputs
    real(dp),intent(in) :: p_co2, c_co2

    pH = 7.4_dp - 0.004_dp * (p_co2 - 40.0_dp)

  end function pH_funct_CO2

!!!##############################################################################

end module gas_exchange

