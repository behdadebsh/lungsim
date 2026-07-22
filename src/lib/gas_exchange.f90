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

  !Interfaces

  public  get_ABG_value            ! return a value stored in bg_o2 or bg_co2
  public  o2_content_from_po2      ! calculate O2 content for a given PO2 (and PCO2, sat)
  public  solve_gasexchange        ! solve gas exchange during insp, breath-hold, or expn over defined time interval
  public  steadystate_co2          ! calculation of steady-state CO2 in gx units based on mass balance
  public  steadystate_o2           ! calculation of steady-state O2 in gx units based on mass balance

  private calc_O2_diffusion_capacity ! diffusion capacity for o2
  private co2_content_from_pco2    ! calculate CO2 content for a given PCO2 (and PO2, pH, sat)
  private derived_gx_params        ! calculate variable values that have been declared above
  private fdash_co2                ! --helper: derivative of CO2 balance residual wrt capillary PCO2
  private fdash_o2                 ! --helper: derivative of O2 balance residual wrt capillary PO2
  private fdash_o2_kelman          ! --option: Kelman model for Hb saturation slope
  private fdash_o2_dash            ! --option: Dash model for Hb saturation slope
  private flow_weighted_distribution
  private function_co2             ! --helper: residual of CO2 mass balance
  private function_o2              ! --helper: residual of O2 mass balance
  private gas_exchange_unit_o2_step ! solve in one gx unit for one time step
  private get_path_length
  private get_unit_v_q             ! calculate v/q ratio for a single unit
  private initialise_gasexchange   ! allocate memory and initialise for time-dep gas exchange problem
  private initialise_gastransfer   ! allocate memory and initialise for s-s gas transfer problems
  private openfile                 ! for opening file as new or to append. move to general modules when used elsewhere
  private o2_exchange_in_units     ! calculate time-dependent o2 exchange in all gx units for given time period
  private mmol_in_air, mmol_in_blood
  private pH_funct_CO2             ! simple estimation of PH as a function of PCO2
  private pco2_from_co2content     ! calculate PCO2 for a given CO2 content (and PO2)
  private po2_from_o2content       ! calculate PO2 for a given O2 content (and PCO2, pH)
  private saturation_dash          ! --option: Bassingthwaigthe group. Expands to very detailed model (not here)
  private saturation_kelman        ! --option: accurate around normal PCO2, not good for extremes of PCO2
  private saturation_of_o2         ! saturation of O2 for a given PO2, c_CO2, PCO2, pH 
  private saturation_valsecchi     ! --option: empirical, very accurate at 37C and normal pH. no pH dependence
  private sparse_gasexchange       ! allocates memory and sets up sparsity arrays
  private unit_co2_steadystate     ! steady-state CO2 transfer for a single gx unit
  private unit_o2_steadystate      ! steady-state O2 transfer for a single gx unit
  private update_volumes_below     ! update elem_field(ne_vol_bel,:) to the current volume
  private update_terminal_conc_from_unit
  private write_field
  private assemble_gasmix, element_gasmix, reduce_gasmix ! for assembling and reducing matrix system
  ! tracking subroutines
  private append_expn_source_elem, append_expn_source_unit, build_expn_sources_for_node, &
       build_insp_sources_for_node, build_insp_sources_for_unit, clear_expiration_sources, &
       clear_inspiration_sources, ensure_expn_capacity, general_track, init_expn_tracking, &
       init_insp_tracking, tracking_step_expn, tracking_step_insp
  
  !Module parameters

  ! coefficients in the Kelman model for SHbO2
  real(dp), parameter :: A1 = -8.538889e+3_dp, A2 = 2.121401e+3_dp, A3 = -6.707399e+1_dp, &
       A4 = 9.359609e+5_dp, A5 = -3.134626e+4_dp, A6=2.396167e+3_dp, A7=-6.710441e+1_dp

  !Module types

  ! key global physiological variables that are carried over between calls to the module's functions
  type :: bloodgas_o2
     ! global blood gas and acid-base variables associated with O2. 
     ! initialised here to reasonable values; updated by the models
     real(dp) :: p_art_o2  = 100.0_dp
     real(dp) :: p_alv_o2  = 100.0_dp
     real(dp) :: p_cap_o2  = 100.0_dp
     real(dp) :: p_ven_o2  = 40.0_dp 
     real(dp) :: c_art_o2  = 0.2_dp
     real(dp) :: c_cap_o2  = 0.2_dp
     real(dp) :: c_ven_o2  = 0.15_dp
     real(dp) :: sat_art = 0.97_dp
     real(dp) :: sat_ven = 0.75_dp
     real(dp) :: time_av_p_art_o2 = 100.0_dp
     real(dp) :: time_av_p_alv_o2 = 100.0_dp
     real(dp) :: time_av_p_cap_o2 = 100.0_dp
     real(dp) :: vo2_blood = 0.0_dp
  end type bloodgas_o2

  type :: bloodgas_co2
     ! global blood gas and acid-base variables associated with CO2. 
     ! initialised here to reasonable values; updated by the models
     real(dp) :: p_art_co2 = 40.0_dp 
     real(dp) :: p_ven_co2 = 45.0_dp 
     real(dp) :: c_art_co2 = 0.47_dp
     real(dp) :: c_ven_co2 = 0.51_dp
     real(dp) :: pH_art = 7.4_dp
     real(dp) :: pH_ven = 7.37_dp
  end type bloodgas_co2

  type(bloodgas_o2)  :: bg_o2  ! used in the code, allowing for values to be updated
  type(bloodgas_co2) :: bg_co2 ! used in the code, allowing for values to be updated

  type track_gx_solution
     ! type to track important features/variables associated with gas exchange
     real(dp) :: total_o2_uptake = 0.0_dp
     real(dp) :: current_o2_uptake = 0.0_dp
     real(dp) :: time_in_transit = 0.0_dp
     real(dp) :: total_transit_time = 0.0_dp
     real(dp) :: time_in_breath = 0.0_dp
     real(dp) :: total_time = 0.0_dp
     real(dp) :: VO2_fick = 0.0_dp
     real(dp) :: init_blood_mmol
     real(dp) :: init_air_mmol
     integer  :: breath_num
  end type track_gx_solution

  type(track_gx_solution) :: track_gx_soln

  !Module variables

  ! derived parameters - calculated once in update_derived (called from initialisation subroutines)
  real(dp) :: c_i_o2              ! mmol/mm^3, concentration of inspired O2
  real(dp) :: p_i_o2              ! mmHg, partial pressure of inspired O2
  real(dp) :: p_atm_dry           ! mmHg, atmospheric pressure less water vapour pressue - at 37 degrees
  real(dp) :: o2_cnv_pp2c         ! (mmol/mm^3)/mmHg, conversion from partial pressure (mmHg) to concentration (mmol/mm^3)
  real(dp) :: o2_cnv_c2pp         ! mmHg/(mmol/mm^3), conversion from concentration (mmol/mm^3) to partial pressure (mmHg)
  real(dp) :: Hb_conc             ! concentration of Hb, mmol/mm^3
  real(dp) :: pH_a                ! pH of arterial blood
  real(dp) :: pH_v                ! pH of venous blood
  real(dp) :: Hb_g_dL             ! haemoglobin in g/dL
  real(dp) :: K_stpd              ! conversion from BTPS to STPD using K = P_B * T/(273.15)
  real(dp) :: initial_model_mmol  ! gas mmol when the model is initialised
  real(dp) :: prior_model_mmol    ! store the mmol at the end of a call to solve_gasexchange
  real(dp) :: mmol_inhaled = 0.0_dp, mmol_exhaled = 0.0_dp
  logical  :: initialised_gastransfer = .false.
  logical  :: initialised_gasexchange = .false.
  logical  :: initialised_sparsity = .false.
  character (len=11) :: current_phase = 'expiration'

  ! allocatable arrays for solving FE gas exchange model
  integer,  allocatable :: sparsity_col(:), reduced_col(:), sparsity_row(:), reduced_row(:), solution(:)
  real(dp), allocatable :: global_k(:), global_m(:), global_aa(:), global_bb(:), global_r(:)

  ! variables to do with the FE matrix solution. need to be carried between calls
  integer :: matrixsize, noffset_entry, noffset_row, nonzeros, nonzeros_unreduced

  ! for tracking component of gas exchange model
  ! For each node: CSR pointers into source arrays
  integer,  allocatable, save :: sptr(:)          ! (num_nodes+1)
  integer,  allocatable, save :: s_id(:)          ! source id: node index or unit index
  real(dp), allocatable, save :: s_w(:)           ! weights (sum to 1 typically)
  logical,  allocatable, save :: s_is_unit(:)     ! true -> unit source, false -> node source

  ! Source records (length = nsrc)
  integer,  allocatable, save :: src_elem(:)     ! element id
  integer,  allocatable, save :: src_unit(:)     ! unit index if src_is_unit
  real(dp), allocatable, save :: src_xi(:)       ! xi within element
  real(dp), allocatable, save :: src_w(:)        ! weight
  logical,  allocatable, save :: src_is_unit(:)   ! if true, use unit BC

  ! For nodal advection
  integer,  allocatable, save :: src1(:), src2(:)
  real(dp), allocatable, save :: w1(:), w2(:)

  integer,  allocatable, save :: uptr(:)   ! (num_units+1)
  integer,  allocatable, save :: pelem(:)  ! element id per path entry
  real(dp), allocatable, save :: pfrac(:)  ! segment fraction (0..1), distal portion within dt
  real(dp), allocatable, save :: ppf(:)    ! cumulative branch flow fraction down to unit
  real(dp), allocatable, save :: pxi(:)    ! xi for partial (>=0); -1 for full segments

  ! If a node backtracks beyond the model inlet, source from inlet_conc
  logical,  allocatable, save :: node_from_inlet(:)
  real(dp), allocatable, save :: node_inlet_w(:)

  ! Map distal terminal node -> unit index (0 if none)
  integer,  allocatable, save :: unit_at_node(:)

  logical, save :: maps_built = .false.
  logical, save :: built = .false.
  real(dp), save :: dt_built  = -1.0_dp

  
contains
  
!!!##############################################################################

  subroutine initialise_gasexchange()
    ! allocate memory as required and initialise array types for gas transfer

    use parameter_types, only: lung_params, Q_params
!!! Locals
    integer :: nunit
    real(dp) :: flow_scale, pH_c
    
    call derived_gx_params()

    if(.not.initialised_gastransfer)then
       call initialise_gastransfer()
       initialised_gastransfer = .true.
    endif

    ! assuming that we are starting from venous pO2 in capillaries
    gasex%p_cap_o2 = bg_o2%p_ven_o2
    gasex%c_cap_o2 = bg_o2%c_ven_o2
    do nunit = 1,num_units
       pH_c = pH_funct_CO2(gasex%p_cap_co2(nunit), gasex%c_cap_co2(nunit)) 
       gasex%sat_cap(nunit) = saturation_of_o2(gasex%c_cap_co2(nunit), gasex%p_cap_co2(nunit), &
            gasex%p_cap_o2(nunit), pH_c)
    enddo

!!! allocate array types only needed for gas exchange
    if(.not.allocated(gasex%V_cap))  allocate(gasex%V_cap(num_units))
    if(.not.allocated(gasex%S_area)) allocate(gasex%S_area(num_units))
    if(.not.allocated(gasex%t_time)) allocate(gasex%t_time(num_units))
    if(.not.allocated(gasex%t_in_transit)) allocate(gasex%t_in_transit(num_units))
    if(.not.allocated(gasex%volume)) allocate(gasex%volume(num_units))

    track_gx_soln%total_transit_time = lung_params%capillary_volume / &
         ((1.0_dp - Q_params%shunt_fraction) * Q_params%cardiac_output)

    track_gx_soln%total_o2_uptake = 0.0_dp
    track_gx_soln%current_o2_uptake = 0.0_dp

    ! use 'flow_scale' so that we are agnostic of Q field being absolute or normalised
    flow_scale = Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction) / elem_field(ne_Qdot,1)

    ! the following assumes that TT is fixed, and V_cap from TT = V_cap/Qcap
    gasex%t_time = lung_params%capillary_volume / &
         (Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction))
    gasex%V_cap = gasex%Qdot * gasex%t_time * flow_scale
    gasex%S_area = gasex%V_cap / lung_params%capillary_volume * lung_params%surface_area

    gasex%t_in_transit = 0.0_dp

    gasex%volume(:) = unit_field(nu_vol,:)

    track_gx_soln%init_air_mmol = mmol_in_air(nj_conc1, gasex%conc_o2)
    track_gx_soln%init_blood_mmol = mmol_in_blood()
    mmol_inhaled = 0.0_dp
    mmol_exhaled = 0.0_dp
    track_gx_soln%time_in_breath = 0.0_dp
    track_gx_soln%breath_num = 0

!!! allocate and set up the unreduced sparsity arrays
    call sparse_gasexchange()

    initialised_gasexchange = .true.
    
  end subroutine initialise_gasexchange
  
!!!##############################################################################

  subroutine sparse_gasexchange()

!!! Locals
    integer :: n_unreduced, i, ncol, ne, ne2, np1, np2, nrow

!!! allocate the arrays for solving gas exchange
    if(.not.allocated(sparsity_col)) allocate(sparsity_col(1+3*(num_nodes-1)))
    if(.not.allocated(reduced_col))  allocate(reduced_col(1+3*(num_nodes-1)))
    if(.not.allocated(sparsity_row)) allocate(sparsity_row(num_nodes+1))
    if(.not.allocated(reduced_row))  allocate(reduced_row(num_nodes+1))
    if(.not.allocated(global_K))     allocate(global_K(1+3*(num_nodes-1)))
    if(.not.allocated(global_M))     allocate(global_M(1+3*(num_nodes-1)))
    if(.not.allocated(global_AA))    allocate(global_AA(1+3*(num_nodes-1)))
    if(.not.allocated(global_BB))    allocate(global_BB(num_nodes))
    if(.not.allocated(global_R))     allocate(global_R(num_nodes))

!!! set up the arrays using sparsity structures

    sparsity_row(1) = 1
    n_unreduced = 1

    do ne = 1,num_elems ! note using local numbering
       if(elem_cnct(-1,0,ne).eq.0)then !at the inlet
          np1 = elem_nodes(1,ne) ! start node
          nrow = np1
          do i = 1,2
             np2 = elem_nodes(i,ne)
             ncol = np2
             sparsity_col(n_unreduced) = ncol
             n_unreduced = n_unreduced+1
          enddo
          sparsity_row(nrow+1) = n_unreduced
       endif

       np1 = elem_nodes(2,ne) !end node
       nrow = np1
       do i = 1,2
          np2 = elem_nodes(i,ne)
          ncol = np2
          sparsity_col(n_unreduced) = ncol
          n_unreduced = n_unreduced+1
       enddo
       do i = 1,elem_cnct(1,0,ne) ! for each child branch
          ne2 = elem_cnct(1,i,ne)
          np2 = elem_nodes(2,ne2)
          ncol = np2
          sparsity_col(n_unreduced) = ncol
          n_unreduced = n_unreduced+1
       enddo
       sparsity_row(nrow+1) = n_unreduced
    enddo !noelem

    NonZeros_unreduced = n_unreduced - 1

  end subroutine sparse_gasexchange

!!!##############################################################################

  subroutine initialise_gastransfer()
    ! allocate memory as required and initialise array types for gas transfer
    
    use parameter_types, only: constants, gx_params
    ! gasex is declared in module 'arrays'
    
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

    gasex%p_alv_o2  = bg_o2%p_art_o2
    gasex%p_cap_o2  = bg_o2%p_art_o2
    gasex%c_cap_o2  = bg_o2%c_art_o2
    gasex%sat_cap   = bg_o2%sat_art
    gasex%conc_o2   = gx_params%init_p_alv_o2 * o2_cnv_pp2c
    
    gasex%p_alv_co2 = bg_co2%p_art_co2
    gasex%p_cap_co2 = bg_co2%p_art_co2
    gasex%c_cap_co2 = bg_co2%c_art_co2
    gasex%ph_cap    = bg_co2%ph_art 
    gasex%conc_co2  = gasex%p_alv_co2 * o2_cnv_pp2c
    
    node_field(nj_conc1,:) = bg_o2%p_art_o2 * o2_cnv_pp2c
    node_field(nj_conc2,:) = bg_co2%p_art_co2 * o2_cnv_pp2c  ! o2molvol is just molar volume for all gas species
    
    node_field(nj_conc1,1) = gx_params%FiO2 / constants%o2molvol_37deg !mmol/mm^3, inspired O2
    node_field(nj_conc2,1) = 0.0_dp ! inspired CO2; should make FiCO2 user-defined

  end subroutine initialise_gastransfer
  
!!!##############################################################################

  subroutine derived_gx_params()
    ! calculates values of variables (which become parameters) declared at top of the module

    use parameter_types, only: constants, gx_params

    p_atm_dry   = gx_params%press_atm - gx_params%press_h2o
    p_i_o2      = gx_params%FiO2 * p_atm_dry ! accounting for humidification by the upper airway
    o2_cnv_c2pp = constants%R * (gx_params%body_temp + 273.15_dp)  ! mm^3.mmHg/mmol = mm^3.mmHg/mmol/K * K
    o2_cnv_pp2c = 1.0_dp / o2_cnv_c2pp
    c_i_o2      = p_i_o2 * o2_cnv_pp2c
    Hb_g_dl     = gx_params%Hb
    Hb_conc     = Hb_g_dL * 10.0_dp / constants%mw * 1.0e-3_dp ! g/dL * 10dL/L / (g/mol) * 10-3 mmol/mm^3 --> mmol/mm^3
    ! concentration of Hb, g/dL * 10 dL/L / (g/mol) * 1e-3 mmol/mm^3 --> mmol/mm^3
    ! note: Hb_conc should be ~= 2.33e-3_dp mM==mmol/L for all species 
    pH_a        = gx_params%pHa
    pH_v        = pH_a - 0.03_dp
    K_stpd      = gx_params%press_atm * (gx_params%body_temp + 273.15_dp) / 273.15_dp

  end subroutine derived_gx_params

!!!##############################################################################

  real(dp) function steadystate_CO2 (Vdot_alv) result(p_art_co2)
    ! Steadystate CO2 model following Kelman. Uses CO2 content<->PCO2 mapping with Haldane coupling:
    ! steady-state is reached when there is no change between current and previous p_ven_co2

    use parameter_types, only: constants, gx_params, Q_params
!!! Inputs
    real(dp), intent(in) :: Vdot_alv
!!! Local parameters
    real(dp), parameter :: err_tol = 1.0e-3_dp
!!! Local variables
    integer :: counter, k, ne, np, nunit
    real(dp) :: c_art_co2, c_cap_co2, c_cap_o2, c_ven_co2, fdash, fun_co2,  &
         p_art_co2_last, p_cap_co2, p_cap_o2, pH_c, p_ven_co2, &
         p_ven_co2_last, sat_c, v_q
    logical :: continue

    if(.not.initialised_gastransfer)then
       ! allocate arrays for O2 and CO2, and initialise
       ! all values to those stored in bloodgas_co2
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif

    ! re-set the CO2 values for bg_co2 and gasex to initial values from bloodgas_co2
    ! don't change O2 values beyond the initialisation
    bg_co2 = bloodgas_co2()
    gasex%p_alv_co2 = bg_co2%p_art_co2
    gasex%p_cap_co2 = bg_co2%p_art_co2
    gasex%c_cap_co2 = bg_co2%c_art_co2
    gasex%ph_cap    = bg_co2%ph_art 
    gasex%conc_co2  = gasex%p_alv_co2 * o2_cnv_pp2c
    node_field(nj_conc2,:) = bg_co2%p_art_co2 * o2_cnv_pp2c  ! o2molvol is just molar volume for all gas species
    node_field(nj_conc2,1) = 0.0_dp ! inspired CO2; should make FiCO2 user-defined
   
    c_ven_co2 = bg_co2%c_ven_co2
    p_art_co2 = bg_co2%p_art_co2
    p_ven_co2 = bg_co2%p_ven_co2
    p_ven_co2_last = bg_co2%p_ven_co2
    p_art_co2_last = bg_co2%p_art_co2

    counter = 1
    continue = .true.

    do while (continue)

       c_art_co2 = 0.0_dp ! initiating a flow-weighted sum
       
       do nunit = 1, num_units
          ne = units(nunit)
          ! Initialise to previous capillary value
          p_cap_co2 = gasex%p_cap_co2(nunit)
          p_cap_o2  = gasex%p_cap_o2(nunit)
          c_cap_co2 = gasex%c_cap_co2(nunit)
          c_cap_o2  = gasex%c_cap_o2(nunit)
          
          pH_c  = gasex%ph_cap(nunit)
          sat_c = gasex%sat_cap(nunit)

          v_q = get_unit_v_q(nunit, Vdot_alv)

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
             c_art_co2 = c_art_co2 + c_cap_co2 * units_effective(nunit) * abs(gasex%Qdot(nunit))
          endif
          
          ! update the gas exchange unit variables
          gasex%p_cap_co2(nunit) = p_cap_co2
          gasex%p_alv_co2(nunit) = p_cap_co2
          gasex%ph_cap(nunit) = pH_c
          gasex%sat_cap(nunit) = sat_c
          gasex%c_cap_co2(nunit) = c_cap_co2

       end do ! nunit

       c_art_co2 = c_art_co2 / (Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction))

       ! add shunt contribution
       c_art_co2 = c_art_co2 + Q_params%shunt_fraction * (c_ven_co2 - c_art_co2)
       p_art_co2 = pco2_from_co2content(c_art_co2, p_art_co2, bg_o2%p_art_o2)
       
       ! update local c_ven_co2 via metabolic VCO2 addition
       c_ven_co2 = c_art_co2 + gx_params%VCO2 / Q_params%cardiac_output   ! units: (ml/ml)
       p_ven_co2 = pco2_from_co2content(c_ven_co2, p_ven_co2, bg_o2%p_ven_o2)

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
    bg_co2%p_art_co2 = p_art_co2
    bg_co2%p_ven_co2 = p_ven_co2
    bg_co2%c_art_co2 = c_art_co2
    bg_co2%c_ven_co2 = c_ven_co2
    bg_co2%pH_art = pH_funct_CO2(p_art_co2, c_art_co2)
    bg_o2%sat_art = saturation_of_o2(c_art_co2, p_art_co2, bg_o2%p_art_o2, bg_co2%pH_art)
    bg_co2%pH_ven = pH_funct_CO2(p_ven_co2, c_ven_co2)
    bg_o2%sat_ven = saturation_of_o2(c_ven_co2, p_ven_co2, bg_o2%p_ven_o2, bg_co2%pH_ven)

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
       p_cap_co2 = bg_co2%p_ven_co2
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
    
    use parameter_types, only: constants, gx_params, Q_params
!!! Inputs
    real(dp),intent(in) :: Vdot_alv
!!! Locals
    integer :: counter, k, ne, np, nunit
    real(dp) :: cardiac_output, c_art_o2, c_cap_co2, c_cap_o2, c_ven_o2, fdash, fun_o2, &
         p_art_o2_last, p_cap_co2, p_cap_o2, pH_c, p_ven_o2, p_ven_o2_last, &
         sat_c, v_q, sum_o2, c_ven_co2
    logical :: continue
    
    ! call initialisation if not already done
    if(.not.initialised_gastransfer)then
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif
    
    ! re-set the O2 values for bg_co2 and gasex to initial values from bloodgas_o2
    ! don't change CO2 values beyond the initialisation
    bg_o2 = bloodgas_o2()
    gasex%p_alv_o2  = bg_o2%p_art_o2
    gasex%p_cap_o2  = bg_o2%p_art_o2
    gasex%c_cap_o2  = bg_o2%c_art_o2
    gasex%sat_cap   = bg_o2%sat_art
    gasex%conc_o2   = gx_params%init_p_alv_o2 * o2_cnv_pp2c
    node_field(nj_conc1,:) = bg_o2%p_art_o2 * o2_cnv_pp2c
    node_field(nj_conc1,1) = gx_params%FiO2 / constants%o2molvol_37deg !mmol/mm^3, inspired O2
    
    c_ven_o2 = bg_o2%c_ven_o2
    p_art_o2 = bg_o2%p_art_o2
    p_ven_o2 = bg_o2%p_ven_o2
    p_ven_o2_last = p_ven_o2
    
    counter = 1
    continue = .true.

    do while (continue)
       
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
          
          v_q = get_unit_v_q(nunit, Vdot_alv)

          p_cap_o2 = unit_o2_steadystate(c_cap_co2, c_cap_o2, p_cap_co2, p_cap_o2, &
               pH_c, v_q, sat_c)
          
          ! update saturation and content for new p_cap_o2
          sat_c = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH_c)
          c_cap_o2 = o2_content_from_po2(p_cap_co2, p_cap_o2, sat_c)
          ! Flow-weighted sum of O2 content
          ! assumes that Qdot is absolute flow value (not proportional) and has shunt subtracted
          c_art_o2 = c_art_o2 + units_effective(nunit) * (c_cap_o2 * abs(gasex%Qdot(nunit)))
          
          ! update the gas exchange unit variables
          gasex%p_cap_o2(nunit) = p_cap_o2
          gasex%p_alv_o2(nunit) = p_cap_o2
          gasex%ph_cap(nunit)   = pH_c
          gasex%sat_cap(nunit)  = sat_c
          gasex%c_cap_o2(nunit) = c_cap_o2

       enddo !nunit

       c_art_o2 = c_art_o2 / (Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction))

       ! just for output:
       c_cap_o2 = c_art_o2
       p_cap_o2 = po2_from_o2content(c_cap_o2, bg_co2%c_art_co2, bg_co2%p_art_co2, &
            bg_o2%p_art_o2, bg_co2%ph_art, bg_o2%sat_art)
       
       ! add shunt contribution
       c_art_o2 = c_art_o2 + Q_params%shunt_fraction * (c_ven_o2 - c_art_o2)
       p_art_o2 = po2_from_o2content(c_art_o2, bg_co2%c_art_co2, bg_co2%p_art_co2, &
            p_art_o2, bg_co2%ph_art, bg_o2%sat_art)

       ! Subtract metabolic consumption of O2 via VO2 to get c_ven_o2
       c_ven_o2 = c_art_o2 - gx_params%VO2 / Q_params%cardiac_output   ! units: (ml/ml)
       p_ven_o2 = po2_from_o2content(c_ven_o2, bg_co2%c_ven_co2, bg_co2%p_ven_co2, &
            p_ven_o2, bg_co2%ph_ven, bg_o2%sat_ven)

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
    bg_o2%p_art_o2 = p_art_o2
    bg_o2%p_ven_o2 = p_ven_o2
    bg_o2%c_art_o2 = c_art_o2
    bg_o2%c_ven_o2 = c_ven_o2
    bg_o2%sat_art = saturation_of_o2(bg_co2%c_art_co2, bg_co2%p_art_co2, p_art_o2, bg_co2%pH_art)
    bg_o2%sat_ven = saturation_of_o2(bg_co2%c_ven_co2, bg_co2%p_ven_co2, p_ven_o2, bg_co2%pH_ven)
    bg_o2%time_av_p_cap_o2 = p_cap_o2
    bg_o2%time_av_p_art_o2 = p_art_o2
    bg_o2%time_av_p_alv_o2 = p_cap_o2

    ! calculate the flow weighted distribution of concentrations to initialise airway field
    call flow_weighted_distribution()
    
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
       p_cap_o2 = bg_o2%p_ven_o2 ! use stored global value
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
          p_cap_o2 = p_cap_o2 - 0.5_dp * fun_o2/fdash
          p_cap_o2 = max(1.0e-2_dp, min(p_cap_o2, p_i_o2))
          sat_c = saturation_of_o2(c_cap_co2, p_cap_co2, p_cap_o2, pH_c)
          c_cap_o2 = o2_content_from_po2(p_cap_co2, p_cap_o2, sat_c)
          k = k + 1
       enddo
    endif
          
    ! including a limitation that p_cap_o2 cannot be less than p_ven_o2
    p_cap_o2 = max(p_cap_o2, bg_o2%p_ven_o2)
          
  end function unit_o2_steadystate
  
!!!##############################################################################

  pure real(dp) function function_o2( v_q, c_cap_o2, p_cap_o2) result (fun_o2)
    ! O₂ flux mismatch across alveolar-capillary interface for given V/Q state
    ! calculates the residual of O2 mass balance across capillary
    
!!! Inputs
    real(dp),intent (in) :: c_cap_o2, p_cap_o2, v_q

    ! use K_stpd to convert BTPS on airside to STPD on blood side
    fun_o2 = v_q * (p_i_o2 - p_cap_o2) - K_stpd * (c_cap_o2 - bg_o2%c_ven_o2)

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
    
    C = (constants%Wbl * constants%alphaO2 + 4.0_dp * Hb_conc * dsat_o2_dp) * constants%o2molvol_stpd ! mmHg-1
    
    fdash = -v_q - K_stpd * C ! dimensionless

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
    
    if (p_o2 <= 1.0_dp) then
       dsat_o2_dp = 0.0_dp
    end if
    
  end function fdash_o2_dash

!!!##############################################################################

  real(dp) function o2_content_from_po2 (p_co2, p_o2, sat_o2) result(c_from_po2)
!!! Kelman method for calculating the content of O2 from partial pressure

    use parameter_types, only: constants
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
!!! o2molvol is in units of mm^3/mmol; alphaO2 is mmol/mm^3/mmHg; content should be ml/ml
!!! Hb_conc is mmol/mm^3
       c_from_po2 = (constants%Wbl * constants%alphaO2 * p_o2 + 4.0_dp * Hb_conc * sat_o2) * &
            constants%o2molvol_stpd
    endif

    if(c_from_po2.LT.0.0_dp) c_from_po2=0.0_dp !curve fit behaves poorly at low PO2

  end function o2_content_from_po2

!!!##############################################################################

  real(dp) function saturation_of_o2 (c_co2, p_co2, p_o2, pH) result(sat_o2)

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

  real(dp) function saturation_dash(pco2, po2, pH) result(sat)
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

  real(dp) function co2_content_from_pco2(p_co2, p_o2, pH, sat_o2) result(c_blood)
    ! Implementation of Douglas et al., J Appl Physiol, 65(1): 473-477, 1985.
    ! returns content of CO2 in mL STPD/mL blood

    use parameter_types, only: constants, gx_params
!!! Inputs
    real(dp), intent(in) :: p_co2, pH, p_o2, sat_o2
!!! Locals
    real(dp) :: blood_factor, c_plasma, pkp

    !! Apparent dissociation constant for plasma CO2-bicarbonate system
    !! Eq. (5), which comes from Kelman, Respir Physiol, 3: 111-115, 1967.
    pkp = 6.086_dp + 0.042_dp*(7.4_dp - pH) + (38.0_dp - gx_params%body_temp) * &
         (0.0047_dp + 0.0014_dp * (7.4_dp - pH))
    
    !! From Eq. (1). Plasma CO2 content in mL/mL. includes conversion to STPD
    !! Limitation: alphaCO2 (temperature dependent) is for T=37
    c_plasma = constants%o2molvol_stpd * constants%alphaCO2 * p_co2 * (1.0_dp + 10.0_dp**(pH - pkp))
    
    ! From Eq. (6), Douglas whole-blood correction factor; so2_frac must be 0-1 here
    blood_factor = 1.0_dp - (0.0289_dp * Hb_g_dL) / &
         ((3.352_dp - 0.456_dp * sat_o2) * (8.142_dp - pH))
    
    c_blood = c_plasma * blood_factor  ! Whole-blood CO2 content in mL STPD / mL
    
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
  
  real(dp) function po2_from_o2content(c_o2, c_co2, p_co2, p_o2_init, pH, sat_o2_init) result(p_o2)

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
    fun_co2 = v_q * (p_cap_co2 - p_i_co2) - K_stpd * (bg_co2%c_ven_co2 - c_cap_co2)

  end function function_co2
    
!!!##############################################################################

  real(dp) function fdash_co2(v_q, c_cap_co2, p_cap_co2, p_cap_o2) result(fdash)
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
    fdash = v_q + K_stpd * dC_dP

  end function fdash_co2

!!!##############################################################################

  pure real(dp) function pH_funct_CO2(p_co2, c_co2) result(pH)
    ! using the simplest approximation. more complicated ones don't
    ! work well in this current framework because of the interdependence
    ! between content_CO2, pH, sat etc.

!!! Inputs
    real(dp),intent(in) :: p_co2, c_co2

    pH = 7.4_dp - 0.004_dp * (p_co2 - 40.0_dp)

  end function pH_funct_CO2

!!!##############################################################################

  pure real(dp) function get_unit_v_q(nunit, Vdot) result(v_q)
    ! calculate v_q for the unit

    use parameter_types, only: Q_params
!!! Inputs
    integer, intent(in) :: nunit
    real(dp),intent(in) :: Vdot
!!! Locals
    real(dp) :: unit_v, unit_q

    unit_v = Vdot * gasex%Vdot(nunit) / elem_field(ne_Vdot,1)
    unit_q = Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction) * &
         gasex%Qdot(nunit) / elem_field(ne_Qdot,1)
    if (abs(unit_q) < loose_tol) then
       v_q = 1.0e5_dp ! set to high enough, but not ridiculous, value
    else
       v_q = unit_v / unit_q
    endif

  end function get_unit_v_q

!!!##############################################################################

  subroutine openfile(funit, fname, suffix, append)
    ! open a file to write to. either as a new file or appending to old file

    use other_consts, only: max_filename_len
!!! Inputs
    integer :: funit
    character (len=*) :: fname
    character (len=*) :: suffix
    logical :: append
!!! Locals
    character (len=max_filename_len) :: writefile
    logical :: exists

    if(index(fname, trim(suffix))> 0) then !full filename is given
       writefile = fname
    else ! need to append the correct filename extension
       writefile = trim(fname)//'.'//trim(suffix)
    endif
    
    if(append)then
       ! check whether the file exists
       inquire(file=writefile, exist=exists)
       if (exists) then
          ! open existing file and append
          open(funit, file=writefile, status='old', action='write', position='append')
       else
          ! create a new file
          open(funit, file=writefile, status='replace', action='write')
       endif
    else
       open(funit, file=writefile, status='replace', action='write')
    endif

  end subroutine openfile

!!!##############################################################################

  subroutine solve_gasexchange(time_start, time_end, phase, fname)
    ! Assemble matrices for 1D gas mixing equation, solve, and then solve for gas exchange. 
    ! The subroutine assumes that the phase is different each time it is called, such
    ! that reduce_gasmix and the init_*_tracking subroutines need to be called again

    use indices
    use geometry, only: volume_of_mesh
    use parameter_types, only: constants, gx_params, Q_params, solve_gx_params
    use solve, only: pmgmres_ilu_cr_cached
!!! Inputs
    real(dp),intent(in) :: time_start, time_end
    logical :: is_expiration
    character(len=*) :: phase
    character(len=*) :: fname
!!! Locals
    integer :: ncol, nentry, np, nrow, nrow_BB
    integer, parameter :: file_time = 20
    real(dp),allocatable :: solution(:)
    real(dp) :: aa, bb, dt_theta, err_air, err_blood, ideal_air, ideal_blood, gx_time, mmol, o2_uptake, &
         sum_o2_total, sum_o2_uptake, time, factor, err_blood_side, err_gas_side, VO2_airside, VO2_bloodside, &
         mmol_current
    logical :: carryon, pattern_changed
    logical :: breathhold, expiration, inspiration
    logical :: is_gx = .true.

    if(.not.initialised_gasexchange)then
       call initialise_gasexchange()
       initialised_gasexchange = .true.
       call openfile(file_time, fname, 'time', append=.false.)
    else
       call openfile(file_time, fname, 'time', append=.true.)
    endif

    ! allocatable array to store the current solution 
    if(.not.allocated(solution)) allocate(solution(num_nodes))

   ! used for phase-specific options and BCs
    inspiration = .false.
    breathhold  = .false.
    expiration  = .false.

    ! need to include breathhold too
    sum_o2_total = track_gx_soln%current_o2_uptake
    select case(trim(phase))
    case('inspiration')
       inspiration = .true.
       if(trim(current_phase) == 'expiration') then
          track_gx_soln%time_in_breath = 0.0_dp
          track_gx_soln%init_air_mmol = mmol_in_air(nj_conc1, gasex%conc_o2)
          mmol_inhaled = 0.0_dp ! re-initialise when changing phase
          sum_o2_total = 0.0_dp
       endif
       current_phase = 'inspiration'
    case('breathhold')
       breathhold = .true.
    case('expiration')
       expiration = .true.
       if(trim(current_phase) == 'inspiration') mmol_exhaled = 0.0_dp ! re-initialise when changing phase
       current_phase = 'expiration'
    end select

    ! get the sparsity arrays for the reduced system. uses compressed row format.
    call reduce_gasmix(expiration)

    ! dt * the weighting for matrices in the reduced system: A = M+K*dt*theta; B = -K*c^(n)*dt
    dt_theta = solve_gx_params%dt * solve_gx_params%theta

    if(.not.expiration) node_field(nj_conc1,1) = c_i_o2
    if(inspiration)then
       call init_insp_tracking()
    elseif(expiration)then
       call init_expn_tracking()
    endif
    pattern_changed = .true.

    time = time_start ! initialise the time
!!! main time-stepping loop
    do
       if (time_end - time <= zero_tol) exit

       track_gx_soln%time_in_breath = track_gx_soln%time_in_breath + solve_gx_params%dt
       
       if (.not.breathhold) then
          ! note that airway_mesh_deform is not called for lumped units. acinar volume change takes place in
          ! general_track. call volume_of_mesh to update elem_field(ne_vol_bel,:)
          call general_track(.true.)
          call update_volumes_below()
       endif

       ! assemble the element matrices. Element matrix calculation can be done directly 
       ! (based on assumption of interpolation functions) or using Gaussian interpolation. 
       call assemble_gasmix()

       ! initialise the values in the solution matrices
       global_AA(:) = 0.0_dp ! equivalent to M 
       global_BB(:) = 0.0_dp ! equivalent to K 
      
       ! Assemble the reduced system of matrices
       ! all of the global matrix terms (A, B) are in units of mmol (checked)
       do np = 1, num_nodes ! Loop over rows of unreduced system
          nrow = np ! conveniently true for the way we set up our models
          ! different boundary conditions are applied during inspiration and 
          ! expiration: Dirichlet at model entry during inspiration (concentration
          ! = inlet_concentration), and Neumann at model entry during expiration (dcdt = 0)
          if (.not.inspiration) then
             BB = global_R(nrow)         !get reduced R.H.S.vector
             do nentry = sparsity_row(nrow),sparsity_row(nrow+1)-1  !each row entry
                ncol = sparsity_col(nentry)
                BB = BB-global_K(nentry) * node_field(nj_conc1,ncol) * solve_gx_params%dt ! -K*c^(n)*dt
                AA = global_M(nentry) + dt_theta * global_K(nentry) ! M+K*dt*theta
                global_AA(nentry) = global_AA(nentry) + AA
             enddo
             global_BB(nrow) =  global_BB(nrow) + BB
          elseif (inspiration.and.np.ne.1) then !not first row
             BB = global_R(nrow)         !get reduced R.H.S.vector
             do nentry = sparsity_row(nrow), sparsity_row(nrow+1)-1  !each row entry
                ncol = sparsity_col(nentry)
                BB = BB - global_K(nentry)*node_field(nj_conc1,ncol) * solve_gx_params%dt
                AA = global_M(nentry) + dt_theta * global_K(nentry) !M+K*dt*theta
                if(ncol.ne.1)then ! not first column
                   global_AA(nentry-noffset_entry) = &
                        global_AA(nentry-noffset_entry) + AA
                endif
             enddo
             global_BB(nrow-noffset_row) = &
                  global_BB(nrow-noffset_row) + BB
          endif
       enddo
       
       solution(:) = zero_tol

       ! Call a solver to solve the system of reduced equations. 
       ! Here we use an iterative solver (GMRES == Generalised Minimal 
       ! RESidual method). The solver requires the solution matrices to 
       ! be represented in compressed row format.
       call pmgmres_ilu_cr_cached ( matrixsize, nonzeros, reduced_row, reduced_col, &
            global_AA, solution, global_BB, pattern_changed )
       pattern_changed = .false.

       ! transfer the solver solution (in 'Solution') to the node field array          
       do np = 1, num_nodes
          if (inspiration .and. np == 1) cycle
          nrow_BB = np - merge(1, 0, inspiration)
          node_field(nj_conc1,np) = max(0.d0, min(1.d0, &
               node_field(nj_conc1,np) + Solution(nrow_BB)))  !c^(n+1)=c^(n)+dc
       enddo
       
       if(is_gx)then ! temporary condition while developing
          gx_time = 0.0_dp
          do 
             if(solve_gx_params%dt - gx_time <= zero_tol) exit
             track_gx_soln%time_in_transit = track_gx_soln%time_in_transit + solve_gx_params%dt_gx
             call o2_exchange_in_units(o2_uptake, time)
             sum_o2_total = sum_o2_total + o2_uptake ! total o2 volume moving into blood
             gx_time = gx_time + solve_gx_params%dt_gx
          enddo
       endif

       if(expiration) then ! update terminal node concentration each time-step
          call update_terminal_conc_from_unit
       endif
       
       call update_volumes_below()

       if(inspiration)then
          mmol_inhaled = mmol_inhaled + elem_field(ne_Vdot,1) * node_field(nj_conc1,1) * solve_gx_params%dt
       elseif(expiration)then
          mmol_exhaled = mmol_exhaled + elem_field(ne_Vdot,1) * node_field(nj_conc1,1) * solve_gx_params%dt
       endif

       ! air mass ==      initial                +   insp/exp   + exchanged   (sum_o2_uptake is negative for air-->blood)
       ideal_air   = track_gx_soln%init_air_mmol + mmol_inhaled + mmol_exhaled + sum_o2_total
       ideal_blood = track_gx_soln%init_blood_mmol - sum_o2_total
       
       err_air   = (ideal_air - mmol_in_air(nj_conc1, gasex%conc_o2)) / ideal_air * 100.0_dp
       err_blood = (ideal_blood - mmol_in_blood()) / ideal_blood * 100.0_dp

       factor = constants%o2molvol_37deg/1.0e3_dp

       write(file_time,'(F7.3, F9.3, 10(F9.2))') &
            time, elem_field(ne_vol_bel,1)/1.0e+6_dp, node_field(nj_conc1,1) * o2_cnv_c2pp, & 
            bg_o2%time_av_p_alv_o2, bg_o2%time_av_p_cap_o2, bg_o2%time_av_p_art_o2, & 
            bg_o2%p_alv_o2, bg_o2%p_cap_o2, bg_o2%p_art_o2, bg_o2%p_ven_o2, &
            bg_co2%p_art_co2, bg_co2%p_ven_co2
       time = time + solve_gx_params%dt ! increment time
       track_gx_soln%total_time = track_gx_soln%total_time + solve_gx_params%dt
    enddo 

    bg_o2%c_ven_o2 = bg_o2%c_art_o2 - gx_params%VO2 / Q_params%cardiac_output   ! units: (ml/ml)
    bg_o2%p_ven_o2 = po2_from_o2content(bg_o2%c_ven_o2, bg_co2%c_ven_co2, bg_co2%p_ven_co2, &
         bg_o2%p_ven_o2, bg_co2%ph_ven, bg_o2%sat_ven)

    close(file_time)

    if(expiration)then
       mmol_current = mmol_in_air(nj_conc1, gasex%conc_o2)
       factor = constants%o2molvol_stpd / 1.0e3_dp 
       VO2_airside = (track_gx_soln%init_air_mmol + mmol_inhaled + mmol_exhaled - &
            mmol_current) * factor / track_gx_soln%time_in_breath * 60.0_dp
       track_gx_soln%total_o2_uptake = track_gx_soln%total_o2_uptake + sum_o2_total
       VO2_bloodside = track_gx_soln%total_o2_uptake * factor / track_gx_soln%total_time * 60.0_dp
       bg_o2%vo2_blood = VO2_bloodside
       write (*,'(i6, f12.2, 8(f9.2))') track_gx_soln%breath_num, &
            VO2_airside, VO2_bloodside, track_gx_soln%VO2_fick, &
            bg_o2%time_av_p_alv_o2, bg_o2%time_av_p_cap_o2, &
            bg_o2%time_av_p_art_o2, bg_o2%p_ven_o2, bg_co2%p_art_co2, bg_co2%p_ven_co2
       track_gx_soln%breath_num = track_gx_soln%breath_num + 1
    endif

    track_gx_soln%current_o2_uptake = sum_o2_total

  end subroutine solve_gasexchange
  
!!!##############################################################################
  
  subroutine update_volumes_below()
    ! updates elem_field(ne_vol_bel,:) to the current volume

!!! Locals
    integer :: ne, ne0, nunit
    real(dp) :: fac

    elem_field(ne_vol_bel,:) = elem_field(ne_vol,:) !initialise to branch volume
    elem_field(ne_vd_bel,:)  = elem_field(ne_vol,:) !initialise to branch volume

    do nunit = 1, num_units
       ne = units(nunit)
       if(ne /= 0) elem_field(ne_vol_bel,ne) = elem_field(ne_vol_bel,ne) + gasex%volume(nunit) !add elastic unit volume
    enddo !nunit

    do ne = num_elems, 2, -1
       ne0 = elem_cnct(-1,1,ne)
       fac = dble(elem_symmetry(ne)) * dble(elem_ordrs(no_type,ne))
       elem_field(ne_vol_bel,ne0) = elem_field(ne_vol_bel,ne0) + fac * elem_field(ne_vol_bel,ne)
       elem_field(ne_vd_bel,ne0)  = elem_field(ne_vd_bel,ne0)  + fac * elem_field(ne_vd_bel,ne)
    enddo !noelem

  end subroutine update_volumes_below

!!!##############################################################################
  
  subroutine reduce_gasmix(expiration)
    
!!! Inputs
    logical,intent(in) :: expiration
!!! Locals
    integer :: i

    if(.not.expiration)then !remove first row and column (note: also for breath-hold)
       
       do i=1,num_nodes ! one more than # of rows
          reduced_row(i) = sparsity_row(i+1)-3
       enddo
       NonZeros = NonZeros_unreduced - 3
       do i=1,NonZeros
          reduced_col(i) = sparsity_col(i+3)-1
       enddo
       reduced_row(1)=1
       MatrixSize = num_nodes - 1
       noffset_entry = 3
       noffset_row = 1

    elseif(expiration)then
       
       do i=1,num_nodes+1
          reduced_row(i) = sparsity_row(i)
       enddo
       NonZeros = NonZeros_unreduced
       do i=1,NonZeros
          reduced_col(i) = sparsity_col(i)
       enddo
       reduced_row(1)=1
       MatrixSize = num_nodes
       noffset_entry = 0
       noffset_row = 0
       
    endif
    
  end subroutine reduce_gasmix
  
!!!################################################################################

  subroutine init_insp_tracking()

    use parameter_types, only: solve_gx_params
!!! Locals
    integer,parameter :: elem_f_to_node = 1, f_sign = 2

    call clear_inspiration_sources()
    call build_insp_sources_for_node()
    call build_insp_sources_for_unit()

    maps_built = .true.
    dt_built   = solve_gx_params%dt
    
  end subroutine init_insp_tracking

!!!##############################################################################
  
  subroutine clear_inspiration_sources()
    
    if (allocated(src1)) deallocate(src1, src2, w1, w2)
    if (allocated(uptr)) deallocate(uptr, pelem, pfrac, ppf, pxi)
    if (allocated(node_from_inlet)) deallocate(node_from_inlet, node_inlet_w)
    maps_built = .false.
    dt_built = -1.0_dp
    
  end subroutine clear_inspiration_sources

!!!##############################################################################
  
  subroutine build_insp_sources_for_node()
  ! Build footpoint map for every node (steady flow)

    use indices
    use parameter_types, only: solve_gx_params
!!! Locals
    integer :: np, ne, np1, np2, ne0, np1_parent, np2_parent
    real(dp) :: t_in_elem, total_time, local_xi
    logical :: cont

    allocate(node_from_inlet(num_nodes), node_inlet_w(num_nodes))
    node_from_inlet = .false.
    node_inlet_w = 0.0_dp

    allocate(src1(num_nodes), src2(num_nodes), w1(num_nodes), w2(num_nodes))
    src1 = 0; src2 = 0
    w1 = 0.0_dp; w2 = 0.0_dp
    
    do np = 2, num_nodes
       ne = elems_at_node(np,1)     ! simple inflow: one parent element
       np1 = elem_nodes(1,ne)
       np2 = elem_nodes(2,ne)
       t_in_elem = abs(elem_field(ne_vol,ne)/elem_field(ne_Vdot,ne))
       total_time = t_in_elem
       
       if (total_time >= solve_gx_params%dt) then
          local_xi = solve_gx_params%dt / t_in_elem
          src1(np) = np1
          src2(np) = np2
          w1(np) = local_xi           ! weight for np1
          w2(np) = 1.0_dp - local_xi  ! weight for np2
       else
          ! Walk upstream until dt is reached.
          cont = .true.
          ne0 = ne
          
          do while (cont)
             ne0 = elem_cnct(-1,1,ne0)  ! parent element
             if (ne0 == 0) then ! reached tree entry before dt
                node_from_inlet(np) = .true.
                node_inlet_w(np) = 1.0_dp
                src1(np) = 1
                src2(np) = 1
                w1(np) = 0.0_dp
                w2(np) = 0.0_dp
                cont = .false.
                cycle
             endif
             
             np1_parent = elem_nodes(1,ne0)
             np2_parent = elem_nodes(2,ne0)
             
             t_in_elem = abs(elem_field(ne_vol,ne0)/elem_field(ne_Vdot,ne0))
             total_time = total_time + t_in_elem
             
             if (total_time >= solve_gx_params%dt) then
                local_xi = (total_time - solve_gx_params%dt) / t_in_elem
                src1(np) = np1_parent
                src2(np) = np2_parent
                w1(np) = 1.0_dp - local_xi
                w2(np) = local_xi
                cont = .false.
             endif
          enddo
       endif
    enddo
    
  end subroutine build_insp_sources_for_node

!!!################################################################################

  subroutine build_insp_sources_for_unit()
    ! Build per-unit pathway segment list (for steady flow)

    use indices
    use parameter_types, only: solve_gx_params
!!! Locals
    integer :: nunit, ne, ne0, ne0_child
    integer :: np2_parent
    real(dp) :: total_time, t_in_elem, local_xi
    real(dp) :: mass_fraction, pf
    integer :: count, k

    ! first pass: count total number of segments that will need to be tracked
    ! sum segments in pathway supplying each unit
    allocate(uptr(num_units+1))
    uptr = 0
    count = 0

    do nunit = 1, num_units
       ne = units(nunit)     ! element to which unit is attached
       ne0 = ne
       ne0_child = ne
       pf = 1.0_dp
       total_time = 0.0_dp
       
       do
          t_in_elem = abs(elem_field(ne_vol,ne0)/elem_field(ne_Vdot,ne0))
          if (total_time + t_in_elem >= solve_gx_params%dt) then
             count = count + 1 ! gas will come from terminal element
             exit
          else
             count = count + 1 ! count the number of elements in path to unit
             total_time = total_time + t_in_elem
             ne0 = elem_cnct(-1,1,ne0)
             if (ne0 <= 0) stop "build_insp_unit_path_map: dt implies unit supply outside model (not allowed)."
          endif
       enddo
    enddo
    
    allocate(pelem(count), pfrac(count), ppf(count), pxi(count))
    
    ! second pass: fill the arrays
    k = 1
    uptr(1) = 1
    
    do nunit = 1, num_units
       ne = units(nunit)
       ne0 = ne
       ne0_child = ne
       pf = 1.0_dp
       total_time = 0.0_dp
       
       do
          t_in_elem = abs(elem_field(ne_vol,ne0)/elem_field(ne_Vdot,ne0))
           if (total_time + t_in_elem >= solve_gx_params%dt) then
             local_xi = (total_time + t_in_elem - solve_gx_params%dt) / t_in_elem   
             pelem(k) = ne0
             pfrac(k) = 1.0_dp - local_xi                 ! distal fraction within dt
             ppf(k) = pf
             pxi(k) = local_xi                          ! >=0 marks partial
             k = k + 1
             exit
          else
             pelem(k) = ne0
             pfrac(k) = 1.0_dp
             ppf(k) = pf
             pxi(k) = -1.0_dp                           ! full element marker
             k = k + 1
             total_time = total_time + t_in_elem
             ne0 = elem_cnct(-1,1,ne0) ! parent element
             if (ne0 <= 0) stop "build_insp_unit_path_map: dt implies supply outside model (not allowed)."
             mass_fraction = elem_field(ne_Vdot,ne0_child) / elem_field(ne_Vdot,ne0)
             pf = pf * mass_fraction
             ne0_child = ne0
          endif
       enddo
       uptr(nunit+1) = k
    enddo
   
  end subroutine build_insp_sources_for_unit

!!!################################################################################

  subroutine init_expn_tracking()
    ! Build sources for all nodes. Each node np gets a list of (element, xi, weight) sources.
    
    use parameter_types, only: solve_gx_params
!!! Locals
    integer :: cap, maxstack, np, nsrc, nunit, ne
    integer, allocatable :: st_node(:)
    real(dp), allocatable :: st_time(:), st_wt(:)
    
    call clear_expiration_sources()
    
    if (.not. allocated(unit_at_node)) allocate(unit_at_node(num_nodes))
    unit_at_node = 0
    
    do nunit = 1, num_units
       ne = units(nunit)
       np = elem_nodes(2,ne)  
       unit_at_node(np) = nunit
    enddo
    
    ! safe default for sizing 'stack' arrays (st_)
    maxstack = 8 * num_nodes
    if (maxstack < 2048) maxstack = 2048
    allocate(st_node(maxstack), st_time(maxstack), st_wt(maxstack))
    allocate(sptr(num_nodes+1))
    sptr = 0
    
    ! cap is the current size of arrays
    cap = max(1000, 8*num_nodes)
    if(allocated(src_unit)) deallocate(src_unit)
    if(.not.allocated(src_elem)) &
         allocate(src_elem(cap), src_xi(cap), src_w(cap), src_is_unit(cap), src_unit(cap))
    nsrc = 0
    
    sptr(1) = 1
    do np = 1, num_nodes
       call build_expn_sources_for_node(np, maxstack, cap, nsrc, st_node, st_time, st_wt)
       sptr(np+1) = nsrc + 1
    enddo
    
    built = .true. ! flag that tracking is built
    dt_built = solve_gx_params%dt  ! store the dt size that tracking is built for
    
    deallocate(st_node, st_time, st_wt)
    
  end subroutine init_expn_tracking

!!!##############################################################################
  
  subroutine clear_expiration_sources()
    
    if (allocated(sptr)) deallocate(sptr)
    if (allocated(src_elem)) deallocate(src_elem, src_xi, src_w, src_is_unit)
    if (allocated(unit_at_node)) deallocate(unit_at_node)
    built = .false.
    dt_built = -1.0_dp
    
  end subroutine clear_expiration_sources

!!!##############################################################################
  
  subroutine build_expn_sources_for_node(np0, maxstack, cap, nsrc, st_node, st_time, st_wt)
    ! Build sources for a single node (np0) into the global arrays

    use indices
    use parameter_types, only: solve_gx_params
!!! Inputs
    integer, intent(in)    :: maxstack, np0
    integer, intent(inout) :: cap, nsrc
    integer, intent(inout) :: st_node(:)
    real(dp), intent(inout):: st_time(:), st_wt(:)
!!! Locals 
    integer :: ii, last_supp, nc, ne, np, np2, nsupp, nunit, top
    real(dp) :: child_weight, frac, flow_used, flowmag, rem_t, sumflow, &
         t_in_elem, weight, weight_used, xi
    
    ! Seed task: start at node np0 and backtrack dt with total weight 1
    top = 1
    st_node(1) = np0
    st_time(1) = solve_gx_params%dt
    st_wt(1) = 1.0_dp
    
    do while (top > 0)
       
       np = st_node(top)
       rem_t = st_time(top)
       weight = st_wt(top)
       top = top - 1
       
       ! If no time to backtrack, use unit BC if present
       if (rem_t <= 0.0_dp) then
          nunit = unit_at_node(np)
          if (nunit > 0) call append_expn_source_unit(nunit, weight, nsrc, cap)
          cycle
       endif
       
       nc = elems_at_node(np, 0)
       
       ! Sum |flow| over supplying children for expiration:
       nsupp = 0
       sumflow = 0.0_dp
       do ii = 1, nc
          ne = elems_at_node(np, ii)
          if (elem_nodes(1, ne) /= np) cycle
          flowmag = abs(elem_field(ne_Vdot, ne))     ! magnitude; ne_Vdot is negative
          if (flowmag <= zero_tol) cycle
          nsupp = nsupp + 1
          sumflow = sumflow  + flowmag
       enddo
       
       if (nsupp == 0 .or. sumflow <= zero_tol) then
          ! Terminal node in expiration sense: must use unit boundary condition
          nunit = unit_at_node(np)
          if (nunit > 0) then
             call append_expn_source_unit(nunit, weight, nsrc, cap)
          else
             write(*,*) "ERROR: terminal node without unit BC in expiration mapping. np=", np
             stop
          endif
          cycle
       endif
       
       ! Split weight among children by flow fractions, with remainder to ensure exact sum
       last_supp = nsupp
       flow_used = 0.0_dp
       weight_used = 0.0_dp
       
       do ii = 1, nc
          ne = elems_at_node(np, ii)
          if (elem_nodes(1, ne) /= np) cycle
          
          flowmag = abs(elem_field(ne_Vdot, ne))
          if (flowmag <= zero_tol) cycle
          
          last_supp = last_supp - 1
          
          if (last_supp == 0) then
             child_weight = weight - weight_used                  ! remainder => sum exactly weight
          else
             frac = flowmag / (sumflow - flow_used)        ! stable fraction on remaining sum
             child_weight = (weight - weight_used) * frac
             flow_used = flow_used  + flowmag
             weight_used = weight_used + child_weight
          endif
          
          np2 = elem_nodes(2, ne)
          
          ! Transit time through element: vol / |flow|
          t_in_elem = elem_field(ne_vol, ne) / flowmag        ! ne_vol is true volume; flowmag > 0
          
          if (rem_t < t_in_elem) then
             ! Footpoint inside this element at xi = rem_t / t_in_elem (0 at node1, 1 at node2)
             xi = rem_t / t_in_elem
             call append_expn_source_elem(ne, xi, child_weight, nsrc, cap)
          else
             ! Backtrack passes fully through this element: continue from downstream node np2
             top = top + 1
             if (top > maxstack) then
                write(*,*) "ERROR: MAXSTACK exceeded in expiration mapping. dt=", solve_gx_params%dt, " np=", np
                stop
             endif
             st_node(top) = np2
             st_time(top) = rem_t - t_in_elem
             st_wt(top) = child_weight
          endif
       enddo
    enddo
    
  end subroutine build_expn_sources_for_node

!!!##############################################################################
  
  subroutine append_expn_source_elem(ne, xi, weight, nsrc, cap)
    ! concentration mapping at a node comes from xi in element ne
    ! 'src_'(nsrc) store info efficiently
    
!!! Inputs
    integer, intent(in) :: ne
    real(dp), intent(in) :: xi, weight
    integer, intent(inout) :: nsrc, cap

    call ensure_expn_capacity(1, nsrc, cap)
    nsrc = nsrc + 1
    src_elem(nsrc) = ne
    src_xi(nsrc) = xi
    src_w(nsrc) = weight
    src_is_unit(nsrc) = .false.
    src_unit(nsrc) = 0
    
  end subroutine append_expn_source_elem

!!!##############################################################################
  
  subroutine append_expn_source_unit(nunit, weight, nsrc, cap)
    ! concentration mapping at a node comes from a unit
    
!!! Inputs
    integer, intent(in) :: nunit
    real(dp), intent(in) :: weight
    integer, intent(inout) :: nsrc, cap
!!! Locals
    integer :: ne

    ne = units(nunit)
    call ensure_expn_capacity(1, nsrc, cap)
    nsrc = nsrc + 1
    src_elem(nsrc) = ne
    src_xi(nsrc) = 1.0_dp  ! unit at xi = 1
    src_w(nsrc) = weight
    src_is_unit(nsrc) = .true.
    src_unit(nsrc) = nunit

  end subroutine append_expn_source_unit

!!!##############################################################################
  
  subroutine ensure_expn_capacity(needed, nsrc, cap)
    ! increase size of cap and append extra memory to key arrays if (nsrc + needed) > cap
    
!!! Inputs
    integer, intent(in) :: needed, nsrc
    integer, intent(inout) :: cap
!!! Locals
    integer :: newcap
    integer, allocatable :: tmp_i(:), tmp_u(:)
    real(dp), allocatable :: tmp_x(:), tmp_w(:)
    logical, allocatable :: tmp_b(:)

    if (nsrc + needed <= cap) return

    newcap = max(cap*2, nsrc + needed + 1000)

    allocate(tmp_i(newcap), tmp_u(newcap), tmp_x(newcap), tmp_w(newcap), tmp_b(newcap))
    tmp_i(1:nsrc) = src_elem(1:nsrc)
    tmp_u(1:nsrc) = src_unit(1:nsrc)
    tmp_x(1:nsrc) = src_xi(1:nsrc)
    tmp_w(1:nsrc) = src_w(1:nsrc)
    tmp_b(1:nsrc) = src_is_unit(1:nsrc)

    call move_alloc(tmp_i, src_elem)
    call move_alloc(tmp_u, src_unit)
    call move_alloc(tmp_x, src_xi)
    call move_alloc(tmp_w, src_w)
    call move_alloc(tmp_b, src_is_unit)

    cap = newcap
    
  end subroutine ensure_expn_capacity

!!!##############################################################################
  
  subroutine general_track(update)  
    ! the argument 'update' is a tag to decide whether the concentrations have to updated or not
    ! this should be false in case of mass calculation which is used in the "update_unit_mass" subroutine

    use indices
    use mesh_utilities,only: group_elem_by_parent
!!! Inputs
    logical, intent(in) :: update
!!! Locals
    integer,parameter :: n_gases = 3
    integer :: i,ne,ne_stem,nj_g(3),np,np2, &
         num_list_total,nunit,sum_inout,sum_dir
    integer,parameter :: elem_f_to_node = 1, f_sign = 2
    integer,allocatable :: TMAT(:,:,:),elem_list_total(:)
    real(dp) :: inlet_conc,ratio_mass,total_mass
!!! currently unused but might be again
!!!  ideal_mass_total,mass_at_max,mass_below_max, total_mass,ratio_mass,
    real(dp), allocatable :: concent(:)
    logical :: go_on

    nj_g(1) = nj_conc1
    nj_g(2) = nj_conc2
    inlet_conc = c_i_o2

    if (.not.allocated(TMAT)) allocate(TMAT(num_nodes,2,3))
    
    !second index: entering/exiting (1/2) flow towards/from a node 
    ! and positive/negative (1/2) flow at each element (positive: downward,
    ! negative:upward), Third index: the elements at the node
    
!!! If the flow is negative in an element, the negative flow always come from its children
!!! however, if the flow is positive the flow might be coming from its parents or its parents' other children
    !call track_mat_coupled(elem_f_to_node,f_sign,TMAT) !calling the matrix containing flow directions and nodes in/out data
    go_on = .true.
    do np = 1,num_nodes
!!!    check that the flows at the node make sense. Can't have flow in 
!!!    all elements directed towards the node, and can't have all flows directed out from the node.
!!! TMAT(np,2,i) = 1 when flow in ith element attached to np is distal (to small airways)
!!! TMAT(np,2,i) = 2 when flow in ith element attached to np is proximal (to large airways)
!!! TMAT(np,1,i) = 1 when ith element attached to np brings flow to node np
!!! TMAT(np,1,i) = 2 when ith element attached to np takes flow from node np
       sum_inout = sum(TMAT(np,elem_f_to_node,1:elems_at_node(np,0))) !sum of flow directions: proximal (2) and distal (1) 
       sum_dir = sum(TMAT(np,f_sign,1:elems_at_node(np,0)))     !sum of flow directions w.r.t. node np
       if ((sum_inout == 6) .and. (sum_dir == 4)) go_on = .false. !impossible
       if ((sum_inout == 3) .and. (sum_dir == 5)) go_on = .false. !trapping
       if ((sum_inout == 4) .and. (sum_dir == 3)) go_on = .false. !impossible
       if ((sum_inout == 2) .and. (sum_dir == 3)) go_on = .false. !trapping

       go_on = .true.
       
    enddo

    if(go_on)then
       
       allocate(concent(num_nodes))
       concent = 0.0_dp
       
       if(all(elem_field(ne_Vdot,:).ge.0.0_dp))then ! all flows directed towards distal branches
          call tracking_step_insp(nj_conc1, concent, inlet_conc)
       else  
          call tracking_step_expn(concent)
       endif
       
!!! update the stored concentrations at all nodes
       if (update) then
          !node_field(nj_g(:),1:num_nodes) =  concent(:,1:num_nodes)
          node_field(nj_g(1),1:num_nodes) =  concent(1:num_nodes)
          
!!! adjust the concentrations to conserve mass. Only for nodes where the concentration
!!! is less than the maximum (set in 'initial_gasmix')
          total_mass = mmol_in_air(nj_conc1, gasex%conc_o2)

          allocate(elem_list_total(num_elems))
          do nunit = 1,num_units
             ne_stem = units(nunit)
             num_list_total = 0
             if(elem_cnct(1,0,ne_stem).ne.0)then
                elem_list_total = 0
!!! get a list of all elements that are within an elastic unit
                call group_elem_by_parent(ne_stem,elem_list_total)
                num_list_total = count(elem_list_total.ne.0)
             endif
!!! the 'ideal' mass for the unit is elem_field(ne_resist,ne_stem)
!!! the 'current' mass for the unit is elem_field(ne_mass,ne_stem)
             if(elem_field(ne_mass,ne_stem).gt.zero_tol)then
                ratio_mass = elem_field(ne_resist,ne_stem)/elem_field(ne_mass,ne_stem)
             else
                ratio_mass = 1.0_dp
             endif
             do i = 1,num_list_total
                ne = elem_list_total(i)
                np2 = elem_nodes(2,ne)
                !if(node_field(nj_conc1,np2).lt.max_concentration)then
                   node_field(nj_conc1,np2) = node_field(nj_conc1,np2)*ratio_mass
                !endif
             enddo
          enddo
          deallocate(elem_list_total)
       endif
       
       deallocate(concent)
    else
       write(*,'('' Inconsistent flows, e.g. accumulation at a node'')')
       read(*,*)
    endif

  end subroutine general_track
  
!!!##############################################################################
  
  subroutine tracking_step_insp(nj_g, concent, inlet_conc)
    ! Fast update of nodal concentrations using a pre-computed inspiration transport map.
    ! Map entries store (element, xi, weight) and optionally a unit BC.

    use indices
    use parameter_types, only: solve_gx_params
!!! Inputs
    integer, intent(in) :: nj_g
    real(dp), intent(in) :: inlet_conc
    real(dp), intent(inout) :: concent(:)
!!! Locals
    integer :: np, i, nunit, k, ne, np1, np2
    real(dp) :: xi, volseg, pf
    real(dp) :: unit_mass
    real(dp) :: mean_c, c_fp
    
    ! Inlet node
    concent(1) = inlet_conc
    
    ! Fast nodal concentration update
    do np = 2, num_nodes
       if (node_from_inlet(np)) then
          concent(np) = node_inlet_w(np) * inlet_conc
          if (concent(np) <= zero_tol) concent(np) = 0.0_dp
       else
          concent(np) = w1(np)*node_field(nj_g, src1(np)) + &
               w2(np)*node_field(nj_g, src2(np))
          if (concent(np) <= zero_tol) concent(np) = 0.0_dp
       endif
    enddo
    
    do nunit = 1, num_units
       unit_mass = gasex%volume(nunit) * gasex%conc_o2(nunit)
       
       ! add inflow mass from each contributing segment on the path
       do k = uptr(nunit), uptr(nunit+1)-1
          ne = pelem(k)
          pf = ppf(k)
          np1 = elem_nodes(1,ne)
          np2 = elem_nodes(2,ne)
          if (pxi(k) >= 0.0_dp) then
             ! partial element: footpoint inside element at xi
             xi = pxi(k)
             c_fp = (1.0_dp-xi)*node_field(nj_g,np1) + xi*node_field(nj_g,np2)
             mean_c = 0.5_dp * (c_fp + node_field(nj_g,np2))
          else
             ! full element
             mean_c = 0.5_dp * (node_field(nj_g,np1) + node_field(nj_g,np2))
          endif
          volseg = abs(elem_field(ne_vol,ne)) * pfrac(k)
          unit_mass = unit_mass + pf * volseg * mean_c
       enddo
       ! update unit volume by inflow over dt from the attached element
       ne = units(nunit)
       gasex%volume(nunit) = gasex%volume(nunit) + abs(elem_field(ne_Vdot,ne)) * solve_gx_params%dt
       gasex%conc_o2(nunit) = unit_mass / gasex%volume(nunit)
       gasex%p_alv_o2(nunit) = gasex%conc_o2(nunit) * o2_cnv_c2pp
       
    enddo
    
  end subroutine tracking_step_insp
  
!!!##############################################################################
  
  subroutine tracking_step_expn(concent)
    ! Fast update of nodal concentrations using a pre-computed expiration transport map.
    ! Map entries store (element, xi, weight) and optionally a unit BC.

    use indices
    use parameter_types, only: solve_gx_params
!!! Inputs
    real(dp), intent(inout):: concent(:)
!!! Locals
    integer :: k, k0, k1, ne, np, np1, np2, nunit
    real(dp), allocatable :: conc_old(:)
    real(dp) :: c_source, sum_weight, weight, xi
    
    if (.not. built) stop "tracking_step_expn: source map not built"
    
    allocate(conc_old(num_nodes))
    
    ! Previous nodal concentration field for interpolation
    conc_old = node_field(nj_conc1, :)
    
    do np = 1, num_nodes
       concent(np) = 0.0_dp
       sum_weight = 0.0_dp
       k0 = sptr(np)
       k1 = sptr(np+1) - 1
       do k = k0, k1
          weight = src_w(k)
          if (abs(weight) <= zero_tol) cycle
          sum_weight = sum_weight + weight
          if (src_is_unit(k)) then
             nunit    = src_unit(k)
             c_source = gasex%conc_o2(nunit)
          else
             ne = src_elem(k)
             xi = src_xi(k)
             np1 = elem_nodes(1, ne)
             np2 = elem_nodes(2, ne)
             c_source = (1.0_dp - xi) * conc_old(np1) + xi * conc_old(np2)
          endif
          concent(np) = concent(np) + weight * c_source
       enddo
       if (concent(np) <= 1.0e-10_dp) concent(np) = 0.0_dp
    enddo
    
    deallocate(conc_old)
    
    do nunit = 1, num_units
       ne = units(nunit)
       np = elem_nodes(2,ne)
       elem_field(nj_conc1,np) = gasex%conc_o2(nunit)
       gasex%volume(nunit) = gasex%volume(nunit) + elem_field(ne_Vdot, ne) * solve_gx_params%dt
    enddo
    
  end subroutine tracking_step_expn

!!!##############################################################################
  
  subroutine assemble_gasmix()

!!! Locals
    integer :: i,j,ncol,ne,nentry,nrow
    real(dp) :: elem_K(2,2),elem_M(2,2),elem_R(2)
    logical :: found
    
!!!................................................................

    global_K(1:nonzeros_unreduced) = 0.0_dp
    global_M(1:nonzeros_unreduced) = 0.0_dp
    global_R(1:num_nodes) = 0.0_dp
    
    do ne=1,num_elems
       call element_gasmix(ne,elem_K,elem_M,elem_R)
       do i=1,2
          nrow = elem_nodes(i,ne)
          do j=1,2
             ncol = elem_nodes(j,ne)
             found=.false.
             nentry = sparsity_row(nrow) ! start check at start of row
             do while (.not.found)
                if(ncol.eq.sparsity_col(nentry))then
                   found = .true.
                else
                   nentry = nentry+1
                endif
             enddo
             global_K(nentry) = global_K(nentry) + elem_K(i,j)
             global_M(nentry) = global_M(nentry) + elem_M(i,j)
          enddo !j
          global_R(nrow) = global_R(nrow) + elem_R(i)
       enddo !i
    enddo !noelem
    
  end subroutine assemble_gasmix
  
!!!##############################################################################
  
  subroutine element_gasmix(ne, elem_K, elem_M, elem_R)

    use indices
    use other_consts, only: pi
    use parameter_types, only: gx_params
!!! Inputs
    integer,intent(in) :: ne
    real(dp) :: elem_K(2,2), elem_M(2,2), elem_R(2)
!!! Locals
    real(dp) :: a_A_ratio, f1, f2, inner_area, length, outer_area, radius

    radius = elem_field(ne_radius,ne)
    length = elem_field(ne_length,ne)
    a_A_ratio = elem_field(ne_a_A,ne)
    outer_area = PI * radius**2
    inner_area = outer_area * a_A_ratio

    f1 = outer_area * length/3.0_dp
    f2 = inner_area * gx_params%diffusion_coeff / length
    
    elem_M(1,1) = f1 * dble(elem_symmetry(ne))
    elem_M(1,2) = f1 / 2.0_dp * dble(elem_symmetry(ne))
    elem_M(2,1) = f1 / 2.0_dp
    elem_M(2,2) = f1

    elem_K(1,1) = f2 * dble(elem_symmetry(ne))
    elem_K(1,2) = -f2 * dble(elem_symmetry(ne))
    elem_K(2,1) = -f2
    elem_K(2,2) = f2

    elem_R(1:2) = 0.0_dp

  end subroutine element_gasmix

!!!##############################################################################
  
  subroutine o2_exchange_in_units(o2_uptake, time)

    use indices
    use parameter_types, only: constants, gx_params, solve_gx_params, Q_params
!!! Inputs
    real(dp),intent(in) :: time
    real(dp),intent(inout) :: o2_uptake
!!! Locals
    integer :: nunit, idx(1)
    real(dp) :: sat_c, sum_uptake
    real(dp) :: fw_c_cap_o2, c_ven_o2, time_av_c_cap_o2
    real(dp) :: o2_diff_cond, p_alv_o2, p_cap_o2, pH_c
    real(dp) :: S_area, sum_alv_vol, flow_scale
    real(dp) :: dvo2, rate, mmol_start
    real(dp) :: V_cap, vol, tnow, tt, absQ, ueff
    real(dp), parameter :: alpha_ema = 0.25_dp ! weighting for exponential moving average
    
    ! Initialise
    p_alv_o2   = 0.0_dp  ! to get average p_alv_o2
    fw_c_cap_o2   = 0.0_dp  ! for flow-weighted c_cap_o2
    o2_uptake  = 0.0_dp  ! summation of O2 uptake from air
    sum_alv_vol = 0.0_dp ! summation of all gas exchange unit volumes

    ! Update alveolar partial pressures from concentrations once per call
    gasex%p_alv_o2(:)  = gasex%conc_o2(:)  * o2_cnv_c2pp
    gasex%p_alv_co2(:) = gasex%conc_co2(:) * o2_cnv_c2pp

    if (track_gx_soln%time_in_transit >= track_gx_soln%total_transit_time) then
       ! update time averaged capillary content from time averaged partial pressure
       time_av_c_cap_o2 = o2_content_from_po2(bg_co2%p_art_co2, bg_o2%time_av_p_cap_o2, sat_c)
       ! add shunt contribution to content
       bg_o2%c_art_o2 = time_av_c_cap_o2 + Q_params%shunt_fraction * &
               (bg_o2%c_ven_o2 - time_av_c_cap_o2)
       ! update arterial partial pressure from updated content (including shunt)
       bg_o2%p_art_o2 = po2_from_o2content(bg_o2%c_art_o2, bg_co2%c_art_co2, bg_co2%p_art_co2, &
            bg_o2%p_art_o2, bg_co2%pH_art, sat_c)
       ! exponential moving average for time-averaged partial pressure
       bg_o2%time_av_p_art_o2 = (1.0_dp - alpha_ema) * bg_o2%time_av_p_art_o2 + &
            alpha_ema * bg_o2%p_art_o2
       ! re-set the time in transit to zero
       track_gx_soln%time_in_transit = 0.0_dp
       track_gx_soln%VO2_fick = (bg_o2%c_art_o2 - bg_o2%c_ven_o2) * &
            Q_params%cardiac_output / 1.0e3_dp * 60.0_dp
    endif
    
    mmol_start = mmol_in_blood()
    flow_scale = (Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction)) / &
            elem_field(ne_Qdot,1)

    do nunit = 1, num_units
       ueff = units_effective(nunit)
       V_cap = gasex%V_cap(nunit)
       S_area = gasex%S_area(nunit)
       vol = gasex%volume(nunit)
       sat_c = gasex%sat_cap(nunit)
       absQ = abs(flow_scale * gasex%Qdot(nunit))

       tnow = gasex%t_in_transit(nunit) + solve_gx_params%dt_gx
       gasex%t_in_transit(nunit) = tnow
       
       ! Heartbeat reset if transit time exceeded
       if (tnow > gasex%t_time(nunit)) then
          gasex%t_in_transit(nunit) = 0.0_dp
          tnow = 0.0_dp
          gasex%p_cap_o2(nunit) = bg_o2%p_ven_o2
          gasex%c_cap_o2(nunit) = bg_o2%c_ven_o2
          pH_c = pH_funct_CO2(gasex%p_cap_co2(nunit), gasex%c_cap_co2(nunit)) 
          gasex%sat_cap(nunit) = saturation_of_o2(gasex%c_cap_co2(nunit), gasex%p_cap_co2(nunit), &
               gasex%p_cap_o2(nunit), pH_c)
       endif

       ! Diffusion capacity for this unit
       o2_diff_cond = calc_O2_diffusion_capacity(gasex%p_cap_o2(nunit), S_area, sat_c, V_cap)

       ! O2 solve - gives dvo2 (mm^3) and new gasex%p_cap_o2,c_cap_o2, sat_cap
       dvo2 = gas_exchange_unit_o2_step( nunit, o2_diff_cond, tnow )
          
       ! O2 uptake (mmol)
       o2_uptake = o2_uptake + dvo2 * ueff ! mmmol of O2

       ! Removal from alveolar airspace. use _stpd because this gives mmol coming from blood side
       gasex%conc_o2(nunit) = (gasex%conc_o2(nunit) * vol + dvo2) / vol
       gasex%p_alv_o2(nunit) =  gasex%conc_o2(nunit) * o2_cnv_c2pp
       
       ! sum end-capillary O2 content (flow-weighted)
       if (absQ > 1.0e-9_dp) then
          fw_c_cap_o2 = fw_c_cap_o2 + gasex%c_cap_o2(nunit) * ueff * absQ
       endif
       
       ! sum alveolar partial pressures (volume-weighted)
       p_alv_o2  = p_alv_o2  + gasex%p_alv_o2(nunit) * vol * ueff
       sum_alv_vol = sum_alv_vol + vol * ueff

    enddo

    ! Global averages
    if (sum_alv_vol > zero_tol) then
       bg_o2%p_alv_o2  = p_alv_o2  / sum_alv_vol
    else
       bg_o2%p_alv_o2  = 0.0_dp
    endif

    fw_c_cap_o2 = fw_c_cap_o2 / (Q_params%cardiac_output * (1.0_dp - Q_params%shunt_fraction))
    
    ! use arterial values for CO2 and previous p_cap_o2 to get pH and saturation in current capillary blood
    bg_o2%c_cap_o2 = fw_c_cap_o2
    sat_c = saturation_of_o2(bg_co2%c_art_co2, bg_co2%p_art_co2, bg_o2%p_cap_o2, bg_co2%pH_art)
    bg_o2%p_cap_o2 = po2_from_o2content(fw_c_cap_o2, bg_co2%c_art_co2, bg_co2%p_art_co2, &
         bg_o2%p_art_o2, bg_co2%pH_art, sat_c)

    ! exponential moving time-averages
    bg_o2%time_av_p_alv_o2 = (1.0_dp - alpha_ema) * bg_o2%time_av_p_alv_o2 + alpha_ema * bg_o2%p_alv_o2
    bg_o2%time_av_p_cap_o2 = (1.0_dp - alpha_ema) * bg_o2%time_av_p_cap_o2 + alpha_ema * bg_o2%p_cap_o2

  end subroutine o2_exchange_in_units

!!!##############################################################################

  real(dp) function gas_exchange_unit_o2_step(nunit, o2_diff_cond, time_now) result (dvo2)
    ! solve for one gas exchange step. note that p_ven_o2 is the capillary pO2 before a reset

    use parameter_types, only: constants, gx_params, solve_gx_params
    use solve, only: runge_kutta_4th
!!! Inputs
    integer,  intent(in)    :: nunit
    real(dp), intent(in) :: o2_diff_cond, time_now
!!! Locals    
    real(dp) :: c_before, c_after, rpar(0:5), tbeg, tend, Y
    
    ! Build rpar once for this solve
    rpar(1) = gx_params%press_atm  ! redundant, not actually used in solver
    rpar(2) = gasex%V_cap(nunit)
    rpar(3) = o2_diff_cond
    rpar(4) = gasex%p_alv_o2(nunit)
    rpar(5) = Hb_conc * 1.0e3_dp ! Hb_conc is in units of mmol/mm^3. solver units mol/L
    
    tbeg = time_now
    tend = time_now + solve_gx_params%dt_gx
    
    ! Content before
    c_before = gasex%c_cap_o2(nunit)
    Y = gasex%p_cap_o2(nunit) ! the current capillary pO2

    if ( Y > gasex%p_alv_o2(nunit) )then
       dvo2 = 0.0_dp
       return
    endif
    
    call runge_kutta_4th(1, tbeg, tend, Y, rpar)
    
    gasex%p_cap_o2(nunit) = Y ! the new capillary pO2
    
    ! O2 content after
    gasex%sat_cap(nunit) = saturation_of_o2(gasex%c_cap_co2(nunit), gasex%p_cap_co2(nunit), &
         gasex%p_cap_o2(nunit), gasex%ph_cap(nunit))
    c_after = o2_content_from_po2(gasex%p_cap_co2(nunit), gasex%p_cap_o2(nunit), gasex%sat_cap(nunit))
    gasex%c_cap_o2(nunit) = c_after

    ! Convert change in content to volume in blood at STPD
    dvo2 = gasex%V_cap(nunit) * (c_before - c_after) / constants%o2molvol_stpd  ! mmol of O2 taken up by blood
     
  end function gas_exchange_unit_o2_step

!!!##############################################################################

  real(dp) function calc_O2_diffusion_capacity(p_cap_o2, s_area, sat_o2, V_cap) result (DO2)
    ! Calculates the oxygen diffusing capacity (diffusive conductance). 
    ! Calculation of DO2 from Weibel 1997 (mm3/s/mmHg)
    ! model equations from Annalisa Swan PhD thesis
    
    use parameter_types, only: constants, species_params
    ! constants%kappa_o2 = 3.85_dp                     ! mol(O2)/mol(blood); O2 carrying capacity of haemoglobin [pg.26]
    ! constants%kc_O2 = 4.4e8_dp                       ! mm^3/mmol/s; forward reaction velocity for O2 with Hb (Weibel 1997)
    ! constants%sigma_o2 = 1.4e-9_dp                   ! mmol/mm^3/mmHg; solubility of O2 in blood (Hill et al., 1973a)
    ! constants%K = 5.5e-8_dp                          ! mm^2/s/mmHg. Krogh's permeation coefficient for O2. (==3.3e-8 cm2/min/mmHg); Weibel 1993
    ! species%tau_h      = air-blood barrier thickness, mm
    ! constants%o2molvol_37deg = 27.128e+3_dp          ! mm^3/mmol, O2 molecular volume @BTPD = R.T/P_dry
    ! Hb_conc             ! concentration of Hb, mmol/mm^3
!!! Inputs
    real(dp),intent(in) :: p_cap_o2                ! mmHg, partial pressure of O2 in capillary
    real(dp),intent(in) :: S_area                  ! mm2, surface area of alveoli in unit
    real(dp),intent(in) :: sat_o2                  ! fractional, saturation of blood with O2
    real(dp),intent(in) :: V_cap                   ! mm3, volume of capillary blood in unit
!!! Local parameters
    real(dp),parameter :: tol = 1.0e-6_dp
    real(dp),parameter :: phi = 0.8_dp             ! surface correction factor (dimensionless)
!!! Local variables
    real(dp) :: DeO2                               ! ethrocyte component of diffusing capacity, mm3/s/mmHg
    real(dp) :: DmO2                               ! membrane component of diffusing capacity, mm3/s/mmHg 
    real(dp) :: thetaO2                            ! reaction rate between gas and blood, mm^3/mm^3/mmHg/s

    if (abs(V_cap) < tol .or. abs(S_area) < tol) then
       DO2 = 0.0_dp
       return
    endif
    
!!! Calculate O2 diffusing capacity
    ! Swan PhD thesis p.26, equation (2.10) with units corrected
    ! note: using o2molvol conversion at body temp because of transition to gas
    thetaO2 = constants%kc_O2 * constants%sigma_o2 * (1.0_dp - sat_o2) * constants%kappa_o2 * &
         Hb_conc * constants%o2molvol_37deg    ! mm^3/mm^3/mmHg/s

    DeO2 = V_cap * thetaO2                     ! mm^3(blood) * mm^3(O2)/mm^3(blood)/mmHg/s == mm^3/s/mmHg
    
    ! Swan PhD thesis, equation (2.8) 
    DmO2 = constants%K * phi * S_area / species_params%tau_h  ! mm^3/s/mmHg == mm^2/s/mmHg * mm^2 / mm 
    
    DO2 = 1.0_dp / (1.0_dp / DmO2 + 1.0_dp / DeO2) ! mm3/s/mmHg
    
  end function calc_O2_diffusion_capacity
  
!!!##############################################################################

  subroutine update_terminal_conc_from_unit
    
    implicit none
  
    integer :: ne,np,nunit
    
    do nunit = 1,num_units
       ne = units(nunit)
       if(ne.ne.0)then
          np = elem_nodes(2,ne)
          node_field(nj_conc1,np) = gasex%conc_o2(nunit)
          node_field(nj_conc2,np) = gasex%conc_co2(nunit)
       endif
    enddo

  end subroutine update_terminal_conc_from_unit
  
!!!##############################################################################

  real(dp) function mmol_in_air(nj, unit_concs) result(gas_mmol)
    ! calculate the molar quantity (mmol) in alveolar side of each unit
    ! plus airway elements, and return for whole model
    ! not pure: updates elem_field(nj)

    use indices
!!! Inputs
    integer,intent(in) :: nj
    real(dp) :: unit_concs(:)
!!! Locals
    integer :: ne, ne0, np1, np2, nunit
    real(dp) :: average_conc, tree_mmol(num_nodes)

    tree_mmol = 0.0_dp
    elem_field(ne_mass,:) = 0.0_dp

    ! initialise to the mmol in each element
    do ne = 1, num_elems
       np1 = elem_nodes(1,ne)
       np2 = elem_nodes(2,ne)
       average_conc = (node_field(nj,np1) + node_field(nj,np2))/2.0_dp
       tree_mmol(ne) = average_conc * elem_field(ne_vol,ne) ! mmol/mm^3 * mm^3
       if(elem_ordrs(no_type,ne) == 1)then
          elem_field(ne_mass,ne) = tree_mmol(ne)
       endif
    enddo

    ! add the mmol in each elastic unit to terminal elements
    do nunit = 1,num_units
       ne = units(nunit)
       if(ne /= 0 .and. elem_cnct(1,0,ne) == 0)then
          tree_mmol(ne) = tree_mmol(ne) + gasex%volume(nunit) * unit_concs(nunit)
       endif
    enddo
    
    ! sum mmol recursively up the tree
    do ne = num_elems,2,-1 ! not for the stem branch; parent = 0
       ne0 = elem_cnct(-1,1,ne)
       tree_mmol(ne0) = tree_mmol(ne0) + dble(elem_symmetry(ne))*tree_mmol(ne)
    enddo !noelem

    elem_field(ne_mass,:) = tree_mmol(:)
    gas_mmol = tree_mmol(1)
    
  end function mmol_in_air

!!!##############################################################################

  real(dp) function mmol_in_blood() result (blood_mmol)

    use parameter_types,only: constants

    blood_mmol = sum(gasex%c_cap_o2 * gasex%V_cap) / constants%o2molvol_stpd

  end function mmol_in_blood
  
!!!##############################################################################

  subroutine write_field(fileid, file)
    ! for each node write out its coordinates, path length, V, Q, PO2
    ! if the node is at a terminal end, replace with unit values

!!! Inputs
    integer,intent(in) :: fileid
    character(len=*),intent(in) :: file
!!! Locals
    integer :: ne, ne_parent, np2, nunit
    real(dp) :: path_length
    character (len=4), parameter :: suffix = 'path'

    call openfile(fileid, file, suffix, append=.false.)
        
    write(fileid,'(i6, 7(f12.3))') 1, node_xyz(:,1), 0.0_dp,  node_field(nj_conc1,1) * &
         o2_cnv_c2pp, 0.0_dp, 0.0_dp
    do ne = 1,num_elems
       path_length = get_path_length(ne)
       np2 = elem_nodes(2,ne)
       write(fileid,'(i6, 7(f12.3))') np2, node_xyz(:,np2), path_length, &
            node_field(nj_conc1,np2) * o2_cnv_c2pp, 0.0_dp, 0.0_dp
    enddo

    do nunit = 1, num_units
       ne = units(nunit)
       path_length = get_path_length(ne)
       np2 = elem_nodes(2,ne)
       write(fileid,'(i6, 7(f12.3))') np2, node_xyz(:,np2), path_length, &
            node_field(nj_conc1,np2) * o2_cnv_c2pp, gasex%Vdot(nunit), gasex%Qdot(nunit)
    enddo
    
    close(fileid)
    
  end subroutine write_field

  
!!!##############################################################################

  real(dp) function get_path_length(ne) result (path_length)

    use indices
!!! Inputs
    integer, intent(in) :: ne
!!! Locals
    integer :: ne_parent, np2
    
    np2 = elem_nodes(2,ne)
    path_length = elem_field(ne_length,ne)
    if(ne.ne.1)then
       ne_parent = elem_cnct(-1,1,ne)
       path_length = path_length + elem_field(ne_length,ne_parent)
       do while(elem_cnct(-1,0,ne_parent).ne.0)
          ne_parent = elem_cnct(-1,1,ne_parent)
          path_length = path_length + elem_field(ne_length,ne_parent)
       enddo
    endif

  end function get_path_length


!!!##############################################################################

  subroutine flow_weighted_distribution()

    use indices
!!!Locals
    integer :: i, ne, nec, npc, np2, nunit
    real(dp) :: wt_sum
    
    do nunit = 1, num_units
       ne = units(nunit)
       np2 = elem_nodes(2,ne)
       node_field(nj_conc1,np2) = gasex%p_alv_o2(nunit) * o2_cnv_pp2c
    enddo

    do ne = num_elems, 1, -1
       np2 = elem_nodes(2,ne)
       wt_sum = 0.0_dp
       if(elem_cnct(1,0,ne) > 0)then
          do i = 1, elem_cnct(1,0,ne) ! each child branch
             nec = elem_cnct(1,i,ne)
             npc = elem_nodes(2,nec)
             wt_sum = wt_sum + elem_field(ne_Vdot,nec) * node_field(nj_conc1,npc)
          enddo
          node_field(nj_conc1,np2) = wt_sum / elem_field(ne_Vdot,ne)
       endif
    enddo
    
  end subroutine flow_weighted_distribution

  
!!!##############################################################################

  real(dp) function get_ABG_value(request) result (my_value)

!!! Inputs
    character(len=*) :: request

    select case (trim(request))
    case ('p_art_o2')
       my_value = bg_o2%p_art_o2
    case ('p_alv_o2')
       my_value = bg_o2%p_alv_o2
    case ('p_cap_o2')
       my_value = bg_o2%p_cap_o2
    case ('p_ven_o2')
       my_value = bg_o2%p_ven_o2
    case ('c_art_o2')
       my_value = bg_o2%c_art_o2
    case ('c_cap_o2')
       my_value = bg_o2%c_cap_o2
    case ('c_ven_o2')
       my_value = bg_o2%c_ven_o2
    case ('sat_art')
       my_value = bg_o2%sat_art
    case ('sat_ven')
       my_value = bg_o2%sat_ven
    case ('p_art_co2')
       my_value = bg_co2%p_art_co2
    case ('p_ven_co2')
       my_value = bg_co2%p_ven_co2
    case ('c_art_co2')
       my_value = bg_co2%c_art_co2
    case ('c_ven_co2')
       my_value = bg_co2%c_ven_co2
    case ('ph_art')
       my_value = bg_co2%ph_art
    case ('ph_ven')
       my_value = bg_co2%ph_ven
    case ('vo2_blood')
       my_value = bg_o2%vo2_blood
    end select

  end function get_ABG_value
  
!!!##############################################################################

end module gas_exchange

