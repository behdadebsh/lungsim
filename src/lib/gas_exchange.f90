module gas_exchange
!*Brief Description:* This module is for simulating lung gas exchange.
!
!*LICENSE:*
!
!
!
!*Full Description:*
!More info on what the module does if necessary
  !
  use arrays
  use diagnostics
  use indices
  use other_consts
  use precision

  implicit none

  !Module parameters

  !Module types

  !Module variables

  !Interfaces
  private 
  public initial_gasexchange, steadystate_gasexchange, steadystate_co2, &
       content_from_po2

  logical :: initialised_gastransfer = .false.
  real(dp),parameter :: standard_molar_vol = 22.4136e+3_dp ! at STP; mm^3/mmol
  real(dp),parameter :: p_water = 47.0_dp !mmHg
  real(dp),parameter :: Kr = 3.6e+6_dp !L/mol
  real(dp),parameter :: Kt = 10.0e+3_dp !L/mol
  real(dp),parameter :: L = 171.2e+6_dp
  real(dp) :: mcv, mch, mw, Hct, S2, tau_h
  !real(dp),parameter :: mcv = 90.0e-15_dp !L
  !real(dp),parameter :: mch = 30.0e-12_dp !grams
  !real(dp),parameter :: mw = 64458_dp !molecular weight of Hb, g/mol
  real(dp),parameter :: pH=7.4_dp ! pH of plasma
  real(dp),parameter :: temperature=37.0_dp !blood temperature,degrees
  real(dp),parameter :: press_atm=760.0_dp !atmospheric pressure, mmHg
  real(dp),parameter :: o2molvol = 25.44e+3_dp ! mm^3/mmol (converted from 22.41e3 at STP using V2=T2*V1/T1)
  real(dp),parameter :: Hb = 2.33e-3_dp !haemoglobin
  real(dp),parameter :: alphaO2 = 1.46e-6_dp ! O2 solubilitiy in water at T=37, mol/mmHg
  real(dp),parameter :: Wbl=0.81_dp !fractional water content of blood
  real(dp),parameter :: mmL_to_mlml = 25.452e-3_dp ! to convert mmol/L to mL/mL
  !!! The O2 and CO2 concentrations are stored in node_field(nj_conc1/nj_conc2,np)

!!! The O2 and CO2 partial pressures are stored in gasex_field(ng_p_x_y,nunit),
!!! using the nomenclature below for the indices.

!!! uses the following nomenclature:
  !  p_art_o2 ! arterial partial pressure of oxygen (PaO2)
  !  p_alv_o2 ! alveolar partial pressure of oxygen (PAO2)
  !  p_ven_o2 ! venous partial pressure of oxygen (PvO2)

  !  p_art_co2 ! arterial partial pressure of CO2 (PaCO2)
  !  p_alv_co2 ! alveolar partial pressure of CO2 (PACO2)
  !  p_ven_co2 ! venous partial pressure of CO2 (PvCO2)

  !  c_art_o2 ! arterial content of oxygen (CaO2)
  !  c_ven_o2 ! mixed venous content of oxygen (CvO2)

  !  c_art_co2 ! arterial content of CO2 (CaCO2)
  !  c_ven_co2 ! mixed venous content of CO2 (CvCO2)

contains
!
!##############################################################################
!
  subroutine initial_gasexchange(initial_concentration,surface_area,V_cap)
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_INITIAL_GASEXCHANGE" :: INITIAL_GASEXCHANGE

    !local variables
    real(dp),intent(in) :: initial_concentration
    real(dp), optional ::  surface_area,V_cap
    
    integer :: nunit
    real(dp) :: Vcap_unit
    real(dp),parameter :: p_water = 47.0_dp
    real(dp),parameter :: press_atm=760.0_dp !atmospheric pressure, mmHg
    
    
    character(len=60) :: sub_name
    
    sub_name = 'initial_gasexchange'
    call enter_exit(sub_name,1)
    
!!! allocate memory for the gasex_field array, if not already allocated
    if(.not.allocated(gasex_field)) allocate(gasex_field(num_gx,num_units))
    
!!! initialiase nj_conc2 (for CO2 concentration); currently hardcoded to 40 mmHg
    node_field(nj_conc2,1:num_nodes) = 40.0_dp/(o2molvol*(press_atm-p_water))
    write(*,'('' Initialising Palv_CO2 to 40 mmHg'')')
    
!!! initialise the gas exchange field for o2 partial pressures
    gasex_field(ng_p_alv_o2,1:num_units) = initial_concentration* &
         o2molvol*(press_atm-p_water)
    gasex_field(ng_p_cap_o2,1:num_units) = initial_concentration*&
         o2molvol*(press_atm-p_water)
    
    gasex_field(ng_p_alv_co2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    gasex_field(ng_p_ven_o2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    
    unit_field(nu_conc1,1:num_units) = gasex_field(ng_p_alv_o2,1:num_units)/&
         (o2molvol*(press_atm-p_water)) ! from mmHg to mmol/mm^3
    unit_field(nu_conc2,1:num_units) = gasex_field(ng_p_alv_co2,1:num_units)/&
         (o2molvol*(press_atm-p_water)) ! from mmHg to mmol/mm^3
    
!!! initialise the gas exchange field for co2 partial pressures
    gasex_field(ng_p_alv_co2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    gasex_field(ng_p_cap_co2,1:num_units) = 40.0_dp ! mmHg; should make this user defined
    gasex_field(ng_p_ven_co2,1:num_units) = 45.0_dp ! mmHg; should make this user defined
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
    
    call enter_exit(sub_name,2)
  end subroutine initial_gasexchange
  
!!! ######################################################################

  subroutine initialise_gastransfer()

    use parameter_types
    
    implicit none
  
    real(dp) :: p_i_o2

    ! set up species-specific values
    select case(species_params%species)
    case ('Human')
       mcv = 90e-3_dp        ! pico-L, mean RBC volume for human (ref range 80-96)
       mch = 30_dp           ! pico-grams, mean mass Hb/RBC for human (ref 27-31 picograms/cell)
       Hct = 0.45_dp         ! hematocrit (Dietel & Kampmann 1971)
       tau_h = 1.11e-3_dp    ! mm (1.11 um). Thickness of tissue barrier plus plasma. Weibel (1993)
       S2 = 2.34e4_dp        ! coefficient in Severinghaus 
    case ('Rabbit')
       mcv = 66.7e-3_dp       ! pico-L, mean RBC volume for human (ref range 80-96)
       mch = 20.95_dp          ! pico-grams, mean mass Hb/RBC for human (ref 27-31 picograms/cell)
       Hct = 0.436_dp        ! hematocrit (Dietel & Kampmann 1971)
       tau_h = 0.8e-3_dp   ! mm (1.11 um). Thickness of tissue barrier plus plasma. Weibel (1993)
       S2 = 3.5e4_dp        ! coefficient in Severinghaus 
    case ('Rat')
       mcv = 59.35e-3_dp       ! pico-L, mean RBC volume for human (ref range 80-96)
       mch = 17.9_dp          ! pico-grams, mean mass Hb/RBC for human (ref 27-31 picograms/cell)
       Hct = 0.5182_dp        ! hematocrit (Dietel & Kampmann 1971)
       tau_h = 0.754e-3_dp   ! mm (1.11 um). Thickness of tissue barrier plus plasma. Weibel (1993)
       S2 = 5.0e4_dp        ! coefficient in Severinghaus 
    case ('Mouse')
       mcv = 55.1e-3_dp     
       mch = 15.95_dp       
       Hct = 0.523_dp       
       tau_h = 0.7e-3_dp   
       S2 = 8.0e4_dp        
    case default
       write(*,*) 'Warning: unknown species, using human blood values as default'
       mcv = 90e-3_dp        ! pico-L, mean RBC volume for human (ref range 80-96)
       mch = 30_dp           ! pico-grams, mean mass Hb/RBC for human (ref 27-31 picograms/cell)
       Hct = 0.45_dp         ! hematocrit (Dietel & Kampmann 1971)
       tau_h = 1.11e-3_dp    ! mm (1.11 um). Thickness of tissue barrier plus plasma. Weibel (1993)
       S2 = 2.34e4_dp        ! coefficient in Severinghaus 
    end select

!!! allocate memory for the gasex_field array, if not already allocated
    if(.not.allocated(gasex_field)) allocate(gasex_field(num_gx,num_units))
    
    gasex_field(ng_p_cap_co2, :) = 40.0_dp
    gasex_field(ng_p_alv_co2, :) = 40.0_dp
    gasex_field(ng_p_cap_o2, :) = gx_params%init_p_alv_o2
    gasex_field(ng_p_alv_o2, :) = gx_params%init_p_alv_o2
    node_field(nj_conc1,:) = gx_params%init_p_alv_o2 / (constants%o2molvol_37deg * gx_params%press_atm)
    node_field(nj_conc2,:) = 40.0_dp / (constants%o2molvol_37deg * gx_params%press_atm)
    unit_field(nu_conc1,:) = gx_params%init_p_alv_o2 / (constants%o2molvol_37deg * gx_params%press_atm)
    unit_field(nu_conc2,:) = gasex_field(ng_p_alv_co2,:) / (constants%o2molvol_37deg * gx_params%press_atm)
    
    p_i_o2 = gx_params%FiO2 * (gx_params%press_atm - gx_params%press_h2O) ! accounting for humidification by the upper airway
    node_field(nj_conc1,1) = p_i_o2/gx_params%press_atm * constants%max_o2_concentration !mmol/mm^3, inspired O2
    node_field(nj_conc2,1) = 0.0_dp ! inspired CO2; should make FiCO2 user-defined

  end subroutine initialise_gastransfer
  
!!! ######################################################################

  subroutine steadystate_gasexchange(c_art_o2,c_ven_o2,&
       p_art_co2,p_art_o2,p_i_o2,p_ven_co2,p_ven_o2,shunt_fraction,&
       VCO2,VO2)

!!! Parameter List
    real(dp),intent(in) :: p_i_o2,shunt_fraction,VCO2,VO2
    real(dp), intent(inout) :: c_art_o2,c_ven_o2,p_art_co2,p_art_o2,p_ven_o2,p_ven_co2
!!! Local Variables
    integer :: counter,k,ne,np,nunit
    real(dp) :: c_art_co2,c_cap_co2,c_cap_o2,c_ven_co2,fun_o2, &
         fun_co2,fdash,p_cap_co2,p_cap_o2,p_art_co2_last, &
         p_art_o2_last,p_ven_co2_last,p_ven_o2_last,Q_total,V_total, &
         target_c_ven_co2,target_c_ven_o2,v_q,p_alv_o2,p_alv_co2
    
    real(dp),parameter :: m = 0.02386_dp, tol = 1.0e-6_dp
    logical :: continue
    character(len=60) :: sub_name
    
    sub_name = 'steadystate_gasexchange'
    call enter_exit(sub_name,1)

    ! call initialisation if not already done
    if(.not.initialised_gastransfer)then
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif
    
!!! Calculate steady state gas exchange for CO2
    p_ven_co2_last = p_ven_co2 ! updates at each iteration, until converged
    counter = 1                ! count the number of iterations
    continue = .true.
    do while(continue)
       Q_total = 0.0_dp        ! sum the blood flows; should be same as cardiac output!
       V_total = 0.0_dp        !sum the ventilations
       c_art_co2 = 0.0_dp      ! initialise the content in arterial blood
       do nunit = 1,num_units  ! for each elastic/gas exchange unit
          ne = units(nunit)    ! terminal element number
          p_cap_co2 = gasex_field(ng_p_cap_co2,nunit)      ! initialise capillary CO2
          v_q = unit_field(nu_Vdot0,nunit) &
               /unit_field(nu_perf,nunit)             ! the unit v/q
          if(dabs(v_q) .le. 1.0e-3_dp)then ! no ventilation; cap CO2 == venous CO2
             p_cap_co2 = p_ven_co2
          else                             ! calculate the steady-state PCO2
             fun_co2 = function_co2(v_q,p_cap_co2,p_ven_co2)
             fdash = fdash_co2(v_q,p_cap_co2)
             K=0
             do while(dabs(fun_co2).ge.1.0e-4_dp.and.(k.LT.200))
                K=K+1
                p_cap_co2 = p_cap_co2 - fun_CO2/fdash
                fun_co2 = function_co2(v_q,p_cap_co2,p_ven_co2)
                fdash = fdash_co2(v_q,p_cap_co2)
             enddo
          endif

          Q_total = Q_total + elem_units_below(ne) * dabs(unit_field(nu_perf,nunit)) !mm3/s
          V_total = V_total + elem_units_below(ne) * dabs(unit_field(nu_Vdot0,nunit))

!!! including a limitation that p_cap_co2 cannot be less than zero
          p_cap_co2 = max(p_cap_co2,0.0_dp)


          gasex_field(ng_p_cap_co2,nunit) = p_cap_co2 ! store the capillary/alveolar CO2
          gasex_field(ng_p_alv_co2,nunit) = p_cap_co2 ! store the capillary/alveolar CO2

!!! calculate the content of CO2 in capillary blood, given the partial pressure
          c_cap_co2 = m*p_cap_co2/(1 + m*p_cap_co2)
!!! sum the content in arterial blood (flow weighted sum)
          c_art_co2 = c_art_co2 + elem_units_below(ne)* &
               (c_cap_co2*dabs(unit_field(nu_perf,nunit))) !flow-weighted
!! sum the alveolar co2
          p_alv_co2=p_alv_co2 + elem_units_below(ne)* &
               (p_cap_co2*dabs(unit_field(nu_Vdot0,nunit))) !flow-weighted

       enddo !nunit
!!! update the arterial content of CO2
       c_art_co2 = c_art_co2/Q_total !ml CO2 / ml blood
!!! include the shunt fraction in total arterial CO2
       c_ven_co2 = m * p_ven_co2/(1+m*p_ven_co2)
       c_art_co2 = c_art_co2*(1-SHUNT_FRACTION)+c_ven_co2*SHUNT_FRACTION
 !!  summed alveolar pco2
       p_alv_co2=p_alv_co2/elem_field(ne_Vdot,1)

!!! calculate the partial pressure of pulmonary arterial CO2:
       p_art_co2 = 1/(m*(1-c_art_co2)) ! initialise p_art_co2
       K=0 !counter
       fun_co2 = m*p_art_co2/(1+m*p_art_co2)-c_art_co2
       do while (dabs(fun_co2).ge.1.0e-4_dp.and.(k.lt.200))
          K=K+1
          fdash=m/(1+m*p_art_co2)**2
          p_art_co2 = p_art_co2 - fun_co2/fdash
          fun_co2 = m*p_art_co2/(1+m*p_art_co2)-c_art_co2
       enddo !while
!!! find the p_ven_co2 for the new (target) content of venous CO2
       target_c_ven_co2 = c_art_co2 + VCO2/(elem_field(ne_Qdot,1)*(1+SHUNT_FRACTION))
       !mL(CO2)/mL(blood)   mL/mL  [mm^3/s]/[mm^3/s]
       p_ven_co2 = 1/(m*(1-target_c_ven_co2))
       K=0
       fun_co2=m*p_ven_co2/(1+m*p_ven_co2)-target_c_ven_CO2
       do while (dabs(fun_co2).ge.1.0e-4_dp.and.(k.lt.200))
          K=K+1
          fdash=m/(1+m*p_ven_co2)**2
          p_ven_co2 = p_ven_co2-fun_co2/fdash
          fun_co2 = m*p_ven_co2/(1+m*p_ven_co2)-target_c_ven_co2
       enddo !while
!!! now have updated values for p_art_co2 and p_ven_co2
       write(*,'('' Interim PPs:'',4(f8.3))') p_art_o2,p_ven_o2,p_art_co2,p_ven_co2
!!! check whether p_ven_co2 and p_art_co2 have converged
       if(counter.gt.1)then
          if(dabs(p_ven_co2-p_ven_co2_last)/p_ven_co2_last.lt.tol.and. &
               dabs(p_art_co2-p_art_co2_last)/p_art_co2_last.lt.tol) then
             continue = .false.
          else
             if(counter.gt.200) continue = .false.
             counter=counter+1
             p_ven_co2_last = p_ven_co2
             p_art_co2_last = p_art_co2
          endif !convergence check
       else
          counter = counter+1
          p_ven_co2_last = p_ven_co2
          p_art_co2_last = p_art_co2
       endif

    enddo !while continue
!    read(*,*)    

    write(*,'('' Total blood flow ='',F10.1,'' mm3/s,&
         & alveolar ventilation='',F10.1,'' mm3/s'')') Q_total,V_total
    write(*,'('' Steady-state P_art_CO2 ='',F6.1,'' mmHg,&
         & P_ven_CO2='',F6.1,'' mmHg'')') p_art_co2,p_ven_co2
    write(*,'(''               P_alv_CO2 ='',F6.1,'' mmHg,&
         &  P(A-a)CO2='',F6.1,'' mmHg'')') p_alv_co2,p_alv_co2-p_art_co2

!!! Calculate steady state gas exchange for O2
    p_ven_o2_last = p_ven_o2
    counter = 1
    continue = .true.
    do while (continue)
       c_art_o2 = 0.0_dp
       p_alv_o2=0.0_dp
       do nunit=1,num_units
          ne = units(nunit)
          p_cap_o2 = gasex_field(ng_p_cap_o2,nunit) !initialise
          v_q = unit_field(nu_Vdot0,nunit) &
               /unit_field(nu_perf,nunit)             ! the unit v/q
          if(abs(v_q) .le. 1.0e-3_dp)then
             p_cap_o2 = p_ven_o2
          else ! calculate the steady-state p_cap_o2
             p_cap_co2 = gasex_field(ng_p_cap_co2,nunit)
             fun_o2 = function_o2(p_cap_co2,p_cap_o2,p_i_o2,&
                  p_ven_co2,p_ven_o2,v_q)
             K=0
             do while (abs(fun_o2).ge.1.0e-4_dp.and.(k.lt.200))
                K=K+1
                fdash = fdash_o2(p_cap_co2,p_cap_o2,v_q)
                p_cap_o2 = p_cap_o2 - fun_o2/fdash
                fun_o2 = function_o2(p_cap_co2,p_cap_o2,p_i_o2,&
                     p_ven_co2,p_ven_o2,v_q)
             enddo
          endif
!!! including a limitation that p_cap_o2 cannot be less than p_ven_o2
          p_cap_o2 = max(p_cap_o2,p_ven_o2)

          gasex_field(ng_p_cap_o2,nunit) = p_cap_o2
          gasex_field(ng_p_alv_o2,nunit) = p_cap_o2

!!! calculate the content of O2 in capillary blood, given the partial pressure
          c_cap_o2 = content_from_po2(p_cap_co2,p_cap_o2)
!!! sum the content in arterial blood (flow weighted sum)
          c_art_o2 = c_art_o2 + elem_units_below(ne)* &
               (c_cap_o2*dabs(unit_field(nu_perf,nunit))) !flow-weighted
!! sum the alveolar o2
          p_alv_o2=p_alv_o2 + elem_units_below(ne)* &
               (p_cap_o2*dabs(unit_field(nu_Vdot0,nunit))) !flow-weighted

         ! write(*,*) 'V/Q=',v_q,' pO2=',p_cap_o2,c_cap_o2,c_art_o2
       enddo !nunit

!!! update the arterial content of O2
       c_art_o2 = c_art_o2/Q_total !ml O2 / ml blood
!!! include the shunt fraction in total arterial O2
       c_ven_o2 = content_from_po2(p_ven_co2,p_ven_o2)
       c_art_o2 = c_art_o2*(1.0_dp-SHUNT_FRACTION)+c_ven_o2*SHUNT_FRACTION
!!! calculate the partial pressure of pulmonary arterial O2:
       p_art_o2 = po2_from_content(c_art_o2,p_art_co2)
!!  summed alveolar po2
       p_alv_o2=p_alv_o2/elem_field(ne_Vdot,1)
!!! find the p_ven_o2 for the new (target) content of venous O2
       target_c_ven_o2 = c_art_o2 - VO2/(elem_field(ne_Qdot,1)*(1+SHUNT_FRACTION))
       !mL(O2)/mL(blood)   mL/mL   [mm^3/s]/[mm^3/s]
       p_ven_o2 = po2_from_content(target_c_ven_o2,p_ven_co2)


!!! now have updated values for p_art_o2 and p_ven_o2
!!! check whether p_ven_o2 and p_art_o2 have converged
       if(counter.gt.1)then
          if(abs(p_ven_o2-p_ven_o2_last)/p_ven_o2_last.lt.tol.and. &
               abs(p_art_o2-p_art_o2_last)/p_art_o2_last.lt.tol) then
             continue = .false.
          else
             if(counter.gt.200) continue = .false. !ARC made this one
             counter=counter+1
             p_ven_o2_last = p_ven_o2
             p_art_o2_last = p_art_o2
          endif !convergence check
       else
          counter=counter+1
          p_ven_o2_last = p_ven_o2
          p_art_o2_last = p_art_o2
       endif

    enddo !while continue

    write(*,'('' Steady-state  P_art_O2 ='',F6.1,'' mmHg,&
         &  P_ven_O2='',F6.1,'' mmHg'')') p_art_o2,p_ven_o2
    write(*,'(''               P_alv_O2 ='',F6.1,'' mmHg,&
         &  P(A-a)O2='',F6.1,'' mmHg'')') p_alv_o2,p_alv_o2-p_art_o2

    do nunit=1,num_units
       ne=units(nunit)
       np=elem_nodes(2,ne)
       node_field(nj_conc1,np) = p_cap_o2/(o2molvol*(press_atm-p_water))
       node_field(nj_conc2,np) = p_cap_co2/(o2molvol*(press_atm-p_water))
!!! note: and update the pco2
    enddo

!!! calculate concentrations in the gas exchange units from partial pressures.
!!! this information is used in 'track_back' during expiration
    unit_field(nu_conc1,1:num_units) = gasex_field(ng_p_alv_o2,1:num_units)/&
         (o2molvol*(press_atm-p_water)) ! from mmHg to mmol/mm^3
    unit_field(nu_conc2,1:num_units) = gasex_field(ng_p_alv_co2,1:num_units)/&
         (o2molvol*(press_atm-p_water)) ! from mmHg to mmol/mm^3

    call enter_exit(sub_name,2)

  end subroutine steadystate_gasexchange

  !!! ####################################################

  function steadystate_CO2 (p_art_co20, p_art_o2, p_ven_co20, p_ven_o2, Vdot_alv) result(p_art_co2)

    ! Uses CO2 content<->PCO2 mapping with Haldane coupling:
    !   content_from_pco2(pco2, so2, pH, Hb_g_dL) returns ml(gas)/ml(blood)
    !   pco2_from_content(c_mlml, so2, pH, Hb_g_dL) returns mmHg

    ! steady-state is reached when there is no change between current and previous p_ven_co2

    use precision, only: dp
    use parameter_types
    implicit none

    ! Arguments
    real(dp), intent(in) :: p_art_co20, p_art_o2, p_ven_co20, p_ven_o2, Vdot_alv
    ! Local variables
    integer :: counter, k, ne, np, nunit
    real(dp) :: cardiac_output, c_art_co2, c_cap_co2, c_ven_co2, fdash, fun_co2, Hb_g_dL, &
         p_art_co2, p_art_co2_last, p_cap_co2, pH_a, pH_v, p_ven_co2, &
         p_ven_co2_last, Q_total, RV_flow, SaO2, &
         shunt_flow, SvO2, target_c_ven_co2, VCO2, v_q
    logical :: continue

    ! call initialisation if not already done
    if(.not.initialised_gastransfer)then
       call initialise_gastransfer
       initialised_gastransfer = .true.
    endif
    
    pH_a = gx_params%pHa
    pH_v = pH_a - 0.03_dp
    Hb_g_dL = gx_params%Hb

    VCO2 = gx_params%VCO2

    cardiac_output = Q_params%cardiac_output
    shunt_flow = Q_params%shunt_fraction * cardiac_output
    RV_flow = cardiac_output - shunt_flow

    p_ven_co2 = p_ven_co20
    p_art_co2 = p_art_co20
    p_ven_co2_last = p_ven_co2
    p_art_co2_last = p_art_co2
    counter = 1
    continue = .true.

    do while (continue)

       Q_total   = 0.0_dp
       c_art_co2 = 0.0_dp

       do nunit = 1, num_units
          ne = units(nunit)
          ! Initialise to previous capillary value
          p_cap_co2 = gasex_field(ng_p_cap_co2, nunit)
          if (unit_field(nu_perf, nunit) < loose_tol) then
             v_q = 1.0e5_dp ! set to high enough, but not ridiculous, value
          else
             v_q = Vdot_alv / RV_flow * &
                  (unit_field(nu_Vdot0, nunit) / elem_field(ne_Vdot,1)) / &
                  unit_field(nu_perf, nunit)
          endif
          if (abs(v_q) <= 1.0e-3_dp) then
             ! no ventilation: capillary CO2 tends to venous CO2
             p_cap_co2 = p_ven_co2
          elseif (abs(v_q) > 100.0_dp) then
             ! essentially infinite ventilation: alveolar/capillary CO2 ~ 0
             p_cap_co2 = 0.0_dp
          else
             k = 0
             do
                fun_co2 = function_co2(v_q, p_cap_co2, p_ven_co2)
                if (abs(fun_co2) < 1.0e-4_dp) exit
                if (k >= 200) exit
                fdash = fdash_co2(v_q, p_cap_co2)
                if (abs(fdash) < zero_tol) exit
                p_cap_co2 = p_cap_co2 - fun_co2 / fdash
                k = k + 1
             end do
          endif

          p_cap_co2 = max(p_cap_co2, 0.0_dp)

          ! Store alveolar/capillary PCO2
          gasex_field(ng_p_cap_co2, nunit) = p_cap_co2
          gasex_field(ng_p_alv_co2, nunit) = p_cap_co2

          if(p_cap_co2 < 1.0e-2_dp)then
             SvO2 = 0.0_dp
             c_cap_co2 = 0.0_dp
          else
             ! Compute O2 saturation in the capillary for Haldane effect on CO2 content.
             SvO2 = saturation_of_o2(p_cap_co2, gasex_field(ng_p_cap_o2, nunit))
             ! CO2 content in capillary blood in ml/ml
             c_cap_co2 = co2_content_from_pco2(p_cap_co2, SvO2, pH_a, Hb_g_dL)
             ! Flow-weighted sum of CO2 content
             Q_total   = Q_total + abs(unit_field(nu_perf, nunit)) * units_effective(nunit)
             c_art_co2 = c_art_co2 + units_effective(nunit) * (c_cap_co2 * abs(unit_field(nu_perf, nunit)))
          endif
       end do ! nunit

       ! Normalise by total flow
       if (Q_total > 0.0_dp) then
          c_art_co2 = c_art_co2 / Q_total
       else
          c_art_co2 = 0.0_dp
       endif

       ! Mixed venous CO2 content (ml/ml) at current guess p_ven_co2, using venous oxygenation
       SvO2 = saturation_of_o2(p_ven_co2, p_ven_o2)
       c_ven_co2 = co2_content_from_pco2(p_ven_co2, SvO2, pH_v, Hb_g_dL)

       ! Add shunt
       c_art_co2 = (c_art_co2 * RV_flow + c_ven_co2 * shunt_flow) / (RV_flow + shunt_flow)

       !infer arterial PCO2 from arterial CO2 content using arterial oxygenation (SaO2)
       SaO2 = saturation_of_o2(p_art_co2, p_art_o2)  ! uses current p_art_co2
       p_art_co2 = pco2_from_co2content(c_art_co2, SaO2, pH_a, Hb_g_dL)

       ! Tissue addition of CO2: target venous CO2 content
       target_c_ven_co2 = c_art_co2 + VCO2 / (RV_flow + shunt_flow)   ! units: (ml/ml)

       ! Infer venous PCO2 from target venous content, using current venous oxygenation SvO2
       SvO2 = saturation_of_o2(p_ven_co2, p_ven_o2)
       p_ven_co2 = pco2_from_co2content(target_c_ven_co2, SvO2, pH_v, Hb_g_dL)

       ! Convergence check
       if (counter > 1) then
          if (abs(p_ven_co2 - p_ven_co2_last) / max(zero_tol, abs(p_ven_co2_last)) < loose_tol .and. &
               abs(p_art_co2 - p_art_co2_last) / max(zero_tol, abs(p_art_co2_last)) < loose_tol) then
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
       node_field(nj_conc2, np) = gasex_field(ng_p_cap_co2, nunit) / (constants%o2molvol_37deg * gx_params%press_atm)
    end do

    unit_field(nu_conc2, 1:num_units) = gasex_field(ng_p_alv_co2, 1:num_units) / (constants%o2molvol_37deg * gx_params%press_atm)

  end function steadystate_CO2
  

!!! ####################################################

  function function_co2 ( v_q, p_cap_co2, p_ven_co2)

    real(dp),intent(in) :: v_q, p_cap_co2, p_ven_co2
    real(dp),parameter :: m = 0.02386_dp
    real(dp) :: function_co2

    function_co2 = v_q * p_cap_co2 + (press_atm - p_water) * &
         (m*p_cap_co2/(1 + m*p_cap_co2) - m*p_ven_co2/(1 + m*p_ven_co2))

  end function function_co2

!!! ####################################################

  function function_o2(p_cap_co2,p_cap_o2,p_i_o2,&
       p_ven_co2,p_ven_o2,v_q)

    use parameter_types

!!! Parameters
    real(dp),intent (in) :: p_cap_co2,p_cap_o2,p_i_o2,p_ven_co2,&
         p_ven_o2,v_q
!!! Local variables
    real(dp) :: c_cap_o2,c_ven_o2,function_o2

    c_cap_o2 = content_from_po2(p_cap_co2,p_cap_o2)
    c_ven_o2 = content_from_po2(p_ven_co2,p_ven_o2)

    function_o2 = v_q * (p_i_o2 - p_cap_o2) - (gx_params%press_atm - &
         gx_params%press_h2o) * (c_cap_o2 - c_ven_o2)

  end function function_o2

!!! ####################################################

  function fdash_co2 (v_q,p_cap_co2)

    use parameter_types

!!! Parameters
    real(dp),intent(in) :: v_q,p_cap_co2
!!! Local variables
    real(dp) :: fdash_co2
    real(dp),parameter :: m = 0.02386_dp

    fdash_co2 = v_q + (gx_params%press_atm - &
         gx_params%press_h2o) * m/(1 + m * p_cap_co2)**2

  end function fdash_co2

!!! ####################################################

  function fdash_o2 (p_x_co2,p_x_o2,v_q)

    use parameter_types

!!! Parameters
    real(dp),intent(in) :: p_x_co2, v_q
    real(dp) :: p_x_o2
!!! Local variables
    real(dp),parameter :: A1=-8.538889e+3_dp, A2=2.121401e+3_dp, A3=-6.707399e+1_dp,&
         A4=9.359609e+5_dp, A5=-3.134626e+4_dp, A6=2.396167e+3_dp, A7=-6.710441e+1_dp
    real(dp) :: aa,bb,aa_dash,bb_dash,C,gamma, Hb_conc, pH, X
    real(dp) :: fdash_o2

    Hb_conc = gx_params%Hb * 10.0_dp / constants%mw  ! g/dL * 10 dL/L / (g/mol) --> mol/L
    pH = gx_params%pHa                               ! assumes pH for arterial blood
    
    gamma = 10.0_dp**(0.024_dp*(37.0_dp-temperature)+0.4_dp*(pH-7.4_dp)+ &
         0.06_dp*(DLOG10(DBLE(40.0_dp))-DLOG10(DBLE(p_x_co2))))
    X = p_x_o2*gamma

    aa = (X*(X*(X*(X+A3)+A2)+A1))
    bb = (X*(X*(X*(X+A7)+A6)+A5)+A4)
    aa_dash = gamma*(4.0_dp*X**3 + 3.0_dp*A3*X**2 + 2.0_dp*A2*X+A1)
    bb_dash = gamma*(4.0_dp*X**3 + 3.0_dp*A7*X**2 + 2.0_dp*A6*X+A5)
    C = (Wbl*alphaO2 + 4.0_dp* Hb_conc *(aa_dash*bb-aa*bb_dash)/bb**2)*(O2molVol*1.0e-3_dp)
    
    FDASH_O2 = -v_q - (gx_params%press_atm - gx_params%press_h2o) * C

    RETURN
  END function fdash_o2


!!! ####################################################

  function content_from_po2 (PCO2,po2) result(c_from_po2)

    use parameter_types
    
!!! Kelman method for calculating the content of O2 from partial pressure

!!! Parameters
    real(dp) :: PCo2,po2
!!! Local variables
    real(dp) :: c_from_po2, Hb_conc, ShbO2

    Hb_conc = gx_params%Hb * 10.0_dp / constants%mw  ! g/dL * 10 dL/L / (g/mol) --> mol/L
    
    if(dabs(po2).lt.zero_tol)then
       SHbO2 = 0.0_dp
       c_from_po2 = 0.0_dp
    else
       SHbO2 = saturation_of_o2(pco2,po2)

!!! Calculate O2 content (convert from molar to ml O2 per ml blood)
!!! o2molvol is in units of mm^3/mmol; alphaO2 is mol/mmHg; content should be ml/ml
       c_from_po2 = (constants%Wbl * constants%alphaO2 * PO2 + 4.0_dp * Hb_conc * SHbO2) * &
            (constants%o2molvol_37deg * 1.0e-3_dp)
    endif

    if(c_from_po2.LT.0.0_dp) c_from_po2=0.0_dp !curve fit behaves poorly at low PO2

  end function content_from_po2


!!! ####################################################

  function saturation_of_o2 (PCO2,po2)

!!! Kelman method for calculating the saturation of O2 from partial pressure

!!! Parameters
    real(dp),intent(in) :: PCo2,po2
!!! Local variables
    real(dp),parameter :: A1=-8.538889e+3_dp, A2=2.121401e+3_dp, A3=-6.707399e+1_dp,&
         A4=9.359609e+5_dp, A5=-3.134626e+4_dp, A6=2.396167e+3_dp, A7=-6.710441e+1_dp
    real(dp) :: saturation_of_o2,X,ShbO2

    if(dabs(po2).lt.zero_tol)then
       SHbO2 = 0.0_dp
    else

!!! Calculate Hb-O2 saturation
       X=PO2*10.0_dp**(0.024_dp*(37.0_dp-temperature)+0.4_dp*(pH-7.4_dp)+ &
            0.06_dp*(DLOG10(DBLE(40.0_dp))-DLOG10(DBLE(PCO2))))
       SHbO2=(X*(X*(X*(X+A3)+A2)+A1))/(X*(X*(X*(X+A7)+A6)+A5)+A4)
    endif
    if(SHbO2.LT.0.0_dp) SHbO2 = 0.0_dp

    saturation_of_o2 = SHbO2

  end function saturation_of_o2

!!! ####################################################
  
   function po2_from_content(c_o2,p_co2)

!!! Parameter List
    real(dp),intent(in) :: c_o2,p_co2
!!! Local Variables
    integer :: i
    integer,parameter :: max_iterations = 100
    real(dp) :: c_o2_new,c_o2_old,diff_new,diff_old,diff_step,&
         inc,p_o2_new,p_o2_old,po2_from_content
    real(dp),parameter :: tolerance=1.0e-5_dp
    logical :: converged

    if(dabs(c_o2).lt.tolerance)then
       po2_from_content = 0.0_dp
    else
       converged = .false.
       i = 1
       inc = 10.0_dp
       ! initial guess for p_x_o2
       p_o2_new = 50.0_dp  ! mmHg
       c_o2_old = 0.0_dp   ! updated after each iteration from c_o2_new
       c_o2_new = content_from_po2(p_co2,p_o2_new)
       ! Check convergence
       if(dabs((c_o2_new - c_o2)/c_o2).lt.tolerance*c_o2) converged =.true.
       ! Loop to find PO2 value
       do while (.not.converged.and.(i.lt.max_iterations))
          ! Modify increment size
          if(c_o2_new.gt.c_o2)then
             inc = -dabs(inc)
          elseif(c_o2_new.lt.c_o2)then
             inc = dabs(inc)
          endif
          if(i.gt.1)then
             diff_new = c_o2_new - c_o2
             diff_old = c_o2_old - c_o2
             diff_step = dabs(c_o2_new-c_o2_old)
             if((diff_old.gt.0.0_dp.and.diff_new.lt.0.0_dp).or. &
                  (diff_old.lt.0.0_dp.and.diff_new.gt.0.0_dp))then ! the last 2 steps straddle point
                inc=inc/2.0_dp
             elseif(dabs(diff_new).gt.diff_step)THEN
                inc=inc*2.0_dp
             endif
          endif

          ! Increment to find new PO2
          p_o2_old = p_o2_new
          c_o2_old = c_o2_new
          p_o2_new = p_o2_new + inc
          c_o2_new = content_from_po2(p_co2,p_o2_new)
          ! Check convergence
          if(dabs((c_o2_new-c_o2)/c_o2).LT.tolerance*c_o2) converged = .true.

          i=i+1

       enddo !while

       if(.not.converged) write(*,'(''>>Error: PO2 value not found'')')

       po2_from_content = p_o2_new

    endif

  end function po2_from_content

!!!#########################################################################################

  function co2_content_from_pco2(pco2, so2_frac, pH, Hb_g_dL) result(c_mlml)
    
    ! Implementation of Douglas, J Appl Physiol, 1985. 
    
    real(dp), intent(in) :: pco2        ! mmHg
    real(dp), intent(in) :: so2_frac    ! O2 saturation, fraction 0-1
    real(dp), intent(in) :: pH          ! blood pH
    real(dp), intent(in) :: Hb_g_dL     ! haemoglobin concentration, g/dL
    real(dp) :: c_mlml                  ! mL CO2 (STPD) / mL blood

    ! Local variables
    real(dp) :: alpha_co2, pkp, c_plasma_mldl, blood_factor, c_blood_mldl
    
    alpha_co2 = 0.0307_dp! CO2 solubility in plasma, mmol / (L * mmHg)
    ! Apparent dissociation constant for plasma CO2 / bicarbonate
    pkp = 6.125_dp - log10(1.0_dp + 10.0_dp**(pH - 8.7_dp))
    ! Plasma CO2 content in mL STPD / dL. 2.226 converts mmol/L to mL STPD/dL
    c_plasma_mldl = 2.226_dp * alpha_co2 * pco2 * (1.0_dp + 10.0_dp**(pH - pkp))
    ! Douglas whole-blood correction factor; so2_frac must be 0-1 here
    blood_factor = 1.0_dp - (0.0289_dp * Hb_g_dL) / &
         ((3.352_dp - 0.456_dp * so2_frac) * (8.142_dp - pH))
    c_blood_mldl = c_plasma_mldl * blood_factor  ! Whole-blood CO2 content in mL STPD / dL
    c_mlml = c_blood_mldl / 100.0_dp  ! Convert mL/dL -> mL/mL
    
  end function co2_content_from_pco2

!!!#########################################################################################

  function pco2_from_co2content(c_mlml, so2, pH, Hb_g_dL) result(pco2)

    real(dp), intent(in) :: c_mlml, so2, pH, Hb_g_dL
    real(dp) :: cc_target, pco2
    real(dp) :: lo, hi, mid
    real(dp) :: f_lo, f_hi, f_mid
    integer :: it
    integer, parameter :: itmax = 80
    real(dp), parameter :: tol = 1.0e-8_dp
    
    cc_target = c_mlml / mmL_to_mlml ! Convert ml/ml → mmol/L
    
    ! Physiologic bracket in mmHg
    lo = 0.1_dp
    hi = 200.0_dp
    
    f_lo = co2_content_from_pco2(lo, so2, pH, Hb_g_dL)/ mmL_to_mlml - cc_target
    f_hi = co2_content_from_pco2(hi, so2, pH, Hb_g_dL)/ mmL_to_mlml - cc_target
    
    ! If target is out of bracket, expand hi a bit (rare)
    if (f_lo*f_hi > 0.0_dp) then
       hi = 400.0_dp
       f_hi = co2_content_from_pco2(hi, so2, pH, Hb_g_dL)/ mmL_to_mlml - cc_target
       if (f_lo*f_hi > 0.0_dp) then
          ! end safely (return best guess)
          pco2 = max(lo, min(hi, 40.0_dp))
          return
       endif
    endif
    
    do it = 1, itmax
       mid = 0.5_dp*(lo + hi)
       f_mid = co2_content_from_pco2(mid, so2, pH, Hb_g_dL)/ mmL_to_mlml - cc_target
       
       if (abs(f_mid) < tol) exit
       
       if (f_lo*f_mid <= 0.0_dp) then
          hi = mid
          f_hi = f_mid
       else
          lo = mid
          f_lo = f_mid
       endif
    enddo
    
    pco2 = 0.5_dp*(lo + hi)
    
  end function pco2_from_co2content

!!!#########################################################################################

end module gas_exchange
