module lymphatics
  !*Brief Description:* This module contains all lymphatic-specific subroutines
  !
  !*LICENSE:*
  !
  !Test Test Test
  !
  !*Full Description:*
  !
  !This module contains code for pulmonary fluid flux within the alveolo-capillary network,
  !and lymph transport through lymphatic collecting vessels.
  
  use arrays
  use diagnostics
  use indices
  use precision ! sets dp for precision
  
  implicit none
  
  !Module parameters
  
  !Module types
  
  !Module variables
  
  !Interfaces
!  private
  public alveolar_capillary_flux
  public lymphatic_transport
contains
  
!!!#############################################################################
  
  subroutine alveolar_capillary_flux(ne)
    !*alveolar_capillary_flux:* calculate fluid flux from blood to interstitium
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_ALVEOLAR_CAPILLARY_FLUX" :: ALVEOLAR_CAPILLARY_FLUX

    use other_consts,only: pi
    
    integer,intent(in) :: ne

    ! Baseline value parameters (eventually will be user-defined?)
    integer,parameter :: sex = 0 ! 0 = male, 1 = female
    integer,parameter :: au = 32768 ! number of acinar units
    real(dp),parameter :: height = 175.0_dp, weight = 75.0_dp, body_mass = 74.0_dp
    real(dp),parameter :: lymphatic_density = 1.0_dp !to be calculated from CT??
    
    ! Capillary parameters
    real(dp),parameter :: capillary_conductivity = 4.41335e-8_dp !  obtained from literature - Parker range in cm H2O
    real(dp),parameter :: reflection_coefficient = 0.0_dp

    ! Initial lymphatics parameters
    real(dp),parameter :: lymphatic_resistance = 1000.0_dp ! Ngo 2019
    real(dp),parameter :: valve_resistance = 0.020_dp     ! pressure requirement to overcome the valve resistance and cause flow OR the pressure lost during open the valve
    real(dp),parameter :: lymphatic_integrity = 1.0_dp ! a measure of how 'leaky' the lymphatic vessels are and prone to backflow

    ! Osmotic pressure parameters
    real(dp),parameter :: capillary_molar_conc = 0.0010250_dp ! mol/L in blood plasma
    real(dp),parameter :: IGC = 62.3630_dp ! ideal gas constant in mmHg.L.mol-1.K-1
    real(dp),parameter :: T = 310.0_dp ! temperature in Kelvin - based on 37C blood temp

    ! Simulation parameters
    real(dp),parameter :: breathing_rate = 15.0_dp

    
    ! Local variables
    integer :: i,liflowcount,nunit,printcount,time_loop
    real(dp) :: alveolar_volume,breathing_function,capillary_flow,capillary_vps,capillary_osmotic,capillary_osm_n, &
         capillary_volume,capillary_volume_raw,cap_osm_conc,diffusion,excess,flux_a,flux_b,flux_c,gas_diffusion_restriction, &
         initial_lymphatic_flow,initial_lymphatic_pressure,initial_lymphatic_surface_area,initial_lymphatic_volume, &
         initial_lymph_conc,initial_osm_n,interstitial_capacity,interstitial_capacity_a,interstitial_capacity_b, &
         interstitial_osmotic,interstitial_pressure_a,interstitial_pressure_b,interstitial_saturation,interstitial_volume, &
         int_osm_conc,int_osm_n,interstitial_volume_a,interstitial_volume_b,lung_mass,lymphatic_conductivity,net_flux, &
         osm_flux,osm_n_flux,overflow,sumflux,sumuptake,test_time,time,time2,time_dt,time_period,time_variable,total_flux, &
         total_hydro_flux,total_osm_flux,capillary_pressure,transit_time,capillary_SA
    
    character(len=60) :: sub_name
    
    ! --------------------------------------------------------------------------
    
    sub_name = 'alveolar_capillary_flux'
    call enter_exit(sub_name,1)

    ! get information for the unit fron unit_field 
    ! ne is the 'linker' element in the artery-capillary-vein model, so nunit is for the parent element
    nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne))) 
    capillary_pressure = unit_field(nu_blood_press,nunit)/133.0_dp
    transit_time = unit_field(nu_tt,nunit)
    capillary_SA = unit_field(nu_sa,nunit)
    ! 
    
    ! Calculated values
    lung_mass = abs(real((1-sex)*840.0_dp))+real(sex)*639.0_dp  ! g; gives female lung weight of 639g and male of 840g

    ! interstitial values
    interstitial_capacity = ((30.0_dp*(lung_mass/100.0_dp))/real(au))*1000.0_dp  ! maximal volume before spillover into alveolar in mm^3 - based on 30ml.100g of fluid (Drake 2002)
    interstitial_capacity_a = 0.005_dp*interstitial_capacity
    interstitial_capacity_b = 0.995_dp*interstitial_capacity
    interstitial_volume_a = 0.0_dp
    interstitial_volume_b = 0.49_dp*interstitial_capacity
    alveolar_volume = 0.0_dp
    liflowcount = 0
    initial_lymphatic_surface_area = capillary_SA*3.648_dp  ! 

    ! initial lymphatic values
    initial_lymphatic_volume = 0.0_dp

    ! Osmotic pressures
    i = 1.0_dp
    capillary_osmotic = i*capillary_molar_conc*IGC*T
    capillary_osm_n = 0.0_dp
    interstitial_osmotic = 0.0_dp
    int_osm_n = 0.0_dp
    initial_osm_n = 0.0_dp
    total_osm_flux = 0.0_dp
  
    time = 0.0_dp
    time2 = 1.0_dp
    printcount = 0
    total_hydro_flux = 0.0_dp
    time_loop = 96

    breathing_function = (2.0_dp*pi)/(60.0_dp/breathing_rate)

    !write(*,'('' breathing rate is '',f8.2,'' breaths per minute'')') breathing_rate
    !write(*,'('' lung mass is '',f8.2)') lung_mass
    !write(*,'('' cap SA is '',f8.2)') capillary_SA
    !write(*,'('' int cap is '',f8.2)') interstitial_capacity
    !write(*,'('' int capA is '',f8.2)') interstitial_capacity_a
    !write(*,'('' int capB is '',f8.2)') interstitial_capacity_b
    !write(*,'('' int vol is '',f8.2)') interstitial_volume_b
    !write(*,'('' lymphSA is '',f8.2)') initial_lymphatic_surface_area
    !write(*,'('' int cap is '',f8.2)') interstitial_capacity
    
    write(*,'(8X,''Time|'',5X,''flux/s| intrstl|  a.intrstl| b.intrstl| intrstl|'',X, &
         &''init lymph|init lymph|     total|  alveolar|'')') 
    write(*,'(9X,''(s)|'',7X,''(uL)| vol(mL)|    vol(mL)|   vol(mL)|  sat(%)|'',3X, &
         &''flux(uL)|   vol(mL)|  flux(mL)|    vol(?)|'')') 

    do while(time < 32400.0_dp)
       time = time + transit_time
       !write(*,'(''code is running at '',f8.2)')time
       !write(*,'(''time variable '',f8.2)')time_variable
       !write(*,'(''breathing function '',f8.2)')breathing_function
       write(*,*) 'time=',time,transit_time,time2
       write(*,*) 'ia,b',interstitial_volume_a, interstitial_volume_b
       do while(time2 < 97.0_dp)
          interstitial_volume = interstitial_volume_a + interstitial_volume_b
          interstitial_saturation = interstitial_volume / interstitial_capacity  ! saturation as a proportion of 0-100%
          time_variable = time-transit_time + (transit_time*(time2/time_loop))
          ! calculating flux from capillary into interstitium
          interstitial_pressure_a = 1.47_dp * sin(time_variable * breathing_function) + &
               ((-3.98_dp * (interstitial_volume_a / interstitial_capacity_a)**2.0_dp) + &
               8.03_dp * (interstitial_volume_a / interstitial_capacity_a) - 6.52_dp)
          interstitial_pressure_b = 1.47_dp * sin(time_variable * breathing_function) + &
               ((-3.98_dp * (interstitial_volume_b / interstitial_capacity_b)**2.0_dp) + &
               8.03_dp * (interstitial_volume_b / interstitial_capacity_b) - 6.52_dp)
          ! pressure determined from saturation equation based of literature (currently linear, but likely not)
          
          write(*,*) time_variable,interstitial_pressure_a,interstitial_pressure_b,interstitial_volume_a, &
               interstitial_volume_b
          
          if(capillary_pressure > interstitial_pressure_a)then
             flux_a = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
                  interstitial_pressure_a)) * (transit_time/time_loop)
          else
             flux_a = 0.0_dp
          endif
          if(capillary_pressure > interstitial_pressure_b)then
             flux_b = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
                  interstitial_pressure_b)) * (transit_time / time_loop)
          else
             flux_b = 0.0_dp
          endif
          flux_c = flux_a + flux_b
          total_hydro_flux = total_hydro_flux + flux_c

          if(interstitial_volume_a + flux_a > interstitial_capacity_a)then
             excess = flux_a - (interstitial_capacity_a - interstitial_volume_a)
             interstitial_volume_a = interstitial_capacity_a
             write(*,*) 'up1:',interstitial_volume_a
             alveolar_volume = alveolar_volume + 0.5_dp*excess

             if((interstitial_volume_b + 0.5_dp*excess) > interstitial_capacity_b)then
                overflow = 0.5_dp * excess - (interstitial_capacity_b - interstitial_volume_b)
                alveolar_volume = alveolar_volume + overflow
                interstitial_volume_b = interstitial_capacity_b
             else
                interstitial_volume_b = interstitial_volume_b + 0.5*excess
             endif
          else
             interstitial_volume_a = interstitial_volume_a + flux_a
             write(*,*) 'up2:',interstitial_volume_a, flux_a
          endif
          
          interstitial_volume_b = interstitial_volume_b + flux_b
          !diffusion = (interstitial_volume_b/(interstitial_capacity-interstitial_capacity_a))*-diffusion_constant + diffusion_constant
          diffusion = (((interstitial_volume_a/interstitial_capacity_a)-(interstitial_volume_b/interstitial_capacity_b))/ &
               (160_dp*1.1_dp))*(transit_time/time_loop)
          interstitial_volume_b = interstitial_volume_b + diffusion
          interstitial_volume_a = interstitial_volume_a - diffusion
          write(*,*) 'up3:',interstitial_volume_a, diffusion
          !alveolar_volume = alveolar_volume - 4.45e-5

          ! Osmotic
          int_osm_conc = int_osm_n/interstitial_volume
          interstitial_osmotic = i*int_osm_conc*IGC*T
          osm_flux = (reflection_coefficient * capillary_SA * (capillary_osmotic - interstitial_osmotic)) &
               * (transit_time / time_loop)
          cap_osm_conc = capillary_osm_n / capillary_volume
          int_osm_conc = int_osm_n / interstitial_volume

          if(capillary_osmotic > interstitial_osmotic)then
             capillary_volume = capillary_volume - osm_flux
             interstitial_volume_b = interstitial_volume_b + osm_flux
          else
             capillary_volume = capillary_volume + osm_flux
             interstitial_volume_b = interstitial_volume_b - osm_flux
          endif
          
          net_flux = osm_flux + flux_a + flux_b
          if(net_flux > 0.0_dp)then
             osm_n_flux = net_flux * cap_osm_conc
             int_osm_n = int_osm_n + osm_n_flux
             !assuming capillary is constant and does not need updating
          else
             osm_n_flux = net_flux * int_osm_conc
             int_osm_n = int_osm_n - osm_n_flux
          endif
          
          total_osm_flux = total_osm_flux + osm_flux
          
          !calculating flux from interstitium to initial lymphatics
          if(interstitial_volume_b/interstitial_capacity_b < 0.3_dp)then
             lymphatic_conductivity = 0.9_dp * capillary_conductivity
          else
             lymphatic_conductivity = ((-1625.1_dp * (interstitial_volume_b / interstitial_capacity_b)**5_dp) + &
                  (3815.1_dp * (interstitial_volume_b / interstitial_capacity_b)**4_dp) + (-3229.5_dp * &
                  (interstitial_volume_b / interstitial_capacity_b)**3_dp) + (1258_dp * (interstitial_volume_b / &
                  interstitial_capacity_b)**2_dp) + (-213.23_dp * (interstitial_volume_b / interstitial_capacity_b)) &
                  + 11.812_dp)* 4.41335e-8_dp !(capillary_conductivity)
          endif

          initial_lymphatic_pressure = ((1.47_dp * sin(time_variable * breathing_function + pi/2_dp)) + &
               ((6.82_dp* (interstitial_volume_b / interstitial_capacity_b)**2_dp) + (0.77_dp * (interstitial_volume_b / &
               interstitial_capacity_b)) - 6.52_dp))

          if(interstitial_volume.le.0.0_dp)then
             initial_lymphatic_flow = 0.0_dp
             interstitial_volume = 0.0_dp
          elseif (interstitial_pressure_b > initial_lymphatic_pressure)then
             if((lymphatic_conductivity * initial_lymphatic_surface_area * (interstitial_pressure_b-initial_lymphatic_pressure)) &
                  * (transit_time/time_loop) > ((27.0_dp * capillary_conductivity * capillary_SA) * transit_time)) then
                initial_lymphatic_flow = ((27.0_dp * capillary_conductivity * capillary_SA) * transit_time)
             else
                initial_lymphatic_flow = (lymphatic_conductivity * initial_lymphatic_surface_area * (interstitial_pressure_b - &
                     initial_lymphatic_pressure)) * (transit_time/time_loop)
             endif
          else
             initial_lymphatic_flow = 0.0_dp
          endif

          interstitial_volume_b = interstitial_volume_b - initial_lymphatic_flow
          initial_lymphatic_volume = initial_lymphatic_volume + initial_lymphatic_flow

          int_osm_conc = int_osm_n/interstitial_volume_b
          liflowcount = liflowcount + initial_lymphatic_flow
          initial_osm_n = initial_osm_n + (initial_lymphatic_flow*int_osm_conc)
          int_osm_n = int_osm_n - (initial_lymphatic_flow*int_osm_conc)
          if (initial_lymphatic_volume > 0)then
             initial_lymph_conc = initial_osm_n/initial_lymphatic_volume
          else
             initial_lymph_conc = 0.0_dp
          endif
          
          !gas_diffusion_restriction = 0.0000152587890625_dp * exp(13.8629436112_dp*(interstitial_saturation/100.0_dp))

          time2 = time2 + 1
       !write(*,'(''capillary pressure is '',f8.2)')capillary_pressure
       !write(*,'(''interstitial pressure is '',f8.2)')interstitial_pressure_b
       enddo
       
       time2 = 1

       ! if (interstitial_pressure_b > -0.95 or interstitial_pressure_b < -8.05):
       !     raise ValueError('interstitial pressure_b outside of range: ' + str(interstitial_pressure_b))
       ! if initial_lymphatic_pressure > 1.05 or initial_lymphatic_pressure < -8.05:
       !     raise ValueError('initial lymphatic pressure outside of range: '+ str(initial_lymphatic_pressure) + '  int pressure: ' + str(interstitial_pressure_b))
       
       total_flux = total_hydro_flux ! +total_osm_flux
       sumuptake = sumuptake + initial_lymphatic_flow
       printcount = printcount + 1
       !if (printcount.eq.100)then
       if (printcount.eq.1)then
          write(*,'(f12.2, e12.3, f9.3, e12.3, f11.3, f9.3, e12.3, 2(f11.3), e12.3)') time, &
               flux_c/transit_time*1000.0_dp,interstitial_volume,interstitial_volume_a,interstitial_volume_b, &
               100.0_dp*interstitial_saturation,initial_lymphatic_flow*1000.0_dp, &
               initial_lymphatic_volume,total_flux,alveolar_volume
          write(*,*) 'at ',time, ' s:'
          write(*,*) '     flux per second is ',flux_c/transit_time*1000,' ul'
          write(*,*) '     interstitial volume is ',interstitial_volume,' mL'
          write(*,*) '        interstitial volume a is ',interstitial_volume_a,' mL'
          write(*,*) '        interstitial volume b is ',interstitial_volume_b,' mL'
          write(*,*) '     interstitial saturation is ',100*interstitial_saturation,'%'
          write(*,*) '     diffusion is ',diffusion,' mL'
          write(*,*) '     interstitial pressure is ',interstitial_pressure_b,' mmHg'
          write(*,*) '     initial lymphatic flux is ',initial_lymphatic_flow*1000,' uL'
          write(*,*) '     initial lymphatic volume is ',initial_lymphatic_volume,' mL'
          write(*,*) '     total flux is ',total_flux,' mL'
          write(*,*) '     alveolar volume is ',alveolar_volume
          printcount = 0
       endif
       read(*,*)
    enddo !while
    write(*,'(i8,f12.2, e12.3, f9.3, e12.3, f11.3, f9.3, e12.3, 2(f11.3), e12.3)') nunit,time, &
         flux_c/transit_time*1000.0_dp,interstitial_volume,interstitial_volume_a,interstitial_volume_b, &
         100.0_dp*interstitial_saturation,initial_lymphatic_flow*1000.0_dp, &
         initial_lymphatic_volume,total_flux,alveolar_volume
    
    call enter_exit(sub_name,2)
    
  end subroutine alveolar_capillary_flux
  
!!!#############################################################################

  subroutine lymphatic_transport()
    !*lymphatic_transport:* whole system transport
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_LYMPHATIC_TRANSPORT" :: LYMPHATIC_TRANSPORT

    integer :: ne
    
    character(len=60) :: sub_name
    
    ! --------------------------------------------------------------------------
    
    sub_name = 'lymphatic_transport'
    call enter_exit(sub_name,1)

    do ne = 1,num_elems
       if(elem_field(ne_group,ne).eq.1.0_dp)then!(elem_field(ne_group,ne)-1.0_dp).lt.TOLERANCE)then
          call alveolar_capillary_flux(ne)
       endif
    enddo

    call enter_exit(sub_name,2)
    
  end subroutine lymphatic_transport

!!!#############################################################################

end module lymphatics
