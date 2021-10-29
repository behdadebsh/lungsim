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
  use other_consts
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
  
  subroutine alveolar_capillary_flux(ne,write_out)
    !*alveolar_capillary_flux:* calculate fluid flux from blood to interstitium
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_ALVEOLAR_CAPILLARY_FLUX" :: ALVEOLAR_CAPILLARY_FLUX

    use other_consts,only: pi
    
    integer,intent(in) :: ne
    logical,intent(in) :: write_out

    ! Baseline value parameters (eventually will be user-defined?)
    integer,parameter :: sex = 0 ! 0 = male, 1 = female
    integer,parameter :: au = 30676 ! number of acinar units
    real(dp),parameter :: height = 175.0_dp, weight = 75.0_dp, body_mass = 74.0_dp
    
    ! Capillary parameters
    real(dp),parameter :: capillary_conductivity = 4.41335e-8_dp !  obtained from literature - Parker range in cm H2O

    ! Osmotic pressure parameters
    real(dp),parameter :: capillary_molar_conc = 0.0010250_dp ! mol/L in blood plasma
    real(dp),parameter :: IGC = 62.3630_dp ! ideal gas constant in mmHg.L.mol-1.K-1
    real(dp),parameter :: T = 310.0_dp ! temperature in Kelvin - based on 37C blood temp

    ! Simulation parameters
    real(dp),parameter :: breathing_rate = 15.0_dp

    
    ! Local variables
    integer :: i,liflowcount,nunit,n_timesteps,printcount
    real(dp) :: alveolar_volume,breathing_function,capillary_flow,capillary_vps,capillary_osmotic,capillary_osm_n, &
         capillary_volume,capillary_volume_raw,cap_osm_conc,diffusion,dt,excess,flux_a,flux_b,flux_c,gas_diffusion_restriction, &
         initial_lymphatic_flow,initial_lymphatic_pressure,initial_lymphatic_surface_area,initial_lymphatic_volume, &
         initial_lymph_conc,initial_osm_n,interstitial_capacity,interstitial_capacity_a,interstitial_capacity_b, &
         interstitial_osmotic,interstitial_pressure_a,interstitial_pressure_b,interstitial_saturation,interstitial_volume, &
         int_osm_conc,int_osm_n,interstitial_volume_a,interstitial_volume_b,lung_mass,lymphatic_conductivity,max_Pe, &
         min_Pe,net_flux,fluctuation,mx_pe,mn_pe,intPmax,intPmin,lymphPmax,lymphPmin, &
         open_capillaries,osm_flux,osm_n_flux,overflow,sumflux,sumuptake,test_time,time,time_period,time_sum, &
         time_variable,total_flux,total_hydro_flux,total_osm_flux,capillary_pressure,transit_time,capillary_SA
         
    
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
    max_Pe = unit_field(nu_Pe_max,nunit)
    min_Pe = unit_field(nu_Pe_min,nunit)

    !write(*,*) 'lp -reflcoef',lymphatic_properties%reflection_coefficient
    !write(*,*) 'ld',lymphatic_properties%lymphatic_density

    if(write_out)then
       write(*,'('' Unit'',i8,'': Pblood='',f7.2,'' mmHg; TT='',f7.2,'' s; SA='',f8.2,'' mm^2; Pe range='',f6.2,'' mmHg'')') &
            nunit,capillary_pressure,transit_time,capillary_SA,(max_Pe-min_Pe)/133.32239_dp
    endif

    ! Calculated values
    lung_mass = abs(real((1-sex)*840.0_dp))+real(sex)*639.0_dp  ! g; gives female lung weight of 639g and male of 840g
    open_capillaries = 1.0_dp/6.0_dp
    capillary_volume_raw = body_mass/0.3474_dp                  ! Gehr 213 ml and body mass of 74 kg - rough estimate?
    capillary_volume = (capillary_volume_raw*open_capillaries)/real(num_units)

    ! interstitial values
    ! interstitial_capacity == maximal volume before spillover into alveolar in mm^3 - based on 30ml.100g of fluid (Drake 2002)
    interstitial_capacity = ((30.0_dp*(lung_mass/100.0_dp))/real(num_units))*1000.0_dp 
    interstitial_capacity_a = 0.005_dp*interstitial_capacity
    interstitial_capacity_b = 0.995_dp*interstitial_capacity
    interstitial_volume_a = 0.0_dp
    interstitial_volume_b = 0.49_dp*interstitial_capacity
    alveolar_volume = 0.0_dp
    liflowcount = 0
    initial_lymphatic_surface_area = capillary_SA  

    ! initial lymphatic values
    initial_lymphatic_volume = 0.0_dp

    ! Osmotic pressures
    i = 1
    capillary_osmotic = real(i)*capillary_molar_conc*IGC*T
    capillary_osm_n = 0.0_dp
    interstitial_osmotic = 0.0_dp
    int_osm_n = 0.0_dp
    initial_osm_n = 0.0_dp
    total_osm_flux = 0.0_dp
  
    time = 0.0_dp
    printcount = 0
    total_hydro_flux = 0.0_dp

    intPmax = -1.00_dp
    intPmin = -8.00_dp
    lymphPmax = 1.00_dp
    lymphPmin = -8.00_dp
    mx_pe = max_Pe/133.32239_dp
    mn_pe = min_Pe/133.32239_dp
    fluctuation = ((mx_pe-mn_pe)/2.0_dp)

    if(write_out) write(*,'(''fluctuation '',f8.4)')fluctuation
    ! dt or n_timesteps should be controlled by the user
    n_timesteps = 96
    dt = transit_time/real(n_timesteps)
    
    breathing_function = (2.0_dp*pi)/(60.0_dp/breathing_rate)

    if(write_out) then
       write(*,'(8X,''Time|'',5X,''flux/s| intrstl|  a.intrstl| b.intrstl| intrstl|'',X, &
            &''init lymph|init lymph|     total|  alveolar|'')') 
       write(*,'(9X,''(s)|'',7X,''(uL)| vol(mL)|    vol(mL)|   vol(mL)|  sat(%)|'',3X, &
            &''flux(uL)|   vol(mL)|  flux(mL)|    vol(?)|'')')
    endif

    do while(time < lymphatic_properties%test_time)
       time_sum = dt
       do while(time_sum < transit_time)
          interstitial_volume = interstitial_volume_a + interstitial_volume_b
          interstitial_saturation = interstitial_volume / interstitial_capacity  ! saturation as a proportion of 0-100%
          time_variable = time + time_sum
          
          ! calculating flux from capillary into interstitium
          interstitial_pressure_a = fluctuation * sin(time_variable * breathing_function) + &
               (((intPmin-intPmax+(fluctuation*2.0_dp)) * (interstitial_volume_a / interstitial_capacity_a)**2.0_dp) + &
               ((intPmin-intPmax+(fluctuation*2.0_dp))*(-2.0_dp)) * (interstitial_volume_a / interstitial_capacity_a) + &
               (intPmin + fluctuation))
          interstitial_pressure_b = fluctuation * sin(time_variable * breathing_function) + &
               (((intPmin-intPmax+(fluctuation*2.0_dp)) * (interstitial_volume_b / interstitial_capacity_b)**2.0_dp) + &
               ((intPmin-intPmax+(fluctuation*2.0_dp))*(-2.0_dp)) * (interstitial_volume_b / interstitial_capacity_b) + &
               (intPmin + fluctuation))
          !write(*,'(''Pint: '',f8.4)')interstitial_pressure_b
          ! pressure determined from saturation equation based of literature (currently linear, but likely not)
          if(capillary_pressure > interstitial_pressure_a)then
             flux_a = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
                  interstitial_pressure_a)) * dt
          else
             flux_a = 0.0_dp
          endif
          if(capillary_pressure > interstitial_pressure_b)then
             flux_b = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
                  interstitial_pressure_b)) * dt
          else
             flux_b = 0.0_dp
          endif
          flux_c = flux_a + flux_b
          total_hydro_flux = total_hydro_flux + flux_c

          if(interstitial_volume_a + flux_a > interstitial_capacity_a)then
             excess = flux_a - (interstitial_capacity_a - interstitial_volume_a)
             interstitial_volume_a = interstitial_capacity_a
             alveolar_volume = alveolar_volume + 0.5_dp*excess

             if((interstitial_volume_b + 0.5_dp*excess) > interstitial_capacity_b)then
                overflow = 0.5_dp * excess - (interstitial_capacity_b - interstitial_volume_b)
                alveolar_volume = alveolar_volume + overflow
                interstitial_volume_b = interstitial_capacity_b
             else
                interstitial_volume_b = interstitial_volume_b + 0.5_dp*excess
             endif
          else
             interstitial_volume_a = interstitial_volume_a + flux_a
          endif
          
          interstitial_volume_b = interstitial_volume_b + flux_b
          diffusion = (((interstitial_volume_a/interstitial_capacity_a)-(interstitial_volume_b/interstitial_capacity_b))/ &
               (200_dp)) * dt
          interstitial_volume_b = interstitial_volume_b + diffusion
          interstitial_volume_a = interstitial_volume_a - diffusion

          ! Osmotic
          int_osm_conc = int_osm_n/interstitial_volume
          interstitial_osmotic = real(i)*int_osm_conc*IGC*T
          osm_flux = (lymphatic_properties%reflection_coefficient * capillary_SA * &
               (capillary_osmotic - interstitial_osmotic))* dt 
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
             lymphatic_conductivity = 1.48_dp * capillary_conductivity
          else
             !lymphatic_conductivity = ((-57.556_dp * (interstitial_volume_b / interstitial_capacity_b)**5.0_dp) + &
              !    (-268.41_dp * (interstitial_volume_b / interstitial_capacity_b)**4.0_dp) + (593.68_dp * &
               !   (interstitial_volume_b / interstitial_capacity_b)**3.0_dp) + (-293.78_dp * (interstitial_volume_b / &
                !  interstitial_capacity_b)**2.0_dp) + (47.955_dp * (interstitial_volume_b / interstitial_capacity_b)) &
                 ! - 0.0049_dp)* 4.41335e-8_dp !(capillary_conductivity)
             lymphatic_conductivity = ((845.87_dp * (interstitial_volume_b / interstitial_capacity_b)**5.0_dp) + &
                  (-2416.7_dp * (interstitial_volume_b / interstitial_capacity_b)**4.0_dp) + (2388.5_dp * &
                  (interstitial_volume_b / interstitial_capacity_b)**3.0_dp) + (-922.24_dp * (interstitial_volume_b / &
                  interstitial_capacity_b)**2.0_dp) + (125.85_dp * (interstitial_volume_b / interstitial_capacity_b)) &
                  - 0.0067_dp)* 4.41335e-8_dp !(capillary_conductivity)
          endif

          initial_lymphatic_pressure = fluctuation * sin((time_variable * breathing_function) + pi/2.0_dp) + &
               ((((lymphPmax-lymphPmin-(fluctuation*2.0_dp))* ((interstitial_volume_b / interstitial_capacity_b)**2.0_dp)) + &
               (lymphPmin + fluctuation)))
          !write(*,'(''Plym: '',f8.4)')initial_lymphatic_pressure
          if(interstitial_volume.le.0.0_dp)then
             initial_lymphatic_flow = 0.0_dp
             interstitial_volume = 0.0_dp
          elseif (interstitial_pressure_b > initial_lymphatic_pressure)then
             initial_lymphatic_flow = (lymphatic_conductivity * initial_lymphatic_surface_area * (interstitial_pressure_b - &
                     initial_lymphatic_pressure)) * dt
          else
             initial_lymphatic_flow = 0.0_dp
          endif

          interstitial_volume_b = interstitial_volume_b - initial_lymphatic_flow
          initial_lymphatic_volume = initial_lymphatic_volume + initial_lymphatic_flow

          int_osm_conc = int_osm_n/interstitial_volume_b
          liflowcount = liflowcount + initial_lymphatic_flow
          initial_osm_n = initial_osm_n + (initial_lymphatic_flow*int_osm_conc)
          int_osm_n = int_osm_n - (initial_lymphatic_flow*int_osm_conc)
          if (initial_lymphatic_volume > 0.0_dp)then
             initial_lymph_conc = initial_osm_n/initial_lymphatic_volume
          else
             initial_lymph_conc = 0.0_dp
          endif
          
          time_sum = time_sum + dt

          total_flux = total_hydro_flux ! +total_osm_flux
          sumuptake = sumuptake + initial_lymphatic_flow
          !printcount = printcount + 1
          !if (printcount.eq.100)then
!          write(*,'(f12.2, e12.3, f9.3, e12.3, f11.3, f9.3, e12.3, 2(f11.3), e12.3)') time_variable, &
!               flux_c/transit_time*1000.0_dp,interstitial_volume,interstitial_volume_a,interstitial_volume_b, &
!               100.0_dp*interstitial_saturation,initial_lymphatic_flow*1000.0_dp, &
!               initial_lymphatic_volume,total_flux,alveolar_volume
          !endif

       enddo
       
       time = time + transit_time

       if(write_out)then
          printcount = printcount + 1
          if (printcount.eq.100)then
             write(*,'(f12.2, e12.3, f9.3, e12.3, f11.3, f9.3, e12.3, 2(f11.3), e12.3)') time_variable, &
                  flux_c/transit_time*1000.0_dp,interstitial_volume,interstitial_volume_a,interstitial_volume_b, &
                  100.0_dp*interstitial_saturation,initial_lymphatic_flow*1000.0_dp, &
                  initial_lymphatic_volume,total_flux,alveolar_volume
             printcount = 0
          endif
       endif

    enddo !while
    
    call enter_exit(sub_name,2)
    
  end subroutine alveolar_capillary_flux
  
!!!#############################################################################

  subroutine lymphatic_transport(filename)
    !*lymphatic_transport:* whole system transport
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_LYMPHATIC_TRANSPORT" :: LYMPHATIC_TRANSPORT

    character(len=MAX_FILENAME_LEN), intent(in) :: filename
    ! Local parameters
    integer :: ne
    character(len=300) :: writefile
    character(len=60) :: sub_name
    
    ! --------------------------------------------------------------------------
    
    sub_name = 'lymphatic_transport'
    call enter_exit(sub_name,1)

    if(index(filename, ".oplymph")> 0) then !full filename is given
       writefile = filename
    else ! need to append the correct filename extension
       writefile = trim(filename)//'.oplymph'
    endif
    
    open(10, file=writefile, status='replace')
    
    do ne = 1,num_elems
       if(elem_field(ne_group,ne).eq.1.0_dp)then!(elem_field(ne_group,ne)-1.0_dp).lt.TOLERANCE)then
          call alveolar_capillary_flux(ne,.false.)
          write(10,'(i8)') ne
          write(*,*) ne
       endif
    enddo

    close(10)
    
    call enter_exit(sub_name,2)
    
  end subroutine lymphatic_transport

!!!#############################################################################

end module lymphatics
