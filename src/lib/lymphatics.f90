!module lymphatics
!  !*Brief Description:* This module contains all lymphatic-specific subroutines
!  !
!  !*LICENSE:*
!  !
!  !Test Test Test
!  !
!  !*Full Description:*
!  !
!  !This module contains code for pulmonary fluid flux within the alveolo-capillary network,
!  !and lymph transport through lymphatic collecting vessels.
!
!
!  ! Supplementary equations for the original paper: https://www.protocols.io/view/supplementary-material-an-in-silico-approach-to-un-kxygx9zmwg8j/v1
!
!  use arrays
!  use diagnostics
!  use indices
!  use other_consts
!  use precision ! sets dp for precision
!
!  implicit none
!
!  !Module parameters
!
!  ! Baseline value parameters (eventually will be user-defined?)
!  integer,protected :: sex,n_timesteps
!  !sex (0 = male, 1 = female) only determines the weight and therefore size of the lung. Should be updated based on CT
!  real(dp),protected :: lung_mass,capillary_volume_raw
!
!  ! Capillary parameters
!  real(dp),parameter :: capillary_conductivity = 4.41335e-8 !mL.s-1.mmHg-1  obtained from Parker (6e-8 cm H2O)
!  !real(dp),parameter :: capillary_conductivity = 9.26e-8 !mL.s-1.mmHg-1  obtained from Parker (6e-8 cm H2O)
!
!  real(dp),parameter :: open_capillaries = 1.0_dp/6.0_dp !based on open capillaries at rest. Should be solved for by perfusion model?
!  real(dp),protected :: interstitial_capacity
!
!  ! Osmotic pressure parameters
!  real(dp),parameter :: capillary_molar_conc = 0.0010250_dp ! this number is currenlty g.L-1, likely needs to change for osmolar to work... == YES, neeeds to be mol.L-1
!  real(dp),parameter :: IGC = 62.3630_dp ! ideal gas constant in mmHg.L.mol-1.K-1
!  real(dp),parameter :: T = 310.0_dp ! temperature in K - based on 37C blood temp
!  real(dp),protected :: capillary_osmotic,IGC_T
!
!  ! Simulation parameters
!  real(dp),protected :: breathing_rate !constant but should be imported directly from ventilation model
!  real(dp),protected :: breathing_function
!
!
!  ! whether to printout the alveolar flux results
!  logical,parameter :: write_out=.false.
!  !Module types
!
!  !Module variables
!
!  !Interfaces
!!  private
!  public alveolar_capillary_flux
!  public lymphatic_transport
!
!contains
!
!!!!#############################################################################
!
!  subroutine alveolar_capillary_flux(ne)
!    !*alveolar_capillary_flux:* calculate fluid flux from blood to interstitium
!
!    integer,intent(in) :: ne
!
!    ! Local variables
!    integer :: nunit,printcount,nq
!    real(dp) :: alveolar_volume,capillary_flow,capillary_osm_n, &
!         capillary_volume,cap_osm_conc,diffusion,dt,excess,flux_c, &
!         initial_lymph_flow,initial_lymph_pressure,initial_lymph_volume, &
!         initial_lymph_conc,initial_osm_n,interstitial_osmotic,interstitial_saturation,interstitial_volume, &
!         int_osm_conc,int_osm_n,lymph_conductivity, &
!         net_flux,fluctuation,osm_flux,osm_n_flux,overflow,sumuptake,test_time,time,time_sum, &
!         time_variable,total_flux,total_hydro_flux,total_osm_flux,capillary_pressure,transit_time,capillary_SA
!    real(dp) :: gas_diffusion_restriction,capillary_vps,time_period,int_diff,lymph_diff
!    real(dp) :: sats(5), ratio
!    real(dp) :: iv_array(2),ic_array(2),ip_array(2),flux_array(2)
!    real(dp) :: time_1,time_2
!
!    logical :: cont
!    character(len=60) :: sub_name
!
!    ! --------------------------------------------------------------------------
!    cont = .true.
!
!    sub_name = 'alveolar_capillary_flux'
!    call enter_exit(sub_name,1)
!
!    ! get information for the unit from unit_field
!    ! ne is the 'linker' element in the artery-capillary-vein model, so nunit is for the parent element
!    nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!    capillary_pressure = unit_field(nu_blood_press,nunit)/133.32239_dp !converted to mmHg from Pa
!    transit_time = unit_field(nu_tt,nunit) ! time in s
!    capillary_SA = unit_field(nu_sa,nunit) ! in mm2?? !initial lymphatic SA assumed to be the same as capillary SA as no other indication.
!    !both initial lymphatic SA and initial lymphatic hydraulic conductivity make up the filtration coefficient and currently it is the
!    !hydraulic conductivity that is adjusted to compensate
!
!!    print*, 'capillary_pressure', capillary_pressure
!    if(write_out)then
!       write(*,'('' Unit'',i8,'': Pblood='',f7.2,'' mmHg; TT='',f7.2,'' s; SA='',f8.2,'' mm^2; Pe range='',f6.2,'' mmHg'')') &
!            nunit,capillary_pressure,transit_time,capillary_SA,&
!            (unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp
!    endif
!
!    capillary_volume = (capillary_volume_raw*open_capillaries)/real(num_units)!unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !old variant: (capillary_volume_raw*open_capillaries)/real(num_units) in mL
!    !Only used for the osmotic model at the moment (which isn't operational) can volume be obtained elsewhere?
!    alveolar_volume = 0.0_dp !alveolar volume likely greater at rest, but is lost to respiration - further information needed to put in model
!    ! Is this where the tidal volume importing could go????
!
!    ! interstitial values (capacity, volume) | first index corresponds to A, second to B
!    ic_array = (/ 0.005_dp*interstitial_capacity, 0.995_dp*interstitial_capacity /) ! in mm3 ! arbitrarily sized
!    iv_array = (/ 0.0_dp, 0.48_dp*interstitial_capacity /) ! in mm3 ! assumption of 48% saturation at rest
!
!    ! initial lymphatic values
!    initial_lymph_volume = 0.0_dp ! in mL ===> dependent on capillary_conductivity volume units
!
!    ! Osmotic pressures
!    capillary_osm_n = 0.0_dp ! This doesnt change????? The bleed on effect means the rest of the osmotic flux doesnt work
!    interstitial_osmotic = 0.0_dp
!    int_osm_n = 0.0_dp
!    initial_osm_n = 0.0_dp
!    total_osm_flux = 0.0_dp
!
!    time = 0.0_dp
!    total_hydro_flux = 0.0_dp
!    printcount = 0
!
!    ! These two would presumably change in a geometrically consistent lymphatics model
!    int_diff = -8.00_dp - (-1.00_dp) ! intPmin - intPmax in mmHg
!    lymph_diff = 1.00_dp - (-8.00_dp) ! lymphPmax-lymphPmin in mmHg
!    fluctuation = 0.5_dp*((unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp) ! in mmHg
!
!    if(write_out) write(*,'(''fluctuation '',f8.4)')fluctuation
!    dt = transit_time/real(n_timesteps) ! in s
!    sats = (/ 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /) ! unitless
!
!    !outer: do while(time < lymphatic_properties%test_time), obsolete
!    outer: do while(cont)
!       time_sum = dt
!       if(transit_time.le.0.001)then
!          write(*,*) "TT= ",transit_time
!          write(*,*) "time_sum= ",time_sum
!          cont =.false.
!       endif
!       inner: do while(time_sum .lt. transit_time)
!!          print*,'iv_array(1)=',iv_array(1),'iv_array(2)',iv_array(2)
!
!          interstitial_volume = SUM(iv_array) ! volume in mm3
!!          print*,'interstitial_volume=',interstitial_volume
!!          print*,'interstitial_capacity=',interstitial_capacity
!!          print*,'interstitial_saturation=',interstitial_saturation
!          interstitial_saturation = interstitial_volume / interstitial_capacity  ! saturation as a proportion of 0-100%
!          time_variable = (time + time_sum)!*breathing_function
!
!          do nq = 1,2
!             ! calculating flux from capillary into interstitium
!             !arbitrarily defined mathematical relationship between interstitial volume and pressure: (same for a and b)
!             !interstitial pressure changes a lot at low volumes with a small volume change, but at high volumes
!             !a large volume change is needed to cause a small change in pressure
!             ratio = iv_array(nq) / ic_array(nq)
!!             print*, 'iv_array(1)', iv_array(1), ' iv_array(2)', iv_array(2)
!!             print*, 'ic_array(1)', ic_array(1), ' ic_array(2)', ic_array(2)
!             ip_array(nq) = fluctuation * sin(time_variable*breathing_function) + &
!                (((int_diff+(fluctuation*2.0_dp)) * (ratio)**2.0_dp) + &
!                ((int_diff+(fluctuation*2.0_dp))*(-2.0_dp)) * (ratio) + &
!                (-8.00_dp + fluctuation)) ! -8.00_dp = intPmin ! pressure in mmHg
!!             print*, 'ip_array(1)=', ip_array(1), ' ip_array(2)=', ip_array(2)
!             ! pressure determined from saturation equation based of literature (currently linear, but likely not)
!             if (capillary_pressure.gt.ip_array(nq)) then
!                flux_array(nq) = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
!                  ip_array(nq))) * dt ! DIMENSIONALLY INCONSISTENT??????? ===> comes out as mL.mm2: Should this balance out to mL??
!             else
!                flux_array(nq) = 0.0_dp
!             endif
!          enddo
!          flux_c = SUM(flux_array)
!          total_hydro_flux = total_hydro_flux + flux_c
!
!
!          if((iv_array(1) + flux_array(1)).gt.ic_array(1))then
!             excess = flux_array(1) - (ic_array(1) - iv_array(1))
!             iv_array(1) = ic_array(1)
!             alveolar_volume = alveolar_volume + 0.5_dp*excess
!
!             if((iv_array(2) + 0.5_dp*excess).gt.ic_array(2))then
!                overflow = 0.5_dp * excess - (ic_array(2) - iv_array(2))
!                alveolar_volume = alveolar_volume + overflow
!                iv_array(2) = ic_array(2)
!             else
!                iv_array(2) = iv_array(2) + 0.5_dp*excess
!             endif
!          else
!             iv_array(1) = iv_array(1) + flux_array(1)
!          endif
!
!          iv_array(2) = iv_array(2) + flux_array(2) ! mm3 = mm3 + mL.mm2
!
!          !!!! DIMENSIONALLY INCONSISTENT??????? ==> doesn't reduce to mm3; 200 is presumably R_alv which is highly assumptive based on parameterisation
!          diffusion = (((iv_array(1)/ic_array(1))-(iv_array(2)/ic_array(2)))/(200_dp)) * dt   !200_dp can change to alv_rai
!          iv_array(2) = iv_array(2) + diffusion
!          iv_array(1) = iv_array(1) - diffusion
!
!          ! Osmotic
!          int_osm_conc = int_osm_n/interstitial_volume ! interstitial volume has technically changed since initialising in this loop but not officially
!          interstitial_osmotic = int_osm_conc*IGC_T
!          ! Dimensionally inconsistent, can't compute capillary_osmotic - interstitial_osmotic: capillary_osmotic in wrong units
!          osm_flux = (lymphatic_properties%reflection_coefficient * capillary_SA * &
!               (capillary_osmotic - interstitial_osmotic))* dt
!!          print*,'osm_flux',osm_flux
!          cap_osm_conc = capillary_osm_n / capillary_volume ! capillary_osm_n NEVER CHANGES???????
!
!          if(capillary_osmotic .gt. interstitial_osmotic)then
!             capillary_volume = capillary_volume - osm_flux
!             iv_array(2) = iv_array(2) + osm_flux
!          else
!             capillary_volume = capillary_volume + osm_flux
!             iv_array(2) = iv_array(2) - osm_flux
!          endif
!
!          net_flux = osm_flux + SUM(flux_array)
!          if(net_flux.gt.0.0_dp)then
!             osm_n_flux = net_flux * cap_osm_conc ! cap_osm_conc is 0 ==> never updates
!             int_osm_n = int_osm_n + osm_n_flux ! This also doesn't update
!             !assuming capillary is constant and does not need updating
!          else
!             osm_n_flux = net_flux * int_osm_conc
!             int_osm_n = int_osm_n - osm_n_flux
!          endif
!
!          total_osm_flux = total_osm_flux + osm_flux
!
!          !calculating flux from interstitium to initial lymphatics
!          ratio = iv_array(2)/ic_array(2)
!!          print*, 'ic_array(2)=', ic_array(2), ' iv_array(2)=', iv_array(2)
!          if(ratio.lt.0.3_dp)then
!             lymph_conductivity = 1.48_dp * capillary_conductivity !all calculated does as a function of capillary_conductivity
!          !no information on the size of pores or similar for lympatic conductivity so assumed to be similar to capillary.
!          else
!             lymph_conductivity = ((845.87_dp * (ratio)**5.0_dp) + &
!                  (-2416.7_dp * (ratio)**4.0_dp) + (2388.5_dp * &
!                  (ratio)**3.0_dp) + (-922.24_dp * (ratio)**2.0_dp) + &
!                  (125.85_dp * (ratio)) - 0.0067_dp)*  capillary_conductivity !(capillary_conductivity)
!          endif
!
!          initial_lymph_pressure = fluctuation * sin(time_variable*breathing_function + pi/2.0_dp) + &
!               ((((lymph_diff-(fluctuation*2.0_dp))* (ratio**2.0_dp)) + &
!               (-8.00_dp + fluctuation))) !lymphPmin = -8.00_dp
!          !arbitrarily defined mathematical relationship to show that lymphatic pressure does not change much at low volumes with a
!          !large volume change, but at high volumes only a small volume change is needed to cause a large change in pressure
!          !write(*,'(''Plym: '',f8.4)')initial_lymphatic_pressure
!          initial_lymph_flow = 0.0_dp
!          if(interstitial_volume.le.0.0_dp)then
!             interstitial_volume = 0.0_dp
!          elseif (ip_array(2).gt.initial_lymph_pressure)then
!             initial_lymph_flow = (lymph_conductivity * capillary_SA * (ip_array(2) - &
!               initial_lymph_pressure)) * dt
!          endif
!          iv_array(2) = iv_array(2) - initial_lymph_flow
!          initial_lymph_volume = initial_lymph_volume + initial_lymph_flow
!
!          int_osm_conc = int_osm_n/iv_array(2)
!          initial_osm_n = initial_osm_n + (initial_lymph_flow*int_osm_conc)
!          int_osm_n = int_osm_n - (initial_lymph_flow*int_osm_conc)
!
!          if (initial_lymph_volume.gt.0.0_dp)then
!             initial_lymph_conc = initial_osm_n/initial_lymph_volume
!          else
!             initial_lymph_conc = 0.0_dp
!          endif
!
!          time_sum = time_sum + dt
!
!          total_flux = total_hydro_flux ! +total_osm_flux
!          sumuptake = sumuptake + initial_lymph_flow
!       enddo inner ! end inner transit time loop
!
!       time = time + transit_time
!       sats(5) = sats(4)
!       sats(4) = sats(3)
!       sats(3) = sats(2)
!       sats(2) = sats(1)
!       sats(1) = interstitial_saturation
!
!       printcount = 1!printcount + 1
!       if (printcount.eq.100)then
!          if(write_out)then
!             write(*,'(f12.2, e12.4, f9.4, e12.4, f11.4, f9.4, e12.4, 2(f11.4), f12.8,f12.4)') time_variable, &
!                  flux_c/transit_time*1000.0_dp,interstitial_volume,iv_array(1),iv_array(2), &
!                  100.0_dp*sats(1),initial_lymph_flow*1000.0_dp, &
!                  initial_lymph_volume,total_flux,alveolar_volume,ip_array(2)
!          endif
!          printcount = 0
!       endif
!       if((time.gt.200.0_dp*transit_time).and.(abs((SUM(sats)/5.0_dp)-sats(1)).le.0.000005_dp))then ! Removing this if statement doesn't particularly change the result
!          write(*,*) 'sats=',sats
!          cont = .false.
!       elseif(time.gt.10000.0_dp*transit_time)then
!          write(*,*) 'Times=',time,transit_time
!          cont = .false.
!       endif
!
!    enddo outer ! End outer continue loop
!
!    unit_field(nu_intsat,nunit) = interstitial_saturation
!    unit_field(nu_time,nunit) = time
!    unit_field(nu_av_flux,nunit) = total_flux/time !!! NEED THESE "/time" STATEMENTS TO REPLICATE THE RESULTS IN THE PAPER
!    unit_field(nu_lymphflow,nunit) = initial_lymph_volume/time
!
!!    write(*,'('' T='',e12.3,'': intsat='',e12.6,'' %; flux='',e12.3,'' ul/s; avFlux='',e12.3,'' ul/s; lyFlo='',e12.6,'' ul/s'')') &
!!         unit_field(nu_time,nunit),unit_field(nu_intsat,nunit),&
!!         unit_field(nu_flux,nunit),unit_field(nu_av_flux,nunit),unit_field(nu_lymphflow,nunit)
!
!    call enter_exit(sub_name,2)
!
!  end subroutine alveolar_capillary_flux
!
!!!!#############################################################################
!
!  subroutine lymphatic_transport(filename)
!    !*lymphatic_transport:* whole system transport
!    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_LYMPHATIC_TRANSPORT" :: LYMPHATIC_TRANSPORT
!
!    character(len=MAX_FILENAME_LEN), intent(in) :: filename
!    ! Local parameters
!    integer :: i,ne,ne_child,np,nunit
!    real(dp) :: time_to_run,time_0,time_1,flux_time
!    character(len=300) :: writefile
!    character(len=60) :: sub_name
!    !real(dp) :: interstitial_saturation,interstitial_pressure_b,nu_av_flux,nu_lymphflow,nu_time
!    ! --------------------------------------------------------------------------
!
!    sub_name = 'lymphatic_transport'
!    call enter_exit(sub_name,1)
!    call set_lymph_factors(1,15.0_dp,213.00_dp,96) ! mass, breathing rate, capillary volume raw (not used), number of timesteps
!
!    if(index(filename, ".oplymph")> 0) then !full filename is given
!       writefile = filename
!    else ! need to append the correct filename extension
!       writefile = trim(filename)//'.oplymph'
!    endif
!
!    open(10, file=writefile, status='replace')
!
!    call cpu_time(time_0)
!    do ne = 1,num_elems
!       if(elem_field(ne_group,ne).eq.1.0_dp)then
!          nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!          call cpu_time(time_1)
!          call alveolar_capillary_flux(ne)
!          flux_time = time_1
!          call cpu_time(time_1)
!          write(*,*) 'flux run time=',flux_time-time_1
!          np = elem_nodes(2,ne)
!          write(10,'(i8,11(e14.5))') ne,node_xyz(1:3,np),unit_field(nu_av_flux,nunit),unit_field(nu_intsat,nunit), &
!               unit_field(nu_lymphflow,nunit),unit_field(nu_blood_press,nunit),unit_field(nu_tt,nunit), &
!               unit_field(nu_sa,nunit),unit_field(nu_Pe_max,nunit),unit_field(nu_Pe_min,nunit)
!
!       endif
!    enddo
!
!    time_to_run = time_0
!    call cpu_time(time_0)
!    time_to_run = time_0- time_to_run
!    write(*,*) 'run time=',time_to_run
!    close(10)
!
!    do ne = num_elems,1,-1
!       if(elem_field(ne_group,ne).eq.1.0_dp)then
!          nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!          elem_field(ne_radius_in0,ne) = unit_field(nu_flux,nunit)
!          elem_field(ne_radius_out0,ne) = unit_field(nu_intsat,nunit)
!       else if(elem_field(ne_group,ne).eq.0.0_dp)then ! artery
!          elem_field(ne_radius_in0,ne) = 0.0_dp
!          elem_field(ne_radius_out0,ne) = 0.0_dp
!          do i = 1,elem_cnct(1,0,ne) ! each child branch
!             ne_child = elem_cnct(1,i,ne)
!             elem_field(ne_radius_in0,ne) = elem_field(ne_radius_in0,ne) + elem_field(ne_radius_in0,ne_child)
!             elem_field(ne_radius_out0,ne) = elem_field(ne_radius_out0,ne) + elem_field(ne_radius_out0,ne_child)
!          enddo
!          elem_field(ne_radius_in0,ne) = elem_field(ne_radius_in0,ne)/real(elem_cnct(1,0,ne))
!          elem_field(ne_radius_out0,ne) = elem_field(ne_radius_out0,ne)/real(elem_cnct(1,0,ne))
!       endif
!    enddo
!
!    call enter_exit(sub_name,2)
!
!  end subroutine lymphatic_transport
!
!!!!#############################################################################
!
!subroutine set_lymph_factors(mass,br,cvr,n_time)
!
!  integer,intent(in) :: mass,n_time
!  real(dp),intent(in) :: br,cvr
!
!  sex = mass
!  breathing_rate = br
!
!  ! dt or n_timesteps should be controlled by the user
!  n_timesteps = n_time
!  ! lung_mass = mass ! Replace the calculated value with this one when implementing the CT update
!  ! Calculated values
!  lung_mass = abs(real((1-sex)*840.0_dp))+real(sex)*639.0_dp  ! in g;female lung weight of 639g and male of 840g - should be updated from CT
!  breathing_function = (2.0_dp*pi)/(60.0_dp/breathing_rate)
!
!  ! interstitial_capacity == maximal volume before spillover into alveolar in mm^3 - based on 30ml.100g of fluid (Drake 2002)
!  interstitial_capacity = ((30.0_dp*(lung_mass/100.0_dp))/real(num_units))*1000.0_dp !based on lung mass which should be obtained from CT
!  IGC_T = IGC*T
!  ! this may need to be adaptable for a dynamic model
!  capillary_osmotic = capillary_molar_conc*IGC_T  ! Van't Hoffs osmotic pressure reduction - van't hoff factor, 'i' [real(1)] has been reduced to 1 to save storage
!
!  ! this value is unused
!  capillary_volume_raw = cvr  ! in mL Gehr 1978 based on having a body mass of 74 kg - should be unique to each person
!
!end subroutine set_lymph_factors
!
!
!end module lymphatics
!
!
!!FUTURE DIRECTIONS
!!input a constant to account for difference between current values and expected values
!     !Model appeared to be working within the range of the literature but is likely off by a factor of 1000 due to nl to ul conversion error.
!     !Need to check that outputted units are correct - most things are in ml and mmHg
!!Lymphatic network tree
!     !currently all lymph is returned to the circulation immediately, in reality it moves up a tree of lymphatics against a pressure gradient
!     !would require excessive modelling perhaps
!!impairment of gas diffusion caused by high interstitial saturation
!     !unclear at what level this would occur
!!alveolar flooding changes
!     !alveolar flooding should be able to move between adjacent compartments
!     !alveolar fluid should be removed via respiration naturally and therefore should always occur naturally at some low level
!!individuality needs to be added in line with the other modules
!     !currently operates only on preset male/female values

!!!!#############################################################################
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


  ! Supplementary equations for the original paper: https://www.protocols.io/view/supplementary-material-an-in-silico-approach-to-un-kxygx9zmwg8j/v1

  use arrays
  use diagnostics
  use indices
  use other_consts
  use precision ! sets dp for precision

  implicit none

  !Module parameters

  ! Baseline value parameters (eventually will be user-defined?)
  integer,protected :: sex,n_timesteps
  !sex (0 = male, 1 = female) only determines the weight and therefore size of the lung. Should be updated based on CT
  real(dp),protected :: lung_mass,capillary_volume_raw

  ! Capillary parameters
  real(dp),parameter :: capillary_conductivity = 4.41335e-8 !mL.s-1.mmHg-1  obtained from Parker (6e-8 cm H2O)
  !real(dp),parameter :: capillary_conductivity = 9.26e-8 !mL.s-1.mmHg-1  obtained from Parker (6e-8 cm H2O)

  real(dp),parameter :: open_capillaries = 1.0_dp/6.0_dp !based on open capillaries at rest. Should be solved for by perfusion model?

  real(dp),protected :: interstitial_capacity
  real(dp),protected ::    interstitial_capacity_a  !arbitrarily sized - needs further studies on the capillary-lymph interface
  real(dp),protected ::   interstitial_capacity_b  !arbitrarily sized - needs further studies on the capillary-lymph interface
!  real(dp),protected ::   interstitial_volume_a
!  real(dp),protected ::   interstitial_volume_b  !assumed to be around 48% saturated at rest
!  real(dp),protected :: iv_array(2),ic_array(2)
  real(dp),protected :: capillary_volume
  ! Osmotic pressure parameters
  real(dp),parameter :: capillary_molar_conc = 0.0010250_dp ! this number is currenlty g.L-1, likely needs to change for osmolar to work... == YES, neeeds to be mol.L-1
  real(dp),parameter :: IGC = 62.3630_dp ! ideal gas constant in mmHg.L.mol-1.K-1
  real(dp),parameter :: T = 310.0_dp ! temperature in K - based on 37C blood temp
  real(dp),protected :: capillary_osmotic,IGC_T

  ! Simulation parameters
  real(dp),protected :: breathing_rate !constant but should be imported directly from ventilation model
  real(dp),protected :: breathing_function


!  real(dp), allocatable :: interstitial_pressure(:,:)        ! shape (2, num_units)
  real(dp), allocatable ::  interstitial_pressure_a (:,:)
  real(dp), allocatable ::  interstitial_pressure_b (:,:)
  real(dp),  allocatable :: flux_a(:,:)
  real(dp),  allocatable :: flux_b(:,:)
!  real(dp), allocatable :: iv(:,:)     ! shape (2, num_units)
!  real(dp), allocatable :: ic(:,:)     ! shape (2, num_units)
  real(dp), allocatable :: time_sum(:)       ! shape (num_units)
  real(dp), allocatable :: lym_condition(:)
!  real(dp), allocatable :: total_hydro_flux(:) ! shape (num_units)
!  real(dp), allocatable :: total_osm_flux(:)  ! shape (num_units)
!  real(dp), allocatable :: initial_lymph_volume(:) ! shape (num_units)
!  real(dp), allocatable :: int_osm_n_unit(:)       ! shape (num_units)
  real(dp), allocatable :: sats(:,:)
  real(dp), allocatable :: P_initial_lymphtix(:,:)
  real(dp), allocatable :: total_hydro_flux (:,:)
  real(dp), allocatable ::  initial_lymph_flow(:,:)
  real(dp), allocatable :: initial_lymph_volume(:,:)
  real(dp), allocatable :: interstitial_volume(:,:)
  real(dp), allocatable :: interstitial_volume_a(:,:)
  real(dp), allocatable :: interstitial_volume_b(:,:)
  real(dp), allocatable :: interstitial_saturation(:,:)
!  real(dp), allocatable :: ic_array_unit(:,:)        ! shape (2, num_units)
!  real(dp), allocatable :: capillary_volume_unit(:)  ! length num_units
!  real(dp), allocatable :: int_osm_n_unit(:)         ! length num_units
!  real(dp), allocatable :: initial_lymph_volume_unit(:)
!  real(dp), allocatable :: initial_osm_n_unit(:)
!  real(dp), allocatable :: time_sum_unit(:)
!  real(dp), allocatable :: total_hydro_flux_unit(:)
!  real(dp), allocatable :: total_osm_flux_unit(:)
!  real(dp), allocatable :: sumuptake_unit(:)
!  real(dp), allocatable :: transit_time_unit(:)
!  real(dp), allocatable :: interstitial_capacity_unit(:)



  ! whether to printout the alveolar flux results
  logical,parameter :: write_out=.false.
  !Module types

  !Module variables

  !Interfaces
!  private
  public alveolar_capillary_flux
  public alveolar_flux
  public lymphatic_transport

contains

!!!#############################################################################
  subroutine alveolar_flux(dt, time, Tinsp,Pe_unit_field_pre)
    !*alveolar_capillary_flux:* calculate fluid flux from blood to interstitium

!    integer,intent(in) :: nunit
!    real(dp), dimension(:,:), intent(inout) :: unit_field
    real(dp), intent(in) :: dt,time, Tinsp
    real(dp), dimension(:,:), intent(in) :: Pe_unit_field_pre
    ! Local variables
    integer :: printcount,nq,nunit
    real(dp) :: alveolar_volume,capillary_flow,capillary_osm_n, &
         capillary_volume,cap_osm_conc,diffusion,excess,flux_c, &
         initial_lymph_pressure, &
         initial_lymph_conc,initial_osm_n,interstitial_osmotic, &
         int_osm_conc,int_osm_n,lymph_conductivity,&
         net_flux,fluctuation,osm_flux,osm_n_flux,overflow,sumuptake,test_time, &
         time_variable,total_flux,total_osm_flux,transit_time,capillary_SA !total_hydro_flux, interstitial_saturation,,interstitial_volume,initial_lymph_flow,,initial_lymph_volume, interstitial_saturation,time_sum
    real(dp) :: gas_diffusion_restriction,capillary_vps,time_period,int_diff,lymph_diff
    real(dp) ::  ratio
   ! real(dp) :: iv_array(2),ic_array(2)
    real(dp) :: lym_time!time_1,time_2
!    real(dp) :: flux_a,flux_b
    real(dp) :: capillary_pressure, P_elastic, diff_Pe !,P_elastic_pre
!    real(dp) :: flux_a,flux_b
    logical :: cont
    character(len=60) :: sub_name

    ! --------------------------------------------------------------------------
!    cont = .true.

    sub_name = 'alveolar_capillary_flux'
    call enter_exit(sub_name,1)


    do nunit = 1, num_units

     transit_time= unit_field(nu_tt, nunit)   ! assume already in seconds

!    if(transit_time.le.0.001)then
!
!    endif
!    do while(time_sum .lt. transit_time)

     capillary_pressure = unit_field(nu_blood_press,nunit)/133.32239_dp  !capillary_pressure !from Pa to mmHg

     P_elastic = (unit_field(nu_Pe,nunit))/133.32239_dp !from Pa to mmHg!-664.0_dp
     diff_Pe = ((unit_field(nu_Pe,nunit)-Pe_unit_field_pre(nu_pe,nunit))/133.32239_dp)/dt

     if (time_sum(nunit) < transit_time) then
     interstitial_volume (nu_intsat, nunit)= interstitial_volume_a (nu_intsat, nunit)+ interstitial_volume_b(nu_intsat, nunit)

     interstitial_saturation (nu_intsat, nunit)= interstitial_volume (nu_intsat, nunit)/ interstitial_capacity  ! saturation as a proportion of 0-100%

             ! calculating flux from capillary into interstitium
             !arbitrarily defined mathematical relationship between interstitial volume and pressure: (same for a and b)
             !interstitial pressure changes a lot at low volumes with a small volume change, but at high volumes
             !a large volume change is needed to cause a small change in pressure
             !remove  fluctuation
          interstitial_pressure_a (nu_Pe,nunit)= P_elastic + &
               (int_diff) * (interstitial_volume_a (nu_intsat, nunit)/ interstitial_capacity_a)**2.0_dp + &
               (int_diff)*(-2.0_dp) * (interstitial_volume_a(nu_intsat, nunit) / interstitial_capacity_a) + &
               (-8.00_dp)
          !arbitrarily defined mathematical relationship between interstitial volume and pressure: (same for a and b)
          !interstitial pressure changes a lot at low volumes with a small volume change, but at high volumes
          !a large volume change is needed to cause a small change in pressure
          interstitial_pressure_b (nu_Pe,nunit)= P_elastic + &
               (int_diff) * (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**2.0_dp + &
               (int_diff)*(-2.0_dp) * (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b) + &
               (-8.00_dp)

          if(capillary_pressure > interstitial_pressure_a(nu_Pe,nunit))then
             flux_a(nu_av_flux,nunit) = 0.5_dp * (capillary_conductivity * unit_field(nu_sa,nunit) * &
             (capillary_pressure - interstitial_pressure_a(nu_Pe,nunit))) * dt
          else
             flux_a(nu_av_flux,nunit) = 0.0_dp
          endif
          if(capillary_pressure > interstitial_pressure_b(nu_Pe,nunit))then
             flux_b(nu_av_flux,nunit) = 0.5_dp * (capillary_conductivity * unit_field(nu_sa,nunit) * &
             (capillary_pressure - interstitial_pressure_b(nu_Pe,nunit))) * dt
          else
             flux_b(nu_av_flux,  nunit) = 0.0_dp
          endif

          flux_c = flux_a(nu_av_flux,  nunit) + flux_b(nu_av_flux,  nunit)
          total_hydro_flux (nu_flux,nunit) = total_hydro_flux (nu_flux,nunit) + flux_c

          if(interstitial_volume_a(nu_intsat, nunit) + flux_a(nu_av_flux,  nunit) > interstitial_capacity_a)then
             excess = flux_a(nu_av_flux,  nunit) - (interstitial_capacity_a - interstitial_volume_a(nu_intsat, nunit))
             interstitial_volume_a(nu_intsat, nunit) = interstitial_capacity_a
             alveolar_volume = alveolar_volume + 0.5_dp*excess

             if((interstitial_volume_b(nu_intsat, nunit) + 0.5_dp*excess) > interstitial_capacity_b)then
                overflow = 0.5_dp * excess - (interstitial_capacity_b - interstitial_volume_b(nu_intsat, nunit))
                alveolar_volume = alveolar_volume + overflow
                interstitial_volume_b(nu_intsat, nunit) = interstitial_capacity_b
             else
                interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) + 0.5_dp*excess
             endif
          else
             interstitial_volume_a(nu_intsat, nunit) = interstitial_volume_a(nu_intsat, nunit) + flux_a(nu_av_flux,nunit)
          endif


          interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) + flux_b(nu_av_flux,nunit)
          !!!! DIMENSIONALLY INCONSISTENT??????? ==> doesn't reduce to mm3; 200 is presumably R_alv which is highly assumptive based on parameterisation
          !!R_alv is calculaed now

          diffusion = (((interstitial_volume_a(nu_intsat, nunit)/interstitial_capacity_a)- &
                  (interstitial_volume_b(nu_intsat, nunit)/interstitial_capacity_b))/(200_dp)) * dt
          interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) + diffusion
          interstitial_volume_a(nu_intsat, nunit) = interstitial_volume_a (nu_intsat, nunit)- diffusion

          ! Osmotic
          int_osm_conc = int_osm_n/interstitial_volume(nu_intsat, nunit) ! interstitial volume has technically changed since initialising in this loop but not officially
          interstitial_osmotic = int_osm_conc*IGC_T
          ! Dimensionally inconsistent, can't compute capillary_osmotic - interstitial_osmotic: capillary_osmotic in wrong units
          osm_flux = (lymphatic_properties%reflection_coefficient * unit_field(nu_sa,nunit)* &
               (capillary_osmotic - interstitial_osmotic))* dt
          cap_osm_conc = capillary_osm_n / capillary_volume ! capillary_osm_n NEVER CHANGES???????
          int_osm_conc = int_osm_n / interstitial_volume(nu_intsat, nunit)

!          if(capillary_osmotic .gt. interstitial_osmotic)then
!             capillary_volume = capillary_volume - osm_flux
!             iv_array(2) = iv_array(2) + osm_flux
!          else
!             capillary_volume = capillary_volume + osm_flux
!             iv_array(2) = iv_array(2) - osm_flux
!          endif
!
!          net_flux = osm_flux + flux_c
!          if(net_flux.gt.0.0_dp)then
!             osm_n_flux = net_flux * cap_osm_conc ! cap_osm_conc is 0 ==> never updates
!             int_osm_n = int_osm_n + osm_n_flux ! This also doesn't update
!             !assuming capillary is constant and does not need updating
!          else
!             osm_n_flux = net_flux * int_osm_conc
!             int_osm_n = int_osm_n - osm_n_flux
!          endif
          if(capillary_osmotic > interstitial_osmotic)then
             capillary_volume = capillary_volume - osm_flux
             interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) + osm_flux
          else
             capillary_volume = capillary_volume + osm_flux
             interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) - osm_flux
          endif

          net_flux = osm_flux + flux_a(nu_av_flux,  nunit) + flux_b(nu_av_flux,  nunit)
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
!          ratio = iv_array(2)/ic_array(2)
!          if(ratio.lt.0.3_dp)then
!             lymph_conductivity = 1.48_dp * capillary_conductivity !all calculated does as a function of capillary_conductivity
!          !no information on the size of pores or similar for lympatic conductivity so assumed to be similar to capillary.
!          else
!             lymph_conductivity = ((845.87_dp * (ratio)**5.0_dp) + &
!                  (-2416.7_dp * (ratio)**4.0_dp) + (2388.5_dp * &
!                  (ratio)**3.0_dp) + (-922.24_dp * (ratio)**2.0_dp) + &
!                  (125.85_dp * (ratio)) - 0.0067_dp)*  capillary_conductivity !(capillary_conductivity)
!          endif
          if(interstitial_volume_b(nu_intsat, nunit)/interstitial_capacity_b < 0.3_dp)then
             lymph_conductivity = 1.48_dp * 4.41335e-8 !all calculated does as a function of capillary_conductivity
          !no information on the size of pores or similar for lympatic conductivity so assumed to be similar to capillary.
          else

             lymph_conductivity = ((845.87_dp * (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**5.0_dp) + &
                  (-2416.7_dp * (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**4.0_dp) + (2388.5_dp * &
                  (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**3.0_dp) + (-922.24_dp * &
                     (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**2.0_dp) + &
                     (125.85_dp * (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)) - 0.0067_dp)* 4.41335e-8 !(capillary_conductivity)
          endif

          P_initial_lymphtix (nu_Pe,nunit) = (diff_Pe /(2.0_dp * pi * 0.25_dp))+ &
               (lymph_diff)* (interstitial_volume_b(nu_intsat, nunit) / interstitial_capacity_b)**2.0_dp + &
               (-8.00_dp)
          !arbitrarily defined mathematical relationship to show that lymphatic pressure does not change much at low volumes with a
          !large volume change, but at high volumes only a small volume change is needed to cause a large change in pressure
          !write(*,'(''Plym: '',f8.4)')initial_lymphatic_pressure


          if(interstitial_volume(nu_intsat, nunit).le.0.0_dp)then
             initial_lymph_flow(nu_lymphflow,nunit) = 0.0_dp
             interstitial_volume(nu_intsat, nunit) = 0.0_dp
          elseif (interstitial_pressure_b (nu_Pe,nunit)>  P_initial_lymphtix (nu_Pe,nunit))then
             initial_lymph_flow(nu_lymphflow,nunit) = (lymph_conductivity * unit_field(nu_sa,nunit) * &
                     (interstitial_pressure_b(nu_Pe,nunit) - P_initial_lymphtix (nu_Pe,nunit))) * dt
          else
             initial_lymph_flow(nu_lymphflow,nunit) = 0.0_dp
          endif
!          iv_array(2) = iv_array(2) - initial_lymph_flow
!          initial_lymph_volume = initial_lymph_volume + initial_lymph_flow
          interstitial_volume_b(nu_intsat, nunit) = interstitial_volume_b(nu_intsat, nunit) &
                  - initial_lymph_flow(nu_lymphflow,nunit)
          initial_lymph_volume(nu_lymphflow,nunit) = initial_lymph_volume(nu_lymphflow,nunit) &
                  + initial_lymph_flow(nu_lymphflow,nunit)


          int_osm_conc = int_osm_n/interstitial_volume_b(nu_intsat, nunit)
!          liflowcount = liflowcount + initial_lymphatic_flow
          initial_osm_n = initial_osm_n + (initial_lymph_flow(nu_lymphflow,nunit)*int_osm_conc)
          int_osm_n = int_osm_n - (initial_lymph_flow(nu_lymphflow,nunit)*int_osm_conc)
          if (initial_lymph_volume(nu_lymphflow,nunit) > 0.0_dp)then
             initial_lymph_conc = initial_osm_n/initial_lymph_volume(nu_lymphflow,nunit)
          else
             initial_lymph_conc = 0.0_dp
          endif


          total_flux = total_hydro_flux(nu_flux,nunit) ! +total_osm_flux
          sumuptake = sumuptake + initial_lymph_flow(nu_lymphflow,nunit)
          time_sum(nunit) = time_sum(nunit) + dt
!    end if !inner ! end inner transit time loop
     else
       sats(5,nunit) = sats(4,nunit)
       sats(4,nunit) = sats(3,nunit)
       sats(3,nunit) = sats(2,nunit)
       sats(2,nunit) = sats(1,nunit)
       sats(1,nunit) = interstitial_saturation(nu_intsat, nunit)



       lym_condition(nunit) = abs(sum(sats(:,nunit)) / 5.0_dp - sats(1,nunit))

       lym_time=lym_time+ time_sum(nunit)
!      if (time_sum >= transit_time) then
        ! write out exactly as the original did:
        unit_field(nu_intsat,   nunit) = interstitial_saturation(nu_intsat, nunit)
        unit_field(nu_time,     nunit) = lym_time ! why time here is global time not the transit time
        unit_field(nu_av_flux,  nunit) = total_flux !flux_c
        unit_field(nu_lymphflow,nunit) = initial_lymph_volume(nu_lymphflow,nunit)

!        ! now reset state so the next cycle starts fresh:
!        iv_array_unit(:,nunit)        = (/ 0.0_dp, 0.48_dp*interstitial_capacity /)
        time_sum(nunit) = 0.0_dp
!        total_hydro_flux_unit(nunit) = 0.0_dp
!        total_osm_flux_unit(nunit)   = 0.0_dp
!        initial_lymph_volume_unit(nunit) = 0.0_dp
!        int_osm_n_unit(nunit)        = 0.0_dp
!      else
        ! still mid‐transit ⇒ just write back updated state
!        iv_array_unit(1,i_unit)        = iv1
!        iv_array_unit(2,i_unit)        = iv2
!        total_hydro_flux_unit(i_unit)  = total_hydro_flux
!        total_osm_flux_unit(i_unit)    = total_osm_flux
!        initial_lymph_volume_unit(i_unit) = initial_lymph_volume
!        int_osm_n_unit(i_unit)         = int_osm_n
      endif

!
!        unit_field(nu_intsat,nunit) = interstitial_saturation
!        unit_field(nu_time,nunit) = time
!        unit_field(nu_av_flux,nunit) = total_flux/transit_time !!! NEED THESE "/time" STATEMENTS TO REPLICATE THE RESULTS IN THE PAPER
!        unit_field(nu_lymphflow,nunit) = initial_lymph_volume

!    print*,'int', alveolar_volume
!    !write(*,'('' T='',e12.3,'': intsat='',e12.6,'' %; flux='',e12.3,'' ul/s; avFlux='',e12.3,'' ul/s; lyFlo='',e12.6,'' ul/s'')') &
!    !     unit_field(nu_time,nunit),unit_field(nu_intsat,nunit),&
!    !     unit_field(nu_flux,nunit),unit_field(nu_av_flux,nunit),unit_field(nu_lymphflow,nunit)
      enddo
!            print*,'ratio:', interstitial_pressure(1,1)
!        print*,'bp', unit_field(nu_blood_press,1)!    unit_field(nu_intsat,nunit) = interstitial_saturation
!    print*,'bp',  lymphatic_properties%reflection_coefficient
!    print*,'int',  unit_field(nu_intsat,1)
!    print*,'int',   unit_field(nu_time,1)
!    print*,'int', unit_field(nu_av_flux,1)
!    print*,'int',  unit_field(nu_lymphflow,1)

    call enter_exit(sub_name,2)

  end subroutine alveolar_flux
!!!#############################################################################
  subroutine alveolar_capillary_flux(nunit)
    !*alveolar_capillary_flux:* calculate fluid flux from blood to interstitium

    integer,intent(in) :: nunit

    ! Local variables
    integer :: printcount,nq
    real(dp) :: alveolar_volume,capillary_flow,capillary_osm_n, &
         capillary_volume,cap_osm_conc,diffusion,dt,excess,flux_c, &
         initial_lymph_flow,initial_lymph_pressure,initial_lymph_volume, &
         initial_lymph_conc,initial_osm_n,interstitial_osmotic,interstitial_saturation,interstitial_volume, &
         int_osm_conc,int_osm_n,lymph_conductivity, &
         net_flux,fluctuation,osm_flux,osm_n_flux,overflow,sumuptake,test_time,time,time_sum, &
         time_variable,total_flux,total_hydro_flux,total_osm_flux,capillary_pressure,transit_time,capillary_SA
    real(dp) :: gas_diffusion_restriction,capillary_vps,time_period,int_diff,lymph_diff
    real(dp) :: sats(5), ratio
    real(dp) :: ip_array(2),flux_array(2)
    real(dp) :: time_1,time_2
    real(dp) :: iv_array(2),ic_array(2)
    logical :: cont
    character(len=60) :: sub_name

    ! --------------------------------------------------------------------------
    cont = .true.

    sub_name = 'alveolar_capillary_flux'
    call enter_exit(sub_name,1)

!    ! get information for the unit from unit_field
!    ! ne is the 'linker' element in the artery-capillary-vein model, so nunit is for the parent element
!    nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!    capillary_pressure = unit_field(nu_blood_press,nunit)/133.32239_dp !converted to mmHg from Pa
!    transit_time = unit_field(nu_tt,nunit) ! time in s
!    capillary_SA = unit_field(nu_sa,nunit) ! in mm2?? !initial lymphatic SA assumed to be the same as capillary SA as no other indication.
!    !both initial lymphatic SA and initial lymphatic hydraulic conductivity make up the filtration coefficient and currently it is the
!    !hydraulic conductivity that is adjusted to compensate
!
!
!    if(write_out)then
!       write(*,'('' Unit'',i8,'': Pblood='',f7.2,'' mmHg; TT='',f7.2,'' s; SA='',f8.2,'' mm^2; Pe range='',f6.2,'' mmHg'')') &
!            nunit,capillary_pressure,transit_time,capillary_SA,&
!            (unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp
!    endif
!
!    capillary_volume = unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !old variant: (capillary_volume_raw*open_capillaries)/real(num_units) in mL
!    !Only used for the osmotic model at the moment (which isn't operational) can volume be obtained elsewhere?
!    alveolar_volume = 0.0_dp !alveolar volume likely greater at rest, but is lost to respiration - further information needed to put in model
!    ! Is this where the tidal volume importing could go????
!
!    ! interstitial values (capacity, volume) | first index corresponds to A, second to B
!    ic_array = (/ 0.005_dp*interstitial_capacity, 0.995_dp*interstitial_capacity /) ! in mm3 ! arbitrarily sized
!    iv_array = (/ 0.0_dp, 0.48_dp*interstitial_capacity /) ! in mm3 ! assumption of 48% saturation at rest
!
!    ! initial lymphatic values
!    initial_lymph_volume = 0.0_dp ! in mL ===> dependent on capillary_conductivity volume units
!
!    ! Osmotic pressures
!    capillary_osm_n = 0.0_dp ! This doesnt change????? The bleed on effect means the rest of the osmotic flux doesnt work
!    interstitial_osmotic = 0.0_dp
!    int_osm_n = 0.0_dp
!    initial_osm_n = 0.0_dp
!    total_osm_flux = 0.0_dp
!
!    time = 0.0_dp
!    total_hydro_flux = 0.0_dp
!    printcount = 0
!
!    ! These two would presumably change in a geometrically consistent lymphatics model
!    int_diff = -8.00_dp - (-1.00_dp) ! intPmin - intPmax in mmHg
!    lymph_diff = 1.00_dp - (-8.00_dp) ! lymphPmax-lymphPmin in mmHg
!    fluctuation = 0.5_dp*((unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp) ! in mmHg
!
!    if(write_out) write(*,'(''fluctuation '',f8.4)')fluctuation
!    dt = transit_time/real(n_timesteps) ! in s
!    sats = (/ 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /) ! unitless

!    !outer: do while(time < lymphatic_properties%test_time), obsolete
!    outer: do while(cont)
!        ! get information for the unit from unit_field
!        ! ne is the 'linker' element in the artery-capillary-vein model, so nunit is for the parent element
!!        nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!        capillary_pressure = unit_field(nu_blood_press,nunit)/133.32239_dp !converted to mmHg from Pa
!        transit_time = unit_field(nu_tt,nunit) ! time in s
!        capillary_SA = unit_field(nu_sa,nunit) ! in mm2?? !initial lymphatic SA assumed to be the same as capillary SA as no other indication.
!        !both initial lymphatic SA and initial lymphatic hydraulic conductivity make up the filtration coefficient and currently it is the
!        !hydraulic conductivity that is adjusted to compensate
!        if(write_out)then
!            write(*,'('' Unit'',i8,'': Pblood='',f7.2,'' mmHg; TT='',f7.2,'' s; SA='',f8.2,'' mm^2; Pe range='',f6.2,'' mmHg'')') &
!            nunit,capillary_pressure,transit_time,capillary_SA,&
!            (unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp
!        endif
!       capillary_volume = unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !old variant: (capillary_volume_raw*open_capillaries)/real(num_units) in mL
!       ! interstitial values (capacity, volume) | first index corresponds to A, second to B
!       ic_array = (/ 0.005_dp*interstitial_capacity, 0.995_dp*interstitial_capacity /) ! in mm3 ! arbitrarily sized
!       iv_array = (/ 0.0_dp, 0.48_dp*interstitial_capacity /) ! in mm3 ! assumption of 48% saturation at rest
!       fluctuation = 0.5_dp*((unit_field(nu_Pe_max,nunit)-unit_field(nu_Pe_min,nunit))/133.32239_dp) ! in mmHg
!
!!       if(write_out) write(*,'(''fluctuation '',f8.4)')fluctuation
!       dt = transit_time/real(n_timesteps) ! in s
!!       sats = (/ 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /) ! unitless
!
!       time_sum = dt
!       if(transit_time.le.0.001)then
!          write(*,*) "TT= ",transit_time
!          write(*,*) "time_sum= ",time_sum
!          cont =.false.
!       endif
!       inner: do while(time_sum .lt. transit_time)
!          interstitial_volume = SUM(iv_array) ! volume in mm3
!          interstitial_saturation = interstitial_volume / interstitial_capacity  ! saturation as a proportion of 0-100%
!          time_variable = (time + time_sum)!*breathing_function
!
!          do nq = 1,2
!             ! calculating flux from capillary into interstitium
!             !arbitrarily defined mathematical relationship between interstitial volume and pressure: (same for a and b)
!             !interstitial pressure changes a lot at low volumes with a small volume change, but at high volumes
!             !a large volume change is needed to cause a small change in pressure
!             ratio = iv_array(nq) / ic_array(nq)
!             ip_array(nq) = fluctuation * sin(time_variable*breathing_function) + &
!                (((int_diff+(fluctuation*2.0_dp)) * (ratio)**2.0_dp) + &
!                ((int_diff+(fluctuation*2.0_dp))*(-2.0_dp)) * (ratio) + &
!                (-8.00_dp + fluctuation)) ! -8.00_dp = intPmin ! pressure in mmHg
!             ! pressure determined from saturation equation based of literature (currently linear, but likely not)
!             if (capillary_pressure.gt.ip_array(nq)) then
!                flux_array(nq) = 0.5_dp * (capillary_conductivity * capillary_SA * (capillary_pressure - &
!                  ip_array(nq))) * dt ! DIMENSIONALLY INCONSISTENT??????? ===> comes out as mL.mm2: Should this balance out to mL??
!             else
!                flux_array(nq) = 0.0_dp
!             endif
!          enddo
!          flux_c = SUM(flux_array)
!          total_hydro_flux = total_hydro_flux + flux_c
!
!
!          if((iv_array(1) + flux_array(1)).gt.ic_array(1))then
!             excess = flux_array(1) - (ic_array(1) - iv_array(1))
!             iv_array(1) = ic_array(1)
!             alveolar_volume = alveolar_volume + 0.5_dp*excess
!
!             if((iv_array(2) + 0.5_dp*excess).gt.ic_array(2))then
!                overflow = 0.5_dp * excess - (ic_array(2) - iv_array(2))
!                alveolar_volume = alveolar_volume + overflow
!                iv_array(2) = ic_array(2)
!             else
!                iv_array(2) = iv_array(2) + 0.5_dp*excess
!             endif
!          else
!             iv_array(1) = iv_array(1) + flux_array(1)
!          endif
!
!          iv_array(2) = iv_array(2) + flux_array(2) ! mm3 = mm3 + mL.mm2
!
!          !!!! DIMENSIONALLY INCONSISTENT??????? ==> doesn't reduce to mm3; 200 is presumably R_alv which is highly assumptive based on parameterisation
!          diffusion = (((iv_array(1)/ic_array(1))-(iv_array(2)/ic_array(2)))/(200_dp)) * dt
!          iv_array(2) = iv_array(2) + diffusion
!          iv_array(1) = iv_array(1) - diffusion
!
!          ! Osmotic
!          int_osm_conc = int_osm_n/interstitial_volume ! interstitial volume has technically changed since initialising in this loop but not officially
!          interstitial_osmotic = int_osm_conc*IGC_T
!          ! Dimensionally inconsistent, can't compute capillary_osmotic - interstitial_osmotic: capillary_osmotic in wrong units
!          osm_flux = (lymphatic_properties%reflection_coefficient * capillary_SA * &
!               (capillary_osmotic - interstitial_osmotic))* dt
!          cap_osm_conc = capillary_osm_n / capillary_volume ! capillary_osm_n NEVER CHANGES???????
!
!          if(capillary_osmotic .gt. interstitial_osmotic)then
!             capillary_volume = capillary_volume - osm_flux
!             iv_array(2) = iv_array(2) + osm_flux
!          else
!             capillary_volume = capillary_volume + osm_flux
!             iv_array(2) = iv_array(2) - osm_flux
!          endif
!
!          net_flux = osm_flux + SUM(flux_array)
!          if(net_flux.gt.0.0_dp)then
!             osm_n_flux = net_flux * cap_osm_conc ! cap_osm_conc is 0 ==> never updates
!             int_osm_n = int_osm_n + osm_n_flux ! This also doesn't update
!             !assuming capillary is constant and does not need updating
!          else
!             osm_n_flux = net_flux * int_osm_conc
!             int_osm_n = int_osm_n - osm_n_flux
!          endif
!
!          total_osm_flux = total_osm_flux + osm_flux
!
!          !calculating flux from interstitium to initial lymphatics
!          ratio = iv_array(2)/ic_array(2)
!          if(ratio.lt.0.3_dp)then
!             lymph_conductivity = 1.48_dp * capillary_conductivity !all calculated does as a function of capillary_conductivity
!          !no information on the size of pores or similar for lympatic conductivity so assumed to be similar to capillary.
!          else
!             lymph_conductivity = ((845.87_dp * (ratio)**5.0_dp) + &
!                  (-2416.7_dp * (ratio)**4.0_dp) + (2388.5_dp * &
!                  (ratio)**3.0_dp) + (-922.24_dp * (ratio)**2.0_dp) + &
!                  (125.85_dp * (ratio)) - 0.0067_dp)*  capillary_conductivity !(capillary_conductivity)
!          endif
!
!          initial_lymph_pressure = fluctuation * sin(time_variable*breathing_function + pi/2.0_dp) + &
!               ((((lymph_diff-(fluctuation*2.0_dp))* (ratio**2.0_dp)) + &
!               (-8.00_dp + fluctuation))) !lymphPmin = -8.00_dp
!          !arbitrarily defined mathematical relationship to show that lymphatic pressure does not change much at low volumes with a
!          !large volume change, but at high volumes only a small volume change is needed to cause a large change in pressure
!          !write(*,'(''Plym: '',f8.4)')initial_lymphatic_pressure
!          initial_lymph_flow = 0.0_dp
!          if(interstitial_volume.le.0.0_dp)then
!             interstitial_volume = 0.0_dp
!          elseif (ip_array(2).gt.initial_lymph_pressure)then
!             initial_lymph_flow = (lymph_conductivity * capillary_SA * (ip_array(2) - &
!               initial_lymph_pressure)) * dt
!          endif
!          iv_array(2) = iv_array(2) - initial_lymph_flow
!          initial_lymph_volume = initial_lymph_volume + initial_lymph_flow
!
!          int_osm_conc = int_osm_n/iv_array(2)
!          initial_osm_n = initial_osm_n + (initial_lymph_flow*int_osm_conc)
!          int_osm_n = int_osm_n - (initial_lymph_flow*int_osm_conc)
!
!          if (initial_lymph_volume.gt.0.0_dp)then
!             initial_lymph_conc = initial_osm_n/initial_lymph_volume
!          else
!             initial_lymph_conc = 0.0_dp
!          endif
!
!          time_sum = time_sum + dt
!
!          total_flux = total_hydro_flux ! +total_osm_flux
!          sumuptake = sumuptake + initial_lymph_flow
!       enddo inner ! end inner transit time loop
!
!!       time = time + transit_time
!       sats(5) = sats(4)
!       sats(4) = sats(3)
!       sats(3) = sats(2)
!       sats(2) = sats(1)
!       sats(1) = interstitial_saturation
!
!       printcount = 1!printcount + 1
!       if (printcount.eq.100)then
!          if(write_out)then
!             write(*,'(f12.2, e12.4, f9.4, e12.4, f11.4, f9.4, e12.4, 2(f11.4), f12.8,f12.4)') time_variable, &
!                  flux_c/transit_time*1000.0_dp,interstitial_volume,iv_array(1),iv_array(2), &
!                  100.0_dp*sats(1),initial_lymph_flow*1000.0_dp, &
!                  initial_lymph_volume,total_flux,alveolar_volume,ip_array(2)
!          endif
!          printcount = 0
!       endif
!       if((time.gt.200.0_dp*transit_time).and.(abs((SUM(sats)/5.0_dp)-sats(1)).le.0.000005_dp))then ! Removing this if statement doesn't particularly change the result
!          write(*,*) 'sats=',sats
!          cont = .false.
!       elseif(time.gt.10000.0_dp*transit_time)then
!          write(*,*) 'Times=',time,transit_time
!          cont = .false.
!       endif
!
!    enddo outer ! End outer continue loop
!
!    unit_field(nu_intsat,nunit) = interstitial_saturation
!    unit_field(nu_time,nunit) = time
!    unit_field(nu_av_flux,nunit) = total_flux/time !!! NEED THESE "/time" STATEMENTS TO REPLICATE THE RESULTS IN THE PAPER
!    unit_field(nu_lymphflow,nunit) = initial_lymph_volume/time
    !write(*,'('' T='',e12.3,'': intsat='',e12.6,'' %; flux='',e12.3,'' ul/s; avFlux='',e12.3,'' ul/s; lyFlo='',e12.6,'' ul/s'')') &
    !     unit_field(nu_time,nunit),unit_field(nu_intsat,nunit),&
    !     unit_field(nu_flux,nunit),unit_field(nu_av_flux,nunit),unit_field(nu_lymphflow,nunit)

    call enter_exit(sub_name,2)

  end subroutine alveolar_capillary_flux

!!!#############################################################################

  subroutine lymphatic_transport(filename)
    !*lymphatic_transport:* whole system transport
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_LYMPHATIC_TRANSPORT" :: LYMPHATIC_TRANSPORT

    character(len=MAX_FILENAME_LEN), intent(in) :: filename
    real(dp) :: alveolar_volume,capillary_flow,capillary_osm_n, &
         capillary_volume,cap_osm_conc,diffusion,dt,excess,flux_c, &
         initial_lymph_flow,initial_lymph_pressure,initial_lymph_volume, &
         initial_lymph_conc,initial_osm_n,interstitial_osmotic,interstitial_saturation,interstitial_volume, &
         int_osm_conc,int_osm_n,lymph_conductivity, &
         net_flux,fluctuation,osm_flux,osm_n_flux,overflow,sumuptake,test_time,time_sum, &
         time_variable,total_flux,total_hydro_flux,total_osm_flux,capillary_pressure,transit_time,capillary_SA
    real(dp) :: gas_diffusion_restriction,capillary_vps,time_period,int_diff,lymph_diff,sats(5)
    real(dp) :: lym_time

    ! Local parameters
    integer :: i,ne,ne_child,np,nunit,printcount
    real(dp) :: time_to_run,time_0,time_1,flux_time
    character(len=300) :: writefile
    character(len=60) :: sub_name
    !real(dp) :: interstitial_saturation,interstitial_pressure_b,nu_av_flux,nu_lymphflow,nu_time
    ! --------------------------------------------------------------------------

    sub_name = 'lymphatic_transport'
    call enter_exit(sub_name,1)
    call set_lymph_factors(1,213.00_dp) ! mass, breathing rate, capillary volume raw (not used), number of timesteps

!  sex = 1
!!  breathing_rate = br
!
!  ! dt or n_timesteps should be controlled by the user
!!  n_timesteps = n_time
!  ! lung_mass = mass ! Replace the calculated value with this one when implementing the CT update
!  ! Calculated values
!  lung_mass = abs(real((1-sex)*840.0_dp))+real(sex)*639.0_dp  ! in g;female lung weight of 639g and male of 840g - should be updated from CT
!!  breathing_function = (2.0_dp*pi)/(60.0_dp/breathing_rate)
!
!  ! interstitial_capacity == maximal volume before spillover into alveolar in mm^3 - based on 30ml.100g of fluid (Drake 2002)
!  interstitial_capacity = ((30.0_dp*(lung_mass/100.0_dp))/real(num_units))*1000.0_dp !based on lung mass which should be obtained from CT
!  IGC_T = IGC*T
!  ! this may need to be adaptable for a dynamic model
!  capillary_osmotic = capillary_molar_conc*IGC_T  ! Van't Hoffs osmotic pressure reduction - van't hoff factor, 'i' [real(1)] has been reduced to 1 to save storage
!
!  ! this value is unused
!  capillary_volume_raw = 213.00_dp  ! in mL Gehr 1978 based on having a body mass of 74 kg - should be unique to each person
!  capillary_volume = (capillary_volume_raw*open_capillaries)/real(num_units) !in mL  Ben: unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !  unit_field(nu_vol,nunit) from venti not works

    if(index(filename, ".oplymph")> 0) then !full filename is given
       writefile = filename
    else ! need to append the correct filename extension
       writefile = trim(filename)//'.oplymph'
    endif

    open(10, file=writefile, status='replace')
    !Only used for the osmotic model at the moment (which isn't operational) can volume be obtained elsewhere?
    alveolar_volume = 0.0_dp !alveolar volume likely greater at rest, but is lost to respiration - further information needed to put in model
    ! Is this where the tidal volume importing could go????



    ! initial lymphatic values
    initial_lymph_volume = 0.0_dp ! in mL ===> dependent on capillary_conductivity volume units

    ! Osmotic pressures
    capillary_osm_n = 0.0_dp ! This doesnt change????? The bleed on effect means the rest of the osmotic flux doesnt work 1.025_dp / 66.5!
    interstitial_osmotic = 0.0_dp
    int_osm_n = 0.0_dp
    initial_osm_n = 0.0_dp
    total_osm_flux = 0.0_dp

    lym_time = 0.0_dp
    time_sum = 0.0_dp
    total_hydro_flux = 0.0_dp
    printcount = 0

    ! These two would presumably change in a geometrically consistent lymphatics model
    int_diff = -8.00_dp - (-1.00_dp) ! intPmin - intPmax in mmHg
    lymph_diff = 1.00_dp - (-8.00_dp) ! lymphPmax-lymphPmin in mmHg

!    sat1 = 1.0_dp
!    sat2 = 2.0_dp
!    sat3 = 3.0_dp
!    sat4 = 4.0_dp
!    sat5 = 5.0_dp
    sats = (/ 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /) ! unitless
!    interstitial_capacity_a = 0.005_dp*interstitial_capacity !arbitrarily sized - needs further studies on the capillary-lymph interface
!    interstitial_capacity_b = 0.995_dp*interstitial_capacity !arbitrarily sized - needs further studies on the capillary-lymph interface
!    interstitial_volume_a = 0.0_dp
!    interstitial_volume_b = 0.48_dp*interstitial_capacity !assumed to be around 48% saturated at rest
!       capillary_volume = (capillary_volume_raw*open_capillaries)/real(num_units) !in mL  Ben: unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !  unit_field(nu_vol,nunit) from venti not works
!!       ! interstitial values (capacity, volume) | first index corresponds to A, second to B
!       ic_array = (/ 0.005_dp*interstitial_capacity, 0.995_dp*interstitial_capacity /) ! in mm3 ! arbitrarily sized
!       iv_array = (/ 0.0_dp, 0.48_dp*interstitial_capacity /) ! in mm3 ! assumption of 48% saturation at rest

    !outer: do while(time < lymphatic_properties%test_time), obsolete
!    outer: do while(cont)
!    call cpu_time(time_0)
!    do nunit = 1,num_units
!!    do ne = 1,num_elems
!!       if(elem_field(ne_group,ne).eq.1.0_dp)then
!!          nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!!          call cpu_time(time_1)
!          call alveolar_capillary_flux(nunit)
!!          flux_time = time_1
!!          call cpu_time(time_1)
!!          write(*,*) 'flux run time=',flux_time-time_1
!!          np = elem_nodes(2,ne)
!!          write(10,'(i8,11(e14.5))') ne,node_xyz(1:3,np),unit_field(nu_av_flux,nunit),unit_field(nu_intsat,nunit), &
!!               unit_field(nu_lymphflow,nunit),unit_field(nu_blood_press,nunit),unit_field(nu_tt,nunit), &
!!               unit_field(nu_sa,nunit),unit_field(nu_Pe_max,nunit),unit_field(nu_Pe_min,nunit)
!
!!       endif
!    enddo
!!    time_to_run = time_0
!!    call cpu_time(time_0)
!!    time_to_run = time_0- time_to_run
!    write(*,*) 'run time=',time_to_run
!    enddo outer ! End outer continue loop
    close(10)
!
!not put back to perfusion model yet,block first
!    do ne = num_elems,1,-1
!       if(elem_field(ne_group,ne).eq.1.0_dp)then
!          nunit = int(elem_field(ne_unit,elem_cnct(-1,1,ne)))
!          elem_field(ne_radius_in0,ne) = unit_field(nu_flux,nunit)
!          elem_field(ne_radius_out0,ne) = unit_field(nu_intsat,nunit)
!       else if(elem_field(ne_group,ne).eq.0.0_dp)then ! artery
!          elem_field(ne_radius_in0,ne) = 0.0_dp
!          elem_field(ne_radius_out0,ne) = 0.0_dp
!          do i = 1,elem_cnct(1,0,ne) ! each child branch
!             ne_child = elem_cnct(1,i,ne)
!             elem_field(ne_radius_in0,ne) = elem_field(ne_radius_in0,ne) + elem_field(ne_radius_in0,ne_child)
!             elem_field(ne_radius_out0,ne) = elem_field(ne_radius_out0,ne) + elem_field(ne_radius_out0,ne_child)
!          enddo
!          elem_field(ne_radius_in0,ne) = elem_field(ne_radius_in0,ne)/real(elem_cnct(1,0,ne))
!          elem_field(ne_radius_out0,ne) = elem_field(ne_radius_out0,ne)/real(elem_cnct(1,0,ne))
!       endif
!    enddo

    call enter_exit(sub_name,2)

  end subroutine lymphatic_transport

!!!#############################################################################

subroutine set_lymph_factors(mass,cvr)

  integer,intent(in) :: mass
  real(dp),intent(in) :: cvr

  sex = mass
!  breathing_rate = br

  ! dt or n_timesteps should be controlled by the user
!  n_timesteps = n_time

  ! lung_mass = mass ! Replace the calculated value with this one when implementing the CT update
  ! Calculated values
  lung_mass = abs(real((1-sex)*840.0_dp))+real(sex)*639.0_dp  ! in g;female lung weight of 639g and male of 840g - should be updated from CT
  breathing_function = (2.0_dp*pi)/(60.0_dp/breathing_rate)

  ! interstitial_capacity == maximal volume before spillover into alveolar in mm^3 - based on 30ml.100g of fluid (Drake 2002)
  interstitial_capacity = ((30.0_dp*(lung_mass/100.0_dp))/real(num_units))*1000.0_dp !based on lung mass which should be obtained from CT
  IGC_T = IGC*T
  ! this may need to be adaptable for a dynamic model
  capillary_osmotic = capillary_molar_conc*IGC_T  ! Van't Hoffs osmotic pressure reduction - van't hoff factor, 'i' [real(1)] has been reduced to 1 to save storage

  ! this value is unused
  capillary_volume_raw = cvr  ! in mL Gehr 1978 based on having a body mass of 74 kg - should be unique to each person
  capillary_volume = (capillary_volume_raw*open_capillaries)/real(num_units) !in mL  Ben: unit_field(nu_vol,nunit)/1000.0_dp!volume in mm3 (from perfusion model) converted to mL !  unit_field(nu_vol,nunit) from venti not works
!       interstitial values (capacity, volume) | first index corresponds to A, second to B
!  ic_array = (/ 0.005_dp*interstitial_capacity, 0.995_dp*interstitial_capacity /) ! in mm3 ! arbitrarily sized
!  iv_array = (/ 0.0_dp, 0.48_dp*interstitial_capacity /) ! in mm3 ! assumption of 48% saturation at rest
  interstitial_capacity_a = 0.005_dp*interstitial_capacity !arbitrarily sized - needs further studies on the capillary-lymph interface
  interstitial_capacity_b = 0.995_dp*interstitial_capacity !arbitrarily sized - needs further studies on the capillary-lymph interface
  interstitial_volume_a = 0.0_dp
  interstitial_volume_b = 0.48_dp*interstitial_capacity !assumed to be around 48% saturated at rest
end subroutine set_lymph_factors


end module lymphatics


!FUTURE DIRECTIONS
!input a constant to account for difference between current values and expected values
     !Model appeared to be working within the range of the literature but is likely off by a factor of 1000 due to nl to ul conversion error.
     !Need to check that outputted units are correct - most things are in ml and mmHg
!Lymphatic network tree
     !currently all lymph is returned to the circulation immediately, in reality it moves up a tree of lymphatics against a pressure gradient
     !would require excessive modelling perhaps
!impairment of gas diffusion caused by high interstitial saturation
     !unclear at what level this would occur
!alveolar flooding changes
     !alveolar flooding should be able to move between adjacent compartments
     !alveolar fluid should be removed via respiration naturally and therefore should always occur naturally at some low level
!individuality needs to be added in line with the other modules
     !currently operates only on preset male/female values
