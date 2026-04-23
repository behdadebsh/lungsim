module parameter_types

  use precision

  implicit none

  ! make the defined types available across modules
  public :: lung_params
  public :: gx_params
  public :: V_params
  public :: Q_params
  public :: solve_params
  public :: species_params

  ! make the 'update' subroutines accessible via python bindings
  public :: update_lung
  public :: update_gasexchange
  public :: update_ventilation
  public :: update_cardiac
  public :: update_solve
  public :: update_species

  type :: fundamental_constants
     ! fixed constants; no update option
     real(dp) :: o2molvol_37deg = 25.452e+3_dp          ! mm^3/mmol, O2 molecular volume @BTP; converted from 22.41e3 at STP using V2=T2*V1/T1
     real(dp) :: o2molvol_stpd = 22.41e+3_dp            ! mm^3/mmol, O2 molecular volume @STPD; 
     real(dp) :: max_o2_concentration = 3.93236e-5_dp   ! mmol/mm^3, maximum concentration (at 100% O2)
     real(dp) :: mw = 64458.0_dp                        ! g/mol, molecular weight of Hb
     real(dp) :: alphaO2 = 1.46e-6_dp                   ! mol/mmHg, O2 solubility in water at T=37
     real(dp) :: alphaCO2 = 0.0307_dp                   ! mmol/L/mmHg, CO2 solubility in plasma at T=37 (T dependent)
     real(dp) :: R = 6.23e4_dp                          ! mm^3.mmHg/mmol/K
     real(dp) :: Wbl = 0.81_dp                          ! fractional water content of blood
  end type fundamental_constants
  
  type :: lung_parameters
     ! parameters for species, lung orientation, and sizing
     integer  :: gravity_dirn = 3                       ! gravity direction, 1== on side, 2==supine, 3==upright          
     real(dp) :: surface_area = 3.0e3_dp * 32.0e3_dp    ! mm^2, gas exchange surface area == 30 mm^2/acinus * 32K acini
     real(dp) :: capillary_volume = 80.0e3_dp           ! mm^3, capillary blood volume
     real(dp) :: FRC = 3.0e6_dp                         ! mm^3, functional residual capacity
     real(dp) :: TLC = 6.0e6_dp                         ! mm^3, total lung capacity
     real(dp) :: anatomical_deadspace = 150.0e3_dp      ! mm^3, volume of airways
  end type lung_parameters

  type :: gasexchange_parameters
     ! parameters related to gas properties and gas exchange
     real(dp) :: press_atm = 760.0_dp                   ! mmHg, atmospheric pressure
     real(dp) :: press_H2O = 47.0_dp                    ! mmHg, water vapour pressure
     real(dp) :: diffusion_coeff = 22.5_dp              ! mm^2/s, binary diffusion coefficient
     real(dp) :: FiO2 = 0.21_dp                         ! fractional inspired O2
     real(dp) :: init_p_alv_o2 = 100.0_dp               ! mmHg, initial PO2 in alveoli
     real(dp) :: target_p_art_co2 = 40.0_dp             ! mmHg, target PaCO2
     real(dp) :: target_p_ven_o2 = 38.0_dp              ! mmHg, target PvO2
     real(dp) :: VO2 = 250.0e3_dp /60.0_dp              ! mm^3/s, metabolic consumption of O2
     real(dp) :: VCO2 = 0.8_dp * 250.0e3_dp /60.0_dp    ! mm^3/s, metabolic production of CO2
     real(dp) :: Hb = 150.0_dp * 0.1_dp                 ! g/dL, haemoglobin concentration [M 13.5 - 17.5; F 12.0 - 15.5]
     real(dp) :: pHa = 7.4_dp                           ! pH of arterial blood
     real(dp) :: body_temp = 37.0_dp                    ! degC, body temperature
     character(len=9) :: sat_model = 'dash'             ! the model of O2 saturation. options: dash, kelman, valsecchi
  end type gasexchange_parameters

  type :: ventilation_parameters
     integer  :: breaths_per_minute = 15                ! breaths per minute
     real(dp) :: tidal_volume = 400.0e3_dp              ! mm^3, tidal volume
  end type ventilation_parameters
  
  type :: cardiac_parameters
     ! parameters for cardiac output
     real(dp) :: cardiac_output = 6.0e6_dp /60.0_dp     ! mm^3/s, cardiac output
     real(dp) :: shunt_fraction = 0.02_dp    !proportion of cardiac output that is shunt
  end type cardiac_parameters

  type :: solve_parameters
     ! parameters to control solution and solver
     integer  :: num_breaths = 20                       ! max # breaths to solve for
     integer  :: out_itr_max = 200                      ! max # (outer) iterations using GMRES solver.
     integer  :: inr_itr_max = 100                      ! max # (inner) iterations using GMRES solver.
     real(dp) :: solve_tolerance = 1.0e-8_dp            ! tolerance for comparing residuals
     real(dp) :: dt = 0.025_dp                          ! time step for PDE solution
  end type solve_parameters
    
  type :: species_parameters
     ! define species-specific parameters (non-geometric or functional)

     ! gas exchange - default human values. use update_species to set new values
     real(dp) :: mcv = 90e-3_dp                         ! pico-L, mean RBC volume for human (ref range 80-96)
     real(dp) :: mch = 30_dp                            ! pico-grams, mean mass Hb/RBC for human (ref 27-31 picograms/cell)
     real(dp) :: Hct = 0.45_dp                          ! hematocrit (Dietel & Kampmann 1971)
     real(dp) :: tau_h = 1.11e-3_dp                     ! mm (1.11 um). Thickness of tissue barrier plus plasma. Weibel (1993)
     real(dp) :: S2 = 2.34e4_dp                         ! coefficient in Severinghaus 
  end type species_parameters
  
  ! retain between modules
  type(fundamental_constants)  :: constants
  type(lung_parameters)        :: lung_params
  type(gasexchange_parameters) :: gx_params
  type(ventilation_parameters) :: V_params
  type(cardiac_parameters)     :: Q_params
  type(solve_parameters)       :: solve_params
  type(species_parameters)     :: species_params

  
  contains

    
    subroutine update_lung(param_name, param_value)

      character(len=*), intent(in) :: param_name
      real(dp), intent(in) :: param_value

      select case (trim(param_name))
      case ('gravity_dirn')
         lung_params%gravity_dirn = int(param_value)
      case ('surface_area')
         lung_params%surface_area = param_value
      case ('capillary_volume')
         lung_params%capillary_volume = param_value
      case('FRC')
         lung_params%FRC = param_value
      case('TLC')
         lung_params%TLC = param_value
      case('anatomical_deadspace')
         lung_params%anatomical_deadspace = param_value
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select

    end subroutine update_lung

    subroutine update_gasexchange(param_name, param_value)
      
      character(len=*), intent(in) :: param_name
      real(dp), intent(in) :: param_value

      select case (trim(param_name))
      case ('press_atm')
         gx_params%press_atm = param_value
      case ('press_h2o')
         gx_params%press_H2O = param_value
      case ('diffusion_coefficient')
         gx_params%diffusion_coeff = param_value
      case ('fio2')
         gx_params%FiO2 = param_value
      case('init_p_alv_o2')
         gx_params%init_p_alv_o2 = param_value
      case ('target_p_art_co2')
         gx_params%target_p_art_co2 = param_value
      case ('target_p_ven_o2')
         gx_params%target_p_ven_o2 = param_value
      case ('vo2')
         gx_params%VO2 = param_value
      case ('vco2')
         gx_params%VCO2 = param_value
      case ('hb')
         gx_params%Hb = param_value
      case ('pha')
         gx_params%pHa = param_value
      case ('body_temp')
         gx_params%body_temp = param_value
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select
    end subroutine update_gasexchange

    
    subroutine update_ventilation(param_name, param_value)
      
      character(len=*), intent(in) :: param_name
      real(dp), intent(in) :: param_value
      
      select case (trim(param_name))
      case ('breaths_per_min')
         V_params%breaths_per_minute = param_value
      case ('tidal_volume')
         V_params%tidal_volume = param_value
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select
    end subroutine update_ventilation


    subroutine update_cardiac(param_name, param_value)
      
      character(len=*), intent(in) :: param_name
      real(dp), intent(in) :: param_value

      select case (trim(param_name))
      case ('cardiac_output')
         Q_params%cardiac_output = param_value
      case ('shunt_fraction')
         Q_params%shunt_fraction = param_value
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select
    end subroutine update_cardiac
    

    subroutine update_solve(param_name, param_value)
      
      character(len=*), intent(in) :: param_name
      real(dp), intent(in) :: param_value

      select case (trim(param_name))
      case ('number_of_breaths')
         solve_params%num_breaths = param_value
      case ('max_outer_iterations')
         solve_params%out_itr_max = param_value
      case ('max_inner_iterations')
         solve_params%inr_itr_max = param_value
      case ('solver_tolerance')
         solve_params%solve_tolerance = param_value
      case ('dt')
         solve_params%dt = param_value
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select
    end subroutine update_solve

    
    subroutine update_species(param_name)
      
      character(len=*), intent(in) :: param_name    ! human, rabbit, rat, mouse

      select case (trim(param_name))
      case ('Human')
         species_params%mcv = 90e-3_dp 
         species_params%mch = 30_dp    
         species_params%Hct = 0.45_dp  
         species_params%tau_h = 1.11e-3_dp
         species_params%S2 = 2.34e4_dp    
      case ('Rabbit')
         species_params%mcv = 66.7e-3_dp  
         species_params%mch = 20.95_dp    
         species_params%Hct = 0.436_dp    
         species_params%tau_h = 0.8e-3_dp 
         species_params%S2 = 3.5e4_dp     
      case ('Rat')
         species_params%mcv = 59.35e-3_dp 
         species_params%mch = 17.9_dp     
         species_params%Hct = 0.5182_dp   
         species_params%tau_h = 0.754e-3_dp
         species_params%S2 = 5.0e4_dp      
      case ('Mouse')
         species_params%mcv = 55.1e-3_dp     
         species_params%mch = 15.95_dp       
         species_params%Hct = 0.523_dp       
         species_params%tau_h = 0.7e-3_dp   
         species_params%S2 = 8.0e4_dp        
      case default
         write(*,*) 'WARNING: unknown parameter name: ', trim(param_name)
         write(*,*) '         parameters are case sensitive: use all lowercase'
      end select
    end subroutine update_species


  end module parameter_types
