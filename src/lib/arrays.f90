module arrays
  !*Brief Description:* This module defines arrays.
  !
  !*LICENSE:*
  !
  !
  !*Contributor(s):* Merryn Tawhai, Alys Clark
  !
  !*Full Description:*
  !
  !This module defines arrays
  
  use precision
  
  implicit none

  integer :: num_elems,num_elems_2d,num_groups,num_nodes,num_data, &
       num_nodes_2d,num_triangles,num_units,num_vertices,num_lines_2d,maxgen

  integer,allocatable :: nodes(:) !allocated in define_node_geometry
  integer,allocatable :: nodes_2d(:) !allocated in define_node_geometry_2d
  integer,allocatable :: node_versn_2d(:) !allocated in define_node_geometry_2d
  integer :: ndata_groups(20,2)
  integer,allocatable :: nelem_groups(:,:)
  integer,allocatable :: elems(:) !allocated in define_1d_elements
  integer,allocatable :: lines_2d(:)
  integer,allocatable :: line_versn_2d(:,:,:)
  integer,allocatable :: lines_in_elem(:,:)
  integer,allocatable :: nodes_in_line(:,:,:)
  integer,allocatable :: elems_2d(:) !allocated in define_elem_geometry_2d
  integer,allocatable :: elem_cnct(:,:,:)  !NXI(-ni:ni,1,ne)
  integer,allocatable :: elem_cnct_2d(:,:,:)
  integer,allocatable :: elem_nodes(:,:)
  integer,allocatable :: elem_nodes_2d(:,:)
  integer,allocatable :: elem_versn_2d(:,:)
  integer,allocatable :: elem_lines_2d(:,:)
  integer,allocatable :: elem_ordrs(:,:)
  integer,allocatable :: elem_symmetry(:)
  integer,allocatable :: elem_units_below(:)
  integer,allocatable :: elems_at_node(:,:)
  integer,allocatable :: elems_at_node_2d(:,:)
  integer,allocatable :: triangle(:,:)
  integer,allocatable :: units(:)
  integer,allocatable :: units_effective(:)

  ! from p-r-f
  integer,allocatable :: mesh_from_depvar(:,:,:)
  integer, allocatable :: depvar_at_node(:,:,:)
  integer, allocatable :: depvar_at_elem(:,:,:)
  integer, allocatable :: SparseCol(:)
  integer, allocatable :: SparseRow(:)
  integer, allocatable :: update_resistance_entries(:)
  real(dp), allocatable :: SparseVal(:)
  real(dp), allocatable :: RHS(:)
  real(dp), allocatable :: prq_solution(:,:),solver_solution(:)
  logical, allocatable :: FIX(:)
  
  real(dp),allocatable :: arclength(:)
  real(dp),allocatable :: elem_field(:,:) !properties of elements
  real(dp),allocatable :: elem_direction(:,:)
  real(dp),allocatable :: node_xyz(:,:)
  real(dp),allocatable :: data_field(:,:)
  real(dp),allocatable :: data_xyz(:,:)
  real(dp),allocatable :: data_weight(:,:)
  real(dp),allocatable :: node_xyz_2d(:,:,:,:)
  real(dp),allocatable :: gasex_field(:,:) !gasexchange specific fields
  real(dp),allocatable :: unit_field(:,:) !properties of elastic units
  real(dp),allocatable :: vertex_xyz(:,:)
  real(dp),allocatable :: node_field(:,:)
  real(dp),allocatable :: scale_factors_2d(:,:)

  character(len=20),dimension(20) :: data_group_names,elem_group_names
  
  logical,allocatable :: expansile(:)

  type capillary_bf_parameters
    integer :: num_symm_gen=9 !no units
    real(dp) :: total_cap_area=0.63000e02_dp !m
    real(dp) :: Palv=0.0_dp!Pa
    real(dp) :: H0=0.35000e-05_dp !m
    real(dp) :: K_cap=0.12000e02_dp
    real(dp) :: F_cap=0.18000e01_dp
    real(dp) :: F_sheet=0.10400e00_dp
    real(dp) :: sigma_cap=0.43637e03_dp !Pa
    real(dp) :: mu_c=0.19200e-02_dp !Pa.s
    real(dp) :: alpha_a=2.33e-08_dp !m/Pa
    real(dp) :: alpha_v=2.33e-08_dp !m/Pa
    real(dp) :: F_rec=0.64630e00_dp
    real(dp) :: sigma_rec=0.22300e04_dp
    real(dp) :: L_c=0.11880e-02_dp !m
    real(dp) :: Plb_c=0.0_dp !Pa
    real(dp) :: Pub_c=3138.24_dp !Pa
    real(dp) :: Pub_a_v=3138.24_dp !Pa
    real(dp) :: L_art_terminal=0.13000e-03_dp !m
    real(dp) :: L_vein_terminal=0.13000e-03_dp !m
    real(dp) :: R_art_terminal=0.10000e-04_dp !m
    real(dp) :: R_vein_terminal=0.90000e-05!m
  end type capillary_bf_parameters

  type admittance_param
    character (len=20) :: admittance_type
    character (len=20) :: bc_type
  end type admittance_param
  type, EXTENDS (admittance_param) :: two_parameter
     real(dp) :: admit_P1=1.0_dp
     real(dp) :: admit_P2=1.0_dp
  end type two_parameter
  type, EXTENDS (two_parameter) :: three_parameter
    real(dp) :: admit_P3=1.0_dp
  end type three_parameter
  type, EXTENDS (three_parameter) :: four_parameter
    real(dp) :: admit_P4=1.0_dp
  end type four_parameter
  type,EXTENDS (four_parameter) :: all_admit_param
  end type all_admit_param

  type elasticity_vessels
    character(len=20) ::vessel_type
  end type elasticity_vessels
  type, EXTENDS(elasticity_vessels) :: elasticity_param
    real(dp) :: elasticity_parameters(3)=0.0_dp
  end type elasticity_param

  type fluid_properties
     real(dp) :: blood_viscosity = 0.33600e-02_dp ! Pa.s
     real(dp) :: blood_density = 0.10500e-02_dp   ! kg/cm3
     real(dp) :: air_viscosity = 1.8e-5_dp        ! Pa.s
     real(dp) :: air_density = 1.146e-6_dp        ! g.mm^-3
  end type fluid_properties

  type default_lymphatic_properties
     ! Published human defaults for the pulmonary lymphatic transport model.
     real(dp) :: lung_mass_g = 639.0_dp
     real(dp) :: breathing_rate_bpm = 15.0_dp
     real(dp) :: capillary_hydraulic_conductivity = 4.41335e-8_dp
     real(dp) :: interstitial_capacity_ml_per_100g = 30.0_dp
     real(dp) :: initial_interstitial_saturation = 0.48_dp
     real(dp) :: interstitial_compartment_a_fraction = 0.005_dp
     real(dp) :: interstitial_pressure_min_mmhg = -8.0_dp
     real(dp) :: interstitial_pressure_max_mmhg = -1.0_dp
     real(dp) :: lymphatic_pressure_min_mmhg = -8.0_dp
     real(dp) :: lymphatic_pressure_max_mmhg = 1.0_dp
     real(dp) :: lymphatic_density = 1.0_dp
     real(dp) :: lymphatic_saturation_threshold = 0.3_dp
     real(dp) :: lymphatic_baseline_conductivity_ratio = 1.48_dp
     real(dp) :: lymphatic_conductivity_coefficient_1 = 845.87_dp
     real(dp) :: lymphatic_conductivity_coefficient_2 = -2416.7_dp
     real(dp) :: lymphatic_conductivity_coefficient_3 = 2388.5_dp
     real(dp) :: lymphatic_conductivity_coefficient_4 = -922.24_dp
     real(dp) :: lymphatic_conductivity_coefficient_5 = 125.85_dp
     real(dp) :: lymphatic_conductivity_coefficient_6 = -0.0067_dp
     real(dp) :: pressure_phase_offset_radians = 1.570796326794895_dp
     integer :: integration_steps_per_transit = 96
     real(dp) :: convergence_tolerance = 0.000005_dp
     ! Retained legacy fields. Integrity and test_time are not used by the
     ! published equations; reflection_coefficient only affects the inactive
     ! osmotic pathway.
     real(dp) :: lymphatic_integrity = 1.0_dp
     real(dp) :: reflection_coefficient = 0.0_dp
     real(dp) :: test_time = 86400.0_dp
  end type default_lymphatic_properties

  type(default_lymphatic_properties) :: lymphatic_properties

  type :: tree_nodes
     real(dp), allocatable :: xyz(:,:)
  end type tree_nodes

  type(tree_nodes) :: airway_nodes  
     
  type :: tree_elems
     real(dp), allocatable :: seed_xyz(:,:)
  end type tree_elems

  type(tree_elems) :: airway_elems
     
  type :: gx_units
     
     real(dp), allocatable :: Vdot(:)
     real(dp), allocatable :: Qdot(:)
     
     real(dp), allocatable :: p_alv_o2(:)
     real(dp), allocatable :: p_cap_o2(:)
     real(dp), allocatable :: c_cap_o2(:)
     real(dp), allocatable :: conc_o2(:)
     
     real(dp), allocatable :: p_alv_co2(:)
     real(dp), allocatable :: p_cap_co2(:)
     real(dp), allocatable :: c_cap_co2(:)
     real(dp), allocatable :: conc_co2(:)

     real(dp), allocatable :: ph_cap(:)
     real(dp), allocatable :: sat_cap(:)

     real(dp), allocatable :: V_cap(:)
     real(dp), allocatable :: S_area(:)
     real(dp), allocatable :: volume(:)
     real(dp), allocatable :: t_time(:)
     real(dp), allocatable :: t_in_transit(:)
     
  end type gx_units

  type(gx_units) :: gasex

! temporary, for debugging:
  real(dp) :: unit_before

  private

  public set_node_field_value, elem_field, num_elems, num_elems_2d, num_groups, elem_nodes, node_xyz, &
       nodes,nodes_2d, elems, num_nodes, num_nodes_2d, num_data, num_triangles, num_vertices, &
       data_field, data_xyz, data_weight, &
       node_xyz_2d, node_versn_2d, units, units_effective, num_units, unit_field, node_field, dp, &
       data_group_names, elem_group_names, ndata_groups, nelem_groups, &
       elem_cnct, elem_ordrs, elem_direction, elems_at_node, elem_symmetry, expansile, &
       elem_units_below, maxgen,capillary_bf_parameters, zero_tol,loose_tol,gasex_field, &
       num_lines_2d, lines_2d, line_versn_2d, lines_in_elem, nodes_in_line, elems_2d, &
       elem_cnct_2d, elem_nodes_2d, elem_versn_2d, elem_lines_2d, elems_at_node_2d, arclength, &
       scale_factors_2d, fluid_properties, lymphatic_properties, elasticity_vessels, admittance_param, &
       elasticity_param, two_parameter, three_parameter, four_parameter, all_admit_param, &
       mesh_from_depvar, depvar_at_node, depvar_at_elem, SparseCol, SparseRow, triangle, &
       update_resistance_entries, vertex_xyz, &
       SparseVal, RHS, prq_solution, solver_solution, FIX, gasex, airway_elems, airway_nodes, &
       update_parameter

contains
  subroutine set_node_field_value(row, col, value)
    implicit none

    integer, intent(in) :: row, col
    real(dp), intent(in) :: value

    node_field(row, col) = value

  end subroutine set_node_field_value

  subroutine update_parameter(parameter_name, parameter_value)
    ! Update a user-configurable lymphatic model parameter.
    !DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SO_UPDATE_PARAMETER" :: UPDATE_PARAMETER
    character(len=*), intent(in) :: parameter_name
    real(dp), intent(in) :: parameter_value

    select case (trim(parameter_name))
    case ('lung_mass_g')
       lymphatic_properties%lung_mass_g = parameter_value
    case ('breathing_rate_bpm')
       lymphatic_properties%breathing_rate_bpm = parameter_value
    case ('capillary_hydraulic_conductivity')
       lymphatic_properties%capillary_hydraulic_conductivity = parameter_value
    case ('interstitial_capacity_ml_per_100g')
       lymphatic_properties%interstitial_capacity_ml_per_100g = parameter_value
    case ('initial_interstitial_saturation')
       lymphatic_properties%initial_interstitial_saturation = parameter_value
    case ('interstitial_compartment_a_fraction')
       lymphatic_properties%interstitial_compartment_a_fraction = parameter_value
    case ('interstitial_pressure_min_mmhg')
       lymphatic_properties%interstitial_pressure_min_mmhg = parameter_value
    case ('interstitial_pressure_max_mmhg')
       lymphatic_properties%interstitial_pressure_max_mmhg = parameter_value
    case ('lymphatic_pressure_min_mmhg')
       lymphatic_properties%lymphatic_pressure_min_mmhg = parameter_value
    case ('lymphatic_pressure_max_mmhg')
       lymphatic_properties%lymphatic_pressure_max_mmhg = parameter_value
    case ('lymphatic_surface_area_ratio')
       ! Descriptive alias for the historical lymphatic_density name.
       lymphatic_properties%lymphatic_density = parameter_value
    case ('lymphatic_density')
       lymphatic_properties%lymphatic_density = parameter_value
    case ('lymphatic_saturation_threshold')
       lymphatic_properties%lymphatic_saturation_threshold = parameter_value
    case ('lymphatic_baseline_conductivity_ratio')
       lymphatic_properties%lymphatic_baseline_conductivity_ratio = parameter_value
    case ('lymphatic_conductivity_coefficient_1')
       lymphatic_properties%lymphatic_conductivity_coefficient_1 = parameter_value
    case ('lymphatic_conductivity_coefficient_2')
       lymphatic_properties%lymphatic_conductivity_coefficient_2 = parameter_value
    case ('lymphatic_conductivity_coefficient_3')
       lymphatic_properties%lymphatic_conductivity_coefficient_3 = parameter_value
    case ('lymphatic_conductivity_coefficient_4')
       lymphatic_properties%lymphatic_conductivity_coefficient_4 = parameter_value
    case ('lymphatic_conductivity_coefficient_5')
       lymphatic_properties%lymphatic_conductivity_coefficient_5 = parameter_value
    case ('lymphatic_conductivity_coefficient_6')
       lymphatic_properties%lymphatic_conductivity_coefficient_6 = parameter_value
    case ('pressure_phase_offset_radians')
       lymphatic_properties%pressure_phase_offset_radians = parameter_value
    case ('integration_steps_per_transit')
       lymphatic_properties%integration_steps_per_transit = nint(parameter_value)
    case ('convergence_tolerance')
       lymphatic_properties%convergence_tolerance = parameter_value
    case ('lymphatic_integrity')
       lymphatic_properties%lymphatic_integrity = parameter_value
    case ('reflection_coefficient')
       lymphatic_properties%reflection_coefficient = parameter_value
    case ('test_time')
       lymphatic_properties%test_time = parameter_value
    case ('help')
       call print_lymphatic_parameters()
    case default
       write(*,*) 'WARNING: unknown lymphatic parameter name: ', trim(parameter_name)
       write(*,*) '         parameters are case sensitive: use all lowercase'
    end select

  end subroutine update_parameter

  subroutine print_lymphatic_parameters()
    write(*,'('' Current values for update_parameter:'')')
    write(*,'(''    - lung_mass_g = '',es12.5)') lymphatic_properties%lung_mass_g
    write(*,'(''    - breathing_rate_bpm = '',es12.5)') lymphatic_properties%breathing_rate_bpm
    write(*,'(''    - capillary_hydraulic_conductivity = '',es12.5)') &
         lymphatic_properties%capillary_hydraulic_conductivity
    write(*,'(''    - interstitial_capacity_ml_per_100g = '',es12.5)') &
         lymphatic_properties%interstitial_capacity_ml_per_100g
    write(*,'(''    - initial_interstitial_saturation = '',es12.5)') &
         lymphatic_properties%initial_interstitial_saturation
    write(*,'(''    - interstitial_compartment_a_fraction = '',es12.5)') &
         lymphatic_properties%interstitial_compartment_a_fraction
    write(*,'(''    - interstitial_pressure_min_mmhg = '',es12.5)') &
         lymphatic_properties%interstitial_pressure_min_mmhg
    write(*,'(''    - interstitial_pressure_max_mmhg = '',es12.5)') &
         lymphatic_properties%interstitial_pressure_max_mmhg
    write(*,'(''    - lymphatic_pressure_min_mmhg = '',es12.5)') &
         lymphatic_properties%lymphatic_pressure_min_mmhg
    write(*,'(''    - lymphatic_pressure_max_mmhg = '',es12.5)') &
         lymphatic_properties%lymphatic_pressure_max_mmhg
    write(*,'(''    - lymphatic_density = '',es12.5)') lymphatic_properties%lymphatic_density
    write(*,'(''    - lymphatic_saturation_threshold = '',es12.5)') &
         lymphatic_properties%lymphatic_saturation_threshold
    write(*,'(''    - lymphatic_baseline_conductivity_ratio = '',es12.5)') &
         lymphatic_properties%lymphatic_baseline_conductivity_ratio
    write(*,'(''    - lymphatic_conductivity_coefficient_1 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_1
    write(*,'(''    - lymphatic_conductivity_coefficient_2 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_2
    write(*,'(''    - lymphatic_conductivity_coefficient_3 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_3
    write(*,'(''    - lymphatic_conductivity_coefficient_4 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_4
    write(*,'(''    - lymphatic_conductivity_coefficient_5 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_5
    write(*,'(''    - lymphatic_conductivity_coefficient_6 = '',es12.5)') &
         lymphatic_properties%lymphatic_conductivity_coefficient_6
    write(*,'(''    - pressure_phase_offset_radians = '',es12.5)') &
         lymphatic_properties%pressure_phase_offset_radians
    write(*,'(''    - integration_steps_per_transit = '',i0)') &
         lymphatic_properties%integration_steps_per_transit
    write(*,'(''    - convergence_tolerance = '',es12.5)') &
         lymphatic_properties%convergence_tolerance
  end subroutine print_lymphatic_parameters

end module arrays
