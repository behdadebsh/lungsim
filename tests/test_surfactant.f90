module test_surfactant
  use testdrive, only: new_unittest, unittest_type, error_type, check
  use precision, only: dp
  use surfactant, only: surfactant_state, initialise_surface, advance_surface, surface_tension
  use coupled_lymphatics, only: fluid_state, initialise_fluid, advance_fluid
  use coupled_lymphatics, only: fluid_running, sample_fluid_convergence, fluid_stop_status
  use parameter_types, only: surfactant_params, coupled_lymphatic_params, update_surfactant, update_coupled_lymphatics
  implicit none
  private
  public :: collect_surfactant
contains
  subroutine collect_surfactant(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [new_unittest('constitutive_law',test_law), &
         new_unittest('flooding_and_repeatability',test_flooding), &
         new_unittest('fluid_and_protein_conservation',test_conservation), &
         new_unittest('mapped_input_and_reset',test_mapping), &
         new_unittest('transit_time_stopping',test_stopping), &
         new_unittest('parameter_updates',test_parameters)]
  end subroutine collect_surfactant

  subroutine test_law(error)
    type(error_type), allocatable, intent(out) :: error
    type(surfactant_state) :: s
    real(dp) :: gmax, expected_radius
    gmax = surfactant_params%gamma_star*(1.0_dp+(surfactant_params%tension_hat- &
         surfactant_params%tension_min)/surfactant_params%m2)
    call check(error,abs(surface_tension(0.0_dp)-70.0_dp) < 1.0e-12_dp)
    if (allocated(error)) return
    call check(error,abs(surface_tension(surfactant_params%gamma_star)-22.0_dp) < 1.0e-12_dp)
    if (allocated(error)) return
    call check(error,abs(surface_tension(gmax)-1.0_dp) < 1.0e-12_dp)
    if (allocated(error)) return
    call initialise_surface(s,100.0_dp)
    expected_radius = (300.0_dp/(37700000.0_dp*2.0_dp*acos(-1.0_dp)))**(1.0_dp/3.0_dp)
    call check(error,abs(s%pressure-2.0_dp*46.0_dp/expected_radius/10.0_dp) < 1.0e-10_dp)
    if (allocated(error)) return
    call check(error,abs(s%compliance*s%pressure-300.0_dp) < 1.0e-10_dp)
  end subroutine test_law

  subroutine test_flooding(error)
    type(error_type), allocatable, intent(out) :: error
    type(surfactant_state) :: normal, flooded, repeated
    integer :: i
    call initialise_surface(normal,100.0_dp)
    flooded = normal
    repeated = normal
    do i = 1,100
       call advance_surface(normal,100.0_dp,.false.,0.01_dp)
       call advance_surface(flooded,100.0_dp,.true.,0.01_dp)
       call advance_surface(repeated,100.0_dp,.false.,0.01_dp)
    enddo
    call check(error,normal%gamma == repeated%gamma)
    if (allocated(error)) return
    call check(error,flooded%gamma < normal%gamma .and. flooded%tension > normal%tension)
    if (allocated(error)) return
    call advance_surface(normal,10.0_dp,.false.,1.0_dp)
    call check(error,normal%gamma >= 0.0_dp .and. normal%tension >= surfactant_params%tension_min-1.0e-12_dp)
  end subroutine test_flooding

  subroutine test_conservation(error)
    type(error_type), allocatable, intent(out) :: error
    type(fluid_state) :: s, repeated
    real(dp) :: v0, q0, v1, q1
    integer :: i
    call initialise_fluid(s,30000)
    repeated = s
    v0 = s%volume_a+s%volume_b
    q0 = s%protein_a+s%protein_b
    do i = 1,10000
       call advance_fluid(s,3000.0_dp,2000.0_dp,100.0_dp,4.0_dp,0.005_dp)
       call advance_fluid(repeated,3000.0_dp,2000.0_dp,100.0_dp,4.0_dp,0.005_dp)
    enddo
    v1 = s%volume_a+s%volume_b+s%alveolar+s%drained
    q1 = s%protein_a+s%protein_b+s%protein_alveolar+s%protein_drained
    call check(error,abs(v1-v0-s%filtered) < 1.0e-10_dp,'Fluid mass balance')
    if (allocated(error)) return
    call check(error,abs(q1-q0-s%protein_filtered) < 1.0e-12_dp,'Protein mass balance')
    if (allocated(error)) return
    call check(error,s%alveolar > 0.0_dp .and. s%drained > 0.0_dp,'Exercise both overflow and drainage')
    if (allocated(error)) return
    call check(error,min(s%volume_a,s%volume_b,s%protein_a,s%protein_b) >= 0.0_dp)
    if (allocated(error)) return
    call check(error,s%volume_a == repeated%volume_a .and. s%protein_a == repeated%protein_a)
  end subroutine test_conservation

  subroutine test_mapping(error)
    use arrays, only: num_units, num_elems, units, unit_field
    use indices, only: ventilation_indices, num_nu, nu_vol
    use coupled_transport, only: prepare_coupling, initialise_coupling, release_coupling, export_coupled, &
         coupled_active, load_coupled_capillary
    type(error_type), allocatable, intent(out) :: error
    integer :: io, terminal
    real(dp) :: values(18)
    character(len=1024) :: header
    call ventilation_indices()
    num_units = 2
    num_elems = 3
    allocate(units(2),unit_field(num_nu,2))
    units = [2,3]
    unit_field = 0.0_dp
    unit_field(nu_vol,:) = 100.0_dp
    open(newunit=io,file='test_coupled_mapping.txt',status='replace')
    write(io,'(a)') '# Shuffled rows must map by terminal element, not row position'
    write(io,*) 3,1500.0_dp,100.0_dp,0.5_dp
    write(io,*) 2,2100.0_dp,200.0_dp,0.75_dp
    close(io)
    call load_coupled_capillary('test_coupled_mapping.txt')
    call prepare_coupling('lymphatic_surfactant')
    call initialise_coupling()
    call export_coupled('test_coupled_mapping.coupled.csv')
    call release_coupling()
    open(newunit=io,file='test_coupled_mapping.coupled.csv',status='old')
    read(io,'(a)') header
    read(io,*) terminal,values
    close(io,status='delete')
    open(newunit=io,file='test_coupled_mapping.txt',status='old')
    close(io,status='delete')
    deallocate(units,unit_field)
    num_units = 0
    num_elems = 0
    call check(error,.not. coupled_active)
    if (allocated(error)) return
    call check(error,terminal == 2 .and. values(10) == 2100.0_dp .and. values(11) == 4200.0_dp)
    if (allocated(error)) return
    call check(error,values(12) == 200.0_dp .and. values(13) == 0.75_dp)
  end subroutine test_mapping

  subroutine test_stopping(error)
    type(error_type), allocatable, intent(out) :: error
    type(fluid_state) :: s
    integer :: i
    call initialise_fluid(s,30000)
    do i = 1,5
       call sample_fluid_convergence(s)
    enddo
    call check(error,fluid_running(s,1.0_dp),'Minimum active time must be honoured even with stable saturation')
    if (allocated(error)) return
    s%elapsed = coupled_lymphatic_params%minimum_transit_times
    call check(error,fluid_stop_status(s,1.0_dp) == 1,'Stable saturation after minimum time should converge')
    if (allocated(error)) return
    s%saturation_error = 0.5_dp
    s%elapsed = coupled_lymphatic_params%maximum_transit_times
    call check(error,fluid_stop_status(s,1.0_dp) == 2,'Transit cap is not saturation convergence')
    if (allocated(error)) return
    call check(error,fluid_stop_status(s,0.0_dp) == 4,'Zero-transit excluded unit must not divide by zero')
  end subroutine test_stopping

  subroutine test_parameters(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: saved_area, saved_scale
    saved_area = surfactant_params%alveoli_per_unit
    saved_scale = coupled_lymphatic_params%pressure_multiplier
    call update_surfactant('alveoli_per_unit',20000.0_dp)
    call update_coupled_lymphatics('pressure_multiplier',1.0_dp)
    call check(error,surfactant_params%alveoli_per_unit == 20000.0_dp .and. &
         coupled_lymphatic_params%pressure_multiplier == 1.0_dp)
    call update_surfactant('alveoli_per_unit',saved_area)
    call update_coupled_lymphatics('pressure_multiplier',saved_scale)
  end subroutine test_parameters
end module test_surfactant
