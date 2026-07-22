
module test_diagnostics
  use testdrive, only : new_unittest, unittest_type, error_type, check
  implicit none
  private

  public :: collect_diagnostics

contains

!> Collect all exported unit tests
subroutine collect_diagnostics(testsuite)
  !> Collection of tests
  type(unittest_type), allocatable, intent(out) :: testsuite(:)

  testsuite = [ &
    new_unittest("test_set_and_get", test_set_and_get) &
    ]

end subroutine collect_diagnostics

subroutine test_set_and_get(error)
  use diagnostics, only: get_diagnostics_on, set_diagnostics_on
  implicit none

  type(error_type), allocatable, intent(out) :: error

  logical :: level

  call get_diagnostics_on(level)
  call check(error, .false., level)
  if (allocated(error)) return

  call set_diagnostics_on(.true.)
  call get_diagnostics_on(level)
  call check(error, .true., level)
  if (allocated(error)) return
  
end subroutine test_set_and_get

end module test_diagnostics
