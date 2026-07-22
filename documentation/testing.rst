
=======
Testing
=======

Testing is an integral part of developing software and validating the code.
It is also builds trust in users of the software that the software will work as expected.
When adding new code to the library itself a test must also be added.  Ideally the tests test every single line of code so that we have 100% code coverage.
When we have good coverage of tests over the library we inherently get regression testing.
We don't want to have new code changes breaking code that is already considered to be working.

For the testing of the Fortran code the pFUnit testing framework has been chosen.
The pFUnit testing framework uses Python to manage some of the test generation, without Python we cannot build the tests.

How to add a test
=================

All tests live under the *tests* directory.
In the following example we are going to add a new testing module for the *diagnostics* module which resides in the *src/lib* directory.

Write test
----------

To start we create the testing module.
Because we want to test the diagnostics module from the library we will create a test file named *test_diagnostics.f90* in the *tests* directory.
With your favourite text editor create a file named *test_diagnostics.f90*.
We could choose *vi* for this task as shown below but any text editor will work::

   vi tests/test_diagnostics.f90

Into this file we will write our test for the module.
First, we are going to add declare a module called *test_diagnostics*::

  module test_diagnostics

  !> We will add framework declarations and code here.

  !> Then we will write our test(s) here.

  end module test_diagnostics

Into this module, we have to add some boiler plate code to declare our test to the framework and make it findable::

  use testdrive, only : new_unittest, unittest_type, error_type, check
  implicit none
  private

In this chunk of code we can see that we are using the *new_unittest, unittest_type, error_type, check* subroutines from the *testdrive* module.
We will also declare a helper subroutine to provide a list of test names to the framework::

  public :: collect_diagnostics

Now we can start with defining the *collect_diagnostics* subroutine and our test(s).
We are going to call our test *test_set_and_get*, and this is what our *collect_diagnostics* subroutine will report to the framework::

  !> Collect all exported unit tests
  subroutine collect_diagnostics(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("test_set_and_get", test_set_and_get) &
      ]

  end subroutine collect_diagnostics

Next we define the *test_set_and_get* test::

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

The complete *tests_diagnostics.f90* file looks like this::

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

With our test written we now need to add this into the CMake build generation system.


Add test to CMake
-----------------

In the file *tests/CMakeLists.txt* we need to add the name of our new test, which is *diagnostics* in this case, to the list of tests.
At the top of this file there is a statement to define a variable named *TESTS*, looking something like this::

  set(
    TESTS
    # Other tests may already be present, if so they will appear here separated by white-space.
  )

When we add the *diagnostics* test the variable definition should look like this::


  set(
    TESTS
    # Other tests may already be present, if so they will appear here separated by white-space.
    "diagnostics"
  )

That is all we need to do to add our test into the testing framework.


Build and run test
------------------

The test we have just completed will be built when we build the configuration from the build directory by default.
That is if we execute the *BUILD_ALL* build target for IDEs like Visual Studio or on *Makefile* generation builds we would simple issue the command *make* in the build directory.

To run the test we can execute the ctest command from the command line in the build directory with the following arguments::

  ctest -R diagnostics_test

we will also execute all tests if we execute the command::

  ctest

A handy flag to add to both of these commands is the *--verbose* flag.
This gives us the detailed output from each test and not just the summary statement::

  ctest -V
