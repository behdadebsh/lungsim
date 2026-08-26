
# Unit testing
set(
  TESTS
  "diagnostics"
  "lymphatic_parameters"
  "surfactant"
)
set(
  TEST_SRCS
  "testdriver.f90"
)

set(
  TEST_SUITE_USE_STATEMENT
)
set(
  TEST_SUITE_LIST_STATEMENT
)

foreach(t IN LISTS TESTS)
  string(MAKE_C_IDENTIFIER ${t} t)
  list(APPEND TEST_SRCS "test_${t}.f90")
  set(TEST_SUITE_USE_STATEMENT "${TEST_SUITE_USE_STATEMENT}  use test_${t}, only : collect_${t}\n")
  set(TEST_SUITE_LIST_STATEMENT "${TEST_SUITE_LIST_STATEMENT}    new_testsuite(\"${t}\", collect_${t}), &\n")
endforeach()

# Exercise the actual C/SWIG interface as well as the native equations.
if(TARGET pybindings)
  get_directory_property(_coupled_python
    DIRECTORY "${PROJECT_SOURCE_DIR}/src/bindings/python" DEFINITION Python_EXECUTABLE)
  get_directory_property(_coupled_package
    DIRECTORY "${PROJECT_SOURCE_DIR}/src/bindings/python" DEFINITION PYTHON_PACKAGE_DIR)
  add_test(NAME "${PROJECT_NAME}/coupled_python_test"
    COMMAND "${CMAKE_COMMAND}" -E env "PYTHONPATH=${_coupled_package}"
      "${_coupled_python}" "${CMAKE_CURRENT_SOURCE_DIR}/smoke_coupled.py")
  set_tests_properties("${PROJECT_NAME}/coupled_python_test" PROPERTIES TIMEOUT 180)
endif()

string(REGEX REPLACE ", &\n$" " &" TEST_SUITE_LIST_STATEMENT "${TEST_SUITE_LIST_STATEMENT}")
set(TEST_MAIN "${CMAKE_CURRENT_BINARY_DIR}/testmain.f90")
configure_file(testmain.in.f90 ${TEST_MAIN})
list(APPEND TEST_SRCS ${TEST_MAIN})

add_executable(
  "${PROJECT_NAME}-tester"
  "${TEST_SRCS}"
)

target_compile_definitions(
  "${PROJECT_NAME}-tester"
  PRIVATE
  "WITH_QP=$<BOOL:${WITH_QP}>"
  "WITH_XDP=$<BOOL:${WITH_XDP}>"
)

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    set_source_files_properties(testdriver.f90 PROPERTIES COMPILE_FLAGS -cpp)
endif ()

if (MSVC)
  target_compile_options("${PROJECT_NAME}-tester" PUBLIC -fpp)
endif()

target_link_libraries(
  "${PROJECT_NAME}-tester"
  PRIVATE
  aether
)

foreach(t IN LISTS TESTS)
  set(_TEST_NAME "${PROJECT_NAME}/${t}_test")
  add_test("${_TEST_NAME}" "${PROJECT_NAME}-tester" "${t}")
  if(MSVC)
    set(_TEST_PROPERTIES "PATH=$<TARGET_FILE_DIR:aether>\;${FORTRAN_RUNTIME_PATH}")
    set_tests_properties("${_TEST_NAME}" PROPERTIES ENVIRONMENT "${_TEST_PROPERTIES}" RANDOM_THING $<TARGET_FILE_DIR:aether_c>)
  endif()
endforeach()
