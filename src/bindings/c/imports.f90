module imports_c
  implicit none
  private

contains
!
!###################################################################################
!
  subroutine import_ventilation_c(FLOWFILE, filename_len) bind(C, name="import_ventilation_c")
  
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use imports, only: import_ventilation
    use other_consts, only: MAX_STRING_LEN, MAX_FILENAME_LEN
    implicit none
    integer,intent(in) :: filename_len
    type(c_ptr), value, intent(in) :: FLOWFILE
    character(len=MAX_FILENAME_LEN) :: filename_f

    call strncpy(filename_f, FLOWFILE, filename_len)

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_import_ventilation(filename_f)
#else
    call import_ventilation(filename_f)
#endif

  end subroutine import_ventilation_c
  
!
!###################################################################################
!
  subroutine import_perfusion_c(FLOWFILE, filename_len) bind(C, name="import_perfusion_c")

    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use imports, only: import_perfusion
    use other_consts, only: MAX_STRING_LEN, MAX_FILENAME_LEN
    implicit none
    integer,intent(in) :: filename_len
    type(c_ptr), value, intent(in) :: FLOWFILE
    character(len=MAX_FILENAME_LEN) :: filename_f

    call strncpy(filename_f, FLOWFILE, filename_len)

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_import_perfusion(filename_f)
#else
    call import_perfusion(filename_f)
#endif

  end subroutine import_perfusion_c

!
!###################################################################################
!
  subroutine import_exelemfield_c(FLOWFILE, FLOWFILE_len, field_no) bind(C, name="import_exelemfield_c")

    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use imports, only: import_exelemfield
    use other_consts, only: MAX_STRING_LEN, MAX_FILENAME_LEN
    implicit none
    integer,intent(in) :: FLOWFILE_len,field_no
    type(c_ptr), value, intent(in) :: FLOWFILE
    character(len=MAX_FILENAME_LEN) :: FLOWFILE_f

    call strncpy(FLOWFILE_f, FLOWFILE, FLOWFILE_len)

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_import_exelemfield(FLOWFILE_f,field_no)
#else
    call import_exelemfield(FLOWFILE_f,field_no)
#endif

  end subroutine import_exelemfield_c

!
!###################################################################################
!
  subroutine import_terminalfield_c(FILENAME, filename_len, field_no, field1name, field1name_len, &
field2name,field2name_len) bind(C, name="import_terminalfield_c")

    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use imports, only: import_terminalfield
    use other_consts, only: MAX_STRING_LEN, MAX_FILENAME_LEN
    implicit none
    integer,intent(in) :: filename_len,field_no,field1name_len,field2name_len
    type(c_ptr), value, intent(in) :: FILENAME,field1name,field2name
    character(len=MAX_FILENAME_LEN) :: filename_f,field1name_f,field2name_f

    call strncpy(filename_f, FILENAME, filename_len)
    call strncpy(field1name_f, field1name, field1name_len)
    call strncpy(field2name_f, field2name, field2name_len)

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_import_terminalfield(filename_f,field_no,field1name_f,field2name_f)
#else
    call import_terminalfield(filename_f,field_no,field1name_f,field2name_f)
#endif

  end subroutine import_terminalfield_c

end module imports_c

