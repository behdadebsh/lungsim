module parameter_types_c

  implicit none

  private

contains

!!!######################################################################
  
  subroutine update_lung_c(param_name, param_name_len, param_value) bind(C, name="update_lung_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use precision, only: dp
    use parameter_types, only: update_lung
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    real(dp), intent(in) :: param_value
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_lung(param_name_f, param_value)

  end subroutine update_lung_c

!!!######################################################################
  
  subroutine update_gasexchange_c(param_name, param_name_len, param_value) bind(C, name="update_gasexchange_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use precision, only: dp
    use parameter_types, only: update_gasexchange
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    real(dp), intent(in) :: param_value
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_gasexchange(param_name_f, param_value)

  end subroutine update_gasexchange_c

!!!######################################################################
  
  subroutine update_ventilation_c(param_name, param_name_len, param_value) bind(C, name="update_ventilation_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use precision, only: dp
    use parameter_types, only: update_ventilation
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    real(dp), intent(in) :: param_value
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_ventilation(param_name_f, param_value)

  end subroutine update_ventilation_c

!!!######################################################################
  
  subroutine update_cardiac_c(param_name, param_name_len, param_value) bind(C, name="update_cardiac_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use precision, only: dp
    use parameter_types, only: update_cardiac
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    real(dp), intent(in) :: param_value
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_cardiac(param_name_f, param_value)

  end subroutine update_cardiac_c

!!!######################################################################
  
  subroutine update_solve_c(param_name, param_name_len, param_value) bind(C, name="update_solve_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use precision, only: dp
    use parameter_types, only: update_solve
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    real(dp), intent(in) :: param_value
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_solve(param_name_f, param_value)

  end subroutine update_solve_c

!!!######################################################################
  
  subroutine update_species_c(param_name, param_name_len) bind(C, name="update_species_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use parameter_types, only: update_species
    use other_consts, only: MAX_STRING_LEN
    implicit none
    
    integer,intent(in) :: param_name_len
    type(c_ptr), value, intent(in) :: param_name
    character(len=MAX_STRING_LEN) :: param_name_f

    call strncpy(param_name_f, param_name, param_name_len)
    call update_species(param_name_f)

  end subroutine update_species_c

end module parameter_types_c
