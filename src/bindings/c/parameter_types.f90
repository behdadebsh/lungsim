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

end module parameter_types_c
