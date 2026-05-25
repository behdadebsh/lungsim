module gas_exchange_c

  use precision, only: dp
  
  implicit none
  private
  
contains
  
!!!######################################################################
  
  function steadystate_co2_c(Vdot_alv) &
       result(p_art_co2) bind(C, name="steadystate_co2_c")
    use gas_exchange, only: steadystate_co2
    
    real(dp) :: Vdot_alv
    real(dp) :: p_art_co2
    
    p_art_co2 = steadystate_co2(Vdot_alv)
    
  end function steadystate_co2_c
  
!!!######################################################################
  
  function steadystate_o2_c(Vdot_alv) &
       result(p_art_o2) bind(C, name="steadystate_o2_c")
    use gas_exchange, only: steadystate_o2
    
    real(dp) :: Vdot_alv
    real(dp) :: p_art_o2
    
    p_art_o2 = steadystate_o2(Vdot_alv)
    
  end function steadystate_o2_c
  
!!!######################################################################

  function get_ABG_value_c(request, request_len) result(my_value) &
       bind(C, name="get_ABG_value_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use other_consts, only: MAX_FILENAME_LEN
    use gas_exchange, only: get_ABG_value

    integer, intent(in) :: request_len
    type(c_ptr), value, intent(in) :: request
    character(len=MAX_FILENAME_LEN) :: request_f
    real(dp) :: my_value
    
    call strncpy(request_f, request, request_len)
    my_value = get_ABG_value(request_f)

  end function get_ABG_value_c
    
!!!######################################################################

  subroutine solve_gasexchange_c(t_0, t_1, phase, phase_len, filename, filename_len) &
       bind(C, name="solve_gasexchange_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use other_consts, only: MAX_FILENAME_LEN
    use gas_exchange, only: solve_gasexchange

    integer, intent(in) :: phase_len, filename_len
    real(dp), intent(in) :: t_0, t_1
    type(c_ptr), value, intent(in) :: phase, filename
    character(len=MAX_FILENAME_LEN) :: phase_f, filename_f

    call strncpy(phase_f, phase, phase_len)
    call strncpy(filename_f, filename, filename_len)

    call solve_gasexchange(t_0, t_1, phase_f, filename_f)

  end subroutine solve_gasexchange_c
  
end module gas_exchange_c

