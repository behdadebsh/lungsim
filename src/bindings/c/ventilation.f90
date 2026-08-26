module ventilation_c
  implicit none
  private

contains

  subroutine evaluate_vent_coupled_c(filename, filename_len, model, model_len) &
       bind(C, name="evaluate_vent_coupled_c")
    use iso_c_binding, only: c_ptr, c_int
    use utils_c, only: strncpy
    use ventilation, only: evaluate_vent_coupled
    use other_consts, only: MAX_FILENAME_LEN
    integer(c_int), intent(in) :: filename_len, model_len
    type(c_ptr), value, intent(in) :: filename, model
    character(len=MAX_FILENAME_LEN) :: filename_f, model_f
    call strncpy(filename_f,filename,filename_len)
    call strncpy(model_f,model,model_len)
    call evaluate_vent_coupled(filename_f,model_f)
  end subroutine evaluate_vent_coupled_c

!!!###################################################################################

  subroutine evaluate_vent_c(filename, filename_len) bind(C, name="evaluate_vent_c")

    use arrays,only: dp
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use ventilation, only: evaluate_vent
    use other_consts, only: MAX_STRING_LEN, MAX_FILENAME_LEN
    implicit none
    integer,intent(in) :: filename_len
    type(c_ptr), value, intent(in) :: filename
    character(len=MAX_FILENAME_LEN) :: filename_f

    call strncpy(filename_f, filename, filename_len)
    call evaluate_vent(filename_f)

  end subroutine evaluate_vent_c


  !###################################################################################

  subroutine evaluate_uniform_flow_c() bind(C, name="evaluate_uniform_flow_c")

    use ventilation, only: evaluate_uniform_flow
    implicit none

    call evaluate_uniform_flow

  end subroutine evaluate_uniform_flow_c


!###################################################################################
end module ventilation_c
