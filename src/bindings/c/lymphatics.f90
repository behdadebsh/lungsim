module lymphatics_c
  implicit none

  private

contains
!
!###################################################################################
!
!*alveolar_flux:*
  subroutine alveolar_flux_c(dt, time, T_interval,Pe_unit_field_pre) bind(C, name="alveolar_flux_c")
    use lymphatics,only: alveolar_flux
    use arrays,only: dp
    implicit none

    real(dp), intent(in) :: dt,time, T_interval
    real(dp), dimension(:,:), intent(in) :: Pe_unit_field_pre

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_alveolar_flux(dt, time, T_interval,Pe_unit_field_pre)
#else
    call alveolar_flux(dt, time, T_interval,Pe_unit_field_pre)
#endif
    
  end subroutine alveolar_flux_c
!
!###################################################################################
!
!*lymphatic_transport* 
  subroutine lymphatic_transport_c(filename, filename_len) bind(C, name="lymphatic_transport_c")
    use iso_c_binding, only: c_ptr
    use utils_c, only: strncpy
    use other_consts, only: MAX_FILENAME_LEN
    use lymphatics,only: lymphatic_transport
    implicit none
    
    integer,intent(in) :: filename_len
    type(c_ptr), value, intent(in) :: filename
    character(len=MAX_FILENAME_LEN) :: filename_f

    call strncpy(filename_f, filename, filename_len)
#if defined _WIN32 && defined __INTEL_COMPILER
    call so_lymphatic_transport(filename_f)
#else
    call lymphatic_transport(filename_f)
#endif
    
  end subroutine lymphatic_transport_c

!
!###################################################################################
!

end module lymphatics_c
