module lymphatics_c
  implicit none

  private

contains
!
!###################################################################################
!
!*alveolar_capillary_flux:*
  subroutine alveolar_capillary_flux_c(num_nodes,write_out) bind(C, name="alveolar_capillary_flux_c")
    use iso_c_binding, only: c_int
    use lymphatics,only: alveolar_capillary_flux
    implicit none

    integer(c_int),intent(in) :: num_nodes
    integer(c_int),intent(in) :: write_out
    logical :: write_out_f

    write_out_f = write_out /= 0

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_alveolar_capillary_flux(num_nodes,write_out_f)
#else
    call alveolar_capillary_flux(num_nodes,write_out_f)
#endif

  end subroutine alveolar_capillary_flux_c
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
