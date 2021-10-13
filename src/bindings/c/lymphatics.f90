module lymphatics_c
  implicit none

  private

contains
!
!###################################################################################
!
!*alveolar_capillary_flux:* 
  subroutine alveolar_capillary_flux_c(num_nodes) bind(C, name="alveolar_capillary_flux_c")
    use lymphatics,only: alveolar_capillary_flux
    implicit none

    integer,intent(in) :: num_nodes

#if defined _WIN32 && defined __INTEL_COMPILER
    call so_alveolar_capillary_flux(num_nodes)
#else
    call alveolar_capillary_flux(num_nodes)
#endif
    
  end subroutine alveolar_capillary_flux_c
!
!###################################################################################
!
!*lymphatic_transport* 
  subroutine lymphatic_transport_c() bind(C, name="lymphatic_transport_c")
    use lymphatics,only: lymphatic_transport
    implicit none
    
#if defined _WIN32 && defined __INTEL_COMPILER
    call so_lymphatic_transport()
#else
    call lymphatic_transport()
#endif
    
  end subroutine lymphatic_transport_c

!
!###################################################################################
!

end module lymphatics_c
