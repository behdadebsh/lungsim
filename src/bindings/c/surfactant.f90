module surfactant_c

  !use surfactant


  implicit none

  private 


contains
!

!##############################################################################
  subroutine evaluate_surf_c () &
          bind(C, name="evaluate_surf_c")

    use surfactant, only: evaluate_surf
    use arrays,only: dp

    implicit none


    call evaluate_surf ()


  end subroutine evaluate_surf_c

!##############################################################################

  subroutine update_surfactant_concentration_c (dt, alv_area_current, alv_dA, surf_concentration&
          ) bind(C, name="update_surfactant_concentration_c")

    use surfactant, only: update_surfactant_concentration
    use arrays,only: dp
!    use ventilation, only: update_alveolar_info

    implicit none


    real(dp), dimension(:,:), intent(in) :: alv_area_current, alv_dA
    real(dp), dimension(:,:), intent(inout) :: surf_concentration
    real(dp), intent(in):: dt
    real(dp):: time
!    integer :: nu_vol, nalv

    call update_surfactant_concentration (dt, alv_area_current, alv_dA, surf_concentration)


  end subroutine update_surfactant_concentration_c
!################################################################################

  subroutine update_surface_tension_c(surf_concentration, surface_tension) &
          bind(C, name="update_surface_tension_c")

    use surfactant, only: update_surface_tension
    use arrays,only: dp
    implicit none

    real(dp), dimension(:,:), intent(in) :: surf_concentration



    real(dp), dimension(:,:), intent(inout) :: surface_tension




    call update_surface_tension(surf_concentration, surface_tension)


  end subroutine update_surface_tension_c
!################################################################################

end module surfactant_c


