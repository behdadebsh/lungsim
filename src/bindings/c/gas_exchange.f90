module gas_exchange_c

  use precision, only: dp
  
  implicit none
  private
  
contains
  
!!!######################################################################
  
  function steadystate_co2_c(p_art_co20, p_art_o2, p_ven_co20, p_ven_o2, Vdot_alv) &
       result(p_art_co2) bind(C, name="steadystate_co2_c")
    use gas_exchange, only: steadystate_co2
    
    real(dp) :: p_art_co20, p_art_o2, p_ven_co20, p_ven_o2, Vdot_alv
    real(dp) :: p_art_co2
    
    p_art_co2 = steadystate_co2(p_art_co20, p_art_o2, p_ven_co20, p_ven_o2, Vdot_alv)
    
  end function steadystate_co2_c
  
!!!######################################################################

  function content_from_po2_c(pco2, po2) result (c_from_po2) bind(C, name="content_from_po2_c")
    use gas_exchange, only: content_from_po2

    real(dp) :: pco2, po2
    real(dp) :: c_from_po2

    c_from_po2 = content_from_po2(pco2, po2)

  end function content_from_po2_c
  
end module gas_exchange_c

