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
  
end module gas_exchange_c

