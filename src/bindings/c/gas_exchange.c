#include "gas_exchange.h"

double content_from_po2_c(double *pco2, double *po2);

double steadystate_co2_c(double *p_art_co20, double *p_art_o2, double *p_ven_co20, double *p_ven_o2, double *Vdot_alv);
     
double content_from_po2(double pco2, double po2)
{
  return content_from_po2_c(&pco2, &po2);
}

double steadystate_co2(double p_art_co20, double p_art_o2, double p_ven_co20, double p_ven_o2, double Vdot_alv)
{
  return steadystate_co2_c(&p_art_co20, &p_art_o2, &p_ven_co20, &p_ven_o2, &Vdot_alv);
}
