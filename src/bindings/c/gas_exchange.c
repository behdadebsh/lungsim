#include "gas_exchange.h"

double steadystate_co2_c(double *Vdot_alv);
     
double steadystate_o2_c(double *Vdot_alv);
     
double steadystate_co2(double Vdot_alv)
{
  return steadystate_co2_c(&Vdot_alv);
}

double steadystate_o2(double Vdot_alv)
{
  return steadystate_o2_c(&Vdot_alv);
}
