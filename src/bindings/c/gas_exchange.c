#include "gas_exchange.h"

#include "string.h"

double steadystate_co2_c(double *Vdot_alv);
     
double steadystate_o2_c(double *Vdot_alv);

double get_ABG_value_c(const char *request, int *request_len);

void solve_gasexchange_c(double *t_0, double *t_1, const char *phase, int *phase_len, const char *filename, int *filename_len);
     
double steadystate_co2(double Vdot_alv)
{
  return steadystate_co2_c(&Vdot_alv);
}

double steadystate_o2(double Vdot_alv)
{
  return steadystate_o2_c(&Vdot_alv);
}

double get_ABG_value(const char *request)
{
  int request_len = (int)strlen(request);
  return get_ABG_value_c(request, &request_len);
}

void solve_gasexchange(double t_0, double t_1, const char *phase, const char *filename)
{
  int phase_len = (int)strlen(phase);
  int filename_len = (int)strlen(filename);
  solve_gasexchange_c(&t_0, &t_1, phase, &phase_len, filename, &filename_len);
}
