
#include "parameter_types.h"

#include "string.h"

extern void update_lung_c(const char *parm_name, int *param_name_len, double *param_value);
extern void update_gasexchange_c(const char *parm_name, int *param_name_len, double *param_value);
extern void update_ventilation_c(const char *parm_name, int *param_name_len, double *param_value);
extern void update_cardiac_c(const char *parm_name, int *param_name_len, double *param_value);
extern void update_solve_c(const char *parm_name, int *param_name_len, double *param_value);
extern void update_species_c(const char *parm_name, int *param_name_len);

void update_lung(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_lung_c(param_name, &param_name_len, &param_value);
}

void update_gasexchange(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_gasexchange_c(param_name, &param_name_len, &param_value);
}

void update_ventilation(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_ventilation_c(param_name, &param_name_len, &param_value);
}

void update_cardiac(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_cardiac_c(param_name, &param_name_len, &param_value);
}

void update_solve(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_solve_c(param_name, &param_name_len, &param_value);
}

void update_species(const char *param_name)
{
  int param_name_len = strlen(param_name);
  update_species_c(param_name, &param_name_len);
}
