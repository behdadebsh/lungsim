
#include "diagnostics.h"

#include "string.h"

void update_lung_c(const char *parm_name, int *param_name_len, double *param_value);

void update_lung(const char *param_name, double param_value)
{
  int param_name_len = strlen(param_name);
  update_lung_c(param_name, &param_name_len, &param_value);
}
