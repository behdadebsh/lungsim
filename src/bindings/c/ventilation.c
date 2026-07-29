
#include "ventilation.h"
#include <string.h>

void evaluate_vent_c(const char *filename, int *filename_len);
void evaluate_uniform_flow_c();

void evaluate_vent(const char *filename)
{
  int filename_len = strlen(filename);
  evaluate_vent_c(filename, &filename_len);
}

void evaluate_uniform_flow()
{
  evaluate_uniform_flow_c();
}

