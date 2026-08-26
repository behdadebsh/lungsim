
#include "ventilation.h"
#include <string.h>

void evaluate_vent_c(const char *filename, int *filename_len);
void evaluate_uniform_flow_c();
void evaluate_vent_coupled_c(const char *filename, int *filename_len, const char *capillary_file, int *capillary_file_len);

void evaluate_vent_coupled(const char *filename, const char *capillary_file)
{
  int filename_len = strlen(filename);
  int capillary_file_len = strlen(capillary_file);
  evaluate_vent_coupled_c(filename, &filename_len, capillary_file, &capillary_file_len);
}

void evaluate_vent(const char *filename)
{
  int filename_len = strlen(filename);
  evaluate_vent_c(filename, &filename_len);
}

void evaluate_uniform_flow()
{
  evaluate_uniform_flow_c();
}

