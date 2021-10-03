#include "lymphatics.h"
#include "string.h"

void alveolar_capillary_flux_c(int *num_nodes);

void alveolar_capillary_flux(int num_nodes)
{
  alveolar_capillary_flux_c(&num_nodes);
}
