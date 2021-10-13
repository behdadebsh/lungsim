#include "lymphatics.h"
#include "string.h"

void alveolar_capillary_flux_c(int *num_nodes);
void lymphatic_transport_c();


void alveolar_capillary_flux(int num_nodes)
{
  alveolar_capillary_flux_c(&num_nodes);
}

void lymphatic_transport()
{
  lymphatic_transport_c();
}
