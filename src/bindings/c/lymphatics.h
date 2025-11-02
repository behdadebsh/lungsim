#ifndef AETHER_LYMPHATICS_H
#define AETHER_LYMPHATICS_H

#include "symbol_export.h"

SHO_PUBLIC void alveolar_flux(double dt, double time, double T_interval,double Pe_unit_field_pre);
SHO_PUBLIC void lymphatic_transport(const char *filename);

#endif /* AETHER_LYMPHATICS_H */
