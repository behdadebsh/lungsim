#ifndef AETHER_SURFACTANT_H
#define AETHER_SURFACTANT_H

#include "symbol_export.h"

SHO_PUBLIC void evaluate_surf();
SHO_PUBLIC void update_surfactant_concentration (double dt, double alv_area_current, double alv_dA, double surf_concentration);
SHO_PUBLIC void update_surface_tension (double surf_concentration, double surface_tension);

#endif /* AETHER_SURFACTANT_H */