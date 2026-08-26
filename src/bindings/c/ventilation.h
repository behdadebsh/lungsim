#ifndef AETHER_VENTILATION_H
#define AETHER_VENTILATION_H

#include "symbol_export.h"

SHO_PUBLIC void evaluate_vent(const char *filename);
SHO_PUBLIC void evaluate_vent_coupled(const char *filename, const char *model);
SHO_PUBLIC void evaluate_uniform_flow();

#endif /* AETHER_VENTILATION_H */
