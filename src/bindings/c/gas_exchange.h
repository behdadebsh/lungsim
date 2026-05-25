#ifndef AETHER_GAS_EXCHANGE_H
#define AETHER_GAS_EXCHANGE_H

#include "symbol_export.h"

SHO_PUBLIC double steadystate_co2(double Vdot_alv);
SHO_PUBLIC double steadystate_o2(double Vdot_alv);
SHO_PUBLIC double get_ABG_value(const char *request);
SHO_PUBLIC void solve_gasexchange(double t_0, double t_1, const char *phase, const char *filename);

#endif /* AETHER_GAS_EXCHANGE_H */
