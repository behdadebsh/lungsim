#ifndef AETHER_GAS_EXCHANGE_H
#define AETHER_GAS_EXCHANGE_H

#include "symbol_export.h"

SHO_PUBLIC double content_from_po2(double pco2, double po2);
SHO_PUBLIC double steadystate_co2(double p_art_co20, double p_art_o2, double p_ven_co20, double p_ven_o2, double Vdot_alv);

#endif /* AETHER_GAS_EXCHANGE_H */
