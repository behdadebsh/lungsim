#ifndef AETHER_PRESSURE_RESISTANCE_FLOW_H
#define AETHER_PRESSURE_RESISTANCE_FLOW_H

#include "symbol_export.h"


SHO_PUBLIC void evaluate_prq(const char *mesh_type, const char *vessel_type, int grav_dirn, double grav_factor, const char *bc_type, double inlet_bc, double outlet_bc, double RMPA_flow, double LMPA_flow, int remodeling_grade);

SHO_PUBLIC void find_occlusion(double cluster_number, double intensity_ratio);


#endif /* AETHER_PRESSURE_RESISTANCE_FLOW_H */
