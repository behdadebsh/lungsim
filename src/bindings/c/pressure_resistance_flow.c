#include "pressure_resistance_flow.h"
#include <string.h>

void compliance_list_c(int *elemlist2_len, int elemlist2[]);
void occlusion_list_c(int *elemlist_len, int elemlist[]);

void evaluate_prq_c(const char *mesh_type, int *mesh_type_len, const char *vessel_type, int *vessel_type_len,int *grav_dirn, double *grav_factor, const char *bc_type, int *bc_type_len,  double *inlet_bc, double *outlet_bc,  double *remodeling_grade);

void compliance_list(int elemlist2_len, int elemlist2[])
{
  compliance_list_c(&elemlist2_len, elemlist2);
}

void occlusion_list(int elemlist_len, int elemlist[])
{
  occlusion_list_c(&elemlist_len, elemlist);
}

void evaluate_prq(const char *mesh_type, const char *vessel_type,int grav_dirn, double grav_factor, const char *bc_type, double inlet_bc, double outlet_bc, double remodeling_grade)
{
    int mesh_type_len = strlen(mesh_type);
	int bc_type_len = strlen(bc_type);
	int vessel_type_len = strlen(vessel_type);
    evaluate_prq_c(mesh_type, &mesh_type_len,vessel_type, &vessel_type_len, &grav_dirn, &grav_factor, bc_type, &bc_type_len, &inlet_bc, &outlet_bc, &remodeling_grade);
}
