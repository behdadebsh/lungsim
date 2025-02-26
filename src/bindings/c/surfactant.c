#include "surfactant.h"


void evaluate_surf_c ();
void evaluate_surf ()
{
  evaluate_surf_c ();
}

void update_surfactant_concentration_c (double *dt, double *alv_area_current, double *alv_dA, double *surf_concentration);
void update_surfactant_concentration (double dt, double alv_area_current, double alv_dA, double surf_concentration)
{
   update_surfactant_concentration_c (&dt, &alv_area_current, &alv_dA, &surf_concentration);
}


void update_surface_tension_c (double *surf_concentration, double *surface_tension);
void update_surface_tension (double surf_concentration, double surface_tension)
{
   update_surface_tension_c (&surf_concentration, &surface_tension);
}
