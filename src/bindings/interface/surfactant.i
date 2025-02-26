%module(package="aether") surfactant
%include symbol_export.h
%include surfactant.h

%{
#include "surfactant.h"
%}

//%{
//#define SWIG_FILE_WITH_INIT
//%}

//%include "numpy.i"

//%init %{
//  import_array();
//%}

//%apply (double* IN_ARRAY1) {(double* vec1),
//                            (double* vec2)}


//%rename (update_surface_tension) my_update_surface_tension;

//%exception my_update_surface_tension {
//    $action
//    if (PyErr_Occurred()) SWIG_fail;
//}

//%inline %{
//void my_update_surface_tension(double* vec1, double* vec2) {
    //if (len1 != len2) {
    //    PyErr_Format(PyExc_ValueError,
    //                 "Arrays of lengths (%d,%d) given",
    //                 len1, len2);
    //    return;
    //}
//    update_surface_tension(surf_concentration, surface_tension);
//}
//%}