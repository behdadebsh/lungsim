
%module(package="aether") exports
%include symbol_export.h
%include exports.h
/* Coupled terminal exporters are declared in exports.h. */

%{
#include "exports.h"
%}
