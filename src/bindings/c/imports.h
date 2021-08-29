#ifndef AETHER_IMPORTS_H
#define AETHER_IMPORTS_H

#include "symbol_export.h"

SHO_PUBLIC void import_ventilation(const char *FLOWFILE);
SHO_PUBLIC void import_perfusion(const char *FLOWFILE);
SHO_PUBLIC void import_exelemfield(const char *FLOWFILE, int field_no);
SHO_PUBLIC void import_terminalfield(const char *FILENAME, int field_no, const char *field1name, const char *field2name);


#endif /* AETHER_IMPORTS_H */
