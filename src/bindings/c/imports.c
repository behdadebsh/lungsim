
#include "imports.h"
#include "utils.h"

#include <string.h>

void import_ventilation_c(const char *FLOWFILE,int *FLOWFILE_LEN);
void import_perfusion_c(const char *FLOWFILE,int *FLOWFILE_LEN);
void import_exelemfield_c(const char *FLOWFILE,int *FLOWFILE_LEN, int *field_no);
void import_terminalfield_c(const char *FILENAME,int *FILENAME_LEN, int *field_no, const char *field1name, int *field1name_len,const char *field2name, int *field2name_len);

void import_ventilation(const char *FLOWFILE)
{
	int filename_len = strlen(FLOWFILE);
	import_ventilation_c(FLOWFILE, &filename_len);
}
void import_perfusion(const char *FLOWFILE)
{
	int filename_len = strlen(FLOWFILE);
	import_perfusion_c(FLOWFILE, &filename_len);
}
void import_exelemfield(const char *FLOWFILE,  int field_no)
{
	int FLOWFILE_LEN = strlen(FLOWFILE);
	import_exelemfield_c(FLOWFILE, &FLOWFILE_LEN, &field_no);
}
void import_terminalfield(const char *FILENAME,  int field_no, const char *field1name, const char *field2name)
{
	int FILENAME_LEN = strlen(FILENAME);
        int field1name_len = strlen(field1name);
	int field2name_len = strlen(field2name);
	import_terminalfield_c(FILENAME, &FILENAME_LEN, &field_no, field1name, &field1name_len, field2name, &field2name_len);
}

