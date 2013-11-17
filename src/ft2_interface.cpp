
#include "ft2_interface.h"

#include <cstdio>
#include <cstdlib>

#include <ft2build.h>
#include FT_FREETYPE_H

const char * errorToString(FT_Error error)
{
    #undef __FTERRORS_H__
    #define FT_ERRORDEF( e, v, s ) { e, s },
    #define FT_ERROR_START_LIST    {
    #define FT_ERROR_END_LIST      { 0, NULL } };

    static const struct
    {
        int          err_code;
        const char*  err_msg;
    } ft_errors [] =

    #include FT_ERRORS_H

    unsigned int i = 0;
    while (ft_errors[i].err_msg != NULL) 
    {
        if (ft_errors[i].err_code == error)
            return ft_errors[i].err_msg;
    }

    return NULL;
}

FT_Library library;

void initFreeType()
{
    int error = FT_Init_FreeType(&library);
    if (error)
        printf("FT2 Error\n");

    printf("FT2 Init\n");
}

void shutdownFreeType()
{
    int error = FT_Done_FreeType(library);
    if (error)
        printf("FT2 Error\n");

    printf("FT2 Shutdown\n");
}

