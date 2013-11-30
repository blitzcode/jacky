
#include "ft2_interface.h"

#include <cstdio>
#include <cstdlib>

#define CHECK_ERROR(x) do { if ((x) != 0) return (x); } while (0)

const char * errorToString(FT_Error error)
{
    #undef __FTERRORS_H__
    #define FT_ERRORDEF( e, v, s ) { e, s },
    #define FT_ERROR_START_LIST    {
    #define FT_ERROR_END_LIST      { 0, NULL } };

    static const struct
    {
        int         err_code;
        const char *err_msg;
    } ft_errors [] =

    #include FT_ERRORS_H

    for (unsigned int i=0; ft_errors[i].err_msg != NULL; i++)
    {
        if (ft_errors[i].err_code == error)
            return ft_errors[i].err_msg;
    }

    return NULL;
}

FT_Library g_library;
FT_Face    g_face;

FT_Error initFreeType()
{
    CHECK_ERROR(FT_Init_FreeType(&g_library));

    CHECK_ERROR(FT_New_Face(g_library, "/System/Library/Fonts/HelveticaLight.ttf", 0, &g_face));

    return 0;
}

FT_Error shutdownFreeType()
{
    CHECK_ERROR(FT_Done_FreeType(g_library));

    return 0;
}

