
#include "ft2_interface.h"

#include <cstdio>
#include <cstdlib>

#include <ft2build.h>
#include FT_FREETYPE_H

FT_Library library;

void initFreeType()
{
    int error = FT_Init_FreeType (&library);
    if (error)
        printf("FT2 Error\n");
    
    printf("FT2 Success\n");
}

