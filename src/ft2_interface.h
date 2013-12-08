
#ifndef FT2_INTERFACE_H
#define FT2_INTERFACE_H

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

const char * errorToString(FT_Error error);
FT_Error debugPrintTest();

#endif // FT2_INTERFACE_H

