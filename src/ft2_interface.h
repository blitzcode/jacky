
#ifndef FT2_INTERFACE_H
#define FT2_INTERFACE_H

#include <ft2build.h>
#include FT_FREETYPE_H

extern "C"
{

FT_Error initFreeType();
FT_Error shutdownFreeType();
const char * errorToString(FT_Error error);
FT_Int * libraryVersion();

}

#endif // FT2_INTERFACE_H

