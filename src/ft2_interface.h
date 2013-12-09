
#ifndef FT2_INTERFACE_H
#define FT2_INTERFACE_H

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

const char * errorToString(FT_Error error);
FT_Error debugPrintTest();
FT_Error renderGlyph(
    FT_Face face,
    FT_ULong char_code,
    unsigned int *advance_horz_out,
    int *bearing_x_out,
    int *bearing_y_out,
    unsigned int *bitmap_width_out,
    unsigned int *bitmap_pitch_out,
    unsigned int *bitmap_rows_out,
    unsigned char **bitmap);

#endif // FT2_INTERFACE_H

