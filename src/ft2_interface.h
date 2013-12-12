
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
    int force_autohint,
    float *advance_horz_out,
    int *bearing_x_out,
    int *bearing_y_out,
    unsigned int *bitmap_width_out,
    unsigned int *bitmap_pitch_out,
    unsigned int *bitmap_rows_out,
    unsigned char **bitmap);

void faceInfo(
    FT_Face face,
    FT_String **family_name_out,
    FT_String **style_name_out,
    FT_Long *num_glyphs_out,
    int *has_kerning_out,
    FT_Short *height_out,
    FT_Short *ascender_out,
    FT_Short *descender_out);

#endif // FT2_INTERFACE_H

