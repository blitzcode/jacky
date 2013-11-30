
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
        if (ft_errors[i].err_code == error)
            return ft_errors[i].err_msg;

    return NULL;
}

FT_Library g_library;
FT_Face    g_face;

void debugPrintBitmap (FT_Bitmap bitmap)
{
    printf("\n");
    for (int y=0; y<bitmap.rows; y++)
    {
        for (int x=0; x<bitmap.width; x++)
        {
            unsigned char opacity = bitmap.buffer[x + y * bitmap.pitch];
            if (opacity == 0)
                printf("  ");
            else if (opacity < 64)
                printf("░░");
            else if (opacity < 128)
                printf("▒▒");
            else if (opacity < 192)
                printf("▓▓");
            else
                printf("██");
        }
        printf("\n");
    }
    printf("\n");
}

FT_Error initFreeType()
{
    CHECK_ERROR(FT_Init_FreeType(&g_library));

    CHECK_ERROR(FT_New_Face(g_library, "/System/Library/Fonts/HelveticaLight.ttf", 0, &g_face));

    CHECK_ERROR(FT_Set_Pixel_Sizes(g_face, 0, 32));

    // error = FT_Load_Char( face, text[n], FT_LOAD_RENDER );

    FT_ULong charcode = '@';
    FT_UInt glyph_index = FT_Get_Char_Index(g_face, charcode);

    CHECK_ERROR(FT_Load_Glyph(g_face, glyph_index, FT_LOAD_DEFAULT /*FT_LOAD_RENDER*/));

    FT_GlyphSlot glyph = g_face->glyph;

    FT_Render_Mode render_mode = FT_RENDER_MODE_NORMAL;
    CHECK_ERROR(FT_Render_Glyph(glyph, render_mode));

    FT_Bitmap bitmap = glyph->bitmap;

    // my_draw_bitmap( &slot->bitmap, pen_x + slot->bitmap_left, pen_y - slot->bitmap_top );
    // pen_x += slot->advance.x >> 6;

    debugPrintBitmap(bitmap);

    return 0;
}

FT_Error shutdownFreeType()
{
    CHECK_ERROR(FT_Done_FreeType(g_library));

    return 0;
}

FT_Int * libraryVersion()
{
    static FT_Int ver[3];
    FT_Library_Version(g_library, &ver[0], &ver[1], &ver[2]);
    return ver;
}

