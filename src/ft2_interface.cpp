
#include "ft2_interface.h"

#include <cstdio>
#include <cstdlib>
#include <vector>
#include <cassert>

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

void debugPrintBitmap(
    unsigned char *buffer,
    unsigned int width,
    unsigned int height,
    unsigned int pitch)
{
    std::printf("\n");
    for (unsigned int y=0; y<height; y++)
    {
        for (unsigned int x=0; x<width; x++)
        {
            unsigned char opacity = buffer[x + y * pitch];
            if (opacity == 0)
                std::printf("· ");
            else if (opacity < 64)
                std::printf("░░");
            else if (opacity < 128)
                std::printf("▒▒");
            else if (opacity < 192)
                std::printf("▓▓");
            else
                std::printf("██");
        }
        std::printf("\n");
    }
    std::printf("\n");
}

void debugPrintBitmap(FT_Bitmap bitmap)
{
    debugPrintBitmap(bitmap.buffer, bitmap.width, bitmap.rows, bitmap.pitch);
}

struct Glyph
{
    FT_UInt   index;
    FT_Vector pos;
    FT_Glyph  image;
};

FT_Error initFreeType()
{
    CHECK_ERROR(FT_Init_FreeType(&g_library));

    CHECK_ERROR(FT_New_Face(
            g_library,
            //"/Library/Fonts/Futura.ttc",
            "/System/Library/Fonts/HelveticaLight.ttf",
            //"/Users/Tim/Downloads/news-cycle/newscycle-regular.ttf",
            //"/System/Library/Fonts/LucidaGrande.ttc",
            0,
            &g_face));

    if (FT_HAS_KERNING(g_face) == 0)
        printf("ERROR: Font has no kerning!\n");

    CHECK_ERROR(FT_Set_Pixel_Sizes(g_face, 0, 32));

    std::vector<Glyph> glyphs;

    const unsigned int text[] = { 'H', 'g', 0xB3, '!', 's', '@' };
    const size_t ln = sizeof(text) / sizeof(unsigned int);

    int pen_x = 0, pen_y = 0;
    int error;
    FT_BBox bbox;

    bbox.xMin = bbox.yMin =  32000;
    bbox.xMax = bbox.yMax = -32000;

    for (unsigned int i=0, previous=0; i<ln; i++)
    {
        Glyph glyph;
        glyph.index = FT_Get_Char_Index(g_face, text[i]);

         if (previous != 0 && glyph.index != 0)
         {
             FT_Vector delta;
             CHECK_ERROR(FT_Get_Kerning(g_face, previous, glyph.index, FT_KERNING_UNFITTED, &delta));
             //pen_x += delta.x >> 6;
             //printf("%d\n", delta.x);
         }

         glyph.pos.x = pen_x;
         glyph.pos.y = pen_y;

         error = FT_Load_Glyph(g_face, glyph.index, FT_LOAD_DEFAULT);
         if (error)
             continue;

         error = FT_Get_Glyph(g_face->glyph, &glyph.image);
         if (error)
             continue;

         FT_BBox glyph_bbox;
         FT_Glyph_Get_CBox(glyph.image, FT_GLYPH_BBOX_PIXELS, &glyph_bbox);

         /*
         printf("xmin:%i, xmax:%i, ymin:%i, ymax:%i, bearx:%i, beary:%i\n",
             glyph_bbox.xMin,
             glyph_bbox.xMax,
             glyph_bbox.yMin,
             glyph_bbox.yMax,
             g_face->glyph->metrics.horiBearingX >> 6,
             g_face->glyph->metrics.horiBearingY >> 6);
         */

         //glyph_bbox.xMax += g_face->glyph->metrics.horiBearingX >> 6;
         //glyph_bbox.yMax += g_face->glyph->metrics.horiBearingY >> 6;
         //glyph_bbox.yMin -= (glyph_bbox.yMax - glyph_bbox.yMin) - (g_face->glyph->metrics.horiBearingY >> 6);

         glyph_bbox.xMin += glyph.pos.x;
         glyph_bbox.xMax += glyph.pos.x;
         glyph_bbox.yMin += glyph.pos.y;
         glyph_bbox.yMax += glyph.pos.y;

         bbox.xMin = std::min(bbox.xMin, glyph_bbox.xMin);
         bbox.xMax = std::max(bbox.xMax, glyph_bbox.xMax);
         bbox.yMin = std::min(bbox.yMin, glyph_bbox.yMin);
         bbox.yMax = std::max(bbox.yMax, glyph_bbox.yMax);

         //FT_Glyph_Transform(glyph.image, NULL, &glyph.pos);
         pen_x += g_face->glyph->advance.x >> 6;

         previous = glyph.index;
         glyphs.push_back(glyph);
    }
    //printf("\n\n");

    if (bbox.xMin > bbox.xMax)
        bbox.xMin = bbox.yMin = bbox.xMax = bbox.yMax = 0;

    const int string_wdh = bbox.xMax - bbox.xMin;
    const int string_hgt = bbox.yMax - bbox.yMin;

    std::vector<unsigned char> image(string_wdh * string_hgt);
    std::fill(image.begin(), image.end(), 0);

    const int start_x = 0;
    const int start_y = 0;

    for (unsigned int i=0; i<ln; i++)
    {
        FT_Vector pen;
        pen.x = start_x + glyphs[i].pos.x;
        pen.y = start_y + glyphs[i].pos.y;

        error = FT_Glyph_To_Bitmap(&glyphs[i].image, FT_RENDER_MODE_NORMAL, NULL /*&pen*/, 1);
        if (error == 0)
        {
            FT_BitmapGlyph bit = (FT_BitmapGlyph) glyphs[i].image;
            //printf("(%i, %i)", bit->left, bit->top);

            for (int y=0; y<bit->bitmap.rows; y++)
            {
                for (int x=0; x<bit->bitmap.width; x++)
                {
                    image[(x + pen.x + bit->left - bbox.xMin) +
                          (y + pen.y + (string_hgt - bit->top) + bbox.yMin) * string_wdh] =
                          bit->bitmap.buffer[x + y * bit->bitmap.pitch];

                }
            }
        }

        FT_Done_Glyph(glyphs[i].image);
    }

    debugPrintBitmap(&image[0], string_wdh, string_hgt, string_wdh);

    /*
    // error = FT_Load_Char( face, text[n], FT_LOAD_RENDER );

    FT_ULong charcode = 0x2126;
    FT_UInt glyph_index = FT_Get_Char_Index(g_face, charcode);

    CHECK_ERROR(FT_Load_Glyph(g_face, glyph_index, FT_LOAD_DEFAULT)); // FT_LOAD_RENDER

    FT_GlyphSlot glyph = g_face->glyph;

    FT_Render_Mode render_mode = FT_RENDER_MODE_NORMAL;
    CHECK_ERROR(FT_Render_Glyph(glyph, render_mode));

    FT_Bitmap bitmap = glyph->bitmap;

    // my_draw_bitmap( &slot->bitmap, pen_x + slot->bitmap_left, pen_y - slot->bitmap_top );
    // pen_x += slot->advance.x >> 6;

    debugPrintBitmap(bitmap);
    */

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

