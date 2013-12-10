
#include "ft2_interface.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

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

// Print buffer to the terminal using Unicode block drawing characters
void debugPrintBuffer(
    const unsigned char *buffer,
    unsigned int width,
    unsigned int height,
    unsigned int pitch)
{
    printf("\n");
    for (unsigned int y=0; y<height; y++)
    {
        for (unsigned int x=0; x<width; x++)
        {
            unsigned char opacity = buffer[x + y * pitch];
            if (opacity == 0)
                printf("· ");
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

void debugPrintBitmap(FT_Bitmap bitmap)
{
    debugPrintBuffer(bitmap.buffer, bitmap.width, bitmap.rows, bitmap.pitch);
}

FT_Error renderGlyph(
    FT_Face face,
    FT_ULong char_code,
    unsigned int *advance_horz_out,
    int *bearing_x_out,
    int *bearing_y_out,
    unsigned int *bitmap_width_out,
    unsigned int *bitmap_pitch_out,
    unsigned int *bitmap_rows_out,
    unsigned char **bitmap)
{
    // This combines FT_Get_Char_Index, FT_Load_Glyph and FT_Glyph_To_Bitmap
    CHECK_ERROR(FT_Load_Char(face, char_code, FT_LOAD_RENDER));

    (* advance_horz_out) = face->glyph->advance.x >> 6;
    (* bearing_x_out   ) = face->glyph->bitmap_left;
    (* bearing_y_out   ) = face->glyph->bitmap_top;
    (* bitmap_width_out) = face->glyph->bitmap.width;
    (* bitmap_pitch_out) = face->glyph->bitmap.pitch;
    (* bitmap_rows_out ) = face->glyph->bitmap.rows;
    (* bitmap          ) = face->glyph->bitmap.buffer;

    return 0;
}

void faceInfo(
    FT_Face face,
    FT_String **family_name_out,
    FT_String **style_name_out,
    FT_Long *num_glyphs_out,
    int *has_kerning_out,
    FT_Short *height_out,
    FT_Short *ascender_out,
    FT_Short *descender_out)
{
    (* family_name_out) = face->family_name;
    (* style_name_out ) = face->style_name;
    (* num_glyphs_out ) = face->num_glyphs;
    (* has_kerning_out) = FT_HAS_KERNING(face);
    if (face->size != NULL)
    {
        (* height_out   ) = face->size->metrics.height    >> 6;
        (* ascender_out ) = face->size->metrics.ascender  >> 6;
        (* descender_out) = face->size->metrics.descender >> 6;
    }
    else
        (* height_out) = (* ascender_out) = (* descender_out) = 0;
}

// Basic font rendering test / sandbox
FT_Error debugPrintTest(FT_Library library)
{
    FT_Face face;

    CHECK_ERROR(FT_New_Face(
            library,
            //"/Library/Fonts/Futura.ttc",
            "/System/Library/Fonts/HelveticaLight.ttf",
            //"/System/Library/Fonts/LucidaGrande.ttc",
            0,
            &face));

    if (FT_HAS_KERNING(face) == 0)
        printf("ERROR: Font has no kerning!\n");

    CHECK_ERROR(FT_Set_Pixel_Sizes(face, 0, 32));

    typedef struct
    {
        FT_UInt   index;
        FT_Vector pos;
        FT_Glyph  image;
    } Glyph;

    Glyph glyphs[32];

    const unsigned int text[] = { 'H', 'g', '!', '@', 0xC2 };
    const size_t ln = sizeof(text) / sizeof(unsigned int);

    int pen_x = 0, pen_y = 0;
    int error;
    FT_BBox bbox;

    bbox.xMin = bbox.yMin =  32000;
    bbox.xMax = bbox.yMax = -32000;

    for (unsigned int i=0, previous=0; i<ln; i++)
    {
        glyphs[i].index = FT_Get_Char_Index(face, text[i]);

         if (previous != 0 && glyphs[i].index != 0)
         {
             FT_Vector delta;
             CHECK_ERROR(FT_Get_Kerning(face, previous, glyphs[i].index, FT_KERNING_UNFITTED, &delta));
             //pen_x += delta.x >> 6;
             //printf("%d\n", delta.x);
         }

         glyphs[i].pos.x = pen_x;
         glyphs[i].pos.y = pen_y;

         error = FT_Load_Glyph(face, glyphs[i].index, FT_LOAD_DEFAULT);
         if (error)
             continue;

         error = FT_Get_Glyph(face->glyph, &glyphs[i].image);
         if (error)
             continue;

         FT_BBox glyph_bbox;
         FT_Glyph_Get_CBox(glyphs[i].image, FT_GLYPH_BBOX_PIXELS, &glyph_bbox);

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

         glyph_bbox.xMin += glyphs[i].pos.x;
         glyph_bbox.xMax += glyphs[i].pos.x;
         glyph_bbox.yMin += glyphs[i].pos.y;
         glyph_bbox.yMax += glyphs[i].pos.y;

         if (bbox.xMin > glyph_bbox.xMin)
             bbox.xMin = glyph_bbox.xMin;
         if (bbox.xMax < glyph_bbox.xMax)
             bbox.xMax = glyph_bbox.xMax;
         if (bbox.yMin > glyph_bbox.yMin)
             bbox.yMin = glyph_bbox.yMin;
         if (bbox.yMax < glyph_bbox.yMax)
             bbox.yMax = glyph_bbox.yMax;

         //FT_Glyph_Transform(glyph.image, NULL, &glyph.pos);
         pen_x += face->glyph->advance.x >> 6;

         previous = glyphs[i].index;
    }
    //printf("\n\n");

    if (bbox.xMin > bbox.xMax)
        bbox.xMin = bbox.yMin = bbox.xMax = bbox.yMax = 0;

    const int string_wdh = bbox.xMax - bbox.xMin;
    const int string_hgt = bbox.yMax - bbox.yMin;

    unsigned char *image = alloca(string_wdh * string_hgt);
    memset(image, 0, string_wdh * string_hgt);

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

    debugPrintBuffer(image, string_wdh, string_hgt, string_wdh);

    return 0;
}

