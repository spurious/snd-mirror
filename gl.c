/* gl.c: Guile and Ruby bindings for GL, GLU
 *   generated automatically from makegl.scm and gldata.scm
 *   needs xen.h
 *
 * reference args are ignored if passed, resultant values are returned in a list.
 * the various "v" forms are omitted for now -- are they needed in this context?
 * 'gl is added to *features*
 *
 * TODO: glGet* returning more than one value
 *
 * HISTORY:
 *     18-June:   GL 1.1 stubs.
 *     4-June:    GtkGLext support.
 *     20-May-02: initial version.
 */

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#if USE_GTK
  #include <gtk/gtkgl.h>
#endif
#include <GL/gl.h>
#include <GL/glu.h>
#if USE_MOTIF
  #include <GL/glx.h>
#endif
#include <string.h>

/* kludges for GL 1.1 */
#ifndef GL_VERSION_1_2
#define GL_TEXTURE_BINDING_3D 0
#define GL_TEXTURE0_ARB 0
#define GL_TEXTURE1_ARB 0
#define GL_TEXTURE2_ARB 0
#define GL_TEXTURE3_ARB 0
#define GL_TEXTURE4_ARB 0
#define GL_TEXTURE5_ARB 0
#define GL_TEXTURE6_ARB 0
#define GL_TEXTURE7_ARB 0
#define GL_TEXTURE8_ARB 0
#define GL_TEXTURE9_ARB 0
#define GL_TEXTURE10_ARB 0
#define GL_TEXTURE11_ARB 0
#define GL_TEXTURE12_ARB 0
#define GL_TEXTURE13_ARB 0
#define GL_TEXTURE14_ARB 0
#define GL_TEXTURE15_ARB 0
#define GL_TEXTURE16_ARB 0
#define GL_TEXTURE17_ARB 0
#define GL_TEXTURE18_ARB 0
#define GL_TEXTURE19_ARB 0
#define GL_TEXTURE20_ARB 0
#define GL_TEXTURE21_ARB 0
#define GL_TEXTURE22_ARB 0
#define GL_TEXTURE23_ARB 0
#define GL_TEXTURE24_ARB 0
#define GL_TEXTURE25_ARB 0
#define GL_TEXTURE26_ARB 0
#define GL_TEXTURE27_ARB 0
#define GL_TEXTURE28_ARB 0
#define GL_TEXTURE29_ARB 0
#define GL_TEXTURE30_ARB 0
#define GL_TEXTURE31_ARB 0
#define GL_ACTIVE_TEXTURE_ARB 0
#define GL_CLIENT_ACTIVE_TEXTURE_ARB 0
#define GL_MAX_TEXTURE_UNITS_ARB 0
#define GL_COLOR_TABLE_FORMAT_EXT 0
#define GL_COLOR_TABLE_WIDTH_EXT 0
#define GL_COLOR_TABLE_RED_SIZE_EXT 0
#define GL_COLOR_TABLE_GREEN_SIZE_EXT 0
#define GL_COLOR_TABLE_BLUE_SIZE_EXT 0
#define GL_COLOR_TABLE_ALPHA_SIZE_EXT 0
#define GL_COLOR_TABLE_LUMINANCE_SIZE_EXT 0
#define GL_COLOR_TABLE_INTENSITY_SIZE_EXT 0
#define GL_TEXTURE_INDEX_SIZE_EXT 0
#define GL_COLOR_INDEX1_EXT 0
#define GL_COLOR_INDEX2_EXT 0
#define GL_COLOR_INDEX4_EXT 0
#define GL_COLOR_INDEX8_EXT 0
#define GL_COLOR_INDEX12_EXT 0
#define GL_COLOR_INDEX16_EXT 0
#define GL_CLIP_VOLUME_CLIPPING_HINT_EXT 0
#define GL_INCR_WRAP_EXT 0
#define GL_DECR_WRAP_EXT 0
#define GL_NORMAL_MAP_NV 0
#define GL_REFLECTION_MAP_NV 0
#define GL_CONSTANT_COLOR 0
#define GL_ONE_MINUS_CONSTANT_COLOR 0
#define GL_CONSTANT_ALPHA 0
#define GL_ONE_MINUS_CONSTANT_ALPHA 0
#define GL_PACK_SKIP_IMAGES 0
#define GL_PACK_IMAGE_HEIGHT 0
#define GL_UNPACK_SKIP_IMAGES 0
#define GL_UNPACK_IMAGE_HEIGHT 0
#define GL_TEXTURE_3D 0
#define GL_PROXY_TEXTURE_3D 0
#define GL_TEXTURE_DEPTH 0
#define GL_TEXTURE_WRAP_R 0
#define GL_MAX_3D_TEXTURE_SIZE 0
#define GL_RESCALE_NORMAL 0
#define GL_CLAMP_TO_EDGE 0
#define GL_MAX_ELEMENTS_VERTICES 0
#define GL_MAX_ELEMENTS_INDICES 0
#define GL_BGR 0
#define GL_BGRA 0
#define GL_UNSIGNED_BYTE_3_3_2 0
#define GL_UNSIGNED_BYTE_2_3_3_REV 0
#define GL_UNSIGNED_SHORT_5_6_5 0
#define GL_UNSIGNED_SHORT_5_6_5_REV 0
#define GL_UNSIGNED_SHORT_4_4_4_4 0
#define GL_UNSIGNED_SHORT_4_4_4_4_REV 0
#define GL_UNSIGNED_SHORT_5_5_5_1 0
#define GL_UNSIGNED_SHORT_1_5_5_5_REV 0
#define GL_UNSIGNED_INT_8_8_8_8 0
#define GL_UNSIGNED_INT_8_8_8_8_REV 0
#define GL_UNSIGNED_INT_10_10_10_2 0
#define GL_UNSIGNED_INT_2_10_10_10_REV 0
#define GL_LIGHT_MODEL_COLOR_CONTROL 0
#define GL_SINGLE_COLOR 0
#define GL_SEPARATE_SPECULAR_COLOR 0
#define GL_TEXTURE_MIN_LOD 0
#define GL_TEXTURE_MAX_LOD 0
#define GL_TEXTURE_BASE_LEVEL 0
#define GL_TEXTURE_MAX_LEVEL 0
#define GL_COLOR_TABLE 0
#define GL_POST_CONVOLUTION_COLOR_TABLE 0
#define GL_POST_COLOR_MATRIX_COLOR_TABLE 0
#define GL_PROXY_COLOR_TABLE 0
#define GL_PROXY_POST_CONVOLUTION_COLOR_TABLE 0
#define GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE 0
#define GL_COLOR_TABLE_SCALE 0
#define GL_COLOR_TABLE_BIAS 0
#define GL_COLOR_TABLE_FORMAT 0
#define GL_COLOR_TABLE_WIDTH 0
#define GL_COLOR_TABLE_RED_SIZE 0
#define GL_COLOR_TABLE_GREEN_SIZE 0
#define GL_COLOR_TABLE_BLUE_SIZE 0
#define GL_COLOR_TABLE_ALPHA_SIZE 0
#define GL_COLOR_TABLE_LUMINANCE_SIZE 0
#define GL_COLOR_TABLE_INTENSITY_SIZE 0
#define GL_CONVOLUTION_1D 0
#define GL_CONVOLUTION_2D 0
#define GL_SEPARABLE_2D 0
#define GL_CONVOLUTION_BORDER_MODE 0
#define GL_CONVOLUTION_FILTER_SCALE 0
#define GL_CONVOLUTION_FILTER_BIAS 0
#define GL_REDUCE 0
#define GL_CONVOLUTION_FORMAT 0
#define GL_CONVOLUTION_WIDTH 0
#define GL_CONVOLUTION_HEIGHT 0
#define GL_MAX_CONVOLUTION_WIDTH 0
#define GL_MAX_CONVOLUTION_HEIGHT 0
#define GL_POST_CONVOLUTION_RED_SCALE 0
#define GL_POST_CONVOLUTION_GREEN_SCALE 0
#define GL_POST_CONVOLUTION_BLUE_SCALE 0
#define GL_POST_CONVOLUTION_ALPHA_SCALE 0
#define GL_POST_CONVOLUTION_RED_BIAS 0
#define GL_POST_CONVOLUTION_GREEN_BIAS 0
#define GL_POST_CONVOLUTION_BLUE_BIAS 0
#define GL_POST_CONVOLUTION_ALPHA_BIAS 0
#define GL_CONSTANT_BORDER 0
#define GL_REPLICATE_BORDER 0
#define GL_CONVOLUTION_BORDER_COLOR 0
#define GL_COLOR_MATRIX 0
#define GL_COLOR_MATRIX_STACK_DEPTH 0
#define GL_MAX_COLOR_MATRIX_STACK_DEPTH 0
#define GL_POST_COLOR_MATRIX_RED_SCALE 0
#define GL_POST_COLOR_MATRIX_GREEN_SCALE 0
#define GL_POST_COLOR_MATRIX_BLUE_SCALE 0
#define GL_POST_COLOR_MATRIX_ALPHA_SCALE 0
#define GL_POST_COLOR_MATRIX_RED_BIAS 0
#define GL_POST_COLOR_MATRIX_GREEN_BIAS 0
#define GL_POST_COLOR_MATRIX_BLUE_BIAS 0
#define GL_POST_COLOR_MATRIX_ALPHA_BIAS 0
#define GL_HISTOGRAM 0
#define GL_PROXY_HISTOGRAM 0
#define GL_HISTOGRAM_WIDTH 0
#define GL_HISTOGRAM_FORMAT 0
#define GL_HISTOGRAM_RED_SIZE 0
#define GL_HISTOGRAM_GREEN_SIZE 0
#define GL_HISTOGRAM_BLUE_SIZE 0
#define GL_HISTOGRAM_ALPHA_SIZE 0
#define GL_HISTOGRAM_LUMINANCE_SIZE 0
#define GL_HISTOGRAM_SINK 0
#define GL_MINMAX 0
#define GL_MINMAX_FORMAT 0
#define GL_MINMAX_SINK 0
#define GL_TABLE_TOO_LARGE 0
#define GL_BLEND_EQUATION 0
#define GL_MIN 0
#define GL_MAX 0
#define GL_FUNC_ADD 0
#define GL_FUNC_SUBTRACT 0
#define GL_FUNC_REVERSE_SUBTRACT 0
#define GL_BLEND_COLOR 0
#define GL_POINT_SIZE_MIN_EXT 0
#define GL_POINT_SIZE_MAX_EXT 0
#define GL_POINT_FADE_THRESHOLD_SIZE_EXT 0
#define GL_DISTANCE_ATTENUATION_EXT 0
#define GL_SHARED_TEXTURE_PALETTE_EXT 0
static void glDrawRangeElements( GLenum mode, GLuint start,GLuint end, GLsizei count, GLenum type, const GLvoid *indices ) {}
static void glTexImage3D( GLenum target, GLint level, GLint internalFormat, GLsizei width, GLsizei height,
                                      GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels ) {}
static void glTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth,
                                         GLenum format, GLenum type, const GLvoid *pixels) {}
static void glCopyTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                             GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height ) {}
static void glColorTable( GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table ) {}
static void glColorSubTable( GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data ) {}
static void glColorTableParameteriv(GLenum target, GLenum pname, const GLint *params) {}
static void glColorTableParameterfv(GLenum target, GLenum pname, const GLfloat *params) {}
static void glCopyColorSubTable( GLenum target, GLsizei start, GLint x, GLint y, GLsizei width ) {}
static void glCopyColorTable( GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width ) {}
static void glGetColorTable( GLenum target, GLenum format, GLenum type, GLvoid *table ) {}
static void glGetColorTableParameterfv( GLenum target, GLenum pname, GLfloat *params ) {}
static void glGetColorTableParameteriv( GLenum target, GLenum pname, GLint *params ) {}
static void glBlendEquation( GLenum mode ) {}
static void glBlendColor( GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha ) {}
static void glHistogram( GLenum target, GLsizei width, GLenum internalformat, GLboolean sink ) {}
static void glResetHistogram( GLenum target ) {}
static void glGetHistogram( GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values ) {}
static void glGetHistogramParameterfv( GLenum target, GLenum pname, GLfloat *params ) {}
static void glGetHistogramParameteriv( GLenum target, GLenum pname, GLint *params ) {}
static void glMinmax( GLenum target, GLenum internalformat, GLboolean sink ) {}
static void glResetMinmax( GLenum target ) {}
static void glGetMinmax( GLenum target, GLboolean reset, GLenum format, GLenum types, GLvoid *values ) {}
static void glGetMinmaxParameterfv( GLenum target, GLenum pname, GLfloat *params ) {}
static void glGetMinmaxParameteriv( GLenum target, GLenum pname, GLint *params ) {}
static void glConvolutionFilter1D( GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *image ) {}
static void glConvolutionFilter2D( GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *image ) {}
static void glConvolutionParameterf( GLenum target, GLenum pname, GLfloat params ) {}
static void glConvolutionParameterfv( GLenum target, GLenum pname, const GLfloat *params ) {}
static void glConvolutionParameteri( GLenum target, GLenum pname, GLint params ) {}
static void glConvolutionParameteriv( GLenum target, GLenum pname, const GLint *params ) {}
static void glCopyConvolutionFilter1D( GLenum target,GLenum internalformat, GLint x, GLint y, GLsizei width ) {}
static void glCopyConvolutionFilter2D( GLenum target,GLenum internalformat, GLint x, GLint y, GLsizei width,GLsizei height) {}
static void glGetConvolutionFilter( GLenum target, GLenum format,GLenum type, GLvoid *image ) {}
static void glGetConvolutionParameterfv( GLenum target, GLenum pname,GLfloat *params ) {}
static void glGetConvolutionParameteriv( GLenum target, GLenum pname,GLint *params ) {}
static void glSeparableFilter2D( GLenum target,GLenum internalformat, GLsizei width, GLsizei height, GLenum format,
				 GLenum type, const GLvoid *row, const GLvoid *column ) {}
static void glGetSeparableFilter( GLenum target, GLenum format,GLenum type, GLvoid *row, GLvoid *column, GLvoid *span ) {}
static void glActiveTextureARB(GLenum texture) {}
static void glClientActiveTextureARB(GLenum texture) {}
static void glMultiTexCoord1dARB(GLenum target, GLdouble s) {}
static void glMultiTexCoord1dvARB(GLenum target, const GLdouble *v) {}
static void glMultiTexCoord1fARB(GLenum target, GLfloat s) {}
static void glMultiTexCoord1fvARB(GLenum target, const GLfloat *v) {}
static void glMultiTexCoord1iARB(GLenum target, GLint s) {}
static void glMultiTexCoord1ivARB(GLenum target, const GLint *v) {}
static void glMultiTexCoord1sARB(GLenum target, GLshort s) {}
static void glMultiTexCoord1svARB(GLenum target, const GLshort *v) {}
static void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t) {}
static void glMultiTexCoord2dvARB(GLenum target, const GLdouble *v) {}
static void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t) {}
static void glMultiTexCoord2fvARB(GLenum target, const GLfloat *v) {}
static void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t) {}
static void glMultiTexCoord2ivARB(GLenum target, const GLint *v) {}
static void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t) {}
static void glMultiTexCoord2svARB(GLenum target, const GLshort *v) {}
static void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r) {}
static void glMultiTexCoord3dvARB(GLenum target, const GLdouble *v) {}
static void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r) {}
static void glMultiTexCoord3fvARB(GLenum target, const GLfloat *v) {}
static void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r) {}
static void glMultiTexCoord3ivARB(GLenum target, const GLint *v) {}
static void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r) {}
static void glMultiTexCoord3svARB(GLenum target, const GLshort *v) {}
static void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q) {}
static void glMultiTexCoord4dvARB(GLenum target, const GLdouble *v) {}
static void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q) {}
static void glMultiTexCoord4fvARB(GLenum target, const GLfloat *v) {}
static void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q) {}
static void glMultiTexCoord4ivARB(GLenum target, const GLint *v) {}
static void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q) {}
static void glMultiTexCoord4svARB(GLenum target, const GLshort *v) {}
static void glLockArraysEXT( GLint first, GLsizei count ) {}
static void glUnlockArraysEXT( void ) {}
static void glPointParameterfEXT( GLenum pname, GLfloat param ) {}
static void glPointParameterfvEXT( GLenum pname, const GLfloat *params ) {}
static void glColorTableEXT( GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table ) {}
static void glColorSubTableEXT( GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data ) {}
static void glGetColorTableEXT( GLenum target, GLenum format, GLenum type, GLvoid *table ) {}
static void glGetColorTableParameterfvEXT( GLenum target, GLenum pname, GLfloat *params ) {}
static void glGetColorTableParameterivEXT( GLenum target, GLenum pname, GLint *params ) {}
#endif
/* end of 1.1 kludgery */

#if USE_SND
  /* USE_SND causes xm to use Snd's error handlers which are much smarter than xen's fallback versions */
  #include "snd.h"
#else
  #include "xen.h"
#endif
#ifndef CALLOC
  #define CALLOC(a, b)  calloc((size_t)(a), (size_t)(b))
  #define FREE(a)       free(a)
#endif

/* prefix for all names */
#if HAVE_GUILE
  #define XL_PRE "|"
  #define XL_POST ""
#else
/* for Ruby, XG PRE needs to be uppercase */
  #define XL_PRE "R"
  #define XL_POST ""
#endif

#define WRAP_FOR_XEN(Name, Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), C_TO_XEN_ULONG((unsigned long)Value))
#define WRAP_P(Name, Value) (XEN_LIST_P(Value) && \
                            (XEN_LIST_LENGTH(Value) >= 2) && \
                            (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                            (strcmp(Name, XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define XL_TYPE(Name, XType) \
  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \
  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \
  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}
#define XL_TYPE_1(Name, XType) \
  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \
  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}

#define XL_TYPE_PTR(Name, XType) \
  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \
  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \
  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));} /* if NULL ok, should be explicit */
#define XL_TYPE_PTR_1(Name, XType) \
  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \
  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));} /* if NULL ok, should be explicit */


/* ---------------------------------------- types ---------------------------------------- */

#if USE_MOTIF
XL_TYPE(XVisualInfo, XVisualInfo*)
XL_TYPE_1(Display, Display*)
#define C_TO_XEN_int(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_int(Arg) (int)(XEN_TO_C_INT(Arg))
#define XEN_int_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_1(int_, int*)
XL_TYPE(GLXContext, GLXContext)
#define C_TO_XEN_ulong(Arg) C_TO_XEN_ULONG(Arg)
#define XEN_TO_C_ulong(Arg) (ulong)(XEN_TO_C_ULONG(Arg))
#define XEN_ulong_P(Arg) XEN_ULONG_P(Arg)
#define C_TO_XEN_Bool(Arg) C_TO_XEN_BOOLEAN(Arg)
#define XEN_TO_C_Bool(Arg) (Bool)(XEN_TO_C_BOOLEAN(Arg))
#define XEN_Bool_P(Arg) XEN_BOOLEAN_P(Arg)
XL_TYPE(GLXPixmap, GLXPixmap)
XL_TYPE_1(Pixmap, Pixmap)
XL_TYPE(Window, Window)
XL_TYPE_1(Font, Font)
#define C_TO_XEN_char_(Arg) C_TO_XEN_STRING(Arg)
#define XEN_TO_C_char_(Arg) (char_)(XEN_TO_C_STRING(Arg))
#define XEN_char__P(Arg) XEN_STRING_P(Arg)
#endif
#define C_TO_XEN_GLfloat(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLfloat(Arg) (GLfloat)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLfloat_P(Arg) XEN_NUMBER_P(Arg)
#define C_TO_XEN_GLclampf(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLclampf(Arg) (GLclampf)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLclampf_P(Arg) XEN_NUMBER_P(Arg)
#define C_TO_XEN_GLbitfield(Arg) C_TO_XEN_ULONG(Arg)
#define XEN_TO_C_GLbitfield(Arg) (GLbitfield)(XEN_TO_C_ULONG(Arg))
#define XEN_GLbitfield_P(Arg) XEN_ULONG_P(Arg)
#define C_TO_XEN_GLuint(Arg) C_TO_XEN_ULONG(Arg)
#define XEN_TO_C_GLuint(Arg) (GLuint)(XEN_TO_C_ULONG(Arg))
#define XEN_GLuint_P(Arg) XEN_ULONG_P(Arg)
#define C_TO_XEN_GLboolean(Arg) C_TO_XEN_BOOLEAN(Arg)
#define XEN_TO_C_GLboolean(Arg) (GLboolean)(XEN_TO_C_BOOLEAN(Arg))
#define XEN_GLboolean_P(Arg) XEN_BOOLEAN_P(Arg)
#define C_TO_XEN_GLenum(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLenum(Arg) (GLenum)(XEN_TO_C_INT(Arg))
#define XEN_GLenum_P(Arg) XEN_INTEGER_P(Arg)
#define C_TO_XEN_GLint(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLint(Arg) (GLint)(XEN_TO_C_INT(Arg))
#define XEN_GLint_P(Arg) XEN_INTEGER_P(Arg)
#define C_TO_XEN_GLushort(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLushort(Arg) (GLushort)(XEN_TO_C_INT(Arg))
#define XEN_GLushort_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_1(GLubyte_, GLubyte*)
#define C_TO_XEN_GLubyte(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLubyte(Arg) (GLubyte)(XEN_TO_C_INT(Arg))
#define XEN_GLubyte_P(Arg) XEN_INTEGER_P(Arg)
#define C_TO_XEN_GLsizei(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLsizei(Arg) (GLsizei)(XEN_TO_C_INT(Arg))
#define XEN_GLsizei_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_1(GLdouble_, GLdouble*)
#define C_TO_XEN_GLdouble(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLdouble(Arg) (GLdouble)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLdouble_P(Arg) XEN_NUMBER_P(Arg)
#define C_TO_XEN_constchar_(Arg) C_TO_XEN_STRING((char *)(Arg))
#define XEN_TO_C_constchar_(Arg) (constchar_)(XEN_TO_C_STRING(Arg))
#define XEN_constchar__P(Arg) XEN_STRING_P(Arg)
#define C_TO_XEN_GLclampd(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLclampd(Arg) (GLclampd)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLclampd_P(Arg) XEN_NUMBER_P(Arg)
XL_TYPE_PTR_1(GLfloat_, GLfloat*)
XL_TYPE_PTR_1(GLvoid_, GLvoid*)
#define C_TO_XEN_GLshort(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLshort(Arg) (GLshort)(XEN_TO_C_INT(Arg))
#define XEN_GLshort_P(Arg) XEN_INTEGER_P(Arg)
#define C_TO_XEN_GLbyte(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_GLbyte(Arg) (GLbyte)(XEN_TO_C_INT(Arg))
#define XEN_GLbyte_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR(void_, void*)
XL_TYPE_PTR_1(GLuint_, GLuint*)
XL_TYPE_PTR_1(GLboolean_, GLboolean*)
#ifdef GLU_VERSION_1_2
XL_TYPE_PTR(GLUtesselator_, GLUtesselator*)
#endif
XL_TYPE_PTR_1(GLint_, GLint*)
XL_TYPE_PTR_1(GLshort_, GLshort*)


/* ---------------------------------------- functions ---------------------------------------- */

#if USE_MOTIF
static XEN gxg_glXChooseVisual(XEN dpy, XEN screen, XEN attribList)
{
  #define H_glXChooseVisual "XVisualInfo* glXChooseVisual(Display* dpy, int screen, int* attribList)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXChooseVisual", "Display*");
  XEN_ASSERT_TYPE(XEN_int_P(screen), screen, 2, "glXChooseVisual", "int");
  XEN_ASSERT_TYPE(XEN_int__P(attribList), attribList, 3, "glXChooseVisual", "int*");
  return(C_TO_XEN_XVisualInfo(glXChooseVisual(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen), XEN_TO_C_int_(attribList))));
}

static XEN gxg_glXCopyContext(XEN dpy, XEN src, XEN dst, XEN mask)
{
  #define H_glXCopyContext "void glXCopyContext(Display* dpy, GLXContext src, GLXContext dst, ulong mask)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXCopyContext", "Display*");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(src), src, 2, "glXCopyContext", "GLXContext");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(dst), dst, 3, "glXCopyContext", "GLXContext");
  XEN_ASSERT_TYPE(XEN_ulong_P(mask), mask, 4, "glXCopyContext", "ulong");
  glXCopyContext(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(src), XEN_TO_C_GLXContext(dst), XEN_TO_C_ulong(mask));
  return(XEN_FALSE);
}

static XEN gxg_glXCreateContext(XEN dpy, XEN vis, XEN shareList, XEN direct)
{
  #define H_glXCreateContext "GLXContext glXCreateContext(Display* dpy, XVisualInfo* vis, GLXContext shareList, \
Bool direct)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXCreateContext", "Display*");
  XEN_ASSERT_TYPE(XEN_XVisualInfo_P(vis), vis, 2, "glXCreateContext", "XVisualInfo*");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(shareList), shareList, 3, "glXCreateContext", "GLXContext");
  XEN_ASSERT_TYPE(XEN_Bool_P(direct), direct, 4, "glXCreateContext", "Bool");
  return(C_TO_XEN_GLXContext(glXCreateContext(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_GLXContext(shareList), 
                                              XEN_TO_C_Bool(direct))));
}

static XEN gxg_glXCreateGLXPixmap(XEN dpy, XEN vis, XEN pixmap)
{
  #define H_glXCreateGLXPixmap "GLXPixmap glXCreateGLXPixmap(Display* dpy, XVisualInfo* vis, Pixmap pixmap)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXCreateGLXPixmap", "Display*");
  XEN_ASSERT_TYPE(XEN_XVisualInfo_P(vis), vis, 2, "glXCreateGLXPixmap", "XVisualInfo*");
  XEN_ASSERT_TYPE(XEN_Pixmap_P(pixmap), pixmap, 3, "glXCreateGLXPixmap", "Pixmap");
  return(C_TO_XEN_GLXPixmap(glXCreateGLXPixmap(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_Pixmap(pixmap))));
}

static XEN gxg_glXDestroyContext(XEN dpy, XEN ctx)
{
  #define H_glXDestroyContext "void glXDestroyContext(Display* dpy, GLXContext ctx)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXDestroyContext", "Display*");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(ctx), ctx, 2, "glXDestroyContext", "GLXContext");
  glXDestroyContext(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(ctx));
  return(XEN_FALSE);
}

static XEN gxg_glXDestroyGLXPixmap(XEN dpy, XEN pix)
{
  #define H_glXDestroyGLXPixmap "void glXDestroyGLXPixmap(Display* dpy, GLXPixmap pix)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXDestroyGLXPixmap", "Display*");
  XEN_ASSERT_TYPE(XEN_GLXPixmap_P(pix), pix, 2, "glXDestroyGLXPixmap", "GLXPixmap");
  glXDestroyGLXPixmap(XEN_TO_C_Display(dpy), XEN_TO_C_GLXPixmap(pix));
  return(XEN_FALSE);
}

static XEN gxg_glXGetConfig(XEN dpy, XEN vis, XEN attrib, XEN value)
{
  #define H_glXGetConfig "int glXGetConfig(Display* dpy, XVisualInfo* vis, int attrib, int* [value])"
  int ref_value;
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXGetConfig", "Display*");
  XEN_ASSERT_TYPE(XEN_XVisualInfo_P(vis), vis, 2, "glXGetConfig", "XVisualInfo*");
  XEN_ASSERT_TYPE(XEN_int_P(attrib), attrib, 3, "glXGetConfig", "int");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_int(glXGetConfig(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_int(attrib), &ref_value));
    return(XEN_LIST_2(result, C_TO_XEN_int(ref_value)));
   }
}

static XEN gxg_glXGetCurrentContext(void)
{
  #define H_glXGetCurrentContext "GLXContext glXGetCurrentContext( void)"
  return(C_TO_XEN_GLXContext(glXGetCurrentContext()));
}

static XEN gxg_glXGetCurrentDrawable(void)
{
  #define H_glXGetCurrentDrawable "Window glXGetCurrentDrawable( void)"
  return(C_TO_XEN_Window(glXGetCurrentDrawable()));
}

static XEN gxg_glXIsDirect(XEN dpy, XEN ctx)
{
  #define H_glXIsDirect "Bool glXIsDirect(Display* dpy, GLXContext ctx)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXIsDirect", "Display*");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(ctx), ctx, 2, "glXIsDirect", "GLXContext");
  return(C_TO_XEN_Bool(glXIsDirect(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(ctx))));
}

static XEN gxg_glXMakeCurrent(XEN dpy, XEN drawable, XEN ctx)
{
  #define H_glXMakeCurrent "Bool glXMakeCurrent(Display* dpy, Window drawable, GLXContext ctx)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXMakeCurrent", "Display*");
  XEN_ASSERT_TYPE(XEN_Window_P(drawable), drawable, 2, "glXMakeCurrent", "Window");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(ctx), ctx, 3, "glXMakeCurrent", "GLXContext");
  return(C_TO_XEN_Bool(glXMakeCurrent(XEN_TO_C_Display(dpy), XEN_TO_C_Window(drawable), XEN_TO_C_GLXContext(ctx))));
}

static XEN gxg_glXQueryExtension(XEN dpy, XEN errorBase, XEN eventBase)
{
  #define H_glXQueryExtension "Bool glXQueryExtension(Display* dpy, int* [errorBase], int* [eventBase])"
  int ref_errorBase;
  int ref_eventBase;
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryExtension", "Display*");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_Bool(glXQueryExtension(XEN_TO_C_Display(dpy), &ref_errorBase, &ref_eventBase));
    return(XEN_LIST_3(result, C_TO_XEN_int(ref_errorBase), C_TO_XEN_int(ref_eventBase)));
   }
}

static XEN gxg_glXQueryVersion(XEN dpy, XEN major, XEN minor)
{
  #define H_glXQueryVersion "Bool glXQueryVersion(Display* dpy, int* [major], int* [minor])"
  int ref_major;
  int ref_minor;
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryVersion", "Display*");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_Bool(glXQueryVersion(XEN_TO_C_Display(dpy), &ref_major, &ref_minor));
    return(XEN_LIST_3(result, C_TO_XEN_int(ref_major), C_TO_XEN_int(ref_minor)));
   }
}

static XEN gxg_glXSwapBuffers(XEN dpy, XEN drawable)
{
  #define H_glXSwapBuffers "void glXSwapBuffers(Display* dpy, Window drawable)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXSwapBuffers", "Display*");
  XEN_ASSERT_TYPE(XEN_Window_P(drawable), drawable, 2, "glXSwapBuffers", "Window");
  glXSwapBuffers(XEN_TO_C_Display(dpy), XEN_TO_C_Window(drawable));
  return(XEN_FALSE);
}

static XEN gxg_glXUseXFont(XEN font, XEN first, XEN count, XEN listBase)
{
  #define H_glXUseXFont "void glXUseXFont(Font font, int first, int count, int listBase)"
  XEN_ASSERT_TYPE(XEN_Font_P(font), font, 1, "glXUseXFont", "Font");
  XEN_ASSERT_TYPE(XEN_int_P(first), first, 2, "glXUseXFont", "int");
  XEN_ASSERT_TYPE(XEN_int_P(count), count, 3, "glXUseXFont", "int");
  XEN_ASSERT_TYPE(XEN_int_P(listBase), listBase, 4, "glXUseXFont", "int");
  glXUseXFont(XEN_TO_C_Font(font), XEN_TO_C_int(first), XEN_TO_C_int(count), XEN_TO_C_int(listBase));
  return(XEN_FALSE);
}

static XEN gxg_glXWaitGL(void)
{
  #define H_glXWaitGL "void glXWaitGL( void)"
  glXWaitGL();
  return(XEN_FALSE);
}

static XEN gxg_glXWaitX(void)
{
  #define H_glXWaitX "void glXWaitX( void)"
  glXWaitX();
  return(XEN_FALSE);
}

static XEN gxg_glXGetClientString(XEN dpy, XEN name)
{
  #define H_glXGetClientString "char* glXGetClientString(Display* dpy, int name)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXGetClientString", "Display*");
  XEN_ASSERT_TYPE(XEN_int_P(name), name, 2, "glXGetClientString", "int");
  return(C_TO_XEN_char_(glXGetClientString(XEN_TO_C_Display(dpy), XEN_TO_C_int(name))));
}

static XEN gxg_glXQueryServerString(XEN dpy, XEN screen, XEN name)
{
  #define H_glXQueryServerString "char* glXQueryServerString(Display* dpy, int screen, int name)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryServerString", "Display*");
  XEN_ASSERT_TYPE(XEN_int_P(screen), screen, 2, "glXQueryServerString", "int");
  XEN_ASSERT_TYPE(XEN_int_P(name), name, 3, "glXQueryServerString", "int");
  return(C_TO_XEN_char_(glXQueryServerString(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen), XEN_TO_C_int(name))));
}

static XEN gxg_glXQueryExtensionsString(XEN dpy, XEN screen)
{
  #define H_glXQueryExtensionsString "char* glXQueryExtensionsString(Display* dpy, int screen)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryExtensionsString", "Display*");
  XEN_ASSERT_TYPE(XEN_int_P(screen), screen, 2, "glXQueryExtensionsString", "int");
  return(C_TO_XEN_char_(glXQueryExtensionsString(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen))));
}

#endif
static XEN gxg_glClearIndex(XEN c)
{
  #define H_glClearIndex "void glClearIndex(GLfloat c)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(c), c, 1, "glClearIndex", "GLfloat");
  glClearIndex(XEN_TO_C_GLfloat(c));
  return(XEN_FALSE);
}

static XEN gxg_glClearColor(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glClearColor "void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"
  XEN_ASSERT_TYPE(XEN_GLclampf_P(red), red, 1, "glClearColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(green), green, 2, "glClearColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(blue), blue, 3, "glClearColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(alpha), alpha, 4, "glClearColor", "GLclampf");
  glClearColor(XEN_TO_C_GLclampf(red), XEN_TO_C_GLclampf(green), XEN_TO_C_GLclampf(blue), XEN_TO_C_GLclampf(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glClear(XEN mask)
{
  #define H_glClear "void glClear(GLbitfield mask)"
  XEN_ASSERT_TYPE(XEN_GLbitfield_P(mask), mask, 1, "glClear", "GLbitfield");
  glClear(XEN_TO_C_GLbitfield(mask));
  return(XEN_FALSE);
}

static XEN gxg_glIndexMask(XEN mask)
{
  #define H_glIndexMask "void glIndexMask(GLuint mask)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(mask), mask, 1, "glIndexMask", "GLuint");
  glIndexMask(XEN_TO_C_GLuint(mask));
  return(XEN_FALSE);
}

static XEN gxg_glColorMask(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColorMask "void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)"
  XEN_ASSERT_TYPE(XEN_GLboolean_P(red), red, 1, "glColorMask", "GLboolean");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(green), green, 2, "glColorMask", "GLboolean");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(blue), blue, 3, "glColorMask", "GLboolean");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(alpha), alpha, 4, "glColorMask", "GLboolean");
  glColorMask(XEN_TO_C_GLboolean(red), XEN_TO_C_GLboolean(green), XEN_TO_C_GLboolean(blue), XEN_TO_C_GLboolean(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glAlphaFunc(XEN func, XEN ref)
{
  #define H_glAlphaFunc "void glAlphaFunc(GLenum func, GLclampf ref)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(func), func, 1, "glAlphaFunc", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(ref), ref, 2, "glAlphaFunc", "GLclampf");
  glAlphaFunc(XEN_TO_C_GLenum(func), XEN_TO_C_GLclampf(ref));
  return(XEN_FALSE);
}

static XEN gxg_glBlendFunc(XEN sfactor, XEN dfactor)
{
  #define H_glBlendFunc "void glBlendFunc(GLenum sfactor, GLenum dfactor)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(sfactor), sfactor, 1, "glBlendFunc", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(dfactor), dfactor, 2, "glBlendFunc", "GLenum");
  glBlendFunc(XEN_TO_C_GLenum(sfactor), XEN_TO_C_GLenum(dfactor));
  return(XEN_FALSE);
}

static XEN gxg_glLogicOp(XEN opcode)
{
  #define H_glLogicOp "void glLogicOp(GLenum opcode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(opcode), opcode, 1, "glLogicOp", "GLenum");
  glLogicOp(XEN_TO_C_GLenum(opcode));
  return(XEN_FALSE);
}

static XEN gxg_glCullFace(XEN mode)
{
  #define H_glCullFace "void glCullFace(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glCullFace", "GLenum");
  glCullFace(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glFrontFace(XEN mode)
{
  #define H_glFrontFace "void glFrontFace(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glFrontFace", "GLenum");
  glFrontFace(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glPointSize(XEN size)
{
  #define H_glPointSize "void glPointSize(GLfloat size)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(size), size, 1, "glPointSize", "GLfloat");
  glPointSize(XEN_TO_C_GLfloat(size));
  return(XEN_FALSE);
}

static XEN gxg_glLineWidth(XEN width)
{
  #define H_glLineWidth "void glLineWidth(GLfloat width)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(width), width, 1, "glLineWidth", "GLfloat");
  glLineWidth(XEN_TO_C_GLfloat(width));
  return(XEN_FALSE);
}

static XEN gxg_glLineStipple(XEN factor, XEN pattern)
{
  #define H_glLineStipple "void glLineStipple(GLint factor, GLushort pattern)"
  XEN_ASSERT_TYPE(XEN_GLint_P(factor), factor, 1, "glLineStipple", "GLint");
  XEN_ASSERT_TYPE(XEN_GLushort_P(pattern), pattern, 2, "glLineStipple", "GLushort");
  glLineStipple(XEN_TO_C_GLint(factor), XEN_TO_C_GLushort(pattern));
  return(XEN_FALSE);
}

static XEN gxg_glPolygonMode(XEN face, XEN mode)
{
  #define H_glPolygonMode "void glPolygonMode(GLenum face, GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glPolygonMode", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 2, "glPolygonMode", "GLenum");
  glPolygonMode(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glPolygonOffset(XEN factor, XEN units)
{
  #define H_glPolygonOffset "void glPolygonOffset(GLfloat factor, GLfloat units)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(factor), factor, 1, "glPolygonOffset", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(units), units, 2, "glPolygonOffset", "GLfloat");
  glPolygonOffset(XEN_TO_C_GLfloat(factor), XEN_TO_C_GLfloat(units));
  return(XEN_FALSE);
}

static XEN gxg_glPolygonStipple(XEN mask)
{
  #define H_glPolygonStipple "void glPolygonStipple(GLubyte* mask)"
  XEN_ASSERT_TYPE(XEN_GLubyte__P(mask), mask, 1, "glPolygonStipple", "GLubyte*");
  glPolygonStipple(XEN_TO_C_GLubyte_(mask));
  return(XEN_FALSE);
}

static XEN gxg_glGetPolygonStipple(XEN mask)
{
  #define H_glGetPolygonStipple "void glGetPolygonStipple(GLubyte* [mask])"
  GLubyte ref_mask;
  glGetPolygonStipple(&ref_mask);
  return(XEN_LIST_1(C_TO_XEN_GLubyte(ref_mask)));
}

static XEN gxg_glEdgeFlag(XEN flag)
{
  #define H_glEdgeFlag "void glEdgeFlag(GLboolean flag)"
  XEN_ASSERT_TYPE(XEN_GLboolean_P(flag), flag, 1, "glEdgeFlag", "GLboolean");
  glEdgeFlag(XEN_TO_C_GLboolean(flag));
  return(XEN_FALSE);
}

static XEN gxg_glScissor(XEN x, XEN y, XEN width, XEN height)
{
  #define H_glScissor "void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glScissor", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glScissor", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glScissor", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glScissor", "GLsizei");
  glScissor(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glClipPlane(XEN plane, XEN equation)
{
  #define H_glClipPlane "void glClipPlane(GLenum plane, GLdouble* equation)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(plane), plane, 1, "glClipPlane", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(equation), equation, 2, "glClipPlane", "GLdouble*");
  glClipPlane(XEN_TO_C_GLenum(plane), XEN_TO_C_GLdouble_(equation));
  return(XEN_FALSE);
}

static XEN gxg_glGetClipPlane(XEN plane, XEN equation)
{
  #define H_glGetClipPlane "void glGetClipPlane(GLenum plane, GLdouble* [equation])"
  GLdouble ref_equation;
  XEN_ASSERT_TYPE(XEN_GLenum_P(plane), plane, 1, "glGetClipPlane", "GLenum");
  glGetClipPlane(XEN_TO_C_GLenum(plane), &ref_equation);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_equation)));
}

static XEN gxg_glDrawBuffer(XEN mode)
{
  #define H_glDrawBuffer "void glDrawBuffer(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glDrawBuffer", "GLenum");
  glDrawBuffer(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glReadBuffer(XEN mode)
{
  #define H_glReadBuffer "void glReadBuffer(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glReadBuffer", "GLenum");
  glReadBuffer(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glEnable(XEN cap)
{
  #define H_glEnable "void glEnable(GLenum cap)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(cap), cap, 1, "glEnable", "GLenum");
  glEnable(XEN_TO_C_GLenum(cap));
  return(XEN_FALSE);
}

static XEN gxg_glDisable(XEN cap)
{
  #define H_glDisable "void glDisable(GLenum cap)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(cap), cap, 1, "glDisable", "GLenum");
  glDisable(XEN_TO_C_GLenum(cap));
  return(XEN_FALSE);
}

static XEN gxg_glIsEnabled(XEN cap)
{
  #define H_glIsEnabled "GLboolean glIsEnabled(GLenum cap)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(cap), cap, 1, "glIsEnabled", "GLenum");
  return(C_TO_XEN_GLboolean(glIsEnabled(XEN_TO_C_GLenum(cap))));
}

static XEN gxg_glEnableClientState(XEN cap)
{
  #define H_glEnableClientState "void glEnableClientState(GLenum cap)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(cap), cap, 1, "glEnableClientState", "GLenum");
  glEnableClientState(XEN_TO_C_GLenum(cap));
  return(XEN_FALSE);
}

static XEN gxg_glDisableClientState(XEN cap)
{
  #define H_glDisableClientState "void glDisableClientState(GLenum cap)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(cap), cap, 1, "glDisableClientState", "GLenum");
  glDisableClientState(XEN_TO_C_GLenum(cap));
  return(XEN_FALSE);
}

static XEN gxg_glGetBooleanv(XEN pname, XEN params)
{
  #define H_glGetBooleanv "void glGetBooleanv(GLenum pname, GLboolean* [params])"
  GLboolean ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetBooleanv", "GLenum");
  glGetBooleanv(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLboolean(ref_params)));
}

static XEN gxg_glGetDoublev(XEN pname, XEN params)
{
  #define H_glGetDoublev "void glGetDoublev(GLenum pname, GLdouble* [params])"
  GLdouble ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetDoublev", "GLenum");
  glGetDoublev(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_params)));
}

static XEN gxg_glGetFloatv(XEN pname, XEN params)
{
  #define H_glGetFloatv "void glGetFloatv(GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetFloatv", "GLenum");
  glGetFloatv(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetIntegerv(XEN pname, XEN params)
{
  #define H_glGetIntegerv "void glGetIntegerv(GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetIntegerv", "GLenum");
  glGetIntegerv(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glPushAttrib(XEN mask)
{
  #define H_glPushAttrib "void glPushAttrib(GLbitfield mask)"
  XEN_ASSERT_TYPE(XEN_GLbitfield_P(mask), mask, 1, "glPushAttrib", "GLbitfield");
  glPushAttrib(XEN_TO_C_GLbitfield(mask));
  return(XEN_FALSE);
}

static XEN gxg_glPopAttrib(void)
{
  #define H_glPopAttrib "void glPopAttrib( void)"
  glPopAttrib();
  return(XEN_FALSE);
}

static XEN gxg_glPushClientAttrib(XEN mask)
{
  #define H_glPushClientAttrib "void glPushClientAttrib(GLbitfield mask)"
  XEN_ASSERT_TYPE(XEN_GLbitfield_P(mask), mask, 1, "glPushClientAttrib", "GLbitfield");
  glPushClientAttrib(XEN_TO_C_GLbitfield(mask));
  return(XEN_FALSE);
}

static XEN gxg_glPopClientAttrib(void)
{
  #define H_glPopClientAttrib "void glPopClientAttrib( void)"
  glPopClientAttrib();
  return(XEN_FALSE);
}

static XEN gxg_glRenderMode(XEN mode)
{
  #define H_glRenderMode "GLint glRenderMode(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glRenderMode", "GLenum");
  return(C_TO_XEN_GLint(glRenderMode(XEN_TO_C_GLenum(mode))));
}

static XEN gxg_glGetError(void)
{
  #define H_glGetError "GLenum glGetError( void)"
  return(C_TO_XEN_GLenum(glGetError()));
}

static XEN gxg_glGetString(XEN name)
{
  #define H_glGetString "constchar* glGetString(GLenum name)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(name), name, 1, "glGetString", "GLenum");
  return(C_TO_XEN_constchar_(glGetString(XEN_TO_C_GLenum(name))));
}

static XEN gxg_glFinish(void)
{
  #define H_glFinish "void glFinish( void)"
  glFinish();
  return(XEN_FALSE);
}

static XEN gxg_glFlush(void)
{
  #define H_glFlush "void glFlush( void)"
  glFlush();
  return(XEN_FALSE);
}

static XEN gxg_glHint(XEN target, XEN mode)
{
  #define H_glHint "void glHint(GLenum target, GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glHint", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 2, "glHint", "GLenum");
  glHint(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glClearDepth(XEN depth)
{
  #define H_glClearDepth "void glClearDepth(GLclampd depth)"
  XEN_ASSERT_TYPE(XEN_GLclampd_P(depth), depth, 1, "glClearDepth", "GLclampd");
  glClearDepth(XEN_TO_C_GLclampd(depth));
  return(XEN_FALSE);
}

static XEN gxg_glDepthFunc(XEN func)
{
  #define H_glDepthFunc "void glDepthFunc(GLenum func)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(func), func, 1, "glDepthFunc", "GLenum");
  glDepthFunc(XEN_TO_C_GLenum(func));
  return(XEN_FALSE);
}

static XEN gxg_glDepthMask(XEN flag)
{
  #define H_glDepthMask "void glDepthMask(GLboolean flag)"
  XEN_ASSERT_TYPE(XEN_GLboolean_P(flag), flag, 1, "glDepthMask", "GLboolean");
  glDepthMask(XEN_TO_C_GLboolean(flag));
  return(XEN_FALSE);
}

static XEN gxg_glDepthRange(XEN near_val, XEN far_val)
{
  #define H_glDepthRange "void glDepthRange(GLclampd near_val, GLclampd far_val)"
  XEN_ASSERT_TYPE(XEN_GLclampd_P(near_val), near_val, 1, "glDepthRange", "GLclampd");
  XEN_ASSERT_TYPE(XEN_GLclampd_P(far_val), far_val, 2, "glDepthRange", "GLclampd");
  glDepthRange(XEN_TO_C_GLclampd(near_val), XEN_TO_C_GLclampd(far_val));
  return(XEN_FALSE);
}

static XEN gxg_glClearAccum(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glClearAccum "void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(red), red, 1, "glClearAccum", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(green), green, 2, "glClearAccum", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(blue), blue, 3, "glClearAccum", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(alpha), alpha, 4, "glClearAccum", "GLfloat");
  glClearAccum(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue), XEN_TO_C_GLfloat(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glAccum(XEN op, XEN value)
{
  #define H_glAccum "void glAccum(GLenum op, GLfloat value)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(op), op, 1, "glAccum", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(value), value, 2, "glAccum", "GLfloat");
  glAccum(XEN_TO_C_GLenum(op), XEN_TO_C_GLfloat(value));
  return(XEN_FALSE);
}

static XEN gxg_glMatrixMode(XEN mode)
{
  #define H_glMatrixMode "void glMatrixMode(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glMatrixMode", "GLenum");
  glMatrixMode(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glOrtho(XEN left, XEN right, XEN bottom, XEN top, XEN near_val, XEN far_val)
{
  #define H_glOrtho "void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, \
GLdouble far_val)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(left), left, 1, "glOrtho", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(right), right, 2, "glOrtho", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(bottom), bottom, 3, "glOrtho", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(top), top, 4, "glOrtho", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(near_val), near_val, 5, "glOrtho", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(far_val), far_val, 6, "glOrtho", "GLdouble");
  glOrtho(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top), XEN_TO_C_GLdouble(near_val), 
          XEN_TO_C_GLdouble(far_val));
  return(XEN_FALSE);
}

static XEN gxg_glFrustum(XEN left, XEN right, XEN bottom, XEN top, XEN near_val, XEN far_val)
{
  #define H_glFrustum "void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, \
GLdouble far_val)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(left), left, 1, "glFrustum", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(right), right, 2, "glFrustum", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(bottom), bottom, 3, "glFrustum", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(top), top, 4, "glFrustum", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(near_val), near_val, 5, "glFrustum", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(far_val), far_val, 6, "glFrustum", "GLdouble");
  glFrustum(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top), XEN_TO_C_GLdouble(near_val), 
            XEN_TO_C_GLdouble(far_val));
  return(XEN_FALSE);
}

static XEN gxg_glViewport(XEN x, XEN y, XEN width, XEN height)
{
  #define H_glViewport "void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glViewport", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glViewport", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glViewport", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glViewport", "GLsizei");
  glViewport(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glPushMatrix(void)
{
  #define H_glPushMatrix "void glPushMatrix( void)"
  glPushMatrix();
  return(XEN_FALSE);
}

static XEN gxg_glPopMatrix(void)
{
  #define H_glPopMatrix "void glPopMatrix( void)"
  glPopMatrix();
  return(XEN_FALSE);
}

static XEN gxg_glLoadIdentity(void)
{
  #define H_glLoadIdentity "void glLoadIdentity( void)"
  glLoadIdentity();
  return(XEN_FALSE);
}

static XEN gxg_glLoadMatrixd(XEN m)
{
  #define H_glLoadMatrixd "void glLoadMatrixd(GLdouble* m)"
  XEN_ASSERT_TYPE(XEN_GLdouble__P(m), m, 1, "glLoadMatrixd", "GLdouble*");
  glLoadMatrixd(XEN_TO_C_GLdouble_(m));
  return(XEN_FALSE);
}

static XEN gxg_glLoadMatrixf(XEN m)
{
  #define H_glLoadMatrixf "void glLoadMatrixf(GLfloat* m)"
  XEN_ASSERT_TYPE(XEN_GLfloat__P(m), m, 1, "glLoadMatrixf", "GLfloat*");
  glLoadMatrixf(XEN_TO_C_GLfloat_(m));
  return(XEN_FALSE);
}

static XEN gxg_glMultMatrixd(XEN m)
{
  #define H_glMultMatrixd "void glMultMatrixd(GLdouble* m)"
  XEN_ASSERT_TYPE(XEN_GLdouble__P(m), m, 1, "glMultMatrixd", "GLdouble*");
  glMultMatrixd(XEN_TO_C_GLdouble_(m));
  return(XEN_FALSE);
}

static XEN gxg_glMultMatrixf(XEN m)
{
  #define H_glMultMatrixf "void glMultMatrixf(GLfloat* m)"
  XEN_ASSERT_TYPE(XEN_GLfloat__P(m), m, 1, "glMultMatrixf", "GLfloat*");
  glMultMatrixf(XEN_TO_C_GLfloat_(m));
  return(XEN_FALSE);
}

static XEN gxg_glRotated(XEN angle, XEN x, XEN y, XEN z)
{
  #define H_glRotated "void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(angle), angle, 1, "glRotated", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 2, "glRotated", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 3, "glRotated", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 4, "glRotated", "GLdouble");
  glRotated(XEN_TO_C_GLdouble(angle), XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(XEN_FALSE);
}

static XEN gxg_glRotatef(XEN angle, XEN x, XEN y, XEN z)
{
  #define H_glRotatef "void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(angle), angle, 1, "glRotatef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 2, "glRotatef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 3, "glRotatef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 4, "glRotatef", "GLfloat");
  glRotatef(XEN_TO_C_GLfloat(angle), XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(XEN_FALSE);
}

static XEN gxg_glScaled(XEN x, XEN y, XEN z)
{
  #define H_glScaled "void glScaled(GLdouble x, GLdouble y, GLdouble z)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glScaled", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glScaled", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glScaled", "GLdouble");
  glScaled(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(XEN_FALSE);
}

static XEN gxg_glScalef(XEN x, XEN y, XEN z)
{
  #define H_glScalef "void glScalef(GLfloat x, GLfloat y, GLfloat z)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glScalef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glScalef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glScalef", "GLfloat");
  glScalef(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(XEN_FALSE);
}

static XEN gxg_glTranslated(XEN x, XEN y, XEN z)
{
  #define H_glTranslated "void glTranslated(GLdouble x, GLdouble y, GLdouble z)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glTranslated", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glTranslated", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glTranslated", "GLdouble");
  glTranslated(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(XEN_FALSE);
}

static XEN gxg_glTranslatef(XEN x, XEN y, XEN z)
{
  #define H_glTranslatef "void glTranslatef(GLfloat x, GLfloat y, GLfloat z)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glTranslatef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glTranslatef", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glTranslatef", "GLfloat");
  glTranslatef(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(XEN_FALSE);
}

static XEN gxg_glIsList(XEN list)
{
  #define H_glIsList "GLboolean glIsList(GLuint list)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(list), list, 1, "glIsList", "GLuint");
  return(C_TO_XEN_GLboolean(glIsList(XEN_TO_C_GLuint(list))));
}

static XEN gxg_glDeleteLists(XEN list, XEN range)
{
  #define H_glDeleteLists "void glDeleteLists(GLuint list, GLsizei range)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(list), list, 1, "glDeleteLists", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(range), range, 2, "glDeleteLists", "GLsizei");
  glDeleteLists(XEN_TO_C_GLuint(list), XEN_TO_C_GLsizei(range));
  return(XEN_FALSE);
}

static XEN gxg_glGenLists(XEN range)
{
  #define H_glGenLists "GLuint glGenLists(GLsizei range)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(range), range, 1, "glGenLists", "GLsizei");
  return(C_TO_XEN_GLuint(glGenLists(XEN_TO_C_GLsizei(range))));
}

static XEN gxg_glNewList(XEN list, XEN mode)
{
  #define H_glNewList "void glNewList(GLuint list, GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(list), list, 1, "glNewList", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 2, "glNewList", "GLenum");
  glNewList(XEN_TO_C_GLuint(list), XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glEndList(void)
{
  #define H_glEndList "void glEndList( void)"
  glEndList();
  return(XEN_FALSE);
}

static XEN gxg_glCallList(XEN list)
{
  #define H_glCallList "void glCallList(GLuint list)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(list), list, 1, "glCallList", "GLuint");
  glCallList(XEN_TO_C_GLuint(list));
  return(XEN_FALSE);
}

static XEN gxg_glCallLists(XEN n, XEN type, XEN lists)
{
  #define H_glCallLists "void glCallLists(GLsizei n, GLenum type, GLvoid* lists)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glCallLists", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glCallLists", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(lists), lists, 3, "glCallLists", "GLvoid*");
  glCallLists(XEN_TO_C_GLsizei(n), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(lists));
  return(XEN_FALSE);
}

static XEN gxg_glListBase(XEN base)
{
  #define H_glListBase "void glListBase(GLuint base)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(base), base, 1, "glListBase", "GLuint");
  glListBase(XEN_TO_C_GLuint(base));
  return(XEN_FALSE);
}

static XEN gxg_glBegin(XEN mode)
{
  #define H_glBegin "void glBegin(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glBegin", "GLenum");
  glBegin(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glEnd(void)
{
  #define H_glEnd "void glEnd( void)"
  glEnd();
  return(XEN_FALSE);
}

static XEN gxg_glVertex2d(XEN x, XEN y)
{
  #define H_glVertex2d "void glVertex2d(GLdouble x, GLdouble y)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glVertex2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glVertex2d", "GLdouble");
  glVertex2d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y));
  return(XEN_FALSE);
}

static XEN gxg_glVertex2f(XEN x, XEN y)
{
  #define H_glVertex2f "void glVertex2f(GLfloat x, GLfloat y)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glVertex2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glVertex2f", "GLfloat");
  glVertex2f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y));
  return(XEN_FALSE);
}

static XEN gxg_glVertex2i(XEN x, XEN y)
{
  #define H_glVertex2i "void glVertex2i(GLint x, GLint y)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glVertex2i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glVertex2i", "GLint");
  glVertex2i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y));
  return(XEN_FALSE);
}

static XEN gxg_glVertex2s(XEN x, XEN y)
{
  #define H_glVertex2s "void glVertex2s(GLshort x, GLshort y)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glVertex2s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glVertex2s", "GLshort");
  glVertex2s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y));
  return(XEN_FALSE);
}

static XEN gxg_glVertex3d(XEN x, XEN y, XEN z)
{
  #define H_glVertex3d "void glVertex3d(GLdouble x, GLdouble y, GLdouble z)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glVertex3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glVertex3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glVertex3d", "GLdouble");
  glVertex3d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(XEN_FALSE);
}

static XEN gxg_glVertex3f(XEN x, XEN y, XEN z)
{
  #define H_glVertex3f "void glVertex3f(GLfloat x, GLfloat y, GLfloat z)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glVertex3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glVertex3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glVertex3f", "GLfloat");
  glVertex3f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(XEN_FALSE);
}

static XEN gxg_glVertex3i(XEN x, XEN y, XEN z)
{
  #define H_glVertex3i "void glVertex3i(GLint x, GLint y, GLint z)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glVertex3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glVertex3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(z), z, 3, "glVertex3i", "GLint");
  glVertex3i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z));
  return(XEN_FALSE);
}

static XEN gxg_glVertex3s(XEN x, XEN y, XEN z)
{
  #define H_glVertex3s "void glVertex3s(GLshort x, GLshort y, GLshort z)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glVertex3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glVertex3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(z), z, 3, "glVertex3s", "GLshort");
  glVertex3s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z));
  return(XEN_FALSE);
}

static XEN gxg_glVertex4d(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4d "void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glVertex4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glVertex4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glVertex4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(w), w, 4, "glVertex4d", "GLdouble");
  glVertex4d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z), XEN_TO_C_GLdouble(w));
  return(XEN_FALSE);
}

static XEN gxg_glVertex4f(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4f "void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glVertex4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glVertex4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glVertex4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(w), w, 4, "glVertex4f", "GLfloat");
  glVertex4f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z), XEN_TO_C_GLfloat(w));
  return(XEN_FALSE);
}

static XEN gxg_glVertex4i(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4i "void glVertex4i(GLint x, GLint y, GLint z, GLint w)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glVertex4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glVertex4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(z), z, 3, "glVertex4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(w), w, 4, "glVertex4i", "GLint");
  glVertex4i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z), XEN_TO_C_GLint(w));
  return(XEN_FALSE);
}

static XEN gxg_glVertex4s(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4s "void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glVertex4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glVertex4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(z), z, 3, "glVertex4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(w), w, 4, "glVertex4s", "GLshort");
  glVertex4s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z), XEN_TO_C_GLshort(w));
  return(XEN_FALSE);
}

static XEN gxg_glNormal3b(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3b "void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)"
  XEN_ASSERT_TYPE(XEN_GLbyte_P(nx), nx, 1, "glNormal3b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(ny), ny, 2, "glNormal3b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(nz), nz, 3, "glNormal3b", "GLbyte");
  glNormal3b(XEN_TO_C_GLbyte(nx), XEN_TO_C_GLbyte(ny), XEN_TO_C_GLbyte(nz));
  return(XEN_FALSE);
}

static XEN gxg_glNormal3d(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3d "void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(nx), nx, 1, "glNormal3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(ny), ny, 2, "glNormal3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(nz), nz, 3, "glNormal3d", "GLdouble");
  glNormal3d(XEN_TO_C_GLdouble(nx), XEN_TO_C_GLdouble(ny), XEN_TO_C_GLdouble(nz));
  return(XEN_FALSE);
}

static XEN gxg_glNormal3f(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3f "void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(nx), nx, 1, "glNormal3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(ny), ny, 2, "glNormal3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(nz), nz, 3, "glNormal3f", "GLfloat");
  glNormal3f(XEN_TO_C_GLfloat(nx), XEN_TO_C_GLfloat(ny), XEN_TO_C_GLfloat(nz));
  return(XEN_FALSE);
}

static XEN gxg_glNormal3i(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3i "void glNormal3i(GLint nx, GLint ny, GLint nz)"
  XEN_ASSERT_TYPE(XEN_GLint_P(nx), nx, 1, "glNormal3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(ny), ny, 2, "glNormal3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(nz), nz, 3, "glNormal3i", "GLint");
  glNormal3i(XEN_TO_C_GLint(nx), XEN_TO_C_GLint(ny), XEN_TO_C_GLint(nz));
  return(XEN_FALSE);
}

static XEN gxg_glNormal3s(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3s "void glNormal3s(GLshort nx, GLshort ny, GLshort nz)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(nx), nx, 1, "glNormal3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(ny), ny, 2, "glNormal3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(nz), nz, 3, "glNormal3s", "GLshort");
  glNormal3s(XEN_TO_C_GLshort(nx), XEN_TO_C_GLshort(ny), XEN_TO_C_GLshort(nz));
  return(XEN_FALSE);
}

static XEN gxg_glIndexd(XEN c)
{
  #define H_glIndexd "void glIndexd(GLdouble c)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(c), c, 1, "glIndexd", "GLdouble");
  glIndexd(XEN_TO_C_GLdouble(c));
  return(XEN_FALSE);
}

static XEN gxg_glIndexf(XEN c)
{
  #define H_glIndexf "void glIndexf(GLfloat c)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(c), c, 1, "glIndexf", "GLfloat");
  glIndexf(XEN_TO_C_GLfloat(c));
  return(XEN_FALSE);
}

static XEN gxg_glIndexi(XEN c)
{
  #define H_glIndexi "void glIndexi(GLint c)"
  XEN_ASSERT_TYPE(XEN_GLint_P(c), c, 1, "glIndexi", "GLint");
  glIndexi(XEN_TO_C_GLint(c));
  return(XEN_FALSE);
}

static XEN gxg_glIndexs(XEN c)
{
  #define H_glIndexs "void glIndexs(GLshort c)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(c), c, 1, "glIndexs", "GLshort");
  glIndexs(XEN_TO_C_GLshort(c));
  return(XEN_FALSE);
}

static XEN gxg_glIndexub(XEN c)
{
  #define H_glIndexub "void glIndexub(GLubyte c)"
  XEN_ASSERT_TYPE(XEN_GLubyte_P(c), c, 1, "glIndexub", "GLubyte");
  glIndexub(XEN_TO_C_GLubyte(c));
  return(XEN_FALSE);
}

static XEN gxg_glColor3b(XEN red, XEN green, XEN blue)
{
  #define H_glColor3b "void glColor3b(GLbyte red, GLbyte green, GLbyte blue)"
  XEN_ASSERT_TYPE(XEN_GLbyte_P(red), red, 1, "glColor3b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(green), green, 2, "glColor3b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(blue), blue, 3, "glColor3b", "GLbyte");
  glColor3b(XEN_TO_C_GLbyte(red), XEN_TO_C_GLbyte(green), XEN_TO_C_GLbyte(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3d(XEN red, XEN green, XEN blue)
{
  #define H_glColor3d "void glColor3d(GLdouble red, GLdouble green, GLdouble blue)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(red), red, 1, "glColor3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(green), green, 2, "glColor3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(blue), blue, 3, "glColor3d", "GLdouble");
  glColor3d(XEN_TO_C_GLdouble(red), XEN_TO_C_GLdouble(green), XEN_TO_C_GLdouble(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3f(XEN red, XEN green, XEN blue)
{
  #define H_glColor3f "void glColor3f(GLfloat red, GLfloat green, GLfloat blue)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(red), red, 1, "glColor3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(green), green, 2, "glColor3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(blue), blue, 3, "glColor3f", "GLfloat");
  glColor3f(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3i(XEN red, XEN green, XEN blue)
{
  #define H_glColor3i "void glColor3i(GLint red, GLint green, GLint blue)"
  XEN_ASSERT_TYPE(XEN_GLint_P(red), red, 1, "glColor3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(green), green, 2, "glColor3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(blue), blue, 3, "glColor3i", "GLint");
  glColor3i(XEN_TO_C_GLint(red), XEN_TO_C_GLint(green), XEN_TO_C_GLint(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3s(XEN red, XEN green, XEN blue)
{
  #define H_glColor3s "void glColor3s(GLshort red, GLshort green, GLshort blue)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(red), red, 1, "glColor3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(green), green, 2, "glColor3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(blue), blue, 3, "glColor3s", "GLshort");
  glColor3s(XEN_TO_C_GLshort(red), XEN_TO_C_GLshort(green), XEN_TO_C_GLshort(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3ub(XEN red, XEN green, XEN blue)
{
  #define H_glColor3ub "void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)"
  XEN_ASSERT_TYPE(XEN_GLubyte_P(red), red, 1, "glColor3ub", "GLubyte");
  XEN_ASSERT_TYPE(XEN_GLubyte_P(green), green, 2, "glColor3ub", "GLubyte");
  XEN_ASSERT_TYPE(XEN_GLubyte_P(blue), blue, 3, "glColor3ub", "GLubyte");
  glColor3ub(XEN_TO_C_GLubyte(red), XEN_TO_C_GLubyte(green), XEN_TO_C_GLubyte(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3ui(XEN red, XEN green, XEN blue)
{
  #define H_glColor3ui "void glColor3ui(GLuint red, GLuint green, GLuint blue)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(red), red, 1, "glColor3ui", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(green), green, 2, "glColor3ui", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(blue), blue, 3, "glColor3ui", "GLuint");
  glColor3ui(XEN_TO_C_GLuint(red), XEN_TO_C_GLuint(green), XEN_TO_C_GLuint(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor3us(XEN red, XEN green, XEN blue)
{
  #define H_glColor3us "void glColor3us(GLushort red, GLushort green, GLushort blue)"
  XEN_ASSERT_TYPE(XEN_GLushort_P(red), red, 1, "glColor3us", "GLushort");
  XEN_ASSERT_TYPE(XEN_GLushort_P(green), green, 2, "glColor3us", "GLushort");
  XEN_ASSERT_TYPE(XEN_GLushort_P(blue), blue, 3, "glColor3us", "GLushort");
  glColor3us(XEN_TO_C_GLushort(red), XEN_TO_C_GLushort(green), XEN_TO_C_GLushort(blue));
  return(XEN_FALSE);
}

static XEN gxg_glColor4b(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4b "void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)"
  XEN_ASSERT_TYPE(XEN_GLbyte_P(red), red, 1, "glColor4b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(green), green, 2, "glColor4b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(blue), blue, 3, "glColor4b", "GLbyte");
  XEN_ASSERT_TYPE(XEN_GLbyte_P(alpha), alpha, 4, "glColor4b", "GLbyte");
  glColor4b(XEN_TO_C_GLbyte(red), XEN_TO_C_GLbyte(green), XEN_TO_C_GLbyte(blue), XEN_TO_C_GLbyte(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4d(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4d "void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(red), red, 1, "glColor4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(green), green, 2, "glColor4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(blue), blue, 3, "glColor4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(alpha), alpha, 4, "glColor4d", "GLdouble");
  glColor4d(XEN_TO_C_GLdouble(red), XEN_TO_C_GLdouble(green), XEN_TO_C_GLdouble(blue), XEN_TO_C_GLdouble(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4f(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4f "void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(red), red, 1, "glColor4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(green), green, 2, "glColor4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(blue), blue, 3, "glColor4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(alpha), alpha, 4, "glColor4f", "GLfloat");
  glColor4f(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue), XEN_TO_C_GLfloat(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4i(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4i "void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)"
  XEN_ASSERT_TYPE(XEN_GLint_P(red), red, 1, "glColor4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(green), green, 2, "glColor4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(blue), blue, 3, "glColor4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(alpha), alpha, 4, "glColor4i", "GLint");
  glColor4i(XEN_TO_C_GLint(red), XEN_TO_C_GLint(green), XEN_TO_C_GLint(blue), XEN_TO_C_GLint(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4s(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4s "void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(red), red, 1, "glColor4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(green), green, 2, "glColor4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(blue), blue, 3, "glColor4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(alpha), alpha, 4, "glColor4s", "GLshort");
  glColor4s(XEN_TO_C_GLshort(red), XEN_TO_C_GLshort(green), XEN_TO_C_GLshort(blue), XEN_TO_C_GLshort(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4ub(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4ub "void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)"
  XEN_ASSERT_TYPE(XEN_GLubyte_P(red), red, 1, "glColor4ub", "GLubyte");
  XEN_ASSERT_TYPE(XEN_GLubyte_P(green), green, 2, "glColor4ub", "GLubyte");
  XEN_ASSERT_TYPE(XEN_GLubyte_P(blue), blue, 3, "glColor4ub", "GLubyte");
  XEN_ASSERT_TYPE(XEN_GLubyte_P(alpha), alpha, 4, "glColor4ub", "GLubyte");
  glColor4ub(XEN_TO_C_GLubyte(red), XEN_TO_C_GLubyte(green), XEN_TO_C_GLubyte(blue), XEN_TO_C_GLubyte(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4ui(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4ui "void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(red), red, 1, "glColor4ui", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(green), green, 2, "glColor4ui", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(blue), blue, 3, "glColor4ui", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(alpha), alpha, 4, "glColor4ui", "GLuint");
  glColor4ui(XEN_TO_C_GLuint(red), XEN_TO_C_GLuint(green), XEN_TO_C_GLuint(blue), XEN_TO_C_GLuint(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glColor4us(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4us "void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)"
  XEN_ASSERT_TYPE(XEN_GLushort_P(red), red, 1, "glColor4us", "GLushort");
  XEN_ASSERT_TYPE(XEN_GLushort_P(green), green, 2, "glColor4us", "GLushort");
  XEN_ASSERT_TYPE(XEN_GLushort_P(blue), blue, 3, "glColor4us", "GLushort");
  XEN_ASSERT_TYPE(XEN_GLushort_P(alpha), alpha, 4, "glColor4us", "GLushort");
  glColor4us(XEN_TO_C_GLushort(red), XEN_TO_C_GLushort(green), XEN_TO_C_GLushort(blue), XEN_TO_C_GLushort(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord1d(XEN s)
{
  #define H_glTexCoord1d "void glTexCoord1d(GLdouble s)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 1, "glTexCoord1d", "GLdouble");
  glTexCoord1d(XEN_TO_C_GLdouble(s));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord1f(XEN s)
{
  #define H_glTexCoord1f "void glTexCoord1f(GLfloat s)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 1, "glTexCoord1f", "GLfloat");
  glTexCoord1f(XEN_TO_C_GLfloat(s));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord1i(XEN s)
{
  #define H_glTexCoord1i "void glTexCoord1i(GLint s)"
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 1, "glTexCoord1i", "GLint");
  glTexCoord1i(XEN_TO_C_GLint(s));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord1s(XEN s)
{
  #define H_glTexCoord1s "void glTexCoord1s(GLshort s)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 1, "glTexCoord1s", "GLshort");
  glTexCoord1s(XEN_TO_C_GLshort(s));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord2d(XEN s, XEN t)
{
  #define H_glTexCoord2d "void glTexCoord2d(GLdouble s, GLdouble t)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 1, "glTexCoord2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 2, "glTexCoord2d", "GLdouble");
  glTexCoord2d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord2f(XEN s, XEN t)
{
  #define H_glTexCoord2f "void glTexCoord2f(GLfloat s, GLfloat t)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 1, "glTexCoord2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 2, "glTexCoord2f", "GLfloat");
  glTexCoord2f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord2i(XEN s, XEN t)
{
  #define H_glTexCoord2i "void glTexCoord2i(GLint s, GLint t)"
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 1, "glTexCoord2i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 2, "glTexCoord2i", "GLint");
  glTexCoord2i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord2s(XEN s, XEN t)
{
  #define H_glTexCoord2s "void glTexCoord2s(GLshort s, GLshort t)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 1, "glTexCoord2s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 2, "glTexCoord2s", "GLshort");
  glTexCoord2s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord3d(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3d "void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 1, "glTexCoord3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 2, "glTexCoord3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(r), r, 3, "glTexCoord3d", "GLdouble");
  glTexCoord3d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord3f(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3f "void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 1, "glTexCoord3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 2, "glTexCoord3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(r), r, 3, "glTexCoord3f", "GLfloat");
  glTexCoord3f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord3i(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3i "void glTexCoord3i(GLint s, GLint t, GLint r)"
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 1, "glTexCoord3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 2, "glTexCoord3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(r), r, 3, "glTexCoord3i", "GLint");
  glTexCoord3i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord3s(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3s "void glTexCoord3s(GLshort s, GLshort t, GLshort r)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 1, "glTexCoord3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 2, "glTexCoord3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(r), r, 3, "glTexCoord3s", "GLshort");
  glTexCoord3s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord4d(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4d "void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 1, "glTexCoord4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 2, "glTexCoord4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(r), r, 3, "glTexCoord4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(q), q, 4, "glTexCoord4d", "GLdouble");
  glTexCoord4d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r), XEN_TO_C_GLdouble(q));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord4f(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4f "void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 1, "glTexCoord4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 2, "glTexCoord4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(r), r, 3, "glTexCoord4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(q), q, 4, "glTexCoord4f", "GLfloat");
  glTexCoord4f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r), XEN_TO_C_GLfloat(q));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord4i(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4i "void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)"
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 1, "glTexCoord4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 2, "glTexCoord4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(r), r, 3, "glTexCoord4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(q), q, 4, "glTexCoord4i", "GLint");
  glTexCoord4i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r), XEN_TO_C_GLint(q));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoord4s(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4s "void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 1, "glTexCoord4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 2, "glTexCoord4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(r), r, 3, "glTexCoord4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(q), q, 4, "glTexCoord4s", "GLshort");
  glTexCoord4s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r), XEN_TO_C_GLshort(q));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos2d(XEN x, XEN y)
{
  #define H_glRasterPos2d "void glRasterPos2d(GLdouble x, GLdouble y)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glRasterPos2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glRasterPos2d", "GLdouble");
  glRasterPos2d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos2f(XEN x, XEN y)
{
  #define H_glRasterPos2f "void glRasterPos2f(GLfloat x, GLfloat y)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glRasterPos2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glRasterPos2f", "GLfloat");
  glRasterPos2f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos2i(XEN x, XEN y)
{
  #define H_glRasterPos2i "void glRasterPos2i(GLint x, GLint y)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glRasterPos2i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glRasterPos2i", "GLint");
  glRasterPos2i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos2s(XEN x, XEN y)
{
  #define H_glRasterPos2s "void glRasterPos2s(GLshort x, GLshort y)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glRasterPos2s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glRasterPos2s", "GLshort");
  glRasterPos2s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos3d(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3d "void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glRasterPos3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glRasterPos3d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glRasterPos3d", "GLdouble");
  glRasterPos3d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos3f(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3f "void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glRasterPos3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glRasterPos3f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glRasterPos3f", "GLfloat");
  glRasterPos3f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos3i(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3i "void glRasterPos3i(GLint x, GLint y, GLint z)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glRasterPos3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glRasterPos3i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(z), z, 3, "glRasterPos3i", "GLint");
  glRasterPos3i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos3s(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3s "void glRasterPos3s(GLshort x, GLshort y, GLshort z)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glRasterPos3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glRasterPos3s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(z), z, 3, "glRasterPos3s", "GLshort");
  glRasterPos3s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos4d(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4d "void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "glRasterPos4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "glRasterPos4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(z), z, 3, "glRasterPos4d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(w), w, 4, "glRasterPos4d", "GLdouble");
  glRasterPos4d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z), XEN_TO_C_GLdouble(w));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos4f(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4f "void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x), x, 1, "glRasterPos4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y), y, 2, "glRasterPos4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(z), z, 3, "glRasterPos4f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(w), w, 4, "glRasterPos4f", "GLfloat");
  glRasterPos4f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z), XEN_TO_C_GLfloat(w));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos4i(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4i "void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glRasterPos4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glRasterPos4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(z), z, 3, "glRasterPos4i", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(w), w, 4, "glRasterPos4i", "GLint");
  glRasterPos4i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z), XEN_TO_C_GLint(w));
  return(XEN_FALSE);
}

static XEN gxg_glRasterPos4s(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4s "void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x), x, 1, "glRasterPos4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y), y, 2, "glRasterPos4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(z), z, 3, "glRasterPos4s", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(w), w, 4, "glRasterPos4s", "GLshort");
  glRasterPos4s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z), XEN_TO_C_GLshort(w));
  return(XEN_FALSE);
}

static XEN gxg_glRectd(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRectd "void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x1), x1, 1, "glRectd", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y1), y1, 2, "glRectd", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x2), x2, 3, "glRectd", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y2), y2, 4, "glRectd", "GLdouble");
  glRectd(XEN_TO_C_GLdouble(x1), XEN_TO_C_GLdouble(y1), XEN_TO_C_GLdouble(x2), XEN_TO_C_GLdouble(y2));
  return(XEN_FALSE);
}

static XEN gxg_glRectf(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRectf "void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x1), x1, 1, "glRectf", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y1), y1, 2, "glRectf", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(x2), x2, 3, "glRectf", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(y2), y2, 4, "glRectf", "GLfloat");
  glRectf(XEN_TO_C_GLfloat(x1), XEN_TO_C_GLfloat(y1), XEN_TO_C_GLfloat(x2), XEN_TO_C_GLfloat(y2));
  return(XEN_FALSE);
}

static XEN gxg_glRecti(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRecti "void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x1), x1, 1, "glRecti", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y1), y1, 2, "glRecti", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(x2), x2, 3, "glRecti", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y2), y2, 4, "glRecti", "GLint");
  glRecti(XEN_TO_C_GLint(x1), XEN_TO_C_GLint(y1), XEN_TO_C_GLint(x2), XEN_TO_C_GLint(y2));
  return(XEN_FALSE);
}

static XEN gxg_glRects(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRects "void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)"
  XEN_ASSERT_TYPE(XEN_GLshort_P(x1), x1, 1, "glRects", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y1), y1, 2, "glRects", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(x2), x2, 3, "glRects", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(y2), y2, 4, "glRects", "GLshort");
  glRects(XEN_TO_C_GLshort(x1), XEN_TO_C_GLshort(y1), XEN_TO_C_GLshort(x2), XEN_TO_C_GLshort(y2));
  return(XEN_FALSE);
}

static XEN gxg_glVertexPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glVertexPointer "void glVertexPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glVertexPointer", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glVertexPointer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glVertexPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 4, "glVertexPointer", "GLvoid*");
  glVertexPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glNormalPointer(XEN type, XEN stride, XEN ptr)
{
  #define H_glNormalPointer "void glNormalPointer(GLenum type, GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 1, "glNormalPointer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 2, "glNormalPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 3, "glNormalPointer", "GLvoid*");
  glNormalPointer(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glColorPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glColorPointer "void glColorPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glColorPointer", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glColorPointer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glColorPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 4, "glColorPointer", "GLvoid*");
  glColorPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glIndexPointer(XEN type, XEN stride, XEN ptr)
{
  #define H_glIndexPointer "void glIndexPointer(GLenum type, GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 1, "glIndexPointer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 2, "glIndexPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 3, "glIndexPointer", "GLvoid*");
  glIndexPointer(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoordPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glTexCoordPointer "void glTexCoordPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glTexCoordPointer", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glTexCoordPointer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glTexCoordPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 4, "glTexCoordPointer", "GLvoid*");
  glTexCoordPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glEdgeFlagPointer(XEN stride, XEN ptr)
{
  #define H_glEdgeFlagPointer "void glEdgeFlagPointer(GLsizei stride, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 1, "glEdgeFlagPointer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 2, "glEdgeFlagPointer", "GLvoid*");
  glEdgeFlagPointer(XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glGetPointerv(XEN pname, XEN params)
{
  #define H_glGetPointerv "void glGetPointerv(GLenum pname, void** [params])"
  void* ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetPointerv", "GLenum");
  glGetPointerv(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_void_(ref_params)));
}

static XEN gxg_glArrayElement(XEN i)
{
  #define H_glArrayElement "void glArrayElement(GLint i)"
  XEN_ASSERT_TYPE(XEN_GLint_P(i), i, 1, "glArrayElement", "GLint");
  glArrayElement(XEN_TO_C_GLint(i));
  return(XEN_FALSE);
}

static XEN gxg_glDrawArrays(XEN mode, XEN first, XEN count)
{
  #define H_glDrawArrays "void glDrawArrays(GLenum mode, GLint first, GLsizei count)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glDrawArrays", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(first), first, 2, "glDrawArrays", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glDrawArrays", "GLsizei");
  glDrawArrays(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(first), XEN_TO_C_GLsizei(count));
  return(XEN_FALSE);
}

static XEN gxg_glDrawElements(XEN mode, XEN count, XEN type, XEN indices)
{
  #define H_glDrawElements "void glDrawElements(GLenum mode, GLsizei count, GLenum type, GLvoid* indices)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glDrawElements", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 2, "glDrawElements", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 3, "glDrawElements", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(indices), indices, 4, "glDrawElements", "GLvoid*");
  glDrawElements(XEN_TO_C_GLenum(mode), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(indices));
  return(XEN_FALSE);
}

static XEN gxg_glInterleavedArrays(XEN format, XEN stride, XEN pointer)
{
  #define H_glInterleavedArrays "void glInterleavedArrays(GLenum format, GLsizei stride, GLvoid* pointer)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 1, "glInterleavedArrays", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 2, "glInterleavedArrays", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pointer), pointer, 3, "glInterleavedArrays", "GLvoid*");
  glInterleavedArrays(XEN_TO_C_GLenum(format), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(pointer));
  return(XEN_FALSE);
}

static XEN gxg_glShadeModel(XEN mode)
{
  #define H_glShadeModel "void glShadeModel(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glShadeModel", "GLenum");
  glShadeModel(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glLightf(XEN light, XEN pname, XEN param)
{
  #define H_glLightf "void glLightf(GLenum light, GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glLightf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glLightf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 3, "glLightf", "GLfloat");
  glLightf(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glLighti(XEN light, XEN pname, XEN param)
{
  #define H_glLighti "void glLighti(GLenum light, GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glLighti", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glLighti", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 3, "glLighti", "GLint");
  glLighti(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetLightfv(XEN light, XEN pname, XEN params)
{
  #define H_glGetLightfv "void glGetLightfv(GLenum light, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glGetLightfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetLightfv", "GLenum");
  glGetLightfv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetLightiv(XEN light, XEN pname, XEN params)
{
  #define H_glGetLightiv "void glGetLightiv(GLenum light, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glGetLightiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetLightiv", "GLenum");
  glGetLightiv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glLightModelf(XEN pname, XEN param)
{
  #define H_glLightModelf "void glLightModelf(GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glLightModelf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 2, "glLightModelf", "GLfloat");
  glLightModelf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glLightModeli(XEN pname, XEN param)
{
  #define H_glLightModeli "void glLightModeli(GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glLightModeli", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 2, "glLightModeli", "GLint");
  glLightModeli(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glMaterialf(XEN face, XEN pname, XEN param)
{
  #define H_glMaterialf "void glMaterialf(GLenum face, GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glMaterialf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glMaterialf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 3, "glMaterialf", "GLfloat");
  glMaterialf(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glMateriali(XEN face, XEN pname, XEN param)
{
  #define H_glMateriali "void glMateriali(GLenum face, GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glMateriali", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glMateriali", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 3, "glMateriali", "GLint");
  glMateriali(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetMaterialfv(XEN face, XEN pname, XEN params)
{
  #define H_glGetMaterialfv "void glGetMaterialfv(GLenum face, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glGetMaterialfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMaterialfv", "GLenum");
  glGetMaterialfv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetMaterialiv(XEN face, XEN pname, XEN params)
{
  #define H_glGetMaterialiv "void glGetMaterialiv(GLenum face, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glGetMaterialiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMaterialiv", "GLenum");
  glGetMaterialiv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glColorMaterial(XEN face, XEN mode)
{
  #define H_glColorMaterial "void glColorMaterial(GLenum face, GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glColorMaterial", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 2, "glColorMaterial", "GLenum");
  glColorMaterial(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glPixelZoom(XEN xfactor, XEN yfactor)
{
  #define H_glPixelZoom "void glPixelZoom(GLfloat xfactor, GLfloat yfactor)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(xfactor), xfactor, 1, "glPixelZoom", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(yfactor), yfactor, 2, "glPixelZoom", "GLfloat");
  glPixelZoom(XEN_TO_C_GLfloat(xfactor), XEN_TO_C_GLfloat(yfactor));
  return(XEN_FALSE);
}

static XEN gxg_glPixelStoref(XEN pname, XEN param)
{
  #define H_glPixelStoref "void glPixelStoref(GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPixelStoref", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 2, "glPixelStoref", "GLfloat");
  glPixelStoref(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glPixelStorei(XEN pname, XEN param)
{
  #define H_glPixelStorei "void glPixelStorei(GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPixelStorei", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 2, "glPixelStorei", "GLint");
  glPixelStorei(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glPixelTransferf(XEN pname, XEN param)
{
  #define H_glPixelTransferf "void glPixelTransferf(GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPixelTransferf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 2, "glPixelTransferf", "GLfloat");
  glPixelTransferf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glPixelTransferi(XEN pname, XEN param)
{
  #define H_glPixelTransferi "void glPixelTransferi(GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPixelTransferi", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 2, "glPixelTransferi", "GLint");
  glPixelTransferi(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetPixelMapfv(XEN map, XEN values)
{
  #define H_glGetPixelMapfv "void glGetPixelMapfv(GLenum map, GLfloat* [values])"
  GLfloat ref_values;
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapfv", "GLenum");
  glGetPixelMapfv(XEN_TO_C_GLenum(map), &ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_values)));
}

static XEN gxg_glGetPixelMapuiv(XEN map, XEN values)
{
  #define H_glGetPixelMapuiv "void glGetPixelMapuiv(GLenum map, GLuint* [values])"
  GLuint ref_values;
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapuiv", "GLenum");
  glGetPixelMapuiv(XEN_TO_C_GLenum(map), &ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLuint(ref_values)));
}

static XEN gxg_glGetPixelMapusv(XEN map, XEN values)
{
  #define H_glGetPixelMapusv "void glGetPixelMapusv(GLenum map, GLushort* [values])"
  GLushort ref_values;
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapusv", "GLenum");
  glGetPixelMapusv(XEN_TO_C_GLenum(map), &ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLushort(ref_values)));
}

static XEN gxg_glBitmap(XEN width, XEN height, XEN xorig, XEN yorig, XEN xmove, XEN ymove, XEN bitmap)
{
  #define H_glBitmap "void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, \
GLfloat ymove, GLubyte* bitmap)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 1, "glBitmap", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 2, "glBitmap", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(xorig), xorig, 3, "glBitmap", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(yorig), yorig, 4, "glBitmap", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(xmove), xmove, 5, "glBitmap", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(ymove), ymove, 6, "glBitmap", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLubyte__P(bitmap), bitmap, 7, "glBitmap", "GLubyte*");
  glBitmap(XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLfloat(xorig), XEN_TO_C_GLfloat(yorig), XEN_TO_C_GLfloat(xmove), 
           XEN_TO_C_GLfloat(ymove), XEN_TO_C_GLubyte_(bitmap));
  return(XEN_FALSE);
}

static XEN gxg_glReadPixels(XEN x, XEN y, XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glReadPixels "void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, \
GLenum type, GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glReadPixels", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glReadPixels", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glReadPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glReadPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 5, "glReadPixels", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 6, "glReadPixels", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 7, "glReadPixels", "GLvoid*");
  glReadPixels(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), 
               XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glDrawPixels(XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glDrawPixels "void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type, \
GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 1, "glDrawPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 2, "glDrawPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 3, "glDrawPixels", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 4, "glDrawPixels", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 5, "glDrawPixels", "GLvoid*");
  glDrawPixels(XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glCopyPixels(XEN x, XEN y, XEN width, XEN height, XEN type)
{
  #define H_glCopyPixels "void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)"
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 1, "glCopyPixels", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 2, "glCopyPixels", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glCopyPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glCopyPixels", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glCopyPixels", "GLenum");
  glCopyPixels(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(type));
  return(XEN_FALSE);
}

static XEN gxg_glStencilFunc(XEN func, XEN ref, XEN mask)
{
  #define H_glStencilFunc "void glStencilFunc(GLenum func, GLint ref, GLuint mask)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(func), func, 1, "glStencilFunc", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(ref), ref, 2, "glStencilFunc", "GLint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(mask), mask, 3, "glStencilFunc", "GLuint");
  glStencilFunc(XEN_TO_C_GLenum(func), XEN_TO_C_GLint(ref), XEN_TO_C_GLuint(mask));
  return(XEN_FALSE);
}

static XEN gxg_glStencilMask(XEN mask)
{
  #define H_glStencilMask "void glStencilMask(GLuint mask)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(mask), mask, 1, "glStencilMask", "GLuint");
  glStencilMask(XEN_TO_C_GLuint(mask));
  return(XEN_FALSE);
}

static XEN gxg_glStencilOp(XEN fail, XEN zfail, XEN zpass)
{
  #define H_glStencilOp "void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(fail), fail, 1, "glStencilOp", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(zfail), zfail, 2, "glStencilOp", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(zpass), zpass, 3, "glStencilOp", "GLenum");
  glStencilOp(XEN_TO_C_GLenum(fail), XEN_TO_C_GLenum(zfail), XEN_TO_C_GLenum(zpass));
  return(XEN_FALSE);
}

static XEN gxg_glClearStencil(XEN s)
{
  #define H_glClearStencil "void glClearStencil(GLint s)"
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 1, "glClearStencil", "GLint");
  glClearStencil(XEN_TO_C_GLint(s));
  return(XEN_FALSE);
}

static XEN gxg_glTexGend(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGend "void glTexGend(GLenum coord, GLenum pname, GLdouble param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glTexGend", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexGend", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(param), param, 3, "glTexGend", "GLdouble");
  glTexGend(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLdouble(param));
  return(XEN_FALSE);
}

static XEN gxg_glTexGenf(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGenf "void glTexGenf(GLenum coord, GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glTexGenf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexGenf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 3, "glTexGenf", "GLfloat");
  glTexGenf(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glTexGeni(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGeni "void glTexGeni(GLenum coord, GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glTexGeni", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexGeni", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 3, "glTexGeni", "GLint");
  glTexGeni(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetTexGendv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGendv "void glGetTexGendv(GLenum coord, GLenum pname, GLdouble* [params])"
  GLdouble ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGendv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGendv", "GLenum");
  glGetTexGendv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_params)));
}

static XEN gxg_glGetTexGenfv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGenfv "void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGenfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGenfv", "GLenum");
  glGetTexGenfv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetTexGeniv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGeniv "void glGetTexGeniv(GLenum coord, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGeniv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGeniv", "GLenum");
  glGetTexGeniv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glTexEnvf(XEN target, XEN pname, XEN param)
{
  #define H_glTexEnvf "void glTexEnvf(GLenum target, GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexEnvf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexEnvf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 3, "glTexEnvf", "GLfloat");
  glTexEnvf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glTexEnvi(XEN target, XEN pname, XEN param)
{
  #define H_glTexEnvi "void glTexEnvi(GLenum target, GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexEnvi", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexEnvi", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 3, "glTexEnvi", "GLint");
  glTexEnvi(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetTexEnvfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexEnvfv "void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexEnvfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexEnvfv", "GLenum");
  glGetTexEnvfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetTexEnviv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexEnviv "void glGetTexEnviv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexEnviv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexEnviv", "GLenum");
  glGetTexEnviv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glTexParameterf(XEN target, XEN pname, XEN param)
{
  #define H_glTexParameterf "void glTexParameterf(GLenum target, GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexParameterf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexParameterf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 3, "glTexParameterf", "GLfloat");
  glTexParameterf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glTexParameteri(XEN target, XEN pname, XEN param)
{
  #define H_glTexParameteri "void glTexParameteri(GLenum target, GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexParameteri", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glTexParameteri", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 3, "glTexParameteri", "GLint");
  glTexParameteri(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glGetTexParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexParameterfv "void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexParameterfv", "GLenum");
  glGetTexParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetTexParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexParameteriv "void glGetTexParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexParameteriv", "GLenum");
  glGetTexParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glGetTexLevelParameterfv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameterfv "void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexLevelParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glGetTexLevelParameterfv", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 3, "glGetTexLevelParameterfv", "GLenum");
  glGetTexLevelParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetTexLevelParameteriv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameteriv "void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, \
GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexLevelParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glGetTexLevelParameteriv", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 3, "glGetTexLevelParameteriv", "GLenum");
  glGetTexLevelParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glTexImage1D(XEN target, XEN level, XEN internalFormat, XEN width, XEN border, XEN format, XEN type, XEN pixels)
{
  #define H_glTexImage1D "void glTexImage1D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(internalFormat), internalFormat, 3, "glTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 4, "glTexImage1D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 5, "glTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 6, "glTexImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 7, "glTexImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 8, "glTexImage1D", "GLvoid*");
  glTexImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLint(border), 
               XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glTexImage2D(XEN target, XEN level, XEN internalFormat, XEN width, XEN height, XEN border, XEN format, XEN type, XEN pixels)
{
  #define H_glTexImage2D "void glTexImage2D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLsizei height, GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(internalFormat), internalFormat, 3, "glTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 4, "glTexImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 5, "glTexImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 6, "glTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 7, "glTexImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 8, "glTexImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 9, "glTexImage2D", "GLvoid*");
  glTexImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
               XEN_TO_C_GLint(border), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glGenTextures(XEN n, XEN textures)
{
  #define H_glGenTextures "void glGenTextures(GLsizei n, GLuint* textures)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glGenTextures", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glGenTextures", "GLuint*");
  glGenTextures(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(XEN_FALSE);
}

static XEN gxg_glDeleteTextures(XEN n, XEN textures)
{
  #define H_glDeleteTextures "void glDeleteTextures(GLsizei n, GLuint* textures)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glDeleteTextures", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glDeleteTextures", "GLuint*");
  glDeleteTextures(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(XEN_FALSE);
}

static XEN gxg_glBindTexture(XEN target, XEN texture)
{
  #define H_glBindTexture "void glBindTexture(GLenum target, GLuint texture)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glBindTexture", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLuint_P(texture), texture, 2, "glBindTexture", "GLuint");
  glBindTexture(XEN_TO_C_GLenum(target), XEN_TO_C_GLuint(texture));
  return(XEN_FALSE);
}

static XEN gxg_glAreTexturesResident(XEN n, XEN textures, XEN residences)
{
  #define H_glAreTexturesResident "GLboolean glAreTexturesResident(GLsizei n, GLuint* textures, GLboolean* residences)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glAreTexturesResident", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glAreTexturesResident", "GLuint*");
  XEN_ASSERT_TYPE(XEN_GLboolean__P(residences), residences, 3, "glAreTexturesResident", "GLboolean*");
  return(C_TO_XEN_GLboolean(glAreTexturesResident(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures), XEN_TO_C_GLboolean_(residences))));
}

static XEN gxg_glIsTexture(XEN texture)
{
  #define H_glIsTexture "GLboolean glIsTexture(GLuint texture)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(texture), texture, 1, "glIsTexture", "GLuint");
  return(C_TO_XEN_GLboolean(glIsTexture(XEN_TO_C_GLuint(texture))));
}

static XEN gxg_glTexSubImage1D(XEN target, XEN level, XEN xoffset, XEN width, XEN format, XEN type, XEN pixels)
{
  #define H_glTexSubImage1D "void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, \
GLenum format, GLenum type, GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexSubImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 4, "glTexSubImage1D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 5, "glTexSubImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 6, "glTexSubImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 7, "glTexSubImage1D", "GLvoid*");
  glTexSubImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
                  XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glTexSubImage2D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glTexSubImage2D "void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, \
GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexSubImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 5, "glTexSubImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 6, "glTexSubImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 7, "glTexSubImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 8, "glTexSubImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 9, "glTexSubImage2D", "GLvoid*");
  glTexSubImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLsizei(width), 
                  XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexImage1D(XEN target, XEN level, XEN internalformat, XEN x, XEN y, XEN width, XEN border)
{
  #define H_glCopyTexImage1D "void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLint border)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 3, "glCopyTexImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 4, "glCopyTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 5, "glCopyTexImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 6, "glCopyTexImage1D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 7, "glCopyTexImage1D", "GLint");
  glCopyTexImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                   XEN_TO_C_GLsizei(width), XEN_TO_C_GLint(border));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexImage2D(XEN target, XEN level, XEN internalformat, XEN x, XEN y, XEN width, XEN height, XEN border)
{
  #define H_glCopyTexImage2D "void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 3, "glCopyTexImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 4, "glCopyTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 5, "glCopyTexImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 6, "glCopyTexImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 7, "glCopyTexImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 8, "glCopyTexImage2D", "GLint");
  glCopyTexImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                   XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLint(border));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexSubImage1D(XEN target, XEN level, XEN xoffset, XEN x, XEN y, XEN width)
{
  #define H_glCopyTexSubImage1D "void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, \
GLint x, GLint y, GLsizei width)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexSubImage1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glCopyTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 4, "glCopyTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 5, "glCopyTexSubImage1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 6, "glCopyTexSubImage1D", "GLsizei");
  glCopyTexSubImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                      XEN_TO_C_GLsizei(width));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexSubImage2D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyTexSubImage2D "void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, \
GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexSubImage2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glCopyTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glCopyTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 5, "glCopyTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 6, "glCopyTexSubImage2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 7, "glCopyTexSubImage2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 8, "glCopyTexSubImage2D", "GLsizei");
  glCopyTexSubImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(x), 
                      XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glMap1d(XEN target, XEN u1, XEN u2, XEN stride, XEN order, XEN points)
{
  #define H_glMap1d "void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, \
GLdouble* points)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMap1d", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u1), u1, 2, "glMap1d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u2), u2, 3, "glMap1d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLint_P(stride), stride, 4, "glMap1d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(order), order, 5, "glMap1d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(points), points, 6, "glMap1d", "GLdouble*");
  glMap1d(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(stride), XEN_TO_C_GLint(order), 
          XEN_TO_C_GLdouble_(points));
  return(XEN_FALSE);
}

static XEN gxg_glMap1f(XEN target, XEN u1, XEN u2, XEN stride, XEN order, XEN points)
{
  #define H_glMap1f "void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, GLfloat* points)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMap1f", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u1), u1, 2, "glMap1f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u2), u2, 3, "glMap1f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLint_P(stride), stride, 4, "glMap1f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(order), order, 5, "glMap1f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(points), points, 6, "glMap1f", "GLfloat*");
  glMap1f(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(stride), XEN_TO_C_GLint(order), 
          XEN_TO_C_GLfloat_(points));
  return(XEN_FALSE);
}

static XEN gxg_glMap2d(XEN arglist)
{
  #define H_glMap2d "void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, \
GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, GLdouble* points)"
  XEN target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points;
  target = XEN_LIST_REF(arglist, 0);
  u1 = XEN_LIST_REF(arglist, 1);
  u2 = XEN_LIST_REF(arglist, 2);
  ustride = XEN_LIST_REF(arglist, 3);
  uorder = XEN_LIST_REF(arglist, 4);
  v1 = XEN_LIST_REF(arglist, 5);
  v2 = XEN_LIST_REF(arglist, 6);
  vstride = XEN_LIST_REF(arglist, 7);
  vorder = XEN_LIST_REF(arglist, 8);
  points = XEN_LIST_REF(arglist, 9);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMap2d", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u1), u1, 2, "glMap2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u2), u2, 3, "glMap2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLint_P(ustride), ustride, 4, "glMap2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(uorder), uorder, 5, "glMap2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(v1), v1, 6, "glMap2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(v2), v2, 7, "glMap2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLint_P(vstride), vstride, 8, "glMap2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(vorder), vorder, 9, "glMap2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(points), points, 10, "glMap2d", "GLdouble*");
  glMap2d(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(ustride), XEN_TO_C_GLint(uorder), 
          XEN_TO_C_GLdouble(v1), XEN_TO_C_GLdouble(v2), XEN_TO_C_GLint(vstride), XEN_TO_C_GLint(vorder), XEN_TO_C_GLdouble_(points));
  return(XEN_FALSE);
}

static XEN gxg_glMap2f(XEN arglist)
{
  #define H_glMap2f "void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, \
GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, GLfloat* points)"
  XEN target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points;
  target = XEN_LIST_REF(arglist, 0);
  u1 = XEN_LIST_REF(arglist, 1);
  u2 = XEN_LIST_REF(arglist, 2);
  ustride = XEN_LIST_REF(arglist, 3);
  uorder = XEN_LIST_REF(arglist, 4);
  v1 = XEN_LIST_REF(arglist, 5);
  v2 = XEN_LIST_REF(arglist, 6);
  vstride = XEN_LIST_REF(arglist, 7);
  vorder = XEN_LIST_REF(arglist, 8);
  points = XEN_LIST_REF(arglist, 9);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMap2f", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u1), u1, 2, "glMap2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u2), u2, 3, "glMap2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLint_P(ustride), ustride, 4, "glMap2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(uorder), uorder, 5, "glMap2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(v1), v1, 6, "glMap2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(v2), v2, 7, "glMap2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLint_P(vstride), vstride, 8, "glMap2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(vorder), vorder, 9, "glMap2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(points), points, 10, "glMap2f", "GLfloat*");
  glMap2f(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(ustride), XEN_TO_C_GLint(uorder), 
          XEN_TO_C_GLfloat(v1), XEN_TO_C_GLfloat(v2), XEN_TO_C_GLint(vstride), XEN_TO_C_GLint(vorder), XEN_TO_C_GLfloat_(points));
  return(XEN_FALSE);
}

static XEN gxg_glGetMapdv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapdv "void glGetMapdv(GLenum target, GLenum query, GLdouble* [v])"
  GLdouble ref_v;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapdv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapdv", "GLenum");
  glGetMapdv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), &ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_v)));
}

static XEN gxg_glGetMapfv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapfv "void glGetMapfv(GLenum target, GLenum query, GLfloat* [v])"
  GLfloat ref_v;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapfv", "GLenum");
  glGetMapfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), &ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_v)));
}

static XEN gxg_glGetMapiv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapiv "void glGetMapiv(GLenum target, GLenum query, GLint* [v])"
  GLint ref_v;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapiv", "GLenum");
  glGetMapiv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), &ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_v)));
}

static XEN gxg_glEvalCoord1d(XEN u)
{
  #define H_glEvalCoord1d "void glEvalCoord1d(GLdouble u)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u), u, 1, "glEvalCoord1d", "GLdouble");
  glEvalCoord1d(XEN_TO_C_GLdouble(u));
  return(XEN_FALSE);
}

static XEN gxg_glEvalCoord1f(XEN u)
{
  #define H_glEvalCoord1f "void glEvalCoord1f(GLfloat u)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u), u, 1, "glEvalCoord1f", "GLfloat");
  glEvalCoord1f(XEN_TO_C_GLfloat(u));
  return(XEN_FALSE);
}

static XEN gxg_glEvalCoord2d(XEN u, XEN v)
{
  #define H_glEvalCoord2d "void glEvalCoord2d(GLdouble u, GLdouble v)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u), u, 1, "glEvalCoord2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(v), v, 2, "glEvalCoord2d", "GLdouble");
  glEvalCoord2d(XEN_TO_C_GLdouble(u), XEN_TO_C_GLdouble(v));
  return(XEN_FALSE);
}

static XEN gxg_glEvalCoord2f(XEN u, XEN v)
{
  #define H_glEvalCoord2f "void glEvalCoord2f(GLfloat u, GLfloat v)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u), u, 1, "glEvalCoord2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(v), v, 2, "glEvalCoord2f", "GLfloat");
  glEvalCoord2f(XEN_TO_C_GLfloat(u), XEN_TO_C_GLfloat(v));
  return(XEN_FALSE);
}

static XEN gxg_glMapGrid1d(XEN un, XEN u1, XEN u2)
{
  #define H_glMapGrid1d "void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)"
  XEN_ASSERT_TYPE(XEN_GLint_P(un), un, 1, "glMapGrid1d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u1), u1, 2, "glMapGrid1d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u2), u2, 3, "glMapGrid1d", "GLdouble");
  glMapGrid1d(XEN_TO_C_GLint(un), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2));
  return(XEN_FALSE);
}

static XEN gxg_glMapGrid1f(XEN un, XEN u1, XEN u2)
{
  #define H_glMapGrid1f "void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)"
  XEN_ASSERT_TYPE(XEN_GLint_P(un), un, 1, "glMapGrid1f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u1), u1, 2, "glMapGrid1f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u2), u2, 3, "glMapGrid1f", "GLfloat");
  glMapGrid1f(XEN_TO_C_GLint(un), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2));
  return(XEN_FALSE);
}

static XEN gxg_glMapGrid2d(XEN un, XEN u1, XEN u2, XEN vn, XEN v1, XEN v2)
{
  #define H_glMapGrid2d "void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, \
GLdouble v2)"
  XEN_ASSERT_TYPE(XEN_GLint_P(un), un, 1, "glMapGrid2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u1), u1, 2, "glMapGrid2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(u2), u2, 3, "glMapGrid2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLint_P(vn), vn, 4, "glMapGrid2d", "GLint");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(v1), v1, 5, "glMapGrid2d", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(v2), v2, 6, "glMapGrid2d", "GLdouble");
  glMapGrid2d(XEN_TO_C_GLint(un), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(vn), XEN_TO_C_GLdouble(v1), 
              XEN_TO_C_GLdouble(v2));
  return(XEN_FALSE);
}

static XEN gxg_glMapGrid2f(XEN un, XEN u1, XEN u2, XEN vn, XEN v1, XEN v2)
{
  #define H_glMapGrid2f "void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)"
  XEN_ASSERT_TYPE(XEN_GLint_P(un), un, 1, "glMapGrid2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u1), u1, 2, "glMapGrid2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(u2), u2, 3, "glMapGrid2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLint_P(vn), vn, 4, "glMapGrid2f", "GLint");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(v1), v1, 5, "glMapGrid2f", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(v2), v2, 6, "glMapGrid2f", "GLfloat");
  glMapGrid2f(XEN_TO_C_GLint(un), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(vn), XEN_TO_C_GLfloat(v1), XEN_TO_C_GLfloat(v2));
  return(XEN_FALSE);
}

static XEN gxg_glEvalPoint1(XEN i)
{
  #define H_glEvalPoint1 "void glEvalPoint1(GLint i)"
  XEN_ASSERT_TYPE(XEN_GLint_P(i), i, 1, "glEvalPoint1", "GLint");
  glEvalPoint1(XEN_TO_C_GLint(i));
  return(XEN_FALSE);
}

static XEN gxg_glEvalPoint2(XEN i, XEN j)
{
  #define H_glEvalPoint2 "void glEvalPoint2(GLint i, GLint j)"
  XEN_ASSERT_TYPE(XEN_GLint_P(i), i, 1, "glEvalPoint2", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(j), j, 2, "glEvalPoint2", "GLint");
  glEvalPoint2(XEN_TO_C_GLint(i), XEN_TO_C_GLint(j));
  return(XEN_FALSE);
}

static XEN gxg_glEvalMesh1(XEN mode, XEN i1, XEN i2)
{
  #define H_glEvalMesh1 "void glEvalMesh1(GLenum mode, GLint i1, GLint i2)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glEvalMesh1", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(i1), i1, 2, "glEvalMesh1", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(i2), i2, 3, "glEvalMesh1", "GLint");
  glEvalMesh1(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(i1), XEN_TO_C_GLint(i2));
  return(XEN_FALSE);
}

static XEN gxg_glEvalMesh2(XEN mode, XEN i1, XEN i2, XEN j1, XEN j2)
{
  #define H_glEvalMesh2 "void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glEvalMesh2", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(i1), i1, 2, "glEvalMesh2", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(i2), i2, 3, "glEvalMesh2", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(j1), j1, 4, "glEvalMesh2", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(j2), j2, 5, "glEvalMesh2", "GLint");
  glEvalMesh2(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(i1), XEN_TO_C_GLint(i2), XEN_TO_C_GLint(j1), XEN_TO_C_GLint(j2));
  return(XEN_FALSE);
}

static XEN gxg_glFogf(XEN pname, XEN param)
{
  #define H_glFogf "void glFogf(GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glFogf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 2, "glFogf", "GLfloat");
  glFogf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glFogi(XEN pname, XEN param)
{
  #define H_glFogi "void glFogi(GLenum pname, GLint param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glFogi", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(param), param, 2, "glFogi", "GLint");
  glFogi(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(XEN_FALSE);
}

static XEN gxg_glFeedbackBuffer(XEN size, XEN type, XEN buffer)
{
  #define H_glFeedbackBuffer "void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat* buffer)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(size), size, 1, "glFeedbackBuffer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glFeedbackBuffer", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(buffer), buffer, 3, "glFeedbackBuffer", "GLfloat*");
  glFeedbackBuffer(XEN_TO_C_GLsizei(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLfloat_(buffer));
  return(XEN_FALSE);
}

static XEN gxg_glPassThrough(XEN token)
{
  #define H_glPassThrough "void glPassThrough(GLfloat token)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(token), token, 1, "glPassThrough", "GLfloat");
  glPassThrough(XEN_TO_C_GLfloat(token));
  return(XEN_FALSE);
}

static XEN gxg_glSelectBuffer(XEN size, XEN buffer)
{
  #define H_glSelectBuffer "void glSelectBuffer(GLsizei size, GLuint* buffer)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(size), size, 1, "glSelectBuffer", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(buffer), buffer, 2, "glSelectBuffer", "GLuint*");
  glSelectBuffer(XEN_TO_C_GLsizei(size), XEN_TO_C_GLuint_(buffer));
  return(XEN_FALSE);
}

static XEN gxg_glInitNames(void)
{
  #define H_glInitNames "void glInitNames( void)"
  glInitNames();
  return(XEN_FALSE);
}

static XEN gxg_glLoadName(XEN name)
{
  #define H_glLoadName "void glLoadName(GLuint name)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(name), name, 1, "glLoadName", "GLuint");
  glLoadName(XEN_TO_C_GLuint(name));
  return(XEN_FALSE);
}

static XEN gxg_glPushName(XEN name)
{
  #define H_glPushName "void glPushName(GLuint name)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(name), name, 1, "glPushName", "GLuint");
  glPushName(XEN_TO_C_GLuint(name));
  return(XEN_FALSE);
}

static XEN gxg_glPopName(void)
{
  #define H_glPopName "void glPopName( void)"
  glPopName();
  return(XEN_FALSE);
}

static XEN gxg_glDrawRangeElements(XEN mode, XEN start, XEN end, XEN count, XEN type, XEN indices)
{
  #define H_glDrawRangeElements "void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, \
GLenum type, GLvoid* indices)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glDrawRangeElements", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLuint_P(start), start, 2, "glDrawRangeElements", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLuint_P(end), end, 3, "glDrawRangeElements", "GLuint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 4, "glDrawRangeElements", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glDrawRangeElements", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(indices), indices, 6, "glDrawRangeElements", "GLvoid*");
  glDrawRangeElements(XEN_TO_C_GLenum(mode), XEN_TO_C_GLuint(start), XEN_TO_C_GLuint(end), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(type), 
                      XEN_TO_C_GLvoid_(indices));
  return(XEN_FALSE);
}

static XEN gxg_glTexImage3D(XEN arglist)
{
  #define H_glTexImage3D "void glTexImage3D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, internalFormat, width, height, depth, border, format, type, pixels;
  target = XEN_LIST_REF(arglist, 0);
  level = XEN_LIST_REF(arglist, 1);
  internalFormat = XEN_LIST_REF(arglist, 2);
  width = XEN_LIST_REF(arglist, 3);
  height = XEN_LIST_REF(arglist, 4);
  depth = XEN_LIST_REF(arglist, 5);
  border = XEN_LIST_REF(arglist, 6);
  format = XEN_LIST_REF(arglist, 7);
  type = XEN_LIST_REF(arglist, 8);
  pixels = XEN_LIST_REF(arglist, 9);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(internalFormat), internalFormat, 3, "glTexImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 4, "glTexImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 5, "glTexImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(depth), depth, 6, "glTexImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 7, "glTexImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 8, "glTexImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 9, "glTexImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 10, "glTexImage3D", "GLvoid*");
  glTexImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
               XEN_TO_C_GLsizei(depth), XEN_TO_C_GLint(border), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glTexSubImage3D(XEN arglist)
{
  #define H_glTexSubImage3D "void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, \
GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels;
  target = XEN_LIST_REF(arglist, 0);
  level = XEN_LIST_REF(arglist, 1);
  xoffset = XEN_LIST_REF(arglist, 2);
  yoffset = XEN_LIST_REF(arglist, 3);
  zoffset = XEN_LIST_REF(arglist, 4);
  width = XEN_LIST_REF(arglist, 5);
  height = XEN_LIST_REF(arglist, 6);
  depth = XEN_LIST_REF(arglist, 7);
  format = XEN_LIST_REF(arglist, 8);
  type = XEN_LIST_REF(arglist, 9);
  pixels = XEN_LIST_REF(arglist, 10);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexSubImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(zoffset), zoffset, 5, "glTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 6, "glTexSubImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 7, "glTexSubImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(depth), depth, 8, "glTexSubImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 9, "glTexSubImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 10, "glTexSubImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 11, "glTexSubImage3D", "GLvoid*");
  glTexSubImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(zoffset), 
                  XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                  XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexSubImage3D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN zoffset, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyTexSubImage3D "void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, \
GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexSubImage3D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(zoffset), zoffset, 5, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 6, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 7, "glCopyTexSubImage3D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 8, "glCopyTexSubImage3D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 9, "glCopyTexSubImage3D", "GLsizei");
  glCopyTexSubImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(zoffset), 
                      XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glColorTable(XEN target, XEN internalformat, XEN width, XEN format, XEN type, XEN table)
{
  #define H_glColorTable "void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, \
GLenum type, GLvoid* table)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glColorTable", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "glColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(table), table, 6, "glColorTable", "GLvoid*");
  glColorTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
               XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(table));
  return(XEN_FALSE);
}

static XEN gxg_glColorSubTable(XEN target, XEN start, XEN count, XEN format, XEN type, XEN data)
{
  #define H_glColorSubTable "void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, \
GLenum type, GLvoid* data)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glColorSubTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(start), start, 2, "glColorSubTable", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glColorSubTable", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "glColorSubTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glColorSubTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(data), data, 6, "glColorSubTable", "GLvoid*");
  glColorSubTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(start), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                  XEN_TO_C_GLvoid_(data));
  return(XEN_FALSE);
}

static XEN gxg_glCopyColorSubTable(XEN target, XEN start, XEN x, XEN y, XEN width)
{
  #define H_glCopyColorSubTable "void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, \
GLsizei width)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyColorSubTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(start), start, 2, "glCopyColorSubTable", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 3, "glCopyColorSubTable", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 4, "glCopyColorSubTable", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 5, "glCopyColorSubTable", "GLsizei");
  glCopyColorSubTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(start), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width));
  return(XEN_FALSE);
}

static XEN gxg_glCopyColorTable(XEN target, XEN internalformat, XEN x, XEN y, XEN width)
{
  #define H_glCopyColorTable "void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, \
GLsizei width)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glCopyColorTable", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 3, "glCopyColorTable", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 4, "glCopyColorTable", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 5, "glCopyColorTable", "GLsizei");
  glCopyColorTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width));
  return(XEN_FALSE);
}

static XEN gxg_glGetColorTableParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameterfv "void glGetColorTableParameterfv(GLenum target, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameterfv", "GLenum");
  glGetColorTableParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetColorTableParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameteriv "void glGetColorTableParameteriv(GLenum target, GLenum pname, \
GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameteriv", "GLenum");
  glGetColorTableParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glBlendEquation(XEN mode)
{
  #define H_glBlendEquation "void glBlendEquation(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glBlendEquation", "GLenum");
  glBlendEquation(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glBlendColor(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glBlendColor "void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"
  XEN_ASSERT_TYPE(XEN_GLclampf_P(red), red, 1, "glBlendColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(green), green, 2, "glBlendColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(blue), blue, 3, "glBlendColor", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(alpha), alpha, 4, "glBlendColor", "GLclampf");
  glBlendColor(XEN_TO_C_GLclampf(red), XEN_TO_C_GLclampf(green), XEN_TO_C_GLclampf(blue), XEN_TO_C_GLclampf(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glHistogram(XEN target, XEN width, XEN internalformat, XEN sink)
{
  #define H_glHistogram "void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glHistogram", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 2, "glHistogram", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 3, "glHistogram", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(sink), sink, 4, "glHistogram", "GLboolean");
  glHistogram(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLboolean(sink));
  return(XEN_FALSE);
}

static XEN gxg_glResetHistogram(XEN target)
{
  #define H_glResetHistogram "void glResetHistogram(GLenum target)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glResetHistogram", "GLenum");
  glResetHistogram(XEN_TO_C_GLenum(target));
  return(XEN_FALSE);
}

static XEN gxg_glGetHistogram(XEN target, XEN reset, XEN format, XEN type, XEN values)
{
  #define H_glGetHistogram "void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, \
GLvoid* values)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetHistogram", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(reset), reset, 2, "glGetHistogram", "GLboolean");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 3, "glGetHistogram", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 4, "glGetHistogram", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(values), values, 5, "glGetHistogram", "GLvoid*");
  glGetHistogram(XEN_TO_C_GLenum(target), XEN_TO_C_GLboolean(reset), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(values));
  return(XEN_FALSE);
}

static XEN gxg_glGetHistogramParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetHistogramParameterfv "void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetHistogramParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetHistogramParameterfv", "GLenum");
  glGetHistogramParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetHistogramParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetHistogramParameteriv "void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetHistogramParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetHistogramParameteriv", "GLenum");
  glGetHistogramParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glMinmax(XEN target, XEN internalformat, XEN sink)
{
  #define H_glMinmax "void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMinmax", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glMinmax", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(sink), sink, 3, "glMinmax", "GLboolean");
  glMinmax(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLboolean(sink));
  return(XEN_FALSE);
}

static XEN gxg_glResetMinmax(XEN target)
{
  #define H_glResetMinmax "void glResetMinmax(GLenum target)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glResetMinmax", "GLenum");
  glResetMinmax(XEN_TO_C_GLenum(target));
  return(XEN_FALSE);
}

static XEN gxg_glGetMinmax(XEN target, XEN reset, XEN format, XEN types, XEN values)
{
  #define H_glGetMinmax "void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum types, \
GLvoid* values)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMinmax", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLboolean_P(reset), reset, 2, "glGetMinmax", "GLboolean");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 3, "glGetMinmax", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(types), types, 4, "glGetMinmax", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(values), values, 5, "glGetMinmax", "GLvoid*");
  glGetMinmax(XEN_TO_C_GLenum(target), XEN_TO_C_GLboolean(reset), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(types), XEN_TO_C_GLvoid_(values));
  return(XEN_FALSE);
}

static XEN gxg_glGetMinmaxParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetMinmaxParameterfv "void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMinmaxParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMinmaxParameterfv", "GLenum");
  glGetMinmaxParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetMinmaxParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetMinmaxParameteriv "void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMinmaxParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMinmaxParameteriv", "GLenum");
  glGetMinmaxParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

static XEN gxg_glConvolutionFilter1D(XEN target, XEN internalformat, XEN width, XEN format, XEN type, XEN image)
{
  #define H_glConvolutionFilter1D "void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, \
GLenum format, GLenum type, GLvoid* image)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glConvolutionFilter1D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "glConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(image), image, 6, "glConvolutionFilter1D", "GLvoid*");
  glConvolutionFilter1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
                        XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(image));
  return(XEN_FALSE);
}

static XEN gxg_glConvolutionFilter2D(XEN target, XEN internalformat, XEN width, XEN height, XEN format, XEN type, XEN image)
{
  #define H_glConvolutionFilter2D "void glConvolutionFilter2D(GLenum target, GLenum internalformat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, GLvoid* image)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glConvolutionFilter2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glConvolutionFilter2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 5, "glConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 6, "glConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(image), image, 7, "glConvolutionFilter2D", "GLvoid*");
  glConvolutionFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
                        XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(image));
  return(XEN_FALSE);
}

static XEN gxg_glConvolutionParameterf(XEN target, XEN pname, XEN params)
{
  #define H_glConvolutionParameterf "void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glConvolutionParameterf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glConvolutionParameterf", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(params), params, 3, "glConvolutionParameterf", "GLfloat");
  glConvolutionParameterf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(params));
  return(XEN_FALSE);
}

static XEN gxg_glConvolutionParameteri(XEN target, XEN pname, XEN params)
{
  #define H_glConvolutionParameteri "void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glConvolutionParameteri", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glConvolutionParameteri", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(params), params, 3, "glConvolutionParameteri", "GLint");
  glConvolutionParameteri(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(params));
  return(XEN_FALSE);
}

static XEN gxg_glCopyConvolutionFilter1D(XEN target, XEN internalformat, XEN x, XEN y, XEN width)
{
  #define H_glCopyConvolutionFilter1D "void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, \
GLint x, GLint y, GLsizei width)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glCopyConvolutionFilter1D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 3, "glCopyConvolutionFilter1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 4, "glCopyConvolutionFilter1D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 5, "glCopyConvolutionFilter1D", "GLsizei");
  glCopyConvolutionFilter1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                            XEN_TO_C_GLsizei(width));
  return(XEN_FALSE);
}

static XEN gxg_glCopyConvolutionFilter2D(XEN target, XEN internalformat, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyConvolutionFilter2D "void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glCopyConvolutionFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 3, "glCopyConvolutionFilter2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 4, "glCopyConvolutionFilter2D", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 5, "glCopyConvolutionFilter2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 6, "glCopyConvolutionFilter2D", "GLsizei");
  glCopyConvolutionFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                            XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glSeparableFilter2D(XEN target, XEN internalformat, XEN width, XEN height, XEN format, XEN type, XEN row, XEN column)
{
  #define H_glSeparableFilter2D "void glSeparableFilter2D(GLenum target, GLenum internalformat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, GLvoid* row, GLvoid* column)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glSeparableFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glSeparableFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glSeparableFilter2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "glSeparableFilter2D", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 5, "glSeparableFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 6, "glSeparableFilter2D", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(row), row, 7, "glSeparableFilter2D", "GLvoid*");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(column), column, 8, "glSeparableFilter2D", "GLvoid*");
  glSeparableFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
                      XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(row), XEN_TO_C_GLvoid_(column));
  return(XEN_FALSE);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluBeginPolygon(XEN tess)
{
  #define H_gluBeginPolygon "void gluBeginPolygon(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluBeginPolygon", "GLUtesselator*");
  gluBeginPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

static XEN gxg_gluBuild1DMipmaps(XEN target, XEN internalFormat, XEN width, XEN format, XEN type, XEN data)
{
  #define H_gluBuild1DMipmaps "GLint gluBuild1DMipmaps(GLenum target, GLint internalFormat, GLsizei width, \
GLenum format, GLenum type, void* data)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "gluBuild1DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(internalFormat), internalFormat, 2, "gluBuild1DMipmaps", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "gluBuild1DMipmaps", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "gluBuild1DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "gluBuild1DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_void__P(data), data, 6, "gluBuild1DMipmaps", "void*");
  return(C_TO_XEN_GLint(gluBuild1DMipmaps(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                          XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild2DMipmaps(XEN target, XEN internalFormat, XEN width, XEN height, XEN format, XEN type, XEN data)
{
  #define H_gluBuild2DMipmaps "GLint gluBuild2DMipmaps(GLenum target, GLint internalFormat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, void* data)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "gluBuild2DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(internalFormat), internalFormat, 2, "gluBuild2DMipmaps", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "gluBuild2DMipmaps", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 4, "gluBuild2DMipmaps", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 5, "gluBuild2DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 6, "gluBuild2DMipmaps", "GLenum");
  XEN_ASSERT_TYPE(XEN_void__P(data), data, 7, "gluBuild2DMipmaps", "void*");
  return(C_TO_XEN_GLint(gluBuild2DMipmaps(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                          XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_void_(data))));
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluDeleteTess(XEN tess)
{
  #define H_gluDeleteTess "void gluDeleteTess(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluDeleteTess", "GLUtesselator*");
  gluDeleteTess(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluEndPolygon(XEN tess)
{
  #define H_gluEndPolygon "void gluEndPolygon(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluEndPolygon", "GLUtesselator*");
  gluEndPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

static XEN gxg_gluErrorString(XEN error)
{
  #define H_gluErrorString "constchar* gluErrorString(GLenum error)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(error), error, 1, "gluErrorString", "GLenum");
  return(C_TO_XEN_constchar_(gluErrorString(XEN_TO_C_GLenum(error))));
}

static XEN gxg_gluGetString(XEN name)
{
  #define H_gluGetString "constchar* gluGetString(GLenum name)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(name), name, 1, "gluGetString", "GLenum");
  return(C_TO_XEN_constchar_(gluGetString(XEN_TO_C_GLenum(name))));
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluGetTessProperty(XEN tess, XEN which, XEN data)
{
  #define H_gluGetTessProperty "void gluGetTessProperty(GLUtesselator* tess, GLenum which, GLdouble* data)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluGetTessProperty", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLenum_P(which), which, 2, "gluGetTessProperty", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(data), data, 3, "gluGetTessProperty", "GLdouble*");
  gluGetTessProperty(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(which), XEN_TO_C_GLdouble_(data));
  return(XEN_FALSE);
}
#endif

static XEN gxg_gluLookAt(XEN eyeX, XEN eyeY, XEN eyeZ, XEN centerX, XEN centerY, XEN centerZ, XEN upX, XEN upY, XEN upZ)
{
  #define H_gluLookAt "void gluLookAt(GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ, GLdouble centerX, \
GLdouble centerY, GLdouble centerZ, GLdouble upX, GLdouble upY, GLdouble upZ)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(eyeX), eyeX, 1, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(eyeY), eyeY, 2, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(eyeZ), eyeZ, 3, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(centerX), centerX, 4, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(centerY), centerY, 5, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(centerZ), centerZ, 6, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(upX), upX, 7, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(upY), upY, 8, "gluLookAt", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(upZ), upZ, 9, "gluLookAt", "GLdouble");
  gluLookAt(XEN_TO_C_GLdouble(eyeX), XEN_TO_C_GLdouble(eyeY), XEN_TO_C_GLdouble(eyeZ), XEN_TO_C_GLdouble(centerX), XEN_TO_C_GLdouble(centerY), 
            XEN_TO_C_GLdouble(centerZ), XEN_TO_C_GLdouble(upX), XEN_TO_C_GLdouble(upY), XEN_TO_C_GLdouble(upZ));
  return(XEN_FALSE);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluNewTess(void)
{
  #define H_gluNewTess "GLUtesselator* gluNewTess( void)"
  return(C_TO_XEN_GLUtesselator_(gluNewTess()));
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluNextContour(XEN tess, XEN type)
{
  #define H_gluNextContour "void gluNextContour(GLUtesselator* tess, GLenum type)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluNextContour", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "gluNextContour", "GLenum");
  gluNextContour(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(type));
  return(XEN_FALSE);
}
#endif

static XEN gxg_gluOrtho2D(XEN left, XEN right, XEN bottom, XEN top)
{
  #define H_gluOrtho2D "void gluOrtho2D(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(left), left, 1, "gluOrtho2D", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(right), right, 2, "gluOrtho2D", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(bottom), bottom, 3, "gluOrtho2D", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(top), top, 4, "gluOrtho2D", "GLdouble");
  gluOrtho2D(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top));
  return(XEN_FALSE);
}

static XEN gxg_gluPerspective(XEN fovy, XEN aspect, XEN zNear, XEN zFar)
{
  #define H_gluPerspective "void gluPerspective(GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(fovy), fovy, 1, "gluPerspective", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(aspect), aspect, 2, "gluPerspective", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(zNear), zNear, 3, "gluPerspective", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(zFar), zFar, 4, "gluPerspective", "GLdouble");
  gluPerspective(XEN_TO_C_GLdouble(fovy), XEN_TO_C_GLdouble(aspect), XEN_TO_C_GLdouble(zNear), XEN_TO_C_GLdouble(zFar));
  return(XEN_FALSE);
}

static XEN gxg_gluPickMatrix(XEN x, XEN y, XEN delX, XEN delY, XEN viewport)
{
  #define H_gluPickMatrix "void gluPickMatrix(GLdouble x, GLdouble y, GLdouble delX, GLdouble delY, GLint* viewport)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(x), x, 1, "gluPickMatrix", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(y), y, 2, "gluPickMatrix", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(delX), delX, 3, "gluPickMatrix", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(delY), delY, 4, "gluPickMatrix", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLint__P(viewport), viewport, 5, "gluPickMatrix", "GLint*");
  gluPickMatrix(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(delX), XEN_TO_C_GLdouble(delY), XEN_TO_C_GLint_(viewport));
  return(XEN_FALSE);
}

static XEN gxg_gluProject(XEN objX, XEN objY, XEN objZ, XEN model, XEN proj, XEN view, XEN winX, XEN winY, XEN winZ)
{
  #define H_gluProject "GLint gluProject(GLdouble objX, GLdouble objY, GLdouble objZ, GLdouble* model, \
GLdouble* proj, GLint* view, GLdouble* winX, GLdouble* winY, GLdouble* winZ)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(objX), objX, 1, "gluProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(objY), objY, 2, "gluProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(objZ), objZ, 3, "gluProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(model), model, 4, "gluProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(proj), proj, 5, "gluProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLint__P(view), view, 6, "gluProject", "GLint*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(winX), winX, 7, "gluProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(winY), winY, 8, "gluProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(winZ), winZ, 9, "gluProject", "GLdouble*");
  return(C_TO_XEN_GLint(gluProject(XEN_TO_C_GLdouble(objX), XEN_TO_C_GLdouble(objY), XEN_TO_C_GLdouble(objZ), XEN_TO_C_GLdouble_(model), 
                                   XEN_TO_C_GLdouble_(proj), XEN_TO_C_GLint_(view), XEN_TO_C_GLdouble_(winX), XEN_TO_C_GLdouble_(winY), 
                                   XEN_TO_C_GLdouble_(winZ))));
}

static XEN gxg_gluScaleImage(XEN format, XEN wIn, XEN hIn, XEN typeIn, XEN dataIn, XEN wOut, XEN hOut, XEN typeOut, XEN dataOut)
{
  #define H_gluScaleImage "GLint gluScaleImage(GLenum format, GLsizei wIn, GLsizei hIn, GLenum typeIn, \
void* dataIn, GLsizei wOut, GLsizei hOut, GLenum typeOut, GLvoid* dataOut)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 1, "gluScaleImage", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(wIn), wIn, 2, "gluScaleImage", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(hIn), hIn, 3, "gluScaleImage", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(typeIn), typeIn, 4, "gluScaleImage", "GLenum");
  XEN_ASSERT_TYPE(XEN_void__P(dataIn), dataIn, 5, "gluScaleImage", "void*");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(wOut), wOut, 6, "gluScaleImage", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(hOut), hOut, 7, "gluScaleImage", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(typeOut), typeOut, 8, "gluScaleImage", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(dataOut), dataOut, 9, "gluScaleImage", "GLvoid*");
  return(C_TO_XEN_GLint(gluScaleImage(XEN_TO_C_GLenum(format), XEN_TO_C_GLsizei(wIn), XEN_TO_C_GLsizei(hIn), XEN_TO_C_GLenum(typeIn), 
                                      XEN_TO_C_void_(dataIn), XEN_TO_C_GLsizei(wOut), XEN_TO_C_GLsizei(hOut), XEN_TO_C_GLenum(typeOut), 
                                      XEN_TO_C_GLvoid_(dataOut))));
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessBeginContour(XEN tess)
{
  #define H_gluTessBeginContour "void gluTessBeginContour(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessBeginContour", "GLUtesselator*");
  gluTessBeginContour(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessBeginPolygon(XEN tess, XEN data)
{
  #define H_gluTessBeginPolygon "void gluTessBeginPolygon(GLUtesselator* tess, GLvoid* data)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessBeginPolygon", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(data), data, 2, "gluTessBeginPolygon", "GLvoid*");
  gluTessBeginPolygon(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLvoid_(data));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessEndContour(XEN tess)
{
  #define H_gluTessEndContour "void gluTessEndContour(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessEndContour", "GLUtesselator*");
  gluTessEndContour(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessEndPolygon(XEN tess)
{
  #define H_gluTessEndPolygon "void gluTessEndPolygon(GLUtesselator* tess)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessEndPolygon", "GLUtesselator*");
  gluTessEndPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessNormal(XEN tess, XEN valueX, XEN valueY, XEN valueZ)
{
  #define H_gluTessNormal "void gluTessNormal(GLUtesselator* tess, GLdouble valueX, GLdouble valueY, \
GLdouble valueZ)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessNormal", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(valueX), valueX, 2, "gluTessNormal", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(valueY), valueY, 3, "gluTessNormal", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(valueZ), valueZ, 4, "gluTessNormal", "GLdouble");
  gluTessNormal(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLdouble(valueX), XEN_TO_C_GLdouble(valueY), XEN_TO_C_GLdouble(valueZ));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessProperty(XEN tess, XEN which, XEN data)
{
  #define H_gluTessProperty "void gluTessProperty(GLUtesselator* tess, GLenum which, GLdouble data)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessProperty", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLenum_P(which), which, 2, "gluTessProperty", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(data), data, 3, "gluTessProperty", "GLdouble");
  gluTessProperty(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(which), XEN_TO_C_GLdouble(data));
  return(XEN_FALSE);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessVertex(XEN tess, XEN location, XEN data)
{
  #define H_gluTessVertex "void gluTessVertex(GLUtesselator* tess, GLdouble* location, GLvoid* data)"
  XEN_ASSERT_TYPE(XEN_GLUtesselator__P(tess), tess, 1, "gluTessVertex", "GLUtesselator*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(location), location, 2, "gluTessVertex", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(data), data, 3, "gluTessVertex", "GLvoid*");
  gluTessVertex(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLdouble_(location), XEN_TO_C_GLvoid_(data));
  return(XEN_FALSE);
}
#endif

static XEN gxg_gluUnProject(XEN winX, XEN winY, XEN winZ, XEN model, XEN proj, XEN view, XEN objX, XEN objY, XEN objZ)
{
  #define H_gluUnProject "GLint gluUnProject(GLdouble winX, GLdouble winY, GLdouble winZ, GLdouble* model, \
GLdouble* proj, GLint* view, GLdouble* objX, GLdouble* objY, GLdouble* objZ)"
  XEN_ASSERT_TYPE(XEN_GLdouble_P(winX), winX, 1, "gluUnProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(winY), winY, 2, "gluUnProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(winZ), winZ, 3, "gluUnProject", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(model), model, 4, "gluUnProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(proj), proj, 5, "gluUnProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLint__P(view), view, 6, "gluUnProject", "GLint*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(objX), objX, 7, "gluUnProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(objY), objY, 8, "gluUnProject", "GLdouble*");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(objZ), objZ, 9, "gluUnProject", "GLdouble*");
  return(C_TO_XEN_GLint(gluUnProject(XEN_TO_C_GLdouble(winX), XEN_TO_C_GLdouble(winY), XEN_TO_C_GLdouble(winZ), XEN_TO_C_GLdouble_(model), 
                                     XEN_TO_C_GLdouble_(proj), XEN_TO_C_GLint_(view), XEN_TO_C_GLdouble_(objX), XEN_TO_C_GLdouble_(objY), 
                                     XEN_TO_C_GLdouble_(objZ))));
}

static XEN gxg_glActiveTextureARB(XEN texture)
{
  #define H_glActiveTextureARB "void glActiveTextureARB(GLenum texture)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(texture), texture, 1, "glActiveTextureARB", "GLenum");
  glActiveTextureARB(XEN_TO_C_GLenum(texture));
  return(XEN_FALSE);
}

static XEN gxg_glClientActiveTextureARB(XEN texture)
{
  #define H_glClientActiveTextureARB "void glClientActiveTextureARB(GLenum texture)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(texture), texture, 1, "glClientActiveTextureARB", "GLenum");
  glClientActiveTextureARB(XEN_TO_C_GLenum(texture));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1dARB(XEN target, XEN s)
{
  #define H_glMultiTexCoord1dARB "void glMultiTexCoord1dARB(GLenum target, GLdouble s)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1dARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 2, "glMultiTexCoord1dARB", "GLdouble");
  glMultiTexCoord1dARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(s));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1dvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord1dvARB "void glMultiTexCoord1dvARB(GLenum target, GLdouble* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1dvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(v), v, 2, "glMultiTexCoord1dvARB", "GLdouble*");
  glMultiTexCoord1dvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1fARB(XEN target, XEN s)
{
  #define H_glMultiTexCoord1fARB "void glMultiTexCoord1fARB(GLenum target, GLfloat s)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1fARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 2, "glMultiTexCoord1fARB", "GLfloat");
  glMultiTexCoord1fARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(s));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1fvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord1fvARB "void glMultiTexCoord1fvARB(GLenum target, GLfloat* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1fvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(v), v, 2, "glMultiTexCoord1fvARB", "GLfloat*");
  glMultiTexCoord1fvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1iARB(XEN target, XEN s)
{
  #define H_glMultiTexCoord1iARB "void glMultiTexCoord1iARB(GLenum target, GLint s)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1iARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 2, "glMultiTexCoord1iARB", "GLint");
  glMultiTexCoord1iARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(s));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1ivARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord1ivARB "void glMultiTexCoord1ivARB(GLenum target, GLint* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1ivARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint__P(v), v, 2, "glMultiTexCoord1ivARB", "GLint*");
  glMultiTexCoord1ivARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1sARB(XEN target, XEN s)
{
  #define H_glMultiTexCoord1sARB "void glMultiTexCoord1sARB(GLenum target, GLshort s)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1sARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 2, "glMultiTexCoord1sARB", "GLshort");
  glMultiTexCoord1sARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort(s));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord1svARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord1svARB "void glMultiTexCoord1svARB(GLenum target, GLshort* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord1svARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort__P(v), v, 2, "glMultiTexCoord1svARB", "GLshort*");
  glMultiTexCoord1svARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2dARB(XEN target, XEN s, XEN t)
{
  #define H_glMultiTexCoord2dARB "void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2dARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 2, "glMultiTexCoord2dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 3, "glMultiTexCoord2dARB", "GLdouble");
  glMultiTexCoord2dARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2dvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord2dvARB "void glMultiTexCoord2dvARB(GLenum target, GLdouble* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2dvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(v), v, 2, "glMultiTexCoord2dvARB", "GLdouble*");
  glMultiTexCoord2dvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2fARB(XEN target, XEN s, XEN t)
{
  #define H_glMultiTexCoord2fARB "void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2fARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 2, "glMultiTexCoord2fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 3, "glMultiTexCoord2fARB", "GLfloat");
  glMultiTexCoord2fARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2fvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord2fvARB "void glMultiTexCoord2fvARB(GLenum target, GLfloat* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2fvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(v), v, 2, "glMultiTexCoord2fvARB", "GLfloat*");
  glMultiTexCoord2fvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2iARB(XEN target, XEN s, XEN t)
{
  #define H_glMultiTexCoord2iARB "void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2iARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 2, "glMultiTexCoord2iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 3, "glMultiTexCoord2iARB", "GLint");
  glMultiTexCoord2iARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(s), XEN_TO_C_GLint(t));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2ivARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord2ivARB "void glMultiTexCoord2ivARB(GLenum target, GLint* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2ivARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint__P(v), v, 2, "glMultiTexCoord2ivARB", "GLint*");
  glMultiTexCoord2ivARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2sARB(XEN target, XEN s, XEN t)
{
  #define H_glMultiTexCoord2sARB "void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2sARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 2, "glMultiTexCoord2sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 3, "glMultiTexCoord2sARB", "GLshort");
  glMultiTexCoord2sARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord2svARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord2svARB "void glMultiTexCoord2svARB(GLenum target, GLshort* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord2svARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort__P(v), v, 2, "glMultiTexCoord2svARB", "GLshort*");
  glMultiTexCoord2svARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3dARB(XEN target, XEN s, XEN t, XEN r)
{
  #define H_glMultiTexCoord3dARB "void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3dARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 2, "glMultiTexCoord3dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 3, "glMultiTexCoord3dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(r), r, 4, "glMultiTexCoord3dARB", "GLdouble");
  glMultiTexCoord3dARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3dvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord3dvARB "void glMultiTexCoord3dvARB(GLenum target, GLdouble* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3dvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(v), v, 2, "glMultiTexCoord3dvARB", "GLdouble*");
  glMultiTexCoord3dvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3fARB(XEN target, XEN s, XEN t, XEN r)
{
  #define H_glMultiTexCoord3fARB "void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3fARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 2, "glMultiTexCoord3fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 3, "glMultiTexCoord3fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(r), r, 4, "glMultiTexCoord3fARB", "GLfloat");
  glMultiTexCoord3fARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3fvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord3fvARB "void glMultiTexCoord3fvARB(GLenum target, GLfloat* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3fvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(v), v, 2, "glMultiTexCoord3fvARB", "GLfloat*");
  glMultiTexCoord3fvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3iARB(XEN target, XEN s, XEN t, XEN r)
{
  #define H_glMultiTexCoord3iARB "void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3iARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 2, "glMultiTexCoord3iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 3, "glMultiTexCoord3iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(r), r, 4, "glMultiTexCoord3iARB", "GLint");
  glMultiTexCoord3iARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3ivARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord3ivARB "void glMultiTexCoord3ivARB(GLenum target, GLint* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3ivARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint__P(v), v, 2, "glMultiTexCoord3ivARB", "GLint*");
  glMultiTexCoord3ivARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3sARB(XEN target, XEN s, XEN t, XEN r)
{
  #define H_glMultiTexCoord3sARB "void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3sARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 2, "glMultiTexCoord3sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 3, "glMultiTexCoord3sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(r), r, 4, "glMultiTexCoord3sARB", "GLshort");
  glMultiTexCoord3sARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord3svARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord3svARB "void glMultiTexCoord3svARB(GLenum target, GLshort* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord3svARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort__P(v), v, 2, "glMultiTexCoord3svARB", "GLshort*");
  glMultiTexCoord3svARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4dARB(XEN target, XEN s, XEN t, XEN r, XEN q)
{
  #define H_glMultiTexCoord4dARB "void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, \
GLdouble q)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4dARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(s), s, 2, "glMultiTexCoord4dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(t), t, 3, "glMultiTexCoord4dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(r), r, 4, "glMultiTexCoord4dARB", "GLdouble");
  XEN_ASSERT_TYPE(XEN_GLdouble_P(q), q, 5, "glMultiTexCoord4dARB", "GLdouble");
  glMultiTexCoord4dARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r), XEN_TO_C_GLdouble(q));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4dvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord4dvARB "void glMultiTexCoord4dvARB(GLenum target, GLdouble* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4dvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLdouble__P(v), v, 2, "glMultiTexCoord4dvARB", "GLdouble*");
  glMultiTexCoord4dvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4fARB(XEN target, XEN s, XEN t, XEN r, XEN q)
{
  #define H_glMultiTexCoord4fARB "void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, \
GLfloat q)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4fARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(s), s, 2, "glMultiTexCoord4fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(t), t, 3, "glMultiTexCoord4fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(r), r, 4, "glMultiTexCoord4fARB", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(q), q, 5, "glMultiTexCoord4fARB", "GLfloat");
  glMultiTexCoord4fARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r), XEN_TO_C_GLfloat(q));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4fvARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord4fvARB "void glMultiTexCoord4fvARB(GLenum target, GLfloat* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4fvARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(v), v, 2, "glMultiTexCoord4fvARB", "GLfloat*");
  glMultiTexCoord4fvARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4iARB(XEN target, XEN s, XEN t, XEN r, XEN q)
{
  #define H_glMultiTexCoord4iARB "void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, \
GLint q)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4iARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(s), s, 2, "glMultiTexCoord4iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(t), t, 3, "glMultiTexCoord4iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(r), r, 4, "glMultiTexCoord4iARB", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(q), q, 5, "glMultiTexCoord4iARB", "GLint");
  glMultiTexCoord4iARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r), XEN_TO_C_GLint(q));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4ivARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord4ivARB "void glMultiTexCoord4ivARB(GLenum target, GLint* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4ivARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint__P(v), v, 2, "glMultiTexCoord4ivARB", "GLint*");
  glMultiTexCoord4ivARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLint_(v));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4sARB(XEN target, XEN s, XEN t, XEN r, XEN q)
{
  #define H_glMultiTexCoord4sARB "void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, \
GLshort q)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4sARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort_P(s), s, 2, "glMultiTexCoord4sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(t), t, 3, "glMultiTexCoord4sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(r), r, 4, "glMultiTexCoord4sARB", "GLshort");
  XEN_ASSERT_TYPE(XEN_GLshort_P(q), q, 5, "glMultiTexCoord4sARB", "GLshort");
  glMultiTexCoord4sARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r), XEN_TO_C_GLshort(q));
  return(XEN_FALSE);
}

static XEN gxg_glMultiTexCoord4svARB(XEN target, XEN v)
{
  #define H_glMultiTexCoord4svARB "void glMultiTexCoord4svARB(GLenum target, GLshort* v)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glMultiTexCoord4svARB", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLshort__P(v), v, 2, "glMultiTexCoord4svARB", "GLshort*");
  glMultiTexCoord4svARB(XEN_TO_C_GLenum(target), XEN_TO_C_GLshort_(v));
  return(XEN_FALSE);
}

static XEN gxg_glLockArraysEXT(XEN first, XEN count)
{
  #define H_glLockArraysEXT "void glLockArraysEXT(GLint first, GLsizei count)"
  XEN_ASSERT_TYPE(XEN_GLint_P(first), first, 1, "glLockArraysEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 2, "glLockArraysEXT", "GLsizei");
  glLockArraysEXT(XEN_TO_C_GLint(first), XEN_TO_C_GLsizei(count));
  return(XEN_FALSE);
}

static XEN gxg_glUnlockArraysEXT(void)
{
  #define H_glUnlockArraysEXT "void glUnlockArraysEXT( void)"
  glUnlockArraysEXT();
  return(XEN_FALSE);
}

static XEN gxg_glBlendColorEXT(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glBlendColorEXT "void glBlendColorEXT(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"
  XEN_ASSERT_TYPE(XEN_GLclampf_P(red), red, 1, "glBlendColorEXT", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(green), green, 2, "glBlendColorEXT", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(blue), blue, 3, "glBlendColorEXT", "GLclampf");
  XEN_ASSERT_TYPE(XEN_GLclampf_P(alpha), alpha, 4, "glBlendColorEXT", "GLclampf");
  glBlendColorEXT(XEN_TO_C_GLclampf(red), XEN_TO_C_GLclampf(green), XEN_TO_C_GLclampf(blue), XEN_TO_C_GLclampf(alpha));
  return(XEN_FALSE);
}

static XEN gxg_glPolygonOffsetEXT(XEN factor, XEN bias)
{
  #define H_glPolygonOffsetEXT "void glPolygonOffsetEXT(GLfloat factor, GLfloat bias)"
  XEN_ASSERT_TYPE(XEN_GLfloat_P(factor), factor, 1, "glPolygonOffsetEXT", "GLfloat");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(bias), bias, 2, "glPolygonOffsetEXT", "GLfloat");
  glPolygonOffsetEXT(XEN_TO_C_GLfloat(factor), XEN_TO_C_GLfloat(bias));
  return(XEN_FALSE);
}

static XEN gxg_glTexImage3DEXT(XEN arglist)
{
  #define H_glTexImage3DEXT "void glTexImage3DEXT(GLenum target, GLint level, GLenum internalFormat, \
GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, internalFormat, width, height, depth, border, format, type, pixels;
  target = XEN_LIST_REF(arglist, 0);
  level = XEN_LIST_REF(arglist, 1);
  internalFormat = XEN_LIST_REF(arglist, 2);
  width = XEN_LIST_REF(arglist, 3);
  height = XEN_LIST_REF(arglist, 4);
  depth = XEN_LIST_REF(arglist, 5);
  border = XEN_LIST_REF(arglist, 6);
  format = XEN_LIST_REF(arglist, 7);
  type = XEN_LIST_REF(arglist, 8);
  pixels = XEN_LIST_REF(arglist, 9);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalFormat), internalFormat, 3, "glTexImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 4, "glTexImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 5, "glTexImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(depth), depth, 6, "glTexImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLint_P(border), border, 7, "glTexImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 8, "glTexImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 9, "glTexImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 10, "glTexImage3DEXT", "GLvoid*");
  glTexImage3DEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(internalFormat), XEN_TO_C_GLsizei(width), 
                  XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLint(border), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                  XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glTexSubImage3DEXT(XEN arglist)
{
  #define H_glTexSubImage3DEXT "void glTexSubImage3DEXT(GLenum target, GLint level, GLint xoffset, GLint yoffset, \
GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels;
  target = XEN_LIST_REF(arglist, 0);
  level = XEN_LIST_REF(arglist, 1);
  xoffset = XEN_LIST_REF(arglist, 2);
  yoffset = XEN_LIST_REF(arglist, 3);
  zoffset = XEN_LIST_REF(arglist, 4);
  width = XEN_LIST_REF(arglist, 5);
  height = XEN_LIST_REF(arglist, 6);
  depth = XEN_LIST_REF(arglist, 7);
  format = XEN_LIST_REF(arglist, 8);
  type = XEN_LIST_REF(arglist, 9);
  pixels = XEN_LIST_REF(arglist, 10);
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glTexSubImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(zoffset), zoffset, 5, "glTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 6, "glTexSubImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 7, "glTexSubImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(depth), depth, 8, "glTexSubImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 9, "glTexSubImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 10, "glTexSubImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(pixels), pixels, 11, "glTexSubImage3DEXT", "GLvoid*");
  glTexSubImage3DEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(zoffset), 
                     XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLenum(format), 
                     XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(XEN_FALSE);
}

static XEN gxg_glCopyTexSubImage3DEXT(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN zoffset, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyTexSubImage3DEXT "void glCopyTexSubImage3DEXT(GLenum target, GLint level, GLint xoffset, \
GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glCopyTexSubImage3DEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(xoffset), xoffset, 3, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(yoffset), yoffset, 4, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(zoffset), zoffset, 5, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(x), x, 6, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLint_P(y), y, 7, "glCopyTexSubImage3DEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 8, "glCopyTexSubImage3DEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(height), height, 9, "glCopyTexSubImage3DEXT", "GLsizei");
  glCopyTexSubImage3DEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), 
                         XEN_TO_C_GLint(zoffset), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(XEN_FALSE);
}

static XEN gxg_glGenTexturesEXT(XEN n, XEN textures)
{
  #define H_glGenTexturesEXT "void glGenTexturesEXT(GLsizei n, GLuint* textures)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glGenTexturesEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glGenTexturesEXT", "GLuint*");
  glGenTexturesEXT(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(XEN_FALSE);
}

static XEN gxg_glDeleteTexturesEXT(XEN n, XEN textures)
{
  #define H_glDeleteTexturesEXT "void glDeleteTexturesEXT(GLsizei n, GLuint* textures)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glDeleteTexturesEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glDeleteTexturesEXT", "GLuint*");
  glDeleteTexturesEXT(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(XEN_FALSE);
}

static XEN gxg_glBindTextureEXT(XEN target, XEN texture)
{
  #define H_glBindTextureEXT "void glBindTextureEXT(GLenum target, GLuint texture)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glBindTextureEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLuint_P(texture), texture, 2, "glBindTextureEXT", "GLuint");
  glBindTextureEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLuint(texture));
  return(XEN_FALSE);
}

static XEN gxg_glAreTexturesResidentEXT(XEN n, XEN textures, XEN residences)
{
  #define H_glAreTexturesResidentEXT "GLboolean glAreTexturesResidentEXT(GLsizei n, GLuint* textures, \
GLboolean* residences)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(n), n, 1, "glAreTexturesResidentEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLuint__P(textures), textures, 2, "glAreTexturesResidentEXT", "GLuint*");
  XEN_ASSERT_TYPE(XEN_GLboolean__P(residences), residences, 3, "glAreTexturesResidentEXT", "GLboolean*");
  return(C_TO_XEN_GLboolean(glAreTexturesResidentEXT(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures), XEN_TO_C_GLboolean_(residences))));
}

static XEN gxg_glIsTextureEXT(XEN texture)
{
  #define H_glIsTextureEXT "GLboolean glIsTextureEXT(GLuint texture)"
  XEN_ASSERT_TYPE(XEN_GLuint_P(texture), texture, 1, "glIsTextureEXT", "GLuint");
  return(C_TO_XEN_GLboolean(glIsTextureEXT(XEN_TO_C_GLuint(texture))));
}

static XEN gxg_glVertexPointerEXT(XEN size, XEN type, XEN stride, XEN count, XEN ptr)
{
  #define H_glVertexPointerEXT "void glVertexPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, \
GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glVertexPointerEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glVertexPointerEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glVertexPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 4, "glVertexPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 5, "glVertexPointerEXT", "GLvoid*");
  glVertexPointerEXT(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glNormalPointerEXT(XEN type, XEN stride, XEN count, XEN ptr)
{
  #define H_glNormalPointerEXT "void glNormalPointerEXT(GLenum type, GLsizei stride, GLsizei count, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 1, "glNormalPointerEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 2, "glNormalPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glNormalPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 4, "glNormalPointerEXT", "GLvoid*");
  glNormalPointerEXT(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glColorPointerEXT(XEN size, XEN type, XEN stride, XEN count, XEN ptr)
{
  #define H_glColorPointerEXT "void glColorPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, \
GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glColorPointerEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glColorPointerEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glColorPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 4, "glColorPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 5, "glColorPointerEXT", "GLvoid*");
  glColorPointerEXT(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glIndexPointerEXT(XEN type, XEN stride, XEN count, XEN ptr)
{
  #define H_glIndexPointerEXT "void glIndexPointerEXT(GLenum type, GLsizei stride, GLsizei count, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 1, "glIndexPointerEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 2, "glIndexPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glIndexPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 4, "glIndexPointerEXT", "GLvoid*");
  glIndexPointerEXT(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glTexCoordPointerEXT(XEN size, XEN type, XEN stride, XEN count, XEN ptr)
{
  #define H_glTexCoordPointerEXT "void glTexCoordPointerEXT(GLint size, GLenum type, GLsizei stride, \
GLsizei count, GLvoid* ptr)"
  XEN_ASSERT_TYPE(XEN_GLint_P(size), size, 1, "glTexCoordPointerEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 2, "glTexCoordPointerEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 3, "glTexCoordPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 4, "glTexCoordPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(ptr), ptr, 5, "glTexCoordPointerEXT", "GLvoid*");
  glTexCoordPointerEXT(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLvoid_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glEdgeFlagPointerEXT(XEN stride, XEN count, XEN ptr)
{
  #define H_glEdgeFlagPointerEXT "void glEdgeFlagPointerEXT(GLsizei stride, GLsizei count, GLboolean* ptr)"
  XEN_ASSERT_TYPE(XEN_GLsizei_P(stride), stride, 1, "glEdgeFlagPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 2, "glEdgeFlagPointerEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLboolean__P(ptr), ptr, 3, "glEdgeFlagPointerEXT", "GLboolean*");
  glEdgeFlagPointerEXT(XEN_TO_C_GLsizei(stride), XEN_TO_C_GLsizei(count), XEN_TO_C_GLboolean_(ptr));
  return(XEN_FALSE);
}

static XEN gxg_glGetPointervEXT(XEN pname, XEN params)
{
  #define H_glGetPointervEXT "void glGetPointervEXT(GLenum pname, void** [params])"
  void* ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetPointervEXT", "GLenum");
  glGetPointervEXT(XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_void_(ref_params)));
}

static XEN gxg_glArrayElementEXT(XEN i)
{
  #define H_glArrayElementEXT "void glArrayElementEXT(GLint i)"
  XEN_ASSERT_TYPE(XEN_GLint_P(i), i, 1, "glArrayElementEXT", "GLint");
  glArrayElementEXT(XEN_TO_C_GLint(i));
  return(XEN_FALSE);
}

static XEN gxg_glDrawArraysEXT(XEN mode, XEN first, XEN count)
{
  #define H_glDrawArraysEXT "void glDrawArraysEXT(GLenum mode, GLint first, GLsizei count)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glDrawArraysEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(first), first, 2, "glDrawArraysEXT", "GLint");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glDrawArraysEXT", "GLsizei");
  glDrawArraysEXT(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(first), XEN_TO_C_GLsizei(count));
  return(XEN_FALSE);
}

static XEN gxg_glBlendEquationEXT(XEN mode)
{
  #define H_glBlendEquationEXT "void glBlendEquationEXT(GLenum mode)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(mode), mode, 1, "glBlendEquationEXT", "GLenum");
  glBlendEquationEXT(XEN_TO_C_GLenum(mode));
  return(XEN_FALSE);
}

static XEN gxg_glPointParameterfEXT(XEN pname, XEN param)
{
  #define H_glPointParameterfEXT "void glPointParameterfEXT(GLenum pname, GLfloat param)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPointParameterfEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat_P(param), param, 2, "glPointParameterfEXT", "GLfloat");
  glPointParameterfEXT(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(XEN_FALSE);
}

static XEN gxg_glPointParameterfvEXT(XEN pname, XEN params)
{
  #define H_glPointParameterfvEXT "void glPointParameterfvEXT(GLenum pname, GLfloat* params)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glPointParameterfvEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLfloat__P(params), params, 2, "glPointParameterfvEXT", "GLfloat*");
  glPointParameterfvEXT(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat_(params));
  return(XEN_FALSE);
}

static XEN gxg_glColorTableEXT(XEN target, XEN internalformat, XEN width, XEN format, XEN type, XEN table)
{
  #define H_glColorTableEXT "void glColorTableEXT(GLenum target, GLenum internalformat, GLsizei width, \
GLenum format, GLenum type, GLvoid* table)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glColorTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(internalformat), internalformat, 2, "glColorTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(width), width, 3, "glColorTableEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "glColorTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glColorTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(table), table, 6, "glColorTableEXT", "GLvoid*");
  glColorTableEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
                  XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(table));
  return(XEN_FALSE);
}

static XEN gxg_glColorSubTableEXT(XEN target, XEN start, XEN count, XEN format, XEN type, XEN data)
{
  #define H_glColorSubTableEXT "void glColorSubTableEXT(GLenum target, GLsizei start, GLsizei count, \
GLenum format, GLenum type, GLvoid* data)"
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glColorSubTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(start), start, 2, "glColorSubTableEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLsizei_P(count), count, 3, "glColorSubTableEXT", "GLsizei");
  XEN_ASSERT_TYPE(XEN_GLenum_P(format), format, 4, "glColorSubTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(type), type, 5, "glColorSubTableEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLvoid__P(data), data, 6, "glColorSubTableEXT", "GLvoid*");
  glColorSubTableEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(start), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(format), 
                     XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(data));
  return(XEN_FALSE);
}

static XEN gxg_glGetColorTableParameterfvEXT(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameterfvEXT "void glGetColorTableParameterfvEXT(GLenum target, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameterfvEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameterfvEXT", "GLenum");
  glGetColorTableParameterfvEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params)));
}

static XEN gxg_glGetColorTableParameterivEXT(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameterivEXT "void glGetColorTableParameterivEXT(GLenum target, GLenum pname, \
GLint* [params])"
  GLint ref_params;
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameterivEXT", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameterivEXT", "GLenum");
  glGetColorTableParameterivEXT(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), &ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params)));
}

#if HAVE_GUILE
static void define_functions(void)
{
#if USE_MOTIF
  XEN_DEFINE_PROCEDURE(XL_PRE "glXChooseVisual" XL_POST, gxg_glXChooseVisual, 3, 0, 0, H_glXChooseVisual);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXCopyContext" XL_POST, gxg_glXCopyContext, 4, 0, 0, H_glXCopyContext);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXCreateContext" XL_POST, gxg_glXCreateContext, 4, 0, 0, H_glXCreateContext);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXCreateGLXPixmap" XL_POST, gxg_glXCreateGLXPixmap, 3, 0, 0, H_glXCreateGLXPixmap);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXDestroyContext" XL_POST, gxg_glXDestroyContext, 2, 0, 0, H_glXDestroyContext);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXDestroyGLXPixmap" XL_POST, gxg_glXDestroyGLXPixmap, 2, 0, 0, H_glXDestroyGLXPixmap);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXGetConfig" XL_POST, gxg_glXGetConfig, 3, 1, 0, H_glXGetConfig);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXGetCurrentContext" XL_POST, gxg_glXGetCurrentContext, 0, 0, 0, H_glXGetCurrentContext);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXGetCurrentDrawable" XL_POST, gxg_glXGetCurrentDrawable, 0, 0, 0, H_glXGetCurrentDrawable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXIsDirect" XL_POST, gxg_glXIsDirect, 2, 0, 0, H_glXIsDirect);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXMakeCurrent" XL_POST, gxg_glXMakeCurrent, 3, 0, 0, H_glXMakeCurrent);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXQueryExtension" XL_POST, gxg_glXQueryExtension, 1, 2, 0, H_glXQueryExtension);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXQueryVersion" XL_POST, gxg_glXQueryVersion, 1, 2, 0, H_glXQueryVersion);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXSwapBuffers" XL_POST, gxg_glXSwapBuffers, 2, 0, 0, H_glXSwapBuffers);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXUseXFont" XL_POST, gxg_glXUseXFont, 4, 0, 0, H_glXUseXFont);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXWaitGL" XL_POST, gxg_glXWaitGL, 0, 0, 0, H_glXWaitGL);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXWaitX" XL_POST, gxg_glXWaitX, 0, 0, 0, H_glXWaitX);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXGetClientString" XL_POST, gxg_glXGetClientString, 2, 0, 0, H_glXGetClientString);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXQueryServerString" XL_POST, gxg_glXQueryServerString, 3, 0, 0, H_glXQueryServerString);
  XEN_DEFINE_PROCEDURE(XL_PRE "glXQueryExtensionsString" XL_POST, gxg_glXQueryExtensionsString, 2, 0, 0, H_glXQueryExtensionsString);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "glClearIndex" XL_POST, gxg_glClearIndex, 1, 0, 0, H_glClearIndex);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClearColor" XL_POST, gxg_glClearColor, 4, 0, 0, H_glClearColor);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClear" XL_POST, gxg_glClear, 1, 0, 0, H_glClear);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexMask" XL_POST, gxg_glIndexMask, 1, 0, 0, H_glIndexMask);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorMask" XL_POST, gxg_glColorMask, 4, 0, 0, H_glColorMask);
  XEN_DEFINE_PROCEDURE(XL_PRE "glAlphaFunc" XL_POST, gxg_glAlphaFunc, 2, 0, 0, H_glAlphaFunc);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBlendFunc" XL_POST, gxg_glBlendFunc, 2, 0, 0, H_glBlendFunc);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLogicOp" XL_POST, gxg_glLogicOp, 1, 0, 0, H_glLogicOp);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCullFace" XL_POST, gxg_glCullFace, 1, 0, 0, H_glCullFace);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFrontFace" XL_POST, gxg_glFrontFace, 1, 0, 0, H_glFrontFace);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPointSize" XL_POST, gxg_glPointSize, 1, 0, 0, H_glPointSize);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLineWidth" XL_POST, gxg_glLineWidth, 1, 0, 0, H_glLineWidth);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLineStipple" XL_POST, gxg_glLineStipple, 2, 0, 0, H_glLineStipple);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPolygonMode" XL_POST, gxg_glPolygonMode, 2, 0, 0, H_glPolygonMode);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPolygonOffset" XL_POST, gxg_glPolygonOffset, 2, 0, 0, H_glPolygonOffset);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPolygonStipple" XL_POST, gxg_glPolygonStipple, 1, 0, 0, H_glPolygonStipple);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPolygonStipple" XL_POST, gxg_glGetPolygonStipple, 0, 1, 0, H_glGetPolygonStipple);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEdgeFlag" XL_POST, gxg_glEdgeFlag, 1, 0, 0, H_glEdgeFlag);
  XEN_DEFINE_PROCEDURE(XL_PRE "glScissor" XL_POST, gxg_glScissor, 4, 0, 0, H_glScissor);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClipPlane" XL_POST, gxg_glClipPlane, 2, 0, 0, H_glClipPlane);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetClipPlane" XL_POST, gxg_glGetClipPlane, 1, 1, 0, H_glGetClipPlane);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawBuffer" XL_POST, gxg_glDrawBuffer, 1, 0, 0, H_glDrawBuffer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glReadBuffer" XL_POST, gxg_glReadBuffer, 1, 0, 0, H_glReadBuffer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEnable" XL_POST, gxg_glEnable, 1, 0, 0, H_glEnable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDisable" XL_POST, gxg_glDisable, 1, 0, 0, H_glDisable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIsEnabled" XL_POST, gxg_glIsEnabled, 1, 0, 0, H_glIsEnabled);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEnableClientState" XL_POST, gxg_glEnableClientState, 1, 0, 0, H_glEnableClientState);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDisableClientState" XL_POST, gxg_glDisableClientState, 1, 0, 0, H_glDisableClientState);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetBooleanv" XL_POST, gxg_glGetBooleanv, 1, 1, 0, H_glGetBooleanv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetDoublev" XL_POST, gxg_glGetDoublev, 1, 1, 0, H_glGetDoublev);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetFloatv" XL_POST, gxg_glGetFloatv, 1, 1, 0, H_glGetFloatv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetIntegerv" XL_POST, gxg_glGetIntegerv, 1, 1, 0, H_glGetIntegerv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPushAttrib" XL_POST, gxg_glPushAttrib, 1, 0, 0, H_glPushAttrib);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPopAttrib" XL_POST, gxg_glPopAttrib, 0, 0, 0, H_glPopAttrib);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPushClientAttrib" XL_POST, gxg_glPushClientAttrib, 1, 0, 0, H_glPushClientAttrib);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPopClientAttrib" XL_POST, gxg_glPopClientAttrib, 0, 0, 0, H_glPopClientAttrib);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRenderMode" XL_POST, gxg_glRenderMode, 1, 0, 0, H_glRenderMode);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetError" XL_POST, gxg_glGetError, 0, 0, 0, H_glGetError);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetString" XL_POST, gxg_glGetString, 1, 0, 0, H_glGetString);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFinish" XL_POST, gxg_glFinish, 0, 0, 0, H_glFinish);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFlush" XL_POST, gxg_glFlush, 0, 0, 0, H_glFlush);
  XEN_DEFINE_PROCEDURE(XL_PRE "glHint" XL_POST, gxg_glHint, 2, 0, 0, H_glHint);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClearDepth" XL_POST, gxg_glClearDepth, 1, 0, 0, H_glClearDepth);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDepthFunc" XL_POST, gxg_glDepthFunc, 1, 0, 0, H_glDepthFunc);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDepthMask" XL_POST, gxg_glDepthMask, 1, 0, 0, H_glDepthMask);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDepthRange" XL_POST, gxg_glDepthRange, 2, 0, 0, H_glDepthRange);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClearAccum" XL_POST, gxg_glClearAccum, 4, 0, 0, H_glClearAccum);
  XEN_DEFINE_PROCEDURE(XL_PRE "glAccum" XL_POST, gxg_glAccum, 2, 0, 0, H_glAccum);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMatrixMode" XL_POST, gxg_glMatrixMode, 1, 0, 0, H_glMatrixMode);
  XEN_DEFINE_PROCEDURE(XL_PRE "glOrtho" XL_POST, gxg_glOrtho, 6, 0, 0, H_glOrtho);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFrustum" XL_POST, gxg_glFrustum, 6, 0, 0, H_glFrustum);
  XEN_DEFINE_PROCEDURE(XL_PRE "glViewport" XL_POST, gxg_glViewport, 4, 0, 0, H_glViewport);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPushMatrix" XL_POST, gxg_glPushMatrix, 0, 0, 0, H_glPushMatrix);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPopMatrix" XL_POST, gxg_glPopMatrix, 0, 0, 0, H_glPopMatrix);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLoadIdentity" XL_POST, gxg_glLoadIdentity, 0, 0, 0, H_glLoadIdentity);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLoadMatrixd" XL_POST, gxg_glLoadMatrixd, 1, 0, 0, H_glLoadMatrixd);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLoadMatrixf" XL_POST, gxg_glLoadMatrixf, 1, 0, 0, H_glLoadMatrixf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultMatrixd" XL_POST, gxg_glMultMatrixd, 1, 0, 0, H_glMultMatrixd);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultMatrixf" XL_POST, gxg_glMultMatrixf, 1, 0, 0, H_glMultMatrixf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRotated" XL_POST, gxg_glRotated, 4, 0, 0, H_glRotated);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRotatef" XL_POST, gxg_glRotatef, 4, 0, 0, H_glRotatef);
  XEN_DEFINE_PROCEDURE(XL_PRE "glScaled" XL_POST, gxg_glScaled, 3, 0, 0, H_glScaled);
  XEN_DEFINE_PROCEDURE(XL_PRE "glScalef" XL_POST, gxg_glScalef, 3, 0, 0, H_glScalef);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTranslated" XL_POST, gxg_glTranslated, 3, 0, 0, H_glTranslated);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTranslatef" XL_POST, gxg_glTranslatef, 3, 0, 0, H_glTranslatef);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIsList" XL_POST, gxg_glIsList, 1, 0, 0, H_glIsList);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDeleteLists" XL_POST, gxg_glDeleteLists, 2, 0, 0, H_glDeleteLists);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGenLists" XL_POST, gxg_glGenLists, 1, 0, 0, H_glGenLists);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNewList" XL_POST, gxg_glNewList, 2, 0, 0, H_glNewList);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEndList" XL_POST, gxg_glEndList, 0, 0, 0, H_glEndList);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCallList" XL_POST, gxg_glCallList, 1, 0, 0, H_glCallList);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCallLists" XL_POST, gxg_glCallLists, 3, 0, 0, H_glCallLists);
  XEN_DEFINE_PROCEDURE(XL_PRE "glListBase" XL_POST, gxg_glListBase, 1, 0, 0, H_glListBase);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBegin" XL_POST, gxg_glBegin, 1, 0, 0, H_glBegin);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEnd" XL_POST, gxg_glEnd, 0, 0, 0, H_glEnd);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex2d" XL_POST, gxg_glVertex2d, 2, 0, 0, H_glVertex2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex2f" XL_POST, gxg_glVertex2f, 2, 0, 0, H_glVertex2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex2i" XL_POST, gxg_glVertex2i, 2, 0, 0, H_glVertex2i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex2s" XL_POST, gxg_glVertex2s, 2, 0, 0, H_glVertex2s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex3d" XL_POST, gxg_glVertex3d, 3, 0, 0, H_glVertex3d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex3f" XL_POST, gxg_glVertex3f, 3, 0, 0, H_glVertex3f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex3i" XL_POST, gxg_glVertex3i, 3, 0, 0, H_glVertex3i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex3s" XL_POST, gxg_glVertex3s, 3, 0, 0, H_glVertex3s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex4d" XL_POST, gxg_glVertex4d, 4, 0, 0, H_glVertex4d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex4f" XL_POST, gxg_glVertex4f, 4, 0, 0, H_glVertex4f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex4i" XL_POST, gxg_glVertex4i, 4, 0, 0, H_glVertex4i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertex4s" XL_POST, gxg_glVertex4s, 4, 0, 0, H_glVertex4s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormal3b" XL_POST, gxg_glNormal3b, 3, 0, 0, H_glNormal3b);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormal3d" XL_POST, gxg_glNormal3d, 3, 0, 0, H_glNormal3d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormal3f" XL_POST, gxg_glNormal3f, 3, 0, 0, H_glNormal3f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormal3i" XL_POST, gxg_glNormal3i, 3, 0, 0, H_glNormal3i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormal3s" XL_POST, gxg_glNormal3s, 3, 0, 0, H_glNormal3s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexd" XL_POST, gxg_glIndexd, 1, 0, 0, H_glIndexd);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexf" XL_POST, gxg_glIndexf, 1, 0, 0, H_glIndexf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexi" XL_POST, gxg_glIndexi, 1, 0, 0, H_glIndexi);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexs" XL_POST, gxg_glIndexs, 1, 0, 0, H_glIndexs);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexub" XL_POST, gxg_glIndexub, 1, 0, 0, H_glIndexub);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3b" XL_POST, gxg_glColor3b, 3, 0, 0, H_glColor3b);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3d" XL_POST, gxg_glColor3d, 3, 0, 0, H_glColor3d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3f" XL_POST, gxg_glColor3f, 3, 0, 0, H_glColor3f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3i" XL_POST, gxg_glColor3i, 3, 0, 0, H_glColor3i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3s" XL_POST, gxg_glColor3s, 3, 0, 0, H_glColor3s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3ub" XL_POST, gxg_glColor3ub, 3, 0, 0, H_glColor3ub);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3ui" XL_POST, gxg_glColor3ui, 3, 0, 0, H_glColor3ui);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor3us" XL_POST, gxg_glColor3us, 3, 0, 0, H_glColor3us);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4b" XL_POST, gxg_glColor4b, 4, 0, 0, H_glColor4b);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4d" XL_POST, gxg_glColor4d, 4, 0, 0, H_glColor4d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4f" XL_POST, gxg_glColor4f, 4, 0, 0, H_glColor4f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4i" XL_POST, gxg_glColor4i, 4, 0, 0, H_glColor4i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4s" XL_POST, gxg_glColor4s, 4, 0, 0, H_glColor4s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4ub" XL_POST, gxg_glColor4ub, 4, 0, 0, H_glColor4ub);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4ui" XL_POST, gxg_glColor4ui, 4, 0, 0, H_glColor4ui);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColor4us" XL_POST, gxg_glColor4us, 4, 0, 0, H_glColor4us);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord1d" XL_POST, gxg_glTexCoord1d, 1, 0, 0, H_glTexCoord1d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord1f" XL_POST, gxg_glTexCoord1f, 1, 0, 0, H_glTexCoord1f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord1i" XL_POST, gxg_glTexCoord1i, 1, 0, 0, H_glTexCoord1i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord1s" XL_POST, gxg_glTexCoord1s, 1, 0, 0, H_glTexCoord1s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord2d" XL_POST, gxg_glTexCoord2d, 2, 0, 0, H_glTexCoord2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord2f" XL_POST, gxg_glTexCoord2f, 2, 0, 0, H_glTexCoord2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord2i" XL_POST, gxg_glTexCoord2i, 2, 0, 0, H_glTexCoord2i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord2s" XL_POST, gxg_glTexCoord2s, 2, 0, 0, H_glTexCoord2s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord3d" XL_POST, gxg_glTexCoord3d, 3, 0, 0, H_glTexCoord3d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord3f" XL_POST, gxg_glTexCoord3f, 3, 0, 0, H_glTexCoord3f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord3i" XL_POST, gxg_glTexCoord3i, 3, 0, 0, H_glTexCoord3i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord3s" XL_POST, gxg_glTexCoord3s, 3, 0, 0, H_glTexCoord3s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord4d" XL_POST, gxg_glTexCoord4d, 4, 0, 0, H_glTexCoord4d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord4f" XL_POST, gxg_glTexCoord4f, 4, 0, 0, H_glTexCoord4f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord4i" XL_POST, gxg_glTexCoord4i, 4, 0, 0, H_glTexCoord4i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoord4s" XL_POST, gxg_glTexCoord4s, 4, 0, 0, H_glTexCoord4s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos2d" XL_POST, gxg_glRasterPos2d, 2, 0, 0, H_glRasterPos2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos2f" XL_POST, gxg_glRasterPos2f, 2, 0, 0, H_glRasterPos2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos2i" XL_POST, gxg_glRasterPos2i, 2, 0, 0, H_glRasterPos2i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos2s" XL_POST, gxg_glRasterPos2s, 2, 0, 0, H_glRasterPos2s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos3d" XL_POST, gxg_glRasterPos3d, 3, 0, 0, H_glRasterPos3d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos3f" XL_POST, gxg_glRasterPos3f, 3, 0, 0, H_glRasterPos3f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos3i" XL_POST, gxg_glRasterPos3i, 3, 0, 0, H_glRasterPos3i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos3s" XL_POST, gxg_glRasterPos3s, 3, 0, 0, H_glRasterPos3s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos4d" XL_POST, gxg_glRasterPos4d, 4, 0, 0, H_glRasterPos4d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos4f" XL_POST, gxg_glRasterPos4f, 4, 0, 0, H_glRasterPos4f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos4i" XL_POST, gxg_glRasterPos4i, 4, 0, 0, H_glRasterPos4i);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRasterPos4s" XL_POST, gxg_glRasterPos4s, 4, 0, 0, H_glRasterPos4s);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRectd" XL_POST, gxg_glRectd, 4, 0, 0, H_glRectd);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRectf" XL_POST, gxg_glRectf, 4, 0, 0, H_glRectf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRecti" XL_POST, gxg_glRecti, 4, 0, 0, H_glRecti);
  XEN_DEFINE_PROCEDURE(XL_PRE "glRects" XL_POST, gxg_glRects, 4, 0, 0, H_glRects);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertexPointer" XL_POST, gxg_glVertexPointer, 4, 0, 0, H_glVertexPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormalPointer" XL_POST, gxg_glNormalPointer, 3, 0, 0, H_glNormalPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorPointer" XL_POST, gxg_glColorPointer, 4, 0, 0, H_glColorPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexPointer" XL_POST, gxg_glIndexPointer, 3, 0, 0, H_glIndexPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoordPointer" XL_POST, gxg_glTexCoordPointer, 4, 0, 0, H_glTexCoordPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEdgeFlagPointer" XL_POST, gxg_glEdgeFlagPointer, 2, 0, 0, H_glEdgeFlagPointer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPointerv" XL_POST, gxg_glGetPointerv, 1, 1, 0, H_glGetPointerv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glArrayElement" XL_POST, gxg_glArrayElement, 1, 0, 0, H_glArrayElement);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawArrays" XL_POST, gxg_glDrawArrays, 3, 0, 0, H_glDrawArrays);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawElements" XL_POST, gxg_glDrawElements, 4, 0, 0, H_glDrawElements);
  XEN_DEFINE_PROCEDURE(XL_PRE "glInterleavedArrays" XL_POST, gxg_glInterleavedArrays, 3, 0, 0, H_glInterleavedArrays);
  XEN_DEFINE_PROCEDURE(XL_PRE "glShadeModel" XL_POST, gxg_glShadeModel, 1, 0, 0, H_glShadeModel);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLightf" XL_POST, gxg_glLightf, 3, 0, 0, H_glLightf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLighti" XL_POST, gxg_glLighti, 3, 0, 0, H_glLighti);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetLightfv" XL_POST, gxg_glGetLightfv, 2, 1, 0, H_glGetLightfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetLightiv" XL_POST, gxg_glGetLightiv, 2, 1, 0, H_glGetLightiv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLightModelf" XL_POST, gxg_glLightModelf, 2, 0, 0, H_glLightModelf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLightModeli" XL_POST, gxg_glLightModeli, 2, 0, 0, H_glLightModeli);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMaterialf" XL_POST, gxg_glMaterialf, 3, 0, 0, H_glMaterialf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMateriali" XL_POST, gxg_glMateriali, 3, 0, 0, H_glMateriali);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMaterialfv" XL_POST, gxg_glGetMaterialfv, 2, 1, 0, H_glGetMaterialfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMaterialiv" XL_POST, gxg_glGetMaterialiv, 2, 1, 0, H_glGetMaterialiv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorMaterial" XL_POST, gxg_glColorMaterial, 2, 0, 0, H_glColorMaterial);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPixelZoom" XL_POST, gxg_glPixelZoom, 2, 0, 0, H_glPixelZoom);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPixelStoref" XL_POST, gxg_glPixelStoref, 2, 0, 0, H_glPixelStoref);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPixelStorei" XL_POST, gxg_glPixelStorei, 2, 0, 0, H_glPixelStorei);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPixelTransferf" XL_POST, gxg_glPixelTransferf, 2, 0, 0, H_glPixelTransferf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPixelTransferi" XL_POST, gxg_glPixelTransferi, 2, 0, 0, H_glPixelTransferi);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPixelMapfv" XL_POST, gxg_glGetPixelMapfv, 1, 1, 0, H_glGetPixelMapfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPixelMapuiv" XL_POST, gxg_glGetPixelMapuiv, 1, 1, 0, H_glGetPixelMapuiv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPixelMapusv" XL_POST, gxg_glGetPixelMapusv, 1, 1, 0, H_glGetPixelMapusv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBitmap" XL_POST, gxg_glBitmap, 7, 0, 0, H_glBitmap);
  XEN_DEFINE_PROCEDURE(XL_PRE "glReadPixels" XL_POST, gxg_glReadPixels, 7, 0, 0, H_glReadPixels);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawPixels" XL_POST, gxg_glDrawPixels, 5, 0, 0, H_glDrawPixels);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyPixels" XL_POST, gxg_glCopyPixels, 5, 0, 0, H_glCopyPixels);
  XEN_DEFINE_PROCEDURE(XL_PRE "glStencilFunc" XL_POST, gxg_glStencilFunc, 3, 0, 0, H_glStencilFunc);
  XEN_DEFINE_PROCEDURE(XL_PRE "glStencilMask" XL_POST, gxg_glStencilMask, 1, 0, 0, H_glStencilMask);
  XEN_DEFINE_PROCEDURE(XL_PRE "glStencilOp" XL_POST, gxg_glStencilOp, 3, 0, 0, H_glStencilOp);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClearStencil" XL_POST, gxg_glClearStencil, 1, 0, 0, H_glClearStencil);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexGend" XL_POST, gxg_glTexGend, 3, 0, 0, H_glTexGend);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexGenf" XL_POST, gxg_glTexGenf, 3, 0, 0, H_glTexGenf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexGeni" XL_POST, gxg_glTexGeni, 3, 0, 0, H_glTexGeni);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexGendv" XL_POST, gxg_glGetTexGendv, 2, 1, 0, H_glGetTexGendv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexGenfv" XL_POST, gxg_glGetTexGenfv, 2, 1, 0, H_glGetTexGenfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexGeniv" XL_POST, gxg_glGetTexGeniv, 2, 1, 0, H_glGetTexGeniv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexEnvf" XL_POST, gxg_glTexEnvf, 3, 0, 0, H_glTexEnvf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexEnvi" XL_POST, gxg_glTexEnvi, 3, 0, 0, H_glTexEnvi);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexEnvfv" XL_POST, gxg_glGetTexEnvfv, 2, 1, 0, H_glGetTexEnvfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexEnviv" XL_POST, gxg_glGetTexEnviv, 2, 1, 0, H_glGetTexEnviv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexParameterf" XL_POST, gxg_glTexParameterf, 3, 0, 0, H_glTexParameterf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexParameteri" XL_POST, gxg_glTexParameteri, 3, 0, 0, H_glTexParameteri);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexParameterfv" XL_POST, gxg_glGetTexParameterfv, 2, 1, 0, H_glGetTexParameterfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexParameteriv" XL_POST, gxg_glGetTexParameteriv, 2, 1, 0, H_glGetTexParameteriv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexLevelParameterfv" XL_POST, gxg_glGetTexLevelParameterfv, 3, 1, 0, H_glGetTexLevelParameterfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetTexLevelParameteriv" XL_POST, gxg_glGetTexLevelParameteriv, 3, 1, 0, H_glGetTexLevelParameteriv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexImage1D" XL_POST, gxg_glTexImage1D, 8, 0, 0, H_glTexImage1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexImage2D" XL_POST, gxg_glTexImage2D, 9, 0, 0, H_glTexImage2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGenTextures" XL_POST, gxg_glGenTextures, 2, 0, 0, H_glGenTextures);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDeleteTextures" XL_POST, gxg_glDeleteTextures, 2, 0, 0, H_glDeleteTextures);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBindTexture" XL_POST, gxg_glBindTexture, 2, 0, 0, H_glBindTexture);
  XEN_DEFINE_PROCEDURE(XL_PRE "glAreTexturesResident" XL_POST, gxg_glAreTexturesResident, 3, 0, 0, H_glAreTexturesResident);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIsTexture" XL_POST, gxg_glIsTexture, 1, 0, 0, H_glIsTexture);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexSubImage1D" XL_POST, gxg_glTexSubImage1D, 7, 0, 0, H_glTexSubImage1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexSubImage2D" XL_POST, gxg_glTexSubImage2D, 9, 0, 0, H_glTexSubImage2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexImage1D" XL_POST, gxg_glCopyTexImage1D, 7, 0, 0, H_glCopyTexImage1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexImage2D" XL_POST, gxg_glCopyTexImage2D, 8, 0, 0, H_glCopyTexImage2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexSubImage1D" XL_POST, gxg_glCopyTexSubImage1D, 6, 0, 0, H_glCopyTexSubImage1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexSubImage2D" XL_POST, gxg_glCopyTexSubImage2D, 8, 0, 0, H_glCopyTexSubImage2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMap1d" XL_POST, gxg_glMap1d, 6, 0, 0, H_glMap1d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMap1f" XL_POST, gxg_glMap1f, 6, 0, 0, H_glMap1f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMap2d" XL_POST, gxg_glMap2d, 0, 0, 1, H_glMap2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMap2f" XL_POST, gxg_glMap2f, 0, 0, 1, H_glMap2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMapdv" XL_POST, gxg_glGetMapdv, 2, 1, 0, H_glGetMapdv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMapfv" XL_POST, gxg_glGetMapfv, 2, 1, 0, H_glGetMapfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMapiv" XL_POST, gxg_glGetMapiv, 2, 1, 0, H_glGetMapiv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalCoord1d" XL_POST, gxg_glEvalCoord1d, 1, 0, 0, H_glEvalCoord1d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalCoord1f" XL_POST, gxg_glEvalCoord1f, 1, 0, 0, H_glEvalCoord1f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalCoord2d" XL_POST, gxg_glEvalCoord2d, 2, 0, 0, H_glEvalCoord2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalCoord2f" XL_POST, gxg_glEvalCoord2f, 2, 0, 0, H_glEvalCoord2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMapGrid1d" XL_POST, gxg_glMapGrid1d, 3, 0, 0, H_glMapGrid1d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMapGrid1f" XL_POST, gxg_glMapGrid1f, 3, 0, 0, H_glMapGrid1f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMapGrid2d" XL_POST, gxg_glMapGrid2d, 6, 0, 0, H_glMapGrid2d);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMapGrid2f" XL_POST, gxg_glMapGrid2f, 6, 0, 0, H_glMapGrid2f);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalPoint1" XL_POST, gxg_glEvalPoint1, 1, 0, 0, H_glEvalPoint1);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalPoint2" XL_POST, gxg_glEvalPoint2, 2, 0, 0, H_glEvalPoint2);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalMesh1" XL_POST, gxg_glEvalMesh1, 3, 0, 0, H_glEvalMesh1);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEvalMesh2" XL_POST, gxg_glEvalMesh2, 5, 0, 0, H_glEvalMesh2);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFogf" XL_POST, gxg_glFogf, 2, 0, 0, H_glFogf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFogi" XL_POST, gxg_glFogi, 2, 0, 0, H_glFogi);
  XEN_DEFINE_PROCEDURE(XL_PRE "glFeedbackBuffer" XL_POST, gxg_glFeedbackBuffer, 3, 0, 0, H_glFeedbackBuffer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPassThrough" XL_POST, gxg_glPassThrough, 1, 0, 0, H_glPassThrough);
  XEN_DEFINE_PROCEDURE(XL_PRE "glSelectBuffer" XL_POST, gxg_glSelectBuffer, 2, 0, 0, H_glSelectBuffer);
  XEN_DEFINE_PROCEDURE(XL_PRE "glInitNames" XL_POST, gxg_glInitNames, 0, 0, 0, H_glInitNames);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLoadName" XL_POST, gxg_glLoadName, 1, 0, 0, H_glLoadName);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPushName" XL_POST, gxg_glPushName, 1, 0, 0, H_glPushName);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPopName" XL_POST, gxg_glPopName, 0, 0, 0, H_glPopName);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawRangeElements" XL_POST, gxg_glDrawRangeElements, 6, 0, 0, H_glDrawRangeElements);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexImage3D" XL_POST, gxg_glTexImage3D, 0, 0, 1, H_glTexImage3D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexSubImage3D" XL_POST, gxg_glTexSubImage3D, 0, 0, 1, H_glTexSubImage3D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexSubImage3D" XL_POST, gxg_glCopyTexSubImage3D, 9, 0, 0, H_glCopyTexSubImage3D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorTable" XL_POST, gxg_glColorTable, 6, 0, 0, H_glColorTable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorSubTable" XL_POST, gxg_glColorSubTable, 6, 0, 0, H_glColorSubTable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyColorSubTable" XL_POST, gxg_glCopyColorSubTable, 5, 0, 0, H_glCopyColorSubTable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyColorTable" XL_POST, gxg_glCopyColorTable, 5, 0, 0, H_glCopyColorTable);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetColorTableParameterfv" XL_POST, gxg_glGetColorTableParameterfv, 2, 1, 0, H_glGetColorTableParameterfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetColorTableParameteriv" XL_POST, gxg_glGetColorTableParameteriv, 2, 1, 0, H_glGetColorTableParameteriv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBlendEquation" XL_POST, gxg_glBlendEquation, 1, 0, 0, H_glBlendEquation);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBlendColor" XL_POST, gxg_glBlendColor, 4, 0, 0, H_glBlendColor);
  XEN_DEFINE_PROCEDURE(XL_PRE "glHistogram" XL_POST, gxg_glHistogram, 4, 0, 0, H_glHistogram);
  XEN_DEFINE_PROCEDURE(XL_PRE "glResetHistogram" XL_POST, gxg_glResetHistogram, 1, 0, 0, H_glResetHistogram);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetHistogram" XL_POST, gxg_glGetHistogram, 5, 0, 0, H_glGetHistogram);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetHistogramParameterfv" XL_POST, gxg_glGetHistogramParameterfv, 2, 1, 0, H_glGetHistogramParameterfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetHistogramParameteriv" XL_POST, gxg_glGetHistogramParameteriv, 2, 1, 0, H_glGetHistogramParameteriv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMinmax" XL_POST, gxg_glMinmax, 3, 0, 0, H_glMinmax);
  XEN_DEFINE_PROCEDURE(XL_PRE "glResetMinmax" XL_POST, gxg_glResetMinmax, 1, 0, 0, H_glResetMinmax);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMinmax" XL_POST, gxg_glGetMinmax, 5, 0, 0, H_glGetMinmax);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMinmaxParameterfv" XL_POST, gxg_glGetMinmaxParameterfv, 2, 1, 0, H_glGetMinmaxParameterfv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetMinmaxParameteriv" XL_POST, gxg_glGetMinmaxParameteriv, 2, 1, 0, H_glGetMinmaxParameteriv);
  XEN_DEFINE_PROCEDURE(XL_PRE "glConvolutionFilter1D" XL_POST, gxg_glConvolutionFilter1D, 6, 0, 0, H_glConvolutionFilter1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glConvolutionFilter2D" XL_POST, gxg_glConvolutionFilter2D, 7, 0, 0, H_glConvolutionFilter2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glConvolutionParameterf" XL_POST, gxg_glConvolutionParameterf, 3, 0, 0, H_glConvolutionParameterf);
  XEN_DEFINE_PROCEDURE(XL_PRE "glConvolutionParameteri" XL_POST, gxg_glConvolutionParameteri, 3, 0, 0, H_glConvolutionParameteri);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyConvolutionFilter1D" XL_POST, gxg_glCopyConvolutionFilter1D, 5, 0, 0, H_glCopyConvolutionFilter1D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyConvolutionFilter2D" XL_POST, gxg_glCopyConvolutionFilter2D, 6, 0, 0, H_glCopyConvolutionFilter2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "glSeparableFilter2D" XL_POST, gxg_glSeparableFilter2D, 8, 0, 0, H_glSeparableFilter2D);
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluBeginPolygon" XL_POST, gxg_gluBeginPolygon, 1, 0, 0, H_gluBeginPolygon);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "gluBuild1DMipmaps" XL_POST, gxg_gluBuild1DMipmaps, 6, 0, 0, H_gluBuild1DMipmaps);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluBuild2DMipmaps" XL_POST, gxg_gluBuild2DMipmaps, 7, 0, 0, H_gluBuild2DMipmaps);
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluDeleteTess" XL_POST, gxg_gluDeleteTess, 1, 0, 0, H_gluDeleteTess);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluEndPolygon" XL_POST, gxg_gluEndPolygon, 1, 0, 0, H_gluEndPolygon);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "gluErrorString" XL_POST, gxg_gluErrorString, 1, 0, 0, H_gluErrorString);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluGetString" XL_POST, gxg_gluGetString, 1, 0, 0, H_gluGetString);
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluGetTessProperty" XL_POST, gxg_gluGetTessProperty, 3, 0, 0, H_gluGetTessProperty);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "gluLookAt" XL_POST, gxg_gluLookAt, 9, 0, 0, H_gluLookAt);
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluNewTess" XL_POST, gxg_gluNewTess, 0, 0, 0, H_gluNewTess);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluNextContour" XL_POST, gxg_gluNextContour, 2, 0, 0, H_gluNextContour);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "gluOrtho2D" XL_POST, gxg_gluOrtho2D, 4, 0, 0, H_gluOrtho2D);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluPerspective" XL_POST, gxg_gluPerspective, 4, 0, 0, H_gluPerspective);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluPickMatrix" XL_POST, gxg_gluPickMatrix, 5, 0, 0, H_gluPickMatrix);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluProject" XL_POST, gxg_gluProject, 9, 0, 0, H_gluProject);
  XEN_DEFINE_PROCEDURE(XL_PRE "gluScaleImage" XL_POST, gxg_gluScaleImage, 9, 0, 0, H_gluScaleImage);
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessBeginContour" XL_POST, gxg_gluTessBeginContour, 1, 0, 0, H_gluTessBeginContour);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessBeginPolygon" XL_POST, gxg_gluTessBeginPolygon, 2, 0, 0, H_gluTessBeginPolygon);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessEndContour" XL_POST, gxg_gluTessEndContour, 1, 0, 0, H_gluTessEndContour);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessEndPolygon" XL_POST, gxg_gluTessEndPolygon, 1, 0, 0, H_gluTessEndPolygon);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessNormal" XL_POST, gxg_gluTessNormal, 4, 0, 0, H_gluTessNormal);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessProperty" XL_POST, gxg_gluTessProperty, 3, 0, 0, H_gluTessProperty);
#endif
#ifdef GLU_VERSION_1_2
  XEN_DEFINE_PROCEDURE(XL_PRE "gluTessVertex" XL_POST, gxg_gluTessVertex, 3, 0, 0, H_gluTessVertex);
#endif
  XEN_DEFINE_PROCEDURE(XL_PRE "gluUnProject" XL_POST, gxg_gluUnProject, 9, 0, 0, H_gluUnProject);
  XEN_DEFINE_PROCEDURE(XL_PRE "glActiveTextureARB" XL_POST, gxg_glActiveTextureARB, 1, 0, 0, H_glActiveTextureARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glClientActiveTextureARB" XL_POST, gxg_glClientActiveTextureARB, 1, 0, 0, H_glClientActiveTextureARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1dARB" XL_POST, gxg_glMultiTexCoord1dARB, 2, 0, 0, H_glMultiTexCoord1dARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1dvARB" XL_POST, gxg_glMultiTexCoord1dvARB, 2, 0, 0, H_glMultiTexCoord1dvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1fARB" XL_POST, gxg_glMultiTexCoord1fARB, 2, 0, 0, H_glMultiTexCoord1fARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1fvARB" XL_POST, gxg_glMultiTexCoord1fvARB, 2, 0, 0, H_glMultiTexCoord1fvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1iARB" XL_POST, gxg_glMultiTexCoord1iARB, 2, 0, 0, H_glMultiTexCoord1iARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1ivARB" XL_POST, gxg_glMultiTexCoord1ivARB, 2, 0, 0, H_glMultiTexCoord1ivARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1sARB" XL_POST, gxg_glMultiTexCoord1sARB, 2, 0, 0, H_glMultiTexCoord1sARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord1svARB" XL_POST, gxg_glMultiTexCoord1svARB, 2, 0, 0, H_glMultiTexCoord1svARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2dARB" XL_POST, gxg_glMultiTexCoord2dARB, 3, 0, 0, H_glMultiTexCoord2dARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2dvARB" XL_POST, gxg_glMultiTexCoord2dvARB, 2, 0, 0, H_glMultiTexCoord2dvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2fARB" XL_POST, gxg_glMultiTexCoord2fARB, 3, 0, 0, H_glMultiTexCoord2fARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2fvARB" XL_POST, gxg_glMultiTexCoord2fvARB, 2, 0, 0, H_glMultiTexCoord2fvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2iARB" XL_POST, gxg_glMultiTexCoord2iARB, 3, 0, 0, H_glMultiTexCoord2iARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2ivARB" XL_POST, gxg_glMultiTexCoord2ivARB, 2, 0, 0, H_glMultiTexCoord2ivARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2sARB" XL_POST, gxg_glMultiTexCoord2sARB, 3, 0, 0, H_glMultiTexCoord2sARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord2svARB" XL_POST, gxg_glMultiTexCoord2svARB, 2, 0, 0, H_glMultiTexCoord2svARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3dARB" XL_POST, gxg_glMultiTexCoord3dARB, 4, 0, 0, H_glMultiTexCoord3dARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3dvARB" XL_POST, gxg_glMultiTexCoord3dvARB, 2, 0, 0, H_glMultiTexCoord3dvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3fARB" XL_POST, gxg_glMultiTexCoord3fARB, 4, 0, 0, H_glMultiTexCoord3fARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3fvARB" XL_POST, gxg_glMultiTexCoord3fvARB, 2, 0, 0, H_glMultiTexCoord3fvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3iARB" XL_POST, gxg_glMultiTexCoord3iARB, 4, 0, 0, H_glMultiTexCoord3iARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3ivARB" XL_POST, gxg_glMultiTexCoord3ivARB, 2, 0, 0, H_glMultiTexCoord3ivARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3sARB" XL_POST, gxg_glMultiTexCoord3sARB, 4, 0, 0, H_glMultiTexCoord3sARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord3svARB" XL_POST, gxg_glMultiTexCoord3svARB, 2, 0, 0, H_glMultiTexCoord3svARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4dARB" XL_POST, gxg_glMultiTexCoord4dARB, 5, 0, 0, H_glMultiTexCoord4dARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4dvARB" XL_POST, gxg_glMultiTexCoord4dvARB, 2, 0, 0, H_glMultiTexCoord4dvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4fARB" XL_POST, gxg_glMultiTexCoord4fARB, 5, 0, 0, H_glMultiTexCoord4fARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4fvARB" XL_POST, gxg_glMultiTexCoord4fvARB, 2, 0, 0, H_glMultiTexCoord4fvARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4iARB" XL_POST, gxg_glMultiTexCoord4iARB, 5, 0, 0, H_glMultiTexCoord4iARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4ivARB" XL_POST, gxg_glMultiTexCoord4ivARB, 2, 0, 0, H_glMultiTexCoord4ivARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4sARB" XL_POST, gxg_glMultiTexCoord4sARB, 5, 0, 0, H_glMultiTexCoord4sARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glMultiTexCoord4svARB" XL_POST, gxg_glMultiTexCoord4svARB, 2, 0, 0, H_glMultiTexCoord4svARB);
  XEN_DEFINE_PROCEDURE(XL_PRE "glLockArraysEXT" XL_POST, gxg_glLockArraysEXT, 2, 0, 0, H_glLockArraysEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glUnlockArraysEXT" XL_POST, gxg_glUnlockArraysEXT, 0, 0, 0, H_glUnlockArraysEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBlendColorEXT" XL_POST, gxg_glBlendColorEXT, 4, 0, 0, H_glBlendColorEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPolygonOffsetEXT" XL_POST, gxg_glPolygonOffsetEXT, 2, 0, 0, H_glPolygonOffsetEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexImage3DEXT" XL_POST, gxg_glTexImage3DEXT, 0, 0, 1, H_glTexImage3DEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexSubImage3DEXT" XL_POST, gxg_glTexSubImage3DEXT, 0, 0, 1, H_glTexSubImage3DEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glCopyTexSubImage3DEXT" XL_POST, gxg_glCopyTexSubImage3DEXT, 9, 0, 0, H_glCopyTexSubImage3DEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGenTexturesEXT" XL_POST, gxg_glGenTexturesEXT, 2, 0, 0, H_glGenTexturesEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDeleteTexturesEXT" XL_POST, gxg_glDeleteTexturesEXT, 2, 0, 0, H_glDeleteTexturesEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBindTextureEXT" XL_POST, gxg_glBindTextureEXT, 2, 0, 0, H_glBindTextureEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glAreTexturesResidentEXT" XL_POST, gxg_glAreTexturesResidentEXT, 3, 0, 0, H_glAreTexturesResidentEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIsTextureEXT" XL_POST, gxg_glIsTextureEXT, 1, 0, 0, H_glIsTextureEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glVertexPointerEXT" XL_POST, gxg_glVertexPointerEXT, 5, 0, 0, H_glVertexPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glNormalPointerEXT" XL_POST, gxg_glNormalPointerEXT, 4, 0, 0, H_glNormalPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorPointerEXT" XL_POST, gxg_glColorPointerEXT, 5, 0, 0, H_glColorPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glIndexPointerEXT" XL_POST, gxg_glIndexPointerEXT, 4, 0, 0, H_glIndexPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glTexCoordPointerEXT" XL_POST, gxg_glTexCoordPointerEXT, 5, 0, 0, H_glTexCoordPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glEdgeFlagPointerEXT" XL_POST, gxg_glEdgeFlagPointerEXT, 3, 0, 0, H_glEdgeFlagPointerEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetPointervEXT" XL_POST, gxg_glGetPointervEXT, 1, 1, 0, H_glGetPointervEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glArrayElementEXT" XL_POST, gxg_glArrayElementEXT, 1, 0, 0, H_glArrayElementEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glDrawArraysEXT" XL_POST, gxg_glDrawArraysEXT, 3, 0, 0, H_glDrawArraysEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glBlendEquationEXT" XL_POST, gxg_glBlendEquationEXT, 1, 0, 0, H_glBlendEquationEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPointParameterfEXT" XL_POST, gxg_glPointParameterfEXT, 2, 0, 0, H_glPointParameterfEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glPointParameterfvEXT" XL_POST, gxg_glPointParameterfvEXT, 2, 0, 0, H_glPointParameterfvEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorTableEXT" XL_POST, gxg_glColorTableEXT, 6, 0, 0, H_glColorTableEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glColorSubTableEXT" XL_POST, gxg_glColorSubTableEXT, 6, 0, 0, H_glColorSubTableEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetColorTableParameterfvEXT" XL_POST, gxg_glGetColorTableParameterfvEXT, 2, 1, 0, H_glGetColorTableParameterfvEXT);
  XEN_DEFINE_PROCEDURE(XL_PRE "glGetColorTableParameterivEXT" XL_POST, gxg_glGetColorTableParameterivEXT, 2, 1, 0, H_glGetColorTableParameterivEXT);
}

#else
  #include "gl-ruby.c"
#endif
/* ---------------------------------------- constants ---------------------------------------- */

static void define_integers(void)
{

#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  #define DEFINE_INTEGER(Name, Value) scm_c_define(Name, C_TO_XEN_INT(Value))
#else
  #define DEFINE_INTEGER(Name, Value) gh_define(Name, C_TO_XEN_INT(Value))
#endif
#else
  #define DEFINE_INTEGER(Name, Value) rb_define_global_const(Name, C_TO_XEN_INT(Value))
#endif

#if USE_MOTIF
  DEFINE_INTEGER(XL_PRE "GLX_USE_GL" XL_POST,                                   GLX_USE_GL);
  DEFINE_INTEGER(XL_PRE "GLX_BUFFER_SIZE" XL_POST,                              GLX_BUFFER_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_LEVEL" XL_POST,                                    GLX_LEVEL);
  DEFINE_INTEGER(XL_PRE "GLX_RGBA" XL_POST,                                     GLX_RGBA);
  DEFINE_INTEGER(XL_PRE "GLX_DOUBLEBUFFER" XL_POST,                             GLX_DOUBLEBUFFER);
  DEFINE_INTEGER(XL_PRE "GLX_STEREO" XL_POST,                                   GLX_STEREO);
  DEFINE_INTEGER(XL_PRE "GLX_AUX_BUFFERS" XL_POST,                              GLX_AUX_BUFFERS);
  DEFINE_INTEGER(XL_PRE "GLX_RED_SIZE" XL_POST,                                 GLX_RED_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_GREEN_SIZE" XL_POST,                               GLX_GREEN_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_BLUE_SIZE" XL_POST,                                GLX_BLUE_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_ALPHA_SIZE" XL_POST,                               GLX_ALPHA_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_DEPTH_SIZE" XL_POST,                               GLX_DEPTH_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_STENCIL_SIZE" XL_POST,                             GLX_STENCIL_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_ACCUM_RED_SIZE" XL_POST,                           GLX_ACCUM_RED_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_ACCUM_GREEN_SIZE" XL_POST,                         GLX_ACCUM_GREEN_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_ACCUM_BLUE_SIZE" XL_POST,                          GLX_ACCUM_BLUE_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_ACCUM_ALPHA_SIZE" XL_POST,                         GLX_ACCUM_ALPHA_SIZE);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_SCREEN" XL_POST,                               GLX_BAD_SCREEN);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_ATTRIBUTE" XL_POST,                            GLX_BAD_ATTRIBUTE);
  DEFINE_INTEGER(XL_PRE "GLX_NO_EXTENSION" XL_POST,                             GLX_NO_EXTENSION);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_VISUAL" XL_POST,                               GLX_BAD_VISUAL);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_CONTEXT" XL_POST,                              GLX_BAD_CONTEXT);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_VALUE" XL_POST,                                GLX_BAD_VALUE);
  DEFINE_INTEGER(XL_PRE "GLX_BAD_ENUM" XL_POST,                                 GLX_BAD_ENUM);
  DEFINE_INTEGER(XL_PRE "GLX_X_VISUAL_TYPE_EXT" XL_POST,                        GLX_X_VISUAL_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_TYPE_EXT" XL_POST,                     GLX_TRANSPARENT_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_INDEX_VALUE_EXT" XL_POST,              GLX_TRANSPARENT_INDEX_VALUE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_RED_VALUE_EXT" XL_POST,                GLX_TRANSPARENT_RED_VALUE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_GREEN_VALUE_EXT" XL_POST,              GLX_TRANSPARENT_GREEN_VALUE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_BLUE_VALUE_EXT" XL_POST,               GLX_TRANSPARENT_BLUE_VALUE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_ALPHA_VALUE_EXT" XL_POST,              GLX_TRANSPARENT_ALPHA_VALUE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRUE_COLOR_EXT" XL_POST,                           GLX_TRUE_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_DIRECT_COLOR_EXT" XL_POST,                         GLX_DIRECT_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_PSEUDO_COLOR_EXT" XL_POST,                         GLX_PSEUDO_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_STATIC_COLOR_EXT" XL_POST,                         GLX_STATIC_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_GRAY_SCALE_EXT" XL_POST,                           GLX_GRAY_SCALE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_STATIC_GRAY_EXT" XL_POST,                          GLX_STATIC_GRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_NONE_EXT" XL_POST,                                 GLX_NONE_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_RGB_EXT" XL_POST,                      GLX_TRANSPARENT_RGB_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_TRANSPARENT_INDEX_EXT" XL_POST,                    GLX_TRANSPARENT_INDEX_EXT);
  DEFINE_INTEGER(XL_PRE "GLX_VENDOR" XL_POST,                                   GLX_VENDOR);
  DEFINE_INTEGER(XL_PRE "GLX_VERSION" XL_POST,                                  GLX_VERSION);
  DEFINE_INTEGER(XL_PRE "GLX_EXTENSIONS" XL_POST,                               GLX_EXTENSIONS);
#endif
  DEFINE_INTEGER(XL_PRE "GL_FALSE" XL_POST,                                     GL_FALSE);
  DEFINE_INTEGER(XL_PRE "GL_TRUE" XL_POST,                                      GL_TRUE);
  DEFINE_INTEGER(XL_PRE "GL_BYTE" XL_POST,                                      GL_BYTE);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_BYTE" XL_POST,                             GL_UNSIGNED_BYTE);
  DEFINE_INTEGER(XL_PRE "GL_SHORT" XL_POST,                                     GL_SHORT);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT" XL_POST,                            GL_UNSIGNED_SHORT);
  DEFINE_INTEGER(XL_PRE "GL_INT" XL_POST,                                       GL_INT);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_INT" XL_POST,                              GL_UNSIGNED_INT);
  DEFINE_INTEGER(XL_PRE "GL_FLOAT" XL_POST,                                     GL_FLOAT);
  DEFINE_INTEGER(XL_PRE "GL_DOUBLE" XL_POST,                                    GL_DOUBLE);
  DEFINE_INTEGER(XL_PRE "GL_2_BYTES" XL_POST,                                   GL_2_BYTES);
  DEFINE_INTEGER(XL_PRE "GL_3_BYTES" XL_POST,                                   GL_3_BYTES);
  DEFINE_INTEGER(XL_PRE "GL_4_BYTES" XL_POST,                                   GL_4_BYTES);
  DEFINE_INTEGER(XL_PRE "GL_POINTS" XL_POST,                                    GL_POINTS);
  DEFINE_INTEGER(XL_PRE "GL_LINES" XL_POST,                                     GL_LINES);
  DEFINE_INTEGER(XL_PRE "GL_LINE_LOOP" XL_POST,                                 GL_LINE_LOOP);
  DEFINE_INTEGER(XL_PRE "GL_LINE_STRIP" XL_POST,                                GL_LINE_STRIP);
  DEFINE_INTEGER(XL_PRE "GL_TRIANGLES" XL_POST,                                 GL_TRIANGLES);
  DEFINE_INTEGER(XL_PRE "GL_TRIANGLE_STRIP" XL_POST,                            GL_TRIANGLE_STRIP);
  DEFINE_INTEGER(XL_PRE "GL_TRIANGLE_FAN" XL_POST,                              GL_TRIANGLE_FAN);
  DEFINE_INTEGER(XL_PRE "GL_QUADS" XL_POST,                                     GL_QUADS);
  DEFINE_INTEGER(XL_PRE "GL_QUAD_STRIP" XL_POST,                                GL_QUAD_STRIP);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON" XL_POST,                                   GL_POLYGON);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY" XL_POST,                              GL_VERTEX_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY" XL_POST,                              GL_NORMAL_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY" XL_POST,                               GL_COLOR_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY" XL_POST,                               GL_INDEX_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY" XL_POST,                       GL_TEXTURE_COORD_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY" XL_POST,                           GL_EDGE_FLAG_ARRAY);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_SIZE" XL_POST,                         GL_VERTEX_ARRAY_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_TYPE" XL_POST,                         GL_VERTEX_ARRAY_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_STRIDE" XL_POST,                       GL_VERTEX_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_TYPE" XL_POST,                         GL_NORMAL_ARRAY_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_STRIDE" XL_POST,                       GL_NORMAL_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_SIZE" XL_POST,                          GL_COLOR_ARRAY_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_TYPE" XL_POST,                          GL_COLOR_ARRAY_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_STRIDE" XL_POST,                        GL_COLOR_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_TYPE" XL_POST,                          GL_INDEX_ARRAY_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_STRIDE" XL_POST,                        GL_INDEX_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_SIZE" XL_POST,                  GL_TEXTURE_COORD_ARRAY_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_TYPE" XL_POST,                  GL_TEXTURE_COORD_ARRAY_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_STRIDE" XL_POST,                GL_TEXTURE_COORD_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_STRIDE" XL_POST,                    GL_EDGE_FLAG_ARRAY_STRIDE);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_POINTER" XL_POST,                      GL_VERTEX_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_POINTER" XL_POST,                      GL_NORMAL_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_POINTER" XL_POST,                       GL_COLOR_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_POINTER" XL_POST,                       GL_INDEX_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_POINTER" XL_POST,               GL_TEXTURE_COORD_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_POINTER" XL_POST,                   GL_EDGE_FLAG_ARRAY_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_V2F" XL_POST,                                       GL_V2F);
  DEFINE_INTEGER(XL_PRE "GL_V3F" XL_POST,                                       GL_V3F);
  DEFINE_INTEGER(XL_PRE "GL_C4UB_V2F" XL_POST,                                  GL_C4UB_V2F);
  DEFINE_INTEGER(XL_PRE "GL_C4UB_V3F" XL_POST,                                  GL_C4UB_V3F);
  DEFINE_INTEGER(XL_PRE "GL_C3F_V3F" XL_POST,                                   GL_C3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_N3F_V3F" XL_POST,                                   GL_N3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_C4F_N3F_V3F" XL_POST,                               GL_C4F_N3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T2F_V3F" XL_POST,                                   GL_T2F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T4F_V4F" XL_POST,                                   GL_T4F_V4F);
  DEFINE_INTEGER(XL_PRE "GL_T2F_C4UB_V3F" XL_POST,                              GL_T2F_C4UB_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T2F_C3F_V3F" XL_POST,                               GL_T2F_C3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T2F_N3F_V3F" XL_POST,                               GL_T2F_N3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T2F_C4F_N3F_V3F" XL_POST,                           GL_T2F_C4F_N3F_V3F);
  DEFINE_INTEGER(XL_PRE "GL_T4F_C4F_N3F_V4F" XL_POST,                           GL_T4F_C4F_N3F_V4F);
  DEFINE_INTEGER(XL_PRE "GL_MATRIX_MODE" XL_POST,                               GL_MATRIX_MODE);
  DEFINE_INTEGER(XL_PRE "GL_MODELVIEW" XL_POST,                                 GL_MODELVIEW);
  DEFINE_INTEGER(XL_PRE "GL_PROJECTION" XL_POST,                                GL_PROJECTION);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE" XL_POST,                                   GL_TEXTURE);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SMOOTH" XL_POST,                              GL_POINT_SMOOTH);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SIZE" XL_POST,                                GL_POINT_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SIZE_GRANULARITY" XL_POST,                    GL_POINT_SIZE_GRANULARITY);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SIZE_RANGE" XL_POST,                          GL_POINT_SIZE_RANGE);
  DEFINE_INTEGER(XL_PRE "GL_LINE_SMOOTH" XL_POST,                               GL_LINE_SMOOTH);
  DEFINE_INTEGER(XL_PRE "GL_LINE_STIPPLE" XL_POST,                              GL_LINE_STIPPLE);
  DEFINE_INTEGER(XL_PRE "GL_LINE_STIPPLE_PATTERN" XL_POST,                      GL_LINE_STIPPLE_PATTERN);
  DEFINE_INTEGER(XL_PRE "GL_LINE_STIPPLE_REPEAT" XL_POST,                       GL_LINE_STIPPLE_REPEAT);
  DEFINE_INTEGER(XL_PRE "GL_LINE_WIDTH" XL_POST,                                GL_LINE_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_LINE_WIDTH_GRANULARITY" XL_POST,                    GL_LINE_WIDTH_GRANULARITY);
  DEFINE_INTEGER(XL_PRE "GL_LINE_WIDTH_RANGE" XL_POST,                          GL_LINE_WIDTH_RANGE);
  DEFINE_INTEGER(XL_PRE "GL_POINT" XL_POST,                                     GL_POINT);
  DEFINE_INTEGER(XL_PRE "GL_LINE" XL_POST,                                      GL_LINE);
  DEFINE_INTEGER(XL_PRE "GL_FILL" XL_POST,                                      GL_FILL);
  DEFINE_INTEGER(XL_PRE "GL_CW" XL_POST,                                        GL_CW);
  DEFINE_INTEGER(XL_PRE "GL_CCW" XL_POST,                                       GL_CCW);
  DEFINE_INTEGER(XL_PRE "GL_FRONT" XL_POST,                                     GL_FRONT);
  DEFINE_INTEGER(XL_PRE "GL_BACK" XL_POST,                                      GL_BACK);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_MODE" XL_POST,                              GL_POLYGON_MODE);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_SMOOTH" XL_POST,                            GL_POLYGON_SMOOTH);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_STIPPLE" XL_POST,                           GL_POLYGON_STIPPLE);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG" XL_POST,                                 GL_EDGE_FLAG);
  DEFINE_INTEGER(XL_PRE "GL_CULL_FACE" XL_POST,                                 GL_CULL_FACE);
  DEFINE_INTEGER(XL_PRE "GL_CULL_FACE_MODE" XL_POST,                            GL_CULL_FACE_MODE);
  DEFINE_INTEGER(XL_PRE "GL_FRONT_FACE" XL_POST,                                GL_FRONT_FACE);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_FACTOR" XL_POST,                     GL_POLYGON_OFFSET_FACTOR);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_UNITS" XL_POST,                      GL_POLYGON_OFFSET_UNITS);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_POINT" XL_POST,                      GL_POLYGON_OFFSET_POINT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_LINE" XL_POST,                       GL_POLYGON_OFFSET_LINE);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_FILL" XL_POST,                       GL_POLYGON_OFFSET_FILL);
  DEFINE_INTEGER(XL_PRE "GL_COMPILE" XL_POST,                                   GL_COMPILE);
  DEFINE_INTEGER(XL_PRE "GL_COMPILE_AND_EXECUTE" XL_POST,                       GL_COMPILE_AND_EXECUTE);
  DEFINE_INTEGER(XL_PRE "GL_LIST_BASE" XL_POST,                                 GL_LIST_BASE);
  DEFINE_INTEGER(XL_PRE "GL_LIST_INDEX" XL_POST,                                GL_LIST_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_LIST_MODE" XL_POST,                                 GL_LIST_MODE);
  DEFINE_INTEGER(XL_PRE "GL_NEVER" XL_POST,                                     GL_NEVER);
  DEFINE_INTEGER(XL_PRE "GL_LESS" XL_POST,                                      GL_LESS);
  DEFINE_INTEGER(XL_PRE "GL_EQUAL" XL_POST,                                     GL_EQUAL);
  DEFINE_INTEGER(XL_PRE "GL_LEQUAL" XL_POST,                                    GL_LEQUAL);
  DEFINE_INTEGER(XL_PRE "GL_GREATER" XL_POST,                                   GL_GREATER);
  DEFINE_INTEGER(XL_PRE "GL_NOTEQUAL" XL_POST,                                  GL_NOTEQUAL);
  DEFINE_INTEGER(XL_PRE "GL_GEQUAL" XL_POST,                                    GL_GEQUAL);
  DEFINE_INTEGER(XL_PRE "GL_ALWAYS" XL_POST,                                    GL_ALWAYS);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_TEST" XL_POST,                                GL_DEPTH_TEST);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_BITS" XL_POST,                                GL_DEPTH_BITS);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_CLEAR_VALUE" XL_POST,                         GL_DEPTH_CLEAR_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_FUNC" XL_POST,                                GL_DEPTH_FUNC);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_RANGE" XL_POST,                               GL_DEPTH_RANGE);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_WRITEMASK" XL_POST,                           GL_DEPTH_WRITEMASK);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_COMPONENT" XL_POST,                           GL_DEPTH_COMPONENT);
  DEFINE_INTEGER(XL_PRE "GL_LIGHTING" XL_POST,                                  GL_LIGHTING);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT0" XL_POST,                                    GL_LIGHT0);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT1" XL_POST,                                    GL_LIGHT1);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT2" XL_POST,                                    GL_LIGHT2);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT3" XL_POST,                                    GL_LIGHT3);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT4" XL_POST,                                    GL_LIGHT4);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT5" XL_POST,                                    GL_LIGHT5);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT6" XL_POST,                                    GL_LIGHT6);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT7" XL_POST,                                    GL_LIGHT7);
  DEFINE_INTEGER(XL_PRE "GL_SPOT_EXPONENT" XL_POST,                             GL_SPOT_EXPONENT);
  DEFINE_INTEGER(XL_PRE "GL_SPOT_CUTOFF" XL_POST,                               GL_SPOT_CUTOFF);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_ATTENUATION" XL_POST,                      GL_CONSTANT_ATTENUATION);
  DEFINE_INTEGER(XL_PRE "GL_LINEAR_ATTENUATION" XL_POST,                        GL_LINEAR_ATTENUATION);
  DEFINE_INTEGER(XL_PRE "GL_QUADRATIC_ATTENUATION" XL_POST,                     GL_QUADRATIC_ATTENUATION);
  DEFINE_INTEGER(XL_PRE "GL_AMBIENT" XL_POST,                                   GL_AMBIENT);
  DEFINE_INTEGER(XL_PRE "GL_DIFFUSE" XL_POST,                                   GL_DIFFUSE);
  DEFINE_INTEGER(XL_PRE "GL_SPECULAR" XL_POST,                                  GL_SPECULAR);
  DEFINE_INTEGER(XL_PRE "GL_SHININESS" XL_POST,                                 GL_SHININESS);
  DEFINE_INTEGER(XL_PRE "GL_EMISSION" XL_POST,                                  GL_EMISSION);
  DEFINE_INTEGER(XL_PRE "GL_POSITION" XL_POST,                                  GL_POSITION);
  DEFINE_INTEGER(XL_PRE "GL_SPOT_DIRECTION" XL_POST,                            GL_SPOT_DIRECTION);
  DEFINE_INTEGER(XL_PRE "GL_AMBIENT_AND_DIFFUSE" XL_POST,                       GL_AMBIENT_AND_DIFFUSE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEXES" XL_POST,                             GL_COLOR_INDEXES);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT_MODEL_TWO_SIDE" XL_POST,                      GL_LIGHT_MODEL_TWO_SIDE);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT_MODEL_LOCAL_VIEWER" XL_POST,                  GL_LIGHT_MODEL_LOCAL_VIEWER);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT_MODEL_AMBIENT" XL_POST,                       GL_LIGHT_MODEL_AMBIENT);
  DEFINE_INTEGER(XL_PRE "GL_FRONT_AND_BACK" XL_POST,                            GL_FRONT_AND_BACK);
  DEFINE_INTEGER(XL_PRE "GL_SHADE_MODEL" XL_POST,                               GL_SHADE_MODEL);
  DEFINE_INTEGER(XL_PRE "GL_FLAT" XL_POST,                                      GL_FLAT);
  DEFINE_INTEGER(XL_PRE "GL_SMOOTH" XL_POST,                                    GL_SMOOTH);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_MATERIAL" XL_POST,                            GL_COLOR_MATERIAL);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_MATERIAL_FACE" XL_POST,                       GL_COLOR_MATERIAL_FACE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_MATERIAL_PARAMETER" XL_POST,                  GL_COLOR_MATERIAL_PARAMETER);
  DEFINE_INTEGER(XL_PRE "GL_NORMALIZE" XL_POST,                                 GL_NORMALIZE);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE0" XL_POST,                               GL_CLIP_PLANE0);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE1" XL_POST,                               GL_CLIP_PLANE1);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE2" XL_POST,                               GL_CLIP_PLANE2);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE3" XL_POST,                               GL_CLIP_PLANE3);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE4" XL_POST,                               GL_CLIP_PLANE4);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_PLANE5" XL_POST,                               GL_CLIP_PLANE5);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_RED_BITS" XL_POST,                            GL_ACCUM_RED_BITS);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_GREEN_BITS" XL_POST,                          GL_ACCUM_GREEN_BITS);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_BLUE_BITS" XL_POST,                           GL_ACCUM_BLUE_BITS);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_ALPHA_BITS" XL_POST,                          GL_ACCUM_ALPHA_BITS);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_CLEAR_VALUE" XL_POST,                         GL_ACCUM_CLEAR_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM" XL_POST,                                     GL_ACCUM);
  DEFINE_INTEGER(XL_PRE "GL_ADD" XL_POST,                                       GL_ADD);
  DEFINE_INTEGER(XL_PRE "GL_LOAD" XL_POST,                                      GL_LOAD);
  DEFINE_INTEGER(XL_PRE "GL_MULT" XL_POST,                                      GL_MULT);
  DEFINE_INTEGER(XL_PRE "GL_RETURN" XL_POST,                                    GL_RETURN);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_TEST" XL_POST,                                GL_ALPHA_TEST);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_TEST_REF" XL_POST,                            GL_ALPHA_TEST_REF);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_TEST_FUNC" XL_POST,                           GL_ALPHA_TEST_FUNC);
  DEFINE_INTEGER(XL_PRE "GL_BLEND" XL_POST,                                     GL_BLEND);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_SRC" XL_POST,                                 GL_BLEND_SRC);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_DST" XL_POST,                                 GL_BLEND_DST);
  DEFINE_INTEGER(XL_PRE "GL_ZERO" XL_POST,                                      GL_ZERO);
  DEFINE_INTEGER(XL_PRE "GL_ONE" XL_POST,                                       GL_ONE);
  DEFINE_INTEGER(XL_PRE "GL_SRC_COLOR" XL_POST,                                 GL_SRC_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_SRC_COLOR" XL_POST,                       GL_ONE_MINUS_SRC_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_DST_COLOR" XL_POST,                                 GL_DST_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_DST_COLOR" XL_POST,                       GL_ONE_MINUS_DST_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_SRC_ALPHA" XL_POST,                                 GL_SRC_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_SRC_ALPHA" XL_POST,                       GL_ONE_MINUS_SRC_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_DST_ALPHA" XL_POST,                                 GL_DST_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_DST_ALPHA" XL_POST,                       GL_ONE_MINUS_DST_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_SRC_ALPHA_SATURATE" XL_POST,                        GL_SRC_ALPHA_SATURATE);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_COLOR" XL_POST,                            GL_CONSTANT_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_CONSTANT_COLOR" XL_POST,                  GL_ONE_MINUS_CONSTANT_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_ALPHA" XL_POST,                            GL_CONSTANT_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_CONSTANT_ALPHA" XL_POST,                  GL_ONE_MINUS_CONSTANT_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_FEEDBACK" XL_POST,                                  GL_FEEDBACK);
  DEFINE_INTEGER(XL_PRE "GL_RENDER" XL_POST,                                    GL_RENDER);
  DEFINE_INTEGER(XL_PRE "GL_SELECT" XL_POST,                                    GL_SELECT);
  DEFINE_INTEGER(XL_PRE "GL_2D" XL_POST,                                        GL_2D);
  DEFINE_INTEGER(XL_PRE "GL_3D" XL_POST,                                        GL_3D);
  DEFINE_INTEGER(XL_PRE "GL_3D_COLOR" XL_POST,                                  GL_3D_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_3D_COLOR_TEXTURE" XL_POST,                          GL_3D_COLOR_TEXTURE);
  DEFINE_INTEGER(XL_PRE "GL_4D_COLOR_TEXTURE" XL_POST,                          GL_4D_COLOR_TEXTURE);
  DEFINE_INTEGER(XL_PRE "GL_POINT_TOKEN" XL_POST,                               GL_POINT_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_LINE_TOKEN" XL_POST,                                GL_LINE_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_LINE_RESET_TOKEN" XL_POST,                          GL_LINE_RESET_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_TOKEN" XL_POST,                             GL_POLYGON_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_BITMAP_TOKEN" XL_POST,                              GL_BITMAP_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_DRAW_PIXEL_TOKEN" XL_POST,                          GL_DRAW_PIXEL_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_COPY_PIXEL_TOKEN" XL_POST,                          GL_COPY_PIXEL_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_PASS_THROUGH_TOKEN" XL_POST,                        GL_PASS_THROUGH_TOKEN);
  DEFINE_INTEGER(XL_PRE "GL_FEEDBACK_BUFFER_POINTER" XL_POST,                   GL_FEEDBACK_BUFFER_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_FEEDBACK_BUFFER_SIZE" XL_POST,                      GL_FEEDBACK_BUFFER_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_FEEDBACK_BUFFER_TYPE" XL_POST,                      GL_FEEDBACK_BUFFER_TYPE);
  DEFINE_INTEGER(XL_PRE "GL_SELECTION_BUFFER_POINTER" XL_POST,                  GL_SELECTION_BUFFER_POINTER);
  DEFINE_INTEGER(XL_PRE "GL_SELECTION_BUFFER_SIZE" XL_POST,                     GL_SELECTION_BUFFER_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_FOG" XL_POST,                                       GL_FOG);
  DEFINE_INTEGER(XL_PRE "GL_FOG_MODE" XL_POST,                                  GL_FOG_MODE);
  DEFINE_INTEGER(XL_PRE "GL_FOG_DENSITY" XL_POST,                               GL_FOG_DENSITY);
  DEFINE_INTEGER(XL_PRE "GL_FOG_COLOR" XL_POST,                                 GL_FOG_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_FOG_INDEX" XL_POST,                                 GL_FOG_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_FOG_START" XL_POST,                                 GL_FOG_START);
  DEFINE_INTEGER(XL_PRE "GL_FOG_END" XL_POST,                                   GL_FOG_END);
  DEFINE_INTEGER(XL_PRE "GL_LINEAR" XL_POST,                                    GL_LINEAR);
  DEFINE_INTEGER(XL_PRE "GL_EXP" XL_POST,                                       GL_EXP);
  DEFINE_INTEGER(XL_PRE "GL_EXP2" XL_POST,                                      GL_EXP2);
  DEFINE_INTEGER(XL_PRE "GL_LOGIC_OP" XL_POST,                                  GL_LOGIC_OP);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_LOGIC_OP" XL_POST,                            GL_INDEX_LOGIC_OP);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_LOGIC_OP" XL_POST,                            GL_COLOR_LOGIC_OP);
  DEFINE_INTEGER(XL_PRE "GL_LOGIC_OP_MODE" XL_POST,                             GL_LOGIC_OP_MODE);
  DEFINE_INTEGER(XL_PRE "GL_CLEAR" XL_POST,                                     GL_CLEAR);
  DEFINE_INTEGER(XL_PRE "GL_SET" XL_POST,                                       GL_SET);
  DEFINE_INTEGER(XL_PRE "GL_COPY" XL_POST,                                      GL_COPY);
  DEFINE_INTEGER(XL_PRE "GL_COPY_INVERTED" XL_POST,                             GL_COPY_INVERTED);
  DEFINE_INTEGER(XL_PRE "GL_NOOP" XL_POST,                                      GL_NOOP);
  DEFINE_INTEGER(XL_PRE "GL_INVERT" XL_POST,                                    GL_INVERT);
  DEFINE_INTEGER(XL_PRE "GL_AND" XL_POST,                                       GL_AND);
  DEFINE_INTEGER(XL_PRE "GL_NAND" XL_POST,                                      GL_NAND);
  DEFINE_INTEGER(XL_PRE "GL_OR" XL_POST,                                        GL_OR);
  DEFINE_INTEGER(XL_PRE "GL_NOR" XL_POST,                                       GL_NOR);
  DEFINE_INTEGER(XL_PRE "GL_XOR" XL_POST,                                       GL_XOR);
  DEFINE_INTEGER(XL_PRE "GL_EQUIV" XL_POST,                                     GL_EQUIV);
  DEFINE_INTEGER(XL_PRE "GL_AND_REVERSE" XL_POST,                               GL_AND_REVERSE);
  DEFINE_INTEGER(XL_PRE "GL_AND_INVERTED" XL_POST,                              GL_AND_INVERTED);
  DEFINE_INTEGER(XL_PRE "GL_OR_REVERSE" XL_POST,                                GL_OR_REVERSE);
  DEFINE_INTEGER(XL_PRE "GL_OR_INVERTED" XL_POST,                               GL_OR_INVERTED);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_TEST" XL_POST,                              GL_STENCIL_TEST);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_WRITEMASK" XL_POST,                         GL_STENCIL_WRITEMASK);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_BITS" XL_POST,                              GL_STENCIL_BITS);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_FUNC" XL_POST,                              GL_STENCIL_FUNC);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_VALUE_MASK" XL_POST,                        GL_STENCIL_VALUE_MASK);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_REF" XL_POST,                               GL_STENCIL_REF);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_FAIL" XL_POST,                              GL_STENCIL_FAIL);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_PASS_DEPTH_PASS" XL_POST,                   GL_STENCIL_PASS_DEPTH_PASS);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_PASS_DEPTH_FAIL" XL_POST,                   GL_STENCIL_PASS_DEPTH_FAIL);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_CLEAR_VALUE" XL_POST,                       GL_STENCIL_CLEAR_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_INDEX" XL_POST,                             GL_STENCIL_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_KEEP" XL_POST,                                      GL_KEEP);
  DEFINE_INTEGER(XL_PRE "GL_REPLACE" XL_POST,                                   GL_REPLACE);
  DEFINE_INTEGER(XL_PRE "GL_INCR" XL_POST,                                      GL_INCR);
  DEFINE_INTEGER(XL_PRE "GL_DECR" XL_POST,                                      GL_DECR);
  DEFINE_INTEGER(XL_PRE "GL_NONE" XL_POST,                                      GL_NONE);
  DEFINE_INTEGER(XL_PRE "GL_LEFT" XL_POST,                                      GL_LEFT);
  DEFINE_INTEGER(XL_PRE "GL_RIGHT" XL_POST,                                     GL_RIGHT);
  DEFINE_INTEGER(XL_PRE "GL_FRONT_LEFT" XL_POST,                                GL_FRONT_LEFT);
  DEFINE_INTEGER(XL_PRE "GL_FRONT_RIGHT" XL_POST,                               GL_FRONT_RIGHT);
  DEFINE_INTEGER(XL_PRE "GL_BACK_LEFT" XL_POST,                                 GL_BACK_LEFT);
  DEFINE_INTEGER(XL_PRE "GL_BACK_RIGHT" XL_POST,                                GL_BACK_RIGHT);
  DEFINE_INTEGER(XL_PRE "GL_AUX0" XL_POST,                                      GL_AUX0);
  DEFINE_INTEGER(XL_PRE "GL_AUX1" XL_POST,                                      GL_AUX1);
  DEFINE_INTEGER(XL_PRE "GL_AUX2" XL_POST,                                      GL_AUX2);
  DEFINE_INTEGER(XL_PRE "GL_AUX3" XL_POST,                                      GL_AUX3);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX" XL_POST,                               GL_COLOR_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_RED" XL_POST,                                       GL_RED);
  DEFINE_INTEGER(XL_PRE "GL_GREEN" XL_POST,                                     GL_GREEN);
  DEFINE_INTEGER(XL_PRE "GL_BLUE" XL_POST,                                      GL_BLUE);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA" XL_POST,                                     GL_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE" XL_POST,                                 GL_LUMINANCE);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE_ALPHA" XL_POST,                           GL_LUMINANCE_ALPHA);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_BITS" XL_POST,                                GL_ALPHA_BITS);
  DEFINE_INTEGER(XL_PRE "GL_RED_BITS" XL_POST,                                  GL_RED_BITS);
  DEFINE_INTEGER(XL_PRE "GL_GREEN_BITS" XL_POST,                                GL_GREEN_BITS);
  DEFINE_INTEGER(XL_PRE "GL_BLUE_BITS" XL_POST,                                 GL_BLUE_BITS);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_BITS" XL_POST,                                GL_INDEX_BITS);
  DEFINE_INTEGER(XL_PRE "GL_SUBPIXEL_BITS" XL_POST,                             GL_SUBPIXEL_BITS);
  DEFINE_INTEGER(XL_PRE "GL_AUX_BUFFERS" XL_POST,                               GL_AUX_BUFFERS);
  DEFINE_INTEGER(XL_PRE "GL_READ_BUFFER" XL_POST,                               GL_READ_BUFFER);
  DEFINE_INTEGER(XL_PRE "GL_DRAW_BUFFER" XL_POST,                               GL_DRAW_BUFFER);
  DEFINE_INTEGER(XL_PRE "GL_DOUBLEBUFFER" XL_POST,                              GL_DOUBLEBUFFER);
  DEFINE_INTEGER(XL_PRE "GL_STEREO" XL_POST,                                    GL_STEREO);
  DEFINE_INTEGER(XL_PRE "GL_BITMAP" XL_POST,                                    GL_BITMAP);
  DEFINE_INTEGER(XL_PRE "GL_COLOR" XL_POST,                                     GL_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH" XL_POST,                                     GL_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL" XL_POST,                                   GL_STENCIL);
  DEFINE_INTEGER(XL_PRE "GL_DITHER" XL_POST,                                    GL_DITHER);
  DEFINE_INTEGER(XL_PRE "GL_RGB" XL_POST,                                       GL_RGB);
  DEFINE_INTEGER(XL_PRE "GL_RGBA" XL_POST,                                      GL_RGBA);
  DEFINE_INTEGER(XL_PRE "GL_MAX_LIST_NESTING" XL_POST,                          GL_MAX_LIST_NESTING);
  DEFINE_INTEGER(XL_PRE "GL_MAX_ATTRIB_STACK_DEPTH" XL_POST,                    GL_MAX_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_MODELVIEW_STACK_DEPTH" XL_POST,                 GL_MAX_MODELVIEW_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_NAME_STACK_DEPTH" XL_POST,                      GL_MAX_NAME_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_PROJECTION_STACK_DEPTH" XL_POST,                GL_MAX_PROJECTION_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_TEXTURE_STACK_DEPTH" XL_POST,                   GL_MAX_TEXTURE_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_EVAL_ORDER" XL_POST,                            GL_MAX_EVAL_ORDER);
  DEFINE_INTEGER(XL_PRE "GL_MAX_LIGHTS" XL_POST,                                GL_MAX_LIGHTS);
  DEFINE_INTEGER(XL_PRE "GL_MAX_CLIP_PLANES" XL_POST,                           GL_MAX_CLIP_PLANES);
  DEFINE_INTEGER(XL_PRE "GL_MAX_TEXTURE_SIZE" XL_POST,                          GL_MAX_TEXTURE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_MAX_PIXEL_MAP_TABLE" XL_POST,                       GL_MAX_PIXEL_MAP_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_MAX_VIEWPORT_DIMS" XL_POST,                         GL_MAX_VIEWPORT_DIMS);
  DEFINE_INTEGER(XL_PRE "GL_MAX_CLIENT_ATTRIB_STACK_DEPTH" XL_POST,             GL_MAX_CLIENT_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_ATTRIB_STACK_DEPTH" XL_POST,                        GL_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_CLIENT_ATTRIB_STACK_DEPTH" XL_POST,                 GL_CLIENT_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_CLEAR_VALUE" XL_POST,                         GL_COLOR_CLEAR_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_WRITEMASK" XL_POST,                           GL_COLOR_WRITEMASK);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_INDEX" XL_POST,                             GL_CURRENT_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_COLOR" XL_POST,                             GL_CURRENT_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_NORMAL" XL_POST,                            GL_CURRENT_NORMAL);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_COLOR" XL_POST,                      GL_CURRENT_RASTER_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_DISTANCE" XL_POST,                   GL_CURRENT_RASTER_DISTANCE);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_INDEX" XL_POST,                      GL_CURRENT_RASTER_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_POSITION" XL_POST,                   GL_CURRENT_RASTER_POSITION);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_TEXTURE_COORDS" XL_POST,             GL_CURRENT_RASTER_TEXTURE_COORDS);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_RASTER_POSITION_VALID" XL_POST,             GL_CURRENT_RASTER_POSITION_VALID);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_TEXTURE_COORDS" XL_POST,                    GL_CURRENT_TEXTURE_COORDS);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_CLEAR_VALUE" XL_POST,                         GL_INDEX_CLEAR_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_MODE" XL_POST,                                GL_INDEX_MODE);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_WRITEMASK" XL_POST,                           GL_INDEX_WRITEMASK);
  DEFINE_INTEGER(XL_PRE "GL_MODELVIEW_MATRIX" XL_POST,                          GL_MODELVIEW_MATRIX);
  DEFINE_INTEGER(XL_PRE "GL_MODELVIEW_STACK_DEPTH" XL_POST,                     GL_MODELVIEW_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_NAME_STACK_DEPTH" XL_POST,                          GL_NAME_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_PROJECTION_MATRIX" XL_POST,                         GL_PROJECTION_MATRIX);
  DEFINE_INTEGER(XL_PRE "GL_PROJECTION_STACK_DEPTH" XL_POST,                    GL_PROJECTION_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_RENDER_MODE" XL_POST,                               GL_RENDER_MODE);
  DEFINE_INTEGER(XL_PRE "GL_RGBA_MODE" XL_POST,                                 GL_RGBA_MODE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MATRIX" XL_POST,                            GL_TEXTURE_MATRIX);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_STACK_DEPTH" XL_POST,                       GL_TEXTURE_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_VIEWPORT" XL_POST,                                  GL_VIEWPORT);
  DEFINE_INTEGER(XL_PRE "GL_AUTO_NORMAL" XL_POST,                               GL_AUTO_NORMAL);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_COLOR_4" XL_POST,                              GL_MAP1_COLOR_4);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_GRID_DOMAIN" XL_POST,                          GL_MAP1_GRID_DOMAIN);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_GRID_SEGMENTS" XL_POST,                        GL_MAP1_GRID_SEGMENTS);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_INDEX" XL_POST,                                GL_MAP1_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_NORMAL" XL_POST,                               GL_MAP1_NORMAL);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_TEXTURE_COORD_1" XL_POST,                      GL_MAP1_TEXTURE_COORD_1);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_TEXTURE_COORD_2" XL_POST,                      GL_MAP1_TEXTURE_COORD_2);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_TEXTURE_COORD_3" XL_POST,                      GL_MAP1_TEXTURE_COORD_3);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_TEXTURE_COORD_4" XL_POST,                      GL_MAP1_TEXTURE_COORD_4);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_VERTEX_3" XL_POST,                             GL_MAP1_VERTEX_3);
  DEFINE_INTEGER(XL_PRE "GL_MAP1_VERTEX_4" XL_POST,                             GL_MAP1_VERTEX_4);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_COLOR_4" XL_POST,                              GL_MAP2_COLOR_4);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_GRID_DOMAIN" XL_POST,                          GL_MAP2_GRID_DOMAIN);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_GRID_SEGMENTS" XL_POST,                        GL_MAP2_GRID_SEGMENTS);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_INDEX" XL_POST,                                GL_MAP2_INDEX);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_NORMAL" XL_POST,                               GL_MAP2_NORMAL);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_TEXTURE_COORD_1" XL_POST,                      GL_MAP2_TEXTURE_COORD_1);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_TEXTURE_COORD_2" XL_POST,                      GL_MAP2_TEXTURE_COORD_2);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_TEXTURE_COORD_3" XL_POST,                      GL_MAP2_TEXTURE_COORD_3);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_TEXTURE_COORD_4" XL_POST,                      GL_MAP2_TEXTURE_COORD_4);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_VERTEX_3" XL_POST,                             GL_MAP2_VERTEX_3);
  DEFINE_INTEGER(XL_PRE "GL_MAP2_VERTEX_4" XL_POST,                             GL_MAP2_VERTEX_4);
  DEFINE_INTEGER(XL_PRE "GL_COEFF" XL_POST,                                     GL_COEFF);
  DEFINE_INTEGER(XL_PRE "GL_DOMAIN" XL_POST,                                    GL_DOMAIN);
  DEFINE_INTEGER(XL_PRE "GL_ORDER" XL_POST,                                     GL_ORDER);
  DEFINE_INTEGER(XL_PRE "GL_FOG_HINT" XL_POST,                                  GL_FOG_HINT);
  DEFINE_INTEGER(XL_PRE "GL_LINE_SMOOTH_HINT" XL_POST,                          GL_LINE_SMOOTH_HINT);
  DEFINE_INTEGER(XL_PRE "GL_PERSPECTIVE_CORRECTION_HINT" XL_POST,               GL_PERSPECTIVE_CORRECTION_HINT);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SMOOTH_HINT" XL_POST,                         GL_POINT_SMOOTH_HINT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_SMOOTH_HINT" XL_POST,                       GL_POLYGON_SMOOTH_HINT);
  DEFINE_INTEGER(XL_PRE "GL_DONT_CARE" XL_POST,                                 GL_DONT_CARE);
  DEFINE_INTEGER(XL_PRE "GL_FASTEST" XL_POST,                                   GL_FASTEST);
  DEFINE_INTEGER(XL_PRE "GL_NICEST" XL_POST,                                    GL_NICEST);
  DEFINE_INTEGER(XL_PRE "GL_SCISSOR_TEST" XL_POST,                              GL_SCISSOR_TEST);
  DEFINE_INTEGER(XL_PRE "GL_SCISSOR_BOX" XL_POST,                               GL_SCISSOR_BOX);
  DEFINE_INTEGER(XL_PRE "GL_MAP_COLOR" XL_POST,                                 GL_MAP_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_MAP_STENCIL" XL_POST,                               GL_MAP_STENCIL);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_SHIFT" XL_POST,                               GL_INDEX_SHIFT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_OFFSET" XL_POST,                              GL_INDEX_OFFSET);
  DEFINE_INTEGER(XL_PRE "GL_RED_SCALE" XL_POST,                                 GL_RED_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_RED_BIAS" XL_POST,                                  GL_RED_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_GREEN_SCALE" XL_POST,                               GL_GREEN_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_GREEN_BIAS" XL_POST,                                GL_GREEN_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_BLUE_SCALE" XL_POST,                                GL_BLUE_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_BLUE_BIAS" XL_POST,                                 GL_BLUE_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_SCALE" XL_POST,                               GL_ALPHA_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA_BIAS" XL_POST,                                GL_ALPHA_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_SCALE" XL_POST,                               GL_DEPTH_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_BIAS" XL_POST,                                GL_DEPTH_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_S_TO_S_SIZE" XL_POST,                     GL_PIXEL_MAP_S_TO_S_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_I_SIZE" XL_POST,                     GL_PIXEL_MAP_I_TO_I_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_R_SIZE" XL_POST,                     GL_PIXEL_MAP_I_TO_R_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_G_SIZE" XL_POST,                     GL_PIXEL_MAP_I_TO_G_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_B_SIZE" XL_POST,                     GL_PIXEL_MAP_I_TO_B_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_A_SIZE" XL_POST,                     GL_PIXEL_MAP_I_TO_A_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_R_TO_R_SIZE" XL_POST,                     GL_PIXEL_MAP_R_TO_R_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_G_TO_G_SIZE" XL_POST,                     GL_PIXEL_MAP_G_TO_G_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_B_TO_B_SIZE" XL_POST,                     GL_PIXEL_MAP_B_TO_B_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_A_TO_A_SIZE" XL_POST,                     GL_PIXEL_MAP_A_TO_A_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_S_TO_S" XL_POST,                          GL_PIXEL_MAP_S_TO_S);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_I" XL_POST,                          GL_PIXEL_MAP_I_TO_I);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_R" XL_POST,                          GL_PIXEL_MAP_I_TO_R);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_G" XL_POST,                          GL_PIXEL_MAP_I_TO_G);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_B" XL_POST,                          GL_PIXEL_MAP_I_TO_B);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_I_TO_A" XL_POST,                          GL_PIXEL_MAP_I_TO_A);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_R_TO_R" XL_POST,                          GL_PIXEL_MAP_R_TO_R);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_G_TO_G" XL_POST,                          GL_PIXEL_MAP_G_TO_G);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_B_TO_B" XL_POST,                          GL_PIXEL_MAP_B_TO_B);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MAP_A_TO_A" XL_POST,                          GL_PIXEL_MAP_A_TO_A);
  DEFINE_INTEGER(XL_PRE "GL_PACK_ALIGNMENT" XL_POST,                            GL_PACK_ALIGNMENT);
  DEFINE_INTEGER(XL_PRE "GL_PACK_LSB_FIRST" XL_POST,                            GL_PACK_LSB_FIRST);
  DEFINE_INTEGER(XL_PRE "GL_PACK_ROW_LENGTH" XL_POST,                           GL_PACK_ROW_LENGTH);
  DEFINE_INTEGER(XL_PRE "GL_PACK_SKIP_PIXELS" XL_POST,                          GL_PACK_SKIP_PIXELS);
  DEFINE_INTEGER(XL_PRE "GL_PACK_SKIP_ROWS" XL_POST,                            GL_PACK_SKIP_ROWS);
  DEFINE_INTEGER(XL_PRE "GL_PACK_SWAP_BYTES" XL_POST,                           GL_PACK_SWAP_BYTES);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_ALIGNMENT" XL_POST,                          GL_UNPACK_ALIGNMENT);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_LSB_FIRST" XL_POST,                          GL_UNPACK_LSB_FIRST);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_ROW_LENGTH" XL_POST,                         GL_UNPACK_ROW_LENGTH);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_SKIP_PIXELS" XL_POST,                        GL_UNPACK_SKIP_PIXELS);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_SKIP_ROWS" XL_POST,                          GL_UNPACK_SKIP_ROWS);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_SWAP_BYTES" XL_POST,                         GL_UNPACK_SWAP_BYTES);
  DEFINE_INTEGER(XL_PRE "GL_ZOOM_X" XL_POST,                                    GL_ZOOM_X);
  DEFINE_INTEGER(XL_PRE "GL_ZOOM_Y" XL_POST,                                    GL_ZOOM_Y);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_ENV" XL_POST,                               GL_TEXTURE_ENV);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_ENV_MODE" XL_POST,                          GL_TEXTURE_ENV_MODE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_1D" XL_POST,                                GL_TEXTURE_1D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_2D" XL_POST,                                GL_TEXTURE_2D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_WRAP_S" XL_POST,                            GL_TEXTURE_WRAP_S);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_WRAP_T" XL_POST,                            GL_TEXTURE_WRAP_T);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MAG_FILTER" XL_POST,                        GL_TEXTURE_MAG_FILTER);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MIN_FILTER" XL_POST,                        GL_TEXTURE_MIN_FILTER);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_ENV_COLOR" XL_POST,                         GL_TEXTURE_ENV_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GEN_S" XL_POST,                             GL_TEXTURE_GEN_S);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GEN_T" XL_POST,                             GL_TEXTURE_GEN_T);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GEN_MODE" XL_POST,                          GL_TEXTURE_GEN_MODE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BORDER_COLOR" XL_POST,                      GL_TEXTURE_BORDER_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_WIDTH" XL_POST,                             GL_TEXTURE_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_HEIGHT" XL_POST,                            GL_TEXTURE_HEIGHT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BORDER" XL_POST,                            GL_TEXTURE_BORDER);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COMPONENTS" XL_POST,                        GL_TEXTURE_COMPONENTS);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_RED_SIZE" XL_POST,                          GL_TEXTURE_RED_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GREEN_SIZE" XL_POST,                        GL_TEXTURE_GREEN_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BLUE_SIZE" XL_POST,                         GL_TEXTURE_BLUE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_ALPHA_SIZE" XL_POST,                        GL_TEXTURE_ALPHA_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_LUMINANCE_SIZE" XL_POST,                    GL_TEXTURE_LUMINANCE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_INTENSITY_SIZE" XL_POST,                    GL_TEXTURE_INTENSITY_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_NEAREST_MIPMAP_NEAREST" XL_POST,                    GL_NEAREST_MIPMAP_NEAREST);
  DEFINE_INTEGER(XL_PRE "GL_NEAREST_MIPMAP_LINEAR" XL_POST,                     GL_NEAREST_MIPMAP_LINEAR);
  DEFINE_INTEGER(XL_PRE "GL_LINEAR_MIPMAP_NEAREST" XL_POST,                     GL_LINEAR_MIPMAP_NEAREST);
  DEFINE_INTEGER(XL_PRE "GL_LINEAR_MIPMAP_LINEAR" XL_POST,                      GL_LINEAR_MIPMAP_LINEAR);
  DEFINE_INTEGER(XL_PRE "GL_OBJECT_LINEAR" XL_POST,                             GL_OBJECT_LINEAR);
  DEFINE_INTEGER(XL_PRE "GL_OBJECT_PLANE" XL_POST,                              GL_OBJECT_PLANE);
  DEFINE_INTEGER(XL_PRE "GL_EYE_LINEAR" XL_POST,                                GL_EYE_LINEAR);
  DEFINE_INTEGER(XL_PRE "GL_EYE_PLANE" XL_POST,                                 GL_EYE_PLANE);
  DEFINE_INTEGER(XL_PRE "GL_SPHERE_MAP" XL_POST,                                GL_SPHERE_MAP);
  DEFINE_INTEGER(XL_PRE "GL_DECAL" XL_POST,                                     GL_DECAL);
  DEFINE_INTEGER(XL_PRE "GL_MODULATE" XL_POST,                                  GL_MODULATE);
  DEFINE_INTEGER(XL_PRE "GL_NEAREST" XL_POST,                                   GL_NEAREST);
  DEFINE_INTEGER(XL_PRE "GL_REPEAT" XL_POST,                                    GL_REPEAT);
  DEFINE_INTEGER(XL_PRE "GL_CLAMP" XL_POST,                                     GL_CLAMP);
  DEFINE_INTEGER(XL_PRE "GL_S" XL_POST,                                         GL_S);
  DEFINE_INTEGER(XL_PRE "GL_T" XL_POST,                                         GL_T);
  DEFINE_INTEGER(XL_PRE "GL_R" XL_POST,                                         GL_R);
  DEFINE_INTEGER(XL_PRE "GL_Q" XL_POST,                                         GL_Q);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GEN_R" XL_POST,                             GL_TEXTURE_GEN_R);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_GEN_Q" XL_POST,                             GL_TEXTURE_GEN_Q);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_TEXTURE_1D" XL_POST,                          GL_PROXY_TEXTURE_1D);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_TEXTURE_2D" XL_POST,                          GL_PROXY_TEXTURE_2D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_PRIORITY" XL_POST,                          GL_TEXTURE_PRIORITY);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_RESIDENT" XL_POST,                          GL_TEXTURE_RESIDENT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BINDING_1D" XL_POST,                        GL_TEXTURE_BINDING_1D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BINDING_2D" XL_POST,                        GL_TEXTURE_BINDING_2D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_INTERNAL_FORMAT" XL_POST,                   GL_TEXTURE_INTERNAL_FORMAT);
  DEFINE_INTEGER(XL_PRE "GL_PACK_SKIP_IMAGES" XL_POST,                          GL_PACK_SKIP_IMAGES);
  DEFINE_INTEGER(XL_PRE "GL_PACK_IMAGE_HEIGHT" XL_POST,                         GL_PACK_IMAGE_HEIGHT);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_SKIP_IMAGES" XL_POST,                        GL_UNPACK_SKIP_IMAGES);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_IMAGE_HEIGHT" XL_POST,                       GL_UNPACK_IMAGE_HEIGHT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_3D" XL_POST,                                GL_TEXTURE_3D);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_TEXTURE_3D" XL_POST,                          GL_PROXY_TEXTURE_3D);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_DEPTH" XL_POST,                             GL_TEXTURE_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_WRAP_R" XL_POST,                            GL_TEXTURE_WRAP_R);
  DEFINE_INTEGER(XL_PRE "GL_MAX_3D_TEXTURE_SIZE" XL_POST,                       GL_MAX_3D_TEXTURE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BINDING_3D" XL_POST,                        GL_TEXTURE_BINDING_3D);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA4" XL_POST,                                    GL_ALPHA4);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA8" XL_POST,                                    GL_ALPHA8);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA12" XL_POST,                                   GL_ALPHA12);
  DEFINE_INTEGER(XL_PRE "GL_ALPHA16" XL_POST,                                   GL_ALPHA16);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE4" XL_POST,                                GL_LUMINANCE4);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE8" XL_POST,                                GL_LUMINANCE8);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE12" XL_POST,                               GL_LUMINANCE12);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE16" XL_POST,                               GL_LUMINANCE16);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE4_ALPHA4" XL_POST,                         GL_LUMINANCE4_ALPHA4);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE6_ALPHA2" XL_POST,                         GL_LUMINANCE6_ALPHA2);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE8_ALPHA8" XL_POST,                         GL_LUMINANCE8_ALPHA8);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE12_ALPHA4" XL_POST,                        GL_LUMINANCE12_ALPHA4);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE12_ALPHA12" XL_POST,                       GL_LUMINANCE12_ALPHA12);
  DEFINE_INTEGER(XL_PRE "GL_LUMINANCE16_ALPHA16" XL_POST,                       GL_LUMINANCE16_ALPHA16);
  DEFINE_INTEGER(XL_PRE "GL_INTENSITY" XL_POST,                                 GL_INTENSITY);
  DEFINE_INTEGER(XL_PRE "GL_INTENSITY4" XL_POST,                                GL_INTENSITY4);
  DEFINE_INTEGER(XL_PRE "GL_INTENSITY8" XL_POST,                                GL_INTENSITY8);
  DEFINE_INTEGER(XL_PRE "GL_INTENSITY12" XL_POST,                               GL_INTENSITY12);
  DEFINE_INTEGER(XL_PRE "GL_INTENSITY16" XL_POST,                               GL_INTENSITY16);
  DEFINE_INTEGER(XL_PRE "GL_R3_G3_B2" XL_POST,                                  GL_R3_G3_B2);
  DEFINE_INTEGER(XL_PRE "GL_RGB4" XL_POST,                                      GL_RGB4);
  DEFINE_INTEGER(XL_PRE "GL_RGB5" XL_POST,                                      GL_RGB5);
  DEFINE_INTEGER(XL_PRE "GL_RGB8" XL_POST,                                      GL_RGB8);
  DEFINE_INTEGER(XL_PRE "GL_RGB10" XL_POST,                                     GL_RGB10);
  DEFINE_INTEGER(XL_PRE "GL_RGB12" XL_POST,                                     GL_RGB12);
  DEFINE_INTEGER(XL_PRE "GL_RGB16" XL_POST,                                     GL_RGB16);
  DEFINE_INTEGER(XL_PRE "GL_RGBA2" XL_POST,                                     GL_RGBA2);
  DEFINE_INTEGER(XL_PRE "GL_RGBA4" XL_POST,                                     GL_RGBA4);
  DEFINE_INTEGER(XL_PRE "GL_RGB5_A1" XL_POST,                                   GL_RGB5_A1);
  DEFINE_INTEGER(XL_PRE "GL_RGBA8" XL_POST,                                     GL_RGBA8);
  DEFINE_INTEGER(XL_PRE "GL_RGB10_A2" XL_POST,                                  GL_RGB10_A2);
  DEFINE_INTEGER(XL_PRE "GL_RGBA12" XL_POST,                                    GL_RGBA12);
  DEFINE_INTEGER(XL_PRE "GL_RGBA16" XL_POST,                                    GL_RGBA16);
  DEFINE_INTEGER(XL_PRE "GL_VENDOR" XL_POST,                                    GL_VENDOR);
  DEFINE_INTEGER(XL_PRE "GL_RENDERER" XL_POST,                                  GL_RENDERER);
  DEFINE_INTEGER(XL_PRE "GL_VERSION" XL_POST,                                   GL_VERSION);
  DEFINE_INTEGER(XL_PRE "GL_EXTENSIONS" XL_POST,                                GL_EXTENSIONS);
  DEFINE_INTEGER(XL_PRE "GL_NO_ERROR" XL_POST,                                  GL_NO_ERROR);
  DEFINE_INTEGER(XL_PRE "GL_INVALID_VALUE" XL_POST,                             GL_INVALID_VALUE);
  DEFINE_INTEGER(XL_PRE "GL_INVALID_ENUM" XL_POST,                              GL_INVALID_ENUM);
  DEFINE_INTEGER(XL_PRE "GL_INVALID_OPERATION" XL_POST,                         GL_INVALID_OPERATION);
  DEFINE_INTEGER(XL_PRE "GL_STACK_OVERFLOW" XL_POST,                            GL_STACK_OVERFLOW);
  DEFINE_INTEGER(XL_PRE "GL_STACK_UNDERFLOW" XL_POST,                           GL_STACK_UNDERFLOW);
  DEFINE_INTEGER(XL_PRE "GL_OUT_OF_MEMORY" XL_POST,                             GL_OUT_OF_MEMORY);
  DEFINE_INTEGER(XL_PRE "GL_RESCALE_NORMAL" XL_POST,                            GL_RESCALE_NORMAL);
  DEFINE_INTEGER(XL_PRE "GL_CLAMP_TO_EDGE" XL_POST,                             GL_CLAMP_TO_EDGE);
  DEFINE_INTEGER(XL_PRE "GL_MAX_ELEMENTS_VERTICES" XL_POST,                     GL_MAX_ELEMENTS_VERTICES);
  DEFINE_INTEGER(XL_PRE "GL_MAX_ELEMENTS_INDICES" XL_POST,                      GL_MAX_ELEMENTS_INDICES);
  DEFINE_INTEGER(XL_PRE "GL_BGR" XL_POST,                                       GL_BGR);
  DEFINE_INTEGER(XL_PRE "GL_BGRA" XL_POST,                                      GL_BGRA);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_BYTE_3_3_2" XL_POST,                       GL_UNSIGNED_BYTE_3_3_2);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_BYTE_2_3_3_REV" XL_POST,                   GL_UNSIGNED_BYTE_2_3_3_REV);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_5_6_5" XL_POST,                      GL_UNSIGNED_SHORT_5_6_5);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_5_6_5_REV" XL_POST,                  GL_UNSIGNED_SHORT_5_6_5_REV);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_4_4_4_4" XL_POST,                    GL_UNSIGNED_SHORT_4_4_4_4);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_4_4_4_4_REV" XL_POST,                GL_UNSIGNED_SHORT_4_4_4_4_REV);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_5_5_5_1" XL_POST,                    GL_UNSIGNED_SHORT_5_5_5_1);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_SHORT_1_5_5_5_REV" XL_POST,                GL_UNSIGNED_SHORT_1_5_5_5_REV);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_INT_8_8_8_8" XL_POST,                      GL_UNSIGNED_INT_8_8_8_8);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_INT_8_8_8_8_REV" XL_POST,                  GL_UNSIGNED_INT_8_8_8_8_REV);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_INT_10_10_10_2" XL_POST,                   GL_UNSIGNED_INT_10_10_10_2);
  DEFINE_INTEGER(XL_PRE "GL_UNSIGNED_INT_2_10_10_10_REV" XL_POST,               GL_UNSIGNED_INT_2_10_10_10_REV);
  DEFINE_INTEGER(XL_PRE "GL_LIGHT_MODEL_COLOR_CONTROL" XL_POST,                 GL_LIGHT_MODEL_COLOR_CONTROL);
  DEFINE_INTEGER(XL_PRE "GL_SINGLE_COLOR" XL_POST,                              GL_SINGLE_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_SEPARATE_SPECULAR_COLOR" XL_POST,                   GL_SEPARATE_SPECULAR_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MIN_LOD" XL_POST,                           GL_TEXTURE_MIN_LOD);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MAX_LOD" XL_POST,                           GL_TEXTURE_MAX_LOD);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BASE_LEVEL" XL_POST,                        GL_TEXTURE_BASE_LEVEL);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_MAX_LEVEL" XL_POST,                         GL_TEXTURE_MAX_LEVEL);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE" XL_POST,                               GL_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_COLOR_TABLE" XL_POST,              GL_POST_CONVOLUTION_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_COLOR_TABLE" XL_POST,             GL_POST_COLOR_MATRIX_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_COLOR_TABLE" XL_POST,                         GL_PROXY_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_POST_CONVOLUTION_COLOR_TABLE" XL_POST,        GL_PROXY_POST_CONVOLUTION_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE" XL_POST,       GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_SCALE" XL_POST,                         GL_COLOR_TABLE_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_BIAS" XL_POST,                          GL_COLOR_TABLE_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_FORMAT" XL_POST,                        GL_COLOR_TABLE_FORMAT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_WIDTH" XL_POST,                         GL_COLOR_TABLE_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_RED_SIZE" XL_POST,                      GL_COLOR_TABLE_RED_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_GREEN_SIZE" XL_POST,                    GL_COLOR_TABLE_GREEN_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_BLUE_SIZE" XL_POST,                     GL_COLOR_TABLE_BLUE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_ALPHA_SIZE" XL_POST,                    GL_COLOR_TABLE_ALPHA_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_LUMINANCE_SIZE" XL_POST,                GL_COLOR_TABLE_LUMINANCE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_INTENSITY_SIZE" XL_POST,                GL_COLOR_TABLE_INTENSITY_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_1D" XL_POST,                            GL_CONVOLUTION_1D);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_2D" XL_POST,                            GL_CONVOLUTION_2D);
  DEFINE_INTEGER(XL_PRE "GL_SEPARABLE_2D" XL_POST,                              GL_SEPARABLE_2D);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_BORDER_MODE" XL_POST,                   GL_CONVOLUTION_BORDER_MODE);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_FILTER_SCALE" XL_POST,                  GL_CONVOLUTION_FILTER_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_FILTER_BIAS" XL_POST,                   GL_CONVOLUTION_FILTER_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_REDUCE" XL_POST,                                    GL_REDUCE);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_FORMAT" XL_POST,                        GL_CONVOLUTION_FORMAT);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_WIDTH" XL_POST,                         GL_CONVOLUTION_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_HEIGHT" XL_POST,                        GL_CONVOLUTION_HEIGHT);
  DEFINE_INTEGER(XL_PRE "GL_MAX_CONVOLUTION_WIDTH" XL_POST,                     GL_MAX_CONVOLUTION_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_CONVOLUTION_HEIGHT" XL_POST,                    GL_MAX_CONVOLUTION_HEIGHT);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_RED_SCALE" XL_POST,                GL_POST_CONVOLUTION_RED_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_GREEN_SCALE" XL_POST,              GL_POST_CONVOLUTION_GREEN_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_BLUE_SCALE" XL_POST,               GL_POST_CONVOLUTION_BLUE_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_ALPHA_SCALE" XL_POST,              GL_POST_CONVOLUTION_ALPHA_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_RED_BIAS" XL_POST,                 GL_POST_CONVOLUTION_RED_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_GREEN_BIAS" XL_POST,               GL_POST_CONVOLUTION_GREEN_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_BLUE_BIAS" XL_POST,                GL_POST_CONVOLUTION_BLUE_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_CONVOLUTION_ALPHA_BIAS" XL_POST,               GL_POST_CONVOLUTION_ALPHA_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_BORDER" XL_POST,                           GL_CONSTANT_BORDER);
  DEFINE_INTEGER(XL_PRE "GL_REPLICATE_BORDER" XL_POST,                          GL_REPLICATE_BORDER);
  DEFINE_INTEGER(XL_PRE "GL_CONVOLUTION_BORDER_COLOR" XL_POST,                  GL_CONVOLUTION_BORDER_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_MATRIX" XL_POST,                              GL_COLOR_MATRIX);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_MATRIX_STACK_DEPTH" XL_POST,                  GL_COLOR_MATRIX_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_MAX_COLOR_MATRIX_STACK_DEPTH" XL_POST,              GL_MAX_COLOR_MATRIX_STACK_DEPTH);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_RED_SCALE" XL_POST,               GL_POST_COLOR_MATRIX_RED_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_GREEN_SCALE" XL_POST,             GL_POST_COLOR_MATRIX_GREEN_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_BLUE_SCALE" XL_POST,              GL_POST_COLOR_MATRIX_BLUE_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_ALPHA_SCALE" XL_POST,             GL_POST_COLOR_MATRIX_ALPHA_SCALE);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_RED_BIAS" XL_POST,                GL_POST_COLOR_MATRIX_RED_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_GREEN_BIAS" XL_POST,              GL_POST_COLOR_MATRIX_GREEN_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_BLUE_BIAS" XL_POST,               GL_POST_COLOR_MATRIX_BLUE_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_POST_COLOR_MATRIX_ALPHA_BIAS" XL_POST,              GL_POST_COLOR_MATRIX_ALPHA_BIAS);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM" XL_POST,                                 GL_HISTOGRAM);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_HISTOGRAM" XL_POST,                           GL_PROXY_HISTOGRAM);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_WIDTH" XL_POST,                           GL_HISTOGRAM_WIDTH);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_FORMAT" XL_POST,                          GL_HISTOGRAM_FORMAT);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_RED_SIZE" XL_POST,                        GL_HISTOGRAM_RED_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_GREEN_SIZE" XL_POST,                      GL_HISTOGRAM_GREEN_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_BLUE_SIZE" XL_POST,                       GL_HISTOGRAM_BLUE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_ALPHA_SIZE" XL_POST,                      GL_HISTOGRAM_ALPHA_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_LUMINANCE_SIZE" XL_POST,                  GL_HISTOGRAM_LUMINANCE_SIZE);
  DEFINE_INTEGER(XL_PRE "GL_HISTOGRAM_SINK" XL_POST,                            GL_HISTOGRAM_SINK);
  DEFINE_INTEGER(XL_PRE "GL_MINMAX" XL_POST,                                    GL_MINMAX);
  DEFINE_INTEGER(XL_PRE "GL_MINMAX_FORMAT" XL_POST,                             GL_MINMAX_FORMAT);
  DEFINE_INTEGER(XL_PRE "GL_MINMAX_SINK" XL_POST,                               GL_MINMAX_SINK);
  DEFINE_INTEGER(XL_PRE "GL_TABLE_TOO_LARGE" XL_POST,                           GL_TABLE_TOO_LARGE);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_EQUATION" XL_POST,                            GL_BLEND_EQUATION);
  DEFINE_INTEGER(XL_PRE "GL_MIN" XL_POST,                                       GL_MIN);
  DEFINE_INTEGER(XL_PRE "GL_MAX" XL_POST,                                       GL_MAX);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_ADD" XL_POST,                                  GL_FUNC_ADD);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_SUBTRACT" XL_POST,                             GL_FUNC_SUBTRACT);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_REVERSE_SUBTRACT" XL_POST,                     GL_FUNC_REVERSE_SUBTRACT);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_COLOR" XL_POST,                               GL_BLEND_COLOR);
  DEFINE_INTEGER(XL_PRE "GL_CURRENT_BIT" XL_POST,                               GL_CURRENT_BIT);
  DEFINE_INTEGER(XL_PRE "GL_POINT_BIT" XL_POST,                                 GL_POINT_BIT);
  DEFINE_INTEGER(XL_PRE "GL_LINE_BIT" XL_POST,                                  GL_LINE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_BIT" XL_POST,                               GL_POLYGON_BIT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_STIPPLE_BIT" XL_POST,                       GL_POLYGON_STIPPLE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_PIXEL_MODE_BIT" XL_POST,                            GL_PIXEL_MODE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_LIGHTING_BIT" XL_POST,                              GL_LIGHTING_BIT);
  DEFINE_INTEGER(XL_PRE "GL_FOG_BIT" XL_POST,                                   GL_FOG_BIT);
  DEFINE_INTEGER(XL_PRE "GL_DEPTH_BUFFER_BIT" XL_POST,                          GL_DEPTH_BUFFER_BIT);
  DEFINE_INTEGER(XL_PRE "GL_ACCUM_BUFFER_BIT" XL_POST,                          GL_ACCUM_BUFFER_BIT);
  DEFINE_INTEGER(XL_PRE "GL_STENCIL_BUFFER_BIT" XL_POST,                        GL_STENCIL_BUFFER_BIT);
  DEFINE_INTEGER(XL_PRE "GL_VIEWPORT_BIT" XL_POST,                              GL_VIEWPORT_BIT);
  DEFINE_INTEGER(XL_PRE "GL_TRANSFORM_BIT" XL_POST,                             GL_TRANSFORM_BIT);
  DEFINE_INTEGER(XL_PRE "GL_ENABLE_BIT" XL_POST,                                GL_ENABLE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_BUFFER_BIT" XL_POST,                          GL_COLOR_BUFFER_BIT);
  DEFINE_INTEGER(XL_PRE "GL_HINT_BIT" XL_POST,                                  GL_HINT_BIT);
  DEFINE_INTEGER(XL_PRE "GL_EVAL_BIT" XL_POST,                                  GL_EVAL_BIT);
  DEFINE_INTEGER(XL_PRE "GL_LIST_BIT" XL_POST,                                  GL_LIST_BIT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_BIT" XL_POST,                               GL_TEXTURE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_SCISSOR_BIT" XL_POST,                               GL_SCISSOR_BIT);
  DEFINE_INTEGER(XL_PRE "GL_ALL_ATTRIB_BITS" XL_POST,                           GL_ALL_ATTRIB_BITS);
  DEFINE_INTEGER(XL_PRE "GL_CLIENT_PIXEL_STORE_BIT" XL_POST,                    GL_CLIENT_PIXEL_STORE_BIT);
  DEFINE_INTEGER(XL_PRE "GL_CLIENT_VERTEX_ARRAY_BIT" XL_POST,                   GL_CLIENT_VERTEX_ARRAY_BIT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE0_ARB" XL_POST,                              GL_TEXTURE0_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE1_ARB" XL_POST,                              GL_TEXTURE1_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE2_ARB" XL_POST,                              GL_TEXTURE2_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE3_ARB" XL_POST,                              GL_TEXTURE3_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE4_ARB" XL_POST,                              GL_TEXTURE4_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE5_ARB" XL_POST,                              GL_TEXTURE5_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE6_ARB" XL_POST,                              GL_TEXTURE6_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE7_ARB" XL_POST,                              GL_TEXTURE7_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE8_ARB" XL_POST,                              GL_TEXTURE8_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE9_ARB" XL_POST,                              GL_TEXTURE9_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE10_ARB" XL_POST,                             GL_TEXTURE10_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE11_ARB" XL_POST,                             GL_TEXTURE11_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE12_ARB" XL_POST,                             GL_TEXTURE12_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE13_ARB" XL_POST,                             GL_TEXTURE13_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE14_ARB" XL_POST,                             GL_TEXTURE14_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE15_ARB" XL_POST,                             GL_TEXTURE15_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE16_ARB" XL_POST,                             GL_TEXTURE16_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE17_ARB" XL_POST,                             GL_TEXTURE17_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE18_ARB" XL_POST,                             GL_TEXTURE18_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE19_ARB" XL_POST,                             GL_TEXTURE19_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE20_ARB" XL_POST,                             GL_TEXTURE20_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE21_ARB" XL_POST,                             GL_TEXTURE21_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE22_ARB" XL_POST,                             GL_TEXTURE22_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE23_ARB" XL_POST,                             GL_TEXTURE23_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE24_ARB" XL_POST,                             GL_TEXTURE24_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE25_ARB" XL_POST,                             GL_TEXTURE25_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE26_ARB" XL_POST,                             GL_TEXTURE26_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE27_ARB" XL_POST,                             GL_TEXTURE27_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE28_ARB" XL_POST,                             GL_TEXTURE28_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE29_ARB" XL_POST,                             GL_TEXTURE29_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE30_ARB" XL_POST,                             GL_TEXTURE30_ARB);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE31_ARB" XL_POST,                             GL_TEXTURE31_ARB);
  DEFINE_INTEGER(XL_PRE "GL_ACTIVE_TEXTURE_ARB" XL_POST,                        GL_ACTIVE_TEXTURE_ARB);
  DEFINE_INTEGER(XL_PRE "GL_CLIENT_ACTIVE_TEXTURE_ARB" XL_POST,                 GL_CLIENT_ACTIVE_TEXTURE_ARB);
  DEFINE_INTEGER(XL_PRE "GL_MAX_TEXTURE_UNITS_ARB" XL_POST,                     GL_MAX_TEXTURE_UNITS_ARB);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_COLOR_EXT" XL_POST,                        GL_CONSTANT_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_CONSTANT_COLOR_EXT" XL_POST,              GL_ONE_MINUS_CONSTANT_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GL_CONSTANT_ALPHA_EXT" XL_POST,                        GL_CONSTANT_ALPHA_EXT);
  DEFINE_INTEGER(XL_PRE "GL_ONE_MINUS_CONSTANT_ALPHA_EXT" XL_POST,              GL_ONE_MINUS_CONSTANT_ALPHA_EXT);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_COLOR_EXT" XL_POST,                           GL_BLEND_COLOR_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_EXT" XL_POST,                        GL_POLYGON_OFFSET_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_FACTOR_EXT" XL_POST,                 GL_POLYGON_OFFSET_FACTOR_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POLYGON_OFFSET_BIAS_EXT" XL_POST,                   GL_POLYGON_OFFSET_BIAS_EXT);
  DEFINE_INTEGER(XL_PRE "GL_PACK_SKIP_IMAGES_EXT" XL_POST,                      GL_PACK_SKIP_IMAGES_EXT);
  DEFINE_INTEGER(XL_PRE "GL_PACK_IMAGE_HEIGHT_EXT" XL_POST,                     GL_PACK_IMAGE_HEIGHT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_SKIP_IMAGES_EXT" XL_POST,                    GL_UNPACK_SKIP_IMAGES_EXT);
  DEFINE_INTEGER(XL_PRE "GL_UNPACK_IMAGE_HEIGHT_EXT" XL_POST,                   GL_UNPACK_IMAGE_HEIGHT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_3D_EXT" XL_POST,                            GL_TEXTURE_3D_EXT);
  DEFINE_INTEGER(XL_PRE "GL_PROXY_TEXTURE_3D_EXT" XL_POST,                      GL_PROXY_TEXTURE_3D_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_DEPTH_EXT" XL_POST,                         GL_TEXTURE_DEPTH_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_WRAP_R_EXT" XL_POST,                        GL_TEXTURE_WRAP_R_EXT);
  DEFINE_INTEGER(XL_PRE "GL_MAX_3D_TEXTURE_SIZE_EXT" XL_POST,                   GL_MAX_3D_TEXTURE_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_3D_BINDING_EXT" XL_POST,                    GL_TEXTURE_3D_BINDING_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_PRIORITY_EXT" XL_POST,                      GL_TEXTURE_PRIORITY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_RESIDENT_EXT" XL_POST,                      GL_TEXTURE_RESIDENT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_1D_BINDING_EXT" XL_POST,                    GL_TEXTURE_1D_BINDING_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_2D_BINDING_EXT" XL_POST,                    GL_TEXTURE_2D_BINDING_EXT);
  DEFINE_INTEGER(XL_PRE "GL_RESCALE_NORMAL_EXT" XL_POST,                        GL_RESCALE_NORMAL_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_EXT" XL_POST,                          GL_VERTEX_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_EXT" XL_POST,                          GL_NORMAL_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_EXT" XL_POST,                           GL_COLOR_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_EXT" XL_POST,                           GL_INDEX_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_EXT" XL_POST,                   GL_TEXTURE_COORD_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_EXT" XL_POST,                       GL_EDGE_FLAG_ARRAY_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_SIZE_EXT" XL_POST,                     GL_VERTEX_ARRAY_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_TYPE_EXT" XL_POST,                     GL_VERTEX_ARRAY_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_STRIDE_EXT" XL_POST,                   GL_VERTEX_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_COUNT_EXT" XL_POST,                    GL_VERTEX_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_TYPE_EXT" XL_POST,                     GL_NORMAL_ARRAY_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_STRIDE_EXT" XL_POST,                   GL_NORMAL_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_COUNT_EXT" XL_POST,                    GL_NORMAL_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_SIZE_EXT" XL_POST,                      GL_COLOR_ARRAY_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_TYPE_EXT" XL_POST,                      GL_COLOR_ARRAY_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_STRIDE_EXT" XL_POST,                    GL_COLOR_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_COUNT_EXT" XL_POST,                     GL_COLOR_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_TYPE_EXT" XL_POST,                      GL_INDEX_ARRAY_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_STRIDE_EXT" XL_POST,                    GL_INDEX_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_COUNT_EXT" XL_POST,                     GL_INDEX_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_SIZE_EXT" XL_POST,              GL_TEXTURE_COORD_ARRAY_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_TYPE_EXT" XL_POST,              GL_TEXTURE_COORD_ARRAY_TYPE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_STRIDE_EXT" XL_POST,            GL_TEXTURE_COORD_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_COUNT_EXT" XL_POST,             GL_TEXTURE_COORD_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_STRIDE_EXT" XL_POST,                GL_EDGE_FLAG_ARRAY_STRIDE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_COUNT_EXT" XL_POST,                 GL_EDGE_FLAG_ARRAY_COUNT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_VERTEX_ARRAY_POINTER_EXT" XL_POST,                  GL_VERTEX_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_ARRAY_POINTER_EXT" XL_POST,                  GL_NORMAL_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_ARRAY_POINTER_EXT" XL_POST,                   GL_COLOR_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INDEX_ARRAY_POINTER_EXT" XL_POST,                   GL_INDEX_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_COORD_ARRAY_POINTER_EXT" XL_POST,           GL_TEXTURE_COORD_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_EDGE_FLAG_ARRAY_POINTER_EXT" XL_POST,               GL_EDGE_FLAG_ARRAY_POINTER_EXT);
  DEFINE_INTEGER(XL_PRE "GL_CLAMP_TO_EDGE_SGIS" XL_POST,                        GL_CLAMP_TO_EDGE_SGIS);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_ADD_EXT" XL_POST,                              GL_FUNC_ADD_EXT);
  DEFINE_INTEGER(XL_PRE "GL_MIN_EXT" XL_POST,                                   GL_MIN_EXT);
  DEFINE_INTEGER(XL_PRE "GL_MAX_EXT" XL_POST,                                   GL_MAX_EXT);
  DEFINE_INTEGER(XL_PRE "GL_BLEND_EQUATION_EXT" XL_POST,                        GL_BLEND_EQUATION_EXT);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_SUBTRACT_EXT" XL_POST,                         GL_FUNC_SUBTRACT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_FUNC_REVERSE_SUBTRACT_EXT" XL_POST,                 GL_FUNC_REVERSE_SUBTRACT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SIZE_MIN_EXT" XL_POST,                        GL_POINT_SIZE_MIN_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POINT_SIZE_MAX_EXT" XL_POST,                        GL_POINT_SIZE_MAX_EXT);
  DEFINE_INTEGER(XL_PRE "GL_POINT_FADE_THRESHOLD_SIZE_EXT" XL_POST,             GL_POINT_FADE_THRESHOLD_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_DISTANCE_ATTENUATION_EXT" XL_POST,                  GL_DISTANCE_ATTENUATION_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TABLE_TOO_LARGE_EXT" XL_POST,                       GL_TABLE_TOO_LARGE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_FORMAT_EXT" XL_POST,                    GL_COLOR_TABLE_FORMAT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_WIDTH_EXT" XL_POST,                     GL_COLOR_TABLE_WIDTH_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_RED_SIZE_EXT" XL_POST,                  GL_COLOR_TABLE_RED_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_GREEN_SIZE_EXT" XL_POST,                GL_COLOR_TABLE_GREEN_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_BLUE_SIZE_EXT" XL_POST,                 GL_COLOR_TABLE_BLUE_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_ALPHA_SIZE_EXT" XL_POST,                GL_COLOR_TABLE_ALPHA_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_LUMINANCE_SIZE_EXT" XL_POST,            GL_COLOR_TABLE_LUMINANCE_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_TABLE_INTENSITY_SIZE_EXT" XL_POST,            GL_COLOR_TABLE_INTENSITY_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_TEXTURE_INDEX_SIZE_EXT" XL_POST,                    GL_TEXTURE_INDEX_SIZE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX1_EXT" XL_POST,                          GL_COLOR_INDEX1_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX2_EXT" XL_POST,                          GL_COLOR_INDEX2_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX4_EXT" XL_POST,                          GL_COLOR_INDEX4_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX8_EXT" XL_POST,                          GL_COLOR_INDEX8_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX12_EXT" XL_POST,                         GL_COLOR_INDEX12_EXT);
  DEFINE_INTEGER(XL_PRE "GL_COLOR_INDEX16_EXT" XL_POST,                         GL_COLOR_INDEX16_EXT);
  DEFINE_INTEGER(XL_PRE "GL_CLIP_VOLUME_CLIPPING_HINT_EXT" XL_POST,             GL_CLIP_VOLUME_CLIPPING_HINT_EXT);
  DEFINE_INTEGER(XL_PRE "GL_SHARED_TEXTURE_PALETTE_EXT" XL_POST,                GL_SHARED_TEXTURE_PALETTE_EXT);
  DEFINE_INTEGER(XL_PRE "GL_INCR_WRAP_EXT" XL_POST,                             GL_INCR_WRAP_EXT);
  DEFINE_INTEGER(XL_PRE "GL_DECR_WRAP_EXT" XL_POST,                             GL_DECR_WRAP_EXT);
  DEFINE_INTEGER(XL_PRE "GL_NORMAL_MAP_NV" XL_POST,                             GL_NORMAL_MAP_NV);
  DEFINE_INTEGER(XL_PRE "GL_REFLECTION_MAP_NV" XL_POST,                         GL_REFLECTION_MAP_NV);
  DEFINE_INTEGER(XL_PRE "GLU_FALSE" XL_POST,                                    GLU_FALSE);
  DEFINE_INTEGER(XL_PRE "GLU_TRUE" XL_POST,                                     GLU_TRUE);
  DEFINE_INTEGER(XL_PRE "GLU_VERSION" XL_POST,                                  GLU_VERSION);
  DEFINE_INTEGER(XL_PRE "GLU_EXTENSIONS" XL_POST,                               GLU_EXTENSIONS);
  DEFINE_INTEGER(XL_PRE "GLU_INVALID_ENUM" XL_POST,                             GLU_INVALID_ENUM);
  DEFINE_INTEGER(XL_PRE "GLU_INVALID_VALUE" XL_POST,                            GLU_INVALID_VALUE);
  DEFINE_INTEGER(XL_PRE "GLU_OUT_OF_MEMORY" XL_POST,                            GLU_OUT_OF_MEMORY);
  DEFINE_INTEGER(XL_PRE "GLU_OUTLINE_POLYGON" XL_POST,                          GLU_OUTLINE_POLYGON);
  DEFINE_INTEGER(XL_PRE "GLU_OUTLINE_PATCH" XL_POST,                            GLU_OUTLINE_PATCH);
  DEFINE_INTEGER(XL_PRE "GLU_ERROR" XL_POST,                                    GLU_ERROR);
  DEFINE_INTEGER(XL_PRE "GLU_AUTO_LOAD_MATRIX" XL_POST,                         GLU_AUTO_LOAD_MATRIX);
  DEFINE_INTEGER(XL_PRE "GLU_CULLING" XL_POST,                                  GLU_CULLING);
  DEFINE_INTEGER(XL_PRE "GLU_SAMPLING_TOLERANCE" XL_POST,                       GLU_SAMPLING_TOLERANCE);
  DEFINE_INTEGER(XL_PRE "GLU_DISPLAY_MODE" XL_POST,                             GLU_DISPLAY_MODE);
  DEFINE_INTEGER(XL_PRE "GLU_PARAMETRIC_TOLERANCE" XL_POST,                     GLU_PARAMETRIC_TOLERANCE);
  DEFINE_INTEGER(XL_PRE "GLU_SAMPLING_METHOD" XL_POST,                          GLU_SAMPLING_METHOD);
  DEFINE_INTEGER(XL_PRE "GLU_U_STEP" XL_POST,                                   GLU_U_STEP);
  DEFINE_INTEGER(XL_PRE "GLU_V_STEP" XL_POST,                                   GLU_V_STEP);
  DEFINE_INTEGER(XL_PRE "GLU_PATH_LENGTH" XL_POST,                              GLU_PATH_LENGTH);
  DEFINE_INTEGER(XL_PRE "GLU_PARAMETRIC_ERROR" XL_POST,                         GLU_PARAMETRIC_ERROR);
  DEFINE_INTEGER(XL_PRE "GLU_DOMAIN_DISTANCE" XL_POST,                          GLU_DOMAIN_DISTANCE);
  DEFINE_INTEGER(XL_PRE "GLU_MAP1_TRIM_2" XL_POST,                              GLU_MAP1_TRIM_2);
  DEFINE_INTEGER(XL_PRE "GLU_MAP1_TRIM_3" XL_POST,                              GLU_MAP1_TRIM_3);
  DEFINE_INTEGER(XL_PRE "GLU_POINT" XL_POST,                                    GLU_POINT);
  DEFINE_INTEGER(XL_PRE "GLU_LINE" XL_POST,                                     GLU_LINE);
  DEFINE_INTEGER(XL_PRE "GLU_FILL" XL_POST,                                     GLU_FILL);
  DEFINE_INTEGER(XL_PRE "GLU_SILHOUETTE" XL_POST,                               GLU_SILHOUETTE);
  DEFINE_INTEGER(XL_PRE "GLU_SMOOTH" XL_POST,                                   GLU_SMOOTH);
  DEFINE_INTEGER(XL_PRE "GLU_FLAT" XL_POST,                                     GLU_FLAT);
  DEFINE_INTEGER(XL_PRE "GLU_NONE" XL_POST,                                     GLU_NONE);
  DEFINE_INTEGER(XL_PRE "GLU_OUTSIDE" XL_POST,                                  GLU_OUTSIDE);
  DEFINE_INTEGER(XL_PRE "GLU_INSIDE" XL_POST,                                   GLU_INSIDE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_BEGIN" XL_POST,                               GLU_TESS_BEGIN);
  DEFINE_INTEGER(XL_PRE "GLU_BEGIN" XL_POST,                                    GLU_BEGIN);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_VERTEX" XL_POST,                              GLU_TESS_VERTEX);
  DEFINE_INTEGER(XL_PRE "GLU_VERTEX" XL_POST,                                   GLU_VERTEX);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_END" XL_POST,                                 GLU_TESS_END);
  DEFINE_INTEGER(XL_PRE "GLU_END" XL_POST,                                      GLU_END);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_ERROR" XL_POST,                               GLU_TESS_ERROR);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_EDGE_FLAG" XL_POST,                           GLU_TESS_EDGE_FLAG);
  DEFINE_INTEGER(XL_PRE "GLU_EDGE_FLAG" XL_POST,                                GLU_EDGE_FLAG);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_COMBINE" XL_POST,                             GLU_TESS_COMBINE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_BEGIN_DATA" XL_POST,                          GLU_TESS_BEGIN_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_VERTEX_DATA" XL_POST,                         GLU_TESS_VERTEX_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_END_DATA" XL_POST,                            GLU_TESS_END_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_ERROR_DATA" XL_POST,                          GLU_TESS_ERROR_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_EDGE_FLAG_DATA" XL_POST,                      GLU_TESS_EDGE_FLAG_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_COMBINE_DATA" XL_POST,                        GLU_TESS_COMBINE_DATA);
  DEFINE_INTEGER(XL_PRE "GLU_CW" XL_POST,                                       GLU_CW);
  DEFINE_INTEGER(XL_PRE "GLU_CCW" XL_POST,                                      GLU_CCW);
  DEFINE_INTEGER(XL_PRE "GLU_INTERIOR" XL_POST,                                 GLU_INTERIOR);
  DEFINE_INTEGER(XL_PRE "GLU_EXTERIOR" XL_POST,                                 GLU_EXTERIOR);
  DEFINE_INTEGER(XL_PRE "GLU_UNKNOWN" XL_POST,                                  GLU_UNKNOWN);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_RULE" XL_POST,                        GLU_TESS_WINDING_RULE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_BOUNDARY_ONLY" XL_POST,                       GLU_TESS_BOUNDARY_ONLY);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_TOLERANCE" XL_POST,                           GLU_TESS_TOLERANCE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_ODD" XL_POST,                         GLU_TESS_WINDING_ODD);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_NONZERO" XL_POST,                     GLU_TESS_WINDING_NONZERO);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_POSITIVE" XL_POST,                    GLU_TESS_WINDING_POSITIVE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_NEGATIVE" XL_POST,                    GLU_TESS_WINDING_NEGATIVE);
  DEFINE_INTEGER(XL_PRE "GLU_TESS_WINDING_ABS_GEQ_TWO" XL_POST,                 GLU_TESS_WINDING_ABS_GEQ_TWO);
}

/* -------------------------------- initialization -------------------------------- */

static int gl_already_inited = 0;

#if HAVE_GUILE
 XEN init_gl(void);
 XEN init_gl(void)
#else
 XEN Init_libgl(void);
 XEN Init_libgl(void)
#endif
{
  if (!gl_already_inited)
    {
      define_integers();
      define_functions();
      XEN_YES_WE_HAVE("gl");
#if HAVE_GUILE
      XEN_EVAL_C_STRING("(define gl-version \"18-Jun-02\")");
#endif
      gl_already_inited = 1;
    }
  return(XEN_FALSE);
}
