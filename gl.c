/* gl.c: Guile, Gauche, Ruby, and Forth bindings for GL, GLU
 *   generated automatically from makegl.scm and gldata.scm
 *   needs xen.h
 *
 * reference args are ignored if passed, resultant values are returned in a list.
 * the various "v" forms are omitted for now -- are they needed in this context?
 * 'gl is added to *features*
 *
 * HISTORY:
 *     30-Mar-06: check for glu.h, omit GLU_* if necessary.  Add Forth support.
 *     --------
 *     13-Jun-05: merged gl-ruby.c into gl.c.
 *     --------
 *     10-Mar:    Gl_Version.
 *     1-Feb-03:  glGet* funcs now try to handle multiple return values correctly.
 *     --------
 *     18-Nov:    added more GtkGlext bindings.
 *     1-Aug:     removed all 'EXT' junk.
 *     24-July:   changed Guile prefix (R5RS reserves vertical-bar).
 *     18-June:   GL 1.1 stubs.
 *     4-June:    GtkGLext support.
 *     20-May-02: initial version.
 */

#include <mus-config.h>

#if HAVE_EXTENSION_LANGUAGE
#if USE_GTK
  #include <gtk/gtkgl.h>
#endif
#include <GL/gl.h>
#if HAVE_GLU
  #include <GL/glu.h>
#endif
#if USE_MOTIF
  #include <GL/glx.h>
#endif
#include <string.h>

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

#ifndef unsigned_long
  /* for FreeBSD (thanks to Michael Scholz) (can't use ulong here due to collisions elsewhere) */
  typedef unsigned long unsigned_long;
#endif

/* prefix for all names */
#if HAVE_SCHEME
  #define XL_PRE ""
  #define XL_POST ""
#endif
#if HAVE_RUBY
/* for Ruby, XG PRE needs to be uppercase */
  #define XL_PRE "R"
  #define XL_POST ""
#endif
#if HAVE_FORTH
  #define XL_PRE "F"
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
#define XL_TYPE_PTR_2(Name, XType) \
  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);}


/* ---------------------------------------- types ---------------------------------------- */

#if USE_MOTIF
XL_TYPE(XVisualInfo, XVisualInfo*)
XL_TYPE_1(Display, Display*)
#define C_TO_XEN_int(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_int(Arg) (int)(XEN_TO_C_INT(Arg))
#define XEN_int_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_1(int_, int*)
XL_TYPE(GLXContext, GLXContext)
#define XEN_TO_C_unsigned_long(Arg) (unsigned_long)(XEN_TO_C_ULONG(Arg))
#define XEN_unsigned_long_P(Arg) XEN_ULONG_P(Arg)
#define C_TO_XEN_Bool(Arg) C_TO_XEN_BOOLEAN(Arg)
#define XEN_TO_C_Bool(Arg) (Bool)(XEN_TO_C_BOOLEAN(Arg))
#define XEN_Bool_P(Arg) XEN_BOOLEAN_P(Arg)
XL_TYPE(GLXPixmap, GLXPixmap)
XL_TYPE_1(Pixmap, Pixmap)
XL_TYPE(Window, Window)
XL_TYPE_1(Font, Font)
#define C_TO_XEN_char_(Arg) C_TO_XEN_STRING(Arg)
#define XEN_TO_C_char_(Arg) (char*)(XEN_TO_C_STRING(Arg))
#define XEN_char__P(Arg) XEN_STRING_P(Arg)
#endif
#if USE_GTK
#define C_TO_XEN_gboolean(Arg) C_TO_XEN_BOOLEAN(Arg)
#define XEN_TO_C_gboolean(Arg) (gboolean)(XEN_TO_C_BOOLEAN(Arg))
#define XEN_gboolean_P(Arg) XEN_BOOLEAN_P(Arg)
XL_TYPE_PTR_1(int_, int*)
#define C_TO_XEN_char_(Arg) C_TO_XEN_STRING(Arg)
#define XEN_TO_C_char_(Arg) (char*)(XEN_TO_C_STRING(Arg))
#define XEN_char__P(Arg) XEN_STRING_P(Arg)
XL_TYPE_PTR(GdkGLConfig_, GdkGLConfig*)
XL_TYPE_1(GdkGLConfigMode, GdkGLConfigMode)
#define C_TO_XEN_int(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_int(Arg) (int)(XEN_TO_C_INT(Arg))
#define XEN_int_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_2(GdkColormap_, GdkColormap*)
XL_TYPE_PTR_2(GdkVisual_, GdkVisual*)
#define C_TO_XEN_gint(Arg) C_TO_XEN_INT(Arg)
#define XEN_TO_C_gint(Arg) (gint)(XEN_TO_C_INT(Arg))
#define XEN_gint_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR(GdkGLDrawable_, GdkGLDrawable*)
XL_TYPE_PTR(GdkGLContext_, GdkGLContext*)
XL_TYPE_PTR(GdkGLPixmap_, GdkGLPixmap*)
XL_TYPE_PTR(GdkPixmap_, GdkPixmap*)
XL_TYPE_PTR(GdkGLWindow_, GdkGLWindow*)
XL_TYPE_PTR(GdkWindow_, GdkWindow*)
XL_TYPE_PTR_2(PangoFont_, PangoFont*)
XL_TYPE_PTR_1(PangoFontDescription_, PangoFontDescription*)
XL_TYPE_PTR_1(GtkWidget_, GtkWidget*)
#endif
#define C_TO_XEN_GLfloat(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLfloat(Arg) (GLfloat)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLfloat_P(Arg) XEN_NUMBER_P(Arg)
#define XEN_TO_C_GLclampf(Arg) (GLclampf)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLclampf_P(Arg) XEN_NUMBER_P(Arg)
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
#define XEN_TO_C_GLsizei(Arg) (GLsizei)(XEN_TO_C_INT(Arg))
#define XEN_GLsizei_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR_1(GLdouble_, GLdouble*)
#define C_TO_XEN_GLdouble(Arg) C_TO_XEN_DOUBLE(Arg)
#define XEN_TO_C_GLdouble(Arg) (GLdouble)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLdouble_P(Arg) XEN_NUMBER_P(Arg)
#define C_TO_XEN_constchar_(Arg) C_TO_XEN_STRING((char *)(Arg))
#define XEN_TO_C_GLclampd(Arg) (GLclampd)(XEN_TO_C_DOUBLE(Arg))
#define XEN_GLclampd_P(Arg) XEN_NUMBER_P(Arg)
XL_TYPE_PTR_1(GLfloat_, GLfloat*)
XL_TYPE_PTR_1(GLvoid_, GLvoid*)
#define XEN_TO_C_GLshort(Arg) (GLshort)(XEN_TO_C_INT(Arg))
#define XEN_GLshort_P(Arg) XEN_INTEGER_P(Arg)
#define XEN_TO_C_GLbyte(Arg) (GLbyte)(XEN_TO_C_INT(Arg))
#define XEN_GLbyte_P(Arg) XEN_INTEGER_P(Arg)
#define XEN_TO_C_GLubyte(Arg) (GLubyte)(XEN_TO_C_INT(Arg))
#define XEN_GLubyte_P(Arg) XEN_INTEGER_P(Arg)
XL_TYPE_PTR(void_, void*)
XL_TYPE_PTR_1(GLuint_, GLuint*)
XL_TYPE_PTR_1(GLboolean_, GLboolean*)
#ifdef GLU_VERSION_1_2
XL_TYPE_PTR(GLUtesselator_, GLUtesselator*)
#endif
XL_TYPE_PTR_1(GLint_, GLint*)


/* ---------------------------------------- state readback confusion ---------------------------------------- */

static int how_many_vals(GLenum gl)
{
  switch (gl)
    {
    case GL_CURRENT_COLOR:
    case GL_CURRENT_TEXTURE_COORDS:
    case GL_CURRENT_RASTER_POSITION:
    case GL_CURRENT_RASTER_COLOR:
    case GL_CURRENT_RASTER_TEXTURE_COORDS:
    case GL_VIEWPORT:
    case GL_FOG_COLOR:
    case GL_AMBIENT:
    case GL_DIFFUSE:
    case GL_SPECULAR:
    case GL_EMISSION:
    case GL_LIGHT_MODEL_AMBIENT:
    case GL_SCISSOR_BOX:
    case GL_COLOR_WRITEMASK:
    case GL_COLOR_CLEAR_VALUE:
      return(4);
      break;
    case GL_MODELVIEW_MATRIX:
    case GL_PROJECTION_MATRIX:
    case GL_TEXTURE_MATRIX:
      return(16);
      break;
    case GL_CURRENT_NORMAL:
    case GL_SPOT_DIRECTION:
      return(3);
      break;
    case GL_DEPTH_RANGE:
    case GL_LINE_WIDTH_RANGE:
      return(2);
      break;
    default: return(1); break; /* try to squelch c++ babbling */
    }
  return(1);
}


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
  #define H_glXCopyContext "void glXCopyContext(Display* dpy, GLXContext src, GLXContext dst, unsigned_long mask)"
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXCopyContext", "Display*");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(src), src, 2, "glXCopyContext", "GLXContext");
  XEN_ASSERT_TYPE(XEN_GLXContext_P(dst), dst, 3, "glXCopyContext", "GLXContext");
  XEN_ASSERT_TYPE(XEN_unsigned_long_P(mask), mask, 4, "glXCopyContext", "unsigned_long");
  glXCopyContext(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(src), XEN_TO_C_GLXContext(dst), XEN_TO_C_unsigned_long(mask));
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
  int ref_value[1];
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXGetConfig", "Display*");
  XEN_ASSERT_TYPE(XEN_XVisualInfo_P(vis), vis, 2, "glXGetConfig", "XVisualInfo*");
  XEN_ASSERT_TYPE(XEN_int_P(attrib), attrib, 3, "glXGetConfig", "int");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_int(glXGetConfig(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_int(attrib), ref_value));
    return(XEN_LIST_2(result, C_TO_XEN_int(ref_value[0])));
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
  int ref_errorBase[1];
  int ref_eventBase[1];
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryExtension", "Display*");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_Bool(glXQueryExtension(XEN_TO_C_Display(dpy), ref_errorBase, ref_eventBase));
    return(XEN_LIST_3(result, C_TO_XEN_int(ref_errorBase[0]), C_TO_XEN_int(ref_eventBase[0])));
   }
}

static XEN gxg_glXQueryVersion(XEN dpy, XEN major, XEN minor)
{
  #define H_glXQueryVersion "Bool glXQueryVersion(Display* dpy, int* [major], int* [minor])"
  int ref_major[1];
  int ref_minor[1];
  XEN_ASSERT_TYPE(XEN_Display_P(dpy), dpy, 1, "glXQueryVersion", "Display*");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_Bool(glXQueryVersion(XEN_TO_C_Display(dpy), ref_major, ref_minor));
    return(XEN_LIST_3(result, C_TO_XEN_int(ref_major[0]), C_TO_XEN_int(ref_minor[0])));
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
#if USE_GTK
static XEN gxg_gdk_gl_query_extension(void)
{
  #define H_gdk_gl_query_extension "gboolean gdk_gl_query_extension( void)"
  return(C_TO_XEN_gboolean(gdk_gl_query_extension()));
}

static XEN gxg_gdk_gl_query_version(XEN major, XEN minor)
{
  #define H_gdk_gl_query_version "gboolean gdk_gl_query_version(int* major, int* minor)"
  XEN_ASSERT_TYPE(XEN_int__P(major), major, 1, "gdk_gl_query_version", "int*");
  XEN_ASSERT_TYPE(XEN_int__P(minor), minor, 2, "gdk_gl_query_version", "int*");
  return(C_TO_XEN_gboolean(gdk_gl_query_version(XEN_TO_C_int_(major), XEN_TO_C_int_(minor))));
}

static XEN gxg_gdk_gl_query_gl_extension(XEN extension)
{
  #define H_gdk_gl_query_gl_extension "gboolean gdk_gl_query_gl_extension(char* extension)"
  XEN_ASSERT_TYPE(XEN_char__P(extension), extension, 1, "gdk_gl_query_gl_extension", "char*");
  return(C_TO_XEN_gboolean(gdk_gl_query_gl_extension(XEN_TO_C_char_(extension))));
}

static XEN gxg_gdk_gl_config_new(XEN attrib_list)
{
  #define H_gdk_gl_config_new "GdkGLConfig* gdk_gl_config_new(int* attrib_list)"
  XEN_ASSERT_TYPE(XEN_int__P(attrib_list), attrib_list, 1, "gdk_gl_config_new", "int*");
  return(C_TO_XEN_GdkGLConfig_(gdk_gl_config_new(XEN_TO_C_int_(attrib_list))));
}

static XEN gxg_gdk_gl_config_new_by_mode(XEN mode)
{
  #define H_gdk_gl_config_new_by_mode "GdkGLConfig* gdk_gl_config_new_by_mode(GdkGLConfigMode mode)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfigMode_P(mode), mode, 1, "gdk_gl_config_new_by_mode", "GdkGLConfigMode");
  return(C_TO_XEN_GdkGLConfig_(gdk_gl_config_new_by_mode(XEN_TO_C_GdkGLConfigMode(mode))));
}

static XEN gxg_gdk_gl_config_get_attrib(XEN glconfig, XEN attribute, XEN value)
{
  #define H_gdk_gl_config_get_attrib "gboolean gdk_gl_config_get_attrib(GdkGLConfig* glconfig, int attribute, \
int* [value])"
  int ref_value[1];
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_get_attrib", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_int_P(attribute), attribute, 2, "gdk_gl_config_get_attrib", "int");
  {
    XEN result = XEN_FALSE;
    result = C_TO_XEN_gboolean(gdk_gl_config_get_attrib(XEN_TO_C_GdkGLConfig_(glconfig), XEN_TO_C_int(attribute), ref_value));
    return(XEN_LIST_2(result, C_TO_XEN_int(ref_value[0])));
   }
}

static XEN gxg_gdk_gl_config_get_colormap(XEN glconfig)
{
  #define H_gdk_gl_config_get_colormap "GdkColormap* gdk_gl_config_get_colormap(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_get_colormap", "GdkGLConfig*");
  return(C_TO_XEN_GdkColormap_(gdk_gl_config_get_colormap(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_get_visual(XEN glconfig)
{
  #define H_gdk_gl_config_get_visual "GdkVisual* gdk_gl_config_get_visual(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_get_visual", "GdkGLConfig*");
  return(C_TO_XEN_GdkVisual_(gdk_gl_config_get_visual(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_get_depth(XEN glconfig)
{
  #define H_gdk_gl_config_get_depth "gint gdk_gl_config_get_depth(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_get_depth", "GdkGLConfig*");
  return(C_TO_XEN_gint(gdk_gl_config_get_depth(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_is_rgba(XEN glconfig)
{
  #define H_gdk_gl_config_is_rgba "gboolean gdk_gl_config_is_rgba(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_is_rgba", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_is_rgba(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_is_double_buffered(XEN glconfig)
{
  #define H_gdk_gl_config_is_double_buffered "gboolean gdk_gl_config_is_double_buffered(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_is_double_buffered", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_is_double_buffered(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_is_stereo(XEN glconfig)
{
  #define H_gdk_gl_config_is_stereo "gboolean gdk_gl_config_is_stereo(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_is_stereo", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_is_stereo(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_has_alpha(XEN glconfig)
{
  #define H_gdk_gl_config_has_alpha "gboolean gdk_gl_config_has_alpha(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_has_alpha", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_has_alpha(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_has_depth_buffer(XEN glconfig)
{
  #define H_gdk_gl_config_has_depth_buffer "gboolean gdk_gl_config_has_depth_buffer(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_has_depth_buffer", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_has_depth_buffer(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_has_stencil_buffer(XEN glconfig)
{
  #define H_gdk_gl_config_has_stencil_buffer "gboolean gdk_gl_config_has_stencil_buffer(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_has_stencil_buffer", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_has_stencil_buffer(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_config_has_accum_buffer(XEN glconfig)
{
  #define H_gdk_gl_config_has_accum_buffer "gboolean gdk_gl_config_has_accum_buffer(GdkGLConfig* glconfig)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_config_has_accum_buffer", "GdkGLConfig*");
  return(C_TO_XEN_gboolean(gdk_gl_config_has_accum_buffer(XEN_TO_C_GdkGLConfig_(glconfig))));
}

static XEN gxg_gdk_gl_context_get_gl_drawable(XEN glcontext)
{
  #define H_gdk_gl_context_get_gl_drawable "GdkGLDrawable* gdk_gl_context_get_gl_drawable(GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 1, "gdk_gl_context_get_gl_drawable", "GdkGLContext*");
  return(C_TO_XEN_GdkGLDrawable_(gdk_gl_context_get_gl_drawable(XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_context_get_gl_config(XEN glcontext)
{
  #define H_gdk_gl_context_get_gl_config "GdkGLConfig* gdk_gl_context_get_gl_config(GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 1, "gdk_gl_context_get_gl_config", "GdkGLContext*");
  return(C_TO_XEN_GdkGLConfig_(gdk_gl_context_get_gl_config(XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_context_get_share_list(XEN glcontext)
{
  #define H_gdk_gl_context_get_share_list "GdkGLContext* gdk_gl_context_get_share_list(GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 1, "gdk_gl_context_get_share_list", "GdkGLContext*");
  return(C_TO_XEN_GdkGLContext_(gdk_gl_context_get_share_list(XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_context_is_direct(XEN glcontext)
{
  #define H_gdk_gl_context_is_direct "gboolean gdk_gl_context_is_direct(GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 1, "gdk_gl_context_is_direct", "GdkGLContext*");
  return(C_TO_XEN_gboolean(gdk_gl_context_is_direct(XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_context_get_render_type(XEN glcontext)
{
  #define H_gdk_gl_context_get_render_type "int gdk_gl_context_get_render_type(GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 1, "gdk_gl_context_get_render_type", "GdkGLContext*");
  return(C_TO_XEN_int(gdk_gl_context_get_render_type(XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_drawable_make_current(XEN gldrawable, XEN glcontext)
{
  #define H_gdk_gl_drawable_make_current "gboolean gdk_gl_drawable_make_current(GdkGLDrawable* gldrawable, \
GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_make_current", "GdkGLDrawable*");
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 2, "gdk_gl_drawable_make_current", "GdkGLContext*");
  return(C_TO_XEN_gboolean(gdk_gl_drawable_make_current(XEN_TO_C_GdkGLDrawable_(gldrawable), XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_drawable_is_double_buffered(XEN gldrawable)
{
  #define H_gdk_gl_drawable_is_double_buffered "gboolean gdk_gl_drawable_is_double_buffered(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_is_double_buffered", "GdkGLDrawable*");
  return(C_TO_XEN_gboolean(gdk_gl_drawable_is_double_buffered(XEN_TO_C_GdkGLDrawable_(gldrawable))));
}

static XEN gxg_gdk_gl_drawable_swap_buffers(XEN gldrawable)
{
  #define H_gdk_gl_drawable_swap_buffers "void gdk_gl_drawable_swap_buffers(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_swap_buffers", "GdkGLDrawable*");
  gdk_gl_drawable_swap_buffers(XEN_TO_C_GdkGLDrawable_(gldrawable));
  return(XEN_FALSE);
}

static XEN gxg_gdk_gl_drawable_wait_gl(XEN gldrawable)
{
  #define H_gdk_gl_drawable_wait_gl "void gdk_gl_drawable_wait_gl(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_wait_gl", "GdkGLDrawable*");
  gdk_gl_drawable_wait_gl(XEN_TO_C_GdkGLDrawable_(gldrawable));
  return(XEN_FALSE);
}

static XEN gxg_gdk_gl_drawable_wait_gdk(XEN gldrawable)
{
  #define H_gdk_gl_drawable_wait_gdk "void gdk_gl_drawable_wait_gdk(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_wait_gdk", "GdkGLDrawable*");
  gdk_gl_drawable_wait_gdk(XEN_TO_C_GdkGLDrawable_(gldrawable));
  return(XEN_FALSE);
}

static XEN gxg_gdk_gl_drawable_get_gl_config(XEN gldrawable)
{
  #define H_gdk_gl_drawable_get_gl_config "GdkGLConfig* gdk_gl_drawable_get_gl_config(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_get_gl_config", "GdkGLDrawable*");
  return(C_TO_XEN_GdkGLConfig_(gdk_gl_drawable_get_gl_config(XEN_TO_C_GdkGLDrawable_(gldrawable))));
}

static XEN gxg_gdk_gl_drawable_get_size(XEN gldrawable, XEN width, XEN height)
{
  #define H_gdk_gl_drawable_get_size "void gdk_gl_drawable_get_size(GdkGLDrawable* gldrawable, gint* [width], \
gint* [height])"
  gint ref_width[1];
  gint ref_height[1];
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_get_size", "GdkGLDrawable*");
  gdk_gl_drawable_get_size(XEN_TO_C_GdkGLDrawable_(gldrawable), ref_width, ref_height);
  return(XEN_LIST_2(C_TO_XEN_gint(ref_width[0]), C_TO_XEN_gint(ref_height[0])));
}

static XEN gxg_gdk_gl_pixmap_new(XEN glconfig, XEN pixmap, XEN attrib_list)
{
  #define H_gdk_gl_pixmap_new "GdkGLPixmap* gdk_gl_pixmap_new(GdkGLConfig* glconfig, GdkPixmap* pixmap, \
int* attrib_list)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_pixmap_new", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_GdkPixmap__P(pixmap), pixmap, 2, "gdk_gl_pixmap_new", "GdkPixmap*");
  XEN_ASSERT_TYPE(XEN_int__P(attrib_list), attrib_list, 3, "gdk_gl_pixmap_new", "int*");
  return(C_TO_XEN_GdkGLPixmap_(gdk_gl_pixmap_new(XEN_TO_C_GdkGLConfig_(glconfig), XEN_TO_C_GdkPixmap_(pixmap), XEN_TO_C_int_(attrib_list))));
}

static XEN gxg_gdk_gl_pixmap_get_pixmap(XEN glpixmap)
{
  #define H_gdk_gl_pixmap_get_pixmap "GdkPixmap* gdk_gl_pixmap_get_pixmap(GdkGLPixmap* glpixmap)"
  XEN_ASSERT_TYPE(XEN_GdkGLPixmap__P(glpixmap), glpixmap, 1, "gdk_gl_pixmap_get_pixmap", "GdkGLPixmap*");
  return(C_TO_XEN_GdkPixmap_(gdk_gl_pixmap_get_pixmap(XEN_TO_C_GdkGLPixmap_(glpixmap))));
}

static XEN gxg_gdk_pixmap_set_gl_capability(XEN pixmap, XEN glconfig, XEN attrib_list)
{
  #define H_gdk_pixmap_set_gl_capability "GdkGLPixmap* gdk_pixmap_set_gl_capability(GdkPixmap* pixmap, \
GdkGLConfig* glconfig, int* attrib_list)"
  XEN_ASSERT_TYPE(XEN_GdkPixmap__P(pixmap), pixmap, 1, "gdk_pixmap_set_gl_capability", "GdkPixmap*");
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 2, "gdk_pixmap_set_gl_capability", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_int__P(attrib_list), attrib_list, 3, "gdk_pixmap_set_gl_capability", "int*");
  return(C_TO_XEN_GdkGLPixmap_(gdk_pixmap_set_gl_capability(XEN_TO_C_GdkPixmap_(pixmap), XEN_TO_C_GdkGLConfig_(glconfig), 
                                                            XEN_TO_C_int_(attrib_list))));
}

static XEN gxg_gdk_pixmap_unset_gl_capability(XEN pixmap)
{
  #define H_gdk_pixmap_unset_gl_capability "void gdk_pixmap_unset_gl_capability(GdkPixmap* pixmap)"
  XEN_ASSERT_TYPE(XEN_GdkPixmap__P(pixmap), pixmap, 1, "gdk_pixmap_unset_gl_capability", "GdkPixmap*");
  gdk_pixmap_unset_gl_capability(XEN_TO_C_GdkPixmap_(pixmap));
  return(XEN_FALSE);
}

static XEN gxg_gdk_pixmap_is_gl_capable(XEN pixmap)
{
  #define H_gdk_pixmap_is_gl_capable "gboolean gdk_pixmap_is_gl_capable(GdkPixmap* pixmap)"
  XEN_ASSERT_TYPE(XEN_GdkPixmap__P(pixmap), pixmap, 1, "gdk_pixmap_is_gl_capable", "GdkPixmap*");
  return(C_TO_XEN_gboolean(gdk_pixmap_is_gl_capable(XEN_TO_C_GdkPixmap_(pixmap))));
}

static XEN gxg_gdk_pixmap_get_gl_pixmap(XEN pixmap)
{
  #define H_gdk_pixmap_get_gl_pixmap "GdkGLPixmap* gdk_pixmap_get_gl_pixmap(GdkPixmap* pixmap)"
  XEN_ASSERT_TYPE(XEN_GdkPixmap__P(pixmap), pixmap, 1, "gdk_pixmap_get_gl_pixmap", "GdkPixmap*");
  return(C_TO_XEN_GdkGLPixmap_(gdk_pixmap_get_gl_pixmap(XEN_TO_C_GdkPixmap_(pixmap))));
}

static XEN gxg_gdk_gl_window_new(XEN glconfig, XEN window, XEN attrib_list)
{
  #define H_gdk_gl_window_new "GdkGLWindow* gdk_gl_window_new(GdkGLConfig* glconfig, GdkWindow* window, \
int* attrib_list)"
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 1, "gdk_gl_window_new", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_GdkWindow__P(window), window, 2, "gdk_gl_window_new", "GdkWindow*");
  XEN_ASSERT_TYPE(XEN_int__P(attrib_list), attrib_list, 3, "gdk_gl_window_new", "int*");
  return(C_TO_XEN_GdkGLWindow_(gdk_gl_window_new(XEN_TO_C_GdkGLConfig_(glconfig), XEN_TO_C_GdkWindow_(window), XEN_TO_C_int_(attrib_list))));
}

static XEN gxg_gdk_gl_window_get_window(XEN glwindow)
{
  #define H_gdk_gl_window_get_window "GdkWindow* gdk_gl_window_get_window(GdkGLWindow* glwindow)"
  XEN_ASSERT_TYPE(XEN_GdkGLWindow__P(glwindow), glwindow, 1, "gdk_gl_window_get_window", "GdkGLWindow*");
  return(C_TO_XEN_GdkWindow_(gdk_gl_window_get_window(XEN_TO_C_GdkGLWindow_(glwindow))));
}

static XEN gxg_gdk_window_set_gl_capability(XEN window, XEN glconfig, XEN attrib_list)
{
  #define H_gdk_window_set_gl_capability "GdkGLWindow* gdk_window_set_gl_capability(GdkWindow* window, \
GdkGLConfig* glconfig, int* attrib_list)"
  XEN_ASSERT_TYPE(XEN_GdkWindow__P(window), window, 1, "gdk_window_set_gl_capability", "GdkWindow*");
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 2, "gdk_window_set_gl_capability", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_int__P(attrib_list), attrib_list, 3, "gdk_window_set_gl_capability", "int*");
  return(C_TO_XEN_GdkGLWindow_(gdk_window_set_gl_capability(XEN_TO_C_GdkWindow_(window), XEN_TO_C_GdkGLConfig_(glconfig), 
                                                            XEN_TO_C_int_(attrib_list))));
}

static XEN gxg_gdk_window_unset_gl_capability(XEN window)
{
  #define H_gdk_window_unset_gl_capability "void gdk_window_unset_gl_capability(GdkWindow* window)"
  XEN_ASSERT_TYPE(XEN_GdkWindow__P(window), window, 1, "gdk_window_unset_gl_capability", "GdkWindow*");
  gdk_window_unset_gl_capability(XEN_TO_C_GdkWindow_(window));
  return(XEN_FALSE);
}

static XEN gxg_gdk_window_is_gl_capable(XEN window)
{
  #define H_gdk_window_is_gl_capable "gboolean gdk_window_is_gl_capable(GdkWindow* window)"
  XEN_ASSERT_TYPE(XEN_GdkWindow__P(window), window, 1, "gdk_window_is_gl_capable", "GdkWindow*");
  return(C_TO_XEN_gboolean(gdk_window_is_gl_capable(XEN_TO_C_GdkWindow_(window))));
}

static XEN gxg_gdk_window_get_gl_window(XEN window)
{
  #define H_gdk_window_get_gl_window "GdkGLWindow* gdk_window_get_gl_window(GdkWindow* window)"
  XEN_ASSERT_TYPE(XEN_GdkWindow__P(window), window, 1, "gdk_window_get_gl_window", "GdkWindow*");
  return(C_TO_XEN_GdkGLWindow_(gdk_window_get_gl_window(XEN_TO_C_GdkWindow_(window))));
}

static XEN gxg_gdk_gl_font_use_pango_font(XEN font_desc, XEN first, XEN count, XEN list_base)
{
  #define H_gdk_gl_font_use_pango_font "PangoFont* gdk_gl_font_use_pango_font(PangoFontDescription* font_desc, \
int first, int count, int list_base)"
  XEN_ASSERT_TYPE(XEN_PangoFontDescription__P(font_desc), font_desc, 1, "gdk_gl_font_use_pango_font", "PangoFontDescription*");
  XEN_ASSERT_TYPE(XEN_int_P(first), first, 2, "gdk_gl_font_use_pango_font", "int");
  XEN_ASSERT_TYPE(XEN_int_P(count), count, 3, "gdk_gl_font_use_pango_font", "int");
  XEN_ASSERT_TYPE(XEN_int_P(list_base), list_base, 4, "gdk_gl_font_use_pango_font", "int");
  return(C_TO_XEN_PangoFont_(gdk_gl_font_use_pango_font(XEN_TO_C_PangoFontDescription_(font_desc), XEN_TO_C_int(first), XEN_TO_C_int(count), 
                                                        XEN_TO_C_int(list_base))));
}

static XEN gxg_gtk_widget_set_gl_capability(XEN widget, XEN glconfig, XEN share_list, XEN direct, XEN render_type)
{
  #define H_gtk_widget_set_gl_capability "gboolean gtk_widget_set_gl_capability(GtkWidget* widget, GdkGLConfig* glconfig, \
GdkGLContext* share_list, gboolean direct, int render_type)"
  XEN_ASSERT_TYPE(XEN_GtkWidget__P(widget), widget, 1, "gtk_widget_set_gl_capability", "GtkWidget*");
  XEN_ASSERT_TYPE(XEN_GdkGLConfig__P(glconfig), glconfig, 2, "gtk_widget_set_gl_capability", "GdkGLConfig*");
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(share_list), share_list, 3, "gtk_widget_set_gl_capability", "GdkGLContext*");
  XEN_ASSERT_TYPE(XEN_gboolean_P(direct), direct, 4, "gtk_widget_set_gl_capability", "gboolean");
  XEN_ASSERT_TYPE(XEN_int_P(render_type), render_type, 5, "gtk_widget_set_gl_capability", "int");
  return(C_TO_XEN_gboolean(gtk_widget_set_gl_capability(XEN_TO_C_GtkWidget_(widget), XEN_TO_C_GdkGLConfig_(glconfig), XEN_TO_C_GdkGLContext_(share_list), 
                                                        XEN_TO_C_gboolean(direct), XEN_TO_C_int(render_type))));
}

static XEN gxg_gtk_widget_is_gl_capable(XEN widget)
{
  #define H_gtk_widget_is_gl_capable "gboolean gtk_widget_is_gl_capable(GtkWidget* widget)"
  XEN_ASSERT_TYPE(XEN_GtkWidget__P(widget), widget, 1, "gtk_widget_is_gl_capable", "GtkWidget*");
  return(C_TO_XEN_gboolean(gtk_widget_is_gl_capable(XEN_TO_C_GtkWidget_(widget))));
}

static XEN gxg_gtk_widget_get_gl_config(XEN widget)
{
  #define H_gtk_widget_get_gl_config "GdkGLConfig* gtk_widget_get_gl_config(GtkWidget* widget)"
  XEN_ASSERT_TYPE(XEN_GtkWidget__P(widget), widget, 1, "gtk_widget_get_gl_config", "GtkWidget*");
  return(C_TO_XEN_GdkGLConfig_(gtk_widget_get_gl_config(XEN_TO_C_GtkWidget_(widget))));
}

static XEN gxg_gtk_widget_get_gl_context(XEN widget)
{
  #define H_gtk_widget_get_gl_context "GdkGLContext* gtk_widget_get_gl_context(GtkWidget* widget)"
  XEN_ASSERT_TYPE(XEN_GtkWidget__P(widget), widget, 1, "gtk_widget_get_gl_context", "GtkWidget*");
  return(C_TO_XEN_GdkGLContext_(gtk_widget_get_gl_context(XEN_TO_C_GtkWidget_(widget))));
}

static XEN gxg_gtk_widget_get_gl_window(XEN widget)
{
  #define H_gtk_widget_get_gl_window "GdkGLWindow* gtk_widget_get_gl_window(GtkWidget* widget)"
  XEN_ASSERT_TYPE(XEN_GtkWidget__P(widget), widget, 1, "gtk_widget_get_gl_window", "GtkWidget*");
  return(C_TO_XEN_GdkGLWindow_(gtk_widget_get_gl_window(XEN_TO_C_GtkWidget_(widget))));
}

#ifdef GTKGLEXT_MAJOR_VERSION
static XEN gxg_gdk_gl_drawable_gl_begin(XEN gldrawable, XEN glcontext)
{
  #define H_gdk_gl_drawable_gl_begin "gboolean gdk_gl_drawable_gl_begin(GdkGLDrawable* gldrawable, GdkGLContext* glcontext)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_gl_begin", "GdkGLDrawable*");
  XEN_ASSERT_TYPE(XEN_GdkGLContext__P(glcontext), glcontext, 2, "gdk_gl_drawable_gl_begin", "GdkGLContext*");
  return(C_TO_XEN_gboolean(gdk_gl_drawable_gl_begin(XEN_TO_C_GdkGLDrawable_(gldrawable), XEN_TO_C_GdkGLContext_(glcontext))));
}

static XEN gxg_gdk_gl_drawable_gl_end(XEN gldrawable)
{
  #define H_gdk_gl_drawable_gl_end "void gdk_gl_drawable_gl_end(GdkGLDrawable* gldrawable)"
  XEN_ASSERT_TYPE(XEN_GdkGLDrawable__P(gldrawable), gldrawable, 1, "gdk_gl_drawable_gl_end", "GdkGLDrawable*");
  gdk_gl_drawable_gl_end(XEN_TO_C_GdkGLDrawable_(gldrawable));
  return(XEN_FALSE);
}

#endif
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
  GLdouble ref_equation[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(plane), plane, 1, "glGetClipPlane", "GLenum");
  glGetClipPlane(XEN_TO_C_GLenum(plane), ref_equation);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_equation[0])));
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
  GLboolean ref_params[16];
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetBooleanv", "GLenum");
  glGetBooleanv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = XEN_EMPTY_LIST;
    for (i = 0; i < vals; i++)
      result = XEN_CONS(C_TO_XEN_GLboolean(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetDoublev(XEN pname, XEN params)
{
  #define H_glGetDoublev "void glGetDoublev(GLenum pname, GLdouble* [params])"
  GLdouble ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetDoublev", "GLenum");
  glGetDoublev(XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_params[0])));
}

static XEN gxg_glGetFloatv(XEN pname, XEN params)
{
  #define H_glGetFloatv "void glGetFloatv(GLenum pname, GLfloat* [params])"
  GLfloat ref_params[16];
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetFloatv", "GLenum");
  glGetFloatv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = XEN_EMPTY_LIST;
    for (i = 0; i < vals; i++)
      result = XEN_CONS(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetIntegerv(XEN pname, XEN params)
{
  #define H_glGetIntegerv "void glGetIntegerv(GLenum pname, GLint* [params])"
  GLint ref_params[16];
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetIntegerv", "GLenum");
  glGetIntegerv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = XEN_EMPTY_LIST;
    for (i = 0; i < vals; i++)
      result = XEN_CONS(C_TO_XEN_GLint(ref_params[i]), result);
    return(result);
  }
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
  void* ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 1, "glGetPointerv", "GLenum");
  glGetPointerv(XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_void_(ref_params[0])));
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
  GLfloat ref_params[16];
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glGetLightfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetLightfv", "GLenum");
  glGetLightfv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = XEN_EMPTY_LIST;
    for (i = 0; i < vals; i++)
      result = XEN_CONS(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetLightiv(XEN light, XEN pname, XEN params)
{
  #define H_glGetLightiv "void glGetLightiv(GLenum light, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(light), light, 1, "glGetLightiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetLightiv", "GLenum");
  glGetLightiv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_params[16];
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glGetMaterialfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMaterialfv", "GLenum");
  glGetMaterialfv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = XEN_EMPTY_LIST;
    for (i = 0; i < vals; i++)
      result = XEN_CONS(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetMaterialiv(XEN face, XEN pname, XEN params)
{
  #define H_glGetMaterialiv "void glGetMaterialiv(GLenum face, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(face), face, 1, "glGetMaterialiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMaterialiv", "GLenum");
  glGetMaterialiv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_values[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapfv", "GLenum");
  glGetPixelMapfv(XEN_TO_C_GLenum(map), ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_values[0])));
}

static XEN gxg_glGetPixelMapuiv(XEN map, XEN values)
{
  #define H_glGetPixelMapuiv "void glGetPixelMapuiv(GLenum map, GLuint* [values])"
  GLuint ref_values[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapuiv", "GLenum");
  glGetPixelMapuiv(XEN_TO_C_GLenum(map), ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLuint(ref_values[0])));
}

static XEN gxg_glGetPixelMapusv(XEN map, XEN values)
{
  #define H_glGetPixelMapusv "void glGetPixelMapusv(GLenum map, GLushort* [values])"
  GLushort ref_values[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(map), map, 1, "glGetPixelMapusv", "GLenum");
  glGetPixelMapusv(XEN_TO_C_GLenum(map), ref_values);
  return(XEN_LIST_1(C_TO_XEN_GLushort(ref_values[0])));
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
  GLdouble ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGendv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGendv", "GLenum");
  glGetTexGendv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_params[0])));
}

static XEN gxg_glGetTexGenfv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGenfv "void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGenfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGenfv", "GLenum");
  glGetTexGenfv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexGeniv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGeniv "void glGetTexGeniv(GLenum coord, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(coord), coord, 1, "glGetTexGeniv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexGeniv", "GLenum");
  glGetTexGeniv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexEnvfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexEnvfv", "GLenum");
  glGetTexEnvfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexEnviv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexEnviv "void glGetTexEnviv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexEnviv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexEnviv", "GLenum");
  glGetTexEnviv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexParameterfv", "GLenum");
  glGetTexParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexParameteriv "void glGetTexParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetTexParameteriv", "GLenum");
  glGetTexParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glGetTexLevelParameterfv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameterfv "void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexLevelParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glGetTexLevelParameterfv", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 3, "glGetTexLevelParameterfv", "GLenum");
  glGetTexLevelParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexLevelParameteriv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameteriv "void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, \
GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetTexLevelParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLint_P(level), level, 2, "glGetTexLevelParameteriv", "GLint");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 3, "glGetTexLevelParameteriv", "GLenum");
  glGetTexLevelParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  return(xen_return_first(XEN_FALSE, arglist));
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
  return(xen_return_first(XEN_FALSE, arglist));
}

static XEN gxg_glGetMapdv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapdv "void glGetMapdv(GLenum target, GLenum query, GLdouble* [v])"
  GLdouble ref_v[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapdv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapdv", "GLenum");
  glGetMapdv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLdouble(ref_v[0])));
}

static XEN gxg_glGetMapfv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapfv "void glGetMapfv(GLenum target, GLenum query, GLfloat* [v])"
  GLfloat ref_v[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapfv", "GLenum");
  glGetMapfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_v[0])));
}

static XEN gxg_glGetMapiv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapiv "void glGetMapiv(GLenum target, GLenum query, GLint* [v])"
  GLint ref_v[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMapiv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(query), query, 2, "glGetMapiv", "GLenum");
  glGetMapiv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_v[0])));
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
  return(xen_return_first(XEN_FALSE, arglist));
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
  return(xen_return_first(XEN_FALSE, arglist));
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
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameterfv", "GLenum");
  glGetColorTableParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetColorTableParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameteriv "void glGetColorTableParameteriv(GLenum target, GLenum pname, \
GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetColorTableParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetColorTableParameteriv", "GLenum");
  glGetColorTableParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetHistogramParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetHistogramParameterfv", "GLenum");
  glGetHistogramParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetHistogramParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetHistogramParameteriv "void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetHistogramParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetHistogramParameteriv", "GLenum");
  glGetHistogramParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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
  GLfloat ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMinmaxParameterfv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMinmaxParameterfv", "GLenum");
  glGetMinmaxParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetMinmaxParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetMinmaxParameteriv "void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  XEN_ASSERT_TYPE(XEN_GLenum_P(target), target, 1, "glGetMinmaxParameteriv", "GLenum");
  XEN_ASSERT_TYPE(XEN_GLenum_P(pname), pname, 2, "glGetMinmaxParameteriv", "GLenum");
  glGetMinmaxParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(XEN_LIST_1(C_TO_XEN_GLint(ref_params[0])));
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

#if HAVE_GLU
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

#endif
#ifdef XEN_ARGIFY_1
#if USE_MOTIF
XEN_NARGIFY_3(gxg_glXChooseVisual_w, gxg_glXChooseVisual)
XEN_NARGIFY_4(gxg_glXCopyContext_w, gxg_glXCopyContext)
XEN_NARGIFY_4(gxg_glXCreateContext_w, gxg_glXCreateContext)
XEN_NARGIFY_3(gxg_glXCreateGLXPixmap_w, gxg_glXCreateGLXPixmap)
XEN_NARGIFY_2(gxg_glXDestroyContext_w, gxg_glXDestroyContext)
XEN_NARGIFY_2(gxg_glXDestroyGLXPixmap_w, gxg_glXDestroyGLXPixmap)
XEN_ARGIFY_4(gxg_glXGetConfig_w, gxg_glXGetConfig)
XEN_NARGIFY_0(gxg_glXGetCurrentContext_w, gxg_glXGetCurrentContext)
XEN_NARGIFY_0(gxg_glXGetCurrentDrawable_w, gxg_glXGetCurrentDrawable)
XEN_NARGIFY_2(gxg_glXIsDirect_w, gxg_glXIsDirect)
XEN_NARGIFY_3(gxg_glXMakeCurrent_w, gxg_glXMakeCurrent)
XEN_ARGIFY_3(gxg_glXQueryExtension_w, gxg_glXQueryExtension)
XEN_ARGIFY_3(gxg_glXQueryVersion_w, gxg_glXQueryVersion)
XEN_NARGIFY_2(gxg_glXSwapBuffers_w, gxg_glXSwapBuffers)
XEN_NARGIFY_4(gxg_glXUseXFont_w, gxg_glXUseXFont)
XEN_NARGIFY_0(gxg_glXWaitGL_w, gxg_glXWaitGL)
XEN_NARGIFY_0(gxg_glXWaitX_w, gxg_glXWaitX)
XEN_NARGIFY_2(gxg_glXGetClientString_w, gxg_glXGetClientString)
XEN_NARGIFY_3(gxg_glXQueryServerString_w, gxg_glXQueryServerString)
XEN_NARGIFY_2(gxg_glXQueryExtensionsString_w, gxg_glXQueryExtensionsString)
#endif
#if USE_GTK
XEN_NARGIFY_0(gxg_gdk_gl_query_extension_w, gxg_gdk_gl_query_extension)
XEN_NARGIFY_2(gxg_gdk_gl_query_version_w, gxg_gdk_gl_query_version)
XEN_NARGIFY_1(gxg_gdk_gl_query_gl_extension_w, gxg_gdk_gl_query_gl_extension)
XEN_NARGIFY_1(gxg_gdk_gl_config_new_w, gxg_gdk_gl_config_new)
XEN_NARGIFY_1(gxg_gdk_gl_config_new_by_mode_w, gxg_gdk_gl_config_new_by_mode)
XEN_ARGIFY_3(gxg_gdk_gl_config_get_attrib_w, gxg_gdk_gl_config_get_attrib)
XEN_NARGIFY_1(gxg_gdk_gl_config_get_colormap_w, gxg_gdk_gl_config_get_colormap)
XEN_NARGIFY_1(gxg_gdk_gl_config_get_visual_w, gxg_gdk_gl_config_get_visual)
XEN_NARGIFY_1(gxg_gdk_gl_config_get_depth_w, gxg_gdk_gl_config_get_depth)
XEN_NARGIFY_1(gxg_gdk_gl_config_is_rgba_w, gxg_gdk_gl_config_is_rgba)
XEN_NARGIFY_1(gxg_gdk_gl_config_is_double_buffered_w, gxg_gdk_gl_config_is_double_buffered)
XEN_NARGIFY_1(gxg_gdk_gl_config_is_stereo_w, gxg_gdk_gl_config_is_stereo)
XEN_NARGIFY_1(gxg_gdk_gl_config_has_alpha_w, gxg_gdk_gl_config_has_alpha)
XEN_NARGIFY_1(gxg_gdk_gl_config_has_depth_buffer_w, gxg_gdk_gl_config_has_depth_buffer)
XEN_NARGIFY_1(gxg_gdk_gl_config_has_stencil_buffer_w, gxg_gdk_gl_config_has_stencil_buffer)
XEN_NARGIFY_1(gxg_gdk_gl_config_has_accum_buffer_w, gxg_gdk_gl_config_has_accum_buffer)
XEN_NARGIFY_1(gxg_gdk_gl_context_get_gl_drawable_w, gxg_gdk_gl_context_get_gl_drawable)
XEN_NARGIFY_1(gxg_gdk_gl_context_get_gl_config_w, gxg_gdk_gl_context_get_gl_config)
XEN_NARGIFY_1(gxg_gdk_gl_context_get_share_list_w, gxg_gdk_gl_context_get_share_list)
XEN_NARGIFY_1(gxg_gdk_gl_context_is_direct_w, gxg_gdk_gl_context_is_direct)
XEN_NARGIFY_1(gxg_gdk_gl_context_get_render_type_w, gxg_gdk_gl_context_get_render_type)
XEN_NARGIFY_2(gxg_gdk_gl_drawable_make_current_w, gxg_gdk_gl_drawable_make_current)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_is_double_buffered_w, gxg_gdk_gl_drawable_is_double_buffered)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_swap_buffers_w, gxg_gdk_gl_drawable_swap_buffers)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_wait_gl_w, gxg_gdk_gl_drawable_wait_gl)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_wait_gdk_w, gxg_gdk_gl_drawable_wait_gdk)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_get_gl_config_w, gxg_gdk_gl_drawable_get_gl_config)
XEN_ARGIFY_3(gxg_gdk_gl_drawable_get_size_w, gxg_gdk_gl_drawable_get_size)
XEN_NARGIFY_3(gxg_gdk_gl_pixmap_new_w, gxg_gdk_gl_pixmap_new)
XEN_NARGIFY_1(gxg_gdk_gl_pixmap_get_pixmap_w, gxg_gdk_gl_pixmap_get_pixmap)
XEN_NARGIFY_3(gxg_gdk_pixmap_set_gl_capability_w, gxg_gdk_pixmap_set_gl_capability)
XEN_NARGIFY_1(gxg_gdk_pixmap_unset_gl_capability_w, gxg_gdk_pixmap_unset_gl_capability)
XEN_NARGIFY_1(gxg_gdk_pixmap_is_gl_capable_w, gxg_gdk_pixmap_is_gl_capable)
XEN_NARGIFY_1(gxg_gdk_pixmap_get_gl_pixmap_w, gxg_gdk_pixmap_get_gl_pixmap)
XEN_NARGIFY_3(gxg_gdk_gl_window_new_w, gxg_gdk_gl_window_new)
XEN_NARGIFY_1(gxg_gdk_gl_window_get_window_w, gxg_gdk_gl_window_get_window)
XEN_NARGIFY_3(gxg_gdk_window_set_gl_capability_w, gxg_gdk_window_set_gl_capability)
XEN_NARGIFY_1(gxg_gdk_window_unset_gl_capability_w, gxg_gdk_window_unset_gl_capability)
XEN_NARGIFY_1(gxg_gdk_window_is_gl_capable_w, gxg_gdk_window_is_gl_capable)
XEN_NARGIFY_1(gxg_gdk_window_get_gl_window_w, gxg_gdk_window_get_gl_window)
XEN_NARGIFY_4(gxg_gdk_gl_font_use_pango_font_w, gxg_gdk_gl_font_use_pango_font)
XEN_NARGIFY_5(gxg_gtk_widget_set_gl_capability_w, gxg_gtk_widget_set_gl_capability)
XEN_NARGIFY_1(gxg_gtk_widget_is_gl_capable_w, gxg_gtk_widget_is_gl_capable)
XEN_NARGIFY_1(gxg_gtk_widget_get_gl_config_w, gxg_gtk_widget_get_gl_config)
XEN_NARGIFY_1(gxg_gtk_widget_get_gl_context_w, gxg_gtk_widget_get_gl_context)
XEN_NARGIFY_1(gxg_gtk_widget_get_gl_window_w, gxg_gtk_widget_get_gl_window)
#ifdef GTKGLEXT_MAJOR_VERSION
XEN_NARGIFY_2(gxg_gdk_gl_drawable_gl_begin_w, gxg_gdk_gl_drawable_gl_begin)
XEN_NARGIFY_1(gxg_gdk_gl_drawable_gl_end_w, gxg_gdk_gl_drawable_gl_end)
#endif
#endif

XEN_NARGIFY_1(gxg_glClearIndex_w, gxg_glClearIndex)
XEN_NARGIFY_4(gxg_glClearColor_w, gxg_glClearColor)
XEN_NARGIFY_1(gxg_glClear_w, gxg_glClear)
XEN_NARGIFY_1(gxg_glIndexMask_w, gxg_glIndexMask)
XEN_NARGIFY_4(gxg_glColorMask_w, gxg_glColorMask)
XEN_NARGIFY_2(gxg_glAlphaFunc_w, gxg_glAlphaFunc)
XEN_NARGIFY_2(gxg_glBlendFunc_w, gxg_glBlendFunc)
XEN_NARGIFY_1(gxg_glLogicOp_w, gxg_glLogicOp)
XEN_NARGIFY_1(gxg_glCullFace_w, gxg_glCullFace)
XEN_NARGIFY_1(gxg_glFrontFace_w, gxg_glFrontFace)
XEN_NARGIFY_1(gxg_glPointSize_w, gxg_glPointSize)
XEN_NARGIFY_1(gxg_glLineWidth_w, gxg_glLineWidth)
XEN_NARGIFY_2(gxg_glLineStipple_w, gxg_glLineStipple)
XEN_NARGIFY_2(gxg_glPolygonMode_w, gxg_glPolygonMode)
XEN_NARGIFY_2(gxg_glPolygonOffset_w, gxg_glPolygonOffset)
XEN_NARGIFY_1(gxg_glPolygonStipple_w, gxg_glPolygonStipple)
XEN_NARGIFY_1(gxg_glEdgeFlag_w, gxg_glEdgeFlag)
XEN_NARGIFY_4(gxg_glScissor_w, gxg_glScissor)
XEN_NARGIFY_2(gxg_glClipPlane_w, gxg_glClipPlane)
XEN_ARGIFY_2(gxg_glGetClipPlane_w, gxg_glGetClipPlane)
XEN_NARGIFY_1(gxg_glDrawBuffer_w, gxg_glDrawBuffer)
XEN_NARGIFY_1(gxg_glReadBuffer_w, gxg_glReadBuffer)
XEN_NARGIFY_1(gxg_glEnable_w, gxg_glEnable)
XEN_NARGIFY_1(gxg_glDisable_w, gxg_glDisable)
XEN_NARGIFY_1(gxg_glIsEnabled_w, gxg_glIsEnabled)
XEN_NARGIFY_1(gxg_glEnableClientState_w, gxg_glEnableClientState)
XEN_NARGIFY_1(gxg_glDisableClientState_w, gxg_glDisableClientState)
XEN_ARGIFY_2(gxg_glGetBooleanv_w, gxg_glGetBooleanv)
XEN_ARGIFY_2(gxg_glGetDoublev_w, gxg_glGetDoublev)
XEN_ARGIFY_2(gxg_glGetFloatv_w, gxg_glGetFloatv)
XEN_ARGIFY_2(gxg_glGetIntegerv_w, gxg_glGetIntegerv)
XEN_NARGIFY_1(gxg_glPushAttrib_w, gxg_glPushAttrib)
XEN_NARGIFY_0(gxg_glPopAttrib_w, gxg_glPopAttrib)
XEN_NARGIFY_1(gxg_glPushClientAttrib_w, gxg_glPushClientAttrib)
XEN_NARGIFY_0(gxg_glPopClientAttrib_w, gxg_glPopClientAttrib)
XEN_NARGIFY_1(gxg_glRenderMode_w, gxg_glRenderMode)
XEN_NARGIFY_0(gxg_glGetError_w, gxg_glGetError)
XEN_NARGIFY_1(gxg_glGetString_w, gxg_glGetString)
XEN_NARGIFY_0(gxg_glFinish_w, gxg_glFinish)
XEN_NARGIFY_0(gxg_glFlush_w, gxg_glFlush)
XEN_NARGIFY_2(gxg_glHint_w, gxg_glHint)
XEN_NARGIFY_1(gxg_glClearDepth_w, gxg_glClearDepth)
XEN_NARGIFY_1(gxg_glDepthFunc_w, gxg_glDepthFunc)
XEN_NARGIFY_1(gxg_glDepthMask_w, gxg_glDepthMask)
XEN_NARGIFY_2(gxg_glDepthRange_w, gxg_glDepthRange)
XEN_NARGIFY_4(gxg_glClearAccum_w, gxg_glClearAccum)
XEN_NARGIFY_2(gxg_glAccum_w, gxg_glAccum)
XEN_NARGIFY_1(gxg_glMatrixMode_w, gxg_glMatrixMode)
XEN_NARGIFY_6(gxg_glOrtho_w, gxg_glOrtho)
XEN_NARGIFY_6(gxg_glFrustum_w, gxg_glFrustum)
XEN_NARGIFY_4(gxg_glViewport_w, gxg_glViewport)
XEN_NARGIFY_0(gxg_glPushMatrix_w, gxg_glPushMatrix)
XEN_NARGIFY_0(gxg_glPopMatrix_w, gxg_glPopMatrix)
XEN_NARGIFY_0(gxg_glLoadIdentity_w, gxg_glLoadIdentity)
XEN_NARGIFY_1(gxg_glLoadMatrixd_w, gxg_glLoadMatrixd)
XEN_NARGIFY_1(gxg_glLoadMatrixf_w, gxg_glLoadMatrixf)
XEN_NARGIFY_1(gxg_glMultMatrixd_w, gxg_glMultMatrixd)
XEN_NARGIFY_1(gxg_glMultMatrixf_w, gxg_glMultMatrixf)
XEN_NARGIFY_4(gxg_glRotated_w, gxg_glRotated)
XEN_NARGIFY_4(gxg_glRotatef_w, gxg_glRotatef)
XEN_NARGIFY_3(gxg_glScaled_w, gxg_glScaled)
XEN_NARGIFY_3(gxg_glScalef_w, gxg_glScalef)
XEN_NARGIFY_3(gxg_glTranslated_w, gxg_glTranslated)
XEN_NARGIFY_3(gxg_glTranslatef_w, gxg_glTranslatef)
XEN_NARGIFY_1(gxg_glIsList_w, gxg_glIsList)
XEN_NARGIFY_2(gxg_glDeleteLists_w, gxg_glDeleteLists)
XEN_NARGIFY_1(gxg_glGenLists_w, gxg_glGenLists)
XEN_NARGIFY_2(gxg_glNewList_w, gxg_glNewList)
XEN_NARGIFY_0(gxg_glEndList_w, gxg_glEndList)
XEN_NARGIFY_1(gxg_glCallList_w, gxg_glCallList)
XEN_NARGIFY_3(gxg_glCallLists_w, gxg_glCallLists)
XEN_NARGIFY_1(gxg_glListBase_w, gxg_glListBase)
XEN_NARGIFY_1(gxg_glBegin_w, gxg_glBegin)
XEN_NARGIFY_0(gxg_glEnd_w, gxg_glEnd)
XEN_NARGIFY_2(gxg_glVertex2d_w, gxg_glVertex2d)
XEN_NARGIFY_2(gxg_glVertex2f_w, gxg_glVertex2f)
XEN_NARGIFY_2(gxg_glVertex2i_w, gxg_glVertex2i)
XEN_NARGIFY_2(gxg_glVertex2s_w, gxg_glVertex2s)
XEN_NARGIFY_3(gxg_glVertex3d_w, gxg_glVertex3d)
XEN_NARGIFY_3(gxg_glVertex3f_w, gxg_glVertex3f)
XEN_NARGIFY_3(gxg_glVertex3i_w, gxg_glVertex3i)
XEN_NARGIFY_3(gxg_glVertex3s_w, gxg_glVertex3s)
XEN_NARGIFY_4(gxg_glVertex4d_w, gxg_glVertex4d)
XEN_NARGIFY_4(gxg_glVertex4f_w, gxg_glVertex4f)
XEN_NARGIFY_4(gxg_glVertex4i_w, gxg_glVertex4i)
XEN_NARGIFY_4(gxg_glVertex4s_w, gxg_glVertex4s)
XEN_NARGIFY_3(gxg_glNormal3b_w, gxg_glNormal3b)
XEN_NARGIFY_3(gxg_glNormal3d_w, gxg_glNormal3d)
XEN_NARGIFY_3(gxg_glNormal3f_w, gxg_glNormal3f)
XEN_NARGIFY_3(gxg_glNormal3i_w, gxg_glNormal3i)
XEN_NARGIFY_3(gxg_glNormal3s_w, gxg_glNormal3s)
XEN_NARGIFY_1(gxg_glIndexd_w, gxg_glIndexd)
XEN_NARGIFY_1(gxg_glIndexf_w, gxg_glIndexf)
XEN_NARGIFY_1(gxg_glIndexi_w, gxg_glIndexi)
XEN_NARGIFY_1(gxg_glIndexs_w, gxg_glIndexs)
XEN_NARGIFY_1(gxg_glIndexub_w, gxg_glIndexub)
XEN_NARGIFY_3(gxg_glColor3b_w, gxg_glColor3b)
XEN_NARGIFY_3(gxg_glColor3d_w, gxg_glColor3d)
XEN_NARGIFY_3(gxg_glColor3f_w, gxg_glColor3f)
XEN_NARGIFY_3(gxg_glColor3i_w, gxg_glColor3i)
XEN_NARGIFY_3(gxg_glColor3s_w, gxg_glColor3s)
XEN_NARGIFY_3(gxg_glColor3ub_w, gxg_glColor3ub)
XEN_NARGIFY_3(gxg_glColor3ui_w, gxg_glColor3ui)
XEN_NARGIFY_3(gxg_glColor3us_w, gxg_glColor3us)
XEN_NARGIFY_4(gxg_glColor4b_w, gxg_glColor4b)
XEN_NARGIFY_4(gxg_glColor4d_w, gxg_glColor4d)
XEN_NARGIFY_4(gxg_glColor4f_w, gxg_glColor4f)
XEN_NARGIFY_4(gxg_glColor4i_w, gxg_glColor4i)
XEN_NARGIFY_4(gxg_glColor4s_w, gxg_glColor4s)
XEN_NARGIFY_4(gxg_glColor4ub_w, gxg_glColor4ub)
XEN_NARGIFY_4(gxg_glColor4ui_w, gxg_glColor4ui)
XEN_NARGIFY_4(gxg_glColor4us_w, gxg_glColor4us)
XEN_NARGIFY_1(gxg_glTexCoord1d_w, gxg_glTexCoord1d)
XEN_NARGIFY_1(gxg_glTexCoord1f_w, gxg_glTexCoord1f)
XEN_NARGIFY_1(gxg_glTexCoord1i_w, gxg_glTexCoord1i)
XEN_NARGIFY_1(gxg_glTexCoord1s_w, gxg_glTexCoord1s)
XEN_NARGIFY_2(gxg_glTexCoord2d_w, gxg_glTexCoord2d)
XEN_NARGIFY_2(gxg_glTexCoord2f_w, gxg_glTexCoord2f)
XEN_NARGIFY_2(gxg_glTexCoord2i_w, gxg_glTexCoord2i)
XEN_NARGIFY_2(gxg_glTexCoord2s_w, gxg_glTexCoord2s)
XEN_NARGIFY_3(gxg_glTexCoord3d_w, gxg_glTexCoord3d)
XEN_NARGIFY_3(gxg_glTexCoord3f_w, gxg_glTexCoord3f)
XEN_NARGIFY_3(gxg_glTexCoord3i_w, gxg_glTexCoord3i)
XEN_NARGIFY_3(gxg_glTexCoord3s_w, gxg_glTexCoord3s)
XEN_NARGIFY_4(gxg_glTexCoord4d_w, gxg_glTexCoord4d)
XEN_NARGIFY_4(gxg_glTexCoord4f_w, gxg_glTexCoord4f)
XEN_NARGIFY_4(gxg_glTexCoord4i_w, gxg_glTexCoord4i)
XEN_NARGIFY_4(gxg_glTexCoord4s_w, gxg_glTexCoord4s)
XEN_NARGIFY_2(gxg_glRasterPos2d_w, gxg_glRasterPos2d)
XEN_NARGIFY_2(gxg_glRasterPos2f_w, gxg_glRasterPos2f)
XEN_NARGIFY_2(gxg_glRasterPos2i_w, gxg_glRasterPos2i)
XEN_NARGIFY_2(gxg_glRasterPos2s_w, gxg_glRasterPos2s)
XEN_NARGIFY_3(gxg_glRasterPos3d_w, gxg_glRasterPos3d)
XEN_NARGIFY_3(gxg_glRasterPos3f_w, gxg_glRasterPos3f)
XEN_NARGIFY_3(gxg_glRasterPos3i_w, gxg_glRasterPos3i)
XEN_NARGIFY_3(gxg_glRasterPos3s_w, gxg_glRasterPos3s)
XEN_NARGIFY_4(gxg_glRasterPos4d_w, gxg_glRasterPos4d)
XEN_NARGIFY_4(gxg_glRasterPos4f_w, gxg_glRasterPos4f)
XEN_NARGIFY_4(gxg_glRasterPos4i_w, gxg_glRasterPos4i)
XEN_NARGIFY_4(gxg_glRasterPos4s_w, gxg_glRasterPos4s)
XEN_NARGIFY_4(gxg_glRectd_w, gxg_glRectd)
XEN_NARGIFY_4(gxg_glRectf_w, gxg_glRectf)
XEN_NARGIFY_4(gxg_glRecti_w, gxg_glRecti)
XEN_NARGIFY_4(gxg_glRects_w, gxg_glRects)
XEN_NARGIFY_4(gxg_glVertexPointer_w, gxg_glVertexPointer)
XEN_NARGIFY_3(gxg_glNormalPointer_w, gxg_glNormalPointer)
XEN_NARGIFY_4(gxg_glColorPointer_w, gxg_glColorPointer)
XEN_NARGIFY_3(gxg_glIndexPointer_w, gxg_glIndexPointer)
XEN_NARGIFY_4(gxg_glTexCoordPointer_w, gxg_glTexCoordPointer)
XEN_NARGIFY_2(gxg_glEdgeFlagPointer_w, gxg_glEdgeFlagPointer)
XEN_ARGIFY_2(gxg_glGetPointerv_w, gxg_glGetPointerv)
XEN_NARGIFY_1(gxg_glArrayElement_w, gxg_glArrayElement)
XEN_NARGIFY_3(gxg_glDrawArrays_w, gxg_glDrawArrays)
XEN_NARGIFY_4(gxg_glDrawElements_w, gxg_glDrawElements)
XEN_NARGIFY_3(gxg_glInterleavedArrays_w, gxg_glInterleavedArrays)
XEN_NARGIFY_1(gxg_glShadeModel_w, gxg_glShadeModel)
XEN_NARGIFY_3(gxg_glLightf_w, gxg_glLightf)
XEN_NARGIFY_3(gxg_glLighti_w, gxg_glLighti)
XEN_ARGIFY_3(gxg_glGetLightfv_w, gxg_glGetLightfv)
XEN_ARGIFY_3(gxg_glGetLightiv_w, gxg_glGetLightiv)
XEN_NARGIFY_2(gxg_glLightModelf_w, gxg_glLightModelf)
XEN_NARGIFY_2(gxg_glLightModeli_w, gxg_glLightModeli)
XEN_NARGIFY_3(gxg_glMaterialf_w, gxg_glMaterialf)
XEN_NARGIFY_3(gxg_glMateriali_w, gxg_glMateriali)
XEN_ARGIFY_3(gxg_glGetMaterialfv_w, gxg_glGetMaterialfv)
XEN_ARGIFY_3(gxg_glGetMaterialiv_w, gxg_glGetMaterialiv)
XEN_NARGIFY_2(gxg_glColorMaterial_w, gxg_glColorMaterial)
XEN_NARGIFY_2(gxg_glPixelZoom_w, gxg_glPixelZoom)
XEN_NARGIFY_2(gxg_glPixelStoref_w, gxg_glPixelStoref)
XEN_NARGIFY_2(gxg_glPixelStorei_w, gxg_glPixelStorei)
XEN_NARGIFY_2(gxg_glPixelTransferf_w, gxg_glPixelTransferf)
XEN_NARGIFY_2(gxg_glPixelTransferi_w, gxg_glPixelTransferi)
XEN_ARGIFY_2(gxg_glGetPixelMapfv_w, gxg_glGetPixelMapfv)
XEN_ARGIFY_2(gxg_glGetPixelMapuiv_w, gxg_glGetPixelMapuiv)
XEN_ARGIFY_2(gxg_glGetPixelMapusv_w, gxg_glGetPixelMapusv)
XEN_NARGIFY_7(gxg_glBitmap_w, gxg_glBitmap)
XEN_NARGIFY_7(gxg_glReadPixels_w, gxg_glReadPixels)
XEN_NARGIFY_5(gxg_glDrawPixels_w, gxg_glDrawPixels)
XEN_NARGIFY_5(gxg_glCopyPixels_w, gxg_glCopyPixels)
XEN_NARGIFY_3(gxg_glStencilFunc_w, gxg_glStencilFunc)
XEN_NARGIFY_1(gxg_glStencilMask_w, gxg_glStencilMask)
XEN_NARGIFY_3(gxg_glStencilOp_w, gxg_glStencilOp)
XEN_NARGIFY_1(gxg_glClearStencil_w, gxg_glClearStencil)
XEN_NARGIFY_3(gxg_glTexGend_w, gxg_glTexGend)
XEN_NARGIFY_3(gxg_glTexGenf_w, gxg_glTexGenf)
XEN_NARGIFY_3(gxg_glTexGeni_w, gxg_glTexGeni)
XEN_ARGIFY_3(gxg_glGetTexGendv_w, gxg_glGetTexGendv)
XEN_ARGIFY_3(gxg_glGetTexGenfv_w, gxg_glGetTexGenfv)
XEN_ARGIFY_3(gxg_glGetTexGeniv_w, gxg_glGetTexGeniv)
XEN_NARGIFY_3(gxg_glTexEnvf_w, gxg_glTexEnvf)
XEN_NARGIFY_3(gxg_glTexEnvi_w, gxg_glTexEnvi)
XEN_ARGIFY_3(gxg_glGetTexEnvfv_w, gxg_glGetTexEnvfv)
XEN_ARGIFY_3(gxg_glGetTexEnviv_w, gxg_glGetTexEnviv)
XEN_NARGIFY_3(gxg_glTexParameterf_w, gxg_glTexParameterf)
XEN_NARGIFY_3(gxg_glTexParameteri_w, gxg_glTexParameteri)
XEN_ARGIFY_3(gxg_glGetTexParameterfv_w, gxg_glGetTexParameterfv)
XEN_ARGIFY_3(gxg_glGetTexParameteriv_w, gxg_glGetTexParameteriv)
XEN_ARGIFY_4(gxg_glGetTexLevelParameterfv_w, gxg_glGetTexLevelParameterfv)
XEN_ARGIFY_4(gxg_glGetTexLevelParameteriv_w, gxg_glGetTexLevelParameteriv)
XEN_NARGIFY_8(gxg_glTexImage1D_w, gxg_glTexImage1D)
XEN_NARGIFY_9(gxg_glTexImage2D_w, gxg_glTexImage2D)
XEN_NARGIFY_2(gxg_glGenTextures_w, gxg_glGenTextures)
XEN_NARGIFY_2(gxg_glDeleteTextures_w, gxg_glDeleteTextures)
XEN_NARGIFY_2(gxg_glBindTexture_w, gxg_glBindTexture)
XEN_NARGIFY_3(gxg_glAreTexturesResident_w, gxg_glAreTexturesResident)
XEN_NARGIFY_1(gxg_glIsTexture_w, gxg_glIsTexture)
XEN_NARGIFY_7(gxg_glTexSubImage1D_w, gxg_glTexSubImage1D)
XEN_NARGIFY_9(gxg_glTexSubImage2D_w, gxg_glTexSubImage2D)
XEN_NARGIFY_7(gxg_glCopyTexImage1D_w, gxg_glCopyTexImage1D)
XEN_NARGIFY_8(gxg_glCopyTexImage2D_w, gxg_glCopyTexImage2D)
XEN_NARGIFY_6(gxg_glCopyTexSubImage1D_w, gxg_glCopyTexSubImage1D)
XEN_NARGIFY_8(gxg_glCopyTexSubImage2D_w, gxg_glCopyTexSubImage2D)
XEN_NARGIFY_6(gxg_glMap1d_w, gxg_glMap1d)
XEN_NARGIFY_6(gxg_glMap1f_w, gxg_glMap1f)
XEN_VARGIFY(gxg_glMap2d_w, gxg_glMap2d)
XEN_VARGIFY(gxg_glMap2f_w, gxg_glMap2f)
XEN_ARGIFY_3(gxg_glGetMapdv_w, gxg_glGetMapdv)
XEN_ARGIFY_3(gxg_glGetMapfv_w, gxg_glGetMapfv)
XEN_ARGIFY_3(gxg_glGetMapiv_w, gxg_glGetMapiv)
XEN_NARGIFY_1(gxg_glEvalCoord1d_w, gxg_glEvalCoord1d)
XEN_NARGIFY_1(gxg_glEvalCoord1f_w, gxg_glEvalCoord1f)
XEN_NARGIFY_2(gxg_glEvalCoord2d_w, gxg_glEvalCoord2d)
XEN_NARGIFY_2(gxg_glEvalCoord2f_w, gxg_glEvalCoord2f)
XEN_NARGIFY_3(gxg_glMapGrid1d_w, gxg_glMapGrid1d)
XEN_NARGIFY_3(gxg_glMapGrid1f_w, gxg_glMapGrid1f)
XEN_NARGIFY_6(gxg_glMapGrid2d_w, gxg_glMapGrid2d)
XEN_NARGIFY_6(gxg_glMapGrid2f_w, gxg_glMapGrid2f)
XEN_NARGIFY_1(gxg_glEvalPoint1_w, gxg_glEvalPoint1)
XEN_NARGIFY_2(gxg_glEvalPoint2_w, gxg_glEvalPoint2)
XEN_NARGIFY_3(gxg_glEvalMesh1_w, gxg_glEvalMesh1)
XEN_NARGIFY_5(gxg_glEvalMesh2_w, gxg_glEvalMesh2)
XEN_NARGIFY_2(gxg_glFogf_w, gxg_glFogf)
XEN_NARGIFY_2(gxg_glFogi_w, gxg_glFogi)
XEN_NARGIFY_3(gxg_glFeedbackBuffer_w, gxg_glFeedbackBuffer)
XEN_NARGIFY_1(gxg_glPassThrough_w, gxg_glPassThrough)
XEN_NARGIFY_2(gxg_glSelectBuffer_w, gxg_glSelectBuffer)
XEN_NARGIFY_0(gxg_glInitNames_w, gxg_glInitNames)
XEN_NARGIFY_1(gxg_glLoadName_w, gxg_glLoadName)
XEN_NARGIFY_1(gxg_glPushName_w, gxg_glPushName)
XEN_NARGIFY_0(gxg_glPopName_w, gxg_glPopName)
XEN_NARGIFY_6(gxg_glDrawRangeElements_w, gxg_glDrawRangeElements)
XEN_VARGIFY(gxg_glTexImage3D_w, gxg_glTexImage3D)
XEN_VARGIFY(gxg_glTexSubImage3D_w, gxg_glTexSubImage3D)
XEN_NARGIFY_9(gxg_glCopyTexSubImage3D_w, gxg_glCopyTexSubImage3D)
XEN_NARGIFY_6(gxg_glColorTable_w, gxg_glColorTable)
XEN_NARGIFY_6(gxg_glColorSubTable_w, gxg_glColorSubTable)
XEN_NARGIFY_5(gxg_glCopyColorSubTable_w, gxg_glCopyColorSubTable)
XEN_NARGIFY_5(gxg_glCopyColorTable_w, gxg_glCopyColorTable)
XEN_ARGIFY_3(gxg_glGetColorTableParameterfv_w, gxg_glGetColorTableParameterfv)
XEN_ARGIFY_3(gxg_glGetColorTableParameteriv_w, gxg_glGetColorTableParameteriv)
XEN_NARGIFY_1(gxg_glBlendEquation_w, gxg_glBlendEquation)
XEN_NARGIFY_4(gxg_glBlendColor_w, gxg_glBlendColor)
XEN_NARGIFY_4(gxg_glHistogram_w, gxg_glHistogram)
XEN_NARGIFY_1(gxg_glResetHistogram_w, gxg_glResetHistogram)
XEN_NARGIFY_5(gxg_glGetHistogram_w, gxg_glGetHistogram)
XEN_ARGIFY_3(gxg_glGetHistogramParameterfv_w, gxg_glGetHistogramParameterfv)
XEN_ARGIFY_3(gxg_glGetHistogramParameteriv_w, gxg_glGetHistogramParameteriv)
XEN_NARGIFY_3(gxg_glMinmax_w, gxg_glMinmax)
XEN_NARGIFY_1(gxg_glResetMinmax_w, gxg_glResetMinmax)
XEN_NARGIFY_5(gxg_glGetMinmax_w, gxg_glGetMinmax)
XEN_ARGIFY_3(gxg_glGetMinmaxParameterfv_w, gxg_glGetMinmaxParameterfv)
XEN_ARGIFY_3(gxg_glGetMinmaxParameteriv_w, gxg_glGetMinmaxParameteriv)
XEN_NARGIFY_6(gxg_glConvolutionFilter1D_w, gxg_glConvolutionFilter1D)
XEN_NARGIFY_7(gxg_glConvolutionFilter2D_w, gxg_glConvolutionFilter2D)
XEN_NARGIFY_3(gxg_glConvolutionParameterf_w, gxg_glConvolutionParameterf)
XEN_NARGIFY_3(gxg_glConvolutionParameteri_w, gxg_glConvolutionParameteri)
XEN_NARGIFY_5(gxg_glCopyConvolutionFilter1D_w, gxg_glCopyConvolutionFilter1D)
XEN_NARGIFY_6(gxg_glCopyConvolutionFilter2D_w, gxg_glCopyConvolutionFilter2D)
XEN_NARGIFY_8(gxg_glSeparableFilter2D_w, gxg_glSeparableFilter2D)
#if HAVE_GLU
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluBeginPolygon_w, gxg_gluBeginPolygon)
#endif
XEN_NARGIFY_6(gxg_gluBuild1DMipmaps_w, gxg_gluBuild1DMipmaps)
XEN_NARGIFY_7(gxg_gluBuild2DMipmaps_w, gxg_gluBuild2DMipmaps)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluDeleteTess_w, gxg_gluDeleteTess)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluEndPolygon_w, gxg_gluEndPolygon)
#endif
XEN_NARGIFY_1(gxg_gluErrorString_w, gxg_gluErrorString)
XEN_NARGIFY_1(gxg_gluGetString_w, gxg_gluGetString)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluGetTessProperty_w, gxg_gluGetTessProperty)
#endif
XEN_NARGIFY_9(gxg_gluLookAt_w, gxg_gluLookAt)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_0(gxg_gluNewTess_w, gxg_gluNewTess)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_2(gxg_gluNextContour_w, gxg_gluNextContour)
#endif
XEN_NARGIFY_4(gxg_gluOrtho2D_w, gxg_gluOrtho2D)
XEN_NARGIFY_4(gxg_gluPerspective_w, gxg_gluPerspective)
XEN_NARGIFY_5(gxg_gluPickMatrix_w, gxg_gluPickMatrix)
XEN_NARGIFY_9(gxg_gluProject_w, gxg_gluProject)
XEN_NARGIFY_9(gxg_gluScaleImage_w, gxg_gluScaleImage)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessBeginContour_w, gxg_gluTessBeginContour)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_2(gxg_gluTessBeginPolygon_w, gxg_gluTessBeginPolygon)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessEndContour_w, gxg_gluTessEndContour)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessEndPolygon_w, gxg_gluTessEndPolygon)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_4(gxg_gluTessNormal_w, gxg_gluTessNormal)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluTessProperty_w, gxg_gluTessProperty)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluTessVertex_w, gxg_gluTessVertex)
#endif
XEN_NARGIFY_9(gxg_gluUnProject_w, gxg_gluUnProject)
#endif

#else

#if USE_MOTIF
#define gxg_glXChooseVisual_w gxg_glXChooseVisual
#define gxg_glXCopyContext_w gxg_glXCopyContext
#define gxg_glXCreateContext_w gxg_glXCreateContext
#define gxg_glXCreateGLXPixmap_w gxg_glXCreateGLXPixmap
#define gxg_glXDestroyContext_w gxg_glXDestroyContext
#define gxg_glXDestroyGLXPixmap_w gxg_glXDestroyGLXPixmap
#define gxg_glXGetConfig_w gxg_glXGetConfig
#define gxg_glXGetCurrentContext_w gxg_glXGetCurrentContext
#define gxg_glXGetCurrentDrawable_w gxg_glXGetCurrentDrawable
#define gxg_glXIsDirect_w gxg_glXIsDirect
#define gxg_glXMakeCurrent_w gxg_glXMakeCurrent
#define gxg_glXQueryExtension_w gxg_glXQueryExtension
#define gxg_glXQueryVersion_w gxg_glXQueryVersion
#define gxg_glXSwapBuffers_w gxg_glXSwapBuffers
#define gxg_glXUseXFont_w gxg_glXUseXFont
#define gxg_glXWaitGL_w gxg_glXWaitGL
#define gxg_glXWaitX_w gxg_glXWaitX
#define gxg_glXGetClientString_w gxg_glXGetClientString
#define gxg_glXQueryServerString_w gxg_glXQueryServerString
#define gxg_glXQueryExtensionsString_w gxg_glXQueryExtensionsString
#endif
#if USE_GTK
#define gxg_gdk_gl_query_extension_w gxg_gdk_gl_query_extension
#define gxg_gdk_gl_query_version_w gxg_gdk_gl_query_version
#define gxg_gdk_gl_query_gl_extension_w gxg_gdk_gl_query_gl_extension
#define gxg_gdk_gl_config_new_w gxg_gdk_gl_config_new
#define gxg_gdk_gl_config_new_by_mode_w gxg_gdk_gl_config_new_by_mode
#define gxg_gdk_gl_config_get_attrib_w gxg_gdk_gl_config_get_attrib
#define gxg_gdk_gl_config_get_colormap_w gxg_gdk_gl_config_get_colormap
#define gxg_gdk_gl_config_get_visual_w gxg_gdk_gl_config_get_visual
#define gxg_gdk_gl_config_get_depth_w gxg_gdk_gl_config_get_depth
#define gxg_gdk_gl_config_is_rgba_w gxg_gdk_gl_config_is_rgba
#define gxg_gdk_gl_config_is_double_buffered_w gxg_gdk_gl_config_is_double_buffered
#define gxg_gdk_gl_config_is_stereo_w gxg_gdk_gl_config_is_stereo
#define gxg_gdk_gl_config_has_alpha_w gxg_gdk_gl_config_has_alpha
#define gxg_gdk_gl_config_has_depth_buffer_w gxg_gdk_gl_config_has_depth_buffer
#define gxg_gdk_gl_config_has_stencil_buffer_w gxg_gdk_gl_config_has_stencil_buffer
#define gxg_gdk_gl_config_has_accum_buffer_w gxg_gdk_gl_config_has_accum_buffer
#define gxg_gdk_gl_context_get_gl_drawable_w gxg_gdk_gl_context_get_gl_drawable
#define gxg_gdk_gl_context_get_gl_config_w gxg_gdk_gl_context_get_gl_config
#define gxg_gdk_gl_context_get_share_list_w gxg_gdk_gl_context_get_share_list
#define gxg_gdk_gl_context_is_direct_w gxg_gdk_gl_context_is_direct
#define gxg_gdk_gl_context_get_render_type_w gxg_gdk_gl_context_get_render_type
#define gxg_gdk_gl_drawable_make_current_w gxg_gdk_gl_drawable_make_current
#define gxg_gdk_gl_drawable_is_double_buffered_w gxg_gdk_gl_drawable_is_double_buffered
#define gxg_gdk_gl_drawable_swap_buffers_w gxg_gdk_gl_drawable_swap_buffers
#define gxg_gdk_gl_drawable_wait_gl_w gxg_gdk_gl_drawable_wait_gl
#define gxg_gdk_gl_drawable_wait_gdk_w gxg_gdk_gl_drawable_wait_gdk
#define gxg_gdk_gl_drawable_get_gl_config_w gxg_gdk_gl_drawable_get_gl_config
#define gxg_gdk_gl_drawable_get_size_w gxg_gdk_gl_drawable_get_size
#define gxg_gdk_gl_pixmap_new_w gxg_gdk_gl_pixmap_new
#define gxg_gdk_gl_pixmap_get_pixmap_w gxg_gdk_gl_pixmap_get_pixmap
#define gxg_gdk_pixmap_set_gl_capability_w gxg_gdk_pixmap_set_gl_capability
#define gxg_gdk_pixmap_unset_gl_capability_w gxg_gdk_pixmap_unset_gl_capability
#define gxg_gdk_pixmap_is_gl_capable_w gxg_gdk_pixmap_is_gl_capable
#define gxg_gdk_pixmap_get_gl_pixmap_w gxg_gdk_pixmap_get_gl_pixmap
#define gxg_gdk_gl_window_new_w gxg_gdk_gl_window_new
#define gxg_gdk_gl_window_get_window_w gxg_gdk_gl_window_get_window
#define gxg_gdk_window_set_gl_capability_w gxg_gdk_window_set_gl_capability
#define gxg_gdk_window_unset_gl_capability_w gxg_gdk_window_unset_gl_capability
#define gxg_gdk_window_is_gl_capable_w gxg_gdk_window_is_gl_capable
#define gxg_gdk_window_get_gl_window_w gxg_gdk_window_get_gl_window
#define gxg_gdk_gl_font_use_pango_font_w gxg_gdk_gl_font_use_pango_font
#define gxg_gtk_widget_set_gl_capability_w gxg_gtk_widget_set_gl_capability
#define gxg_gtk_widget_is_gl_capable_w gxg_gtk_widget_is_gl_capable
#define gxg_gtk_widget_get_gl_config_w gxg_gtk_widget_get_gl_config
#define gxg_gtk_widget_get_gl_context_w gxg_gtk_widget_get_gl_context
#define gxg_gtk_widget_get_gl_window_w gxg_gtk_widget_get_gl_window
#ifdef GTKGLEXT_MAJOR_VERSION
#define gxg_gdk_gl_drawable_gl_begin_w gxg_gdk_gl_drawable_gl_begin
#define gxg_gdk_gl_drawable_gl_end_w gxg_gdk_gl_drawable_gl_end
#endif
#endif

#define gxg_glClearIndex_w gxg_glClearIndex
#define gxg_glClearColor_w gxg_glClearColor
#define gxg_glClear_w gxg_glClear
#define gxg_glIndexMask_w gxg_glIndexMask
#define gxg_glColorMask_w gxg_glColorMask
#define gxg_glAlphaFunc_w gxg_glAlphaFunc
#define gxg_glBlendFunc_w gxg_glBlendFunc
#define gxg_glLogicOp_w gxg_glLogicOp
#define gxg_glCullFace_w gxg_glCullFace
#define gxg_glFrontFace_w gxg_glFrontFace
#define gxg_glPointSize_w gxg_glPointSize
#define gxg_glLineWidth_w gxg_glLineWidth
#define gxg_glLineStipple_w gxg_glLineStipple
#define gxg_glPolygonMode_w gxg_glPolygonMode
#define gxg_glPolygonOffset_w gxg_glPolygonOffset
#define gxg_glPolygonStipple_w gxg_glPolygonStipple
#define gxg_glEdgeFlag_w gxg_glEdgeFlag
#define gxg_glScissor_w gxg_glScissor
#define gxg_glClipPlane_w gxg_glClipPlane
#define gxg_glGetClipPlane_w gxg_glGetClipPlane
#define gxg_glDrawBuffer_w gxg_glDrawBuffer
#define gxg_glReadBuffer_w gxg_glReadBuffer
#define gxg_glEnable_w gxg_glEnable
#define gxg_glDisable_w gxg_glDisable
#define gxg_glIsEnabled_w gxg_glIsEnabled
#define gxg_glEnableClientState_w gxg_glEnableClientState
#define gxg_glDisableClientState_w gxg_glDisableClientState
#define gxg_glGetBooleanv_w gxg_glGetBooleanv
#define gxg_glGetDoublev_w gxg_glGetDoublev
#define gxg_glGetFloatv_w gxg_glGetFloatv
#define gxg_glGetIntegerv_w gxg_glGetIntegerv
#define gxg_glPushAttrib_w gxg_glPushAttrib
#define gxg_glPopAttrib_w gxg_glPopAttrib
#define gxg_glPushClientAttrib_w gxg_glPushClientAttrib
#define gxg_glPopClientAttrib_w gxg_glPopClientAttrib
#define gxg_glRenderMode_w gxg_glRenderMode
#define gxg_glGetError_w gxg_glGetError
#define gxg_glGetString_w gxg_glGetString
#define gxg_glFinish_w gxg_glFinish
#define gxg_glFlush_w gxg_glFlush
#define gxg_glHint_w gxg_glHint
#define gxg_glClearDepth_w gxg_glClearDepth
#define gxg_glDepthFunc_w gxg_glDepthFunc
#define gxg_glDepthMask_w gxg_glDepthMask
#define gxg_glDepthRange_w gxg_glDepthRange
#define gxg_glClearAccum_w gxg_glClearAccum
#define gxg_glAccum_w gxg_glAccum
#define gxg_glMatrixMode_w gxg_glMatrixMode
#define gxg_glOrtho_w gxg_glOrtho
#define gxg_glFrustum_w gxg_glFrustum
#define gxg_glViewport_w gxg_glViewport
#define gxg_glPushMatrix_w gxg_glPushMatrix
#define gxg_glPopMatrix_w gxg_glPopMatrix
#define gxg_glLoadIdentity_w gxg_glLoadIdentity
#define gxg_glLoadMatrixd_w gxg_glLoadMatrixd
#define gxg_glLoadMatrixf_w gxg_glLoadMatrixf
#define gxg_glMultMatrixd_w gxg_glMultMatrixd
#define gxg_glMultMatrixf_w gxg_glMultMatrixf
#define gxg_glRotated_w gxg_glRotated
#define gxg_glRotatef_w gxg_glRotatef
#define gxg_glScaled_w gxg_glScaled
#define gxg_glScalef_w gxg_glScalef
#define gxg_glTranslated_w gxg_glTranslated
#define gxg_glTranslatef_w gxg_glTranslatef
#define gxg_glIsList_w gxg_glIsList
#define gxg_glDeleteLists_w gxg_glDeleteLists
#define gxg_glGenLists_w gxg_glGenLists
#define gxg_glNewList_w gxg_glNewList
#define gxg_glEndList_w gxg_glEndList
#define gxg_glCallList_w gxg_glCallList
#define gxg_glCallLists_w gxg_glCallLists
#define gxg_glListBase_w gxg_glListBase
#define gxg_glBegin_w gxg_glBegin
#define gxg_glEnd_w gxg_glEnd
#define gxg_glVertex2d_w gxg_glVertex2d
#define gxg_glVertex2f_w gxg_glVertex2f
#define gxg_glVertex2i_w gxg_glVertex2i
#define gxg_glVertex2s_w gxg_glVertex2s
#define gxg_glVertex3d_w gxg_glVertex3d
#define gxg_glVertex3f_w gxg_glVertex3f
#define gxg_glVertex3i_w gxg_glVertex3i
#define gxg_glVertex3s_w gxg_glVertex3s
#define gxg_glVertex4d_w gxg_glVertex4d
#define gxg_glVertex4f_w gxg_glVertex4f
#define gxg_glVertex4i_w gxg_glVertex4i
#define gxg_glVertex4s_w gxg_glVertex4s
#define gxg_glNormal3b_w gxg_glNormal3b
#define gxg_glNormal3d_w gxg_glNormal3d
#define gxg_glNormal3f_w gxg_glNormal3f
#define gxg_glNormal3i_w gxg_glNormal3i
#define gxg_glNormal3s_w gxg_glNormal3s
#define gxg_glIndexd_w gxg_glIndexd
#define gxg_glIndexf_w gxg_glIndexf
#define gxg_glIndexi_w gxg_glIndexi
#define gxg_glIndexs_w gxg_glIndexs
#define gxg_glIndexub_w gxg_glIndexub
#define gxg_glColor3b_w gxg_glColor3b
#define gxg_glColor3d_w gxg_glColor3d
#define gxg_glColor3f_w gxg_glColor3f
#define gxg_glColor3i_w gxg_glColor3i
#define gxg_glColor3s_w gxg_glColor3s
#define gxg_glColor3ub_w gxg_glColor3ub
#define gxg_glColor3ui_w gxg_glColor3ui
#define gxg_glColor3us_w gxg_glColor3us
#define gxg_glColor4b_w gxg_glColor4b
#define gxg_glColor4d_w gxg_glColor4d
#define gxg_glColor4f_w gxg_glColor4f
#define gxg_glColor4i_w gxg_glColor4i
#define gxg_glColor4s_w gxg_glColor4s
#define gxg_glColor4ub_w gxg_glColor4ub
#define gxg_glColor4ui_w gxg_glColor4ui
#define gxg_glColor4us_w gxg_glColor4us
#define gxg_glTexCoord1d_w gxg_glTexCoord1d
#define gxg_glTexCoord1f_w gxg_glTexCoord1f
#define gxg_glTexCoord1i_w gxg_glTexCoord1i
#define gxg_glTexCoord1s_w gxg_glTexCoord1s
#define gxg_glTexCoord2d_w gxg_glTexCoord2d
#define gxg_glTexCoord2f_w gxg_glTexCoord2f
#define gxg_glTexCoord2i_w gxg_glTexCoord2i
#define gxg_glTexCoord2s_w gxg_glTexCoord2s
#define gxg_glTexCoord3d_w gxg_glTexCoord3d
#define gxg_glTexCoord3f_w gxg_glTexCoord3f
#define gxg_glTexCoord3i_w gxg_glTexCoord3i
#define gxg_glTexCoord3s_w gxg_glTexCoord3s
#define gxg_glTexCoord4d_w gxg_glTexCoord4d
#define gxg_glTexCoord4f_w gxg_glTexCoord4f
#define gxg_glTexCoord4i_w gxg_glTexCoord4i
#define gxg_glTexCoord4s_w gxg_glTexCoord4s
#define gxg_glRasterPos2d_w gxg_glRasterPos2d
#define gxg_glRasterPos2f_w gxg_glRasterPos2f
#define gxg_glRasterPos2i_w gxg_glRasterPos2i
#define gxg_glRasterPos2s_w gxg_glRasterPos2s
#define gxg_glRasterPos3d_w gxg_glRasterPos3d
#define gxg_glRasterPos3f_w gxg_glRasterPos3f
#define gxg_glRasterPos3i_w gxg_glRasterPos3i
#define gxg_glRasterPos3s_w gxg_glRasterPos3s
#define gxg_glRasterPos4d_w gxg_glRasterPos4d
#define gxg_glRasterPos4f_w gxg_glRasterPos4f
#define gxg_glRasterPos4i_w gxg_glRasterPos4i
#define gxg_glRasterPos4s_w gxg_glRasterPos4s
#define gxg_glRectd_w gxg_glRectd
#define gxg_glRectf_w gxg_glRectf
#define gxg_glRecti_w gxg_glRecti
#define gxg_glRects_w gxg_glRects
#define gxg_glVertexPointer_w gxg_glVertexPointer
#define gxg_glNormalPointer_w gxg_glNormalPointer
#define gxg_glColorPointer_w gxg_glColorPointer
#define gxg_glIndexPointer_w gxg_glIndexPointer
#define gxg_glTexCoordPointer_w gxg_glTexCoordPointer
#define gxg_glEdgeFlagPointer_w gxg_glEdgeFlagPointer
#define gxg_glGetPointerv_w gxg_glGetPointerv
#define gxg_glArrayElement_w gxg_glArrayElement
#define gxg_glDrawArrays_w gxg_glDrawArrays
#define gxg_glDrawElements_w gxg_glDrawElements
#define gxg_glInterleavedArrays_w gxg_glInterleavedArrays
#define gxg_glShadeModel_w gxg_glShadeModel
#define gxg_glLightf_w gxg_glLightf
#define gxg_glLighti_w gxg_glLighti
#define gxg_glGetLightfv_w gxg_glGetLightfv
#define gxg_glGetLightiv_w gxg_glGetLightiv
#define gxg_glLightModelf_w gxg_glLightModelf
#define gxg_glLightModeli_w gxg_glLightModeli
#define gxg_glMaterialf_w gxg_glMaterialf
#define gxg_glMateriali_w gxg_glMateriali
#define gxg_glGetMaterialfv_w gxg_glGetMaterialfv
#define gxg_glGetMaterialiv_w gxg_glGetMaterialiv
#define gxg_glColorMaterial_w gxg_glColorMaterial
#define gxg_glPixelZoom_w gxg_glPixelZoom
#define gxg_glPixelStoref_w gxg_glPixelStoref
#define gxg_glPixelStorei_w gxg_glPixelStorei
#define gxg_glPixelTransferf_w gxg_glPixelTransferf
#define gxg_glPixelTransferi_w gxg_glPixelTransferi
#define gxg_glGetPixelMapfv_w gxg_glGetPixelMapfv
#define gxg_glGetPixelMapuiv_w gxg_glGetPixelMapuiv
#define gxg_glGetPixelMapusv_w gxg_glGetPixelMapusv
#define gxg_glBitmap_w gxg_glBitmap
#define gxg_glReadPixels_w gxg_glReadPixels
#define gxg_glDrawPixels_w gxg_glDrawPixels
#define gxg_glCopyPixels_w gxg_glCopyPixels
#define gxg_glStencilFunc_w gxg_glStencilFunc
#define gxg_glStencilMask_w gxg_glStencilMask
#define gxg_glStencilOp_w gxg_glStencilOp
#define gxg_glClearStencil_w gxg_glClearStencil
#define gxg_glTexGend_w gxg_glTexGend
#define gxg_glTexGenf_w gxg_glTexGenf
#define gxg_glTexGeni_w gxg_glTexGeni
#define gxg_glGetTexGendv_w gxg_glGetTexGendv
#define gxg_glGetTexGenfv_w gxg_glGetTexGenfv
#define gxg_glGetTexGeniv_w gxg_glGetTexGeniv
#define gxg_glTexEnvf_w gxg_glTexEnvf
#define gxg_glTexEnvi_w gxg_glTexEnvi
#define gxg_glGetTexEnvfv_w gxg_glGetTexEnvfv
#define gxg_glGetTexEnviv_w gxg_glGetTexEnviv
#define gxg_glTexParameterf_w gxg_glTexParameterf
#define gxg_glTexParameteri_w gxg_glTexParameteri
#define gxg_glGetTexParameterfv_w gxg_glGetTexParameterfv
#define gxg_glGetTexParameteriv_w gxg_glGetTexParameteriv
#define gxg_glGetTexLevelParameterfv_w gxg_glGetTexLevelParameterfv
#define gxg_glGetTexLevelParameteriv_w gxg_glGetTexLevelParameteriv
#define gxg_glTexImage1D_w gxg_glTexImage1D
#define gxg_glTexImage2D_w gxg_glTexImage2D
#define gxg_glGenTextures_w gxg_glGenTextures
#define gxg_glDeleteTextures_w gxg_glDeleteTextures
#define gxg_glBindTexture_w gxg_glBindTexture
#define gxg_glAreTexturesResident_w gxg_glAreTexturesResident
#define gxg_glIsTexture_w gxg_glIsTexture
#define gxg_glTexSubImage1D_w gxg_glTexSubImage1D
#define gxg_glTexSubImage2D_w gxg_glTexSubImage2D
#define gxg_glCopyTexImage1D_w gxg_glCopyTexImage1D
#define gxg_glCopyTexImage2D_w gxg_glCopyTexImage2D
#define gxg_glCopyTexSubImage1D_w gxg_glCopyTexSubImage1D
#define gxg_glCopyTexSubImage2D_w gxg_glCopyTexSubImage2D
#define gxg_glMap1d_w gxg_glMap1d
#define gxg_glMap1f_w gxg_glMap1f
#define gxg_glMap2d_w gxg_glMap2d
#define gxg_glMap2f_w gxg_glMap2f
#define gxg_glGetMapdv_w gxg_glGetMapdv
#define gxg_glGetMapfv_w gxg_glGetMapfv
#define gxg_glGetMapiv_w gxg_glGetMapiv
#define gxg_glEvalCoord1d_w gxg_glEvalCoord1d
#define gxg_glEvalCoord1f_w gxg_glEvalCoord1f
#define gxg_glEvalCoord2d_w gxg_glEvalCoord2d
#define gxg_glEvalCoord2f_w gxg_glEvalCoord2f
#define gxg_glMapGrid1d_w gxg_glMapGrid1d
#define gxg_glMapGrid1f_w gxg_glMapGrid1f
#define gxg_glMapGrid2d_w gxg_glMapGrid2d
#define gxg_glMapGrid2f_w gxg_glMapGrid2f
#define gxg_glEvalPoint1_w gxg_glEvalPoint1
#define gxg_glEvalPoint2_w gxg_glEvalPoint2
#define gxg_glEvalMesh1_w gxg_glEvalMesh1
#define gxg_glEvalMesh2_w gxg_glEvalMesh2
#define gxg_glFogf_w gxg_glFogf
#define gxg_glFogi_w gxg_glFogi
#define gxg_glFeedbackBuffer_w gxg_glFeedbackBuffer
#define gxg_glPassThrough_w gxg_glPassThrough
#define gxg_glSelectBuffer_w gxg_glSelectBuffer
#define gxg_glInitNames_w gxg_glInitNames
#define gxg_glLoadName_w gxg_glLoadName
#define gxg_glPushName_w gxg_glPushName
#define gxg_glPopName_w gxg_glPopName
#define gxg_glDrawRangeElements_w gxg_glDrawRangeElements
#define gxg_glTexImage3D_w gxg_glTexImage3D
#define gxg_glTexSubImage3D_w gxg_glTexSubImage3D
#define gxg_glCopyTexSubImage3D_w gxg_glCopyTexSubImage3D
#define gxg_glColorTable_w gxg_glColorTable
#define gxg_glColorSubTable_w gxg_glColorSubTable
#define gxg_glCopyColorSubTable_w gxg_glCopyColorSubTable
#define gxg_glCopyColorTable_w gxg_glCopyColorTable
#define gxg_glGetColorTableParameterfv_w gxg_glGetColorTableParameterfv
#define gxg_glGetColorTableParameteriv_w gxg_glGetColorTableParameteriv
#define gxg_glBlendEquation_w gxg_glBlendEquation
#define gxg_glBlendColor_w gxg_glBlendColor
#define gxg_glHistogram_w gxg_glHistogram
#define gxg_glResetHistogram_w gxg_glResetHistogram
#define gxg_glGetHistogram_w gxg_glGetHistogram
#define gxg_glGetHistogramParameterfv_w gxg_glGetHistogramParameterfv
#define gxg_glGetHistogramParameteriv_w gxg_glGetHistogramParameteriv
#define gxg_glMinmax_w gxg_glMinmax
#define gxg_glResetMinmax_w gxg_glResetMinmax
#define gxg_glGetMinmax_w gxg_glGetMinmax
#define gxg_glGetMinmaxParameterfv_w gxg_glGetMinmaxParameterfv
#define gxg_glGetMinmaxParameteriv_w gxg_glGetMinmaxParameteriv
#define gxg_glConvolutionFilter1D_w gxg_glConvolutionFilter1D
#define gxg_glConvolutionFilter2D_w gxg_glConvolutionFilter2D
#define gxg_glConvolutionParameterf_w gxg_glConvolutionParameterf
#define gxg_glConvolutionParameteri_w gxg_glConvolutionParameteri
#define gxg_glCopyConvolutionFilter1D_w gxg_glCopyConvolutionFilter1D
#define gxg_glCopyConvolutionFilter2D_w gxg_glCopyConvolutionFilter2D
#define gxg_glSeparableFilter2D_w gxg_glSeparableFilter2D
#if HAVE_GLU
#ifdef GLU_VERSION_1_2
#define gxg_gluBeginPolygon_w gxg_gluBeginPolygon
#endif
#define gxg_gluBuild1DMipmaps_w gxg_gluBuild1DMipmaps
#define gxg_gluBuild2DMipmaps_w gxg_gluBuild2DMipmaps
#ifdef GLU_VERSION_1_2
#define gxg_gluDeleteTess_w gxg_gluDeleteTess
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluEndPolygon_w gxg_gluEndPolygon
#endif
#define gxg_gluErrorString_w gxg_gluErrorString
#define gxg_gluGetString_w gxg_gluGetString
#ifdef GLU_VERSION_1_2
#define gxg_gluGetTessProperty_w gxg_gluGetTessProperty
#endif
#define gxg_gluLookAt_w gxg_gluLookAt
#ifdef GLU_VERSION_1_2
#define gxg_gluNewTess_w gxg_gluNewTess
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluNextContour_w gxg_gluNextContour
#endif
#define gxg_gluOrtho2D_w gxg_gluOrtho2D
#define gxg_gluPerspective_w gxg_gluPerspective
#define gxg_gluPickMatrix_w gxg_gluPickMatrix
#define gxg_gluProject_w gxg_gluProject
#define gxg_gluScaleImage_w gxg_gluScaleImage
#ifdef GLU_VERSION_1_2
#define gxg_gluTessBeginContour_w gxg_gluTessBeginContour
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessBeginPolygon_w gxg_gluTessBeginPolygon
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessEndContour_w gxg_gluTessEndContour
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessEndPolygon_w gxg_gluTessEndPolygon
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessNormal_w gxg_gluTessNormal
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessProperty_w gxg_gluTessProperty
#endif
#ifdef GLU_VERSION_1_2
#define gxg_gluTessVertex_w gxg_gluTessVertex
#endif
#define gxg_gluUnProject_w gxg_gluUnProject
#endif
#endif
static void define_functions(void)
{
  #define GL_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XL_PRE #Name XL_POST, Value, A1, A2, A3, Help)
#if USE_MOTIF
  GL_DEFINE_PROCEDURE(glXChooseVisual, gxg_glXChooseVisual_w, 3, 0, 0, H_glXChooseVisual);
  GL_DEFINE_PROCEDURE(glXCopyContext, gxg_glXCopyContext_w, 4, 0, 0, H_glXCopyContext);
  GL_DEFINE_PROCEDURE(glXCreateContext, gxg_glXCreateContext_w, 4, 0, 0, H_glXCreateContext);
  GL_DEFINE_PROCEDURE(glXCreateGLXPixmap, gxg_glXCreateGLXPixmap_w, 3, 0, 0, H_glXCreateGLXPixmap);
  GL_DEFINE_PROCEDURE(glXDestroyContext, gxg_glXDestroyContext_w, 2, 0, 0, H_glXDestroyContext);
  GL_DEFINE_PROCEDURE(glXDestroyGLXPixmap, gxg_glXDestroyGLXPixmap_w, 2, 0, 0, H_glXDestroyGLXPixmap);
  GL_DEFINE_PROCEDURE(glXGetConfig, gxg_glXGetConfig_w, 3, 1, 0, H_glXGetConfig);
  GL_DEFINE_PROCEDURE(glXGetCurrentContext, gxg_glXGetCurrentContext_w, 0, 0, 0, H_glXGetCurrentContext);
  GL_DEFINE_PROCEDURE(glXGetCurrentDrawable, gxg_glXGetCurrentDrawable_w, 0, 0, 0, H_glXGetCurrentDrawable);
  GL_DEFINE_PROCEDURE(glXIsDirect, gxg_glXIsDirect_w, 2, 0, 0, H_glXIsDirect);
  GL_DEFINE_PROCEDURE(glXMakeCurrent, gxg_glXMakeCurrent_w, 3, 0, 0, H_glXMakeCurrent);
  GL_DEFINE_PROCEDURE(glXQueryExtension, gxg_glXQueryExtension_w, 1, 2, 0, H_glXQueryExtension);
  GL_DEFINE_PROCEDURE(glXQueryVersion, gxg_glXQueryVersion_w, 1, 2, 0, H_glXQueryVersion);
  GL_DEFINE_PROCEDURE(glXSwapBuffers, gxg_glXSwapBuffers_w, 2, 0, 0, H_glXSwapBuffers);
  GL_DEFINE_PROCEDURE(glXUseXFont, gxg_glXUseXFont_w, 4, 0, 0, H_glXUseXFont);
  GL_DEFINE_PROCEDURE(glXWaitGL, gxg_glXWaitGL_w, 0, 0, 0, H_glXWaitGL);
  GL_DEFINE_PROCEDURE(glXWaitX, gxg_glXWaitX_w, 0, 0, 0, H_glXWaitX);
  GL_DEFINE_PROCEDURE(glXGetClientString, gxg_glXGetClientString_w, 2, 0, 0, H_glXGetClientString);
  GL_DEFINE_PROCEDURE(glXQueryServerString, gxg_glXQueryServerString_w, 3, 0, 0, H_glXQueryServerString);
  GL_DEFINE_PROCEDURE(glXQueryExtensionsString, gxg_glXQueryExtensionsString_w, 2, 0, 0, H_glXQueryExtensionsString);
#endif
#if USE_GTK
  GL_DEFINE_PROCEDURE(gdk_gl_query_extension, gxg_gdk_gl_query_extension_w, 0, 0, 0, H_gdk_gl_query_extension);
  GL_DEFINE_PROCEDURE(gdk_gl_query_version, gxg_gdk_gl_query_version_w, 2, 0, 0, H_gdk_gl_query_version);
  GL_DEFINE_PROCEDURE(gdk_gl_query_gl_extension, gxg_gdk_gl_query_gl_extension_w, 1, 0, 0, H_gdk_gl_query_gl_extension);
  GL_DEFINE_PROCEDURE(gdk_gl_config_new, gxg_gdk_gl_config_new_w, 1, 0, 0, H_gdk_gl_config_new);
  GL_DEFINE_PROCEDURE(gdk_gl_config_new_by_mode, gxg_gdk_gl_config_new_by_mode_w, 1, 0, 0, H_gdk_gl_config_new_by_mode);
  GL_DEFINE_PROCEDURE(gdk_gl_config_get_attrib, gxg_gdk_gl_config_get_attrib_w, 2, 1, 0, H_gdk_gl_config_get_attrib);
  GL_DEFINE_PROCEDURE(gdk_gl_config_get_colormap, gxg_gdk_gl_config_get_colormap_w, 1, 0, 0, H_gdk_gl_config_get_colormap);
  GL_DEFINE_PROCEDURE(gdk_gl_config_get_visual, gxg_gdk_gl_config_get_visual_w, 1, 0, 0, H_gdk_gl_config_get_visual);
  GL_DEFINE_PROCEDURE(gdk_gl_config_get_depth, gxg_gdk_gl_config_get_depth_w, 1, 0, 0, H_gdk_gl_config_get_depth);
  GL_DEFINE_PROCEDURE(gdk_gl_config_is_rgba, gxg_gdk_gl_config_is_rgba_w, 1, 0, 0, H_gdk_gl_config_is_rgba);
  GL_DEFINE_PROCEDURE(gdk_gl_config_is_double_buffered, gxg_gdk_gl_config_is_double_buffered_w, 1, 0, 0, H_gdk_gl_config_is_double_buffered);
  GL_DEFINE_PROCEDURE(gdk_gl_config_is_stereo, gxg_gdk_gl_config_is_stereo_w, 1, 0, 0, H_gdk_gl_config_is_stereo);
  GL_DEFINE_PROCEDURE(gdk_gl_config_has_alpha, gxg_gdk_gl_config_has_alpha_w, 1, 0, 0, H_gdk_gl_config_has_alpha);
  GL_DEFINE_PROCEDURE(gdk_gl_config_has_depth_buffer, gxg_gdk_gl_config_has_depth_buffer_w, 1, 0, 0, H_gdk_gl_config_has_depth_buffer);
  GL_DEFINE_PROCEDURE(gdk_gl_config_has_stencil_buffer, gxg_gdk_gl_config_has_stencil_buffer_w, 1, 0, 0, H_gdk_gl_config_has_stencil_buffer);
  GL_DEFINE_PROCEDURE(gdk_gl_config_has_accum_buffer, gxg_gdk_gl_config_has_accum_buffer_w, 1, 0, 0, H_gdk_gl_config_has_accum_buffer);
  GL_DEFINE_PROCEDURE(gdk_gl_context_get_gl_drawable, gxg_gdk_gl_context_get_gl_drawable_w, 1, 0, 0, H_gdk_gl_context_get_gl_drawable);
  GL_DEFINE_PROCEDURE(gdk_gl_context_get_gl_config, gxg_gdk_gl_context_get_gl_config_w, 1, 0, 0, H_gdk_gl_context_get_gl_config);
  GL_DEFINE_PROCEDURE(gdk_gl_context_get_share_list, gxg_gdk_gl_context_get_share_list_w, 1, 0, 0, H_gdk_gl_context_get_share_list);
  GL_DEFINE_PROCEDURE(gdk_gl_context_is_direct, gxg_gdk_gl_context_is_direct_w, 1, 0, 0, H_gdk_gl_context_is_direct);
  GL_DEFINE_PROCEDURE(gdk_gl_context_get_render_type, gxg_gdk_gl_context_get_render_type_w, 1, 0, 0, H_gdk_gl_context_get_render_type);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_make_current, gxg_gdk_gl_drawable_make_current_w, 2, 0, 0, H_gdk_gl_drawable_make_current);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_is_double_buffered, gxg_gdk_gl_drawable_is_double_buffered_w, 1, 0, 0, H_gdk_gl_drawable_is_double_buffered);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_swap_buffers, gxg_gdk_gl_drawable_swap_buffers_w, 1, 0, 0, H_gdk_gl_drawable_swap_buffers);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_wait_gl, gxg_gdk_gl_drawable_wait_gl_w, 1, 0, 0, H_gdk_gl_drawable_wait_gl);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_wait_gdk, gxg_gdk_gl_drawable_wait_gdk_w, 1, 0, 0, H_gdk_gl_drawable_wait_gdk);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_get_gl_config, gxg_gdk_gl_drawable_get_gl_config_w, 1, 0, 0, H_gdk_gl_drawable_get_gl_config);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_get_size, gxg_gdk_gl_drawable_get_size_w, 1, 2, 0, H_gdk_gl_drawable_get_size);
  GL_DEFINE_PROCEDURE(gdk_gl_pixmap_new, gxg_gdk_gl_pixmap_new_w, 3, 0, 0, H_gdk_gl_pixmap_new);
  GL_DEFINE_PROCEDURE(gdk_gl_pixmap_get_pixmap, gxg_gdk_gl_pixmap_get_pixmap_w, 1, 0, 0, H_gdk_gl_pixmap_get_pixmap);
  GL_DEFINE_PROCEDURE(gdk_pixmap_set_gl_capability, gxg_gdk_pixmap_set_gl_capability_w, 3, 0, 0, H_gdk_pixmap_set_gl_capability);
  GL_DEFINE_PROCEDURE(gdk_pixmap_unset_gl_capability, gxg_gdk_pixmap_unset_gl_capability_w, 1, 0, 0, H_gdk_pixmap_unset_gl_capability);
  GL_DEFINE_PROCEDURE(gdk_pixmap_is_gl_capable, gxg_gdk_pixmap_is_gl_capable_w, 1, 0, 0, H_gdk_pixmap_is_gl_capable);
  GL_DEFINE_PROCEDURE(gdk_pixmap_get_gl_pixmap, gxg_gdk_pixmap_get_gl_pixmap_w, 1, 0, 0, H_gdk_pixmap_get_gl_pixmap);
  GL_DEFINE_PROCEDURE(gdk_gl_window_new, gxg_gdk_gl_window_new_w, 3, 0, 0, H_gdk_gl_window_new);
  GL_DEFINE_PROCEDURE(gdk_gl_window_get_window, gxg_gdk_gl_window_get_window_w, 1, 0, 0, H_gdk_gl_window_get_window);
  GL_DEFINE_PROCEDURE(gdk_window_set_gl_capability, gxg_gdk_window_set_gl_capability_w, 3, 0, 0, H_gdk_window_set_gl_capability);
  GL_DEFINE_PROCEDURE(gdk_window_unset_gl_capability, gxg_gdk_window_unset_gl_capability_w, 1, 0, 0, H_gdk_window_unset_gl_capability);
  GL_DEFINE_PROCEDURE(gdk_window_is_gl_capable, gxg_gdk_window_is_gl_capable_w, 1, 0, 0, H_gdk_window_is_gl_capable);
  GL_DEFINE_PROCEDURE(gdk_window_get_gl_window, gxg_gdk_window_get_gl_window_w, 1, 0, 0, H_gdk_window_get_gl_window);
  GL_DEFINE_PROCEDURE(gdk_gl_font_use_pango_font, gxg_gdk_gl_font_use_pango_font_w, 4, 0, 0, H_gdk_gl_font_use_pango_font);
  GL_DEFINE_PROCEDURE(gtk_widget_set_gl_capability, gxg_gtk_widget_set_gl_capability_w, 5, 0, 0, H_gtk_widget_set_gl_capability);
  GL_DEFINE_PROCEDURE(gtk_widget_is_gl_capable, gxg_gtk_widget_is_gl_capable_w, 1, 0, 0, H_gtk_widget_is_gl_capable);
  GL_DEFINE_PROCEDURE(gtk_widget_get_gl_config, gxg_gtk_widget_get_gl_config_w, 1, 0, 0, H_gtk_widget_get_gl_config);
  GL_DEFINE_PROCEDURE(gtk_widget_get_gl_context, gxg_gtk_widget_get_gl_context_w, 1, 0, 0, H_gtk_widget_get_gl_context);
  GL_DEFINE_PROCEDURE(gtk_widget_get_gl_window, gxg_gtk_widget_get_gl_window_w, 1, 0, 0, H_gtk_widget_get_gl_window);
#ifdef GTKGLEXT_MAJOR_VERSION
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_gl_begin, gxg_gdk_gl_drawable_gl_begin_w, 2, 0, 0, H_gdk_gl_drawable_gl_begin);
  GL_DEFINE_PROCEDURE(gdk_gl_drawable_gl_end, gxg_gdk_gl_drawable_gl_end_w, 1, 0, 0, H_gdk_gl_drawable_gl_end);
#endif
#endif
  GL_DEFINE_PROCEDURE(glClearIndex, gxg_glClearIndex_w, 1, 0, 0, H_glClearIndex);
  GL_DEFINE_PROCEDURE(glClearColor, gxg_glClearColor_w, 4, 0, 0, H_glClearColor);
  GL_DEFINE_PROCEDURE(glClear, gxg_glClear_w, 1, 0, 0, H_glClear);
  GL_DEFINE_PROCEDURE(glIndexMask, gxg_glIndexMask_w, 1, 0, 0, H_glIndexMask);
  GL_DEFINE_PROCEDURE(glColorMask, gxg_glColorMask_w, 4, 0, 0, H_glColorMask);
  GL_DEFINE_PROCEDURE(glAlphaFunc, gxg_glAlphaFunc_w, 2, 0, 0, H_glAlphaFunc);
  GL_DEFINE_PROCEDURE(glBlendFunc, gxg_glBlendFunc_w, 2, 0, 0, H_glBlendFunc);
  GL_DEFINE_PROCEDURE(glLogicOp, gxg_glLogicOp_w, 1, 0, 0, H_glLogicOp);
  GL_DEFINE_PROCEDURE(glCullFace, gxg_glCullFace_w, 1, 0, 0, H_glCullFace);
  GL_DEFINE_PROCEDURE(glFrontFace, gxg_glFrontFace_w, 1, 0, 0, H_glFrontFace);
  GL_DEFINE_PROCEDURE(glPointSize, gxg_glPointSize_w, 1, 0, 0, H_glPointSize);
  GL_DEFINE_PROCEDURE(glLineWidth, gxg_glLineWidth_w, 1, 0, 0, H_glLineWidth);
  GL_DEFINE_PROCEDURE(glLineStipple, gxg_glLineStipple_w, 2, 0, 0, H_glLineStipple);
  GL_DEFINE_PROCEDURE(glPolygonMode, gxg_glPolygonMode_w, 2, 0, 0, H_glPolygonMode);
  GL_DEFINE_PROCEDURE(glPolygonOffset, gxg_glPolygonOffset_w, 2, 0, 0, H_glPolygonOffset);
  GL_DEFINE_PROCEDURE(glPolygonStipple, gxg_glPolygonStipple_w, 1, 0, 0, H_glPolygonStipple);
  GL_DEFINE_PROCEDURE(glEdgeFlag, gxg_glEdgeFlag_w, 1, 0, 0, H_glEdgeFlag);
  GL_DEFINE_PROCEDURE(glScissor, gxg_glScissor_w, 4, 0, 0, H_glScissor);
  GL_DEFINE_PROCEDURE(glClipPlane, gxg_glClipPlane_w, 2, 0, 0, H_glClipPlane);
  GL_DEFINE_PROCEDURE(glGetClipPlane, gxg_glGetClipPlane_w, 1, 1, 0, H_glGetClipPlane);
  GL_DEFINE_PROCEDURE(glDrawBuffer, gxg_glDrawBuffer_w, 1, 0, 0, H_glDrawBuffer);
  GL_DEFINE_PROCEDURE(glReadBuffer, gxg_glReadBuffer_w, 1, 0, 0, H_glReadBuffer);
  GL_DEFINE_PROCEDURE(glEnable, gxg_glEnable_w, 1, 0, 0, H_glEnable);
  GL_DEFINE_PROCEDURE(glDisable, gxg_glDisable_w, 1, 0, 0, H_glDisable);
  GL_DEFINE_PROCEDURE(glIsEnabled, gxg_glIsEnabled_w, 1, 0, 0, H_glIsEnabled);
  GL_DEFINE_PROCEDURE(glEnableClientState, gxg_glEnableClientState_w, 1, 0, 0, H_glEnableClientState);
  GL_DEFINE_PROCEDURE(glDisableClientState, gxg_glDisableClientState_w, 1, 0, 0, H_glDisableClientState);
  GL_DEFINE_PROCEDURE(glGetBooleanv, gxg_glGetBooleanv_w, 1, 1, 0, H_glGetBooleanv);
  GL_DEFINE_PROCEDURE(glGetDoublev, gxg_glGetDoublev_w, 1, 1, 0, H_glGetDoublev);
  GL_DEFINE_PROCEDURE(glGetFloatv, gxg_glGetFloatv_w, 1, 1, 0, H_glGetFloatv);
  GL_DEFINE_PROCEDURE(glGetIntegerv, gxg_glGetIntegerv_w, 1, 1, 0, H_glGetIntegerv);
  GL_DEFINE_PROCEDURE(glPushAttrib, gxg_glPushAttrib_w, 1, 0, 0, H_glPushAttrib);
  GL_DEFINE_PROCEDURE(glPopAttrib, gxg_glPopAttrib_w, 0, 0, 0, H_glPopAttrib);
  GL_DEFINE_PROCEDURE(glPushClientAttrib, gxg_glPushClientAttrib_w, 1, 0, 0, H_glPushClientAttrib);
  GL_DEFINE_PROCEDURE(glPopClientAttrib, gxg_glPopClientAttrib_w, 0, 0, 0, H_glPopClientAttrib);
  GL_DEFINE_PROCEDURE(glRenderMode, gxg_glRenderMode_w, 1, 0, 0, H_glRenderMode);
  GL_DEFINE_PROCEDURE(glGetError, gxg_glGetError_w, 0, 0, 0, H_glGetError);
  GL_DEFINE_PROCEDURE(glGetString, gxg_glGetString_w, 1, 0, 0, H_glGetString);
  GL_DEFINE_PROCEDURE(glFinish, gxg_glFinish_w, 0, 0, 0, H_glFinish);
  GL_DEFINE_PROCEDURE(glFlush, gxg_glFlush_w, 0, 0, 0, H_glFlush);
  GL_DEFINE_PROCEDURE(glHint, gxg_glHint_w, 2, 0, 0, H_glHint);
  GL_DEFINE_PROCEDURE(glClearDepth, gxg_glClearDepth_w, 1, 0, 0, H_glClearDepth);
  GL_DEFINE_PROCEDURE(glDepthFunc, gxg_glDepthFunc_w, 1, 0, 0, H_glDepthFunc);
  GL_DEFINE_PROCEDURE(glDepthMask, gxg_glDepthMask_w, 1, 0, 0, H_glDepthMask);
  GL_DEFINE_PROCEDURE(glDepthRange, gxg_glDepthRange_w, 2, 0, 0, H_glDepthRange);
  GL_DEFINE_PROCEDURE(glClearAccum, gxg_glClearAccum_w, 4, 0, 0, H_glClearAccum);
  GL_DEFINE_PROCEDURE(glAccum, gxg_glAccum_w, 2, 0, 0, H_glAccum);
  GL_DEFINE_PROCEDURE(glMatrixMode, gxg_glMatrixMode_w, 1, 0, 0, H_glMatrixMode);
  GL_DEFINE_PROCEDURE(glOrtho, gxg_glOrtho_w, 6, 0, 0, H_glOrtho);
  GL_DEFINE_PROCEDURE(glFrustum, gxg_glFrustum_w, 6, 0, 0, H_glFrustum);
  GL_DEFINE_PROCEDURE(glViewport, gxg_glViewport_w, 4, 0, 0, H_glViewport);
  GL_DEFINE_PROCEDURE(glPushMatrix, gxg_glPushMatrix_w, 0, 0, 0, H_glPushMatrix);
  GL_DEFINE_PROCEDURE(glPopMatrix, gxg_glPopMatrix_w, 0, 0, 0, H_glPopMatrix);
  GL_DEFINE_PROCEDURE(glLoadIdentity, gxg_glLoadIdentity_w, 0, 0, 0, H_glLoadIdentity);
  GL_DEFINE_PROCEDURE(glLoadMatrixd, gxg_glLoadMatrixd_w, 1, 0, 0, H_glLoadMatrixd);
  GL_DEFINE_PROCEDURE(glLoadMatrixf, gxg_glLoadMatrixf_w, 1, 0, 0, H_glLoadMatrixf);
  GL_DEFINE_PROCEDURE(glMultMatrixd, gxg_glMultMatrixd_w, 1, 0, 0, H_glMultMatrixd);
  GL_DEFINE_PROCEDURE(glMultMatrixf, gxg_glMultMatrixf_w, 1, 0, 0, H_glMultMatrixf);
  GL_DEFINE_PROCEDURE(glRotated, gxg_glRotated_w, 4, 0, 0, H_glRotated);
  GL_DEFINE_PROCEDURE(glRotatef, gxg_glRotatef_w, 4, 0, 0, H_glRotatef);
  GL_DEFINE_PROCEDURE(glScaled, gxg_glScaled_w, 3, 0, 0, H_glScaled);
  GL_DEFINE_PROCEDURE(glScalef, gxg_glScalef_w, 3, 0, 0, H_glScalef);
  GL_DEFINE_PROCEDURE(glTranslated, gxg_glTranslated_w, 3, 0, 0, H_glTranslated);
  GL_DEFINE_PROCEDURE(glTranslatef, gxg_glTranslatef_w, 3, 0, 0, H_glTranslatef);
  GL_DEFINE_PROCEDURE(glIsList, gxg_glIsList_w, 1, 0, 0, H_glIsList);
  GL_DEFINE_PROCEDURE(glDeleteLists, gxg_glDeleteLists_w, 2, 0, 0, H_glDeleteLists);
  GL_DEFINE_PROCEDURE(glGenLists, gxg_glGenLists_w, 1, 0, 0, H_glGenLists);
  GL_DEFINE_PROCEDURE(glNewList, gxg_glNewList_w, 2, 0, 0, H_glNewList);
  GL_DEFINE_PROCEDURE(glEndList, gxg_glEndList_w, 0, 0, 0, H_glEndList);
  GL_DEFINE_PROCEDURE(glCallList, gxg_glCallList_w, 1, 0, 0, H_glCallList);
  GL_DEFINE_PROCEDURE(glCallLists, gxg_glCallLists_w, 3, 0, 0, H_glCallLists);
  GL_DEFINE_PROCEDURE(glListBase, gxg_glListBase_w, 1, 0, 0, H_glListBase);
  GL_DEFINE_PROCEDURE(glBegin, gxg_glBegin_w, 1, 0, 0, H_glBegin);
  GL_DEFINE_PROCEDURE(glEnd, gxg_glEnd_w, 0, 0, 0, H_glEnd);
  GL_DEFINE_PROCEDURE(glVertex2d, gxg_glVertex2d_w, 2, 0, 0, H_glVertex2d);
  GL_DEFINE_PROCEDURE(glVertex2f, gxg_glVertex2f_w, 2, 0, 0, H_glVertex2f);
  GL_DEFINE_PROCEDURE(glVertex2i, gxg_glVertex2i_w, 2, 0, 0, H_glVertex2i);
  GL_DEFINE_PROCEDURE(glVertex2s, gxg_glVertex2s_w, 2, 0, 0, H_glVertex2s);
  GL_DEFINE_PROCEDURE(glVertex3d, gxg_glVertex3d_w, 3, 0, 0, H_glVertex3d);
  GL_DEFINE_PROCEDURE(glVertex3f, gxg_glVertex3f_w, 3, 0, 0, H_glVertex3f);
  GL_DEFINE_PROCEDURE(glVertex3i, gxg_glVertex3i_w, 3, 0, 0, H_glVertex3i);
  GL_DEFINE_PROCEDURE(glVertex3s, gxg_glVertex3s_w, 3, 0, 0, H_glVertex3s);
  GL_DEFINE_PROCEDURE(glVertex4d, gxg_glVertex4d_w, 4, 0, 0, H_glVertex4d);
  GL_DEFINE_PROCEDURE(glVertex4f, gxg_glVertex4f_w, 4, 0, 0, H_glVertex4f);
  GL_DEFINE_PROCEDURE(glVertex4i, gxg_glVertex4i_w, 4, 0, 0, H_glVertex4i);
  GL_DEFINE_PROCEDURE(glVertex4s, gxg_glVertex4s_w, 4, 0, 0, H_glVertex4s);
  GL_DEFINE_PROCEDURE(glNormal3b, gxg_glNormal3b_w, 3, 0, 0, H_glNormal3b);
  GL_DEFINE_PROCEDURE(glNormal3d, gxg_glNormal3d_w, 3, 0, 0, H_glNormal3d);
  GL_DEFINE_PROCEDURE(glNormal3f, gxg_glNormal3f_w, 3, 0, 0, H_glNormal3f);
  GL_DEFINE_PROCEDURE(glNormal3i, gxg_glNormal3i_w, 3, 0, 0, H_glNormal3i);
  GL_DEFINE_PROCEDURE(glNormal3s, gxg_glNormal3s_w, 3, 0, 0, H_glNormal3s);
  GL_DEFINE_PROCEDURE(glIndexd, gxg_glIndexd_w, 1, 0, 0, H_glIndexd);
  GL_DEFINE_PROCEDURE(glIndexf, gxg_glIndexf_w, 1, 0, 0, H_glIndexf);
  GL_DEFINE_PROCEDURE(glIndexi, gxg_glIndexi_w, 1, 0, 0, H_glIndexi);
  GL_DEFINE_PROCEDURE(glIndexs, gxg_glIndexs_w, 1, 0, 0, H_glIndexs);
  GL_DEFINE_PROCEDURE(glIndexub, gxg_glIndexub_w, 1, 0, 0, H_glIndexub);
  GL_DEFINE_PROCEDURE(glColor3b, gxg_glColor3b_w, 3, 0, 0, H_glColor3b);
  GL_DEFINE_PROCEDURE(glColor3d, gxg_glColor3d_w, 3, 0, 0, H_glColor3d);
  GL_DEFINE_PROCEDURE(glColor3f, gxg_glColor3f_w, 3, 0, 0, H_glColor3f);
  GL_DEFINE_PROCEDURE(glColor3i, gxg_glColor3i_w, 3, 0, 0, H_glColor3i);
  GL_DEFINE_PROCEDURE(glColor3s, gxg_glColor3s_w, 3, 0, 0, H_glColor3s);
  GL_DEFINE_PROCEDURE(glColor3ub, gxg_glColor3ub_w, 3, 0, 0, H_glColor3ub);
  GL_DEFINE_PROCEDURE(glColor3ui, gxg_glColor3ui_w, 3, 0, 0, H_glColor3ui);
  GL_DEFINE_PROCEDURE(glColor3us, gxg_glColor3us_w, 3, 0, 0, H_glColor3us);
  GL_DEFINE_PROCEDURE(glColor4b, gxg_glColor4b_w, 4, 0, 0, H_glColor4b);
  GL_DEFINE_PROCEDURE(glColor4d, gxg_glColor4d_w, 4, 0, 0, H_glColor4d);
  GL_DEFINE_PROCEDURE(glColor4f, gxg_glColor4f_w, 4, 0, 0, H_glColor4f);
  GL_DEFINE_PROCEDURE(glColor4i, gxg_glColor4i_w, 4, 0, 0, H_glColor4i);
  GL_DEFINE_PROCEDURE(glColor4s, gxg_glColor4s_w, 4, 0, 0, H_glColor4s);
  GL_DEFINE_PROCEDURE(glColor4ub, gxg_glColor4ub_w, 4, 0, 0, H_glColor4ub);
  GL_DEFINE_PROCEDURE(glColor4ui, gxg_glColor4ui_w, 4, 0, 0, H_glColor4ui);
  GL_DEFINE_PROCEDURE(glColor4us, gxg_glColor4us_w, 4, 0, 0, H_glColor4us);
  GL_DEFINE_PROCEDURE(glTexCoord1d, gxg_glTexCoord1d_w, 1, 0, 0, H_glTexCoord1d);
  GL_DEFINE_PROCEDURE(glTexCoord1f, gxg_glTexCoord1f_w, 1, 0, 0, H_glTexCoord1f);
  GL_DEFINE_PROCEDURE(glTexCoord1i, gxg_glTexCoord1i_w, 1, 0, 0, H_glTexCoord1i);
  GL_DEFINE_PROCEDURE(glTexCoord1s, gxg_glTexCoord1s_w, 1, 0, 0, H_glTexCoord1s);
  GL_DEFINE_PROCEDURE(glTexCoord2d, gxg_glTexCoord2d_w, 2, 0, 0, H_glTexCoord2d);
  GL_DEFINE_PROCEDURE(glTexCoord2f, gxg_glTexCoord2f_w, 2, 0, 0, H_glTexCoord2f);
  GL_DEFINE_PROCEDURE(glTexCoord2i, gxg_glTexCoord2i_w, 2, 0, 0, H_glTexCoord2i);
  GL_DEFINE_PROCEDURE(glTexCoord2s, gxg_glTexCoord2s_w, 2, 0, 0, H_glTexCoord2s);
  GL_DEFINE_PROCEDURE(glTexCoord3d, gxg_glTexCoord3d_w, 3, 0, 0, H_glTexCoord3d);
  GL_DEFINE_PROCEDURE(glTexCoord3f, gxg_glTexCoord3f_w, 3, 0, 0, H_glTexCoord3f);
  GL_DEFINE_PROCEDURE(glTexCoord3i, gxg_glTexCoord3i_w, 3, 0, 0, H_glTexCoord3i);
  GL_DEFINE_PROCEDURE(glTexCoord3s, gxg_glTexCoord3s_w, 3, 0, 0, H_glTexCoord3s);
  GL_DEFINE_PROCEDURE(glTexCoord4d, gxg_glTexCoord4d_w, 4, 0, 0, H_glTexCoord4d);
  GL_DEFINE_PROCEDURE(glTexCoord4f, gxg_glTexCoord4f_w, 4, 0, 0, H_glTexCoord4f);
  GL_DEFINE_PROCEDURE(glTexCoord4i, gxg_glTexCoord4i_w, 4, 0, 0, H_glTexCoord4i);
  GL_DEFINE_PROCEDURE(glTexCoord4s, gxg_glTexCoord4s_w, 4, 0, 0, H_glTexCoord4s);
  GL_DEFINE_PROCEDURE(glRasterPos2d, gxg_glRasterPos2d_w, 2, 0, 0, H_glRasterPos2d);
  GL_DEFINE_PROCEDURE(glRasterPos2f, gxg_glRasterPos2f_w, 2, 0, 0, H_glRasterPos2f);
  GL_DEFINE_PROCEDURE(glRasterPos2i, gxg_glRasterPos2i_w, 2, 0, 0, H_glRasterPos2i);
  GL_DEFINE_PROCEDURE(glRasterPos2s, gxg_glRasterPos2s_w, 2, 0, 0, H_glRasterPos2s);
  GL_DEFINE_PROCEDURE(glRasterPos3d, gxg_glRasterPos3d_w, 3, 0, 0, H_glRasterPos3d);
  GL_DEFINE_PROCEDURE(glRasterPos3f, gxg_glRasterPos3f_w, 3, 0, 0, H_glRasterPos3f);
  GL_DEFINE_PROCEDURE(glRasterPos3i, gxg_glRasterPos3i_w, 3, 0, 0, H_glRasterPos3i);
  GL_DEFINE_PROCEDURE(glRasterPos3s, gxg_glRasterPos3s_w, 3, 0, 0, H_glRasterPos3s);
  GL_DEFINE_PROCEDURE(glRasterPos4d, gxg_glRasterPos4d_w, 4, 0, 0, H_glRasterPos4d);
  GL_DEFINE_PROCEDURE(glRasterPos4f, gxg_glRasterPos4f_w, 4, 0, 0, H_glRasterPos4f);
  GL_DEFINE_PROCEDURE(glRasterPos4i, gxg_glRasterPos4i_w, 4, 0, 0, H_glRasterPos4i);
  GL_DEFINE_PROCEDURE(glRasterPos4s, gxg_glRasterPos4s_w, 4, 0, 0, H_glRasterPos4s);
  GL_DEFINE_PROCEDURE(glRectd, gxg_glRectd_w, 4, 0, 0, H_glRectd);
  GL_DEFINE_PROCEDURE(glRectf, gxg_glRectf_w, 4, 0, 0, H_glRectf);
  GL_DEFINE_PROCEDURE(glRecti, gxg_glRecti_w, 4, 0, 0, H_glRecti);
  GL_DEFINE_PROCEDURE(glRects, gxg_glRects_w, 4, 0, 0, H_glRects);
  GL_DEFINE_PROCEDURE(glVertexPointer, gxg_glVertexPointer_w, 4, 0, 0, H_glVertexPointer);
  GL_DEFINE_PROCEDURE(glNormalPointer, gxg_glNormalPointer_w, 3, 0, 0, H_glNormalPointer);
  GL_DEFINE_PROCEDURE(glColorPointer, gxg_glColorPointer_w, 4, 0, 0, H_glColorPointer);
  GL_DEFINE_PROCEDURE(glIndexPointer, gxg_glIndexPointer_w, 3, 0, 0, H_glIndexPointer);
  GL_DEFINE_PROCEDURE(glTexCoordPointer, gxg_glTexCoordPointer_w, 4, 0, 0, H_glTexCoordPointer);
  GL_DEFINE_PROCEDURE(glEdgeFlagPointer, gxg_glEdgeFlagPointer_w, 2, 0, 0, H_glEdgeFlagPointer);
  GL_DEFINE_PROCEDURE(glGetPointerv, gxg_glGetPointerv_w, 1, 1, 0, H_glGetPointerv);
  GL_DEFINE_PROCEDURE(glArrayElement, gxg_glArrayElement_w, 1, 0, 0, H_glArrayElement);
  GL_DEFINE_PROCEDURE(glDrawArrays, gxg_glDrawArrays_w, 3, 0, 0, H_glDrawArrays);
  GL_DEFINE_PROCEDURE(glDrawElements, gxg_glDrawElements_w, 4, 0, 0, H_glDrawElements);
  GL_DEFINE_PROCEDURE(glInterleavedArrays, gxg_glInterleavedArrays_w, 3, 0, 0, H_glInterleavedArrays);
  GL_DEFINE_PROCEDURE(glShadeModel, gxg_glShadeModel_w, 1, 0, 0, H_glShadeModel);
  GL_DEFINE_PROCEDURE(glLightf, gxg_glLightf_w, 3, 0, 0, H_glLightf);
  GL_DEFINE_PROCEDURE(glLighti, gxg_glLighti_w, 3, 0, 0, H_glLighti);
  GL_DEFINE_PROCEDURE(glGetLightfv, gxg_glGetLightfv_w, 2, 1, 0, H_glGetLightfv);
  GL_DEFINE_PROCEDURE(glGetLightiv, gxg_glGetLightiv_w, 2, 1, 0, H_glGetLightiv);
  GL_DEFINE_PROCEDURE(glLightModelf, gxg_glLightModelf_w, 2, 0, 0, H_glLightModelf);
  GL_DEFINE_PROCEDURE(glLightModeli, gxg_glLightModeli_w, 2, 0, 0, H_glLightModeli);
  GL_DEFINE_PROCEDURE(glMaterialf, gxg_glMaterialf_w, 3, 0, 0, H_glMaterialf);
  GL_DEFINE_PROCEDURE(glMateriali, gxg_glMateriali_w, 3, 0, 0, H_glMateriali);
  GL_DEFINE_PROCEDURE(glGetMaterialfv, gxg_glGetMaterialfv_w, 2, 1, 0, H_glGetMaterialfv);
  GL_DEFINE_PROCEDURE(glGetMaterialiv, gxg_glGetMaterialiv_w, 2, 1, 0, H_glGetMaterialiv);
  GL_DEFINE_PROCEDURE(glColorMaterial, gxg_glColorMaterial_w, 2, 0, 0, H_glColorMaterial);
  GL_DEFINE_PROCEDURE(glPixelZoom, gxg_glPixelZoom_w, 2, 0, 0, H_glPixelZoom);
  GL_DEFINE_PROCEDURE(glPixelStoref, gxg_glPixelStoref_w, 2, 0, 0, H_glPixelStoref);
  GL_DEFINE_PROCEDURE(glPixelStorei, gxg_glPixelStorei_w, 2, 0, 0, H_glPixelStorei);
  GL_DEFINE_PROCEDURE(glPixelTransferf, gxg_glPixelTransferf_w, 2, 0, 0, H_glPixelTransferf);
  GL_DEFINE_PROCEDURE(glPixelTransferi, gxg_glPixelTransferi_w, 2, 0, 0, H_glPixelTransferi);
  GL_DEFINE_PROCEDURE(glGetPixelMapfv, gxg_glGetPixelMapfv_w, 1, 1, 0, H_glGetPixelMapfv);
  GL_DEFINE_PROCEDURE(glGetPixelMapuiv, gxg_glGetPixelMapuiv_w, 1, 1, 0, H_glGetPixelMapuiv);
  GL_DEFINE_PROCEDURE(glGetPixelMapusv, gxg_glGetPixelMapusv_w, 1, 1, 0, H_glGetPixelMapusv);
  GL_DEFINE_PROCEDURE(glBitmap, gxg_glBitmap_w, 7, 0, 0, H_glBitmap);
  GL_DEFINE_PROCEDURE(glReadPixels, gxg_glReadPixels_w, 7, 0, 0, H_glReadPixels);
  GL_DEFINE_PROCEDURE(glDrawPixels, gxg_glDrawPixels_w, 5, 0, 0, H_glDrawPixels);
  GL_DEFINE_PROCEDURE(glCopyPixels, gxg_glCopyPixels_w, 5, 0, 0, H_glCopyPixels);
  GL_DEFINE_PROCEDURE(glStencilFunc, gxg_glStencilFunc_w, 3, 0, 0, H_glStencilFunc);
  GL_DEFINE_PROCEDURE(glStencilMask, gxg_glStencilMask_w, 1, 0, 0, H_glStencilMask);
  GL_DEFINE_PROCEDURE(glStencilOp, gxg_glStencilOp_w, 3, 0, 0, H_glStencilOp);
  GL_DEFINE_PROCEDURE(glClearStencil, gxg_glClearStencil_w, 1, 0, 0, H_glClearStencil);
  GL_DEFINE_PROCEDURE(glTexGend, gxg_glTexGend_w, 3, 0, 0, H_glTexGend);
  GL_DEFINE_PROCEDURE(glTexGenf, gxg_glTexGenf_w, 3, 0, 0, H_glTexGenf);
  GL_DEFINE_PROCEDURE(glTexGeni, gxg_glTexGeni_w, 3, 0, 0, H_glTexGeni);
  GL_DEFINE_PROCEDURE(glGetTexGendv, gxg_glGetTexGendv_w, 2, 1, 0, H_glGetTexGendv);
  GL_DEFINE_PROCEDURE(glGetTexGenfv, gxg_glGetTexGenfv_w, 2, 1, 0, H_glGetTexGenfv);
  GL_DEFINE_PROCEDURE(glGetTexGeniv, gxg_glGetTexGeniv_w, 2, 1, 0, H_glGetTexGeniv);
  GL_DEFINE_PROCEDURE(glTexEnvf, gxg_glTexEnvf_w, 3, 0, 0, H_glTexEnvf);
  GL_DEFINE_PROCEDURE(glTexEnvi, gxg_glTexEnvi_w, 3, 0, 0, H_glTexEnvi);
  GL_DEFINE_PROCEDURE(glGetTexEnvfv, gxg_glGetTexEnvfv_w, 2, 1, 0, H_glGetTexEnvfv);
  GL_DEFINE_PROCEDURE(glGetTexEnviv, gxg_glGetTexEnviv_w, 2, 1, 0, H_glGetTexEnviv);
  GL_DEFINE_PROCEDURE(glTexParameterf, gxg_glTexParameterf_w, 3, 0, 0, H_glTexParameterf);
  GL_DEFINE_PROCEDURE(glTexParameteri, gxg_glTexParameteri_w, 3, 0, 0, H_glTexParameteri);
  GL_DEFINE_PROCEDURE(glGetTexParameterfv, gxg_glGetTexParameterfv_w, 2, 1, 0, H_glGetTexParameterfv);
  GL_DEFINE_PROCEDURE(glGetTexParameteriv, gxg_glGetTexParameteriv_w, 2, 1, 0, H_glGetTexParameteriv);
  GL_DEFINE_PROCEDURE(glGetTexLevelParameterfv, gxg_glGetTexLevelParameterfv_w, 3, 1, 0, H_glGetTexLevelParameterfv);
  GL_DEFINE_PROCEDURE(glGetTexLevelParameteriv, gxg_glGetTexLevelParameteriv_w, 3, 1, 0, H_glGetTexLevelParameteriv);
  GL_DEFINE_PROCEDURE(glTexImage1D, gxg_glTexImage1D_w, 8, 0, 0, H_glTexImage1D);
  GL_DEFINE_PROCEDURE(glTexImage2D, gxg_glTexImage2D_w, 9, 0, 0, H_glTexImage2D);
  GL_DEFINE_PROCEDURE(glGenTextures, gxg_glGenTextures_w, 2, 0, 0, H_glGenTextures);
  GL_DEFINE_PROCEDURE(glDeleteTextures, gxg_glDeleteTextures_w, 2, 0, 0, H_glDeleteTextures);
  GL_DEFINE_PROCEDURE(glBindTexture, gxg_glBindTexture_w, 2, 0, 0, H_glBindTexture);
  GL_DEFINE_PROCEDURE(glAreTexturesResident, gxg_glAreTexturesResident_w, 3, 0, 0, H_glAreTexturesResident);
  GL_DEFINE_PROCEDURE(glIsTexture, gxg_glIsTexture_w, 1, 0, 0, H_glIsTexture);
  GL_DEFINE_PROCEDURE(glTexSubImage1D, gxg_glTexSubImage1D_w, 7, 0, 0, H_glTexSubImage1D);
  GL_DEFINE_PROCEDURE(glTexSubImage2D, gxg_glTexSubImage2D_w, 9, 0, 0, H_glTexSubImage2D);
  GL_DEFINE_PROCEDURE(glCopyTexImage1D, gxg_glCopyTexImage1D_w, 7, 0, 0, H_glCopyTexImage1D);
  GL_DEFINE_PROCEDURE(glCopyTexImage2D, gxg_glCopyTexImage2D_w, 8, 0, 0, H_glCopyTexImage2D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage1D, gxg_glCopyTexSubImage1D_w, 6, 0, 0, H_glCopyTexSubImage1D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage2D, gxg_glCopyTexSubImage2D_w, 8, 0, 0, H_glCopyTexSubImage2D);
  GL_DEFINE_PROCEDURE(glMap1d, gxg_glMap1d_w, 6, 0, 0, H_glMap1d);
  GL_DEFINE_PROCEDURE(glMap1f, gxg_glMap1f_w, 6, 0, 0, H_glMap1f);
  GL_DEFINE_PROCEDURE(glMap2d, gxg_glMap2d_w, 0, 0, 1, H_glMap2d);
  GL_DEFINE_PROCEDURE(glMap2f, gxg_glMap2f_w, 0, 0, 1, H_glMap2f);
  GL_DEFINE_PROCEDURE(glGetMapdv, gxg_glGetMapdv_w, 2, 1, 0, H_glGetMapdv);
  GL_DEFINE_PROCEDURE(glGetMapfv, gxg_glGetMapfv_w, 2, 1, 0, H_glGetMapfv);
  GL_DEFINE_PROCEDURE(glGetMapiv, gxg_glGetMapiv_w, 2, 1, 0, H_glGetMapiv);
  GL_DEFINE_PROCEDURE(glEvalCoord1d, gxg_glEvalCoord1d_w, 1, 0, 0, H_glEvalCoord1d);
  GL_DEFINE_PROCEDURE(glEvalCoord1f, gxg_glEvalCoord1f_w, 1, 0, 0, H_glEvalCoord1f);
  GL_DEFINE_PROCEDURE(glEvalCoord2d, gxg_glEvalCoord2d_w, 2, 0, 0, H_glEvalCoord2d);
  GL_DEFINE_PROCEDURE(glEvalCoord2f, gxg_glEvalCoord2f_w, 2, 0, 0, H_glEvalCoord2f);
  GL_DEFINE_PROCEDURE(glMapGrid1d, gxg_glMapGrid1d_w, 3, 0, 0, H_glMapGrid1d);
  GL_DEFINE_PROCEDURE(glMapGrid1f, gxg_glMapGrid1f_w, 3, 0, 0, H_glMapGrid1f);
  GL_DEFINE_PROCEDURE(glMapGrid2d, gxg_glMapGrid2d_w, 6, 0, 0, H_glMapGrid2d);
  GL_DEFINE_PROCEDURE(glMapGrid2f, gxg_glMapGrid2f_w, 6, 0, 0, H_glMapGrid2f);
  GL_DEFINE_PROCEDURE(glEvalPoint1, gxg_glEvalPoint1_w, 1, 0, 0, H_glEvalPoint1);
  GL_DEFINE_PROCEDURE(glEvalPoint2, gxg_glEvalPoint2_w, 2, 0, 0, H_glEvalPoint2);
  GL_DEFINE_PROCEDURE(glEvalMesh1, gxg_glEvalMesh1_w, 3, 0, 0, H_glEvalMesh1);
  GL_DEFINE_PROCEDURE(glEvalMesh2, gxg_glEvalMesh2_w, 5, 0, 0, H_glEvalMesh2);
  GL_DEFINE_PROCEDURE(glFogf, gxg_glFogf_w, 2, 0, 0, H_glFogf);
  GL_DEFINE_PROCEDURE(glFogi, gxg_glFogi_w, 2, 0, 0, H_glFogi);
  GL_DEFINE_PROCEDURE(glFeedbackBuffer, gxg_glFeedbackBuffer_w, 3, 0, 0, H_glFeedbackBuffer);
  GL_DEFINE_PROCEDURE(glPassThrough, gxg_glPassThrough_w, 1, 0, 0, H_glPassThrough);
  GL_DEFINE_PROCEDURE(glSelectBuffer, gxg_glSelectBuffer_w, 2, 0, 0, H_glSelectBuffer);
  GL_DEFINE_PROCEDURE(glInitNames, gxg_glInitNames_w, 0, 0, 0, H_glInitNames);
  GL_DEFINE_PROCEDURE(glLoadName, gxg_glLoadName_w, 1, 0, 0, H_glLoadName);
  GL_DEFINE_PROCEDURE(glPushName, gxg_glPushName_w, 1, 0, 0, H_glPushName);
  GL_DEFINE_PROCEDURE(glPopName, gxg_glPopName_w, 0, 0, 0, H_glPopName);
  GL_DEFINE_PROCEDURE(glDrawRangeElements, gxg_glDrawRangeElements_w, 6, 0, 0, H_glDrawRangeElements);
  GL_DEFINE_PROCEDURE(glTexImage3D, gxg_glTexImage3D_w, 0, 0, 1, H_glTexImage3D);
  GL_DEFINE_PROCEDURE(glTexSubImage3D, gxg_glTexSubImage3D_w, 0, 0, 1, H_glTexSubImage3D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage3D, gxg_glCopyTexSubImage3D_w, 9, 0, 0, H_glCopyTexSubImage3D);
  GL_DEFINE_PROCEDURE(glColorTable, gxg_glColorTable_w, 6, 0, 0, H_glColorTable);
  GL_DEFINE_PROCEDURE(glColorSubTable, gxg_glColorSubTable_w, 6, 0, 0, H_glColorSubTable);
  GL_DEFINE_PROCEDURE(glCopyColorSubTable, gxg_glCopyColorSubTable_w, 5, 0, 0, H_glCopyColorSubTable);
  GL_DEFINE_PROCEDURE(glCopyColorTable, gxg_glCopyColorTable_w, 5, 0, 0, H_glCopyColorTable);
  GL_DEFINE_PROCEDURE(glGetColorTableParameterfv, gxg_glGetColorTableParameterfv_w, 2, 1, 0, H_glGetColorTableParameterfv);
  GL_DEFINE_PROCEDURE(glGetColorTableParameteriv, gxg_glGetColorTableParameteriv_w, 2, 1, 0, H_glGetColorTableParameteriv);
  GL_DEFINE_PROCEDURE(glBlendEquation, gxg_glBlendEquation_w, 1, 0, 0, H_glBlendEquation);
  GL_DEFINE_PROCEDURE(glBlendColor, gxg_glBlendColor_w, 4, 0, 0, H_glBlendColor);
  GL_DEFINE_PROCEDURE(glHistogram, gxg_glHistogram_w, 4, 0, 0, H_glHistogram);
  GL_DEFINE_PROCEDURE(glResetHistogram, gxg_glResetHistogram_w, 1, 0, 0, H_glResetHistogram);
  GL_DEFINE_PROCEDURE(glGetHistogram, gxg_glGetHistogram_w, 5, 0, 0, H_glGetHistogram);
  GL_DEFINE_PROCEDURE(glGetHistogramParameterfv, gxg_glGetHistogramParameterfv_w, 2, 1, 0, H_glGetHistogramParameterfv);
  GL_DEFINE_PROCEDURE(glGetHistogramParameteriv, gxg_glGetHistogramParameteriv_w, 2, 1, 0, H_glGetHistogramParameteriv);
  GL_DEFINE_PROCEDURE(glMinmax, gxg_glMinmax_w, 3, 0, 0, H_glMinmax);
  GL_DEFINE_PROCEDURE(glResetMinmax, gxg_glResetMinmax_w, 1, 0, 0, H_glResetMinmax);
  GL_DEFINE_PROCEDURE(glGetMinmax, gxg_glGetMinmax_w, 5, 0, 0, H_glGetMinmax);
  GL_DEFINE_PROCEDURE(glGetMinmaxParameterfv, gxg_glGetMinmaxParameterfv_w, 2, 1, 0, H_glGetMinmaxParameterfv);
  GL_DEFINE_PROCEDURE(glGetMinmaxParameteriv, gxg_glGetMinmaxParameteriv_w, 2, 1, 0, H_glGetMinmaxParameteriv);
  GL_DEFINE_PROCEDURE(glConvolutionFilter1D, gxg_glConvolutionFilter1D_w, 6, 0, 0, H_glConvolutionFilter1D);
  GL_DEFINE_PROCEDURE(glConvolutionFilter2D, gxg_glConvolutionFilter2D_w, 7, 0, 0, H_glConvolutionFilter2D);
  GL_DEFINE_PROCEDURE(glConvolutionParameterf, gxg_glConvolutionParameterf_w, 3, 0, 0, H_glConvolutionParameterf);
  GL_DEFINE_PROCEDURE(glConvolutionParameteri, gxg_glConvolutionParameteri_w, 3, 0, 0, H_glConvolutionParameteri);
  GL_DEFINE_PROCEDURE(glCopyConvolutionFilter1D, gxg_glCopyConvolutionFilter1D_w, 5, 0, 0, H_glCopyConvolutionFilter1D);
  GL_DEFINE_PROCEDURE(glCopyConvolutionFilter2D, gxg_glCopyConvolutionFilter2D_w, 6, 0, 0, H_glCopyConvolutionFilter2D);
  GL_DEFINE_PROCEDURE(glSeparableFilter2D, gxg_glSeparableFilter2D_w, 8, 0, 0, H_glSeparableFilter2D);
#if HAVE_GLU
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluBeginPolygon, gxg_gluBeginPolygon_w, 1, 0, 0, H_gluBeginPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluBuild1DMipmaps, gxg_gluBuild1DMipmaps_w, 6, 0, 0, H_gluBuild1DMipmaps);
  GL_DEFINE_PROCEDURE(gluBuild2DMipmaps, gxg_gluBuild2DMipmaps_w, 7, 0, 0, H_gluBuild2DMipmaps);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluDeleteTess, gxg_gluDeleteTess_w, 1, 0, 0, H_gluDeleteTess);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluEndPolygon, gxg_gluEndPolygon_w, 1, 0, 0, H_gluEndPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluErrorString, gxg_gluErrorString_w, 1, 0, 0, H_gluErrorString);
  GL_DEFINE_PROCEDURE(gluGetString, gxg_gluGetString_w, 1, 0, 0, H_gluGetString);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluGetTessProperty, gxg_gluGetTessProperty_w, 3, 0, 0, H_gluGetTessProperty);
#endif
  GL_DEFINE_PROCEDURE(gluLookAt, gxg_gluLookAt_w, 9, 0, 0, H_gluLookAt);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNewTess, gxg_gluNewTess_w, 0, 0, 0, H_gluNewTess);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNextContour, gxg_gluNextContour_w, 2, 0, 0, H_gluNextContour);
#endif
  GL_DEFINE_PROCEDURE(gluOrtho2D, gxg_gluOrtho2D_w, 4, 0, 0, H_gluOrtho2D);
  GL_DEFINE_PROCEDURE(gluPerspective, gxg_gluPerspective_w, 4, 0, 0, H_gluPerspective);
  GL_DEFINE_PROCEDURE(gluPickMatrix, gxg_gluPickMatrix_w, 5, 0, 0, H_gluPickMatrix);
  GL_DEFINE_PROCEDURE(gluProject, gxg_gluProject_w, 9, 0, 0, H_gluProject);
  GL_DEFINE_PROCEDURE(gluScaleImage, gxg_gluScaleImage_w, 9, 0, 0, H_gluScaleImage);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginContour, gxg_gluTessBeginContour_w, 1, 0, 0, H_gluTessBeginContour);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginPolygon, gxg_gluTessBeginPolygon_w, 2, 0, 0, H_gluTessBeginPolygon);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessEndContour, gxg_gluTessEndContour_w, 1, 0, 0, H_gluTessEndContour);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessEndPolygon, gxg_gluTessEndPolygon_w, 1, 0, 0, H_gluTessEndPolygon);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessNormal, gxg_gluTessNormal_w, 4, 0, 0, H_gluTessNormal);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessProperty, gxg_gluTessProperty_w, 3, 0, 0, H_gluTessProperty);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessVertex, gxg_gluTessVertex_w, 3, 0, 0, H_gluTessVertex);
#endif
  GL_DEFINE_PROCEDURE(gluUnProject, gxg_gluUnProject_w, 9, 0, 0, H_gluUnProject);
#endif
}

/* ---------------------------------------- constants ---------------------------------------- */

static void define_integers(void)
{

#define DEFINE_INTEGER(Name) XEN_DEFINE(XL_PRE #Name XL_POST, C_TO_XEN_INT(Name))

#if USE_MOTIF
  DEFINE_INTEGER(GLX_USE_GL);
  DEFINE_INTEGER(GLX_BUFFER_SIZE);
  DEFINE_INTEGER(GLX_LEVEL);
  DEFINE_INTEGER(GLX_RGBA);
  DEFINE_INTEGER(GLX_DOUBLEBUFFER);
  DEFINE_INTEGER(GLX_STEREO);
  DEFINE_INTEGER(GLX_AUX_BUFFERS);
  DEFINE_INTEGER(GLX_RED_SIZE);
  DEFINE_INTEGER(GLX_GREEN_SIZE);
  DEFINE_INTEGER(GLX_BLUE_SIZE);
  DEFINE_INTEGER(GLX_ALPHA_SIZE);
  DEFINE_INTEGER(GLX_DEPTH_SIZE);
  DEFINE_INTEGER(GLX_STENCIL_SIZE);
  DEFINE_INTEGER(GLX_ACCUM_RED_SIZE);
  DEFINE_INTEGER(GLX_ACCUM_GREEN_SIZE);
  DEFINE_INTEGER(GLX_ACCUM_BLUE_SIZE);
  DEFINE_INTEGER(GLX_ACCUM_ALPHA_SIZE);
  DEFINE_INTEGER(GLX_BAD_SCREEN);
  DEFINE_INTEGER(GLX_BAD_ATTRIBUTE);
  DEFINE_INTEGER(GLX_NO_EXTENSION);
  DEFINE_INTEGER(GLX_BAD_VISUAL);
  DEFINE_INTEGER(GLX_BAD_CONTEXT);
  DEFINE_INTEGER(GLX_BAD_VALUE);
  DEFINE_INTEGER(GLX_BAD_ENUM);
  DEFINE_INTEGER(GLX_VENDOR);
  DEFINE_INTEGER(GLX_VERSION);
  DEFINE_INTEGER(GLX_EXTENSIONS);
#endif
#if USE_GTK
  DEFINE_INTEGER(GDK_GL_SUCCESS);
  DEFINE_INTEGER(GDK_GL_ATTRIB_LIST_NONE);
  DEFINE_INTEGER(GDK_GL_USE_GL);
  DEFINE_INTEGER(GDK_GL_BUFFER_SIZE);
  DEFINE_INTEGER(GDK_GL_LEVEL);
  DEFINE_INTEGER(GDK_GL_RGBA);
  DEFINE_INTEGER(GDK_GL_DOUBLEBUFFER);
  DEFINE_INTEGER(GDK_GL_STEREO);
  DEFINE_INTEGER(GDK_GL_AUX_BUFFERS);
  DEFINE_INTEGER(GDK_GL_RED_SIZE);
  DEFINE_INTEGER(GDK_GL_GREEN_SIZE);
  DEFINE_INTEGER(GDK_GL_BLUE_SIZE);
  DEFINE_INTEGER(GDK_GL_ALPHA_SIZE);
  DEFINE_INTEGER(GDK_GL_DEPTH_SIZE);
  DEFINE_INTEGER(GDK_GL_STENCIL_SIZE);
  DEFINE_INTEGER(GDK_GL_ACCUM_RED_SIZE);
  DEFINE_INTEGER(GDK_GL_ACCUM_GREEN_SIZE);
  DEFINE_INTEGER(GDK_GL_ACCUM_BLUE_SIZE);
  DEFINE_INTEGER(GDK_GL_ACCUM_ALPHA_SIZE);
  DEFINE_INTEGER(GDK_GL_X_VISUAL_TYPE);
  DEFINE_INTEGER(GDK_GL_CONFIG_CAVEAT);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_TYPE);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_INDEX_VALUE);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_RED_VALUE);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_GREEN_VALUE);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_BLUE_VALUE);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_ALPHA_VALUE);
  DEFINE_INTEGER(GDK_GL_DRAWABLE_TYPE);
  DEFINE_INTEGER(GDK_GL_RENDER_TYPE);
  DEFINE_INTEGER(GDK_GL_X_RENDERABLE);
  DEFINE_INTEGER(GDK_GL_FBCONFIG_ID);
  DEFINE_INTEGER(GDK_GL_MAX_PBUFFER_WIDTH);
  DEFINE_INTEGER(GDK_GL_MAX_PBUFFER_HEIGHT);
  DEFINE_INTEGER(GDK_GL_MAX_PBUFFER_PIXELS);
  DEFINE_INTEGER(GDK_GL_VISUAL_ID);
  DEFINE_INTEGER(GDK_GL_BAD_SCREEN);
  DEFINE_INTEGER(GDK_GL_BAD_ATTRIBUTE);
  DEFINE_INTEGER(GDK_GL_NO_EXTENSION);
  DEFINE_INTEGER(GDK_GL_BAD_VISUAL);
  DEFINE_INTEGER(GDK_GL_BAD_CONTEXT);
  DEFINE_INTEGER(GDK_GL_BAD_VALUE);
  DEFINE_INTEGER(GDK_GL_BAD_ENUM);
  DEFINE_INTEGER(GDK_GL_DONT_CARE);
  DEFINE_INTEGER(GDK_GL_RGBA_BIT);
  DEFINE_INTEGER(GDK_GL_COLOR_INDEX_BIT);
  DEFINE_INTEGER(GDK_GL_WINDOW_BIT);
  DEFINE_INTEGER(GDK_GL_PIXMAP_BIT);
  DEFINE_INTEGER(GDK_GL_PBUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_NONE);
  DEFINE_INTEGER(GDK_GL_SLOW_CONFIG);
  DEFINE_INTEGER(GDK_GL_NON_CONFORMANT_CONFIG);
  DEFINE_INTEGER(GDK_GL_TRUE_COLOR);
  DEFINE_INTEGER(GDK_GL_DIRECT_COLOR);
  DEFINE_INTEGER(GDK_GL_PSEUDO_COLOR);
  DEFINE_INTEGER(GDK_GL_STATIC_COLOR);
  DEFINE_INTEGER(GDK_GL_GRAY_SCALE);
  DEFINE_INTEGER(GDK_GL_STATIC_GRAY);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_RGB);
  DEFINE_INTEGER(GDK_GL_TRANSPARENT_INDEX);
  DEFINE_INTEGER(GDK_GL_PRESERVED_CONTENTS);
  DEFINE_INTEGER(GDK_GL_LARGEST_PBUFFER);
  DEFINE_INTEGER(GDK_GL_PBUFFER_HEIGHT);
  DEFINE_INTEGER(GDK_GL_PBUFFER_WIDTH);
  DEFINE_INTEGER(GDK_GL_WIDTH);
  DEFINE_INTEGER(GDK_GL_HEIGHT);
  DEFINE_INTEGER(GDK_GL_EVENT_MASK);
  DEFINE_INTEGER(GDK_GL_RGBA_TYPE);
  DEFINE_INTEGER(GDK_GL_COLOR_INDEX_TYPE);
  DEFINE_INTEGER(GDK_GL_SCREEN);
  DEFINE_INTEGER(GDK_GL_PBUFFER_CLOBBER_MASK);
  DEFINE_INTEGER(GDK_GL_DAMAGED);
  DEFINE_INTEGER(GDK_GL_SAVED);
  DEFINE_INTEGER(GDK_GL_WINDOW);
  DEFINE_INTEGER(GDK_GL_PBUFFER);
  DEFINE_INTEGER(GDK_GL_FRONT_LEFT_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_FRONT_RIGHT_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_BACK_LEFT_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_BACK_RIGHT_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_AUX_BUFFERS_BIT);
  DEFINE_INTEGER(GDK_GL_DEPTH_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_STENCIL_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_ACCUM_BUFFER_BIT);
  DEFINE_INTEGER(GDK_GL_MODE_RGB);
  DEFINE_INTEGER(GDK_GL_MODE_RGBA);
  DEFINE_INTEGER(GDK_GL_MODE_INDEX);
  DEFINE_INTEGER(GDK_GL_MODE_SINGLE);
  DEFINE_INTEGER(GDK_GL_MODE_DOUBLE);
  DEFINE_INTEGER(GDK_GL_MODE_ACCUM);
  DEFINE_INTEGER(GDK_GL_MODE_ALPHA);
  DEFINE_INTEGER(GDK_GL_MODE_DEPTH);
  DEFINE_INTEGER(GDK_GL_MODE_STENCIL);
  DEFINE_INTEGER(GDK_GL_MODE_STEREO);
  DEFINE_INTEGER(GDK_GL_MODE_MULTISAMPLE);
#ifdef GTKGLEXT_MAJOR_VERSION
  DEFINE_INTEGER(GDKGLEXT_MAJOR_VERSION);
  DEFINE_INTEGER(GDKGLEXT_MINOR_VERSION);
  DEFINE_INTEGER(GDKGLEXT_MICRO_VERSION);
  DEFINE_INTEGER(GDKGLEXT_INTERFACE_AGE);
  DEFINE_INTEGER(GDKGLEXT_BINARY_AGE);
  DEFINE_INTEGER(GDK_GL_SAMPLE_BUFFERS);
  DEFINE_INTEGER(GDK_GL_SAMPLES);
  DEFINE_INTEGER(GTKGLEXT_MAJOR_VERSION);
  DEFINE_INTEGER(GTKGLEXT_MINOR_VERSION);
  DEFINE_INTEGER(GTKGLEXT_MICRO_VERSION);
  DEFINE_INTEGER(GTKGLEXT_INTERFACE_AGE);
  DEFINE_INTEGER(GTKGLEXT_BINARY_AGE);
#endif
#endif
  DEFINE_INTEGER(GL_FALSE);
  DEFINE_INTEGER(GL_TRUE);
  DEFINE_INTEGER(GL_BYTE);
  DEFINE_INTEGER(GL_UNSIGNED_BYTE);
  DEFINE_INTEGER(GL_SHORT);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT);
  DEFINE_INTEGER(GL_INT);
  DEFINE_INTEGER(GL_UNSIGNED_INT);
  DEFINE_INTEGER(GL_FLOAT);
  DEFINE_INTEGER(GL_DOUBLE);
  DEFINE_INTEGER(GL_2_BYTES);
  DEFINE_INTEGER(GL_3_BYTES);
  DEFINE_INTEGER(GL_4_BYTES);
  DEFINE_INTEGER(GL_POINTS);
  DEFINE_INTEGER(GL_LINES);
  DEFINE_INTEGER(GL_LINE_LOOP);
  DEFINE_INTEGER(GL_LINE_STRIP);
  DEFINE_INTEGER(GL_TRIANGLES);
  DEFINE_INTEGER(GL_TRIANGLE_STRIP);
  DEFINE_INTEGER(GL_TRIANGLE_FAN);
  DEFINE_INTEGER(GL_QUADS);
  DEFINE_INTEGER(GL_QUAD_STRIP);
  DEFINE_INTEGER(GL_POLYGON);
  DEFINE_INTEGER(GL_VERTEX_ARRAY);
  DEFINE_INTEGER(GL_NORMAL_ARRAY);
  DEFINE_INTEGER(GL_COLOR_ARRAY);
  DEFINE_INTEGER(GL_INDEX_ARRAY);
  DEFINE_INTEGER(GL_TEXTURE_COORD_ARRAY);
  DEFINE_INTEGER(GL_EDGE_FLAG_ARRAY);
  DEFINE_INTEGER(GL_VERTEX_ARRAY_SIZE);
  DEFINE_INTEGER(GL_VERTEX_ARRAY_TYPE);
  DEFINE_INTEGER(GL_VERTEX_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_NORMAL_ARRAY_TYPE);
  DEFINE_INTEGER(GL_NORMAL_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_COLOR_ARRAY_SIZE);
  DEFINE_INTEGER(GL_COLOR_ARRAY_TYPE);
  DEFINE_INTEGER(GL_COLOR_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_INDEX_ARRAY_TYPE);
  DEFINE_INTEGER(GL_INDEX_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_TEXTURE_COORD_ARRAY_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_COORD_ARRAY_TYPE);
  DEFINE_INTEGER(GL_TEXTURE_COORD_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_EDGE_FLAG_ARRAY_STRIDE);
  DEFINE_INTEGER(GL_VERTEX_ARRAY_POINTER);
  DEFINE_INTEGER(GL_NORMAL_ARRAY_POINTER);
  DEFINE_INTEGER(GL_COLOR_ARRAY_POINTER);
  DEFINE_INTEGER(GL_INDEX_ARRAY_POINTER);
  DEFINE_INTEGER(GL_TEXTURE_COORD_ARRAY_POINTER);
  DEFINE_INTEGER(GL_EDGE_FLAG_ARRAY_POINTER);
  DEFINE_INTEGER(GL_V2F);
  DEFINE_INTEGER(GL_V3F);
  DEFINE_INTEGER(GL_C4UB_V2F);
  DEFINE_INTEGER(GL_C4UB_V3F);
  DEFINE_INTEGER(GL_C3F_V3F);
  DEFINE_INTEGER(GL_N3F_V3F);
  DEFINE_INTEGER(GL_C4F_N3F_V3F);
  DEFINE_INTEGER(GL_T2F_V3F);
  DEFINE_INTEGER(GL_T4F_V4F);
  DEFINE_INTEGER(GL_T2F_C4UB_V3F);
  DEFINE_INTEGER(GL_T2F_C3F_V3F);
  DEFINE_INTEGER(GL_T2F_N3F_V3F);
  DEFINE_INTEGER(GL_T2F_C4F_N3F_V3F);
  DEFINE_INTEGER(GL_T4F_C4F_N3F_V4F);
  DEFINE_INTEGER(GL_MATRIX_MODE);
  DEFINE_INTEGER(GL_MODELVIEW);
  DEFINE_INTEGER(GL_PROJECTION);
  DEFINE_INTEGER(GL_TEXTURE);
  DEFINE_INTEGER(GL_POINT_SMOOTH);
  DEFINE_INTEGER(GL_POINT_SIZE);
  DEFINE_INTEGER(GL_POINT_SIZE_GRANULARITY);
  DEFINE_INTEGER(GL_POINT_SIZE_RANGE);
  DEFINE_INTEGER(GL_LINE_SMOOTH);
  DEFINE_INTEGER(GL_LINE_STIPPLE);
  DEFINE_INTEGER(GL_LINE_STIPPLE_PATTERN);
  DEFINE_INTEGER(GL_LINE_STIPPLE_REPEAT);
  DEFINE_INTEGER(GL_LINE_WIDTH);
  DEFINE_INTEGER(GL_LINE_WIDTH_GRANULARITY);
  DEFINE_INTEGER(GL_LINE_WIDTH_RANGE);
  DEFINE_INTEGER(GL_POINT);
  DEFINE_INTEGER(GL_LINE);
  DEFINE_INTEGER(GL_FILL);
  DEFINE_INTEGER(GL_CW);
  DEFINE_INTEGER(GL_CCW);
  DEFINE_INTEGER(GL_FRONT);
  DEFINE_INTEGER(GL_BACK);
  DEFINE_INTEGER(GL_POLYGON_MODE);
  DEFINE_INTEGER(GL_POLYGON_SMOOTH);
  DEFINE_INTEGER(GL_POLYGON_STIPPLE);
  DEFINE_INTEGER(GL_EDGE_FLAG);
  DEFINE_INTEGER(GL_CULL_FACE);
  DEFINE_INTEGER(GL_CULL_FACE_MODE);
  DEFINE_INTEGER(GL_FRONT_FACE);
  DEFINE_INTEGER(GL_POLYGON_OFFSET_FACTOR);
  DEFINE_INTEGER(GL_POLYGON_OFFSET_UNITS);
  DEFINE_INTEGER(GL_POLYGON_OFFSET_POINT);
  DEFINE_INTEGER(GL_POLYGON_OFFSET_LINE);
  DEFINE_INTEGER(GL_POLYGON_OFFSET_FILL);
  DEFINE_INTEGER(GL_COMPILE);
  DEFINE_INTEGER(GL_COMPILE_AND_EXECUTE);
  DEFINE_INTEGER(GL_LIST_BASE);
  DEFINE_INTEGER(GL_LIST_INDEX);
  DEFINE_INTEGER(GL_LIST_MODE);
  DEFINE_INTEGER(GL_NEVER);
  DEFINE_INTEGER(GL_LESS);
  DEFINE_INTEGER(GL_EQUAL);
  DEFINE_INTEGER(GL_LEQUAL);
  DEFINE_INTEGER(GL_GREATER);
  DEFINE_INTEGER(GL_NOTEQUAL);
  DEFINE_INTEGER(GL_GEQUAL);
  DEFINE_INTEGER(GL_ALWAYS);
  DEFINE_INTEGER(GL_DEPTH_TEST);
  DEFINE_INTEGER(GL_DEPTH_BITS);
  DEFINE_INTEGER(GL_DEPTH_CLEAR_VALUE);
  DEFINE_INTEGER(GL_DEPTH_FUNC);
  DEFINE_INTEGER(GL_DEPTH_RANGE);
  DEFINE_INTEGER(GL_DEPTH_WRITEMASK);
  DEFINE_INTEGER(GL_DEPTH_COMPONENT);
  DEFINE_INTEGER(GL_LIGHTING);
  DEFINE_INTEGER(GL_LIGHT0);
  DEFINE_INTEGER(GL_LIGHT1);
  DEFINE_INTEGER(GL_LIGHT2);
  DEFINE_INTEGER(GL_LIGHT3);
  DEFINE_INTEGER(GL_LIGHT4);
  DEFINE_INTEGER(GL_LIGHT5);
  DEFINE_INTEGER(GL_LIGHT6);
  DEFINE_INTEGER(GL_LIGHT7);
  DEFINE_INTEGER(GL_SPOT_EXPONENT);
  DEFINE_INTEGER(GL_SPOT_CUTOFF);
  DEFINE_INTEGER(GL_CONSTANT_ATTENUATION);
  DEFINE_INTEGER(GL_LINEAR_ATTENUATION);
  DEFINE_INTEGER(GL_QUADRATIC_ATTENUATION);
  DEFINE_INTEGER(GL_AMBIENT);
  DEFINE_INTEGER(GL_DIFFUSE);
  DEFINE_INTEGER(GL_SPECULAR);
  DEFINE_INTEGER(GL_SHININESS);
  DEFINE_INTEGER(GL_EMISSION);
  DEFINE_INTEGER(GL_POSITION);
  DEFINE_INTEGER(GL_SPOT_DIRECTION);
  DEFINE_INTEGER(GL_AMBIENT_AND_DIFFUSE);
  DEFINE_INTEGER(GL_COLOR_INDEXES);
  DEFINE_INTEGER(GL_LIGHT_MODEL_TWO_SIDE);
  DEFINE_INTEGER(GL_LIGHT_MODEL_LOCAL_VIEWER);
  DEFINE_INTEGER(GL_LIGHT_MODEL_AMBIENT);
  DEFINE_INTEGER(GL_FRONT_AND_BACK);
  DEFINE_INTEGER(GL_SHADE_MODEL);
  DEFINE_INTEGER(GL_FLAT);
  DEFINE_INTEGER(GL_SMOOTH);
  DEFINE_INTEGER(GL_COLOR_MATERIAL);
  DEFINE_INTEGER(GL_COLOR_MATERIAL_FACE);
  DEFINE_INTEGER(GL_COLOR_MATERIAL_PARAMETER);
  DEFINE_INTEGER(GL_NORMALIZE);
  DEFINE_INTEGER(GL_CLIP_PLANE0);
  DEFINE_INTEGER(GL_CLIP_PLANE1);
  DEFINE_INTEGER(GL_CLIP_PLANE2);
  DEFINE_INTEGER(GL_CLIP_PLANE3);
  DEFINE_INTEGER(GL_CLIP_PLANE4);
  DEFINE_INTEGER(GL_CLIP_PLANE5);
  DEFINE_INTEGER(GL_ACCUM_RED_BITS);
  DEFINE_INTEGER(GL_ACCUM_GREEN_BITS);
  DEFINE_INTEGER(GL_ACCUM_BLUE_BITS);
  DEFINE_INTEGER(GL_ACCUM_ALPHA_BITS);
  DEFINE_INTEGER(GL_ACCUM_CLEAR_VALUE);
  DEFINE_INTEGER(GL_ACCUM);
  DEFINE_INTEGER(GL_ADD);
  DEFINE_INTEGER(GL_LOAD);
  DEFINE_INTEGER(GL_MULT);
  DEFINE_INTEGER(GL_RETURN);
  DEFINE_INTEGER(GL_ALPHA_TEST);
  DEFINE_INTEGER(GL_ALPHA_TEST_REF);
  DEFINE_INTEGER(GL_ALPHA_TEST_FUNC);
  DEFINE_INTEGER(GL_BLEND);
  DEFINE_INTEGER(GL_BLEND_SRC);
  DEFINE_INTEGER(GL_BLEND_DST);
  DEFINE_INTEGER(GL_ZERO);
  DEFINE_INTEGER(GL_ONE);
  DEFINE_INTEGER(GL_SRC_COLOR);
  DEFINE_INTEGER(GL_ONE_MINUS_SRC_COLOR);
  DEFINE_INTEGER(GL_DST_COLOR);
  DEFINE_INTEGER(GL_ONE_MINUS_DST_COLOR);
  DEFINE_INTEGER(GL_SRC_ALPHA);
  DEFINE_INTEGER(GL_ONE_MINUS_SRC_ALPHA);
  DEFINE_INTEGER(GL_DST_ALPHA);
  DEFINE_INTEGER(GL_ONE_MINUS_DST_ALPHA);
  DEFINE_INTEGER(GL_SRC_ALPHA_SATURATE);
  DEFINE_INTEGER(GL_CONSTANT_COLOR);
  DEFINE_INTEGER(GL_ONE_MINUS_CONSTANT_COLOR);
  DEFINE_INTEGER(GL_CONSTANT_ALPHA);
  DEFINE_INTEGER(GL_ONE_MINUS_CONSTANT_ALPHA);
  DEFINE_INTEGER(GL_FEEDBACK);
  DEFINE_INTEGER(GL_RENDER);
  DEFINE_INTEGER(GL_SELECT);
  DEFINE_INTEGER(GL_2D);
  DEFINE_INTEGER(GL_3D);
  DEFINE_INTEGER(GL_3D_COLOR);
  DEFINE_INTEGER(GL_3D_COLOR_TEXTURE);
  DEFINE_INTEGER(GL_4D_COLOR_TEXTURE);
  DEFINE_INTEGER(GL_POINT_TOKEN);
  DEFINE_INTEGER(GL_LINE_TOKEN);
  DEFINE_INTEGER(GL_LINE_RESET_TOKEN);
  DEFINE_INTEGER(GL_POLYGON_TOKEN);
  DEFINE_INTEGER(GL_BITMAP_TOKEN);
  DEFINE_INTEGER(GL_DRAW_PIXEL_TOKEN);
  DEFINE_INTEGER(GL_COPY_PIXEL_TOKEN);
  DEFINE_INTEGER(GL_PASS_THROUGH_TOKEN);
  DEFINE_INTEGER(GL_FEEDBACK_BUFFER_POINTER);
  DEFINE_INTEGER(GL_FEEDBACK_BUFFER_SIZE);
  DEFINE_INTEGER(GL_FEEDBACK_BUFFER_TYPE);
  DEFINE_INTEGER(GL_SELECTION_BUFFER_POINTER);
  DEFINE_INTEGER(GL_SELECTION_BUFFER_SIZE);
  DEFINE_INTEGER(GL_FOG);
  DEFINE_INTEGER(GL_FOG_MODE);
  DEFINE_INTEGER(GL_FOG_DENSITY);
  DEFINE_INTEGER(GL_FOG_COLOR);
  DEFINE_INTEGER(GL_FOG_INDEX);
  DEFINE_INTEGER(GL_FOG_START);
  DEFINE_INTEGER(GL_FOG_END);
  DEFINE_INTEGER(GL_LINEAR);
  DEFINE_INTEGER(GL_EXP);
  DEFINE_INTEGER(GL_EXP2);
  DEFINE_INTEGER(GL_LOGIC_OP);
  DEFINE_INTEGER(GL_INDEX_LOGIC_OP);
  DEFINE_INTEGER(GL_COLOR_LOGIC_OP);
  DEFINE_INTEGER(GL_LOGIC_OP_MODE);
  DEFINE_INTEGER(GL_CLEAR);
  DEFINE_INTEGER(GL_SET);
  DEFINE_INTEGER(GL_COPY);
  DEFINE_INTEGER(GL_COPY_INVERTED);
  DEFINE_INTEGER(GL_NOOP);
  DEFINE_INTEGER(GL_INVERT);
  DEFINE_INTEGER(GL_AND);
  DEFINE_INTEGER(GL_NAND);
  DEFINE_INTEGER(GL_OR);
  DEFINE_INTEGER(GL_NOR);
  DEFINE_INTEGER(GL_XOR);
  DEFINE_INTEGER(GL_EQUIV);
  DEFINE_INTEGER(GL_AND_REVERSE);
  DEFINE_INTEGER(GL_AND_INVERTED);
  DEFINE_INTEGER(GL_OR_REVERSE);
  DEFINE_INTEGER(GL_OR_INVERTED);
  DEFINE_INTEGER(GL_STENCIL_TEST);
  DEFINE_INTEGER(GL_STENCIL_WRITEMASK);
  DEFINE_INTEGER(GL_STENCIL_BITS);
  DEFINE_INTEGER(GL_STENCIL_FUNC);
  DEFINE_INTEGER(GL_STENCIL_VALUE_MASK);
  DEFINE_INTEGER(GL_STENCIL_REF);
  DEFINE_INTEGER(GL_STENCIL_FAIL);
  DEFINE_INTEGER(GL_STENCIL_PASS_DEPTH_PASS);
  DEFINE_INTEGER(GL_STENCIL_PASS_DEPTH_FAIL);
  DEFINE_INTEGER(GL_STENCIL_CLEAR_VALUE);
  DEFINE_INTEGER(GL_STENCIL_INDEX);
  DEFINE_INTEGER(GL_KEEP);
  DEFINE_INTEGER(GL_REPLACE);
  DEFINE_INTEGER(GL_INCR);
  DEFINE_INTEGER(GL_DECR);
  DEFINE_INTEGER(GL_NONE);
  DEFINE_INTEGER(GL_LEFT);
  DEFINE_INTEGER(GL_RIGHT);
  DEFINE_INTEGER(GL_FRONT_LEFT);
  DEFINE_INTEGER(GL_FRONT_RIGHT);
  DEFINE_INTEGER(GL_BACK_LEFT);
  DEFINE_INTEGER(GL_BACK_RIGHT);
  DEFINE_INTEGER(GL_AUX0);
  DEFINE_INTEGER(GL_AUX1);
  DEFINE_INTEGER(GL_AUX2);
  DEFINE_INTEGER(GL_AUX3);
  DEFINE_INTEGER(GL_COLOR_INDEX);
  DEFINE_INTEGER(GL_RED);
  DEFINE_INTEGER(GL_GREEN);
  DEFINE_INTEGER(GL_BLUE);
  DEFINE_INTEGER(GL_ALPHA);
  DEFINE_INTEGER(GL_LUMINANCE);
  DEFINE_INTEGER(GL_LUMINANCE_ALPHA);
  DEFINE_INTEGER(GL_ALPHA_BITS);
  DEFINE_INTEGER(GL_RED_BITS);
  DEFINE_INTEGER(GL_GREEN_BITS);
  DEFINE_INTEGER(GL_BLUE_BITS);
  DEFINE_INTEGER(GL_INDEX_BITS);
  DEFINE_INTEGER(GL_SUBPIXEL_BITS);
  DEFINE_INTEGER(GL_AUX_BUFFERS);
  DEFINE_INTEGER(GL_READ_BUFFER);
  DEFINE_INTEGER(GL_DRAW_BUFFER);
  DEFINE_INTEGER(GL_DOUBLEBUFFER);
  DEFINE_INTEGER(GL_STEREO);
  DEFINE_INTEGER(GL_BITMAP);
  DEFINE_INTEGER(GL_COLOR);
  DEFINE_INTEGER(GL_DEPTH);
  DEFINE_INTEGER(GL_STENCIL);
  DEFINE_INTEGER(GL_DITHER);
  DEFINE_INTEGER(GL_RGB);
  DEFINE_INTEGER(GL_RGBA);
  DEFINE_INTEGER(GL_MAX_LIST_NESTING);
  DEFINE_INTEGER(GL_MAX_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_MODELVIEW_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_NAME_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_PROJECTION_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_TEXTURE_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_EVAL_ORDER);
  DEFINE_INTEGER(GL_MAX_LIGHTS);
  DEFINE_INTEGER(GL_MAX_CLIP_PLANES);
  DEFINE_INTEGER(GL_MAX_TEXTURE_SIZE);
  DEFINE_INTEGER(GL_MAX_PIXEL_MAP_TABLE);
  DEFINE_INTEGER(GL_MAX_VIEWPORT_DIMS);
  DEFINE_INTEGER(GL_MAX_CLIENT_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(GL_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(GL_CLIENT_ATTRIB_STACK_DEPTH);
  DEFINE_INTEGER(GL_COLOR_CLEAR_VALUE);
  DEFINE_INTEGER(GL_COLOR_WRITEMASK);
  DEFINE_INTEGER(GL_CURRENT_INDEX);
  DEFINE_INTEGER(GL_CURRENT_COLOR);
  DEFINE_INTEGER(GL_CURRENT_NORMAL);
  DEFINE_INTEGER(GL_CURRENT_RASTER_COLOR);
  DEFINE_INTEGER(GL_CURRENT_RASTER_DISTANCE);
  DEFINE_INTEGER(GL_CURRENT_RASTER_INDEX);
  DEFINE_INTEGER(GL_CURRENT_RASTER_POSITION);
  DEFINE_INTEGER(GL_CURRENT_RASTER_TEXTURE_COORDS);
  DEFINE_INTEGER(GL_CURRENT_RASTER_POSITION_VALID);
  DEFINE_INTEGER(GL_CURRENT_TEXTURE_COORDS);
  DEFINE_INTEGER(GL_INDEX_CLEAR_VALUE);
  DEFINE_INTEGER(GL_INDEX_MODE);
  DEFINE_INTEGER(GL_INDEX_WRITEMASK);
  DEFINE_INTEGER(GL_MODELVIEW_MATRIX);
  DEFINE_INTEGER(GL_MODELVIEW_STACK_DEPTH);
  DEFINE_INTEGER(GL_NAME_STACK_DEPTH);
  DEFINE_INTEGER(GL_PROJECTION_MATRIX);
  DEFINE_INTEGER(GL_PROJECTION_STACK_DEPTH);
  DEFINE_INTEGER(GL_RENDER_MODE);
  DEFINE_INTEGER(GL_RGBA_MODE);
  DEFINE_INTEGER(GL_TEXTURE_MATRIX);
  DEFINE_INTEGER(GL_TEXTURE_STACK_DEPTH);
  DEFINE_INTEGER(GL_VIEWPORT);
  DEFINE_INTEGER(GL_AUTO_NORMAL);
  DEFINE_INTEGER(GL_MAP1_COLOR_4);
  DEFINE_INTEGER(GL_MAP1_GRID_DOMAIN);
  DEFINE_INTEGER(GL_MAP1_GRID_SEGMENTS);
  DEFINE_INTEGER(GL_MAP1_INDEX);
  DEFINE_INTEGER(GL_MAP1_NORMAL);
  DEFINE_INTEGER(GL_MAP1_TEXTURE_COORD_1);
  DEFINE_INTEGER(GL_MAP1_TEXTURE_COORD_2);
  DEFINE_INTEGER(GL_MAP1_TEXTURE_COORD_3);
  DEFINE_INTEGER(GL_MAP1_TEXTURE_COORD_4);
  DEFINE_INTEGER(GL_MAP1_VERTEX_3);
  DEFINE_INTEGER(GL_MAP1_VERTEX_4);
  DEFINE_INTEGER(GL_MAP2_COLOR_4);
  DEFINE_INTEGER(GL_MAP2_GRID_DOMAIN);
  DEFINE_INTEGER(GL_MAP2_GRID_SEGMENTS);
  DEFINE_INTEGER(GL_MAP2_INDEX);
  DEFINE_INTEGER(GL_MAP2_NORMAL);
  DEFINE_INTEGER(GL_MAP2_TEXTURE_COORD_1);
  DEFINE_INTEGER(GL_MAP2_TEXTURE_COORD_2);
  DEFINE_INTEGER(GL_MAP2_TEXTURE_COORD_3);
  DEFINE_INTEGER(GL_MAP2_TEXTURE_COORD_4);
  DEFINE_INTEGER(GL_MAP2_VERTEX_3);
  DEFINE_INTEGER(GL_MAP2_VERTEX_4);
  DEFINE_INTEGER(GL_COEFF);
  DEFINE_INTEGER(GL_DOMAIN);
  DEFINE_INTEGER(GL_ORDER);
  DEFINE_INTEGER(GL_FOG_HINT);
  DEFINE_INTEGER(GL_LINE_SMOOTH_HINT);
  DEFINE_INTEGER(GL_PERSPECTIVE_CORRECTION_HINT);
  DEFINE_INTEGER(GL_POINT_SMOOTH_HINT);
  DEFINE_INTEGER(GL_POLYGON_SMOOTH_HINT);
  DEFINE_INTEGER(GL_DONT_CARE);
  DEFINE_INTEGER(GL_FASTEST);
  DEFINE_INTEGER(GL_NICEST);
  DEFINE_INTEGER(GL_SCISSOR_TEST);
  DEFINE_INTEGER(GL_SCISSOR_BOX);
  DEFINE_INTEGER(GL_MAP_COLOR);
  DEFINE_INTEGER(GL_MAP_STENCIL);
  DEFINE_INTEGER(GL_INDEX_SHIFT);
  DEFINE_INTEGER(GL_INDEX_OFFSET);
  DEFINE_INTEGER(GL_RED_SCALE);
  DEFINE_INTEGER(GL_RED_BIAS);
  DEFINE_INTEGER(GL_GREEN_SCALE);
  DEFINE_INTEGER(GL_GREEN_BIAS);
  DEFINE_INTEGER(GL_BLUE_SCALE);
  DEFINE_INTEGER(GL_BLUE_BIAS);
  DEFINE_INTEGER(GL_ALPHA_SCALE);
  DEFINE_INTEGER(GL_ALPHA_BIAS);
  DEFINE_INTEGER(GL_DEPTH_SCALE);
  DEFINE_INTEGER(GL_DEPTH_BIAS);
  DEFINE_INTEGER(GL_PIXEL_MAP_S_TO_S_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_I_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_R_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_G_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_B_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_A_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_R_TO_R_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_G_TO_G_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_B_TO_B_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_A_TO_A_SIZE);
  DEFINE_INTEGER(GL_PIXEL_MAP_S_TO_S);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_I);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_R);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_G);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_B);
  DEFINE_INTEGER(GL_PIXEL_MAP_I_TO_A);
  DEFINE_INTEGER(GL_PIXEL_MAP_R_TO_R);
  DEFINE_INTEGER(GL_PIXEL_MAP_G_TO_G);
  DEFINE_INTEGER(GL_PIXEL_MAP_B_TO_B);
  DEFINE_INTEGER(GL_PIXEL_MAP_A_TO_A);
  DEFINE_INTEGER(GL_PACK_ALIGNMENT);
  DEFINE_INTEGER(GL_PACK_LSB_FIRST);
  DEFINE_INTEGER(GL_PACK_ROW_LENGTH);
  DEFINE_INTEGER(GL_PACK_SKIP_PIXELS);
  DEFINE_INTEGER(GL_PACK_SKIP_ROWS);
  DEFINE_INTEGER(GL_PACK_SWAP_BYTES);
  DEFINE_INTEGER(GL_UNPACK_ALIGNMENT);
  DEFINE_INTEGER(GL_UNPACK_LSB_FIRST);
  DEFINE_INTEGER(GL_UNPACK_ROW_LENGTH);
  DEFINE_INTEGER(GL_UNPACK_SKIP_PIXELS);
  DEFINE_INTEGER(GL_UNPACK_SKIP_ROWS);
  DEFINE_INTEGER(GL_UNPACK_SWAP_BYTES);
  DEFINE_INTEGER(GL_ZOOM_X);
  DEFINE_INTEGER(GL_ZOOM_Y);
  DEFINE_INTEGER(GL_TEXTURE_ENV);
  DEFINE_INTEGER(GL_TEXTURE_ENV_MODE);
  DEFINE_INTEGER(GL_TEXTURE_1D);
  DEFINE_INTEGER(GL_TEXTURE_2D);
  DEFINE_INTEGER(GL_TEXTURE_WRAP_S);
  DEFINE_INTEGER(GL_TEXTURE_WRAP_T);
  DEFINE_INTEGER(GL_TEXTURE_MAG_FILTER);
  DEFINE_INTEGER(GL_TEXTURE_MIN_FILTER);
  DEFINE_INTEGER(GL_TEXTURE_ENV_COLOR);
  DEFINE_INTEGER(GL_TEXTURE_GEN_S);
  DEFINE_INTEGER(GL_TEXTURE_GEN_T);
  DEFINE_INTEGER(GL_TEXTURE_GEN_MODE);
  DEFINE_INTEGER(GL_TEXTURE_BORDER_COLOR);
  DEFINE_INTEGER(GL_TEXTURE_WIDTH);
  DEFINE_INTEGER(GL_TEXTURE_HEIGHT);
  DEFINE_INTEGER(GL_TEXTURE_BORDER);
  DEFINE_INTEGER(GL_TEXTURE_COMPONENTS);
  DEFINE_INTEGER(GL_TEXTURE_RED_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_GREEN_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_BLUE_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_ALPHA_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_LUMINANCE_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_INTENSITY_SIZE);
  DEFINE_INTEGER(GL_NEAREST_MIPMAP_NEAREST);
  DEFINE_INTEGER(GL_NEAREST_MIPMAP_LINEAR);
  DEFINE_INTEGER(GL_LINEAR_MIPMAP_NEAREST);
  DEFINE_INTEGER(GL_LINEAR_MIPMAP_LINEAR);
  DEFINE_INTEGER(GL_OBJECT_LINEAR);
  DEFINE_INTEGER(GL_OBJECT_PLANE);
  DEFINE_INTEGER(GL_EYE_LINEAR);
  DEFINE_INTEGER(GL_EYE_PLANE);
  DEFINE_INTEGER(GL_SPHERE_MAP);
  DEFINE_INTEGER(GL_DECAL);
  DEFINE_INTEGER(GL_MODULATE);
  DEFINE_INTEGER(GL_NEAREST);
  DEFINE_INTEGER(GL_REPEAT);
  DEFINE_INTEGER(GL_CLAMP);
  DEFINE_INTEGER(GL_S);
  DEFINE_INTEGER(GL_T);
  DEFINE_INTEGER(GL_R);
  DEFINE_INTEGER(GL_Q);
  DEFINE_INTEGER(GL_TEXTURE_GEN_R);
  DEFINE_INTEGER(GL_TEXTURE_GEN_Q);
  DEFINE_INTEGER(GL_PROXY_TEXTURE_1D);
  DEFINE_INTEGER(GL_PROXY_TEXTURE_2D);
  DEFINE_INTEGER(GL_TEXTURE_PRIORITY);
  DEFINE_INTEGER(GL_TEXTURE_RESIDENT);
  DEFINE_INTEGER(GL_TEXTURE_BINDING_1D);
  DEFINE_INTEGER(GL_TEXTURE_BINDING_2D);
  DEFINE_INTEGER(GL_TEXTURE_INTERNAL_FORMAT);
  DEFINE_INTEGER(GL_PACK_SKIP_IMAGES);
  DEFINE_INTEGER(GL_PACK_IMAGE_HEIGHT);
  DEFINE_INTEGER(GL_UNPACK_SKIP_IMAGES);
  DEFINE_INTEGER(GL_UNPACK_IMAGE_HEIGHT);
  DEFINE_INTEGER(GL_TEXTURE_3D);
  DEFINE_INTEGER(GL_PROXY_TEXTURE_3D);
  DEFINE_INTEGER(GL_TEXTURE_DEPTH);
  DEFINE_INTEGER(GL_TEXTURE_WRAP_R);
  DEFINE_INTEGER(GL_MAX_3D_TEXTURE_SIZE);
  DEFINE_INTEGER(GL_TEXTURE_BINDING_3D);
  DEFINE_INTEGER(GL_ALPHA4);
  DEFINE_INTEGER(GL_ALPHA8);
  DEFINE_INTEGER(GL_ALPHA12);
  DEFINE_INTEGER(GL_ALPHA16);
  DEFINE_INTEGER(GL_LUMINANCE4);
  DEFINE_INTEGER(GL_LUMINANCE8);
  DEFINE_INTEGER(GL_LUMINANCE12);
  DEFINE_INTEGER(GL_LUMINANCE16);
  DEFINE_INTEGER(GL_LUMINANCE4_ALPHA4);
  DEFINE_INTEGER(GL_LUMINANCE6_ALPHA2);
  DEFINE_INTEGER(GL_LUMINANCE8_ALPHA8);
  DEFINE_INTEGER(GL_LUMINANCE12_ALPHA4);
  DEFINE_INTEGER(GL_LUMINANCE12_ALPHA12);
  DEFINE_INTEGER(GL_LUMINANCE16_ALPHA16);
  DEFINE_INTEGER(GL_INTENSITY);
  DEFINE_INTEGER(GL_INTENSITY4);
  DEFINE_INTEGER(GL_INTENSITY8);
  DEFINE_INTEGER(GL_INTENSITY12);
  DEFINE_INTEGER(GL_INTENSITY16);
  DEFINE_INTEGER(GL_R3_G3_B2);
  DEFINE_INTEGER(GL_RGB4);
  DEFINE_INTEGER(GL_RGB5);
  DEFINE_INTEGER(GL_RGB8);
  DEFINE_INTEGER(GL_RGB10);
  DEFINE_INTEGER(GL_RGB12);
  DEFINE_INTEGER(GL_RGB16);
  DEFINE_INTEGER(GL_RGBA2);
  DEFINE_INTEGER(GL_RGBA4);
  DEFINE_INTEGER(GL_RGB5_A1);
  DEFINE_INTEGER(GL_RGBA8);
  DEFINE_INTEGER(GL_RGB10_A2);
  DEFINE_INTEGER(GL_RGBA12);
  DEFINE_INTEGER(GL_RGBA16);
  DEFINE_INTEGER(GL_VENDOR);
  DEFINE_INTEGER(GL_RENDERER);
  DEFINE_INTEGER(GL_VERSION);
  DEFINE_INTEGER(GL_EXTENSIONS);
  DEFINE_INTEGER(GL_NO_ERROR);
  DEFINE_INTEGER(GL_INVALID_VALUE);
  DEFINE_INTEGER(GL_INVALID_ENUM);
  DEFINE_INTEGER(GL_INVALID_OPERATION);
  DEFINE_INTEGER(GL_STACK_OVERFLOW);
  DEFINE_INTEGER(GL_STACK_UNDERFLOW);
  DEFINE_INTEGER(GL_OUT_OF_MEMORY);
  DEFINE_INTEGER(GL_RESCALE_NORMAL);
  DEFINE_INTEGER(GL_CLAMP_TO_EDGE);
  DEFINE_INTEGER(GL_MAX_ELEMENTS_VERTICES);
  DEFINE_INTEGER(GL_MAX_ELEMENTS_INDICES);
  DEFINE_INTEGER(GL_BGR);
  DEFINE_INTEGER(GL_BGRA);
  DEFINE_INTEGER(GL_UNSIGNED_BYTE_3_3_2);
  DEFINE_INTEGER(GL_UNSIGNED_BYTE_2_3_3_REV);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_5_6_5);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_5_6_5_REV);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_4_4_4_4);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_4_4_4_4_REV);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_5_5_5_1);
  DEFINE_INTEGER(GL_UNSIGNED_SHORT_1_5_5_5_REV);
  DEFINE_INTEGER(GL_UNSIGNED_INT_8_8_8_8);
  DEFINE_INTEGER(GL_UNSIGNED_INT_8_8_8_8_REV);
  DEFINE_INTEGER(GL_UNSIGNED_INT_10_10_10_2);
  DEFINE_INTEGER(GL_UNSIGNED_INT_2_10_10_10_REV);
  DEFINE_INTEGER(GL_LIGHT_MODEL_COLOR_CONTROL);
  DEFINE_INTEGER(GL_SINGLE_COLOR);
  DEFINE_INTEGER(GL_SEPARATE_SPECULAR_COLOR);
  DEFINE_INTEGER(GL_TEXTURE_MIN_LOD);
  DEFINE_INTEGER(GL_TEXTURE_MAX_LOD);
  DEFINE_INTEGER(GL_TEXTURE_BASE_LEVEL);
  DEFINE_INTEGER(GL_TEXTURE_MAX_LEVEL);
  DEFINE_INTEGER(GL_COLOR_TABLE);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_COLOR_TABLE);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_COLOR_TABLE);
  DEFINE_INTEGER(GL_PROXY_COLOR_TABLE);
  DEFINE_INTEGER(GL_PROXY_POST_CONVOLUTION_COLOR_TABLE);
  DEFINE_INTEGER(GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE);
  DEFINE_INTEGER(GL_COLOR_TABLE_SCALE);
  DEFINE_INTEGER(GL_COLOR_TABLE_BIAS);
  DEFINE_INTEGER(GL_COLOR_TABLE_FORMAT);
  DEFINE_INTEGER(GL_COLOR_TABLE_WIDTH);
  DEFINE_INTEGER(GL_COLOR_TABLE_RED_SIZE);
  DEFINE_INTEGER(GL_COLOR_TABLE_GREEN_SIZE);
  DEFINE_INTEGER(GL_COLOR_TABLE_BLUE_SIZE);
  DEFINE_INTEGER(GL_COLOR_TABLE_ALPHA_SIZE);
  DEFINE_INTEGER(GL_COLOR_TABLE_LUMINANCE_SIZE);
  DEFINE_INTEGER(GL_COLOR_TABLE_INTENSITY_SIZE);
  DEFINE_INTEGER(GL_CONVOLUTION_1D);
  DEFINE_INTEGER(GL_CONVOLUTION_2D);
  DEFINE_INTEGER(GL_SEPARABLE_2D);
  DEFINE_INTEGER(GL_CONVOLUTION_BORDER_MODE);
  DEFINE_INTEGER(GL_CONVOLUTION_FILTER_SCALE);
  DEFINE_INTEGER(GL_CONVOLUTION_FILTER_BIAS);
  DEFINE_INTEGER(GL_REDUCE);
  DEFINE_INTEGER(GL_CONVOLUTION_FORMAT);
  DEFINE_INTEGER(GL_CONVOLUTION_WIDTH);
  DEFINE_INTEGER(GL_CONVOLUTION_HEIGHT);
  DEFINE_INTEGER(GL_MAX_CONVOLUTION_WIDTH);
  DEFINE_INTEGER(GL_MAX_CONVOLUTION_HEIGHT);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_RED_SCALE);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_GREEN_SCALE);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_BLUE_SCALE);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_ALPHA_SCALE);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_RED_BIAS);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_GREEN_BIAS);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_BLUE_BIAS);
  DEFINE_INTEGER(GL_POST_CONVOLUTION_ALPHA_BIAS);
  DEFINE_INTEGER(GL_CONSTANT_BORDER);
  DEFINE_INTEGER(GL_REPLICATE_BORDER);
  DEFINE_INTEGER(GL_CONVOLUTION_BORDER_COLOR);
  DEFINE_INTEGER(GL_COLOR_MATRIX);
  DEFINE_INTEGER(GL_COLOR_MATRIX_STACK_DEPTH);
  DEFINE_INTEGER(GL_MAX_COLOR_MATRIX_STACK_DEPTH);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_RED_SCALE);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_GREEN_SCALE);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_BLUE_SCALE);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_ALPHA_SCALE);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_RED_BIAS);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_GREEN_BIAS);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_BLUE_BIAS);
  DEFINE_INTEGER(GL_POST_COLOR_MATRIX_ALPHA_BIAS);
  DEFINE_INTEGER(GL_HISTOGRAM);
  DEFINE_INTEGER(GL_PROXY_HISTOGRAM);
  DEFINE_INTEGER(GL_HISTOGRAM_WIDTH);
  DEFINE_INTEGER(GL_HISTOGRAM_FORMAT);
  DEFINE_INTEGER(GL_HISTOGRAM_RED_SIZE);
  DEFINE_INTEGER(GL_HISTOGRAM_GREEN_SIZE);
  DEFINE_INTEGER(GL_HISTOGRAM_BLUE_SIZE);
  DEFINE_INTEGER(GL_HISTOGRAM_ALPHA_SIZE);
  DEFINE_INTEGER(GL_HISTOGRAM_LUMINANCE_SIZE);
  DEFINE_INTEGER(GL_HISTOGRAM_SINK);
  DEFINE_INTEGER(GL_MINMAX);
  DEFINE_INTEGER(GL_MINMAX_FORMAT);
  DEFINE_INTEGER(GL_MINMAX_SINK);
  DEFINE_INTEGER(GL_TABLE_TOO_LARGE);
  DEFINE_INTEGER(GL_BLEND_EQUATION);
  DEFINE_INTEGER(GL_MIN);
  DEFINE_INTEGER(GL_MAX);
  DEFINE_INTEGER(GL_FUNC_ADD);
  DEFINE_INTEGER(GL_FUNC_SUBTRACT);
  DEFINE_INTEGER(GL_FUNC_REVERSE_SUBTRACT);
  DEFINE_INTEGER(GL_BLEND_COLOR);
  DEFINE_INTEGER(GL_CURRENT_BIT);
  DEFINE_INTEGER(GL_POINT_BIT);
  DEFINE_INTEGER(GL_LINE_BIT);
  DEFINE_INTEGER(GL_POLYGON_BIT);
  DEFINE_INTEGER(GL_POLYGON_STIPPLE_BIT);
  DEFINE_INTEGER(GL_PIXEL_MODE_BIT);
  DEFINE_INTEGER(GL_LIGHTING_BIT);
  DEFINE_INTEGER(GL_FOG_BIT);
  DEFINE_INTEGER(GL_DEPTH_BUFFER_BIT);
  DEFINE_INTEGER(GL_ACCUM_BUFFER_BIT);
  DEFINE_INTEGER(GL_STENCIL_BUFFER_BIT);
  DEFINE_INTEGER(GL_VIEWPORT_BIT);
  DEFINE_INTEGER(GL_TRANSFORM_BIT);
  DEFINE_INTEGER(GL_ENABLE_BIT);
  DEFINE_INTEGER(GL_COLOR_BUFFER_BIT);
  DEFINE_INTEGER(GL_HINT_BIT);
  DEFINE_INTEGER(GL_EVAL_BIT);
  DEFINE_INTEGER(GL_LIST_BIT);
  DEFINE_INTEGER(GL_TEXTURE_BIT);
  DEFINE_INTEGER(GL_SCISSOR_BIT);
  DEFINE_INTEGER(GL_ALL_ATTRIB_BITS);
  DEFINE_INTEGER(GL_CLIENT_PIXEL_STORE_BIT);
  DEFINE_INTEGER(GL_CLIENT_VERTEX_ARRAY_BIT);
#if HAVE_GLU
  DEFINE_INTEGER(GLU_FALSE);
  DEFINE_INTEGER(GLU_TRUE);
  DEFINE_INTEGER(GLU_VERSION);
  DEFINE_INTEGER(GLU_EXTENSIONS);
  DEFINE_INTEGER(GLU_INVALID_ENUM);
  DEFINE_INTEGER(GLU_INVALID_VALUE);
  DEFINE_INTEGER(GLU_OUT_OF_MEMORY);
  DEFINE_INTEGER(GLU_OUTLINE_POLYGON);
  DEFINE_INTEGER(GLU_OUTLINE_PATCH);
  DEFINE_INTEGER(GLU_ERROR);
  DEFINE_INTEGER(GLU_AUTO_LOAD_MATRIX);
  DEFINE_INTEGER(GLU_CULLING);
  DEFINE_INTEGER(GLU_SAMPLING_TOLERANCE);
  DEFINE_INTEGER(GLU_DISPLAY_MODE);
  DEFINE_INTEGER(GLU_PARAMETRIC_TOLERANCE);
  DEFINE_INTEGER(GLU_SAMPLING_METHOD);
  DEFINE_INTEGER(GLU_U_STEP);
  DEFINE_INTEGER(GLU_V_STEP);
  DEFINE_INTEGER(GLU_PATH_LENGTH);
  DEFINE_INTEGER(GLU_PARAMETRIC_ERROR);
  DEFINE_INTEGER(GLU_DOMAIN_DISTANCE);
  DEFINE_INTEGER(GLU_MAP1_TRIM_2);
  DEFINE_INTEGER(GLU_MAP1_TRIM_3);
  DEFINE_INTEGER(GLU_POINT);
  DEFINE_INTEGER(GLU_LINE);
  DEFINE_INTEGER(GLU_FILL);
  DEFINE_INTEGER(GLU_SILHOUETTE);
  DEFINE_INTEGER(GLU_SMOOTH);
  DEFINE_INTEGER(GLU_FLAT);
  DEFINE_INTEGER(GLU_NONE);
  DEFINE_INTEGER(GLU_OUTSIDE);
  DEFINE_INTEGER(GLU_INSIDE);
  DEFINE_INTEGER(GLU_TESS_BEGIN);
  DEFINE_INTEGER(GLU_BEGIN);
  DEFINE_INTEGER(GLU_TESS_VERTEX);
  DEFINE_INTEGER(GLU_VERTEX);
  DEFINE_INTEGER(GLU_TESS_END);
  DEFINE_INTEGER(GLU_END);
  DEFINE_INTEGER(GLU_TESS_ERROR);
  DEFINE_INTEGER(GLU_TESS_EDGE_FLAG);
  DEFINE_INTEGER(GLU_EDGE_FLAG);
  DEFINE_INTEGER(GLU_TESS_COMBINE);
  DEFINE_INTEGER(GLU_TESS_BEGIN_DATA);
  DEFINE_INTEGER(GLU_TESS_VERTEX_DATA);
  DEFINE_INTEGER(GLU_TESS_END_DATA);
  DEFINE_INTEGER(GLU_TESS_ERROR_DATA);
  DEFINE_INTEGER(GLU_TESS_EDGE_FLAG_DATA);
  DEFINE_INTEGER(GLU_TESS_COMBINE_DATA);
  DEFINE_INTEGER(GLU_CW);
  DEFINE_INTEGER(GLU_CCW);
  DEFINE_INTEGER(GLU_INTERIOR);
  DEFINE_INTEGER(GLU_EXTERIOR);
  DEFINE_INTEGER(GLU_UNKNOWN);
  DEFINE_INTEGER(GLU_TESS_WINDING_RULE);
  DEFINE_INTEGER(GLU_TESS_BOUNDARY_ONLY);
  DEFINE_INTEGER(GLU_TESS_TOLERANCE);
  DEFINE_INTEGER(GLU_TESS_WINDING_ODD);
  DEFINE_INTEGER(GLU_TESS_WINDING_NONZERO);
  DEFINE_INTEGER(GLU_TESS_WINDING_POSITIVE);
  DEFINE_INTEGER(GLU_TESS_WINDING_NEGATIVE);
  DEFINE_INTEGER(GLU_TESS_WINDING_ABS_GEQ_TWO);
#endif
}

/* -------------------------------- initialization -------------------------------- */

static bool gl_already_inited = false;

void Init_libgl(void);
void Init_libgl(void)
{
  if (!gl_already_inited)
    {
      define_integers();
      define_functions();
      XEN_YES_WE_HAVE("gl");
      XEN_DEFINE("gl-version", C_TO_XEN_STRING("26-Sep-06"));
      gl_already_inited = true;
    }
}
#else
 void Init_libgl(void);
 void Init_libgl(void)
{
}
#endif
