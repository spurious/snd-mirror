/* gl.c: s7, Ruby, and Forth bindings for GL, GLU
 *   generated automatically from makegl.scm and gldata.scm
 *   needs xen.h
 *
 * reference args are ignored if passed, resultant values are returned in a list.
 * the various "v" forms are omitted for now -- are they needed in this context?
 * 'gl is added to *features*
 *
 * HISTORY:
 *     --------
 *     16-Dec-09: removed Guile support.
 *     --------
 *     17-Oct-08: removed gtkglext bindings.
 *     --------
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

#define WRAP_FOR_XEN(Name, Value) Xen_list_2(C_string_to_Xen_symbol(Name), XEN_WRAP_C_POINTER(Value))
#define IS_WRAPPED(Name, Value) (Xen_is_list(Value) && \
                            (Xen_list_length(Value) >= 2) && \
                            (Xen_is_symbol(Xen_car(Value))) && \
                            (strcmp(Name, Xen_symbol_to_C_string(Xen_car(Value))) == 0))

#define XL_TYPE(Name, XType) \
  static XEN C_TO_XEN_ ## Name (XType val) {return(Xen_list_2(C_string_to_Xen_symbol(#Name), C_ulong_to_Xen_ulong(val)));} \
  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \
  static bool Xen_is_ ## Name (XEN val) {return(IS_WRAPPED(#Name, val));}
#define XL_TYPE_1(Name, XType) \
  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \
  static bool Xen_is_ ## Name (XEN val) {return(IS_WRAPPED(#Name, val));}

#define XL_TYPE_PTR(Name, XType) \
  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(Xen_false);} \
  static XType XEN_TO_C_ ## Name (XEN val) {if (Xen_is_false(val)) return(NULL); return((XType)XEN_UNWRAP_C_POINTER(Xen_cadr(val)));} \
  static bool Xen_is_ ## Name (XEN val) {return(IS_WRAPPED(#Name, val));} /* if NULL ok, should be explicit */
#define XL_TYPE_PTR_1(Name, XType) \
  static XType XEN_TO_C_ ## Name (XEN val) {if (Xen_is_false(val)) return(NULL); return((XType)XEN_UNWRAP_C_POINTER(Xen_cadr(val)));} \
  static bool Xen_is_ ## Name (XEN val) {return(IS_WRAPPED(#Name, val));} /* if NULL ok, should be explicit */


/* ---------------------------------------- types ---------------------------------------- */

#if USE_MOTIF
XL_TYPE_PTR(XVisualInfo, XVisualInfo*)
XL_TYPE_PTR(Display, Display*)
#define C_TO_XEN_int(Arg) C_int_to_Xen_integer(Arg)
#define XEN_TO_C_int(Arg) (int)(Xen_integer_to_C_int(Arg))
#define Xen_is_int(Arg) Xen_is_integer(Arg)
XL_TYPE_PTR_1(int_, int*)
XL_TYPE_PTR(GLXContext, GLXContext)
#define XEN_TO_C_unsigned_long(Arg) (unsigned_long)(Xen_ulong_to_C_ulong(Arg))
#define Xen_is_unsigned_long(Arg) Xen_is_ulong_int(Arg)
#define C_TO_XEN_Bool(Arg) C_bool_to_Xen_boolean(Arg)
#define XEN_TO_C_Bool(Arg) (Bool)(Xen_boolean_to_C_bool(Arg))
#define Xen_is_Bool(Arg) Xen_is_boolean(Arg)
XL_TYPE(GLXPixmap, GLXPixmap)
XL_TYPE_1(Pixmap, Pixmap)
XL_TYPE(Window, Window)
XL_TYPE_1(Font, Font)
#define C_TO_XEN_char_(Arg) C_string_to_Xen_string(Arg)
#define XEN_TO_C_char_(Arg) (char*)(Xen_string_to_C_string(Arg))
#define Xen_is_char_(Arg) Xen_is_string(Arg)
#endif
#define C_TO_XEN_GLfloat(Arg) C_double_to_Xen_real(Arg)
#define XEN_TO_C_GLfloat(Arg) (GLfloat)(Xen_real_to_C_double(Arg))
#define Xen_is_GLfloat(Arg) Xen_is_number(Arg)
#define XEN_TO_C_GLclampf(Arg) (GLclampf)(Xen_real_to_C_double(Arg))
#define Xen_is_GLclampf(Arg) Xen_is_number(Arg)
#define XEN_TO_C_GLbitfield(Arg) (GLbitfield)(Xen_ulong_to_C_ulong(Arg))
#define Xen_is_GLbitfield(Arg) Xen_is_ulong_int(Arg)
#define C_TO_XEN_GLuint(Arg) C_ulong_to_Xen_ulong(Arg)
#define XEN_TO_C_GLuint(Arg) (GLuint)(Xen_ulong_to_C_ulong(Arg))
#define Xen_is_GLuint(Arg) Xen_is_ulong_int(Arg)
#define C_TO_XEN_GLboolean(Arg) C_bool_to_Xen_boolean(Arg)
#define XEN_TO_C_GLboolean(Arg) (GLboolean)(Xen_boolean_to_C_bool(Arg))
#define Xen_is_GLboolean(Arg) Xen_is_boolean(Arg)
#define C_TO_XEN_GLenum(Arg) C_int_to_Xen_integer(Arg)
#define XEN_TO_C_GLenum(Arg) (GLenum)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLenum(Arg) Xen_is_integer(Arg)
#define C_TO_XEN_GLint(Arg) C_int_to_Xen_integer(Arg)
#define XEN_TO_C_GLint(Arg) (GLint)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLint(Arg) Xen_is_integer(Arg)
#define C_TO_XEN_GLushort(Arg) C_int_to_Xen_integer(Arg)
#define XEN_TO_C_GLushort(Arg) (GLushort)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLushort(Arg) Xen_is_integer(Arg)
XL_TYPE_PTR_1(GLubyte_, GLubyte*)
#define XEN_TO_C_GLsizei(Arg) (GLsizei)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLsizei(Arg) Xen_is_integer(Arg)
XL_TYPE_PTR_1(GLdouble_, GLdouble*)
#define C_TO_XEN_GLdouble(Arg) C_double_to_Xen_real(Arg)
#define XEN_TO_C_GLdouble(Arg) (GLdouble)(Xen_real_to_C_double(Arg))
#define Xen_is_GLdouble(Arg) Xen_is_number(Arg)
#define C_TO_XEN_constchar_(Arg) C_string_to_Xen_string((char *)(Arg))
#define XEN_TO_C_GLclampd(Arg) (GLclampd)(Xen_real_to_C_double(Arg))
#define Xen_is_GLclampd(Arg) Xen_is_number(Arg)
XL_TYPE_PTR_1(GLfloat_, GLfloat*)
XL_TYPE_PTR_1(GLvoid_, GLvoid*)
#define XEN_TO_C_GLshort(Arg) (GLshort)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLshort(Arg) Xen_is_integer(Arg)
#define XEN_TO_C_GLbyte(Arg) (GLbyte)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLbyte(Arg) Xen_is_integer(Arg)
#define XEN_TO_C_GLubyte(Arg) (GLubyte)(Xen_integer_to_C_int(Arg))
#define Xen_is_GLubyte(Arg) Xen_is_integer(Arg)
XL_TYPE_PTR(void_, void*)
XL_TYPE_PTR_1(GLuint_, GLuint*)
XL_TYPE_PTR_1(GLboolean_, GLboolean*)
#if HAVE_GLU
XL_TYPE_PTR(GLUnurbs_, GLUnurbs*)
#endif
#ifdef GLU_VERSION_1_2
XL_TYPE_PTR(GLUtesselator_, GLUtesselator*)
#endif
#if HAVE_GLU
XL_TYPE_PTR(GLUquadric_, GLUquadric*)
#endif
XL_TYPE_PTR_1(GLint_, GLint*)
#if HAVE_GLU
XL_TYPE(_GLUfuncptr, _GLUfuncptr)
#endif


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
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXChooseVisual", "Display*");
  Xen_check_type(Xen_is_int(screen), screen, 2, "glXChooseVisual", "int");
  Xen_check_type(Xen_is_int_(attribList), attribList, 3, "glXChooseVisual", "int*");
  return(C_TO_XEN_XVisualInfo(glXChooseVisual(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen), XEN_TO_C_int_(attribList))));
}

static XEN gxg_glXCopyContext(XEN dpy, XEN src, XEN dst, XEN mask)
{
  #define H_glXCopyContext "void glXCopyContext(Display* dpy, GLXContext src, GLXContext dst, unsigned_long mask)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXCopyContext", "Display*");
  Xen_check_type(Xen_is_GLXContext(src), src, 2, "glXCopyContext", "GLXContext");
  Xen_check_type(Xen_is_GLXContext(dst), dst, 3, "glXCopyContext", "GLXContext");
  Xen_check_type(Xen_is_unsigned_long(mask), mask, 4, "glXCopyContext", "unsigned_long");
  glXCopyContext(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(src), XEN_TO_C_GLXContext(dst), XEN_TO_C_unsigned_long(mask));
  return(Xen_false);
}

static XEN gxg_glXCreateContext(XEN dpy, XEN vis, XEN shareList, XEN direct)
{
  #define H_glXCreateContext "GLXContext glXCreateContext(Display* dpy, XVisualInfo* vis, GLXContext shareList, \
Bool direct)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXCreateContext", "Display*");
  Xen_check_type(Xen_is_XVisualInfo(vis), vis, 2, "glXCreateContext", "XVisualInfo*");
  Xen_check_type(Xen_is_GLXContext(shareList), shareList, 3, "glXCreateContext", "GLXContext");
  Xen_check_type(Xen_is_Bool(direct), direct, 4, "glXCreateContext", "Bool");
  return(C_TO_XEN_GLXContext(glXCreateContext(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_GLXContext(shareList), 
                                              XEN_TO_C_Bool(direct))));
}

static XEN gxg_glXCreateGLXPixmap(XEN dpy, XEN vis, XEN pixmap)
{
  #define H_glXCreateGLXPixmap "GLXPixmap glXCreateGLXPixmap(Display* dpy, XVisualInfo* vis, Pixmap pixmap)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXCreateGLXPixmap", "Display*");
  Xen_check_type(Xen_is_XVisualInfo(vis), vis, 2, "glXCreateGLXPixmap", "XVisualInfo*");
  Xen_check_type(Xen_is_Pixmap(pixmap), pixmap, 3, "glXCreateGLXPixmap", "Pixmap");
  return(C_TO_XEN_GLXPixmap(glXCreateGLXPixmap(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_Pixmap(pixmap))));
}

static XEN gxg_glXDestroyContext(XEN dpy, XEN ctx)
{
  #define H_glXDestroyContext "void glXDestroyContext(Display* dpy, GLXContext ctx)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXDestroyContext", "Display*");
  Xen_check_type(Xen_is_GLXContext(ctx), ctx, 2, "glXDestroyContext", "GLXContext");
  glXDestroyContext(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(ctx));
  return(Xen_false);
}

static XEN gxg_glXDestroyGLXPixmap(XEN dpy, XEN pix)
{
  #define H_glXDestroyGLXPixmap "void glXDestroyGLXPixmap(Display* dpy, GLXPixmap pix)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXDestroyGLXPixmap", "Display*");
  Xen_check_type(Xen_is_GLXPixmap(pix), pix, 2, "glXDestroyGLXPixmap", "GLXPixmap");
  glXDestroyGLXPixmap(XEN_TO_C_Display(dpy), XEN_TO_C_GLXPixmap(pix));
  return(Xen_false);
}

static XEN gxg_glXGetConfig(XEN dpy, XEN vis, XEN attrib, XEN value)
{
  #define H_glXGetConfig "int glXGetConfig(Display* dpy, XVisualInfo* vis, int attrib, int* [value])"
  int ref_value[1];
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXGetConfig", "Display*");
  Xen_check_type(Xen_is_XVisualInfo(vis), vis, 2, "glXGetConfig", "XVisualInfo*");
  Xen_check_type(Xen_is_int(attrib), attrib, 3, "glXGetConfig", "int");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_int(glXGetConfig(XEN_TO_C_Display(dpy), XEN_TO_C_XVisualInfo(vis), XEN_TO_C_int(attrib), ref_value));
    return(Xen_list_2(result, C_TO_XEN_int(ref_value[0])));
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
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXIsDirect", "Display*");
  Xen_check_type(Xen_is_GLXContext(ctx), ctx, 2, "glXIsDirect", "GLXContext");
  return(C_TO_XEN_Bool(glXIsDirect(XEN_TO_C_Display(dpy), XEN_TO_C_GLXContext(ctx))));
}

static XEN gxg_glXMakeCurrent(XEN dpy, XEN drawable, XEN ctx)
{
  #define H_glXMakeCurrent "Bool glXMakeCurrent(Display* dpy, Window drawable, GLXContext ctx)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXMakeCurrent", "Display*");
  Xen_check_type(Xen_is_Window(drawable), drawable, 2, "glXMakeCurrent", "Window");
  Xen_check_type(Xen_is_GLXContext(ctx), ctx, 3, "glXMakeCurrent", "GLXContext");
  return(C_TO_XEN_Bool(glXMakeCurrent(XEN_TO_C_Display(dpy), XEN_TO_C_Window(drawable), XEN_TO_C_GLXContext(ctx))));
}

static XEN gxg_glXQueryExtension(XEN dpy, XEN errorBase, XEN eventBase)
{
  #define H_glXQueryExtension "Bool glXQueryExtension(Display* dpy, int* [errorBase], int* [eventBase])"
  int ref_errorBase[1];
  int ref_eventBase[1];
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXQueryExtension", "Display*");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_Bool(glXQueryExtension(XEN_TO_C_Display(dpy), ref_errorBase, ref_eventBase));
    return(Xen_list_3(result, C_TO_XEN_int(ref_errorBase[0]), C_TO_XEN_int(ref_eventBase[0])));
   }
}

static XEN gxg_glXQueryVersion(XEN dpy, XEN major, XEN minor)
{
  #define H_glXQueryVersion "Bool glXQueryVersion(Display* dpy, int* [major], int* [minor])"
  int ref_major[1];
  int ref_minor[1];
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXQueryVersion", "Display*");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_Bool(glXQueryVersion(XEN_TO_C_Display(dpy), ref_major, ref_minor));
    return(Xen_list_3(result, C_TO_XEN_int(ref_major[0]), C_TO_XEN_int(ref_minor[0])));
   }
}

static XEN gxg_glXSwapBuffers(XEN dpy, XEN drawable)
{
  #define H_glXSwapBuffers "void glXSwapBuffers(Display* dpy, Window drawable)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXSwapBuffers", "Display*");
  Xen_check_type(Xen_is_Window(drawable), drawable, 2, "glXSwapBuffers", "Window");
  glXSwapBuffers(XEN_TO_C_Display(dpy), XEN_TO_C_Window(drawable));
  return(Xen_false);
}

static XEN gxg_glXUseXFont(XEN font, XEN first, XEN count, XEN listBase)
{
  #define H_glXUseXFont "void glXUseXFont(Font font, int first, int count, int listBase)"
  Xen_check_type(Xen_is_Font(font), font, 1, "glXUseXFont", "Font");
  Xen_check_type(Xen_is_int(first), first, 2, "glXUseXFont", "int");
  Xen_check_type(Xen_is_int(count), count, 3, "glXUseXFont", "int");
  Xen_check_type(Xen_is_int(listBase), listBase, 4, "glXUseXFont", "int");
  glXUseXFont(XEN_TO_C_Font(font), XEN_TO_C_int(first), XEN_TO_C_int(count), XEN_TO_C_int(listBase));
  return(Xen_false);
}

static XEN gxg_glXWaitGL(void)
{
  #define H_glXWaitGL "void glXWaitGL( void)"
  glXWaitGL();
  return(Xen_false);
}

static XEN gxg_glXWaitX(void)
{
  #define H_glXWaitX "void glXWaitX( void)"
  glXWaitX();
  return(Xen_false);
}

static XEN gxg_glXGetClientString(XEN dpy, XEN name)
{
  #define H_glXGetClientString "char* glXGetClientString(Display* dpy, int name)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXGetClientString", "Display*");
  Xen_check_type(Xen_is_int(name), name, 2, "glXGetClientString", "int");
  return(C_TO_XEN_char_(glXGetClientString(XEN_TO_C_Display(dpy), XEN_TO_C_int(name))));
}

static XEN gxg_glXQueryServerString(XEN dpy, XEN screen, XEN name)
{
  #define H_glXQueryServerString "char* glXQueryServerString(Display* dpy, int screen, int name)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXQueryServerString", "Display*");
  Xen_check_type(Xen_is_int(screen), screen, 2, "glXQueryServerString", "int");
  Xen_check_type(Xen_is_int(name), name, 3, "glXQueryServerString", "int");
  return(C_TO_XEN_char_(glXQueryServerString(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen), XEN_TO_C_int(name))));
}

static XEN gxg_glXQueryExtensionsString(XEN dpy, XEN screen)
{
  #define H_glXQueryExtensionsString "char* glXQueryExtensionsString(Display* dpy, int screen)"
  Xen_check_type(Xen_is_Display(dpy), dpy, 1, "glXQueryExtensionsString", "Display*");
  Xen_check_type(Xen_is_int(screen), screen, 2, "glXQueryExtensionsString", "int");
  return(C_TO_XEN_char_(glXQueryExtensionsString(XEN_TO_C_Display(dpy), XEN_TO_C_int(screen))));
}

#endif
static XEN gxg_glClearIndex(XEN c)
{
  #define H_glClearIndex "void glClearIndex(GLfloat c)"
  Xen_check_type(Xen_is_GLfloat(c), c, 1, "glClearIndex", "GLfloat");
  glClearIndex(XEN_TO_C_GLfloat(c));
  return(Xen_false);
}

static XEN gxg_glClearColor(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glClearColor "void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"
  Xen_check_type(Xen_is_GLclampf(red), red, 1, "glClearColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(green), green, 2, "glClearColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(blue), blue, 3, "glClearColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(alpha), alpha, 4, "glClearColor", "GLclampf");
  glClearColor(XEN_TO_C_GLclampf(red), XEN_TO_C_GLclampf(green), XEN_TO_C_GLclampf(blue), XEN_TO_C_GLclampf(alpha));
  return(Xen_false);
}

static XEN gxg_glClear(XEN mask)
{
  #define H_glClear "void glClear(GLbitfield mask)"
  Xen_check_type(Xen_is_GLbitfield(mask), mask, 1, "glClear", "GLbitfield");
  glClear(XEN_TO_C_GLbitfield(mask));
  return(Xen_false);
}

static XEN gxg_glIndexMask(XEN mask)
{
  #define H_glIndexMask "void glIndexMask(GLuint mask)"
  Xen_check_type(Xen_is_GLuint(mask), mask, 1, "glIndexMask", "GLuint");
  glIndexMask(XEN_TO_C_GLuint(mask));
  return(Xen_false);
}

static XEN gxg_glColorMask(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColorMask "void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)"
  Xen_check_type(Xen_is_GLboolean(red), red, 1, "glColorMask", "GLboolean");
  Xen_check_type(Xen_is_GLboolean(green), green, 2, "glColorMask", "GLboolean");
  Xen_check_type(Xen_is_GLboolean(blue), blue, 3, "glColorMask", "GLboolean");
  Xen_check_type(Xen_is_GLboolean(alpha), alpha, 4, "glColorMask", "GLboolean");
  glColorMask(XEN_TO_C_GLboolean(red), XEN_TO_C_GLboolean(green), XEN_TO_C_GLboolean(blue), XEN_TO_C_GLboolean(alpha));
  return(Xen_false);
}

static XEN gxg_glAlphaFunc(XEN func, XEN ref)
{
  #define H_glAlphaFunc "void glAlphaFunc(GLenum func, GLclampf ref)"
  Xen_check_type(Xen_is_GLenum(func), func, 1, "glAlphaFunc", "GLenum");
  Xen_check_type(Xen_is_GLclampf(ref), ref, 2, "glAlphaFunc", "GLclampf");
  glAlphaFunc(XEN_TO_C_GLenum(func), XEN_TO_C_GLclampf(ref));
  return(Xen_false);
}

static XEN gxg_glBlendFunc(XEN sfactor, XEN dfactor)
{
  #define H_glBlendFunc "void glBlendFunc(GLenum sfactor, GLenum dfactor)"
  Xen_check_type(Xen_is_GLenum(sfactor), sfactor, 1, "glBlendFunc", "GLenum");
  Xen_check_type(Xen_is_GLenum(dfactor), dfactor, 2, "glBlendFunc", "GLenum");
  glBlendFunc(XEN_TO_C_GLenum(sfactor), XEN_TO_C_GLenum(dfactor));
  return(Xen_false);
}

static XEN gxg_glLogicOp(XEN opcode)
{
  #define H_glLogicOp "void glLogicOp(GLenum opcode)"
  Xen_check_type(Xen_is_GLenum(opcode), opcode, 1, "glLogicOp", "GLenum");
  glLogicOp(XEN_TO_C_GLenum(opcode));
  return(Xen_false);
}

static XEN gxg_glCullFace(XEN mode)
{
  #define H_glCullFace "void glCullFace(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glCullFace", "GLenum");
  glCullFace(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glFrontFace(XEN mode)
{
  #define H_glFrontFace "void glFrontFace(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glFrontFace", "GLenum");
  glFrontFace(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glPointSize(XEN size)
{
  #define H_glPointSize "void glPointSize(GLfloat size)"
  Xen_check_type(Xen_is_GLfloat(size), size, 1, "glPointSize", "GLfloat");
  glPointSize(XEN_TO_C_GLfloat(size));
  return(Xen_false);
}

static XEN gxg_glLineWidth(XEN width)
{
  #define H_glLineWidth "void glLineWidth(GLfloat width)"
  Xen_check_type(Xen_is_GLfloat(width), width, 1, "glLineWidth", "GLfloat");
  glLineWidth(XEN_TO_C_GLfloat(width));
  return(Xen_false);
}

static XEN gxg_glLineStipple(XEN factor, XEN pattern)
{
  #define H_glLineStipple "void glLineStipple(GLint factor, GLushort pattern)"
  Xen_check_type(Xen_is_GLint(factor), factor, 1, "glLineStipple", "GLint");
  Xen_check_type(Xen_is_GLushort(pattern), pattern, 2, "glLineStipple", "GLushort");
  glLineStipple(XEN_TO_C_GLint(factor), XEN_TO_C_GLushort(pattern));
  return(Xen_false);
}

static XEN gxg_glPolygonMode(XEN face, XEN mode)
{
  #define H_glPolygonMode "void glPolygonMode(GLenum face, GLenum mode)"
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glPolygonMode", "GLenum");
  Xen_check_type(Xen_is_GLenum(mode), mode, 2, "glPolygonMode", "GLenum");
  glPolygonMode(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glPolygonOffset(XEN factor, XEN units)
{
  #define H_glPolygonOffset "void glPolygonOffset(GLfloat factor, GLfloat units)"
  Xen_check_type(Xen_is_GLfloat(factor), factor, 1, "glPolygonOffset", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(units), units, 2, "glPolygonOffset", "GLfloat");
  glPolygonOffset(XEN_TO_C_GLfloat(factor), XEN_TO_C_GLfloat(units));
  return(Xen_false);
}

static XEN gxg_glPolygonStipple(XEN mask)
{
  #define H_glPolygonStipple "void glPolygonStipple(GLubyte* mask)"
  Xen_check_type(Xen_is_GLubyte_(mask), mask, 1, "glPolygonStipple", "GLubyte*");
  glPolygonStipple(XEN_TO_C_GLubyte_(mask));
  return(Xen_false);
}

static XEN gxg_glEdgeFlag(XEN flag)
{
  #define H_glEdgeFlag "void glEdgeFlag(GLboolean flag)"
  Xen_check_type(Xen_is_GLboolean(flag), flag, 1, "glEdgeFlag", "GLboolean");
  glEdgeFlag(XEN_TO_C_GLboolean(flag));
  return(Xen_false);
}

static XEN gxg_glScissor(XEN x, XEN y, XEN width, XEN height)
{
  #define H_glScissor "void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glScissor", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glScissor", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glScissor", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glScissor", "GLsizei");
  glScissor(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(Xen_false);
}

static XEN gxg_glClipPlane(XEN plane, XEN equation)
{
  #define H_glClipPlane "void glClipPlane(GLenum plane, GLdouble* equation)"
  Xen_check_type(Xen_is_GLenum(plane), plane, 1, "glClipPlane", "GLenum");
  Xen_check_type(Xen_is_GLdouble_(equation), equation, 2, "glClipPlane", "GLdouble*");
  glClipPlane(XEN_TO_C_GLenum(plane), XEN_TO_C_GLdouble_(equation));
  return(Xen_false);
}

static XEN gxg_glGetClipPlane(XEN plane, XEN equation)
{
  #define H_glGetClipPlane "void glGetClipPlane(GLenum plane, GLdouble* [equation])"
  GLdouble ref_equation[1];
  Xen_check_type(Xen_is_GLenum(plane), plane, 1, "glGetClipPlane", "GLenum");
  glGetClipPlane(XEN_TO_C_GLenum(plane), ref_equation);
  return(Xen_list_1(C_TO_XEN_GLdouble(ref_equation[0])));
}

static XEN gxg_glDrawBuffer(XEN mode)
{
  #define H_glDrawBuffer "void glDrawBuffer(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glDrawBuffer", "GLenum");
  glDrawBuffer(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glReadBuffer(XEN mode)
{
  #define H_glReadBuffer "void glReadBuffer(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glReadBuffer", "GLenum");
  glReadBuffer(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glEnable(XEN cap)
{
  #define H_glEnable "void glEnable(GLenum cap)"
  Xen_check_type(Xen_is_GLenum(cap), cap, 1, "glEnable", "GLenum");
  glEnable(XEN_TO_C_GLenum(cap));
  return(Xen_false);
}

static XEN gxg_glDisable(XEN cap)
{
  #define H_glDisable "void glDisable(GLenum cap)"
  Xen_check_type(Xen_is_GLenum(cap), cap, 1, "glDisable", "GLenum");
  glDisable(XEN_TO_C_GLenum(cap));
  return(Xen_false);
}

static XEN gxg_glIsEnabled(XEN cap)
{
  #define H_glIsEnabled "GLboolean glIsEnabled(GLenum cap)"
  Xen_check_type(Xen_is_GLenum(cap), cap, 1, "glIsEnabled", "GLenum");
  return(C_TO_XEN_GLboolean(glIsEnabled(XEN_TO_C_GLenum(cap))));
}

static XEN gxg_glEnableClientState(XEN cap)
{
  #define H_glEnableClientState "void glEnableClientState(GLenum cap)"
  Xen_check_type(Xen_is_GLenum(cap), cap, 1, "glEnableClientState", "GLenum");
  glEnableClientState(XEN_TO_C_GLenum(cap));
  return(Xen_false);
}

static XEN gxg_glDisableClientState(XEN cap)
{
  #define H_glDisableClientState "void glDisableClientState(GLenum cap)"
  Xen_check_type(Xen_is_GLenum(cap), cap, 1, "glDisableClientState", "GLenum");
  glDisableClientState(XEN_TO_C_GLenum(cap));
  return(Xen_false);
}

static XEN gxg_glGetBooleanv(XEN pname, XEN params)
{
  #define H_glGetBooleanv "void glGetBooleanv(GLenum pname, GLboolean* [params])"
  GLboolean ref_params[16];
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glGetBooleanv", "GLenum");
  glGetBooleanv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = Xen_empty_list;
    for (i = 0; i < vals; i++)
      result = Xen_cons(C_TO_XEN_GLboolean(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetDoublev(XEN pname, XEN params)
{
  #define H_glGetDoublev "void glGetDoublev(GLenum pname, GLdouble* [params])"
  GLdouble ref_params[1];
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glGetDoublev", "GLenum");
  glGetDoublev(XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLdouble(ref_params[0])));
}

static XEN gxg_glGetFloatv(XEN pname, XEN params)
{
  #define H_glGetFloatv "void glGetFloatv(GLenum pname, GLfloat* [params])"
  GLfloat ref_params[16];
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glGetFloatv", "GLenum");
  glGetFloatv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = Xen_empty_list;
    for (i = 0; i < vals; i++)
      result = Xen_cons(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetIntegerv(XEN pname, XEN params)
{
  #define H_glGetIntegerv "void glGetIntegerv(GLenum pname, GLint* [params])"
  GLint ref_params[16];
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glGetIntegerv", "GLenum");
  glGetIntegerv(XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = Xen_empty_list;
    for (i = 0; i < vals; i++)
      result = Xen_cons(C_TO_XEN_GLint(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glPushAttrib(XEN mask)
{
  #define H_glPushAttrib "void glPushAttrib(GLbitfield mask)"
  Xen_check_type(Xen_is_GLbitfield(mask), mask, 1, "glPushAttrib", "GLbitfield");
  glPushAttrib(XEN_TO_C_GLbitfield(mask));
  return(Xen_false);
}

static XEN gxg_glPopAttrib(void)
{
  #define H_glPopAttrib "void glPopAttrib( void)"
  glPopAttrib();
  return(Xen_false);
}

static XEN gxg_glPushClientAttrib(XEN mask)
{
  #define H_glPushClientAttrib "void glPushClientAttrib(GLbitfield mask)"
  Xen_check_type(Xen_is_GLbitfield(mask), mask, 1, "glPushClientAttrib", "GLbitfield");
  glPushClientAttrib(XEN_TO_C_GLbitfield(mask));
  return(Xen_false);
}

static XEN gxg_glPopClientAttrib(void)
{
  #define H_glPopClientAttrib "void glPopClientAttrib( void)"
  glPopClientAttrib();
  return(Xen_false);
}

static XEN gxg_glRenderMode(XEN mode)
{
  #define H_glRenderMode "GLint glRenderMode(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glRenderMode", "GLenum");
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
  Xen_check_type(Xen_is_GLenum(name), name, 1, "glGetString", "GLenum");
  return(C_TO_XEN_constchar_(glGetString(XEN_TO_C_GLenum(name))));
}

static XEN gxg_glFinish(void)
{
  #define H_glFinish "void glFinish( void)"
  glFinish();
  return(Xen_false);
}

static XEN gxg_glFlush(void)
{
  #define H_glFlush "void glFlush( void)"
  glFlush();
  return(Xen_false);
}

static XEN gxg_glHint(XEN target, XEN mode)
{
  #define H_glHint "void glHint(GLenum target, GLenum mode)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glHint", "GLenum");
  Xen_check_type(Xen_is_GLenum(mode), mode, 2, "glHint", "GLenum");
  glHint(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glClearDepth(XEN depth)
{
  #define H_glClearDepth "void glClearDepth(GLclampd depth)"
  Xen_check_type(Xen_is_GLclampd(depth), depth, 1, "glClearDepth", "GLclampd");
  glClearDepth(XEN_TO_C_GLclampd(depth));
  return(Xen_false);
}

static XEN gxg_glDepthFunc(XEN func)
{
  #define H_glDepthFunc "void glDepthFunc(GLenum func)"
  Xen_check_type(Xen_is_GLenum(func), func, 1, "glDepthFunc", "GLenum");
  glDepthFunc(XEN_TO_C_GLenum(func));
  return(Xen_false);
}

static XEN gxg_glDepthMask(XEN flag)
{
  #define H_glDepthMask "void glDepthMask(GLboolean flag)"
  Xen_check_type(Xen_is_GLboolean(flag), flag, 1, "glDepthMask", "GLboolean");
  glDepthMask(XEN_TO_C_GLboolean(flag));
  return(Xen_false);
}

static XEN gxg_glDepthRange(XEN near_val, XEN far_val)
{
  #define H_glDepthRange "void glDepthRange(GLclampd near_val, GLclampd far_val)"
  Xen_check_type(Xen_is_GLclampd(near_val), near_val, 1, "glDepthRange", "GLclampd");
  Xen_check_type(Xen_is_GLclampd(far_val), far_val, 2, "glDepthRange", "GLclampd");
  glDepthRange(XEN_TO_C_GLclampd(near_val), XEN_TO_C_GLclampd(far_val));
  return(Xen_false);
}

static XEN gxg_glClearAccum(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glClearAccum "void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"
  Xen_check_type(Xen_is_GLfloat(red), red, 1, "glClearAccum", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(green), green, 2, "glClearAccum", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(blue), blue, 3, "glClearAccum", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(alpha), alpha, 4, "glClearAccum", "GLfloat");
  glClearAccum(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue), XEN_TO_C_GLfloat(alpha));
  return(Xen_false);
}

static XEN gxg_glAccum(XEN op, XEN value)
{
  #define H_glAccum "void glAccum(GLenum op, GLfloat value)"
  Xen_check_type(Xen_is_GLenum(op), op, 1, "glAccum", "GLenum");
  Xen_check_type(Xen_is_GLfloat(value), value, 2, "glAccum", "GLfloat");
  glAccum(XEN_TO_C_GLenum(op), XEN_TO_C_GLfloat(value));
  return(Xen_false);
}

static XEN gxg_glMatrixMode(XEN mode)
{
  #define H_glMatrixMode "void glMatrixMode(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glMatrixMode", "GLenum");
  glMatrixMode(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glOrtho(XEN left, XEN right, XEN bottom, XEN top, XEN near_val, XEN far_val)
{
  #define H_glOrtho "void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, \
GLdouble far_val)"
  Xen_check_type(Xen_is_GLdouble(left), left, 1, "glOrtho", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(right), right, 2, "glOrtho", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(bottom), bottom, 3, "glOrtho", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(top), top, 4, "glOrtho", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(near_val), near_val, 5, "glOrtho", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(far_val), far_val, 6, "glOrtho", "GLdouble");
  glOrtho(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top), XEN_TO_C_GLdouble(near_val), 
          XEN_TO_C_GLdouble(far_val));
  return(Xen_false);
}

static XEN gxg_glFrustum(XEN left, XEN right, XEN bottom, XEN top, XEN near_val, XEN far_val)
{
  #define H_glFrustum "void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, \
GLdouble far_val)"
  Xen_check_type(Xen_is_GLdouble(left), left, 1, "glFrustum", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(right), right, 2, "glFrustum", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(bottom), bottom, 3, "glFrustum", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(top), top, 4, "glFrustum", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(near_val), near_val, 5, "glFrustum", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(far_val), far_val, 6, "glFrustum", "GLdouble");
  glFrustum(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top), XEN_TO_C_GLdouble(near_val), 
            XEN_TO_C_GLdouble(far_val));
  return(Xen_false);
}

static XEN gxg_glViewport(XEN x, XEN y, XEN width, XEN height)
{
  #define H_glViewport "void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glViewport", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glViewport", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glViewport", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glViewport", "GLsizei");
  glViewport(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(Xen_false);
}

static XEN gxg_glPushMatrix(void)
{
  #define H_glPushMatrix "void glPushMatrix( void)"
  glPushMatrix();
  return(Xen_false);
}

static XEN gxg_glPopMatrix(void)
{
  #define H_glPopMatrix "void glPopMatrix( void)"
  glPopMatrix();
  return(Xen_false);
}

static XEN gxg_glLoadIdentity(void)
{
  #define H_glLoadIdentity "void glLoadIdentity( void)"
  glLoadIdentity();
  return(Xen_false);
}

static XEN gxg_glLoadMatrixd(XEN m)
{
  #define H_glLoadMatrixd "void glLoadMatrixd(GLdouble* m)"
  Xen_check_type(Xen_is_GLdouble_(m), m, 1, "glLoadMatrixd", "GLdouble*");
  glLoadMatrixd(XEN_TO_C_GLdouble_(m));
  return(Xen_false);
}

static XEN gxg_glLoadMatrixf(XEN m)
{
  #define H_glLoadMatrixf "void glLoadMatrixf(GLfloat* m)"
  Xen_check_type(Xen_is_GLfloat_(m), m, 1, "glLoadMatrixf", "GLfloat*");
  glLoadMatrixf(XEN_TO_C_GLfloat_(m));
  return(Xen_false);
}

static XEN gxg_glMultMatrixd(XEN m)
{
  #define H_glMultMatrixd "void glMultMatrixd(GLdouble* m)"
  Xen_check_type(Xen_is_GLdouble_(m), m, 1, "glMultMatrixd", "GLdouble*");
  glMultMatrixd(XEN_TO_C_GLdouble_(m));
  return(Xen_false);
}

static XEN gxg_glMultMatrixf(XEN m)
{
  #define H_glMultMatrixf "void glMultMatrixf(GLfloat* m)"
  Xen_check_type(Xen_is_GLfloat_(m), m, 1, "glMultMatrixf", "GLfloat*");
  glMultMatrixf(XEN_TO_C_GLfloat_(m));
  return(Xen_false);
}

static XEN gxg_glRotated(XEN angle, XEN x, XEN y, XEN z)
{
  #define H_glRotated "void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)"
  Xen_check_type(Xen_is_GLdouble(angle), angle, 1, "glRotated", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(x), x, 2, "glRotated", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 3, "glRotated", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 4, "glRotated", "GLdouble");
  glRotated(XEN_TO_C_GLdouble(angle), XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(Xen_false);
}

static XEN gxg_glRotatef(XEN angle, XEN x, XEN y, XEN z)
{
  #define H_glRotatef "void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)"
  Xen_check_type(Xen_is_GLfloat(angle), angle, 1, "glRotatef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(x), x, 2, "glRotatef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 3, "glRotatef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 4, "glRotatef", "GLfloat");
  glRotatef(XEN_TO_C_GLfloat(angle), XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(Xen_false);
}

static XEN gxg_glScaled(XEN x, XEN y, XEN z)
{
  #define H_glScaled "void glScaled(GLdouble x, GLdouble y, GLdouble z)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glScaled", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glScaled", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glScaled", "GLdouble");
  glScaled(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(Xen_false);
}

static XEN gxg_glScalef(XEN x, XEN y, XEN z)
{
  #define H_glScalef "void glScalef(GLfloat x, GLfloat y, GLfloat z)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glScalef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glScalef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glScalef", "GLfloat");
  glScalef(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(Xen_false);
}

static XEN gxg_glTranslated(XEN x, XEN y, XEN z)
{
  #define H_glTranslated "void glTranslated(GLdouble x, GLdouble y, GLdouble z)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glTranslated", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glTranslated", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glTranslated", "GLdouble");
  glTranslated(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(Xen_false);
}

static XEN gxg_glTranslatef(XEN x, XEN y, XEN z)
{
  #define H_glTranslatef "void glTranslatef(GLfloat x, GLfloat y, GLfloat z)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glTranslatef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glTranslatef", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glTranslatef", "GLfloat");
  glTranslatef(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(Xen_false);
}

static XEN gxg_glIsList(XEN list)
{
  #define H_glIsList "GLboolean glIsList(GLuint list)"
  Xen_check_type(Xen_is_GLuint(list), list, 1, "glIsList", "GLuint");
  return(C_TO_XEN_GLboolean(glIsList(XEN_TO_C_GLuint(list))));
}

static XEN gxg_glDeleteLists(XEN list, XEN range)
{
  #define H_glDeleteLists "void glDeleteLists(GLuint list, GLsizei range)"
  Xen_check_type(Xen_is_GLuint(list), list, 1, "glDeleteLists", "GLuint");
  Xen_check_type(Xen_is_GLsizei(range), range, 2, "glDeleteLists", "GLsizei");
  glDeleteLists(XEN_TO_C_GLuint(list), XEN_TO_C_GLsizei(range));
  return(Xen_false);
}

static XEN gxg_glGenLists(XEN range)
{
  #define H_glGenLists "GLuint glGenLists(GLsizei range)"
  Xen_check_type(Xen_is_GLsizei(range), range, 1, "glGenLists", "GLsizei");
  return(C_TO_XEN_GLuint(glGenLists(XEN_TO_C_GLsizei(range))));
}

static XEN gxg_glNewList(XEN list, XEN mode)
{
  #define H_glNewList "void glNewList(GLuint list, GLenum mode)"
  Xen_check_type(Xen_is_GLuint(list), list, 1, "glNewList", "GLuint");
  Xen_check_type(Xen_is_GLenum(mode), mode, 2, "glNewList", "GLenum");
  glNewList(XEN_TO_C_GLuint(list), XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glEndList(void)
{
  #define H_glEndList "void glEndList( void)"
  glEndList();
  return(Xen_false);
}

static XEN gxg_glCallList(XEN list)
{
  #define H_glCallList "void glCallList(GLuint list)"
  Xen_check_type(Xen_is_GLuint(list), list, 1, "glCallList", "GLuint");
  glCallList(XEN_TO_C_GLuint(list));
  return(Xen_false);
}

static XEN gxg_glCallLists(XEN n, XEN type, XEN lists)
{
  #define H_glCallLists "void glCallLists(GLsizei n, GLenum type, GLvoid* lists)"
  Xen_check_type(Xen_is_GLsizei(n), n, 1, "glCallLists", "GLsizei");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "glCallLists", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(lists), lists, 3, "glCallLists", "GLvoid*");
  glCallLists(XEN_TO_C_GLsizei(n), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(lists));
  return(Xen_false);
}

static XEN gxg_glListBase(XEN base)
{
  #define H_glListBase "void glListBase(GLuint base)"
  Xen_check_type(Xen_is_GLuint(base), base, 1, "glListBase", "GLuint");
  glListBase(XEN_TO_C_GLuint(base));
  return(Xen_false);
}

static XEN gxg_glBegin(XEN mode)
{
  #define H_glBegin "void glBegin(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glBegin", "GLenum");
  glBegin(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glEnd(void)
{
  #define H_glEnd "void glEnd( void)"
  glEnd();
  return(Xen_false);
}

static XEN gxg_glVertex2d(XEN x, XEN y)
{
  #define H_glVertex2d "void glVertex2d(GLdouble x, GLdouble y)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glVertex2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glVertex2d", "GLdouble");
  glVertex2d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y));
  return(Xen_false);
}

static XEN gxg_glVertex2f(XEN x, XEN y)
{
  #define H_glVertex2f "void glVertex2f(GLfloat x, GLfloat y)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glVertex2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glVertex2f", "GLfloat");
  glVertex2f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y));
  return(Xen_false);
}

static XEN gxg_glVertex2i(XEN x, XEN y)
{
  #define H_glVertex2i "void glVertex2i(GLint x, GLint y)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glVertex2i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glVertex2i", "GLint");
  glVertex2i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y));
  return(Xen_false);
}

static XEN gxg_glVertex2s(XEN x, XEN y)
{
  #define H_glVertex2s "void glVertex2s(GLshort x, GLshort y)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glVertex2s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glVertex2s", "GLshort");
  glVertex2s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y));
  return(Xen_false);
}

static XEN gxg_glVertex3d(XEN x, XEN y, XEN z)
{
  #define H_glVertex3d "void glVertex3d(GLdouble x, GLdouble y, GLdouble z)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glVertex3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glVertex3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glVertex3d", "GLdouble");
  glVertex3d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(Xen_false);
}

static XEN gxg_glVertex3f(XEN x, XEN y, XEN z)
{
  #define H_glVertex3f "void glVertex3f(GLfloat x, GLfloat y, GLfloat z)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glVertex3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glVertex3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glVertex3f", "GLfloat");
  glVertex3f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(Xen_false);
}

static XEN gxg_glVertex3i(XEN x, XEN y, XEN z)
{
  #define H_glVertex3i "void glVertex3i(GLint x, GLint y, GLint z)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glVertex3i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glVertex3i", "GLint");
  Xen_check_type(Xen_is_GLint(z), z, 3, "glVertex3i", "GLint");
  glVertex3i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z));
  return(Xen_false);
}

static XEN gxg_glVertex3s(XEN x, XEN y, XEN z)
{
  #define H_glVertex3s "void glVertex3s(GLshort x, GLshort y, GLshort z)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glVertex3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glVertex3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(z), z, 3, "glVertex3s", "GLshort");
  glVertex3s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z));
  return(Xen_false);
}

static XEN gxg_glVertex4d(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4d "void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glVertex4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glVertex4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glVertex4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(w), w, 4, "glVertex4d", "GLdouble");
  glVertex4d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z), XEN_TO_C_GLdouble(w));
  return(Xen_false);
}

static XEN gxg_glVertex4f(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4f "void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glVertex4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glVertex4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glVertex4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(w), w, 4, "glVertex4f", "GLfloat");
  glVertex4f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z), XEN_TO_C_GLfloat(w));
  return(Xen_false);
}

static XEN gxg_glVertex4i(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4i "void glVertex4i(GLint x, GLint y, GLint z, GLint w)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glVertex4i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glVertex4i", "GLint");
  Xen_check_type(Xen_is_GLint(z), z, 3, "glVertex4i", "GLint");
  Xen_check_type(Xen_is_GLint(w), w, 4, "glVertex4i", "GLint");
  glVertex4i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z), XEN_TO_C_GLint(w));
  return(Xen_false);
}

static XEN gxg_glVertex4s(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glVertex4s "void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glVertex4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glVertex4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(z), z, 3, "glVertex4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(w), w, 4, "glVertex4s", "GLshort");
  glVertex4s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z), XEN_TO_C_GLshort(w));
  return(Xen_false);
}

static XEN gxg_glNormal3b(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3b "void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)"
  Xen_check_type(Xen_is_GLbyte(nx), nx, 1, "glNormal3b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(ny), ny, 2, "glNormal3b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(nz), nz, 3, "glNormal3b", "GLbyte");
  glNormal3b(XEN_TO_C_GLbyte(nx), XEN_TO_C_GLbyte(ny), XEN_TO_C_GLbyte(nz));
  return(Xen_false);
}

static XEN gxg_glNormal3d(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3d "void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)"
  Xen_check_type(Xen_is_GLdouble(nx), nx, 1, "glNormal3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(ny), ny, 2, "glNormal3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(nz), nz, 3, "glNormal3d", "GLdouble");
  glNormal3d(XEN_TO_C_GLdouble(nx), XEN_TO_C_GLdouble(ny), XEN_TO_C_GLdouble(nz));
  return(Xen_false);
}

static XEN gxg_glNormal3f(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3f "void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)"
  Xen_check_type(Xen_is_GLfloat(nx), nx, 1, "glNormal3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(ny), ny, 2, "glNormal3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(nz), nz, 3, "glNormal3f", "GLfloat");
  glNormal3f(XEN_TO_C_GLfloat(nx), XEN_TO_C_GLfloat(ny), XEN_TO_C_GLfloat(nz));
  return(Xen_false);
}

static XEN gxg_glNormal3i(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3i "void glNormal3i(GLint nx, GLint ny, GLint nz)"
  Xen_check_type(Xen_is_GLint(nx), nx, 1, "glNormal3i", "GLint");
  Xen_check_type(Xen_is_GLint(ny), ny, 2, "glNormal3i", "GLint");
  Xen_check_type(Xen_is_GLint(nz), nz, 3, "glNormal3i", "GLint");
  glNormal3i(XEN_TO_C_GLint(nx), XEN_TO_C_GLint(ny), XEN_TO_C_GLint(nz));
  return(Xen_false);
}

static XEN gxg_glNormal3s(XEN nx, XEN ny, XEN nz)
{
  #define H_glNormal3s "void glNormal3s(GLshort nx, GLshort ny, GLshort nz)"
  Xen_check_type(Xen_is_GLshort(nx), nx, 1, "glNormal3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(ny), ny, 2, "glNormal3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(nz), nz, 3, "glNormal3s", "GLshort");
  glNormal3s(XEN_TO_C_GLshort(nx), XEN_TO_C_GLshort(ny), XEN_TO_C_GLshort(nz));
  return(Xen_false);
}

static XEN gxg_glIndexd(XEN c)
{
  #define H_glIndexd "void glIndexd(GLdouble c)"
  Xen_check_type(Xen_is_GLdouble(c), c, 1, "glIndexd", "GLdouble");
  glIndexd(XEN_TO_C_GLdouble(c));
  return(Xen_false);
}

static XEN gxg_glIndexf(XEN c)
{
  #define H_glIndexf "void glIndexf(GLfloat c)"
  Xen_check_type(Xen_is_GLfloat(c), c, 1, "glIndexf", "GLfloat");
  glIndexf(XEN_TO_C_GLfloat(c));
  return(Xen_false);
}

static XEN gxg_glIndexi(XEN c)
{
  #define H_glIndexi "void glIndexi(GLint c)"
  Xen_check_type(Xen_is_GLint(c), c, 1, "glIndexi", "GLint");
  glIndexi(XEN_TO_C_GLint(c));
  return(Xen_false);
}

static XEN gxg_glIndexs(XEN c)
{
  #define H_glIndexs "void glIndexs(GLshort c)"
  Xen_check_type(Xen_is_GLshort(c), c, 1, "glIndexs", "GLshort");
  glIndexs(XEN_TO_C_GLshort(c));
  return(Xen_false);
}

static XEN gxg_glIndexub(XEN c)
{
  #define H_glIndexub "void glIndexub(GLubyte c)"
  Xen_check_type(Xen_is_GLubyte(c), c, 1, "glIndexub", "GLubyte");
  glIndexub(XEN_TO_C_GLubyte(c));
  return(Xen_false);
}

static XEN gxg_glColor3b(XEN red, XEN green, XEN blue)
{
  #define H_glColor3b "void glColor3b(GLbyte red, GLbyte green, GLbyte blue)"
  Xen_check_type(Xen_is_GLbyte(red), red, 1, "glColor3b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(green), green, 2, "glColor3b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(blue), blue, 3, "glColor3b", "GLbyte");
  glColor3b(XEN_TO_C_GLbyte(red), XEN_TO_C_GLbyte(green), XEN_TO_C_GLbyte(blue));
  return(Xen_false);
}

static XEN gxg_glColor3d(XEN red, XEN green, XEN blue)
{
  #define H_glColor3d "void glColor3d(GLdouble red, GLdouble green, GLdouble blue)"
  Xen_check_type(Xen_is_GLdouble(red), red, 1, "glColor3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(green), green, 2, "glColor3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(blue), blue, 3, "glColor3d", "GLdouble");
  glColor3d(XEN_TO_C_GLdouble(red), XEN_TO_C_GLdouble(green), XEN_TO_C_GLdouble(blue));
  return(Xen_false);
}

static XEN gxg_glColor3f(XEN red, XEN green, XEN blue)
{
  #define H_glColor3f "void glColor3f(GLfloat red, GLfloat green, GLfloat blue)"
  Xen_check_type(Xen_is_GLfloat(red), red, 1, "glColor3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(green), green, 2, "glColor3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(blue), blue, 3, "glColor3f", "GLfloat");
  glColor3f(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue));
  return(Xen_false);
}

static XEN gxg_glColor3i(XEN red, XEN green, XEN blue)
{
  #define H_glColor3i "void glColor3i(GLint red, GLint green, GLint blue)"
  Xen_check_type(Xen_is_GLint(red), red, 1, "glColor3i", "GLint");
  Xen_check_type(Xen_is_GLint(green), green, 2, "glColor3i", "GLint");
  Xen_check_type(Xen_is_GLint(blue), blue, 3, "glColor3i", "GLint");
  glColor3i(XEN_TO_C_GLint(red), XEN_TO_C_GLint(green), XEN_TO_C_GLint(blue));
  return(Xen_false);
}

static XEN gxg_glColor3s(XEN red, XEN green, XEN blue)
{
  #define H_glColor3s "void glColor3s(GLshort red, GLshort green, GLshort blue)"
  Xen_check_type(Xen_is_GLshort(red), red, 1, "glColor3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(green), green, 2, "glColor3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(blue), blue, 3, "glColor3s", "GLshort");
  glColor3s(XEN_TO_C_GLshort(red), XEN_TO_C_GLshort(green), XEN_TO_C_GLshort(blue));
  return(Xen_false);
}

static XEN gxg_glColor3ub(XEN red, XEN green, XEN blue)
{
  #define H_glColor3ub "void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)"
  Xen_check_type(Xen_is_GLubyte(red), red, 1, "glColor3ub", "GLubyte");
  Xen_check_type(Xen_is_GLubyte(green), green, 2, "glColor3ub", "GLubyte");
  Xen_check_type(Xen_is_GLubyte(blue), blue, 3, "glColor3ub", "GLubyte");
  glColor3ub(XEN_TO_C_GLubyte(red), XEN_TO_C_GLubyte(green), XEN_TO_C_GLubyte(blue));
  return(Xen_false);
}

static XEN gxg_glColor3ui(XEN red, XEN green, XEN blue)
{
  #define H_glColor3ui "void glColor3ui(GLuint red, GLuint green, GLuint blue)"
  Xen_check_type(Xen_is_GLuint(red), red, 1, "glColor3ui", "GLuint");
  Xen_check_type(Xen_is_GLuint(green), green, 2, "glColor3ui", "GLuint");
  Xen_check_type(Xen_is_GLuint(blue), blue, 3, "glColor3ui", "GLuint");
  glColor3ui(XEN_TO_C_GLuint(red), XEN_TO_C_GLuint(green), XEN_TO_C_GLuint(blue));
  return(Xen_false);
}

static XEN gxg_glColor3us(XEN red, XEN green, XEN blue)
{
  #define H_glColor3us "void glColor3us(GLushort red, GLushort green, GLushort blue)"
  Xen_check_type(Xen_is_GLushort(red), red, 1, "glColor3us", "GLushort");
  Xen_check_type(Xen_is_GLushort(green), green, 2, "glColor3us", "GLushort");
  Xen_check_type(Xen_is_GLushort(blue), blue, 3, "glColor3us", "GLushort");
  glColor3us(XEN_TO_C_GLushort(red), XEN_TO_C_GLushort(green), XEN_TO_C_GLushort(blue));
  return(Xen_false);
}

static XEN gxg_glColor4b(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4b "void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)"
  Xen_check_type(Xen_is_GLbyte(red), red, 1, "glColor4b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(green), green, 2, "glColor4b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(blue), blue, 3, "glColor4b", "GLbyte");
  Xen_check_type(Xen_is_GLbyte(alpha), alpha, 4, "glColor4b", "GLbyte");
  glColor4b(XEN_TO_C_GLbyte(red), XEN_TO_C_GLbyte(green), XEN_TO_C_GLbyte(blue), XEN_TO_C_GLbyte(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4d(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4d "void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)"
  Xen_check_type(Xen_is_GLdouble(red), red, 1, "glColor4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(green), green, 2, "glColor4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(blue), blue, 3, "glColor4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(alpha), alpha, 4, "glColor4d", "GLdouble");
  glColor4d(XEN_TO_C_GLdouble(red), XEN_TO_C_GLdouble(green), XEN_TO_C_GLdouble(blue), XEN_TO_C_GLdouble(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4f(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4f "void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"
  Xen_check_type(Xen_is_GLfloat(red), red, 1, "glColor4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(green), green, 2, "glColor4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(blue), blue, 3, "glColor4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(alpha), alpha, 4, "glColor4f", "GLfloat");
  glColor4f(XEN_TO_C_GLfloat(red), XEN_TO_C_GLfloat(green), XEN_TO_C_GLfloat(blue), XEN_TO_C_GLfloat(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4i(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4i "void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)"
  Xen_check_type(Xen_is_GLint(red), red, 1, "glColor4i", "GLint");
  Xen_check_type(Xen_is_GLint(green), green, 2, "glColor4i", "GLint");
  Xen_check_type(Xen_is_GLint(blue), blue, 3, "glColor4i", "GLint");
  Xen_check_type(Xen_is_GLint(alpha), alpha, 4, "glColor4i", "GLint");
  glColor4i(XEN_TO_C_GLint(red), XEN_TO_C_GLint(green), XEN_TO_C_GLint(blue), XEN_TO_C_GLint(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4s(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4s "void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)"
  Xen_check_type(Xen_is_GLshort(red), red, 1, "glColor4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(green), green, 2, "glColor4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(blue), blue, 3, "glColor4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(alpha), alpha, 4, "glColor4s", "GLshort");
  glColor4s(XEN_TO_C_GLshort(red), XEN_TO_C_GLshort(green), XEN_TO_C_GLshort(blue), XEN_TO_C_GLshort(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4ub(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4ub "void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)"
  Xen_check_type(Xen_is_GLubyte(red), red, 1, "glColor4ub", "GLubyte");
  Xen_check_type(Xen_is_GLubyte(green), green, 2, "glColor4ub", "GLubyte");
  Xen_check_type(Xen_is_GLubyte(blue), blue, 3, "glColor4ub", "GLubyte");
  Xen_check_type(Xen_is_GLubyte(alpha), alpha, 4, "glColor4ub", "GLubyte");
  glColor4ub(XEN_TO_C_GLubyte(red), XEN_TO_C_GLubyte(green), XEN_TO_C_GLubyte(blue), XEN_TO_C_GLubyte(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4ui(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4ui "void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)"
  Xen_check_type(Xen_is_GLuint(red), red, 1, "glColor4ui", "GLuint");
  Xen_check_type(Xen_is_GLuint(green), green, 2, "glColor4ui", "GLuint");
  Xen_check_type(Xen_is_GLuint(blue), blue, 3, "glColor4ui", "GLuint");
  Xen_check_type(Xen_is_GLuint(alpha), alpha, 4, "glColor4ui", "GLuint");
  glColor4ui(XEN_TO_C_GLuint(red), XEN_TO_C_GLuint(green), XEN_TO_C_GLuint(blue), XEN_TO_C_GLuint(alpha));
  return(Xen_false);
}

static XEN gxg_glColor4us(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glColor4us "void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)"
  Xen_check_type(Xen_is_GLushort(red), red, 1, "glColor4us", "GLushort");
  Xen_check_type(Xen_is_GLushort(green), green, 2, "glColor4us", "GLushort");
  Xen_check_type(Xen_is_GLushort(blue), blue, 3, "glColor4us", "GLushort");
  Xen_check_type(Xen_is_GLushort(alpha), alpha, 4, "glColor4us", "GLushort");
  glColor4us(XEN_TO_C_GLushort(red), XEN_TO_C_GLushort(green), XEN_TO_C_GLushort(blue), XEN_TO_C_GLushort(alpha));
  return(Xen_false);
}

static XEN gxg_glTexCoord1d(XEN s)
{
  #define H_glTexCoord1d "void glTexCoord1d(GLdouble s)"
  Xen_check_type(Xen_is_GLdouble(s), s, 1, "glTexCoord1d", "GLdouble");
  glTexCoord1d(XEN_TO_C_GLdouble(s));
  return(Xen_false);
}

static XEN gxg_glTexCoord1f(XEN s)
{
  #define H_glTexCoord1f "void glTexCoord1f(GLfloat s)"
  Xen_check_type(Xen_is_GLfloat(s), s, 1, "glTexCoord1f", "GLfloat");
  glTexCoord1f(XEN_TO_C_GLfloat(s));
  return(Xen_false);
}

static XEN gxg_glTexCoord1i(XEN s)
{
  #define H_glTexCoord1i "void glTexCoord1i(GLint s)"
  Xen_check_type(Xen_is_GLint(s), s, 1, "glTexCoord1i", "GLint");
  glTexCoord1i(XEN_TO_C_GLint(s));
  return(Xen_false);
}

static XEN gxg_glTexCoord1s(XEN s)
{
  #define H_glTexCoord1s "void glTexCoord1s(GLshort s)"
  Xen_check_type(Xen_is_GLshort(s), s, 1, "glTexCoord1s", "GLshort");
  glTexCoord1s(XEN_TO_C_GLshort(s));
  return(Xen_false);
}

static XEN gxg_glTexCoord2d(XEN s, XEN t)
{
  #define H_glTexCoord2d "void glTexCoord2d(GLdouble s, GLdouble t)"
  Xen_check_type(Xen_is_GLdouble(s), s, 1, "glTexCoord2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(t), t, 2, "glTexCoord2d", "GLdouble");
  glTexCoord2d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t));
  return(Xen_false);
}

static XEN gxg_glTexCoord2f(XEN s, XEN t)
{
  #define H_glTexCoord2f "void glTexCoord2f(GLfloat s, GLfloat t)"
  Xen_check_type(Xen_is_GLfloat(s), s, 1, "glTexCoord2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(t), t, 2, "glTexCoord2f", "GLfloat");
  glTexCoord2f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t));
  return(Xen_false);
}

static XEN gxg_glTexCoord2i(XEN s, XEN t)
{
  #define H_glTexCoord2i "void glTexCoord2i(GLint s, GLint t)"
  Xen_check_type(Xen_is_GLint(s), s, 1, "glTexCoord2i", "GLint");
  Xen_check_type(Xen_is_GLint(t), t, 2, "glTexCoord2i", "GLint");
  glTexCoord2i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t));
  return(Xen_false);
}

static XEN gxg_glTexCoord2s(XEN s, XEN t)
{
  #define H_glTexCoord2s "void glTexCoord2s(GLshort s, GLshort t)"
  Xen_check_type(Xen_is_GLshort(s), s, 1, "glTexCoord2s", "GLshort");
  Xen_check_type(Xen_is_GLshort(t), t, 2, "glTexCoord2s", "GLshort");
  glTexCoord2s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t));
  return(Xen_false);
}

static XEN gxg_glTexCoord3d(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3d "void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)"
  Xen_check_type(Xen_is_GLdouble(s), s, 1, "glTexCoord3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(t), t, 2, "glTexCoord3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(r), r, 3, "glTexCoord3d", "GLdouble");
  glTexCoord3d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r));
  return(Xen_false);
}

static XEN gxg_glTexCoord3f(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3f "void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)"
  Xen_check_type(Xen_is_GLfloat(s), s, 1, "glTexCoord3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(t), t, 2, "glTexCoord3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(r), r, 3, "glTexCoord3f", "GLfloat");
  glTexCoord3f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r));
  return(Xen_false);
}

static XEN gxg_glTexCoord3i(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3i "void glTexCoord3i(GLint s, GLint t, GLint r)"
  Xen_check_type(Xen_is_GLint(s), s, 1, "glTexCoord3i", "GLint");
  Xen_check_type(Xen_is_GLint(t), t, 2, "glTexCoord3i", "GLint");
  Xen_check_type(Xen_is_GLint(r), r, 3, "glTexCoord3i", "GLint");
  glTexCoord3i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r));
  return(Xen_false);
}

static XEN gxg_glTexCoord3s(XEN s, XEN t, XEN r)
{
  #define H_glTexCoord3s "void glTexCoord3s(GLshort s, GLshort t, GLshort r)"
  Xen_check_type(Xen_is_GLshort(s), s, 1, "glTexCoord3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(t), t, 2, "glTexCoord3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(r), r, 3, "glTexCoord3s", "GLshort");
  glTexCoord3s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r));
  return(Xen_false);
}

static XEN gxg_glTexCoord4d(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4d "void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)"
  Xen_check_type(Xen_is_GLdouble(s), s, 1, "glTexCoord4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(t), t, 2, "glTexCoord4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(r), r, 3, "glTexCoord4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(q), q, 4, "glTexCoord4d", "GLdouble");
  glTexCoord4d(XEN_TO_C_GLdouble(s), XEN_TO_C_GLdouble(t), XEN_TO_C_GLdouble(r), XEN_TO_C_GLdouble(q));
  return(Xen_false);
}

static XEN gxg_glTexCoord4f(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4f "void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)"
  Xen_check_type(Xen_is_GLfloat(s), s, 1, "glTexCoord4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(t), t, 2, "glTexCoord4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(r), r, 3, "glTexCoord4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(q), q, 4, "glTexCoord4f", "GLfloat");
  glTexCoord4f(XEN_TO_C_GLfloat(s), XEN_TO_C_GLfloat(t), XEN_TO_C_GLfloat(r), XEN_TO_C_GLfloat(q));
  return(Xen_false);
}

static XEN gxg_glTexCoord4i(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4i "void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)"
  Xen_check_type(Xen_is_GLint(s), s, 1, "glTexCoord4i", "GLint");
  Xen_check_type(Xen_is_GLint(t), t, 2, "glTexCoord4i", "GLint");
  Xen_check_type(Xen_is_GLint(r), r, 3, "glTexCoord4i", "GLint");
  Xen_check_type(Xen_is_GLint(q), q, 4, "glTexCoord4i", "GLint");
  glTexCoord4i(XEN_TO_C_GLint(s), XEN_TO_C_GLint(t), XEN_TO_C_GLint(r), XEN_TO_C_GLint(q));
  return(Xen_false);
}

static XEN gxg_glTexCoord4s(XEN s, XEN t, XEN r, XEN q)
{
  #define H_glTexCoord4s "void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)"
  Xen_check_type(Xen_is_GLshort(s), s, 1, "glTexCoord4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(t), t, 2, "glTexCoord4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(r), r, 3, "glTexCoord4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(q), q, 4, "glTexCoord4s", "GLshort");
  glTexCoord4s(XEN_TO_C_GLshort(s), XEN_TO_C_GLshort(t), XEN_TO_C_GLshort(r), XEN_TO_C_GLshort(q));
  return(Xen_false);
}

static XEN gxg_glRasterPos2d(XEN x, XEN y)
{
  #define H_glRasterPos2d "void glRasterPos2d(GLdouble x, GLdouble y)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glRasterPos2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glRasterPos2d", "GLdouble");
  glRasterPos2d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y));
  return(Xen_false);
}

static XEN gxg_glRasterPos2f(XEN x, XEN y)
{
  #define H_glRasterPos2f "void glRasterPos2f(GLfloat x, GLfloat y)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glRasterPos2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glRasterPos2f", "GLfloat");
  glRasterPos2f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y));
  return(Xen_false);
}

static XEN gxg_glRasterPos2i(XEN x, XEN y)
{
  #define H_glRasterPos2i "void glRasterPos2i(GLint x, GLint y)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glRasterPos2i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glRasterPos2i", "GLint");
  glRasterPos2i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y));
  return(Xen_false);
}

static XEN gxg_glRasterPos2s(XEN x, XEN y)
{
  #define H_glRasterPos2s "void glRasterPos2s(GLshort x, GLshort y)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glRasterPos2s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glRasterPos2s", "GLshort");
  glRasterPos2s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y));
  return(Xen_false);
}

static XEN gxg_glRasterPos3d(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3d "void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glRasterPos3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glRasterPos3d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glRasterPos3d", "GLdouble");
  glRasterPos3d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z));
  return(Xen_false);
}

static XEN gxg_glRasterPos3f(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3f "void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glRasterPos3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glRasterPos3f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glRasterPos3f", "GLfloat");
  glRasterPos3f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z));
  return(Xen_false);
}

static XEN gxg_glRasterPos3i(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3i "void glRasterPos3i(GLint x, GLint y, GLint z)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glRasterPos3i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glRasterPos3i", "GLint");
  Xen_check_type(Xen_is_GLint(z), z, 3, "glRasterPos3i", "GLint");
  glRasterPos3i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z));
  return(Xen_false);
}

static XEN gxg_glRasterPos3s(XEN x, XEN y, XEN z)
{
  #define H_glRasterPos3s "void glRasterPos3s(GLshort x, GLshort y, GLshort z)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glRasterPos3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glRasterPos3s", "GLshort");
  Xen_check_type(Xen_is_GLshort(z), z, 3, "glRasterPos3s", "GLshort");
  glRasterPos3s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z));
  return(Xen_false);
}

static XEN gxg_glRasterPos4d(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4d "void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "glRasterPos4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "glRasterPos4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(z), z, 3, "glRasterPos4d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(w), w, 4, "glRasterPos4d", "GLdouble");
  glRasterPos4d(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(z), XEN_TO_C_GLdouble(w));
  return(Xen_false);
}

static XEN gxg_glRasterPos4f(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4f "void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"
  Xen_check_type(Xen_is_GLfloat(x), x, 1, "glRasterPos4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y), y, 2, "glRasterPos4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(z), z, 3, "glRasterPos4f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(w), w, 4, "glRasterPos4f", "GLfloat");
  glRasterPos4f(XEN_TO_C_GLfloat(x), XEN_TO_C_GLfloat(y), XEN_TO_C_GLfloat(z), XEN_TO_C_GLfloat(w));
  return(Xen_false);
}

static XEN gxg_glRasterPos4i(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4i "void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glRasterPos4i", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glRasterPos4i", "GLint");
  Xen_check_type(Xen_is_GLint(z), z, 3, "glRasterPos4i", "GLint");
  Xen_check_type(Xen_is_GLint(w), w, 4, "glRasterPos4i", "GLint");
  glRasterPos4i(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLint(z), XEN_TO_C_GLint(w));
  return(Xen_false);
}

static XEN gxg_glRasterPos4s(XEN x, XEN y, XEN z, XEN w)
{
  #define H_glRasterPos4s "void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)"
  Xen_check_type(Xen_is_GLshort(x), x, 1, "glRasterPos4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(y), y, 2, "glRasterPos4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(z), z, 3, "glRasterPos4s", "GLshort");
  Xen_check_type(Xen_is_GLshort(w), w, 4, "glRasterPos4s", "GLshort");
  glRasterPos4s(XEN_TO_C_GLshort(x), XEN_TO_C_GLshort(y), XEN_TO_C_GLshort(z), XEN_TO_C_GLshort(w));
  return(Xen_false);
}

static XEN gxg_glRectd(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRectd "void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)"
  Xen_check_type(Xen_is_GLdouble(x1), x1, 1, "glRectd", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y1), y1, 2, "glRectd", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(x2), x2, 3, "glRectd", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y2), y2, 4, "glRectd", "GLdouble");
  glRectd(XEN_TO_C_GLdouble(x1), XEN_TO_C_GLdouble(y1), XEN_TO_C_GLdouble(x2), XEN_TO_C_GLdouble(y2));
  return(Xen_false);
}

static XEN gxg_glRectf(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRectf "void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)"
  Xen_check_type(Xen_is_GLfloat(x1), x1, 1, "glRectf", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y1), y1, 2, "glRectf", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(x2), x2, 3, "glRectf", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(y2), y2, 4, "glRectf", "GLfloat");
  glRectf(XEN_TO_C_GLfloat(x1), XEN_TO_C_GLfloat(y1), XEN_TO_C_GLfloat(x2), XEN_TO_C_GLfloat(y2));
  return(Xen_false);
}

static XEN gxg_glRecti(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRecti "void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)"
  Xen_check_type(Xen_is_GLint(x1), x1, 1, "glRecti", "GLint");
  Xen_check_type(Xen_is_GLint(y1), y1, 2, "glRecti", "GLint");
  Xen_check_type(Xen_is_GLint(x2), x2, 3, "glRecti", "GLint");
  Xen_check_type(Xen_is_GLint(y2), y2, 4, "glRecti", "GLint");
  glRecti(XEN_TO_C_GLint(x1), XEN_TO_C_GLint(y1), XEN_TO_C_GLint(x2), XEN_TO_C_GLint(y2));
  return(Xen_false);
}

static XEN gxg_glRects(XEN x1, XEN y1, XEN x2, XEN y2)
{
  #define H_glRects "void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)"
  Xen_check_type(Xen_is_GLshort(x1), x1, 1, "glRects", "GLshort");
  Xen_check_type(Xen_is_GLshort(y1), y1, 2, "glRects", "GLshort");
  Xen_check_type(Xen_is_GLshort(x2), x2, 3, "glRects", "GLshort");
  Xen_check_type(Xen_is_GLshort(y2), y2, 4, "glRects", "GLshort");
  glRects(XEN_TO_C_GLshort(x1), XEN_TO_C_GLshort(y1), XEN_TO_C_GLshort(x2), XEN_TO_C_GLshort(y2));
  return(Xen_false);
}

static XEN gxg_glVertexPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glVertexPointer "void glVertexPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLint(size), size, 1, "glVertexPointer", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "glVertexPointer", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 3, "glVertexPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 4, "glVertexPointer", "GLvoid*");
  glVertexPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glNormalPointer(XEN type, XEN stride, XEN ptr)
{
  #define H_glNormalPointer "void glNormalPointer(GLenum type, GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLenum(type), type, 1, "glNormalPointer", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 2, "glNormalPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 3, "glNormalPointer", "GLvoid*");
  glNormalPointer(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glColorPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glColorPointer "void glColorPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLint(size), size, 1, "glColorPointer", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "glColorPointer", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 3, "glColorPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 4, "glColorPointer", "GLvoid*");
  glColorPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glIndexPointer(XEN type, XEN stride, XEN ptr)
{
  #define H_glIndexPointer "void glIndexPointer(GLenum type, GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLenum(type), type, 1, "glIndexPointer", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 2, "glIndexPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 3, "glIndexPointer", "GLvoid*");
  glIndexPointer(XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glTexCoordPointer(XEN size, XEN type, XEN stride, XEN ptr)
{
  #define H_glTexCoordPointer "void glTexCoordPointer(GLint size, GLenum type, GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLint(size), size, 1, "glTexCoordPointer", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "glTexCoordPointer", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 3, "glTexCoordPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 4, "glTexCoordPointer", "GLvoid*");
  glTexCoordPointer(XEN_TO_C_GLint(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glEdgeFlagPointer(XEN stride, XEN ptr)
{
  #define H_glEdgeFlagPointer "void glEdgeFlagPointer(GLsizei stride, GLvoid* ptr)"
  Xen_check_type(Xen_is_GLsizei(stride), stride, 1, "glEdgeFlagPointer", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(ptr), ptr, 2, "glEdgeFlagPointer", "GLvoid*");
  glEdgeFlagPointer(XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(ptr));
  return(Xen_false);
}

static XEN gxg_glGetPointerv(XEN pname, XEN params)
{
  #define H_glGetPointerv "void glGetPointerv(GLenum pname, void** [params])"
  void* ref_params[1];
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glGetPointerv", "GLenum");
  glGetPointerv(XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_void_(ref_params[0])));
}

static XEN gxg_glArrayElement(XEN i)
{
  #define H_glArrayElement "void glArrayElement(GLint i)"
  Xen_check_type(Xen_is_GLint(i), i, 1, "glArrayElement", "GLint");
  glArrayElement(XEN_TO_C_GLint(i));
  return(Xen_false);
}

static XEN gxg_glDrawArrays(XEN mode, XEN first, XEN count)
{
  #define H_glDrawArrays "void glDrawArrays(GLenum mode, GLint first, GLsizei count)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glDrawArrays", "GLenum");
  Xen_check_type(Xen_is_GLint(first), first, 2, "glDrawArrays", "GLint");
  Xen_check_type(Xen_is_GLsizei(count), count, 3, "glDrawArrays", "GLsizei");
  glDrawArrays(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(first), XEN_TO_C_GLsizei(count));
  return(Xen_false);
}

static XEN gxg_glDrawElements(XEN mode, XEN count, XEN type, XEN indices)
{
  #define H_glDrawElements "void glDrawElements(GLenum mode, GLsizei count, GLenum type, GLvoid* indices)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glDrawElements", "GLenum");
  Xen_check_type(Xen_is_GLsizei(count), count, 2, "glDrawElements", "GLsizei");
  Xen_check_type(Xen_is_GLenum(type), type, 3, "glDrawElements", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(indices), indices, 4, "glDrawElements", "GLvoid*");
  glDrawElements(XEN_TO_C_GLenum(mode), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(indices));
  return(Xen_false);
}

static XEN gxg_glInterleavedArrays(XEN format, XEN stride, XEN pointer)
{
  #define H_glInterleavedArrays "void glInterleavedArrays(GLenum format, GLsizei stride, GLvoid* pointer)"
  Xen_check_type(Xen_is_GLenum(format), format, 1, "glInterleavedArrays", "GLenum");
  Xen_check_type(Xen_is_GLsizei(stride), stride, 2, "glInterleavedArrays", "GLsizei");
  Xen_check_type(Xen_is_GLvoid_(pointer), pointer, 3, "glInterleavedArrays", "GLvoid*");
  glInterleavedArrays(XEN_TO_C_GLenum(format), XEN_TO_C_GLsizei(stride), XEN_TO_C_GLvoid_(pointer));
  return(Xen_false);
}

static XEN gxg_glShadeModel(XEN mode)
{
  #define H_glShadeModel "void glShadeModel(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glShadeModel", "GLenum");
  glShadeModel(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glLightf(XEN light, XEN pname, XEN param)
{
  #define H_glLightf "void glLightf(GLenum light, GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(light), light, 1, "glLightf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glLightf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 3, "glLightf", "GLfloat");
  glLightf(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glLighti(XEN light, XEN pname, XEN param)
{
  #define H_glLighti "void glLighti(GLenum light, GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(light), light, 1, "glLighti", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glLighti", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 3, "glLighti", "GLint");
  glLighti(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetLightfv(XEN light, XEN pname, XEN params)
{
  #define H_glGetLightfv "void glGetLightfv(GLenum light, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[16];
  Xen_check_type(Xen_is_GLenum(light), light, 1, "glGetLightfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetLightfv", "GLenum");
  glGetLightfv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = Xen_empty_list;
    for (i = 0; i < vals; i++)
      result = Xen_cons(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetLightiv(XEN light, XEN pname, XEN params)
{
  #define H_glGetLightiv "void glGetLightiv(GLenum light, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(light), light, 1, "glGetLightiv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetLightiv", "GLenum");
  glGetLightiv(XEN_TO_C_GLenum(light), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glLightModelf(XEN pname, XEN param)
{
  #define H_glLightModelf "void glLightModelf(GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glLightModelf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 2, "glLightModelf", "GLfloat");
  glLightModelf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glLightModeli(XEN pname, XEN param)
{
  #define H_glLightModeli "void glLightModeli(GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glLightModeli", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 2, "glLightModeli", "GLint");
  glLightModeli(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glMaterialf(XEN face, XEN pname, XEN param)
{
  #define H_glMaterialf "void glMaterialf(GLenum face, GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glMaterialf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glMaterialf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 3, "glMaterialf", "GLfloat");
  glMaterialf(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glMateriali(XEN face, XEN pname, XEN param)
{
  #define H_glMateriali "void glMateriali(GLenum face, GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glMateriali", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glMateriali", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 3, "glMateriali", "GLint");
  glMateriali(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetMaterialfv(XEN face, XEN pname, XEN params)
{
  #define H_glGetMaterialfv "void glGetMaterialfv(GLenum face, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[16];
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glGetMaterialfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetMaterialfv", "GLenum");
  glGetMaterialfv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), ref_params);
  {
    XEN result;
    int i, vals;
    vals = how_many_vals(XEN_TO_C_GLenum(pname));
    result = Xen_empty_list;
    for (i = 0; i < vals; i++)
      result = Xen_cons(C_TO_XEN_GLfloat(ref_params[i]), result);
    return(result);
  }
}

static XEN gxg_glGetMaterialiv(XEN face, XEN pname, XEN params)
{
  #define H_glGetMaterialiv "void glGetMaterialiv(GLenum face, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glGetMaterialiv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetMaterialiv", "GLenum");
  glGetMaterialiv(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glColorMaterial(XEN face, XEN mode)
{
  #define H_glColorMaterial "void glColorMaterial(GLenum face, GLenum mode)"
  Xen_check_type(Xen_is_GLenum(face), face, 1, "glColorMaterial", "GLenum");
  Xen_check_type(Xen_is_GLenum(mode), mode, 2, "glColorMaterial", "GLenum");
  glColorMaterial(XEN_TO_C_GLenum(face), XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glPixelZoom(XEN xfactor, XEN yfactor)
{
  #define H_glPixelZoom "void glPixelZoom(GLfloat xfactor, GLfloat yfactor)"
  Xen_check_type(Xen_is_GLfloat(xfactor), xfactor, 1, "glPixelZoom", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(yfactor), yfactor, 2, "glPixelZoom", "GLfloat");
  glPixelZoom(XEN_TO_C_GLfloat(xfactor), XEN_TO_C_GLfloat(yfactor));
  return(Xen_false);
}

static XEN gxg_glPixelStoref(XEN pname, XEN param)
{
  #define H_glPixelStoref "void glPixelStoref(GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glPixelStoref", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 2, "glPixelStoref", "GLfloat");
  glPixelStoref(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glPixelStorei(XEN pname, XEN param)
{
  #define H_glPixelStorei "void glPixelStorei(GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glPixelStorei", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 2, "glPixelStorei", "GLint");
  glPixelStorei(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glPixelTransferf(XEN pname, XEN param)
{
  #define H_glPixelTransferf "void glPixelTransferf(GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glPixelTransferf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 2, "glPixelTransferf", "GLfloat");
  glPixelTransferf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glPixelTransferi(XEN pname, XEN param)
{
  #define H_glPixelTransferi "void glPixelTransferi(GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glPixelTransferi", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 2, "glPixelTransferi", "GLint");
  glPixelTransferi(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetPixelMapfv(XEN map, XEN values)
{
  #define H_glGetPixelMapfv "void glGetPixelMapfv(GLenum map, GLfloat* [values])"
  GLfloat ref_values[1];
  Xen_check_type(Xen_is_GLenum(map), map, 1, "glGetPixelMapfv", "GLenum");
  glGetPixelMapfv(XEN_TO_C_GLenum(map), ref_values);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_values[0])));
}

static XEN gxg_glGetPixelMapuiv(XEN map, XEN values)
{
  #define H_glGetPixelMapuiv "void glGetPixelMapuiv(GLenum map, GLuint* [values])"
  GLuint ref_values[1];
  Xen_check_type(Xen_is_GLenum(map), map, 1, "glGetPixelMapuiv", "GLenum");
  glGetPixelMapuiv(XEN_TO_C_GLenum(map), ref_values);
  return(Xen_list_1(C_TO_XEN_GLuint(ref_values[0])));
}

static XEN gxg_glGetPixelMapusv(XEN map, XEN values)
{
  #define H_glGetPixelMapusv "void glGetPixelMapusv(GLenum map, GLushort* [values])"
  GLushort ref_values[1];
  Xen_check_type(Xen_is_GLenum(map), map, 1, "glGetPixelMapusv", "GLenum");
  glGetPixelMapusv(XEN_TO_C_GLenum(map), ref_values);
  return(Xen_list_1(C_TO_XEN_GLushort(ref_values[0])));
}

static XEN gxg_glBitmap(XEN width, XEN height, XEN xorig, XEN yorig, XEN xmove, XEN ymove, XEN bitmap)
{
  #define H_glBitmap "void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, \
GLfloat ymove, GLubyte* bitmap)"
  Xen_check_type(Xen_is_GLsizei(width), width, 1, "glBitmap", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 2, "glBitmap", "GLsizei");
  Xen_check_type(Xen_is_GLfloat(xorig), xorig, 3, "glBitmap", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(yorig), yorig, 4, "glBitmap", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(xmove), xmove, 5, "glBitmap", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(ymove), ymove, 6, "glBitmap", "GLfloat");
  Xen_check_type(Xen_is_GLubyte_(bitmap), bitmap, 7, "glBitmap", "GLubyte*");
  glBitmap(XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLfloat(xorig), XEN_TO_C_GLfloat(yorig), XEN_TO_C_GLfloat(xmove), 
           XEN_TO_C_GLfloat(ymove), XEN_TO_C_GLubyte_(bitmap));
  return(Xen_false);
}

static XEN gxg_glReadPixels(XEN x, XEN y, XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glReadPixels "void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, \
GLenum type, GLvoid* pixels)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glReadPixels", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glReadPixels", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glReadPixels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glReadPixels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "glReadPixels", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "glReadPixels", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 7, "glReadPixels", "GLvoid*");
  glReadPixels(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), 
               XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glDrawPixels(XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glDrawPixels "void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type, \
GLvoid* pixels)"
  Xen_check_type(Xen_is_GLsizei(width), width, 1, "glDrawPixels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 2, "glDrawPixels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 3, "glDrawPixels", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 4, "glDrawPixels", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 5, "glDrawPixels", "GLvoid*");
  glDrawPixels(XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glCopyPixels(XEN x, XEN y, XEN width, XEN height, XEN type)
{
  #define H_glCopyPixels "void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)"
  Xen_check_type(Xen_is_GLint(x), x, 1, "glCopyPixels", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 2, "glCopyPixels", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glCopyPixels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glCopyPixels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "glCopyPixels", "GLenum");
  glCopyPixels(XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(type));
  return(Xen_false);
}

static XEN gxg_glStencilFunc(XEN func, XEN ref, XEN mask)
{
  #define H_glStencilFunc "void glStencilFunc(GLenum func, GLint ref, GLuint mask)"
  Xen_check_type(Xen_is_GLenum(func), func, 1, "glStencilFunc", "GLenum");
  Xen_check_type(Xen_is_GLint(ref), ref, 2, "glStencilFunc", "GLint");
  Xen_check_type(Xen_is_GLuint(mask), mask, 3, "glStencilFunc", "GLuint");
  glStencilFunc(XEN_TO_C_GLenum(func), XEN_TO_C_GLint(ref), XEN_TO_C_GLuint(mask));
  return(Xen_false);
}

static XEN gxg_glStencilMask(XEN mask)
{
  #define H_glStencilMask "void glStencilMask(GLuint mask)"
  Xen_check_type(Xen_is_GLuint(mask), mask, 1, "glStencilMask", "GLuint");
  glStencilMask(XEN_TO_C_GLuint(mask));
  return(Xen_false);
}

static XEN gxg_glStencilOp(XEN fail, XEN zfail, XEN zpass)
{
  #define H_glStencilOp "void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)"
  Xen_check_type(Xen_is_GLenum(fail), fail, 1, "glStencilOp", "GLenum");
  Xen_check_type(Xen_is_GLenum(zfail), zfail, 2, "glStencilOp", "GLenum");
  Xen_check_type(Xen_is_GLenum(zpass), zpass, 3, "glStencilOp", "GLenum");
  glStencilOp(XEN_TO_C_GLenum(fail), XEN_TO_C_GLenum(zfail), XEN_TO_C_GLenum(zpass));
  return(Xen_false);
}

static XEN gxg_glClearStencil(XEN s)
{
  #define H_glClearStencil "void glClearStencil(GLint s)"
  Xen_check_type(Xen_is_GLint(s), s, 1, "glClearStencil", "GLint");
  glClearStencil(XEN_TO_C_GLint(s));
  return(Xen_false);
}

static XEN gxg_glTexGend(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGend "void glTexGend(GLenum coord, GLenum pname, GLdouble param)"
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glTexGend", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexGend", "GLenum");
  Xen_check_type(Xen_is_GLdouble(param), param, 3, "glTexGend", "GLdouble");
  glTexGend(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLdouble(param));
  return(Xen_false);
}

static XEN gxg_glTexGenf(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGenf "void glTexGenf(GLenum coord, GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glTexGenf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexGenf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 3, "glTexGenf", "GLfloat");
  glTexGenf(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glTexGeni(XEN coord, XEN pname, XEN param)
{
  #define H_glTexGeni "void glTexGeni(GLenum coord, GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glTexGeni", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexGeni", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 3, "glTexGeni", "GLint");
  glTexGeni(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetTexGendv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGendv "void glGetTexGendv(GLenum coord, GLenum pname, GLdouble* [params])"
  GLdouble ref_params[1];
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glGetTexGendv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexGendv", "GLenum");
  glGetTexGendv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLdouble(ref_params[0])));
}

static XEN gxg_glGetTexGenfv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGenfv "void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glGetTexGenfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexGenfv", "GLenum");
  glGetTexGenfv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexGeniv(XEN coord, XEN pname, XEN params)
{
  #define H_glGetTexGeniv "void glGetTexGeniv(GLenum coord, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(coord), coord, 1, "glGetTexGeniv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexGeniv", "GLenum");
  glGetTexGeniv(XEN_TO_C_GLenum(coord), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glTexEnvf(XEN target, XEN pname, XEN param)
{
  #define H_glTexEnvf "void glTexEnvf(GLenum target, GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexEnvf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexEnvf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 3, "glTexEnvf", "GLfloat");
  glTexEnvf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glTexEnvi(XEN target, XEN pname, XEN param)
{
  #define H_glTexEnvi "void glTexEnvi(GLenum target, GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexEnvi", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexEnvi", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 3, "glTexEnvi", "GLint");
  glTexEnvi(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetTexEnvfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexEnvfv "void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexEnvfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexEnvfv", "GLenum");
  glGetTexEnvfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexEnviv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexEnviv "void glGetTexEnviv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexEnviv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexEnviv", "GLenum");
  glGetTexEnviv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glTexParameterf(XEN target, XEN pname, XEN param)
{
  #define H_glTexParameterf "void glTexParameterf(GLenum target, GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexParameterf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexParameterf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 3, "glTexParameterf", "GLfloat");
  glTexParameterf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glTexParameteri(XEN target, XEN pname, XEN param)
{
  #define H_glTexParameteri "void glTexParameteri(GLenum target, GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexParameteri", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glTexParameteri", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 3, "glTexParameteri", "GLint");
  glTexParameteri(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glGetTexParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexParameterfv "void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexParameterfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexParameterfv", "GLenum");
  glGetTexParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetTexParameteriv "void glGetTexParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexParameteriv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetTexParameteriv", "GLenum");
  glGetTexParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glGetTexLevelParameterfv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameterfv "void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexLevelParameterfv", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glGetTexLevelParameterfv", "GLint");
  Xen_check_type(Xen_is_GLenum(pname), pname, 3, "glGetTexLevelParameterfv", "GLenum");
  glGetTexLevelParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetTexLevelParameteriv(XEN target, XEN level, XEN pname, XEN params)
{
  #define H_glGetTexLevelParameteriv "void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, \
GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetTexLevelParameteriv", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glGetTexLevelParameteriv", "GLint");
  Xen_check_type(Xen_is_GLenum(pname), pname, 3, "glGetTexLevelParameteriv", "GLenum");
  glGetTexLevelParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glTexImage1D(XEN target, XEN level, XEN internalFormat, XEN width, XEN border, XEN format, XEN type, XEN pixels)
{
  #define H_glTexImage1D "void glTexImage1D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexImage1D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 3, "glTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 4, "glTexImage1D", "GLsizei");
  Xen_check_type(Xen_is_GLint(border), border, 5, "glTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLenum(format), format, 6, "glTexImage1D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 7, "glTexImage1D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 8, "glTexImage1D", "GLvoid*");
  glTexImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLint(border), 
               XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glTexImage2D(XEN target, XEN level, XEN internalFormat, XEN width, XEN height, XEN border, XEN format, XEN type, XEN pixels)
{
  #define H_glTexImage2D "void glTexImage2D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLsizei height, GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexImage2D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 3, "glTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 4, "glTexImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 5, "glTexImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLint(border), border, 6, "glTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLenum(format), format, 7, "glTexImage2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 8, "glTexImage2D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 9, "glTexImage2D", "GLvoid*");
  glTexImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
               XEN_TO_C_GLint(border), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glGenTextures(XEN n, XEN textures)
{
  #define H_glGenTextures "void glGenTextures(GLsizei n, GLuint* textures)"
  Xen_check_type(Xen_is_GLsizei(n), n, 1, "glGenTextures", "GLsizei");
  Xen_check_type(Xen_is_GLuint_(textures), textures, 2, "glGenTextures", "GLuint*");
  glGenTextures(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(Xen_false);
}

static XEN gxg_glDeleteTextures(XEN n, XEN textures)
{
  #define H_glDeleteTextures "void glDeleteTextures(GLsizei n, GLuint* textures)"
  Xen_check_type(Xen_is_GLsizei(n), n, 1, "glDeleteTextures", "GLsizei");
  Xen_check_type(Xen_is_GLuint_(textures), textures, 2, "glDeleteTextures", "GLuint*");
  glDeleteTextures(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures));
  return(Xen_false);
}

static XEN gxg_glBindTexture(XEN target, XEN texture)
{
  #define H_glBindTexture "void glBindTexture(GLenum target, GLuint texture)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glBindTexture", "GLenum");
  Xen_check_type(Xen_is_GLuint(texture), texture, 2, "glBindTexture", "GLuint");
  glBindTexture(XEN_TO_C_GLenum(target), XEN_TO_C_GLuint(texture));
  return(Xen_false);
}

static XEN gxg_glAreTexturesResident(XEN n, XEN textures, XEN residences)
{
  #define H_glAreTexturesResident "GLboolean glAreTexturesResident(GLsizei n, GLuint* textures, GLboolean* residences)"
  Xen_check_type(Xen_is_GLsizei(n), n, 1, "glAreTexturesResident", "GLsizei");
  Xen_check_type(Xen_is_GLuint_(textures), textures, 2, "glAreTexturesResident", "GLuint*");
  Xen_check_type(Xen_is_GLboolean_(residences), residences, 3, "glAreTexturesResident", "GLboolean*");
  return(C_TO_XEN_GLboolean(glAreTexturesResident(XEN_TO_C_GLsizei(n), XEN_TO_C_GLuint_(textures), XEN_TO_C_GLboolean_(residences))));
}

static XEN gxg_glIsTexture(XEN texture)
{
  #define H_glIsTexture "GLboolean glIsTexture(GLuint texture)"
  Xen_check_type(Xen_is_GLuint(texture), texture, 1, "glIsTexture", "GLuint");
  return(C_TO_XEN_GLboolean(glIsTexture(XEN_TO_C_GLuint(texture))));
}

static XEN gxg_glTexSubImage1D(XEN target, XEN level, XEN xoffset, XEN width, XEN format, XEN type, XEN pixels)
{
  #define H_glTexSubImage1D "void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, \
GLenum format, GLenum type, GLvoid* pixels)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexSubImage1D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 4, "glTexSubImage1D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "glTexSubImage1D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "glTexSubImage1D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 7, "glTexSubImage1D", "GLvoid*");
  glTexSubImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
                  XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glTexSubImage2D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN width, XEN height, XEN format, XEN type, XEN pixels)
{
  #define H_glTexSubImage2D "void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, \
GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexSubImage2D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(yoffset), yoffset, 4, "glTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 5, "glTexSubImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 6, "glTexSubImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 7, "glTexSubImage2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 8, "glTexSubImage2D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 9, "glTexSubImage2D", "GLvoid*");
  glTexSubImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLsizei(width), 
                  XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glCopyTexImage1D(XEN target, XEN level, XEN internalformat, XEN x, XEN y, XEN width, XEN border)
{
  #define H_glCopyTexImage1D "void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLint border)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyTexImage1D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glCopyTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 3, "glCopyTexImage1D", "GLenum");
  Xen_check_type(Xen_is_GLint(x), x, 4, "glCopyTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 5, "glCopyTexImage1D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 6, "glCopyTexImage1D", "GLsizei");
  Xen_check_type(Xen_is_GLint(border), border, 7, "glCopyTexImage1D", "GLint");
  glCopyTexImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                   XEN_TO_C_GLsizei(width), XEN_TO_C_GLint(border));
  return(Xen_false);
}

static XEN gxg_glCopyTexImage2D(XEN target, XEN level, XEN internalformat, XEN x, XEN y, XEN width, XEN height, XEN border)
{
  #define H_glCopyTexImage2D "void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyTexImage2D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glCopyTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 3, "glCopyTexImage2D", "GLenum");
  Xen_check_type(Xen_is_GLint(x), x, 4, "glCopyTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 5, "glCopyTexImage2D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 6, "glCopyTexImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 7, "glCopyTexImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLint(border), border, 8, "glCopyTexImage2D", "GLint");
  glCopyTexImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                   XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLint(border));
  return(Xen_false);
}

static XEN gxg_glCopyTexSubImage1D(XEN target, XEN level, XEN xoffset, XEN x, XEN y, XEN width)
{
  #define H_glCopyTexSubImage1D "void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, \
GLint x, GLint y, GLsizei width)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyTexSubImage1D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glCopyTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glCopyTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(x), x, 4, "glCopyTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 5, "glCopyTexSubImage1D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 6, "glCopyTexSubImage1D", "GLsizei");
  glCopyTexSubImage1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                      XEN_TO_C_GLsizei(width));
  return(Xen_false);
}

static XEN gxg_glCopyTexSubImage2D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyTexSubImage2D "void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, \
GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyTexSubImage2D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glCopyTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glCopyTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(yoffset), yoffset, 4, "glCopyTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(x), x, 5, "glCopyTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 6, "glCopyTexSubImage2D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 7, "glCopyTexSubImage2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 8, "glCopyTexSubImage2D", "GLsizei");
  glCopyTexSubImage2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(x), 
                      XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(Xen_false);
}

static XEN gxg_glMap1d(XEN target, XEN u1, XEN u2, XEN stride, XEN order, XEN points)
{
  #define H_glMap1d "void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, \
GLdouble* points)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glMap1d", "GLenum");
  Xen_check_type(Xen_is_GLdouble(u1), u1, 2, "glMap1d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(u2), u2, 3, "glMap1d", "GLdouble");
  Xen_check_type(Xen_is_GLint(stride), stride, 4, "glMap1d", "GLint");
  Xen_check_type(Xen_is_GLint(order), order, 5, "glMap1d", "GLint");
  Xen_check_type(Xen_is_GLdouble_(points), points, 6, "glMap1d", "GLdouble*");
  glMap1d(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(stride), XEN_TO_C_GLint(order), 
          XEN_TO_C_GLdouble_(points));
  return(Xen_false);
}

static XEN gxg_glMap1f(XEN target, XEN u1, XEN u2, XEN stride, XEN order, XEN points)
{
  #define H_glMap1f "void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, GLfloat* points)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glMap1f", "GLenum");
  Xen_check_type(Xen_is_GLfloat(u1), u1, 2, "glMap1f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(u2), u2, 3, "glMap1f", "GLfloat");
  Xen_check_type(Xen_is_GLint(stride), stride, 4, "glMap1f", "GLint");
  Xen_check_type(Xen_is_GLint(order), order, 5, "glMap1f", "GLint");
  Xen_check_type(Xen_is_GLfloat_(points), points, 6, "glMap1f", "GLfloat*");
  glMap1f(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(stride), XEN_TO_C_GLint(order), 
          XEN_TO_C_GLfloat_(points));
  return(Xen_false);
}

static XEN gxg_glMap2d(XEN arglist)
{
  #define H_glMap2d "void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, \
GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, GLdouble* points)"
  XEN target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points;
  target = Xen_list_ref(arglist, 0);
  u1 = Xen_list_ref(arglist, 1);
  u2 = Xen_list_ref(arglist, 2);
  ustride = Xen_list_ref(arglist, 3);
  uorder = Xen_list_ref(arglist, 4);
  v1 = Xen_list_ref(arglist, 5);
  v2 = Xen_list_ref(arglist, 6);
  vstride = Xen_list_ref(arglist, 7);
  vorder = Xen_list_ref(arglist, 8);
  points = Xen_list_ref(arglist, 9);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glMap2d", "GLenum");
  Xen_check_type(Xen_is_GLdouble(u1), u1, 2, "glMap2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(u2), u2, 3, "glMap2d", "GLdouble");
  Xen_check_type(Xen_is_GLint(ustride), ustride, 4, "glMap2d", "GLint");
  Xen_check_type(Xen_is_GLint(uorder), uorder, 5, "glMap2d", "GLint");
  Xen_check_type(Xen_is_GLdouble(v1), v1, 6, "glMap2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(v2), v2, 7, "glMap2d", "GLdouble");
  Xen_check_type(Xen_is_GLint(vstride), vstride, 8, "glMap2d", "GLint");
  Xen_check_type(Xen_is_GLint(vorder), vorder, 9, "glMap2d", "GLint");
  Xen_check_type(Xen_is_GLdouble_(points), points, 10, "glMap2d", "GLdouble*");
  glMap2d(XEN_TO_C_GLenum(target), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(ustride), XEN_TO_C_GLint(uorder), 
          XEN_TO_C_GLdouble(v1), XEN_TO_C_GLdouble(v2), XEN_TO_C_GLint(vstride), XEN_TO_C_GLint(vorder), XEN_TO_C_GLdouble_(points));
  return(Xen_false);
}

static XEN gxg_glMap2f(XEN arglist)
{
  #define H_glMap2f "void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, \
GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, GLfloat* points)"
  XEN target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points;
  target = Xen_list_ref(arglist, 0);
  u1 = Xen_list_ref(arglist, 1);
  u2 = Xen_list_ref(arglist, 2);
  ustride = Xen_list_ref(arglist, 3);
  uorder = Xen_list_ref(arglist, 4);
  v1 = Xen_list_ref(arglist, 5);
  v2 = Xen_list_ref(arglist, 6);
  vstride = Xen_list_ref(arglist, 7);
  vorder = Xen_list_ref(arglist, 8);
  points = Xen_list_ref(arglist, 9);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glMap2f", "GLenum");
  Xen_check_type(Xen_is_GLfloat(u1), u1, 2, "glMap2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(u2), u2, 3, "glMap2f", "GLfloat");
  Xen_check_type(Xen_is_GLint(ustride), ustride, 4, "glMap2f", "GLint");
  Xen_check_type(Xen_is_GLint(uorder), uorder, 5, "glMap2f", "GLint");
  Xen_check_type(Xen_is_GLfloat(v1), v1, 6, "glMap2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(v2), v2, 7, "glMap2f", "GLfloat");
  Xen_check_type(Xen_is_GLint(vstride), vstride, 8, "glMap2f", "GLint");
  Xen_check_type(Xen_is_GLint(vorder), vorder, 9, "glMap2f", "GLint");
  Xen_check_type(Xen_is_GLfloat_(points), points, 10, "glMap2f", "GLfloat*");
  glMap2f(XEN_TO_C_GLenum(target), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(ustride), XEN_TO_C_GLint(uorder), 
          XEN_TO_C_GLfloat(v1), XEN_TO_C_GLfloat(v2), XEN_TO_C_GLint(vstride), XEN_TO_C_GLint(vorder), XEN_TO_C_GLfloat_(points));
  return(Xen_false);
}

static XEN gxg_glGetMapdv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapdv "void glGetMapdv(GLenum target, GLenum query, GLdouble* [v])"
  GLdouble ref_v[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMapdv", "GLenum");
  Xen_check_type(Xen_is_GLenum(query), query, 2, "glGetMapdv", "GLenum");
  glGetMapdv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(Xen_list_1(C_TO_XEN_GLdouble(ref_v[0])));
}

static XEN gxg_glGetMapfv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapfv "void glGetMapfv(GLenum target, GLenum query, GLfloat* [v])"
  GLfloat ref_v[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMapfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(query), query, 2, "glGetMapfv", "GLenum");
  glGetMapfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_v[0])));
}

static XEN gxg_glGetMapiv(XEN target, XEN query, XEN v)
{
  #define H_glGetMapiv "void glGetMapiv(GLenum target, GLenum query, GLint* [v])"
  GLint ref_v[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMapiv", "GLenum");
  Xen_check_type(Xen_is_GLenum(query), query, 2, "glGetMapiv", "GLenum");
  glGetMapiv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(query), ref_v);
  return(Xen_list_1(C_TO_XEN_GLint(ref_v[0])));
}

static XEN gxg_glEvalCoord1d(XEN u)
{
  #define H_glEvalCoord1d "void glEvalCoord1d(GLdouble u)"
  Xen_check_type(Xen_is_GLdouble(u), u, 1, "glEvalCoord1d", "GLdouble");
  glEvalCoord1d(XEN_TO_C_GLdouble(u));
  return(Xen_false);
}

static XEN gxg_glEvalCoord1f(XEN u)
{
  #define H_glEvalCoord1f "void glEvalCoord1f(GLfloat u)"
  Xen_check_type(Xen_is_GLfloat(u), u, 1, "glEvalCoord1f", "GLfloat");
  glEvalCoord1f(XEN_TO_C_GLfloat(u));
  return(Xen_false);
}

static XEN gxg_glEvalCoord2d(XEN u, XEN v)
{
  #define H_glEvalCoord2d "void glEvalCoord2d(GLdouble u, GLdouble v)"
  Xen_check_type(Xen_is_GLdouble(u), u, 1, "glEvalCoord2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(v), v, 2, "glEvalCoord2d", "GLdouble");
  glEvalCoord2d(XEN_TO_C_GLdouble(u), XEN_TO_C_GLdouble(v));
  return(Xen_false);
}

static XEN gxg_glEvalCoord2f(XEN u, XEN v)
{
  #define H_glEvalCoord2f "void glEvalCoord2f(GLfloat u, GLfloat v)"
  Xen_check_type(Xen_is_GLfloat(u), u, 1, "glEvalCoord2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(v), v, 2, "glEvalCoord2f", "GLfloat");
  glEvalCoord2f(XEN_TO_C_GLfloat(u), XEN_TO_C_GLfloat(v));
  return(Xen_false);
}

static XEN gxg_glMapGrid1d(XEN un, XEN u1, XEN u2)
{
  #define H_glMapGrid1d "void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)"
  Xen_check_type(Xen_is_GLint(un), un, 1, "glMapGrid1d", "GLint");
  Xen_check_type(Xen_is_GLdouble(u1), u1, 2, "glMapGrid1d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(u2), u2, 3, "glMapGrid1d", "GLdouble");
  glMapGrid1d(XEN_TO_C_GLint(un), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2));
  return(Xen_false);
}

static XEN gxg_glMapGrid1f(XEN un, XEN u1, XEN u2)
{
  #define H_glMapGrid1f "void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)"
  Xen_check_type(Xen_is_GLint(un), un, 1, "glMapGrid1f", "GLint");
  Xen_check_type(Xen_is_GLfloat(u1), u1, 2, "glMapGrid1f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(u2), u2, 3, "glMapGrid1f", "GLfloat");
  glMapGrid1f(XEN_TO_C_GLint(un), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2));
  return(Xen_false);
}

static XEN gxg_glMapGrid2d(XEN un, XEN u1, XEN u2, XEN vn, XEN v1, XEN v2)
{
  #define H_glMapGrid2d "void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, \
GLdouble v2)"
  Xen_check_type(Xen_is_GLint(un), un, 1, "glMapGrid2d", "GLint");
  Xen_check_type(Xen_is_GLdouble(u1), u1, 2, "glMapGrid2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(u2), u2, 3, "glMapGrid2d", "GLdouble");
  Xen_check_type(Xen_is_GLint(vn), vn, 4, "glMapGrid2d", "GLint");
  Xen_check_type(Xen_is_GLdouble(v1), v1, 5, "glMapGrid2d", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(v2), v2, 6, "glMapGrid2d", "GLdouble");
  glMapGrid2d(XEN_TO_C_GLint(un), XEN_TO_C_GLdouble(u1), XEN_TO_C_GLdouble(u2), XEN_TO_C_GLint(vn), XEN_TO_C_GLdouble(v1), 
              XEN_TO_C_GLdouble(v2));
  return(Xen_false);
}

static XEN gxg_glMapGrid2f(XEN un, XEN u1, XEN u2, XEN vn, XEN v1, XEN v2)
{
  #define H_glMapGrid2f "void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)"
  Xen_check_type(Xen_is_GLint(un), un, 1, "glMapGrid2f", "GLint");
  Xen_check_type(Xen_is_GLfloat(u1), u1, 2, "glMapGrid2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(u2), u2, 3, "glMapGrid2f", "GLfloat");
  Xen_check_type(Xen_is_GLint(vn), vn, 4, "glMapGrid2f", "GLint");
  Xen_check_type(Xen_is_GLfloat(v1), v1, 5, "glMapGrid2f", "GLfloat");
  Xen_check_type(Xen_is_GLfloat(v2), v2, 6, "glMapGrid2f", "GLfloat");
  glMapGrid2f(XEN_TO_C_GLint(un), XEN_TO_C_GLfloat(u1), XEN_TO_C_GLfloat(u2), XEN_TO_C_GLint(vn), XEN_TO_C_GLfloat(v1), XEN_TO_C_GLfloat(v2));
  return(Xen_false);
}

static XEN gxg_glEvalPoint1(XEN i)
{
  #define H_glEvalPoint1 "void glEvalPoint1(GLint i)"
  Xen_check_type(Xen_is_GLint(i), i, 1, "glEvalPoint1", "GLint");
  glEvalPoint1(XEN_TO_C_GLint(i));
  return(Xen_false);
}

static XEN gxg_glEvalPoint2(XEN i, XEN j)
{
  #define H_glEvalPoint2 "void glEvalPoint2(GLint i, GLint j)"
  Xen_check_type(Xen_is_GLint(i), i, 1, "glEvalPoint2", "GLint");
  Xen_check_type(Xen_is_GLint(j), j, 2, "glEvalPoint2", "GLint");
  glEvalPoint2(XEN_TO_C_GLint(i), XEN_TO_C_GLint(j));
  return(Xen_false);
}

static XEN gxg_glEvalMesh1(XEN mode, XEN i1, XEN i2)
{
  #define H_glEvalMesh1 "void glEvalMesh1(GLenum mode, GLint i1, GLint i2)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glEvalMesh1", "GLenum");
  Xen_check_type(Xen_is_GLint(i1), i1, 2, "glEvalMesh1", "GLint");
  Xen_check_type(Xen_is_GLint(i2), i2, 3, "glEvalMesh1", "GLint");
  glEvalMesh1(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(i1), XEN_TO_C_GLint(i2));
  return(Xen_false);
}

static XEN gxg_glEvalMesh2(XEN mode, XEN i1, XEN i2, XEN j1, XEN j2)
{
  #define H_glEvalMesh2 "void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glEvalMesh2", "GLenum");
  Xen_check_type(Xen_is_GLint(i1), i1, 2, "glEvalMesh2", "GLint");
  Xen_check_type(Xen_is_GLint(i2), i2, 3, "glEvalMesh2", "GLint");
  Xen_check_type(Xen_is_GLint(j1), j1, 4, "glEvalMesh2", "GLint");
  Xen_check_type(Xen_is_GLint(j2), j2, 5, "glEvalMesh2", "GLint");
  glEvalMesh2(XEN_TO_C_GLenum(mode), XEN_TO_C_GLint(i1), XEN_TO_C_GLint(i2), XEN_TO_C_GLint(j1), XEN_TO_C_GLint(j2));
  return(Xen_false);
}

static XEN gxg_glFogf(XEN pname, XEN param)
{
  #define H_glFogf "void glFogf(GLenum pname, GLfloat param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glFogf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(param), param, 2, "glFogf", "GLfloat");
  glFogf(XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(param));
  return(Xen_false);
}

static XEN gxg_glFogi(XEN pname, XEN param)
{
  #define H_glFogi "void glFogi(GLenum pname, GLint param)"
  Xen_check_type(Xen_is_GLenum(pname), pname, 1, "glFogi", "GLenum");
  Xen_check_type(Xen_is_GLint(param), param, 2, "glFogi", "GLint");
  glFogi(XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(param));
  return(Xen_false);
}

static XEN gxg_glFeedbackBuffer(XEN size, XEN type, XEN buffer)
{
  #define H_glFeedbackBuffer "void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat* buffer)"
  Xen_check_type(Xen_is_GLsizei(size), size, 1, "glFeedbackBuffer", "GLsizei");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "glFeedbackBuffer", "GLenum");
  Xen_check_type(Xen_is_GLfloat_(buffer), buffer, 3, "glFeedbackBuffer", "GLfloat*");
  glFeedbackBuffer(XEN_TO_C_GLsizei(size), XEN_TO_C_GLenum(type), XEN_TO_C_GLfloat_(buffer));
  return(Xen_false);
}

static XEN gxg_glPassThrough(XEN token)
{
  #define H_glPassThrough "void glPassThrough(GLfloat token)"
  Xen_check_type(Xen_is_GLfloat(token), token, 1, "glPassThrough", "GLfloat");
  glPassThrough(XEN_TO_C_GLfloat(token));
  return(Xen_false);
}

static XEN gxg_glSelectBuffer(XEN size, XEN buffer)
{
  #define H_glSelectBuffer "void glSelectBuffer(GLsizei size, GLuint* buffer)"
  Xen_check_type(Xen_is_GLsizei(size), size, 1, "glSelectBuffer", "GLsizei");
  Xen_check_type(Xen_is_GLuint_(buffer), buffer, 2, "glSelectBuffer", "GLuint*");
  glSelectBuffer(XEN_TO_C_GLsizei(size), XEN_TO_C_GLuint_(buffer));
  return(Xen_false);
}

static XEN gxg_glInitNames(void)
{
  #define H_glInitNames "void glInitNames( void)"
  glInitNames();
  return(Xen_false);
}

static XEN gxg_glLoadName(XEN name)
{
  #define H_glLoadName "void glLoadName(GLuint name)"
  Xen_check_type(Xen_is_GLuint(name), name, 1, "glLoadName", "GLuint");
  glLoadName(XEN_TO_C_GLuint(name));
  return(Xen_false);
}

static XEN gxg_glPushName(XEN name)
{
  #define H_glPushName "void glPushName(GLuint name)"
  Xen_check_type(Xen_is_GLuint(name), name, 1, "glPushName", "GLuint");
  glPushName(XEN_TO_C_GLuint(name));
  return(Xen_false);
}

static XEN gxg_glPopName(void)
{
  #define H_glPopName "void glPopName( void)"
  glPopName();
  return(Xen_false);
}

static XEN gxg_glDrawRangeElements(XEN mode, XEN start, XEN end, XEN count, XEN type, XEN indices)
{
  #define H_glDrawRangeElements "void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, \
GLenum type, GLvoid* indices)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glDrawRangeElements", "GLenum");
  Xen_check_type(Xen_is_GLuint(start), start, 2, "glDrawRangeElements", "GLuint");
  Xen_check_type(Xen_is_GLuint(end), end, 3, "glDrawRangeElements", "GLuint");
  Xen_check_type(Xen_is_GLsizei(count), count, 4, "glDrawRangeElements", "GLsizei");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "glDrawRangeElements", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(indices), indices, 6, "glDrawRangeElements", "GLvoid*");
  glDrawRangeElements(XEN_TO_C_GLenum(mode), XEN_TO_C_GLuint(start), XEN_TO_C_GLuint(end), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(type), 
                      XEN_TO_C_GLvoid_(indices));
  return(Xen_false);
}

static XEN gxg_glTexImage3D(XEN arglist)
{
  #define H_glTexImage3D "void glTexImage3D(GLenum target, GLint level, GLint internalFormat, GLsizei width, \
GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, internalFormat, width, height, depth, border, format, type, pixels;
  target = Xen_list_ref(arglist, 0);
  level = Xen_list_ref(arglist, 1);
  internalFormat = Xen_list_ref(arglist, 2);
  width = Xen_list_ref(arglist, 3);
  height = Xen_list_ref(arglist, 4);
  depth = Xen_list_ref(arglist, 5);
  border = Xen_list_ref(arglist, 6);
  format = Xen_list_ref(arglist, 7);
  type = Xen_list_ref(arglist, 8);
  pixels = Xen_list_ref(arglist, 9);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexImage3D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 3, "glTexImage3D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 4, "glTexImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 5, "glTexImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(depth), depth, 6, "glTexImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLint(border), border, 7, "glTexImage3D", "GLint");
  Xen_check_type(Xen_is_GLenum(format), format, 8, "glTexImage3D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 9, "glTexImage3D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 10, "glTexImage3D", "GLvoid*");
  glTexImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
               XEN_TO_C_GLsizei(depth), XEN_TO_C_GLint(border), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glTexSubImage3D(XEN arglist)
{
  #define H_glTexSubImage3D "void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, \
GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLvoid* pixels)"
  XEN target, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels;
  target = Xen_list_ref(arglist, 0);
  level = Xen_list_ref(arglist, 1);
  xoffset = Xen_list_ref(arglist, 2);
  yoffset = Xen_list_ref(arglist, 3);
  zoffset = Xen_list_ref(arglist, 4);
  width = Xen_list_ref(arglist, 5);
  height = Xen_list_ref(arglist, 6);
  depth = Xen_list_ref(arglist, 7);
  format = Xen_list_ref(arglist, 8);
  type = Xen_list_ref(arglist, 9);
  pixels = Xen_list_ref(arglist, 10);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glTexSubImage3D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(yoffset), yoffset, 4, "glTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(zoffset), zoffset, 5, "glTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 6, "glTexSubImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 7, "glTexSubImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(depth), depth, 8, "glTexSubImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 9, "glTexSubImage3D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 10, "glTexSubImage3D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(pixels), pixels, 11, "glTexSubImage3D", "GLvoid*");
  glTexSubImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(zoffset), 
                  XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                  XEN_TO_C_GLvoid_(pixels));
  return(Xen_false);
}

static XEN gxg_glCopyTexSubImage3D(XEN target, XEN level, XEN xoffset, XEN yoffset, XEN zoffset, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyTexSubImage3D "void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, \
GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyTexSubImage3D", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 2, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(xoffset), xoffset, 3, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(yoffset), yoffset, 4, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(zoffset), zoffset, 5, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(x), x, 6, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 7, "glCopyTexSubImage3D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 8, "glCopyTexSubImage3D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 9, "glCopyTexSubImage3D", "GLsizei");
  glCopyTexSubImage3D(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(level), XEN_TO_C_GLint(xoffset), XEN_TO_C_GLint(yoffset), XEN_TO_C_GLint(zoffset), 
                      XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(Xen_false);
}

static XEN gxg_glColorTable(XEN target, XEN internalformat, XEN width, XEN format, XEN type, XEN table)
{
  #define H_glColorTable "void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, \
GLenum type, GLvoid* table)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glColorTable", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glColorTable", "GLenum");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glColorTable", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 4, "glColorTable", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "glColorTable", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(table), table, 6, "glColorTable", "GLvoid*");
  glColorTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
               XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(table));
  return(Xen_false);
}

static XEN gxg_glColorSubTable(XEN target, XEN start, XEN count, XEN format, XEN type, XEN data)
{
  #define H_glColorSubTable "void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, \
GLenum type, GLvoid* data)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glColorSubTable", "GLenum");
  Xen_check_type(Xen_is_GLsizei(start), start, 2, "glColorSubTable", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(count), count, 3, "glColorSubTable", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 4, "glColorSubTable", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "glColorSubTable", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(data), data, 6, "glColorSubTable", "GLvoid*");
  glColorSubTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(start), XEN_TO_C_GLsizei(count), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                  XEN_TO_C_GLvoid_(data));
  return(Xen_false);
}

static XEN gxg_glCopyColorSubTable(XEN target, XEN start, XEN x, XEN y, XEN width)
{
  #define H_glCopyColorSubTable "void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, \
GLsizei width)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyColorSubTable", "GLenum");
  Xen_check_type(Xen_is_GLsizei(start), start, 2, "glCopyColorSubTable", "GLsizei");
  Xen_check_type(Xen_is_GLint(x), x, 3, "glCopyColorSubTable", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 4, "glCopyColorSubTable", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 5, "glCopyColorSubTable", "GLsizei");
  glCopyColorSubTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(start), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width));
  return(Xen_false);
}

static XEN gxg_glCopyColorTable(XEN target, XEN internalformat, XEN x, XEN y, XEN width)
{
  #define H_glCopyColorTable "void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, \
GLsizei width)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyColorTable", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glCopyColorTable", "GLenum");
  Xen_check_type(Xen_is_GLint(x), x, 3, "glCopyColorTable", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 4, "glCopyColorTable", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 5, "glCopyColorTable", "GLsizei");
  glCopyColorTable(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), XEN_TO_C_GLsizei(width));
  return(Xen_false);
}

static XEN gxg_glGetColorTableParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameterfv "void glGetColorTableParameterfv(GLenum target, GLenum pname, \
GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetColorTableParameterfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetColorTableParameterfv", "GLenum");
  glGetColorTableParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetColorTableParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetColorTableParameteriv "void glGetColorTableParameteriv(GLenum target, GLenum pname, \
GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetColorTableParameteriv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetColorTableParameteriv", "GLenum");
  glGetColorTableParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glBlendEquation(XEN mode)
{
  #define H_glBlendEquation "void glBlendEquation(GLenum mode)"
  Xen_check_type(Xen_is_GLenum(mode), mode, 1, "glBlendEquation", "GLenum");
  glBlendEquation(XEN_TO_C_GLenum(mode));
  return(Xen_false);
}

static XEN gxg_glBlendColor(XEN red, XEN green, XEN blue, XEN alpha)
{
  #define H_glBlendColor "void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"
  Xen_check_type(Xen_is_GLclampf(red), red, 1, "glBlendColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(green), green, 2, "glBlendColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(blue), blue, 3, "glBlendColor", "GLclampf");
  Xen_check_type(Xen_is_GLclampf(alpha), alpha, 4, "glBlendColor", "GLclampf");
  glBlendColor(XEN_TO_C_GLclampf(red), XEN_TO_C_GLclampf(green), XEN_TO_C_GLclampf(blue), XEN_TO_C_GLclampf(alpha));
  return(Xen_false);
}

static XEN gxg_glHistogram(XEN target, XEN width, XEN internalformat, XEN sink)
{
  #define H_glHistogram "void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glHistogram", "GLenum");
  Xen_check_type(Xen_is_GLsizei(width), width, 2, "glHistogram", "GLsizei");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 3, "glHistogram", "GLenum");
  Xen_check_type(Xen_is_GLboolean(sink), sink, 4, "glHistogram", "GLboolean");
  glHistogram(XEN_TO_C_GLenum(target), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLboolean(sink));
  return(Xen_false);
}

static XEN gxg_glResetHistogram(XEN target)
{
  #define H_glResetHistogram "void glResetHistogram(GLenum target)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glResetHistogram", "GLenum");
  glResetHistogram(XEN_TO_C_GLenum(target));
  return(Xen_false);
}

static XEN gxg_glGetHistogram(XEN target, XEN reset, XEN format, XEN type, XEN values)
{
  #define H_glGetHistogram "void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, \
GLvoid* values)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetHistogram", "GLenum");
  Xen_check_type(Xen_is_GLboolean(reset), reset, 2, "glGetHistogram", "GLboolean");
  Xen_check_type(Xen_is_GLenum(format), format, 3, "glGetHistogram", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 4, "glGetHistogram", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(values), values, 5, "glGetHistogram", "GLvoid*");
  glGetHistogram(XEN_TO_C_GLenum(target), XEN_TO_C_GLboolean(reset), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(values));
  return(Xen_false);
}

static XEN gxg_glGetHistogramParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetHistogramParameterfv "void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetHistogramParameterfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetHistogramParameterfv", "GLenum");
  glGetHistogramParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetHistogramParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetHistogramParameteriv "void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetHistogramParameteriv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetHistogramParameteriv", "GLenum");
  glGetHistogramParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glMinmax(XEN target, XEN internalformat, XEN sink)
{
  #define H_glMinmax "void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glMinmax", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glMinmax", "GLenum");
  Xen_check_type(Xen_is_GLboolean(sink), sink, 3, "glMinmax", "GLboolean");
  glMinmax(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLboolean(sink));
  return(Xen_false);
}

static XEN gxg_glResetMinmax(XEN target)
{
  #define H_glResetMinmax "void glResetMinmax(GLenum target)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glResetMinmax", "GLenum");
  glResetMinmax(XEN_TO_C_GLenum(target));
  return(Xen_false);
}

static XEN gxg_glGetMinmax(XEN target, XEN reset, XEN format, XEN types, XEN values)
{
  #define H_glGetMinmax "void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum types, \
GLvoid* values)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMinmax", "GLenum");
  Xen_check_type(Xen_is_GLboolean(reset), reset, 2, "glGetMinmax", "GLboolean");
  Xen_check_type(Xen_is_GLenum(format), format, 3, "glGetMinmax", "GLenum");
  Xen_check_type(Xen_is_GLenum(types), types, 4, "glGetMinmax", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(values), values, 5, "glGetMinmax", "GLvoid*");
  glGetMinmax(XEN_TO_C_GLenum(target), XEN_TO_C_GLboolean(reset), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(types), XEN_TO_C_GLvoid_(values));
  return(Xen_false);
}

static XEN gxg_glGetMinmaxParameterfv(XEN target, XEN pname, XEN params)
{
  #define H_glGetMinmaxParameterfv "void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat* [params])"
  GLfloat ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMinmaxParameterfv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetMinmaxParameterfv", "GLenum");
  glGetMinmaxParameterfv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLfloat(ref_params[0])));
}

static XEN gxg_glGetMinmaxParameteriv(XEN target, XEN pname, XEN params)
{
  #define H_glGetMinmaxParameteriv "void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint* [params])"
  GLint ref_params[1];
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glGetMinmaxParameteriv", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glGetMinmaxParameteriv", "GLenum");
  glGetMinmaxParameteriv(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), ref_params);
  return(Xen_list_1(C_TO_XEN_GLint(ref_params[0])));
}

static XEN gxg_glConvolutionFilter1D(XEN target, XEN internalformat, XEN width, XEN format, XEN type, XEN image)
{
  #define H_glConvolutionFilter1D "void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, \
GLenum format, GLenum type, GLvoid* image)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glConvolutionFilter1D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 4, "glConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "glConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(image), image, 6, "glConvolutionFilter1D", "GLvoid*");
  glConvolutionFilter1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLenum(format), 
                        XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(image));
  return(Xen_false);
}

static XEN gxg_glConvolutionFilter2D(XEN target, XEN internalformat, XEN width, XEN height, XEN format, XEN type, XEN image)
{
  #define H_glConvolutionFilter2D "void glConvolutionFilter2D(GLenum target, GLenum internalformat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, GLvoid* image)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glConvolutionFilter2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glConvolutionFilter2D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "glConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "glConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(image), image, 7, "glConvolutionFilter2D", "GLvoid*");
  glConvolutionFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
                        XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(image));
  return(Xen_false);
}

static XEN gxg_glConvolutionParameterf(XEN target, XEN pname, XEN params)
{
  #define H_glConvolutionParameterf "void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glConvolutionParameterf", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glConvolutionParameterf", "GLenum");
  Xen_check_type(Xen_is_GLfloat(params), params, 3, "glConvolutionParameterf", "GLfloat");
  glConvolutionParameterf(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLfloat(params));
  return(Xen_false);
}

static XEN gxg_glConvolutionParameteri(XEN target, XEN pname, XEN params)
{
  #define H_glConvolutionParameteri "void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glConvolutionParameteri", "GLenum");
  Xen_check_type(Xen_is_GLenum(pname), pname, 2, "glConvolutionParameteri", "GLenum");
  Xen_check_type(Xen_is_GLint(params), params, 3, "glConvolutionParameteri", "GLint");
  glConvolutionParameteri(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(pname), XEN_TO_C_GLint(params));
  return(Xen_false);
}

static XEN gxg_glCopyConvolutionFilter1D(XEN target, XEN internalformat, XEN x, XEN y, XEN width)
{
  #define H_glCopyConvolutionFilter1D "void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, \
GLint x, GLint y, GLsizei width)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glCopyConvolutionFilter1D", "GLenum");
  Xen_check_type(Xen_is_GLint(x), x, 3, "glCopyConvolutionFilter1D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 4, "glCopyConvolutionFilter1D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 5, "glCopyConvolutionFilter1D", "GLsizei");
  glCopyConvolutionFilter1D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                            XEN_TO_C_GLsizei(width));
  return(Xen_false);
}

static XEN gxg_glCopyConvolutionFilter2D(XEN target, XEN internalformat, XEN x, XEN y, XEN width, XEN height)
{
  #define H_glCopyConvolutionFilter2D "void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, \
GLint x, GLint y, GLsizei width, GLsizei height)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glCopyConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glCopyConvolutionFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLint(x), x, 3, "glCopyConvolutionFilter2D", "GLint");
  Xen_check_type(Xen_is_GLint(y), y, 4, "glCopyConvolutionFilter2D", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 5, "glCopyConvolutionFilter2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 6, "glCopyConvolutionFilter2D", "GLsizei");
  glCopyConvolutionFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLint(x), XEN_TO_C_GLint(y), 
                            XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height));
  return(Xen_false);
}

static XEN gxg_glSeparableFilter2D(XEN target, XEN internalformat, XEN width, XEN height, XEN format, XEN type, XEN row, XEN column)
{
  #define H_glSeparableFilter2D "void glSeparableFilter2D(GLenum target, GLenum internalformat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, GLvoid* row, GLvoid* column)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "glSeparableFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(internalformat), internalformat, 2, "glSeparableFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "glSeparableFilter2D", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "glSeparableFilter2D", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "glSeparableFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "glSeparableFilter2D", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(row), row, 7, "glSeparableFilter2D", "GLvoid*");
  Xen_check_type(Xen_is_GLvoid_(column), column, 8, "glSeparableFilter2D", "GLvoid*");
  glSeparableFilter2D(XEN_TO_C_GLenum(target), XEN_TO_C_GLenum(internalformat), XEN_TO_C_GLsizei(width), XEN_TO_C_GLsizei(height), 
                      XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLvoid_(row), XEN_TO_C_GLvoid_(column));
  return(Xen_false);
}

#if HAVE_GLU
static XEN gxg_gluBeginCurve(XEN nurb)
{
  #define H_gluBeginCurve "void gluBeginCurve(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluBeginCurve", "GLUnurbs*");
  gluBeginCurve(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluBeginPolygon(XEN tess)
{
  #define H_gluBeginPolygon "void gluBeginPolygon(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluBeginPolygon", "GLUtesselator*");
  gluBeginPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

static XEN gxg_gluBeginSurface(XEN nurb)
{
  #define H_gluBeginSurface "void gluBeginSurface(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluBeginSurface", "GLUnurbs*");
  gluBeginSurface(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

static XEN gxg_gluBeginTrim(XEN nurb)
{
  #define H_gluBeginTrim "void gluBeginTrim(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluBeginTrim", "GLUnurbs*");
  gluBeginTrim(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

static XEN gxg_gluBuild1DMipmapLevels(XEN target, XEN internalFormat, XEN width, XEN format, XEN type, XEN level, XEN base, XEN max, XEN data)
{
  #define H_gluBuild1DMipmapLevels "GLint gluBuild1DMipmapLevels(GLenum target, GLint internalFormat, \
GLsizei width, GLenum format, GLenum type, GLint level, GLint base, GLint max, void* data)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild1DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild1DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild1DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 4, "gluBuild1DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "gluBuild1DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 6, "gluBuild1DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(base), base, 7, "gluBuild1DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(max), max, 8, "gluBuild1DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_void_(data), data, 9, "gluBuild1DMipmapLevels", "void*");
  return(C_TO_XEN_GLint(gluBuild1DMipmapLevels(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                               XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_GLint(level), XEN_TO_C_GLint(base), 
                                               XEN_TO_C_GLint(max), XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild1DMipmaps(XEN target, XEN internalFormat, XEN width, XEN format, XEN type, XEN data)
{
  #define H_gluBuild1DMipmaps "GLint gluBuild1DMipmaps(GLenum target, GLint internalFormat, GLsizei width, \
GLenum format, GLenum type, void* data)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild1DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild1DMipmaps", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild1DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 4, "gluBuild1DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "gluBuild1DMipmaps", "GLenum");
  Xen_check_type(Xen_is_void_(data), data, 6, "gluBuild1DMipmaps", "void*");
  return(C_TO_XEN_GLint(gluBuild1DMipmaps(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                          XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild2DMipmapLevels(XEN arglist)
{
  #define H_gluBuild2DMipmapLevels "GLint gluBuild2DMipmapLevels(GLenum target, GLint internalFormat, \
GLsizei width, GLsizei height, GLenum format, GLenum type, GLint level, GLint base, GLint max, void* data)"
  XEN target, internalFormat, width, height, format, type, level, base, max, data;
  target = Xen_list_ref(arglist, 0);
  internalFormat = Xen_list_ref(arglist, 1);
  width = Xen_list_ref(arglist, 2);
  height = Xen_list_ref(arglist, 3);
  format = Xen_list_ref(arglist, 4);
  type = Xen_list_ref(arglist, 5);
  level = Xen_list_ref(arglist, 6);
  base = Xen_list_ref(arglist, 7);
  max = Xen_list_ref(arglist, 8);
  data = Xen_list_ref(arglist, 9);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild2DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild2DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild2DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "gluBuild2DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "gluBuild2DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "gluBuild2DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 7, "gluBuild2DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(base), base, 8, "gluBuild2DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(max), max, 9, "gluBuild2DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_void_(data), data, 10, "gluBuild2DMipmapLevels", "void*");
  return(C_TO_XEN_GLint(gluBuild2DMipmapLevels(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                               XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                                               XEN_TO_C_GLint(level), XEN_TO_C_GLint(base), XEN_TO_C_GLint(max), XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild2DMipmaps(XEN target, XEN internalFormat, XEN width, XEN height, XEN format, XEN type, XEN data)
{
  #define H_gluBuild2DMipmaps "GLint gluBuild2DMipmaps(GLenum target, GLint internalFormat, GLsizei width, \
GLsizei height, GLenum format, GLenum type, void* data)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild2DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild2DMipmaps", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild2DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "gluBuild2DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 5, "gluBuild2DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 6, "gluBuild2DMipmaps", "GLenum");
  Xen_check_type(Xen_is_void_(data), data, 7, "gluBuild2DMipmaps", "void*");
  return(C_TO_XEN_GLint(gluBuild2DMipmaps(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                          XEN_TO_C_GLsizei(height), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild3DMipmapLevels(XEN arglist)
{
  #define H_gluBuild3DMipmapLevels "GLint gluBuild3DMipmapLevels(GLenum target, GLint internalFormat, \
GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLint level, GLint base, GLint max, \
void* data)"
  XEN target, internalFormat, width, height, depth, format, type, level, base, max, data;
  target = Xen_list_ref(arglist, 0);
  internalFormat = Xen_list_ref(arglist, 1);
  width = Xen_list_ref(arglist, 2);
  height = Xen_list_ref(arglist, 3);
  depth = Xen_list_ref(arglist, 4);
  format = Xen_list_ref(arglist, 5);
  type = Xen_list_ref(arglist, 6);
  level = Xen_list_ref(arglist, 7);
  base = Xen_list_ref(arglist, 8);
  max = Xen_list_ref(arglist, 9);
  data = Xen_list_ref(arglist, 10);
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild3DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild3DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild3DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "gluBuild3DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(depth), depth, 5, "gluBuild3DMipmapLevels", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 6, "gluBuild3DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 7, "gluBuild3DMipmapLevels", "GLenum");
  Xen_check_type(Xen_is_GLint(level), level, 8, "gluBuild3DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(base), base, 9, "gluBuild3DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_GLint(max), max, 10, "gluBuild3DMipmapLevels", "GLint");
  Xen_check_type(Xen_is_void_(data), data, 11, "gluBuild3DMipmapLevels", "void*");
  return(C_TO_XEN_GLint(gluBuild3DMipmapLevels(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                               XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLenum(format), 
                                               XEN_TO_C_GLenum(type), XEN_TO_C_GLint(level), XEN_TO_C_GLint(base), XEN_TO_C_GLint(max), 
                                               XEN_TO_C_void_(data))));
}

static XEN gxg_gluBuild3DMipmaps(XEN target, XEN internalFormat, XEN width, XEN height, XEN depth, XEN format, XEN type, XEN data)
{
  #define H_gluBuild3DMipmaps "GLint gluBuild3DMipmaps(GLenum target, GLint internalFormat, GLsizei width, \
GLsizei height, GLsizei depth, GLenum format, GLenum type, void* data)"
  Xen_check_type(Xen_is_GLenum(target), target, 1, "gluBuild3DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLint(internalFormat), internalFormat, 2, "gluBuild3DMipmaps", "GLint");
  Xen_check_type(Xen_is_GLsizei(width), width, 3, "gluBuild3DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(height), height, 4, "gluBuild3DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(depth), depth, 5, "gluBuild3DMipmaps", "GLsizei");
  Xen_check_type(Xen_is_GLenum(format), format, 6, "gluBuild3DMipmaps", "GLenum");
  Xen_check_type(Xen_is_GLenum(type), type, 7, "gluBuild3DMipmaps", "GLenum");
  Xen_check_type(Xen_is_void_(data), data, 8, "gluBuild3DMipmaps", "void*");
  return(C_TO_XEN_GLint(gluBuild3DMipmaps(XEN_TO_C_GLenum(target), XEN_TO_C_GLint(internalFormat), XEN_TO_C_GLsizei(width), 
                                          XEN_TO_C_GLsizei(height), XEN_TO_C_GLsizei(depth), XEN_TO_C_GLenum(format), XEN_TO_C_GLenum(type), 
                                          XEN_TO_C_void_(data))));
}

static XEN gxg_gluCheckExtension(XEN extName, XEN extString)
{
  #define H_gluCheckExtension "GLboolean gluCheckExtension(GLubyte* extName, GLubyte* extString)"
  Xen_check_type(Xen_is_GLubyte_(extName), extName, 1, "gluCheckExtension", "GLubyte*");
  Xen_check_type(Xen_is_GLubyte_(extString), extString, 2, "gluCheckExtension", "GLubyte*");
  return(C_TO_XEN_GLboolean(gluCheckExtension(XEN_TO_C_GLubyte_(extName), XEN_TO_C_GLubyte_(extString))));
}

static XEN gxg_gluCylinder(XEN quad, XEN base, XEN top, XEN height, XEN slices, XEN stacks)
{
  #define H_gluCylinder "void gluCylinder(GLUquadric* quad, GLdouble base, GLdouble top, GLdouble height, \
GLint slices, GLint stacks)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluCylinder", "GLUquadric*");
  Xen_check_type(Xen_is_GLdouble(base), base, 2, "gluCylinder", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(top), top, 3, "gluCylinder", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(height), height, 4, "gluCylinder", "GLdouble");
  Xen_check_type(Xen_is_GLint(slices), slices, 5, "gluCylinder", "GLint");
  Xen_check_type(Xen_is_GLint(stacks), stacks, 6, "gluCylinder", "GLint");
  gluCylinder(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLdouble(base), XEN_TO_C_GLdouble(top), XEN_TO_C_GLdouble(height), XEN_TO_C_GLint(slices), 
              XEN_TO_C_GLint(stacks));
  return(Xen_false);
}

static XEN gxg_gluDeleteNurbsRenderer(XEN nurb)
{
  #define H_gluDeleteNurbsRenderer "void gluDeleteNurbsRenderer(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluDeleteNurbsRenderer", "GLUnurbs*");
  gluDeleteNurbsRenderer(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

static XEN gxg_gluDeleteQuadric(XEN quad)
{
  #define H_gluDeleteQuadric "void gluDeleteQuadric(GLUquadric* quad)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluDeleteQuadric", "GLUquadric*");
  gluDeleteQuadric(XEN_TO_C_GLUquadric_(quad));
  return(Xen_false);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluDeleteTess(XEN tess)
{
  #define H_gluDeleteTess "void gluDeleteTess(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluDeleteTess", "GLUtesselator*");
  gluDeleteTess(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

static XEN gxg_gluDisk(XEN quad, XEN inner, XEN outer, XEN slices, XEN loops)
{
  #define H_gluDisk "void gluDisk(GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluDisk", "GLUquadric*");
  Xen_check_type(Xen_is_GLdouble(inner), inner, 2, "gluDisk", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(outer), outer, 3, "gluDisk", "GLdouble");
  Xen_check_type(Xen_is_GLint(slices), slices, 4, "gluDisk", "GLint");
  Xen_check_type(Xen_is_GLint(loops), loops, 5, "gluDisk", "GLint");
  gluDisk(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLdouble(inner), XEN_TO_C_GLdouble(outer), XEN_TO_C_GLint(slices), XEN_TO_C_GLint(loops));
  return(Xen_false);
}

static XEN gxg_gluEndCurve(XEN nurb)
{
  #define H_gluEndCurve "void gluEndCurve(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluEndCurve", "GLUnurbs*");
  gluEndCurve(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluEndPolygon(XEN tess)
{
  #define H_gluEndPolygon "void gluEndPolygon(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluEndPolygon", "GLUtesselator*");
  gluEndPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

static XEN gxg_gluEndSurface(XEN nurb)
{
  #define H_gluEndSurface "void gluEndSurface(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluEndSurface", "GLUnurbs*");
  gluEndSurface(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

static XEN gxg_gluEndTrim(XEN nurb)
{
  #define H_gluEndTrim "void gluEndTrim(GLUnurbs* nurb)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluEndTrim", "GLUnurbs*");
  gluEndTrim(XEN_TO_C_GLUnurbs_(nurb));
  return(Xen_false);
}

static XEN gxg_gluErrorString(XEN error)
{
  #define H_gluErrorString "constchar* gluErrorString(GLenum error)"
  Xen_check_type(Xen_is_GLenum(error), error, 1, "gluErrorString", "GLenum");
  return(C_TO_XEN_constchar_(gluErrorString(XEN_TO_C_GLenum(error))));
}

static XEN gxg_gluGetNurbsProperty(XEN nurb, XEN property, XEN data)
{
  #define H_gluGetNurbsProperty "void gluGetNurbsProperty(GLUnurbs* nurb, GLenum property, GLfloat* data)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluGetNurbsProperty", "GLUnurbs*");
  Xen_check_type(Xen_is_GLenum(property), property, 2, "gluGetNurbsProperty", "GLenum");
  Xen_check_type(Xen_is_GLfloat_(data), data, 3, "gluGetNurbsProperty", "GLfloat*");
  gluGetNurbsProperty(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLenum(property), XEN_TO_C_GLfloat_(data));
  return(Xen_false);
}

static XEN gxg_gluGetString(XEN name)
{
  #define H_gluGetString "constchar* gluGetString(GLenum name)"
  Xen_check_type(Xen_is_GLenum(name), name, 1, "gluGetString", "GLenum");
  return(C_TO_XEN_constchar_(gluGetString(XEN_TO_C_GLenum(name))));
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluGetTessProperty(XEN tess, XEN which, XEN data)
{
  #define H_gluGetTessProperty "void gluGetTessProperty(GLUtesselator* tess, GLenum which, GLdouble* data)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluGetTessProperty", "GLUtesselator*");
  Xen_check_type(Xen_is_GLenum(which), which, 2, "gluGetTessProperty", "GLenum");
  Xen_check_type(Xen_is_GLdouble_(data), data, 3, "gluGetTessProperty", "GLdouble*");
  gluGetTessProperty(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(which), XEN_TO_C_GLdouble_(data));
  return(Xen_false);
}
#endif

static XEN gxg_gluLoadSamplingMatrices(XEN nurb, XEN model, XEN perspective, XEN view)
{
  #define H_gluLoadSamplingMatrices "void gluLoadSamplingMatrices(GLUnurbs* nurb, GLfloat* model, GLfloat* perspective, \
GLint* view)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluLoadSamplingMatrices", "GLUnurbs*");
  Xen_check_type(Xen_is_GLfloat_(model), model, 2, "gluLoadSamplingMatrices", "GLfloat*");
  Xen_check_type(Xen_is_GLfloat_(perspective), perspective, 3, "gluLoadSamplingMatrices", "GLfloat*");
  Xen_check_type(Xen_is_GLint_(view), view, 4, "gluLoadSamplingMatrices", "GLint*");
  gluLoadSamplingMatrices(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLfloat_(model), XEN_TO_C_GLfloat_(perspective), XEN_TO_C_GLint_(view));
  return(Xen_false);
}

static XEN gxg_gluLookAt(XEN eyeX, XEN eyeY, XEN eyeZ, XEN centerX, XEN centerY, XEN centerZ, XEN upX, XEN upY, XEN upZ)
{
  #define H_gluLookAt "void gluLookAt(GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ, GLdouble centerX, \
GLdouble centerY, GLdouble centerZ, GLdouble upX, GLdouble upY, GLdouble upZ)"
  Xen_check_type(Xen_is_GLdouble(eyeX), eyeX, 1, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(eyeY), eyeY, 2, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(eyeZ), eyeZ, 3, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(centerX), centerX, 4, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(centerY), centerY, 5, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(centerZ), centerZ, 6, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(upX), upX, 7, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(upY), upY, 8, "gluLookAt", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(upZ), upZ, 9, "gluLookAt", "GLdouble");
  gluLookAt(XEN_TO_C_GLdouble(eyeX), XEN_TO_C_GLdouble(eyeY), XEN_TO_C_GLdouble(eyeZ), XEN_TO_C_GLdouble(centerX), XEN_TO_C_GLdouble(centerY), 
            XEN_TO_C_GLdouble(centerZ), XEN_TO_C_GLdouble(upX), XEN_TO_C_GLdouble(upY), XEN_TO_C_GLdouble(upZ));
  return(Xen_false);
}

static XEN gxg_gluNewNurbsRenderer(void)
{
  #define H_gluNewNurbsRenderer "GLUnurbs* gluNewNurbsRenderer( void)"
  return(C_TO_XEN_GLUnurbs_(gluNewNurbsRenderer()));
}

static XEN gxg_gluNewQuadric(void)
{
  #define H_gluNewQuadric "GLUquadric* gluNewQuadric( void)"
  return(C_TO_XEN_GLUquadric_(gluNewQuadric()));
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
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluNextContour", "GLUtesselator*");
  Xen_check_type(Xen_is_GLenum(type), type, 2, "gluNextContour", "GLenum");
  gluNextContour(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(type));
  return(Xen_false);
}
#endif

static XEN gxg_gluNurbsCallback(XEN nurb, XEN which, XEN CallBackFunc)
{
  #define H_gluNurbsCallback "void gluNurbsCallback(GLUnurbs* nurb, GLenum which, _GLUfuncptr CallBackFunc)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsCallback", "GLUnurbs*");
  Xen_check_type(Xen_is_GLenum(which), which, 2, "gluNurbsCallback", "GLenum");
  Xen_check_type(Xen_is__GLUfuncptr(CallBackFunc), CallBackFunc, 3, "gluNurbsCallback", "_GLUfuncptr");
  gluNurbsCallback(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLenum(which), XEN_TO_C__GLUfuncptr(CallBackFunc));
  return(Xen_false);
}

static XEN gxg_gluNurbsCallbackData(XEN nurb, XEN userData)
{
  #define H_gluNurbsCallbackData "void gluNurbsCallbackData(GLUnurbs* nurb, GLvoid* userData)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsCallbackData", "GLUnurbs*");
  Xen_check_type(Xen_is_GLvoid_(userData), userData, 2, "gluNurbsCallbackData", "GLvoid*");
  gluNurbsCallbackData(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLvoid_(userData));
  return(Xen_false);
}

static XEN gxg_gluNurbsCallbackDataEXT(XEN nurb, XEN userData)
{
  #define H_gluNurbsCallbackDataEXT "void gluNurbsCallbackDataEXT(GLUnurbs* nurb, GLvoid* userData)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsCallbackDataEXT", "GLUnurbs*");
  Xen_check_type(Xen_is_GLvoid_(userData), userData, 2, "gluNurbsCallbackDataEXT", "GLvoid*");
  gluNurbsCallbackDataEXT(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLvoid_(userData));
  return(Xen_false);
}

static XEN gxg_gluNurbsCurve(XEN nurb, XEN knotCount, XEN knots, XEN stride, XEN control, XEN order, XEN type)
{
  #define H_gluNurbsCurve "void gluNurbsCurve(GLUnurbs* nurb, GLint knotCount, GLfloat* knots, GLint stride, \
GLfloat* control, GLint order, GLenum type)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsCurve", "GLUnurbs*");
  Xen_check_type(Xen_is_GLint(knotCount), knotCount, 2, "gluNurbsCurve", "GLint");
  Xen_check_type(Xen_is_GLfloat_(knots), knots, 3, "gluNurbsCurve", "GLfloat*");
  Xen_check_type(Xen_is_GLint(stride), stride, 4, "gluNurbsCurve", "GLint");
  Xen_check_type(Xen_is_GLfloat_(control), control, 5, "gluNurbsCurve", "GLfloat*");
  Xen_check_type(Xen_is_GLint(order), order, 6, "gluNurbsCurve", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 7, "gluNurbsCurve", "GLenum");
  gluNurbsCurve(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLint(knotCount), XEN_TO_C_GLfloat_(knots), XEN_TO_C_GLint(stride), XEN_TO_C_GLfloat_(control), 
                XEN_TO_C_GLint(order), XEN_TO_C_GLenum(type));
  return(Xen_false);
}

static XEN gxg_gluNurbsProperty(XEN nurb, XEN property, XEN value)
{
  #define H_gluNurbsProperty "void gluNurbsProperty(GLUnurbs* nurb, GLenum property, GLfloat value)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsProperty", "GLUnurbs*");
  Xen_check_type(Xen_is_GLenum(property), property, 2, "gluNurbsProperty", "GLenum");
  Xen_check_type(Xen_is_GLfloat(value), value, 3, "gluNurbsProperty", "GLfloat");
  gluNurbsProperty(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLenum(property), XEN_TO_C_GLfloat(value));
  return(Xen_false);
}

static XEN gxg_gluNurbsSurface(XEN arglist)
{
  #define H_gluNurbsSurface "void gluNurbsSurface(GLUnurbs* nurb, GLint sKnotCount, GLfloat* sKnots, \
GLint tKnotCount, GLfloat* tKnots, GLint sStride, GLint tStride, GLfloat* control, GLint sOrder, GLint tOrder, \
GLenum type)"
  XEN nurb, sKnotCount, sKnots, tKnotCount, tKnots, sStride, tStride, control, sOrder, tOrder, type;
  nurb = Xen_list_ref(arglist, 0);
  sKnotCount = Xen_list_ref(arglist, 1);
  sKnots = Xen_list_ref(arglist, 2);
  tKnotCount = Xen_list_ref(arglist, 3);
  tKnots = Xen_list_ref(arglist, 4);
  sStride = Xen_list_ref(arglist, 5);
  tStride = Xen_list_ref(arglist, 6);
  control = Xen_list_ref(arglist, 7);
  sOrder = Xen_list_ref(arglist, 8);
  tOrder = Xen_list_ref(arglist, 9);
  type = Xen_list_ref(arglist, 10);
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluNurbsSurface", "GLUnurbs*");
  Xen_check_type(Xen_is_GLint(sKnotCount), sKnotCount, 2, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLfloat_(sKnots), sKnots, 3, "gluNurbsSurface", "GLfloat*");
  Xen_check_type(Xen_is_GLint(tKnotCount), tKnotCount, 4, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLfloat_(tKnots), tKnots, 5, "gluNurbsSurface", "GLfloat*");
  Xen_check_type(Xen_is_GLint(sStride), sStride, 6, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLint(tStride), tStride, 7, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLfloat_(control), control, 8, "gluNurbsSurface", "GLfloat*");
  Xen_check_type(Xen_is_GLint(sOrder), sOrder, 9, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLint(tOrder), tOrder, 10, "gluNurbsSurface", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 11, "gluNurbsSurface", "GLenum");
  gluNurbsSurface(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLint(sKnotCount), XEN_TO_C_GLfloat_(sKnots), XEN_TO_C_GLint(tKnotCount), 
                  XEN_TO_C_GLfloat_(tKnots), XEN_TO_C_GLint(sStride), XEN_TO_C_GLint(tStride), XEN_TO_C_GLfloat_(control), 
                  XEN_TO_C_GLint(sOrder), XEN_TO_C_GLint(tOrder), XEN_TO_C_GLenum(type));
  return(Xen_false);
}

static XEN gxg_gluOrtho2D(XEN left, XEN right, XEN bottom, XEN top)
{
  #define H_gluOrtho2D "void gluOrtho2D(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top)"
  Xen_check_type(Xen_is_GLdouble(left), left, 1, "gluOrtho2D", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(right), right, 2, "gluOrtho2D", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(bottom), bottom, 3, "gluOrtho2D", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(top), top, 4, "gluOrtho2D", "GLdouble");
  gluOrtho2D(XEN_TO_C_GLdouble(left), XEN_TO_C_GLdouble(right), XEN_TO_C_GLdouble(bottom), XEN_TO_C_GLdouble(top));
  return(Xen_false);
}

static XEN gxg_gluPartialDisk(XEN quad, XEN inner, XEN outer, XEN slices, XEN loops, XEN start, XEN sweep)
{
  #define H_gluPartialDisk "void gluPartialDisk(GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, \
GLint loops, GLdouble start, GLdouble sweep)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluPartialDisk", "GLUquadric*");
  Xen_check_type(Xen_is_GLdouble(inner), inner, 2, "gluPartialDisk", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(outer), outer, 3, "gluPartialDisk", "GLdouble");
  Xen_check_type(Xen_is_GLint(slices), slices, 4, "gluPartialDisk", "GLint");
  Xen_check_type(Xen_is_GLint(loops), loops, 5, "gluPartialDisk", "GLint");
  Xen_check_type(Xen_is_GLdouble(start), start, 6, "gluPartialDisk", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(sweep), sweep, 7, "gluPartialDisk", "GLdouble");
  gluPartialDisk(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLdouble(inner), XEN_TO_C_GLdouble(outer), XEN_TO_C_GLint(slices), 
                 XEN_TO_C_GLint(loops), XEN_TO_C_GLdouble(start), XEN_TO_C_GLdouble(sweep));
  return(Xen_false);
}

static XEN gxg_gluPerspective(XEN fovy, XEN aspect, XEN zNear, XEN zFar)
{
  #define H_gluPerspective "void gluPerspective(GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar)"
  Xen_check_type(Xen_is_GLdouble(fovy), fovy, 1, "gluPerspective", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(aspect), aspect, 2, "gluPerspective", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(zNear), zNear, 3, "gluPerspective", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(zFar), zFar, 4, "gluPerspective", "GLdouble");
  gluPerspective(XEN_TO_C_GLdouble(fovy), XEN_TO_C_GLdouble(aspect), XEN_TO_C_GLdouble(zNear), XEN_TO_C_GLdouble(zFar));
  return(Xen_false);
}

static XEN gxg_gluPickMatrix(XEN x, XEN y, XEN delX, XEN delY, XEN viewport)
{
  #define H_gluPickMatrix "void gluPickMatrix(GLdouble x, GLdouble y, GLdouble delX, GLdouble delY, GLint* viewport)"
  Xen_check_type(Xen_is_GLdouble(x), x, 1, "gluPickMatrix", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(y), y, 2, "gluPickMatrix", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(delX), delX, 3, "gluPickMatrix", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(delY), delY, 4, "gluPickMatrix", "GLdouble");
  Xen_check_type(Xen_is_GLint_(viewport), viewport, 5, "gluPickMatrix", "GLint*");
  gluPickMatrix(XEN_TO_C_GLdouble(x), XEN_TO_C_GLdouble(y), XEN_TO_C_GLdouble(delX), XEN_TO_C_GLdouble(delY), XEN_TO_C_GLint_(viewport));
  return(Xen_false);
}

static XEN gxg_gluProject(XEN objX, XEN objY, XEN objZ, XEN model, XEN proj, XEN view, XEN winX, XEN winY, XEN winZ)
{
  #define H_gluProject "GLint gluProject(GLdouble objX, GLdouble objY, GLdouble objZ, GLdouble* model, \
GLdouble* proj, GLint* view, GLdouble* [winX], GLdouble* [winY], GLdouble* [winZ])"
  GLdouble ref_winX[1];
  GLdouble ref_winY[1];
  GLdouble ref_winZ[1];
  Xen_check_type(Xen_is_GLdouble(objX), objX, 1, "gluProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(objY), objY, 2, "gluProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(objZ), objZ, 3, "gluProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble_(model), model, 4, "gluProject", "GLdouble*");
  Xen_check_type(Xen_is_GLdouble_(proj), proj, 5, "gluProject", "GLdouble*");
  Xen_check_type(Xen_is_GLint_(view), view, 6, "gluProject", "GLint*");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_GLint(gluProject(XEN_TO_C_GLdouble(objX), XEN_TO_C_GLdouble(objY), XEN_TO_C_GLdouble(objZ), XEN_TO_C_GLdouble_(model), 
                                       XEN_TO_C_GLdouble_(proj), XEN_TO_C_GLint_(view), ref_winX, ref_winY, ref_winZ));
    return(Xen_list_4(result, C_TO_XEN_GLdouble(ref_winX[0]), C_TO_XEN_GLdouble(ref_winY[0]), C_TO_XEN_GLdouble(ref_winZ[0])));
   }
}

static XEN gxg_gluPwlCurve(XEN nurb, XEN count, XEN data, XEN stride, XEN type)
{
  #define H_gluPwlCurve "void gluPwlCurve(GLUnurbs* nurb, GLint count, GLfloat* data, GLint stride, GLenum type)"
  Xen_check_type(Xen_is_GLUnurbs_(nurb), nurb, 1, "gluPwlCurve", "GLUnurbs*");
  Xen_check_type(Xen_is_GLint(count), count, 2, "gluPwlCurve", "GLint");
  Xen_check_type(Xen_is_GLfloat_(data), data, 3, "gluPwlCurve", "GLfloat*");
  Xen_check_type(Xen_is_GLint(stride), stride, 4, "gluPwlCurve", "GLint");
  Xen_check_type(Xen_is_GLenum(type), type, 5, "gluPwlCurve", "GLenum");
  gluPwlCurve(XEN_TO_C_GLUnurbs_(nurb), XEN_TO_C_GLint(count), XEN_TO_C_GLfloat_(data), XEN_TO_C_GLint(stride), XEN_TO_C_GLenum(type));
  return(Xen_false);
}

static XEN gxg_gluQuadricCallback(XEN quad, XEN which, XEN CallBackFunc)
{
  #define H_gluQuadricCallback "void gluQuadricCallback(GLUquadric* quad, GLenum which, _GLUfuncptr CallBackFunc)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluQuadricCallback", "GLUquadric*");
  Xen_check_type(Xen_is_GLenum(which), which, 2, "gluQuadricCallback", "GLenum");
  Xen_check_type(Xen_is__GLUfuncptr(CallBackFunc), CallBackFunc, 3, "gluQuadricCallback", "_GLUfuncptr");
  gluQuadricCallback(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLenum(which), XEN_TO_C__GLUfuncptr(CallBackFunc));
  return(Xen_false);
}

static XEN gxg_gluQuadricDrawStyle(XEN quad, XEN draw)
{
  #define H_gluQuadricDrawStyle "void gluQuadricDrawStyle(GLUquadric* quad, GLenum draw)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluQuadricDrawStyle", "GLUquadric*");
  Xen_check_type(Xen_is_GLenum(draw), draw, 2, "gluQuadricDrawStyle", "GLenum");
  gluQuadricDrawStyle(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLenum(draw));
  return(Xen_false);
}

static XEN gxg_gluQuadricNormals(XEN quad, XEN normal)
{
  #define H_gluQuadricNormals "void gluQuadricNormals(GLUquadric* quad, GLenum normal)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluQuadricNormals", "GLUquadric*");
  Xen_check_type(Xen_is_GLenum(normal), normal, 2, "gluQuadricNormals", "GLenum");
  gluQuadricNormals(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLenum(normal));
  return(Xen_false);
}

static XEN gxg_gluQuadricOrientation(XEN quad, XEN orientation)
{
  #define H_gluQuadricOrientation "void gluQuadricOrientation(GLUquadric* quad, GLenum orientation)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluQuadricOrientation", "GLUquadric*");
  Xen_check_type(Xen_is_GLenum(orientation), orientation, 2, "gluQuadricOrientation", "GLenum");
  gluQuadricOrientation(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLenum(orientation));
  return(Xen_false);
}

static XEN gxg_gluQuadricTexture(XEN quad, XEN texture)
{
  #define H_gluQuadricTexture "void gluQuadricTexture(GLUquadric* quad, GLboolean texture)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluQuadricTexture", "GLUquadric*");
  Xen_check_type(Xen_is_GLboolean(texture), texture, 2, "gluQuadricTexture", "GLboolean");
  gluQuadricTexture(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLboolean(texture));
  return(Xen_false);
}

static XEN gxg_gluScaleImage(XEN format, XEN wIn, XEN hIn, XEN typeIn, XEN dataIn, XEN wOut, XEN hOut, XEN typeOut, XEN dataOut)
{
  #define H_gluScaleImage "GLint gluScaleImage(GLenum format, GLsizei wIn, GLsizei hIn, GLenum typeIn, \
void* dataIn, GLsizei wOut, GLsizei hOut, GLenum typeOut, GLvoid* dataOut)"
  Xen_check_type(Xen_is_GLenum(format), format, 1, "gluScaleImage", "GLenum");
  Xen_check_type(Xen_is_GLsizei(wIn), wIn, 2, "gluScaleImage", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(hIn), hIn, 3, "gluScaleImage", "GLsizei");
  Xen_check_type(Xen_is_GLenum(typeIn), typeIn, 4, "gluScaleImage", "GLenum");
  Xen_check_type(Xen_is_void_(dataIn), dataIn, 5, "gluScaleImage", "void*");
  Xen_check_type(Xen_is_GLsizei(wOut), wOut, 6, "gluScaleImage", "GLsizei");
  Xen_check_type(Xen_is_GLsizei(hOut), hOut, 7, "gluScaleImage", "GLsizei");
  Xen_check_type(Xen_is_GLenum(typeOut), typeOut, 8, "gluScaleImage", "GLenum");
  Xen_check_type(Xen_is_GLvoid_(dataOut), dataOut, 9, "gluScaleImage", "GLvoid*");
  return(C_TO_XEN_GLint(gluScaleImage(XEN_TO_C_GLenum(format), XEN_TO_C_GLsizei(wIn), XEN_TO_C_GLsizei(hIn), XEN_TO_C_GLenum(typeIn), 
                                      XEN_TO_C_void_(dataIn), XEN_TO_C_GLsizei(wOut), XEN_TO_C_GLsizei(hOut), XEN_TO_C_GLenum(typeOut), 
                                      XEN_TO_C_GLvoid_(dataOut))));
}

static XEN gxg_gluSphere(XEN quad, XEN radius, XEN slices, XEN stacks)
{
  #define H_gluSphere "void gluSphere(GLUquadric* quad, GLdouble radius, GLint slices, GLint stacks)"
  Xen_check_type(Xen_is_GLUquadric_(quad), quad, 1, "gluSphere", "GLUquadric*");
  Xen_check_type(Xen_is_GLdouble(radius), radius, 2, "gluSphere", "GLdouble");
  Xen_check_type(Xen_is_GLint(slices), slices, 3, "gluSphere", "GLint");
  Xen_check_type(Xen_is_GLint(stacks), stacks, 4, "gluSphere", "GLint");
  gluSphere(XEN_TO_C_GLUquadric_(quad), XEN_TO_C_GLdouble(radius), XEN_TO_C_GLint(slices), XEN_TO_C_GLint(stacks));
  return(Xen_false);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessBeginContour(XEN tess)
{
  #define H_gluTessBeginContour "void gluTessBeginContour(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessBeginContour", "GLUtesselator*");
  gluTessBeginContour(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessBeginPolygon(XEN tess, XEN data)
{
  #define H_gluTessBeginPolygon "void gluTessBeginPolygon(GLUtesselator* tess, GLvoid* data)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessBeginPolygon", "GLUtesselator*");
  Xen_check_type(Xen_is_GLvoid_(data), data, 2, "gluTessBeginPolygon", "GLvoid*");
  gluTessBeginPolygon(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLvoid_(data));
  return(Xen_false);
}
#endif

static XEN gxg_gluTessCallback(XEN tess, XEN which, XEN CallBackFunc)
{
  #define H_gluTessCallback "void gluTessCallback(GLUtesselator* tess, GLenum which, _GLUfuncptr CallBackFunc)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessCallback", "GLUtesselator*");
  Xen_check_type(Xen_is_GLenum(which), which, 2, "gluTessCallback", "GLenum");
  Xen_check_type(Xen_is__GLUfuncptr(CallBackFunc), CallBackFunc, 3, "gluTessCallback", "_GLUfuncptr");
  gluTessCallback(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(which), XEN_TO_C__GLUfuncptr(CallBackFunc));
  return(Xen_false);
}

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessEndContour(XEN tess)
{
  #define H_gluTessEndContour "void gluTessEndContour(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessEndContour", "GLUtesselator*");
  gluTessEndContour(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessEndPolygon(XEN tess)
{
  #define H_gluTessEndPolygon "void gluTessEndPolygon(GLUtesselator* tess)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessEndPolygon", "GLUtesselator*");
  gluTessEndPolygon(XEN_TO_C_GLUtesselator_(tess));
  return(Xen_false);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessNormal(XEN tess, XEN valueX, XEN valueY, XEN valueZ)
{
  #define H_gluTessNormal "void gluTessNormal(GLUtesselator* tess, GLdouble valueX, GLdouble valueY, \
GLdouble valueZ)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessNormal", "GLUtesselator*");
  Xen_check_type(Xen_is_GLdouble(valueX), valueX, 2, "gluTessNormal", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(valueY), valueY, 3, "gluTessNormal", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(valueZ), valueZ, 4, "gluTessNormal", "GLdouble");
  gluTessNormal(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLdouble(valueX), XEN_TO_C_GLdouble(valueY), XEN_TO_C_GLdouble(valueZ));
  return(Xen_false);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessProperty(XEN tess, XEN which, XEN data)
{
  #define H_gluTessProperty "void gluTessProperty(GLUtesselator* tess, GLenum which, GLdouble data)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessProperty", "GLUtesselator*");
  Xen_check_type(Xen_is_GLenum(which), which, 2, "gluTessProperty", "GLenum");
  Xen_check_type(Xen_is_GLdouble(data), data, 3, "gluTessProperty", "GLdouble");
  gluTessProperty(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLenum(which), XEN_TO_C_GLdouble(data));
  return(Xen_false);
}
#endif

#ifdef GLU_VERSION_1_2
static XEN gxg_gluTessVertex(XEN tess, XEN location, XEN data)
{
  #define H_gluTessVertex "void gluTessVertex(GLUtesselator* tess, GLdouble* location, GLvoid* data)"
  Xen_check_type(Xen_is_GLUtesselator_(tess), tess, 1, "gluTessVertex", "GLUtesselator*");
  Xen_check_type(Xen_is_GLdouble_(location), location, 2, "gluTessVertex", "GLdouble*");
  Xen_check_type(Xen_is_GLvoid_(data), data, 3, "gluTessVertex", "GLvoid*");
  gluTessVertex(XEN_TO_C_GLUtesselator_(tess), XEN_TO_C_GLdouble_(location), XEN_TO_C_GLvoid_(data));
  return(Xen_false);
}
#endif

static XEN gxg_gluUnProject(XEN winX, XEN winY, XEN winZ, XEN model, XEN proj, XEN view, XEN objX, XEN objY, XEN objZ)
{
  #define H_gluUnProject "GLint gluUnProject(GLdouble winX, GLdouble winY, GLdouble winZ, GLdouble* model, \
GLdouble* proj, GLint* view, GLdouble* [objX], GLdouble* [objY], GLdouble* [objZ])"
  GLdouble ref_objX[1];
  GLdouble ref_objY[1];
  GLdouble ref_objZ[1];
  Xen_check_type(Xen_is_GLdouble(winX), winX, 1, "gluUnProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(winY), winY, 2, "gluUnProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(winZ), winZ, 3, "gluUnProject", "GLdouble");
  Xen_check_type(Xen_is_GLdouble_(model), model, 4, "gluUnProject", "GLdouble*");
  Xen_check_type(Xen_is_GLdouble_(proj), proj, 5, "gluUnProject", "GLdouble*");
  Xen_check_type(Xen_is_GLint_(view), view, 6, "gluUnProject", "GLint*");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_GLint(gluUnProject(XEN_TO_C_GLdouble(winX), XEN_TO_C_GLdouble(winY), XEN_TO_C_GLdouble(winZ), XEN_TO_C_GLdouble_(model), 
                                         XEN_TO_C_GLdouble_(proj), XEN_TO_C_GLint_(view), ref_objX, ref_objY, ref_objZ));
    return(Xen_list_4(result, C_TO_XEN_GLdouble(ref_objX[0]), C_TO_XEN_GLdouble(ref_objY[0]), C_TO_XEN_GLdouble(ref_objZ[0])));
   }
}

static XEN gxg_gluUnProject4(XEN arglist)
{
  #define H_gluUnProject4 "GLint gluUnProject4(GLdouble winX, GLdouble winY, GLdouble winZ, GLdouble clipW, \
GLdouble* model, GLdouble* proj, GLint* view, GLdouble near, GLdouble far, GLdouble* [objX], GLdouble* [objY], \
GLdouble* [objZ], GLdouble* [objW])"
  GLdouble ref_objX[1];
  GLdouble ref_objY[1];
  GLdouble ref_objZ[1];
  GLdouble ref_objW[1];
  XEN winX, winY, winZ, clipW, model, proj, view, near, far;
  winX = Xen_list_ref(arglist, 0);
  winY = Xen_list_ref(arglist, 1);
  winZ = Xen_list_ref(arglist, 2);
  clipW = Xen_list_ref(arglist, 3);
  model = Xen_list_ref(arglist, 4);
  proj = Xen_list_ref(arglist, 5);
  view = Xen_list_ref(arglist, 6);
  near = Xen_list_ref(arglist, 7);
  far = Xen_list_ref(arglist, 8);
  Xen_check_type(Xen_is_GLdouble(winX), winX, 1, "gluUnProject4", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(winY), winY, 2, "gluUnProject4", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(winZ), winZ, 3, "gluUnProject4", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(clipW), clipW, 4, "gluUnProject4", "GLdouble");
  Xen_check_type(Xen_is_GLdouble_(model), model, 5, "gluUnProject4", "GLdouble*");
  Xen_check_type(Xen_is_GLdouble_(proj), proj, 6, "gluUnProject4", "GLdouble*");
  Xen_check_type(Xen_is_GLint_(view), view, 7, "gluUnProject4", "GLint*");
  Xen_check_type(Xen_is_GLdouble(near), near, 8, "gluUnProject4", "GLdouble");
  Xen_check_type(Xen_is_GLdouble(far), far, 9, "gluUnProject4", "GLdouble");
  {
    XEN result = Xen_false;
    result = C_TO_XEN_GLint(gluUnProject4(XEN_TO_C_GLdouble(winX), XEN_TO_C_GLdouble(winY), XEN_TO_C_GLdouble(winZ), XEN_TO_C_GLdouble(clipW), 
                                          XEN_TO_C_GLdouble_(model), XEN_TO_C_GLdouble_(proj), XEN_TO_C_GLint_(view), XEN_TO_C_GLdouble(near), 
                                          XEN_TO_C_GLdouble(far), ref_objX, ref_objY, ref_objZ, ref_objW));
    return(Xen_list_5(result, C_TO_XEN_GLdouble(ref_objX[0]), C_TO_XEN_GLdouble(ref_objY[0]), C_TO_XEN_GLdouble(ref_objZ[0]), C_TO_XEN_GLdouble(ref_objW[0])));
   }
}

#endif
#if USE_MOTIF
Xen_wrap_3_args(gxg_glXChooseVisual_w, gxg_glXChooseVisual)
Xen_wrap_4_args(gxg_glXCopyContext_w, gxg_glXCopyContext)
Xen_wrap_4_args(gxg_glXCreateContext_w, gxg_glXCreateContext)
Xen_wrap_3_args(gxg_glXCreateGLXPixmap_w, gxg_glXCreateGLXPixmap)
Xen_wrap_2_args(gxg_glXDestroyContext_w, gxg_glXDestroyContext)
Xen_wrap_2_args(gxg_glXDestroyGLXPixmap_w, gxg_glXDestroyGLXPixmap)
Xen_wrap_4_optional_args(gxg_glXGetConfig_w, gxg_glXGetConfig)
Xen_wrap_no_args(gxg_glXGetCurrentContext_w, gxg_glXGetCurrentContext)
Xen_wrap_no_args(gxg_glXGetCurrentDrawable_w, gxg_glXGetCurrentDrawable)
Xen_wrap_2_args(gxg_glXIsDirect_w, gxg_glXIsDirect)
Xen_wrap_3_args(gxg_glXMakeCurrent_w, gxg_glXMakeCurrent)
Xen_wrap_3_optional_args(gxg_glXQueryExtension_w, gxg_glXQueryExtension)
Xen_wrap_3_optional_args(gxg_glXQueryVersion_w, gxg_glXQueryVersion)
Xen_wrap_2_args(gxg_glXSwapBuffers_w, gxg_glXSwapBuffers)
Xen_wrap_4_args(gxg_glXUseXFont_w, gxg_glXUseXFont)
Xen_wrap_no_args(gxg_glXWaitGL_w, gxg_glXWaitGL)
Xen_wrap_no_args(gxg_glXWaitX_w, gxg_glXWaitX)
Xen_wrap_2_args(gxg_glXGetClientString_w, gxg_glXGetClientString)
Xen_wrap_3_args(gxg_glXQueryServerString_w, gxg_glXQueryServerString)
Xen_wrap_2_args(gxg_glXQueryExtensionsString_w, gxg_glXQueryExtensionsString)
#endif
Xen_wrap_1_arg(gxg_glClearIndex_w, gxg_glClearIndex)
Xen_wrap_4_args(gxg_glClearColor_w, gxg_glClearColor)
Xen_wrap_1_arg(gxg_glClear_w, gxg_glClear)
Xen_wrap_1_arg(gxg_glIndexMask_w, gxg_glIndexMask)
Xen_wrap_4_args(gxg_glColorMask_w, gxg_glColorMask)
Xen_wrap_2_args(gxg_glAlphaFunc_w, gxg_glAlphaFunc)
Xen_wrap_2_args(gxg_glBlendFunc_w, gxg_glBlendFunc)
Xen_wrap_1_arg(gxg_glLogicOp_w, gxg_glLogicOp)
Xen_wrap_1_arg(gxg_glCullFace_w, gxg_glCullFace)
Xen_wrap_1_arg(gxg_glFrontFace_w, gxg_glFrontFace)
Xen_wrap_1_arg(gxg_glPointSize_w, gxg_glPointSize)
Xen_wrap_1_arg(gxg_glLineWidth_w, gxg_glLineWidth)
Xen_wrap_2_args(gxg_glLineStipple_w, gxg_glLineStipple)
Xen_wrap_2_args(gxg_glPolygonMode_w, gxg_glPolygonMode)
Xen_wrap_2_args(gxg_glPolygonOffset_w, gxg_glPolygonOffset)
Xen_wrap_1_arg(gxg_glPolygonStipple_w, gxg_glPolygonStipple)
Xen_wrap_1_arg(gxg_glEdgeFlag_w, gxg_glEdgeFlag)
Xen_wrap_4_args(gxg_glScissor_w, gxg_glScissor)
Xen_wrap_2_args(gxg_glClipPlane_w, gxg_glClipPlane)
Xen_wrap_2_optional_args(gxg_glGetClipPlane_w, gxg_glGetClipPlane)
Xen_wrap_1_arg(gxg_glDrawBuffer_w, gxg_glDrawBuffer)
Xen_wrap_1_arg(gxg_glReadBuffer_w, gxg_glReadBuffer)
Xen_wrap_1_arg(gxg_glEnable_w, gxg_glEnable)
Xen_wrap_1_arg(gxg_glDisable_w, gxg_glDisable)
Xen_wrap_1_arg(gxg_glIsEnabled_w, gxg_glIsEnabled)
Xen_wrap_1_arg(gxg_glEnableClientState_w, gxg_glEnableClientState)
Xen_wrap_1_arg(gxg_glDisableClientState_w, gxg_glDisableClientState)
Xen_wrap_2_optional_args(gxg_glGetBooleanv_w, gxg_glGetBooleanv)
Xen_wrap_2_optional_args(gxg_glGetDoublev_w, gxg_glGetDoublev)
Xen_wrap_2_optional_args(gxg_glGetFloatv_w, gxg_glGetFloatv)
Xen_wrap_2_optional_args(gxg_glGetIntegerv_w, gxg_glGetIntegerv)
Xen_wrap_1_arg(gxg_glPushAttrib_w, gxg_glPushAttrib)
Xen_wrap_no_args(gxg_glPopAttrib_w, gxg_glPopAttrib)
Xen_wrap_1_arg(gxg_glPushClientAttrib_w, gxg_glPushClientAttrib)
Xen_wrap_no_args(gxg_glPopClientAttrib_w, gxg_glPopClientAttrib)
Xen_wrap_1_arg(gxg_glRenderMode_w, gxg_glRenderMode)
Xen_wrap_no_args(gxg_glGetError_w, gxg_glGetError)
Xen_wrap_1_arg(gxg_glGetString_w, gxg_glGetString)
Xen_wrap_no_args(gxg_glFinish_w, gxg_glFinish)
Xen_wrap_no_args(gxg_glFlush_w, gxg_glFlush)
Xen_wrap_2_args(gxg_glHint_w, gxg_glHint)
Xen_wrap_1_arg(gxg_glClearDepth_w, gxg_glClearDepth)
Xen_wrap_1_arg(gxg_glDepthFunc_w, gxg_glDepthFunc)
Xen_wrap_1_arg(gxg_glDepthMask_w, gxg_glDepthMask)
Xen_wrap_2_args(gxg_glDepthRange_w, gxg_glDepthRange)
Xen_wrap_4_args(gxg_glClearAccum_w, gxg_glClearAccum)
Xen_wrap_2_args(gxg_glAccum_w, gxg_glAccum)
Xen_wrap_1_arg(gxg_glMatrixMode_w, gxg_glMatrixMode)
Xen_wrap_6_args(gxg_glOrtho_w, gxg_glOrtho)
Xen_wrap_6_args(gxg_glFrustum_w, gxg_glFrustum)
Xen_wrap_4_args(gxg_glViewport_w, gxg_glViewport)
Xen_wrap_no_args(gxg_glPushMatrix_w, gxg_glPushMatrix)
Xen_wrap_no_args(gxg_glPopMatrix_w, gxg_glPopMatrix)
Xen_wrap_no_args(gxg_glLoadIdentity_w, gxg_glLoadIdentity)
Xen_wrap_1_arg(gxg_glLoadMatrixd_w, gxg_glLoadMatrixd)
Xen_wrap_1_arg(gxg_glLoadMatrixf_w, gxg_glLoadMatrixf)
Xen_wrap_1_arg(gxg_glMultMatrixd_w, gxg_glMultMatrixd)
Xen_wrap_1_arg(gxg_glMultMatrixf_w, gxg_glMultMatrixf)
Xen_wrap_4_args(gxg_glRotated_w, gxg_glRotated)
Xen_wrap_4_args(gxg_glRotatef_w, gxg_glRotatef)
Xen_wrap_3_args(gxg_glScaled_w, gxg_glScaled)
Xen_wrap_3_args(gxg_glScalef_w, gxg_glScalef)
Xen_wrap_3_args(gxg_glTranslated_w, gxg_glTranslated)
Xen_wrap_3_args(gxg_glTranslatef_w, gxg_glTranslatef)
Xen_wrap_1_arg(gxg_glIsList_w, gxg_glIsList)
Xen_wrap_2_args(gxg_glDeleteLists_w, gxg_glDeleteLists)
Xen_wrap_1_arg(gxg_glGenLists_w, gxg_glGenLists)
Xen_wrap_2_args(gxg_glNewList_w, gxg_glNewList)
Xen_wrap_no_args(gxg_glEndList_w, gxg_glEndList)
Xen_wrap_1_arg(gxg_glCallList_w, gxg_glCallList)
Xen_wrap_3_args(gxg_glCallLists_w, gxg_glCallLists)
Xen_wrap_1_arg(gxg_glListBase_w, gxg_glListBase)
Xen_wrap_1_arg(gxg_glBegin_w, gxg_glBegin)
Xen_wrap_no_args(gxg_glEnd_w, gxg_glEnd)
Xen_wrap_2_args(gxg_glVertex2d_w, gxg_glVertex2d)
Xen_wrap_2_args(gxg_glVertex2f_w, gxg_glVertex2f)
Xen_wrap_2_args(gxg_glVertex2i_w, gxg_glVertex2i)
Xen_wrap_2_args(gxg_glVertex2s_w, gxg_glVertex2s)
Xen_wrap_3_args(gxg_glVertex3d_w, gxg_glVertex3d)
Xen_wrap_3_args(gxg_glVertex3f_w, gxg_glVertex3f)
Xen_wrap_3_args(gxg_glVertex3i_w, gxg_glVertex3i)
Xen_wrap_3_args(gxg_glVertex3s_w, gxg_glVertex3s)
Xen_wrap_4_args(gxg_glVertex4d_w, gxg_glVertex4d)
Xen_wrap_4_args(gxg_glVertex4f_w, gxg_glVertex4f)
Xen_wrap_4_args(gxg_glVertex4i_w, gxg_glVertex4i)
Xen_wrap_4_args(gxg_glVertex4s_w, gxg_glVertex4s)
Xen_wrap_3_args(gxg_glNormal3b_w, gxg_glNormal3b)
Xen_wrap_3_args(gxg_glNormal3d_w, gxg_glNormal3d)
Xen_wrap_3_args(gxg_glNormal3f_w, gxg_glNormal3f)
Xen_wrap_3_args(gxg_glNormal3i_w, gxg_glNormal3i)
Xen_wrap_3_args(gxg_glNormal3s_w, gxg_glNormal3s)
Xen_wrap_1_arg(gxg_glIndexd_w, gxg_glIndexd)
Xen_wrap_1_arg(gxg_glIndexf_w, gxg_glIndexf)
Xen_wrap_1_arg(gxg_glIndexi_w, gxg_glIndexi)
Xen_wrap_1_arg(gxg_glIndexs_w, gxg_glIndexs)
Xen_wrap_1_arg(gxg_glIndexub_w, gxg_glIndexub)
Xen_wrap_3_args(gxg_glColor3b_w, gxg_glColor3b)
Xen_wrap_3_args(gxg_glColor3d_w, gxg_glColor3d)
Xen_wrap_3_args(gxg_glColor3f_w, gxg_glColor3f)
Xen_wrap_3_args(gxg_glColor3i_w, gxg_glColor3i)
Xen_wrap_3_args(gxg_glColor3s_w, gxg_glColor3s)
Xen_wrap_3_args(gxg_glColor3ub_w, gxg_glColor3ub)
Xen_wrap_3_args(gxg_glColor3ui_w, gxg_glColor3ui)
Xen_wrap_3_args(gxg_glColor3us_w, gxg_glColor3us)
Xen_wrap_4_args(gxg_glColor4b_w, gxg_glColor4b)
Xen_wrap_4_args(gxg_glColor4d_w, gxg_glColor4d)
Xen_wrap_4_args(gxg_glColor4f_w, gxg_glColor4f)
Xen_wrap_4_args(gxg_glColor4i_w, gxg_glColor4i)
Xen_wrap_4_args(gxg_glColor4s_w, gxg_glColor4s)
Xen_wrap_4_args(gxg_glColor4ub_w, gxg_glColor4ub)
Xen_wrap_4_args(gxg_glColor4ui_w, gxg_glColor4ui)
Xen_wrap_4_args(gxg_glColor4us_w, gxg_glColor4us)
Xen_wrap_1_arg(gxg_glTexCoord1d_w, gxg_glTexCoord1d)
Xen_wrap_1_arg(gxg_glTexCoord1f_w, gxg_glTexCoord1f)
Xen_wrap_1_arg(gxg_glTexCoord1i_w, gxg_glTexCoord1i)
Xen_wrap_1_arg(gxg_glTexCoord1s_w, gxg_glTexCoord1s)
Xen_wrap_2_args(gxg_glTexCoord2d_w, gxg_glTexCoord2d)
Xen_wrap_2_args(gxg_glTexCoord2f_w, gxg_glTexCoord2f)
Xen_wrap_2_args(gxg_glTexCoord2i_w, gxg_glTexCoord2i)
Xen_wrap_2_args(gxg_glTexCoord2s_w, gxg_glTexCoord2s)
Xen_wrap_3_args(gxg_glTexCoord3d_w, gxg_glTexCoord3d)
Xen_wrap_3_args(gxg_glTexCoord3f_w, gxg_glTexCoord3f)
Xen_wrap_3_args(gxg_glTexCoord3i_w, gxg_glTexCoord3i)
Xen_wrap_3_args(gxg_glTexCoord3s_w, gxg_glTexCoord3s)
Xen_wrap_4_args(gxg_glTexCoord4d_w, gxg_glTexCoord4d)
Xen_wrap_4_args(gxg_glTexCoord4f_w, gxg_glTexCoord4f)
Xen_wrap_4_args(gxg_glTexCoord4i_w, gxg_glTexCoord4i)
Xen_wrap_4_args(gxg_glTexCoord4s_w, gxg_glTexCoord4s)
Xen_wrap_2_args(gxg_glRasterPos2d_w, gxg_glRasterPos2d)
Xen_wrap_2_args(gxg_glRasterPos2f_w, gxg_glRasterPos2f)
Xen_wrap_2_args(gxg_glRasterPos2i_w, gxg_glRasterPos2i)
Xen_wrap_2_args(gxg_glRasterPos2s_w, gxg_glRasterPos2s)
Xen_wrap_3_args(gxg_glRasterPos3d_w, gxg_glRasterPos3d)
Xen_wrap_3_args(gxg_glRasterPos3f_w, gxg_glRasterPos3f)
Xen_wrap_3_args(gxg_glRasterPos3i_w, gxg_glRasterPos3i)
Xen_wrap_3_args(gxg_glRasterPos3s_w, gxg_glRasterPos3s)
Xen_wrap_4_args(gxg_glRasterPos4d_w, gxg_glRasterPos4d)
Xen_wrap_4_args(gxg_glRasterPos4f_w, gxg_glRasterPos4f)
Xen_wrap_4_args(gxg_glRasterPos4i_w, gxg_glRasterPos4i)
Xen_wrap_4_args(gxg_glRasterPos4s_w, gxg_glRasterPos4s)
Xen_wrap_4_args(gxg_glRectd_w, gxg_glRectd)
Xen_wrap_4_args(gxg_glRectf_w, gxg_glRectf)
Xen_wrap_4_args(gxg_glRecti_w, gxg_glRecti)
Xen_wrap_4_args(gxg_glRects_w, gxg_glRects)
Xen_wrap_4_args(gxg_glVertexPointer_w, gxg_glVertexPointer)
Xen_wrap_3_args(gxg_glNormalPointer_w, gxg_glNormalPointer)
Xen_wrap_4_args(gxg_glColorPointer_w, gxg_glColorPointer)
Xen_wrap_3_args(gxg_glIndexPointer_w, gxg_glIndexPointer)
Xen_wrap_4_args(gxg_glTexCoordPointer_w, gxg_glTexCoordPointer)
Xen_wrap_2_args(gxg_glEdgeFlagPointer_w, gxg_glEdgeFlagPointer)
Xen_wrap_2_optional_args(gxg_glGetPointerv_w, gxg_glGetPointerv)
Xen_wrap_1_arg(gxg_glArrayElement_w, gxg_glArrayElement)
Xen_wrap_3_args(gxg_glDrawArrays_w, gxg_glDrawArrays)
Xen_wrap_4_args(gxg_glDrawElements_w, gxg_glDrawElements)
Xen_wrap_3_args(gxg_glInterleavedArrays_w, gxg_glInterleavedArrays)
Xen_wrap_1_arg(gxg_glShadeModel_w, gxg_glShadeModel)
Xen_wrap_3_args(gxg_glLightf_w, gxg_glLightf)
Xen_wrap_3_args(gxg_glLighti_w, gxg_glLighti)
Xen_wrap_3_optional_args(gxg_glGetLightfv_w, gxg_glGetLightfv)
Xen_wrap_3_optional_args(gxg_glGetLightiv_w, gxg_glGetLightiv)
Xen_wrap_2_args(gxg_glLightModelf_w, gxg_glLightModelf)
Xen_wrap_2_args(gxg_glLightModeli_w, gxg_glLightModeli)
Xen_wrap_3_args(gxg_glMaterialf_w, gxg_glMaterialf)
Xen_wrap_3_args(gxg_glMateriali_w, gxg_glMateriali)
Xen_wrap_3_optional_args(gxg_glGetMaterialfv_w, gxg_glGetMaterialfv)
Xen_wrap_3_optional_args(gxg_glGetMaterialiv_w, gxg_glGetMaterialiv)
Xen_wrap_2_args(gxg_glColorMaterial_w, gxg_glColorMaterial)
Xen_wrap_2_args(gxg_glPixelZoom_w, gxg_glPixelZoom)
Xen_wrap_2_args(gxg_glPixelStoref_w, gxg_glPixelStoref)
Xen_wrap_2_args(gxg_glPixelStorei_w, gxg_glPixelStorei)
Xen_wrap_2_args(gxg_glPixelTransferf_w, gxg_glPixelTransferf)
Xen_wrap_2_args(gxg_glPixelTransferi_w, gxg_glPixelTransferi)
Xen_wrap_2_optional_args(gxg_glGetPixelMapfv_w, gxg_glGetPixelMapfv)
Xen_wrap_2_optional_args(gxg_glGetPixelMapuiv_w, gxg_glGetPixelMapuiv)
Xen_wrap_2_optional_args(gxg_glGetPixelMapusv_w, gxg_glGetPixelMapusv)
Xen_wrap_7_args(gxg_glBitmap_w, gxg_glBitmap)
Xen_wrap_7_args(gxg_glReadPixels_w, gxg_glReadPixels)
Xen_wrap_5_args(gxg_glDrawPixels_w, gxg_glDrawPixels)
Xen_wrap_5_args(gxg_glCopyPixels_w, gxg_glCopyPixels)
Xen_wrap_3_args(gxg_glStencilFunc_w, gxg_glStencilFunc)
Xen_wrap_1_arg(gxg_glStencilMask_w, gxg_glStencilMask)
Xen_wrap_3_args(gxg_glStencilOp_w, gxg_glStencilOp)
Xen_wrap_1_arg(gxg_glClearStencil_w, gxg_glClearStencil)
Xen_wrap_3_args(gxg_glTexGend_w, gxg_glTexGend)
Xen_wrap_3_args(gxg_glTexGenf_w, gxg_glTexGenf)
Xen_wrap_3_args(gxg_glTexGeni_w, gxg_glTexGeni)
Xen_wrap_3_optional_args(gxg_glGetTexGendv_w, gxg_glGetTexGendv)
Xen_wrap_3_optional_args(gxg_glGetTexGenfv_w, gxg_glGetTexGenfv)
Xen_wrap_3_optional_args(gxg_glGetTexGeniv_w, gxg_glGetTexGeniv)
Xen_wrap_3_args(gxg_glTexEnvf_w, gxg_glTexEnvf)
Xen_wrap_3_args(gxg_glTexEnvi_w, gxg_glTexEnvi)
Xen_wrap_3_optional_args(gxg_glGetTexEnvfv_w, gxg_glGetTexEnvfv)
Xen_wrap_3_optional_args(gxg_glGetTexEnviv_w, gxg_glGetTexEnviv)
Xen_wrap_3_args(gxg_glTexParameterf_w, gxg_glTexParameterf)
Xen_wrap_3_args(gxg_glTexParameteri_w, gxg_glTexParameteri)
Xen_wrap_3_optional_args(gxg_glGetTexParameterfv_w, gxg_glGetTexParameterfv)
Xen_wrap_3_optional_args(gxg_glGetTexParameteriv_w, gxg_glGetTexParameteriv)
Xen_wrap_4_optional_args(gxg_glGetTexLevelParameterfv_w, gxg_glGetTexLevelParameterfv)
Xen_wrap_4_optional_args(gxg_glGetTexLevelParameteriv_w, gxg_glGetTexLevelParameteriv)
Xen_wrap_8_args(gxg_glTexImage1D_w, gxg_glTexImage1D)
Xen_wrap_9_args(gxg_glTexImage2D_w, gxg_glTexImage2D)
Xen_wrap_2_args(gxg_glGenTextures_w, gxg_glGenTextures)
Xen_wrap_2_args(gxg_glDeleteTextures_w, gxg_glDeleteTextures)
Xen_wrap_2_args(gxg_glBindTexture_w, gxg_glBindTexture)
Xen_wrap_3_args(gxg_glAreTexturesResident_w, gxg_glAreTexturesResident)
Xen_wrap_1_arg(gxg_glIsTexture_w, gxg_glIsTexture)
Xen_wrap_7_args(gxg_glTexSubImage1D_w, gxg_glTexSubImage1D)
Xen_wrap_9_args(gxg_glTexSubImage2D_w, gxg_glTexSubImage2D)
Xen_wrap_7_args(gxg_glCopyTexImage1D_w, gxg_glCopyTexImage1D)
Xen_wrap_8_args(gxg_glCopyTexImage2D_w, gxg_glCopyTexImage2D)
Xen_wrap_6_args(gxg_glCopyTexSubImage1D_w, gxg_glCopyTexSubImage1D)
Xen_wrap_8_args(gxg_glCopyTexSubImage2D_w, gxg_glCopyTexSubImage2D)
Xen_wrap_6_args(gxg_glMap1d_w, gxg_glMap1d)
Xen_wrap_6_args(gxg_glMap1f_w, gxg_glMap1f)
Xen_wrap_any_args(gxg_glMap2d_w, gxg_glMap2d)
Xen_wrap_any_args(gxg_glMap2f_w, gxg_glMap2f)
Xen_wrap_3_optional_args(gxg_glGetMapdv_w, gxg_glGetMapdv)
Xen_wrap_3_optional_args(gxg_glGetMapfv_w, gxg_glGetMapfv)
Xen_wrap_3_optional_args(gxg_glGetMapiv_w, gxg_glGetMapiv)
Xen_wrap_1_arg(gxg_glEvalCoord1d_w, gxg_glEvalCoord1d)
Xen_wrap_1_arg(gxg_glEvalCoord1f_w, gxg_glEvalCoord1f)
Xen_wrap_2_args(gxg_glEvalCoord2d_w, gxg_glEvalCoord2d)
Xen_wrap_2_args(gxg_glEvalCoord2f_w, gxg_glEvalCoord2f)
Xen_wrap_3_args(gxg_glMapGrid1d_w, gxg_glMapGrid1d)
Xen_wrap_3_args(gxg_glMapGrid1f_w, gxg_glMapGrid1f)
Xen_wrap_6_args(gxg_glMapGrid2d_w, gxg_glMapGrid2d)
Xen_wrap_6_args(gxg_glMapGrid2f_w, gxg_glMapGrid2f)
Xen_wrap_1_arg(gxg_glEvalPoint1_w, gxg_glEvalPoint1)
Xen_wrap_2_args(gxg_glEvalPoint2_w, gxg_glEvalPoint2)
Xen_wrap_3_args(gxg_glEvalMesh1_w, gxg_glEvalMesh1)
Xen_wrap_5_args(gxg_glEvalMesh2_w, gxg_glEvalMesh2)
Xen_wrap_2_args(gxg_glFogf_w, gxg_glFogf)
Xen_wrap_2_args(gxg_glFogi_w, gxg_glFogi)
Xen_wrap_3_args(gxg_glFeedbackBuffer_w, gxg_glFeedbackBuffer)
Xen_wrap_1_arg(gxg_glPassThrough_w, gxg_glPassThrough)
Xen_wrap_2_args(gxg_glSelectBuffer_w, gxg_glSelectBuffer)
Xen_wrap_no_args(gxg_glInitNames_w, gxg_glInitNames)
Xen_wrap_1_arg(gxg_glLoadName_w, gxg_glLoadName)
Xen_wrap_1_arg(gxg_glPushName_w, gxg_glPushName)
Xen_wrap_no_args(gxg_glPopName_w, gxg_glPopName)
Xen_wrap_6_args(gxg_glDrawRangeElements_w, gxg_glDrawRangeElements)
Xen_wrap_any_args(gxg_glTexImage3D_w, gxg_glTexImage3D)
Xen_wrap_any_args(gxg_glTexSubImage3D_w, gxg_glTexSubImage3D)
Xen_wrap_9_args(gxg_glCopyTexSubImage3D_w, gxg_glCopyTexSubImage3D)
Xen_wrap_6_args(gxg_glColorTable_w, gxg_glColorTable)
Xen_wrap_6_args(gxg_glColorSubTable_w, gxg_glColorSubTable)
Xen_wrap_5_args(gxg_glCopyColorSubTable_w, gxg_glCopyColorSubTable)
Xen_wrap_5_args(gxg_glCopyColorTable_w, gxg_glCopyColorTable)
Xen_wrap_3_optional_args(gxg_glGetColorTableParameterfv_w, gxg_glGetColorTableParameterfv)
Xen_wrap_3_optional_args(gxg_glGetColorTableParameteriv_w, gxg_glGetColorTableParameteriv)
Xen_wrap_1_arg(gxg_glBlendEquation_w, gxg_glBlendEquation)
Xen_wrap_4_args(gxg_glBlendColor_w, gxg_glBlendColor)
Xen_wrap_4_args(gxg_glHistogram_w, gxg_glHistogram)
Xen_wrap_1_arg(gxg_glResetHistogram_w, gxg_glResetHistogram)
Xen_wrap_5_args(gxg_glGetHistogram_w, gxg_glGetHistogram)
Xen_wrap_3_optional_args(gxg_glGetHistogramParameterfv_w, gxg_glGetHistogramParameterfv)
Xen_wrap_3_optional_args(gxg_glGetHistogramParameteriv_w, gxg_glGetHistogramParameteriv)
Xen_wrap_3_args(gxg_glMinmax_w, gxg_glMinmax)
Xen_wrap_1_arg(gxg_glResetMinmax_w, gxg_glResetMinmax)
Xen_wrap_5_args(gxg_glGetMinmax_w, gxg_glGetMinmax)
Xen_wrap_3_optional_args(gxg_glGetMinmaxParameterfv_w, gxg_glGetMinmaxParameterfv)
Xen_wrap_3_optional_args(gxg_glGetMinmaxParameteriv_w, gxg_glGetMinmaxParameteriv)
Xen_wrap_6_args(gxg_glConvolutionFilter1D_w, gxg_glConvolutionFilter1D)
Xen_wrap_7_args(gxg_glConvolutionFilter2D_w, gxg_glConvolutionFilter2D)
Xen_wrap_3_args(gxg_glConvolutionParameterf_w, gxg_glConvolutionParameterf)
Xen_wrap_3_args(gxg_glConvolutionParameteri_w, gxg_glConvolutionParameteri)
Xen_wrap_5_args(gxg_glCopyConvolutionFilter1D_w, gxg_glCopyConvolutionFilter1D)
Xen_wrap_6_args(gxg_glCopyConvolutionFilter2D_w, gxg_glCopyConvolutionFilter2D)
Xen_wrap_8_args(gxg_glSeparableFilter2D_w, gxg_glSeparableFilter2D)
#if HAVE_GLU
Xen_wrap_1_arg(gxg_gluBeginCurve_w, gxg_gluBeginCurve)
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluBeginPolygon_w, gxg_gluBeginPolygon)
#endif
Xen_wrap_1_arg(gxg_gluBeginSurface_w, gxg_gluBeginSurface)
Xen_wrap_1_arg(gxg_gluBeginTrim_w, gxg_gluBeginTrim)
Xen_wrap_9_args(gxg_gluBuild1DMipmapLevels_w, gxg_gluBuild1DMipmapLevels)
Xen_wrap_6_args(gxg_gluBuild1DMipmaps_w, gxg_gluBuild1DMipmaps)
Xen_wrap_any_args(gxg_gluBuild2DMipmapLevels_w, gxg_gluBuild2DMipmapLevels)
Xen_wrap_7_args(gxg_gluBuild2DMipmaps_w, gxg_gluBuild2DMipmaps)
Xen_wrap_any_args(gxg_gluBuild3DMipmapLevels_w, gxg_gluBuild3DMipmapLevels)
Xen_wrap_8_args(gxg_gluBuild3DMipmaps_w, gxg_gluBuild3DMipmaps)
Xen_wrap_2_args(gxg_gluCheckExtension_w, gxg_gluCheckExtension)
Xen_wrap_6_args(gxg_gluCylinder_w, gxg_gluCylinder)
Xen_wrap_1_arg(gxg_gluDeleteNurbsRenderer_w, gxg_gluDeleteNurbsRenderer)
Xen_wrap_1_arg(gxg_gluDeleteQuadric_w, gxg_gluDeleteQuadric)
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluDeleteTess_w, gxg_gluDeleteTess)
#endif
Xen_wrap_5_args(gxg_gluDisk_w, gxg_gluDisk)
Xen_wrap_1_arg(gxg_gluEndCurve_w, gxg_gluEndCurve)
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluEndPolygon_w, gxg_gluEndPolygon)
#endif
Xen_wrap_1_arg(gxg_gluEndSurface_w, gxg_gluEndSurface)
Xen_wrap_1_arg(gxg_gluEndTrim_w, gxg_gluEndTrim)
Xen_wrap_1_arg(gxg_gluErrorString_w, gxg_gluErrorString)
Xen_wrap_3_args(gxg_gluGetNurbsProperty_w, gxg_gluGetNurbsProperty)
Xen_wrap_1_arg(gxg_gluGetString_w, gxg_gluGetString)
#ifdef GLU_VERSION_1_2
Xen_wrap_3_args(gxg_gluGetTessProperty_w, gxg_gluGetTessProperty)
#endif
Xen_wrap_4_args(gxg_gluLoadSamplingMatrices_w, gxg_gluLoadSamplingMatrices)
Xen_wrap_9_args(gxg_gluLookAt_w, gxg_gluLookAt)
Xen_wrap_no_args(gxg_gluNewNurbsRenderer_w, gxg_gluNewNurbsRenderer)
Xen_wrap_no_args(gxg_gluNewQuadric_w, gxg_gluNewQuadric)
#ifdef GLU_VERSION_1_2
Xen_wrap_no_args(gxg_gluNewTess_w, gxg_gluNewTess)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_2_args(gxg_gluNextContour_w, gxg_gluNextContour)
#endif
Xen_wrap_3_args(gxg_gluNurbsCallback_w, gxg_gluNurbsCallback)
Xen_wrap_2_args(gxg_gluNurbsCallbackData_w, gxg_gluNurbsCallbackData)
Xen_wrap_2_args(gxg_gluNurbsCallbackDataEXT_w, gxg_gluNurbsCallbackDataEXT)
Xen_wrap_7_args(gxg_gluNurbsCurve_w, gxg_gluNurbsCurve)
Xen_wrap_3_args(gxg_gluNurbsProperty_w, gxg_gluNurbsProperty)
Xen_wrap_any_args(gxg_gluNurbsSurface_w, gxg_gluNurbsSurface)
Xen_wrap_4_args(gxg_gluOrtho2D_w, gxg_gluOrtho2D)
Xen_wrap_7_args(gxg_gluPartialDisk_w, gxg_gluPartialDisk)
Xen_wrap_4_args(gxg_gluPerspective_w, gxg_gluPerspective)
Xen_wrap_5_args(gxg_gluPickMatrix_w, gxg_gluPickMatrix)
Xen_wrap_9_optional_args(gxg_gluProject_w, gxg_gluProject)
Xen_wrap_5_args(gxg_gluPwlCurve_w, gxg_gluPwlCurve)
Xen_wrap_3_args(gxg_gluQuadricCallback_w, gxg_gluQuadricCallback)
Xen_wrap_2_args(gxg_gluQuadricDrawStyle_w, gxg_gluQuadricDrawStyle)
Xen_wrap_2_args(gxg_gluQuadricNormals_w, gxg_gluQuadricNormals)
Xen_wrap_2_args(gxg_gluQuadricOrientation_w, gxg_gluQuadricOrientation)
Xen_wrap_2_args(gxg_gluQuadricTexture_w, gxg_gluQuadricTexture)
Xen_wrap_9_args(gxg_gluScaleImage_w, gxg_gluScaleImage)
Xen_wrap_4_args(gxg_gluSphere_w, gxg_gluSphere)
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluTessBeginContour_w, gxg_gluTessBeginContour)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_2_args(gxg_gluTessBeginPolygon_w, gxg_gluTessBeginPolygon)
#endif
Xen_wrap_3_args(gxg_gluTessCallback_w, gxg_gluTessCallback)
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluTessEndContour_w, gxg_gluTessEndContour)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_1_arg(gxg_gluTessEndPolygon_w, gxg_gluTessEndPolygon)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_4_args(gxg_gluTessNormal_w, gxg_gluTessNormal)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_3_args(gxg_gluTessProperty_w, gxg_gluTessProperty)
#endif
#ifdef GLU_VERSION_1_2
Xen_wrap_3_args(gxg_gluTessVertex_w, gxg_gluTessVertex)
#endif
Xen_wrap_9_optional_args(gxg_gluUnProject_w, gxg_gluUnProject)
Xen_wrap_any_args(gxg_gluUnProject4_w, gxg_gluUnProject4)
#endif
static void define_functions(void)
{
  #define GL_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) Xen_define_procedure(XL_PRE #Name XL_POST, Value, A1, A2, A3, Help)
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
  GL_DEFINE_PROCEDURE(gluBeginCurve, gxg_gluBeginCurve_w, 1, 0, 0, H_gluBeginCurve);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluBeginPolygon, gxg_gluBeginPolygon_w, 1, 0, 0, H_gluBeginPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluBeginSurface, gxg_gluBeginSurface_w, 1, 0, 0, H_gluBeginSurface);
  GL_DEFINE_PROCEDURE(gluBeginTrim, gxg_gluBeginTrim_w, 1, 0, 0, H_gluBeginTrim);
  GL_DEFINE_PROCEDURE(gluBuild1DMipmapLevels, gxg_gluBuild1DMipmapLevels_w, 9, 0, 0, H_gluBuild1DMipmapLevels);
  GL_DEFINE_PROCEDURE(gluBuild1DMipmaps, gxg_gluBuild1DMipmaps_w, 6, 0, 0, H_gluBuild1DMipmaps);
  GL_DEFINE_PROCEDURE(gluBuild2DMipmapLevels, gxg_gluBuild2DMipmapLevels_w, 0, 0, 1, H_gluBuild2DMipmapLevels);
  GL_DEFINE_PROCEDURE(gluBuild2DMipmaps, gxg_gluBuild2DMipmaps_w, 7, 0, 0, H_gluBuild2DMipmaps);
  GL_DEFINE_PROCEDURE(gluBuild3DMipmapLevels, gxg_gluBuild3DMipmapLevels_w, 0, 0, 1, H_gluBuild3DMipmapLevels);
  GL_DEFINE_PROCEDURE(gluBuild3DMipmaps, gxg_gluBuild3DMipmaps_w, 8, 0, 0, H_gluBuild3DMipmaps);
  GL_DEFINE_PROCEDURE(gluCheckExtension, gxg_gluCheckExtension_w, 2, 0, 0, H_gluCheckExtension);
  GL_DEFINE_PROCEDURE(gluCylinder, gxg_gluCylinder_w, 6, 0, 0, H_gluCylinder);
  GL_DEFINE_PROCEDURE(gluDeleteNurbsRenderer, gxg_gluDeleteNurbsRenderer_w, 1, 0, 0, H_gluDeleteNurbsRenderer);
  GL_DEFINE_PROCEDURE(gluDeleteQuadric, gxg_gluDeleteQuadric_w, 1, 0, 0, H_gluDeleteQuadric);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluDeleteTess, gxg_gluDeleteTess_w, 1, 0, 0, H_gluDeleteTess);
#endif
  GL_DEFINE_PROCEDURE(gluDisk, gxg_gluDisk_w, 5, 0, 0, H_gluDisk);
  GL_DEFINE_PROCEDURE(gluEndCurve, gxg_gluEndCurve_w, 1, 0, 0, H_gluEndCurve);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluEndPolygon, gxg_gluEndPolygon_w, 1, 0, 0, H_gluEndPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluEndSurface, gxg_gluEndSurface_w, 1, 0, 0, H_gluEndSurface);
  GL_DEFINE_PROCEDURE(gluEndTrim, gxg_gluEndTrim_w, 1, 0, 0, H_gluEndTrim);
  GL_DEFINE_PROCEDURE(gluErrorString, gxg_gluErrorString_w, 1, 0, 0, H_gluErrorString);
  GL_DEFINE_PROCEDURE(gluGetNurbsProperty, gxg_gluGetNurbsProperty_w, 3, 0, 0, H_gluGetNurbsProperty);
  GL_DEFINE_PROCEDURE(gluGetString, gxg_gluGetString_w, 1, 0, 0, H_gluGetString);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluGetTessProperty, gxg_gluGetTessProperty_w, 3, 0, 0, H_gluGetTessProperty);
#endif
  GL_DEFINE_PROCEDURE(gluLoadSamplingMatrices, gxg_gluLoadSamplingMatrices_w, 4, 0, 0, H_gluLoadSamplingMatrices);
  GL_DEFINE_PROCEDURE(gluLookAt, gxg_gluLookAt_w, 9, 0, 0, H_gluLookAt);
  GL_DEFINE_PROCEDURE(gluNewNurbsRenderer, gxg_gluNewNurbsRenderer_w, 0, 0, 0, H_gluNewNurbsRenderer);
  GL_DEFINE_PROCEDURE(gluNewQuadric, gxg_gluNewQuadric_w, 0, 0, 0, H_gluNewQuadric);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNewTess, gxg_gluNewTess_w, 0, 0, 0, H_gluNewTess);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNextContour, gxg_gluNextContour_w, 2, 0, 0, H_gluNextContour);
#endif
  GL_DEFINE_PROCEDURE(gluNurbsCallback, gxg_gluNurbsCallback_w, 3, 0, 0, H_gluNurbsCallback);
  GL_DEFINE_PROCEDURE(gluNurbsCallbackData, gxg_gluNurbsCallbackData_w, 2, 0, 0, H_gluNurbsCallbackData);
  GL_DEFINE_PROCEDURE(gluNurbsCallbackDataEXT, gxg_gluNurbsCallbackDataEXT_w, 2, 0, 0, H_gluNurbsCallbackDataEXT);
  GL_DEFINE_PROCEDURE(gluNurbsCurve, gxg_gluNurbsCurve_w, 7, 0, 0, H_gluNurbsCurve);
  GL_DEFINE_PROCEDURE(gluNurbsProperty, gxg_gluNurbsProperty_w, 3, 0, 0, H_gluNurbsProperty);
  GL_DEFINE_PROCEDURE(gluNurbsSurface, gxg_gluNurbsSurface_w, 0, 0, 1, H_gluNurbsSurface);
  GL_DEFINE_PROCEDURE(gluOrtho2D, gxg_gluOrtho2D_w, 4, 0, 0, H_gluOrtho2D);
  GL_DEFINE_PROCEDURE(gluPartialDisk, gxg_gluPartialDisk_w, 7, 0, 0, H_gluPartialDisk);
  GL_DEFINE_PROCEDURE(gluPerspective, gxg_gluPerspective_w, 4, 0, 0, H_gluPerspective);
  GL_DEFINE_PROCEDURE(gluPickMatrix, gxg_gluPickMatrix_w, 5, 0, 0, H_gluPickMatrix);
  GL_DEFINE_PROCEDURE(gluProject, gxg_gluProject_w, 6, 3, 0, H_gluProject);
  GL_DEFINE_PROCEDURE(gluPwlCurve, gxg_gluPwlCurve_w, 5, 0, 0, H_gluPwlCurve);
  GL_DEFINE_PROCEDURE(gluQuadricCallback, gxg_gluQuadricCallback_w, 3, 0, 0, H_gluQuadricCallback);
  GL_DEFINE_PROCEDURE(gluQuadricDrawStyle, gxg_gluQuadricDrawStyle_w, 2, 0, 0, H_gluQuadricDrawStyle);
  GL_DEFINE_PROCEDURE(gluQuadricNormals, gxg_gluQuadricNormals_w, 2, 0, 0, H_gluQuadricNormals);
  GL_DEFINE_PROCEDURE(gluQuadricOrientation, gxg_gluQuadricOrientation_w, 2, 0, 0, H_gluQuadricOrientation);
  GL_DEFINE_PROCEDURE(gluQuadricTexture, gxg_gluQuadricTexture_w, 2, 0, 0, H_gluQuadricTexture);
  GL_DEFINE_PROCEDURE(gluScaleImage, gxg_gluScaleImage_w, 9, 0, 0, H_gluScaleImage);
  GL_DEFINE_PROCEDURE(gluSphere, gxg_gluSphere_w, 4, 0, 0, H_gluSphere);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginContour, gxg_gluTessBeginContour_w, 1, 0, 0, H_gluTessBeginContour);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginPolygon, gxg_gluTessBeginPolygon_w, 2, 0, 0, H_gluTessBeginPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluTessCallback, gxg_gluTessCallback_w, 3, 0, 0, H_gluTessCallback);
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
  GL_DEFINE_PROCEDURE(gluUnProject, gxg_gluUnProject_w, 6, 3, 0, H_gluUnProject);
  GL_DEFINE_PROCEDURE(gluUnProject4, gxg_gluUnProject4_w, 0, 0, 1, H_gluUnProject4);
#endif
}

/* ---------------------------------------- constants ---------------------------------------- */

static void define_integers(void)
{

#if HAVE_SCHEME
#define DEFINE_INTEGER(Name) s7_define_constant(s7, XL_PRE #Name XL_POST, C_int_to_Xen_integer(Name))
#else
#define DEFINE_INTEGER(Name) Xen_define(XL_PRE #Name XL_POST, C_int_to_Xen_integer(Name))
#endif

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
  DEFINE_INTEGER(GLU_INVALID_OPERATION);
  DEFINE_INTEGER(GLU_OUTLINE_POLYGON);
  DEFINE_INTEGER(GLU_OUTLINE_PATCH);
  DEFINE_INTEGER(GLU_NURBS_ERROR);
  DEFINE_INTEGER(GLU_ERROR);
  DEFINE_INTEGER(GLU_NURBS_BEGIN);
  DEFINE_INTEGER(GLU_NURBS_BEGIN_EXT);
  DEFINE_INTEGER(GLU_NURBS_VERTEX);
  DEFINE_INTEGER(GLU_NURBS_VERTEX_EXT);
  DEFINE_INTEGER(GLU_NURBS_NORMAL);
  DEFINE_INTEGER(GLU_NURBS_NORMAL_EXT);
  DEFINE_INTEGER(GLU_NURBS_COLOR);
  DEFINE_INTEGER(GLU_NURBS_COLOR_EXT);
  DEFINE_INTEGER(GLU_NURBS_TEXTURE_COORD);
  DEFINE_INTEGER(GLU_NURBS_TEX_COORD_EXT);
  DEFINE_INTEGER(GLU_NURBS_END);
  DEFINE_INTEGER(GLU_NURBS_END_EXT);
  DEFINE_INTEGER(GLU_NURBS_BEGIN_DATA);
  DEFINE_INTEGER(GLU_NURBS_BEGIN_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_VERTEX_DATA);
  DEFINE_INTEGER(GLU_NURBS_VERTEX_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_NORMAL_DATA);
  DEFINE_INTEGER(GLU_NURBS_NORMAL_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_COLOR_DATA);
  DEFINE_INTEGER(GLU_NURBS_COLOR_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_TEXTURE_COORD_DATA);
  DEFINE_INTEGER(GLU_NURBS_TEX_COORD_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_END_DATA);
  DEFINE_INTEGER(GLU_NURBS_END_DATA_EXT);
  DEFINE_INTEGER(GLU_NURBS_ERROR1);
  DEFINE_INTEGER(GLU_NURBS_ERROR2);
  DEFINE_INTEGER(GLU_NURBS_ERROR3);
  DEFINE_INTEGER(GLU_NURBS_ERROR4);
  DEFINE_INTEGER(GLU_NURBS_ERROR5);
  DEFINE_INTEGER(GLU_NURBS_ERROR6);
  DEFINE_INTEGER(GLU_NURBS_ERROR7);
  DEFINE_INTEGER(GLU_NURBS_ERROR8);
  DEFINE_INTEGER(GLU_NURBS_ERROR9);
  DEFINE_INTEGER(GLU_NURBS_ERROR10);
  DEFINE_INTEGER(GLU_NURBS_ERROR11);
  DEFINE_INTEGER(GLU_NURBS_ERROR12);
  DEFINE_INTEGER(GLU_NURBS_ERROR13);
  DEFINE_INTEGER(GLU_NURBS_ERROR14);
  DEFINE_INTEGER(GLU_NURBS_ERROR15);
  DEFINE_INTEGER(GLU_NURBS_ERROR16);
  DEFINE_INTEGER(GLU_NURBS_ERROR17);
  DEFINE_INTEGER(GLU_NURBS_ERROR18);
  DEFINE_INTEGER(GLU_NURBS_ERROR19);
  DEFINE_INTEGER(GLU_NURBS_ERROR20);
  DEFINE_INTEGER(GLU_NURBS_ERROR21);
  DEFINE_INTEGER(GLU_NURBS_ERROR22);
  DEFINE_INTEGER(GLU_NURBS_ERROR23);
  DEFINE_INTEGER(GLU_NURBS_ERROR24);
  DEFINE_INTEGER(GLU_NURBS_ERROR25);
  DEFINE_INTEGER(GLU_NURBS_ERROR26);
  DEFINE_INTEGER(GLU_NURBS_ERROR27);
  DEFINE_INTEGER(GLU_NURBS_ERROR28);
  DEFINE_INTEGER(GLU_NURBS_ERROR29);
  DEFINE_INTEGER(GLU_NURBS_ERROR30);
  DEFINE_INTEGER(GLU_NURBS_ERROR31);
  DEFINE_INTEGER(GLU_NURBS_ERROR32);
  DEFINE_INTEGER(GLU_NURBS_ERROR33);
  DEFINE_INTEGER(GLU_NURBS_ERROR34);
  DEFINE_INTEGER(GLU_NURBS_ERROR35);
  DEFINE_INTEGER(GLU_NURBS_ERROR36);
  DEFINE_INTEGER(GLU_NURBS_ERROR37);
  DEFINE_INTEGER(GLU_AUTO_LOAD_MATRIX);
  DEFINE_INTEGER(GLU_CULLING);
  DEFINE_INTEGER(GLU_SAMPLING_TOLERANCE);
  DEFINE_INTEGER(GLU_DISPLAY_MODE);
  DEFINE_INTEGER(GLU_PARAMETRIC_TOLERANCE);
  DEFINE_INTEGER(GLU_SAMPLING_METHOD);
  DEFINE_INTEGER(GLU_U_STEP);
  DEFINE_INTEGER(GLU_V_STEP);
  DEFINE_INTEGER(GLU_NURBS_MODE);
  DEFINE_INTEGER(GLU_NURBS_MODE_EXT);
  DEFINE_INTEGER(GLU_NURBS_TESSELLATOR);
  DEFINE_INTEGER(GLU_NURBS_TESSELLATOR_EXT);
  DEFINE_INTEGER(GLU_NURBS_RENDERER);
  DEFINE_INTEGER(GLU_NURBS_RENDERER_EXT);
  DEFINE_INTEGER(GLU_OBJECT_PARAMETRIC_ERROR);
  DEFINE_INTEGER(GLU_OBJECT_PARAMETRIC_ERROR_EXT);
  DEFINE_INTEGER(GLU_OBJECT_PATH_LENGTH);
  DEFINE_INTEGER(GLU_OBJECT_PATH_LENGTH_EXT);
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
  DEFINE_INTEGER(GLU_TESS_ERROR1);
  DEFINE_INTEGER(GLU_TESS_ERROR2);
  DEFINE_INTEGER(GLU_TESS_ERROR3);
  DEFINE_INTEGER(GLU_TESS_ERROR4);
  DEFINE_INTEGER(GLU_TESS_ERROR5);
  DEFINE_INTEGER(GLU_TESS_ERROR6);
  DEFINE_INTEGER(GLU_TESS_ERROR7);
  DEFINE_INTEGER(GLU_TESS_ERROR8);
  DEFINE_INTEGER(GLU_TESS_MISSING_BEGIN_POLYGON);
  DEFINE_INTEGER(GLU_TESS_MISSING_BEGIN_CONTOUR);
  DEFINE_INTEGER(GLU_TESS_MISSING_END_POLYGON);
  DEFINE_INTEGER(GLU_TESS_MISSING_END_CONTOUR);
  DEFINE_INTEGER(GLU_TESS_COORD_TOO_LARGE);
  DEFINE_INTEGER(GLU_TESS_NEED_COMBINE_CALLBACK);
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
      Xen_provide_feature("gl");
      Xen_define("gl-version", C_string_to_Xen_string("21-Feb-14"));
      gl_already_inited = true;
    }
}
#else
 void Init_libgl(void);
 void Init_libgl(void)
{
}
#endif
