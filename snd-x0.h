#ifndef SND_X0_H
#define SND_X0_H

#include <config.h>
#include <Xm/XmAll.h>
#include <X11/keysym.h>

#if HAVE_GL
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#endif

#if (XmVERSION >= 2)
  #define USE_RENDITIONS 1
#endif

#if (USE_RENDITIONS)
  #define xm_font_t XmRenderTable
  #define XM_FONT_RESOURCE XmNrenderTable
  #define XM_FONT_FREE XmRenderTableFree
#else
  #define xm_font_t XmFontList
  #define XM_FONT_RESOURCE XmNfontList
  #define XM_FONT_FREE XmFontListFree
#endif

#ifdef SGI
  #define CLOSED_CTRLS_HEIGHT 22
#else
  #define CLOSED_CTRLS_HEIGHT 19
#endif

#define LOTSA_PIXELS 10000
#define XEN_WRAP_WIDGET(Value)       ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Widget"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)       ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Window"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_GC(Value)           XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GC"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_WRAP_APPCONTEXT(Value)   XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("XtAppContext"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_WRAP_PIXEL(Value)        XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Pixel"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_UNWRAP_WIDGET(Value)     (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_GC(Value)         XEN_TO_C_ULONG(XEN_CADR(Value))
#define XEN_UNWRAP_PIXEL(Value)      XEN_TO_C_ULONG(XEN_CADR(Value))
#define XEN_WIDGET_P(Value)          (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                      (strcmp("Widget", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#define XEN_PIXEL_P(Value)           (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                      (strcmp("Pixel", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define POINT_BUFFER_SIZE 4096

typedef enum {NOT_ACTIVATABLE, ACTIVATABLE} text_cr_t;

#ifdef SGI
  #define LINE_MARGIN 10
  #define CONTROLS_MARGIN 0
#else
  #define LINE_MARGIN 4
  #define CONTROLS_MARGIN 1
#endif

#define SCROLLBAR_MAX 1000

#define BACKGROUND_QUIT 1
#define BACKGROUND_CONTINUE 0
#define BACKGROUND_REMOVE(func) XtRemoveWorkProc(func)
/* #define BACKGROUND_ADD(func, data) XtAppAddWorkProc(MAIN_APP(ss), func, (XtPointer)data) */
#define BACKGROUND_ADD(func, data) add_work_proc(func, (XtPointer)data)

#if DEBUGGING
  #define ASSERT_WIDGET_TYPE(Cond, Wid) if (!(Cond)) fprintf(stderr, "%s:[%s %d] %s is wrong type", c__FUNCTION__, __FILE__, __LINE__, XtName(Wid))
#else
  #define ASSERT_WIDGET_TYPE(Cond, Wid)
#endif

#define widget_t Widget

#define Cessator XtWorkProcId
#define Cessate Boolean
#define Indicium XtPointer
#define Tempus Time
#define Locus short
#define Latus unsigned short
/* Position/Dimension in X terms */

typedef struct {
  GC gc;
  Display *dp;
  Drawable wn;
  Font current_font;
} axis_context;

typedef struct {
  /* we need two versions of each GC because the selected channel's colors can be different from the unselected channels' */
  Widget *chan_widgets;
  Cessator fft_in_progress;
  Cessator amp_env_in_progress;
  void *amp_env_state;
  axis_context *ax;
  bool selected;
} chan_context;

typedef struct {
  Pixmap file_pix;
  Widget *snd_widgets;
  Widget tab;
  void *flt;
  Widget dialog;
} snd_context;

typedef struct {
  Widget header_list, format_list, srate_text, chans_text, comment_text, location_text, samples_text;
  int current_type, current_format, formats, header_pos, format_pos;
} file_data;

typedef struct {
  XtAppContext mainapp;     
  Widget mainshell;
  Widget mainpane;
  Widget soundpane;
  Widget soundpanebox;
  Display *mdpy;
  xm_font_t peaks_fontlist;
  XFontStruct *peaks_fontstruct;
  xm_font_t bold_peaks_fontlist;
  XFontStruct *bold_peaks_fontstruct; 
  xm_font_t listener_fontlist;
  XFontStruct *listener_fontstruct;
  XFontStruct *axis_label_fontstruct;
  XFontStruct *axis_numbers_fontstruct;
  xm_font_t tiny_fontlist;
  XFontStruct *tiny_fontstruct;

  Pixel white, black, red, yellow, green, light_blue, lighter_blue;
  Pixel data_color, selected_data_color, mark_color, graph_color, selected_graph_color, listener_color, listener_text_color, cursor_color;
  Pixel basic_color, selection_color, zoom_color, position_color, highlight_color, enved_waveform_color;
  Pixel text_focus_color, filter_control_waveform_color, mix_color, pushed_button_color, sash_color;
  Pixel help_button_color, doit_again_button_color, doit_button_color, quit_button_color, reset_button_color;
  Pixel selected_grid_color, grid_color;

  GC basic_gc, selected_basic_gc, combined_basic_gc;        
  GC cursor_gc, selected_cursor_gc;      
  GC selection_gc, selected_selection_gc;
  GC erase_gc, selected_erase_gc;        
  GC mark_gc, selected_mark_gc;          
  GC mix_gc;
  GC fltenv_basic_gc, fltenv_data_gc;
  Widget listener_pane;
  Widget *dialogs;
  Cursor graph_cursor, wait_cursor;
  Widget completion_requestor;
#if HAVE_GL
  GLXContext cx;
#endif
} state_context;

typedef struct {
  Widget graph;
  XPoint *p0, *p1;
  int lastpj;
  Pixel color;
} mix_context;

typedef enum {WITHOUT_PANED_WINDOW, WITH_PANED_WINDOW} dialog_paned_t;
typedef enum {DONT_PAD_TITLE, PAD_TITLE_ON_RIGHT, PAD_TITLE_ON_LEFT} dialog_pad_t;
typedef enum {WITHOUT_SORT_BUTTON, WITH_SORT_BUTTON} dialog_sort_t;

typedef struct {
  Widget ww;
  Widget list;
  Widget plw;
  Widget bydate;
  Widget bysize;
  Widget byname;
  Widget byentry;
  Widget byproc;
  Widget panes, toppane;
} ww_info;

#define snd_ShiftMask ShiftMask
#define snd_ControlMask ControlMask
#ifndef SUN
  #define snd_MetaMask Mod1Mask
#else
  #define snd_MetaMask (Mod1Mask | Mod4Mask)
#endif

#define MAIN_SHELL(a) (a->sgx)->mainshell
#define MAIN_PANE(a) (a->sgx)->mainpane
#define SOUND_PANE(a) (a->sgx)->soundpane
#define SOUND_PANE_BOX(a) (a->sgx)->soundpanebox
#define MAIN_APP(a) (a->sgx)->mainapp
#define MAIN_DISPLAY(a) (a->sgx)->mdpy
#define PEAK_NUMBERS_FONT(a) (a->sgx)->peaks_fontstruct
#define BOLD_PEAK_NUMBERS_FONT(a) (a->sgx)->bold_peaks_fontstruct
#define AXIS_NUMBERS_FONT(a) (a->sgx)->axis_numbers_fontstruct
#define AXIS_LABEL_FONT(a) (a->sgx)->axis_label_fontstruct
#define TINY_NUMBERS_FONT(a) (a->sgx)->tiny_fontstruct
#define color_t Pixel
/* this was unsigned long = Pixel (/usr/X11R6/include/X11/Intrinsic.h) */

#define KEY_TO_NAME(key) XKeysymToString(key)
/* on the Sun, if key is 0, XKeysymToString segfaults! */

/* #define GUI_CURRENT_TIME(ss) XtLastTimestampProcessed(MAIN_DISPLAY(ss)) */


/* now pull in the key names (/usr/include/X11/keysymdef.h) */
#define snd_K_Shift_L XK_Shift_L	 
#define snd_K_space XK_space 
#define snd_K_openparen XK_parenleft 
#define snd_K_closeparen XK_parenright 
#define snd_K_plus XK_plus 
#define snd_K_minus XK_minus 
#define snd_K_period XK_period 
#define snd_K_slash XK_slash 
#define snd_K_0 XK_0 
#define snd_K_1 XK_1 
#define snd_K_2 XK_2 
#define snd_K_3 XK_3 
#define snd_K_4 XK_4 
#define snd_K_5 XK_5 
#define snd_K_6 XK_6 
#define snd_K_7 XK_7 
#define snd_K_8 XK_8 
#define snd_K_9 XK_9 
#define snd_K_less XK_less 
#define snd_K_greater XK_greater 
#define snd_K_A XK_A 
#define snd_K_B XK_B 
#define snd_K_C XK_C 
#define snd_K_D XK_D 
#define snd_K_E XK_E 
#define snd_K_F XK_F 
#define snd_K_G XK_G 
#define snd_K_H XK_H 
#define snd_K_I XK_I 
#define snd_K_J XK_J 
#define snd_K_K XK_K 
#define snd_K_L XK_L 
#define snd_K_M XK_M 
#define snd_K_N XK_N 
#define snd_K_O XK_O 
#define snd_K_P XK_P 
#define snd_K_Q XK_Q 
#define snd_K_R XK_R 
#define snd_K_S XK_S 
#define snd_K_T XK_T 
#define snd_K_U XK_U 
#define snd_K_V XK_V 
#define snd_K_W XK_W 
#define snd_K_X XK_X 
#define snd_K_Y XK_Y 
#define snd_K_Z XK_Z 
#define snd_K_underscore XK_underscore 
#define snd_K_a XK_a 
#define snd_K_b XK_b 
#define snd_K_c XK_c 
#define snd_K_d XK_d 
#define snd_K_e XK_e 
#define snd_K_f XK_f 
#define snd_K_g XK_g 
#define snd_K_h XK_h 
#define snd_K_i XK_i 
#define snd_K_j XK_j 
#define snd_K_k XK_k 
#define snd_K_l XK_l 
#define snd_K_m XK_m 
#define snd_K_n XK_n 
#define snd_K_o XK_o 
#define snd_K_p XK_p 
#define snd_K_q XK_q 
#define snd_K_r XK_r 
#define snd_K_s XK_s 
#define snd_K_t XK_t 
#define snd_K_u XK_u 
#define snd_K_v XK_v 
#define snd_K_w XK_w 
#define snd_K_x XK_x 
#define snd_K_y XK_y 
#define snd_K_z XK_z 
#define snd_K_Home XK_Home		 
#define snd_K_Left XK_Left		 
#define snd_K_Up XK_Up		 
#define snd_K_Right XK_Right	 
#define snd_K_Down XK_Down		 

#define snd_keypad_PageUp XK_KP_Page_Up
#define snd_keypad_PageDown XK_KP_Page_Down
#define snd_keypad_Insert XK_KP_Insert
#define snd_keypad_Delete XK_KP_Delete
#define snd_keypad_Left XK_KP_Left
#define snd_keypad_Up XK_KP_Up
#define snd_keypad_Right XK_KP_Right
#define snd_keypad_Down XK_KP_Down
#define snd_keypad_Multiply XK_KP_Multiply
#define snd_keypad_Add XK_KP_Add
#define snd_keypad_Subtract XK_KP_Subtract
#define snd_keypad_Divide XK_KP_Divide
#define snd_keypad_Decimal XK_KP_Decimal
#define snd_keypad_Enter XK_KP_Enter

#define snd_keypad_0 XK_KP_0
#define snd_keypad_1 XK_KP_1
#define snd_keypad_2 XK_KP_2
#define snd_keypad_3 XK_KP_3
#define snd_keypad_4 XK_KP_4
#define snd_keypad_5 XK_KP_5
#define snd_keypad_6 XK_KP_6
#define snd_keypad_7 XK_KP_7
#define snd_keypad_8 XK_KP_8
#define snd_keypad_9 XK_KP_9

#endif
