/* X-specific type/macro declarations */

#ifndef SND_X0_H_LOADED
#define SND_X0_H_LOADED

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <Xm/XmAll.h>
#include <X11/keysym.h>

#if (USE_RENDITIONS)
  #define XM_FONT_TYPE XmRenderTable
  #define XM_FONT_RESOURCE XmNrenderTable
  #define XM_FONT_FREE XmRenderTableFree
#else
  #define XM_FONT_TYPE XmFontList
  #define XM_FONT_RESOURCE XmNfontList
  #define XM_FONT_FREE XmFontListFree
#endif

#ifdef XmNtoolTipString
  #define MOTIF_2_2 1
#endif

#if (XmVERSION >= 2)
  #define OVERRIDE_TOGGLE 1
  /* Motif 2.0 defines control-button1 to be "take focus" -- this is not a good idea!! */
#else
  #define OVERRIDE_TOGGLE 0
#endif

#if (!(defined(HAVE_XPM)))
  #ifdef LINUX
    #define HAVE_XPM 1
  #else
    #define HAVE_XPM 0
  #endif
#endif

#ifdef SGI
  #define CLOSED_CTRLS_HEIGHT 22
#else
  #define CLOSED_CTRLS_HEIGHT 18
#endif

#define LOTSA_PIXELS 10000
#define XEN_WRAP_WIDGET(Value)       ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Widget"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)       ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Window"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_GC(Value)           XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GC"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_WRAP_APPCONTEXT(Value)   XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("XtAppContext"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_WRAP_PIXEL(Value)        XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Pixel"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_UNWRAP_WIDGET(Value)     (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_WINDOW(Value)     (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_GC(Value)         XEN_TO_C_ULONG(XEN_CADR(Value))
#define XEN_UNWRAP_PIXEL(Value)      XEN_TO_C_ULONG(XEN_CADR(Value))
#define XEN_UNWRAP_APPCONTEXT(Value) XEN_TO_C_ULONG(XEN_CADR(Value))
#define XEN_WIDGET_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                            (strcmp("Widget", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define POINT_BUFFER_SIZE 4096

#ifdef WITH_BIG_COLORMAP
  #define COLORMAP_SIZE 512
#else
  #define COLORMAP_SIZE 64
#endif

#define ACTIVATABLE 1
#define NOT_ACTIVATABLE 0

#ifdef SGI
  #define LINE_MARGIN 10
  #define CONTROLS_MARGIN 0
  #define NEED_TOGGLE_MARGIN 0
#else
  #define LINE_MARGIN 4
  #define CONTROLS_MARGIN 1
  #define TOGGLE_MARGIN 0
  #define NEED_TOGGLE_MARGIN 1
#endif

#define SCROLLBAR_MAX 1000
#define SCROLLBAR_SX_MAX 2000
#define SCROLLBAR_MID 500
#define SCROLLBAR_LINEAR_MAX 150
#define SCROLLBAR_LINEAR_MULT 0.0011584929

#define BACKGROUND_TYPE Boolean
#define BACKGROUND_QUIT TRUE
#define BACKGROUND_CONTINUE FALSE
#define BACKGROUND_FUNCTION_TYPE XtWorkProcId
#define BACKGROUND_REMOVE(func) XtRemoveWorkProc(func)
/* #define BACKGROUND_ADD(ss, func, data) XtAppAddWorkProc(MAIN_APP(ss), func, (XtPointer)data) */
#define BACKGROUND_ADD(ss, func, data) add_work_proc(ss, func, (XtPointer)data)

#define GUI_POINTER XtPointer
#define GUI_WIDGET Widget

#define TIME_TYPE Time
#define BUTTON_1 Button1
#define BUTTON_2 Button2
#define BUTTON_3 Button3

#define NUM_GLASSES 15
#define NUM_BOMBS 15
#define NUM_COLORMAPS 16

#if DEBUGGING
  #define ASSERT_WIDGET_TYPE(Cond, Wid) if (!(Cond)) fprintf(stderr,"%s:[%s %d] %s is wrong type", __FUNCTION__, __FILE__, __LINE__, XtName(Wid))
#else
  #define ASSERT_WIDGET_TYPE(Cond, Wid)
#endif

#define Locus short
#define Latus unsigned short
/* Position/Dimension in X terms */

typedef struct {
  GC gc;
  Display *dp;
  Drawable wn;
  void *ss;
  Font current_font;
} axis_context;

typedef struct {
  /* we need two versions of each GC because the selected channel's colors can be different from the unselected channels' */
  Widget *chan_widgets;
  BACKGROUND_FUNCTION_TYPE fft_in_progress;
  BACKGROUND_FUNCTION_TYPE amp_env_in_progress;
  void *amp_env_state;
  axis_context *ax;
  int selected;
} chan_context;

typedef struct {
  Pixmap speed_r, speed_l, file_pix;
  BACKGROUND_FUNCTION_TYPE apply_in_progress;
  Widget *snd_widgets;
  Widget tab;
  void *flt;
  Widget dialog;
} snd_context;

typedef struct {
  Widget header_list, format_list, srate_text, chans_text, comment_text, location_text;
  int current_type, current_format, formats, header_pos, format_pos;
} file_data;

typedef struct {
  XtAppContext mainapp;     
  Widget mainshell;
  Widget mainpane;
  Widget soundpane;
  Widget soundpanebox;
  Display *mdpy;
  XM_FONT_TYPE button_fontlist;
  XFontStruct *button_fontstruct;
  XM_FONT_TYPE bold_button_fontlist;
  XFontStruct *bold_button_fontstruct; 
  XM_FONT_TYPE listener_fontlist;
  XFontStruct *listener_fontstruct;
  XFontStruct *axis_label_fontstruct;
  XFontStruct *axis_numbers_fontstruct;
  XM_FONT_TYPE help_text_fontlist;
  XFontStruct *help_text_fontstruct;
  XM_FONT_TYPE tiny_fontlist;
  XFontStruct *tiny_fontstruct;

  Pixel white, black, red, yellow, green, light_blue, lighter_blue;
  Pixel data_color, selected_data_color, mark_color, graph_color, selected_graph_color, listener_color, listener_text_color, cursor_color;
  Pixel basic_color, selection_color, zoom_color, position_color, highlight_color, enved_waveform_color;
  Pixel selected_mix_color, text_focus_color, filter_waveform_color, mix_color, pushed_button_color, sash_color;

  GC basic_gc, selected_basic_gc, combined_basic_gc;        
  GC cursor_gc, selected_cursor_gc;      
  GC selection_gc, selected_selection_gc;
  GC erase_gc, selected_erase_gc;        
  GC mark_gc, selected_mark_gc;          
  GC mix_gc, selected_mix_gc;         
  GC fltenv_basic_gc, fltenv_data_gc, speed_gc;

  XEvent *text_activate_event;
  Widget text_widget, listener_pane;
  Widget *dialogs;

  Cursor mix_cursor, graph_cursor, wait_cursor;
} state_context;

typedef struct {
  Widget graph;
  XPoint *p0, *p1;
  int lastpj;
  Pixel color;
} mix_context;

typedef struct {
  Pixmap off_label;
  Pixmap on_label;
  Pixmap clip_label;
  XFontStruct *label_font;
  Float size;
} vu_label;

#define WITHOUT_PANED_WINDOW 0
#define WITH_PANED_WINDOW 1
#define DONT_PAD_TITLE 0
#define PAD_TITLE_ON_RIGHT 1
#define PAD_TITLE_ON_LEFT 2
#define WITHOUT_SORT_BUTTON 0
#define WITH_SORT_BUTTON 1

typedef struct {
  Widget ww;
  Widget list;
  Widget plw;
  Widget svw;
  Widget dbline;
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
#define BUTTON_FONT(a) (a->sgx)->button_fontlist
#define BOLD_BUTTON_FONT(a) (a->sgx)->bold_button_fontlist
#define PEAK_NUMBERS_FONT(a) (a->sgx)->button_fontstruct
#define BOLD_PEAK_NUMBERS_FONT(a) (a->sgx)->bold_button_fontstruct
#define AXIS_NUMBERS_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_numbers_fontstruct
#define AXIS_LABEL_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_label_fontstruct
#define TINY_NUMBERS_FONT(a) (a->sgx)->tiny_fontstruct
#define HELP_TEXT_FONT(a) (a->sgx)->help_text_fontlist
#define COLOR_TYPE Pixel
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
#if !defined(UW2)
#define snd_keypad_Left XK_KP_Left
#define snd_keypad_Up XK_KP_Up
#define snd_keypad_Right XK_KP_Right
#define snd_keypad_Down XK_KP_Down
#endif
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
