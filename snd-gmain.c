#include "snd.h"

#define FALLBACK_FONT "Monospace 14"
#define HIGHLIGHT_COLOR      "ivory1"
#define BASIC_COLOR          "ivory2"
#define POSITION_COLOR       "ivory3"
#define ZOOM_COLOR           "ivory4"
#define CURSOR_COLOR         "red"
#define SELECTION_COLOR      "lightsteelblue1"
#define MIX_COLOR            "darkgray"
#define SELECTED_MIX_COLOR   "lightgreen"
#define ENVED_WAVEFORM_COLOR "blue"
#define GRAPH_COLOR          "white"
#define SELECTED_GRAPH_COLOR "white"
#define DATA_COLOR           "black"
#define SELECTED_DATA_COLOR  "black"
#define MARK_COLOR           "red"
#define LISTENER_COLOR       "AliceBlue"
#define LISTENER_TEXT_COLOR  "black"
#define LIGHT_BLUE_COLOR     "lightsteelblue1"
#define LIGHTER_BLUE_COLOR   "AliceBlue"
#define WHITE_COLOR          "white"
#define BLACK_COLOR          "black"
#define GREEN_COLOR          "green2"
#define RED_COLOR            "red"
#define YELLOW_COLOR         "yellow"
#define TEXT_FOCUS_COLOR     "white"
#define FILTER_WAVEFORM_COLOR "blue"
#define PUSHED_BUTTON_COLOR  "lightsteelblue1"
#define SASH_COLOR           "lightgreen"

#if HAVE_GL
  #define DEFAULT_SPECTROGRAM_COLOR DEFAULT_COLOR_MAP
#else
  #define DEFAULT_SPECTROGRAM_COLOR 0
#endif

#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 10
#define ENVED_POINT_SIZE 10
#define NOTEBOOK_BINDING_WIDTH 20

#define DEFAULT_TINY_FONT "Monospace 8"
#define DEFAULT_PEAKS_FONT "Serif 10"
#define DEFAULT_BOLD_PEAKS_FONT "Serif Bold 10"
#define DEFAULT_AXIS_NUMBERS_FONT "Monospace 10"
#define DEFAULT_AXIS_LABEL_FONT "Serif 14"

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#define TOGGLE_SIZE 15
#define CHANNEL_MIN_HEIGHT 150
#define SASH_SIZE 14
#define SASH_INDENT -6
#define AUTO_RESIZE_DEFAULT true

#ifndef SND_AS_WIDGET
static gint window_close(GtkWidget *w, GdkEvent *event, gpointer context)
{
  if (snd_exit_cleanly(true))
    snd_exit(0);
  return(false);
}
#endif

static gint auto_update_check(gpointer context)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if ((!(play_in_progress())) && 
	  (!(record_in_progress())))
	for_each_sound(sound_not_current, NULL);
      gtk_timeout_add((guint32)(auto_update_interval(ss) * 1000), auto_update_check, context);
    }
  return(0);
}

static GdkAtom snd_v, snd_c;

#if HAVE_EXTENSION_LANGUAGE
static XEN window_property_changed_hook;

static void who_called(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  /* watch for communication from some other program via the SND_COMMAND property */
  GdkEventProperty *ev = (GdkEventProperty *)event;
  GdkAtom type;
  gint format, nitems;
  guchar *version[1];
  if (ev->atom == snd_c)
    {
      if (gdk_property_get(MAIN_WINDOW(ss), snd_c, 
			   GDK_TARGET_STRING, 0L, (long)BUFSIZ, false,
			   &type, &format, &nitems, (guchar **)version))
	{
	  if (version[0])
	    {
	      if ((!(XEN_HOOKED(window_property_changed_hook))) ||
		  (!(XEN_TRUE_P(run_or_hook(window_property_changed_hook,
					    XEN_LIST_1(C_TO_XEN_STRING((char *)(version[0]))),
					    S_window_property_changed_hook)))))
		snd_eval_property_str((char *)(version[0]));
	      free(version[0]);
	    }
	}
    }
}
#endif

#if HAVE_SETJMP_H
#include <setjmp.h>

#if TRAP_SEGFAULT
/* stolen from scwm.c */
static sigjmp_buf envHandleEventsLoop;

static RETSIGTYPE segv(int ignored)
{
  siglongjmp(envHandleEventsLoop, 1);
}
#endif

static jmp_buf top_level_jump;
RETSIGTYPE top_level_catch(int ignore);
RETSIGTYPE top_level_catch(int ignore)
{
  longjmp(top_level_jump, 1);
}
#endif

static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static bool noglob = false, noinit = false, batch = false, nostdin = false;

#if HAVE_EXTENSION_LANGUAGE
static gint stdin_id = 0;

static void get_stdin_string(gpointer context, gint fd, int condition)
{
  int bytes, size;
  char *buf;
  buf = (char *)CALLOC(1024, sizeof(char));
  size = 1024;
  bytes = read(fd, buf, 1024);
  if (bytes <= 0) 
    {
      /* redirected to /dev/null?? */
      gtk_input_remove(stdin_id);
      stdin_id = 0;
    }
  else
    {
      while (bytes == 1024)
	{
	  size += 1024;
	  buf = (char *)REALLOC(buf, size);
	  bytes = read(fd, (char *)(buf + size - 1024), 1024);
	}
      snd_eval_stdin_str(buf);
    }
  FREE(buf);
}
#endif

static void setup_gcs (void)
{
  GdkWindow *wn;	
  state_context *sx;

  sx = ss->sgx;
  wn = MAIN_WINDOW(ss);

  sx->basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->basic_gc, sx->graph_color);
  gdk_gc_set_foreground(sx->basic_gc, sx->data_color);

  sx->combined_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->combined_basic_gc, sx->graph_color);
  gdk_gc_set_foreground(sx->combined_basic_gc, sx->data_color);

  sx->mix_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->mix_gc, sx->graph_color);
  gdk_gc_set_foreground(sx->mix_gc, sx->mix_color);
  gdk_gc_set_function(sx->mix_gc, GDK_COPY);

  sx->selected_mix_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_mix_gc, sx->selected_graph_color);
  gdk_gc_set_foreground(sx->selected_mix_gc, sx->selected_mix_color);
  gdk_gc_set_function(sx->selected_mix_gc, GDK_COPY);

  sx->cursor_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->cursor_gc, sx->graph_color);
  gc_set_foreground_xor(sx->cursor_gc, sx->cursor_color, sx->graph_color);
  gdk_gc_set_function(sx->cursor_gc, GDK_XOR);

  sx->selection_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selection_gc, sx->graph_color);
  gc_set_foreground_xor(sx->selection_gc, sx->selection_color, sx->graph_color);
  gdk_gc_set_function(sx->selection_gc, GDK_XOR);

  sx->mark_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->mark_gc, sx->graph_color);
  gc_set_foreground_xor(sx->mark_gc, sx->mark_color, sx->graph_color);
  gdk_gc_set_function(sx->mark_gc, GDK_XOR);

  sx->erase_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->erase_gc, sx->data_color);
  gdk_gc_set_foreground(sx->erase_gc, sx->graph_color);
  gdk_gc_set_function(sx->erase_gc, GDK_COPY);

  sx->selected_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_basic_gc, sx->selected_graph_color);
  gdk_gc_set_foreground(sx->selected_basic_gc, sx->selected_data_color);

  sx->selected_cursor_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_cursor_gc, sx->graph_color);
  gc_set_foreground_xor(sx->selected_cursor_gc, sx->cursor_color, sx->graph_color);
  gdk_gc_set_function(sx->selected_cursor_gc, GDK_XOR);

  sx->selected_selection_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_selection_gc, sx->graph_color);
  gc_set_foreground_xor(sx->selected_selection_gc, sx->selection_color, sx->graph_color);
  gdk_gc_set_function(sx->selected_selection_gc, GDK_XOR);

  sx->selected_mark_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_mark_gc, sx->selected_graph_color);
  gc_set_foreground_xor(sx->selected_mark_gc, sx->mark_color, sx->selected_graph_color);
  gdk_gc_set_function(sx->selected_mark_gc, GDK_XOR);

  sx->selected_erase_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_erase_gc, sx->selected_data_color);
  gdk_gc_set_foreground(sx->selected_erase_gc, sx->selected_graph_color);
  gdk_gc_set_function(sx->selected_erase_gc, GDK_COPY);

  sx->fltenv_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->fltenv_basic_gc, sx->basic_color);
  gdk_gc_set_foreground(sx->fltenv_basic_gc, sx->black);
  gdk_gc_set_function(sx->fltenv_basic_gc, GDK_COPY);

  sx->fltenv_data_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->fltenv_data_gc, sx->basic_color);
  gdk_gc_set_foreground(sx->fltenv_data_gc, sx->filter_waveform_color);
  gdk_gc_set_function(sx->fltenv_data_gc, GDK_COPY);

  initialize_colormap();
}

#if HAVE_EXTENSION_LANGUAGE
static gboolean io_invoke(GIOChannel *source, GIOCondition condition, gpointer data)
{
  get_stdin_string(NULL, g_io_channel_unix_get_fd(source), 0);
  return(true);
}
#endif

static int tm_slice = 0;

static Cessate startup_funcs(gpointer context)
{
  snd_info *sp;
  static int auto_open_ctr = 0;
  switch (tm_slice)
    {
    case 0:
#ifndef SND_AS_WIDGET
      /* add X property level communication path (see sndctrl.c for the other side) */
      snd_v = gdk_atom_intern("SND_VERSION", false);
      snd_c = gdk_atom_intern("SND_COMMAND", false);

      gdk_property_change(MAIN_WINDOW(ss), 
			  snd_v, 
			  GDK_TARGET_STRING, 8, 
			  GDK_PROP_MODE_REPLACE, 
			  (guchar *)(SND_VERSION), 
			  strlen(SND_VERSION) + 1);
#if HAVE_EXTENSION_LANGUAGE
      gtk_widget_add_events (MAIN_SHELL(ss), gtk_widget_get_events (MAIN_SHELL(ss)) | GDK_PROPERTY_CHANGE_MASK);
      g_signal_connect_closure_by_id(GTK_OBJECT(MAIN_SHELL(ss)),
				     g_signal_lookup("property_notify_event", G_OBJECT_TYPE(GTK_OBJECT(MAIN_SHELL(ss)))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(who_called), NULL, 0),
				     0);
#endif
      /* trap outer-level Close for cleanup check */
      g_signal_connect_closure_by_id(GTK_OBJECT(MAIN_SHELL(ss)),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(MAIN_SHELL(ss)))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(window_close), NULL, 0),
				     0);
#endif

      (ss->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(ss));
      (ss->sgx)->wait_cursor = gdk_cursor_new(GDK_WATCH);
      (ss->sgx)->arrow_cursor = gdk_cursor_new(GDK_LEFT_PTR);

      break;
    case 1: 
#if HAVE_EXTENSION_LANGUAGE
      snd_load_init_file(noglob, noinit);
#endif
#if HAVE_SIGNAL && HAVE_EXTENSION_LANGUAGE
      if (!nostdin)
	{
	  GIOChannel *channel;
	  signal(SIGTTIN, SIG_IGN);
	  signal(SIGTTOU, SIG_IGN);
	  /* these signals are sent by a shell if we start Snd as a background process,
	   * but try to read stdin (needed to support the emacs subjob connection).  If
	   * we don't do this, the background job is suspended when the shell sends SIGTTIN.
	   */
	  channel = g_io_channel_unix_new(fileno(stdin));
	  stdin_id = g_io_add_watch_full(channel, 
					 G_PRIORITY_DEFAULT, 
					 (GIOCondition)(G_IO_IN | G_IO_HUP | G_IO_ERR), 
					 io_invoke, NULL, NULL);
	  g_io_channel_unref(channel);
	}
#endif
      break;
    case 2: 
      if (auto_open_files > 0)
	{
	  auto_open_ctr = handle_next_startup_arg(auto_open_ctr, auto_open_file_names, true, auto_open_files);
	  if (auto_open_ctr < auto_open_files) return(BACKGROUND_CONTINUE); /* i.e. come back to this branch */
	}
      break;
    case 3:
#ifndef SND_AS_WIDGET
      if ((ss->init_window_width > 0) && (ss->init_window_height > 0))
	set_widget_size(GTK_WIDGET(MAIN_SHELL(ss)), ss->init_window_width, ss->init_window_height);
      if ((ss->init_window_x != DEFAULT_INIT_WINDOW_X) && (ss->init_window_y != DEFAULT_INIT_WINDOW_Y))
	set_widget_position(GTK_WIDGET(MAIN_SHELL(ss)), ss->init_window_x, ss->init_window_y);
#endif
      gtk_timeout_add((guint32)(auto_update_interval(ss) * 1000), auto_update_check, NULL);
      break;
    case 4: 
#if TRAP_SEGFAULT
      if (trap_segfault(ss)) signal(SIGSEGV, segv);
#endif
      if ((ss->sounds) &&
	  (ss->selected_sound == NO_SELECTION))
	{
	  sp = ss->sounds[0];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) &&
	      (sp->selected_channel == NO_SELECTION)) /* don't clobber possible select-channel in loaded startup files */
	    select_channel(sp, 0);
	}
      return(BACKGROUND_QUIT); 
      break;
    }
  tm_slice++;
  return(BACKGROUND_CONTINUE);
}

#ifndef SND_AS_WIDGET
static void SetupIcon(GtkWidget *shell)
{
  GdkPixmap *pix;
  GdkBitmap *mask;
  pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), &mask, NULL, snd_icon_bits());
  gdk_window_set_icon(MAIN_WINDOW(ss), NULL, pix, mask);
}
#endif

static GdkColor *get_color(char *defined_color, char *fallback_color, char *second_fallback_color, bool use_white)
{
  GdkColor tmp_color;
  GdkColor *new_color;
  if ((!(gdk_color_parse(defined_color, &tmp_color))) &&
      ((!fallback_color) || (!(gdk_color_parse(fallback_color, &tmp_color)))) &&
      ((!second_fallback_color) || (!(gdk_color_parse(second_fallback_color, &tmp_color)))))
    {
      if (use_white)
	{
	  /* snd_error here can cause trouble (no error dialog or something) */
	  fprintf(stderr, _("can't get %s -- will use white\n"), defined_color);
	  gdk_color_parse("white", &tmp_color);
	}
      else
	{
	  fprintf(stderr, _("can't get %s -- will use black\n"), defined_color);
	  gdk_color_parse("black", &tmp_color);
	}
    }
  new_color = gdk_color_copy(&tmp_color);
  gdk_rgb_find_color(gdk_colormap_get_system(), new_color);
  return(new_color);
}

#if HAVE_PWD_H
  #include <pwd.h>
#endif

#ifdef SND_AS_WIDGET
GtkWidget *snd_as_widget(int argc, char **argv, GtkWidget *parent, void (*error_func)(const char *))
{

#else

void snd_doit(int argc, char **argv)
{
#endif
  GtkWidget *shell;
  int i;
  state_context *sx;
#ifdef SND_AS_WIDGET
  set_snd_error_display (error_func);
  ss = snd_main(argc, argv);
#else
  gtk_init(&argc, &argv);
#ifndef MAC_OSX
  gdk_set_locale();
#endif
#endif

  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = GDK_CROSSHAIR;

#ifndef SND_AS_WIDGET
  shell = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  sg_make_resizable(shell);
#endif

  auto_open_files = argc-1;
  if (argc > 1) auto_open_file_names = (char **)(argv + 1);
  ss->startup_title = copy_string("snd");

  set_sound_style(SOUNDS_VERTICAL);
  for (i = 1; i < argc; i++)
    if ((strcmp(argv[i], "-h") == 0) || 
	(strcmp(argv[i], "-horizontal") == 0))
      set_sound_style(SOUNDS_HORIZONTAL);
    else
      if ((strcmp(argv[i], "-v") == 0) || 
	  (strcmp(argv[i], "-vertical") == 0))
	set_sound_style(SOUNDS_VERTICAL);
      else
	if (strcmp(argv[i], "-notebook") == 0)
	  set_sound_style(SOUNDS_IN_NOTEBOOK);
	else
	  if (strcmp(argv[i], "-separate") == 0)
	    set_sound_style(SOUNDS_IN_SEPARATE_WINDOWS);
	  else
	    if (strcmp(argv[i], "-noglob") == 0)
	      noglob = true;
	    else
	      if (strcmp(argv[i], "-noinit") == 0)
		noinit = true;
	      else
		if (strcmp(argv[i], "-nostdin") == 0)
		  nostdin = true;
		else
		  if ((strcmp(argv[i], "-b") == 0) || 
		      (strcmp(argv[i], "-batch") == 0))
		    batch = true;

  ss->batch_mode = batch;
  set_auto_resize(AUTO_RESIZE_DEFAULT);
  ss->zoom_slider_width = ZOOM_SLIDER_WIDTH;
  ss->position_slider_width = POSITION_SLIDER_WIDTH;
  ss->channel_sash_indent = CHANNEL_SASH_INDENT; /* not currently used */
  ss->channel_sash_size = CHANNEL_SASH_SIZE;
  ss->sash_size = SASH_SIZE;
  ss->sash_indent = SASH_INDENT;
  ss->toggle_size = TOGGLE_SIZE;
  ss->enved_point_size = ENVED_POINT_SIZE;

  ss->sgx = (state_context *)CALLOC(1, sizeof(state_context));
  sx = ss->sgx;
  sx->graph_is_active = false;
  set_html_dir(copy_string(DEFAULT_HTML_DIR));

  /* the gray shades are an attempt to get around Netscape which hogs all the colors */
  sx->white =                 get_color(WHITE_COLOR,           NULL, NULL, true);
  sx->black =                 get_color(BLACK_COLOR,           NULL, NULL, false);
  sx->light_blue =            get_color(LIGHT_BLUE_COLOR,      NULL, NULL, true);
  sx->lighter_blue =          get_color(LIGHTER_BLUE_COLOR,    NULL, NULL, true);
  sx->red =                   get_color(RED_COLOR,             NULL, NULL, false);
  sx->green =                 get_color(GREEN_COLOR,           NULL, NULL, false);
  sx->yellow =                get_color(YELLOW_COLOR,          NULL, NULL, true);
  sx->highlight_color =       get_color(HIGHLIGHT_COLOR,       "gray90", NULL, true);
  sx->basic_color =           get_color(BASIC_COLOR,           "gray80", "gray", true);
  sx->position_color =        get_color(POSITION_COLOR,        "gray60", "gray", false);
  sx->zoom_color =            get_color(ZOOM_COLOR,            "gray20", "gray", false);
  sx->cursor_color =          get_color(CURSOR_COLOR,          NULL, NULL, false);
  sx->selection_color =       get_color(SELECTION_COLOR,       "gray80", NULL, false);
  sx->mix_color =             get_color(MIX_COLOR,             NULL, NULL, false);
  sx->selected_mix_color =    get_color(SELECTED_MIX_COLOR,    NULL, NULL, false);
  sx->enved_waveform_color =  get_color(ENVED_WAVEFORM_COLOR,  NULL, NULL, false);
  sx->filter_waveform_color = get_color(FILTER_WAVEFORM_COLOR, NULL, NULL, false);
  sx->listener_color =        get_color(LISTENER_COLOR,        NULL, NULL, true);
  sx->listener_text_color =   get_color(LISTENER_TEXT_COLOR,   NULL, NULL, true);
  sx->graph_color =           get_color(GRAPH_COLOR,           NULL, NULL, true);
  sx->selected_graph_color =  get_color(SELECTED_GRAPH_COLOR,  NULL, NULL, true);
  sx->data_color =            get_color(DATA_COLOR,            NULL, NULL, false);
  sx->selected_data_color =   get_color(SELECTED_DATA_COLOR,   NULL, NULL, false);
  sx->mark_color =            get_color(MARK_COLOR,            NULL, NULL, false);
  sx->sash_color =            get_color(SASH_COLOR,            NULL, NULL, false);
  sx->pushed_button_color =   get_color(PUSHED_BUTTON_COLOR,   NULL, NULL, false);
  sx->text_focus_color =      get_color(TEXT_FOCUS_COLOR,      NULL, NULL, false);

  if ((!(set_tiny_font(DEFAULT_TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font: %s"), DEFAULT_TINY_FONT);

  if ((!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font: %s"), DEFAULT_AXIS_LABEL_FONT);

  if ((!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font: %s"), DEFAULT_AXIS_NUMBERS_FONT);

  if ((!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font: %s"), DEFAULT_PEAKS_FONT);

  if ((!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font: %s"), DEFAULT_BOLD_PEAKS_FONT);

  ss->init_file = copy_string(getenv(SND_INIT_FILE_ENVIRONMENT_NAME));
  if (ss->init_file == NULL)
    ss->init_file = INIT_FILE_NAME;

  set_color_map(DEFAULT_SPECTROGRAM_COLOR);
  set_ask_before_overwrite(false);
  MAIN_PANE(ss) = gtk_vbox_new(false, 0); /* not homogenous, spacing 0 */

#ifdef SND_AS_WIDGET
  MAIN_SHELL(ss) = parent;
  shell = MAIN_PANE(ss);
#else
  MAIN_SHELL(ss) = shell;
  gtk_container_add(GTK_CONTAINER(MAIN_SHELL(ss)), MAIN_PANE(ss));
#endif
  gtk_widget_modify_bg(MAIN_SHELL(ss), GTK_STATE_NORMAL, ss->sgx->basic_color);
  add_menu();
  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      SOUND_PANE(ss) = gtk_vpaned_new();
      gtk_container_set_border_width(GTK_CONTAINER(SOUND_PANE(ss)), 0);
      gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), SOUND_PANE(ss));
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  SOUND_PANE_BOX(ss) = gtk_notebook_new();
	  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), GTK_POS_RIGHT);
	}
      else 
	{
	  if (sound_style(ss) == SOUNDS_HORIZONTAL)
	    SOUND_PANE_BOX(ss) = gtk_hbox_new(false, 0);
	  else SOUND_PANE_BOX(ss) = gtk_vbox_new(false, 0);
	}
      gtk_paned_add1(GTK_PANED(SOUND_PANE(ss)), SOUND_PANE_BOX(ss));
      gtk_widget_show(SOUND_PANE_BOX(ss));
      gtk_widget_show(SOUND_PANE(ss));
    }
  gtk_widget_modify_bg(SOUND_PANE(ss), GTK_STATE_NORMAL, ss->sgx->basic_color);
  gtk_widget_modify_bg(SOUND_PANE_BOX(ss), GTK_STATE_NORMAL, ss->sgx->basic_color);

  gtk_widget_show(MAIN_PANE(ss));
  gtk_widget_show (MAIN_SHELL(ss));

#ifndef SND_AS_WIDGET
  MAIN_WINDOW(ss) = MAIN_SHELL(ss)->window;
#else
  MAIN_WINDOW(ss) = gtk_widget_get_parent_window(MAIN_SHELL(ss));
#endif

  setup_gcs();
  make_icons_transparent(BASIC_COLOR);

  if (mus_file_probe("Snd.gtkrc"))
    gtk_rc_parse("Snd.gtkrc");
  else gtk_rc_parse_string ("\n\
style \"default\"\n\
{\n\
  font_name = \"Serif 11\"\n\
\n\
  fg[NORMAL]      = { 0.0,  0.00, 0.0 }\n\
  text[NORMAL]    = { 0.0,  0.0,  0.0 }\n\
  bg[NORMAL]      = { 0.93, 0.93, 0.87 }\n\
  bg[ACTIVE]      = { 0.80, 0.80, 0.75 }\n\
  bg[INSENSITIVE] = { 0.93, 0.93, 0.87 }\n\
  base[NORMAL]    = { 1.00, 1.00, 1.00 }\n\
  bg[PRELIGHT]    = { 0.54, 0.54, 0.51 }\n\
  fg[PRELIGHT]    = { 1.0,  0.0,  0.0}\n\
}\n\
style \"default_button\" = \"default\"\n\
{\n\
  bg[ACTIVE]   = { 0.79, 0.88, 1.0 }\n\
  bg[SELECTED] = { 0.79, 0.88, 1.0 }\n\
}\n\
style \"default_menu\" = \"default\"\n\
{\n\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n\
}\n\
style \"default_text\" = \"default\"\n\
{\n\
  base[ACTIVE]      = { 0.93, 0.93, 0.87 }\n\
  base[SELECTED]    = { 1.0, 1.0, 1.0 }\n\
  base[PRELIGHT]    = { 1.0, 1.0, 1.0}\n\
  bg[ACTIVE]        = { 1.0, 1.0, 1.0 }\n\
  bg[SELECTED]      = { 1.0, 1.0, 1.0 }\n\
  bg[PRELIGHT]      = { 1.0, 1.0, 1.0 }\n\
  text[ACTIVE]      = { 0.0, 0.0, 0.0 }\n\
  text[SELECTED]    = { 0.0, 0.0, 0.0 }\n\
  text[PRELIGHT]    = { 0.0, 0.0, 0.0 }\n\
  base[NORMAL]      = { 0.93, 0.93, 0.87 }\n\
  base[INSENSITIVE] = { 0.93, 0.93, 0.87 }\n\
}\n\
class \"GtkWidget\" style \"default\"\n\
class \"GtkButton\" style \"default_button\"\n\
class \"GtkMenu\" style \"default_menu\"\n\
class \"GtkEntry\" style \"default_text\"\n\
class \"GtkTextView\" style \"default_text\"\n\
");

  if (batch) gtk_widget_hide(MAIN_SHELL(ss));
  BACKGROUND_ADD(startup_funcs, NULL);

#if HAVE_SETJMP_H
#if TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      snd_error(_("Caught seg fault (will try to continue):\n"));
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error(_("Caught top level error (will try to continue):\n"));
      else ss->jump_ok = false;
    }
#endif

#ifndef SND_AS_WIDGET
  SetupIcon(shell);
  gtk_main();
#else
  return(shell);
#endif
}

void g_init_gxmain(void)
{
}
