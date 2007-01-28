#include "snd.h"

#define FALLBACK_FONT "Monospace 14"
#define HIGHLIGHT_COLOR      "ivory1"
#define BASIC_COLOR          "ivory2"
#define POSITION_COLOR       "ivory3"
#define ZOOM_COLOR           "ivory4"
#define CURSOR_COLOR         "red"
#define SELECTION_COLOR      "lightsteelblue1"
#define MIX_COLOR            "darkgray"
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
#define FILTER_CONTROL_WAVEFORM_COLOR "blue"
#define PUSHED_BUTTON_COLOR  "lightsteelblue1"
#define SASH_COLOR           "lightgreen"

#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 10

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
  if (snd_exit_cleanly(EXIT_FORCED))
    snd_exit(0);
  return(false);
}

static GtkWidget **iconify_active_dialogs = NULL;
static gint window_iconify(GtkWidget *w, GdkEventWindowState *event, gpointer context)
{
  int i;
  if ((!ss) || (!(ss->sgx)) || (!(ss->sgx->dialogs)))
    return(false);
  if (event->new_window_state & GDK_WINDOW_STATE_ICONIFIED)
    {
      /* presumably we are now iconified */

      if (iconify_active_dialogs) FREE(iconify_active_dialogs);
      iconify_active_dialogs = (GtkWidget **)CALLOC(ss->sgx->num_dialogs, sizeof(GtkWidget *));

      for (i = 0; i < ss->sgx->num_dialogs; i++)
	if (ss->sgx->dialogs[i])
	  {
	    if (GTK_WIDGET_VISIBLE(ss->sgx->dialogs[i]))
	      iconify_active_dialogs[i] = ss->sgx->dialogs[i];
	    gtk_widget_hide(ss->sgx->dialogs[i]);
	  }
    }
  else
    {
      if (!(event->new_window_state & GDK_WINDOW_STATE_ICONIFIED))
	{
	  /* this is confusing -- can I assume I've been deiconified or not?  if not, how to tell? */
	  if (iconify_active_dialogs) 
	    {
	      for (i = 0; i < ss->sgx->num_dialogs; i++)
		if (iconify_active_dialogs[i])
		  gtk_widget_show(iconify_active_dialogs[i]);

	      FREE(iconify_active_dialogs);
	      iconify_active_dialogs = NULL;
	    }
	}
    }
  return(false);
}
#endif

#if (!HAVE_FAM)
static guint auto_update_proc = 0;

static gint auto_update_check(gpointer context)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if ((!(play_in_progress())) && 
	  (!(record_in_progress())))
	for_each_sound(sound_not_current);
      auto_update_proc = g_timeout_add_full(0, (guint32)(auto_update_interval(ss) * 1000), auto_update_check, context, NULL);
    }
  else auto_update_proc = 0;
  return(0);
}

void auto_update_restart(void)
{
  if (auto_update_proc == 0)
    g_timeout_add_full(0, (guint32)(auto_update_interval(ss) * 1000), auto_update_check, NULL, NULL);
}
#else
void auto_update_restart(void) {}
#endif

static GdkAtom snd_v, snd_c;

#if HAVE_EXTENSION_LANGUAGE
static XEN window_property_changed_hook;

static gboolean who_called(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  /* watch for communication from some other program via the SND_COMMAND property */
  GdkEventProperty *ev = (GdkEventProperty *)event;
  if (ev->atom == snd_c)
    {
      GdkAtom type;
      gint format, nitems;
      guchar *version[1];
      if (gdk_property_get(MAIN_WINDOW(ss), snd_c, 
			   GDK_TARGET_STRING, 0L, (long)BUFSIZ, false,
			   &type, &format, &nitems, (guchar **)version))
	{
	  if (version[0])
	    {
	      char *buf;
	      buf = (char *)(version[0]);
	      if ((snd_strlen(buf) > 1) ||
		  ((snd_strlen(buf) == 1) && (buf[0] != '\n')))
		{
		  if ((!(XEN_HOOKED(window_property_changed_hook))) ||
		      (!(XEN_TRUE_P(run_or_hook(window_property_changed_hook,
						XEN_LIST_1(C_TO_XEN_STRING(buf)),
						S_window_property_changed_hook)))))
		    snd_report_result(snd_catch_any(eval_str_wrapper, (void *)buf, buf), NULL);
		}
	      free(version[0]);
	    }
	}
    }
  return(false);
}
#endif

#if HAVE_SETJMP_H
#include <setjmp.h>

#if MUS_TRAP_SEGFAULT
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
static bool noglob = false, noinit = false, batch = false, nostdin = false, nogtkrc = false;

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
      g_source_remove(stdin_id);
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

static void setup_gcs(void)
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
  gdk_gc_set_foreground(sx->fltenv_data_gc, sx->filter_control_waveform_color);
  gdk_gc_set_function(sx->fltenv_data_gc, GDK_COPY);

  initialize_colormap();
}

static void save_a_color(FILE *Fp, const char *def_name, GdkColor *current_color, const char *ext_name)
{
#if HAVE_EXTENSION_LANGUAGE
  GdkColor default_color;
  if (gdk_color_parse(def_name, &default_color))
    {
      if ((current_color->red != default_color.red) ||
	  (current_color->green != default_color.green) ||
	  (current_color->blue != default_color.blue))
#if HAVE_FORTH
	fprintf(Fp, "%.3f %.3f %.3f %s set-%s drop\n", 
		(float)current_color->red / 65535.0,
		(float)current_color->green / 65535.0,
		(float)current_color->blue / 65535.0,
		S_make_color,
		ext_name); 
#else
#if HAVE_SCHEME
	fprintf(Fp, "(set! (%s) (%s %.3f %.3f %.3f))\n", 
#endif
#if HAVE_RUBY
	fprintf(Fp, "set_%s(%s(%.3f, %.3f, %.3f))\n", 
#endif
		TO_PROC_NAME(ext_name), 
		TO_PROC_NAME(S_make_color),
		(float)current_color->red / 65535.0,
		(float)current_color->green / 65535.0,
		(float)current_color->blue / 65535.0);
#endif
    }
#endif
}

void save_colors(FILE *Fp)
{
  save_a_color(Fp, BASIC_COLOR, ss->sgx->basic_color, S_basic_color);
  save_a_color(Fp, CURSOR_COLOR, ss->sgx->cursor_color, S_cursor_color);
  save_a_color(Fp, DATA_COLOR, ss->sgx->data_color, S_data_color);
  save_a_color(Fp, SELECTED_DATA_COLOR, ss->sgx->selected_data_color, S_selected_data_color);
  save_a_color(Fp, HIGHLIGHT_COLOR, ss->sgx->highlight_color, S_highlight_color);
  save_a_color(Fp, POSITION_COLOR, ss->sgx->position_color, S_position_color);
  save_a_color(Fp, ZOOM_COLOR, ss->sgx->zoom_color, S_zoom_color);
  save_a_color(Fp, SELECTION_COLOR, ss->sgx->selection_color, S_selection_color);
  save_a_color(Fp, MIX_COLOR, ss->sgx->mix_color, S_mix_color);
  save_a_color(Fp, ENVED_WAVEFORM_COLOR, ss->sgx->enved_waveform_color, S_enved_waveform_color);
  save_a_color(Fp, FILTER_CONTROL_WAVEFORM_COLOR, ss->sgx->filter_control_waveform_color, S_filter_control_waveform_color);
  save_a_color(Fp, LISTENER_COLOR, ss->sgx->listener_color, S_listener_color);
  save_a_color(Fp, LISTENER_TEXT_COLOR, ss->sgx->listener_text_color, S_listener_text_color);
  save_a_color(Fp, GRAPH_COLOR, ss->sgx->graph_color, S_graph_color);
  save_a_color(Fp, SELECTED_GRAPH_COLOR, ss->sgx->selected_graph_color, S_selected_graph_color);
  save_a_color(Fp, MARK_COLOR, ss->sgx->mark_color, S_mark_color);
  save_a_color(Fp, SASH_COLOR, ss->sgx->sash_color, S_sash_color);
  save_a_color(Fp, PUSHED_BUTTON_COLOR, ss->sgx->pushed_button_color, S_pushed_button_color);
  save_a_color(Fp, TEXT_FOCUS_COLOR, ss->sgx->text_focus_color, S_text_focus_color);
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
			  (guchar *)(SND_DATE), 
			  strlen(SND_DATE) + 1);
#if HAVE_EXTENSION_LANGUAGE
      gtk_widget_add_events(MAIN_SHELL(ss), GDK_PROPERTY_CHANGE_MASK);
      SG_SIGNAL_CONNECT(MAIN_SHELL(ss), "property_notify_event", who_called, NULL);
#endif
      /* trap outer-level Close for cleanup check */
      SG_SIGNAL_CONNECT(MAIN_SHELL(ss), "delete_event", window_close, NULL);
      /* when iconified, we need to hide any dialogs as well */
      SG_SIGNAL_CONNECT(MAIN_SHELL(ss), "window_state_event", window_iconify, NULL);
#endif
      ss->sgx->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(ss));
      ss->sgx->wait_cursor = gdk_cursor_new(GDK_WATCH);
      ss->sgx->arrow_cursor = gdk_cursor_new(GDK_LEFT_PTR);
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
#if (!HAVE_FAM)
      if (auto_update_interval(ss) > 0.0)
	g_timeout_add_full(0, (guint32)(auto_update_interval(ss) * 1000), auto_update_check, NULL, NULL);
#endif
      break;
    case 4: 
#if MUS_TRAP_SEGFAULT
      if (trap_segfault(ss)) signal(SIGSEGV, segv);
#endif
      if ((ss->sounds) &&
	  (ss->selected_sound == NO_SELECTION))
	{
	  snd_info *sp;
	  sp = ss->sounds[0];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) &&
	      (sp->selected_channel == NO_SELECTION)) /* don't clobber possible select-channel in loaded startup files */
	    select_channel(sp, 0);
	}
      if (ss->startup_errors)
	{
	  handle_listener(true);
	  listener_append(ss->startup_errors);
	  FREE(ss->startup_errors);
	  ss->startup_errors = NULL;
	}
      return(BACKGROUND_QUIT); 
      break;
    }
  tm_slice++;
  return(BACKGROUND_CONTINUE);
}

#ifndef SND_AS_WIDGET
static void set_up_icon(void)
{
  GdkPixmap *pix;
  GdkBitmap *mask;
  pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), &mask, NULL, snd_icon_bits());
  gdk_window_set_icon(MAIN_WINDOW(ss), NULL, pix, mask);
}
#endif

color_t get_in_between_color(color_t fg, color_t bg)
{
  GdkColor gcolor;
  GdkColor *new_color;
  gcolor.red = (unsigned short)((fg->red + (2 * bg->red)) / 3);
  gcolor.green = (unsigned short)((fg->green + (2 * bg->green)) / 3);
  gcolor.blue = (unsigned short)((fg->blue + (2 * bg->blue)) / 3);
  new_color = gdk_color_copy(&gcolor);
  gdk_rgb_find_color(gdk_colormap_get_system(), new_color);
  return(new_color);
}

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

static void notebook_switch_page(GtkNotebook *w, GtkNotebookPage *page_widget, gint page_num)
{
  /* as far as I can tell there's nothing that a user can do with the idiotic page_widget argument --
   *   the GtkNotebookPage structure is hidden, and not one public function gives any access to it.
   */
  GtkWidget *pw;
  pw = gtk_notebook_get_nth_page(w, page_num);
  if (pw)
    {
      int index = 0;
      index = get_user_int_data(G_OBJECT(pw));
      if ((index < ss->max_sounds) && 
	  (snd_ok(ss->sounds[index])))
	{
	  snd_info *sp;
	  sp = ss->sounds[index];
	  if (sp->selected_channel == NO_SELECTION)
	    select_channel(ss->sounds[index], 0);
	  else select_channel(ss->sounds[index], sp->selected_channel);
	}
    }
}

#ifdef SND_AS_WIDGET
GtkWidget *snd_as_widget(int argc, char **argv, GtkWidget *parent, void (*error_func)(const char *msg))
{

#else

void snd_doit(int argc, char **argv)
{
#endif
  char *str = NULL;
  GtkWidget *shell;
  int i;
  state_context *sx;
#ifdef SND_AS_WIDGET
  set_error_display(error_func);
  ss = snd_main(argc, argv);
#else
  gtk_init(&argc, &argv);
#ifndef MUS_MAC_OSX
  gdk_set_locale();
#endif
#endif

  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = DEFAULT_GRAPH_CURSOR;

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
		  if (strcmp(argv[i], "-nogtkrc") == 0)
		    nogtkrc = true;
		  else
		    if ((strcmp(argv[i], "-b") == 0) || 
			(strcmp(argv[i], "-batch") == 0))
		      batch = true;
		    else
		      if (strcmp(argv[i], "--features") == 0) /* testing (compsnd) */
			check_features_list(argv[i + 1]);

  ss->batch_mode = batch;
  set_auto_resize(AUTO_RESIZE_DEFAULT);
  ss->zoom_slider_width = ZOOM_SLIDER_WIDTH;
  ss->position_slider_width = POSITION_SLIDER_WIDTH;
  ss->channel_sash_indent = CHANNEL_SASH_INDENT; /* not currently used */
  ss->channel_sash_size = CHANNEL_SASH_SIZE;
  ss->sash_size = SASH_SIZE;
  ss->sash_indent = SASH_INDENT;
  ss->toggle_size = TOGGLE_SIZE;
  ss->sgx = (state_context *)CALLOC(1, sizeof(state_context));
  sx = ss->sgx;
  sx->graph_is_active = false;

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
  sx->enved_waveform_color =  get_color(ENVED_WAVEFORM_COLOR,  NULL, NULL, false);
  sx->filter_control_waveform_color = get_color(FILTER_CONTROL_WAVEFORM_COLOR, NULL, NULL, false);
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

  sx->grid_color = get_in_between_color(sx->data_color, sx->graph_color);
  sx->selected_grid_color = get_in_between_color(sx->selected_data_color, sx->selected_graph_color);

  sx->orig_data_color = sx->data_color;
  sx->orig_selected_data_color = sx->selected_data_color;
  sx->orig_mark_color = sx->mark_color;
  sx->orig_mix_color = sx->mix_color;
  sx->orig_graph_color = sx->graph_color;
  sx->orig_selected_graph_color = sx->selected_graph_color;
  sx->orig_listener_color = sx->listener_color;
  sx->orig_listener_text_color = sx->listener_text_color;
  sx->orig_cursor_color = sx->cursor_color;
  sx->orig_basic_color = sx->basic_color;
  sx->orig_selection_color = sx->selection_color;
  sx->orig_zoom_color = sx->zoom_color;
  sx->orig_position_color = sx->position_color;
  sx->orig_highlight_color = sx->highlight_color;

  if ((!(set_tiny_font(DEFAULT_TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find tiny font: %s"), DEFAULT_TINY_FONT);

  if ((!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find axis label font: %s"), DEFAULT_AXIS_LABEL_FONT);

  if ((!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find axis numbers font: %s"), DEFAULT_AXIS_NUMBERS_FONT);

  if ((!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find peaks font: %s"), DEFAULT_PEAKS_FONT);

  if ((!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find bold peaks font: %s"), DEFAULT_BOLD_PEAKS_FONT);

  if (!(set_listener_font(FALLBACK_FONT)))
    fprintf(stderr, _("can't find listener font: %s"), FALLBACK_FONT);

  ss->orig_axis_label_font = copy_string(axis_label_font(ss));
  ss->orig_axis_numbers_font = copy_string(axis_numbers_font(ss));
  ss->orig_peaks_font = copy_string(peaks_font(ss));
  ss->orig_bold_peaks_font = copy_string(bold_peaks_font(ss));
  ss->orig_listener_font = copy_string(listener_font(ss));
  ss->orig_tiny_font = copy_string(tiny_font(ss));

  if (!nogtkrc)
    {
      str = mus_expand_filename("~/.gtkrc-2.0");
      if (mus_file_probe(str))
	gtk_rc_parse(str);
      else
	{
	  if (mus_file_probe("Snd.gtkrc"))
	    gtk_rc_parse("Snd.gtkrc");
	  else
	    {
	      if (str) FREE(str);
	      str = mus_expand_filename("~/Snd.gtkrc");
	      if (mus_file_probe(str))
		gtk_rc_parse(str);
#if WITH_BUILTIN_GTKRC
	      else 
		{
		  gtk_rc_parse_string("\n\
\n					 \
# This is the same as Snd.gtkrc\n	 \
style \"default\"\n			 \
{\n					 \
#  font_name = \"Serif 11\"\n		 \
\n						\
  fg[NORMAL]      = { 0.0,  0.00, 0.0 }\n	\
  text[NORMAL]    = { 0.0,  0.0,  0.0 }\n	\
  bg[NORMAL]      = { 0.93, 0.93, 0.87 }\n	\
  bg[ACTIVE]      = { 0.80, 0.80, 0.75 }\n	\
  bg[INSENSITIVE] = { 0.93, 0.93, 0.87 }\n	\
  base[NORMAL]    = { 1.00, 1.00, 1.00 }\n	\
  bg[PRELIGHT]    = { 0.70, 0.70, 0.64 }\n	\
  fg[PRELIGHT]    = { 1.0,  0.0,  0.0}\n	\
\n						\
  GtkPaned::handle_size = 6\n			\
  xthickness = 1\n				\
  ythickness = 1\n				\
}\n						\
\n						\
style \"default_button\" = \"default\"\n	\
{\n						\
  GtkButton::default_border = { 1, 0, 1, 0 }\n		\
  GtkButton::default_outside_border = { 1, 0, 1, 0 }\n	\
  GtkButton::inner_border = { 1, 0, 1, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
}\n							\
\n							\
style \"default_menu\" = \"default\"\n			\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
style \"default_pane\" = \"default\"\n			\
{\n							\
  bg[NORMAL] = { 0.56, 0.93, 0.56 }\n			\
  bg[PRELIGHT] = { 0.26, 0.8, 0.26}\n			\
}\n							\
\n							\
style \"default_entry\" = \"default\"\n			\
{\n							\
  base[ACTIVE]      = { 0.93, 0.93, 0.87 }\n		\
  base[SELECTED]    = { 0.80, 0.80, 0.75 }\n		\
  base[PRELIGHT]    = { 1.0, 1.0, 1.0}\n		\
  base[NORMAL]      = { 0.93, 0.93, 0.87 }\n		\
  base[INSENSITIVE] = { 0.93, 0.93, 0.87 }\n		\
  bg[ACTIVE]        = { 1.0, 1.0, 1.0 }\n		\
  bg[SELECTED]      = { 1.0, 1.0, 1.0 }\n		\
  bg[PRELIGHT]      = { 1.0, 1.0, 1.0 }\n		\
  text[ACTIVE]      = { 0.0, 0.0, 0.0 }\n		\
  text[SELECTED]    = { 0.0, 0.0, 0.0 }\n		\
  text[PRELIGHT]    = { 0.0, 0.0, 0.0 }\n		\
}\n							\
\n							\
style \"default_text\" = \"default_entry\"\n		\
{\n							\
  base[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
}\n							\
\n							\
style \"default_slider\" = \"default\"\n		\
{\n							\
  bg[NORMAL] = { 0.90, 0.90, 0.85 }\n			\
  bg[ACTIVE] = { 0.80, 0.80, 0.75 }\n			\
  bg[PRELIGHT] = { 0.70, 0.70, 0.64 }\n			\
\n							\
  GtkRange::slider_width = 13\n				\
  GtkRange::stepper_size = 10\n				\
}\n							\
style \"default_frame\" = \"default\"\n			\
{\n							\
  xthickness = 4\n					\
  ythickness = 4\n					\
}\n							\
class \"GtkWidget\" style \"default\"\n			\
class \"GtkButton\" style \"default_button\"\n		\
class \"GtkMenu\" style \"default_menu\"\n		\
class \"GtkMenuBar\" style \"default_menu\"\n		\
class \"GtkEntry\" style \"default_entry\"\n		\
class \"GtkTextView\" style \"default_text\"\n		\
class \"GtkPaned\" style \"default_pane\"\n		\
class \"GtkRange\" style \"default_slider\"\n		\
class \"GtkFrame\" style \"default_frame\"\n		\
\n							\
style \"zoom_slider\" = \"default_slider\"\n		\
{\n							\
  bg[NORMAL] = { 0.70, 0.70, 0.64 }\n			\
  bg[ACTIVE] = { 0.54, 0.54, 0.51 }\n			\
  bg[PRELIGHT] = { 0.44, 0.44, 0.36 }\n			\
\n							\
  GtkRange::slider_width = 12\n				\
  GtkRange::stepper_size = 12\n				\
}\n							\
\n							\
widget \"*.zx_slider\" style \"zoom_slider\"\n		\
widget \"*.zy_slider\" style \"zoom_slider\"\n		\
widget \"*.gzy_slider\" style \"zoom_slider\"\n		\
widget \"*.panel_button\" style \"zoom_slider\"\n	\
\n							\
style \"listener\" = \"default\"\n			\
{\n							\
#  font_name = \"Monospace 10\"\n			\
\n							\
  base[NORMAL] = { 0.94, 0.97, 1.0 }\n			\
  text[NORMAL] = { 0.0, 0.0, 0.0 }\n			\
}\n							\
\n							\
widget \"*.listener_text\" style \"listener\"\n		\
\n							\
style \"help\" = \"default_button\"\n			\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
widget \"*.help_button\" style \"help\"\n		\
\n							\
style \"quit\" = \"default_button\"\n			\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
widget \"*.quit_button\" style \"quit\"\n		\
\n							\
style \"doit\" = \"default_button\"\n			\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
widget \"*.doit_button\" style \"doit\"\n		\
\n							\
style \"doit_again\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
widget \"*.doit_again_button\" style \"doit_again\"\n	\
\n							\
style \"reset\" = \"default_button\"\n			\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
}\n							\
\n							\
widget \"*.reset_button\" style \"reset\"\n		\
widget \"*.the_unpane\" style \"default\"\n		\
style \"recorder\" = \"default\"\n			\
{\n							\
  xthickness = 8\n					\
  ythickness = 8\n					\
}\n							\
widget \"*.record_frame\" style \"recorder\"\n		\
style \"white_button\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
  bg[PRELIGHT] = { 0.94, 0.97, 1.0 }\n			\
  fg[PRELIGHT] = { 0.0,  0.0,  0.0}\n			\
  GtkButton::default_border = { 0, 0, 0, 0 }\n		\
  GtkButton::default_outside_border = { 0, 0, 0, 0 }\n	\
  GtkButton::inner_border = { 0, 0, 0, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
  xthickness = 0\n					\
  ythickness = 0\n					\
}\n							\
\n							\
widget \"*.white_button\" style \"white_button\"\n	\
style \"env_button\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 0.94, 0.97, 1.0 }\n			\
  bg[INSENSITIVE] = { 0.94, 0.97, 1.0 }\n		\
  GtkButton::default_border = { 0, 0, 0, 0 }\n		\
  GtkButton::default_outside_border = { 0, 0, 0, 0 }\n	\
  GtkButton::inner_border = { 0, 0, 0, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
  xthickness = 0\n					\
  ythickness = 0\n					\
}\n							\
\n							\
widget \"*.env_button\" style \"env_button\"\n		\
style \"label_button\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n			\
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n			\
  fg[PRELIGHT] = { 0.0,  0.0,  0.0}\n			\
\n							\
  GtkButton::default_border = { 0, 0, 0, 0 }\n		\
  GtkButton::default_outside_border = { 0, 0, 0, 0 }\n	\
  GtkButton::inner_border = { 0, 0, 0, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
  xthickness = 0\n					\
  ythickness = 0\n					\
}\n							\
widget \"*.label_button\" style \"label_button\"\n	\
binding \"gtk-emacs-text-entry\"\n			\
{\n									\
  bind \"<ctrl>b\" { \"move-cursor\" (logical-positions, -1, 0) }\n	\
  bind \"<shift><ctrl>b\" { \"move-cursor\" (logical-positions, -1, 1) }\n \
  bind \"<ctrl>f\" { \"move-cursor\" (logical-positions, 1, 0) }\n	\
  bind \"<shift><ctrl>f\" { \"move-cursor\" (logical-positions, 1, 1) }\n \
  bind \"<alt>b\" { \"move-cursor\" (words, -1, 0) }\n			\
  bind \"<shift><alt>b\" { \"move-cursor\" (words, -1, 1) }\n		\
  bind \"<alt>f\" { \"move-cursor\" (words, 1, 0) }\n			\
  bind \"<shift><alt>f\" { \"move-cursor\" (words, 1, 1) }\n		\
  bind \"<ctrl>a\" { \"move-cursor\" (paragraph-ends, -1, 0) }\n	\
  bind \"<shift><ctrl>a\" { \"move-cursor\" (paragraph-ends, -1, 1) }\n	\
  bind \"<ctrl>e\" { \"move-cursor\" (paragraph-ends, 1, 0) }\n		\
  bind \"<shift><ctrl>e\" { \"move-cursor\" (paragraph-ends, 1, 1) }\n	\
  bind \"<ctrl>w\" { \"cut-clipboard\" () }\n				\
  bind \"<ctrl>y\" { \"paste-clipboard\" () }\n				\
  bind \"<ctrl>d\" { \"delete-from-cursor\" (chars, 1) }\n		\
  bind \"<alt>d\" { \"delete-from-cursor\" (word-ends, 1) }\n		\
  bind \"<ctrl>k\" { \"delete-from-cursor\" (paragraph-ends, 1) }\n	\
  bind \"<alt>backslash\" { \"delete-from-cursor\" (whitespace, 1) }\n	\
  bind \"<alt>space\" { \"delete-from-cursor\" (whitespace, 1)\n	\
                      \"insert-at-cursor\" (\" \") }\n			\
  bind \"<alt>KP_Space\" { \"delete-from-cursor\" (whitespace, 1)\n	\
                         \"insert-at-cursor\" (\" \")  }\n		\
  bind \"<ctrl>u\" {\n							\
     \"move-cursor\" (paragraph-ends, -1, 0)\n				\
     \"delete-from-cursor\" (paragraph-ends, 1)\n			\
  }\n									\
  bind \"<ctrl>h\" { \"delete-from-cursor\" (chars, -1) }\n		\
  bind \"<ctrl>w\" { \"delete-from-cursor\" (word-ends, -1) }\n		\
}\n									\
binding \"gtk-emacs-text-view\"\n					\
{\n									\
  bind \"<ctrl>p\" { \"move-cursor\" (display-lines, -1, 0) }\n		\
  bind \"<shift><ctrl>p\" { \"move-cursor\" (display-lines, -1, 1) }\n	\
  bind \"<ctrl>n\" { \"move-cursor\" (display-lines, 1, 0) }\n		\
  bind \"<shift><ctrl>n\" { \"move-cursor\" (display-lines, 1, 1) }\n	\
  bind \"<ctrl>space\" { \"set-anchor\" () }\n				\
  bind \"<ctrl>KP_Space\" { \"set-anchor\" () }\n			\
}\n									\
class \"GtkEntry\" binding \"gtk-emacs-text-entry\"\n			\
class \"GtkTextView\" binding \"gtk-emacs-text-entry\"\n		\
class \"GtkTextView\" binding \"gtk-emacs-text-view\"\n			\
");
		}
#endif
	    }
	}
      if (str) FREE(str);
    } /* not nogtkrc */
  
  MAIN_PANE(ss) = gtk_vbox_new(false, 0); /* not homogenous, spacing 0 */
  
#ifdef SND_AS_WIDGET
  MAIN_SHELL(ss) = parent;
  shell = MAIN_PANE(ss);
#else
  MAIN_SHELL(ss) = shell;
  gtk_container_add(GTK_CONTAINER(MAIN_SHELL(ss)), MAIN_PANE(ss));
#endif
  add_menu(); /* adds menubar to MAIN_PANE (via box_pack_start) */
  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      SOUND_PANE(ss) = gtk_vpaned_new();
      gtk_container_set_border_width(GTK_CONTAINER(SOUND_PANE(ss)), 0);
      gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), SOUND_PANE(ss));
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  SOUND_PANE_BOX(ss) = gtk_notebook_new();
	  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), GTK_POS_TOP);
          SG_SIGNAL_CONNECT(SOUND_PANE_BOX(ss), "switch_page", notebook_switch_page, NULL);
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
  gtk_widget_show(MAIN_PANE(ss));
  gtk_widget_show(MAIN_SHELL(ss));
  
#ifndef SND_AS_WIDGET
  MAIN_WINDOW(ss) = MAIN_SHELL(ss)->window;
#else
  MAIN_WINDOW(ss) = gtk_widget_get_parent_window(MAIN_SHELL(ss));
#endif
  
  setup_gcs();
  make_icons_transparent(BASIC_COLOR);
  
  if (batch) gtk_widget_hide(MAIN_SHELL(ss));
  BACKGROUND_ADD(startup_funcs, NULL);
  
#if HAVE_SETJMP_H
#if MUS_TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      if (!(ss->exiting))
	snd_error_without_format(_("Caught seg fault (will try to continue):\n"));
      else
	{
	  snd_error_without_format(_("Caught seg fault while trying to exit.\n"));
	  exit(0);
	}
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error_without_format(_("Caught top level error (will try to continue):\n"));
      else ss->jump_ok = false;
    }
#endif
  
#ifndef SND_AS_WIDGET
  set_up_icon();
  gtk_main();
#else
  return(shell);
#endif
}
 
 void g_init_gxmain(void)
 {
#if HAVE_EXTENSION_LANGUAGE
#define H_window_property_changed_hook S_window_property_changed_hook "(command): called upon receipt of a change in SND_COMMAND (an X window property)"
  window_property_changed_hook = XEN_DEFINE_HOOK(S_window_property_changed_hook, 1, H_window_property_changed_hook);
#endif
}
