#include "snd.h"

#define FALLBACK_FONT "Sans 14"

#define HIGHLIGHT_COLOR      rgb_to_color(1.00, 1.00, 0.94) /* "ivory1" */
#define BASIC_COLOR          rgb_to_color(0.96, 0.96, 0.90) /* "ivory2" lightened */
#define POSITION_COLOR       rgb_to_color(0.80, 0.80, 0.75) /* "ivory3" */
#define ZOOM_COLOR           rgb_to_color(0.54, 0.54, 0.51) /* "ivory4" */
#define CURSOR_COLOR         rgb_to_color(1.0, 0.0, 0.0)    /* "red" */
#define SELECTION_COLOR      rgb_to_color(0.79, 0.88, 1.00) /* "lightsteelblue1" */
#define MIX_COLOR            rgb_to_color(0.66, 0.66, 0.66) /* "darkgray" */
#define ENVED_WAVEFORM_COLOR rgb_to_color(0.0, 0.0, 1.0)    /* "blue" */
#define GRAPH_COLOR          rgb_to_color(1.0, 1.0, 1.0)    /* "white" */
#define SELECTED_GRAPH_COLOR rgb_to_color(1.0, 1.0, 1.0)    /* "white" */
#define DATA_COLOR           rgb_to_color(0.0, 0.0, 0.0)    /* "black" */
#define SELECTED_DATA_COLOR  rgb_to_color(0.0, 0.0, 0.0)    /* "black" */
#define MARK_COLOR           rgb_to_color(1.0, 0.0, 0.0)    /* "red" */
#define LISTENER_COLOR       rgb_to_color(1.0, 1.0, 1.0)    /* "white" */ /* rgb_to_color(0.94, 0.97, 1.00) */ /* "AliceBlue" */
#define LISTENER_TEXT_COLOR  rgb_to_color(0.0, 0.0, 0.0)    /* "black" */
#define LIGHT_BLUE_COLOR     rgb_to_color(0.79, 0.88, 1.00) /* "lightsteelblue1" */
#define LIGHTER_BLUE_COLOR   rgb_to_color(0.94, 0.97, 1.00) /* "AliceBlue" */
#define WHITE_COLOR          rgb_to_color(1.0, 1.0, 1.0)    /* "white" */
#define BLACK_COLOR          rgb_to_color(0.0, 0.0, 0.0)    /* "black" */
#define GREEN_COLOR          rgb_to_color(0.00, 0.93, 0.00) /* "green2" */
#define RED_COLOR            rgb_to_color(1.0, 0.0, 0.0)    /* "red" */
#define YELLOW_COLOR         rgb_to_color(1.00, 1.00, 0.00) /* "yellow" */
#define TEXT_FOCUS_COLOR     rgb_to_color(1.0, 1.0, 1.0)    /* "white" */
#define FILTER_CONTROL_WAVEFORM_COLOR rgb_to_color(0.0, 0.0, 1.0) /* "blue" */
#define SASH_COLOR           rgb_to_color(0.56, 0.93, 0.56) /* "lightgreen" */
#define BLUE_COLOR           rgb_to_color(0.0, 0.0, 1.0);

#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 10

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#define TOGGLE_SIZE 15
#define CHANNEL_MIN_HEIGHT 150
#define SASH_SIZE 14
#define SASH_INDENT -6
#define AUTO_RESIZE_DEFAULT true

#define INITIAL_WINDOW_WIDTH 700
#define INITIAL_WINDOW_HEIGHT 300


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
  if ((!ss) || (!(ss->dialogs)))
    return(false);
  if (event->new_window_state & GDK_WINDOW_STATE_ICONIFIED)
    {
      /* presumably we are now iconified */

      if (iconify_active_dialogs) free(iconify_active_dialogs);
      iconify_active_dialogs = (GtkWidget **)calloc(ss->num_dialogs, sizeof(GtkWidget *));

      for (i = 0; i < ss->num_dialogs; i++)
	if (ss->dialogs[i])
	  {
	    if (widget_is_active(ss->dialogs[i]))
	      iconify_active_dialogs[i] = ss->dialogs[i];
	    gtk_widget_hide(ss->dialogs[i]);
	  }
    }
  else
    {
      if (!(event->new_window_state & GDK_WINDOW_STATE_ICONIFIED))
	{
	  /* this is confusing -- can I assume I've been deiconified or not?  if not, how to tell? */
	  if (iconify_active_dialogs) 
	    {
	      for (i = 0; i < ss->num_dialogs; i++)
		if (iconify_active_dialogs[i])
		  gtk_widget_show(iconify_active_dialogs[i]);

	      free(iconify_active_dialogs);
	      iconify_active_dialogs = NULL;
	    }
	}
    }
  return(false);
}
#endif



static guint auto_update_proc = 0;

static gint auto_update_check(gpointer context)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if (!(play_in_progress()))
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



#if HAVE_SETJMP_H
#include <setjmp.h>

#if MUS_TRAP_SEGFAULT
/* stolen from scwm.c */
static sigjmp_buf envHandleEventsLoop;

static void segv(int ignored)
{
  siglongjmp(envHandleEventsLoop, 1);
}
#endif

static jmp_buf top_level_jump;

void top_level_catch(int ignore);
void top_level_catch(int ignore)
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
  buf = (char *)calloc(1024, sizeof(char));
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
	  buf = (char *)realloc(buf, size);
	  bytes = read(fd, (char *)(buf + size - 1024), 1024);
	}
      snd_eval_stdin_str(buf);
    }
  free(buf);
}
#endif


static void setup_gcs(void)
{
  ss->basic_gc = gc_new();
  gc_set_background(ss->basic_gc, ss->graph_color);
  gc_set_foreground(ss->basic_gc, ss->data_color);

  ss->combined_basic_gc = gc_new();
  gc_set_background(ss->combined_basic_gc, ss->graph_color);
  gc_set_foreground(ss->combined_basic_gc, ss->data_color);

  ss->mix_gc = gc_new();
  gc_set_background(ss->mix_gc, ss->graph_color);
  gc_set_foreground(ss->mix_gc, ss->mix_color);

  ss->cursor_gc = gc_new();
  gc_set_background(ss->cursor_gc, ss->graph_color);
  gc_set_colors(ss->cursor_gc, ss->cursor_color, ss->graph_color);

  ss->selection_gc = gc_new();
  gc_set_background(ss->selection_gc, ss->graph_color);
  gc_set_colors(ss->selection_gc, ss->selection_color, ss->graph_color);

  ss->mark_gc = gc_new();
  gc_set_background(ss->mark_gc, ss->graph_color);
  gc_set_colors(ss->mark_gc, ss->mark_color, ss->graph_color);

  ss->erase_gc = gc_new();
  gc_set_background(ss->erase_gc, ss->data_color);
  gc_set_foreground(ss->erase_gc, ss->graph_color);

  ss->selected_basic_gc = gc_new();
  gc_set_background(ss->selected_basic_gc, ss->selected_graph_color);
  gc_set_foreground(ss->selected_basic_gc, ss->selected_data_color);

  ss->selected_cursor_gc = gc_new();
  gc_set_background(ss->selected_cursor_gc, ss->graph_color);
  gc_set_colors(ss->selected_cursor_gc, ss->cursor_color, ss->graph_color);

  ss->selected_selection_gc = gc_new();
  gc_set_background(ss->selected_selection_gc, ss->graph_color);
  gc_set_colors(ss->selected_selection_gc, ss->selection_color, ss->graph_color);

  ss->selected_mark_gc = gc_new();
  gc_set_background(ss->selected_mark_gc, ss->selected_graph_color);
  gc_set_colors(ss->selected_mark_gc, ss->mark_color, ss->selected_graph_color);

  ss->selected_erase_gc = gc_new();
  gc_set_background(ss->selected_erase_gc, ss->selected_data_color);
  gc_set_foreground(ss->selected_erase_gc, ss->selected_graph_color);

  ss->fltenv_basic_gc = gc_new();
  gc_set_background(ss->fltenv_basic_gc, ss->basic_color);
  gc_set_foreground(ss->fltenv_basic_gc, ss->black);

  ss->fltenv_data_gc = gc_new();
  gc_set_background(ss->fltenv_data_gc, ss->basic_color);
  gc_set_foreground(ss->fltenv_data_gc, ss->filter_control_waveform_color);

  initialize_colormap();
}


static void save_a_color(FILE *Fp, color_info *default_color, color_info *current_color, const char *ext_name)
{
#if HAVE_EXTENSION_LANGUAGE
  if ((current_color->red != default_color->red) ||
      (current_color->green != default_color->green) ||
      (current_color->blue != default_color->blue))

#if HAVE_FORTH
    fprintf(Fp, "%.3f %.3f %.3f %s set-%s drop\n", 
	    RGB_TO_FLOAT(current_color->red),
	    RGB_TO_FLOAT(current_color->green),
	    RGB_TO_FLOAT(current_color->blue),
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
	    RGB_TO_FLOAT(current_color->red),
	    RGB_TO_FLOAT(current_color->green),
	    RGB_TO_FLOAT(current_color->blue));
#endif /* not forth */
#endif /* ext lang */

  free(default_color); /* macro has rgb_to_color which allocates */
}


void save_colors(FILE *Fp)
{
  save_a_color(Fp, BASIC_COLOR, ss->basic_color, S_basic_color);
  save_a_color(Fp, CURSOR_COLOR, ss->cursor_color, S_cursor_color);
  save_a_color(Fp, DATA_COLOR, ss->data_color, S_data_color);
  save_a_color(Fp, SELECTED_DATA_COLOR, ss->selected_data_color, S_selected_data_color);
  save_a_color(Fp, HIGHLIGHT_COLOR, ss->highlight_color, S_highlight_color);
  save_a_color(Fp, POSITION_COLOR, ss->position_color, S_position_color);
  save_a_color(Fp, ZOOM_COLOR, ss->zoom_color, S_zoom_color);
  save_a_color(Fp, SELECTION_COLOR, ss->selection_color, S_selection_color);
  save_a_color(Fp, MIX_COLOR, ss->mix_color, S_mix_color);
  save_a_color(Fp, ENVED_WAVEFORM_COLOR, ss->enved_waveform_color, S_enved_waveform_color);
  save_a_color(Fp, FILTER_CONTROL_WAVEFORM_COLOR, ss->filter_control_waveform_color, S_filter_control_waveform_color);
  save_a_color(Fp, LISTENER_COLOR, ss->listener_color, S_listener_color);
  save_a_color(Fp, LISTENER_TEXT_COLOR, ss->listener_text_color, S_listener_text_color);
  save_a_color(Fp, GRAPH_COLOR, ss->graph_color, S_graph_color);
  save_a_color(Fp, SELECTED_GRAPH_COLOR, ss->selected_graph_color, S_selected_graph_color);
  save_a_color(Fp, MARK_COLOR, ss->mark_color, S_mark_color);
  save_a_color(Fp, SASH_COLOR, ss->sash_color, S_sash_color);
  save_a_color(Fp, TEXT_FOCUS_COLOR, ss->text_focus_color, S_text_focus_color);
}


#if HAVE_EXTENSION_LANGUAGE
static gboolean io_invoke(GIOChannel *source, GIOCondition condition, gpointer data)
{
  get_stdin_string(NULL, g_io_channel_unix_get_fd(source), 0);
  return(true);
}
#endif


static void startup_funcs(void)
{
  static int auto_open_ctr = 0;
  ss->file_monitor_ok = true;

#ifndef SND_AS_WIDGET
  /* trap outer-level Close for cleanup check */
  SG_SIGNAL_CONNECT(MAIN_SHELL(ss), "delete_event", window_close, NULL);
  /* when iconified, we need to hide any dialogs as well */
  SG_SIGNAL_CONNECT(MAIN_SHELL(ss), "window_state_event", window_iconify, NULL);
#endif

  ss->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(ss));
  ss->wait_cursor = gdk_cursor_new(GDK_WATCH);
  ss->bounds_cursor = gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW);
  ss->yaxis_cursor = gdk_cursor_new(GDK_SB_V_DOUBLE_ARROW);
  ss->play_cursor = gdk_cursor_new(GDK_SB_RIGHT_ARROW);
  ss->loop_play_cursor = gdk_cursor_new(GDK_SB_LEFT_ARROW);
  ss->arrow_cursor = gdk_cursor_new(GDK_LEFT_PTR);

#if HAVE_EXTENSION_LANGUAGE
  snd_load_init_file(noglob, noinit);
#endif

#if HAVE_SIGNAL && HAVE_EXTENSION_LANGUAGE && !__MINGW32__
  if (!nostdin)
    {
      GIOChannel *channel;
      signal(SIGTTIN, SIG_IGN);
      signal(SIGTTOU, SIG_IGN);
      /* these signals are sent by a shell if we start Snd as a background process,
       * but try to read stdin (needed to support the emacs subjob connection).  If
       * we don't do this, the background job is suspended when the shell sends SIGTTIN.
       */
      channel = g_io_channel_unix_new(STDIN_FILENO);
      stdin_id = g_io_add_watch_full(channel, 
				     G_PRIORITY_DEFAULT, 
				     (GIOCondition)(G_IO_IN | G_IO_HUP | G_IO_ERR), 
				     io_invoke, NULL, NULL);
      g_io_channel_unref(channel);
    }
#endif

  while (auto_open_ctr < auto_open_files)
    auto_open_ctr = handle_next_startup_arg(auto_open_ctr, auto_open_file_names, true, auto_open_files);

#ifndef SND_AS_WIDGET
  if ((ss->init_window_width > 0) && (ss->init_window_height > 0))
    set_widget_size(GTK_WIDGET(MAIN_SHELL(ss)), ss->init_window_width, ss->init_window_height);
  if ((ss->init_window_x != DEFAULT_INIT_WINDOW_X) && (ss->init_window_y != DEFAULT_INIT_WINDOW_Y))
    set_widget_position(GTK_WIDGET(MAIN_SHELL(ss)), ss->init_window_x, ss->init_window_y);
#endif

  if (!(ss->file_monitor_ok))
    {
      if (auto_update_interval(ss) > 0.0)
	g_timeout_add_full(0, (guint32)(auto_update_interval(ss) * 1000), auto_update_check, NULL, NULL);
    }

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
}


#ifndef SND_AS_WIDGET
static void set_up_icon(void)
{
  gtk_window_set_icon(GTK_WINDOW(MAIN_SHELL(ss)), gdk_pixbuf_new_from_xpm_data(snd_icon_bits()));
}
#endif


color_t get_in_between_color(color_t fg, color_t bg)
{
  color_info *new_color;
  new_color = (color_info *)calloc(1, sizeof(color_info)); /* memleak here */
  new_color->red = (rgb_t)((fg->red + (2 * bg->red)) / 3);
  new_color->green = (rgb_t)((fg->green + (2 * bg->green)) / 3);
  new_color->blue = (rgb_t)((fg->blue + (2 * bg->blue)) / 3);
  new_color->alpha = 1.0;
  return(new_color);
}


static void notebook_switch_page(GtkNotebook *w, GtkWidget *page_widget, gint page_num)
{
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
  GtkWidget *shell;
  int i;

#ifdef SND_AS_WIDGET
  set_error_display(error_func);
  ss = snd_main(argc, argv);
#else
  gtk_init(&argc, &argv);

#if (!HAVE_GTK_3)
#if (!__APPLE__)
  gdk_set_locale();
#endif
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
  ss->startup_title = mus_strdup("snd");
  set_sound_style(SOUNDS_VERTICAL);
  for (i = 1; i < argc; i++)
    if ((strcmp(argv[i], "-h") == 0) || 
	(strcmp(argv[i], "-horizontal") == 0) ||
	(strcmp(argv[i], "--horizontal") == 0))
      set_sound_style(SOUNDS_HORIZONTAL);
    else
      if ((strcmp(argv[i], "-v") == 0) || 
	  (strcmp(argv[i], "-vertical") == 0) ||
	  (strcmp(argv[i], "--vertical") == 0))
	set_sound_style(SOUNDS_VERTICAL);
      else
	if ((strcmp(argv[i], "-notebook") == 0) ||
	    (strcmp(argv[i], "--notebook") == 0))
	  set_sound_style(SOUNDS_IN_NOTEBOOK);
	else
	  if ((strcmp(argv[i], "-separate") == 0) ||
	      (strcmp(argv[i], "--separate") == 0))
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
		      (strcmp(argv[i], "-batch") == 0) ||
		      (strcmp(argv[i], "--batch") == 0))
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
  ss->graph_is_active = false;
  ss->bg_gradient = 0.05;

  ss->white =                         WHITE_COLOR;
  ss->black =                         BLACK_COLOR;
  ss->light_blue =                    LIGHT_BLUE_COLOR;
  ss->lighter_blue =                  LIGHTER_BLUE_COLOR;
  ss->red =                           RED_COLOR;
  ss->green =                         GREEN_COLOR;
  ss->blue =                          BLUE_COLOR;
  ss->yellow =                        YELLOW_COLOR;
  ss->highlight_color =               HIGHLIGHT_COLOR;
  ss->basic_color =                   BASIC_COLOR;
  ss->position_color =                POSITION_COLOR;
  ss->zoom_color =                    ZOOM_COLOR;
  ss->cursor_color =                  CURSOR_COLOR;
  ss->selection_color =               SELECTION_COLOR;
  ss->mix_color =                     MIX_COLOR;
  ss->enved_waveform_color =          ENVED_WAVEFORM_COLOR;
  ss->filter_control_waveform_color = FILTER_CONTROL_WAVEFORM_COLOR;
  ss->listener_color =                LISTENER_COLOR;
  ss->listener_text_color =           LISTENER_TEXT_COLOR;
  ss->graph_color =                   GRAPH_COLOR;
  ss->selected_graph_color =          SELECTED_GRAPH_COLOR;
  ss->data_color =                    DATA_COLOR;
  ss->selected_data_color =           SELECTED_DATA_COLOR;
  ss->mark_color =                    MARK_COLOR;
  ss->sash_color =                    SASH_COLOR;
  ss->text_focus_color =              TEXT_FOCUS_COLOR;

  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);

  ss->axis_color_set = false;

  ss->orig_data_color = ss->data_color;
  ss->orig_selected_data_color = ss->selected_data_color;
  ss->orig_mark_color = ss->mark_color;
  ss->orig_mix_color = ss->mix_color;
  ss->orig_graph_color = ss->graph_color;
  ss->orig_selected_graph_color = ss->selected_graph_color;
  ss->orig_listener_color = ss->listener_color;
  ss->orig_listener_text_color = ss->listener_text_color;
  ss->orig_cursor_color = ss->cursor_color;
  ss->orig_basic_color = ss->basic_color;
  ss->orig_selection_color = ss->selection_color;
  ss->orig_zoom_color = ss->zoom_color;
  ss->orig_position_color = ss->position_color;
  ss->orig_highlight_color = ss->highlight_color;

  if ((!(set_tiny_font(DEFAULT_TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find tiny font: %s", DEFAULT_TINY_FONT);

  if ((!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find axis label font: %s", DEFAULT_AXIS_LABEL_FONT);

  if ((!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find axis numbers font: %s", DEFAULT_AXIS_NUMBERS_FONT);

  if ((!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find peaks font: %s", DEFAULT_PEAKS_FONT);

  if ((!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find bold peaks font: %s", DEFAULT_BOLD_PEAKS_FONT);

  if ((!(set_listener_font(DEFAULT_LISTENER_FONT))) &&
      (!(set_listener_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find listener font: %s", DEFAULT_LISTENER_FONT);

  ss->orig_axis_label_font = mus_strdup(axis_label_font(ss));
  ss->orig_axis_numbers_font = mus_strdup(axis_numbers_font(ss));
  ss->orig_peaks_font = mus_strdup(peaks_font(ss));
  ss->orig_bold_peaks_font = mus_strdup(bold_peaks_font(ss));
  ss->orig_listener_font = mus_strdup(listener_font(ss));
  ss->orig_tiny_font = mus_strdup(tiny_font(ss));

#if (!HAVE_GTK_3)
  init_gtk();
#endif

  MAIN_PANE(ss) = gtk_vbox_new(false, 0); /* not homogenous, spacing 0 */

#if (HAVE_GTK_3)
  init_gtk();
#endif
  
#ifdef SND_AS_WIDGET
  MAIN_SHELL(ss) = parent;
  shell = MAIN_PANE(ss);
#else
  MAIN_SHELL(ss) = shell;
  gtk_container_add(GTK_CONTAINER(MAIN_SHELL(ss)), MAIN_PANE(ss));
#endif
  add_menu(); /* adds menubar to MAIN_PANE (via box_pack_start) */
  if (with_toolbar(ss)) show_toolbar();

  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      /* hseparator here looks bad */

      SOUND_PANE(ss) = gtk_vpaned_new();
      add_paned_style(SOUND_PANE(ss));
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
#if HAVE_GTK_3
	  gtk_widget_set_hexpand(GTK_WIDGET(SOUND_PANE_BOX(ss)), true);
	  gtk_widget_set_vexpand(GTK_WIDGET(SOUND_PANE_BOX(ss)), true);
#endif
	}
      gtk_paned_add1(GTK_PANED(SOUND_PANE(ss)), SOUND_PANE_BOX(ss));
      gtk_widget_show(SOUND_PANE_BOX(ss));
      gtk_widget_show(SOUND_PANE(ss));
    }
  gtk_widget_show(MAIN_PANE(ss));
  gtk_widget_show(MAIN_SHELL(ss));
  
#ifndef SND_AS_WIDGET
#if HAVE_GTK_ADJUSTMENT_GET_UPPER
  MAIN_WINDOW(ss) = gtk_widget_get_window(MAIN_SHELL(ss));
#else
  MAIN_WINDOW(ss) = MAIN_SHELL(ss)->window;
#endif
#else
  MAIN_WINDOW(ss) = gtk_widget_get_parent_window(MAIN_SHELL(ss));
#endif
  
  setup_gcs();
  if (batch) gtk_widget_hide(MAIN_SHELL(ss));
  else gdk_window_resize(WIDGET_TO_WINDOW(MAIN_SHELL(ss)), INITIAL_WINDOW_WIDTH, INITIAL_WINDOW_HEIGHT);
  startup_funcs();
  
#if HAVE_SETJMP_H
#if MUS_TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      if (!(ss->exiting))
	snd_error_without_format("Caught seg fault (will try to continue):\n");
      else
	{
	  snd_error_without_format("Caught seg fault while trying to exit.\n");
	  exit(0);
	}
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error_without_format("Caught top level error (will try to continue):\n");
      else ss->jump_ok = false;
    }
#endif
  
  if (ss->startup_errors)
    {
      post_it("Error in initialization", ss->startup_errors);
      free(ss->startup_errors);
      ss->startup_errors = NULL;
    }

#if HAVE_GTK_3
  set_basic_color(ss->basic_color);
  color_listener(ss->listener_color);

  color_chan_components(ss->zoom_color, COLOR_ZOOM);
  color_chan_components(ss->position_color, COLOR_POSITION);
#endif
  if ((!listener_exists()) &&
      (!(ss->sounds[0])))
    handle_listener(true);

#ifndef SND_AS_WIDGET
  set_up_icon();
  gtk_main();
#else
  return(shell);
#endif
}

