/* TODO: other outer choice here ... (separate windows)
 *       also key_press event to graph_key_press
 *       sash color (currently basically invisible)
 *       debug X property support -- appears to be missing property notify events?
 */

/* DIFF: no .Xdefaults -- use ~/.sndrc 
 *       no directory pre-read
 */

#include "snd.h"

#if defined(NEXT) || defined(HAVE_SYS_DIR_H)
  #include <sys/dir.h>
  #include <sys/dirent.h>
#else
  #if defined(WINDOZE) && (!(defined(__CYGWIN__)))
    #include <direct.h>
  #else
    #include <dirent.h>
  #endif
#endif

#ifndef WINDOZE
  #define INIT_FILE_NAME "~/.snd"
#else
  #define INIT_FILE_NAME "snd-init"
#endif
#define RC_FILE_NAME "~/.sndrc"
#define EPS_FILE_NAME "snd.eps"
#define FALLBACK_FONT "fixed"
#define HIGHLIGHT_COLOR      "ivory1"
#define BASIC_COLOR          "ivory2"
#define POSITION_COLOR       "ivory3"
#define ZOOM_COLOR           "ivory4"
#define CURSOR_COLOR         "red"
#define SELECTION_COLOR      "lightsteelblue1"
#define MIX_COLOR            "lightgreen"
#define MIX_FOCUS_COLOR      "yellow2"
#define ENVED_WAVEFORM_COLOR "blue"
#define MIX_WAVEFORM_COLOR   "darkgray"
#define GRAPH_COLOR          "white"
#define SELECTED_GRAPH_COLOR "white"
#define DATA_COLOR           "black"
#define SELECTED_DATA_COLOR  "black"
#define MARK_COLOR           "red"
#define LISTENER_COLOR       "AliceBlue"
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

#define DEFAULT_SPECTROGRAM_COLOR -1 /* -1 = just lines or index */
#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 10
#define ENVED_POINT_SIZE 10
#define NOTEBOOK_BINDING_WIDTH 20

#define NO_ICON 0
#define PLAIN_ICON 1
#define XPM_ICON 2

#define TINY_FONT "6x12"

/* we assume later that we can always find these fonts (if resource file gives bogus entry, we fall back on these) */
#ifdef SGI
  #define DEFAULT_BUTTON_FONT "-*-times-medium-r-*-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_BOLD_BUTTON_FONT "-*-times-bold-r-*-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_LABEL_FONT "-*-times-medium-r-normal-*-20-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_NUMBERS_FONT "-*-courier-medium-r-normal-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_HELP_TEXT_FONT "9x15"
#else
#if defined(LINUX) || defined(SCO5) || defined(UW2) || defined(SOLARIS) || defined(HPUX) || defined(ALPHA)
  #define DEFAULT_BUTTON_FONT "-*-times-medium-r-*-*-12-*-*-*-*-*-iso8859-1"
  #define DEFAULT_BOLD_BUTTON_FONT "-*-times-bold-r-*-*-12-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_LABEL_FONT "-*-times-medium-r-*-*-16-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_NUMBERS_FONT "-*-courier-medium-r-*-*-12-*-*-*-*-*-iso8859-1"
  #define DEFAULT_HELP_TEXT_FONT "8x13"
#else
  #define DEFAULT_BUTTON_FONT "-*-times-medium-r-*-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_BOLD_BUTTON_FONT "-*-times-bold-r-*-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_LABEL_FONT "-*-times-medium-r-*-*-20-*-*-*-*-*-iso8859-1"
  #define DEFAULT_AXIS_NUMBERS_FONT "-*-courier-medium-r-*-*-14-*-*-*-*-*-iso8859-1"
  #define DEFAULT_HELP_TEXT_FONT "9x15"
#endif
#endif

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#define TOGGLE_SIZE 15
#define CHANNEL_MIN_HEIGHT 150
#define SASH_SIZE 14
#define SASH_INDENT -6
#define AUTO_RESIZE_DEFAULT 1

#ifndef SND_AS_WIDGET
static gint Window_Close(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  snd_exit_cleanly(ss);
  snd_exit(0);
  return(FALSE);
}
#endif

static gint corruption_check(gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  if (corruption_time(ss) > 0.0)
    {
      if ((!(play_in_progress())) && (!(record_in_progress())))
	{
	  map_over_sounds(ss,snd_not_current,NULL);
	}
      gtk_timeout_add((guint32)(corruption_time(ss)*1000),corruption_check,clientData);
    }
  return(0);
}

void add_dialog(snd_state *ss, GtkWidget *dialog)
{
  state_context *sx;
  int i;
  sx = ss->sgx;
  if (sx->dialog_list_size == 0)
    {
      sx->dialog_list_size = 8;
      sx->dialogs = (GtkWidget **)CALLOC(sx->dialog_list_size,sizeof(GtkWidget *));
      sx->ndialogs = 0;
    }
  else
    {
      if (sx->ndialogs == sx->dialog_list_size)
	{
	  sx->dialog_list_size *= 2;
	  sx->dialogs = (GtkWidget **)REALLOC(sx->dialogs,sx->dialog_list_size * sizeof(GtkWidget *));
	  for (i=sx->ndialogs;i<sx->dialog_list_size;i++) sx->dialogs[i] = NULL;
	}
    }
  sx->dialogs[sx->ndialogs] = dialog;
  sx->ndialogs++;
}

void dismiss_all_dialogs(snd_state *ss)
{
  state_context *sx;
  int i;
  sx = ss->sgx;
  if (record_dialog_is_active()) close_recorder_audio();
  if (sx->dialog_list_size > 0)
    {
      for (i=0;i<sx->ndialogs;i++)
	{
	  if (sx->dialogs[i])
	    {
	      if (GTK_WIDGET_VISIBLE(sx->dialogs[i])) gtk_widget_hide(sx->dialogs[i]); 
	    }
	}
    }
}

#ifndef SND_AS_WIDGET
static gint iconify_window(GtkWidget *w,GdkEvent *event,gpointer clientData) 
{ 
  dismiss_all_dialogs((snd_state *)clientData);
  return(FALSE);
}
#endif

static GdkAtom snd_v,snd_c;

static void who_called(GtkWidget *w,GdkEvent *event, gpointer clientData) 
{
  /* watch for communication from some other program via the SND_COMMAND property */

  /* this is broken */

  GdkEventProperty *ev = (GdkEventProperty *)event;
  snd_state *ss = (snd_state *)clientData;
  GdkAtom type;
  gint format,nitems;
  guchar *version[1];
  if (ev->atom == snd_c)
    {
      if (gdk_property_get(MAIN_WINDOW(ss),snd_c,GDK_TARGET_STRING,0L,(long)BUFSIZ,FALSE,
			       &type,&format,&nitems,(guchar **)version))
	{
	  if (version[0])
	    {
	      snd_eval_listener_str(ss,version[0]);
	      free(version[0]);
	    }
	}
    }
}

#if TRAP_SEGFAULT
#include <setjmp.h>
/* stolen from scwm.c */
static jmp_buf envHandleEventsLoop;

static RETSIGTYPE segv(int ignored)
{
  siglongjmp(envHandleEventsLoop,1);
}
#endif

static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static int noglob = 0, noinit = 0;
static char *startup_filename = NULL;
static gint stdin_id = 0;

static void GetStdinString (gpointer clientData, gint fd, GdkInputCondition condition)
{
  int bytes,size;
  char *buf;
  buf = (char *)CALLOC(1024,sizeof(char));
  size=1024;
  bytes = read(fd,buf,1024);
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
	  size+=1024;
	  buf = (char *)REALLOC(buf,size);
	  bytes = read(fd,(char *)(buf+size-1024),1024);
	}
      snd_eval_stdin_str((snd_state *)clientData,buf);
    }
  FREE(buf);
}

typedef struct {int slice; snd_state *ss; GtkWidget *shell;} startup_state;

static BACKGROUND_TYPE startup_funcs(gpointer clientData)
{
  startup_state *tm = (startup_state *)clientData;
  snd_info *sp;
  snd_state *ss;
  chan_info *cp;
  axis_info *ap;
  float apsx,apzx,apsy,apzy;
  char *argname;
  int i;
  static int auto_open_ctr = 0;
  ss = tm->ss;
  switch (tm->slice)
    {
    case 0:
#ifndef SND_AS_WIDGET
#ifndef __alpha__
      intern_atoms(ss);
      InitializeDrop(ss);
#endif
#endif
#ifndef SND_AS_WIDGET
#ifndef NEXT

      /* add X property level communication path (see sndctrl.c for the other side) */
      snd_v = gdk_atom_intern("SND_VERSION",FALSE);
      snd_c = gdk_atom_intern("SND_COMMAND",FALSE);
      gdk_property_change(MAIN_WINDOW(ss),snd_v,GDK_TARGET_STRING,8,GDK_PROP_MODE_REPLACE,SND_VERSION,strlen(SND_VERSION)+1);
      gtk_signal_connect(GTK_OBJECT(tm->shell),"property_notify_event",GTK_SIGNAL_FUNC(who_called),(gpointer)ss);
      /* does gtk actually handle this event correctly?? */

      /* trap outer-level Close for cleanup check */
      gtk_signal_connect(GTK_OBJECT(tm->shell),"delete_event",GTK_SIGNAL_FUNC(Window_Close),(gpointer)ss);
#endif
      gtk_signal_connect(GTK_OBJECT(tm->shell),"unmap_event",GTK_SIGNAL_FUNC(iconify_window),(gpointer)ss);
#endif
      (ss->sgx)->graph_cursor = gdk_cursor_new(in_graph_cursor(ss));
      (ss->sgx)->mix_cursor = gdk_cursor_new(GDK_LEFT_PTR);
      (ss->sgx)->wait_cursor = gdk_cursor_new(GDK_WATCH);
      (ss->sgx)->arrow_cursor = gdk_cursor_new(GDK_LEFT_PTR);
#if HAVE_SIGNAL
      signal(SIGTTIN,SIG_IGN);
      signal(SIGTTOU,SIG_IGN);
      /* these signals are sent by a shell if we start Snd as a background process,
       * but try to read stdin (needed to support the emacs subjob connection).  If
       * we don't do this, the background job is suspended when the shell sends SIGTTIN.
       */
      stdin_id = gdk_input_add(fileno(stdin),GDK_INPUT_READ,GetStdinString,(gpointer)ss);
#endif
      break;
    case 1: 
      gtk_rc_parse(RC_FILE_NAME); /* whatever ... */
      snd_load_init_file(ss,noglob,noinit);
      break;
    case 2: 
      if (auto_open_files > 0)
	{
	  argname = auto_open_file_names[auto_open_ctr];
	  if (argname)
	    { /* wanted to use "-d" and "-i" (or "-s") but they're in use */
	      if ((strcmp("-h",argname) == 0) || 
		  (strcmp("-horizontal",argname) == 0) ||
		  (strcmp("-v",argname) == 0) || 
		  (strcmp("-vertical",argname) == 0) ||
		  (strcmp("-notebook",argname) == 0) ||
		  (strcmp("-separate",argname) == 0) ||
		  (strcmp("-noglob",argname) == 0) ||
		  (strcmp("-noinit",argname) == 0))
		auto_open_ctr++; 
	      else
		{
		if (strcmp("-title",argname) == 0) 
		  {
		    ss->startup_title = copy_string(auto_open_file_names[auto_open_ctr+1]);
		    auto_open_ctr+=2;
		  }
		else
		  {
		    if ((strcmp("-p",argname) == 0) ||
			(strcmp("-preload",argname) == 0))
		      {
			/* preload sound files in dir (can be ., should be unquoted) */
			auto_open_ctr++;
			add_directory_to_prevlist(ss,auto_open_file_names[auto_open_ctr]);
			auto_open_ctr++;
		      }
		    else
		      {
			if ((strcmp("-l",argname) == 0) ||
			    (strcmp("-load",argname) == 0))
			  {
			    /* grab session name -- if arg is "." grab latest on this directory */
			    auto_open_ctr++;
			    snd_load_file(ss,auto_open_file_names[auto_open_ctr]);
			    auto_open_ctr++;
			  }
			else
			  {
			    if ((strcmp("-e",argname) == 0) ||
				(strcmp("-eval",argname) == 0))
			      {
				/* evaluate expression */
				auto_open_ctr++;
				snd_eval_str(ss,auto_open_file_names[auto_open_ctr],1);
				auto_open_ctr++;
			      }
			    else
			      {
				if (startup_filename == NULL) 
				  {
				    startup_filename = copy_string(argname);
				    if (dont_start(ss,startup_filename)) snd_exit(1);
				  }
				sp = snd_open_file_unselected(argname,ss);
				auto_open_ctr++;
				if ((sp) && (auto_open_ctr < auto_open_files))
				  {
				    if (strcmp("-s",auto_open_file_names[auto_open_ctr]) == 0)
				      {
					/* start up info follows -- [sx,sy,zx,zy] ... (per chan) */
					auto_open_ctr++;
					for (i=0;i<sp->nchans;i++)
					  {
					    cp = sp->chans[i];
					    ap = cp->axis;
					    sscanf(auto_open_file_names[auto_open_ctr],"%f,%f,%f,%f",&apsx,&apsy,&apzx,&apzy);
					    ap->sx = (Float)apsx; 
					    ap->zx = (Float)apzx;
					    ap->sy = (Float)apsy;
					    ap->zy = (Float)apzy;
					    set_xy_bounds(cp,ap);
					    auto_open_ctr++;
					  }}}}}}}}}
	  if (auto_open_ctr < auto_open_files) return(BACKGROUND_CONTINUE); /* i.e. come back to this branch */
	}
      break;
    case 3:
#ifndef SND_AS_WIDGET
      if ((ss->init_window_width > 0) && (ss->init_window_height > 0))
	set_widget_size(GTK_WIDGET(MAIN_SHELL(ss)),ss->init_window_width,ss->init_window_height);
      else
	{
	  if (ss->init_window_width > 0) set_widget_width(MAIN_SHELL(ss),ss->init_window_width);
	  if (ss->init_window_height > 0) set_widget_height(MAIN_SHELL(ss),ss->init_window_height);
	}
      if ((ss->init_window_x != -1) && (ss->init_window_y != -1))
	set_widget_position(GTK_WIDGET(MAIN_SHELL(ss)),ss->init_window_x,ss->init_window_y);
      else
	{
	  if (ss->init_window_x != -1) set_widget_x(MAIN_SHELL(ss),ss->init_window_x);
	  if (ss->init_window_y != -1) set_widget_y(MAIN_SHELL(ss),ss->init_window_y);
	}
#endif
      gtk_timeout_add((guint32)(corruption_time(ss)*1000),corruption_check,(gpointer)ss);
      break;
    case 4: 
#if TRAP_SEGFAULT
      if (trap_segfault(ss)) signal(SIGSEGV,segv);
#endif
      if ((ss->sounds) && (ss->sounds[0]) && ((ss->sounds[0])->inuse))
	select_channel(ss->sounds[0],0);
      FREE(tm);
      return(BACKGROUND_QUIT); 
      break;
    }
  tm->slice++;
  return(BACKGROUND_CONTINUE);
}

#ifndef SND_AS_WIDGET
static void SetupIcon(GtkWidget *shell)
{
  GdkPixmap *pix;
  GdkBitmap *mask;
  snd_state *ss;
  ss = get_global_state();
  pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss),&mask,
				     (ss->sgx)->white,
				     snd_icon_bits());
  gdk_window_set_icon(MAIN_WINDOW(ss),NULL,pix,mask);
}
#endif

static GdkColor *get_color(char *defined_color, char *fallback_color, char *second_fallback_color,
			   int use_white)
{
  GdkColor tmp_color;
  GdkColor *new_color;
  if ((!(gdk_color_parse(defined_color,&tmp_color))) &&
      ((!fallback_color) || (!(gdk_color_parse(fallback_color,&tmp_color)))) &&
      ((!second_fallback_color) || (!(gdk_color_parse(second_fallback_color,&tmp_color)))))
    {
      if (use_white)
	{
	  /* snd_error here can cause trouble (no error dialog or something) */
	  fprintf(stderr,STR_no_color_use_white,defined_color);
	  gdk_color_white(gdk_colormap_get_system(),&tmp_color);
	}
      else
	{
	  fprintf(stderr,STR_no_color_use_black,defined_color);
	  gdk_color_black(gdk_colormap_get_system(),&tmp_color);
	}
    }
  new_color = gdk_color_copy(&tmp_color);
  gdk_color_alloc(gdk_colormap_get_system(),new_color);
  return(new_color);
}

#ifdef SND_AS_WIDGET
GtkWidget *snd_as_widget(int argc, char **argv, GdkWindow *wn)
{
  snd_state *ss;
#else

void snd_doit(snd_state *ss,int argc, char **argv)
{
  GdkWindow *wn;
#endif
  
  GtkWidget *shell;
  int i;
  state_context *sx;
  GtkWidget *menu;
  startup_state *tm;

#ifdef SND_AS_WIDGET
  ss = snd_main(argc,argv);
#else
  gtk_init(&argc,&argv);
  gdk_set_locale();
#endif

  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = GDK_CROSSHAIR;

#ifndef SND_AS_WIDGET
  shell = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_policy(GTK_WINDOW(shell),TRUE,TRUE,FALSE); /* allow shrink or grow */
  /* TODO: keyboard policy */
#endif

  auto_open_files = argc-1;
  if (argc > 1) auto_open_file_names = (char **)(argv+1);
  ss->startup_title = copy_string("snd");

  set_sound_style(ss,SOUNDS_VERTICAL);
  for (i=1;i<argc;i++)
    {
      if ((strcmp(argv[i],"-h") == 0) || (strcmp(argv[i],"-horizontal") == 0))
	set_sound_style(ss,SOUNDS_HORIZONTAL);
      else
	if ((strcmp(argv[i],"-v") == 0) || (strcmp(argv[i],"-vertical") == 0))
	  set_sound_style(ss,SOUNDS_VERTICAL);
	else
	  if (strcmp(argv[i],"-notebook") == 0)
	    set_sound_style(ss,SOUNDS_IN_NOTEBOOK);
	  else
	    if (strcmp(argv[i],"-separate") == 0)
	      set_sound_style(ss,SOUNDS_IN_SEPARATE_WINDOWS);
	    else
	      if (strcmp(argv[i],"-noglob") == 0)
		noglob = 1;
	      else
		if (strcmp(argv[i],"-noinit") == 0)
		  noinit = 1;
    }

  ss->using_schemes = 0;
  set_auto_resize(ss,AUTO_RESIZE_DEFAULT);
  ss->zoom_slider_width = ZOOM_SLIDER_WIDTH;
  ss->position_slider_width = POSITION_SLIDER_WIDTH;
  ss->channel_sash_indent = CHANNEL_SASH_INDENT; /* not currently used */
  ss->channel_sash_size = CHANNEL_SASH_SIZE;
  ss->sash_size = SASH_SIZE;
  ss->sash_indent = SASH_INDENT;
  ss->toggle_size = TOGGLE_SIZE;
  ss->enved_point_size = ENVED_POINT_SIZE;

  ss->sgx = (state_context *)CALLOC(1,sizeof(state_context));
  sx = ss->sgx;
  sx->dialog_list_size = 0;
  sx->graph_is_active = 0;

  /* the gray shades are an attempt to get around Netscape which hogs all the colors */
  sx->white = get_color(WHITE_COLOR,NULL,NULL,TRUE);
  sx->black = get_color(BLACK_COLOR,NULL,NULL,FALSE);
  sx->light_blue = get_color(LIGHT_BLUE_COLOR,NULL,NULL,TRUE);
  sx->lighter_blue = get_color(LIGHTER_BLUE_COLOR,NULL,NULL,TRUE);
  sx->red = get_color(RED_COLOR,NULL,NULL,FALSE);
  sx->green = get_color(GREEN_COLOR,NULL,NULL,FALSE);
  sx->yellow = get_color(YELLOW_COLOR,NULL,NULL,TRUE);
  sx->highlight_color = get_color(HIGHLIGHT_COLOR,"gray90",NULL,TRUE);
  sx->basic_color = get_color(BASIC_COLOR,"gray80","gray",TRUE);
  sx->position_color = get_color(POSITION_COLOR,"gray60","gray",FALSE);
  sx->zoom_color = get_color(ZOOM_COLOR,"gray20","gray",FALSE);
  sx->cursor_color = get_color(CURSOR_COLOR,NULL,NULL,FALSE);
  sx->selection_color = get_color(SELECTION_COLOR,"gray80",NULL,FALSE);
  sx->mix_color = get_color(MIX_COLOR,NULL,NULL,FALSE);
  sx->mix_focus_color = get_color(MIX_FOCUS_COLOR,NULL,NULL,FALSE);
  sx->mix_waveform_color = get_color(MIX_WAVEFORM_COLOR,NULL,NULL,FALSE);
  sx->enved_waveform_color = get_color(ENVED_WAVEFORM_COLOR,NULL,NULL,FALSE);
  sx->filter_waveform_color = get_color(FILTER_WAVEFORM_COLOR,NULL,NULL,FALSE);
  sx->listener_color = get_color(LISTENER_COLOR,NULL,NULL,TRUE);
  sx->graph_color = get_color(GRAPH_COLOR,NULL,NULL,TRUE);
  sx->selected_graph_color = get_color(SELECTED_GRAPH_COLOR,NULL,NULL,TRUE);
  sx->data_color = get_color(DATA_COLOR,NULL,NULL,FALSE);
  sx->selected_data_color = get_color(SELECTED_DATA_COLOR,NULL,NULL,FALSE);
  sx->mark_color = get_color(MARK_COLOR,NULL,NULL,FALSE);
  sx->sash_color = get_color(SASH_COLOR,NULL,NULL,FALSE);
  sx->pushed_button_color = get_color(PUSHED_BUTTON_COLOR,NULL,NULL,FALSE);
  sx->text_focus_color = get_color(TEXT_FOCUS_COLOR,NULL,NULL,FALSE);

  if ((!(set_button_font(ss,DEFAULT_BUTTON_FONT))) &&
      (!(set_button_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,DEFAULT_BUTTON_FONT);

  if ((!(set_tiny_font(ss,TINY_FONT))) &&
      (!(set_tiny_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,TINY_FONT);

  if ((!(set_bold_button_font(ss,DEFAULT_BOLD_BUTTON_FONT))) &&
      (!(set_bold_button_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,DEFAULT_BOLD_BUTTON_FONT);

  if ((!(set_axis_label_font(ss,DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,DEFAULT_AXIS_LABEL_FONT);

  if ((!(set_axis_numbers_font(ss,DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,DEFAULT_AXIS_NUMBERS_FONT);

  if ((!(set_help_text_font(ss,DEFAULT_HELP_TEXT_FONT))) &&
      (!(set_help_text_font(ss,FALLBACK_FONT))))
    fprintf(stderr,STR_cant_find_font,DEFAULT_HELP_TEXT_FONT);

  ss->init_file = INIT_FILE_NAME;
  set_eps_file(ss,EPS_FILE_NAME);

  set_color_map(ss,DEFAULT_SPECTROGRAM_COLOR);
  set_ask_before_overwrite(ss,FALSE);

#ifdef SND_AS_WIDGET
  sx->mainpane = gtk_vbox_new(FALSE,0); /* not homogenous, spacing 0 */
  sx->mainshell = sx->mainpane;
  shell = sx->mainpane;
#else
  sx->mainshell = shell;
  sx->mainpane = gtk_vbox_new(FALSE,0); /* not homogenous, spacing 0 */
  gtk_container_add(GTK_CONTAINER(MAIN_SHELL(ss)),MAIN_PANE(ss));
  set_background(MAIN_SHELL(ss),(ss->sgx)->basic_color);
#endif
  set_background(MAIN_PANE(ss),(ss->sgx)->basic_color);

  menu = add_menu(ss);

  sx->soundpane = gtk_vpaned_new();
  gtk_paned_set_handle_size(GTK_PANED(SOUND_PANE(ss)),ss->sash_size);
  gtk_paned_set_gutter_size(GTK_PANED(SOUND_PANE(ss)),8);
  gtk_container_set_border_width(GTK_CONTAINER(SOUND_PANE(ss)),0);
  /* we need gtk 1.2.7 or later here */
  gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)),SOUND_PANE(ss));
  set_background(SOUND_PANE(ss),(ss->sgx)->basic_color);

  /* gtk_signal_connect(GTK_OBJECT(MAIN_SHELL(ss)),"key_press_event",GTK_SIGNAL_FUNC(shell_key_press),(gpointer)ss); */

  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
    {
      sx->soundpanebox = gtk_notebook_new();
      set_background(sx->soundpanebox,sx->basic_color);
      gtk_notebook_set_tab_pos(GTK_NOTEBOOK(sx->soundpanebox),GTK_POS_RIGHT);
    }
  else 
    {
      if (sound_style(ss) == SOUNDS_HORIZONTAL)
	sx->soundpanebox = gtk_hbox_new(FALSE,0);
      else sx->soundpanebox = gtk_vbox_new(FALSE,0);
    }
  gtk_paned_add1(GTK_PANED(SOUND_PANE(ss)),sx->soundpanebox);
  gtk_widget_show(sx->soundpanebox);

  gtk_widget_show(SOUND_PANE(ss));
  gtk_widget_show(MAIN_PANE(ss));
#ifndef SND_AS_WIDGET
  gtk_widget_show(MAIN_SHELL(ss));
  wn = shell->window;
#endif
  sx->mainwindow = wn;

  sx->basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->basic_gc,sx->graph_color);
  gdk_gc_set_foreground(sx->basic_gc,sx->data_color);

  sx->combined_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->combined_basic_gc,sx->graph_color);
  gdk_gc_set_foreground(sx->combined_basic_gc,sx->data_color);

  sx->mix_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->mix_gc,sx->graph_color);
  gdk_gc_set_foreground(sx->mix_gc,sx->mix_waveform_color);

  sx->cursor_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->cursor_gc,sx->graph_color);
  gc_set_foreground_xor(sx->cursor_gc,sx->cursor_color,sx->graph_color);
  gdk_gc_set_function(sx->cursor_gc,GDK_XOR);

  sx->selection_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selection_gc,sx->graph_color);
  gc_set_foreground_xor(sx->selection_gc,sx->selection_color,sx->graph_color);
  gdk_gc_set_function(sx->selection_gc,GDK_XOR);

  sx->mark_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->mark_gc,sx->graph_color);
  gc_set_foreground_xor(sx->mark_gc,sx->mark_color,sx->graph_color);
  gdk_gc_set_function(sx->mark_gc,GDK_XOR);

  sx->erase_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->erase_gc,sx->data_color);
  gdk_gc_set_foreground(sx->erase_gc,sx->graph_color);
  gdk_gc_set_function(sx->erase_gc,GDK_COPY);

  sx->selected_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_basic_gc,sx->selected_graph_color);
  gdk_gc_set_foreground(sx->selected_basic_gc,sx->selected_data_color);

  sx->selected_cursor_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_cursor_gc,sx->graph_color);
  gc_set_foreground_xor(sx->selected_cursor_gc,sx->cursor_color,sx->graph_color);
  gdk_gc_set_function(sx->selected_cursor_gc,GDK_XOR);

  sx->selected_selection_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_selection_gc,sx->graph_color);
  gc_set_foreground_xor(sx->selected_selection_gc,sx->selection_color,sx->graph_color);
  gdk_gc_set_function(sx->selected_selection_gc,GDK_XOR);

  sx->selected_mark_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_mark_gc,sx->selected_graph_color);
  gc_set_foreground_xor(sx->selected_mark_gc,sx->mark_color,sx->selected_graph_color);
  gdk_gc_set_function(sx->selected_mark_gc,GDK_XOR);

  sx->selected_erase_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->selected_erase_gc,sx->selected_data_color);
  gdk_gc_set_foreground(sx->selected_erase_gc,sx->selected_graph_color);
  gdk_gc_set_function(sx->selected_erase_gc,GDK_COPY);

  sx->fltenv_basic_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->fltenv_basic_gc,sx->basic_color);
  gdk_gc_set_foreground(sx->fltenv_basic_gc,sx->black);
  gdk_gc_set_function(sx->fltenv_basic_gc,GDK_COPY);

  sx->fltenv_data_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->fltenv_data_gc,sx->basic_color);
  gdk_gc_set_foreground(sx->fltenv_data_gc,sx->filter_waveform_color);
  gdk_gc_set_function(sx->fltenv_data_gc,GDK_COPY);

  sx->speed_gc = gdk_gc_new(wn);
  gdk_gc_set_background(sx->speed_gc,sx->basic_color);
  gdk_gc_set_foreground(sx->speed_gc,sx->black);
  gdk_gc_set_function(sx->speed_gc,GDK_COPY);

  initialize_colormap(ss);

  tm = (startup_state *)CALLOC(1,sizeof(startup_state));
  tm->slice = 0;
  tm->ss = ss;
  tm->shell = MAIN_SHELL(ss);

  gtk_idle_add(startup_funcs,(gpointer)tm);

#if TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop,1))
    snd_error("Caught seg fault. trying to continue...");
#endif

#ifndef SND_AS_WIDGET
  SetupIcon(shell);
  gtk_main();
#else
  return(shell);
#endif
}

