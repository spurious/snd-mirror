#include "snd.h"

static Widget recorder = NULL, meters = NULL, record_button = NULL, recorder_output = NULL;
static bool reading = false, recording = false;
static char *recorder_filename = NULL;
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static mus_long_t recorder_total_bytes = 0;
static graphics_context *recorder_ax = NULL;
static int meter_width = 0, meter_height = 0, meters_width = 0;
static bool meters_in_db = false;
static Widget file_label;

#define RECORDER_HEIGHT 300


/* error handling */

static bool record_error_watching = false;
static Widget error_info;

static void clear_record_error(void);

static void watch_record_error(Widget w, XtPointer context, XtPointer info)
{
  clear_record_error();
}


static char *base_message(void)
{
  return(mus_format("srate: %d, chans: %d", recorder_srate, recorder_chans));
}


static void clear_record_error(void)
{
  XmString s1; 
  char *bmsg;
  XtVaSetValues(error_info, XmNbackground, ss->basic_color, NULL);
  bmsg = base_message();
  s1 = XmStringCreateLocalized(bmsg);
  free(bmsg);
  XtVaSetValues(error_info, XmNlabelString, s1, NULL);
  XmStringFree(s1);
  if (record_error_watching)
    {
      record_error_watching = false;
      XtRemoveCallback(recorder_output, XmNvalueChangedCallback, watch_record_error, NULL);
    }
}


static void report_in_error_info(const char *msg, void *ignore)
{
  XmString s1;
  if ((!msg) || (!(*msg))) return;
  XtVaSetValues(error_info, XmNbackground, ss->highlight_color, NULL);
  s1 = XmStringCreateLocalized((char *)msg);
  XtVaSetValues(error_info, XmNlabelString, s1, NULL);
  record_error_watching = true;
  XtAddCallback(recorder_output, XmNvalueChangedCallback, watch_record_error, NULL);
  XmStringFree(s1);
}


static void stop_recording(void)
{
  XmString s2;
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
  recorder_fd = -1;
  recorder_total_bytes = 0;
  s2 = XmStringCreateLocalized((char *)"Record");
  XtVaSetValues(record_button, XmNlabelString, s2, NULL);
  XmStringFree(s2);
}


static void close_recorder(Widget w, XtPointer context, XtPointer info)
{
  /* window manager close button */
  if (recording)
    stop_recording();
  reading = false;
  XtUnmanageChild(recorder);
}


static void quit_recorder(Widget w, XtPointer context, XtPointer info) 
{
  /* Quit button in the recorder dialog */
  clear_record_error();
  if (recording)
    stop_recording();
  reading = false;
  XtUnmanageChild(recorder);
}


static void recorder_help(Widget w, XtPointer context, XtPointer info)
{
  recording_help();
}


static void display_meters(mus_float_t *maxes)
{
  int i, x0 = 0, yc;
  /* if maxes is NULL, assume all 0's */
  if (recorder_chans == 0) return;
  if (!(recorder_ax->wn)) recorder_ax->wn = XtWindow(meters);

  XSetForeground(recorder_ax->dp, recorder_ax->gc, ss->white);
  XSetBackground(recorder_ax->dp, recorder_ax->gc, ss->white);
  XFillRectangle(recorder_ax->dp, recorder_ax->wn, recorder_ax->gc, 0, 0, meters_width, meter_height);

  XSetLineAttributes(recorder_ax->dp, recorder_ax->gc, 2, LineSolid, CapRound, JoinRound);
  XSetForeground(recorder_ax->dp, recorder_ax->gc, ss->black);

  yc = (int)(0.5 * meter_width + 0.2 * meter_height);

  for (i = 0; i < recorder_chans; i++, x0 += meter_width)
    {
      mus_float_t cur_max = 0.0, rads;
      int xc;
      if (maxes) 
	{
	  if (!meters_in_db)
	    cur_max = maxes[i];
	  else
	    {
	      mus_float_t dv;
	      dv = in_dB(min_dB(ss), ss->lin_dB, maxes[i]);
	      cur_max = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
	    }
	}

      rads = (M_PI * 0.5 * cur_max) - (M_PI / 4);
      xc = (int)(x0 + 0.5 * meter_width);

      XDrawArc(recorder_ax->dp, recorder_ax->wn, recorder_ax->gc, x0, 20, meter_width, meter_width, 45 * 64, 90 * 64);
      XDrawLine(recorder_ax->dp, recorder_ax->wn, recorder_ax->gc, 
		xc, yc, 
		(int)(xc + 0.55 * meter_width * sin(rads)),
		(int)(yc - 0.55 * meter_width * cos(rads)));
    }
}


static void start_recording(void)
{
  char *str;
  clear_record_error();
  if (recorder_filename) free(recorder_filename);
  str = XmTextGetString(recorder_output);
  if (!str)
    recorder_filename = mus_strdup("test.snd");
  else 
    {
      recorder_filename = mus_strdup(str);
      XtFree(str);
    }
  redirect_snd_error_to(report_in_error_info, NULL);
  recorder_fd = mus_sound_open_output(recorder_filename, recorder_srate, recorder_chans, recorder_format, 
				      (recorder_format == MUS_LFLOAT) ? MUS_RIFF : MUS_NEXT, 
				      NULL);
  redirect_snd_error_to(NULL, NULL);
  if (recorder_fd < 0)
    {
      recording = false;
    }
  else 
    {
      XmString s2;
      recording = true;
      s2 = XmStringCreateLocalized((char *)"Done");
      XtVaSetValues(record_button, XmNlabelString, s2, NULL);
      XmStringFree(s2);
    }
}


static void start_reading(void)
{
  mus_float_t *maxes;
  int input_device, buffer_size, err = MUS_NO_ERROR;
  unsigned char *inbuf;

  clear_record_error();

  recorder_chans = mus_audio_device_channels(MUS_AUDIO_DEFAULT);
  if (recorder_chans > 4) recorder_chans = 8;
  if (recorder_chans <= 0)
    {
      char *msg;
      msg = mus_format("chans: %d?\n", recorder_chans);
      report_in_error_info(msg, NULL);
      free(msg);
      reading = false;
      return;
    }

  recorder_srate = 44100;
  recorder_format = mus_audio_device_format(MUS_AUDIO_DEFAULT);  
  buffer_size = 4096;
  
  input_device = mus_audio_open_input(MUS_AUDIO_DEFAULT, recorder_srate, recorder_chans, recorder_format, buffer_size);
  if (input_device < 0)
    {
      char *msg;
      msg = mus_format("open input failed: chans: %d, srate: %d, format: %s, size: %d -> %d\n", 
		       recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), buffer_size, input_device);
      report_in_error_info(msg, NULL);
      free(msg);
      reading = false;
      return;
    }

  maxes = (mus_float_t *)calloc(recorder_chans, sizeof(mus_float_t));
  inbuf = (unsigned char *)calloc(buffer_size, sizeof(unsigned char));

  reading = true;
  while (true)
    {
      err = mus_audio_read(input_device, (char *)inbuf, buffer_size);
      if (err != MUS_NO_ERROR) break;
      if (recording)
	{
	  ssize_t bytes;
	  bytes = write(recorder_fd, (char *)inbuf, buffer_size);
	  if (bytes != buffer_size)
	    {
	      char *msg;
	      msg = mus_format("recorder wrote " SSIZE_TD " bytes of %d requested?", bytes, buffer_size);
	      report_in_error_info(msg, NULL);
	      free(msg);
	    }
	  recorder_total_bytes += buffer_size;
	}
      check_for_event();  /* watch for close event or "go away" clicked */
      if (!reading) break;
      err = mus_samples_peak(inbuf, buffer_size, recorder_chans, recorder_format, maxes);
      if (err != MUS_ERROR) 
	display_meters(maxes);
      else 
	{
	  char *msg;
	  msg = mus_format("data conversion problem; format is %s\n", mus_data_format_name(recorder_format));
	  report_in_error_info(msg, NULL);
	  free(msg);
	  err = MUS_NO_ERROR;
	  break;
	}
    }

  if (err != MUS_NO_ERROR)
    {
      char *msg;
      msg = mus_format("error: %s\n", mus_error_type_to_string(err));
      report_in_error_info(msg, NULL);
      free(msg);
    }

  mus_audio_close(input_device);
  free(inbuf);
  free(maxes);
}


static void start_or_stop_recorder(Widget w, XtPointer context, XtPointer info)
{
  if (recording)
    stop_recording();
  else start_recording();
}


static void meters_resize(Widget w, XtPointer context, XtPointer info)
{
  if (recorder_chans == 0) return;
  meters_width = widget_width(meters);
  meter_width = meters_width / recorder_chans;
  meter_height = widget_height(meters);
  display_meters(NULL);
}


static void db_callback(Widget w, XtPointer context, XtPointer info)
{
  meters_in_db = (bool)XmToggleButtonGetState(w);
}


widget_t record_file(void) 
{
  if (!recorder)
    {
      Arg args[32];
      int n;
      XmString xquit, xhelp, xrecord, xtitle;
      Atom wm_delete;

      xquit = XmStringCreateLocalized((char *)"Go Away");
      xhelp = XmStringCreateLocalized((char *)"Help");
      xrecord = XmStringCreateLocalized((char *)"Record");
      xtitle = XmStringCreateLocalized((char *)"Record");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xquit); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xrecord); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_ANY); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      XtSetArg(args[n], XmNheight, RECORDER_HEIGHT); n++;
      recorder = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Record", args, n);

      record_button = XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON);

      XtAddCallback(recorder, XmNcancelCallback, quit_recorder, NULL);
      XtAddCallback(recorder, XmNhelpCallback, recorder_help, NULL);
      XtAddCallback(recorder, XmNokCallback, start_or_stop_recorder, NULL);

      XmStringFree(xhelp);
      XmStringFree(xquit);
      XmStringFree(xrecord);
      XmStringFree(xtitle);

      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color, NULL);

      recorder_filename = mus_strdup("test.snd");

      {
	Widget sep, sep1, form, db_button, error_info_box, error_info_frame;

	n = 0;
	form = XtCreateManagedWidget("form", xmFormWidgetClass, recorder, args, n);

	/* error/info display */
	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNallowResize, true); n++; 
	XtSetArg(args[n], XmNmargin, 20); n++;
	error_info_box = XtCreateManagedWidget("error-box", xmRowColumnWidgetClass, form, args, n);
	
	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNmarginHeight, 4); n++;
	error_info_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, error_info_box, args, n);
	
	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
	error_info = XtCreateManagedWidget("error-info", xmLabelWidgetClass, error_info_frame, args, n);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, error_info_box); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
	XtSetArg(args[n], XmNheight, 10); n++;
	sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, form, args, n);


	/* dB button, filename */
	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, sep1); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
	XtSetArg(args[n], XmNmarginWidth, 8); n++;
	db_button = make_togglebutton_widget("dB", form, args, n);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, sep1); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	file_label = XtCreateManagedWidget("file:", xmLabelWidgetClass, form, args, n);

	n = 0;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNleftWidget, file_label); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, sep1); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNrightWidget, db_button); n++;
	XtSetArg(args[n], XmNvalue, recorder_filename); n++;
	recorder_output = make_textfield_widget("text", form, args, n, ACTIVATABLE, NO_COMPLETER);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, recorder_output); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
	XtSetArg(args[n], XmNheight, 10); n++;
	sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, form, args, n);


	/* meters */
	n = 0;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, sep); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNheight, 50); n++;
	meters = XtCreateManagedWidget("meters", xmDrawingAreaWidgetClass, form, args, n);
	
	XtAddCallback(db_button, XmNvalueChangedCallback, db_callback, NULL);
      }

      recorder_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      {
	XGCValues gv;
	gv.function = GXcopy;
	XtVaGetValues(meters, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
	recorder_ax->gc = XtGetGC(meters, GCForeground | GCFunction, &gv);
      }

      wm_delete = XmInternAtom(XtDisplay(recorder), (char *)"WM_DELETE_WINDOW", false);
      XmAddWMProtocolCallback(XtParent(recorder), wm_delete, close_recorder, NULL);

      XtManageChild(recorder);
      map_over_children(recorder, set_main_color_of_widget);
      set_dialog_widget(RECORDER_DIALOG, recorder);

      recorder_ax->wn = XtWindow(meters);
      recorder_ax->dp = XtDisplay(meters);

      XtAddCallback(meters, XmNresizeCallback, meters_resize, NULL);
      XtAddCallback(meters, XmNexposeCallback, meters_resize, NULL);
      
      XtVaSetValues(recorder, XmNheight, RECORDER_HEIGHT, NULL);
    }
  else 
    {
      if (!XtIsManaged(recorder)) XtManageChild(recorder);
      raise_dialog(recorder);
    }

  start_reading();
  return(recorder);
}


