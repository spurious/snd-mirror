#include "snd.h"

static Widget recorder = NULL, meters = NULL, record_button = NULL, recorder_output = NULL;
static bool reading = false, recording = false;
static char *recorder_filename = NULL;
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static off_t recorder_total_bytes = 0;
static axis_context *recorder_ax = NULL;
static int meter_width = 0, meter_height = 0, meters_width = 0;
static bool meters_in_db = false;

static void stop_recording(void)
{
  XmString s2;
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
  recorder_fd = -1;
  recorder_total_bytes = 0;
  s2 = XmStringCreateLocalized(_("Record"));
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
  if (recording)
    stop_recording();
  reading = false;
  XtUnmanageChild(recorder);
}

static void recorder_help(Widget w, XtPointer context, XtPointer info)
{
  recording_help();
}

static void display_meters(Float *maxes)
{
  int i, x0 = 0, yc;
  /* if maxes is NULL, assume all 0's */
  if (recorder_chans == 0) return;
  if (!(recorder_ax->wn)) recorder_ax->wn = XtWindow(meters);

  XSetForeground(recorder_ax->dp, recorder_ax->gc, ss->sgx->white);
  XSetBackground(recorder_ax->dp, recorder_ax->gc, ss->sgx->white);
  XFillRectangle(recorder_ax->dp, recorder_ax->wn, recorder_ax->gc, 0, 0, meters_width, meter_height);

  XSetLineAttributes(recorder_ax->dp, recorder_ax->gc, 2, LineSolid, CapRound, JoinRound);
  XSetForeground(recorder_ax->dp, recorder_ax->gc, ss->sgx->black);

  yc = (int)(0.5 * meter_width + 0.2 * meter_height);

  for (i = 0; i < recorder_chans; i++, x0 += meter_width)
    {
      Float cur_max = 0.0, rads, xc;
      if (maxes) 
	{
	  if (!meters_in_db)
	    cur_max = maxes[i];
	  else
	    {
	      Float dv;
	      dv = in_dB(min_dB(ss), ss->lin_dB, maxes[i]);
	      cur_max = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
	    }
	}

      rads = (M_PI * 0.5 * cur_max) - (M_PI / 4);
      xc = x0 + 0.5 * meter_width;

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
  if (recorder_filename) FREE(recorder_filename);
  str = XmTextGetString(recorder_output);
  if (!str)
    recorder_filename = copy_string("test.snd");
  else 
    {
      recorder_filename = copy_string(str);
      XtFree(str);
    }
  recorder_fd = mus_sound_open_output(recorder_filename, recorder_srate, recorder_chans, recorder_format, MUS_NEXT, NULL);
  if (recorder_fd < 0)
    {
      fprintf(stderr,"open output file chans: %d, srate: %d, format: %s -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), recorder_fd);
      recording = false;
    }
  else 
    {
      XmString s2;
      recording = true;
      s2 = XmStringCreateLocalized(_("Done"));
      XtVaSetValues(record_button, XmNlabelString, s2, NULL);
      XmStringFree(s2);
    }
}

static int look_for_format (float *mixer_vals, int format)
{
  int i, lim;
  lim = (int)mixer_vals[0];
  for (i = 1; i <= lim; i++)
    if ((int)mixer_vals[i] == format)
      return(format);
  return(-1);
}

static void start_reading(void)
{
  Float *maxes;
  #define MIXER_SIZE 8
  float mixer_vals[MIXER_SIZE];
  int input_device, buffer_size, err = MUS_NO_ERROR;
  unsigned char *inbuf;

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_CHANNEL, MIXER_SIZE, mixer_vals);
  recorder_chans = (int)mixer_vals[0];
  if (recorder_chans > 4) recorder_chans = 8;
  if (recorder_chans <= 0)
    {
      fprintf(stderr,"chans: %d?\n", recorder_chans);
      reading = false;
      return;
    }

  /*
  mixer_vals[0] = 44100.0;
  mus_audio_mixer_write(MUS_AUDIO_DEFAULT, MUS_AUDIO_SRATE, 0, mixer_vals);
  */
  recorder_srate = 44100;

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_FORMAT, MIXER_SIZE, mixer_vals);
#if MUS_LITTLE_ENDIAN
  recorder_format = look_for_format(mixer_vals, MUS_LFLOAT);
  if (recorder_format == -1)
    {
      recorder_format = look_for_format(mixer_vals, MUS_LSHORT);
      if (recorder_format == -1)
	recorder_format = (int)mixer_vals[1];
    }
#else
  recorder_format = look_for_format(mixer_vals, MUS_BFLOAT);
  if (recorder_format == -1)
    {
      recorder_format = look_for_format(mixer_vals, MUS_BSHORT);
      if (recorder_format == -1)
	recorder_format = (int)mixer_vals[1];
    }
#endif

  buffer_size = 4096;
  
  input_device = mus_audio_open_input(MUS_AUDIO_DEFAULT, recorder_srate, recorder_chans, recorder_format, buffer_size);
  if (input_device < 0)
    {
      fprintf(stderr,"open input failed: chans: %d, srate: %d, format: %s, size: %d -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), buffer_size, input_device);

      reading = false;
      return;
    }

  maxes = (Float *)CALLOC(recorder_chans, sizeof(Float));
  inbuf = (unsigned char *)CALLOC(buffer_size, sizeof(unsigned char));

  reading = true;
  while (true)
    {
      err = mus_audio_read(input_device, (char *)inbuf, buffer_size);
      if (err != MUS_NO_ERROR) break;
      if (recording)
	{
	  write(recorder_fd, (char *)inbuf, buffer_size);
	  recorder_total_bytes += buffer_size;
	}
      check_for_event();  /* watch for close event or "go away" clicked */
      if (!reading) break;
      err = mus_samples_peak(inbuf, buffer_size, recorder_chans, recorder_format, maxes);
      if (err != MUS_ERROR) 
	display_meters(maxes);
      else fprintf(stderr,"mus samples peak: %d\n", err);
    }
  if (err != MUS_NO_ERROR)
    fprintf(stderr,"error: %s\n", mus_error_type_to_string(err));
  else fprintf(stderr,"done\n");

  mus_audio_close(input_device);
  FREE(inbuf);
  FREE(maxes);
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

      xquit = XmStringCreateLocalized(_("Go Away"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xrecord = XmStringCreateLocalized(_("Record"));
      xtitle = XmStringCreateLocalized(_("Record"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xquit); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xrecord); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_ANY); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      XtSetArg(args[n], XmNheight, 175); n++;
      recorder = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Record"), args, n);

      XtAddCallback(recorder, XmNcancelCallback, quit_recorder, NULL);
      XtAddCallback(recorder, XmNhelpCallback, recorder_help, NULL);
      XtAddCallback(recorder, XmNokCallback, start_or_stop_recorder, NULL);

      XmStringFree(xhelp);
      XmStringFree(xquit);
      XmStringFree(xrecord);
      XmStringFree(xtitle);

      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color, NULL);

      recorder_filename = copy_string("test.snd");

      {
	Widget form, dl, db_button, sep;
	n = 0;
	form = XtCreateManagedWidget("form", xmFormWidgetClass, recorder, args, n);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
	XtSetArg(args[n], XmNmarginWidth, 8); n++;
	db_button = make_togglebutton_widget(_("dB"), form, args, n);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	dl = XtCreateManagedWidget(_("file:"), xmLabelWidgetClass, form, args, n);

	n = 0;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNleftWidget, dl); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNrightWidget, db_button); n++;
	XtSetArg(args[n], XmNvalue, recorder_filename); n++;
	recorder_output = make_textfield_widget("text", form, args, n, ACTIVATABLE, NO_COMPLETER);

	n = 0;
	XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, db_button); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
	XtSetArg(args[n], XmNheight, 10); n++;
	sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, form, args, n);

	n = 0;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, sep); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	meters = XtCreateManagedWidget("meters", xmDrawingAreaWidgetClass, form, args, n);
	
	XtAddCallback(db_button, XmNvalueChangedCallback, db_callback, NULL);
      }

      recorder_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      {
	XGCValues gv;
	gv.function = GXcopy;
	XtVaGetValues(meters, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
	recorder_ax->gc = XtGetGC(meters, GCForeground | GCFunction, &gv);
      }

      wm_delete = XmInternAtom(XtDisplay(recorder), "WM_DELETE_WINDOW", false);
      XmAddWMProtocolCallback(XtParent(recorder), wm_delete, close_recorder, NULL);

      XtManageChild(recorder);
      map_over_children(recorder, set_main_color_of_widget);
      set_dialog_widget(RECORDER_DIALOG, recorder);

      recorder_ax->wn = XtWindow(meters);
      recorder_ax->dp = XtDisplay(meters);

      XtAddCallback(meters, XmNresizeCallback, meters_resize, NULL);
      XtAddCallback(meters, XmNexposeCallback, meters_resize, NULL);
      
    }
  else 
    {
      if (!XtIsManaged(recorder)) XtManageChild(recorder);
      raise_dialog(recorder);
    }

  start_reading();
  return(recorder);
}


