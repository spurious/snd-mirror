#include "snd.h"

/* -------- region browser -------- */

static GtkWidget *region_dialog = NULL, *region_list, *region_grf;
static regrow **region_rows = NULL;
static int region_rows_size = 0;
static snd_info *reg_sp = NULL;
static int current_region = -1;
static GtkWidget *srate_text, *length_text, *chans_text, *maxamp_text;
static regrow *region_row(int n);

void reflect_regions_in_region_browser(void)
{
  int i;
  if (reg_sp)
    {
      reg_sp->active = 1;
      if (reg_sp->chans)
	for (i = 0; i < reg_sp->nchans; i++)
	  reg_sp->chans[i]->active = 1;
    }
}

void reflect_no_regions_in_region_browser(void)
{
  int i;
  if (reg_sp)
    {
      reg_sp->active = 0;
      if (reg_sp->chans)
	for (i = 0; i < reg_sp->nchans; i++)
	  reg_sp->chans[i]->active = 0;
    }
}

static void region_update_graph(chan_info *cp)
{
  if (current_region == -1) return;
  reg_sp->nchans = region_chans(stack_position_to_id(current_region));
  if (reg_sp->nchans == 0) return;
  update_graph(cp, NULL);
  reg_sp->nchans = 1;
}

void reflect_region_graph_style(snd_state *ss)
{
  if (current_region == -1) return;
  if ((reg_sp) &&
      (reg_sp->chans) &&
      (reg_sp->chans[0]) &&
      (region_dialog_is_active()))
    {
      reg_sp->chans[0]->graph_style = region_graph_style(ss);
      reg_sp->chans[0]->dot_size = dot_size(ss);
      update_graph(reg_sp->chans[0], NULL);
    }
}

static void make_region_element(region_state *rs, int i)
{
  regrow *r;
  r = region_row(i);
  set_button_label_bold(r->nm, rs->name[i]);
  set_toggle_button(r->sv, rs->save[i], FALSE, (void *)r);
  set_toggle_button(r->pl, FALSE, FALSE, (void *)r);
  gtk_widget_show(r->rw);
}

static void unhighlight_region(snd_state *ss)
{
  regrow *oldr;
  if (current_region != -1)
    {
      oldr = region_row(current_region);
      set_backgrounds(oldr->rw, (ss->sgx)->highlight_color);
      set_backgrounds(oldr->nm, (ss->sgx)->highlight_color);
    }
}

static void unmake_region_labels (void)
{
  set_label(srate_text, STR_srate_p);
  set_label(chans_text, STR_chans_p);
  set_label(length_text, STR_length_p);
  set_label(maxamp_text, STR_maxamp_p);
}

static void highlight_region(snd_state *ss)
{
  regrow *oldr;
  if (current_region != -1)
    {
      oldr = region_row(current_region);
      set_backgrounds(oldr->rw, (ss->sgx)->zoom_color);
      set_backgrounds(oldr->nm, (ss->sgx)->zoom_color);
    }
}

static void make_region_labels(file_info *hdr)
{
  char *str;
  if (hdr == NULL) return;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, STR_srate, hdr->srate);
  set_label(srate_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, STR_chans, hdr->chans);
  set_label(chans_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, STR_length, (float)(hdr->samples) / (float)(hdr->chans * hdr->srate));
  set_label(length_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, STR_maxamp, region_maxamp(stack_position_to_id(current_region)));
  set_label(maxamp_text, str);
  FREE(str);
}

void update_region_browser(snd_state *ss, int grf_too)
{
  int i, len;
  region_state *rs;
  chan_info *cp;
  rs = region_report();
  len = rs->len;
  for (i = 0; i < len; i++) make_region_element(rs, i);
  for (i = len; i < max_regions(ss); i++) 
    if (region_rows[i])
      gtk_widget_hide(region_rows[i]->rw);
  free_region_state(rs);
  if (len == 0) return;
  gtk_widget_show(region_list);
  if (grf_too)
    {
      unhighlight_region(ss);
      current_region = 0;
      highlight_region(ss);
      goto_window(region_rows[0]->nm);
      cp = reg_sp->chans[0];
      cp->sound = reg_sp;
      if (cp) 
	{
	  cp->chan = 0;
	  set_sensitive(channel_f(cp), FALSE);
	  set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(0)) > 1));
	  if (region_ok(stack_position_to_id(0))) 
	    {
	      reg_sp->hdr = fixup_region_data(cp, 0, 0);
	      make_region_labels(reg_sp->hdr);
	      region_update_graph(cp);
	    }
	  else unmake_region_labels();
	}
    }
}


static void region_browser_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(region_dialog);
}

static void region_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(region_dialog);
}

int region_browser_is_active(void)
{
  return((region_dialog) && (GTK_WIDGET_VISIBLE(region_dialog)));
}

static void region_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
}

static void region_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
}

void delete_region_and_update_browser(snd_state *ss, int pos)
{
  int act;
  unhighlight_region(ss);
  act = remove_region_from_stack(pos);
  if (act == INVALID_REGION) return;
  if (region_dialog)
    {
      if (act != NO_REGIONS)
	{
	  current_region = 0;
	  highlight_region(ss);
	  goto_window(region_rows[0]->nm);
	}
      else 
	current_region = -1;
      update_region_browser(ss, 1);
    }
}

static void region_delete_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (current_region != -1)
    delete_region_and_update_browser(ss, current_region);
}

static void region_help_callback(GtkWidget *w, gpointer context)
{
  region_dialog_help((snd_state *)context);
}

static void region_up_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = reg_sp->chans[0];
  cp->sound = reg_sp;
  if (cp->chan > 0)
    {
      cp->chan--;
      set_sensitive(channel_f(cp), (cp->chan > 0));
      set_sensitive(channel_w(cp), TRUE);
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
}

static void region_down_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = reg_sp->chans[0];
  cp->sound = reg_sp;
  if ((cp->chan + 1) < region_chans(stack_position_to_id(current_region)))
    {
      cp->chan++;
      set_sensitive(channel_f(cp), TRUE);
      set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(current_region)) > (cp->chan + 1)));
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
}

static void region_focus_callback(GtkWidget *w, gpointer context) /* button clicked callback */
{
  snd_state *ss;
  chan_info *cp;
  regrow *r = (regrow *)context;
  ss = r->ss;
  unhighlight_region(ss);
  if (stack_position_to_id(r->pos) == INVALID_REGION) return; /* needed by auto-tester */
  current_region = r->pos;
  cp = reg_sp->chans[0];
  cp->sound = reg_sp;
  cp->chan  = 0;
  highlight_region(ss);
  set_sensitive(channel_f(cp), FALSE);
  set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(current_region)) > 1));
  reg_sp->hdr = fixup_region_data(cp, 0, current_region);
  make_region_labels(reg_sp->hdr);
  region_update_graph(cp);
}

void reflect_play_region_stop(int n)
{
  regrow *rg;
  if (region_rows)
    {
      rg = region_row(id_to_stack_position(n));
      if (rg) set_toggle_button(rg->pl, FALSE, FALSE, (void *)rg);
    }
}

static void region_play_callback(GtkWidget *w, gpointer context)
{
  regrow *r = (regrow *)context;
  if (GTK_TOGGLE_BUTTON(r->pl)->active)
    play_region(r->ss, stack_position_to_id(r->pos), IN_BACKGROUND);
  else stop_playing_region(stack_position_to_id(r->pos));
}

static void region_save_callback(GtkWidget *w, gpointer context)
{
  regrow *r = (regrow *)context;
  protect_region(r->pos, GTK_TOGGLE_BUTTON(r->sv)->active);
}

void set_region_protect(int id, int protect)
{
  regrow *r;
  protect_region(id, protect);
  if (region_rows)
    {
      r = region_row(id_to_stack_position(id));
      if ((r) && (r->sv)) set_toggle_button(r->sv, protect, FALSE, (void *)r);
    }
}

static void region_print_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (current_region != -1)
    region_print(eps_file(ss), "region", reg_sp->chans[0]);
}

static void region_edit_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1) 
    region_edit((snd_state *)context, current_region);
}

static void region_labels_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  gtk_signal_emit_stop_by_name(GTK_OBJECT(w), "enter_notify_event");
}

static GtkWidget *print_button, *edit_button;
static GtkWidget *dismiss_button, *help_button, *delete_button;

static void make_region_dialog(snd_state *ss)
{
  int i, id;
  regrow *r;
  chan_info *cp;
  file_info *hdr;
  ww_info *wwl;
  GtkWidget *infobox, *labels, *labbox;

  region_dialog = gtk_dialog_new();
  set_dialog_widget(ss, REGION_DIALOG, region_dialog);
  gtk_signal_connect(GTK_OBJECT(region_dialog), "delete_event", GTK_SIGNAL_FUNC(region_browser_delete_callback), (gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(region_dialog), STR_Regions);
  gtk_window_set_policy(GTK_WINDOW(region_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
  set_backgrounds(region_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width(GTK_CONTAINER(region_dialog), 10);
  gtk_widget_set_usize(GTK_WIDGET(region_dialog), 400, 400);
  gtk_widget_realize(region_dialog);

  help_button = gtk_button_new_with_label(STR_Help);
  dismiss_button = gtk_button_new_with_label(STR_Dismiss);
  delete_button = gtk_button_new_with_label(STR_Delete);

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), dismiss_button, TRUE, TRUE, 4);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), help_button, TRUE, TRUE, 4);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), delete_button, TRUE, TRUE, 4);

  gtk_signal_connect(GTK_OBJECT(delete_button), "clicked", GTK_SIGNAL_FUNC(region_delete_callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(region_help_callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(region_ok_callback), (gpointer)ss);

  set_pushed_button_colors(help_button, ss);
  set_pushed_button_colors(delete_button, ss);
  set_pushed_button_colors(dismiss_button, ss);

  gtk_widget_show(delete_button);
  gtk_widget_show(help_button);
  gtk_widget_show(dismiss_button);

  wwl = make_title_row(ss, GTK_DIALOG(region_dialog)->vbox, STR_save, STR_play, STR_regions, DONT_PAD_TITLE, WITHOUT_SORT_BUTTON, WITH_PANED_WINDOW);
  region_list = wwl->list;

  infobox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(wwl->toppane), infobox, FALSE, FALSE, 2);
  gtk_widget_show(infobox);
  
  region_rows = (regrow **)CALLOC(max_regions(ss), sizeof(regrow *));
  region_rows_size = max_regions(ss);
  for (i = 0; i < max_regions(ss); i++)
    {
      r = make_regrow(ss, region_list, 
		      (void (*)())region_save_callback, 
		      (void (*)())region_play_callback, 
		      (void (*)())region_focus_callback);
      region_rows[i] = r;
      r->pos = i;
      r->ss = ss;
      r->parent = REGION_VIEWER;
    }

  update_region_browser(ss, 0);


  /* in Gtk, apparently, labels are just the text, not the background (i.e. they're transparent) */
  /* we need a button simply to get the background color, then a vbox to put four labels on the button */
  /* but we get a button which flashes whenever the mouse comes near it and has "relief" */
  /* if we turn off the relief, the colors go away */
  /* all I want is an opaque label with a background color */

  labels = gtk_button_new();
  set_background(labels, (ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(infobox), labels, TRUE, TRUE, 2);
  gtk_widget_show(labels);
  gtk_signal_connect(GTK_OBJECT(labels), "enter_notify_event", GTK_SIGNAL_FUNC(region_labels_mouse_enter), NULL);

  labbox = gtk_vbox_new(TRUE, 0);
  gtk_container_add(GTK_CONTAINER(labels), labbox);
  gtk_widget_show(labbox);
  
  srate_text = gtk_label_new(STR_srate_p);
  gtk_label_set_justify(GTK_LABEL(srate_text), GTK_JUSTIFY_LEFT);  /* these appear to be no-ops! */
  gtk_box_pack_start(GTK_BOX(labbox), srate_text, FALSE, FALSE, 2);
  gtk_widget_show(srate_text);

  chans_text = gtk_label_new(STR_chans_p);
  gtk_label_set_justify(GTK_LABEL(chans_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), chans_text, FALSE, FALSE, 2);
  gtk_widget_show(chans_text);

  length_text = gtk_label_new(STR_length_p);
  gtk_label_set_justify(GTK_LABEL(length_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), length_text, FALSE, FALSE, 2);
  gtk_widget_show(length_text);

  maxamp_text = gtk_label_new(STR_maxamp_p);
  gtk_label_set_justify(GTK_LABEL(maxamp_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), maxamp_text, FALSE, FALSE, 2);
  gtk_widget_show(maxamp_text);

  edit_button = gtk_button_new_with_label(STR_edit);
  set_pushed_button_colors(edit_button, ss);
  gtk_signal_connect(GTK_OBJECT(edit_button), "clicked", GTK_SIGNAL_FUNC(region_edit_callback), (gpointer)ss);
  gtk_box_pack_start(GTK_BOX(infobox), edit_button, TRUE, TRUE, 2);
  gtk_widget_show(edit_button);

  print_button = gtk_button_new_with_label(STR_print);
  set_pushed_button_colors(print_button, ss);
  gtk_signal_connect(GTK_OBJECT(print_button), "clicked", GTK_SIGNAL_FUNC(region_print_callback), (gpointer)ss);
  gtk_box_pack_start(GTK_BOX(infobox), print_button, TRUE, TRUE, 2);
  gtk_widget_show(print_button);

  region_grf = wwl->panes;

  gtk_widget_show(region_dialog);

  if (!reg_sp) 
    { /* just a place holder, I think -- see make_region_readable in snd-clip.c */
      id = stack_position_to_id(0);
      reg_sp = (snd_info *)CALLOC(1, sizeof(snd_info));
      reg_sp->inuse = 1;
      reg_sp->active = 1;
      reg_sp->nchans = 1;
      reg_sp->allocated_chans = 1;
      reg_sp->chans = (chan_info **)CALLOC(1, sizeof(chan_info *));
      reg_sp->sx_scroll_max = 100;
      reg_sp->hdr = (file_info *)CALLOC(1, sizeof(file_info));
      reg_sp->search_proc = XEN_UNDEFINED;
      reg_sp->prompt_callback = XEN_UNDEFINED;
      hdr = reg_sp->hdr;
      hdr->samples = region_len(id);
      hdr->srate = region_srate(id);
      hdr->comment = NULL;
      hdr->chans = 1;
      current_region = 0;
      add_channel_window(reg_sp, 0, ss, 0, 0, region_grf, WITH_ARROWS);
      cp = reg_sp->chans[0];
      cp->sound = reg_sp;
      cp->edit_size = 1;
      cp->edit_ctr = 0;
      allocate_ed_list(cp);
      cp->samples = (int *)CALLOC(cp->edit_size, sizeof(int));
      cp->sound_size = 1;
      cp->sound_ctr = 0;
      cp->sounds = (snd_data **)CALLOC(cp->sound_size, sizeof(snd_data *));
      cp->samples[0] = region_len(id);
      cp->graph_style = region_graph_style(ss); /* added 8-Aug-01 */
      cp->dot_size = dot_size(ss);
    }
  else 
    {
      add_channel_window(reg_sp, 0, ss, 0, 0, region_grf, WITH_ARROWS);
      cp = reg_sp->chans[0];
    }

  gtk_paned_set_position(GTK_PANED(region_grf), 150);

  cp->hookable = 0;
  gtk_signal_connect(GTK_OBJECT(channel_graph(cp)), "expose_event", GTK_SIGNAL_FUNC(region_resize_callback), (gpointer)cp);
  gtk_signal_connect(GTK_OBJECT(channel_graph(cp)), "configure_event", GTK_SIGNAL_FUNC(region_expose_callback), (gpointer)cp);

  gtk_signal_connect(GTK_OBJECT(channel_up_arrow(cp)), "button_press_event", GTK_SIGNAL_FUNC(region_up_arrow_callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(channel_down_arrow(cp)), "button_press_event", GTK_SIGNAL_FUNC(region_down_arrow_callback), (gpointer)ss);

  set_sensitive(channel_f(cp), FALSE);
  if (region_chans(stack_position_to_id(0)) > 1) set_sensitive(channel_w(cp), TRUE);
  cp->chan = 0;
  reg_sp->hdr = fixup_region_data(cp, 0, 0);
  make_region_labels(reg_sp->hdr);
  highlight_region(ss);
  region_update_graph(cp);
  FREE(wwl); 
  wwl = NULL;
}

void view_region_callback(GtkWidget *w, gpointer context)
{
  /* put up scrollable dialog describing/playing/editing the region list */
  snd_state *ss = (snd_state *)context;
  if (region_dialog == NULL)
    make_region_dialog(ss);
  else 
    {
      raise_dialog(region_dialog);
      current_region = 0;
    }
}

int region_dialog_is_active(void)
{
  return((region_dialog != NULL) && 
	 (GTK_WIDGET_VISIBLE(region_dialog)));
}

void allocate_region_rows(snd_state *ss, int n)
{
  int i;
  if ((region_dialog) && 
      (n > region_rows_size))
    {
      region_rows = (regrow **)REALLOC(region_rows, n * sizeof(regrow *));
      for (i = region_rows_size; i < n; i++) region_rows[i] = NULL;
      region_rows_size = n;
    }
}

static regrow *region_row(int n)
{
  regrow *r;
  snd_state *ss;
  if (n < region_rows_size)
    {
      if (region_rows[n] == NULL)
	{
	  ss = get_global_state();
	  r = make_regrow(ss, region_list, 
			  (void (*)())region_save_callback, 
			  (void (*)())region_play_callback, 
			  (void (*)())region_focus_callback);
	  region_rows[n] = r;
	  r->pos = n;
	  r->ss = ss;
	  r->parent = REGION_VIEWER;
	}
      return(region_rows[n]);
    }
  return(NULL);
}

static XEN g_region_dialog(void) 
{
  #define H_region_dialog "(" S_region_dialog ") starts the region dialog"
  snd_state *ss;
  ss = get_global_state();
  if (snd_regions() > 0) 
    view_region_callback(MAIN_PANE(ss), (gpointer)ss); 
  return(XEN_WRAP_C_POINTER(region_dialog));
}

#if DEBUGGING
XEN g_channel_widgets_1(chan_info *cp);
static XEN g_region_dialog_widgets(void)
{
  if (region_dialog)
    return(XEN_CONS(XEN_WRAP_C_POINTER(region_dialog),
             XEN_CONS(XEN_WRAP_C_POINTER(print_button),
	       XEN_CONS(XEN_WRAP_C_POINTER(edit_button),
		 XEN_CONS(XEN_WRAP_C_POINTER(dismiss_button),
		   XEN_CONS(XEN_WRAP_C_POINTER(delete_button),
		     XEN_CONS(XEN_WRAP_C_POINTER(help_button),
		       (reg_sp) ? g_channel_widgets_1(reg_sp->chans[0]) : XEN_EMPTY_LIST)))))));
  return(XEN_EMPTY_LIST);
}
static XEN g_region_row_widgets(void)
{
  int i;
  XEN lst;
  lst = XEN_EMPTY_LIST;
  for (i = region_rows_size - 1; i >= 0; i--)
    if ((region_rows[i]) &&
	(GTK_WIDGET_IS_SENSITIVE(region_rows[i]->nm)))
      lst = XEN_CONS(XEN_WRAP_C_POINTER(region_rows[i]->nm), lst);
  return(lst);
}
#endif

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_region_dialog_w, g_region_dialog)
#if DEBUGGING
XEN_NARGIFY_0(g_region_dialog_widgets_w, g_region_dialog_widgets)
XEN_NARGIFY_0(g_region_row_widgets_w, g_region_row_widgets)
#endif
#else
#define g_region_dialog_w g_region_dialog
#if DEBUGGING
#define g_region_dialog_widgets_w g_region_dialog_widgets
#define g_region_row_widgets_w g_region_row_widgets
#endif
#endif

void g_init_gxregion(void)
{
  XEN_DEFINE_PROCEDURE(S_region_dialog, g_region_dialog_w, 0, 0, 0,  H_region_dialog);

#if DEBUGGING
  XEN_DEFINE_PROCEDURE("region-dialog-widgets", g_region_dialog_widgets_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("region-row-widgets", g_region_row_widgets_w, 0, 0, 0, "");
#endif
}

