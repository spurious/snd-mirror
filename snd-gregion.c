#include "snd.h"

/* -------- region browser -------- */

static GtkWidget *region_dialog = NULL, *region_list, *region_grf;
static regrow **region_rows = NULL;
static int region_rows_size = 0;
static snd_info *rsp = NULL;
static int current_region = -1;
static GtkWidget *srate_text, *length_text, *chans_text, *maxamp_text;
static regrow *region_row(int n);

void reflect_regions_in_region_browser(void)
{
  int i;
  if (rsp)
    {
      rsp->active = TRUE;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = TRUE;
    }
}

void reflect_no_regions_in_region_browser(void)
{
  int i;
  if (rsp)
    {
      rsp->active = FALSE;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = FALSE;
    }
}

static void region_update_graph(chan_info *cp)
{
  if (current_region == -1) return;
  rsp->nchans = region_chans(stack_position_to_id(current_region));
  if (rsp->nchans == 0) return;
  update_graph(cp);
  rsp->nchans = 1;
}

void reflect_region_graph_style(snd_state *ss)
{
  if (current_region == -1) return;
  if ((rsp) &&
      (rsp->chans) &&
      (rsp->chans[0]) &&
      (region_dialog_is_active()))
    {
      rsp->chans[0]->time_graph_style = region_graph_style(ss);
      rsp->chans[0]->dot_size = dot_size(ss);
      update_graph(rsp->chans[0]);
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
  mus_snprintf(str, PRINT_BUFFER_SIZE, "srate: %d", hdr->srate);
  set_label(srate_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, "chans: %d", hdr->chans);
  set_label(chans_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, "length: %.3f", (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)));
  set_label(length_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, "maxamp: %.3f", region_maxamp(stack_position_to_id(current_region)));
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
      cp = rsp->chans[0];
      if (cp) 
	{
	  cp->sound = rsp;
	  cp->chan = 0;
	  set_sensitive(channel_f(cp), FALSE);
	  set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(0)) > 1));
	  rsp->hdr = fixup_region_data(cp, 0, 0);
	  make_region_labels(rsp->hdr);
	  region_update_graph(cp);
	}
    }
}


static gboolean region_browser_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(region_dialog);
  return(FALSE);
}

static void region_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(region_dialog);
}

int region_browser_is_active(void)
{
  return((region_dialog) && (GTK_WIDGET_VISIBLE(region_dialog)));
}

static gboolean region_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(FALSE);
}

static gboolean region_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(FALSE);
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

static gboolean region_up_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if (cp->chan > 0)
    {
      cp->chan--;
      set_sensitive(channel_f(cp), (cp->chan > 0));
      set_sensitive(channel_w(cp), TRUE);
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(FALSE);
}

static gboolean region_down_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if ((cp->chan + 1) < region_chans(stack_position_to_id(current_region)))
    {
      cp->chan++;
      set_sensitive(channel_f(cp), TRUE);
      set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(current_region)) > (cp->chan + 1)));
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(FALSE);
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
  cp = rsp->chans[0];
  cp->sound = rsp;
  cp->chan  = 0;
  highlight_region(ss);
  set_sensitive(channel_f(cp), FALSE);
  set_sensitive(channel_w(cp), (region_chans(stack_position_to_id(current_region)) > 1));
  rsp->hdr = fixup_region_data(cp, 0, current_region);
  make_region_labels(rsp->hdr);
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
    region_print(eps_file(ss), "region", rsp->chans[0]);
}

static void region_edit_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1) 
    region_edit((snd_state *)context, current_region);
}

static gboolean region_labels_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(FALSE);
}

static GtkWidget *print_button, *edit_button;
static GtkWidget *dismiss_button, *help_button, *delete_button;

static void make_region_dialog(snd_state *ss)
{
  int i;
  regrow *r;
  chan_info *cp;
  ww_info *wwl;
  GtkWidget *infobox, *labels, *labbox;

  region_dialog = gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(region_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(region_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_browser_delete_callback), (gpointer)ss, 0),
				 0);
  gtk_window_set_title(GTK_WINDOW(region_dialog), "Regions");
  sg_make_resizable(region_dialog);
  set_backgrounds(region_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width(GTK_CONTAINER(region_dialog), 10);
  gtk_window_resize(GTK_WINDOW(region_dialog), 400, 400);
  gtk_widget_realize(region_dialog);

  help_button = gtk_button_new_with_label("Help");
  dismiss_button = gtk_button_new_with_label("Dismiss");
  delete_button = gtk_button_new_with_label("Delete");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), dismiss_button, TRUE, TRUE, 4);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), help_button, TRUE, TRUE, 4);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), delete_button, TRUE, TRUE, 4);

  g_signal_connect_closure_by_id(GTK_OBJECT(delete_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(delete_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_delete_callback), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_help_callback), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_ok_callback), (gpointer)ss, 0),
				 0);


  gtk_widget_show(delete_button);
  gtk_widget_show(help_button);
  gtk_widget_show(dismiss_button);

  wwl = make_title_row(ss, GTK_DIALOG(region_dialog)->vbox, "save", "play", "regions", DONT_PAD_TITLE, WITHOUT_SORT_BUTTON, WITH_PANED_WINDOW);
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
  g_signal_connect_closure_by_id(GTK_OBJECT(labels),
				 g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(labels))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_labels_mouse_enter), NULL, 0),
				 0);

  labbox = gtk_vbox_new(TRUE, 0);
  gtk_container_add(GTK_CONTAINER(labels), labbox);
  gtk_widget_show(labbox);
  
  srate_text = gtk_label_new("srate:");
  gtk_label_set_justify(GTK_LABEL(srate_text), GTK_JUSTIFY_LEFT);  /* these appear to be no-ops! */
  gtk_box_pack_start(GTK_BOX(labbox), srate_text, FALSE, FALSE, 2);
  gtk_widget_show(srate_text);

  chans_text = gtk_label_new("chans:");
  gtk_label_set_justify(GTK_LABEL(chans_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), chans_text, FALSE, FALSE, 2);
  gtk_widget_show(chans_text);

  length_text = gtk_label_new("length:");
  gtk_label_set_justify(GTK_LABEL(length_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), length_text, FALSE, FALSE, 2);
  gtk_widget_show(length_text);

  maxamp_text = gtk_label_new("maxamp:");
  gtk_label_set_justify(GTK_LABEL(maxamp_text), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(labbox), maxamp_text, FALSE, FALSE, 2);
  gtk_widget_show(maxamp_text);

  edit_button = gtk_button_new_with_label("edit");
  g_signal_connect_closure_by_id(GTK_OBJECT(edit_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(edit_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_edit_callback), (gpointer)ss, 0),
				 0);
  gtk_box_pack_start(GTK_BOX(infobox), edit_button, TRUE, TRUE, 2);
  gtk_widget_show(edit_button);

  print_button = gtk_button_new_with_label("print");
  g_signal_connect_closure_by_id(GTK_OBJECT(print_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(print_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_print_callback), (gpointer)ss, 0),
				 0);
  gtk_box_pack_start(GTK_BOX(infobox), print_button, TRUE, TRUE, 2);
  gtk_widget_show(print_button);

  region_grf = wwl->panes;

  gtk_widget_show(region_dialog);

  if (!rsp) 
    { 
      rsp = make_initial_region_sp(ss, region_grf);
      current_region = 0;
    }
  else add_channel_window(rsp, 0, ss, 0, 0, region_grf, WITH_ARROWS);
  cp = rsp->chans[0];

  gtk_paned_set_position(GTK_PANED(region_grf), 150);

  cp->hookable = 0;
  g_signal_connect_closure_by_id(GTK_OBJECT(channel_graph(cp)),
				 g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(channel_graph(cp)))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_resize_callback), (gpointer)cp, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(channel_graph(cp)),
				 g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(channel_graph(cp)))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_expose_callback), (gpointer)cp, 0),
				 0);

  g_signal_connect_closure_by_id(GTK_OBJECT(channel_up_arrow(cp)),
				 g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(channel_up_arrow(cp)))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_up_arrow_callback), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(channel_down_arrow(cp)),
				 g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(channel_down_arrow(cp)))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(region_down_arrow_callback), (gpointer)ss, 0),
				 0);

  set_sensitive(channel_f(cp), FALSE);
  if (region_chans(stack_position_to_id(0)) > 1) set_sensitive(channel_w(cp), TRUE);
  cp->chan = 0;
  rsp->hdr = fixup_region_data(cp, 0, 0);
  make_region_labels(rsp->hdr);
  highlight_region(ss);
  region_update_graph(cp);
  FREE(wwl); 
  wwl = NULL;
  set_dialog_widget(ss, REGION_DIALOG, region_dialog);
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

void allocate_region_rows(int n)
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
  return(XEN_WRAP_WIDGET(region_dialog));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_region_dialog_w, g_region_dialog)
#else
#define g_region_dialog_w g_region_dialog
#endif

void g_init_gxregion(void)
{
  XEN_DEFINE_PROCEDURE(S_region_dialog, g_region_dialog_w, 0, 0, 0,  H_region_dialog);
}

