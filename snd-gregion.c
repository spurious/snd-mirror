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
  if (rsp)
    {
      int i;
      rsp->active = true;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = true;
    }
}

void reflect_no_regions_in_region_browser(void)
{
  if (rsp)
    {
      int i;
      rsp->active = false;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = false;
    }
}

static void region_update_graph(chan_info *cp)
{
  if (current_region == -1) return;
  rsp->nchans = region_chans(region_list_position_to_id(current_region));
  if (rsp->nchans == 0) return;
  update_graph(cp);
  rsp->nchans = 1;
}

void reflect_region_graph_style(void)
{
  if (current_region == -1) return;
  if ((rsp) &&
      (rsp->chans) &&
      (rsp->chans[0]) &&
      (region_dialog_is_active()))
    {
      rsp->chans[0]->time_graph_style = region_graph_style(ss);
      rsp->chans[0]->dot_size = dot_size(ss);
      /* update_graph(rsp->chans[0]); */
      update_region_browser(true);
    }
}

static void unhighlight_region(void)
{
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      gtk_widget_modify_bg(oldr->nm, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_modify_base(oldr->nm, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_modify_bg(oldr->rw, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_modify_base(oldr->rw, GTK_STATE_NORMAL, ss->sgx->basic_color);
    }
}

static void highlight_region(void)
{
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      gtk_widget_modify_bg(oldr->nm, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_base(oldr->nm, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_bg(oldr->rw, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_base(oldr->rw, GTK_STATE_NORMAL, ss->sgx->zoom_color);
    }
}

static void make_region_labels(file_info *hdr)
{
  char *str;
  if (hdr == NULL) return;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("srate: %d"), hdr->srate);
  set_label(srate_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("chans: %d"), hdr->chans);
  set_label(chans_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("length: %.3f"), (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)));
  set_label(length_text, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("maxamp: %.3f"), region_maxamp(region_list_position_to_id(current_region)));
  set_label(maxamp_text, str);
  FREE(str);
}

void update_region_browser(bool grf_too)
{
  int i, len;
  region_state *rs;
  rs = region_report();
  len = rs->len;
  for (i = 0; i < len; i++) 
    {
      regrow *r;
      r = region_row(i);
      set_button_label(r->nm, rs->name[i]);
      set_toggle_button(r->pl, false, false, (void *)r);
      gtk_widget_show(r->rw);
    }
  for (i = len; i < max_regions(ss); i++) 
    if (region_rows[i])
      gtk_widget_hide(region_rows[i]->rw);
  free_region_state(rs);
  if (len == 0) return;
  gtk_widget_show(region_list);
  if (grf_too)
    {
      chan_info *cp;
      unhighlight_region();
      current_region = 0;
      highlight_region();
      goto_window(region_rows[0]->nm);
      cp = rsp->chans[0];
      if (cp) 
	{
	  cp->sound = rsp;
	  cp->chan = 0;
	  set_sensitive(channel_f(cp), false);
	  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(0)) > 1));
	  rsp->hdr = fixup_region_data(cp, 0, 0);
	  make_region_labels(rsp->hdr);
	  region_update_graph(cp);
	}
    }
}

static gint region_browser_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(region_dialog);
  return(true);
}

static void region_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(region_dialog);
}

bool region_browser_is_active(void)
{
  return((region_dialog) && 
	 (GTK_WIDGET_VISIBLE(region_dialog)));
}

static gboolean region_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(false);
}

static gboolean region_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(false);
}

void delete_region_and_update_browser(int pos)
{
  int act;
  unhighlight_region();
  act = remove_region_from_list(pos);
  if (act == INVALID_REGION) return;
  if (region_dialog)
    {
      if (act != NO_REGIONS)
	{
	  current_region = 0;
	  highlight_region();
	  goto_window(region_rows[0]->nm);
	}
      else 
	current_region = -1;
      update_region_browser(true);
    }
}

static void region_delete_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1)
    delete_region_and_update_browser(current_region);
}

static void region_help_callback(GtkWidget *w, gpointer context)
{
  region_dialog_help();
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
      set_sensitive(channel_w(cp), true);
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(false);
}

static gboolean region_down_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if ((cp->chan + 1) < region_chans(region_list_position_to_id(current_region)))
    {
      cp->chan++;
      set_sensitive(channel_f(cp), true);
      set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > (cp->chan + 1)));
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(false);
}

static void region_focus_callback(GtkWidget *w, gpointer context) /* button clicked callback */
{
  chan_info *cp;
  regrow *r = (regrow *)context;
  unhighlight_region();
  if (region_list_position_to_id(r->pos) == INVALID_REGION) return; /* needed by auto-tester */
  current_region = r->pos;
  cp = rsp->chans[0];
  cp->sound = rsp;
  cp->chan  = 0;
  highlight_region();
  set_sensitive(channel_f(cp), false);
  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > 1));
  rsp->hdr = fixup_region_data(cp, 0, current_region);
  make_region_labels(rsp->hdr);
  region_update_graph(cp);
}

void reflect_play_region_stop(int n)
{
  if (region_rows)
    {
      regrow *rg;
      rg = region_row(region_id_to_list_position(n));
      if (rg) set_toggle_button(rg->pl, false, false, (void *)rg);
    }
}

static void region_play_callback(GtkWidget *w, gpointer context)
{
  regrow *r = (regrow *)context;
  if (GTK_TOGGLE_BUTTON(r->pl)->active)
    play_region(region_list_position_to_id(r->pos), IN_BACKGROUND);
  else stop_playing_region(region_list_position_to_id(r->pos), PLAY_BUTTON_UNSET);
}

static void region_print_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1)
    region_print(eps_file(ss), "region", rsp->chans[0]);
}

static void region_edit_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1) 
    region_edit(current_region);
}

static gboolean region_labels_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(false);
}

static GtkWidget *print_button, *edit_button;
static GtkWidget *dismiss_button, *help_button, *delete_button;

static void make_region_dialog(void)
{
  int i, id;
  regrow *r;
  chan_info *cp;
  ww_info *wwl;
  GtkWidget *infobox, *labels, *labbox;
  region_dialog = snd_gtk_dialog_new();
  SG_SIGNAL_CONNECT(region_dialog, "delete_event", region_browser_delete_callback, NULL);
  gtk_window_set_title(GTK_WINDOW(region_dialog), _("Regions"));
  sg_make_resizable(region_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(region_dialog), 10);
  gtk_window_resize(GTK_WINDOW(region_dialog), 400, 400);
  gtk_widget_realize(region_dialog);

  help_button = gtk_button_new_with_label(_("Help"));
  gtk_widget_set_name(help_button, "help_button");
  dismiss_button = gtk_button_new_with_label(_("Dismiss"));
  gtk_widget_set_name(dismiss_button, "quit_button");
  delete_button = gtk_button_new_with_label(_("Delete"));
  gtk_widget_set_name(delete_button, "doit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), dismiss_button, true, true, 4);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), delete_button, true, true, 4);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(region_dialog)->action_area), help_button, true, true, 4);

  SG_SIGNAL_CONNECT(delete_button, "clicked", region_delete_callback, NULL);
  SG_SIGNAL_CONNECT(help_button, "clicked", region_help_callback, NULL);
  SG_SIGNAL_CONNECT(dismiss_button, "clicked", region_ok_callback, NULL);

  gtk_widget_show(delete_button);
  gtk_widget_show(help_button);
  gtk_widget_show(dismiss_button);

  wwl = make_title_row(GTK_DIALOG(region_dialog)->vbox, _("play"), NULL, DONT_PAD_TITLE, WITHOUT_SORT_BUTTON, WITH_PANED_WINDOW);
  region_list = wwl->list;

  infobox = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(wwl->toppane), infobox, false, false, 2);
  gtk_widget_show(infobox);
  
  region_rows = (regrow **)CALLOC(max_regions(ss), sizeof(regrow *));
  region_rows_size = max_regions(ss);
  for (i = 0; i < max_regions(ss); i++)
    {
      r = make_regrow(region_list, (void (*)())region_play_callback, (void (*)())region_focus_callback);
      region_rows[i] = r;
      r->pos = i;
      r->parent = REGION_VIEWER;
    }

  update_region_browser(false);

  /* in Gtk, apparently, labels are just the text, not the background (i.e. they're transparent) */
  /* we need a button simply to get the background color, then a vbox to put four labels on the button */
  /* but we get a button which flashes whenever the mouse comes near it and has "relief" */
  /* if we turn off the relief, the colors go away */
  /* all I want is an opaque label with a background color */

  labels = gtk_button_new();
  gtk_box_pack_start(GTK_BOX(infobox), labels, true, true, 2);
  gtk_widget_show(labels);
  gtk_widget_modify_bg(labels, GTK_STATE_NORMAL, ss->sgx->highlight_color);
  SG_SIGNAL_CONNECT(labels, "enter_notify_event", region_labels_mouse_enter, NULL);

  labbox = gtk_vbox_new(true, 0);
  gtk_container_add(GTK_CONTAINER(labels), labbox);
  gtk_widget_show(labbox);
  gtk_widget_modify_bg(labbox, GTK_STATE_NORMAL, ss->sgx->highlight_color);

  srate_text = gtk_label_new(_("srate:"));
  sg_left_justify_label(srate_text);
  gtk_box_pack_start(GTK_BOX(labbox), srate_text, false, false, 2);
  gtk_widget_show(srate_text);

  chans_text = gtk_label_new(_("chans:"));
  sg_left_justify_label(chans_text);
  gtk_box_pack_start(GTK_BOX(labbox), chans_text, false, false, 2);
  gtk_widget_show(chans_text);

  length_text = gtk_label_new(_("length:"));
  sg_left_justify_label(length_text);
  gtk_box_pack_start(GTK_BOX(labbox), length_text, false, false, 2);
  gtk_widget_show(length_text);

  maxamp_text = gtk_label_new(_("maxamp:"));
  sg_left_justify_label(maxamp_text);
  gtk_box_pack_start(GTK_BOX(labbox), maxamp_text, false, false, 2);
  gtk_widget_show(maxamp_text);

  edit_button = gtk_button_new_with_label(_("edit"));
  SG_SIGNAL_CONNECT(edit_button, "clicked", region_edit_callback, NULL);
  gtk_box_pack_start(GTK_BOX(infobox), edit_button, true, true, 2);
  gtk_widget_show(edit_button);
  gtk_widget_modify_bg(edit_button, GTK_STATE_NORMAL, ss->sgx->lighter_blue);
  gtk_widget_modify_bg(edit_button, GTK_STATE_ACTIVE, ss->sgx->red);

  print_button = gtk_button_new_with_label(_("print"));
  SG_SIGNAL_CONNECT(print_button, "clicked", region_print_callback, NULL);
  gtk_box_pack_start(GTK_BOX(infobox), print_button, true, true, 2);
  gtk_widget_show(print_button);
  gtk_widget_modify_bg(print_button, GTK_STATE_NORMAL, ss->sgx->lighter_blue);
  gtk_widget_modify_bg(print_button, GTK_STATE_ACTIVE, ss->sgx->red);

  region_grf = wwl->panes;
  gtk_widget_show(region_dialog);

  id = region_list_position_to_id(0);
  rsp = make_simple_channel_display(region_srate(id), region_len(id), WITH_ARROWS, region_graph_style(ss), region_grf, WITHOUT_EVENTS);
  rsp->inuse = SOUND_REGION;
  current_region = 0;
  cp = rsp->chans[0];

  gtk_paned_set_position(GTK_PANED(region_grf), 150);
  SG_SIGNAL_CONNECT(channel_graph(cp), "expose_event", region_resize_callback, cp);
  SG_SIGNAL_CONNECT(channel_graph(cp), "configure_event", region_expose_callback, cp);

  SG_SIGNAL_CONNECT(channel_up_arrow(cp), "button_press_event", region_up_arrow_callback, NULL);
  SG_SIGNAL_CONNECT(channel_down_arrow(cp), "button_press_event", region_down_arrow_callback, NULL);

  set_sensitive(channel_f(cp), false);
  if (region_chans(region_list_position_to_id(0)) > 1) set_sensitive(channel_w(cp), true);
  cp->chan = 0;
  rsp->hdr = fixup_region_data(cp, 0, 0);
  make_region_labels(rsp->hdr);
  highlight_region();
  region_update_graph(cp);
  FREE(wwl); 
  wwl = NULL;
  set_dialog_widget(REGION_DIALOG, region_dialog);
}

void view_region_callback(GtkWidget *w, gpointer context)
{
  /* put up scrollable dialog describing/playing/editing the region list */
  if (region_dialog == NULL)
    make_region_dialog();
  else 
    {
      update_region_browser(true);
      raise_dialog(region_dialog);
      current_region = 0;
    }
}

bool region_dialog_is_active(void)
{
  return((region_dialog != NULL) && 
	 (GTK_WIDGET_VISIBLE(region_dialog)));
}

void allocate_region_rows(int n)
{
  if ((region_dialog) && 
      (n > region_rows_size))
    {
      int i;
      region_rows = (regrow **)REALLOC(region_rows, n * sizeof(regrow *));
      for (i = region_rows_size; i < n; i++) region_rows[i] = NULL;
      region_rows_size = n;
    }
}

static regrow *region_row(int n)
{
  if (n < region_rows_size)
    {
      if (region_rows[n] == NULL)
	{
	  regrow *r;
	  r = make_regrow(region_list, (void (*)())region_play_callback, (void (*)())region_focus_callback);
	  region_rows[n] = r;
	  r->pos = n;
	  r->parent = REGION_VIEWER;
	}
      return(region_rows[n]);
    }
  return(NULL);
}

static XEN g_view_regions_dialog(void) 
{
  #define H_view_regions_dialog "(" S_view_regions_dialog "): start the region dialog"
  if (snd_regions() > 0) 
    view_region_callback(MAIN_PANE(ss), NULL); 
  return(XEN_WRAP_WIDGET(region_dialog));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_view_regions_dialog_w, g_view_regions_dialog)
#else
#define g_view_regions_dialog_w g_view_regions_dialog
#endif

void g_init_gxregion(void)
{
  XEN_DEFINE_PROCEDURE(S_view_regions_dialog, g_view_regions_dialog_w, 0, 0, 0,  H_view_regions_dialog);
}

