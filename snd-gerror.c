#include "snd.h" 

/* error handlers -- these include the error dialog (in case no sound is active) and an error history list */

static GtkWidget *snd_error_dialog = NULL;
static GtkWidget *snd_error_history = NULL;

static void dismiss_snd_error(GtkWidget *w,gpointer clientData)
{
  gtk_widget_hide(snd_error_dialog);
}

static void delete_snd_error(GtkWidget *w,GdkEvent *event,gpointer clientData)
{
  gtk_widget_hide(snd_error_dialog);
}

static void create_snd_error_dialog(snd_state *ss, int popup)
{
  GtkWidget *ok_button,*table;
  GtkWidget *hscrollbar;
  GtkWidget *vscrollbar;

  snd_error_dialog = gtk_dialog_new();
  gtk_signal_connect(GTK_OBJECT(snd_error_dialog),"delete_event",GTK_SIGNAL_FUNC(delete_snd_error),(gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(snd_error_dialog),STR_Error);
  gtk_window_set_policy(GTK_WINDOW(snd_error_dialog),TRUE,TRUE,FALSE); /* allow shrink or grow */
  set_background(snd_error_dialog,(ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(snd_error_dialog), 10);
  gtk_widget_set_usize (GTK_WIDGET(snd_error_dialog),200,300);
  gtk_widget_realize(snd_error_dialog);
  add_dialog(ss,snd_error_dialog);

  ok_button = gtk_button_new_with_label(STR_Ok);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(snd_error_dialog)->action_area),ok_button,FALSE,TRUE,20);
  gtk_signal_connect(GTK_OBJECT(ok_button),"clicked",GTK_SIGNAL_FUNC(dismiss_snd_error),(gpointer)ss);
  set_pushed_button_colors(ok_button,ss);
  gtk_widget_show(ok_button);

  table = gtk_table_new (2, 2, FALSE);
  snd_error_history = gtk_text_new (NULL, NULL);
  gtk_table_attach(GTK_TABLE(table), snd_error_history, 0, 1, 0, 1, GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND | GTK_SHRINK, 0, 0);
  gtk_text_set_editable(GTK_TEXT(snd_error_history),FALSE);
  gtk_text_set_word_wrap(GTK_TEXT(snd_error_history),FALSE);
  gtk_widget_show (snd_error_history);
  hscrollbar = gtk_hscrollbar_new(GTK_TEXT(snd_error_history)->hadj);
  set_background(hscrollbar,(ss->sgx)->position_color);
  gtk_table_attach(GTK_TABLE(table),hscrollbar, 0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (hscrollbar);
  vscrollbar = gtk_vscrollbar_new(GTK_TEXT (snd_error_history)->vadj);
  set_background(vscrollbar,(ss->sgx)->position_color);
  gtk_table_attach(GTK_TABLE(table),vscrollbar, 1, 2, 0, 1, GTK_FILL, GTK_EXPAND | GTK_FILL | GTK_SHRINK, 0, 0);
  gtk_widget_show (vscrollbar);

  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(snd_error_dialog)->vbox),table);
  gtk_widget_show(table);
  if (popup) gtk_widget_show(snd_error_dialog);
}

void add_to_error_history(snd_state *ss, char *msg, int popup)
{
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
  char *tim,*buf;
  time_t ts;
#endif
  int pos;
  if (!snd_error_dialog) 
    create_snd_error_dialog(ss,popup);
  else
    if ((popup) && (!(GTK_WIDGET_VISIBLE(snd_error_dialog))))
      gtk_widget_show(snd_error_dialog);
  gtk_text_freeze(GTK_TEXT(snd_error_history));
  pos = gtk_text_get_length(GTK_TEXT(snd_error_history));
  if (pos > 0) gtk_text_set_point(GTK_TEXT(snd_error_history),pos);

#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
  tim = (char *)CALLOC(TIME_STR_SIZE,sizeof(char));
  buf = (char *)CALLOC(TIME_STR_SIZE,sizeof(char));
  time(&ts);
  strftime(tim,TIME_STR_SIZE,"%H:%M:%S",localtime(&ts));
  sprintf(buf,"\n[%s] ",tim);
  gtk_text_insert(GTK_TEXT(snd_error_history),(ss->sgx)->help_text_fnt,(ss->sgx)->black,(ss->sgx)->white,buf,-1);
  FREE(buf);
  FREE(tim);
#endif
  gtk_text_insert(GTK_TEXT(snd_error_history),(ss->sgx)->help_text_fnt,(ss->sgx)->black,(ss->sgx)->white,msg,-1);
  gtk_text_thaw(GTK_TEXT(snd_error_history));
}

void post_error_dialog(snd_state *ss, char *msg)
{
  if (!snd_error_dialog) create_snd_error_dialog(ss,TRUE); else raise_dialog(snd_error_dialog);
  add_to_error_history(ss,msg,TRUE);
}

void show_snd_errors(snd_state *ss)
{
  if (snd_error_dialog)
    {
      if (GTK_WIDGET_VISIBLE(snd_error_dialog))
	raise_dialog(snd_error_dialog);
      gtk_widget_show(snd_error_dialog);
    }
  else post_error_dialog(ss,"no errors yet");
}

static int yes_or_no = 0;
static GtkWidget *yes_or_no_dialog = NULL;
static GtkWidget *yn_label;

static void YesCallback(GtkWidget *w,gpointer clientData) {gtk_widget_hide(yes_or_no_dialog); yes_or_no=1;}
static void NoCallback(GtkWidget *w,gpointer clientData) {gtk_widget_hide(yes_or_no_dialog); yes_or_no=0;}
static void delete_yes_or_no_dialog(GtkWidget *w,GdkEvent *event,gpointer clientData) {gtk_widget_hide(yes_or_no_dialog); yes_or_no=1;}

int snd_yes_or_no_p(snd_state *ss, const char *format, ...)
{
  GtkWidget *yes_button,*no_button;

  char *yes_buf;
#if HAVE_VPRINTF
  va_list ap;
  yes_buf = (char *)CALLOC(256,sizeof(char));
  va_start(ap,format);
  vsprintf(yes_buf,format,ap);
  va_end(ap);
#else
  yes_buf = (char *)CALLOC(256,sizeof(char));
  sprintf(yes_buf,"%s...[you need vprintf]",format);
#endif

  yes_or_no = 0;
  if (!yes_or_no_dialog)
    {
      yes_or_no_dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(yes_or_no_dialog),"delete_event",GTK_SIGNAL_FUNC(delete_yes_or_no_dialog),(gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(yes_or_no_dialog),STR_Big_Trouble);
      gtk_window_set_policy(GTK_WINDOW(yes_or_no_dialog),TRUE,TRUE,FALSE); /* allow shrink or grow */
      set_background(yes_or_no_dialog,(ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(yes_or_no_dialog),4);
      gtk_widget_realize(yes_or_no_dialog);
      add_dialog(ss,yes_or_no_dialog);
      gtk_widget_set_usize(GTK_WIDGET(yes_or_no_dialog),180,100);

      yes_button = gtk_button_new_with_label(STR_Yes);
      no_button = gtk_button_new_with_label(STR_No);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area),yes_button,FALSE,TRUE,10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area),no_button,FALSE,TRUE,10);
      gtk_signal_connect(GTK_OBJECT(yes_button),"clicked",GTK_SIGNAL_FUNC(YesCallback),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(no_button),"clicked",GTK_SIGNAL_FUNC(NoCallback),(gpointer)ss);
      set_pushed_button_colors(no_button,ss);
      set_pushed_button_colors(yes_button,ss);
      gtk_widget_show(yes_button);
      gtk_widget_show(no_button);

      yn_label = gtk_label_new(yes_buf);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(yes_or_no_dialog)->vbox),yn_label);

      gtk_widget_show(yn_label);
    }
  else gtk_label_set_text(GTK_LABEL(yn_label),yes_buf);
  gtk_widget_show(yes_or_no_dialog);
  while (GTK_WIDGET_VISIBLE(yes_or_no_dialog)) check_for_event(ss);
  FREE(yes_buf);
  return(yes_or_no);
}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define Sg_error_dialog_widget  "sg-error-dialog-widget"

static SCM sg_error_dialog_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(snd_error_dialog)));}

void init_error_widgets(SCM local_doc)
{
  gh_new_procedure0_0(Sg_error_dialog_widget,sg_error_dialog_widget);
}

#endif
