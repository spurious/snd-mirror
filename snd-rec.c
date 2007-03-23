#include "snd.h"
#include "snd-rec.h"


static XEN g_recorder_dialog(void) 
{
  #define H_recorder_dialog "(" S_recorder_dialog "): start the Recorder"
  return(XEN_WRAP_WIDGET(record_file()));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_recorder_dialog_w, g_recorder_dialog)
#else
#define g_recorder_dialog_w g_recorder_dialog
#endif

void g_init_recorder(void)
{
  XEN_DEFINE_PROCEDURE(S_recorder_dialog, g_recorder_dialog_w, 0, 0, 0, H_recorder_dialog);
}


