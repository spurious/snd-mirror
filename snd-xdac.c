#include "snd.h"

static BACKGROUND_TYPE run_dac(XtPointer dacData) 
{
  return(feed_dac((dac_manager *)dacData));
}

void set_play_in_progress (snd_state *ss, dac_manager *dac_m) 
{
  XtAppAddWorkProc((ss->sgx)->mainapp,run_dac,(XtPointer)dac_m);
}
