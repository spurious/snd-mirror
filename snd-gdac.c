#include "snd.h"

static gboolean run_dac(gpointer dacData) 
{
  return(feed_dac((dac_manager *)dacData));
}

void set_play_in_progress (snd_state *ss, dac_manager *dac_m) 
{
  gtk_idle_add(run_dac,(gpointer)dac_m);
}
