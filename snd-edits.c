#include "snd.h"

#define NO_SUCH_DIRECTION XEN_ERROR_TYPE("no-such-direction")

/* these are used internally by the save-state process */
#define S_delete_samples_with_origin    "delete-samples-with-origin"
#define S_change_samples_with_origin    "change-samples-with-origin"
#define S_insert_samples_with_origin    "insert-samples-with-origin"

static int dont_edit(chan_info *cp) 
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(cp->edit_hook))
    res = run_or_hook(cp->edit_hook, XEN_EMPTY_LIST, S_edit_hook);
  return(XEN_TRUE_P(res));
}

static XEN save_hook;
static int dont_save(snd_info *sp, const char *newname)
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(save_hook))
    res = run_or_hook(save_hook,
		      XEN_LIST_2(C_TO_SMALL_XEN_INT(sp->index),
				 (newname) ? C_TO_XEN_STRING(newname) : XEN_FALSE),
		      S_save_hook);
  return(XEN_TRUE_P(res));
}

static void after_edit(chan_info *cp)
{
  if (XEN_HOOKED(cp->after_edit_hook))
    run_hook(cp->after_edit_hook, XEN_EMPTY_LIST, S_after_edit_hook);
}

void free_sound_list(chan_info *cp)
{
  int i;
  if (cp)
    {
      if (cp->sounds)
	{
	  if ((cp->sound) && (cp->sound->playing)) stop_playing_sound(cp->sound);
	  for (i = 0; i < cp->sound_size; i++)
	    if (cp->sounds[i]) 
	      cp->sounds[i] = free_snd_data(cp->sounds[i]);
	  FREE(cp->sounds);
	  cp->sounds = NULL;
	}
      cp->sound_ctr = -1;
      cp->sound_size = 0;
    }
}

static void release_pending_sounds(chan_info *cp, int edit_ctr)
{
  /* look for buffers or temp files that are no longer reachable after pruning the edit tree */
  int i;
  snd_data *sf;
  if ((cp) && (cp->sounds))
    {
      if ((cp->sound) && (cp->sound->playing)) 
	stop_playing_sound(cp->sound);
      for (i = 0; i < cp->sound_size; i++)
	{
	  sf = cp->sounds[i];
	  if (sf)
	    {
	      if (sf->edit_ctr >= edit_ctr)
		cp->sounds[i] = free_snd_data(sf);
	      else cp->sound_ctr = i;
	    }
	}
    }
}

#define EDIT_ALLOC_SIZE 32
/* EDIT_ALLOC_SIZE is the allocation amount (pointers) each time cp->sounds is (re)allocated */

static void prepare_sound_list (chan_info *cp)
{
  int i;
  cp->sound_ctr++;
  /* this is the only place the sound set is incremented */
  if (cp->sound_ctr >= cp->sound_size)
    {
      cp->sound_size += EDIT_ALLOC_SIZE;
      cp->sounds = (snd_data **)REALLOC(cp->sounds, cp->sound_size * sizeof(snd_data *));
      for (i = cp->sound_ctr; i < cp->sound_size; i++) cp->sounds[i] = NULL;
    }
  if (cp->sounds[cp->sound_ctr]) 
    {
      if ((cp->sound) && (cp->sound->playing)) stop_playing_sound(cp->sound);
      cp->sounds[cp->sound_ctr] = free_snd_data(cp->sounds[cp->sound_ctr]);
    }
}

static int add_sound_buffer_to_edit_list(chan_info *cp, mus_sample_t *data, int len)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(data, len, cp->edit_ctr);
  return(cp->sound_ctr);
}

static int add_sound_file_to_edit_list(chan_info *cp, const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int chan)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_file(name, io, hdr, temp, cp->edit_ctr, chan);
  return(cp->sound_ctr);
}


void free_ptree_list(chan_info *cp)
{
  int i;
  if (cp->ptrees)
    {
      for (i = 0; i < cp->ptree_size; i++)
 	{
	  if (cp->ptrees[i]) 
	    cp->ptrees[i] = free_ptree(cp->ptrees[i]);
	  if (XEN_PROCEDURE_P(cp->ptree_inits[i]))
	    snd_unprotect(cp->ptree_inits[i]);
	  if (XEN_PROCEDURE_P(cp->xens[i]))
	    snd_unprotect(cp->xens[i]);
	}
      FREE(cp->ptrees);
      cp->ptrees = NULL;
      FREE(cp->xens);
      cp->xens = NULL;
      FREE(cp->ptree_inits);
      cp->ptree_inits = NULL;
    }
  cp->ptree_ctr = -1;
  cp->ptree_size = 0;
}

static int add_ptree(chan_info *cp)
{
  int i;
  if (cp->ptrees)
    for (i = 0; i <= cp->ptree_ctr; i++)
      if ((cp->ptrees[i] == NULL) &&
	  (XEN_FALSE_P(cp->xens[i])))
	return(i);
  cp->ptree_ctr++;
  if (cp->ptree_ctr >= cp->ptree_size)
    {
      cp->ptree_size += EDIT_ALLOC_SIZE;
      if (cp->ptrees)
	{
	  cp->ptrees = (void **)REALLOC(cp->ptrees, cp->ptree_size * sizeof(void *));
	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->ptrees[i] = NULL;
 	  cp->ptree_inits = (XEN *)REALLOC(cp->ptree_inits, cp->ptree_size * sizeof(XEN));
 	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->ptree_inits[i] = XEN_FALSE;
 	  cp->xens = (XEN *)REALLOC(cp->xens, cp->ptree_size * sizeof(XEN));
 	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->xens[i] = XEN_FALSE;
 	}
       else 
 	{
 	  cp->ptrees = (void **)CALLOC(cp->ptree_size, sizeof(void *));
 	  cp->ptree_inits = (XEN *)CALLOC(cp->ptree_size, sizeof(XEN));
 	  for (i = 0; i < cp->ptree_size; i++) cp->ptree_inits[i] = XEN_FALSE;
 	  cp->xens = (XEN *)CALLOC(cp->ptree_size, sizeof(XEN));
 	  for (i = 0; i < cp->ptree_size; i++) cp->xens[i] = XEN_FALSE;
	}
    }
  return(cp->ptree_ctr);
}


static ed_list *free_ed_list(ed_list *ed, chan_info *cp);
static void prune_edits(chan_info *cp, int edpt)
{
  int es = 0;

  int i;
  if (cp->edits[edpt]) 
    {
      if ((cp->state) && (cp->state->deferred_regions > 0))
	sequester_deferred_regions(cp, edpt - 1);
      for (i = edpt; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = free_ed_list(cp->edits[i], cp);
	  if (cp->amp_envs[i]) es++;
	  cp->amp_envs[i] = free_amp_env(cp, i);
	}
      release_pending_marks(cp, edpt);
      release_pending_mixes(cp, edpt);
      release_pending_sounds(cp, edpt);
      release_dangling_readers(cp, edpt);
      reflect_no_more_redo_in_menu();
    }
}

static void prepare_edit_list(chan_info *cp, off_t len, int pos, const char *caller)
{
  /* pos is the edit position the current edit is referring to --
   *   we normally can't set up an edit list entry that refers to a situation
   *   that will be clobbered by prune_edits below
   */
  int i;
  snd_info *sp;
  if (pos > cp->edit_ctr)
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING("edpos: ~A, ~A chan ~A has ~A edits"),
			 XEN_LIST_4(C_TO_XEN_INT(pos),
				    C_TO_XEN_STRING(cp->sound->short_filename),
				    C_TO_XEN_INT(cp->chan),
				    C_TO_XEN_INT(cp->edit_ctr))));
  sp = cp->sound;
  stop_amp_env(cp);
  if ((sp) && (sp->playing)) stop_playing_sound(sp);
  cp->edit_ctr++;
  if (cp->edit_ctr >= cp->edit_size)
    {
      cp->edit_size += EDIT_ALLOC_SIZE;
      if (!cp->edits) cp->edits = (ed_list **)CALLOC(cp->edit_size, sizeof(ed_list *));
      else cp->edits = (ed_list **)REALLOC(cp->edits, cp->edit_size * sizeof(ed_list *));
      if (!cp->samples) cp->samples = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
      else cp->samples = (off_t *)REALLOC(cp->samples, cp->edit_size * sizeof(off_t));
      if (!cp->cursors) cp->cursors = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
      else cp->cursors = (off_t *)REALLOC(cp->cursors, cp->edit_size * sizeof(off_t));
      if (!(cp->amp_envs)) cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
      else cp->amp_envs = (env_info **)REALLOC(cp->amp_envs, cp->edit_size * sizeof(env_info *));
      for (i = cp->edit_ctr; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = NULL; 
	  cp->amp_envs[i] = NULL; 
	  cp->samples[i] = 0;
	  cp->cursors[i] = 0;
	}
    }
  prune_edits(cp, cp->edit_ctr);
  reflect_undo_ok_in_menu();
  CURRENT_SAMPLES(cp) = len;
  if (cp->edit_ctr > 0)
    CURSOR(cp) = cp->cursors[cp->edit_ctr - 1];
}

static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  off_t samps;
  ap = cp->axis;
  if (ap)
    {
      samps = CURRENT_SAMPLES(cp);
      ap->xmax = (double)samps / (double)SND_SRATE(cp->sound);
      ap->x_ambit = ap->xmax - ap->xmin;
      if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
      if ((samps == 0) || (ap->no_data))
	{
	  ap->no_data = (samps == 0);
	  if (ap->xlabel) FREE(ap->xlabel);
	  if (samps == 0) 
	    ap->xlabel = copy_string(_("(no data)")); 
	  else ap->xlabel = copy_string(_("time"));
	}
      set_x_bounds(ap);
    }
}

void reflect_file_change_in_label(chan_info *cp)
{
  snd_info *sp;
  char *starred_name;
  int len;
  if (cp->edit_ctr == 0) return;
  sp = cp->sound;
  if (sp->sgx == NULL) return;
  if (!(cp->squelch_update))
    {
      len = strlen(shortname(sp)) + 16;
      starred_name = (char *)CALLOC(len, sizeof(char));
      strcpy(starred_name, shortname_indexed(sp));
      if (sp->read_only) 
	strcat(starred_name, "(*)");
      else strcat(starred_name, "*");
      set_sound_pane_file_label(sp, starred_name);
      FREE(starred_name);
    }
}

static void check_for_first_edit(chan_info *cp)
{
  if ((cp->cgx) && (cp->edit_ctr == 1)) /* first edit on this file (?) */
    {
      reflect_file_change_in_menu();
      reflect_file_change_in_label(cp);
    }
}

static XEN save_state_hook = XEN_FALSE;

char *run_save_state_hook(char *filename)
{
  XEN result = XEN_FALSE;
  if (XEN_HOOKED(save_state_hook))
    {
#if HAVE_GUILE
      XEN procs = XEN_HOOK_PROCEDURES(save_state_hook);
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_CALL_1(XEN_CAR(procs),
			      C_TO_XEN_STRING(filename),
			      "save state hook");
	  if (XEN_STRING_P(result))
	    {
	      FREE(filename);
	      filename = copy_string(XEN_TO_C_STRING(result));
	    }
	  procs = XEN_CDR (procs);
	}
#else
      result = XEN_CALL_1(save_state_hook,
			  C_TO_XEN_STRING(filename),
			  "save state hook");
      if (XEN_STRING_P(result))
	{
	  FREE(filename);
	  filename = copy_string(XEN_TO_C_STRING(result));
	}
#endif
    }
  return(filename);
}




/* -------- EDIT LISTS --------
 *
 * each channel has a list of lists containing the current edit history and the associated sound temp files or buffers
 * undo: back up current list position
 * redo: push position foward
 * No actual changes are flushed out to the file system until the file is saved.
 *
 * the editing possibilities are insert, change, delete, scaling, zero, [x]ramp[2,3], ptree(xen). 
 * All input goes through these lists (with minor exceptions -- see chn_sample below).
 *
 * The accessors are highly optimized (split into numerous choices) since everything else in Snd
 *   goes through the accessors to get at the data.  My tests indicate that even a lowly old
 *   267 MHz PC (and equivalent Sun) can play stereo 44KHz through the double xramp op (or ptree2!)
 *   without glitches, and on a modern machine the readers scarcely register in loadav.
 *
 * to be more precise (1.7 GHz Pentium):
 *
 *                   float               int
 *    straight     38   0.001         31    0.001 ; 28 20 if buffer
 *    ramp         40   0.001         33    0.001 
 *    ramp2        51   0.001         48    0.001 
 *    ramp3        54   0.001         52    0.001 
 *    ptree        86   0.002         84    0.002 ; y * 0.5
 *    ptreec       133  0.003         131   0.003 ; y * vct-ref
 *    ptree2       168  0.004         159   0.004 
 *    xramp        376  0.009         371   0.009 
 *    cosine       476  0.012         511   0.012 ; cosine-channel-via-ptree
 *    xramp2       682  0.016         672   0.016 
 *    xen          3600 0.087         3650  0.088 
 *
 * so even the xen case is running 10X real-time! There are no glitches in playback,
 *   and only slight hesitation in some redisplays -- where the peak-env is about to kick in.
 *
 * On the aforementioned old Sun:
 *
 *    straight     451  0.011         332   0.008 
 *    ramp         459  0.011         392   0.009 
 *    ramp2        375  0.009         359   0.009 
 *    ramp3        382  0.009         438   0.011 
 *    ptree        714  0.017         1216  0.029 
 *    ptreec       929  0.022         953   0.023 
 *    ptree2       1107 0.027         1140  0.028 
 *    xramp        1004 0.024         840   0.020 
 *    xramp2       2087 0.050         2599  0.063 
 *    cosine       4337 0.105         4088  0.099 
 *    xen          22945 0.554        23205 0.561 
 */

typedef struct {
  off_t out,       /* running segment location within current overall edited data */
        beg,       /* index into the associated data => start point of data used in current segment */
        end;       /* index into the associated data => end point of data used in current segment */
  Float scl,       /* segment scaler */
        rmp0,      /* first val of ramp */
        rmp1,      /* end val of ramp */
        rmp2, rmp3,  /* ramp2 vals */
        rmp4, rmp5,  /* ramp3 vals */
        pscl, pscl1, /* scales the arg to the ptree */
        scaler,    /* exp-env segment scaler */
        offset,    /* exp-env segment offset */
        scaler2, offset2;
  int   snd,       /* either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END|ZERO_MARK */
        typ,       /* code for accessor choice (ED_SIMPLE etc) */
        ptree_loc, /* index into the cp->ptrees array */
        ptree_loc1;
  off_t ptree_pos, /* segment position within original at time of ptree edit */
        ptree_dur, /* original (unfragmented) segment length */
        ptree_pos1, ptree_dur1;
} ed_fragment;

/* these are the virtual-op accessor choices */
enum {ED_SIMPLE, ED_ZERO,

      ED_RAMP, ED_RAMP2, ED_RAMP3, 
      ED_XRAMP, ED_XRAMP2, ED_RAMP_XRAMP, ED_XRAMP_RAMP, ED_RAMP2_XRAMP, ED_RAMP_XRAMP2, ED_RAMP_XRAMP_RAMP,

      ED_PTREE, ED_PTREE_RAMP, ED_PTREE_XRAMP, ED_PTREE_ZERO, ED_PTREE_RAMP2, ED_PTREE_RAMP3,
      ED_PTREE_RAMP_XRAMP, ED_PTREE_RAMP2_XRAMP, ED_PTREE_XRAMP_RAMP, ED_PTREE_RAMP_XRAMP_RAMP, ED_PTREE_XRAMP2,
      ED_RAMP_PTREE, ED_RAMP_PTREE_ZERO, 
      ED_RAMP2_PTREE, ED_RAMP2_PTREE_ZERO, 
      ED_RAMP3_PTREE, ED_RAMP3_PTREE_ZERO,
      ED_XRAMP_PTREE, ED_XRAMP_PTREE_ZERO, 
      ED_XRAMP2_PTREE, ED_XRAMP2_PTREE_ZERO, 
      ED_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_RAMP, ED_RAMP_PTREE_RAMP2,
      ED_RAMP_PTREE_XRAMP, ED_XRAMP_PTREE_RAMP, ED_RAMP2_PTREE_XRAMP,
      ED_RAMP_XRAMP_PTREE, ED_RAMP_XRAMP_PTREE_ZERO,
      ED_RAMP2_XRAMP_PTREE, ED_RAMP2_XRAMP_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_ZERO,
      ED_RAMP_XRAMP_RAMP_PTREE, ED_RAMP_XRAMP_RAMP_PTREE_ZERO,

      ED_PTREE2, ED_PTREE2_RAMP, ED_PTREE2_ZERO, 
      ED_RAMP_PTREE2, ED_RAMP_PTREE2_ZERO, ED_RAMP2_PTREE2, ED_RAMP2_PTREE2_ZERO, ED_RAMP3_PTREE2, ED_RAMP3_PTREE2_ZERO, 
      ED_XRAMP_PTREE2, ED_XRAMP_PTREE2_ZERO, 
      ED_PTREE_RAMP_PTREE, ED_PTREE_RAMP_PTREE_ZERO, 
      ED_PTREE_RAMP2_PTREE, ED_PTREE_RAMP2_PTREE_ZERO, 
      ED_PTREE_RAMP3_PTREE, ED_PTREE_RAMP3_PTREE_ZERO,
      ED_PTREE2_RAMP2, ED_PTREE2_RAMP3, ED_PTREE2_XRAMP, ED_PTREE_XRAMP_PTREE, ED_PTREE_XRAMP_PTREE_ZERO,
      ED_PTREE_RAMP2_PTREE_RAMP, ED_PTREE_RAMP_PTREE_RAMP2, ED_RAMP_PTREE_RAMP_PTREE_RAMP, ED_PTREE_RAMP_PTREE_RAMP,

      ED_XEN, ED_XEN_RAMP, ED_XEN_XRAMP, ED_XEN_ZERO, ED_XEN_RAMP2, ED_XEN_RAMP3,
      ED_RAMP_XEN, ED_RAMP2_XEN, ED_RAMP3_XEN, ED_RAMP_XEN_ZERO, ED_RAMP2_XEN_ZERO, ED_RAMP3_XEN_ZERO,
      ED_XRAMP_XEN, ED_XRAMP_XEN_ZERO,
      ED_RAMP_XEN_RAMP, ED_RAMP2_XEN_RAMP, ED_RAMP_XEN_RAMP2
};

#define PTREE_OP(Typ) ((Typ) >= ED_PTREE)
#define XEN_OP(Typ) ((Typ) >= ED_XEN)
#define PTREE2_OP(Typ) (((Typ) >= ED_PTREE2) && ((Typ) < ED_XEN))

static bool ZERO_OP(int type)
{
  switch (type)
    {
    case ED_ZERO:
    case ED_XEN_ZERO:
    case ED_PTREE2_ZERO:
    case ED_PTREE_ZERO:
    case ED_RAMP_PTREE_ZERO: case ED_RAMP2_PTREE_ZERO: case ED_RAMP3_PTREE_ZERO:
    case ED_XRAMP_PTREE_ZERO: case ED_XRAMP2_PTREE_ZERO:
    case ED_RAMP_XEN_ZERO: case ED_RAMP2_XEN_ZERO: case ED_RAMP3_XEN_ZERO:
    case ED_XRAMP_XEN_ZERO:
    case ED_RAMP2_PTREE2_ZERO: case ED_RAMP_PTREE2_ZERO: case ED_XRAMP_PTREE2_ZERO: case ED_RAMP3_PTREE2_ZERO:
    case ED_RAMP_XRAMP_PTREE_ZERO: case ED_RAMP2_XRAMP_PTREE_ZERO: case ED_XRAMP_RAMP_PTREE_ZERO: case ED_RAMP_XRAMP_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP_PTREE_ZERO: case ED_PTREE_RAMP2_PTREE_ZERO: case ED_PTREE_RAMP3_PTREE_ZERO: case ED_PTREE_XRAMP_PTREE_ZERO:
      return(true);
      break;
    }
  return(false);
}
static bool RAMP_OP(int type)
{
  switch (type)
    {
    case ED_RAMP: case ED_RAMP2: case ED_RAMP3:
    case ED_XRAMP: case ED_XRAMP2:
    case ED_XRAMP_RAMP: case ED_RAMP_XRAMP_RAMP: case ED_RAMP_XRAMP:
    case ED_RAMP_XRAMP2: case ED_RAMP2_XRAMP:
    case ED_RAMP_PTREE: case ED_RAMP_PTREE_ZERO: 
    case ED_RAMP2_PTREE: case ED_RAMP2_PTREE_ZERO: 
    case ED_RAMP3_PTREE: case ED_RAMP3_PTREE_ZERO:
    case ED_XRAMP_PTREE: case ED_XRAMP_PTREE_ZERO: 
    case ED_XRAMP2_PTREE: case ED_XRAMP2_PTREE_ZERO: 
    case ED_PTREE2_RAMP:
    case ED_XEN_RAMP: case ED_XEN_RAMP2: case ED_XEN_RAMP3:
    case ED_XEN_XRAMP:
    case ED_RAMP_XEN: case ED_RAMP2_XEN: case ED_RAMP3_XEN:
    case ED_XRAMP_XEN:
    case ED_RAMP_XEN_ZERO: case ED_RAMP2_XEN_ZERO: case ED_RAMP3_XEN_ZERO:
    case ED_XRAMP_XEN_ZERO:
    case ED_PTREE_RAMP: case ED_PTREE_RAMP2: case ED_PTREE_RAMP3:
    case ED_PTREE_XRAMP:
    case ED_RAMP_PTREE2: case ED_RAMP_PTREE2_ZERO: case ED_RAMP2_PTREE2: case ED_RAMP2_PTREE2_ZERO: 
    case ED_RAMP3_PTREE2: case ED_RAMP3_PTREE2_ZERO: 
    case ED_XRAMP_PTREE2: case ED_XRAMP_PTREE2_ZERO:
    case ED_RAMP_PTREE_RAMP: case ED_RAMP2_PTREE_RAMP: case ED_RAMP_PTREE_RAMP2:
    case ED_RAMP_PTREE_XRAMP: case ED_XRAMP_PTREE_RAMP: case ED_RAMP2_PTREE_XRAMP:
    case ED_RAMP_XRAMP_PTREE: case ED_RAMP_XRAMP_PTREE_ZERO:
    case ED_RAMP2_XRAMP_PTREE: case ED_RAMP2_XRAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_ZERO:
    case ED_RAMP_XRAMP_RAMP_PTREE: case ED_RAMP_XRAMP_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP_XRAMP: case ED_PTREE_RAMP2_XRAMP: case ED_PTREE_XRAMP_RAMP: case ED_PTREE_RAMP_XRAMP_RAMP:
    case ED_PTREE_RAMP_PTREE: case ED_PTREE_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP2_PTREE: case ED_PTREE_RAMP2_PTREE_ZERO:
    case ED_PTREE_RAMP3_PTREE: case ED_PTREE_RAMP3_PTREE_ZERO:
    case ED_PTREE2_RAMP2: case ED_PTREE2_RAMP3: case ED_PTREE2_XRAMP:
    case ED_PTREE_XRAMP_PTREE: case ED_PTREE_XRAMP_PTREE_ZERO:
    case ED_PTREE_RAMP2_PTREE_RAMP: case ED_PTREE_RAMP_PTREE_RAMP2: case ED_RAMP_PTREE_RAMP_PTREE_RAMP:
    case ED_PTREE_XRAMP2:
    case ED_PTREE_RAMP_PTREE_RAMP:
    case ED_RAMP_XEN_RAMP: case ED_RAMP2_XEN_RAMP: case ED_RAMP_XEN_RAMP2:
      return(true);
      break;
    }
  return(false);
}
static bool RAMP2_OP(int type)
{
  switch (type)
    {
    case ED_RAMP2: case ED_RAMP3:
    case ED_XRAMP2:
    case ED_RAMP_XRAMP: case ED_RAMP_XRAMP2: case ED_RAMP2_XRAMP: case ED_XRAMP_RAMP: case ED_RAMP_XRAMP_RAMP:
    case ED_RAMP2_PTREE: case ED_RAMP2_PTREE_ZERO: 
    case ED_RAMP3_PTREE: case ED_RAMP3_PTREE_ZERO:
    case ED_XRAMP2_PTREE: case ED_XRAMP2_PTREE_ZERO: 
    case ED_XEN_RAMP2: case ED_XEN_RAMP3:
    case ED_PTREE_RAMP2: case ED_PTREE_RAMP3:
    case ED_RAMP2_XEN: case ED_RAMP3_XEN:
    case ED_RAMP2_XEN_ZERO: case ED_RAMP3_XEN_ZERO:
    case ED_RAMP2_PTREE2: case ED_RAMP2_PTREE2_ZERO: 
    case ED_RAMP3_PTREE2: case ED_RAMP3_PTREE2_ZERO: 
    case ED_RAMP_PTREE_RAMP: case ED_RAMP2_PTREE_RAMP: case ED_RAMP_PTREE_RAMP2:
    case ED_RAMP_PTREE_XRAMP: case ED_XRAMP_PTREE_RAMP: case ED_RAMP2_PTREE_XRAMP:
    case ED_RAMP_XRAMP_PTREE: case ED_RAMP_XRAMP_PTREE_ZERO:
    case ED_RAMP2_XRAMP_PTREE: case ED_RAMP2_XRAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_ZERO:
    case ED_RAMP_XRAMP_RAMP_PTREE: case ED_RAMP_XRAMP_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP_XRAMP: case ED_PTREE_RAMP2_XRAMP: case ED_PTREE_XRAMP_RAMP: case ED_PTREE_RAMP_XRAMP_RAMP:
    case ED_PTREE_RAMP2_PTREE: case ED_PTREE_RAMP2_PTREE_ZERO:
    case ED_PTREE_RAMP3_PTREE: case ED_PTREE_RAMP3_PTREE_ZERO:
    case ED_PTREE2_RAMP2: case ED_PTREE2_RAMP3:
    case ED_PTREE_RAMP2_PTREE_RAMP: case ED_PTREE_RAMP_PTREE_RAMP2: case ED_RAMP_PTREE_RAMP_PTREE_RAMP:
    case ED_PTREE_XRAMP2:
    case ED_PTREE_RAMP_PTREE_RAMP:
    case ED_RAMP_XEN_RAMP: case ED_RAMP2_XEN_RAMP: case ED_RAMP_XEN_RAMP2:
      return(true);
      break;
    }
  return(false);
}

static bool RAMP3_OP(int type)
{
  switch (type)
    {
    case ED_RAMP3:
    case ED_RAMP2_XRAMP:
    case ED_RAMP_XRAMP_RAMP:
    case ED_RAMP_XRAMP2:
    case ED_PTREE_RAMP3:
    case ED_XEN_RAMP3:
    case ED_RAMP3_XEN: case ED_RAMP3_XEN_ZERO:
    case ED_RAMP3_PTREE: case ED_RAMP3_PTREE_ZERO:
    case ED_RAMP2_PTREE_RAMP: case ED_RAMP_PTREE_RAMP2: case ED_RAMP2_PTREE_XRAMP:
    case ED_RAMP2_XRAMP_PTREE: case ED_RAMP2_XRAMP_PTREE_ZERO:
    case ED_RAMP_XRAMP_RAMP_PTREE: case ED_RAMP_XRAMP_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP2_XRAMP: case ED_PTREE_RAMP_XRAMP_RAMP:
    case ED_PTREE_RAMP3_PTREE: case ED_PTREE_RAMP3_PTREE_ZERO:
    case ED_PTREE2_RAMP3:
    case ED_PTREE_RAMP2_PTREE_RAMP: case ED_PTREE_RAMP_PTREE_RAMP2: case ED_RAMP_PTREE_RAMP_PTREE_RAMP:
    case ED_RAMP2_XEN_RAMP: case ED_RAMP_XEN_RAMP2:
      return(true);
    }
  return(false);
}

#define FRAGMENTS(Ed)                       (ed_fragment **)((Ed)->fragments)
#define FRAGMENT(Ed, Pos)                  ((ed_fragment **)((Ed)->fragments))[Pos]
#define FRAGMENT_GLOBAL_POSITION(Ed, Pos)  ((ed_fragment **)((Ed)->fragments))[Pos]->out
#define FRAGMENT_LOCAL_POSITION(Ed, Pos)   ((ed_fragment **)((Ed)->fragments))[Pos]->beg
#define FRAGMENT_LOCAL_END(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->end
#define FRAGMENT_SCALER(Ed, Pos)           ((ed_fragment **)((Ed)->fragments))[Pos]->scl
#define FRAGMENT_TYPE(Ed, Pos)             ((ed_fragment **)((Ed)->fragments))[Pos]->typ
#define FRAGMENT_SOUND(Ed, Pos)            ((ed_fragment **)((Ed)->fragments))[Pos]->snd
#define FRAGMENT_RAMP_BEG(Ed, Pos)         ((ed_fragment **)((Ed)->fragments))[Pos]->rmp0
#define FRAGMENT_RAMP_END(Ed, Pos)         ((ed_fragment **)((Ed)->fragments))[Pos]->rmp1
#define FRAGMENT_RAMP2_BEG(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp2
#define FRAGMENT_RAMP2_END(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp3
#define FRAGMENT_RAMP3_BEG(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp4
#define FRAGMENT_RAMP3_END(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp5
#define FRAGMENT_XRAMP_SCALER(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->scaler
#define FRAGMENT_XRAMP_OFFSET(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->offset
#define FRAGMENT_XRAMP_SCALER2(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->scaler2
#define FRAGMENT_XRAMP_OFFSET2(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->offset2
#define FRAGMENT_PTREE_SCALER(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->pscl
#define FRAGMENT_PTREE2_SCALER(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->pscl1
#define FRAGMENT_PTREE_INDEX(Ed, Pos)      ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_loc
#define FRAGMENT_PTREE2_INDEX(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_loc1
#define FRAGMENT_PTREE_DUR(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_dur
#define FRAGMENT_PTREE2_DUR(Ed, Pos)       ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_dur1
#define FRAGMENT_PTREE_POSITION(Ed, Pos)   ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_pos
#define FRAGMENT_PTREE2_POSITION(Ed, Pos)  ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_pos1
#define FRAGMENT_LENGTH(Ed, Pos)           (FRAGMENT_LOCAL_END(Ed, Pos) - FRAGMENT_LOCAL_POSITION(Ed, Pos) + 1)

#define READER_GLOBAL_POSITION(Sf)  ((ed_fragment *)((Sf)->cb))->out
#define READER_LOCAL_POSITION(Sf)   ((ed_fragment *)((Sf)->cb))->beg
#define READER_LOCAL_END(Sf)        ((ed_fragment *)((Sf)->cb))->end
#define READER_SCALER(Sf)           ((ed_fragment *)((Sf)->cb))->scl
#define READER_TYPE(Sf)             ((ed_fragment *)((Sf)->cb))->typ
#define READER_SOUND(Sf)            ((ed_fragment *)((Sf)->cb))->snd
#define READER_RAMP_BEG(Sf)         ((ed_fragment *)((Sf)->cb))->rmp0
#define READER_RAMP_END(Sf)         ((ed_fragment *)((Sf)->cb))->rmp1
#define READER_RAMP2_BEG(Sf)        ((ed_fragment *)((Sf)->cb))->rmp2
#define READER_RAMP2_END(Sf)        ((ed_fragment *)((Sf)->cb))->rmp3
#define READER_RAMP3_BEG(Sf)        ((ed_fragment *)((Sf)->cb))->rmp4
#define READER_RAMP3_END(Sf)        ((ed_fragment *)((Sf)->cb))->rmp5
#define READER_XRAMP_SCALER(Sf)     ((ed_fragment *)((Sf)->cb))->scaler
#define READER_XRAMP_OFFSET(Sf)     ((ed_fragment *)((Sf)->cb))->offset
#define READER_XRAMP_SCALER2(Sf)    ((ed_fragment *)((Sf)->cb))->scaler2
#define READER_XRAMP_OFFSET2(Sf)    ((ed_fragment *)((Sf)->cb))->offset2
#define READER_PTREE_SCALER(Sf)     ((ed_fragment *)((Sf)->cb))->pscl
#define READER_PTREE2_SCALER(Sf)    ((ed_fragment *)((Sf)->cb))->pscl1
#define READER_PTREE_INDEX(Sf)      ((ed_fragment *)((Sf)->cb))->ptree_loc
#define READER_PTREE2_INDEX(Sf)     ((ed_fragment *)((Sf)->cb))->ptree_loc1
#define READER_PTREE_DUR(Sf)        ((ed_fragment *)((Sf)->cb))->ptree_dur
#define READER_PTREE2_DUR(Sf)       ((ed_fragment *)((Sf)->cb))->ptree_dur1
#define READER_PTREE_POSITION(Sf)   ((ed_fragment *)((Sf)->cb))->ptree_pos
#define READER_PTREE2_POSITION(Sf)  ((ed_fragment *)((Sf)->cb))->ptree_pos1
#define READER_LENGTH(Sf)           (READER_LOCAL_END(Sf) - READER_LOCAL_POSITION(Sf) + 1)

#define ED_GLOBAL_POSITION(Ed)  (Ed)->out
#define ED_LOCAL_POSITION(Ed)   (Ed)->beg
#define ED_LOCAL_END(Ed)        (Ed)->end
#define ED_SCALER(Ed)           (Ed)->scl
#define ED_TYPE(Ed)             (Ed)->typ
#define ED_SOUND(Ed)            (Ed)->snd
#define ED_RAMP_BEG(Ed)         (Ed)->rmp0
#define ED_RAMP_END(Ed)         (Ed)->rmp1
#define ED_RAMP2_BEG(Ed)        (Ed)->rmp2
#define ED_RAMP2_END(Ed)        (Ed)->rmp3
#define ED_RAMP3_BEG(Ed)        (Ed)->rmp4
#define ED_RAMP3_END(Ed)        (Ed)->rmp5
#define ED_XRAMP_SCALER(Ed)     (Ed)->scaler
#define ED_XRAMP_OFFSET(Ed)     (Ed)->offset
#define ED_XRAMP_SCALER2(Ed)    (Ed)->scaler2
#define ED_XRAMP_OFFSET2(Ed)    (Ed)->offset2
#define ED_PTREE_SCALER(Ed)     (Ed)->pscl
#define ED_PTREE2_SCALER(Ed)    (Ed)->pscl1
#define ED_PTREE_INDEX(Ed)      (Ed)->ptree_loc
#define ED_PTREE2_INDEX(Ed)     (Ed)->ptree_loc1
#define ED_PTREE_DUR(Ed)        (Ed)->ptree_dur
#define ED_PTREE2_DUR(Ed)       (Ed)->ptree_dur1
#define ED_PTREE_POSITION(Ed)   (Ed)->ptree_pos
#define ED_PTREE2_POSITION(Ed)  (Ed)->ptree_pos1
#define ED_LENGTH(Ed)           (ED_LOCAL_END(Ed) - ED_LOCAL_POSITION(Ed) + 1)

#define EDIT_LIST_END_MARK -2
#define EDIT_LIST_ZERO_MARK -1

enum {INSERTION_EDIT, DELETION_EDIT, CHANGE_EDIT, INITIALIZE_EDIT, SCALED_EDIT, ZERO_EDIT, RAMP_EDIT, PTREE_EDIT, XEN_EDIT};
static char *edit_names[10] = {"insert", "delete", "set", "init", "scale", "zero", "env", "ptree", "xen", ""};

static void display_ed_list(chan_info *cp, FILE *outp, int i, ed_list *ed, bool with_source)
{
  int len, j, type, index;
  snd_data *sd;
  if (ed == NULL)
    {
      fprintf(outp, "\n (NULL FRAGMENT at %d)", i);
      return;
    }
  if (i >= cp->edit_size)
    {
      fprintf(outp, "\n (BOGUS FRAGMENT at %d of %d)", i, cp->edit_size);
      return;
    }
  len = ed->size; /* number of fragments in this list */
  type = ed->edit_type;
  switch (type)
    {
    case INSERTION_EDIT:  fprintf(outp, "\n (insert " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                        break;
    case DELETION_EDIT:   fprintf(outp, "\n (delete " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                        break;
    case CHANGE_EDIT:     fprintf(outp, "\n (set " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                           break;
    case SCALED_EDIT:     fprintf(outp, "\n (scale " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                         break;
    case ZERO_EDIT:       fprintf(outp, "\n (silence " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                       break;
    case RAMP_EDIT:       fprintf(outp, "\n (ramp " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                          break;
    case XEN_EDIT:        fprintf(outp, "\n (xen[%d] " OFF_TD " " OFF_TD ") ", ed->ptree_location, ed->beg, ed->len);   break; 
    case PTREE_EDIT:      fprintf(outp, "\n (ptree[%d] " OFF_TD " " OFF_TD ") ", ed->ptree_location, ed->beg, ed->len); break; 
    case INITIALIZE_EDIT: fprintf(outp, "\n (begin) ");                                                                 break;
    }
  if (ed->origin) fprintf(outp, "; %s ", ed->origin);
  fprintf(outp, "[%d:%d]:", i, len);
  for (j = 0; j < len; j++)
    {
      index = FRAGMENT_SOUND(ed, j);
      if (index == EDIT_LIST_END_MARK)
	fprintf(outp, "\n   (at " OFF_TD ", end_mark)", FRAGMENT_GLOBAL_POSITION(ed, j));
      else
	{
	  fprintf(outp, "\n   (at " OFF_TD ", cp->sounds[%d][" OFF_TD ":" OFF_TD ", %f",
		  FRAGMENT_GLOBAL_POSITION(ed, j),
		  index,
		  FRAGMENT_LOCAL_POSITION(ed, j),
		  FRAGMENT_LOCAL_END(ed, j),
		  FRAGMENT_SCALER(ed, j));
	  if (RAMP_OP(FRAGMENT_TYPE(ed, j)))
	    {
	      fprintf(outp, ", %f -> %f", 
		      FRAGMENT_RAMP_BEG(ed, j),
		      FRAGMENT_RAMP_END(ed, j));
	      if (RAMP2_OP(FRAGMENT_TYPE(ed, j)))
		fprintf(outp, ", %f -> %f", 
			FRAGMENT_RAMP2_BEG(ed, j),
			FRAGMENT_RAMP2_END(ed, j));
	      if (RAMP3_OP(FRAGMENT_TYPE(ed, j)))
		fprintf(outp, ", %f -> %f", 
			FRAGMENT_RAMP3_BEG(ed, j),
			FRAGMENT_RAMP3_END(ed, j));
	      if (FRAGMENT_XRAMP_SCALER(ed, j) != 0.0) /* ptree or xramp cases */
		fprintf(outp, ", off: %f, scl: %f", 
			FRAGMENT_XRAMP_OFFSET(ed, j),
			FRAGMENT_XRAMP_SCALER(ed, j));
	      if (FRAGMENT_XRAMP_SCALER2(ed, j) != 0.0)
		fprintf(outp, ", off2: %f, scl2: %f", 
			FRAGMENT_XRAMP_OFFSET2(ed, j),
			FRAGMENT_XRAMP_SCALER2(ed, j));
	    }
	  if (PTREE2_OP(FRAGMENT_TYPE(ed, j)))
	    {
	      fprintf(outp, ", loc2: %d, pos2: " OFF_TD ", scl2: %f",
		      FRAGMENT_PTREE2_INDEX(ed, j),
		      FRAGMENT_PTREE2_POSITION(ed, j),
		      FRAGMENT_PTREE2_SCALER(ed, j));
	    }
	  if (PTREE_OP(FRAGMENT_TYPE(ed, j)))
	    {
	      XEN code;
	      fprintf(outp, ", loc: %d, pos: " OFF_TD ", scl: %f",
		      FRAGMENT_PTREE_INDEX(ed, j),
		      FRAGMENT_PTREE_POSITION(ed, j),
		      (float)(MUS_FLOAT_TO_FIX * FRAGMENT_PTREE_SCALER(ed, j)));
	      if (with_source)
		{
		  if (XEN_OP(FRAGMENT_TYPE(ed, j)))
		    {
		      code = cp->xens[FRAGMENT_PTREE_INDEX(ed, j)];
#if HAVE_GUILE
		      if ((code) && (XEN_PROCEDURE_P(code)))
			fprintf(outp, ", code: %s", XEN_AS_STRING(scm_procedure_source(code)));
#endif
		    }
		  else 
		    {
		      code = ptree_code(cp->ptrees[FRAGMENT_PTREE_INDEX(ed, j)]);
		      if (XEN_LIST_P(code))
			fprintf(outp, ", code: %s", XEN_AS_STRING(code));
		    }
#if HAVE_GUILE
		  code = cp->ptree_inits[FRAGMENT_PTREE_INDEX(ed, j)];
		  if (XEN_PROCEDURE_P(code))
		    fprintf(outp, ", init: %s", XEN_AS_STRING(scm_procedure_source(code)));
#endif
		}
	    }
	  fprintf(outp, "])");
	  if (index != EDIT_LIST_ZERO_MARK)
	    {
	      sd = cp->sounds[index];
	      if (sd == NULL) 
		fprintf(outp, " [nil!]");
	      else 
		if (sd->type == SND_DATA_FILE)
		  fprintf(outp, " [file: %s[%d]]", sd->filename, sd->chan);
		else 
		  if (sd->type == SND_DATA_BUFFER)
		    fprintf(outp, " [buf: " OFF_TD "] ", sd->len / sizeof(mus_sample_t));
		  else fprintf(outp, " [bogus!]");
	    }
	}
    }
  fprintf(outp, "\n");
}

off_t edit_changes_begin_at(chan_info *cp)
{
  ed_list *ed, *old_ed;
  int len, old_len, i, min_len;
  old_ed = cp->edits[cp->edit_ctr - 1];
  ed = cp->edits[cp->edit_ctr];
  old_len = old_ed->size;
  len = ed->size;
  if (len < old_len) 
    min_len = len;
  else min_len = old_len;
  for (i = 0; i < min_len; i ++)
    if ((FRAGMENT_GLOBAL_POSITION(ed, i) != FRAGMENT_GLOBAL_POSITION(old_ed, i)) || 
	(FRAGMENT_SOUND(ed, i) != FRAGMENT_SOUND(old_ed, i)) || 
	(FRAGMENT_LOCAL_POSITION(ed, i) != FRAGMENT_LOCAL_POSITION(old_ed, i)))
      return(FRAGMENT_GLOBAL_POSITION(ed, i));
  return(0);
}

off_t edit_changes_end_at(chan_info *cp)
{
  ed_list *ed, *old_ed;
  int len, old_len;
  old_ed = cp->edits[cp->edit_ctr - 1];
  ed = cp->edits[cp->edit_ctr];
  old_len = old_ed->size - 1;
  len = ed->size - 1;
  if (FRAGMENT_SOUND(ed, len) == EDIT_LIST_END_MARK) len--;
  if (FRAGMENT_SOUND(old_ed, old_len) == EDIT_LIST_END_MARK) old_len--;
  while ((len >= 0) && (old_len >= 0))
    {
      if ((FRAGMENT_SOUND(ed, len) == FRAGMENT_SOUND(old_ed, old_len)) &&
	  (FRAGMENT_LOCAL_END(ed, len) == FRAGMENT_LOCAL_END(old_ed, old_len)))
	{
	  if (FRAGMENT_LOCAL_POSITION(ed, len) != FRAGMENT_LOCAL_POSITION(old_ed, old_len))
	    return(cp->samples[cp->edit_ctr - 1] - (FRAGMENT_LOCAL_END(ed, len) - FRAGMENT_LOCAL_POSITION(ed, len)));
	  len--;
	  old_len--;
	}
      else break;
    }
  return(0);
}

static char edbuf[PRINT_BUFFER_SIZE];
char *edit_to_string(chan_info *cp, int edit)
{
  ed_list *ed;
  ed = cp->edits[edit];
  /* only for edit list in snd-xchn.c */
  mus_snprintf(edbuf, PRINT_BUFFER_SIZE, 
#if HAVE_RUBY
	       "%s : %s(" OFF_TD ", " OFF_TD ")", 
#else
	       "%s : (%s " OFF_TD " " OFF_TD ")", 
#endif
	       ed->origin, 
	       edit_names[ed->edit_type], 
	       ed->beg, ed->len);
  return(edbuf);
}

#if HAVE_RUBY
  #define TO_PROC_NAME(Str) xen_scheme_procedure_to_ruby(Str)
  #define PROC_OPEN "("
  #define PROC_SEP ", "
  #define VECTOR_OPEN "["
  #define VECTOR_CLOSE "]"
#else
  #define TO_PROC_NAME(Str) Str
  #define PROC_OPEN " "
  #define PROC_SEP " "
  #define VECTOR_OPEN "'#("
  #define VECTOR_CLOSE ")"
#endif

static void display_edits(chan_info *cp, FILE *outp, bool with_source)
{
  int i;
  fprintf(outp, "\nEDITS: %d\n", cp->edit_ctr);
  for (i = 0; i <= cp->edit_ctr; i++)
    display_ed_list(cp, outp, i, cp->edits[i], with_source);
}

#define BUFFER_NOT_FILE_LIMIT 64

static void edit_data_to_file(FILE *fd, ed_list *ed, chan_info *cp)
{
  snd_data *sd;
  snd_state *ss;
  char *newname;
  int i, snd;
  snd = ed->sound_location; /* sound_location read only here (modulo error reporting) */
  if (snd < cp->sound_size)
    {
      sd = cp->sounds[snd];
      ss = cp->state;
      if (sd->type == SND_DATA_BUFFER)
	{
	  if ((ed->len > BUFFER_NOT_FILE_LIMIT) && (save_dir(ss)))
	    {
	      newname = run_save_state_hook(shorter_tempnam(save_dir(ss), "snd_"));
	      mus_array_to_file(newname, sd->buffered_data, ed->len, 22050, 1);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      fprintf(fd, VECTOR_OPEN);
	      for (i = 0; i < ed->len; i++) 
		{
#if SNDLIB_USE_FLOATS
		  fprintf(fd, "%f" PROC_SEP, sd->buffered_data[i]);
#else
		  fprintf(fd, "%d" PROC_SEP, sd->buffered_data[i]);
#endif
		}
	      fprintf(fd, VECTOR_CLOSE);
	    }
	}
      else
	{
	  if (save_dir(ss))
	    {
	      newname = run_save_state_hook(shorter_tempnam(save_dir(ss), "snd_"));
	      copy_file(sd->filename, newname);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      /* read at very low level and write to (text) history files as sample list */
	      int ifd, bufnum;
	      off_t idataloc, n, samples, cursamples, sample;
	      mus_sample_t *buffer;
	      mus_sample_t **ibufs;
	      fprintf(fd, VECTOR_OPEN);
	      ifd = mus_file_open_read(sd->filename);
	      if (ifd == -1) 
		{
		  snd_error(_("save edits: can't open %s: %s!"),
			    sd->filename,
			    strerror(errno));
		  return;
		}
	      idataloc = mus_sound_data_location(sd->filename);
	      mus_file_open_descriptors(ifd,
					sd->filename,
					mus_sound_data_format(sd->filename),
					mus_sound_datum_size(sd->filename),
					idataloc,
					mus_sound_chans(sd->filename),
					mus_sound_header_type(sd->filename));
	      samples = mus_sound_samples(sd->filename);
	      lseek(ifd, idataloc, SEEK_SET);
	      ibufs = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
	      ibufs[0] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t));
	      bufnum = (FILE_BUFFER_SIZE);
	      sample = 0;
	      for (n = 0; n < samples; n += bufnum)
		{
		  if ((n + bufnum) < samples) cursamples = bufnum; else cursamples = (samples - n);
		  mus_file_read(ifd, 0, cursamples - 1, 1, ibufs);
		  buffer = (mus_sample_t *)(ibufs[0]);
		  for (i = 0; i < cursamples; i++) 
		    {
#if SNDLIB_USE_FLOATS
		      fprintf(fd, "%f" PROC_SEP, MUS_SAMPLE_TO_FLOAT(buffer[i]));
#else
		      fprintf(fd, "%d" PROC_SEP, MUS_SAMPLE_TO_INT(buffer[i]));
#endif
		      sample++;
		      if (sample == ed->len) goto ALL_DONE;
		    }
		}
	    ALL_DONE:
	      mus_file_close(ifd);
	      fprintf(fd, VECTOR_CLOSE);
	      FREE(ibufs[0]);
	      FREE(ibufs);
	    }
	}
    }
}

void edit_history_to_file(FILE *fd, chan_info *cp)
{
  /* write edit list as a snd-guile program to fd (open for writing) for subsequent load */
  /*   the actual user-operations that produced these are included as comments */
  /*   the data is sometimes included as a vector, so the file can be very large! */
  /*   the entire current list is written, then the edit_ctr is fixed up to reflect its current state */
  int i, edits;
  ed_list *ed;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  /* 0 case = open-sound */
  for (i = 1; i <= edits; i++)
    {
      ed = cp->edits[i];
      if (ed)
	{
#if HAVE_RUBY
	  fprintf(fd, "      ");
#else
	  fprintf(fd, "      (");
#endif
	  switch (ed->edit_type)
	    {
	    case INSERTION_EDIT: 
	      /* samp data snd chn */
	      fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP "\"%s\" ",
		      TO_PROC_NAME(S_insert_samples_with_origin),
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, PROC_SEP "sfile" PROC_SEP "%d", cp->chan);
	      break;
	    case DELETION_EDIT:
	      /* samp samps snd chn */
	      fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP "\"%s\"" PROC_SEP "sfile" PROC_SEP "%d",
		      TO_PROC_NAME(S_delete_samples_with_origin),
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "",
		      cp->chan);
	      break;
	    case CHANGE_EDIT:
	      fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP  OFF_TD PROC_SEP "\"%s\"" PROC_SEP,
		      TO_PROC_NAME(S_change_samples_with_origin),
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, PROC_SEP "sfile" PROC_SEP "%d", cp->chan);
	      break;
	    case SCALED_EDIT: 
	      fprintf(fd, "%s" PROC_SEP "sfile" PROC_SEP "%d",
		      ed->origin, /* imports scaler */
		      cp->chan);
	      break;
	    case ZERO_EDIT:
	      fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP "sfile" PROC_SEP "%d",
		      TO_PROC_NAME(S_pad_channel),
		      ed->beg,
		      ed->len,
		      cp->chan);
	      break;
	    case RAMP_EDIT:
	      fprintf(fd, "%s" PROC_SEP "sfile" PROC_SEP "%d",
		      ed->origin,
		      cp->chan);
	      break;
	    case XEN_EDIT:
	      fprintf(fd, "%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP  OFF_TD PROC_SEP "sfile" PROC_SEP "%d",
		      TO_PROC_NAME("xen-channel"),
#if HAVE_GUILE
		      XEN_AS_STRING(scm_procedure_source(cp->xens[ed->ptree_location])),
#else
		      XEN_AS_STRING(cp->xens[ed->ptree_location]),
#endif
		      ed->beg,
		      ed->len,
		      cp->chan);
	      break;
	    case PTREE_EDIT:
	      fprintf(fd, "%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP  OFF_TD PROC_SEP "sfile" PROC_SEP "%d",
		      TO_PROC_NAME(S_ptree_channel),
		      XEN_AS_STRING(ptree_code(cp->ptrees[ed->ptree_location])),
		      ed->beg,
		      ed->len,
		      cp->chan);
	      break;
	    default:
	      snd_error("unknown edit branch: %s: %d %d",
			ed->origin, 
			ed->edit_type,
			ed->sound_location);
	      break;
	    }
	  if ((ed->edpos != AT_CURRENT_EDIT_POSITION) &&
	      (ed->edpos != (i - 1)))
	    fprintf(fd, " %d", ed->edpos);
#if HAVE_RUBY
	  else fprintf(fd, ", false");
#else
	  else fprintf(fd, " #f");
#endif
#if HAVE_GUILE
	  if ((ed->edit_type == PTREE_EDIT) || (ed->edit_type == XEN_EDIT))
	      {
		XEN code;
		fprintf(fd, " %s", (ed->ptree_env_too) ? "#t" : "#f");
		code = cp->ptree_inits[ed->ptree_location];
		if (XEN_PROCEDURE_P(code))
		  fprintf(fd, " %s", XEN_AS_STRING(scm_procedure_source(code)));
	      }
#endif
	  fprintf(fd, ")\n"); /* works for both Ruby and Scheme */
	}
    }
  if (cp->edit_ctr < edits) 
#if HAVE_RUBY
    fprintf(fd, "      undo(%d, sfile, %d);\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#else
    fprintf(fd, "      (undo %d sfile %d)\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
  save_mark_list(fd, cp);
}

#define copy_ed_fragment(New_Ed, Old_Ed) memcpy((void *)(New_Ed), (void *)(Old_Ed), sizeof(ed_fragment))

static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  int i;
  ed = (ed_list *)CALLOC(1, sizeof(ed_list));
  ed->size = size;
  ed->allocated_size = size;
#if defined(SGI) && (!(defined(__GNUC__)))
  /* this form required by the SGI C compiler: */
  ed->fragments = (void *)CALLOC(size, sizeof(ed_fragment *));
#else
#if defined(__SUNPRO_C)
  ed->fragments = (void *)CALLOC(size, sizeof(ed_fragment *));
#else
  FRAGMENTS(ed) = (ed_fragment **)CALLOC(size, sizeof(ed_fragment *));
#endif
#endif
  for (i = 0; i < size; i++)
    FRAGMENT(ed, i) = (ed_fragment *)CALLOC(1, sizeof(ed_fragment));
  ed->origin = NULL;
  ed->maxamp = -1.0;
  ed->selection_maxamp = -1.0;
  return(ed);
}

void set_ed_maxamp(chan_info *cp, int edpos, Float val)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  ed->maxamp = val;
}

Float ed_maxamp(chan_info *cp, int edpos)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  return(ed->maxamp);
}

void set_ed_selection_maxamp(chan_info *cp, Float val)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  ed->selection_maxamp = val;
}

Float ed_selection_maxamp(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return(ed->selection_maxamp);
}

static ed_list *free_ed_list(ed_list *ed, chan_info *cp)
{
  if (ed)
    {
      if (FRAGMENTS(ed)) 
	{
	  int i;
	  for (i = 0; i < ed->allocated_size; i++)
	    if (FRAGMENT(ed, i))
	      FREE(FRAGMENT(ed, i));
	  FREE(FRAGMENTS(ed));
	}
      if (ed->origin) FREE(ed->origin);
      if ((ed->edit_type == PTREE_EDIT) || (ed->edit_type == XEN_EDIT))
	{
	  int loc;
	  loc = ed->ptree_location;
	  /* fprintf(stderr,"free loc: %d, %p, %d %d\n", loc, cp->ptrees[loc], XEN_PROCEDURE_P(cp->ptree_inits[loc]), (XEN_PROCEDURE_P(cp->xens[loc]))); */
	  if (cp->ptrees[loc]) 
	    cp->ptrees[loc] = free_ptree(cp->ptrees[loc]);
	  if (XEN_PROCEDURE_P(cp->ptree_inits[loc]))
	    {
	      snd_unprotect(cp->ptree_inits[loc]);
	      cp->ptree_inits[loc] = XEN_FALSE;
	    }
	  if (XEN_PROCEDURE_P(cp->xens[loc]))
	    {
	      snd_unprotect(cp->xens[loc]);
	      cp->xens[loc] = XEN_FALSE;
	    }
	}
      FREE(ed);
    }
  return(NULL);
}

void backup_edit_list(chan_info *cp)
{
  int cur, i;
  snd_data *sd;
  cur = cp->edit_ctr;
  if (cur <= 0) return;
  cp->edits[cur]->edpos = cp->edits[cur - 1]->edpos;
  free_ed_list(cp->edits[cur - 1], cp);
  free_amp_env(cp, cur - 1);
  cp->edits[cur - 1] = cp->edits[cur];
  cp->amp_envs[cur - 1] = cp->amp_envs[cur];
  cp->edits[cur] = NULL;
  cp->amp_envs[cur] = NULL;
  cp->samples[cur - 1] = cp->samples[cur];
  if (cp->sounds) /* protect from release_pending_sounds upon edit after undo after as-one-edit or whatever */
    for (i = 0; i < cp->sound_size; i++)
      {
	sd = cp->sounds[i];
	if ((sd) && (sd->edit_ctr == cur)) sd->edit_ctr--;
      }
  backup_mark_list(cp, cur);
  cp->edit_ctr--;
  reflect_edit_history_change(cp);
}

void free_edit_list(chan_info *cp)
{
  int i;
  if (cp)
    {
      if (cp->edits)
	{
	  for (i = 0; i < cp->edit_size; i++)
	    {
	      /* cp->edit_ctr follows current edit state (redo/undo) */
	      if (cp->edits[i]) free_ed_list(cp->edits[i], cp);
	      if (cp->amp_envs[i]) free_amp_env(cp, i);
	    }
	  FREE(cp->edits);
	}
      cp->edits = NULL;
      if (cp->amp_envs) {FREE(cp->amp_envs); cp->amp_envs = NULL;}
      cp->edit_ctr = -1;
      cp->edit_size = 0;
    }
}

ed_list *initial_ed_list(off_t beg, off_t end)
{
  ed_list *ed;
  ed = make_ed_list(2);
  ed->beg = beg;
  ed->len = end + 1;
  ed->selection_beg = NO_SELECTION;
  ed->selection_end = 0;
  ed->edit_type = INITIALIZE_EDIT;
  ed->sound_location = 0;
  /* origin (channel %s %d) desc channel should be obvious from context */
  FRAGMENT_LOCAL_POSITION(ed, 0) = beg;
  FRAGMENT_LOCAL_END(ed, 0) = end;
  FRAGMENT_SCALER(ed, 0) = 1.0;
  FRAGMENT_TYPE(ed, 0) = ED_SIMPLE;
  if (ed->len > 0)
    {
      /* second block is our end-of-tree marker */
      FRAGMENT_SOUND(ed, 1) = EDIT_LIST_END_MARK;
      FRAGMENT_GLOBAL_POSITION(ed, 1) = end + 1;
    }
  else
    {
      FRAGMENT_SOUND(ed, 0) = EDIT_LIST_END_MARK;
      ed->size = 1;
    }
  return(ed);
}

void allocate_ed_list(chan_info *cp) 
{
  cp->edits = (ed_list **)CALLOC(cp->edit_size, sizeof(ed_list *));
}

static void new_leading_ramp(ed_fragment *new_start, ed_fragment *old_start, off_t samp)
{
  Float rmp1, rmp0;
  Float val;
  double xpos = 0.0;
  rmp0 = ED_RAMP_BEG(old_start);
  rmp1 = ED_RAMP_END(old_start);
  if ((rmp0 != 0.0) || (rmp1 != 0.0))
    {
      if (ED_LOCAL_END(old_start) == ED_LOCAL_POSITION(old_start))
	val = rmp0;
      else 
	{
	  xpos = (double)(samp - 1 - ED_GLOBAL_POSITION(old_start)) / (double)(ED_LOCAL_END(old_start) - ED_LOCAL_POSITION(old_start));
	  val = rmp0 + (rmp1 - rmp0) * xpos;
	}
      ED_RAMP_BEG(new_start) = rmp0;
      ED_RAMP_END(new_start) = val;
      if ((val == 0.0) && (rmp0 == 0.0))
	{
	  ED_SCALER(new_start) = 0.0; /* not redundant because reader chooser thinks rmps=0 means no env */
	  ED_TYPE(new_start) = ED_ZERO;
	}
      else
	{
	  ED_XRAMP_OFFSET(new_start) = ED_XRAMP_OFFSET(old_start);
	  ED_XRAMP_SCALER(new_start) = ED_XRAMP_SCALER(old_start);
	  ED_XRAMP_OFFSET2(new_start) = ED_XRAMP_OFFSET2(old_start);
	  ED_XRAMP_SCALER2(new_start) = ED_XRAMP_SCALER2(old_start);
	  rmp0 = ED_RAMP2_BEG(old_start);
	  rmp1 = ED_RAMP2_END(old_start);
	  ED_RAMP2_BEG(new_start) = rmp0;
	  if (ED_LOCAL_END(old_start) == ED_LOCAL_POSITION(old_start))
	    ED_RAMP2_END(new_start) = rmp0;
	  else ED_RAMP2_END(new_start) = rmp0 + (rmp1 - rmp0) * xpos;
	  rmp0 = ED_RAMP3_BEG(old_start);
	  rmp1 = ED_RAMP3_END(old_start);
	  ED_RAMP3_BEG(new_start) = rmp0;
	  if (ED_LOCAL_END(old_start) == ED_LOCAL_POSITION(old_start))
	    ED_RAMP3_END(new_start) = rmp0;
	  else ED_RAMP3_END(new_start) = rmp0 + (rmp1 - rmp0) * xpos;
	}
    }
}

static void new_trailing_ramp(ed_fragment *new_back, ed_fragment *old_back, off_t samp)
{
  Float rmp1, rmp0;
  Float val;
  double xpos = 0.0;
  rmp0 = ED_RAMP_BEG(old_back);
  rmp1 = ED_RAMP_END(old_back);
  if ((rmp0 != 0.0) || (rmp1 != 0.0))
    {
      if (ED_LOCAL_END(old_back) == ED_LOCAL_POSITION(old_back))
	val = rmp0;
      else 
	{
	  xpos = (double)(samp - ED_GLOBAL_POSITION(old_back)) / (double)(ED_LOCAL_END(old_back) - ED_LOCAL_POSITION(old_back));
	  val = rmp0 + (rmp1 - rmp0) * xpos;
	}
      ED_RAMP_BEG(new_back) = val;
      ED_RAMP_END(new_back) = rmp1;
      if ((val == 0.0) && (rmp1 == 0.0))
	{
	  ED_SCALER(new_back) = 0.0;
	  ED_TYPE(new_back) = ED_ZERO;
	}
      else
	{
	  ED_XRAMP_OFFSET(new_back) = ED_XRAMP_OFFSET(old_back);
	  ED_XRAMP_SCALER(new_back) = ED_XRAMP_SCALER(old_back);
	  ED_XRAMP_OFFSET2(new_back) = ED_XRAMP_OFFSET2(old_back);
	  ED_XRAMP_SCALER2(new_back) = ED_XRAMP_SCALER2(old_back);
	  rmp0 = ED_RAMP2_BEG(old_back);
	  rmp1 = ED_RAMP2_END(old_back);
	  if (ED_LOCAL_END(old_back) == ED_LOCAL_POSITION(old_back))
	    val = rmp0;
	  else val = rmp0 + (rmp1 - rmp0) * xpos;
	  ED_RAMP2_BEG(new_back) = val;
	  ED_RAMP2_END(new_back) = rmp1;
	  rmp0 = ED_RAMP3_BEG(old_back);
	  rmp1 = ED_RAMP3_END(old_back);
	  if (ED_LOCAL_END(old_back) == ED_LOCAL_POSITION(old_back))
	    val = rmp0;
	  else val = rmp0 + (rmp1 - rmp0) * xpos;
	  ED_RAMP3_BEG(new_back) = val;
	  ED_RAMP3_END(new_back) = rmp1;
	}
    }
  if (PTREE_OP(ED_TYPE(new_back)))
    ED_PTREE_POSITION(new_back) = ED_PTREE_POSITION(old_back) + samp - ED_GLOBAL_POSITION(old_back);
  if (PTREE2_OP(ED_TYPE(new_back)))
    ED_PTREE2_POSITION(new_back) = ED_PTREE2_POSITION(old_back) + samp - ED_GLOBAL_POSITION(old_back);
}

static void ripple_all(chan_info *cp, off_t beg, off_t samps)
{
  ripple_marks(cp, beg, samps);
  ripple_mixes(cp, beg, samps);
  check_for_first_edit(cp);
}


/* -------------------------------- insert samples -------------------------------- */

static ed_list *insert_section_into_list(off_t samp, off_t num, ed_list *current_state, ed_fragment **rtn, const char *origin, Float scaler)
{
  int cur_len, cur_i, new_i;
  ed_fragment *cur_f, *new_f, *inserted_f = NULL;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  new_state = make_ed_list(cur_len + 2); /* leave room for possible split */
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++, new_i++)
    {
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) > samp)
	{
	  /* copy this fragment and ripple */
	  copy_ed_fragment(new_f, cur_f);
	  ED_GLOBAL_POSITION(new_f) += num;
	}
      else
	{
	  if (ED_GLOBAL_POSITION(cur_f) == samp)
	    {
	      /* insert new fragment, copy to end */
	      inserted_f = new_f;
	      
	      /* make newf and increment */
	      new_i++;
	      new_f = FRAGMENT(new_state, new_i);
	      copy_ed_fragment(new_f, cur_f);
	      ED_GLOBAL_POSITION(new_f) += num;
	    }
	  else
	    {
	      copy_ed_fragment(new_f, cur_f);
	      /* look for splits */
	      if (FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1)) > samp)
		{
		  ed_fragment *split_front_f, *split_back_f;
		  /* split current at samp */
		  split_front_f = new_f;
		  copy_ed_fragment(split_front_f, cur_f);
		  ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + samp - ED_GLOBAL_POSITION(split_front_f) - 1;
		  /* samp - global position = where in current fragment, offset that by its local offset, turn into end sample */
		  
		  new_i++;
		  inserted_f = FRAGMENT(new_state, new_i);
		  /* deal with that later */
		  
		  new_i++;
		  split_back_f = FRAGMENT(new_state, new_i);
		  copy_ed_fragment(split_back_f, cur_f);
		  ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		  ED_GLOBAL_POSITION(split_back_f) = samp + num; /* rippled */
		  
		  /* now fixup ramps/ptrees affected by the split */
		  if ((ED_TYPE(cur_f) != ED_SIMPLE) && (ED_TYPE(cur_f) != ED_ZERO))
		    {
		      new_leading_ramp(split_front_f, cur_f, samp);
		      new_trailing_ramp(split_back_f, cur_f, samp);
		    }
		}
	    }
	}
    }
  ED_GLOBAL_POSITION(inserted_f) = samp;
  ED_LOCAL_POSITION(inserted_f) = 0;
  ED_LOCAL_END(inserted_f) = num - 1;
  ED_TYPE(inserted_f) = ED_SIMPLE;
  ED_SCALER(inserted_f) = scaler;
  if (scaler == 0.0) ED_TYPE(inserted_f) = ED_ZERO;
  (*rtn) = inserted_f;
  new_state->size = new_i;
  new_state->beg = samp;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  return(new_state);
}

static ed_list *insert_samples_into_list(off_t samp, off_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin, Float scaler)
{
  ed_list *new_state, *old_state;
  new_state = insert_section_into_list(samp, num, cp->edits[pos], rtn, origin, scaler);
  new_state->edpos = pos;
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
    }
  ripple_all(cp, samp, num);
  ripple_selection(new_state, samp, num);
  if (CURSOR(cp) > samp) CURSOR(cp) += num;
  reflect_sample_change_in_axis(cp);
  return(new_state);
}

void extend_with_zeros(chan_info *cp, off_t beg, off_t num, const char *origin, int edpos)
{
  off_t len, new_len;
  ed_fragment *cb;
  ed_list *ed;
  if (num <= 0) return;
  len = cp->samples[edpos];
  if (beg > len) 
    {
      new_len = beg + num; 
      beg = len;
      num = new_len - beg;
    }
  else new_len = len + num;
  prepare_edit_list(cp, new_len, edpos, origin);
  ed = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 0.0);
  cp->edits[cp->edit_ctr] = ed;
  ED_SOUND(cb) = EDIT_LIST_ZERO_MARK;
  ED_SCALER(cb) = 0.0;
  ED_TYPE(cb) = ED_ZERO;
  ed->edit_type = ZERO_EDIT;
  ed->sound_location = 0;
  reflect_edit_history_change(cp);
  check_for_first_edit(cp); /* needed to activate revert menu option */
  amp_env_insert_zeros(cp, beg, num, edpos);
}

void file_insert_samples(off_t beg, off_t num, char *inserted_file, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  int fd;
  file_info *hdr;
  snd_state *ss;
  if (dont_edit(cp)) return;
  len = cp->samples[edpos];
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len, "(insert-extend)", edpos);
      edpos = cp->edit_ctr;
      len = CURRENT_SAMPLES(cp);
    }
  ss = cp->state;
  prepare_edit_list(cp, len + num, edpos, origin);
  cp->edits[cp->edit_ctr] = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  reflect_edit_history_change(cp);
  ss->catch_message = NULL;
  hdr = make_file_info(inserted_file, ss);
  if (hdr)
    {
      fd = snd_open_read(ss, inserted_file);
      mus_file_open_descriptors(fd,
				inserted_file,
				hdr->format,
				mus_data_format_to_bytes_per_sample(hdr->format),
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, inserted_file, SND_INSERT_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, inserted_file, 
						 make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      {
	ed_list *ed;
	ed = cp->edits[cp->edit_ctr];
	ed->edit_type = INSERTION_EDIT;
	ed->sound_location = ED_SOUND(cb);
      }
      lock_affected_mixes(cp, beg, beg + num);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
      after_edit(cp);
    }
  else
    {
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_2(C_TO_XEN_STRING(origin),
			   C_TO_XEN_STRING(ss->catch_message)));
    }
}

static void insert_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = cp->samples[edpos];
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len, "(insert-extend)", edpos);
      edpos = cp->edit_ctr;
      len = CURRENT_SAMPLES(cp);
    }
  prepare_edit_list(cp, len + num, edpos, origin);
  cp->edits[cp->edit_ctr] = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  ED_SOUND(cb) = add_sound_buffer_to_edit_list(cp, vals, (int)num); 
  {
    ed_list *ed;
    ed = cp->edits[cp->edit_ctr];
    ed->edit_type = INSERTION_EDIT;
    ed->sound_location = ED_SOUND(cb);
  }
  reflect_edit_history_change(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
  after_edit(cp);
}

/* -------------------------------- delete samples -------------------------------- */

static ed_list *delete_section_from_list(off_t beg, off_t num, ed_list *current_state, const char *origin)
{
  int cur_len, cur_i, new_i;
  ed_fragment *cur_f, *new_f;
  off_t end, next_pos;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  end = beg + num;
  new_state = make_ed_list(cur_len + 2); /* leave room for possible splits */
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++)
    {
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) >= end)
	{
	  /* copy this fragment (we're past the deletion) */
	  copy_ed_fragment(new_f, cur_f);
	  ED_GLOBAL_POSITION(new_f) -= num;
	  new_i++;
	}
      else
	{
	  next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	  if (next_pos <= beg)
	    {
	      /* we're before deletion without any split, just copy */
	      copy_ed_fragment(new_f, cur_f);
	      new_i++;
	    }
	  else
	    {
	      /* split off begin (if any), delete until num used up, split off end (if any) */
	      /* if global_pos > beg and global_pos next <= end, just drop it, else split */
	      if (ED_GLOBAL_POSITION(cur_f) < beg)
		{
		  ed_fragment *split_front_f;
		  /* split front */
		  split_front_f = new_f;
		  copy_ed_fragment(split_front_f, cur_f);
		  new_i++;
		  ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + beg - ED_GLOBAL_POSITION(split_front_f) - 1;
		  /* samp - global position = where in current fragment, offset that by its local offset, turn into end sample */
		  if ((ED_TYPE(cur_f) != ED_SIMPLE) && (ED_TYPE(cur_f) != ED_ZERO))
		    new_leading_ramp(split_front_f, cur_f, beg);
		}
	      next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	      if (next_pos > end)
		{
		  ed_fragment *split_back_f;
		  new_f = FRAGMENT(new_state, new_i);
		  split_back_f = new_f;
		  copy_ed_fragment(split_back_f, cur_f);
		  new_i++;
		  ED_GLOBAL_POSITION(split_back_f) = beg;
		  ED_LOCAL_POSITION(split_back_f) += end - ED_GLOBAL_POSITION(cur_f);
		  if ((ED_TYPE(cur_f) != ED_SIMPLE) && (ED_TYPE(cur_f) != ED_ZERO))
		    new_trailing_ramp(split_back_f, cur_f, end);
		}
	    }
	}
    }
  new_state->size = new_i;
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  new_state->edit_type = DELETION_EDIT;
  new_state->sound_location = 0;
  return(new_state);
}

static ed_list *delete_samples_from_list(off_t beg, off_t num, int pos, chan_info *cp, const char *origin)
{
  ed_list *new_state, *old_state;
  new_state = delete_section_from_list(beg, num, cp->edits[pos], origin);
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
    }
  new_state->edpos = pos;
  ripple_all(cp, beg, -num);
  ripple_selection(new_state, beg, -num);
  if (CURSOR(cp) > beg)
    {
      /* this added 6-Dec-02 */
      CURSOR(cp) -= num;
      if (CURSOR(cp) < beg) CURSOR(cp) = beg;
    }
  reflect_sample_change_in_axis(cp);
  return(new_state);
}    

void delete_samples(off_t beg, off_t num, chan_info *cp, const char *origin, int edpos)
{
  off_t len;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = cp->samples[edpos];
  if ((beg < len) && (beg >= 0))
    {
      if ((beg + num) > len) num = len - beg;
      prepare_edit_list(cp, len - num, edpos, origin);
      cp->edits[cp->edit_ctr] = delete_samples_from_list(beg, num, edpos, cp, origin);
      reflect_edit_history_change(cp);
      lock_affected_mixes(cp, beg, beg + num);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
      after_edit(cp);
    }
  else
    {
      if (num == 1)
	report_in_minibuffer_and_save(cp->sound, 
				      _("can't delete sample " PRId64 " (current len = " PRId64 ")"), 
				      beg, len);
      else report_in_minibuffer_and_save(cp->sound, 
					 _("can't delete samples " PRId64 " to " PRId64 " (current len = " PRId64")"), 
					 beg, beg + num - 1, len);
    }
}



/* -------------------------------- change samples -------------------------------- */

static ed_list *change_samples_in_list(off_t beg, off_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin)
{
  /* delete + insert -- already checked that beg < cur end */
  ed_list *new_state, *old_state;
  ed_list *del_state;
  off_t del_num, cur_end;
  ed_fragment *changed_f;
  if (num <= 0) return(NULL);
  cur_end = cp->samples[pos];
#if DEBUGGING
  if (beg > cur_end) {fprintf(stderr,"unextended change?"); abort();}
#endif
  del_num = cur_end - beg;
  if (num < del_num) del_num = num;
  del_state = delete_section_from_list(beg, del_num, cp->edits[pos], NULL);
  new_state = insert_section_into_list(beg, num, del_state, &changed_f, origin, 1.0);
  del_state = free_ed_list(del_state, cp);
  (*rtn) = changed_f;
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
    }
  new_state->edpos = pos;
  ripple_all(cp, 0, 0);
  return(new_state);
}

void file_change_samples(off_t beg, off_t num, char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, int lock, const char *origin, int edpos)
{
  off_t prev_len, new_len;
  ed_fragment *cb;
  int fd;
  file_info *hdr;
  snd_state *ss;
  if (dont_edit(cp)) return;
  ss = cp->state;
  ss->catch_message = NULL;
  hdr = make_file_info(tempfile, ss);
  if (hdr)
    {
      prev_len = cp->samples[edpos];
      if (beg >= prev_len)
	{
	  extend_with_zeros(cp, prev_len, beg - prev_len + 1, "(change-extend)", edpos);
	  edpos = cp->edit_ctr;
	  prev_len = CURRENT_SAMPLES(cp);
	}
      new_len = beg + num;
      if (new_len < prev_len) new_len = prev_len;
      prepare_edit_list(cp, new_len, edpos, origin);
      cp->edits[cp->edit_ctr] = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
      if (new_len > prev_len) reflect_sample_change_in_axis(cp);
      reflect_edit_history_change(cp);
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
      fd = snd_open_read(ss, tempfile);
      mus_file_open_descriptors(fd,
				tempfile,
				hdr->format,
				mus_data_format_to_bytes_per_sample(hdr->format),
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_CHANGE_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, tempfile, 
						 make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      {
	ed_list *ed;
	ed = cp->edits[cp->edit_ctr];
	ed->edit_type = CHANGE_EDIT;
	ed->sound_location = ED_SOUND(cb);
      }
      if (cp->mix_md) reflect_mix_edit(cp, origin);
      after_edit(cp);
    }
  else
    {
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_2(C_TO_XEN_STRING(origin),
			   C_TO_XEN_STRING(ss->catch_message)));
    }
}

void file_override_samples(off_t num, char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, int lock, const char *origin)
{
  int fd;
  ed_list *e;
  file_info *hdr;
  snd_state *ss;
  if (dont_edit(cp)) return;
  ss = cp->state;
  ss->catch_message = NULL;
  hdr = make_file_info(tempfile, ss);
  if (hdr) 
    {
      if (num == -1) num = (hdr->samples / hdr->chans);
      prepare_edit_list(cp, num, AT_CURRENT_EDIT_POSITION, origin);
      fd = snd_open_read(ss, tempfile);
      mus_file_open_descriptors(fd,
				tempfile,
				hdr->format,
				mus_data_format_to_bytes_per_sample(hdr->format),
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_OVERRIDE_FILE);
      e = initial_ed_list(0, num - 1);
      if (origin) e->origin = copy_string(origin);
      cp->edits[cp->edit_ctr] = e;
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, 0, num);
      FRAGMENT_SOUND(e, 0) = add_sound_file_to_edit_list(cp, tempfile, 
							 make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE),
							 hdr, auto_delete, chan);
      e->edit_type = CHANGE_EDIT;
      e->sound_location = FRAGMENT_SOUND(e, 0);
      e->edpos = cp->edit_ctr - 1;
      reflect_edit_history_change(cp);
      reflect_sample_change_in_axis(cp);
      ripple_all(cp, 0, 0);
      /* update_graph(cp); */
      if (cp->mix_md) reflect_mix_edit(cp, origin);
      after_edit(cp);
    }
  else
    {
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_2(C_TO_XEN_STRING(origin),
			   C_TO_XEN_STRING(ss->catch_message)));
    }
}

void change_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, int lock, const char *origin, int edpos)
{
  off_t prev_len, new_len;
  ed_fragment *cb;
  ed_list *ed;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  prev_len = cp->samples[edpos];
  if (beg >= prev_len)
    {
      extend_with_zeros(cp, prev_len, beg - prev_len + 1, "(change-extend)", edpos);
      edpos = cp->edit_ctr;
      prev_len = CURRENT_SAMPLES(cp);
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  prepare_edit_list(cp, new_len, edpos, origin);
  ed = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
  if (new_len > prev_len) reflect_sample_change_in_axis(cp);
  cp->edits[cp->edit_ctr] = ed;
  ED_SOUND(cb) = add_sound_buffer_to_edit_list(cp, vals, (int)num); 
  ed->edit_type = CHANGE_EDIT;
  ed->sound_location = ED_SOUND(cb);
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
  after_edit(cp);
}



/* -------------------------------- ramp/scale/ptree -------------------------------- */

static bool rampable_ptree_op(int typ)
{
  switch (typ)
    {
    case ED_PTREE_RAMP: case ED_PTREE_XRAMP: case ED_PTREE_RAMP2:
    case ED_RAMP_PTREE_RAMP: case ED_RAMP_PTREE_XRAMP:
    case ED_PTREE: case ED_PTREE_ZERO:
    case ED_RAMP_PTREE: case ED_RAMP_PTREE_ZERO:
    case ED_RAMP2_PTREE: case ED_RAMP2_PTREE_ZERO:
    case ED_XRAMP_PTREE: case ED_XRAMP_PTREE_ZERO:
    case ED_RAMP_XRAMP_PTREE: case ED_RAMP_XRAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_ZERO:
    case ED_XEN: case ED_XEN_ZERO:
    case ED_RAMP_XEN: case ED_RAMP_XEN_ZERO:
    case ED_RAMP2_XEN: case ED_RAMP2_XEN_ZERO:
    case ED_XEN_RAMP: case ED_XEN_RAMP2: case ED_RAMP_XEN_RAMP:
    case ED_PTREE2: case ED_PTREE2_ZERO:
    case ED_RAMP_PTREE2: case ED_RAMP_PTREE2_ZERO:
    case ED_RAMP2_PTREE2: case ED_RAMP2_PTREE2_ZERO:
    case ED_PTREE_RAMP_PTREE_RAMP:
      return(true);
      break;
    }
  return(false);
}

static bool xrampable_ptree_op(int typ)
{
  switch (typ)
    {
    case ED_PTREE: case ED_PTREE_ZERO:
    case ED_XRAMP_PTREE: case ED_XRAMP_PTREE_ZERO:
    case ED_PTREE_RAMP:
    case ED_RAMP_PTREE: case ED_RAMP_PTREE_ZERO:
    case ED_XEN: case ED_XEN_ZERO:
    case ED_PTREE2: case ED_PTREE2_ZERO:
      return(true);
      break;
    }
  return(false);
}

bool ramp_or_ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, Float base)
{
  /* from enveloper (snd-sig.c) */
  ed_list *ed;
  int i, typ;
  off_t end, loc, next_loc;
  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);         /* i.e. next loc = current fragment end point */
      /* fragment starts at loc, ends just before next_loc, is of type typ */
      if (next_loc > beg)
	{
	  if (((PTREE_OP(typ)) && 
	       (((base == 1.0) && (!(rampable_ptree_op(typ)))) ||
		((base != 1.0) && (!(xrampable_ptree_op(typ)))))) ||
	      (RAMP3_OP(typ)))               /* all ramp fields in use */
	    return(true);
	  if ((base != 1.0) && RAMP2_OP(typ)) /* no xramp in 3rd field */
	    return(true);
	}
    }
  return(false);
}

static bool xenable_op(int typ)
{
  switch (typ)
    {
    case ED_SIMPLE: case ED_ZERO:
    case ED_RAMP: case ED_RAMP2: case ED_RAMP3:
    case ED_XRAMP:
      return(true);
      break;
    }
  return(false);
}

static bool ptreeable_op(int typ)
{
  switch (typ)
    {
    case ED_SIMPLE: case ED_ZERO:
    case ED_PTREE: case ED_PTREE_ZERO: case ED_PTREE_RAMP:
    case ED_RAMP: case ED_XRAMP: case ED_RAMP2: case ED_RAMP3:
    case ED_RAMP_XRAMP: case ED_RAMP2_XRAMP: case ED_XRAMP_RAMP: case ED_RAMP_XRAMP_RAMP: case ED_XRAMP2:
    case ED_RAMP_PTREE: case ED_RAMP_PTREE_ZERO: 
    case ED_RAMP2_PTREE: case ED_RAMP2_PTREE_ZERO: 
    case ED_RAMP3_PTREE: case ED_RAMP3_PTREE_ZERO:
    case ED_PTREE_RAMP2: case ED_PTREE_RAMP3: case ED_PTREE_XRAMP: case ED_XRAMP_PTREE: case ED_XRAMP_PTREE_ZERO:
    case ED_RAMP2_PTREE_RAMP: case ED_RAMP_PTREE_RAMP2: case ED_RAMP_PTREE_RAMP:
      return(true);
      break;
    }
  return(false);
}

bool ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, int is_xen)
{
  /* from ptree-channel (snd-sig.c) check for pre-existing ptree-channel */
  ed_list *ed;
  int i, typ;
  off_t end, loc, next_loc;
  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);         /* i.e. next loc = current fragment end point */
      /* fragment starts at loc, ends just before next_loc, is of type typ */
      if (next_loc > beg)
	{
	  if ((is_xen) && (!(xenable_op(typ))))
	    return(true);
	  if (!(ptreeable_op(typ)))
	    return(true);
	}
    }
  return(false);
}

bool ptree_or_sound_fragments_in_use(chan_info *cp, int pos)
{
  /* (swap-channels): are there any non-simple/non-ramp edits? */
  int i, index;
  ed_list *ed;
  ed = cp->edits[pos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      index = FRAGMENT_SOUND(ed, i);
      if (index == EDIT_LIST_END_MARK) return(false);
      if ((index != 0) &&
	  (index != EDIT_LIST_ZERO_MARK))
	return(true);
      if (PTREE_OP(FRAGMENT_TYPE(ed, i)))
	return(true);
    }
  return(false);
}

static ed_list *copy_and_split_list(off_t beg, off_t num, ed_list *current_state, const char *origin)
{
  off_t end, next_pos;
  int cur_len, cur_i, new_i;
  ed_list *new_state;
  ed_fragment *new_f, *cur_f, *mid_f = NULL;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  new_state = make_ed_list(cur_len + 2); /* leave room for possible split */
  end = beg + num;
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++, new_i++)
    {
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) >= end)
	{
	  /* after any split, copy this fragment */
	  copy_ed_fragment(new_f, cur_f);
	}
      else
	{
	  next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	  if (next_pos <= beg)
	    {
	      /* we're before any split, just copy */
	      copy_ed_fragment(new_f, cur_f);
	    }
	  else
	    {
	      if ((ED_GLOBAL_POSITION(cur_f) >= beg) && (next_pos < end))
		{
		  /* entire segment is included */
		  copy_ed_fragment(new_f, cur_f);
		}
	      else
		{
		  /* check for front and back splits, copy cur */
		  copy_ed_fragment(new_f, cur_f);
		  if (ED_GLOBAL_POSITION(cur_f) < beg)
		    {
		      ed_fragment *split_front_f, *split_back_f;
		      /* split current at samp */
		      split_front_f = new_f;
		      new_i++;
		      split_back_f = FRAGMENT(new_state, new_i);
		      copy_ed_fragment(split_back_f, cur_f);
		      new_f = split_back_f;
		      ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + beg - ED_GLOBAL_POSITION(split_front_f) - 1;
		      /* beg - global position = where in current fragment, offset that by its local offset, turn into end sample */
		      ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		      ED_GLOBAL_POSITION(split_back_f) = beg;
		      /* now fixup ramps/ptrees affected by the split */
		      if ((ED_TYPE(cur_f) != ED_SIMPLE) && (ED_TYPE(cur_f) != ED_ZERO))
			{
			  new_leading_ramp(split_front_f, cur_f, beg);
			  new_trailing_ramp(split_back_f, cur_f, beg);
			  mid_f = split_back_f;
			}
		    }
		  
		  if (next_pos > end)
		    {
		      ed_fragment *split_front_f, *split_back_f;
		      /* split current at samp */
		      split_front_f = new_f;
		      new_i++;
		      split_back_f = FRAGMENT(new_state, new_i);
		      copy_ed_fragment(split_back_f, cur_f);
		      ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + end - ED_GLOBAL_POSITION(split_front_f) - 1;
		      ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		      ED_GLOBAL_POSITION(split_back_f) = end;
		      /* now fixup ramps/ptrees affected by the split */
		      if ((ED_TYPE(cur_f) != ED_SIMPLE) && (ED_TYPE(cur_f) != ED_ZERO))
			{
			  Float old_beg, old_beg2, old_beg3;
			  old_beg = ED_RAMP_BEG(split_front_f);
			  old_beg2 = ED_RAMP2_BEG(split_front_f);
			  old_beg3 = ED_RAMP3_BEG(split_front_f);
			  new_leading_ramp(split_front_f, cur_f, end);
			  if (mid_f == split_front_f)
			    {
			      ED_RAMP_BEG(split_front_f) = old_beg;
			      ED_RAMP2_BEG(split_front_f) = old_beg2;
			      ED_RAMP3_BEG(split_front_f) = old_beg3;
			    }
			  new_trailing_ramp(split_back_f, cur_f, end);
			}
		    }
		}
	    }
	}
    }
  new_state->size = new_i;
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  return(new_state);
}

void scale_channel(chan_info *cp, Float scl, off_t beg, off_t num, int pos, bool in_as_one_edit)
{
  /* copy current ed-list and reset scalers */
  off_t len;
  int i;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]) ||
      (scl == 1.0))
    return; 
  len = cp->samples[pos];
  prepare_edit_list(cp, len, pos, S_scale_channel);
  old_ed = cp->edits[pos];
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      new_ed->beg = beg;
      new_ed->len = num;
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	  FRAGMENT_SCALER(new_ed, i) *= scl;
	  if (scl == 0.0) FRAGMENT_TYPE(new_ed, i) = ED_ZERO;
	}
      amp_env_scale_by(cp, scl, pos); 
    }
  else 
    {
      if (beg + num > len) num = len - beg;
      new_ed = copy_and_split_list(beg, num, old_ed, NULL);

      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg) 
	    {
	      FRAGMENT_SCALER(new_ed, i) *= scl;
	      if (scl == 0.0) FRAGMENT_TYPE(new_ed, i) = ED_ZERO;
	    }
	}
      amp_env_scale_selection_by(cp, scl, beg, num, pos);
    }
  new_ed->edit_type = SCALED_EDIT;
  new_ed->sound_location = 0;
#if HAVE_RUBY
  new_ed->origin = mus_format("%s(%.4f, " OFF_TD ", " OFF_TD, TO_PROC_NAME(S_scale_channel), scl, beg, num);
#else
  new_ed->origin = mus_format("%s %.4f " OFF_TD " " OFF_TD, S_scale_channel, scl, beg, num);
#endif
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "scale"); /* 30-Jan-02 */
  reflect_edit_history_change(cp);
  if (!in_as_one_edit) update_graph(cp);
}

static void setup_ramp_fragments(ed_list *new_ed, int i, double seg0, double seg1, Float scaler, Float offset, bool is_xramp)
{
  if (FRAGMENT_TYPE(new_ed, i) != ED_ZERO)
    {
      switch (FRAGMENT_TYPE(new_ed, i))
	{
	case ED_RAMP2:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3;
	  break;
	case ED_XRAMP2:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP2;
	  break;
	case ED_RAMP_XRAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XRAMP;
	  break;
	case ED_XRAMP_RAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP_RAMP;
	  break;
	case ED_RAMP:
	  /* underlying is ramp */
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_RAMP;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP2;
	  break;
	case ED_XRAMP:
	  /* underlying is xramp */
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP2;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP;
	  break;
	case ED_PTREE:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_PTREE;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE;
	  break;
	case ED_PTREE_ZERO:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_PTREE_ZERO;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE_ZERO;
	  break;
	case ED_RAMP_PTREE:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_RAMP_PTREE;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE;
	  break;
	case ED_RAMP_PTREE_ZERO:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_RAMP_PTREE_ZERO;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE_ZERO;
	  break;
	case ED_XRAMP_PTREE:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP2_PTREE;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP_PTREE;
	  break;
	case ED_XRAMP_PTREE_ZERO:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP2_PTREE_ZERO;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP_PTREE_ZERO;
	  break;
	case ED_RAMP_XRAMP_PTREE:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XRAMP_PTREE;
	  break;
	case ED_RAMP_XRAMP_PTREE_ZERO:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XRAMP_PTREE_ZERO;
	  break;
	case ED_XRAMP_RAMP_PTREE:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP_RAMP_PTREE;
	  break;
	case ED_XRAMP_RAMP_PTREE_ZERO:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XRAMP_RAMP_PTREE_ZERO;
	  break;
	case ED_RAMP2_PTREE:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_PTREE;
	  break;
	case ED_RAMP2_PTREE_ZERO:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_PTREE_ZERO;
	  break;
	case ED_PTREE_XRAMP:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE_XRAMP;
	  break;
	case ED_PTREE_RAMP:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_PTREE_RAMP;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE_RAMP;
	  break;
	case ED_RAMP_PTREE_RAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE_RAMP;
	  break;
	case ED_RAMP_PTREE_XRAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE_XRAMP;
	  break;
	case ED_PTREE_RAMP2:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE_RAMP2;
	  break;

	case ED_XEN:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_XEN;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XEN;
	  break;
	case ED_XEN_ZERO:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_XEN_ZERO;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XEN_ZERO;
	  break;
	case ED_RAMP_XEN:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XEN;
	  break;
	case ED_RAMP2_XEN:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_XEN;
	  break;
	case ED_RAMP_XEN_ZERO:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XEN_ZERO;
	  break;
	case ED_RAMP2_XEN_ZERO:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_XEN_ZERO;
	  break;
	case ED_XEN_RAMP:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XEN_RAMP;
	  break;
	case ED_XEN_RAMP2:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_XEN_RAMP2;
	  break;
	case ED_RAMP_XEN_RAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_XEN_RAMP;
	  break;

	case ED_PTREE2:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_PTREE2;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE2;
	  break;
	case ED_PTREE2_ZERO:
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP_PTREE2_ZERO;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE2_ZERO;
	  break;
	case ED_RAMP_PTREE2:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE2;
	  break;
	case ED_RAMP_PTREE2_ZERO:
	  FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP2_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP2_PTREE2_ZERO;
	  break;
	case ED_RAMP2_PTREE2:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_PTREE2;
	  break;
	case ED_RAMP2_PTREE2_ZERO:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP3_PTREE2_ZERO;
	  break;
	case ED_PTREE_RAMP_PTREE_RAMP:
	  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP_PTREE_RAMP_PTREE_RAMP;
	  break;

	default:
	  /* underlying is not zero and not ramp-related */
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  if (is_xramp)
	    {
	      FRAGMENT_TYPE(new_ed, i) = ED_XRAMP;
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	    }
	  else FRAGMENT_TYPE(new_ed, i) = ED_RAMP;
	  break;
	}
    }
}

static void all_ramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
			     off_t beg, off_t num, int pos, bool in_as_one_edit, const char *origin, 
			     bool is_x, mus_any *e, int e_pos)
{
  off_t len;
  int i;
  ed_list *new_ed, *old_ed;
  Float seg0, seg1;
  double incr;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]))
    return; 
  if ((rmp0 == rmp1) || (num == 1))
    {
      scale_channel(cp, rmp0, beg, num, pos, in_as_one_edit);
      return;
    }
  len = cp->samples[pos];
  prepare_edit_list(cp, len, pos, origin);
  old_ed = cp->edits[pos];
  incr = (double)(rmp1 - rmp0) / (double)(num - 1);
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      /* one ramp over entire fragment list -- no splits will occur here */
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      new_ed->beg = beg;
      new_ed->len = num;
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++)
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
      seg1 = rmp0 - incr;
      for (i = 0; i < new_ed->size - 1; i++) /* -1 here to leave end mark alone */
	{
	  seg0 = seg1 + incr;
	  seg1 = seg0 + (incr * (FRAGMENT_LENGTH(new_ed, i) - 1));
	  setup_ramp_fragments(new_ed, i, seg0, seg1, scaler, offset, is_x);
	}
    }
  else 
    {
      if (beg + num > len) num = len - beg;
      new_ed = copy_and_split_list(beg, num, old_ed, NULL);
      
      cp->edits[cp->edit_ctr] = new_ed;
      seg1 = rmp0 - incr;
      for (i = 0; i < new_ed->size - 1; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	    {
	      seg0 = seg1 + incr;
	      seg1 = seg0 + (incr * (FRAGMENT_LENGTH(new_ed, i) - 1));
	      setup_ramp_fragments(new_ed, i, seg0, seg1, scaler, offset, is_x);
	    }
	}
    }
  new_ed->edit_type = RAMP_EDIT;
  new_ed->sound_location = 0;
  if (!is_x)
    {
#if HAVE_RUBY
  new_ed->origin = mus_format("%s(%.4f, %.4f, " OFF_TD ", " OFF_TD, TO_PROC_NAME(origin), rmp0, rmp1, beg, num);
#else
  new_ed->origin = mus_format("%s %.4f %.4f " OFF_TD " " OFF_TD, origin, rmp0, rmp1, beg, num);
#endif
    }
  else
    {
      Float *data;
      data = mus_data(e);
#if HAVE_RUBY
  new_ed->origin = mus_format("%s(%.4f, %.4f, %.4f, " OFF_TD ", " OFF_TD, 
			      TO_PROC_NAME(origin), 
			      data[e_pos * 2 + 1], data[e_pos * 2 + 3],
			      mus_increment(e),
			      beg, num);
#else
  new_ed->origin = mus_format("%s %.4f %.4f %.4f " OFF_TD " " OFF_TD, 
			      origin, 
			      data[e_pos * 2 + 1], data[e_pos * 2 + 3],
			      mus_increment(e),
			      beg, num);
#endif
    }
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "ramp");
  reflect_edit_history_change(cp);
}

void ramp_channel(chan_info *cp, Float rmp0, Float rmp1, off_t beg, off_t num, int pos, bool in_as_one_edit)
{
  all_ramp_channel(cp, rmp0, rmp1, 0.0, 0.0, beg, num, pos, in_as_one_edit, S_ramp_channel, false, NULL, 0);
}

void xramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
		   off_t beg, off_t num, int pos, bool in_as_one_edit, mus_any *e, int e_pos)
{
  all_ramp_channel(cp, rmp0, rmp1, scaler, offset, beg, num, pos, in_as_one_edit, S_xramp_channel, true, e, e_pos);
}

static void make_ptree_fragment(ed_list *new_ed, int i, int ptree_loc, off_t beg, off_t num, bool is_xen)
{
  switch (FRAGMENT_TYPE(new_ed, i))
    {
    case ED_ZERO: 
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN_ZERO : ED_PTREE_ZERO);
      break;
    case ED_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN_RAMP : ED_PTREE_RAMP);
      break;
    case ED_XRAMP:
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN_XRAMP : ED_PTREE_XRAMP);
      break;
    case ED_RAMP2:
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN_RAMP2 : ED_PTREE_RAMP2);
      break;
    case ED_RAMP3:
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN_RAMP3 : ED_PTREE_RAMP3);
      break;
    case ED_RAMP_XRAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_XRAMP;
      break;
    case ED_RAMP2_XRAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP2_XRAMP;
      break;
    case ED_RAMP_XRAMP_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_XRAMP_RAMP;
      break;
    case ED_XRAMP_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_XRAMP_RAMP;
      break;
    case ED_PTREE:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2;
      break;
    case ED_PTREE_ZERO:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2_ZERO;
      break;
    case ED_PTREE_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2_RAMP;
      break;
    default:
      FRAGMENT_TYPE(new_ed, i) = ((is_xen) ? ED_XEN : ED_PTREE);
      break;
    case ED_RAMP_PTREE:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_PTREE;
      break;
    case ED_RAMP_PTREE_ZERO:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_PTREE_ZERO;
      break;
    case ED_RAMP2_PTREE:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP2_PTREE;
      break;
    case ED_RAMP2_PTREE_ZERO:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP2_PTREE_ZERO;
      break;
    case ED_RAMP3_PTREE:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP3_PTREE;
      break;
    case ED_RAMP3_PTREE_ZERO:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP3_PTREE_ZERO;
      break;
    case ED_PTREE_RAMP2:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2_RAMP2;
      break;
    case ED_PTREE_RAMP3:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2_RAMP3;
      break;
    case ED_PTREE_XRAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE2_XRAMP;
      break;
    case ED_XRAMP_PTREE:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_XRAMP_PTREE;
      break;
    case ED_XRAMP_PTREE_ZERO:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_XRAMP_PTREE_ZERO;
      break;
    case ED_XRAMP2:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_XRAMP2;
      break;
    case ED_RAMP2_PTREE_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP2_PTREE_RAMP;
      break;
    case ED_RAMP_PTREE_RAMP2:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_PTREE_RAMP2;
      break;
    case ED_RAMP_PTREE_RAMP:
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE_RAMP_PTREE_RAMP;
      break;
    }
  if (!(PTREE2_OP(FRAGMENT_TYPE(new_ed, i))))
    {
      FRAGMENT_PTREE_INDEX(new_ed, i) = ptree_loc;
      FRAGMENT_PTREE_SCALER(new_ed, i) = MUS_SAMPLE_TO_FLOAT(FRAGMENT_SCALER(new_ed, i)); /* arg is mus_sample data, need convert to float */
      FRAGMENT_PTREE_DUR(new_ed, i) = num;
      FRAGMENT_PTREE_POSITION(new_ed, i) = FRAGMENT_GLOBAL_POSITION(new_ed, i) - beg;
    }
  else 
    {
      FRAGMENT_PTREE2_INDEX(new_ed, i) = ptree_loc;
      FRAGMENT_PTREE2_SCALER(new_ed, i) = FRAGMENT_SCALER(new_ed, i); /* already float, so no need to convert */
      FRAGMENT_PTREE2_DUR(new_ed, i) = num;
      FRAGMENT_PTREE2_POSITION(new_ed, i) = FRAGMENT_GLOBAL_POSITION(new_ed, i) - beg;
    }
  FRAGMENT_SCALER(new_ed, i) = 1.0;
}

void ptree_channel(chan_info *cp, void *ptree, off_t beg, off_t num, int pos, bool env_it, XEN init_func, bool is_xen)
{
  off_t len;
  int i, ptree_loc = 0;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]) ||
      (ptree == NULL))
    return; 
  len = cp->samples[pos];
  prepare_edit_list(cp, len, pos, S_ptree_channel);
  old_ed = cp->edits[pos];
  ptree_loc = add_ptree(cp);
  if (is_xen)
    {
      snd_protect((XEN)ptree);
      cp->xens[ptree_loc] = (XEN)ptree;
    }
  else cp->ptrees[ptree_loc] = ptree;

  if (XEN_PROCEDURE_P(init_func))
    {
      cp->ptree_inits[ptree_loc] = init_func;
      snd_protect(init_func);
    }
  else cp->ptree_inits[ptree_loc] = XEN_FALSE;

  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      new_ed->beg = beg;
      new_ed->len = num;
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	  make_ptree_fragment(new_ed, i, ptree_loc, beg, num, is_xen);
	}
      if (env_it)
	amp_env_ptree(cp, ptree, pos, init_func, is_xen);
    }
  else 
    {
      if (beg + num > len) num = len - beg;
      new_ed = copy_and_split_list(beg, num, old_ed, NULL);

      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) 
	    break;                                                    /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	    make_ptree_fragment(new_ed, i, ptree_loc, beg, num, is_xen);
	}
      if (env_it)
	amp_env_ptree_selection(cp, ptree, beg, num, pos, init_func, is_xen);
    }
  new_ed->edit_type = (is_xen) ? XEN_EDIT : PTREE_EDIT;
  new_ed->sound_location = 0;
  new_ed->ptree_location = ptree_loc;
#if HAVE_RUBY
  new_ed->origin = mus_format("%s(%d, " OFF_TD ", " OFF_TD, (is_xen) ? "xen" : "ptree", ptree_loc, beg, num);
#else
  new_ed->origin = mus_format("%s %d " OFF_TD " " OFF_TD, (is_xen) ? "xen" : "ptree", ptree_loc, beg, num);
#endif
  new_ed->edpos = pos;
  new_ed->ptree_env_too = env_it;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "ptree");
  reflect_edit_history_change(cp);
  update_graph(cp);
}


/* -------------------------------- sample readers -------------------------------- */

static XEN empty_closure;

static void reader_out_of_data(snd_fd *sf);
snd_fd *free_snd_fd_almost(snd_fd *sf)
{
  snd_data *sd;
  if (sf) 
    {
      if ((sf->closure) && (sf->closure != empty_closure) && (XEN_BOUND_P(sf->closure)))
 	{
 	  snd_unprotect_at(sf->protect);
 	  sf->closure = XEN_UNDEFINED;
 	}
      if ((sf->closure1) && (sf->closure1 != empty_closure) && (XEN_BOUND_P(sf->closure1)))
 	{
 	  snd_unprotect_at(sf->protect1);
 	  sf->closure1 = XEN_UNDEFINED;
 	}
      reader_out_of_data(sf);
      sd = sf->current_sound;
      if ((sd) && 
	  ((sd->type == SND_DATA_BUFFER) || (sd->type == SND_DATA_FILE)))
	{
	  sd->inuse = false;
	  if ((sd->copy) || (sd->free_me))
	    sd = free_snd_data(sd); 
	}
      sf->current_state = NULL;
      sf->current_sound = NULL;
      /* FREE(sf); */
    }
  return(NULL);
}

#if DEBUGGING
  static void check_dangling_readers(snd_fd *md);
#endif

snd_fd *free_snd_fd(snd_fd *sf)
{
#if DEBUGGING
  check_dangling_readers(sf);
#endif      
  free_snd_fd_almost(sf);
  FREE(sf);
  return(NULL);
}

off_t current_location(snd_fd *sf) 
{
  /* only used by moving cursor code in snd-dac.c [and sample-reader-position] */
  if (sf->current_sound)
    return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + sf->current_sound->io->beg + sf->loc);
  return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + sf->loc);
}



/* -------------------------------- fragment accessors --------------------------------
 *
 * each fragment has two associated read funcs: run and runf
 *   run -> mus_sample_t
 *   runf -> Float
 *   the following functions are possible run/runf choices, based on the fragment type
 *     (if fragment has scaler, use next_sample else next_sample_unscaled, etc)
 * read_sample calls run, read_sample_to_float runf
 *
 * all sample accesses go through run/runf
 *
 * each type of accessor needs 8 cases: 
 *   [back | forth] [normal | on_air] -> [mus_sample_t | float]
 * on_air case only needed if input 0 doesn't imply output 0
 */

/* envelope handlers */

static Float next_ramp2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval2;
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_ramp2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval2;
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval2 * sf->curval3;
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval2 * sf->curval3;
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval += sf->incr;
  return(val);
}

static Float previous_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval -= sf->incr;
  return(val);
}

static Float next_xramp_2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval2)));
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_xramp_2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval2)));
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_ramp_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp2_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp2_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval3 * (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval * sf->curval3 * (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * 
          (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
          (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * 
          (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
          (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * sf->curval3 *
	  (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
	  (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_SCALER(sf) * sf->curval3 *
	  (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
	  (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_ptree_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval += sf->incr;
  return(val);
}

static Float previous_ptree_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval -= sf->incr;
  return(val);
}

static Float next_ptree_xramp_1_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE2_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval += sf->incr;
  return(val);
}

static Float previous_ptree_xramp_1_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE2_SCALER(sf) * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval)));
  sf->curval -= sf->incr;
  return(val);
}

static Float next_ptree_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE_SCALER(sf) * 
          (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
          (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval += sf->incr;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_ptree_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = READER_PTREE_SCALER(sf) * 
          (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval))) *
          (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval2)));
  sf->curval -= sf->incr;
  sf->curval2 -= sf->incr2;
  return(val);
}



static mus_sample_t previous_sound (snd_fd *sf);
static Float previous_sound_as_float(snd_fd *sf);
static mus_sample_t next_sound(snd_fd *sf);
static Float next_sound_as_float(snd_fd *sf);


/* ---------------- at end ---------------- */
static mus_sample_t end_sample(snd_fd *sf) {return(MUS_SAMPLE_0);}
static Float end_sample_to_float(snd_fd *sf) {return(0.0);}


/* ---------------- scaled ---------------- */
static mus_sample_t next_sample(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * READER_SCALER(sf)));
}

static mus_sample_t previous_sample(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * READER_SCALER(sf)));
}

static Float next_sample_to_float(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return((sf->data[sf->loc++]) * sf->fscaler);
}

static Float previous_sample_to_float(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return((sf->data[sf->loc--]) * sf->fscaler);
}

#if (!SNDLIB_USE_FLOATS)
static mus_sample_t next_sample_by_int(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return(sf->data[sf->loc++] * sf->iscaler);
}

static mus_sample_t previous_sample_by_int(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return(sf->data[sf->loc--] * sf->iscaler);
}
#endif


/* ---------------- unscaled ---------------- */
static mus_sample_t next_sample_unscaled(snd_fd *sf) {if (sf->loc > sf->last) return(next_sound(sf)); else return(sf->data[sf->loc++]);}
static mus_sample_t previous_sample_unscaled(snd_fd *sf) {if (sf->loc < sf->first) return(previous_sound(sf)); else return(sf->data[sf->loc--]);}
static Float next_sample_to_float_unscaled(snd_fd *sf) {if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(sf->data[sf->loc++]);}
static Float previous_sample_to_float_unscaled(snd_fd *sf) {if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(sf->data[sf->loc--]);}


/* ---------------- zeros ---------------- */
static mus_sample_t next_zero_sample(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf));
  sf->loc++;
  return(MUS_SAMPLE_0);
}

static mus_sample_t previous_zero_sample(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  sf->loc--;
  return(MUS_SAMPLE_0);
}

static Float next_zero_sample_to_float(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf));
  sf->loc++;
  return(0.0);
}

static Float previous_zero_sample_to_float(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf));
  sf->loc--;
  return(0.0);
}


/* ---------------- ramp ---------------- */
static mus_sample_t next_sample_with_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  else
    {
      mus_sample_t val;
      val = (mus_sample_t)(sf->data[sf->loc++] * sf->curval);
      sf->curval += sf->incr;
      return(val);
    }
}

static mus_sample_t previous_sample_with_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else 
    {
      mus_sample_t val;
      val = (mus_sample_t)(sf->data[sf->loc--] * sf->curval);
      sf->curval -= sf->incr;
      return(val);
    }
}

static Float next_sample_to_float_with_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval * MUS_FIX_TO_FLOAT;
      sf->curval += sf->incr;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval * MUS_FIX_TO_FLOAT;
      sf->curval -= sf->incr;
      return(val);
    }
}


/* ---------------- ramp2 ---------------- */

static mus_sample_t next_sample_with_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp2_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp2_to_float(sf)));
}

static Float next_sample_to_float_with_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_ramp2_to_float(sf)));
}

static Float previous_sample_to_float_with_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_ramp2_to_float(sf)));
}



/* ---------------- ramp3 ---------------- */

static mus_sample_t next_sample_with_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp3_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp3_to_float(sf)));
}

static Float next_sample_to_float_with_ramp3(snd_fd *sf) 
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_ramp3_to_float(sf)));
}

static Float previous_sample_to_float_with_ramp3(snd_fd *sf) 
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_ramp3_to_float(sf)));
}


/* ---------------- xramp ---------------- */

static mus_sample_t next_sample_with_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_xramp_to_float(sf)));
}

static mus_sample_t previous_sample_with_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_xramp_to_float(sf)));
}

static Float next_sample_to_float_with_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_xramp_to_float(sf)));
}

static Float previous_sample_to_float_with_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_xramp_to_float(sf)));
}


/* ---------------- ramp on xramp ---------------- */

static mus_sample_t next_sample_with_ramp_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp_xramp_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp_xramp_to_float(sf)));
}

static Float next_sample_to_float_with_ramp_xramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_ramp_xramp(sf)));}
static Float previous_sample_to_float_with_ramp_xramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_ramp_xramp(sf)));}


/* ---------------- ramp2 on xramp ---------------- */

static mus_sample_t next_sample_with_ramp2_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp2_xramp_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp2_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp2_xramp_to_float(sf)));
}

static Float next_sample_to_float_with_ramp2_xramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_ramp2_xramp(sf)));}
static Float previous_sample_to_float_with_ramp2_xramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_ramp2_xramp(sf)));}


/* ---------------- xramp on ramp ---------------- */

static mus_sample_t next_sample_with_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_xramp_ramp_to_float(sf)));
}

static mus_sample_t previous_sample_with_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_xramp_ramp_to_float(sf)));
}

static Float next_sample_to_float_with_xramp_ramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_xramp_ramp(sf)));}
static Float previous_sample_to_float_with_xramp_ramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_xramp_ramp(sf)));}


/* ---------------- ramp on xramp on ramp ---------------- */

static mus_sample_t next_sample_with_ramp_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp_xramp_ramp_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp_xramp_ramp_to_float(sf)));
}

static Float next_sample_to_float_with_ramp_xramp_ramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_ramp_xramp_ramp(sf)));}
static Float previous_sample_to_float_with_ramp_xramp_ramp(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_ramp_xramp_ramp(sf)));}


/* ---------------- xramp2 ---------------- */

static mus_sample_t next_sample_with_xramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_xramp2_to_float(sf)));
}

static mus_sample_t previous_sample_with_xramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_xramp2_to_float(sf)));
}

static Float next_sample_to_float_with_xramp2(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_xramp2(sf)));}
static Float previous_sample_to_float_with_xramp2(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_xramp2(sf)));}


/* ---------------- ramp on xramp2 ---------------- */

static mus_sample_t next_sample_with_ramp_xramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp_xramp2_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp_xramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp_xramp2_to_float(sf)));
}

static Float next_sample_to_float_with_ramp_xramp2(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_with_ramp_xramp2(sf)));}
static Float previous_sample_to_float_with_ramp_xramp2(snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_with_ramp_xramp2(sf)));}


/* ---------------- ptree ---------------- */

static mus_sample_t next_sample_with_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound(sf));
  else return((mus_sample_t)(sf->rscaler * 
			     evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true)));
}

static mus_sample_t previous_sample_with_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else return((mus_sample_t)(sf->rscaler * 
			     evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false)));
}

static Float next_sample_to_float_with_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound(sf));
  else 
    {
      sf->loc++;
      return((mus_sample_t)(sf->rscaler * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true)));
    }
}

static mus_sample_t previous_sample_with_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else 
    {
      sf->loc--;
      return((mus_sample_t)(sf->rscaler * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false)));
    }
}

static Float next_sample_to_float_with_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}


/* -------- ptree on ramp -------- */

static Float next_sample_to_float_with_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval;
      sf->curval += sf->incr;
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval;
      sf->curval -= sf->incr;
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ptree_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp(sf)));}
static mus_sample_t next_sample_with_ptree_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp(sf)));}


/* -------- ptree on ramp2 -------- */

static Float next_sample_to_float_with_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ptree_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp2(sf)));}
static mus_sample_t next_sample_with_ptree_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp2(sf)));}


/* -------- ptree on ramp3 -------- */

static Float next_sample_to_float_with_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * next_ramp3_to_float(sf);
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc--] * previous_ramp3_to_float(sf);
      return(READER_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ptree_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp3(sf)));}
static mus_sample_t next_sample_with_ptree_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp3(sf)));}


/* -------- ptree on xramp -------- */

static Float next_sample_to_float_with_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_xramp(sf)));}
static mus_sample_t previous_sample_with_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_xramp(sf)));}

/* -------- ptree on xramp2 -------- */

static Float next_sample_to_float_with_ptree_xramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ptree_xramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_xramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ptree_xramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_xramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_xramp2(sf)));}
static mus_sample_t previous_sample_with_ptree_xramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_xramp2(sf)));}

/* -------- ramp on ptree -------- */

static Float next_sample_to_float_with_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      return(val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      return(val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree(sf)));}
static mus_sample_t next_sample_with_ramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree(sf)));}

/* -------- ramp2 on ptree -------- */

static Float next_sample_to_float_with_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ramp2_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree(sf)));}
static mus_sample_t next_sample_with_ramp2_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree(sf)));}


/* -------- ramp3 on ptree -------- */

static Float next_sample_to_float_with_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp3_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp3_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ramp3_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_ptree(sf)));}
static mus_sample_t next_sample_with_ramp3_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_ptree(sf)));}

/* -------- ramp on ptree zero -------- */

static Float next_sample_to_float_with_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      sf->loc++;
      return(val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      sf->loc--;
      return(val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree_air(sf)));
}

/* -------- ramp2 on ptree zero -------- */

static Float next_sample_to_float_with_ramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp2_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp2_to_float(sf) *
	     evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp2_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp2_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree_air(sf)));
}


/* -------- ramp3 on ptree zero -------- */

static Float next_sample_to_float_with_ramp3_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else
    {
      sf->loc++;
      return(next_ramp3_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp3_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp3_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp3_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp3_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_ptree_air(sf)));
}


/* -------- xramp on ptree -------- */

static Float next_sample_to_float_with_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_xramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ptree(sf)));}
static mus_sample_t next_sample_with_xramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ptree(sf)));}

static Float next_sample_to_float_with_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ptree_air(sf)));
}


/* -------- xramp2 on ptree -------- */

static Float next_sample_to_float_with_xramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp2_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_xramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp2_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_xramp2_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp2_ptree(sf)));}
static mus_sample_t next_sample_with_xramp2_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp2_ptree(sf)));}

static Float next_sample_to_float_with_xramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_xramp2_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_xramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_xramp2_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_xramp2_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp2_ptree_air(sf)));
}

static mus_sample_t next_sample_with_xramp2_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp2_ptree_air(sf)));
}

/* -------- ramp on xramp on ptree -------- */

static Float next_sample_to_float_with_ramp_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp_xramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ramp_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp_xramp_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ramp_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xramp_ptree(sf)));
}

static mus_sample_t next_sample_with_ramp_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xramp_ptree(sf)));
}

static Float next_sample_to_float_with_ramp_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xramp_ptree_air(sf)));
}

/* -------- ramp2 on xramp on ptree -------- */

static Float next_sample_to_float_with_ramp2_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_xramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ramp2_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_xramp_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ramp2_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_xramp_ptree(sf)));
}

static mus_sample_t next_sample_with_ramp2_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_xramp_ptree(sf)));
}

static Float next_sample_to_float_with_ramp2_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp2_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp2_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp2_xramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp2_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_xramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp2_xramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_xramp_ptree_air(sf)));
}

/* -------- xramp on ramp on ptree -------- */

static Float next_sample_to_float_with_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_ramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_ramp_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_xramp_ramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ramp_ptree(sf)));
}

static mus_sample_t next_sample_with_xramp_ramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ramp_ptree(sf)));
}

static Float next_sample_to_float_with_xramp_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_xramp_ramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_xramp_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_xramp_ramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_xramp_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_xramp_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ramp_ptree_air(sf)));
}

/* -------- ramp on xramp on ramp on ptree -------- */

static Float next_sample_to_float_with_ramp_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp_xramp_ramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ramp_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp_xramp_ramp_to_float(sf) *
	      evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t previous_sample_with_ramp_xramp_ramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xramp_ramp_ptree(sf)));
}

static mus_sample_t next_sample_with_ramp_xramp_ramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xramp_ramp_ptree(sf)));
}

static Float next_sample_to_float_with_ramp_xramp_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp_xramp_ramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp_xramp_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp_xramp_ramp_to_float(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t previous_sample_with_ramp_xramp_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xramp_ramp_ptree_air(sf)));
}

static mus_sample_t next_sample_with_ramp_xramp_ramp_ptree_air(snd_fd *sf)
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xramp_ramp_ptree_air(sf)));
}


/* -------- ramp on ptree on ramp -------- */

static Float next_sample_to_float_with_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      val = sf->curval2 * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * sf->curval, (vct *)XEN_OBJECT_REF(sf->closure), true);
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2 * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * sf->curval, (vct *)XEN_OBJECT_REF(sf->closure), false);
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree_ramp(sf)));
}

static mus_sample_t previous_sample_with_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree_ramp(sf)));
}

/* -------- ramp2 on ptree on ramp -------- */

static Float next_sample_to_float_with_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      val = sf->curval2 * sf->curval3 * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * sf->curval, (vct *)XEN_OBJECT_REF(sf->closure), true);
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2 * sf->curval3 * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * sf->curval, (vct *)XEN_OBJECT_REF(sf->closure), false);
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp2_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree_ramp(sf)));
}

static mus_sample_t previous_sample_with_ramp2_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree_ramp(sf)));
}


/* -------- ramp on ptree on ramp2 -------- */

static Float next_sample_to_float_with_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      val = sf->curval3 * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true);
      sf->curval3 += sf->incr3;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval3 * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ramp2_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false);
      sf->curval3 -= sf->incr3;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp_ptree_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree_ramp2(sf)));}
static mus_sample_t previous_sample_with_ramp_ptree_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree_ramp2(sf)));}


/* -------- ramp on ptree on xramp -------- */

static Float next_sample_to_float_with_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2;
      sf->curval2 += sf->incr2;
      return(val * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2;
      sf->curval2 -= sf->incr2;
      return(val * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t next_sample_with_ramp_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree_xramp(sf)));}
static mus_sample_t previous_sample_with_ramp_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree_xramp(sf)));}


/* -------- ramp2 on ptree on xramp -------- */

static Float next_sample_to_float_with_ramp2_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2 * sf->curval3;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(val * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_ramp2_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval2 * sf->curval3;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(val * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ptree_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t next_sample_with_ramp2_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree_xramp(sf)));}
static mus_sample_t previous_sample_with_ramp2_ptree_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree_xramp(sf)));}


/* -------- xramp on ptree on ramp -------- */

static Float next_sample_to_float_with_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      return(next_xramp_2_to_float(sf) * evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * val, (vct *)XEN_OBJECT_REF(sf->closure), true));
    }
}

static Float previous_sample_to_float_with_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      return(previous_xramp_2_to_float(sf) * evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * val, (vct *)XEN_OBJECT_REF(sf->closure), false));
    }
}

static mus_sample_t next_sample_with_xramp_ptree_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ptree_ramp(sf)));}
static mus_sample_t previous_sample_with_xramp_ptree_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ptree_ramp(sf)));}

/* -------- ptree on ramp on xramp -------- */

static Float next_sample_to_float_with_ptree_ramp_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ramp_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_ramp_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ramp_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_ramp_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_xramp(sf)));}
static mus_sample_t previous_sample_with_ptree_ramp_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_xramp(sf)));}

/* -------- ptree on ramp2 on xramp -------- */

static Float next_sample_to_float_with_ptree_ramp2_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ramp2_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_ramp2_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ramp2_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_ramp2_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp2_xramp(sf)));}
static mus_sample_t previous_sample_with_ptree_ramp2_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp2_xramp(sf)));}

/* -------- ptree on xramp on ramp -------- */

static Float next_sample_to_float_with_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_xramp_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_xramp_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_xramp_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_xramp_ramp(sf)));}
static mus_sample_t previous_sample_with_ptree_xramp_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_xramp_ramp(sf)));}

/* -------- ptree on ramp on xramp on ramp -------- */

static Float next_sample_to_float_with_ptree_ramp_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc++] * next_ramp_xramp_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), true));
}

static Float previous_sample_to_float_with_ptree_ramp_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree, sf->data[sf->loc--] * previous_ramp_xramp_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure), false));
}

static mus_sample_t next_sample_with_ptree_ramp_xramp_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_xramp_ramp(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp_xramp_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_xramp_ramp(sf)));
}

/* ---------------- xen ---------------- */

static Float next_sample_to_float_with_xen(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN val;
      val = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(READER_PTREE_SCALER(sf) * sf->data[sf->loc++]), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
    }
}

static Float previous_sample_to_float_with_xen(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN val;
      val = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(READER_PTREE_SCALER(sf) * sf->data[sf->loc--]), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
    }
}

static mus_sample_t next_sample_with_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen(sf)));}
static mus_sample_t previous_sample_with_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen(sf)));}



/* ---------------- xen on air ---------------- */

static Float next_sample_to_float_with_xen_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN val;
      sf->loc++;
      val = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
    }
}

static Float previous_sample_to_float_with_xen_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN val;
      sf->loc--;
      val = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
    }
}

static mus_sample_t next_sample_with_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen_air(sf)));}
static mus_sample_t previous_sample_with_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen_air(sf)));}



/* ---------------- xen on ramp ---------------- */

static Float next_sample_to_float_with_xen_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc++] * sf->curval;
      sf->curval += sf->incr;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xen_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc--] * sf->curval;
      sf->curval -= sf->incr;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xen_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen_ramp(sf)));}
static mus_sample_t previous_sample_with_xen_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen_ramp(sf)));}


/* -------- xen on xramp -------- */

static Float next_sample_to_float_with_xen_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * next_ptree_xramp_to_float(sf)), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xen_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * previous_ptree_xramp_to_float(sf)), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xen_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen_xramp(sf)));}
static mus_sample_t previous_sample_with_xen_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen_xramp(sf)));}


/* -------- xen on ramp2 -------- */

static Float next_sample_to_float_with_xen_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc++] * next_ramp2_to_float(sf);
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xen_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc--] * previous_ramp2_to_float(sf);
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xen_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen_ramp2(sf)));}
static mus_sample_t previous_sample_with_xen_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen_ramp2(sf)));}


/* -------- xen on ramp3 -------- */

static Float next_sample_to_float_with_xen_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc++] * next_ramp3_to_float(sf);
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_TRUE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xen_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->data[sf->loc--] * previous_ramp3_to_float(sf);
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(val), sf->closure, XEN_FALSE, "xen-channel");
      return(READER_SCALER(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xen_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xen_ramp3(sf)));}
static mus_sample_t previous_sample_with_xen_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xen_ramp3(sf)));}


/* ---------------- ramp on xen ---------------- */

static Float next_sample_to_float_with_ramp_xen(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * READER_PTREE_SCALER(sf)), sf->closure, XEN_TRUE, "xen-channel");
      return(val * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp_xen(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * READER_PTREE_SCALER(sf)), sf->closure, XEN_FALSE, "xen-channel");
      return(val * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xen(sf)));}
static mus_sample_t previous_sample_with_ramp_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xen(sf)));}

/* ---------------- ramp2 on xen ---------------- */

static Float next_sample_to_float_with_ramp2_xen(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * READER_PTREE_SCALER(sf)), sf->closure, XEN_TRUE, "xen-channel");
      return(next_ramp2_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp2_xen(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * READER_PTREE_SCALER(sf)), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_ramp2_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp2_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_xen(sf)));}
static mus_sample_t previous_sample_with_ramp2_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_xen(sf)));}

/* ---------------- ramp3 on xen ---------------- */

static Float next_sample_to_float_with_ramp3_xen(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * READER_PTREE_SCALER(sf)), sf->closure, XEN_TRUE, "xen-channel");
      return(next_ramp3_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp3_xen(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * READER_PTREE_SCALER(sf)), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_ramp3_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp3_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_xen(sf)));}
static mus_sample_t previous_sample_with_ramp3_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_xen(sf)));}


/* ---------------- ramp on xen zero ---------------- */

static Float next_sample_to_float_with_ramp_xen_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      sf->loc++;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_TRUE, "xen-channel");
      return(val * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp_xen_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      sf->loc--;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_FALSE, "xen-channel");
      return(val * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xen_air(sf)));}
static mus_sample_t previous_sample_with_ramp_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xen_air(sf)));}

/* ---------------- ramp2 on xen zero ---------------- */

static Float next_sample_to_float_with_ramp2_xen_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc++;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_TRUE, "xen-channel");
      return(next_ramp2_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp2_xen_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc--;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_ramp2_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp2_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_xen_air(sf)));}
static mus_sample_t previous_sample_with_ramp2_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_xen_air(sf)));}

/* ---------------- ramp3 on xen zero ---------------- */

static Float next_sample_to_float_with_ramp3_xen_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc++;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_TRUE, "xen-channel");
      return(next_ramp3_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_ramp3_xen_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc--;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_ramp3_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_ramp3_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_xen_air(sf)));}
static mus_sample_t previous_sample_with_ramp3_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_xen_air(sf)));}


/* ---------------- xramp on xen ---------------- */

static Float next_sample_to_float_with_xramp_xen(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * READER_PTREE_SCALER(sf)), sf->closure, XEN_TRUE, "xen-channel");
      return(next_xramp_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xramp_xen(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * READER_PTREE_SCALER(sf)), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_xramp_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xramp_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_xen(sf)));}
static mus_sample_t previous_sample_with_xramp_xen(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_xen(sf)));}

static Float next_sample_to_float_with_xramp_xen_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc++;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_TRUE, "xen-channel");
      return(next_xramp_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static Float previous_sample_to_float_with_xramp_xen_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      XEN xval;
      sf->loc--;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(0.0), sf->closure, XEN_FALSE, "xen-channel");
      return(previous_xramp_to_float(sf) * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0));
    }
}

static mus_sample_t next_sample_with_xramp_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_xen_air(sf)));}
static mus_sample_t previous_sample_with_xramp_xen_air(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_xen_air(sf)));}




/* -------- ramp on xen on ramp -------- */

static Float next_sample_to_float_with_ramp_xen_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * sf->curval), sf->closure, XEN_TRUE, "xen-channel");
      val = sf->curval2 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp_xen_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * sf->curval), sf->closure, XEN_FALSE, "xen-channel");
      val = sf->curval2 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp_xen_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xen_ramp(sf)));
}

static mus_sample_t previous_sample_with_ramp_xen_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xen_ramp(sf)));
}

/* -------- ramp2 on xen on ramp -------- */

static Float next_sample_to_float_with_ramp2_xen_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * sf->curval), sf->closure, XEN_TRUE, "xen-channel");
      val = sf->curval2 * sf->curval3 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp2_xen_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * sf->curval), sf->closure, XEN_FALSE, "xen-channel");
      val = sf->curval2 * sf->curval3 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp2_xen_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_xen_ramp(sf)));
}

static mus_sample_t previous_sample_with_ramp2_xen_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_xen_ramp(sf)));
}


/* -------- ramp on xen on ramp2 -------- */

static Float next_sample_to_float_with_ramp_xen_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val; /* 2 has reader scale, 1 has ptree scale */
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc++] * next_ramp2_to_float(sf)), sf->closure, XEN_TRUE, "xen-channel");
      val = sf->curval3 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval3 += sf->incr3;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp_xen_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      XEN xval;
      xval = XEN_CALL_3((XEN)(sf->ptree), C_TO_XEN_DOUBLE(sf->data[sf->loc--] * previous_ramp2_to_float(sf)), sf->closure, XEN_FALSE, "xen-channel");
      val = sf->curval3 * XEN_TO_C_DOUBLE_OR_ELSE(xval, 0.0);
      sf->curval3 -= sf->incr3;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp_xen_ramp2(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_xen_ramp2(sf)));
}

static mus_sample_t previous_sample_with_ramp_xen_ramp2(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_xen_ramp2(sf)));
}




/* ---------------- ptree2 ---------------- */

static Float next_sample_to_float_with_ptree2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc++], 
									 (vct *)XEN_OBJECT_REF(sf->closure), 
									 true), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), 
			      true));
}

static Float previous_sample_to_float_with_ptree2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc--], 
									 (vct *)XEN_OBJECT_REF(sf->closure), 
									 false), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), 
			      false));
}

static mus_sample_t next_sample_with_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2(sf)));}
static mus_sample_t previous_sample_with_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2(sf)));}


/* ---------------- ptree2 on air ---------------- */

static Float next_sample_to_float_with_ptree2_zero(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else
    {
      sf->loc++;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree2_zero(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree2_zero(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2_zero(sf)));}
static mus_sample_t previous_sample_with_ptree2_zero(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2_zero(sf)));}


/* ---------------- ptree2 on ramp ---------------- */

static Float next_sample_to_float_with_ptree2_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval;
      sf->curval += sf->incr;
      /* curval and incr have possible post-ramp but pre-ptree scaling embedded (choose_accessor) */
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree2_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval;
      sf->curval -= sf->incr;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree2_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2_ramp(sf)));}
static mus_sample_t previous_sample_with_ptree2_ramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2_ramp(sf)));}

/* ---------------- ramp on ptree2 ---------------- */

static Float next_sample_to_float_with_ramp_ptree2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      return(val * evaluate_ptreec(sf->ptree1, 
				   READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									      READER_PTREE_SCALER(sf) * sf->data[sf->loc++], 
									      (vct *)XEN_OBJECT_REF(sf->closure), true), 
				   (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      return(val * evaluate_ptreec(sf->ptree1, 
				   READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									      READER_PTREE_SCALER(sf) * sf->data[sf->loc--], 
									      (vct *)XEN_OBJECT_REF(sf->closure), false), 
				   (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ramp_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree2(sf)));}
static mus_sample_t previous_sample_with_ramp_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree2(sf)));}

static Float next_sample_to_float_with_ramp_ptree2_zero(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval += sf->incr;
      sf->loc++;
      return(val * evaluate_ptreec(sf->ptree1, 
				   READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
				   (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree2_zero(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval; 
      sf->curval -= sf->incr;
      sf->loc--;
      return(val * evaluate_ptreec(sf->ptree1, 
				   READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
				   (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ramp_ptree2_zero(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree2_zero(sf)));}
static mus_sample_t previous_sample_with_ramp_ptree2_zero(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree2_zero(sf)));}

/* ---------------- ramp2 on ptree2 ---------------- */

static Float next_sample_to_float_with_ramp2_ptree2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc++], 
									 (vct *)XEN_OBJECT_REF(sf->closure), true), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), true));
}

static Float previous_sample_to_float_with_ramp2_ptree2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc--], 
									 (vct *)XEN_OBJECT_REF(sf->closure), false), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), false));
}

static mus_sample_t next_sample_with_ramp2_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree2(sf)));}
static mus_sample_t previous_sample_with_ramp2_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree2(sf)));}

static Float next_sample_to_float_with_ramp2_ptree2_zero(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp2_to_float(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ramp2_ptree2_zero(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp2_to_float(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ramp2_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp2_ptree2_zero(sf)));
}

static mus_sample_t previous_sample_with_ramp2_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp2_ptree2_zero(sf)));
}

/* ---------------- ramp3 on ptree2 ---------------- */

static Float next_sample_to_float_with_ramp3_ptree2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp3_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc++], 
									 (vct *)XEN_OBJECT_REF(sf->closure), true), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), true));
}

static Float previous_sample_to_float_with_ramp3_ptree2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp3_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc--], 
									 (vct *)XEN_OBJECT_REF(sf->closure), false), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), false));
}

static mus_sample_t next_sample_with_ramp3_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_ptree2(sf)));}
static mus_sample_t previous_sample_with_ramp3_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_ptree2(sf)));}

static Float next_sample_to_float_with_ramp3_ptree2_zero(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_ramp3_to_float(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ramp3_ptree2_zero(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_ramp3_to_float(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ramp3_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp3_ptree2_zero(sf)));
}

static mus_sample_t previous_sample_with_ramp3_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp3_ptree2_zero(sf)));
}

/* ---------------- xramp on ptree2 ---------------- */

static Float next_sample_to_float_with_xramp_ptree2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc++], 
									 (vct *)XEN_OBJECT_REF(sf->closure), true), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), true));
}

static Float previous_sample_to_float_with_xramp_ptree2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 READER_PTREE_SCALER(sf) * sf->data[sf->loc--], 
									 (vct *)XEN_OBJECT_REF(sf->closure), false), 
			      (vct *)XEN_OBJECT_REF(sf->closure1), false));
}

static mus_sample_t next_sample_with_xramp_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ptree2(sf)));}
static mus_sample_t previous_sample_with_xramp_ptree2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ptree2(sf)));}

static Float next_sample_to_float_with_xramp_ptree2_zero(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(next_xramp_to_float(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_xramp_ptree2_zero(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(previous_xramp_to_float(sf) *
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_xramp_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_xramp_ptree2_zero(sf)));
}

static mus_sample_t previous_sample_with_xramp_ptree2_zero(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_xramp_ptree2_zero(sf)));
}


/* ---------------- ptree on ramp on ptree ---------------- */

static Float next_sample_to_float_with_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval; /* has ptree2 scaler */
      sf->curval += sf->incr;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval;
      sf->curval -= sf->incr;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_ptree(sf)));}
static mus_sample_t previous_sample_with_ptree_ramp_ptree(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_ptree(sf)));}

static Float next_sample_to_float_with_ptree_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc++;
      val = sf->curval; /* has ptree2 scaler */
      sf->curval += sf->incr;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc--;
      val = sf->curval;
      sf->curval -= sf->incr;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_ptree_air(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_ptree_air(sf)));
}


/* ---------------- ptree on ramp2 on ptree ---------------- */

static Float next_sample_to_float_with_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval * sf->curval2; /* has ptree2 scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval * sf->curval2;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp2_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp2_ptree(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp2_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp2_ptree(sf)));
}

static Float next_sample_to_float_with_ptree_ramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc++;
      val = sf->curval * sf->curval2; /* has ptree2 scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp2_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc--;
      val = sf->curval * sf->curval2;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp2_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp2_ptree_air(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp2_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp2_ptree_air(sf)));
}

/* ---------------- ptree on ramp3 on ptree ---------------- */

static Float next_sample_to_float_with_ptree_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval * sf->curval2 * sf->curval3; /* has ptree2 scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->curval * sf->curval2 * sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, READER_PTREE_SCALER(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp3_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp3_ptree(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp3_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp3_ptree(sf)));
}

static Float next_sample_to_float_with_ptree_ramp3_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc++;
      val = sf->curval * sf->curval2 * sf->curval3; /* has ptree2 scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp3_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      sf->loc--;
      val = sf->curval * sf->curval2 * sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val * evaluate_ptreec(sf->ptree, 0.0, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp3_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp3_ptree_air(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp3_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp3_ptree_air(sf)));
}


/* ---------------- ptree2 on ramp2 ---------------- */

static Float next_sample_to_float_with_ptree2_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval * sf->curval2;
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree2_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval * sf->curval2;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree2_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2_ramp2(sf)));}
static mus_sample_t previous_sample_with_ptree2_ramp2(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2_ramp2(sf)));}

/* ---------------- ptree2 on ramp3 ---------------- */

static Float next_sample_to_float_with_ptree2_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval * sf->curval2 * sf->curval3;
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree2_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval * sf->curval2 * sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, val, (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree2_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2_ramp3(sf)));}
static mus_sample_t previous_sample_with_ptree2_ramp3(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2_ramp3(sf)));}


/* ---------------- ptree on ramp on ptree on ramp ---------------- */

static Float next_sample_to_float_with_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval; /* has ptree scaler */
      val1 = sf->curval2; /* has ptree2 scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval;
      val1 = sf->curval2;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_ptree_ramp(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_ptree_ramp(sf)));
}


/* ---------------- ptree on ramp on ptree on ramp2 ---------------- */

static Float next_sample_to_float_with_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval * sf->curval2; /* has ptree scaler */
      val1 = sf->curval3; 
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval * sf->curval2;
      val1 = sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp_ptree_ramp2(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp_ptree_ramp2(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp_ptree_ramp2(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp_ptree_ramp2(sf)));
}

/* ---------------- ptree on ramp2 on ptree on ramp ---------------- */

static Float next_sample_to_float_with_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval; /* has ptree scaler */
      val1 = sf->curval2 * sf->curval3; 
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val, val1;
      val = sf->curval;
      val1 = sf->curval2 * sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_ramp2_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_ramp2_ptree_ramp(sf)));
}

static mus_sample_t previous_sample_with_ptree_ramp2_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_ramp2_ptree_ramp(sf)));
}

/* ---------------- ramp on ptree on ramp on ptree on ramp ---------------- */

static Float next_sample_to_float_with_ramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val, val1, val2;
      val = sf->curval;  /* has ptree scaler */
      val1 = sf->curval2; /* has ptree2 scaler */
      val2 = sf->curval3; /* has reader scaler */
      sf->curval += sf->incr;
      sf->curval2 += sf->incr2;
      sf->curval3 += sf->incr3;
      return(val2 * evaluate_ptreec(sf->ptree1, 
				    val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure), true), 
				    (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      Float val, val1, val2;
      val = sf->curval;
      val1 = sf->curval2;
      val2 = sf->curval3;
      sf->curval -= sf->incr;
      sf->curval2 -= sf->incr2;
      sf->curval3 -= sf->incr3;
      return(val2 * evaluate_ptreec(sf->ptree1, 
				    val1 * evaluate_ptreec(sf->ptree, val * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure), false), 
				    (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ramp_ptree_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ramp_ptree_ramp_ptree_ramp(sf)));
}

static mus_sample_t previous_sample_with_ramp_ptree_ramp_ptree_ramp(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ramp_ptree_ramp_ptree_ramp(sf)));
}

/* -------- ptree2 on xramp -------- */

static Float next_sample_to_float_with_ptree2_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 sf->data[sf->loc++] * next_ptree_xramp_to_float(sf), 
									 (vct *)XEN_OBJECT_REF(sf->closure), true),
			      (vct *)XEN_OBJECT_REF(sf->closure1), true));
}

static Float previous_sample_to_float_with_ptree2_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      READER_PTREE2_SCALER(sf) * evaluate_ptreec(sf->ptree, 
									 sf->data[sf->loc--] * previous_ptree_xramp_to_float(sf), 
									 (vct *)XEN_OBJECT_REF(sf->closure), false),
			      (vct *)XEN_OBJECT_REF(sf->closure1), false));
}

static mus_sample_t next_sample_with_ptree2_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree2_xramp(sf)));}
static mus_sample_t previous_sample_with_ptree2_xramp(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree2_xramp(sf)));}

/* -------- ptree on xramp on ptree -------- */

static Float next_sample_to_float_with_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      next_ptree_xramp_1_to_float(sf) * evaluate_ptreec(sf->ptree, 
									      sf->data[sf->loc++] * READER_PTREE_SCALER(sf),
									      (vct *)XEN_OBJECT_REF(sf->closure), true),
			      (vct *)XEN_OBJECT_REF(sf->closure1), true));
}

static Float previous_sample_to_float_with_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      evaluate_ptreec(sf->ptree1, 
			      previous_ptree_xramp_1_to_float(sf) * evaluate_ptreec(sf->ptree, 
										    sf->data[sf->loc--] * READER_PTREE_SCALER(sf),
										    (vct *)XEN_OBJECT_REF(sf->closure), false),
			      (vct *)XEN_OBJECT_REF(sf->closure1), false));
}

static mus_sample_t next_sample_with_ptree_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_xramp_ptree(sf)));
}

static mus_sample_t previous_sample_with_ptree_xramp_ptree(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_xramp_ptree(sf)));
}


static Float next_sample_to_float_with_ptree_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      sf->loc++;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     next_ptree_xramp_1_to_float(sf) * evaluate_ptreec(sf->ptree, 
									       0.0,
									       (vct *)XEN_OBJECT_REF(sf->closure), true),
			     (vct *)XEN_OBJECT_REF(sf->closure1), true));
    }
}

static Float previous_sample_to_float_with_ptree_xramp_ptree_air(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else 
    {
      sf->loc--;
      return(READER_SCALER(sf) * 
	     evaluate_ptreec(sf->ptree1, 
			     previous_ptree_xramp_1_to_float(sf) * evaluate_ptreec(sf->ptree, 
										   0.0,
										   (vct *)XEN_OBJECT_REF(sf->closure), false),
			     (vct *)XEN_OBJECT_REF(sf->closure1), false));
    }
}

static mus_sample_t next_sample_with_ptree_xramp_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(next_sample_to_float_with_ptree_xramp_ptree_air(sf)));
}

static mus_sample_t previous_sample_with_ptree_xramp_ptree_air(snd_fd *sf) 
{
  return(MUS_FLOAT_TO_SAMPLE(previous_sample_to_float_with_ptree_xramp_ptree_air(sf)));
}



/* -------------------------------------------------------------------------------- */


static void swap_readers(snd_fd *sf)
{
  mus_sample_t (*rrun)(struct snd_fd *sf);
  Float (*rrunf)(struct snd_fd *sf);
  rrun = sf->run;
  rrunf = sf->runf;
  sf->run = sf->rev_run;
  sf->runf = sf->rev_runf;
  sf->rev_run = rrun;
  sf->rev_runf = rrunf;
}

void read_sample_change_direction(snd_fd *sf, int dir1) /* can't use "dir" on Mac */
{
  /* direction reversal can happen in dac(speed arrow), src gen, or user can call next/previous independent of initial dir */
  swap_readers(sf);
  sf->direction = dir1;
  /* can't optimize anything here -- some accessors have state, but how to handle the loc=-1 case? */
  if ((dir1 == READ_FORWARD) && (sf->loc < 0)) 
    sf->loc = 0;
  else read_sample(sf);
}

Float protected_next_sample_to_float(snd_fd *sf)
{
  if (sf->direction == READ_BACKWARD) 
    read_sample_change_direction(sf, READ_FORWARD);
  return(read_sample_to_float(sf));
}

Float protected_previous_sample_to_float(snd_fd *sf)
{
  if (sf->direction == READ_FORWARD) 
    read_sample_change_direction(sf, READ_BACKWARD);
  return(read_sample_to_float(sf));
}

static void reader_out_of_data(snd_fd *sf)
{
  sf->at_eof = true;
  sf->run = end_sample;
  sf->runf = end_sample_to_float;
  sf->rev_run = end_sample;
  sf->rev_runf = end_sample_to_float;
}

static snd_fd *cancel_reader(snd_fd *sf)
{
  sf->current_sound = NULL;
  sf->cbi = 0;
  reader_out_of_data(sf);
  return(sf);
}

static void get_sf_closure(snd_fd *sf)
{
  XEN proc;
  proc = sf->cp->ptree_inits[READER_PTREE_INDEX(sf)];
  if ((sf->closure) && (sf->closure != empty_closure) && (XEN_BOUND_P(sf->closure)))
    snd_unprotect_at(sf->protect);
  sf->closure = empty_closure;
  if (XEN_PROCEDURE_P(proc))
    {
      sf->closure = XEN_CALL_2(proc,
			       C_TO_XEN_OFF_T(sf->frag_pos + READER_PTREE_POSITION(sf)),
			       C_TO_XEN_OFF_T(READER_PTREE_DUR(sf)),
			       "ptree-channel init func");
      if (XEN_BOUND_P(sf->closure))
	sf->protect = snd_protect(sf->closure);
    }
}

static void get_sf_closure1(snd_fd *sf)
{
  XEN proc;
  proc = sf->cp->ptree_inits[READER_PTREE2_INDEX(sf)];
  if ((sf->closure1) && (sf->closure1 != empty_closure) && (XEN_BOUND_P(sf->closure1)))
    snd_unprotect_at(sf->protect1);
  sf->closure1 = empty_closure;
  if (XEN_PROCEDURE_P(proc))
    {
      sf->closure1 = XEN_CALL_2(proc,
				C_TO_XEN_OFF_T(sf->frag_pos + READER_PTREE2_POSITION(sf)),
				C_TO_XEN_OFF_T(READER_PTREE2_DUR(sf)),
				"ptree-channel init func");
      if (XEN_BOUND_P(sf->closure1))
	sf->protect1 = snd_protect(sf->closure1);
    }
}

static void setup_ramp(snd_fd *sf, double rmp0, double rmp1)
{
  if (READER_LOCAL_END(sf) == READER_LOCAL_POSITION(sf))
    sf->incr = 0.0;
  else sf->incr = (double)(rmp1 - rmp0) / (double)(READER_LOCAL_END(sf) - READER_LOCAL_POSITION(sf));
  sf->curval = rmp0 + sf->incr * sf->frag_pos;
}

static void setup_ramp2(snd_fd *sf, double rmp0, double rmp1)
{
  if (READER_LOCAL_END(sf) == READER_LOCAL_POSITION(sf))
    sf->incr2 = 0.0;
  else sf->incr2 = (double)(rmp1 - rmp0) / (double)(READER_LOCAL_END(sf) - READER_LOCAL_POSITION(sf));
  sf->curval2 = rmp0 + sf->incr2 * sf->frag_pos;
}

static void setup_ramp3(snd_fd *sf, double rmp0, double rmp1)
{
  if (READER_LOCAL_END(sf) == READER_LOCAL_POSITION(sf))
    sf->incr3 = 0.0;
  else sf->incr3 = (double)(rmp1 - rmp0) / (double)(READER_LOCAL_END(sf) - READER_LOCAL_POSITION(sf));
  sf->curval3 = rmp0 + sf->incr3 * sf->frag_pos;
}

static void choose_accessor(snd_fd *sf)
{
  /* fragment-specific reader choice */
  switch (READER_TYPE(sf))
    {
    case ED_ZERO:
      sf->run = next_zero_sample;
      sf->runf = next_zero_sample_to_float;
      sf->rev_run = previous_zero_sample;
      sf->rev_runf = previous_zero_sample_to_float;
      break;
    case ED_RAMP:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->run = next_sample_with_ramp;
      sf->runf = next_sample_to_float_with_ramp;
      sf->rev_run = previous_sample_with_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp;
      break;
    case ED_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->run = next_sample_with_xramp;
      sf->runf = next_sample_to_float_with_xramp;
      sf->rev_run = previous_sample_with_xramp;
      sf->rev_runf = previous_sample_to_float_with_xramp;
      break;
    case ED_XRAMP2:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->run = next_sample_with_xramp2;
      sf->runf = next_sample_to_float_with_xramp2;
      sf->rev_run = previous_sample_with_xramp2;
      sf->rev_runf = previous_sample_to_float_with_xramp2;
      break;
    case ED_RAMP_XRAMP2:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->run = next_sample_with_ramp_xramp2;
      sf->runf = next_sample_to_float_with_ramp_xramp2;
      sf->rev_run = previous_sample_with_ramp_xramp2;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp2;
      break;
    case ED_RAMP2:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->run = next_sample_with_ramp2;
      sf->runf = next_sample_to_float_with_ramp2;
      sf->rev_run = previous_sample_with_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ramp2;
      break;
    case ED_RAMP_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->run = next_sample_with_ramp_xramp;
      sf->runf = next_sample_to_float_with_ramp_xramp;
      sf->rev_run = previous_sample_with_ramp_xramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp;
      break;
    case ED_RAMP2_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->run = next_sample_with_ramp2_xramp;
      sf->runf = next_sample_to_float_with_ramp2_xramp;
      sf->rev_run = previous_sample_with_ramp2_xramp;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xramp;
      break;
    case ED_XRAMP_RAMP:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->run = next_sample_with_xramp_ramp;
      sf->runf = next_sample_to_float_with_xramp_ramp;
      sf->rev_run = previous_sample_with_xramp_ramp;
      sf->rev_runf = previous_sample_to_float_with_xramp_ramp;
      break;
    case ED_RAMP_XRAMP_RAMP:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->run = next_sample_with_ramp_xramp_ramp;
      sf->runf = next_sample_to_float_with_ramp_xramp_ramp;
      sf->rev_run = previous_sample_with_ramp_xramp_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp_ramp;
      break;
    case ED_RAMP3:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->run = next_sample_with_ramp3;
      sf->runf = next_sample_to_float_with_ramp3;
      sf->rev_run = previous_sample_with_ramp3;
      sf->rev_runf = previous_sample_to_float_with_ramp3;
      break;
    case ED_PTREE:
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree;
      sf->runf = next_sample_to_float_with_ptree;
      sf->rev_run = previous_sample_with_ptree;
      sf->rev_runf = previous_sample_to_float_with_ptree;
      break;
    case ED_PTREE_ZERO:
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_air;
      sf->runf = next_sample_to_float_with_ptree_air;
      sf->rev_run = previous_sample_with_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ptree_air;
      break;

    case ED_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp;
      sf->runf = next_sample_to_float_with_ptree_ramp;
      sf->rev_run = previous_sample_with_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp;
      break;
    case ED_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp2;
      sf->runf = next_sample_to_float_with_ptree_ramp2;
      sf->rev_run = previous_sample_with_ptree_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp2;
      break;
    case ED_PTREE_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp3;
      sf->runf = next_sample_to_float_with_ptree_ramp3;
      sf->rev_run = previous_sample_with_ptree_ramp3;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp3;
      break;
    case ED_PTREE_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_xramp;
      sf->runf = next_sample_to_float_with_ptree_xramp;
      sf->rev_run = previous_sample_with_ptree_xramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_xramp;
      break;
    case ED_PTREE_XRAMP2:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_xramp2;
      sf->runf = next_sample_to_float_with_ptree_xramp2;
      sf->rev_run = previous_sample_with_ptree_xramp2;
      sf->rev_runf = previous_sample_to_float_with_ptree_xramp2;
      break;
    case ED_RAMP_PTREE:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_ptree;
      sf->runf = next_sample_to_float_with_ramp_ptree;
      sf->rev_run = previous_sample_with_ramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree;
      break;
    case ED_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_ptree_air;
      sf->runf = next_sample_to_float_with_ramp_ptree_air;
      sf->rev_run = previous_sample_with_ramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree_air;
      break;
    case ED_RAMP2_PTREE:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_ptree;
      sf->runf = next_sample_to_float_with_ramp2_ptree;
      sf->rev_run = previous_sample_with_ramp2_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree;
      break;
    case ED_RAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_ptree_air;
      sf->runf = next_sample_to_float_with_ramp2_ptree_air;
      sf->rev_run = previous_sample_with_ramp2_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree_air;
      break;
    case ED_RAMP3_PTREE:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp3_ptree;
      sf->runf = next_sample_to_float_with_ramp3_ptree;
      sf->rev_run = previous_sample_with_ramp3_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp3_ptree;
      break;
    case ED_RAMP3_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp3_ptree_air;
      sf->runf = next_sample_to_float_with_ramp3_ptree_air;
      sf->rev_run = previous_sample_with_ramp3_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp3_ptree_air;
      break;

    case ED_XRAMP_PTREE:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_ptree;
      sf->runf = next_sample_to_float_with_xramp_ptree;
      sf->rev_run = previous_sample_with_xramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_xramp_ptree;
      break;
    case ED_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_ptree_air;
      sf->runf = next_sample_to_float_with_xramp_ptree_air;
      sf->rev_run = previous_sample_with_xramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_xramp_ptree_air;
      break;
    case ED_XRAMP2_PTREE:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp2_ptree;
      sf->runf = next_sample_to_float_with_xramp2_ptree;
      sf->rev_run = previous_sample_with_xramp2_ptree;
      sf->rev_runf = previous_sample_to_float_with_xramp2_ptree;
      break;
    case ED_XRAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp2_ptree_air;
      sf->runf = next_sample_to_float_with_xramp2_ptree_air;
      sf->rev_run = previous_sample_with_xramp2_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_xramp2_ptree_air;
      break;
    case ED_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_ptree_ramp;
      sf->runf = next_sample_to_float_with_ramp_ptree_ramp;
      sf->rev_run = previous_sample_with_ramp_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree_ramp;
      break;
    case ED_RAMP2_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_ptree_ramp;
      sf->runf = next_sample_to_float_with_ramp2_ptree_ramp;
      sf->rev_run = previous_sample_with_ramp2_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree_ramp;
      break;
    case ED_RAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_ptree_ramp2;
      sf->runf = next_sample_to_float_with_ramp_ptree_ramp2;
      sf->rev_run = previous_sample_with_ramp_ptree_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree_ramp2;
      break;
    case ED_RAMP_PTREE_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_ptree_xramp;
      sf->runf = next_sample_to_float_with_ramp_ptree_xramp;
      sf->rev_run = previous_sample_with_ramp_ptree_xramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree_xramp;
      break;
    case ED_XRAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_ptree_ramp;
      sf->runf = next_sample_to_float_with_xramp_ptree_ramp;
      sf->rev_run = previous_sample_with_xramp_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_xramp_ptree_ramp;
      break;
    case ED_RAMP2_PTREE_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_ptree_xramp;
      sf->runf = next_sample_to_float_with_ramp2_ptree_xramp;
      sf->rev_run = previous_sample_with_ramp2_ptree_xramp;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree_xramp;
      break;

    case ED_PTREE_RAMP_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp_xramp;
      sf->runf = next_sample_to_float_with_ptree_ramp_xramp;
      sf->rev_run = previous_sample_with_ptree_ramp_xramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_xramp;
      break;
    case ED_PTREE_RAMP2_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp2_xramp;
      sf->runf = next_sample_to_float_with_ptree_ramp2_xramp;
      sf->rev_run = previous_sample_with_ptree_ramp2_xramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp2_xramp;
      break;
    case ED_PTREE_RAMP_XRAMP_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_ramp_xramp_ramp;
      sf->runf = next_sample_to_float_with_ptree_ramp_xramp_ramp;
      sf->rev_run = previous_sample_with_ptree_ramp_xramp_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_xramp_ramp;
      break;
    case ED_PTREE_XRAMP_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ptree_xramp_ramp;
      sf->runf = next_sample_to_float_with_ptree_xramp_ramp;
      sf->rev_run = previous_sample_with_ptree_xramp_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_xramp_ramp;
      break;

    case ED_RAMP_XRAMP_PTREE:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xramp_ptree;
      sf->runf = next_sample_to_float_with_ramp_xramp_ptree;
      sf->rev_run = previous_sample_with_ramp_xramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp_ptree;
      break;
    case ED_RAMP_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xramp_ptree_air;
      sf->runf = next_sample_to_float_with_ramp_xramp_ptree_air;
      sf->rev_run = previous_sample_with_ramp_xramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp_ptree_air;
      break;
    case ED_XRAMP_RAMP_PTREE:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_ramp_ptree;
      sf->runf = next_sample_to_float_with_xramp_ramp_ptree;
      sf->rev_run = previous_sample_with_xramp_ramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_xramp_ramp_ptree;
      break;
    case ED_XRAMP_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_ramp_ptree_air;
      sf->runf = next_sample_to_float_with_xramp_ramp_ptree_air;
      sf->rev_run = previous_sample_with_xramp_ramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_xramp_ramp_ptree_air;
      break;
    case ED_RAMP_XRAMP_RAMP_PTREE:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xramp_ramp_ptree;
      sf->runf = next_sample_to_float_with_ramp_xramp_ramp_ptree;
      sf->rev_run = previous_sample_with_ramp_xramp_ramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp_ramp_ptree;
      break;
    case ED_RAMP_XRAMP_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xramp_ramp_ptree_air;
      sf->runf = next_sample_to_float_with_ramp_xramp_ramp_ptree_air;
      sf->rev_run = previous_sample_with_ramp_xramp_ramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp_xramp_ramp_ptree_air;
      break;
    case ED_RAMP2_XRAMP_PTREE:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_xramp_ptree;
      sf->runf = next_sample_to_float_with_ramp2_xramp_ptree;
      sf->rev_run = previous_sample_with_ramp2_xramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xramp_ptree;
      break;
    case ED_RAMP2_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_xramp_ptree_air;
      sf->runf = next_sample_to_float_with_ramp2_xramp_ptree_air;
      sf->rev_run = previous_sample_with_ramp2_xramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xramp_ptree_air;
      break;

    case ED_XEN:
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen;
      sf->runf = next_sample_to_float_with_xen;
      sf->rev_run = previous_sample_with_xen;
      sf->rev_runf = previous_sample_to_float_with_xen;
      break;
    case ED_XEN_ZERO:
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen_air;
      sf->runf = next_sample_to_float_with_xen_air;
      sf->rev_run = previous_sample_with_xen_air;
      sf->rev_runf = previous_sample_to_float_with_xen_air;
      break;
    case ED_XEN_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen_ramp;
      sf->runf = next_sample_to_float_with_xen_ramp;
      sf->rev_run = previous_sample_with_xen_ramp;
      sf->rev_runf = previous_sample_to_float_with_xen_ramp;
      break;
    case ED_XEN_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen_ramp2;
      sf->runf = next_sample_to_float_with_xen_ramp2;
      sf->rev_run = previous_sample_with_xen_ramp2;
      sf->rev_runf = previous_sample_to_float_with_xen_ramp2;
      break;
    case ED_XEN_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen_ramp3;
      sf->runf = next_sample_to_float_with_xen_ramp3;
      sf->rev_run = previous_sample_with_xen_ramp3;
      sf->rev_runf = previous_sample_to_float_with_xen_ramp3;
      break;
    case ED_XEN_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xen_xramp;
      sf->runf = next_sample_to_float_with_xen_xramp;
      sf->rev_run = previous_sample_with_xen_xramp;
      sf->rev_runf = previous_sample_to_float_with_xen_xramp;
      break;
    case ED_RAMP_XEN:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xen;
      sf->runf = next_sample_to_float_with_ramp_xen;
      sf->rev_run = previous_sample_with_ramp_xen;
      sf->rev_runf = previous_sample_to_float_with_ramp_xen;
      break;
    case ED_RAMP_XEN_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xen_air;
      sf->runf = next_sample_to_float_with_ramp_xen_air;
      sf->rev_run = previous_sample_with_ramp_xen_air;
      sf->rev_runf = previous_sample_to_float_with_ramp_xen_air;
      break;

    case ED_XRAMP_XEN:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_xen;
      sf->runf = next_sample_to_float_with_xramp_xen;
      sf->rev_run = previous_sample_with_xramp_xen;
      sf->rev_runf = previous_sample_to_float_with_xramp_xen;
      break;
    case ED_XRAMP_XEN_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_xramp_xen_air;
      sf->runf = next_sample_to_float_with_xramp_xen_air;
      sf->rev_run = previous_sample_with_xramp_xen_air;
      sf->rev_runf = previous_sample_to_float_with_xramp_xen_air;
      break;

    case ED_RAMP2_XEN:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_xen;
      sf->runf = next_sample_to_float_with_ramp2_xen;
      sf->rev_run = previous_sample_with_ramp2_xen;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xen;
      break;
    case ED_RAMP2_XEN_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_xen_air;
      sf->runf = next_sample_to_float_with_ramp2_xen_air;
      sf->rev_run = previous_sample_with_ramp2_xen_air;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xen_air;
      break;
    case ED_RAMP3_XEN:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp3_xen;
      sf->runf = next_sample_to_float_with_ramp3_xen;
      sf->rev_run = previous_sample_with_ramp3_xen;
      sf->rev_runf = previous_sample_to_float_with_ramp3_xen;
      break;
    case ED_RAMP3_XEN_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp3_xen_air;
      sf->runf = next_sample_to_float_with_ramp3_xen_air;
      sf->rev_run = previous_sample_with_ramp3_xen_air;
      sf->rev_runf = previous_sample_to_float_with_ramp3_xen_air;
      break;
    case ED_RAMP_XEN_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xen_ramp;
      sf->runf = next_sample_to_float_with_ramp_xen_ramp;
      sf->rev_run = previous_sample_with_ramp_xen_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_xen_ramp;
      break;
    case ED_RAMP2_XEN_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp2_xen_ramp;
      sf->runf = next_sample_to_float_with_ramp2_xen_ramp;
      sf->rev_run = previous_sample_with_ramp2_xen_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp2_xen_ramp;
      break;
    case ED_RAMP_XEN_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = (void *)(sf->cp->xens[READER_PTREE_INDEX(sf)]);
      get_sf_closure(sf);
      sf->run = next_sample_with_ramp_xen_ramp2;
      sf->runf = next_sample_to_float_with_ramp_xen_ramp2;
      sf->rev_run = previous_sample_with_ramp_xen_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ramp_xen_ramp2;
      break;

    case ED_PTREE2:
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2;
      sf->runf = next_sample_to_float_with_ptree2;
      sf->rev_run = previous_sample_with_ptree2;
      sf->rev_runf = previous_sample_to_float_with_ptree2;
      break;
    case ED_PTREE2_ZERO:
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2_zero;
      sf->runf = next_sample_to_float_with_ptree2_zero;
      sf->rev_run = previous_sample_with_ptree2_zero;
      sf->rev_runf = previous_sample_to_float_with_ptree2_zero;
      break;
    case ED_PTREE2_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2_ramp;
      sf->runf = next_sample_to_float_with_ptree2_ramp;
      sf->rev_run = previous_sample_with_ptree2_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree2_ramp;
      break;
    case ED_PTREE2_XRAMP:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2_xramp;
      sf->runf = next_sample_to_float_with_ptree2_xramp;
      sf->rev_run = previous_sample_with_ptree2_xramp;
      sf->rev_runf = previous_sample_to_float_with_ptree2_xramp;
      break;
    case ED_PTREE_XRAMP_PTREE:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_xramp_ptree;
      sf->runf = next_sample_to_float_with_ptree_xramp_ptree;
      sf->rev_run = previous_sample_with_ptree_xramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ptree_xramp_ptree;
      break;
    case ED_PTREE_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_xramp_ptree_air;
      sf->runf = next_sample_to_float_with_ptree_xramp_ptree_air;
      sf->rev_run = previous_sample_with_ptree_xramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ptree_xramp_ptree_air;
      break;
    case ED_PTREE2_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2_ramp2;
      sf->runf = next_sample_to_float_with_ptree2_ramp2;
      sf->rev_run = previous_sample_with_ptree2_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ptree2_ramp2;
      break;
    case ED_PTREE2_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree2_ramp3;
      sf->runf = next_sample_to_float_with_ptree2_ramp3;
      sf->rev_run = previous_sample_with_ptree2_ramp3;
      sf->rev_runf = previous_sample_to_float_with_ptree2_ramp3;
      break;

    case ED_RAMP_PTREE2:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp_ptree2;
      sf->runf = next_sample_to_float_with_ramp_ptree2;
      sf->rev_run = previous_sample_with_ramp_ptree2;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree2;
      break;
    case ED_RAMP_PTREE2_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp_ptree2_zero;
      sf->runf = next_sample_to_float_with_ramp_ptree2_zero;
      sf->rev_run = previous_sample_with_ramp_ptree2_zero;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree2_zero;
      break;

    case ED_RAMP2_PTREE2:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp2_ptree2;
      sf->runf = next_sample_to_float_with_ramp2_ptree2;
      sf->rev_run = previous_sample_with_ramp2_ptree2;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree2;
      break;
    case ED_RAMP2_PTREE2_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp2_ptree2_zero;
      sf->runf = next_sample_to_float_with_ramp2_ptree2_zero;
      sf->rev_run = previous_sample_with_ramp2_ptree2_zero;
      sf->rev_runf = previous_sample_to_float_with_ramp2_ptree2_zero;
      break;

    case ED_RAMP3_PTREE2:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp3_ptree2;
      sf->runf = next_sample_to_float_with_ramp3_ptree2;
      sf->rev_run = previous_sample_with_ramp3_ptree2;
      sf->rev_runf = previous_sample_to_float_with_ramp3_ptree2;
      break;
    case ED_RAMP3_PTREE2_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp3_ptree2_zero;
      sf->runf = next_sample_to_float_with_ramp3_ptree2_zero;
      sf->rev_run = previous_sample_with_ramp3_ptree2_zero;
      sf->rev_runf = previous_sample_to_float_with_ramp3_ptree2_zero;
      break;

    case ED_XRAMP_PTREE2:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_xramp_ptree2;
      sf->runf = next_sample_to_float_with_xramp_ptree2;
      sf->rev_run = previous_sample_with_xramp_ptree2;
      sf->rev_runf = previous_sample_to_float_with_xramp_ptree2;
      break;
    case ED_XRAMP_PTREE2_ZERO:
      setup_ramp(sf, READER_RAMP_BEG(sf), READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_xramp_ptree2_zero;
      sf->runf = next_sample_to_float_with_xramp_ptree2_zero;
      sf->rev_run = previous_sample_with_xramp_ptree2_zero;
      sf->rev_runf = previous_sample_to_float_with_xramp_ptree2_zero;
      break;

    case ED_PTREE_RAMP_PTREE:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp_ptree;
      sf->runf = next_sample_to_float_with_ptree_ramp_ptree;
      sf->rev_run = previous_sample_with_ptree_ramp_ptree;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_ptree;
      break;
    case ED_PTREE_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp_ptree_air;
      sf->runf = next_sample_to_float_with_ptree_ramp_ptree_air;
      sf->rev_run = previous_sample_with_ptree_ramp_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_ptree_air;
      break;
    case ED_PTREE_RAMP2_PTREE:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp2_ptree;
      sf->runf = next_sample_to_float_with_ptree_ramp2_ptree;
      sf->rev_run = previous_sample_with_ptree_ramp2_ptree;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp2_ptree;
      break;
    case ED_PTREE_RAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp2_ptree_air;
      sf->runf = next_sample_to_float_with_ptree_ramp2_ptree_air;
      sf->rev_run = previous_sample_with_ptree_ramp2_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp2_ptree_air;
      break;
    case ED_PTREE_RAMP3_PTREE:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp3_ptree;
      sf->runf = next_sample_to_float_with_ptree_ramp3_ptree;
      sf->rev_run = previous_sample_with_ptree_ramp3_ptree;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp3_ptree;
      break;
    case ED_PTREE_RAMP3_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp3_ptree_air;
      sf->runf = next_sample_to_float_with_ptree_ramp3_ptree_air;
      sf->rev_run = previous_sample_with_ptree_ramp3_ptree_air;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp3_ptree_air;
      break;
    case ED_PTREE_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp_ptree_ramp;
      sf->runf = next_sample_to_float_with_ptree_ramp_ptree_ramp;
      sf->rev_run = previous_sample_with_ptree_ramp_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_ptree_ramp;
      break;
    case ED_PTREE_RAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_PTREE2_SCALER(sf) * READER_RAMP3_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp_ptree_ramp2;
      sf->runf = next_sample_to_float_with_ptree_ramp_ptree_ramp2;
      sf->rev_run = previous_sample_with_ptree_ramp_ptree_ramp2;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp_ptree_ramp2;
      break;
    case ED_PTREE_RAMP2_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ptree_ramp2_ptree_ramp;
      sf->runf = next_sample_to_float_with_ptree_ramp2_ptree_ramp;
      sf->rev_run = previous_sample_with_ptree_ramp2_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ptree_ramp2_ptree_ramp;
      break;
    case ED_RAMP_PTREE_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      sf->ptree = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      sf->ptree1 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
      get_sf_closure(sf);
      get_sf_closure1(sf);
      sf->run = next_sample_with_ramp_ptree_ramp_ptree_ramp;
      sf->runf = next_sample_to_float_with_ramp_ptree_ramp_ptree_ramp;
      sf->rev_run = previous_sample_with_ramp_ptree_ramp_ptree_ramp;
      sf->rev_runf = previous_sample_to_float_with_ramp_ptree_ramp_ptree_ramp;
      break;


    default:
      if (READER_SCALER(sf) == 1.0)
	{
	  sf->run = next_sample_unscaled;
	  sf->rev_run = previous_sample_unscaled;
	  if (sf->fscaler == 1.0) 
	    {
	      sf->runf = next_sample_to_float_unscaled;
	      sf->rev_runf = previous_sample_to_float_unscaled;
	    }
	  else 
	    {
	      sf->runf = next_sample_to_float;
	      sf->rev_runf = previous_sample_to_float;
	    }
	}
      else
	{
#if (!SNDLIB_USE_FLOATS)
	  Float scl;
	  /* more concerned here with the int->float->int conversions than the multiply */
	  scl = READER_SCALER(sf);
	  if (scl == floor(scl))
	    {
	      sf->iscaler = (int)floor(scl);
	      sf->run = next_sample_by_int;
	      sf->rev_run = previous_sample_by_int;
	    }
	  else
	    {
	      sf->run = next_sample;
	      sf->rev_run = previous_sample;
	    }
	  sf->runf = next_sample_to_float;
	  sf->rev_runf = previous_sample_to_float;
#else
	  sf->run = next_sample;
	  sf->runf = next_sample_to_float;
	  sf->rev_run = previous_sample;
	  sf->rev_runf = previous_sample_to_float;
#endif
	}
      break;
    }
  if (sf->direction == READ_BACKWARD) swap_readers(sf);
}


static snd_fd *init_sample_read_any_with_bufsize(off_t samp, chan_info *cp, int direction, int edit_position, int bufsize)
{
  snd_fd *sf;
  snd_info *sp;
  ed_list *ed;
  int len, i;
  off_t ind0, ind1, indx, curlen;
  ed_fragment *cb;
  snd_data *first_snd = NULL;
  if (!(cp->active)) return(NULL);
  if ((edit_position < 0) || (edit_position > cp->edit_size)) return(NULL);
  ed = (ed_list *)(cp->edits[edit_position]);
  if (!ed) return(NULL);
  sp = cp->sound;
  if (sp->need_update) 
    {
      if (mus_file_probe(sp->filename) == 0)
	snd_error(_("file %s no longer exists!"), sp->short_filename);
      else snd_warning(_("file %s has changed since we last read it!"), sp->short_filename);
    }
  curlen = cp->samples[edit_position];
  /* snd_fd allocated only here */
  sf = (snd_fd *)CALLOC(1, sizeof(snd_fd));
  sf->initial_samp = samp;
  sf->cp = cp;
  sf->fscaler = MUS_FIX_TO_FLOAT;
  sf->rscaler = MUS_FLOAT_TO_FIX;
  sf->direction = direction;
  sf->current_state = ed;
  sf->edit_ctr = edit_position;
  sf->dangling_loc = -1;
  if ((curlen <= 0) ||    /* no samples, not ed->len (delete->len = #deleted samps) */
      (samp < 0) ||       /* this should never happen */
      ((samp >= curlen) && (direction == READ_FORWARD)))
    return(cancel_reader(sf));
  if (samp >= curlen) samp = curlen - 1;
  len = ed->size;
  for (i = 0; i < len; i++)
    {
      cb = FRAGMENT(ed, i);
      if ((ED_GLOBAL_POSITION(cb) > samp) || 
	  (ED_SOUND(cb) == EDIT_LIST_END_MARK))             /* i.e. we went one too far */
	{
	  sf->cb = FRAGMENT(ed, i - 1);  /* so back up one */
	  sf->cbi = i - 1;
	  sf->frag_pos = samp - READER_GLOBAL_POSITION(sf);
	  ind0 = READER_LOCAL_POSITION(sf);
	  indx = READER_LOCAL_POSITION(sf) + sf->frag_pos;
	  ind1 = READER_LOCAL_END(sf);
	  sf->fscaler = MUS_FIX_TO_FLOAT * READER_SCALER(sf);
	  sf->rscaler = MUS_FLOAT_TO_FIX * READER_SCALER(sf);
	  if (ZERO_OP(READER_TYPE(sf)))
	    {
	      sf->current_sound = NULL;
	      sf->loc = indx;
	      sf->first = ind0;
	      sf->last = ind1;
	      sf->data = NULL;
	      choose_accessor(sf);
	      return(sf);
	    }
	  first_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (first_snd->type == SND_DATA_FILE)
	    {
	      /* since arbitrarily many work procs can be running in parallel, reading the same 
	       * data (edit tree sound file entries), we can't share the clm-style IO buffers since these contain
	       * a local notion of current position which is not accessed on every sample by the
	       * sample readers (they trust their snd_fd indices); we wouldn't want to be
	       * constantly jumping around and re-reading data buffers (in the worst case
	       * many times per sample) anyway, so we copy the IO buffer, allocate a relatively
	       * small data buffer, and then free all the copied snd_data stuff as soon as
	       * the current reader is done.
	       */
	      if (first_snd->inuse)
		{
		  first_snd = copy_snd_data(first_snd, cp, bufsize);
		  if (first_snd == NULL)
		    return(cancel_reader(sf));
		}
	      first_snd->inuse = true;
	      sf->current_sound = first_snd;
	      sf->data = first_snd->buffered_data;
	      if (direction == READ_FORWARD)
		file_buffers_forward(ind0, ind1, indx, sf, first_snd);
	      else file_buffers_back(ind0, ind1, indx, sf, first_snd);
	    }
	  else 
	    {
	      sf->current_sound = NULL;
	      sf->data = first_snd->buffered_data;
	      sf->first = ind0;
	      sf->last = ind1;
	      sf->loc = indx;
	    }
	  choose_accessor(sf);
	  return(sf);
	}
    }
  if (sf) FREE(sf);
  return(NULL);
}

snd_fd *init_sample_read_any(off_t samp, chan_info *cp, int direction, int edit_position)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, edit_position, MIX_FILE_BUFFER_SIZE));
}

snd_fd *init_sample_read(off_t samp, chan_info *cp, int direction)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, cp->edit_ctr, MIX_FILE_BUFFER_SIZE));
}

Float chn_sample(off_t samp, chan_info *cp, int pos)
{ 
  snd_data *sd;
  snd_fd *sf;
  Float val = 0.0;
  if ((!(cp->active)) || (samp < 0) || (pos < 0) || (pos > cp->edit_size) || (samp >= cp->samples[pos])) return(0.0);
  /* try the quick case */
  if (pos == 0)
    {
      sd = cp->sounds[0];
      if ((sd) && (sd->io) && (sd->io->beg <= samp) && (sd->io->end >= samp))
	return(MUS_SAMPLE_TO_FLOAT(sd->buffered_data[samp - sd->io->beg]));
    }
  /* do it the hard way */
  sf = init_sample_read_any_with_bufsize(samp, cp, READ_FORWARD, pos, 2);
  if (sf)
    {
      val = read_sample_to_float(sf);
      free_snd_fd(sf);
    }
  return(val);
}

static void previous_sound_1 (snd_fd *sf) 
{
  off_t ind0, ind1, indx;
  snd_data *prev_snd;
  int at_start;
  at_start = ((sf->cb == NULL) || (sf->current_sound == NULL) || (READER_LOCAL_POSITION(sf) >= sf->current_sound->io->beg));
  if (at_start)
    {
      if (sf->current_sound) 
	{
	  prev_snd = sf->current_sound; 
	  prev_snd->inuse = false; 
	  sf->current_sound = NULL;
	  if (prev_snd->copy) prev_snd = free_snd_data(prev_snd);
	}
      if (sf->cbi == 0) 
	{
	  reader_out_of_data(sf);
	  return;
	}
      sf->cbi--;
      /* now start in the final portion of this block (if a file) */
      sf->cb = FRAGMENT((sf->current_state), sf->cbi);
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      sf->fscaler = MUS_FIX_TO_FLOAT * READER_SCALER(sf);
      sf->rscaler = MUS_FLOAT_TO_FIX * READER_SCALER(sf);
      if (ZERO_OP(READER_TYPE(sf)))
	{
	  sf->current_sound = NULL;
	  sf->loc = ind1;
	  sf->first = ind0;
	  sf->last = ind1;
	  sf->data = NULL;
	}
      else
	{
	  prev_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (prev_snd->type == SND_DATA_FILE)
	    {
	      if (prev_snd->inuse) 
		prev_snd = copy_snd_data(prev_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	      sf->data = prev_snd->buffered_data;
	      prev_snd->inuse = true;
	      sf->current_sound = prev_snd;
	      file_buffers_back(ind0, ind1, ind1, sf, prev_snd);
	    }
	  else 
	    {
	      sf->data = prev_snd->buffered_data;
	      sf->loc = ind1;
	      sf->first = ind0;
	      sf->last = ind1;
	    }
	}
      sf->frag_pos = ind1 - ind0;
      choose_accessor(sf);
    }
  else
    {
      /* back up in current file */
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      indx = sf->current_sound->io->beg - 1;
      file_buffers_back(ind0, ind1, indx, sf, sf->current_sound);
    }
}

static mus_sample_t previous_sound(snd_fd *sf)
{
  previous_sound_1(sf);
  return(read_sample(sf));
}

static Float previous_sound_as_float(snd_fd *sf)
{
  previous_sound_1(sf);
  return(read_sample_to_float(sf));
}

static void next_sound_1(snd_fd *sf)
{
  off_t ind0, ind1, indx;
  snd_data *nxt_snd;
  bool at_end = false;
  at_end = ((sf->cb == NULL) || 
	    (sf->current_sound == NULL) || 
	    (READER_LOCAL_END(sf) <= sf->current_sound->io->end));
  if (at_end)
    {
      if (sf->current_sound) 
	{
	  nxt_snd = sf->current_sound; 
	  nxt_snd->inuse = false; 
	  sf->current_sound = NULL;
	  if (nxt_snd->copy) nxt_snd = free_snd_data(nxt_snd);
	}
      sf->cbi++;
      if (sf->cbi >= (sf->current_state)->size) 
	{
	  reader_out_of_data(sf);
	  return;
	}
      sf->cb = FRAGMENT((sf->current_state), sf->cbi);
      if ((!(sf->cb)) || 
	  (READER_SOUND(sf) == EDIT_LIST_END_MARK))
	{
	  reader_out_of_data(sf);
	  return;
	}
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      sf->fscaler = MUS_FIX_TO_FLOAT * READER_SCALER(sf);
      sf->rscaler = MUS_FLOAT_TO_FIX * READER_SCALER(sf);
      if (ZERO_OP(READER_TYPE(sf)))
	{
	  sf->current_sound = NULL;
	  sf->loc = ind0;
	  sf->first = ind0;
	  sf->last = ind1;
	  sf->data = NULL;
	}
      else
	{
	  nxt_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (nxt_snd->type == SND_DATA_FILE)
	    {
	      if (nxt_snd->inuse)
		nxt_snd = copy_snd_data(nxt_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	      sf->data = nxt_snd->buffered_data;
	      nxt_snd->inuse = true;
	      sf->current_sound = nxt_snd;
	      file_buffers_forward(ind0, ind1, ind0, sf, nxt_snd);
	    }
	  else 
	    {
	      sf->data = nxt_snd->buffered_data;
	      sf->loc = ind0;
	      sf->first = ind0;
	      sf->last = ind1;
	    }
	}
      sf->frag_pos = 0;
      choose_accessor(sf);
    }
  else
    { 
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      indx = sf->current_sound->io->end + 1;
      file_buffers_forward(ind0, ind1, indx, sf, sf->current_sound);
    }
}

static mus_sample_t next_sound(snd_fd *sf)
{
  next_sound_1(sf);
  return(read_sample(sf));
}

static Float next_sound_as_float(snd_fd *sf)
{
  next_sound_1(sf);
  return(read_sample_to_float(sf));
}


void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1)
{
  int i, fd, new0, new1;
  char *name;
  ed_list *new_ed, *old_ed;
  file_info *hdr0, *hdr1;
  snd_state *ss;
  env_info *e0, *e1;
  ss = cp0->state;
  /*
  fprintf(stderr,"copy then swap %s[%d]:%d of %d and %s[%d]:%d of %d\n",
	  cp0->sound->short_filename, cp0->chan, pos0, cp0->edit_ctr,
	  cp1->sound->short_filename, cp1->chan, pos1, cp1->edit_ctr);
  */
  name = cp0->sound->filename;
  hdr0 = copy_header(name, cp0->sound->hdr);
  fd = snd_open_read(ss, name);
  mus_file_open_descriptors(fd,
			    name,
			    hdr0->format,
			    mus_data_format_to_bytes_per_sample(hdr0->format),
			    hdr0->data_location,
			    hdr0->chans,
			    hdr0->type);
  new0 = add_sound_file_to_edit_list(cp1, name,
				     make_file_state(fd, hdr0, cp0->chan, FILE_BUFFER_SIZE),
				     hdr0, DONT_DELETE_ME, cp0->chan);
  name = cp1->sound->filename;
  hdr1 = copy_header(name, cp1->sound->hdr);
  fd = snd_open_read(ss, name);
  mus_file_open_descriptors(fd,
			    name,
			    hdr1->format,
			    mus_data_format_to_bytes_per_sample(hdr1->format),
			    hdr1->data_location,
			    hdr1->chans,
			    hdr1->type);
  new1 = add_sound_file_to_edit_list(cp0, name,
				     make_file_state(fd, hdr1, cp1->chan, FILE_BUFFER_SIZE),
				     hdr1, DONT_DELETE_ME, cp1->chan);
  e0 = amp_env_copy(cp0, false, pos0);
  e1 = amp_env_copy(cp1, false, pos1);
  old_ed = cp1->edits[pos1];
  prepare_edit_list(cp0, cp1->samples[pos1], AT_CURRENT_EDIT_POSITION, S_swap_channels);
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new1;
  new_ed->edpos = pos1;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->beg = 0;
  new_ed->len = old_ed->len;
  new_ed->origin = copy_string(TO_PROC_NAME(S_swap_channels));
  cp0->edits[cp0->edit_ctr] = new_ed;
  if (new_ed->len > 0)
    for (i = 0; i < new_ed->size; i++) 
      {
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	if (FRAGMENT_SOUND(new_ed, i) == 0) FRAGMENT_SOUND(new_ed, i) = new1;
      }
  old_ed = cp0->edits[pos0];
  prepare_edit_list(cp1, cp0->samples[pos0], AT_CURRENT_EDIT_POSITION, S_swap_channels);
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new0;
  new_ed->edpos = pos0;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->beg = 0;
  new_ed->len = old_ed->len;
  new_ed->origin = copy_string(TO_PROC_NAME(S_swap_channels));
  cp1->edits[cp1->edit_ctr] = new_ed;
  if (new_ed->len > 0)
    for (i = 0; i < new_ed->size; i++) 
      {
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	if (FRAGMENT_SOUND(new_ed, i) == 0) FRAGMENT_SOUND(new_ed, i) = new0;
      }
  if ((e0) && (e1))
    {
      cp0->amp_envs[cp0->edit_ctr] = e1;
      cp1->amp_envs[cp1->edit_ctr] = e0;
    }
  ripple_all(cp0, 0, 0);
  ripple_all(cp1, 0, 0);
  swap_marks(cp0, cp1);
  reflect_edit_history_change(cp0);
  reflect_edit_history_change(cp1);
  if (cp0->samples[cp0->edit_ctr] != cp0->samples[cp0->edit_ctr - 1])
    reflect_sample_change_in_axis(cp0);
  if (cp1->samples[cp1->edit_ctr] != cp1->samples[cp1->edit_ctr - 1])
    reflect_sample_change_in_axis(cp1);
  update_graph(cp0);
  update_graph(cp1);
}

static int snd_make_file(char *ofile, int chans, file_info *hdr, snd_fd **sfs, off_t length, snd_state *ss)
{
  /* create ofile, fill it by following sfs, use hdr for srate/type/format decisions */
  /* used only in this file and snd-chn (for external temps, snd->temp) */
  int ofd;
  int i, j, datumb, reporting = 0, err = 0;
  off_t len, total = 0;
  chan_info *cp = NULL;
  mus_sample_t **obufs;
  err = MUS_NO_ERROR;
  ofd = open_temp_file(ofile, chans, hdr, ss);
  mus_file_set_data_clipped(ofd, data_clipped(ss));
  if (ofd == -1) return(MUS_CANT_OPEN_TEMP_FILE);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  obufs = (mus_sample_t **)MALLOC(chans * sizeof(mus_sample_t *));
  ss->stopped_explicitly = false;
  for (i = 0; i < chans; i++)
    obufs[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t));
  j = 0;
  reporting = (length > REPORTING_SIZE);
  if (reporting) 
    {
      cp = sfs[0]->cp;
      start_progress_report(cp->sound, NOT_FROM_ENVED);
    }
  if (chans == 1)
    {
      if (length > FILE_BUFFER_SIZE)
	{
	  for (len = 0; len < length; len++)
	    {
	      obufs[0][j] = read_sample(sfs[0]);
	      j++;
	      if (j == FILE_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, j - 1, 1, obufs);
		  j = 0;
		  if (err == -1) break;
		  if (reporting)
		    {
		      total += FILE_BUFFER_SIZE;
		      progress_report(cp->sound, NULL, 1, 1, (Float)((double)total / (double)length), NOT_FROM_ENVED);
		    }
		  check_for_event(ss);
		  if (ss->stopped_explicitly)
		    {
		      ss->stopped_explicitly = false;
		      snd_warning(_("file save cancelled by C-g"));
		      err = MUS_INTERRUPTED;
		      break;
		    }
		}
	    }
	}
      else
	{
	  for (len = 0; len < length; len++)
	    obufs[0][len] = read_sample(sfs[0]);
	  j = (int)length;
	}
    }
  else
    {
      for (len = 0; len < length; len++)
	{
	  for (i = 0; i < chans; i++)
	    obufs[i][j] = read_sample(sfs[i]);
	  j++;
	  if (j == FILE_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      if (err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(cp->sound, NULL, 1, 1, (Float)((double)total / (double)length), NOT_FROM_ENVED);
		}
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = false;
		  snd_warning(_("file save cancelled by C-g"));
		  err = MUS_INTERRUPTED;
		  break;
		}
	    }
	}
    }
  if ((err == MUS_NO_ERROR) && (j > 0))
    mus_file_write(ofd, 0, j - 1, chans, obufs);
  if (err == MUS_NO_ERROR)
    {
      err = close_temp_file(ofd, hdr, len * chans * datumb, any_selected_sound(ss));
      alert_new_file();
    }
  else err = mus_file_close(ofd);
  if (reporting) finish_progress_report(cp->sound, NOT_FROM_ENVED);
  for (i = 0; i < chans; i++) FREE(obufs[i]);
  FREE(obufs);
  return(err);
}

static int save_edits_and_update_display(snd_info *sp)
{
  /* open temp, write current state, rename to old, reopen and clear all state */
  /* can't overwrite current because we may have cut/paste backpointers scattered around the current edit list */
  /* have to decide here what header/data type to write as well -- original? */
  /* if latter, must be able to write all headers! -- perhaps warn user and use snd/aiff/riff/ircam */

  /* sp->read_only already checked */

  char *ofile = NULL;
  int err = MUS_NO_ERROR, saved_errno = 0;
  snd_state *ss;
  int i;
  off_t samples = 0;
  off_t *old_cursors = NULL;
  chan_info *cp;
  snd_fd **sf;
  void *sa;
  file_info *sphdr = NULL;
  ss = sp->state;
  if (dont_save(sp, NULL)) return(MUS_NO_ERROR);
  err = MUS_NO_ERROR;
  ofile = snd_tempnam(ss); 
  /* this will use user's TMPDIR if temp_dir(ss) is not set, else stdio.h's P_tmpdir else /tmp */
  sa = make_axes_data(sp);
  sf = (snd_fd **)CALLOC(sp->nchans, sizeof(snd_fd *));
  for (i = 0; i < sp->nchans; i++)
    {
      sf[i] = init_sample_read(0, sp->chans[i], READ_FORWARD);
      if (sf[i] == NULL) err = MUS_ERROR;
      if (samples < CURRENT_SAMPLES(sp->chans[i]))
	samples = CURRENT_SAMPLES(sp->chans[i]);
    }
  if (err == MUS_NO_ERROR)
    {
      report_in_minibuffer(sp, _("saving %s"), sp->short_filename);
      sphdr = sp->hdr;
      err = snd_make_file(ofile, sp->nchans, sp->hdr, sf, samples, ss);
    }
  if (err != MUS_NO_ERROR) 
    {
      for (i = 0; i < sp->nchans; i++) free_snd_fd(sf[i]);
      FREE(sf);
      sa = free_axes_data(sa);
      return(err);
    }
  sphdr->samples = samples * sp->nchans;
  collapse_marks(sp);
  old_cursors = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      old_cursors[i] = CURSOR(cp); /* depends on edit_ctr -- set to -1 by free_edit_list below */
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, -1);
      if (cp->mixes) reset_mix_list(cp);
      if (cp->edits) free_edit_list(cp);
      free_snd_fd(sf[i]);  /* must precede free_sound_list since it accesses the snd_data structs that free_sound_list frees */
      if (cp->sounds) free_sound_list(cp);
      if (cp->samples) 
	{
	  FREE(cp->samples); 
	  cp->samples = NULL;
	}
      if (cp->cursors) 
	{
	  FREE(cp->cursors); 
	  cp->cursors = NULL;
	}
      cp->axis = free_axis_info(cp->axis);
    }
  FREE(sf);

#if (!HAVE_ACCESS)
  err = 0;
#else
  err = access(sp->filename, W_OK);
#endif
  /* very weird -- in Linux we can write a write-protected file?? */
  if (err == 0)
    {
      mus_sound_forget(sp->filename);
      err = move_file(ofile, sp->filename);
      if (err) saved_errno = errno;
    }
  else saved_errno = errno;
  sp->write_date = file_write_date(sp->filename);
  add_sound_data(sp->filename, sp, WITHOUT_INITIAL_GRAPH_HOOK);
  restore_axes_data(sp, sa, mus_sound_duration(sp->filename), true);
  sa = free_axes_data(sa);
  for (i = 0; i < sp->nchans; i++)
    CURSOR(sp->chans[i]) = old_cursors[i];
  FREE(old_cursors);
  reflect_file_revert_in_label(sp);
  reflect_file_save_in_menu(ss);
  if (err)
    report_in_minibuffer_and_save(sp, _("file write failed: %s, edits are saved in: %s"), strerror(saved_errno), ofile);
  else report_in_minibuffer(sp, _("wrote %s"), sp->filename); 
  if (ofile) 
    {
      FREE(ofile); 
      ofile = NULL;
    }
  if (auto_update(ss)) 
    for_each_sound(ss, sound_not_current, NULL);
  return(MUS_NO_ERROR); /* don't erase our error message for the special write-permission problem */
}

int save_edits_without_display(snd_info *sp, char *new_name, int type, int format, int srate, char *comment, XEN edpos, const char *caller, int arg_pos)
{ 
  /* file save as menu option -- changed 19-June-97 to retain current state after writing */
  file_info *hdr, *ohdr;
  snd_state *ss;
  int i, err = MUS_NO_ERROR, pos;
  off_t frames = 0;
  snd_fd **sf;
  chan_info *cp;
  ss = sp->state;
  if ((sp->read_only) && (strcmp(new_name, sp->filename) == 0))
    {
      snd_error(_("%s is write-protected"), sp->filename);
      return(MUS_ERROR);
    }
  if (dont_save(sp, new_name)) return(MUS_NO_ERROR);
  if (MUS_DATA_FORMAT_OK(format))
    {
      if (MUS_HEADER_TYPE_OK(type))
	{
	  ohdr = sp->hdr;
	  hdr = copy_header(new_name, ohdr);
	  hdr->format = format;
	  hdr->srate = srate;
	  hdr->type = type;
	  if (comment) 
	    hdr->comment = copy_string(comment); 
	  else hdr->comment = NULL;
	  hdr->data_location = 0; /* in case comment changes it */
	  sf = (snd_fd **)MALLOC(sp->nchans * sizeof(snd_fd *));
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      cp = sp->chans[i];
	      pos = to_c_edit_position(cp, edpos, caller, arg_pos);
	      sf[i] = init_sample_read_any(0, cp, READ_FORWARD, pos);
	      if (frames < cp->samples[pos]) frames = cp->samples[pos];
	      if (sf[i] == NULL) err = MUS_ERROR;
	    }
	  if (err == MUS_NO_ERROR)
	    err = snd_make_file(new_name, sp->nchans, hdr, sf, frames, ss);
	  for (i = 0; i < sp->nchans; i++) 
	    free_snd_fd(sf[i]);
	  FREE(sf);
	  free_file_info(hdr);
	  return(err);
	}
      else 
	{
	  if (strcmp(caller, "file save as") == 0)
	    snd_error(_("save-edits: unknown header type? %d"), type);
	  else ss->catch_message = "unknown header type";
	  return(MUS_UNSUPPORTED_HEADER_TYPE);
	}
    }
  if (strcmp(caller, "file save as") == 0)
    snd_error(_("save-edits: unknown data format? %d"), format);
  else ss->catch_message = "unknown data format";
  return(MUS_UNSUPPORTED_DATA_FORMAT);
}

int save_channel_edits(chan_info *cp, char *ofile, XEN edpos, const char *caller, int arg_pos)
{
  /* channel extraction -- does not (normally) cause reversion of edits, or change of in-window file, etc */
  snd_info *sp;
  snd_fd **sf;
  int err, pos;
  char *nfile;
  snd_state *ss;
  ss = cp->state;
  sp = cp->sound;
  err = MUS_NO_ERROR;
  if (!(snd_overwrite_ok(ss, ofile))) return(MUS_NO_ERROR); /* no error because decision was explicit */
  pos = to_c_edit_position(cp, edpos, caller, arg_pos);
  if (strcmp(ofile, sp->filename) == 0)
    {
      if (sp->read_only)
	{
	  report_in_minibuffer_and_save(sp, _("can't save (extract) channel as %s (%s is write-protected)"), ofile, sp->short_filename);
	  return(MUS_WRITE_ERROR);
	}
      /* here we're overwriting the current (possibly multi-channel) file with one of its channels */
      nfile = snd_tempnam(ss); 
      sf = (snd_fd **)MALLOC(sizeof(snd_fd *));
      sf[0] = init_sample_read_any(0, cp, READ_FORWARD, pos); 
      if (sf[0] == NULL)
	err = MUS_ERROR;
      else
	{
	  err = snd_make_file(nfile, 1, sp->hdr, sf, cp->samples[pos], ss);
	  free_snd_fd(sf[0]);
	}
      FREE(sf);
      if (err != MUS_NO_ERROR)
	report_in_minibuffer_and_save(sp, _("save channel as %s hit error: %s"), nfile, strerror(errno));
      else 
	{
	  err = move_file(nfile, ofile);
	  if (err == 0) snd_update(ss, sp);
	}
      FREE(nfile);
    }
  else
    {
      sf = (snd_fd **)MALLOC(sizeof(snd_fd *));
      sf[0] = init_sample_read_any(0, cp, READ_FORWARD, pos);
      if (sf[0] == NULL)
	err = MUS_ERROR;
      else
	{
	  err = snd_make_file(ofile, 1, sp->hdr, sf, cp->samples[pos], ss);
	  free_snd_fd(sf[0]);
	}
      FREE(sf);
    }
  return(err);
}

void save_edits(snd_info *sp, void *ptr)
{
  int i, err;
  bool need_save;
  time_t current_write_date;
  chan_info *cp;
  if (sp == NULL) return;
  if (!sp->read_only)
    {
      need_save = false;
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  if (cp->edit_ctr > 0) 
	    {
	      need_save = true;
	      break;
	    }
	}
      if (need_save)
	{
	  errno = 0;
	  /* check for change to file while we were editing it */
	  current_write_date = file_write_date(sp->filename);
	  if ((current_write_date - sp->write_date) > 1) /* weird!! In Redhat 7.1 these can differ by 1?? Surely this is a bug! */
	    {
	      err = snd_yes_or_no_p(sp->state, _("%s changed on disk! Save anyway?"), sp->short_filename);
	      if (err == 0) return;
	    }
	  err = save_edits_and_update_display(sp);
	  if (err)
	    report_in_minibuffer_and_save(sp, "%s: %s", sp->filename, strerror(errno));
	  else
	    {
	      if (sp->edited_region) 
		save_region_backpointer(sp);
	    }
	}
      else
	report_in_minibuffer(sp, _("(no changes need to be saved)"));
    }
  else
    report_in_minibuffer_and_save(sp, _("can't write %s (it is read-only)"), sp->short_filename);
}

void revert_edits(chan_info *cp, void *ptr)
{
  if (cp->edit_ctr == 0) return;
  cp->edit_ctr = 0;
  clear_transform_edit_ctrs(cp);
  reflect_edit_counter_change(cp);
  reflect_sample_change_in_axis(cp);
  if (selection_is_active())
    reflect_edit_with_selection_in_menu(); 
  else reflect_edit_without_selection_in_menu();
  update_graph(cp);
  reflect_mix_in_menu();
  reflect_mix_in_enved();
  if (XEN_HOOKED(cp->undo_hook))
    run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
}

void undo_edit(chan_info *cp, int count)
{
  snd_info *sp;
  if ((cp) && (cp->edit_ctr > 0) && (count != 0))
    {
      sp = cp->sound;
      cp->edit_ctr -= count; 
      if (cp->edit_ctr < 0) cp->edit_ctr = 0;
      clear_transform_edit_ctrs(cp);
      reflect_edit_counter_change(cp);
      reflect_sample_change_in_axis(cp);
      reflect_undo_in_menu();
      if (cp->edit_ctr == 0)
	{
	  reflect_file_revert_in_label(sp);
	  reflect_file_revert_in_menu(cp->state);
	}
      if (selection_is_active()) 
	reflect_edit_with_selection_in_menu();
      else reflect_edit_without_selection_in_menu();
      update_graph(cp);
      reflect_mix_in_menu();
      reflect_mix_in_enved();
      if (XEN_HOOKED(cp->undo_hook))
	run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
    }
}

void undo_edit_with_sync(chan_info *cp, int count)
{
  /* there is a problem with syncd undo: if the syncd edit decided one portion
   *   was a no-op (scale by 1.0 etc), but not another, a subsequent undo with
   *   sync can end up in a state different from where it started.
   */
  snd_info *sp;
  int i;
  sync_info *si;
  if (count == 0) return;
  if (count < 0)
    redo_edit_with_sync(cp, -count);
  else
    {
      si = NULL;
      if (cp)
	{
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(cp->state, sp->sync);
	  if (si)
	    {
	      for (i = 0; i < si->chans; i++) undo_edit(si->cps[i], count);
	      si = free_sync_info(si);
	    }
	  else undo_edit(cp, count);
	}
    }
}

void redo_edit(chan_info *cp, int count)
{
  if ((cp) && (count != 0))
    {
      cp->edit_ctr += count; 
      while ((cp->edit_ctr >= cp->edit_size) || 
	     (!(cp->edits[cp->edit_ctr]))) 
	cp->edit_ctr--;
      if (((cp->edit_ctr + 1) == cp->edit_size) || 
	  (!(cp->edits[cp->edit_ctr + 1]))) 
	reflect_no_more_redo_in_menu();
      if (cp->edit_ctr != 0) /* possibly a sync'd redo to chan that has no edits */
	{
	  clear_transform_edit_ctrs(cp);
	  reflect_file_change_in_label(cp);
	  reflect_redo_in_menu();
	  reflect_edit_counter_change(cp);
	  reflect_sample_change_in_axis(cp);
	  if (selection_is_active()) 
	    reflect_edit_with_selection_in_menu(); 
	  else reflect_edit_without_selection_in_menu();
	  update_graph(cp);
	  reflect_mix_in_menu();
	  reflect_mix_in_enved();
	}
      if (XEN_HOOKED(cp->undo_hook))
	run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
    }
}

void redo_edit_with_sync(chan_info *cp, int count)
{
  snd_info *sp;
  int i;
  sync_info *si;
  if (count == 0) return;
  if (count < 0)
    undo_edit_with_sync(cp, -count);
  else
    {
      si = NULL;
      if (cp)
	{
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(cp->state, sp->sync);
	  if (si)
	    {
	      for (i = 0; i < si->chans; i++) redo_edit(si->cps[i], count);
	      si = free_sync_info(si);
	    }
	  else redo_edit(cp, count);
	}
    }
}


static XEN g_display_edits(XEN snd, XEN chn, XEN edpos, XEN with_source)
{
  #define H_display_edits "(" S_display_edits " (snd #f) (chn #f) (edpos #f) (with-source #t)): current edit tree"
  FILE *tmp = NULL;
  char *buf, *name;
  chan_info *cp;
  int fd;
  bool include_source = true;
  off_t len;
  snd_state *ss;
  XEN res;
  ASSERT_CHANNEL(S_display_edits, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_source), with_source, XEN_ARG_4, S_display_edits, "boolean");
  cp = get_cp(snd, chn, S_display_edits);
  if (XEN_BOOLEAN_P(with_source)) include_source = XEN_TO_C_BOOLEAN(with_source);
  ss = get_global_state();
  name = snd_tempnam(ss);
  tmp = FOPEN(name, "w");
  if (tmp) 
    {
      if (XEN_INTEGER_P(edpos))
	{
	  int pos;
	  pos = XEN_TO_C_INT(edpos);
	  if ((pos >= 0) && (pos < cp->edit_size) && (cp->edits[pos]))
	    display_ed_list(cp, tmp, pos, cp->edits[pos], include_source);
	  else 
	    {
	      FCLOSE(tmp);
	      XEN_ERROR(NO_SUCH_EDIT,
			XEN_LIST_2(C_TO_XEN_STRING(S_display_edits),
				   edpos));
	    }
	}
      else display_edits(cp, tmp, include_source);
    }
  if ((!tmp) || (FCLOSE(tmp) != 0))
    {
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_display_edits),
			   C_TO_XEN_STRING(name),
			   C_TO_XEN_STRING(strerror(errno))));
    }
  fd = mus_file_open_read(name);
  len = lseek(fd, 0L, SEEK_END);
  buf = (char *)CALLOC(len + 1, sizeof(char));
  lseek(fd, 0L, SEEK_SET);
  read(fd, buf, len);
  snd_close(fd, name);
  snd_remove(name, IGNORE_CACHE);
  if (name) FREE(name);
  res = C_TO_XEN_STRING(buf);
  FREE(buf);
  return(res);
}

static XEN g_edit_fragment(XEN uctr, XEN snd, XEN chn)
{
  #define H_edit_fragment "(" S_edit_fragment " (ctr current-edit-position) (snd #f) (chn #f)): edit history entry at ctr \
associated with snd's channel chn; the returned value is a list (origin type start-sample samps)"

  chan_info *cp;
  ed_list *ed;
  int ctr;
  ASSERT_CHANNEL(S_edit_fragment, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uctr), uctr, XEN_ARG_1, S_edit_fragment, "an integer");
  cp = get_cp(snd, chn, S_edit_fragment);
  ctr = XEN_TO_C_INT_OR_ELSE(uctr, cp->edit_ctr);
  if ((ctr < cp->edit_size) && 
      (ctr >= 0))
    {
      ed = cp->edits[ctr];
      if (ed) 
	return(XEN_LIST_4(C_TO_XEN_STRING(ed->origin),
			  C_TO_XEN_STRING(edit_names[ed->edit_type]),
			  C_TO_XEN_OFF_T(ed->beg),
			  C_TO_XEN_OFF_T(ed->len)));
    }
  XEN_ERROR(NO_SUCH_EDIT,
	    XEN_LIST_4(C_TO_XEN_STRING(S_edit_fragment),
		       uctr, snd, chn));
  return(uctr);
}

static XEN g_edit_tree(XEN snd, XEN chn, XEN upos)
{
  #define H_edit_tree "(" S_edit_tree " (snd #f) (chn #f) (edpos #f)): \
the edit lists '((global-pos data-num local-pos local-end scaler rmp0 rmp1 type)...)"
  /* internal debugging (auto-test) aid -- return complete ed list at pos */
  int i, len, pos;
  chan_info *cp;
  ed_list *ed;
  XEN res = XEN_EMPTY_LIST;
  ASSERT_CHANNEL(S_edit_tree, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_tree);
  pos = to_c_edit_position(cp, upos, S_edit_tree, 3);
  ed = cp->edits[pos];
  len = ed->size; /* fragments in this list */
  for (i = len - 1; i >= 0; i--)
    res = XEN_CONS(XEN_LIST_8(C_TO_XEN_OFF_T(FRAGMENT_GLOBAL_POSITION(ed, i)),
			      C_TO_XEN_INT(FRAGMENT_SOUND(ed, i)),
			      C_TO_XEN_OFF_T(FRAGMENT_LOCAL_POSITION(ed, i)),
			      C_TO_XEN_OFF_T(FRAGMENT_LOCAL_END(ed, i)),
			      C_TO_XEN_DOUBLE(FRAGMENT_SCALER(ed, i)),
			      C_TO_XEN_DOUBLE(FRAGMENT_RAMP_BEG(ed, i)),
			      C_TO_XEN_DOUBLE(FRAGMENT_RAMP_END(ed, i)),
			      C_TO_XEN_INT(FRAGMENT_TYPE(ed, i))),
		   res);
  return(res);
}


/* ---------------- sample readers ---------------- */

static XEN_OBJECT_TYPE sf_tag;
int sf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sf_tag));}
#define SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, sf_tag)

static XEN g_sf_p(XEN obj) 
{
  #define H_sf_p "(" S_sample_reader_p " obj): #t if obj is a sample-reader"
  return(C_TO_XEN_BOOLEAN(SAMPLE_READER_P(obj)));
}

snd_fd *get_sf(XEN obj) {if (SAMPLE_READER_P(obj)) return((snd_fd *)XEN_OBJECT_REF(obj)); else return(NULL);}
#define TO_SAMPLE_READER(obj) ((snd_fd *)XEN_OBJECT_REF(obj))

char *sf_to_string(snd_fd *fd)
{
  char *desc, *name = NULL;
  chan_info *cp;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (fd == NULL)
    sprintf(desc, "#<sample-reader: null>");
  else
    {
      cp = fd->cp;
      if ((fd->local_sp) && (fd->local_sp->hdr))
	name = ((fd->local_sp)->hdr)->name;
      else
	{
	  if ((cp) && (cp->sound) && (cp->active) && (!(fd->at_eof)))
	    {
	      name = (cp->sound)->short_filename;
	      if (name == NULL)
		switch (cp->sound->inuse)
		  {
		  case SOUND_IDLE: name = "idle source";        break;
		  case SOUND_NORMAL: name = "unknown source";   break;
		  case SOUND_WRAPPER: name = "wrapped source";  break;
		  case SOUND_REGION: name = "region as source"; break;
		  case SOUND_READER: name = "reader source";    break;
		  }
	    }
	}
      if (name == NULL) name = "unknown source";
      if (fd->at_eof)
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s at eof or freed>",
		     fd, name);
      else 
	{
	  if (cp)
	    mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s[%d: %d] from " OFF_TD ", at " OFF_TD ">",
			 fd, name, cp->chan, fd->edit_ctr, fd->initial_samp, current_location(fd));
	  else mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s from " OFF_TD ", at " OFF_TD ">",
			    fd, name, fd->initial_samp, current_location(fd));
	}
    }
  return(desc);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(snd_fd, print_sf, sf_to_string)

/* make-sample-reader can refer to any edit of any sound, user can subsequently
 *   either clobber that edit (undo, new edit), or close the sound, but forget
 *   that the reader is now invalid.  So, we keep a list of these and unconnect
 *   them by hand when an edit is pruned or a sound is closed.
 *
 * channel|sound-properties are ok in this regard because the variable stays in
 *   xen and is merely cleared, not freed at the C level.
 */

static snd_fd **dangling_readers = NULL;
static int dangling_reader_size = 0;
#define DANGLING_READER_INCREMENT 16

static void list_reader(snd_fd *fd)
{
  int i, loc = -1;
  if (dangling_reader_size == 0)
    {
      dangling_reader_size = DANGLING_READER_INCREMENT;
      dangling_readers = (snd_fd **)CALLOC(dangling_reader_size, sizeof(snd_fd *));
      loc = 0;
    }
  else
    {
      for (i = 0; i < dangling_reader_size; i++)
	if (dangling_readers[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = dangling_reader_size;
	  dangling_reader_size += DANGLING_READER_INCREMENT;
	  dangling_readers = (snd_fd **)REALLOC(dangling_readers, dangling_reader_size * sizeof(snd_fd *));
	  for (i = loc; i < dangling_reader_size; i++) dangling_readers[i] = NULL;
	}
    }
  fd->dangling_loc = loc;
  dangling_readers[loc] = fd;
}

static void unlist_reader(snd_fd *fd)
{
  if ((fd) && (fd->dangling_loc >= 0))
    {
      dangling_readers[fd->dangling_loc] = NULL;
      fd->dangling_loc = -1;
    }
}

static void sf_free(snd_fd *fd)
{
  snd_info *sp = NULL;
  if (fd) 
    {
      /* changed to reflect g_free_sample_reader 29-Oct-00 */
      unlist_reader(fd);
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd(fd);
      if (sp) completely_free_snd_info(sp);
    }
}

void release_dangling_readers(chan_info *cp, int edit_ctr)
{
  int i;
  snd_fd *fd;
  for (i = 0; i < dangling_reader_size; i++)
    {
      fd = dangling_readers[i];
      if ((fd) && 
	  (fd->cp == cp) && 
	  (edit_ctr <= fd->edit_ctr))
	{
	  reader_out_of_data(fd); /* sf_free would free fd causing infinite trouble later */
	  dangling_readers[i] = NULL;
	}
    }
}

#if DEBUGGING
void report_dangling_readers(FILE *fp);
void report_dangling_readers(FILE *fp)
{
  int i;
  bool titled = false;
  for (i = 0; i < dangling_reader_size; i++)
    if (dangling_readers[i])
      {
	snd_fd *sf;
	sf = dangling_readers[i];
	if (!titled)
	  {
	    fprintf(fp, "\nDangling snd_fd:\n");
	    fprintf(stderr, "\nDangling snd_fd:\n");
	    titled = true;
	  }
	fprintf(fp, "   %p, cp: %p%s, beg: " OFF_TD ", at " OFF_TD " [frag_pos: " OFF_TD ", first: " OFF_TD ", last: " OFF_TD "], fragment %d\n",
		sf, sf->cp,
		(sf->at_eof) ? ", at eof" : "",
		sf->initial_samp,
		sf->loc, sf->frag_pos, sf->first, sf->last,
		sf->cbi);
	fprintf(stderr, "   %p, cp: %p%s, beg: " OFF_TD ", at " OFF_TD " [frag_pos: " OFF_TD ", first: " OFF_TD ", last: " OFF_TD "], fragment %d\n",
		sf, sf->cp,
		(sf->at_eof) ? ", at eof" : "",
		sf->initial_samp,
		sf->loc, sf->frag_pos, sf->first, sf->last,
		sf->cbi);
      }
}
static void check_dangling_readers(snd_fd *fd)
{
  int i;
  if (fd)
    {
      for (i = 0; i < dangling_reader_size; i++)
	if (dangling_readers[i] == fd)
	  {
	    fprintf(stderr, "lost reader: %p\n", fd);
	    abort();
	  }
    }
}
#endif

XEN_MAKE_OBJECT_FREE_PROCEDURE(snd_fd, free_sf, sf_free)

static XEN g_sample_reader_at_end(XEN obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_end_p " obj): #t if sample-reader has reached the end of its data"
  snd_fd *sf;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_at_end_p, "a sample-reader");
  sf = TO_SAMPLE_READER(obj);
  return(C_TO_XEN_BOOLEAN(sf->at_eof));
}

static XEN g_inspect_sample_reader(XEN obj) 
{
  snd_fd *sf = NULL;
  char *buf;
  XEN res;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, "inspect-sample-reader", "a sample-reader");
  sf = TO_SAMPLE_READER(obj);
  buf = (char *)malloc(4096);
  mus_snprintf(buf, 4096, "snd_fd: %f, %s[%d: %d](%s%s) \
beg: " OFF_TD ", at " OFF_TD " [frag_pos: " OFF_TD ", first: " OFF_TD ", last: " OFF_TD "], fragment %d",
	       sf->curval,
	       (sf->cp->sound) ? sf->cp->sound->filename : "no sound",
	       sf->cp->chan, sf->edit_ctr,
	       (sf->direction == 1) ? "forward" : "backward",
	       (sf->at_eof) ? ", at eof" : "",
	       sf->initial_samp,
	       sf->loc, sf->frag_pos, sf->first, sf->last,
	       sf->cbi);
  res = C_TO_XEN_STRING(buf);
  free(buf);
  return(res);
}

static XEN g_sample_reader_position(XEN obj) 
{
  #define H_sample_reader_position "(" S_sample_reader_position " obj): current (sample-wise) location of sample-reader"
  snd_fd *fd = NULL;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_position, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  if ((fd->cp) && (fd->cp->active) && (fd->cp->sound))
    return(C_TO_XEN_OFF_T(current_location(fd)));
  return(XEN_ZERO);
}

static XEN g_sample_reader_home(XEN obj)
{
  #define H_sample_reader_home "(" S_sample_reader_home " obj): (list sound-index chan-num) associated with reader"
  snd_fd *fd = NULL;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_home, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  if ((fd->cp) && (fd->cp->active) && (fd->cp->sound))
    return(XEN_LIST_2(C_TO_SMALL_XEN_INT(fd->cp->sound->index),
		      C_TO_SMALL_XEN_INT(fd->cp->chan)));
  return(XEN_FALSE);
}

XEN g_c_make_sample_reader(snd_fd *fd)
{
  XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
}

static XEN g_make_sample_reader(XEN samp_n, XEN snd, XEN chn, XEN dir1, XEN pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sample_reader "(" S_make_sample_reader " (start-samp 0) (snd #f) (chn #f) (dir 1) (edpos #f)): \
return a reader ready to access snd's channel chn's data starting at start-samp, going in direction dir (1 = \
forward, -1 = backward), reading the version of the data indicated by edpos which defaults to the current version. \
snd can be a filename, a sound index number, or a list with a mix id number."

  snd_fd *fd = NULL;
  int chan, edpos, direction = 1;
  chan_info *cp;
  snd_state *ss;
  char *filename;
  snd_info *loc_sp = NULL;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_make_sample_reader, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, XEN_ARG_4, S_make_sample_reader, "an integer");
  ss = get_global_state();
  if (XEN_STRING_P(snd))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chn), chn, XEN_ARG_3, S_make_sample_reader, "an integer or boolean");
      filename = XEN_TO_C_STRING(snd);
      if (mus_file_probe(filename))
	loc_sp = make_sound_readable(ss, filename, false);
      else return(snd_no_such_file_error(S_make_sample_reader, snd));
      chan = XEN_TO_C_INT_OR_ELSE(chn, 0);
      if ((chan < 0) || 
	  (chan >= loc_sp->nchans))
	{
	  completely_free_snd_info(loc_sp);
	  return(snd_no_such_channel_error(S_make_sample_reader, snd, chn));	
	}
      cp = loc_sp->chans[chan];
    }
  else 
    {
      ASSERT_CHANNEL(S_make_sample_reader, snd, chn, 2);
      cp = get_cp(snd, chn, S_make_sample_reader);
    }
  edpos = to_c_edit_position(cp, pos, S_make_sample_reader, 5);
  direction = XEN_TO_C_INT_OR_ELSE(dir1, READ_FORWARD);
  beg = beg_to_sample(samp_n, S_make_sample_reader);
  if ((direction == READ_FORWARD) || (direction == READ_BACKWARD))
    fd = init_sample_read_any(beg, cp, direction, edpos);
  else XEN_ERROR(NO_SUCH_DIRECTION,
		 XEN_LIST_2(C_TO_XEN_STRING(S_make_sample_reader),
			    dir1));
  if (fd)
    {
      fd->local_sp = loc_sp;
      list_reader(fd);
      XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(XEN_FALSE);
}

static XEN g_make_region_sample_reader(XEN samp_n, XEN reg, XEN chn, XEN dir1)
{
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " (start-samp 0) (region 0) (chn 0) (dir 1)): \
return a reader ready to access region's channel chn data starting at start-samp going in direction dir"

  snd_fd *fd = NULL;
  int reg_n, chn_n;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_make_sample_reader, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(reg), reg, XEN_ARG_2, S_make_sample_reader, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn), chn, XEN_ARG_3, S_make_sample_reader, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, XEN_ARG_4, S_make_sample_reader, "an integer");

  reg_n = XEN_TO_C_INT_OR_ELSE(reg, 0);
  if (!(region_ok(reg_n))) 
    XEN_ERROR(NO_SUCH_REGION,
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_region_sample_reader),
                         reg));
  chn_n = XEN_TO_C_INT_OR_ELSE(chn, 0);
  if ((chn_n < 0) || (chn_n >= region_chans(reg_n)))
    return(snd_no_such_channel_error(S_make_region_sample_reader, XEN_LIST_1(reg), chn));
  beg = beg_to_sample(samp_n, S_make_region_sample_reader);
  fd = init_region_read(beg,
			reg_n,
			chn_n,
			XEN_TO_C_INT_OR_ELSE(dir1, READ_FORWARD));
  if (fd)
    {
      fd->edit_ctr = -2 - reg_n; /* can't use fd->cp because deferred case is pointer to original (not copied) data */
                                 /* has to be less than -1 because that is the "delete all readers" sign on chan close */
      list_reader(fd);
      XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(XEN_FALSE);
}

void release_region_readers(int reg)
{
  int i, regval;
  snd_fd *fd;
  regval = -2 - reg;
  for (i = 0; i < dangling_reader_size; i++)
    {
      fd = dangling_readers[i];
      if ((fd) && 
	  (fd->edit_ctr == regval))
	{
	  reader_out_of_data(fd); /* sf_free would free fd causing infinite trouble later */
	  dangling_readers[i] = NULL;
	}
    }
}

static XEN g_next_sample(XEN obj)
{
  #define H_next_sample "(" S_next_sample " reader): next sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(protected_next_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_read_sample(XEN obj)
{
  #define H_read_sample "(" S_read_sample " reader): read sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(read_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_previous_sample(XEN obj)
{
  #define H_previous_sample "(" S_previous_sample " reader): previous sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_previous_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(protected_previous_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_free_sample_reader(XEN obj)
{
  #define H_free_sample_reader "(" S_free_sample_reader " reader): free a sample reader"
  snd_fd *fd;
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_sample_reader, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  sp = fd->local_sp; 
  fd->local_sp = NULL;
  free_snd_fd_almost(fd); /* this is different from sf_free! */
  if (sp) completely_free_snd_info(sp);
  return(xen_return_first(XEN_FALSE, obj));
}

static XEN g_save_edit_history(XEN filename, XEN snd, XEN chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename (snd #f) (chn #f)): save snd channel's chn edit history in filename"
  FILE *fd;
  int i, j;
  snd_info *sp;
  chan_info *cp;
  char *mcf = NULL;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_save_edit_history, "a string");
  ASSERT_CHANNEL(S_save_edit_history, snd, chn, 2);
  mcf = mus_expand_filename(XEN_TO_C_STRING(filename));
  fd = FOPEN(mcf, "w");
  if (mcf) FREE(mcf);
  if (fd)
    {
      if ((XEN_INTEGER_P(chn)) && (XEN_INTEGER_P(snd)))
	{
	  cp = get_cp(snd, chn, S_save_edit_history);
	  edit_history_to_file(fd, cp);
	}
      else
	{
	  if (XEN_INTEGER_P(snd))
	    {
	      sp = get_sp(snd, NO_PLAYERS);
	      if (sp)
		for (i = 0; i < sp->nchans; i++)
		  edit_history_to_file(fd, sp->chans[i]);
	    }
	  else
	    {
	      ss = get_global_state();
	      for (i = 0; i < ss->max_sounds; i++)
		{
		  sp = ss->sounds[i];
		  if ((sp) && (sp->inuse == SOUND_NORMAL))
		    for (j = 0; j < sp->nchans; j++)
		      edit_history_to_file(fd, sp->chans[j]);
		}
	    }
	}
    }
  if ((!fd) || (FCLOSE(fd) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_edit_history),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

static XEN g_undo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_undo "(" S_undo " (count 1) (snd #f) (chn #f)): undo 'count' edits in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_undo, "an integer");
  if (XEN_LIST_P(snd_n))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(S_undo),
			 snd_n,
			 C_TO_XEN_STRING(_("can't undo an underlying mix edit except through the outer (mixed-into) sound"))));
  ASSERT_CHANNEL(S_undo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_undo);
  if (XEN_INTEGER_P(ed_n))
    {
      undo_edit_with_sync(cp, 
			  XEN_TO_C_INT(ed_n));
      return(ed_n);
    }
  undo_edit_with_sync(cp, 1);
  return(C_TO_XEN_INT(1));
}

static XEN g_redo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_redo "(" S_redo " (count 1) (snd #f) (chn #f)): redo 'count' edits in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_redo, "an integer");
  if (XEN_LIST_P(snd_n))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(S_redo),
			 snd_n,
			 C_TO_XEN_STRING(_("can't redo an underlying mix edit except through the outer (mixed-into) sound"))));
  ASSERT_CHANNEL(S_redo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_redo);
  if (XEN_INTEGER_P(ed_n))
    {
      redo_edit_with_sync(cp, 
			  XEN_TO_C_INT(ed_n));
      return(ed_n);
    }
  redo_edit_with_sync(cp, 1);
  return(C_TO_XEN_INT(1));
}

void as_one_edit(chan_info *cp, int one_edit, const char *one_edit_origin) /* origin copied here */
{
  bool need_backup = false;
  ed_list *ed;
  need_backup = (cp->edit_ctr > one_edit);      /* cp->edit_ctr will be changing, so save this */
  if (cp->edit_ctr >= one_edit)                 /* ">=" here because the origin needs to be set even if there were no extra edits */
    {
      if ((cp->state) && (cp->state->deferred_regions > 0))
	sequester_deferred_regions(cp, one_edit - 1);
      while (cp->edit_ctr > one_edit) backup_edit_list(cp);
      if ((need_backup) && (cp->mixes)) backup_mix_list(cp, one_edit);
      if (one_edit_origin)
	{
	  ed = cp->edits[cp->edit_ctr];
	  if (ed)
	    {
	      if (ed->origin) FREE(ed->origin);
	      ed->origin = copy_string(one_edit_origin);
	    }
	}
      if (need_backup) prune_edits(cp, cp->edit_ctr + 1);
    }
}

static int chan_ctr = 0;
static char *as_one_edit_origin;

static void init_as_one_edit(chan_info *cp, void *ptr) 
{
  ((int *)ptr)[chan_ctr] = cp->edit_ctr; 
  cp->previous_squelch_update = cp->squelch_update; /* preserve possible user setting across as-one-edit call */
  cp->squelch_update = true;
  cp->in_as_one_edit = true;
  chan_ctr++; 
}

static void finish_as_one_edit(chan_info *cp, void *ptr) 
{
  as_one_edit(cp, (((int *)ptr)[chan_ctr] + 1), as_one_edit_origin);
  cp->squelch_update = cp->previous_squelch_update;
  cp->in_as_one_edit = false;
  reflect_edit_history_change(cp);
  update_graph(cp);
  chan_ctr++; 
}

#if HAVE_DYNAMIC_WIND
/* protect against errors within as-one-edit */
typedef struct {
  XEN proc;
  int *cur_edits;
  int chans;
} as_one_edit_context;

static void before_as_one_edit(void *context)
{
  as_one_edit_context *sc = (as_one_edit_context *)context;
  sc->cur_edits = (int *)CALLOC(sc->chans, sizeof(int));
  chan_ctr = 0;
  for_each_chan_1(get_global_state(), init_as_one_edit, (void *)(sc->cur_edits));
}

static XEN as_one_edit_body(void *context)
{
  as_one_edit_context *sc = (as_one_edit_context *)context;
  return(XEN_CALL_0_NO_CATCH(sc->proc, S_as_one_edit));
}

static void after_as_one_edit(void *context)
{
  as_one_edit_context *sc = (as_one_edit_context *)context;
  chan_ctr = 0;
  for_each_chan_1(get_global_state(), finish_as_one_edit, (void *)(sc->cur_edits));
  FREE(sc->cur_edits);
  FREE(sc);
}
#endif


static XEN g_as_one_edit(XEN proc, XEN origin)
{
  #define H_as_one_edit "(" S_as_one_edit " thunk (origin #f)): evaluate thunk, collecting all edits into one from the edit historys' point of view"
  int chans;
#if (!HAVE_DYNAMIC_WIND)
  int *cur_edits;
#endif
  snd_state *ss;
  XEN result = XEN_FALSE;
  char *errmsg;
  XEN errstr;
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, S_as_one_edit, "a procedure");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_2, S_as_one_edit, "a string");
  errmsg = procedure_ok(proc, 0, S_as_one_edit, "edit", 1);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_as_one_edit, errstr, proc));
    }
  ss = get_global_state();
  chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
  if (chans > 0)
    {
      if (XEN_STRING_P(origin))
	as_one_edit_origin = XEN_TO_C_STRING(origin);
      else as_one_edit_origin = NULL;
#if HAVE_DYNAMIC_WIND
      {
	as_one_edit_context *sc;
	sc = (as_one_edit_context *)CALLOC(1, sizeof(as_one_edit_context));
	sc->chans = chans;
	sc->proc = proc;
	result = scm_internal_dynamic_wind((scm_t_guard)before_as_one_edit, 
					   (scm_t_inner)as_one_edit_body, 
					   (scm_t_guard)after_as_one_edit, 
					   (void *)sc,
					   (void *)sc);
      }
#else
      cur_edits = (int *)CALLOC(chans, sizeof(int));
      chan_ctr = 0;
      for_each_chan_1(ss, init_as_one_edit, (void *)cur_edits); /* redo here can't make sense, can it? */
      /* this is problematic mainly because we now squelch updates within as-one-edit */
      /*   so we really need the dynamic unwind above to make sure graphics aren't disabled by a user programming error */
      result = XEN_CALL_0_NO_CATCH(proc, S_as_one_edit);
      chan_ctr = 0;
      for_each_chan_1(ss, finish_as_one_edit, (void *)cur_edits);
      FREE(cur_edits);
#endif
    }
  return(xen_return_first(result, proc, origin));
}

static XEN g_scale_sound_by(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_scale_sound_by "(" S_scale_sound_by " scaler (beg 0) (num len) (snd #f) (chn #f) (edpos #f)): \
scale samples in the given sound/channel \
between beg and beg + num by scaler.  If channel is omitted, the scaling applies to the entire sound (and edpos is ignored)."

  snd_info *sp;
  chan_info *cp;
  int i, pos;
  off_t samp;
  Float scaler;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_1, S_scale_sound_by, "a number");
  ASSERT_SAMPLE_TYPE(S_scale_sound_by, beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_scale_sound_by, num, XEN_ARG_3);
  ASSERT_SOUND(S_scale_sound_by, snd, 4);
  scaler = XEN_TO_C_DOUBLE(scl);
  samp = beg_to_sample(beg, S_scale_sound_by);
  if (XEN_INTEGER_P(chn))
    {
      cp = get_cp(snd, chn, S_scale_sound_by);
      pos = to_c_edit_position(cp, edpos, S_scale_sound_by, 6);
      scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_sound_by), pos, false);
    }
  else
    {
      sp = get_sp(snd, NO_PLAYERS);
      if (sp == NULL)
	return(snd_no_such_sound_error(S_scale_sound_by, snd));
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  pos = cp->edit_ctr;
	  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_sound_by), pos, false);
	}
    }
  return(scl);
}

static XEN g_scale_channel(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_scale_channel "(" S_scale_channel " scaler (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
scale samples in the given sound/channel between beg and beg + num by scaler."

  Float scaler;
  chan_info *cp;
  off_t samp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_1, S_scale_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_scale_channel, beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_scale_channel, num, XEN_ARG_3);
  ASSERT_SOUND(S_scale_channel, snd, 4);
  scaler = XEN_TO_C_DOUBLE(scl);
  samp = beg_to_sample(beg, S_scale_channel);
  cp = get_cp(snd, chn, S_scale_channel);
  pos = to_c_edit_position(cp, edpos, S_scale_channel, 6);
  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_channel), pos, false);
  return(scl);
}			  

Float local_maxamp(chan_info *cp, off_t beg, off_t num, int edpos)
{
  snd_fd *sf;
  snd_state *ss;
  mus_sample_t ymax, mval;
  off_t i;
  int j = 0;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) return(0.0);
  ymax = MUS_SAMPLE_0;
  if (num > (1 << 30))
    {
      ss = cp->state;
      ss->stopped_explicitly = false;
      for (i = 0; i < num; i++)
	{
	  mval = mus_sample_abs(read_sample(sf));
	  if (mval > ymax) ymax = mval;
	  j++;
	  if (j > 1000000)
	    {
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = false;
		  report_in_minibuffer(cp->sound, _("maxamp check interrupted..."));
		  break;
		}
	    }
	}
    }
  else
    {
      for (i = 0; i < num; i++)
	{
	  mval = mus_sample_abs(read_sample(sf));
	  if (mval > ymax) ymax = mval;
	}
    }
  free_snd_fd(sf);
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

static XEN g_scale_sound_to(XEN norm, XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_scale_sound_to "(" S_scale_sound_to " norm (beg 0) (num len) (snd #f) (chn #f)): \
scale samples in the given sound/channel \
between beg and beg + num to peak value norm.  If channel is omitted, the scaling applies to the entire sound."

  snd_info *sp;
  chan_info *cp;
  int i;
  off_t samp, samps;
  Float scaler, maxamp = 0.0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(norm), norm, XEN_ARG_1, S_scale_sound_to, "a number");
  ASSERT_SAMPLE_TYPE(S_scale_sound_to, beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_scale_sound_to, num, XEN_ARG_3);
  ASSERT_SOUND(S_scale_sound_to, snd, 4);
  scaler = XEN_TO_C_DOUBLE(norm);
  samp = beg_to_sample(beg, S_scale_sound_to);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_scale_sound_to, snd));
  if (XEN_INTEGER_P(chn))
    {
      cp = get_cp(snd, chn, S_scale_sound_by);
      samps = dur_to_samples(num, samp, cp, cp->edit_ctr, 3, S_scale_sound_to);
      if ((samp == 0) &&
	  (samps >= CURRENT_SAMPLES(cp)))
	maxamp = get_maxamp(sp, cp, AT_CURRENT_EDIT_POSITION);
      else maxamp = local_maxamp(cp, samp, samps, cp->edit_ctr);
      if (maxamp > 0.0)
	{
	  scaler /= maxamp;
	  scale_channel(cp, scaler, samp, samps, cp->edit_ctr, false);
	}
    }
  else
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  samps = dur_to_samples(num, samp, cp, cp->edit_ctr, 3, S_scale_sound_to);
	  if ((samp == 0) &&
	      (samps >= CURRENT_SAMPLES(cp)))
	    maxamp = get_maxamp(sp, cp, AT_CURRENT_EDIT_POSITION);
	  else maxamp = local_maxamp(cp, samp, samps, cp->edit_ctr);
	}
      if (maxamp > 0.0)
	{
	  scaler /= maxamp;
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      samps = dur_to_samples(num, samp, cp, cp->edit_ctr, 3, S_scale_sound_to);
	      scale_channel(cp, scaler, samp, samps, cp->edit_ctr, false);
	    }
	}
    }
  return(norm);
}

static mus_sample_t *g_floats_to_samples(XEN obj, int *size, const char *caller, int position)
{
  mus_sample_t *vals = NULL;
  XEN *vdata;
  vct *v;
  int i, num = 0;
  XEN lst;
  if (XEN_LIST_P_WITH_LENGTH(obj, num))
    {
      if (num == 0) return(NULL);
      if (((*size) > 0) && (num > (*size))) 
	num = (*size);
      vals = (mus_sample_t *)MALLOC(num * sizeof(mus_sample_t));
      for (i = 0, lst = XEN_COPY_ARG(obj); i < num; i++, lst = XEN_CDR(lst)) 
	vals[i] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0));
    }
  else
    {
      if (XEN_VECTOR_P(obj))
	{
	  num = XEN_VECTOR_LENGTH(obj); 
	  if (num == 0) return(NULL);
	  if (((*size) > 0) && (num > (*size)))
	    num = (*size);
	  vals = (mus_sample_t *)MALLOC(num * sizeof(mus_sample_t));
	  vdata = XEN_VECTOR_ELEMENTS(obj);
	  for (i = 0; i < num; i++) 
	    vals[i] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(vdata[i], 0.0));
	}
      else
	{
	  if (VCT_P(obj))
	    {
	      v = TO_VCT(obj);
	      num = v->length; 
	      if (((*size) > 0) && (num > (*size)))
		num = (*size);
	      vals = (mus_sample_t *)MALLOC(num * sizeof(mus_sample_t));
	      for (i = 0; i < num; i++) 
		vals[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
	    }
	  else XEN_ASSERT_TYPE(0, obj, position, caller, "a vct, vector, or list");
	}
    }
  (*size) = num;
  return(vals);
}

static XEN g_sample(XEN samp_n, XEN snd_n, XEN chn_n, XEN pos_n)
{
  #define H_sample "(" S_sample " samp (snd #f) (chn #f) (edpos #f)): \
return sample samp in snd's channel chn (this is a slow access -- use sample-readers for speed)"
  chan_info *cp;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_sample, "a number");
  ASSERT_CHANNEL(S_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_sample);
  if (XEN_BOUND_P(samp_n))
    beg = beg_to_sample(samp_n, S_sample);
  else beg = CURSOR(cp);
  return(C_TO_XEN_DOUBLE(chn_sample(beg,
				    cp, 
				    to_c_edit_position(cp, pos_n, S_sample, 4))));

}

static XEN g_set_sample(XEN samp_n, XEN val, XEN snd_n, XEN chn_n, XEN edpos)
{
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  int pos;
  off_t beg;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_setB S_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_sample, "a number");
  ASSERT_CHANNEL(S_setB S_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_setB S_sample);
  pos = to_c_edit_position(cp, edpos, S_setB S_sample, 5);
  if (XEN_BOUND_P(samp_n))
    beg = beg_to_sample(samp_n, S_setB S_sample);
  else beg = CURSOR(cp);
  ival[0] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  change_samples(beg, 1, ival, cp, LOCK_MIXES, S_setB S_sample, pos);
  update_graph(cp);
  return(val);
}

#if HAVE_GUILE
static XEN g_set_sample_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5)
{
  if (XEN_NOT_BOUND_P(arg2))
    return(g_set_sample(XEN_UNDEFINED, arg1, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(arg3))
	return(g_set_sample(arg1, arg2, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else 
	{
	  if (XEN_NOT_BOUND_P(arg4)) 
	    return(g_set_sample(arg1, arg3, arg2, XEN_UNDEFINED, XEN_UNDEFINED)); 
	  else 
	    {
	      if (XEN_NOT_BOUND_P(arg5)) 
		return(g_set_sample(arg1, arg4, arg2, arg3, XEN_UNDEFINED)); 
	      else return(g_set_sample(arg1, arg5, arg2, arg3, arg4));
	    }
	}
    }
}
#endif

static XEN g_set_samples(XEN samp_0, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN truncate, XEN edname, XEN infile_chan, XEN edpos, XEN auto_delete)
{
  #define H_set_samples "(set-" S_samples " start-samp samps data (snd #f) (chn #f) (truncate #f) (edname #f) (infile-chan 0) (edpos #f) (auto-delete #f)): \
set snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename)); \
start-samp can be beyond current data end; if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end."

  chan_info *cp;
  mus_sample_t *ivals;
  off_t len = 0, beg, curlen;
  int override = 0, inchan = 0, pos;
  bool delete_file = false;
  char *fname, *caller;
  if (XEN_STRING_P(edname))
    caller = XEN_TO_C_STRING(edname);
  else caller = "set-samples";
  ASSERT_SAMPLE_TYPE(caller, samp_0, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(caller, samps, XEN_ARG_2);
  ASSERT_CHANNEL(caller, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(truncate), truncate, XEN_ARG_6, caller, "a boolean");
  cp = get_cp(snd_n, chn_n, caller);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(infile_chan), infile_chan, XEN_ARG_8, caller, "an integer");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(edname) || XEN_STRING_P(edname) || XEN_BOOLEAN_P(edname), edname, XEN_ARG_7, caller, "a string");
  pos = to_c_edit_position(cp, edpos, caller, 9);
  beg = beg_to_sample(samp_0, caller);
  len = dur_to_samples(samps, beg, cp, pos, 2, caller);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, 10, caller, "a boolean");
  if (len == 0) return(XEN_FALSE);
  override = XEN_TRUE_P(truncate);
  if (XEN_STRING_P(vect))
    {
      curlen = CURRENT_SAMPLES(cp);
      fname = XEN_TO_C_STRING(vect);
      inchan = XEN_TO_C_INT_OR_ELSE(infile_chan, 0);
      if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
      if ((beg == 0) && 
	  ((len > curlen) || override))
	file_override_samples(len, fname, cp, inchan, (delete_file) ? DELETE_ME : DONT_DELETE_ME, LOCK_MIXES, caller);
      else file_change_samples(beg, len, fname, cp, inchan, (delete_file) ? DELETE_ME : DONT_DELETE_ME, LOCK_MIXES, caller, pos);
    }
  else
    {
      int ilen;
      ilen = (int)len;
      ivals = g_floats_to_samples(vect, &ilen, caller, 3);
      if (ivals)
	{
	  change_samples(beg, (off_t)ilen, ivals, cp, LOCK_MIXES, caller, pos);
	  FREE(ivals);
	}
    }
  update_graph(cp);
  return(vect);
}

static XEN g_vct2samples(XEN samp_0, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN truncate, XEN edname, XEN infile_chan)
{
  #define H_vct2samples "(" S_vct2samples " start-samp samps data (snd #f) (chn #f) (truncate #f) (edname #f) (infile-chan 0)): \
set snd's channel chn's samples starting at start-samp for samps from data (a vct); \
start-samp can be beyond current data end; if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end.  start-samp can also be a vct, as can samps."

  vct *v;
  if (VCT_P(samp_0))
    {
      v = TO_VCT(samp_0);
      if (XEN_INTEGER_P(samps))
	return(g_set_samples(XEN_ZERO, samps, samp_0, 
			     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else return(g_set_samples(XEN_ZERO, C_TO_XEN_INT(v->length), samp_0, 
				XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
    }
  else
    {
      if ((XEN_INTEGER_P(samp_0)) && (VCT_P(samps)))
	{
	  v = TO_VCT(samps);
	  return(g_set_samples(samp_0, C_TO_XEN_INT(v->length), samps, 
			       XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	}
    }
  return(g_set_samples(samp_0, samps, vect, snd_n, chn_n, truncate, edname, infile_chan, XEN_UNDEFINED, XEN_UNDEFINED));
}

static XEN g_vct2channel(XEN v, XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_vct2channel "(" S_vct2channel " vct (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
set snd's channel chn's samples starting at beg for dur samps from vct data"
  vct *v1;
  XEN_ASSERT_TYPE(VCT_P(v), v, XEN_ARG_1, S_vct2channel, "a vct");
  if (XEN_NOT_BOUND_P(beg)) beg = XEN_ZERO;
  if (XEN_NOT_BOUND_P(dur)) 
    {
      v1 = TO_VCT(v);
      dur = C_TO_XEN_INT(v1->length);
    }
  return(g_set_samples(beg, dur, v, snd_n, chn_n, XEN_FALSE, C_TO_XEN_STRING(S_vct2channel), XEN_FALSE, edpos, XEN_UNDEFINED));
}

static XEN samples2vct_1(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN v, XEN edpos, const char *caller)
{
  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  off_t i, len, beg;
  int pos;
  vct *v1 = get_vct(v);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0), samp_0, XEN_ARG_1, caller, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_2, caller, "a number");
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  pos = to_c_edit_position(cp, edpos, caller, 6);
  beg = beg_to_sample(samp_0, caller);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, cp->samples[pos] - beg);
  if ((beg == 0) && (len == 0)) return(XEN_FALSE); /* empty file (channel) possibility */
  if (len <= 0) XEN_OUT_OF_RANGE_ERROR(caller, 2, samps, "samples ~A < 0?");
  if (v1)
    {
      fvals = v1->data;
      if (len > v1->length)
	len = v1->length;
    }
  else fvals = (Float *)MALLOC(len * sizeof(Float));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf)
    {
      for (i = 0; i < len; i++) 
	fvals[i] = read_sample_to_float(sf);
      free_snd_fd(sf);
    }
  if (v1)
    return(v);
  else return(make_vct(len, fvals));
}

static XEN g_samples2vct(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN v, XEN edpos)
{
  #define H_samples2vct "(" S_samples2vct " (start-samp 0) (samps len) (snd #f) (chn #f) (vct-obj #f) (edpos #f)): \
return a vct (vct-obj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edpos (defaulting to the current version)"

  return(samples2vct_1(samp_0, samps, snd_n, chn_n, v, edpos, S_samples2vct));
}

static XEN g_channel2vct(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_channel2vct "(" S_channel2vct " (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
return a vct containing snd channel chn's data starting at beg for dur samps"

  return(samples2vct_1(samp_0, samps, snd_n, chn_n, XEN_FALSE, edpos, S_channel2vct));
}

static XEN g_samples(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_samples "(" S_samples " (start-samp 0) (samps len) (snd #f) (chn #f) (edpos #f)): \
return a vct containing snd channel chn's samples starting a start-samp for samps samples; edpos is the edit \
history position to read (defaults to current position)."

  return(samples2vct_1(samp_0, samps, snd_n, chn_n, XEN_FALSE, edpos, S_samples));
}

#if HAVE_GUILE
static XEN g_set_samples_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8, XEN arg9, XEN arg10)
{
  /* (set! (samples start samps [snd chn trunc edname infilechan edpos delete]) vect) */
  if (XEN_NOT_BOUND_P(arg4))
    return(g_set_samples(arg1, arg2, arg3, 
			 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(arg5))
	return(g_set_samples(arg1, arg2, arg4, arg3, 
			     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else 
	{
	  if (XEN_NOT_BOUND_P(arg6)) 
	    return(g_set_samples(arg1, arg2, arg5, arg3, arg4, 
				 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	  else
	    {
	      if (XEN_NOT_BOUND_P(arg7)) 
		return(g_set_samples(arg1, arg2, arg6, arg3, arg4, arg5, 
				     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	      else
		{
		  if (XEN_NOT_BOUND_P(arg8)) 
		    return(g_set_samples(arg1, arg2, arg7, arg3, arg4, arg5, arg6, 
					 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
		  else
		    {
		      if (XEN_NOT_BOUND_P(arg9)) 
			return(g_set_samples(arg1, arg2, arg8, arg3, arg4, arg5, arg6, arg7,
					     XEN_UNDEFINED, XEN_UNDEFINED));
		      else 
			{
			  if (XEN_NOT_BOUND_P(arg10)) 
			    return(g_set_samples(arg1, arg2, arg9, arg3, arg4, arg5, arg6, arg7, arg8, XEN_UNDEFINED));
			  else return(g_set_samples(arg1, arg2, arg10, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
			}
		    }
		}
	    }
	}
    }
}
#endif

static XEN g_change_samples_with_origin(XEN samp_0, XEN samps, XEN origin, XEN vect, XEN snd_n, XEN chn_n, XEN edpos)
{
  chan_info *cp;
  mus_sample_t *ivals;
  int pos;
  off_t beg, i, len;
  XEN *vdata;
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samp_0), samp_0, XEN_ARG_1, S_change_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samps), samps, XEN_ARG_2, S_change_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_3, S_change_samples_with_origin, "a string");
  XEN_ASSERT_TYPE((XEN_VECTOR_P(vect)) || (XEN_STRING_P(vect)), vect, XEN_ARG_4, S_change_samples_with_origin, "a vector or a string");
  ASSERT_CHANNEL(S_change_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_change_samples_with_origin);
  beg = beg_to_sample(samp_0, S_change_samples_with_origin);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_change_samples_with_origin, 7);
  if (XEN_VECTOR_P(vect))
    {
      ivals = (mus_sample_t *)MALLOC(len * sizeof(mus_sample_t));
      vdata = XEN_VECTOR_ELEMENTS(vect);
      if (len > XEN_VECTOR_LENGTH(vect)) len = XEN_VECTOR_LENGTH(vect);
      if (len <= 0) return(XEN_FALSE); /* should this be an error? */
#if SNDLIB_USE_FLOATS
      for (i = 0; i < len; i++) ivals[i] = XEN_TO_C_DOUBLE(vdata[i]);
#else
      for (i = 0; i < len; i++) ivals[i] = XEN_TO_C_INT_OR_ELSE(vdata[i], 0);
#endif
      change_samples(beg, len, ivals, cp, LOCK_MIXES, XEN_TO_C_STRING(origin), pos);
      FREE(ivals);
    }
  else
    {
      /* string = filename here */
      file_change_samples(beg, len,
			  XEN_TO_C_STRING(vect),
			  cp, 0, DONT_DELETE_ME, LOCK_MIXES,
			  XEN_TO_C_STRING(origin), 
			  pos);
    }
  update_graph(cp);
  return(xen_return_first(vect, origin));
}

static XEN g_insert_sound(XEN file, XEN ubeg, XEN file_chn, XEN snd_n, XEN chn_n, XEN edpos, XEN auto_delete)
{
  #define H_insert_sound "(" S_insert_sound " file (beg 0) (file-chan 0) (snd #f) (chn #f) (edpos #f) (auto-delete #f)): \
insert channel file-chan of file (or all chans if file-chan is not given) into snd's channel chn at beg or at the cursor position.\n\
  (insert-sound \"oboe.snd\" 1000)\n\
inserts all of oboe.snd starting at sample 1000."

  chan_info *cp;
  snd_info *sp;
  char *filename = NULL;
  int nc, fchn, i;
  bool delete_file = false;
  off_t beg = 0, len;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_insert_sound, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(ubeg), ubeg, XEN_ARG_2, S_insert_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(file_chn), file_chn, XEN_ARG_3, S_insert_sound, "an integer");
  ASSERT_CHANNEL(S_insert_sound, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_sound);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_insert_sound, "a boolean");
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  nc = mus_sound_chans(filename);
  if (nc <= 0)
    {
      if (filename) FREE(filename);
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_4(C_TO_XEN_STRING(S_insert_sound),
			   file,
			   C_TO_XEN_STRING("chans <= 0"),
			   C_TO_XEN_INT(nc)));
    }
  len = mus_sound_frames(filename);
  if (len <= 0) 
    {
      if (filename) FREE(filename);
      return(C_TO_XEN_OFF_T(len));
    }
  if (XEN_NUMBER_P(ubeg))
    beg = beg_to_sample(ubeg, S_insert_sound);
  else beg = CURSOR(cp);
  if (XEN_INTEGER_P(file_chn))
    {
      fchn = XEN_TO_C_INT(file_chn);
      if (fchn < mus_sound_chans(filename))
	{
	  file_insert_samples(beg, len, filename, cp, fchn, (delete_file) ? DELETE_ME : DONT_DELETE_ME, S_insert_sound,
			      to_c_edit_position(cp, edpos, S_insert_sound, 6));
	  update_graph(cp);
	  if (filename) FREE(filename);
	  return(C_TO_XEN_OFF_T(len));
	}
      else 
	{
	  if (filename) FREE(filename);
	  return(snd_no_such_channel_error(S_insert_sound, file, file_chn));	
	}
    }
  else
    {
      sp = cp->sound;
      if (sp->nchans < nc) nc = sp->nchans;
      for (i = 0; i < nc; i++)
	{
	  file_insert_samples(beg, len, filename, sp->chans[i], i, (delete_file) ? DELETE_ME: DONT_DELETE_ME, S_insert_sound,
			      /* this edit_position cannot be optimized out -- each channel may have
			       *   a different edit history, but edpos might be -1 throughout etc.
			       */
			      to_c_edit_position(sp->chans[i], edpos, S_insert_sound, 6));
	  update_graph(sp->chans[i]);
	}
      if (filename) FREE(filename);
      return(C_TO_XEN_OFF_T(len));
    }
  return(XEN_FALSE); /* not reached */
}

static XEN g_delete_sample(XEN samp_n, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_delete_sample "(" S_delete_sample " samp (snd #f) (chn #f) (edpos #f)): delete sample 'samp' from snd's channel chn"
  chan_info *cp;
  off_t samp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_delete_sample, "a number");
  ASSERT_CHANNEL(S_delete_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_delete_sample);
  samp = beg_to_sample(samp_n, S_delete_sample);
  pos = to_c_edit_position(cp, edpos, S_delete_sample, 4);
  if ((samp < 0) || (samp > CURRENT_SAMPLES(cp)))
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_4(C_TO_XEN_STRING(S_delete_sample),
			 samp_n,
			 snd_n, chn_n));
  delete_samples(samp, 1, cp, S_delete_sample, pos);
  update_graph(cp);
  return(samp_n);
}

static XEN g_delete_samples_1(XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, const char *origin, XEN edpos)
{
  chan_info *cp;
  int pos;
  off_t samp, len;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, origin, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, origin, "a number");
  ASSERT_CHANNEL(origin, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, origin);
  pos = to_c_edit_position(cp, edpos, S_delete_samples, 6);
  samp = beg_to_sample(samp_n, origin);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(XEN_FALSE);
  delete_samples(samp, len, cp, origin, pos);
  update_graph(cp);
  return(samp_n);
}

static XEN g_delete_samples(XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps (snd #f) (chn #f) (edpos #f)): \
delete 'samps' samples from snd's channel chn starting at 'start-samp'"

  return(g_delete_samples_1(samp_n, samps, snd_n, chn_n, S_delete_samples, edpos));
}

static XEN g_delete_samples_with_origin(XEN samp_n, XEN samps, XEN origin, XEN snd_n, XEN chn_n, XEN edpos)
{
  XEN res;
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_3, S_delete_samples_with_origin, "a string");
  res = g_delete_samples_1(samp_n, samps, snd_n, chn_n, XEN_TO_C_STRING(origin), edpos);
  return(xen_return_first(res, origin));
}

static XEN g_insert_sample(XEN samp_n, XEN val, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_insert_sample "(" S_insert_sample " sample value (snd #f) (chn #f) (edpos #f)): insert 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  int pos;
  off_t beg;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_insert_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_insert_sample, "a number");
  ASSERT_CHANNEL(S_insert_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_sample);
  beg = beg_to_sample(samp_n, S_insert_sample);
  pos = to_c_edit_position(cp, edpos, S_insert_sample, 5);
  ival[0] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  insert_samples(beg, 1, ival, cp, S_insert_sample, pos);
  update_graph(cp);
  return(val);
}

static XEN g_insert_samples(XEN samp, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN edpos, XEN auto_delete)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data (snd #f) (chn #f) (edpos #f) (auto-delete #f)): \
insert data (either a vector, vct, or list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  mus_sample_t *ivals;
  int pos;
  bool delete_file = false;
  off_t beg, len = 0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_1, S_insert_samples, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, S_insert_samples, "a number");
  ASSERT_CHANNEL(S_insert_samples, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_samples);
  beg = beg_to_sample(samp, S_insert_samples);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples, 6);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_insert_samples, "a boolean");
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  if (XEN_STRING_P(vect))
    file_insert_samples(beg, len, XEN_TO_C_STRING(vect), cp, 0, (delete_file) ? DELETE_ME : DONT_DELETE_ME, S_insert_samples, pos);
  else
    {
      int ilen;
      ilen = (int)len;
      ivals = g_floats_to_samples(vect, &ilen, S_insert_samples, 3);
      if (ivals)
	{
	  insert_samples(beg, (off_t)ilen, ivals, cp, S_insert_samples, pos);
	  FREE(ivals);
	}
    }
  update_graph(cp);
  return(C_TO_XEN_OFF_T(len));
}

static XEN g_insert_samples_with_origin(XEN samp, XEN samps, XEN origin, XEN vect, XEN snd_n, XEN chn_n, XEN edpos)
{
  chan_info *cp;
  mus_sample_t *ivals;
  int pos;
  off_t i, beg, len;
  XEN *vdata;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samp), samp, XEN_ARG_1, S_insert_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samps), samps, XEN_ARG_2, S_insert_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_3, S_insert_samples_with_origin, "a string");
  XEN_ASSERT_TYPE((XEN_VECTOR_P(vect)) || (XEN_STRING_P(vect)), vect, XEN_ARG_4, S_insert_samples_with_origin, "a vector or a string");
  ASSERT_CHANNEL(S_insert_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_insert_samples_with_origin);
  beg = beg_to_sample(samp, S_insert_samples_with_origin);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples_with_origin, 7);
  if (XEN_VECTOR_P(vect))
    {
      ivals = (mus_sample_t *)MALLOC(len * sizeof(mus_sample_t));
      vdata = XEN_VECTOR_ELEMENTS(vect);
      if (len > XEN_VECTOR_LENGTH(vect)) len = XEN_VECTOR_LENGTH(vect);
      if (len <= 0) return(XEN_FALSE); /* should this be an error? */
#if SNDLIB_USE_FLOATS
      for (i = 0; i < len; i++) ivals[i] = XEN_TO_C_DOUBLE(vdata[i]);
#else
      for (i = 0; i < len; i++) ivals[i] = XEN_TO_C_INT_OR_ELSE(vdata[i], MUS_SAMPLE_0);
#endif
      insert_samples(beg, len, ivals, cp, XEN_TO_C_STRING(origin), pos);
      FREE(ivals);
    }
  else file_insert_samples(beg, len, XEN_TO_C_STRING(vect), cp, 0, DONT_DELETE_ME, XEN_TO_C_STRING(origin), pos);
  update_graph(cp);
  return(C_TO_XEN_OFF_T(len));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_5(g_make_sample_reader_w, g_make_sample_reader)
XEN_ARGIFY_4(g_make_region_sample_reader_w, g_make_region_sample_reader)
XEN_NARGIFY_1(g_next_sample_w, g_next_sample)
XEN_NARGIFY_1(g_read_sample_w, g_read_sample)
XEN_NARGIFY_1(g_previous_sample_w, g_previous_sample)
XEN_NARGIFY_1(g_free_sample_reader_w, g_free_sample_reader)
XEN_NARGIFY_1(g_sample_reader_home_w, g_sample_reader_home)
XEN_NARGIFY_1(g_sample_reader_position_w, g_sample_reader_position)
XEN_NARGIFY_1(g_inspect_sample_reader_w, g_inspect_sample_reader)
XEN_NARGIFY_1(g_sf_p_w, g_sf_p)
XEN_NARGIFY_1(g_sample_reader_at_end_w, g_sample_reader_at_end)
XEN_ARGIFY_3(g_save_edit_history_w, g_save_edit_history)
XEN_ARGIFY_3(g_edit_fragment_w, g_edit_fragment)
XEN_ARGIFY_3(g_undo_w, g_undo)
XEN_ARGIFY_3(g_redo_w, g_redo)
XEN_ARGIFY_2(g_as_one_edit_w, g_as_one_edit)
XEN_ARGIFY_4(g_display_edits_w, g_display_edits)
XEN_ARGIFY_3(g_edit_tree_w, g_edit_tree)
XEN_ARGIFY_4(g_delete_sample_w, g_delete_sample)
XEN_ARGIFY_5(g_delete_samples_w, g_delete_samples)
XEN_ARGIFY_5(g_insert_sample_w, g_insert_sample)
XEN_ARGIFY_7(g_insert_samples_w, g_insert_samples)
XEN_ARGIFY_8(g_vct2samples_w, g_vct2samples)
XEN_ARGIFY_6(g_vct2channel_w, g_vct2channel)
XEN_ARGIFY_6(g_samples2vct_w, g_samples2vct)
XEN_ARGIFY_5(g_channel2vct_w, g_channel2vct)
XEN_ARGIFY_7(g_insert_sound_w, g_insert_sound)
XEN_ARGIFY_6(g_scale_sound_by_w, g_scale_sound_by)
XEN_ARGIFY_6(g_scale_channel_w, g_scale_channel)
XEN_ARGIFY_5(g_scale_sound_to_w, g_scale_sound_to)
XEN_ARGIFY_7(g_change_samples_with_origin_w, g_change_samples_with_origin)
XEN_ARGIFY_6(g_delete_samples_with_origin_w, g_delete_samples_with_origin)
XEN_ARGIFY_7(g_insert_samples_with_origin_w, g_insert_samples_with_origin)
XEN_ARGIFY_4(g_sample_w, g_sample)
XEN_ARGIFY_5(g_set_sample_w, g_set_sample)
XEN_ARGIFY_5(g_samples_w, g_samples)
XEN_ARGIFY_10(g_set_samples_w, g_set_samples)
#else
#define g_make_sample_reader_w g_make_sample_reader
#define g_make_region_sample_reader_w g_make_region_sample_reader
#define g_next_sample_w g_next_sample
#define g_read_sample_w g_read_sample
#define g_previous_sample_w g_previous_sample
#define g_free_sample_reader_w g_free_sample_reader
#define g_sample_reader_home_w g_sample_reader_home
#define g_sample_reader_position_w g_sample_reader_position
#define g_inspect_sample_reader_w g_inspect_sample_reader
#define g_sf_p_w g_sf_p
#define g_sample_reader_at_end_w g_sample_reader_at_end
#define g_save_edit_history_w g_save_edit_history
#define g_edit_fragment_w g_edit_fragment
#define g_undo_w g_undo
#define g_redo_w g_redo
#define g_as_one_edit_w g_as_one_edit
#define g_display_edits_w g_display_edits
#define g_edit_tree_w g_edit_tree
#define g_delete_sample_w g_delete_sample
#define g_delete_samples_w g_delete_samples
#define g_insert_sample_w g_insert_sample
#define g_insert_samples_w g_insert_samples
#define g_vct2samples_w g_vct2samples
#define g_vct2channel_w g_vct2channel
#define g_samples2vct_w g_samples2vct
#define g_channel2vct_w g_channel2vct
#define g_insert_sound_w g_insert_sound
#define g_scale_sound_by_w g_scale_sound_by
#define g_scale_channel_w g_scale_channel
#define g_scale_sound_to_w g_scale_sound_to
#define g_change_samples_with_origin_w g_change_samples_with_origin
#define g_delete_samples_with_origin_w g_delete_samples_with_origin
#define g_insert_samples_with_origin_w g_insert_samples_with_origin
#define g_sample_w g_sample
#define g_set_sample_w g_set_sample
#define g_samples_w g_samples
#define g_set_samples_w g_set_samples
#endif

void g_init_edits(void)
{
  sf_tag = XEN_MAKE_OBJECT_TYPE("SampleReader", sizeof(snd_fd));

#if HAVE_GUILE
  scm_set_smob_print(sf_tag, print_sf);
  scm_set_smob_free(sf_tag, free_sf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sf_tag, XEN_PROCEDURE_CAST g_read_sample, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(sf_tag, "to_s", XEN_PROCEDURE_CAST print_sf, 0);
#endif

  XEN_DEFINE_CONSTANT(S_current_edit_position,      AT_CURRENT_EDIT_POSITION,             "current edit position indicator for 'edpos' args");

  XEN_DEFINE_PROCEDURE(S_make_sample_reader,        g_make_sample_reader_w, 0, 5, 0,        H_make_sample_reader);
  XEN_DEFINE_PROCEDURE(S_make_region_sample_reader, g_make_region_sample_reader_w, 0, 4, 0, H_make_region_sample_reader);
  XEN_DEFINE_PROCEDURE(S_read_sample,               g_read_sample_w, 1, 0, 0,               H_read_sample);
  XEN_DEFINE_PROCEDURE(S_next_sample,               g_next_sample_w, 1, 0, 0,               H_next_sample);
  XEN_DEFINE_PROCEDURE(S_previous_sample,           g_previous_sample_w, 1, 0, 0,           H_previous_sample);
  XEN_DEFINE_PROCEDURE(S_free_sample_reader,        g_free_sample_reader_w, 1, 0, 0,        H_free_sample_reader);
  XEN_DEFINE_PROCEDURE(S_sample_reader_home,        g_sample_reader_home_w, 1, 0, 0,        H_sample_reader_home);
  XEN_DEFINE_PROCEDURE(S_sample_reader_p,           g_sf_p_w, 1, 0, 0,                      H_sf_p);
  XEN_DEFINE_PROCEDURE(S_sample_reader_at_end_p,    g_sample_reader_at_end_w, 1, 0, 0,      H_sample_reader_at_end);
  XEN_DEFINE_PROCEDURE(S_sample_reader_position,    g_sample_reader_position_w, 1, 0, 0,    H_sample_reader_position);
  XEN_DEFINE_PROCEDURE("inspect-sample-reader",     g_inspect_sample_reader_w, 1, 0, 0,     "internal debugging function");

  XEN_DEFINE_PROCEDURE(S_save_edit_history,         g_save_edit_history_w, 1, 2, 0,         H_save_edit_history);
  XEN_DEFINE_PROCEDURE(S_edit_fragment,             g_edit_fragment_w, 0, 3, 0,             H_edit_fragment);
  XEN_DEFINE_PROCEDURE(S_undo,                      g_undo_w, 0, 3, 0,                      H_undo);
#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE("undo_edit",                 g_undo_w, 0, 3, 0,                      H_undo);
#endif
  XEN_DEFINE_PROCEDURE(S_redo,                      g_redo_w, 0, 3, 0,                      H_redo);
  XEN_DEFINE_PROCEDURE(S_as_one_edit,               g_as_one_edit_w, 1, 1, 0,               H_as_one_edit);
  XEN_DEFINE_PROCEDURE(S_display_edits,             g_display_edits_w, 0, 4, 0,             H_display_edits);
  XEN_DEFINE_PROCEDURE(S_edit_tree,                 g_edit_tree_w, 0, 3, 0,                 H_edit_tree);

  XEN_DEFINE_PROCEDURE(S_delete_sample,             g_delete_sample_w, 1, 3, 0,             H_delete_sample);
  XEN_DEFINE_PROCEDURE(S_delete_samples,            g_delete_samples_w, 2, 3, 0,            H_delete_samples);
  XEN_DEFINE_PROCEDURE(S_insert_sample,             g_insert_sample_w, 2, 3, 0,             H_insert_sample);
  XEN_DEFINE_PROCEDURE(S_insert_samples,            g_insert_samples_w, 3, 4, 0,            H_insert_samples);
  XEN_DEFINE_PROCEDURE(S_vct2samples,               g_vct2samples_w, 1, 7, 0,               H_vct2samples);
  XEN_DEFINE_PROCEDURE(S_vct2channel,               g_vct2channel_w, 1, 5, 0,               H_vct2channel);
  XEN_DEFINE_PROCEDURE(S_samples2vct,               g_samples2vct_w, 0, 6, 0,               H_samples2vct);
  XEN_DEFINE_PROCEDURE(S_channel2vct,               g_channel2vct_w, 0, 5, 0,               H_channel2vct);
  XEN_DEFINE_PROCEDURE(S_insert_sound,              g_insert_sound_w, 1, 6, 0,              H_insert_sound);
  XEN_DEFINE_PROCEDURE(S_scale_sound_by,            g_scale_sound_by_w, 1, 5, 0,            H_scale_sound_by);
  XEN_DEFINE_PROCEDURE(S_scale_channel,             g_scale_channel_w, 1, 5, 0,             H_scale_channel);
  XEN_DEFINE_PROCEDURE(S_scale_sound_to,            g_scale_sound_to_w, 1, 4, 0,            H_scale_sound_to);

  /* semi-internal functions (restore-state) */
  XEN_DEFINE_PROCEDURE("section-scale-by",           g_scale_sound_by_w, 1, 5, 0,           "internal scaling function used in save-state");
  XEN_DEFINE_PROCEDURE(S_change_samples_with_origin, g_change_samples_with_origin_w, 4, 3, 0, "internal function used in save-state");
  XEN_DEFINE_PROCEDURE(S_delete_samples_with_origin, g_delete_samples_with_origin_w, 3, 3, 0, "internal function used in save-state");
  XEN_DEFINE_PROCEDURE(S_insert_samples_with_origin, g_insert_samples_with_origin_w, 4, 3, 0, "internal function used in save-state");

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sample, g_sample_w, H_sample,
					    S_setB S_sample, g_set_sample_w, g_set_sample_reversed, 0, 4, 1, 4);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_samples, g_samples_w, H_samples,
					    S_setB S_samples, g_set_samples_w, g_set_samples_reversed, 0, 5, 3, 7);
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("set-samples", g_set_samples_w, 3, 7, 0, H_samples);
#endif

  #define H_save_hook S_save_hook " (snd name): called each time a file is about to be saved. \
If it returns #t, the file is not saved.  'name' is #f unless the file is being saved under a new name (as in sound-save-as)."

  XEN_DEFINE_HOOK(save_hook, S_save_hook, 2, H_save_hook);      /* arg = sound index, possible new name */

  #define H_save_state_hook S_save_state_hook " (temp-filename): called each time the save-state \
mechanism is about to create a new temporary file to save some edit history sample values. \
temp-filename is the current file. \
If the hook returns a string, it is treated as the new temp filename.  This hook provides a way to \
keep track of which files are in a given saved state batch, and a way to rename or redirect those files."

  XEN_DEFINE_HOOK(save_state_hook, S_save_state_hook, 1, H_save_state_hook);      /* arg = temp-filename */
  empty_closure = make_vct(1, (Float *)CALLOC(1, sizeof(Float)));
  snd_protect(empty_closure);
}

/* from Anders:

How to have the trees displayed in a meaningful manner,
especially related to common-music type work?  Maybe the edits
could be given optional names?  ie. "inversed", "prolongue-1",
"scale-1" etc. - and a more specialised graphing system put
together.

If all this is based on closures, doing "Select-All", should it
write out a temp-file of the state as now?  And how to import it
into some arbitrary place in the tree again?  Would pasting it in
be analogous to take a certain stage of the edit-history and
append the rest?
*/

/* 
 * Another possibility: mix as virtual op, ED_MIX would have two readers etc.
 *  this could be done perhaps with ptree-channel: channel-amp-envs for mixed data peaks,
 *    init pos: set up reader for mixed data (or peak-envs)
 *    need mix-amp etc (mix-speed is problematic)
 *    release of mix sample-reader needs to know when closure is unprotected (snd-unprotect-hook? a guardian? snd-protect|unprotect?)
 *    if the last, how to know when to unprotect -- need to know that a given edit list (ptree loc?) is being flushed.
 *    for "small-enough" mixes, the data could be stored in the vct along with a current-loc ptr
 *    how to tie into existing mix-tag support?
 */

/* and ED_REVERSE -- split using pos, no higher cases, peak-env needs check of trailing junk at end
 *  this is implemented via xen-channel in extsnd.html (does that code work for sections?)
 */

/* 
      ramp4, ramp5, ramp6 (these require rmp beg/end in fragment, cur/incr in snd_fd)
      same on ptree[zero]
*/
