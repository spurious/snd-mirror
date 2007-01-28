#include "snd.h"

static XEN save_hook;
static bool dont_save(snd_info *sp, const char *newname)
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(save_hook))
    res = run_or_hook(save_hook,
		      XEN_LIST_2(C_TO_XEN_INT(sp->index),
				 (newname) ? C_TO_XEN_STRING(newname) : XEN_FALSE),
		      S_save_hook);
  return(XEN_TRUE_P(res));
}

void after_edit(chan_info *cp)
{
  reflect_edit_history_change(cp);
  reflect_enved_spectra_change(cp);
  if ((XEN_HOOK_P(cp->after_edit_hook)) && (XEN_HOOKED(cp->after_edit_hook)))
    run_hook(cp->after_edit_hook, XEN_EMPTY_LIST, S_after_edit_hook);
}

void free_sound_list(chan_info *cp)
{
  if (cp)
    {
      if (cp->sounds)
	{
	  int i;
	  if ((cp->sound) && (cp->sound->playing)) stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
	  for (i = 0; i < cp->sound_size; i++)
	    if (cp->sounds[i]) 
	      cp->sounds[i] = free_snd_data(cp->sounds[i]);
	  FREE(cp->sounds);
	  cp->sounds = NULL;
	}
      cp->sound_ctr = NOT_A_SOUND;
      cp->sound_size = 0;
    }
}

static void release_pending_sounds(chan_info *cp, int edit_ctr)
{
  /* look for buffers or temp files that are no longer reachable after pruning the edit tree */
  if ((cp) && (cp->sounds))
    {
      int i;
      if ((cp->sound) && (cp->sound->playing)) 
	stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
      for (i = 0; i < cp->sound_size; i++)
	{
	  snd_data *sf;
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

static void prepare_sound_list(chan_info *cp)
{
  cp->sound_ctr++;
  /* this is the only place the sound set is incremented */
  if (cp->sound_ctr >= cp->sound_size)
    {
      int i;
      cp->sound_size += EDIT_ALLOC_SIZE;
      cp->sounds = (snd_data **)REALLOC(cp->sounds, cp->sound_size * sizeof(snd_data *));
      for (i = cp->sound_ctr; i < cp->sound_size; i++) cp->sounds[i] = NULL;
    }
  if (cp->sounds[cp->sound_ctr]) 
    {
      if ((cp->sound) && (cp->sound->playing)) stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
      cp->sounds[cp->sound_ctr] = free_snd_data(cp->sounds[cp->sound_ctr]);
    }
}

static int add_sound_file_to_edit_list(chan_info *cp, const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int chan)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_file(name, io, hdr, temp, cp->edit_ctr, chan);
  return(cp->sound_ctr);
}

void free_ptree_list(chan_info *cp)
{
  if (cp->ptrees)
    {
      int i;
      for (i = 0; i < cp->ptree_size; i++)
 	{
	  if (cp->ptrees[i])
	    {
	      free_ptree(cp->ptrees[i]);
	      cp->ptrees[i] = NULL;
	    }
	  if (XEN_PROCEDURE_P(cp->ptree_inits[i]))
	    snd_unprotect_at(cp->init_locs[i]);
	}
      FREE(cp->ptrees);
      cp->ptrees = NULL;
      FREE(cp->ptree_inits);
      cp->ptree_inits = NULL;
      FREE(cp->init_locs);
      cp->init_locs = NULL;
    }
  cp->ptree_ctr = -1;
  cp->ptree_size = 0;
}

static int add_ptree(chan_info *cp)
{
  int i;
  if (cp->ptrees)
    for (i = 0; i <= cp->ptree_ctr; i++)
      if (cp->ptrees[i] == NULL)
	return(i);
  cp->ptree_ctr++;
  if (cp->ptree_ctr >= cp->ptree_size)
    {
      cp->ptree_size += EDIT_ALLOC_SIZE;
      if (cp->ptrees)
	{
	  cp->ptrees = (struct ptree **)REALLOC(cp->ptrees, cp->ptree_size * sizeof(struct ptree *));
	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->ptrees[i] = NULL;
 	  cp->ptree_inits = (XEN *)REALLOC(cp->ptree_inits, cp->ptree_size * sizeof(XEN));
 	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->ptree_inits[i] = XEN_FALSE;
 	  cp->init_locs = (int *)REALLOC(cp->init_locs, cp->ptree_size * sizeof(int));
 	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->init_locs[i] = -1;
 	}
       else 
 	{
 	  cp->ptrees = (struct ptree **)CALLOC(cp->ptree_size, sizeof(struct ptree *));
 	  cp->ptree_inits = (XEN *)CALLOC(cp->ptree_size, sizeof(XEN));
 	  for (i = 0; i < cp->ptree_size; i++) cp->ptree_inits[i] = XEN_FALSE;
 	  cp->init_locs = (int *)CALLOC(cp->ptree_size, sizeof(int));
 	  for (i = 0; i < cp->ptree_size; i++) cp->init_locs[i] = -1;
	}
    }
  return(cp->ptree_ctr);
}

static ed_list *free_ed_list(ed_list *ed, chan_info *cp);

static void prune_edits(chan_info *cp, int edpt)
{
  if (cp->edits[edpt]) 
    {
      int i;
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, edpt - 1);
      for (i = edpt; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = free_ed_list(cp->edits[i], cp);
	  cp->amp_envs[i] = free_amp_env(cp, i);
	  cp->tracks[i] = free_track_info(cp, i);
	}
      release_pending_marks(cp, edpt);
      release_pending_mixes(cp, edpt);
      release_pending_sounds(cp, edpt);
      release_dangling_readers(cp, edpt);
      release_dangling_enved_spectra(cp, edpt);
    }
}

bool editable_p(chan_info *cp)
{
  if (!(cp->editable)) return(false);
  if ((!(cp->edit_hook_checked)) &&
      (XEN_HOOK_P(cp->edit_hook)) &&
      (XEN_HOOKED(cp->edit_hook)))
    {
      XEN res;
      res = run_or_hook(cp->edit_hook, XEN_EMPTY_LIST, S_edit_hook);
      if (XEN_TRUE_P(res)) 
	return(false);
    }
  cp->edit_hook_checked = true;
  return(true);
}

static bool prepare_edit_list(chan_info *cp, off_t len, int pos, const char *caller)
{
  /* pos is the edit position the current edit is referring to --
   *   we normally can't set up an edit list entry that refers to a situation
   *   that will be clobbered by prune_edits below
   */
  snd_info *sp;
  if (!(cp->editable)) return(false);
  if (pos > cp->edit_ctr)
    {
      cp->edit_hook_checked = false;
      XEN_ERROR(NO_SUCH_EDIT,
		XEN_LIST_3(C_TO_XEN_STRING(caller),
			   C_TO_XEN_STRING("edpos: ~A, ~A chan ~A has ~A edits"),
			   XEN_LIST_4(C_TO_XEN_INT(pos),
				      C_TO_XEN_STRING(cp->sound->short_filename),
				      C_TO_XEN_INT(cp->chan),
				      C_TO_XEN_INT(cp->edit_ctr))));
    }
  if ((!(cp->edit_hook_checked)) &&
      (XEN_HOOK_P(cp->edit_hook)) &&
      (XEN_HOOKED(cp->edit_hook)))
    {
      XEN res;
      res = run_or_hook(cp->edit_hook, XEN_EMPTY_LIST, S_edit_hook);
      if (XEN_TRUE_P(res)) 
	return(false);
    }
  sp = cp->sound;
  stop_amp_env(cp);
  if ((sp) && (sp->playing)) stop_playing_sound_without_hook(sp, PLAY_EDIT);
  cp->edit_ctr++;
  if (cp->edit_ctr >= cp->edit_size)
    {
      int i;
      cp->edit_size += EDIT_ALLOC_SIZE;
      if (!cp->edits) cp->edits = (ed_list **)CALLOC(cp->edit_size, sizeof(ed_list *));
      else cp->edits = (ed_list **)REALLOC(cp->edits, cp->edit_size * sizeof(ed_list *));
      if (!cp->samples) cp->samples = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
      else cp->samples = (off_t *)REALLOC(cp->samples, cp->edit_size * sizeof(off_t));
      if (!cp->cursors) cp->cursors = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
      else cp->cursors = (off_t *)REALLOC(cp->cursors, cp->edit_size * sizeof(off_t));
      if (!(cp->amp_envs)) cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
      else cp->amp_envs = (env_info **)REALLOC(cp->amp_envs, cp->edit_size * sizeof(env_info *));
      if (!(cp->tracks)) cp->tracks = (track_info **)CALLOC(cp->edit_size, sizeof(track_info *));
      else cp->tracks = (track_info **)REALLOC(cp->tracks, cp->edit_size * sizeof(track_info *));
      for (i = cp->edit_ctr; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = NULL; 
	  cp->amp_envs[i] = NULL; 
	  cp->tracks[i] = NULL; 
	  cp->samples[i] = 0;
	  cp->cursors[i] = 0;
	}
    }
  prune_edits(cp, cp->edit_ctr);
  CURRENT_SAMPLES(cp) = len;
  if (cp->edit_ctr > 0)
    CURSOR(cp) = cp->cursors[cp->edit_ctr - 1];
  record_initial_track_info(cp);
  return(true);
}

static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap)
    {
      off_t samps;
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
	  else 
	    {
	      if (ap->default_xlabel)
		ap->xlabel = copy_string(ap->default_xlabel);
	      else ap->xlabel = copy_string(_("time"));
	    }
	}
      set_x_bounds(ap);
    }
}

void reflect_file_change_in_label(chan_info *cp)
{
  snd_info *sp;
  if (cp->edit_ctr == 0) return;
  sp = cp->sound;
  if (sp->sgx == NULL) return;
  if (!(cp->squelch_update))
    {
      char *starred_name;
      int len;
      len = strlen(shortname(sp)) + 16;
      starred_name = (char *)CALLOC(len, sizeof(char));
      strcpy(starred_name, shortname_indexed(sp));
      if (sp->user_read_only || sp->file_read_only)
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
      reflect_file_change_in_label(cp);
    }
}

static XEN save_state_hook;

char *run_save_state_hook(char *file)
{
  char *filename;
  filename = copy_string(file);
  if (XEN_HOOKED(save_state_hook))
    {
      XEN result = XEN_FALSE;
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
 *
 * (3.2 GHz):          -ffast-math    (int)  (double)
 *    straight     21    20           18       26
 *    ramp         24    21           20       26
 *    ramp2        27    22           21       27
 *    ramp3        38    24           22       29
 *    ramp4        35    44           24       30
 *    ptree-zero   46    47           48       61
 *    ptree        52    58           53       71
 *    ptreec       80    80           76       93
 *    ptree2       105  100          102      107
 *    ptree3       129  125          122      157
 *    xramp        152   94           86      140 (91)
 *    xramp2       290  148          149      242 (154)
 *    cosine       304  270          284      355 (290)
 */

typedef struct ed_fragment {
  off_t out,                               /* running segment location within current overall edited data */
        beg,                               /* index into the associated data => start point of data used in current segment */
        end;                               /* index into the associated data => end point of data used in current segment */
  Float scl,                               /* segment scaler */
        rmp0,                              /* first val of ramp */
        rmp1,                              /* end val of ramp */
        rmp2, rmp3,                        /* ramp2 vals */
        rmp4, rmp5,                        /* ramp3 vals */
        rmp6, rmp7,                        /* ramp4 vals */
        ptree_scl, ptree_scl2, ptree_scl3, /* scales the arg to the ptree */
        scaler,                            /* exp-env segment scaler */
        offset,                            /* exp-env segment offset */
        scaler2, offset2;
  int   snd,                               /* either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END|ZERO_MARK */
        typ,                               /* code for accessor choice (ED_SIMPLE etc) */
        ptree_loc,                         /* index into the cp->ptrees array */
        ptree_loc2, ptree_loc3;
  off_t ptree_pos,                         /* segment position within original at time of ptree edit */
        ptree_dur,                         /* original (unfragmented) segment length */
        ptree_pos2, ptree_dur2, ptree_pos3, ptree_dur3;
} ed_fragment;

/* when editing long edit lists, it might be faster to copy the current one if
 *   we used arrays of structs here, rather than arrays of pointers to structs
 */

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
#define FRAGMENT_RAMP4_BEG(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp6
#define FRAGMENT_RAMP4_END(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->rmp7
#define FRAGMENT_XRAMP_SCALER(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->scaler
#define FRAGMENT_XRAMP_OFFSET(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->offset
#define FRAGMENT_XRAMP_SCALER2(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->scaler2
#define FRAGMENT_XRAMP_OFFSET2(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->offset2
#define FRAGMENT_PTREE_SCALER(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_scl
#define FRAGMENT_PTREE2_SCALER(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_scl2
#define FRAGMENT_PTREE3_SCALER(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_scl3
#define FRAGMENT_PTREE_INDEX(Ed, Pos)      ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_loc
#define FRAGMENT_PTREE2_INDEX(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_loc2
#define FRAGMENT_PTREE3_INDEX(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_loc3
#define FRAGMENT_PTREE_DUR(Ed, Pos)        ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_dur
#define FRAGMENT_PTREE2_DUR(Ed, Pos)       ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_dur2
#define FRAGMENT_PTREE3_DUR(Ed, Pos)       ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_dur3
#define FRAGMENT_PTREE_POSITION(Ed, Pos)   ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_pos
#define FRAGMENT_PTREE2_POSITION(Ed, Pos)  ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_pos2
#define FRAGMENT_PTREE3_POSITION(Ed, Pos)  ((ed_fragment **)((Ed)->fragments))[Pos]->ptree_pos3
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
#define READER_RAMP4_BEG(Sf)        ((ed_fragment *)((Sf)->cb))->rmp6
#define READER_RAMP4_END(Sf)        ((ed_fragment *)((Sf)->cb))->rmp7
#define READER_XRAMP_SCALER(Sf)     ((ed_fragment *)((Sf)->cb))->scaler
#define READER_XRAMP_OFFSET(Sf)     ((ed_fragment *)((Sf)->cb))->offset
#define READER_XRAMP_SCALER2(Sf)    ((ed_fragment *)((Sf)->cb))->scaler2
#define READER_XRAMP_OFFSET2(Sf)    ((ed_fragment *)((Sf)->cb))->offset2
#define READER_PTREE_SCALER(Sf)     ((ed_fragment *)((Sf)->cb))->ptree_scl
#define READER_PTREE2_SCALER(Sf)    ((ed_fragment *)((Sf)->cb))->ptree_scl2
#define READER_PTREE3_SCALER(Sf)    ((ed_fragment *)((Sf)->cb))->ptree_scl3
#define READER_PTREE_INDEX(Sf)      ((ed_fragment *)((Sf)->cb))->ptree_loc
#define READER_PTREE2_INDEX(Sf)     ((ed_fragment *)((Sf)->cb))->ptree_loc2
#define READER_PTREE3_INDEX(Sf)     ((ed_fragment *)((Sf)->cb))->ptree_loc3
#define READER_PTREE_DUR(Sf)        ((ed_fragment *)((Sf)->cb))->ptree_dur
#define READER_PTREE2_DUR(Sf)       ((ed_fragment *)((Sf)->cb))->ptree_dur2
#define READER_PTREE3_DUR(Sf)       ((ed_fragment *)((Sf)->cb))->ptree_dur3
#define READER_PTREE_POSITION(Sf)   ((ed_fragment *)((Sf)->cb))->ptree_pos
#define READER_PTREE2_POSITION(Sf)  ((ed_fragment *)((Sf)->cb))->ptree_pos2
#define READER_PTREE3_POSITION(Sf)  ((ed_fragment *)((Sf)->cb))->ptree_pos3

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
#define ED_RAMP4_BEG(Ed)        (Ed)->rmp6
#define ED_RAMP4_END(Ed)        (Ed)->rmp7
#define ED_XRAMP_SCALER(Ed)     (Ed)->scaler
#define ED_XRAMP_OFFSET(Ed)     (Ed)->offset
#define ED_XRAMP_SCALER2(Ed)    (Ed)->scaler2
#define ED_XRAMP_OFFSET2(Ed)    (Ed)->offset2
#if 0
#define ED_PTREE_SCALER(Ed)     (Ed)->ptree_scl
#define ED_PTREE2_SCALER(Ed)    (Ed)->ptree_scl2
#define ED_PTREE3_SCALER(Ed)    (Ed)->ptree_scl3
#define ED_PTREE_INDEX(Ed)      (Ed)->ptree_loc
#define ED_PTREE2_INDEX(Ed)     (Ed)->ptree_loc2
#define ED_PTREE3_INDEX(Ed)     (Ed)->ptree_loc3
#define ED_PTREE_DUR(Ed)        (Ed)->ptree_dur
#define ED_PTREE2_DUR(Ed)       (Ed)->ptree_dur2
#define ED_PTREE3_DUR(Ed)       (Ed)->ptree_dur3
#define ED_LENGTH(Ed)           (ED_LOCAL_END(Ed) - ED_LOCAL_POSITION(Ed) + 1)
#define READER_LENGTH(Sf)       (READER_LOCAL_END(Sf) - READER_LOCAL_POSITION(Sf) + 1)
#endif
#define ED_PTREE_POSITION(Ed)   (Ed)->ptree_pos
#define ED_PTREE2_POSITION(Ed)  (Ed)->ptree_pos2
#define ED_PTREE3_POSITION(Ed)  (Ed)->ptree_pos3


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
 *   [back | forth] [normal | on_zero] -> [mus_sample_t | float]
 * on_zero case only needed if input 0 doesn't imply output 0
 *
 * to simplify reordering, ramps 1-4 use incrs 1-4, but xramp1 uses 4, then xramp2 uses 3
 */

/* envelope handlers */

static Float next_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1;
  sf->curval1 += sf->incr1;
  return(val);
}

static Float previous_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1;
  sf->curval1 -= sf->incr1;
  return(val);
}

static Float next_ramp2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2;
  sf->curval1 += sf->incr1;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_ramp2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2;
  sf->curval1 -= sf->incr1;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp2_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp2_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_ramp2_3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval3 * sf->curval4;
  sf->curval3 += sf->incr3;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_ramp2_3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval3 * sf->curval4;
  sf->curval3 -= sf->incr3;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_ramp1_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2;
  sf->curval2 += sf->incr2;
  return(val);
}

static Float previous_ramp1_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2;
  sf->curval2 -= sf->incr2;
  return(val);
}

static Float next_ramp1_3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval3;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp1_4_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval4;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_ramp1_4_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval4;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_ramp1_3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval3;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_ramp3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2 * sf->curval3;
  sf->curval1 += sf->incr1;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_ramp3_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2 * sf->curval3;
  sf->curval1 -= sf->incr1;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_ramp3_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3 * sf->curval4;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_ramp3_2_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval2 * sf->curval3 * sf->curval4;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_ramp4_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2 * sf->curval3 * sf->curval4;
  sf->curval1 += sf->incr1;
  sf->curval2 += sf->incr2;
  sf->curval3 += sf->incr3;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_ramp4_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * sf->curval2 * sf->curval3 * sf->curval4;
  sf->curval1 -= sf->incr1;
  sf->curval2 -= sf->incr2;
  sf->curval3 -= sf->incr3;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_xramp_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_xramp1_2_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
  sf->curval3 += sf->incr3;
  return(val);
}

static Float previous_xramp1_2_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
  sf->curval3 -= sf->incr3;
  return(val);
}

static Float next_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval1 += sf->incr1;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_xramp_ramp_to_float(snd_fd *sf)
{
  Float val;
  val = sf->curval1 * (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval1 -= sf->incr1;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4))) *
    (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
  sf->curval3 += sf->incr3;
  sf->curval4 += sf->incr4;
  return(val);
}

static Float previous_xramp2_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4))) *
    (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
  sf->curval3 -= sf->incr3;
  sf->curval4 -= sf->incr4;
  return(val);
}

static Float next_xramp2_ramp_to_float(snd_fd *sf) {return(next_ramp_to_float(sf) * next_xramp2_to_float(sf));}
static Float previous_xramp2_ramp_to_float(snd_fd *sf) {return(previous_ramp_to_float(sf) * previous_xramp2_to_float(sf));}
static Float next_xramp2_ramp2_to_float(snd_fd *sf) {return(next_ramp2_to_float(sf) * next_xramp2_to_float(sf));}
static Float previous_xramp2_ramp2_to_float(snd_fd *sf) {return(previous_ramp2_to_float(sf) * previous_xramp2_to_float(sf));}

static Float next_xramp_ramp2_to_float(snd_fd *sf) {return(next_ramp2_to_float(sf) * next_xramp_to_float(sf));}
static Float previous_xramp_ramp2_to_float(snd_fd *sf) {return(previous_ramp2_to_float(sf) * previous_xramp_to_float(sf));}
static Float next_xramp_ramp3_to_float(snd_fd *sf) {return(next_ramp3_to_float(sf) * next_xramp_to_float(sf));}
static Float previous_xramp_ramp3_to_float(snd_fd *sf) {return(previous_ramp3_to_float(sf) * previous_xramp_to_float(sf));}

static mus_sample_t previous_sound(snd_fd *sf);
static Float previous_sound_as_float(snd_fd *sf);
static mus_sample_t next_sound(snd_fd *sf);
static Float next_sound_as_float(snd_fd *sf);


static mus_sample_t end_sample(snd_fd *ignore) {return(MUS_SAMPLE_0);}
static Float end_sample_to_float(snd_fd *ignore) {return(0.0);}


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
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(sf->data[sf->loc++] * sf->fscaler);
}

static Float previous_sample_to_float(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(sf->data[sf->loc--] * sf->fscaler);
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


static mus_sample_t next_sample_unscaled(snd_fd *sf) {if (sf->loc > sf->last) return(next_sound(sf)); else return(sf->data[sf->loc++]);}
static mus_sample_t previous_sample_unscaled(snd_fd *sf) {if (sf->loc < sf->first) return(previous_sound(sf)); else return(sf->data[sf->loc--]);}
static Float next_sample_to_float_unscaled(snd_fd *sf) {if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(sf->data[sf->loc++]);}
static Float previous_sample_to_float_unscaled(snd_fd *sf) {if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(sf->data[sf->loc--]);}


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

static Float next_zero(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf));
  sf->loc++;
  return(0.0);
}

static Float previous_zero(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf));
  sf->loc--;
  return(0.0);
}


static mus_sample_t next_sample_with_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  else
    {
      mus_sample_t val;
      val = (mus_sample_t)(sf->data[sf->loc++] * sf->curval1);
      sf->curval1 += sf->incr1;
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
      val = (mus_sample_t)(sf->data[sf->loc--] * sf->curval1);
      sf->curval1 -= sf->incr1;
      return(val);
    }
}

static Float next_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else 
    {
      Float val;
      val = sf->data[sf->loc++] * sf->curval1 * MUS_FIX_TO_FLOAT;
      sf->curval1 += sf->incr1;
      return(val);
    }
}

static Float previous_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else
    {
      Float val;
      val = sf->data[sf->loc--] * sf->curval1 * MUS_FIX_TO_FLOAT;
      sf->curval1 -= sf->incr1;
      return(val);
    }
}

static mus_sample_t next_sample_with_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp2_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp2_to_float(sf)));
}

static Float next_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_ramp2_to_float(sf)));
}

static Float previous_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_ramp2_to_float(sf)));
}

static mus_sample_t next_sample_with_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp3_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp3_to_float(sf)));
}

static Float next_ramp3(snd_fd *sf) 
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_ramp3_to_float(sf)));
}

static Float previous_ramp3(snd_fd *sf) 
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_ramp3_to_float(sf)));
}

static mus_sample_t next_sample_with_ramp4(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc++] * next_ramp4_to_float(sf)));
}

static mus_sample_t previous_sample_with_ramp4(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf)); else return((mus_sample_t)(sf->data[sf->loc--] * previous_ramp4_to_float(sf)));
}

static Float next_ramp4(snd_fd *sf) 
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_ramp4_to_float(sf)));
}

static Float previous_ramp4(snd_fd *sf) 
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_ramp4_to_float(sf)));
}

static Float next_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp_to_float(sf)));
}

static Float previous_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp_to_float(sf)));
}

static Float next_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * next_xramp_ramp_to_float(sf)));
}

static Float previous_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * previous_xramp_ramp_to_float(sf)));
}

static Float next_xramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp2_to_float(sf)));
}

static Float previous_xramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp2_to_float(sf)));
}

static Float next_xrampn_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++] * (*(sf->rampf))(sf)));
}

static Float previous_xrampn_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--] * (*(sf->rev_rampf))(sf)));
}


/* ---------------- ptree ---------------- */

static Float next_pxramp_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval4 += sf->incr4;
  if (sf->xramp2)
    {
      val *= (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
      sf->curval3 += sf->incr3;
    }
  return(val);
}

static Float previous_pxramp_to_float(snd_fd *sf)
{
  Float val;
  val = (READER_XRAMP_OFFSET(sf) + (READER_XRAMP_SCALER(sf) * exp(sf->curval4)));
  sf->curval4 -= sf->incr4;
  if (sf->xramp2)
    {
      val *= (READER_XRAMP_OFFSET2(sf) + (READER_XRAMP_SCALER2(sf) * exp(sf->curval3)));
      sf->curval3 -= sf->incr3;
    }
  return(val);
}


static Float next_ptree_to_float(snd_fd *sf)
{
  Float val1 = 0.0;
  if (!(sf->zero))
    val1 = READER_PTREE_SCALER(sf) * sf->data[sf->loc];
  sf->loc++;
  val1 = evaluate_ptreec(sf->ptree1, val1, (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float previous_ptree_to_float(snd_fd *sf)
{
  Float val1 = 0.0;
  if (!(sf->zero))
    val1 = READER_PTREE_SCALER(sf) * sf->data[sf->loc];
  sf->loc--;
  val1 = evaluate_ptreec(sf->ptree1, val1, (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float next_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_ptree_to_float(sf));
}

static Float previous_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_ptree_to_float(sf));
}

static Float next_ptree_rampn_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, (*(sf->rampf))(sf) * sf->data[sf->loc++], (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float next_ptree_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ptree_rampn_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, (*(sf->rev_rampf))(sf) * sf->data[sf->loc--], (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float previous_ptree_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ptree_xramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc++] * READER_PTREE_SCALER(sf) * next_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float next_ptree_pxramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc++] * READER_PTREE_SCALER(sf) * next_pxramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float next_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_ptree_pxramp_to_float(sf));
}

static Float previous_ptree_xramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc--] * READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float previous_ptree_pxramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc--] * READER_PTREE_SCALER(sf) * previous_pxramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float previous_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_ptree_pxramp_to_float(sf));
}

static Float next_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return((*(sf->rampf))(sf) * next_ptree_to_float(sf));
}

static Float previous_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return((*(sf->rev_rampf))(sf) * previous_ptree_to_float(sf));
}

static Float next_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_pxramp_to_float(sf) * READER_SCALER(sf) * next_ptree_to_float(sf));
}

static Float previous_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_pxramp_to_float(sf) * READER_SCALER(sf) * previous_ptree_to_float(sf));
}

static Float next_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_pxramp_to_float(sf) * (*(sf->rampf))(sf) * next_ptree_to_float(sf));
}

static Float previous_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_pxramp_to_float(sf) * (*(sf->rev_rampf))(sf) * previous_ptree_to_float(sf));
}

static Float next_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp1_2_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp1_2_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp2_2_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp2_2_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ramp3_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp3_2_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp3_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp3_2_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp1_3_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp1_3_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ramp2_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp2_3_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp2_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp2_3_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_ramp_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp1_4_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_ramp_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp1_4_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return((*(sf->rampf))(sf) * next_ptree_pxramp_to_float(sf));
}

static Float previous_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return((*(sf->rev_rampf))(sf) * previous_ptree_pxramp_to_float(sf));
}

static Float next_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * next_ptree_xramp_to_float(sf));
}

static Float previous_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * previous_ptree_xramp_to_float(sf));
}

static Float next_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_pxramp_to_float(sf) * READER_SCALER(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_pxramp_to_float(sf) * READER_SCALER(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * next_pxramp_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * previous_pxramp_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_xramp_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * next_xramp_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_xramp_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * previous_xramp_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_xramp_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * next_ramp2_2_to_float(sf) * next_ptree_rampn_to_float(sf));
}

static Float previous_xramp_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) * previous_ramp2_2_to_float(sf) * previous_ptree_rampn_to_float(sf));
}

static Float next_xramp_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * (*(sf->rampf))(sf) * next_ptree_xramp_to_float(sf));
}

static Float previous_xramp_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * (*(sf->rev_rampf))(sf) * previous_ptree_xramp_to_float(sf));
}

static Float next_ptree_xrampn_rampn_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc++] * (*(sf->rampf))(sf), (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float previous_ptree_xrampn_rampn_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc--] * (*(sf->rev_rampf))(sf), (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float next_ptree_pxramp_ramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc++] * next_pxramp_to_float(sf) * next_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), true);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), true);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), true));
    }
  return(val1);
}

static Float previous_ptree_pxramp_ramp_to_float(snd_fd *sf)
{
  Float val1;
  val1 = evaluate_ptreec(sf->ptree1, sf->data[sf->loc--] * previous_pxramp_to_float(sf) * previous_ramp_to_float(sf), (vct *)XEN_OBJECT_REF(sf->closure1), false);
  if (sf->ptree2)
    {
      val1 = evaluate_ptreec(sf->ptree2, READER_PTREE2_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure2), false);
      if (sf->ptree3)
	return(evaluate_ptreec(sf->ptree3, READER_PTREE3_SCALER(sf) * val1, (vct *)XEN_OBJECT_REF(sf->closure3), false));
    }
  return(val1);
}

static Float next_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_ptree_pxramp_ramp_to_float(sf));
}

static Float previous_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_ptree_pxramp_ramp_to_float(sf));
}

static Float next_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp1_2_to_float(sf) * next_ptree_pxramp_ramp_to_float(sf));
}

static Float previous_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp1_2_to_float(sf) * previous_ptree_pxramp_ramp_to_float(sf));
}

static Float next_xramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound_as_float(sf)); 
  else return(next_xramp1_2_to_float(sf) * READER_SCALER(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_xramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound_as_float(sf)); 
  else return(previous_xramp1_2_to_float(sf) * READER_SCALER(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}

static Float next_ptree_xramp_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(READER_SCALER(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_ptree_xramp_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(READER_SCALER(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}

static Float next_ramp2_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp2_2_to_float(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_ramp2_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp2_2_to_float(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}

static Float next_ramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound_as_float(sf)); else return(next_ramp1_3_to_float(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_ramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound_as_float(sf)); else return(previous_ramp1_3_to_float(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}

static Float next_xramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound_as_float(sf)); 
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_xramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound_as_float(sf)); 
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}

static Float next_xramp_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound_as_float(sf)); 
  else return(next_ramp1_2_to_float(sf) * next_xramp1_2_to_float(sf) * next_ptree_xrampn_rampn_to_float(sf));
}

static Float previous_xramp_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound_as_float(sf)); 
  else return(previous_ramp1_2_to_float(sf) * previous_xramp1_2_to_float(sf) * previous_ptree_xrampn_rampn_to_float(sf));
}


/* ---------------- split ptree ---------------- */

static Float next_split_ptree2_to_float(snd_fd *sf, Float val1, Float val2)
{
  return(evaluate_ptreec(sf->ptree2, 
			 val1 * evaluate_ptreec(sf->ptree1, val2, (vct *)XEN_OBJECT_REF(sf->closure1), true), 
			 (vct *)XEN_OBJECT_REF(sf->closure2), true));
}

static Float next_split_ptree_arg(snd_fd *sf)
{
  Float val1 = 0.0;
  if (!(sf->zero))
    val1 = READER_PTREE_SCALER(sf) * sf->data[sf->loc];
  sf->loc++;
  return(val1);
}

static Float previous_split_ptree_arg(snd_fd *sf)
{
  Float val1 = 0.0;
  if (!(sf->zero))
    val1 = READER_PTREE_SCALER(sf) * sf->data[sf->loc];
  sf->loc--;
  return(val1);
}

static Float previous_split_ptree2_to_float(snd_fd *sf, Float val1, Float val2)
{
  return(evaluate_ptreec(sf->ptree2, 
			 val1 * evaluate_ptreec(sf->ptree1, val2, (vct *)XEN_OBJECT_REF(sf->closure1), false), 
			 (vct *)XEN_OBJECT_REF(sf->closure2), false));
}

static Float next_ptree_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), next_split_ptree_arg(sf)));
}

static Float previous_ptree_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), previous_split_ptree_arg(sf)));
}

static Float next_ptree_rampn_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_rampn_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_pxramp_to_float(sf), sf->data[sf->loc++] * (*(sf->rampf))(sf)));
}

static Float previous_ptree_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_pxramp_to_float(sf), sf->data[sf->loc--] * (*(sf->rev_rampf))(sf)));
}

static Float next_ptree_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), next_pxramp_to_float(sf) * next_split_ptree_arg(sf)));
}

static Float previous_ptree_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), previous_pxramp_to_float(sf) * previous_split_ptree_arg(sf)));
}

static Float next_rampn_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return((*(sf->rampf))(sf) * next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_pxramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_rampn_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return((*(sf->rev_rampf))(sf) * previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_pxramp_to_float(sf), 
								      previous_split_ptree_arg(sf)));
}

static Float next_xramp_ptree_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_pxramp_to_float(sf) * READER_SCALER(sf) *
	      next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ptree_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_pxramp_to_float(sf) * READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), previous_split_ptree_arg(sf)));
}

static Float next_ramp_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * next_split_ptree2_to_float(sf, next_pxramp_to_float(sf) * next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_ramp_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * previous_split_ptree2_to_float(sf, previous_pxramp_to_float(sf) * previous_ramp_to_float(sf), 
									     previous_split_ptree_arg(sf)));
}

static Float next_xramp_ptree_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * (*(sf->rampf))(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ptree_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * (*(sf->rev_rampf))(sf), previous_split_ptree_arg(sf)));
}

static Float next_rampn_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return((*(sf->rampf))(sf) * next_split_ptree2_to_float(sf, next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_rampn_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return((*(sf->rev_rampf))(sf) * previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_rampn_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return((*(sf->rampf))(sf) * next_split_ptree2_to_float(sf, next_ramp2_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_rampn_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return((*(sf->rev_rampf))(sf) * previous_split_ptree2_to_float(sf, previous_ramp2_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_ramp_ptree_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_4_to_float(sf) * next_split_ptree2_to_float(sf, next_ramp3_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_ramp_ptree_ramp3_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_4_to_float(sf) * previous_split_ptree2_to_float(sf, previous_ramp3_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_ptree_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp1_2_to_float(sf), next_xramp_to_float(sf) * next_split_ptree_arg(sf)));
}

static Float previous_ptree_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp1_2_to_float(sf), 
					     previous_xramp_to_float(sf) * previous_split_ptree_arg(sf)));
}

static Float next_ptree_rampn_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), sf->data[sf->loc++] * next_ramp2_to_float(sf)));
}

static Float previous_ptree_rampn_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), sf->data[sf->loc--] * previous_ramp2_to_float(sf)));
}

static Float next_ptree_ramp_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, next_ramp1_4_to_float(sf), next_ramp3_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_ramp_ptree_ramp3(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, previous_ramp1_4_to_float(sf), previous_ramp3_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_rampn_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return((*(sf->rampf))(sf) * next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_rampn_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return((*(sf->rev_rampf))(sf) * previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_4_to_float(sf) * next_split_ptree2_to_float(sf, next_ramp1_3_to_float(sf), next_ramp2_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_4_to_float(sf) * previous_split_ptree2_to_float(sf, previous_ramp1_3_to_float(sf), 
									     previous_ramp2_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, next_pxramp_to_float(sf) * next_ramp1_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, previous_pxramp_to_float(sf) * previous_ramp1_2_to_float(sf), 
					     previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp1_2_to_float(sf) * (*(sf->rampf))(sf), 
					 READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp1_2_to_float(sf) * (*(sf->rev_rampf))(sf), 
					     READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_pxramp_to_float(sf) * next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), 
					     previous_pxramp_to_float(sf) * previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ptree_xramp_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp1_2_to_float(sf), 
					 next_xramp_to_float(sf) * (*(sf->rampf))(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_ptree_xramp_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp1_2_to_float(sf), 
					     previous_xramp_to_float(sf) * (*(sf->rev_rampf))(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, (*(sf->rampf))(sf), READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_rampn_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, (*(sf->rev_rampf))(sf), READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_rampn_ptree_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return((*(sf->rampf))(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp1_2_to_float(sf), 
					 READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_rampn_ptree_xramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return((*(sf->rev_rampf))(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp1_2_to_float(sf), 
					     READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp_to_float(sf), READER_PTREE_SCALER(sf) * next_pxramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), READER_PTREE_SCALER(sf) * previous_pxramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_pxramp_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_pxramp_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), (*(sf->rampf))(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_xramp_ptree_rampn(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), (*(sf->rev_rampf))(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_pxramp_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_pxramp_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_pxramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_pxramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_ptree_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, next_pxramp_to_float(sf) * (*(sf->rampf))(sf), next_split_ptree_arg(sf)));
}

static Float previous_ptree_xramp_rampn_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, previous_pxramp_to_float(sf) * (*(sf->rev_rampf))(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ramp_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_pxramp_to_float(sf) * next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ramp_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_pxramp_to_float(sf) * previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ramp2_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_pxramp_to_float(sf) * next_ramp2_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ramp2_ptree_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_pxramp_to_float(sf) * previous_ramp2_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_rampn_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * (*(sf->rampf))(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_rampn_ptree_xramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * (*(sf->rev_rampf))(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ramp_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ramp_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * previous_ramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ramp_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp2_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_xramp_ramp_ptree_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) * previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp2_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_ramp_ptree_xramp_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * next_ramp2_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_ramp_ptree_xramp_ramp2_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * previous_ramp2_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_ramp2_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * next_ramp_to_float(sf), next_split_ptree_arg(sf)));
}

static Float previous_ramp2_ptree_xramp_ramp_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * previous_ramp_to_float(sf), previous_split_ptree_arg(sf)));
}

static Float next_xramp_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_3_to_float(sf), next_ramp2_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_3_to_float(sf), previous_ramp2_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp2_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp_to_float(sf), READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp2_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp2_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp2_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp2_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp2_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_4_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp2_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_4_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp2_2_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf) * next_xramp1_2_to_float(sf), next_xramp_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf) * previous_xramp1_2_to_float(sf), 
					     previous_xramp_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf) * next_xramp_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf) * previous_xramp_to_float(sf), 
					     previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * READER_SCALER(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf) * next_xramp_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_xramp_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * READER_SCALER(sf)  *
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf) * previous_xramp_to_float(sf), 
					     previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_xramp_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), previous_xramp_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_xramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp1_2_to_float(sf), next_xramp_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_xramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp1_2_to_float(sf), 
					     previous_xramp_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_xramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_xramp_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_ramp_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_xramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), previous_xramp_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp1_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ramp_ptree_ramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) * previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp1_2_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ramp_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp_to_float(sf), READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ramp_ptree_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp_to_float(sf), READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ramp_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp1_2_to_float(sf) * next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ramp_ptree_xramp_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp1_2_to_float(sf) * previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_xramp_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_xramp_to_float(sf) * READER_SCALER(sf) *
	      next_split_ptree2_to_float(sf, next_ramp2_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_xramp_ptree_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_xramp_to_float(sf) * READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, previous_ramp2_2_to_float(sf), previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * next_ramp2_2_to_float(sf), next_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_ramp2_ptree_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * previous_ramp2_2_to_float(sf), 
					     previous_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_xramp_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      next_split_ptree2_to_float(sf, next_xramp_to_float(sf) * next_ramp1_3_to_float(sf), next_ramp2_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_xramp_ramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, previous_xramp_to_float(sf) * previous_ramp1_3_to_float(sf), 
					     previous_ramp2_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_ramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      next_split_ptree2_to_float(sf, next_ramp1_3_to_float(sf), next_xramp_to_float(sf) * next_ramp2_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_ramp_ptree_xramp_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) *
	      previous_split_ptree2_to_float(sf, previous_ramp1_3_to_float(sf), 
					     previous_xramp_to_float(sf) * previous_ramp2_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ptree_ramp2_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(READER_SCALER(sf) * next_split_ptree2_to_float(sf, next_ramp2_2_to_float(sf), next_xramp_ramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ptree_ramp2_ptree_xramp_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(READER_SCALER(sf) * previous_split_ptree2_to_float(sf, previous_ramp2_2_to_float(sf), previous_xramp_ramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_ramp2_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_ramp2_to_float(sf), READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_ramp2_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_ramp2_to_float(sf), READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_xramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_3_to_float(sf) * 
	      next_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * next_xramp_to_float(sf), next_ramp2_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_xramp_ptree_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_3_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, READER_PTREE2_SCALER(sf) * previous_xramp_to_float(sf), previous_ramp2_to_float(sf) * sf->data[sf->loc--]));
}

static Float next_ramp_ptree_xramp_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound_as_float(sf));
  else return(next_ramp1_2_to_float(sf) * 
	      next_split_ptree2_to_float(sf, next_xramp1_2_to_float(sf) * next_ramp_to_float(sf), 
					 READER_PTREE_SCALER(sf) * next_xramp_to_float(sf) * sf->data[sf->loc++]));
}

static Float previous_ramp_ptree_xramp_ramp_ptree_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound_as_float(sf));
  else return(previous_ramp1_2_to_float(sf) * 
	      previous_split_ptree2_to_float(sf, previous_xramp1_2_to_float(sf) * previous_ramp_to_float(sf), 
					     READER_PTREE_SCALER(sf) * previous_xramp_to_float(sf) * sf->data[sf->loc--]));
}



/* -------------------------------------------------------------------------------- */
/* these are the virtual-op accessor choices 
   (ramp->xramp are flipped in many cases to reduce clutter, a*b=b*a so (a(b))=(b(a))) 
   ptree zero cases use accessor-internal switch sf->zero
   complex cases simply return float and use generic conversion to sample
*/

enum {ED_SIMPLE, ED_ZERO,
      
      /* pure (x)ramp ops */
      ED_RAMP, ED_RAMP2, ED_RAMP3, ED_RAMP4,
      ED_XRAMP, ED_XRAMP2, ED_XRAMP_RAMP, 
      ED_XRAMP_RAMP2, ED_XRAMP2_RAMP,
      ED_XRAMP_RAMP3, ED_XRAMP2_RAMP2,

      /* single ptree ops */
      ED_PTREE, ED_PTREE_RAMP, ED_PTREE_XRAMP, ED_PTREE_ZERO, ED_PTREE_RAMP2, ED_PTREE_RAMP3,
      ED_PTREE_XRAMP_RAMP, ED_PTREE_XRAMP2,
      ED_RAMP_PTREE, ED_RAMP_PTREE_ZERO, 
      ED_RAMP2_PTREE, ED_RAMP2_PTREE_ZERO, 
      ED_RAMP3_PTREE, ED_RAMP3_PTREE_ZERO,
      ED_XRAMP_PTREE, ED_XRAMP_PTREE_ZERO, 
      ED_XRAMP2_PTREE, ED_XRAMP2_PTREE_ZERO, 
      ED_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_RAMP, ED_RAMP_PTREE_RAMP2,
      ED_RAMP_PTREE_XRAMP, ED_XRAMP_PTREE_RAMP, ED_RAMP2_PTREE_XRAMP,
      ED_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_ZERO,
      ED_XRAMP_PTREE_XRAMP, 
      ED_XRAMP_RAMP2_PTREE, ED_XRAMP_RAMP2_PTREE_ZERO, 
      ED_XRAMP2_RAMP_PTREE, ED_XRAMP2_RAMP_PTREE_ZERO, 
      ED_PTREE_XRAMP2_RAMP, ED_PTREE_XRAMP_RAMP2, ED_XRAMP_PTREE_RAMP2, ED_XRAMP_RAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_XRAMP,
      ED_XRAMP2_PTREE_RAMP, ED_XRAMP_PTREE_XRAMP_RAMP, ED_RAMP_PTREE_XRAMP_RAMP, ED_RAMP_PTREE_XRAMP2, 

      ED_RAMP4_PTREE, ED_RAMP4_PTREE_ZERO, ED_PTREE_RAMP4,
      ED_RAMP3_PTREE_RAMP, ED_RAMP_PTREE_RAMP3,
      ED_RAMP2_PTREE_RAMP2, ED_RAMP3_PTREE_XRAMP, ED_XRAMP_PTREE_RAMP3,
      ED_RAMP2_PTREE_XRAMP2, ED_XRAMP2_PTREE_RAMP2,
      ED_XRAMP_RAMP_PTREE_XRAMP_RAMP, ED_XRAMP_PTREE_XRAMP_RAMP2, ED_RAMP_PTREE_XRAMP2_RAMP,
      ED_PTREE_XRAMP_RAMP3, ED_PTREE_XRAMP2_RAMP2,
      ED_XRAMP_RAMP3_PTREE, ED_XRAMP_RAMP3_PTREE_ZERO, ED_XRAMP2_RAMP2_PTREE, ED_XRAMP2_RAMP2_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE_RAMP2, ED_XRAMP_RAMP2_PTREE_RAMP, ED_RAMP_PTREE_XRAMP_RAMP2, ED_XRAMP_RAMP2_PTREE_XRAMP,
      ED_XRAMP2_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_XRAMP_RAMP,

      /* double ptree ops */
      ED_PTREE2, ED_PTREE2_RAMP, ED_PTREE2_XRAMP, ED_PTREE2_ZERO, ED_PTREE2_RAMP2, ED_PTREE2_RAMP3,
      ED_PTREE2_XRAMP_RAMP, ED_PTREE2_XRAMP2,
      ED_RAMP_PTREE2, ED_RAMP_PTREE2_ZERO, 
      ED_RAMP2_PTREE2, ED_RAMP2_PTREE2_ZERO, 
      ED_RAMP3_PTREE2, ED_RAMP3_PTREE2_ZERO,
      ED_XRAMP_PTREE2, ED_XRAMP_PTREE2_ZERO, 
      ED_XRAMP2_PTREE2, ED_XRAMP2_PTREE2_ZERO, 
      ED_RAMP_PTREE2_RAMP, ED_RAMP2_PTREE2_RAMP, ED_RAMP_PTREE2_RAMP2,
      ED_RAMP_PTREE2_XRAMP, ED_XRAMP_PTREE2_RAMP, ED_RAMP2_PTREE2_XRAMP,
      ED_XRAMP_RAMP_PTREE2, ED_XRAMP_RAMP_PTREE2_ZERO,
      ED_XRAMP_PTREE2_XRAMP, 
      ED_XRAMP_RAMP2_PTREE2, ED_XRAMP_RAMP2_PTREE2_ZERO, 
      ED_XRAMP2_RAMP_PTREE2, ED_XRAMP2_RAMP_PTREE2_ZERO, 
      ED_PTREE2_XRAMP2_RAMP, ED_PTREE2_XRAMP_RAMP2, ED_XRAMP_PTREE2_RAMP2, ED_XRAMP_RAMP_PTREE2_RAMP, ED_XRAMP_RAMP_PTREE2_XRAMP,
      ED_XRAMP2_PTREE2_RAMP, ED_XRAMP_PTREE2_XRAMP_RAMP, ED_RAMP_PTREE2_XRAMP_RAMP, ED_RAMP_PTREE2_XRAMP2, 

      ED_RAMP4_PTREE2, ED_RAMP4_PTREE2_ZERO, ED_PTREE2_RAMP4,
      ED_RAMP3_PTREE2_RAMP, ED_RAMP_PTREE2_RAMP3,
      ED_RAMP2_PTREE2_RAMP2, ED_RAMP3_PTREE2_XRAMP, ED_XRAMP_PTREE2_RAMP3,
      ED_RAMP2_PTREE2_XRAMP2, ED_XRAMP2_PTREE2_RAMP2,
      ED_XRAMP_RAMP_PTREE2_XRAMP_RAMP, ED_XRAMP_PTREE2_XRAMP_RAMP2, ED_RAMP_PTREE2_XRAMP2_RAMP,
      ED_PTREE2_XRAMP_RAMP3, ED_PTREE2_XRAMP2_RAMP2,
      ED_XRAMP_RAMP3_PTREE2, ED_XRAMP_RAMP3_PTREE2_ZERO, ED_XRAMP2_RAMP2_PTREE2, ED_XRAMP2_RAMP2_PTREE2_ZERO,
      ED_XRAMP_RAMP_PTREE2_RAMP2, ED_XRAMP_RAMP2_PTREE2_RAMP, ED_RAMP_PTREE2_XRAMP_RAMP2,
      ED_XRAMP_RAMP2_PTREE2_XRAMP, ED_XRAMP2_RAMP_PTREE2_RAMP, ED_RAMP2_PTREE2_XRAMP_RAMP, 

      /* double ptree split ops */
      ED_PTREE_RAMP2_PTREE, ED_PTREE_RAMP2_PTREE_ZERO, 
      ED_PTREE_RAMP_PTREE, ED_PTREE_RAMP_PTREE_ZERO, 
      ED_PTREE_RAMP3_PTREE, ED_PTREE_RAMP3_PTREE_ZERO,
      ED_PTREE_XRAMP_PTREE, ED_PTREE_XRAMP_PTREE_ZERO,
      ED_PTREE_RAMP2_PTREE_RAMP, ED_PTREE_RAMP_PTREE_RAMP2, ED_RAMP_PTREE_RAMP_PTREE_RAMP, ED_PTREE_RAMP_PTREE_RAMP,

      ED_RAMP2_PTREE_RAMP_PTREE, ED_RAMP2_PTREE_RAMP_PTREE_ZERO,
      ED_XRAMP_PTREE_RAMP_PTREE, ED_XRAMP_PTREE_RAMP_PTREE_ZERO,
      ED_RAMP_PTREE_XRAMP_PTREE, ED_RAMP_PTREE_XRAMP_PTREE_ZERO, 
      ED_PTREE_XRAMP_RAMP_PTREE, ED_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_PTREE_XRAMP_PTREE_RAMP,
      ED_PTREE_RAMP_PTREE_XRAMP,

      ED_RAMP_PTREE_RAMP2_PTREE, ED_RAMP_PTREE_RAMP2_PTREE_ZERO,
      ED_XRAMP_PTREE_RAMP2_PTREE, ED_XRAMP_PTREE_RAMP2_PTREE_ZERO,
      ED_RAMP2_PTREE_XRAMP_PTREE, ED_RAMP2_PTREE_XRAMP_PTREE_ZERO, 
      ED_PTREE_XRAMP_RAMP2_PTREE, ED_PTREE_XRAMP_RAMP2_PTREE_ZERO, 
      ED_PTREE_XRAMP2_RAMP_PTREE, ED_PTREE_XRAMP2_RAMP_PTREE_ZERO, 
      ED_PTREE_XRAMP_PTREE_RAMP2, ED_PTREE_RAMP2_PTREE_XRAMP,
      ED_PTREE_XRAMP2_PTREE, ED_PTREE_XRAMP2_PTREE_ZERO,
      ED_RAMP_PTREE_RAMP_PTREE, ED_RAMP_PTREE_RAMP_PTREE_ZERO,
      ED_XRAMP_PTREE_XRAMP_PTREE, ED_XRAMP_PTREE_XRAMP_PTREE_ZERO,
      ED_PTREE_XRAMP_PTREE_XRAMP, ED_RAMP_PTREE_XRAMP_RAMP_PTREE, ED_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO,
      ED_XRAMP_PTREE_XRAMP_RAMP_PTREE, ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_ZERO,
      ED_XRAMP2_PTREE_RAMP_PTREE, ED_XRAMP2_PTREE_RAMP_PTREE_ZERO,
      ED_RAMP_PTREE_XRAMP2_PTREE, ED_RAMP_PTREE_XRAMP2_PTREE_ZERO,
      ED_RAMP2_PTREE_RAMP2_PTREE, ED_RAMP2_PTREE_RAMP2_PTREE_ZERO,
      ED_RAMP_PTREE_RAMP3_PTREE, ED_RAMP_PTREE_RAMP3_PTREE_ZERO,
      ED_RAMP3_PTREE_RAMP_PTREE, ED_RAMP3_PTREE_RAMP_PTREE_ZERO,
      ED_PTREE_RAMP2_PTREE_RAMP2, ED_PTREE_RAMP_PTREE_RAMP3, ED_PTREE_RAMP3_PTREE_RAMP,

      ED_XRAMP_PTREE_RAMP_PTREE_RAMP, ED_XRAMP_PTREE_XRAMP_PTREE_RAMP, ED_RAMP_PTREE_XRAMP_PTREE_RAMP,
      ED_PTREE_RAMP_PTREE_XRAMP_RAMP, ED_PTREE_XRAMP_PTREE_XRAMP_RAMP, ED_PTREE_XRAMP_RAMP_PTREE_RAMP,
      ED_PTREE_XRAMP2_PTREE_RAMP, ED_PTREE_XRAMP_RAMP_PTREE_XRAMP, ED_RAMP_PTREE_RAMP_PTREE_XRAMP,
      ED_XRAMP_PTREE_RAMP_PTREE_XRAMP, ED_RAMP_PTREE_XRAMP_PTREE_XRAMP, ED_PTREE_RAMP_PTREE_XRAMP2,
      ED_XRAMP2_PTREE_RAMP2_PTREE, ED_XRAMP2_PTREE_RAMP2_PTREE_ZERO,
      ED_PTREE_XRAMP2_RAMP2_PTREE, ED_PTREE_XRAMP2_RAMP2_PTREE_ZERO,
      ED_PTREE_XRAMP2_PTREE_RAMP2, ED_PTREE_RAMP2_PTREE_XRAMP2,
      ED_RAMP_PTREE_XRAMP2_RAMP_PTREE, ED_RAMP_PTREE_XRAMP2_RAMP_PTREE_ZERO,
      ED_RAMP2_PTREE_XRAMP2_PTREE, ED_RAMP2_PTREE_XRAMP2_PTREE_ZERO, ED_RAMP_PTREE_RAMP_PTREE_XRAMP2,
      ED_RAMP_PTREE_XRAMP2_PTREE_RAMP, ED_XRAMP2_PTREE_RAMP_PTREE_RAMP, ED_PTREE_RAMP_PTREE_XRAMP2_RAMP, 
      ED_XRAMP2_RAMP_PTREE_RAMP_PTREE, ED_XRAMP2_RAMP_PTREE_RAMP_PTREE_ZERO,

      ED_RAMP2_PTREE_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_RAMP_PTREE_XRAMP, 
      ED_RAMP2_PTREE_XRAMP_PTREE_RAMP, ED_RAMP2_PTREE_XRAMP_PTREE_XRAMP,
      ED_RAMP2_PTREE_XRAMP_RAMP_PTREE, ED_RAMP2_PTREE_XRAMP_RAMP_PTREE_ZERO,
      ED_RAMP3_PTREE_XRAMP_PTREE, ED_RAMP3_PTREE_XRAMP_PTREE_ZERO,
      ED_RAMP_PTREE_RAMP2_PTREE_RAMP, ED_RAMP_PTREE_RAMP2_PTREE_XRAMP,
      ED_RAMP_PTREE_RAMP_PTREE_RAMP2, ED_RAMP_PTREE_RAMP_PTREE_XRAMP_RAMP,
      ED_RAMP_PTREE_XRAMP_PTREE_RAMP2, ED_RAMP_PTREE_XRAMP_PTREE_XRAMP_RAMP,
      ED_RAMP_PTREE_XRAMP_RAMP2_PTREE, ED_RAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO,
      ED_RAMP_PTREE_XRAMP_RAMP_PTREE_RAMP, ED_RAMP_PTREE_XRAMP_RAMP_PTREE_XRAMP,
      ED_XRAMP_PTREE_RAMP2_PTREE_RAMP, ED_XRAMP_PTREE_RAMP2_PTREE_XRAMP,
      ED_XRAMP_PTREE_RAMP3_PTREE, ED_XRAMP_PTREE_RAMP3_PTREE_ZERO,
      ED_XRAMP_PTREE_RAMP_PTREE_RAMP2, ED_XRAMP_PTREE_RAMP_PTREE_XRAMP_RAMP, ED_XRAMP_PTREE_XRAMP_PTREE_RAMP2,
      ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE, ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO, ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_RAMP,
      ED_XRAMP_RAMP2_PTREE_RAMP_PTREE, ED_XRAMP_RAMP2_PTREE_RAMP_PTREE_ZERO,
      ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE_RAMP2_PTREE, ED_XRAMP_RAMP_PTREE_RAMP2_PTREE_ZERO,
      ED_XRAMP_RAMP_PTREE_RAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_XRAMP, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_RAMP,
      ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO,
      ED_PTREE_XRAMP_RAMP3_PTREE, ED_PTREE_XRAMP_RAMP3_PTREE_ZERO,

      ED_PTREE_RAMP2_PTREE_XRAMP_RAMP, ED_PTREE_RAMP3_PTREE_XRAMP,
      ED_PTREE_RAMP4_PTREE, ED_PTREE_RAMP4_PTREE_ZERO,
      ED_PTREE_RAMP_PTREE_XRAMP_RAMP2, ED_PTREE_XRAMP2_RAMP_PTREE_RAMP, ED_PTREE_XRAMP_PTREE_RAMP3,
      ED_PTREE_XRAMP_PTREE_XRAMP_RAMP2, ED_PTREE_XRAMP_RAMP2_PTREE_RAMP, ED_PTREE_XRAMP_RAMP2_PTREE_XRAMP,
      ED_PTREE_XRAMP_RAMP_PTREE_RAMP2, ED_PTREE_XRAMP_RAMP_PTREE_XRAMP_RAMP,

      /* triple ptree ops */
      ED_PTREE3, ED_PTREE3_RAMP, ED_PTREE3_XRAMP, ED_PTREE3_ZERO, ED_PTREE3_RAMP2, ED_PTREE3_RAMP3,
      ED_PTREE3_XRAMP_RAMP, ED_PTREE3_XRAMP2,
      ED_RAMP_PTREE3, ED_RAMP_PTREE3_ZERO, 
      ED_RAMP2_PTREE3, ED_RAMP2_PTREE3_ZERO, 
      ED_RAMP3_PTREE3, ED_RAMP3_PTREE3_ZERO,
      ED_XRAMP_PTREE3, ED_XRAMP_PTREE3_ZERO, 
      ED_XRAMP2_PTREE3, ED_XRAMP2_PTREE3_ZERO, 
      ED_RAMP_PTREE3_RAMP, ED_RAMP2_PTREE3_RAMP, ED_RAMP_PTREE3_RAMP2,
      ED_RAMP_PTREE3_XRAMP, ED_XRAMP_PTREE3_RAMP, ED_RAMP2_PTREE3_XRAMP,
      ED_XRAMP_RAMP_PTREE3, ED_XRAMP_RAMP_PTREE3_ZERO,
      ED_XRAMP_PTREE3_XRAMP, 
      ED_XRAMP_RAMP2_PTREE3, ED_XRAMP_RAMP2_PTREE3_ZERO, 
      ED_XRAMP2_RAMP_PTREE3, ED_XRAMP2_RAMP_PTREE3_ZERO, 
      ED_PTREE3_XRAMP2_RAMP, ED_PTREE3_XRAMP_RAMP2, ED_XRAMP_PTREE3_RAMP2, ED_XRAMP_RAMP_PTREE3_RAMP, ED_XRAMP_RAMP_PTREE3_XRAMP,
      ED_XRAMP2_PTREE3_RAMP, ED_XRAMP_PTREE3_XRAMP_RAMP, ED_RAMP_PTREE3_XRAMP_RAMP, ED_RAMP_PTREE3_XRAMP2, 

      ED_RAMP4_PTREE3, ED_RAMP4_PTREE3_ZERO, ED_PTREE3_RAMP4,
      ED_RAMP3_PTREE3_RAMP, ED_RAMP_PTREE3_RAMP3,
      ED_RAMP2_PTREE3_RAMP2, ED_RAMP3_PTREE3_XRAMP, ED_XRAMP_PTREE3_RAMP3,
      ED_RAMP2_PTREE3_XRAMP2, ED_XRAMP2_PTREE3_RAMP2,
      ED_XRAMP_RAMP_PTREE3_XRAMP_RAMP, ED_XRAMP_PTREE3_XRAMP_RAMP2, ED_RAMP_PTREE3_XRAMP2_RAMP,
      ED_PTREE3_XRAMP_RAMP3, ED_PTREE3_XRAMP2_RAMP2,
      ED_XRAMP_RAMP3_PTREE3, ED_XRAMP_RAMP3_PTREE3_ZERO, ED_XRAMP2_RAMP2_PTREE3, ED_XRAMP2_RAMP2_PTREE3_ZERO,
      ED_XRAMP_RAMP_PTREE3_RAMP2, ED_XRAMP_RAMP2_PTREE3_RAMP, ED_RAMP_PTREE3_XRAMP_RAMP2,
      ED_XRAMP_RAMP2_PTREE3_XRAMP, ED_XRAMP2_RAMP_PTREE3_RAMP, ED_RAMP2_PTREE3_XRAMP_RAMP, 

      NUM_OPS
};

typedef struct {
  int type, add_ramp, add_xramp, add_ptree, ptrees, ramps, xramps;
  bool ptree_zero;
  char *name;
  Float (*next)(struct snd_fd *sf);  
  Float (*previous)(struct snd_fd *sf);  
  Float (*rampf)(struct snd_fd *sf);  
  Float (*rev_rampf)(struct snd_fd *sf);  
} fragment_type_info;

/* this is a kind of state machine for the virtual editor: if, for example, the current
     op is ed_ramp and we want to add an xramp, we look at the add_xramp field of 
     ed_ramp's type_info struct, and if it's not -1, the add_xramp field is the
     new virtual editor op; otherwise the op is collapsed to a change edit.
     So, to add a new entry, make the accessor, and fill in the fields that
     can reach it.  Since, for example, ramp->xramp is flipped to xramp->ramp
     since a(b()) == b(a()) in this case, there can be several ways to reach
     a given op. Then fix the ramp segments in choose_accessor.  The first
     struct field is redundant, but makes editing this table much easier. 
*/

static fragment_type_info type_info[NUM_OPS] = {
  {ED_SIMPLE, ED_RAMP, ED_XRAMP, ED_PTREE, 0, 0, 0, false, 
   "ed_simple", NULL, NULL, NULL, NULL},
  {ED_ZERO, ED_ZERO, ED_ZERO, ED_PTREE_ZERO, 0, 0, 0, true, 
   "ed_zero", next_zero, previous_zero, NULL, NULL},

  {ED_RAMP, ED_RAMP2, ED_XRAMP_RAMP, ED_PTREE_RAMP, 0, 1, 0, false, 
   "ed_ramp", next_ramp, previous_ramp, NULL, NULL},
  {ED_RAMP2, ED_RAMP3, ED_XRAMP_RAMP2, ED_PTREE_RAMP2, 0, 2, 0, false, 
   "ed_ramp2", next_ramp2, previous_ramp2, NULL, NULL},
  {ED_RAMP3, ED_RAMP4, ED_XRAMP_RAMP3, ED_PTREE_RAMP3, 0, 3, 0, false, 
   "ed_ramp3", next_ramp3, previous_ramp3, NULL, NULL},
  {ED_RAMP4, -1, -1, ED_PTREE_RAMP4, 0, 4, 0, false, 
   "ed_ramp4", next_ramp4, previous_ramp4, NULL, NULL},
  {ED_XRAMP, ED_XRAMP_RAMP, ED_XRAMP2, ED_PTREE_XRAMP, 0, 0, 1, false, 
   "ed_xramp", next_xramp, previous_xramp, NULL, NULL},
  {ED_XRAMP2, ED_XRAMP2_RAMP, -1, ED_PTREE_XRAMP2, 0, 0, 2, false, 
   "ed_xramp2", next_xramp2, previous_xramp2, NULL, NULL},
  {ED_XRAMP_RAMP, ED_XRAMP_RAMP2, ED_XRAMP2_RAMP, ED_PTREE_XRAMP_RAMP, 0, 1, 1, false, 
   "ed_xramp_ramp", next_xramp_ramp, previous_xramp_ramp, NULL, NULL},
  {ED_XRAMP_RAMP2, ED_XRAMP_RAMP3, ED_XRAMP2_RAMP2, ED_PTREE_XRAMP_RAMP2, 0, 2, 1, false, 
   "ed_xramp_ramp2", next_xrampn_rampn, previous_xrampn_rampn, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP2_RAMP, ED_XRAMP2_RAMP2, -1, ED_PTREE_XRAMP2_RAMP, 0, 1, 2, false, 
   "ed_xramp2_ramp", next_xrampn_rampn, previous_xrampn_rampn, next_xramp2_ramp_to_float, previous_xramp2_ramp_to_float},
  {ED_XRAMP_RAMP3, -1, -1, ED_PTREE_XRAMP_RAMP3, 0, 3, 1, false, 
   "ed_xramp_ramp3", next_xrampn_rampn, previous_xrampn_rampn, next_xramp_ramp3_to_float, previous_xramp_ramp3_to_float},
  {ED_XRAMP2_RAMP2, -1, -1, ED_PTREE_XRAMP2_RAMP2, 0, 2, 2, false, 
   "ed_xramp2_ramp2", next_xrampn_rampn, previous_xrampn_rampn, next_xramp2_ramp2_to_float, previous_xramp2_ramp2_to_float},

  /* ptree */
  {ED_PTREE, ED_RAMP_PTREE, ED_XRAMP_PTREE, ED_PTREE2, 1, 0, 0, false, 
   "ed_ptree", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE_RAMP, ED_RAMP_PTREE_RAMP, ED_XRAMP_PTREE_RAMP, ED_PTREE2_RAMP, 1, 1, 0, false, 
   "ed_ptree_ramp", next_ptree_rampn, previous_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP, ED_RAMP_PTREE_XRAMP, ED_XRAMP_PTREE_XRAMP, ED_PTREE2_XRAMP, 1, 0, 1, false, 
   "ed_ptree_xramp", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_PTREE_ZERO, ED_RAMP_PTREE_ZERO, ED_XRAMP_PTREE_ZERO, ED_PTREE2_ZERO, 1, 0, 0, true, 
   "ed_ptree_zero", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE_RAMP2, ED_RAMP_PTREE_RAMP2, ED_XRAMP_PTREE_RAMP2, ED_PTREE2_RAMP2, 1, 2, 0, false, 
   "ed_ptree_ramp2", next_ptree_rampn, previous_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_RAMP3, ED_RAMP_PTREE_RAMP3, ED_XRAMP_PTREE_RAMP3, ED_PTREE2_RAMP3, 1, 3, 0, false, 
   "ed_ptree_ramp3", next_ptree_rampn, previous_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_XRAMP_RAMP, ED_RAMP_PTREE_XRAMP_RAMP, ED_XRAMP_PTREE_XRAMP_RAMP, ED_PTREE2_XRAMP_RAMP, 1, 1, 1, false, 
   "ed_ptree_xramp_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE_XRAMP2, ED_RAMP_PTREE_XRAMP2, -1, ED_PTREE2_XRAMP2, 1, 0, 2, false, 
   "ed_ptree_xramp2", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE, ED_RAMP2_PTREE, ED_XRAMP_RAMP_PTREE, ED_PTREE_RAMP_PTREE, 1, 1, 0, false, 
   "ed_ramp_ptree", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_ZERO, ED_RAMP2_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_ZERO, ED_PTREE_RAMP_PTREE_ZERO, 1, 1, 0, true, 
   "ed_ramp_ptree_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE, ED_RAMP3_PTREE, ED_XRAMP_RAMP2_PTREE, ED_PTREE_RAMP2_PTREE, 1, 2, 0, false, 
   "ed_ramp2_ptree", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE_ZERO, ED_RAMP3_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_ZERO, ED_PTREE_RAMP2_PTREE_ZERO, 1, 2, 0, true, 
   "ed_ramp2_ptree_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE, ED_RAMP4_PTREE, ED_XRAMP_RAMP3_PTREE, ED_PTREE_RAMP3_PTREE, 1, 3, 0, false, 
   "ed_ramp3_ptree", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP3_PTREE_ZERO, ED_RAMP4_PTREE_ZERO, ED_XRAMP_RAMP3_PTREE_ZERO, ED_PTREE_RAMP3_PTREE_ZERO, 1, 3, 0, true, 
   "ed_ramp3_ptree_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE, ED_XRAMP_RAMP_PTREE, ED_XRAMP2_PTREE, ED_PTREE_XRAMP_PTREE, 1, 0, 1, false, 
   "ed_xramp_ptree", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_ZERO, ED_XRAMP2_PTREE_ZERO, ED_PTREE_XRAMP_PTREE_ZERO, 1, 0, 1, true, 
   "ed_xramp_ptree_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE, ED_XRAMP2_RAMP_PTREE, -1, ED_PTREE_XRAMP2_PTREE, 1, 0, 2, false, 
   "ed_xramp2_ptree", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE_ZERO, ED_XRAMP2_RAMP_PTREE_ZERO, -1, ED_PTREE_XRAMP2_PTREE_ZERO, 1, 0, 2, true, 
   "ed_xramp2_ptree_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_RAMP, ED_PTREE_RAMP_PTREE_RAMP, 1, 2, 0, false, 
   "ed_ramp_ptree_ramp", next_ramp_ptree_ramp, previous_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE_RAMP, ED_RAMP3_PTREE_RAMP, ED_XRAMP_RAMP2_PTREE_RAMP, ED_PTREE_RAMP2_PTREE_RAMP, 1, 3, 0, false, 
   "ed_ramp2_ptree_ramp", next_ramp2_ptree_ramp, previous_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_RAMP2, ED_RAMP2_PTREE_RAMP2, ED_XRAMP_RAMP_PTREE_RAMP2, ED_PTREE_RAMP_PTREE_RAMP2, 1, 3, 0, false, 
   "ed_ramp_ptree_ramp2", next_ramp_ptree_ramp2, previous_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP_PTREE_XRAMP, ED_RAMP2_PTREE_XRAMP, ED_XRAMP_RAMP_PTREE_XRAMP, ED_PTREE_RAMP_PTREE_XRAMP, 1, 1, 1, false, 
   "ed_ramp_ptree_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_RAMP, ED_XRAMP2_PTREE_RAMP, ED_PTREE_XRAMP_PTREE_RAMP, 1, 1, 1, false, 
   "ed_xramp_ptree_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE_XRAMP, ED_RAMP3_PTREE_XRAMP, ED_XRAMP_RAMP2_PTREE_XRAMP, ED_PTREE_RAMP2_PTREE_XRAMP, 1, 2, 1, false, 
   "ed_ramp2_ptree_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP2_PTREE, ED_XRAMP2_RAMP_PTREE, ED_PTREE_XRAMP_RAMP_PTREE, 1, 1, 1, false, 
   "ed_xramp_ramp_ptree", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_ZERO, ED_XRAMP2_RAMP_PTREE_ZERO, ED_PTREE_XRAMP_RAMP_PTREE_ZERO, 1, 1, 1, true, 
   "ed_xramp_ramp_ptree_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE_XRAMP, ED_XRAMP_RAMP_PTREE_XRAMP, -1, ED_PTREE_XRAMP_PTREE_XRAMP, 1, 0, 2, false, 
   "ed_xramp_ptree_xramp", next_xramp_ptree_xramp, previous_xramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE, ED_XRAMP_RAMP3_PTREE, ED_XRAMP2_RAMP2_PTREE, ED_PTREE_XRAMP_RAMP2_PTREE, 1, 2, 1, false, 
   "ed_xramp_ramp2_ptree", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE_ZERO, ED_XRAMP_RAMP3_PTREE_ZERO, ED_XRAMP2_RAMP2_PTREE_ZERO, ED_PTREE_XRAMP_RAMP2_PTREE_ZERO, 1, 2, 1, true, 
   "ed_xramp_ramp2_ptree_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE, ED_XRAMP2_RAMP2_PTREE, -1, ED_PTREE_XRAMP2_RAMP_PTREE, 1, 1, 2, false, 
   "ed_xramp2_ramp_ptree", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_RAMP_PTREE_ZERO, ED_XRAMP2_RAMP2_PTREE_ZERO, -1, ED_PTREE_XRAMP2_RAMP_PTREE_ZERO, 1, 1, 2, true, 
   "ed_xramp2_ramp_ptree_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP2_RAMP, ED_RAMP_PTREE_XRAMP2_RAMP, -1, ED_PTREE2_XRAMP2_RAMP, 1, 1, 2, false, 
   "ed_ptree_xramp2_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE_XRAMP_RAMP2, ED_RAMP_PTREE_XRAMP_RAMP2, ED_XRAMP_PTREE_XRAMP_RAMP2, ED_PTREE2_XRAMP_RAMP2, 1, 2, 1, false, 
   "ed_ptree_xramp_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_PTREE_RAMP2, ED_XRAMP_RAMP_PTREE_RAMP2, ED_XRAMP2_PTREE_RAMP2, ED_PTREE_XRAMP_PTREE_RAMP2, 1, 2, 1, false, 
   "ed_xramp_ptree_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE_RAMP, ED_XRAMP_RAMP2_PTREE_RAMP, ED_XRAMP2_RAMP_PTREE_RAMP, ED_PTREE_XRAMP_RAMP_PTREE_RAMP, 1, 2, 1, false, 
   "ed_xramp_ramp_ptree_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE_XRAMP, ED_XRAMP_RAMP2_PTREE_XRAMP, -1, ED_PTREE_XRAMP_RAMP_PTREE_XRAMP, 1, 1, 2, false, 
   "ed_xramp_ramp_ptree_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_PTREE_RAMP, ED_XRAMP2_RAMP_PTREE_RAMP, -1, ED_PTREE_XRAMP2_PTREE_RAMP, 1, 1, 2, false, 
   "ed_xramp2_ptree_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP, -1, ED_PTREE_XRAMP_PTREE_XRAMP_RAMP, 1, 1, 2, false, 
   "ed_xramp_ptree_xramp_ramp", next_xramp_ptree_xramp_ramp, previous_xramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_RAMP, ED_RAMP2_PTREE_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP, ED_PTREE_RAMP_PTREE_XRAMP_RAMP, 1, 2, 1, false, 
   "ed_ramp_ptree_xramp_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP2, ED_RAMP2_PTREE_XRAMP2, -1, ED_PTREE_RAMP_PTREE_XRAMP2, 1, 1, 2, false, 
   "ed_ramp_ptree_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP4_PTREE, -1, -1, ED_PTREE_RAMP4_PTREE, 1, 4, 0, false, 
   "ed_ramp4_ptree", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP4_PTREE_ZERO, -1, -1, ED_PTREE_RAMP4_PTREE_ZERO, 1, 4, 0, true, 
   "ed_ramp4_ptree_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_PTREE_RAMP4, -1, -1, ED_PTREE2_RAMP4, 1, 4, 0, false, 
   "ed_ptree_ramp4", next_ptree_rampn, previous_ptree_rampn, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP3_PTREE_RAMP, -1, -1, ED_PTREE_RAMP3_PTREE_RAMP, 1, 4, 0, false, 
   "ed_ramp3_ptree_ramp", next_ramp3_ptree_ramp, previous_ramp3_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_RAMP3, -1, -1, ED_PTREE_RAMP_PTREE_RAMP3, 1, 4, 0, false, 
   "ed_ramp_ptree_ramp3", next_ramp_ptree_ramp3, previous_ramp_ptree_ramp3, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE_RAMP2, -1, -1, ED_PTREE_RAMP2_PTREE_RAMP2, 1, 4, 0, false, 
   "ed_ramp2_ptree_ramp2", next_ramp2_ptree_ramp2, previous_ramp2_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE_XRAMP, -1, -1, ED_PTREE_RAMP3_PTREE_XRAMP, 1, 3, 1, false, 
   "ed_ramp3_ptree_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE_RAMP3, -1, -1, ED_PTREE_XRAMP_PTREE_RAMP3, 1, 3, 1, false, 
   "ed_xramp_ptree_ramp3", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE_XRAMP2, -1, -1, ED_PTREE_RAMP2_PTREE_XRAMP2, 1, 2, 2, false, 
   "ed_ramp2_ptree_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_PTREE_RAMP2, -1, -1, ED_PTREE_XRAMP2_PTREE_RAMP2, 1, 2, 2, false, 
   "ed_xramp2_ptree_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE_XRAMP_RAMP, -1, -1, ED_PTREE_XRAMP_RAMP_PTREE_XRAMP_RAMP, 1, 2, 2, false, 
   "ed_xramp_ramp_ptree_xramp_ramp", next_xramp_ramp_ptree_xramp_ramp, previous_xramp_ramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP2, -1, -1, ED_PTREE_XRAMP_PTREE_XRAMP_RAMP2, 1, 2, 2, false, 
   "ed_xramp_ptree_xramp_ramp2", next_xramp_ptree_xramp_ramp2, previous_xramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_RAMP_PTREE_XRAMP2_RAMP, -1, -1, ED_PTREE_RAMP_PTREE_XRAMP2_RAMP, 1, 2, 2, false, 
   "ed_ramp_ptree_xramp2_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE_XRAMP_RAMP3, -1, -1, ED_PTREE2_XRAMP_RAMP3, 1, 3, 1, false, 
   "ed_ptree_xramp_ramp3", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp3_to_float, previous_xramp_ramp3_to_float},
  {ED_PTREE_XRAMP2_RAMP2, -1, -1, ED_PTREE2_XRAMP2_RAMP2, 1, 2, 2, false, 
   "ed_ptree_xramp2_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp2_ramp2_to_float, previous_xramp2_ramp2_to_float},
  {ED_XRAMP_RAMP3_PTREE, -1, -1, ED_PTREE_XRAMP_RAMP3_PTREE, 1, 3, 1, false, 
   "ed_xramp_ramp3_ptree", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_RAMP3_PTREE_ZERO, -1, -1, ED_PTREE_XRAMP_RAMP3_PTREE_ZERO, 1, 3, 1, true, 
   "ed_xramp_ramp3_ptree_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP2_RAMP2_PTREE, -1, -1, ED_PTREE_XRAMP2_RAMP2_PTREE, 1, 2, 2, false, 
   "ed_xramp2_ramp2_ptree", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP2_PTREE_ZERO, -1, -1, ED_PTREE_XRAMP2_RAMP2_PTREE_ZERO, 1, 2, 2, true, 
   "ed_xramp2_ramp2_ptree_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE_RAMP2, -1, -1, ED_PTREE_XRAMP_RAMP_PTREE_RAMP2, 1, 3, 1, false, 
   "ed_xramp_ramp_ptree_ramp2", next_xramp_ramp_ptree_ramp2, previous_xramp_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE_RAMP, -1, -1, ED_PTREE_XRAMP_RAMP2_PTREE_RAMP, 1, 3, 1, false, 
   "ed_xramp_ramp2_ptree_ramp", next_xramp_ramp2_ptree_ramp, previous_xramp_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_RAMP2, -1, -1, ED_PTREE_RAMP_PTREE_XRAMP_RAMP2, 1, 3, 1, false, 
   "ed_ramp_ptree_xramp_ramp2", next_ramp_ptree_xramp_ramp2, previous_ramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE_XRAMP, -1, -1, ED_PTREE_XRAMP_RAMP2_PTREE_XRAMP, 1, 2, 2, false, 
   "ed_xramp_ramp2_ptree_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE_RAMP, -1, -1, ED_PTREE_XRAMP2_RAMP_PTREE_RAMP, 1, 2, 2, false, 
   "ed_xramp2_ramp_ptree_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE_XRAMP_RAMP, -1, -1, ED_PTREE_RAMP2_PTREE_XRAMP_RAMP, 1, 3, 1, false, 
   "ed_ramp2_ptree_xramp_ramp", next_ramp2_ptree_xramp_ramp, previous_ramp2_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},

  /* ptree2 */
  {ED_PTREE2, ED_RAMP_PTREE2, ED_XRAMP_PTREE2, ED_PTREE3, 2, 0, 0, false, 
   "ed_ptree2", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE2_RAMP, ED_RAMP_PTREE2_RAMP, ED_XRAMP_PTREE2_RAMP, ED_PTREE3_RAMP, 2, 1, 0, false, 
   "ed_ptree2_ramp", next_ptree_rampn, previous_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE2_XRAMP, ED_RAMP_PTREE2_XRAMP, ED_XRAMP_PTREE2_XRAMP, ED_PTREE3_XRAMP, 2, 0, 1, false, 
   "ed_ptree2_xramp", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_PTREE2_ZERO, ED_RAMP_PTREE2_ZERO, ED_XRAMP_PTREE2_ZERO, ED_PTREE3_ZERO, 2, 0, 0, true, 
   "ed_ptree2_zero", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE2_RAMP2, ED_RAMP_PTREE2_RAMP2, ED_XRAMP_PTREE2_RAMP2, ED_PTREE3_RAMP2, 2, 2, 0, false, 
   "ed_ptree2_ramp2", next_ptree_rampn, previous_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE2_RAMP3, ED_RAMP_PTREE2_RAMP3, ED_XRAMP_PTREE2_RAMP3, ED_PTREE3_RAMP3, 2, 3, 0, false, 
   "ed_ptree2_ramp3", next_ptree_rampn, previous_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE2_XRAMP_RAMP, ED_RAMP_PTREE2_XRAMP_RAMP, ED_XRAMP_PTREE2_XRAMP_RAMP, ED_PTREE3_XRAMP_RAMP, 2, 1, 1, false, 
   "ed_ptree2_xramp_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_PTREE2_XRAMP2, ED_RAMP_PTREE2_XRAMP2, -1, ED_PTREE3_XRAMP2, 2, 0, 2, false, 
   "ed_ptree2_xramp2", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE2, ED_RAMP2_PTREE2, ED_XRAMP_RAMP_PTREE2, -1, 2, 1, 0, false, 
   "ed_ramp_ptree2", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE2_ZERO, ED_RAMP2_PTREE2_ZERO, ED_XRAMP_RAMP_PTREE2_ZERO, -1, 2, 1, 0, true, 
   "ed_ramp_ptree2_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE2, ED_RAMP3_PTREE2, ED_XRAMP_RAMP2_PTREE2, -1, 2, 2, 0, false, 
   "ed_ramp2_ptree2", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE2_ZERO, ED_RAMP3_PTREE2_ZERO, ED_XRAMP_RAMP2_PTREE2_ZERO, -1, 2, 2, 0, true, 
   "ed_ramp2_ptree2_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE2, ED_RAMP4_PTREE2, ED_XRAMP_RAMP3_PTREE2, -1, 2, 3, 0, false, 
   "ed_ramp3_ptree2", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP3_PTREE2_ZERO, ED_RAMP4_PTREE2_ZERO, ED_XRAMP_RAMP3_PTREE2_ZERO, -1, 2, 3, 0, true, 
   "ed_ramp3_ptree2_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE2, ED_XRAMP_RAMP_PTREE2, ED_XRAMP2_PTREE2, -1, 2, 0, 1, false, 
   "ed_xramp_ptree2", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP_PTREE2_ZERO, ED_XRAMP_RAMP_PTREE2_ZERO, ED_XRAMP2_PTREE2_ZERO, -1, 2, 0, 1, true, 
   "ed_xramp_ptree2_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE2, ED_XRAMP2_RAMP_PTREE2, -1, -1, 2, 0, 2, false, 
   "ed_xramp2_ptree2", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE2_ZERO, ED_XRAMP2_RAMP_PTREE2_ZERO, -1, -1, 2, 0, 2, true, 
   "ed_xramp2_ptree2_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE2_RAMP, ED_RAMP2_PTREE2_RAMP, ED_XRAMP_RAMP_PTREE2_RAMP, -1, 2, 2, 0, false, 
   "ed_ramp_ptree2_ramp", next_ramp_ptree_ramp, previous_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE2_RAMP, ED_RAMP3_PTREE2_RAMP, ED_XRAMP_RAMP2_PTREE2_RAMP, -1, 2, 3, 0, false, 
   "ed_ramp2_ptree2_ramp", next_ramp2_ptree_ramp, previous_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE2_RAMP2, ED_RAMP2_PTREE2_RAMP2, ED_XRAMP_RAMP_PTREE2_RAMP2, -1, 2, 3, 0, false, 
   "ed_ramp_ptree2_ramp2", next_ramp_ptree_ramp2, previous_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP_PTREE2_XRAMP, ED_RAMP2_PTREE2_XRAMP, ED_XRAMP_RAMP_PTREE2_XRAMP, -1, 2, 1, 1, false, 
   "ed_ramp_ptree2_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE2_RAMP, ED_XRAMP_RAMP_PTREE2_RAMP, ED_XRAMP2_PTREE2_RAMP, -1, 2, 1, 1, false, 
   "ed_xramp_ptree2_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE2_XRAMP, ED_RAMP3_PTREE2_XRAMP, ED_XRAMP_RAMP2_PTREE2_XRAMP, -1, 2, 2, 1, false, 
   "ed_ramp2_ptree2_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE2, ED_XRAMP_RAMP2_PTREE2, ED_XRAMP2_RAMP_PTREE2, -1, 2, 1, 1, false, 
   "ed_xramp_ramp_ptree2", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE2_ZERO, ED_XRAMP_RAMP2_PTREE2_ZERO, ED_XRAMP2_RAMP_PTREE2_ZERO, -1, 2, 1, 1, true, 
   "ed_xramp_ramp_ptree2_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE2_XRAMP, ED_XRAMP_RAMP_PTREE2_XRAMP, -1, -1, 2, 0, 2, false, 
   "ed_xramp_ptree2_xramp", next_xramp_ptree_xramp, previous_xramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE2, ED_XRAMP_RAMP3_PTREE2, ED_XRAMP2_RAMP2_PTREE2, -1, 2, 2, 1, false, 
   "ed_xramp_ramp2_ptree2", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE2_ZERO, ED_XRAMP_RAMP3_PTREE2_ZERO, ED_XRAMP2_RAMP2_PTREE2_ZERO, -1, 2, 2, 1, true, 
   "ed_xramp_ramp2_ptree2_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE2, ED_XRAMP2_RAMP2_PTREE2, -1, -1, 2, 1, 2, false, 
   "ed_xramp2_ramp_ptree2", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_RAMP_PTREE2_ZERO, ED_XRAMP2_RAMP2_PTREE2_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_xramp2_ramp_ptree2_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE2_XRAMP2_RAMP, ED_RAMP_PTREE2_XRAMP2_RAMP, -1, ED_PTREE3_XRAMP2_RAMP, 2, 1, 2, false, 
   "ed_ptree2_xramp2_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE2_XRAMP_RAMP2, ED_RAMP_PTREE2_XRAMP_RAMP2, ED_XRAMP_PTREE2_XRAMP_RAMP2, ED_PTREE3_XRAMP_RAMP2, 2, 2, 1, false, 
   "ed_ptree2_xramp_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_PTREE2_RAMP2, ED_XRAMP_RAMP_PTREE2_RAMP2, ED_XRAMP2_PTREE2_RAMP2, -1, 2, 2, 1, false, 
   "ed_xramp_ptree2_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE2_RAMP, ED_XRAMP_RAMP2_PTREE2_RAMP, ED_XRAMP2_RAMP_PTREE2_RAMP, -1, 2, 2, 1, false, 
   "ed_xramp_ramp_ptree2_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE2_XRAMP, ED_XRAMP_RAMP2_PTREE2_XRAMP, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ramp_ptree2_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_PTREE2_RAMP, ED_XRAMP2_RAMP_PTREE2_RAMP, -1, -1, 2, 1, 2, false, 
   "ed_xramp2_ptree2_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE2_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE2_XRAMP_RAMP, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ptree2_xramp_ramp", next_xramp_ptree_xramp_ramp, previous_xramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_RAMP_PTREE2_XRAMP_RAMP, ED_RAMP2_PTREE2_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE2_XRAMP_RAMP, -1, 2, 2, 1, false, 
   "ed_ramp_ptree2_xramp_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_RAMP_PTREE2_XRAMP2, ED_RAMP2_PTREE2_XRAMP2, -1, -1, 2, 1, 2, false, 
   "ed_ramp_ptree2_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP4_PTREE2, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp4_ptree2", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP4_PTREE2_ZERO, -1, -1, -1, 2, 4, 0, true, 
   "ed_ramp4_ptree2_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_PTREE2_RAMP4, -1, -1, ED_PTREE3_RAMP4, 2, 4, 0, false, 
   "ed_ptree2_ramp4", next_ptree_rampn, previous_ptree_rampn, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP3_PTREE2_RAMP, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp3_ptree2_ramp", next_ramp3_ptree_ramp, previous_ramp3_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE2_RAMP3, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp_ptree2_ramp3", next_ramp_ptree_ramp3, previous_ramp_ptree_ramp3, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE2_RAMP2, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp2_ptree2_ramp2", next_ramp2_ptree_ramp2, previous_ramp2_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE2_XRAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp3_ptree2_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE2_RAMP3, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ptree2_ramp3", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE2_XRAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp2_ptree2_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_PTREE2_RAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ptree2_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE2_XRAMP_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp_ptree2_xramp_ramp", next_xramp_ramp_ptree_xramp_ramp, previous_xramp_ramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_XRAMP_PTREE2_XRAMP_RAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree2_xramp_ramp2", next_xramp_ptree_xramp_ramp2, previous_xramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_RAMP_PTREE2_XRAMP2_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree2_xramp2_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE2_XRAMP_RAMP3, -1, -1, ED_PTREE3_XRAMP_RAMP3, 2, 3, 1, false, 
   "ed_ptree2_xramp_ramp3", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp3_to_float, previous_xramp_ramp3_to_float},
  {ED_PTREE2_XRAMP2_RAMP2, -1, -1, ED_PTREE3_XRAMP2_RAMP2, 2, 2, 2, false, 
   "ed_ptree2_xramp2_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp2_ramp2_to_float, previous_xramp2_ramp2_to_float},
  {ED_XRAMP_RAMP3_PTREE2, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp3_ptree2", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_RAMP3_PTREE2_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_xramp_ramp3_ptree2_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP2_RAMP2_PTREE2, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ramp2_ptree2", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP2_PTREE2_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp2_ramp2_ptree2_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE2_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp_ptree2_ramp2", next_xramp_ramp_ptree_ramp2, previous_xramp_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE2_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp2_ptree2_ramp", next_xramp_ramp2_ptree_ramp, previous_xramp_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE2_XRAMP_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree2_xramp_ramp2", next_ramp_ptree_xramp_ramp2, previous_ramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE2_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp2_ptree2_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE2_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ramp_ptree2_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE2_XRAMP_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp2_ptree2_xramp_ramp", next_ramp2_ptree_xramp_ramp, previous_ramp2_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},

  {ED_PTREE_RAMP2_PTREE, ED_RAMP_PTREE_RAMP2_PTREE, ED_XRAMP_PTREE_RAMP2_PTREE, -1, 2, 2, 0, false, 
   "ed_ptree_ramp2_ptree", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_RAMP2_PTREE_ZERO, ED_RAMP_PTREE_RAMP2_PTREE_ZERO, ED_XRAMP_PTREE_RAMP2_PTREE_ZERO, -1, 2, 2, 0, true, 
   "ed_ptree_ramp2_ptree_zero", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_RAMP_PTREE, ED_RAMP_PTREE_RAMP_PTREE, ED_XRAMP_PTREE_RAMP_PTREE, -1, 2, 1, 0, false, 
   "ed_ptree_ramp_ptree", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_RAMP_PTREE_ZERO, ED_RAMP_PTREE_RAMP_PTREE_ZERO, ED_XRAMP_PTREE_RAMP_PTREE_ZERO, -1, 2, 1, 0, true, 
   "ed_ptree_ramp_ptree_zero", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_RAMP3_PTREE, ED_RAMP_PTREE_RAMP3_PTREE, ED_XRAMP_PTREE_RAMP3_PTREE, -1, 2, 3, 0, false, 
   "ed_ptree_ramp3_ptree", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_RAMP3_PTREE_ZERO, ED_RAMP_PTREE_RAMP3_PTREE_ZERO, ED_XRAMP_PTREE_RAMP3_PTREE_ZERO, -1, 2, 3, 0, true, 
   "ed_ptree_ramp3_ptree_zero", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_XRAMP_PTREE, ED_RAMP_PTREE_XRAMP_PTREE, ED_XRAMP_PTREE_XRAMP_PTREE, -1, 2, 0, 1, false, 
   "ed_ptree_xramp_ptree", next_ptree_xramp_ptree, previous_ptree_xramp_ptree, NULL, NULL},
  {ED_PTREE_XRAMP_PTREE_ZERO, ED_RAMP_PTREE_XRAMP_PTREE_ZERO, ED_XRAMP_PTREE_XRAMP_PTREE_ZERO, -1, 2, 0, 1, true, 
   "ed_ptree_xramp_ptree_zero", next_ptree_xramp_ptree, previous_ptree_xramp_ptree, NULL, NULL},
  {ED_PTREE_RAMP2_PTREE_RAMP, ED_RAMP_PTREE_RAMP2_PTREE_RAMP, ED_XRAMP_PTREE_RAMP2_PTREE_RAMP, -1, 2, 3, 0, false, 
   "ed_ptree_ramp2_ptree_ramp", next_ptree_rampn_ptree_ramp, previous_ptree_rampn_ptree_ramp, next_ramp2_2_to_float, previous_ramp2_2_to_float},
  {ED_PTREE_RAMP_PTREE_RAMP2, ED_RAMP_PTREE_RAMP_PTREE_RAMP2, ED_XRAMP_PTREE_RAMP_PTREE_RAMP2, -1, 2, 3, 0, false, 
   "ed_ptree_ramp_ptree_ramp2", next_ptree_rampn_ptree_ramp2, previous_ptree_rampn_ptree_ramp2, next_ramp1_3_to_float, previous_ramp1_3_to_float},
  {ED_RAMP_PTREE_RAMP_PTREE_RAMP, ED_RAMP2_PTREE_RAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_RAMP, -1, 2, 3, 0, false, 
   "ed_ramp_ptree_ramp_ptree_ramp", next_rampn_ptree_ramp_ptree_ramp, previous_rampn_ptree_ramp_ptree_ramp, next_ramp1_3_to_float, previous_ramp1_3_to_float},
  {ED_PTREE_RAMP_PTREE_RAMP, ED_RAMP_PTREE_RAMP_PTREE_RAMP, ED_XRAMP_PTREE_RAMP_PTREE_RAMP, -1, 2, 2, 0, false, 
   "ed_ptree_ramp_ptree_ramp", next_ptree_rampn_ptree_ramp, previous_ptree_rampn_ptree_ramp, next_ramp1_2_to_float, previous_ramp1_2_to_float},
  {ED_RAMP2_PTREE_RAMP_PTREE, ED_RAMP3_PTREE_RAMP_PTREE, ED_XRAMP_RAMP2_PTREE_RAMP_PTREE, -1, 2, 3, 0, false, 
   "ed_ramp2_ptree_ramp_ptree", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp2_2_to_float, previous_ramp2_2_to_float},
  {ED_RAMP2_PTREE_RAMP_PTREE_ZERO, ED_RAMP3_PTREE_RAMP_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_RAMP_PTREE_ZERO, -1, 2, 3, 0, true, 
   "ed_ramp2_ptree_ramp_ptree_zero", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp2_2_to_float, previous_ramp2_2_to_float},
  {ED_XRAMP_PTREE_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_RAMP_PTREE, ED_XRAMP2_PTREE_RAMP_PTREE, -1, 2, 1, 1, false, 
   "ed_xramp_ptree_ramp_ptree", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE_RAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_ZERO, ED_XRAMP2_PTREE_RAMP_PTREE_ZERO, -1, 2, 1, 1, true, 
   "ed_xramp_ptree_ramp_ptree_zero", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_PTREE, ED_RAMP2_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE, -1, 2, 1, 1, false, 
   "ed_ramp_ptree_xramp_ptree", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_PTREE_ZERO, ED_RAMP2_PTREE_XRAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_ZERO, -1, 2, 1, 1, true, 
   "ed_ramp_ptree_xramp_ptree_zero", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_RAMP_PTREE, ED_RAMP_PTREE_XRAMP_RAMP_PTREE, ED_XRAMP_PTREE_XRAMP_RAMP_PTREE, -1, 2, 1, 1, false, 
   "ed_ptree_xramp_ramp_ptree", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, -1, 2, 1, 1, true, 
   "ed_ptree_xramp_ramp_ptree_zero", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_PTREE_RAMP, ED_RAMP_PTREE_XRAMP_PTREE_RAMP, ED_XRAMP_PTREE_XRAMP_PTREE_RAMP, -1, 2, 1, 1, false, 
   "ed_ptree_xramp_ptree_ramp", next_ptree_xramp_ptree_rampn, previous_ptree_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_RAMP_PTREE_XRAMP, ED_RAMP_PTREE_RAMP_PTREE_XRAMP, ED_XRAMP_PTREE_RAMP_PTREE_XRAMP, -1, 2, 1, 1, false, 
   "ed_ptree_ramp_ptree_xramp", next_ptree_rampn_ptree_xramp, previous_ptree_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_RAMP2_PTREE, ED_RAMP2_PTREE_RAMP2_PTREE, ED_XRAMP_RAMP_PTREE_RAMP2_PTREE, -1, 2, 3, 0, false, 
   "ed_ramp_ptree_ramp2_ptree", next_rampn_ptree_ramp2_ptree, previous_rampn_ptree_ramp2_ptree, next_ramp1_3_to_float, previous_ramp1_3_to_float},
  {ED_RAMP_PTREE_RAMP2_PTREE_ZERO, ED_RAMP2_PTREE_RAMP2_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_RAMP2_PTREE_ZERO, -1, 2, 3, 0, true, 
   "ed_ramp_ptree_ramp2_ptree_zero", next_rampn_ptree_ramp2_ptree, previous_rampn_ptree_ramp2_ptree, next_ramp1_3_to_float, previous_ramp1_3_to_float},
  {ED_XRAMP_PTREE_RAMP2_PTREE, ED_XRAMP_RAMP_PTREE_RAMP2_PTREE, ED_XRAMP2_PTREE_RAMP2_PTREE, -1, 2, 2, 1, false, 
   "ed_xramp_ptree_ramp2_ptree", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_PTREE_RAMP2_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_RAMP2_PTREE_ZERO, ED_XRAMP2_PTREE_RAMP2_PTREE_ZERO, -1, 2, 2, 1, true, 
   "ed_xramp_ptree_ramp2_ptree_zero", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE_XRAMP_PTREE, ED_RAMP3_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE, -1, 2, 2, 1, false, 
   "ed_ramp2_ptree_xramp_ptree", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE_XRAMP_PTREE_ZERO, ED_RAMP3_PTREE_XRAMP_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE_ZERO, -1, 2, 2, 1, true, 
   "ed_ramp2_ptree_xramp_ptree_zero", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP_RAMP2_PTREE, ED_RAMP_PTREE_XRAMP_RAMP2_PTREE, ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE, -1, 2, 2, 1, false, 
   "ed_ptree_xramp_ramp2_ptree", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP_RAMP2_PTREE_ZERO, ED_RAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO, ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO, -1, 2, 2, 1, true, 
   "ed_ptree_xramp_ramp2_ptree_zero", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP2_RAMP_PTREE, ED_RAMP_PTREE_XRAMP2_RAMP_PTREE, -1, -1, 2, 1, 2, false, 
   "ed_ptree_xramp2_ramp_ptree", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP2_RAMP_PTREE_ZERO, ED_RAMP_PTREE_XRAMP2_RAMP_PTREE_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_ptree_xramp2_ramp_ptree_zero", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_PTREE_RAMP2, ED_RAMP_PTREE_XRAMP_PTREE_RAMP2, ED_XRAMP_PTREE_XRAMP_PTREE_RAMP2, -1, 2, 2, 1, false, 
   "ed_ptree_xramp_ptree_ramp2", next_ptree_xramp_ptree_rampn, previous_ptree_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_RAMP2_PTREE_XRAMP, ED_RAMP_PTREE_RAMP2_PTREE_XRAMP, ED_XRAMP_PTREE_RAMP2_PTREE_XRAMP, -1, 2, 2, 1, false, 
   "ed_ptree_ramp2_ptree_xramp", next_ptree_rampn_ptree_xramp, previous_ptree_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP2_PTREE, ED_RAMP_PTREE_XRAMP2_PTREE, -1, -1, 2, 0, 2, false, 
   "ed_ptree_xramp2_ptree", next_ptree_xramp_ptree, previous_ptree_xramp_ptree, NULL, NULL},
  {ED_PTREE_XRAMP2_PTREE_ZERO, ED_RAMP_PTREE_XRAMP2_PTREE_ZERO, -1, -1, 2, 0, 2, true, 
   "ed_ptree_xramp2_ptree_zero", next_ptree_xramp_ptree, previous_ptree_xramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE_RAMP_PTREE, ED_RAMP2_PTREE_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_RAMP_PTREE, -1, 2, 2, 0, false, 
   "ed_ramp_ptree_ramp_ptree", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp1_2_to_float, previous_ramp1_2_to_float},
  {ED_RAMP_PTREE_RAMP_PTREE_ZERO, ED_RAMP2_PTREE_RAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_ZERO, -1, 2, 2, 0, true, 
   "ed_ramp_ptree_ramp_ptree", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp1_2_to_float, previous_ramp1_2_to_float},
  {ED_XRAMP_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE, -1, -1, 2, 0, 2, false, 
   "ed_xramp_ptree_xramp_ptree", next_xramp_ptree_xramp_ptree, previous_xramp_ptree_xramp_ptree, NULL, NULL},
  {ED_XRAMP_PTREE_XRAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_ZERO, -1, -1, 2, 0, 2, true, 
   "ed_xramp_ptree_xramp_ptree", next_xramp_ptree_xramp_ptree, previous_xramp_ptree_xramp_ptree, NULL, NULL},
  {ED_PTREE_XRAMP_PTREE_XRAMP, ED_RAMP_PTREE_XRAMP_PTREE_XRAMP, -1, -1, 2, 0, 2, false, 
   "ed_ptree_xramp_ptree_xramp", next_ptree_xramp_ptree_xramp, previous_ptree_xramp_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP_PTREE, ED_RAMP2_PTREE_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE, -1, 2, 2, 1, false, 
   "ed_ramp_ptree_xramp_ramp_ptree", next_ramp_ptree_xramp_ramp_ptree, previous_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_RAMP2_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, -1, 2, 2, 1, true, 
   "ed_ramp_ptree_xramp_ramp_ptree_zero", next_ramp_ptree_xramp_ramp_ptree, previous_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_XRAMP_PTREE_XRAMP_RAMP_PTREE, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ptree_xramp_ramp_ptree", next_xramp_ptree_xramp_rampn_ptree, previous_xramp_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_xramp_ptree_xramp_ramp_ptree_zero", next_xramp_ptree_xramp_rampn_ptree, previous_xramp_ptree_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE_RAMP_PTREE, ED_XRAMP_RAMP2_PTREE_RAMP_PTREE, ED_XRAMP2_RAMP_PTREE_RAMP_PTREE, -1, 2, 2, 1, false, 
   "ed_xramp_ramp_ptree_ramp_ptree", next_xramp_ramp_ptree_ramp_ptree, previous_xramp_ramp_ptree_ramp_ptree, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_RAMP_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_RAMP_PTREE_ZERO, ED_XRAMP2_RAMP_PTREE_RAMP_PTREE_ZERO, -1, 2, 2, 1, true, 
   "ed_xramp_ramp_ptree_ramp_ptree_zero", next_xramp_ramp_ptree_ramp_ptree, previous_xramp_ramp_ptree_ramp_ptree, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_XRAMP_PTREE, ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ramp_ptree_xramp_ptree", next_xramp_rampn_ptree_xramp_ptree, previous_xramp_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_ZERO, ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_xramp_ramp_ptree_xramp_ptree_zero", next_xramp_rampn_ptree_xramp_ptree, previous_xramp_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_PTREE_RAMP_PTREE, ED_XRAMP2_RAMP_PTREE_RAMP_PTREE, -1, -1, 2, 1, 2, false, 
   "ed_xramp2_ptree_ramp_ptree", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_PTREE_RAMP_PTREE_ZERO, ED_XRAMP2_RAMP_PTREE_RAMP_PTREE_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_xramp2_ptree_ramp_ptree_zero", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP2_PTREE, ED_RAMP2_PTREE_XRAMP2_PTREE, -1, -1, 2, 1, 2, false, 
   "ed_ramp_ptree_xramp2_ptree", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP2_PTREE_ZERO, ED_RAMP2_PTREE_XRAMP2_PTREE_ZERO, -1, -1, 2, 1, 2, true, 
   "ed_ramp_ptree_xramp2_ptree_zero", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp_to_float, previous_ramp_to_float},

  {ED_RAMP2_PTREE_RAMP2_PTREE, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp2_ptree_ramp2_ptree", next_rampn_ptree_ramp2_ptree, previous_rampn_ptree_ramp2_ptree, next_ramp2_3_to_float, previous_ramp2_3_to_float},
  {ED_RAMP2_PTREE_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 4, 0, true, 
   "ed_ramp2_ptree_ramp2_ptree_zero", next_rampn_ptree_ramp2_ptree, previous_rampn_ptree_ramp2_ptree, next_ramp2_3_to_float, previous_ramp2_3_to_float},
  {ED_RAMP_PTREE_RAMP3_PTREE, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp_ptree_ramp3_ptree", next_ramp_ptree_ramp3_ptree, previous_ramp_ptree_ramp3_ptree, NULL, NULL},
  {ED_RAMP_PTREE_RAMP3_PTREE_ZERO, -1, -1, -1, 2, 4, 0, true, 
   "ed_ramp_ptree_ramp3_ptree_zero", next_ramp_ptree_ramp3_ptree, previous_ramp_ptree_ramp3_ptree, NULL, NULL},
  {ED_RAMP3_PTREE_RAMP_PTREE, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp3_ptree_ramp_ptree", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp3_2_to_float, previous_ramp3_2_to_float},
  {ED_RAMP3_PTREE_RAMP_PTREE_ZERO, -1, -1, -1, 2, 4, 0, true, 
   "ed_ramp3_ptree_ramp_ptree_zero", next_rampn_ptree_ramp_ptree, previous_rampn_ptree_ramp_ptree, next_ramp3_2_to_float, previous_ramp3_2_to_float},
  {ED_PTREE_RAMP2_PTREE_RAMP2, -1, -1, -1, 2, 4, 0, false, 
   "ed_ptree_ramp2_ptree_ramp2", next_ptree_rampn_ptree_ramp2, previous_ptree_rampn_ptree_ramp2, next_ramp2_3_to_float, previous_ramp2_3_to_float},
  {ED_PTREE_RAMP_PTREE_RAMP3, -1, -1, -1, 2, 4, 0, false, 
   "ed_ptree_ramp_ptree_ramp3", next_ptree_ramp_ptree_ramp3, previous_ptree_ramp_ptree_ramp3, NULL, NULL},
  {ED_PTREE_RAMP3_PTREE_RAMP, -1, -1, -1, 2, 4, 0, false, 
   "ed_ptree_ramp3_ptree_ramp", next_ptree_rampn_ptree_ramp, previous_ptree_rampn_ptree_ramp, next_ramp3_2_to_float, previous_ramp3_2_to_float},

  {ED_XRAMP_PTREE_RAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_RAMP, ED_XRAMP2_PTREE_RAMP_PTREE_RAMP, -1, 2, 2, 1, false, 
   "ed_xramp_ptree_ramp_ptree_ramp", next_xramp_ptree_ramp_ptree_ramp, previous_xramp_ptree_ramp_ptree_ramp, NULL, NULL},
  {ED_XRAMP_PTREE_XRAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_RAMP, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ptree_xramp_ptree_ramp", next_xramp_ptree_xramp_ptree_rampn, previous_xramp_ptree_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_PTREE_RAMP, ED_RAMP2_PTREE_XRAMP_PTREE_RAMP, ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_RAMP, -1, 2, 2, 1, false, 
   "ed_ramp_ptree_xramp_ptree_ramp", next_ramp_ptree_xramp_ptree_ramp, previous_ramp_ptree_xramp_ptree_ramp, NULL, NULL},
  {ED_PTREE_RAMP_PTREE_XRAMP_RAMP,  ED_RAMP_PTREE_RAMP_PTREE_XRAMP_RAMP, ED_XRAMP_PTREE_RAMP_PTREE_XRAMP_RAMP, -1, 2, 2, 1, false, 
   "ed_ptree_ramp_ptree_xramp_ramp", next_ptree_ramp_ptree_xramp_ramp, previous_ptree_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE_XRAMP_PTREE_XRAMP_RAMP,  ED_RAMP_PTREE_XRAMP_PTREE_XRAMP_RAMP, -1, -1, 2, 1, 2, false, 
   "ed_ptree_xramp_ptree_xramp_ramp", next_ptree_xramp_ptree_xramp_rampn, previous_ptree_xramp_ptree_xramp_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_RAMP_PTREE_RAMP, ED_RAMP_PTREE_XRAMP_RAMP_PTREE_RAMP, ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_RAMP, -1, 2, 2, 1, false, 
   "ed_ptree_xramp_ramp_ptree_ramp", next_ptree_xramp_ramp_ptree_ramp, previous_ptree_xramp_ramp_ptree_ramp, NULL, NULL},
  {ED_PTREE_XRAMP2_PTREE_RAMP,  ED_RAMP_PTREE_XRAMP2_PTREE_RAMP, -1, -1, 2, 1, 2, false, 
   "ed_ptree_xramp2_ptree_ramp", next_ptree_xramp_ptree_rampn, previous_ptree_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_XRAMP_RAMP_PTREE_XRAMP,  ED_RAMP_PTREE_XRAMP_RAMP_PTREE_XRAMP, -1, -1, 2, 1, 2, false, 
   "ed_ptree_xramp_ramp_ptree_xramp", next_ptree_xramp_rampn_ptree_xramp, previous_ptree_xramp_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_RAMP_PTREE_XRAMP, ED_RAMP2_PTREE_RAMP_PTREE_XRAMP, ED_XRAMP_RAMP_PTREE_RAMP_PTREE_XRAMP, -1, 2, 2, 1, false, 
   "ed_ramp_ptree_ramp_ptree_xramp", next_ramp_ptree_ramp_ptree_xramp, previous_ramp_ptree_ramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_PTREE_RAMP_PTREE_XRAMP,  ED_XRAMP_RAMP_PTREE_RAMP_PTREE_XRAMP, -1, -1, 2, 1, 2, false, 
   "ed_xramp_ptree_ramp_ptree_xramp", next_xramp_ptree_rampn_ptree_xramp, previous_xramp_ptree_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE_XRAMP_PTREE_XRAMP, ED_RAMP2_PTREE_XRAMP_PTREE_XRAMP, -1, -1, 2, 1, 2, false, 
   "ed_ramp_ptree_xramp_ptree_xramp", next_rampn_ptree_xramp_ptree_xramp, previous_rampn_ptree_xramp_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE_RAMP_PTREE_XRAMP2, ED_RAMP_PTREE_RAMP_PTREE_XRAMP2, -1, -1, 2, 1, 2, false, 
   "ed_ptree_ramp_ptree_xramp2", next_ptree_rampn_ptree_xramp, previous_ptree_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},

  {ED_XRAMP2_PTREE_RAMP2_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ptree_ramp2_ptree", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_PTREE_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp2_ptree_ramp2_ptree_zero", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP2_RAMP2_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp2_ramp2_ptree", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP2_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_ptree_xramp2_ramp2_ptree_zero", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP2_PTREE_RAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp2_ptree_ramp2", next_ptree_xramp_ptree_rampn, previous_ptree_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_RAMP2_PTREE_XRAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_ramp2_ptree_xramp2", next_ptree_rampn_ptree_xramp, previous_ptree_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},

  {ED_RAMP_PTREE_XRAMP2_RAMP_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree_xramp2_ramp_ptree", next_ramp_ptree_xramp_ramp_ptree, previous_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP2_RAMP_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_ramp_ptree_xramp2_ramp_ptree_zero", next_ramp_ptree_xramp_ramp_ptree, previous_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_RAMP2_PTREE_XRAMP2_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp2_ptree_xramp2_ptree", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE_XRAMP2_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_ramp2_ptree_xramp2_ptree_zero", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP_PTREE_RAMP_PTREE_XRAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree_ramp_ptree_xramp2", next_ramp_ptree_ramp_ptree_xramp, previous_ramp_ptree_ramp_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP2_PTREE_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree_xramp2_ptree_ramp", next_ramp_ptree_xramp_ptree_ramp, previous_ramp_ptree_xramp_ptree_ramp, NULL, NULL},
  {ED_XRAMP2_PTREE_RAMP_PTREE_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ptree_ramp_ptree_ramp", next_xramp_ptree_ramp_ptree_ramp, previous_xramp_ptree_ramp_ptree_ramp, NULL, NULL},
  {ED_PTREE_RAMP_PTREE_XRAMP2_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_ramp_ptree_xramp2_ramp", next_ptree_ramp_ptree_xramp_ramp, previous_ptree_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_XRAMP2_RAMP_PTREE_RAMP_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp2_ramp_ptree_ramp_ptree", next_xramp_ramp_ptree_ramp_ptree, previous_xramp_ramp_ptree_ramp_ptree, NULL, NULL},
  {ED_XRAMP2_RAMP_PTREE_RAMP_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp2_ramp_ptree_ramp_ptree_zero", next_xramp_ramp_ptree_ramp_ptree, previous_xramp_ramp_ptree_ramp_ptree, NULL, NULL},

  {ED_RAMP2_PTREE_RAMP_PTREE_RAMP, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp2_ptree_ramp_ptree_ramp", next_rampn_ptree_ramp_ptree_ramp, previous_rampn_ptree_ramp_ptree_ramp, next_ramp2_3_to_float, previous_ramp2_3_to_float},
  {ED_RAMP2_PTREE_RAMP_PTREE_XRAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp2_ptree_ramp_ptree_xramp", next_ramp2_ptree_ramp_ptree_xramp, previous_ramp2_ptree_ramp_ptree_xramp, NULL, NULL},
  {ED_RAMP2_PTREE_XRAMP_PTREE_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp2_ptree_xramp_ptree_ramp", next_ramp2_ptree_xramp_ptree_ramp, previous_ramp2_ptree_xramp_ptree_ramp, NULL, NULL},
  {ED_RAMP2_PTREE_XRAMP_PTREE_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp2_ptree_xramp_ptree_xramp", next_rampn_ptree_xramp_ptree_xramp, previous_rampn_ptree_xramp_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE_XRAMP_RAMP_PTREE, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp2_ptree_xramp_ramp_ptree", next_ramp2_ptree_xramp_ramp_ptree, previous_ramp2_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_RAMP2_PTREE_XRAMP_RAMP_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_ramp2_ptree_xramp_ramp_ptree_zero", next_ramp2_ptree_xramp_ramp_ptree, previous_ramp2_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_RAMP3_PTREE_XRAMP_PTREE,  -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp3_ptree_xramp_ptree", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP3_PTREE_XRAMP_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_ramp3_ptree_xramp_ptree_zero", next_rampn_ptree_xramp_ptree, previous_rampn_ptree_xramp_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP_PTREE_RAMP2_PTREE_RAMP, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp_ptree_ramp2_ptree_ramp", next_ramp_ptree_ramp2_ptree_ramp, previous_ramp_ptree_ramp2_ptree_ramp, NULL, NULL},
  {ED_RAMP_PTREE_RAMP2_PTREE_XRAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree_ramp2_ptree_xramp", next_ramp_ptree_ramp2_ptree_xramp, previous_ramp_ptree_ramp2_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE_RAMP_PTREE_RAMP2, -1, -1, -1, 2, 4, 0, false, 
   "ed_ramp_ptree_ramp_ptree_ramp2", next_ramp_ptree_ramp_ptree_ramp2, previous_ramp_ptree_ramp_ptree_ramp2, NULL, NULL},
  {ED_RAMP_PTREE_RAMP_PTREE_XRAMP_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree_ramp_ptree_xramp_ramp", next_ramp_ptree_ramp_ptree_xramp_ramp, previous_ramp_ptree_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_PTREE_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree_xramp_ptree_ramp2", next_ramp_ptree_xramp_ptree_ramp2, previous_ramp_ptree_xramp_ptree_ramp2, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_PTREE_XRAMP_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree_xramp_ptree_xramp_ramp", next_ramp_ptree_xramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ptree_xramp_ramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP2_PTREE,  -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree_xramp_ramp2_ptree", next_ramp_ptree_xramp_ramp2_ptree, previous_ramp_ptree_xramp_ramp2_ptree, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_ramp_ptree_xramp_ramp2_ptree_zero", next_ramp_ptree_xramp_ramp2_ptree, previous_ramp_ptree_xramp_ramp2_ptree, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP_PTREE_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ramp_ptree_xramp_ramp_ptree_ramp", next_ramp_ptree_xramp_ramp_ptree_ramp, previous_ramp_ptree_xramp_ramp_ptree_ramp, NULL, NULL},
  {ED_RAMP_PTREE_XRAMP_RAMP_PTREE_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ramp_ptree_xramp_ramp_ptree_xramp", next_ramp_ptree_xramp_ramp_ptree_xramp, previous_ramp_ptree_xramp_ramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_PTREE_RAMP2_PTREE_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ptree_ramp2_ptree_ramp", next_xramp_ptree_ramp2_ptree_ramp, previous_xramp_ptree_ramp2_ptree_ramp, NULL, NULL},
  {ED_XRAMP_PTREE_RAMP2_PTREE_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree_ramp2_ptree_xramp", next_xramp_ptree_rampn_ptree_xramp, previous_xramp_ptree_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_PTREE_RAMP3_PTREE,  -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ptree_ramp3_ptree", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE_RAMP3_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_xramp_ptree_ramp3_ptree_zero", next_xramp_ptree_rampn_ptree, previous_xramp_ptree_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE_RAMP_PTREE_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ptree_ramp_ptree_ramp2", next_xramp_ptree_ramp_ptree_ramp2, previous_xramp_ptree_ramp_ptree_ramp2, NULL, NULL},
  {ED_XRAMP_PTREE_RAMP_PTREE_XRAMP_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree_ramp_ptree_xramp_ramp", next_xramp_ptree_ramp_ptree_xramp_ramp, previous_xramp_ptree_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_XRAMP_PTREE_XRAMP_PTREE_RAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree_xramp_ptree_ramp2", next_xramp_ptree_xramp_ptree_rampn, previous_xramp_ptree_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE,  -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree_xramp_ramp2_ptree", next_xramp_ptree_xramp_rampn_ptree, previous_xramp_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp_ptree_xramp_ramp2_ptree_zero", next_xramp_ptree_xramp_rampn_ptree, previous_xramp_ptree_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ptree_xramp_ramp_ptree_ramp", next_xramp_ptree_xramp_ramp_ptree_ramp, previous_xramp_ptree_xramp_ramp_ptree_ramp, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE_RAMP_PTREE,  -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp2_ptree_ramp_ptree", next_xramp_ramp2_ptree_ramp_ptree, previous_xramp_ramp2_ptree_ramp_ptree, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE_RAMP_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_xramp_ramp2_ptree_ramp_ptree_zero", next_xramp_ramp2_ptree_ramp_ptree, previous_xramp_ramp2_ptree_ramp_ptree, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE,  -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp2_ptree_xramp_ptree", next_xramp_rampn_ptree_xramp_ptree, previous_xramp_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp_ramp2_ptree_xramp_ptree_zero", next_xramp_rampn_ptree_xramp_ptree, previous_xramp_rampn_ptree_xramp_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE_RAMP2_PTREE,  -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp_ptree_ramp2_ptree", next_xramp_ramp_ptree_ramp2_ptree, previous_xramp_ramp_ptree_ramp2_ptree, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_RAMP2_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_xramp_ramp_ptree_ramp2_ptree_zero", next_xramp_ramp_ptree_ramp2_ptree, previous_xramp_ramp_ptree_ramp2_ptree, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_RAMP_PTREE_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_xramp_ramp_ptree_ramp_ptree_ramp", next_xramp_ramp_ptree_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp_ptree_ramp, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_RAMP_PTREE_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp_ptree_ramp_ptree_xramp", next_xramp_ramp_ptree_ramp_ptree_xramp, previous_xramp_ramp_ptree_ramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp_ptree_xramp_ptree_ramp", next_xramp_ramp_ptree_xramp_ptree_ramp, previous_xramp_ramp_ptree_xramp_ptree_ramp, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE, -1, -1, -1, 2, 2, 2, false, 
   "ed_xramp_ramp_ptree_xramp_ramp_ptree", next_xramp_ramp_ptree_xramp_ramp_ptree, previous_xramp_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO, -1, -1, -1, 2, 2, 2, true, 
   "ed_xramp_ramp_ptree_xramp_ramp_ptree_zero", next_xramp_ramp_ptree_xramp_ramp_ptree, previous_xramp_ramp_ptree_xramp_ramp_ptree, NULL, NULL},
  {ED_PTREE_XRAMP_RAMP3_PTREE, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_xramp_ramp3_ptree", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_XRAMP_RAMP3_PTREE_ZERO, -1, -1, -1, 2, 3, 1, true, 
   "ed_ptree_xramp_ramp3_ptree_zero", next_ptree_xramp_rampn_ptree, previous_ptree_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},

  {ED_PTREE_RAMP2_PTREE_XRAMP_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_ramp2_ptree_xramp_ramp", next_ptree_ramp2_ptree_xramp_ramp, previous_ptree_ramp2_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE_RAMP3_PTREE_XRAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_ramp3_ptree_xramp", next_ptree_rampn_ptree_xramp, previous_ptree_rampn_ptree_xramp, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_RAMP4_PTREE, -1, -1, -1, 2, 4, 0, false, 
   "ed_ptree_ramp4_ptree", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_PTREE_RAMP4_PTREE_ZERO, -1, -1, -1, 2, 4, 0, true, 
   "ed_ptree_ramp4_ptree_zero", next_ptree_rampn_ptree, previous_ptree_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_PTREE_RAMP_PTREE_XRAMP_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_ramp_ptree_xramp_ramp2", next_ptree_ramp_ptree_xramp_ramp2, previous_ptree_ramp_ptree_xramp_ramp2, NULL, NULL},
  {ED_PTREE_XRAMP2_RAMP_PTREE_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp2_ramp_ptree_ramp", next_ptree_xramp_ramp_ptree_ramp, previous_ptree_xramp_ramp_ptree_ramp, NULL, NULL},
  {ED_PTREE_XRAMP_PTREE_RAMP3, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_xramp_ptree_ramp3", next_ptree_xramp_ptree_rampn, previous_ptree_xramp_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE_XRAMP_PTREE_XRAMP_RAMP2, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp_ptree_xramp_ramp2", next_ptree_xramp_ptree_xramp_rampn, previous_ptree_xramp_ptree_xramp_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP_RAMP2_PTREE_RAMP, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_xramp_ramp2_ptree_ramp", next_ptree_xramp_ramp2_ptree_ramp, previous_ptree_xramp_ramp2_ptree_ramp, NULL, NULL},
  {ED_PTREE_XRAMP_RAMP2_PTREE_XRAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp_ramp2_ptree_xramp", next_ptree_xramp_rampn_ptree_xramp, previous_ptree_xramp_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE_XRAMP_RAMP_PTREE_RAMP2, -1, -1, -1, 2, 3, 1, false, 
   "ed_ptree_xramp_ramp_ptree_ramp2", next_ptree_xramp_ramp_ptree_ramp2, previous_ptree_xramp_ramp_ptree_ramp2, NULL, NULL},
  {ED_PTREE_XRAMP_RAMP_PTREE_XRAMP_RAMP, -1, -1, -1, 2, 2, 2, false, 
   "ed_ptree_xramp_ramp_ptree_xramp_ramp", next_ptree_xramp_ramp_ptree_xramp_ramp, previous_ptree_xramp_ramp_ptree_xramp_ramp, NULL, NULL},

  /* ptree3 */
  {ED_PTREE3, ED_RAMP_PTREE3, ED_XRAMP_PTREE3, -1, 3, 0, 0, false, 
   "ed_ptree3", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE3_RAMP, ED_RAMP_PTREE3_RAMP, ED_XRAMP_PTREE3_RAMP, -1, 3, 1, 0, false, 
   "ed_ptree3_ramp", next_ptree_rampn, previous_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE3_XRAMP, ED_RAMP_PTREE3_XRAMP, ED_XRAMP_PTREE3_XRAMP, -1, 3, 0, 1, false, 
   "ed_ptree3_xramp", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_PTREE3_ZERO, ED_RAMP_PTREE3_ZERO, ED_XRAMP_PTREE3_ZERO, -1, 3, 0, 0, true, 
   "ed_ptree3_zero", next_ptree, previous_ptree, NULL, NULL},
  {ED_PTREE3_RAMP2, ED_RAMP_PTREE3_RAMP2, ED_XRAMP_PTREE3_RAMP2, -1, 3, 2, 0, false, 
   "ed_ptree3_ramp2", next_ptree_rampn, previous_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_PTREE3_RAMP3, ED_RAMP_PTREE3_RAMP3, ED_XRAMP_PTREE3_RAMP3, -1, 3, 3, 0, false, 
   "ed_ptree3_ramp3", next_ptree_rampn, previous_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_PTREE3_XRAMP_RAMP, ED_RAMP_PTREE3_XRAMP_RAMP, ED_XRAMP_PTREE3_XRAMP_RAMP, -1, 3, 1, 1, false, 
   "ed_ptree3_xramp_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_PTREE3_XRAMP2, ED_RAMP_PTREE3_XRAMP2, -1, -1, 3, 0, 2, false, 
   "ed_ptree3_xramp2", next_ptree_xramp, previous_ptree_xramp, NULL, NULL},
  {ED_RAMP_PTREE3, ED_RAMP2_PTREE3, ED_XRAMP_RAMP_PTREE3, -1, 3, 1, 0, false, 
   "ed_ramp_ptree3", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE3_ZERO, ED_RAMP2_PTREE3_ZERO, ED_XRAMP_RAMP_PTREE3_ZERO, -1, 3, 1, 0, true, 
   "ed_ramp_ptree3_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE3, ED_RAMP3_PTREE3, ED_XRAMP_RAMP2_PTREE3, -1, 3, 2, 0, false, 
   "ed_ramp2_ptree3", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP2_PTREE3_ZERO, ED_RAMP3_PTREE3_ZERO, ED_XRAMP_RAMP2_PTREE3_ZERO, -1, 3, 2, 0, true, 
   "ed_ramp2_ptree3_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE3, ED_RAMP4_PTREE3, ED_XRAMP_RAMP3_PTREE3, -1, 3, 3, 0, false, 
   "ed_ramp3_ptree3", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP3_PTREE3_ZERO, ED_RAMP4_PTREE3_ZERO, ED_XRAMP_RAMP3_PTREE3_ZERO, -1, 3, 3, 0, true, 
   "ed_ramp3_ptree3_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE3, ED_XRAMP_RAMP_PTREE3, ED_XRAMP2_PTREE3, -1, 3, 0, 1, false, 
   "ed_xramp_ptree3", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP_PTREE3_ZERO, ED_XRAMP_RAMP_PTREE3_ZERO, ED_XRAMP2_PTREE3_ZERO, -1, 3, 0, 1, true, 
   "ed_xramp_ptree3_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE3, ED_XRAMP2_RAMP_PTREE3, -1, -1, 3, 0, 2, false, 
   "ed_xramp2_ptree3", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_XRAMP2_PTREE3_ZERO, ED_XRAMP2_RAMP_PTREE3_ZERO, -1, -1, 3, 0, 2, true, 
   "ed_xramp2_ptree3_zero", next_xramp_ptree, previous_xramp_ptree, NULL, NULL},
  {ED_RAMP_PTREE3_RAMP, ED_RAMP2_PTREE3_RAMP, ED_XRAMP_RAMP_PTREE3_RAMP, -1, 3, 2, 0, false, 
   "ed_ramp_ptree3_ramp", next_ramp_ptree_ramp, previous_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE3_RAMP, ED_RAMP3_PTREE3_RAMP, ED_XRAMP_RAMP2_PTREE3_RAMP, -1, 3, 3, 0, false, 
   "ed_ramp2_ptree3_ramp", next_ramp2_ptree_ramp, previous_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE3_RAMP2, ED_RAMP2_PTREE3_RAMP2, ED_XRAMP_RAMP_PTREE3_RAMP2, -1, 3, 3, 0, false, 
   "ed_ramp_ptree3_ramp2", next_ramp_ptree_ramp2, previous_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP_PTREE3_XRAMP, ED_RAMP2_PTREE3_XRAMP, ED_XRAMP_RAMP_PTREE3_XRAMP, -1, 3, 1, 1, false, 
   "ed_ramp_ptree3_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE3_RAMP, ED_XRAMP_RAMP_PTREE3_RAMP, ED_XRAMP2_PTREE3_RAMP, -1, 3, 1, 1, false, 
   "ed_xramp_ptree3_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE3_XRAMP, ED_RAMP3_PTREE3_XRAMP, ED_XRAMP_RAMP2_PTREE3_XRAMP, -1, 3, 2, 1, false, 
   "ed_ramp2_ptree3_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE3, ED_XRAMP_RAMP2_PTREE3, ED_XRAMP2_RAMP_PTREE3, -1, 3, 1, 1, false, 
   "ed_xramp_ramp_ptree3", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE3_ZERO, ED_XRAMP_RAMP2_PTREE3_ZERO, ED_XRAMP2_RAMP_PTREE3_ZERO, -1, 3, 1, 1, true, 
   "ed_xramp_ramp_ptree3_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE3_XRAMP, ED_XRAMP_RAMP_PTREE3_XRAMP, -1, -1, 3, 0, 2, false, 
   "ed_xramp_ptree3_xramp", next_xramp_ptree_xramp, previous_xramp_ptree_xramp, NULL, NULL},
  {ED_XRAMP_RAMP2_PTREE3, ED_XRAMP_RAMP3_PTREE3, ED_XRAMP2_RAMP2_PTREE3, -1, 3, 2, 1, false, 
   "ed_xramp_ramp2_ptree3", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE3_ZERO, ED_XRAMP_RAMP3_PTREE3_ZERO, ED_XRAMP2_RAMP2_PTREE3_ZERO, -1, 3, 2, 1, true, 
   "ed_xramp_ramp2_ptree3_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE3, ED_XRAMP2_RAMP2_PTREE3, -1, -1, 3, 1, 2, false, 
   "ed_xramp2_ramp_ptree3", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_RAMP_PTREE3_ZERO, ED_XRAMP2_RAMP2_PTREE3_ZERO, -1, -1, 3, 1, 2, true, 
   "ed_xramp2_ramp_ptree3_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp_to_float, previous_ramp_to_float},
  {ED_PTREE3_XRAMP2_RAMP, ED_RAMP_PTREE3_XRAMP2_RAMP, -1, -1, 3, 1, 2, false, 
   "ed_ptree3_xramp2_ramp", next_ptree_xramp_ramp, previous_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE3_XRAMP_RAMP2, ED_RAMP_PTREE3_XRAMP_RAMP2, ED_XRAMP_PTREE3_XRAMP_RAMP2, -1, 3, 2, 1, false, 
   "ed_ptree3_xramp_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_PTREE3_RAMP2, ED_XRAMP_RAMP_PTREE3_RAMP2, ED_XRAMP2_PTREE3_RAMP2, -1, 3, 2, 1, false, 
   "ed_xramp_ptree3_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE3_RAMP, ED_XRAMP_RAMP2_PTREE3_RAMP, ED_XRAMP2_RAMP_PTREE3_RAMP, -1, 3, 2, 1, false, 
   "ed_xramp_ramp_ptree3_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_RAMP_PTREE3_XRAMP, ED_XRAMP_RAMP2_PTREE3_XRAMP, -1, -1, 3, 1, 2, false, 
   "ed_xramp_ramp_ptree3_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP2_PTREE3_RAMP, ED_XRAMP2_RAMP_PTREE3_RAMP, -1, -1, 3, 1, 2, false, 
   "ed_xramp2_ptree3_ramp", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp_to_float, previous_ramp_to_float},
  {ED_XRAMP_PTREE3_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE3_XRAMP_RAMP, -1, -1, 3, 1, 2, false, 
   "ed_xramp_ptree3_xramp_ramp", next_xramp_ptree_xramp_ramp, previous_xramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_RAMP_PTREE3_XRAMP_RAMP, ED_RAMP2_PTREE3_XRAMP_RAMP, ED_XRAMP_RAMP_PTREE3_XRAMP_RAMP, -1, 3, 2, 1, false, 
   "ed_ramp_ptree3_xramp_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_RAMP_PTREE3_XRAMP2, ED_RAMP2_PTREE3_XRAMP2, -1, -1, 3, 1, 2, false, 
   "ed_ramp_ptree3_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP4_PTREE3, -1, -1, -1, 3, 4, 0, false, 
   "ed_ramp4_ptree3", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP4_PTREE3_ZERO, -1, -1, -1, 3, 4, 0, true, 
   "ed_ramp4_ptree3_zero", next_rampn_ptree, previous_rampn_ptree, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_PTREE3_RAMP4, -1, -1, -1, 3, 4, 0, false, 
   "ed_ptree3_ramp4", next_ptree_rampn, previous_ptree_rampn, next_ramp4_to_float, previous_ramp4_to_float},
  {ED_RAMP3_PTREE3_RAMP, -1, -1, -1, 3, 4, 0, false, 
   "ed_ramp3_ptree3_ramp", next_ramp3_ptree_ramp, previous_ramp3_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE3_RAMP3, -1, -1, -1, 3, 4, 0, false, 
   "ed_ramp_ptree3_ramp3", next_ramp_ptree_ramp3, previous_ramp_ptree_ramp3, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE3_RAMP2, -1, -1, -1, 3, 4, 0, false, 
   "ed_ramp2_ptree3_ramp2", next_ramp2_ptree_ramp2, previous_ramp2_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_RAMP3_PTREE3_XRAMP, -1, -1, -1, 3, 3, 1, false, 
   "ed_ramp3_ptree3_xramp", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_PTREE3_RAMP3, -1, -1, -1, 3, 3, 1, false, 
   "ed_xramp_ptree3_ramp3", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_RAMP2_PTREE3_XRAMP2, -1, -1, -1, 3, 2, 2, false, 
   "ed_ramp2_ptree3_xramp2", next_rampn_ptree_xramp, previous_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_PTREE3_RAMP2, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp2_ptree3_ramp2", next_xramp_ptree_rampn, previous_xramp_ptree_rampn, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE3_XRAMP_RAMP, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp_ramp_ptree3_xramp_ramp", next_xramp_ramp_ptree_xramp_ramp, previous_xramp_ramp_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},
  {ED_XRAMP_PTREE3_XRAMP_RAMP2, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp_ptree3_xramp_ramp2", next_xramp_ptree_xramp_ramp2, previous_xramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_RAMP_PTREE3_XRAMP2_RAMP, -1, -1, -1, 3, 2, 2, false, 
   "ed_ramp_ptree3_xramp2_ramp", next_ramp_ptree_xramp_ramp, previous_ramp_ptree_xramp_ramp, NULL, NULL},
  {ED_PTREE3_XRAMP_RAMP3, -1, -1, -1, 3, 3, 1, false, 
   "ed_ptree3_xramp_ramp3", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp_ramp3_to_float, previous_xramp_ramp3_to_float},
  {ED_PTREE3_XRAMP2_RAMP2, -1, -1, -1, 3, 2, 2, false, 
   "ed_ptree3_xramp2_ramp2", next_ptree_xramp_rampn, previous_ptree_xramp_rampn, next_xramp2_ramp2_to_float, previous_xramp2_ramp2_to_float},
  {ED_XRAMP_RAMP3_PTREE3, -1, -1, -1, 3, 3, 1, false, 
   "ed_xramp_ramp3_ptree3", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP_RAMP3_PTREE3_ZERO, -1, -1, -1, 3, 3, 1, true, 
   "ed_xramp_ramp3_ptree3_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp3_to_float, previous_ramp3_to_float},
  {ED_XRAMP2_RAMP2_PTREE3, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp2_ramp2_ptree3", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP2_PTREE3_ZERO, -1, -1, -1, 3, 2, 2, true, 
   "ed_xramp2_ramp2_ptree3_zero", next_xramp_rampn_ptree, previous_xramp_rampn_ptree, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP_PTREE3_RAMP2, -1, -1, -1, 3, 3, 1, false, 
   "ed_xramp_ramp_ptree3_ramp2", next_xramp_ramp_ptree_ramp2, previous_xramp_ramp_ptree_ramp2, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE3_RAMP, -1, -1, -1, 3, 3, 1, false, 
   "ed_xramp_ramp2_ptree3_ramp", next_xramp_ramp2_ptree_ramp, previous_xramp_ramp2_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP_PTREE3_XRAMP_RAMP2, -1, -1, -1, 3, 3, 1, false, 
   "ed_ramp_ptree3_xramp_ramp2", next_ramp_ptree_xramp_ramp2, previous_ramp_ptree_xramp_ramp2, next_xramp_ramp2_to_float, previous_xramp_ramp2_to_float},
  {ED_XRAMP_RAMP2_PTREE3_XRAMP, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp_ramp2_ptree3_xramp", next_xramp_rampn_ptree_xramp, previous_xramp_rampn_ptree_xramp, next_ramp2_to_float, previous_ramp2_to_float},
  {ED_XRAMP2_RAMP_PTREE3_RAMP, -1, -1, -1, 3, 2, 2, false, 
   "ed_xramp2_ramp_ptree3_ramp", next_xramp_ramp_ptree_ramp, previous_xramp_ramp_ptree_ramp, next_ramp_to_float, previous_ramp_to_float},
  {ED_RAMP2_PTREE3_XRAMP_RAMP, -1, -1, -1, 3, 3, 1, false, 
   "ed_ramp2_ptree3_xramp_ramp", next_ramp2_ptree_xramp_ramp, previous_ramp2_ptree_xramp_ramp, next_xramp_ramp_to_float, previous_xramp_ramp_to_float},

};

static bool PTREE123_OP(int type)
{
  return(type_info[type].ptrees > 0);
}

static bool PTREE1_OP(int type)
{
  return(type_info[type].ptrees == 1);
}

static bool PTREE2_OP(int type)
{
  return(type_info[type].ptrees == 2);
}

static bool PTREE23_OP(int type)
{
  return(type_info[type].ptrees > 1);
}

static bool PTREE3_OP(int type)
{
  return(type_info[type].ptrees == 3);
}

static bool ZERO_OP(int type)
{
  return(type_info[type].ptree_zero);
}

static bool RAMP_OP(int type)
{
  return(type_info[type].xramps + type_info[type].ramps > 0);
}

#if MUS_DEBUGGING && 0
static int hit_entry[NUM_OPS];
static void init_hit_entries(void)
{
  int i;
  for (i = 0; i < NUM_OPS; i++) hit_entry[i] = 0;
}

static void report_unhit_entries(void)
{
  int i;
  for (i = 0; i < NUM_OPS; i++)
    if (hit_entry[i] == 0)
      {
	if (type_info[i].next)
	  fprintf(stderr, "accessible ");
	fprintf(stderr, "%s unhit\n", type_info[i].name);
      }
}

static void check_type_info_entry(int op, int expected_ramps, int expected_xramps, int expected_ptrees, bool is_zero)
{
  hit_entry[op]++;
  if (op != type_info[op].type) 
    fprintf(stderr, "%s type: %d %d\n", type_info[op].name, op, type_info[op].type);
  if (expected_ramps != type_info[op].ramps) 
    fprintf(stderr, "%s ramps: %d %d\n", type_info[op].name, expected_ramps, type_info[op].ramps);
  if (expected_xramps != type_info[op].xramps) 
    fprintf(stderr, "%s xramps: %d %d\n", type_info[op].name, expected_xramps, type_info[op].xramps);
  if (expected_ptrees != type_info[op].ptrees) 
    fprintf(stderr, "%s ptrees: %d %d\n", type_info[op].name, expected_ptrees, type_info[op].ptrees);
  if (op != ED_SIMPLE)
    {
      if (type_info[op].next == NULL) 
	fprintf(stderr, "%s no next\n", type_info[op].name);
      if (type_info[op].previous == NULL) 
	fprintf(stderr, "%s no previous\n", type_info[op].name);
    }
  if (is_zero != type_info[op].ptree_zero) 
    fprintf(stderr, "%s zero: %d %d\n", type_info[op].name, is_zero, type_info[op].ptree_zero);  
  if ((type_info[op].add_ramp != -1) && (type_info[op].add_ramp != op))
    check_type_info_entry(type_info[op].add_ramp, expected_ramps + 1, expected_xramps, expected_ptrees, is_zero);
  if ((type_info[op].add_xramp != -1) && (type_info[op].add_xramp != op))
    check_type_info_entry(type_info[op].add_xramp, expected_ramps, expected_xramps + 1, expected_ptrees, is_zero);
  if (type_info[op].add_ptree != -1) 
    check_type_info_entry(type_info[op].add_ptree, expected_ramps, expected_xramps, expected_ptrees + 1, is_zero);
  if (type_info[op].ptrees == -1)
    fprintf(stderr, "%s ptrees: %d\n", type_info[op].name, type_info[op].ptrees);  

  if (type_info[op].ramps + type_info[op].xramps < 4)
    {
      char *name;
      int i;
      name = strdup(type_info[op].name);
      name += 3;
      if (type_info[op].add_xramp == -1)
	if (type_info[op].xramps < 2)
	  fprintf(stderr, "ed_xramp_%s\n", name);
      if (type_info[op].add_ramp == -1)
	for (i = type_info[op].ramps + 1; i <= (4 - type_info[op].xramps); i++)
	  fprintf(stderr, "ed_ramp_%s\n", name);
    }
  if ((PTREE1_OP(op)) && (type_info[op].add_ptree == -1))
    {
      char *name;
      name = strdup(type_info[op].name);
      name += 3;
      fprintf(stderr, "ed_ptree_%s no ptree2?\n", name);
    }
}
#endif

static XEN empty_closure;

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

void read_sample_change_direction(snd_fd *sf, read_direction_t dir1) /* can't use "dir" on Mac */
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
  /* if ((XEN_BOUND_P(sf->closure1)) && (!(XEN_EQ_P(sf->closure1, empty_closure)))) */
  if (sf->protect1 != NOT_A_GC_LOC)
    {
      snd_unprotect_at(sf->protect1);
      sf->protect1 = NOT_A_GC_LOC;
    }
  sf->closure1 = empty_closure;
  if (XEN_PROCEDURE_P(proc))
    {
      sf->closure1 = XEN_CALL_2(proc,
				C_TO_XEN_OFF_T(sf->frag_pos + READER_PTREE_POSITION(sf)),
				C_TO_XEN_OFF_T(READER_PTREE_DUR(sf)),
				S_ptree_channel " init func");
      if (XEN_BOUND_P(sf->closure1))
	sf->protect1 = snd_protect(sf->closure1);
    }
}

static void get_sf_closure2(snd_fd *sf)
{
  XEN proc;
  proc = sf->cp->ptree_inits[READER_PTREE2_INDEX(sf)];
  /* if ((XEN_BOUND_P(sf->closure2)) && (!(XEN_EQ_P(sf->closure2, empty_closure)))) */
  if (sf->protect2 != NOT_A_GC_LOC)
    {
      snd_unprotect_at(sf->protect2);
      sf->protect2 = NOT_A_GC_LOC;
    }
  sf->closure2 = empty_closure;
  if (XEN_PROCEDURE_P(proc))
    {
      sf->closure2 = XEN_CALL_2(proc,
				C_TO_XEN_OFF_T(sf->frag_pos + READER_PTREE2_POSITION(sf)),
				C_TO_XEN_OFF_T(READER_PTREE2_DUR(sf)),
				S_ptree_channel " init func");
      if (XEN_BOUND_P(sf->closure2))
	sf->protect2 = snd_protect(sf->closure2);
    }
}

static void get_sf_closure3(snd_fd *sf)
{
  XEN proc;
  proc = sf->cp->ptree_inits[READER_PTREE3_INDEX(sf)];
  /* if ((XEN_BOUND_P(sf->closure3)) && (!(XEN_EQ_P(sf->closure3, empty_closure)))) */
  if (sf->protect3 != NOT_A_GC_LOC)
    {
      snd_unprotect_at(sf->protect3);
      sf->protect3 = NOT_A_GC_LOC;
    }
  sf->closure3 = empty_closure;
  if (XEN_PROCEDURE_P(proc))
    {
      sf->closure3 = XEN_CALL_2(proc,
				C_TO_XEN_OFF_T(sf->frag_pos + READER_PTREE3_POSITION(sf)),
				C_TO_XEN_OFF_T(READER_PTREE3_DUR(sf)),
				S_ptree_channel " init func");
      if (XEN_BOUND_P(sf->closure3))
	sf->protect3 = snd_protect(sf->closure3);
    }
}

static void setup_ramp(snd_fd *sf, double rmp0, double rmp1)
{
  if (READER_LOCAL_END(sf) == READER_LOCAL_POSITION(sf))
    sf->incr1 = 0.0;
  else sf->incr1 = (double)(rmp1 - rmp0) / (double)(READER_LOCAL_END(sf) - READER_LOCAL_POSITION(sf));
  sf->curval1 = rmp0 + sf->incr1 * sf->frag_pos;
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

static void setup_ramp4(snd_fd *sf, double rmp0, double rmp1)
{
  if (READER_LOCAL_END(sf) == READER_LOCAL_POSITION(sf))
    sf->incr4 = 0.0;
  else sf->incr4 = (double)(rmp1 - rmp0) / (double)(READER_LOCAL_END(sf) - READER_LOCAL_POSITION(sf));
  sf->curval4 = rmp0 + sf->incr4 * sf->frag_pos;
}

static mus_sample_t to_sample(snd_fd *sf) {return(MUS_FLOAT_TO_SAMPLE(read_sample_to_float(sf)));}

static void choose_accessor(snd_fd *sf)
{
  int typ;
  /* fragment-specific reader choice */
  /* most cases use floats */
  typ = READER_TYPE(sf);
  if (PTREE123_OP(typ))
    {
      sf->ptree1 = sf->cp->ptrees[READER_PTREE_INDEX(sf)];
      get_sf_closure(sf);
      if (PTREE23_OP(typ))
	{
	  sf->ptree2 = sf->cp->ptrees[READER_PTREE2_INDEX(sf)];
	  get_sf_closure2(sf);
	  if (PTREE3_OP(typ))
	    {
	      sf->ptree3 = sf->cp->ptrees[READER_PTREE3_INDEX(sf)];
	      get_sf_closure3(sf);
	    }
	  else sf->ptree3 = NULL;
	}
      else sf->ptree2 = NULL;
      sf->zero = ZERO_OP(typ);
      sf->xramp2 = (type_info[typ].xramps == 2);
    }
#if MUS_DEBUGGING
  if ((typ != ED_SIMPLE) && ((type_info[typ].next == NULL) || (type_info[typ].previous == NULL)))
    {
      fprintf(stderr,"got %s null accessor\n", type_info[typ].name);
      abort();
    }
#endif
  sf->run = to_sample;
  sf->rev_run = to_sample;
  sf->runf = type_info[typ].next;
  sf->rev_runf = type_info[typ].previous;
  sf->rampf = type_info[typ].rampf;
  sf->rev_rampf = type_info[typ].rev_rampf;
  switch (typ)
    {
    case ED_ZERO:
      sf->run = next_zero_sample;
      sf->rev_run = previous_zero_sample;
      break;
    case ED_PTREE: case ED_PTREE_ZERO:
    case ED_PTREE2: case ED_PTREE2_ZERO:
    case ED_PTREE3: case ED_PTREE3_ZERO:
      break;
    case ED_XRAMP:
    case ED_PTREE_XRAMP_PTREE: case ED_PTREE_XRAMP_PTREE_ZERO:
    case ED_PTREE_XRAMP:
    case ED_PTREE2_XRAMP: case ED_PTREE3_XRAMP:
    case ED_XRAMP_PTREE: case ED_XRAMP_PTREE_ZERO:
    case ED_XRAMP_PTREE2: case ED_XRAMP_PTREE2_ZERO:
    case ED_XRAMP_PTREE3: case ED_XRAMP_PTREE3_ZERO:
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP2:
    case ED_PTREE_XRAMP2:
    case ED_PTREE2_XRAMP2: case ED_PTREE3_XRAMP2:
    case ED_XRAMP2_PTREE: case ED_XRAMP2_PTREE_ZERO:
    case ED_XRAMP2_PTREE2: case ED_XRAMP2_PTREE2_ZERO:
    case ED_XRAMP2_PTREE3: case ED_XRAMP2_PTREE3_ZERO:
    case ED_XRAMP_PTREE_XRAMP:
    case ED_XRAMP_PTREE2_XRAMP: case ED_XRAMP_PTREE3_XRAMP:
    case ED_XRAMP_PTREE_XRAMP_PTREE: case ED_XRAMP_PTREE_XRAMP_PTREE_ZERO:
    case ED_PTREE_XRAMP2_PTREE: case ED_PTREE_XRAMP2_PTREE_ZERO:
    case ED_PTREE_XRAMP_PTREE_XRAMP:
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;

      /* scaler cases */
    case ED_RAMP:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      sf->run = next_sample_with_ramp;
      sf->rev_run = previous_sample_with_ramp;
      break;
    case ED_RAMP2:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      sf->run = next_sample_with_ramp2;
      sf->rev_run = previous_sample_with_ramp2;
      break;
    case ED_RAMP3:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      sf->run = next_sample_with_ramp3;
      sf->rev_run = previous_sample_with_ramp3;
      break;
    case ED_RAMP4:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      sf->run = next_sample_with_ramp4;
      sf->rev_run = previous_sample_with_ramp4;
      break;
    case ED_RAMP_PTREE: case ED_RAMP_PTREE_ZERO:
    case ED_RAMP_PTREE2: case ED_RAMP_PTREE2_ZERO:
    case ED_RAMP_PTREE3: case ED_RAMP_PTREE3_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      break;
    case ED_RAMP2_PTREE: case ED_RAMP2_PTREE_ZERO:
    case ED_RAMP2_PTREE2: case ED_RAMP2_PTREE2_ZERO:
    case ED_RAMP2_PTREE3: case ED_RAMP2_PTREE3_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      break;
    case ED_RAMP3_PTREE: case ED_RAMP3_PTREE_ZERO:
    case ED_RAMP3_PTREE2: case ED_RAMP3_PTREE2_ZERO:
    case ED_RAMP3_PTREE3: case ED_RAMP3_PTREE3_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_XRAMP_RAMP:
    case ED_RAMP_PTREE_XRAMP:
    case ED_RAMP_PTREE2_XRAMP: case ED_RAMP_PTREE3_XRAMP:
    case ED_RAMP_PTREE_XRAMP_PTREE: case ED_RAMP_PTREE_XRAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE2: case ED_XRAMP_RAMP_PTREE2_ZERO:
    case ED_XRAMP_RAMP_PTREE3: case ED_XRAMP_RAMP_PTREE3_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP2_RAMP:
    case ED_RAMP_PTREE_XRAMP2:
    case ED_RAMP_PTREE2_XRAMP2: case ED_RAMP_PTREE3_XRAMP2:
    case ED_XRAMP2_RAMP_PTREE: case ED_XRAMP2_RAMP_PTREE_ZERO:
    case ED_XRAMP2_RAMP_PTREE2: case ED_XRAMP2_RAMP_PTREE2_ZERO:
    case ED_XRAMP2_RAMP_PTREE3: case ED_XRAMP2_RAMP_PTREE3_ZERO:
    case ED_XRAMP_RAMP_PTREE_XRAMP:
    case ED_XRAMP_RAMP_PTREE2_XRAMP: case ED_XRAMP_RAMP_PTREE3_XRAMP:
    case ED_XRAMP_RAMP_PTREE_XRAMP_PTREE: case ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_ZERO:
    case ED_RAMP_PTREE_XRAMP2_PTREE: case ED_RAMP_PTREE_XRAMP2_PTREE_ZERO:
    case ED_RAMP_PTREE_XRAMP_PTREE_XRAMP:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP_RAMP2:
    case ED_XRAMP_RAMP2_PTREE: case ED_XRAMP_RAMP2_PTREE_ZERO:
    case ED_XRAMP_RAMP2_PTREE2: case ED_XRAMP_RAMP2_PTREE2_ZERO:
    case ED_XRAMP_RAMP2_PTREE3: case ED_XRAMP_RAMP2_PTREE3_ZERO:
    case ED_RAMP2_PTREE_XRAMP:
    case ED_RAMP2_PTREE2_XRAMP: case ED_RAMP2_PTREE3_XRAMP:
    case ED_RAMP2_PTREE_XRAMP_PTREE: case ED_RAMP2_PTREE_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP_RAMP2_PTREE_XRAMP:
    case ED_XRAMP_RAMP2_PTREE2_XRAMP: case ED_XRAMP_RAMP2_PTREE3_XRAMP:
    case ED_XRAMP2_RAMP2:
    case ED_XRAMP_RAMP3:
    case ED_RAMP4_PTREE: case ED_RAMP4_PTREE_ZERO:
    case ED_RAMP4_PTREE2: case ED_RAMP4_PTREE2_ZERO:
    case ED_RAMP4_PTREE3: case ED_RAMP4_PTREE3_ZERO:
    case ED_XRAMP_RAMP3_PTREE: case ED_XRAMP_RAMP3_PTREE_ZERO:
    case ED_XRAMP_RAMP3_PTREE2: case ED_XRAMP_RAMP3_PTREE2_ZERO:
    case ED_XRAMP_RAMP3_PTREE3: case ED_XRAMP_RAMP3_PTREE3_ZERO:
    case ED_XRAMP2_RAMP2_PTREE: case ED_XRAMP2_RAMP2_PTREE_ZERO:
    case ED_XRAMP2_RAMP2_PTREE2: case ED_XRAMP2_RAMP2_PTREE2_ZERO:
    case ED_XRAMP2_RAMP2_PTREE3: case ED_XRAMP2_RAMP2_PTREE3_ZERO:
    case ED_RAMP2_PTREE_XRAMP2:
    case ED_RAMP2_PTREE2_XRAMP2: case ED_RAMP2_PTREE3_XRAMP2:
    case ED_RAMP3_PTREE_XRAMP:
    case ED_RAMP3_PTREE2_XRAMP: case ED_RAMP3_PTREE3_XRAMP:
    case ED_RAMP2_PTREE_XRAMP2_PTREE: case ED_RAMP2_PTREE_XRAMP2_PTREE_ZERO:
    case ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE: case ED_XRAMP_RAMP2_PTREE_XRAMP_PTREE_ZERO:
    case ED_RAMP2_PTREE_XRAMP_PTREE_XRAMP:
    case ED_RAMP3_PTREE_XRAMP_PTREE: case ED_RAMP3_PTREE_XRAMP_PTREE_ZERO:
      setup_ramp(sf, READER_SCALER(sf) * READER_RAMP_BEG(sf), READER_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;

      /* ptree_scaler cases */
    case ED_PTREE_RAMP:
    case ED_PTREE2_RAMP:
    case ED_PTREE3_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      break;
    case ED_PTREE_RAMP2:
    case ED_PTREE2_RAMP2:
    case ED_PTREE3_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      break;
    case ED_PTREE_RAMP3:
    case ED_PTREE2_RAMP3:
    case ED_PTREE3_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_PTREE_RAMP4: case ED_PTREE2_RAMP4: case ED_PTREE3_RAMP4:
    case ED_PTREE_XRAMP_RAMP3: case ED_PTREE2_XRAMP_RAMP3: case ED_PTREE3_XRAMP_RAMP3:
    case ED_PTREE_XRAMP2_RAMP2: case ED_PTREE2_XRAMP2_RAMP2: case ED_PTREE3_XRAMP2_RAMP2:
    case ED_XRAMP_PTREE_RAMP3: case ED_XRAMP_PTREE2_RAMP3: case ED_XRAMP_PTREE3_RAMP3:
    case ED_XRAMP2_PTREE_RAMP2: case ED_XRAMP2_PTREE2_RAMP2: case ED_XRAMP2_PTREE3_RAMP2:
    case ED_XRAMP_PTREE_XRAMP_RAMP2: case ED_XRAMP_PTREE2_XRAMP_RAMP2: case ED_XRAMP_PTREE3_XRAMP_RAMP2:
    case ED_PTREE_XRAMP2_PTREE_RAMP2: 
    case ED_XRAMP_PTREE_XRAMP_PTREE_RAMP2:
    case ED_PTREE_XRAMP_PTREE_RAMP3:
    case ED_PTREE_XRAMP_PTREE_XRAMP_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP:
    case ED_RAMP_PTREE2_RAMP:
    case ED_RAMP_PTREE3_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      break;
    case ED_RAMP2_PTREE_RAMP:
    case ED_RAMP2_PTREE2_RAMP:
    case ED_RAMP2_PTREE3_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_RAMP3_PTREE_RAMP: case ED_RAMP3_PTREE2_RAMP: case ED_RAMP3_PTREE3_RAMP:
    case ED_XRAMP_RAMP2_PTREE_RAMP: case ED_XRAMP_RAMP2_PTREE2_RAMP: case ED_XRAMP_RAMP2_PTREE3_RAMP:
    case ED_XRAMP2_RAMP_PTREE_RAMP: case ED_XRAMP2_RAMP_PTREE2_RAMP: case ED_XRAMP2_RAMP_PTREE3_RAMP:
    case ED_XRAMP_RAMP_PTREE_XRAMP_RAMP: case ED_XRAMP_RAMP_PTREE2_XRAMP_RAMP: case ED_XRAMP_RAMP_PTREE3_XRAMP_RAMP:
    case ED_RAMP2_PTREE_XRAMP_RAMP: case ED_RAMP2_PTREE2_XRAMP_RAMP: case ED_RAMP2_PTREE3_XRAMP_RAMP:
    case ED_RAMP_PTREE_XRAMP2_RAMP: case ED_RAMP_PTREE2_XRAMP2_RAMP: case ED_RAMP_PTREE3_XRAMP2_RAMP:
    case ED_RAMP2_PTREE_XRAMP_PTREE_RAMP:
    case ED_RAMP_PTREE_XRAMP_PTREE_XRAMP_RAMP:
    case ED_RAMP_PTREE_XRAMP2_PTREE_RAMP:
    case ED_XRAMP_RAMP_PTREE_XRAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP2:
    case ED_RAMP_PTREE2_RAMP2:
    case ED_RAMP_PTREE3_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      break;
    case ED_RAMP2_PTREE_RAMP2:
    case ED_RAMP2_PTREE2_RAMP2:
    case ED_RAMP2_PTREE3_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP3:
    case ED_RAMP_PTREE2_RAMP3:
    case ED_RAMP_PTREE3_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_SCALER(sf) * READER_RAMP4_BEG(sf), READER_SCALER(sf) * READER_RAMP4_END(sf));
      break;
    case ED_XRAMP_PTREE_RAMP: case ED_XRAMP_PTREE2_RAMP: case ED_XRAMP_PTREE3_RAMP:
    case ED_PTREE_XRAMP_RAMP: case ED_PTREE2_XRAMP_RAMP: case ED_PTREE3_XRAMP_RAMP:
    case ED_PTREE_XRAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP2_PTREE_RAMP: case ED_XRAMP2_PTREE2_RAMP: case ED_XRAMP2_PTREE3_RAMP:
    case ED_PTREE_XRAMP2_RAMP: case ED_PTREE2_XRAMP2_RAMP: case ED_PTREE3_XRAMP2_RAMP:
    case ED_XRAMP_PTREE_XRAMP_RAMP: case ED_XRAMP_PTREE2_XRAMP_RAMP: case ED_XRAMP_PTREE3_XRAMP_RAMP:
    case ED_XRAMP_PTREE_XRAMP_PTREE_RAMP:
    case ED_PTREE_XRAMP_PTREE_XRAMP_RAMP:
    case ED_PTREE_XRAMP2_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP_RAMP_PTREE_RAMP: case ED_XRAMP_RAMP_PTREE2_RAMP: case ED_XRAMP_RAMP_PTREE3_RAMP:
    case ED_RAMP_PTREE_XRAMP_RAMP: case ED_RAMP_PTREE2_XRAMP_RAMP: case ED_RAMP_PTREE3_XRAMP_RAMP:
    case ED_RAMP_PTREE_XRAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_XRAMP_RAMP_PTREE_RAMP2: case ED_XRAMP_RAMP_PTREE2_RAMP2: case ED_XRAMP_RAMP_PTREE3_RAMP2:
    case ED_RAMP_PTREE_XRAMP_RAMP2: case ED_RAMP_PTREE2_XRAMP_RAMP2: case ED_RAMP_PTREE3_XRAMP_RAMP2:
    case ED_RAMP_PTREE_XRAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_XRAMP_RAMP2: case ED_PTREE2_XRAMP_RAMP2: case ED_PTREE3_XRAMP_RAMP2:
    case ED_XRAMP_PTREE_RAMP2: case ED_XRAMP_PTREE2_RAMP2: case ED_XRAMP_PTREE3_RAMP2:
    case ED_PTREE_XRAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;

      /* ptree2_scaler cases */
    case ED_PTREE_RAMP_PTREE: case ED_PTREE_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      break;
    case ED_PTREE_RAMP2_PTREE: case ED_PTREE_RAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      break;
    case ED_PTREE_RAMP3_PTREE: case ED_PTREE_RAMP3_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_PTREE_XRAMP_RAMP_PTREE: case ED_PTREE_XRAMP_RAMP_PTREE_ZERO:
    case ED_PTREE_RAMP_PTREE_XRAMP:
    case ED_XRAMP_PTREE_RAMP_PTREE: case ED_XRAMP_PTREE_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      break;
    case ED_XRAMP_PTREE_RAMP_PTREE_RAMP:
    case ED_PTREE_RAMP_PTREE_XRAMP_RAMP:
    case ED_PTREE_XRAMP_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_RAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_PTREE2_SCALER(sf) * READER_RAMP3_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP3_END(sf));
      break;
    case ED_PTREE_RAMP2_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP_PTREE: case ED_RAMP_PTREE_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      break;
    case ED_RAMP2_PTREE_RAMP_PTREE: case ED_RAMP2_PTREE_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP2_PTREE: case ED_RAMP_PTREE_RAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      break;
    case ED_XRAMP_PTREE_RAMP2_PTREE: case ED_XRAMP_PTREE_RAMP2_PTREE_ZERO:
    case ED_PTREE_XRAMP_RAMP2_PTREE: case ED_PTREE_XRAMP_RAMP2_PTREE_ZERO:
    case ED_PTREE_RAMP2_PTREE_XRAMP:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
    case ED_PTREE_XRAMP2_RAMP_PTREE: case ED_PTREE_XRAMP2_RAMP_PTREE_ZERO:
    case ED_XRAMP_PTREE_XRAMP_RAMP_PTREE: case ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_ZERO:
    case ED_XRAMP2_PTREE_RAMP_PTREE: case ED_XRAMP2_PTREE_RAMP_PTREE_ZERO:
    case ED_PTREE_XRAMP_RAMP_PTREE_XRAMP:
    case ED_XRAMP_PTREE_RAMP_PTREE_XRAMP:
    case ED_PTREE_RAMP_PTREE_XRAMP2: 
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_XRAMP_RAMP_PTREE: case ED_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_RAMP_PTREE_ZERO:
    case ED_RAMP_PTREE_RAMP_PTREE_XRAMP:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP2_PTREE_RAMP2_PTREE: case ED_RAMP2_PTREE_RAMP2_PTREE_ZERO:
    case ED_RAMP_PTREE_RAMP2_PTREE_XRAMP:
    case ED_RAMP_PTREE_XRAMP_RAMP2_PTREE: case ED_RAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO:
    case ED_XRAMP_RAMP_PTREE_RAMP2_PTREE: case ED_XRAMP_RAMP_PTREE_RAMP2_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP3_PTREE: case ED_RAMP_PTREE_RAMP3_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_SCALER(sf) * READER_RAMP4_BEG(sf), READER_SCALER(sf) * READER_RAMP4_END(sf));
      break;
    case ED_RAMP3_PTREE_RAMP_PTREE: case ED_RAMP3_PTREE_RAMP_PTREE_ZERO:
    case ED_RAMP_PTREE_RAMP_PTREE_XRAMP2:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_RAMP2_PTREE_RAMP2:
    case ED_XRAMP_PTREE_RAMP_PTREE_RAMP2:
    case ED_PTREE_RAMP_PTREE_XRAMP_RAMP2:
    case ED_PTREE_XRAMP_RAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_PTREE2_SCALER(sf) * READER_RAMP3_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_RAMP3_PTREE_RAMP:
    case ED_XRAMP2_PTREE_RAMP_PTREE_RAMP:
    case ED_PTREE_RAMP_PTREE_XRAMP2_RAMP:
    case ED_XRAMP_PTREE_XRAMP_RAMP_PTREE_RAMP:
    case ED_XRAMP_PTREE_RAMP2_PTREE_RAMP:
    case ED_XRAMP_PTREE_RAMP_PTREE_XRAMP_RAMP:
    case ED_PTREE_RAMP2_PTREE_XRAMP_RAMP:
    case ED_PTREE_XRAMP2_RAMP_PTREE_RAMP:
    case ED_PTREE_XRAMP_RAMP2_PTREE_RAMP:
    case ED_PTREE_XRAMP_RAMP_PTREE_XRAMP_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_PTREE_RAMP_PTREE_RAMP3:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_PTREE2_SCALER(sf) * READER_RAMP4_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP4_END(sf));
      break;
    case ED_XRAMP2_PTREE_RAMP2_PTREE: case ED_XRAMP2_PTREE_RAMP2_PTREE_ZERO:
    case ED_PTREE_XRAMP2_RAMP2_PTREE: case ED_PTREE_XRAMP2_RAMP2_PTREE_ZERO: case ED_PTREE_RAMP2_PTREE_XRAMP2:
    case ED_XRAMP_PTREE_RAMP2_PTREE_XRAMP:
    case ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE: case ED_XRAMP_PTREE_XRAMP_RAMP2_PTREE_ZERO:
    case ED_XRAMP_PTREE_RAMP3_PTREE: case ED_XRAMP_PTREE_RAMP3_PTREE_ZERO:
    case ED_PTREE_XRAMP_RAMP3_PTREE: case ED_PTREE_XRAMP_RAMP3_PTREE_ZERO: 
    case ED_PTREE_RAMP3_PTREE_XRAMP:
    case ED_PTREE_RAMP4_PTREE: case ED_PTREE_RAMP4_PTREE_ZERO:
    case ED_PTREE_XRAMP_RAMP2_PTREE_XRAMP:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_XRAMP2_RAMP_PTREE: case ED_RAMP_PTREE_XRAMP2_RAMP_PTREE_ZERO:
    case ED_XRAMP2_RAMP_PTREE_RAMP_PTREE: case ED_XRAMP2_RAMP_PTREE_RAMP_PTREE_ZERO:
    case ED_RAMP2_PTREE_RAMP_PTREE_XRAMP: case ED_RAMP_PTREE_XRAMP_RAMP_PTREE_XRAMP:
    case ED_XRAMP_RAMP_PTREE_RAMP_PTREE_XRAMP:
    case ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE: case ED_XRAMP_RAMP_PTREE_XRAMP_RAMP_PTREE_ZERO:
    case ED_XRAMP_RAMP2_PTREE_RAMP_PTREE: case ED_XRAMP_RAMP2_PTREE_RAMP_PTREE_ZERO:
    case ED_RAMP2_PTREE_XRAMP_RAMP_PTREE: case ED_RAMP2_PTREE_XRAMP_RAMP_PTREE_ZERO:
      setup_ramp(sf, READER_PTREE2_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_SCALER(sf) * READER_RAMP2_BEG(sf), READER_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP2_PTREE_RAMP_PTREE_RAMP:
    case ED_XRAMP_RAMP_PTREE_RAMP_PTREE_RAMP:
    case ED_RAMP_PTREE_RAMP_PTREE_XRAMP_RAMP:
    case ED_RAMP_PTREE_XRAMP_RAMP_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_SCALER(sf) * READER_RAMP3_BEG(sf), READER_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_RAMP4_BEG(sf), READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP2_PTREE_RAMP:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_PTREE2_SCALER(sf) * READER_RAMP2_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_RAMP3_BEG(sf), READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_SCALER(sf) * READER_RAMP4_BEG(sf), READER_SCALER(sf) * READER_RAMP4_END(sf));
      break;
    case ED_RAMP_PTREE_RAMP_PTREE_RAMP2:
      setup_ramp(sf, READER_PTREE_SCALER(sf) * READER_RAMP_BEG(sf), READER_PTREE_SCALER(sf) * READER_RAMP_END(sf));
      setup_ramp2(sf, READER_RAMP2_BEG(sf), READER_RAMP2_END(sf));
      setup_ramp3(sf, READER_PTREE2_SCALER(sf) * READER_RAMP3_BEG(sf), READER_PTREE2_SCALER(sf) * READER_RAMP3_END(sf));
      setup_ramp4(sf, READER_SCALER(sf) * READER_RAMP4_BEG(sf), READER_SCALER(sf) * READER_RAMP4_END(sf));
      break;

    case ED_SIMPLE:
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
    default:
#if MUS_DEBUGGING
      fprintf(stderr, "choose accessor: %d %s?\n", typ, (typ < NUM_OPS) ? type_info[typ].name : "unknown");
#endif
      break;
    }
  if (sf->direction == READ_BACKWARD) swap_readers(sf);
}



#define EDIT_LIST_END_MARK -2
#define EDIT_LIST_ZERO_MARK -1

enum {INSERTION_EDIT, DELETION_EDIT, CHANGE_EDIT, INITIALIZE_EDIT, SCALED_EDIT, ZERO_EDIT, RAMP_EDIT, PTREE_EDIT, EXTEND_EDIT};
static char *edit_names[10] = {"insert", "delete", "set", "init", "scale", "zero", "env", "ptree", "extend", ""};

static void display_ed_list(chan_info *cp, FILE *outp, int i, ed_list *ed, bool with_source)
{
  int len, j, index;
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
  switch (ed->edit_type)
    {
    case INSERTION_EDIT:  fprintf(outp, "\n (insert " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                        break;
    case DELETION_EDIT:   fprintf(outp, "\n (delete " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                        break;
    case CHANGE_EDIT:     fprintf(outp, "\n (set " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                           break;
    case SCALED_EDIT:     fprintf(outp, "\n (scale " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                         break;
    case ZERO_EDIT:       fprintf(outp, "\n (silence " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                       break;
    case RAMP_EDIT:       fprintf(outp, "\n (ramp " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);                          break;
    case PTREE_EDIT:      fprintf(outp, "\n (ptree[%d] " OFF_TD " " OFF_TD ") ", ed->ptree_location, ed->beg, ed->len); break; 
    case EXTEND_EDIT:     fprintf(outp, "\n (extend edit list with no-op)");                                            break;
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
	  int typ;
	  typ = FRAGMENT_TYPE(ed, j);
	  fprintf(outp, "\n   (at " OFF_TD ", cp->sounds[%d][" OFF_TD ":" OFF_TD ", %.3f",
		  FRAGMENT_GLOBAL_POSITION(ed, j),
		  index,
		  FRAGMENT_LOCAL_POSITION(ed, j),
		  FRAGMENT_LOCAL_END(ed, j),
		  FRAGMENT_SCALER(ed, j));
	  if (RAMP_OP(typ))
	    {
	      if (type_info[typ].ramps > 0)
		fprintf(outp, ", [1]%.3f -> %.3f", 
			FRAGMENT_RAMP_BEG(ed, j),
			FRAGMENT_RAMP_END(ed, j));
	      if (type_info[typ].ramps > 1)
		fprintf(outp, ", [2]%.3f -> %.3f", 
			FRAGMENT_RAMP2_BEG(ed, j),
			FRAGMENT_RAMP2_END(ed, j));
	      if ((type_info[typ].ramps > 2) || (type_info[typ].xramps == 2))
		fprintf(outp, ", [3]%.3f -> %.3f", 
			FRAGMENT_RAMP3_BEG(ed, j),
			FRAGMENT_RAMP3_END(ed, j));
	      if ((type_info[typ].ramps == 4) || (type_info[typ].xramps > 0))
		fprintf(outp, ", [4]%.3f -> %.3f", 
			FRAGMENT_RAMP4_BEG(ed, j),
			FRAGMENT_RAMP4_END(ed, j));
	      if (FRAGMENT_XRAMP_SCALER(ed, j) != 0.0)
		fprintf(outp, ", off: %.3f, scl: %.3f", 
			FRAGMENT_XRAMP_OFFSET(ed, j),
			FRAGMENT_XRAMP_SCALER(ed, j));
	      if (FRAGMENT_XRAMP_SCALER2(ed, j) != 0.0)
		fprintf(outp, ", off2: %.3f, scl2: %.3f", 
			FRAGMENT_XRAMP_OFFSET2(ed, j),
			FRAGMENT_XRAMP_SCALER2(ed, j));
	    }
	  if (PTREE123_OP(typ))
	    {
	      XEN code;
	      if (PTREE23_OP(typ))
		{
		  if (PTREE3_OP(typ))
		    {
		      fprintf(outp, ", loc3: %d, pos3: " OFF_TD ", scl3: %.3f",
			      FRAGMENT_PTREE3_INDEX(ed, j),
			      FRAGMENT_PTREE3_POSITION(ed, j),
			      FRAGMENT_PTREE3_SCALER(ed, j));
		    }
		  fprintf(outp, ", loc2: %d, pos2: " OFF_TD ", scl2: %.3f",
			  FRAGMENT_PTREE2_INDEX(ed, j),
			  FRAGMENT_PTREE2_POSITION(ed, j),
			  FRAGMENT_PTREE2_SCALER(ed, j));
		}
	      fprintf(outp, ", loc: %d, pos: " OFF_TD ", scl: %.3f",
		      FRAGMENT_PTREE_INDEX(ed, j),
		      FRAGMENT_PTREE_POSITION(ed, j),
		      (float)(MUS_FLOAT_TO_FIX * FRAGMENT_PTREE_SCALER(ed, j)));
	      if (with_source)
		{
		  code = ptree_code(cp->ptrees[FRAGMENT_PTREE_INDEX(ed, j)]);
		  if (XEN_LIST_P(code))
		    fprintf(outp, ", code: %s", XEN_AS_STRING(code));
#if HAVE_GUILE
		  /* procedure source unimplemented in gauche */
		  code = cp->ptree_inits[FRAGMENT_PTREE_INDEX(ed, j)];
		  if (XEN_PROCEDURE_P(code))
		    fprintf(outp, ", init: %s", XEN_AS_STRING(XEN_PROCEDURE_SOURCE(code)));
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
		    fprintf(outp, " [buf: " OFF_TD "] ", sd->data_bytes / sizeof(mus_sample_t));
		  else fprintf(outp, " [bogus!]");
	    }
	}
    }
  fprintf(outp, "\n");
}

off_t edit_changes_begin_at(chan_info *cp, int edpos)
{
  ed_list *ed, *old_ed;
  int len, old_len, i, min_len;
  old_ed = cp->edits[edpos - 1];
  ed = cp->edits[edpos];
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

off_t edit_changes_end_at(chan_info *cp, int edpos)
{
  ed_list *ed, *old_ed;
  int len, old_len;
  old_ed = cp->edits[edpos - 1];
  ed = cp->edits[edpos];
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
	    return(cp->samples[edpos - 1] - (FRAGMENT_LOCAL_END(ed, len) - FRAGMENT_LOCAL_POSITION(ed, len)));
	  len--;
	  old_len--;
	}
      else break;
    }
  return(0);
}


/* ---------------- edit list display, save, etc ---------------- */

static char edbuf[PRINT_BUFFER_SIZE];
char *edit_to_string(chan_info *cp, int edit)
{
  ed_list *ed;
  ed = cp->edits[edit];
  /* only for edit list in snd-xchn.c */
#if HAVE_FORTH
  mus_snprintf(edbuf, PRINT_BUFFER_SIZE, 
	       "%s : " OFF_TD " " OFF_TD " %s", 
	       ed->origin, 
	       ed->beg, ed->len,
	       edit_names[ed->edit_type]);
#else
  mus_snprintf(edbuf, PRINT_BUFFER_SIZE, 
#if HAVE_RUBY
	       "%s : %s(" OFF_TD ", " OFF_TD ")", 
#endif
#if HAVE_SCHEME
	       "%s : (%s " OFF_TD " " OFF_TD ")", 
#endif
	       ed->origin, 
	       edit_names[ed->edit_type], 
	       ed->beg, ed->len);
#endif
  return(edbuf);
}

static void display_edits(chan_info *cp, FILE *outp, bool with_source)
{
  int i;
  fprintf(outp, "\nEDITS: %d\n", cp->edit_ctr);
  for (i = 0; i <= cp->edit_ctr; i++)
    display_ed_list(cp, outp, i, cp->edits[i], with_source);
}

static io_error_t snd_make_file(const char *ofile, int chans, file_info *hdr, snd_fd **sfs, off_t length)
{
  /* create ofile, fill it by following sfs, use hdr for srate/type/format decisions */
  int ofd;
  int i, j, datumb;
  bool reporting = false;
  off_t len, total = 0;
  chan_info *cp = NULL;
  mus_sample_t **obufs;
  io_error_t io_err = IO_NO_ERROR;
  int sl_err = MUS_NO_ERROR;
  ofd = open_temp_file(ofile, chans, hdr, &io_err);
  if (ofd == -1) 
    return(io_err);
  mus_file_set_clipping(ofd, clipping(ss));
  datumb = mus_bytes_per_sample(hdr->format);
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
		  sl_err = mus_file_write(ofd, 0, j - 1, 1, obufs);
		  j = 0;
		  if (sl_err == -1) break;
		  if (reporting)
		    {
		      total += FILE_BUFFER_SIZE;
		      progress_report(cp->sound, NULL, 1, 1, (Float)((double)total / (double)length), NOT_FROM_ENVED);
		    }
		  check_for_event();
		  if ((ss->stopped_explicitly) || ((cp) && (!(cp->active))))
		    {
		      ss->stopped_explicitly = false;
		      snd_warning_without_format(_("file save cancelled by C-g"));
		      sl_err = MUS_INTERRUPTED;
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
	      sl_err = mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      if (sl_err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(cp->sound, NULL, 1, 1, (Float)((double)total / (double)length), NOT_FROM_ENVED);
		}
	      check_for_event();
	      if ((ss->stopped_explicitly) || ((cp) && (!(cp->active))))
		{
		  ss->stopped_explicitly = false;
		  snd_warning_without_format(_("file save cancelled by C-g"));
		  sl_err = MUS_INTERRUPTED;
		  break;
		}
	    }
	}
    }
  if ((sl_err == MUS_NO_ERROR) && (j > 0))
    sl_err = mus_file_write(ofd, 0, j - 1, chans, obufs);
  if (sl_err == MUS_NO_ERROR)
    {
      io_err = close_temp_file(ofile, ofd, hdr->type, len * chans * datumb);
      if (!(ss->fam_ok))
	alert_new_file();
    }
  else 
    {
      mus_file_close(ofd);
      io_err = sndlib_error_to_snd(sl_err);
    }
  if (reporting) finish_progress_report(cp->sound, NOT_FROM_ENVED);
  for (i = 0; i < chans; i++) FREE(obufs[i]);
  FREE(obufs);
  return(io_err);
}

static io_error_t channel_to_file(chan_info *cp, const char *ofile, int edpos)
{
  snd_info *sp;
  snd_fd **sf;
  io_error_t err = IO_NO_ERROR;
  sp = cp->sound;
  sf = (snd_fd **)MALLOC(sizeof(snd_fd *));
  sf[0] = init_sample_read_any(0, cp, READ_FORWARD, edpos);
  if (sf[0] == NULL)
    {
      FREE(sf);
      snd_error(_("no such edit: %s[%d]: %d (this channel has %d edit%s"),
		cp->sound->short_filename,
		cp->chan,
		edpos,
		cp->edit_ctr,
		(cp->edit_ctr == 1) ? "" : "s");
    }
  else
    {
      err = snd_make_file(ofile, 1, sp->hdr, sf, cp->samples[edpos]);
      free_snd_fd(sf[0]);
      FREE(sf);
      if ((err != IO_NO_ERROR) &&
	  (err != IO_INTERRUPTED))
	snd_error("can't save %s chan %d: %s %s", 
		  cp->sound->short_filename,
		  cp->chan,
		  ofile,
		  snd_io_strerror());
    }
  return(err);
}

/* these are used internally by the save-state process */
#define S_change_samples_with_origin    "change-samples-with-origin"
#define S_insert_samples_with_origin    "insert-samples-with-origin"
#define S_override_samples_with_origin  "override-samples-with-origin"

static void fprintf_with_possible_embedded_string(FILE *fd, const char *str)
{
  int i, len;
  len = snd_strlen(str);
  fputc('"', fd);
  for (i = 0; i < len; i++)
    {
      if (str[i] == '"')
	fputc('\\', fd);
      fputc(str[i], fd);
    }
  fputc('"', fd);
}

static char *edit_list_data_to_temp_file(chan_info *cp, ed_list *ed, file_delete_t delete_me, bool with_save_state_hook)
{
  snd_data *sd;
  char *ofile;
  if (with_save_state_hook)
    {
      char *nfile;
      nfile = shorter_tempnam(save_dir(ss), "snd_");
      ofile = run_save_state_hook(nfile);
      FREE(nfile);
    }
  else ofile = shorter_tempnam(save_dir(ss), "snd_");
  sd = cp->sounds[ed->sound_location];
  if (sd->type == SND_DATA_BUFFER)
    mus_array_to_file(ofile, sd->buffered_data, ed->len, 22050, 1);
  else 
    {
      io_error_t io_err;
      io_err = copy_file(sd->filename, ofile);
      if (io_err != IO_NO_ERROR)
	{
	  if (io_err == IO_CANT_OPEN_FILE)
	    snd_warning("%s edit list original temp file %s: %s", io_error_name(io_err), sd->filename, snd_io_strerror());
	  else snd_warning("%s edit list saved temp file %s: %s", io_error_name(io_err), ofile, snd_io_strerror());
	}
    }
  if (delete_me == DELETE_ME) remember_temp(ofile, 1); /* deletion upon exit (forget_temps) if a temp (edit-list->function, but not save-state) */
  return(ofile);
}
  
#if HAVE_FORTH
/*
 * ret_name: last word in origin (function name)
 *     func: rest-origin (must be freed)
 */
static char *split_origin(char *origin, char **ret_name)
{
  if (origin && *origin)
    {
      char *func = (char *)CALLOC(strlen(origin), sizeof(char));
      if ((*ret_name = strrchr(origin, ' ')))
	{
	  (*ret_name)++;
	  strncpy(func, origin, strlen(origin) - strlen(*ret_name) - 1);
	}
      else *ret_name = origin;
      return(func);
    }
  return NULL;
}
#endif

void edit_history_to_file(FILE *fd, chan_info *cp, bool with_save_state_hook)
{
  /* write edit list as a guile|ruby program to fd (open for writing) for subsequent load */
  /*   the entire current list is written, then the edit_ctr is fixed up to reflect its current state */
  int i, edits;
  ed_list *ed;
#if HAVE_FORTH
  char *forth_func = NULL;
#endif
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
	  if (ed->backed_up)
	    {
	      /* as-one-edit (and internally backup_edit_list) remove edit history entries,
	       * making it impossible to reconstruct exactly the edit sequence in save/restore.
	       * The backed_up flag is set in the backed-up entry, and for save/restore, we
	       * override the entire current sound with a saved file.
	       */
	      char *nfile = NULL, *ofile = NULL;
	      off_t len;
	      io_error_t io_err;
	      if (with_save_state_hook)
		{
		  ofile = shorter_tempnam(save_dir(ss), "snd_");
		  nfile = run_save_state_hook(ofile);
		  FREE(ofile);
		}
	      else nfile = shorter_tempnam(save_dir(ss), "snd_");
	      len = cp->samples[i];
	      io_err = channel_to_file(cp, nfile, i);

	      if (io_err != IO_NO_ERROR)
		{
		  /* error is trapped at lower level and pulled up via redirection */
		  FREE(nfile);
		  return;
		}
#if HAVE_RUBY
	      fprintf(fd, "      %s(\"%s\", " OFF_TD ", sfile, %d, ", TO_PROC_NAME(S_override_samples_with_origin), nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
 	      fprintf(fd, ", [%d, " OFF_TD "])\n",
 		      (int)mus_sound_write_date(nfile),
 		      mus_sound_length(nfile));
#endif
#if HAVE_SCHEME
	      fprintf(fd, "      (%s \"%s\" " OFF_TD " sfile %d ", S_override_samples_with_origin, nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
	      fprintf(fd, " (list %d " OFF_TD "))\n",
		      (int)mus_sound_write_date(nfile),
		      mus_sound_length(nfile));
#endif
#if HAVE_FORTH
	      fprintf(fd, "      \"%s\" " OFF_TD " sfile %d ", nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
 	      fprintf(fd, " '( %d " OFF_TD " ) %s drop\n",
 		      (int)mus_sound_write_date(nfile),
 		      mus_sound_length(nfile),
		      S_override_samples_with_origin);
#endif
	      FREE(nfile);
	    }
	  else
	    {
	      char *nfile = NULL;
#if HAVE_RUBY || HAVE_FORTH
	      fprintf(fd, "      ");
#endif
#if HAVE_SCHEME
	      fprintf(fd, "      (");
#endif
#if HAVE_FORTH
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* samp data snd chn */
		  forth_func = S_insert_samples_with_origin;
		  fprintf(fd, OFF_TD " " OFF_TD " ",
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"%s\"", S_insert_samples);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, " \"%s\" sfile %d", nfile, cp->chan);
		  break;
		case DELETION_EDIT:
		  /* samp samps snd chn */
		  forth_func = S_delete_samples;
		  fprintf(fd, OFF_TD " " OFF_TD " sfile %d",
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;
		case CHANGE_EDIT:
		  forth_func = S_change_samples_with_origin;
		  fprintf(fd, OFF_TD " " OFF_TD " ",
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"\"");
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, " \"%s\" sfile %d", nfile, cp->chan);
		  break;
		case EXTEND_EDIT:
		  /* not currently savable (this is a dummy edit fragment for zero-mix-drag position change) */
		  break;
		case ZERO_EDIT:
		  forth_func = S_pad_channel;
		  fprintf(fd, OFF_TD " " OFF_TD " sfile %d",
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;
		case SCALED_EDIT:
		case RAMP_EDIT:
		  {
		    char *func;
		    if ((func = split_origin(ed->origin, &forth_func)))
		      {
			fprintf(fd, "%s sfile %d", func, cp->chan);
			FREE(func);
		      }
		    else fprintf(fd, "sfile %d", cp->chan);
		  }
		  break;
		case PTREE_EDIT:
		  forth_func = S_ptree_channel;
		  if ((ed->origin) && (strcmp(ed->origin, S_ptree_channel) != 0))
		    {
		      char *func;
		      if ((func = split_origin(ed->origin, &forth_func)))
			{
			  fprintf(fd, "%s sfile %d", func, cp->chan);
			  FREE(func);
			}
		      else fprintf(fd, "sfile %d", cp->chan);
		    }
		  else
		    {
		      fprintf(fd, "%s " OFF_TD " " OFF_TD " sfile %d",
			      XEN_AS_STRING(ptree_code(cp->ptrees[ed->ptree_location])),
			      ed->beg,
			      ed->len,
			      cp->chan);
		    }
		  break;
		default:
		  snd_error("unknown edit branch: %s: %d %d",
			    ed->origin, 
			    ed->edit_type,
			    ed->sound_location);
		  break;
		}
#else
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* samp data snd chn */
		  fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP,
			  TO_PROC_NAME(S_insert_samples_with_origin),
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"%s\"", S_insert_samples);
		  fprintf(fd, PROC_SEP);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, "\"%s\"" PROC_SEP "sfile" PROC_SEP "%d", nfile, cp->chan);
		  break;
		case DELETION_EDIT:
		  /* samp samps snd chn */
		  fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP "sfile" PROC_SEP "%d",
			  TO_PROC_NAME(S_delete_samples),
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;
		case CHANGE_EDIT:
		  fprintf(fd, "%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP,
			  TO_PROC_NAME(S_change_samples_with_origin),
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"\"");
		  fprintf(fd, PROC_SEP);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, "\"%s\"" PROC_SEP "sfile" PROC_SEP "%d", nfile, cp->chan);
		  break;
		case EXTEND_EDIT:
		  /* not currently savable (this is a dummy edit fragment for zero-mix-drag position change) */
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
		case PTREE_EDIT:
		  if ((ed->origin) && (strcmp(ed->origin, S_ptree_channel) != 0))
		    fprintf(fd, "%s" PROC_SEP "sfile" PROC_SEP "%d",
			    ed->origin,
			    cp->chan);
		  else fprintf(fd, "%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP  OFF_TD PROC_SEP "sfile" PROC_SEP "%d",
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
#endif
	      if ((ed->edpos != AT_CURRENT_EDIT_POSITION) &&
		  (ed->edpos != (i - 1)))
		fprintf(fd, PROC_SEP " %d", ed->edpos);
#if HAVE_RUBY
	      else fprintf(fd, ", false");
#endif
#if HAVE_SCHEME || HAVE_FORTH
	      else fprintf(fd, " #f");
#endif
#if HAVE_GUILE
	      if (ed->edit_type == PTREE_EDIT)
		{
		  XEN code;
		  fprintf(fd, " %s", (ed->ptree_env_too) ? "#t" : "#f");
		  code = cp->ptree_inits[ed->ptree_location];
		  if (XEN_PROCEDURE_P(code))
		    fprintf(fd, " %s", XEN_AS_STRING(XEN_PROCEDURE_SOURCE(code)));
		}
#endif
	      if (nfile) 
		{
#if HAVE_SCHEME
		  fprintf(fd, " (list %d " OFF_TD ")",
			  (int)mus_sound_write_date(nfile),
			  mus_sound_length(nfile));
#endif
#if HAVE_RUBY
 		  fprintf(fd, ", [%d, " OFF_TD "]",
  			  (int)mus_sound_write_date(nfile),
  			  mus_sound_length(nfile));
#endif
#if HAVE_FORTH
		  fprintf(fd, " '( %d " OFF_TD " )",
			  (int)mus_sound_write_date(nfile),
			  mus_sound_length(nfile));
#endif
		  FREE(nfile);
		}
#if HAVE_FORTH
	      fprintf(fd, " %s drop\n", forth_func);
#else
	      fprintf(fd, ")\n"); /* works for both Ruby and Scheme */
#endif
	    }
	}
    }
  if (cp->edit_ctr < edits) 
#if HAVE_RUBY
    fprintf(fd, "      undo(%d, sfile, %d);\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
#if HAVE_SCHEME
    fprintf(fd, "      (undo %d sfile %d)\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
#if HAVE_FORTH
    fprintf(fd, "      %d sfile %d undo drop\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
    save_mark_list(fd, cp, false); /* false -> save just the current channel's marks */
}

static char *edit_list_to_function(chan_info *cp, int start_pos, int end_pos)
{
#if HAVE_SCHEME
  char *function = NULL, *old_function = NULL;
  bool close_mix_let = false;
  int i, edits;
  ed_list *ed;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  if ((end_pos > 0) && (end_pos < edits)) edits = end_pos;
  if (start_pos > edits)
    return(copy_string("(lambda (snd chn) #f)"));
  if (cp->have_mixes)
    {
      char *mix_list;
      mix_list = edit_list_mix_and_track_init(cp);
      if (mix_list)
	{
	  close_mix_let = true;
	  function = mus_format("(lambda (snd chn) (let (%s)", mix_list);
	  FREE(mix_list);
	}
      else function = copy_string("(lambda (snd chn)");
    }
  else function = copy_string("(lambda (snd chn)");
  for (i = start_pos; i <= edits; i++)
    {
      ed = cp->edits[i];
      if (ed)
	{
	  old_function = function;
	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change/ptree/xen cases, there's basically no choice */
	  if (ed->backed_up)
	    {
	      if ((ed->origin) && (strncmp(ed->origin, "set!", 4) == 0))
		function = mus_format("%s (%s)", function, ed->origin);
	      else function = mus_format("%s (%s snd chn)", function, ed->origin);
	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* this and change_edit are not bullet-proof -- there are many ways an incomplete
		   *   origin can get here, but we want to trap the mix setters.  In save-state above,
		   *   origin is just ignored, which is also less than ideal, but there are cases
		   *   (map-channel for example) where the lambda form can't be saved correctly,
		   *   so "the right thing" is not reachable.  Here, perhaps the strcmp should
		   *   check for "set! -mix" or "set! (".  But then the track stuff is lost...
		   */
		  if ((!(ed->origin)) || (strcmp(ed->origin, S_insert_samples) == 0))
		    {
		      /* save data in temp file, use insert-samples with file name */
		      char *ofile;
		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
		      function = mus_format("%s (%s " OFF_TD " " OFF_TD " \"%s\" snd chn)", function, S_insert_samples, ed->beg, ed->len, ofile);
		      FREE(ofile);
		    }
		  else function = mus_format("%s (%s snd chn)", function, ed->origin);
		  break;
		case CHANGE_EDIT:
		  if ((!(ed->origin)) || (strcmp(ed->origin, "set-samples") == 0))
		    {
		      /* save data in temp file, use set-samples with file name */
		      char *ofile;
		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
		      function = mus_format("%s (set-samples " OFF_TD " " OFF_TD " \"%s\" snd chn)", function, ed->beg, ed->len, ofile);
		      FREE(ofile);
		    }
		  else
		    {
		      if (strncmp(ed->origin, "set!", 4) == 0)
			function = mus_format("%s (%s)", function, ed->origin);
		      else function = mus_format("%s (%s snd chn)", function, ed->origin);
		    }
		  break;
		case DELETION_EDIT:
		  /* what about delete-mix? */
		  function = mus_format("%s (%s " OFF_TD " " OFF_TD " snd chn)", function, S_delete_samples, ed->beg, ed->len);
		  break;
		case SCALED_EDIT: 
		  function = mus_format("%s (%s snd chn)", function, ed->origin);
		  break;
		case EXTEND_EDIT:
		  /* mix drag case */
		  break;
		case PTREE_EDIT:
		  if ((ed->origin) && (strcmp(ed->origin, S_ptree_channel) != 0))
		    function = mus_format("%s (%s snd chn)", function, ed->origin);
		  else 
		    {
		      char *durstr;
		      if (ed->len == cp->samples[i])
			durstr = copy_string("#f");
		      else durstr = mus_format(OFF_TD, ed->len);
		      function = mus_format("%s (%s %s " OFF_TD " %s snd chn)",
					    function, S_ptree_channel,
					    XEN_AS_STRING(ptree_code(cp->ptrees[ed->ptree_location])),
					    ed->beg, durstr);
		      FREE(durstr);
		    }
		  break;
		case RAMP_EDIT:
		  function = mus_format("%s (%s snd chn)", function, ed->origin);
		  break;
		case ZERO_EDIT:
		  /* origin here is useless (see extend_with_zeros cases) */
		  function = mus_format("%s (%s " OFF_TD " " OFF_TD " snd chn)", function, S_pad_channel, ed->beg, ed->len);
		  break;
		}
	    }
	  if (old_function) {FREE(old_function); old_function = NULL;}
	}
    }
  old_function = function;
  if (close_mix_let)
    function = mus_format("%s))", function);
  else function = mus_format("%s)", function);
  FREE(old_function);
  return(function);
#endif

#if HAVE_RUBY
  char *function = NULL, *old_function = NULL;
  bool close_mix_let = false, first = true;
  int i, edits;
  ed_list *ed;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  if ((end_pos > 0) && (end_pos < edits)) edits = end_pos;
  if (start_pos > edits)
    return(copy_string("Proc.new {|snd, chn| false }"));
  if (cp->have_mixes)
    {
      char *mix_list;
      mix_list = edit_list_mix_and_track_init(cp);
      if (mix_list)
	{
	  close_mix_let = true;
	  function = mus_format("Proc.new {|snd, chn| %s; ", mix_list);
	  FREE(mix_list);
	}
      else function = copy_string("Proc.new {|snd, chn| ");
    }
  else function = copy_string("Proc.new {|snd, chn| ");
  for (i = start_pos; i <= edits; i++)
    {
      ed = cp->edits[i];
      if (ed)
	{
	  old_function = function;
	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change/ptree/xen cases, there's basically no choice */
	  if (ed->backed_up)
  	    {
 	      if ((ed->origin) &&
 		  ((strncmp(ed->origin, "set_mix", 7) == 0) ||
 		   (strncmp(ed->origin, "set_track", 9) == 0)))
  		function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
 	      else function = mus_format("%s%s %s%ssnd, chn)",
 					 function,
 					 (first) ? "" : ";",
 					 ed->origin,
 					 (ed->origin[snd_strlen(ed->origin) - 1] == '(') ? "" : ", ");
  	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
 		  if ((!(ed->origin)) || (strcmp(ed->origin, TO_PROC_NAME(S_insert_samples)) == 0))
 		    {
 		      /* from HAVE_SCHEME above */
 		      /* save data in temp file, use insert-samples with file name */
 		      char *ofile;
 		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
 		      function = mus_format("%s %s(" OFF_TD ", " OFF_TD ", \"%s\", snd, chn)", 
					    function, TO_PROC_NAME(S_insert_samples), ed->beg, ed->len, ofile);
 		      FREE(ofile);
 		    }
 		  else function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
  		  break;
  		case CHANGE_EDIT:
 		  if ((!(ed->origin)) || (strcmp(ed->origin, "set-samples") == 0))
 		    {
 		      /* from HAVE_SCHEME above */
 		      /* save data in temp file, use set-samples with file name */
 		      char *ofile;
 		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
 		      function = mus_format("%s set_samples(" OFF_TD ", " OFF_TD ", \"%s\", snd, chn)", 
					    function, ed->beg, ed->len, ofile);
 		      FREE(ofile);
 		    }
 		  else if ((ed->origin) &&
 			   ((strncmp(ed->origin, "set_mix", 7) == 0) ||
 			    (strncmp(ed->origin, "set_track", 9) == 0)))
 		    function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
 		  else function = mus_format("%s%s %s%ssnd, chn)",
 					     function,
 					     (first) ? "" : ";",
 					     ed->origin,
 					     (ed->origin[snd_strlen(ed->origin) - 1] == '(') ? "" : ", ");
		  break;
		case DELETION_EDIT:
		  /* what about delete-mix? */
		  function = mus_format("%s%s %s(" OFF_TD ", " OFF_TD ", snd, chn)", 
					function, (first) ? "" : ";", TO_PROC_NAME(S_delete_samples), ed->beg, ed->len);
		  break;
		case SCALED_EDIT: 
		  function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
		  break;
		case EXTEND_EDIT:
		  /* mix drag case */
		  break;
		case RAMP_EDIT:
		  function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
		  break;
		case ZERO_EDIT:
		  /* origin here is useless (see extend_with_zeros cases) */
		  function = mus_format("%s%s %s(" OFF_TD ", " OFF_TD ", snd, chn)", 
					function, (first) ? "" : ";", TO_PROC_NAME(S_pad_channel), ed->beg, ed->len);
		  break;
		}
	    }
	  if (old_function) {FREE(old_function); old_function = NULL;}
	}
      first = false;
    }
  old_function = function;
  if (close_mix_let)
    function = mus_format("%s }", function);
  else function = mus_format("%s }", function);
  FREE(old_function);
  return(function);
#endif

#if HAVE_FORTH
  char *function = NULL, *old_function = NULL;
  bool close_mix_let = false, first = true;
  int i, edits;
  ed_list *ed;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  if ((end_pos > 0) && (end_pos < edits)) edits = end_pos;
  if (start_pos > edits)
    return(copy_string("lambda: <{ snd chn -- val }> #f ;"));
  if (cp->have_mixes)
    {
      char *mix_list;
      mix_list = edit_list_mix_and_track_init(cp);
      if (mix_list)
	{
	  close_mix_let = true;
	  function = mus_format("lambda: <{ snd chn -- val }> %s", mix_list);
	  FREE(mix_list);
	}
      else function = copy_string("lambda: <{ snd chn -- val }>");
    }
  else function = copy_string("lambda: <{ snd chn -- val }>");
  for (i = start_pos; i <= edits; i++)
    {
      ed = cp->edits[i];
      if ((ed->origin) && strstr(ed->origin, "make-track"))
	continue;
      if (ed)
	{
	  old_function = function;
	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change/ptree/xen cases, there's basically no choice */
	  if (ed->backed_up)
	    {
	      char *name, *func;
	      func = split_origin(ed->origin, &name);
	      if ((name) && (strncmp(name, "set-", 4) == 0))
		function = mus_format("%s %s drop", function, ed->origin);
	      else if ((ed->origin) && strstr(ed->origin, "mix-selection"))
		function = mus_format("%s %s", function, ed->origin);
	      else
		{
		  if (func)
		    function = mus_format("%s %s snd chn %s drop", function, func, name);
		  else function = mus_format("%s snd chn %s drop", function, name);
		}
	      if (func) FREE(func);
	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case CHANGE_EDIT:
		  {
		    char *name, *func;
		    func = split_origin(ed->origin, &name);
		    if ((name) && (strncmp(name, "set-", 4) == 0))
		      function = mus_format("%s %s drop", function, ed->origin);
		    else if ((ed->origin) && strstr(ed->origin, "mix-selection"))
		      function = mus_format("%s %s", function, ed->origin);
		    else
		      {
			if (func)
			  function = mus_format("%s %s snd chn %s drop", function, func, name);
			else function = mus_format("%s snd chn %s drop", function, name);
		      }
		    if (func) FREE(func);
		  }
		  break;
		case DELETION_EDIT:
		  /* what about delete-mix? */
		  function = mus_format("%s " OFF_TD " " OFF_TD " snd chn %s drop", 
					function, ed->beg, ed->len, S_delete_samples);
		  break;
		case INSERTION_EDIT: 
		case SCALED_EDIT: 
		case RAMP_EDIT:
		  {
		    char *name, *func;
		    if ((func = split_origin(ed->origin, &name)))
		      {
			function = mus_format("%s %s snd chn %s drop", function, func, name);
			FREE(func);
		      }
		    else function = mus_format("%s snd chn %s drop", function, name);
		  }
		  break;
		case EXTEND_EDIT:
		  /* mix drag case */
		  break;
		case ZERO_EDIT:
		  /* origin here is useless (see extend_with_zeros cases) */
		  function = mus_format("%s " OFF_TD " " OFF_TD " snd chn %s drop", 
					function, ed->beg, ed->len, S_pad_channel);
		  break;
		}
	    }
	  if (old_function) {FREE(old_function); old_function = NULL;}
	}
      first = false;
    }
  old_function = function;
  function = mus_format("%s ;", function);
  FREE(old_function);
  return(function);
#endif
}

#define copy_ed_fragment(New_Ed, Old_Ed) memcpy((void *)(New_Ed), (void *)(Old_Ed), sizeof(ed_fragment))

static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  int i;
  ed = (ed_list *)CALLOC(1, sizeof(ed_list));
  ed->size = size;
  ed->allocated_size = size;
  ed->fragments = (ed_fragment **)malloc(size * sizeof(ed_fragment *));
  for (i = 0; i < size; i++)
    FRAGMENT(ed, i) = (ed_fragment *)calloc(1, sizeof(ed_fragment)); /* "calloc" removes this from the memory tracker -- it's glomming up everything */
  ed->origin = NULL;
  ed->maxamp = -1.0;
  ed->maxamp_position = -1;
  ed->selection_maxamp = -1.0;
  ed->selection_maxamp_position = -1;
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

void set_ed_maxamp_position(chan_info *cp, int edpos, off_t val)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  ed->maxamp_position = val;
}

off_t ed_maxamp_position(chan_info *cp, int edpos)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  return(ed->maxamp_position);
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

void set_ed_selection_maxamp_position(chan_info *cp, off_t val)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  ed->selection_maxamp_position = val;
}

off_t ed_selection_maxamp_position(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return(ed->selection_maxamp_position);
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
	      free(FRAGMENT(ed, i));
	  free(FRAGMENTS(ed));
	}
      if (ed->origin) FREE(ed->origin);
      if (ed->edit_type == PTREE_EDIT)
	{
	  int loc;
	  loc = ed->ptree_location;
	  if (cp->ptrees[loc])
	    {
	      free_ptree(cp->ptrees[loc]);
	      cp->ptrees[loc] = NULL;
	    }
	  if (XEN_PROCEDURE_P(cp->ptree_inits[loc]))
	    {
	      snd_unprotect_at(cp->init_locs[loc]);
	      cp->init_locs[loc] = -1;
	      cp->ptree_inits[loc] = XEN_FALSE;
	    }
	}
      FREE(ed);
    }
  return(NULL);
}

void backup_edit_list(chan_info *cp)
{
  int cur, i;
  ed_list *old_ed, *new_ed;
  cur = cp->edit_ctr;
  if (cur <= 0) return;
  new_ed = cp->edits[cur];
  old_ed = cp->edits[cur - 1];
  new_ed->edpos = old_ed->edpos;
  new_ed->backed_up = true;

  /* make sure backup_edit_list (as-one-edit) doesn't clobber our ptrees */
  /* this puts off gc of un-needed ptrees until close time -- a bit wasteful. */
  /*   it might be enough to save the second tree loc in ptree2 cases, and include it in the block above */
  if (old_ed->edit_type == PTREE_EDIT)
    old_ed->edit_type = ED_SIMPLE;
  free_ed_list(old_ed, cp);
  old_ed = NULL;

  free_amp_env(cp, cur - 1);
  cp->edits[cur - 1] = new_ed;
  cp->amp_envs[cur - 1] = cp->amp_envs[cur];
  if (cp->tracks)
    {
      if (cp->tracks[cur - 1]) free_track_info(cp, cur - 1);
      cp->tracks[cur - 1] = cp->tracks[cur];
      cp->tracks[cur] = NULL;
    }
  cp->edits[cur] = NULL;
  cp->amp_envs[cur] = NULL;
  cp->samples[cur - 1] = cp->samples[cur];
  if (cp->sounds) /* protect from release_pending_sounds upon edit after undo after as-one-edit or whatever */
    for (i = 0; i < cp->sound_size; i++)
      {
	snd_data *sd;
	sd = cp->sounds[i];
	if ((sd) && (sd->edit_ctr == cur)) sd->edit_ctr--;
      }
  backup_mark_list(cp, cur);
  cp->edit_ctr--;
  reflect_edit_history_change(cp);
}

void free_edit_list(chan_info *cp)
{
  if (cp)
    {
      int i;
      if (cp->edits)
	{
	  for (i = 0; i < cp->edit_size; i++)
	    if (cp->edits[i]) 
	      free_ed_list(cp->edits[i], cp);
	  FREE(cp->edits);
	  cp->edits = NULL;
	}
      if (cp->amp_envs)
	{
	  for (i = 0; i < cp->edit_size; i++)
	    free_amp_env(cp, i);
	  FREE(cp->amp_envs); 
	  cp->amp_envs = NULL;
	}
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

snd_info *sound_is_silence(snd_info *sp)
{
  int i;
  if (sp)
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  chan_info *cp;
	  ed_list *ed;
	  cp = sp->chans[i];
	  ed = cp->edits[0];
	  FRAGMENT_SCALER(ed, 0) = 0.0;
	  FRAGMENT_TYPE(ed, 0) = ED_ZERO;
	}
    }
  return(sp);
}

void allocate_ed_list(chan_info *cp) 
{
  cp->edits = (ed_list **)CALLOC(cp->edit_size, sizeof(ed_list *));
  cp->tracks = (track_info **)CALLOC(cp->edit_size, sizeof(track_info *));
}

static void new_leading_ramp(ed_fragment *new_start, ed_fragment *old_start, off_t samp)
{
  if (RAMP_OP(ED_TYPE(old_start)))
    {
      Float rmp1, rmp0;
      Float val;
      double xpos = 0.0;
      rmp0 = ED_RAMP_BEG(old_start);
      rmp1 = ED_RAMP_END(old_start);
      if (ED_LOCAL_END(old_start) == ED_LOCAL_POSITION(old_start))
	val = rmp0;
      else 
	{
	  xpos = (double)(samp - 1 - ED_GLOBAL_POSITION(old_start)) / (double)(ED_LOCAL_END(old_start) - ED_LOCAL_POSITION(old_start));
	  val = rmp0 + (rmp1 - rmp0) * xpos;
	}
      ED_RAMP_BEG(new_start) = rmp0;
      ED_RAMP_END(new_start) = val;

      ED_XRAMP_OFFSET(new_start) = ED_XRAMP_OFFSET(old_start);
      ED_XRAMP_SCALER(new_start) = ED_XRAMP_SCALER(old_start);
      ED_XRAMP_OFFSET2(new_start) = ED_XRAMP_OFFSET2(old_start);
      ED_XRAMP_SCALER2(new_start) = ED_XRAMP_SCALER2(old_start);

      rmp0 = ED_RAMP2_BEG(old_start);
      rmp1 = ED_RAMP2_END(old_start);
      ED_RAMP2_BEG(new_start) = rmp0;
      ED_RAMP2_END(new_start) = rmp0 + (rmp1 - rmp0) * xpos;

      rmp0 = ED_RAMP3_BEG(old_start);
      rmp1 = ED_RAMP3_END(old_start);
      ED_RAMP3_BEG(new_start) = rmp0;
      ED_RAMP3_END(new_start) = rmp0 + (rmp1 - rmp0) * xpos;

      rmp0 = ED_RAMP4_BEG(old_start);
      rmp1 = ED_RAMP4_END(old_start);
      ED_RAMP4_BEG(new_start) = rmp0;
      ED_RAMP4_END(new_start) = rmp0 + (rmp1 - rmp0) * xpos;
    }
}

static void new_trailing_ramp(ed_fragment *new_back, ed_fragment *old_back, off_t samp)
{
  if (RAMP_OP(ED_TYPE(old_back)))
    {
      Float rmp1, rmp0;
      Float val;
      double xpos = 0.0;
      rmp0 = ED_RAMP_BEG(old_back);
      rmp1 = ED_RAMP_END(old_back);
      if (ED_LOCAL_END(old_back) == ED_LOCAL_POSITION(old_back))
	val = rmp0;
      else 
	{
	  xpos = (double)(samp - ED_GLOBAL_POSITION(old_back)) / (double)(ED_LOCAL_END(old_back) - ED_LOCAL_POSITION(old_back));
	  val = rmp0 + (rmp1 - rmp0) * xpos;
	}
      ED_RAMP_BEG(new_back) = val;
      ED_RAMP_END(new_back) = rmp1;

      ED_XRAMP_OFFSET(new_back) = ED_XRAMP_OFFSET(old_back);
      ED_XRAMP_SCALER(new_back) = ED_XRAMP_SCALER(old_back);
      ED_XRAMP_OFFSET2(new_back) = ED_XRAMP_OFFSET2(old_back);
      ED_XRAMP_SCALER2(new_back) = ED_XRAMP_SCALER2(old_back);

      rmp0 = ED_RAMP2_BEG(old_back);
      rmp1 = ED_RAMP2_END(old_back);
      val = rmp0 + (rmp1 - rmp0) * xpos;
      ED_RAMP2_BEG(new_back) = val;
      ED_RAMP2_END(new_back) = rmp1;
      
      rmp0 = ED_RAMP3_BEG(old_back);
      rmp1 = ED_RAMP3_END(old_back);
      val = rmp0 + (rmp1 - rmp0) * xpos;
      ED_RAMP3_BEG(new_back) = val;
      ED_RAMP3_END(new_back) = rmp1;

      rmp0 = ED_RAMP4_BEG(old_back);
      rmp1 = ED_RAMP4_END(old_back);
      val = rmp0 + (rmp1 - rmp0) * xpos;
      ED_RAMP4_BEG(new_back) = val;
      ED_RAMP4_END(new_back) = rmp1;
    }
  if (PTREE123_OP(ED_TYPE(new_back)))
    {
      ED_PTREE_POSITION(new_back) = ED_PTREE_POSITION(old_back) + samp - ED_GLOBAL_POSITION(old_back);
      if (PTREE23_OP(ED_TYPE(new_back)))
	{
	  ED_PTREE2_POSITION(new_back) = ED_PTREE2_POSITION(old_back) + samp - ED_GLOBAL_POSITION(old_back);
	  if (PTREE3_OP(ED_TYPE(new_back)))
	    ED_PTREE3_POSITION(new_back) = ED_PTREE3_POSITION(old_back) + samp - ED_GLOBAL_POSITION(old_back);
	}
    }
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
  ed_list *new_state;
  new_state = insert_section_into_list(samp, num, cp->edits[pos], rtn, origin, scaler);
  new_state->edpos = pos;
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      ed_list *old_state;
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

bool extend_with_zeros(chan_info *cp, off_t beg, off_t num, int edpos)
{
  off_t len, new_len;
  ed_fragment *cb;
  ed_list *ed, *old_ed;
  if (num <= 0) return(true); /* false if can't edit, but this is a no-op */
  old_ed = cp->edits[edpos];
  len = cp->samples[edpos];
  if (beg > len) 
    {
      new_len = beg + num; 
      beg = len;
      num = new_len - beg;
    }
  else new_len = len + num;
  if (!(prepare_edit_list(cp, new_len, edpos, S_pad_channel))) return(false); /* actual edit-list origin is done at zero_edit level */
  ed = insert_samples_into_list(beg, num, edpos, cp, &cb, S_pad_channel, 0.0);
  cp->edits[cp->edit_ctr] = ed;
  ED_SOUND(cb) = EDIT_LIST_ZERO_MARK;
  ED_SCALER(cb) = 0.0;
  ED_TYPE(cb) = ED_ZERO;
  ed->edit_type = ZERO_EDIT;
  ed->sound_location = 0;
  ed->maxamp = old_ed->maxamp;
  check_for_first_edit(cp); /* needed to activate revert menu option */
  amp_env_insert_zeros(cp, beg, num, edpos);
  lock_affected_mixes(cp, beg, beg);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  after_edit(cp);
  return(true);
}

void extend_edit_list(chan_info *cp, int edpos)
{
  /* called by drag-mix (remix_file) in snd-mix.c to provide an "undo point" for mix reposition */
  /*   when both sides of the mix are currently zeroed out -- that is, no edit takes place, */
  /*   but it's confusing to the user not to have the position change show up in the history list */
  int i;
  ed_list *new_ed, *old_ed;
  if (!(prepare_edit_list(cp, cp->samples[edpos], edpos, S_pad_channel))) return;
  old_ed = cp->edits[edpos];
  new_ed = make_ed_list(cp->edits[edpos]->size);
  new_ed->beg = 0;
  new_ed->len = cp->samples[edpos];
  cp->edits[cp->edit_ctr] = new_ed;
  for (i = 0; i < new_ed->size; i++) 
    {
      copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
    }
  new_ed->edit_type = EXTEND_EDIT;
  new_ed->sound_location = 0;
  new_ed->origin = copy_string(S_pad_channel);
  new_ed->edpos = edpos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  cp->amp_envs[cp->edit_ctr] = amp_env_copy(cp, false, edpos);
  after_edit(cp);
}

bool file_insert_samples(off_t beg, off_t num, const char *inserted_file, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  file_info *hdr;
  len = cp->samples[edpos];
  if (beg >= len)
    {
      if (!(extend_with_zeros(cp, len, beg - len, edpos))) return(false);
      edpos = cp->edit_ctr;
      len = CURRENT_SAMPLES(cp);
    }
  if (!(prepare_edit_list(cp, len + num, edpos, origin))) return(false);
  cp->edits[cp->edit_ctr] = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  hdr = make_file_info(inserted_file, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr)
    {
      int fd;
      fd = snd_open_read(inserted_file);
      snd_file_open_descriptors(fd,
				inserted_file,
				hdr->format,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, inserted_file, SND_INSERT_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, inserted_file, 
						 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      {
	ed_list *ed;
	ed = cp->edits[cp->edit_ctr];
	ed->edit_type = INSERTION_EDIT;
	ed->sound_location = ED_SOUND(cb);
      }
      /* mixes were rippled by insert_samples_into_list above (ripple_all -> ripple_mixes)
       *   so the lock should affect those that were split by the insertion, which in current
       *   terms means we're interested only in 'beg'
       */
      lock_affected_mixes(cp, beg, beg);
      reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
      after_edit(cp);
      return(true);
    }
  return(false);
}

bool insert_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  if (num <= 0) return(true);
  len = cp->samples[edpos];
  if (beg >= len)
    {
      if (!(extend_with_zeros(cp, len, beg - len, edpos))) return(false);
      edpos = cp->edit_ctr;
      len = CURRENT_SAMPLES(cp);
    }
  if (!(prepare_edit_list(cp, len + num, edpos, origin))) return(false);
  cp->edits[cp->edit_ctr] = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(vals, (int)num, cp->edit_ctr);
  ED_SOUND(cb) = cp->sound_ctr;
  {
    ed_list *ed;
    ed = cp->edits[cp->edit_ctr];
    ed->edit_type = INSERTION_EDIT;
    ed->sound_location = ED_SOUND(cb);
  }
  lock_affected_mixes(cp, beg, beg);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  after_edit(cp);
  return(true);
}

bool insert_complete_file(snd_info *sp, const char *str, off_t chan_beg, file_delete_t auto_delete)
{
  int nc;
  bool ok = false;
  char *filename;
  filename = mus_expand_filename(str);
  nc = mus_sound_chans(filename);
  if (nc > 0)
    {
      off_t len;
      len = mus_sound_frames(filename);
      if (len == 0)
	snd_warning(_("%s has no data"), str);
      else
	{
	  int i, j, first_chan = 0;
	  char *origin;
	  chan_info *ncp;
	  if (sp->sync != 0)
	    ncp = sp->chans[0];
	  else ncp = any_selected_channel(sp);
	  first_chan = ncp->chan;
	  for (i = first_chan, j = 0; (j < nc) && (i < sp->nchans); i++, j++)
	    {
	      ncp = sp->chans[i];
#if HAVE_FORTH
	      origin = mus_format("\"%s\" " OFF_TD " %d %s drop", 
				  filename, chan_beg, j, S_insert_sound);
#else
	      origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "%d", 
				  TO_PROC_NAME(S_insert_sound), filename, chan_beg, j);
#endif
	      ok = file_insert_samples(chan_beg, len, filename, ncp, j, auto_delete, origin, ncp->edit_ctr);
	      if (ok)
		update_graph(ncp);
	      FREE(origin);
	    }
	}
    }
  else snd_warning(_("can't read %s"), str);
  FREE(filename);
  return(ok);
}

bool insert_complete_file_at_cursor(snd_info *sp, const char *filename)
{
  chan_info *ncp;
  ncp = any_selected_channel(sp);
  return(insert_complete_file(sp, filename, CURSOR(ncp), DONT_DELETE_ME));
}



/* -------------------------------- delete samples -------------------------------- */

static ed_list *delete_section_from_list(off_t beg, off_t num, ed_list *current_state)
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
#if HAVE_FORTH
  new_state->origin = mus_format(OFF_TD " " OFF_TD " %s drop", beg, num, S_delete_samples);
#else
  new_state->origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_delete_samples), beg, num);
#endif
  new_state->edit_type = DELETION_EDIT;
  new_state->sound_location = 0;
  return(new_state);
}

static ed_list *delete_samples_from_list(off_t beg, off_t num, int pos, chan_info *cp)
{
  ed_list *new_state;
  new_state = delete_section_from_list(beg, num, cp->edits[pos]);
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      ed_list *old_state;
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

bool delete_samples(off_t beg, off_t num, chan_info *cp, int edpos)
{
  off_t len;
  if (num <= 0) return(true);
  len = cp->samples[edpos];
  if ((beg < len) && (beg >= 0))
    {
      if ((beg + num) > len) num = len - beg;
      if (!(prepare_edit_list(cp, len - num, edpos, S_delete_samples))) return(false);
      lock_affected_mixes(cp, beg, beg + num); /* edit_ctr should be ready, need pre-ripple values for block overlap check */
      cp->edits[cp->edit_ctr] = delete_samples_from_list(beg, num, edpos, cp);
      reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
      after_edit(cp);
      return(true);
    }
  return(false);
}



/* -------------------------------- change samples -------------------------------- */

static ed_list *change_samples_in_list(off_t beg, off_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin)
{
  /* delete + insert -- already checked that beg < cur end */
  ed_list *new_state;
  ed_list *del_state;
  off_t del_num, cur_end;
  ed_fragment *changed_f;
  if (num <= 0) return(NULL);
  cur_end = cp->samples[pos];
  del_num = cur_end - beg;
  if (num < del_num) del_num = num;
  del_state = delete_section_from_list(beg, del_num, cp->edits[pos]);
  new_state = insert_section_into_list(beg, num, del_state, &changed_f, origin, 1.0);
  del_state = free_ed_list(del_state, cp);
  (*rtn) = changed_f;
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      ed_list *old_state;
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
    }
  new_state->edpos = pos;
  ripple_all(cp, 0, 0);
  return(new_state);
}

bool file_mix_change_samples(off_t beg, off_t num, const char *tempfile, chan_info *cp, int chan, 
			     file_delete_t auto_delete, lock_mix_t lock, const char *origin, int edpos, bool with_mix)
{
  file_info *hdr;
  hdr = make_file_info(tempfile, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr)
    {
      off_t prev_len, new_len;
      ed_fragment *cb;
      int fd;
      prev_len = cp->samples[edpos];
      if (beg >= prev_len)
	{
	  if (!(extend_with_zeros(cp, prev_len, beg - prev_len + 1, edpos))) 
	    {
	      free_file_info(hdr);
	      return(false);
	    }
	  edpos = cp->edit_ctr;
	  prev_len = CURRENT_SAMPLES(cp);
	}
      new_len = beg + num;
      if (new_len < prev_len) new_len = prev_len;
      if (!(prepare_edit_list(cp, new_len, edpos, origin)))
	{
	  free_file_info(hdr);
	  return(false);
	}
      cp->edits[cp->edit_ctr] = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
      if (new_len > prev_len) reflect_sample_change_in_axis(cp);
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
      fd = snd_open_read(tempfile);
      snd_file_open_descriptors(fd,
				tempfile,
				hdr->format,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_CHANGE_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, tempfile, 
						 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      {
	ed_list *ed;
	ed = cp->edits[cp->edit_ctr];
	ed->edit_type = CHANGE_EDIT;
	ed->sound_location = ED_SOUND(cb);
      }

      if (!with_mix)
	{
	  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
	  after_edit(cp);
	}
    }
  else
    {
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_2(C_TO_XEN_STRING(origin),
			   C_TO_XEN_STRING(snd_io_strerror())));
    }
  return(true);
}

bool file_change_samples(off_t beg, off_t num, const char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, lock_mix_t lock, const char *origin, int edpos)
{
  return(file_mix_change_samples(beg, num, tempfile, cp, chan, auto_delete, lock, origin, edpos, false));
}


bool file_override_samples(off_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, lock_mix_t lock, const char *origin)
{
  file_info *hdr;
  hdr = make_file_info(tempfile, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr) 
    {
      int fd;
      ed_list *e;
      if (num == -1) num = (hdr->samples / hdr->chans);
      if (!(prepare_edit_list(cp, num, AT_CURRENT_EDIT_POSITION, origin)))
	{
	  free_file_info(hdr);
	  return(false);
	}
      fd = snd_open_read(tempfile);
      snd_file_open_descriptors(fd,
				tempfile,
				hdr->format,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_OVERRIDE_FILE);
      e = initial_ed_list(0, num - 1);
      if (origin) e->origin = copy_string(origin);
      cp->edits[cp->edit_ctr] = e;
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, 0, num);
      FRAGMENT_SOUND(e, 0) = add_sound_file_to_edit_list(cp, tempfile, 
							 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
							 hdr, auto_delete, chan);
      e->edit_type = CHANGE_EDIT;
      e->sound_location = FRAGMENT_SOUND(e, 0);
      e->edpos = cp->edit_ctr - 1;
      reflect_sample_change_in_axis(cp);
      ripple_all(cp, 0, 0);
      reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
      after_edit(cp);
    }
  else
    {
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_2(C_TO_XEN_STRING(origin),
			   C_TO_XEN_STRING(snd_io_strerror())));
    }
  return(true);
}

bool change_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, lock_mix_t lock, const char *origin, int edpos)
{
  off_t prev_len, new_len;
  ed_fragment *cb;
  ed_list *ed;
  if (num <= 0) return(true);
  prev_len = cp->samples[edpos];
  if (beg >= prev_len)
    {
      if (!(extend_with_zeros(cp, prev_len, beg - prev_len + 1, edpos))) return(false);
      edpos = cp->edit_ctr;
      prev_len = CURRENT_SAMPLES(cp);
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  if (!(prepare_edit_list(cp, new_len, edpos, origin))) return(false);
  ed = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
  if (new_len > prev_len) reflect_sample_change_in_axis(cp);
  cp->edits[cp->edit_ctr] = ed;
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(vals, (int)num, cp->edit_ctr);
  ED_SOUND(cb) = cp->sound_ctr;
  ed->edit_type = CHANGE_EDIT;
  ed->sound_location = ED_SOUND(cb);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  after_edit(cp);
  return(true);
}


/* -------------------------------- ramp/scale/ptree -------------------------------- */

bool ramp_or_ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, bool is_xramp)
{
  /* from enveloper (snd-sig.c) */
  ed_list *ed;
  int i;
  off_t end;
  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      off_t loc, next_loc;
      int typ;
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);             /* i.e. next loc = current fragment end point */
      /* fragment starts at loc, ends just before next_loc, is of type typ */
      if ((next_loc > beg) &&
	  (((!is_xramp) && (type_info[typ].add_ramp == -1)) ||
	   ((is_xramp) && (type_info[typ].add_xramp == -1))))
	return(true);
    }
  return(false);
}

bool ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos)
{
  /* from ptree-channel (snd-sig.c) check for pre-existing ptree-channel */
  ed_list *ed;
  int i;
  off_t end;
  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      off_t loc, next_loc;
      int typ;
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);         /* i.e. next loc = current fragment end point */
      /* fragment starts at loc, ends just before next_loc, is of type typ */
      if (next_loc > beg)
	{
	  if (type_info[typ].add_ptree == -1)
	    return(true);
	}
    }
  return(false);
}

bool ptree_or_sound_fragments_in_use(chan_info *cp, int pos)
{
  /* (swap-channels): are there any non-simple/non-ramp edits? */
  int i;
  ed_list *ed;
  ed = cp->edits[pos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      int index;
      index = FRAGMENT_SOUND(ed, i);
      if (index == EDIT_LIST_END_MARK) return(false);
      if ((index != 0) &&
	  (index != EDIT_LIST_ZERO_MARK))
	return(true);
      if (PTREE123_OP(FRAGMENT_TYPE(ed, i)))
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
			  Float old_beg, old_beg2, old_beg3, old_beg4;
			  old_beg = ED_RAMP_BEG(split_front_f);
			  old_beg2 = ED_RAMP2_BEG(split_front_f);
			  old_beg3 = ED_RAMP3_BEG(split_front_f);
			  old_beg4 = ED_RAMP4_BEG(split_front_f);
			  new_leading_ramp(split_front_f, cur_f, end);
			  if (mid_f == split_front_f)
			    {
			      ED_RAMP_BEG(split_front_f) = old_beg;
			      ED_RAMP2_BEG(split_front_f) = old_beg2;
			      ED_RAMP3_BEG(split_front_f) = old_beg3;
			      ED_RAMP4_BEG(split_front_f) = old_beg4;
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

bool scale_channel_with_origin(chan_info *cp, Float scl, off_t beg, off_t num, int pos, bool in_as_one_edit, const char *origin)
{
  /* copy current ed-list and reset scalers */
  off_t len = 0;
  int i;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]) ||
      (scl == 1.0))
    return(true); 
  len = cp->samples[pos];
  if (!(prepare_edit_list(cp, len, pos, S_scale_channel))) return(false);
  old_ed = cp->edits[pos];
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      if (scl == 0.0)
	{
	  new_ed = initial_ed_list(0, num - 1);
	  FRAGMENT_SCALER(new_ed, 0) = 0.0;
	  FRAGMENT_TYPE(new_ed, 0) = ED_ZERO;
	  new_ed->maxamp = 0.0;
	  new_ed->maxamp_position = 0;
	}
      else
	{
	  num = len;
	  new_ed = make_ed_list(cp->edits[pos]->size);
	  new_ed->beg = beg;
	  new_ed->len = num;
	  for (i = 0; i < new_ed->size; i++) 
	    {
	      copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	      FRAGMENT_SCALER(new_ed, i) *= scl;
	    }
	}
      cp->edits[cp->edit_ctr] = new_ed;
      amp_env_scale_by(cp, scl, pos); /* this seems wasteful if this is an intermediate (in_as_one_edit etc) */
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
  if (origin)
    new_ed->origin = copy_string(origin);
  else
    {
      if (num == len)
#if HAVE_FORTH
	new_ed->origin = mus_format("%.3f " OFF_TD PROC_SEP PROC_FALSE " %s", scl, beg, S_scale_channel);
#else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_scale_channel), scl, beg);
#endif
      else
	{
#if HAVE_FORTH
	  if (len == num)
	    new_ed->origin = mus_format("%.3f " OFF_TD PROC_SEP PROC_FALSE " %s", scl, beg, S_scale_channel);
	  else
	    new_ed->origin = mus_format("%.3f " OFF_TD PROC_SEP OFF_TD " %s", scl, beg, num, S_scale_channel);
#else
	  if (len == num)
	      new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_scale_channel), scl, beg);
	  else
	    new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_scale_channel), scl, beg, num);
#endif
	}
    }
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  if (!in_as_one_edit) 
    {
      update_graph(cp);
      reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
    }
  after_edit(cp);
  return(true);
}

bool scale_channel(chan_info *cp, Float scl, off_t beg, off_t num, int pos, bool in_as_one_edit)
{
  return(scale_channel_with_origin(cp, scl, beg, num, pos, in_as_one_edit, NULL));
}

static void setup_ramp_fragments(ed_list *new_ed, int i, double seg0, double seg1, Float scaler, Float offset, bool is_xramp)
{
  int typ;
  typ = FRAGMENT_TYPE(new_ed, i);
  if (typ != ED_ZERO)
    {
      if ((seg0 == 0.0) && (seg1 == 0.0))
	{
	  FRAGMENT_TYPE(new_ed, i) = ED_ZERO;
	  return;
	}
      if (is_xramp)
	{
	  if (type_info[typ].xramps == 1)
	    {
	      /* has xramp -- use 3:2 */
	      FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
	      FRAGMENT_RAMP3_END(new_ed, i) = seg1;
	      FRAGMENT_XRAMP_SCALER2(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET2(new_ed, i) = offset;
	      /*
	      if ((FRAGMENT_XRAMP_OFFSET(new_ed, i) == 0.0) &&
		  (FRAGMENT_XRAMP_OFFSET2(new_ed, i) == 0.0))
		  ...collapse to one xramp... -- never happens, so I didn't write the obvious code
	      */
	    }
	  else
	    {
	      /* no previous xramp -- use 4:1 */
	      FRAGMENT_RAMP4_BEG(new_ed, i) = seg0;
	      FRAGMENT_RAMP4_END(new_ed, i) = seg1;
	      FRAGMENT_XRAMP_SCALER(new_ed, i) = scaler;
	      FRAGMENT_XRAMP_OFFSET(new_ed, i) = offset;
	    }
	  FRAGMENT_TYPE(new_ed, i) = type_info[typ].add_xramp;
	}
      else
	{
	  if (type_info[typ].ramps == 3)
	    {
	      FRAGMENT_RAMP4_BEG(new_ed, i) = seg0;
	      FRAGMENT_RAMP4_END(new_ed, i) = seg1;
	    }
	  else
	    {
	      if (type_info[typ].ramps == 2)
		{
		  FRAGMENT_RAMP3_BEG(new_ed, i) = seg0;
		  FRAGMENT_RAMP3_END(new_ed, i) = seg1;
		}
	      else
		{
		  if (type_info[typ].ramps == 1)
		    {
		      FRAGMENT_RAMP2_BEG(new_ed, i) = seg0;
		      FRAGMENT_RAMP2_END(new_ed, i) = seg1;
		    }
		  else
		    {
		      FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
		      FRAGMENT_RAMP_END(new_ed, i) = seg1;
		    }
		}
	    }
	  FRAGMENT_TYPE(new_ed, i) = type_info[typ].add_ramp;
	}
    }
  
#if MUS_DEBUGGING
  if (FRAGMENT_TYPE(new_ed, i) == -1)
    {
      fprintf(stderr, "%s -> ??\n", type_info[typ].name);
      abort();
    }
#endif
}

static bool all_ramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
			     off_t beg, off_t num, int pos, bool in_as_one_edit, const char *origin, 
			     bool is_x, mus_any *e, int e_pos)
{
  off_t len = 0;
  int i;
  ed_list *new_ed, *old_ed;
  Float seg0, seg1;
  double incr;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]))
    return(true); 
  if ((rmp0 == rmp1) || (num == 1))
    return(scale_channel(cp, rmp0, beg, num, pos, in_as_one_edit));
  len = cp->samples[pos];
  if (!(prepare_edit_list(cp, len, pos, origin))) return(false);
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
#if HAVE_FORTH
      if (num == len)
	new_ed->origin = mus_format("%.3f %.3f " OFF_TD PROC_SEP PROC_FALSE " %s", rmp0, rmp1, beg, origin);
      else
	new_ed->origin = mus_format("%.3f %.3f " OFF_TD PROC_SEP OFF_TD " %s", rmp0, rmp1, beg, num, origin);
#else
      if (num == len)
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(origin), rmp0, rmp1, beg);
      else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(origin), rmp0, rmp1, beg, num);
#endif
    }
  else
    {
      Float *data;
      data = mus_data(e);
#if HAVE_FORTH
      if (num == len)
	new_ed->origin = mus_format("%.3f %.3f %.3f " OFF_TD PROC_SEP PROC_FALSE " %s",
				    data[e_pos * 2 + 1], data[e_pos * 2 + 3], mus_increment(e), beg, origin);
      else
	new_ed->origin = mus_format("%.3f %.3f %.3f " OFF_TD PROC_SEP OFF_TD " %s",
				    data[e_pos * 2 + 1], data[e_pos * 2 + 3], mus_increment(e), beg, num, origin);
#else
      if (num == len)
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%.3f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, 
				    TO_PROC_NAME(origin), data[e_pos * 2 + 1], data[e_pos * 2 + 3], mus_increment(e), beg);
      else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD, 
				    TO_PROC_NAME(origin), data[e_pos * 2 + 1], data[e_pos * 2 + 3], mus_increment(e), beg, num);
#endif
    }
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  after_edit(cp);
  return(true);
}

bool ramp_channel(chan_info *cp, Float rmp0, Float rmp1, off_t beg, off_t num, int pos, bool in_as_one_edit)
{
  return(all_ramp_channel(cp, rmp0, rmp1, 0.0, 0.0, beg, num, pos, in_as_one_edit, S_ramp_channel, false, NULL, 0));
}

bool xramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
		   off_t beg, off_t num, int pos, bool in_as_one_edit, mus_any *e, int e_pos)
{
  return(all_ramp_channel(cp, rmp0, rmp1, scaler, offset, beg, num, pos, in_as_one_edit, S_xramp_channel, true, e, e_pos));
}

static void make_ptree_fragment(ed_list *new_ed, int i, int ptree_loc, off_t beg, off_t num)
{
  int typ;
  typ = FRAGMENT_TYPE(new_ed, i);
  if (typ >= ED_SIMPLE)
    {
      FRAGMENT_TYPE(new_ed, i) = type_info[typ].add_ptree;
    }
  else
    {
      /* this can happen at the very end (actually seems like a bug...) */
      FRAGMENT_TYPE(new_ed, i) = ED_PTREE;
    }
  if (PTREE1_OP(FRAGMENT_TYPE(new_ed, i)))
    {
      FRAGMENT_PTREE_INDEX(new_ed, i) = ptree_loc;
      FRAGMENT_PTREE_SCALER(new_ed, i) = MUS_SAMPLE_TO_FLOAT(FRAGMENT_SCALER(new_ed, i)); /* arg is mus_sample data, need convert to float */
      FRAGMENT_PTREE_DUR(new_ed, i) = num;
      FRAGMENT_PTREE_POSITION(new_ed, i) = FRAGMENT_GLOBAL_POSITION(new_ed, i) - beg;
    }
  else 
    {
      if (PTREE2_OP(FRAGMENT_TYPE(new_ed, i)))
	{
	  FRAGMENT_PTREE2_INDEX(new_ed, i) = ptree_loc;
	  FRAGMENT_PTREE2_SCALER(new_ed, i) = FRAGMENT_SCALER(new_ed, i); /* already float, so no need to convert */
	  FRAGMENT_PTREE2_DUR(new_ed, i) = num;
	  FRAGMENT_PTREE2_POSITION(new_ed, i) = FRAGMENT_GLOBAL_POSITION(new_ed, i) - beg;
	}
      else
	{
	  FRAGMENT_PTREE3_INDEX(new_ed, i) = ptree_loc;
	  FRAGMENT_PTREE3_SCALER(new_ed, i) = FRAGMENT_SCALER(new_ed, i); /* already float, so no need to convert */
	  FRAGMENT_PTREE3_DUR(new_ed, i) = num;
	  FRAGMENT_PTREE3_POSITION(new_ed, i) = FRAGMENT_GLOBAL_POSITION(new_ed, i) - beg;
	}
    }
  FRAGMENT_SCALER(new_ed, i) = 1.0;
}

void ptree_channel(chan_info *cp, struct ptree *tree, off_t beg, off_t num, int pos, bool env_it, XEN init_func, const char *origin)
{
  off_t len;
  int i, ptree_loc = 0;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= cp->samples[pos]) ||
      (tree == NULL))
    {
      if (tree) 
	{
	  free_ptree(tree);
	  tree = NULL;
	}
      return; 
    }
  len = cp->samples[pos];
  if (pos > cp->edit_ctr)
    {
      /* prepare_edit_list will throw 'no-such-edit, but we need to clean up the ptree first */
      free_ptree(tree);
      tree = NULL;
    }
  if (!(prepare_edit_list(cp, len, pos, S_ptree_channel)))
    {
      /* perhaps edit-hook blocked the edit */
      if (tree) 
	{
	  free_ptree(tree);
	  tree = NULL;
	}
      return;
    }
  old_ed = cp->edits[pos];
  ptree_loc = add_ptree(cp);
  cp->ptrees[ptree_loc] = tree;
  if (XEN_PROCEDURE_P(init_func))
    {
      cp->init_locs[ptree_loc] = snd_protect(init_func);
      cp->ptree_inits[ptree_loc] = init_func;
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
	  make_ptree_fragment(new_ed, i, ptree_loc, beg, num);
	}
      if (env_it)
	amp_env_ptree(cp, tree, pos, init_func);
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
	    make_ptree_fragment(new_ed, i, ptree_loc, beg, num);
	}
      if (env_it)
	amp_env_ptree_selection(cp, tree, beg, num, pos, init_func);
    }
  new_ed->edit_type = PTREE_EDIT;
  new_ed->sound_location = 0;
  new_ed->ptree_location = ptree_loc;
  new_ed->origin = copy_string(origin);
  new_ed->edpos = pos;
  new_ed->ptree_env_too = env_it;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  lock_affected_mixes(cp, beg, beg + num);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  after_edit(cp);
  update_graph(cp);
}


/* -------------------------------- sample readers -------------------------------- */

snd_fd *free_snd_fd_almost(snd_fd *sf)
{
  if (sf) 
    {
      snd_data *sd;
      /* if ((XEN_BOUND_P(sf->closure1)) && (!(XEN_EQ_P(sf->closure1, empty_closure)))) */
      if (sf->protect1 != NOT_A_GC_LOC)
 	{
 	  snd_unprotect_at(sf->protect1);
 	  sf->closure1 = XEN_UNDEFINED;
	  sf->protect1 = NOT_A_GC_LOC;
 	}
      /* if ((XEN_BOUND_P(sf->closure2)) && (!(XEN_EQ_P(sf->closure2, empty_closure)))) */
      if (sf->protect2 != NOT_A_GC_LOC)
 	{
 	  snd_unprotect_at(sf->protect2);
 	  sf->closure2 = XEN_UNDEFINED;
	  sf->protect2 = NOT_A_GC_LOC;
 	}
      /* if ((XEN_BOUND_P(sf->closure3)) && (!(XEN_EQ_P(sf->closure3, empty_closure)))) */
      if (sf->protect3 != NOT_A_GC_LOC)
 	{
 	  snd_unprotect_at(sf->protect3);
 	  sf->closure3 = XEN_UNDEFINED;
	  sf->protect3 = NOT_A_GC_LOC;
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
    }
  return(NULL);
}

snd_fd *free_snd_fd(snd_fd *sf)
{
  if (sf)
    {
      free_snd_fd_almost(sf);
      FREE(sf);
    }
  return(NULL);
}

off_t current_location(snd_fd *sf) 
{
  /* only used by moving cursor code in snd-dac.c [and sample-reader-position] */
  if (sf->current_sound)
    return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + sf->current_sound->io->beg + sf->loc);
  return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + sf->loc);
}

enum {SAMPLE_READER, REGION_READER};

static snd_fd *init_sample_read_any_with_bufsize(off_t samp, chan_info *cp, read_direction_t direction, int edit_position, int bufsize)
{
  snd_fd *sf;
  snd_info *sp;
  ed_list *ed;
  int len, i;
  off_t curlen;
  snd_data *first_snd = NULL;
  if (!(cp->active)) return(NULL);
  if ((edit_position < 0) || (edit_position >= cp->edit_size)) return(NULL); /* was ">" not ">=": 6-Jan-05 */
  ed = (ed_list *)(cp->edits[edit_position]);
  if (!ed) return(NULL);
  sp = cp->sound;
  if (sp->inuse == SOUND_IDLE) return(NULL);

  if ((sp->need_update) &&
      (!(sp->writing)))
    {
      if (mus_file_probe(sp->filename) == 0)
	{
	  snd_warning(_("%s no longer exists!"), sp->short_filename);
	  return(NULL);
	}
      else snd_warning(_("%s has changed since we last read it!"), sp->short_filename);
    }

  curlen = cp->samples[edit_position];
  /* snd_fd allocated only here */
  sf = (snd_fd *)CALLOC(1, sizeof(snd_fd)); /* only creation point */
#if MUS_DEBUGGING
  set_printable(PRINT_SND_FD);
#endif
  sf->closure1 = XEN_UNDEFINED;
  sf->closure2 = XEN_UNDEFINED;
  sf->closure3 = XEN_UNDEFINED;
  sf->protect1 = NOT_A_GC_LOC;
  sf->protect2 = NOT_A_GC_LOC;
  sf->protect3 = NOT_A_GC_LOC;
  sf->region = INVALID_REGION;
  sf->type = SAMPLE_READER;
  sf->initial_samp = samp;
  sf->cp = cp;
  sf->fscaler = MUS_FIX_TO_FLOAT;
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
      ed_fragment *cb;
      cb = FRAGMENT(ed, i);
      if ((ED_GLOBAL_POSITION(cb) > samp) || 
	  (ED_SOUND(cb) == EDIT_LIST_END_MARK))             /* i.e. we went one too far */
	{
	  off_t ind0, ind1, indx;
	  sf->cb = FRAGMENT(ed, i - 1);  /* so back up one */
	  sf->cbi = i - 1;
	  sf->frag_pos = samp - READER_GLOBAL_POSITION(sf);
	  ind0 = READER_LOCAL_POSITION(sf);                   /* cb->beg */
	  indx = READER_LOCAL_POSITION(sf) + sf->frag_pos;
	  ind1 = READER_LOCAL_END(sf);                        /* cb->end */
	  sf->fscaler = MUS_FIX_TO_FLOAT * READER_SCALER(sf);
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
	  if (!first_snd)
	    return(cancel_reader(sf));
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
		  first_snd = copy_snd_data(first_snd, samp, bufsize);
		  if (!first_snd)
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

snd_fd *init_sample_read_any(off_t samp, chan_info *cp, read_direction_t direction, int edit_position)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, edit_position, MIX_FILE_BUFFER_SIZE));
}

snd_fd *init_sample_read(off_t samp, chan_info *cp, read_direction_t direction)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, cp->edit_ctr, MIX_FILE_BUFFER_SIZE));
}

Float chn_sample(off_t samp, chan_info *cp, int pos)
{ 
  snd_fd *sf;
  Float val = 0.0;

  /* pos is assumed to be right here, not AT_CURRENT_EDIT_POSITION for example */
  if ((!(cp->active)) || 
      (samp < 0) || 
      (pos < 0) || 
      (pos >= cp->edit_size) || 
      (samp >= cp->samples[pos])) 
    return(0.0);

  /* try the quick case */
  if (pos == 0)
    {
      snd_data *sd;
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

static void previous_sound_1(snd_fd *sf) 
{
  off_t ind0, ind1, indx;
  bool at_start;
  if ((sf->cp) && (!(sf->cp->active)))
    {
      reader_out_of_data(sf);
      return;
    }
  at_start = ((sf->cb == NULL) || 
	      (sf->current_sound == NULL) || 
	      (READER_LOCAL_POSITION(sf) >= sf->current_sound->io->beg));
  if (at_start)
    {
      snd_data *prev_snd;
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
		{
		  prev_snd = copy_snd_data(prev_snd, ind0, MIX_FILE_BUFFER_SIZE);
		  if (prev_snd == NULL)
		    {
		      /* too many files open or something of that sort */
		      reader_out_of_data(sf);
		      return;
		    }
		}
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
  bool at_end = false;
  if ((sf->cp) && (!(sf->cp->active)))
    {
      reader_out_of_data(sf);
      return;
    }
  at_end = ((sf->cb == NULL) || 
	    (sf->current_sound == NULL) || 
	    (READER_LOCAL_END(sf) <= sf->current_sound->io->end));
  if (at_end)
    {
      snd_data *nxt_snd;
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
		{
		  nxt_snd = copy_snd_data(nxt_snd, ind0, MIX_FILE_BUFFER_SIZE);
		  if (nxt_snd == NULL)
		    {
		      reader_out_of_data(sf);
		      return;
		    }
		}
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
  env_info *e0 = NULL, *e1 = NULL;

  if ((!(prepare_edit_list(cp0, cp1->samples[pos1], AT_CURRENT_EDIT_POSITION, S_swap_channels))) ||
      (!(prepare_edit_list(cp1, cp0->samples[pos0], AT_CURRENT_EDIT_POSITION, S_swap_channels))))
    return;

  name = cp0->sound->filename;
  hdr0 = copy_header(name, cp0->sound->hdr);
  fd = snd_open_read(name);
  snd_file_open_descriptors(fd,
			    name,
			    hdr0->format,
			    hdr0->data_location,
			    hdr0->chans,
			    hdr0->type);
  new0 = add_sound_file_to_edit_list(cp1, name,
				     make_file_state(fd, hdr0, cp0->chan, 0, FILE_BUFFER_SIZE),
				     hdr0, DONT_DELETE_ME, cp0->chan);
  name = cp1->sound->filename;
  hdr1 = copy_header(name, cp1->sound->hdr);
  fd = snd_open_read(name);
  snd_file_open_descriptors(fd,
			    name,
			    hdr1->format,
			    hdr1->data_location,
			    hdr1->chans,
			    hdr1->type);
  new1 = add_sound_file_to_edit_list(cp0, name,
				     make_file_state(fd, hdr1, cp1->chan, 0, FILE_BUFFER_SIZE),
				     hdr1, DONT_DELETE_ME, cp1->chan);
  e0 = amp_env_copy(cp0, false, pos0);
  if (e0) e1 = amp_env_copy(cp1, false, pos1);
  old_ed = cp1->edits[pos1];
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new1;
  new_ed->edpos = pos1;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;
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
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new0;
  new_ed->edpos = pos0;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;
  new_ed->beg = 0;
  new_ed->len = old_ed->len;
  new_ed->origin = copy_string(TO_PROC_NAME(S_swap_channels)); /* swap = stored change-edit at restore time, so no redundancy here */
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
  else
    {
      if (e0) e0 = free_env_info(e0);
      if (e1) e1 = free_env_info(e1);
    }
  ripple_all(cp0, 0, 0);
  ripple_all(cp1, 0, 0);
  swap_marks(cp0, cp1);
  if (cp0->samples[cp0->edit_ctr] != cp0->samples[cp0->edit_ctr - 1])
    reflect_sample_change_in_axis(cp0);
  if (cp1->samples[cp1->edit_ctr] != cp1->samples[cp1->edit_ctr - 1])
    reflect_sample_change_in_axis(cp1);
  after_edit(cp0);
  after_edit(cp1);
  update_graph(cp0);
  update_graph(cp1);
}

io_error_t save_edits_and_update_display(snd_info *sp)
{
  /* open temp, write current state, rename to old, reopen and clear all state */
  /* can't overwrite current because we may have cut/paste backpointers scattered around the current edit list */
  /* have to decide here what header/data type to write as well -- original? */
  /* if latter, must be able to write all headers! -- perhaps warn user and use snd/aiff/riff/ircam */

  /* read_only already checked */
  char *ofile = NULL;
  int i;
  off_t samples = 0;
  off_t *old_cursors = NULL;
  chan_info *cp;
  snd_fd **sf;
  void *sa;
  file_info *sphdr = NULL;
  io_error_t io_err = IO_NO_ERROR;

  if (dont_save(sp, NULL)) return(IO_SAVE_HOOK_CANCELLATION);
  ofile = snd_tempnam(); 
  /* this will use user's TMPDIR if temp_dir(ss) is not set, else stdio.h's P_tmpdir else /tmp */

  sf = (snd_fd **)CALLOC(sp->nchans, sizeof(snd_fd *));
  for (i = 0; i < sp->nchans; i++)
    {
      sf[i] = init_sample_read(0, sp->chans[i], READ_FORWARD);
      if (sf[i] == NULL)
	{
	  int j;
	  for (j = 0; j < i; j++) free_snd_fd(sf[j]);
	  FREE(sf);
	  return(IO_BAD_CHANNEL);
	}
      if (samples < CURRENT_SAMPLES(sp->chans[i]))
	samples = CURRENT_SAMPLES(sp->chans[i]);
    }
  sphdr = sp->hdr;

  /* write the new file */
  io_err = snd_make_file(ofile, sp->nchans, sp->hdr, sf, samples);
  if (io_err != IO_NO_ERROR) 
    {
      for (i = 0; i < sp->nchans; i++) free_snd_fd(sf[i]);
      FREE(sf);
      return(io_err);
    }

  sa = make_axes_data(sp);
  sphdr->samples = samples * sp->nchans;
  collapse_marks(sp);
  old_cursors = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      old_cursors[i] = CURSOR(cp); /* depends on edit_ctr -- set to -1 by free_edit_list below */
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, -1);
      if (cp->tracks) free_track_info_list(cp); /* needs to precede free_edit_list which clobbers cp->edit_size */
      if (cp->have_mixes) reset_mix_list(cp);
      if (cp->edits) free_edit_list(cp);
      sf[i] = free_snd_fd(sf[i]);  /* must precede free_sound_list since it accesses the snd_data structs that free_sound_list frees */
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

#if (HAVE_ACCESS)
  if (access(sp->filename, W_OK))
    {
      sa = free_axes_data(sa);
      if (ofile) FREE(ofile);
      if (old_cursors) FREE(old_cursors);
      return(IO_WRITE_PROTECTED);
    }
#endif

  mus_sound_forget(sp->filename);
  sp->writing = true;
  io_err = move_file(ofile, sp->filename); /* should we cancel and restart a monitor? */
  sp->writing = false;
  if (io_err != IO_NO_ERROR)
    {
      if (ofile) FREE(ofile);
      if (old_cursors) FREE(old_cursors);
      return(io_err);
    }

  sp->write_date = file_write_date(sp->filename);
  add_sound_data(sp->filename, sp, WITHOUT_INITIAL_GRAPH_HOOK);
  restore_axes_data(sp, sa, mus_sound_duration(sp->filename), true);
  sa = free_axes_data(sa);
  for (i = 0; i < sp->nchans; i++)
    CURSOR(sp->chans[i]) = old_cursors[i];
  FREE(old_cursors);
  reflect_file_revert_in_label(sp);
  if (ofile) 
    {
      FREE(ofile); 
      ofile = NULL;
    }
  if (!(ss->fam_ok))
    if (auto_update(ss)) 
      for_each_sound(sound_not_current);
  return(IO_NO_ERROR);
}

io_error_t save_edits_without_display(snd_info *sp, const char *new_name, int type, int format, int srate, const char *comment, int pos)
{ 
  /* assume we've already checked for (over)write permissions, and header-type+data-format writable,
   */
  file_info *hdr;
  snd_fd **sf;
  off_t frames = 0;
  int i;
  file_info *ohdr;
  io_error_t err = IO_NO_ERROR;

  if (dont_save(sp, new_name)) 
    return(IO_SAVE_HOOK_CANCELLATION);

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
      chan_info *cp;
      int local_pos;
      cp = sp->chans[i];
      if (pos == AT_CURRENT_EDIT_POSITION) local_pos = cp->edit_ctr; else local_pos = pos;
      if (frames < cp->samples[local_pos]) frames = cp->samples[local_pos];
      sf[i] = init_sample_read_any(0, cp, READ_FORWARD, local_pos); /* & err */
      if (sf[i] == NULL)
	{
	  int k;
	  /* this should not (cannot?) happen since we've supposedly checked before getting here... */
	  for (k = 0; k < sp->nchans; k++) 
	    sf[k] = free_snd_fd(sf[k]);
	  FREE(sf);
	  sf = NULL;
	  hdr = free_file_info(hdr);
	  return(err);
	}
    }
  {
    bool old_check;
    old_check = ss->checking_explicitly;
    ss->checking_explicitly = true;
    err = snd_make_file(new_name, sp->nchans, hdr, sf, frames);
    ss->checking_explicitly = old_check;
  }

  for (i = 0; i < sp->nchans; i++) 
    free_snd_fd(sf[i]);
  FREE(sf);
  free_file_info(hdr);

  return(err);
}

io_error_t save_channel_edits(chan_info *cp, const char *ofile, int pos)
{
  /* channel extraction -- does not (normally) cause reversion of edits, or change of in-window file, etc */
  snd_info *sp;
  io_error_t err = IO_NO_ERROR;
  sp = cp->sound;
  if (pos == AT_CURRENT_EDIT_POSITION) 
    pos = cp->edit_ctr;
  if (strcmp(ofile, sp->filename) == 0)       /* overwriting current file with one of its channels */
    {
      char *nfile = NULL;
      if (sp->user_read_only || sp->file_read_only)
	{
	  snd_error(_("can't save channel as %s (%s is write-protected)"), ofile, sp->short_filename);
	  return(IO_WRITE_PROTECTED);
	}
      nfile = snd_tempnam();
      err = channel_to_file(cp, nfile, pos);  /* snd_error unless MUS_INTERRUPTED (???) */
      if (err == IO_NO_ERROR)
	{
	  err = move_file(nfile, ofile);
	  if (err == IO_NO_ERROR)
	    snd_update(sp);
	  else
	    {
	      if (SERIOUS_IO_ERROR(err))
		{
		  FREE(nfile);
		  nfile = NULL;
		  snd_error("save channel %s -> %s: %s (%s)", 
			    nfile, ofile, 
			    io_error_name(err),
			    snd_io_strerror());
		}
	    }
	}
      if (nfile) FREE(nfile);
    }
  else err = channel_to_file(cp, ofile, pos); /* snd_error unless MUS_INTERRUPTED */
  return(err);
}

bool has_unsaved_edits(snd_info *sp)
{
  int i;
  for (i = 0; i < sp->nchans; i++)
    if (sp->chans[i]->edit_ctr > 0)
      return(true);
  return(false);
}

static io_error_t save_edits_1(snd_info *sp, bool ask)
{
  io_error_t err;
  time_t current_write_date;

  if (sp == NULL)
    snd_error_without_format("save edits of null sound!");
  if ((sp->user_read_only) || (sp->file_read_only))
    return(IO_WRITE_PROTECTED);

  if (!(has_unsaved_edits(sp))) return(IO_NO_CHANGES);

  /* check for change to file while we were editing it */
  current_write_date = file_write_date(sp->filename);
  /* returns -1 if file does not exist (stat -> -1) */
  if (current_write_date < 0)
    {
      snd_error(_("can't save edits; %s has disappeared!"), sp->filename); 
      /* unless by chance it fits in one in-core buffer, there's nothing we can do now */
      return(IO_CANT_OPEN_FILE);
    }
  if ((ask) &&
      (ask_before_overwrite(ss)) &&
      ((current_write_date - sp->write_date) > 1)) /* In Redhat 7.1 these can differ by 1?? Surely this is a bug! */
    return(IO_NEED_WRITE_CONFIRMATION);            /* see snd-kbd.c save_edits_with_prompt for the rest of this */
  err = save_edits_and_update_display(sp);
  if (err == IO_NO_ERROR)
    {
      if (sp->edited_region) 
	save_region_backpointer(sp);
    }
  return(err);
}

io_error_t save_edits(snd_info *sp)
{
  return(save_edits_1(sp, true));
}

io_error_t save_edits_without_asking(snd_info *sp)
{
  return(save_edits_1(sp, false));
}

void revert_edits(chan_info *cp)
{
  int old_ctr;
  if (cp->edit_ctr == 0) return;
  old_ctr = cp->edit_ctr;
  cp->edit_ctr = 0;
  clear_transform_edit_ctrs(cp);
  reflect_edit_counter_change(cp);
  reflect_sample_change_in_axis(cp);
  call_selection_watchers(SELECTION_IN_DOUBT);
  if (cp->have_mixes) sync_mixes_with_edits(cp);
  update_track_lists(cp, old_ctr - 1);
  update_graph(cp);
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
  reflect_enved_spectra_change(cp);
  if ((XEN_HOOK_P(cp->undo_hook)) && (XEN_HOOKED(cp->undo_hook)))
    run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
}

/* how to handle something like safe-map-channel that wants to disallow undo? */

bool undo_edit(chan_info *cp, int count)
{
  if ((cp) && 
      (cp->edit_ctr > 0) && 
      (count != 0))
    {
      snd_info *sp;
      sp = cp->sound;
      cp->edit_ctr -= count; 
      if (cp->edit_ctr < 0) cp->edit_ctr = 0;
      clear_transform_edit_ctrs(cp);
      reflect_edit_counter_change(cp);
      reflect_sample_change_in_axis(cp);
      if (cp->edit_ctr == 0)
	{
	  reflect_file_revert_in_label(sp);
	}
      call_selection_watchers(SELECTION_IN_DOUBT);
      if (cp->have_mixes) sync_mixes_with_edits(cp);
      update_track_lists(cp, 0);
      update_graph(cp);
      reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
      reflect_enved_spectra_change(cp);
      if ((XEN_HOOK_P(cp->undo_hook)) && (XEN_HOOKED(cp->undo_hook)))
	run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
      return(true);
    }
  return(false);
}

bool undo_edit_with_sync(chan_info *cp, int count)
{
  /* there is a problem with syncd undo: if the syncd edit decided one portion
   *   was a no-op (scale by 1.0 etc), but not another, a subsequent undo with
   *   sync can end up in a state different from where it started.
   */
  if (count == 0) return(false);
  if (count < 0)
    return(redo_edit_with_sync(cp, -count));
  else
    {
      if (cp)
	{
	  bool something_changed = false;
	  snd_info *sp;
	  sync_info *si = NULL;
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(sp->sync);
	  if (si)
	    {
	      int i;
	      for (i = 0; i < si->chans; i++) 
		if (undo_edit(si->cps[i], count))
		  something_changed = true;
	      si = free_sync_info(si);
	      return(something_changed);
	    }
	  else return(undo_edit(cp, count));
	}
    }
  return(false);
}

bool redo_edit(chan_info *cp, int count)
{
  /* returns true if an edit history change occurred */
  if ((cp) && (count != 0))
    {
      int old_edit_ctr;
      old_edit_ctr = cp->edit_ctr;
      cp->edit_ctr += count;
      if (cp->edit_ctr >= cp->edit_size) cp->edit_ctr = cp->edit_size - 1;
      while (!(cp->edits[cp->edit_ctr]))
	cp->edit_ctr--;
      if ((cp->edit_ctr != 0) &&          /* possibly a sync'd redo to chan that has no edits */
	  (cp->edit_ctr != old_edit_ctr)) /* or attempt to redo when nothing to redo */
	{
	  clear_transform_edit_ctrs(cp);
	  reflect_file_change_in_label(cp);
	  reflect_edit_counter_change(cp);
	  reflect_sample_change_in_axis(cp);
	  call_selection_watchers(SELECTION_IN_DOUBT);
	  if (cp->have_mixes) sync_mixes_with_edits(cp); 
	  /* update_graph also checks this, but it may not get run for various reasons,
	   *   and the mix states have to be up-to-date to avoid being optimized out
	   *   when, for example, set-mix-amp undo set again
	   */
	  update_track_lists(cp, 0);
	  update_graph(cp);
	  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
	  reflect_enved_spectra_change(cp);
	  if ((XEN_HOOK_P(cp->undo_hook)) && (XEN_HOOKED(cp->undo_hook)))
	    run_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
	  return(true);
	}
    }
  return(false);
}

bool redo_edit_with_sync(chan_info *cp, int count)
{
  if (count == 0) return(false);
  if (count < 0)
    return(undo_edit_with_sync(cp, -count));
  else
    {
      if (cp)
	{
	  bool something_changed = false;
	  snd_info *sp;
	  sync_info *si = NULL;
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(sp->sync);
	  if (si)
	    {
	      int i;
	      for (i = 0; i < si->chans; i++) 
		if (redo_edit(si->cps[i], count))
		  something_changed = true;
	      si = free_sync_info(si);
	      return(something_changed);
	    }
	  else return(redo_edit(cp, count));
	}
    }
  return(false);
}



/* ----------------------- Xen connection -------------------------------- */

static XEN g_display_edits(XEN snd, XEN chn, XEN edpos, XEN with_source)
{
  #define H_display_edits "(" S_display_edits " :optional snd chn edpos (with-source " PROC_TRUE ")): current edit tree"
  FILE *tmp = NULL;
  char *buf, *name;
  chan_info *cp;
  int fd, pos = AT_CURRENT_EDIT_POSITION;
  bool include_source = true;
  off_t len;
  XEN res;
  size_t bytes;

  ASSERT_CHANNEL(S_display_edits, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_source), with_source, XEN_ARG_4, S_display_edits, "boolean");
  cp = get_cp(snd, chn, S_display_edits);
  if (!cp) return(XEN_FALSE);

  if (XEN_BOOLEAN_P(with_source)) include_source = XEN_TO_C_BOOLEAN(with_source);
  if (XEN_INTEGER_P(edpos)) 
    {
      pos = XEN_TO_C_INT(edpos);
      if (pos == AT_CURRENT_EDIT_POSITION)
	pos = cp->edit_ctr;
      if ((pos < 0) || (pos >= cp->edit_size) || (!(cp->edits[pos])))
	XEN_ERROR(NO_SUCH_EDIT,
		  XEN_LIST_2(C_TO_XEN_STRING(S_display_edits),
			     edpos));
    }
  name = snd_tempnam();
  tmp = FOPEN(name, "w");
  if (tmp)
    {
      if (pos != AT_CURRENT_EDIT_POSITION)
	display_ed_list(cp, tmp, pos, cp->edits[pos], include_source);
      else display_edits(cp, tmp, include_source);
      snd_fclose(tmp, name);
    }
  else XEN_ERROR(CANNOT_SAVE,
		 XEN_LIST_3(C_TO_XEN_STRING(S_display_edits),
			    C_TO_XEN_STRING(name),
			    C_TO_XEN_STRING(snd_io_strerror())));
  fd = mus_file_open_read(name);
  len = lseek(fd, 0L, SEEK_END);
  buf = (char *)CALLOC(len + 1, sizeof(char));
  lseek(fd, 0L, SEEK_SET);
  bytes = read(fd, buf, len);
  snd_close(fd, name);
  snd_remove(name, IGNORE_CACHE);
  if (name) FREE(name);
  if (bytes != 0)
    res = C_TO_XEN_STRING(buf);
  else res = C_STRING_TO_XEN_SYMBOL("read-error");
  FREE(buf);
  return(res);
}

static XEN g_edit_fragment(XEN uctr, XEN snd, XEN chn)
{
  #define H_edit_fragment "(" S_edit_fragment " :optional (ctr " S_current_edit_position ") snd chn): edit history entry at ctr \
associated with snd's channel chn; the returned value is a list (origin type start-sample samps)"

  chan_info *cp;
  int ctr;
  ASSERT_CHANNEL(S_edit_fragment, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uctr), uctr, XEN_ARG_1, S_edit_fragment, "an integer");
  cp = get_cp(snd, chn, S_edit_fragment);
  if (!cp) return(XEN_FALSE);
  ctr = XEN_TO_C_INT_OR_ELSE(uctr, cp->edit_ctr);
  if ((ctr < cp->edit_size) && 
      (ctr >= 0))
    {
      ed_list *ed;
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
  #define H_edit_tree "(" S_edit_tree " :optional snd chn edpos): \
the edit lists '((global-pos data-num local-pos local-end scaler rmp0 rmp1 type)...)"
  /* internal debugging (auto-test) aid -- return complete ed list at pos */
  int i, len, pos;
  chan_info *cp;
  ed_list *ed;
  XEN res = XEN_EMPTY_LIST;
  ASSERT_CHANNEL(S_edit_tree, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_tree);
  if (!cp) return(XEN_FALSE);
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

static XEN g_edit_fragment_type_name(XEN type)
{
  int typ;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, "edit-fragment-type-name", "an int");
  typ = XEN_TO_C_INT(type);
  if ((typ >= 0) && (typ < NUM_OPS))
    return(C_TO_XEN_STRING(type_info[typ].name));
  return(XEN_FALSE);
}



/* ---------------- sample readers ---------------- */

static XEN_OBJECT_TYPE sf_tag;
bool sf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sf_tag));}
#define SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, sf_tag)
#define ANY_SAMPLE_READER_P(Obj) ((sf_p(Obj)) || (mf_p(Obj)) || (tf_p(Obj)))
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
	      if (fd->type == SAMPLE_READER)
		{
		  name = cp->sound->short_filename;
		  if (name == NULL)
		    switch (cp->sound->inuse)
		      {
		      case SOUND_IDLE:    name = "idle source";      break;
		      case SOUND_NORMAL:  name = "unknown source";   break;
		      case SOUND_WRAPPER: name = "wrapped source";   break;
		      case SOUND_REGION:  name = "region as source"; break;
		      case SOUND_READER:  name = "readable source";  break;
		      }
		}
	      else name = "region as source";
	    }
	}
      if (name == NULL) name = "unknown source";
      if (fd->at_eof)
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader: %s at eof or freed>",
		     name);
      else 
	{
	  if (cp)
	    mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader: %s[%d: %d] from " OFF_TD ", at " OFF_TD ">",
			 name, cp->chan, fd->edit_ctr, fd->initial_samp, current_location(fd));
	  else mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader: %s from " OFF_TD ", at " OFF_TD ">",
			    name, fd->initial_samp, current_location(fd));
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
  int loc = -1;
  if (dangling_reader_size == 0)
    {
      dangling_reader_size = DANGLING_READER_INCREMENT;
      dangling_readers = (snd_fd **)CALLOC(dangling_reader_size, sizeof(snd_fd *));
      loc = 0;
    }
  else
    {
      int i;
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
  if (fd) 
    {
      snd_info *sp = NULL;
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
  for (i = 0; i < dangling_reader_size; i++)
    {
      snd_fd *fd;
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

XEN_MAKE_OBJECT_FREE_PROCEDURE(snd_fd, free_sf, sf_free)
/* sf_free is original, free_sf is wrapped form */

static XEN g_sample_reader_at_end(XEN obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_end_p " obj): " PROC_TRUE " if sample-reader has reached the end of its data"
  XEN_ASSERT_TYPE(ANY_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_at_end_p, "a sample-reader (of any kind)");
  if (sf_p(obj))
    {
      snd_fd *sf;
      sf = TO_SAMPLE_READER(obj);
      return(C_TO_XEN_BOOLEAN(sf->at_eof));
    }
  if (mf_p(obj))
    return(g_mix_sample_reader_at_end_p(obj));
  if (tf_p(obj))
    return(g_track_sample_reader_at_end_p(obj));
  return(XEN_FALSE);
}

/* can sample-reader-position be settable? 
 *   this requires that we find the fragment that holds the new position (as at the start of init_sample_read_any_with_bufsize 6892)
 *   set the fragment bounds (ind0, ind1), call file_buffers_forward|backward
 *   also check for reader_at_end complications, etc
 *   so, it's simpler and just as fast to require that the user make a new reader or use random access (channel->vct)
 *   (the only thing we avoid is choose_accessor)
 */

static XEN g_sample_reader_position(XEN obj) 
{
  #define H_sample_reader_position "(" S_sample_reader_position " obj): current (sample-wise) location of sample-reader"
  XEN_ASSERT_TYPE(ANY_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_position, "a sample-reader (of any kind)");
  if (sf_p(obj))
    {
      snd_fd *fd = NULL;
      fd = TO_SAMPLE_READER(obj);
      if (fd->at_eof) return(XEN_ZERO); /* -1? frames? */
      if ((fd->cp) && (fd->cp->active) && (fd->cp->sound))
	{
	  if (fd->type == SAMPLE_READER)
	    return(C_TO_XEN_OFF_T(current_location(fd)));
	  return(C_TO_XEN_OFF_T(region_current_location(fd)));
	}
    }
  if (mf_p(obj))
    return(g_mix_sample_reader_position(obj));
  if (tf_p(obj))
    return(g_track_sample_reader_position(obj));
  return(XEN_ZERO);
}

static XEN g_sample_reader_home(XEN obj)
{
  #define H_sample_reader_home "(" S_sample_reader_home " obj): (list sound-index chan-num) associated with a sound reader, or \
if 'obj' is a mix-sample-reader, the id of underlying mix, or if a track-sample-reader, (list track-id chan)"
  XEN_ASSERT_TYPE(ANY_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_home, "a sample-reader (of any kind)");
  if (sf_p(obj))
    {
      snd_fd *fd = NULL;
      fd = TO_SAMPLE_READER(obj);
      if ((fd->cp) && (fd->cp->active) && (fd->cp->sound))
	{
	  if (fd->type == SAMPLE_READER)
	    return(XEN_LIST_2(C_TO_XEN_INT(fd->cp->sound->index),
			      C_TO_XEN_INT(fd->cp->chan)));
	  return(XEN_LIST_2(C_TO_XEN_INT(fd->region),
			    C_TO_XEN_INT(fd->cp->chan)));
	}
    }
  if (mf_p(obj))
    return(g_mix_sample_reader_home(obj));
  if (tf_p(obj))
    return(g_track_sample_reader_home(obj));
  return(XEN_FALSE);
}

XEN g_c_make_sample_reader(snd_fd *fd)
{
  XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
}

static XEN g_make_sample_reader(XEN samp_n, XEN snd, XEN chn, XEN dir1, XEN pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sample_reader "(" S_make_sample_reader " :optional (start-samp 0) snd chn (dir 1) edpos): \
return a reader ready to access snd's channel chn's data starting at start-samp, going in direction dir (1 = \
forward, -1 = backward), reading the version of the data indicated by edpos which defaults to the current version. \
snd can be a filename, or a sound index number."

  snd_fd *fd = NULL;
  int chan, edpos, direction = 1; /* in Scheme 1=forward, -1=backward */
  chan_info *cp;
  char *filename;
  snd_info *loc_sp = NULL;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_make_sample_reader, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, XEN_ARG_4, S_make_sample_reader, "an integer");
  if (XEN_STRING_P(snd))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chn), chn, XEN_ARG_3, S_make_sample_reader, "an integer or boolean");
      filename = XEN_TO_C_STRING(snd);
      if (mus_file_probe(filename))
	loc_sp = make_sound_readable(filename, false);
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
      if (!cp) return(XEN_FALSE);
    }
  edpos = to_c_edit_position(cp, pos, S_make_sample_reader, 5);
  direction = XEN_TO_C_INT_OR_ELSE(dir1, 1);
  beg = beg_to_sample(samp_n, S_make_sample_reader);
  if (direction == 1)
    fd = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  else
    {
      if (direction == -1)
	fd = init_sample_read_any(beg, cp, READ_BACKWARD, edpos);
      else XEN_ERROR(XEN_ERROR_TYPE("no-such-direction"),
		     XEN_LIST_2(C_TO_XEN_STRING(S_make_sample_reader),
				dir1));
    }
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
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " :optional (start-samp 0) (region 0) (chn 0) (dir 1)): \
return a reader ready to access region's channel chn data starting at start-samp going in direction dir"

  snd_fd *fd = NULL;
  int reg_n, chn_n;
  off_t beg;
  int direction = 1;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_make_region_sample_reader, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(reg), reg, XEN_ARG_2, S_make_region_sample_reader, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn), chn, XEN_ARG_3, S_make_region_sample_reader, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, XEN_ARG_4, S_make_region_sample_reader, "an integer");
  reg_n = XEN_TO_C_INT_OR_ELSE(reg, 0);
  if (!(region_ok(reg_n))) 
    XEN_ERROR(XEN_ERROR_TYPE("no-such-region"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_region_sample_reader),
                         reg));
  chn_n = XEN_TO_C_INT_OR_ELSE(chn, 0);
  if ((chn_n < 0) || (chn_n >= region_chans(reg_n)))
    return(snd_no_such_channel_error(S_make_region_sample_reader, XEN_LIST_1(reg), chn));
  beg = beg_to_sample(samp_n, S_make_region_sample_reader);
  direction = XEN_TO_C_INT_OR_ELSE(dir1, 1);
  if (direction == 1)
    fd = init_region_read(beg, reg_n, chn_n, READ_FORWARD);
  else
    {
      if (direction == -1)
	fd = init_region_read(beg, reg_n, chn_n, READ_BACKWARD);
      else XEN_ERROR(XEN_ERROR_TYPE("no-such-direction"),
		     XEN_LIST_2(C_TO_XEN_STRING(S_make_region_sample_reader),
				dir1));
    }
  if (fd)
    {
      fd->edit_ctr = -2 - reg_n; /* can't use fd->cp because deferred case is pointer to original (not copied) data */
                                 /* has to be less than -1 because that is the "delete all readers" sign on chan close */
      fd->region = reg_n;
      fd->type = REGION_READER;
      list_reader(fd);
      XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(XEN_FALSE);
}

static XEN g_sample_reader_p(XEN obj)
{
  #define H_sample_reader_p "(" S_sample_reader_p " obj) -> " PROC_TRUE " if obj is a sound sample-reader."
  if (sf_p(obj))
    {
      snd_fd *fd;
      fd = TO_SAMPLE_READER(obj);
      return(C_TO_XEN_BOOLEAN(fd->type == SAMPLE_READER));
    }
  return(XEN_FALSE);
}

static XEN g_region_sample_reader_p(XEN obj)
{
  #define H_region_sample_reader_p "(" S_region_sample_reader_p " obj) -> " PROC_TRUE " if obj is a region sample-reader."
  if (sf_p(obj))
    {
      snd_fd *fd;
      fd = TO_SAMPLE_READER(obj);
      return(C_TO_XEN_BOOLEAN(fd->type == REGION_READER));
    }
  return(XEN_FALSE);
}

static XEN g_copy_sample_reader(XEN obj)
{
  #define H_copy_sample_reader "(" S_copy_sample_reader " reader): return a copy of reader"
  XEN_ASSERT_TYPE(ANY_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_copy_sample_reader, "a sample-reader (of any kind)");
  if (sf_p(obj))
    {
      snd_fd *fd = NULL;
      fd = TO_SAMPLE_READER(obj);
      if ((fd->cp) && (fd->cp->active) && (fd->cp->sound))
	{
	  if (fd->type == SAMPLE_READER)
	    return(g_make_sample_reader(C_TO_XEN_OFF_T(current_location(fd)),
					C_TO_XEN_INT(fd->cp->sound->index),
					C_TO_XEN_INT(fd->cp->chan),
					C_TO_XEN_INT((fd->direction == READ_FORWARD) ? 1 : -1), /* Scheme side is different from C side */
					C_TO_XEN_INT(fd->edit_ctr)));
	  return(g_make_region_sample_reader(C_TO_XEN_OFF_T(region_current_location(fd)),
					     C_TO_XEN_INT(fd->region),
					     C_TO_XEN_INT(fd->cp->chan),
					     C_TO_XEN_INT((fd->direction == READ_FORWARD) ? 1 : -1)));
	}
      return(XEN_FALSE);
    }
  if (mf_p(obj))
    return(g_copy_mix_sample_reader(obj));
  if (tf_p(obj))
    return(g_copy_track_sample_reader(obj));
  return(XEN_FALSE);
}

void release_region_readers(int reg)
{
  int i, regval;
  regval = -2 - reg;
  for (i = 0; i < dangling_reader_size; i++)
    {
      snd_fd *fd;
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
  #define H_free_sample_reader "(" S_free_sample_reader " reader): free a sample reader (of any kind)"
  XEN_ASSERT_TYPE(ANY_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_sample_reader, "a sample-reader");
  if (sf_p(obj))
    {
      snd_info *sp = NULL;
      snd_fd *fd;
      fd = TO_SAMPLE_READER(obj);
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd_almost(fd); /* this is different from sf_free! */
      if (sp) completely_free_snd_info(sp);
    }
  if (mf_p(obj))
    return(g_free_mix_sample_reader(obj));
  if (tf_p(obj))
    return(g_free_track_sample_reader(obj));
  return(xen_return_first(XEN_FALSE, obj));
}

static XEN g_save_edit_history(XEN filename, XEN snd, XEN chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename :optional snd chn): save snd channel's chn edit history in filename"
  FILE *fd;
  char *name;
  char *mcf = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_save_edit_history, "a string");
  ASSERT_CHANNEL(S_save_edit_history, snd, chn, 2);
  name = XEN_TO_C_STRING(filename);
  mcf = mus_expand_filename(name);
  fd = FOPEN(mcf, "w");
  if (mcf) FREE(mcf);
  if (fd)
    {
      chan_info *cp;
      if ((XEN_INTEGER_P(chn)) && (XEN_INTEGER_P(snd)))
	{
	  cp = get_cp(snd, chn, S_save_edit_history);
	  if (!cp) return(XEN_FALSE);
	  edit_history_to_file(fd, cp, false);
	}
      else
	{
	  int i, j;
	  snd_info *sp;
	  if (XEN_INTEGER_P(snd))
	    {
	      sp = get_sp(snd, NO_PLAYERS);
	      if (sp)
		for (i = 0; i < sp->nchans; i++)
		  edit_history_to_file(fd, sp->chans[i], false);
	    }
	  else
	    {
	      
	      for (i = 0; i < ss->max_sounds; i++)
		{
		  sp = ss->sounds[i];
		  if ((sp) && (sp->inuse == SOUND_NORMAL))
		    for (j = 0; j < sp->nchans; j++)
		      edit_history_to_file(fd, sp->chans[j], false);
		}
	    }
	}
      snd_fclose(fd, name);
    }
  else
    {
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_save_edit_history),
			   filename,
			   C_TO_XEN_STRING(snd_open_strerror())));
    }
  return(filename);
}

static XEN g_undo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_undo "(" S_undo " :optional (count 1) snd chn): undo 'count' edits in snd's channel chn"
  chan_info *cp;
  int num;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_undo, "an integer");
  ASSERT_CHANNEL(S_undo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_undo);
  if (!cp) return(XEN_FALSE);
  if (XEN_INTEGER_P(ed_n))
    {
      num = XEN_TO_C_INT(ed_n);
      if ((num != 0) && (num < 1000000000) && (num > -1000000000))
	{
	  if (undo_edit_with_sync(cp, num))
	    return(C_TO_XEN_INT(num));
	}
      return(XEN_ZERO);
    }
  if (undo_edit_with_sync(cp, 1))
    return(C_TO_XEN_INT(1));
  return(XEN_ZERO);
}

static XEN g_redo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_redo "(" S_redo " :optional (count 1) snd chn): redo 'count' edits in snd's channel chn"
  chan_info *cp;
  int num;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_redo, "an integer");
  ASSERT_CHANNEL(S_redo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_redo);
  if (!cp) return(XEN_FALSE);
  if (XEN_INTEGER_P(ed_n))
    {
      num = XEN_TO_C_INT(ed_n);
      if ((num != 0) && (num < 1000000000) && (num > -1000000000))
	{
	  if (redo_edit_with_sync(cp, num))
	    return(C_TO_XEN_INT(num));
	}
      return(XEN_ZERO);
    }
  if (redo_edit_with_sync(cp, 1))
    return(C_TO_XEN_INT(1));
  return(XEN_ZERO);
}


/* ---------------------------------------- AS-ONE-EDIT ---------------------------------------- */

#define INITIAL_AS_ONE_EDIT_POSITIONS_SIZE 2

void as_one_edit(chan_info *cp, int one_edit)
{
  bool need_backup = false;
  need_backup = (cp->edit_ctr > one_edit);      /* cp->edit_ctr will be changing, so save this */
  if (cp->edit_ctr >= one_edit)                 /* ">=" here because the origin needs to be set even if there were no extra edits */
    {
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, one_edit - 1);
      while (cp->edit_ctr > one_edit) backup_edit_list(cp);
      if ((need_backup) && (cp->have_mixes)) backup_mix_list(cp, one_edit);
      if (need_backup) prune_edits(cp, cp->edit_ctr + 1);
    }
}

static void init_as_one_edit(chan_info *cp) 
{
  if (cp->in_as_one_edit == 0)
    cp->previous_squelch_update = cp->squelch_update; /* preserve possible user setting across as-one-edit call */
  cp->squelch_update = true;
  if (cp->as_one_edit_positions_size == 0)
    {
      cp->as_one_edit_positions_size = INITIAL_AS_ONE_EDIT_POSITIONS_SIZE;
      cp->as_one_edit_positions = (int *)CALLOC(cp->as_one_edit_positions_size, sizeof(int));
    }
  else
    {
      if (cp->in_as_one_edit >= cp->as_one_edit_positions_size)
	{
	  cp->as_one_edit_positions_size += INITIAL_AS_ONE_EDIT_POSITIONS_SIZE;
	  cp->as_one_edit_positions = (int *)REALLOC(cp->as_one_edit_positions, cp->as_one_edit_positions_size * sizeof(int));
	}
    }
  cp->as_one_edit_positions[cp->in_as_one_edit] = cp->edit_ctr;
  cp->in_as_one_edit++;
}

static void as_one_edit_set_origin(chan_info *cp, void *origin)
{
  if (cp->as_one_edit_positions)
    {
      if ((cp->as_one_edit_positions[cp->in_as_one_edit] + 1) == cp->edit_ctr)
	{
	  ed_list *ed;
	  ed = cp->edits[cp->edit_ctr];
	  if (ed)
	    {
	      if (ed->origin) FREE(ed->origin);
	      ed->origin = copy_string((char *)origin);
	    }
	}
    }
}

static void finish_as_one_edit(chan_info *cp) 
{
  /* if a sound was opened within as-one-edit, it will have 0 here and no array */
  if ((cp->in_as_one_edit > 0) && (cp->as_one_edit_positions))
    {
      cp->in_as_one_edit--;
      if (cp->in_as_one_edit < 0)
	{
#if MUS_DEBUGGING
	  fprintf(stderr, "in_as_one_edit: %d\n", cp->in_as_one_edit);
#endif
	  cp->in_as_one_edit = 0;
	}
      as_one_edit(cp, cp->as_one_edit_positions[cp->in_as_one_edit] + 1);
      if (cp->in_as_one_edit == 0)
	{
	  cp->squelch_update = cp->previous_squelch_update;
	  if (!(cp->squelch_update)) clear_minibuffer(cp->sound);
	  reflect_edit_history_change(cp);
	  update_graph(cp);
	}
    }
}

#if HAVE_GUILE_DYNAMIC_WIND
static void before_as_one_edit(void *context)
{
  for_each_normal_chan(init_as_one_edit);
}

static XEN as_one_edit_body(void *context)
{
  return(XEN_CALL_0_NO_CATCH((XEN)context));
}

static void after_as_one_edit(void *context)
{
  for_each_normal_chan(finish_as_one_edit);
}
#endif


static XEN g_as_one_edit(XEN proc, XEN origin)
{
  #define H_as_one_edit "(" S_as_one_edit " thunk :optional origin): evaluate thunk, collecting all edits into one from the edit historys' point of view"
  XEN result = XEN_FALSE;
  char *errmsg, *as_one_edit_origin = NULL;
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

  if (XEN_STRING_P(origin))
	as_one_edit_origin = copy_string(XEN_TO_C_STRING(origin));
      else as_one_edit_origin = NULL;
#if HAVE_GUILE_DYNAMIC_WIND
  result = scm_internal_dynamic_wind((scm_t_guard)before_as_one_edit, 
				     (scm_t_inner)as_one_edit_body, 
				     (scm_t_guard)after_as_one_edit, 
				     (void *)proc,
				     (void *)proc);
#else
  for_each_normal_chan(init_as_one_edit);
  result = XEN_CALL_0_NO_CATCH(proc);
  for_each_normal_chan(finish_as_one_edit);
#endif

  if (as_one_edit_origin)
    {
      for_each_normal_chan_with_void(as_one_edit_set_origin, (void *)as_one_edit_origin);
      FREE(as_one_edit_origin);
    }
  return(xen_return_first(result, proc, origin));
}


static XEN g_scale_channel(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_scale_channel "(" S_scale_channel " scaler :optional (beg 0) (dur len) snd chn edpos): \
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
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_scale_channel, 6);
  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_channel), pos, NOT_IN_AS_ONE_EDIT);
  return(scl);
}			  

static XEN g_normalize_channel(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_normalize_channel "(" S_normalize_channel " norm :optional (beg 0) (dur len) snd chn edpos): \
scale samples in the given sound/channel between beg and beg + num to norm."

  Float norm, cur_max;
  chan_info *cp;
  off_t samp, samps;
  int pos;
  char *origin = NULL;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_1, S_normalize_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_normalize_channel, beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_normalize_channel, num, XEN_ARG_3);
  ASSERT_SOUND(S_normalize_channel, snd, 4);
  norm = XEN_TO_C_DOUBLE(scl);
  samp = beg_to_sample(beg, S_normalize_channel);
  cp = get_cp(snd, chn, S_normalize_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_normalize_channel, 6);
  samps = dur_to_samples(num, samp, cp, pos, 3, S_normalize_channel);
#if HAVE_FORTH
  if ((samp == 0) && (samps == CURRENT_SAMPLES(cp)))
    {
      cur_max = channel_maxamp(cp, pos);
      origin = mus_format("%.3f 0 " PROC_FALSE " %s", norm, S_normalize_channel);
    }
  else 
    {
      cur_max = channel_local_maxamp(cp, samp, samps, pos, NULL);
      origin = mus_format("%.3f " OFF_TD PROC_SEP OFF_TD " %s", norm, samp, samps, S_normalize_channel);
    }
#else
  if ((samp == 0) && (samps == CURRENT_SAMPLES(cp)))
    {
      cur_max = channel_maxamp(cp, pos);
      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "0" PROC_SEP PROC_FALSE, TO_PROC_NAME(S_normalize_channel), norm);
    }
  else 
    {
      cur_max = channel_local_maxamp(cp, samp, samps, pos, NULL);
      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_normalize_channel), norm, samp, samps);
    }
#endif
  if (cur_max != 0.0)
    scale_channel_with_origin(cp, norm / cur_max, samp, samps, pos, NOT_IN_AS_ONE_EDIT, origin);
  if (origin) FREE(origin);
  return(scl);
}			  

Float channel_local_maxamp(chan_info *cp, off_t beg, off_t num, int edpos, off_t *maxpos)
{
  snd_fd *sf;
  mus_sample_t ymax, mval;
  off_t i, mpos = -1;
  int j = 0;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) return(0.0);
  ymax = MUS_SAMPLE_0;
  if (num > (1 << 30))
    {
      ss->stopped_explicitly = false;
      for (i = 0; i < num; i++)
	{
	  mval = mus_sample_abs(read_sample(sf));
	  if (mval > ymax) 
	    {
	      ymax = mval;
	      mpos = i;
	    }
	  j++;
	  if (j > 1000000)
	    {
	      check_for_event();
	      if ((ss->stopped_explicitly) || (!(cp->active)))
		{
		  ss->stopped_explicitly = false;
		  string_to_minibuffer(cp->sound, _("maxamp check interrupted..."));
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
	  if (mval > ymax) 
	    {
	      ymax = mval;
	      mpos = i;
	    }
	}
    }
  if (maxpos) (*maxpos) = mpos;
  free_snd_fd(sf);
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

static mus_sample_t *g_floats_to_samples(XEN obj, int *size, const char *caller, int position)
{
  mus_sample_t *vals = NULL;
  int i, num = 0;
  if (XEN_LIST_P_WITH_LENGTH(obj, num))
    {
      XEN lst;
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
	  for (i = 0; i < num; i++) 
	    vals[i] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(XEN_VECTOR_REF(obj, i), 0.0));
	}
      else
	{
	  if (MUS_VCT_P(obj))
	    {
	      vct *v;
	      v = XEN_TO_VCT(obj);
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
  #define H_sample "(" S_sample " samp :optional snd chn edpos): \
return sample samp in snd's channel chn (this is a slow access -- use sample-readers for speed)"
  chan_info *cp;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_sample, "a number");
  ASSERT_CHANNEL(S_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_sample);
  if (!cp) return(XEN_FALSE);
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
  char *origin;
  off_t beg;
  Float fval;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_setB S_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_sample, "a number");
  ASSERT_CHANNEL(S_setB S_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_setB S_sample);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_setB S_sample, 5);
  if (pos > cp->edit_ctr)
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_2(C_TO_XEN_STRING(S_setB S_sample),
			 edpos));
  if (XEN_BOUND_P(samp_n))
    beg = beg_to_sample(samp_n, S_setB S_sample);
  else beg = CURSOR(cp);

  fval = XEN_TO_C_DOUBLE(val);
  if ((fval == 1.0) && 
      (mus_bytes_per_sample(((cp->sound)->hdr)->format) == 2))
    fval = 32767.0 / 32768.0;
  ival[0] = MUS_FLOAT_TO_SAMPLE(fval);

#if HAVE_FORTH
  origin = mus_format(OFF_TD " %.4f %s drop", beg, fval, "set-sample");
#else
  origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP "%.4f", TO_PROC_NAME("set-sample"), beg, fval);
#endif
  if (change_samples(beg, 1, ival, cp, LOCK_MIXES, origin, pos))
    update_graph(cp);
  FREE(origin);
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
#if HAVE_GAUCHE
static XEN g_set_sample_reversed(XEN *argv, int argc, void *self)
{
  XEN args[5];
  xen_gauche_load_args(args, argc, 5, argv);
  if (XEN_NOT_BOUND_P(args[1]))
    return(g_set_sample(XEN_UNDEFINED, args[0], XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(args[2]))
	return(g_set_sample(args[0], args[1], XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else 
	{
	  if (XEN_NOT_BOUND_P(args[3])) 
	    return(g_set_sample(args[0], args[2], args[1], XEN_UNDEFINED, XEN_UNDEFINED)); 
	  else 
	    {
	      if (XEN_NOT_BOUND_P(args[4])) 
		return(g_set_sample(args[0], args[3], args[1], args[2], XEN_UNDEFINED)); 
	      else return(g_set_sample(args[0], args[4], args[1], args[2], args[3]));
	    }
	}
    }
}
#endif

static XEN g_set_samples_with_origin(XEN samp_0, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN truncate, 
				     char *edname, XEN infile_chan, XEN edpos, XEN auto_delete)
{
  #define H_set_samples "(set-" S_samples " start-samp samps data :optional snd chn truncate edname (infile-chan 0) edpos auto-delete): \
set snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename)); \
start-samp can be beyond current data end; if truncate is " PROC_TRUE " and start-samp is 0, the end of the file is set to match \
the new data's end."

  chan_info *cp;
  off_t len = 0, beg;
  bool override = false;
  int pos;
  char *caller;
  if (edname)
    caller = edname;
  else caller = "set-samples";
  ASSERT_SAMPLE_TYPE(caller, samp_0, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(caller, samps, XEN_ARG_2);
  ASSERT_CHANNEL(caller, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(truncate), truncate, XEN_ARG_6, caller, "a boolean");
  cp = get_cp(snd_n, chn_n, caller);
  if (!cp) return(XEN_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(infile_chan), infile_chan, XEN_ARG_8, caller, "an integer");
  pos = to_c_edit_position(cp, edpos, caller, 9);
  beg = beg_to_sample(samp_0, caller);
  len = dur_to_samples(samps, beg, cp, pos, 2, caller);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_10, caller, "a boolean");
  if (len == 0) return(XEN_FALSE);
  override = XEN_TRUE_P(truncate);
  if (XEN_STRING_P(vect))
    {
      int inchan = 0;
      bool delete_file = false;
      off_t curlen;
      char *fname;
      curlen = CURRENT_SAMPLES(cp);
      fname = XEN_TO_C_STRING(vect);
      if (!mus_file_probe(fname))
	return(snd_no_such_file_error(caller, vect));
      inchan = XEN_TO_C_INT_OR_ELSE(infile_chan, 0);
      if ((inchan < 0) ||
	  (inchan >= mus_sound_chans(fname)))
	XEN_ERROR(NO_SUCH_CHANNEL,
		  XEN_LIST_3(C_TO_XEN_STRING(caller),
			     vect,
			     infile_chan));
      if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
      if ((beg == 0) && 
	  ((len > curlen) || override))
	file_override_samples(len, fname, cp, inchan, (delete_file) ? DELETE_ME : DONT_DELETE_ME, LOCK_MIXES, caller);
      else file_change_samples(beg, len, fname, cp, inchan, (delete_file) ? DELETE_ME : DONT_DELETE_ME, LOCK_MIXES, caller, pos);
    }
  else
    {
      mus_sample_t *ivals;
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

static XEN g_set_samples(XEN samp_0, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN truncate, XEN edname, XEN infile_chan, XEN edpos, XEN auto_delete)
{
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(edname) || XEN_STRING_P(edname) || XEN_FALSE_P(edname), edname, XEN_ARG_7, "set-samples", "a string");
  return(g_set_samples_with_origin(samp_0, samps, vect, snd_n, chn_n, truncate,
				   (char *)((XEN_STRING_P(edname)) ? XEN_TO_C_STRING(edname) : "set-samples"),
				   infile_chan, edpos, auto_delete));
}

void check_saved_temp_file(const char *type, XEN filename, XEN date_and_length)
{
  char *file;
  time_t old_time, new_time;
  off_t old_bytes, new_bytes;
  file = XEN_TO_C_STRING(filename);
  if (mus_file_probe(file))
    {
      old_time = (time_t)XEN_TO_C_ULONG(XEN_CAR(date_and_length));
      old_bytes = XEN_TO_C_OFF_T(XEN_CADR(date_and_length));
      new_time = mus_sound_write_date(file);
      new_bytes = mus_sound_length(file);
      if ((new_time != old_time) || (new_bytes != old_bytes))
	{
	  char *buf = NULL;
	  if (old_time != new_time)
	    {
	      if (old_bytes != new_bytes)
		buf = mus_format("Saved %s temp file %s: original write date: %s, current: %s, original length: " OFF_TD "bytes, current: " OFF_TD,
				 type, file,
				 snd_strftime(STRFTIME_FORMAT, old_time),
				 snd_strftime(STRFTIME_FORMAT, new_time),
				 old_bytes, new_bytes);
	      else 
		buf = mus_format("Saved %s temp file %s: original write date: %s, current: %s",
				 type, file,
				 snd_strftime(STRFTIME_FORMAT, old_time),
				 snd_strftime(STRFTIME_FORMAT, new_time));
	    }
	  else buf = mus_format("Saved %s temp file %s: original length: " OFF_TD "bytes, current: " OFF_TD,
				 type, file,
				 old_bytes, new_bytes);
	  snd_warning_without_format(buf);
	  FREE(buf);
	}
    }
}

static XEN g_override_samples_with_origin(XEN filename, XEN samps, XEN snd_n, XEN chn_n, XEN origin, XEN date)
{
  check_saved_temp_file("sound", filename, date);
  return(g_set_samples(XEN_ZERO, samps, filename, snd_n, chn_n, XEN_TRUE, origin, XEN_ZERO, XEN_FALSE, XEN_FALSE));
}

static XEN g_vct_to_channel(XEN v, XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos, XEN origin)
{
  #define H_vct_to_channel "(" S_vct_to_channel " vct :optional (beg 0) (dur len) snd chn edpos origin): \
set snd's channel chn's samples starting at beg for dur samps from vct data"
  char *caller;
  XEN_ASSERT_TYPE(MUS_VCT_P(v), v, XEN_ARG_1, S_vct_to_channel, "a vct");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_7, S_vct_to_channel, "a string");
  if (XEN_NOT_BOUND_P(beg)) beg = XEN_ZERO;
  if (XEN_NOT_BOUND_P(dur)) 
    {
      vct *v1;
      v1 = XEN_TO_VCT(v);
      dur = C_TO_XEN_INT(v1->length);
    }
  if (XEN_NOT_BOUND_P(origin))
    caller = S_vct_to_channel;
  else caller = XEN_TO_C_STRING(origin);
  return(g_set_samples_with_origin(beg, dur, v, snd_n, chn_n, XEN_FALSE, caller, XEN_FALSE, edpos, XEN_UNDEFINED));
}

static XEN samples_to_vct_1(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos, const char *caller)
{
  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  off_t i, len, beg;
  int pos, num_to_read = MIX_FILE_BUFFER_SIZE;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0) || XEN_FALSE_P(samp_0), samp_0, XEN_ARG_1, caller, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps) || XEN_FALSE_P(samps), samps, XEN_ARG_2, caller, "a number");
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, caller, 6);
  beg = beg_to_sample(samp_0, caller);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, cp->samples[pos] - beg);
  if ((beg == 0) && (len == 0)) return(XEN_FALSE); /* empty file (channel) possibility */
  if (len <= 0) XEN_OUT_OF_RANGE_ERROR(caller, 2, samps, "samples ~A <= 0?");
  fvals = (Float *)MALLOC(len * sizeof(Float));
  if (len < num_to_read) num_to_read = (int)len; /* we often want fewer than 2048 samps (MIX_FILE_BUFFER_SIZE) */
                                                 /* but this has less effect than I thought -- affects only copy case */
  sf = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, pos, num_to_read);
  if (sf)
    {
      for (i = 0; i < len; i++) 
	fvals[i] = read_sample_to_float(sf);
      free_snd_fd(sf);
    }
  return(xen_make_vct(len, fvals));
}

static XEN g_channel_to_vct(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_channel_to_vct "(" S_channel_to_vct " :optional (beg 0) (dur len) snd chn edpos): \
return a vct containing snd channel chn's data starting at beg for dur samps"

  return(samples_to_vct_1(samp_0, samps, snd_n, chn_n, edpos, S_channel_to_vct));
}

static XEN g_samples(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_samples "(" S_samples " :optional (start-samp 0) (samps len) snd chn edpos): \
return a vct containing snd channel chn's samples starting a start-samp for samps samples; edpos is the edit \
history position to read (defaults to current position)."

  return(samples_to_vct_1(samp_0, samps, snd_n, chn_n, edpos, S_samples));
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
#if HAVE_GAUCHE
static XEN g_set_samples_reversed(XEN *argv, int argc, void *self)
{
  XEN args[10];
  xen_gauche_load_args(args, argc, 10, argv);
  if (XEN_NOT_BOUND_P(args[3]))
    return(g_set_samples(args[0], args[1], args[2], 
			 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(args[4]))
	return(g_set_samples(args[0], args[1], args[3], args[2], 
			     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else 
	{
	  if (XEN_NOT_BOUND_P(args[5])) 
	    return(g_set_samples(args[0], args[1], args[4], args[2], args[3], 
				 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	  else
	    {
	      if (XEN_NOT_BOUND_P(args[6])) 
		return(g_set_samples(args[0], args[1], args[5], args[2], args[3], args[4], 
				     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	      else
		{
		  if (XEN_NOT_BOUND_P(args[7])) 
		    return(g_set_samples(args[0], args[1], args[6], args[2], args[3], args[4], args[5], 
					 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
		  else
		    {
		      if (XEN_NOT_BOUND_P(args[8])) 
			return(g_set_samples(args[0], args[1], args[7], args[2], args[3], args[4], args[5], args[6],
					     XEN_UNDEFINED, XEN_UNDEFINED));
		      else 
			{
			  if (XEN_NOT_BOUND_P(args[9]))
			    return(g_set_samples(args[0], args[1], args[8], args[2], args[3], args[4], args[5], args[6], args[7], XEN_UNDEFINED));
			  else return(g_set_samples(args[0], args[1], args[9], args[2], args[3], args[4], args[5], args[6], args[7], args[8]));
			}
		    }
		}
	    }
	}
    }

}
#endif

static XEN g_change_samples_with_origin(XEN samp_0, XEN samps, XEN origin, XEN vect, XEN snd_n, XEN chn_n, XEN edpos, XEN date)
{
  chan_info *cp;
  int pos;
  off_t beg, len;
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samp_0), samp_0, XEN_ARG_1, S_change_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samps), samps, XEN_ARG_2, S_change_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_3, S_change_samples_with_origin, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(vect), vect, XEN_ARG_4, S_change_samples_with_origin, "a filename");
  ASSERT_CHANNEL(S_change_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_change_samples_with_origin);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp_0, S_change_samples_with_origin);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_change_samples_with_origin, 7);
  check_saved_temp_file("sound", vect, date);
  file_change_samples(beg, len,
		      XEN_TO_C_STRING(vect),
		      cp, 0, DONT_DELETE_ME, LOCK_MIXES,
		      XEN_TO_C_STRING(origin), 
		      pos);
  update_graph(cp);
  return(xen_return_first(vect, origin));
}

static XEN g_insert_sound(XEN file, XEN ubeg, XEN file_chn, XEN snd_n, XEN chn_n, XEN edpos, XEN auto_delete)
{
  #if HAVE_SCHEME
    #define insert_sound_example "(" S_insert_sound " \"oboe.snd\" 1000)"
  #endif
  #if HAVE_RUBY
    #define insert_sound_example "insert_sound(\"oboe.snd\", 1000)"
  #endif
  #if HAVE_FORTH
    #define insert_sound_example "\"oboe.snd\" 1000 insert-sound"
  #endif

  #define H_insert_sound "(" S_insert_sound " file :optional (beg 0) (file-chan 0) snd chn edpos auto-delete): \
insert channel file-chan of file (or all chans if file-chan is not given) into snd's channel chn at beg or at the cursor \
position.\n  " insert_sound_example "\ninserts all of oboe.snd starting at sample 1000."

  chan_info *cp;
  static char *filename = NULL;
  int nc, i;
  char *origin;
  bool delete_file = false;
  off_t beg = 0, len;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_insert_sound, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(ubeg), ubeg, XEN_ARG_2, S_insert_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(file_chn), file_chn, XEN_ARG_3, S_insert_sound, "an integer");
  ASSERT_CHANNEL(S_insert_sound, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_sound);
  if (!cp) return(XEN_FALSE);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_insert_sound, "a boolean");
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  if (filename) FREE(filename);
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  if (!mus_file_probe(filename))
    return(snd_no_such_file_error(S_insert_sound, file));
  nc = mus_sound_chans(filename);
  if (nc <= 0)
    {
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_4(C_TO_XEN_STRING(S_insert_sound),
			   file,
			   C_TO_XEN_STRING("chans <= 0"),
			   C_TO_XEN_INT(nc)));
    }
  len = mus_sound_frames(filename);
  if (len <= 0) return(C_TO_XEN_OFF_T(len));
  if (XEN_NUMBER_P(ubeg))
    beg = beg_to_sample(ubeg, S_insert_sound);
  else beg = CURSOR(cp);
  if (XEN_INTEGER_P(file_chn))
    {
      int fchn;
      fchn = XEN_TO_C_INT(file_chn);
      if (fchn < nc)
	{
#if HAVE_FORTH
	  origin = mus_format("\"%s\" " OFF_TD " %d %s drop", filename, beg, fchn, S_insert_sound);
#else
	  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "%d", TO_PROC_NAME(S_insert_sound), filename, beg, fchn);
#endif
	  if (file_insert_samples(beg, len, filename, cp, fchn, (delete_file) ? DELETE_ME : DONT_DELETE_ME, origin,
				  to_c_edit_position(cp, edpos, S_insert_sound, 6)))
	    update_graph(cp);
	  FREE(origin);
	  return(C_TO_XEN_OFF_T(len));
	}
      else return(snd_no_such_channel_error(S_insert_sound, file, file_chn));	
    }
  else
    {
      snd_info *sp;
      sp = cp->sound;
      if (sp->nchans < nc) nc = sp->nchans;
      for (i = 0; i < nc; i++)
	{
#if HAVE_FORTH
	  origin = mus_format("\"%s\" " OFF_TD " %d %s drop", filename, beg, i, S_insert_sound);
#else
	  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "%d", TO_PROC_NAME(S_insert_sound), filename, beg, i);
#endif
	  if (file_insert_samples(beg, len, filename, sp->chans[i], i, (delete_file) ? DELETE_ME: DONT_DELETE_ME, origin,
				  /* this edit_position cannot be optimized out -- each channel may have
				   *   a different edit history, but edpos might be -1 throughout etc.
				   */
				  to_c_edit_position(sp->chans[i], edpos, S_insert_sound, 6)))
	    update_graph(sp->chans[i]);
	  FREE(origin);
	}
      return(C_TO_XEN_OFF_T(len));
    }
  return(XEN_FALSE); /* not reached */
}

static XEN g_delete_sample(XEN samp_n, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_delete_sample "(" S_delete_sample " samp :optional snd chn edpos): delete sample 'samp' from snd's channel chn"
  chan_info *cp;
  off_t samp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_delete_sample, "a number");
  ASSERT_CHANNEL(S_delete_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_delete_sample);
  if (!cp) return(XEN_FALSE);
  samp = beg_to_sample(samp_n, S_delete_sample);
  pos = to_c_edit_position(cp, edpos, S_delete_sample, 4);
  if ((samp < 0) || (samp > CURRENT_SAMPLES(cp)))
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_4(C_TO_XEN_STRING(S_delete_sample),
			 samp_n,
			 snd_n, chn_n));
  if (delete_samples(samp, 1, cp, pos))
    update_graph(cp);
  return(samp_n);
}

static XEN g_delete_samples(XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps :optional snd chn edpos): \
delete 'samps' samples from snd's channel chn starting at 'start-samp'"

  chan_info *cp;
  int pos;
  off_t samp, len;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_delete_samples, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, S_delete_samples, "a number");
  ASSERT_CHANNEL(S_delete_samples, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_delete_samples);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_delete_samples, 6);
  samp = beg_to_sample(samp_n, S_delete_samples);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(XEN_FALSE);
  if (delete_samples(samp, len, cp, pos))
    update_graph(cp);
  return(samp_n);
}

static XEN g_insert_sample(XEN samp_n, XEN val, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_insert_sample "(" S_insert_sample " sample value :optional snd chn edpos): insert 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  char *origin;
  int pos;
  off_t beg;
  Float fval;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_insert_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_insert_sample, "a number");
  ASSERT_CHANNEL(S_insert_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_sample);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp_n, S_insert_sample);
  pos = to_c_edit_position(cp, edpos, S_insert_sample, 5);
  fval = XEN_TO_C_DOUBLE(val);
  ival[0] = MUS_FLOAT_TO_SAMPLE(fval);
#if HAVE_FORTH
  origin = mus_format(OFF_TD " %.4f %s drop", beg, fval, S_insert_sample);
#else
  origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP "%.4f", TO_PROC_NAME(S_insert_sample), beg, fval);
#endif
  if (insert_samples(beg, 1, ival, cp, origin, pos))
    update_graph(cp); 
  FREE(origin);
  return(val);
}

static XEN g_insert_samples(XEN samp, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN edpos, XEN auto_delete, XEN caller)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data :optional snd chn edpos auto-delete origin): \
insert data (either a vct, a list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  int pos;
  char *origin = NULL;
  bool delete_file = false;
  off_t beg, len = 0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_1, S_insert_samples, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, S_insert_samples, "a number");
  ASSERT_CHANNEL(S_insert_samples, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(caller), caller, XEN_ARG_8, S_insert_samples, "a string");
  cp = get_cp(snd_n, chn_n, S_insert_samples);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp, S_insert_samples);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples, 6);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_insert_samples, "a boolean");
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  if (XEN_STRING_P(caller))
    origin = copy_string(XEN_TO_C_STRING(caller));
  if (XEN_STRING_P(vect))
    {
      char *filename;
      filename = mus_expand_filename(XEN_TO_C_STRING(vect));
      if (!mus_file_probe(filename))
	{
	  FREE(filename);
	  return(snd_no_such_file_error(S_insert_samples, vect));
	}
      if (mus_sound_frames(filename) <= 0) return(C_TO_XEN_INT(0));
#if HAVE_FORTH
      if (!origin) origin = mus_format(OFF_TD PROC_SEP OFF_TD " \"%s\" %s drop", beg, len, filename, S_insert_samples);
#else
      if (!origin) origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD PROC_SEP "\"%s\"", TO_PROC_NAME(S_insert_samples), beg, len, filename);
#endif
      file_insert_samples(beg, len, filename, cp, 0, (delete_file) ? DELETE_ME : DONT_DELETE_ME, origin, pos);
      if (filename) FREE(filename);
    }
  else
    {
      int ilen;
      mus_sample_t *ivals;
      ilen = (int)len;
      ivals = g_floats_to_samples(vect, &ilen, S_insert_samples, 3);
      if (ivals)
	{
 	  if (!origin) origin = copy_string(TO_PROC_NAME(S_insert_samples));
	  insert_samples(beg, (off_t)ilen, ivals, cp, origin, pos);
	  FREE(ivals);
	}
    }
  if (origin) FREE(origin);
  update_graph(cp);
  return(C_TO_XEN_OFF_T(len));
}

static XEN g_insert_samples_with_origin(XEN samp, XEN samps, XEN origin, XEN vect, XEN snd_n, XEN chn_n, XEN edpos, XEN date)
{
  chan_info *cp;
  int pos;
  off_t beg, len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samp), samp, XEN_ARG_1, S_insert_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samps), samps, XEN_ARG_2, S_insert_samples_with_origin, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_3, S_insert_samples_with_origin, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(vect), vect, XEN_ARG_4, S_insert_samples_with_origin, "a filename");
  ASSERT_CHANNEL(S_insert_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_insert_samples_with_origin);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp, S_insert_samples_with_origin);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples_with_origin, 7);
  check_saved_temp_file("sound", vect, date);
  file_insert_samples(beg, len, XEN_TO_C_STRING(vect), cp, 0, DONT_DELETE_ME, XEN_TO_C_STRING(origin), pos);
  update_graph(cp);
  return(xen_return_first(C_TO_XEN_OFF_T(len), vect));
}


/* -------- re-direct CLM input (ina etc) to use sample-readers -------- */

#include "clm2xen.h"

static int SND_TO_SAMPLE = 0;

typedef struct {
  mus_any_class *core;
  snd_info *sp;
  snd_fd **sfs;
  int chans;
  off_t *samps;
} snd_to_sample;

bool snd_to_sample_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == SND_TO_SAMPLE));}
static bool snd_to_sample_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static int snd_to_sample_channels(mus_any *ptr) {return(((snd_to_sample *)ptr)->chans);}
static off_t snd_to_sample_location(mus_any *ptr) {return(((snd_to_sample *)ptr)->samps[0]);}
static char *snd_to_sample_file_name(mus_any *ptr) {return(((snd_to_sample *)ptr)->sp->filename);}
static off_t snd_to_sample_length(mus_any *ptr) {return(CURRENT_SAMPLES(((snd_to_sample *)ptr)->sp->chans[0]));}

static int snd_to_sample_free(mus_any *ptr)
{
  snd_to_sample *spl = (snd_to_sample *)ptr;
  if (spl)
    {
      if (spl->sfs)
	{
	  int i;
	  for (i = 0; i < spl->chans; i++)
	    spl->sfs[i] = free_snd_fd(spl->sfs[i]);
	  FREE(spl->sfs);
	  spl->sfs = NULL;
	}
      if (spl->samps)
	{
	  FREE(spl->samps);
	  spl->samps = NULL;
	}
      FREE(spl);
    }
  return(0);
}

static char *snd_to_sample_buf = NULL;
static char *snd_to_sample_describe(mus_any *ptr)
{
  int i, len = PRINT_BUFFER_SIZE;
  snd_to_sample *spl = (snd_to_sample *)ptr;
  char *temp;
  if (snd_to_sample_buf)
    {
      FREE(snd_to_sample_buf);
      snd_to_sample_buf = NULL;
    }
  if (spl->sfs)
    {
      len += spl->chans * 8;
      for (i = 0; i < spl->chans; i++)
	if (spl->sfs[i])
	  {
	    temp = sf_to_string(spl->sfs[i]);
	    if (temp)
	      {
		len += snd_strlen(temp);
		FREE(temp);
	      }
	  }
    }
  snd_to_sample_buf = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(snd_to_sample_buf, len, "%s: reading %s (%d chan%s) at " OFF_TD ":[", 
	       S_snd_to_sample, 
	       spl->sp->short_filename, 
	       spl->chans, 
	       (spl->chans > 1) ? "s" : "",
	       spl->samps[0]);
  if (spl->sfs)
    {
      for (i = 0; i < spl->chans; i++)
	if (spl->sfs[i])
	  {
	    temp = sf_to_string(spl->sfs[i]);
	    if (temp)
	      {
		strcat(snd_to_sample_buf, temp);
		FREE(temp);
	      }
	    if (i < spl->chans - 1) 
	      strcat(snd_to_sample_buf, ", ");
	    else strcat(snd_to_sample_buf, "]");
	  }
	else strcat(snd_to_sample_buf, "nil, ");
    }
  else strcat(snd_to_sample_buf, "no readers]");
  return(snd_to_sample_buf);
}

Float snd_to_sample_read(mus_any *ptr, off_t frame, int chan) 
{
  snd_to_sample *spl = (snd_to_sample *)ptr;
  off_t diff, i;
  if (!(spl->sfs)) 
    spl->sfs = (snd_fd **)CALLOC(spl->chans, sizeof(snd_fd *));
  if (!(spl->sfs[chan])) 
    {
      spl->sfs[chan] = init_sample_read(frame, spl->sp->chans[chan], READ_FORWARD);
      spl->samps[chan] = frame;
      return(next_sample_to_float(spl->sfs[chan]));
    }
  diff = frame - spl->samps[chan];
  if (diff == 1)
    {
      spl->samps[chan]++;
      return(next_sample_to_float(spl->sfs[chan]));
    }
  if (diff > 1)
    {
      for (i = 1; i < diff; i++) next_sample_unscaled(spl->sfs[chan]); /* just push pointer forward */
      spl->samps[chan] = frame;
      return(next_sample_to_float(spl->sfs[chan]));
    }
  diff = -diff;
  for (i = 0; i <= diff; i++) previous_sample_unscaled(spl->sfs[chan]); /* just push pointer backward (one too far) */
  spl->samps[chan] = frame;
  return(next_sample_to_float(spl->sfs[chan])); /* always end up going forward (for simpler code) */
}

static mus_any_class SND_TO_SAMPLE_CLASS = {
  -1,
  S_snd_to_sample,
  &snd_to_sample_free,
  &snd_to_sample_describe,
  &snd_to_sample_equalp,
  0, 0,
  &snd_to_sample_length, 0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  0,
  MUS_INPUT,
  NULL,               /* environ */
  &snd_to_sample_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  &snd_to_sample_read,
  0,
  &snd_to_sample_file_name,
  0,
  &snd_to_sample_location,
  0,  /* set location (ptr, off_t loc) */
  0,
  0, 0, 0, 0, 0, 0, 0
};

static mus_any *make_snd_to_sample(snd_info *sp)
{
  snd_to_sample *gen;
  gen = (snd_to_sample *)CALLOC(1, sizeof(snd_to_sample));
  gen->core = &SND_TO_SAMPLE_CLASS;
  gen->core->type = SND_TO_SAMPLE;
  gen->chans = sp->nchans;
  gen->sp = sp;
  gen->samps = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  gen->sfs = NULL; /* created as needed */
  return((mus_any *)gen);
}

static XEN g_snd_to_sample_p(XEN os) 
{
  #define H_snd_to_sample_p "(" S_snd_to_sample_p " gen): " PROC_TRUE " if gen is an " S_snd_to_sample " generator"
  return(C_TO_XEN_BOOLEAN((mus_xen_p(os)) && 
			  (snd_to_sample_p(XEN_TO_MUS_ANY(os)))));
}

static XEN g_snd_to_sample(XEN os, XEN frame, XEN chan)
{
  #define H_snd_to_sample "(" S_snd_to_sample " gen frame chan): input sample (via snd->sample gen) at frame in channel chan"
  XEN_ASSERT_TYPE((mus_xen_p(os)) && (snd_to_sample_p(XEN_TO_MUS_ANY(os))), os, XEN_ARG_1, S_snd_to_sample, "a " S_snd_to_sample " gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(frame), frame, XEN_ARG_2, S_snd_to_sample, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_3, S_snd_to_sample, "an integer");
  return(C_TO_XEN_DOUBLE(snd_to_sample_read((mus_any *)XEN_TO_MUS_ANY(os), 
					    XEN_TO_C_OFF_T(frame), 
					    XEN_TO_C_INT_OR_ELSE(chan, 0))));
}

static XEN g_make_snd_to_sample(XEN snd)
{
  #define H_make_snd_to_sample "(" S_make_snd_to_sample " snd): return a new " S_snd_to_sample " (Snd to CLM input) generator"
  mus_any *ge;
  snd_info *sp;
  ASSERT_SOUND(S_make_snd_to_sample, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_make_snd_to_sample, snd));
  ge = make_snd_to_sample(sp);
  if (ge)
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_edit_list_to_function(XEN snd, XEN chn, XEN start, XEN end)
{
  #define H_edit_list_to_function "(" S_edit_list_to_function " :optional snd chn start end) -> function encapsulating edits"
  chan_info *cp;
  char *funcstr = NULL;
  XEN func;
  int start_pos = 1, end_pos = -1;
  ASSERT_CHANNEL(S_edit_list_to_function, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_list_to_function);
  if (!cp) return(XEN_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(start), start, XEN_ARG_3, S_edit_list_to_function, "an integer");  
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(end), end, XEN_ARG_4, S_edit_list_to_function, "an integer");  
  start_pos = XEN_TO_C_INT_OR_ELSE(start, 1);
  end_pos = XEN_TO_C_INT_OR_ELSE(end, -1);
  funcstr = edit_list_to_function(cp, start_pos, end_pos);
  func = XEN_EVAL_C_STRING(funcstr);
#if HAVE_RUBY
  rb_set_property(rb_obj_id(func), C_STRING_TO_XEN_SYMBOL("proc_source"), C_TO_XEN_STRING(funcstr));
#endif
#if HAVE_FORTH
  fth_proc_source_set(func, C_TO_XEN_STRING(funcstr));
#endif
  FREE(funcstr);
  return(func);
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
XEN_NARGIFY_1(g_sample_reader_p_w, g_sample_reader_p)
XEN_NARGIFY_1(g_region_sample_reader_p_w, g_region_sample_reader_p)
XEN_NARGIFY_1(g_sample_reader_at_end_w, g_sample_reader_at_end)
XEN_NARGIFY_1(g_copy_sample_reader_w, g_copy_sample_reader)
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
XEN_ARGIFY_8(g_insert_samples_w, g_insert_samples)
XEN_ARGIFY_7(g_vct_to_channel_w, g_vct_to_channel)
XEN_ARGIFY_5(g_channel_to_vct_w, g_channel_to_vct)
XEN_ARGIFY_7(g_insert_sound_w, g_insert_sound)
XEN_ARGIFY_6(g_scale_channel_w, g_scale_channel)
XEN_ARGIFY_6(g_normalize_channel_w, g_normalize_channel)
XEN_ARGIFY_8(g_change_samples_with_origin_w, g_change_samples_with_origin)
XEN_ARGIFY_8(g_insert_samples_with_origin_w, g_insert_samples_with_origin)
XEN_ARGIFY_6(g_override_samples_with_origin_w, g_override_samples_with_origin)
XEN_ARGIFY_4(g_sample_w, g_sample)
XEN_ARGIFY_5(g_set_sample_w, g_set_sample)
XEN_ARGIFY_5(g_samples_w, g_samples)
XEN_ARGIFY_10(g_set_samples_w, g_set_samples)
XEN_NARGIFY_1(g_snd_to_sample_p_w, g_snd_to_sample_p)
XEN_ARGIFY_3(g_snd_to_sample_w, g_snd_to_sample)
XEN_ARGIFY_1(g_make_snd_to_sample_w, g_make_snd_to_sample)
XEN_ARGIFY_4(g_edit_list_to_function_w, g_edit_list_to_function)
#else
#define g_make_sample_reader_w g_make_sample_reader
#define g_make_region_sample_reader_w g_make_region_sample_reader
#define g_next_sample_w g_next_sample
#define g_read_sample_w g_read_sample
#define g_previous_sample_w g_previous_sample
#define g_free_sample_reader_w g_free_sample_reader
#define g_sample_reader_home_w g_sample_reader_home
#define g_sample_reader_position_w g_sample_reader_position
#define g_sample_reader_p_w g_sample_reader_p
#define g_region_sample_reader_p_w g_region_sample_reader_p
#define g_sample_reader_at_end_w g_sample_reader_at_end
#define g_copy_sample_reader_w g_copy_sample_reader
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
#define g_vct_to_channel_w g_vct_to_channel
#define g_channel_to_vct_w g_channel_to_vct
#define g_insert_sound_w g_insert_sound
#define g_scale_channel_w g_scale_channel
#define g_normalize_channel_w g_normalize_channel
#define g_change_samples_with_origin_w g_change_samples_with_origin
#define g_insert_samples_with_origin_w g_insert_samples_with_origin
#define g_override_samples_with_origin_w g_override_samples_with_origin
#define g_sample_w g_sample
#define g_set_sample_w g_set_sample
#define g_samples_w g_samples
#define g_set_samples_w g_set_samples
#define g_snd_to_sample_p_w g_snd_to_sample_p
#define g_make_snd_to_sample_w g_make_snd_to_sample
#define g_snd_to_sample_w g_snd_to_sample
#define g_edit_list_to_function_w g_edit_list_to_function
#endif



void g_init_edits(void)
{
#if (!HAVE_GAUCHE)
  sf_tag = XEN_MAKE_OBJECT_TYPE("SampleReader", sizeof(snd_fd));
#else
  sf_tag = XEN_MAKE_OBJECT_TYPE("<sample-reader>", sizeof(snd_fd), print_sf, free_sf);
  XEN_EVAL_C_STRING("(define-method object-apply ((rd <sample-reader>)) (read-sample rd))");
#endif

#if HAVE_GUILE
  scm_set_smob_print(sf_tag, print_sf);
  scm_set_smob_free(sf_tag, free_sf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sf_tag, XEN_PROCEDURE_CAST g_read_sample, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(sf_tag, "to_s", XEN_PROCEDURE_CAST print_sf, 0);
  rb_define_method(sf_tag, "call", XEN_PROCEDURE_CAST g_read_sample, 0);
#endif
#if HAVE_FORTH
  fth_set_object_inspect(sf_tag, print_sf);
  fth_set_object_free(sf_tag, free_sf);
  fth_set_object_apply(sf_tag, XEN_PROCEDURE_CAST g_read_sample, 0, 0, 0);
#endif

  XEN_DEFINE_CONSTANT(S_current_edit_position,         AT_CURRENT_EDIT_POSITION,         "represents the current edit history list position (-1)");

  XEN_DEFINE_PROCEDURE(S_make_sample_reader,           g_make_sample_reader_w,           0, 5, 0, H_make_sample_reader);
  XEN_DEFINE_PROCEDURE(S_make_region_sample_reader,    g_make_region_sample_reader_w,    0, 4, 0, H_make_region_sample_reader);
  XEN_DEFINE_PROCEDURE(S_read_sample,                  g_read_sample_w,                  1, 0, 0, H_read_sample);
  XEN_DEFINE_PROCEDURE(S_read_region_sample,           g_read_sample_w,                  1, 0, 0, H_read_sample);
  XEN_DEFINE_PROCEDURE(S_next_sample,                  g_next_sample_w,                  1, 0, 0, H_next_sample);
  XEN_DEFINE_PROCEDURE(S_previous_sample,              g_previous_sample_w,              1, 0, 0, H_previous_sample);
  XEN_DEFINE_PROCEDURE(S_free_sample_reader,           g_free_sample_reader_w,           1, 0, 0, H_free_sample_reader);
  XEN_DEFINE_PROCEDURE(S_sample_reader_home,           g_sample_reader_home_w,           1, 0, 0, H_sample_reader_home);
  XEN_DEFINE_PROCEDURE(S_sample_reader_p,              g_sample_reader_p_w,              1, 0, 0, H_sample_reader_p);
  XEN_DEFINE_PROCEDURE(S_region_sample_reader_p,       g_region_sample_reader_p_w,       1, 0, 0, H_region_sample_reader_p);
  XEN_DEFINE_PROCEDURE(S_sample_reader_at_end_p,       g_sample_reader_at_end_w,         1, 0, 0, H_sample_reader_at_end);
  XEN_DEFINE_PROCEDURE(S_sample_reader_position,       g_sample_reader_position_w,       1, 0, 0, H_sample_reader_position);
  XEN_DEFINE_PROCEDURE(S_copy_sample_reader,           g_copy_sample_reader_w,           1, 0, 0, H_copy_sample_reader);

  XEN_DEFINE_PROCEDURE(S_save_edit_history,            g_save_edit_history_w,            1, 2, 0, H_save_edit_history);
  XEN_DEFINE_PROCEDURE(S_edit_fragment,                g_edit_fragment_w,                0, 3, 0, H_edit_fragment);
  XEN_DEFINE_PROCEDURE("edit-fragment-type-name",      g_edit_fragment_type_name,        1, 0, 0, "internal testing function");

  XEN_DEFINE_PROCEDURE(S_undo,                         g_undo_w,                         0, 3, 0, H_undo);
#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE("undo_edit",                    g_undo_w,                         0, 3, 0, H_undo);
#endif
  XEN_DEFINE_PROCEDURE(S_redo,                         g_redo_w,                         0, 3, 0, H_redo);
  XEN_DEFINE_PROCEDURE(S_as_one_edit,                  g_as_one_edit_w,                  1, 1, 0, H_as_one_edit);
  XEN_DEFINE_PROCEDURE(S_display_edits,                g_display_edits_w,                0, 4, 0, H_display_edits);
  XEN_DEFINE_PROCEDURE(S_edit_tree,                    g_edit_tree_w,                    0, 3, 0, H_edit_tree);

  XEN_DEFINE_PROCEDURE(S_delete_sample,                g_delete_sample_w,                1, 3, 0, H_delete_sample);
  XEN_DEFINE_PROCEDURE(S_delete_samples,               g_delete_samples_w,               2, 3, 0, H_delete_samples);
  XEN_DEFINE_PROCEDURE(S_insert_sample,                g_insert_sample_w,                2, 3, 0, H_insert_sample);
  XEN_DEFINE_PROCEDURE(S_insert_samples,               g_insert_samples_w,               3, 5, 0, H_insert_samples);
  XEN_DEFINE_PROCEDURE(S_vct_to_channel,               g_vct_to_channel_w,               1, 6, 0, H_vct_to_channel);
  XEN_DEFINE_PROCEDURE(S_channel_to_vct,               g_channel_to_vct_w,               0, 5, 0, H_channel_to_vct);
  XEN_DEFINE_PROCEDURE(S_insert_sound,                 g_insert_sound_w,                 1, 6, 0, H_insert_sound);
  XEN_DEFINE_PROCEDURE(S_scale_channel,                g_scale_channel_w,                1, 5, 0, H_scale_channel);
  XEN_DEFINE_PROCEDURE(S_normalize_channel,            g_normalize_channel_w,            1, 5, 0, H_normalize_channel);

  XEN_DEFINE_PROCEDURE(S_change_samples_with_origin,   g_change_samples_with_origin_w,   7, 1, 0, "internal function used in save-state");
  XEN_DEFINE_PROCEDURE(S_insert_samples_with_origin,   g_insert_samples_with_origin_w,   7, 1, 0, "internal function used in save-state");
  XEN_DEFINE_PROCEDURE(S_override_samples_with_origin, g_override_samples_with_origin_w, 5, 1, 0, "internal function used in save-state");

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sample,  g_sample_w, H_sample,
					    S_setB S_sample, g_set_sample_w, g_set_sample_reversed, 0, 4, 1, 4);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_samples, g_samples_w, H_samples,
					    S_setB S_samples, g_set_samples_w, g_set_samples_reversed, 0, 5, 3, 7);

  XEN_DEFINE_PROCEDURE("set-sample",                   g_set_sample_w,                   2, 3, 0, H_sample);   /* for edit-list->function */
  XEN_DEFINE_PROCEDURE("set-samples",                  g_set_samples_w,                  3, 7, 0, H_set_samples);

  XEN_DEFINE_PROCEDURE(S_snd_to_sample_p,              g_snd_to_sample_p_w,              1, 0, 0, H_snd_to_sample_p);
  XEN_DEFINE_PROCEDURE(S_make_snd_to_sample,           g_make_snd_to_sample_w,           0, 1, 0, H_make_snd_to_sample);
  XEN_DEFINE_PROCEDURE(S_snd_to_sample,                g_snd_to_sample_w,                2, 1, 0, H_snd_to_sample);
  XEN_DEFINE_PROCEDURE(S_edit_list_to_function,        g_edit_list_to_function_w,        0, 4, 0, H_edit_list_to_function);

  #define H_save_hook S_save_hook " (snd name): called each time a file is about to be saved. \
If it returns " PROC_TRUE ", the file is not saved.  'name' is " PROC_FALSE " unless the file is being saved under a new name (as in sound-save-as)."

  save_hook = XEN_DEFINE_HOOK(S_save_hook, 2, H_save_hook);      /* arg = sound index, possible new name */

  #define H_save_state_hook S_save_state_hook " (temp-filename): called each time the " S_save_state " \
mechanism is about to create a new temporary file to save some edit history sample values. \
temp-filename is the current file. \
If the hook returns a string, it is treated as the new temp filename.  This hook provides a way to \
keep track of which files are in a given saved state batch, and a way to rename or redirect those files."

  save_state_hook = XEN_DEFINE_HOOK(S_save_state_hook, 1, H_save_state_hook);      /* arg = temp-filename */
  empty_closure = xen_make_vct(1, (Float *)CALLOC(1, sizeof(Float)));
  XEN_PROTECT_FROM_GC(empty_closure);

  SND_TO_SAMPLE = mus_make_class_tag();

#if MUS_DEBUGGING && 0
  /* consistency checks for the accessor state table */
  init_hit_entries();
  check_type_info_entry(ED_SIMPLE, 0, 0, 0, false);
  check_type_info_entry(ED_ZERO, 0, 0, 0, true);
  report_unhit_entries();
#endif
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
      rampn can be implemented via polynomial multiply: incr currently has implicit
        ramp location, would need float array of polynomial coeffs and some more
        explicit notion of x.  (ax+b)(cx+d)->(acx^2 + (ad+bc)x + bd) and so on
        xramps could also be collapsed, but I think they would require an extra exp:
        we only win via algebra if both segments offsets are 0.0; then it can collapse 
        to e^(x1+x2) essentially.  Unfortunately, this almost never happens.
        We could also use Horner's rule here, and use just one "x" (and incr) for
        all ramps, but this entire discussion assumes ramps in order (i.e. ramp3,
        not split), and costs an add (or a multiply) per ramp over the current version,
        so we're trading space for speed and flexibility.

        Both cases look complicated at fragment setup -- perhaps the current brute-force-but-obvious 
        code is better.

      change over to an array of functions: ramp_start_number, xramp+scale, ptree(zero), etc
        the basic accessor sequence can be (*(arr[1]))(sf, ((*(arr[0]))(sf, sf->data[loc...]))) --
        means changing function type slightly, and ptree_zero case is sticky, and scalers are tricky 
*/
