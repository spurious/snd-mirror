#include "snd.h"

static int dont_edit(chan_info *cp) 
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(cp->edit_hook))
    res = g_c_run_or_hook(cp->edit_hook, XEN_EMPTY_LIST, S_edit_hook);
  return(XEN_TRUE_P(res));
}

static XEN save_hook;
static int dont_save(snd_info *sp, char *newname)
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(save_hook))
    res = g_c_run_or_hook(save_hook,
			  XEN_LIST_2(C_TO_SMALL_XEN_INT(sp->index),
				     (newname) ? C_TO_XEN_STRING(newname) : XEN_FALSE),
			  S_save_hook);
  return(XEN_TRUE_P(res));
}

static void after_edit(chan_info *cp)
{
  if (XEN_HOOKED(cp->after_edit_hook))
    g_c_run_progn_hook(cp->after_edit_hook, XEN_EMPTY_LIST, S_after_edit_hook);
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
  /* look for buffers or open temp files that are no longer reachable after pruning the edit tree */
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

static int add_sound_file_to_edit_list(chan_info *cp, char *name, snd_io *io, file_info *hdr, int temp, int chan)
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
	if (cp->ptrees[i]) 
	  cp->ptrees[i] = free_ptree(cp->ptrees[i]);
      FREE(cp->ptrees);
      cp->ptrees = NULL;
    }
  cp->ptree_ctr = -1;
  cp->ptree_size = 0;
}


static int add_ptree(chan_info *cp)
{
  int i;
  cp->ptree_ctr++;
  if (cp->ptree_ctr >= cp->ptree_size)
    {
      cp->ptree_size += EDIT_ALLOC_SIZE;
      if (cp->ptrees)
	{
	  cp->ptrees = (void **)REALLOC(cp->ptrees, cp->ptree_size * sizeof(void *));
	  for (i = cp->ptree_ctr; i < cp->ptree_size; i++) cp->ptrees[i] = NULL;
	}
      else cp->ptrees = (void **)CALLOC(cp->ptree_size, sizeof(void *));
    }
  return(cp->ptree_ctr);
}


static ed_list *free_ed_list(ed_list *ed);
static void prune_edits(chan_info *cp, int edpt)
{
  int i;
  if (cp->edits[edpt]) 
    {
      if ((cp->state) && (cp->state->deferred_regions > 0))
	sequester_deferred_regions(cp, edpt - 1);
      for (i = edpt; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = free_ed_list(cp->edits[i]);
	  cp->amp_envs[i] = free_amp_env(cp, i);
	}
      release_pending_marks(cp, edpt);
      release_pending_mixes(cp, edpt);
      release_pending_sounds(cp, edpt);
      reflect_no_more_redo_in_menu();
    }
}

static void prepare_edit_list(chan_info *cp, off_t len)
{
  int i;
  snd_info *sp;
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
      if (!(cp->amp_envs)) cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
      else cp->amp_envs = (env_info **)REALLOC(cp->amp_envs, cp->edit_size * sizeof(env_info *));
      for (i = cp->edit_ctr; i < cp->edit_size; i++) 
	{
	  cp->edits[i] = NULL; 
	  cp->amp_envs[i] = NULL; 
	  cp->samples[i] = 0;
	}
    }
  prune_edits(cp, cp->edit_ctr);
  reflect_undo_ok_in_menu();
  cp->samples[cp->edit_ctr] = len;
}

off_t current_ed_samples(chan_info *cp)
{
  if (cp) 
    return(cp->samples[cp->edit_ctr]);
  else return(0);
}

static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  off_t samps;
  ap = cp->axis;
  if (ap)
    {
      samps = current_ed_samples(cp);
      ap->xmax = (double)samps / (double)SND_SRATE(cp->sound);
      ap->x_ambit = ap->xmax - ap->xmin;
      if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
      if ((samps == 0) || (ap->no_data))
	{
	  ap->no_data = (samps == 0);
	  if (ap->xlabel) FREE(ap->xlabel);
	  if (samps == 0) 
	    ap->xlabel = copy_string(STR_no_data); 
	  else ap->xlabel = copy_string(STR_time);
	}
      set_x_bounds(ap);
    }
}

static void reflect_file_change_in_label (chan_info *cp)
{
  snd_info *sp;
  char *starred_name;
  int len;
  if (cp->edit_ctr == 0) return;
  sp = cp->sound;
  if (sp->sgx == NULL) return;
  len = strlen(shortname(sp)) + 16;
  starred_name = (char *)CALLOC(len, sizeof(char));
  strcpy(starred_name, shortname_indexed(sp));
  if (sp->read_only) 
    strcat(starred_name, "(*)");
  else strcat(starred_name, "*");
  set_sound_pane_file_label(sp, starred_name);
  make_a_big_star_outa_me(sp->short_filename, 1);
  FREE(starred_name);
}

static void check_for_first_edit(chan_info *cp)
{
  if ((cp->cgx) && (cp->edit_ctr == 1)) /* first edit on this file (?) */
    {
      reflect_file_change_in_menu();
      reflect_file_change_in_label(cp);
    }
}




/* -------- EDIT LISTS --------
 *
 * each channel has a list of lists containing the current edit history and the associated sound temp files or buffers
 * undo: back up current list position
 * redo: push position foward
 * No actual changes are flushed out to the file system until the file is saved.
 *
 * the editing possibilities are insert, change, delete, scaling, zero, ramp, ptree.  All input goes through these lists.
 */

/* ed_list fields accessed only in this file */
enum {ED_SIMPLE, ED_RAMP, ED_PTREE}; /* typ field choices */
#define UNWRAP_SAMPLE(X, ED) ((X) * (ED))
#define UNWRAP_SAMPLE_TO_FLOAT(X, SF) ((X) * (SF->fscaler))

/* try to make this code easier to read... */
#define FRAGMENT_GLOBAL_POSITION(Ed, Pos)  Ed->fragments[Pos]->out
#define FRAGMENT_LOCAL_POSITION(Ed, Pos)   Ed->fragments[Pos]->beg
#define FRAGMENT_LOCAL_END(Ed, Pos)        Ed->fragments[Pos]->end
#define FRAGMENT_SCALER(Ed, Pos)           Ed->fragments[Pos]->scl
#define FRAGMENT_RAMP_BEG(Ed, Pos)         Ed->fragments[Pos]->rmp0
#define FRAGMENT_RAMP_END(Ed, Pos)         Ed->fragments[Pos]->rmp1
#define FRAGMENT_TYPE(Ed, Pos)             Ed->fragments[Pos]->typ
#define FRAGMENT_SOUND(Ed, Pos)            Ed->fragments[Pos]->snd
#define FRAGMENT_PTREE(Ed, Pos)            Ed->fragments[Pos]->loc
#define FRAGMENT_LENGTH(Ed, Pos)           (FRAGMENT_LOCAL_END(Ed, Pos) - FRAGMENT_LOCAL_POSITION(Ed, Pos) + 1)

#define EDIT_LIST_END_MARK -2
#define EDIT_LIST_ZERO_MARK -1

/* ed->snd is either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END|ZERO_MARK */
/* ed->beg and ed->end are indices into the associated data => start/end point of data used in current segment */
/* ed->out is running segment location within current overall edited data */
/* ed->scl is the segment scaler */
/* ed->rmp0 is first val of ramp, ed->rmp1 is end val */
/* ed->typ has code for accessor choice */
/* ed->loc is an index into the cp->ptrees array */
/* EDIT_ALLOC_SIZE is the allocation amount (pointers) each time cp->sounds is (re)allocated */

enum {INSERTION_EDIT, DELETION_EDIT, CHANGE_EDIT, INITIALIZE_EDIT, SCALED_EDIT, ZERO_EDIT, RAMP_EDIT, PTREE_EDIT};
static char *edit_names[9] = {"insert", "delete", "set", "init", "scale", "zero", "env", "ptree", ""};

static void display_ed_list(chan_info *cp, FILE *outp, int i, ed_list *ed)
{
  int len, j, type, index;
  snd_data *sd;
  len = ed->size; /* number of fragments in this list */
  type = ed->edit_type;
  switch (type)
    {
    case INSERTION_EDIT:  fprintf(outp, "\n (insert " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);   break;
    case DELETION_EDIT:   fprintf(outp, "\n (delete " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);   break;
    case CHANGE_EDIT:     fprintf(outp, "\n (set " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);      break;
    case SCALED_EDIT:     fprintf(outp, "\n (scale " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);    break;
    case ZERO_EDIT:       fprintf(outp, "\n (silence " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);  break;
    case RAMP_EDIT:       fprintf(outp, "\n (ramp " OFF_TD " " OFF_TD ") ", ed->beg, ed->len);     break;
    case PTREE_EDIT:      
      fprintf(outp, "\n (ptree[%d] " OFF_TD " " OFF_TD ") ", 
	      ed->ptree_location, ed->beg, ed->len);
      break; 
    case INITIALIZE_EDIT: fprintf(outp, "\n (begin) ");                                            break;
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
	  if (FRAGMENT_TYPE(ed, j) == ED_RAMP)
	    fprintf(outp, ", %f -> %f", 
		    FRAGMENT_RAMP_BEG(ed, j),
		    FRAGMENT_RAMP_END(ed, j));
	  if (FRAGMENT_TYPE(ed, j) == ED_PTREE)
	    {
	      XEN code;
	      fprintf(outp, ", arg: %f",
		      (float)(MUS_FLOAT_TO_FIX * FRAGMENT_RAMP_BEG(ed, j)));
	      code = ptree_code(cp->ptrees[FRAGMENT_PTREE(ed, j)]);
	      if (XEN_LIST_P(code))
		fprintf(outp, ", code: %s", XEN_AS_STRING(code));
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
		    fprintf(outp, " [buf: " OFF_TD "] ", sd->len / 4);
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
  mus_snprintf(edbuf, PRINT_BUFFER_SIZE, "%s : (%s " OFF_TD " " OFF_TD ")", 
	       ed->origin, 
	       edit_names[ed->edit_type], 
	       ed->beg, ed->len);
  return(edbuf);
}

static void display_edits(chan_info *cp, FILE *outp)
{
  int eds, i;
  ed_list *ed;
  eds = cp->edit_ctr;
  fprintf(outp, "\nEDITS: %d\n", eds);
  for (i = 0; i <= eds; i++)
    {
      ed = cp->edits[i];
      if (!ed) 
	fprintf(outp, "\nedit_ctr is %d, but [%d] is nil!", eds, i);
      else display_ed_list(cp, outp, i, ed);
    }
}

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
	  if ((ed->len > 16) && (save_dir(ss)))
	    {
	      newname = shorter_tempnam(save_dir(ss), "snd_");
	      mus_array_to_file(newname, sd->buffered_data, ed->len, 22050, 1);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      fprintf(fd, "#(");
	      for (i = 0; i < ed->len; i++) 
#if SNDLIB_USE_FLOATS
		fprintf(fd, "%f ", sd->buffered_data[i]);
#else
	        fprintf(fd, "%d ", sd->buffered_data[i]);
#endif
	      fprintf(fd, ")");
	    }
	}
      else
	{
	  if (save_dir(ss))
	    {
	      newname = shorter_tempnam(save_dir(ss), "snd_");
	      copy_file(sd->filename, newname);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      /* read at very low level and write to (text) history files as sample list */
	      int ifd, bufnum, cursamples, sample;
	      off_t idataloc, n, samples;
	      mus_sample_t *buffer;
	      mus_sample_t **ibufs;
	      fprintf(fd, "#(");
	      ifd = mus_file_open_read(sd->filename);
	      if (ifd == -1) 
		{
		  snd_error("can't open %s: %s! [%s[%d] %s]",
			    sd->filename,
			    strerror(errno),
			    __FILE__, __LINE__, __FUNCTION__); 
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
		      fprintf(fd, "%f ", MUS_SAMPLE_TO_FLOAT(buffer[i]));
#else
		      fprintf(fd, "%d ", MUS_SAMPLE_TO_INT(buffer[i]));
#endif
		      sample++;
		      if (sample == ed->len) goto ALL_DONE;
		    }
		}
	    ALL_DONE:
	      mus_file_close(ifd);
	      fprintf(fd, ")");
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
	  switch (ed->edit_type)
	    {
	    case INSERTION_EDIT: 
	      /* samp data snd chn */
	      fprintf(fd, "      (%s " OFF_TD " " OFF_TD " \"%s\" ",
		      S_insert_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, " sfile %d)\n", cp->chan);
	      break;
	    case DELETION_EDIT:
	      /* samp samps snd chn */
	      fprintf(fd, "      (%s " OFF_TD " " OFF_TD " \"%s\" sfile %d)\n",
		      S_delete_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "",
		      cp->chan);
	      break;
	    case CHANGE_EDIT:
	      fprintf(fd, "      (%s " OFF_TD " " OFF_TD " \"%s\" ",
		      S_change_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, " sfile %d)\n", cp->chan);
	      break;
	    case SCALED_EDIT: 
	      fprintf(fd, "      (%s sfile %d)\n",
		      ed->origin, /* imports scaler */
		      cp->chan);
	      break;
	    case ZERO_EDIT:
	      fprintf(fd, "      (%s " OFF_TD " " OFF_TD " sfile %d)\n",
		      S_pad_channel,
		      ed->beg,
		      ed->len,
		      cp->chan);
	      break;
	    case RAMP_EDIT:
	      fprintf(fd, "      (%s sfile %d)\n",
		      ed->origin,
		      cp->chan);
	      break;
	    case PTREE_EDIT:
	      fprintf(fd, "      (ptree-channel %s " OFF_TD " " OFF_TD " sfile %d)\n",
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
	}
    }
  if (cp->edit_ctr < edits) 
    fprintf(fd, "      (undo %d sfile %d)\n",
	    edits - cp->edit_ctr,
	    cp->chan);
  save_mark_list(fd, cp);
}

static void ripple_out(ed_fragment **list, int beg, off_t num, int len)
{
  int i;
  for (i = beg; i < len; i++) list[i]->out += num;
}

#if 0
static void copy_ed_fragment(ed_fragment *new_ed, ed_fragment *old_ed)
{
  new_ed->out = old_ed->out;
  new_ed->beg = old_ed->beg;
  new_ed->end = old_ed->end;
  new_ed->scl = old_ed->scl;
  new_ed->rmp0 = old_ed->rmp0;
  new_ed->rmp1 = old_ed->rmp1;
  new_ed->snd = old_ed->snd;
  new_ed->typ = old_ed->typ;
  new_ed->loc = old_ed->loc;
}
#else
#define copy_ed_fragment(New_Ed, Old_Ed) memcpy((void *)(New_Ed), (void *)(Old_Ed), sizeof(ed_fragment))
#endif

static void copy_ed_blocks(ed_fragment **new_list, ed_fragment **old_list, int new_beg, int old_beg, int num_lists)
{
  /* beg and num_lists here are in terms of blocks, not ints */
  int i, end, k;
  if (num_lists > 0)
    {
      end = old_beg + num_lists;
      for (k = new_beg, i = old_beg; i < end; i++, k++) 
	copy_ed_fragment(new_list[k], old_list[i]);
    }
}

static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  int i;
  ed=(ed_list *)CALLOC(1, sizeof(ed_list));
  ed->size = size;
  ed->allocated_size = size;
  ed->fragments = (ed_fragment **)CALLOC(size, sizeof(ed_fragment *));
  for (i = 0; i < size; i++)
    ed->fragments[i] = (ed_fragment *)CALLOC(1, sizeof(ed_fragment));
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

static ed_list *append_ed_list(chan_info *cp, int size)
{
  /* edit_ctr is already incremented */
  ed_list *new_ed, *old_ed;
  new_ed = make_ed_list(size);
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      old_ed = cp->edits[cp->edit_ctr - 1];
      new_ed->selection_beg = old_ed->selection_beg;
      new_ed->selection_end = old_ed->selection_end;
    }
  return(new_ed);
}

static ed_list *free_ed_list(ed_list *ed)
{
  int i;
  if (ed)
    {
      if (ed->fragments) 
	{
	  for (i = 0; i < ed->allocated_size; i++)
	    if (ed->fragments[i]) 
	      FREE(ed->fragments[i]);
	  FREE(ed->fragments);
	}
      if (ed->origin) FREE(ed->origin);
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
  free_ed_list(cp->edits[cur - 1]);
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
  /* marks backup added 23-Jun-00 */
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
	      if (cp->edits[i]) free_ed_list(cp->edits[i]);
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

static ed_list *initial_ed_list(off_t beg, off_t end)
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
  FRAGMENT_SOUND(ed, 0) = 0;
  FRAGMENT_GLOBAL_POSITION(ed, 0) = 0;
  FRAGMENT_SCALER(ed, 0) = 1.0;
  FRAGMENT_RAMP_BEG(ed, 0) = 0.0;
  FRAGMENT_RAMP_END(ed, 0) = 0.0;
  FRAGMENT_TYPE(ed, 0) = ED_SIMPLE;
  /* second block is our end-of-tree marker */
  FRAGMENT_LOCAL_POSITION(ed, 1) = 0;
  FRAGMENT_LOCAL_END(ed, 1) = 0;
  FRAGMENT_SOUND(ed, 1) = EDIT_LIST_END_MARK;
  FRAGMENT_GLOBAL_POSITION(ed, 1) = end + 1;
  return(ed);
}

void allocate_ed_list(chan_info *cp) 
{
  cp->edits = (ed_list **)CALLOC(cp->edit_size, sizeof(ed_list *));
}

void set_initial_ed_list(chan_info *cp, off_t len) 
{
  cp->edits[0] = initial_ed_list(0, len);
}

static int find_split_loc(off_t samp, ed_list *current_state)
{
  int i;
  for (i = 0; i < current_state->size; i++)
    if (FRAGMENT_GLOBAL_POSITION(current_state, i) >= samp) 
      return(i);
  return(0); /* make sgi compiler happy */
}

static ed_list *selected_ed_list(off_t beg, off_t end, ed_list *current_state)
{
  int bk, ek, new_size, k, oldk;
  off_t len, diff;
  ed_list *new_ed;

  /* this refers to selected data, so beg and end must be within the current data bounds! */
  if (beg < 0) beg = 0;
  if (end < 0) end = 0;
  len = FRAGMENT_GLOBAL_POSITION(current_state, current_state->size - 1);
  if (beg >= len) beg = len - 1;
  if (end >= len) end = len - 1;
  bk = find_split_loc(beg, current_state);
  if (FRAGMENT_GLOBAL_POSITION(current_state, bk) > beg) bk--;
  ek = find_split_loc(end + 1, current_state) - 1; /* was end 11-Nov-00 */
  new_size = current_state->size;
  /* if (ek < 0) ek = new_size - 1; */
  if (ek < 0) ek = 0;
  if (FRAGMENT_GLOBAL_POSITION(current_state, bk) != beg) new_size++;
  if (FRAGMENT_GLOBAL_POSITION(current_state, (ek + 1)) != end + 1) new_size++;
  new_ed = make_ed_list(new_size);
  new_ed->size = new_size;
  for (k = 0; k <= bk; k++)
    copy_ed_fragment(new_ed->fragments[k], current_state->fragments[k]);
  k = bk + 1;
  diff = beg - FRAGMENT_GLOBAL_POSITION(current_state, bk);
  if (diff != 0)
    {
      copy_ed_fragment(new_ed->fragments[k], current_state->fragments[bk]);
      FRAGMENT_LOCAL_END(new_ed, bk) = FRAGMENT_LOCAL_POSITION(new_ed, bk) + diff - 1;
      FRAGMENT_GLOBAL_POSITION(new_ed, k) = beg;
      FRAGMENT_LOCAL_POSITION(new_ed, k) = FRAGMENT_LOCAL_END(new_ed, bk) + 1;
      k++;
    }
  for (oldk = bk + 1; oldk <= ek; oldk++, k++)
      copy_ed_fragment(new_ed->fragments[k], current_state->fragments[oldk]);
  diff = FRAGMENT_GLOBAL_POSITION(current_state, (ek + 1)) - end - 1;
  if (diff != 0)
    {
      copy_ed_fragment(new_ed->fragments[k], current_state->fragments[ek]);
      FRAGMENT_LOCAL_END(new_ed, (k - 1)) -= diff;
      FRAGMENT_LOCAL_POSITION(new_ed, k) = FRAGMENT_LOCAL_END(new_ed, (k - 1)) + 1;
      FRAGMENT_GLOBAL_POSITION(new_ed, k) = end + 1; /* 1? */
      k++;
    }
  for (oldk = ek + 1; (oldk <= current_state->size) && (k < new_size); oldk++, k++)
    copy_ed_fragment(new_ed->fragments[k], current_state->fragments[oldk]);
  return(new_ed);
}


static void fixup_edlist_endmark(ed_list *new_state)
{
  int k;
  if (FRAGMENT_SOUND(new_state, (new_state->size - 1)) != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size - 1; k > 0; k--)
	if (FRAGMENT_SOUND(new_state, k) == EDIT_LIST_END_MARK) break;
      if (k > 0) 
	new_state->size = k + 1;
    }
}

static void new_leading_ramp(ed_fragment *new_start, ed_fragment *old_start, off_t samp)
{
  Float rmp1, rmp0;
  Float val;
  rmp0 = old_start->rmp0;
  rmp1 = old_start->rmp1;
  val = rmp0 + (rmp1 - rmp0) * (double)(samp - 1 - old_start->out) / (double)(old_start->end - old_start->beg);
  new_start->typ = ED_RAMP;
  new_start->rmp0 = old_start->rmp0;
  new_start->rmp1 = val;
}

static void new_trailing_ramp(ed_fragment *new_back, ed_fragment *old_back, off_t samp)
{
  Float rmp1, rmp0;
  Float val;
  rmp0 = old_back->rmp0;
  rmp1 = old_back->rmp1;
  val = rmp0 + (rmp1 - rmp0) * (double)(samp - old_back->out) / (double)(old_back->end - old_back->beg);
  new_back->typ = ED_RAMP;
  new_back->rmp0 = val;
  new_back->rmp1 = old_back->rmp1;
}

static void new_trailing_fragment(ed_fragment *new_back, ed_fragment *old_back, off_t new_loc, off_t new_beg)
{
  new_back->out = new_loc;
  new_back->snd = old_back->snd;
  new_back->beg = new_beg;
  new_back->end = old_back->end;
  new_back->scl = old_back->scl;
  if (old_back->typ == ED_RAMP)
    new_trailing_ramp(new_back, old_back, new_beg);
}

static void new_leading_fragment(ed_fragment *new_front, ed_fragment *old_front, off_t beg)
{
  new_front->out = old_front->out;
  new_front->snd = old_front->snd;
  new_front->beg = old_front->beg;
  new_front->end = old_front->beg + beg - old_front->out - 1;
  new_front->scl = old_front->scl;
  if (old_front->typ == ED_RAMP)
    new_leading_ramp(new_front, old_front, beg);
}

static ed_list *insert_samples_1 (off_t samp, off_t num, ed_list *current_state, chan_info *cp, ed_fragment **cb_rtn, const char *origin, Float scaler)
{
  int len, k;
  ed_list *new_state;
  ed_fragment *cb_new, *cb_old, *cb_old_0, *cb_new_1, *cb_new_0;
  ed_fragment **ed;
  if (num <= 0) return(NULL);
  len = current_state->size;
  ed = current_state->fragments;
  k = find_split_loc(samp, current_state);
  cb_old = ed[k];
  if ((k == 0) && (cb_old->end == -1))
    {
      /* no data: just set insertion */
      new_state = append_ed_list(cp, len);
      copy_ed_blocks(new_state->fragments, ed, 0, 0, len);
      cb_new = new_state->fragments[0];
      cb_new->end = num - 1;
    }
  else
    {
      if ((samp == cb_old->out) || (samp == (cb_old->out - 1)))
	{
	  new_state = append_ed_list(cp, len + 1);
	  copy_ed_blocks(new_state->fragments, ed, 0, 0, k);
	  copy_ed_blocks(new_state->fragments, ed, k + 1, k, len - k);
	  len++;
	}
      else
	{
	  off_t new_beg_1;
	  cb_old_0 = ed[k - 1];
	  new_beg_1 = cb_old_0->beg + samp - cb_old_0->out;
	  new_state = append_ed_list(cp, len + 2);
	  copy_ed_blocks(new_state->fragments, ed, 0, 0, k);
	  copy_ed_blocks(new_state->fragments, ed, k + 2, k, len - k);
	  cb_new_0 = new_state->fragments[k - 1];
	  cb_new_0->end = new_beg_1 - 1;
	  cb_new_1 = new_state->fragments[k + 1];
	  cb_new_1->snd = cb_old_0->snd;
	  cb_new_1->out = samp;
	  cb_new_1->beg = new_beg_1;
	  cb_new_1->end = cb_old_0->end;
	  cb_new_1->scl = cb_old_0->scl;
	  if (cb_old_0->typ == ED_RAMP)
	    {
	      new_leading_ramp(cb_new_0, cb_old_0, samp);
	      new_trailing_ramp(cb_new_1, cb_old_0, samp);
	    }
	  len += 2;
	}
      cb_new = new_state->fragments[k];
      cb_new->beg = 0;
      cb_new->end = num - 1;
      cb_new->out = samp;
      cb_new->typ = ED_SIMPLE;
    }
  cb_new->scl = scaler;
  (*cb_rtn) = cb_new;
  new_state->beg = samp;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  ripple_out(new_state->fragments, k + 1, num, len);
  ripple_marks(cp, samp, num);
  ripple_mixes(cp, samp, num);
  ripple_selection(new_state, samp, num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state);
  return(new_state);
}

void extend_with_zeros(chan_info *cp, off_t beg, off_t num, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  if (num <= 0) return;
  len = cp->samples[edpos];
  prepare_edit_list(cp, len + num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, cp->edits[edpos], cp, &cb, origin, 0.0);
  cb->snd = -1;
  cp->edits[cp->edit_ctr]->edit_type = ZERO_EDIT;
  cp->edits[cp->edit_ctr]->sound_location = 0;
  reflect_edit_history_change(cp);
  check_for_first_edit(cp); /* needed to activate revert menu option */
}

void file_insert_samples(off_t beg, off_t num, char *inserted_file, chan_info *cp, int chan, int auto_delete, const char *origin, int edpos)
{
  off_t len;
  ed_fragment *cb;
  int fd;
  snd_io *io;
  ed_list *ed;
  file_info *hdr;
  snd_state *ss;
  if (num <= 0) /* can't happen!? */
    {
      if ((inserted_file) && (auto_delete == DELETE_ME)) snd_remove(inserted_file, TRUE);
      if ((inserted_file) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(inserted_file, chan);
      return;
    }
  if (dont_edit(cp)) return;
  len = cp->samples[edpos];
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len + 1, "(insert-extend)", edpos);
      edpos = cp->edit_ctr;
      len = current_ed_samples(cp);
    }
  ss = cp->state;
  prepare_edit_list(cp, len + num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, cp->edits[edpos], cp, &cb, origin, 1.0);
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
      io = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      cb->snd = add_sound_file_to_edit_list(cp, inserted_file, io, hdr, auto_delete, chan);
      ed = cp->edits[cp->edit_ctr];
      ed->edit_type = INSERTION_EDIT;
      ed->sound_location = cb->snd;
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
  ed_list *ed;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = cp->samples[edpos];
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len + 1, "(insert-extend)", edpos);
      edpos = cp->edit_ctr;
      len = current_ed_samples(cp);
    }
  prepare_edit_list(cp, len + num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, cp->edits[edpos], cp, &cb, origin, 1.0);
  cb->snd = add_sound_buffer_to_edit_list(cp, vals, num); 
  ed = cp->edits[cp->edit_ctr];
  ed->edit_type = INSERTION_EDIT;
  ed->sound_location = cb->snd;
  reflect_edit_history_change(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
  after_edit(cp);
}

static ed_list *delete_samples_1(off_t beg, off_t num, ed_list *current_state, chan_info *cp, const char *origin)
{
  int len, k, cbi, start_del, len_fixup;
  ed_fragment *cb_old_0, *cb_old_1;
  off_t curbeg, need_to_delete;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  len = current_state->size;
  len_fixup = -1;
  k = find_split_loc(beg, current_state);
  need_to_delete = num;
  start_del = k;
  curbeg = beg;
  cb_old_0 = current_state->fragments[k];
  if (cb_old_0->out > beg) start_del--;
  new_state = append_ed_list(cp, len + 1);
  copy_ed_blocks(new_state->fragments, current_state->fragments, 0, 0, start_del);
  cbi = start_del;
  cb_old_0 = current_state->fragments[start_del];
  if (beg > cb_old_0->out)
    {
      new_leading_fragment(new_state->fragments[start_del], cb_old_0, beg);
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
      off_t old_out;
      old_out = FRAGMENT_GLOBAL_POSITION(current_state, (cbi + 1));
      need_to_delete -= (old_out - curbeg);
      if (need_to_delete > 0)
	{
	  cbi++;
	  curbeg = old_out;
	}
    }
  if (need_to_delete < 0)
    {
      cb_old_1 = current_state->fragments[cbi];
      new_trailing_fragment(new_state->fragments[start_del],
			    cb_old_1, 
			    beg, 
			    cb_old_1->end + 1 + need_to_delete);
      start_del++;
      len_fixup++;
    }
  cbi++;
  copy_ed_blocks(new_state->fragments, current_state->fragments, start_del, cbi, len - cbi); /* ??? */
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  new_state->edit_type = DELETION_EDIT;
  new_state->sound_location = 0;
  ripple_out(new_state->fragments, start_del, -num, len + len_fixup);
  ripple_marks(cp, beg, -num);
  ripple_mixes(cp, beg, -num);
  ripple_selection(new_state, beg, -num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  new_state->size = len + len_fixup; /* don't propagate useless trailing blocks */
  if (FRAGMENT_SOUND(new_state, (new_state->size - 1)) != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size - 1; k > 0; k--)
	if (FRAGMENT_SOUND(new_state, k) == EDIT_LIST_END_MARK) break;
      if (k > 0) 
	new_state->size = k + 1; 
    }
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
      prepare_edit_list(cp, len - num);
      cp->edits[cp->edit_ctr] = delete_samples_1(beg, num, cp->edits[edpos], cp, origin);
      reflect_edit_history_change(cp);
      lock_affected_mixes(cp, beg, beg + num);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
      after_edit(cp);
    }
  else
    {
      if (num == 1)
	report_in_minibuffer_and_save(cp->sound, 
				      "can't delete sample " OFF_TD " (current len=" OFF_TD ")", 
				      beg, len);
      else report_in_minibuffer_and_save(cp->sound, 
					 "can't delete samples " OFF_TD " to " OFF_TD " (current len=" OFF_TD")", 
					 beg, beg + num - 1, len);
    }
}

static ed_list *change_samples_1(off_t beg, off_t num, ed_list *current_state, chan_info *cp, ed_fragment **cb_rtn, off_t lengthen, const char *origin)
{
  int len, k, start_del, cbi, len_fixup;
  off_t curbeg, need_to_delete;
  ed_list *new_state;
  ed_fragment *cb_old_0, *cb_new, *cb_old_1;
  if (num <= 0) return(NULL);
  len = current_state->size;
  len_fixup = -1;
  k = find_split_loc(beg, current_state);
  need_to_delete = num - lengthen;
  start_del = k;
  curbeg = beg;
  cbi = 0;
  cb_old_0 = current_state->fragments[k];
  if (cb_old_0->out > beg) start_del--;
  new_state = append_ed_list(cp, len + 2);
  copy_ed_blocks(new_state->fragments, current_state->fragments, 0, 0, start_del);
  cbi = start_del;
  cb_old_0 = current_state->fragments[start_del];
  if (beg > cb_old_0->out)
    {
      new_leading_fragment(new_state->fragments[start_del], cb_old_0, beg);
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
      off_t old_out;
      old_out = FRAGMENT_GLOBAL_POSITION(current_state, (cbi + 1));
      need_to_delete -= (old_out - curbeg);
      if (need_to_delete > 0)
	{
	  cbi++;
	  curbeg = old_out;
	}
    }
  cb_new = new_state->fragments[start_del];
  (*cb_rtn) = cb_new;
  cb_new->out = beg;
  cb_new->beg = 0;
  cb_new->end = num - 1;
  cb_new->scl = 1.0;
  cb_new->rmp0 = 0.0;
  cb_new->rmp1 = 0.0;
  cb_new->typ = ED_SIMPLE;
  start_del++;
  len_fixup++;
  if (need_to_delete < 0)
    {
      cb_old_1 = current_state->fragments[cbi];
      new_trailing_fragment(new_state->fragments[start_del],
			    cb_old_1, 
			    beg + num, 
			    cb_old_1->end + 1 + need_to_delete);
      start_del++;
      len_fixup++;
    }
  cbi++;
  copy_ed_blocks(new_state->fragments, current_state->fragments, start_del, cbi, len - cbi);
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  if (lengthen)
    {
      ripple_out(new_state->fragments, k + 1, lengthen, len + len_fixup);
      reflect_sample_change_in_axis(cp);
    }
  new_state->size = len + len_fixup; /* don't propagate useless trailing blocks */
  ripple_marks(cp, 0, 0);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state);
  return(new_state);
}    

void file_change_samples(off_t beg, off_t num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, const char *origin, int edpos)
{
  off_t prev_len, new_len;
  ed_fragment *cb;
  int fd;
  snd_io *io;
  ed_list *ed;
  file_info *hdr;
  snd_state *ss;
  if (num <= 0) /* not sure this can happen */
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) snd_remove(tempfile, TRUE);
      if ((tempfile) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(tempfile, chan);
      return;
    }
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
	  prev_len = current_ed_samples(cp);
	}
      new_len = beg + num;
      if (new_len < prev_len) new_len = prev_len;
      prepare_edit_list(cp, new_len);
      cp->edits[cp->edit_ctr] = change_samples_1(beg, num, cp->edits[edpos], cp, &cb, new_len - prev_len, origin);
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
      io = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      cb->snd = add_sound_file_to_edit_list(cp, tempfile, io, hdr, auto_delete, chan);
      ed = cp->edits[cp->edit_ctr];
      ed->edit_type = CHANGE_EDIT;
      ed->sound_location = cb->snd;
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

void file_override_samples(off_t num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, const char *origin)
{
  int fd;
  ed_list *e;
  snd_io *io;
  file_info *hdr;
  snd_state *ss;
  if (num == 0) /* not sure this can happen */
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) snd_remove(tempfile, TRUE);
      if ((tempfile) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(tempfile, chan);
      return;
    }
  if (dont_edit(cp)) return;
  ss = cp->state;
  ss->catch_message = NULL;
  hdr = make_file_info(tempfile, ss);
  if (hdr) 
    {
      if (num == -1) num = (hdr->samples / hdr->chans);
      prepare_edit_list(cp, num);
      fd = snd_open_read(ss, tempfile);
      mus_file_open_descriptors(fd,
				tempfile,
				hdr->format,
				mus_data_format_to_bytes_per_sample(hdr->format),
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_OVERRIDE_FILE);
      io = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      e = initial_ed_list(0, num - 1);
      if (origin) e->origin = copy_string(origin);
      cp->edits[cp->edit_ctr] = e;
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, 0, num);
      e->fragments[0]->snd = add_sound_file_to_edit_list(cp, tempfile, io, hdr, auto_delete, chan);
      e->edit_type = CHANGE_EDIT;
      e->sound_location = FRAGMENT_SOUND(e, 0);
      reflect_edit_history_change(cp);
      reflect_sample_change_in_axis(cp);
      ripple_marks(cp, 0, 0);
      check_for_first_edit(cp);
      update_graph(cp);
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
      prev_len = current_ed_samples(cp);
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  prepare_edit_list(cp, new_len);
  ed = change_samples_1(beg, num, cp->edits[edpos], cp, &cb, new_len - prev_len, origin);
  cp->edits[cp->edit_ctr] = ed;
  cb->snd = add_sound_buffer_to_edit_list(cp, vals, num); 
  ed->edit_type = CHANGE_EDIT;
  ed->sound_location = cb->snd;
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
  after_edit(cp);
}

int ramp_or_ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos)
{
  /* used only in snd-sig.c 1929 */
  ed_list *ed;
  int i, typ;
  off_t end, loc;
  ed = cp->edits[pos];
  end = beg + dur;
  loc = FRAGMENT_GLOBAL_POSITION(ed, 0);
  for (i = 0; i < ed->size - 1; i++) 
    {
      if ((loc > end) || (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK))
	return(FALSE);
      typ = FRAGMENT_TYPE(ed, i);
      if ((typ != ED_SIMPLE) && (loc >= beg))                 /* loc not beyond end, but is beyond beg, so this ramp falls into our segment */
	return(TRUE);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);            /* i.e. next loc = current fragment end point */
      if ((typ != ED_SIMPLE) && (loc >= beg) && (loc <= end)) /* current ramp fragment ends in segment */
	return(TRUE);
    }
  return(FALSE);
}

void scale_channel(chan_info *cp, Float scl, off_t beg, off_t num, int pos)
{
  /* copy current ed-list and reset scalers */
  off_t len;
  int i;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg > cp->samples[pos]) ||
      (scl == 1.0))
    return; 
  len = cp->samples[pos];
  old_ed = cp->edits[pos];
  prepare_edit_list(cp, len);
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  copy_ed_fragment(new_ed->fragments[i], old_ed->fragments[i]);
	  FRAGMENT_SCALER(new_ed, i) *= scl;
	}
      amp_env_scale_by(cp, scl, pos); 
    }
  else 
    {
      new_ed = selected_ed_list(beg, beg + num - 1, old_ed);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg) FRAGMENT_SCALER(new_ed, i) *= scl;
	}
      amp_env_scale_selection_by(cp, scl, beg, num, pos);
    }
  new_ed->edit_type = SCALED_EDIT;
  new_ed->sound_location = 0;
  new_ed->origin = mus_format("%s %.4f " OFF_TD " " OFF_TD, S_scale_channel, scl, beg, num);
  new_ed->beg = beg;
  new_ed->len = num;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_marks(cp, 0, 0); /* 0,0 -> copy marks */
  check_for_first_edit(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "scale"); /* 30-Jan-02 */
  reflect_edit_history_change(cp);
  update_graph(cp);
}

void ramp_channel(chan_info *cp, Float rmp0, Float rmp1, off_t beg, off_t num, int pos)
{
  /* copy current ed-list and set env fields */
  off_t len;
  int i;
  ed_list *new_ed, *old_ed;
  Float seg0, seg1;
  double incr;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg > cp->samples[pos]))
    return; 
  len = cp->samples[pos];
  old_ed = cp->edits[pos];
  prepare_edit_list(cp, len);
  incr = (double)(rmp1 - rmp0) / (double)num;
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      /* one ramp over entire fragment list -- no splits will occur here */
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++)
	copy_ed_fragment(new_ed->fragments[i], old_ed->fragments[i]);
      seg1 = rmp0 - incr;
      for (i = 0; i < new_ed->size - 1; i++) /* -1 here to leave end mark alone */
	{
	  seg0 = seg1 + incr;
	  seg1 = seg0 + (incr * FRAGMENT_LENGTH(new_ed, i));
	  FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	  FRAGMENT_RAMP_END(new_ed, i) = seg1;
	  FRAGMENT_TYPE(new_ed, i) = ED_RAMP;
	}
    }
  else 
    {
      new_ed = selected_ed_list(beg, beg + num - 1, old_ed);
      cp->edits[cp->edit_ctr] = new_ed;
      seg1 = rmp0 - incr;
      for (i = 0; i < new_ed->size - 1; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	    {
	      seg0 = seg1 + incr;
	      seg1 = seg0 + (incr * FRAGMENT_LENGTH(new_ed, i));
	      FRAGMENT_RAMP_BEG(new_ed, i) = seg0;
	      FRAGMENT_RAMP_END(new_ed, i) = seg1;
	      FRAGMENT_TYPE(new_ed, i) = ED_RAMP;
	    }
	}
    }
  new_ed->edit_type = RAMP_EDIT;
  new_ed->sound_location = 0;
  new_ed->origin = mus_format("%s %.4f %.4f " OFF_TD " " OFF_TD, S_ramp_channel, rmp0, rmp1, beg, num);
  new_ed->beg = beg;
  new_ed->len = num;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_marks(cp, 0, 0); /* 0,0 -> copy marks */
  check_for_first_edit(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "ramp"); /* 30-Jan-02 */
  reflect_edit_history_change(cp);
  update_graph(cp);
}

/* The ptree can be called moving in either direction, starting anywhere, without 
 *   any way to tell position, sound, chan, edpos etc. We could set up an initialization
 *   call, but even if all the rest were passed in, dir could change without warning.
 *   Also currently assumes sample->sample mapping (as opposed to src etc)
 *   and randomness is out -- need to be able to start anywhere with same result etc.
 * Next step in virtual ops (leaving aside mixing support) might require
 *   user-written edit list, or perhaps pass in sample reader rather than 
 *   current sample -- return new length+peak envs?  (But sample reader
 *   would need prescaler to keep compatibility with scale-by etc).
 */
void ptree_channel(chan_info *cp, void *ptree, off_t beg, off_t num, int pos, void *env_pt)
{
  off_t len;
  int i, ptree_loc = 0;
  ed_list *new_ed, *old_ed;
  if ((beg < 0) || 
      (num <= 0) ||
      (beg > cp->samples[pos]) ||
      (ptree == NULL))
    return; 
  len = cp->samples[pos];
  old_ed = cp->edits[pos];
  prepare_edit_list(cp, len);
  ptree_loc = add_ptree(cp);
  cp->ptrees[ptree_loc] = ptree;
  if ((beg == 0) && 
      (num >= cp->samples[pos]))
    {
      num = len;
      new_ed = make_ed_list(cp->edits[pos]->size);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  copy_ed_fragment(new_ed->fragments[i], old_ed->fragments[i]);
	  FRAGMENT_PTREE(new_ed, i) = ptree_loc;
	  FRAGMENT_TYPE(new_ed, i) = ED_PTREE;
	  FRAGMENT_RAMP_BEG(new_ed, i) = MUS_SAMPLE_TO_FLOAT(FRAGMENT_SCALER(new_ed, i));
	  FRAGMENT_SCALER(new_ed, i) = 1.0;
	}
      if (env_pt)
	amp_env_ptree(cp, env_pt, pos);
    }
  else 
    {
      new_ed = selected_ed_list(beg, beg + num - 1, old_ed);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	    {
	      FRAGMENT_PTREE(new_ed, i) = ptree_loc;
	      FRAGMENT_TYPE(new_ed, i) = ED_PTREE;
	      FRAGMENT_RAMP_BEG(new_ed, i) = MUS_SAMPLE_TO_FLOAT(FRAGMENT_SCALER(new_ed, i));
	      FRAGMENT_SCALER(new_ed, i) = 1.0;
	    }
	}
      if (env_pt)
	amp_env_ptree_selection(cp, env_pt, beg, num, pos);
    }
  new_ed->edit_type = PTREE_EDIT;
  new_ed->sound_location = 0;
  new_ed->ptree_location = ptree_loc;
  new_ed->origin = mus_format("ptree %d " OFF_TD " " OFF_TD, ptree_loc, beg, num);
  new_ed->beg = beg;
  new_ed->len = num;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_marks(cp, 0, 0); /* 0,0 -> copy marks */
  check_for_first_edit(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, "ptree");
  reflect_edit_history_change(cp);
  update_graph(cp);
}

snd_fd *free_snd_fd_almost(snd_fd *sf)
{
  snd_data *sd;
  if (sf) 
    {
      sd = sf->current_sound;
      if ((sd) && 
	  ((sd->type == SND_DATA_BUFFER) || (sd->type == SND_DATA_FILE)))
	{
	  sd->inuse = FALSE;
	  if ((sd->copy == 1) || (sd->free_me == 1))
	    sd = free_snd_data(sd); 
	}
      sf->current_state = NULL;
      sf->current_sound = NULL;
      /* FREE(sf); */
    }
  return(NULL);
}

snd_fd *free_snd_fd(snd_fd *sf)
{
  free_snd_fd_almost(sf);
  FREE(sf);
  return(NULL);
}

off_t current_location(snd_fd *sf) 
{
  /* only used by moving cursor code in snd-dac.c */
  if (sf->current_sound)
    return(sf->cb->out - sf->cb->beg + sf->current_sound->io->beg + sf->loc);
  return(sf->cb->out - sf->cb->beg + sf->loc);
}

/* -------- fragment handlers -------- */
/*
 * each fragment has two associated read funcs: run and runf
 *   run -> mus_sample_t
 *   runf -> Float
 *   the following functions are possible run/runf choices, based on the fragment type
 *     (if fragment has scaler, use next_sample else next_sample_unscaled, etc)
 * read_sample calls run, read_sample_to_float runf
 *
 * it is assumed all sample accesses will go through run/runf -- no "direct" access anymore.
 */

static mus_sample_t next_sample(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  else return((mus_sample_t)(UNWRAP_SAMPLE(sf->data[sf->loc++], sf->cb->scl)));
}

static mus_sample_t previous_sample(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else return((mus_sample_t)(UNWRAP_SAMPLE(sf->data[sf->loc--], sf->cb->scl)));
}

#if (!SNDLIB_USE_FLOATS)
static mus_sample_t next_sample_by_int(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  else return(sf->data[sf->loc++] * sf->iscaler);
}

static mus_sample_t previous_sample_by_int(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else return(sf->data[sf->loc--] * sf->iscaler);
}
#endif

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

static mus_sample_t next_sample_unscaled(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  else return(sf->data[sf->loc++]);
}

static mus_sample_t previous_sample_unscaled(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else return(sf->data[sf->loc--]);
}

static mus_sample_t next_zero_sample(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound(sf));
  sf->loc++;
  return(MUS_SAMPLE_0);
}

static mus_sample_t previous_zero_sample(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  sf->loc--;
  return(MUS_SAMPLE_0);
}

static mus_sample_t end_sample(snd_fd *sf)
{
  return(MUS_SAMPLE_0);
}

static Float end_sample_to_float(snd_fd *sf)
{
  return(0.0);
}

static Float next_zero_sample_to_float(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(MUS_SAMPLE_TO_FLOAT(next_sound(sf)));
  sf->loc++;
  return(0.0);
}

static Float previous_zero_sample_to_float(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(MUS_SAMPLE_TO_FLOAT(previous_sound(sf)));
  sf->loc--;
  return(0.0);
}

static Float next_sample_to_float(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(MUS_SAMPLE_TO_FLOAT(next_sound(sf)));
  else return((sf->data[sf->loc++]) * sf->fscaler);
}

static Float previous_sample_to_float(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(MUS_SAMPLE_TO_FLOAT(previous_sound(sf)));
  else return((sf->data[sf->loc--]) * sf->fscaler);
}

static Float next_sample_to_float_with_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(MUS_SAMPLE_TO_FLOAT(next_sound(sf)));
  else 
    {
      /* assume fscaler or iscaler has been incorporated into the incr/curval fields already */
      Float val;
      val = MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc++]) * sf->curval;
      sf->curval += sf->incr;
      return(val);
    }
}

static Float previous_sample_to_float_with_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(MUS_SAMPLE_TO_FLOAT(previous_sound(sf)));
  else
    {
      Float val;
      val = MUS_SAMPLE_TO_FLOAT(sf->data[sf->loc--]) * sf->curval;
      sf->curval -= sf->incr;
      return(val);
    }
}

static Float next_sample_to_float_unscaled(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(MUS_SAMPLE_TO_FLOAT(next_sound(sf)));
  else return(sf->data[sf->loc++]);
}

static Float previous_sample_to_float_unscaled(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(MUS_SAMPLE_TO_FLOAT(previous_sound(sf)));
  else return(sf->data[sf->loc--]);
}


static mus_sample_t next_sample_with_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound(sf));
  else return(MUS_FLOAT_TO_SAMPLE(sf->cb->scl * evaluate_ptree_1f2f(sf->ptree, sf->cb->rmp0 * sf->data[sf->loc++])));
  /* fscaler after ptree so that subsequent scaling of ptree-fragment actually scales it */
}

static mus_sample_t previous_sample_with_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else return(MUS_FLOAT_TO_SAMPLE(sf->cb->scl * evaluate_ptree_1f2f(sf->ptree, sf->cb->rmp0 * sf->data[sf->loc--])));
}

static Float next_sample_to_float_with_ptree(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(MUS_SAMPLE_TO_FLOAT(next_sound(sf)));
  else return(sf->cb->scl * evaluate_ptree_1f2f(sf->ptree, sf->cb->rmp0 * sf->data[sf->loc++]));
}

static Float previous_sample_to_float_with_ptree(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(MUS_SAMPLE_TO_FLOAT(previous_sound(sf)));
  else return(sf->cb->scl * evaluate_ptree_1f2f(sf->ptree, sf->cb->rmp0 * sf->data[sf->loc--]));
}


void read_sample_change_direction(snd_fd *sf, int dir)
{
  if (dir == READ_FORWARD)
    {
      if (sf->fscaler != 0.0)
	{
	  if (sf->run == previous_sample_with_ramp)
	    {
	      sf->run = next_sample_with_ramp;
	      sf->runf = next_sample_to_float_with_ramp;
	    }
	  else
	    {
	      if (sf->run == previous_sample_with_ptree)
		{
		  sf->run = next_sample_with_ptree;
		  sf->runf = next_sample_to_float_with_ptree;
		}
	      else
		{
		  sf->run = next_sample;
		  sf->runf = next_sample_to_float;
		}
	    }
	}
      else
	{
	  sf->run = next_zero_sample;
	  sf->runf = next_zero_sample_to_float;
	}
    }
  else
    {
      if (sf->fscaler != 0.0)
	{
	  if (sf->run == next_sample_with_ramp)
	    {
	      sf->run = previous_sample_with_ramp;
	      sf->runf = previous_sample_to_float_with_ramp;
	    }
	  else
	    {
	      if (sf->run == next_sample_with_ptree)
		{
		  sf->run = previous_sample_with_ptree;
		  sf->runf = previous_sample_to_float_with_ptree;
		}
	      else
		{
		  sf->run = previous_sample;
		  sf->runf = previous_sample_to_float;
		}
	    }
	}
      else
	{
	  sf->run = previous_zero_sample;
	  sf->runf = previous_zero_sample_to_float;
	}
    }
  sf->direction = dir;
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

void move_to_next_sample(snd_fd *sf)
{
  if (sf->loc > sf->last)
    next_sound(sf);
  else sf->loc++;
}

static mus_sample_t reader_out_of_data(snd_fd *sf)
{
  sf->at_eof = TRUE;
  sf->run = end_sample;
  sf->runf = end_sample_to_float;
  return(MUS_SAMPLE_0);
}

static snd_fd *cancel_reader(snd_fd *sf)
{
  sf->current_sound = NULL;
  sf->cbi = 0;
  reader_out_of_data(sf);
  return(sf);
}

static void choose_accessor(snd_fd *sf)
{
  Float rmp0, rmp1, scl;
  /* fragment-specific reader choice */
  if (sf->direction == READ_FORWARD)
    {
      if (sf->cb->scl == 0)
	{
	  sf->run = next_zero_sample;
	  sf->runf = next_zero_sample_to_float;
	}
      else
	{
	  switch (sf->cb->typ)
	    {
	    case ED_RAMP:
	      scl = sf->cb->scl;
	      rmp0 = scl * sf->cb->rmp0;
	      rmp1 = scl * sf->cb->rmp1;
	      sf->incr = (double)(rmp1 - rmp0) / (double)(sf->cb->end - sf->cb->beg);
	      sf->curval = rmp0 + sf->incr * sf->frag_pos;
	      sf->run = next_sample_with_ramp;
	      sf->runf = next_sample_to_float_with_ramp;
	      break;
	    case ED_PTREE:
	      sf->run = next_sample_with_ptree;
	      sf->runf = next_sample_to_float_with_ptree;
	      sf->ptree = sf->cp->ptrees[sf->cb->loc];
	      break;
	    default:
	      if (sf->cb->scl == 1.0)
		{
		  sf->run = next_sample_unscaled;
		  if (sf->fscaler == 1.0) 
		    sf->runf = next_sample_to_float_unscaled;
		  else sf->runf = next_sample_to_float;
		}
	      else
		{
#if (!SNDLIB_USE_FLOATS)
		  /* more concerned here with the int->float->int conversions than the multiply */
		  scl = sf->cb->scl;
		  if (scl == floor(scl))
		    {
		      sf->iscaler = (int)floor(scl);
		      sf->run = next_sample_by_int;
		    }
		  else
		    {
		      sf->run = next_sample;
		    }
		  sf->runf = next_sample_to_float;
#else
		  sf->run = next_sample;
		  sf->runf = next_sample_to_float;
#endif
		}
	      break;
	    }
	}
    }
  else
    {
      if (sf->cb->scl == 0)
	{
	  sf->run = previous_zero_sample;
	  sf->runf = previous_zero_sample_to_float;
	}
      else
	{
	  switch (sf->cb->typ)
	    {
	    case ED_RAMP:
	      scl = sf->cb->scl;
	      rmp0 = scl * sf->cb->rmp0;
	      rmp1 = scl * sf->cb->rmp1;
	      sf->incr = (double)(rmp1 - rmp0) / (double)(sf->cb->end - sf->cb->beg);
	      sf->curval = rmp0 + sf->incr * sf->frag_pos;
	      sf->run = previous_sample_with_ramp;
	      sf->runf = previous_sample_to_float_with_ramp;
	      break;
	    case ED_PTREE:
	      sf->run = previous_sample_with_ptree;
	      sf->runf = previous_sample_to_float_with_ptree;
	      sf->ptree = sf->cp->ptrees[sf->cb->loc];
	      break;
	    default:
	      if (sf->cb->scl == 1.0)
		{
		  sf->run = previous_sample_unscaled;
		  if (sf->fscaler == 1.0) 
		    sf->runf = previous_sample_to_float_unscaled;
		  else sf->runf = previous_sample_to_float;
		}
	      else
		{
#if (!SNDLIB_USE_FLOATS)
		  scl = sf->cb->scl;
		  if (scl == floor(scl))
		    {
		      sf->iscaler = (int)floor(scl);
		      sf->run = previous_sample_by_int;
		    }
		  else
		    {
		      sf->run = previous_sample;
		    }
		  sf->runf = previous_sample_to_float;
#else
		  sf->run = previous_sample;
		  sf->runf = previous_sample_to_float;
#endif
		}
	      break;
	    }
	}
    }
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
  if (cp->active == 0) return(NULL);
  if ((edit_position < 0) || (edit_position > cp->edit_size)) return(NULL);
  if ((samp == 0) && (direction == READ_BACKWARD))
    {
      snd_warning("reading backward from sample 0?");
      direction = READ_FORWARD;
    }
  ed = (ed_list *)(cp->edits[edit_position]);
  if (!ed) return(NULL);
  sp = cp->sound;
  if (sp->need_update) 
    {
      if (mus_file_probe(sp->filename) == 0)
	snd_error("%s no longer exists!", sp->short_filename);
      else snd_warning("%s has changed since we last read it!", sp->short_filename);
    }
  curlen = cp->samples[edit_position];
  /* snd_fd allocated only here */
  sf = (snd_fd *)CALLOC(1, sizeof(snd_fd));
  sf->initial_samp = samp;
  sf->cp = cp;
  sf->fscaler = MUS_SAMPLE_TO_FLOAT(1.0);
  sf->direction = direction;

  /*
   * TODO: fragment change func (with possibility of callback fragment-hook)
   * TODO: mix reader (i.e. two readers running in parallel)
   */

  sf->current_state = ed;
  if ((curlen <= 0) ||    /* no samples, not ed->len (delete->len = #deleted samps) */
      (samp < 0) ||       /* this should never happen */
      ((samp >= curlen) && (direction == READ_FORWARD)))
    return(cancel_reader(sf));
  if (samp >= curlen) samp = curlen - 1;
  len = ed->size;
  sf->sounds = (snd_data **)(cp->sounds);
  for (i = 0; i < len; i++)
    {
      cb = ed->fragments[i];
      if ((cb->out > samp) || 
	  (cb->snd == EDIT_LIST_END_MARK))             /* i.e. we went one too far */
	{
	  sf->cb = ed->fragments[i - 1];  /* so back up one */
	  sf->cbi = i - 1;
	  sf->frag_pos = samp - sf->cb->out;
	  ind0 = sf->cb->beg;
	  indx = sf->cb->beg + sf->frag_pos;
	  ind1 = sf->cb->end;
	  sf->fscaler = MUS_SAMPLE_TO_FLOAT(sf->cb->scl);
	  if (sf->cb->scl == 0)
	    {
	      sf->current_sound = NULL;
	      sf->loc = indx;
	      sf->first = ind0;
	      sf->last = ind1;
	      choose_accessor(sf);
	      return(sf);
	    }
	  first_snd = sf->sounds[sf->cb->snd];
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
	      first_snd->inuse = TRUE;
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
  if ((cp->active == 0) || (samp < 0) || (pos < 0) || (pos > cp->edit_size) || (samp >= cp->samples[pos])) return(0.0);
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

mus_sample_t previous_sound (snd_fd *sf) 
{
  off_t ind0, ind1, indx;
  snd_data *prev_snd;
  int at_start;
  at_start = ((sf->cb == NULL) || (sf->current_sound == NULL) || (sf->cb->beg >= sf->current_sound->io->beg));
  if (at_start)
    {
      if (sf->current_sound) 
	{
	  prev_snd = sf->current_sound; 
	  prev_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if (prev_snd->copy) prev_snd = free_snd_data(prev_snd);
	}
      if (sf->cbi == 0) return(reader_out_of_data(sf));
      sf->cbi--;
      /* now start in the final portion of this block (if a file) */
      sf->cb = sf->current_state->fragments[sf->cbi];
      ind0 = sf->cb->beg;
      ind1 = sf->cb->end;
      sf->fscaler = MUS_SAMPLE_TO_FLOAT(sf->cb->scl);
      if (sf->cb->scl == 0)
	{
	  sf->current_sound = NULL;
	  sf->loc = ind1;
	  sf->first = ind0;
	  sf->last = ind1;
	}
      else
	{
	  prev_snd = sf->sounds[sf->cb->snd];
	  if (prev_snd->type == SND_DATA_FILE)
	    {
	      if (prev_snd->inuse) 
		prev_snd = copy_snd_data(prev_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	      sf->data = prev_snd->buffered_data;
	      prev_snd->inuse = TRUE;
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
      ind0 = sf->cb->beg;
      ind1 = sf->cb->end;
      indx = sf->current_sound->io->beg - 1;
      file_buffers_back(ind0, ind1, indx, sf, sf->current_sound);
    }
  return(read_sample(sf));
}

mus_sample_t next_sound (snd_fd *sf)
{
  off_t ind0, ind1, indx;
  snd_data *nxt_snd;
  int at_end;
  /* if (sf->last == 0) return(reader_out_of_data(sf)); */
  at_end = ((sf->cb == NULL) || (sf->current_sound == NULL) || (sf->cb->end <= sf->current_sound->io->end));
  if (at_end)
    {
      if (sf->current_sound) 
	{
	  nxt_snd = sf->current_sound; 
	  nxt_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if (nxt_snd->copy) nxt_snd = free_snd_data(nxt_snd);
	}
      sf->cbi++;
      if (sf->cbi >= (sf->current_state)->size) return(reader_out_of_data(sf));
      sf->cb = sf->current_state->fragments[sf->cbi];
      if ((!(sf->cb)) || 
	  (sf->cb->snd == EDIT_LIST_END_MARK))
	return(reader_out_of_data(sf));
      ind0 = sf->cb->beg;
      ind1 = sf->cb->end;
      sf->fscaler = MUS_SAMPLE_TO_FLOAT(sf->cb->scl);
      if (sf->cb->scl == 0)
	{
	  sf->current_sound = NULL;
	  sf->loc = ind0;
	  sf->first = ind0;
	  sf->last = ind1;
	}
      else
	{
	  nxt_snd = sf->sounds[sf->cb->snd];
	  if (nxt_snd->type == SND_DATA_FILE)
	    {
	      if (nxt_snd->inuse)
		nxt_snd = copy_snd_data(nxt_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	      sf->data = nxt_snd->buffered_data;
	      nxt_snd->inuse = TRUE;
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
      ind0 = sf->cb->beg;
      ind1 = sf->cb->end;
      indx = sf->current_sound->io->end + 1;
      file_buffers_forward(ind0, ind1, indx, sf, sf->current_sound);
    }
  return(read_sample(sf));
}

int read_sample_eof (snd_fd *sf)
{
  return(sf->at_eof);
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
  ss->stopped_explicitly = 0;
  for (i = 0; i < chans; i++)
    obufs[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t));
  j = 0;
  reporting = (length > (10 * MAX_BUFFER_SIZE));
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
		      ss->stopped_explicitly = 0;
		      snd_warning("file save cancelled by C-g");
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
	  j = length;
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
		  ss->stopped_explicitly = 0;
		  snd_warning("file save cancelled by C-g");
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
  chan_info *cp;
  snd_fd **sf;
  axes_data *sa;
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
      if (samples < current_ed_samples(sp->chans[i]))
	samples = current_ed_samples(sp->chans[i]);
    }
  if (err == MUS_NO_ERROR)
    {
      report_in_minibuffer(sp, "saving %s", sp->short_filename);
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
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
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
  add_sound_data(sp->filename, sp, ss, WITHOUT_INITIAL_GRAPH_HOOK);
  restore_axes_data(sp, sa, mus_sound_duration(sp->filename), TRUE);
  sa = free_axes_data(sa);
  reflect_file_revert_in_label(sp);
  reflect_file_save_in_menu(ss);
  if (err)
    report_in_minibuffer_and_save(sp, "write failed: %s, edits saved in: %s", strerror(saved_errno), ofile);
  else report_in_minibuffer(sp, "wrote %s", sp->filename); 
  if (ofile) 
    {
      FREE(ofile); 
      ofile = NULL;
    }
  if (auto_update(ss)) 
    map_over_sounds(ss, snd_not_current, NULL);
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
      snd_error("%s is write-protected", sp->filename);
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
	  if (err == MUS_NO_ERROR)
	    for (i = 0; i < sp->nchans; i++) 
	      reflect_save_as_in_edit_history(sp->chans[i], new_name);
	  free_file_info(hdr);
	  return(err);
	}
      else 
	{
	  if (strcmp(caller, "file save as") == 0)
	    snd_error("save-edits: unknown header type?!? %d ", type);
	  else ss->catch_message = "unknown header type";
	  return(MUS_UNSUPPORTED_HEADER_TYPE);
	}
    }
  if (strcmp(caller, "file save as") == 0)
    snd_error("save-edits: unknown data format?!? %d", format);
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
	  report_in_minibuffer_and_save(sp, "can't save channel as %s (%s is write-protected)", ofile, sp->short_filename);
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
	  err = snd_make_file(nfile, 1, sp->hdr, sf, cp->samples[pos], cp->state);
	  free_snd_fd(sf[0]);
	}
      FREE(sf);
      if (err != MUS_NO_ERROR)
	report_in_minibuffer_and_save(sp, "save channel as temp: %s: %s)", nfile, strerror(errno));
      else 
	{
	  err = move_file(nfile, ofile);
	  if (err == 0)
	    {
	      reflect_save_as_in_edit_history(cp, ofile);
	      snd_update(ss, sp);
	    }
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
	  err = snd_make_file(ofile, 1, sp->hdr, sf, cp->samples[pos], cp->state);
	  free_snd_fd(sf[0]);
	}
      FREE(sf);
    }
  return(err);
}

void save_edits(snd_info *sp, void *ptr)
{
  int i, need_save, err;
  time_t current_write_date;
  chan_info *cp;
  if (sp == NULL) return;
  if (!sp->read_only)
    {
      need_save = 0;
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  if (cp->edit_ctr > 0) 
	    {
	      need_save = 1;
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
	      err = snd_yes_or_no_p(sp->state, "%s changed on disk! Save anyway?", sp->short_filename);
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
	report_in_minibuffer(sp, "(no changes need to be saved)");
    }
  else
    report_in_minibuffer_and_save(sp, "can't write %s (it is read-only)", sp->short_filename);
}

void revert_edits(chan_info *cp, void *ptr)
{
  int old_ctr;
  old_ctr = cp->edit_ctr;
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
    g_c_run_progn_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
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
	g_c_run_progn_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
    }
}

void undo_edit_with_sync(chan_info *cp, int count)
{
  snd_info *sp;
  int i;
  sync_info *si;
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
  if (cp)
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
	g_c_run_progn_hook(cp->undo_hook, XEN_EMPTY_LIST, S_undo_hook);
    }
}

void redo_edit_with_sync(chan_info *cp, int count)
{
  snd_info *sp;
  int i;
  sync_info *si;
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


#include "vct.h"

static XEN g_display_edits(XEN snd, XEN chn)
{
  #define H_display_edits "(" S_display_edits " &optional snd chn) returns the current edit tree state"
  FILE *tmp = NULL;
  char *buf, *name;
  chan_info *cp;
  int fd;
  off_t len;
  snd_state *ss;
  XEN res;
  ASSERT_CHANNEL(S_display_edits, snd, chn, 1);
  cp = get_cp(snd, chn, S_display_edits);
  ss = get_global_state();
  name = snd_tempnam(ss);
  tmp = fopen(name, "w");
  if (tmp) display_edits(cp, tmp);
  if ((!tmp) || (fclose(tmp) != 0))
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
  snd_remove(name, FALSE);
  if (name) FREE(name);
  res = C_TO_XEN_STRING(buf);
  FREE(buf);
  return(res);
}

static XEN g_edit_fragment(XEN uctr, XEN snd, XEN chn)
{
  #define H_edit_fragment "(" S_edit_fragment " &optional (ctr current-edit-position) snd chn) returns the edit history entry at 'ctr' \
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
		       snd, chn,
		       uctr));
  return(uctr);
}

static XEN g_edit_tree(XEN snd, XEN chn, XEN upos)
{
  #define H_edit_tree "(" S_edit_tree " snd chn pos) returns the edit lists '((global-pos data-num local-pos local-end scaler rmp0 rmp1 type)...)"
  /* internal debugging (auto-test) aid -- return complete ed list at pos */
  int i, len, pos;
  chan_info *cp;
  ed_list *ed;
  XEN res;
  ASSERT_CHANNEL(S_edit_tree, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_tree);
  if (cp)
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(upos), upos, XEN_ARG_3, S_edit_tree, "an integer");
      pos = XEN_TO_C_INT_OR_ELSE(upos, cp->edit_ctr);
      ed = cp->edits[pos];
      if (ed) 
	{
	  res = XEN_EMPTY_LIST;
	  len = ed->size; /* fragments in this list */
	  for (i = len - 1; i >= 0; i--)
	    res = XEN_CONS(XEN_LIST_8(C_TO_XEN_OFF_T(FRAGMENT_GLOBAL_POSITION(ed, i)),
				      C_TO_XEN_OFF_T(FRAGMENT_SOUND(ed, i)),
				      C_TO_XEN_OFF_T(FRAGMENT_LOCAL_POSITION(ed, i)),
				      C_TO_XEN_OFF_T(FRAGMENT_LOCAL_END(ed, i)),
				      C_TO_XEN_DOUBLE(FRAGMENT_SCALER(ed, i)),
				      C_TO_XEN_DOUBLE(FRAGMENT_RAMP_BEG(ed, i)),
				      C_TO_XEN_DOUBLE(FRAGMENT_RAMP_END(ed, i)),
				      C_TO_XEN_OFF_T(FRAGMENT_TYPE(ed, i))),
			   res);
	  return(res);
	}
    }
  return(XEN_EMPTY_LIST);
}

/* ---------------- sample readers ---------------- */

static XEN_OBJECT_TYPE sf_tag;
int sf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sf_tag));}
#define SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, sf_tag)

static XEN g_sf_p(XEN obj) 
{
  #define H_sf_p "(" S_sample_reader_p " obj) -> #t if obj is a sample-reader"
  return(C_TO_XEN_BOOLEAN(SAMPLE_READER_P(obj)));
}

snd_fd *get_sf(XEN obj) {if (SAMPLE_READER_P(obj)) return((snd_fd *)XEN_OBJECT_REF(obj)); else return(NULL);}
#define TO_SAMPLE_READER(obj) ((snd_fd *)XEN_OBJECT_REF(obj))

char *sf_to_string(snd_fd *fd)
{
  char *desc, *name;
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
	  if (cp) 
	    name = (cp->sound)->short_filename;
	  else name = "unknown source";
	}
      if (fd->cb)
	{
	  if (fd->current_sound == NULL)
	    sprintf(desc, "#<sample-reader: %s>", (fd->cb) ? "uninitialized" : "at eof");
	  else mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s from " OFF_TD ", at " OFF_TD ">",
			    fd, name, fd->initial_samp, current_location(fd));
	}
      else mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s at eof>",
			fd, name);
    }
  return(desc);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(snd_fd, print_sf, sf_to_string)

static void sf_free(snd_fd *fd)
{
  snd_info *sp = NULL;
  if (fd) 
    {
      /* changed to reflect g_free_sample_reader 29-Oct-00 */
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd(fd);
      if (sp) completely_free_snd_info(sp);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(snd_fd, free_sf, sf_free)

static XEN g_sample_reader_at_end(XEN obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_end_p " obj) -> #t if sample-reader has reached the end of its data"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_at_end_p, "a sample-reader");
  return(C_TO_XEN_BOOLEAN(read_sample_eof(TO_SAMPLE_READER(obj))));
}

static XEN g_sample_reader_home(XEN obj)
{
  #define H_sample_reader_home "(" S_sample_reader_home " obj) -> (list sound-index chan-num) associated with reader"
  snd_fd *fd = NULL;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_sample_reader_home, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  return(XEN_LIST_2(C_TO_SMALL_XEN_INT(fd->cp->sound->index),
		    C_TO_SMALL_XEN_INT(fd->cp->chan)));
}

XEN g_c_make_sample_reader(snd_fd *fd)
{
  XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
}

static XEN g_make_sample_reader(XEN samp_n, XEN snd, XEN chn, XEN dir1, XEN pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sample_reader "(" S_make_sample_reader " &optional (start-samp 0) snd chn (dir 1) edit-position)\n\
returns a reader ready to access snd's channel chn's data starting at 'start-samp', going in direction 'dir' (1 = \
forward, -1 = backward), reading the version of the data indicated by 'edit-position' which defaults to the current version. \
snd can be a filename, a sound index number, or a list with a mix id number."

  snd_fd *fd = NULL;
  int chan, edpos, direction = 1;
  chan_info *cp;
  snd_state *ss;
  char *filename;
  snd_info *loc_sp = NULL;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_make_sample_reader, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, XEN_ARG_4, S_make_sample_reader, "an integer");
  ss = get_global_state();
  if (XEN_STRING_P(snd))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chn), chn, XEN_ARG_3, S_make_sample_reader, "an integer or boolean");
      filename = XEN_TO_C_STRING(snd);
      if (mus_file_probe(filename))
	loc_sp = make_sound_readable(ss, filename, FALSE);
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
  if ((direction == READ_FORWARD) || (direction == READ_BACKWARD))
    fd = init_sample_read_any(XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0), 
			      cp, 
			      direction,
			      edpos);
  else XEN_ERROR(NO_SUCH_DIRECTION,
		 XEN_LIST_2(C_TO_XEN_STRING(S_make_sample_reader),
			    dir1));
  if (fd)
    {
      fd->local_sp = loc_sp;
      XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(XEN_FALSE);
}

static XEN g_make_region_sample_reader(XEN samp_n, XEN reg, XEN chn, XEN dir1)
{
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " &optional (start-samp 0) (region 0) chn (dir 1))\n\
returns a reader ready to access region's channel chn data starting at 'start-samp' going in direction 'dir'"

  snd_fd *fd = NULL;
  int reg_n, chn_n;
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
  if (chn_n >= region_chans(reg_n)) 
    return(snd_no_such_channel_error(S_make_region_sample_reader, XEN_LIST_1(reg), chn));

  fd = init_region_read(XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0), 
			reg_n,
			chn_n,
			XEN_TO_C_INT_OR_ELSE(dir1, READ_FORWARD));
  if (fd)
    {
      XEN_MAKE_AND_RETURN_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(XEN_FALSE);
}

static XEN g_next_sample(XEN obj)
{
  #define H_next_sample "(" S_next_sample " reader) -> next sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(protected_next_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_read_sample(XEN obj)
{
  #define H_read_sample "(" S_read_sample " reader) -> read sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(read_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_previous_sample(XEN obj)
{
  #define H_previous_sample "(" S_previous_sample " reader) -> previous sample from reader"
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_previous_sample, "a sample-reader");
  return(C_TO_XEN_DOUBLE(protected_previous_sample_to_float(TO_SAMPLE_READER(obj))));
}

static XEN g_free_sample_reader(XEN obj)
{
  #define H_free_sample_reader "(" S_free_sample_reader " reader) frees sample reader 'reader'"
  snd_fd *fd;
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_sample_reader, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  sp = fd->local_sp; 
  fd->local_sp = NULL;
  free_snd_fd_almost(fd);
  if (sp) completely_free_snd_info(sp);
  return(xen_return_first(XEN_FALSE, obj));
}

typedef float (*g_plug)(float val);
typedef float (*g_plug_env)(float val, void *envp);

static XEN g_loop_samples(XEN reader, XEN proc, XEN calls, XEN origin, XEN environ)
{
  #define H_loop_samples "(" S_loop_samples " reader func calls origin environ) calls (func (reader)) 'calls' times, \
replacing current data with the function results; origin is the edit-history name for this operation"

  /* proc here is a pointer to a float procedure that takes a float arg */
  g_plug func = NULL;
  g_plug_env func_env = NULL;
  chan_info *cp;
  snd_info *sp;
  char *ofile;
  snd_state *ss;
  int j = 0, ofd, datumb, err = 0;
  off_t i, num;
  snd_fd *sf;
  void *envp = NULL;
  file_info *hdr;
  mus_sample_t **data;
  mus_sample_t *idata;
  XEN_ASSERT_TYPE(SAMPLE_READER_P(reader), reader, XEN_ARG_1, S_loop_samples, "a sample-reader");
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(proc), proc, XEN_ARG_2, S_loop_samples, "a wrapped object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(calls), calls, XEN_ARG_3, S_loop_samples, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(origin), origin, XEN_ARG_4, S_loop_samples, "a string");
  num = XEN_TO_C_OFF_T(calls);
  if (num <= 0) return(XEN_FALSE);
  sf = TO_SAMPLE_READER(reader);
  cp = sf->cp;
  ss = cp->state;
  if ((XEN_BOUND_P(environ)) && (!(XEN_FALSE_P(environ))))
    {
      XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(environ), environ, XEN_ARG_5, S_loop_samples, "a wrapped object");
      envp = (void *)XEN_UNWRAP_C_POINTER(environ);
      func_env = (g_plug_env)XEN_UNWRAP_C_POINTER(proc);
    }
  else
    {
      func = (g_plug)XEN_UNWRAP_C_POINTER(proc);
      envp = NULL;
    }
  ofile = snd_tempnam(ss);
  sp = (cp->sound);
  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, num, XEN_TO_C_STRING(origin));
  ofd = open_temp_file(ofile, 1, hdr, ss);
  if (ofd == -1)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_loop_samples),
			 C_TO_XEN_STRING(ofile),
			 C_TO_XEN_STRING(strerror(errno))));
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (envp)
    {
      for (i = 0; i < num; i++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE((*func_env)(read_sample_to_float(sf), envp));
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }
  else
    {
      for (i = 0; i < num; i++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE((*func)(read_sample_to_float(sf)));
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }
  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
  close_temp_file(ofd, hdr, num * datumb, sp);
  hdr = free_file_info(hdr);
  file_change_samples(sf->initial_samp, num, ofile, cp, 0, DELETE_ME, LOCK_MIXES, XEN_TO_C_STRING(origin), cp->edit_ctr);
  update_graph(cp);
  if (ofile) FREE(ofile);
  FREE(data[0]);
  FREE(data);
  return(XEN_FALSE);
}


static XEN g_save_edit_history(XEN filename, XEN snd, XEN chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename &optional snd chn) saves snd channel's chn edit history in filename"
  FILE *fd;
  int i, j;
  snd_info *sp;
  chan_info *cp;
  char *mcf = NULL;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_save_edit_history, "a string");
  ASSERT_CHANNEL(S_save_edit_history, snd, chn, 2);
  mcf = mus_expand_filename(XEN_TO_C_STRING(filename));
  fd = fopen(mcf, "w");
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
	      sp = get_sp(snd);
	      if (sp)
		for (i = 0; i < sp->nchans; i++)
		  edit_history_to_file(fd, sp->chans[i]);
	    }
	  else
	    {
	      ss = get_global_state();
	      for (i = 0; i < ss->max_sounds; i++)
		if ((sp = ((snd_info *)(ss->sounds[i]))) && (sp->inuse))
		  for (j = 0; j < sp->nchans; j++)
		    edit_history_to_file(fd, sp->chans[j]);
	    }
	}
    }
  if ((!fd) || (fclose(fd) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_edit_history),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

static XEN g_undo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_undo "("  S_undo " &optional (count 1) snd chn) undoes count edits in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_undo, "an integer");
  if (XEN_LIST_P(snd_n))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(S_undo),
			 snd_n,
			 C_TO_XEN_STRING("can't undo an underlying mix edit except through the outer (mixed-into) sound")));
  ASSERT_CHANNEL(S_undo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_undo);
  if (XEN_INTEGER_P(ed_n))
    undo_edit_with_sync(cp, 
			XEN_TO_C_INT(ed_n));
  else undo_edit_with_sync(cp, 1);
  return(XEN_TRUE);
}

static XEN g_redo(XEN ed_n, XEN snd_n, XEN chn_n) /* opt ed_n */
{
  #define H_redo "("  S_redo " &optional (count 1) snd chn) redoes count edits in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ed_n), ed_n, XEN_ARG_1, S_redo, "an integer");
  if (XEN_LIST_P(snd_n))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(S_redo),
			 snd_n,
			 C_TO_XEN_STRING("can't redo an underlying mix edit except through the outer (mixed-into) sound")));
  ASSERT_CHANNEL(S_redo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_redo);
  if (XEN_INTEGER_P(ed_n))
    redo_edit_with_sync(cp, 
			XEN_TO_C_INT(ed_n));
  else redo_edit_with_sync(cp, 1);
  return(XEN_TRUE);
}

void as_one_edit(chan_info *cp, int one_edit, char *one_edit_origin) /* origin copied here */
{
  int need_backup = 0;
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
	      reflect_edit_history_change(cp);
	    }
	}
      if (need_backup) prune_edits(cp, cp->edit_ctr + 1);
      update_graph(cp); 
    }
}

static int chan_ctr = 0;
static char *as_one_edit_origin;

static int init_as_one_edit(chan_info *cp, void *ptr) 
{
  ((int *)ptr)[chan_ctr] = cp->edit_ctr; 
  chan_ctr++; 
  return(0);
}

static int finish_as_one_edit(chan_info *cp, void *ptr) 
{
  as_one_edit(cp, (((int *)ptr)[chan_ctr] + 1), as_one_edit_origin);
  chan_ctr++; 
  return(0);
}

static XEN g_as_one_edit(XEN proc, XEN origin)
{
  #define H_as_one_edit "(" S_as_one_edit " func &optional origin) runs func, collecting all edits into one from the edit historys' point of view"
  int chans;
  int *cur_edits;
  snd_state *ss;
  XEN result = XEN_FALSE;
  char *errmsg;
  XEN errstr;
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, S_as_one_edit, "a procedure");
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
      cur_edits = (int *)CALLOC(chans, sizeof(int));
      chan_ctr = 0;
      map_over_chans(ss, init_as_one_edit, (void *)cur_edits); /* redo here can't make sense, can it? */
      result = XEN_CALL_0_NO_CATCH(proc, S_as_one_edit);
      chan_ctr = 0;
      map_over_chans(ss, finish_as_one_edit, (void *)cur_edits);
      FREE(cur_edits);
    }
  return(xen_return_first(result, proc, origin));
}

static XEN g_scale_sound_by(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_scale_sound_by "(" S_scale_sound_by " scaler beg num snd chn edpos) scales samples in the given sound/channel \
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
      scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_sound_by), pos);
    }
  else
    {
      sp = get_sp(snd);
      if (sp == NULL)
	return(snd_no_such_sound_error(S_scale_sound_by, snd));
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  pos = cp->edit_ctr;
	  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_sound_by), pos);
	}
    }
  return(scl);
}

static XEN g_scale_channel(XEN scl, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_scale_channel "(" S_scale_channel " scaler beg dur snd chn edpos) scales samples in the given sound/channel \
between beg and beg + num by scaler."

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
  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_channel), pos);
  return(scl);
}			  

static XEN g_ramp_channel(XEN rmp0, XEN rmp1, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_ramp_channel "(" S_ramp_channel " rmp0 rmp1 beg dur snd chn edpos) scales samples in the given sound/channel \
between beg and beg + num by a ramp going from rmp0 to rmp1."

  Float ramp0, ramp1;
  chan_info *cp;
  off_t samp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp0), rmp0, XEN_ARG_1, S_ramp_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp1), rmp1, XEN_ARG_2, S_ramp_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_ramp_channel, beg, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(S_ramp_channel, num, XEN_ARG_4);
  ASSERT_SOUND(S_ramp_channel, snd, 5);
  ramp0 = XEN_TO_C_DOUBLE(rmp0);
  ramp1 = XEN_TO_C_DOUBLE(rmp1);
  samp = beg_to_sample(beg, S_ramp_channel);
  cp = get_cp(snd, chn, S_ramp_channel);
  pos = to_c_edit_position(cp, edpos, S_ramp_channel, 7);
  ramp_channel(cp, ramp0, ramp1, samp, dur_to_samples(num, samp, cp, pos, 4, S_ramp_channel), pos);
  return(rmp0);
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
      for (i = 0; i < num; i++)
	{
	  mval = read_sample(sf);
	  if (mval < MUS_SAMPLE_0) mval = -mval;
	  if (mval > ymax) ymax = mval;
	  j++;
	  if (j > 1000000)
	    {
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  report_in_minibuffer(cp->sound, "maxamp check interrupted...");
		  break;
		}
	    }
	}
    }
  else
    {
      for (i = 0; i < num; i++)
	{
	  mval = read_sample(sf);
	  if (mval < MUS_SAMPLE_0) mval = -mval;
	  if (mval > ymax) ymax = mval;
	}
    }
  free_snd_fd(sf);
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

static XEN g_scale_sound_to(XEN norm, XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_scale_sound_to "(" S_scale_sound_to " norm beg num snd chn) scales samples in the given sound/channel \
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
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_scale_sound_to, snd));
  if (XEN_INTEGER_P(chn))
    {
      cp = get_cp(snd, chn, S_scale_sound_by);
      samps = dur_to_samples(num, samp, cp, cp->edit_ctr, 3, S_scale_sound_to);
      if ((samp == 0) &&
	  (samps >= current_ed_samples(cp)))
	maxamp = get_maxamp(sp, cp, AT_CURRENT_EDIT_POSITION);
      else maxamp = local_maxamp(cp, samp, samps, cp->edit_ctr);
      if (maxamp > 0.0)
	{
	  scaler /= maxamp;
	  scale_channel(cp, scaler, samp, samps, cp->edit_ctr);
	}
    }
  else
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  samps = dur_to_samples(num, samp, cp, cp->edit_ctr, 3, S_scale_sound_to);
	  if ((samp == 0) &&
	      (samps >= current_ed_samples(cp)))
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
	      scale_channel(cp, scaler, samp, samps, cp->edit_ctr);
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
	vals[i] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(XEN_CAR(lst)));
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
	    vals[i] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(vdata[i]));
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
  #define H_sample "(" S_sample " samp &optional snd chn pos) -> sample samp in snd's channel chn (slow access -- use sample-readers for speed)"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(pos_n), pos_n, XEN_ARG_4, S_sample, "a number");
  ASSERT_CHANNEL(S_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_sample);
  return(C_TO_XEN_DOUBLE(chn_sample(XEN_TO_C_OFF_T_OR_ELSE(samp_n, cp->cursor), 
				    cp, 
				    to_c_edit_position(cp, pos_n, S_sample, 4))));

}

static XEN g_set_sample(XEN samp_n, XEN val, XEN snd_n, XEN chn_n, XEN edpos)
{
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  int pos;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, "set-" S_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, "set-" S_sample, "a number");
  ASSERT_CHANNEL("set-" S_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, "set-" S_sample);
  pos = to_c_edit_position(cp, edpos, "set-" S_sample, 5);
  ival[0] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  change_samples(XEN_TO_C_OFF_T_OR_ELSE(samp_n, cp->cursor), 1, ival, cp, LOCK_MIXES, "set-" S_sample, pos);
  update_graph(cp);
  return(val);
}

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

static XEN g_set_samples(XEN samp_0, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN truncate, XEN edname, XEN infile_chan, XEN edpos)
{
  #define H_set_samples "(" "set-" S_samples " start-samp samps data &optional snd chn truncate edname infile-chan edpos)\n\
sets snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename)); \
start-samp can be beyond current data end if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end."

  chan_info *cp;
  mus_sample_t *ivals;
  off_t len = 0, beg, curlen;
  int override = 0, inchan = 0, pos;
  char *fname, *caller;
  if (XEN_STRING_P(edname))
    caller = XEN_TO_C_STRING(edname);
  else caller = "set-" S_samples;
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
  if (len == 0) return(XEN_FALSE);
  override = XEN_TRUE_P(truncate);
  if (XEN_STRING_P(vect))
    {
      curlen = current_ed_samples(cp);
      fname = XEN_TO_C_STRING(vect);
      inchan = XEN_TO_C_INT_OR_ELSE(infile_chan, 0);
      if ((beg == 0) && 
	  ((len > curlen) || override))
	file_override_samples(len, fname, cp, inchan, DONT_DELETE_ME, LOCK_MIXES, caller);
      else file_change_samples(beg, len, fname, cp, inchan, DONT_DELETE_ME, LOCK_MIXES, caller, pos);
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
  #define H_vct2samples "(" S_vct2samples " start-samp samps data &optional snd chn truncate edname infile-chan)\n\
sets snd's channel chn's samples starting at start-samp for samps from data (a vct); \
start-samp can be beyond current data end if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end.  start-samp can also be a vct, as can samps"

  vct *v;
  if (VCT_P(samp_0))
    {
      v = TO_VCT(samp_0);
      if (XEN_INTEGER_P(samps))
	return(g_set_samples(XEN_ZERO, samps, samp_0, 
			     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else return(g_set_samples(XEN_ZERO, C_TO_XEN_INT(v->length), samp_0, 
				XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
    }
  else
    {
      if ((XEN_INTEGER_P(samp_0)) && (VCT_P(samps)))
	{
	  v = TO_VCT(samps);
	  return(g_set_samples(samp_0, C_TO_XEN_INT(v->length), samps, 
			       XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	}
    }
  return(g_set_samples(samp_0, samps, vect, snd_n, chn_n, truncate, edname, infile_chan, XEN_UNDEFINED));
}

static XEN g_vct2channel(XEN v, XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_vct2channel "(" S_vct2channel " vct &optional beg dur snd chn edpos)\n\
sets snd's channel chn's samples starting at beg for dur samps from vct data"
  vct *v1;
  XEN_ASSERT_TYPE(VCT_P(v), v, XEN_ARG_1, S_vct2channel, "a vct");
  if (XEN_NOT_BOUND_P(beg)) beg = XEN_ZERO;
  if (XEN_NOT_BOUND_P(dur)) 
    {
      v1 = TO_VCT(v);
      dur = C_TO_XEN_INT(v1->length);
    }
  return(g_set_samples(beg, dur, v, snd_n, chn_n, XEN_FALSE, C_TO_XEN_STRING(S_vct2channel), XEN_FALSE, edpos));
}


static XEN samples2vct_1(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN v, XEN edpos, const char *caller)
{
  chan_info *cp;
#if HAVE_LLONGS
  snd_state *ss;
#endif
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
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp_0, 0);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, cp->samples[pos] - beg);
  if ((beg == 0) && (len == 0)) return(XEN_FALSE); /* empty file (channel) possibility */
  if (len <= 0) 
    XEN_ERROR(IMPOSSIBLE_BOUNDS,
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_OFF_T(beg),
			 C_TO_XEN_OFF_T(len)));
#if HAVE_LLONGS
  ss = get_global_state();
  if ((ss->memory_available > 0) && (ss->memory_available < (len / 1024)))
    {
      snd_error("not enough memory!");
      return(XEN_FALSE);
    }
#endif
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
  #define H_samples2vct "(" S_samples2vct " &optional (start-samp 0)\n    samps snd chn vct-obj edit-position)\n\
returns a vct object (vct-obj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"
  return(samples2vct_1(samp_0, samps, snd_n, chn_n, v, edpos, S_samples2vct));
}

static XEN g_channel2vct(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_channel2vct "(" S_channel2vct " &optional beg dur snd chn edpos)\n\
returns a vct object (vct-obj if given) containing snd channel chn's data starting at beg for dur samps, \
reading edit version edpos (defaulting to the current version)"
  return(samples2vct_1(samp_0, samps, snd_n, chn_n, XEN_FALSE, edpos, S_channel2vct));
}

static XEN g_samples(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_samples "(" S_samples " &optional (start-samp 0) samps snd chn edit-position)\n\
returns a vector containing snd channel chn's samples starting a start-samp for samps samples; edit-position is the edit \
history position to read (defaults to current position)."

  XEN val;
  val = samples2vct_1(samp_0, samps, snd_n, chn_n, XEN_FALSE, edpos, S_samples);
  if (VCT_P(val))
    return(vct2vector(val));
  return(XEN_FALSE);
}

static XEN g_set_samples_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8, XEN arg9)
{
  /* (set! (samples start samps [snd chn trunc edname infilechan edpos]) vect) */
  if (XEN_NOT_BOUND_P(arg4))
    return(g_set_samples(arg1, arg2, arg3, 
			 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(arg5))
	return(g_set_samples(arg1, arg2, arg4, arg3, 
			     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
      else 
	{
	  if (XEN_NOT_BOUND_P(arg6)) 
	    return(g_set_samples(arg1, arg2, arg5, arg3, arg4, 
				 XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	  else
	    {
	      if (XEN_NOT_BOUND_P(arg7)) 
		return(g_set_samples(arg1, arg2, arg6, arg3, arg4, arg5, 
				     XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
	      else
		{
		  if (XEN_NOT_BOUND_P(arg8)) 
		    return(g_set_samples(arg1, arg2, arg7, arg3, arg4, arg5, arg6, 
					 XEN_UNDEFINED, XEN_UNDEFINED));
		  else
		    {
		      if (XEN_NOT_BOUND_P(arg9)) 
			return(g_set_samples(arg1, arg2, arg8, arg3, arg4, arg5, arg6, arg7,
					     XEN_UNDEFINED));
		      else return(g_set_samples(arg1, arg2, arg9, arg3, arg4, arg5, arg6, arg7, arg8));
		    }
		}
	    }
	}
    }
}

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
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp_0, 0);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
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

static XEN g_insert_sound(XEN file, XEN ubeg, XEN file_chn, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_insert_sound "(" S_insert_sound " file &optional beg file-chan snd chn edpos)\n\
inserts channel 'file-chan' of 'file' (or all chans if file-chan not given) into snd's channel chn at beg or the cursor position"

  chan_info *cp;
  snd_info *sp;
  char *filename = NULL;
  int nc, fchn, i;
  off_t beg = 0, len;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_insert_sound, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(ubeg), ubeg, XEN_ARG_2, S_insert_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(file_chn), file_chn, XEN_ARG_3, S_insert_sound, "an integer");
  ASSERT_CHANNEL(S_insert_sound, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_sound);
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  nc = mus_sound_chans(filename);
  if (nc == -1)
    {
      if (filename) FREE(filename);
      return(snd_no_such_file_error(S_insert_sound, file));
    }
  len = mus_sound_samples(filename) / nc;
  if (len == 0) 
    {
      if (filename) FREE(filename);
      return(C_TO_XEN_OFF_T(len));
    }
  if (XEN_NUMBER_P(ubeg))
    beg = XEN_TO_C_OFF_T_OR_ELSE(ubeg, 0);
  else beg = cp->cursor;
  if (XEN_INTEGER_P(file_chn))
    {
      fchn = XEN_TO_C_INT(file_chn);
      if (fchn < mus_sound_chans(filename))
	{
	  file_insert_samples(beg, len, filename, cp, fchn, DONT_DELETE_ME, S_insert_sound,
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
	  file_insert_samples(beg, len, filename, sp->chans[i], i, DONT_DELETE_ME, S_insert_sound,
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
  #define H_delete_sample "(" S_delete_sample " samp &optional snd chn) deletes sample 'samp' from snd's channel chn"
  chan_info *cp;
  off_t samp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_delete_sample, "a number");
  ASSERT_CHANNEL(S_delete_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_delete_sample);
  samp = XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0);
  pos = to_c_edit_position(cp, edpos, S_delete_sample, 4);
  if ((samp >= 0) && (samp <= current_ed_samples(cp)))
    {
      delete_samples(samp, 1, cp, S_delete_sample, pos);
      update_graph(cp);
      return(XEN_TRUE);
    }
  XEN_ERROR(NO_SUCH_SAMPLE,
	    XEN_LIST_4(C_TO_XEN_STRING(S_delete_sample),
		       samp_n,
		       snd_n, chn_n));
  return(samp_n);
}

static XEN g_delete_samples_1(XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, const char *origin, XEN edpos)
{
  chan_info *cp;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, origin, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, origin, "a number");
  ASSERT_CHANNEL(origin, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, origin);
  pos = to_c_edit_position(cp, edpos, S_delete_samples, 6);
  delete_samples(XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0),
		 XEN_TO_C_OFF_T_OR_ELSE(samps, 0),
		 cp, origin, pos);
  update_graph(cp);
  return(XEN_TRUE);
}

static XEN g_delete_samples(XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps &optional snd chn edpos)\n\
deletes 'samps' samples from snd's channel chn starting at 'start-samp'"

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
  #define H_insert_sample "(" S_insert_sample " sample value &optional snd chn edpos) inserts 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  int pos;
  off_t beg;
  mus_sample_t ival[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_insert_sample, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_insert_sample, "a number");
  ASSERT_CHANNEL(S_insert_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_sample);
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0);
  if (beg < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_4(C_TO_XEN_STRING(S_insert_sample),
			 samp_n,
			 snd_n, chn_n));
  pos = to_c_edit_position(cp, edpos, S_insert_sample, 5);
  ival[0] = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  insert_samples(beg, 1, ival, cp, S_insert_sample, pos);
  update_graph(cp);
  return(val);
}

static XEN g_insert_samples(XEN samp, XEN samps, XEN vect, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data &optional snd chn)\n\
inserts data (either a vector, vct, or list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  mus_sample_t *ivals;
  int pos;
  off_t beg, len = 0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_1, S_insert_samples, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, S_insert_samples, "a number");
  ASSERT_CHANNEL(S_insert_samples, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_samples);
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp, 0);
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples, 6);
  if (XEN_STRING_P(vect))
    file_insert_samples(beg, len, XEN_TO_C_STRING(vect), cp, 0, DONT_DELETE_ME, S_insert_samples, pos);
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
  XEN_ASSERT_TYPE((XEN_VECTOR_P(vect)) || (XEN_STRING_P(vect)) || XEN_FALSE_P(vect), vect, XEN_ARG_4, S_insert_samples_with_origin, "a vector or a string");
  ASSERT_CHANNEL(S_insert_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_insert_samples_with_origin);
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp, 0);
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
  else
    {
      if (XEN_STRING_P(vect))
	file_insert_samples(beg, len,
			    XEN_TO_C_STRING(vect),
			    cp, 0, 
			    DONT_DELETE_ME,
			    XEN_TO_C_STRING(origin),
			    pos);
      else extend_with_zeros(cp, beg, len, XEN_TO_C_STRING(origin), pos);
    }
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
XEN_NARGIFY_1(g_sf_p_w, g_sf_p)
XEN_NARGIFY_1(g_sample_reader_at_end_w, g_sample_reader_at_end)
XEN_ARGIFY_5(g_loop_samples_w, g_loop_samples)
XEN_ARGIFY_3(g_save_edit_history_w, g_save_edit_history)
XEN_ARGIFY_3(g_edit_fragment_w, g_edit_fragment)
XEN_ARGIFY_3(g_undo_w, g_undo)
XEN_ARGIFY_3(g_redo_w, g_redo)
XEN_ARGIFY_2(g_as_one_edit_w, g_as_one_edit)
XEN_ARGIFY_2(g_display_edits_w, g_display_edits)
XEN_ARGIFY_3(g_edit_tree_w, g_edit_tree)
XEN_ARGIFY_4(g_delete_sample_w, g_delete_sample)
XEN_ARGIFY_5(g_delete_samples_w, g_delete_samples)
XEN_ARGIFY_5(g_insert_sample_w, g_insert_sample)
XEN_ARGIFY_6(g_insert_samples_w, g_insert_samples)
XEN_ARGIFY_8(g_vct2samples_w, g_vct2samples)
XEN_ARGIFY_6(g_vct2channel_w, g_vct2channel)
XEN_ARGIFY_6(g_samples2vct_w, g_samples2vct)
XEN_ARGIFY_5(g_channel2vct_w, g_channel2vct)
XEN_ARGIFY_6(g_insert_sound_w, g_insert_sound)
XEN_ARGIFY_6(g_scale_sound_by_w, g_scale_sound_by)
XEN_ARGIFY_6(g_scale_channel_w, g_scale_channel)
XEN_ARGIFY_7(g_ramp_channel_w, g_ramp_channel)
XEN_ARGIFY_5(g_scale_sound_to_w, g_scale_sound_to)
XEN_ARGIFY_7(g_change_samples_with_origin_w, g_change_samples_with_origin)
XEN_ARGIFY_6(g_delete_samples_with_origin_w, g_delete_samples_with_origin)
XEN_ARGIFY_7(g_insert_samples_with_origin_w, g_insert_samples_with_origin)
XEN_ARGIFY_4(g_sample_w, g_sample)
XEN_ARGIFY_5(g_set_sample_w, g_set_sample)
XEN_ARGIFY_5(g_samples_w, g_samples)
XEN_ARGIFY_9(g_set_samples_w, g_set_samples)
#else
#define g_make_sample_reader_w g_make_sample_reader
#define g_make_region_sample_reader_w g_make_region_sample_reader
#define g_next_sample_w g_next_sample
#define g_read_sample_w g_read_sample
#define g_previous_sample_w g_previous_sample
#define g_free_sample_reader_w g_free_sample_reader
#define g_sample_reader_home_w g_sample_reader_home
#define g_sf_p_w g_sf_p
#define g_sample_reader_at_end_w g_sample_reader_at_end
#define g_loop_samples_w g_loop_samples
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
#define g_ramp_channel_w g_ramp_channel
#define g_scale_sound_to_w g_scale_sound_to
#define g_change_samples_with_origin_w g_change_samples_with_origin
#define g_delete_samples_with_origin_w g_delete_samples_with_origin
#define g_insert_samples_with_origin_w g_insert_samples_with_origin
#define g_sample_w g_sample
#define g_set_sample_w g_set_sample
#define g_samples_w g_samples
#define g_set_samples_w g_set_samples
#endif

#if DEBUGGING
static float test_a2(float b) {return(b * 2.0);}
static XEN g_get_test_a2(void) {return(XEN_WRAP_C_POINTER(test_a2));}

/* test the grfsnd.html example */
typedef struct {
  mus_any *del, *ri;
} flg;

static flg *make_flange(float flg_speed, float flg_amount, float flg_time)
{
  flg *gens;
  int len;
  len = (int)(flg_time * 22050) + 1;
  gens = (flg *)calloc(1, sizeof(flg));
  gens->del = mus_make_delay(len, NULL, (int)(len + 1 + flg_amount));
  gens->ri = mus_make_rand_interp(flg_speed, flg_amount);
  return(gens);
}

static float run_flange(float inval, void *ugens)
{
  flg *gens = (flg *)ugens;
  return(0.75 * (inval + mus_delay(gens->del, inval, mus_rand_interp(gens->ri, 0.0))));
}

static void free_flange(flg *gens)
{
  mus_free(gens->del);
  mus_free(gens->ri);
  free(gens);
}

static SCM g_make_flange(SCM speed, SCM amount, SCM time)
{
  flg *gens;
  gens = make_flange(scm_num2dbl(speed, "make-flange"), scm_num2dbl(amount, "make-flange"), scm_num2dbl(time, "make-flange"));
  return(scm_ulong2num((unsigned long)(gens)));
}

static SCM g_get_flange(void)
{
  return(scm_ulong2num((unsigned long)run_flange));
}

static SCM g_free_flange(SCM g_gens)
{
  free_flange((flg *)scm_num2ulong(g_gens, 0, "free-flange"));
  return(XEN_FALSE);
}

/* test the sndscm.html examples */
#include "clm2xen.h"
static int MUS_FCOMB = 0; /* this will be our fcomb type identifier */

typedef struct {
  mus_any_class *core;
  int loc, size;
  Float *line;
  Float xscl, a0, a1, x1;
} fcomb;

static int mus_fcomb_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FCOMB));}

static char *describe_fcomb(void *ptr) 
{
  char *desc = NULL;
  fcomb *gen = (fcomb *)ptr;
  desc = (char *)calloc(1024, sizeof(char));
  if (desc)
    {
      if (mus_fcomb_p((mus_any *)ptr))
        sprintf(desc, "fcomb: scaler: %.3f,  a0: %.3f,  a1: %.3f,  line[%d]", 
                gen->xscl, gen->a0, gen->a1, gen->size);
      else sprintf(desc, "not an fcomb gen");
    }
  return(desc);
}

static int fcomb_equalp(void *p1, void *p2) {return(p1 == p2);}
static int fcomb_length(void *ptr) {return(((fcomb *)ptr)->size);}
static Float *fcomb_data(void *ptr) {return(((fcomb *)ptr)->line);}
static Float fcomb_scaler(void *ptr) {return(((fcomb *)ptr)->xscl);}
static Float set_fcomb_scaler(void *ptr, Float val) {((fcomb *)ptr)->xscl = val; return(val);}

static int free_fcomb(void *uptr) 
{
  fcomb *ptr = (fcomb *)uptr;
  if (ptr)
    {
      if (ptr->line) 
        free(ptr->line);
      free(ptr); 
    }
  return(0);
}

/* now the actual run-time code executed by fcomb */
/* the extra "ignored" argument is for the run method */

static Float mus_fcomb (mus_any *ptr, Float input, Float ignored) 
{
  fcomb *gen = (fcomb *)ptr;
  Float tap_result, filter_result;
  tap_result = gen->line[gen->loc];
  filter_result = (gen->a0 * tap_result) + (gen->a1 * gen->x1);
  gen->x1 = tap_result;
  gen->line[gen->loc] = input + filter_result * gen->xscl;
  gen->loc++;
  if (gen->loc >= gen->size) gen->loc = 0;
  return(tap_result);
}

/* this is our core class descriptor */

static mus_any_class FCOMB_CLASS = {
  -1, /* MUS_FCOMB eventually */ /* mus_type: this is assigned at run-time via mus_make_class_tag below */
  "fcomb",                       /* mus_name: class name (used in descriptive/error messages */
  &free_fcomb,                   /* mus_free: free gen's struct etc */
  &describe_fcomb,               /* mus_describe: user-friendly description */
  &describe_fcomb,               /* mus_inspect: internal debugging description */
  &fcomb_equalp,                 /* mus_equalp: check equality of fcomb gens */
  &fcomb_data,                   /* mus_data: the fcomb delay line, a float array */
  0,                             /* mus_set_data: not implemented for fcomb */
  &fcomb_length,                 /* mus_length: delay line length */
  0,                             /* mus_set_length: not implemented for fcomb */
  0,0,                           /* mus_frequency, mus_set_frequency */
  0,0,                           /* mus_phase, mus_set_phase */
  &fcomb_scaler,                 /* mus_scaler: the feedback term */
  &set_fcomb_scaler,             /* mus_set_scaler */
  0, 0,
  &mus_fcomb,                    /* mus_run: the run-time fcomb function, MUS_RUN(gen) for speed */
  0,                             /* type extension (normally 0) */
  NULL, 0                         
};

/* now a function to make a new generator */

static mus_any *mus_make_fcomb (Float scaler, int size, Float a0, Float a1)
{
  fcomb *gen = NULL;
  gen = (fcomb *)calloc(1, sizeof(fcomb));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, 
              "can't allocate struct for mus_make_fcomb!");
  else
    {
      gen->core = &FCOMB_CLASS;
      if (MUS_FCOMB == 0) 
        {
          MUS_FCOMB = mus_make_class_tag();  /* this gives us a unique fcomb type id */
          gen->core->type = MUS_FCOMB;
        }
      gen->loc = 0;
      gen->xscl = scaler;
      gen->x1 = 0.0;
      gen->a0 = a0;
      gen->a1 = a1;
      gen->size = size;
      gen->line = (Float *)calloc(size, sizeof(Float));
      if (gen->line == NULL) 
        mus_error(MUS_MEMORY_ALLOCATION_FAILED, 
                  "can't allocate %d bytes for fcomb delay line in mus_make_fcomb!",
                  (int)(size * sizeof(Float)));
    }
  return((mus_any *)gen);
}

/* that is the end of the C side; the rest ties this generator into Guile/Ruby via the Xen package */
/*   in Snd's case, it's actually not needed because the generator is only called from C */

static XEN g_fcomb(XEN obj, XEN input)
{
  return(C_TO_XEN_DOUBLE(mus_fcomb(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(input), 0.0)));
}

static XEN g_fcomb_p(XEN obj)
{
  return(C_TO_XEN_BOOLEAN((mus_xen_p(obj)) && (mus_fcomb_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_make_fcomb(XEN scaler, XEN size, XEN a0, XEN a1)
{
  mus_xen *gn;
  gn = (mus_xen *)CALLOC(1,sizeof(mus_xen));
  gn->gen = mus_make_fcomb(XEN_TO_C_DOUBLE(scaler),
                           XEN_TO_C_INT(size),
                           XEN_TO_C_DOUBLE(a0),
                           XEN_TO_C_DOUBLE(a1));
  gn->nvcts = 0;
  return(mus_xen_to_object(gn));
}

static void init_fcomb(void)
{
  XEN_DEFINE_PROCEDURE("dfcomb?", g_fcomb_p, 1, 0, 0, "(fcomb? gen) -> #t if gen is an fcomb generator");
  XEN_DEFINE_PROCEDURE("dmake-fcomb", g_make_fcomb, 4, 0, 0, "(make-fcomb scaler size a0 a1) -> new fcomb gen");
  XEN_DEFINE_PROCEDURE("dfcomb", g_fcomb, 2, 0, 0, "(fcomb gen input) returns result of running fcomb gen");
}
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
  XEN_DEFINE_PROCEDURE(S_loop_samples,              g_loop_samples_w, 4, 1, 0,              H_loop_samples);

  XEN_DEFINE_PROCEDURE(S_save_edit_history,         g_save_edit_history_w, 1, 2, 0,         H_save_edit_history);
  XEN_DEFINE_PROCEDURE(S_edit_fragment,             g_edit_fragment_w, 0, 3, 0,             H_edit_fragment);
  XEN_DEFINE_PROCEDURE(S_undo,                      g_undo_w, 0, 3, 0,                      H_undo);
  XEN_DEFINE_PROCEDURE(S_redo,                      g_redo_w, 0, 3, 0,                      H_redo);
  XEN_DEFINE_PROCEDURE(S_as_one_edit,               g_as_one_edit_w, 1, 1, 0,               H_as_one_edit);
  XEN_DEFINE_PROCEDURE(S_display_edits,             g_display_edits_w, 0, 2, 0,             H_display_edits);
  XEN_DEFINE_PROCEDURE(S_edit_tree,                 g_edit_tree_w, 0, 3, 0,                 H_edit_tree);

  XEN_DEFINE_PROCEDURE(S_delete_sample,             g_delete_sample_w, 1, 3, 0,             H_delete_sample);
  XEN_DEFINE_PROCEDURE(S_delete_samples,            g_delete_samples_w, 2, 3, 0,            H_delete_samples);
  XEN_DEFINE_PROCEDURE(S_insert_sample,             g_insert_sample_w, 2, 3, 0,             H_insert_sample);
  XEN_DEFINE_PROCEDURE(S_insert_samples,            g_insert_samples_w, 3, 3, 0,            H_insert_samples);
  XEN_DEFINE_PROCEDURE(S_vct2samples,               g_vct2samples_w, 1, 7, 0,               H_vct2samples);
  XEN_DEFINE_PROCEDURE(S_vct2channel,               g_vct2channel_w, 1, 5, 0,               H_vct2channel);
  XEN_DEFINE_PROCEDURE(S_samples2vct,               g_samples2vct_w, 0, 6, 0,               H_samples2vct);
  XEN_DEFINE_PROCEDURE(S_channel2vct,               g_channel2vct_w, 0, 5, 0,               H_channel2vct);
  XEN_DEFINE_PROCEDURE(S_insert_sound,              g_insert_sound_w, 1, 5, 0,              H_insert_sound);
  XEN_DEFINE_PROCEDURE(S_scale_sound_by,            g_scale_sound_by_w, 1, 5, 0,            H_scale_sound_by);
  XEN_DEFINE_PROCEDURE(S_scale_channel,             g_scale_channel_w, 1, 5, 0,             H_scale_channel);
  XEN_DEFINE_PROCEDURE(S_ramp_channel,              g_ramp_channel_w, 2, 5, 0,              H_ramp_channel);
  XEN_DEFINE_PROCEDURE(S_scale_sound_to,            g_scale_sound_to_w, 1, 4, 0,            H_scale_sound_to);

  /* semi-internal functions (restore-state) */
  XEN_DEFINE_PROCEDURE("section-scale-by",           g_scale_sound_by_w, 5, 0, 0,           "internal scaling function");
  XEN_DEFINE_PROCEDURE(S_change_samples_with_origin, g_change_samples_with_origin_w, 4, 3, 0, "");
  XEN_DEFINE_PROCEDURE(S_delete_samples_with_origin, g_delete_samples_with_origin_w, 3, 3, 0, "");
  XEN_DEFINE_PROCEDURE(S_insert_samples_with_origin, g_insert_samples_with_origin_w, 4, 3, 0, "");

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sample, g_sample_w, H_sample,
					    "set-" S_sample, g_set_sample_w, g_set_sample_reversed, 0, 4, 0, 5);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_samples, g_samples_w, H_samples,
					    "set-" S_samples, g_set_samples_w, g_set_samples_reversed, 0, 5, 3, 6);

  #define H_save_hook S_save_hook " (snd name) is called each time a file is about to be saved. \
If it returns #t, the file is not saved.  'name' is #f unless the file is being saved under a new name (as in sound-save-as)."

  XEN_DEFINE_HOOK(save_hook, S_save_hook, 2, H_save_hook);      /* arg = sound index, possible new name */

#if DEBUGGING
  XEN_DEFINE_PROCEDURE("get-test-a2", g_get_test_a2, 0, 0, 0, "internal test function");
  scm_c_define_gsubr("free-flange", 1, 0, 0, g_free_flange);
  scm_c_define_gsubr("get-flange", 0, 0, 0, g_get_flange);
  scm_c_define_gsubr("make-flange", 3, 0, 0, g_make_flange);
  init_fcomb();
#endif
}
