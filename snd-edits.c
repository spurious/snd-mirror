#include "snd.h"

/* 
 * not implemented yet...
 * under the WITH_PARSE_TREES switch;  ed_list has 2 extra fields:
 *      MUS_SAMPLE_TYPE (*func)(struct chan__info *cp, int pos, struct snd__fd *sf, void *env);
 *      void *environ;
 * where the function (if defined) applies to the entire ed_list fragment list using environ as its state
 *   when this function needs a new sample (i.e. "as-needed input" as in CLM)
 *     it calls the function beneath it in the cp->edits list; that can also descend etc;
 *   if a list is hit that has no function, its next sample is returned (via the as-needed
 *     handler of the function above it in the list), the calls then unwind the stack of calls 
 *     (func(func...(func val))), returning the next value.  This means a given init_sample_read
 *     needs to initialize (have a make function for) the stack of functions, needs to set up
 *     the actual reader on the bottom level.  Also, the current length of the sound has no relation
 *     to its fragment indices.  Any non-global edits would have to force the list to become explicit.
 *   To include a user-defined function, we'd need a wrapper for SCM procs and a tie into user-declared
 *     as-needed-input.
 *   Even something simple like reverse needs a way to handle cut/paste pointers after the (virtual) reversal.
 *
 * A simpler version would allow only one level of this, and collapse it out if any further edits take place.
 *   (problem is that amp env reads new form and that's just as expensive as writing it out in some cases --
 *   reverse and amp-env can be done directly).
 *
 * step func as amp env: split input into pieces following the steps and set scalers (no loss ampenv)
 *   seg envs could be similar, but add incr (so now + * not just *)
 * reverse could set read-dir and loc based on assumed reverse; if subsequent edit, perform the reverse
 *   or do the reverse as a background op until it's needed (whereupon force completion)
 * offsets could be added, but need reader-proc at init time (and isn't a very common editing operation)
 */

/* TODO: if edit applies to edpos not current, show that somehow in the edit-history list */


/* -------------------------------- EDIT LISTS --------------------------------
 *
 * each channel has a list of lists containing the current edit history and the associated sound temp files or buffers
 * undo: back up current list position
 * redo: push position foward
 * No actual changes are flushed out to the file system until the file is saved.
 *
 * the three editing possibilities are insert, change, delete [and scaling].  All input goes through these lists.
 */

/* ed_list fields accessed only in this file */

static int dont_edit(chan_info *cp);
static int dont_save(snd_info *sp, char *newname);

/* each block in an edit-list list describes one fragment of the current sound */
#define ED_OUT 0
#define ED_SND 1
#define ED_BEG 2
#define ED_END 3
#define ED_SCL 4
#define ED_SIZE 5
#define INT_AS_FLOAT(X) (*((float *)(&(X))))
#define FLOAT_AS_INT(X) (*((int *)(&(X))))
/* we're assuming here that float and int are the same size, and that we'll be careful
 *   below not to use Float by mistake (since it can be double)
 */
#if __GNUC__
#if (defined(SIZEOF_FLOAT)) && (defined(SIZEOF_INT)) && (SIZEOF_INT != SIZEOF_FLOAT)
  #warning edit tree scalers assume ints and floats are the same size but they are different here!
#endif
#endif
#define UNWRAP_SAMPLE(X, ED) ((X) * (INT_AS_FLOAT(ED)))
#define UNWRAP_SAMPLE_TO_FLOAT(X, SF) ((X) * (SF->scaler))

/* try to make this code easier to read... */
#define FRAGMENT_GLOBAL_POSITION(EDLIST, FRAGMENT_NUM)  EDLIST->fragments[(FRAGMENT_NUM) * ED_SIZE + ED_OUT]
#define FRAGMENT_LOCAL_POSITION(EDLIST, FRAGMENT_NUM)   EDLIST->fragments[(FRAGMENT_NUM) * ED_SIZE + ED_BEG]
#define FRAGMENT_LOCAL_END(EDLIST, FRAGMENT_NUM)        EDLIST->fragments[(FRAGMENT_NUM) * ED_SIZE + ED_END]
#define FRAGMENT_SCALER(EDLIST, FRAGMENT_NUM)           EDLIST->fragments[(FRAGMENT_NUM) * ED_SIZE + ED_SCL]
#define FRAGMENT_SOUND(EDLIST, FRAGMENT_NUM)            EDLIST->fragments[(FRAGMENT_NUM) * ED_SIZE + ED_SND]

#define FRAGMENT_GLOBAL_POSITION_OFFSET(EDLIST, OFFSET) EDLIST->fragments[(OFFSET) + ED_OUT]
#define FRAGMENT_LOCAL_POSITION_OFFSET(EDLIST, OFFSET)  EDLIST->fragments[(OFFSET) + ED_BEG]
#define FRAGMENT_LOCAL_END_OFFSET(EDLIST, OFFSET)       EDLIST->fragments[(OFFSET) + ED_END]
#define FRAGMENT_SCALER_OFFSET(EDLIST, OFFSET)          EDLIST->fragments[(OFFSET) + ED_SCL]
#define FRAGMENT_SOUND_OFFSET(EDLIST, OFFSET)           EDLIST->fragments[(OFFSET) + ED_SND]

#define EDIT_LIST_END_MARK -2
#define EDIT_ALLOC_SIZE 128

/* ED_SND is either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END_MARK */
/* ED_BEG and ED_END are indices into the associated data => start/end point of data used in current segment */
/* ED_OUT is running segment location within current overall edited data */
/* ED_SCL is the segment scaler */
/* EDIT_ALLOC_SIZE is the allocation amount (pointers) each time cp->sounds is (re)allocated */
/* color could be added at this level ED_COLOR 4, then sf->cb[ED_COLOR] during edit tree read */

#define INSERTION_EDIT 0
#define DELETION_EDIT 1
#define CHANGE_EDIT 2
#define INITIALIZE_EDIT 3
#define PARSED_EDIT 4

#define PACK_EDIT(a, b) ((a) << 16 | (b))
#define EDIT_TYPE(a) (((a) >> 16) & 0xffff)
#define EDIT_LOCATION(a) ((a) & 0xffff)
/* edit history decoding info */
/* EDIT_LOCATION is the index in cp->sounds holding the associated data */

static char *edit_names[6] = {"insert","delete","set","init","lambda",""};

static void display_ed_list(chan_info *cp, FILE *outp, int i, ed_list *ed)
{
  int len, j, type, index;
  snd_data *sd;
  len = ed->size; /* number of fragments in this list */
  type = EDIT_TYPE(ed->sfnum);
  switch (type)
    {
    case INSERTION_EDIT:  fprintf(outp, "\n (insert %d %d) ", ed->beg, ed->len);         break;
    case DELETION_EDIT:   fprintf(outp, "\n (delete %d %d) ", ed->beg, ed->len);         break;
    case CHANGE_EDIT:     fprintf(outp, "\n (set %d %d) ", ed->beg, ed->len);            break;
    case PARSED_EDIT:     fprintf(outp, "\n (%s %d %d) ", ed->origin, ed->beg, ed->len); break;
    case INITIALIZE_EDIT: fprintf(outp, "\n (begin) ");                                  break;
    }
  if (ed->origin) fprintf(outp, "; %s ", ed->origin);
  fprintf(outp, "[%d:%d]:", i, len);
  for (j = 0; j < len; j++)
    {
      fprintf(outp, "\n   (at %d, cp->sounds[%d][%d:%d, %f])",
	      FRAGMENT_GLOBAL_POSITION(ed, j),
	      index = FRAGMENT_SOUND(ed, j),
	      FRAGMENT_LOCAL_POSITION(ed, j),
	      FRAGMENT_LOCAL_END(ed, j),
	      INT_AS_FLOAT(FRAGMENT_SCALER(ed, j)));
      if (index != EDIT_LIST_END_MARK)
	{
	  sd = cp->sounds[index];
	  if (sd == NULL) 
	    fprintf(outp, " [nil!]");
	  else 
	    if (sd->type == SND_DATA_FILE)
	      {
		if (sd->just_zeros)
		  fprintf(outp, " [zeros]");
		else fprintf(outp, " [file: %s[%d]]",
			     sd->filename, sd->chan);
	      }
	    else 
	      if (sd->type == SND_DATA_BUFFER)
		fprintf(outp, " [buf: %d] ", sd->len/4);
	      else fprintf(outp, " [bogus!]");
	}
    }
  fprintf(outp, "\n");
}

int no_ed_scalers(chan_info *cp)
{
  ed_list *ed;
  int i, len;
  ed = cp->edits[cp->edit_ctr];
  if (ed)
    {
      len = ed->size;
      for (i = 0; i < len - 1; i++)
	if (INT_AS_FLOAT(FRAGMENT_SCALER(ed, i)) != 1.0)
	  return(0);
      return(1);
    }
  return(1); /* if no edit list, no edits? */
}

int edit_changes_begin_at(chan_info *cp)
{
  ed_list *ed, *old_ed;
  int len, old_len, i, min_len;
  old_ed = cp->edits[cp->edit_ctr - 1];
  ed = cp->edits[cp->edit_ctr];
  old_len = old_ed->size;
  len = ed->size;
  if (len < old_len) 
    min_len = (len * ED_SIZE); 
  else min_len = (old_len * ED_SIZE);
  for (i = 0; i < min_len; i += ED_SIZE)
    if ((FRAGMENT_GLOBAL_POSITION_OFFSET(ed, i) != FRAGMENT_GLOBAL_POSITION_OFFSET(old_ed, i)) || 
	(FRAGMENT_SOUND_OFFSET(ed, i) != FRAGMENT_SOUND_OFFSET(old_ed, i)) || 
	(FRAGMENT_LOCAL_POSITION_OFFSET(ed, i) != FRAGMENT_LOCAL_POSITION_OFFSET(old_ed, i)))
      return(FRAGMENT_GLOBAL_POSITION_OFFSET(ed, i));
  return(0);
}

int edit_changes_end_at(chan_info *cp)
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
  mus_snprintf(edbuf, PRINT_BUFFER_SIZE, "%s : (%s %d %d)", 
	       ed->origin, edit_names[EDIT_TYPE(ed->sfnum)], ed->beg, ed->len);
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
  snd_data *sf;
  snd_state *ss;
  char *newname;
  int i, snd;
  snd = EDIT_LOCATION(ed->sfnum);
  if (snd < cp->sound_size)
    {
      sf = cp->sounds[snd];
      ss = cp->state;
      if (sf->type == SND_DATA_BUFFER)
	{
	  if ((ed->len > 16) && (save_dir(ss)))
	    {
	      newname = shorter_tempnam(save_dir(ss), "snd_");
	      mus_array_to_file(newname, sf->buffered_data, ed->len, 22050, 1);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      fprintf(fd, "#(");
	      for (i = 0; i < ed->len; i++) 
#if SNDLIB_USE_FLOATS
		fprintf(fd, "%f ", sf->buffered_data[i]);
#else
	        fprintf(fd, "%d ", sf->buffered_data[i]);
#endif
	      fprintf(fd, ")");
	    }
	}
      else
	{
	  if (sf->just_zeros) 
	    {
	      fprintf(fd, "#f");
	      return;
	    }
	  if (save_dir(ss))
	    {
	      newname = shorter_tempnam(save_dir(ss), "snd_");
	      copy_file(sf->filename, newname);
	      fprintf(fd, "\"%s\"", newname);
	      FREE(newname);
	    }
	  else
	    {
	      /* read at very low level */
	      int ifd, idataloc, bufnum, n, cursamples, samples, sample;
	      MUS_SAMPLE_TYPE *buffer;
	      MUS_SAMPLE_TYPE **ibufs;
	      fprintf(fd, "#(");
	      ifd = mus_file_open_read(sf->filename);
	      if (ifd == -1) 
		{
		  snd_error("can't open %s: %s! [%s[%d] %s]",
			    sf->filename,
			    strerror(errno),
			    __FILE__, __LINE__, __FUNCTION__); 
		  return;
		}
	      idataloc = mus_sound_data_location(sf->filename);
	      mus_file_open_descriptors(ifd,
				       sf->filename,
				       mus_sound_data_format(sf->filename),
				       mus_sound_datum_size(sf->filename),
				       idataloc,
				       mus_sound_chans(sf->filename),
				       mus_sound_header_type(sf->filename));
	      samples = mus_sound_samples(sf->filename);
	      mus_file_seek(ifd, idataloc, SEEK_SET);
	      ibufs = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
	      ibufs[0] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));
	      bufnum = (FILE_BUFFER_SIZE);
	      sample = 0;
	      for (n = 0; n < samples; n += bufnum)
		{
		  if ((n + bufnum) < samples) cursamples = bufnum; else cursamples = (samples - n);
		  mus_file_read(ifd, 0, cursamples - 1, 1, ibufs);
		  buffer = (MUS_SAMPLE_TYPE *)(ibufs[0]);
		  for (i = 0; i < cursamples; i++) 
		    {
#if SNDLIB_USE_FLOAT
		      fprintf(fd, "%f ", MUS_SAMPLE_TO_FLOAT(buffer[i]));
#else
		      fprintf(fd, "%d ", MUS_SAMPLE_TO_INT(buffer[i]));
#endif
		      sample++;
		      if (sample == ed->len)
			{
			  if (mus_file_close(ifd) != 0)
			    snd_error("can't close %d (%s): %s! [%s[%d] %s]",
				      ifd, sf->filename,
				      strerror(errno),
				      __FILE__, __LINE__, __FUNCTION__);
			  fprintf(fd, ")");
			  return;
			}
		    }
		}
	      FREE(ibufs[0]);
	      FREE(ibufs);
	      fprintf(fd, ")");
	      if (mus_file_close(ifd) != 0)
		snd_error("can't close %d (%s): %s! [%s[%d] %s]",
			  ifd, sf->filename,
			  strerror(errno),
			  __FILE__, __LINE__, __FUNCTION__);
	    }
	}
    }
}

/* TODO: edit_list_to_function (branch-sound with place of branching)
 *          if edits->function, the current edit sequence could be applied to any sound
 *            but to do this we need "true" edit history info (which may mean saving user-functions etc)
 */

void edit_history_to_file(FILE *fd, chan_info *cp)
{
  /* write edit list as a snd-guile program to fd (open for writing) for subsequent load */
  /* this needs only 3 operations: delete-samples, insert-samples, and change-samples */
  /*   the actual user-operations that produced these are included as comments */
  /*   the data is included as a vector, so the file can be very large! */
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
	  switch (EDIT_TYPE(ed->sfnum))
	    {
	    case INSERTION_EDIT: 
	      /* samp data snd chn */
	      fprintf(fd, "      (%s %d %d \"%s\" ",
		      S_insert_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, " sfile %d)\n", cp->chan);
	      break;
	    case DELETION_EDIT:
	      /* samp samps snd chn */
	      fprintf(fd, "      (%s %d %d \"%s\" sfile %d)\n",
		      S_delete_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "",
		      cp->chan);
	      break;
	    case CHANGE_EDIT:
	      fprintf(fd, "      (%s %d %d \"%s\" ",
		      S_change_samples_with_origin,
		      ed->beg,
		      ed->len,
		      (ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd, ed, cp);
	      fprintf(fd, " sfile %d)\n", cp->chan);
	      break;
	    case PARSED_EDIT: 
	      /* a parsed edit has to be able to recreate itself completely */
	      fprintf(fd, "      (%s sfile %d)\n",
		      ed->origin,
		      cp->chan);
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

static void copy_ed_blocks(int *new_list, int *old_list, int new_beg, int old_beg, int num_lists)
{
  /* beg and num_lists here are in terms of blocks, not ints */
  int i, end, k;
  if (num_lists > 0)
    {
      end = (old_beg + num_lists) * ED_SIZE;
      for (k = new_beg * ED_SIZE, i = old_beg * ED_SIZE; i < end; i++, k++) 
	new_list[k] = old_list[i];
    }
}

static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  ed=(ed_list *)CALLOC(1, sizeof(ed_list));
  ed->size = size;
  ed->fragments = (int *)CALLOC(size * ED_SIZE, sizeof(int));
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
  if (ed)
    {
      if (ed->fragments) FREE(ed->fragments);
      if (ed->origin) FREE(ed->origin);
      FREE(ed);
    }
  return(NULL);
}

void backup_edit_list(chan_info *cp)
{
  int cur, i;
  snd_data *sf;
  cur = cp->edit_ctr;
  if (cur <= 0) return;
  free_ed_list(cp->edits[cur - 1]);
  free_amp_env(cp, cur-1);
  cp->edits[cur - 1] = cp->edits[cur];
  cp->amp_envs[cur - 1] = cp->amp_envs[cur];
  cp->edits[cur] = NULL;
  cp->amp_envs[cur] = NULL;
  cp->samples[cur - 1] = cp->samples[cur];
  if (cp->sounds)
    { /* protect from release_pending_sounds upon edit after undo after as-one-edit or whatever */
      for (i = 0; i < cp->sound_size; i++)
	{
	  sf = cp->sounds[i];
	  if ((sf) && (sf->edit_ctr == cur)) sf->edit_ctr--;
	}
    }
  /* marks backup added 23-Jun-00 */
  if (cp->marks)
    {
      release_pending_marks(cp, cur - 1);
      if (cp->marks[cur - 1]) FREE(cp->marks[cur - 1]); /* not freed by release_pending_marks */
      cp->marks[cur - 1] = cp->marks[cur];
      cp->marks[cur] = NULL;
      cp->mark_ctr[cur - 1] = cp->mark_ctr[cur];
      cp->mark_ctr[cur] = -1;
    }
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

static ed_list *initial_ed_list(int beg, int end)
{
  ed_list *ed;
  float one = 1.0;
  ed = make_ed_list(2);
  ed->beg = beg;
  ed->len = end + 1;
  ed->selection_beg = NO_SELECTION;
  ed->selection_end = 0;
  ed->sfnum = PACK_EDIT(INITIALIZE_EDIT, 0);
  /* origin (channel %s %d) desc channel should be obvious from context */
  FRAGMENT_LOCAL_POSITION(ed, 0) = beg;
  FRAGMENT_LOCAL_END(ed, 0) = end;
  FRAGMENT_SOUND(ed, 0) = 0;
  FRAGMENT_GLOBAL_POSITION(ed, 0) = 0;
  FRAGMENT_SCALER(ed, 0) = FLOAT_AS_INT(one);
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

void set_initial_ed_list(chan_info *cp, int len) 
{
  cp->edits[0] = initial_ed_list(0, len);
}

static int find_split_loc(int samp, ed_list *current_state)
{
  int i, k;
  for (i = 0, k = 0; i < current_state->size; i++, k += ED_SIZE)
    if (FRAGMENT_GLOBAL_POSITION_OFFSET(current_state, k) >= samp) 
      return(i);
  return(0); /* make sgi compiler happy */
}

static ed_list *selected_ed_list(int beg, int end, ed_list *current_state)
{
  int bk, ek, new_size, i, k, j, oldk, diff, len;
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
  /* if (ek < 0) ek = new_size-1; */
  if (ek < 0) ek = 0;
  if (FRAGMENT_GLOBAL_POSITION(current_state, bk) != beg) new_size++;
  if (FRAGMENT_GLOBAL_POSITION(current_state, (ek + 1)) != end + 1) new_size++;
  new_ed = make_ed_list(new_size);
  new_ed->size = new_size;
  for (k = 0; k <= bk; k++)
    {
      j = k * ED_SIZE;
      for (i = 0; i < ED_SIZE; i++)
	new_ed->fragments[j + i] = current_state->fragments[j + i];
    }
  k = bk + 1;
  diff = beg - FRAGMENT_GLOBAL_POSITION(current_state, bk);
  if (diff != 0)
    {
      for (i = 0; i < ED_SIZE; i++)
	new_ed->fragments[k * ED_SIZE + i] = current_state->fragments[bk * ED_SIZE + i];
      FRAGMENT_LOCAL_END(new_ed, bk) = FRAGMENT_LOCAL_POSITION(new_ed, bk) + diff - 1;
      FRAGMENT_GLOBAL_POSITION(new_ed, k) = beg;
      FRAGMENT_LOCAL_POSITION(new_ed, k) = FRAGMENT_LOCAL_END(new_ed, bk) + 1;
      k++;
    }
  for (oldk = bk + 1; oldk <= ek; oldk++, k++)
    {
      for (i = 0; i < ED_SIZE; i++)
	new_ed->fragments[k * ED_SIZE + i] = current_state->fragments[oldk * ED_SIZE + i];
    }
  diff = FRAGMENT_GLOBAL_POSITION(current_state, (ek + 1)) - end - 1;
  if (diff != 0)
    {
      for (i = 0; i < ED_SIZE; i++)
	new_ed->fragments[k * ED_SIZE + i] = current_state->fragments[ek * ED_SIZE + i];
      FRAGMENT_LOCAL_END(new_ed, (k-1)) -= diff;
      FRAGMENT_LOCAL_POSITION(new_ed, k) = FRAGMENT_LOCAL_END(new_ed, (k-1)) + 1;
      FRAGMENT_GLOBAL_POSITION(new_ed, k) = end + 1; /* 1? */
      k++;
    }
  for (oldk = ek + 1; (oldk <= current_state->size) && (k < new_size); oldk++, k++)
    {
      for (i = 0; i < ED_SIZE; i++)
	new_ed->fragments[k * ED_SIZE + i] = current_state->fragments[oldk * ED_SIZE + i];
    }
  return(new_ed);
}


/* there are a few special-case multi-channel temp files that need a kind of reference count to handle deletion */
/* this machinery affects only these special cases, not temp files in general */

typedef struct {
  char *name;
  int chans;
  int *ticks;
} tempfile_ctr;

static tempfile_ctr **tempfiles = NULL;
static int tempfiles_size = 0;

void remember_temp(char *filename, int chans)
{
  int i;
  tempfile_ctr *tmp;
  if (tempfiles_size == 0)
    {
      tempfiles_size = 8;
      tempfiles = (tempfile_ctr **)CALLOC(tempfiles_size, sizeof(tempfile_ctr *));
      i = 0;
    }
  else
    {
      for (i = 0; i < tempfiles_size; i++)
	if (tempfiles[i] == NULL)
	  break;
      if (i >= tempfiles_size)
	{
	  tempfiles = (tempfile_ctr **)REALLOC(tempfiles, tempfiles_size * sizeof(tempfile_ctr *));
	  for (i = tempfiles_size; i<(tempfiles_size + 8); i++) tempfiles[i] = NULL;
	  i = tempfiles_size;
	  tempfiles_size += 8;
	}
    }
  tmp = (tempfile_ctr *)CALLOC(1, sizeof(tempfile_ctr));
  tempfiles[i] = tmp;
  tmp->name = copy_string(filename);
  tmp->chans = chans;
  tmp->ticks = (int *)CALLOC(chans, sizeof(int));
}

static void remember_redundant_temp(char *filename, int cur_chan)
{
  int i, j;
  tempfile_ctr *tmp;
  for (i = 0; i < tempfiles_size; i++)
    {
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename, tmp->name) == 0))
	{
	  if (cur_chan >= tmp->chans)
	    {
	      tmp->ticks = (int *)REALLOC(tmp->ticks, (cur_chan + 1) * sizeof(int));
	      for (j = tmp->chans; j < cur_chan + 1; j++) tmp->ticks[j] = 0;
	      tmp->chans = cur_chan + 1;
	    }
	  return;
	}
    }
  remember_temp(filename, cur_chan + 1);
}

static void forget_temp(char *filename, int chan)
{
  int i, j, happy = 0;
  tempfile_ctr *tmp;
  for (i = 0; i < tempfiles_size; i++)
    {
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename, tmp->name) == 0))
	{
	  tmp->ticks[chan]--;
	  for (j = 0; j < tmp->chans; j++)
	    if (tmp->ticks[j] > 0) 
	      {
		happy = 1;
		return;
	      }
	  if (happy == 0)
	    {
	      snd_remove(tmp->name);
	      FREE(tmp->name);
	      FREE(tmp->ticks);
	      FREE(tmp);
	      tempfiles[i] = NULL;
	    }
	  return;
	}
    }
}

static void tick_temp(char *filename, int chan)
{
  int i;
  tempfile_ctr *tmp;
  for (i = 0; i < tempfiles_size; i++)
    {
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename, tmp->name) == 0))
	{
	  tmp->ticks[chan]++;
	  return;
	}
    }
}

void forget_temps(void)
{
  int i;
  tempfile_ctr *tmp;
  for (i = 0; i < tempfiles_size; i++)
    {
      tmp = tempfiles[i];
      if (tmp) 
	snd_remove(tmp->name);
    }
}

snd_data *make_snd_data_file(char *name, int *io, MUS_SAMPLE_TYPE *data, file_info *hdr, int temp, int ctr, int temp_chan)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = SND_DATA_FILE;
  sf->buffered_data = data;
  sf->io = io;
  sf->filename = copy_string(name);
  sf->hdr = hdr;
  sf->temporary = temp;
  if (temp == MULTICHANNEL_DELETION) tick_temp(name, temp_chan);
  sf->edit_ctr = ctr;
  sf->open = FD_OPEN;
  sf->inuse = FALSE;
  sf->copy = FALSE;
  sf->chan = temp_chan;
  sf->len = (hdr->samples) * (mus_data_format_to_bytes_per_sample(hdr->format)) + hdr->data_location;
  sf->just_zeros = 0;
#if DEBUGGING
  sf->caller = __FUNCTION__;
  sf->active = 1;
#endif
  return(sf);
}

static snd_data *make_snd_data_zero_file(int size, int *io, MUS_SAMPLE_TYPE *data, int ctr)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = SND_DATA_FILE;
  sf->buffered_data = data;
  sf->io = io;
  sf->filename = NULL;
  sf->hdr = NULL;
  sf->temporary = DONT_DELETE_ME;
  sf->edit_ctr = ctr;
  sf->open = FD_CLOSED;
  sf->inuse = FALSE;
  sf->copy = FALSE;
  sf->chan = 0;
  sf->len = size;
  sf->just_zeros = 1;
#if DEBUGGING
  sf->caller = __FUNCTION__;
  sf->active = 1;
#endif
  return(sf);
}

snd_data *copy_snd_data(snd_data *sd, chan_info *cp, int bufsize)
{
  snd_data *sf;
  int *datai;
  int fd;
  file_info *hdr;
  if (sd->just_zeros)
    {
      datai = make_zero_file_state(sd->len);
      sf = make_snd_data_zero_file(sd->len,
				   datai,
				   MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(0)]),
				   sd->edit_ctr);
      sf->copy = 1;
      return(sf);
    }
  hdr = sd->hdr;
  fd = snd_open_read(cp->state, sd->filename);
  if (fd == -1) return(NULL);
  mus_file_open_descriptors(fd,
			   sd->filename,
			   hdr->format,
			   mus_data_format_to_bytes_per_sample(hdr->format),
			   hdr->data_location,
			   hdr->chans,
			   hdr->type);
  during_open(fd, sd->filename, SND_COPY_READER);
  datai = make_file_state(fd, hdr, sd->chan, bufsize);
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = sd->type;
  sf->buffered_data = MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(sd->chan)]);
  sf->io = datai;
  sf->filename = copy_string(sd->filename);
  sf->hdr = hdr;
  sf->temporary = DONT_DELETE_ME;
  sf->edit_ctr = sd->edit_ctr;
  sf->open = FD_OPEN;
  sf->inuse = FALSE;
  sf->copy = 1;
  sf->just_zeros = 0;
#if DEBUGGING
  sf->caller = __FUNCTION__;
  sf->active = 1;
#endif
  return(sf);
}

snd_data *make_snd_data_buffer(MUS_SAMPLE_TYPE *data, int len, int ctr)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->buffered_data = (MUS_SAMPLE_TYPE *)MALLOC((len + 1) * sizeof(MUS_SAMPLE_TYPE));
  /* sigh... using len + 1 rather than len to protect against access to inserted buffer at end mixups (final fragment uses end + 1) */
  /*   the real problem here is that I never decided whether insert starts at the cursor or just past it */
  /*   when the cursor is on the final sample, this causes cross-fragment ambiguity as to the length of a trailing insertion */
  /*   C > (make-region 1000 2000) (insert-region (cursor)) C-v hits this empty slot and gets confused about the previously final sample value */
  memcpy((void *)(sf->buffered_data), (void *)data, len * sizeof(MUS_SAMPLE_TYPE));
  sf->edit_ctr = ctr;
  sf->copy = FALSE;
  sf->inuse = FALSE;
  sf->len = len * 4;
  sf->just_zeros = 0;
#if DEBUGGING
  sf->caller = __FUNCTION__;
  sf->active = 1;
#endif
  return(sf);
}

#if DEBUGGING
void print_snd_data(snd_data *sf);
void print_snd_data(snd_data *sf)
{
  fprintf(stderr,"snd_data: caller: %s, filename: %s, active: %d\n\
   copy:%d, io: %p, hdr: %p, edit_ctr: %d,\n\
   open: %d, inuse: %d, chan: %d, len: %d\n\
   just_zeros: %d, type: %d\n",
	  sf->caller, sf->filename, sf->active,
	  sf->copy, sf->io, sf->hdr, sf->edit_ctr,
	  sf->open, sf->inuse, sf->chan, sf->len, 
	  sf->just_zeros, sf->type);
}

void print_snd_fd(snd_fd *sf);
void print_snd_fd(snd_fd *sf)
{
  fprintf(stderr,"snd_fd: caller: %s, filename: %s\n\
    current_sound: %p, first: %p, last: %p, view_buffered_data: %p,\n\
    eof: %d, beg: %d, end: %d, direction: %d, initial_samp: %d, scaler: %f,\n\
    cbi: %d, cp: %p, edit_pos: %d, local_sp: %p, sounds: %p, cb: %p, current_state: %p\n",
	  sf->caller, sf->filename, 
	  sf->current_sound, sf->first, sf->last, sf->view_buffered_data,
	  sf->eof, sf->beg, sf->end, sf->direction, sf->initial_samp, sf->scaler,
	  sf->cbi, sf->cp, sf->edit_pos, sf->local_sp, sf->sounds, sf->cb, sf->current_state);
}
#endif

snd_data *free_snd_data(snd_data *sf)
{
  /* in the snd file case, these pointers are dealt with elsewhere (where??) */
  if (sf)
    {
#if DEBUGGING
      if (sf->active != 1)
	{
	  fprintf(stderr,"double snd_data free (%s): %s %s [%d %p %p %d %d %d %d %d %d]\n",
		  (sf->temporary == ALREADY_DELETED) ? "recognized" : "not recognized",
		  sf->caller, sf->filename, sf->copy, sf->io, sf->hdr, sf->edit_ctr,
		  sf->open, sf->inuse, sf->chan, sf->len, sf->just_zeros);
	  return(NULL);
	}
      sf->active = 0;
#endif
      if (sf->temporary == ALREADY_DELETED)
	return(NULL);
      if (sf->temporary == MULTICHANNEL_DELETION)
	forget_temp(sf->filename, sf->chan);
      if ((sf->type == SND_DATA_BUFFER) && 
	  (sf->buffered_data)) 
	FREE(sf->buffered_data);
      sf->buffered_data = NULL;
      if ((!(sf->copy)) && 
	  (sf->hdr)) 
	free_file_info(sf->hdr);
      sf->hdr = NULL;
      if (sf->io)
	{
	  if (sf->open == FD_OPEN) close_file_state_fd(sf->io);
	  sf->io = free_file_state(sf->io);
	  if (sf->temporary == DELETE_ME) 
	    snd_remove(sf->filename);
	}
      if (sf->filename) FREE(sf->filename);
      sf->filename = NULL;
      sf->temporary = ALREADY_DELETED;
      sf->copy = FALSE;
      FREE(sf);
    }
  return(NULL);
}

void free_sound_list (chan_info *cp)
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

static int gather_usage_stats_1(chan_info *cp, void *ptr)
{
  int i;
  snd_data *sf;
  env_info *ep;
  if (cp)
    {
      if (cp->stats) 
	for (i = 0; i < 8; i++) 
	  cp->stats[i] = 0;
      else cp->stats = (int *)CALLOC(8, sizeof(int));
      if (cp->amp_envs)
	for (i = 0; i < cp->edit_size; i++)
	  {
	    ep = cp->amp_envs[i];
	    if (ep)
	      {
		cp->stats[AMP_ENVS_ACTIVE]++;
		cp->stats[AMP_ENV_USAGE] += (2 * ((cp->amp_envs[i])->amp_env_size) * sizeof(int));
	      }
	  }
      if (cp->sounds)
	for (i = 0; i < cp->sound_size; i++)
	  if ((sf = (cp->sounds[i])))
	    {
	      if (sf->type == SND_DATA_BUFFER)
		{
		  if (sf->buffered_data) 
		    {
		      cp->stats[ARRAY_USAGE] += sf->len;
		      cp->stats[ARRAYS_ACTIVE]++;
		    }
		}
	      else 
		{
		  if (sf->io)
		    {
		      cp->stats[ARRAY_USAGE] += (file_state_buffer_size(sf->io) * 4);
		      cp->stats[ARRAYS_ACTIVE]++;
		    }
		  if (sf->temporary == DELETE_ME)
		    {
		      cp->stats[TEMP_USAGE] += sf->len;
		      cp->stats[TEMPS_ACTIVE]++;
		      if (sf->open == FD_OPEN) cp->stats[TEMPS_OPEN]++;
		    }
		  else
		    cp->stats[FILE_USAGE] += (sf->len) / ((cp->sound)->nchans);
		}
	    }
    }
  return(0);
}

void gather_usage_stats(chan_info *cp)
{
  gather_usage_stats_1(cp, NULL);
  update_stats_display(cp->state, FALSE);
}

void update_all_usage_stats(snd_state *ss)
{
  map_over_chans(ss, gather_usage_stats_1, NULL);
}
  
static void release_pending_sounds(chan_info *cp, int edit_ctr)
{
  /* look for buffers or open temp files that are no longer reachable after pruning the edit tree */
  int i;
  snd_data *sf;
  if (cp)
    {
      if (cp->sounds)
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
      if (show_usage_stats(cp->state)) 
	gather_usage_stats(cp);
    }
}

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

static int add_sound_buffer_to_edit_list(chan_info *cp, MUS_SAMPLE_TYPE *data, int len)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(data, len, cp->edit_ctr);
#if DEBUGGING
  cp->sounds[cp->sound_ctr]->caller = __FUNCTION__;
#endif
  if (show_usage_stats(cp->state)) gather_usage_stats(cp);
  return(cp->sound_ctr);
}

static int add_sound_file_to_edit_list(chan_info *cp, char *name, int *io, MUS_SAMPLE_TYPE *data, file_info *hdr, int temp, int chan)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_file(name, io, data, hdr, temp, cp->edit_ctr, chan);
  if (show_usage_stats(cp->state)) gather_usage_stats(cp);
  return(cp->sound_ctr);
}

static int add_zero_file_to_edit_list(chan_info *cp, int size)
{
  int *datai;
  prepare_sound_list(cp);
  datai = make_zero_file_state(size);
  cp->sounds[cp->sound_ctr] = make_snd_data_zero_file(size,
						      datai,
						      MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(0)]),
						      cp->edit_ctr);
  return(cp->sound_ctr);
}

static void ripple_out(int *list, int beg, int num, int len)
{
  int i, k;
  for (i = beg, k = beg * ED_SIZE; i < len; i++, k += ED_SIZE) list[k + ED_OUT] += num;
}

static void prune_edits(chan_info *cp, int edpt)
{
  int i;
  if (cp->edits[edpt]) 
    {
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

static void prepare_edit_list(chan_info *cp, int len)
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
      if (!cp->samples) cp->samples = (int *)CALLOC(cp->edit_size, sizeof(int));
      else cp->samples = (int *)REALLOC(cp->samples, cp->edit_size * sizeof(int));
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

int current_ed_samples(chan_info *cp)
{
  if (cp) 
    return(cp->samples[cp->edit_ctr]);
  else return(0);
}

static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  int samps;
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

static void fixup_edlist_endmark(ed_list *new_state, ed_list *current_state, int len)
{
  int k;
  if (FRAGMENT_SOUND(new_state, (new_state->size - 1)) != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size - 1; k > 0; k--)
	if (FRAGMENT_SOUND(new_state, k) == EDIT_LIST_END_MARK) break;
      if (k > 0) 
	new_state->size = k + 1;
      else
	{
	  if (new_state->size == current_state->size)
	    {
	      if (new_state->size < (len + 1))
		{
		  k = new_state->size;
		  new_state->size += 1;
		  FRAGMENT_SOUND(new_state, new_state->size) = EDIT_LIST_END_MARK;
		  FRAGMENT_LOCAL_POSITION(new_state, new_state->size) = FRAGMENT_LOCAL_POSITION(new_state, k);
		  FRAGMENT_GLOBAL_POSITION(new_state, new_state->size) = FRAGMENT_GLOBAL_POSITION(new_state, k);
		  FRAGMENT_LOCAL_END(new_state, new_state->size) = FRAGMENT_LOCAL_END(new_state, k);
		  FRAGMENT_SCALER(new_state, new_state->size) = FRAGMENT_SCALER(new_state, k);
		}
	    }
	}
    }
}

static ed_list *insert_samples_1 (int samp, int num, MUS_SAMPLE_TYPE* vals, ed_list *current_state, 
				  chan_info *cp, int **cb_back, const char *origin)
{
  int len, k, old_beg, old_end, old_snd, old_out;
  ed_list *new_state;
  int *cb, *cbback, *ed;
  int old_scl;
  float one = 1.0;
  if (num <= 0) return(NULL);
  len = current_state->size;
  ed = current_state->fragments;
  k = find_split_loc(samp, current_state);
  cb = (int *)(ed + k * ED_SIZE);
  if ((k == 0) && (cb[ED_END] == -1))
    {
      /* no data: just set insertion */
      new_state = append_ed_list(cp, len);
      copy_ed_blocks(new_state->fragments, ed, 0, 0, len);
      cb = (int *)(new_state->fragments);
      cb[ED_END] = num - 1;
    }
  else
    {
      if ((samp == cb[ED_OUT]) || (samp == (cb[ED_OUT] - 1)))
	{
	  new_state = append_ed_list(cp, len + 1);
	  copy_ed_blocks(new_state->fragments, ed, 0, 0, k);
	  copy_ed_blocks(new_state->fragments, ed, k + 1, k, len - k);
	  len++;
	}
      else
	{
	  cbback = (int *)(ed + (k - 1) * ED_SIZE);
	  old_beg = cbback[ED_BEG];
	  old_end = cbback[ED_END];
	  old_snd = cbback[ED_SND];
	  old_out = cbback[ED_OUT];
	  old_scl = cbback[ED_SCL];
	  new_state = append_ed_list(cp, len + 2);
	  copy_ed_blocks(new_state->fragments, ed, 0, 0, k);
	  copy_ed_blocks(new_state->fragments, ed, k + 2, k, len - k);
	  cb = (int *)(new_state->fragments + (k + 1) * ED_SIZE);  /* old after split */
	  cbback = (int *)(new_state->fragments + (k - 1) * ED_SIZE); /* old before split */
	  cb[ED_SND] = old_snd;
	  cb[ED_OUT] = samp;
	  cb[ED_BEG] = old_beg + samp - old_out;
	  cb[ED_END] = old_end;
	  cb[ED_SCL] = old_scl;
	  cbback[ED_END] = old_beg + samp - old_out - 1;
	  len += 2;
	}
      cb = (int *)(new_state->fragments + k * ED_SIZE); /* new */
      cb[ED_BEG] = 0;
      cb[ED_END] = num - 1;
      cb[ED_OUT] = samp;
      cb[ED_SCL] = FLOAT_AS_INT(one); /* trust that insertion collapsed possible "tree" */
    }
  if (vals) 
    {
      cb[ED_SND] = add_sound_buffer_to_edit_list(cp, vals, num); 
      new_state->sfnum = PACK_EDIT(INSERTION_EDIT, cb[ED_SND]);
    }
  else (*cb_back) = cb;
  /* vals is null if data is in a file -- handled later */
  new_state->beg = samp;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  ripple_out(new_state->fragments, k + 1, num, len);
  ripple_marks(cp, samp, num);
  ripple_mixes(cp, samp, num);
  ripple_selection(new_state, samp, num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state, current_state, len);
  return(new_state);
}

void extend_with_zeros(chan_info *cp, int beg, int num, const char *origin)
{
  MUS_SAMPLE_TYPE *zeros;
  int k, len;
  int *cb;
  ed_list *ed;
  if (num <= 0) return;
  len = current_ed_samples(cp);
  k = cp->edit_ctr;
  prepare_edit_list(cp, len+num);
  if (num > 1024)
    {
      cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, NULL, cp->edits[k], cp, &cb, origin);
      cb[ED_SND] = add_zero_file_to_edit_list(cp, num);
      ed = cp->edits[cp->edit_ctr];
      ed->sfnum = PACK_EDIT(INSERTION_EDIT, cb[ED_SND]);
    }
  else
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(num, sizeof(MUS_SAMPLE_TYPE));
      cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, zeros, cp->edits[k], cp, &cb, origin);
      FREE(zeros);
    }
  reflect_edit_history_change(cp);
  check_for_first_edit(cp); /* needed to activate revert menu option */
}

void file_insert_samples(int beg, int num, char *inserted_file, chan_info *cp, int chan, int auto_delete, const char *origin)
{
  int k, len;
  int *cb;
  int fd;
  int *datai;
  ed_list *ed;
  file_info *hdr;
  snd_state *ss;
  if (num <= 0) 
    {
      if ((inserted_file) && (auto_delete == DELETE_ME)) snd_remove(inserted_file);
      if ((inserted_file) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(inserted_file, chan);
      return;
    }
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len + 1, "(insert-extend)");
      len = current_ed_samples(cp);
    }
  ss = cp->state;
  k = cp->edit_ctr;
  prepare_edit_list(cp, len + num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, NULL, cp->edits[k], cp, &cb, origin);
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
      datai = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      cb[ED_SND] = add_sound_file_to_edit_list(cp, inserted_file, datai,
					       MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(chan)]),
					       hdr, auto_delete, chan);
      ed = cp->edits[cp->edit_ctr];
      ed->sfnum = PACK_EDIT(INSERTION_EDIT, cb[ED_SND]);
      lock_affected_mixes(cp, beg, beg + num);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
    }
  else
    {
      if (ss->catch_exists)
	ERROR(NO_SUCH_FILE,
	      LIST_2(TO_SCM_STRING(origin),
		     TO_SCM_STRING(ss->catch_message)));
    }
}

void insert_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, const char *origin)
{
  int k, len;
  int *cb;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if (beg >= len)
    {
      extend_with_zeros(cp, len, beg - len + 1, "(insert-extend)");
      len = current_ed_samples(cp);
    }
  k = cp->edit_ctr;
  prepare_edit_list(cp, len + num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg, num, vals, cp->edits[k], cp, &cb, origin);
  reflect_edit_history_change(cp);
  lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
}

static ed_list *delete_samples_1(int beg, int num, ed_list *current_state, chan_info *cp, const char *origin)
{
  int len, k, need_to_delete, curbeg, old_out, cbi, start_del, len_fixup;
  int *cb, *temp_cb;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  len = current_state->size;
  len_fixup = -1;
  k = find_split_loc(beg, current_state);
  need_to_delete = num;
  start_del = k;
  curbeg = beg;
  cb = (int *)(current_state->fragments + k * ED_SIZE);
  if (cb[ED_OUT]>beg) start_del--;
  new_state = append_ed_list(cp, len + 1);
  copy_ed_blocks(new_state->fragments, current_state->fragments, 0, 0, start_del);
  cbi = start_del;
  temp_cb = (int *)(current_state->fragments + start_del * ED_SIZE);
  old_out = temp_cb[ED_OUT];
  if (beg > old_out)
    {
      cb = (int *)(new_state->fragments + start_del * ED_SIZE);
      cb[ED_OUT] = old_out;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_BEG];
      cb[ED_END] = temp_cb[ED_BEG] + beg - old_out - 1;
      cb[ED_SCL] = temp_cb[ED_SCL];
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
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
      temp_cb = (int *)(current_state->fragments + cbi * ED_SIZE);
      cb = (int *)(new_state->fragments + start_del * ED_SIZE);
      cb[ED_OUT] = beg;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_END] + 1 + need_to_delete;
      cb[ED_END] = temp_cb[ED_END];
      cb[ED_SCL] = temp_cb[ED_SCL];
      start_del++;
      len_fixup++;
    }
  cbi++;
  copy_ed_blocks(new_state->fragments, current_state->fragments, start_del, cbi, len - cbi); /* ??? */
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  new_state->sfnum = PACK_EDIT(DELETION_EDIT, 0);
  ripple_out(new_state->fragments, start_del, -num, len + len_fixup);
  ripple_marks(cp, beg, -num);
  ripple_mixes(cp, beg, -num);
  ripple_selection(new_state, beg, -num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  new_state->size = len+len_fixup; /* don't propogate useless trailing blocks */
  
  if (FRAGMENT_SOUND(new_state, (new_state->size - 1)) != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size - 1; k > 0; k--)
	if (FRAGMENT_SOUND(new_state, k) == EDIT_LIST_END_MARK) break;
      if (k > 0) 
	new_state->size = k + 1; 
    }
  if (new_state->size == 1)
    {
      /* if cut all, no data remains, so (I think) it is legit to have a one-length fragment = end_mark */
      /* but just to be safe, I'll copy that mark */
      new_state->size = 2;
      if (FRAGMENT_LOCAL_POSITION(new_state, 0) < 0) FRAGMENT_LOCAL_POSITION(new_state, 0) = 0;
      if (FRAGMENT_GLOBAL_POSITION(new_state, 0) < 0) FRAGMENT_GLOBAL_POSITION(new_state, 0) = 0;
      FRAGMENT_SOUND(new_state, 1) = EDIT_LIST_END_MARK;
      FRAGMENT_LOCAL_POSITION(new_state, 1) = FRAGMENT_LOCAL_POSITION(new_state, 0);
      FRAGMENT_GLOBAL_POSITION(new_state, 1) = FRAGMENT_GLOBAL_POSITION(new_state, 0);
      FRAGMENT_LOCAL_END(new_state, 1) = FRAGMENT_LOCAL_END(new_state, 0);
      FRAGMENT_SCALER(new_state, 1) = FRAGMENT_SCALER(new_state, 0);
    }

  return(new_state);
}    

void delete_samples(int beg, int num, chan_info *cp, const char *origin)
{
  int k, len;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if ((beg < len) && (beg >= 0))
    {
      if ((beg + num) > len) num = len - beg;
      k = cp->edit_ctr;
      prepare_edit_list(cp, len - num);
      cp->edits[cp->edit_ctr] = delete_samples_1(beg, num, cp->edits[k], cp, origin);
      reflect_edit_history_change(cp);
      lock_affected_mixes(cp, beg, beg + num);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
    }
  else
    {
      if (num == 1)
	report_in_minibuffer_and_save(cp->sound, "can't delete sample %d (current len=%d)", beg, len);
      else report_in_minibuffer_and_save(cp->sound, "can't delete samples %d to %d (current len=%d)", beg, beg + num - 1, len);
    }
}

static ed_list *change_samples_1(int beg, int num, MUS_SAMPLE_TYPE *vals, 
				 ed_list *current_state, chan_info *cp, 
				 int **cb_back, int lengthen, const char *origin)
{
  int len, k, start_del, cbi, curbeg, len_fixup, need_to_delete, old_out;
  ed_list *new_state;
  int *cb, *temp_cb;
  float one = 1.0;
  if (num <= 0) return(NULL);
  len = current_state->size;
  len_fixup = -1;
  k = find_split_loc(beg, current_state);
  need_to_delete = num - lengthen;
  start_del = k;
  curbeg = beg;
  cbi = 0;
  cb = (int *)(current_state->fragments + k * ED_SIZE);
  if (cb[ED_OUT]>beg) start_del--;
  new_state = append_ed_list(cp, len + 2);
  copy_ed_blocks(new_state->fragments, current_state->fragments, 0, 0, start_del);
  cbi = start_del;
  temp_cb = (int *)(current_state->fragments + start_del * ED_SIZE);  
  old_out = temp_cb[ED_OUT];
  if (beg > old_out)
    {
      cb = (int *)(new_state->fragments + start_del * ED_SIZE);
      cb[ED_OUT] = old_out;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_BEG];
      cb[ED_END] = temp_cb[ED_BEG] + beg - old_out - 1;
      cb[ED_SCL] = temp_cb[ED_SCL];
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
      old_out = FRAGMENT_GLOBAL_POSITION(current_state, (cbi + 1));
      need_to_delete -= (old_out - curbeg);
      if (need_to_delete > 0)
	{
	  cbi++;
	  curbeg = old_out;
	}
    }
  cb = (int *)(new_state->fragments + start_del * ED_SIZE);
  if (vals) 
    {
      cb[ED_SND] = add_sound_buffer_to_edit_list(cp, vals, num); 
      new_state->sfnum = PACK_EDIT(CHANGE_EDIT, cb[ED_SND]);
    }
  else (*cb_back) = cb;
  cb[ED_OUT] = beg;
  cb[ED_BEG] = 0;
  cb[ED_END] = num - 1;
  cb[ED_SCL] = FLOAT_AS_INT(one);
  start_del++;
  len_fixup++;
  if (need_to_delete < 0)
    {
      temp_cb = (int *)(current_state->fragments + cbi * ED_SIZE);
      cb = (int *)(new_state->fragments + start_del * ED_SIZE);
      cb[ED_OUT] = beg + num;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_END] + 1 + need_to_delete;
      cb[ED_END] = temp_cb[ED_END];
      cb[ED_SCL] = temp_cb[ED_SCL];
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
  new_state->size = len + len_fixup; /* don't propogate useless trailing blocks */
  ripple_marks(cp, 0, 0);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state, current_state, len);
  return(new_state);
}    

void file_change_samples(int beg, int num, char *tempfile, 
			 chan_info *cp, int chan, int auto_delete, int lock, const char *origin)
{
  int k, prev_len, new_len;
  int *cb;
  int fd;
  int *datai;
  ed_list *ed;
  file_info *hdr;
  snd_state *ss;
  if (num <= 0) 
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) snd_remove(tempfile);
      if ((tempfile) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(tempfile, chan);
      return;
    }
  if (dont_edit(cp)) return;
  prev_len = current_ed_samples(cp);
  ss = cp->state;
  if (beg >= prev_len)
    {
      extend_with_zeros(cp, prev_len, beg - prev_len + 1, "(change-extend)");
      prev_len = current_ed_samples(cp);
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  k = cp->edit_ctr;
  prepare_edit_list(cp, new_len);
  cp->edits[cp->edit_ctr] = change_samples_1(beg, num, NULL, cp->edits[k], cp, &cb, new_len - prev_len, origin);
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
  ss->catch_message = NULL;
  hdr = make_file_info(tempfile, ss);
  if (hdr)
    {
      fd = snd_open_read(ss, tempfile);
      mus_file_open_descriptors(fd,
			       tempfile,
			       hdr->format,
			       mus_data_format_to_bytes_per_sample(hdr->format),
			       hdr->data_location,
			       hdr->chans,
			       hdr->type);
      during_open(fd, tempfile, SND_CHANGE_FILE);
      datai = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      cb[ED_SND] = add_sound_file_to_edit_list(cp, tempfile, datai,
					       MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(chan)]),
					       hdr, auto_delete, chan);
      ed = cp->edits[cp->edit_ctr];
      ed->sfnum = PACK_EDIT(CHANGE_EDIT, cb[ED_SND]);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
    }
  else
    {
      if (ss->catch_exists)
	ERROR(NO_SUCH_FILE,
	      LIST_2(TO_SCM_STRING(origin),
		     TO_SCM_STRING(ss->catch_message)));
    }
}

void file_override_samples(int num, char *tempfile, 
			   chan_info *cp, int chan, int auto_delete, int lock, const char *origin)
{
  int fd;
  ed_list *e;
  int *datai;
  file_info *hdr;
  snd_state *ss;
  if (num == 0) 
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) snd_remove(tempfile);
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
      datai = make_file_state(fd, hdr, chan, FILE_BUFFER_SIZE);
      e = initial_ed_list(0, num - 1);
      if (origin) e->origin = copy_string(origin);
      cp->edits[cp->edit_ctr] = e;
      if (lock == LOCK_MIXES) lock_affected_mixes(cp, 0, num);
      e->fragments[0 + ED_SND] = add_sound_file_to_edit_list(cp, tempfile, datai,
							     MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(chan)]),
							     hdr, auto_delete, chan);
      e->sfnum = PACK_EDIT(CHANGE_EDIT, FRAGMENT_SOUND(e, 0));
      reflect_edit_history_change(cp);
      reflect_sample_change_in_axis(cp);
      ripple_marks(cp, 0, 0);
      check_for_first_edit(cp);
      update_graph(cp, NULL);
      if (cp->mix_md) reflect_mix_edit(cp, origin);
    }
  else
    {
      if (ss->catch_exists)
	ERROR(NO_SUCH_FILE,
	      LIST_2(TO_SCM_STRING(origin),
		     TO_SCM_STRING(ss->catch_message)));
    }
}

void change_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, int lock, const char *origin)
{
  int k, prev_len, new_len;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  prev_len = current_ed_samples(cp);
  if (beg >= prev_len)
    {
      extend_with_zeros(cp, prev_len, beg - prev_len + 1, "(change-extend)");
      prev_len = current_ed_samples(cp);
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  k = cp->edit_ctr;
  prepare_edit_list(cp, new_len);
  cp->edits[cp->edit_ctr] = change_samples_1(beg, num, vals, cp->edits[k], cp, NULL, new_len - prev_len, origin);
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp, beg, beg + num);
  if (cp->mix_md) reflect_mix_edit(cp, origin);
}

void parse_tree_scale_by(chan_info *cp, Float scl)
{
  /* copy current ed-list and reset scalers */
  int len, pos, i;
  float ed_scl;
  ed_list *new_ed, *old_ed;
  len = current_ed_samples(cp);
  pos = cp->edit_ctr;
  old_ed = cp->edits[pos];
  prepare_edit_list(cp, len);
  new_ed = make_ed_list(cp->edits[pos]->size);
  cp->edits[cp->edit_ctr] = new_ed;
  memcpy((void *)(new_ed->fragments), (void *)(old_ed->fragments), new_ed->size * ED_SIZE * sizeof(int)); /* old_ed->size? */
  for (i = 0; i < new_ed->size; i++) 
    {
      ed_scl = (float)(scl * INT_AS_FLOAT(FRAGMENT_SCALER(new_ed, i)));
      FRAGMENT_SCALER(new_ed, i) = FLOAT_AS_INT(ed_scl);
    }
  new_ed->sfnum = PACK_EDIT(PARSED_EDIT, 0);
  new_ed->origin = mus_format("scale-by %.4f", scl);
  new_ed->beg = 0;
  new_ed->len = len;
  new_ed->size = old_ed->size;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_marks(cp, 0, 0);
  check_for_first_edit(cp);
  lock_affected_mixes(cp, 0, len);
  reflect_edit_history_change(cp);
}

void parse_tree_selection_scale_by(chan_info *cp, Float scl, int beg, int num)
{
  /* copy current ed-list and reset scalers */
  int len, pos, i;
  float ed_scl;
  ed_list *new_ed, *old_ed;
  len = current_ed_samples(cp);
  pos = cp->edit_ctr;
  old_ed = cp->edits[pos];
  prepare_edit_list(cp, len);
  new_ed = selected_ed_list(beg, beg + num - 1, old_ed);
  for (i = 0; i < new_ed->size; i++) 
    {
      if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
      if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	{
	  ed_scl = (float)(scl * INT_AS_FLOAT(FRAGMENT_SCALER(new_ed, i)));
	  FRAGMENT_SCALER(new_ed, i) = FLOAT_AS_INT(ed_scl);
	}
    }

  cp->edits[cp->edit_ctr] = new_ed;
  new_ed->sfnum = PACK_EDIT(PARSED_EDIT, 0);
  new_ed->origin = mus_format("section-scale-by %.4f %d %d", scl, beg, num);
  new_ed->beg = beg;
  new_ed->len = num;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  ripple_marks(cp, 0, 0);
  check_for_first_edit(cp);
  lock_affected_mixes(cp, beg, beg+num);
  reflect_edit_history_change(cp);
}

Float sample(int samp, chan_info *cp)
{ /* slow access */
  ed_list *current_state;
  snd_data *sd;
  int len, i, cb, true_cb, index;
  int *data;
  if (samp < 0) return(0.0);
  if (samp > cp->samples[cp->edit_ctr]) return(0.0);
  current_state = (ed_list *)(cp->edits[cp->edit_ctr]);
  data = current_state->fragments;
  len = current_state->size;
  for (i = 0, cb = 0; i < len; i++, cb += ED_SIZE)
    if (samp < data[cb + ED_OUT])
      {
	true_cb = cb-ED_SIZE;
	if (data[true_cb + ED_SND] == EDIT_LIST_END_MARK) return(0.0);
	index = data[true_cb + ED_BEG] + samp - data[true_cb + ED_OUT];
	sd = cp->sounds[data[true_cb + ED_SND]];
	if (sd->type == SND_DATA_BUFFER)
	  return(UNWRAP_SAMPLE(MUS_SAMPLE_TO_FLOAT(sd->buffered_data[index]),
			       data[true_cb + ED_SCL]));
	else return(UNWRAP_SAMPLE(MUS_SAMPLE_TO_FLOAT(snd_file_read_sample(sd, index, cp)),
				  data[true_cb + ED_SCL]));
      }
  return(0.0);
}

/* now for optimized sample access -- since everything goes through these lists, we want the access to be fast */


snd_fd *free_snd_fd_almost(snd_fd *sf)
{
  snd_data *sd;
  if (sf) 
    {
#if DEBUGGING
      if (sf->filename) {FREE(sf->filename); sf->filename = NULL;}
#endif
      sd = sf->current_sound;
      if (sd)
	{
	  sd->inuse = FALSE;
	  if (sd->copy == 1) sd = free_snd_data(sd); 
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

#if LONG_INT_P
  int current_location(snd_fd *sf) 
  {
    if (sf->eof) return(sf->end);
    return(sf->cb[ED_OUT] - sf->cb[ED_BEG] + sf->beg + (int)(((long)(sf->view_buffered_data) - (long)(sf->first)) >> 2));
  }
#else
  int current_location(snd_fd *sf) 
  {
    if (sf->eof) return(sf->end);
    return(sf->cb[ED_OUT] - sf->cb[ED_BEG] + sf->beg + (((int)(sf->view_buffered_data) - (int)(sf->first)) >> 2));
  }
#endif

#if DEBUGGING
snd_fd *init_sample_read_any_1(int samp, chan_info *cp, int direction, int edit_position, const char *caller)
#else
snd_fd *init_sample_read_any(int samp, chan_info *cp, int direction, int edit_position)
#endif
{
  snd_fd *sf;
  snd_info *sp;
  ed_list *ed;
  int len, i, k, ind0, ind1, indx, curlen;
  int *cb;
  snd_data *first_snd = NULL;
  if (cp->active == 0) return(NULL);
  if ((edit_position < 0) || (edit_position > cp->edit_size)) return(NULL);
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
#if DEBUGGING
  sf->caller = caller;
  sf->filename = copy_string(sp->filename);
  sf->edit_pos = edit_position;
#endif
  sf->initial_samp = samp;
  sf->direction = 0;
  sf->cp = cp;
  sf->scaler = MUS_SAMPLE_TO_FLOAT(1.0);
  sf->current_state = ed;
  if ((curlen <= 0) ||    /* no samples, not ed->len (delete->len = #deleted samps) */
      (samp < 0) ||       /* this should never happen */
      ((samp >= curlen) && (direction == READ_FORWARD)))
    {
      sf->eof = 1;
      sf->view_buffered_data = (MUS_SAMPLE_TYPE *)1;
      sf->last = (MUS_SAMPLE_TYPE *)0; /* can I get away with this?? -- we're trying to do something with an empty file here */
      sf->first = (MUS_SAMPLE_TYPE *)2;
      /* data > last and data < first are triggers to ignore data and try to move in the fragment list */
      sf->current_sound = NULL;
      sf->cbi = 0;
      return(sf);
    }
  if (samp >= curlen) samp = curlen - 1;
  len = ed->size;
  for (i = 0, k = 0; i < len; i++, k += ED_SIZE)
    {
      cb = (int *)(ed->fragments + k);
      if ((cb[ED_OUT] > samp) || 
	  (cb[ED_SND] == EDIT_LIST_END_MARK))
	{
	  sf->cb = (int *)(ed->fragments + k - ED_SIZE);
	  sf->cbi = i - 1;
	  ind0 = sf->cb[ED_BEG];
	  indx = sf->cb[ED_BEG] + samp - sf->cb[ED_OUT];
	  ind1 = sf->cb[ED_END];
	  sf->sounds = (snd_data **)(cp->sounds);
	  sf->scaler = MUS_SAMPLE_TO_FLOAT(INT_AS_FLOAT(sf->cb[ED_SCL]));
	  first_snd = sf->sounds[sf->cb[ED_SND]];
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
		first_snd = copy_snd_data(first_snd, cp, MIX_FILE_BUFFER_SIZE);
	      first_snd->inuse = TRUE;
	      sf->current_sound = first_snd;
#if DEBUGGING
	      if (first_snd)
		first_snd->caller = sf->caller;
#endif
	      if (direction == READ_FORWARD)
		file_buffers_forward(ind0, ind1, indx, sf, first_snd);
	      else file_buffers_back(ind0, ind1, indx, sf, first_snd);
	    }
	  else 
	    {
	      sf->current_sound = NULL;
	      sf->view_buffered_data = (MUS_SAMPLE_TYPE *)(first_snd->buffered_data + indx);
	      sf->first = (MUS_SAMPLE_TYPE *)(first_snd->buffered_data + ind0);
	      sf->last = (MUS_SAMPLE_TYPE *)(first_snd->buffered_data + ind1);
	      sf->eof = 1;
	    }
	  return(sf);
	}
    }
  if (sf) FREE(sf);
  return(NULL);
}

snd_fd *init_sample_read(int samp, chan_info *cp, int direction)
{
  return(init_sample_read_any(samp, cp, direction, cp->edit_ctr));
}

MUS_SAMPLE_TYPE previous_sound (snd_fd *sf) 
{
  int ind0, ind1, indx;
  snd_data *prev_snd;
  if (sf->eof)
    {
      if (sf->current_sound) 
	{
	  prev_snd = sf->current_sound; 
	  prev_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if (prev_snd->copy) prev_snd = free_snd_data(prev_snd);
	}
      if (sf->cbi == 0) return(MUS_SAMPLE_0); /* can't back up any further */
      sf->cbi--;
      /* now start in the final portion of this block (if a file) */
      sf->cb = (int *)((sf->current_state)->fragments + sf->cbi * ED_SIZE);
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      sf->scaler = MUS_SAMPLE_TO_FLOAT(INT_AS_FLOAT(sf->cb[ED_SCL]));
      prev_snd = sf->sounds[sf->cb[ED_SND]];

      if (prev_snd->type == SND_DATA_FILE)
	{
	  if (prev_snd->inuse) 
	    prev_snd = copy_snd_data(prev_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	  prev_snd->inuse = TRUE;
	  sf->current_sound = prev_snd;
#if DEBUGGING
	  prev_snd->caller = sf->caller;
#endif
	  file_buffers_back(ind0, ind1, ind1, sf, prev_snd);
	}
      else 
	{
	  sf->view_buffered_data = (MUS_SAMPLE_TYPE *)(prev_snd->buffered_data + ind1);
	  sf->first = (MUS_SAMPLE_TYPE *)(prev_snd->buffered_data + ind0);
	  sf->last = sf->view_buffered_data;
	  sf->eof = 1;
	}
    }
  else
    {
      /* back up in current file */
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      indx = sf->beg - 1;
      file_buffers_back(ind0, ind1, indx, sf, sf->current_sound);
    }
  return((MUS_SAMPLE_TYPE)(UNWRAP_SAMPLE(*sf->view_buffered_data--, sf->cb[ED_SCL])));
}

MUS_SAMPLE_TYPE next_sound (snd_fd *sf)
{
  int ind0, ind1, indx;
  snd_data *nxt_snd;
  if (sf->eof) /* a convenience -- we could figure this out from various pointers */
    {
      if (sf->current_sound) 
	{
	  nxt_snd = sf->current_sound; 
	  nxt_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if (nxt_snd->copy) nxt_snd = free_snd_data(nxt_snd);
	}
      if (sf->last == (MUS_SAMPLE_TYPE *)0) return(MUS_SAMPLE_0);
      sf->cbi++;
      if (sf->cbi >= (sf->current_state)->size)
	{
          sf->view_buffered_data = (MUS_SAMPLE_TYPE *)1;
	  sf->last = (MUS_SAMPLE_TYPE *)0;
	  return(MUS_SAMPLE_0);
	}
      sf->cb = (int *)((sf->current_state)->fragments + sf->cbi * ED_SIZE);
      if ((!(sf->cb)) || 
	  (sf->cb[ED_SND] == EDIT_LIST_END_MARK))
	{
          sf->view_buffered_data = (MUS_SAMPLE_TYPE *)1;
	  sf->last = (MUS_SAMPLE_TYPE *)0;
	  return(MUS_SAMPLE_0);
	}
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      sf->scaler = MUS_SAMPLE_TO_FLOAT(INT_AS_FLOAT(sf->cb[ED_SCL]));
      nxt_snd = sf->sounds[sf->cb[ED_SND]];
      if (nxt_snd->type == SND_DATA_FILE)
	{
	  if (nxt_snd->inuse)
	    nxt_snd = copy_snd_data(nxt_snd, sf->cp, MIX_FILE_BUFFER_SIZE);
	  nxt_snd->inuse = TRUE;
	  sf->current_sound = nxt_snd;
#if DEBUGGING
	  nxt_snd->caller = sf->caller;
#endif
	  file_buffers_forward(ind0, ind1, ind0, sf, nxt_snd);
	}
      else 
	{
	  sf->view_buffered_data = (MUS_SAMPLE_TYPE *)(nxt_snd->buffered_data + ind0);
	  sf->first = sf->view_buffered_data;
	  sf->last = (MUS_SAMPLE_TYPE *)(nxt_snd->buffered_data + ind1);
	  sf->eof = 1;
	}
    }
  else
    { 
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      indx = sf->end + 1;
      file_buffers_forward(ind0, ind1, indx, sf, sf->current_sound);
    }
  return((MUS_SAMPLE_TYPE)(UNWRAP_SAMPLE(*sf->view_buffered_data++, sf->cb[ED_SCL])));
}

MUS_SAMPLE_TYPE next_sample(snd_fd *sf)
{
  if (sf->view_buffered_data > sf->last)
    return(next_sound(sf));
  else return((MUS_SAMPLE_TYPE)(UNWRAP_SAMPLE(*sf->view_buffered_data++, sf->cb[ED_SCL])));
}

MUS_SAMPLE_TYPE previous_sample(snd_fd *sf)
{
  if (sf->view_buffered_data < sf->first)
    return(previous_sound(sf));
  else return((MUS_SAMPLE_TYPE)(UNWRAP_SAMPLE(*sf->view_buffered_data--, sf->cb[ED_SCL])));
}

int read_sample_eof (snd_fd *sf)
{
  if (sf->cb)
    return((sf->cb[ED_SND] == EDIT_LIST_END_MARK) || 
	   ((sf->cbi == 0) && (sf->view_buffered_data < sf->first)));
  else return(sf->view_buffered_data >= sf->last);
}


/* -------------------------------- EDITS -------------------------------- */


int open_temp_file(char *ofile, int chans, file_info *hdr, snd_state *ss)
{
  int ofd, len, err;
  len = snd_strlen(hdr->comment);
  if (!(mus_header_writable(hdr->type, hdr->format)))
    {
      hdr->type = default_output_type(ss);
      if (mus_header_writable(hdr->type, default_output_format(ss)))
	hdr->format = default_output_format(ss);
      else
	{
	  /* was default_output_* here, but that's for the user's output not ours */
	  hdr->type = MUS_NEXT;
	  hdr->format = MUS_OUT_FORMAT;
	}
    }
  err = snd_write_header(ss, ofile, hdr->type, hdr->srate, chans, 0, 0, hdr->format, hdr->comment, len, hdr->loops);
  if (err == -1) return(-1);
  if ((ofd = snd_reopen_write(ss, ofile)) == -1) return(-1);
  hdr->data_location = mus_header_data_location(); /* header might have changed size (aiff extras) */
  mus_file_open_descriptors(ofd,
			   ofile,
			   hdr->format,
			   mus_data_format_to_bytes_per_sample(hdr->format),
			   hdr->data_location,
			   chans,
			   hdr->type);
  mus_file_set_data_clipped(ofd, data_clipped(ss));
  mus_file_seek(ofd, hdr->data_location, SEEK_SET);
  return(ofd);
}

int close_temp_file(int ofd, file_info *hdr, long bytes, snd_info *sp)
{
  int kleft, kused;
  mus_header_update_with_fd(ofd, hdr->type, bytes);
  kleft = disk_kspace(ofd, hdr->name);
  if (kleft < 0)
    snd_error("close temp file: %s", strerror(errno));
  else
    {
      kused = bytes >> 10;
      if ((kused > kleft) && (sp))
	report_in_minibuffer_and_save(sp, "disk nearly full: used %d Kbytes in the last operation, leaving %d", kused, kleft);
    }
  return(mus_file_close(ofd));
}

int snd_make_file(char *ofile, int chans, file_info *hdr, snd_fd **sfs, int length, snd_state *ss)
{
  /* create ofile, fill it by following sfs, use hdr for srate/type/format decisions */
  /* used only in this file and snd-chn (for external temps, snd->temp) */
  int ofd;
  int i, j, len, datumb, reporting = 0, total = 0, err = 0;
  chan_info *cp = NULL;
  MUS_SAMPLE_TYPE **obufs;
  err = MUS_NO_ERROR;
  ofd = open_temp_file(ofile, chans, hdr, ss);
  if (ofd == -1) return(MUS_CANT_OPEN_TEMP_FILE);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  obufs = (MUS_SAMPLE_TYPE **)MALLOC(chans * sizeof(MUS_SAMPLE_TYPE *));
  ss->stopped_explicitly = 0;
  for (i = 0; i < chans; i++)
    obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));
  j = 0;
  reporting = (length > (10 * MAX_BUFFER_SIZE));
  if (reporting) 
    {
      cp = sfs[0]->cp;
      start_progress_report(cp->sound, NOT_FROM_ENVED);
    }
  if (chans == 1)
    {
      for (len = 0; len < length; len++)
	{
	  obufs[0][j] = next_sample(sfs[0]);
	  j++;
	  if (j == FILE_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, obufs);
	      j = 0;
	      if (err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(cp->sound, NULL, 1, 1, (Float)total / (Float)length, NOT_FROM_ENVED);
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
	{
	  for (i = 0; i < chans; i++)
	    obufs[i][j] = next_sample(sfs[i]);
	  j++;
	  if (j == FILE_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      if (err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(cp->sound, NULL, 1, 1, (Float)total / (Float)length, NOT_FROM_ENVED);
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
  char *ofile = NULL;
  int err = MUS_NO_ERROR, saved_errno = 0;
  snd_state *ss;
  int i, samples;
  chan_info *cp;
  axis_info *ap;
  snd_fd **sf;
  Float *axis_data;
  int *ffts, *waves;
  file_info *sphdr = NULL;
  ss = sp->state;
  if (dont_save(sp, NULL)) return(MUS_NO_ERROR);
  samples = current_ed_samples(sp->chans[0]);
  err = MUS_NO_ERROR;
  ofile = snd_tempnam(ss); 
  /* this will use user's TMPDIR if temp_dir(ss) is not set, else stdio.h's P_tmpdir else /tmp */
  axis_data = (Float *)CALLOC(4 * sp->nchans, sizeof(Float));
  ffts = (int *)CALLOC(sp->nchans, sizeof(int));
  waves = (int *)CALLOC(sp->nchans, sizeof(int));
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      ap = cp->axis;
      axis_data[(i * 4) + 0] = ap->x0;
      axis_data[(i * 4) + 1] = ap->x1;
      axis_data[(i * 4) + 2] = ap->y0;
      axis_data[(i * 4) + 3] = ap->y1;
      waves[i] = cp->graph_time_p;
      ffts[i] = cp->graph_transform_p;
    }
  sf = (snd_fd **)CALLOC(sp->nchans, sizeof(snd_fd *));
  for (i = 0; i < sp->nchans; i++)
    {
      sf[i] = init_sample_read(0, sp->chans[i], READ_FORWARD);
      if (sf[i] == NULL) err = MUS_ERROR;
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
      FREE(ffts);
      FREE(waves);
      FREE(axis_data);
      return(err);
    }
  sphdr->samples = samples * sp->nchans;
  collapse_marks(sp);
  for (i = 0; i < sp->nchans; i++)
    {
      /* why not free_chan_info here? */
      cp = sp->chans[i];
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

  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      set_axes(cp, 
	       axis_data[(i * 4) + 0], 
	       axis_data[(i * 4) + 1], 
	       axis_data[(i * 4) + 2],
	       axis_data[(i * 4) + 3]);
      update_graph(cp, NULL); /* get normalized state before messing with it */
      if (ffts[i]) fftb(cp, TRUE);
      if (!(waves[i])) waveb(cp, FALSE);
      reflect_edit_history_change(cp);
    }
  FREE(axis_data);
  FREE(waves);
  FREE(ffts);

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

int save_edits_without_display(snd_info *sp, char *new_name, int type, int format, int srate, char *comment, SCM edpos, const char *caller, int arg_pos)
{ 
  /* file save as menu option -- changed 19-June-97 to retain current state after writing */
  file_info *hdr, *ohdr;
  snd_state *ss;
  int i, err = MUS_NO_ERROR, frames = 0, pos;
  snd_fd **sf;
  chan_info *cp;
  ss = sp->state;
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
#if DEBUGGING
	      /* a memory leak (sf) here if snd_make_file throws an error */
	      sf[i]->caller = caller;
	      if (sf[i]->filename) FREE(sf[i]->filename);
	      sf[i]->filename = copy_string(new_name);
#endif
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
	  snd_error("save-edits: unknown header type?!? %d ", type);
	  return(MUS_UNSUPPORTED_HEADER_TYPE);
	}
    }
  else snd_error("save-edits: impossible data format?!? %d", format);
  return(MUS_UNSUPPORTED_DATA_FORMAT);
}

int save_channel_edits(chan_info *cp, char *ofile, SCM edpos, const char *caller, int arg_pos)
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
	report_in_minibuffer(sp, "save channel as temp: %s: %s)", nfile, strerror(errno));
      else err = move_file(nfile, ofile);
      reflect_save_as_in_edit_history(cp, ofile);
      snd_update(ss, sp);
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
  snd_info *sp;
  int old_ctr;
  old_ctr = cp->edit_ctr;
  sp = cp->sound;
  cp->edit_ctr = 0;
  reflect_edit_counter_change(cp);
  reflect_sample_change_in_axis(cp);
  if (selection_is_active())
    reflect_edit_with_selection_in_menu(); 
  else reflect_edit_without_selection_in_menu();
  update_graph(cp, NULL);
  if ((cp->mix_md) && (old_ctr != 0)) reflect_mix_edit(cp, "revert-sound");
  if (HOOKED(cp->undo_hook))
    g_c_run_progn_hook(cp->undo_hook, EMPTY_LIST, S_undo_hook);
}

void undo_edit(chan_info *cp, int count)
{
  snd_info *sp;
  if ((cp) && (cp->edit_ctr > 0) && (count != 0))
    {
      sp = cp->sound;
      cp->edit_ctr -= count; 
      if (cp->edit_ctr < 0) cp->edit_ctr = 0;
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
      update_graph(cp, NULL);
      if (cp->mix_md) reflect_mix_edit(cp, "undo");
      if (HOOKED(cp->undo_hook))
	g_c_run_progn_hook(cp->undo_hook, EMPTY_LIST, S_undo_hook);
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
  snd_info *sp;
  if (cp)
    {
      sp = cp->sound;
      cp->edit_ctr += count; 
      while ((cp->edit_ctr >= cp->edit_size) || 
	     (!(cp->edits[cp->edit_ctr]))) 
	cp->edit_ctr--;
      if (((cp->edit_ctr + 1) == cp->edit_size) || 
	  (!(cp->edits[cp->edit_ctr + 1]))) 
	reflect_no_more_redo_in_menu();
      if (cp->edit_ctr != 0) /* possibly a sync'd redo to chan that has no edits */
	{
	  reflect_file_change_in_label(cp);
	  reflect_redo_in_menu();
	  reflect_edit_counter_change(cp);
	  reflect_sample_change_in_axis(cp);
	  if (selection_is_active()) 
	    reflect_edit_with_selection_in_menu(); 
	  else reflect_edit_without_selection_in_menu();
	  update_graph(cp, NULL);
	  if (cp->mix_md) reflect_mix_edit(cp, "redo");
	}
      if (HOOKED(cp->undo_hook))
	g_c_run_progn_hook(cp->undo_hook, EMPTY_LIST, S_undo_hook);
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

static SCM g_display_edits(SCM snd, SCM chn)
{
  #define H_display_edits "(" S_display_edits " &optional snd chn) returns the current edit tree state"
  FILE *tmp = NULL;
  char *buf, *name;
  chan_info *cp;
  int fd;
  off_t len;
  snd_state *ss;
  SCM res;
  ASSERT_CHANNEL(S_display_edits, snd, chn, 1);
  cp = get_cp(snd, chn, S_display_edits);
  ss = get_global_state();
  name = snd_tempnam(ss);
  tmp = fopen(name, "w");
  if (tmp) display_edits(cp, tmp);
  if ((!tmp) || (fclose(tmp) != 0))
    {
      if (name) FREE(name);
      ERROR(CANNOT_SAVE,
	    LIST_3(TO_SCM_STRING(S_display_edits),
		   TO_SCM_STRING(name),
		   TO_SCM_STRING(strerror(errno))));
    }
  fd = mus_file_open_read(name);
  len = lseek(fd, 0L, SEEK_END);
  buf = (char *)CALLOC(len + 1, sizeof(char));
  lseek(fd, 0L, SEEK_SET);
  read(fd, buf, len);
  close(fd);
  if (remove(name) == -1)
    snd_error("can't remove %s: %s", name, strerror(errno));
  if (name) FREE(name);
  res = TO_SCM_STRING(buf);
  FREE(buf);
  return(res);
}

static SCM g_edit_fragment(SCM uctr, SCM snd, SCM chn)
{
  #define H_edit_fragment "(" S_edit_fragment " ctr &optional snd chn) returns the edit history entry at 'ctr' \
associated with snd's channel chn; this is a list (origin type start-sample samps)"

  chan_info *cp;
  ed_list *ed;
  int ctr;
  ASSERT_CHANNEL(S_edit_fragment, snd, chn, 2);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(uctr), uctr, ARG1, S_edit_fragment, "an integer");
  cp = get_cp(snd, chn, S_edit_fragment);
  ctr = TO_C_INT_OR_ELSE(uctr, cp->edit_ctr);
  if ((ctr < cp->edit_size) && 
      (ctr >= 0))
    {
      ed = cp->edits[ctr];
      if (ed) 
	return(LIST_4(TO_SCM_STRING(ed->origin),
		      TO_SCM_STRING(edit_names[EDIT_TYPE(ed->sfnum)]),
		      TO_SCM_INT(ed->beg),
		      TO_SCM_INT(ed->len)));
    }
  ERROR(NO_SUCH_EDIT,
	LIST_4(TO_SCM_STRING(S_edit_fragment),
	       snd, chn,
	       uctr));
  return(uctr);
}

static SCM g_edit_tree(SCM snd, SCM chn, SCM upos)
{
  #define H_edit_tree "(" S_edit_tree " snd chn pos) returns the edit lists '((global-pos data-num local-pos local-end scaler)...)"
  /* internal debugging (auto-test) aid -- return complete ed list at pos */
  int i, len, pos;
  chan_info *cp;
  ed_list *ed;
  SCM res;
  ASSERT_CHANNEL(S_edit_tree, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_tree);
  if (cp)
    {
      ASSERT_TYPE(INTEGER_IF_BOUND_P(upos), upos, ARG3, S_edit_tree, "an integer");
      pos = TO_C_INT_OR_ELSE(upos, cp->edit_ctr);
      ed = cp->edits[pos];
      if (ed) 
	{
	  res = EMPTY_LIST;
	  len = ed->size; /* fragments in this list */
	  for (i = len - 1; i >= 0; i--)
	    res = CONS(LIST_5(TO_SCM_INT(FRAGMENT_GLOBAL_POSITION(ed, i)),
			      TO_SCM_INT(FRAGMENT_SOUND(ed, i)),
			      TO_SCM_INT(FRAGMENT_LOCAL_POSITION(ed, i)),
			      TO_SCM_INT(FRAGMENT_LOCAL_END(ed, i)),
			      TO_SCM_DOUBLE(INT_AS_FLOAT(FRAGMENT_SCALER(ed, i)))),
		       res);
	  return(res);
	}
    }
  return(LIST_0);
}

/* ---------------- sample readers ---------------- */

static TAG_TYPE sf_tag;
int sf_p(SCM obj); /* currently for snd-ladspa.c */
int sf_p(SCM obj) {return(OBJECT_TYPE_P(obj, sf_tag));}
#define SAMPLE_READER_P(Obj) OBJECT_TYPE_P(Obj, sf_tag)

static SCM g_sf_p(SCM obj) 
{
  #define H_sf_p "(" S_sample_reader_p " obj) -> #t if obj is a sample-reader"
  return(TO_SCM_BOOLEAN(SAMPLE_READER_P(obj)));
}

snd_fd *get_sf(SCM obj); /* currently for snd-ladspa.c */
snd_fd *get_sf(SCM obj) {if (SAMPLE_READER_P(obj)) return((snd_fd *)OBJECT_REF(obj)); else return(NULL);}
#define TO_SAMPLE_READER(obj) ((snd_fd *)OBJECT_REF(obj))

static int print_sf(SCM obj, SCM port, scm_print_state *pstate) 
{
  char *desc, *name;
  chan_info *cp;
  snd_fd *fd;
  fd = get_sf(obj);
  if (fd == NULL)
    WRITE_STRING("#<sample-reader: null>", port);
  else
    {
      cp = fd->cp;
      if (fd->local_sp) 
	name = ((fd->local_sp)->hdr)->name;
      else
	{
	  if (cp) 
	    name = (cp->sound)->short_filename;
	  else name = "unknown source";
	}
      desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      if (fd->eof)
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s from %d, at eof>",
		     fd, name, fd->initial_samp);
      else mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<sample-reader %p: %s from %d, at %d>",
			fd, name, fd->initial_samp, current_location(fd));
      WRITE_STRING(desc, port); 
      FREE(desc);
    }
  return(1);
}

static FREE_OBJECT_TYPE free_sf(SCM obj) 
{
  snd_fd *fd = TO_SAMPLE_READER(obj); 
  snd_info *sp = NULL;
  if (fd) 
    {
      /* changed to reflect g_free_sample_reader 29-Oct-00 */
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd(fd);
      if (sp) completely_free_snd_info(sp);
    }
#if HAVE_RUBY
  return(NULL);
#else
  return(sizeof(snd_fd));
#endif
}

static SCM g_sample_reader_at_end(SCM obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_end_p " obj) -> #t if sample-reader has reached the end of its data"
  ASSERT_TYPE(SAMPLE_READER_P(obj), obj, ARGn, S_sample_reader_at_end_p, "a sample-reader");
  return(TO_SCM_BOOLEAN(read_sample_eof(TO_SAMPLE_READER(obj))));
}

static SCM g_sample_reader_home(SCM obj)
{
  #define H_sample_reader_home "(" S_sample_reader_home " obj) -> (list sound-index chan-num) associated with reader"
  snd_fd *fd = NULL;
  ASSERT_TYPE(SAMPLE_READER_P(obj), obj, ARGn, S_sample_reader_home, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  return(LIST_2(TO_SMALL_SCM_INT(fd->cp->sound->index),
		TO_SMALL_SCM_INT(fd->cp->chan)));
}

SCM g_c_make_sample_reader(snd_fd *fd)
{
  scm_done_malloc(sizeof(snd_fd));
  RETURN_NEW_OBJECT(sf_tag, fd, 0, free_sf);
}

static SCM g_make_sample_reader(SCM samp_n, SCM snd, SCM chn, SCM dir1, SCM pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sample_reader "(" S_make_sample_reader " &optional (start-samp 0) snd chn (dir 1) edit-position)\n\
returns a reader ready to access snd's channel chn's data starting at 'start-samp', going in direction 'dir' \
(-1 = backward), reading the version of the data indicated by 'edit-position' which defaults to the current version. \
snd can be a filename, a sound index number, or a list with a mix id number."

  snd_fd *fd = NULL;
  int chan;
  chan_info *cp;
  snd_state *ss;
  char *filename;
  snd_info *loc_sp = NULL;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_n), samp_n, ARG1, S_make_sample_reader, "a number");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, ARG4, S_make_sample_reader, "an integer");
  ss = get_global_state();
  if (STRING_P(snd))
    {
      ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(chn), chn, ARG3, S_make_sample_reader, "an integer or boolean");
      filename = TO_C_STRING(snd);
      if (mus_file_probe(filename))
	loc_sp = make_sound_readable(ss, filename, FALSE);
      else return(snd_no_such_file_error(S_make_sample_reader, snd));
      chan = TO_C_INT_OR_ELSE(chn, 0);
      if ((chan < 0) || 
	  (chan > loc_sp->nchans))
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
  fd = init_sample_read_any(TO_C_INT_OR_ELSE(samp_n, 0), 
			    cp, 
			    TO_C_INT_OR_ELSE(dir1, 1), 
                            to_c_edit_position(cp, pos, S_make_sample_reader, 5));
  if (fd)
    {
      fd->local_sp = loc_sp;
      scm_done_malloc(sizeof(snd_fd));
      RETURN_NEW_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(FALSE_VALUE);
}

static SCM g_make_region_sample_reader(SCM samp_n, SCM reg, SCM chn, SCM dir1)
{
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " &optional (start-samp 0) (region 0) chn (dir 1))\n\
returns a reader ready to access region's channel chn data starting at 'start-samp' going in direction 'dir'"

  snd_fd *fd = NULL;
  int reg_n, chn_n;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_n), samp_n, ARG1, S_make_sample_reader, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg), reg, ARG2, S_make_sample_reader, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chn), chn, ARG3, S_make_sample_reader, "an integer");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(dir1), dir1, ARG4, S_make_sample_reader, "an integer");

  reg_n = TO_C_INT_OR_ELSE(reg, 0);
  if (!(region_ok(reg_n))) 
    ERROR(NO_SUCH_REGION,
	  LIST_2(TO_SCM_STRING(S_make_region_sample_reader),
		 reg));
  chn_n = TO_C_INT_OR_ELSE(chn, 0);
  if (chn_n >= region_chans(reg_n)) 
    return(snd_no_such_channel_error(S_make_region_sample_reader, LIST_1(reg), chn));

  fd = init_region_read(get_global_state(), 
			TO_C_INT_OR_ELSE(samp_n, 0), 
			reg_n,
			chn_n,
			TO_C_INT_OR_ELSE(dir1, 1));
  if (fd)
    {
      scm_done_malloc(sizeof(snd_fd));
      RETURN_NEW_OBJECT(sf_tag, fd, 0, free_sf);
    }
  return(FALSE_VALUE);
}

static SCM g_next_sample(SCM obj)
{
  #define H_next_sample "(" S_next_sample " reader) -> next sample from reader"
  ASSERT_TYPE(SAMPLE_READER_P(obj), obj, ARGn, S_next_sample, "a sample-reader");
  return(TO_SCM_DOUBLE(next_sample_to_float(TO_SAMPLE_READER(obj))));
}

static SCM g_previous_sample(SCM obj)
{
  #define H_previous_sample "(" S_previous_sample " reader) -> previous sample from reader"
  ASSERT_TYPE(SAMPLE_READER_P(obj), obj, ARGn, S_previous_sample, "a sample-reader");
  return(TO_SCM_DOUBLE(previous_sample_to_float(TO_SAMPLE_READER(obj))));
}

static SCM g_free_sample_reader(SCM obj)
{
  #define H_free_sample_reader "(" S_free_sample_reader " reader) frees sample reader 'reader'"
  snd_fd *fd;
  snd_info *sp = NULL;
  ASSERT_TYPE(SAMPLE_READER_P(obj), obj, ARGn, S_free_sample_reader, "a sample-reader");
  fd = TO_SAMPLE_READER(obj);
  sp = fd->local_sp; 
  fd->local_sp = NULL;
  free_snd_fd_almost(fd);
  /* free_snd_fd looks at its snd_data field to see if the latter's copy flag is set,
   *   free_snd_info may free this snd_data structure (via free_sound_list), so we have to
   *   call free_snd_fd before free_snd_info
   */
  if (sp) completely_free_snd_info(sp);
  return(scm_return_first(FALSE_VALUE, obj));
}

typedef Float (*g_plug)(Float val);
typedef Float (*g_plug_env)(Float val, void *envp);

static SCM g_loop_samples(SCM reader, SCM proc, SCM calls, SCM origin, SCM environ)
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
  int num, i, j = 0, ofd, datumb, err = 0;
  snd_fd *sf;
  void *envp = NULL;
  file_info *hdr;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  ASSERT_TYPE(SAMPLE_READER_P(reader), reader, ARG1, S_loop_samples, "a sample-reader");
  ASSERT_TYPE(SND_WRAPPED(proc), proc, ARG2, S_loop_samples, "a wrapped object");
  ASSERT_TYPE(INTEGER_P(calls), calls, ARG3, S_loop_samples, "an integer");
  ASSERT_TYPE(STRING_P(origin), origin, ARG4, S_loop_samples, "a string");
  num = TO_C_INT(calls);
  sf = TO_SAMPLE_READER(reader);
  cp = sf->cp;
  ss = cp->state;
  if (BOUND_P(environ))
    {
      ASSERT_TYPE(SND_WRAPPED(environ), environ, ARG5, S_loop_samples, "a wrapped object");
      envp = (void *)SND_UNWRAP(environ);
      func_env = (g_plug_env)SND_UNWRAP(proc);
    }
  else
    {
      func = (g_plug)SND_UNWRAP(proc);
      envp = NULL;
    }
  ofile = snd_tempnam(ss);
  sp = (cp->sound);
  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, num, TO_C_STRING(origin));
  ofd = open_temp_file(ofile, 1, hdr, ss);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  data = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  idata = data[0];
  if (envp)
    {
      for (i = 0; i < num; i++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE((*func_env)(next_sample_to_float(sf), envp));
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
	  idata[j++] = MUS_FLOAT_TO_SAMPLE((*func)(next_sample_to_float(sf)));
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
  file_change_samples(sf->initial_samp, num, ofile, cp, 0, DELETE_ME, LOCK_MIXES, TO_C_STRING(origin));
  update_graph(cp, NULL); /* is this needed? */
  if (ofile) FREE(ofile);
  FREE(data[0]);
  FREE(data);
  return(FALSE_VALUE);
}


static SCM g_save_edit_history(SCM filename, SCM snd, SCM chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename &optional snd chn) saves snd channel's chn edit history in filename"
  FILE *fd;
  int i, j;
  snd_info *sp;
  chan_info *cp;
  char *mcf = NULL;
  snd_state *ss;
  ASSERT_TYPE(STRING_P(filename), filename, ARG1, S_save_edit_history, "a string");
  ASSERT_CHANNEL(S_save_edit_history, snd, chn, 2);
  mcf = mus_expand_filename(TO_C_STRING(filename));
  fd = fopen(mcf, "w");
  if (mcf) FREE(mcf);
  if (fd)
    {
      if ((INTEGER_P(chn)) && (INTEGER_P(snd)))
	{
	  cp = get_cp(snd, chn, S_save_edit_history);
	  edit_history_to_file(fd, cp);
	}
      else
	{
	  if (INTEGER_P(snd))
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
    ERROR(CANNOT_SAVE,
	  LIST_3(TO_SCM_STRING(S_save_edit_history),
		 filename,
		 TO_SCM_STRING(strerror(errno))));
  return(filename);
}

static SCM g_undo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_undo "("  S_undo " &optional (count 1) snd chn) undoes count edits in snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_undo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_undo);
  if (INTEGER_P(ed_n))
    undo_edit_with_sync(cp, 
			TO_C_INT(ed_n));
  else undo_edit_with_sync(cp, 1);
  return(TRUE_VALUE);
}

static SCM g_redo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_redo "("  S_redo " &optional (count 1) snd chn) redoes count edits in snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_redo, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_redo);
  if (INTEGER_P(ed_n))
    redo_edit_with_sync(cp, 
			TO_C_INT(ed_n));
  else redo_edit_with_sync(cp, 1);
  return(TRUE_VALUE);
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
  int one_edit, need_backup = 0;
  ed_list *ed;
  one_edit = (((int *)ptr)[chan_ctr] + 1);
  need_backup = (cp->edit_ctr > one_edit);      /* cp->edit_ctr will be changing, so save this */
  if (cp->edit_ctr >= one_edit)                 /* ">=" here because the origin needs to be set even if there were no extra edits */
    {
      while (cp->edit_ctr > one_edit) backup_edit_list(cp);
      if ((need_backup) && (cp->mixes)) backup_mix_list(cp, one_edit);
      if (as_one_edit_origin)
	{
	  ed = cp->edits[cp->edit_ctr];
	  if (ed)
	    {
	      if (ed->origin) FREE(ed->origin);
	      ed->origin = as_one_edit_origin;
	      reflect_edit_history_change(cp);
	    }
	}
      if (need_backup) prune_edits(cp, cp->edit_ctr + 1);
      update_graph(cp, NULL); 
    }
  chan_ctr++; 
  return(0);
}

static SCM g_as_one_edit(SCM proc, SCM origin)
{
  #define H_as_one_edit "(" S_as_one_edit " func &optional origin) runs func, collecting all edits into one from the edit historys' point of view"
  int chans;
  int *cur_edits;
  snd_state *ss;
  SCM result = FALSE_VALUE;
  char *errmsg;
  SCM errstr;
  errmsg = procedure_ok(proc, 0, S_as_one_edit, "edit", 1);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_as_one_edit, errstr, proc));
    }
  ss = get_global_state();
  chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
  if (chans > 0)
    {
      if (STRING_P(origin))
	as_one_edit_origin = copy_string(TO_C_STRING(origin)); /* not TO_NEW_C_STRING since we have to use FREE elsewhere */
      else as_one_edit_origin = NULL;
      cur_edits = (int *)CALLOC(chans, sizeof(int));
      chan_ctr = 0;
      map_over_chans(ss, init_as_one_edit, (void *)cur_edits); /* redo here can't make sense, can it? */
      result = CALL_0(proc, S_as_one_edit);
      chan_ctr = 0;
      map_over_chans(ss, finish_as_one_edit, (void *)cur_edits);
      FREE(cur_edits);
    }
  return(scm_return_first(result, proc, origin));
}

static SCM g_section_scale_by(SCM scl, SCM beg, SCM num, SCM snd, SCM chn)
{
  /* for saved state involving selection-scale-by (where there might not actually be a selection) */
  chan_info *cp;
  ASSERT_CHANNEL("section-scale-by", snd, chn, 4);
  cp = get_cp(snd, chn, "section-scale-by");
  parse_tree_selection_scale_by(cp,
				TO_C_DOUBLE(scl),
				TO_C_INT(beg),
				TO_C_INT(num));
  return(scl);
}

MUS_SAMPLE_TYPE *g_floats_to_samples(SCM obj, int *size, const char *caller, int position)
{
  MUS_SAMPLE_TYPE *vals = NULL;
  SCM *vdata;
  vct *v;
  int i, num = 0;
  SCM lst;
  if (LIST_P_WITH_LENGTH(obj, num))
    {
      if (num == 0) return(NULL);
      if (((*size) > 0) && (num > (*size))) 
	num = (*size);
      vals = (MUS_SAMPLE_TYPE *)MALLOC(num * sizeof(MUS_SAMPLE_TYPE));
      for (i = 0, lst = obj; i < num; i++, lst = CDR(lst)) 
	vals[i] = MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(CAR(lst)));
    }
  else
    {
      if (VECTOR_P(obj))
	{
	  num = VECTOR_LENGTH(obj); 
	  if (((*size) > 0) && (num > (*size)))
	    num = (*size);
	  vals = (MUS_SAMPLE_TYPE *)MALLOC(num * sizeof(MUS_SAMPLE_TYPE));
	  vdata = VECTOR_ELEMENTS(obj);
	  for (i = 0; i < num; i++) 
	    vals[i] = MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(vdata[i]));
	}
      else
	{
	  if (VCT_P(obj))
	    {
	      v = TO_VCT(obj);
	      num = v->length; 
	      if (((*size) > 0) && (num > (*size)))
		num = (*size);
	      vals = (MUS_SAMPLE_TYPE *)MALLOC(num * sizeof(MUS_SAMPLE_TYPE));
	      for (i = 0; i < num; i++) 
		vals[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
	    }
	  else ASSERT_TYPE(0, obj, position, caller, "a vct, vector, or list");
	}
    }
  (*size) = num;
  return(vals);
}

static SCM g_sample(SCM samp_n, SCM snd_n, SCM chn_n)
{
  #define H_sample "(" S_sample " samp &optional snd chn) -> sample samp in snd's channel chn (slow access -- use sample-readers for speed)"
  chan_info *cp;
  ASSERT_TYPE(NUMBER_P(samp_n), samp_n, ARG1, S_sample, "a number");
  ASSERT_CHANNEL(S_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_sample);
  return(TO_SCM_DOUBLE(sample(TO_C_INT_OR_ELSE(samp_n, 0), cp)));
}

static SCM g_set_sample(SCM samp_n, SCM val, SCM snd_n, SCM chn_n)
{
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  MUS_SAMPLE_TYPE ival[1];
  ASSERT_TYPE(NUMBER_P(samp_n), samp_n, ARG1, "set-" S_sample, "a number");
  ASSERT_TYPE(NUMBER_P(val), val, ARG2, "set-" S_sample, "a number");
  ASSERT_CHANNEL("set-" S_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, "set-" S_sample);
  ival[0] = MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(val));
  change_samples(TO_C_INT_OR_ELSE(samp_n, 0), 1, ival, cp, LOCK_MIXES, "set-" S_sample);
  update_graph(cp, NULL);
  return(val);
}

static SCM g_set_sample_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  if (NOT_BOUND_P(arg2))
    return(g_set_sample(UNDEFINED_VALUE, arg1, UNDEFINED_VALUE, UNDEFINED_VALUE));
  else
    {
      if (NOT_BOUND_P(arg3))
	return(g_set_sample(arg1, arg2, UNDEFINED_VALUE, UNDEFINED_VALUE));
      else 
	{
	  if (NOT_BOUND_P(arg4)) 
	    return(g_set_sample(arg1, arg3, arg2, UNDEFINED_VALUE)); 
	  else return(g_set_sample(arg1, arg4, arg2, arg3));
	}
    }
}

static SCM g_samples(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM edpos)
{
  #define H_samples "(" S_samples " &optional (start-samp 0) samps snd chn edit-position)\n\
returns a vector containing snd channel chn's samples starting a start-samp for samps samples; edit-position is the edit \
history position to read (defaults to current position)."

  chan_info *cp;
  snd_fd *sf;
  int i, len, beg, pos;
  SCM new_vect;
  SCM *vdata;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_0), samp_0, ARG1, S_samples, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samps), samps, ARG2, S_samples, "a number");
  ASSERT_CHANNEL(S_samples, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_samples);
  pos = to_c_edit_position(cp, edpos, S_samples, 5);
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, cp->samples[pos] - beg);
  new_vect = MAKE_VECTOR(len, TO_SCM_DOUBLE(0.0));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf)
    {
      vdata = VECTOR_ELEMENTS(new_vect);
      for (i = 0; i < len; i++) 
	vdata[i] = TO_SCM_DOUBLE(next_sample_to_float(sf));
      free_snd_fd(sf);
    }
  return(new_vect);
}

static SCM g_set_samples(SCM samp_0, SCM samps, SCM vect, SCM snd_n, SCM chn_n, SCM truncate, SCM edname, SCM infile_chan)
{
  #define H_set_samples "(" "set-" S_samples " start-samp samps data &optional snd chn truncate edname infile-chan)\n\
sets snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename)); \
start-samp can be beyond current data end if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end."

  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int len = 0, beg, curlen, override = 0, inchan = 0, delete_choice = DELETE_ME;
  char *fname, *caller;
  ASSERT_TYPE(NUMBER_P(samp_0), samp_0, ARG1, "set-" S_samples, "a number");
  ASSERT_TYPE(NUMBER_P(samps), samps, ARG2, "set-" S_samples, "a number");
  ASSERT_CHANNEL("set-" S_samples, snd_n, chn_n, 4);
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(truncate), truncate, ARG6, "set-" S_samples, "a boolean");
  cp = get_cp(snd_n, chn_n, "set-" S_samples);
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(infile_chan), infile_chan, ARG8, "set-" S_samples, "an integer");
  ASSERT_TYPE(NOT_BOUND_P(edname) || STRING_P(edname) || BOOLEAN_P(edname), edname, ARG7, "set-" S_samples, "a string");
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, 0);
  override = TRUE_P(truncate);
  if (STRING_P(edname))
    caller = TO_C_STRING(edname);
  else caller = "set-" S_samples;
  if (STRING_P(vect))
    {
      curlen = current_ed_samples(cp);
      fname = TO_C_STRING(vect);
      inchan = TO_C_INT_OR_ELSE(infile_chan, 0);
      if (BOUND_P(infile_chan)) 
	{
	  delete_choice = MULTICHANNEL_DELETION;
	  remember_redundant_temp(fname, inchan);
	}
      if ((beg == 0) && 
	  ((len > curlen) || override))
	file_override_samples(len, fname, cp, inchan, delete_choice, LOCK_MIXES, caller);
      else file_change_samples(beg, len, fname, cp, inchan, delete_choice, LOCK_MIXES, caller);
    }
  else
    {
      ivals = g_floats_to_samples(vect, &len, caller, 3);
      if (ivals)
	{
	  change_samples(beg, len, ivals, cp, LOCK_MIXES, caller);
	  FREE(ivals);
	}
    }
  update_graph(cp, NULL);
  return(vect);
}

static SCM g_vct2samples(SCM samp_0, SCM samps, SCM vect, SCM snd_n, SCM chn_n, SCM truncate, SCM edname, SCM infile_chan)
{
  #define H_vct2samples "(" S_vct2samples " start-samp samps data &optional snd chn truncate edname infile-chan)\n\
sets snd's channel chn's samples starting at start-samp for samps from data (a vct); \
start-samp can be beyond current data end if truncate is #t and start-samp is 0, the end of the file is set to match \
the new data's end.  start-samp can also be a vct, as can samps"

  vct *v;
  if (VCT_P(samp_0))
    {
      v = TO_VCT(samp_0);
      if (INTEGER_P(samps))
	return(g_set_samples(INTEGER_ZERO, samps, samp_0, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
      else return(g_set_samples(INTEGER_ZERO, TO_SCM_INT(v->length), samp_0, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
    }
  else
    {
      if ((INTEGER_P(samp_0)) && (VCT_P(samps)))
	{
	  v = TO_VCT(samps);
	  return(g_set_samples(samp_0, TO_SCM_INT(v->length), samps, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
	}
    }
  return(g_set_samples(samp_0, samps, vect, snd_n, chn_n, truncate, edname, infile_chan));
}

static SCM g_set_samples_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  /* (set! (samples start samps [snd chn trunc edname infilechan]) vect) */
  if (NOT_BOUND_P(arg4))
    return(g_set_samples(arg1, arg2, arg3, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
  else
    {
      if (NOT_BOUND_P(arg5))
	return(g_set_samples(arg1, arg2, arg4, arg3, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
      else 
	{
	  if (NOT_BOUND_P(arg6)) 
	    return(g_set_samples(arg1, arg2, arg5, arg3, arg4, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
	  else
	    {
	      if (NOT_BOUND_P(arg7)) 
		return(g_set_samples(arg1, arg2, arg6, arg3, arg4, arg5, UNDEFINED_VALUE, UNDEFINED_VALUE));
	      else
		{
		  if (NOT_BOUND_P(arg8)) 
		    return(g_set_samples(arg1, arg2, arg7, arg3, arg4, arg5, arg6, UNDEFINED_VALUE));
		  else return(g_set_samples(arg1, arg2, arg8, arg3, arg4, arg5, arg6, arg7));
		}
	    }
	}
    }
}

static SCM g_change_samples_with_origin(SCM samp_0, SCM samps, SCM origin, SCM vect, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int i, len, beg;
  SCM *vdata;
  ASSERT_TYPE(INTEGER_P(samp_0), samp_0, ARG1, S_change_samples_with_origin, "an integer");
  ASSERT_TYPE(INTEGER_P(samps), samps, ARG2, S_change_samples_with_origin, "an integer");
  ASSERT_TYPE(STRING_P(origin), origin, ARG3, S_change_samples_with_origin, "a string");
  ASSERT_TYPE((VECTOR_P(vect)) || (STRING_P(vect)), vect, ARG4, S_change_samples_with_origin, "a vector or a string");
  ASSERT_CHANNEL(S_change_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_change_samples_with_origin);
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, 0);
  if (VECTOR_P(vect))
    {
      ivals = (MUS_SAMPLE_TYPE *)MALLOC(len * sizeof(MUS_SAMPLE_TYPE));
      vdata = VECTOR_ELEMENTS(vect);
#if SNDLIB_USE_FLOATS
      for (i = 0; i < len; i++) ivals[i] = TO_C_DOUBLE(vdata[i]);
#else
      for (i = 0; i < len; i++) ivals[i] = TO_C_INT_OR_ELSE(vdata[i], 0);
#endif
      change_samples(beg, len, ivals, cp, LOCK_MIXES, TO_C_STRING(origin));
      FREE(ivals);
    }
  else
    {
      /* string = filename here */
      file_change_samples(beg, len,
			  TO_C_STRING(vect),
			  cp, 0, 0, 1,
			  TO_C_STRING(origin));
    }
  update_graph(cp, NULL);
  return(scm_return_first(vect, origin));
}

static SCM g_insert_sound(SCM file, SCM ubeg, SCM file_chn, SCM snd_n, SCM chn_n)
{
  #define H_insert_sound "(" S_insert_sound " file &optional beg file-chan snd chn)\n\
inserts channel 'file-chan' of 'file' (or all chans file-chan not given) into snd's channel chn at beg or the cursor position"

  chan_info *cp;
  snd_info *sp;
  char *filename = NULL;
  int nc, len, fchn, beg = 0, i;
  ASSERT_TYPE(STRING_P(file), file, ARG1, S_insert_sound, "a string");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(ubeg), ubeg, ARG2, S_insert_sound, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(file_chn), file_chn, ARG3, S_insert_sound, "an integer");
  ASSERT_CHANNEL(S_insert_sound, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_sound);
  filename = mus_expand_filename(TO_C_STRING(file));
  nc = mus_sound_chans(filename);
  if (nc == -1)
    {
      if (filename) FREE(filename);
      return(snd_no_such_file_error(S_insert_sound, file));
    }
  len = mus_sound_samples(filename) / nc;
  if (NUMBER_P(ubeg))
    beg = TO_C_INT_OR_ELSE(ubeg, 0);
  else beg = cp->cursor;
  if (INTEGER_P(file_chn))
    {
      fchn = TO_C_INT(file_chn);
      if (fchn < mus_sound_chans(filename))
	{
	  file_insert_samples(beg, len, filename, cp, fchn, DONT_DELETE_ME, S_insert_sound);
	  update_graph(cp, NULL);
	  if (filename) FREE(filename);
	  return(TO_SCM_INT(len));
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
	  file_insert_samples(beg, len, filename, sp->chans[i], i, DONT_DELETE_ME, S_insert_sound);
	  update_graph(sp->chans[i], NULL);
	}
      if (filename) FREE(filename);
      return(TO_SCM_INT(len));
    }
  return(FALSE_VALUE); /* not reached */
}

static SCM g_delete_sample(SCM samp_n, SCM snd_n, SCM chn_n)
{
  #define H_delete_sample "(" S_delete_sample " samp &optional snd chn) deletes sample 'samp' from snd's channel chn"
  chan_info *cp;
  int samp;
  ASSERT_TYPE(NUMBER_P(samp_n), samp_n, ARG1, S_delete_sample, "a number");
  ASSERT_CHANNEL(S_delete_sample, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_delete_sample);
  samp = TO_C_INT_OR_ELSE(samp_n, 0);
  if ((samp >= 0) && (samp <= current_ed_samples(cp)))
    {
      delete_samples(samp, 1, cp, S_delete_sample);
      update_graph(cp, NULL);
      return(TRUE_VALUE);
    }
  ERROR(NO_SUCH_SAMPLE,
	LIST_4(TO_SCM_STRING(S_delete_sample),
	       samp_n,
	       snd_n, chn_n));
  return(samp_n);
}

static SCM g_delete_samples_1(SCM samp_n, SCM samps, SCM snd_n, SCM chn_n, const char *origin)
{
  chan_info *cp;
  ASSERT_TYPE(NUMBER_P(samp_n), samp_n, ARG1, origin, "a number");
  ASSERT_TYPE(NUMBER_P(samps), samps, ARG2, origin, "a number");
  ASSERT_CHANNEL(origin, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, origin);
  delete_samples(TO_C_INT_OR_ELSE(samp_n, 0),
		 TO_C_INT_OR_ELSE(samps, 0),
		 cp, origin);
  update_graph(cp, NULL);
  return(TRUE_VALUE);
}

static SCM g_delete_samples(SCM samp_n, SCM samps, SCM snd_n, SCM chn_n)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps &optional snd chn)\n\
deletes 'samps' samples from snd's channel chn starting at 'start-samp'"

  return(g_delete_samples_1(samp_n, samps, snd_n, chn_n, S_delete_samples));
}

static SCM g_delete_samples_with_origin(SCM samp_n, SCM samps, SCM origin, SCM snd_n, SCM chn_n)
{
  SCM res;
  ASSERT_TYPE(STRING_P(origin), origin, ARG3, S_delete_samples_with_origin, "a string");
  res = g_delete_samples_1(samp_n, samps, snd_n, chn_n, TO_C_STRING(origin));
  return(scm_return_first(res, origin));
}

static SCM g_insert_sample(SCM samp_n, SCM val, SCM snd_n, SCM chn_n)
{
  #define H_insert_sample "(" S_insert_sample " sample value &optional snd chn) inserts 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  int beg;
  MUS_SAMPLE_TYPE ival[1];
  ASSERT_TYPE(NUMBER_P(samp_n), samp_n, ARG1, S_insert_sample, "a number");
  ASSERT_TYPE(NUMBER_P(val), val, ARG2, S_insert_sample, "a number");
  ASSERT_CHANNEL(S_insert_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_sample);
  beg = TO_C_INT_OR_ELSE(samp_n, 0);
  if (beg < 0) 
    ERROR(NO_SUCH_SAMPLE,
	  LIST_4(TO_SCM_STRING(S_insert_sample),
		 samp_n,
		 snd_n, chn_n));
  ival[0] = MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(val));
  insert_samples(beg, 1, ival, cp, S_insert_sample);
  update_graph(cp, NULL);
  return(val);
}

static SCM g_insert_samples(SCM samp, SCM samps, SCM vect, SCM snd_n, SCM chn_n)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data &optional snd chn)\n\
inserts data (either a vector, vct, or list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int beg, len = 0;
  ASSERT_TYPE(NUMBER_P(samp), samp, ARG1, S_insert_samples, "a number");
  ASSERT_TYPE(NUMBER_P(samps), samps, ARG2, S_insert_samples, "a number");
  ASSERT_CHANNEL(S_insert_samples, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_insert_samples);
  beg = TO_C_INT_OR_ELSE(samp, 0);
  len = TO_C_INT_OR_ELSE(samps, 0);
  if (STRING_P(vect))
    {
      file_insert_samples(beg, len, TO_C_STRING(vect), cp, 0, DELETE_ME, S_insert_samples);
    }
  else
    {
      ivals = g_floats_to_samples(vect, &len, S_insert_samples, 3);
      if (ivals)
	{
	  insert_samples(beg, len, ivals, cp, S_insert_samples);
	  FREE(ivals);
	}
    }
  update_graph(cp, NULL);
  return(TO_SCM_INT(len));
}

static SCM g_insert_samples_with_origin(SCM samp, SCM samps, SCM origin, SCM vect, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int i, beg, len;
  SCM *vdata;
  ASSERT_TYPE(INTEGER_P(samp), samp, ARG1, S_insert_samples_with_origin, "an integer");
  ASSERT_TYPE(INTEGER_P(samps), samps, ARG2, S_insert_samples_with_origin, "an integer");
  ASSERT_TYPE(STRING_P(origin), origin, ARG3, S_insert_samples_with_origin, "a string");
  ASSERT_TYPE((VECTOR_P(vect)) || (STRING_P(vect)) || FALSE_P(vect), vect, ARG4, S_insert_samples_with_origin, "a vector or a string");
  ASSERT_CHANNEL(S_insert_samples_with_origin, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_insert_samples_with_origin);
  beg = TO_C_INT_OR_ELSE(samp, 0);
  len = TO_C_INT_OR_ELSE(samps, 0);
  if (VECTOR_P(vect))
    {
      ivals = (MUS_SAMPLE_TYPE *)MALLOC(len * sizeof(MUS_SAMPLE_TYPE));
      vdata = VECTOR_ELEMENTS(vect);
#if SNDLIB_USE_FLOATS
      for (i = 0; i < len; i++) ivals[i] = TO_C_DOUBLE(vdata[i]);
#else
      for (i = 0; i < len; i++) ivals[i] = TO_C_INT_OR_ELSE(vdata[i], MUS_SAMPLE_0);
#endif
      insert_samples(beg, len, ivals, cp, TO_C_STRING(origin));
      FREE(ivals);
    }
  else
    {
      if (STRING_P(vect))
	file_insert_samples(beg, len,
			    TO_C_STRING(vect),
			    cp, 0, 0,
			    TO_C_STRING(origin));
      else extend_with_zeros(cp, beg, len, TO_C_STRING(origin));
    }
  update_graph(cp, NULL);
  return(TO_SCM_INT(len));
}


static int dont_edit(chan_info *cp) 
{
  SCM res = FALSE_VALUE;
  if (HOOKED(cp->edit_hook))
    res = g_c_run_or_hook(cp->edit_hook, EMPTY_LIST, S_edit_hook);
  return(TRUE_P(res));
}

static SCM save_hook;
static int dont_save(snd_info *sp, char *newname)
{
  SCM res = FALSE_VALUE;
  if (HOOKED(save_hook))
    res = g_c_run_or_hook(save_hook,
			  LIST_2(TO_SMALL_SCM_INT(sp->index),
				 (newname) ? TO_SCM_STRING(newname) : FALSE_VALUE),
			  S_save_hook);
  return(TRUE_P(res));
}


#ifdef ARGIFY_1
ARGIFY_5(g_make_sample_reader_w, g_make_sample_reader)
ARGIFY_4(g_make_region_sample_reader_w, g_make_region_sample_reader)
NARGIFY_1(g_next_sample_w, g_next_sample)
NARGIFY_1(g_previous_sample_w, g_previous_sample)
NARGIFY_1(g_free_sample_reader_w, g_free_sample_reader)
NARGIFY_1(g_sample_reader_home_w, g_sample_reader_home)
NARGIFY_1(g_sf_p_w, g_sf_p)
NARGIFY_1(g_sample_reader_at_end_w, g_sample_reader_at_end)
ARGIFY_5(g_loop_samples_w, g_loop_samples)
ARGIFY_3(g_save_edit_history_w, g_save_edit_history)
ARGIFY_3(g_edit_fragment_w, g_edit_fragment)
ARGIFY_3(g_undo_w, g_undo)
ARGIFY_3(g_redo_w, g_redo)
ARGIFY_2(g_as_one_edit_w, g_as_one_edit)
ARGIFY_2(g_display_edits_w, g_display_edits)
ARGIFY_3(g_edit_tree_w, g_edit_tree)
ARGIFY_3(g_delete_sample_w, g_delete_sample)
ARGIFY_4(g_delete_samples_w, g_delete_samples)
ARGIFY_4(g_insert_sample_w, g_insert_sample)
ARGIFY_5(g_insert_samples_w, g_insert_samples)
ARGIFY_8(g_vct2samples_w, g_vct2samples)
ARGIFY_5(g_insert_sound_w, g_insert_sound)
NARGIFY_5(g_section_scale_by_w, g_section_scale_by)
ARGIFY_6(g_change_samples_with_origin_w, g_change_samples_with_origin)
ARGIFY_5(g_delete_samples_with_origin_w, g_delete_samples_with_origin)
ARGIFY_6(g_insert_samples_with_origin_w, g_insert_samples_with_origin)
ARGIFY_3(g_sample_w, g_sample)
ARGIFY_4(g_set_sample_w, g_set_sample)
ARGIFY_5(g_samples_w, g_samples)
ARGIFY_8(g_set_samples_w, g_set_samples)
#else
#define g_make_sample_reader_w g_make_sample_reader
#define g_make_region_sample_reader_w g_make_region_sample_reader
#define g_next_sample_w g_next_sample
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
#define g_insert_sound_w g_insert_sound
#define g_section_scale_by_w g_section_scale_by
#define g_change_samples_with_origin_w g_change_samples_with_origin
#define g_delete_samples_with_origin_w g_delete_samples_with_origin
#define g_insert_samples_with_origin_w g_insert_samples_with_origin
#define g_sample_w g_sample
#define g_set_sample_w g_set_sample
#define g_samples_w g_samples
#define g_set_samples_w g_set_samples
#endif

void g_init_edits(SCM local_doc)
{
#if HAVE_GUILE
  sf_tag = scm_make_smob_type("sf", sizeof(snd_fd));
  scm_set_smob_print(sf_tag, print_sf);
  scm_set_smob_free(sf_tag, free_sf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sf_tag, PROCEDURE g_next_sample, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  sf_tag = rb_define_class("SampleReader", rb_cObject);
#endif

  DEFINE_CONST(S_current_edit_position,      AT_CURRENT_EDIT_POSITION,             "current edit position indicator for 'edpos' args");

  DEFINE_PROC(S_make_sample_reader,        g_make_sample_reader_w, 0, 5, 0,        H_make_sample_reader);
  DEFINE_PROC(S_make_region_sample_reader, g_make_region_sample_reader_w, 0, 4, 0, H_make_region_sample_reader);
  DEFINE_PROC(S_next_sample,               g_next_sample_w, 1, 0, 0,               H_next_sample);
  DEFINE_PROC(S_previous_sample,           g_previous_sample_w, 1, 0, 0,           H_previous_sample);
  DEFINE_PROC(S_free_sample_reader,        g_free_sample_reader_w, 1, 0, 0,        H_free_sample_reader);
  DEFINE_PROC(S_sample_reader_home,        g_sample_reader_home_w, 1, 0, 0,        H_sample_reader_home);
  DEFINE_PROC(S_sample_reader_p,           g_sf_p_w, 1, 0, 0,                      H_sf_p);
  DEFINE_PROC(S_sample_reader_at_end_p,    g_sample_reader_at_end_w, 1, 0, 0,      H_sample_reader_at_end);
  DEFINE_PROC(S_loop_samples,              g_loop_samples_w, 4, 1, 0,              H_loop_samples);

  DEFINE_PROC(S_save_edit_history,         g_save_edit_history_w, 1, 2, 0,         H_save_edit_history);
  DEFINE_PROC(S_edit_fragment,             g_edit_fragment_w, 0, 3, 0,             H_edit_fragment);
  DEFINE_PROC(S_undo,                      g_undo_w, 0, 3, 0,                      H_undo);
  DEFINE_PROC(S_redo,                      g_redo_w, 0, 3, 0,                      H_redo);
  DEFINE_PROC(S_as_one_edit,               g_as_one_edit_w, 1, 1, 0,               H_as_one_edit);
  DEFINE_PROC(S_display_edits,             g_display_edits_w, 0, 2, 0,             H_display_edits);
  DEFINE_PROC(S_edit_tree,                 g_edit_tree_w, 0, 3, 0,                 H_edit_tree);

  DEFINE_PROC(S_delete_sample,             g_delete_sample_w, 1, 2, 0,             H_delete_sample);
  DEFINE_PROC(S_delete_samples,            g_delete_samples_w, 2, 2, 0,            H_delete_samples);
  DEFINE_PROC(S_insert_sample,             g_insert_sample_w, 2, 2, 0,             H_insert_sample);
  DEFINE_PROC(S_insert_samples,            g_insert_samples_w, 3, 2, 0,            H_insert_samples);
  DEFINE_PROC(S_vct2samples,               g_vct2samples_w, 1, 7, 0,               H_vct2samples);
  DEFINE_PROC(S_insert_sound,              g_insert_sound_w, 1, 4, 0,              H_insert_sound);

  /* semi-internal functions (restore-state) */
  DEFINE_PROC("section-scale-by",           g_section_scale_by_w, 5, 0, 0,          "internal scaling function");
  DEFINE_PROC(S_change_samples_with_origin, g_change_samples_with_origin_w, 4, 2, 0, "");
  DEFINE_PROC(S_delete_samples_with_origin, g_delete_samples_with_origin_w, 3, 2, 0, "");
  DEFINE_PROC(S_insert_samples_with_origin, g_insert_samples_with_origin_w, 4, 2, 0, "");

  define_procedure_with_reversed_setter(S_sample, PROCEDURE g_sample_w, H_sample,
					"set-" S_sample, PROCEDURE g_set_sample_w, PROCEDURE g_set_sample_reversed,
					local_doc, 1, 2, 1, 3);

  define_procedure_with_reversed_setter(S_samples, PROCEDURE g_samples_w, H_samples,
					"set-" S_samples, PROCEDURE g_set_samples_w, PROCEDURE g_set_samples_reversed,
					local_doc, 2, 3, 3, 5);

  #define H_save_hook S_save_hook " (snd name) is called each time a file is about to be saved. \
If it returns #t, the file is not saved.  'name' is #f unless \
the file is being saved under a new name (as in sound-save-as)."

  save_hook = MAKE_HOOK(S_save_hook, 2, H_save_hook);      /* arg = sound index, possible new name */
}
/* TODO: ask-before-overwrite could be handled by save-hook, but they seem to be checked at different times? */
