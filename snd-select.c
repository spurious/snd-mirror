#include "snd.h"

/* selection support changed 11-Sep-00 to handle edit list movements */

static int cp_has_selection(chan_info *cp, void *ignore)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return((ed) && (ed->selection_beg != NO_SELECTION));
}

int selection_is_active(void)  /* REPLACE selection_is_current */
{
  /* is selection active in any channel */
  return(map_over_chans(get_global_state(),cp_has_selection,NULL));
}

int selection_is_active_in_channel(chan_info *cp) /* REPLACE selection_is_current_in_channel and active_selection */
{
  return((cp) && (cp_has_selection(cp,NULL)));
}

static int sp_has_selection(chan_info *cp, void *sp)
{
  return((cp_has_selection(cp,NULL)) && (cp->sound == (snd_info *)sp));
}

int selection_member(snd_info *sp) /* call it (and replace) selection_is_active_in_sound */
{
  return(map_over_chans(get_global_state(),sp_has_selection,(void *)sp));
}

static int selection_is_visible(chan_info *cp)
{
  ed_list *ed;
  axis_info *ap;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg == NO_SELECTION) return(0);
  ap = cp->axis;
  return((ed) && (ap->losamp < ed->selection_end) && (ap->hisamp > ed->selection_beg));
}

int selection_is_visible_in_channel (chan_info *cp) /* REPLACE active_selection */
{
  return((cp_has_selection(cp,NULL)) && (selection_is_visible(cp)));
}

int selection_beg(chan_info *cp) 
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return(ed->selection_beg); /* fallback? */
}

static int cp_selection_len(chan_info *cp, void *ptr)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    return(ed->selection_end - ed->selection_beg + 1); 
  return(0);
}

int selection_len(void)
{
  return(map_over_chans(cp_selection_len,NULL));
}

static int cp_delete_selection(chan_info *cp, void *origin)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      delete_samples(ed->selection_beg,cp->selection_len(cp,NULL),cp,(char *)origin);
      ed = cp->edits[cp->edit_ctr];
      ed->selection_beg = NO_SELECTION;
    }
  return(0);
}

static int delete_selection(char *origin, int regraph)
{
  if (selection_is_current())
    {
      map_over_chans(cp_delete_selection,(void *)origin);
      if (regraph == UPDATE_DISPLAY) map_over_chans(update_graph,NULL);
      reflect_edit_without_selection_in_menu();
      return(1);
    }
  return(0);
}

static int cp_deactivate_selection(chan_info *cp, void *ignore)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed) ed->selection_beg = NO_SELECTION;
}

void selection_off(chan_info *cp) /* deactivate_selection_in_channel */
{
  cp_deactivate_selection(cp,NULL);
}

void deactivate_selection(void)
{
  map_over_chans(get_global_state(),cp_deactivate_selection,NULL);
  map_over_chans(get_global_state(),update_graph,NULL);
  reflect_edit_without_selection_in_menu();
}

void ripple_selection(chan_info *cp, int beg, int num)
{
  /* beg=insert or delete begin point (snd-edits.c), num = samps inserted (num positive) or deleted (num negative) at beg */
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      if (beg < ed->selection_beg) 
	{
	  ed->selection_beg += num;
	  if (beg >= ed->selection_beg) 
	    ed->selection_beg = NO_SELECTION; /* deletion included some of current selection from outside */
	  else ed->selection_end += num;
	}
      else
	{
	  if (beg < ed->selection_end)
	    {
	      ed->selection_end += num;
	      if (ed->selection_end < beg)
		ed->selection_beg = NO_SELECTION; /* same as above but from end */
	    }
	}
    }
}

/* paste_selection? */
/* mix_selection? */



#if HAVE_GUILE
#include "sg.h"

static SCM g_cut(void)
{
  #define H_cut "(" S_cut ") cuts (deletes) the currently selected portion"
  finish_keyboard_selection();
  if (selection_is_current())
    {
      delete_selection(S_cut,UPDATE_DISPLAY);
      return(SCM_BOOL_T);
    }
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_cut))));
}

static SCM g_selectionQ(void)
{
  #define H_selectionQ "(" S_selectionQ ") -> #t if selection is currently active, visible, etc"
  RTNBOOL(selection_is_current());
}

static SCM g_selection_beg(void)
{
  #define H_selection_beg "(" S_selection_beg ") -> selection start samp in selected sound"
  if (selection_is_current())
    return(gh_int2scm(selection_beg(NULL)));
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_selection_beg))));
}

/* (catch 'no-active-selection (lambda () (+ 1 (selection-beg))) (lambda (tag val) 0)) */

static SCM g_selection_length(void)
{
  #define H_selection_length "(" S_selection_length ") -> length (frames) of selected portion"
  if (selection_is_current())
    return(gh_int2scm(selection_len()));
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_selection_length))));
}

static SCM g_selection_member(SCM snd, SCM chn)
{
  #define H_selection_member "(" S_selection_member " &optional snd chn) -> #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  ERRCP(S_selection_member,snd,chn,1);
  cp = get_cp(snd,chn,S_selection_member);
  if (selection_is_current_in_channel(cp))
    return(SCM_BOOL_T);
  return(SCM_BOOL_F);
}

void g_init_selection(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure(S_selection_beg,SCM_FNC g_selection_beg,0,0,0),H_selection_beg);
  DEFINE_PROC(gh_new_procedure(S_selection_length,SCM_FNC g_selection_length,0,0,0),H_selection_length);
  DEFINE_PROC(gh_new_procedure(S_selection_member,SCM_FNC g_selection_member,0,2,0),H_selection_member);
  DEFINE_PROC(gh_new_procedure(S_selectionQ,SCM_FNC g_selectionQ,0,0,0),H_selectionQ);
  DEFINE_PROC(gh_new_procedure(S_cut,SCM_FNC g_cut,0,0,0),H_cut);
}

#endif

