#ifndef SNDLIB2SCM_H
#define SNDLIB2SCM_H

/* error indications */

#define NO_SUCH_CHANNEL      TO_SCM_SYMBOL("no-such-channel")
#define NO_SUCH_SOUND        TO_SCM_SYMBOL("no-such-sound")
#define NO_SUCH_MARK         TO_SCM_SYMBOL("no-such-mark")
#define NO_SUCH_MIX          TO_SCM_SYMBOL("no-such-mix")
#define NO_SUCH_TRACK        TO_SCM_SYMBOL("no-such-track")
#define NO_SUCH_MENU         TO_SCM_SYMBOL("no-such-menu")
#define NO_SUCH_FILE         TO_SCM_SYMBOL("no-such-file")
#define NO_SUCH_REGION       TO_SCM_SYMBOL("no-such-region")
#define NO_SUCH_SAMPLE       TO_SCM_SYMBOL("no-such-sample")
#define NO_SUCH_ENVELOPE     TO_SCM_SYMBOL("no-such-envelope")
#define NO_SUCH_EDIT         TO_SCM_SYMBOL("no-such-edit")
#define CANNOT_SAVE          TO_SCM_SYMBOL("cannot-save")
#define CANNOT_PRINT         TO_SCM_SYMBOL("cannot-print")
#define IMPOSSIBLE_BOUNDS    TO_SCM_SYMBOL("impossible-bounds")
#define NO_ACTIVE_SELECTION  TO_SCM_SYMBOL("no-active-selection")
#define MUS_MISC_ERROR       TO_SCM_SYMBOL("mus-error")
#define NO_SUCH_AXIS_INFO    TO_SCM_SYMBOL("no-such-axis")
#define NO_SUCH_PLAYER       TO_SCM_SYMBOL("no-such-player")
#define NO_SUCH_COLOR        TO_SCM_SYMBOL("no-such-color")
#define NO_SUCH_WIDGET       TO_SCM_SYMBOL("no-such-widget")
#define NO_SUCH_AXIS_CONTEXT TO_SCM_SYMBOL("no-such-graphics-context")
#define BAD_ARITY            TO_SCM_SYMBOL("bad-arity")

#define NOT_TRUE_P(a)    (!(TRUE_P(a)))
#define NOT_FALSE_P(a)   (!(FALSE_P(a)))
#define NOT_NULL_P(a)    (!(NULL_P(a)))
#define BOOLEAN_IF_BOUND_P(Arg)   ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)))
#define INTEGER_IF_BOUND_P(Arg)   ((NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_IF_BOUND_P(Arg)    ((NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define STRING_IF_BOUND_P(Arg)    ((NOT_BOUND_P(Arg)) || (STRING_P(Arg)))
#define INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define NUMBER_OR_BOOLEAN_P(Arg)  ((BOOLEAN_P(Arg)) || (NUMBER_P(Arg)))
#define INTEGER_OR_BOOLEAN_P(Arg) ((BOOLEAN_P(Arg)) || (INTEGER_P(Arg)))

typedef struct {
  int length, chans;
  MUS_SAMPLE_TYPE **data;
} sound_data;

int sound_data_p(SCM obj);
SCM make_sound_data(int chans, int frames);
void mus_misc_error(const char *caller, char *msg, SCM val);

#if (!USE_SND)
  int to_c_int_or_else(SCM obj, int fallback, char *origin);
  void define_procedure_with_setter(char *get_name, SCM (*get_func)(), char *get_help,
	 			    char *set_name, SCM (*set_func)(), 
				    SCM local_doc,
				    int get_req, int get_opt, int set_req, int set_opt);
#endif

#endif
