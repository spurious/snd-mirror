#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#if (!USE_SND)
#if HAVE_GUILE
  #include <guile/gh.h>
#endif
#if HAVE_LIBREP 
  #include <rep.h>
#endif
#if HAVE_MZSCHEME
  #include <scheme.h>
#endif
#if HAVE_RUBY
  #include <ruby.h>
#endif
#include "xen.h"
#endif

/* error indications */

#define NO_SUCH_CHANNEL      C_STRING_TO_XEN_SYMBOL("no-such-channel")
#define NO_SUCH_SOUND        C_STRING_TO_XEN_SYMBOL("no-such-sound")
#define NO_SUCH_MARK         C_STRING_TO_XEN_SYMBOL("no-such-mark")
#define NO_SUCH_MIX          C_STRING_TO_XEN_SYMBOL("no-such-mix")
#define NO_SUCH_TRACK        C_STRING_TO_XEN_SYMBOL("no-such-track")
#define NO_SUCH_MENU         C_STRING_TO_XEN_SYMBOL("no-such-menu")
#define NO_SUCH_FILE         C_STRING_TO_XEN_SYMBOL("no-such-file")
#define NO_SUCH_REGION       C_STRING_TO_XEN_SYMBOL("no-such-region")
#define NO_SUCH_SAMPLE       C_STRING_TO_XEN_SYMBOL("no-such-sample")
#define NO_SUCH_ENVELOPE     C_STRING_TO_XEN_SYMBOL("no-such-envelope")
#define NO_SUCH_EDIT         C_STRING_TO_XEN_SYMBOL("no-such-edit")
#define CANNOT_SAVE          C_STRING_TO_XEN_SYMBOL("cannot-save")
#define CANNOT_PRINT         C_STRING_TO_XEN_SYMBOL("cannot-print")
#define IMPOSSIBLE_BOUNDS    C_STRING_TO_XEN_SYMBOL("impossible-bounds")
#define NO_ACTIVE_SELECTION  C_STRING_TO_XEN_SYMBOL("no-active-selection")
#define MUS_MISC_ERROR       C_STRING_TO_XEN_SYMBOL("mus-error")
#define NO_SUCH_AXIS_INFO    C_STRING_TO_XEN_SYMBOL("no-such-axis")
#define NO_SUCH_PLAYER       C_STRING_TO_XEN_SYMBOL("no-such-player")
#define NO_SUCH_COLOR        C_STRING_TO_XEN_SYMBOL("no-such-color")
#define NO_SUCH_WIDGET       C_STRING_TO_XEN_SYMBOL("no-such-widget")
#define NO_SUCH_AXIS_CONTEXT C_STRING_TO_XEN_SYMBOL("no-such-graphics-context")
#define BAD_ARITY            C_STRING_TO_XEN_SYMBOL("bad-arity")

#define XEN_NOT_TRUE_P(a)    (!(XEN_TRUE_P(a)))
#define XEN_NOT_FALSE_P(a)   (!(XEN_FALSE_P(a)))
#define XEN_NOT_NULL_P(a)    (!(XEN_NULL_P(a)))
#define XEN_BOOLEAN_IF_BOUND_P(Arg)   ((XEN_BOOLEAN_P(Arg)) || (XEN_NOT_BOUND_P(Arg)))
#define XEN_INTEGER_IF_BOUND_P(Arg)   ((XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
#define XEN_NUMBER_IF_BOUND_P(Arg)    ((XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
#define XEN_STRING_IF_BOUND_P(Arg)    ((XEN_NOT_BOUND_P(Arg)) || (XEN_STRING_P(Arg)))
#define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((XEN_BOOLEAN_P(Arg)) || (XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
#define XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) ((XEN_BOOLEAN_P(Arg)) || (XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
#define XEN_NUMBER_OR_BOOLEAN_P(Arg)  ((XEN_BOOLEAN_P(Arg)) || (XEN_NUMBER_P(Arg)))
#define XEN_INTEGER_OR_BOOLEAN_P(Arg) ((XEN_BOOLEAN_P(Arg)) || (XEN_INTEGER_P(Arg)))

typedef struct {
  int length, chans;
  MUS_SAMPLE_TYPE **data;
} sound_data;

int sound_data_p(XEN obj);
XEN make_sound_data(int chans, int frames);
void mus_misc_error(const char *caller, char *msg, XEN val);

#if (!USE_SND)
  int to_c_int_or_else(XEN obj, int fallback, char *origin);
  void define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help,
	 			    char *set_name, XEN (*set_func)(), 
				    XEN local_doc,
				    int get_req, int get_opt, int set_req, int set_opt);
#endif

void mus_sndlib2xen_initialize (void);

#endif
