/* sndins.c -- Sndins for Snd
 *
 * Copyright (C) 2003--2005 Michael Scholz
 *
 * Author: Michael Scholz <scholz-micha@gmx.de>
 * Created: Sat Jun 07 02:24:46 CEST 2003
 * Last: Sun Jan 09 01:25:39 CET 2005
 *
 * This file is part of Sndins.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *
 * Commentary:
 * 
 * helper functions
 * fcomb functions (sndscm.html)
 * instrument functions
 *
 * static void init_keywords(void);
 * static mus_any *get_global_output(int kind);
 * static bool get_global_verbose(const char *caller);
 * static Float *xen_list2array(XEN list);
 * static int *xen_list2iarray(XEN list);
 * static Float *array2array(Float *list, int len);
 * static int *int_array2array(int *list, int len);
 * static int message(const char *fmt, ...);
 * static void sndins_error(const char *caller, const char *msg);
 * static int round2int(Float val);
 * static int feq(Float x, int i);
 * static bool prime_p(off_t x);
 * static char *format_array(Float *line, int size);
 * 
 * mus_any *mus_make_fcomb(Float scaler, int size, Float a0, Float a1);
 * int mus_fcomb_p(mus_any *ptr);
 * Float mus_fcomb(mus_any *ptr, Float input, Float ignored);
 * Float mus_fcomb_1(mus_any *ptr, Float input);
 * 
 * static char *describe_fcomb(mus_any *ptr);
 * static bool fcomb_equalp(mus_any *p1, mus_any *p2);
 * static off_t fcomb_length(mus_any *ptr);
 * static Float *fcomb_data(mus_any *ptr);
 * static Float fcomb_scaler(mus_any *ptr);
 * static Float set_fcomb_scaler(mus_any *ptr, Float val);
 * static int free_fcomb(mus_any *uptr);
 * static XEN c_make_fcomb(XEN args);
 * static XEN c_fcomb_p(XEN gen);
 * static XEN c_fcomb(XEN gen, XEN input);
 *
 * off_t ins_fm_violin(Float start, [...]);
 * off_t ins_jc_reverb(Float start, [...]);
 * off_t ins_nrev(Float start, [...]);
 * off_t ins_freeverb(Float start, [...]);
 * 
 * static XEN c_fm_violin(XEN args);
 * static XEN c_jc_reverb(XEN args);
 * static XEN c_nrev(XEN args);
 * static XEN c_freeverb(XEN args);
 *
 * void init_sndins(void);
 * void Init_libsndins(void);
 * void Init_sndins(void);
 * void init_libsndins(void);
 * 
 * Code:
 */

static const char rcsid[] = "$Id: sndins.c,v 1.28 2005/01/09 00:26:15 mike Exp $";

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif

#include <stdarg.h>
#include <math.h>
#include <sndlib.h>
#include <clm.h>
#include <xen.h>
#include <clm2xen.h>
#include <sndlib2xen.h>
#include <sndins.h>

enum {C_startime, C_duration, C_frequency, C_amplitude, C_amp_env, C_fm_index, C_reverb_amount,
      C_degree, C_distance, C_periodic_vibrato_rate, C_periodic_vibrato_amplitude,
      C_random_vibrato_rate, C_random_vibrato_amplitude, C_noise_freq, C_noise_amount,
      C_ind_noise_freq, C_ind_noise_amount, C_amp_noise_freq, C_amp_noise_amount,
      C_gliss_env, C_glissando_amount, C_fm1_env, C_fm2_env, C_fm3_env, C_fm1_rat, C_fm2_rat,
      C_fm3_rat, C_fm1_index, C_fm2_index, C_fm3_index, C_base, C_index_type,
      C_no_waveshaping,
      C_low_pass, C_volume, C_delay1, C_delay2, C_delay3, C_delay4, C_doubled,
      C_reverb_factor, C_lp_coeff, C_lp_out_coeff, C_output_scale,
      C_room_decay, C_damping, C_global, C_predelay, C_output_gain, C_output_mixer,
      C_scale_room_decay, C_offset_room_decay, C_combtuning, C_allpasstuning, C_scale_damping,
      C_stereo_spread,
      C_scaler, C_size, C_a0, C_a1,
      NKEYS};

static const char *keywords[NKEYS] =
{SC_startime, SC_duration, SC_frequency, SC_amplitude, SC_amp_env, SC_fm_index, SC_reverb_amount,
 SC_degree, SC_distance, SC_periodic_vibrato_rate, SC_periodic_vibrato_amplitude,
 SC_random_vibrato_rate, SC_random_vibrato_amplitude, SC_noise_freq, SC_noise_amount,
 SC_ind_noise_freq, SC_ind_noise_amount, SC_amp_noise_freq, SC_amp_noise_amount,
 SC_gliss_env, SC_glissando_amount, SC_fm1_env, SC_fm2_env, SC_fm3_env, SC_fm1_rat, SC_fm2_rat,
 SC_fm3_rat, SC_fm1_index, SC_fm2_index, SC_fm3_index, SC_base, SC_index_type,
 SC_no_waveshaping,
 SC_low_pass, SC_volume, SC_delay1, SC_delay2, SC_delay3, SC_delay4, SC_doubled,
 SC_reverb_factor, SC_lp_coeff, SC_lp_out_coeff, SC_output_scale,
 SC_room_decay, SC_damping, SC_global, SC_predelay, SC_output_gain, SC_output_mixer,
 SC_scale_room_decay, SC_offset_room_decay, SC_combtuning, SC_allpasstuning, SC_scale_damping,
 SC_stereo_spread,
 SC_scaler, SC_size, SC_a0, SC_a1};

static XEN allkeys[NKEYS];

/* clm2xen.c */
static XEN_OBJECT_TYPE mus_xen_tag;
#define MUS_XEN_P(obj) (XEN_OBJECT_TYPE_P(obj, mus_xen_tag))

static void
init_keywords(void)
{
    int i;

    for (i = 0; i < NKEYS; i++) allkeys[i] = XEN_MAKE_KEYWORD((char *)(keywords[i]));
}

enum {SX_OUTPUT, SX_REVERB};

static mus_any *
get_global_output(int kind)
{
    mus_any *out = NULL;

    switch (kind) {
    case SX_OUTPUT:
#if HAVE_RUBY
	if (!(XEN_NULL_P(rb_gv_get("$rbm_output")) || XEN_FALSE_P(rb_gv_get("$rbm_output"))))
	    out = XEN_TO_MUS_ANY(rb_gv_get("$rbm_output"));
#else
	if (!(XEN_FALSE_P(XEN_NAME_AS_C_STRING_TO_VALUE("*output*"))))
	    out = XEN_TO_MUS_ANY(XEN_NAME_AS_C_STRING_TO_VALUE("*output*"));
#endif	/* HAVE_RUBY */
	break;
    case SX_REVERB:
#if HAVE_RUBY
	if (!(XEN_NULL_P(rb_gv_get("$rbm_reverb")) || XEN_FALSE_P(rb_gv_get("$rbm_reverb"))))
	    out = XEN_TO_MUS_ANY(rb_gv_get("$rbm_reverb"));
#else
	if (!(XEN_FALSE_P(XEN_NAME_AS_C_STRING_TO_VALUE("*reverb*"))))
	    out = XEN_TO_MUS_ANY(XEN_NAME_AS_C_STRING_TO_VALUE("*reverb*"));
#endif	/* HAVE_RUBY */
	break;
    }
    return out;
}

static bool
get_global_verbose(const char *caller)
{
    bool verbose = false;
    
#if HAVE_RUBY
    if (XEN_BOUND_P(rb_gv_get("$rbm_verbose"))) {
	XEN_ASSERT_TYPE(XEN_BOOLEAN_P(rb_gv_get("$rbm_verbose")),
			rb_gv_get("$rbm_verbose"), NULL, caller, "a bool");
	verbose = XEN_TO_C_BOOLEAN(rb_gv_get("$rbm_verbose"));
    }
#else
    if (XEN_BOUND_P(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-verbose*"))) {
	XEN_ASSERT_TYPE(XEN_BOOLEAN_P(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-verbose*")),
			XEN_NAME_AS_C_STRING_TO_VALUE("*clm-verbose*"), NULL, caller, "a bool");
	verbose = XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-verbose*"));
    }
#endif	/* HAVE_RUBY */
    return verbose;
}

/*
 * Returns a Float array initialized with contents of XEN list.
 * (mus_make_env())
 */

static Float *
xen_list2array(XEN list)
{
    int i = 0;
    int len = XEN_LIST_LENGTH(list);
    Float *flist = NULL;
    XEN lst;

    if (len == 0) return NULL;
    if (!(flist = (Float *)CALLOC(len, sizeof(Float))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));
    for (i = 0, lst = XEN_COPY_ARG(list); i < len; i++, lst = XEN_CDR(lst))
	flist[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
    return flist;
}

static vct *
xen_list2vct(XEN list)
{
    int i = 0;
    int len = XEN_LIST_LENGTH(list);
    vct *v = c_make_vct(len);
    XEN lst;

    if (len == 0) return NULL;
    for (i = 0, lst = XEN_COPY_ARG(list); i < len; i++, lst = XEN_CDR(lst))
	v->data[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
    return v;
}

/*
 * Returns an int array initialized with contents of XEN list.
 * (mus_make_mixer())
 */

static int *
xen_list2iarray(XEN list)
{
    int i = 0;
    int len = XEN_LIST_LENGTH(list);
    int *ilist = NULL;
    XEN lst;

    if (len == 0) return NULL;
    if (!(ilist = (int *)CALLOC(len, sizeof(int))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));
    for (i = 0, lst = XEN_COPY_ARG(list); i < len; i++, lst = XEN_CDR(lst))
	ilist[i] = XEN_TO_C_INT_OR_ELSE(XEN_CAR(lst), 0);
    return ilist;
}

/*
 * Returns a Float array initialized with contents of Float list.
 * (mus_make_env())
 */

static Float *
array2array(Float *list, int len)
{
    int i = 0;
    Float *flist = NULL;

    if (len == 0) return NULL;
    if (!(flist = (Float *)CALLOC(len, sizeof(Float))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));
    for (i = 0; i < len; i++) flist[i] = (Float)list[i];
    return flist;
}

vct *
array2vct(Float *list, int len)
{
    int i = 0;
    vct *v = c_make_vct(len);

    for (i = 0; i < len; i++) v->data[i] = (Float)list[i];
    return v;
}

/*
 * Returns an int array initialized with contents of int list.
 * (mus_make_mixer())
 */

static int *
int_array2array(int *list, int len)
{
    int i = 0;
    int *ilist = NULL;

    if (len == 0) return NULL;
    if (!(ilist = (int *)CALLOC(len, sizeof(int))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));
    for (i = 0; i < len; i++) ilist[i] = (int)list[i];
    return ilist;
}

/*
 * To print a message in the listener or emacs buffer.
 */

static int
message(const char *fmt, ...)
{
    int result = 0;
    int len = strlen(fmt) + 100;
    char *str = NULL;
    va_list ap;

    if (!(str = (char *)CALLOC(len, sizeof(char))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));
    va_start(ap, fmt);
    result = vsnprintf(str, len, fmt, ap);
    va_end(ap);
    if (getenv("EMACS"))
	fprintf(stdout, "%s %s",
#if HAVE_RUBY
		"#",
#else
		";;",
#endif	/* HAVE_RUBY */
		str);
    else
	mus_print(str);
    FREE(str);
    return result;
}

static void
sndins_error(const char *caller, const char *msg)
{
    XEN_ERROR(MUS_MISC_ERROR, XEN_LIST_2(C_TO_XEN_STRING(caller), C_TO_XEN_STRING(msg)));
}

static int
round2int(Float val)
{
    return (int)floor(val);
}
    

static int
feq(Float x, int i)
{
    return(fabs(x - i) < 0.00001);
}

static bool
prime_p(off_t x)
{
    int i = 0;
    
    if (x == 2)
	return true;
    else if (x % 2) {
	for (i = 3; i < sqrt(x); i += 2)
	    if (!(x % i))
		return false;
	return true;
    }
    else
	return false;
}

static char *
format_array(Float *line, int size)
{
    int i, lim = size;
    char *str = (char *)CALLOC(1024, sizeof(char));
    char *tmp = (char *)CALLOC(32, sizeof(char));

    if (lim > mus_array_print_length())
	lim = mus_array_print_length();
    
    if (line) {
	mus_snprintf(str, 1024, "[");
	
	for (i = 0; i < lim - 1; i++) {
	    mus_snprintf(tmp, 32, "%.3f ", line[i]);
	    strcat(str, tmp);
	}

	mus_snprintf(tmp, 32, "%.3f%s]", line[i], (size > lim) ? "..." : "");
	strcat(str, tmp);
	FREE(tmp);
    }
    else
	mus_snprintf(str, 1024, "nil");

    return str;
}

/* --- fcomb --- */

/* sndscm.html */
static int MUS_FCOMB = 0; /* this will be our fcomb type identifier */

typedef struct {
    mus_any_class *core;
    int loc, size;
    Float *line;
    Float xscl, a0, a1, x1;
} fcomb;

/* each CLM-in-C generator has mus_any_class *core as the first thing in its structure.
 *   it defines most of the built-in "generic" functions like mus-describe.
 * The next set of functions implement the core functions/
 *   The address of the function is stored in the class's core struct.
 *   For example, the scaler method is defined as Float (*scaler)(void *ptr);
 *   in the mus_any_class declaration (clm.h); for fcomb it will correspond
 *   to the fcomb_scaler function below; it is invoked via mus_scaler(gen)
 *   where gen is an fcomb generator (the actual call is (*((gen->core)->scaler))(gen)).
 *   the core->scaler pointer (the function address) is set in the declaration
 *   of mus_any_class FCOMB_CLASS below.  If a method doesn't apply to a given
 *   generator class, just set its slot to 0.
 */

int
mus_fcomb_p(mus_any *ptr)
{
    return ((ptr) && ((ptr->core)->type == MUS_FCOMB));
}

static char *
describe_fcomb(mus_any *ptr) 
{
    char *desc = NULL, *s;
    fcomb *gen = (fcomb *)ptr;

    desc = (char *)CALLOC(1024, sizeof(char));
    if (desc) {
	if (mus_fcomb_p(ptr)) {
	    mus_snprintf(desc, 1024, "fcomb: scaler: %.3f, a0: %.3f, a1: %.3f, line[%d]: %s", 
			 gen->xscl, gen->a0, gen->a1, gen->size,
			 s = format_array(gen->line, gen->size));
	    FREE(s);
	}
	else
	    mus_snprintf(desc, 1024, "not an fcomb gen");
    }
    return desc;
}

static bool
fcomb_equalp(mus_any *p1, mus_any *p2)
{
    return (p1 == p2);
}

static off_t
fcomb_length(mus_any *ptr)
{
    return ((fcomb *)ptr)->size;
}

static Float *
fcomb_data(mus_any *ptr)
{
    return ((fcomb *)ptr)->line;
}

static Float
fcomb_scaler(mus_any *ptr)
{
    return ((fcomb *)ptr)->xscl;
}

static Float
set_fcomb_scaler(mus_any *ptr, Float val)
{
    ((fcomb *)ptr)->xscl = val;
    return val;
}

static int
free_fcomb(mus_any *uptr) 
{
    fcomb *ptr = (fcomb *)uptr;
    
    if (ptr) {
	if (ptr->line)
	    FREE(ptr->line);
	FREE(ptr); 
    }
    return 0;
}

/* now the actual run-time code executed by fcomb
   the extra "ignored" argument is for the run method */

Float
mus_fcomb(mus_any *ptr, Float input, Float ignored) 
{
    fcomb *gen = (fcomb *)ptr;
    Float tap_result, filter_result;
    
    tap_result = gen->line[gen->loc];
    filter_result = (gen->a0 * tap_result) + (gen->a1 * gen->x1);
    gen->x1 = tap_result;
    gen->line[gen->loc] = input + filter_result * gen->xscl;
    gen->loc++;
    if (gen->loc >= gen->size)
	gen->loc = 0;
    return tap_result;
}
Float
mus_fcomb_1(mus_any *ptr, Float input)
{
    return mus_fcomb(ptr, input, 0.0);
}

/* this is our core class descriptor */

static mus_any_class FCOMB_CLASS = {
    -1,                /* mus_type: this is assigned at run-time via mus_make_class_tag below */
    "fcomb",           /* mus_name: class name (used in descriptive/error messages) */
    &free_fcomb,       /* mus_free: free gen's struct etc */
    &describe_fcomb,   /* mus_describe: user-friendly description */
    &fcomb_equalp,     /* mus_equalp: check equality of fcomb gens */
    &fcomb_data,       /* mus_data: the fcomb delay line, a float array */
    0,                 /* mus_set_data: not implemented for fcomb */
    &fcomb_length,     /* mus_length: delay line length */
    0,                 /* mus_set_length: not implemented for fcomb */
    0,0,               /* mus_frequency, mus_set_frequency */
    0,0,               /* mus_phase, mus_set_phase */
    &fcomb_scaler,     /* mus_scaler: the feedback term */
    &set_fcomb_scaler, /* mus_set_scaler */
    0, 0,
    &mus_fcomb,        /* mus_run: the run-time fcomb function, MUS_RUN(gen) for speed */
    MUS_NOT_SPECIAL,   /* type extension */
    NULL,                     
    0,
    0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0
};

/* now a function to make a new generator */

mus_any *
mus_make_fcomb(Float scaler, int size, Float a0, Float a1)
{
    fcomb *gen = NULL;

    gen = (fcomb *)CALLOC(1, sizeof(fcomb));
    if (gen == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate struct for mus_make_fcomb!");
    else {
	gen->core = &FCOMB_CLASS;
	if (MUS_FCOMB == 0) {
	    MUS_FCOMB = mus_make_class_tag(); /* this gives us a unique fcomb type id */
	    gen->core->type = MUS_FCOMB;
        }
	gen->loc = 0;
	gen->xscl = scaler;
	gen->x1 = 0.0;
	gen->a0 = a0;
	gen->a1 = a1;
	gen->size = size;
	gen->line = (Float *)CALLOC(size, sizeof(Float));
	if (gen->line == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED, 
		      "can't allocate %d bytes for fcomb delay line in mus_make_fcomb!",
		      (int)(size * sizeof(Float)));
    }
    return (mus_any *)gen;
}

/* that is the end of the C side; the rest ties this generator into
   Guile/Ruby via the Xen package
   in Snd's case, it's actually not needed because the generator is
   only called from C */

#if HAVE_RUBY
#  define S_make_fcomb "make_fcomb"
#  define H_make_fcomb S_make_fcomb"(\
:scaler, 0.0, \
:size, 1, \
:a0, 0.0, \
:a1, 0.0): new fcomb gen"
#else
#  define S_make_fcomb "make-fcomb"
#  define H_make_fcomb "(" S_make_fcomb"\
 (:scaler 0.0)\
 (:size 1)\
 (:a0 0.0)\
 (:a1 0.0)): new fcomb gen"
#endif	/* HAVE_RUBY */

#define MAX_TABLE_SIZE (1024 * 1024 * 20)

static XEN
c_make_fcomb(XEN args)
{
    int i, keyn, argn = 0, vals = 0, last_key = 4, lst_len = XEN_LIST_LENGTH(args), size = 1;
    Float scaler = 0.0, a0 = 0.0, a1 = 0.0;
    XEN kargs[8], keys[4];
    int orig_arg[4] = {0};
    mus_xen *gn = NULL;

    keys[argn++] = allkeys[C_scaler];
    keys[argn++] = allkeys[C_size];
    keys[argn++] = allkeys[C_a0];
    keys[argn++] = allkeys[C_a1];

    for (i = 0; i < last_key * 2; i++) kargs[i] = XEN_UNDEFINED;
    for (i = 0; i < lst_len; i++) kargs[i] = XEN_LIST_REF(args, i);
    vals = mus_optkey_unscramble(S_make_fcomb, argn, keys, kargs, orig_arg);

    if (vals > 0) {
	keyn = 0;
	scaler = mus_optkey_to_float(keys[keyn], S_make_fcomb, orig_arg[keyn], scaler);
	keyn++;
	size = mus_optkey_to_int(keys[keyn], S_make_fcomb, orig_arg[keyn], size);
	if (size < 0)
	    XEN_OUT_OF_RANGE_ERROR(S_make_fcomb, orig_arg[keyn], keys[keyn], "size < 0?");
	if (size > MAX_TABLE_SIZE)
	    XEN_OUT_OF_RANGE_ERROR(S_make_fcomb, orig_arg[keyn], keys[keyn], "size too large");
	keyn++;
	a0 = mus_optkey_to_float(keys[keyn], S_make_fcomb, orig_arg[keyn], a0);
	keyn++;
	a1 = mus_optkey_to_float(keys[keyn], S_make_fcomb, orig_arg[keyn], a1);
    }

    if (!(gn = (mus_xen *)CALLOC(1, sizeof(mus_xen))))
	XEN_ERROR(NO_MEMORY, XEN_LIST_1(C_TO_XEN_STRING("cannot allocate memory")));

    gn->gen = mus_make_fcomb(scaler, size, a0, a1);
    gn->nvcts = 0;
    return mus_xen_to_object(gn);
}

#define S_fcomb_p "fcomb?"
#if HAVE_RUBY
#  define H_fcomb_p S_fcomb_p "(gen): true if gen is an fcomb generator"
#else
#  define H_fcomb_p "(" S_fcomb_p " gen): #t if gen is an fcomb generator"
#endif	/* HAVE_RUBY */

static XEN
c_fcomb_p(XEN gen)
{
    return C_TO_XEN_BOOLEAN((mus_xen_p(gen)) && (mus_fcomb_p(XEN_TO_MUS_ANY(gen))));
}

#define S_fcomb "fcomb"
#if HAVE_RUBY
#  define H_fcomb S_fcomb "(gen[, input = 0.0]): returns result of running fcomb gen"
#else
#  define H_fcomb "(" S_fcomb " gen (input 0.0)): returns result of running fcomb gen"
#endif	/* HAVE_RUBY */

static XEN
c_fcomb(XEN gen, XEN input)
{
    XEN_ASSERT_TYPE((mus_xen_p(gen)) && (mus_fcomb_p(XEN_TO_MUS_ANY(gen))),
		    gen, XEN_ARG_1, S_fcomb, "a fcomb");
    if (XEN_BOUND_P(input))
	XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_fcomb, "a number");
    return C_TO_XEN_DOUBLE(mus_fcomb(XEN_TO_MUS_ANY(gen),
				     XEN_TO_C_DOUBLE_OR_ELSE(input, 0.0), 0.0));
}
/* sndscm.html */

/* --- instruments --- */
#if HAVE_RUBY
#  define S_fm_violin "fm_violin"
#  define S_jc_reverb "jc_reverb"
#else
#  define S_fm_violin "fm-violin"
#  define S_jc_reverb "jc-reverb"
#endif /* HAVE_RUBY */
#define S_nrev "nrev"
#define S_freeverb "freeverb"

off_t
ins_fm_violin(Float start, Float dur, Float freq, Float amp, Float fm_index,
	      Float *amp_env, int amp_len,
	      Float periodic_vibrato_rate, Float periodic_vibrato_amp,
	      Float random_vibrato_rate, Float random_vibrato_amp,
	      Float noise_freq, Float noise_amount,
	      Float ind_noise_freq, Float ind_noise_amount,
	      Float amp_noise_freq, Float amp_noise_amount,
	      Float *gliss_env, int gliss_len, Float gliss_amount,
	      Float *fm1_env, int fm1_len, Float *fm2_env, int fm2_len, Float *fm3_env, int fm3_len,
	      Float fm1_rat, Float fm2_rat, Float fm3_rat,
	      Float fm1_index, Float fm2_index, Float fm3_index,
	      Float base, Float degree, Float distance, Float reverb_amount,
	      int index_type, bool no_waveshaping, mus_any *out, mus_any *rev,
	      mus_interp_t mode)
{
    off_t i, beg, len;
    int npartials = 0;
    bool vln = true, easy_case = false, modulate = false;
    Float frq_scl = 0.0, maxdev = 0.0, index1 = 0.0, index2 = 0.0, index3 = 0.0, vib = 0.0;
    Float logfrq = 0.0, sqrtfrq = 0.0, norm = 0.0, mod = 0.0;
    Float *coeffs = NULL, *partials = NULL;
    Float fuzz = 0.0, ind_fuzz = 1.0, amp_fuzz = 1.0;
    mus_any *os = NULL, *fmosc1 = NULL, *fmosc2 = NULL, *fmosc3 = NULL;
    mus_any *ampf = NULL, *indf1 = NULL, *indf2 = NULL, *indf3 = NULL, *pervib = NULL;
    mus_any *fm_noi = NULL, *ind_noi = NULL, *amp_noi = NULL, *ranvib = NULL, *frqf = NULL;
    mus_any *loc = NULL;

    beg = mus_seconds_to_samples(start);
    len = beg + mus_seconds_to_samples(dur);
    frq_scl = mus_hz_to_radians(freq);
    modulate = (fm_index == 0.0) ? false : true;
    maxdev = frq_scl * fm_index;
    logfrq = log(freq);
    sqrtfrq = sqrt(freq);
    if (fm1_index != 0.0)
	index1 = fm1_index;
    else {
	index1 = vln ? (maxdev * 5.0 / logfrq) : (maxdev * 7.5 / logfrq);
	if (index1 > M_PI) index1 = M_PI;
    }
    if (fm2_index != 0.0)
	index2 = fm2_index;
    else {
	index2 = vln ? (maxdev * 3.0 * (8.5 - logfrq) / (3.0 + freq * 0.001)) :
	    (maxdev * 3.0 * 15.0 / sqrtfrq);
	if (index2 > M_PI) index2 = M_PI;
    }
    if (fm3_index != 0.0)
	index3 = fm3_index;
    else {
	index3 = vln ? (maxdev * 4.0 / sqrtfrq) : (maxdev * 8.0 / sqrtfrq);
	if (index3 > M_PI) index3 = M_PI;
    }
    if ((noise_amount == 0.0) && (!no_waveshaping) &&
       (feq(fm1_rat, round2int(fm1_rat))) &&
       (feq(fm2_rat, round2int(fm2_rat))) &&
       (feq(fm3_rat, round2int(fm3_rat))))
	easy_case = 1;
    if (easy_case && modulate) {
	npartials = round2int(fm1_rat);
	if (round2int(fm2_rat) > npartials) npartials = round2int(fm2_rat);
	if (round2int(fm3_rat) > npartials) npartials = round2int(fm3_rat);
	npartials++;
	partials = (Float *)CALLOC(npartials, sizeof(Float));
	partials[(int)(fm1_rat)] = index1;
	partials[(int)(fm2_rat)] = index2;
	partials[(int)(fm3_rat)] = index3;
	coeffs = mus_partials_to_polynomial(npartials, partials, 1);
	norm = 1.0;
    }
    else
	norm = index1;
    os = mus_make_oscil(freq, 0.0);
    if (modulate) {
	fmosc1 = mus_make_oscil(freq * fm1_rat, 0.0);
	if (!easy_case) {
	    fmosc2 = mus_make_oscil(freq * fm2_rat, 0.0);
	    fmosc3 = mus_make_oscil(freq * fm3_rat, 0.0);
	}
    }
    ampf = mus_make_env(amp_env, amp_len / 2,
			amp, 0.0, base, dur, 0, 0, NULL);
    if (modulate) {
	indf1 = mus_make_env(fm1_env, fm1_len / 2,
			     norm, 0.0, 1.0, dur, 0, 0, NULL);
	if (!easy_case) {
	    indf2 = mus_make_env(fm2_env, fm2_len / 2,
				 index2, 0.0, 1.0, dur, 0, 0, NULL);
	    indf3 = mus_make_env(fm3_env, fm3_len / 2,
				 index3, 0.0, 1.0, dur, 0, 0, NULL);
	}
    }
    frqf = mus_make_env(gliss_env, gliss_len / 2,
			gliss_amount * frq_scl, 0.0, 1.0, dur, 0, 0, NULL);
    pervib = mus_make_triangle_wave(periodic_vibrato_rate, periodic_vibrato_amp * frq_scl, 0.0);
    ranvib = mus_make_rand_interp(random_vibrato_rate, random_vibrato_amp * frq_scl);
    if (noise_amount != 0.0)
	fm_noi = mus_make_rand(noise_freq, noise_amount * M_PI);
    if (ind_noise_amount != 0.0 && ind_noise_freq != 0.0)
	ind_noi = mus_make_rand_interp(ind_noise_freq, ind_noise_amount);
    if (amp_noise_amount != 0.0 && amp_noise_freq != 0.0)
	amp_noi = mus_make_rand_interp(amp_noise_freq, amp_noise_amount);
    if (degree == 0.0)
	degree = mus_random(90.0);
    loc = mus_make_locsig(degree, distance, reverb_amount, mus_channels(out), out, rev, mode);
    for (i = beg; i < len; i++) {
	if (noise_amount != 0.0) fuzz = mus_rand(fm_noi, 0.0);
	vib = mus_env(frqf) + mus_triangle_wave(pervib, 0.0) + mus_rand_interp(ranvib, 0.0);
	if (ind_noi) ind_fuzz = 1.0 + mus_rand_interp(ind_noi, 0.0);
	if (amp_noi) amp_fuzz = 1.0 + mus_rand_interp(amp_noi, 0.0);
	if (modulate && easy_case)
	    mod = mus_env(indf1) * mus_polynomial(coeffs, mus_oscil_1(fmosc1, vib), npartials);
	else
	    mod = mus_env(indf1) * mus_oscil_1(fmosc1, fuzz + fm1_rat * vib) + 
		mus_env(indf2) * mus_oscil_1(fmosc2, (fuzz + fm2_rat * vib)) + 
		mus_env(indf3) * mus_oscil_1(fmosc3, (fuzz + fm3_rat * vib));
	mus_locsig(loc, i, mus_env(ampf) * amp_fuzz * mus_oscil_1(os, vib + ind_fuzz * mod));
    }
    mus_free(pervib);
    mus_free(ranvib);
    mus_free(os);
    mus_free(fmosc1);
    mus_free(ampf);
    mus_free(indf1);
    mus_free(loc);
    if (fm_noi) mus_free(fm_noi);
    if (ind_noi) mus_free(ind_noi);
    if (amp_noi) mus_free(amp_noi);
    if (frqf) mus_free(frqf);
    if (!easy_case) {
	mus_free(indf2);
	mus_free(indf3);
	mus_free(fmosc2);
	mus_free(fmosc3);
    }
    else {
	FREE(partials);
    }
    return i;
}

off_t
ins_jc_reverb(Float start, Float dur, Float volume, bool low_pass, bool doubled,
	      Float delay1, Float delay2, Float delay3, Float delay4,
	      Float *amp_env, int amp_len, mus_any *out, mus_any *rev)
{
    off_t i, beg, len;
    int del_len = 0, chans = 0, rev_chans = 0;
    bool chan2 = false, chan4 = false;
    Float delA = 0.0, delB = 0.0, vol = 0.0;
    Float allpass_sum = 0.0, comb_sum = 0.0, comb_sum_1 = 0.0, comb_sum_2 = 0.0, all_sums = 0.0;
    mus_any *allpass1 = NULL, *allpass2 = NULL, *allpass3 = NULL;
    mus_any *comb1 = NULL, *comb2 = NULL, *comb3 = NULL, *comb4 = NULL;
    mus_any *outdel1 = NULL, *outdel2 = NULL, *outdel3 = NULL, *outdel4 = NULL;
    mus_any *env_a = NULL;

    beg = mus_seconds_to_samples(start);
    if (dur > 0.0)
	len = beg + mus_seconds_to_samples(dur);
    else
	len = beg + mus_seconds_to_samples(1.0) + mus_sound_frames(mus_file_name(rev));
    chans = mus_channels(out);
    rev_chans = mus_channels(rev);
    allpass1 = mus_make_all_pass(-0.7, 0.7, 1051, NULL, 1051, MUS_INTERP_LINEAR);
    allpass2 = mus_make_all_pass(-0.7, 0.7, 337, NULL, 337, MUS_INTERP_LINEAR);
    allpass3 = mus_make_all_pass(-0.7, 0.7, 113, NULL, 113, MUS_INTERP_LINEAR);
    comb1 = mus_make_comb(0.742, 4799, NULL, 4799, MUS_INTERP_LINEAR);
    comb2 = mus_make_comb(0.733, 4999, NULL, 4999, MUS_INTERP_LINEAR);
    comb3 = mus_make_comb(0.715, 5399, NULL, 5399, MUS_INTERP_LINEAR);
    comb4 = mus_make_comb(0.697, 5801, NULL, 5801, MUS_INTERP_LINEAR);
    if (chans > 1) chan2 = true;
    if (chans == 4) chan4 = true;
    del_len = mus_seconds_to_samples(delay1);
    outdel1 = mus_make_delay(del_len, NULL, del_len, MUS_INTERP_LINEAR);
    if (chan2) {
	del_len = mus_seconds_to_samples(delay2);
	outdel2 = mus_make_delay(del_len, NULL, del_len, MUS_INTERP_LINEAR);
    }
    if (doubled || chan4) {
	del_len = mus_seconds_to_samples(delay3);
	outdel3 = mus_make_delay(del_len, NULL, del_len, MUS_INTERP_LINEAR);
    }
    if (chan4 || (doubled && chan2)) {
	del_len = mus_seconds_to_samples(delay4);
	outdel4 = mus_make_delay(del_len, NULL, del_len, MUS_INTERP_LINEAR);
    }
    if (amp_env)
	env_a = mus_make_env(amp_env, amp_len / 2, volume, 0.0, 1.0, dur, 0, 0, NULL);
    if (doubled && chan4)
	sndins_error(S_jc_reverb, "is not set up for doubled reverb in quad");
    for (i = beg; i < len; i++) {
	int j;
	Float ho;

	for (j = 0, ho = 0.0; j < rev_chans; j++) ho += mus_in_any(i, j, rev);
	allpass_sum = mus_all_pass_1(allpass3,
				     mus_all_pass_1(allpass2,
						    mus_all_pass_1(allpass1, ho)));
	comb_sum_2 = comb_sum_1;
	comb_sum_1 = comb_sum;
	comb_sum = mus_comb_1(comb1, allpass_sum) +
	    mus_comb_1(comb2, allpass_sum) +
	    mus_comb_1(comb3, allpass_sum) +
	    mus_comb_1(comb4, allpass_sum);
	if (low_pass)
	    all_sums = 0.25 * (comb_sum + comb_sum_2) + 0.5 * comb_sum_1;
	else
	    all_sums = comb_sum;
	delA = mus_delay_1(outdel1, all_sums);
	if (doubled) delA += mus_delay_1(outdel3, all_sums);
	if (env_a) volume = mus_env(env_a);
	mus_out_any(i, delA * volume, 0, out);
	if (chan2) {
	    delB = mus_delay_1(outdel2, all_sums);
	    if (doubled) delB += mus_delay_1(outdel4, all_sums);
	    mus_out_any(i, delB * volume, 1, out);
	    if (chan4) {
		mus_out_any(i, volume * mus_delay_1(outdel3, all_sums), 2, out);
		mus_out_any(i, volume * mus_delay_1(outdel4, all_sums), 3, out);
	    }
	}
    }
    mus_free(allpass1);
    mus_free(allpass2);
    mus_free(allpass3);
    mus_free(comb1);
    mus_free(comb2);
    mus_free(comb3);
    mus_free(comb4);
    mus_free(outdel1);
    if (outdel2) mus_free(outdel2);
    if (outdel3) mus_free(outdel3);
    if (outdel4) mus_free(outdel4);
    if (env_a) mus_free(env_a);
    return i;
}

off_t
ins_nrev(Float start, Float dur, Float reverb_factor, Float lp_coeff, Float lp_out_coeff,
	 Float output_scale, Float volume, Float *amp_env, int amp_len,
	 mus_any *out, mus_any *rev)
{
    off_t i, beg, len;
    int chans = 0, rev_chans = 0, val = 0, env_len = 2;
    int dly_len[15] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19};
    Float srscale = mus_srate() / 25641.0;
    Float sample_a = 0.0, sample_b = 0.0, sample_c = 0.0, sample_d = 0.0;
    Float inrev = 0.0, outrev = 0.0;
    mus_any *allpass1 = NULL, *allpass2 = NULL, *allpass3 = NULL, *allpass4 = NULL;
    mus_any *allpass5 = NULL, *allpass6 = NULL, *allpass7 = NULL, *allpass8 = NULL;
    mus_any *comb1 = NULL, *comb2 = NULL, *comb3 = NULL;
    mus_any *comb4 = NULL, *comb5 = NULL, *comb6 = NULL;
    mus_any *low = NULL, *low_a = NULL, *low_b = NULL, *low_c = NULL, *low_d = NULL;
    mus_any *env_a = NULL;

    beg = mus_seconds_to_samples(start);
    if (dur > 0.0)
	len = beg + mus_seconds_to_samples(dur);
    else
	len = beg + mus_seconds_to_samples(1.0) + mus_sound_frames(mus_file_name(rev));
    chans = mus_channels(out);
    rev_chans = mus_channels(rev);
    env_a = mus_make_env(amp_env, amp_len / 2, output_scale, 0.0, 1.0, dur, beg, 0, NULL);
    for (i = 0; i < 14; i++) {
	int x = dly_len[i];
	val = round2int(srscale * x);
	if (!(val % 2)) val++;
	while (!prime_p(val)) val += 2;
	dly_len[i] = val;
    }
    i = 0;
    comb1 = mus_make_comb(reverb_factor * 0.822, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    comb2 = mus_make_comb(reverb_factor * 0.802, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    comb3 = mus_make_comb(reverb_factor * 0.773, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    comb4 = mus_make_comb(reverb_factor * 0.753, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    comb5 = mus_make_comb(reverb_factor * 0.753, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    comb6 = mus_make_comb(reverb_factor * 0.733, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i++;
    low = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
    low_a = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
    low_b = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
    low_c = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
    low_d = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
    allpass1 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass2 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass3 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    i += 2;
    allpass4 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass5 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass6 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass7 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR); i++;
    allpass8 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    for (i = beg; i < len; i++) {
	int j;
	Float ho;

	for (j = 0, ho = 0.0; j < rev_chans; j++) ho += mus_in_any(i, j, rev);
	inrev = volume * mus_env(env_a) * ho;
	outrev =
	    mus_all_pass_1(
		allpass4,
		mus_one_pole(
		    low,
		    mus_all_pass_1(
			allpass3,
			mus_all_pass_1(
			    allpass2,
			    mus_all_pass_1(
				allpass1,
				mus_comb_1(comb1, inrev) +
				mus_comb_1(comb2, inrev) +
				mus_comb_1(comb3, inrev) +
				mus_comb_1(comb4, inrev) +
				mus_comb_1(comb5, inrev) +
				mus_comb_1(comb6, inrev))))));
	sample_a = output_scale * mus_one_pole(low_a, mus_all_pass_1(allpass5, outrev));
	sample_b = output_scale * mus_one_pole(low_b, mus_all_pass_1(allpass6, outrev));
	sample_c = output_scale * mus_one_pole(low_c, mus_all_pass_1(allpass7, outrev));
	sample_d = output_scale * mus_one_pole(low_d, mus_all_pass_1(allpass8, outrev));
	if (chans == 2)
	    mus_out_any(i, (sample_a + sample_d) / 2.0, 0, out);
	else
	    mus_out_any(i, sample_a, 0, out);
	if ((chans == 2) || (chans == 4)) {
	    if (chans == 2)
		mus_out_any(i, (sample_b + sample_c) / 2.0, 1, out);
	    else
		mus_out_any(i, sample_b, 1, out);
	}
	if (chans == 4) {
	    mus_out_any(i, sample_c, 2, out);
	    mus_out_any(i, sample_d, 3, out);
	}
    }
    mus_free(allpass1);
    mus_free(allpass2);
    mus_free(allpass3);
    mus_free(allpass4);
    mus_free(allpass5);
    mus_free(allpass6);
    mus_free(allpass7);
    mus_free(allpass8);
    mus_free(comb1);
    mus_free(comb2);
    mus_free(comb3);
    mus_free(comb4);
    mus_free(comb5);
    mus_free(comb6);
    mus_free(env_a);
    mus_free(low);
    mus_free(low_a);
    mus_free(low_b);
    mus_free(low_c);
    mus_free(low_d);
    return i;
}

off_t
ins_freeverb(Float start, Float dur, Float room_decay, Float damping, Float global,
	     Float predelay, Float output_gain, Float scale_room_decay,
	     Float offset_room_decay, Float scale_damping, Float stereo_spread,
	     int *combtuning, int comb_len, int *allpasstuning, int all_len,
	     mus_any *output_mixer, mus_any *out, mus_any *rev)
{
    off_t i, beg, len;
    int out_chans = 0, in_chans = 0;
    Float srate_scale = mus_srate() / 44100.0;
    Float local_gain = 0.0, global_gain = 0.0;
    Float tmp;
    mus_any *out_mix = NULL, *out_buf = NULL, *f_out = NULL, *f_in = NULL;

    beg = mus_seconds_to_samples(start);
    if (dur > 0.0)
	len = beg + mus_seconds_to_samples(dur);
    else
	len = beg + mus_seconds_to_samples(1.0) + mus_sound_frames(mus_file_name(rev));
    out_chans = mus_channels(out);
    in_chans = mus_channels(rev);
    if (in_chans > 1 && in_chans != out_chans)
	sndins_error(S_freeverb,
		     "input must be mono or input channels must equal output channels");
    out_buf = mus_make_empty_frame(out_chans);
    f_out = mus_make_empty_frame(out_chans);
    f_in = mus_make_empty_frame(in_chans);
    local_gain = (1.0 - global) * (1.0 - 1.0 / out_chans) + 1.0 / out_chans;
    tmp = (out_chans * out_chans - out_chans);
    if (tmp < 1.0) tmp = 1.0;
    global_gain = (out_chans - local_gain * out_chans) / tmp;
    if (mus_mixer_p(output_mixer))
	out_mix = output_mixer;
    else {
	out_mix = mus_make_empty_mixer(out_chans);
	for (i = 0; i < out_chans; i++) {
	    int j;
	    
	    for (j = 0; j < out_chans; j++)
		mus_mixer_set(out_mix, i, j,
			      (Float)((output_gain *
				       ((i == j) ? local_gain : global_gain)) / out_chans));
	}
    }
    {
	mus_any **predelays = (mus_any **)CALLOC(in_chans, sizeof(mus_any *));
	mus_any ***allpasses = (mus_any ***)CALLOC(out_chans, sizeof(mus_any **));
	mus_any ***combs = (mus_any ***)CALLOC(out_chans, sizeof(mus_any **));
	Float room_decay_val = room_decay * scale_room_decay + offset_room_decay;
	int j, k, size;

	for (i = 0; i < out_chans; i++) {
	    allpasses[i] = (mus_any **)CALLOC(all_len, sizeof(mus_any *));
	    combs[i] = (mus_any **)CALLOC(comb_len, sizeof(mus_any *));
	}

	/* predelays */
	for (i = 0; i < in_chans; i++) {
	    size = round2int(mus_srate() * predelay);
	    predelays[i] = mus_make_delay(size, NULL, size, MUS_INTERP_LINEAR);
	}
	for (i = 0; i < out_chans; i++) {
	    /* comb filters */
	    for (j = 0; j < comb_len; j++) {
		Float dmp = scale_damping * damping;

		size = round2int(srate_scale * combtuning[j]);
		if (i % 2) size += round2int(srate_scale * stereo_spread);
		combs[i][j] = mus_make_fcomb(room_decay_val, size, 1.0 - dmp, dmp);
	    }
	    /* allpass filters */
	    for (j = 0; j < all_len; j++) {
		size = round2int(srate_scale * allpasstuning[j]);
		if (i % 2) size += round2int(srate_scale * stereo_spread);
		allpasses[i][j] = mus_make_all_pass(0.5, -1.0, size, NULL, size, MUS_INTERP_LINEAR);
	    }
	}
	/* run loop */
	for (i = beg; i < len; i++) {
	    f_in = mus_file_to_frame(rev, i, f_in);
	    if (in_chans > 1)
		for (j = 0; j < out_chans; j++) {
		    mus_frame_set(f_in, j, mus_delay_1(predelays[j], mus_frame_ref(f_in, j)));
		    mus_frame_set(f_out, j, 0.0);
		    for (k = 0; k < comb_len; k++)
			mus_frame_set(f_out, j,
				      mus_frame_ref(f_out, j) +
				      mus_fcomb_1(combs[j][k], mus_frame_ref(f_in, j)));
		}
	    else {
		mus_frame_set(f_in, 0, mus_delay_1(predelays[0], mus_frame_ref(f_in, 0)));
		for (j = 0; j < out_chans; j++) {
		    mus_frame_set(f_out, j, 0.0);
		    for (k = 0; k < comb_len; k++)
			mus_frame_set(f_out, j,
				      mus_frame_ref(f_out, j) +
				      mus_fcomb_1(combs[j][k], mus_frame_ref(f_in, 0)));
		}
	    }
	    for (j = 0; j < out_chans; j++)
		for (k = 0; k < all_len; k++)
		    mus_frame_set(f_out, j,
				  mus_all_pass_1(allpasses[j][k], mus_frame_ref(f_out, j)));
	    mus_frame_to_file(out, i, mus_frame_to_frame(out_mix, f_out, out_buf));
	} /* run loop */
	for (i = 0; i < in_chans; i++)
	    mus_free(predelays[i]);
	FREE(predelays);
	for (i = 0; i < out_chans; i++) {
	    for (j = 0; j < comb_len; j++)
		mus_free(combs[i][j]);
	    FREE(combs[i]);
	    for (j = 0; j < all_len; j++)
		mus_free(allpasses[i][j]);
	    FREE(allpasses[i]);
	}
	FREE(combs);
	FREE(allpasses);
    } /* block */
    if (!output_mixer) mus_free(out_mix);
    mus_free(out_buf);
    mus_free(f_out);
    mus_free(f_in);
    return i;
}

#if HAVE_RUBY
#  define H_fm_violin S_fm_violin "(\
:startime, 0.0, \
:duration, 1.0, \
:frequency, 440.0, \
:amplitude, 0.5, \
:fm_index, 1.0, \
:amp_env, [0, 0, 25, 1, 75, 1, 100, 0], \
:periodic_vibrato_rate, 5.0, \
:periodic_vibrato_amplitude, 0.0025, \
:random_vibrato_rate, 16.0, \
:random_vibrato_amplitude, 0.005, \
:noise_freq, 1000.0, \
:noise_amount, 0.0, \
:ind_noise_freq, 10.0, \
:ind_noise_amount, 0.0, \
:amp_noise_freq, 20.0, \
:amp_noise_amount, 0.0, \
:gliss_env, [0, 0, 100, 0], \
:glissando_amount, 0.0, \
:fm1_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0], \
:fm2_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0], \
:fm3_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0], \
:fm1_rat, 1.0, \
:fm2_rat, 3.0, \
:fm3_rat, 4.0, \
:fm1_index, 0.0, \
:fm2_index, 0.0, \
:fm3_index, 0.0, \
:base, 1.0, \
:degree, 0.0, \
:distance, 1.0, \
:reverb_amount, 0.01, \
:index_type, 1 [0 = cello, 1 = violin], \
:no_waveshaping, false)\n\
\n\
 require 'ws'\n\
 require 'sndins'\n\
\n\
 with_sound(:play, 1, :statistics, true, :reverb, :jc_reverb) do\n\
   fm_violin(0, 1, 440, 0.7, :fm_index, 1.3)\n\
 end"
#else
#  define H_fm_violin "(" S_fm_violin "\
 (:startime 0.0)\
 (:duration 1.0)\
 (:frequency 440.0)\
 (:amplitude 0.5)\
 (:fm-index 1.0)\
 (:amp-env '(0 0 25 1 75 1 100 0))\
 (:periodic-vibrato-rate 5.0)\
 (:periodic-vibrato-amplitude 0.0025)\
 (:random-vibrato-rate 16.0)\
 (:random-vibrato-amplitude 0.005)\
 (:noise-freq 1000.0)\
 (:noise-amount 0.0)\
 (:ind-noise-freq 10.0)\
 (:ind-noise-amount 0.0)\
 (:amp-noise-freq 20.0)\
 (:amp-noise-amount 0.0)\
 (:gliss-env '(0 0 100 0))\
 (:glissando-amount 0.0)\
 (:fm1-env '(0 1 25 0.4 75 0.6 100 0))\
 (:fm2-env '(0 1 25 0.4 75 0.6 100 0))\
 (:fm3-env '(0 1 25 0.4 75 0.6 100 0))\
 (:fm1-rat 1.0)\
 (:fm2-rat 3.0)\
 (:fm3-rat 4.0)\
 (:fm1-index 0.0)\
 (:fm2-index 0.0)\
 (:fm3-index 0.0)\
 (:base 1.0)\
 (:degree 0.0)\
 (:distance 1.0)\
 (:reverb-amount 0.01)\
 (:index-type 1 [0 = cello, 1 = violin])\
 (:no-waveshaping #f))\n\
\n\
 (load-from-path \"ws.scm\")\n\
 (load-extension \"libsndins\" \"init_sndins\")\n\
\n\
 (with-sound (:play #t :statistics #t :reverb jc-reverb)\n\
   (fm-violin 0 1 440 0.7 :fm-index 1.3))"
#endif /* HAVE_RUBY */

#define V_LAST_KEY 33
static XEN
c_fm_violin(XEN args)
{
    off_t result;
    int vals = 0, lst_len = XEN_LIST_LENGTH(args);
    int mode = MUS_INTERP_LINEAR;
    int amp_len = 8, gls_len = 4, fm1_len = 8, fm2_len = 8, fm3_len = 8;
    bool index_type = true, no_waveshaping = false;
    bool amp_del = false, gls_del = false, fm1_del = false, fm2_del = false, fm3_del = false;
    Float start = 0.0, dur = 1.0, freq = 440.0, amp = 0.5, fm_index = 1.0;
    Float tamp_env[8] = {0.0, 0.0, 25.0, 1.0, 75.0, 1.0, 100.0, 0.0};
    Float periodic_vibrato_rate = 5.0, periodic_vibrato_amp = 0.0025;
    Float random_vibrato_rate = 16.0, random_vibrato_amp = 0.005;
    Float noise_freq = 1000.0, noise_amount = 0.0;
    Float ind_noise_freq = 10.0, ind_noise_amount = 0.0;
    Float amp_noise_freq = 20.0, amp_noise_amount = 0.0;
    Float tgliss_env[4] = {0.0, 0.0, 100.0, 0.0}, gliss_amount = 0.0;
    Float tfm_env[8] = {0.0, 1.0, 25.0, 0.4, 75.0, 0.6, 100.0, 0.0};
    Float fm1_rat = 1.0, fm2_rat = 3.0, fm3_rat = 4.0;
    Float fm1_index = 0.0, fm2_index = 0.0, fm3_index = 0.0, base = 1.0;
    Float degree = 0.0, distance = 1.0, reverb_amount = 0.01;
    Float *amp_env = NULL, *gliss_env = NULL, *fm1_env = NULL, *fm2_env = NULL, *fm3_env = NULL;
    vct *tmp_env = NULL;
    mus_any *out = NULL, *rev = NULL;
    XEN kargs[V_LAST_KEY * 2], keys[V_LAST_KEY];
    int orig_arg[V_LAST_KEY] = {0};
    int i = 0;

    keys[i++] = allkeys[C_startime];
    keys[i++] = allkeys[C_duration];
    keys[i++] = allkeys[C_frequency];
    keys[i++] = allkeys[C_amplitude];
    keys[i++] = allkeys[C_fm_index];
    keys[i++] = allkeys[C_amp_env];
    keys[i++] = allkeys[C_periodic_vibrato_rate];
    keys[i++] = allkeys[C_periodic_vibrato_amplitude];
    keys[i++] = allkeys[C_random_vibrato_rate];
    keys[i++] = allkeys[C_random_vibrato_amplitude];
    keys[i++] = allkeys[C_noise_freq];
    keys[i++] = allkeys[C_noise_amount];
    keys[i++] = allkeys[C_ind_noise_freq];
    keys[i++] = allkeys[C_ind_noise_amount];
    keys[i++] = allkeys[C_amp_noise_freq];
    keys[i++] = allkeys[C_amp_noise_amount];
    keys[i++] = allkeys[C_gliss_env];
    keys[i++] = allkeys[C_glissando_amount];
    keys[i++] = allkeys[C_fm1_env];
    keys[i++] = allkeys[C_fm2_env];
    keys[i++] = allkeys[C_fm3_env];
    keys[i++] = allkeys[C_fm1_rat];
    keys[i++] = allkeys[C_fm2_rat];
    keys[i++] = allkeys[C_fm3_rat];
    keys[i++] = allkeys[C_fm1_index];
    keys[i++] = allkeys[C_fm2_index];
    keys[i++] = allkeys[C_fm3_index];
    keys[i++] = allkeys[C_base];
    keys[i++] = allkeys[C_degree];
    keys[i++] = allkeys[C_distance];
    keys[i++] = allkeys[C_reverb_amount];
    keys[i++] = allkeys[C_index_type];
    keys[i++] = allkeys[C_no_waveshaping];

    for (i = 0; i < V_LAST_KEY * 2; i++) kargs[i] = XEN_UNDEFINED;
    for (i = 0; i < lst_len; i++) kargs[i] = XEN_LIST_REF(args, i);
    vals = mus_optkey_unscramble(S_fm_violin, V_LAST_KEY, keys, kargs, orig_arg);

    if (vals > 0) {
	i = 0;
	start = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], start); i++;
	dur = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], dur); i++;
	freq = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], freq); i++;
	amp = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], amp); i++;
	fm_index = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm_index); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_fm_violin, "a list or vct");
	    if ((tmp_env = get_vct(keys[i]))) {
		amp_env = array2array(tmp_env->data, tmp_env->length);
		amp_len = tmp_env->length;
	    }
	    else {
		amp_env = xen_list2array(keys[i]);
		amp_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	periodic_vibrato_rate = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
						    periodic_vibrato_rate); i++;
	periodic_vibrato_amp = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
						   periodic_vibrato_amp); i++;
	random_vibrato_rate = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
						  random_vibrato_rate); i++;
	random_vibrato_amp = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
						 random_vibrato_amp); i++;
	noise_freq = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], noise_freq); i++;
	noise_amount = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], noise_amount); i++;
	ind_noise_freq = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
					     ind_noise_freq); i++;
	ind_noise_amount = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
					       ind_noise_amount); i++;
	amp_noise_freq = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
					     amp_noise_freq); i++;
	amp_noise_amount = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i],
					       amp_noise_amount); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_fm_violin, "a list or vct");
	    if ((tmp_env = get_vct(keys[i]))) {
		gliss_env = array2array(tmp_env->data, tmp_env->length);
		gls_len = tmp_env->length;
	    }
	    else {
		gliss_env = xen_list2array(keys[i]);
		gls_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	gliss_amount = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], gliss_amount); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_fm_violin, "a list or vct");
	    if ((tmp_env = get_vct(keys[i]))) {
		fm1_env = array2array(tmp_env->data, tmp_env->length);
		fm1_len = tmp_env->length;
	    }
	    else {
		fm1_env = xen_list2array(keys[i]);
		fm1_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_fm_violin, "a list or vct");
	    if ((tmp_env = get_vct(keys[i]))) {
		fm2_env = array2array(tmp_env->data, tmp_env->length);
		fm2_len = tmp_env->length;
	    }
	    else {
		fm2_env = xen_list2array(keys[i]);
		fm2_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_fm_violin, "a list or vct");
	    if ((tmp_env = get_vct(keys[i]))) {
		fm3_env = array2array(tmp_env->data, tmp_env->length);
		fm3_len = tmp_env->length;
	    }
	    else {
		fm3_env = xen_list2array(keys[i]);
		fm3_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	fm1_rat = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm1_rat); i++;
	fm2_rat = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm2_rat); i++;
	fm3_rat = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm3_rat); i++;
	fm1_index = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm1_index); i++;
	fm2_index = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm2_index); i++;
	fm3_index = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], fm3_index); i++;
	base = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], base); i++;
	degree = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], degree); i++;
	distance = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], distance); i++;
	reverb_amount = mus_optkey_to_float(keys[i], S_fm_violin, orig_arg[i], reverb_amount); i++;
	index_type = mus_optkey_to_int(keys[i], S_fm_violin, orig_arg[i], index_type); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i], orig_arg[i], S_fm_violin, "a bool");
	    no_waveshaping = XEN_TO_C_BOOLEAN(keys[i]);
	}
    }

    if (!amp_env) {
	amp_env = array2array(tamp_env, 8);
	amp_del = true;
    }
    if (!gliss_env) {
	gliss_env = array2array(tgliss_env, 4);
	gls_del = true;
    }
    if (!fm1_env) {
	fm1_env = array2array(tfm_env, 8);
	fm1_del = true;
    }
    if (!fm2_env) {
	fm2_env = array2array(tfm_env, 8);
	fm2_del = true;
    }
    if (!fm3_env) {
	fm3_env = array2array(tfm_env, 8);
	fm3_del = true;
    }

    if (!mus_output_p(out = get_global_output(SX_OUTPUT)))
	sndins_error(S_fm_violin, "needs an output generator");
    rev = get_global_output(SX_REVERB);

#if HAVE_RUBY
    if (XEN_BOUND_P(rb_gv_get("$rbm_locsig_type"))) {
	XEN_ASSERT_TYPE(XEN_INTEGER_P(rb_gv_get("$rbm_locsig_type")),
			rb_gv_get("$rbm_locsig_type"), NULL, S_fm_violin,
			"a constant (Mus_interp_linear or Mus_interp_sinusoidal)");
	mode = XEN_TO_C_INT(rb_gv_get("$rbm_locsig_type"));
    }
#else
    if (XEN_BOUND_P(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-locsig-type*"))) {
	XEN_ASSERT_TYPE(XEN_INTEGER_P(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-locsig-type*")),
			XEN_NAME_AS_C_STRING_TO_VALUE("*clm-locsig-type*"), NULL, S_fm_violin,
			"a constant (mus-interp-linear or mus-interp-sinusoidal)");
	mode = XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-locsig-type*"));
    }
#endif	/* HAVE_RUBY */

    result = ins_fm_violin(start, dur, freq, amp, fm_index,
			   amp_env, amp_len,
			   periodic_vibrato_rate, periodic_vibrato_amp,
			   random_vibrato_rate, random_vibrato_amp,
			   noise_freq, noise_amount,
			   ind_noise_freq, ind_noise_amount,
			   amp_noise_freq, amp_noise_amount,
			   gliss_env, gls_len, gliss_amount,
			   fm1_env, fm1_len, fm2_env, fm2_len, fm3_env, fm3_len,
			   fm1_rat, fm2_rat, fm3_rat, fm1_index, fm2_index, fm3_index,
			   base, degree, distance, reverb_amount,
			   index_type, no_waveshaping, out, rev, mode);

    if (amp_del) FREE(amp_env);
    if (gls_del) FREE(gliss_env);
    if (fm1_del) FREE(fm1_env);
    if (fm2_del) FREE(fm2_env);
    if (fm3_del) FREE(fm3_env);

    return C_TO_XEN_INT(result);
}

#if HAVE_RUBY
#  define H_jc_reverb S_jc_reverb "(\
:startime, 0.0, \
:duration, nil, \
:low_pass, false, \
:volume, 1.0, \
:doubled, false, \
:delay1, 0.013, \
:delay2, 0.011, \
:delay3, 0.015, \
:delay4, 0.017, \
:amp_env, false)\n\
\n\
 require 'ws'\n\
 require 'sndins'\n\
\n\
 with_sound(:play, 1, :statistics, true, :reverb, :jc_reverb) do\n\
   fm_violin(0, 1, 440, 0.7, :fm_index, 1.3)\n\
 end"
#else
#  define H_jc_reverb "(" S_jc_reverb "\
 (:startime 0.0)\
 (:duration #f)\
 (:low-pass #f)\
 (:volume 1.0)\
 (:doubled #f)\
 (:delay1 0.013)\
 (:delay2 0.011)\
 (:delay3 0.015)\
 (:delay4 0.017)\
 (:amp-env #f))\n\
\n\
 (load-from-path \"ws.scm\")\n\
 (load-extension \"libsndins\" \"init_sndins\")\n\
\n\
 (with-sound (:play #t :statistics #t :reverb jc-reverb)\n\
   (fm-violin 0 1 440 0.7 :fm-index 1.3))"
#endif /* HAVE_RUBY */

#define JC_LAST_KEY 10
static XEN
c_jc_reverb(XEN args)
{
    off_t result;
    int vals = 0, lst_len = XEN_LIST_LENGTH(args), amp_len = 0;
    bool low_pass = false, doubled = false;
    Float volume = 1.0, delay1 = 0.013, delay2 = 0.011, delay3 = 0.015, delay4 = 0.017;
    Float start = 0.0, dur = 0.0, *amp_env = NULL;
    vct *tmp_env = NULL;
    mus_any *out = NULL, *rev = NULL;
    int orig_arg[JC_LAST_KEY] = {0};
    XEN kargs[JC_LAST_KEY * 2], keys[JC_LAST_KEY];
    int i = 0;

    keys[i++] = allkeys[C_startime];
    keys[i++] = allkeys[C_duration];
    keys[i++] = allkeys[C_low_pass];
    keys[i++] = allkeys[C_volume];
    keys[i++] = allkeys[C_doubled];
    keys[i++] = allkeys[C_delay1];
    keys[i++] = allkeys[C_delay2];
    keys[i++] = allkeys[C_delay3];
    keys[i++] = allkeys[C_delay4];
    keys[i++] = allkeys[C_amp_env];

    for (i = 0; i < JC_LAST_KEY * 2; i++) kargs[i] = XEN_UNDEFINED;
    for (i = 0; i < lst_len; i++) kargs[i] = XEN_LIST_REF(args, i);
    vals = mus_optkey_unscramble(S_jc_reverb, JC_LAST_KEY, keys, kargs, orig_arg);

    if (vals > 0) {
	i = 0;
	start = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], start); i++;
	dur = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], dur); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i], orig_arg[i], S_jc_reverb, "a bool");
	    low_pass = XEN_TO_C_BOOLEAN(keys[i]);
	}
	i++;
	volume = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], volume); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i], orig_arg[i], S_jc_reverb, "a bool");
	    doubled = XEN_TO_C_BOOLEAN(keys[i]);
	}
	i++;
	delay1 = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], delay1); i++;
	delay2 = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], delay2); i++;
	delay3 = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], delay3); i++;
	delay4 = mus_optkey_to_float(keys[i], S_jc_reverb, orig_arg[i], delay4); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_jc_reverb, "a list");
	    if ((tmp_env = get_vct(keys[i]))) {
		amp_env = array2array(tmp_env->data, tmp_env->length);
		amp_len = tmp_env->length;
	    }
	    else {
		amp_env = xen_list2array(keys[i]);
		amp_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
    }

    if (!mus_output_p(out = get_global_output(SX_OUTPUT)))
	sndins_error(S_jc_reverb, "needs an output generator");
    if (!mus_input_p(rev = get_global_output(SX_REVERB)))
	sndins_error(S_jc_reverb, "needs an input (reverb) generator");
    if (get_global_verbose(S_jc_reverb))
	message("%s on %d in and %d out channels\n",
		S_jc_reverb, mus_channels(rev), mus_channels(out));

    result = ins_jc_reverb(start, dur, volume, low_pass, doubled,
			   delay1, delay2, delay3, delay4,
			   amp_env, amp_len, out, rev);

    return C_TO_XEN_INT(result);
}

#if HAVE_RUBY
#  define H_nrev S_nrev "(\
:startime, 0.0, \
:duration, nil, \
:reverb_factor, 1.09, \
:lp_coeff, 0.7, \
:lp_out_coeff, 0.85, \
:output_scale, 1.0, \
:amp_env, [0, 1, 1, 1], \
:volume, 1.0)\n\
\n\
 require 'ws'\n\
 require 'sndins'\n\
\n\
 with_sound(:play, 1, :statistics, true, :reverb, :nrev) do\n\
   fm_violin(0, 1, 440, 0.7, :fm_index, 1.3)\n\
 end"

#else
#  define H_nrev "(" S_nrev "\
 (:startime 0.0)\
 (:duration #f)\
 (:reverb-factor 1.09)\
 (:lp-coeff 0.7)\
 (:lp-out-coeff 0.85)\
 (:output-scale 1.0)\
 (:amp-env '(0 1 1 1))\
 (:volume 1.0))\n\
\n\
 (load-from-path \"ws.scm\")\n\
 (load-extension \"libsndins\" \"init_sndins\")\n\
\n\
 (with-sound (:play #t :statistics #t :reverb nrev)\n\
   (fm-violin 0 1 440 0.7 :fm-index 1.3))"
#endif /* HAVE_RUBY */

#define N_LAST_KEY 8
static XEN
c_nrev(XEN args)
{
    off_t result;
    int vals = 0, lst_len = XEN_LIST_LENGTH(args), amp_len = 4;
    bool amp_del = false;
    Float start = 0.0, dur = 0.0, lp_coeff = 0.7, lp_out_coeff = 0.85;
    Float output_scale = 1.0, reverb_factor = 1.09, volume = 1.0;
    Float tamp_env[4] = {0, 1, 1, 1};
    Float *amp_env = NULL;
    vct *tmp_env = NULL;
    mus_any *out = NULL, *rev = NULL;
    int orig_arg[N_LAST_KEY] = {0};
    XEN kargs[N_LAST_KEY * 2], keys[N_LAST_KEY];
    int i = 0;

    keys[i++] = allkeys[C_startime];
    keys[i++] = allkeys[C_duration];
    keys[i++] = allkeys[C_reverb_factor];
    keys[i++] = allkeys[C_lp_coeff];
    keys[i++] = allkeys[C_lp_out_coeff];
    keys[i++] = allkeys[C_output_scale];
    keys[i++] = allkeys[C_amp_env];
    keys[i++] = allkeys[C_volume];

    for (i = 0; i < N_LAST_KEY * 2; i++) kargs[i] = XEN_UNDEFINED;
    for (i = 0; i < lst_len; i++) kargs[i] = XEN_LIST_REF(args, i);
    vals = mus_optkey_unscramble(S_nrev, N_LAST_KEY, keys, kargs, orig_arg);

    if (vals > 0) {
	i = 0;
	start = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], start); i++;
	dur = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], dur); i++;
	reverb_factor = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], reverb_factor); i++;
	lp_coeff = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], lp_coeff); i++;
	lp_out_coeff = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], lp_out_coeff); i++;
	output_scale = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], output_scale); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]) || VCT_P(keys[i]),
			    keys[i], orig_arg[i], S_nrev, "a list");
	    if ((tmp_env = get_vct(keys[i]))) {
		amp_env = array2array(tmp_env->data, tmp_env->length);
		amp_len = tmp_env->length;
	    }
	    else {
		amp_env = xen_list2array(keys[i]);
		amp_len = XEN_LIST_LENGTH(keys[i]);
	    }
	}
	i++;
	volume = mus_optkey_to_float(keys[i], S_nrev, orig_arg[i], volume);
    }

    if (!amp_env) {
	amp_env = array2array(tamp_env, 4);
	amp_del = true;
    }
    if (!mus_output_p(out = get_global_output(SX_OUTPUT)))
	sndins_error(S_nrev, "needs an output generator");

    if (!mus_input_p(rev = get_global_output(SX_REVERB)))
	sndins_error(S_nrev, "needs an input (reverb) generator");
    if (get_global_verbose(S_nrev))
	message("%s on %d in and %d out channels\n", S_nrev, mus_channels(rev), mus_channels(out));

    result = ins_nrev(start, dur, reverb_factor, lp_coeff, lp_out_coeff,
		      output_scale, volume, amp_env, amp_len, out, rev);

    if (amp_del) FREE(amp_env);
    
    return C_TO_XEN_INT(result);
}

#if HAVE_RUBY
#  define H_freeverb S_freeverb "(\
:startime, 0.0, \
:duration, nil, \
:room_decay, 0.5, \
:damping, 0.5, \
:global, 0.3, \
:predelay, 0.03, \
:output_gain, 1.0, \
:output_mixer, nil, \
:scale_room_decay, 0.28, \
:offset_room_decay, 0.7, \
:combtuning, [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617], \
:allpasstuning, [556, 441, 341, 225], \
:scale_damping, 0.4, \
:stereo_spread, 23.0,\n\
\n\
 require 'ws'\n\
 require 'sndins'\n\
\n\
 with_sound(:play, 1, :statistics, true, :reverb, :freeverb) do\n\
   fm_violin(0, 1, 440, 0.7, :fm_index, 1.3)\n\
 end"

#else
#  define H_freeverb "(" S_freeverb "\
 (:startime 0.0)\
 (:duration #f)\
 (:room-decay 0.5)\
 (:damping 0.5)\
 (:global 0.3)\
 (:predelay 0.03)\
 (:output-gain 1.0)\
 (:output-mixer #f)\
 (:scale-room-decay 0.28)\
 (:offset-room-decay 0.7)\
 (:combtuning '(1116 1188 1277 1356 1422 1491 1557 1617))\
 (:allpasstuning '(556 441 341 225))\
 (:scale-damping 0.4)\
 (:stereo-spread 23.0))\n\
\n\
 (load-from-path \"ws.scm\")\n\
 (load-extension \"libsndins\" \"init_sndins\")\n\
\n\
 (with-sound (:play #t :statistics #t :reverb freeverb)\n\
   (fm-violin 0 1 440 0.7 :fm-index 1.3))"
#endif /* HAVE_RUBY */

#define F_LAST_KEY 14
static XEN
c_freeverb(XEN args)
{
    off_t result;
    int vals = 0, lst_len = XEN_LIST_LENGTH(args);
    int numcombs = 8, numallpasses = 4;
    int tcombtun[8] = {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
    int tallpass[4] = {556, 441, 341, 225};
    int *combtuning = NULL, *allpasstuning = NULL;
    bool comb_del = false, allpass_del = false;
    Float start = 0.0, dur = 0.0, room_decay = 0.5, global = 0.3, damping = 0.5;
    Float predelay = 0.03, output_gain = 1.0, scale_room_decay = 0.28;
    Float offset_room_decay = 0.7, scale_damping = 0.4, stereo_spread = 23.0;
    mus_any *output_mixer = NULL, *out = NULL, *rev = NULL;
    int orig_arg[F_LAST_KEY] = {0};
    XEN kargs[F_LAST_KEY * 2], keys[F_LAST_KEY];
    int i = 0;

    keys[i++] = allkeys[C_startime];
    keys[i++] = allkeys[C_duration];
    keys[i++] = allkeys[C_room_decay];
    keys[i++] = allkeys[C_damping];
    keys[i++] = allkeys[C_global];
    keys[i++] = allkeys[C_predelay];
    keys[i++] = allkeys[C_output_gain];
    keys[i++] = allkeys[C_output_mixer];
    keys[i++] = allkeys[C_scale_room_decay];
    keys[i++] = allkeys[C_offset_room_decay];
    keys[i++] = allkeys[C_combtuning];
    keys[i++] = allkeys[C_allpasstuning];
    keys[i++] = allkeys[C_scale_damping];
    keys[i++] = allkeys[C_stereo_spread];

    for (i = 0; i < F_LAST_KEY * 2; i++) kargs[i] = XEN_UNDEFINED;
    for (i = 0; i < lst_len; i++) kargs[i] = XEN_LIST_REF(args, i);
    vals = mus_optkey_unscramble(S_freeverb, F_LAST_KEY, keys, kargs, orig_arg);

    if (vals > 0) {
	i = 0;
	start = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], start); i++;
	dur = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], dur); i++;
	room_decay = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], room_decay); i++;
	damping = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], damping); i++;
	global = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], global); i++;
	predelay = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], predelay); i++;
	output_gain = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], output_gain); i++;
	output_mixer = mus_optkey_to_mus_any(keys[i], S_freeverb, orig_arg[i], output_mixer); i++;
	scale_room_decay = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i],
					       scale_room_decay); i++;
	offset_room_decay = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i],
						offset_room_decay); i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i], orig_arg[i], S_freeverb, "a list");
	    combtuning = xen_list2iarray(keys[i]);
	    numcombs = XEN_LIST_LENGTH(keys[i]);
	}
	i++;
	if (!(XEN_KEYWORD_P(keys[i]))) {
	    XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i], orig_arg[i], S_freeverb, "a list");
	    allpasstuning = xen_list2iarray(keys[i]);
	    numallpasses = XEN_LIST_LENGTH(keys[i]);
	}
	i++;
	scale_damping = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], scale_damping); i++;
	stereo_spread = mus_optkey_to_float(keys[i], S_freeverb, orig_arg[i], stereo_spread);
    }

    if (!combtuning) {
	combtuning = int_array2array(tcombtun, 8);
	comb_del = true;
    }
    if (!allpasstuning) {
	allpasstuning = int_array2array(tallpass, 4);
	allpass_del = true;
    }

    if (!mus_output_p(out = get_global_output(SX_OUTPUT)))
	sndins_error(S_freeverb, "needs an output generator");
    if (!mus_input_p(rev = get_global_output(SX_REVERB)))
	sndins_error(S_freeverb, "needs an input (reverb) generator");
    if (get_global_verbose(S_freeverb))
	message("%s on %d in and %d out channels\n",
		S_freeverb, mus_channels(rev), mus_channels(out));

    result = ins_freeverb(start, dur, room_decay, damping, global,
			  predelay, output_gain, scale_room_decay,
			  offset_room_decay, scale_damping, stereo_spread,
			  combtuning, numcombs, allpasstuning, numallpasses,
			  output_mixer, out, rev);

    if (comb_del) FREE(combtuning);
    if (allpass_del) FREE(allpasstuning);
    
    return C_TO_XEN_INT(result);
}

/* propagate the XEN functions */

#ifdef XEN_ARGIFY_1
XEN_VARGIFY(x_make_fcomb, c_make_fcomb);
XEN_ARGIFY_2(x_fcomb, c_fcomb);
XEN_NARGIFY_1(x_fcomb_p, c_fcomb_p);
XEN_VARGIFY(x_fm_violin, c_fm_violin);
XEN_VARGIFY(x_jc_reverb, c_jc_reverb);
XEN_VARGIFY(x_nrev, c_nrev);
XEN_VARGIFY(x_freeverb, c_freeverb);
#else
#  define x_make_fcomb c_make_fcomb
#  define x_fcomb c_fcomb
#  define x_fcomb_p c_fcomb_p
#  define x_fm_violin c_fm_violin
#  define x_jc_reverb c_jc_reverb
#  define x_nrev c_nrev
#  define x_freeverb c_freeverb
#endif	/* XEN_ARGIFY_1 */

void
init_sndins(void)
{
    init_keywords();
    XEN_DEFINE_PROCEDURE(S_make_fcomb, x_make_fcomb, 0, 0, 1, H_make_fcomb);
    XEN_DEFINE_PROCEDURE(S_fcomb,      x_fcomb,      1, 1, 0, H_fcomb);
    XEN_DEFINE_PROCEDURE(S_fcomb_p,    x_fcomb_p,    1, 0, 0, H_fcomb_p);
    XEN_DEFINE_PROCEDURE(S_fm_violin,  x_fm_violin,  0, 0, 1, H_fm_violin);
    XEN_DEFINE_PROCEDURE(S_jc_reverb,  x_jc_reverb,  0, 0, 1, H_jc_reverb);
    XEN_DEFINE_PROCEDURE(S_nrev,       x_nrev,       0, 0, 1, H_nrev);
    XEN_DEFINE_PROCEDURE(S_freeverb,   x_freeverb,   0, 0, 1, H_freeverb);
    XEN_YES_WE_HAVE("sndins");
}

/*
 * Ruby's library name can be `/path/to/ruby/libs/libsndins.so' or
 * `/path/to/ruby/libs/sndins.so' and it can be called:
 * require 'libsndins' or
 * require 'sndins'
 *
 * Guile's init name can be `init_libsndins' or `init_sndins':
 * (load-extension "libsndins" "init_libsndins") or
 * (load-extension "libsndins" "init_sndins")
 */

void
Init_libsndins(void)
{
    init_sndins();
}

void
Init_sndins(void)
{
    init_sndins();
}

void
init_libsndins(void)
{
    init_sndins();
}

/*
 * sndins.c ends here
 */
