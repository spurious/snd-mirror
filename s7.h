#ifndef _S7_H
#define _S7_H

#include <stdio.h>

#ifndef __cplusplus
#if HAVE_STDBOOL_H
  #include <stdbool.h>
#else
#ifndef true
  #define bool	int
  #define true	1
  #define false	0
#endif
#endif
#endif

#define Int int
#define Int_d "%d"


#define S7_VERSION "0"

typedef struct scheme scheme;
typedef struct cell *pointer;
typedef void *(*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

typedef pointer (*foreign_func)(scheme *scheme, pointer a);

pointer s7_F(scheme *sc);
pointer s7_T(scheme *sc);
pointer s7_NIL(scheme *sc);
pointer s7_UNDEFINED(scheme *sc);
pointer s7_EOF_OBJ(scheme *sc);
pointer s7_global_env(scheme *sc);

bool s7_is_immutable(pointer p);
void s7_setimmutable(pointer p);
pointer s7_immutable_cons(scheme *sc, pointer a, pointer b);

pointer s7_cons(scheme *sc, pointer a, pointer b);
pointer s7_car(pointer p);
pointer s7_cdr(pointer p);
bool s7_is_pair(pointer p);
pointer s7_pair_car(pointer p);
pointer s7_pair_cdr(pointer p);
pointer s7_set_car(pointer p, pointer q);
pointer s7_set_cdr(pointer p, pointer q);

int s7_list_length(scheme *sc, pointer a);
pointer s7_reverse(scheme *sc, pointer a);
pointer s7_reverse_in_place(scheme *sc, pointer term, pointer list);
pointer s7_append(scheme *sc, pointer a, pointer b);
pointer s7_list_ref(scheme *sc, pointer lst, int num);
pointer s7_list_set(scheme *sc, pointer lst, int num, pointer val);
pointer s7_assoc(scheme *sc, pointer sym, pointer lst);
pointer s7_member(scheme *sc, pointer sym, pointer lst);
pointer s7_remv(scheme *sc, pointer a, pointer obj);


bool s7_is_string(pointer p);
char *s7_string_value(pointer p);
pointer s7_make_string(scheme *sc, const char *str);
pointer s7_make_counted_string(scheme *sc, const char *str, int len);

bool s7_is_character(pointer p);
long s7_charvalue(pointer p);
pointer s7_make_character(scheme *sc, int c);


bool s7_is_number(pointer p);
bool s7_is_exact(pointer p);
bool s7_is_inexact(pointer p);

int s7_to_c_int(pointer p);
bool s7_is_integer(pointer p);
pointer s7_make_integer(scheme *sc, Int num);

bool s7_is_real(pointer p);
double s7_to_c_double(pointer p);
pointer s7_make_real(scheme *sc, double num);

bool s7_is_ulong(pointer arg);
unsigned long s7_to_c_ulong(pointer num);
pointer s7_make_ulong(scheme *sc, unsigned long num);

bool s7_is_rational(pointer arg);
bool s7_is_ratio(pointer arg);
pointer s7_make_ratio(scheme *sc, Int a, Int b);
pointer s7_rationalize(scheme *sc, double x, double error);
Int s7_numerator(pointer x);
Int s7_denominator(pointer x);

bool s7_is_complex(pointer arg);
pointer s7_make_complex(scheme *sc, double a, double b);
double s7_real_part(pointer z);
double s7_imag_part(pointer z);

bool s7_is_vector(pointer p);
void s7_fill_vector(pointer vec, pointer obj);
pointer s7_vector_ref(pointer vec, int ielem);
pointer s7_vector_set(pointer vec, int ielem, pointer a);
pointer s7_make_vector(scheme *sc, int len);
pointer s7_make_and_fill_vector(scheme *sc, int len, pointer fill);
int s7_vector_length(pointer vec);
pointer s7_vector_to_list(scheme *sc, pointer vect);

bool s7_is_port(pointer p);
bool s7_is_proc(pointer p);


bool s7_is_symbol(pointer p);
char *s7_symbol_name(pointer p);
pointer s7_make_symbol(scheme *sc, const char *name);
pointer s7_gensym(scheme *sc);

bool s7_is_keyword(pointer obj);
bool s7_keyword_eq_p(pointer k1, pointer k2);
pointer s7_make_keyword(scheme *sc, const char *key);


bool s7_is_foreign_function(pointer p);
pointer s7_make_foreign_function(scheme *sc, foreign_func f, const char *doc);
const char *s7_foreign_function_doc(pointer p);

int s7_new_foreign_type(const char *name, 
			char *(*print)(void *value), 
			void (*free)(void *value), 
			bool (*equal)(void *val1, void *val2),
			void (*gc_mark)(void *val));

void *s7_foreign_object_value(pointer obj);
int s7_foreign_object_type(pointer obj);
bool s7_is_foreign_object(pointer p);
pointer s7_make_foreign_object(scheme *sc, int type, void *value);
char *s7_describe_foreign_object(pointer a);
void s7_free_foreign_object(pointer a);
bool s7_equalp_foreign_objects(pointer a, pointer b);
void s7_mark_object(pointer a); /* s7 for foreign objects with embedded scheme objects */


bool s7_is_closure(pointer p);
pointer s7_closure_code(pointer p);
pointer s7_closure_env(pointer p);
pointer s7_make_closure(scheme *sc, pointer c, pointer e);

pointer s7_make_continuation(scheme *sc, pointer d);
bool s7_is_continuation(pointer p);

bool s7_is_environment(pointer p);


scheme *s7_init_new(void);
scheme *s7_init_new_custom_alloc(func_alloc smalloc, func_dealloc free);
int s7_init(scheme *sc);
int s7_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
void s7_deinit(scheme *sc);
void s7_set_input_port_file(scheme *sc, FILE *fin);
void s7_set_input_port_string(scheme *sc, char *start, char *past_the_end);
void s7_set_output_port_file(scheme *sc, FILE *fin);
void s7_set_output_port_string(scheme *sc, char *start, char *past_the_end);

void s7_set_external_data(scheme *sc, void *p);


void s7_putstr(scheme *sc, const char *s);
void s7_putchars(scheme *sc, const char *s, int len);

pointer s7_apply1(scheme *sc, const char *procname, pointer);
pointer s7_apply0(scheme *sc, const char *procname);
pointer s7_call(scheme *sc, pointer func, pointer args);

bool s7_eqv_p(pointer a, pointer b);
bool s7_eq_p(pointer obj1, pointer obj2);

void s7_atom2str(scheme *sc, pointer l, int f, char **pp, int *plen);

void s7_define(scheme *sc, pointer env, pointer symbol, pointer value);
void s7_new_slot_spec_in_env(scheme *sc, pointer env, pointer variable, pointer value);
pointer s7_find_slot_in_env(scheme *sc, pointer env, pointer sym, int all);
pointer s7_slot_value_in_env(pointer slot);
pointer s7_name_to_value(scheme *sc, const char *name);
pointer s7_symbol_value(scheme *sc, pointer sym);
pointer s7_symbol_set_value(scheme *sc, pointer sym, pointer val);


pointer s7_eval_string(scheme *sc, const char *str);
pointer s7_eval_form(scheme *sc, pointer form);

pointer s7_string_to_form(scheme *sc, const char *str);
pointer s7_object_to_string(scheme *sc, pointer arg);
void s7_load_string(scheme *sc, const char *cmd);

void s7_load_open_file(scheme *sc, FILE *fin);
pointer s7_load_file(scheme *sc, const char *file);
pointer s7_load_path(scheme *sc);
pointer s7_add_to_load_path(scheme *sc, const char *file);
pointer s7_load_file_with_path(scheme *sc, const char *file);

void s7_provide(scheme *sc, const char *feature);

const char *s7_get_output_string(scheme *sc, pointer p);
pointer s7_open_output_string(scheme *sc);


pointer s7_error(scheme *sc, pointer type, pointer info);
pointer s7_throw(scheme *sc, pointer type, pointer info);
pointer s7_wrong_type_arg_error(scheme *sc, const char *caller, int arg_n, pointer arg, const char *descr);
pointer s7_out_of_range_error(scheme *sc, const char *caller, int arg_n, pointer arg, const char *descr);


#endif

