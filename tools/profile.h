#if 0
#if 1
#define NUM_COUNTS 65536
static int counts[NUM_COUNTS];
static void clear_counts(void) {int i; for (i = 0; i < NUM_COUNTS; i++) counts[i] = 0;}
void tick(int this) {counts[this]++;}
static void report_counts(s7_scheme *sc)
{
  int i, mx, mxi, total = 0;
  bool happy = true;

  for (i = 0; i < NUM_COUNTS; i++)
    total += counts[i];

  fprintf(stderr, "total: %d\n", total);
  while (happy)
    {
      mx = 0;
      for (i = 0; i < NUM_COUNTS; i++)
	{
	  if (counts[i] > mx)
	    {
	      mx = counts[i];
	      mxi = i;
	    }
	}
      if (mx > 0)
	{
	  /* if (mx > total/100) */
	    fprintf(stderr, "%d: %d (%f)\n", mxi, mx, 100.0*mx/(float)total);
	  counts[mxi] = 0;
	}
      else happy = false;
    }
}
#else

#if 1
#define NUM_COUNTS 500
static int counts[70000][NUM_COUNTS];
static void clear_counts(void) {int i, j; for (i = 0; i < NUM_COUNTS; i++) for (j = 0; j < NUM_COUNTS; j++) counts[i][j] = 0;}
static void tick(int line, int op) {counts[line][op]++; }

static void report_counts(s7_scheme *sc)
{
  int j, mx, mxi, mxj;
  fprintf(stderr, "\n");

  for (mxi = 0; mxi < 70000; mxi++)
    {
      int k, ctotal = 0;
      for (k = 0; k < 500; k++) ctotal += counts[mxi][k];
      if (ctotal > 0)
	{
	  mx = 0;
	  for (j = 0; j < NUM_COUNTS; j++)
	    {
	      if (counts[mxi][j] > mx)
		{
		  mx = counts[mxi][j];
		  mxj = j;
		}
	    }
	  fprintf(stderr, "%d: %d %d of %d\n", mxi, mxj, counts[mxi][mxj], ctotal);
	}
    }
#if 0
  {
  int i;
  bool happy = true;
  while (happy)
    {
      mx = 0;
      for (i = 0; i < 70000; i++)
	for (j = 0; j < NUM_COUNTS; j++)
	  {
	    if (counts[i][j] > mx)
	      {
		mx = counts[i][j];
		mxi = i;
		mxj = j;
	      }
	  }
      if (mx > 0)
	{
	  int k, ctotal = 0;
	  for (k = 0; k < 500; k++) ctotal += counts[mxi][k];
	
	  fprintf(stderr, "%d: %d %d of %d\n", mxi, mxj, counts[mxi][mxj], ctotal);
	  counts[mxi][mxj] = 0;
	}
      else happy = false;
    }
  }
#endif
}

#else
#define NUM_COUNTS 1000
static int counts[NUM_COUNTS];
static void clear_counts(void) {int i; for (i = 0; i < NUM_COUNTS; i++) counts[i] = 0;}
static void tick(int op) {counts[op]++;}
static void report_counts(s7_scheme *sc)
{
  int k, i, mx;
  bool happy = true;
  fprintf(stderr, "\n");
  while (happy)
    {
      mx = 0;
      for (k = 0; k < OP_MAX_DEFINED; k++)
	{
	  if (counts[k] > mx)
	    {
	      mx = counts[k];
	      i = k;
	    }
	}
      if (mx > 0)
	{
	  fprintf(stderr, "%d: %d\n", i, counts[i]);
	  counts[i] = 0;
	}
      else happy = false;
    }
  /* fprintf(stderr, "\n"); */
}
#endif
#endif

static void init_hashes(s7_scheme *sc) {}

#else

void clear_counts(void) {}
static s7_pointer hashes;
void add_expr(s7_scheme *sc, s7_pointer expr);
void add_expr(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer val;
  /* expr = sc->cur_code; */
  val = s7_hash_table_ref(sc, hashes, expr);
  if (val == sc->F)
    {
      if (!is_any_closure(expr))
	s7_hash_table_set(sc, hashes, expr, s7_make_integer(sc, 1));
    }
  else
    {
      s7_hash_table_set(sc, hashes, expr, s7_make_integer(sc, 1 + s7_integer(val)));
    }
}
static void init_hashes(s7_scheme *sc)
{
  hashes = s7_make_hash_table(sc, 65536);
  s7_gc_protect(sc, hashes);
}

typedef struct {
  s7_Int count;
  s7_pointer expr;
} datum;

static datum *new_datum(s7_Int ctr, s7_pointer e)
{
  datum *d;
  d = calloc(1, sizeof(datum));
  d->count = ctr;
  d->expr = e;
  return(d);
}
static int sort_data(const void *v1, const void *v2)
{
  datum *d1 = *(datum **)v1;
  datum *d2 = *(datum **)v2;
  if (d1->count > d2->count)
    return(-1);
  return(1);
}
static void report_counts(s7_scheme *sc)
{
  int len, i, loc = 0, entries;
  hash_entry_t **elements;
  datum **data;

  len = hash_table_length(hashes);
  elements = hash_table_elements(hashes);
  entries = hash_table_entries(hashes);
  if (entries == 0)
    {
      fprintf(stderr, "no counts\n");
      return;
    }
  data = (datum **)calloc(entries, sizeof(datum *));

  for (i = 0; i < len; i++)
    {
      hash_entry_t *x;
      for (x = elements[i]; x; x = x->next)
	data[loc++] = new_datum(s7_integer(x->value), x->key);
    }

  qsort((void *)data, loc, sizeof(datum *), sort_data);
  if (loc > 400) loc = 400;
  fprintf(stderr, "\n");
  for (i = 0; i < loc; i++)
    if (data[i]->count > 0)
      fprintf(stderr, "%lld: %s\n", data[i]->count, DISPLAY_80(data[i]->expr));

  free(data);
}
void add_code(s7_scheme *sc);
void add_code(s7_scheme *sc)
{
  add_expr(sc, sc->code);
}
/* use xen.h and s7 here */
#endif
