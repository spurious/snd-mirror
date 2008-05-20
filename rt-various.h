
int fix_stalin_c_source(char* infile, char *outfile);

void init_rollendurchmesserzeitsammler(int a,int b,int c);


/* hbgc->rollendurchmesserzeitsammler */


#define GC_malloc_atomic(size) tar_alloc_atomic(heap,size)
#define GC_malloc(size) tar_alloc(heap,size)

#define GC_malloc_atomic_uncollectable(size) tar_malloc_atomic_uncollectable(size)

// This does not seem to be correct! (no! its very wrong! :-)
//#define GC_malloc_uncollectable(size) tar_malloc_atomic_uncollectable(size)

// Hmm, only works if its atomic uncollectable mem. Not good.
#define GC_free(mem) tar_free_atomic_uncollectable(mem)
