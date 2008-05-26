
# include <rollendurchmesserzeitsammler.h>

int fix_stalin_c_source(char* infile, char *outfile);

void init_rollendurchmesserzeitsammler(int a,int b,int c);


/* hbgc->rollendurchmesserzeitsammler */

// disabled for now since the mem main contain void* pointers.
//#define GC_malloc_atomic(size) tar_alloc_atomic(heap,size)

#define GC_malloc_atomic(size) tar_alloc(heap,size)

#define GC_malloc(size) tar_alloc(heap,size)

// This one too.
//#define GC_malloc_atomic_uncollectable(size) tar_malloc_atomic_uncollectable(size)
#define GC_malloc_atomic_uncollectable(size) tar_alloc(heap,size)

#define GC_malloc_uncollectable(size) tar_alloc_uncollectable(size)

// Hmm, only works if its atomic uncollectable mem. Not good.
//#define GC_free(mem) tar_free_atomic_uncollectable(mem)
// Same reason. Just make it a dummy:
#define GC_free(mem) /* */


// In clm.c returns the old heap.
//tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap);
tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap);

typedef void (*error_func_t)(char*);

error_func_t clm_set_error_func(error_func_t new_error);

int rt_mus_error(int type,const char* fmt,...);

void* clm_calloc_atomic(int num,size_t size,const char* what);
void* clm_calloc(int num,size_t size,const char* what);
void* clm_malloc_atomic(size_t size,const char* what);
void* clm_malloc(size_t size,const char* what);

