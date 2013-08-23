;;; libc.scm
;;;
;;; tie the C library into the *libc* environment

(if (not (provided? 'cload.scm)) (load "cload.scm"))
(provide 'libc.scm)

(if (not (defined? '*libc*))
    (define-constant *libc*
      (with-environment (initial-environment)
	(set! *libraries* (cons (cons "libc.scm" (current-environment)) *libraries*))
	
	;; -------- stddef.h --------
	(define NULL (c-pointer 0))

	;; -------- stdbool.h --------
	(define false #f)
	(define true #t)

	;; -------- iso646.h --------
	;; spelled-out names for & = bitand et al

	;; -------- stdarg.h --------
	;; the varargs macros

	;; -------- assert.h --------
	;; assert macro
	;; we could make a #reader for this so that the assertion really does disappear if not debugging
	;; or better use #+debugging (assert expr) and move the #+ code into s7

	;; -------- setjmp.h --------
	;; longjmp etc

	;; -------- dlfn.h --------
	;; see libdl.scm, similarly for pthreads see libpthread.scm

	;; -------- sys/types.h --------
	;; C type declarations, getopt.h and inttypes.h are also irrelevant here

	(c-define '(;; -------- limits.h --------
		    (C-macro (int (SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_BIT CHAR_MIN CHAR_MAX __WORDSIZE 
				   SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX
				   LLONG_MIN LLONG_MAX ULLONG_MAX
				   _POSIX_AIO_LISTIO_MAX _POSIX_AIO_MAX _POSIX_ARG_MAX _POSIX_CHILD_MAX _POSIX_DELAYTIMER_MAX _POSIX_HOST_NAME_MAX 
				   _POSIX_LINK_MAX _POSIX_LOGIN_NAME_MAX _POSIX_MAX_CANON _POSIX_MAX_INPUT _POSIX_MQ_OPEN_MAX _POSIX_MQ_PRIO_MAX 
				   _POSIX_NAME_MAX _POSIX_NGROUPS_MAX _POSIX_OPEN_MAX _POSIX_FD_SETSIZE _POSIX_PATH_MAX _POSIX_PIPE_BUF _POSIX_RE_DUP_MAX 
				   _POSIX_RTSIG_MAX _POSIX_SEM_NSEMS_MAX _POSIX_SEM_VALUE_MAX _POSIX_SIGQUEUE_MAX _POSIX_SSIZE_MAX _POSIX_STREAM_MAX 
				   _POSIX_SYMLINK_MAX _POSIX_SYMLOOP_MAX _POSIX_TIMER_MAX _POSIX_TTY_NAME_MAX _POSIX_TZNAME_MAX _POSIX_QLIMIT 
				   _POSIX_HIWAT _POSIX_UIO_MAXIOV _POSIX_CLOCKRES_MIN SSIZE_MAX NGROUPS_MAX _POSIX2_BC_BASE_MAX _POSIX2_BC_DIM_MAX 
				   _POSIX2_BC_SCALE_MAX _POSIX2_BC_STRING_MAX _POSIX2_COLL_WEIGHTS_MAX _POSIX2_EXPR_NEST_MAX _POSIX2_LINE_MAX 
				   _POSIX2_RE_DUP_MAX _POSIX2_CHARCLASS_NAME_MAX BC_BASE_MAX BC_DIM_MAX BC_SCALE_MAX BC_STRING_MAX COLL_WEIGHTS_MAX 
				   EXPR_NEST_MAX LINE_MAX CHARCLASS_NAME_MAX RE_DUP_MAX)))


		    ;; -------- float.h --------
		    (C-macro (int (FLT_RADIX FLT_MANT_DIG DBL_MANT_DIG LDBL_MANT_DIG FLT_DIG DBL_DIG LDBL_DIG FLT_MIN_EXP DBL_MIN_EXP
				   LDBL_MIN_EXP FLT_MIN_10_EXP DBL_MIN_10_EXP LDBL_MIN_10_EXP FLT_MAX_EXP DBL_MAX_EXP LDBL_MAX_EXP
				   FLT_MAX_10_EXP DBL_MAX_10_EXP LDBL_MAX_10_EXP FLT_ROUNDS FLT_EVAL_METHOD)))
		    (C-macro (double (FLT_MAX DBL_MAX LDBL_MAX FLT_EPSILON DBL_EPSILON LDBL_EPSILON FLT_MIN DBL_MIN LDBL_MIN)))


		    ;; -------- stdint.h --------
		    (C-macro (int (INT8_MIN INT16_MIN INT32_MIN INT64_MIN INT8_MAX INT16_MAX INT32_MAX INT64_MAX UINT8_MAX UINT16_MAX 
				   UINT32_MAX UINT64_MAX INT_LEAST8_MIN INT_LEAST16_MIN INT_LEAST32_MIN INT_LEAST64_MIN INT_LEAST8_MAX 
				   INT_LEAST16_MAX INT_LEAST32_MAX INT_LEAST64_MAX UINT_LEAST8_MAX UINT_LEAST16_MAX UINT_LEAST32_MAX 
				   UINT_LEAST64_MAX INT_FAST8_MIN INT_FAST16_MIN INT_FAST32_MIN INT_FAST64_MIN INT_FAST8_MAX INT_FAST16_MAX 
				   INT_FAST32_MAX INT_FAST64_MAX UINT_FAST8_MAX UINT_FAST16_MAX UINT_FAST32_MAX UINT_FAST64_MAX INTPTR_MIN 
				   INTPTR_MAX UINTPTR_MAX INTMAX_MIN INTMAX_MAX UINTMAX_MAX PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX 
				   SIZE_MAX WCHAR_MIN WCHAR_MAX WINT_MIN WINT_MAX )))


		    ;; -------- endian.h --------
		    ;; also has htobe16 etc
		    (C-macro (int (__BYTE_ORDER __BIG_ENDIAN __LITTLE_ENDIAN)))

		    
		    ;; -------- ctype.h --------
		    (int isalnum (int))
		    (int isalpha (int))
		    (int iscntrl (int))
		    (int isdigit (int))
		    (int islower (int))
		    (int isgraph (int))
		    (int isprint (int))
		    (int ispunct (int))
		    (int isspace (int))
		    (int isupper (int))
		    (int isxdigit (int))
		    (int tolower (int))
		    (int toupper (int))


		    ;; -------- fcntl.h --------
		    (C-macro (int (S_IFMT S_IFDIR S_IFCHR S_IFBLK S_IFREG S_IFIFO __S_IFLNK S_IFSOCK S_ISUID S_ISGID S_IRUSR 
				   S_IWUSR S_IXUSR S_IRWXU S_IRGRP S_IWGRP S_IXGRP S_IRWXG S_IROTH S_IWOTH S_IXOTH S_IRWXO R_OK W_OK X_OK 
				   F_OK SEEK_SET SEEK_CUR SEEK_END F_ULOCK F_LOCK F_TLOCK F_TEST O_ACCMODE O_RDONLY O_WRONLY O_RDWR O_CREAT 
				   O_EXCL O_NOCTTY O_TRUNC O_APPEND O_NONBLOCK O_NDELAY O_SYNC O_FSYNC O_ASYNC O_DSYNC O_RSYNC O_LARGEFILE 
				   F_DUPFD F_GETFD F_SETFD F_GETFL F_SETFL F_GETLK F_SETLK F_SETLKW F_GETLK64 F_SETLK64 F_SETLKW64 
				   FD_CLOEXEC F_RDLCK F_WRLCK F_UNLCK POSIX_FADV_NORMAL POSIX_FADV_RANDOM POSIX_FADV_SEQUENTIAL 
				   POSIX_FADV_WILLNEED POSIX_FADV_DONTNEED POSIX_FADV_NOREUSE)))
		    (int fcntl (int int))
		    (int open (char* int))
		    (int creat (char* (mode_t int)))
		    (int lockf (int int int))
		    ;; (not in openbsd) (int posix_fadvise (int int int int))
		    ;; (not in openbsd) (int posix_fallocate (int int int))
		    ;; #-openbsd would be nicer, and the reader in s7.html works -- perhaps move it to s7.c?

		    
		    ;; -------- fenv.h --------
		    (C-macro (int (FE_INEXACT FE_DIVBYZERO FE_UNDERFLOW FE_OVERFLOW FE_INVALID FE_ALL_EXCEPT
				   FE_TONEAREST FE_UPWARD FE_DOWNWARD FE_TOWARDZERO)))
		    (int feclearexcept (int))
		    (int fegetexceptflag (fexcept_t* int) )
		    (int feraiseexcept (int) )
		    (int fesetexceptflag (fexcept_t* int) )
		    (int fetestexcept (int) )
		    (int fegetround (void) )
		    (int fesetround (int) )
		    (int fegetenv (fenv_t*) )
		    (int feholdexcept (fenv_t*) )
		    (int fesetenv (fenv_t*) )
		    (int feupdateenv (fenv_t*) )


		    ;; -------- string.h --------
		    (void* memcpy (void* void* size_t))
		    (void* memmove (void* void* size_t))
		    (void* memset (void* int size_t))
		    (int memcmp (void* void* size_t))
		    (void* memchr (void* int size_t))
		    ;; (void* memrchr (void* int size_t))
		    (char* strcpy (char* char*))
		    (char* strncpy (char* char* size_t))
		    (char* strcat (char* char*))
		    (char* strncat (char* char* size_t))
		    (int strcmp (char* char*))
		    (int strncmp (char* char* size_t))
		    (int strcoll (char* char*))
		    (size_t strxfrm (char* char* size_t))
		    (char* strchr (char* int))
		    (char* strrchr (char* int))
		    ;; (char* strchrnul (char* int))
		    (size_t strcspn (char* char*))
		    (size_t strspn (char* char*))
		    (char* strpbrk (char* char*))
		    (char* strstr (char* char*))
		    (char* strtok (char* char*))
		    ;; (char* strcasestr (char* char*))
		    (size_t strlen (char*))
		    (size_t strnlen (char* size_t))
		    (char* strerror (int))
		    (int strcasecmp (char* char*))
		    (int strncasecmp (char* char* size_t))


		    ;; -------- stdio.h --------
		    (C-macro (int (_IOFBF _IOLBF _IONBF BUFSIZ EOF L_tmpnam TMP_MAX FILENAME_MAX L_ctermid L_cuserid FOPEN_MAX IOV_MAX)))
		    (C-macro (char* P_tmpdir))

		    (int remove (char*))
		    (int rename (char* char*))
		    (FILE* tmpfile (void))
		    (char* tmpnam (char*))
		    (char* tempnam (char* char*))
		    (int fclose (FILE*))
		    (int fflush (FILE*))
		    ;; (not in openbsd) (int fcloseall (void))
		    (FILE* fopen (char* char*))
		    (FILE* freopen (char*  char* FILE*))
		    (FILE* fdopen (int char*))
		    (void setbuf (FILE* char*))
		    (int setvbuf (FILE* char* int size_t))
		    (void setlinebuf (FILE*))
		    (int fgetc (FILE*))
		    (int getc (FILE*))
		    (int getchar (void))
		    (int fputc (int FILE*))
		    (int putc (int FILE*))
		    (int putchar (int))
		    (char* fgets (char* int FILE*))
		    (int fputs (char* FILE*))
		    (int puts (char*))
		    (int ungetc (int FILE*))
		    (size_t fread (void* size_t size_t FILE*))
		    (size_t fwrite (void* size_t size_t FILE*))
		    (int fseek (FILE* int int))
		    (int ftell (FILE*))
		    (void rewind (FILE*))
		    (int fgetpos (FILE* fpos_t*))
		    (int fsetpos (FILE* fpos_t*))
		    (void clearerr (FILE*))
		    (int feof (FILE*))
		    (int ferror (FILE*))
		    (void perror (char*))
		    (int fileno (FILE*))
		    (FILE* popen (char* char*))
		    (int pclose (FILE*))
		    (char* ctermid (char*))
		    ;; (char* cuserid (char*))
		    (void flockfile (FILE*))
		    (int ftrylockfile (FILE*))
		    (void funlockfile (FILE*))
		    ;; int fprintf (FILE* char* ...)
		    ;; int printf (char* ...)
		    ;; int sprintf (char* char* ...) 
		    ;; int vfprintf (FILE* char* va_list)
		    ;; int vprintf (char* va_list)
		    ;; int vsprintf (char* char* va_list) 
		    ;; int snprintf (char* size_t char* ...)
		    ;; int vsnprintf (char* size_t char* va_list)
		    ;; int vasprintf (char** char* va_list)
		    ;; int asprintf (char** char* ...)
		    ;; int fscanf (FILE* char* ...)
		    ;; int scanf (char* ...)
		    ;; int sscanf (char* char* ...) 
		    ;; int vfscanf (FILE* char* va_list)
		    ;; int vscanf (char* va_list)
		    ;; int vsscanf (char* char* va_list)


                    ;; -------- stdlib.h --------
                    (C-macro (int (RAND_MAX EXIT_FAILURE EXIT_SUCCESS MB_CUR_MAX)))
		    (double atof (char*))
		    (int atoi (char*))
		    (int atol (char*))
		    (int atoll (char*))
		    (int random (void))
		    (void srandom (int))
		    (char* initstate (int char* size_t))
		    (char* setstate (char*))
		    (int rand (void))
		    (void srand (int))
		    (void* malloc (size_t))
		    (void* calloc (size_t size_t))
		    (void* realloc (void* size_t))
		    (void free (void*))
		    (void abort (void))
		    (void exit (int))
		    (char* getenv (char*))
		    (int putenv (char*))
		    (int setenv (char* char* int))
		    (int unsetenv (char*))
		    (char* mktemp (char*))
		    (int mkstemp (char*))
		    (int system (char*))
		    (char* realpath (char* char*))
		    (int abs (int))
		    (int labs (int))
		    (int llabs (int))

		    (in-C "
static s7_pointer g_strtod(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, strtod(s7_string(s7_car(args)), NULL)));}
static s7_pointer g_strtof(s7_scheme *sc, s7_pointer args) {return(s7_make_real(sc, strtof(s7_string(s7_car(args)), NULL)));}
static s7_pointer g_strtol(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, strtol(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
static s7_pointer g_strtoll(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, strtoll(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
static s7_pointer g_div(s7_scheme *sc, s7_pointer args)
{
  div_t d;
  d = div(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)));
  return(s7_list(sc, 2, s7_make_integer(sc, d.quot), s7_make_integer(sc, d.rem)));
}
static s7_pointer g_ldiv(s7_scheme *sc, s7_pointer args)
{
  ldiv_t d;
  d = ldiv(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)));
  return(s7_list(sc, 2, s7_make_integer(sc, d.quot), s7_make_integer(sc, d.rem)));
}
")
                    (C-function ("strtod" g_strtod "" 1))
                    (C-function ("strtof" g_strtof "" 1))
                    (C-function ("strtol" g_strtol "" 2))
                    (C-function ("strtoll" g_strtoll "" 2))
                    (C-function ("div" g_div "" 1))
                    (C-function ("ldiv" g_ldiv "" 1))


		    ;; -------- errno.h --------
		    ;; pws for errno?
		    (C-macro (int (__GLIBC__ __GLIBC_MINOR__ ; features.h from errno.h
				   ECANCELED EOWNERDEAD ENOTRECOVERABLE ERFKILL EILSEQ
				   ;; asm-generic/errno-base.h
				   EPERM ENOENT ESRCH EINTR EIO ENXIO E2BIG ENOEXEC EBADF ECHILD EAGAIN ENOMEM EACCES EFAULT
				   ENOTBLK EBUSY EEXIST EXDEV ENODEV ENOTDIR EISDIR EINVAL ENFILE EMFILE ENOTTY ETXTBSY EFBIG
				   ENOSPC ESPIPE EROFS EMLINK EPIPE EDOM ERANGE
				   )))
		    (in-C "static s7_pointer g_errno(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, errno));}
                           static s7_pointer g_set_errno(s7_scheme *sc, s7_pointer args) {errno = (int)s7_integer(s7_car(args)); return(s7_car(args));}")
                    (C-function ("errno" g_errno "" 0))
                    (C-function ("set_errno" g_set_errno "" 1))


		    ;; -------- locale.h --------
		    (C-macro (int (LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL LC_PAPER LC_NAME 
				   LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION)))
		    (char* setlocale (int char*))
		    (in-C "
static s7_pointer g_localeconv(s7_scheme *sc, s7_pointer args)
{
  struct lconv *lc;
  lc = localeconv();
  return(s7_list(sc, 18,
		     s7_cons(sc, s7_make_symbol(sc, \"decimal_point\"),     s7_make_string(sc, lc->decimal_point)),
		     s7_cons(sc, s7_make_symbol(sc, \"thousands_sep\"),     s7_make_string(sc, lc->thousands_sep)),
		     s7_cons(sc, s7_make_symbol(sc, \"grouping\"),          s7_make_string(sc, lc->grouping)),
		     s7_cons(sc, s7_make_symbol(sc, \"int_curr_symbol\"),   s7_make_string(sc, lc->int_curr_symbol)),
		     s7_cons(sc, s7_make_symbol(sc, \"currency_symbol\"),   s7_make_string(sc, lc->currency_symbol)),
		     s7_cons(sc, s7_make_symbol(sc, \"mon_decimal_point\"), s7_make_string(sc, lc->mon_decimal_point)),
		     s7_cons(sc, s7_make_symbol(sc, \"mon_thousands_sep\"), s7_make_string(sc, lc->mon_thousands_sep)),
		     s7_cons(sc, s7_make_symbol(sc, \"mon_grouping\"),      s7_make_string(sc, lc->mon_grouping)),
		     s7_cons(sc, s7_make_symbol(sc, \"positive_sign\"),     s7_make_string(sc, lc->positive_sign)),
		     s7_cons(sc, s7_make_symbol(sc, \"negative_sign\"),     s7_make_string(sc, lc->negative_sign)),
		     
		     s7_cons(sc, s7_make_symbol(sc, \"int_frac_digits\"),   s7_make_integer(sc, lc->int_frac_digits)),
		     s7_cons(sc, s7_make_symbol(sc, \"frac_digits\"),       s7_make_integer(sc, lc->frac_digits)),
		     s7_cons(sc, s7_make_symbol(sc, \"p_cs_precedes\"),     s7_make_integer(sc, lc->p_cs_precedes)),
		     s7_cons(sc, s7_make_symbol(sc, \"p_sep_by_space\"),    s7_make_integer(sc, lc->p_sep_by_space)),
		     s7_cons(sc, s7_make_symbol(sc, \"n_cs_precedes\"),     s7_make_integer(sc, lc->n_cs_precedes)),
		     s7_cons(sc, s7_make_symbol(sc, \"n_sep_by_space\"),    s7_make_integer(sc, lc->n_sep_by_space)),
		     s7_cons(sc, s7_make_symbol(sc, \"p_sign_posn\"),       s7_make_integer(sc, lc->p_sign_posn)),
		     s7_cons(sc, s7_make_symbol(sc, \"n_sign_posn\"),       s7_make_integer(sc, lc->n_sign_posn))));
}
  ")
                    (C-function ("localeconv" g_localeconv "" 0))


		    ;; -------- sys/utsname.h --------
		    (in-C "
static s7_pointer g_uname(s7_scheme *sc, s7_pointer args)
{
  struct utsname buf;
  uname(&buf);
  return(s7_list(sc, 5, s7_make_string(sc, buf.sysname), 
		        s7_make_string(sc, buf.machine), 
		        s7_make_string(sc, buf.nodename), 
		        s7_make_string(sc, buf.version), 
		        s7_make_string(sc, buf.release)));
}
")
                    (C-function ("uname" g_uname "" 0))


		    ;; -------- unistd.h --------                  
		    (C-macro (int (_POSIX_VERSION _POSIX2_VERSION _POSIX_JOB_CONTROL _POSIX_SAVED_IDS _POSIX_PRIORITY_SCHEDULING _POSIX_SYNCHRONIZED_IO
				   _POSIX_FSYNC _POSIX_MAPPED_FILES _POSIX_MEMLOCK _POSIX_MEMLOCK_RANGE _POSIX_MEMORY_PROTECTION _POSIX_CHOWN_RESTRICTED
				   _POSIX_VDISABLE _POSIX_NO_TRUNC _POSIX_THREADS _POSIX_REENTRANT_FUNCTIONS _POSIX_THREAD_SAFE_FUNCTIONS
				   _POSIX_THREAD_PRIORITY_SCHEDULING _POSIX_THREAD_ATTR_STACKSIZE _POSIX_THREAD_ATTR_STACKADDR _POSIX_THREAD_PRIO_INHERIT
				   _POSIX_THREAD_PRIO_PROTECT _POSIX_SEMAPHORES _POSIX_REALTIME_SIGNALS _POSIX_ASYNCHRONOUS_IO _POSIX_ASYNC_IO
				   _POSIX_PRIORITIZED_IO _POSIX_SHARED_MEMORY_OBJECTS _POSIX_CPUTIME _POSIX_THREAD_CPUTIME _POSIX_REGEXP
				   _POSIX_READER_WRITER_LOCKS _POSIX_SHELL _POSIX_TIMEOUTS _POSIX_SPIN_LOCKS _POSIX_SPAWN _POSIX_TIMERS 
				   _POSIX_BARRIERS _POSIX_MESSAGE_PASSING _POSIX_THREAD_PROCESS_SHARED _POSIX_MONOTONIC_CLOCK _POSIX_CLOCK_SELECTION
				   _POSIX_ADVISORY_INFO _POSIX_IPV6 _POSIX_RAW_SOCKETS _POSIX2_CHAR_TERM _POSIX_SPORADIC_SERVER _POSIX_THREAD_SPORADIC_SERVER
				   _POSIX_TRACE _POSIX_TRACE_EVENT_FILTER _POSIX_TRACE_INHERIT _POSIX_TRACE_LOG _POSIX_TYPED_MEMORY_OBJECTS 
				   STDIN_FILENO STDOUT_FILENO STDERR_FILENO)))

                    (C-macro 
		     (int (_PC_LINK_MAX _PC_MAX_CANON _PC_MAX_INPUT _PC_NAME_MAX _PC_PATH_MAX _PC_PIPE_BUF _PC_CHOWN_RESTRICTED _PC_NO_TRUNC
			   _PC_VDISABLE _PC_SYNC_IO _PC_ASYNC_IO _PC_PRIO_IO _PC_SOCK_MAXBUF _PC_FILESIZEBITS _PC_REC_INCR_XFER_SIZE _PC_REC_MAX_XFER_SIZE
			   _PC_REC_MIN_XFER_SIZE _PC_REC_XFER_ALIGN _PC_ALLOC_SIZE_MIN _PC_SYMLINK_MAX _PC_2_SYMLINKS _SC_ARG_MAX _SC_CHILD_MAX _SC_CLK_TCK
			   _SC_NGROUPS_MAX _SC_OPEN_MAX _SC_STREAM_MAX _SC_TZNAME_MAX _SC_JOB_CONTROL _SC_SAVED_IDS _SC_REALTIME_SIGNALS _SC_PRIORITY_SCHEDULING
			   _SC_TIMERS _SC_ASYNCHRONOUS_IO _SC_PRIORITIZED_IO _SC_SYNCHRONIZED_IO _SC_FSYNC _SC_MAPPED_FILES _SC_MEMLOCK _SC_MEMLOCK_RANGE
			   _SC_MEMORY_PROTECTION _SC_MESSAGE_PASSING _SC_SEMAPHORES _SC_SHARED_MEMORY_OBJECTS _SC_AIO_LISTIO_MAX _SC_AIO_MAX _SC_AIO_PRIO_DELTA_MAX
			   _SC_DELAYTIMER_MAX _SC_MQ_OPEN_MAX _SC_MQ_PRIO_MAX _SC_VERSION _SC_PAGESIZE _SC_PAGE_SIZE _SC_RTSIG_MAX _SC_SEM_NSEMS_MAX _SC_SEM_VALUE_MAX
			   _SC_SIGQUEUE_MAX _SC_TIMER_MAX _SC_BC_BASE_MAX _SC_BC_DIM_MAX _SC_BC_SCALE_MAX _SC_BC_STRING_MAX _SC_COLL_WEIGHTS_MAX _SC_EQUIV_CLASS_MAX
			   _SC_EXPR_NEST_MAX _SC_LINE_MAX _SC_RE_DUP_MAX _SC_CHARCLASS_NAME_MAX _SC_2_VERSION _SC_2_C_BIND _SC_2_C_DEV _SC_2_FORT_DEV _SC_2_FORT_RUN
			   _SC_2_SW_DEV _SC_2_LOCALEDEF _SC_PII _SC_PII_XTI _SC_PII_SOCKET _SC_PII_INTERNET _SC_PII_OSI _SC_POLL _SC_SELECT _SC_UIO_MAXIOV 
			   _SC_IOV_MAX _SC_PII_INTERNET_STREAM _SC_PII_INTERNET_DGRAM _SC_PII_OSI_COTS _SC_PII_OSI_CLTS _SC_PII_OSI_M _SC_T_IOV_MAX _SC_THREADS
			   _SC_THREAD_SAFE_FUNCTIONS _SC_GETGR_R_SIZE_MAX _SC_GETPW_R_SIZE_MAX _SC_LOGIN_NAME_MAX _SC_TTY_NAME_MAX _SC_THREAD_DESTRUCTOR_ITERATIONS 
			   _SC_THREAD_KEYS_MAX _SC_THREAD_STACK_MIN _SC_THREAD_THREADS_MAX _SC_THREAD_ATTR_STACKADDR _SC_THREAD_ATTR_STACKSIZE 
			   _SC_THREAD_PRIO_INHERIT _SC_THREAD_PRIO_PROTECT _SC_THREAD_PROCESS_SHARED _SC_NPROCESSORS_CONF _SC_NPROCESSORS_ONLN _SC_PHYS_PAGES 
			   _SC_AVPHYS_PAGES _SC_ATEXIT_MAX _SC_PASS_MAX _SC_2_CHAR_TERM _SC_2_C_VERSION _SC_2_UPE _SC_CHAR_BIT _SC_CHAR_MAX _SC_CHAR_MIN _SC_INT_MAX
			   _SC_INT_MIN _SC_LONG_BIT _SC_WORD_BIT _SC_MB_LEN_MAX _SC_NZERO _SC_SSIZE_MAX _SC_SCHAR_MAX _SC_SCHAR_MIN _SC_SHRT_MAX _SC_SHRT_MIN
			   _SC_UCHAR_MAX _SC_UINT_MAX _SC_ULONG_MAX _SC_USHRT_MAX _SC_NL_ARGMAX _SC_NL_LANGMAX _SC_NL_MSGMAX _SC_NL_NMAX _SC_NL_SETMAX
			   _SC_NL_TEXTMAX _SC_ADVISORY_INFO _SC_BARRIERS _SC_BASE _SC_C_LANG_SUPPORT _SC_C_LANG_SUPPORT_R _SC_CLOCK_SELECTION _SC_CPUTIME
			   _SC_THREAD_CPUTIME _SC_DEVICE_IO _SC_DEVICE_SPECIFIC _SC_DEVICE_SPECIFIC_R _SC_FD_MGMT _SC_FIFO _SC_PIPE _SC_FILE_ATTRIBUTES
			   _SC_FILE_LOCKING _SC_FILE_SYSTEM _SC_MONOTONIC_CLOCK _SC_MULTI_PROCESS _SC_SINGLE_PROCESS _SC_NETWORKING _SC_READER_WRITER_LOCKS
			   _SC_SPIN_LOCKS _SC_REGEXP _SC_REGEX_VERSION _SC_SHELL _SC_SIGNALS _SC_SPAWN _SC_SPORADIC_SERVER _SC_THREAD_SPORADIC_SERVER
			   _SC_SYSTEM_DATABASE _SC_SYSTEM_DATABASE_R _SC_TIMEOUTS _SC_TYPED_MEMORY_OBJECTS _SC_USER_GROUPS _SC_USER_GROUPS_R
			   _SC_2_PBS _SC_2_PBS_ACCOUNTING _SC_2_PBS_LOCATE _SC_2_PBS_MESSAGE _SC_2_PBS_TRACK _SC_SYMLOOP_MAX _SC_STREAMS _SC_2_PBS_CHECKPOINT
			   _SC_HOST_NAME_MAX _SC_TRACE _SC_TRACE_EVENT_FILTER _SC_TRACE_INHERIT _SC_TRACE_LOG _SC_LEVEL1_ICACHE_SIZE _SC_LEVEL1_ICACHE_ASSOC
			   _SC_LEVEL1_ICACHE_LINESIZE _SC_LEVEL1_DCACHE_SIZE _SC_LEVEL1_DCACHE_ASSOC _SC_LEVEL1_DCACHE_LINESIZE _SC_LEVEL2_CACHE_SIZE 
			   _SC_LEVEL2_CACHE_LINESIZE _SC_LEVEL3_CACHE_SIZE _SC_LEVEL3_CACHE_ASSOC _SC_LEVEL3_CACHE_LINESIZE _SC_LEVEL4_CACHE_SIZE 
			   _SC_LEVEL4_CACHE_LINESIZE _SC_IPV6 _SC_RAW_SOCKETS _SC_SS_REPL_MAX _SC_TRACE_EVENT_NAME_MAX _SC_TRACE_NAME_MAX _SC_TRACE_SYS_MAX
			   _SC_TRACE_USER_EVENT_MAX _SC_THREAD_ROBUST_PRIO_INHERIT _SC_THREAD_ROBUST_PRIO_PROTECT _CS_PATH _CS_GNU_LIBC_VERSION 
		           _SC_THREAD_PRIORITY_SCHEDULING _SC_LEVEL2_CACHE_ASSOC _SC_LEVEL4_CACHE_ASSOC _CS_GNU_LIBPTHREAD_VERSION)))

		    (int access (char* int))
		    (int lseek (int int int))
		    (int close (int))
		    (ssize_t read (int void* size_t))
		    (ssize_t write (int void* size_t))
		    (ssize_t pread (int void* size_t int))
		    (ssize_t pwrite (int void* size_t int))
		    (int pipe (int*))
		    (int alarm (int))
		    (int sleep (int))
		    (int pause (void))
		    (int chown (char* int int))
		    (int chdir (char*))
		    (char* getcwd (char* size_t))
		    ;; (deprecated) (char* getwd (char*))
		    (int dup (int))
		    (int dup2 (int int))
		    (void _exit (int))
		    (int pathconf (char* int))
		    (int fpathconf (int int))
		    (int sysconf (int))
		    (size_t confstr (int char* size_t))
		    (int getpid (void))
		    (int getppid (void))
		    (int getpgid (int))
		    (int setpgid (int int))
		    (int setsid (void))
		    (int getsid (int))
		    (int getuid (void))
		    (int geteuid (void))
		    (int getgid (void))
		    (int getegid (void))
		    (int setuid (int))
		    (int setgid (int))
		    (int fork (void))
		    (char* ttyname (int))
		    (int isatty (int))
		    (int link (char* char*))
		    (int unlink (char*))
		    (int rmdir (char*))
		    (int tcgetpgrp (int))
		    (int tcsetpgrp (int int))
		    (char* getlogin (void))
		    (int truncate (char* int))
		    (int ftruncate (int int))

                    (in-C "extern char **environ; 
                           static s7_pointer getenvs(s7_scheme *sc, s7_pointer args)
                           {
                             s7_pointer p;
                             int i;
                             p = s7_nil(sc);
                             for (i = 0; environ[i]; i++)
                               {
                                const char *eq;
                                s7_pointer name, value;
                                eq = strchr((const char *)environ[i], (int)'=');
                                name = s7_make_string_with_length(sc, environ[i], eq - environ[i]);
                                value = s7_make_string(sc, (char *)(eq + 1));
                                p = s7_cons(sc, s7_cons(sc, name, value), p);
                               }
                             return(p);
	                   }")
                    (C-function ("getenvs" getenvs "(getenvs) returns all the environment variables in an alist" 0))

                    ;; perhaps call these as (define* n (path ...) = args?
                    ;; int execve (char* path  char* argv[]  char* envp[])
                    ;; int execv (char* path  char* argv[])
                    ;; int execle (char* path  char* arg  ...)
                    ;; int execl (char* path  char* arg  ...)
                    ;; int execvp (char* file  char* argv[])
                    ;; int execlp (char* file  char* arg  ...)
                    ;; int getgroups (int size  gid_t list[])


		    )
	
		  "" 
		  (list "limits.h" "ctype.h" "errno.h" "float.h" "stdint.h" "locale.h" "stdlib.h" "string.h" "fcntl.h" 
                        "fenv.h" "stdio.h" "sys/utsname.h" "unistd.h")
		  "" "" "libc_s7")
	
	(current-environment))))

*libc*


;; signal.h time.h sys/time? sys/stat
;; dirent utime[easy]? grp? process? sys/wait termios sys/types? env?
;; ftw pwd net netinet netdb

#|


|#
