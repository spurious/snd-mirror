;;; libdl.scm
;;;
;;; tie the dynamic loader library into the *libdl* environment

(require cload.scm)
(provide 'libdl.scm)

(if (not (defined? '*libdl*))
    (define *libdl*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libdl.scm" (curlet)) *libraries*))
	(c-define '((void* dlopen (char* int))
		    (int dlclose (void*))
		    (void* dlsym (void* char*))
		    (char* dlerror (void))
		    (C-macro (int (RTLD_LAZY RTLD_NOW RTLD_BINDING_MASK RTLD_NOLOAD RTLD_DEEPBIND RTLD_GLOBAL RTLD_LOCAL RTLD_NODELETE))))
		  "" "dlfcn.h" "" "" "libdl_s7")
	(curlet))))

*libdl*
;; the loader will return *libdl*
