(define-module (gmeteor))

(let ((gmeteor-dir "/usr/local/share/gmeteor"))
  (or (member gmeteor-dir %load-path)
      (set! %load-path (cons gmeteor-dir %load-path))))
      
(define-public gmeteor-libdir "/usr/local/lib")

(use-modules (gmeteor-core) 
	     (gmeteor-simple)
	     (gmeteor-getopt)
	     (gmeteor-lib))

(define-module (guile)
  :use-module (gmeteor)
  :use-module (gmeteor-core) 
  :use-module (gmeteor-simple)
  :use-module (gmeteor-getopt)
  :use-module (gmeteor-lib))

