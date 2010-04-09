;;; ./snd -noinit indexer.scm

;(load "clm23.scm")		   
;(load "edit123.scm")		   
(load "snd7.scm")
(load "snd11.scm")
(load "snd6.scm")
(load "snd9.scm")
(load "snd8.scm")
(load "snd10.scm")
(load "analog-filter.scm")	   
;;; (load "new-backgrounds.scm")
(load "animals.scm")		   
;;; (load "new-effects.scm")
(load "autosave.scm")	   
(load "noise.scm")
;(load "bess1.scm")		   
(load "nrev.scm")
;(load "bess.scm")		  
(load "numerics.scm")
(load "big-gens.scm")	  
;(load "oscope.scm")
(load "bird.scm")		   
;;; (load "panic.scm")
(load "clean.scm")	
(load "peak-phases.scm")
(load "piano.scm")
(load "clm-ins.scm")		   
(load "play.scm")
(load "dlocsig.scm")		   
(load "poly.scm")
(load "draw.scm")		  
(load "popup.scm")
(load "dsp.scm")		   
(load "prc95.scm")
(load "pretty-print.scm")
;;; (load "edit-menu.scm")	   
(load "primes.scm")
;;; (load "effects-utils.scm")	   
(load "pvoc.scm")
(load "enved.scm")		   
(load "rgb.scm")
(load "env.scm")		   
(load "rtio.scm")
(load "examp.scm")		   
(load "rubber.scm")
(load "expandn.scm")		   
;(load "s7-slib-init.scm")
(load "extensions.scm")	   
;(load "s7test.scm")
(load "fade.scm")		   
(load "selection.scm")
;;; (load "fft-menu.scm")	   
(load "singer.scm")
;;; (load "fmv.scm")		   
(load "frame.scm")		   
(load "freeverb.scm")	   
(load "fullmix.scm")		   
(load "generators.scm")	   
(load "grani.scm")		   
;;; (load "gtk-effects.scm")	   
(load "snddiff.scm")
;;; (load "gtk-effects-utils.scm")  
;;; (load "snd-gl.scm")
;;; (load "gtk-popup.scm")	   
;;; (load "snd-gtk.scm")
(load "hooks.scm")		   
;(load "sndlib-ws.scm")
(load "index.scm")		   
;;; (load "snd-motif.scm")
(load "jcrev.scm")		   
;(load "snd-test.scm")
(load "jcvoi.scm")		   
(load "sndwarp.scm")

;;; (load "kmenu.scm")	

;;; (load "special-menu.scm")
(load "maraca.scm")		   
(load "spectr.scm")
;;; (load "marks-menu.scm")	   
(load "spokenword.scm")
(load "marks.scm")		   
(load "stochastic.scm")
(load "maxf.scm")		   
(load "strad.scm")
;;; (load "misc.scm")		   
;;; (load "toolbar.scm")
(load "mixer.scm")		  
(load "v.scm")
(load "mix.scm")		   
(load "ws.scm")
(load "moog.scm")		   
;;; (load "xm-enved.scm")
(load "musglyphs.scm")	   
(load "zip.scm")
(load "nb.scm")

(let ()
  (define (report-places)
    (let ((names ())
	  (places ()))
      
      (define (where-is func)
	(let* ((e (procedure-environment func))
	       (binding (and (pair? e)
			     (pair? (car e))
			     (assoc '__func__ (car e))))
	       (addr (and (pair? binding)
			  (cdr binding))))
	  (if (pair? addr)
	      (cadr addr)
	      #f)))
      
      (define (apropos-1 alist)
	(for-each
	 (lambda (binding)
	   (let ((symbol (car binding))
		 (value (cdr binding)))
	     (if (procedure? value)
		 (let ((file (where-is value)))
		   (if (and file
			    (not (string=? file "~/.snd_s7"))
			    (not (string=? file "/home/bil/.snd_s7"))
			    (not (string=? file "t.scm"))
			    (not (string=? file "/home/bil/cl/t.scm"))
			    )
		       (begin
			 (set! names (cons (cons symbol file) names))
			 (if (not (member file places))
			     (set! places (cons file places)))))))))
	 alist))
      
      (for-each
       (lambda (frame)
	 (if (vector? frame)
	     (let ((len (vector-length frame)))
	       (do ((i 0 (+ i 1)))
		   ((= i len))
		 (apropos-1 (vector-ref frame i))))
	     (apropos-1 frame)))
       (global-environment))
      
      (let ((name-len (length names))
	    (file-len (length places)))
	(do ((i 0 (+ i 1)))
	    ((= i file-len))
	  (let* ((pos -1)
		 (place (list-ref places i))
		 (slen (length place)))
	    (do ((k 0 (+ k 1)))
		((= k slen))
	      (if (char=? (string-ref place k) #\/)
		  (set! pos k)))
	    (if (> pos -1)
		(list-set! places i (substring place pos)))))
	
	(set! places (sort! places string<?))
	(set! names (sort! names (lambda (a b)
				   (string<? (symbol->string (car a)) 
					     (symbol->string (car b))))))
	
	(call-with-output-file "indexer.data"
	  (lambda (p)
	    
	    (define (find-file name)
	      (call-with-exit
	       (lambda (return)
		 (do ((i 0 (+ i 1)))
		     ((= i file-len) (format #t "oops! ~A~%" name) 0)
		   (if (string=? name (places i))
		       (return i))))))
	    
	    (format p "#define AUTOLOAD_FILES ~D~%~%" file-len)
	    (format p "static const char *autoload_files[AUTOLOAD_FILES] = {~%  ")
	    (do ((i 0 (+ i 1)))
		((= i (- file-len 1)))
	      (if (and (> i 0)
		       (= (modulo i 6) 0))
		  (format p "~S, ~%  " (places i))
		  (format p "~S, " (places i))))
	    (format p "~S};~%~%" (places (- file-len 1)))
	    
	    (format p "#define AUTOLOAD_NAMES ~D~%~%" name-len)
	    (format p "static const char *autoload_names[AUTOLOAD_NAMES] = {~%  ")
	    (do ((i 0 (+ i 1)))
		((= i (- name-len 1)))
	      (if (and (> i 0)
		       (= (modulo i 4) 0))
		  (format p "~S, ~%  " (symbol->string (car (names i))))
		  (format p "~S, " (symbol->string (car (names i))))))
	    (format p "~S};~%~%" (symbol->string (car (names (- name-len 1)))))
	    
	    (format p "static int autoload_indices[AUTOLOAD_NAMES] = {~%  ")
	    (do ((i 0 (+ i 1)))
		((= i (- name-len 1)))
	      (if (and (> i 0)
		       (= (modulo i 24) 0))
		  (format p "~D, ~%  " (find-file (cdr (names i))))
		  (format p "~D, " (find-file (cdr (names i))))))
	    (format p "~D};~%~%" (find-file (cdr (names (- name-len 1))))))))))
  
  (report-places))
(exit)



;;; TODO: snd-motif|gtk (or neither) based on gui choice

