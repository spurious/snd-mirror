;;; a script to search for allocation mismatches and unterminated XtVa args 

(define xtva-ctr 0)

(define (for-each-file func files)
  (if (pair? files)
      (let ((count 1))
	(call-with-input-file 
	    (car files)
	  (lambda (file)
	    (let loop ((line (read-line file #t)))
	      (or (eof-object? line)
		  (begin
		    (func line (car files) count)
		    (set! count (+ 1 count))
		    (loop (read-line file #t)))))))
	(for-each-file func (cdr files)))))

(for-each-file 
 (let ((va-state 'before))
   (lambda (line file count)
     (let ((len (string-length line)))
       ;; look for "XtVa..." then NULL);
       (do ((i 0 (+ i 1)))
	   ((= i len))
	 (case (string-ref line i)
	   ((#\return) 
	    (format #t "~A has /r (~A)~%" file count))
	   ((#\X)
	    (if (and (< i (- len 4))
		     (string=? (substring line i (+ i 4)) "XtVa"))
		(set! va-state 'within)))
	    ((#\N)
	     (if (and (< i (- len 6))
		      (string=? (substring line i (+ i 6)) "NULL);"))
		 (begin
		   (if (eq? va-state 'within) (set! xtva-ctr (+ xtva-ctr 1)))
		   (set! va-state 'before))))
	    ((#\;)
	     (if (eq? va-state 'within)
		 (begin
		   (format #t "~A[~A]: ~A~%" file count line)
		   (set! va-state 'before)))))))))
 (list "snd-xutils.c" "snd-xhelp.c" "snd-xfind.c" "snd-xmenu.c" "snd-xdraw.c" "snd-xlistener.c" "snd-xchn.c" 
       "snd-xsnd.c" "snd-xregion.c" "snd-xmain.c" "snd-xmix.c" "snd-xenv.c"
       "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-xfft.c" "snd-xprint.c" "snd-xfile.c" "snd-xen.c" 
       "snd-data.c" "snd-draw.c" "snd-xprefs.c"))


(for-each
 (lambda (func)
   (system (format #f "fgrep ~A *.c > vahi" func))
   (call-with-input-file "vahi"
     (lambda (file)
       (let loop ((line (read-line file #t)))
	 (or (eof-object? line)
	     (let ((len (string-length line))
		   (precount 0)
		   (ok #f)
		   (count 0)
		   (flen (string-length func)))
	       ;; look for * miscounts
	       (call-with-exit
		(lambda (return)
		  (do ((i 0 (+ i 1)))
		      ((= i len))
		    (case (string-ref line i)
		      ((#\*)
		       (set! count (+ 1 count)))
		      ((#\=)
		       (set! count 0))
		      (else
		       (if (and (< i (- len 2))
				(string=? (substring line i (+ i 2)) "/*"))
			   (return #f)
			   (if (and (< i (- len flen))
				    (string=? (substring line i (+ i flen)) func))
			       (begin
				 (set! precount count)
				 (set! count 0))
			       (if (and (< i (- len 6))
					(string=? (substring line i (+ i 6)) "sizeof"))
				   (begin
				     (set! ok #t)
				     (set! count 0))))))))))
	       (if (and ok
			(not (= precount count 0))
			(not (= count (- precount 1))))
		   (format #t "calloc ~D->~D: ~A~%" precount count line))
	       (loop (read-line file #t))))))))
 (list "calloc" "malloc" "realloc"))


;;; look for missing or unused tips

(define (find-if pred l)
  (cond ((null? l) #f)
	((pred (car l)) (car l))
	(else (find-if pred (cdr l)))))
  
(let ((tip-list ())
      (new-tip-list ())
      (warned-list ()))

  (call-with-input-file "wz_data.js"
    (lambda (file)
      (let loop ((line (read-line file #t))) ; concat means leave the final crlf in place
	(or (eof-object? line)
	    (let ((len (string-length line)))
	      (if (and (> len 8)
		       (string=? "var " (substring line 0 4)))
		  (let ((end (do ((i 4 (+ i 1)))
				 ((or (>= i len)
				      (char=? (line i) #\space))
				  i))))
		    (if (< end len)
			(set! tip-list (cons (substring line 4 end) tip-list)))))
	      (loop (read-line file #t)))))))
  
  (for-each
   (lambda (filename)
     (call-with-input-file filename
       (lambda (file)
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (string-length line)))
		 (if (> len 8)
		     (let ((start 0))
		       (do ((i 0 (+ i 1)))
			   ((>= i len))
			 (let ((chr (line i)))
			   (if (char=? chr #\))
			       (let* ((name (substring line (+ 1 start) i))
				      (len (string-length name)))
				 (if (and (> len 4)
					  (string=? "_tip" (substring name (- len 4) len)))
				     (begin
				       (if (and (not (find-if (lambda (str)
								(string=? str name))
							      tip-list))
						(not (find-if (lambda (str)
								(string=? str name))
							      warned-list)))
					   (begin
					     (set! warned-list (cons name warned-list))
					     (format #t ";can't find ~A in wz_data.js~%" name)))
				       (if (not (find-if (lambda (str)
							   (string=? str name))
							 new-tip-list))
					   (set! new-tip-list (cons name new-tip-list))))))
			       (if (and (not (char=? chr #\_))
					(not (char-alphabetic? chr))
					(not (char-numeric? chr)))
				   (set! start i)))))))
		 (loop (read-line file #t))))))))
   (list "snd.html" "extsnd.html" "sndlib.html" "grfsnd.html" "sndclm.html" "sndscm.html" "s7.html" "fm.html"))

  (for-each
   (lambda (name)
     (if (not (find-if (lambda (str)
			 (string=? str name))
		       new-tip-list))
	 (format #t ";defined in wz_data.js but not used: ~A~%" name)))
   tip-list))


(load "lint.scm")

(for-each
 (lambda (filename)
   (if (and (provided? 'gtk3)
	    (provided? 'xg))
       (call-with-input-file filename
	 (lambda (file)
	   (let ((line-number 0))
	     (let loop ((line (read-line file #t)))
	       (or (eof-object? line)
		   (let ((len (string-length line)))
		     (set! line-number (+ line-number 1))
		     (if (> len 8)
			 (let ((start 0))
			   (do ((i 1 (+ i 1)))
			       ((>= i len))
			     (let ((chr (line i)))
			       (if (or (char-whitespace? chr)
				       (char=? chr #\)))
				   (let* ((name (substring line (+ 1 start) i))
					  (name-len (string-length name)))
				     (if (and (or (and (> name-len 4)
						       (or (string-ci=? "gtk_" (substring name 0 4))
							   (string-ci=? "gdk_" (substring name 0 4))))
						  (and (> name-len 6)
						       (or (string-ci=? "pango_" (substring name 0 6))
							   (string-ci=? "cairo_" (substring name 0 6)))))
					      (not (defined? (string->symbol name))))
					 (format #t "~A (~A[~D]) is not defined~%" name filename line-number))))
			       (if (and (not (char=? chr #\_))
					(not (char-alphabetic? chr))
					(not (char-numeric? chr)))
				   (set! start i))))))
		     (loop (read-line file #t)))))))))
;   (if (string=? (substring filename (- (length filename) 3)) "scm")
;       (lint filename))
   )
 (list 
  "analog-filter.scm"
  "animals.scm"
  "autosave.scm"
  "bess.scm"
  "bess1.scm"
  "big-gens.scm"
  "binary-io.scm"
  "bird.scm"
  "clean.scm"
  "clm-ins.scm"
  "clm23.scm"
  "dlocsig.scm"
  "draw.scm"
  "dsp.scm"
  "edit-menu.scm"
  "edit123.scm"
  "effects-utils.scm"
  "env.scm"
  "enved.scm"
  "examp.scm"
  "expandn.scm"
  "extensions.scm"
  "fade.scm"
  "fft-menu.scm"
  "fmv.scm"
;  "frame.scm"
  "freeverb.scm"
  "fullmix.scm"
  "generators.scm"
  "grani.scm"
  "gtk-effects-utils.scm"
  "gtk-effects.scm"
  "hooks.scm"
  "index.scm"
  "jcrev.scm"
  "jcvoi.scm"
  "lint.scm"
  "maraca.scm"
  "marks-menu.scm"
  "marks.scm"
  "maxf.scm"
  "misc.scm"
  "mix.scm"
;  "mixer.scm"
  "moog.scm"
  "musglyphs.scm"
  "nb.scm"
  "new-backgrounds.scm"
  "new-effects.scm"
  "noise.scm"
  "nrev.scm"
  "numerics.scm"
  "peak-phases.scm"
  "piano.scm"
  "play.scm"
  "poly.scm"
  "prc95.scm"
  "primes.scm"
  "pvoc.scm"
  "rgb.scm"
;  "rtio.scm"
  "rubber.scm"
  "s7-slib-init.scm"
  "s7test.scm"
  "selection.scm"
  "singer.scm"
  "snd-gl.scm"
  "snd-gtk.scm"
  "snd-motif.scm"
  "snd-test.scm"
  "snd11.scm"
  "snd12.scm"
  "snddiff.scm"
  "sndlib-ws.scm"
  "sndwarp.scm"
  "special-menu.scm"
  "spectr.scm"
  "spokenword.scm"
  "stochastic.scm"
  "strad.scm"
  "v.scm"
  "write.scm"
  "ws.scm"
  "xm-enved.scm"
  "zip.scm"
  
  "snd.html"
  "sndscm.html"
  "grfsnd.html"
  "extsnd.html"
  "sndclm.html"
  "fm.html"
  "s7.html"
  "sndlib.html"
  ))


#|
(for-each
 (lambda (filename)
   (call-with-input-file filename
     (lambda (file)
       (let ((line-number 0)
	     (last-name ""))
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (string-length line)))
		 (set! line-number (+ line-number 1))
		 (if (> len 0)
		     (let ((start #f))
		       (do ((i 0 (+ i 1)))
			   ((>= i len))
			 (let ((chr (line i)))
			   (if (not (char-whitespace? chr))
			       (if (not start)
				   (set! start i))
			       (if start
				   (let* ((name (substring line start i))
					  (name-len (string-length name)))
				     ;(format #t "~C: ~A~%" chr name)
				     (if (and (> name-len 0)
					      (char-alphabetic? (name 0))
					      (string=? name last-name))
					 (format #t ";~A[~D]: ~A repeats in ~A~%" filename line-number name line))
				     (set! last-name name)
				     (set! start #f))))))))
		 (loop (read-line file #t)))))))))
 (list
  "snd.html"
  "sndscm.html"
  "grfsnd.html"
  "extsnd.html"
  "sndclm.html"
  "fm.html"
  "s7.html"
  "sndlib.html"
  ))
|#


#|
(format #t "--------------------------------------------------------------------------------~%")
(let ((png-files (directory->list "/home/bil/cl/pix"))
      (baddies ()))
  (for-each
   (lambda (file)
     (if (and (not (directory? file))
	      (not (zero? (system (format #f "fgrep ~A *.html" file)))))
	 (set! baddies (cons file baddies))))
   png-files)
  (if (not (null? baddies))
      (begin
	(format #t "--------------------------------------------------------------------------------~%")
	(format #t ";unused pix/png: ~{~A ~}~%" baddies)
	(format #t "--------------------------------------------------------------------------------~%"))))
|#

#|
;;; check for incorrect port_write_string lengths -- is confused by \" 
(call-with-input-file "s7.c"
  (lambda (file)
    (let ((count 0))
      (let loop ((line (read-line file #t)))
	(or (eof-object? line)
	    (let ((pos (string-position "port_write_string(" line)))
	      (set! count (+ count 1))
	      (if pos
		  (let ((qpos (char-position #\" (substring line (+ pos 1)))))
		    (if qpos
			(let ((npos (char-position #\, (substring line (+ pos qpos 2)))))
			  (if npos
			      (let ((len (- npos 1))
				    (new-line (substring line (+ pos qpos 2 npos))))
				(let ((ccpos (char-position #\, (substring new-line 1))))
				  (if ccpos
				      (let ((clen (string->number (substring new-line 2 (+ ccpos 1)))))
					(if (and clen
						 (not (= clen len)))
					    (format *stderr* "[~D]: ~A: length ~A ~A~%" count 
						    (substring line (char-position #\p line) (char-position #\; line))
						    len clen)))))))))))
	      (loop (read-line file #t))))))))
|#

(exit)


