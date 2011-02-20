;;; a script to search for allocation mismatches and unterminated XtVa args 

(define xtva-ctr 0)

(define (for-each-file func files)
  (if (not (null? files))
      (let ((count 1))
	;(display (format #f "~%~A" (car files)))
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
       (do ((i 0 (+ 1 i)))
	   ((= i len))
	 (let ((ch (string-ref line i)))
	   (if (char=? ch (integer->char #o015))
	       (display (format #f "~A has /r (~A)~%" file count)))
	   (if (char=? ch #\X)
	       (if (and (< i (- len 4))
			(string=? (substring line i (+ i 4)) "XtVa"))
		   (set! va-state 'within))
	       (if (char=? ch #\N)
		   (if (and (< i (- len 6))
			    (string=? (substring line i (+ i 6)) "NULL);"))
		       (begin
			 (if (eq? va-state 'within) (set! xtva-ctr (+ xtva-ctr 1)))
			 (set! va-state 'before)))
		   (if (char=? ch #\;)
		       (if (eq? va-state 'within)
			   (begin
			     (display (format #f "~A[~A]: ~A~%" file count line))
			     (set! va-state 'before)))))))))))

 (list "snd-xutils.c" "snd-xhelp.c" "snd-xfind.c" "snd-xmenu.c" "snd-xdraw.c" "snd-xlistener.c" "snd-xchn.c" 
       "snd-xsnd.c" "snd-xregion.c" "snd-xdrop.c" "snd-xmain.c" "snd-xmix.c" "snd-xrec.c" "snd-xenv.c"
       "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-xfft.c" "snd-xprint.c" "snd-xfile.c" "snd-xen.c" 
       "snd-data.c" "snd-draw.c" "snd-xprefs.c" ))


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
		  (do ((i 0 (+ 1 i)))
		      ((= i len))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\*)
			  (set! count (+ 1 count))
			  (if (char=? ch #\=)
			      (set! count 0)
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
					    (set! count 0)))))))))))
	       (if (and ok
			(not (= precount count 0))
			(not (= count (- precount 1))))
		   (display (format #f "calloc ~D->~D: ~A~%" precount count line)))
	       (loop (read-line file #t))))))))
 (list "CALLOC" "MALLOC" "REALLOC" "calloc" "malloc" "realloc"))


;;; look for missing or unused tips

(if (not (defined? 'find-if))
    (define (find-if pred l)
      "(find-if func lst) scans 'lst' for any element that 'func' likes"
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l))))))
  
(let ((tip-list '())
      (new-tip-list '())
      (warned-list '()))

  (call-with-input-file "wz_data.js"
    (lambda (file)
      (let loop ((line (read-line file #t))) ; concat means leave the final crlf in place
	(or (eof-object? line)
	    (let ((len (string-length line)))
	      (if (and (> len 8)
		       (string=? "var " (substring line 0 4)))
		  (let ((end (do ((i 4 (+ 1 i)))
				 ((or (>= i len)
				      (char=? (string-ref line i) #\space))
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
		       (do ((i 0 (+ 1 i)))
			   ((>= i len))
			 (let ((chr (string-ref line i)))
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
					     (display (format #f ";can't find ~A in wz_data.js~%" name))))
				       (if (not (find-if (lambda (str)
							   (string=? str name))
							 new-tip-list))
					   (set! new-tip-list (cons name new-tip-list))))))
			       (if (and (not (char=? chr #\_))
					(not (char-alphabetic? chr))
					(not (char-numeric? chr)))
				   (set! start i)))))))
		 (loop (read-line file #t))))))))
   (list "snd.html" "extsnd.html" "sndlib.html" "grfsnd.html" "sndclm.html" "sndscm.html"))

  (for-each
   (lambda (name)
     (if (not (find-if (lambda (str)
			 (string=? str name))
		       new-tip-list))
	 (display (format #f ";defined in wz_data.js but not used: ~A~%" name))))
   tip-list))


;;; look for untranslated gtk2 stuff

(if (and (provided? 'gtk3)
	 (provided? 'xg))
    (for-each
     (lambda (filename)
       (call-with-input-file filename
	 (lambda (file)
	   (let ((line-number 0))
	     (let loop ((line (read-line file #t)))
	       (or (eof-object? line)
		   (let ((len (string-length line)))
		     (set! line-number (+ line-number 1))
		     (if (> len 8)
			 (let ((start 0))
			   (do ((i 1 (+ 1 i)))
			       ((>= i len))
			     (let ((chr (string-ref line i)))
			       (if (or (char-whitespace? chr)
				       (char=? chr #\)))
				   (let* ((name (substring line (+ 1 start) i))
					  (name-len (string-length name)))
				     (if (or (and (> name-len 4)
						  (or (string=? "gtk_" (substring name 0 4))
						      (string=? "GTK_" (substring name 0 4))
						      (string=? "gdk_" (substring name 0 4))
						      (string=? "GDK_" (substring name 0 4))))
					     (and (> name-len 6)
						  (or (string=? "pango_" (substring name 0 6))
						      (string=? "PANGO_" (substring name 0 6))
						      (string=? "cairo_" (substring name 0 6))
						      (string=? "CAIRO_" (substring name 0 6)))))
					 (if (not (defined? (string->symbol name)))
					     (format #t "~A (~A[~D]) is not defined~%" name filename line-number)))))
			       (if (and (not (char=? chr #\_))
					(not (char-alphabetic? chr))
					(not (char-numeric? chr)))
				   (set! start i))))))
		     (loop (read-line file #t)))))))))
     (list 
      "analog-filter.scm"
      "musglyphs.scm"
      "animals.scm"
      "nb.scm"
      "autosave.scm"
      "new-backgrounds.scm"
      "bess1.scm"
      "new-effects.scm"
      "bess.scm"
      "noise.scm"
      "big-gens.scm"
      "nrev.scm"
      "binary-io.scm"
      "numerics.scm"
      "bird.scm"
      "oscope.scm"
      "clean.scm"
      "panic.scm"
      "clm23.scm"
      "peak-phases.scm"
      "clm-ins.scm"
      "piano.scm"
      "dlocsig.scm"
      "play.scm"
      "draw.scm"
      "poly.scm"
      "dsp.scm"
      "popup.scm"
      "edit123.scm"
      "prc95.scm"
      "edit-menu.scm"
      "pretty-print.scm"
      "effects-utils.scm"
      "primes.scm"
      "enved.scm"
      "pvoc.scm"
      "env.scm"
      "rgb.scm"
      "examp.scm"
      "rtio.scm"
      "expandn.scm"
      "rubber.scm"
      "extensions.scm"
      "s7-slib-init.scm"
      "fade.scm"
      "s7test.scm"
      "fft-menu.scm"
      "selection.scm"
      "fmv.scm"
      "singer.scm"
      "frame.scm"
      "snd10.scm"
      "freeverb.scm"
      "snd11.scm"
      "fullmix.scm"
      "snd9.scm"
      "generators.scm"
      "snddiff.scm"
      "grani.scm"
      "snd-gl.scm"
      "gtk-effects.scm"
      "snd-gtk.scm"
      "gtk-effects-utils.scm"
      "sndlib-ws.scm"
      "gtk-popup.scm"
      "snd-motif.scm"
      "hooks.scm"
      "snd-test.scm"
      "index.scm"
      "sndwarp.scm"
      "jcrev.scm"
      "special-menu.scm"
      "jcvoi.scm"
      "spectr.scm"
      "kmenu.scm"
      "spokenword.scm"
      "maraca.scm"
      "stochastic.scm"
      "marks-menu.scm"
      "strad.scm"
      "marks.scm"
      "maxf.scm"
      "v.scm"
      "misc.scm"
      "ws.scm"
      "mixer.scm"
      "xm-enved.scm"
      "mix.scm"
      "zip.scm"
      "moog.scm"
      
      "snd.html"
      "sndscm.html"
      "grfsnd.html"
      "extsnd.html"
      "libxm.html"
      )))

(exit)
