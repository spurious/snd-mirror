;;; a script to search for allocation mismatches and unterminated XtVa args 

(if (not (defined? 'sort))
    (begin
      (define sort!
	;; http://www.math.grin.edu/~stone/events/scheme-workshop/quicksort.html
	(lambda (ls . opt)
	  (let* ((precedes? (if (null? opt) < (car opt)))

		 (partition
		  (lambda (ls pivot)
		    (let loop ((rest ls)
			       (smalls '())
			       (larges '()))
		      (if (null? rest)
			  (cons smalls larges)
			  (let ((fore (car rest))
				(aft (cdr rest)))
			    (if (precedes? fore pivot)
				(loop aft (cons fore smalls) larges)
				(loop aft smalls (cons fore larges)))))))))

	    (let qs ((rest ls))
	      (if (or (null? rest)
		      (null? (cdr rest)))
		  rest
		  (let* ((pivot (car rest))
			 (parts (partition (cdr rest) pivot))
			 (smalls (car parts))
			 (larges (cdr parts)))
		    (append (qs smalls)
			    (cons pivot (qs larges)))))))))

      (define (sort lst . opt) (sort! (append lst '()) (if (null? opt) < (car opt))))
      ))

(define xtva-ctr 0)

(define (for-each-file func files)
  (if (not (null? files))
      (let ((count 1))
	;(display (format #f "~%~A" (car files)))
	(call-with-input-file 
	    (car files)
	  (lambda (file)
	    (let loop ((line (read-line file 'concat)))
	      (or (eof-object? line)
		  (begin
		    (func line (car files) count)
		    (set! count (+ 1 count))
		    (loop (read-line file 'concat)))))))
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
       "snd-xsnd.c" "snd-xregion.c" "snd-xdrop.c" "snd-xmain.c" "snd-xmix.c" "snd-xrec.c" "snd-xenv.c" "snd-gxutils.c" 
       "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-xfft.c" "snd-xprint.c" "snd-xfile.c" "snd-xen.c" 
       "snd-data.c" "snd-draw.c" "snd-xprefs.c" ))


(for-each
 (lambda (func)
   (system (format #f "fgrep ~A *.c > vahi" func))
   (call-with-input-file "vahi"
     (lambda (file)
       (let loop ((line (read-line file 'concat)))
	 (or (eof-object? line)
	     (let ((len (string-length line))
		   (precount 0)
		   (ok #f)
		   (count 0)
		   (flen (string-length func)))
	       ;; look for * miscounts
	       (call-with-exit
		(lambda (break)
		  (do ((i 0 (+ 1 i)))
		      ((= i len))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\*)
			  (set! count (+ 1 count))
			  (if (char=? ch #\=)
			      (set! count 0)
			      (if (and (< i (- len 2))
				       (string=? (substring line i (+ i 2)) "/*"))
				  (break #f)
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
	       (loop (read-line file 'concat))))))))
 (list "CALLOC" "MALLOC" "REALLOC" "calloc" "malloc" "realloc"))

(system "fgrep 'setf ' *.scm > vahi")
(call-with-input-file "vahi"
  (lambda (file)
    (let loop ((line (read-line file 'concat)))
      (or (eof-object? line)
	  (begin
	    (display line)
	    (loop (read-line file 'concat)))))))

(let ((flen (string-length "snd-display  "))
      (line-ctr 0))
  (call-with-input-file "snd-test.scm"
    (lambda (file)
      (let loop ((line (read-line file 'concat)))
	(or (eof-object? line)
	    (let ((len (string-length line)))
	      (set! line-ctr (+ 1 line-ctr))
	      (if (> len (+ flen 3))
		  (do ((i 0 (+ 1 i)))
		      ((= i (- len flen 3)))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\s)
			  (if (string=? (substring line i (+ i flen)) "snd-display \"")
			      (if (not (char=? (string-ref line (+ i flen)) #\;))
				  (display (format #f "~A: ~A~%" line-ctr line))))))))
	      (loop (read-line file 'concat))))))))
#!
(define (directory->list dir)
  (let ((dport (opendir dir)))
    (let loop ((entry (readdir dport))
	       (files '()))
      (if (not (eof-object? entry))
	  (loop (readdir dport) (cons entry files))
	  (begin
	    (closedir dport)
	    (reverse! files))))))

(define (grep rx strings)
  (define (filter-list pred? objects)
    (let loop ((objs objects)
	       (result '()))
      (cond ((null? objs) (reverse! result))
	    ((pred? (car objs)) (loop (cdr objs) (cons (car objs) result)))
	    (else (loop (cdr objs) result)))))
  (let ((r (make-regexp rx)))
    (filter-list (lambda (x) (regexp-exec r x)) strings)))

(for-each-file 
 (lambda (line file count)
   (let ((len (string-length line)))
     (call-with-exit
      (lambda (return)
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (let ((ch (string-ref line i)))
	    (if (char=? ch (integer->char #o015))
		(begin
		  (display (format #f "~A has /r (~A)~%" file count))
		  (return)))))))))
 (sort (grep
	(format #f "\\.(~{~A~^|~})$" (list "scm" "rb"))
	(directory->list "."))
       string<?))

;;; look for out-of-place S_* strings -- lots of false positives!
(for-each-file 
 (let ((gs-state 'before)
       (func-name "")
       (func-name-len 0))
   (lambda (line file count)
     (let ((len (string-length line)))
       ;; look for "\nstatic XEN g_"
       (if (eq? gs-state 'before)
	   (if (and (> len 14)
		    (string=? (substring line 0 13) "static XEN g_"))
	       (let ((end #f))
		 (do ((k 14 (+ 1 k)))
		     ((or (>= k len)
			  (char=? (string-ref line k) #\()
			  (char=? (string-ref line k) #\space)))
		   (set! end k))
		 (if (number? end)
		     (begin
		       (set! gs-state 'within)
		       (set! func-name (substring line 13 (+ 1 end)))
		       (if (and (> (string-length func-name) 5)
				(string=? "set_" (substring func-name 0 4)))
			   (set! func-name (substring func-name 4)))
		       (set! func-name-len (string-length func-name))))))
	   (if (char=? (string-ref line 0) #\})
	       (set! gs-state 'before)
	       (if (> len 10)
		   (let ((ch (string-ref line 0))
			 (ch1 (string-ref line 1)))
		     ;; look for S_<not func name>
		     (do ((i 2 (+ 1 i)))
			 ((= i (- len 6)))
		       (let ((ch2 (string-ref line i)))
			 (if (and (char=? ch1 #\S)
				  (char=? ch2 #\_)
				  (not (char-numeric? ch))
				  (not (char-alphabetic? ch)))
			     (if (and (<= (+ i 1 func-name-len) len)
				      (not (string=? "_setB" (substring line i (+ i 5)))))
				 (if (not (string=? func-name (substring line (+ 1 i) (+ i 1 func-name-len))))
				     (let ((start (+ 1 i))
					   (end (min (- len 1) (+ i 1 func-name-len))))
				       (do ((k end (+ k 1)))
					   ((or (>= k len)
						(let ((ch3 (string-ref line k)))
						  (and (not (char-numeric? ch3))
						       (not (char-alphabetic? ch3))
						       (not (char=? ch3 #\_))))))
					 (set! end k))
				       (display (format #f ";g_~A has S_~A~%" 
							func-name 
							(substring line start (+ 1 end))))))))
			 (set! ch ch1)
			 (set! ch1 ch2))))))))))
 (list
  "headers.c" "audio.c" "io.c" "sound.c" "clm.c" "xen.c" "vct.c" "sndlib2xen.c" "clm2xen.c" "midi.c" 
  "snd-io.c" "snd-utils.c" "snd-listener.c" "snd-error.c" "snd-completion.c" "snd-menu.c" "snd-axis.c" 
  "snd-data.c" "snd-fft.c" "snd-marks.c" "snd-file.c" "snd-edits.c" "snd-chn.c" "snd-dac.c" "snd-region.c" 
  "snd-select.c" "snd-find.c" "snd-snd.c" "snd-help.c" "snd-main.c" "snd-print.c" "snd-trans.c" "snd-mix.c" 
  "snd.c" "snd-env.c" "snd-xen.c" "snd-ladspa.c" "snd-kbd.c" "snd-sig.c" "snd-draw.c" "run.c" 
  "snd-xutils.c" "snd-xhelp.c" "snd-xfind.c" "snd-xmenu.c" "snd-xdraw.c" "snd-xlistener.c" "snd-xchn.c" 
  "snd-xsnd.c" "snd-xregion.c" "snd-xdrop.c" "snd-xmain.c" "snd-xmix.c" "snd-xrec.c" "snd-xenv.c" 
  "snd-gxutils.c" "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-xfft.c" "snd-xprint.c" "snd-xfile.c" 
  "snd-xprefs.c" "xm.c" "snd-gutils.c" "snd-ghelp.c" "snd-gfind.c" "snd-gmenu.c" "snd-gdraw.c" 
  "snd-glistener.c" "snd-gchn.c" "snd-gsnd.c" "snd-gregion.c" "snd-gdrop.c" "snd-gmain.c" "snd-gmix.c" 
  "snd-grec.c" "snd-genv.c" "snd-gxutils.c" "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-gfft.c" "snd-gprint.c" 
  "snd-gfile.c" "snd-gprefs.c" "xg.c" "gl.c" "snd-nogui.c")
)

;;; similarly look for \n... (... with help strings causing confusion
(for-each-file 
 (lambda (line file count)
   (let ((len (string-length line)))
     (if (char-alphabetic? (string-ref line 0))
	 (let ((last-char (string-ref line 0))
	       (ok #f))
	   (do ((i 1 (+ 1 i)))
	       ((or ok (= i len)))
	     (let ((this-char (string-ref line i)))
	       (if (and (char=? last-char #\space)
			(char=? this-char #\())
		   (begin
		     (display (format #f "~A[~A]: ~A~%" file count line))
		     (set! ok #t)))
	       (if (char=? last-char #\()
		   (set! ok #t))
	       (set! last-char this-char)))))))
 (list ...))

!#


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
      (let loop ((line (read-line file 'concat))) ; concat means leave the final crlf in place
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
	      (loop (read-line file 'concat)))))))
  
  (for-each
   (lambda (filename)
     (call-with-input-file filename
       (lambda (file)
	 (let loop ((line (read-line file 'concat)))
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
		 (loop (read-line file 'concat))))))))
   (list "snd.html" "extsnd.html" "sndlib.html" "grfsnd.html" "sndclm.html" "sndscm.html"))

  (for-each
   (lambda (name)
     (if (not (find-if (lambda (str)
			 (string=? str name))
		       new-tip-list))
	 (display (format #f ";defined in wz_data.js but not used: ~A~%" name))))
   tip-list))
  
   
(exit)
