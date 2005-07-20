#!/usr/local/bin/guile -s
!#

;;; a script to search for allocation mismatches and unterminated XtVa args 

(use-modules (ice-9 format))
(if (not (defined? 'read-line)) (use-modules (ice-9 rdelim)))

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
		    (set! count (1+ count))
		    (loop (read-line file 'concat)))))))
	(for-each-file func (cdr files)))))

(for-each-file 
 (let ((va-state 'before))
   (lambda (line file count)
     (let ((len (string-length line)))
       ;; look for "XtVa..." then NULL);
       (do ((i 0 (1+ i)))
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
 (list 
"snd-xutils.c" "snd-xhelp.c" "snd-xfind.c" "snd-xmenu.c" "snd-xdraw.c" "snd-xlistener.c" "snd-xchn.c" "snd-xsnd.c" "snd-xregion.c" "snd-xdrop.c" "snd-xmain.c" "snd-xmix.c" "snd-xrec.c" "snd-xenv.c" "snd-gxutils.c" "snd-gxbitmaps.c" "snd-gxcolormaps.c" "snd-xfft.c" "snd-xprint.c" "snd-xfile.c" "snd-xxen.c" "snd-xen.c" "snd-data.c" "snd-draw.c" ))

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
	       (call-with-current-continuation
		(lambda (break)
		  (do ((i 0 (1+ i)))
		      ((= i len))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\*)
			  (set! count (1+ count))
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
			(not (= count (1- precount))))
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
	      (set! line-ctr (1+ line-ctr))
	      (if (> len (+ flen 3))
		  (do ((i 0 (1+ i)))
		      ((= i (- len flen 3)))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\s)
			  (if (string=? (substring line i (+ i flen)) "snd-display \"")
			      (if (not (char=? (string-ref line (+ i flen)) #\;))
				  (display (format #f "~A: ~A~%" line-ctr line))))))))
	      (loop (read-line file 'concat))))))))

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
     (call-with-current-continuation
      (lambda (return)
	(do ((i 0 (1+ i)))
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
