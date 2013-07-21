#!/home/bil/snd-13/snd
!#

;; this is currently just a hack -- it can be used as follows
;;
;; gdb program | gdb-s7.scm
;;
;; where "program" has s7 embedded in it.
;; 
;; The script annotates the gdb output (stack traces in particular) inserting
;;   in bold-face whatever s7 values it finds.  These are otherwise simply
;;   pointers like args=0xda1c50, but this script turns that into  args[(1/12)]=0xda1c50
;;   (or whatever -- in this case "args" is a list holding the ratio 1/12).
;;   gdb interaction is unchanged -- the result looks like gdb acting normally,
;;   but printing out s7 values whenever they are encountered.
;;
;; The program that reads this script needs to accept input and print results -- the snd version
;;   mentioned above is built --without-gui.  Ideally I'd make an s7 terminal app like ruby. 
;;
;; The script uses xdotool to communicate back to gdb.  This also could be simplified.
;;
;; another temporary assumption: the s7 interpreter is named "sc"


(define escape (integer->char 27))
;(define (red-text str) (format *stderr* "~C[31m~A~C[0m" escape str escape))
(define (bold-text str) (format *stderr* "~C[1m~A~C[22m" escape str escape))

(define (gdb-output)
  (let ((line "")
	(text ""))
    ;; gather up gdb's response and return it
    (call-with-exit
     (lambda (return)
       (do ((c (read-char *stdin*) (read-char *stdin*)))
	   ((eof-object? c)
	    c)
	 (set! text (string-append text (string c)))
	 (if (char=? c #\newline)
	     (set! line "")
	     (begin
	       (set! line (string-append line (string c)))
	       (if (and (char=? c #\space)
			(or (string=? line "(gdb) ")
			    (string=? line "Quit anyway? (y or n) ")))
		   (return text)))))))))

(define (gdb-input)
  ;; echo the user typing immediately
  (do ((c (read-char *stdin*) (read-char *stdin*)))
      ((char=? c #\newline) 
       (write-char c *stderr*))
    (write-char c *stderr*)))

(define (gdb)
  (gdb-input)
  (gdb-output))

(define (annotate-gdb)
  (let ((text (gdb)))
    (let ((frame 0))
      (call-with-input-string text
	(lambda (p)
	  (do ((line (read-line p #t) (read-line p #t))) ; #t=>include \n
	      ((eof-object? line))
	    (if (char=? (line 0) #\#)
		(let ((sp (char-position #\space line)))
		  (set! frame (string->number (substring line 1 sp)))))
	    (let ((pos (string-position "sc=0x" line))
		  (out-pos 0))
	      (if pos
		  (do ((epos (string-position "=0x" (substring line (+ pos 4))) 
			     (string-position "=0x" (substring line (+ pos 4)))))
		      ((not epos))
		    (set! pos (+ epos pos 4))
		    (system (format #f "xdotool type \"frame ~D\"" frame))
		    (system "xdotool key Return")
		    (gdb-output) ; toss the redundant output
		    (let ((name-start pos))
		      (do ((i name-start (- i 1)))
			  ((or (char=? (line i) #\space)
			       (= i 0))
			   (set! name-start (+ i 1))))
		      (let ((name (substring line name-start pos)))
		       (system (format #f "xdotool type \"p s7_is_valid(sc, ~A)\"" name))
		       (system "xdotool key Return")
		       (let ((answer (gdb-output)))
			 (if (string-position "true" answer)
			     (begin
			       (system (format #f "xdotool type \"p s7_object_to_c_string(sc, ~A)\"" name))
			       (system "xdotool key Return")
			       (let ((value (gdb-output)))
				 (let* ((start-pos (char-position #\" value))
					(end-pos (and start-pos (char-position #\" value (+ start-pos 1)))))
				   (if start-pos
				       (begin
					 (write-string (substring line out-pos pos) *stderr*)
					 (bold-text (format #f "[~A]" (substring value (+ start-pos 1) end-pos)))
					 (set! out-pos pos))))))))))))
	      (write-string (substring line out-pos) *stderr*))))))))

(catch #t
  (lambda ()
    (do () () (annotate-gdb)))
	
  (lambda args
    (if (not (eq? (car args) 'wrong-type-arg))
	(format *stderr* "oops: ~A~%" args))
    (exit)))


