;;; with-sound and friends

(use-modules (ice-9 optargs) (ice-9 format))
(if (not (defined? 'local-variables)) (load-from-path "debug.scm"))

;;; changed default variable names 3-Apr-03 for Common Music's benefit
;;;   *clm-channels* is the default number of with-sound output chans in
;;;   both CL and Scheme now.  *channels* (the old name) was a dynamic
;;;   binding in CL to implement dynamic-wind by hand, I think.


;;; -------- with-sound debugger --------
;;;
;;; slightly different from the snd-break debugger because there are several
;;;   ways one might want to get out of the with-sound (w/out rev etc).

(define *ws-stacks* '())
(define *ws-stack* #f)
(define *ws-continues* '())
(define *ws-continue* #f)
(define *ws-finishes* '())
(define *ws-finish* #f)
(define *ws-top-level-prompt* (listener-prompt))

(define (ws-prompt)
  (let ((stack-len (length *ws-stacks*)))
    (if (> stack-len 1)
	(format #f "ws(~D)~A" (1- stack-len) *ws-top-level-prompt*)
	(if (> stack-len 0)
	    (format #f "ws~A" *ws-top-level-prompt*)
	    *ws-top-level-prompt*))))

(define (pop-ws)
  (if (not (null? *ws-continues*))
      (begin
	(set! *ws-continue* (car *ws-continues*))
	(set! *ws-continues* (cdr *ws-continues*)))
      (set! *ws-continue* #f))
  (if (not (null? *ws-stacks*))
      (begin
	(set! *ws-stack* (car *ws-stacks*))
	(set! *ws-stacks* (cdr *ws-stacks*)))
      (set! *ws-stack* #f))
  (if (not (null? *ws-finishes*))
      (begin
	(set! *ws-finish* (car *ws-finishes*))
	(set! *ws-finishes* (cdr *ws-finishes*)))
      (set! *ws-continue* #f))
  (set! (listener-prompt) (ws-prompt))
  (snd-print (format #f "~%~A" (listener-prompt)))
  (goto-listener-end))

(define (push-ws continue finish stack)
  (if (null? *ws-continues*) (set! *ws-top-level-prompt* (listener-prompt)))
  (set! *ws-continues* (cons *ws-continue* *ws-continues*))
  (set! *ws-continue* continue)
  (set! *ws-stacks* (cons *ws-stack* *ws-stacks*))
  (set! *ws-stack* stack)
  (set! *ws-finishes* (cons *ws-finish* *ws-finishes*))
  (set! *ws-finish* finish)
  (set! (listener-prompt) (ws-prompt))
  (snd-print (format #f "~%~A" (listener-prompt)))
  (goto-listener-end))

(define* (ws-go #:optional val)
  "(ws-go (val #f)) tries to continue from the point at which with-sound was interrupted. 'val' is \
the value returned by the interrupt (ws-interrupt? normally)"
  (let ((current-continuation *ws-continue*))
    (pop-ws)
    (if (continuation? current-continuation)
	(current-continuation val)
	";no notelist to continue?")))

(define (ws-quit)
  "(ws-quit) exits a with-sound context (without reverb)"
  (let ((current-finish *ws-finish*))
    (pop-ws)
    (if (continuation? current-finish)
	(current-finish #f)
	";no with-sound to quit")))

(define (ws-quit!)
  "(ws-quit!) exits all current with-sound contexts, returning you to the true top-level."
  (ws-quit)
  (if (not (null? *ws-continues*))
      (ws-quit!)))

(define (ws-stop)
  "(ws-stop) exits a with-sound context (but runs the reverb, if any)"
  (let ((current-finish *ws-finish*))
    (pop-ws)
    (if (continuation? current-finish)
	(current-finish #t)
	";no with-sound to stop")))

(define (ws-stop!)
  "(ws-quit!) exits all current with-sound contexts (running any reverbs on the way), \
returning you to the true top-level."
  (ws-stop)
  (if (not (null? *ws-continues*))
      (ws-stop!)))

(define* (ws-backtrace #:optional all) 
  "(ws-backtrace (all #f)) shows the stack backtrace at the point where with-sound was interrupted."
  (backtrace *ws-stack* all))

(define* (ws-locals #:optional (index 0)) 
  "(ws-locals (index 0)) shows the local variables at the index-th frame."
  (local-variables *ws-stack* index))

(define* (ws-local obj #:optional (index 0)) 
  "(ws-local obj (index 0)) shows the value of obj in the context where with-sound was interrupted."
  (local-variable *ws-stack* obj index))

(define (ws-location)
  (if (stack? *ws-stack*)
      (let ((len (stack-length *ws-stack*)))
	(call-with-current-continuation
	 (lambda (ok)
	   (do ((i 3 (1+ i)))
	       ((= i len))
	     (let ((fr (stack-ref *ws-stack* i)))
	       (if (frame-procedure? fr) ; perhaps better here would be (procedure-property 'definstrument) or some such test
		   (let ((source (frame-source fr)))
		     (if (memoized? source)
			 (ok (unmemoize source))
			 (ok source)))))))))
      #f))
  
(define (ws-help)
  "\n\
;The '?' prompt means you're in the with-sound debugger.\n\
;This is the standard Snd listener, so anything is ok at this level, but\n\
;there are also several additional functions:\n\
;  (ws-go (val #f)) continues from the point of the interrupt.\n\
;  (ws-quit) finishes with-sound, ignoring any reverb request.\n\
;  (ws-stop) runs the reverb, then finishes with-sound.\n\
;  (ws-quit!) and (ws-stop!) are similar, but if you're nested several\n\
;     levels deep in the debugger, these two will pop you back to the top level.\n\
;  (ws-locals) shows the local variables and their values.\n\
;  (ws-local obj) shows the value of obj.\n\
;  (ws-backtrace) shows the backtrace at the point of the interrupt.\n\
")

;;; ws-interrupt? checks for C-g within with-sound, setting up continuation etc
;;; this goes anywhere in the instrument (and any number of times), 
;;;    but not in the run macro body (run doesn't (yet?) handle code this complex)
(defmacro* ws-interrupt? (#:optional (message "with-sound"))
  ;; using defmacro, not define, so that we can more easily find the instrument (as a procedure)
  ;;   for ws-locals above -- some sort of procedure property is probably better
  `(if (c-g?) 
       (let ((stack (make-stack #t)))
	 (call-with-current-continuation
	  (lambda (continue)
	    (if ,message
		(throw 'with-sound-interrupt continue stack ,message)
		(throw 'with-sound-interrupt continue stack)))))
       #f)) ; there is a possible return value from the continuation, so I guess this will make it easier to use?


;;; -------- with-sound --------

(define *clm-srate* (default-output-srate))
(define *clm-file-name* "test.snd")
(define *clm-channels* (default-output-chans))
(define *clm-data-format* (default-output-format))
(define *clm-header-type* (default-output-type))
(define *clm-verbose* #f)
(define *to-snd* #t)

(define *reverb* #f) ; these are sample->file (outa) gens
(define *output* #f)
(define *clm-delete-reverb* #f) ; should with-sound clean up reverb stream

(define (seconds->samples secs) (inexact->exact (round (* secs (mus-srate)))))
(define (times->samples beg dur) (list (seconds->samples beg) (seconds->samples (+ beg dur))))

(if (not (defined? 'definstrument)) (define definstrument define*))

(define* (with-sound-helper thunk 
			    #:key (srate *clm-srate*) 
			          (output *clm-file-name*) 
				  (channels *clm-channels*)
				  (header-type *clm-header-type*)
				  (data-format *clm-data-format*)
				  (comment #f)
				  (reverb #f)
				  (revfile "test.rev")
				  (reverb-data #f)
				  (continue-old-file #f)
				  (statistics #f)
				  (scaled-to #f)
				  (play #f)
				  (to-snd *to-snd*)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-*output* *output*)
	(old-*reverb* *reverb*)
	(in-debugger #f)
	(output-1 output)) ; protect during nesting
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate))

     (lambda ()

       (if continue-old-file
	   (begin
	     (set! *output* (continue-sample->file output-1))
	     (set! (mus-srate) (mus-sound-srate output-1))
	     (if reverb (set! *reverb* (continue-sample->file revfile)))
	     (let ((ind (find-sound output-1)))
	       (if ind (close-sound ind))))
	   (begin
	     (if (file-exists? output-1) (delete-file output-1))
	     (if (and reverb (file-exists? revfile)) (delete-file revfile))
	     (set! *output* (make-sample->file output-1 channels data-format header-type comment))
	     (if reverb (set! *reverb* (make-sample->file revfile 1 data-format header-type)))))
       (let ((start (if statistics (get-internal-real-time)))
	     (flush-reverb #f)
	     (cycles 0))
	 (catch 'with-sound-interrupt
		thunk
		(lambda args 
		  ;; if from ws-interrupt? we have (continue stack message) as args
		  (begin
		    (if (and (not (null? (cdr args)))
			     (continuation? (cadr args)))
			;; instrument passed us a way to continue, so drop into the with-sound debugger
			(let ((val (call-with-current-continuation
				    (lambda (finish)
				      (push-ws (cadr args)              ; continue in notelist from ws-interrupt
					       finish                   ; stop notelist, go to output close section below
					       (if (> (length args) 2)
						   (list-ref args 2) 
						   (make-stack #t)))    ; stack at point of ws-interrupt or stack right here
				      (set! in-debugger #t)             ; gad -- turn off dynamic-wind output fixup below
				      (throw 'snd-top-level             ; return to "top level" (can be nested)
					     (format #f ";~A~A" 
						     (if (> (length args) 3)    ; optional message to ws-interrupt
							 (list-ref args 3)
							 "")
						     (let ((loc (ws-location)))
						       (if (string? loc)
							   (format #f ", interrupted at: ~A" loc)
							   ""))))))))
			  (set! in-debugger #f)
			  (set! flush-reverb (eq? val #f)))
			(begin
			  (snd-print (format #f "with-sound interrupted: ~{~A~^ ~}" (cdr args)))
			  (set! flush-reverb #t)))
		    args)))
	 (if (and reverb (not flush-reverb))
	     (begin
	       (mus-close *reverb*)
	       (set! *reverb* (make-file->sample revfile))
	       (reverb)
	       (mus-close *reverb*)
	       (if *clm-delete-reverb* (delete-file revfile))))
	 (mus-close *output*)
	 (if to-snd
	     (begin
	       (if statistics
		   (set! cycles (/ (- (get-internal-real-time) start) 100)))
	       (let ((cur (find-sound output-1)))
		 (if cur (close-sound cur)))
	       (let ((snd-output (open-sound output-1)))
		 (set! (sync snd-output) #t)
		 (if statistics
		     (snd-print 
		      (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			      output-1
			      (maxamp snd-output #t)
			      cycles)))
		 (if scaled-to
		     (scale-to scaled-to snd-output)
		     (if scaled-by
			 (scale-by scaled-by snd-output)))
		 (if play (play-and-wait snd-output))
		 (update-time-graph snd-output))))
	 output-1))

     (lambda () 
       (if (not in-debugger)
	   (begin
	     (if *reverb*
		 (begin
		   (mus-close *reverb*)
		   (set! *reverb* old-*reverb*)))
	     (if *output*
		 (begin
		   (mus-close *output*)
		   (set! *output* old-*output*)))
	     (set! (mus-srate) old-srate)))))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))

(defmacro with-temp-sound (args . body)
  ;; with-sound but using tempnam for output (can be over-ridden by explicit :output) and does not open result in Snd
  `(let ((old-file-name *clm-file-name*)
	 (old-to-snd *to-snd*))
     (set! *clm-file-name* (snd-tempnam))
     (set! *to-snd* #f)
     (let ((val (with-sound-helper (lambda () ,@body) ,@args)))
       (set! *to-snd* old-to-snd)
       (set! *clm-file-name* old-file-name)
       val)))


;;; -------- def-clm-struct --------

(defmacro* def-clm-struct (name #:rest fields)
  ;; (def-clm-struct fd loc (chan 1))
  ;; (def-clm-struct hiho i x (s "hiho") (ii 3 :type int) (xx 0.0 :type float))
  ;; we need the :type indication if Snd's run macro is to have any hope of handling these structs as function args
  ;; should this code enforce the type indication?
  (let* ((sname (if (string? name) name (symbol->string name)))
	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))
	 (field-types (map (lambda (n)
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (list-ref n 2) :type))
				 (list-ref n 3)
				 #f))
			   fields)))
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))
       (define* (,(string->symbol (string-append "make-" sname)) 
		 #:key ,@(map (lambda (n)
				(if (and (list? n)
					 (> (length n) 2))
				    (list (car n) (cadr n))
				    n))
			      fields))
	 (list ',(string->symbol sname)
	       ,@(map string->symbol field-names)))
       (add-clm-type ,sname)
       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field (string-append sname "-" n) ctr type)
		    (set! ctr (1+ ctr))
		    val)))
	      field-names field-types))))


;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(defmacro sound-let (snds . body) 
  `(let ((temp-files '())
	 (old-hook-list (hook->list new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (begin
       (reset-hook! new-sound-hook)
       (add-hook! new-sound-hook (lambda (file)       ; save current sound-let temp file list
				   (set! temp-files (cons file temp-files))))
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				                      ; if with-sound, embed with-temp-sound
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))              ; else use direct (normal var in the let)
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (file-exists? file)
			 (delete-file file)))
		   temp-files)
	 (reset-hook! new-sound-hook)                 ; restore old new-sound-hook (should this happen before ,@body?)
	 (if (not (null? old-hook-list))
	     (for-each (lambda (proc)
			 (add-hook! new-sound-hook proc))
		       old-hook-list))
	 val))))                                      ; return body result



;;; -------- Common Music --------

(define* (init-with-sound #:key 
			  (srate *clm-srate*) 
			  (output *clm-file-name*) 
			  (channels *clm-channels*)
			  (header-type *clm-header-type*)
			  (data-format *clm-data-format*)
			  (comment #f)
			  (reverb #f)
			  (revfile "test.rev")
			  (reverb-data #f)
			  (continue-old-file #f)
			  (statistics #f)
			  (scaled-to #f)
			  (play #f)
			  (to-snd *to-snd*)
			  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(start (if statistics (get-internal-real-time))))
    (if continue-old-file
	(begin
	  (set! *output* (continue-sample->file output))
	  (set! (mus-srate) (mus-sound-srate output))
	  (if reverb (set! *reverb* (continue-sample->file revfile)))
	  (let ((ind (find-sound output)))
	    (if ind (close-sound ind))))
	(begin
	  (if (file-exists? output) (delete-file output))
	  (if (and reverb (file-exists? revfile)) (delete-file revfile))
	  (set! *output* (make-sample->file output channels data-format header-type comment))
	  (if reverb (set! *reverb* (make-sample->file revfile 1 data-format header-type)))))
    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  to-snd
	  scaled-to
	  scaled-by
	  play)))

(define (finish-with-sound wsd)
  (if (eq? (car wsd) 'with-sound-data)
      (let ((cycles 0)
	    (output (list-ref wsd 1))
	    (reverb (list-ref wsd 2))
	    (revfile (list-ref wsd 3))
	    (old-srate (list-ref wsd 4))
	    (statistics (list-ref wsd 5))
	    (to-snd (list-ref wsd 6))
	    (scaled-to (list-ref wsd 7))
	    (scaled-by (list-ref wsd 8))
	    (play (list-ref wsd 9)))
	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (set! *reverb* (make-file->sample revfile))
	      (reverb)
	      (mus-close *reverb*)))
	(mus-close *output*)
	(if statistics
	    (set! cycles (/ (- (get-internal-real-time) start) 100)))
	(if to-snd
	    (let ((snd-output (open-sound output)))
	      (set! (sync snd-output) #t)
	      (if statistics
		  (snd-print 
		   (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			   output
			   (maxamp snd-output #t)
			   cycles)))
	      (if scaled-to
		  (scale-to scaled-to snd-output)
		  (if scaled-by
		      (scale-by scaled-by snd-output)))
	      (if play (play-and-wait snd-output))
	      (update-time-graph snd-output)))
	(set! (mus-srate) old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))

#!
;;; these are for compatibility with CLM/CM
;;;(defstruct wsdat revfun revdat revdecay outtype play stats wait scaled-to format file channels scaled-by)
(define (wsdat-revfun w) (list-ref w 2))
(define (wsdat-revdat w) #f)
(define (wsdat-revdecay w) #f)
(define (wsdat-outtype w) #f)
(define (wsdat-play w) (list-ref w 9))
(define (wsdat-stats w) (list-ref w 5))
(define (wsdat-wait w) #f)
(define (wsdat-scaled-to w) (list-ref w 7))
(define (wsdat-format w) #f)
(define (wsdat-file w) (list-ref w 1))
(define (wsdat-channels w) #f)
(define (wsdat-scaled-by w) (list-ref w 8))
!#
(define wsdat-play
  (make-procedure-with-setter
   (lambda (w)
     (list-ref w 9))
   (lambda (w val)
     (list-set! w 9 val))))


;;; -------- with-sound save state --------
(define (mus-data-format->string df)
  (cond ((= df mus-bshort) "mus-bshort")
	((= df mus-mulaw) "mus-mulaw")
	((= df mus-byte) "mus-byte")
	((= df mus-bfloat) "mus-bfloat")
	((= df mus-bfloat-unscaled) "mus-bfloat-unscaled")
	((= df mus-bint) "mus-bint")
	((= df mus-alaw) "mus-alaw")
	((= df mus-ubyte) "mus-ubyte")
	((= df mus-b24int) "mus-b24int")
	((= df mus-bdouble) "mus-bdouble")
	((= df mus-bdouble-unscaled) "mus-bdouble-unscaled")
	((= df mus-lshort) "mus-lshort")
	((= df mus-lint) "mus-lint")
	((= df mus-lfloat-unscaled) "mus-lfloat-unscaled")
	((= df mus-ldouble-unscaled) "mus-ldouble-unscaled")
	((= df mus-lfloat) "mus-lfloat")
	((= df mus-ldouble) "mus-ldouble")
	((= df mus-ubshort) "mus-ubshort")
	((= df mus-ulshort) "mus-ulshort")
	((= df mus-l24int) "mus-l24int")
	((= df mus-bintn) "mus-bintn")
	((= df mus-lintn) "mus-lintn")
	(#t df)))

(define (mus-header-type->string ht)
  ;; these are the writable headers
  (cond ((= ht mus-next) "mus-next")
	((= ht mus-aifc) "mus-aifc")
	((= ht mus-riff) "mus-riff")
	((= ht mus-aiff) "mus-aiff")
	((= ht mus-nist) "mus-nist")
	((= ht mus-raw)  "mus-raw")
	((= ht mus-ircam) "mus-ircam")
	((= ht mus-bicsf) "mus-bicsf")
	(#t ht)))

(define (ws-save-state filename)
  (let ((fd (open filename (logior O_RDWR O_APPEND))))
    ;; open in Guile throws 'system-error (I think) if anything goes wrong
    ;; fd is a Scheme port at this point (not an integer), so we can use format etc
    ;; should the save-state file load this file if it hasn't been loaded? (what path?)
    (format fd "~%~%;;; from ws.scm~%")
    (format fd "(if (defined? '*clm-srate*)~%")
    (format fd "    (begin~%")
    (format fd "      (set! *clm-srate* ~A)~%" *clm-srate*)
    (format fd "      (set! *clm-file-name* ~S)~%" *clm-file-name*)
    (format fd "      (set! *clm-channels* ~A)~%" *clm-channels*)
    (format fd "      (set! *clm-data-format* ~A)~%" (mus-data-format->string *clm-data-format*))
    (format fd "      (set! *clm-header-type* ~A)))~%" (mus-header-type->string *clm-header-type*))
    (close fd)))

(add-hook! after-save-state-hook ws-save-state)


;;; -------- with-marked-sound --------
#!
;;; the following code places a mark at the start of each note in the with-sound body
;;;    with arbitrary info in the :ws mark-property, displayed in the help dialog
;;;    when the mark is clicked.  The corresponding code in the instrument is:
;;;          (if (defined? '*note-hook*)
;;;	         (run-hook *note-hook* beg (list 'fm-violin dur frequency amplitude)))

(load "marks.scm")
(define *note-hook* (make-hook 2)) ; args = beg[sample] comment[anything]

(defmacro with-marked-sound (args . body)
  ;; a wrapper around with-sound to set up the marks
  `(let ((*ws-prog* '())
	 (old-*note-hook* (hook->list *note-hook*))) ; save old *note-hook* (nested with-sound)
     (define (list->hook hook l)
       (if (not (null? l))
	   (begin
	     (add-hook! hook (car l))
	     (list->hook hook (cdr l)))))
     (reset-hook! *note-hook*)
     (add-hook! *note-hook*                          ; current hook saves mark data in *ws-prog*
		(lambda (beg comment)
		  (set! *ws-prog* (cons (list beg comment) *ws-prog*))))
     (reset-hook! mark-click-hook)
     (add-hook! mark-click-hook mark-click-info)
     (let* ((name (with-sound-helper (lambda () ,@body) ,@args)) ; with-sound itself
	    (snd (find-sound name)))
       (reset-hook! *note-hook*)
       (list->hook *note-hook* old-*note-hook*)      ; restore previous *note-hook*, if any
       (for-each 
	(lambda (m)
	  (let ((mk (add-mark (car m) snd)))         ; put a mark at each note begin sample
	    (set! (mark-property :ws mk) (cadr m)))) ; and set its :ws property to the other info
	*ws-prog*))))
!#


;;; -------- with-mix --------
;;;
;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body
;;;
;;; (with-sound () 
;;;   (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
;;;                              (fm-violin 1 2 660 .1))
;;; ...)

(define (with-mix-find-file-with-extensions file extensions)
  (if (file-exists? file)
      file
      (call-with-current-continuation
       (lambda (found-one)
	 (for-each
	  (lambda (ext)
	    (let ((new-file (string-append file "." ext)))
	      (if (file-exists? new-file)
		  (found-one new-file))))
	  extensions)
	 #f))))

(define (with-mix-file-extension file default)
  (let ((pos #f)
	(len (string-length file)))
    (call-with-current-continuation
     (lambda (ok)
       (do ((i (1- len) (1- i)))
	   ((= i 0))
	 (if (char=? (string-ref file i) #\.)
	     (ok (substring file (1+ i) len))))
       default))))

(defmacro with-mix-error (message)
  `(let ((stack (make-stack #t)))
     (call-with-current-continuation
      (lambda (continue)
	(throw 'with-sound-interrupt continue stack ,message)))))

(defmacro with-mix (options ur-chkpt-file ur-beg . body)
  `(let ((chkpt-file ,ur-chkpt-file)
	 (beg-1 ,ur-beg))
     (if (not (list? ',options))
	 (with-mix-error (format #f "with-mix options list (arg 1) is ~A?~%;" ',options))
	 (if (not (string? chkpt-file))
	     (with-mix-error (format #f "with-mix file (arg 2) is ~A?~%;" ,ur-chkpt-file))
	     (if (not (number? beg-1))
		 (with-mix-error (format #f "with-mix begin time (arg 3) for ~S = ~A?~%;" chkpt-file beg-1))
		 (let ((beg (inexact->exact (round (* (mus-srate) beg-1)))))
		   (if (null? ',body)
		       (mus-mix *output* chkpt-file beg)
		       (let* ((call-str (object->string ',body))
			      (option-str (object->string ',options))
			      (sndf (with-mix-find-file-with-extensions chkpt-file (list (with-mix-file-extension *clm-file-name* "snd") "snd")))
			      (revf (and sndf *reverb* (with-mix-find-file-with-extensions chkpt-file (list "rev"))))
			      (mix-values (and sndf
					       (or (not *reverb*)
						   revf)
					       (let ((comment (mus-sound-comment sndf)))
						 (and (string? comment)
						      (catch #t
							     (lambda ()
							       (eval-string comment))
							     (lambda args #f))))))) ; any error means we lost
			 (if (and sndf
				  (or (not *reverb*)
				      revf)
				  (list? mix-values)
				  (= (length mix-values) 2)
				  (string? (car mix-values))
				  (string? (cadr mix-values))
				  (string=? (car mix-values) option-str)
				  (string=? (cadr mix-values) call-str))
			     (begin
			       (if *clm-verbose* (snd-print (format #f "mix ~S at ~,3F~%" sndf beg)))
			       (mus-mix *output* sndf beg)
			       (if revf (mus-mix *reverb* revf beg)))
			     ;; else recompute
			     (let ((old-to-snd *to-snd*))
			       (set! *to-snd* #f)
			       (if *clm-verbose* (snd-print (format #f "remake ~S at ~,3F~%" chkpt-file beg)))
			       (let ((new-sound 
				      (apply with-sound-helper 
					     (lambda () ,@body) 
					     (append (list :output (string-append chkpt-file "." (with-mix-file-extension *clm-file-name* "snd")))
						     (list :comment (format #f "(begin~%;; written ~A (Snd: ~A)~%(list ~S ~S))~%"
									    (strftime "%a %d-%b-%Y %H:%M %Z" (localtime (current-time)))
									    (snd-version)
									    option-str
									    call-str))
						     (if (and (> (mus-channels *output*) 1)
							      (not (member :channels ',options)))
							 (list :channels (mus-channels *output*))
							 '())
						     ',options))))
				 (set! *to-snd* old-to-snd)
				 (mus-mix *output* new-sound beg)
				 (if revf (mus-mix *reverb* revf beg)))))))))))))
  
  
  
;;; a test case for ws-debug
;;; (with-sound () (fm-violin 0 1 440 .1) (sleep 1) (fm-violin 1 1 440 .1) (sleep 1) (fm-violin 2 1 440 .1) (sleep 1))
