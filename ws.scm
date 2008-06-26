;;; with-sound and friends

(use-modules (ice-9 optargs) (ice-9 format))

(provide 'snd-ws.scm)
(if (and (provided? 'snd-gauche) (not (provided? 'gauche-optargs.scm))) (load-from-path "gauche-optargs.scm"))
(if (and (provided? 'snd-guile) (not (provided? 'snd-debug.scm))) (load-from-path "debug.scm"))
(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm")) ; we need sound-property in with-mixed-sound


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
  "(ws-prompt) returns the current with-sound debugger prompt, set partially by the variable *ws-top-level-prompt*"
  (let ((stack-len (length *ws-stacks*)))
    (if (> stack-len 1)
	(format #f "ws(~D)~A" (1- stack-len) *ws-top-level-prompt*)
	(if (> stack-len 0)
	    (format #f "ws~A" *ws-top-level-prompt*)
	    *ws-top-level-prompt*))))

(define (pop-ws)
  "(pop-ws) pops the with-sound continuation stack"
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
  "(push-ws continue finish stack) adds a continuation to the with-sound stack of continuations"
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

(define* (ws-go :optional val)
  "(ws-go (val #f)) tries to continue from the point at which with-sound was interrupted. 'val' is 
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
  "(ws-quit!) exits all current with-sound contexts (running any reverbs on the way), 
returning you to the true top-level."
  (ws-stop)
  (if (not (null? *ws-continues*))
      (ws-stop!)))

(define* (ws-backtrace :optional all) 
  "(ws-backtrace (all #f)) shows the stack backtrace at the point where with-sound was interrupted."
  (snd-backtrace *ws-stack* all))

(define* (ws-locals :optional (index 0)) 
  "(ws-locals (index 0)) shows the local variables at the index-th frame."
  (local-variables *ws-stack* index))

(define* (ws-local obj :optional (index 0)) 
  "(ws-local obj (index 0)) shows the value of obj in the context where with-sound was interrupted."
  (local-variable *ws-stack* obj index))

(define (ws-location)
  "(ws-location) tries to report where with-sound was at the point of an interruption"
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
			 (ok ((if (defined? 'unmemoize) unmemoize unmemoize-expr) source))
			 (ok source)))))))))
      #f))
  
(define (ws-help)
  "
;The '?' prompt means you're in the with-sound debugger.
;This is the standard Snd listener, so anything is ok at this level, but
;there are also several additional functions:
;  (ws-go (val #f)) continues from the point of the interrupt.
;  (ws-quit) finishes with-sound, ignoring any reverb request.
;  (ws-stop) runs the reverb, then finishes with-sound.
;  (ws-quit!) and (ws-stop!) are similar, but if you're nested several
;     levels deep in the debugger, these two will pop you back to the top level.
;  (ws-locals) shows the local variables and their values.
;  (ws-local obj) shows the value of obj.
;  (ws-backtrace) shows the backtrace at the point of the interrupt.
")

;;; ws-interrupt? checks for C-g within with-sound, setting up continuation etc
;;; this goes anywhere in the instrument (and any number of times), 
;;;    but not in the run macro body (run doesn't (yet?) handle code this complex)

(if (provided? 'snd-gauche)
    (define (make-stack . args) #f))

(defmacro ws-interrupt? ()
  ;; using defmacro, not define, so that we can more easily find the instrument (as a procedure)
  ;;   for ws-locals above -- some sort of procedure property is probably better
  `(if (c-g?) 
       (let ((stack (make-stack #t)))
	 (call-with-current-continuation
	  (lambda (continue)
	    (throw 'with-sound-interrupt continue stack))))
       #f)) ; there is a possible return value from the continuation, so I guess this will make it easier to use?


;;; -------- with-sound defaults --------

(define *clm-srate* (default-output-srate))
(define *clm-file-name* "test.snd")
(define *clm-channels* (default-output-chans))
(define *clm-data-format* (default-output-data-format))
(define *clm-header-type* (default-output-header-type))
(define *clm-verbose* #f)
(define *clm-play* #f)
(define *clm-statistics* #f)
(define *clm-reverb* #f)
(define *clm-reverb-channels* 1)
(define *clm-reverb-data* '())
(define *clm-table-size* 512)
(define *clm-file-buffer-size* 65536)
(define *clm-locsig-type* mus-interp-linear)
(define *clm-clipped* #t)
(define *clm-array-print-length* (print-length))
(define *clm-player* #f)          ; default is play-and-wait (takes index of newly created sound, not the sound's file name)
(define *clm-notehook* #f)
(define *clm-with-sound-depth* 0) ; for CM, not otherwise used
(define *clm-default-frequency* 0.0)
(define *clm-safety* run-safety)  ; slightly different from CL/CLM but has similar effect
(define *clm-delete-reverb* #f)   ; should with-sound clean up reverb stream
(define *clm-threads* 4)

(define *to-snd* #t)


(define (times->samples beg dur) 
  "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"
  (list (seconds->samples beg) (seconds->samples (+ beg dur))))


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05

(define *definstrument-hook* #f) ; for CM

(defmacro definstrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names '()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names))))
  `(begin 
     (define* (,name ,@targs)
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       (if (not (zero? (run-safety))) 
	   (set! *ws-stack* (make-stack #t)))
       ((lambda () ; for inner defines, if any
	  ,@body)))
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))

(defmacro definstrument+ (args . body) ; for Gauche -- throw away the documentation string
  `(definstrument ,args ,@(cdr body)))

;;; definstrument help string is currently lost


;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    :key (srate *clm-srate*) 
			          (output *clm-file-name*) 
				  (channels *clm-channels*)
				  (header-type *clm-header-type*)
				  (data-format *clm-data-format*)
				  (comment #f)
				  (verbose *clm-verbose*)
				  (reverb *clm-reverb*)
				  (revfile "test.rev")
				  (reverb-data *clm-reverb-data*)
				  (reverb-channels *clm-reverb-channels*)
				  (continue-old-file #f)
				  (statistics *clm-statistics*)
				  (scaled-to #f)
				  (play *clm-play*)
				  (to-snd *to-snd*)
				  (clipped 'unset)
				  (notehook *clm-notehook*)               ; (with-sound (:notehook (lambda args (display args))) (fm-violin 0 1 440 .1))
				  (scaled-by #f)
				  (ignore-output #f))
  "with-sound-helper is the business portion of the with-sound macro"
  (let ((old-srate (mus-srate))
	(old-*output* *output*)
	(old-*reverb* *reverb*)
	(in-debugger #f)
	(old-notehook *clm-notehook*)
	(old-verbose *clm-verbose*)
	(old-auto-update-interval (auto-update-interval))
	(output-1 output)                    ; protect during nesting
	(output-to-file (string? output))
	(reverb-1 revfile)
	(reverb-to-file (and reverb (string? revfile))))

    (if ignore-output
	(begin
	  (set! output-1 *clm-file-name*)
	  (set! output-to-file (string? output-1))))

    (dynamic-wind 

     (lambda () 
       (set! *clm-verbose* verbose)
       (set! *clm-notehook* notehook)
       (set! (clm-table-size) *clm-table-size*)
       (set! (clm-default-frequency) *clm-default-frequency*)
       (set! (mus-file-buffer-size) *clm-file-buffer-size*)
       (set! (locsig-type) *clm-locsig-type*)
       (set! (mus-array-print-length) *clm-array-print-length*)
       (set! (auto-update-interval) 0.0) ; turn it off
       (if (equal? clipped 'unset)
	   (if (and (or scaled-by scaled-to)
		    (member data-format (list mus-bfloat mus-lfloat mus-bdouble mus-ldouble)))
	       (set! (mus-clipping) #f)
	       (set! (mus-clipping) *clm-clipped*))
	   (set! (mus-clipping) clipped))
       (set! (mus-srate) srate))

     (lambda ()
       (if output-to-file
	   (if continue-old-file
	       (begin
		 (set! *output* (continue-sample->file output-1))
		 (set! (mus-srate) (mus-sound-srate output-1))
		 (let ((ind (find-sound output-1)))
		   (if (sound? ind)
		       (close-sound ind))))
	       (begin
		 (if (file-exists? output-1) 
		     (delete-file output-1))
		 (set! *output* (make-sample->file output-1 channels data-format header-type comment))))
	   (begin
	     (if (not continue-old-file)
		 (if (vct? output-1)
		     (vct-fill! output-1 0.0)
		     (if (sound-data? output-1)
			 (sound-data-fill! output-1 0.0))))
	     (set! *output* output-1)))

       (if reverb
	   (if reverb-to-file
	       (if continue-old-file
		   (set! *reverb* (continue-sample->file reverb-1))
		   (begin
		     (if (file-exists? reverb-1) 
			 (delete-file reverb-1))
		     (set! *reverb* (make-sample->file reverb-1 reverb-channels data-format header-type))))
	       (begin
		 (if (not continue-old-file)
		     (if (vct? reverb-1)
			 (vct-fill! reverb-1 0.0)
			 (if (sound-data? reverb-1)
			     (sound-data-fill! reverb-1 0.0))))
		 (set! *reverb* reverb-1))))

       (let ((start (if statistics (get-internal-real-time)))
	     (flush-reverb #f)
	     (cycles 0)
	     (revmax #f))
	 (catch 'mus-error

		(lambda ()
		  (catch 'with-sound-interrupt
			 thunk
			 (lambda args 
			   ;; if from ws-interrupt? we have (continue stack message) as args
			   (begin

			     (if (provided? 'snd-gauche)
				 (snd-print (object->string args)))

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
			     args))))

		(lambda args
		  ;; hit mus-error, for example:
		  ;;   (with-sound () (fm-violin 0 1 440 .1 :amp-env '(0 0 1 1 1 2 3 0)))

		  ;; user might have listener closed, or no listener so...
		  (display (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))

		  ;; now try to get something to listener, since there may be no stdout
		  (snd-print (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))

		  ;; we want to be able to clean up and yet we want a stack trace...
		  (ws-backtrace)
		  ;; but for that to work right we need     
		  ;;    (if (not (zero? (run-safety))) (set! *ws-stack* (make-stack #t)))
		  ;; before the outer let in definstrument -- this seems to be ok -- I see only a slight slowdown
		  (set! flush-reverb #t)))
		  
	 (if (and reverb 
		  (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	     (begin
	       (if reverb-to-file
		   (mus-close *reverb*))
	       (if statistics 
		   (if reverb-to-file
		       (set! revmax (cadr (mus-sound-maxamp reverb-1)))
		       (if (vct? reverb-1)
			   (set! revmax (vct-peak reverb-1))
			   (if (sound-data? reverb-1)
			       (set! revmax (sound-data-peak reverb-1))))))
	       (if reverb-to-file
		   (set! *reverb* (make-file->sample reverb-1)))
	       (apply reverb reverb-data)
	       (if reverb-to-file
		   (mus-close *reverb*))
	       (if (and reverb-to-file *clm-delete-reverb*)
		   (delete-file reverb-1))))

	 (if output-to-file
	     (mus-close *output*))

	 (let ((snd-output #f)
	       (cur-sync #f))
	   (if statistics
	       (set! cycles (exact->inexact (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

	   (if (and to-snd output-to-file)
	       (let* ((cur (find-sound output-1)))
		 (set! cur-sync (and cur (sync cur)))
		 (if cur 
		     (set! snd-output (update-sound cur))
		     (if (= header-type mus-raw)
			 (set! snd-output (open-raw-sound output-1 channels srate data-format))
			 ;; open-sound here would either ask for raw settings or use possibly irrelevant defaults
			 (set! snd-output (open-sound output-1))))
		 (set! (sync snd-output) #t)))

	   (if statistics
	       ((if (procedure? statistics) ; :statistics (lambda (str) (snd-print str)) -- intended for auto test suite
		    statistics 
		    (if to-snd 
			snd-print 
			display))
		(format #f "~%;~A:~%  maxamp~A:~{ ~,4F~}~%~A  compute time: ~,3F~%"
			      (if output-to-file
				  output-1
				  (if (vct? output-1) "vct" 
				      (if (sound-data? output-1) "sound-data"
					  (if (procedure? output-1) "function" 
					      "flush"))))
			      (if (or scaled-to scaled-by) 
				  " (before scaling)" 
				  "")
			      (if output-to-file
				  (if to-snd 
				      (maxamp snd-output #t) 
				      (mus-sound-maxamp output-1))
				  (if (vct? output-1)
				      (list (vct-peak output-1))
				      (if (sound-data? output-1)
					  (sound-data-maxamp output-1)
					  0.0)))
			      (if revmax (format #f "  rev max: ~,4F~%" revmax) "")
			      cycles)))

	   (if (or scaled-to scaled-by)
	       (if output-to-file
		   (let ((scale-output (or snd-output (open-sound output-1))))
		     (if scaled-to
			 (scale-to scaled-to scale-output)
			 (if scaled-by
			     (scale-by scaled-by scale-output)))
		     (save-sound scale-output)
		     (if (not to-snd) 
			 (close-sound scale-output)))
		   (if (vct? output-1)
		       (if scaled-to
			   (let ((pk (vct-peak output-1)))
			     (if (> pk 0.0)
				 (vct-scale! output-1 (/ scaled-to pk))))
			   (vct-scale! output-1 scaled-by))
		       (if (sound-data? output-1)
			   (if scaled-to
			       (let ((pk (sound-data-peak output-1)))
				 (if (> pk 0.0)
				     (sound-data-scale! output-1 (/ scaled-to pk))))
			       (sound-data-scale! output-1 scaled-by))))))

	   (if (and play output-to-file)
	       (if to-snd
		   (if *clm-player*
		       (*clm-player* snd-output)
		       (play-and-wait 0 snd-output))
		   (play output-1)))

	   (if (and to-snd output-to-file)
	       (begin
		 (update-time-graph snd-output)
		 (goto-listener-end)
		 (if (number? cur-sync) (set! (sync snd-output) cur-sync)))))
	 output-1))

     (lambda () 
       (set! *clm-verbose* old-verbose)
       (set! *clm-notehook* old-notehook)
       (set! (auto-update-interval) old-auto-update-interval)
       (if (not in-debugger)
	   (begin
	     (if *reverb*
		 (begin
		   (mus-close *reverb*)
		   (set! *reverb* old-*reverb*)))
	     (if *output*
		 (begin
		   (if (mus-output? *output*)
		       (mus-close *output*))
		   (set! *output* old-*output*)))
	     (set! (mus-srate) old-srate)))))))


(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))


;;; -------- with-full-sound --------

(defmacro with-full-sound (args . body)
  ;; with-sound but display full sound in Snd window
  `(let ((snd (with-sound-helper (lambda () ,@body) ,@args)))
     (set! (x-bounds *snd-opened-sound*) (list 0.0 (/ (frames *snd-opened-sound*) (srate *snd-opened-sound*))))
     snd))


;;; -------- with-threaded-sound

(if (provided? 'snd-gauche)
    (use gauche.threads))
(if (provided? 'snd-gauche)
    (define join-thread thread-join!))
(if (provided? 'snd-gauche)
    (define (call-with-new-thread thunk)
      (let ((thread (make-thread thunk)))
	(thread-start! thread)
	thread)))

(defmacro with-threaded-sound (args . body) 
  (if (provided? 'snd-threads)
      `(with-sound-helper 
	(lambda () 
	  (let ((threads '()))
	    ,@(map (lambda (expr) 
		     `(begin 
			(set! threads (cons (call-with-new-thread 
					     (lambda () 
					       ,expr))
					    threads))
			(if (>= (length threads) *clm-threads*)
			    (begin
			      (for-each 
			       (lambda (thread) 
				 (join-thread thread))
			       threads)
			      (set! threads '())))))
		   body)
	    (for-each 
	     (lambda (thread) 
	       (join-thread thread))
	     threads)))
	,@args)

      ;; if no threads, don't use the underlying threads since Snd is assuming various table references will not be interrupted
      `(with-sound-helper
	(lambda ()
	  ,@body)
	,@args)))


;;; this still non-optimal because of thread handling overhead -- if I break the "body" into
;;;   4 big pieces (4 threads total), it runs much faster.  So, off to make another version that
;;;   somehow divides the body into n big pieces...


#|
(defmacro with-threaded-sound (args . body) 
  `(with-sound-helper 
    (lambda () 
      (let ((threads '()))
	,@(map (lambda (expr) 
		 `(set! threads (cons (call-with-new-thread 
				       (lambda () 
					 ,expr))
				      threads)))
	       ;; here we could insert joins based on list length (clearing threads)
	       body)
	(for-each 
	 (lambda (thread) 
	   (join-thread thread))
	 threads))) ; using threads list, not (all-threads) because the latter hangs in the with-threads case below
    ,@args))

;;; this form not good because we end up with "too many open files" -- apparently each thread
;;;   counts its copy of the output file separately.


(define (with-threads func)
  (let ((chns (chans))
	(threads '()))
    (do ((chn 0 (1+ chn)))
	((= chn chns))
      (set! threads (cons (call-with-new-thread (lambda () (func chn))) threads)))
    (for-each 
     (lambda (expr) 
       (join-thread expr))
     threads)))

;(with-threads (lambda (chn) (src-channel 2.0 0 #f #f chn)))

;;; TODO: check filter-channel and map-channel/ptrees (with init func etc) here, also reverse, maybe smooth, convolve, scan [tmp38]
;;;   also Ruby? [ruby does have threads, but how to handle them in with_sound?]

(with-threaded-sound ()
  (fm-violin 0 1 440 .1)
  (fm-violin 0 1 660 .1))
|#


;;; -------- with-temp-sound --------

(defmacro with-temp-sound (args . body)
  `(let ((old-file-name *clm-file-name*)
	 (old-to-snd *to-snd*))
     ;; with-sound but using tempnam for output (can be over-ridden by explicit :output) and does not open result in Snd
     (dynamic-wind
	 (lambda () 
	   (set! *clm-file-name* (snd-tempnam))
	   (set! *to-snd* #f))
	 (lambda ()
	   (with-sound-helper (lambda () ,@body) ,@args)) ; dynamic-wind returns this as its result
	 (lambda ()
	   (set! *to-snd* old-to-snd)
	   (set! *clm-file-name* old-file-name)))))


;;; -------- clm-load --------
;;;
;;; CM wants this to be a function so that it can use apply

(define (clm-load file . args) 
  "(clm-load file . args) loads 'file' in the context of with-sound"
  (apply with-sound-helper (lambda () (load file)) args))


;;; -------- with-mixed-sound --------

(define (with-mixed-sound-mix-info id snd)
  (let ((all-info (sound-property 'with-mixed-sound-info snd)))
    ;; each entry is '(mx-id beg chans note)
    (let ((find-if (lambda (pred l)
		     (cond ((null? l) #f)
			   ((pred (car l)) (car l))
			   (else (find-if pred (cdr l)))))))
      (find-if (lambda (info)
		 (and (>= id (car info))
		      (< id (+ (car info) (caddr info)))))
	       all-info))))

(defmacro with-mixed-sound (args . body)
  `(let* ((output (with-sound-helper (lambda () #f) ,@args :to-snd #t)) ; pick up args for output
	  (outsnd (find-sound output)))

     (if (sound? outsnd)
	 (let ((mix-info '())
	       (old-sync (sync outsnd)))

	   ;; if multichannel output, make sure cross-chan mixes move together 
	   (if (> (chans outsnd) 1)
	       (begin
		 (reset-hook! mix-release-hook)
		 (add-hook! mix-release-hook
			    (lambda (id samps-moved)
			      (let ((new-pos (+ samps-moved (mix-position id)))
				    (base (mix-sync id)))
				(do ((mx base (+ mx 1)))
				    ((or (not (mix? mx))
					 (not (= (mix-sync mx) base))))
				  (set! (mix-position mx) new-pos))
				#t)))))

	   ;; click shows the original note list entry
	   (reset-hook! mix-click-hook)
	   (add-hook! mix-click-hook
		      (lambda (id)
			(let ((info (with-mixed-sound-mix-info id outsnd)))
			  (report-in-minibuffer (format #f "mix ~A: ~A" 
						      id (or (and info
								  (cadddr info))
							     (exact->inexact (/ (mix-position id) (srate outsnd))))))
			  #t))) ; #t -> don't print the mix id in the minibuffer

	   (dynamic-wind
	       (lambda ()
		 (set! (sync outsnd) 0)
		 (do ((chan 0 (1+ chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #t)))

	       (lambda ()
		 (for-each
		  (lambda (note)
		    (let* ((snd (with-temp-sound (list ,@args :ignore-output #t) (eval (append (list (car note) 0.0) (cddr note)) (current-module))))
			   ;; I can't immediately find a way around the "eval" 
			   ;;   current-module is a synonym for interaction-environment in Gauche
			   (beg (inexact->exact (floor (* (srate outsnd) (cadr note)))))
			   ;; can't use seconds->samples here because the global mus-srate value might not match the local one
			   (mx (mix snd beg #t outsnd #f #t #t))     ; all chans mixed, current output sound, with mixes, with auto-delete
			   (chans (mus-sound-chans snd)))
		      (set! (mix-name mx) (format #f "(~A ~A)" (car note) (cadr note)))
		      (do ((chan 0 (1+ chan)))
			  ((= chan chans))
			(set! (mix-sync (+ mx chan)) mx))
		      (set! mix-info (cons (list mx beg chans note) mix-info))))
		  ',body)
		 (set! (sound-property 'with-mixed-sound-info outsnd) (reverse mix-info)))

	       (lambda ()
		 (set! (sync outsnd) old-sync)
		 (do ((chan 0 (1+ chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #f)
		   (update-time-graph outsnd chan))))))
     output))

;(with-mixed-sound () (fm-violin 0 .1 440 .1) (fm-violin 1 .1 660 .1))
;(with-mixed-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (fm-violin 1 .1 660 .1 :degree 45))


(define* (with-mixed-sound->notelist :optional (output-file "temp.scm") snd)
  (let* ((outsnd (or snd (selected-sound) (car (sounds))))
	 (mix-info (sound-property 'with-mixed-sound-info outsnd)))
    (if (not mix-info)
	(throw 'no-such-mixed-sound (list "with-mixed-sound->notelist" outsnd))
	(let ((cur-mixes (mixes outsnd 0)) ; for now assume each mix is multichannel
	      (oput (open-output-file output-file)))
	  (display (format #f "(with-sound (:channels ~D)~%" (chans snd)) oput)
	  (for-each
	   (lambda (id)
	     (let ((info (with-mixed-sound-mix-info id snd)))
	       (if info
		   (let ((call (cadddr info)))
		     (if (not (= (cadr info) (mix-position id)))
			 (display (format #f "  (~A ~,3F~{ ~A~})~%"
					  (car call) 
					  (exact->inexact (/ (mix-position id) (srate snd)))
					  (cddr call)) 
				  oput)
			 (display (format #f "  ~A~%" call) oput)))
		   (report-in-minibuffer "can't find note associated with mix ~A" id))))
	   cur-mixes)
	  (display (format #f ")~%") oput)
	  (close-output-port oput)))))


;;; TODO: the mix tags need to be placed according to frequency in with-mixed-sound [amp/freq/dur/env change -> notelist?]
;;; TODO: it might be nice to notice interrupted with-temp-sounds here and exit the mixing loop
;;; SOMEDAY: what happens to an expression that isn't a note?  How to handle notes that don't put begin time 1st? (and other such changes)


;;; -------- with-marked-sound --------

(defmacro with-marked-sound (args . body)
  `(let ((old-notehook *clm-notehook*)
	 (mark-list '()))
     (dynamic-wind
	 (lambda ()
	   (set! *clm-notehook* (lambda (name . args)
				  (set! mark-list (cons (append (list name) args) mark-list)))))

	 (lambda ()
	   (let* ((result (with-sound-helper (lambda () ,@body) ,@args))
		  (snd (find-sound result))
		  (old-update (squelch-update snd 0)))
	     (dynamic-wind
		 (lambda ()
		   (set! (squelch-update snd 0) #t))
		 (lambda ()
		   (for-each
		    (lambda (descr)
		      (let ((m (add-mark (inexact->exact (floor (* (srate snd) (cadr descr)))) snd)))
			(set! (mark-name m) (format #f "~A ~A ~A" (car descr) (cadr descr) (caddr descr)))))
		    mark-list))
		 (lambda ()
		   (set! (squelch-update snd 0) old-update)))
	     result))
		  
	 (lambda ()
	   (set! *clm-notehook* old-notehook)))))

;;; (with-marked-sound () (do ((i 0 (1+ i))) ((= i 5)) (fm-violin i .1 440 .1)))



;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(defmacro sound-let (snds . body) 
  `(let ((temp-files '())
	 (old-hook-list (hook->list new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (begin
       (reset-hook! new-sound-hook)
       (add-hook! new-sound-hook (lambda (file)       ; save current sound-let temp file list
				   (if (string? file) ; try to ignore vcts and sound-data objects
				       (set! temp-files (cons file temp-files)))))
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				                      ; if with-sound, embed with-temp-sound
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))              ; else use direct (normal var in the let)
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (and (string? file)          ; is it a file? (might be a vct or sound-data object)
			      (file-exists? file))
			 (delete-file file)))
		   temp-files)
	 (reset-hook! new-sound-hook)                 ; restore old new-sound-hook (should this happen before ,@body?)
	 (if (not (null? old-hook-list))
	     (for-each (lambda (proc)
			 (add-hook! new-sound-hook proc))
		       old-hook-list))
	 val))))                                      ; return body result



;;; -------- Common Music --------

(define* (init-with-sound
	  :key (srate *clm-srate*) 
	  (output *clm-file-name*) 
	  (channels *clm-channels*)
	  (header-type *clm-header-type*)
	  (data-format *clm-data-format*)
	  (comment #f)
	  ;(verbose *clm-verbose*) ; why is this commented out?
	  (reverb *clm-reverb*)
	  (revfile "test.rev")
	  (reverb-data *clm-reverb-data*)
	  (reverb-channels *clm-reverb-channels*)
	  (continue-old-file #f)
	  (statistics *clm-statistics*)
	  (scaled-to #f)
	  (play *clm-play*)
	  (to-snd *to-snd*)
	  (scaled-by #f))
  "(init-with-sound . args) is the first half of with-sound; it sets up the CLM output choices, reverb, etc. Use \
finish-with-sound to complete the process."
  (let ((old-srate (mus-srate))
	(start (if statistics (get-internal-real-time)))
	(output-to-file (string? output))
	(reverb-to-file (and reverb (string? revfile))))
    (if output-to-file
	(if continue-old-file
	    (begin
	      (set! *output* (continue-sample->file output))
	      (set! (mus-srate) (mus-sound-srate output))
	      (let ((ind (find-sound output)))
		(if (sound? ind)
		    (close-sound ind))))
	    (begin
	      (if (file-exists? output) 
		  (delete-file output))
	      (set! *output* (make-sample->file output channels data-format header-type comment))))
	(begin
	  (if (not continue-old-file)
	      (if (vct? output)
		  (vct-fill! output 0.0)
		  (sound-data-fill! output 0.0)))
	  (set! *output* output)))

    (if reverb
	(if reverb-to-file
	    (if continue-old-file
		(set! *reverb* (continue-sample->file revfile))
		(begin
		  (if (file-exists? revfile) 
		      (delete-file revfile))
		  (set! *reverb* (make-sample->file revfile reverb-channels data-format header-type))))
	    (begin
	      (if (not continue-old-file)
		  (if (vct? revfile)
		      (vct-fill! revfile 0.0)
		      (sound-data-fill! revfile 0.0)))
	      (set! *reverb* revfile))))

    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  to-snd
	  scaled-to
	  scaled-by
	  play
	  reverb-data
	  start)))

(define (finish-with-sound wsd)
  "(finish-with-sound wsd) closes the notelist process started by init-with-sound"
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
	    (play (list-ref wsd 9))
	    (reverb-data (list-ref wsd 10))
	    (start (list-ref wsd 11)))

	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (if (string? revfile)
		  (set! *reverb* (make-file->sample revfile))
		  (set! *reverb* revfile))
	      (apply reverb reverb-data)
	      (mus-close *reverb*)))
	(if (mus-output? *output*)
	    (mus-close *output*))

	(if statistics
	    (set! cycles (/ (- (get-internal-real-time) start) 100)))
	(if (and to-snd (string? output))
	    (let ((snd-output (open-sound output)))
	      (set! (sync snd-output) #t)
	      (if statistics
		  (snd-print 
		   (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			   output
			   (maxamp snd-output #t)
			   cycles)))
		 (if (or scaled-to scaled-by)
		     (begin
		       (if scaled-to
			   (scale-to scaled-to snd-output)
			   (if scaled-by
			       (scale-by scaled-by snd-output)))
		       (save-sound snd-output)))
	      (if play (play-and-wait 0 snd-output))
	      (update-time-graph snd-output)))
	(set! (mus-srate) old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))


(define wsdat-play ; for cm
  (make-procedure-with-setter
   (lambda (w)
     "accessor for play field of init-with-sound struct"
     (list-ref w 9))
   (lambda (w val)
     (list-set! w 9 val))))


;;; -------- with-sound save state --------

(if (not (defined? 'open-appending))
    (define (open-appending filename)
      (if (provided? 'snd-guile)
	  (open filename (logior O_RDWR O_APPEND))
	  (open-output-file filename :if-exists :append :if-does-not-exist :create))))

(if (not (defined? 'close-appending))
    (define (close-appending fd)
      (if (provided? 'snd-guile)
	  (close fd)
	  (close-output-port fd))))

(define (ws-save-state filename)
  "(ws-save-state filename) is an after-save-state-hook function that saves the current with-sound global settings"
  (let ((fd (open-appending filename)))
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
    (close-appending fd)))

(add-hook! after-save-state-hook ws-save-state)


;;; -------- with-mix --------
;;;
;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body
;;;
;;; (with-sound () 
;;;   (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
;;;                              (fm-violin 1 2 660 .1))
;;; ...)

(define (with-mix-find-file-with-extensions file extensions)
  "(with-mix-find-file-with-extensions file extensions) helps the with-mix macro find checkpoint files"
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
  "(with-mix-file-extension file default) is a helper function for the with-mix macro"
  (let ((len (string-length file)))
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
					     (append (list :output 
							   (string-append chkpt-file "." (with-mix-file-extension *clm-file-name* "snd")))
						     (list :comment 
							   (format #f "(begin~%;; written ~A (Snd: ~A)~%(list ~S ~S))~%"
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


;;; -------- def-optkey-fun --------
;;;
;;; this is a translation of CLM's def-optkey-fun

(defmacro def-optkey-fun (decls . body)
  (let ((func-name (car decls))
	(func-args (cdr decls)))
    (if (null? func-args)
	`(define (,func-name) ,@body)
	(let ((args (map (lambda (arg)
			   (symbol->keyword (if (list? arg) (car arg) arg)))
			 func-args))
	      (key-name (string->symbol (string-append (symbol->string func-name) "-1"))))
	  (if (= (length func-args) 1)
	      `(begin
		 (define* (,key-name :key ,@func-args) ,@body)
		 (define* (,func-name :rest passed-args)
		   (if (or (null? passed-args)
			   (keyword? (car passed-args)))
		       (apply ,key-name passed-args)
		       (apply ,key-name ,(car args) passed-args))))
	      `(begin
		 (define* (,key-name :key ,@func-args) ,@body)
		 (define* (,func-name :rest passed-args)
		   (if (or (null? passed-args)
			   (keyword? (car passed-args)))
		       (apply ,key-name passed-args)
		       (let ((arglen (length passed-args)))
			 (if (or (= arglen 1) 
				 (and (> arglen 2) 
				      (keyword? (cadr passed-args))))
			     (apply ,key-name ,(car args) (car passed-args) (cdr passed-args))
			     (if (or (= arglen 2) 
				     (and (> arglen 3) 
					  (keyword? (caddr passed-args))))
				 (apply ,key-name 
					,(car args) (car passed-args) 
					,(cadr args) (cadr passed-args) 
					(cddr passed-args))
				 (let ((allargs (call-with-current-continuation
						 (lambda (break)
						   (let ((arglist '())
							 (pa passed-args)
							 (na ',args))
						     (do ((k 0 (1+ k)))
							 ((= k arglen) arglist)
						       (if (keyword? (car pa))
							   (break (append arglist pa))
							   (begin
							     (set! arglist (append arglist (list (car na) (car pa))))
							     (set! pa (cdr pa))
							     (set! na (cdr na))))))))))
				   (apply ,key-name allargs)))))))))))))

(defmacro def-optkey-instrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names '()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names))))
  `(begin 
     (def-optkey-fun (,name ,@targs)
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       (if (not (zero? (run-safety))) 
	   (set! *ws-stack* (make-stack #t)))
       ((lambda () ; for inner defines, if any
	  ,@body)))
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))


(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 (/ 57 12)))) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                             ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0)))
    (lambda* (pitch :optional pythagorean)          ; pitch can be pitch name or actual frequency
      "(->frequency pitch :optional pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."
      (if (symbol? pitch)
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (base-char (string-ref name 0))
		 (sign-char (and (> (string-length name) 1)
				 (not (char-numeric? (string-ref name 1)))
				 (not (char=? (string-ref name 1) #\n))
				 (string-ref name 1)))
		 (octave-char (if (and (> (string-length name) 1)
				       (char-numeric? (string-ref name 1))) 
				  (string-ref name 1)
				  (if (and (> (string-length name) 2) 
					   (char-numeric? (string-ref name 2)))
				      (string-ref name 2)
				      #f)))
		 (base (modulo (+ 5 (- (char->integer base-char) (char->integer #\a))) 7)) ; c-based (diatonic) octaves
		 (sign (if (not sign-char) 0 (if (char=? sign-char #\f) -1 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (+ sign (case base ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (if pythagorean
		(* main-pitch (expt 2 octave) (vector-ref ratios base-pitch))
		(* main-pitch (expt 2.0 (/ et-pitch 12)))))
	  pitch))))

(define (->sample beg)
  "(->sample time-in-seconds) -> time-in-samples"
  (inexact->exact (round (* (if (not (null? (sounds))) (srate) (mus-srate)) beg))))



;;; -------- def-clm-struct --------

(if (not (defined? 'add-clm-type)) (define (add-clm-type . args) #f)) ; these are in snd-run
(if (not (defined? 'add-clm-field)) (define (add-clm-field . args) #f))


;;;  :(def-clm-struct (osc :make-wrapper (lambda (gen) (set! (osc-freq gen) (hz->radians (osc-freq gen))) gen)) freq phase)
;;;  #<unspecified>
;;;  :(define hi (make-osc 440.0 0.0))
;;;  #<unspecified>
;;;  :hi
;;;  (osc 0.125378749798983 0.0)

;;; besides setting up the list accessors, the make function, and the type predicate, this
;;;   calls add-clm-type to make sure run knows about the struct and, on each field,
;;;   add-clm-field to tell run the type of each list element (only actually needed if
;;;   there are different types in use)
;;;
;;; see defgenerator in generators.scm for an extension that adds various methods such as mus-describe

(defmacro def-clm-struct (struct-name . fields)
  (let* ((name (if (list? struct-name) (car struct-name) struct-name))
	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (list-ref struct-name 1) :make-wrapper)
				    (list-ref struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (list-ref struct-name 3) :make-wrapper)
				    (list-ref struct-name 4))))
		      (lambda (gen) gen)))
	 (methods (and (list? struct-name)
		       (or (and (> (length struct-name) 2)
				(equal? (list-ref struct-name 1) :methods)
				(list-ref struct-name 2))
			   (and (= (length struct-name) 5)
				(equal? (list-ref struct-name 3) :methods)
				(list-ref struct-name 4)))))
	 (sname (if (string? name) name (symbol->string name)))
	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))
	 (field-types (map (lambda (n)
			     (if (and (list? n) (cadr n) (eq? (cadr n) :type)) 
				 (snd-error (format #f ":type indication for def-clm-struct (~A) field (~A) should be after the default value" name n)))
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (list-ref n 2) :type))
				 (list-ref n 3)
				 (if (and (list? n)
					  (= (length n) 2))
				     (if (number? (cadr n))
					 (if (exact? (cadr n))
					     'int
					     'float)
					 (if (string? (cadr n))
					     'string
					     (if (char? (cadr n))
						 'char
						 (if (or (equal? (cadr n) #t)
							 (equal? (cadr n) #f))
						     'boolean
						     'float))))
				     'float)))
			   fields)))
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   "clm struct type check"
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))
       (define ,(string->symbol (string-append sname "-methods"))
	 (lambda ()
	   "clm struct local method list accessor"
	   ,methods))
       (def-optkey-fun (,(string->symbol (string-append "make-" sname))
		        ,@(map (lambda (n)
				(if (and (list? n)
					 (>= (length n) 2))
				    (list (car n) (cadr n))
				    (list n 0.0)))
			      fields))
	 (,wrapper (if (list? ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)
			     ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)))))
       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   "clm struct field accessor"
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field sname (string-append sname "-" n) ctr type)
		    (set! ctr (1+ ctr))
		    val)))
	      field-names field-types))))


;;; ----------------
;;;
;;; display all the globals that might affect with-sound unexpectedly

(define (clm-display-globals)

  (format #f ";CLM globals:~%;  *clm-srate*: ~A (default: ~A, mus-srate: ~A)~%;  *clm-file-name*: ~A~%;  *clm-channels: ~A (default: ~A)~%;  *clm-data-format*: ~A (default: ~A)~%;  *clm-header-type*: ~A (default: ~A)~%;  *clm-reverb-channels*: ~A, *clm-reverb-data*: ~A~%;  *clm-table-size*: ~A~%;  *clm-file-buffer-size*: ~A (~A)~%;  *clm-locsig-type*: ~A~%;  *clm-array-print-length*: ~A (~A)~%;  *clm-notehook*: ~A~%;  *clm-default-frequency*: ~A~%;  *clm-clipped*: ~A, mus-clipping: ~A, mus-prescaler: ~A~%;  *clm-threads*: ~A~%~%"

	  *clm-srate* (default-output-srate) (mus-srate)
	  *clm-file-name*
	  *clm-channels* (default-output-chans)
	  (mus-data-format->string *clm-data-format*) (mus-data-format->string (default-output-data-format))
	  (mus-header-type->string *clm-header-type*) (mus-header-type->string (default-output-header-type))
	  *clm-reverb-channels* *clm-reverb-data*
	  *clm-table-size*
	  *clm-file-buffer-size* (mus-file-buffer-size)
	  *clm-locsig-type*
	  *clm-array-print-length* (print-length)
	  *clm-notehook*
	  *clm-default-frequency*
	  *clm-clipped* (mus-clipping)
	  (mus-prescaler)
	  *clm-threads*))


