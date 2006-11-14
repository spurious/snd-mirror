(use-modules (ice-9 debug) (ice-9 optargs) (ice-9 format))
(provide 'snd-debug.scm)

(if (provided? 'snd-gauche)
    (snd-warning ";debug.scm depends on functions specific to Guile."))


;;; -------- backtrace --------

(define *snd-port* (make-soft-port
		    (vector
		     (lambda (c) (snd-print c))
		     (lambda (s) (snd-print s))
		     (lambda () #f)
		     (lambda () #f)
		     (lambda () #f))
		    "w"))

(define* (snd-backtrace stack #:optional all)
  "(snd-backtrace stack (all #f)) displays a stack backtrace.  If 'all' is not #t, 
it looks for the first procedure on the stack and centers around that."
  (if (stack? stack)
      (if all
	  (snd-backtrace stack *snd-port*)
	  (let ((len (stack-length stack)))
	    (call-with-current-continuation
	     (lambda (ok)
	       (do ((i 3 (1+ i)))
		   ((= i len))
		 (if (frame-procedure? (stack-ref stack i))
		     (begin
		       (display-backtrace stack *snd-port* (min (1- len) (1+ i)) (min i 5))
		       (ok #f))))
	       (display-backtrace stack *snd-port*)))))
      ";no stack"))


;;; -------- local variables --------

(define* (local-variables stack #:optional (index 0))
  "(local-variables stack (index 0)) displays the local variables on the stack. 'index' sets 
the stack frame to use (0 is the current frame)"
  (if (stack? stack)
      (let ((vars (memoized-environment (frame-source (stack-ref stack index)))))
	(if vars
	    (for-each 
	     (lambda (var)
	       (if (and (or (list? var)
			    (pair? var))
			(symbol? (car var)))
		   (snd-print (format #f "; ~A: ~A~%" (car var) (cdr var)))))
	     vars)
	    ";no locals"))
      ";no stack"))

(define* (local-variable stack obj #:optional (index 0))
  "(local-variable stack obj (index 0) shows the value of obj searching in the stack frame 'index' 
where 0 is the current frame."
  (if (stack? stack)
      (let ((vars (memoized-environment (frame-source (stack-ref stack index))))
	    (sym (if (string? obj) (string->symbol obj) obj)))
	(if vars
	    (or
	      (call-with-current-continuation
	       (lambda (found-it)
		 (for-each 
		  (lambda (var)
		    (if (and (or (list? var)
				 (pair? var))
			     (symbol? (car var))
			     (eq? sym (car var)))
			(found-it (format #f ";~A: ~A" (car var) (cdr var)))))
		  vars)
		 #f))
	      (format #f ";can't find ~A" obj))
	    ";no locals"))
      ";no stack"))

;;; setter for local-variable? 


;;; -------- breakpoint --------

(define *break-continues* '())
(define *break-continue* #f)
(define *break-stacks* '())
(define *break-stack* #f)
(define *break-top-level-prompt* (listener-prompt))

(define (break-prompt)
  (let ((stack-len (length *break-stacks*)))
    (if (> stack-len 1)
	(format #f "break(~D)~A" (1- stack-len) *break-top-level-prompt*)
	(if (> stack-len 0)
	    (format #f "break~A" *break-top-level-prompt*)
	    *break-top-level-prompt*))))

(define (pop-break)
  (if (not (null? *break-continues*))
      (begin
	(set! *break-continue* (car *break-continues*))
	(set! *break-continues* (cdr *break-continues*)))
      (set! *break-continue* #f))
  (if (not (null? *break-stacks*))
      (begin
	(set! *break-stack* (car *break-stacks*))
	(set! *break-stacks* (cdr *break-stacks*)))
      (set! *break-stack* #f))
  (set! (listener-prompt) (break-prompt))
  (snd-print (format #f "~%~A" (listener-prompt)))
  (goto-listener-end))

(define (push-break continue stack)
  (if (null? *break-continues*) (set! *break-top-level-prompt* (listener-prompt)))
  (set! *break-continues* (cons *break-continue* *break-continues*))
  (set! *break-continue* continue)
  (set! *break-stacks* (cons *break-stack* *break-stacks*))
  (set! *break-stack* stack)
  (set! (listener-prompt) (break-prompt))
  (snd-print (format #f "~%~A" (listener-prompt)))
  (goto-listener-end))

(define* (break-go #:optional val)
  "(break-go (val #f)) tries to continue from the point of the last snd-break call. 'val' is 
the value returned by the snd-break function."
  (let ((current-continuation *break-continue*))
    (pop-break)
    (if (continuation? current-continuation)
	(current-continuation val)
	";nothing to go to")))

(define* (break-locals #:optional (index 0))
  "(break-locals (index 0)) shows the local variables at the index-th frame (0 is the current frame) after a call to snd-break"
  (local-variables *break-stack* index))

(define* (break-local obj #:optional (index 0)) 
  "(break-local obj (index 0)) shows the value of obj in the context of the last snd-break call."
  (local-variable *break-stack* obj index))

(define* (break-backtrace #:optional all) 
  "(break-backtrace (all #f)) shows the stack backtrace at the point of the last snd-break call."
  (snd-backtrace *break-stack* all))

(define (break-quit)
  "(break-quit) exits a snd-break context"
  (pop-break))

(define (break-quit!)
  "(break-quit!) exists all current break contexts, returning you to the true top-level."
  (pop-break)
  (if (not (null? *break-continues*))
      (break-quit!)))

(define (break-help)
  "
;The 'break' prompt means you're in the Snd debugger.
;This is the standard Snd listener, so anything is ok at this level, but
;there are also some additional functions:
;  (break-go (return-value #f)) continues from the point of the snd-break call.
;  (break-locals (index 0)) shows the local variables and their values.
;  (break-local obj) shows the value of obj.
;  (break-backtrace (full #f)) shows the backtrace at the point of the break.
;  (break-quit) exits the current break context.
;  (break-quit!) exits all break contexts.
")

(define* (snd-break #:optional message)
  "(snd-break (message #f)) prints 'message' if any, then drops you into the debugger.  (break-help) at
that point prints out the break-specific options."
  (let ((stack (make-stack #t)))
    (call-with-current-continuation
     (lambda (continue)
       (push-break continue stack)       ;save pre-existing break info, if any, and set up this break's context
       (throw 'snd-top-level message))))) ;exit to top-level, but with break continuation/stack set up for examination etc

;(define (testing a) (let ((b (+ a (snd-break "hiho")))) b))
;(define hi 123)
;(set! hi (testing 1))


;;; -------- snd-debug --------

(define *stack* #f)
(define (bt) 
  "(bt) displays the stack backtrace at the point of the last throw (presumably an error indication). 
To set up the needed information, call snd-debug first."
  (if (not *stack*) (set! *stack* (fluid-ref the-last-stack)))
  (snd-backtrace *stack*))

(define* (lv #:optional obj) 
  "(lv (obj #f)) shows the values of either all local variables ('obj' omitted), or a given local 
variable ('obj') from the point of the last throw (presumably an error indication). To set up the 
needed information, call snd-debug first (after receiving the error)."
  (if (not *stack*) (set! *stack* (fluid-ref the-last-stack)))
  (if obj (local-variable *stack* obj) (local-variables *stack*)))

(define (snd-debug) 
  "(snd-debug) sets up the info needed by bt and lv to look into the context of the last throw (error)."
  (set! *stack* (fluid-ref the-last-stack)))


;;; -------- snd-trace --------

(defmacro snd-trace body
  "(snd-trace body) activates tracing and redirects its output to the Snd listener.  To get trace info, 
first call trace with the function (not its name) you want to trace, then wrap snd-trace around the 
code that will invoke that function."
  `(let* ((stderr (current-error-port)))
     (dynamic-wind
      (lambda ()
	(snd-print #\newline)
	(set-current-error-port *snd-port*))
      (lambda ()
	(with-traps 
	 (lambda ()
	   (start-stack 'repl-stack (begin ,@body)))))
      (lambda ()
	(set-current-error-port stderr)))))
