(use-modules (ice-9 debug) (ice-9 optargs) (ice-9 format))

;;; TODO: help dialog for environment (how are showdowed vars handled?) (click to expand?)
;;;       similarly stack with click to show locals + args?
;;;       or multi-level display stack|vars|local|expanded
;;; TODO: test snd-break/debug
;;; TODO: doc strings

;;; -------- backtrace --------

(define *snd-port* (make-soft-port
		    (vector
		     (lambda (c) (snd-print c))
		     (lambda (s) (snd-print s))
		     (lambda () #f)
		     (lambda () #f)
		     (lambda () #f))
		    "w"))

(define* (backtrace stack #:optional all)
  (if (stack? stack)
      (if all
	  (display-backtrace stack *snd-port*)
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

(define *break-continue* #f)
(define *break-stack* #f)
(define *break-level* 0)

(define* (break-go #:optional val)
  (if (continuation? *break-continue*)
      (*break-continue* val)
      ";nothing to go to"))

(define* (break-locals #:optional (index 0)) (local-variables *break-stack* index))
(define* (break-local obj #:optional (index 0)) (local-variable *break-stack* obj index))
(define* (break-backtrace #:optional all) (backtrace *break-stack* all))

(define (break-help)
  "\n\
;The 'break:' prompt means you're in the Snd debugger.\n\
;This is the standard Snd listener, so anything is ok at this level, but\n\
;there are also some additional functions:\n\
;  (break-go (return-value #f)) continues from the point of the snd-break call.\n\
;  (break-locals (index 0)) shows the local variables and their values.\n\
;  (break-local obj) shows the value of obj.\n\
;  (break-backtrace (full #f)) shows the backtrace at the point of the break.\n\
")

(define (snd-break)
  (let ((stack (make-stack #t))
	(old-prompt (listener-prompt))
	(old-break-continue *break-continue*)
	(old-break-stack *break-stack*))
    (call-with-current-continuation
     (lambda (continue)
       (reset-listener-cursor)
       (dynamic-wind
	(lambda ()
	  (set! *break-level* (1+ *break-level*))
	  (if (> *break-level* 1)
	      (set! (listener-prompt) (format #f "break(~D):" *break-level*))
	      (set! (listener-prompt) "break:"))
	  (goto-listener-end))
	(lambda ()
	  (event-loop))
	(lambda ()
	  (set! (listener-prompt) old-prompt)
	  (set! *break-level* (max 0 (1- *break-level*)))
	  (set! *break-stack* old-break-stack)
	  (set! *break-continue* old-break-continue)))))))


;;; -------- snd-debug --------

(define *stack* #f)
(define (bt) (backtrace *stack*))
(define* (lv #:optional obj) (if obj (local-variable *stack* obj) (local-variables *stack*)))
(define (snd-debug) (set! *stack* (fluid-ref the-last-stack)))


;;; -------- snd-trace 
;;; 
;;; this activates tracing and redirects its output to the Snd listener

(defmacro snd-trace (body)
  "(snd-trace body) activates tracing and redirects its output to the Snd listener"
  `(let* ((stderr (current-error-port)))
     (dynamic-wind
      (lambda ()
	(snd-print "\n")
	(set-current-error-port *snd-port*))
      (lambda ()
	(with-traps 
	 (lambda ()
	   (start-stack 'repl-stack ,body))))
      (lambda ()
	(set-current-error-port stderr)))))
