;;; various envelope functions

;;; -------- envelope-interp (named envelope-interp in clm's env.lisp)
;;;
;;; (envelope-interp .3 '(0 0 .5 1 1 0) -> .6

(define envelope-interp                      ;env is list of x y breakpoint pairs, interpolate at x returning y
  (lambda args                          ;  (x env &optional (base 1.0)
    "(envelope-interp x env &optional (base 1.0)) -> value of env at x (base controls connecting segment type)"
    (let ((x (car args))
	  (env (cadr args))
	  (base (if (null? (cddr args)) #f (caddr args))))
      (cond ((null? env) 0.0)		;no data -- return 0.0
	    ((or (<= x (car env))	;we're sitting on x val (or if < we blew it)
		 (null? (cddr env)))	;or we're at the end of the list
	     (cadr env))		;so return current y value
	    ((> (caddr env) x)		;x <= next env x axis value
	     (if (or (= (cadr env) (cadddr env))
		     (and base (= base 0.0)))
		 (cadr env)		;y1=y0, so just return y0 (avoid endless calculations below)
		 (if (or (not base) (= base 1.0))
		     (+ (cadr env)	;y0+(x-x0)*(y1-y0)/(x1-x0)
			(* (- x (car env))
			   (/ (- (cadddr env) (cadr env))
			      (- (caddr env) (car env)))))
		     (+ (cadr env)
			(* (/ (- (cadddr env) (cadr env))
			      (- base 1.0))
			   (- (expt base (/ (- x (car env))
					    (- (caddr env) (car env))))
			      1.0))))))
	    (else (envelope-interp x (cddr env))))))) ;go on looking for x segment

;;; -------- window-envelope (a kinda brute-force translation from the CL version in env.lisp)
;;;
;;; (window-envelope 1.0 3.0 '(0.0 0.0 5.0 1.0)) -> '(1.0 0.2 3.0 0.6)

(define window-envelope 
  (lambda (beg end env)
    "(window-envelope beg end env) -> portion of env lying between x axis values beg and end"
    (let ((nenv '())
	  (lasty (if env (cadr env) 0.0))
	  (len (length env)))
      (call-with-current-continuation
       (lambda (return-early)               
	 (do ((i 0 (+ i 2)))
	     ((>= i len))
	   (let ((x (list-ref env i))
		 (y (list-ref env (+ i 1))))
	     (set! lasty y)
	     (if (null? nenv)
		 (if (>= x beg)
		     (begin
		       (set! nenv (append nenv (list beg (envelope-interp beg env))))
		       (if (not (= x beg))
			   (if (>= x end)
			       (return-early (append nenv (list end (envelope-interp end env))))
			       (set! nenv (append nenv (list x y)))))))
		 (if (<= x end)
		     (begin
		       (set! nenv (append nenv (list x y)))
		       (if (= x end)
			   (return-early nenv)))
		     (if (> x end)
			 (return-early (append nenv (list end (envelope-interp end env)))))))))
	 (append nenv (list end lasty)))))))

;;; map-envelopes like map-across-envelopes in env.lisp

(define map-envelopes 
  (lambda (op e1 e2)
    "(map-envelopes func env1 env2) maps func over the breakpoints in env1 and env2 returning a new envelope"
    (let ((xs '()))
      (letrec ((at0 
		(lambda (e)
		  (let* ((diff (car e))
			 (len (length e))
			 (lastx (list-ref e (- len 2))))
		    (do ((i 0 (+ i 2)))
			((>= i len) e)
		      (let ((x (/ (- (list-ref e i) diff) lastx)))
			(set! xs (cons x xs))
			(list-set! e i x))))))
	       (remove-duplicates
		(lambda (lst)
		  (letrec ((rem-dup
			    (lambda (lst nlst)
			      (cond ((null? lst) nlst)
				    ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
				    (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
		    (rem-dup lst '())))))
	(if (null? e1)
	    (at0 e2)
	    (if (null? e2)
		(at0 e1)
		(let ((ee1 (at0 e1))
		      (ee2 (at0 e2))
		      (newe '()))
		  (set! xs (sort! (remove-duplicates xs) <))
		  (let ((len (length xs)))
		    (do ((i 0 (1+ i)))
			((= i len))
		      (let ((x (list-ref xs i)))
			(set! newe (append newe (list x (op (envelope-interp x ee1) (envelope-interp x ee2)))))))
		    newe))))))))

(define multiply-envelopes
  (lambda (e1 e2)
    "(multiply-envelopes env1 env2) multiplies break-points of env1 and env2 returning a new envelope"
    (map-envelopes * e1 e2)))

; (multiply-envelopes '(0 0 2 .5) '(0 0 1 2 2 1)) -> '(0 0 0.5 0.5 1.0 0.5)

(define max-envelope
  (lambda (e mx)
    "(max-envelope env 0.0) -> max y value in env)"
    (if (null? e)
	mx
      (max-envelope (cddr e) (max mx (abs (cadr e)))))))

(define integrate-envelope
  (lambda (e sum)
    "(integrate-envelope env 0.0) -> area under env"
    (if (or (null? e) (null? (cddr e)))
	sum
      (integrate-envelope (cddr e) (+ sum (* (+ (cadr e) (cadddr e)) .5 (- (caddr e) (car e))))))))

(define max-x
  (lambda (e)
    "(max-x env) -> max x axis break point position"
    (if (null? (cddr e))
	(car e)
      (max-x (cddr e)))))

