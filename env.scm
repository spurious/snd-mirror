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
  (lambda (env)
    "(max-envelope env) -> max y value in env)"
    (define max-envelope-1
      (lambda (e mx)
      (if (null? e)
	  mx
          (max-envelope-1 (cddr e) (max mx (abs (cadr e)))))))
    (max-envelope-1 env 0.0)))

(define integrate-envelope
  (lambda (env)
    "(integrate-envelope env) -> area under env"
    (define integrate-envelope-1
      (lambda (e sum)
	(if (or (null? e) (null? (cddr e)))
	    sum
	    (integrate-envelope-1 (cddr e) (+ sum (* (+ (cadr e) (cadddr e)) .5 (- (caddr e) (car e))))))))
    (integrate-envelope-1 env 0.0)))

(define max-x
  (lambda (e)
    "(max-x env) -> max x axis break point position"
    (if (null? (cddr e))
	(car e)
      (max-x (cddr e)))))


;;; return a new envelope taking into account the attack and decay times given
(define (stretch-envelope . args)
  (let ((fn (list-ref args 0))
	(old-att (if (> (length args) 1) (list-ref args 1) #f))
	(new-att (if (> (length args) 2) (list-ref args 2) #f))
	(old-dec (if (> (length args) 3) (list-ref args 3) #f))
	(new-dec (if (> (length args) 4) (list-ref args 4) #f)))
    (if (and old-att
	     (not new-att))
	(snd-error (format #f "old-attack but no new-attack? (stretch-envelope ~{~A ~})" args))
	(if (not new-att)
	    fn
	    (if (and old-dec
		     (not new-dec))
		(snd-error (format #f "old-decay but no new-decay? (stretch-envelope ~{~A ~})" args))
		(let* ((x0 (car fn))
		       (new-x x0)
		       (last-x (list-ref fn (- (length fn) 2)))
		       (y0 (cadr fn))
		       (new-fn (list y0 x0))
		       (scl (/ (- new-att x0) (max .0001 (- old-att x0)))))
		  (define (stretch-envelope-1 new-fn old-fn)
		    (if (null? old-fn)
			new-fn
			(let ((x1 (car old-fn))
			      (y1 (cadr old-fn)))
			  (if (and (< x0 old-att)
				   (>= x1 old-att))
			      (begin
				(if (= x1 old-att)
				    (set! y0 y1)
				    (set! y0 (+ y0 (* (- y1 y0) (/ (- old-att x0) (- x1 x0))))))
				(set! x0 old-att)
				(set! new-x new-att)
				(set! new-fn (cons new-x new-fn))
				(set! new-fn (cons y0 new-fn))
				(set! scl (if old-dec 
					      (/ (- new-dec new-att) (- old-dec old-att))
					      (/ (- last-x new-att) (- last-x old-att))))))
			  (if (and old-dec
				   (< x0 old-dec)
				   (>= x1 old-dec))
			      (begin
				(if (= x1 old-dec)
				    (set! y0 y1)
				    (set! y0 (+ y0 (* (- y1 y0) (/ (- old-dec x0) (- x1 x0))))))
				(set! x0 old-dec)
				(set! new-x new-dec)
				(set! new-fn (cons new-x new-fn))
				(set! new-fn (cons y0 new-fn))
				(set! scl (/ (- last-x new-dec) (- last-x old-dec)))))
			  (if (not (= x0 x1))
			      (begin
				(set! new-x (+ new-x (* scl (- x1 x0))))
				(set! new-fn (cons new-x new-fn))
				(set! new-fn (cons y1 new-fn))
				(set! x0 x1)
				(set! y0 y1)))
			  (stretch-envelope-1 new-fn (cddr old-fn)))))
		  
		  (if (and old-dec 
			   (= old-dec old-att)) 
		      (set! old-dec (* .000001 last-x)))
		  (reverse (stretch-envelope-1 new-fn (cddr fn)))))))))

    
