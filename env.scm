;;; various envelope functions
;;;
;;; envelope-interp (x env &optional (base 1.0)) -> value of env at x (base controls connecting segment type)
;;; window-envelope (beg end env) -> portion of env lying between x axis values beg and end
;;; map-envelopes (func env1 env2) maps func over the breakpoints in env1 and env2 returning a new envelope
;;;   multiply-envelopes (env1 env2) multiplies break-points of env1 and env2 returning a new envelope
;;;   add-envelopes (env1 env2) adds break-points of env1 and env2 returning a new envelope
;;; max-envelope (env) -> max y value in env
;;; integrate-envelope (env) -> area under env
;;; envelope-last-x (env) -> max x axis break point position
;;; stretch-envelope env old-attack new-attack &optional old-decay new-decay -> divseg-like envelope mangler
;;; scale-envelope (env scaler &optional offset) scales y axis values by 'scaler' and optionally adds 'offset'
;;; reverse-envelope (env) reverses the breakpoints in 'env'
;;; concatenate-envelopes (&rest envs) concatenates its arguments into a new envelope


(use-modules (ice-9 format) (ice-9 optargs))

;;; -------- envelope-interp

(define envelope-interp                      ;env is list of x y breakpoint pairs, interpolate at x returning y
  (lambda args                          ;  (x env &optional (base 1.0)
    "(envelope-interp x env &optional (base 1.0)) -> value of env at x; base controls connecting segment \
type: (envelope-interp .3 '(0 0 .5 1 1 0) -> .6"
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

(define (window-envelope beg end env)
  "(window-envelope beg end env) -> portion of env lying between x axis values beg and \
end: (window-envelope 1.0 3.0 '(0.0 0.0 5.0 1.0)) -> '(1.0 0.2 3.0 0.6)"
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
       (append nenv (list end lasty))))))


;;; -------- map-envelopes like map-across-envelopes in env.lisp

(define (map-envelopes op e1 e2)
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
		  newe)))))))


;;; -------- multiply-envelopes, add-envelopes

(define (multiply-envelopes e1 e2)
  "(multiply-envelopes env1 env2) multiplies break-points of env1 and env2 returning a new \
envelope: (multiply-envelopes '(0 0 2 .5) '(0 0 1 2 2 1)) -> '(0 0 0.5 0.5 1.0 0.5)"
  (map-envelopes * e1 e2))

(define (add-envelopes e1 e2)
  "(add-envelopes env1 env2) adds break-points of env1 and env2 returning a new envelope"
  (map-envelopes + e1 e2))


;;; -------- max-envelope

(define (max-envelope env)
  "(max-envelope env) -> max y value in env"
  (define (max-envelope-1 e mx)
    (if (null? e)
	mx
	(max-envelope-1 (cddr e) (max mx (abs (cadr e))))))
  (max-envelope-1 env 0.0))


;;; -------- integrate-envelope

(define (integrate-envelope env)
  "(integrate-envelope env) -> area under env"
  (define (integrate-envelope-1 e sum)
    (if (or (null? e) (null? (cddr e)))
	sum
	(integrate-envelope-1 (cddr e) (+ sum (* (+ (cadr e) (cadddr e)) .5 (- (caddr e) (car e)))))))
  (integrate-envelope-1 env 0.0))


;;; -------- envelope-last-x

(define (envelope-last-x e)
  "(envelope-last-x env) -> max x axis break point position"
  (if (null? (cddr e))
      (car e)
      (envelope-last-x (cddr e))))


;;; -------- stretch-envelope

(define (stretch-envelope . args)

  "(stretch-envelope env old-attack new-attack &optional old-decay new-decay) takes 'env' and \
returns a new envelope based on it but with the attack and optionally decay portions stretched \
or squeezed; 'old-attack' is the original x axis attack end point, 'new-attack' is where that \
section should end in the new envelope.  Similarly for 'old-decay' and 'new-decay'.  This mimics \
divseg in early versions of CLM and its antecedents in Sambox and Mus10 (linen).\n\
  (stretch-envelope '(0 0 1 1) .1 .2) -> (0 0 0.2 0.1 1.0 1) \n\
  (stretch-envelope '(0 0 1 1 2 0) .1 .2 1.5 1.6) -> (0 0 0.2 0.1 1.1 1 1.6 0.5 2.0 0)"

  (let ((fn (list-ref args 0))
	(old-att (if (> (length args) 1) (list-ref args 1) #f))
	(new-att (if (> (length args) 2) (list-ref args 2) #f))
	(old-dec (if (> (length args) 3) (list-ref args 3) #f))
	(new-dec (if (> (length args) 4) (list-ref args 4) #f)))
    (if (and old-att
	     (not new-att))
	(throw 'wrong-number-of-args (list "stretch-envelope" 
					   old-attack 
					   "old-attack but no new-attack?"))
	(if (not new-att)
	    fn
	    (if (and old-dec
		     (not new-dec))
		(throw 'wrong-number-of-args (list "stretch-envelope" 
						   old-attack new-attack old-decay
						   "old-decay but no new-decay?"))
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

    
;;; -------- scale-envelope

(define* (scale-envelope e scl #:optional (offset 0))
  "(scale-envelope env scaler &optional offset) scales y axis values by 'scaler' and optionally adds 'offset'"
  (if (null? e)
      '()
      (append (list (car e) (+ offset (* scl (cadr e))))
	      (scale-envelope (cddr e) scl offset))))


;;; -------- reverse-envelope

(define (reverse-envelope e)
  "(reverse-envelope env) reverses the breakpoints in 'env'"
  (define (reverse-env-1 e newe xd)
    (if (null? e)
	newe
	(reverse-env-1 (cddr e)
		       (cons (- xd (car e))
			     (cons (cadr e)
				   newe))
		       xd)))
  (let ((len (length e)))
    (if (or (= len 0) (= len 2))
	e
	(let ((xmax (list-ref e (- len 2))))
	  (reverse-env-1 e '() xmax)))))


;;; -------- concatenate-envelopes

(define* (concatenate-envelopes #:rest envs)
  "(concatenate-envelopes &rest envs) concatenates its arguments into a new envelope"
  (define (cat-1 e newe xoff x0)
    (if (null? e)
	newe
	(cat-1 (cddr e)
	       (cons (cadr e)
		     (cons (+ (- (car e) x0) xoff)
			   newe))
	       xoff
	       x0)))
  (let ((ne '())
	(xoff 0.0))
    (for-each 
     (lambda (e)
       (if (and (not (null? ne))
		(= (car ne) (cadr e)))
	   (begin
	     (set! xoff (- xoff .01))
	     (set! ne (cat-1 (cddr e) ne xoff (car e))))
	   (set! ne (cat-1 e ne xoff (car e))))
       (set! xoff (+ xoff .01 (cadr ne))))
     envs)
    (reverse ne)))


;;; TODO: from env.lisp power env, db env, repeat env
