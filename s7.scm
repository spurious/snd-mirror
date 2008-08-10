;;; init for S7 from:

;    Initialization file for TinySCHEME 1.39

; Per R5RS, up to four deep compositions should be defined
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list-tail x k)
  (if (zero? k)
      x
      (list-tail (cdr x) (- k 1))))

(define (list-head l n)
  (reverse (list-tail (reverse l) (- (length l) n))))

(define (list-copy list)
  (if (null? list)
      '()
      (cons (car list)
            (list-copy (cdr list)))))

(define (list-ref x k)
    (car (list-tail x k)))

(define (last-pair x)
    (if (pair? (cdr x))
        (last-pair (cdr x))
        x))



(macro (unless form)
     `(if (not ,(cadr form)) (begin ,@(cddr form))))

(macro (when form)
     `(if ,(cadr form) (begin ,@(cddr form))))

; DEFINE-MACRO Contributed by Andy Gaynor
(macro (define-macro dform)
  (if (symbol? (cadr dform))
      `(macro ,@(cdr dform))
      (let ((form (gensym)))
	`(macro (,(caadr dform) ,form)
	   (apply (lambda ,(cdadr dform) ,@(cddr dform)) (cdr ,form))))))


(define (log10 n) (/ (log n) (log 10)))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define call/cc call-with-current-continuation)

(define (string . charlist)
     (list->string charlist))

(define (list->string charlist)
     (let* ((len (length charlist))
            (newstr (make-string len))
            (fill-string!
               (lambda (str i len charlist)
                    (if (= i len)
                         str
                         (begin (string-set! str i (car charlist))
                         (fill-string! str (+ i 1) len (cdr charlist)))))))
          (fill-string! newstr 0 len charlist)))

(define (string-fill! s e)
     (let ((n (string-length s)))
          (let loop ((i 0))
               (if (= i n)
                    s
                    (begin (string-set! s i e) (loop (1+ i)))))))

(define (string->list s)
     (let loop ((n (1- (string-length s))) (l '()))
          (if (= n -1)
               l
               (loop (1- n) (cons (string-ref s n) l)))))

(define (string-copy str)
     (string-append str))

(define (string->anyatom str pred)
  (let* ((a (string->atom str)))
    (and (pred a) a)))

(define (string->number str)
  (string->anyatom str number?))

(define (anyatom->string n pred)
  (if (pred n)
      (atom->string n)
      #f)) ; TODO error should go through error hook

(define (number->string n) (anyatom->string n number?))    

(define (char-cmp? cmp a b)
     (cmp (char->integer a) (char->integer b)))
(define (char-ci-cmp? cmp a b)
     (cmp (char->integer (char-downcase a)) (char->integer (char-downcase b))))

(define (char=? a b) (char-cmp? = a b))
(define (char<? a b) (char-cmp? < a b))
(define (char>? a b) (char-cmp? > a b))
(define (char<=? a b) (char-cmp? <= a b))
(define (char>=? a b) (char-cmp? >= a b))

(define (char-ci=? a b) (char-ci-cmp? = a b))
(define (char-ci<? a b) (char-ci-cmp? < a b))
(define (char-ci>? a b) (char-ci-cmp? > a b))
(define (char-ci<=? a b) (char-ci-cmp? <= a b))
(define (char-ci>=? a b) (char-ci-cmp? >= a b))

; Note the trick of returning (cmp x y)
(define (string-cmp? chcmp cmp a b)
     (let ((na (string-length a)) (nb (string-length b)))
          (let loop ((i 0))
               (cond
                    ((= i na)
                         (if (= i nb) (cmp 0 0) (cmp 0 1)))
                    ((= i nb)
                         (cmp 1 0))
                    ((chcmp = (string-ref a i) (string-ref b i))
                         (loop (1+ i)))
                    (else
                         (chcmp cmp (string-ref a i) (string-ref b i)))))))


(define (string=? a b) (string-cmp? char-cmp? = a b))
(define (string<? a b) (string-cmp? char-cmp? < a b))
(define (string>? a b) (string-cmp? char-cmp? > a b))
(define (string<=? a b) (string-cmp? char-cmp? <= a b))
(define (string>=? a b) (string-cmp? char-cmp? >= a b))

(define (string-ci=? a b) (string-cmp? char-ci-cmp? = a b))
(define (string-ci<? a b) (string-cmp? char-ci-cmp? < a b))
(define (string-ci>? a b) (string-cmp? char-ci-cmp? > a b))
(define (string-ci<=? a b) (string-cmp? char-ci-cmp? <= a b))
(define (string-ci>=? a b) (string-cmp? char-ci-cmp? >= a b))

(define (list . x) x)

(define (foldr f x lst)
     (if (null? lst)
          x
          (foldr f (f x (car lst)) (cdr lst))))

(define (unzip1-with-cdr . lists)
  (unzip1-with-cdr-iterative lists '() '()))

(define (unzip1-with-cdr-iterative lists cars cdrs)
  (if (null? lists)
      (cons cars cdrs)
      (let ((car1 (caar lists))
	    (cdr1 (cdar lists)))
	(unzip1-with-cdr-iterative 
	 (cdr lists) 
	 (append cars (list car1))
	 (append cdrs (list cdr1))))))

(define (map proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
	  '()
	  (let* ((unz (apply unzip1-with-cdr lists))
		 (cars (car unz))
		 (cdrs (cdr unz)))
	    (cons (apply proc cars) (apply map (cons proc cdrs)))))))

(define (for-each proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
	  #t
	  (let* ((unz (apply unzip1-with-cdr lists))
		 (cars (car unz))
		 (cdrs (cdr unz)))
	    (apply proc cars) (apply map (cons proc cdrs))))))

(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))

(define (list->vector x)
     (apply vector x))

(define (vector-fill! v e)
     (let ((n (vector-length v)))
          (let loop ((i 0))
               (if (= i n)
                    v
                    (begin (vector-set! v i e) (loop (1+ i)))))))

(define (vector->list v)
     (let loop ((n (1- (vector-length v))) (l '()))
          (if (= n -1)
               l
               (loop (1- n) (cons (vector-ref v n) l)))))

;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis

(macro
 quasiquote
 (lambda (l)
   (define (mcons f l r)
     (if (and (pair? r)
              (eq? (car r) 'quote)
              (eq? (car (cdr r)) (cdr f))
              (pair? l)
              (eq? (car l) 'quote)
              (eq? (car (cdr l)) (car f)))
         (if (or (procedure? f) (number? f) (string? f))
               f
               (list 'quote f))
         (if (eqv? l vector)
               (apply l (eval r))
               (list 'cons l r)
               )))
   (define (mappend f l r)
     (if (or (null? (cdr f))
             (and (pair? r)
                  (eq? (car r) 'quote)
                  (eq? (car (cdr r)) '())))
         l
         (list 'append l r)))
   (define (foo level form)
     (cond ((not (pair? form))
               (if (or (procedure? form) (number? form) (string? form))
                    form
                    (list 'quote form))
               )
           ((eq? 'quasiquote (car form))
            (mcons form ''quasiquote (foo (+ level 1) (cdr form))))
           (#t (if (zero? level)
                   (cond ((eq? (car form) 'unquote) (car (cdr form)))
                         ((eq? (car form) 'unquote-splicing)
                          (error "Unquote-splicing wasn't in a list:"
                                 form))
                         ((and (pair? (car form))
                               (eq? (car (car form)) 'unquote-splicing))
                          (mappend form (car (cdr (car form)))
                                   (foo level (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))
                   (cond ((eq? (car form) 'unquote)
                          (mcons form ''unquote (foo (- level 1)
                                                     (cdr form))))
                         ((eq? (car form) 'unquote-splicing)
                          (mcons form ''unquote-splicing
                                      (foo (- level 1) (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))))))
   (foo 0 (car (cdr l)))))


;;;;; atom? and equal? written by a.k

;;;; atom?
(define (atom? x)
  (not (pair? x)))


;;;; (do ((var init inc) ...) (endtest result ...) body ...)
;;
(macro do
  (lambda (do-macro)
    (apply (lambda (do vars endtest . body)
             (let ((do-loop (gensym)))
               `(letrec ((,do-loop
                           (lambda ,(map (lambda (x)
                                           (if (pair? x) (car x) x))
                                      `,vars)
                             (if ,(car endtest)
                               (begin ,@(cdr endtest))
                               (begin
                                 ,@body
                                 (,do-loop
                                   ,@(map (lambda (x)
                                            (cond
                                              ((not (pair? x)) x)
                                              ((< (length x) 3) (car x))
                                              (else (car (cdr (cdr x))))))
                                       `,vars)))))))
                  (,do-loop
                    ,@(map (lambda (x)
                             (if (and (pair? x) (cdr x))
                               (car (cdr x))
                               '()))
                        `,vars)))))
      do-macro)))

;;;; generic-member
(define (generic-member cmp obj lst)
  (cond
    ((null? lst) #f)
    ((cmp obj (car lst)) lst)
    (else (generic-member cmp obj (cdr lst)))))

(define (memq obj lst)
     (generic-member eq? obj lst))
(define (memv obj lst)
     (generic-member eqv? obj lst))
(define (member obj lst)
     (generic-member equal? obj lst))

;;;; generic-assoc
(define (generic-assoc cmp obj alst)
     (cond
          ((null? alst) #f)
          ((cmp obj (caar alst)) (car alst))
          (else (generic-assoc cmp obj (cdr alst)))))

(define (assq obj alst)
     (generic-assoc eq? obj alst))
(define (assv obj alst)
     (generic-assoc eqv? obj alst))
(define (assoc obj alst)
     (generic-assoc equal? obj alst))

(define (acons x y z) (cons (cons x y) z))

;;;; Utility to ease macro creation
(define (macro-expand form)
     ((eval (get-closure-code (eval (car form)))) form))

;;;; Handy for imperative programs
;;;; Used as: (define-with-return (foo x y) .... (return z) ...)
(macro (define-with-return form)
     `(define ,(cadr form)
          (call/cc (lambda (return) ,@(cddr form)))))


;;;;; Definition of MAKE-ENVIRONMENT, to be used with two-argument EVAL

(macro (make-environment form)
     `(apply (lambda ()
               ,@(cdr form)
               (current-environment))))

(define-macro (eval-polymorphic x . envl)
  (display envl)
  (let* ((env (if (null? envl) (current-environment) (eval (car envl))))
         (xval (eval x env)))
    (if (closure? xval)
	(make-closure (get-closure-code xval) env)
	xval)))

; Redefine this if you install another package infrastructure
; Also redefine 'package'
(define *colon-hook* eval)

;;;;; I/O

(define (input-output-port? p)
     (and (input-port? p) (output-port? p)))

(define (close-port p)
     (cond 
          ((input-output-port? p) (close-input-port (close-output-port p)))
          ((input-port? p) (close-input-port p))
          ((output-port? p) (close-output-port p))
          (else (throw "Not a port" p))))

(define (call-with-input-file s p)
     (let ((inport (open-input-file s)))
          (if (eq? inport #f)
               #f
               (let ((res (p inport)))
                    (close-input-port inport)
                    res))))

(define (call-with-output-file s p)
     (let ((outport (open-output-file s)))
          (if (eq? outport #f)
               #f
               (let ((res (p outport)))
                    (close-output-port outport)
                    res))))

(define (with-input-from-file s p)
     (let ((inport (open-input-file s)))
          (if (eq? inport #f)
               #f
               (let ((prev-inport (current-input-port)))
                    (set-input-port inport)
                    (let ((res (p)))
                         (close-input-port inport)
                         (set-input-port prev-inport)
                         res)))))

(define (with-output-to-file s p)
     (let ((outport (open-output-file s)))
          (if (eq? outport #f)
               #f
               (let ((prev-outport (current-output-port)))
                    (set-output-port outport)
                    (let ((res (p)))
                         (close-output-port outport)
                         (set-output-port prev-outport)
                         res)))))

(define (with-input-output-from-to-files si so p)
     (let ((inport (open-input-file si))
           (outport (open-input-file so)))
          (if (not (and inport outport))
               (begin
                    (close-input-port inport)
                    (close-output-port outport)
                    #f)
               (let ((prev-inport (current-input-port))
                     (prev-outport (current-output-port)))
                    (set-input-port inport)
                    (set-output-port outport)
                    (let ((res (p)))
                         (close-input-port inport)
                         (close-output-port outport)
                         (set-input-port prev-inport)
                         (set-output-port prev-outport)
                         res)))))


;; SRFI-0 
;; COND-EXPAND
;; Implemented as a macro
(define *features* '(srfi-0))

(define-macro (cond-expand . cond-action-list)
  (cond-expand-runtime cond-action-list))

(define (cond-expand-runtime cond-action-list)
  (if (null? cond-action-list)
      #t
      (if (cond-eval (caar cond-action-list))
          `(begin ,@(cdar cond-action-list))
          (cond-expand-runtime (cdr cond-action-list)))))

(define (cond-eval-and cond-list)
  (foldr (lambda (x y) (and (cond-eval x) (cond-eval y))) #t cond-list))

(define (cond-eval-or cond-list)
  (foldr (lambda (x y) (or (cond-eval x) (cond-eval y))) #f cond-list))

(define (cond-eval condition)
  (cond ((symbol? condition)
	 (if (member condition *features*) #t #f))
	((eq? condition #t) #t)
	((eq? condition #f) #f)
	(else (case (car condition)
		((and) (cond-eval-and (cdr condition)))
		((or) (cond-eval-or (cdr condition)))
		((not) (if (not (null? (cddr condition)))
			   (error "cond-expand : 'not' takes 1 argument")
			   (not (cond-eval (cadr condition)))))
		(else (error "cond-expand : unknown operator" (car condition)))))))

(gc-verbose #f)
(tracing 0)


;;; --------------------------------------------------------------------------------

(define (provide sym)
  (set! *features* (cons sym *features*)))

(define (provided? sym)
  (member sym *features*))

(define *load-path* '())

(define (call-with-output-string t)
  (let* ((p (open-output-string))
	 (r (t p))
	 (s (get-output-string p)))
    (close-output-port p)
    s))

(define (call-with-input-string s t)
  (let* ((p (open-input-string s))
	 (r (t p)))
    (close-input-port p)
    r))


;;; defmacro from slib
(define-macro (defmacro name args . body)
 `(define-macro (,name ,@args) ,@body))

;;; scheme side make-procedure-with-setter -- not pretty (only needed for snd-nogui.c fallbacks)
(defmacro make-procedure-with-setter (name getter setter)
  `(begin
     (define ,(string->symbol (string-append "set-" name)) ,setter)
     ,getter))

;(define-syntax defmacro
;  (syntax-rules ()
;    ((_ name params . body) (define-macro (name . params) . body)))))

(define (object->string obj)
  (call-with-output-string
   (lambda (port)
     (write obj port))))

(define (with-output-to-string thunk)
  (let* ((output-port (current-output-port))
	 (string-port (open-output-string)))
    (set-output-port string-port)
    (thunk)
    (let ((result (get-output-string string-port)))
      (set-output-port output-port)
      (close-output-port string-port)
      result)))


(define *catcher* #f)
(define *tag* #f)

(define (throw . args)
  (if *catcher*
      (begin
	(apply *catcher* (append *tag* args))
	(set! *catcher* #f)))
  'error)

(define *error-hook* throw)

(define (catch tag body tag-handler)
  (call/cc
   (lambda (catcher)
     (set! *catcher* catcher)
     (set! *tag* tag)
     (let ((result (body)))
       (set! *catcher* #f)
       result))))
     
