
#!

rt-compiler.scm
-Kjetil S. Matheussen/Notam, 2005

rt-compiler.scm is developed with support from Notam/Oslo:
http://www.notam02.no


Oops! Not everything in the documentation below is implemented yet,
and some of the things that already are implemented will probably
change. This file is currently under heavy development.



***************************************************************
Introduction
************
rt-compiler provides various functions and macros to compile]1]
and run simple lisp functions[2[.

The original purpose of the langauge was to generate code that
can be hard realtime safe, (can run safely inside the jack realtime
thread), but the compiler can also be used for general number
crunching. The generated code should be extremely efficient.

(Note that the current implementation isn't very optimal, so
you probably have to set
(debug-set! stack 200000)
in some initialization file if you want to compile larger functions.)



***************************************************************
Short example
**************

(define-rt (fib n)
  (let* ((fib (lambda (n)
		(if (< n 2)
		    n
		    (+ (fib (- n 1))
		       (fib (- n 2)))))))
    (fib n)))
(fib 30)
=> 832040.0



***************************************************************
Features
********
-Compilation of very simple lisp functions into machine code.
-All compiled code should normally be hard real time safe.
-Usually the generated code is extremely efficient.
 Hand-written c-code should normally not run any faster.
-Possible to both read and write Guile variables.
-Guile can both read and write variables which is used inside
 the compiled functions.
-Lisp macros
-Most of Common Lisp Music is supported as well as various other snd and sndlib functions.
-It _should_ not be possible to cause a segfault by running
 a compiled functions. But for now, I know that at least when dividing or moduling by
 0, you will get a segfault. I don't know how to handle that situation yet.
 There are also probably a lot of other situations that might cause
 a segfault, so be careful. Please send me code that cause segfaults.
-Extensive error checking. If there is an error in your code that
 cause the compilation to stop, you should get a human readable
 explanation about it. (at least, most of the time, maybe).
-An interface so that you can add your own functions written in eval-c.




***************************************************************
Limitations
***********
-Only three types: float, int and shared.
-No dynamic types
-A bit limited function-set. No list operations, for example.
-The language does not have a name.
-No optional arguments or keyword arguments. (This can
 be fixed with the help of macros though.)
-#f=0, #t=1, #undefined=0
-Not possible to call Guile functions.



***************************************************************
Functions and macros to compile and run rt-functions
****************************************************

rt/rt-funcall => (define a (rt (lambda (b c) (* b c))))
                 (rt-funcall a 2 3)
                  => 6.0

rt-func       => (define a (rt-func (lambda (b c) (* b c))))
                 (a 2 3)
                 => 6.0

define-rt     => (define-rt (a b c) (* b c))
	         (a 2 3)
                 => 6.0

rt-run        => (rt-run 1 11
			 (lambda ()
			   (out (- (random 1.8) 0.9))))
                 (rt-start)
                 [one second later, white noise is heard for ten seconds]

(rt-run requires that rt-engine.scm has been loaded first.)


For define-rt, I have the following lines in my .emacs file:

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(define-rt\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 (cond ((match-beginning 1) font-lock-function-name-face)
	     ((match-beginning 2) font-lock-variable-name-face)
	     (t font-lock-type-face))
       nil t))))



***************************************************************
Types
*****

The rt language does not support dynamic typing, and only three types are supported:
float, int and shared.


Float
-----
By default all variables are floats:

(let* ((onefloat 1)
       (anotherfloat 2))
  ...)


Int
---
To use ints, one must specify by using <int> inside the let*-block:

(let* ((<int> oneint 1)
       (<int> anotherint 2))
  ...)

For lambda-functions and defines:
(lambda ((<int> a) b)
  ...)
(define (afunc (<int> a) b)
  ...)

(Topic: the size of the int type is currently sizeof(int) in C, which is normally 32 bit.
	Should the int type always be 64 bits instead?)

Important! There is no boolean type, so #f=0 and #t=1.


Shared [3]
----------
"shared" is a special type, because it specify a float variable that can
be accessed from guile.

A shared type is specified by using < and > on the sides of the
variable name. A shared type is always a float, so the following
is not possible: (let* ((<int> <oneshared> 0)) ...).

Example 1: (a bad example)

(define rt-func (rt (lambda ()
		      (let* ((<oneshared> 0))
			(set! <oneshared> (1+ <oneshared>))
			<oneshared>))))
=> (rt-func ....)

(rt-get rt-func oneshared)
=> 0

(rt-funcall rt-func)
=> 1

(rt-get rt-func oneshared)
=> 1


Example 2: (a better example)

A shrared variable is especially (or perhaps only) useful in conjunction with
the rt-run macro:

(definstrument (rt-oscil freq)
  (let ((osc (make-oscil :freq freq)))
    (rt-run 0 10
	    (lambda ()
	      (let* ((<vol> 1))
		(out (* <vol>
			(oscil osc))))))))
=> #<unspecified>

(define osc (rt-oscil))
=> #<unspecified>

(-> rt-engine start)
=> #<unspecified>

(set! (-> osc vol) 0.1)
=> #<unspecified>

(-> rt-engine stop)
=> #<unspecified>


(The shared type in the rt language is supposed to provide the same functionality as
the "args" type in the SuperCollider3 SynthDef objects.)




**************************************************************************
Macros:
*******

Macros are straight forward:

(define-rt-macro (add . args)
  `(+ ,@args))

(rt-funcall (rt (lambda (a b c)
		  (add a b c)))
	    2 3 4)
=> 9


For the define-rt-macro macro, I have the following lines in my .emacs file:

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(define-rt-macro\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face
       nil t))))




**************************************************************************
Special forms and special form-like macros:
*******************************************

Special forms
-------------
begin => Works as in scheme


break => Used to break out of a while loop:
         (while 1
		(break))


define => Works nearly as in guile, but unlike guile,

          (begin
	    (set! a 2)
	    (define d 9))

          ...is legal. And

          (let ((b 9))
	    (define (a) (+ b 2))
	    (define b 3))

          ...will return 11.

          The second one will return 11 instead of 5 because defines are tranlated to let* instead of letrec
          since letrec is not supported. Beware that this behaviour might/will probably change.


if => Works as in scheme. But beware that there is no special boolean type (#f=0 and #t=1).
      Therefore, the following expression will return 1, which is not the case
      for scheme: (if 0 0 1)


lambda =>  Works as in scheme, except:
           *Functions might not be tail-recursive if possible. Its not very
            difficult to guarantiue a function to be tail-recursive for single functions, but I think gcc already supports
	    tail-recursive functions, so I hope its not necesarry to add it explicitly. But I might be wrong!
           *Rest argument is not supported: (lambda (a . rest) ...) (error)
            (You can work around this to a certain degree by using macros)
           *The functions are usually compiled into c-functions that return a float.
            So if your function returns a string or something other than an int or a float,
            the c-function will instead always return 0: 

            (rt-funcall (rt (lambda ()
			      (let* ((b 2)
				     (a (lambda ()
					  (set! b 9))))
				(+ 5 (a))))))
            => 5.0

           *The functions can return <int> as well: (let* ((<int> afunc (lambda (a b) ....))))
            Other return-types are currently not supported.


let => Is currently not supported.


let* => Works as in scheme, but recursive functions work as well:
        (let* ((twicify (lambda (a)
			  (if (> a 0)
			      (+ 2 (twicify (1- a)))
			      a))))
	  (twicify 200))


letrec => Is currently not supported, but should probably be. Please tell
          me if its needed.

          Probably, I'll decide to never add letrec
          spesific though, but instead expand the functionality for let*.
          So! beware that the following block might return 2 instead of 1
          in the future:

          (let* ((a (lambda () 1)))
             (let* ((b (lambda () (a)))
		    (a (lambda () 2)))
	       (b)))

          (In case you have some thoughts on the syntax for let/let*/letrec,
	   I really want to hear them. (My personal opinion is that only one of them
	   should be enough, but thats not very lispish...))


set! => Works as in scheme, except that all Guile variables that might be set! will
        be converted to inexact before calling no matter whether its set in the function or not:

        (let ((a 5))
	  (rt-funcall (rt (lambda ()
			    (if 0
				(set! a 9)))))
	  a)
        => 5.0

        And, this will not work:

        (let ((a #f))
	  (rt-funcall (rt (lambda ()
			    (set! a 9)))))
        (error)

        ...because a must be a number.


while => Works as in Guile (Using C's while)



Special form-like macros
------------------------
and => works as in scheme

cond => works as in sheme, but "=>" is not supported

do => works as in scheme (does not expand to a recursive function)

or => works as in scheme

range => (range i 5 10
		(printf "%d " i))
         => 5 6 7 8 9 10
        (does not expand to a recursive function)






**************************************************************************
Functions and macros: (unless note, works as in scheme)
*******************************************************

+ - * /
1+ 1-
min max
< > <= >= =
not
or and
sin cos tan abs log exp expt acos asin atan sqrt
atan2 (see "man atan2")

zero? positive? negative? odd? even?
remainder modulo quotient

floor ceiling truncate round truncate

logand logior lognot logxor ash
random (only float random)

printf (Using c's fprintf with stderr as the first argument. Warning, this one is not realtime safe!)



**************************************************************************
Using Common Lisp Music:
************************

Almost all common lisp music classes are supported, as well as all their methods:


(define osc (make-oscil :frequency 440))
(define func (rt (lambda ()
		   (oscil osc))))
(rt-funcall func))
=>0.0
(rt-funcall func))
=>0.125050514936447

However, this will not work:

(define func (rt (lambda ()
		   (let* ((osc (make-oscil :frequency 440)))
		     (oscil osc)))))

...because the clm constructors are not supported.





**************************************************************************
Notes
*****

[1] I guess "translator" would be a more accurate word to use for the
    process, rather than "compiler".
[2] Since lists are not supported, calling the functions for "lisp functions"
    probably isn't correct. (?)
[3] Would "port" be a better word than "shared"? Or perhaps "args"? ("args"
    is the SuperCollider3 word for the same type)


**************************************************************************
**************************************************************************
**************************************************************************
!#


(provide 'snd-rt-compiler.scm)

(use-modules (srfi srfi-1))


(if (not (provided? 'snd-oo.scm)) (load-from-path "oo.scm"))

(c-load-from-path eval-c)


(if (not (defined? 'snd-header-files-path))
    (let ((path #f))
      (for-each (lambda (l-path)
		  (if (not path)
		      (if (access? (string-append l-path "/clm.h") R_OK)
			  (set! path l-path))))
		%load-path)
      (if path
	  (define-toplevel 'snd-header-files-path path)
	  (begin
	    (c-display "Error! Header files for SND not found. Try setting snd-header-files-path.")
	    (catch 'header-files-path-not-found)))))
      




;; Various general functions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (number-or-symbol? . rest)
  (if (null? rest)
      #t
      (and (or (number? (car rest))
	       (symbol? (car rest)))
	   (apply number-or-symbol? (cdr rest)))))
  
(eval-c  (string-append "-I" snd-header-files-path)
	 "#include <clm.h>"
	 "#include <xen.h>"
	 "#include <clm2xen.h>"

	 (public
	  (<void> rt-set-float (lambda ((<SCM> das_float) (<float> newval))
				 (set! (SCM_REAL_VALUE das_float) newval))))
					
	 (<SCM> gakk (lambda ((<SCM> scm))
		       (return (MAKE_POINTER (XEN_TO_MUS_ANY scm)))))
	 (<SCM> gakk2 (lambda ((<SCM> sym) (<SCM> toplevel))
			(return (scm_sym2var sym toplevel SCM_BOOL_F))))
	 (run-now
	  (scm_c_define_gsubr (string "XEN_TO_MUS_ANY") 1 0 0 gakk)
	  (scm_c_define_gsubr (string "c-global-symbol") 2 0 0 gakk2)))


;; Get the address of a guile SCM variable.
(define-macro (c-get-cell-address varname)
  (let ((findfunc (string->symbol (eval-c-get-unique-name))))
    `(let ((,findfunc (procedure->macro (lambda (x env)
					  (define (findit env)
					    (call-with-current-continuation
					     (lambda (return)
					       (c-display "env" env)
					       (cond ((not (list? env)) (return #f))
						     ((= 1 (length env)) (return (let ((varname2 ',varname))
										   `(c-global-symbol ,varname2 ,(car env)))))
						     ;;((= 1 (length env)) (return `(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module)))))
						     ((= 1 (length env)) (return (car env)))
						     (else
						      (let ((names (car (car env)))
							    (vals (cdr (car env))))
							(if (not (list? names))
							    (if (eq? ',varname names)
								(return vals))
							    (for-each (lambda (name val)
									(if (eq? ',varname name)
									    (return ,val)))
								      names
								      vals)))
						      (findit (cdr env)))))))
					  (findit env)))))
       (,findfunc))))
;;(c-display (,findfunc))




					     
#!
(define ai2 90.0)
(let ((ai 50))
  (c-get-cell-address ai2))
(c-global-symbol 'ai2 (interaction-environment))
(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module)))
(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module))))))
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-operators '(+ - * / = < > <= >=))

(define rt-very-special-forms '(if begin let*))
(define rt-very-special-forms2 (cons 'lambda rt-very-special-forms))

(define rt-macro-prefix 'rt-macro-)
	 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Recursively check if the term contains a very special form.
;; (In special situations it can return #t even though it should
;;  have returned #f.)
(define (rt-contains-very-special-form? term)
  (call-with-current-continuation
   (lambda (return)
     (define (check term)
       (cond ((and (symbol? term)
		   (member term rt-very-special-forms))
	      (return #t))
	     ((not (list? term)) #f)
	     ((null? term) #f)
	     (else
	      (for-each check term))))
     (check term)
     #f)))
#!
(rt-contains-very-special-form? '((let2* begin a b)))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-remove-unused++ removes unused variables and numbers.
;;
;; It will also redo forms that will confuse C, based on
;; whether a function will be translated to a c-operator.
;;
;; And finally it will reorganize let* values that contains very special forms.
;;
;; rt-check-calls must have been called on the term before calling
;;
;; (begin 2 4) -> (begin 4)
;; (begin (+ 2 3) 4)) -> (begin (let* ((u2 (lambda () (+ 2 3)))) (u2)) 4)
;; (begin (set! a 5) 3) -> (begin (set! a 5) 3)
;; (let* ((a (if b c))) ...) -> (let* ((u3 (lambda () (if b c))) (a (u3))) ...)

(define (rt-remove-unused++ term)
  (define (das-remove t)
    (cond ((null? t) t)
	  ((= (length t) 1) (list (rt-remove-unused++ (car t))))
	  ((not (list? (car t)))
	   (das-remove (cdr t)))
	  ((member (caar t) rt-operators)
	   (let ((funcname (string->symbol (eval-c-get-unique-name))))
	     (cons
	      `(let* ((,funcname <float> (lambda ()
					   ,(rt-remove-unused++ (car t)))))
		 (,funcname))
	      (das-remove (cdr t)))))
	  (else
	   (cons (rt-remove-unused++ (car t))
		 (das-remove (cdr t))))))
  
  (cond ((not (list? term)) term)
	((eq? 'begin (car term))
	 `(begin ,@(das-remove (cdr term))))
	((eq? 'let* (car term))
	 (let ((newlets '()))
	   (for-each (lambda (let*form)
		       (let ((value (caddr let*form)))
			 (if (and (list? value)
				  (not (eq? 'lambda (car value)))
				  (rt-contains-very-special-form? value))
			     (let ((helpfuncname (string->symbol (eval-c-get-unique-name))))
			       (set! newlets (cons (list helpfuncname (cadr let*form) `(lambda () ,value))
						   newlets))
			       (set! newlets (cons (list (car let*form) (cadr let*form) (list helpfuncname))
						   newlets)))
			     (set! newlets (cons let*form newlets)))))
		     (cadr term))
	   (set! newlets (reverse! newlets))
	   `(let* ,(map (lambda (let*form)
			  (list (car let*form)
				(cadr let*form)
				(rt-remove-unused++ (caddr let*form))))
			newlets)
	      ,@(das-remove (cddr term)))))
	((eq? 'lambda (car term))
	 `(lambda ,(cadr term)
	    ,@(das-remove (cddr term))))
	(else
	 (map rt-remove-unused++ term))))

#!
(rt-remove-unused++ '(lambda ()
		       (let* ((a <float> (let* ((b <float> 5))
					   a b c)))
			 a)))

(rt-remove-unused++ '(let* ((d <float> 0)
			    (a <float> (lambda () (+ 2 (if b c)))))
		       (a)))

			  
->
(let* ((d 0)
       (u1 (lambda ()
	     (if b c)))
       (a (u1)))
  ...)

  

(rt-remove-unused++ '(begin 2 5))
(rt-remove-unused++ '(lambda ()
		     3
		     (begin 2 5)))

(rt-funcify (rt-remove-unused++ '(begin (begin 2 4) 3)))
(rt-remove-unused++ '(begin (+ 2 3) 4))
(rt-remove-unused++ '(begin (set! a 5) 3))
(rt-funcify (rt-remove-unused++ '(let* ((a <float> (lambda ()
						   2
						   (+ (begin 2 5) 3))))
				 (+ 2 3 4)
				 (a))))
(rt-remove-unused++ '(begin 2
			  (+ 2 3 4)
			  (set! a 5)
			  0
			  3))

2+3+4;
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add return calls.
;; rt-funcify must have been called on the term before calling.
;;
;; (5) -> (return 5)
;; (begin (set! a 2) 5) -> (begin (set! a 2) (return 5))
;; (let* ((<float> a (lambda () 3))) 4) -> (let* ((<float> a (lambda () return 3))) (return 4))
;;
(define (rt-insert-returns term)
  (define (insertit term)
    (if (and (list? term)
	     (member (car term) rt-very-special-forms))
	(rt-insert-returns term)
	`(return ,term)))
  (cond ((not (list? term)) term)
	((null? term) term)
	((list? (car term))
	 (map rt-insert-returns term))
	((eq? 'begin (car term))
	 `(begin
	    ,@(rt-insert-returns (c-butlast (cdr term)))
	    ,(insertit (last term))))
	((eq? 'lambda (car term))
	 `(lambda ,(cadr term)
	    ,@(rt-insert-returns (c-butlast (cddr term)))
	    ,(insertit (last (cddr term)))))
	((eq? 'let* (car term))
	 `(let* ,(map (lambda (t)
			(list (car t)
			      (cadr t)
			      (rt-insert-returns (caddr t))))
		      (cadr term))
	    ,@(rt-insert-returns (c-butlast (cddr term)))
	    ,(insertit (last (cddr term)))))
	((eq? 'if (car term))
	 `(if ,(cadr term)
	      ,(insertit (caddr term))
	      ,(if (null? (cdddr term))
		   '(return 0)
		   (insertit (cadddr term)))))
	(else
	 (c-display "jepp")
	 term)))



#!
(rt-insert-returns '(lambda ()
		      (let* ((a <float> (lambda ()
					  (let* ((b <float> 5))
					    b))))
			(+ 2 3))
		      6))

(rt-insert-returns '5)
(rt-insert-returns '(begin a))
(rt-insert-returns '(begin (+ 2 3 4)))
(rt-insert-returns '(begin (set! a 2) 5))
(rt-insert-returns '(begin (let* ((< <float> 2)) 3)))
(rt-insert-returns '(let* ((a <int> (lambda () 3))) b))
(rt-funcify '(begin (+ 3 (let* ((a <int> 2)) (+ a (begin 5) 4)))))
(rt-insert-returns (rt-funcify '(begin (+ 3 (let* ((a <int> 2)) (+ a (begin 5) 4))))))
(rt-insert-returns '(let* ((unique_name_5 <int> (lambda () (begin 4)))) (begin (unique_name_5) 3)))

(rt-insert-returns '(begin (if 2
			       3)
			   (+ a b)))
(rt-insert-returns (rt-funcify '(begin (if 2
					   3)
				       (+ a b))))
(rt-insert-returns '(if (let* ((a <int> 5))
			  a)
			(begin 
			  (set! a 3)
			  (+ 2 3))))
(rt-insert-returns (rt-funcify '(if (let* ((a <int> 5))
				      a)
				    (begin 
				      (set! a 3)
				      (+ 2 3)))))
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funcify special forms.
;; rt-remove-unused++ must be called on the term before calling.
;;
;; This is not allowed in C: (+ 2 (let* ((<float> a 3)) a) b)
;; However, gcc supports local functions, so we make a bunch of them.
;;
;; (+ 2 (if a b)) -> (let* ((<float> u23 (lambda () (if a b)))) (+ 2 (u23)))
;;
;; (This function can still be optimized.)
;;
(define (rt-must-funcify? must term)
  (call-with-current-continuation
   (lambda (return)
     (cond ((not (list? term)) #f)
	   ((null? term) #f)
	   ((member (car term) rt-very-special-forms)
	    (if (not must)
		(return #t)
		(if (and (eq? (car term) 'let*)
			 (= (length term) 3))
		    (begin
		      (for-each (lambda (t)
				  (if (rt-must-funcify? #t (caddr t))
				      (return #t)))
				(cadr term))
		      (return (rt-must-funcify? #t (caddr term))))
		    (if (= (length term) 2)
			(return (rt-must-funcify? #t (cadr term)))
			(for-each (lambda (t)
				    (if (rt-must-funcify? #f t)
					(return #t)))
				  term))))
	    #f)
	   ((eq? (car term) 'lambda)
	    (rt-must-funcify? #t `(begin
				    ,@(cddr term))))
;	    (for-each (lambda (t)
;			;; #f or #t as argument??
;			(if (rt-must-funcify? #t t)
;			    (return #t)))
;		      (cddr term))
;	    #f)
	   (else
	    (for-each (lambda (t)
			(if (rt-must-funcify? #f t)
			    (return #t)))
		      term)
	    #f)))))

#!

(rt-must-funcify? #t '(lambda ()
			(let* ((a <float> 6))
			  a)
			5)) -> #t

(rt-must-funcify? #t '(lambda ()
			(let* ((unique_name_193 <float> (lambda ()
							  (let* ((a <float> 6))
							    a))))
			  (begin
			    (unique_name_193)
			    5)))) -> #f

(rt-must-funcify? #t '(lambda ()
			(set! a 5)
			(set! b 10))) -> #f

(rt-must-funcify? #t '(lambda ()
			(begin
			  (set! a 5)
			  (set! b 10)))) -> #f

(rt-must-funcify? #t '(begin (let* ((<float> a 5))
			       a)
			     9)) -> #t

(rt-must-funcify? #t  '(lambda ()
			 (let* ((b <float> 6))
			   b))) -> #f

(rt-must-funcify? #t '(begin (if 2 3 4))) -> #f
(rt-must-funcify? #t '(let* ((ai <float> (lambda ()
					   (let* ((a <float> 2))
					     5))))
			(ai))) -> #f
(rt-must-funcify? #t '(let* ((a <float> 2))
			(let* ((b <float> 3))
			  5)
			3)) -> #t
(rt-must-funcify? #t '(begin 3)) -> #f
(rt-must-funcify? #t '(begin 4 3)) -> #f
(rt-must-funcify? #t '(begin 4 (as))) -> #f
(rt-must-funcify? #t '(begin (begin 2))) -> #f
(rt-must-funcify? #t '(begin 4 (begin 2))) -> #t
(rt-must-funcify? #t '(begin (begin 2) 4)) -> #t

(rt-must-funcify? #t '(+ 2 3)) -> #f
(rt-must-funcify? #t '(+ (begin 2))) -> #t
(rt-must-funcify? #t '(+ (begin 2))) -> #t
!#


(define (rt-funcify-do term)
  ;;(c-display "funcify-do" term)
  (cond ((not (list? term)) term)
	((null? term) term)
	((and (eq? 'let* (car term))
	      (= (length term) 3)
	      (or (not (list? (caddr term)))
		  (= (length (caddr term)) 1)))
	 term)
	((and (eq? 'lambda (car term)))
	 `(lambda ,(cadr term)
	    ,(rt-funcify-do (if (and (eq? 'begin (car (caddr term)))
				     (= 1 (length (cddr term))))
				(caddr term)
				`(begin
				   ,@(cddr term))))))
	(else
	 (let* ((helpfuncs '())
		(newterm (map (lambda (t)
				;;(c-display "t" t)
				(cond ((null? t) t)
				      ((and (list? t)
					    (member (car t) rt-very-special-forms))
				       (set! helpfuncs (cons `(,(string->symbol (eval-c-get-unique-name)) <float> (lambda ()
														    ,(rt-funcify-do t)))
							     helpfuncs))
				       (list (caar helpfuncs)))
				      ((list? t)
				       (rt-funcify-do t))
				      (else
				       t)))
			      term)))
	   (if (not (null? helpfuncs))
	       (if (and (list? newterm)
			(eq? 'let* (car newterm)))
		   `(let* ,(cadr newterm)
		      (let* (,@(reverse! helpfuncs))
			,@(cddr newterm)))
		   `(let* (,@(reverse! helpfuncs))
		      ,newterm))
	       newterm)))))

(define (rt-funcify term)
  (call-with-current-continuation
   (lambda (return)
     (let ((tries 0)
	   (ret term))
       (while (rt-must-funcify? #t ret)
	      (if (> tries 40)
		  (begin
		    (c-display "rt-compiler.scm: rt-funcify. Had to give up trying to funcify expression" term ".")
		    (c-display "Yes, this is a bug in the compiler. Please send the code you tried to compile to k.s.matheussen@notam02.no. Thanks!")
		    (return #f)))
	      (set! tries (1+ tries))
	      (set! ret (rt-funcify-do ret)))
       ret))))

#!
(rt-funcify-do (rt-remove-unused++ '(let* ((a <float> (let* ((b <float> 5))
							a b c)))
				      a))))

(rt-funcify-do  '(lambda ((<float> a))
		   (let* ((u5 <float> (rt-if/?kolon a a 0)))
		     (rt-if/?kolon u5
				   u5
				   0))))
(rt-funcify-do  '(lambda ((<float> a))
		   (let* ((u5 <float> (if a
					  a
					  0)))
		     (if u5
			 u5
			 0))))
(rt-funcify '(lambda ()
	       (begin
		 (let* ((a <float> 6))
		   a)
		 5)))

(rt-funcify '(begin
	       (let* ((a <float> 6))
		 a)
	       5))

(rt-funcify (rt-remove-unused++ '(begin (begin 2 4) 3)))
(rt-funcify '(begin (begin 2) 3))
->
'(let ((<float> u23 (lambda () 2)))
   (u23) 3)

(rt-funcify '(+ (begin 2) 3))
->
'(let ((<float> u23 (lambda () 2)))
   (+ (u23) 3))


(rt-funcify '(begin (+ 3 (let* ((a <float> 2)) (+ a (begin 5) 4)))))
(rt-insert-returns (rt-funcify '(begin (+ 3 (let* ((a <float> 2)) (+ a (begin 5) 4))))))
(rt-funcify '(begin 2 3))

(define a (rt-2 '(lambda ()
	 (* 1 
	    (if (begin (+ 5 6) 2)
		(begin 8 3)
		4)
	    (begin
	      2
	      7))))
  )
(rt-funcall a)


(rt-funcall
 (rt-2 '(lambda ()
	  (let* ((a  (lambda ()
		       2
		       (+ (begin 2 5) 3))))
	    (+ 2 3 4)
	    (a))))
 )

(rt-3 '(lambda ()
	 2
	 3))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-replace-define-with-let*s replaces all defines with
;; let*s. (function copied from snd-hobbit.scm)
;;
;; rt-fix-various must have been called on the term before calling.
(define (rt-replace-define-with-let*s term)
  (cond 
   ((not (list? term)) term)
   ((null? term) term)
   (else
    (map rt-replace-define-with-let*s
	 (call-with-values (lambda () (break (lambda (t) (and (pair? t)
							      (eq? 'define (car t))))
					     term))
	   (lambda (beforedefines definestart)
	     (if (null? definestart)
		 term
		 (call-with-values (lambda () (span (lambda (t) (and (pair? t)
								     (eq? 'define (car t))))
						    definestart))
		   (lambda (defines afterdefines)
		     (append beforedefines
			     (list (append (list 'let*
						 (map-in-order (lambda (t) (list (cadr t) '<float> (caddr t)))
							       defines))
					   afterdefines))))))))))))

#!
(rt-replace-define-with-let*s '(begin
				 (+ 1 2)
				 (define a 3)
				 (- 4 5)
				 (define b (lambda ()
					     (define d 8)
					     d))
				 (define c 7)
				 (* a (b) c)
				 ))
(begin
  (+ 1 2)
  (let* ((a <float> 3))
    (- 4 5)
    (let* ((b <float> (lambda ()
			(let* ((d <float> 8))
			  d)))
	   (c <float> 7))
      (* a (b) c))))

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-uniqify-varnames makes all internal variable names unique
;; and transforms all lisp let/let*/letrecs into the eval-c version
;; of let*.

(define rt-get-unique-name
  (let ((n 0))
    (lambda (orgname)
      (set! n (1+ n))
      (symbol-append orgname '_u (string->symbol (number->string n))))))
				 
(define (rt-uniqify-varnames term)

  (define (uniqify varlist term)
    (cond ((null? term) term)
	  ((number? term) term)
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; A variable
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ((symbol? term)

	   (let ((var (assq term varlist)))
	     (if var
		 (cadr var)
		 term)))
	   
	  ((eq? 'lambda (car term))
	   (let* ((varnames (map (lambda (t)
				   (list (cadr t)
					 (rt-get-unique-name (cadr t))))
				 (cadr term)))
		  (lambdaargs (map (lambda (t name)
				     (list (car t)
					   (cadr name)))
				   (cadr term)
				   varnames)))
	     `(lambda ,lambdaargs
		,@(map (lambda (t)
			 (uniqify (append varnames varlist) t))
		       (cddr term)))))
	  
	  ((eq? 'let (car term))
	   (let* ((newvarlist varlist)
		  (vardecls (map-in-order (lambda (vardecl)
					    (let* ((uname (rt-get-unique-name (car vardecl)))
						   (ret `(,uname ,(cadr vardecl) ,(uniqify varlist (caddr vardecl)))))
					       (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
					       (c-display "2" newvarlist)
					       ret))
					  (cadr term))))
	     `(let* ,vardecls
		,@(map (lambda (t)
			 (c-display "1" newvarlist)
			 (uniqify newvarlist t))
		       (cddr term)))))
	  
	  ((eq? 'let* (car term))
	   (let* ((body (cddr term))
		  (vardecls (map-in-order (lambda (vardecl)
					    (let* ((uname (rt-get-unique-name (car vardecl)))
						   (ret `(,uname ,(cadr vardecl) ,(uniqify varlist (caddr vardecl)))))
					      (set! varlist (cons (list (car vardecl) uname) varlist))
					      ret))
					  (cadr term))))
	     `(let* ,vardecls
		,@(map (lambda (t)
			 (uniqify varlist t))
		       body))))
	  
	  ((eq? 'letrec (car term))
	   (let* ((body (cddr term))
		  (newvarlist (append (map (lambda (vardecl)
					     (list (car vardecl)
						   (rt-get-unique-name (car vardecl))))
					   (cadr term))
				      varlist))
		  (vardecls (map-in-order (lambda (vardecl var)
					    (list (cadr var) (cadr vardecl) (uniqify newvarlist (caddr vardecl))))
					  (cadr term)
					  newvarlist)))
	     `(let* ,vardecls
		,@(map (lambda (t)
			 (uniqify newvarlist t))
		       body))))
	  (else
	   (map (lambda (t)
		  (uniqify varlist t))
		term))))

  (uniqify '() term))


#!
(letrec ((a b)
	 (b 9))
  (+ a))

(rt-uniqify-varnames '(lambda ((<int> a) (<float> b))
			(+ a b c)))

(rt-uniqify-varnames '(lambda ((<int> a) (<float> b))
			(letrec ((c <float> 2)
			      (d <float> 4)
			      (e <float> e)
			      (f <float> d)
			      (g <float> h))
			  (+ e g h))))
(lambda ((<int> a_u6) (<float> b_u7))
  (let* ((c_u8 <float> 2)
	 (d_u9 <float> 4)
	 (e_u10 <float> e)
	 (f_u11 <float> d)
	 (g_u12 <float> h))
    (+ e g h)))

!#
					   
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-fix-various does various things.
;;
;; (let* ((a 5)) ...)       -> (let* ((a <float> 5)) ...)
;; (let* ((<int> a 5)) ...) -> (let* ((a <int> 5)) ...)
;; (define (a b c) d e)     -> (define a (lambda (b c) d e))
;; (lambda (a b)...)        -> (lambda ((<float> a)(<float> b))...)
;; + expand rt macros
;;
(define (rt-fix-various term)
  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (apply c-display args)
       (return #f))
     
     (define (fix term)
       (c-display "fix-term" term)
       (cond ((not (list? term)) term)
	     ((null? term) term)

	     ((eq? 'lambda (car term))
	      `(lambda ,(map (lambda (t)
			       (if (list? t)
				   t
				   (list '<float> t)))
			     (cadr term))
		 ,@(map fix (cddr term))))
	     
	     ;;(define (a b c) d e) -> (define a (lambda (b c) d e))
	     ((and (eq? 'define (car term))
		   (pair? (cadr term)))
	      (if (< (length term) 3)
		  (check-failed "rt-compiler.scm/rt-fix-various: Bad define block: " term ".")
		  `(define ,(caadr term) ,(fix `(lambda ,(cdadr term)
						  ,@(cddr term))))))
	     
	     ((eq? 'let* (car term))
	      (if (< (length term) 3)
		  (check-failed "rt-compiler.scm/rt-fix-various: Bad let*-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "rt-compiler.scm/rt-fix-various: First argument to let* must be a list of variables: " term ".")
		      (begin
			(for-each (lambda (var)
				    (cond ((not (list? var))
					   (check-failed "rt-compiler.scm/rt-fix-various: \"" var "\" is not a list in expression " term "."))
					  ((not (symbol? (car var)))
					   (check-failed "rt-compiler.scm/rt-fix-various: Illegal variable name: " (car var) " in expression " term "."))
					  ((and (= 3 (length var))
						(not (or (eq? '<int> (car var))
							 (eq? '<float> (car var))
							 (eq? '<int> (cadr var))
							 (eq? '<float> (cadr var)))))
					   (check-failed "rt-compiler.scm/rt-fix-various: Unsupported type: " (car var) " in expression " term "."))
					  ((= 1 (length var))
					   (check-failed "rt-compiler.scm/rt-fix-various: Variable \"" (car var) "\" in expression " term " does not have a value assigned."))))
				  (cadr term))
			
			`(let* ,(map (lambda (t)
				       (if (= (length t) 2)
					   (list (car t)
						 '<float>
						 (fix (cadr t)))
					   (if (or (eq? '<int> (car t))
						   (eq? '<float> (car t)))
					       (list (cadr t)
						     (car t)
						     (fix (last t)))
					       (list (car t)
						     (cadr t)
						     (fix (last t))))))
				     (cadr term))
			   ,@(map fix (cddr term)))))))
	     
	     (else
	      (if (not (symbol? (car term)))
		  (check-failed "rt-compiler.scm/rt-fix-various: Illegal function call:" term ".")
		  (let* ((args (cdr term))
			 ;;((args (map fix (cdr term)))
			 (a (cons (symbol-append rt-macro-prefix (car term)) args))
			 (b (macroexpand-1 a )))
		    (if (not b)
			(return #f)
			(if (not (equal? a b))
			    (fix b)
			    ;;(cons (car term) args))))))))
			    (map fix term))))))))
     (fix term))))
     


#!
(rt-fix-various '(lambda (x)
		   (if 1
		       (let* ((y (* x x)))
			 y)
		       0)))

(rt-fix-various '(- 2))
(rt-fix-various '(min 4 (max 5)))
(rt-fix-various '(and (+ 5 a)))
(rt-fix-various '(let* ((a 5)) (and (+ 5 a) (- 2 3) 4 5)))
(rt-fix-various '(begin (define (a b c) d e)))
(rt-fix-various '(begin (define (a (<int> b) c) d e)))
(rt-check-calls '(lambda ()
		   (let* ((a <int> 5))
		     (+ 2 3))))


(rt-3.5 '(lambda (a b c)
	   (or a (and a))))

(rt-3 '(lambda (a b c)
	 (and a b c
	     (or a b c)
	     (and a b c c c c b)
	     (or a (and b (or c a))
		 (and c)))))

(rt-3 '(lambda ()
	 (let* ((u5 (if a a 0)))
	   (if u5
	       u5
	       0))))

(rt-3 '(lambda (a b c)
	 (if a
	     (let* ((u254 2))
	       (if u254
		   u254
		   0))
	     0)))
(rt-3 '(lambda (a)
	 (begin
	   (+ a 2)
	   (- a 5))))

(macroexpand-1 '(rt-macro-and a (or a)))
(macroexpand-1 '(rt-macro-or a)))


(rt-3 '(lambda ()
	 (let* ((unique_name_72 (+ 5 a)))
	   (if unique_name_72
	       5))))


!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-check-calls does the following:
;; *checks that that the term is a legal expression.
;; *find guile number variables that is read
;; *find guile pointer variables that is read
;; *find guile number variables that is written to
;;
;; rt-replace-define-with-let*s must have been called on the term before calling.
;;
(define (rt-check-calls term)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display args)
       (return #f))

     (define (is-number? type)
       (or (eq? type '<float>)
	   (eq? type '<int>)
	   (eq? type '<double>)
	   (let ((type (eval-c-get-known-type type)))
	     (or (eq? type '<float>)
		 (eq? type '<int>)
		 (eq? type '<double>)))))

     (define external-vars '())

     (define (add-external-var varname type iswriting)
       (let ((old (assq varname external-vars)))
	 (if old
	     (let ((rt-type (cadr old)))
	       (if (not (eq? type (-> rt-type rt-type)))
		   (if (and (is-number? (-> rt-type rt-type))
			    (is-number? type))
		       (set-car! (cdr old) (hashq-ref rt-types '<float>))
		       (check-failed "rt-compiler.scm/rt-check-calls: Different types for guile variable \"" varname "\": " type "/" (-> rt-type rt-type) ".")))
	       (set-car! (cddr old) (or iswriting (caddr old))))
	     (let ((rt-type (hashq-ref rt-types type)))
	       (if (not rt-type)
		   (check-failed "rt-compiler.scm/rt-check-calls: Unknown type " type ".")
		   (set! external-vars (cons (list varname rt-type iswriting)
					     external-vars)))))))
   
     (define (get-returntype varlist term)
       (cond ((symbol? term) (let ((avar (assq term varlist)))
			       (c-display "jepp" avar)
			       (if avar
				   (cadr avar)
				   'symbol)))
	     ((number? term) 'number)
	     ((list? term) (if (member (car term) rt-very-special-forms)
			       'number
			       (let ((func (assq (car term) varlist)))
				 (if func
				     (cadr func)
				     (let ((func (hashq-ref rt-funcs (car term))))
				       (if func
					   (-> func return-type)
					   (check-failed "rt-compiler.scm/rt-check-calls(2): Unknown function \"" (car term) "\": " term)))))))
	     (else 'unknown-type)))
     
       
     (define (check-call varlist term)
       (c-display "check-call" term)
       (if (not (symbol? (car term)))
	   (check-failed "rt-compiler.scm/rt-check-calls: Illegal term: " term)
	   (let ((func (assq (car term) varlist)))
	     (if func
		 (begin
		   (if (or (= 2 (length func))
			   (not (list? (caddr func)))
			   (not (eq? 'lambda (car (caddr func)))))
		       (check-failed "rt-compiler.scm/rt-check-calls: Local variable \"" (car term) "\" is not a function:" term))
		   (if (not (= (length (cadr (caddr func)))
			       (- (length term) 1)))
		       (check-failed "rt-compiler.scm/rt-check-calls: Illegal number of argumentes to local function \"" (car term) "\":" term))
		   (for-each (lambda (aterm) (check-calls varlist aterm))
			     term)
		   (for-each (lambda (termarg)
			       (let ((ret-type (get-returntype varlist termarg)))
				 (if (eq? 'symbol ret-type)
				     (add-external-var termarg '<float> #f)
				     (if (and (not (eq? 'number ret-type))
					      (not (is-number? ret-type)))
					 (check-failed "rt-compiler.scm/rt-check-calls: Illegal argument \"" termarg "\" to local function \"" (car term) "\"."
						       "expected a number, found" ret-type ", in expression:" term)))))
			     (cdr term)))
		 (begin
		   (set! func (hashq-ref rt-funcs (car term)))
		   (if (not func)
		       (check-failed "rt-compiler.scm/rt-check-calls: Unknown function \"" (car term) "\": " term)
		       (if (not (-> func legal-number-of-arguments? term))
			   (return #f)
			   (begin
			     (for-each (lambda (aterm) (check-calls varlist aterm))
				       term)
			     (for-each (lambda (termarg argtype)
					 (c-display "termarg/argtype" termarg argtype)
					 (let ((ret-type (get-returntype varlist termarg)))
					   (c-display "ret-type:" ret-type)
					   (cond ((eq? 'symbol ret-type)
						  (add-external-var termarg argtype #f))
						 ((or (is-number? ret-type)
						      (eq? 'number ret-type) (if (not (is-number? argtype))
										 (check-failed "rt-compiler.scm/rt-check-calls: Wrong type in: " term ". "
											       termarg " is not a " argtype "."))))
						 (else
						  (if (not (eq? ret-type argtype))
						      (check-failed "rt-compiler.scm/rt-check-calls: Wrong type in: " term ". "
								    (car termarg) " Does not return " argtype "."))))))
				       (cdr term)
				       (list-tabulate (length (cdr term))
						      (lambda (i)
							(-> func arg-type i))))))))))))
     
     
     
     (define (check-calls varlist term)
       (c-display "check-calls varlist/term" varlist term)
       (cond ((not (list? term)) #t)
	     ((null? term) #t)

	     ;; LAMBDA
	     ((eq? 'lambda (car term))
	      (if (< (length term) 3)
		  (check-failed "rt-compiler.scm/rt-check-calls: Bad lambda-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "rt-compiler.scm/rt-check-calls: Second argument for lambda is not a list: " term ".")	       
		      (if (not (= (length (cadr term))
				  (length (delete-duplicates (cadr term)))))
			  (check-failed "rt-compiler.scm/rt-check-calls: Same argument name for lambda function used more than once: " term ".")
			  (begin
			    (for-each (lambda (varname)
					(if (not (symbol? (cadr varname)))
					    (check-failed "rt-compiler.scm/rt-check-calls: Illegal variable \"" (cadr varname) "\" in lambda-form: " term ".")))
				      (cadr term))
			    (for-each (lambda (aterm)
					(c-display "cadrterm" (cadr term))
					(check-calls (append (map (lambda (s)
								    (list (cadr s) (car s)))
								  (cadr term))
							     varlist)
						     aterm))
				      (cddr term)))))))

	     ;; SET!
	     ((eq? 'set! (car term))
	      (if (not (symbol? (cadr term)))
		  (check-failed "rt-compiler.scm/rt-check-calls(2): Illegal set! term: " term)
		  (if (not (= 3 (length term)))
		      (check-failed "rt-compiler.scm/rt-check-calls(3): Illegal set! term: " term)
		      (let ((avar (assq (cadr term) varlist)))
			(if (not avar)
			    (add-external-var (cadr term) '<float> #t))
			(check-calls varlist (caddr term))))))
	     
	     ;; BEGIN
	     ((eq? 'begin (car term))
	      (if (null? (cdr term))
		  (check-failed "rt-compiler.scm/rt-check-calls: begin needs a body: " term ".")
		  (for-each (lambda (aterm) (check-calls varlist aterm))
			    (cdr term))))

	     ;; LET*
	     ((eq? 'let* (car term))
	      (let ((newvarlist varlist))
		(for-each (lambda (var)
			    (set! newvarlist (cons var
						   newvarlist))
			    (if (symbol? (caddr var))
				(let ((avar (assq term varlist)))
				  (if (not avar)
				      (add-external-var (caddr var) (cadr var) #f)))
				(check-calls newvarlist (caddr var))))
			  (cadr term))
		(for-each (lambda (aterm) (check-calls newvarlist aterm))
			  (cddr term))))
	     
	     ;; IF (not used anymore)
	     ((eq? 'if (car term))
	      (if (< (length term) 3)
		  (check-failed "rt-compiler.scm/rt-check-calls: To few arguments for if:" term ".")
		  (if (> (length term) 4)
		      (check-failed "rt-compiler.scm/rt-check-calls: To many arguments for if:" term ".")))
	      (check-calls varlist (cadr term))
	      (check-calls varlist (caddr term))
	      (if (= (length term) 4)
		  (check-calls varlist (cadddr term))))

	     (else
	      (check-call varlist term))))
     
     (if (not (eq? 'lambda (car term)))
	 (check-failed "rt-compiler.scm/rt-check-calls: This is no a lambda function: " term))
     
     (check-calls '() term)

     ;;(c-display "external-vars:" external-vars)

     (let ((extnumbers '())
	   (extpointers '())
	   (extnumbers-writing '()))
       (for-each (lambda (extvar)
		   (if (caddr extvar)
		       (set! extnumbers-writing (cons extvar extnumbers-writing))
		       (if (is-number? (-> (cadr extvar) rt-type))
			   (set! extnumbers (cons extvar extnumbers))
			   (set! extpointers (cons extvar extpointers)))))
		 external-vars)
       (list extnumbers extpointers extnumbers-writing)))))


#!
((<float> c (lambda ((<float> d)) (+ a b)))
 (<float> b 5)
 (<float> a))

(rt-check-calls '(lambda ()
		   (rt-if/?kolon a
				 a
				 b)))

(+ 2 3)

(rt-check-calls '(lambda ()
		   (set! osc 9)
		   (rt-oscil/mus_oscil_0 osc)))

(map car (caddr 
	  (rt-check-calls '(lambda ((<float> a))
			     (let* ((b <float> 5)
				    (c <float> (lambda ((<float> d))
						 (+ 2 a b))))

			       (c (c d))
			       (rt-oscil/mus_oscil_0 anosc)
			       (set! anosc 5)
			       (set! extw 9)
			       (+ ext1 ext2))))
	  ))

!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-types (make-hash-table 256))

(def-class (<rt-type> rt-type checkfunc #:key transformfunc error-message (c-type rt-type))
  (def-method (rt-type)
    rt-type)
  (def-method (c-type)
    c-type)
  (def-method (check type)
    (if checkfunc
	(checkfunc type)
	(eq? type rt-name)))

  (def-method (transform var)
    (if (not (this->check var))
	(begin
	  (c-display "rt-compiler/<rt-type>. Wrong type. \"" var "\" is not a" rt-type ".")
	  (throw 'wrong-type))
	(if transformfunc
	    (transformfunc var)
	    var)))

  (hashq-set! rt-types rt-type this)

  )


(begin
  (<rt-type> '<float> number?)
  (<rt-type> '<int> number?)
  (<rt-type> '<char-*> string?)
  (<rt-type> '<mus_oscil-*> oscil? #:c-type '<mus_any-*> #:transformfunc XEN_TO_MUS_ANY))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rt-funcs (make-hash-table 256))


(def-class (<rt-func> name returntype args #:key (min-arguments #f) (max-arguments #f))
  
  (def-method (legal-number-of-arguments? term)
    (if (not (and (or (not max-arguments)
		      (<= (- (length term) 1)
			  max-arguments))))
	(begin
	  (c-display "rt-compiler.scm/rt-check-calls: Wrong number of arguments in \"" term "\". Expected maximum"
		     max-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	  #f)
	(if (not (or (not min-arguments)
		     (>= (- (length term) 1)
			 min-arguments)))
	    (begin
	      (c-display "rt-compiler.scm/rt-check-calls: Wrong number of arguments in \"" term "\". Expected minimum"
			 min-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	      #f)
	    #t)))
  
  (def-method (return-type)
    returntype)
  
  (def-method (arg-type argnum)
    (if (list? args)
	(list-ref args argnum)
	args))

  (if (not min-arguments)
      (if (list? args)
	  (set! min-arguments (length args))))

  (if (not max-arguments)
      (if (list? args)
	  (set! max-arguments (length args))))

  (hashq-set! rt-funcs name this)

  )


(begin

  ;; Functions defined directly. (Note that most functions are defined indirectly using the rt-renamefunc macro.)
  
  ;; Basic
  (<rt-func> '+ '<float> '<float> #:min-arguments 2)
  (<rt-func> 'rt--/- '<float> '<float> #:min-arguments 2)
  (<rt-func> 'rt--/minusoneargument '<float> '(<float>))
  (<rt-func> '* '<float> '<float> #:min-arguments 2)
  (<rt-func> '/ '<float> '<float> #:min-arguments 2)
  
  (<rt-func> '1+ '<float> '(<float>))
  (<rt-func> '1- '<float> '(<float>))

  (<rt-func> 'rt-min/MIN '<float> '(<float> <float>))
  (<rt-func> 'rt-max/MAX '<float> '(<float> <float>))

  (<rt-func> 'set! '<void> '(<float> <float>))

  (<rt-func> 'rt-outs/outs '<float> '(<int>))
  (<rt-func> 'rt-set-outs! '<void> '(<int> <float>))
  (<rt-func> 'rt-num_outs/num_outs '<int> '())
  
   ;; Testing
  (<rt-func> '< '<int> '(<float> <float>))
  (<rt-func> '> '<int> '(<float> <float>))
  (<rt-func> '<= '<int> '(<float> <float>))
  (<rt-func> '>= '<int> '(<float> <float>))
  
  (<rt-func> 'rt-=/== '<int> '(<float> <float>))
  
  (<rt-func> 'not '<int> '(<float>))
  
  
  ;; Float operations
  (for-each (lambda (func)
	      (primitive-eval `(<rt-func> ',func '<float> '(<float>))))
	    `(sin cos tan acos asin atan exp log log10 sqrt))

  (<rt-func> 'atan2 '<float> '(<float> <float>))

  ;; Bitwise operations
  (<rt-func> 'rt-ash/<< '<int> '(<int> <int>))
  (<rt-func> 'rt-ash/>> '<int> '(<int> <int>))

  ;; Various
  (<rt-func> 'rt-if/?kolon '<float> '(<int> <float> <float>))
  (<rt-func> 'rt-begin_p/begin_p '<float> '<float> #:min-arguments 1)

  ;; Common Lisp Music
  (<rt-func> 'rt-oscil/mus_oscil '<float> '(<mus_oscil-*> <float> <float>))
  (<rt-func> 'rt-oscil/mus_oscil_0 '<float> '(<mus_oscil-*>))
  (<rt-func> 'rt-oscil/mus_oscil_1 '<float> '(<mus_oscil-*> <float>))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-rt-macro def . body)
  `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def)) ,@body))

(define-macro (rt-renamefunc rt-name c-name returntype args)
  (let ((funcname (symbol-append 'rt- rt-name '/ c-name)))
    (<rt-func> funcname returntype args)
    (primitive-eval `(define-c-macro ,(cons funcname 'rest )
		       `(,',c-name ,@rest)))
    `(define-rt-macro ,(cons rt-name 'rest)
       `(,',funcname ,@rest))))


(define-rt-macro (- firstarg . rest)
  (if (null? rest)
      `(rt--/minusoneargument ,firstarg)
      `(rt--/- ,firstarg ,@rest)))
(define-c-macro (rt--/minusoneargument a)
  `(- ,a))
(define-c-macro (rt--/- . rest)
  `(- ,@rest))


 
(rt-renamefunc = == <int> (<float> <float>))
(rt-renamefunc expt pow <float> (<float> <float>))
(rt-renamefunc abs fabsf <float> (<float>))

(rt-renamefunc floor floorf <float> (<float>))
(rt-renamefunc ceiling ceilf <float> (<float>))

(rt-renamefunc logand & <int> (<int> <int>))
(rt-renamefunc logior | <int> (<int> <int>))
(rt-renamefunc lognot ~ <int> (<int>))
  
(rt-renamefunc rt-remainder % <int> (<int> <int>))


(define-rt-macro (zero? z)
  `(== 0 ,z))
(define-rt-macro (positive? z)
  `(> ,z 0))
(define-rt-macro (negative? z)
  `(< ,z 0))


;; modulo-logic picked up from snd-run.c
(define-rt-macro (modulo a b)
  (let ((x (string->symbol (eval-c-get-unique-name)))
	(y (string->symbol (eval-c-get-unique-name)))
	(z (string->symbol (eval-c-get-unique-name))))
    `(let* ((,x ,a)
	    (,y ,b)
	    (,z (remainder ,x ,y)))
       (if (or (and (negative? ,y)
		    (positive? ,z))
	       (and (positive? ,y)
		    (negative? ,z)))
	   (+ ,z ,y)
	   ,z))))

(define-rt-macro (quotient a b)
  `(/ ,a ,b))

(define-rt-macro (odd? n)
  `(remainder ,n 2))
(define-rt-macro (even? n)
  `(not (odd? ,n)))


;; truncate-logic picked up from snd-run.c
(define-rt-macro (truncate a)
  (if (number-or-symbol? a)
      `(if (negative? ,a)
	   (- (floor (- ,a)))
	   (floor ,a))
      (let ((x (string->symbol (eval-c-get-unique-name)))  )
	`(let* ((,x ,a))
	   (if (negative? ,x)
	       (- (floor (- ,x)))
	       (floor ,x))))))

;; round-logic picked up from snd-run.c
(define-rt-macro (round a)
  (let ((plus_half (string->symbol (eval-c-get-unique-name)))
	(result (string->symbol (eval-c-get-unique-name))))
    `(let* ((,plus_half (+ ,a 0.5))
	    (,result (floor ,plus_half)))
       (if (and (= ,plus_half ,result)
		(not (= (/ ,plus_half 2) (floor (/ ,plus_half 2)))))
	   (- ,result 1)
	   ,result))))
       

;;logxor-logic picked up from snd-run.c
(define-rt-macro (logxor x y)
  (if (and (number? x)
	   (number? y))
      (logxor x y)
      (if (number-or-symbol? x y)
	  `(logand (lognot (logand ,x ,y))
		   (logior ,x ,y))
	  (let ((a (string->symbol (eval-c-get-unique-name)))
		(b (string->symbol (eval-c-get-unique-name))))
	    `(let* ((,a ,x)
		    (,b ,y))
	       (logand (lognot (logand ,a ,b))
		       (logior ,a ,b)))))))


;;ash-logic picked up from snd-run.c
(define-rt-macro (ash a b)
  (if (and (number? a)
	   (number? b))
      (ash a b)
      (if (number-or-symbol? a b)
	  `(if (>= ,b 0)
	       (rt-ash/<< ,a ,b)
	       (rt-ash/>> ,a (- ,b)))	
	  (let ((arg1 (string->symbol (eval-c-get-unique-name)))
		(arg2 (string->symbol (eval-c-get-unique-name))))
	    `(let* ((,arg1 ,a)
		    (,arg2 ,b))
	       (if (>= ,arg2 0)
		   (rt-ash/<< ,arg1 ,arg2)
		   (rt-ash/>> ,arg1 (- ,arg2))))))))
(define-c-macro (rt-ash/<< a b)
  `(<< ,a ,b))
(define-c-macro (rt-ash/>> a b)
  `(>> ,a ,b))

(define-rt-macro (random a)
  `(mus_frandom ,a))


(define-rt-macro (max . rest)
  (define (expand rest)
    (if (= 1 (length rest))
	(car rest)
	`(rt-max/MAX ,(expand (cdr rest)) ,(car rest))))  
  (if (null? rest)
      (begin
	(c-display "Error. \"max\" expect at least one argument: (max).")
	#f)
      (if (= 1 (length rest))
	  (car rest)
	  (let ((number-args '())
		(rest-args '())
		(new-args '())
		(varnames '()))
	    (for-each (lambda (arg)
			(if (number-or-symbol? arg)
			    (begin
			      (set! number-args (cons arg number-args ))
			      (set! new-args (cons arg new-args)))
			    (begin
			      (set! rest-args (cons arg rest-args))
			      (set! varnames (cons (string->symbol (eval-c-get-unique-name)) varnames))
			      (set! new-args (cons (car varnames) new-args)))))
		      rest)
	    (set! number-args (reverse! number-args))
	    (set! rest-args (reverse! rest-args))
	    (set! new-args (reverse! new-args))
	    (set! varnames (reverse! varnames))
	    (if (null? rest-args)
		(expand number-args)
		`(let* ,(map list
			     varnames
			     rest-args)
		   ,(expand new-args)))))))
(define-c-macro (rt-max/MAX a b)
  `(MAX ,a ,b))

(define-rt-macro (min . rest)
  (define (expand rest)
    (if (= 1 (length rest))
	(car rest)
	`(rt-min/MIN ,(expand (cdr rest)) ,(car rest))))  
  (if (null? rest)
      (begin
	(c-display "Error. \"min\" expect at least one argument: (min)")
	#f)
      (if (= 1 (length rest))
	  (car rest)
	  (let ((number-args '())
		(rest-args '())
		(new-args '())
		(varnames '()))
	    (for-each (lambda (arg)
			(if (number-or-symbol? arg)
			    (begin
			      (set! number-args (cons arg number-args ))
			      (set! new-args (cons arg new-args)))
			    (begin
			      (set! rest-args (cons arg rest-args))
			      (set! varnames (cons (string->symbol (eval-c-get-unique-name)) varnames))
			      (set! new-args (cons (car varnames) new-args)))))
		      rest)
	    (set! number-args (reverse! number-args))
	    (set! rest-args (reverse! rest-args))
	    (set! new-args (reverse! new-args))
	    (set! varnames (reverse! varnames))	    
	    (if (null? rest-args)
		(expand number-args)
		`(let* ,(map list
			     varnames
			     rest-args)
		   ,(expand new-args)))))))
(define-c-macro (rt-min/MIN a b)
  `(MIN ,a ,b))

  
(define-rt-macro (and . rest)
  (define (expand ret rest)
    (if (null? rest)
	ret
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  0
		  (expand var (cdr rest)))
	      (if (symbol? var)
		  `(if ,var
		       ,(expand var (cdr rest))
		       0)
		  (let ((varname (string->symbol (eval-c-get-unique-name))))
		    `(let* ((,varname ,var))
		       (if ,varname
			   ,(expand varname (cdr rest))
			   0))))))))
  (expand 1 rest))


(define-rt-macro (or . rest)
  (define (expand rest)
    (if (null? rest)
	0
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  (expand (cdr rest))
		  var)
	      (if (symbol? var)
		  `(if ,var
		       ,var
		       ,(expand (cdr rest)))
		  (let ((varname (string->symbol (eval-c-get-unique-name))))
		    `(let* ((,varname ,var))
		       (if ,varname
			   ,varname
			   ,(expand (cdr rest))))))))))
  (expand rest))


(define-rt-macro (if a b . c)
  (if (null? c)
      `(rt-if/?kolon ,a ,b 0)
      `(rt-if/?kolon ,a ,b ,(car c))))
(define-c-macro (rt-if/?kolon . rest)
  `(?kolon ,@rest))

(define-rt-macro (begin_p . rest)
  `(rt-begin_p/begin_p ,@rest))
(define-c-macro (rt-begin/begin_p . rest)
  `(begin_p ,@rest))

(define-rt-macro (out val)
  (if (number-or-symbol? val)
      `(begin
	 (if (> (rt-num_outs/num_outs) 0)
	     (rt-set-outs! 0 (+ (rt-outs/outs 0) ,val)))
	 (if (> (rt-num_outs/num_outs) 1)
	     (rt-set-outs! 1 (+ (rt-outs/outs 1) ,val))))
      (let ((varname (string->symbol (eval-c-get-unique-name))))
	`(let* ((,varname ,val))
	   (if (> (rt-num_outs/num_outs) 0)
	       (rt-set-outs! 0 (+ (rt-outs/outs 0) ,varname)))
	   (if (> (rt-num_outs/num_outs) 1)
	       (rt-set-outs! 1 (+ (rt-outs/outs 1) ,varname)))))))

(define-c-macro (rt-outs/outs n)
  (<-> "_rt_funcarg->outs[" (eval-c-parse n) "]"))
(define-c-macro (rt-set-outs! n val)
  (<-> "_rt_funcarg->outs[" (eval-c-parse n) "]=" (eval-c-parse val)))
(define-c-macro (rt-num_outs/num_outs)
  "_rt_funcarg->num_outs")

(define-rt-macro (oscil osc . rest)
  (if (null? rest)
      `(rt-oscil/mus_oscil_0 ,osc)
      (if (null? (cdr rest))
	  `(rt-oscil/mus_oscil_1 ,osc ,(car rest))
	  `(rt-oscil/mus_oscil ,osc ,(car rest) ,(cadr rest)))))
	  
  
(define-c-macro (rt-oscil/mus_oscil osc fm pm)
  `(mus_oscil ,osc ,fm ,pm))
(define-c-macro (rt-oscil/mus_oscil_0 osc)
  `(mus_oscil_0 ,osc))
(define-c-macro (rt-oscil/mus_oscil_1 osc fm)
  `(mus_oscil_1 ,osc ,fm))


#!
thinking loud
(define-rt-macro (oscil2 osc . rest)
  (let ((fm 0)
	(pm 0))
    (if (not (null? rest))
	(set! fm (car rest))
	(if (not (null? (cdr rest)))
	    (set! pm (cadr rest))))
    `(let* ((res (sin (+ (-> ,osc phase) ,pm))))
       (set! (-> ,osc phase) (+ (-> ,osc phase)
				(-> ,osc freq)
				,fm))
       res)))
!#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; rt/rt-run/rt-funcall/etc. ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rt-4 term)
  (for-each (lambda (func)
	      (if term
		  (set! term (func term))))
	    (list
	     rt-fix-various
	     rt-replace-define-with-let*s))
  (if (not term)
      #f
      (let ((external-vars (rt-check-calls term))
	    (orgargs (cadr term)))
	(if (not external-vars)
	    #f
	    (let ((extnumbers (car external-vars))
		  (extpointers (cadr external-vars))
		  (extnumbers-writing (caddr external-vars)))
	      (set-car! (cdr term)
			(append (map (lambda (var)
				       (list (-> (cadr var) c-type)
					     (car var)))
				     (append extnumbers-writing extpointers extnumbers))
				orgargs))
	      (for-each (lambda (func)
			  (if term
			      (set! term (func term))))
			(list
			 rt-remove-unused++	     
			 rt-funcify
			 rt-insert-returns))
	      (list extnumbers
		    extpointers
		    extnumbers-writing
		    orgargs
		    term))))))

(define (rt-3.5 term)
  (let ((t (rt-4 term)))
    (if t
	(cadr (cdddr t)))))

(define (rt-3 term)
  (let ((rt-4-result (rt-4 term)))
    (if (not term)
	#f
	(let* ((extnumbers (car rt-4-result))
	       (extpointers (cadr rt-4-result))
	       (extnumbers-writing (caddr rt-4-result))
	       (orgargs (cadddr rt-4-result))
	       (term (cadr (cdddr rt-4-result)))
	       
	       (funcname (string->symbol (eval-c-get-unique-name)))
	       (das-funcname (string->symbol (eval-c-get-unique-name)))
	       (rt-innerfuncname (string->symbol (eval-c-get-unique-name)))
	       (rt-funcname (string->symbol (eval-c-get-unique-name)))
	       
	       (funcarg (string->symbol (eval-c-get-unique-name)))
	       (publicargs (append (map (lambda (extvar)
					  `(<SCM> ,(symbol-append '_rt_scm_ (car extvar))))
					extnumbers-writing)
				   (map (lambda (extvar)
					  (list (-> (cadr extvar) c-type) (car extvar)))
					(append extpointers extnumbers))
				   orgargs)))
	  (c-display "term" term)
	  (newline)
	  
	  (list funcname
		rt-funcname
		extnumbers-writing
		extpointers
		extnumbers
		
		`( (define-struct <func_args>
		     <int> num_outs
		     <float-*> outs
		     <int> num_ins
		     <float-*> ins
		     <float> time
		     <float> res
		     <char-*> error
		     <SCM> errorvariable
		     <int> errorvarnum)
		   
		   
		   (<void> ,das-funcname (lambda ,(cons `(<struct-func_args-*> _rt_funcarg) publicargs)
					   
					   ,@(map (lambda (extvar)
						    `(<float> ,(car extvar)))
						  extnumbers-writing)
					   
					   (<float> ,rt-innerfuncname (lambda ()
									,@(cddr term)))
					   
					   ,@(let ((n -1))
					       (map-in-order (lambda (extvar)
							       (let ((name (symbol-append '_rt_scm_ (car extvar))))
								 (set! n (1+ n))
								 `(if (|| (SCM_INUMP ,name)
									  (SCM_BIGP ,name)
									  (! (SCM_REALP ,name)))
								      (begin
									(set! _rt_funcarg->errorvariable ,name)
									(set! _rt_funcarg->errorvarnum ,n)
									(set! _rt_funcarg->error (string ,(string-append "\\\""
															 (symbol->string (car extvar))
															 "\\\" is not a real float")))
									return))))
							     extnumbers-writing))
					   
					   ,@(map (lambda (extvar)
						    (let ((name (symbol-append '_rt_scm_ (car extvar))))
						      `(set! ,(car extvar) (SCM_REAL_VALUE ,name))))
						  extnumbers-writing)
					   
					   (set! _rt_funcarg->res (,rt-innerfuncname))
					   
					   ,@(map (lambda (extvar)
						    (let ((name (symbol-append '_rt_scm_ (car extvar))))
						      `(set! (SCM_REAL_VALUE ,name) ,(car extvar))))
						  extnumbers-writing)))
		   
		   (functions->public
		    (<void> ,rt-funcname (lambda ((<SCM> vector)
						   (<int> num_outs) (<float> *outs)
						   (<int> num_ins) (<float> *ins)
						   (<float> time))
					    ,(if (null? orgargs)
						 `(begin
						    (<struct-func_args> funcarg (struct-set num_outs outs num_ins ins time 0 NULL 0 0))
						    ,(if (not (null? extnumbers-writing))
							 '(<SCM> setfloats (SCM_VECTOR_REF vector 0))
							 "/* */")
						    ,(if (not (null? extpointers))
							 '(<SCM> pointers (SCM_VECTOR_REF vector 1))
							 "/* */")
						    ,(if (not (null? extnumbers))
							 '(<SCM> readfloats (SCM_VECTOR_REF vector 2))
							 "/* */")
						    (,das-funcname &funcarg
								   ,@(map (lambda (n) `(SCM_VECTOR_REF setfloats ,n)) (iota (length extnumbers-writing)))
								   ,@(map (lambda (n) `(GET_POINTER(SCM_VECTOR_REF pointers ,n))) (iota (length extpointers)))
								   ,@(map (lambda (n) `(SCM_REAL_VALUE(SCM_VECTOR_REF readfloats ,n))) (iota (length extnumbers)))))
						 'return))))
		   
		   (public			      
		    (<float> ,funcname (lambda ,publicargs
					 (<struct-func_args> ,funcarg "{0}")
					 (,das-funcname ,(string-append "&" (symbol->string funcarg)) ,@(map cadr publicargs))
					 (SCM_ASSERT (== NULL ,(symbol-append funcarg '.error))
						     ,(symbol-append funcarg '.errorvariable)
						     ,(symbol-append funcarg '.errorvarnum)
						     ,(symbol-append funcarg '.error))
					 (return ,(symbol-append funcarg '.res)))))))))))



#!
(define a (rt-2 '(lambda (a)
		   (out 5)
		   (set! c 2)
		   (+ b 3))))
(begin a)

(define c 6)
(define b 9)
(let ((d 5.2))
  (c-display "res" (rt-funcall a 2))
  (c-display "c" c))
(begin c)

(rt (lambda ()
      (sin 50)))
(rt-3 '(lambda (x)
	 (if 1
	     c
	     (let* ((y (* x x)))
	       y)
	     0)))
		 
(eval-c ""
	(public
	 (<float> ai (lambda ()
		       (begin
			 (let* ((unique_name_191 <float> (lambda ()
							   (let* ((<float> y (* x x)))
							     (return y)))))
			   (return (rt-if/?kolon 1
						 (unique_name_191)
						 0))))))))

(define (tak x y z)
  (let* ((tak (lambda (x y z)
		(if (not (< y x))
		    z
		    (tak (tak (- x 1) y z)
			 (tak (- y 1) z x)
			 (tak (- z 1) x y))))))
    (tak x y z)))

(define-rt (tak-rt x y z)
  (let* ((tak (lambda (x y z)
		(if (not (< y x))
		    z
		    (tak (tak (- x 1) y z)
			 (tak (- y 1) z x)
			 (tak (- z 1) x y))))))
    (tak x y z)))


(tak-rt 30 13 6)
(tak 30 13 6)


(define (hanoi n)
  (letrec ((move-them 
	    (lambda (n from to helper)
	      (if (> n 1)
		  (begin
		    (move-them (- n 1) from helper to)
		    (move-them (- n 1) helper to from))))))
    (move-them n 0 1 2)))
(define-rt (hanoi-rt n)
  (let* ((move-them 
	  (lambda (n from to helper)
	    (if (> n 1)
		(begin
		  (move-them (- n 1) from helper to)
		  (move-them (- n 1) helper to from))))))
    (move-them n 0 1 2)))

(hanoi 19)
(hanoi-rt 19)

!#

(define (rt-2 term)
  (let ((rt-3-result (rt-3 term))
	(orgargs (cadr term)))
    (if (not rt-3-result)
	(throw 'compilation-failed)
	(let* ((funcname (car rt-3-result))
	       (rt-funcname (cadr rt-3-result))
	       (extnumbers-writing (caddr rt-3-result))
	       (extpointers (cadddr rt-3-result))
	       (extnumbers (cadr  (cdddr rt-3-result)))
	       (term       (caddr (cdddr rt-3-result)))
	       (callmacro (procedure->macro (lambda (x env)
					      (if (null? extnumbers-writing)
						  `(,funcname ,@(map (lambda (extvar)
								       (let ((name (car extvar))
									     (type (cadr extvar)))
									 `(-> ,type transform ,name)))
								     (append extnumbers-writing extpointers extnumbers))
							      ,@(cdr x))
						  `(begin
						     ,@(map (lambda (extvar)
							      `(if (number? ,(car extvar))
								   (set! ,(car extvar) (exact->inexact ,(car extvar)))))
							    extnumbers-writing)
						     (,funcname ,@(map (lambda (extvar)
									 (let ((name (car extvar))
									       (type (cadr extvar)))
									   `(-> ,type transform ,name)))
								       (append extnumbers-writing extpointers extnumbers))
								,@(cdr x)))))))
	       (rt-callmacro (procedure->macro (lambda (x env)
						 `(begin
						    ,@(map (lambda (extvar)
							     `(if (number? ,(car extvar))
								  (set! ,(car extvar) (exact->inexact ,(car extvar)))))
							   (append extnumbers extnumbers-writing))
						    (let ((ret (<realtime> (,rt-funcname)
									   ;; Note, next vector is gc-marked manually in the funcall smob.
									   (vector (vector ,@(map (lambda (extvar)
												    (let ((name (car extvar))
													  (type (cadr extvar)))
													`(-> ,type transform ,name)))
												  extnumbers-writing))
										   (vector ,@(map (lambda (extvar)
												      (let ((name (car extvar))
													    (type (cadr extvar)))
													`(-> ,type transform ,name)))
												  extpointers))
										   (vector ,@(map (lambda (extvar)
												    (let ((name (car extvar))
													  (type (cadr extvar)))
												      `(-> ,type transform ,name)))
												  extnumbers))
										   ;; Keep untransformed values here so they can be gc-marked. (dont remove!)
										   (list ,@(map car (append extnumbers-writing extpointers extnumbers)))))))
						      ,@(map (lambda (numvar)
							       `(-> ret add-method ',numvar (make-procedure-with-setter
											     (lambda ()
											       ,numvar)
											     (lambda (newval)
											       (rt-set-float ,numvar newval)))))
							     (map car (append extnumbers-writing extnumbers)))
						      ret))))))
						    
	  
	  (apply eval-c-non-macro (append (list (string-append "-I" snd-header-files-path)
						"#include <math.h>"
						"#include <clm.h>")
					  term))
	  
	  (list 'rt-func
		callmacro
		rt-callmacro
		(primitive-eval funcname)
		(primitive-eval rt-funcname))))))



;; Yepp, has to redefine set!
(define rt-old-set! set!)
(define-macro (set! var val)
  (if (and (list? var)
	   (eq? '-> (car var)))
      `((setter (<- ,@(cdr var))) ,val)
      `(,rt-old-set! ,var ,val)))


(define-macro (rt term)
  `(rt-2 ',term))

(define-macro (rt-func term)
  (let ((das-rt (rt-2 term)))
    (if (not das-rt)
	#f
	`(lambda ,(cadr term)
	   (,(cadr das-rt) ,@(cadr term))))))

(define-macro (define-rt def . body)
  `(define ,(car def) (rt-func (lambda ,(cdr def)
				 ,@body))))
  
(define-macro (rt-funcall rt-func . args)
  `((cadr ,rt-func) ,@args))

(define-macro (rt-rt rt-func)
  `((caddr ,rt-func)))

  
#!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Drodle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define readfloat 2)
(define setfloat 3)
(define osc (make-oscil :frequency 400))
(define osc2 (make-oscil :frequency 10))

(define a (rt-2 '(lambda ()
		   ;;(oscil osc2)
		   (set! setfloat (oscil osc))
		   (out (* 0.2 (oscil osc) (oscil osc2))))))

(define b (rt-rt a))

(begin b)

(begin (cadr b))

(caddr a)

(-> b dir)
(-> b setfloat)
(set! (-> b setfloat) 9)
(set! setfloat 10)

(define instrument b)
(-> rt-engine start)
(-> instrument play)
(-> instrument stop)
(c-display setfloat)

(-> instrument setfloat)
    
(set! (mus-frequency osc2) 30)

(begin
  (def-class (gakk b)
    (def-var c b)
    (c-display 5 b)))
  

(gakk 6))

!#
