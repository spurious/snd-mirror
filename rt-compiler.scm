
#!

rt-compiler.scm
-Kjetil S. Matheussen/Notam, 2005

rt-compiler.scm is developed with support from Notam/Oslo:
http://www.notam02.no


Oops! API might still change. This file is currently under
heavy development.

Make sure Jack is running before loading this file!



***************************************************************
Introduction
************
rt-compiler provides various functions and macros to compile]1]
and run simple lisp[2] functions.

The original purpose of the langauge was to generate code that
can be hard realtime safe, (and can run safely inside the jack realtime
thread), but the compiler can also be used for general number
crunching. The generated code should be extremely efficient.

As far as possible I have tried to make the language behave and
look like scheme. However, there are no consing or other operations
that can trigger a garbage collection, so its not really a very schemish
language, although it visually looks a lot like scheme. Perhaps consing
and more advanced stuff will be implemented later, but its not really
necesarry as the code blends very fine into Guile. If you need
to do consing, you do that inside Guile.
Actually, technically, the language is more like C than Scheme.



***************************************************************
Two short examples
******************

1.
--

(let ((osc (make-oscil)))
  (-> rt-engine start)
  (rt-run 0 3
	  (lambda ()
	    (out (* 0.8
		    (oscil osc))))))
=> #:undefined
[A sinus is/should be heard for three seconds.]



2.
--

(define-rt (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))
(fib 30)
=> 832040.0



***************************************************************
Features
********
-Compilation of very simple lisp functions into machine code.
-All compiled code should normally be hard real time safe.
-Usually the generated code is extremely efficient.
 Hand-written c-code should normally not run any faster.
-Possible to read Guile variables. (Writing only half-worked, and sometimes made guile
 segfault, so I removed it)
-Guile can both read and write variables which is used inside
 the compiled functions.
-Lisp macros
-Most of Common Lisp Music is supported as well as various other snd, sndlib, scheme and guile functions.
-It _should_ not be possible to cause a segfault by running
 a compiled functions. But for now, I know that at least when dividing or moduling by
 0, you will get a segfault. I don't know how to handle that situation yet.
 There are also probably a lot of other situations that might cause
 a segfault, so be careful. Please send me code that cause segfaults.
-Error checking. If there is an error in your code that
 cause the compilation to stop, you sometimes get a human readable
 explanation about it, if you are lucky.



***************************************************************
Limitations
***********
-Only two types are supported: float and int.
-No dynamic types
-A bit limited function-set. No list operations, for example.
-The language does not have a name.
-No optional arguments or keyword arguments. (This can
 be fixed with the help of macros though.) (Optional arguments
 should be easy to support, and will probably be, but rest arguments
 are worse)
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
                 (-> rt-engine start)
                 [one second later, white noise is heard for ten seconds]



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

The rt language does not support dynamic typing or clousors,
and only two types of variabeles can be defined: float and int.

Float
-----
By default all defined variables are floats:

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


Important! There is no boolean type, so #f=0 and #t=1.

Topic: Should declare be used instead of <int> ? Declare is the common
       thing to do, of course, but I think <int> looks a _lot_ nicer...




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



And keyword arguments:

(define-rt-macro (add a1 a2 (#:a3 3) (#:a4 4) (#:a5 5))
  `(+ a1 a2 a3 a4 a5))

(rt-funcall (rt (lambda ()
		  (add 1 2 #:a4 9))))
=> 20
[1+2+3+9+5]



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


break => Used to break out of a while loop.


continue => Go to the top of a while loop.


define => Works nearly as in scheme, but unlike scheme,

          (begin
	    (set! a 2)
	    (define d 9))

          ...is legal.


if => Works as in scheme. But beware that there is no boolean type, and #f=0 and #t=1.
      Therefore, the following expression will return 1, which is not the case
      for scheme: (if 0 0 1)


lambda =>  Works as in scheme, except:
           *Functions might not be tail-recursive if possible. Its not very
            difficult to guarantiue a function to be tail-recursive for single functions, but I think gcc already supports
	    tail-recursive functions, so I hope its not necesarry to add it explicitly. But I might be wrong!
           *Rest argument is not supported: (lambda (a . rest) ...) (error) (there's no list type...)
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
           *This is possible: ((lambda () 5)) => 5
           *This is not possible: ((let ((a (lambda () 5))) a)) => (error)


let => Works as in scheme


let* => Works as in scheme.


letrec => Works as in scheme. I think... I'm a bit confused about the following sentence:

          (let ((a 1))
	    (letrec ((a 2)
		     (b (let ((c (lambda ()
				   a)))
			  (c))))
	      b))

          For some reason, it returns 2, which I alsoe think should correct according to the scheme specification. (?) However, I thought there
          should be a bug here that caused it to return 1... (Probably a bug in the bug)
          (my guile (V1.7cvs) returns an error by the way, mit-scheme returns 2.)


letrec* => Like let*, but with the functions available everywhere:
           (rt-funcall (rt (lambda ()
			     (letrec* ((a 2)
				       (b (lambda ()
					    (c)))
				       (c (lambda ()
					    a)))
			       (b)))))
            => 2.0

           (There is also a letrec* macro for guile in oo.scm.)



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



while => Works as in Guile (Using C's while), including both break and continue.



Special forms (and alikes) implemented as macros
------------------------------------------------
and => works as in scheme

case => works as in scheme, except that = is used for testing instead of eqv?

cond => works as in sheme, but "=>" is not supported

do => works as in scheme (Using while)

include-guile-func => Includes the code of a guile function.

                      (define (add a b)
			(+ a b))
                      (rt-funcall (rt (lambda ()
					(define add (include-guile-func add))
					(add 2 3))))
                      => 5

let => named let is implemented as a macro.

or => works as in scheme

range => (range i 5 10
		(printf "%d " i))
          => 5 6 7 8 9
         (range i 10 5
		(printf "%d " i))
          => 10 9 8 7 6

         (Using while)

unquote => (define a 9)
           (rt-funcall (rt (lambda()
			     ,a)))
           => 9




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
asinh acosh atanh cosh sinh tanh
atan2 (see "man atan2")
hypot (see "man hypot")

zero? positive? negative? odd? even?
remainder modulo quotient

floor ceiling truncate round truncate

logand logior lognot logxor ash
random (only float random)

printf (Using c's fprintf with stderr as the first argument. Warning, this one is not realtime safe!)

vct-length vct-ref vct-set! vct-scale! vct-offset! vct-fill!

vector-ref



**************************************************************************
Reading and writing rt-variables from the guile-side
****************************************************

(define (instrument)
  (let ((osc (make-oscil))
	(vol 0.8))
    (rt-run 0 10
	    (lambda ()
	      (out (* (oscil osc)
		      vol))))))
(define i (instrument))

(-> i vol)
=> 0.8

(-> i osc)
=> #<oscil freq: 440.000Hz, phase: 0.256>


To change the volume:

(set! (-> i vol) 0.2)
(-> i vol)
=> 0.2


To change the frequency:

(set! (mus-frequency (-> i osc)) 200)
=> 200


This will return an error:
(set! (-> i osc) (make-oscil))
...only numbers can be set!.



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
Lockfree Ringbuffer
********************

Use the ringbuffer clm-like generator to excange data between guile and
the realtime thread:

(define osc (make-oscil))
(define rb (make-ringbuffer (* 8192 16)               ;; Number of samples to buffer. This one must be _huge_ to avoid clicking.
			    (lambda (direction)       ;; Direction is an optional integer argument when calling the ringbuffer function. Default value is 1.
			      (oscil osc))))
(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer rb)))))


The above example is not very good, because you can run oscil directly in the
realtime thread. A better example is below. You can't run readin in the realtime thread.
This is how you can play a file without buffering the whole file into a vct:

(define file (make-readin "/home/kjetil/t1.wav"))
(define rb (make-ringbuffer (* 8192 16)
			    (lambda (direction)
			      (readin file))))
(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer rb)))))



(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer rb
				  (lambda (direction)
				    (readin file)))))))

(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer (* 8192 16)
				  (lambda (direction)
				    (readin file)))))))




**************************************************************************
Internal functions for threading, mutex and ringbuffers.
********************************************************

Threading
*********
(create-thread (lambda ()
		 (printf "I'm threaded!\\n")))

-Note, extremely non-realtime safe. (at least for most pthread_create implementations)
-Be careful when letting the thread run while the mother has ended its life-cycle.
-This function is mostly for internal use.

Use with care.


Waiting/signalling
******************
(define rt-conditional (make-rt-conditional))

(create-thread (lambda ()
		 (printf "Waiting\\n")
		 (wait rt-conditional)
		 (printf "Finished\\n")))
(signal rt-conditional)


Signal:
pthread_cond_broadcast(&cond)
Wait
pthread_cond_wait(&cond,&mutex);




**************************************************************************
Notes
*****

[1] I guess "translator" would be a more accurate word to use for 
    what does the thing, rather than "compiler".
[2] Since lists are not supported, calling the functions for "lisp functions"
    probably isn't correct. (?)



**************************************************************************
**************************************************************************
**************************************************************************
!#


(provide 'snd-rt-compiler.scm)

(use-modules (srfi srfi-1))


(if (not (provided? 'snd-oo.scm)) (load-from-path "oo.scm"))

(c-load-from-path eval-c)
(c-load-from-path rt-engine)


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
	 "#include <vct.h>"
	 
	 (public
	  (<void> rt-set-float (lambda ((<SCM> das_float) (<float> newval))
				 (set! (SCM_REAL_VALUE das_float) newval))))
					
	 (<SCM> gakk (lambda ((<SCM> scm))
		       (return (MAKE_POINTER (XEN_TO_MUS_ANY scm)))))
	 (<SCM> gakk2 (lambda ((<SCM> sym) (<SCM> toplevel))
			(return (scm_sym2var sym toplevel SCM_BOOL_F))))
	 (<SCM> gakk3 (lambda ((<SCM> scm))
			(return (MAKE_POINTER (TO_VCT scm)))))
	 (run-now
	  (scm_c_define_gsubr (string "XEN_TO_MUS_ANY") 1 0 0 gakk)
	  (scm_c_define_gsubr (string "TO_VCT") 1 0 0 gakk3)
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
;;(define rt-very-special-forms '(if begin let* rt-while/while))
(define rt-very-special-forms2 (cons 'lambda rt-very-special-forms))

(define rt-macro-prefix 'rt-macro-)
	 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Conversion "scheme"->eval-c ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rt-is-number? type)
  (or (eq? type '<float>)
      (eq? type '<int>)
      (eq? type '<double>)
      (let ((type (eval-c-get-known-type type)))
	(or (eq? type '<float>)
	    (eq? type '<int>)
	    (eq? type '<double>)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-remove-unused++ removes unused variables and numbers.
;;
;; rt-check-calls must have been called on the term before calling
;;
;; (begin 2 4) -> (begin 4)

(define (rt-remove-unused++ term)
  (define (das-remove t)
    (cond ((null? t) t)
	  ((= (length t) 1) (list (rt-remove-unused++ (car t))))
	  ((not (list? (car t))) ;; Removed here.
	   (das-remove (cdr t)))
	  (else
	   (cons (rt-remove-unused++ (car t))
		 (das-remove (cdr t))))))
  
  (cond ((not (list? term)) term)
	((null? term) term)
	((eq? 'rt-begin_p/begin_p (car term))
	 `(rt-begin_p/begin_p ,@(das-remove (cdr term))))
	((eq? 'let* (car term))
	 `(let* ,(cadr term)
	    ,@(das-remove (cddr term))))
	((eq? 'rt-lambda-decl/lambda_decl (car term))
	 term)
	((eq? 'lambda (car term))
	 `(lambda ,(cadr term)
	    ,@(das-remove (cddr term))))
	(else
	 (map rt-remove-unused++ term))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Hacks
;; 
;; -Fix the type for _rt_breakcontsig from float to jmp_buf
;; -Remove assignment of 0 to _rt_breakcontsig
;; 
;; 
;;
(define (rt-last-hacks term)
  (cond ((not (list? term)) term)
	((null? term) term)
	((eq? 'let* (car term))
	 `(let* ,(map (lambda (t)
			(if (eq? '_rt_breakcontsig (car t))
			    '(_rt_breakcontsig <jmp_buf>)
			    (list (car t)
				  (cadr t)
				  (rt-last-hacks (caddr t)))))
		      (cadr term))
	    ,@(rt-last-hacks (cddr term))))
	;;((and (eq? 'set!
	(else
	 (map rt-last-hacks term))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert return calls and fix begin and if blocks
;;
;; Begin is only inserted after a let*-block. Or, after a lambda-block
;; if theres no let*-block in the lambda-block.
;;
;; Fix so that all begin and if blocks returns 0.

(define (rt-insert-returns extpointers term)

  (call-with-current-continuation
   (lambda (return)
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-insert-returns:" args))
       (return #f))
     

     ;; Append 0 if expression doesn't evaluate to a number.
     (define (fix t)
       (cond ((string? t)
	      `(rt-begin_p/begin_p ,t 0))
	     ((symbol? t)
	      (let ((ext (assq t extpointers)))
		(if ext
		    `(rt-begin_p/begin_p ,t 0)
		    t)))
	     ((number? t)
	      t)
	     ((not (list? t))
	      (check-failed "Don't know how to handle" t "."))
	     (else
	      (let ((func (hashq-ref rt-funcs (car t))))
		(if (and func
			 (not (rt-is-number? (-> func return-type))))
		    `(rt-begin_p/begin_p ,t 0)
		    t)))))
       
       (define (insert term)
	 (cond ((not (list? term)) term)
	       ((null? term) term)

	       ((eq? 'lambda (car term))
		(let ((body (cddr term)))
		  (if (and (list? (car body))
			   (eq? 'let* (car (car body))))
		      (let ((vardecls (map insert (cadr (car body))))
			    (bodybody (cddr (car body))))
			`(lambda ,(cadr term)
			   (let* ,vardecls
			     ,(if (= 1 (length bodybody))
				  (list 'return (fix (insert (car bodybody))))
				  (list 'return (insert (cons 'rt-begin_p/begin_p bodybody)))))))
		      (if (and (list? (car body))
			       (eq? 'rt-while/while (car (car body))))
			  `(lambda ,(cadr term)
			     ,(car body)
			     (return 0))
			  `(lambda ,(cadr term)
			     ,(if (= 1 (length body))
				  (list 'return (fix (insert (car body))))
				  (list 'return (insert (cons 'rt-begin_p/begin_p body)))))))))
		     
	       ((eq? 'rt-begin_p/begin_p (car term))
		(let* ((body (cdr term))
		       (but-last (map (lambda (t)
					(insert t))
				      (c-butlast body)))
		       (last (fix (insert (last body)))))
		  `(rt-begin_p/begin_p ,@(map (lambda (t)
						(insert t))
					      (append but-last (list last))))))

	       ((eq? 'rt-if/?kolon (car term))
		(let ((b (caddr term))
		      (c (cadddr term)))
		  `(rt-if/?kolon ,(cadr term)
				 ,(fix (insert b))
				 ,(fix (insert c)))))

	       (else
		(map insert term))))

       (insert term))))
	       
#!
(rt-insert-returns '() '(lambda ()
			  (let* ((a <int> (lambda ((<int> b))
					    2
					    (set! a 9))))
			    (set! a 9))))

!#
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-replace-define-with-letrecs replaces all defines with
;; letrecs. (function copied from snd-hobbit.scm (and sligthly modified))
;;
(define (rt-replace-define-with-letrecs term)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (apply c-display (cons "rt-compiler.scm/rt-replace-define-with-letrecs:" args))
       (return #f))
     
     (define (replace term)
       (cond 
	((not (list? term)) term)
	((null? term) term)
	
	(else
	 (map replace
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
				  (list (append (list 'letrec
						      (map-in-order (lambda (t)
								      (if (list? (cadr t))
									  (list (car (cadr t)) '<float> `(lambda ,(cdadr t)
													   ,@(cddr t)))
									  (list (cadr t) '<float> (caddr t))))
								    defines))
						afterdefines))))))))))))

     (replace term))))


#!
(rt-replace-define-with-letrecs '(lambda ()
				   (define (a b)
				     c)
				   (a)))

(rt-replace-define-with-letrecs '(begin
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
  (letrec ((a <float> 3))
    (- 4 5)
    (letrec ((b <float> (lambda ()
			  (letrec ((d <float> 8))
			    d)))
	     (c <float> 7))
      (* a (b) c))))

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-let*-lifter does:
;; -Lift all let* variable declarations (and non-named lambdas)
;;  up to the closest lambda().
;;
;; Example:
;;
;; (lambda ()
;;   (+ 2 (let* ((a 9))
;;           a)
;;      5)))
;; -> (lambda ()
;;      (let* ((a 0))
;;         (+ 2 (begin
;;                 (set! a 9)
;;                 a)
;;            5)))
;;
;; This is much better, because begin can be used inside function-calls, let* can't:
;;   scheme: (begin (set! a 9) a)
;;        c: (a=9,a).
;;
;; However, in many situations, where the let*-lifting
;; hadn't been necesarry, I guess there can be a minor speed-penalty by
;; clustering all variable-declarations at the top of each lambda-block.
;; But I'm not so sure its a very big point to identify those situations...
;;
;; -rt-fix-various must have been called on the term before calling.
;;  (The variable-names need to be unique to avoid name-clash)


(define (rt-let*-lifter term)
  (define* (lifter term #:optional isnamed)
    (cond ((null? term) (values '() term))
	  ((not (list? term)) (values '() term))
	  
	  ((eq? 'lambda (car term))
	   (call-with-values (lambda ()
			       (lifter (cddr term)))
	     (lambda (letlist new-term)
	       (let ((form (if (not (null? letlist))
			       `(lambda ,(cadr term)
				  (let* ,letlist
				    ,@new-term))
			       `(lambda ,(cadr term)
				  ,@new-term))))
		 (values '()
			 form)))))
;;,		 (if isnamed
;;;		     (values '()
;;;			     form)
;;;		     (let ((funcname (string->symbol (eval-c-get-unique-name))))
;;;		       (values (list (list funcname '<float> form))
;;;			       funcname)))))))
	  
	  ((eq? 'let* (car term))
	   (let ((lets (cadr term))
		 (values-letlist '()))
	     ;; Run lifter for the values (b's in ((a <int> b)))
	     (for-each (lambda (l)
			 (call-with-values (lambda ()
					     (lifter (caddr l) #t))
			   (lambda (letlist term)
			     (set! values-letlist (append! values-letlist letlist))
			     (set-car! (cddr l) term))))
		       lets)
	     ;; Run lifter for let-*body
	     (call-with-values (lambda ()
				 (lifter (cddr term)))
	       (lambda (letlist term)
		 (values (append (map (lambda (l)
					(if (and (list? (caddr l))
						 (or (eq? 'lambda (car (caddr l)))
						     (eq? 'rt-lambda-decl/lambda_decl (car (caddr l)))))
					    l
					    (list (car l) (cadr l) 0)))
				      lets)
				 values-letlist
				 letlist)
			 `(rt-begin_p/begin_p ,@(map (lambda (l)
						       `(set! ,(car l) ,(caddr l)))
						     (remove (lambda (l)
							       (or (and (list? (caddr l))
									(or (eq? 'lambda (car (caddr l)))
									    (eq? 'rt-lambda-decl/lambda_decl (car (caddr l)))))
								   (eq? '_rt_breakcontsig (car l))))
							     lets))
					      ,@term))))))
	  (else
	   (let ((new-letlist '())
		 (new-term '()))
	     (for-each (lambda (t)
			 (call-with-values (lambda ()
					     (lifter t))
			   (lambda (letlist term)
			     (set! new-letlist (append! new-letlist letlist))
			     (set! new-term (cons term new-term)))))
		       term)
	     (values new-letlist
		     (reverse! new-term))))))

  (call-with-values (lambda ()
		      (lifter term #t))
    (lambda (letlist term)
      term)))

#!
(rt-let*-lifter '(lambda ()
		   (gakk (+ 2 3) (lambda ((<int> a))
				   (+ a 9)))))
(rt-let*-lifter '(lambda ()
		   (+ 2 (let* ((a <float> (let* ((b <int> 10))
					    b)))
			  a))
		   5))
!#


					   
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-fix-various does various things.
;;
;; -makes all internal variable names unique,
;; -makes all variable-names legal c-names
;; -transforms all lisp let/let*/letrec/letrec*s into the eval-c version of let*.
;; -returns an assoc-list of the renamed global variable names. All variables,
;;  both local and global needs to be uniqified to avoid name-clash with c-functions,
;;  types, variables, etc.
;; -names starting with _rt_ is not uniqified. (unless it must be legalized...)
;; 
;; (let* ((a 5)) ...)       -> (let* ((a <float> 5)) ...)
;; (let* ((<int> a 5)) ...) -> (let* ((a <int> 5)) ...)
;; (define (a b c) d e)     -> (define a (lambda (b c) d e))
;; (lambda (a b)...)        -> (lambda ((<float> a)(<float> b))...)
;; (set! (asetter 5) 2)     -> (setter!-asetter 5 2)
;;
;; -locate functions that returns SCM's
;;  (at the time of writing only vector-ref), and insert code to convert
;;  the SCM's to whats expected.
;;  (+ 2 (vector-ref vec 3)) -> (+ 2 rt-scm-to-float(vector-ref vec 3))
;;
;; + expand rt macros
;;
;; + more, check code.
;;
;; rt-replace-define-with-letrecs must have been called on the term before calling.
;; 
(define (rt-fix-various term)
  (call-with-current-continuation
   (lambda (return)

     (define all-renamed-variables (make-hash-table 997))
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-fix-various:" args))
       (return #f))
     
     (define renamed-guile-vars '())

     (define get-unique-name
       (let ((n 0))
	   (lambda (orgname)
	     (set! n (1+ n))
	     (let ((das-string (symbol->string orgname)))
	       (if (and (> (string-length das-string) 4) ;; Very special situation. Don't rename internal rt-variables (starting with "_rt_")
			(string= "_rt_" das-string 0 4 0 4))
		   orgname
		   (string->symbol (string-append das-string "_u" (number->string n))))))))
       
       
     (define* (get-new-name name #:optional is-guile-var)
       
       (define* (legalize-name name)
	 (call-with-current-continuation
	  (lambda (return)
	    (for-each (lambda (char)
			(if (and (not (char-alphabetic? char))
				 (not (char=? char #\_))
				 (not (char-numeric? char)))
			    (return 'renamed_var)))
		      (string->list (symbol->string name)))
	    name)))

       (let ((newname (get-unique-name (legalize-name name))))
	 (if is-guile-var
	     (set! renamed-guile-vars (cons (list name newname) renamed-guile-vars)))
	 (if is-guile-var
	     (c-display "guilevar" name newname))
	 (hashq-set! all-renamed-variables newname #t)
	 newname))
     
     (define (fix-lambda-args args)
       (map (lambda (t)
	      (if (list? t)
		  t
		  (list '<float> t)))
	    args))
     
     (define* (fix varlist term #:optional isnamed)  ;; If isnamed is #t, don't letify lambdas.
       (cond ((null? term) term)
	     ((string? term) term)
	     ((number? term) term)

	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; A variable
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ((symbol? term)
	      
	      (let ((var (hashq-ref all-renamed-variables term))) ;; In case the variable is already a renamed version. Can happen with macros.
		(if var
		    term
		    (let ((var (assq term varlist)))
		      (if var
			  (cadr var)
			  (let ((var (assq term renamed-guile-vars)))
			    (if var
				(cadr var)
				(begin
				  (get-new-name term #t)))))))))
	     
	     ((not term)
	      0)
	     ((eq? #t term)
	      1)
	     
	     ((not (list? term))
	      (check-failed "What?" term))

	     ((and (eq? 'set! (car term))
		   (list? (cadr term)))
	      (fix varlist `(,(symbol-append 'setter!- (car (cadr term))) ,@(cdr (cadr term)) ,@(cddr term))))
	     
	     ((eq? 'lambda (car term))
	      (if isnamed
		  (let* ((args (fix-lambda-args (cadr term)))
			 (varnames (map (lambda (t)
					  (list (cadr t)
						(get-new-name (cadr t))))
					args))
			 (lambdaargs (map (lambda (t name)
					    (list (car t)
						  (cadr name)))
					  args
					  varnames)))
		    `(lambda ,lambdaargs
		       ,@(map (lambda (t)
				(fix (append varnames varlist) t))
			      (cddr term))))
		  (let ((funcname (string->symbol (eval-c-get-unique-name))))
		    (fix varlist
			 `(let ((,funcname ,term))
			    ,funcname)))))

	     ;; ((lambda () 5)) -> (let ((u23 (lambda () 5))) (u23))
	     ((and (list? (car term))
		   (eq? 'lambda (car (car term))))
	      (let ((funcname (string->symbol (eval-c-get-unique-name))))
		(fix varlist
		     `(let ((,funcname ,(car term)))
			(,funcname ,@(cdr term))))))


	     ((eq? 'rt-begin_p/begin_p (car term))
	      `(rt-begin_p/begin_p ,@(map (lambda (t)
					    (fix varlist t))
					  (cdr term))))
	     
	     ((eq? 'rt-if/?kolon (car term))
	      `(rt-if/?kolon ,@(map (lambda (t)
				      (fix varlist t))
				    (cdr term))))
	     
	     ;; Convert let/let*/letrec/letrec* to eval-c let*
	     ((or (eq? 'rt-let/let* (car term))
		  (eq? 'let* (car term))
		  (eq? 'letrec (car term))
		  (eq? 'letrec* (car term)))
	      (if (< (length term) 3)
		  (check-failed "Bad" (car term) "-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "First argument to let* must be a list of variables: " term ".")
		      (begin
			(let ((das-vardecls (map (lambda (var)
						   (cond ((not (list? var))
							  (check-failed "\"" var "\" is not a list in expression " term "."))
							 ((not (symbol? (car var)))
							  (check-failed "Illegal variable name: " (car var) " in expression " term "."))
							 ((and (= 3 (length var))
							       (not (or (eq? '<int> (car var))
									(eq? '<float> (car var))
									(eq? '<int> (cadr var))
									(eq? '<float> (cadr var)))))
							  (check-failed "Unsupported type: " (car var) " in expression " term "."))
							 ((and (= 3 (length var))
							       (or (eq? '<int> (car var))
								   (eq? '<float> (car var)))
							       (list (cadr var)
								     (car var)
								     (caddr var))))
							 ((= 1 (length var))
							  (check-failed "Variable \""
									(car var) "\" in expression " term " does not have a value assigned."))
							 ((= 2 (length var))
							  (list (car var)
								'<float>
								(cadr var)))
							 (else
							  var)))
						 (cadr term))))
			  
			  (cond ((eq? 'rt-let/let* (car term))
				 (let* ((newvarlist varlist)
					(vardecls (map-in-order (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(cadr vardecl) ,(fix varlist (caddr vardecl) #t))))
								    (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								    ret))
								das-vardecls)))
				   `(let* ,vardecls
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'let* (car term))
				 (let* ((body (cddr term))
					(vardecls (map-in-order (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(cadr vardecl) ,(fix varlist (caddr vardecl) #t))))
								    (set! varlist (cons (list (car vardecl) uname) varlist))
								    ret))
								das-vardecls)))
				   `(let* ,vardecls
				      ,@(map (lambda (t)
					       (fix varlist t))
					     body))))
				
				((eq? 'letrec (car term))
				 (let* ((newvarlist varlist)
					(funclist '())
					(vardecls (map-in-order (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl))))
								    (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								    (if (and (list? (caddr vardecl))
									     (eq? 'lambda (car (caddr vardecl))))
									(begin
									  (set! funclist (cons (list uname (cadr vardecl) (caddr vardecl))
											       funclist))
									  `(,uname ,(cadr vardecl) (rt-lambda-decl/lambda_decl ,(fix-lambda-args (cadr (caddr vardecl))))))
									`(,uname ,(cadr vardecl) ,(fix varlist (caddr vardecl) #t)))))
								
								das-vardecls)))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(cadr funcdecl) ,(fix newvarlist (caddr funcdecl))))
								 (reverse! funclist)))
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'letrec* (car term))
				 (let* ((newvarlist varlist)
					(funclist '())
					(vardecls (map-in-order (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl))))
								    (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								    (if (and (list? (caddr vardecl))
									     (eq? 'lambda (car (caddr vardecl))))
									(begin
									  (set! funclist (cons (list uname (cadr vardecl) (caddr vardecl))
											       funclist))
									  `(,uname ,(cadr vardecl) (rt-lambda-decl/lambda_decl ,(fix-lambda-args (cadr (caddr vardecl))))))
									`(,uname ,(cadr vardecl) ,(fix newvarlist (caddr vardecl) #t)))))
								das-vardecls)))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(cadr funcdecl) ,(fix newvarlist (caddr funcdecl))))
								 (reverse! funclist)))
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))))))))
	      

	     (else
	      (if (not (symbol? (car term)))
		  (check-failed "Illegal function call:" term ".")
		  (begin
		    (let* ((args (cdr term))
			   (a (cons (symbol-append rt-macro-prefix (car term)) args))
			   (b (macroexpand-1 a )))
		      (if (not b)
			  (return #f)
			  (if (not (equal? a b))
			      (fix varlist b)
			      (let ((funcname (let ((var (assq (car term) varlist)))
						(if var
						    (cadr var)
						    (car term))))
				    (args (map (lambda (t)
						 (fix varlist t))
					       (cdr term))))
				;; Check if some of the arguments for the function is a function returning SCM and needs to be converted.
				(let ((newargs (map (lambda (t argtype)
						      (if (not (list? t))
							  t
							  (let ((func (hashq-ref rt-funcs (car t))))
							    (if (and func
								     (not (eq? '<SCM> argtype))
								     (eq? '<SCM> (-> func return-type)))
								(begin
								  (fix varlist `(,(-> (hashq-ref rt-types argtype) c-transformfunc) ,t)))
								t))))
						    args
						    (let ((func (hashq-ref rt-funcs funcname)))
						      (list-tabulate (length args)
								     (lambda (i)
								       (if func
									   (-> func arg-type i)
									   '<float>)))))))
				  (cons funcname newargs)))))))))))
       
     (let ((ret (fix '() term #t)))
       (c-display "renamed:" renamed-guile-vars)
       (list (map (lambda (var)
		    (list (cadr var) (car var)))
		  renamed-guile-vars)
	     ret)))))
     


#!
(rt-fix-various '(lambda ()
		   (gakk (+ 2 3) (lambda ((<int> a))
				   (+ a 9)))))

(rt-let*-lifter (rt-fix-various '(lambda ()
				   (gakk (+ 2 3) (lambda ((<int> a))
						   (+ a 9))))))

(car (rt-fix-various '(lambda ()
			(locsig loc 0.2))))

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
(macroexpand-1 '(rt-macro-oscil 2 3))


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
;; rt-rt-uniqify-vars must have been called on the term before calling.
;;
(define (rt-check-calls term renamed-vars)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-check-calls:" args))
       (return #f))
     
     (define external-vars '())

     (define (add-external-var varname type iswriting)
       (let ((old (assq varname external-vars)))
	 (if old
	     (let ((rt-type (cadr old)))
	       (if (and (not (eq? type (-> rt-type rt-type)))
			(not (eq? type (-> rt-type c-type))))
		   (if (and (rt-is-number? (-> rt-type rt-type))
			    (rt-is-number? type))
		       (set-car! (cdr old) (hashq-ref rt-types '<float>))
		       (check-failed " Different types for guile variable \"" varname "\": " type "/" (-> rt-type rt-type) ".")))
	       (set-car! (cddr old) (or iswriting (caddr old))))
	     (let ((rt-type (hashq-ref rt-types type)))
	       (if (not rt-type)
		   (check-failed "Unknown type " type ".")
		   (set! external-vars (cons (list varname rt-type iswriting (let ((orgname (assq varname renamed-vars)))
									       (if orgname
										   (cadr orgname)
										   varname)))
					     external-vars)))))))
   
     (define (get-returntype varlist term)
       (c-display "get-returntype for " term)
       (cond ((symbol? term) (let ((avar (assq term varlist)))
			       (if avar
				   (if (and (= 3 (length avar))
					    (list? (caddr avar))
					    (eq? 'lambda (car (caddr avar))))
				       (list (cadr avar) (map car (cadr (caddr avar)))) ;; Return-type is a lambda-function.
				       (cadr avar))
				   'symbol)))
	     ((number? term) 'number)
	     ((list? term) (cond ((member (car term) rt-very-special-forms)
				  'number)
				 ((eq? 'rt-begin_p/begin_p (car term))
				  (get-returntype varlist (last term)))
				 ((eq? 'rt-if/?kolon (car term)) ;; We assume that if can only return compatible types...
				  (get-returntype varlist (caddr term)))
				 (else (let ((func (assq (car term) varlist)))
					 (if func
					     (cadr func)
					     (let ((func (hashq-ref rt-funcs (car term))))
					       (if func
						   (-> func return-type)
						   (check-failed "Unknown function(2) \"" (car term) "\": " term))))))))
	     (else 'unknown-type)))
     

     ;; Check-call checks correct types for function-call.
     (define (check-call varlist term)
       (if (not (symbol? (car term)))
	   (check-failed "Illegal term: " term)
	   (let ((func (assq (car term) varlist)))
	     (if func
		 (begin
		   (if (or (= 2 (length func))
			   (not (list? (caddr func)))
			   (not (or (eq? 'lambda (car (caddr func)))
				    (eq? 'rt-lambda-decl/lambda_decl (car (caddr func))))))
		       (check-failed "Local variable \"" (car term) "\" is not a function:" term))
		   (if (not (= (length (cadr (caddr func)))
			       (- (length term) 1)))
		       (check-failed "Illegal number of argumentes to local function \"" (car term) "\":" term))
		   (for-each (lambda (aterm) (check-calls varlist aterm))
			     term)
		   (for-each (lambda (termarg)
			       (let ((ret-type (get-returntype varlist termarg)))
				 (if (eq? 'symbol ret-type)
				     (add-external-var termarg '<float> #f)
				     (if (and (not (eq? 'number ret-type))
					      (not (rt-is-number? ret-type)))
					 (check-failed "Illegal argument \"" termarg "\" to local function \"" (car term) "\"."
						       "expected a number, found" ret-type ", in expression:" term)))))
			     (cdr term)))
		 (begin
		   (set! func (hashq-ref rt-funcs (car term)))
		   (if (not func)
		       (check-failed "Unknown function \"" (car term) "\": " term)
		       (if (not (-> func legal-number-of-arguments? term))
			   (return #f)
			   (begin
			     (for-each (lambda (aterm) (check-calls varlist aterm))
				       term)
			     (for-each (lambda (termarg argtype)
					 (let ((ret-type (get-returntype varlist termarg)))
					   (cond ((eq? 'symbol ret-type)
						  (add-external-var termarg argtype #f))
						 ((string? termarg)
						  (if (not (eq? '<char-*> argtype))
						      (check-failed "Wrong type in expression: " term ". The string"
								    (<-> "\"" termarg "\"") "is not a " argtype "." ret-type)))
						 ((and (symbol? ret-type)
						       (or (rt-is-number? ret-type)
							   (eq? 'number ret-type)))
						  (if (not (rt-is-number? argtype))
						      (check-failed "Wrong type in expression: " term ". "
								    termarg " is not a " argtype "." (rt-is-number? ret-type) (eq? 'number ret-type))))
						 (else
						  (if (not (equal? ret-type argtype))
						      (check-failed "Wrong type in: " term ". "
								    (car termarg) " Does not return " argtype "."))))))
				       (cdr term)
				       (list-tabulate (length (cdr term))
						      (lambda (i)
							(-> func arg-type i))))))))))))
     
     
     
     (define (check-calls varlist term)
       (c-display "check-calls varlist/term" varlist term)
       (cond ((not (list? term)) #t)
	     ((null? term) #t)

	     ;; RT_WHILE/WHILE
	     ;((eq? 'rt-while/while (car term))
	     ; (check-calls varlist (cadr term))
	     ; (check-calls varlist (caddr term)))
	      
	     ;; RT-LAMBDA_DECL/LAMBDA_DECL	
	     ((eq? 'rt-lambda-decl/lambda_decl (car term))
	      #t)
	     
	     ;; LAMBDA 
	     ((eq? 'lambda (car term))
	      (if (< (length term) 3)
		  (check-failed "Bad lambda-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "Second argument for lambda is not a list: " term ".")	       
		      (if (not (= (length (cadr term))
				  (length (delete-duplicates (cadr term)))))
			  (check-failed "Same argument name for lambda function used more than once: " term ".")
			  (begin
			    (for-each (lambda (varname)
					(if (not (symbol? (cadr varname)))
					    (check-failed "Illegal variable \"" (cadr varname) "\" in lambda-form: " term ".")))
				      (cadr term))
			    (for-each (lambda (aterm)
					(check-calls (append (map (lambda (s)
								    (list (cadr s) (car s)))
								  (cadr term))
							     varlist)
						     aterm))
				      (cddr term)))))))
	     
	     ;; SET!
	     ((eq? 'set! (car term))
	      (if (not (symbol? (cadr term)))
		  (check-failed "Illegal set!(2) term: " term)
		  (if (not (= 3 (length term)))
		      (check-failed "Illegal set!(3) term: " term)
		      (let ((avar (assq (cadr term) varlist)))
			(if (not avar)
			    (add-external-var (cadr term) '<float> #t))
			(check-call varlist term)))))
;;			(check-calls varlist (caddr term))))))
	     

	     ;; BEGIN
	     ((eq? 'rt-begin_p/begin_p (car term))
	      (if (null? (cdr term))
		  (check-failed "begin needs a body: " term ".")
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
	     
	     ;; IF
	     ((eq? 'rt-if/?kolon (car term))
	      (if (< (length term) 3)
		  (check-failed "To few arguments for if:" term ".")
		  (if (> (length term) 4)
		      (check-failed "To many arguments for if:" term ".")))
	      (check-calls varlist (cadr term))
	      (check-calls varlist (caddr term))
	      (if (= (length term) 4)
		  (check-calls varlist (cadddr term))))

	     (else
	      (check-call varlist term))))
     
     (if (not (eq? 'lambda (car term)))
	 (check-failed "This is not a lambda function: " term))

     (c-display)
     (c-display "check-calls term" term)
     (c-display)
     
     (check-calls '() term)

     ;;(c-display "external-vars:" external-vars)

     (let ((extnumbers '())
	   (extpointers '())
	   (extnumbers-writing '()))
       (for-each (lambda (extvar)
		   (if (caddr extvar)
		       (set! extnumbers-writing (cons extvar extnumbers-writing))
		       (if (rt-is-number? (-> (cadr extvar) rt-type))
			   (set! extnumbers (cons extvar extnumbers))
			   (set! extpointers (cons extvar extpointers)))))
		 external-vars)
       (c-display "wrting:" extnumbers-writing)
       (c-display "renamed-vars" renamed-vars)
       (list extnumbers extpointers extnumbers-writing)))))


#!
((<float> c (lambda ((<float> d)) (+ a b)))
 (<float> b 5)
 (<float> a))

(rt-check-calls '(lambda ()
		   (rt-if/?kolon a
				 a
				 a))
		'((a ai)))

(rt-check-calls '(lambda ()
		   ;;(oscil osc2)
		   (set! setfloat-2 (sin osc))
		   (sin (* 0.2 (sin osc) (sin osc2))))
		'((setfloat-2 gakkgakk)))

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

(def-class (<rt-type> rt-type checkfunc c-transformfunc #:key transformfunc error-message (c-type rt-type))
  (def-method (rt-type)
    rt-type)
  (def-method (c-type)
    c-type)
  (def-method (c-transformfunc)
    c-transformfunc)
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

;; Never called!
(define (c-nevercalled-true? . something)
  (c-display "Error. What the? c-nevercalled-true? is never supposed to be called. Something:" something)
  #f)


(define-c-macro (rt-mus-any?/mus_xen_p scm)
  `(?kolon (mus_xen_p ,scm)
	   (XEN_TO_MUS_ANY ,scm)
	   (begin_p
	    (rt_error 6)
	    NULL)))

(begin
  (<rt-type> '<float> number? 'rt_scm_to_float)
  (<rt-type> '<int> number? 'rt_scm_to_float)
  (<rt-type> '<char-*> string? 'rt_scm_to_error) ;; Function does not exist
  (<rt-type> '<vct-*> vct? 'rt_scm_to_vct #:transformfunc TO_VCT)
  (<rt-type> '<vector> vector? #f #:c-type '<SCM>)
  (<rt-type> '<mus_any-*> c-nevercalled-true? 'rt-mus-any?/mus_xen_p)
  (<rt-type> '<void-*> c-nevercalled-true? #f)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rt-funcs (make-hash-table 256))


(def-class (<rt-func> name returntype args #:key min-arguments max-arguments)
  (define last-type (if (null? args)
			#f
			(last args)))

  (def-method (legal-number-of-arguments? term)
    (if (and max-arguments
	     (>= (1- (length term)) max-arguments))
	(begin
	  (c-display "rt-compiler.scm/rt-check-calls: Wrong number of arguments in \"" term "\". Expected maximum"
		     max-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	  #f)
	(if (and min-arguments
		 (< (1- (length term)) min-arguments))
	    (begin
	      (c-display "rt-compiler.scm/rt-check-calls: Wrong number of arguments in \"" term "\". Expected minimum"
			 min-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	      #f)
	    (if (and (not min-arguments)
		     (not max-arguments)
		     (not (= (1- (length term)) (length args))))
		(begin
		  (c-display "rt-compiler.scm/rt-check-calls: Wrong number of arguments in \"" term "\". Expected"
			     (length args) "arguments for" (car term) ". Found" (- (length term) 1) ".")
		  #f)
		#t))))
  
  (def-method (return-type)
    returntype)
  
  (def-method (arg-type argnum)
    (if (>= argnum (length args))
	last-type
	(list-ref args argnum)))
  
  ;(if (not min-arguments)
  ;    (set! min-arguments (length args)))

  ;(if (not max-arguments)
  ;    (set! max-arguments (length args)))
  
  (hashq-set! rt-funcs name this)

  )


(begin

  ;; Functions defined directly. (Note that most functions are defined indirectly using the rt-renamefunc macro.)
  
  ;; Basic
  (<rt-func> '+ '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt--/- '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt--/minusoneargument '<float> '(<float>))
  (<rt-func> '* '<float> '(<float>) #:min-arguments 2)
  (<rt-func> '/ '<float> '(<float>) #:min-arguments 2)
  
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
	    `(sin cos tan acos asin atan exp log log10 sqrt
		  asinh acosh atanh cosh sinh tanh))

  (<rt-func> 'atan2 '<float> '(<float> <float>))
  (<rt-func> 'hypot '<float> '(<float> <float>))

  ;; Bitwise operations
  (<rt-func> 'rt-ash/<< '<int> '(<int> <int>))
  (<rt-func> 'rt-ash/>> '<int> '(<int> <int>))

  ;; Various
  (<rt-func> 'rt-if/?kolon '<float> '(<int> <float> <float>)) ;; Special form, only return-type is checked
  (<rt-func> 'rt-begin_p/begin_p '<float> '(<float>) #:min-arguments 1) ;; Special form, only return-type is checked
  (<rt-func> 'rt-lambda-decl/lambda_decl '<float> '())
  (<rt-func> 'rt-while/while '<int> '(<int> <float>))
  (<rt-func> 'rt-break/break '<void> '(<int>))
  (<rt-func> 'rt-break/return '<void> '(<float>))
  (<rt-func> 'rt-contbreakvar/jmp_buf '<void> '())
  (<rt-func> 'rt-setjmp/setjmp '<int> '())
  (<rt-func> 'rt-break/longjmp '<void> '())
  (<rt-func> 'rt-continue/longjmp '<void> '())
  (<rt-func> 'rt-printf/fprintf '<int> '(<char-*> <float>) #:min-arguments 1)

  (<rt-func> 'rt-vector-ref/vector-ref '<SCM> '(<vector> <int>))
  (<rt-func> 'rt_scm_to_float '<float> '(<SCM>))
  (<rt-func> 'rt_scm_to_vct '<vct-*> '(<SCM>))
  (<rt-func> 'rt-mus-any?/mus_xen_p '<mus_any-*> '(<SCM>))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-rt-macro def . body)
  (if (and (pair? def)
	   (not (list? def)))
      `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def)) ,@body)
      (let* ((name (car def))
	     (new-name (symbol-append rt-macro-prefix name))
	     (args (cdr def))
	     (clean-args '())
	     (optionals '())
	     (rest (string->symbol (eval-c-get-unique-name)))
	     (min-args (string->symbol (eval-c-get-unique-name)))
	     (max-args (string->symbol (eval-c-get-unique-name)))
	     (return (string->symbol (eval-c-get-unique-name))))
	(for-each (lambda (arg)
		    (if (list? arg)
			(set! optionals (append! optionals (list (list (car arg) (keyword->symbol (car arg)) (cadr arg))))))
		    (set! clean-args (append! clean-args (list (if (list? arg) (keyword->symbol (car arg)) arg)))))
		  args)
	`(define-macro ,(cons new-name rest)
	   (call-with-current-continuation
	    (lambda (,return)
	      (let ((,min-args ,(- (length clean-args) (length optionals)))
		    (,max-args ,(length args))
		    ,@(map (lambda (varname)
			     (list varname #f))
			   clean-args))
		(define (set-key-args ,rest)
		  (cond ((null? ,rest) #t)
			,@(map (lambda (optarg)
				 `((eq? (car ,rest) ,(car optarg))
				   (set! ,(cadr optarg) (cadr ,rest))
				   (set-key-args (cddr ,rest))))
			       optionals)
			(else
			 (if (not (keyword? (car ,rest)))
			     (c-display "Unknown argument \"" (car ,rest) "\" for rt-macro \"" ',name "\":" ,rest)
			     (c-display "Unknown key-word argument \"" (car ,rest) "\" for rt-macro \"" ',name "\":" ,rest))
			 (,return #f))))
		
		(if (< (length ,rest) ,min-args)
		    (begin
		      (c-display "To few arguments for rt-macro \"" ',name "\". Expected at least" ,min-args ", found " (length ,rest) ":" ,rest)
		      (,return #f)))
		
		(if (> (length ,rest) (+ ,min-args (* ,(length optionals) 2)))
		    (begin
		      (c-display "To many arguments for rt-macro \"" ',name "\". Expected at most" ,max-args ", found:" ,rest)
		      (,return #f)))
		
		,@(map (lambda (name n)
			 `(set! ,name (list-ref ,rest ,n)))
		       clean-args
		       (iota (- (length clean-args) (length optionals))))
		
		(set-key-args (list-tail ,rest ,min-args))
		
		,@(map (lambda (optarg)
			 `(if (not ,(cadr optarg))
			      (set! ,(cadr optarg) ,(caddr optarg))))
		       optionals)
		,@body)))))))
	    

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
  
(rt-renamefunc remainder % <int> (<int> <int>))

;;(rt-renamefunc vector-ref SCM_VECTOR_REF <float> (<vector> <int>))
				   

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


;; if is a combination of macro and special form
(define-rt-macro (if a b . c)
  (if (null? c)
      `(rt-if/?kolon ,a ,b 0)
      `(rt-if/?kolon ,a ,b ,(car c))))
(define-c-macro (rt-if/?kolon . rest)
  `(?kolon ,@rest))

(define (rt-cond->if terms)
  (let ((term (car terms)))
    (if (and (symbol? (car term))
	     (eq? 'else (car term)))
	(cons 'begin_p (cdr term))
	(if (not (null? (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin_p) (cdr term))
		  (rt-cond->if (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin_p) (cdr term)))))))

(define-rt-macro (cond . terms)
  (rt-cond->if terms))

(define-rt-macro (case key . terms)
  (let ((das-key (string->symbol (eval-c-get-unique-name))))
    `(let ((,das-key ,key))
       (cond ,@(map (lambda (term)
		      (let ((datums (car term))
			    (expr (cdr term)))
			(if (eq? 'else datums)
			    term
			    `((or ,@(map (lambda (datum)
					   `(= ,das-key ,datum))
					 datums))
			      ,@expr))))
		    terms)))))
		  

;; While is a combination of macro and special form
(define-rt-macro (begin . rest)
  `(begin_p ,@rest))

(define-rt-macro (begin_p . rest)
  `(rt-begin_p/begin_p ,@rest))

(define-c-macro (rt-begin_p/begin_p . rest)
  `(begin_p ,@rest))

(define-c-macro (rt-lambda-decl/lambda_decl rest)
  `(lambda ,rest decl))


;; While is a combination of macro and special form.
(define-rt-macro (while test . body)
  (let ((whilefunc (string->symbol (eval-c-get-unique-name)))
	(bod (string->symbol (eval-c-get-unique-name)))
	(dasfunc (string->symbol (eval-c-get-unique-name)))
    	(testfunc (string->symbol (eval-c-get-unique-name)))
	(cont (string->symbol (eval-c-get-unique-name))))
    `(let* ((,whilefunc (lambda ()
			 (let* ((,cont <int> 1)
				(_rt_breakcontsig 0)
				(,testfunc <int> (lambda ()
						   (set! ,cont ,test)))
				(,bod <int> (lambda ()
					      ,@body
					      (,testfunc)))
				(,dasfunc <int> (lambda ()
						  (rt-while/while ,cont
								  (,bod))
						  0)))
			   (if (< (rt-setjmp/setjmp) 2)
			       (begin
				 (,testfunc)
				 (,dasfunc)))))))
       (,whilefunc))))
    
(define-c-macro (rt-while/while test . body)
  `(while ,test ,@body))
(define-c-macro (rt-setjmp/setjmp)
  `(setjmp _rt_breakcontsig))

(define-rt-macro (break)
  `(rt-break/longjmp))
(define-c-macro (rt-break/longjmp)
  `(longjmp _rt_breakcontsig 2))
(define-rt-macro (continue)
  `(rt-continue/longjmp))
(define-c-macro (rt-continue/longjmp)
  `(longjmp _rt_breakcontsig 1))

;;(define-c-macro (rt-while/while test . body)
;;  (let* ((_rt_breakcontsig <jmp_buf>)
    


(define-rt-macro (do variables test . commands)
  `(let ,(map (lambda (variable) (list (car variable) (cadr variable)))
	      variables)
     (while (not ,(car test))
	    ,@commands
	    ,@(map (lambda (variable)
		     `(set! ,(car variable) ,(caddr variable)))
		   (remove (lambda (var) (null? (caddr var)))
			   variables)))
     ,@(cdr test)))

(define-rt-macro (range varname start end . body)
  (let ((das-start (string->symbol (eval-c-get-unique-name)))
	(das-end (string->symbol (eval-c-get-unique-name)))
    	(das-add (string->symbol (eval-c-get-unique-name))))
    `(let* ((<int> ,das-start ,start)
	    (<int> ,das-end ,end)
	    (<int> ,das-add (if (> ,das-end ,das-start) 1 -1))
	    (<int> ,varname ,das-start))
       (while (not (= ,varname ,das-end))
	      ,@body
	      (set! ,varname (+ ,varname ,das-add))))))
#!
(rt-funcall (rt-2 '(lambda (s)
		     (range i -2 0
			    (printf "%f\\n" (+ s i)))))
	    5)

!#

(define-rt-macro (printf string . rest)
  `(rt-printf/fprintf ,string ,@rest))
(define-c-macro (rt-printf/fprintf string . rest)
  `(fprintf stderr (string ,string) ,@rest))
			  

;; let is implemented as a macro to easier be able to support named let. The real name for non-named let is rt-let/let*
(define-rt-macro (let a b . c)
  (if (not (symbol? a))
      `(rt-let/let* ,a ,b ,@c)
      `(letrec ((,a (lambda ,(map car b)
		      ,@c)))
	 (,a ,@(map cadr b)))))

			   
(define-c-macro (rt-dummy/dummy)
  "/* */")

(define-rt-macro (vector-ref vec pos)
  `(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos)))
(define-c-macro (rt-vector-ref/vector-ref vec pos)
  `(SCM_VECTOR_REF ,vec ,pos))
;;  `(SCM_VECTOR_REF ,vec (cast <int> ,pos)))

(define-rt-macro (out . rest)
  (let ((channels (c-butlast rest))
	(val (last rest)))
    (if (null? channels)
	(set! channels '(0 1)))
    (if (number-or-symbol? val)
	`(begin_p
	  ,@(map (lambda (ch)
		   `(if (> (rt-num_outs/num_outs) ,ch)
			(rt-set-outs! ,ch (+ (rt-outs/outs ,ch) ,val))))
		 channels))
	(let ((varname (string->symbol (eval-c-get-unique-name))))
	  `(let* ((,varname ,val))
	     ,@(map (lambda (ch)
		      `(if (> (rt-num_outs/num_outs) ,ch)
			   (rt-set-outs! ,ch (+ (rt-outs/outs ,ch) ,varname))))
		    channels))))))

(define-c-macro (rt-outs/outs n)
  (<-> "_rt_funcarg->outs[" (eval-c-parse n) "]"))
(define-c-macro (rt-set-outs! n val)
  (<-> "_rt_funcarg->outs[" (eval-c-parse n) "]=" (eval-c-parse val)))
(define-c-macro (rt-num_outs/num_outs)
  "_rt_funcarg->num_outs")


;; He he. :-)
(define-rt-macro (unquote something)
  (primitive-eval something))

(define-rt-macro (include-guile-func name)
  (procedure-source (primitive-eval name)))


;; create-thread
(define-rt-macro (create-thread thunk)
  `(rt_create_thread ,thunk))
(<rt-func> 'rt_create_thread '<void> '((<float> ())))

#!
(<rt-type> '<pthread_t> c-nevercalled-true? #f)
(define-rt-macro (create-thread thunk)
  (let ((pthread (string->symbol (eval-c-get-unique-name)))
	(das-func (string->symbol (eval-c-get-unique-name))))
    `(let ((,pthread <pthread_t>)
	   (,das-func <int> (lambda ((<void-*> arg))
				 (,thunk))))
       (rt-create-thread/pthread_create ,pthread ,das-func))))
(<rt-func> 'rt-create-thread/pthread_create '<int> '(<pthread_t> (<void-*> (<void-*>))))
(define-c-macro (rt-create-thread/pthread_create pthread func)
  (<-> "pthread_create(&" (eval-c-parse pthread) ",NULL," (eval-c-parse func) ",NULL)"))
!#


		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; rt/rt-run/rt-funcall/etc. ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rt-4 term)
  (c-display "1")
  (set! term (rt-replace-define-with-letrecs term))
  (c-display "2")
  (if (not term)
      #f
      (let* ((uniqify-res (rt-fix-various term))
	     (renamed-vars (if uniqify-res (car uniqify-res)))
	     (term (begin (c-display "3.5")
			  (if term (rt-let*-lifter (cadr uniqify-res)) #f)))
	     (external-vars (begin (c-display "4")
				   (if term (rt-check-calls term renamed-vars) #f)))
	     (orgargs (if term (cadr term))))
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

	      (c-display "5")
	      (set! term (rt-remove-unused++ term))
	      (c-display "6")	      
	      (if term
		  (set! term (rt-insert-returns extpointers term)))
	      (c-display "8")
	      (if term
		  (set! term (rt-last-hacks term)))
	      (c-display "9")
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
				   orgargs))
	       (i 0)
	       (types (map (lambda (var)
			     (list (eval-c-to-scm
				    (string-trim-right
				     (eval-c-get-propertype
				      (car var))))
				   (cadr var)))
			   publicargs)))

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
		     <float> samplerate
		     <float> res
		     <char-*> error
		     <SCM> errorvariable
		     <int> errorvarnum)
		   
		   "typedef float (*ThreadFunc)(void)"
		   
		   (<void> ,das-funcname (lambda ,(cons `(<struct-func_args-*> _rt_funcarg) publicargs)

					   (<jmp_buf> _rt_errorvar)
					   (<void> rt_error (lambda ((<int> errorval))
							      (longjmp _rt_errorvar errorval)))
					   
					   (<float> rt_scm_to_float (lambda ((<SCM> name))
								      (if (SCM_INUMP name)
									  (return (SCM_INUM name))
									  (if (SCM_REALP name)
									      (return (SCM_REAL_VALUE name))
									      (begin
										(rt_error 2)
										(return 0))))))
					   (<vct-*> rt_scm_to_vct (lambda ((<SCM> name))
								    (if (vct_p name)
									(return (TO_VCT name))
									(begin
									  (rt_error 3)
									  (return NULL)))))

					   (<void> rt_create_thread (lambda ((<ThreadFunc> func))
								      "pthread_t _rt_thread={0}"
								      (<int> isrunning 0)
								      (<void-*> threadfunc (lambda ((<void-*> arg))
											     (<ThreadFunc> dasfunc arg)
											     (set! isrunning 1)
											     (dasfunc)
											     (return NULL)))
								      (pthread_create &_rt_thread NULL threadfunc func)
								      (while (not isrunning) ;; I'm not quite sure why...
									     (usleep 50))
								      ))
								      
					   ,@(map (lambda (extvar)
						    `(<float> ,(car extvar)))
						  extnumbers-writing)
					   
					   (<float> ,rt-innerfuncname (lambda ()
									,@(cddr term)))

					   (if (> (setjmp _rt_errorvar) 0)
					       return)
					   
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
						   (<float> time)
						   (<float> samplerate))
					   ,(if (null? orgargs)
						`(begin
						   (<struct-func_args> funcarg (struct-set num_outs outs num_ins ins time samplerate 0 NULL 0 0))
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
								   ,@(map (lambda (n extvar)
									    (if (eq? '<SCM> (-> (cadr extvar) c-type))
										`(SCM_VECTOR_REF pointers ,n)
										`(GET_POINTER(SCM_VECTOR_REF pointers ,n))))
									  (iota (length extpointers))
									  extpointers)
								   ,@(map (lambda (n) `(SCM_REAL_VALUE(SCM_VECTOR_REF readfloats ,n))) (iota (length extnumbers)))))
						 'return))))
		   
		   (public			      
		    (<float> ,funcname (lambda ((<SCM> argvect))
					 (<struct-func_args> ,funcarg "{0}")

					 (SCM_ASSERT (== ,(length publicargs) (SCM_VECTOR_LENGTH argvect))
						     argvect
						     0
						     (string "Wrong number of arguments."))
							 
					 ,@(map (lambda (n name)
						  `(<SCM> ,(cadr name) (SCM_VECTOR_REF argvect ,n)))
						(iota (length publicargs))
						publicargs)
							      
					 ,@(map-in-order (lambda (das-type)
							   (let* ((type (car das-type))
								  (name (cadr das-type)))
							     (cond ((string=? "UNSPECIFIED" type)
								    (c-display "\n\nError! eval-c.scm/eval-c-gen-public-func: Strange type for " das-type "\n\n"))
								   ((string=? "SCM" type) "/* */")
								   (else
								    `(SCM_ASSERT ,(cond ((string=? "STRING" type) `(|| (scm_is_false ,name) (XEN_STRING_P ,name)))
											((string=? "POINTER" type) `(POINTER_P ,name))
											((string=? "INTEGER" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											((string=? "FLOAT" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											((string=? "DOUBLE" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											(else (c-display "\n\nError! eval.cscm/eval-c-gen-public-func: What?\n\n")))
										 ,name
										 ,(let ((ret i)) (set! i (1+ i)) ret)
										 (string ,type))))))
							 types)
					 
					 (,das-funcname ,(string-append "&" (symbol->string funcarg)) ,@(map (lambda (type)
													       (list (string->symbol (<-> "GET_" (car type)))
														     (cadr type)))
													     types))
					 (SCM_ASSERT (== NULL ,(symbol-append funcarg '.error))
						     ,(symbol-append funcarg '.errorvariable)
						     ,(symbol-append funcarg '.errorvarnum)
						     ,(symbol-append funcarg '.error))
					 (return ,(symbol-append funcarg '.res)))))))))))



#!

(do ((i 0 (1+ i)))
    ((= i 10))
  (display i)
  (newline))

(macroexpand-1 '(rt-macro-do ((i 0 (1+ i)))
			     ((= i 10))
			     (printf "%f\\n" i)))
(macroexpand-1 '(rt-macro-while (not (= i 10))
				  (printf "%f\\n" i)
				  (set! i (1+ i))))

(let* ((unique_name_284 <int> (rt-contbreakvar/jmp_buf))
       (unique_name_283 <int> (lambda ()
				(set! unique_name_284 (not (= i 10)))))
       (unique_name_281 <int> (lambda ()
				(printf "%f\\n" i)
				(set! i (1+ i))
				(unique_name_283)))
       (unique_name_282 <int> (lambda ()
				(rt-while/while unique_name_284
						(unique_name_281))
				0)))
  (if (< (rt-setjmp/setjmp) 2)
      (begin
	(unique_name_283)
	(unique_name_282))))

(define b (hashq-ref rt-funcs 'rt-printf/fprintf))
(list-ref (-> b arg-type 1) 1)

(define a (rt-2 '(lambda ()
		   (do ((i 0 (1+ i)))
		       ((= i 10) (+ 2 5))
		     (printf "%f\\n" i)))))
(define a (rt-2 '(lambda ()
		   (let* ((a 0))
		     a)
		   3)))
(rt-funcall a)

(define a (rt-2 '(lambda (a)
		   (begin
		     ;;(set! c 2)
		     (printf "sdf")))))

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
  (if (not (< y x))
      z
      (tak-rt (tak-rt (- x 1) y z)
	      (tak-rt (- y 1) z x)
	      (tak-rt (- z 1) x y))))

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
						  `(,funcname (vector ,@(map (lambda (extvar)
									       (let ((name (cadddr extvar))
										     (type (cadr extvar)))
										 `(-> ,type transform ,name)))
									     (append extnumbers-writing extpointers extnumbers))
								      ,@(cdr x)))
						  `(begin
						     ,@(map (lambda (extvar)
							      `(if (number? ,(cadddr extvar))
								   (set! ,(cadddr extvar) (exact->inexact ,(car extvar)))))
							    extnumbers-writing)
						     (,funcname (vector ,@(map (lambda (extvar)
										 (let ((name (cadddr extvar))
										       (type (cadr extvar)))
										   `(-> ,type transform ,name)))
									       (append extnumbers-writing extpointers extnumbers))
									,@(cdr x))))))))
	       (rt-callmacro (procedure->macro (lambda (x env)
						 `(begin
						    ,@(map (lambda (extvar)
							     `(if (number? ,(cadddr extvar))
								  (set! ,(cadddr extvar) (exact->inexact ,(cadddr extvar)))))
							   (append extnumbers extnumbers-writing))
						    (let ((ret (<realtime> (,rt-funcname)
									   ;; Note, next vector is gc-marked manually in the funcall smob.
									   (vector (vector ,@(map (lambda (extvar)
												    (let ((name (cadddr extvar))
													  (type (cadr extvar)))
													`(-> ,type transform ,name)))
												  extnumbers-writing))
										   (vector ,@(map (lambda (extvar)
												      (let ((name (cadddr extvar))
													    (type (cadr extvar)))
													`(-> ,type transform ,name)))
												  extpointers))
										   (vector ,@(map (lambda (extvar)
												    (let ((name (cadddr extvar))
													  (type (cadr extvar)))
												      `(-> ,type transform ,name)))
												  extnumbers))
										   ;; Keep untransformed values here so they can be gc-marked. (dont remove!)
										   (list ,@(map cadddr (append extnumbers-writing extpointers extnumbers)))))))
						      ,@(map (lambda (numvar)
							       `(-> ret add-method ',numvar (make-procedure-with-setter
											     (lambda ()
											       ,numvar)
											     (lambda (newval)
											       (rt-set-float ,numvar newval)))))
							     (map cadddr (append extnumbers-writing extnumbers)))
						      ,@(map (lambda (pointer)
							       `(-> ret add-method ',pointer (lambda ()
											      ,pointer)))
							     (map cadddr extpointers))
						      
						      ret))))))
						    
	  
	  (apply eval-c-non-macro (append (list (<-> "-I" snd-header-files-path)
						"#include <math.h>"
						"#include <clm.h>"
						"#include <xen.h>"
						"#include <vct.h>"
						"#include <clm2xen.h>"
						
						"typedef struct {
                                                 mus_any_class *core;
                                                 int chans;
                                                 Float *vals;
                                                 bool data_allocated;
                                                 } mus_frame"
						
						"typedef struct {
						mus_any_class *core;
						mus_any *outn_writer;
						mus_any *revn_writer;
						mus_frame *outf, *revf;
						Float *outn;
						Float *revn;
						int chans, rev_chans;
						mus_interp_t type;
						Float reverb;
						} locs;"

						)

					  
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


(define rt-cached-funcs '())
(define (rt-1 term)
  (let ((cached (assoc term rt-cached-funcs)))
    (if cached
	(cadr cached)
	(let ((new (rt-2 term)))
	  (set! rt-cached-funcs (cons (list term new) rt-cached-funcs))
	  new))))
(define (rt-clear-cache)
  (set! rt-cached-funcs '()))

(define-macro (rt term)
  `(rt-1 ',term))

(define-macro (rt-func term)
  (let ((das-rt (rt-2 term)))
    (if (not das-rt)
	#f
	`(lambda ,(cadr term)
	   (,(cadr das-rt) ,@(cadr term))))))

(define-macro (define-rt def . body)
  `(define ,(car def) (rt-func (lambda ,(cdr def)
				 (define ,def
				   ,@body)
				 (,(car def) ,@(cdr def))))))
  
(define-macro (rt-funcall rt-func . args)
  `((cadr ,rt-func) ,@args))

(define-macro (rt-rt rt-func)
  `((caddr ,rt-func)))

(define-macro (rt-run start dur func)
  (let ((instrument (string->symbol (eval-c-get-unique-name))))
    `(let ((,instrument (rt-rt (rt ,func))))
       (-> ,instrument play-now ,start (+ ,start ,dur))
       ,instrument)))
      
		      
#!
(rt-run start dur
	(lambda ()
	  (out (* (env amp-env)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; CLM/etc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;; CLM Generators ;;;;;;;;;;;;;;;

(define rt-clm-generators '((all-pass     (input (pm 0)))
			    (asymmetric-fm (index (fm 0)))
			    (average      (input))
			    (comb         (input (pm 0)))
			    (convolve     (input-function)) ;; redefined later
			    (delay        (input (pm 0)))
			    (env          ())
			    (filter       (input))
			    (fir-filter   (input))
			    (formant      (input))
			    (granulate    (input-function (edit-function 0))) ;; redefined later
			    (iir-filter   (input))
			    ;;(in-any       (
			    (locsig       (input))
			    (notch        (input (pm 0)))
			    (one-pole     (input))
			    (one-zero     (input))
			    (oscil        ((fm 0) (pm 0)))
			    ;;(out-any      (
			    (phase-vocoder (input-function analyze-function edit-function synthesize-function)) ;; redefined later
			    (pulse-train   ((fm 0)))
			    (rand          ((sweep 0)))
			    (rand-interp   ((sweep 0)))
			    (readin        ())
			    (sawtooth-wave ((fm 0)))
			    (sine-summation ((fm 0)))
			    (square-wave    ((fm 0)))
			    (src            (sr-change input-function)) ;; redefined later
			    (ssb-am         ((insig 0) (fm 0)))
			    (sum-of-cosines ((fm 0)))
			    (sum-of-sines   ((fm 0)))
			    (table-lookup   ((fm 0)))
			    (tap            ((offset 0)) delay)
			    (triangle-wave  ((fm 0)))
			    (two-pole       (input))
			    (two-zero       (input))
			    (wave-train     ((fm 0)))
			    (waveshape      ((index 1) (fm 0)))))

(for-each (lambda (clm-generator)
	    (let* ((name (car clm-generator))              ;; all-pass
		   (args (cadr clm-generator))             ;; (input (fm 0))
		   (args1 (remove list? args))             ;; (input)
		   (args2 (filter-org list? args))         ;; ((fm 0))
		   (belongsto-name (if (= 3 (length clm-generator)) ;; all-pass (Third optional argument, Example: 'delay, because tap belongs to 'delay and not to 'tap.)
				       (caddr clm-generator)
				       name))
		   (c-name (string->symbol                 ;; all_pass
			    (list->string (map (lambda (c)
						 (if (equal? #\- c) #\_ c))
					       (string->list (symbol->string name))))))
		   (c-belongsto-name (string->symbol                 ;; all_pass
				      (list->string (map (lambda (c)
							   (if (equal? #\- c) #\_ c))
							 (string->list (symbol->string belongsto-name))))))
		   (etype (symbol-append                   ;; <mus_all-pass-*>
			   '<mus_ belongsto-name '-*>))
		   (testfunc (primitive-eval               ;; all-pass?
			      (symbol-append belongsto-name '?)))
		   (c-func (symbol-append 'mus_ c-name))   ;; mus_all_pass
		   (macroname (symbol-append               ;; rt-all-pass/mus_all_pass
			       'rt- name '/ c-func))
		   
		   (macro-belongsto-name (symbol-append               ;; rt-all-pass/mus_all_pass
					  'rt- belongsto-name '/ c-func))
		   
		   (c-transformfuncname (symbol-append macro-belongsto-name '?)) ;; rt-all-pass/mus_all_pass?
		   (c-transformfuncname2 (symbol-append 'mus_ c-belongsto-name '_p)) ;; all_pass_p
		   )
	      
	      (if (eq? belongsto-name name)
		  (<rt-type> etype testfunc c-transformfuncname #:c-type '<mus_any-*> #:transformfunc XEN_TO_MUS_ANY))
	      (<rt-func> macroname '<float> (cons etype (map (lambda (a) '<float>) args)))
	      (primitive-eval `(define-rt-macro (,name osc ,@args1 . rest)
				 (if (> (length rest) ,(length args2))
				     (begin
				       (c-display "rt-macro, too many arguments for " ',name ":" rest)
				       #f)
				     (let ((n -1))
				       (append (list ',macroname osc)
					       (list ,@args1)
					       (map-in-order (lambda (arg)
							       (set! n (1+ n))
							       (if (> (length rest) n)
								   (list-ref rest n)
								   arg))
							     (list ,@(map cadr args2))))))))
	      (primitive-eval `(define-c-macro (,macroname osc . rest)
				 `(,',c-func ,osc ,@rest)))
	      (if (eq? belongsto-name name)
		  (begin
		    (primitive-eval `(define-c-macro (,c-transformfuncname scm)
				       `(?kolon (&& (mus_xen_p ,scm)
						    (,',c-transformfuncname2 (XEN_TO_MUS_ANY ,scm)))
						(XEN_TO_MUS_ANY ,scm)
						(begin_p
						 (rt_error 5)
						 NULL))))
		    (<rt-func> c-transformfuncname etype '(<SCM>))
		    
		    ))))

	  rt-clm-generators)


;; restart-env
(rt-renamefunc restart-env mus_restart_env <void> (<mus_env-*>))
;; env-interp
(rt-renamefunc env-interp mus_env_interp <float> (<float> <mus_env-*>))

	       
;; polynomial
(<rt-func> 'rt-polynomial/mus_polynomial '<float> '(<vct-*> <float>))
(define-rt-macro (polynomial coeffs x)
  `(rt-polynomial/mus_polynomial ,coeffs ,x))
(define-c-macro (rt-polynomial/mus_polynomial v x)
  (<-> "mus_polynomial(" (eval-c-parse v) "->data," (eval-c-parse x) "," (eval-c-parse v) "->length)"))

;; mus-fft
(<rt-func> 'rt-mus-fft/mus_fft '<void> '(<vct-*> <vct-*> <int> <int>))
(define-rt-macro (mus-fft v1 v2 i1 i2)
  `(rt-mus-fft/mus_fft ,v1 ,v2 ,i1 ,i2))
(define-c-macro (rt-mus-fft/mus_fft v1 v2 i1 i2)
  (<-> "mus_fft(" (eval-c-parse v1) "->data," (eval-c-parse v2) "->data," (eval-c-parse i1) "," (eval-c-parse i2) ")"))


;; hz->radians
(rt-renamefunc hz->radians mus_hz_to_radians <float> (<float>))

;; mus-srate
(<rt-func> 'mus-srate '<float> '())
(define-c-macro (mus-srate)
  "_rt_funcarg->samplerate")
 
;, Locsig, or at least an attempt. I think its okey, but theres no reverb (The autogenerated locsig macro above is not working)
(define-rt-macro (locsig loc val)
  `(begin
     (range i 0 (mus-channels ,loc)
	    (rt-set-locvals ,loc i ,val))
     (range i 0 (mus-channels (rt-get-loc-outf ,loc))
	    (out i (rt-get-float-val (mus-data (rt-get-loc-outf ,loc)) i)))))

(<rt-func> 'rt-set-locvals '<void> '(<mus_locsig-*> <int> <float>))
(<rt-func> 'rt-set-loc-rev-vals '<void> '(<mus_locsig-*> <int> <float>))
(define-c-macro (rt-set-locvals loc i val)
  (<-> "((locs*)" (eval-c-parse loc) ")->outf->vals[" (eval-c-parse i) "]=" (eval-c-parse val) "* ((locs*)" (eval-c-parse loc) ")->outn[" (eval-c-parse i) "]"))
(define-c-macro (rt-set-loc-rev-vals loc i val)
  (<-> "((locs*)" (eval-c-parse loc) ")->revf->vals[" (eval-c-parse i) "]=" (eval-c-parse val) "* ((locs*)" (eval-c-parse loc) ")->revn[" (eval-c-parse i) "]"))

(<rt-func> 'rt-get-float-val '<float> '(<float-*> <int>))
(define-c-macro (rt-get-float-val float* place)
  (<-> (eval-c-parse float*) "[" (eval-c-parse place) "]"))

(<rt-func> 'rt-get-loc-revf '<mus_any-*> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-revf loc)
  (<-> "((locs*)" (eval-c-parse loc) ")->revf"))

(<rt-func> 'rt-get-loc-outf '<mus_any-*> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-outf loc)
  (<-> "(mus_any*)((locs*)" (eval-c-parse loc) ")->outf"))

(<rt-func> 'rt-get-loc-rev-channels '<int> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-rev-channels loc)
  (<-> (eval-c-parse loc) "->rev_chans"))


(begin
  ;; Src needs special treatment as well.
  (define-rt-macro (src gen sr-change input-function)
    `(rt-mus-src/mus_src ,gen ,sr-change (lambda ((<void-*> arg2)
						  (<int> direction2))
					   (,input-function direction2))))
  (<rt-func> 'rt-mus-src/mus_src '<float> '(<mus_src-*> <int> (<float> (<void-*> <int>))))
  (define-c-macro (rt-mus-src/mus_src gen sr-change input-function)
    (<-> "mus_src(" (eval-c-parse gen) "," (eval-c-parse sr-change) "," (eval-c-parse input-function) ")"))
  

  ;; Same for convolve
  (define-rt-macro (convolve gen input-function)
    `(rt-mus-convolve/mus_convolve ,gen (lambda ((<void-*> arg2)
						 (<int> direction2))
					  (,input-function direction2))))
  (<rt-func> 'rt-mus-convolve/mus_convolve '<float> '(<mus_convolve-*> (<float> (<void-*> <int>))))
  (define-c-macro (rt-mus-convolve/mus_convolve gen input-function)
    (<-> "mus_convolve(" (eval-c-parse gen) "," (eval-c-parse input-function) ")"))

  
  ;; And granulate
  (define-rt-macro (granulate gen input-function . rest)
    (if (null? rest)
	`(rt-mus-granulate/mus_granulate ,gen
					 (lambda ((<void-*> arg2)
						  (<int> direction2))
					   (,input-function direction2)))
	(let ((edit-funcname (string->symbol (eval-c-get-unique-name))))
	  `(rt-mus-granulate/mus_granulate_with_editor ,gen
						       (lambda ((<void-*> arg2)
								(<int> direction2))
							 (,input-function direction2))
						       (let ((,edit-funcname <int> (lambda ((<void-*> arg2)) ;; No way to specify return-type for non-named lambda.
										     (,(car rest)))))
							 ,edit-funcname)))))
  (<rt-func> 'rt-mus-granulate/mus_granulate '<float> '(<mus_granulate-*> (<float> (<void-*> <int>))))
  (define-c-macro (rt-mus-granulate/mus_granulate gen input-function)
    (<-> "mus_granulate(" (eval-c-parse gen) "," (eval-c-parse input-function) ")"))
  (<rt-func> 'rt-mus-granulate/mus_granulate_with_editor '<float> '(<mus_granulate-*> (<float> (<void-*> <int>)) (<int> (<mus_any-*>))))
  (define-c-macro (rt-mus-granulate/mus_granulate_with_editor gen input-function edit-function)
    (<-> "mus_granulate_with_editor(" (eval-c-parse gen) "," (eval-c-parse input-function) "," (eval-c-parse input-function) ")"))

  
  ;; And even phase-vocoder 
  (define-rt-macro (phase-vocoder gen input-function (#:edit-function #f) (#:synthesize-function #f))
    (let ((edit-funcname (string->symbol (eval-c-get-unique-name))))
      `(rt-mus-phase-vocoder/mus_phase_vocoder ,gen
					       (lambda ((<void-*> arg2)
							(<int> dir2))
						 (,input-function dir2))
					       ,(if edit-function
						    `(let ((,edit-funcname <int> (lambda ((<void-*> arg2)) ;; No way to specify return-type for non-named lambda.
										   (,edit-function))))
						       ,edit-funcname)
						    `(rt-mus-pv/NULL1))
					       ,(if synthesize-function
						    `(lambda ((<void-*> arg2))
						       (,synthesize-function))
						    `(rt-mus-pv/NULL2)))))
  (<rt-func> 'rt-mus-phase-vocoder/mus_phase_vocoder '<float> '(<mus_phase-vocoder-*> (<float> (<void-*> <int>)) (<int> (<void-*>)) (<float> (<void-*>))))
  (define-c-macro (rt-mus-phase-vocoder/mus_phase_vocoder gen input-function edit-function synthesize-function)
    (<-> "mus_phase_vocoder_with_editors(" (eval-c-parse gen) "," (eval-c-parse input-function)
	 ",NULL," (eval-c-parse edit-function) "," (eval-c-parse synthesize-function) ")"))
  (<rt-func> 'rt-mus-pv/NULL1 '(<int> (<void-*>)) '())
  (define-c-macro (rt-mus-pv/NULL1)
    "NULL")
  (<rt-func> 'rt-mus-pv/NULL2 '(<float> (<void-*>)) '())
  (define-c-macro (rt-mus-pv/NULL2)
    "NULL")
	     
  )






#!
(define osc (make-oscil #:frequency 440))
(define loc (make-locsig :degree 80 :distance 1 :channels 2))
(rt-2 '(lambda ()
	 (mus-frequency (vector-ref a b))))

	 (set! (mus-frequency (vector-ref a b)) 200)))
	 ;;(* transposition (vct-ref peak-freqs k)))))
	  (locsig loc (* (oscil osc) 0.5))))
!#





;;;;;; CLM Methods ;;;;;;;;;;;;;;;

(for-each (lambda (descr)
	    (let* ((returntype (car descr))
		   (name (cadr descr))
		   (args (cons '<mus_any-*> (caddr descr)))
		   (n -1)
		   (argnames (cons 'gen (map (lambda (arg)
					       (set! n (1+ n))
					       (symbol-append 'arg_ (string->symbol (number->string n))))
					     (caddr descr))))
		   (is-setter (and (> (length descr) 3) (cadddr descr)))
		   (rt-name (if is-setter
				(symbol-append 'setter!-mus- (string->symbol (substring (symbol->string name) 4)))
				(symbol-append 'mus- name)))
		   (c-name (symbol-append 'mus_ name))
		   (funcname (symbol-append 'rt- rt-name '/ c-name)))
	      (<rt-func> funcname returntype args)
	      (primitive-eval `(define-c-macro ,(cons funcname 'rest )
				 `(,',c-name ,@rest)))
	      (primitive-eval `(define-rt-macro ,(cons rt-name 'rest)
				 `(,',funcname ,@rest)))))
	  '((<int> release ())
	    (<char-*> describe ())
	    (<int> equalp (<mus_any-*>))
	    (<float-*> data ())
	    (<float-*> set_data (<float-*>) #t)
	    (<int> length ())
	    (<int> set_length (<int>) #t)
	    (<float> frequency ())
	    (<float> set_frequency (<float>) #t)
	    (<float> phase ())
	    (<float> set_phase (<float>) #t)
	    (<float> scaler ())
	    (<float> set_scaler (<float>) #t)
	    (<float> increment ())
	    (<float> set_increment (<float>) #t)
	    (<float> run (<float> <float>))
	    ;;(<void-*> closure ())
	    (<int> channels ())
	    (<float> offset ())
	    (<float> set_offset (<float>) #t)
	    (<float> width ())
	    (<float> set_width (<float>) #t)
	    (<float> xcoeff (<int>))
	    (<float> set_xcoeff (<int> <float>))
	    (<int> hop ())
	    (<int> set_hop (<int>) #t)
	    (<int> ramp ())
	    (<int> set_ramp (<int>) #t)
	    ;;(<int> read_sample (<int> <int>))
	    ;;(<float> write_sample (<int> <int> <float>))
	    ;;(<char-*> file_name ())
	    (<int> end ())
	    (<int> location ())
	    (<int> set_location (<int>) #t)
	    (<int> channel ())
	    (<float> ycoeff (<int>))
	    (<float> set_ycoeff (<int> <float>) #t)
	    (<float-*> xcoeffs ())
	    (<float-*> ycoeffs ())
	    ;;(<void-*> wrapper ())
	    ))

#!
(rt-2 '(lambda ()
	 (out 6 9 0.4)
	 (out 0.2)))
	 (set! (mus-frequency osc) 200)))
!#




#!
return(xen_return_first(C_TO_XEN_DOUBLE(mus_polynomial(v->data, XEN_TO_C_DOUBLE(x), v->length)), arr));


					 
   
(<rt-type> '<mus_oscil-*> oscil? #:c-type '<mus_any-*> #:transformfunc XEN_TO_MUS_ANY))
(<rt-func> 'rt-oscil/mus_oscil '<float> '(<mus_oscil-*> <float> <float>))
(<rt-func> 'rt-oscil/mus_oscil_0 '<float> '(<mus_oscil-*>))
(<rt-func> 'rt-oscil/mus_oscil_1 '<float> '(<mus_oscil-*> <float>))
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



!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; VCT. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rt-macro (vct-length vct)
  `(rt-vct-length/vct-length ,vct))
   
(define-rt-macro (vct-legal-pos vct pos)
  `(and (>= ,pos 0)
	(<= ,pos (vct-length ,vct))))

(define-rt-macro (vct-ref vct pos)
  `(if (vct-legal-pos ,vct ,pos)
       (rt-vct-ref/vct-ref ,vct (rt-castint/castint ,pos))))

(define-rt-macro (vct-set! vct pos val)
  `(if (vct-legal-pos ,vct ,pos)
       (rt-vct-set!/vct-set! ,vct (rt-castint/castint ,pos) ,val)))

(define-rt-macro (vct-scale! vct scl)
  `(range i 0 (vct-length ,vct)
	  (rt-vct-set!/vct-set! ,vct i (* (rt-vct-ref/vct-ref ,vct i) ,scl))))

(define-rt-macro (vct-offset! vct scl)
  `(range i 0 (vct-length ,vct)
	  (rt-vct-set!/vct-set! ,vct i (+ (rt-vct-ref/vct-ref ,vct i) ,scl))))

(define-rt-macro (vct-fill! vct val)
  `(range i 0 (vct-length ,vct)
	  (rt-vct-set!/vct-set! ,vct i ,val)))


(define-c-macro (rt-castint/castint val)
  `(cast <int> ,val))
(<rt-func> 'rt-castint/castint '<int> '(<int>))

(define-c-macro (rt-vct-data/vct-data vct)
  (<-> (eval-c-parse vct) "->data"))
(define-c-macro (rt-vct-ref/vct-ref vct pos)
  (<-> (eval-c-parse vct) "->data[" (eval-c-parse pos) "]"))
(<rt-func> 'rt-vct-ref/vct-ref '<float> '(<vct-*> <int>))

(define-c-macro (rt-vct-set!/vct-set! vct pos val)
  (<-> (eval-c-parse vct) "->data[" (eval-c-parse pos) "]=" (eval-c-parse val)))
(<rt-func> 'rt-vct-set!/vct-set! '<void> '(<vct-*> <int> <float>))

(define-c-macro (rt-vct-length/vct-length vct)
  (<-> (eval-c-parse vct) "->length"))
(<rt-func> 'rt-vct-length/vct-length '<int> '(<vct-*>))


#!
(define v (vct 2 3 4))
(begin v)
(define a (rt-2 '(lambda ()
		   (vct-scale! v 2))))
(rt-funcall a)
(define a (rt-2 '(lambda ()
		   (let ((<int> i 0))
		     (printf "234234a\\n")))))
	 
!#


#!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Drodle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a (rt-2 '(lambda ()
		   2)))
(define a (rt-2 '(lambda ()
		   (create-thread (lambda ()
				    (printf "I'm threaded!\\n"))))))

(rt-funcall a)

((n_u4 <float>)
 (unique_name_400_u3 <float> (lambda ((<float> n_u4))
			       (rt-if/?kolon (< n_u4 2) n_u4
					     (+ (fib_u2 (rt--/- n_u4 1))
						(fib_u2 (rt--/- n_u4 2))))))
 (fib_u2 <float> 0)
 (fib_u2 <float> (rt-lambda-decl/lambda_decl ((<float> n)))) (n_u1 <float>))


 (lambda ((<float> n_u1))
   (let* ((fib_u2 <float> (rt-lambda-decl/lambda_decl ((<float> n))))
	  (fib_u2 <float> (let* ((unique_name_407_u3 <float> (lambda ((<float> n_u4))
							       (rt-if/?kolon (< n_u4 2)
									     n_u4
									     (+ (fib_u2 (rt--/- n_u4 1))
										(fib_u2 (rt--/- n_u4 2)))))))
			    unique_name_407_u3)))
     (fib_u2 n_u1)))

(rt-let*-lifter '(lambda (n)
		   (let* ((a <float> 9)
			  (fib <float> (lambda (n)
					 (if (< n 2)
					     n
					     (+ (fib (- n 1))
						(fib (- n 2)))))))
		     (+ 2 n))))
(lambda ((<float> n_u1))
  (let* ((fib_u2 <float> (rt-lambda-decl/lambda_decl ((<float> n))))
	 (fib_u2 <float> 0)
	 (unique_name_406_u3 <float> (lambda ((<float> n_u4))
				       (rt-if/?kolon (< n_u4 2)
						     n_u4
						     (+ (fib_u2 (rt--/- n_u4 1))
							(fib_u2 (rt--/- n_u4 2)))))))
    (rt-begin_p/begin_p
     (set! fib_u2 (rt-begin_p/begin_p unique_name_406_u3))
     (fib_u2 n_u1))))

(lambda ((<float> n_u1))
  (let* ((fib_u2 <float> (rt-lambda-decl/lambda_decl ((<float> n))))
	 (fib_u2 <float> 0)
	 (unique_name_401_u3 <float> (lambda ((<float> n_u4))
				       (rt-if/?kolon (< n_u4 2)
						     n_u4
						     (+ (fib_u2 (rt--/- n_u4 1))
							(fib_u2 (rt--/- n_u4 2)))))))
    (rt-begin_p/begin_p
     (set! fib_u2 (rt-begin_p/begin_p unique_name_401_u3))
     (fib_u2 n_u1))))

(define-rt-macro (ai b (#:key1 9))
  `(+ 5 ,b ,key1)) ;; ,key1))
(macroexpand-1 '(rt-macro-ai 50)))

(-> rt-engine start)
(define src-gen (make-src :srate 0.9))
(define osc (make-oscil))
(define i (rt-run 0 5
		  (lambda ()
		    (out (* 0.6 (src src-gen 1.1 (lambda (dir)
						   (oscil osc))))))))
(set! (mus-increment src-gen) 0.4)
(define gran (make-granulate))

(rt-2 '(lambda ()
	 (phase-vocoder ph
			(lambda (dir)
			  0.4)
			#:edit-function (lambda ()
					  2))))
(lambda ()
  (let* ((unique_name_238_u2 <float> (lambda ((<mus_any-*> arg2_u3)
					      (<int> direction2_u4))
				       (let* ((unique_name_239_u5 <float> (lambda ((<float> dir_u6))
									    0.4)))
					 (rt-begin_p/begin_p
					  (unique_name_239_u5 direction2_u4)))))
	 (unique_name_240_u7 <float> (lambda ((<mus_any-*> arg2_u8))
				       (let* ((unique_name_241_u9 <float> (lambda ()
									    2)))
					 (rt-begin_p/begin_p
					  (unique_name_241_u9 direction2_u10))))))
    (rt-mus-granulate/mus_granulate_with_editor gran_u1
						(rt-begin_p/begin_p unique_name_238_u2)
						(rt-begin_p/begin_p unique_name_240_u7))))

(rt-check-calls
 (cadr (rt-let*-lifter
       (rt-fix-various '(lambda ()
			  (src 2 (lambda ((<int> dir)
					  (<int> dir9))
				   0.4))))
       ))
 '())

(rt-2 '(lambda ()
	 (locsig locs 5)))
(rt-2 '(lambda ()
	 (begin
	   (range i 0 (mus-channels loc)
		  (rt-set-locvals loc i 5))
	   (range i 0 (mus-channels (rt-get-loc-outf loc))
		  (out i (rt-get-float-val (mus-data (rt-get-loc-outf loc)) i))))))

(rt-2 '(lambda ()
	 (begin
	   (range i 0 (mus-channels locs)
		  (rt-set-locvals locs i 5))
	   (range i 0 (mus-channels (rt-get-loc-outf locs))
		  (out i (rt-get-float-val (mus-data (rt-get-loc-outf locs)) i))))))

(macroexpand-1 '(rt-macro-locsig locs 5))


(define a (vector  2 3 4 5))
(begin a)
(rt-funcall b)
(define b
  (rt-2 '(lambda ()
	   (+ 2 (vector-ref a 2)))))

(let ((a (vector 2 3 4 5)))
  (rt-funcall b))

(rt-2 '(lambda ()
	 (begin_p
	   (+ 2 3))))

(define a (rt-2 '(lambda ()
		   (case 5
		     ((3 5) 2)
		     ((6) 3)
		     (else
		      4)))))

(macroexpand-1 '(rt-macro-cond ((or (= unique_name_65 3) (= unique_name_65 5)) 2) ((or (= unique_name_65 6)) 3) (else 4)))
	       
			       ((3 5) 2)
			       ((6) 3)
			       (else
				4)))

(rt-2 '(lambda ()
	 (let ((a (lambda ()
		    (let ((b 9))
		      b))))
	   (a))))

(define a (rt-2 '(lambda ()
		   (let ((a <int> 0))
		     (while (< a 10)
			    (printf "ai: %d\\n" a)
			    (if (>= a 5)
				(break))
			    (set! a (1+ a))
			    (if (odd? a)
				(continue))
			    (set! a (1+ a))
			    ;;(break)
			    )
		     a))))
(rt-funcall a)

(macroexpand-1 '(rt-macro-oscil osc2 2 3))

(define-rt-macro (oscil osc . rest)
  (case (length rest)
    ((0) `((,macroname) ,osc ,@args1 2 3))
    ((1) (quasiquote ((unquote macroname)
		      (unquote osc) (unquote-splicing args1)
		      2 3)))))


													  (begin rt-macro-oscil)

(rt-funcall a)
(begin a)

(define a (rt-2 '(lambda ()
		   (let ((ai (lambda (a)
				  5)))
		     (ai 2)))))

(define a (rt-2 '(lambda ()
		   (let loop ((a 5)
			      (b 6)
			      (c 7))
		     (if (< (+ a b c) 100)
			 (loop (1+ a) (1+ b) (1+ c))
			 (+ a b c))))))

(let* ((loop_u1 <float> (rt-lambda-decl/lambda_decl ((<float> a) (<float> b) (<float> c))))
       (loop_u1 <float> (lambda ((<float> a_u2) (<float> b_u3) (<float> c_u4))
			  (rt-if/?kolon (< (+ a_u2 b_u3 c_u4) 100)
					(loop_u1 (1+ a_u2) (1+ b_u3) (1+ c_u4))
					0))))
  (loop_u1 5 6 7))

(define a (rt-2 '(lambda ()
		   (range i 0 5
			  (printf "%d " i)))))

		   (let ((a 2))
		     (printf "ai %f %f\\n" a a)
		     (while (< a 9)
			    (set! a (1+ a)))
		     a))))


(define readfloat 2)
(define setfloat-2 3)
(define osc (make-oscil :frequency 400))
(define osc2 (make-oscil :frequency 10))

(macroexpand-1 '(rt-macro-oscil osc))

(define a (rt-2 '(lambda ()
		   ;;(oscil osc2)
		   (set! setfloat-2 (oscil osc))
		   (out (* 0.2 (oscil osc) (oscil osc2))))))

(define b (rt-rt a))

(begin b)

(begin (cadr b))

(caddr a)

(letrec ((a 2))
  2)

(begin
  (let* ((d 1))
    (letrec ((a 2)
	     (b (let ((c (lambda ()
			   (+ 2 3))))
		  (c))))
      (b))))

(-> b dir)
(-> b setfloat-2)
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


(func (begin .1.) 9 (let* .2.))
->
(let* ((u1 (begin .1.))
       (u2 9)
       (u3 (let* .2.)))
  (func u1 u2 u3))


(define (rt-funcify-do term)
  ;;(c-display "funcify-do" term)
  (cond ((not (list? term)) term)
	((null? term) term)
	((and (eq? 'let* (car term))

(remove number? '(a b 9 c d))

!#
