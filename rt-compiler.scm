
#!

rt-compiler.scm
-Kjetil S. Matheussen/Notam, 2005

rt-compiler.scm is developed with support from Notam/Oslo:
http://www.notam02.no


Oops! API might still change. This file is currently under
heavy development.

Jack must be running before loading this file!



***************************************************************
Introduction
************
rt-compiler provides various functions and macros to compile[1]
and run simple lisp[2] functions.

The original purpose of the langauge was to generate code that
can be hard realtime safe, (and can run safely inside the jack realtime
thread), but the compiler can also be used for general number
crunching. The generated code should be extremely efficient.

As far as possible I have tried to make the language behave and
look like scheme. However, there are no support for consing,
creating closures, or other operations
that can trigger a garbage collection, so its not really a very schemish
language although it visually looks a lot like scheme. Perhaps consing
and more advanced stuff will be implemented later, but it should not be
necesarry as the code blends very fine into Guile. If you need
to create lists or closures, you have to do that in Guile.
Actually, technically, the language is more like C than Scheme.



***************************************************************
Two short examples
******************

1.
--

(let ((osc (make-oscil)))
  (rt-run 0 3
	  (lambda ()
	    (out (* 0.8
		    (oscil osc))))))
=> #:<procedure
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



(There are more examples in the file rt-examples.scm)



***************************************************************
Features
********
-Compilation of very simple lisp functions into machine code.
-All compiled code should normally be hard real time safe.
-Usually the generated code is extremely efficient.
 Hand-written c-code should normally not run any faster.
-Possible to read Guile variables. (Writing Guile variables only half-worked,
 and sometimes made guile segfault, so I removed it.)
-Guile can both read and write variables which is used inside
 the compiled functions.
-Lisp macros
-Most of Common Lisp Music is supported as well as various other snd, sndlib, scheme and guile functions.
-It _should_ not be possible to cause a segfault by running
 a compiled functions. But for now, I know that at least when dividing or moduling by
 0, you will get a segfault. I don't know how to handle that situation yet.
 There are also probably a lot of other situations that might cause
 a segfault, so be careful. Please send me code that cause segfaults.
-Types are automatically determined, but the common lisp operators "declare"[3] and "the" can
 be used on numeric varibles and expressions to improve the performance.
-Error checking. If there is an error in your code that
 cause the compilation to stop, you sometimes get a human readable
 explanation about it, if you are lucky.
-The compiled code is so fast, that theres normally nothing to gain by
 writing it as a function in C instead of writing it as a macro
 or a function. (At least, thats the plan, its currently not quite true yet.)


***************************************************************
Limitations
***********
-A variable can not change type.
-No allocation (consing, vectors, etc.)
-No closures
-No optional arguments or keyword arguments. (Optional arguments
 are supported with the help of macros though.)
-#f=0, #t=1
-Not possible to call Guile functions.
-Not possible to set Guile variables. (There are ways around this though)
-Tail-recursiveness is not guaranteed.
-The function to determine types is wrongly designed, so you sometimes
 have to manually set the types for variables by using "declare" or "the".
 (its a bug that should be fixed, but theres other more important tasks
 placed earlier in the queue.)


***************************************************************
Functions and macros to compile and run rt-functions
****************************************************

rt-compile    => (define a (rt-compile (lambda (b c)
					 (* b c))))

rt-c          => Same as rt-compile

rt-funcall    => (rt-funcall a 2 3)
                  => 6.0

rt-func       => (define a (rt-func (lambda (b c) (* b c))))
                 (a 2 3)
                 => 6.0

define-rt     => (define-rt (a b c) (* b c))
	         (a 2 3)
                 => 6.0

rt-safety     => rt-safety is a setter function. If set to 0, no runtime error checking is performed.
                 Its safe to set (rt-safety) to 0 if you don't get any "RT RUNTIME ERROR" error
                 messages to stderr when running your function, and you are sure thats impossible to happen.
                 On the other hand, if you do see an "RT RUNTIME ERROR" message printed to stderr when running
                 your function, theres a good chance you will lock up your machine by setting (rt-safety)
                 to 0.
                 For operations on lists, pairs and vectors, this could have an impact on the performance.
                 But, generally, don't expect to see a big improvement in the performance by setting it to 0.


rt            => Creates a subclass of <realtime> :

                 (define a (rt (lambda ()
				 (out (oscil osc)))))

rt-run        => Creates a subclass of <realtime> :
                 Second and third argument is when to start playing from the current time and for how long.

                 (rt-run 1 10
			 (lambda ()
			   (out (- (random 1.8) 0.9))))
                 [one second later, white noise is heard for ten seconds]

                 (rt-run is really just a small macro that calls rt, check out its implementation if you are in doubt about what
		  it does exactly)

rt-play       => Creates a subclass of <realtime> :
                 (rt-play (lambda ()
			    (out (oscil osc))))

                 (rt-play is really just a small macro that calls rt, check out its implementation if you are in doubt about what
		  it does exactly)


* The <realtime> class has the following methods:

  play [start] [end]       => Starts playing at the absolute time "start", stopping at the absolute time "end". Default value for "start" is the current time.
                              If "end" is not specified, a stop command is not scheduled.
  stop [end]               => Stop playing at the abolute time "end". Default value for "end" is the current time.
  play-now [start] [end]   => Start playing "start" seconds into the future from the current time, stopping at "end" seconds into the future from the current time.
                              Default value for "start" is the current time.
                              If "end" is not specified, a stop command is not scheduled.
  stop-now [end]           => Stop playing "end" seconds into the future from the current time. Default value for "end" is the current time.


* For define-rt, I have the following lines in my .emacs file:

  (font-lock-add-keywords
   'scheme-mode
   '(("(\\(define-rt\\)\\>\\s-*(?\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 (cond ((match-beginning 1) font-lock-function-name-face)
  	     ((match-beginning 2) font-lock-variable-name-face)
	     (t font-lock-type-face))
         nil t))))


* Compiled rt-functions are cached into memory (currently not to disk).
  (rt-clear-cache!) clears the cache.

* The definstrument-macro is implemented so that rt-code
  are compiled when the instruments definition is being evaluated, and
  not when being called.




***************************************************************
Realtime engine functions
*************************

rte-pause     => Pause

rte-continue  => Continue

rte-reset     => Stops and starts engine. Call this function if you have stuck sounds.
                 (theres currently a huge memory-leak in this function though.)

rte-time       => Returns the time in seconds since the engine was started.
                  ((rte-time) = (rte-frames) / (rte-samplerate))

rte-samplerate => Returns the samplerate.

rte-frames     => Returns the number of frames since the engine was started.

rte-is-running? => Returns true if engine is running. (Ie. not paused)

rte-info         => Returns a list of 5 elements:
                    1. Current size of the priority queue.
                    2. Maximum size of the priority queue.
                    3. Number of lost events because the priority queue was full.
                    4. Number of events waiting to be run.
                    5. Number of <realtime> instances currently running.


***************************************************************
Types
*****

* The rt-language does not support dynamic typing.

* There is no boolean type, so #f=0 and #t=1.

* Use "declare" and "the" to specify types, just like in common lisp.
  See below for usage of "declare" and "the".

* It is no point to declare non-numeric variables. But it won't hurt
  either, unless wrongly declared, which will only make the compilation stop.

* Supported numeric types: <int>, <float> and <double> . These are
  directly mapped to the int, float and double C-types.

* If there are more alternative types than one for a variable, and
  its type has not been declared with "declare", the type will
  be determined based on the following rules for merging different types:
  <int>      + <float>                     -> <float>
  <float>    + <double>                    -> <double>
  <int>      + <double>                    -> <double>
  <void>     + Any type                    -> <void>
  Everything else is illegal.

* I guess there can be a need for an int variable that is guaranteed
  to be at least, or exactly, 64 bits wide. Please tell me if you need such a type,
  and what its name should be.

* Guile variables (ie. of type <SCM>) are automatically converted on the fly to the
  proper types:
   (let ((a (the <int> (vector-ref vec 2))))
     a)
  Will result in code works like this:
   (let ((a (scm_to_int (vector-ref vec 2))))
     a)
  Without using the "the" operator, it would have worked like this:
   (let ((a (the <SCM> (vector-ref vec 2))))
     a)

 For the first example, if (vector-ref vec 2) hadn't been a numeric value, or
 there aren't as many as 3 elements in "vec",
 an error had been caught, and the evaluation of the compiled rt-code
 would stop.




***************************************************************
Closures:
*********
Closures are not supported. And worse, there is currently
no checking whether the code is safe in a language
that doesn't support closures.

The following code:

(define a (rt-2 '(lambda ()
		   (let* ((a (lambda (b)
			       (declare (<int> b))
			       (lambda ()
				  b))))
		     ((a 50))))))
(rt-funcall a)

...returns 0 for me. [4]


(Note, I manually had to add "(declare (<int> b))" to make it compile
because of a bug in the compiler.)




**************************************************************************
Macros:
*******

*Macros are straight forward:

(define-rt-macro (add . args)
  `(+ ,@args))

(rt-funcall (rt (lambda (a b c)
		  (add a b c)))
	    2 3 4)
=> 9



*And keyword arguments:

(define-rt-macro (add a1 a2 (#:a3 3) (#:a4 4) (#:a5 5))
  `(+ a1 a2 a3 a4 a5))

(rt-funcall (rt (lambda ()
		  (add 1 2 #:a4 9))))
=> 20
[1+2+3+9+5]



*The function "rt-expand-macros" works the same as "macroexpand", but for
 rt-macros. It can be used inside other rt-macros, and is currently used in
 the "if", "min", "max", "and" and "or" macros to speed up some situations.


*When letting a variable name start with the prefix "expand/", like this:
 (define-rt-macro (add expand/a expand/b)
   `(+ a b))
  ..a and b are macroexpanded automatically. In some situations, this can cause
  increased performance. (But not in the short add macro above though.)


*For the define-rt-macro macro, I have the following lines in my .emacs file:

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


call-with-current-continuation => I think it works as in scheme, but I'm surprised how
                                  simple it was to implement...


continue => Go to the top of a while loop.


declare => Works as in common lisp, except that the name of the types are different: <int>, <float> and <double> .

           (define-rt (int-fib n)
	     (declare (<int> n))
	     (if (< n 2)
		 n
		 (+ (fib (- n 1))
		    (fib (- n 2)))))

       ...which is the same as:

           (define-rt (int-fib n)
	     (declare (<int> n))
	     (the <int>
		  (if (< n 2)
		      n
		      (+ (fib (- n 1))
			 (fib (- n 2))))))

        As you see, the compiler is a little bit intelligent when determing types, so it should not
        be necesarry to use "declare" on all numeric variables and "the" for every expression, although it shouldn't hurt.


define => Works nearly as in scheme, but unlike scheme it can be placed anywhere in a block. For example:

          (begin
	    (set! a 2)
	    (define d 9))

          ...is legal.


if => Works as in scheme. But beware that there is no boolean type, and #f=0 and #t=1.
      Therefore, the following expression will return 1, which is not the case
      for scheme: (if 0 0 1)


is-type? =>  Mostly for internal use:

             (let ((a 5))
	       (is-type? <int> a))
             => 1
             (let ((a 5))
	       (is-type? <float> a))
             => 0

             The first argument must be a type, and the third argument must be a variable-name.
             This is not legal: (is-type? <int> (+ 2 3))
             Used to implement exact?/inexact?/etc., and for various optimalizations.

lambda =>  Works as in scheme, except:
           *Functions might not be tail-recursive if possible. Its not very
            difficult to guarantee a function to be tail-recursive for single functions, but I think gcc already supports
	    tail-recursive functions, so I hope its not necesarry to add it explicitly. But I might be wrong!
           *Rest argument is not supported: (lambda (a . rest) ...) (error)
            (You can work around this to a certain degree by using macros with keywords or optional arguments)


let => Works as in scheme


let* => Works as in scheme.


letrec => Works as in Guile.


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


set! => Works as in scheme, except that setting Guile variables will not affect the Guile side:

        (let* ((a 5)
	       (b (rt (lambda ()
			(set! a 9)
			a)))
	       (c (rt-funcall b)))
	  (list a c))
	 => (5 9.0)

        (Note, for setting a large number of variables to be visible from the Guile-side, you can use vct-set!)


the => Works as in common lisp, except that the name of the types are different: <int>, <float> and <double> are
       the currently supported numeric types.

       (define-rt (int-cast-add a b)
          (the <int>
	       (+ a b)))

       ...which is the same as:

       (define-rt (int-cast-add a b)
          (declare (<float> a b))
          (the <int>
	       (+ a b)))


while => Works as in Guile, including both break and continue. (Does not expand
         to a recursive function.)


	  

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

(Many of these functions are made by looking at snd-run.c)
      
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
random

exact? inexact? number? string?
exact->inexact inexact->exact

printf (Using c's fprintf with stderr as the first argument. Warning, this one is not realtime safe!)

vct-length vct-ref vct-set! vct-scale! vct-offset! vct-fill!

vector? vector-length vector-ref

pair? null?
car cdr
cadr caddr cadddr caddddr
cddr cdddr cddddr cdddddr
cdar cdadr cdaddr cdadddr
caar caadr caaddr caadddr
list-ref
for-each



**************************************************************************
Reading and writing rt-variables from the guile-side
****************************************************

(definstrument (instrument)
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
Using CLM
*********

Almost all CLM classes are supported, as well as all their methods,
and other functions. Most things should work as expected, hopefully.

Exceptions:


* CLM constructors are not supported:

  (define func (rt (lambda ()
		     (let* ((osc (make-oscil :frequency 440)))
		       (oscil osc)))))

  [error]


* For all the generators that may require an input-function argument, (that is convolve, granulate, phase-vocoder
  and src), the input-function argument is not optional but must be supplied: (src s
										   (lambda (direction)
										     (readin file)))

* (mus-srate) returns the samplerate specified by the current rt-driver (ie jack), not what SND reports.
              To avoid different values for mus-srate reported by snd and rt, (set! (mus-srate) (rte-samplerate))
              is called in the init-process of rt-engine. If you set (mus-srate) later (in Guile), you might get unexpected results.

* (mus-srate) is not settable.

* The following CLM generators are not supported: in-any and out-any. However, their functionality probably need to be supported someway,
  probably by in-any and out-any. But thats not implemented yet.

* readin has mostly been rewritten to be able to buffer the whole sound first instead of reading while from harddisk while playing.
  The new readin also remembers which buffers are currently in use, so playing the same file many time simultaniously will not cause extra memory usage.

  There is another thing to be avare of though: While the following block should work as expected:
  (let ((rs (make-readin "1.wav")))
    (rt-run 0 10
	    (lambda ()
	      (out (readin rs)))))

  The following block will not:
  (let ((rs (vector (make-readin "1.wav") (make-readin "2.wav"))))
    (rt-run 0 10
	    (lambda ()
	      (out 0 (readin (vector-ref 0 rs)))
	      (out 1 (readin (vector-ref 1 rs))))))
  [A run-time error-checker will make the function exit before doing anything, and no sound will be heard.]
       
  Instead you have to do:
  (let ((rs (vector (make-rt-readin (make-readin "1.wav")) (make-rt-readin (make-readin "2.wav")))))
    (rt-run 0 10
	    (lambda ()
	      (out 0 (readin (vector-ref 0 rs)))
	      (out 1 (readin (vector-ref 1 rs))))))

* Short example of the use of readin, here's a fileplayer running in an endless loop:
  (let ((rs (make-readin "/home/kjetil/t1.wav")))
    (rt-play (lambda ()
	       (if (>= (mus-location rs) (mus-length rs))
		   (set! (mus-location rs) 0))
	       (out (readin rs)))))

* Reverb for the locsig generator is not implemented. I'm a bit confused about locsig actually. I'm not sure the rt-implementation
  is correct...

* Non of the frames/mixers/sound IO functions are supported, as they require disk-access, which shouldn't be done inside the
  audio thread.

* Only hz->radians is implemented from the "Useful functions" section of the CLM manual. (Most of them probably only requires
  a 2-3 lines long macro to be supported though.)

* array-in, dot-product, sine-bank, edot-product, contrast-enchancement, ring-modulate, amplitude-modulate, fft, multiply-arrays,
  rectangular->polar, rectangular->polar, spectrum and convolution is not implemented. (Most of these probably only requires
  6-10 lines of wrapping-code to be supported.)

* However, "mus-fft" seems to be supported (although I barely remember doing it), perhaps it does the same as "fft"...?




**************************************************************************
Getting sound in and out
************************

This simple function will software monitor the two first channels for 10 seconds:

(rt-run 0 10
	(lambda ()
	  (out 0 (in 0))
	  (out 1 (in 1))))



This function does the same, but swaps the channels:

(rt-run 0 10
	(lambda ()
	  (out 0 (in 1))
	  (out 1 (in 0))))



This function does the same, but will mix both input-channels before sending
the result to both channel 0 and 1.

(rt-run 0 10
	(lambda ()
	  (out 0 1 (in 0 1))))




This function does exactly the same, but using a shorter syntax:

(rt-run 0 10
	(lambda ()
	  (out (in))))




This function will send the sum of the first two input-channels to
the 10 first even-numbered output-channels:


(rt-run 0 10
	(lambda ()
	  (out 0 2 4 6 8 10 12 14 16 18 (in))))







**************************************************************************
Lockfree Ringbuffer (not implemented)
********************

Use the ringbuffer clm-like generators to excange data-streams between guile and
the realtime thread.

* ringbuffer:
*************
(define osc (make-oscil))
(define rb (make-ringbuffer (* 8192 256)       ;; Number of samples to buffer. This one should be _huge_ to avoid clicking.
			    (lambda ()
			      (oscil osc))))
(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer rb)))))


The above example is not very good, because you can run oscil directly in the
realtime thread. A better example is below, because you can't call readin in the realtime thread.
This is how you can play a file without buffering the whole file into memory, which the rt-version of readin does:

(define file (make-readin "/home/kjetil/t1.wav"))
(define rb (make-ringbuffer (* 8192 256)
			    (lambda ()
			      (readin file))))
(rt-run 0 10
	(lambda ()
	  (out (* 0.8 (ringbuffer rb)))))



* ringbuffer-location:
**********************
Assumes that location doesn't change to radically, only 0 or 1 steps more or less
compaired to the last one. It can receive request for any step though, but it might not
be able to catch the value in time.

(define file (file->sample "/home/kjetil/t1.wav"))
(define rb (make-ringbuffer-location (* 8192 256)
				     (lambda (location)
				       (file->sample file location))))
(define position 0)
(rt-run 0 100
	(lambda ()
	  (out (* 0.8 (ringbuffer-location rb position))) ;; If data is not availabe, a value from the buffer is returned instead. Might produce less clicks than zero.
	  (set! position (1+ position))))


To delay playing until data is available:

(rt-run 0 100
	(lambda ()
	  (if (ringbuffer-location? rb position)         ;; ringbuffer-location? whether data at the position is available. If #f, a request is sent.
	      (begin
		(out (* 0.8 (ringbuffer-location rb position)))
		(set! position (1+ position))))))




**************************************************************************
Internal functions for threading, mutex and ringbuffers.
********************************************************

Threading (probably not needed)
*********
(create-thread (lambda ()
		 (printf "I'm threaded!\\n")))

-Note, extremely non-realtime safe. (at least for most pthread_create implementations)
-Be careful when letting the thread run while the mother has ended its life-cycle.
-This function is mostly for internal use.

Use with care.


Waiting/signalling (not implemented)
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
Various
********

* In addition to the functions and macros described above, heres a bounch of very internal functions and macros
  that used wrongly can hang your machine or destroy your harddisk.
  Most of them start with a prefix "rt-" or "rt_".

* Theres still a lot of smaller optimalisations thats possible to do. However,
  gcc (V>=3) should be able to fix most of these.




**************************************************************************
Notes
*****

[1] I guess "translator" would be a more accurate word to use for 
    what does the thing, rather than "compiler".
[2] Since creating lists are not possible, calling the functions for "lisp functions"
    probably isn't correct. (?)
[3] Paul Graham, "ANSI Common Lisp", 1996, p. 313: "Not an operator,
    but resembles one (...)". (About declare)
[4] The return value is actually undefined, not 0 though, at least
    according to the generated C-code.


**************************************************************************
**************************************************************************
**************************************************************************
!#


(provide 'snd-rt-compiler.scm)

(use-modules (srfi srfi-1))


;;; Implementation detail: map must run its arguments in order. There must be no
;;; difference between map-in-order and map. (stupid map, luckily guile maps in proper order)
;;; In case guile change this nice behaviour of map, do: (define map map-in-order)


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


(eval-c  (string-append "-I" snd-header-files-path)
	 "#include <clm.h>"
	 "#include <xen.h>"
	 "#include <clm2xen.h>"
	 "#include <vct.h>"
	 
	 (<SCM> rt_set_float (lambda ((<SCM> das_float) (<SCM> newval))
			       (set! (SCM_REAL_VALUE das_float) (GET_FLOAT newval))
			       (return SCM_UNDEFINED)))
	 (<SCM> gakk (lambda ((<SCM> scm))
		       (return (MAKE_POINTER (XEN_TO_MUS_ANY scm)))))
	 (<SCM> gakk15 (lambda ((<SCM> scm))
			 (return (MAKE_POINTER (SCM_SMOB_DATA scm)))))
	 (<SCM> gakk2 (lambda ((<SCM> sym) (<SCM> toplevel))
			(return (scm_sym2var sym toplevel SCM_BOOL_F))))
	 (<SCM> gakk3 (lambda ((<SCM> scm))
			(return (MAKE_POINTER (TO_VCT scm)))))
	 (run-now
	  (scm_c_define_gsubr (string "rt-set-float!") 2 0 0 rt_set_float)
	  (scm_c_define_gsubr (string "XEN_TO_MUS_ANY") 1 0 0 gakk)
	  (scm_c_define_gsubr (string "SCM_SMOB_DATA") 1 0 0 gakk15)
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

(define rt-verbose #f)
(define rt-very-verbose #f)

(define rt-operators '(+ - * / = < > <= >=))

(define rt-unknown-ret-type-ops '(+ rt--/- rt--/minusoneargument * rt-min/MIN rt-max/MAX 1+ 1-))

(define rt-very-special-forms '(if begin let*))
;;(define rt-very-special-forms '(if begin let* rt-while/while))
(define rt-very-special-forms2 (cons 'lambda rt-very-special-forms))

(define rt-macro-prefix 'rt-macro-)

(define rt-globalstructname '<struct-RT_Globals>)
(define rt-globalvarname 'rt_globals)
(define rt-globalvardecl (list rt-globalstructname (symbol-append '* rt-globalvarname)))


;;; Various functions

(define (rt-symbol-starts-with? sym s)
  (let* ((das-string1 (symbol->string sym))
	 (das-string2 (symbol->string s))
	 (len (min (string-length das-string1) (string-length das-string2))))
    (string= das-string1 das-string2 0 len 0 len)))

#!
(symbol-starts-with? 'abwe4wei 'aiai)
!#

(define rt-safety
  (let ((safety 1))
    (make-procedure-with-setter
     (lambda ()
       safety)
     (lambda (n)
       (set! safety n)))))

(define (rt-is-safety?)
  (not (= 0 (rt-safety))))
  
(define (rt-print . rest)
  (if rt-verbose
      (apply c-display rest)))

(define (rt-print2 . rest)
  (if rt-very-verbose
      (apply c-display rest)))


(define rt-gensym
  (let ((num 0))
    (lambda ()
      (set! num (1+ num))
      (string->symbol (<-> "rt_gen" (number->string num))))))


;; An immediate is something that is not a function-call.

(define rt-immediates (make-hash-table 219))
(define (rt-add-immediate funcname)
  (hashq-set! rt-immediates funcname #t))

(define (rt-immediate? . rest)
  (if (null? rest)
      #t
      (and (or (not (list? (car rest)))
	       (and (not (null? (car rest))) ;; In case, there is most probably an error, but I'm not sure.
		    (hashq-ref rt-immediates (car (car rest)))))
	   (apply rt-immediate? (cdr rest)))))

(rt-add-immediate 'is-type?)
		  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Conversion "scheme"->eval-c ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rt-is-number? type)
  (or (eq? type '<float>)
      (eq? type '<int>)
      (eq? type '<double>)
      (and (symbol? type)
	   (let ((type (eval-c-get-known-type type)))
	     (or (eq? type '<float>)
		 (eq? type '<int>)
		 (eq? type '<double>))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Hacks, last step before code is completely evalycified.
;; 
;; -Change the order of type and variable name for lambda-args. eval-c requires the opposite order of rt:
;;  (lambda ((a <int>)(b <float>))...) -> (lambda ((<int> a)(<float> b))...)
;; -Remove variable names for function types:
;;  (let* ((b (<int> ((c <float>))) 0))...) -> (let* ((b (<int> (<float>))))...)
;; -Surround strings with the eval-c macro "string".
;;  "gakk" -> (string "gakk")
;; -Remove all rt-dummy/dummy calls completely.
;; -Replace all '<undefined> types with <SCM>s, but give warnings.
;; -Replace all rt type-names with theire c type names.
;; -Insert <struct-RT_Globals> *rt_globals as the first argument for all functions.
;; -Insert rt_globals as the first argument in function-calls where required.
;;
(define (rt-last-hacks term)

  (define (make-proper-type t name) ;; Name is only used for warning-message.
    (cond ((eq? '<undefined> t)
	   (begin
	     (c-display "\n\n\nWarning, had to set undefined variable name \"" name "\" to SCM:" term ".\n\n\n")
	     '<SCM>))
	  ((list? t)
	   (list (make-proper-type (car t) name)
		 (map (lambda (t2)
			(make-proper-type (cadr t2) name))
		      (cadr t))))
	  (else
	   (-> (hashq-ref rt-types t) c-type))))
    
  (define globals (cadr term))
			  
  (define localfuncs '())
  
  (define (last-hacks term)
    (define (map2 term)
      (delete '(rt-dummy/dummy) (map last-hacks term)))
  
    (cond ((string? term) `(string ,term))
	  ((symbol? term)
	   (if (assq term globals)
	       (symbol-append 'rt_globals-> term)
	       term))
	  ((not (list? term)) term)
	  ((null? term) term)
	  ((eq? 'let* (car term))
	   (let ((vardecls (map (lambda (vardecl)
				  (if (and (= 3 (length vardecl))
					   (list? (caddr vardecl)))    ;; Ie. a lambda-funcion.
				      (begin
					(set! localfuncs (cons (car vardecl) localfuncs))
					(list (car vardecl)
					      (make-proper-type (cadr vardecl) (car vardecl))
					      (last-hacks (caddr vardecl))))
				      (if (and (= 3 (length vardecl))
					       (not (number? (caddr vardecl))))
					  (list (car vardecl)
						(make-proper-type (cadr vardecl) (car vardecl))
						(last-hacks (caddr vardecl)))
					  (list (car vardecl)
						(make-proper-type (cadr vardecl) (car vardecl))))))
				(cadr term))))
	     `(let* ,vardecls
		,@(last-hacks (cddr term)))))
	  
	  ((or (eq? 'lambda (car term))
	       (eq? 'rt-lambda-decl (car term)))
	   (let ((vardecl (map (lambda (t)
				 (list (make-proper-type (cadr t) (car t))
				       (car t)))
			       (cadr term))))
	     (set! vardecl (cons rt-globalvardecl vardecl))
	     (if (eq? 'lambda (car term))
		  `(lambda ,vardecl
		     ,@(map2 (cddr term)))
		  `(rt-lambda-decl ,vardecl))))
	  
	  ((member (car term) localfuncs)
	   (map last-hacks
		(append (list (car term) 'rt_globals) (cdr term))))

	  (else
	   (let ((func (hashq-ref rt-funcs (car term))))
	     (if (and func
		      (-> func needs-rt-globals))
		 (map last-hacks
		      (append (list (car term) 'rt_globals) (cdr term)))
		 (map2 term))))))
    
  ;;(c-display "term" term)
  ;;(c-display "caddr term" (cadr (caddr term)))
  (for-each (lambda (vardecl)
	      (if (= 2 (length vardecl))
		  (set! globals (cons vardecl globals))))
	    (cadr (caddr term)))

   (last-hacks term))

#!
(rt-last-hacks '(lambda ((freq__1 <float>))
		  (let* ()
		    (return (* freq__1 (rt-/// 6.28318530717959 (mus-srate)))))))
	       
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert return calls and fix begin and if blocks
;;
;;

(define (rt-insert-returns term returntype)

  (call-with-current-continuation
   (lambda (return)
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-insert-returns:" args))
       (return #f))
     

     (define (insert term returntype)
       (define (default-behaviour)
	 (if (eq? '<void> returntype)
	     term
	     `(return ,term)))

       (cond ((not (list? term)) (default-behaviour))
	     ((null? term) (default-behaviour))
	     
	     ;; Can not happen
	     ((eq? 'rt-lambda-decl (car term))
	      (check-failed "Somethings wrong"))
	     ;;term)

	     ((eq? 'lambda (car term))
	      (let ((body (cddr term)))
		`(lambda ,(cadr term)
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ((or (eq? 'begin (car term))
		  (eq? 'rt-begin (car term)))
	      (let ((body (cdr term)))
		`(begin
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ((eq? 'let* (car term))
	      (let* ((body (cddr term))
		     (vardecls (map (lambda (vardecl)
				      (if (and (= 3 (length vardecl))
					       (list? (caddr vardecl))
					       (eq? 'lambda (car (caddr vardecl))))
					  `(,(car vardecl) ,(cadr vardecl) ,(insert (caddr vardecl) (cadr vardecl)))
					  (list-copy vardecl)))
				    (cadr term))))
		`(let* ,vardecls
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ;; ifs are translated to rt-if (a?b:c) by the if-macro, but normal ifs can still be generated in the compiling-processs.
	     ((eq? 'if (car term))
	      (if (= 4 (length term))
		  `(if ,(cadr term)
		       ,(insert (caddr term) returntype)
		       ,(insert (cadddr term) returntype))
		  (if (eq? '<void> returntype)
		      `(if ,(cadr term)
			   ,(insert (caddr term) returntype))
		      (begin
			(c-display "Undefined return value in some function because of missing else-block in non-void returning if sentence.")
			(check-failed "The error is not supposed to show up here. Please send your code to k.s.matheussen@notam02.no ")))))
	     
	     ((and (eq? 'rt-if (car term))
		   (eq? '<void> returntype))
	      (insert `(if ,@(cdr term)) returntype))


	     ((eq? 'begin (car term))
	      (let ((body (cdr term)))
		`(begin
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (body)))))
	     
	     ;;((eq? 'rt-begin (car term))
	      
	     ;;((and (eq? 'rt-begin (car term))
;;		   (eq? '<void> returntype))
;;	      (c-display "jepp")
;;	      (insert `(begin ,@(cdr term)) returntype))
	     
	     (else
	      (default-behaviour))))
     
     (insert term returntype))))

#!

(rt-insert-returns '(lambda ((v_u2 <vct-*>))
		      (let* ()
			(let* ((rt_gen674_u1 <int> 0)
			       (rt_gen675_u3 <int> 0)
			       (i_u4 <int> 0)
			       (_rt_breakcontsig_u5 <jmp_buf> 0))
			  (rt-begin
			   (set! rt_gen674_u1 (rt-vct-length/vct-length v_u2))
			   (set! rt_gen675_u3 (rt-if (rt-> rt_gen674_u1 0)
						     1
						     -1))
			   (set! i_u4 0)
			   (rt-dummy/dummy)
			   (rt-begin
			    (rt-while (not (rt-= i_u4 rt_gen674_u1))
				      (rt-vct-set!/vct-set! v_u2 i_u4 (* (rt-vct-ref/vct-ref v_u2 i_u4) 2))
				      (set! i_u4 (+ i_u4 rt_gen675_u3))))))))
		   '<void>)

(rt-insert-returns '(lambda ()
		      (rt-if (rt-= 0 b)
			     3
			     4))
		   '<int>)
(rt-insert-returns '(lambda ()
		      (+ 2 35))
		   '<int>)

(rt-insert-returns '(lambda ()
		      (let* ((a <void> (lambda ((<int> b))
					2
					(set! a 9))))
			(set! a 9)))
		   '<void>)


(rt-insert-returns '(lambda ((_rt_local_f <int>))
		      (let* ((rt_gen300 <int> 0)
			     (c <int>)
			     (a <int> 0)
			     (f <int>)
			     (d <int> (lambda ()
					(+ c f 2)))
			     (b <int> (rt-lambda-decl ((c <int>))))
			     (rt_gen298 <int> (lambda ((_rt_local_c <int>))
						(set! c _rt_local_c)
						(rt-begin
						 (set! a (d)))))
			     (b <int> (lambda ((_rt_local_c <int>))
					(if rt_gen300
					    (let* ((rt_gen299 <int> c)
						   (_rt_ret <int> (rt_gen298 _rt_local_c)))
					      (set! <int> rt_gen299)
					      _rt_ret)
					    (let* ((_rt_ret <int>))
					      (set! rt_gen300 1)
					      (set! _rt_ret (rt_gen298 _rt_local_c))
					      (set! rt_gen300 0)
					      _rt_ret)))))
			(set! f _rt_local_f)
			(rt-begin
			 (set! a 3)
			 (b e))))
		   '<int>)

!#
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-lambda-lifter lifts up all lambda-functions so they
;; are not inside another lambda function anymore.
;;
;; This is strictly
;; not necesarry because gcc support local functions, but it
;; speeds up the access to local functions a lot. For example,
;; I think mus_src can run 2-3 times as fast...
;;
;; The function puts all local functions and variables that need to be global into the top-level.
;;
;; (lambda ()
;;  (let* ((a <int> 0)
;;	   (b <int> (lambda ((c <int>))
;;	              (let* ((d <int> (lambda ()
;;		                       (+ c 2)))))
;;		         (set! a (d))))
;;         (e <int> 0))
;;    (set! a 3)
;;    (set! e 9)
;;    (b e)))
;;
;;->
;;
;;;; The first let*-block contains global variables and functions. The rest is the main-function.
;;;; (note that all global variables are put into its own struct later, so they are not really global variables though.)
;;;; (note (again) that because the rt-language needs to support callbacks from c-code, it can't do the traditional lambda-lifting, (note to myself, insert references ([])),
;;;;  (by adding extra arguments to the inner function when necessary), but the speed-penalty is probably very low because of this.
;;;;  Supporting both traditional lambda-lifting and the variant below is possible though (by letting the variant below call the traditional version),
;;;;  but that has not been implemented yet. I also
;;;;  need to do some benchmarks to see if traditional lambda-lifting really is faster. Perhaps it isn't...)
;;
;;(lambda ()
;;    (let* ((a <int> 0)
;;           (c <int> 0)
;;           (d <int> (lambda ()
;;		        (+ c 2)))
;;	     (b <int> (lambda ((_rt_local_c <int>))
;;		        (set! c _rt_local_c)
;;		        (set! a (d)))))
;;      (let* ((e <int> 0))
;;	  (set! a 3)
;;	  (set! e 9)
;;	  (b e))))
;;
;; rt-remove-unused++ must have been called on the term before calling. (So that the unused variables can be catched more easely, I think.)
;;

(define (rt-lambda-lifter term)

  (define globals '())   ;; Just the names: (varname1 varname2)
  (define globals2 '())  ;; With type: ((varname1 <int>)(varname2 <float))
  (define globalfuncs '())

  (define (add-global var)
    (if (not (memq var globals))
	(set! globals (cons var globals))))
  
  (define (find-globals varlist term)
    (cond ((symbol? term)
	   (if (not (memq term varlist))
	       (add-global term)))
	  
	  ((not (list? term)) #t)
	  ((null? term) #t)

	  ((eq? 'lambda (car term))
	   (let ((newvarlist (map car (cadr term))))
	     (for-each (lambda (t)
			 (find-globals newvarlist t))
		       (cddr term))))
	   
	  ((eq? 'let* (car term))
	   (let* ((newvarlist varlist))
	     (for-each (lambda (vardecl)
			 (if (and (= 3 (length vardecl))
				  (list? (caddr vardecl)))
			     (find-globals '() (caddr vardecl))
			     (set! newvarlist (cons (car vardecl) newvarlist))))
		       (cadr term))
	     (for-each (lambda (t)
			 (find-globals newvarlist t))
		       (cddr term))))

	  ((eq? 'is-type? (car term))
	   #t)
	  
	  (else
	   (for-each (lambda (t)
		       (find-globals varlist t))
		     (cdr term)))))

  (define* (lambdalifter name type term #:optional dontcapsulate)
    (let* ((globsets '())      ;; Bunch of set!-commands.
	   (globvars-here '()) ;; Global variables handled in this spesific lambda function.
	   (vars (map (lambda (var)
			(if (and (not dontcapsulate)
				 (memq (car var) globals))
			    (let ((tempname (symbol-append '_rt_local_ (car var))))
			      (set! globsets (cons `(set! ,(car var) ,tempname) globsets))
			      (set! globals2 (cons var globals2))
			      (set! globvars-here (cons var globvars-here))
			      (list tempname (cadr var)))
			    var))
		      (cadr term)))

	   (das-func #f)
	   
	   (body (map lifter (cddr term)))

	   (is-first-let*? (and (list? (car body))
				(eq? 'let* (car (car body))))))
      
      
      ;;(if (not (list? (car body)))
      ;;	  (set! body `(rt-begin
      ;;		       ,@body)))
      
      ;; Find any global variables defined in the let*-block in the function.
      (if is-first-let*?
	  (for-each (lambda (var)
		      (if (memq (car var) globals)
			  (set! globvars-here (cons var globvars-here))))
		    (cadr (car body))))

      (set! das-func (if is-first-let*?
			 `(lambda ,vars
			    (let* ,(cadr (car body))
			      ,@(reverse! globsets)
			      ,@(cddr (car body))))
			 `(lambda ,vars
			    ,@(reverse! globsets)
			    ,@body)))

      ;;(c-display "lambdalifter globs/term" globvars-here term)
      ;;(c-display "das-func" das-func)
      ;;(c-display dontcapsulate)
      ;;(newline)

      (if (or (null? globvars-here)
	      dontcapsulate) ;; For some reason, we know that its safe not to put global accessible local variables on the stack. (Ie. its the main function. :-) )
	  (begin
	    das-func)
	  (let* ((realfuncname (rt-gensym))
		 (letbody (map (lambda (name)
				 (cons (rt-gensym) (reverse name)))
			       globvars-here))
		 (funccall `(,realfuncname ,@(map car vars)))
		 (capspart #f))
			     
	    (set! globalfuncs (cons `(,name ,type (rt-lambda-decl ,(cadr term)))
				    globalfuncs))
	    (set! globalfuncs (cons `(,realfuncname ,type ,das-func)
				    globalfuncs))

	    
	    (set! capspart `(let* ,(if (eq? '<void> type)
				       letbody
				       (append letbody
					       (list (list '_rt_ret type funccall))))
			      ,@(if (eq? '<void> type)
				    (append funccall
					    (map (lambda (letb)
						   `(set! ,(caddr letb) ,(car letb)))
						 letbody))
				    (append (map (lambda (letb)
						   `(set! ,(caddr letb) ,(car letb)))
						 letbody)
					    (list '_rt_ret)))))

	    (if (< (length globvars-here) 5)  ;; Performance is dependent on this number. Perhaps 5 is a good value. Haven't done any benchmark.
	 	`(lambda ,vars                ;; It should be set quite high, because of larger amount of code and branching in the second version.
		   ,capspart)                 ;; On the other hand, stacking up these variables are probably usually unnecesarry (but has to be done),
		(let ((recnum (rt-gensym)))   ;; so the program counter usually jumps into the second version. Hard to say whats best...
		  (set! globals2 (cons (list recnum '<int> 0) globals2))
		  `(lambda ,vars
		     (if ,recnum
			 ,capspart
			 ,(if (eq? '<void> type)
			      `(begin
				 (set! ,recnum 1)
				 ,funccall
				 (set! ,recnum 0))
			      `(let* ((_rt_ret ,type))
				 (set! ,recnum 1)
				 (set! _rt_ret ,funccall)
				 (set! ,recnum 0)
				 _rt_ret))))))))))
			    
			       
		 
  
  (define (lifter term)
    (cond ((not (list? term)) term)
	  ((null? term) term)

	  ((eq? 'let* (car term))
	   (let* ((vardecls '()))
	     (for-each (lambda (var)
			 (if (and (= 3 (length var))
				  (list? (caddr var)))
			     (if (eq? 'rt-lambda-decl (car (caddr var)))
				 (set! globalfuncs (cons var
							 globalfuncs))
				 (set! globalfuncs (cons (list (car var) (cadr var) (apply lambdalifter var))
							 globalfuncs)))
			     (if (memq (car var) globals)
				 (set! globals2 (cons var globals2))
				 (set! vardecls (cons var vardecls)))))
		       (cadr term))
	     (set! vardecls (reverse! vardecls))
	     (if (null? vardecls)
		 `(rt-begin
		    ,@(cddr term))
		 `(let* ,vardecls
		    ,@(cddr term)))))

	  (else
	   (map lifter term))))


  (find-globals '() term)

  (let ((res (lambdalifter 'nameignored '<doesntmatter> term #t)))
    `(lambda ,(cadr res)
       (let* ,(append globals2 (reverse! globalfuncs))
	 ,@(cddr res)))))


#!

(rt-lambda-lifter '(lambda ((src5 <float>))
		     (let* ((func <float> (lambda ()
					    (+ 2 src5))))
		       func)))


(rt-lambda-lifter '(lambda ((f <int>))
		     (let* ((a <int> 0)
			    (b <int> 0)
			    (c <int> (lambda ()
				       (+ f b))))
		       (set! b 9)
		       (c))))

(rt-lambda-lifter '(lambda ((f <int>))
		     (let* ((a <int> 0)
			    (b <int> (lambda ((c <int>))
				       (let* ((d <int> (lambda ()
							 (+ c f 2))))
					 (set! a (d)))))
			    ;;(e <int> 0)
			    )
		       (set! a 3)
		       ;;(set! e 9)
		       (b e))))
->
(lambda ((_rt_local_f <int>))
  (let* ((rt_gen300 <int> 0)
	 (c <int>)
	 (a <int> 0)
	 (f <int>)
	 (d <int> (lambda ()
		    (+ c f 2)))
	 (b <int> (rt-lambda-decl ((c <int>))))
	 (rt_gen298 <int> (lambda ((_rt_local_c <int>))
			    (set! c _rt_local_c)
			    (rt-begin
			     (set! a (d)))))
	 (b <int> (lambda ((_rt_local_c <int>))
		    (if rt_gen300
			(let* ((rt_gen299 <int> c)
			       (_rt_ret <int> (rt_gen298 _rt_local_c)))
			  (set! <int> rt_gen299)
			  _rt_ret)
			(let* ((_rt_ret <int>))
			  (set! rt_gen300 1)
			  (set! _rt_ret (rt_gen298 _rt_local_c))
			  (set! rt_gen300 0)
			  _rt_ret)))))
    (set! f _rt_local_f)
    (rt-begin
     (set! a 3)
     (b e))))


(lambda ((_rt_local_f <int>))
  (let* ((c <int>)
	 (a <int> 0)
	 (f <int>)
	 (d <int> (lambda ()
		    (+ c f 2)))
	 (b <int> (lambda ((_rt_local_c <int>))
		    (set! c _rt_local_c)
		    (rt-begin
		     (set! a (d))))))
    (set! f _rt_local_f)
    (let* ((e <int> 0))
      (set! a 3)
      (set! e 9)
      (b e))))

(lambda () (let* ((b_u3 <float>)
		  (a_u1 <float> (rt-lambda-decl ()))
		  (c_u2 <float> (rt-lambda-decl ((b_u3 <float>))))
		  (inner_u4 <void> (lambda ((n_u5 <int>))
				     (set! b_u3 n_u5)))
		  (c_u2 <float> (rt-lambda-decl ((b_u3 <float>))))
		  (rt_gen350 <float> (lambda ((_rt_local_b_u3 <float>))
				       (set! b_u3 _rt_local_b_u3)
				       (rt-begin
					(rt-begin
					 (rt-if (rt-=/== 0 b_u3)
						(c_u2 1)
						(inner_u4 2))
					 b_u3))))
		  (c_u2 <float> (lambda ((_rt_local_b_u3 <float>))
				  (let* ((rt_gen351 <float> b_u3)
					 (_rt_ret <float> (rt_gen350 _rt_local_b_u3)))
				    (set! <float> rt_gen351)
				    _rt_ret)))
		  (a_u1 <float> (lambda ()
				  (rt-begin
				   (rt-begin
				    (c_u2 0))))))
	     (rt-begin
	      (rt-begin
	       (a_u1)))))


!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-remove-unused++ removes unused variables and numbers.
;;
;; + evaluate is-type?, returning either 0 or 1.
;; 
;; rt-check-calls must have been called on the term before calling
;;
;; (begin 2 4) -> (begin 4)
;; (if (not 0) some thing) -> some
;; (if 0 some thing) -> thing
;; 
(define (rt-remove-unused++ term)

  (call-with-current-continuation
   (lambda (return)
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-remove-unused++:" args))
       (return #f))
     
     (define vars (make-hash-table 219))
     
     (define (das-remove t)
       (cond ((null? t) t)
	     ((= (length t) 1) (list (remove++ (car t))))
	     ((not (list? (car t))) ;; Removed here.
	      (das-remove (cdr t)))
	     (else
	      (cons (remove++ (car t))
		    (das-remove (cdr t))))))
     
     (define (remove++ term)
       (rt-print2 "remove++" term)
       (cond ((not (list? term)) term)
	     ((null? term) term)

	     ;; begin
	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(das-remove (cdr term))))

	     ;; is-type?
	     ((eq? 'is-type? (car term))
	      (if (list? (caddr term))
		  (let ((func (hashq-ref rt-funcs (car (caddr term)))))
		    (if (not func)
			(check-failed (caddr term) "not found in term (1)" term)
			(if (eq? (cadr term) (-> func return-type))
			    1
			    0)))
		  (let ((type (hashq-ref vars (caddr term))))
		    (if (not type)
			(check-failed (caddr term) "not found in term (2)" term)
			(if (eq? type (cadr term))
			    1
			    0)))))

	     ;; let*
	     ((eq? 'let* (car term))
	      (for-each (lambda (var)
			  (if (and (= 3 (length var))
				   (list? (caddr var))
				   (or (eq? 'lambda (car (caddr var)))
				       (eq? 'rt-lambda-decl (car (caddr var)))))
			      (hashq-set! vars (car var) (list (cadr var)
							       (map car (cadr (caddr var)))))
			      (hashq-set! vars (car var) (cadr var))))
			(cadr term))
	      (let ((vardecl (map (lambda (var)
				    (list (car var) (cadr var) (remove++ (caddr var))))
				  (cadr term))))
		`(let* ,vardecl
		   ,@(das-remove (cddr term)))))

	     ;; not
	     ((eq? 'not (car term))
	      (let ((a (remove++ (cadr term))))  ;; This can be a is-type? test.
		(if (number? a)
		    (if (= 0 a)
			1
			0)
		    `(not ,a))))

	     ;; if
	     ((eq? 'rt-if (car term))
	      (let ((a (remove++ (cadr term)))) ;; This can be a is-type? test.
		(if (number? a)
		    (if (= 0 a)
			(remove++ (cadddr term))
			(remove++ (caddr term)))
		    `(rt-if ,a
			    ,(remove++ (caddr term))
			    ,(remove++ (cadddr term))))))
	     
	     ;; lambda-decl
	     ((eq? 'rt-lambda-decl (car term))
	      (for-each (lambda (var)
			  (hashq-set! vars (cadr var) (car var)))
			(cadr term))
	      term)

	     ;; lambda
	     ((eq? 'lambda (car term))
	      (for-each (lambda (var)
			  (hashq-set! vars (car var) (cadr var)))
			(cadr term))
	      `(lambda ,(cadr term)
		 ,@(das-remove (cddr term))))

	     (else
	      (map remove++ term))))

     (remove++ term))))




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
						      (map (lambda (t)
								      (if (list? (cadr t))
									  (list (car (cadr t)) `(lambda ,(cdadr t)
													   ,@(cddr t)))
									  (list (cadr t) (caddr t))))
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
;; (However, in many situations, where the let*-lifting
;;  hadn't been necesarry, I guess there can be a minor speed-penalty by
;;  clustering all variable-declarations at the top of each lambda-block.
;;  But I'm not so sure its a very big point to identify those situations...)
;;
;; -rt-fix-various must have been called on the term before calling.
;;  (The variable-names need to be unique to avoid name-clash)


(define (rt-let*-lifter term)
  (define (lifter term)
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
	  
	  ((eq? 'let* (car term))
	   (let ((lets (cadr term))
		 (values-letlist '()))
	     ;; Run lifter for the values (b's in ((a <int> b)))
	     (for-each (lambda (l)
			 (call-with-values (lambda ()
					     (lifter (cadr l)))
			   (lambda (letlist term)
			     (set! values-letlist (append! values-letlist letlist))
			     (set-car! (cdr l) term))))
		       lets)
	     ;; Run lifter for let-*body
	     (call-with-values (lambda ()
				 (lifter (cddr term)))
	       (lambda (letlist term)
		 (values (append (map (lambda (l)
					(if (and (list? (cadr l))
						 (or (eq? 'lambda (car (cadr l)))
						     (eq? 'rt-lambda-decl (car (cadr l)))))
					    l
					    (list (car l) (cond ((rt-symbol-starts-with? '_rt_breakcontsig (car l))
								 '<jmp_buf>)
								
								((and (number? (cadr l))    ;; Quick way to determine type. Type doesn't need to be determined here, but it
								      (exact? (cadr l)))    ;; can speed up things if its possible to know type already at this stage.
								 '<int>)                    ;; The type does not need to be correct either, but a number must be
								                            ;; a number, etc.
								((number? (cadr l))
								 '<float>)
								((string? (cadr l))
								 '<char-*>)
								((not (list? (cadr l)))
								 '<undefined>)
								((memq (car (cadr l)) rt-unknown-ret-type-ops)
								 '<undefined>)
								(else
								 (let ((func (hashq-ref rt-funcs (car (cadr l)))))
								   (if func
								       (-> func return-type)
								       '<undefined>)))))))
				      
				      lets)
				 values-letlist
				 letlist)
			 `(rt-begin ,@(map (lambda (l)
						       `(rt-set*! ,(car l) ,(cadr l)))
						     (remove (lambda (l)
							       (or (and (list? (cadr l))
									(or (eq? 'lambda (car (cadr l)))
									    (eq? 'rt-lambda-decl (car (cadr l)))))
								   (rt-symbol-starts-with? '_rt_breakcontsig (car l))))
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
		      (lifter term))
    (lambda (letlist term)
      term)))

#!
(rt-let*-lifter '(lambda ()
		   (gakk (+ 2 3) (lambda (a)
				   (+ a 9)))))
(rt-let*-lifter '(lambda ()
		   (+ 2 (let* ((a (let* ((b 10))
				    b)))
			  a))
		   5))
!#


					    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-expand-macros does that. First thing to do.
;; +setter handling
;; (set! (asetter 5) 2)     -> (setter!-asetter 5 2)
;;
(define (rt-expand-macros term)
  (rt-print2 "expand" term)
  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-expand-macros:" args))
       (return #f))
     
     (define (expand term)
       (cond ((null? term) term)
	     ((not (list? term)) term)
	     ((and (eq? 'set! (car term))
		   (list? (cadr term)))
	      (expand `(,(symbol-append 'setter!- (car (cadr term))) ,@(cdr (cadr term)) ,@(cddr term))))
	     ((list? (car term))
	      (map expand term))
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
			      (expand b)
			      (cons (car term)
				    (map expand args))))))))))

     (expand term))))




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
;;
;; -locate functions that returns SCM's
;;  (vector-ref, car, cdr, etc.), and insert code to convert
;;  the SCM's to whats expected.
;;  (+ 2 (vector-ref vec 3)) -> (+ 2 (rt-scm-to-float (vector-ref vec 3)))
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
	       (if (and (> (string-length das-string) 7) ;; Very special situation. Don't rename internal rt-variables (starting with "_rt_dr_")
			(string= "_rt_rn_" das-string 0 7 0 7))
		   orgname
		   (string->symbol (string-append das-string "__" (number->string n))))))))
       
       
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
     
     ;; letrec has this stupid(?) rule... (Check out 4.2.2 in R5RS) (The handling here is overstrict though.)
     (define illegal-vars (make-hash-table 151))
     (define (add-illegal-vars . vars)
       (for-each (lambda (var) (hashq-set! illegal-vars var #t)) vars))
     (define (remove-illegal-vars . vars)
       (for-each (lambda (var) (hashq-remove! illegal-vars var)) vars))
     
     
     (define* (fix varlist term #:optional isnamed)  ;; If isnamed is #t, don't letify lambdas.
       ;;(c-display "fixing" term)
       (cond ((null? term) term)
	     ((string? term) term)
	     ((number? term) term)

	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; A variable
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ((symbol? term)

	      (let ((var (let ((var (hashq-ref all-renamed-variables term))) ;; In case the variable is already a renamed version. Can happen with macros.
			   (if var
			       term
			       (let ((var (assq term varlist)))
				 (if var
				     (cadr var)
				     (let ((var (assq term renamed-guile-vars)))
				       (if var
					   (cadr var)
					   #f))))))))
		(if var
		    (let ((illegal-var (hashq-ref illegal-vars var)))
		      (if illegal-var
			  (check-failed "The variable name \"" term "\" can not be accessed here, because it is"
					"a reference to another variable defined in the same letrec-block.")
			  var))
		    (begin
		      (get-new-name term #t)))))
  


	     ((not term)     ;; term=#f
	      0)
	     ((eq? #t term)  ;; term=#t
	      1)
	     
	     ((not (list? term))
	      (check-failed "Don't know how to handle" term))

	     ((eq? 'lambda (car term))
	      (if isnamed
		  (let* ((args (cadr term))
			 (varnames (map (lambda (t)
					  (list t
						(get-new-name t)))
					args))
			 (lambdaargs (map cadr varnames)))
		    `(lambda ,lambdaargs
		       ,@(map (lambda (t)
				(fix (append varnames varlist) t))
			      (cddr term))))
		  (let ((funcname (rt-gensym)))
		    (fix varlist
			 `(rt-let/let* ((,funcname ,term))
				       ,funcname)))))
	     
	     ;; ((lambda () 5)) -> (let ((u23 (lambda () 5))) (u23))
	     ((list? (car term))
	      (let ((funcname (rt-gensym)))
		(fix varlist
		     `(rt-let/let* ((,funcname ,(car term)))
				   (,funcname ,@(cdr term))))))
	      

	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(map (lambda (t)
					    (fix varlist t))
					  (cdr term))))
	     
	     ((eq? 'rt-if (car term))
	      `(rt-if ,@(map (lambda (t)
			       (fix varlist t))
			     (cdr term))))
	     
	     ((eq? 'is-type? (car term))
	      `(is-type? ,(cadr term) ,(fix varlist (caddr term))))
	     
	     ((eq? 'the (car term))
	      `(the ,(cadr term) ,(fix varlist (caddr term))))
	     
	     ;; Convert let/let*/letrec/letrec* to the eval-c version of let*
	     ((or (eq? 'rt-let/let* (car term))
		  (eq? 'let* (car term))
		  (eq? 'letrec (car term))
		  (eq? 'letrec* (car term)))
	      (if (< (length term) 3)
		  (check-failed "Bad" (car term) "-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "First argument to" (car term) "must be a list of variables:" term ".")
		      (begin
			(let ((das-vardecls (map (lambda (var)
						   (cond ((not (list? var))
							  (check-failed "\"" var "\" is not a list in expression " term "."))
							 ((not (symbol? (car var)))
							  (check-failed "Illegal variable name: " (car var) " in expression " term "."))
							 ((= 1 (length var))
							  (check-failed "Variable \""
									(car var) "\" in expression " term " does not have a value assigned."))
							 (else
							  var)))
						 (cadr term))))
			  
			  (cond ((eq? 'rt-let/let* (car term))
				 (let* ((newvarlist varlist)
					(vardecls (map (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(fix varlist (cadr vardecl) #t))))
								    (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								    ret))
								das-vardecls)))
				   `(let* ,vardecls
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'let* (car term))
				 (let* ((body (cddr term))
					(vardecls (map (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(fix varlist (cadr vardecl) #t))))
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
					(das-das-vardecls (map (lambda (vardecl)
								 (let ((uname (get-new-name (car vardecl))))
								   (add-illegal-vars uname)
								   (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								   (cons uname (cdr vardecl))))
							       das-vardecls))
					(vardecls (map (lambda (vardecl)
								  (let ((uname (car vardecl)))
								    (if (and (list? (cadr vardecl))
									     (eq? 'lambda (car (cadr vardecl))))
									(begin
									  (set! funclist (cons (list uname (cadr vardecl))
											       funclist))
									  `(,uname (rt-lambda-decl ,(cadr (cadr vardecl)))))
									`(,uname ,(fix newvarlist (cadr vardecl))))))
								
								das-das-vardecls)))
				   (apply remove-illegal-vars (map car das-das-vardecls))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(fix newvarlist (cadr funcdecl) #t)))
								 (reverse! funclist)))				      
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'letrec* (car term))
				 (let* ((newvarlist varlist)
					(funclist '())
					(vardecls (map (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl))))
								    (set! newvarlist (cons (list (car vardecl) uname) newvarlist))
								    (if (and (list? (caddr vardecl))
									     (eq? 'lambda (car (cadr vardecl))))
									(begin
									  (set! funclist (cons (list uname (cadr vardecl))
											       funclist))
									  `(,uname (rt-lambda-decl ,(cadr (cadr vardecl)))))
									`(,uname ,(fix newvarlist (cadr vardecl))))))
								das-vardecls)))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(fix newvarlist (cadr funcdecl) #t)))
								 (reverse! funclist)))
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))))))))
	      

	     (else
	      (if (not (symbol? (car term)))
		  (check-failed "Illegal function call:" term ".")
		  (let* ((funcname (let ((var (assq (car term) varlist)))
				     (if var
					 (cadr var)
					 (car term))))
			 (args (map (lambda (t)
				      (fix varlist t))
				    (cdr term))))
			 
		    (cons funcname args))))))
     
     (rt-print2 "fix-term:" term)
     (let ((ret (fix '() term #t)))
       (rt-print2 "fixed term" ret)
       ;;(rt-print2 "renamed:" renamed-guile-vars)
       (list (map (lambda (var)
		    (list (cadr var) (car var)))
		  renamed-guile-vars)
	     ret)))))
     

#!
(rt-fix-various '(lambda ()
		   (letrec ((a (lambda (b)
				 b)))
		     (a 2))))

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
;; rt-insert-types insert types for let* and lambda
;;
;; Note that at this stage of compilation, all let/let*/letrec/letrec*s have been converted to let*s,
;; and all variable-names are unique.
;;
;; (let* ((a 0)) (set! a 5)) -> (let* ((a <int> 0)) (set! a 5))
;; (let* ((a 0)) (set! a 5.0)) -> (let* ((a <float> 0)) (set! a 5.0))
;; (let* ((a 0)) (set! a (inexact->exact 5.0))) -> (let* ((a <int> 0)) (set! a (inexact->exact 5.0)))
;;
;; rt-let-lifter* must have been called on the term before calling.
;; 
;; (Warning, ugly ugly ugly code)
;; 
;; Note, during the insert-type-process, lambdas look like this: (lambda ((a <int>) (b <float>)...)...),
;; not like this: (lambda ((<int> a)(<float> b)...)...), which is the case everywhere else.
;;
(define (rt-insert-types term renamed-vars)
  (call-with-current-continuation
   (lambda (return)

     (define (debug . rest)
       (if #f
	   (apply rt-print2 rest)))
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-insert-types:" args))
       (return #f))

     
     ;; The type for let*-variables are just determinded once by checking the value of its type. However, some special care
     ;; has been taken for values where the type can not be determined right away. (I have a feeling the scheme is a bit too intelligent though...)
     ;; This hash-table contains all let*-variables as keys and theire type as value.
     (define let*-variables (make-hash-table 251))

     ;; This hash-table contains declared variables as keys, and theire type as value.
     (define declared-variables (make-hash-table 251))
     
     (define external-vars '())

     
     (define (merge-types type1 type2 term) ;; Term is only used to display understandable error-message.
       (debug "merge-types" type1 type2 term)
       (cond ((eq? type1 type2)
	      type1)
	     ((eq? type1 '<undefined>)
	      type2)
	     ((eq? type2 '<undefined>)
	      type1)

	     ((or (eq? type1 '<void>)
		  (eq? type2 '<void>))
	      '<void>)
	     
	     ((or (list? type1)
		  (list? type2))
	      (if (or (not (list? type1))
		      (not (list? type2))
		      (not (equal? (list (car type1)
					 (map (lambda (t)
						(if (list? t)
						    (cadr t)
						    t))
					      (cadr type1)))
				   (list (car type2)
					 (map (lambda (t)
						(if (list? t)
						    (cadr t)
						    t))
					      (cadr type2))))))
		  (check-failed type1 "and" type2 "are not compatible types. in term" term)
		  type1))
	     
	     ((and (rt-is-number? type1)
		   (rt-is-number? type2))
	      (if (or (eq? type1 '<double>)
		      (eq? type2 '<double>))
		  '<double>
		  (if (or (eq? type1 '<float>)
			  (eq? type2 '<float>))
		      '<float>
		      '<int>)))
	     (else
	      (if (or (not (symbol? type1))
		      (not (symbol? type2)))
		  (check-failed "Very error in rt-insert-types/merge-types" type1 type2 "term: " term))
	      (let ((rt-type1 (hashq-ref rt-types type1))
		    (rt-type2 (hashq-ref rt-types type2)))
		(if (not rt-type1)
		    (check-failed type1 "is not a legal type(1):" term))
		(if (not rt-type2)
		    (check-failed type2 "is not a legal type(2):" term))
		(if (-> rt-type1 type-ok? type2)
		    (-> (-> rt-type1 get-most-spesific-type type2) rt-type)
		    (if (-> rt-type2 type-ok? type1)
			(-> (-> rt-type2 get-most-spesific-type type1) rt-type)
			(check-failed "Incompatitble types for \"" type1 "\" and \"" type2 "\" in term \"" term "\".")))))))

     (define (check-compatible-types type1 type2 term)
       (merge-types type1 type2 term))



     ;; (set! a b) -> a and b have equal types, and are store in the hash-table below.
     ;; (set! a b) will make an empty hash-table that looks like this if a and b are undefined:
     ;; ((#:key a #:val (list (list 'b) '<undefined>))
     ;;  (#:key b #:val (list (list 'a) '<undefined>)))
     (define equal-typed-variables (make-hash-table 251))

     (define (add-equal-variables! name1 name2 type1 type2 term) ;; Term is only used for error-output
       (let* ((equal1 (hashq-ref equal-typed-variables name1))
	      (equal2 (hashq-ref equal-typed-variables name2))
	      (merged-type (merge-types type1 type2 term)))

	 (if (not equal1)
	     (begin
	       (set! equal1 (list (list name2) merged-type))
	       (hashq-set! equal-typed-variables name1 equal1)))
	 
	 (if (not equal2)
	     (begin
	       (set! equal2 (list (list name1) merged-type))
	       (hashq-set! equal-typed-variables name2 equal2)))
	       
	 (for-each (lambda (e)
		     (let ((equal (hashq-ref equal-typed-variables e)))
		       (set-car! equal (cons name2 (cons name1 (car equal))))
		       (set-car! (cdr equal) merged-type)))
		   (append (car equal1) (car equal2)))))
     

     (define (set-equal-type! name newtype term)
       (let* ((equal (hashq-ref equal-typed-variables name)))
	 (if equal
	     (let ((merged-type (merge-types newtype (cadr equal) term)))
	       (for-each (lambda (e)
			   (let ((equal (hashq-ref equal-typed-variables e)))
			     (set-car! (cdr equal) merged-type)))
			 (car equal))
	       merged-type)
	     newtype)))
	      


     (define* (add-external-var varname type #:optional iswriting)
       (let ((old (assq varname external-vars)))
	 (if old
	     (if (eq? type '<undefined>)
		 (-> (cadr old) rt-type)
		 (let ((rt-type (cadr old)))
		   (if (not (-> rt-type type-ok? type))
		       (set-car! (cdr old)
				 (hashq-ref rt-types (merge-types type (-> rt-type rt-type)
								  (format #f " Different types for guile variable \"~A\": \"~A\"/\"~A\"."
									  varname type (-> rt-type rt-type) ))))
		       (set-car! (cdr old) (-> rt-type get-most-spesific-type type)))
		   (set-car! (cddr old) (or iswriting (caddr old)))
		   (-> (cadr old) rt-type)))
	     (let ((rt-type (hashq-ref rt-types type)))
	       (if (not rt-type)
		   (check-failed "Unknown type " type ".")
		   (set! external-vars (cons (list varname rt-type iswriting (let ((orgname (assq varname renamed-vars)))
									       (if orgname
										   (cadr orgname)
										   varname)))
					     external-vars)))
	       type))))
     
     
     ;; The types for lambda-blocks and let*-blocks must be inserted before get-returntype is called.
     ;;
     ;; follow-variable is a variable that always has the same type as term.
     ;;   In "(let* ((a (lambda (b) b))))", "a" is the follow-variable for the term "(lambda (b) b)".
     ;; follow-variable is only used in case the returntype can not be determined now ('<undefined>), which in case its added to the equal-variables hash-table.
     ;;
     (define* (get-returntype varlist term #:optional follow-variable)
       (debug "get-returntype for " term "varlist:" varlist)
       (let ((ret (cond ((symbol? term)
			 (let ((avar (assq term varlist)))
			   (if avar
			       (if (and (= 3 (length avar))
					(list? (caddr avar))
					(eq? 'lambda (car (caddr avar))))
				   (list (cadr avar) (map list-copy (cadr (caddr avar)))) ;; Return-type is a lambda-function.
				   (begin
				     (if (and follow-variable
					      (eq? '<undefined> (cadr avar)))
					 (begin
					   (debug "follow-equal for" follow-variable (car avar) (cadr avar))
					   (add-equal-variables! follow-variable (car avar) '<undefined> '<undefined> term)
					   ))
				     (cadr avar)))
			       (add-external-var term '<undefined>))))
			
			((number? term)
			 (if (exact? term)
			     '<int>
			     '<float>))
			
			((string? term)
			 '<char-*>)
			
			((or (not (list? term)) (null? term))
			 (check-failed "Unable to determine type for \"" term "\"."))
			
			((memq (car term) rt-unknown-ret-type-ops)
			 (let ((ret '<undefined>))
			   (for-each (lambda (t)
				       (set! ret (merge-types ret
							      (get-returntype varlist t follow-variable)
							      term)))
				     (cdr term))
			   ret))
			
			;; let*
			((eq? 'let* (car term))
			 (get-returntype (append (cadr term) varlist)
					 (last (cddr term))))
			
			;; lambda
			((eq? 'lambda (car term))
			 (get-returntype (append (cadr term) varlist)
					 (last (cddr term))
					 follow-variable))
			
			;; begin
			((eq? 'rt-begin (car term))
			 (get-returntype varlist (last term)))

			;; while
			((eq? 'rt-while (car term))
			 '<void>)
			
			;; if
			((eq? 'rt-if (car term))
			 (merge-types (get-returntype varlist (caddr term) follow-variable)
				      (get-returntype varlist (cadddr term) follow-variable)
				      term))
			
			;; is-type?
			((eq? 'is-type? (car term))
			 '<int>)
			
			;; the
			((eq? 'the (car term))
			 (check-compatible-types (cadr term)
						 (get-returntype varlist (caddr term) follow-variable)
						 term)
			 (cadr term))
			
			(else
			 (let ((func (assq (car term) varlist)))
			   (rt-print2 "got func for " (car term) ":" func)
			   (if func
			       (if (and (list? (cadr func))
					(or (= 2 (length func))
					    (not (list? (caddr func)))))
				   (car (cadr func))
				   (cadr func))
			       (let ((func (hashq-ref rt-funcs (car term))))
				 (if func
				     (-> func return-type)
				     (check-failed "Unknown function(2) \"" (car term) "\": " term)))))))))
	 (debug "Get returntype was " ret " for term" term)
	 ret))
       


     ;; This one also returns the type.
     (define (set-type-in-! var type term)
       (debug "set-type-in-!" var type term)

       (let* ((varname (car var))
	      (vartype (cadr var))
	      (let*-variable (hashq-ref let*-variables varname)))
	 
	 (if (and let*-variable
		  (not (eq? '<undefined> let*-variable)))
	     (begin
	       (set! type let*-variable)
	       (set! vartype let*-variable)))
	 
	 (cond ((hashq-ref declared-variables var)
		(hashq-ref declared-variables var)) ;; Nothing can overo the declared type of a variable. This is the type.
	       
	       (else
		(let ((vartype (if (and (= 3 (length var))
					(list? (caddr var))) ;; A lambda function.
				   (list vartype (map list-copy (cadr (caddr var))))
				   vartype)))
		  (let ((ret (merge-types vartype
					  type
					  term)))
		    (set-car! (cdr var) ret)
		    
		    (if let*-variable
			(hashq-set! let*-variables varname ret))
		    (debug "set to" ret)
		    ret))))))

     ;; Set the type for a lambda*-variable. (I'm not sure this works properly, or what is supposed to work at all.)
     (define (set-type-in-lambda! var type term)                       ;; Term is only used to make understandable error-output
       (debug "in lambda" var type term)
       (set-type-in-! var type term))

     (define* (set-type! varlist name type term #:optional iswriting)  ;; Term is only used to make understandable error-output
       ;; Set the type for a variable in a varlist
       (define (set-type-in-varlist! var type term)                       ;; Term is only used to make understandable error-output 
	 ;;(debug "set-type-in-varlist" var type term)                  ;; (Hmmm, this function is equal to set-type-in-lambda!...)
	 (set-type-in-! var type term))

       ;;(debug "set-type! name/type/term" name type term)
       (let ((merged-type (set-equal-type! name type term)))
	 (let ((var (assq name varlist)))
	   (if (not var)
	       (add-external-var name type iswriting)
	       (set-type-in-varlist! var merged-type term)))))
       
     
     
     ;; Check-call checks correct types for function-call.
     ;; This is actually the else-block for the cond-block in the insert function.
     (define (check-call varlist term)
       (debug "check-call" term)
       (let ((funcname (car term)))
	 (if (not (symbol? funcname))
	     (check-failed "Illegal term: " term)
	     
	     (let ((func (assq (car term) varlist)))
	       

	       (if func

		   ;; Local function
		   (let ((name (car func))
			 (functype (if (list? (caddr func))
				       ;;(list (cadr func) (map list-copy (cadr (caddr func))))
				       (list (cadr func) (cadr (caddr func)))
				       (cadr func)))
			 (returntype #f)
			 (args #f))

		     ;;(debug "functype " functype)

		     (if (or (not (list? functype))
			     (= 1 (length functype)))
			 (check-failed "Local variable \"" funcname "\" is not a function:" term ". func:" func))

		     (set! returntype (car functype))
		     (set! args (cadr functype))
		     
		     (if (not (= (length args)
				 (- (length term) 1)))
			 (check-failed "Illegal number of argumentes to local function \"" funcname "\":" term))
		     
		     ;; Then check arg-types.
		     (let ((ret (map (lambda (t)
						(insert varlist t))
					      term)))

		       (cons (car ret) (map (lambda (t funcarg)
					      (let ((argtype (cadr funcarg))
						    (ret-type (get-returntype varlist t)))
						(if (and (eq? '<SCM> ret-type)
							 (not (eq? '<SCM> argtype)))
						    (begin
						      `(,(-> (hashq-ref rt-types argtype) c-transformfunc) ,t))
						    (begin
						      (set-type-in-lambda! funcarg ret-type term)
						      (debug "after lambda-set:" funcarg varlist)
						      (if (symbol? t)
							  (begin
							    (set-type! varlist t argtype term)))
						      t))))
					    (cdr term)
					    args))))
		   
		 
		   ;; Global function
		   (let ((func (hashq-ref rt-funcs funcname)))
		     (if (not func)
			 (check-failed "Unknown function \"" funcname "\": " term)
			 (if (not (-> func legal-number-of-arguments? term))
			     (return #f)
			     (let ((ret (map (lambda (t)
							(insert varlist t))
						      term)))
			       (cons (car ret) (map (lambda (t argtype)
						      (let ((ret-type (get-returntype varlist t)))
							;;(debug "ret-type" ret-type)
							(if (and (eq? '<SCM> ret-type)
								 (not (eq? '<SCM> argtype))
								 (not (eq? '<SCM> (-> (hashq-ref rt-types argtype) supertype))))
							    `(,(-> (hashq-ref rt-types argtype) c-transformfunc) ,t)
							    (begin
							      (check-compatible-types argtype ret-type term)
							      (if (symbol? t)
								  (set-type! varlist t argtype term))
							      t))))
						    (cdr ret)
						    (list-tabulate (length (cdr term))
								   (lambda (i)
								     (-> func arg-type i))))))))))))))

     (define (get-external-varlist)
       (map (lambda (v) (list (car v)
			      (-> (cadr v) rt-type)))
	    external-vars))

     (define (insert varlist term)
       (debug "insert" term " - " varlist)
       (cond ((not (list? term)) term)
	     ((null? term) term)


	     ;;; LET*
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'let* (car term))
	      (let* ((newvarlist varlist)
		     (lambda_decls '())
		     (vardecls (map (lambda (var)
					       (let ((ret (if (list? (cadr var))
							      (if (eq? 'rt-lambda-decl (car (cadr var)))
								  ;; A lambda declaration. (as the result of a letrec function)
								  (let ((ret (list (car var) '<undefined>
										   (append (list (car (cadr var)))
											   (list (map (lambda (t)
													(list t '<undefined>))
												      (car (cdr (cadr var)))))))))
								    (set! lambda_decls (cons ret lambda_decls))
								    ret)

								  ;; A lambda function
								  (let* ((name (car var))
									 (body (cadr var))
									 (type #f)
									 (decl (cadr body))
									 (ifdecl (assq name lambda_decls))) ;; Check if there is a lambda declaration
								    (if ifdecl
									(begin
									  ;; Fill in types from the declaration.
									  (set! decl (map (lambda (v1 v2)
											    (list v1 (cadr v2)))
											  decl
											  (cadr (caddr ifdecl))))))
								    (set! body (insert newvarlist `(lambda ,decl ,@(cddr body))))
								    (set! type (get-returntype newvarlist body))
								    (list name type body)))
							      (if (rt-symbol-starts-with? (car var) '_rt_breakcontsig)
								  (begin
								    (list (car var) '<jmp_buf> 0)
								    )
								  (list (car var) (cadr var) 0)))))
						 (set! newvarlist (cons ret newvarlist))
						 ret))
					     (cadr term)))
		     (body (map (lambda (t)
				  (insert newvarlist t))
				(cddr term))))

		(debug "1vardecls" vardecls)
		(debug "1newvarlist" newvarlist)

		;; Fix up equal typed and declared variables.
		(set! vardecls (map (lambda (var)
				      (let ((decl (hashq-ref declared-variables (car var))))
					(if decl
					    (list (car var) decl (if (= 3 (length var))
								     (caddr var)
								     0))
					    (let ((equal (hashq-ref equal-typed-variables (car var))))
					      (if equal
						  (let* ((name (car var))
							 (def (cadr equal)))
						    (if (and (list? (cadr equal))
							     (list? (caddr var)))
							(let* ((ret-type (car def))
							       (arg-types (map cadr (cadr def))))
							  `(,name ,ret-type (lambda ,(map (lambda (v1 v2)
											    (list (car v2) v1))
											  arg-types
											  (cadr (caddr var)))
									      ,@(cddr (caddr var)))))
							(list (car var) (cadr equal) (if (= 3 (length var))
											 (caddr var)
											 0))))
						  var)))))
				    vardecls))

		;; Make another attempt to determine return-type for lambda-functions. A function like this: "(lambda (a) a)" can sometimes
		;; not determine its return-type right away because the type for a is not always known at the evaluation time for the function.
		(set! vardecls
		      (let ((newvarlist varlist))
			(map (lambda (var)
					(let ((ret (if (and (eq? '<undefined> (cadr var))
							    (list? (caddr var))
							    (eq? 'lambda (car (caddr var))))
						       (let ((type (get-returntype newvarlist (caddr var))))
							 (if (eq? '<undefined> type)
							     (check-failed "Unable to determine return-type for function" (car var)
									   ". (Perhaps you need to use the \"the\" operator?)"))
							 (list (car var) type (caddr var)))
						       var)))
					  (set! newvarlist (cons ret newvarlist))
					  ret))
				      vardecls)))
						     
		;; Fill in lambda-decls types
		(set! vardecls (map (lambda (var)
					       (let ((decl (assq (car var) lambda_decls)))
						 (if decl
						     (let ((real (assq (car var) (reverse vardecls))))
						       (set! lambda_decls (delete decl lambda_decls))
						       ;;(debug "var/real" (car var) real (reverse vardecls))
						       `(,(car var) ,(cadr real) (rt-lambda-decl ,(cadr (caddr real)))))
						     var)))
					     vardecls))
		
		(let ((vardecls (remove (lambda (var)
					  (eq? (cadr var) '<undefined>))
					vardecls)))
		  (rt-print2 "vardecls" vardecls)
		  (rt-print2 "newvarlist" newvarlist)
		  (if (null? vardecls)
		      `(rt-begin
			 ,@body)
		      `(let* ,vardecls
			 ,@body)))))

	     
	     ;; LAMBDA
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'lambda (car term))
	      (let* ((vardecls (map (lambda (var)
				      (if (list? var)
					  var
					  (list var '<undefined>)))
				    (cadr term)))
		     (body (map (lambda (t)
				  (insert (append vardecls varlist) t))
				(cddr term))))

		;; Set correct type for equal typed and declared variables.
		(set! vardecls (map (lambda (var)
				      (let ((decl (hashq-ref declared-variables (car var))))
					(if decl
					    (list (car var) decl)
					    (let ((equal (hashq-ref equal-typed-variables (car var))))
					      (if equal
						  (list (car var) (cadr equal))
						  var)))))
				    vardecls))
		
		`(lambda ,vardecls
		   ,@body)))

	     
	     ;; SET! and RT-SET*! (rt-set*! is the result of let*-lifting and needs special treatment, because the type can't change when rt-set*!).
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ((or (eq? 'set! (car term))
		  (eq? 'rt-set*! (car term)))
	      (let* ((ret-term (insert varlist (caddr term)))
		     (type (get-returntype varlist ret-term))
		     (newtype  (if (eq? 'rt-set*! (car term))
				   type
				   (get-returntype varlist (cadr term)))))

		(if (eq? 'rt-set*! (car term))
		    (hashq-set! let*-variables (cadr term) type))
		
		(if (and (eq? '<SCM> type)
			 (not (or (eq? '<SCM> newtype)
				  (eq? '<undefined> newtype))))
		    (begin
		      `(set! ,(cadr term) (,(-> (hashq-ref rt-types newtype) c-transformfunc) ,ret-term)))
		    (begin
		      (set-type! varlist (cadr term) type term #t)
		      (if (symbol? (caddr term))
			  (begin
			    (add-equal-variables! (cadr term) (caddr term) newtype type term)
			    ;;(debug "setting type for" (caddr term) "to" newtype)
			    (set-type! varlist (caddr term) newtype term)))
		      `(set! ,(cadr term) ,ret-term)))))
	     

	     
	     ;; DECLARE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'declare (car term))
	      (for-each (lambda (decl)
			  (let* ((type (if (eq? (car decl) 'type) (cadr decl) (car decl)))
				 (vars (if (eq? (car decl) 'type) (cddr decl) (cdr decl)))
				 (rt-type (hashq-ref rt-types type)))
			    (if (not rt-type)
				(check-failed type "is not a known type in expression" term))
			    (for-each (lambda (var)
					(hashq-set! declared-variables var type)
					(set-type! varlist var type term)
					)
				      vars)
			    ))
			(cdr term))
	      '(rt-dummy/dummy))
	      

	     ;; THE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'the (car term))
	      (let* ((c (insert varlist (caddr term)))
		     (ret-type (get-returntype varlist c)))
		(if (and (eq? '<SCM> ret-type)
			 (not (eq? '<SCM> (cadr term))))
		    `(the ,(cadr term) (,(-> (hashq-ref rt-types (cadr term)) c-transformfunc) ,c))
		    `(the ,(cadr term) ,c))))


	     ;; IS-TYPE?
             ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'is-type? (car term))
	      term)

	     ;; BEGIN
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(map (lambda (t)
					    (insert varlist t))
					  (cdr term))))

	     
	     ;; (EVERYTHING) ELSE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     (else
	      (check-call varlist term))))

     
     (let ((ret (insert '() term)))


       (for-each (lambda (var)
		   (let ((decl (hashq-ref declared-variables (car var))))
		     (if decl
			 (set-car! (cdr var) (hashq-ref rt-types decl))
			 (let ((equal (hashq-ref equal-typed-variables (car var))))
			   (if equal
			       (set-car! (cdr var) (hashq-ref rt-types (cadr equal))))))))
		 external-vars)
       
       (let ((extnumbers '())
	     (extpointers '())
	     (extnumbers-writing '())
	     (returntype (get-returntype '() ret)))
	 (for-each (lambda (extvar)
		     (if (caddr extvar)
			 (set! extnumbers-writing (cons extvar extnumbers-writing))
			 (if (rt-is-number? (-> (cadr extvar) rt-type))
			     (set! extnumbers (cons extvar extnumbers))
			     (set! extpointers (cons extvar extpointers)))))
		   external-vars)
	 (debug "wrting:" extnumbers-writing)
	 (debug "renamed-vars" renamed-vars)
	 (list ret
	       returntype
	       extnumbers extpointers extnumbers-writing))))))



(define (rt-insert-types2 term)
  (let ((ret (rt-insert-types term '())))
    (if ret
	(list (car ret)
	      (cadr ret)
	      (map (lambda (v) (list (car v)
				     (-> (cadr v) rt-type)))
		   (append (caddr ret) (cadddr ret) (cadr (cdddr ret))))))))


#!

(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda ()
				 9))
			    (b <undefined>))
		       (rt-set*! b a)
		       (b))))


(rt-insert-types2 '(lambda ()
		     (let* ((ai (lambda (a)
				  5)))
		       (ai 2))))

(rt-insert-types2 '(lambda ()
		     (let* ((a <float>))
		       (rt-set*! a 5.2)
		       (declare (<int> a))
		       a)))
 
(define a (rt-2 '(lambda ()
		   (let ((a 2))
		     (let ((gakk (lambda ()
				   (if (< a 10)
				       (set! a (1+ a))))))
		       (gakk))
		     a))))


(rt-insert-types2 '(lambda ()
		     (let* ((a 0))
		       (rt-set*! a 9)
		       (< a 7))))
(rt-insert-types2 '(lambda (a b)
		     (rt-if 1
			    (set! a 9)
			    (set! a 3.9))
		     (set! b 2.3)
		     (+ a b)))
		      
(rt-insert-types2 '(lambda (n1)
	 (let* ((fib (rt-lambda-decl (n2)))
		(fib (lambda (n2)
		       (rt-if (< n2 2)
			      n2
			      (+ (fib (rt--/- n2 1))
				 (fib (rt--/- n2 2)))))))
	   (fib n1))))

(rt-2 '(lambda (n1)
	 (letrec ((fib (lambda (n2)
			 (rt-if (< n2 2)
				(+ a n2)
				(+ (fib (rt--/- n2 1))
				   (fib (rt--/- n2 2)))))))
	   (fib n1))))

(lambda ((<float> n1_u1))
  (let* ((fib_u3 <float> (lambda ((<float> n2_u4))
			   (return
			    (rt-if (< n2_u4 2)
				   n2_u4
				   (+ (fib_u2 (rt--/- n2_u4 1))
				      (fib_u2 (rt--/- n2_u4 2))))))))
    (return
     (rt-begin
      (fib_u3 n1_u1)))))

(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda ()
				 (rt-if 1
					(* 2 3)
					(+ 2 3)))))
		       (a))))

(define a (rt-2 '(lambda ()
		   (let* ((a (lambda (c)
			       ;;(declare (<int> c))
			       (the <char-*> "adsf")))
			  (d (lambda (e)
			       ;;(declare (<double> e))
			       "gakk"))
			  (b a))
		     (set! b d)
		     (b 7)
		     (+ 5 9)
		     ))))
(rt-funcall a)

(lambda (n)
  (letrec ((fib (lambda (n)
		  (rt-if (< n 2)
				n
				(+ (fib (rt--/- n 1))
				   (fib (rt--/- n 2)))))))
    (fib n)))




(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda (c)
				 ;;(declare (<int> c))
				 (the <char-*> "adsf")))
			    (d (lambda (e)
				 ;;(declare (<double> e))
				 "gakk"))
			    (b 0))
		       (set! b a)
		       (set! b d)
		       (b 7)
		       (+ 5 9)
		       )))

(rt-insert-types '(lambda ()
		    (let* ((fib (rt-lambda-decl (n)))
			   (ai (lambda (g)
				 (declare (<double> g))
				 (fib 6)
				 (set! g 9)
				 ;;(set! g (fib 3))
				 (set! a 9)))
			   (fib (lambda (n)
				  (declare (<int> n))
				  (the <double>
				       (rt-if (< n 2)
						      n
						      (+ (fib (rt--/- n 1))
							 (fib (rt--/- n 2)))))))
			   )
		      (ai 3)
		      ;;(fib 5)
		      ))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a 0))
		      (set! a (+ a 3))))
		 '())

(rt-insert-types '(lambda (a b)
		    (let* ((c 0)
			   (d 0))
		      (declare (<double> a b c f))
		      (+ a b c f)))
		 '())
		    
(rt-insert-types '(lambda ()
		    (let* ((a 0))
		      ;;(+ a (rt-vector-ref/vector-ref vec 3))
		      (set! a (rt-vector-ref/vector-ref vec 4))
		      (+ a 9)
		      (set! b a)
		      ))
		 
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a (lambda (b)
				(+ 1 b))))
		      (+ 2 (a (rt-vector-ref/vector-ref vec 2)))))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((d 0)
			   ;;(a_u1 (rt-lambda-decl (b_u3)))
			   (a_u1 (lambda (b_u2)
				   c_u2)))
		      (a_u1 (+ c_u2 2.9))
		      ))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a 0)
			   ;;(f 0)
			   (b (lambda (r2 r3)
				;;(set! r2 a)
				;;(+ a 2)
				;;(+ r2 6)
				(rt-mus-channels/mus_channels r2)
				(rt-oscil/mus_oscil r2 3 4)
				)))
		      (set! f (rt-vector-ref/vector-ref vec 9))
		      (set! a (b d 9))
		      (set! a 9)))
		 '())
  
(rt-insert-types '(lambda ()
		    (let* ((a (lambda (b)
				;;8
				;;(+ b 4)
				b
				)))
		      (+ 1 (a 9))
		      ))
		 '())
!#
		   
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-check-syntax does the following:
;; *various checks that that the term is a legal expression.
;;
;; rt-expand-macros must have been called on the term before calling.
;; 
(define (rt-check-syntax term)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-check-calls:" args))
       (return #f))
     

     (define (check-calls term)
       ;;(c-display "check-calls varlist/term" varlist term)
       (cond ((not (list? term)) #t)
	     ((null? term) #t)

	     ;; RT_WHILE/WHILE
	     ;((eq? 'rt-while (car term))
	     ; (check-calls varlist (cadr term))
	     ; (check-calls varlist (caddr term)))
	      
	     ;; RT-LAMBDA_DECL/LAMBDA_DECL	
	     ((eq? 'rt-lambda-decl (car term))
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
					(if (not (symbol? varname))
					    (check-failed "Illegal variable \"" varname "\" in lambda-form: " term ".")))
				      (cadr term))
			    (for-each check-calls (cddr term)))))))
	     
	     ;; SET!
	     ((eq? 'set! (car term))
	      (if (not (symbol? (cadr term)))
		  (check-failed "Illegal set!(2) term: " term)
		  (if (not (= 3 (length term)))
		      (check-failed "Illegal set!(3) term: " term))))

	     ;; BEGIN
	     ((eq? 'rt-begin (car term))
	      (if (null? (cdr term))
		  (check-failed "begin needs a body: " term ".")
		  (for-each check-calls (cdr term))))
	     
	     ;; LET*
	     ((eq? 'let* (car term))
	      (for-each (lambda (var)
			  (if (not (symbol? (cadr var)))
			      (check-calls (cadr var))))
			(cadr term))
	      (for-each check-calls (cddr term)))
	     
	     ;; IF
	     ((eq? 'rt-if (car term))
	      (if (< (length term) 3)
		  (check-failed "To few arguments for if:" term ".")
		  (if (> (length term) 4)
		      (check-failed "To many arguments for if:" term ".")))
	      (check-calls (cadr term))
	      (check-calls (caddr term))
	      (if (= (length term) 4)
		  (check-calls (cadddr term))))
	     
	     (else
	      (for-each check-calls term))))
     
     (if (not (eq? 'lambda (car term)))
	 (check-failed "This is not a lambda function: " term))

     (rt-print2)
     (rt-print2 "check-calls term" term)
     (rt-print2)
     
     (check-calls term))))


#!
((<float> c (lambda ((<float> d)) (+ a b)))
 (<float> b 5)
 (<float> a))

(rt-check-calls '(lambda ()
		   (rt-if a
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

(define rt-types (make-hash-table 251))

;; rt-type         -> Name used in rt
;; checkfunc       -> Guile-function to check if type is correct.
;; c-transformfunc -> Name of a rt-function (or macro) that converts an SCM version of the type to something that can be used in rt.
;;                    (For example when using vector-ref, list-ref, car, cdr, etc.)
;;                    This function must call rt-error if the variable can not be converted to a compatible type.
;; transformfunc   -> A Guile function that is run on the variable before putting it to rt. Type is already checked with checkfunc.
;; error-message   -> ???
;; c-type          -> Name of the type on the c-side. Usually the same as rt-type.
;; suptype-of      -> This type can be used for all situation which the type for "subtype-of" is compatible with.

(def-class (<rt-type> rt-type checkfunc c-transformfunc #:key transformfunc error-message (c-type rt-type) subtype-of)
  (def-method (rt-type)
    rt-type)
  (def-method (c-type)
    c-type)
  (def-method (supertype)
    subtype-of)
  (def-method (c-transformfunc)
    c-transformfunc)
  (def-method (check type)
    (if checkfunc
	(checkfunc type)
	(eq? type rt-name)))

  (define compatible-types '())
  (def-method (add-compatible-type type)
    (set! compatible-types (cons type compatible-types)))
  
  (def-method (type-ok? type)
    ;;(c-display "type/rt-typ/compatibele-types" type rt-type compatible-types)
    (or (member type (cons rt-type compatible-types))
	(and subtype-of
	     (-> (hashq-ref rt-types subtype-of) type-ok?
		 type))))

  ;; Don't use.
  (def-method (get-most-compatible-type othertype)
    (let ((other ((hashq-ref rt-types othertype))))
      (if (eq? this othertype)
	  this
	  #f)))
	       
  ;; We assume rt-type and type are compabible.
  (def-method (get-most-spesific-type type)
    (if (eq? type rt-type)
	this
	(if (eq? subtype-of type)
	    this
	    (hashq-ref rt-types type))))

  (def-method (transform var #:optional das-add-extra-gc-var-func)
    (let ((ret (if (not (this->check var))
		   (begin
		     (c-display "rt-compiler/<rt-type>. Wrong type. \"" var "\" is not a" rt-type ".")
		     (throw 'wrong-type))
		   (if transformfunc
		       (transformfunc var)
		       var))))
      (if (and (list? ret)
	       (eq? 'extra-gc-var (car ret)))
	  (begin
	    (if das-add-extra-gc-var-func
		(das-add-extra-gc-var-func (cadr ret)))
	    (caddr ret))
	  ret)))
  
  (hashq-set! rt-types rt-type this)

  (if subtype-of
      (-> (hashq-ref rt-types subtype-of) add-compatible-type
	  rt-type))
  
  )

;; Never called!
(define (c-nevercalled-true? . something)
  (c-display "Error. What the? c-nevercalled-true? is never supposed to be called. Arguments:" something)
  #f)


(define-c-macro (rt-mus-any?/mus_xen_p scm)
  (if (rt-is-safety?)
      `(?kolon (mus_xen_p ,scm)
	       (XEN_TO_MUS_ANY ,scm)
	       (begin_p
		(rt_error rt_globals (string "Variable is not a CLM generator."))
		NULL))
      `(XEN_TO_MUS_ANY ,scm)))

(begin
  (<rt-type> '<double>  number? 'rt_scm_to_double)
  (<rt-type> '<float> number? 'rt_scm_to_float) ;;  #:subtype-of <double>)
  (<rt-type> '<int> number? 'rt_scm_to_int)   ;;  #:subtype-of <float>)
  (<rt-type> '<char-*> string? 'rt_scm_to_error) ;; Function does not exist
  (<rt-type> '<vct-*> vct? 'rt_scm_to_vct #:transformfunc TO_VCT)
  ;;(<rt-type> '<vector> vector? #f #:c-type '<SCM>) 
;;  (<rt-type> '<mus_any-*> c-nevercalled-true? 'rt-mus-any?/mus_xen_p)
  (<rt-type> '<mus_any-*> c-nevercalled-true? 'rt_scm_to_mus_any)
  (<rt-type> '<void-*> c-nevercalled-true? #f)
  (<rt-type> '<undefined> (lambda x
			    (c-display "Warning, unused variable with value:" x)
			    #t)
	     #f #:c-type '<SCM>)
  (<rt-type> '<void> c-nevercalled-true? #f)
  (<rt-type> '<SCM> (lambda (t) #t) #f)
  (<rt-type> '<jmp_buf> c-nevercalled-true? #f)
  ;;(<rt-type> '<pair> pair? #f #:c-type '<SCM> #:subtype-of '<SCM>)      ;; Some checking is needed here. I don't think this is safe.
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rt-funcs (make-hash-table 251))


(def-class (<rt-func> name returntype args #:key min-arguments max-arguments is-immediate needs-rt-globals)
  (define last-type (if (null? args)
			#f
			(last args)))

  (def-method (legal-number-of-arguments? term)
    (if (and max-arguments
	     (>= (1- (length term)) max-arguments))
	(begin
	  (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected maximum"
		     max-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	  #f)
	(if (and min-arguments
		 (< (1- (length term)) min-arguments))
	    (begin
	      (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected minimum"
			 min-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	      #f)
	    (if (and (not min-arguments)
		     (not max-arguments)
		     (not (= (1- (length term)) (length args))))
		(begin
		  (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected"
			     (length args) "arguments for" (car term) ". Found" (- (length term) 1) ".")
		  #f)
		#t))))
  
  (def-method (needs-rt-globals)
    needs-rt-globals)
  
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

  (if is-immediate
      (rt-add-immediate name))
  
  (hashq-set! rt-funcs name this)

  )

(begin

  ;; Functions defined directly. (Note that most functions are defined indirectly using the rt-renamefunc macro.)
  
  ;; Basic
  (<rt-func> '+ '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt--/- '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt--/minusoneargument '<float> '(<float>))
  (<rt-func> '* '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt-/// '<double> '(<float>) #:min-arguments 2)
  
  (<rt-func> '1+ '<float> '(<float>))
  (<rt-func> '1- '<float> '(<float>))

  (<rt-func> 'rt-min/MIN '<float> '(<float> <float>))
  (<rt-func> 'rt-max/MAX '<float> '(<float> <float>))

  (<rt-func> 'set! '<void> '(<float> <float>))
  
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
  (<rt-func> 'rt-if '<float> '(<int> <float> <float>)) ;; Special form, only return-type is checked
  (<rt-func> 'rt-begin '<float> '(<float>) #:min-arguments 1) ;; Special form, only return-type is checked
  (<rt-func> 'rt-lambda-decl '<float> '())
  (<rt-func> 'rt-while '<void> '(<int> <float>) #:min-arguments 1)
  (<rt-func> 'rt-break/break '<void> '(<int>))
  (<rt-func> 'rt-break/return '<void> '(<float>))
  (<rt-func> 'rt-contbreakvar/jmp_buf '<void> '())
  (<rt-func> 'rt-setjmp/setjmp '<int> '(<jmp_buf>))
  (<rt-func> 'rt-break/longjmp '<void> '(<jmp_buf>))
  (<rt-func> 'rt-continue/longjmp '<void> '(<jmp_buf>))
  (<rt-func> 'rt-printf/fprintf '<int> '(<char-*> <float>) #:min-arguments 1)

  (<rt-func> 'rt_scm_to_int '<int> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt_scm_to_float '<float> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt_scm_to_double '<double> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt_scm_to_vct '<vct-*> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt-mus-any?/mus_xen_p '<mus_any-*> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt_scm_to_mus_any '<mus_any-*> '(<SCM>) #:needs-rt-globals #t)
  (<rt-func> 'rt_in '<float> '(<int>) #:needs-rt-globals #t)
  (<rt-func> 'rt_out '<void> '(<int> <float>) #:needs-rt-globals #t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-rt-macro def . body)
  (define (is-expand varname)
    (let ((s (symbol->string varname)))
      (if (and (> (string-length s) 7)
	       (string= "expand/" s 0 7 0 7))
	  (string->symbol (string-drop s 7))
	  #f)))
  (if (dotted-list? def)
      (let* ((das-last (cdr (last-pair def)))
	     (exp (is-expand das-last)))
	(if exp
	    `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def))
	       (let ((,exp (map rt-expand-macros ,das-last)))
		 ,@body))
	    `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def))
	       ,@body)))
      (let* ((name (car def))
	     (new-name (symbol-append rt-macro-prefix name))
	     (expand-args '())
	     (args (cdr def))
	     (clean-args '())
	     (optionals '())
	     (rest (rt-gensym))
	     (min-args (rt-gensym))
	     (max-args (rt-gensym))
	     (return (rt-gensym)))
	(for-each (lambda (arg)
		    (if (list? arg)
			(set! optionals (append! optionals (list (list (car arg) (keyword->symbol (car arg)) (cadr arg)))))
			(let ((exp (is-expand arg)))
			  (if exp
			      (set! expand-args (append! expand-args (list exp))))))
		    (set! clean-args (append! clean-args (list (if (list? arg) (keyword->symbol (car arg)) arg)))))
		  args)
	`(define-macro ,(cons new-name rest)
	   (call-with-current-continuation
	    (lambda (,return)
	      (let ((,min-args ,(- (length clean-args) (length optionals)))
		    (,max-args ,(length args))
		    ,@(map (lambda (varname)
			     (list varname #f))
			   clean-args)
		    ,@(map (lambda (expvarname)
			     (list expvarname #f))
			   expand-args))
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

		(let ,(map (lambda (expvarname)
			     `(,expvarname (rt-expand-macros ,(symbol-append 'expand/ expvarname))))
			   expand-args)
		  ,@body))))))))
	    

(define-macro (rt-renamefunc rt-name c-name returntype args)
  (let ((funcname (symbol-append 'rt- rt-name '/ c-name)))
    (<rt-func> funcname returntype args)
    (primitive-eval `(define-c-macro ,(cons funcname 'rest )
		       `(,',c-name ,@rest)))
    `(define-rt-macro ,(cons rt-name 'rest)
       `(,',funcname ,@rest))))


(rt-renamefunc rt-error rt_error <void> (<char-*>))



(define-c-macro (the type somethingmore)
  `(cast ,type ,somethingmore))

(define-rt-macro (inexact->exact z)
  `(the <int> ,z))

(define-rt-macro (exact->inexact z)
  `(the <float> ,z))

(define-rt-macro (exact? expand/var)
  (cond ((number? var) (if (exact? var)
			   1
			   0))
	((rt-immediate? var)
	 `(is-type? <int> ,var))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (is-type? <int> ,s))))))

(define-rt-macro (inexact? var)
  `(not (exact? ,var)))

(define-rt-macro (number? var)
  (cond ((number? var) 1)
	((rt-immediate? var)
	 `(or (is-type? <int> ,var)
	      (is-type? <float> ,var)
	      (is-type? <double> ,var)))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (or (is-type? <int> ,s)
		  (is-type? <float> ,s)
		  (is-type? <double> ,s)))))))

(define-rt-macro (string? var)
  (cond ((stringr? var) 1)
	((rt-immediate? var)
	 `(is-type? <char-*> ,var))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (is-type? <char-*> ,s))))))
  
(define-rt-macro (- firstarg . rest)
  (if (null? rest)
      `(rt--/minusoneargument ,firstarg)
      `(rt--/- ,firstarg ,@rest)))
(define-c-macro (rt--/minusoneargument a)
  `(- ,a))
(define-c-macro (rt--/- . rest)
  `(- ,@rest))


(rt-renamefunc expt pow <float> (<float> <float>))
(rt-renamefunc abs fabsf <float> (<float>))

(rt-renamefunc floor floorf <float> (<float>))
(rt-renamefunc ceiling ceilf <float> (<float>))

(rt-renamefunc logand & <int> (<int> <int>))
(rt-renamefunc logior | <int> (<int> <int>))
(rt-renamefunc lognot ~ <int> (<int>))
  
(rt-renamefunc remainder % <int> (<int> <int>))


;; < > <= >= =
(for-each (lambda (op)
	    (let ((rt-op (symbol-append 'rt- op)))
	      (<rt-func> rt-op '<int> '(<float> <float>))
	      (primitive-eval `(define-c-macro ,(cons rt-op 'rest)
				 `(,',op ,@rest)))
	      (primitive-eval `(define-rt-macro (,op expand/a expand/b)
				 (if (and (number? a)
					  (number? b))
				     (if (,op a b)
					 1
					 0)
				     `(,',rt-op ,a ,b))))))
	  '(< > <= >= =))

(define-c-macro (rt-= a b)
  `(== ,a ,b))


(define-rt-macro (zero? expand/z)
  (if (number? z)
      (if (= 0 z) 1 0)
      `(= 0 ,z)))

(define-rt-macro (positive? expand/z)
  (if (number? z)
      (if (> z 0) 1 0)
      `(> ,z 0)))

(define-rt-macro (negative? expand/z)
  (if (number? z)
      (if (< z 0) 1 0)
      `(< ,z 0)))


  
;; modulo-logic picked up from snd-run.c
(define-rt-macro (modulo a b)
  (let ((x (rt-gensym))
	(y (rt-gensym))
	(z (rt-gensym)))
    `(let* ((,x ,a)
	    (,y ,b)
	    (,z (remainder ,x ,y)))
       (if (or (and (negative? ,y)
		    (positive? ,z))
	       (and (positive? ,y)
		    (negative? ,z)))
	   (+ ,z ,y)
	   ,z))))

(define-c-macro (rt-/// . rest)
  `(/ ,@rest))
(define-rt-macro (quotient a b)
  `(rt-/// (the <int> ,a) (the <int> ,b)))
(define-rt-macro (/% a b)
  `(quotient ,a ,b))
(define-rt-macro (/ a b)
  `(rt-/// (the <float> ,a) (the <float> ,b)))

(define-rt-macro (odd? n)
  `(remainder ,n 2))
(define-rt-macro (even? n)
  `(not (odd? ,n)))


	 
;; truncate-logic picked up from snd-run.c
(define-rt-macro (truncate expand/a)
  (if (rt-immediate? a)
      `(if (negative? ,a)
	   (- (floor (- ,a)))
	   (floor ,a))
      (let ((x (rt-gensym))  )
	`(let ((,x ,a))
	   (if (negative? ,x)
	       (- (floor (- ,x)))
	       (floor ,x))))))

;; round-logic picked up from snd-run.c (I'm not sure this one works correctly, because of the "/"-casting)
(define-rt-macro (round a)
  (let ((plus_half (rt-gensym))
	(result (rt-gensym)))
    `(let* ((,plus_half (+ ,a 0.5))
	    (,result (floor ,plus_half)))
       (if (and (= ,plus_half ,result)
		(not (= (/ ,plus_half 2) (floor (/ ,plus_half 2)))))
	   (- ,result 1)
	   ,result))))
       

;;logxor-logic picked up from snd-run.c
(define-rt-macro (logxor expand/x expand/y)
  (if (and (number? x)
	   (number? y))
      (logxor x y)
      (if (rt-immediate? x y)
	  `(logand (lognot (logand ,x ,y))
		   (logior ,x ,y))
	  (let ((a (rt-gensym))
		(b (rt-gensym)))
	    `(let* ((,a ,x)
		    (,b ,y))
	       (logand (lognot (logand ,a ,b))
		       (logior ,a ,b)))))))


;;ash-logic picked up from snd-run.c
(define-rt-macro (ash expand/a expand/b)
  (if (and (number? a)
	   (number? b))
      (ash a b)
      (if (rt-immediate? a b)
	  `(if (>= ,b 0)
	       (rt-ash/<< ,a ,b)
	       (rt-ash/>> ,a (- ,b)))	
	  (let ((arg1 (rt-gensym))
		(arg2 (rt-gensym)))
	    `(let* ((,arg1 ,a)
		    (,arg2 ,b))
	       (if (>= ,arg2 0)
		   (rt-ash/<< ,arg1 ,arg2)
		   (rt-ash/>> ,arg1 (- ,arg2))))))))
(define-c-macro (rt-ash/<< a b)
  `(<< ,a ,b))
(define-c-macro (rt-ash/>> a b)
  `(>> ,a ,b))


(rt-renamefunc random mus_frandom <float> (<float>))
(rt-renamefunc random mus_irandom <float> (<float>))
(define-rt-macro (random expand/val)
  `(if (exact? ,val)
       (rt-random/mus_irandom ,val)
       (rt-random/mus_frandom ,val)))


(define-rt-macro (max . expand/rest)
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
			(if (rt-immediate? arg)
			    (begin
			      (set! number-args (cons arg number-args ))
			      (set! new-args (cons arg new-args)))
			    (begin
			      (set! rest-args (cons arg rest-args))
			      (set! varnames (cons (rt-gensym) varnames))
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

(define-rt-macro (min . expand/rest)
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
			(if (rt-immediate? arg)
			    (begin
			      (set! number-args (cons arg number-args ))
			      (set! new-args (cons arg new-args)))
			    (begin
			      (set! rest-args (cons arg rest-args))
			      (set! varnames (cons (rt-gensym) varnames))
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

  
(define-rt-macro (and . expand/rest)
  (define (expand ret rest)
    (if (null? rest)
	ret
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  0
		  (expand var (cdr rest)))
	      (if (rt-immediate? var)
		  `(if ,var
		       ,(expand var (cdr rest))
		       0)
		  (let ((varname (rt-gensym)))
		    `(let* ((,varname ,var))
		       (if ,varname
			   ,(expand varname (cdr rest))
			   0))))))))

  (expand 1 rest))


(define-rt-macro (or . expand/rest)
  (define (expand rest)
    (if (null? rest)
	0
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  (expand (cdr rest))
		  var)
	      (if (rt-immediate? var)
		  `(if ,var
		       ,var
		       ,(expand (cdr rest)))
		  (let ((varname (rt-gensym)))
		    `(let* ((,varname ,var))
		       (if ,varname
			   ,varname
			   ,(expand (cdr rest))))))))))

  (expand rest))


;; if is a macro, rt-if is a special form
(define-rt-macro (if a b . c)
  (if (> (length c) 1)
      (begin
	(apply c-display (append (list "Too many arguments for if:" a b) c))
	#f)
      (let ((ae (rt-expand-macros a)))
	(if (number? ae)
	    (if (= 0 ae)
		(if (null? c)
		    '(rt-dummy/dummy)
		    (car c))
		b)
	    (if (null? c)
		`(rt-if ,ae ,b (rt-dummy/dummy))
		`(rt-if ,ae ,b ,(car c)))))))

(define-c-macro (rt-if . rest)
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
  (let ((das-key (rt-gensym)))
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

;; begin and begin_p are macros, while rt-begin is a special form
(define-rt-macro (begin . rest)
  `(begin_p ,@rest))

(define-rt-macro (begin_p . rest)
  `(rt-begin ,@rest))

(define-c-macro (rt-begin . rest)
  `(begin_p ,@rest))

(define-c-macro (rt-lambda-decl rest)
  `(lambda ,rest decl))



;(define-rt-macro (while test . body)
;  (let ((whilefunc (rt-gensym))
;	(dasfunc (rt-gensym)))
;    `(let* ((,whilefunc (lambda ()
;			 (let* ((_rt_breakcontsig 0)
;				(,dasfunc (lambda ()
;					    (rt-while ,test
;						      (begin
;							,@body)))))
;			   (if (< (rt-setjmp/setjmp _rt_breakcontsig) 2)
;			       (,dasfunc))))))
;       (,whilefunc))))

(define-rt-macro (while test . body)
  (define (rec-search term)
    (call-with-current-continuation
     (lambda (return)
       (define (search term)
	 (cond ((eq? '_rt_breakcontsig term)
		(return #t))
	       ((list? term)
		(for-each search term))))
       (search term)
       #f)))
  (let ((test (rt-expand-macros test))
	(body (map rt-expand-macros body)))
    (if (rec-search (cons test body))
	`(let* ((_rt_breakcontsig 0))
	   (if (< (rt-setjmp/setjmp _rt_breakcontsig) 2)
	       (rt-while ,test
			 ,@body)))
	`(rt-while ,test
		   ,@body))))

(define-c-macro (rt-while test . body)
  `(while ,test ,@body))
(define-c-macro (rt-setjmp/setjmp das-sig)
  `(setjmp ,das-sig))

(define-rt-macro (break)
  `(rt-break/longjmp _rt_breakcontsig))
(define-c-macro (rt-break/longjmp das-sig)
  `(longjmp ,das-sig 2))
(define-rt-macro (continue)
  `(rt-continue/longjmp _rt_breakcontsig))
(define-c-macro (rt-continue/longjmp das-sig)
  `(longjmp ,das-sig 1))



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

(<rt-func> 'rt-add-int! '<int> '(<int> <int>))
(define-c-macro (rt-add-int! var inc)
  (<-> (eval-c-parse var)
       (if (number? inc)
	   (if (= 1 inc)
	       "++"
	       (if (= -1 inc)
		   "--"
		   (<-> "+="  (eval-c-parse inc))))
	   (<-> "+="  (eval-c-parse inc)))))

(define-rt-macro (range varname start end . body)
  (let ((das-end (rt-gensym))
    	(das-add (rt-gensym)))
    (if (list? varname)
	(begin
	  (set! das-add (cadr varname))
	  (set! varname (car varname))))
    (if (or (number? das-add)
	    (and (number? start)
		 (number? end)))
	(let ((das-end end)
	      (das-add (if (number? das-add)
			   das-add
			   (if (> end start) 1 -1))))
	  `(let* ((,varname ,start))
	     (declare (<int> ,varname))
	     (while (not (= ,varname ,das-end))
		    ,@body
		    (rt-add-int! ,varname ,das-add))))
	`(let* ((,das-end ,end)
		(,varname ,start))
	   (declare (<int> ,das-end ,varname))
	   (if (> ,das-end ,start)
	       (while (not (= ,varname ,das-end))
		      ,@body
		      (rt-add-int! ,varname 1))
	       (while (not (= ,varname ,das-end))
		      ,@body
		      (rt-add-int! ,varname -1)))))))

(define-rt-macro (rt-range2 varname start end inc . body)
  `(range ,(list varname inc) ,start ,end ,@body))

  
#!
(rt-funcall (rt-2 '(lambda (s)
		     (range i -2 0
			    (printf "%f\\n" (+ s i)))
		     (range i 5 10
			    (printf "%f\\n" (+ s i)))))
	    5)

!#

;; This is bad. Return-values from continuations shouldn't be limited to floats only. (Type is set immediately for let-variables)
;; void-returning functions aren't supported.
(define-rt-macro (call-with-current-continuation func)
  (let ((res (rt-gensym))
	(thunk (rt-gensym)))
    `(let* ((_rt_breakcontsig 0)
	    (,res 0.0)            
	    (,(caadr func) (lambda (retval)
			     (set! ,res retval)
			     (break)))
	    (,thunk (lambda ()
		      ,@(cddr func))))
       (if (= (rt-setjmp/setjmp _rt_breakcontsig) 0)
	   (set! ,res (,thunk)))
       ,res)))

	    
(define-rt-macro (printf string . rest)
  `(rt-printf/fprintf ,string ,@rest))
(define-c-macro (rt-printf/fprintf string . rest)
  `(fprintf stderr ,string ,@rest))
			  

;; let is implemented as a macro to easier be able to support named let. The real name for non-named let is rt-let/let*
(define-rt-macro (let a b . c)
  (if (not (symbol? a))
      `(rt-let/let* ,a ,b ,@c)
      `(letrec ((,a (lambda ,(map car b)   ;; Named let
		      ,@c)))
	 (,a ,@(map cadr b)))))


(<rt-func> 'rt-dummy/dummy '<void> '())
(define-c-macro (rt-dummy/dummy)
  "/* */")



;; Oh, horror. This implemenation is ugly...
;; '(a b c) -> '( (a b c) (a c) (a b) (a) (b c) (b) (c) ())
(define (permutate alist)
  (cond ((null? alist)
	 (list '()))
	((<= (length alist) 1)
	 (list alist '()))
	(else
	 (let ((ret (list (list (car alist))))
	       (alist2 (list (list (last alist)))))
	   (for-each (lambda (s)
		       (set! alist2 (cons (cons s (car alist2))
					  alist2)))
		     (cdr (reverse (cdr alist))))
	   ;;(set! alist2 (append! alist2 (list '())))
	   (for-each (lambda (s rest)
		       (set! ret (append ret (permutate rest)))
		       (for-each (lambda (r)				
				   (set! ret (cons (cons s r)
						   ret)))
				 (permutate rest)))
		     alist
		     alist2)
	   (delete-duplicates! ret)))))


#!
(begin
  (define a-val a)
  (define b-val b)
  (cond ((and (rt-immidiate? a)
	      (rt-immidiate? b))
	 (let ((a (gensym))
	       (b (gensym)))
	   ...))
	((and (rt-immidiate? a)
	      (not (rt-immidiate? b)))
	 (let ((a (gensym)))
	   ...))
	((and (not (rt-immidiate? a))
	      (rt-immidiate? b))
	 (let ((b (gensym)))
	   ...))
	(else
	 (begin
	   ...))))
!#

(define-macro (rt-automate-immediate . rest)
  (let* ((vars (c-butlast rest))
	 (tempvars (map (lambda (var)
			  (cons var (rt-gensym)))
			vars))
	 (body (last rest))
	 (perm (permutate vars)))
    (c-display "vars/body" vars body perm)
    `(cond ,@(map (lambda (immediates)
		    (if (null? immediates)
			`(else ,body)
			`( (and ,@(map (lambda (var)
					 (if (member var immediates)
					     `(rt-immediate? ,var)
					     `(not (rt-immediate? ,var))))
				       vars))
			   (let ,(append (map (lambda (im)
						`(list ,(cdr (assq im tempvars)) ,im))
					      immediates)
					 (map (lambda (im)
						`(list ,im `(rt-gensym)))
					      immediates))
			     `(let ,(map (lambda (im)
					   (c-display "tempvars:" ,tempvars)
					   (let ((n (cdr (assq im ,tempvars))))
					     (list im n)))
					 ',immediates)
				,,body)))))
		  perm))))

#!
(define-macro (ai a b)
  (rt-automate-immediate a
			 `(+ ,a ,b)))

(macroexpand '(ai 9 3))

(macroexpand '(rt-automate-immediate a
				     `(+ 2 5)))
(begin `,`(+ 2 3))

(cond ((and (rt-immediate? a))
       (let ((rt_gen45 a)
	     (a (rt-gensym)))
	 (quasiquote (let (unquote (map (lambda (im)
					  (let ((n (cdr (assq im ((a . rt_gen45))))))
					    (quasiquote (list (unquote im) (unquote n)))))
					(quote (a))))
		       (unquote (quasiquote (+ 2 5)))))))
      (else
       (quasiquote (+ 2 5))))


(cond ((and (rt-immediate? a))
       (let ((rt_gen24 a)
	     (a (rt-gensym))
	     (varnames (map (lambda (im)
			      (let ((n (cdr (assq im tempvars))))
				(list im n)))
			    (a))))
	 (quasiquote (let (unquote (map (lambda (s) (list (car s) (cadr s))) varnames)) (unquote (quasiquote (+ (unquote a) 5))))))) (else (quasiquote (+ (unquote a) 5))))

(let ((newbody `(+ ,a 5)))
  (cond ((and (rt-immediate? a))
	 (let ((rt_gen152 a)
	       (a (rt-gensym))
	       (varnames ((a rt_gen152))))
	   `(let ,(map (lambda (s)
			 (list (car s) (cadr s)))
		       varnames)
	      ,newbody)))
	(else
	 `(+ ,a 5)))))
!#


#!

(define-rt-macro (vector-ref expand/vec expand/pos)
  (if (rt-is-safety?)
      (rt-automate-immediate vec pos
			     `(begin
				(if (not (vector? ,vec))
				    (rt-error "Operation vector-ref failed because first argument is not a vector."))
				(if (>= ,pos (vector-length ,vec))
				    (rt-error "Operation vector-ref failed because the length of the vector is to small"))
				(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
      `(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
!#


;; VECTORS

(rt-renamefunc vector? SCM_VECTORP <int> (<SCM>))
(rt-renamefunc vector-length SCM_VECTOR_LENGTH <int> (<SCM>))

(define-rt-macro (vector-ref expand/vec expand/pos)
  (if (rt-is-safety?)
      (if (not (rt-immediate? vec pos))
	  (let ((das-vec (rt-gensym))
		(das-pos (rt-gensym)))
	    `(let ((,das-vec ,vec)
		   (,das-pos ,pos))
	       (if (not (vector? ,das-vec))
		   (rt-error "Operation vector-ref failed because first argument is not a vector."))
	       (if (>= ,das-pos (vector-length ,das-vec))
		   (rt-error "Operation vector-ref failed because the length of the vector is to small"))
	       `(rt-vector-ref/vector-ref ,das-vec (rt-castint/castint ,das-pos))))
	  `(begin
	     (if (not (vector? ,vec))
		 (rt-error "Operation vector-ref failed because first argument is not a vector."))
	     (if (>= ,pos (vector-length ,vec))
		 (rt-error "Operation vector-ref failed because the length of the vector is to small"))
	     (rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
      `(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))

(define-c-macro (rt-vector-ref/vector-ref vec pos)
  `(SCM_VECTOR_REF ,vec ,pos))
(<rt-func> 'rt-vector-ref/vector-ref '<SCM> '(<SCM> <int>))



;; PAIRS and LISTS

(rt-renamefunc rt-car SCM_CELL_OBJECT_0 <SCM> (<SCM>))
(rt-renamefunc rt-cdr SCM_CELL_OBJECT_1 <SCM> (<SCM>))

(rt-renamefunc pair? SCM_CONSP <int> (<SCM>))
(rt-renamefunc null? SCM_NULLP <int> (<SCM>))


(define-rt-macro (car expand/p)
  (if (not (rt-is-safety?))
      `(rt-car ,p)
      (if (rt-immediate? p)
	  `(begin
	     (if (not (pair? ,p))
		 (rt-error "Operation CAR failed because p is not a pair. (1)"))
	     (rt-car ,p))
	  (let ((s (rt-gensym)))
	    `(let ((,s ,p))
	       (if (not (pair? ,s))
		   (rt-error  "Operation CAR failed because p is not a pair. (2)"))
	       (rt-car ,s))))))
	    
(define-rt-macro (cdr expand/p)
  (if (not (rt-is-safety?))
      `(rt-cdr ,p)
      (if (rt-immediate? p)
	  `(begin
	     (if (not (pair? ,p))
		 (rt-error  "Operation CDR failed because p is not a pair. (1)"))
	     (rt-cdr ,p))
	  (let ((s (rt-gensym)))
	    `(let ((,s ,p))
	       (if (not (pair? ,s))
		   (rt-error "Operation CDR failed because p is not a pair. (2)"))
	       (rt-cdr ,s))))))
	   
(define-rt-macro (cadr p)
  `(car (cdr ,p)))
(define-rt-macro (caddr p)
  `(car (cdr (cdr ,p))))
(define-rt-macro (cadddr p)
  `(car (cdr (cdr (cdr ,p)))))
(define-rt-macro (caddddr p)
  `(car (cdr (cdr (cdr (cdr ,p))))))
(define-rt-macro (cddr p)
  `(cdr (cdr ,p)))
(define-rt-macro (cdddr p)
  `(cdr (cdr (cdr ,p))))
(define-rt-macro (cddddr p)
  `(cdr (cdr (cdr (cdr ,p)))))
(define-rt-macro (cdddddr p)
  `(cdr (cdr (cdr (cdr (cdr ,p))))))
(define-rt-macro (cdadr p)
  `(cdr (car (cdr ,p))))
(define-rt-macro (cdaddr p)
  `(cdr (car (cdr (cdr ,p)))))
(define-rt-macro (cdadddr p)
  `(cdr (car (cdr (cdr (cdr ,p))))))
(define-rt-macro (caar p)
  `(car (car ,p)))
(define-rt-macro (caadr p)
  `(car (car (cdr ,p))))
(define-rt-macro (caaddr p)
  `(car (car (cdr (cdr ,p)))))
(define-rt-macro (caadddr p)
  `(car (car (cdr (cdr (cdr ,p))))))

;; For large n's, this one is a cache-killer, I guess.
(define-rt-macro (list-ref das-list n)
  (define (help n)
    (if (= 0 n)
	das-list
	`(cdr ,(help (1- n)))))
  `(car ,(help n)))


;; rt-insert-types is not designed correctly, therefore the declares of the lists and the ,funcname stuff. Shouldn't have been necesarry.
(define-rt-macro (for-each func . lists)
  (let ((lnames (map (lambda (n) (rt-gensym)) (iota (length lists))))
	(funcname (rt-gensym)))
    `(let ((,funcname (lambda ,(cadr func)
			,@(map (lambda (llist)
				 `(declare (<SCM> ,llist)))
			       (cadr func))
			,@(cddr func)))
	   ,@(map (lambda (lname llist)
		    (list lname llist))
		  lnames
		  lists))
       ,@(map (lambda (llist)
		`(declare (<SCM> ,llist)))
	      lists)
       (while (and ,@(map (lambda (lname)
			    `(not (null? ,lname)))
			  lnames))
	      (,funcname ,@(map (lambda (lname)
				  `(car ,lname))
				lnames))
	      ,@(map (lambda (lname)
		       `(set! ,lname (cdr ,lname)))
		     lnames)))))
    

#!
(define l '(1 2 3 4))
(define a (rt-2 '(lambda ()
		   (for-each (lambda (n)
			       (printf "%f\\n" (+ 100 n)))
			     l))))
(rt-funcall a)
!#
		  

;;; OUT/IN

(define-rt-macro (out . expand/rest)
  (let ((channels (c-butlast rest))
	(val (last rest)))
    (if (null? channels)
	(set! channels '(0 1)))
    (if (= 1 (length channels))
	`(rt_out ,(car channels) ,val)
	(if (rt-immediate? val)
	    `(begin
	       ,@(map (lambda (ch)
			`(rt_out ,ch ,val))
		      channels))
	    (let ((varname (rt-gensym)))
	      `(let* ((,varname ,val))
		 ,@(map (lambda (ch)
			  `(rt_out ,ch ,varname))
			channels)))))))

(define-c-macro (rt-outs/outs n)
  (<-> "rt_globals->outs[" (eval-c-parse n) "]"))
(define-c-macro (rt-set-outs! n val)
  (<-> "rt_globals->outs[" (eval-c-parse n) "]=" (eval-c-parse val)))
(define-c-macro (rt-num_outs/num_outs)
  "rt_globals->num_outs")


(define-rt-macro (in . channels)
  (if (null? channels)
      (set! channels '(0 1)))
  (if (= 1 (length channels))
      `(rt_in ,(car channels))
      `(+ ,@(map (lambda (ch)
		   `(rt_in ,(car channels)))
		 channels))))

(define-c-macro (rt-ins/ins n)
  (<-> "rt_globals->ins[" (eval-c-parse n) "]"))
(define-c-macro (rt-num_ins/num_ins)
  "rt_globals->num_ins")


(<rt-func> 'rt-outs/outs '<float> '(<int>))
(<rt-func> 'rt-set-outs! '<void> '(<int> <float>))
(<rt-func> 'rt-num_outs/num_outs '<int> '() #:is-immediate #t)

(<rt-func> 'rt-ins/ins '<float> '(<int>))
(<rt-func> 'rt-num_ins/num_ins '<int> '() #:is-immediate #t)

  
  

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
  (let ((pthread (rt-gensym))
	(das-func (rt-gensym)))
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
			    (locsig       (input)) ;; redefined later to use out instead of out-any
			    (notch        (input (pm 0)))
			    (one-pole     (input))
			    (one-zero     (input))
			    (oscil        ((fm 0) (pm 0)))
			    ;;(out-any      (
			    (polyshape    ((index 1) (fm 0)))
			    (phase-vocoder (input-function analyze-function edit-function synthesize-function)) ;; redefined later
			    (pulse-train   ((fm 0)))
			    (rand          ((sweep 0)))
			    (rand-interp   ((sweep 0)))
			    (readin        ())       ;; Lots of things redefined later
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
		  (<rt-type> etype testfunc c-transformfuncname #:c-type '<mus_any-*> #:transformfunc XEN_TO_MUS_ANY #:subtype-of '<mus_any-*>))
	      (<rt-func> macroname '<float> (cons etype (map (lambda (a) '<float>) args)))
	      (primitive-eval `(define-rt-macro (,name osc ,@args1 . rest)
				 (if (> (length rest) ,(length args2))
				     (begin
				       (c-display "rt-macro, too many arguments for " ',name ":" rest)
				       #f)
				     (let ((n -1))
				       (append (list ',macroname osc)
					       (list ,@args1)
					       (map (lambda (arg)
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
				       (if (rt-is-safety?)
					   `(?kolon (&& (mus_xen_p ,scm)
							(,',c-transformfuncname2 (XEN_TO_MUS_ANY ,scm)))
						    (XEN_TO_MUS_ANY ,scm)
						    (begin_p
						     (rt_error rt_globals (string "Variable is not a CLM generator (2)"))
						     NULL))
					   `(XEN_TO_MUS_ANY ,scm))))
		    (<rt-func> c-transformfuncname etype '(<SCM>))
		    
		    ))))

	  rt-clm-generators)


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
		   (funcname (if is-setter
				 (symbol-append rt-name '/ c-name)
				 (symbol-append 'rt- rt-name '/ c-name))))
	      (<rt-func> funcname returntype args)
	      (if (or (eq? name 'set_closure)
		      (eq? name 'closure))
		  (begin
		    (rt-print "c-name" c-name)
		    (rt-print "funcname" funcname)
		    (rt-print "rt-name" rt-name)))
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
	    (<void-*> environ ())
	    (<void-*> set_environ (<void-*>) #t)
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
	    (<char-*> file_name ())
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
;;;;;;;;;;;;;;
;; Can't use this one, because mus-srate might differ.
;;(rt-renamefunc hz->radians mus_hz_to_radians <float> (<float>))
;; This one should be fine: (Should probably compute w_rate and put it somewhere though.)
(define-rt-macro (hz->radians hz)
  (if (number? hz)
      (* hz (/ (* pi 2) (-> rt-engine samplerate)))
      `(* ,hz ,(/ (* pi 2) (-> rt-engine samplerate)))))

;; mus-srate
;(<rt-func> 'mus-srate '<float> '() #:is-immediate #t)
;(define-c-macro (mus-srate)
;  "rt_globals->samplerate")

(define-rt-macro (mus-srate)
  (-> rt-engine samplerate))


;; move-locsig
(rt-renamefunc move-locsig mus_move_locsig <void> (<mus_locsig-*> <float> <float>))


;, Locsig, or at least an attempt. I think its okey, but theres no reverb (The autogenerated locsig macro above is not working)
(define-rt-macro (locsig loc val)
  (let ((dasval (rt-gensym))
	(i (rt-gensym)))
    `(let ((,dasval ,val))
       (range ,i 0 (mus-channels ,loc)
	      (rt-set-locvals ,loc ,i ,dasval))
       (range ,i 0 (mus-channels (rt-get-loc-outf ,loc))
	      (out ,i (rt-get-float-val (mus-data (rt-get-loc-outf ,loc)) ,i))))))

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

(<rt-func> 'rt-get-environ '<void-*> '() #:is-immediate #t)
(define-c-macro (rt-get-environ)
  "(void*)rt_globals")

;; The marcros below aren't quite hygienic. Must fix.
(begin
  ;; Src needs special treatment as well.
  (define-rt-macro (src gen sr-change input-function)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret (rt-mus-src/mus_src ,das-gen ,sr-change (lambda (dir)
							      (declare (<int> dir))
							 (the <float> (,input-function dir)))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
  (<rt-func> 'rt-mus-src/mus_src '<float> '(<mus_src-*> <int> (<float> (<int>))))
  (define-c-macro (rt-mus-src/mus_src gen sr-change input-function)
    (<-> "mus_src(" (eval-c-parse gen) "," (eval-c-parse sr-change) ", (void*)" (eval-c-parse input-function) ")"))

  ;; Same for convolve
  (define-rt-macro (convolve gen input-function)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret `(rt-mus-convolve/mus_convolve ,das-gen (lambda (direction2)
							      (declare (<int> direction2))
							      (the <float> (,input-function direction2)))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))

  (<rt-func> 'rt-mus-convolve/mus_convolve '<float> '(<mus_convolve-*> (<float> (<int>))))
  (define-c-macro (rt-mus-convolve/mus_convolve gen input-function)
    (<-> "mus_convolve(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ")"))

  
  ;; And granulate
  (define-rt-macro (granulate gen input-function . rest)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))

	 ,(if (null? rest)
	      `(set! ,ret (rt-mus-granulate/mus_granulate ,das-gen
							  (lambda (arg2 direction2)
							    (declare (<int> direction2))
							    (the <float> (,input-function direction2)))))
	      `(set! ,ret (rt-mus-granulate/mus_granulate_with_editor ,das-gen
								      (lambda (direction2)
									(declare (<int> direction2))
									(the <float> (,input-function direction2)))
								      (lambda ()
									(the <int> (,(car rest)))))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
	 
  (<rt-func> 'rt-mus-granulate/mus_granulate '<float> '(<mus_granulate-*> (<float> (<int>))))
  (define-c-macro (rt-mus-granulate/mus_granulate gen input-function)
    (<-> "mus_granulate(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ")"))
  (<rt-func> 'rt-mus-granulate/mus_granulate_with_editor '<float> '(<mus_granulate-*> (<float> (<int>)) (<int> (<mus_any-*>))))
  (define-c-macro (rt-mus-granulate/mus_granulate_with_editor gen input-function edit-function)
    (<-> "mus_granulate_with_editor(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ",(void*)" (eval-c-parse edit-function) ")"))

  
  ;; And even phase-vocoder 
  (define-rt-macro (phase-vocoder gen input-function (#:edit-function #f) (#:synthesize-function #f))
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret (rt-mus-phase-vocoder/mus_phase_vocoder ,das-gen
							    (lambda (dir2)
							      (declare (<int> dir2))
							      (the <float> (,input-function dir2)))
							    ,(if edit-function
								 `(lambda ()
								    (the <int> (,edit-function)))
								 `(rt-mus-pv/NULL1))
							    ,(if synthesize-function
								 `(lambda ()
								    (the <float> (,synthesize-function)))
								 `(rt-mus-pv/NULL2))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
	 
  (<rt-func> 'rt-mus-phase-vocoder/mus_phase_vocoder '<float> '(<mus_phase-vocoder-*> (<float> (<int>)) (<int> ()) (<float> ())))
  (define-c-macro (rt-mus-phase-vocoder/mus_phase_vocoder gen input-function edit-function synthesize-function)
    (<-> "mus_phase_vocoder_with_editors(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function)
	 ",NULL,(void*)" (eval-c-parse edit-function) ",(void*)" (eval-c-parse synthesize-function) ")"))
  (<rt-func> 'rt-mus-pv/NULL1 '(<int>) '() #:is-immediate #t)
  (define-c-macro (rt-mus-pv/NULL1)
    "NULL")
  (<rt-func> 'rt-mus-pv/NULL2 '(<float> ()) '() #:is-immediate #t)
  (define-c-macro (rt-mus-pv/NULL2)
    "NULL")
	     
  )


;; Readin

(define-ec-struct <mus_rt_readin>
  <mus_any_class-*> core
  <void-*> readin_func ;; Pointer to the function rt_readin
  <void-*> buffer ;; Pointer to struct buffer.
  <int> channel
  <int> channels
  <off_t> location
  <float> increment
  ;;<char-*> file_name
  <off_t> length
  <mus_any-*> readin
  <SCM> scm_readin)


(eval-c (string-append "-I" snd-header-files-path " " (string #\`) "pkg-config --libs sndfile" (string #\`) )
	"#include <clm.h>"
	"#include <xen.h>"
	"#include <clm2xen.h>"
	"#include <sndfile.h>"

	(shared-struct <mus_rt_readin>)

	(define-struct <buffer>
	  <void-*> buffer
	  <struct-buffer-*> next
	  <int> num_frames
	  <int> num_visitors
	  <void-*> readin_raw_func
	  ;;(<float> (<char-*> <int>)) read_func
	  <char> filename[500])

  
	;;;;;; Buffer handling.
	;;;;;; A buffer is only freed if no one is using it. Perhaps it should never be freed at all?
	(<struct-buffer-*> buffers NULL)

	(<struct-buffer-*> find_buffer (lambda ((<char-*> filename))
					 (let* ((buffer <struct-buffer-*> buffers))
					   (while (not (== NULL buffer))
						  (if (not (strncmp buffer->filename filename 499))
						      (begin
							buffer->num_visitors++
							(return buffer)))
						  (set! buffer buffer->next))
					   (set! buffer (calloc 1 (sizeof <struct-buffer>)))
					   (strncpy buffer->filename filename 499)
					   (set! buffer->num_visitors 1)
					   (set! buffer->next buffers)
					   (set! buffers buffer)
					   (return buffer))))

	(<void> free_buffer (lambda ((<struct-buffer-*> buffer))
			      buffer->num_visitors--
			      (if (== 0 buffer->num_visitors)
				  (begin
				    (free buffer->buffer)
				    (set! buffer->buffer NULL)))))
	

	;;;;;;; rt-readin
	"typedef float (*Callback)(void *,int pos)"
	
	(<float> get_byte (lambda ((<char-*> data)(<int> pos))
			    (return (/ data[pos] 128.0f))))
	(<float> get_short (lambda ((<short-*> data)(<int> pos))
			    (return (/ data[pos] 32768.0f))))
	(<float> get_float (lambda ((<float-*> data)(<int> pos))
			     (return data[pos])))
	
	(<nonstatic-float> rt_readin (lambda ((<struct-mus_rt_readin-*> readin))
				       (let* ((buffer <struct-buffer-*> readin->buffer)
					      (callback <Callback> buffer->readin_raw_func)
					      (ret <float> (?kolon (or (< readin->location 0)
								       (>= readin->location readin->length))
								   0.0f
								   (callback buffer->buffer readin->location))))
					 ;;(fprintf stderr (string "dir: %d, pos: %d ret: %f\\n") dir pos ret)
					 (+= readin->location readin->increment)
					 (return ret))))
	
	(public
	 (<float> rt-readin (lambda ((<SCM> rt_readin_smob))
			      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				(return (rt_readin readin))))))
	
	
	;;;;;;; Das SMOB
	
	(<nonstatic-scm_t_bits> rt_readin_tag)
	 
	(public
	  (<SCM> rt-readin-p (lambda ((<SCM> rt_readin_smob))
			       (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
				   (return SCM_BOOL_T)
				   (return SCM_BOOL_F)))))
	 
	 (<SCM> mark_rt_readin (lambda ((<SCM> rt_readin_smob))
				 (let* ((rt_readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				   (return rt_readin->scm_readin))))
	 (<size_t> free_rt_readin (lambda ((<SCM> rt_readin_smob))
				    (let* ((rt_readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				      (free_buffer rt_readin->buffer)
				      (free rt_readin->core)
				      (free rt_readin)
				      (return 0))))
	 (<int> print_rt_readin (lambda ((<SCM> rt_readin_smob) (<SCM> port) (<scm_print_state-*> pstate))
				  (scm_puts (string "#<rt_readin ... > ") port)
				  (return 1)))
	 
	 (public

	   ;;;;;;; CLM methods for rt-readin
	  (<int> rt-readin-channels (lambda ((<SCM> rt_readin_smob))
				      (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					  (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					    (return readin->channels)))))
	  (<int> rt-readin-channel (lambda ((<SCM> rt_readin_smob))
				     (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					 (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					   (return readin->channel)))))
	  (<float> rt-readin-increment (lambda ((<SCM> rt_readin_smob))
					 (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					     (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					       (return readin->increment)))))
	  (<float> rt-readin-set_increment (lambda ((<SCM> rt_readin_smob)
						    (<float> val))
					     (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
						 (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						   (set! readin->increment val)
						   (return val)))))
	  (<int> rt-readin-length (lambda ((<SCM> rt_readin_smob))
				    (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					(let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					  (return readin->length)))))
	  (<char-*> rt-readin-file-name (lambda ((<SCM> rt_readin_smob))
					  (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						(return (mus_file_name "((struct mus_rt_readin *) readin)->readin"))))))
	  (<int> rt-readin-location (lambda ((<SCM> rt_readin_smob))
				      (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					  (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					    (return readin->location)))))
	  (<int> rt-readin-set_location (lambda ((<SCM> rt_readin_smob)						 
						 (<int> loc))
					  (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						(set! readin->location loc)
						(return loc)))))
	

	  (<SCM> make-rt-readin (lambda ((<SCM> scm_readin))
				  (let* ((readin <mus_any-*> (XEN_TO_MUS_ANY scm_readin))
					 (ret <struct-mus_rt_readin-*> (calloc 1 (sizeof <struct-mus_rt_readin>)))
					 (scmret <SCM>)
					 (filename <char-*> (mus_file_name readin))
					 (buffer <struct-buffer-*> (find_buffer filename))
					 (channel <int> (mus_channel readin)))
				    ;;(fprintf stderr (string "readin (make): %x\\n") ret)
				    
				   (set! ret->readin readin)
				   (set! ret->scm_readin scm_readin)
				   (set! ret->readin_func rt_readin)

				   (set! ret->buffer buffer)
				   ;;(fprintf stderr (string "readin (make), buffer: %x\\n") buffer)
				   
				   (if (== NULL buffer->buffer)
				       (let* ((sfinfo <SF_INFO>)
					      (sndfile <SNDFILE-*> (sf_open filename SFM_READ &sfinfo))
					      (framesize <int> 4)
					      (format <int> (& SF_FORMAT_SUBMASK sfinfo.format)))
					 
					 ;;(SCM_ASSERT (!= NULL sndfile) scm_readin 0 (string "make-rt-readin: Could not open file."))
					 (if (== NULL sndfile)
					     (begin
					       (printf (string "file not found\\n"))
					       (return SCM_UNDEFINED)))
					       
					 (cond ((== format SF_FORMAT_PCM_S8)
						(set! framesize 1)
						(set! buffer->readin_raw_func get_byte))
					       ((== format SF_FORMAT_PCM_U8)
						(set! framesize 1)
						(set! buffer->readin_raw_func get_byte))
					       ((== format SF_FORMAT_PCM_16)
						(set! framesize 2)
						(set! buffer->readin_raw_func get_short))
					       (else
						(set! buffer->readin_raw_func get_float)))
					 
					 (set! buffer->buffer (malloc (* framesize sfinfo.frames)))
					 (set! buffer->num_frames sfinfo.frames)
					 ;;(fprintf stderr (string "framesize: %d format: %d frames: %d\\n") framesize format buffer->num_frames)
					 (for-each 0 sfinfo.frames
						   (lambda (i)
						     (cond ((== framesize 1)
							    (if (== sfinfo.format SF_FORMAT_PCM_S8)
								(let* ((new[sfinfo.channels] <char>))
								  (sf_read_raw sndfile new 1)
								  (set! "((char *)buffer->buffer)[i]" new[channel]))
								(let* ((new[sfinfo.channels] <short>))
								  (sf_readf_short sndfile new 1)
								  (set! "((char *)buffer->buffer)[i]" (/ new[channel] 128)))))
							   ((== framesize 2)
							    (let* ((new[sfinfo.channels] <short>))
							      (sf_readf_short sndfile new 1)
							      (set! "((short *)buffer->buffer)[i]" new[channel])))
							   (else
							    (let* ((new[sfinfo.channels] <float>))
							      (sf_readf_float sndfile new 1)
							      (set! "((float *) buffer->buffer)[i]" new[channel]))))))
					 (sf_close sndfile)))

				   
				   (set! ret->channel   channel)
				   (set! ret->channels  (mus_channels readin))
				   (set! ret->location  0)
				   (set! ret->increment (mus_increment readin))
				   (set! ret->length    (mus_length readin))

				   (SCM_NEWSMOB scmret rt_readin_tag ret)
				   (return scmret)))))

	(run-now
	 (set! rt_readin_tag (scm_make_smob_type (string "rt_readin") (sizeof <struct-mus_rt_readin>)))
	 (scm_set_smob_mark rt_readin_tag mark_rt_readin)
	 (scm_set_smob_free rt_readin_tag free_rt_readin)
	 (scm_set_smob_print rt_readin_tag print_rt_readin)))



;;(<rt-type> '<rt-readin> rt-readin-p 'rt_scm_to_rt_readin #:transformfunc SCM_SMOB_DATA #:c-type '<struct-mus_rt_readin-*> #:subtype-of '<mus_any-*>)
(<rt-type> '<rt-readin>
	   (lambda (readin)
	     (or (readin? readin)
		 (rt-readin-p readin)))
	   'rt_scm_to_rt_readin
	   #:transformfunc (lambda (readin)
			     (if (rt-readin-p readin)
				 (SCM_SMOB_DATA readin)
				 (let ((rt-readin (make-rt-readin readin)))
				   (list 'extra-gc-var
					 rt-readin
					 (SCM_SMOB_DATA rt-readin)))))
	   #:c-type '<struct-mus_rt_readin-*>
	   #:subtype-of '<mus_any-*>
	   )

(<rt-func> 'rt_scm_to_rt_readin '<rt-readin> '(<SCM>) #:needs-rt-globals #t)

;;(<rt-func> 'rt_readin '<float> '(<rt-readin>))
(rt-renamefunc readin rt_readin <float> (<rt-readin>))

;(rt-renamefunc readin rt-c-dasreadin <float> (<rt-readin>))
;(define-c-macro (rt-c-dasreadin readin)
;  (<-> "((ReadinFunc) ( ((struct mus_rt_readin *)" (eval-c-parse readin) ")->readin_func)) ((struct mus_rt_readin *)" (eval-c-parse readin) ")"))
  

(for-each (lambda (gen)
	    (let* ((funcname (car gen))
		   (rettype (cadr gen))
		   (elname (if (= 3 (length gen))
			       (caddr gen)
			       funcname)))
	      (primitive-eval `(define-rt-macro (,(symbol-append 'mus- funcname) expand/gen)
				 (if (rt-immediate? gen)
				     `(if (is-type? <rt-readin> ,gen)
					  (,',(symbol-append 'rt-readin- funcname) ,gen)
					  (,',(symbol-append 'rt-mus- funcname '/mus_ funcname) ,gen))
				     (let ((g (rt-gensym)))
				       `(let ((,g ,gen))
					  (if (is-type? <rt-readin> ,g)
					      (,',(symbol-append 'rt-readin- funcname) ,g)
					      (,',(symbol-append 'rt-mus- funcname '/mus_ funcname) ,g)))))))
	      
	      ;;(<rt-func> (symbol-append 'rt-readin- funcname) rettype '(<rt-readin>) #:is-immediate #t)
	      (<rt-func> (symbol-append 'rt-readin- funcname) rettype '(<mus_any-*>) #:is-immediate #t)
	      (primitive-eval `(define-c-macro (,(symbol-append 'rt-readin- funcname) agen2)
				 (<-> (symbol->string ,'agen2) "->" ,(symbol->string elname))))))
	  '((channels <int>)
	    (channel <int>)
	    (increment <float>)
	    (length <int>)
	    (file-name <char-*> file_name)
	    (location <int>)))


(define-rt-macro (setter!-mus-location expand/gen expand/val)
  (if (rt-immediate? gen val)
      `(if (is-type? <rt-readin> ,gen)
	   (rt-readin-set_location ,gen ,val)
	   (setter!-mus-location/mus_set_location ,gen ,val))
      (let ((g (rt-gensym))
	    (g2 (rt-gensym)))
	`(let ((,g ,gen)
	       (,g2 ,val))
	   (if (is-type? <rt-readin> ,g)
	       (rt-readin-set_location ,g ,g2)
	       (setter!-mus-location/mus_set_location ,g ,g2))))))
(<rt-func> 'rt-readin-set_location '<void> '(<rt-readin> <int>) #:is-immediate #t)
(define-c-macro (rt-readin-set_location gen val)
  (<-> (symbol->string gen) "->location=" (eval-c-parse val)))


(define-rt-macro (setter!-mus-increment expand/gen expand/val)
  (if (rt-immediate? gen val)
      `(if (is-type? <rt-readin> ,gen)
	   (rt-readin-set_increment ,gen ,val)
	   (setter!-mus-increment/mus_set_increment ,gen ,val))
      (let ((g (rt-gensym))
	    (g2 (rt-gensym)))
	`(let ((,g ,gen)
	       (,g2 ,val))
	   (if (is-type? <rt-readin> ,g)
	       (rt-readin-set_increment ,g ,g2)
	       (setter!-mus-increment/mus_set_increment ,g ,g2))))))
(<rt-func> 'rt-readin-set_increment '<void> '(<rt-readin> <int>) #:is-immediate #t)
(define-c-macro (rt-readin-set_increment gen val)
  (<-> (symbol->string gen) "->increment=" (eval-c-parse val)))








#!

(define file (make-readin "/home/kjetil/t1.wav"))
(mus-file-name file)
(set! (mus-file-name file) "gakkgakk")
(define rt-file (make-rt-readin file))
(rt-readin rt-file)
(set! (mus-increment file) 1)
(set! (mus-location file) 5470)
(mus-channel file)

(define a (rt-2 '(lambda ()
		   (out (readin rt-file)))))
setter!-rt-mus-set_location/mus_set_location
setter!-rt-mus-location/mus_location

(rt-2 '(lambda ()
	 (set! (mus-location rt_file) 0)))
(macroexpand-1 '(rt-macro-setter!-mus-location ai))


(define file (make-readin "/home/kjetil/t1.wav"))

(define rt-file (make-rt-readin file))

(rt-run 0 100
	(lambda ()
	  (if (>= (mus-location file) (mus-length file))
	      (set! (mus-location file) 200))
	  (out (readin file))))
(mus-length file)
(mus-location file)	  
(set! (mus-location file) 2000)
(-> rt-engine start)
(-> rt-engine stop)
(rt-funcall a)


(let ((rs (make-readin "/home/kjetil/t1.wav")))
  (-> (rt (lambda ()
	       (if (>= (mus-location rs) (mus-length rs))
		   (set! (mus-location rs) 0))
	       (out (readin rs))))
      play))


(define osc (make-oscil #:frequency 440))
(define i (rt (lambda ()
		   (oscil osc))))
(-> i play)
(-> i stop)
(rt-clear-cache!)
(-> rt-engine start)

(define osc (make-oscil #:frequency 440))
(define vol 0.4)
(define instrument (rt (lambda ()
			    (out (* vol (oscil osc))))))
(-> instrument play)
(-> instrument stop)

!#



#!

(define osc (make-oscil #:frequency 440))
(define loc (make-locsig :degree 80 :distance 1 :channels 2))
(rt-2 '(lambda ()
	 (mus-frequency (vector-ref a b))))

	 (set! (mus-frequency (vector-ref a b)) 200)))
	 ;;(* transposition (vct-ref peak-freqs k)))))
	  (locsig loc (* (oscil osc) 0.5))))
!#







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; VCT. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rt-macro (vct-length vct)
  `(rt-vct-length/vct-length ,vct))
   
(define-rt-macro (rt-vct-legal-pos das-vct pos funcname body)
  (if (rt-is-safety?)
      `(begin (if (< ,pos 0)
		  (rt-error ,(<-> "Illegal second argument for " funcname " pos: pos<0.")))
	      (if (>= ,pos (vct-length ,das-vct))
		  (rt-error ,(<-> "Illegal second argument for " funcname "vct-legal-pos: pos>=length.")))
	      ,body)
      ,body))


(define-rt-macro (vct-set! das-vct pos val)
  `(rt-vct-legal-pos ,das-vct ,pos "vct-set!"
		     (rt-vct-set!/vct-set! ,das-vct (rt-castint/castint ,pos) ,val)))

(define-rt-macro (vct-ref vct pos)
  `(rt-vct-legal-pos ,das-vct ,pos "vct-ref"
		     (rt-vct-ref!/vct-ref ,das-vct (rt-castint/castint ,pos))))

(define-rt-macro (vct-scale! vct scl)
  `(rt-range2 i 0 (vct-length ,vct) 1
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
		   (vct-scale! v 2)
		   5)))
(rt-funcall a)
(define a (rt-2 '(lambda ()
		   (vct-set! v 0 5))))
		   
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; <realtime>/rt-compile/rt/rt-run/rt-funcall/etc.;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class (<realtime> func arg #:key (engine rt-engine))

  ;;(define procfunc (<RT_Procfunc> #:func func #:arg arg))
  (define procfunc (rt_make_procfunc func arg))
  (define procfunc-data (rt_get_procfunc_data procfunc))

  (def-method (stop #:optional (end (-> engine get-time)))
    ;;(c-display "stop, end: " end)
    (-> engine add-event
	(-> engine get-frame-time end)
	(rt_remove_procfunc)
	procfunc-data))
  
  (def-method (play #:optional (start (-> engine get-time)) end)
    ;;(c-display "play, now/start/end" (-> engine get-time) start end)
    (rt_protect_var procfunc) ;; Unprotection happens in the rt_non_check_non_rt function. (Perhaps I should make a drawing that shows how the protection/unprotection
    (-> engine add-event      ;; madness happens...)
	(-> engine get-frame-time start)
	(rt_insert_procfunc)
	procfunc-data)
    (if end
	(this->stop end)))

  (def-method (stop-now #:optional (end 0))
    (this->stop (+ (-> engine get-time) end)))
		 
  (def-method (play-now #:optional (start 0) end)
    (if (<= end start)
	(c-display "Error. <realtime> -> play-now, end<=start: (play-now " start end ")")
	(let ((start-time (-> engine get-time)))
	  (this->play (+ start-time start)
		      (if end
			  (+ start-time end)
			  end))))))

	       
;  (def-method (play-now #:optional length)
;    (if length
;	(this->play (-> engine get-time) (+ (-> engine get-time) length))
;	(this->play (-> engine get-time))))

;  (def-method (start #:optional (time (-> engine get-time)))
;    (rt-start))



(define (rt-4 term)

  (call-with-current-continuation
   (lambda (return)

     (let ((uniqify-res #f)
	   (renamed-vars #f)
	   (external-vars #f)
	   (insert-types-res #f)
	   (orgargs #f)
	   (returntype #f)
	   (extnumbers #f)
	   (extpointers #f)
	   (extnumbers-writing #f))

       (rt-print "*RT: Expanding macros")
       (set! term (rt-expand-macros term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Checking syntax")
	      
       (if (not (rt-check-syntax term))
	   (return #f))

       (rt-print "*RT: Replacing defines with letrecs")
	      
       (set! term (rt-replace-define-with-letrecs term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Fixing various")
	      
       (set! uniqify-res (rt-fix-various term))
       (if (not uniqify-res)
	   (return #f))

	      
       (set! renamed-vars (car uniqify-res))
       (set! term (cadr uniqify-res))

       (rt-print "*RT: Let*-lifter" term)
	      
       (set! term (rt-let*-lifter term))
       (if (not term)
	   (return #f))
       
       (rt-print "*RT: Inserting types" term)
       
       (set! insert-types-res (rt-insert-types term renamed-vars))
       (if (not insert-types-res)
	   (return #f))

	      
       (set! orgargs (map (lambda (org new)
			    (rt-print2 "org/new" org new)
			    (list org (-> (hashq-ref rt-types (cadr new)) c-type)))
			  (cadr term)
			  (cadr (car insert-types-res))))

       (set! term (car insert-types-res))
       (set! returntype (cadr insert-types-res))
       (set! extnumbers (caddr insert-types-res))
       (set! extpointers (cadddr insert-types-res))
       (set! extnumbers-writing (car (cdr (cdddr insert-types-res))))
       
       (rt-print2 "before" (cadr term))
       (set-car! (cdr term)
		 (append (map (lambda (var)
				(list (car var)
				      (-> (cadr var) rt-type)))
			      (append extnumbers-writing extpointers extnumbers))
			 orgargs))
       (rt-print2 "after" (cadr term))
       
       (rt-print "*RT: Removing unused stuff" term orgargs (map (lambda (a) (-> (cadr a) rt-type)) extpointers))
       
       (set! term (rt-remove-unused++ term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Lifting lambdas.")
       
       (set! term (rt-lambda-lifter term))
       (if (not term)
	   (return #f))
       
       (rt-print "*RT: Inserting returns" term)
       (set! term (rt-insert-returns term returntype))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Performing last hacks" term)
       (set! term (rt-last-hacks term))
       (if (not term)
	   (return #f))

       (rt-print "RT: Final term:" term)
       (list extnumbers
	     extpointers
	     extnumbers-writing
	     orgargs
	     returntype
	     term)))))



(define (rt-3.5 term)
  (let ((t (rt-4 term)))
    (if t
	(caddr (cdddr t)))))


(define (rt-3 term)
  (let ((rt-4-result (rt-4 term)))
    (if (not rt-4-result)
	#f
	(let* ((extnumbers (append (caddr rt-4-result) (car rt-4-result)))
	       (extpointers (cadr rt-4-result))
	       ;;(extnumbers-writing (caddr rt-4-result))
	       (extnumbers-writing '())
	       (orgargs (cadddr rt-4-result))
	       (returntype (cadr (cdddr rt-4-result)))
	       (term (caddr (cdddr rt-4-result)))
	       (mainfuncargs (cadr term))
	       
	       (funcname (rt-gensym))
	       (das-funcname (rt-gensym))
	       (rt-innerfuncname (rt-gensym))
	       (rt-funcname (rt-gensym))
	       
	       (funcarg (rt-gensym))
	       (publicargs (append (map (lambda (extvar)
					  (rt-print2 "extvar1" extvar)
					  `(<SCM> ,(symbol-append '_rt_scm_ (car extvar))))
					extnumbers-writing)
				   (map (lambda (extvar)
					  (rt-print2 "extvar2" extvar)
					  (list (-> (cadr extvar) c-type) (car extvar)))
					(append extpointers extnumbers))
				   (map (lambda (a)
					  (list (cadr a) (car a)))
					orgargs)))
	       (i 0)
	       (types (map (lambda (var)
			     (rt-print2 "var" var)
			     (list (eval-c-to-scm
				    (string-trim-right
				     (eval-c-get-propertype
				      (car var))))
				   (cadr var)))
			   publicargs)))

	  (rt-print "publicargs" publicargs)
	  (rt-print "orgargs" orgargs)
	  (rt-print "mainfuncargs" mainfuncargs)
	  ;;(rt-print "types" types)
	  
	  (rt-print2 "term" term)
	  (newline)
	  
	  (list funcname
		rt-funcname
		extnumbers-writing
		extpointers
		extnumbers
		
		`( ;;(define-struct <func_args>
		   ;;  <int> num_outs
		   ;;  <float-*> outs
		   ;;  <int> num_ins
		   ;;  <float-*> ins
		   ;;  <float> time
		   ;;  <float> samplerate
		   ;;  <float> res
		   ;;  <char-*> error
		   ;;  <SCM> errorvariable
		   ;;  <int> errorvarnum)

		  "#include <jack/ringbuffer.h>"
		  
		  (shared-struct <RT_Event>)
		  (shared-struct <RT_Procfunc>)
		  (shared-struct <RT_Engine>)
		  
		  (shared-struct <mus_rt_readin>)
		   
		   ;(<struct-func_args-*> _rt_funcarg NULL)
		   
		   ;(<int> _rt_num_outs)
		   ;(<float-*> _rt_outs)
		   ;(<int> _rt_num_ins)
		   ;(<float-*> _rt_ins)
		   ;(<float> _rt_time)
		   ;(<float> _rt_samplerate)
		   ;(<float> _rt_res)
		   ;(<char-*> _rt_error)
		   ;(<SCM> _rt_errorvariable)
		   ;(<int> _rt_errorvarnum)

		   (define-struct <RT_Globals>
		     ,@(apply append publicargs)
		     ,@(apply append (map (lambda (vardecl)
					    (list (cadr vardecl) (car vardecl)))
					  (remove (lambda (vardecl)
						    (= 3 (length vardecl)))
						  (cadr (caddr term)))))
		     <struct-RT_Engine*> engine)

		   
		   (<nonstatic-extern-scm_t_bits> rt_readin_tag)

		   "extern float rt_readin(struct mus_rt_readin*)"
		   
		   "typedef float (*ThreadFunc)(void)"
		   "typedef float (*ReadinFunc)(struct mus_rt_readin*)"


		   (<void> rt_error (lambda (,rt-globalvardecl (<char-*> msg))
				      (<static-int> errordisplayed 0)
				      (if (not errordisplayed)
					  (fprintf stderr (string "RT RUNTIME ERROR: %s.\\n") msg))
				      (set! errordisplayed 1)
				      ,(if (rt-is-safety?)
					   '(longjmp rt_globals->engine->error_jmp 5)
					   "/* */")))
		   
		   
		   (<struct-mus_rt_readin-*> rt_scm_to_rt_readin (lambda (,rt-globalvardecl (<SCM> name))
								   ,(if (rt-is-safety?)
									`(if (not (SCM_SMOB_PREDICATE rt_readin_tag name))
									     (begin
									       (rt_error rt_globals (string "Variable is not an rt-readin generator"))
									       (return NULL))
									     (return (cast <void-*> (SCM_SMOB_DATA name))))
									`(return (cast <void-*> (SCM_SMOB_DATA name))))))
		   
		   (<double> rt_scm_to_double (lambda (,rt-globalvardecl (<SCM> name))
						(if (SCM_INUMP name)
						    (return (SCM_INUM name))
						    (if (SCM_REALP name)
							(return (SCM_REAL_VALUE name))
							(begin
							  (rt_error rt_globals (string "Variable is not a number (to_double)"))
							  (return 0))))))
		   (<float> rt_scm_to_float (lambda (,rt-globalvardecl (<SCM> name))
					      (if (SCM_INUMP name)
						  (return (SCM_INUM name))
						  (if (SCM_REALP name)
						      (return (SCM_REAL_VALUE name))
						      (begin
							(rt_error rt_globals (string "Variable is not a number (to_float)"))
							(return 0))))))
		   (<int> rt_scm_to_int (lambda (,rt-globalvardecl (<SCM> name))
					  (if (SCM_INUMP name)
					      (return (SCM_INUM name))
					      (if (SCM_REALP name)
						  (return (SCM_REAL_VALUE name))
						  (begin
						    (rt_error rt_globals (string "Variable is not a number (to_int)"))
						    (return 0))))))
		   (<vct-*> rt_scm_to_vct (lambda (,rt-globalvardecl (<SCM> name))
					    ,(if (rt-is-safety?)
						 `(if (vct_p name)
						      (return (TO_VCT name))
						      (begin
							(rt_error rt_globals (string "Variable is not a VCT."))
							(return NULL)))
						 `(return (TO_VCT name)))))
		   
		   (<mus_any-*> rt_scm_to_mus_any (lambda (,rt-globalvardecl (<SCM> name))
						    (if (mus_xen_p name)
							(return (cast <void-*> (XEN_TO_MUS_ANY name)))
							(if (not (SCM_SMOB_PREDICATE rt_readin_tag name))
							    (begin
							      (rt_error rt_globals (string "Variable is not a CLM generator or rt-readin generator"))
							      (return NULL))
							    (return (cast <void-*> (SCM_SMOB_DATA name)))))))
		   
		   
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

		   (<float> rt_in (lambda (,rt-globalvardecl (<int> ch))
				    ,(if (rt-is-safety?)
					 '(if (< ch 0)
					      (rt_error rt_globals (string "Channel number for In less than zero")))
					 "/* */")
				    ,(if (rt-is-safety?)
					 '(if (> ch rt_globals->engine->num_ins)
					      (rt_error rt_globals (string "Illegal channel number for In")))
					 "/* */")
				    (return rt_globals->engine->ins[ch][rt_globals->engine->framenum])))
		   
		   (<void> rt_out (lambda (,rt-globalvardecl (<int> ch)(<float> val))
				    ,(if (rt-is-safety?)
					 '(if (< ch 0)
					      (rt_error rt_globals (string "Channel number for Out less than zero")))
					 "/* */")
				    ,(if (rt-is-safety?)
					 '(if (> ch rt_globals->engine->num_outs)
					      (rt_error rt_globals (string "Illegal channel number for Out")))
					 "/* */")
				    (+= rt_globals->engine->outs[ch][rt_globals->engine->framenum] val)))

				    
		   ;; Inserting all inner functions. All global variables have been put into the rt_global struct, and is therefore not used here.
		   ,@(map (lambda (vardecl)
			    (if (= 3 (length vardecl))
				(list (cadr vardecl) (car vardecl) (caddr vardecl))    ;; Function
				"/* */"))                                              ;; Variable
			  ;;(list (cadr vardecl) (car vardecl))))
			  (cadr (caddr term)))
		   
		   
		   ;;(,returntype ,rt-innerfuncname (lambda ,(cadr term)
		   (,returntype ,rt-innerfuncname (lambda ,(cons rt-globalvardecl orgargs) ;;,mainfuncargs ;;,publicargs
						    ,@(cddr (caddr term))))
		   

		   (<void> ,das-funcname (lambda ,(cons rt-globalvardecl orgargs) ;;,publicargs
					   
					   ;;(set! _rt_funcarg _rt_local_rt_funcarg)
					   
					   ;;,@(map (lambda (extvar)
					   ;;	    `(<float> ,(car extvar)))
					   ;;	  extnumbers-writing)

					   (if (> (setjmp rt_globals->engine->error_jmp) 0)
					       return)
					   
					   ;; Code for writing guile-variables. Not used anymore.
					   ;;,@(let ((n -1))
					   ;;    (map (lambda (extvar)
					   ;;		       (let ((name (symbol-append '_rt_scm_ (car extvar))))
					   ;;			 (set! n (1+ n))
					   ;;			 `(if (|| (SCM_INUMP ,name)
					   ;;				  (SCM_BIGP ,name)
					   ;;				  (! (SCM_REALP ,name)))
					   ;;			      (begin
					   ;;				(set! rt_globals->error_jmpiable ,name)
					   ;;				(set! rt_globals->error_jmpnum ,n)
					   ;;				(set! _rt_error (string ,(string-append "\\\""
					   ;;										 (symbol->string (car extvar))
					   ;;										 "\\\" is not a real float")))
					   ;;				return))))
					   ;;		     extnumbers-writing))
					   
					   ;;,@(map (lambda (extvar)
					   ;;	    (let ((name (symbol-append '_rt_scm_ (car extvar))))
					   ;;	      `(set! ,(car extvar) (SCM_REAL_VALUE ,name))))
					   ;;	  extnumbers-writing)
					   
					   
					   
					   ;; This is for writing guile-variables. Commented, because it didn't quite work.
					   ;;,@(map (lambda (extvar)
					   ;;	    (let ((name (symbol-append '_rt_scm_ (car extvar))))
					   ;;	      `(set! (SCM_REAL_VALUE ,name) ,(car extvar))))
					   ;;	  extnumbers-writing)
					   
					   ,(if (rt-is-number? returntype)
						`(set! rt_globals->engine->res (,rt-innerfuncname rt_globals ,@(map cadr orgargs)))
						`(,rt-innerfuncname rt_globals ,@(map cadr orgargs)))
					   
					   ))
		   
		   (functions->public
		    (<void> ,rt-funcname (lambda ((<SCM> vector)
						  (<struct-RT_Engine-*> engine)
						  (<int> startframe)
						  (<int> endframe))
					   (,rt-globalstructname temp {0} )
					   ,(append rt-globalvardecl '(&temp))
					   ,(if (null? orgargs)
						`(begin
						   ;;(<struct-func_args> funcarg (struct-set num_outs outs num_ins ins time samplerate 0 NULL 0 0))
						   ,(if (not (null? extnumbers-writing))
							'(<SCM> setfloats (SCM_VECTOR_REF vector 0))
							"/* */")
						   ,(if (not (null? extpointers))
							 '(<SCM> pointers (SCM_VECTOR_REF vector 1))
							 "/* */")
						   ,(if (not (null? extnumbers))
							'(<SCM> readfloats (SCM_VECTOR_REF vector 2))
							"/* */")
						   
						   ;;,@(map (lambda (n) `(SCM_VECTOR_REF setfloats ,n)) (iota (length extnumbers-writing)))
						   ,@(map (lambda (n extvar)							     
							    (if (eq? '<SCM> (-> (cadr extvar) c-type))
								`(set! ,(symbol-append 'rt_globals-> (car extvar)) (SCM_VECTOR_REF pointers ,n))
								`(set! ,(symbol-append 'rt_globals-> (car extvar)) (GET_POINTER(SCM_VECTOR_REF pointers ,n)))))
							  (iota (length extpointers))
							  extpointers)
						   
						   ,@(map (lambda (n extvar)
							    `(set! ,(symbol-append 'rt_globals-> (car extvar))
								   (SCM_REAL_VALUE(SCM_VECTOR_REF readfloats ,n))))
							  (iota (length extnumbers))
							  extnumbers)
						   
						   (set! rt_globals->engine engine)

						   ;;(printf (string "start/end %d %d\\n") startframe endframe)
						   ;;(if (not (== nframes 2048))
						   ;;    (printf (string "nframes: %d\\n") nframes))
						   
						   (set! engine->framenum startframe)
						   (for-each startframe < endframe 1
							     (lambda (framenum)
							       (,rt-innerfuncname rt_globals)
							       engine->time++
							       engine->framenum++)))
							       
						'return))))
		   
		   (public			      
		    (<float> ,funcname (lambda ((<SCM> argvect))
					 (<struct-RT_Engine> temp2 {0})
					 (<struct-RT_Engine-*> engine &temp2)
					 
					 (,rt-globalstructname temp {0} )
					 ,(append rt-globalvardecl '(&temp))

					 (set! rt_globals->engine engine)
					 
					 ;;(<struct-func_args> ,funcarg "{0}")

					 (SCM_ASSERT (== ,(length publicargs) (SCM_VECTOR_LENGTH argvect))
						     argvect
						     0
						     (string "Wrong number of arguments."))
							 
					 ,@(map (lambda (n name)
						  `(<SCM> ,(cadr name) (SCM_VECTOR_REF argvect ,n)))
						(iota (length publicargs))
						publicargs)
							      
					 ,@(map (lambda (das-type)
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
					 
					 ,@(map (lambda (type)
						  `(set! ,(symbol-append 'rt_globals-> (cadr type)) ,(list (string->symbol (<-> "GET_" (car type)))
													   (cadr type))))
						types)
					 
					 (,das-funcname rt_globals)
					 
					 (SCM_ASSERT (== NULL rt_globals->engine->error)
						     rt_globals->engine->errorvariable
						     rt_globals->engine->errorvarnum
						     rt_globals->engine->error)
					 ;;(SCM_ASSERT (== NULL ,(symbol-append funcarg '.error))
					 ;;	     ,(symbol-append funcarg '.errorvariable)
					 ;;	     ,(symbol-append funcarg '.errorvarnum)
					 ;;	     ,(symbol-append funcarg '.error))
					 (return rt_globals->engine->res))))))))))



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

(define a (rt-2 '(lambda ()
		   (do ((i 0 (1+ i)))
		       ((= i 10) (+ 2 5))
		     (printf "%d\\n" i)))))
(define a (rt-2 '(lambda ()
		   (let* ((a 0))
		     a)
		   3)))
(rt-funcall a 8)

(define a (rt-2 '(lambda (a)
		   (begin
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


(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(define-rt (tak-rt x y z)
  (declare (<int> x y z))
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
  (letrec ((move-them (lambda (n from to helper)
			(declare (<int> n))
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
	       ;;(extnumbers-writing (caddr rt-3-result))
	       (extnumbers-writing '())
	       (extpointers (cadddr rt-3-result))
	       (extnumbers (append (caddr rt-3-result) (cadr  (cdddr rt-3-result))))
	       (term       (caddr (cdddr rt-3-result)))
	       (callmacro (procedure->macro
			   (lambda (x env)
			     (if (null? extnumbers-writing)
				 `(,funcname (vector ,@(map (lambda (extvar)
							      (let ((name (cadddr extvar))
								    (type (cadr extvar)))
								`(-> ,type transform ,name)))
							    (append extnumbers-writing extpointers extnumbers))
						     ,@(cdr x)))
				 `(begin
				    ;;,@(map (lambda (extvar)
				    ;;	      `(if (number? ,(cadddr extvar))
				    ;;		   (set! ,(cadddr extvar) (exact->inexact ,(car extvar)))))
				    ;;	    extnumbers-writing)
				    (,funcname (vector ,@(map (lambda (extvar)
								(let ((name (cadddr extvar))
								      (type (cadr extvar)))
								  `(-> ,type transform ,name)))
							      (append extnumbers-writing extpointers extnumbers))
						       ,@(cdr x))))))))
	       (rt-callmacro (procedure->macro
			      (lambda (x env)
				`(begin
				   (let* ((writing-vector (vector ,@(map (lambda (ext)
									   (exact->inexact ext))
									 (iota (length extnumbers-writing)))))
					  (reading-vector (vector ,@(map (lambda (ext)
									   (exact->inexact ext))
									 (iota (length extnumbers)))))
					  (ret (<realtime> (,rt-funcname)
							   ;; Note, the vector below is gc-marked manually in the funcall smob.
							   (let* ((extra-gc-vars '())
								  (transformed-pointers (vector ,@(map (lambda (extvar)
													 (let ((name (cadddr extvar))
													       (type (cadr extvar)))
													   `(-> ,type transform
														,name
														(lambda (var)
														  (set! extra-gc-vars
															(cons var
															      extra-gc-vars))))))
												       extpointers))))
							     ;;(c-display "1" ,extpointers)
							     ;;(c-display "2" (list ,@(map cadddr extpointers)))
							     (vector writing-vector
								     transformed-pointers
								     reading-vector
								     ;; Keep extra-gv-vars and untransformed values here so they can be gc-marked.
								     (append extra-gc-vars (list ,@(map cadddr extpointers))))))))
				     ,@(map (lambda (n extvar)
					      (let ((name (cadddr extvar))
						    (type (cadr extvar)))
						`(rt-set-float! (vector-ref writing-vector ,n)
								(-> ,type transform ,name))))
					    (iota (length extnumbers-writing))
					    extnumbers-writing)
				     
				     ,@(map (lambda (n extvar)
					      (let ((name (cadddr extvar))
						    (type (cadr extvar)))
						`(rt-set-float! (vector-ref reading-vector ,n)
								(-> ,type transform ,name))))
					    (iota (length extnumbers))
					    extnumbers)
				     
				     ;;(c-display "jepp, creating" ,(length extnumbers) "-" reading-vector (object-address (vector-ref reading-vector 0))
				     ;;	 (object-address (vector-ref reading-vector 0)))
				     
				     ;;(apply c-display ,@(map cadddr extnumbers))
				     
				     ,@(map (lambda (n extvar)
					      `(-> ret add-method ',extvar (make-procedure-with-setter
									    (lambda ()
									      (vector-ref writing-vector ,n))
									    (lambda (newval)
									      (rt-set-float! (vector-ref writing-vector ,n) newval)))))
					    (iota (length extnumbers-writing))
					    (map cadddr extnumbers-writing))
				     
				     ,@(map (lambda (n extvar)
					      `(-> ret add-method ',extvar (make-procedure-with-setter
									    (lambda ()
									      (vector-ref reading-vector ,n))
									    (lambda (newval)
									      (rt-set-float! (vector-ref reading-vector ,n) newval)))))
					    (iota (length extnumbers))
					    (map cadddr extnumbers))
				     
				     ,@(map (lambda (pointer)
					      `(-> ret add-method ',pointer (lambda ()
									      ,pointer)))
					    (map cadddr extpointers))
				     
				     ret))))))
	  
	  (apply eval-c-non-macro (append (list (<-> "-I" snd-header-files-path " ") ;; " -Werror "
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
	  
	  (list 'rt-rt
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


;; rt-1 + compiled code cache handling.
(define rt-cached-funcs (make-hash-table 997))
(define (rt-1 term)
  (let* ((key (list term
		    (rt-safety)               ;; Everything that can change the compiled output must be in the key.
		    (-> rt-engine samplerate))) ;; If saving to disk, the result of (version) and (snd-version) must be added as well. (and probably some more)
	 (cached (hash-ref rt-cached-funcs key)))
    (if cached
	cached
	(let ((new (rt-2 term)))
	  (hash-set! rt-cached-funcs key new)
	  new))))
(define (rt-clear-cache!)
  (set! rt-cached-funcs (make-hash-table 997)))

;; rt
(define-macro (rt-compile term)
  (c-display "rt-compile" term)
  `(rt-1 ',term))
(define-macro rt-c rt-compile)

(define-macro (rt-func term)
  (let ((das-rt (rt-1 term)))
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

(define-macro (rt rt-func)
  `((caddr (rt-compile ,rt-func))))

(define-macro (rt-run start dur func)
  (let ((instrument (rt-gensym))
	(start2 (rt-gensym)))
    `(let ((,instrument (rt ,func))
	   (,start2 ,start))
       (-> ,instrument play-now ,start2 (+ ,start2 ,dur))
       ,instrument)))
      

(define-macro (rt-play func)
  (let ((instrument (rt-gensym)))
    `(let ((,instrument (rt ,func)))
       (-> ,instrument play)
       ,instrument)))
      


(define-macro (definstrument def . body)
  ;; First compile up any rt-code.
  (let ((rt-funcs '()))
    
    (define (add-rt-func code)
      (let ((name (rt-gensym)))
	(set! rt-funcs (cons (list name code)
			     rt-funcs))
	name))
    
    (define (find-rt term)
      (cond ((null? term) term)
	    ((not (list? term)) term)

	    ;((eq? 'rt-compile (car term))
	    ; (let ((func (add-rt-func (cadr term))))
	    ;   `((cadr ,func))))
	    
	    ((eq? 'rt (car term))
	     (let ((func (add-rt-func (cadr term))))
	       `((caddr ,func))))
	    
	    ((eq? 'rt-run (car term))
	     (let ((func (add-rt-func (cadddr term)))
		   (instrument (rt-gensym))
		   (start2 (rt-gensym)))
	       `(let ((,instrument ((caddr ,func)))
		      (,start2 ,(cadr term)))
		  (-> ,instrument play-now ,start2 (+ ,start2 ,(caddr term)))
		  ,instrument)))

	    ((eq? 'rt-play (car term))
	     (let ((func (add-rt-func (cadr term)))
		   (instrument (rt-gensym)))
	       `(let ((,instrument ((caddr ,func))))
		  (-> ,instrument play)
		  ,instrument)))

	    (else
	     (map find-rt term))))


    (let ((newbody (find-rt body)))
      (if (not (null? rt-funcs))
	  `(define* ,(car def)
	     (let ,(map (lambda (def)
			  `(,(car def) (rt-compile ,(cadr def))))
			rt-funcs)
	       (lambda* ,(cdr def)
			,@newbody)))
	  `(define* ,def
	     ,@body)))))
		

#!
(define (a b c)
  2 3 4)
(define a
  (let ((asdfa asdfadsf))
    (lambda (b c)
      2 3 4)))

(rt-run start dur
	(lambda ()
	  (out (* (env amp-env)))))
!#





#!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Drodle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rt-2 '(lambda ()
	 (+ 2 a 35)))

(rt- '(lambda ()
	(call-with-current-continuation
	 (lambda (return)
	   (return 2)))))


(define a (rt-2 '(lambda ()
		   (call-with-current-continuation
		    (lambda (return)
		      (return 2)
		      5)))))
(rt-funcall a)

(define a (rt-2 '(lambda ()
		   2)))
(define a (rt-2 '(lambda ()
		   (create-thread (lambda ()
				    (printf "I'm threaded!\\n"))))))

(rt-funcall a)

(define a (rt-2 '(lambda ()
		   (let* ((a (lambda ()
			       9))
			  (b a))
		     (b)))))

(define a (rt-2 '(lambda ()
		   (let ((a (lambda ()
			      5)))
		     a))))

(define a (rt-2 '(lambda ()
		   (((lambda ()
		       (lambda ()
			 5)))))))


(define a (rt-2 '(lambda ()
		   (let ((a (let ((b (lambda ()
				       9)))
			      b)))
		     (a)))))

(rt-funcall a)



;; Closure: (is not supposed to work)
(define a (rt-2 '(lambda ()
		   (let* ((a (lambda (b)
			       (lambda ()
				 (the <int> b))
			       )))
		     ((a 50))))))
;; Wrong result:
(rt-funcall a)




(rt-2 '(lambda ()
	 (out 0 2 4 6 7 8 9 29 3(in))))


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


(rt-expand-macros '(mus-channels (+ 2 3 locs)))

(rt-expand-macros '(lambda ()
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
		   (letrec ((das_func (lambda ()
				     (let ((a 2))
				       (while (< a 10)
					      (printf "ai: %d\\n" a)
					      (if (>= a 5)
						  (break))
					      (set! a (1+ a))
					      (let ((ai (lambda ()
							  (if (odd? a)
							      (continue)))))
						(ai))
					      (set! a (1+ a))
					      ;;(das_func)
					      ;;(break)
					      )
				       a))))
		     (das_func)))))
(rt-funcall a)

(macroexpand-1 '(rt-macro-oscil osc2 2 3))


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

(define a (rt-2 '(lambda ()
		   (range i 0 5
			  (printf "%d " i)))))

		   (let ((a 2))
		     (printf "ai %f %f\\n" a a)
		     (while (< a 9)
			    (set! a (1+ a)))
		     a))))
(rt-funcall a)


(define readfloat 2)
(define setfloat-2 3)
(define osc (make-oscil :frequency 400))
(define osc2 (make-oscil :frequency 10))

(macroexpand-1 '(rt-macro-oscil osc))

(define a (rt-2 '(lambda (something)
		   ;;(oscil osc2)
		   (set! setfloat-2 (oscil osc))
		   (out (* 0.2 something (oscil osc) (oscil osc2))))))

(define b (rt a))

(begin b)

(begin (cadr b))

(caddr a)


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



;; Error:
(define a (rt-2 '(lambda ()
		   (let ((a 1))
		     (letrec ((a 2)
			      (b (let ((c (lambda ()
					    a)))
				   (c))))
		       b)))))

(rt-funcall a)

(define val 1.4)
(rt-clear-cache!)
(define a (rt-run 1 3
		  (lambda ()
		    (out (- (random val) 0.9)))))
(define a (rt-2 '(lambda()
		   (random 5/2))))
(rt-funcall a)

(-> rt-engine start)

(apply vector (list 0 1 2))

(apply vector (map (lambda (ch)
		     (make-rt-readin (make-readin "/home/kjetil/cemb2.wav" #:channel ch)))
		   (iota 2)))

Kr�sj:
------
(load-from-path "rt-compiler.scm")
(-> rt-engine start)
(definstrument (sc-play filename pan src vol)
  (let ((read (make-readin filename))
	(loc (make-locsig #:degree (* (1+ pan) 45) #:channels 2))
	(dir 1)
	(sr (make-src #:srate src)))
    (rt (lambda ()
	     ;;(locsig-set! loc 0 (  pan))
	     ;;(locsig-set! loc 0 (  pan))
	     (locsig loc
		     (* vol
			(src sr 0.0
			     (lambda (d)
			       (if (>= (mus-location read) (mus-length read))
				   (set! (mus-increment read) -1)
				   (if (<= (mus-location read) 0)
				       (set! (mus-increment read) 1)))
			       (readin read)))))))))

(define s2 (sc-play "/home/kjetil/cemb2.wav" 0.8 0.9 0.7))
(-> s2 play)
(-> s2 stop)

(definstrument (sc-play2)
  (let ((rs (make-readin "/home/kjetil/cemb2.wav"))
	(an-src (make-src)))
    (rt (lambda ()
	     (if (>= (mus-location rs) (mus-length rs))
		 (set! (mus-location rs) 0))
	     (out (src an-src 0.0
		       (lambda (d)
			 (readin rs))))))))

(define s (sc-play2))
(-> s play)
(-> s stop)

Kr�sj2:
-------
(load-from-path "rt-compiler.scm")
(-> rt-engine start)
(definstrument (sc-play2)
  (let ((rs (make-readin "/home/kjetil/cemb2.wav"))
	(an-src (make-src))
	(loc (make-locsig  #:channels 2)))
    (rt (lambda ()
	     (if (>= (mus-location rs) (mus-length rs))
		 (set! (mus-location rs) 0))
	     (out (locsig loc
			  (src an-src 0.0
			       (lambda (d)
				 (readin rs)))))))))
(define s (sc-play2))

(-> s play)
(-> s stop)

(define s (let ((rs (make-readin "/home/kjetil/cemb2.wav")))
	    (rt (lambda ()
		     (if (>= (mus-location rs) (mus-length rs))
			 (set! (mus-location rs) 0))
		     (out (readin rs))))))

(mus-location (-> s rs))
(mus-length (-> s rs))

(-> s rs)

(gc)
[jack watchdog kills snd!]


(define l '(1 2 3 4))
(define l '(1 2))
(define a (rt-2 '(lambda ()
		   (for-each (lambda (n)
			       (printf "%f\\n" (+ 100 n)))
			     l))))

(rt-funcall a)

(for-each (lambda (a b)
	    ...)
	  lda ldb)
->
(let ((func (lambda (a b)
	      ...))
      (la lda)
      (lb ldb))
  (while (and (not (null? la))
	      (not (null? lb)))
	 (func (car la)
	       (car lb))
	 (set! la (cdr la))
	 (set! lb (cdr lb))))


(define-macro (for-each2 func . lists)
  (let ((lnames (map (lambda (n) (rt-gensym)) (iota (length lists))))
	(funcname (rt-gensym)))
    `(let ((,funcname ,func)
	   ,@(map (lambda (lname llist)
		    (list lname llist))
		  lnames
		  lists))
       (while (and ,@(map (lambda (lname)
			    `(not (null? ,lname)))
			  lnames))
	      (,funcname ,@(map (lambda (lname)
				  `(car ,lname))
				lnames))
	      ,@(map (lambda (lname)
		       `(set! ,lname (cdr ,lname)))
		     lnames)))))

(for-each2 (lambda x
	     (apply c-display x))
	   '(a b c d e)
	   '(0 1 2 3 4 6))


(define s (make-src #:width 5))
(begin s)

(define do-things 0)
(rt-2 '(lambda ()
	 (if ,do-things
	     (+ 2 3 4 wef)
	     5)))

(rt-expand-macros '(and (or 0 0) (min 2 3) 4 5))


(define (is-expand varname)
  (if (string= "expand/" varname 0 7 0 7)
      (string-drop varname 7)
      #f))
(is-expand "expand/gakkgeakk")

(let ()
  (+ 2 3 4))

(rt-expand-macros '(truncate (and 2 3)))
(rt-expand-macros '(modulo 5 2))


;; Oh boy!
(define a (rt-2 '(lambda ()
		   (+ 0 (list-ref alist 13)))))
(define alist '(10 1 2 3 4 5 6 7 8 9 10 11 12 13))

(rt-funcall a)

(define a (rt-2 '(lambda ()
		   (let ((a 9))
		     (if (and 1 (or 5 (is-type? <int> a)))
			 5
			 6)))))

(define vars (make-hash-table 219))
(begin vars)
(hashq-set! vars 'a '<int>)

(define l '(5.234 2 3))
(define a (rt-2 '(lambda ()
		   (let* ((a 9.2)
			  (b (inexact->exact a)))
		     (set! b (exact->inexact (car l)))		     
		     b))))

(rt-funcall a)

(define gen (make-readin "/home/kjetil/t1.wav"))
(define gen (make-oscil))

(define a (rt-2 '(lambda ()
		   (printf "length: %d, loc: %d\\n" (the <int> (mus-length gen))
			   (the <int> (mus-location gen)))
		   (readin gen))))
		   (oscil gen))))




(define gen (make-readin "/home/kjetil/t1.wav"))
(define gen2 (make-rt-readin gen))
(begin gen2)

(mus-location gen2)

(rt-readin-channels gen2)

(define a (rt-2 '(lambda ()
		   (let ((a (lambda (ai)
			      (mus-location ai))))
		     (a gen2)))))


(rt-funcall a)

(macroexpand '(eval-c-macro-rt-readin-location gen5))

(begin rt-macro-setter!-mus-location)
(begin rt-macro-rt-mus-location)


c-name mus_location
funcname rt-mus-location/mus_location
rt-name mus-location

c-name mus_set_location
funcname setter!-mus-location/mus_set_location
rt-name setter!-mus-location

(rt-safety)
(set! (rt-safety) 0)

(define b (cons 2 3))
(define a (rt-2 '(lambda ()
		   (readin gakk))))
		   (the <int> (car b)))))


(definstrument (i)
  (let ((rs (make-readin "/home/kjetil/t1.wav")))
    (-> (rt (lambda ()
		 (if (>= (mus-location rs) (mus-length rs))
		     (set! (mus-location rs) 0))
		 (out (readin rs))))
	play)))

(define g (i))
(-> g stop)

(rte-info)


----
Slik fungerer det:


_En_ struct med "globale" variable.

For en slik en:
(lambda ()
  (let* ((a 3)
	 (b (lambda (c)
	      (set! a c))))
    (b 5)))

Den globale structen inneholder a: {int a},
og sliken expanderes til:

(define b (lambda (global c)
	    (set! global->a c)))
(define main (lambda (global)
	       (set! global-> a 3)
	       (b global 5)))

"global" inneholder alle variabler som brukes i mer enn 1 funksjon.


Og videre:

(lambda ()
  (let* ((a 3)
	 (b (lambda (c)
	      (let ((d (lambda ()
			 (+ c 2))))
		(set! a (d))))))
    (b 5)))

->

struct global={int a, int c}

(define d (lambda (global)
	    (+ global->c 2)))
(define b (lambda (global c)
	    (set! global->c c)
	    (set! global->a (d global))))
(define main (lambda (global)
	       (set! global->a 3)
	       (b global 5)))


--> eller ->

static int a;
static int c;

(define d (lambda ()
	    (+ c 2)))
(define b (lambda (_rt_local_c)
	    (set! c _rt_local_c)
	    (set! a (d))))
(define main (lambda ()
	       (set! a 3)
	       (b 5)))

(lambda ()
  (let* ((a 0)
	 (c 0)
	 (d (lambda ...))
	 (b (lambda :::))
	 (main (lambda ...)))))


Merk: Det vil ikke g� an � optimalisere bort global som f�rste argument, siden funksjoner
      kan sendes rundt omkring som pekere. Alts�:

        F�rste argument til _alle_ funksjoner m� v�re global.
        Forskjellige typer global vil heller ikke v�re mulig.


Optimaliseringer.
-Finn ut om en variabel er skrivbar, og bruk lokal variant hvis ikke.
-Finn ut om en variable er skrivbar, og merk den som "const" hvis ikke.
-Sorter rekkef�lga p� de globale variablene, og plasser de flest brukte f�rst. (tja....)
-global kan jo faktisk _v�re_ global, ikke bare hete global. S�nn som situasjonen er n�,
 s� kan jo aldri en funksjon bli akksessert samtidig av to tr�der.
    Ups, jo faktisk-> hvis b�de kallt med rt og rt-funcall. Men det g�r an � l�se. (eller overse...)
    Det g�r ogs� an hvis man har flere servere, med forskjellige drivere.... V�rre. Nei, g�r an � l�se hvis det skulle bli aktuellt.
        Kan jo bare dloade objekt-fila flere ganger, en gang for hver server eller noe slikt.
-Og da kan jo alle variabler i struct global bare v�re globale variabler!?
--> Dette gj�r jo at lambda-lifter funksjonene blir hyperenkel � programmere ogs�.
-----> Hvis det skulle v�re aktuelt � avglobalisere "global" seinere, s� blir jo ikke det
       s�rlig mye mer arbeid enn � lage en uglobal "global" n�.
--> Alts�, global blir bare globale variabler. Enkelt og genialt.

Forresten:
-Ufattelig enkelt � lage closures med dette systemet, bare � ta kopi av global. :-)
-Et begrenset closure-support er faktisk mulig. Lag 200 (eller noe) globals p� stacken f�r oppstart,
 og legg pekere til de i alle globalsene. N�vel. Nei, s� enkelt er det vel ikke, men uansett ikke
 s� veldig vanskelig. Uansett, ikke n�dvendig med closures.


(set! (rt-safety) 0)
(rte-reset)

(define gl 5)
(define osc (make-oscil))

(define a (rt-2 '(lambda ()
		   (let* ((ai (lambda ()
				(oscil osc))))
		     (ai)))))

(rt-funcall a)

(lambda ((_rt_localosc_u2 <mus_oscil-*>))
  (let* ((osc_u2 <mus_oscil-*>)
	 (ai_u1 <float> (lambda ()
			  (rt-oscil/mus_oscil osc_u2 0 0))))
    (set! osc_u2 _rt_localosc_u2)
    (rt-begin
     (rt-begin
      (ai_u1)))))

(lambda ((<mus_any-*> _rt_local_osc_u2))
  (let* ((osc_u2 <mus_any-*>)
	 (ai_u1 <float> (lambda ()
			  (return (rt-oscil/mus_oscil osc_u2 0 0)))))
    (return (rt-begin
	     (set! osc_u2 _rt_local_osc_u2)
	     (rt-begin
	      (rt-begin
	       (ai_u1)))))))


Oops 1:
-------
(define a2 (lambda ()
	     (letrec ((c (lambda (b)
			   (let ((inner (lambda (n)
					  (set! b n))))
			     (if (= 0 b)
				 (c 1)
				 (inner 2))
			     b))))
	       (c 0))))

(define a (rt-2 '(lambda ()
		   (define a (include-guile-func a2))
		   (a))))

(list (a2)
      (rt-funcall a))


Oops 2:
-------
(define b2 (lambda ()
		   (letrec ((c (lambda (b)
				 (let ((inner (lambda (n)
						(let ((ret (c 2)))
						  (+ b ret)))))
				   (if (= 0 b)
				       (inner 1)
				       b)))))
		     (c 0))))

(define b (rt-2 '(lambda ()
		   (define a (include-guile-func b2))
		   (a))))
		      

(list (b2)
      (rt-funcall b))


L�sning
-------
Optimalisering: Sjekk om peker til en funksjon er brukt, eller om funksjonen er kallt fra seg selv eller i en inner-function.
                Da er den potensiell rekursiv, og kan f�re til de to scenarioene over.
                (Veldig enkelt � sjekke)


Argument-variablene og de deklarerte variablene til slike funksjoner m� handteres via indirekte pekere:

(lambda (a)
  (let* ((b 0)
	 (c (lambda ()
	      (set! b 9)
	      (+ a b))))
    (set! b 5)
    (* (c) a b)))

->


(let*-globals ((a_pointer <int-*> 0)
	       (b_pointer <int-*> 0)
	       (c (lambda ()
		    (let* ((b b_pointer)
			   (a a_pointer))
		      (set! *b 9)
		      (+ *a *b))))))
(lambda (a)
  (let* ((b 0))
    (set! a_pointer (pointer-to a))
    (set! b_pointer (pointer-to b))
    (set! b 5)
    (* (c) a b)))


Litt usikker...
Nei, en helt annen funksjon kan jo ha kallt (self), f�r (c) blir kallt. Og da vil a_pointer og b_pointer ha feil
verdier. Men blir ikke det closures? Uansett, dette l�ser jo bare problem 2, og ikke 1, faktisk.


(letrec ((a (lambda (n)
	      (1+ n)))
	 (b (lambda (n)
	      (c (1+ n))))
	 (c (lambda (n)
	      (a (1+ n)))))
  (b 5))



Forslag 2:
----------
Kapsulere rekursive (og potensiellt rekursive) funksjoner: (manuell stack)


(let* ((self (lambda (a)
	       (let* ((b 0)
		      (c (lambda ()
			   (set! b 9)
			   (+ a b))))
		 (set! b 5)
		 (* (c) a b))))))
->

(let* ((a 0)
       (b 0)
       (c (lambda ()
	    (set! b 9)
	    (+ a b)))
       (self (lambda-decl (_rt_local_a)))
       (_rt_realfunc_self (lambda (_rt_local_a)
			    (set! a  _rt_local_a)
			    (set! b 5)
			    (+ (c) a b)))
       (self (lambda (_rt_local_a)
	       (let* ((old_a a)
		      (old_b b)
		      (ret (_rt_realfunc_self _rt_local_a)))
		 (set! a old_a)
		 (set! b old_b)
		 ret)))))


Ser riktig ut. Tror dette skulle virke...



Fors�k med egen funksjon f�r lambda-lifter:
-------------------------------------------

(let* ((self (lambda (a)
	       (1+ a)))))

->

(let* ((self (lambda (a)
	       (let* ((old_a a)
		      (rt_gen234 (lambda ()
				   (1+ a)))
		      (ret (rt_gen234 a)))
		 (set! a old_a)
		 ret)))))

Alle _ber�rte_ variabler m� kapsuleres. I en to-stegs prosess kan dette ordnes ved � flytte
find-globals til toplevel. Eller, heller flytte lambda-lifter inn. Ja, ikke noe problem i hvert fall.

Hva med variabler som er definert i inner-functions? M� de kapsuleres p� hoved-funksjonen?

(let* ((self (lambda ()
	       (let* ((a (lambda (b)
			   (let ((c (lambda ()
				      b)))
			     (1+ (c))))))
		 ...)))))

M� "b" kapsuleres i self her hvis self er potensielt rekursiv?

(Og hva hvis a eller c er potensielt rekursive? Tja, dobbel kapsulering burde ikke gi feil resultat, s� det er vel greit.)

Nei, "b" trenger da aldeles ikke kapsuleres der. Den globale varianten blir jo ikke satt.


Oops 3:
-------

(lambda ()
  (let ((p #f)
	(self (lambda (a)
		(let ((b (lambda ()
			   (p))))
		  ...)))
	(g (lambda ()
	     (self))))
    (set! p g)
    (self)))

Hvilket betyr: S� godt som umulig � finne ut ved kompileringstid om en funksjon (i dette tilfellet "self") er potensielt rekursiv. Arrgh.

Runtime-sjekk:

(let* ((self (lambda (_rt_local_a)
	       (if _rt_realfunc_self_rec
		   (let* ((old_a a)
			  (old_b b)
			  (ret (_rt_realfunc_self _rt_local_a)))
		     (set! a old_a)
		     (set! b old_b)
		     ret)
		   (let* ((ret 0))
		     (set! _rt_realfunc_self_rec 1)
		     (set! ret (_rt_realfunc_self _rt_local_a))
		     (set! _rt_realfunc_self_rec 0)
		     ret))))))


	       




(rt-expand-macros '(= a 3))

(define a (rt-1 '(lambda ()
		   (hz->radians freq))))


(define freq 6700)
(rt-funcall a)
(hz->radians freq)

(hz->radians 500.0)

(define a (rt-2 '(lambda ()
		   (set! b (if b
			       (begin
				 (range i 0 1
					(set! b 9))
				 9)
			       8)))))
			 

(define src-gen (make-src))
(define i (rt-2
	   '(lambda ()
	      (src src-gen 1.1 (lambda (dir)
				5)))))

;;				(oscil osc))))))


(define i (rt-compile (lambda ()
			(lambda ()
			  (+ 2 src5)))))



!#



