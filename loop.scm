;;; reimplement a few common lisp-isms that I like
;;;
;;; ok: dotimes, dolist, when, unless, progn, prog1, if*
;;; not complete: loop

(use-modules (ice-9 syncase))


(define-syntax loop
  ;; how to handle multiple, unordered phrases?
  (syntax-rules (for from below to by downto do)
    ((loop for <counter> from <start> below <finish> do <body> ...)
     (do ((<counter> <start> (+ <counter> 1)))
	 ((>= <counter> <finish>) #f)
       <body> ...))
    ((loop for <counter> from <start> below <finish> by <step> do <body> ...)
     (do ((<counter> <start> (+ <counter> <step>)))
	 ((>= <counter> <finish>))
       <body> ...))
    ((loop for <counter> from <start> to <finish> do <body> ...)
     (do ((<counter> <start> (+ <counter> 1)))
	 ((> <counter> <finish>) #f)
       <body> ...))
    ((loop for <counter> from <start> to <finish> by <step> do <body> ...)
     (do ((<counter> <start> (+ <counter> <step>)))
	 ((> <counter> <finish>) #f)
       <body> ...))
    ((loop for <counter> from <start> downto <finish> do <body> ...)
     (do ((<counter> <start> (- <counter> 1)))
	 ((< <counter> <finish>) #f)
       <body> ...))
    ((loop for <counter> from <start> do <body> ...)
     (do ((<counter> <start> (+ <counter> 1)))
	 (() #f)
       <body> ...))
    ;; across in on, func as step? (#'cddr)
    ))

    
(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (<counter> <finish> <result>) <body> ...)
     (do ((<counter> 0 (+ <counter> 1)))
	 ((>= <counter> <finish>) <result>)
       <body> ...))
    ((dotimes (<counter> <finish>) <body> ...)
     (do ((<counter> 0 (+ <counter> 1)))
	 ((>= <counter> <finish>))
       <body> ...))
    ))
    

(define-syntax dolist
  (syntax-rules ()
    ((dolist (<var> <list>)) #f)
    ((dolist (<var> <list> <result>) <body> ...)
     (let ((<new-list> <list>))
       (do ()
	   ((null? <new-list>) <result>)
	 (let ((<var> (car <new-list>)))
	   <body> ...)
	 (set! <new-list> (cdr <new-list>)))))
    ((dolist (<var> <list>) <body> ...)
     (let ((<new-list> <list>))
       (do ()
	   ((null? <new-list>) #f)
	 (let ((<var> (car <new-list>)))
	   <body> ...)
	 (set! <new-list> (cdr <new-list>)))))
    ))
    

(define-syntax when
  (syntax-rules ()
    ((when <test>) #f)
    ((when <test> <form> ...)
     (if <test> 
	 (begin <form> ...)))))


(define-syntax unless
  (syntax-rules ()
    ((unless <test>) #f)
    ((unless <test> <form> ...)
     (if (not <test> )
	 (begin <form> ...)))))


(define-syntax progn
  (syntax-rules ()
    ((progn) #f)
    ((progn <body> ...)
     (begin <body> ...)) ; but "begin" isn't guaranteed to return its last form?
    ))


(define-syntax prog1
  (syntax-rules ()
    ((prog1 <form1>) <form1>)
    ((prog1 <form1> <form2> ...)
     (let ((_result_ <form1>)) <form2> ... _result_))
    ))


(define-syntax if*
  (syntax-rules ()
    ((if* <form1> <form2>) (if <form1> <form2> #f))
    ((if* <form1> <form2> <form3>) (if <form1> <form2> <form3>))))


