(provide 'write.scm)

;;; pretty-print:   the usual pretty printer, intended for s7
;;; checkpoint:     use write-readably to save the current runtime state of s7 as a loadable scheme file


;;; -------------------------------- pretty-print --------------------------------

(define *pp-max-column* 100)

(define* (pretty-print obj (port (current-output-port)) (column 0))

  (define (pretty-print-1 obj port column)
    (define (spaces n) 
      (write-char #\newline port)
      (do ((i 0 (+ i 1))) ((= i n)) (write-char #\space port)))
    
    (define (stacked-list lst col)
      (do ((l1 lst (cdr l1)))
	  ((not (pair? l1)))
	(let ((added 0))
	  (if (not (eq? l1 lst)) (spaces col))
	  (let* ((str (object->string (car l1)))
		 (len (length str)))
	    (if (and (keyword? (car l1))
		     (pair? (cdr l1)))
		(begin
		  (write (car l1) port)
		  (write-char #\space port)
		  (set! added (+ 1 len))
		  (set! l1 (cdr l1))))
	    (if (pair? l1)
		(if (and (pair? (car l1))
			 (pair? (cdar l1))
			 (null? (cddar l1))
			 (> len (/ *pp-max-column* 2)))
		    (begin
		      (write-char #\( port)
		      (pretty-print-1 (caar l1) port col)
		      (spaces (+ col 1))
		      (pretty-print-1 (cadar l1) port (+ col 1))
		      (write-char #\) port))
		    (pretty-print-1 (car l1) port (+ col added)))
		(format port " . ~S" l1)))
	  (set! added 0))))
    
    (define (stacked-split-list lst col)
      (if (pair? lst)
	  (do ((l1 lst (cdr l1)))
	      ((not (pair? l1)))
	    (if (not (eq? l1 lst)) (spaces col))
	    (write-char #\( port)
	    (if (pair? (car l1))
		(begin
		  (write (caar l1) port)
		  (write-char #\space port)
		  (if (and (pair? (cdar l1))
			   (symbol? (caar l1)))
		      (pretty-print-1 (cadar l1) port (+ col (length (symbol->string (caar l1))) 2))
		      (write (cdar l1) port)))
		(write (car l1) port))
	    (write-char #\) port))
	  (write lst port)))
    
    (define (messy-number z)
      (if (real? z)
	  (if (or (nan? z)
		  (infinite? z))
	      (object->string z)
	      (if (= z pi)
		  "pi"
		  (format #f "~,4f" z)))
	  (format "~A~A~Ai" 
		  (messy-number (real-part z))
		  (if (negative? (imag-part z)) "-" "+")
		  (messy-number (abs (imag-part z))))))

    (define (any-keyword? lst)
      (and (pair? lst)
	   (or (keyword? (car lst))
	       (any-keyword? (cdr lst)))))
    
    (cond ((number? obj)
	   (if (rational? obj)
	       (write obj port)
	       (display (messy-number obj) port)))
	  
	  ((pair? obj)
	   (let ((cobj (if (symbol? (car obj)) (string->symbol (symbol->string (car obj))) (car obj)))) ; this clears out some optimization confusion
	     (case cobj
	       
	       ((lambda lambda* define* define-macro define-macro* define-bacro define-bacro* with-environment when unless
		 call-with-input-string call-with-input-file call-with-output-file
		 with-input-from-file with-input-from-string with-output-to-file)
		(if (or (not (pair? (cdr obj))) ; (when) or (when . #t)
			(not (pair? (cddr obj))))
		    (write obj port)
		    (begin
		      (format port "(~A ~A" (car obj) (cadr obj))
		      (spaces (+ column 2))
		      (stacked-list (cddr obj) (+ column 2))
		      (write-char #\) port))))

	       ((defmacro defmacro*)
		(if (or (not (pair? (cdr obj)))
			(not (pair? (cddr obj))))
		    (write obj port)
		    (begin
		      (format port "(~A ~A ~A" (car obj) (cadr obj) (caddr obj))
		      (spaces (+ column 2))
		      (stacked-list (cdddr obj) (+ column 2))
		      (write-char #\) port))))
	       
	       ((define)
		(if (not (pair? (cdr obj)))
		    (write obj port)
		    (begin
		      (format port "(~A ~A " (car obj) (cadr obj))
		      (if (pair? (cadr obj))
			  (begin
			    (spaces (+ column 2))
			    (stacked-list (cddr obj) (+ column 2)))
			  (begin
			    (if (pair? (cddr obj))
				(let ((str (object->string (caddr obj))))
				  (if (> (length str) 60)
				      (begin
					(spaces (+ column 2))
					(pretty-print-1 (caddr obj) port (+ column 2)))
				      (write (caddr obj) port)))
				(write (cddr obj) port))))
		      (write-char #\) port))))
	       
	       ((do)
		(if (not (pair? (cdr obj)))
		    (write obj port)
		    (begin
		      (format port "(do (")
		      (if (pair? (cadr obj))
			  (stacked-list (cadr obj) (+ column 5)))
		      (write-char #\) port)
		      (if (pair? (cddr obj))
			  (let ((end (caddr obj)))
			    (spaces (+ column 4))
			    (if (< (length (object->string end)) (- *pp-max-column* column))
				(write end port)
				(begin
				  (write-char #\( port)
				  (pretty-print-1 (car end) port (+ column 4))
				  (spaces (+ column 5))
				  (stacked-list (cdr end) (+ column 5))
				  (write-char #\) port)))
			    (spaces (+ column 2))
			    (stacked-list (cdddr obj) (+ column 2))
			    (write-char #\) port))
			  (write-char #\) port)))))
	       
	       ((cond)
		(format port "(cond ")
		(stacked-list (cdr obj) (+ column 6))
		(write-char #\) port))
	       
	       ((or and)
		(if (> (length (object->string obj)) 40)
		    (begin
		      (format port "(~A " (car obj))
		      (stacked-list (cdr obj) (+ column 2 (length (symbol->string (car obj)))))
		      (write-char #\) port))
		    (write obj port)))
	       
	       ((case)
		(if (not (pair? (cdr obj)))
		    (write obj port)
		    (begin
		      (format port "(case ~A" (cadr obj)) ; send out the selector
		      (do ((lst (cddr obj) (cdr lst)))
			  ((not (pair? lst)))
			(spaces (+ column 2))
			(if (not (pair? (car lst)))
			    (write (car lst) port)
			    (begin
			      (write-char #\( port)
			      (if (not (pair? (caar lst)))
				  (write (caar lst) port)
				  (let ((len (length (caar lst))))
				    (if (< len 6)
					(write (caar lst) port)
					(let ((p (caar lst)))
					  (write-char #\( port)
					  (do ((i 0 (+ i 6)))
					      ((>= i len))
					    (do ((j 0 (+ j 1)))
						((or (= j 6)
						     (null? p))
						 (if (pair? p) (spaces (+ column 4))))
					      (write (car p) port)
					      (set! p (cdr p))
					      (if (pair? p) (write-char #\space port))))
					  (write-char #\) port)))))
			      (if (and (pair? (cdar lst))
				       (null? (cddar lst))
				       (< (length (object->string (cadar lst))) 60))
				  (begin
				    (write-char #\space port)
				    (write (cadar lst) port))
				  (begin
				    (spaces (+ column 3))
				    (stacked-list (cdar lst) (+ column 3))))
			      (write-char #\) port))))
		      (write-char #\) port))))
		    
	       ((begin call-with-exit call/cc call-with-current-continuation with-baffle with-output-to-string call-with-output-string
		       map for-each)
		(format port "(~A" (car obj))
		(if (pair? (cdr obj))
		    (begin
		      (spaces (+ column 2))
		      (stacked-list (cdr obj) (+ column 2))))
		(write-char #\) port))
	       
	       ((dynamic-wind)
		(format port "(dynamic-wind")
		(spaces (+ column 2))
		(stacked-list (cdr obj) (+ column 2))
		(write-char #\) port))
	       
	       ((if)
		(let ((objstr (object->string obj))
		      (ifcol (+ column 4)))
		  (if (< (length objstr) 40)
		      (display objstr port)
		      (begin
			(format port "(if ")
			(pretty-print-1 (cadr obj) port ifcol)
			(spaces (+ column 4))
			(pretty-print-1 (caddr obj) port ifcol)
			(if (pair? (cdddr obj))
			    (begin
			      (spaces (+ column 4))
			      (pretty-print-1 (cadddr obj) port ifcol)))
			(write-char #\) port)))))
	       
	       ((let let* letrec letrec*)
		(if (or (not (pair? (cdr obj)))
			(not (pair? (cddr obj))))
		    (write obj port)
		    (let ((head-len (length (symbol->string (car obj)))))
		      (if (symbol? (cadr obj))
			  (begin
			    (format port "(~A ~A (" (car obj) (cadr obj))
			    (if (pair? (cddr obj))
				(if (pair? (caddr obj)) ; (let x () ...)
				    (stacked-split-list (caddr obj) (+ column head-len (length (symbol->string (cadr obj))) 4))
				    (write (caddr obj) port))
				(if (not (null? (cddr obj)))
				    (format port " . ~S" (cddr obj)))))
			  (begin
			    (format port "(~A (" (car obj))
			    (if (pair? (cadr obj))
				(stacked-split-list (cadr obj) (+ column head-len 3)))))
		      (write-char #\) port)
		      (spaces (+ column 2))
		      (if (pair? ((if (symbol? (cadr obj)) cdddr cddr) obj))
			  (stacked-list ((if (symbol? (cadr obj)) cdddr cddr) obj) (+ column 2)))
		      (write-char #\) port))))
		
	       ((environment*)
		(format port "(environment*")
		(if (pair? (cdr obj))
		    (do ((lst (cdr obj) (cddr lst)))
			((or (not (pair? lst))
			     (not (pair? (cdr lst)))))
		      (spaces (+ column 2))
		      (if (pair? (cdr lst))
			  (begin
			    (write (car lst) port)
			    (write-char #\space port)
			    (pretty-print-1 (cadr lst) port (+ column 2 (length (object->string (car lst))))))
			  (write lst port))))
		(write-char #\) port))
	       
	       ((set!)
		(let ((str (object->string obj)))
		  (if (> (length str) 60)
		      (let ((settee (object->string (cadr obj))))
			(format port "(set! ~A" settee)
			(if (> (length settee) 20)
			    (begin
			      (spaces (+ column 6))
			      (pretty-print-1 (caddr obj) port (+ column 6)))
			    (begin
			      (write-char #\space port)
			      (pretty-print-1 (caddr obj) port (+ column 7 (length settee)))))
			(write-char #\) port))
		      (display str port))))
	       
	       ((quote)
		(if (not (pair? (cdr obj))) ; (quote) or (quote . 1)
		    (write obj port)
		    (begin
		      (write-char #\' port)
		      (pretty-print-1 (cadr obj) port column))))
	       
	       (else
		(let* ((objstr (object->string obj))
		       (strlen (length objstr)))
		  (if (< (+ column strlen) *pp-max-column*)
		      (display objstr port)
		      (let ((lstlen (length obj)))
			(if (or (infinite? lstlen)
				(< lstlen 2))
			    (display objstr port)
			    (if (and (pair? (car obj))
				     (memq (caar obj) '(lambda lambda*)))
				(begin
				  (write-char #\( port)
				  (pretty-print-1 (car obj) port column)
				  (spaces (+ column 1))
				  (display (cadr obj) port)
				  (write-char #\) port))
				(let* ((carstr (object->string (car obj)))
				       (carstrlen (length carstr)))
				  (if (eq? (car obj) 'quote)
				      (write-char #\' port)
				      (format port "(~A" carstr))
				  (if (any-keyword? (cdr obj))
				      (begin
					(spaces (+ column 2))
					(stacked-list (cdr obj) (+ column 2)))
				      (let ((line-len (ceiling (/ (- strlen carstrlen) 40)))
					    (line-start (+ column 2 carstrlen)))
					(if (= lstlen 2)
					    (begin
					      (write-char #\space port)
					      (pretty-print-1 (cadr obj) port line-start))
					    (if (< lstlen 5)
						(begin
						  (write-char #\space port)
						  (stacked-list (cdr obj) line-start))
						(let ((lst (cdr obj)))
						  (do ((i 1 (+ i line-len)))
						      ((>= i lstlen))
						    (do ((k 0 (+ k 1)))
							((or (null? lst)
							     (= k line-len)))
						      (let ((str (format #f "~S" (car lst))))
							(if (> (length str) (- *pp-max-column* line-start))
							    (begin
							      (if (not (zero? k)) (spaces line-start))
							      (pretty-print-1 (car lst) port line-start))
							    (begin
							      (if (or (not (zero? k)) (= i 1)) (write-char #\space port))
							      (display str port))))
						      (set! lst (cdr lst)))
						    (if (pair? lst)
							(spaces line-start))))))))
				  (if (not (eq? (car obj) 'quote))
				      (write-char #\) port))))))))))))
	  (else
	   (write obj port))))
  
  (let ((old-port port))
    (if (boolean? old-port)
	(set! port (open-output-string)))
    (pretty-print-1 obj port column)
    (flush-output-port port)
    (if (boolean? old-port)
	(let ((str (get-output-string port)))
	  (close-output-port port)
	  (if (eq? old-port #t)
	      (display str))
	  str)
	(values))))


(define (pp obj)
  (call-with-output-string
    (lambda (p)
      (pretty-print obj p))))

(define (test-pretty-print)

  (if (not (string=? (pp '(lambda* (a b) (+ a b) (* 1 2))) "(lambda* (a b)\n  (+ a b)\n  (* 1 2))"))
      (format *stderr* "pp 1"))

  (if (not (string=? (pp '(let ((a 1) (b 2)) (+ a b))) "(let ((a 1)\n      (b 2))\n  (+ a b))"))
      (format *stderr* "pp 2"))

  (if (not (string=? (pp '(let () (+ a b))) "(let ()\n  (+ a b))"))
      (format *stderr* "pp 2a"))

  (if (not (string=? (pp '(begin (+ 1 2) (* 2 3))) "(begin\n  (+ 1 2)\n  (* 2 3))"))
      (format *stderr* "pp 3"))

  (if (not (string=? (pp '(case a ((a b c) 1) ((d) 2) (else 3))) "(case a\n  ((a b c) 1)\n  ((d) 2)\n  (else 3))"))
      (format *stderr* "pp 4: ~A" (pp '(case a ((a b c) 1) ((d) 2) (else 3)))))

  (if (not (string=? (pp '(cond ((> a 1) 2) ((< a 3) 3) (#t 4))) "(cond ((> a 1) 2)\n      ((< a 3) 3)\n      (#t 4))"))
      (format *stderr* "pp 5"))

  (if (not (string=? (pp '(if a '(1 2 3))) "(if a '(1 2 3))"))
      (format *stderr* "pp7"))
  )

;(test-pretty-print)

(define (pretty-print-all)
  (let ((st (symbol-table)))
    (for-each
     (lambda (sym)
       (if (defined? sym)
	   (let ((val (symbol->value sym)))
	     (let ((source (and (procedure? val)
				(procedure-source val))))
	       (if (pair? source)
		   (format *stderr* "~<sym~> ~<val~>:~%~<(pp source)~>~%~%"))))))
     st)))



 
#|
;;; -------------------------------- checkpoint --------------------------------

(define* (checkpoint (file "checkpoint-s7.scm"))
  (call-with-output-file file
    (lambda (p)
      (let ((st (symbol-table)))
	  (for-each
	   (lambda (sym)
	     (if (defined? sym)
		 (let ((choice (*autoload* sym)))
		   (if (string? choice)
		       (format p "(if (not (defined? '~A)) (load ~S))~%" sym choice)
		       (if (procedure? choice)
			   (format p "(if (not (defined? '~A)) ((~S) (current-environment)))~%" sym choice))))))
	   st)

      ;; now presumably we've loaded all the findable files, and called the autoload functions
      ;; run through the table again checking for diffs or omissions -- will this cover all the s7 settings?

	  (for-each
	   (lambda (sym)
	     (if (and (defined? sym)
		      (not (constant? sym))
		      (not (memq sym '(i st p file 
					 
					 ;; s7
					 multiple-value-bind letrec* *load-path* macroexpand *safety* *maximum-stack-size*
					 *#readers* *vector-print-length* *gc-stats* multiple-value-set! cond-expand
					 *features* call-with-values

					 ;; snd
					 *snd-opened-sound* break break-ok break-enter break-exit undo-edit redo-edit
					 ))))
		 ;; here we need to leave out built-in functions like abs 
		 (let ((choice (*autoload* sym)))
		   (if (and (not choice)
			    (not (string=? (object->string (symbol->value sym)) (object->string sym))))
		       (begin
			 (catch #t
			   (lambda ()
			     (let ((str (object->string (symbol->value sym) :readable)))
			       (format p "(if (not (defined? '~A)) (define ~A ~A))~%" sym sym str)))
			   (lambda args
			     (format *stderr* "~A not saved~%" sym))))))))
	   st)

	;; now look for changes?  This probably can't work for macros/functions (not equal? anyway)

      )))
  #f)
|#
