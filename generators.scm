(provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" is placed first.
;;;   Where a factor is involved, I'll use "r".
;;;   Where the number of terms in the sum is settable, I'll use "n".
;;;   if you actually want to set the initial-phase, it is usually "angle" just after the documented arguments


(define nearly-zero 1.0e-12) ; 1.0e-14 in clm.c, but that is trouble here (noddcos)

;;; --------------------------------------------------------------------------------
;;;
;;; defgenerator

(if (provided? 'snd-guile)
    (define (symbol->value sym)
      (if (defined? 'module-ref)
	  (module-ref (current-module) sym) ; symbol-binding is deprecated
	  (symbol-binding #f sym))))


(if (provided? 'snd-gauche)
    (define (symbol->value sym)
      (global-variable-ref (current-module) sym)))



(defmacro defgenerator (struct-name . fields)

  ;; an extension of def-clm-struct

  ;; this adds the built-in methods mus-name, mus-reset, mus-run, and mus-describe (if they don't already exist), and
  ;;   mus-frequency if a "frequency" field exists (treated as radians)
  ;;   mus-phase if a "phase" or "angle" field exists
  ;;   mus-scaler if "r" or "amplitude",
  ;;   mus-order if "n" or "order"
  ;;   mus-offset if "ratio" (mimics nrxy*)

  (let* ((name (if (list? struct-name) (car struct-name) struct-name))

	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (list-ref struct-name 1) :make-wrapper)
				    (list-ref struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (list-ref struct-name 3) :make-wrapper)
				    (list-ref struct-name 4))))
		      (lambda (gen) gen)))

	 (sname (if (string? name) name (symbol->string name)))

	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))

	 (field-types (map (lambda (n)
			     (if (and (list? n) (cadr n) (eq? (cadr n) :type)) 
				 (snd-error (format #f ":type indication for defgenerator (~A) field (~A) should be after the default value" name n)))
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (list-ref n 2) :type))
				 (list-ref n 3)
				 (if (and (list? n)
					  (= (length n) 2))
				     (if (number? (cadr n))
					 (if (exact? (cadr n))
					     'int
					     'float)
					 (if (string? (cadr n))
					     'string
					     (if (char? (cadr n))
						 'char
						 (if (or (equal? (cadr n) #t)
							 (equal? (cadr n) #f))
						     'boolean
						     'float))))
				     'float)))
			   fields))

	 (find-if (lambda (pred l)
		    (cond ((null? l) #f)
			  ((pred (car l)) (car l))
			  (else (find-if pred (cdr l))))))

	 (original-methods (or (and (list? struct-name)
				    (or (and (> (length struct-name) 2)
					     (equal? (list-ref struct-name 1) :methods)
					     (list-ref struct-name 2))
					(and (= (length struct-name) 5)
					     (equal? (list-ref struct-name 3) :methods)
					     (list-ref struct-name 4))))
			       '()))

	 (method-exists? (lambda (method)
			   (and (not (null? original-methods))
				(find-if (lambda (g)
					   (and (list? g)
						(list? (cadr g))
						(eq? (car (cadr g)) method)))
					 (cdr original-methods)))))

	 (phase-field-name (and (not (method-exists? 'mus-phase))
				(let ((fld (find-if (lambda (name) 
						      (or (string=? name "phase") 
							  (string=? name "angle")))
						    field-names)))
				  (and fld (string-append "-" fld)))))

	 (frequency-field-name (and (not (method-exists? 'mus-frequency))
				    (find-if (lambda (name) 
					       (string=? name "frequency"))
					     field-names)
				    "-frequency"))

	 (offset-field-name (and (not (method-exists? 'mus-offset))
				 (find-if (lambda (name) 
					    (string=? name "ratio"))
					  field-names)
				 "-ratio"))

	 (scaler-field-name (and (not (method-exists? 'mus-scaler))
				 (let ((fld (find-if (lambda (name) 
						       (or (string=? name "r")
							   (string=? name "amplitude")))
						     field-names)))
				   (and fld (string-append "-" fld)))))

	 (order-field-name (and (not (method-exists? 'mus-order))
				(let ((fld (find-if (lambda (name) 
						      (or (string=? name "n") 
							  (string=? name "order")))
						    field-names)))
				  (and fld (string-append "-" fld)))))

	 (methods `(append (if ,(not (null? original-methods))  ; using append to splice out unwanted entries
			       ,original-methods
			       (list))
			   
			   (if ,phase-field-name
			       (list 
				(list 'mus-phase
				      (lambda (g)
					(,(string->symbol (string-append sname (or phase-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or phase-field-name "oops"))) g) val))))
			       (list))
			   
			   (if ,frequency-field-name
			       (list 
				(list 'mus-frequency
				      (lambda (g)
					(radians->hz (,(string->symbol (string-append sname (or frequency-field-name "oops"))) g)))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or frequency-field-name "oops"))) g) (hz->radians val))
					val)))
			       (list))
			   
			   (if ,offset-field-name
			       (list 
				(list 'mus-offset
				      (lambda (g)
					(,(string->symbol (string-append sname (or offset-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or offset-field-name "oops"))) g) val)
					val)))
			       (list))
			   
			   (if ,order-field-name
			       (list  ; not settable -- maybe use mus-length?
				(list 'mus-order
				      (lambda (g)
					(,(string->symbol (string-append sname (or order-field-name "oops"))) g))))
			       (list))
			   
			   (if ,scaler-field-name
			       (list 
				(list 'mus-scaler
				      (lambda (g)
					(,(string->symbol (string-append sname (or scaler-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or scaler-field-name "oops"))) g) val))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-describe))
			       (list 
				(list 'mus-describe
				      (lambda (g)
					(let ((desc (mus-name g))
					      (first-time #t))
					  (for-each
					   (lambda (field)
					     (set! desc (string-append desc 
								       (format #f "~A~A: ~A"
									       (if first-time " " ", ")
									       field
									       (if (string=? field "frequency")
										   (radians->hz ((symbol->value (string->symbol (string-append ,sname "-" field))) g))
										   ((symbol->value (string->symbol (string-append ,sname "-" field))) g)))))
					     (set! first-time #f))
					   (list ,@field-names))
					  desc))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-run))
			       (list
				(list 'mus-run
				      (lambda (g arg1 arg2)
					(,(string->symbol sname) g arg1)))))
			   
			   (if ,(not (method-exists? 'mus-reset))
			       (list 
				(list 'mus-reset
				      (lambda (g)
					(for-each
					 (lambda (name type orig)
					   (if (or (not (string=? type "clm"))
						   (not ((symbol->value (string->symbol (string-append ,sname "-" name))) g)))
					       (set! ((symbol->value (string->symbol (string-append ,sname "-" name))) g) orig)
					       (mus-reset ((symbol->value (string->symbol (string-append ,sname "-" name))) g))))
					 (list ,@field-names)
					 (list ,@(map symbol->string field-types))
					 (list ,@(map (lambda (n)
							(if (and (list? n)
								 (>= (length n) 2))
							    (cadr n)
							    0.0))
						      fields))))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-name))
			       (list 
				(list 'mus-name
				      (lambda (g) 
					,sname)
				      (lambda (g new-name)
					(set-car! (cdr (assoc 'mus-name (list-ref g (1- (length g)))))
						  (lambda (g) 
						    new-name))))) ; depend on closures?
			       (list)))))
    
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   "clm struct type check"
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))

       (define ,(string->symbol (string-append sname "-methods"))
	 (lambda ()
	   "clm struct local method list accessor"
	   ,methods))

       (def-optkey-fun (,(string->symbol (string-append "make-" sname))
		        ,@(map (lambda (n)
				(if (and (list? n)
					 (>= (length n) 2))
				    (list (car n) (cadr n))
				    (list n 0.0)))
			      fields))
	 (,wrapper (if (list? ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)
			     ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)))))

       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   "clm struct field accessor"
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field sname (string-append sname "-" n) ctr type)
		    (set! ctr (1+ ctr))
		    val)))
	      field-names field-types))))



;;; --------------------------------------------------------------------------------

;;; nssb (see nxycos) -- wouldn't a more consistent name be nxycos? but it already exists -- perhaps delete nssb?

(defgenerator (nssb 
	       :make-wrapper (lambda (g)
			       (set! (nssb-frequency g) (hz->radians (nssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nssb gen fm)
  "  (make-nssb frequency (ratio 1.0) (n 1)) creates an nssb generator, similar to nxysin.\n\
   (nssb gen fm) returns n sinusoids from frequency spaced by frequency * ratio."
  (declare (gen nssb) (fm float))
  (let* ((n (nssb-n gen))
	 (cx (nssb-angle gen))
	 (mx (* cx (nssb-ratio gen)))
	 (den (sin (* 0.5 mx))))

    (set! (nssb-angle gen) (+ fm cx (nssb-frequency gen)))

    (if (< (abs den) nearly-zero)
	-1.0
	(/ (- (* (sin cx) 
		 (sin (* mx (/ (+ n 1) 2)))
		 (sin (/ (* n mx) 2)))
	      (* (cos cx) 
		 0.5 (+ den (sin (* mx (+ n 0.5))))))
	   (* (+ n 1) den)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 0.1 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nssb gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 0.1 3))
	(vib (make-oscil 5.0))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000 :scaler 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* (env ampf) 
		    (nssb gen (* (hz->radians 100.0) 
				 (oscil vib))))))))))
|#


;;; --------------------------------------------------------------------------------

;;; G&R 1st col rows 1&2

(defgenerator (nxysin
	       :make-wrapper (lambda (g)
			       (set! (nxysin-frequency g) (hz->radians (nxysin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nxysin gen fm)
  "  (make-nxysin frequency (ratio 1.0) (n 1)) creates an nxysin generator.\n\
   (nxysin gen fm) returns n sines from frequency spaced by frequency * ratio."
  (declare (gen nxysin) (fm float))
  (let* ((x (nxysin-angle gen))
	 (y (* x (nxysin-ratio gen)))
	 (n (nxysin-n gen))
	 (den (sin (* y 0.5))))

    (set! (nxysin-angle gen) (+ fm x (nxysin-frequency gen)))

    (if (< (abs den) nearly-zero)
	0.0
	(/ (* (sin (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   den))))

;;; normalization here is hard (depends on x and y)
#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-nxysin 300 1/3 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxysin gen 0.0)))))))
|#


(defgenerator (nxycos
	       :make-wrapper (lambda (g)
			       (set! (nxycos-frequency g) (hz->radians (nxycos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nxycos gen fm)
  "  (make-nxycos frequency (ratio 1.0) (n 1)) creates an nxycos generator.\n\
   (nxycos gen fm) returns n cosines from frequency spaced by frequency * ratio."
  (declare (gen nxycos) (fm float))
  (let* ((x (nxycos-angle gen))
	 (y (* x (nxycos-ratio gen)))
	 (n (nxycos-n gen))
	 (den (sin (* y 0.5))))

    (set! (nxycos-angle gen) (+ x fm (nxycos-frequency gen)))

    (if (< (abs den) nearly-zero)
	1.0
	(/ (* (cos (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   (* n den))))) ; n=normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxycos 300 1/3 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxycos gen 0.0)))))))
|#

;;; is there any need for the (-1)^k case?  


;;; --------------------------------------------------------------------------------
;;;
;;; G&R 1st col rows 3 4

(defgenerator (nxy1cos
	       :make-wrapper (lambda (g)
			       (set! (nxy1cos-frequency g) (hz->radians (nxy1cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nxy1cos gen fm)
  "  (make-nxy1cos frequency (ratio 1.0) (n 1)) creates an nxy1cos generator.\n\
   (nxy1cos gen fm) returns 2n-1 cosines from frequency spaced by frequency * ratio with every other cosine multiplied by -1."
  (declare (gen nxy1cos) (fm float))
  (let* ((x (nxy1cos-angle gen))
	 (y (* x (nxy1cos-ratio gen)))
	 (n (nxy1cos-n gen))
	 (den (cos (* y 0.5))))

    (set! (nxy1cos-angle gen) (+ x fm (nxy1cos-frequency gen)))

    (max -1.0
	 (min 1.0
	      (/ (* (sin (* n y))
		    (sin (+ x (* (- n 0.5) y))))
		 (* 2 n den))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 1/3 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1cos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 1/3 3))
	(gen1 (make-nxycos 300 1/3 6)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* 0.4 (+ (nxycos gen1 0.0) (nxy1cos gen 0.0)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos (radians->hz (* .01 pi)) 1.0 3)))
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1cos gen 0.0)))))
|#


(defgenerator (nxy1sin
	       :make-wrapper (lambda (g)
			       (set! (nxy1sin-frequency g) (hz->radians (nxy1sin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nxy1sin gen fm)
  "  (make-nxy1sin frequency (ratio 1.0) (n 1)) creates an nxy1sin generator.\n\
   (nxy1sin gen fm) returns 2n-1 sines from frequency spaced by frequency * ratio with every other sine multiplied by -1."
  (declare (gen nxy1sin) (fm float))
  (let* ((x (nxy1sin-angle gen))
	 (y (* x (nxy1sin-ratio gen)))
	 (n (nxy1sin-n gen))
	 (den (cos (* y 0.5))))

    (set! (nxy1sin-angle gen) (+ x fm (nxy1sin-frequency gen)))

    (/ (* (sin (+ x (* 0.5 (- n 1) (+ y pi))))
	  (sin (* 0.5 n (+ y pi))))
       (* n den)))) ; norm not right...

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1sin 300 1/3 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1sin gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

;;; n odd sinusoids: noddsin, noddcos, noddssb

;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

(defgenerator (noddsin 
	       :make-wrapper (lambda (g)
			       (if (< (noddsin-n g) 1) (set! (noddsin-n g) 1))
			       (set! (noddsin-frequency g) (hz->radians (noddsin-frequency g)))
			       (set! (noddsin-norm g) (if (= (noddsin-n g) 1) 1.0
							  (/ (if (= (noddsin-n g) 2) 1.29
								 (if (= (noddsin-n g) 3) 1.34
								     (if (< (noddsin-n g) 6) 1.36
									 (if (< (noddsin-n g) 18) 1.37
									     1.379))))
							     (noddsin-n g))))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0) (norm 1.0))


(define (noddsin gen fm)
  "  (make-noddsin frequency (n 1)) creates an noddsin generator.\n\
   (noddsin gen fm) returns n odd-numbered sines spaced by frequency."
  (declare (gen noddsin) (fm float))
  (let* ((x (noddsin-angle gen))
	 (n (noddsin-n gen))
	 (norm (noddsin-norm gen))
	 (snx (sin (* n x)))
	 (den (sin x)))

    (set! (noddsin-angle gen) (+ x fm (noddsin-frequency gen)))

    (if (< (abs den) nearly-zero)
	0.0
	(/ (* norm snx snx) den))))
	   
#|
;;; get normalization:
(do ((i 1 (1+ i))) 
    ((= i 30))
  (let ((v (with-sound (:output (make-vct 1000) :clipped #f)
		       (let ((gen (make-noddsin (radians->hz .002) :n i)))
			 (do ((k 0 (1+ k)))
			     ((= k 1000))
			   (outa k (noddsin gen 0.0)))))))
    (let ((pos 0)
	  (pk (vct-peak v)))
      (do ((k 0 (1+ k)))
	  ((or (= k 1000)
	       (> pos 0)))
	(if (>= (abs (vct-ref v k)) pk)
	    (set! pos k)))
      (snd-display "~A: ~A ~A, ~A ~A ~A" i pk (/ i pk) pos (/ (* pos 0.002) (* 2 pi)) (inexact->exact (round (/ 1.0 (/ (* pos 0.002) (* 2 pi)))))))))

;;; so max is about at 2pi/(5n+4)
;;; for sum-of-sines it's about half that 2pi/(2.5n+4) -- zero cross at 2pi/4n min at pi/n 
|#

#|
;;; clarinety
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddsin 300 :n 3))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 40000 :scaler .5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 40000))
	(outa i (* (env ampf) (noddsin gen 0.0))))))))
|#


(defgenerator (noddcos
	       :make-wrapper (lambda (g)
			       (set! (noddcos-frequency g) (hz->radians (noddcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (noddcos gen fm)
  "  (make-noddcos frequency (n 1)) creates an noddcos generator.\n\
   (noddcos gen fm) returns n odd-numbered cosines spaced by frequency."
  (declare (gen noddcos) (fm float))
  (let* ((angle (noddcos-angle gen))
	 (n (noddcos-n gen))
	 (den (* 2 n (sin angle)))) ; "n" here is normalization

    (set! (noddcos-angle gen) (+ angle fm (noddcos-frequency gen)))

    (if (< (abs den) nearly-zero)
	(let ((fang (fmod (abs angle) (* 2 pi))))
	  ;; hopefully this almost never happens...
	  (if (or (< fang 0.001)
		  (< (abs (- fang (* 2 pi))) 0.001))
	      1.0
	      -1.0))
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddcos 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddcos gen 0.0)))))))
|#


(defgenerator (noddssb
	       :make-wrapper (lambda (g)
			       (set! (noddssb-frequency g) (hz->radians (noddssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (noddssb gen fm)
  "  (make-noddssb frequency (ratio 1.0) (n 1)) creates an noddssb generator.\n\
   (noddssb gen fm) returns n sinusoids from frequency spaced by 2 * ratio * frequency."
  (declare (gen noddssb) (fm float))
  (let* ((cx (noddssb-angle gen))
	 (mx (* cx (noddssb-ratio gen)))
	 (x (- cx mx))
	 (n (noddssb-n gen))
	 (sinnx (sin (* n mx)))
	 (den (* n (sin mx)))) ; "n" is normalization

    (set! (noddssb-angle gen) (+ cx fm (noddssb-frequency gen)))

    (max -1.0  ; -1.0 is probably the peak, trying to catch bad cases is too much trouble here
	 (min 1.0
	      (- (* (sin x)
		    (/ (* sinnx sinnx) den))
		 (* (cos x)
		    (/ (sin (* 2 n mx))
		       (* 2 den))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 0.1 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddssb gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 0.1 5))
	(vib (make-oscil 5.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddssb gen (* (hz->radians 100.0) (oscil vib)))))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; various kernels: ncos2 = ncos squared (Fejer), ncos4 = ncos2 squared (Jackson), npcos = Poussin kernel

(defgenerator (ncos2
	       :make-wrapper (lambda (g)
			       (set! (ncos2-frequency g) (hz->radians (ncos2-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (ncos2 gen fm)
  "  (make-ncos2 frequency (n 1)) creates an ncos2 (Fejer kernel) generator.\n\
   (ncos2 gen fm) returns n sinusoids spaced by frequency scaled by (n-k)/(n+1)"
  (declare (gen ncos2) (fm float))

  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.

  (let* ((x (ncos2-angle gen))
	 (n (ncos2-n gen))
	 (den (sin (* 0.5 x))))

    (set! (ncos2-angle gen) (+ x fm (ncos2-frequency gen)))

    (if (< (abs den) nearly-zero)
	1.0
	(let ((val (/ (sin (* 0.5 (+ n 1) x)) 
		      (* (+ n 1) den))))
	  (* val val)))))

;;; can't use two oscils here because the angles have to line up perfectly

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ncos2 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos2 gen 0.0)))))))
|#


(define make-ncos4 make-ncos2)

(define (ncos4 gen fm)
  "  (make-ncos4 frequency (n 1)) creates an ncos4 (Jackson kernel) generator.\n\
   (ncos4 gen fm) returns n sinusoids spaced by frequency scaled by ((n-k)/(n+1))^2"
  ;; Katznelson p16
  (declare (gen ncos2) (fm float))
  (let ((val (ncos2 gen fm)))
    (* val val))) ; we already normalized this to 1.0


#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ncos4 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos4 gen 0.0)))))))
|#


(defgenerator (npcos
	       :make-wrapper (lambda (g)
			       (set! (npcos-frequency g) (hz->radians (npcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (npcos gen fm)
  "  (make-npcos frequency (n 1)) creates an npcos (Poussin kernel) generator.\n\
   (npcos gen fm) returns n*2+1 sinusoids spaced by frequency with amplitudes in a sort of tent shape."
  (declare (gen npcos) (fm float))
  (let* ((angle (npcos-angle gen))
	 (den (sin (* 0.5 angle)))
	 (n (npcos-n gen))
	 (result (if (< (abs den) nearly-zero)
		     1.0
		     (let* ((n1 (+ n 1))
			    (result1 
			     (let ((val (/ (sin (* 0.5 n1 angle)) 
					   (* n1 den))))
			       (* val val)))
			    (p2n2 (+ (* 2 n) 2))
			    (result2 
			     (let ((val (/ (sin (* 0.5 p2n2 angle)) 
					   (* p2n2 den))))
			       (* val val))))
		       (- (* 2 result2) result1)))))
    (set! (npcos-angle gen) (+ fm angle (npcos-frequency gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-npcos 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (npcos gen 0.0)))))))
|#


#|
;;; ncos5 and nsin5 are minor variants of nsin and ncos -- the last component is at half amplitude

(defgenerator (ncos5
	       :make-wrapper (lambda (g)
			       (set! (ncos5-frequency g) (hz->radians (ncos5-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (ncos5 gen fm)
  "  (make-ncos5 frequency (n 1)) creates an ncos5 generator.\n\
   (ncos5 gen fm) returns n cosines spaced by frequency. All are equal amplitude except the first and last at half amp."
  (declare (gen ncos5) (fm float))
  
  ;; from "Chebyshev Polynomials", Mason and Handscomb, p87

  (let* ((x (ncos5-angle gen))
	 (n (ncos5-n gen))
	 (den (tan (* 0.5 x))))

    (set! (ncos5-angle gen) (+ x fm (ncos5-frequency gen)))

    (if (< (abs den) nearly-zero)
	1.0
	(/ (- (/ (sin (* n x))
		 (* 2 den))
	      0.5)
	   (- n 0.5)))))


(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ncos5 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos5 gen 0.0)))))))


(defgenerator (nsin5
	       :make-wrapper (lambda (g)
			       (set! (nsin5-frequency g) (hz->radians (nsin5-frequency g)))
			       (set! (nsin5-n g) (max 2 (nsin5-n g)))
			       (if (< (nsin5-n g) 40)
				   (set! (nsin5-norm g) (list-ref (list 1.0 0.0 2.598 4.137 5.637 7.117 8.587 10.051 11.509 
									12.971 14.422 15.880 17.320 18.764 20.224 21.693 23.136 24.554 26.048 
									27.436 28.938 30.377 31.734 33.296 34.725 36.009 37.566 39.101 40.522 
									41.817 43.121 44.768 46.331 47.801 49.171 50.434 51.583 53.058 54.745 56.368)
								  (nsin5-n g)))
				   (set! (nsin5-norm g) (* 1.45 n)))
			       g))
  (frequency *clm-default-frequency*) (n 2 :type int) (angle 0.0) (norm 1.0))


(define (nsin5 gen fm)
  "  (make-nsin5 frequency (n 1)) creates an nsin5 generator.\n\
   (nsin5 gen fm) returns n sines spaced by frequency. All are equal amplitude except last at half amp."
  (declare (gen nsin5) (fm float))
  
  ;; from "Chebyshev Polynomials", Mason and Handscomb, p100

  (let* ((x (nsin5-angle gen))
	 (n (nsin5-n gen))
	 (norm (nsin5-norm gen))
	 (den (tan (* 0.5 x))))

    (set! (nsin5-angle gen) (+ x fm (nsin5-frequency gen)))

    (if (< (abs den) nearly-zero)
	0.0
	(/ (- 1.0 (cos (* n x)))
	   (* den norm)))))


(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nsin5 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nsin5 gen 0.0)))))))

(let ((norms (list 1.0 0.0)))
  (do ((i 2 (1+ i)))
      ((= i 40))
    (let* ((res (with-sound (:clipped #f)
 	          (let ((gen (make-nsin5 100.0 :n i)))
		    (run
		     (lambda ()
		       (do ((i 0 (1+ i)))
			   ((= i 20000))
			 (outa i (nsin5 gen 0.0))))))))
	   (snd (find-sound res)))
      (snd-display ";~D: ~A" i (maxamp snd 0))
      (set! norms (cons (maxamp snd 0) norms))))
  (reverse norms))


;;; from the same book p 110 is atan(x)/x, if x=cos we get:

(with-sound (:clipped #f :statistics #t)
  (let* ((x 0.0)
	 (freq (hz->radians 100.0)))
    (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (/ (- (/ (atan (cos x))
		       (cos x))
		    (* 0.5 1.76275))
		 -0.1187))
      (set! x (+ x freq)))))

(let ((sum 0.0))
  (do ((s 1 (+ s 2)))
      ((>= s 100))
    (set! sum (+ sum (* 4 (/ (expt (- (sqrt 2.0) 1.0) (+ (* 2 s) 1))
				(+ (* 2 s) 1))))))
  sum) ; ~ 0.096

;;; the evens cancel, each of the odds gets through once

|#



(define generator-max-r 0.999999)
(define generator-min-r -0.999999)
(define (generator-clamp-r r)
  (min generator-max-r (max generator-min-r r)))


;;; --------------------------------------------------------------------------------
;;;
;;; n sinusoids scaled by r: nrsin, nrcos, nrssb

(defgenerator (nrsin
	       :make-wrapper (lambda (g)
			       (set! (nrsin-r g) (generator-clamp-r (nrsin-r g)))
			       (set! (nrsin-gen g) (make-nrxysin (nrsin-frequency g) 1.0 (nrsin-n g) (nrsin-r g)))
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (nrsin-gen g)))
			       (lambda (g val) (set! (mus-frequency (nrsin-gen g)) val)))
			 (list 'mus-scaler
			       (lambda (g) (mus-scaler (nrsin-gen g)))
			       (lambda (g val) (set! (mus-scaler (nrsin-gen g)) val)))))
  (frequency *clm-default-frequency*) (n 1 :type int) (r 0.5)
  (gen #f :type clm))


(define (nrsin gen fm)
  "  (make-nrsin frequency (n 1) (r 0.0)) creates an nrsin generator (similar to nrxysin or sine-summation).\n\
   (nrsin gen fm) returns n sines spaced by frequency with amplitudes scaled by r^k."
  (declare (gen nrsin) (fm float))
  (nrxysin (nrsin-gen gen) fm))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-nrsin 830.0 :n 5 :r 0.5))
	(gen2 (make-sine-summation (* 8 830.0) :n 5 :a 0.5 :ratio (/ 1.0 8.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .125 (+ (nrsin gen1 0.0)
			    (* .3 (sine-summation gen2 0.0))))))))))
|#

(defgenerator (nrcos
	       :make-wrapper (lambda (g)
			       (set! (nrcos-frequency g) (hz->radians (nrcos-frequency g)))
			       (set! (nrcos-n g) (+ 1 (nrcos-n g)))
			       (set! (nrcos-r g) (generator-clamp-r (nrcos-r g)))
			       g)
	       :methods (list
			 (list 'mus-order
			       (lambda (g) (1- (nrcos-n g)))
			       (lambda (g val) (set! (nrcos-n g) (1+ val)) val))
			 (list 'mus-scaler
			       (lambda (g) (nrcos-r g))
			       (lambda (g val)
				 (set! (nrcos-r g) (generator-clamp-r val))
				 (nrcos-r g)))))
  (frequency *clm-default-frequency*) (n 1 :type int) (r 0.5) (angle 0.0))


(define (nrcos gen fm)
  "  (make-nrcos frequency (n 1) (r 0.5)) creates an nrcos generator.\n\
   (nrcos gen fm) returns n cosines spaced by frequency with amplitudes scaled by r^k."
  (declare (gen nrcos) (fm float))
  (let* ((x (nrcos-angle gen))
	 (r (nrcos-r gen)))

    (set! (nrcos-angle gen) (+ fm x (nrcos-frequency gen)))

    (if (< (abs r) nearly-zero)
	0.0
	(let* ((n (nrcos-n gen))
	       (norm (- (/ (- (expt (abs r) n) 1) (- (abs r) 1)) 1.0))) ; n+1??

	  (/ (+ (- (* r (cos x)) 
		   (* (expt r n) (cos (* n x))) (* r r)) 
		(* (expt r (+ n 1)) (cos (* (- n 1) x))))
	     (* norm (+ 1.0 (* -2.0 r (cos x)) (* r r))))))))

;;; formula changed to start at k=1 and n increased so we get 1 to n

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 400.0 :n 5 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrcos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .1)
  (let ((gen (make-nrcos 1200.0 :n 3 :r 0.99))
	(mod (make-oscil 400.0)) ; multi-carrier fm
	(index 0.01))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nrcos gen (* index (oscil mod)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0)) ; multi-carrier fm
	(index 0.02))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nrcos gen (* index (oscil mod)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0))
	(index (make-env '(0 0 1 .1) :length 30000))) ; or '(0 .4 1 0)
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nrcos gen (* (env index) (oscil mod)))))))))
|#

(definstrument (lutish beg dur freq amp)
  (let* ((res1 (max 1 (inexact->exact (round (/ 1000.0 (max 1.0 (min 1000.0 freq)))))))
	 (gen (make-nrcos (* freq res1) :n (max 1 (- res1 2))))
	 (mod (make-oscil freq))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (maxind (max .01 (min .3 (/ (- (log freq) 3.5) 8.0))))
	 (index (make-env (list 0 maxind 1 (* maxind .25) (max dur 2.0) 0.0) :duration dur))
	 (amplitude (make-env (list 0 0  .01 1  .2 1  .5 .5  1 .25  (max dur 2.0) 0.0) :duration dur :scaler amp)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((ind (env index)))
	   (set! (nrcos-r gen) ind)
	   (outa i (* (env amplitude)
		      (nrcos gen (* ind (oscil mod)))))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (lutish 0 1 440 .1))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (lutish (* i .1) 2 (* 100 (1+ i)) .05)))
|#


;;; G&R 2nd col 1st and 2nd rows

(defgenerator (nrssb
	       :make-wrapper (lambda (g)
			       (set! (nrssb-frequency g) (hz->radians (nrssb-frequency g)))
			       (set! (nrssb-r g) (generator-clamp-r (nrssb-r g)))
			       (if (< (nrssb-r g) 0.0)
				   (set! (nrssb-r g) 0.0))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (r 0.0) (angle 0.0))


(define (nrssb gen fm)
  "  (make-nrssb frequency (ratio 1.0) (n 1) (r 0.5)) creates an nrssb generator.\n\
   (nrssb gen fm) returns n sinusoids from frequency spaced by frequency * ratio with amplitudes scaled by r^k."
  (declare (gen nrssb) (fm float))
  (let* ((cx (nrssb-angle gen))
	 (mx (* cx (nrssb-ratio gen)))
	 (r (nrssb-r gen))
	 (n (nrssb-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- rn 1) (- r 1)))
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))

    (set! (nrssb-angle gen) (+ cx fm (nrssb-frequency gen)))

    (/ (- (* (sin cx)
	     (+ (* r (sin mx))
		(* rn (sin nmx))
		(* rn1 (sin n1mx))))
	  (* (cos cx)
	     (+ 1.0
		(* -1.0 r (cos mx))
		(* rn (cos nmx))
		(* rn1 (cos n1mx)))))
       den)))

(define (nrssb-interp gen fm interp)
  "  (make-nrssb frequency (ratio 1.0) (n 1) (r 0.0)) creates an nrssb generator for use with nrssb-interp.\n\
   (nrssb-interp gen fm interp) returns n sinusoids from frequency spaced by frequency * ratio with amplitudes scaled by r^k.\
  The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  (declare (gen nrssb) (fm float))
  (let* ((cx (nrssb-angle gen))
	 (mx (* cx (nrssb-ratio gen)))
	 (r (nrssb-r gen))
	 (n (nrssb-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- rn 1) (- r 1)))
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))

    (set! (nrssb-angle gen) (+ cx fm (nrssb-frequency gen)))

    (/ (- (* interp 
	     (sin cx)
	     (+ (* r (sin mx))
		(* rn (sin nmx))
		(* rn1 (sin n1mx))))
	  (* (cos cx)
	     (+ 1.0
		(* -1.0 r (cos mx))
		(* rn (cos nmx))
		(* rn1 (cos n1mx)))))
       den)))

	  
#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrssb 1000 0.1 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrssb gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrssb 1000 0.1 5 0.5))
	(vib (make-oscil 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrssb gen (* (hz->radians 100) (oscil vib)))))))))
|#

(definstrument (oboish beg dur freq amp aenv)
  (let* ((res1 (max 1 (inexact->exact (round (/ 1400.0 (max 1.0 (min 1400.0 freq)))))))
	 (gen (make-nrssb (* freq res1) (/ 1 res1) :n res1 :r 0.75))
	 (mod (make-oscil 5.0))
	 (res2 (max 1 (inexact->exact (round (/ 2400.0 (max 1.0 (min 2400.0 freq)))))))
	 (gen2 (make-oscil (* freq res2)))
	 (gen3 (make-oscil freq))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (amplitude (make-env aenv :duration dur :base 4 :scaler amp))
	 (skenv (make-env (list 0.0 0.0 1 1 2.0 (mus-random 1.0) 3.0 0.0 (max 4.0 (* dur 20.0)) 0.0) 
			  :duration dur :scaler (hz->radians (random (* freq .05)))))
	 (relamp (+ .85 (random .1)))
	 (avib (make-rand-interp 5 .2)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((vol (* (+ .8 (rand-interp avib)) 
			(env amplitude)))
		(vib (+ (* (hz->radians (* freq 0.003)) 
			   (oscil mod))
			(env skenv)))
		(vola (* 0.05 (/ vol amp)))
		(result (* vol
			   (+ (* (- relamp vola) 
				 (nrssb-interp gen (* res1 vib) -1.0))
			      (* (+ (- 1.0 relamp) vola) 
				 (oscil gen2 (+ (* vib res2)
						(* (hz->radians freq)
						   (oscil gen3 vib)))))))))
	   (outa i result)
	   (if *reverb* (outa i (* .01 result) *reverb*))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (oboish 0 1 300 .1 '(0 0 1 1 2 0)))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (oboish (* i .3) .4 (+ 100 (* 50 i)) .05 '(0 0 1 1 2 1 3 0))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((rats (vector 1 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2))
	(mode (vector 0 0 2 4 11 11 5 6 7 9 2 12 0)))
    (do ((i 0 (1+ i)))
	((= i 20))
      (oboish (/ (random 32) 8) 
		(/ (+ 3 (random 8)) 8)
		(* 16.351 16 (vector-ref rats (vector-ref mode (random 12))))
		(+ .25 (random .25))
		(let* ((pt1 (random 1.0))
		       (pt2 (+ pt1 (random 1.0)))
		       (pt3 (+ pt2 (random 1.0))))
		  (list 0 0 pt1 1 pt2 .5 pt3 0))))))

;;; .85 .15 (* 2 freq) 300, 2400 + 0.5*vib
|#


;;; --------------------------------------------------------------------------------
;;;
;;; n sinusoids scaled by k: nkssb


;;; G&R 1st col ksinkx cases

(defgenerator (nkssb
	       :make-wrapper (lambda (g)
			       (set! (nkssb-frequency g) (hz->radians (nkssb-frequency g)))
			       (set! (nkssb-n g) (+ 1 (nkssb-n g))) ; sum goes 1 to n-1
			       g)
	       :methods (list
			 (list 'mus-order
			       (lambda (g) (1- (nkssb-n g)))
			       (lambda (g val) (set! (nkssb-n g) (1+ val)) val))))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1 :type int) (angle 0.0))


(define (nkssb gen fm)
  "  (make-nkssb frequency (ratio 1.0) (n 1)) creates an nkssb generator.\n\
   (nkssb gen fm) returns n sinusoids from frequency spaced by frequency * ratio with amplitude k."
  (declare (gen nkssb) (fm float))
  (let* ((n (nkssb-n gen))
	 (cx (nkssb-angle gen))
	 (x (* cx (nkssb-ratio gen)))
	 (cxx (- cx x))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x)))

    (set! (nkssb-angle gen) (+ cx fm (nkssb-frequency gen)))

    (if (< (abs sx2) 1.0e-8)
	-1.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (sin cxx) s1)
		(* (cos cxx) c1))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1
	       

(define (nkssb-interp gen fm interp)
  "  (make-nkssb frequency (ratio 1.0) (n 1)) creates an nkssb generator for nkssb-interp.\n\
   (nkssb-interp gen fm interp) returns n sinusoids from frequency spaced by frequency * ratio with amplitude k.\
  The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  (declare (gen nkssb) (fm float) (interp float))
  (let* ((n (nkssb-n gen))
	 (cx (nkssb-angle gen))
	 (x (* cx (nkssb-ratio gen)))
	 (cxx (- cx x))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x)))

    (set! (nkssb-angle gen) (+ cx fm (nkssb-frequency gen)))

    (if (< (abs sx2) 1.0e-8)
	-1.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (cos cxx) c1)
		(* interp (* (sin cxx) s1)))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1, peak seems to be solid right through the interpolation

	       
#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nkssb gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 50.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nkssb gen (* vibamp (oscil vib)))))))))
|#

(definstrument (nkssber beg dur freq mfreq n vibfreq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen (make-nkssb freq (/ mfreq freq) n))
	 (move (make-env '(0 1 1 -1) :duration dur))
	 (vib (make-oscil vibfreq))
	 (vibamp (hz->radians (* (/ freq mfreq) 5.0)))
	 (ampf (make-env '(0 0 1 1 5 1 6 0) :scaler amp :duration dur)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (nkssb-interp gen 
				  (* vibamp (oscil vib))
				  (env move))) ; interp env
	      ))))))
#|
(with-sound (:play #t)
  (nkssber 0 1 1000 100 5 5 0.5)
  (nkssber 1 2 600 100 4 1 0.5)
  (nkssber 3 2 1000 540 3 3 0.5)
  (nkssber 5 4 300 120 2 0.25 0.5)
  (nkssber 9 1 30 4 40 0.5 0.5)
  (nkssber 10 1 20 6 80 0.5 0.5))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5))
	(move (make-env '(0 1 1 -1) :length 30000))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 50.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (* 0.5 (nkssb-interp gen 
				      (* vibamp (oscil vib))
				      (env move))) ; interp env
	      ))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 600.0 1/6 4))
	(vib (make-oscil 1.0))
	(vibamp (hz->radians 30.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000)) 
	 (let ((intrp (oscil vib)))
	   (outa i (* 0.5 (nkssb-interp gen 
					(* vibamp intrp)
					intrp)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 (/ 540 1000) 3))
	(vib (make-oscil 3.0)) ; 0.3  or 125 + 0.25 and 2 -> circling sound
	(vibamp (hz->radians (* (/ 1000 540) 5.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000)) (let ((intrp (oscil vib)))
	 (outa i (* 0.5 (nkssb-interp gen 
				      (* vibamp intrp)
				      intrp)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 300.0 (/ 120 300) 2))
	(vib (make-oscil 0.25))
	(vibamp (hz->radians (* (/ 300 120) 5.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 300000)) (let ((intrp (oscil vib)))
	 (outa i (* 0.5 (nkssb-interp gen 
				      (* vibamp intrp)
				      intrp)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 30.0 (/ 4 30) 40))
	(vib (make-oscil 0.5))
	(vibamp (hz->radians (* (/ 30 4) 5.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 300000)) (let ((intrp (oscil vib)))
	 (outa i (* 0.5 (nkssb-interp gen 
				      (* vibamp intrp)
				      intrp)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 20.0 (/ 6 20) 80)) ; 120 8 80 (100), 6 400
	
	(vib (make-oscil 0.5))
	(vibamp (hz->radians (* (/ 20 6) 5.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 300000)) (let ((intrp (oscil vib)))
	 (outa i (* 0.5 (nkssb-interp gen 
				      (* vibamp intrp)
				      intrp)))))))))
|#


;;; --------------------------------------------------------------------------------

;;; n cos scaled by sin(k*pi/(n+1))/sin(pi/(n+1))
;;; "Biased Trigonometric Polynomials", Montgomery and Vorhauer
;;; American Math Monthly vol 114 no 9 Nov 2007

(defgenerator (nsincos
	       :make-wrapper (lambda (g)
			       (let ((n (nsincos-n g)))
				 (set! (nsincos-frequency g) (hz->radians (nsincos-frequency g)))
				 (set! (nsincos-n2 g) (/ (+ n 1) 2))
				 (set! (nsincos-cosn g) (cos (/ pi (+ n 1))))
				 (do ((k 1 (1+ k)))
				     ((> k n))
				   (set! (nsincos-norm g) (+ (nsincos-norm g) 
							     (/ (sin (/ (* k pi) (+ n 1))) 
								(sin (/ pi (+ n 1)))))))
				 g)))
  (frequency *clm-default-frequency*) (n 1 :type int) 
  (angle 0.0) (n2 1.0) (cosn 1.0) (norm 0.0))


(define (nsincos gen fm)
  "  (make-nsincos frequency (n 1)) creates an nsincos generator.\n\
   (nsincos gen fm) returns n cosines spaced by frequency with amplitude sin(k*pi/(n+1))/sin(pi/(n+1))"
  (declare (gen nsincos) (fm float))
  (let* ((x (nsincos-angle gen))
	 (n2 (nsincos-n2 gen))
	 (cosn (nsincos-cosn gen))
	 (num (cos (* n2 x))))

    (set! (nsincos-angle gen) (+ x fm (nsincos-frequency gen)))

    (/ (* num num)
       (* (nsincos-norm gen)
	  (- (cos x) cosn)))))

#|
(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-nsincos 100.0 3)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (nsincos gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; Ramanujan, "On certain Arithmetical Functions"

(defgenerator (n1cos 
	       :make-wrapper (lambda (g)
			       (set! (n1cos-frequency g) (hz->radians (n1cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))

(define (n1cos gen fm)
  (declare (gen n1cos) (fm float))
  (let* ((n (n1cos-n gen))
	 (x (n1cos-angle gen))
	 (tn (tan (* 0.5 x))))

    (set! (n1cos-angle gen) (+ x fm (n1cos-frequency gen)))

    (if (< (abs tn) 1.0e-6)
	1.0
	(/ (- 1.0 (cos (* n x)))
	   (* tn tn
	      n n 2))))) ; normalization -- this still has the very large DC term

#|
(with-sound (:clipped #f)
  (let ((gen (make-n1cos 100.0 10)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (n1cos gen 0.0)))))
|#




#|
;;; --------------------------------------------------------------------------------

;;; not sure the next two are interesting -- 2 more kernels

;;; Dimitrov and Merlo

(defgenerator (npos1cos
	       :make-wrapper (lambda (g)
			       (set! (npos1cos-frequency g) (hz->radians (npos1cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (npos1cos gen fm)
  "  (make-npos1cos frequency (n 1)) creates an npos1cos generator.\n\
   (npos1cos gen fm) returns n cosines spaced by frequency."
  (declare (gen npos1cos) (fm float))
  (let* ((x (npos1cos-angle gen))
	 (n (npos1cos-n gen))
	 (num (- (* (+ n 2) (sin (/ (* n x) 2)))
		 (* n (sin (/ (* (+ n 2) x) 2)))))
	 (sx (sin (/ x 2)))
	 (den (* 4 n (+ n 1) (+ n 2) sx sx sx sx)))

    (set! (npos1cos-angle gen) (+ x fm (npos1cos-frequency gen)))

    (if (< (abs den) nearly-zero)
	0.0

	(/ (* 3 num num)
	   den))))

;;; needs normalization and no DC.   side amps seem close


(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-npos1cos 100.0 3)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (npos1cos gen 0.0)))))))


(defgenerator (npos3cos
	       :make-wrapper (lambda (g)
			       (set! (npos3cos-frequency g) (hz->radians (npos3cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0))


(define (npos3cos gen fm)
  "  (make-npos3cos frequency (n 1)) creates an npos3cos generator.\n\
   (npos3cos gen fm) returns n cosines spaced by frequency."
  (declare (gen npos3cos) (fm float))
  (let* ((x (npos3cos-angle gen))
	 (n (npos3cos-n gen))
	 (sx (sin (/ x 2)))
	 (den (* (+ (* 4 n) 2) sx sx)))

    (set! (npos3cos-angle gen) (+ x fm (npos3cos-frequency gen)))

    (if (< (abs den) nearly-zero)
	(exact->inexact n)

	(/ (- 2 (cos (* n x)) (cos (* (+ n 1) x)))
	   den))))

;;; needs normalization and no DC, peak at den=0 not right.   side amps seem close


(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-npos3cos 100.0 3)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (npos3cos gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; inf sinusoids scaled by r: rcos, rssb

(defgenerator (rcos
	       :make-wrapper (lambda (g)
			       (set! (rcos-osc g) (make-oscil (rcos-frequency g) (* 0.5 pi)))
			       (set! (rcos-r g) (generator-clamp-r (rcos-r g)))
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (rcos-osc g)))
			       (lambda (g val) (set! (mus-frequency (rcos-osc g)) val) val))

			 (list 'mus-scaler
			       (lambda (g) (rcos-r g))
			       (lambda (g val)
				 (set! (rcos-r g) (generator-clamp-r val))
				 (rcos-r g)))
				       
			 (list 'mus-phase
			       (lambda (g) (mus-phase (rcos-osc g)))
			       (lambda (g val) (set! (mus-phase (rcos-osc g)) val) val))))
  (frequency *clm-default-frequency*) (r 0.0)
  (osc #f :type clm))

#|
;;; G&R form:
(define (rcos gen fm)
  (declare (gen rcos) (fm float))
  (let* ((r (rcos-r gen))
	 (absr (abs r))
	 (rcosx (* r (oscil (rcos-osc gen) fm))))
    (* (- (/ (- 1.0 rcosx)
	     (+ 1.0 
		(* r r)
		(* -2.0 rcosx)))
	  1.0)
       (/ (- 1.0 absr) absr)))) ; normalization
|#

(define (rcos gen fm)
  "  (make-rcos frequency (r 0.0)) creates an rcos generator.\n\
   (rcos gen fm) returns many cosines spaced by frequency with amplitude r^k."
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16, p243. r^k cos sum
  ;; a variant of the G&R 2nd col 4th row
  (declare (gen rcos) (fm float))
  (let* ((r (rcos-r gen))
	 (absr (abs r))
	 (rr (* r r)))

    (if (< absr nearly-zero)
	0.0                       ; 1.0 from the formula, but we're subtracting out DC
	(* (- (/ (- 1.0 rr)
		 (- (+ 1.0 rr)
		    (* 2.0 r (oscil (rcos-osc gen) fm))))
	      1.0)
	   (/ (- 1.0 absr) (* 2.0 absr)))))) ; normalization

;;; if r>0 we get the spike at multiples of 2pi, since the k*pi case is flipping -1 1 -1 etc
;;; if r<0, we get the spike at multiples of (2k-1)pi since the r sign now counteracts the cos k*pi sign
;;;  so the peak amp is the same in the two cases, so the normalization has to use abs(r)!
;;;  but in the k*pi case we tend to miss k*pi (whereas we never miss 0 since we start there),
;;;  so the actual maxamp may be less than 1.0



#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rcos 100.0 :r 0.5)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (rcos gen 0.0)))))))
|#

(definstrument (stringy beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (n (inexact->exact (floor (/ (mus-srate) (* 3 freq)))))
	 (r (expt .001 (/ 1 n)))
	 (carrier (make-rcos freq (* .5 r)))
	 (clang (make-rkoddssb (* freq 2) (/ 1.618 2) r))
	 (ampf (make-env '(0 0 1 1 2 .5 4 .25 10 0) :scaler amp :duration dur))
	 (clangf (make-env (list 0 0 .1 1 .2 .1 .3 0) :scaler (* amp .5) :duration .1))
	 (rf (make-env (list 0 1 1 0) :scaler (* 0.5 r) :duration dur))
	 (crf (make-env (list 0 1 1 0) :scaler r :duration .1)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (mus-scaler clang) (env crf))
	 (set! (rcos-r carrier) (env rf))
	 (outa i (+ (* (env clangf)
		       (rkoddssb clang 0.0))
		    (* (env ampf)
		       (rcos carrier 0.0)))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (stringy 0 1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (stringy (* i .3) .3 (+ 200 (* 100 i)) .5)))
|#


(defgenerator (rssb 
	       :make-wrapper (lambda (g)
			       (set! (rssb-frequency g) (hz->radians (rssb-frequency g)))
			       (set! (rssb-r g) (generator-clamp-r (rssb-r g)))
			       g)

	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rssb-r g))
			       (lambda (g val)
				 (set! (rssb-r g) (generator-clamp-r val))
				 (rssb-r g)))))

  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rssb gen fm)
  "  (make-rssb frequency (ratio 1.0) (r 0.0)) creates an rssb generator.\n\
   (rssb gen fm) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k."
  (declare (gen rssb) (fm float))
  (let* ((angle1 (rssb-angle gen))
	 (angle2 (* angle1 (rssb-ratio gen)))
	 (carsin (sin angle1))
	 (canrcos (cos angle1))
	 (r (rssb-r gen))
	 (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	 (sumsin (* r (sin angle2)))
	 (sumcos (- 1.0 (* r (cos angle2)))))

    (set! (rssb-angle gen) (+ angle1 fm (rssb-frequency gen)))

    (/ (- (* carsin sumsin)
	  (* canrcos sumcos))
       (* 2 den))))


(define (rssb-interp gen fm interp)
  "  (make-rssb frequency (ratio 1.0) (r 0.0)) creates an rssb generator for rssb-interp.\n\
   (rssb-interp gen fm interp) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k.\
  The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  (declare (gen rssb) (fm float))
  (let* ((angle1 (rssb-angle gen))
	 (angle2 (* angle1 (rssb-ratio gen)))
	 (carsin (sin angle1))
	 (canrcos (cos angle1))
	 (r (rssb-r gen))
	 (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	 (sumsin (* r (sin angle2)))
	 (sumcos (- 1.0 (* r (cos angle2)))))

    (set! (rssb-angle gen) (+ angle1 fm (rssb-frequency gen)))

    (/ (- (* carsin sumsin)
	  (* interp canrcos sumcos))
       (* 2 den))))



(definstrument (bump beg dur freq amp f0 f1 f2)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (res0 (inexact->exact (round (/ f0 freq))))
	 (res1 (inexact->exact (round (/ f1 freq))))
	 (res2 (inexact->exact (round (/ f2 freq))))
	 (gen1 (make-rssb (* res0 freq) (/ 1 res0) .4))
	 (gen2 (make-rssb (* res1 freq) (/ 1 res1) .5))
	 (gen3 (make-rssb (* res2 freq) (/ 1 res2) .6))
	 (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 1 5 .4 6 .1 80 0) :scaler amp :base 32 :duration dur)) ; or 50 at end
	 ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	 (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	 (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((vib (+ (rand-interp ranvib)
			 (triangle-wave pervib))))
	     (outa i (* (env ampf)
			(+ (* .85 (rssb-interp gen1 (* res0 vib) -1))
			   (* .1 (rssb-interp gen2 (* res1 vib) 0))
			   (* .05 (rssb-interp gen3 (* res2 vib) 1)))))))))))

#|
(with-sound (:play #t)
  (do ((k 0 (1+ k))) 
      ((= k 10))
    (bump (* 0.4 k) 1 (* 16.3 (expt 2.0 (+ 3 (/ k 12)))) .5 520 1190 2390))
  (do ((k 0 (1+ k))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12)))))
	   (scl (sqrt (/ freq 120))))
      (bump (+ 4 (* 0.4 k)) 1 freq  .5 (* scl 520) (* scl 1190) (* scl 2390)))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (1+ k))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12))))) ; if oct=5 (and env end at 100), sort of hammered string effect
	   (f0 520) ; "uh"
	   (f1 1190)
	   (f2 2390)
	   ;; "ah" is good: 730 1090 2440
	   ;; it might be smoother to scale the formant freqs by (sqrt (/ freq 120)) or even (expt (/ freq 120) 0.3)
	   (res0 (inexact->exact (round (/ f0 freq))))
	   (res1 (inexact->exact (round (/ f1 freq))))
	   (res2 (inexact->exact (round (/ f2 freq))))
	   (gen1 (make-rssb (* res0 freq) (/ 1 res0) .4))
	   (gen2 (make-rssb (* res1 freq) (/ 1 res1) .5))
	   (gen3 (make-rssb (* res2 freq) (/ 1 res2) .6))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 1 5 .4 6 .1 80 0) :scaler .5 :base 32 :length 60000)) ; or 50 at end
	   ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 60000))
	   (let ((vib (+ (rand-interp ranvib)
			 (triangle-wave pervib))))
	     (outa (+ i (* k 30000)) (* (env ampf)
					(+ (* .85 (rssb-interp gen1 (* res0 vib) -1))
					   (* .1 (rssb-interp gen2 (* res1 vib) 0))
					   (* .05 (rssb-interp gen3 (* res2 vib) 1))))))))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (1+ k))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12))))) ; froggy if oct=1 or 2 and "ah" (env end at 10 = cycling) ("er" is good too at oct=2)
	   (scl (sqrt (/ freq 120)))
	   (f0 (* scl 520)) ; "uh"
	   (f1 (* scl 1190))
	   (f2 (* scl 2390))
	   ;; "ah" is good: 730 1090 2440
	   (res0 (inexact->exact (floor (/ f0 freq))))
	   (res1 (inexact->exact (floor (/ f1 freq))))
	   (res2 (inexact->exact (floor (/ f2 freq))))
	   (gen1 (make-rk!ssb (* res0 freq) (/ 1 res0) 2.4))
	   (gen2 (make-rssb (* res1 freq) (/ 1 res1) .5))
	   (gen3 (make-rssb (* res2 freq) (/ 1 res2) .6))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 .3 5 .4 6 .1 40 0) :scaler .5 :base 32 :length 60000)) ; or 50 at end
	   ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 60000))
	   (let ((vib (+ (rand-interp ranvib)
			 (triangle-wave pervib))))
	     (outa (+ i (* k 30000)) (* (env ampf)
					(+ (* .85 (rk!ssb gen1 (* res0 vib)))
					   (* .1 (rssb-interp gen2 (* res1 vib) 0))
					   (* .05 (rssb-interp gen3 (* res2 vib) 1))))))))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (1+ k))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12)))))
	   (scl (sqrt (/ freq 120)))
	   (f0 (* scl 490)) ; "uh"
	   (f1 (* scl 1350))
	   (f2 (* scl 2440))
	   ;; "ah" is good: 730 1090 2440
	   (res0 (inexact->exact (floor (/ f0 freq))))
	   (res1 (inexact->exact (floor (/ f1 freq))))
	   (res2 (inexact->exact (floor (/ f2 freq))))
	   (gen1 (make-rk!ssb (* res0 freq) (/ 1 res0) 2))
	   (gen2 (make-rk!ssb (* res1 freq) (/ 1 res1) 3))
	   (gen3 (make-rk!ssb (* res2 freq) (/ 1 res2) 3))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 .3 5 .4 6 .1 40 0) :scaler .5 :base 32 :length 30000))
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 30000))
	   (let ((vib (+ (rand-interp ranvib)
			 (triangle-wave pervib))))
	     (outa (+ i (* k 30000)) (* (env ampf)
					(+ (* .85 (rk!ssb gen1 (* res0 vib)))
					   (* .1 (rk!ssb gen2 (* res1 vib)))
					   (* .05 (rk!ssb gen3 (* res2 vib)))))))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rssb 2000.0 (/ 103.0 2000) 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rssb gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; rxysin
;;;
;;; similar to rssb: (JO 1st)

(defgenerator (rxysin
	       :make-wrapper (lambda (g)
			       (set! (rxysin-frequency g) (hz->radians (rxysin-frequency g)))
			       (set! (rxysin-r g) (generator-clamp-r (rxysin-r g)))
			       g)
	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rxysin-r g))
			       (lambda (g val)
				 (set! (rxysin-r g) (generator-clamp-r val))
				 (rxysin-r g)))))

  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rxysin gen fm)
  "  (make-rxysin frequency (ratio 1.0) (r 0.0)) creates an rxysin generator (similar to rssb).\n\
   (rxysin gen fm) returns many sines from frequency spaced by frequency * ratio with amplitude r^k."
  (declare (gen rxysin) (fm float))
  (let* ((x (rxysin-angle gen))
	 (y (* x (rxysin-ratio gen)))
	 (r (rxysin-r gen)))

    (set! (rxysin-angle gen) (+ x fm (rxysin-frequency gen)))

    (/ (- (sin x)
	  (* r (sin (- x y))))
       (+ 1.0 
	  (* -2.0 r (cos y))
	  (* r r)))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxysin 1000 0.1 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rxysin gen 0.0)))))))
|#


(defgenerator (rxycos
	       :make-wrapper (lambda (g)
			       (set! (rxycos-frequency g) (hz->radians (rxycos-frequency g)))
			       (set! (rxycos-r g) (generator-clamp-r (rxycos-r g)))
			       g)
	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rxycos-r g))
			       (lambda (g val)
				 (set! (rxycos-r g) (generator-clamp-r val))
				 (rxycos-r g)))))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rxycos gen fm)
  "  (make-rxycos frequency (ratio 1.0) (r 0.0)) creates an rxycos generator.\n\
   (rxycos gen fm) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k."
  (declare (gen rxycos) (fm float))
  (let* ((x (rxycos-angle gen))
	 (y (* x (rxycos-ratio gen)))
	 (r (rxycos-r gen)))

    (set! (rxycos-angle gen) (+ x fm (rxycos-frequency gen)))

    (* (/ (- (cos x)
	     (* r (cos (- x y))))
	  (+ 1.0 
	     (* -2.0 r (cos y))
	     (* r r)))
       (- 1.0 (abs r))))) ; norm, abs for negative r

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rxycos 1000 0.1 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rxycos gen 0.0)))))))
|#


(define (clamp-rxycos-r gen fm)
  ;; in this case we need to track ratio, as well as r, since the
  ;;   highest frequency goes as x+ky (y=ratio*x); we want the value of k when
  ;;   we reach srate/3, then solve for the corresponding r.

  (declare (gen safe-rxycos) (fm float))
  (let* ((x (radians->hz (+ (safe-rxycos-frequency gen) fm)))
	 (y (* x (safe-rxycos-ratio gen)))
	 (r (safe-rxycos-r gen))
	 (topk (inexact->exact (floor (/ (- (/ (mus-srate) 3) x) y))))
	 (maxr (expt (safe-rxycos-cutoff gen) 
		     (/ 1.0 topk))))
    (if (>= r 0.0)
	(min r maxr)
	(max r (- maxr)))))

(defgenerator (safe-rxycos
	       :make-wrapper (lambda (g)
			       (set! (safe-rxycos-frequency g) (hz->radians (safe-rxycos-frequency g)))
			       (set! (safe-rxycos-r g) (clamp-rxycos-r g 0.0))
			       g)

	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) 
				 (safe-rxycos-r g))
			       (lambda (g val)
				 (set! (safe-rxycor-r g) val)
				 (set! (safe-rxycos-r g) (clamp-rxycos-r g 0.0))
				 (safe-rxycos-r g)))

			 (list 'mus-frequency
			       (lambda (g) 
				 (radians->hz (rxycos-frequency g)))
			       (lambda (g val)
				 (set! (safe-rxycos-frequency g) (hz->radians val))
				 (set! (safe-rxycos-r g) (clamp-rxycos-r g 0.0))
				 val))

			 (list 'mus-offset ; ratio accessor in defgenerator
			       (lambda (g)
				 (safe-rxycos-ratio g))
			       (lambda (g val)
				 (set! (safe-rxycos-ratio g) val)
				 (set! (safe-rxycos-r g) (clamp-rxycos-r g 0.0))
				 val))))
			       
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0) (cutoff 0.001))


(define (safe-rxycos gen fm)
  "  (make-safe-rxycos frequency (ratio 1.0) (r 0.0)) creates a safe-rxycos generator.\n\
   (safe-rxycos gen fm) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k where 'r' is restricted to a safe value."
  (declare (gen safe-rxycos) (fm float))
  (let* ((x (safe-rxycos-angle gen))
	 (y (* x (safe-rxycos-ratio gen)))
	 (r (safe-rxycos-r gen)))

    (set! (safe-rxycos-angle gen) (+ x fm (safe-rxycos-frequency gen)))
    (if (not (= fm 0.0))
	(set! r (clamp-rxycos-r gen fm)))

    (* (/ (- (cos x)
	     (* r (cos (- x y))))
	  (+ 1.0 
	     (* -2.0 r (cos y))
	     (* r r)))
       (- 1.0 (abs r))))) ; norm, abs for negative r

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-safe-rxycos 1000 0.1 0.99)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (safe-rxycos gen 0.0)))))))
 |#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by e^-r (special case of rcos): ercos, erssb

;;; sndclm.html G&R 2nd col last row (with normalization)

(defgenerator (ercos
	       :make-wrapper (lambda (g)
			       (set! (ercos-osc g) (make-oscil (ercos-frequency g)))
			       (if (<= (ercos-r g) 0.0) (set! (ercos-r g) 0.00001))
			       (set! (ercos-cosh-t g) (cosh (ercos-r g)))
			       (let ((exp-t (exp (- (ercos-r g)))))
				 (set! (ercos-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
				 (set! (ercos-scaler g) (* (sinh (ercos-r g)) (ercos-offset g))))
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (ercos-osc g)))
			       (lambda (g val) (set! (mus-frequency (ercos-osc g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (ercos-osc g)))
			       (lambda (g val) (set! (mus-phase (ercos-osc g)) val) val))))

  (frequency *clm-default-frequency*) (r 1.0)
  (osc #f :type clm) scaler offset cosh-t)


(define (ercos gen fm)
  "  (make-ercos frequency (r 0.0)) creates an ercos generator (a special case of rcos).\n\
   (ercos gen fm) returns many cosines from frequency with amplitude e^(-kr)."
  (declare (gen ercos) (fm float))
  (- (/ (ercos-scaler gen) 
	(- (ercos-cosh-t gen) (oscil (ercos-osc gen) fm)))
     (ercos-offset gen)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ercos 100 :r 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ercos gen 0.0)))))))
|#

(definstrument (ercoser beg dur freq amp r)
   (let* ((start (seconds->samples beg))
	  (stop (+ start (seconds->samples dur)))
	  (gen (make-ercos freq :r r))
	  (t-env (make-env '(0 .1 1 2) :duration dur)))
     (run 
      (lambda ()
	(do ((i start (1+ i)))
	    ((= i stop))
	  (set! (ercos-r gen) (env t-env))
	  (set! (ercos-cosh-t gen) (cosh (ercos-r gen)))
	  (let ((exp-t (exp (- (ercos-r gen)))))
	    (set! (ercos-offset gen) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	    (set! (ercos-scaler gen) (* (sinh (ercos-r gen)) (ercos-offset gen))))
	  (outa i (* amp (ercos gen 0.0))))))))

#|
;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:play #t)
  (ercoser 0 1 100 .5 0.1))
|#


(defgenerator (erssb
	       :make-wrapper (lambda (g)
			       (set! (erssb-frequency g) (hz->radians (erssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (erssb gen fm)
  "  (make-erssb frequency (ratio 1.0) (r 0.0)) creates an erssb generator (a special case of rssb).\n\
   (erssb gen fm) returns many sinusoids from frequency spaced by frequency * ratio with amplitude e^(-kr)."
  (declare (gen erssb) (fm float))
  (let* ((cx (erssb-angle gen))
	 (mx (* cx (erssb-ratio gen)))
	 (cxx (- cx mx))
	 (r (erssb-r gen))
	 (ccmx (- (cosh r) (cos mx))))

    (set! (erssb-angle gen) (+ cx fm (erssb-frequency gen)))

    (if (< (abs ccmx) nearly-zero)
	1.0
	(/ (- (* (cos cxx)
		 (- (/ (sinh r) ccmx)
		    1.0))
	      (* (sin cxx)
		 (/ (sin mx) ccmx)))
	   (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-erssb 1000.0 0.1 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(outa i (erssb gen 0.0)))))))
|#


#|
;;; --------------------------------------------------------------------------------
;;; removed 8-May-08 -- not useful or different from (for example) rk!cos

;;; inf sinusoids scaled by r^2: r2cos, r2sin, r2ssb

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(defgenerator (r2sin
	       :make-wrapper (lambda (g)
			       (set! (r2sin-frequency g) (hz->radians (r2sin-frequency g)))
			       (if (>= (* (r2sin-r g) (r2sin-r g)) 1.0)
				   (set! (r2sin-r g) 0.9999999))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))


(define (r2sin gen fm)
  "  (make-r2sin frequency (r 0.0)) creates an r2sin generator.\n\
   (r2sin gen fm) returns many even-numbered sines from frequency with amplitude r^(2k)/(2k)!."
  (declare (gen r2sin) (fm float))
  (let* ((x (r2sin-angle gen))
	 (r (r2sin-r gen)))

    (set! (r2sin-angle gen) (+ x fm (r2sin-frequency gen)))

    (* (sinh (* r (cos x)))
       (sin (* r (sin x))))))


;;; even harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2sin 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2sin gen 0.0)))))))



(defgenerator (r2cos
	       :make-wrapper (lambda (g)
			       (set! (r2cos-frequency g) (hz->radians (r2cos-frequency g)))
			       (if (>= (* (r2cos-r g) (r2sin-r g)) 1.0)
				   (set! (r2cos-r g) 0.9999999))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))


(define (r2cos gen fm)
  "  (make-r2cos frequency (r 0.0)) creates an r2cos generator.\n\
   (r2cos gen fm) returns many even-numbered cosines from frequency with amplitude r^(2k)/(2k)!."
  (declare (gen r2cos) (fm float))
  (let* ((x (r2cos-angle gen))
	 (r (r2cos-r gen)))

    (set! (r2cos-angle gen) (+ x fm (r2cos-frequency gen)))

    (/ (- (* (cosh (* r (cos x)))
	     (cos (* r (sin x))))
	  1.0)                   ; omit DC
       (- (cosh r) 1.0))))       ; normalize


;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2cos 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2cos gen 0.0)))))))



(defgenerator (r2ssb
	       :make-wrapper (lambda (g)
			       (set! (r2ssb-frequency g) (hz->radians (r2ssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (r2ssb gen fm)
  "  (make-r2ssb frequency (ratio 1.0) (r 0.0)) creates an r2ssb generator.\n\
   (r2ssb gen fm) returns many even-numbered sinusoids from frequency spaced by frequency * ratio, if that makes any sense, with amplitude r^(2k)/(2k)!."
  (declare (gen r2ssb) (fm float))
  (let* ((cx (r2ssb-angle gen))
	 (mx (* cx (r2ssb-ratio gen)))
	 (a (r2ssb-r gen))
	 (asinx (* a (sin mx)))
	 (acosx (* a (cos mx))))

    (set! (r2ssb-angle gen) (+ fm cx (r2ssb-frequency gen)))

    (/ (- (* (cos cx)
	     (cosh acosx)
	     (cos asinx))
	  (* (sin cx)
	     (sinh acosx)
	     (sin asinx)))
       (cosh a)))) ; normalization


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 0.1 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (r2ssb gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 0.1 0.5))
	(vib (make-oscil 5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (r2ssb gen (* (hz->radians 100.0) (oscil vib)))))))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; inf odd cosines scaled by e^-r: eoddcos

;;; Jolley 1st col 2nd row
;;;   heads toward a square wave as "r" -> 0.0 (odd harmonics, 1/k amp)

;;; this is the cos side of rkoddssb with r=e^-a

(defgenerator (eoddcos 
	       :make-wrapper (lambda (g)
			       (set! (eoddcos-osc g) (make-oscil (eoddcos-frequency g) (* 0.5 pi)))
			       g)
	       :methods (list
			 (list 'mus-frequency 
			       (lambda (g) (mus-frequency (eoddcos-osc g)))
			       (lambda (g val) (set! (mus-frequency (eoddcos-osc g)) val) val))

			 (list 'mus-phase 
			       (lambda (g) (mus-phase (eoddcos-osc g)))
			       (lambda (g val) (set! (mus-phase (eoddcos-osc g)) val) val))))
  (frequency *clm-default-frequency*) (r 1.0)
  (osc #f :type clm))


(define (eoddcos gen fm)
  "  (make-eoddcos frequency (r 0.0)) creates an eoddcos generator.\n\
   (eoddcos gen fm) returns many cosines from spaced by frequency with amplitude e^(-r)."
  (declare (gen eoddcos) (fm float))
  (let* ((a (eoddcos-r gen))
	 (sinha (sinh a)))
    (/ (atan (/ (oscil (eoddcos-osc gen) fm) sinha))
       (atan (/ 1.0 sinha))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 1.0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 10000))
      (outa i (eoddcos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 0.0))
	(a-env (make-env '(0 0 1 1) :length 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (eoddcos-r gen) (env a-env))
	     (outa i (eoddcos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-eoddcos 400.0 :r 0.0))
	(gen2 (make-oscil 400.0))
	(a-env (make-env '(0 0 1 1) :length 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (eoddcos-r gen1) (env a-env))
	     (outa i (* .5 (eoddcos gen1 (* .1 (oscil gen2))))))))))
|#


#|
;;; --------------------------------------------------------------------------------
;;; removed 6-May-08

;;; inf odd cosines scaled by complicated mess: koddcos

;;; Jolley 1st col 5th row 

(define make-koddcos make-oscil)

(define (koddcos gen fm)
  "  (make-koddcos frequency) creates a koddcos generator.\n\
   (koddcos gen fm) returns many cosines from spaced by frequency with amplitude too messy to write down, and the output looks wrong anyway."
  (declare (gen clm) (fm float))
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(/ (acos (- 1.0 arg)) pi)
	(/ (acos (+ 1.0 arg)) (- pi)))))


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-koddcos 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (koddcos gen 0.0))))))))


;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; we get odd harmonics but wrong amps
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k: rkcos, rksin, rkssb

;;; G&R 2nd col 6th row, also J 536
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(defgenerator (rkcos 
	       :make-wrapper (lambda (g)
			       (set! (rkcos-osc g) (make-oscil (rkcos-frequency g) (* 0.5 pi)))
			       (set! (rkcos-r g) (generator-clamp-r (rkcos-r g))) ; or clip at 0.0?
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (rkcos-osc g)))
			       (lambda (g val) (set! (mus-frequency (rkcos-osc g)) val) val))
			 
			 (list 'mus-scaler
			       (lambda (g) (rkcos-r g))
			       (lambda (g val)
				 (set! (rkcos-r g) (generator-clamp-r val))
				 (rkcos-r g)))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (rkcos-osc g)))
			       (lambda (g val) (set! (mus-phase (rkcos-osc g)) val) val))))
  (frequency *clm-default-frequency*) (r 0.0)
  (osc #f :type clm))

;;; not very flexible, and very similar to others in the r^k mold


(define (rkcos gen fm)
  "  (make-rkcos frequency (r 0.0)) creates an rkcos generator.\n\
   (rkcos gen fm) returns many cosines from spaced by frequency with amplitude (r^k)/k."
  (declare (gen rkcos) (fm float))
  (let ((cs (oscil (rkcos-osc gen) fm))
	(r (rkcos-r gen)))
    (/ (* 0.5 (log (+ 1.0 (* -2.0 r cs) (* r r))))
       (log (- 1.0 (abs r)))))) ; norm

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkcos gen 0.0)))))))
|#


(defgenerator (rksin
	       :make-wrapper (lambda (g)
			       (set! (rksin-frequency g) (hz->radians (rksin-frequency g)))
			       g)
	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rksin-r g))
			       (lambda (g val)
				 (set! (rksin-r g) (generator-clamp-r val))
				 (rksin-r g)))))

  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))

;;; normalization based on 0 of derivative of atan arg (for max) at cos x = r,
;;;   so we get a maxamp here of (atan (/ (* r (sin (acos r))) (- 1.0 (* r r))))


(define (rksin gen fm)
  "  (make-rksin frequency (r 0.0)) creates an rksin generator.\n\
   (rksin gen fm) returns many sines from spaced by frequency with amplitude (r^k)/k."
  (declare (gen rksin) (fm float))
  (let* ((x (rksin-angle gen))
	 (r (rksin-r gen)))

    (set! (rksin-angle gen) (+ fm x (rksin-frequency gen)))

    (/ (atan (/ (* r (sin x))
		(- 1.0 (* r (cos x)))))
       (atan (/ (* r (sin (acos r)))   ; normalization
		(- 1.0 (* r r)))))))       

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rksin 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rksin gen 0.0)))))))
|#

(defgenerator (rkssb
	       :make-wrapper (lambda (g)
			       (set! (rkssb-frequency g) (hz->radians (rkssb-frequency g)))
			       g)
	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rkssb-r g))
			       (lambda (g val)
				 (set! (rkssb-r g) (generator-clamp-r val))
				 (rkssb-r g)))))

  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rkssb gen fm)
  "  (make-rkssb frequency (ratio 1.0) (r 0.0)) creates an rkssb generator.\n\
   (rkssb gen fm) returns many sinusoids from frequency from spaced by frequency * ratio with amplitude (r^k)/k."
  (declare (gen rkssb) (fm float))
  (let* ((cx (rkssb-angle gen))
	 (mx (* cx (rkssb-ratio gen)))
	 (cxx (* (- 1.0 (rkssb-ratio gen)) cx))
	 (r (rkssb-r gen))
	 (rcosmx (* r (cos mx))))

    (set! (rkssb-angle gen) (+ cx fm (rkssb-frequency gen)))

    (/ (- (* (cos cxx)
	     -0.5 (log (+ 1.0 (* -2.0 rcosmx) (* r r))))
	  (* (sin cxx)
	     (atan (/ (* r (sin mx))
		      (- 1.0 rcosmx)))))
       (- (log (- 1.0 (abs r))))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkssb 1000.0 0.5 :r 0.75)) ; (make-rkssb 300.0 3.0 :r 0.5)
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* (env ampf) 
		    (rkssb gen 0.0))))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k!: rk!cos, rk!ssb

;;; G&R 2nd col 3rd from last (simplified)

(defgenerator (rk!cos
	       :make-wrapper (lambda (g)
			       (set! (rk!cos-frequency g) (hz->radians (rk!cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))


(define (rk!cos gen fm)
  "  (make-rk!cos frequency (r 0.0)) creates an rk!cos generator.\n\
   (rk!cos gen fm) returns many cosines spaced by frequency with amplitude (r^k)/k!."
  (declare (gen rk!cos) (fm float))
  (let* ((r (rk!cos-r gen))
	 (x (rk!cos-angle gen)))

    (set! (rk!cos-angle gen) (+ fm x (rk!cos-frequency gen)))

    (/ (- (* (exp (* r (cos x)))
	     (cos (* r (sin x))))
	  1.0) ; omit DC
       (- (exp (abs r)) 1.0)))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!cos gen 0.0)))))))
|#

;;; the k! denominator dominates, so r * ratio = formant center approximately; (n!)^(1/n) 
;;;   so freq=100, r=30, the center of the spectrum is around 3kHz:

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 100.0 :r 40.0)) 
	(r 40.0) 
	(incr (/ -40.0 100000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000)) 
	 (set! (rk!cos-r gen) r) 
	 (set! r (+ r incr))
	 (outa i (rk!cos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 300.0 :r 10.0)) 
	(ampf (make-env '(0 0 .1 1 .2 1 3 .5 5 .25 10 0) :scaler .5 :length 10000))
	(r 10.0) 
	(incr (/ -10.0 10000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000)) 
	 (set! (rk!cos-r gen) r) 
	 (set! r (+ r incr))
	 (outa i (* (env ampf) (rk!cos gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 1000.0 :r 8.0)) 
	(frqf (make-env '(0 1 1 0) :base 32 :scaler (hz->radians 1000) :length 10000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000)) 
	 (outa i (rk!cos gen (env frqf))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 3000.0 :r 1.0)) (ampf (make-env '(0 0 1 1 10 1 11 0) :length 10000))
	(frqf (make-env '(0 1 1 0 2 .25 3 0) :base 3 :scaler (hz->radians 2000) :length 10000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000)) 
	 (outa i (* (env ampf) (rk!cos gen (env frqf)))))))))

(with-sound (:play #t :scaled-to .5)
  (do ((k 0 (1+ k)))
      ((= k 6))
    (let ((gen (make-rk!cos 3000.0 :r 0.6)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 0 1 1) :base .1 :scaler (hz->radians 2000) :length 3000))) ; '(0 .5  1 1 2 0 3 0) '(0 1 1 0 2 1 6 -1)
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 3000)) 
	   (outa (+ i (* k 4000)) 
		 (* (env ampf) 
		    (rk!cos gen (env frqf))))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (do ((k 0 (1+ k)))
      ((= k 6))
    (let ((gen (make-rk!cos 1000.0 :r 1.0)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 .9 1 1 2 -1) :base .1 :scaler (hz->radians 500) :length 3000)))
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 3000)) 
	   (outa (+ i (* k 10000)) (* (env ampf) (rk!cos gen (env frqf))))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (do ((k 0 (1+ k)))
      ((= k 6))
    (let ((gen (make-rk!cos 500.0 :r 1.5)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 1 1 1 2 -1) :base .5 :scaler (hz->radians 400) :length 3000)))
      (run 
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i 3000)) 
	   (outa (+ i (* k 10000)) (* (env ampf) (rk!cos gen (env frqf))))))))))
|#


(defgenerator (rk!ssb
	       :make-wrapper (lambda (g)
			       (set! (rk!ssb-frequency g) (hz->radians (rk!ssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 1.0) (angle 0.0))


(define (rk!ssb gen fm)
  "  (make-rk!ssb frequency (ratio 1.0) (r 0.0)) creates an rk!ssb generator.\n\
   (rk!ssb gen fm) returns many sinusoids from frequency spaced by frequency * ratio with amplitude (r^k)/k!."
  (declare (gen rk!ssb) (fm float))
  (let* ((cx (rk!ssb-angle gen))
	 (mx (* cx (rk!ssb-ratio gen)))
	 (r (rk!ssb-r gen))
	 (ercosmx (exp (* r (cos mx))))
	 (rsinmx (* r (sin mx))))

    (set! (rk!ssb-angle gen) (+ fm cx (rk!ssb-frequency gen)))

    (/ (- (* (cos cx) ercosmx (cos rsinmx))
	  (* (sin cx) ercosmx (sin rsinmx)))
       (exp (abs r))))) ; normalization (keeping DC term here to get "carrier")
	  
#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!ssb 1000.0 0.1 :r 0.5)) ; (make-rk!ssb 200.0 3.0 :r 2)
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* (env ampf) (rk!ssb gen 0.0))))))))

; (make-rk!ssb 0.0 120.0 :r 15) gives a widely separated wave-train of pulses
;   so (make-rk!ssb 0.0 40.0 :r 70) is insecty (:r 100)
;      (make-rk!ssb 0.0 10.0 :r 100) -- some bird? (make-rk!ssb 0.0 15.0 :r 300)
;      (make-rk!ssb 1000.0 25.0 :r 10) (make-rk!ssb 3000.0 25.0 :r 100) -- another bird (5000)
|#

(definstrument (bouncy beg dur freq amp :optional (bounce-freq 5) (bounce-amp 20))
  (let* ((gen (make-rk!ssb (* freq 4) 1/4 :r 1.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler 3 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (rk!ssb-r gen) (+ (abs (* (env bouncef) 
					 (oscil gen1))) 
				 (env rf)))
	 (outa i (* (env ampf)
		    (rk!ssb gen 0.0))))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 300 .5 5 10))

(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 200 .5 3 2))
|#
			  


;;; --------------------------------------------------------------------------------
;;; rxyk!cos

(defgenerator (rxyk!sin
	       :make-wrapper (lambda (g)
			       (set! (rxyk!sin-frequency g) (hz->radians (rxyk!sin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rxyk!sin gen fm)
  "  (make-rxyk!sin frequency (ratio 1.0) (r 0.0)) creates an rxyk!sin generator.\n\
   (rxyk!sin gen fm) returns many sines from frequency spaced by frequency * ratio with amplitude r^k/k!."
  (declare (gen rxyk!sin) (fm float))
  (let* ((x (rxyk!sin-angle gen))
	 (y (* x (rxyk!sin-ratio gen)))
	 (r (rxyk!sin-r gen)))

    (set! (rxyk!sin-angle gen) (+ x fm (rxyk!sin-frequency gen)))

    (/ (* (exp (* r (cos y)))
	  (cos (+ x (* r (sin y)))))
       (exp (abs r)))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxyk!sin 1000 0.1 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rxyk!sin gen 0.0)))))))
|#


(defgenerator (rxyk!cos
	       :make-wrapper (lambda (g)
			       (set! (rxyk!cos-frequency g) (hz->radians (rxyk!cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rxyk!cos gen fm)
  "  (make-rxyk!cos frequency (ratio 1.0) (r 0.0)) creates an rxyk!cos generator.\n\
   (rxyk!cos gen fm) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k/k!."
  (declare (gen rxyk!cos) (fm float))
  (let* ((x (rxyk!cos-angle gen))
	 (y (* x (rxyk!cos-ratio gen)))
	 (r (rxyk!cos-r gen)))

    (set! (rxyk!cos-angle gen) (+ x fm (rxyk!cos-frequency gen)))

    (/ (* (exp (* r (cos y)))
	  (cos (+ x (* r (sin y)))))
       (exp (abs r)))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxyk!cos 1000 0.1 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rxyk!cos gen 0.0)))))))
|#

(definstrument (brassy beg dur freq amp ampf freqf gliss)
  (let* ((base-freq freq)
	 (gen (make-rxyk!cos base-freq :r 0.0))
	 (start (seconds->samples beg))
	 (samps (seconds->samples dur))
	 (stop (+ start samps))
	 (amp-env (make-env ampf :duration dur :scaler amp))
	 (pitch-env (make-env freqf :scaler gliss :duration dur))
	 (pitch-time .05)
	 (slant (make-moving-average (seconds->samples pitch-time)))
	 (amp-time .1)
	 (vib (make-oscil 5))
	 (vib-index (hz->radians 4.0))
	 )
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i samps))
	 (let* ((pitch (env pitch-env))
		(harmfrq (/ pitch base-freq))
		(harmonic (inexact->exact (floor harmfrq)))
		(dist (abs (- harmfrq harmonic)))
		(frq (* base-freq (moving-average slant harmonic))))
	   (set! (rxyk!cos-r gen) (* (/ 1.0 amp-time)
				     2.0
				     (if (< dist amp-time)
					 dist
					 (if (> dist (- 1.0 amp-time))
					     (- 1.0 dist)
					     amp-time))))
	   (outa i (* (env amp-env)
		      (rxyk!cos gen (+ (hz->radians frq)
				       (* vib-index (oscil vib))))))))))))
#|
(with-sound (:statistics #t :play #t)
  (brassy 0 4 50 .5 '(0 0 1 1 10 1 11 0) '(0 1 1 0) 1000))
|#


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by complicated mess: r2k!cos

;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines

(defgenerator (r2k!cos
	       :make-wrapper (lambda (g)
			       (set! (r2k!cos-osc g) (make-oscil (r2k!cos-frequency g)))
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (r2k!cos-osc g)))
			       (lambda (g val) (set! (mus-frequency (r2k!cos-osc g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (r2k!cos-osc g)))
			       (lambda (g val) (set! (mus-phase (r2k!cos-osc g)) val) val))))
  (frequency *clm-default-frequency*) (r 0.0) (k 0.0)
  (osc #f :type clm))


(define (r2k!cos gen fm)
  "  (make-2rk!cos frequency (r 0.0) (k 0.0)) creates an r2k!cos generator.\n\
   (r2k!cos gen fm) returns many cosines spaced by frequency with amplitude too messy to write down."
  (declare (gen r2k!cos) (fm float))
  (let* ((r (r2k!cos-r gen))
	 (k (r2k!cos-k gen))
	 (rr1 (+ 1.0 (* r r)))
	 (r2 (* 2 (abs r)))) ; abs for negative r
    (* (expt (- rr1
		(* r2 (oscil (r2k!cos-osc gen) fm)))
	     (- k))
       (expt (- rr1 r2) k)))) ; amplitude normalization

;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract (and there's lots of DC anyway)

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k!cos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)) 
	(indf (make-env '(0 1 1 0 10 0) :length 80000 :scaler 10.0 :offset 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 80000)) 
	 (set! (r2k!cos-k gen) (env indf))
	 (outa i (r2k!cos gen 0.0)))))))
|#

(definstrument (pianoy beg dur freq amp)
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (r2k!cos gen 0.0))))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy 0 3 100 .5))
;;; this can be combined with bouncy-like changes to get an evolving sound
|#

(definstrument (pianoy1 beg dur freq amp :optional (bounce-freq 5) (bounce-amp 20))
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler .1 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (r2k!cos-r gen) (+ .25 (abs (* (env bouncef) 
					 (oscil gen1)))
				 (env rf)))
	 (outa i (* (env ampf)
		    (r2k!cos gen 0.0))))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy1 0 4 200 .5 1 .1))
|#

(definstrument (pianoy2 beg dur freq amp)
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (knock (make-fmssb 10.0 20.0 :index 1.0))
	 (kmpf (make-env '(0 0 1 1 3 1 100 0) :base 3 :scaler .05 :length 30000))
	 (indf (make-env '(0 1 1 0) :length 30000 :base 3 :scaler 10))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (fmssb-index knock) (env indf))
	 (outa i (+ (* (env ampf)
		       (r2k!cos gen 0.0))
		    (* (env kmpf) 
		       (fmssb knock 0.0)))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (pianoy2 0 1 100 .5))
|#


;;; --------------------------------------------------------------------------------

;;; inf sines scaled by 1/2^k: k2sin

;;; Jolley 1st col 1st row

;;; not flexible -- very similar to several others

(defgenerator (k2sin
	       :make-wrapper (lambda (g)
			       (set! (k2sin-frequency g) (hz->radians (k2sin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (angle 0.0))


(define (k2sin gen fm)
  "  (make-k2sin frequency) creates a k2sin generator.\n\
   (k2sin gen fm) returns many sines spaced by frequency with amplitude 1/(2^k)."
  (declare (gen k2sin) (fm float))
  (let ((x (k2sin-angle gen)))

    (set! (k2sin-angle gen) (+ x fm (k2sin-frequency gen)))

    (/ (* 3.0 (sin x)) ; 3 rather than 4 for normalization
       (- 5.0 (* 4.0 (cos x))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2sin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2sin gen 0.0)))))))
|#


;;; using the 2nd Sansone formula, we get the sum of cos case by using a=-5b/4 or 3/(4cosx-5)

(defgenerator (k2cos
	       :make-wrapper (lambda (g)
			       (set! (k2cos-frequency g) (hz->radians (k2cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (angle 0.0))


(define (k2cos gen fm)
  "  (make-k2cos frequency) creates a k2cos generator.\n\
   (k2cos gen fm) returns many cosines spaced by frequency with amplitude 1/(2^k)."
  (declare (gen k2cos) (fm float))
  (let ((x (k2cos-angle gen)))

    (set! (k2cos-angle gen) (+ x fm (k2cos-frequency gen)))

   (* 0.5 (- (/ 3.0
		(- 5.0 (* 4.0 (cos x))))
	     1.0))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-k2cos 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2cos gen 0.0)))))))
|#


(defgenerator (k2ssb
	       :make-wrapper (lambda (g)
			       (set! (k2ssb-frequency g) (hz->radians (k2ssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (angle 0.0))


(define (k2ssb gen fm)
  "  (make-k2ssb frequency (ratio 1.0)) creates a k2ssb generator.\n\
   (k2ssb gen fm) returns many sinusoids from frequency spaced by frequency * ratio with amplitude 1/(2^k)."
  (declare (gen k2ssb) (fm float))
  (let* ((cx (k2ssb-angle gen))
	 (mx (* cx (k2ssb-ratio gen))))

    (set! (k2ssb-angle gen) (+ fm cx (k2ssb-frequency gen)))

    (/ (- (* 3 (cos cx))
	  (* (sin cx) 
	     (* 4.0 (sin mx))))
       (* 3.0 (- 5.0 (* 4.0 (cos mx)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2ssb 1000.0 0.1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2ssb gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------


;;; this was inspired by Andrews, Askey, Roy "Special Functions" p396, but there's an error somewhere...
;;;   it produces sum r^k sin(2k-1)x
;;;   (not normalized)

(defgenerator (dblsum
	       :make-wrapper (lambda (g)
			       (set! (dblsum-frequency g) (hz->radians (* 2 (dblsum-frequency g))))
			       g)
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (radians->hz (* 0.5 (dblsum-frequency g))))
			       (lambda (g val) (set! (dblsum-frequency g) (hz->radians (* 2 val))) val))))
  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))


(define (dblsum gen fm)
  "  (make-dblsum frequency (r 0.0)) creates a dblsum generator.\n\
   (dblsum gen fm) returns many sines from frequency spaced by frequency * (2k -1) with amplitude r^k (this is buggy)."
  (declare (gen dblsum) (fm float))
  (let* ((x (dblsum-angle gen))
	 (r (dblsum-r gen)))
    (set! (dblsum-angle gen) (+ fm (dblsum-angle gen) (dblsum-frequency gen)))
    (/ (* (+ 1 r) (sin (* 0.5 x)))
       (* (- 1 r) (+ 1.0 (* -2.0 r (cos x)) (* r r))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-dblsum 100 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (dblsum gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf odd sinusoids scaled by r^odd-k/odd-k: rkoddssb

;;;  G&R 2nd col rows 7&8 (odd r^k/k) 

(defgenerator (rkoddssb
	       :make-wrapper (lambda (g)
			       (set! (rkoddssb-frequency g) (hz->radians (rkoddssb-frequency g)))
			       (set! (rkoddssb-r g) (generator-clamp-r (rkoddssb-r g)))
			       g)

	       :methods (list
			 (list 'mus-scaler
			       (lambda (g) (rkoddssb-r g))
			       (lambda (g val)
				 (set! (rkoddssb-r g) (generator-clamp-r val))
				 (rkoddssb-r g)))))

  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.0) (angle 0.0))


(define (rkoddssb gen fm)
  "  (make-rkoddssb frequency (ratio 1.0) (r 0.0)) creates an rkoddssb generator.\n\
   (rkoddssb gen fm) returns many sinusoids from frequency spaced by frequency * 2 * ratio with amplitude (r^(2k-1))/(2k-1)."
  (declare (gen rkoddssb) (fm float))
  (let* ((r (rkoddssb-r gen))
	 (cx (rkoddssb-angle gen))
	 (mx (* cx (rkoddssb-ratio gen)))
	 (cxx (- cx mx)))

    (set! (rkoddssb-angle gen) (+ fm cx (rkoddssb-frequency gen)))

    (/ (- (* (cos cxx)
	     0.5
	     (log (/ (+ 1.0 (* 2.0 r (cos mx)) (* r r))
		     (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
	  (* (sin cxx)
	     (atan (/ (* 2.0 r (sin mx))
		      (- 1.0 (* r r))))))

       (- (log (+ 1 r))    ; normalization (r^k/k for odd k)
	  (log (- 1 r))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkoddssb 1000.0 0.1 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkoddssb gen 0.0)))))))
|#

(definstrument (glassy beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (n (inexact->exact (floor (/ (mus-srate) (* 3 freq)))))
	 (r (expt .001 (/ 1 n)))
	 (clang (make-rkoddssb (* freq 2) (/ 1.618 2) r))
	 (clangf (make-env (list 0 0 .01 1 .1 1 .2 .4 (max .3 dur) 0) :scaler amp :duration dur))
	 (crf (make-env (list 0 1 1 0) :scaler r :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (rkoddssb-r clang) (env crf))
	 (outa i (* (env clangf)
		    (rkoddssb clang 0.0))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (glassy 0 .1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (glassy (* i .3) .1 (+ 400 (* 100 i)) .5)))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 5000.0 0.1 0.95))
	(ampf (make-env '(0 0 9 1 10 0) :base 32 :length 10000))
	(noi (make-rand 10000 .1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* (env ampf) (sin (rkoddssb gen (rand noi))))))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by kr^k: krksin

;;; Zygmund 1st
;;;   this looks interesting, but how to normalize?  sum of sines is bad enough, kr^k -> 1/(1-x)^2 if x^2<1 (G&R 113)
;;;   for low n, we could use the Tn roots stuff (clm.c)
;;;   the formula must be assuming r<1.0 -- if greater than 1 it's acting like r2k! above

(defgenerator (krksin
	       :make-wrapper (lambda (g)
			       (set! (krksin-frequency g) (hz->radians (krksin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (angle 0.0))


(define (krksin gen fm)
  "  (make-krksin frequency (r 0.0)) creates a krksin generator.\n\
   (krksin gen fm) returns many sines spaced by frequency with amplitude kr^k."
  (declare (gen krksin) (fm float))
  (let* ((x (krksin-angle gen))
	 (r (krksin-r gen))
	 (r2 (* r r))
	 (den (+ 1.0 (* -2.0 r (cos x)) r2)))

    (set! (krksin-angle gen) (+ fm (krksin-angle gen) (krksin-frequency gen)))

    (/ (* r (- 1 r2) (sin x))
       (* den den))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-krksin 440.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (krksin gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :scaled-to .5 :play #t)
  (let ((gen (make-krksin 6.0 0.965))) ; 60 .6 also
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000))
	 (outa i (krksin gen 0.0)))))))

(do ((i 0 (1+ i)))
    ((= i 10))
  (let ((mx (vct-peak (with-sound (:clipped #f :output (make-vct 10000))
				  (let ((gen (make-krksin 20.0 (* i 0.1))))
				    (run 
				     (lambda ()
				       (do ((i 0 (1+ i)))
					   ((= i 10000))
					 (outa i (krksin gen 0.0))))))))))
    (snd-display ";~A: ~A" (* 0.1 i) mx)))
|#

#|
;;; --------------------------------------------------------------------------------

;;; absolute value of oscil: abssin

;;; Zygmund 2nd -- not actually very useful, but shows cos 2nx of abs

(defgenerator (abssin
	       :make-wrapper (lambda (g)
			       (set! (abssin-osc g) (make-oscil (abssin-frequency g)))
			       g)	       
	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (abssin-osc g)))
			       (lambda (g val) (set! (mus-frequency (abssin-osc g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (abssin-osc g)))
			       (lambda (g val) (set! (mus-phase (abssin-osc g)) val) val))))
  (frequency *clm-default-frequency*)
  (osc #f :type clm))


(define (abssin gen fm)
  "  (make-abssin frequency) creates an abssin generator.\n\
   (abssin gen fm) returns (abs oscil)."
  (declare (gen abssin) (fm float))
  (/ (- (abs (oscil (abssin-osc gen) fm))
	(/ 2.0 pi))
     (/ 2.0 pi)))  ; original went from 0 to 1.0, subtract 2/pi, and we get peak at -2/pi

;; DC: sin^2 x = 1/2 - cos 2x, 
;;   so every term in the sum adds 1/(2(4k^2-1)) -> 1/4 (J 397 or 373)
;;   so DC is 2/pi = 0.6366


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abssin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abssin gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((vib (make-abssin 100.0)) ; spacing will be 200, if FM you get index-proportional amount as constant offset
	(gen (make-oscil 1000.0))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :scaler .5 :length 20000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i 
	       (* (env ampf)
		  (oscil gen 0.0 (* 3 (abssin vib 0.0))))))))))

;;; pitch is 2*freq, 200 1, 400 .203, 600 .087, 800 .049, 1000 .031, 1200 .021
;;;                      1      .2        .086      .048       .030       .021 -- (/ 3.0 (- (* 4 (* 6 6)) 1))
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines, scaled by (-a+sqrt(a^2-b^2))^n/b^n: abcos

;;; from Sansone, p182, assumptions: a not 0, b not 0, b/a real, abs(b/a)<1 (b less than a)

(defgenerator (abcos
	       :make-wrapper (lambda (g)
			       (set! (abcos-frequency g) (hz->radians (abcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (a 0.0) (b 0.0) (angle 0.0))


(define (abcos gen fm)
  "  (make-abcos frequency (a 0.0) (b 0.0)) creates an abcos generator.\n\
   (abcos gen fm) returns many cosines spaced by frequency with amplitude (-a+sqrt(a^2-b^2))^k/b^k."
  (declare (gen abcos) (fm float))
  (let* ((x (abcos-angle gen))
	 (a (abcos-a gen))
	 (b (abcos-b gen))
	 (norm (/ 0.5 (- (/ 1.0 
			    (- 1.0 (/ (abs (- (sqrt (- (* a a) (* b b))) 
					      a)) 
				      b))) 
			 1.0)))) ;; i.e. 1/(1-r) -1 because we start at k=1, r=the complicated a/b business

    (set! (abcos-angle gen) (+ fm x (abcos-frequency gen)))

    (* norm (- (/ (sqrt (- (* a a) (* b b)))
		  (+ a (* b (cos x))))
	       1.0))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abcos 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abcos gen 0.0)))))))
|#


(defgenerator (absin
	       :make-wrapper (lambda (g)
			       (set! (absin-frequency g) (hz->radians (absin-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (a 0.0) (b 0.0) (angle 0.0))


(define (absin gen fm)
  "  (make-absin frequency (a 0.0) (b 0.0)) creates an absin generator.\n\
   (absin gen fm) returns many sines spaced by frequency with amplitude (-a+sqrt(a^2-b^2))^k/b^k."
  (declare (gen absin) (fm float))
  (let* ((x (absin-angle gen))
	 (a (absin-a gen))
	 (b (absin-b gen)))

    (set! (absin-angle gen) (+ fm x (absin-frequency gen)))

    (/ (* (sin x) 
	  (sqrt (- (* a a) (* b b))))
       (+ a (* b (cos x))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-absin 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (absin gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by 1/(r^2+k^2): r2k2cos

;;; J 2nd col 3rd row

(defgenerator (r2k2cos
	       :make-wrapper (lambda (g)
			       (set! (r2k2cos-frequency g) (hz->radians (r2k2cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (angle 0.0))


(define (r2k2cos-norm a)
  "  (make-r2k2cos frequency (r 1.0)) creates an r2k2cos generator.\n\
   (r2k2cos gen fm) returns many cosines spaced by frequency with amplitude 1/(r^2+k^2)."
  ;; J 124
  (- (* (/ pi (* 2 a))
	(/ (cosh (* pi a))
	   (sinh (* pi a))))
     (/ 1.0
	(* 2 a a))))

(define (r2k2cos gen fm)
  (declare (gen r2k2cos) (fm float))
  (let* ((x (r2k2cos-angle gen))
	 (a (r2k2cos-r gen)))
    (if (> x (* 2 pi))
	(set! x (fmod x (* 2 pi))))

    (set! (r2k2cos-angle gen) (+ x fm (r2k2cos-frequency gen)))

    (/ (- (* pi (/ (cosh (* a (- pi x)))
		   (sinh (* a pi))))
	  (/ 1.0 a))
       (* 2 a (r2k2cos-norm a)))))


#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k2cos 100.0 1.0))) ; 400 .25 -- this isn't very flexible
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k2cos gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------

;;; coskx/k = -ln(2sin(x/2)) or 1/2ln(1/(2-2cosx))
;;; sinkx/k = (pi-x)/2 both 0..2pi
;;; similarly -1^k : x/2 and ln(2cos(x/2)) (p44..46)
;;; 2k-1: pi/x and 1/2ln cot (x/2) 0..2pi and 0..pi
;;; but all of these are unbounded, and discontinuous

;;; --------------------------------------------------------------------------------

#|
;;;  from Stilson/Smith apparently -- was named "Discrete Summation Formula" which doesn't convey anything to me
;;;    Alexander Kritov suggests time-varying "a" is good (this is a translation of his code)

(defgenerator (blsaw
	       :make-wrapper (lambda (g)
			       (set! (blsaw-frequency g) (hz->radians (blsaw-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (r 0.0) (angle 0.0))


(define (blsaw gen fm)
  "  (make-blsaw frequency (n 1) (r 0.0)) creates a blsaw generator.\n\
   (blsaw gen fm) returns a band-limited sawtooth wave."
  (declare (gen blsaw) (fm float))
  (let* ((a (blsaw-r gen))
	 (N (blsaw-n gen))
	 (x (blsaw-angle gen))
	 (incr (blsaw-frequency gen))
	 (den (+ 1.0 (* -2.0 a (cos x)) (* a a))))
    (set! (blsaw-angle gen) (+ x fm incr))
    (if (< (abs den) nearly-zero)
	0.0
	(let* ((s1 (* (expt a (- N 1.0)) (sin (+ (* (- N 1.0) x) incr))))
	       (s2 (* (expt a N) (sin (+ (* N x) incr))))
	       (s3 (* a (sin (+ x incr)))))
	  (/ (+ (sin incr) 
		(- s3) 
		(- s2) 
		s1) 
	     den)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-blsaw 440.0 :r 0.5 :n 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (blsaw gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(defgenerator (asyfm
	       :make-wrapper (lambda (gen)
			       (set! (asyfm-frequency gen) (hz->radians (asyfm-frequency gen)))
			       gen))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 1.0) (index 1.0) (phase 0.0))


(define (asyfm-J gen input)
  "(asyfm-J gen input) is the same as the CLM asymmetric-fm generator (index=1.0), set r != 1.0 to get the asymmetric spectra"
  (declare (gen asyfm) (input float))
  (let* ((phase (asyfm-phase gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (one (if (or (> r 1.0) 
		      (and (< r 0.0)
			   (> r -1.0)))
		  -1.0 1.0))
	 (modphase (* (asyfm-ratio gen) phase))
	 (result (* (exp (* 0.5 index (- r r1) (+ one (cos modphase))))
		    (cos (+ phase (* 0.5 index (+ r r1) (sin modphase))))))) ; use cos, not sin, to get predictable amp
    (set! (asyfm-phase gen) (+ phase input (asyfm-frequency gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-J gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1 :index 1))
	(r-env (make-env '(0 -4 1 -1) :length 20000)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (set! (asyfm-r gen) (env r-env))
	 (outa i (asyfm-J gen 0.0)))))))

(define (val index r)
  (let ((sum 0.0))
    (do ((i -20 (1+ i)))
	((= i 21))
      (set! sum (+ sum (* (expt r i) (bes-jn i index)))))
    (let ((norm (exp (* 0.5 index (- r (/ 1.0 r))))))
      (list sum norm))))

(for-each
 (lambda (index)
   (for-each
    (lambda (r)
      (let ((peak (vct-peak (with-sound (:clipped #f :output (make-vct 1000))
					(let ((gen (make-asymmetric-fm 2000.0 :ratio .1 :r r)))
					  (run 
					   (lambda () 
					     (do ((i 0 (1+ i)))
						 ((= i 1000))
					       (outa i (asymmetric-fm gen index))))))))))
	(if (> (abs (- peak 1.0)) .1)
	    (snd-display ";asymmetric-fm peak: ~A, index: ~A, r: ~A" peak index r))))
    (list -10.0 -1.5 -0.5 0.5 1.0 1.5 10.0)))
 (list 1.0 3.0 10.0))
|#

(define (asyfm-I gen input)
  "(dsp-asyfm-I gen input) is the I0 case of the asymmetric-fm generator (dsp.scm)"
  (declare (gen asyfm) (input float))
  (let* ((phase (asyfm-phase gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (modphase (* (asyfm-ratio gen) phase))
	 (result (* (exp (* 0.5 index (+ r r1) (- (cos modphase) 1.0)))
		    (cos (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
    (set! (asyfm-phase gen) (+ phase input (asyfm-frequency gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-I gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

;;; bess (returns bes-jn, like oscil returns sin) normalized to peak at 1.0
;;;   frequency here is the frequency in Hz of the damped sinusoid part of the bessel function

(define bessel-peaks (vct 1.000 0.582 0.487 0.435 0.400 0.375 0.355 0.338 0.325 0.313 0.303 0.294 0.286 0.279 0.273 0.267 0.262 0.257 0.252 0.248))

(defgenerator (bess
	       :make-wrapper (lambda (g)
			       (set! (bess-frequency g) (hz->radians (bess-frequency g)))
			       (if (>= (bess-n g) (vct-length bessel-peaks)) 
				   (set! (bess-norm g) (/ 0.67 (expt (bess-n g) (exact->inexact 1/3))))
				   ;; this formula comes from V P Krainov, "Selected Mathetical Methods in Theoretical Physics"
				   (set! (bess-norm g) (vct-ref bessel-peaks (bess-n g))))
			       g))
  (frequency *clm-default-frequency*) (n 0 :type int) (angle 0.0) (norm 1.0))

(define (bess gen fm)
  "  (make-bess frequency (n 0)) creates a bessel function (Jn) generator.\n\
   (bess gen fm) returns Jn."
  (declare (gen bess) (fm float))
  (let ((result (/ (bes-jn (bess-n gen) (bess-angle gen)) 
		   (bess-norm gen))))
    (set! (bess-angle gen) (+ (bess-angle gen) (bess-frequency gen) fm))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-bess 100.0 :n 0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 1000))
      (outa i (bess gen 0.0)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-bess 400.0 :n 1))
	(vol (make-env '(0 0 1 1 9 1 10 0) :scaler 2.0 :length 20000)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (bess gen2 0.0)))))))))

;;; max amps:
(do ((i 1 (1+ i)))
    ((or (c-g?) (= i 100)))
  (let ((mx 0.0))
    (do ((k 0.0 (+ k .001)))
	((> k 200))
      (let ((val (bes-jn i k)))
	(if (> (abs val) mx)
	    (set! mx (abs val)))))
    (snd-display ";~A" (+ mx .001))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-oscil 400.0))
	(vol (make-env '(0 1 1 0) :scaler 1.0 :length 20000)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (oscil gen2 0.0)))))))))

;;; also gen2 800, env scl 0.2
|#


;;; --------------------------------------------------------------------------------

;;; Watson "Bessel Functions" p358 127 128 (J0(k sqrt(r^2+a^2- 2ar cos x)) = sum em Jm(ka)Jm(kr) cos mx
;;;   em here is "Neumann's factor" (p22) = 1 if m=0, 2 otherwise

(defgenerator (jjcos
	       :make-wrapper (lambda (g)
			       (set! (jjcos-frequency g) (hz->radians (jjcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (a 0.0) (k 1.0) (angle 0.0))


(define (jjcos gen fm)
  "  (make-jjcos frequency (r 0.0) (a 0.0) (k 1)) creates a jjcos generator.\n\
   (jjcos gen fm) returns a sum of cosines scaled by a product of Bessel functions."
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (a (jjcos-a gen))
	 (r (jjcos-r gen))
	 (k (jjcos-k gen))
	 (dc (* (bes-j0 (* k a)) (bes-j0 (* k r))))
	 (norm (- (bes-j0 (* k (sqrt (+ (* a a) (* r r) (* -2 a r))))) dc)))

    ;; this norm only works if the a/r/k values all small enough that the initial J0 bump dominates
    ;;   if they're large (k=10 for example), later maxes come into play.
    ;; we need a formula for a sum of JJ's
    ;;
    ;; the resultant spectra are similar to FM (we can get sharper bumps, or low-passed bumps, etc)

    (set! (jjcos-angle gen) (+ x fm (jjcos-frequency gen)))

    (/ (- (bes-j0 (* k (sqrt (+ (* r r) 
				(* a a)
				(* a r -2.0 (cos x))))))
	  dc)             ; get rid of DC component
       norm)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjcos gen 0.0)))))))

;;; example:
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 2.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (jjcos gen 0.0)))))))

:(* (bes-jn 1 1) (bes-jn 1 2))
0.253788089467046
:(* (bes-jn 2 1) (bes-jn 2 2))
0.0405418594904987
:(* (bes-jn 3 1) (bes-jn 3 2))
0.00252256243314325
:(* (bes-jn 4 1) (bes-jn 4 2))
8.41951242883886e-5
which matches perfectly

set k=10
:(* (bes-jn 1 10) (bes-jn 1 20))
0.00290541944296873
:(* (bes-jn 2 10) (bes-jn 2 20))
-0.0408277687368493
:(* (bes-jn 3 10) (bes-jn 3 20))
-0.00577380202685643
:(* (bes-jn 4 10) (bes-jn 4 20))
-0.0286956880041051
:(* (bes-jn 5 10) (bes-jn 5 20))
-0.0353830269096024
:(* (bes-jn 6 10) (bes-jn 6 20))
7.96480491715688e-4
:(* (bes-jn 7 10) (bes-jn 7 20))
-0.0399227881572529
:(* (bes-jn 8 10) (bes-jn 8 20))
-0.0234795438775677
:(* (bes-jn 9 10) (bes-jn 9 20))
0.0365188087949483
:(* (bes-jn 10 10) (bes-jn 10 20))
0.0386925399194178
:(* (bes-jn 11 10) (bes-jn 11 20))
0.00755397504265978
:(* (bes-jn 12 10) (bes-jn 12 20))
-0.00754046620160803
:(* (bes-jn 13 10) (bes-jn 13 20))
-0.00591450759566936
:(* (bes-jn 14 10) (bes-jn 14 20))
-0.00175050411436045
:(* (bes-jn 15 10) (bes-jn 15 20))
-3.66078549147997e-6

which again matches


(define (jjsin gen fm)
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (a (jjcos-a gen))
	 (r (jjcos-r gen))
	 (k (jjcos-k gen)))

    (set! (jjcos-angle gen) (+ x fm (jjcos-frequency gen)))

    (* (sin x)
       (bes-j0 (* k (sqrt (+ (* r r) 
			     (* a a)
			     (* a r -2.0 (cos x)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjsin gen 0.0)))))))

(define (jjesin gen fm)
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (r (jjcos-r gen)))

    (set! (jjcos-angle gen) (+ x fm (jjcos-frequency gen)))

    (* (exp (* r (- (cos x) 1.0))) ; -1 for norm , but there's huge DC offset
       (bes-j0 (* r (sin x))))))


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjesin gen 0.0)))))))

|#


;;; --------------------------------------------------------------------------------

;;; check J0(zsinx) formula 
;;; main difference from FM: index is divided by 2, J value is squared, else just like cos(sin)

(defgenerator (j0evencos
	       :make-wrapper (lambda (g)
			       (set! (j0evencos-frequency g) (hz->radians (j0evencos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (index 1.0) (angle 0.0))


(define (j0evencos gen fm)
  "  (make-j0evencos frequency (index 1.0)) creates a j0evencos generator.\n\
   (j0evencos gen fm) returns a sum of cosines scaled Jk^2(index/2)."
  (declare (gen j0evencos) (fm float))
  (let* ((x (j0evencos-angle gen))
	 (z (j0evencos-index gen))
	 (j0 (bes-j0 (* 0.5 z)))
	 (dc (* j0 j0)))

    (set! (j0evencos-angle gen) (+ x fm (j0evencos-frequency gen)))

    (if (= dc 1.0)
	1.0
	(/ (- (bes-j0 (* z (sin x)))
	      dc)        ; get rid of DC component
	   (- 1.0 dc))))) ; normalize

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (j0evencos gen 0.0)))))))

index 10 (so 10/2 is the bes-jn arg):

(let ((base (* (bes-jn 4 5.0) (bes-jn 4 5.0)))) ; max (fft norms -> 1.0)
  (do ((i 1 (1+ i)))
      ((= i 11))
    (snd-display ";~A: ~A ~A" i (* (bes-jn i 5.0) (bes-jn i 5.0)) (/ (* (bes-jn i 5.0) (bes-jn i 5.0)) base))))
;1: 0.107308091385168 0.701072497819036
;2: 0.00216831005396058 0.0141661502497507
;3: 0.133101826831083 0.86958987897572
;4: 0.153062759870046 1.0
;5: 0.0681943848279407 0.445532178342005
;6: 0.0171737701015899 0.112200839160164
;7: 0.00284904116112987 0.0186135488707298
;8: 3.38752000110201e-4 0.00221315753353599
;9: 3.04735259399795e-5 1.99091705688911e-4
;10: 2.15444461145164e-6 1.4075563600714e-5

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :length 30000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :length 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (oscil carrier) (j0evencos gen 0.0))))))))

;;; why no "carrier"?  I subtracted DC out above -- to make this look right, I need to use the bes(sin) without any fixup.

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 20 1 0) :length 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (oscil carrier)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0))                 ; also 20 800, 20 200 (less index mvt), or 200 50 
	(indf (make-env '(0 10 1 0) :length 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (* .1 (oscil carrier))))))))))

(define (j0even beg dur freq amp mc-ratio index)
  (let* ((gen (make-j0evencos (* mc-ratio freq) 0.0)) 
	 (indf (make-env '(0 10 1 0) :duration dur))
	 (carrier (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (* index (oscil carrier))))))))))

(with-sound (:clipped #f :statistics #t :play #t)
	    (do ((i 0 (1+ i)))
		((= i 10))
	      (j0even i 1.0 2000.0 0.5 (+ .1 (* .05 i)) 0.1)))

(define* (jfm beg dur freq amp mc-ratio index :optional (index-env '(0 1 1 1 2 0)))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (md (make-j0evencos (* freq mc-ratio)))
	 (cr (make-oscil 2000))
	 (vib (make-oscil 5))
	 (vibamp (hz->radians (* freq .01)))
         (ampf (make-env '(0 0 1 1 20 1 21 0) :scaler amp :duration dur)) 
         (indf (make-env index-env :scaler index :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (let ((vb (* vibamp (oscil vib))))
	   (set! (j0evencos-index md) (env indf))
	   (outa i (* (env ampf)
		      (oscil cr vb)
		      (j0evencos md (* vb mc-ratio))))))))))

(with-sound (:output "test1.snd" :play #t) (jfm 0 3.0 400.0 0.5 .5 4.0 '(0 1  1 2  2 .5)))
|#


;;; --------------------------------------------------------------------------------

(defgenerator (j2cos
	       :make-wrapper (lambda (g)
			       (set! (j2cos-frequency g) (hz->radians (j2cos-frequency g)))
			       (if (< (j2cos-n g) 1) (set! (j2cos-n g) 1))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (n 1 :type int) (angle 0.0))


(define (j2cos gen fm)
  "  (make-j2cos frequency (r 0.0) (n 1)) creates a j2cos generator.\n\
   (j2cos gen fm) returns a sum of cosines scaled in a very complicated way."
  (declare (gen j2cos) (fm float))
  (let* ((x (j2cos-angle gen))
	 (r (j2cos-r gen))
	 (rsinx2 (* 2.0 r (sin (* 0.5 x))))
	 (n (j2cos-n gen)))

    (set! (j2cos-angle gen) (+ x fm (j2cos-frequency gen)))

    (if (< (abs rsinx2) nearly-zero)
	1.0
	(/ (bes-jn n rsinx2)
	   rsinx2))))

;;; this goes berserk if n=0, needs normalization, dc omission, doc/test
;;; if n=1, sample 0 = 1, the rest are in the .5 range!
;;; maybe j2cos isn't all that useful...

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j2cos 100.0 :r 1.0 :n 0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (j2cos gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

(defgenerator (jpcos
	       :make-wrapper (lambda (g)
			       (set! (jpcos-frequency g) (hz->radians (jpcos-frequency g)))
			       (if (= (jpcos-r g) (jpcos-a g))
				   (begin
				     (snd-warning (format #f ";jpcos r and a can't be equal (~A)" (jpcos-r g)))
				     (set! (jpcos-r g) (+ (jpcos-a g) .01))))
			       g))
  (frequency *clm-default-frequency*) (r 0.0) (a 0.0) (k 1.0) (angle 0.0))


(define (jpcos gen fm)
  "  (make-jpcos frequency (r 0.0) (a 0.0) (k 1)) creates a jpcos generator.\n\
   (jpcos gen fm) returns a sum of cosines scaled in a very complicated way."
  (declare (gen jpcos) (fm float))
  (let* ((x (jpcos-angle gen))
	 (a (jpcos-a gen))
	 (r (jpcos-r gen))
	 (k (jpcos-k gen))
	 ;; (dc (/ (* (sin (* k a)) (sin (* k r))) (* k a r)))
	 ;; from P0(x)=1, J[1/2](x)=sqrt(2/(pi x))sin(x), omitting original 1/pi
	 ;;   G&R 914 (8.464), 974 (8.912), but it's missing some remaining (small) component
	 ;; also omitting the original divide by (* pi (sqrt arg)) -- it's just an amplitude scaler
	 ;;   and in this context, we get -1..1 peak amps from the sin anyway.
	 (arg (+ (* r r) 
		 (* a a)
		 (* a r -2.0 (cos x)))))

    (set! (jpcos-angle gen) (+ x fm (jpcos-frequency gen)))

    (if (< (abs arg) nearly-zero) ; r = a, darn it! This will produce a spike, but at least it's not a NaN
	1.0
	(sin (* k (sqrt arg))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-jpcos 100.0 :a 1.0 :r 0.5 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 210000))
	 (outa i (jpcos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-jpcos 400.0 :a 1.0 :r 0.5 :k 10))
	 (dur 1.0)
	 (samps (seconds->samples dur))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler 0.5))
	 (indf (make-env '(0 0 1 1) :duration dur :scaler 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i samps))
	 (set! (jpcos-r gen) (env indf))
	 (outa i (* (env ampf)
		    (jpcos gen 0.0))))))))

;;; -.725, 1/.275
(with-sound (:clipped #f :scaled-to .5) 
  (let* ((gen (make-oscil 100.0))) 
    (do ((i 0 (1+ i))) 
	((= i 44100)) 
      (outa i (sqrt (+ 1.0 (oscil gen)))))))

(with-sound (:clipped #f :scaled-to .5) 
  (let* ((gen (make-oscil 100.0))
	 (indf (make-env '(0 .1 1 .9) :length 44100)))
    (do ((i 0 (1+ i))) 
	((= i 44100)) 
      (let ((ind (env indf)))
	(outa i (sqrt (+ (* 1.0 1.0) (* ind ind) (* -2 1.0 ind (oscil gen)))))))))

;;; rkcos r=.4 or so (.6?), so rkcos+indf is mostly equivalent? (k=scaler in both)

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkcos 440.0 :r 0.6)) 
	(gen1 (make-oscil 440.0)) 
	(indf (make-env '(0 .1 1 .8) :length 50000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 50000)) 
	 (set! (rkcos-r gen) (env indf))
	 (outa i (oscil gen1 (* (rkcos-r gen) (rkcos gen 0.0)))))))))
|#



;;; --------------------------------------------------------------------------------

(defgenerator (jncos :make-wrapper (lambda (g)
				     (set! (jncos-frequency g) (hz->radians (jncos-frequency g)))
				     g))
  (frequency *clm-default-frequency*) (r 0.0) (a 1.0) (n 0 :type int) (angle 0.0))


(define (jncos gen fm)
  "  (make-jncos frequency (r 0.0) (a 1.0) (n 0)) creates a jncos generator.\n\
   (jncos gen fm) returns a sum of cosines scaled in a very complicated way."
  (declare (gen jncos) (fm float))
  (let* ((x (jncos-angle gen))
	 (a (jncos-a gen))
	 (r (jncos-r gen))
	 (n (jncos-n gen))
	 (arg (sqrt (+ (* r r) 
		       (* a a)
		       (* a r -2.0 (cos x))))))

    (set! (jncos-angle gen) (+ x fm (jncos-frequency gen)))

    (if (< arg nearly-zero)
	1.0
	(/ (bes-jn n arg)
	   (expt arg n)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jncos 100.0 :a 0.5 :r 1.0 :n 0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 41000))
	 (outa i (jncos gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------

;;; use J0(cos)+J1(cos) to get full spectrum

(defgenerator (j0j1cos
	       :make-wrapper (lambda (g)
			       (set! (j0j1cos-frequency g) (hz->radians (j0j1cos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (index 1.0) (angle 0.0))


(define (j0j1cos gen fm)
  "  (make-j0j1cos frequency (index 1.0)) creates a j0j1cos generator.\n\
   (j0j1cos gen fm) returns a sum of cosines scaled in a very complicated way."
  (declare (gen j0j1cos) (fm float))
  (let* ((x (j0j1cos-angle gen))
	 (z (j0j1cos-index gen))
	 (j0 (bes-j0 (* 0.5 z)))
	 (dc (* j0 j0))
	 (arg (* z (cos x))))

    (set! (j0j1cos-angle gen) (+ x fm (j0j1cos-frequency gen)))

    (/ (- (+ (bes-j0 arg)
	     (bes-j1 arg))
	  dc)        ; get rid of DC component
       1.215)))      ; not the best...

; need to normalize j0j1cos -- min depends on index, so peak depends on max and min and dc
;       (max (- 1.2154 dc)
;	    (- -0.5530 dc)

#|
(let ((mx 0.0) (x 0.0) (saved-x 0.0))
  (do ((i 0 (1+ i)))
      ((= i 1000))
    (let ((val (+ (bes-j0 x) (bes-j1 x))))
      (if (> (abs val) mx)
	  (begin
	    (set! mx (abs val))
	    (set! saved-x x)))
      (set! x (+ x .001))))
  (list mx saved-x))

(1.21533317877749 0.825000000000001)
(1.21533318495717 0.824863000002882)
(1.21533318495718 0.824863061409846)

(-0.552933995255066 4.57000000000269)
(-0.552933995483144 4.56997100028488)

(do ((i 0 (1+ i)))
    ((= i 10))
  (let ((pk (vct-peak 
	     (with-sound (:output (make-vct 10000))
  	       (let ((gen (make-j0j1cos 100.0 i)))
		 (run 
		  (lambda ()
		    (do ((i 0 (1+ i)))
			((= i 10000))
		      (outa i (j0j1cos gen 0.0))))))))))
    (snd-display ";~A: ~A" i pk)))
;0: 0.0
;1: 0.555559098720551
;2: 0.938335597515106
;3: 0.953315675258636
;4: 1.16509592533112
;5: 1.21275520324707
;6: 1.14727067947388
;7: 1.07083106040955
;8: 1.05760526657104
;9: 1.11238932609558
;10: 1.1824289560318
;11: 1.21528387069702
;12: 1.19094204902649
;13: 1.14720714092255
;14: 1.12512302398682
				  
|#

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0j1cos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (j0j1cos gen 0.0)))))))
|#

;;; --------------------------------------------------------------------------------


(defgenerator (jycos
	       :make-wrapper (lambda (g)
			       (set! (jycos-frequency g) (hz->radians (jycos-frequency g)))
			       (set! (jycos-r g) (max .0001 (jycos-r g))) ; 0->inf in bes-y0
			       (let ((a (jycos-a g)) ; "c"
				     (r (jycos-r g))); "b"
				 (if (<= r a)
				     (format #t ";jycos a: ~A must be < r: ~A" a r))
				 (if (<= (+ (* a a) (* r r)) (* 2 a r))
				     (format #t ";jycos a: ~A, r: ~A will cause bes-y0 to return -inf!" a r)))
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0))


(define (jycos gen fm)
  "  (make-jycos frequency (r 1.0) (a 0.5)) creates a jycos generator.\n\
   (jycos gen fm) returns a sum of cosines scaled by Yn(r)*Jn(r)."
  (declare (gen jycos) (fm float))
  (let* ((x (jycos-angle gen))
	 (b (jycos-r gen))
	 (c (jycos-a gen))
	 (b2c2 (+ (* b b) (* c c)))
	 (dc (* (bes-y0 b) (bes-j0 c)))
	 (norm (abs (- (bes-y0 (sqrt (+ b2c2 (* -2 b c)))) dc))))

    (set! (jycos-angle gen) (+ x fm (jycos-frequency gen)))

    (/ (- (bes-y0 (sqrt (+ b2c2 (* -2.0 b c (cos x)))))
	  dc)
       norm)))

;;; oops -- bes-y0(0) is -inf!
;;; norm only works for "reasonable" a and r

#|
(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-jycos 100.0 1.5 1.0))
	(af (make-env '(0 0 1 1) :length 30000))
	(rf (make-env '(0 3 1 3) :length 30000))
	(ampf (make-env '(0 0 1 1 10 1 11 0) :scaler 0.5 :length 30000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (set! (jycos-a gen) (env af))
	 (set! (jycos-r gen) (env rf))
	 (outa i (* (env ampf)
		    (jycos gen 0.0))))))))

:(* (bes-yn 1 1.5) (bes-jn 1 1.0))
-0.181436652807559
:(* (bes-yn 2 1.5) (bes-jn 2 1.0))
-0.107112311628537
:(* (bes-yn 3 1.5) (bes-jn 3 1.0))
-0.0405654243875417

:(/ .107 .181)
0.591160220994475  [0.600]
:(/ .040 .181)
0.220994475138122  [0.228]
|#

;;; --------------------------------------------------------------------------------

#|
(defgenerator (jcos
	       :make-wrapper (lambda (g)
			       (set! (jcos-frequency g) (hz->radians (jcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 0 :type int) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0))


(define (jcos gen fm)
  "  (make-jcos frequency (n 0) (r 1.0) (a 0.5)) creates a jcos generator.\n\
   (jcos gen fm) returns a sum of cosines scaled in some complex manner."
  (declare (gen jcos) (fm float))
  (let* ((x (jcos-angle gen))
	 (b (jcos-r gen))
	 (c (jcos-a gen))
	 (n (jcos-n gen))
	 (dc (* (bes-j0 b) (bes-j0 c))))

    (set! (jcos-angle gen) (+ x fm (jcos-frequency gen)))

    (- (bes-jn n (* (+ n 1) (sqrt (+ (* b b) (* c c) (* -2.0 b c (cos x))))))
       dc)))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jcos 100.0 0 1.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (jcos gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

#|
(defgenerator (sin2n
	       :make-wrapper (lambda (g)
			       (set! (sin2n-frequency g) (hz->radians (sin2n-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (n 1 :type int) (r 1.0) (angle 0.0))


(define (sin2n gen fm)
  "  (make-sin2n frequency (n 0) (r 1.0)) creates a sin2n generator.\n\
   (sin2n gen fm) returns (r*sin)^(2n)"
  (declare (gen sin2n) (fm float))
  (let* ((x (sin2n-angle gen))
	 (n (sin2n-n gen))
	 (r (sin2n-r gen)))
    (set! (sin2n-angle gen) (+ x fm (sin2n-frequency gen)))
    (expt (* r (sin x)) (* 2 n))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-sin2n 100.0 2 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (sin2n gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

#|
;;; do we need fmod 2*pi for the angles? (it is not used in clm.c)

:(let ((ph 0.0)) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) ph)
628.31850751536

:(let ((ph (* 2 pi 1000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000)))
628.318502381444

:(let ((ph (* 2 pi 1000000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000)))
628.311109542847

:(let ((ph (* 2 pi 1000000000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000000)))
624.462890625

;; similar results from running oscil with 0.0 initial-phase, and 2*pi*1000000000, or running one
;;   oscil for 3 hours at 6000 Hz -- the sinusoid is clean even around an angle of a billion -- worst 
;;   case increment is pi, so we get (say) a billion samples before we may notice a sag => ca. 8 hours.  
;;   I think that's a long enough tone...  (In clm.c and here, the phase and increment are both doubles;
;;   53 bits of mantissa, billion=30, so we still have about 23 bits, which actually matches results above).
|#


;;; --------------------------------------------------------------------------------

;;; blackman as a waveform -- all the other fft windows could be implemented
;;;   perhaps most useful as an amplitude envelope

(defgenerator (blackman
	       :make-wrapper (lambda (g)
			       (let ((n (blackman-n g)))
				 (set! n (min (max n 1) 10))
				 (set! (blackman-frequency g) (hz->radians (blackman-frequency g)))
				 (case n
				   ((1) (set! (blackman-coeffs g) (vct 0.54 -0.46)))
				   ((2) (set! (blackman-coeffs g) (vct 0.34401 -0.49755 0.15844)))
				   ((3) (set! (blackman-coeffs g) (vct 0.21747 -0.45325 0.28256 -0.04672)))
				   ((4) (set! (blackman-coeffs g) (vct 0.084037 -0.29145 0.375696 -0.20762 0.041194)))
				   ((5) (set! (blackman-coeffs g) (vct 0.097167 -0.3088448 0.3626224 -0.1889530 0.04020952 -0.0022008)))
				   ((6) (set! (blackman-coeffs g) (vct 0.063964353 -0.239938736 0.3501594961 -0.247740954 0.0854382589
								       -0.012320203 0.0004377882)))
				   ((7) (set! (blackman-coeffs g) (vct 0.04210723 -0.18207621 0.3177137375 -0.284437984 0.1367622316
								       -0.033403806 0.0034167722 -0.000081965)))
				   ((8) (set! (blackman-coeffs g) (vct 0.027614462 -0.135382235 0.2752871215 -0.298843294 0.1853193194
								       -0.064888448 0.0117641902 -0.000885987 0.0000148711)))
				   ((9) (set! (blackman-coeffs g) (vct 0.01799071953 -0.098795950 0.2298837751 -0.294112951 0.2243389785
								       -0.103248745 0.0275674108 -0.003839580	0.0002189716 -0.000002630)))
				   ((10) (set! (blackman-coeffs g) (vct 0.0118717384 -0.071953468 0.1878870875 -0.275808066 0.2489042133 
									-0.141729787 0.0502002984 -0.010458985 0.0011361511 -0.000049617
									0.0000004343))))
				 g))
		 :methods (list
			   (list 'mus-reset
				 (lambda (g)
				   (set! (blackman-angle g) 0.0)))))
    (frequency *clm-default-frequency*) (n 4 :type int) (coeffs #f :type vct) (angle 0.0))


(define (blackman gen fm)
  "  (make-blackman frequency (n 4)) creates a blackman generator.\n\
   (blackman gen fm) returns the nth Blackman-Harris fft data window as a periodic waveform. (n <= 10)"
  (declare (gen blackman) (fm float))
  (let ((x (blackman-angle gen)))
    (set! (blackman-angle gen) (+ x fm (blackman-frequency gen)))
    (polynomial (blackman-coeffs gen) (cos x))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((black4 (make-blackman 440.0)))
    (run (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (blackman black4 0.0)))))))
|#



;;; --------------------------------------------------------------------------------


;;; we can add the sin(cos) and sin(sin) cases, using -index in the latter to get 
;;;   asymmetric fm since Jn(-B) = (-1)^n Jn(B)
;;;
;;; the same trick would work in the other two cases -- gapped spectra

(defgenerator (fmssb
	       :make-wrapper (lambda (g)
			       (set! (fmssb-frequency g) (hz->radians (fmssb-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (index 1.0) (angle 0.0))


(define (fmssb gen fm)
  "  (make-fmssb frequency (ratio 1.0) (index 1.0)) creates an fmssb generator.\n\
   (fmssb gen fm) returns single-sideband FM."
  (declare (gen fmssb) (fm float))
  (let* ((cx (fmssb-angle gen))
	 (mx (* cx (fmssb-ratio gen)))
	 (B (fmssb-index gen)))

    (set! (fmssb-angle gen) (+ fm cx (fmssb-frequency gen)))

    (- (* (cos cx)
	  (sin (* B (cos mx))))
       (* (sin cx)
	  (* (sin (* B (sin mx)))))))) ; use -B for the other side

;;; FM with complex index
(define* (fpmc beg dur freq amp mc-ratio fm-index interp)
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (cr 0.0)
	 (cr-frequency (hz->radians freq))
	 (md-frequency (hz->radians (* freq mc-ratio)))
	 (md 0.0))
    (do ((i start (1+ i)))
	((= i end))
      (let ((val (sin (+ cr (* fm-index (sin md))))))
        (outa i (* amp (+ (* (- 1.0 interp) (real-part val))
                          (* interp (imag-part val)))))
        (set! cr (+ cr cr-frequency))
        (set! md (+ md md-frequency))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.1 :index 8.0)))  ; 1 3 7 11 ... -- interesting effect
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .3 (fmssb gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.1 :index 8.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :scaler 8)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.05 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 100.0 5.4 :index 1.0)) ; also 100 700
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000)) ; also 0 0 1 1 3 1 100 0...
	;; '(0 0 1 .75 2 1 3 .95 4 .5 10 0) -> bowed effect, '(0 0 1 .75 2 1 3 .125 4 .25 5 1 6 .8 20 0)
	;; '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) -> clickier attack (300 too)
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
        ;; '(0 0 1 1 3 0)
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 10.0 2.0 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 3 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0))))))))

(with-sound (:statistics #t :scaled-to .5 :play #t)
  (let ((gen1 (make-fmssb 500 1))
	(gen2 (make-fmssb 1000 .2))
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :length 30000))
	(indf (make-env '(0 1 1 1 10 0) :scaler 5.0 :base 32 :length 30000)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (let ((ind (env indf)))
	   (set! (fmssb-index gen1) ind)
	   (set! (fmssb-index gen2) ind)
	   (outa i (* (env ampf)
		      (+ (fmssb gen1 0.0)
			 (fmssb gen2 0.0))))))))))


;;; imaginary machines (also imaginary beasts)
|#

(definstrument (machine1 beg dur cfreq mfreq amp index gliss)
  (let* ((gen (make-fmssb cfreq (/ mfreq cfreq) :index 1.0))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) :base 32 :scaler amp :duration dur))
	 (indf (make-env '(0 0 1 1 3 0) :duration dur :base 32 :scaler index))
	 (frqf (make-env (if (> gliss 0.0) '(0 0 1 1) '(0 1 1 0)) :duration dur :scaler (hz->radians (* (/ cfreq mfreq) (abs gliss))))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen (env frqf)))))))))
#|

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .5)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 3.0 0.0)
    (machine1 i .1 100 1200 .5 10.0 200.0)
    (machine1 i .3 100 50 .75 10.0 0.0)
    (machine1 (+ i .1) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .36) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .4) .1 400 300 .5 10.0 -900.0)
    (machine1 (+ i .4) .21 100 50 .75 10.0 1000.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 4.0 0.0)
    (machine1 (+ i .1) .3 200 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 1000.0 2.0 0.875))
	(noi (make-rand 15000 .02))
	(gen1 (make-rkoddssb 100.0 0.1 0.9))
	(ampf (make-env '(0 0 1 1 11 1 12 0) :duration 11.0 :scaler .5))
	(frqf (make-env '(0 0 1 1 2 0 10 0 11 1 12 0 20 0) :duration 11.0 :scaler (hz->radians 10.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i (* 12 44100)))
	 (outa i (* (env ampf) 
		    (+ (rkoddssb gen1 (env frqf))
		       (* .2 (sin (rkoddssb gen (rand noi)))))))))))
  (do ((i 0.0 (+ i 2)))
      ((>= i 10.0))
    (machine1 i 3 100 700 0.5 4.0 0.0)
    (machine1 (+ i 1) 3 200 700 0.5 3.0 0.0))
  (do ((i 0.0 (+ i 6)))
      ((>= i 10.0))
    (machine1 i 3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i 1) 1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 1200 540 0.5 40.0 0.0)
    (machine1 (+ i .1) .3 2400 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 10.0 100.0)
    ))

;;; same as above but up octave
(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .1)))
      ((>= i 2.0))
    (machine1 i .15 2400 1080 0.25 40.0 0.0)
    (machine1 (+ i .05) .2 4800 1080 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .3)))
      ((>= i 2.0))
    (machine1 i .15 2000 1080 0.5 6.0 0.0)
    (machine1 (+ i .05) .1 4000 1080 0.5 10.0 100.0)
    ))
|#


(define (fm-cancellation beg dur frequency ratio amp index)
  (let* ((cx 0.0)
	 (mx 0.0)
	 (car-frequency (hz->radians frequency))
	 (mod-frequency (hz->radians ratio))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (do ((i start (1+ i)))
	((= i stop))
      (outa i (* amp (- (* (cos cx)
			   (sin (* index (cos mx))))
			(* (sin cx)
			   (* (sin (* index (sin mx))))))) 
	                   ;; use -index for reflection
	   )
      (set! cx (+ cx car-frequency))
      (set! mx (+ mx mod-frequency)))))

;(with-sound () (fm-cancellation 0 1 1000.0 100.0 0.3 9.0))


;;; --------------------------------------------------------------------------------

;;; k3sin

;;; mostly useful as a test of a vct field 

(defgenerator (k3sin
	       :make-wrapper (lambda (g)
			       (set! (k3sin-frequency g) (hz->radians (k3sin-frequency g)))
			       (set! (k3sin-coeffs g) (vct 0.0
							   (/ (* pi pi) 6.0)
							   (/ pi -4.0)
							   (/ 1.0 12.0)))
			       g)
	       :methods (list
			 (list 'mus-reset
			       (lambda (g)
				 (set! (k3sin-frequency g) 0.0)
				 (set! (k3sin-angle g) 0.0)))))
  (frequency *clm-default-frequency*) (angle 0.0) (coeffs #f :type vct))
		   

(define (k3sin gen fm)
  "  (make-k3sin frequency) creates a k3sin generator.\n\
   (k3sin gen fm) returns a sum of sines scaled by k^3."
  (declare (gen k3sin) (fm float))
  (let ((x (k3sin-angle gen)))
    (set! (k3sin-angle gen) (fmod (+ x fm (k3sin-frequency gen)) (* 2 pi)))
    (polynomial (k3sin-coeffs gen) x)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k3sin 100.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (k3sin gen 0.0)))))))
|#


;;; --------------------------------------------------------------------------------

;;; I(z) case A&S

(defgenerator (izcos
	       :make-wrapper (lambda (g)
			       (set! (izcos-frequency g) (hz->radians (izcos-frequency g)))
			       (set! (izcos-dc g) (bes-i0 (izcos-r g)))
			       (set! (izcos-norm g) (- (exp (izcos-r g)) (izcos-dc g)))
			       g)
	       :methods (list
			 (list 'mus-scaler
			       (lambda (g)
				 (izcos-r g))
			       (lambda (g val)
				 (set! (izcos-r g) val)
				 (set! (izcos-dc g) (bes-i0 val))
				 (set! (izcos-norm g) (- (exp val) (izcos-dc g)))
				 val))))
  (frequency *clm-default-frequency*) (r 1.0) (angle 0.0)
  (dc 0.0) (norm 1.0))


(define (izcos gen fm)
  "  (make-izcos frequency (r 1.0)) creates an izcos generator.\n\
   (izcos gen fm) returns a sum of sines scaled by In(r)."
  (declare (gen izcos) (fm float))
  (let* ((x (izcos-angle gen))
	 (z (izcos-r gen))
	 (dc (izcos-dc gen))
	 (norm (izcos-norm gen)))

    (set! (izcos-angle gen) (+ x fm (izcos-frequency gen)))

    (if (< (abs norm) nearly-zero)
	1.0
	(/ (- (exp (* z (cos x)))
	      dc)
	   norm))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-izcos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (izcos gen 0.0)))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-izcos 100.0 1.0))
	(indf (make-env '(0 0 1 3) :length 30000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (set! (mus-scaler gen) (env indf))
	 (outa i (izcos gen 0.0)))))))

|#



;;; --------------------------------------------------------------------------------

(definstrument (organish beg dur freq amp fm-index amp-env)
  ;; this has an organ-style chiff (better than fm index sweep)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (carriers (make-vector 3 #f))
	 (fmoscs (make-vector 3 #f))
	 (ampfs (make-vector 3 #f))
	 (pervib (make-triangle-wave 5 (hz->radians (* freq .003))))
	 (ranvib (make-rand-interp 6 (hz->radians (* freq .002))))
	 (resc (make-nrssb 340.0 1.0 5 .5))
	 (resf (make-env (list 0 0 .05 1  .1 0 dur 0) :scaler (* amp .05) :duration dur)))

    (do ((i 0 (1+ i)))
	((= i 3))
      (let* ((frq (* freq (expt 2 i)))
	     (index1 (hz->radians (* fm-index frq (/ 5.0 (log frq)))))
	     (index2 (hz->radians (* fm-index frq 3.0 (/ (- 8.5 (log frq)) (+ 3.0 (* frq .001))))))
	     (index3 (hz->radians (* fm-index frq (/ 4.0 (sqrt frq))))))
	(vector-set! carriers i (make-oscil frq))
	(vector-set! fmoscs i (make-polyshape :frequency frq
					      :coeffs (partials->polynomial 
						       (list 1 index1
							     3 index2
							     4 index3))))))

    (vector-set! ampfs 0 (make-env (or amp-env '(0 0 1 1 2 1 3 0)) :scaler amp :duration dur))
    (vector-set! ampfs 1 (make-env (list 0 0  .04 1  .075 0 dur 0) :scaler (* amp .0125) :duration dur))
    (vector-set! ampfs 2 (make-env (list 0 0  .02 1  .05 0 dur 0) :scaler (* amp .025) :duration dur))

    ;; also good:
    ;(vector-set! ampfs 1 (make-env (list 0 0  .02 1  .05 0  (- dur .1) 0  (- dur .05) 1 dur 0) :scaler (* amp .025) :duration dur))
    ;(vector-set! ampfs 2 (make-env (list 0 0  .01 1 .025 0  (- dur .15) 0 (- dur .1) 1 dur 0) :scaler (* amp .05) :duration dur))

    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((vib (+ (triangle-wave pervib) (rand-interp ranvib)))
		(sum (* (env resf)
			(nrssb resc 0.0))))
	   (do ((k 0 (1+ k))
		(n 1 (* n 2)))
	       ((= k 3))
	     (set! sum (+ sum (* (env (vector-ref ampfs k))
				 (oscil (vector-ref carriers k)
					(+ (* n vib)
					   (polyshape (vector-ref fmoscs k) 1.0 (* n vib))))))))
	   (outa i sum)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 #f)))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 '(0 0 1 1 2 .5 3 .25 4 .125 10 0))))
|#



;;; --------------------------------------------------------------------------------

(defgenerator plsenv (pulse #f :type clm) (ampf #f :type clm))

(def-optkey-fun (make-pulsed-env envelope duration frequency)
  (make-plsenv (make-pulse-train frequency)
	       (make-env envelope :duration duration)))

(define (pulsed-env gen fm)
  (if (> (pulse-train (plsenv-pulse gen) fm) 0.1)
      (mus-reset (plsenv-ampf gen)))
  (env (plsenv-ampf gen)))

(define pulsed-env? plsenv?)


;;; --------------------------------------------------------------------------------

(defgenerator (adjustable-square-wave 
	       :make-wrapper 
	       (lambda (g)
		 (set! (adjustable-square-wave-p1 g) (make-pulse-train 
						      (adjustable-square-wave-frequency g) 
						      (adjustable-square-wave-amplitude g)))
		 (set! (adjustable-square-wave-p2 g) (make-pulse-train 
						      (adjustable-square-wave-frequency g) 
						      (- (adjustable-square-wave-amplitude g))
						      (* 2.0 pi (- 1.0 (adjustable-square-wave-duty-factor g)))))
		 g)

	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (adjustable-square-wave-p1 g)))
			       (lambda (g val) (set! (mus-frequency (adjustable-square-wave-p1 g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (adjustable-square-wave-p1 g)))
			       (lambda (g val) (set! (mus-phase (adjustable-square-wave-p1 g)) val) val))

			 (list 'mus-scaler
			       (lambda (g) 
				 (adjustable-square-wave-duty-factor g))
			       (lambda (g val)
				 (set! (adjustable-square-wave-duty-factor g) val)
				 (set! (mus-phase (adjustable-square-wave-p2 g)) (* 2.0 pi (- 1.0 (adjustable-square-wave-duty-factor g))))
				 val))))

  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0)
  (sum 0.0) (p1 #f :type clm) (p2 #f :type clm))


(define (adjustable-square-wave gen fm)
  "  (make-adjustable-square-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-square-wave generator.\n\
   (adjustable-square-wave gen fm) returns a square-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  (declare (gen adjustable-square-wave) (fm float))
  (set! (adjustable-square-wave-sum gen) (+ (adjustable-square-wave-sum gen)
					    (pulse-train (adjustable-square-wave-p1 gen) fm)
					    (pulse-train (adjustable-square-wave-p2 gen) fm)))
  (adjustable-square-wave-sum gen))

#|
(with-sound ()
  (let ((gen (make-adjustable-square-wave 100 .2 .5)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 22050))
	 (outa i (adjustable-square-wave gen 0.0)))))))
|#


(defgenerator (adjustable-triangle-wave 
	       :make-wrapper 
	       (lambda (g)
		 (let ((df (adjustable-triangle-wave-duty-factor g)))
		   (set! (adjustable-triangle-wave-gen g) (make-triangle-wave (adjustable-triangle-wave-frequency g)))
		   (set! (adjustable-triangle-wave-top g) (- 1.0 df))
		   (if (not (= df 0.0))
		       (set! (adjustable-triangle-wave-scl g) (/ (adjustable-triangle-wave-amplitude g) df)))
		   g))

	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (adjustable-triangle-wave-gen g)))
			       (lambda (g val) (set! (mus-frequency (adjustable-triangle-wave-gen g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (adjustable-triangle-wave-gen g)))
			       (lambda (g val) (set! (mus-phase (adjustable-triangle-wave-gen g)) val) val))

			 (list 'mus-scaler
			       (lambda (g) 
				 (adjustable-triangle-wave-duty-factor g))
			       (lambda (g val)
				 (set! (adjustable-triangle-wave-duty-factor g) val)
				 (set! (adjustable-triangle-wave-top g) (- 1.0 val))
				 (if (not (= val 0.0))
				     (set! (adjustable-triangle-wave-scl g) (/ (adjustable-triangle-wave-amplitude g) val)))
				 val))))

  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0) 
  (gen #f :type clm) (top 0.0) (scl 0.0))


(define (adjustable-triangle-wave gen fm)
  "  (make-adjustable-triangle-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-triangle-wave generator.\n\
   (adjustable-triangle-wave gen fm) returns a triangle-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  (declare (gen adjustable-triangle-wave) (fm float))
  (let* ((val (triangle-wave (adjustable-triangle-wave-gen gen) fm))
	 (top (adjustable-triangle-wave-top gen))
	 (scl (adjustable-triangle-wave-scl gen)))
    (* scl (- val (max (- top) (min top val))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-triangle-wave 100 .2 .5)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 22050))
	 (outa i (adjustable-triangle-wave gen 0.0)))))))
|#


(defgenerator (adjustable-sawtooth-wave 
	       :make-wrapper 
	       (lambda (g)
		 (let ((df (adjustable-sawtooth-wave-duty-factor g)))
		   (set! (adjustable-sawtooth-wave-gen g) (make-sawtooth-wave (adjustable-sawtooth-wave-frequency g)))
		   (set! (adjustable-sawtooth-wave-top g) (- 1.0 df))
		   (if (not (= df 0.0))
		       (set! (adjustable-sawtooth-wave-scl g) (/ (adjustable-sawtooth-wave-amplitude g) df)))
		   g))

	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (adjustable-sawtooth-wave-gen g)))
			       (lambda (g val) (set! (mus-frequency (adjustable-sawtooth-wave-gen g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (adjustable-sawtooth-wave-gen g)))
			       (lambda (g val) (set! (mus-phase (adjustable-sawtooth-wave-gen g)) val) val))

			 (list 'mus-scaler
			       (lambda (g) 
				 (adjustable-sawtooth-wave-duty-factor g))
			       (lambda (g val)
				 (set! (adjustable-sawtooth-wave-duty-factor g) val)
				 (set! (adjustable-sawtooth-wave-top g) (- 1.0 val))
				 (if (not (= val 0.0))
				     (set! (adjustable-sawtooth-wave-scl g) (/ (adjustable-sawtooth-wave-amplitude g) val)))
				 val))))

  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0) 
  (gen #f :type clm) (top 0.0) (scl 0.0))


(define (adjustable-sawtooth-wave gen fm)
  "  (make-adjustable-sawtooth-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-sawtooth-wave generator.\n\
   (adjustable-sawtooth-wave gen fm) returns a sawtooth-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  (declare (gen adjustable-sawtooth-wave) (fm float))
  (let* ((val (sawtooth-wave (adjustable-sawtooth-wave-gen gen) fm))
	 (top (adjustable-sawtooth-wave-top gen))
	 (scl (adjustable-sawtooth-wave-scl gen)))
    (* scl (- val (max (- top) (min top val))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-sawtooth-wave 100 .2 .5)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 22050))
	 (outa i (adjustable-sawtooth-wave gen 0.0)))))))
|#


;;; and just for laughs... (almost anything would fit in this hack)
(defgenerator (adjustable-oscil 
	       :make-wrapper (lambda (g)
			       (let ((df (adjustable-oscil-duty-factor g)))
				 (set! (adjustable-oscil-gen g) (make-oscil (adjustable-oscil-frequency g)))
				 (set! (adjustable-oscil-top g) (- 1.0 df))
				 (if (not (= df 0.0))
				     (set! (adjustable-oscil-scl g) (/ 1.0 df)))
				 g))

	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (adjustable-oscil-gen g)))
			       (lambda (g val) (set! (mus-frequency (adjustable-oscil-gen g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (adjustable-oscil-gen g)))
			       (lambda (g val) (set! (mus-phase (adjustable-oscil-gen g)) val) val))

			 (list 'mus-scaler
			       (lambda (g) 
				 (adjustable-oscil-duty-factor g))
			       (lambda (g val)
				 (set! (adjustable-oscil-duty-factor g) val)
				 (set! (adjustable-oscil-top g) (- 1.0 val))
				 (if (not (= val 0.0))
				     (set! (adjustable-oscil-scl g) (/ 1.0 val)))
				 val))))

  (frequency *clm-default-frequency*) (duty-factor 0.5)
  (gen #f :type clm) (top 0.0) (scl 0.0))


(define (adjustable-oscil gen fm)
  "  (make-adjustable-oscil frequency (duty-factor 0.5)) creates an adjustable-oscil generator.\n\
   (adjustable-oscil gen fm) returns a sinusoid where the duty-factor sets the ratio of pulse duration to pulse period."
  (declare (gen adjustable-oscil) (fm float))
  (let* ((val (oscil (adjustable-oscil-gen gen) fm))
	 (top (adjustable-oscil-top gen))
	 (scl (adjustable-oscil-scl gen)))
    (* scl (- val (max (- top) (min top val))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-oscil 100 .2)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 22050))
	 (outa i (adjustable-oscil gen 0.0)))))))
|#

;;;--------------------------------------------------------------------------------

(def-optkey-fun (make-table-lookup-with-env frequency pulse-env size)
  (let* ((len (or size (clm-table-size)))
	 (ve (make-vct len))
	 (e (make-env pulse-env :length len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vct-set! ve i (env e)))
    (make-table-lookup frequency 0.0 ve len)))

(def-optkey-fun (make-wave-train-with-env frequency pulse-env size)
  (let* ((len (or size (clm-table-size)))
	 (ve (make-vct len))
	 (e (make-env pulse-env :length len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vct-set! ve i (env e)))
    (make-wave-train frequency 0.0 ve len)))



;;; --------------------------------------------------------------------------------

(defgenerator (round-interp 
	       :make-wrapper (lambda (g)
			       (set! (round-interp-rnd g) (make-rand-interp (round-interp-frequency g) (round-interp-amplitude g)))
			       (set! (round-interp-flt g) (make-moving-average (round-interp-n g)))
			       g)

	       :methods (list
			 (list 'mus-frequency
			       (lambda (g) (mus-frequency (round-interp-rnd g)))
			       (lambda (g val) (set! (mus-frequency (round-interp-rnd g)) val) val))

			 (list 'mus-phase
			       (lambda (g) (mus-phase (round-interp-rnd g)))
			       (lambda (g val) (set! (mus-phase (round-interp-rnd g)) val) val))))

  (frequency *clm-default-frequency*) (n 1) (amplitude 1.0)
  (rnd #f :type clm) (flt #f :type clm))


(define (round-interp gen fm)
  "  (make-round-interp frequency (n 1) (amplitude 1.0)) creates a round-interp generator.\n\
   (round-interp gen fm) returns a rand-interp sequence low-pass filtered by a moving-average generator of length n."
  (declare (gen round-interp) (fm float))
  (moving-average (round-interp-flt gen) (rand-interp (round-interp-rnd gen) fm)))

#|
(with-sound (:channels 5)
  (let ((gen0 (make-round-interp 100 1))
	(gen1 (make-round-interp 100 10))
	(gen2 (make-round-interp 100 100))
	(gen3 (make-round-interp 100 1000))
	(gen4 (make-round-interp 100 10000)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000))
	 (out-any i (round-interp gen0 0.0) 0)
	 (out-any i (round-interp gen1 0.0) 1)
	 (out-any i (round-interp gen2 0.0) 2)
	 (out-any i (round-interp gen3 0.0) 3)
	 (out-any i (round-interp gen4 0.0) 4))))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; env-any functions

(define (sine-env e)
  (env-any e (lambda (y)
	       (* 0.5 (+ 1.0 (sin (+ (* -0.5 pi) 
				     (* pi y))))))))

(define (square-env e)
  (env-any e (lambda (y)
	       (* y y))))

(define (blackman4-env e)
  (env-any e
	   (lambda (y)
	     (let ((cx (cos (* pi y))))
	       (+ 0.084037 (* cx (+ -.29145 (* cx (+ .375696 (* cx (+ -.20762 (* cx .041194))))))))))))

(define (multi-expt-env e expts)
  (env-any e (lambda (y)
	       (let ((b (vct-ref expts (modulo (mus-channels e) (vct-length expts)))))
		 (/ (- (expt b y) 1.0) (- b 1.0))))))


;;; --------------------------------------------------------------------------------
;;;
;;; pm with any generator that has mus-phase and mus-run:

(define (run-with-fm-and-pm gen fm pm)
  (set! (mus-phase gen) (+ (mus-phase gen) pm))
  (let ((result (mus-run gen fm 0.0)))
    (set! (mus-phase gen) (- (mus-phase gen) pm))
    result))

#|
(let ((gen1 (make-oscil 440.0))
      (gen2 (make-oscil 440.0)))
  (do ((i 0 (1+ i)))
      ((= i 1000))
    (let* ((pm (- 1.0 (random 2.0)))
	   (val1 (oscil gen1 0.0 pm))
	   (val2 (run-with-fm-and-pm gen2 0.0 pm)))
      (if (fneq val1 val2)
	  (snd-display ";run-with-fm-and-pm: ~A ~A" val1 val2)))))
|#


;;; --------------------------------------------------------------------------------

;;; cos^n J 121

(defgenerator (nchoosekcos
	       :make-wrapper (lambda (g)
			       (set! (nchoosekcos-frequency g) (hz->radians (nchoosekcos-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0))

(define (nchoosekcos gen fm)
  "  (make-nchoosekcos frequency (ratio 1.0) (n 1)) creates an nchoosekcos generator.\n\
   (nchoosekcos gen fm) returns a sum of cosines scaled by the binomial coeffcients."
  (declare (gen nchoosekcos) (fm float))
  (let* ((x (nchoosekcos-angle gen))
	 (y (* x (nchoosekcos-ratio gen)))
	 (n (nchoosekcos-n gen)))

    (set! (nchoosekcos-angle gen) (+ x fm (nchoosekcos-frequency gen)))

    (* (cos x)
       (expt (cos y) n))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nchoosekcos 2000.0 0.05 10)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nchoosekcos gen 0.0)))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; sinc-train

(defgenerator (sinc-train 
	       :make-wrapper (lambda (g)
			       (if (<= (sinc-train-n g) 0)
				   (begin
				     (set! (sinc-train-original-n g) 1)
				     (set! (sinc-train-n g) 3))
				   (begin
				     (set! (sinc-train-original-n g) (sinc-train-n g))
				     (set! (sinc-train-n g) (+ 1 (* 2 (sinc-train-n g)))))) ; mimic ncos
			       (set! (sinc-train-original-frequency g) (sinc-train-frequency g))
			       (set! (sinc-train-frequency g) (* 0.5 (sinc-train-n g) (hz->radians (sinc-train-frequency g))))
			       g)

	       :methods (list
			 (list 'mus-order
			       (lambda (g) (sinc-train-original-n g))
			       (lambda (g val)
				 (if (<= val 0)
				     (begin
				       (set! (sinc-train-original-n g) 1)
				       (set! (sinc-train-n g) 3))
				     (begin
				       (set! (sinc-train-original-n g) val)
				       (set! (sinc-train-n g) (+ 1 (* 2 val)))))
				 (set! (sinc-train-frequency g) (* 0.5 (sinc-train-n g) (hz->radians (sinc-train-original-frequency g))))
				 (sinc-train-original-n g)))

			 (list 'mus-frequency
			       (lambda (g) (sinc-train-original-frequency g))
			       (lambda (g val)
				 (set! (sinc-train-original-frequency g) val)
				 (set! (sinc-train-frequency g) (* 0.5 (sinc-train-n g) (hz->radians val)))
				 val))))

  (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0)
  (original-n 1 :type int) (original-frequency 0.0))

	
(define (sinc-train gen fm)
  "  (make-sinc-train frequency (n 1)) creates a sinc-train generator with n components.\n\
   (sinc-train gen fm) returns a sinc-train"
  (declare (gen sinc-train) (fm float))
  (let* ((x (sinc-train-angle gen))
	 (n (sinc-train-n gen))
	 (max-angle (* pi 0.5 n))
	 (new-angle (+ x fm (sinc-train-frequency gen)))
	 (DC (/ 1.0 n))
	 (norm (/ n (1- n))))
    
    (if (> new-angle max-angle)
	(set! new-angle (- new-angle (* pi n))))

    (set! (sinc-train-angle gen) new-angle)

    (if (< (abs x) nearly-zero)
	1.0
	(* norm (- (/ (sin x) x) DC)))))


#|
(with-sound (:clipped #f :statistics #t)
  (let* ((g (make-sinc-train 100.0 40)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (sinc-train g 0.0)))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; pink-noise (based on rand-bank idea of Orfanidis)

(defgenerator (pink-noise
	       :make-wrapper (lambda (g)
			       (if (<= (pink-noise-n g) 0) (set! (pink-noise-n g) 1))
			       (let ((n (pink-noise-n g)))
				 (set! (pink-noise-rands g) (make-vector n))
				 (do ((i 0 (1+ i)))
				     ((= i n))
				   (vector-set! (pink-noise-rands g) i (make-rand :frequency (/ (mus-srate) (expt 2 i))))
				   (set! (mus-phase (vector-ref (pink-noise-rands g) i)) (random pi))))
			       g))
  (n 1) (rands #f :type clm-vector))


(define (pink-noise gen)
  "  (make-pink-noise (n 1)) creates a pink-noise generator with n octaves of rand (12 is recommended).\n\
  (pink-noise gen) returns the next random value in the 1/f stream produced by gen."
  (declare (gen pink-noise))
  (let ((val 0.0) 
	(rands (pink-noise-rands gen))
	(n (pink-noise-n gen)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (set! val (+ val (rand (vector-ref rands i)))))
    (/ val (* 2.5 (sqrt n))))) ; this normalization is not quite right


#|
(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-pink-noise 12)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (pink-noise gen)))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; brown-noise

(defgenerator (brown-noise
	       :make-wrapper (lambda (g)
			       (set! (brown-noise-frequency g) (hz->radians (brown-noise-frequency g)))
			       (set! (brown-noise-sum g) (mus-random (brown-noise-amplitude g)))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) (angle 0.0) (sum 0.0))


(define (brown-noise gen fm)
  "  (make-brown-noise frequency (amplitude 1.0)) returns a generator that produces brownian noise.\n\
  (brown-noise gen fm) returns the next brownian noise sample."
  (declare (gen brown-noise) (fm float))
  (let ((x (brown-noise-angle gen))
	(two-pi (* 2 pi)))
    (if (or (>= x two-pi)
	    (< x 0.0))
	(begin
	  (set! x (fmod x two-pi))
	  (if (< x 0.0) (set! x (+ x two-pi)))
	  (set! (brown-noise-sum gen) (+ (brown-noise-sum gen) (mus-random (brown-noise-amplitude gen))))))
    (set! (brown-noise-angle gen) (+ x fm (brown-noise-frequency gen)))
    (brown-noise-sum gen)))
  
#|
(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-brown-noise 1000)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (brown-noise gen 0.0)))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; green-noise

(defgenerator (green-noise
	       :make-wrapper (lambda (g)
			       (set! (green-noise-frequency g) (hz->radians (green-noise-frequency g)))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) (low -1.0) (high 1.0)
  (angle 0.0) (sum 0.0))


(define (green-noise gen fm)
  "(make-green-noise frequency (amplitude 1.0) (low -1.0) (high 1.0)) returns a new green-noise (bounded brownian noise) generator.\n\
   (green-noise gen fm) returns the next sample in a sequence of bounded brownian noise samples."
  (declare (gen green-noise) (fm float))
  (let* ((x (green-noise-angle gen))
	 (two-pi (* 2 pi)))
    (if (or (>= x two-pi)
	    (< x 0.0))
	(begin
	  (set! x (fmod x two-pi))
	  (if (< x 0.0) (set! x (+ x two-pi)))
	  (let ((val (mus-random (green-noise-amplitude gen))))
	    (set! (green-noise-sum gen) (+ (green-noise-sum gen) val))
	    (if (not (<= (green-noise-low gen) (green-noise-sum gen) (green-noise-high gen)))
		(set! (green-noise-sum gen) (- (green-noise-sum gen) (* 2 val)))))))
    (set! (green-noise-angle gen) (+ x fm (green-noise-frequency gen)))
    (green-noise-sum gen)))

#|
(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-green-noise 1000)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (green-noise gen 0.0)))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; green-noise-interp

(defgenerator (green-noise-interp
	       :make-wrapper (lambda (g)
			       (set! (green-noise-interp-frequency g) (hz->radians (green-noise-interp-frequency g)))
			       (set! (green-noise-interp-incr g) (/ (* (mus-random (green-noise-interp-amplitude g))
								       (green-noise-interp-frequency g))
								    (* 2 pi)))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) (low -1.0) (high 1.0)
  (angle 0.0) (sum 0.0) (incr 0.0))


(define (green-noise-interp gen fm)
  "(make-green-noise-interp frequency (amplitude 1.0) (low -1.0) (high 1.0)) returns a new interpolating green noise (bounded brownian noise) generator.\n\
   (green-noise-interp gen fm) returns the next sample in a sequence of interpolated bounded brownian noise samples."
  (declare (gen green-noise-interp) (fm float))
  (let* ((x (green-noise-interp-angle gen))
	 (two-pi (* 2 pi)))
    (if (or (>= x two-pi)
	    (< x 0.0))
	(begin
	  (set! x (fmod x two-pi))
	  (if (< x 0.0) (set! x (+ x two-pi)))
	  (let* ((val (mus-random (green-noise-interp-amplitude gen)))
		 (end (+ (green-noise-interp-sum gen) val)))
	    (if (not (<= (green-noise-interp-low gen) end (green-noise-interp-high gen)))
		(set! val (* -2 val)))
	    (set! (green-noise-interp-incr gen) (/ (* val (green-noise-interp-frequency gen))
						   (* 2 pi))))))
    (set! (green-noise-interp-angle gen) (+ x fm (green-noise-interp-frequency gen)))
    (set! (green-noise-interp-sum gen) (+ (green-noise-interp-sum gen) (green-noise-interp-incr gen)))
    (green-noise-interp-sum gen)))

#|
(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-green-noise-interp 1000)))
    (do ((i 0 (1+ i)))
	((= i 44100))
      (outa i (green-noise-interp gen 0.0)))))


(definstrument (green1 beg end freq amp lo hi)
  (let ((grn (make-green-noise :frequency freq :amplitude amp :high hi :low lo)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (green-noise grn 0.0)))))))

(definstrument (green2 beg end freq amp lo hi)
  (let ((grn (make-green-noise-interp :frequency freq :amplitude amp :high hi :low lo)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (green-noise-interp grn 0.0)))))))

(with-sound () (green1 0 10000 1000 0.1 -0.5 0.5) (green2 10000 20000 1000 0.1 -0.5 0.5))

(definstrument (green3 start dur freq amp amp-env noise-freq noise-width noise-max-step)
  ;; brownian noise on amp env
  (let* ((grn (make-green-noise-interp :frequency noise-freq :amplitude noise-max-step :high (* 0.5 noise-width) :low (* -0.5 noise-width)))
	 (osc (make-oscil freq))
	 (e (make-env amp-env :scaler amp :duration dur))
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* (env e) 
		    (+ 1.0 (green-noise-interp grn 0.0))
		    (oscil osc))))))))

(with-sound () (green3 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 100 .2 .02))


(definstrument (green4 start dur freq amp freq-env gliss noise-freq noise-width noise-max-step)
  ;; same but on freq env
  (let* ((grn (make-green-noise-interp :frequency noise-freq :amplitude noise-max-step :high (* 0.5 noise-width) :low (* -0.5 noise-width)))
	 (osc (make-oscil freq))
	 (e (make-env freq-env :scaler gliss :duration dur))
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* amp (oscil osc (hz->radians (+ (env e) (green-noise-interp grn 0.0)))))))))))

(with-sound () (green4 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 440 100 100 10))

|#



;;; --------------------------------------------------------------------------------
;;;
;;; moving-max

(defgenerator (moving-max
	       :make-wrapper (lambda (g)
			       (let ((dly (make-delay (moving-max-n g))))
				 (set! (moving-max-gen g) dly)
				 (set! (mus-scaler dly) 0.0)
				 g))
	       :methods (list
			 (list 'mus-data
			       (lambda (g) (mus-data (moving-max-gen g))))
			 (list 'mus-scaler
			       (lambda (g) (mus-scaler (moving-max-gen g)))
			       (lambda (g val)
				 (set! (mus-scaler (moving-max-gen g)) val)
				 val))))
  (n 128 :type int) (gen #f :type clm))


(define (moving-max gen y)
  "(make-moving-max (n 128) returns a moving-max generator.\n\
  (moving-max gen input) returns the maxamp in a moving window over the last n inputs."
  (declare (gen moving-max) (y float))
  (let* ((absy (abs y))
	 (dly (moving-max-gen gen))
	 (mx (delay dly absy)))
    (if (>= absy (mus-scaler dly))
	(set! (mus-scaler dly) absy)
	(if (>= mx (mus-scaler dly))
	    (set! (mus-scaler dly) (vct-peak (mus-data dly)))))
    (mus-scaler dly)))



;;; --------------------------------------------------------------------------------
;;;
;;; moving-sum

(defgenerator (moving-sum
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (moving-sum-n g))))
				 (set! (moving-sum-gen g) dly)
				 (set! (mus-increment dly) 1.0) ; this is 1/n by default
				 g)))
  (n 128 :type int) (gen #f :type clm))


(define (moving-sum gen y)
  "  (make-moving-sum (n 128) returns a moving-sum generator.\n\
  (moving-sum gen input) returns the sum of the absolute values in a moving window over the last n inputs."
  (declare (gen moving-sum) (y float))
  (moving-average (moving-sum-gen gen) (abs y)))
			       


;;; --------------------------------------------------------------------------------
;;;
;;; moving-variance
;;;
;;; this taken from the dsp bboard -- untested!

(defgenerator (moving-variance
	       :make-wrapper (lambda (g)
			       (let ((g1 (make-moving-average (moving-variance-n g)))
				     (g2 (make-moving-average (moving-variance-n g))))
				 (set! (moving-variance-gen1 g) g1)
				 (set! (mus-increment g1) 1.0)
				 (set! (moving-variance-gen2 g) g2)
				 (set! (mus-increment g2) 1.0)
				 g)))
  (n 128 :type int) (gen1 #f :type clm) (gen2 #f :type clm))


(define (moving-variance gen y)
  (declare (gen moving-variance) (y float))
  (let* ((x1 (moving-average (moving-variance-gen1 gen) y))
	 (x2 (moving-average (moving-variance-gen2 gen) (* y y)))
	 (n (moving-variance-n gen)))
    (/ (- (* n x2)
	  (* x1 x1))
       (* n (- n 1)))))

#|
(with-sound (:clipped #f)
  (let* ((gen (make-moving-variance 128))) 
    (do ((i 0 (1+ i))) 
	((= i 10000)) 
      (outa i (moving-variance gen (random 1.0))))))
|#


;;; similarly (moving-inner-product x y) -> (moving-sum (* x y)), 
;;;           (moving-distance x y) -> (sqrt (moving-sum (* (- x y) (- x y))))



;;; --------------------------------------------------------------------------------
;;;
;;; moving-rms

(defgenerator (moving-rms
	       :make-wrapper (lambda (g)
			       (set! (moving-rms-gen g) (make-moving-average (moving-rms-n g)))
			       g))
  (n 128 :type int) (gen #f :type clm))


(define (moving-rms gen y)
  "  (make-moving-rms (n 128) returns a moving-rms generator.\n\
  (moving-rms gen input) returns the rms of the values in a window over the last n inputs."
  (declare (gen moving-rms) (y float))
  (sqrt (max 0.0 ;; this is tricky -- due to floating point inaccuracy, we can get negative output
  		 ;;   from moving-rms even if all the inputs are positive!  The sqrt then returns
                 ;;   a complex number and all hell breaks loose (in run, you'll get a complaint
                 ;;   about an integer > 32 bits).
	     (moving-average (moving-rms-gen gen) (* y y)))))



;;; --------------------------------------------------------------------------------
;;;
;;; moving-length

(defgenerator (moving-length
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (moving-length-n g))))
				 (set! (moving-length-gen g) dly)
				 (set! (mus-increment dly) 1.0)
				 g)))
  (n 128 :type int) (gen #f :type clm))


(define (moving-length gen y)
  "  (make-moving-length (n 128) returns a moving-length generator.\n\
  (moving-length gen input) returns the length of the values in a window over the last few inputs."
  (declare (gen moving-length) (y float))
  (sqrt (max 0.0 (moving-average (moving-length-gen gen) (* y y)))))


#|
;; perhaps also use moving-rms gen to avoid amplifying noise-sections (or even squlech them)
(define* (agc :optional (ramp-speed .001) (window-size 512))
  (let ((maxer (make-moving-max window-size))
	(mult 1.0))
    (map-channel
     (lambda (y)
       (let* ((curmax (moving-max maxer y))
	      (diff (- 0.5 (* mult curmax)))
	      (this-incr (* diff ramp-speed)))
	 (set! mult (+ mult this-incr))
	 (* y mult))))))

;;; moving-mean = average
|#



;;; --------------------------------------------------------------------------------
;;;
;;; weighted-moving-average
;;;
;;; arithmetic (1/n) weights

(defgenerator (weighted-moving-average
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (weighted-moving-average-n g))))
				 (set! (mus-increment dly) 1.0)
				 (set! (weighted-moving-average-gen g) dly)
				 g)))
  (n 128 :type int) (gen #f :type clm) (num 0.0) (sum 0.0))


(define (weighted-moving-average gen y)
  "  (make-weighted-moving-average (n 128) returns a weighted-moving-average generator.\n\
  (weighted-moving-average gen y) returns the sum of the last n inputs weighted by 1/n"
  (declare (gen weighted-moving-average) (y float))
  (let* ((dly (weighted-moving-average-gen gen))
	 (n (mus-order dly))
	 (den (/ (* (1+ n) n) 2))
	 (num (weighted-moving-average-num gen))
	 (sum (weighted-moving-average-sum gen)))
    (set! num (- (+ num (* n y)) sum))
    (set! sum (moving-average dly y))
    (set! (weighted-moving-average-num gen) num)
    (set! (weighted-moving-average-sum gen) sum)
    (/ num den)))




;;; --------------------------------------------------------------------------------
;;;
;;; exponentially-weighted-moving-average
;;;
;;; geometric (r^n) weights

(defgenerator (exponentially-weighted-moving-average
	       :make-wrapper (lambda (g)
			       (let* ((n (exponentially-weighted-moving-average-n g))
				      (flt (make-one-pole (/ 1.0 n) (/ (- n) (+ 1.0 n)))))
				 (set! (exponentially-weighted-moving-average-gen g) flt)
				 g)))
  (n 128 :type int) (gen #f :type clm))


(define (exponentially-weighted-moving-average gen y)
  "  (make-exponentially-weighted-moving-average (n 128) returns an exponentially-weighted-moving-average generator.\n\
  (exponentially-weighted-moving-average gen y) returns the sum of the last n inputs weighted by (-n/(n+1))^k"
  (declare (gen exponentially-weighted-moving-average) (y float))
  (one-pole (exponentially-weighted-moving-average-gen gen) y))



;;; --------------------------------------------------------------------------------
;;;
;;; polyoid -- Tn + Un to get arbitrary initial-phases

(defgenerator (polyoid
	       :make-wrapper (lambda (g)
			       (let* ((lst (polyoid-partial-amps-and-phases g))
				      (len (vct-length lst))
				      (topk (let ((n 0))
					      (do ((i 0 (+ i 3)))
						  ((>= i len))
						(set! n (max n (inexact->exact (floor (vct-ref lst i))))))
					      n))
				      (sin-amps (make-vct (+ topk 1)))
				      (cos-amps (make-vct (+ topk 1))))
				 (do ((j 0 (+ j 3))
				      (i 0 (+ i 1)))
				     ((>= j len))
				   (let ((n (inexact->exact (vct-ref lst j)))
					 (amp (vct-ref lst (+ j 1)))
					 (phase (vct-ref lst (+ j 2))))
				     (if (> n 0)                                   ; constant only applies to cos side
					 (vct-set! sin-amps n (* amp (cos phase))))
				     (vct-set! cos-amps n (* amp (sin phase)))))
				 (set! (polyoid-tn g) cos-amps)
				 (set! (polyoid-un g) sin-amps)
				 (set! (polyoid-frequency g) (hz->radians (polyoid-frequency g)))
				 g))

	       :methods (list
			 (list 'mus-data
			       (lambda (g)
				 (polyoid-tn g)))
			 (list 'mus-xcoeffs
			       (lambda (g)
				 (polyoid-tn g)))
			 (list 'mus-ycoeffs
			       (lambda (g)
				 (polyoid-un g)))
			 (list 'mus-xcoeff
			       (lambda (g ind)
				 (vct-ref (polyoid-tn g) ind))
			       (lambda (g ind val)
				 (vct-set! (poltoid-tn g) ind val)))
			 (list 'mus-ycoeff
			       (lambda (g ind)
				 (vct-ref (polyoid-un g) ind))
			       (lambda (g ind val)
				 (vct-set! (poltoid-un g) ind val)))))

  (frequency *clm-default-frequency*) (partial-amps-and-phases #f :type vct) (angle 0.0)
  (tn #f :type vct) (un #f :type vct))


(define (polyoid gen fm)
  (declare (gen polyoid) (fm float))
  (let ((result (mus-chebyshev-tu-sum (polyoid-angle gen)
				      (polyoid-tn gen)
				      (polyoid-un gen))))

    (set! (polyoid-angle gen) (+ (polyoid-angle gen) fm (polyoid-frequency gen)))
    result))


#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-polyoid 100.0 (vct 1 1 0.0))))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (polyoid gen 0.0)))))

(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-polywave 100.0 '(1 1) mus-chebyshev-second-kind))
	(gen1 (make-oscil 100.0)))
    (set! (mus-phase gen) (* 0.5 pi))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (* (oscil gen1) (polywave gen 0.0))))))

(with-sound (:clipped #f :statistics #t)
  (let ((samps 44100)
	(gen (make-polyoid 100.0 (vct 1 0.5 0.0 51 0.25 0.0 64 .25 (/ pi 2)))))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i samps))
	 (outa i (polyoid gen 0.0)))))))

(define (test-polyoid n)
  (let* ((res (with-sound (:channels 2 :clipped #f)
                (let ((angle 0.0)
		      (incr (hz->radians 1.0))
		      (cur-phases (make-vct (* 3 n))))
		  (do ((i 0 (+ i 1))
		       (j 0 (+ j 3)))
		      ((= i n))
		    (vct-set! cur-phases j (+ i 1))
		    (vct-set! cur-phases (+ j 1) (/ 1.0 n))
		    (vct-set! cur-phases (+ j 2) (random (* 2 pi))))
		  (let ((gen (make-polyoid 1.0 cur-phases)))
		    (do ((i 0 (1+ i)))
			((= i 88200))
		      (let ((poly-sum 0.0)
			    (sin-sum 0.0))
			(do ((k 0 (1+ k)))
			    ((= k n))
			  (set! sin-sum (+ sin-sum (sin (+ (* (+ k 1) angle) (vct-ref cur-phases (+ (* 3 k) 2)))))))
			(set! poly-sum (polyoid gen 0.0))
			(set! angle (+ angle incr))
			(outa i (/ sin-sum n))
			(outb i poly-sum)))))))
	 (snd (find-sound res)))
  (channel-distance snd 0 snd 1)))

(define (test-polyoid-run n)
  (let* ((res (with-sound (:channels 2 :clipped #f)
                (let ((angle 0.0)
		      (incr (hz->radians 1.0))
		      (cur-phases (make-vct (* 3 n))))
		  (do ((i 0 (+ i 1))
		       (j 0 (+ j 3)))
		      ((= i n))
		    (vct-set! cur-phases j (+ i 1))
		    (vct-set! cur-phases (+ j 1) (/ 1.0 n))
		    (vct-set! cur-phases (+ j 2) (random (* 2 pi))))
		  (let ((gen (make-polyoid 1.0 cur-phases)))
		    (run
		     (lambda ()
		       (do ((i 0 (1+ i)))
			   ((= i 88200))
			 (let ((poly-sum 0.0)
			       (sin-sum 0.0))
			   (do ((k 0 (1+ k)))
			       ((= k n))
			     (set! sin-sum (+ sin-sum (sin (+ (* (+ k 1) angle) (vct-ref cur-phases (+ (* 3 k) 2)))))))
			   (set! poly-sum (polyoid gen 0.0))
			   (set! angle (+ angle incr))
			   (outa i (/ sin-sum n))
			   (outb i poly-sum)))))))))
	 (snd (find-sound res)))
  (channel-distance snd 0 snd 1)))

;;; 0 diff up to 4096 so far (unopt and opt) -- 1.0e-12 at 4096, opt is more than 20 times as fast
|#



;;; --------------------------------------------------------------------------------
;;;
;;; noid -- sum of n sinusoids at arbitrary (default=random) initial phases
;;;
;;;   for max peak (all cos), set phases arg to (make-vct n (/ pi 2))
;;;   for min peak, use one of the sets in peak-phases.scm (multiplied through by pi)
;;;
;;; since initial phases are 0 or pi in peak-phases.scm if n>20, this code could be optimized

(def-optkey-fun (make-noid (frequency 0.0) (n 1) (phases #f))
  (make-polyoid frequency
		(let ((amps (make-vct (* 3 n))))
		  (do ((i 1 (1+ i))
		       (j 0 (+ j 3)))
		      ((> i n))
		    (vct-set! amps j i)
		    (vct-set! amps (+ j 1) (/ 1.0 n))
		    (if (vct? phases)
			(vct-set! amps (+ j 2) (vct-ref phases (1- i)))
			(if (not phases)
			    (vct-set! amps (+ j 2) (random (* 2 pi)))
			    (if (eq? phases 'max-peak)
				(vct-set! amps (+ j 2) (/ pi 2))
				;; else min-peak, handled separately
				))))

		  (if (eq? phases 'min-peak)
		      (let ((vector-find-if (lambda (func vect)
					      (let ((len (vector-length vect))
						    (result #f))
						(do ((i 0 (1+ i)))
						    ((or (= i len)
							 result)
						     result)
						  (set! result (func (vector-ref vect i))))))))

			(if (not (defined? 'noid-min-peak-phases))
			    (load "peak-phases.scm"))

			(let ((min-dat (vector-find-if 
					(lambda (val)
					  (and val
					       (vector? val)
					       (= (vector-ref val 0) n)
					       (let* ((a-val (vector-ref val 1))
						      (a-len (vector-length val))
						      (a-data (list a-val (vector-ref val 2))))
						 (do ((k 2 (1+ k)))
						     ((= k a-len))
						   (if (and (number? (vector-ref val k))
							    (< (vector-ref val k) a-val))
						       (begin
							 (set! a-val (vector-ref val k))
							 (set! a-data (list a-val (vector-ref val (+ k 1)))))))
						 a-data)))
					noid-min-peak-phases)))
			  (if min-dat
			      (let ((norm (car min-dat))
				    (rats (cadr min-dat)))
				(do ((i 1 (1+ i))
				     (j 0 (+ j 3)))
				    ((> i n))
				  (vct-set! amps (+ j 1) (/ 0.999 norm)) ; I'm truncating when saving the peak
				  (vct-set! amps (+ j 2) (* pi (vector-ref rats (1- i))))))))))
			      
		  amps)))

(define noid polyoid)
(define noid? polyoid?)

#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-noid 100.0 3)))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (noid gen 0.0)))))

(with-sound (:clipped #f :channels 2)
  (let* ((samps 44100)
	 (n 10)
	 (gen (make-noid 1.0 n 'min-peak))
	 (gen2 (make-oscil n (vct-ref (polyoid-partial-amps-and-phases gen) (1- (vct-length (polyoid-partial-amps-and-phases gen)))))))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (noid gen 0.0))
      (outb i (oscil gen2)))))

(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-noid 100.0 10 'min-peak)))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (noid gen 0.0)))))

(with-sound (:clipped #f :statistics #t)
  (let ((samps 44100)
	(gen (make-noid 10.0 1024 'min-peak)))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (noid gen 0.0)))))

(with-sound (:clipped #f :channels 4)
  (let ((samps 44100)
	(gen1 (make-noid 100.0 32 'max-peak))
	(gen2 (make-noid 100.0 32 (make-vct 32 0.0)))
	(gen3 (make-noid 100.0 32))
	(gen4 (make-noid 100.0 32 'min-peak)))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (noid gen1 0.0))
      (outb i (noid gen2 0.0))
      (outc i (noid gen3 0.0))
      (outd i (noid gen4 0.0)))))


  (do ((i 0 (1+ i)))
      ((= i 4))
    (with-sound (:clipped #f :output (string-append "test-noid-" (number->string i) ".snd"))
      (let ((samps 44100)
	    (gen (make-noid 100.0 32 (if (= i 0) 'max-peak
					 (if (= i 1) (make-vct 32 0.0)
					     (if (= i 2) #f
						 'min-peak))))))
	(do ((i 0 (1+ i)))
	    ((= i samps))
	  (outa i (noid gen 0.0))))))

|#

#|
;;; --------------------------------------------------------------------------------
;;;
;;; roid -- sum of n sinusoids at arbitrary (default=random) initial phases and amp r^n

(def-optkey-fun (make-roid (frequency 0.0) (n 1) (r 1.0) (phases #f))
  (make-polyoid frequency
		(let ((amps (make-vct (* 3 n)))
		      (rn (/ 1.0 n)))
		  (do ((i 1 (1+ i))
		       (j 0 (+ j 3)))
		      ((> i n))
		    (vct-set! amps j i)
		    (vct-set! amps (+ j 1) rn)
		    (set! rn (* rn r))
		    (if (vct? phases)
			(vct-set! amps (+ j 2) (vct-ref phases (1- i)))
			(if (not phases)
			    (vct-set! amps (+ j 2) (random (* 2 pi)))
			    (if (eq? phases 'max-peak)
				(vct-set! amps (+ j 2) (/ pi 2))
				;; else min-peak, handled separately
				))))

		  (if (eq? phases 'min-peak)
		      (let ((vector-find-if (lambda (func vect)
					      (let ((len (vector-length vect))
						    (result #f))
						(do ((i 0 (1+ i)))
						    ((or (= i len)
							 result)
						     result)
						  (set! result (func (vector-ref vect i))))))))

			(if (not (defined? 'roid-min-peak-phases))
			    (load "peak-phases.scm"))

			(let ((min-dat (vector-find-if 
					(lambda (val)
					  (and val
					       (vector? val)
					       (= (vector-ref val 0) n)
					       (let* ((a-val (vector-ref val 1))
						      (a-len (vector-length val))
						      (a-data (list a-val (vector-ref val 2))))
						 (do ((k 2 (1+ k)))
						     ((= k a-len))
						   (if (and (number? (vector-ref val k))
							    (< (vector-ref val k) a-val))
						       (begin
							 (set! a-val (vector-ref val k))
							 (set! a-data (list a-val (vector-ref val (+ k 1)))))))
						 a-data)))
					roid-min-peak-phases)))
			  (if min-dat
			      (let* ((norm (car min-dat))
				     (rats (cadr min-dat))
				     (rn (/ 0.999 norm)))
				(do ((i 1 (1+ i))
				     (j 0 (+ j 3)))
				    ((> i n))
				  (vct-set! amps (+ j 1) rn)
				  (set! rn (* rn r))
				  (vct-set! amps (+ j 2) (* pi (vector-ref rats (1- i))))))))))
			      
		  amps)))

(define roid polyoid)
(define roid? polyoid?)
|#

#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-roid 100.0 6 0.5 'min-peak)))
    (do ((i 0 (1+ i)))
	((= i samps))
      (outa i (roid gen 0.0)))))
|#


;;; ---------------- old waveshape generator ----------------

(define waveshape? polyshape?)
(define waveshape polyshape)

(def-optkey-fun (make-waveshape (frequency *clm-default-frequency*) 
				(partials '(1 1)) 
				wave 
				(size *clm-table-size*))
  (if (not wave)
      (make-polyshape frequency :partials partials)
      (make-polyshape frequency :coeffs wave)))
(def-optkey-fun (partials->waveshape partials 
				     (size *clm-table-size*))
  (partials->polynomial partials))
		  


;;; ---------------- tanh(sin(x)) ----------------

(defgenerator (tanhsin
	       :make-wrapper (lambda (g)
			       (set! (tanhsin-osc g) (make-oscil (tanhsin-frequency g) (tanhsin-initial-phase g)))
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (initial-phase 0.0)
  (osc #f :type clm))


(define (tanhsin gen fm)
  "(make-tanhsin (frequency 0.0) (r 1.0) (initial-phase 0.0) returns a tanhsin generator.\n\
(tanhsin gen fm) produces tanh(r*sin) which approaches a square wave as r increases."
  (declare (gen tanhsin) (fm float))
  (tanh (* (tanhsin-r gen)
	   (oscil (tanhsin-osc gen) fm))))


;;; ---------------- moving spectrum ----------------
;;;
;;; this is the first half of the phase-vocoder (modulo ignoring "interp!=hop" business)
;;;   I will probably move this into C eventually

(defgenerator (moving-spectrum
	       :make-wrapper (lambda (g)
			       (let ((n (moving-spectrum-n g)))
				 (set! (moving-spectrum-amps g) (make-vct n))
				 (set! (moving-spectrum-phases g) (make-vct n))
				 (set! (moving-spectrum-amp-incs g) (make-vct n))
				 (set! (moving-spectrum-freqs g) (make-vct n))
				 (set! (moving-spectrum-freq-incs g) (make-vct n))
				 (set! (moving-spectrum-new-freq-incs g) (make-vct n))
				 (set! (moving-spectrum-data g) (make-vct n))
				 (set! (moving-spectrum-fft-window g) (make-fft-window hamming-window n))
				 (vct-scale! (moving-spectrum-fft-window g) (/ 2.0 (* 0.54 n)))
				 (set! (moving-spectrum-outctr g) (+ n 1)) ; first time fill flag
				 g)))
  (input #f :type clm) (n 512) (hop 128) 
  (outctr 0 :type int)
  (amps #f :type vct) (phases #f :type vct) 
  (amp-incs #f :type vct) (freqs #f :type vct) (freq-incs #f :type vct) (new-freq-incs #f :type vct) 
  (fft-window #f :type vct)
  (data #f :type vct) (dataloc 0 :type int))

(define (moving-spectrum gen)
  (let* ((n (moving-spectrum-n gen))
	 (n2 (/ n 2))
	 (amps (moving-spectrum-amps gen))
	 (phases (moving-spectrum-phases gen))
	 (amp-incs (moving-spectrum-amp-incs gen))
	 (freqs (moving-spectrum-freqs gen))
	 (new-freq-incs (moving-spectrum-new-freq-incs gen))
	 (hop (moving-spectrum-hop gen))
	 (outctr (moving-spectrum-outctr gen)))
    (if (>= outctr hop)
	(let* ((data (moving-spectrum-data gen))
	       (dataloc (moving-spectrum-dataloc gen))
	       (fft-window (moving-spectrum-fft-window gen))
	       (freq-incs (moving-spectrum-freq-incs gen)))

	  (if (> outctr n) ; must be first time through -- fill data array
	      (begin
		(do ((i 0 (1+ i)))
		    ((= i n))
		  (vct-set! data i (readin (moving-spectrum-input gen)))))
	      (begin
		(do ((i 0 (1+ i))
		     (j hop (1+ j)))
		    ((= j n))
		  (vct-set! data i (vct-ref data j)))
		(do ((i (- n hop) (1+ i)))
		    ((= i n))
		  (vct-set! data i (readin (moving-spectrum-input gen))))))

	  (set! outctr 0) ; -1??
	  (set! dataloc (modulo dataloc n))

	  (clear-array new-freq-incs)
	  (do ((i 0 (1+ i))
	       (j dataloc (1+ j)))
	      ((= i n))
	    (if (= j n) (set! j 0))
	    (vct-set! amp-incs j (* (vct-ref fft-window i)
				    (vct-ref data i))))

	  (set! (moving-spectrum-dataloc gen) (+ dataloc hop))
	  
	  (mus-fft amp-incs new-freq-incs n 1)
	  (rectangular->polar amp-incs new-freq-incs)

	  (let ((scl (/ 1.0 hop))
		(kscl (/ (* 2 pi) n)))
	    (vct-subtract! amp-incs amps)
	    (vct-scale! amp-incs scl)

	    (do ((i 0 (1+ i))
		 (ks 0.0 (+ ks kscl)))
		((= i n2))
	      (let ((diff (fmod (- (vct-ref new-freq-incs i) (vct-ref freq-incs i)) (* 2 pi))))
		(vct-set! freq-incs i (vct-ref new-freq-incs i))
		(if (> diff pi) (set! diff (- diff (* 2 pi))))
		(if (< diff (- pi)) (set! diff (+ diff (* 2 pi))))
		(vct-set! new-freq-incs i (+ (* diff scl) ks))))

	    (vct-subtract! new-freq-incs freqs)
	    (vct-scale! new-freq-incs scl))))
	  
    (set! (moving-spectrum-outctr gen) (+ outctr 1))

    (vct-add! amps amp-incs)
    (vct-add! freqs new-freq-incs)
    (vct-add! phases freqs)))


(define (test-sv)
  ;; sv-amps = pv-amps (but len is diff)
  ;; sv-phases = pv-phases
  ;; sv-freqs = pv-phase-increments

  (let ((pv (make-phase-vocoder (make-readin "oboe.snd") ))
	(sv (make-moving-spectrum (make-readin "oboe.snd"))))
    (do ((k 0 (1+ k)))
	((= k 20))
      (do ((i 0 (1+ i))) 
	  ((= i 2000)) 
	(phase-vocoder pv) 
	(moving-spectrum sv))
      (do ((i 0 (1+ i)))
	  ((= i 256))
	(if (fneq (vct-ref (moving-spectrum-amps sv) i) (vct-ref (phase-vocoder-amps pv) i))
	    (snd-display ";~D amps: ~A ~A" i (vct-ref (moving-spectrum-amps sv) i) (vct-ref (phase-vocoder-amps pv) i)))
	(if (fneq (vct-ref (moving-spectrum-phases sv) i) (vct-ref (phase-vocoder-phases pv) i))
	    (snd-display ";~D phases: ~A ~A" i (vct-ref (moving-spectrum-phases sv) i) (vct-ref (phase-vocoder-phases pv) i)))
	(if (fneq (vct-ref (moving-spectrum-freqs sv) i) (vct-ref (phase-vocoder-phase-increments pv) i))
	    (snd-display ";~D freqs: ~A ~A" i (vct-ref (moving-spectrum-freqs sv) i) (vct-ref (phase-vocoder-phase-increments pv) i)))))))

#|
(with-sound (:channels 2)
  (let* ((gen (make-moving-spectrum (make-readin "oboe.snd")))
	 (pv (make-phase-vocoder (make-readin "oboe.snd")))
	 (samps (mus-sound-frames "oboe.snd")))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i samps))
	 (moving-spectrum gen)
	 (outa i (sine-bank (moving-spectrum-amps gen) (moving-spectrum-phases gen) 256)) ; size = n/2 as in pv
	 (outb i (phase-vocoder pv)))))))

; :(channel-distance 0 0 0 1)
; 7.902601100022e-9
|#



;;; TODO:   how to change time arbitrarily? via env?
;;; TODO: also sawtoothize and other such substitutions
;;; TODO: amp>n sin^2
;;; TODO: narrow band noise on some or on loud ones
;;; TODO: change vib (no vib)
;;;           moving-spectrum could replace all the formant bank stuff eventually
;;; TODO:   moving-window (delay data) (two delays + pulse = gran), would need an open-ended delay [outa?], window-ref, array of frames?
;;; TODO:   moving-lpc? (dsp.scm has lpc-coeffs which is optimizable) [moving-poly?] [cheb approx?]




;;; --------------------------------------------------------------------------------
;;;
;;; set up make-* help strings

(if (provided? 'snd-guile)
    (for-each
     (lambda (funcstr makefunc)
       (if funcstr
	   (set-procedure-property! makefunc 'documentation funcstr)))
     (map procedure-documentation 
	  (list nssb nxysin nxycos nxy1cos nxy1sin noddsin noddcos noddssb ncos2 npcos
		nrsin nrcos nrssb nkssb nsincos rcos rssb rxysin rxycos
		rxyk!sin rxyk!cos ercos erssb eoddcos rkcos rksin rkssb
		rk!cos rk!ssb r2k!cos k2sin k2cos k2ssb dblsum rkoddssb krksin
		abcos absin r2k2cos bess jjcos j0evencos j2cos jpcos jncos 
		j0j1cos jycos blackman fmssb k3sin izcos nchoosekcos n1cos
		adjustable-square-wave adjustable-triangle-wave adjustable-sawtooth-wave adjustable-oscil 
		round-interp sinc-train pink-noise green-noise brown-noise green-noise-interp
		moving-max moving-sum moving-rms moving-length weighted-moving-average exponentially-weighted-moving-average 
		tanhsin
		))
     (list make-nssb make-nxysin make-nxycos make-nxy1cos make-nxy1sin make-noddsin make-noddcos make-noddssb make-ncos2 make-npcos
	   make-nrsin make-nrcos make-nrssb make-nkssb make-nsincos make-rcos make-rssb make-rxysin make-rxycos
	   make-rxyk!sin make-rxyk!cos make-ercos make-erssb make-eoddcos make-rkcos make-rksin make-rkssb
	   make-rk!cos make-rk!ssb make-r2k!cos make-k2sin make-k2cos make-k2ssb make-dblsum make-rkoddssb make-krksin
	   make-abcos make-absin make-r2k2cos make-bess make-jjcos make-j0evencos make-j2cos make-jpcos make-jncos
	   make-j0j1cos make-jycos make-blackman make-fmssb make-k3sin make-izcos make-nchoosekcos make-n1cos
	   make-adjustable-square-wave make-adjustable-triangle-wave make-adjustable-sawtooth-wave make-adjustable-oscil
	   make-round-interp make-sinc-train make-pink-noise make-green-noise make-brown-noise make-green-noise-interp
	   make-moving-max make-moving-sum make-moving-rms make-moving-length make-weighted-moving-average make-exponentially-weighted-moving-average 
	   make-tanhsin
	   )))


;;; --------------------------------------------------------------------------------

(define (calling-all-generators)
  ;; for snd-test
  (with-sound (:play #f)
    (lutish 0 1 440 .1)
    (oboish 1 1 300 .1 '(0 0 1 1 2 0))
    (nkssber 2 1 1000 100 5 5 0.5)
    (stringy 3 1 1000 .5)
    (ercoser 4 1 100 .5 0.1)
    (bouncy 5 2 300 .5 5 10)
    (pianoy 6 3 100 .5)
    (pianoy1 7 4 200 .5 1 .1)
    (pianoy2 8 1 100 .5)
    (glassy 9 .1 1000 .5)
    (machine1 10 .3 100 540 0.5 3.0 0.0)
    (organish 11 .4 100 .5 1.0 #f)
    (brassy 12 4 50 .5 '(0 0 1 1 10 1 11 0) '(0 1 1 0) 1000)))


