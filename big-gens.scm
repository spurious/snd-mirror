(provide 'snd-big-gens.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))


;;; -------- conversions --------

(define (big-hz->radians hz)
  (/ (* hz 2 pi) (mus-srate)))

(define (big-radians->hz rad)
  (/ (* rad (mus-srate)) (* 2 pi)))

(define (big-db->linear x)
  (expt 10.0 (/ x 20.0)))

(define (big-linear->db x)
  (if (> x 0.0) (* 20.0 (log x 10)) -100.0))

(define (big-degrees->radians deg)
  (* (/ pi 180) deg))

(define (big-radians->degrees rad)
  (/ (* rad 180) pi))

(define (big-seconds->samples secs)
  (round (* secs (mus-srate))))

(define (big-samples->seconds samps)
  (/ samps (mus-srate)))

(define (big-rectangular->polar rl im)
  (let ((len (length rl)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (let ((rl1 (rl i))
	    (im1 (im i)))
	(set! (rl i) (sqrt (+ (* rl1 rl1) (* im1 im1))))
	(set! (im i) (- (atan im1 rl1)))))))

(define (big-polar->rectangular mag ang)
  (let ((len (length mag)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (let ((mag1 (mag i))
	    (ang1 (- (ang i))))
	(set! (mag i) (* mag1 (cos ang1)))
	(set! (ang i) (* mag1 (sin ang1)))))))


;;; -------- arrays (vectors in this context) --------

(define (big-array-clear v)
  (vector-fill! v 0.0))

(define (big-array-normalize v)
  (let ((len (length v))
	(pk 0.0))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! pk (max pk (abs (v i)))))
    (if (and (not (= pk 0.0))
	     (not (= pk 1.0)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (v i) (/ (v i) pk))))
    v))

(define (big-array-interp wave x n)
  (let* ((xx (modulo x n))
	 (ipart (floor xx))
	 (fpart (- xx ipart)))
    (if (zero? fpart)
	(wave ipart)
	(+ (wave ipart)
	   (* fpart (- (wave (modulo (+ ipart 1) n)) 
		       (wave ipart)))))))

(define (big-multiply-arrays v1 v2)
  (let ((len (min (length v1) (length v2))))
    (do ((i 0 (+ i 1)))
	((= i len) v1)
      (set! (v1 i) (* (v1 i) (v2 i))))))



;;; -------- polynomial --------

(define (big-polynomial coeffs x)
  (let* ((top (- (length coeffs) 1))
	 (sum (coeffs top)))
    (do ((i (- top 1) (- i 1)))
	((< i 0) sum)
      (set! sum (+ (* x sum)
		   (coeffs i))))))


;;; -------- dot-product --------

(define (big-dot-product v1 v2)
  (let ((len (min (length v1) (length v2))))
    (do ((sum 0.0)
	 (i 0 (+ i 1)))
	((= i len) sum)
      (set! sum (+ sum (* (v1 i) (v2 i)))))))


;;; -------- ring-modulate --------

(define (big-ring-modulate in1 in2)
  (* in1 in2))


;;; -------- amplitude-modulate --------

(define (big-amplitude-modulate carrier in1 in2)
  (* in1 (+ carrier in2)))


;;; -------- contrast-enhancement --------

(define* (big-contrast-enhancement in1 (index 1.0))
  (sin (+ (* in1 (/ pi 2))
	  (* index (sin (* 2 pi in1))))))


;;; -------- oscil --------

(defgenerator (big-oscil 
  :make-wrapper 
    (lambda (g) (set! (big-oscil-frequency g) (big-hz->radians (big-oscil-frequency g))) g))
  (frequency *clm-default-frequency*) (angle 0.0))

(define* (big-oscil gen (fm 0.0) (pm 0.0))
  (declare (gen big-oscil) (fm float) (pm float))
  (let ((x (big-oscil-angle gen)))
    (set! (big-oscil-angle gen) (+ fm x (big-oscil-frequency gen)))
    (sin (+ x pm))))


;;; -------- ncos --------

(defgenerator (big-ncos
  :make-wrapper
   (lambda (g)
     (if (<= (big-ncos-n g) 0)
	 (set! (big-ncos-n g) 1))
     (set! (big-ncos-r g) (/ (big-ncos-n g)))
     (set! (big-ncos-frequency g) (big-hz->radians (big-ncos-frequency g)))
     g))
   (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0) (r 1.0))

(define* (big-ncos gen (fm 0.0))
  (declare (gen big-ncos) (fm float))
  (let* ((n (big-ncos-n gen))
	 (x (big-ncos-angle gen))
	 (scl (big-ncos-r gen))
	 (den (* 2 (sin (/ x 2)))))
    (set! (big-ncos-angle gen) (+ fm x (big-ncos-frequency gen)))
    (if (= den 0.0)
	1.0
	(min 1.0 (* scl (- (/ (sin (* (+ n 1/2) x)) den) 1/2))))))


;;; -------- nsin --------

(defgenerator (big-nsin
  :make-wrapper
   (lambda (g)
     (letrec ((nsin-ns (lambda (x n)
			 (let* ((a2 (/ x 2))
				(den (sin a2)))
			   (if (= den 0.0)
			       0.0
			       (/ (* (sin (* n a2))
				     (sin (* (+ n 1) a2)))
				  den)))))
	      (find-nsin-scaler (lambda (n lo hi)
				  (let ((mid (/ (+ lo hi) 2))
					(ylo (nsin-ns lo n))
					(yhi (nsin-ns hi n)))
				    (if (< (abs (- yhi ylo)) 1e-12)
					(nsin-ns mid n)
					(if (> ylo yhi)
					    (find-nsin-scaler n lo mid)
					    (find-nsin-scaler n mid hi)))))))
     (if (<= (big-nsin-n g) 0)
	 (set! (big-nsin-n g) 1))
     (set! (big-nsin-r g) (/ 1.0 (find-nsin-scaler (big-nsin-n g) 0.0 (/ pi (+ (big-nsin-n g) 1/2)))))
     (set! (big-nsin-frequency g) (big-hz->radians (big-nsin-frequency g)))
     g)))
   (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0) (r 1.0))

(define* (big-nsin gen (fm 0.0))
  (declare (gen big-nsin) (fm float))
  (let* ((n (big-nsin-n gen))
	 (x (big-nsin-angle gen))
	 (a2 (/ x 2))
	 (scl (big-nsin-r gen))
	 (den (sin a2)))
    (set! (big-nsin-angle gen) (+ fm x (big-nsin-frequency gen)))
    (if (= den 0.0)
	0.0
	(/ (* scl (sin (* n a2)) (sin (* (+ n 1) a2))) den))))

#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-nsin 100.0 10)))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-nsin g))))))
|#


;;; -------- table-lookup --------

(defgenerator (big-table-lookup
  :make-wrapper
    (lambda (g)
      (if (not (big-table-lookup-wave g))
	  (set! (big-table-lookup-wave g) (make-vector (big-table-lookup-size g) 0.0))
	  (set! (big-table-lookup-size g) (length (big-table-lookup-wave g))))
      (set! (big-table-lookup-frequency g) (/ (* (big-table-lookup-frequency g) (big-table-lookup-size g)) (mus-srate)))
      (set! (big-table-lookup-angle g) (/ (* (big-table-lookup-angle g) (big-table-lookup-size g)) (* 2 pi)))
      g))
  (frequency *clm-default-frequency*) (angle 0.0) (wave #f :type vector) (size *clm-table-size*))

(define* (big-table-lookup gen (fm 0.0))
  (declare (gen big-table-lookup) (fm float))
  (let ((x (big-table-lookup-angle gen))
	(w (big-table-lookup-wave gen))
	(n (big-table-lookup-size gen)))
    (set! (big-table-lookup-angle gen) (+ x (big-table-lookup-frequency gen) (/ (* fm n) (* 2 pi))))
    (big-array-interp w x n)))
      
#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-table-lookup 100.0 0.0 (let ((w (make-vector 32)))
					      (do ((i 0 (+ i 1)))
						  ((= i 32) w)
						(set! (w i) (sin (/ (* i pi) 16))))))))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-table-lookup g))))))
|#



;;; -------- one-zero --------

(defgenerator big-one-zero (a0 1.0) (a1 0.0) (x1 0.0))

(define* (big-one-zero gen x)
  (declare (gen big-one-zero) (x float))
  (let ((val (+ (* x (big-one-zero-a0 gen))
		(* (big-one-zero-x1 gen) (big-one-zero-a1 gen)))))
    (set! (big-one-zero-x1 gen) x)
    val))

		  
;;; -------- one-pole --------

(defgenerator big-one-pole (a0 1.0) (b1 0.0) (y1 0.0))

(define* (big-one-pole gen x)
  (declare (gen big-one-pole) (x float))
  (let ((val (- (* x (big-one-pole-a0 gen))
		(* (big-one-pole-y1 gen) (big-one-pole-b1 gen)))))
    (set! (big-one-pole-y1 gen) val)
    val))


		  




;;; TODO: rest of big-gens and big gen tests/doc etc


