;;; polynomial-related stuff

;;; PERHAPS: mixer*/mixer+ frame*/frame+ -> run.lisp
;;; PERHAPS: vct-offset! (-> vct-add!|subtract! -> vct+), vct-scale! (-> vct-multiply! -> vct*)
;;; PERHAPS:   but perhaps the +/* versions should return a new vct [channel*, channel+, etc?]

;;; PERHAPS: also frame = vct so perhaps they can be combined at some (or all?) levels?
;;; TODO: also mixer* could handle all frame->frame cases

;;; TODO: complex in poly -- needs support in vct or edot

(provide 'snd-poly.scm)

(define (poly+ p1 p2)
  (if (vct? p1)
      (if (vct? p2)
	  (if (> (vct-length p1) (vct-length p2))
	      (vct-add! (vct-copy p1) p2)
	      (vct-add! (vct-copy p2) p1))
	  (let ((v (vct-copy p1)))
	    (vct-set! v 0 (+ (vct-ref v 0) p2))
	    v))
      (let ((v (vct-copy p2)))
	(vct-set! v 0 (+ (vct-ref v 0) p1))
	v)))

;;; (poly+ (vct .1 .2 .3) (vct 0.0 1.0 2.0 3.0 4.0)) -> #<vct[len=5]: 0.100 1.200 2.300 3.000 4.000>
;;; (poly+ (vct .1 .2 .3) .5) -> #<vct[len=3]: 0.600 0.200 0.300>
;;; (poly+ .5 (vct .1 .2 .3)) -> #<vct[len=3]: 0.600 0.200 0.300>


(define (poly* p1 p2)
  (if (vct? p1)
      (if (vct? p2)
	  (let* ((p1len (vct-length p1))
		 (p2len (vct-length p2))
		 (len (+ p1len p2len))
		 (m (make-vct len 0.0)))
	    (do ((i 0 (1+ i)))
		((= i p1len) m)
	      (do ((j 0 (1+ j)))
		  ((= j p2len))
		(vct-set! m (+ i j) (+ (vct-ref m (+ i j)) (* (vct-ref p1 i) (vct-ref p2 j)))))))
	  (vct-scale! (vct-copy p1) p2))
      (vct-scale! (vct-copy p2) p1)))
    
;;; (poly* (vct 1 1) (vct -1 1)) -> #<vct[len=4]: -1.000 0.000 1.000 0.000>
;;; (poly* (vct -5 1) (vct 3 7 2)) -> #<vct[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (vct -30 -4 2) (vct 0.5 1)) -> #<vct[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (vct -30 -4 2) 0.5) -> #<vct[len=3]: -15.000 -2.000 1.000>
;;; (poly* 2.0 (vct -30 -4 2)) -> #<vct[len=3]: -60.000 -8.000 4.000>


(define (poly/ p1 p2)
  (if (vct? p1)
      (if (vct? p2)
	  ;; Numerical Recipes poldiv
	  (let* ((p1len (vct-length p1))
		 (p2len (vct-length p2))
		 (len (max p1len p2len))
		 (r (make-vct len 0.0))
		 (q (make-vct len 0.0)))
	    (do ((i 0 (1+ i)))
		((= i len))
	      (vct-set! r i (vct-ref p1 i)))
	    (let ((n (1- p1len))
		  (nv (1- p2len)))
	      (do ((k (- n nv) (1- k)))
		  ((< k 0))
		(vct-set! q k (/ (vct-ref r (+ nv k)) (vct-ref p2 nv)))
		(do ((j (+ nv k -1) (1- j)))
		    ((< j k))
		  (vct-set! r j (- (vct-ref r j) (* (vct-ref q k) (vct-ref p2 (- j k)))))))
	      (do ((j nv (1+ j)))
		  ((> j n))
		(vct-set! r j 0.0))
	      (list q r)))
	  (poly* p1 (/ 1.0 p2)))
      (poly/ (vct p1) p2))) ; TODO: this is broken

;;; (poly/ (vct -1.0 -0.0 1.0) (vct 1.0 1.0)) -> (#<vct[len=3]: -1.000 1.000 0.000> #<vct[len=3]: 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vct -5 1)) -> (#<vct[len=4]: 3.000 7.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vct 3 1)) -> (#<vct[len=4]: -5.000 -9.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vct .5 1)) -> (#<vct[len=4]: -30.000 -4.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vct 3 7 2)) -> (#<vct[len=4]: -5.000 1.000 0.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) 2.0) -> #<vct[len=4]: -7.500 -16.000 -1.500 1.000>


(define (poly-derivative p1)
  (let* ((len (1- (vct-length p1)))
	 (v (make-vct len)))
    (do ((i (1- len) (1- i))
	 (j len (1- j)))
	((< i 0) v)
      (vct-set! v i (* j (vct-ref p1 j))))))

;;; (poly-derivative (vct 0.5 1.0 2.0 4.0)) -> #<vct[len=3]: 1.000 4.000 12.000>


;;; TODO: factor-poly

