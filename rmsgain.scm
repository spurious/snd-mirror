;;;
;;; rmsgain.scm
;;;
;;; Fabio Furlanete.

(use-modules (oop goops))
(provide 'snd-rmsgain.scm)

(define-class <rmsgain> ()
  (c1 :accessor c1)
  (c2 :accessor c2)
  (q :init-value 0 :accessor q)
  (r :init-value 0 :accessor r)
  (avg :init-value 0.0 :accessor avg)
  (avgc :init-value 0 :accessor avgc))

(define-method (initialize (obj <rmsgain>) initargs)
  (next-method)
  (let* ((hp (get-keyword :hp initargs 10))
 	 (b (- 2 (cos (* hp (/ (* 2 pi) (mus-srate)))))))
    (set! (c2 obj) (- b (sqrt (- (* b b) 1))))
    (set! (c1 obj) (- 1 (c2 obj)))))

(define-method (rms (obj <rmsgain>) sig)
  (set! (q obj) (+ (* (c1 obj) sig sig)
		   (* (c2 obj) (q obj))))
  (sqrt (q obj)))

(define-method (gain (obj <rmsgain>) sig rms)
  (set! (r obj) (+ (* (c1 obj) sig sig)
		   (* (c2 obj) (r obj))))
  (let ((this-gain (if (zero? (r obj))
		       rms
		       (/ rms (sqrt (r obj))))))
    (set! (avg obj) (+ (avg obj) this-gain))
    (set! (avgc obj) (+ (avgc obj) 1))
    (* sig this-gain)))

(define-method (balance (obj <rmsgain>) signal compare)
  (gain obj signal (rms obj compare)))

(define-method (gain-avg (obj <rmsgain>))
  (/ (avg obj) (avgc obj)))

(define-method (balance-avg (obj <rmsgain>))
  (avg obj))
