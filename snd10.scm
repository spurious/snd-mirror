;;; backwards compatibility for snd 10

(provide 'snd-snd10.scm)

(define sine-summation nrxysin)
(define sine-summation? nrxysin?)

(define sum-of-sines nsin)
(define sum-of-sines? nsin?)

(define sum-of-cosines ncos)
(define sum-of-cosines? ncos?)

(define* (make-sum-of-sines :optional (sines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-nsin :frequency frequency :n sines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sum-of-cosines :optional (cosines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-ncos :frequency frequency :n cosines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sine-summation :optional (frequency 0.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0))
  (let ((gen (make-nrxysin :frequency frequency :ratio ratio :n n :r a)))
    (set! (mus-phase gen) initial-phase)
    gen))

