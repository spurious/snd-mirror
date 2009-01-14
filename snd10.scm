;;; backwards compatibility for snd 10

(provide 'snd-snd10.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm")) ; def-optkey-fun for guile's benefit

(define sine-summation nrxysin)
(define sine-summation? nrxysin?)

(define sum-of-sines nsin)
(define sum-of-sines? nsin?)

(define sum-of-cosines ncos)
(define sum-of-cosines? ncos?)

;;; in s7 these could simple be define*
(def-optkey-fun (make-sum-of-sines (sines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-nsin :frequency frequency :n sines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(def-optkey-fun (make-sum-of-cosines (cosines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-ncos :frequency frequency :n cosines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(def-optkey-fun (make-sine-summation (frequency 0.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0))
  (let ((gen (make-nrxysin :frequency frequency :ratio ratio :n n :r a)))
    (set! (mus-phase gen) initial-phase)
    gen))

