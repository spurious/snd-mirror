(provide 'snd-big-gens.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))


(define (big-hz->radians hz)
  (/ (* hz 2 pi)
     (mus-srate)))

(define (big-radians->hz rad)
  (/ (* rad (mus-srate))
     (* 2 pi)))


(defgenerator (big-oscil 
  :make-wrapper 
    (lambda (g)
      (set! (big-oscil-frequency g) (big-hz->radians (big-oscil-frequency g)))
      g))
  (frequency *clm-default-frequency*) (angle 0.0))

(define* (big-oscil gen :optional (fm 0.0) (pm 0.0))
  (declare (gen big-oscil) (fm float))
  (let ((x (big-oscil-angle gen)))
    (set! (big-oscil-angle gen) (+ fm x (big-oscil-frequency gen)))
    (sin (+ x pm))))


;;; TODO: rest of big-gens?

