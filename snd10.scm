;;; backwards compatibility for snd 10

(provide 'snd-snd10.scm)

(define sine-summation nrxysin)
(define sine-summation? nrxysin?)

(define sum-of-sines nsin)
(define sum-of-sines? nsin?)

(define sum-of-cosines ncos)
(define sum-of-cosines? ncos?)

(define* (make-sum-of-sines (sines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-nsin :frequency frequency :n sines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sum-of-cosines (cosines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-ncos :frequency frequency :n cosines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sine-summation (frequency 0.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0))
  (let ((gen (make-nrxysin :frequency frequency :ratio ratio :n n :r a)))
    (set! (mus-phase gen) initial-phase)
    gen))


(define (keypad-spectro-bindings)
  "establish the old spectro-* keypad bindings"

  (bind-key "KP_Page_Down" 0
    (lambda ()
      "move spectrum end down by about 5%"
      (set! (spectrum-end) (* (spectrum-end) 0.95))))

  (bind-key "KP_Page_Up" 0
    (lambda ()
      "move spectrum end up by about 5%"
      (set! (spectrum-end) (min 1.0 (/ (spectrum-end) 0.95)))))

  (bind-key "KP_Up" 0
    (lambda ()
      "increase spectro-z-scale by 0.01"
      (set! (spectro-z-scale) (+ (spectro-z-scale) 0.01))))

  (bind-key "KP_Down" 0
    (lambda ()
      "decrease spectro-z-scale by 0.01"
      (set! (spectro-z-scale) (max 0.0 (- (spectro-z-scale) 0.01)))))

  (bind-key "KP_Left" 0
    (lambda ()
      "decrease spectro-z-angle by 1.0"
      (set! (spectro-z-angle) (max 0.0 (- (spectro-z-angle) 1.0)))))

  (bind-key "KP_Right" 0
    (lambda ()
      "increase spectro-z-angle by 1.0"
      (set! (spectro-z-angle) (min 359.0 (+ (spectro-z-angle) 1.0)))))

  (bind-key "KP_Up" 4
    (lambda ()
      "increase spectro-x-angle by 1.0"
      (set! (spectro-x-angle) (min 359.0 (+ (spectro-x-angle) 1.0)))))

  (bind-key "KP_Down" 4
    (lambda ()
      "decrease spectro-x-angle by 1.0"
      (set! (spectro-x-angle) (max 0.0 (- (spectro-x-angle) 1.0)))))

  (bind-key "KP_Left" 4
    (lambda ()
      "decrease spectro-y-angle by 1.0"
      (set! (spectro-y-angle) (max 0.0 (- (spectro-y-angle) 1.0)))))

  (bind-key "KP_Right" 4
    (lambda ()
      "increase spectro-y-angle by 1.0"
      (set! (spectro-y-angle) (min 359.0 (+ (spectro-y-angle) 1.0)))))
  )


(if (not (defined? 'in-hz)) (define in-hz hz->radians))

;;; all sample-reader -> sampler

(define copy-sample-reader copy-sampler)
(define free-sample-reader free-sampler)
(define make-mix-sample-reader make-mix-sampler)
(define make-region-sample-reader make-region-sampler)
(define make-sample-reader make-sampler)
(define mix-sample-reader? mix-sampler?)
(define region-sample-reader? region-sampler?)
(define sample-reader-at-end? sampler-at-end?)
(define sample-reader-home sampler-home)
(define sample-reader? sampler?)
(define sample-reader-position sampler-position)

(define show-backtrace
  (make-procedure-with-setter
   (lambda () #t)
   (lambda (va) #t)))

