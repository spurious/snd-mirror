;;; backwards compatibility for snd 8

(provide 'snd-snd8.scm)

(define make-ppolar make-two-pole)
(define make-zpolar make-two-zero)

(define make-average make-moving-average)
(define average moving-average)
(define average? moving-average?)

;(define windowed-maxamp moving-max)
;(define make-windowed-maxamp make-moving-max)

(define vu-font
  (make-procedure-with-setter
   (lambda () "not-a-font")
   (lambda (val) "not-a-font")))

(define vu-font-size
  (make-procedure-with-setter
   (lambda () 1.0)
   (lambda (val) 1.0)))


;;; TODO: sample->sound-data rb/fs changes

(define* (samples->sound-data :optional (beg 0) (num #f) (snd #f) (chn #f) (obj #f) (pos #f) (sd-chan 0))
  (vct->sound-data 
   (channel->vct beg num snd chn pos) 
   (or obj (make-sound-data 1 (or num (frames snd chn))))
   sd-chan))

