;;; backwards compatibility for snd 7

(define free-mix-sample-reader free-sample-reader)
(define free-track-sample-reader free-sample-reader)
(define inspect-sample-reader describe-sample-reader)

(define enved-exp?
  (make-procedure-with-setter
   (lambda ()
     (= (enved-style enved-exponential)))
   (lambda (val)
     (set! (enved-style) (if val enved-exponential enved-linear)))))
