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
   (lambda () 
     "dummy accessor for obsolete vu-font"
     "not-a-font")
   (lambda (val) 
     "not-a-font")))

(define vu-font-size
  (make-procedure-with-setter
   (lambda () 
     "dummy accessor for obsolete vu-font-size"
     1.0)
   (lambda (val) 
     1.0)))


(define* (samples->sound-data :optional (beg 0) num snd chn obj pos (sd-chan 0))
  (vct->sound-data 
   (channel->vct beg num snd chn pos) 
   (or obj (make-sound-data 1 (or num (frames snd chn))))
   sd-chan))



(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))

(def-optkey-fun (open-sound-file file channels srate comment header-type)
  (mus-sound-open-output (or file (if (little-endian?) "test.wav" "test.snd"))
			 (or srate 22050)
			 (or channels 1)
			 (if (little-endian?) mus-lfloat mus-bfloat) 
			 (or header-type (if (little-endian?) mus-riff mus-next))
			 (or comment "")))

(define close-sound-file mus-sound-close-output)

(define (vct->sound-file fd v samps)
  (mus-sound-write fd 0 (1- samps) 1 (vct->sound-data v)))

