;;; backwards compatibility for Snd-6

(define graph-lisp?                    lisp-graph?)
(define graph-transform?               transform-graph?)
(define graph-time?                    time-graph?)

(define graph-time-once                graph-once)
(define graph-transform-once           graph-once)
(define graph-time-as-wavogram         graph-as-wavogram)
(define graph-transform-as-sonogram    graph-as-sonogram)
(define graph-transform-as-spectrogram graph-as-spectrogram)

(define dont-normalize-transform       dont-normalize)
(define normalize-transform-by-channel normalize-by-channel)
(define normalize-transform-by-sound   normalize-by-sound)
(define normalize-transform-globally   normalize-globally)

(define set-oss-buffers mus-audio-set-oss-buffers)

(define (max-sounds) (apply max (sounds)))

(define (transform-samples snd chn) (vct->vector (transform-samples->vct snd chn)))
(define (region-samples samp samps reg chn) (vct->vector (region-samples->vct samp samps reg chn)))
