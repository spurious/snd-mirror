(use-modules (ice-9 optargs) (ice-9 format))

;;; -------- with-sound for Snd!
;;;
;;; this is just a bare beginning, but it's the basic idea...
;;;
;;; in Common Lisp this is essentially
;;;    (defmacro with-sound ((&key (srate 22050) ...) &body body) (let (...) ,.body))
;;; so that a call looks like 
;;;    (with-sound (:srate 44100) (fm-violin 0 1 440 .1))

;;; here's a better version courtesy of Kalle Olavi Niemitalo
;;; but it doesn't seem to work in Guile 1.4 (it needs 1.4.1)

(define *snd-srate* (default-output-srate))
(define *snd-file-name* "test.snd")
(define *snd-channels* (default-output-chans))
(define *snd-explode* #f)                           ;with-mix-tags
(define *snd-data-format* (default-output-format))
(define *snd-header-type* (default-output-type))

(define *snd-reverb* #f)
(define *snd-output* #f)

(define* (with-sound-helper thunk 
			    #:key (srate *snd-srate*) 
			          (output *snd-file-name*) 
				  (channels *snd-channels*)
				  (explode *snd-explode*)
				  (header-type *snd-header-type*)
				  (data-format *snd-data-format*)
				  (comment #f)
				  (reverb #f)
				  (revfile #f)
				  (reverb-data #f)
				  (continue-old-file #f)
				  (statistics #f)
				  (scaled-to #f)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-tags (with-mix-tags)))
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate)
       (set! (with-mix-tags) explode))

     (lambda ()

	 (if (not continue-old-file)
	     (begin
	       (if (find-sound output) 
		   (close-sound (find-sound output)))
	       (set! *snd-output* (new-sound output header-type data-format srate channels comment))
	       (if reverb (set! *snd-reverb* (new-sound (or revfile "test.rev") header-type data-format srate 1))))
	     (set! *snd-output* (find-sound output)))
	 (if (sound? *snd-output*)
	     (let ((start (if statistics (get-internal-real-time))))
	       (thunk)
	       (if reverb
		   (begin
		     (reverb *snd-reverb* *snd-output*)
		     (close-sound *snd-reverb*)))

	       (if statistics
		   (snd-print 
		    (format #f "~A: maxamp: ~A, time: ~A"
			    output
			    (maxamp *snd-output* #t)
			    (/ (- (get-internal-real-time) start) 100))))
	       (if scaled-to
		   (scale-to scaled-to *snd-output*)
		   (if scaled-by
		       (scale-by scaled-by *snd-output*)))

	       (update-graph *snd-output*)
	       )))

     (lambda () 
       (set! (with-mix-tags) old-tags)
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))
