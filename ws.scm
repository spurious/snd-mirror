(use-modules (ice-9 optargs) (ice-9 format))

(define *srate* (default-output-srate))
(define *file-name* "test.snd")
(define *channels* (default-output-chans))
(define *explode* #f)                           ;with-mix-tags
(define *data-format* (default-output-format))
(define *header-type* (default-output-type))

(define *reverb* #f) ; these are sample->file (outa) gens
(define *output* #f)

(define* (with-sound-helper thunk 
			    #:key (srate *srate*) 
			          (output *file-name*) 
				  (channels *channels*)
				  (explode *explode*)
				  (header-type *header-type*)
				  (data-format *data-format*)
				  (comment #f)
				  (reverb #f)
				  (revfile #f)
				  (reverb-data #f)
				  (continue-old-file #f)
				  (statistics #f)
				  (scaled-to #f)
				  (play #f)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-tags (with-mix-tags)))
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate)
       (set! (with-mix-tags) explode))

     (lambda ()

       (if (file-exists? output) (delete-file output))
       (if (and reverb (file-exists? (or revfile "test.rev"))) (delete-file (or revfile "test.rev")))
       (set! *output* (make-sample->file output channels data-format header-type comment))
       (if reverb (set! *reverb* (make-sample->file (or revfile "test.rev") 1 data-format header-type)))

       (let ((start (if statistics (get-internal-real-time))))
	 (thunk)
	 (if reverb
	     (begin
	       (mus-close *reverb*)
	       (set! *reverb* (make-file->sample (or revfile "test.rev")))
	       (reverb)))
	 (mus-close *output*)
	 (let ((snd-output (open-sound output)))
	   (set! (sync snd-output) #t)
	   (if statistics
	       (snd-print 
		(format #f "~A:~%  maxamp: ~A,~%  compute time: ~A"
			output
			(maxamp snd-output #t)
			(/ (- (get-internal-real-time) start) 100))))
	   (if scaled-to
	       (scale-to scaled-to snd-output)
	       (if scaled-by
		   (scale-by scaled-by snd-output)))
	   (update-time-graph snd-output))))

     (lambda () 
       (set! (with-mix-tags) old-tags)
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))
