;;; ws_s.scm -- simple with-sound for stand-alone execution (w/o Snd)
;; it's mostly taken from snd/ws.scm

;; This file is part of Sndins.

(use-modules (ice-9 format) (ice-9 optargs))

(if (not (provided? "sndlib")) (load-extension "libsndlib" "init_sndlib"))
(if (not (provided? "sndins")) (load-extension "libsndins" "init_sndins"))

(define *clm-srate* 22050)
(define *clm-file-name* "test.snd")
(define *clm-channels* 1)
(define *clm-data-format* mus-bshort)
(define *clm-header-type* mus-next)
(define *clm-verbose* #f)
(define *to-snd* #t)

(define *reverb* #f) ; these are sample->file (outa) gens
(define *output* #f)
(define *clm-delete-reverb* #f) ; should with-sound clean up reverb stream

(define *clm-play* #f)
(define *clm-statistics* #f)
(define *clm-reverb-channels* 1)
(define *clm-reverb* jc-reverb)
(define *clm-reverb-data* '())

(if (not (defined? 'definstrument)) (define definstrument define*))

(define* (with-sound-helper thunk 
			    #:key (srate *clm-srate*) 
			          (output *clm-file-name*) 
				  (channels *clm-channels*)
				  (header-type *clm-header-type*)
				  (data-format *clm-data-format*)
				  (comment #f)
				  (reverb *clm-reverb*)
				  (revfile "test.rev")
				  (reverb-data *clm-reverb-data*)
				  (continue-old-file #f)
				  (statistics *clm-statistics*)
				  (scaled-to #f)
				  (reverb-channels *clm-reverb-channels*)
				  (play *clm-play*)
				  (to-snd *to-snd*)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-output *output*)
	(old-reverb *reverb*)
	(output-1 output)) ; protect during nesting
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate))

     (lambda ()

       (if continue-old-file
	   (begin
	     (set! *output* (continue-sample->file output-1))
	     (set! (mus-srate) (mus-sound-srate output-1))
	     (if reverb (set! *reverb* (continue-sample->file revfile)))
	     (let ((ind (find-sound output-1)))
	       (if ind (close-sound ind))))
	   (begin
	     (if (file-exists? output-1) (delete-file output-1))
	     (if (and reverb (file-exists? revfile)) (delete-file revfile))
	     (set! *output* (make-sample->file output-1 channels data-format header-type comment))
	     (if reverb (set! *reverb* (make-sample->file revfile reverb-channels data-format header-type)))))
       (let ((start (if statistics (get-internal-real-time)))
	     (intp #f)
	     (cycles 0))
	 (catch 'interrupted
		thunk
		(lambda args 
		  (begin
		    (set! intp #t) 
		    (format #t "interrupted!") 
		    args)))
	 (if (and reverb (not intp))
	     (begin
	       (mus-close *reverb*)
	       (set! *reverb* (make-file->sample revfile))
	       (apply reverb reverb-data)
	       (mus-close *reverb*)
	       (if *clm-delete-reverb* (delete-file revfile))))
	 (mus-close *output*)
	 (if statistics
	     (set! cycles (/ (- (get-internal-real-time) start) 100)))
	 (if to-snd
	     (let ((max-amp (mus-sound-maxamp output-1)))
	       (if statistics
		   (format #t "~A:~%  maxamp: ~A~%  compute time: ~A~%" output-1 max-amp cycles))
	       (if play (system (format #f "sndplay ~A" output-1)))))
	 output-1))

     (lambda () 
       (if *reverb*
	   (begin
	     (mus-close *reverb*)
	     (set! *reverb* old-reverb)))
       (if *output*
	   (begin
	     (mus-close *output*)
	     (set! *output* old-output)))
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))

;; ws_s.scm ends here
