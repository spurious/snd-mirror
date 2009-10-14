;;; with-sound and friends

(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-ws.scm)

(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm")) ; we need sound-property in with-mixed-sound

(defmacro ws-interrupt? ()
  `(if (c-g?) 
       (throw 'with-sound-interrupt)))



;;; -------- with-sound defaults --------

(define *clm-srate* (default-output-srate))
(define *clm-file-name* "test.snd")
(define *clm-channels* (default-output-chans))
(define *clm-data-format* (default-output-data-format))
(define *clm-header-type* (default-output-header-type))
(define *clm-verbose* #f)
(define *clm-play* #f)
(define *clm-statistics* #f)
(define *clm-reverb* #f)
(define *clm-reverb-channels* 1)
(define *clm-reverb-data* '())
(define *clm-table-size* 512)
(define *clm-file-buffer-size* 65536)
(define *clm-locsig-type* mus-interp-linear)
(define *clm-clipped* #t)
(define *clm-array-print-length* (print-length))
(define *clm-player* #f)          ; default is play-and-wait (takes index of newly created sound, not the sound's file name)
(define *clm-notehook* #f)
(define *clm-with-sound-depth* 0) ; for CM, not otherwise used
(define *clm-default-frequency* 0.0)
(define *clm-safety* 0)           ; obsolete
(define *clm-delete-reverb* #f)   ; should with-sound clean up reverb stream
(define *clm-threads* 4)
(define *clm-output-safety* 0)    ; if 1, assume output buffers will not need to be flushed except at the very end

(define *to-snd* #t)


(define (times->samples beg dur) 
  "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"
  (list (seconds->samples beg) (seconds->samples (+ beg dur))))


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05

(define *definstrument-hook* #f) ; for CM

(defmacro definstrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names '()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names)))
	 (doc (if (string? (car body))
		  (let ((val (car body)))
		    (set! body (cdr body))
		    val)
		  "no help")))
  `(begin 
     (define* (,name ,@targs)
       ,doc
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       ((lambda () ; for inner defines, if any
	  ,@body)))
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))


;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    :key (srate *clm-srate*) 
			          (output *clm-file-name*) 
				  (channels *clm-channels*)
				  (header-type *clm-header-type*)
				  (data-format *clm-data-format*)
				  (comment #f)
				  (verbose *clm-verbose*)
				  (reverb *clm-reverb*)
				  (revfile "test.rev")
				  (reverb-data *clm-reverb-data*)
				  (reverb-channels *clm-reverb-channels*)
				  (continue-old-file #f)
				  (statistics *clm-statistics*)
				  (scaled-to #f)
				  (play *clm-play*)
				  (to-snd *to-snd*)
				  (clipped 'unset)
				  (notehook *clm-notehook*)               ; (with-sound (:notehook (lambda args (display args))) (fm-violin 0 1 440 .1))
				  (scaled-by #f)
				  (ignore-output #f)
				  (thread-output #f)
				  (thread-reverb #f)
				  (output-safety *clm-output-safety*))
  "with-sound-helper is the business portion of the with-sound macro"
  (let* ((old-srate (mus-srate))
	 (old-*output* *output*)
	 (old-*reverb* *reverb*)
	 (old-notehook *clm-notehook*)
	 (old-verbose *clm-verbose*)
	 (old-auto-update-interval (auto-update-interval))
	 (output-1 output)                    ; protect during nesting
	 (output-to-file (string? output))
	 (reverb-1 revfile)
	 (reverb-to-file (and reverb (string? revfile))))

    (if ignore-output
	(begin
	  (set! output-1 *clm-file-name*)
	  (set! output-to-file (string? output-1))))

    (dynamic-wind 

     (lambda () 
       (set! *clm-verbose* verbose)
       (set! *clm-notehook* notehook)
       (set! (clm-table-size) *clm-table-size*)
       (set! (clm-default-frequency) *clm-default-frequency*)
       (set! (mus-file-buffer-size) *clm-file-buffer-size*)
       (set! (locsig-type) *clm-locsig-type*)
       (set! (mus-array-print-length) *clm-array-print-length*)
       (set! (auto-update-interval) 0.0) ; turn it off
       (if (equal? clipped 'unset)
	   (if (and (or scaled-by scaled-to)
		    (member data-format (list mus-bfloat mus-lfloat mus-bdouble mus-ldouble)))
	       (set! (mus-clipping) #f)
	       (set! (mus-clipping) *clm-clipped*))
	   (set! (mus-clipping) clipped))
       (set! (mus-srate) srate))

     (lambda ()
       (if (not thread-output)
	   (begin
	     
	     (if output-to-file
		 (begin
		   (if continue-old-file
		       (begin
			 (set! *output* (continue-sample->file output-1))
			 (set! (mus-srate) (mus-sound-srate output-1)) ; "srate" arg shadows the generic func
			 (let ((ind (find-sound output-1)))
			   (if (sound? ind)
			       (close-sound ind))))
		       (begin
			 (if (file-exists? output-1) 
			     (delete-file output-1))
			 (set! *output* (make-sample->file output-1 channels data-format header-type comment))))
		   (set! (mus-safety *output*) output-safety))
		 (begin
		   (if (not continue-old-file)
		       (if (vct? output-1)
			   (vct-fill! output-1 0.0)
			   (if (sound-data? output-1)
			       (sound-data-fill! output-1 0.0))))
		   (set! *output* output-1)))

	     (if reverb
		 (if reverb-to-file
		     (begin
		       (if continue-old-file
			   (set! *reverb* (continue-sample->file reverb-1))
			   (begin
			     (if (file-exists? reverb-1) 
				 (delete-file reverb-1))
			     (set! *reverb* (make-sample->file reverb-1 reverb-channels data-format header-type))))
		       (set! (mus-safety *reverb*) output-safety))
		     (begin
		       (if (not continue-old-file)
			   (if (vct? reverb-1)
			       (vct-fill! reverb-1 0.0)
			       (if (sound-data? reverb-1)
				   (sound-data-fill! reverb-1 0.0))))
		       (set! *reverb* reverb-1)))))

	   ;; else thread-output
	   (begin
	     (if (file-exists? output-1) 
		 (delete-file output-1))
	     (set! (thread-output) (make-sample->file output-1 channels data-format header-type comment))
	     (set! (mus-safety (thread-output)) output-safety)
	     (if thread-reverb
		 (begin
		   (if (file-exists? reverb-1) 
		       (delete-file reverb-1))
		   (set! (thread-reverb) (make-sample->file reverb-1 reverb-channels data-format header-type))
		   (set! (mus-safety (thread-reverb)) output-safety)))
	     (set! statistics #f)
	     ))

       (let ((start (if statistics (get-internal-real-time)))
	     (flush-reverb #f)
	     (cycles 0)
	     (revmax #f))
	 (catch 'mus-error

		(lambda ()
		  (catch 'with-sound-interrupt
			 thunk
			 (lambda args 
			   (snd-print (format #f "with-sound interrupted: ~{~A~^ ~}" (cdr args)))
			   (set! flush-reverb #t)
			   args)))

		(lambda args
		  ;; hit mus-error, for example:
		  ;;   (with-sound () (fm-violin 0 1 440 .1 :amp-env '(0 0 1 1 1 2 3 0)))

		  ;; user might have listener closed, or no listener so...
		  (display (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))

		  ;; now try to get something to listener, since there may be no stdout
		  (snd-print (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))
		  (set! flush-reverb #t)))
		  
	 (if (and reverb 
		  (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	     (begin
	       (if thread-reverb
		   (if (not (= (mus-safety (thread-output)) 1)) (mus-close (thread-reverb)))
		   (if reverb-to-file
		       (mus-close *reverb*)))
	       (if statistics 
		   (if reverb-to-file
		       (set! revmax (maxamp reverb-1))
		       (if (vct? reverb-1)
			   (set! revmax (vct-peak reverb-1))
			   (if (sound-data? reverb-1)
			       (set! revmax (sound-data-peak reverb-1))))))
	       (if (not thread-reverb)
		   (begin
		     (if reverb-to-file
			 (set! *reverb* (make-file->sample reverb-1)))
		     (apply reverb reverb-data)
		     (if reverb-to-file
			 (mus-close *reverb*))
		     (if (and reverb-to-file *clm-delete-reverb*)
			 (delete-file reverb-1))))))

	 (if thread-output
	     (if (not (= (mus-safety (thread-output)) 1)) (mus-close (thread-output)))
	     (if output-to-file
		 (mus-close *output*)))

	 (let ((snd-output #f)
	       (cur-sync #f))
	   (if statistics
	       (set! cycles (exact->inexact (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

	   (if (and to-snd output-to-file)
	       (let* ((cur (find-sound output-1)))
		 (set! cur-sync (and cur (sync cur)))
		 (if cur 
		     (set! snd-output (update-sound cur))
		     (if (= header-type mus-raw)
			 (set! snd-output (open-raw-sound output-1 channels srate data-format))
			 ;; open-sound here would either ask for raw settings or use possibly irrelevant defaults
			 (set! snd-output (open-sound output-1))))
		 (set! (sync snd-output) #t)))

	   (if statistics
	       ((if (procedure? statistics) ; :statistics (lambda (str) (snd-print str)) -- intended for auto test suite
		    statistics 
		    (if to-snd 
			snd-print 
			display))
		(format #f (if (not (member data-format (list mus-bdouble mus-ldouble)))
			       "~%;~A:~%  maxamp~A:~{ ~,4F~}~%~A  compute time: ~,3F~%"
			       "~%;~A:~%  maxamp~A:~{ ~,8F~}~%~A  compute time: ~,3F~%")
			(if output-to-file
			    output-1
			    (if (vct? output-1) "vct" 
				(if (sound-data? output-1) "sound-data"
				    (if (procedure? output-1) "function" 
					"flush"))))
			(if (or scaled-to scaled-by) 
			    " (before scaling)" 
			    "")
			(if output-to-file
			    (if to-snd
				(maxamp snd-output #t) ; this is a list of chan maxs '(.1 .2)
				(let ((lst (mus-sound-maxamp output-1)))
				  (do ((i 0 (+ i 2)))
				      ((>= i (length lst)))
				    (list-set! lst i (/ (list-ref lst i) (mus-srate))))
				  lst))
			    (if (vct? output-1)
				(list (vct-peak output-1))
				(if (sound-data? output-1)
				    (sound-data-maxamp output-1)
				    0.0)))
			(if revmax (format #f "  rev max: ~,4F~%" revmax) "")
			cycles)))

	   (if (or scaled-to scaled-by)
	       (if output-to-file
		   (let ((scale-output (or snd-output (open-sound output-1))))
		     (if scaled-to
			 (scale-to scaled-to scale-output)
			 (if scaled-by
			     (scale-by scaled-by scale-output)))
		     (save-sound scale-output)
		     (if (not to-snd) 
			 (close-sound scale-output)))
		   (if (vct? output-1)
		       (if scaled-to
			   (let ((pk (vct-peak output-1)))
			     (if (> pk 0.0)
				 (vct-scale! output-1 (/ scaled-to pk))))
			   (vct-scale! output-1 scaled-by))
		       (if (sound-data? output-1)
			   (if scaled-to
			       (let ((pk (sound-data-peak output-1)))
				 (if (> pk 0.0)
				     (sound-data-scale! output-1 (/ scaled-to pk))))
			       (sound-data-scale! output-1 scaled-by))))))

	   (if (and play output-to-file)
	       (if to-snd
		   (if *clm-player*
		       (*clm-player* snd-output)
		       (play-and-wait 0 snd-output))
		   (play output-1)))

	   (if (and to-snd output-to-file)
	       (begin
		 (update-time-graph snd-output)
		 (goto-listener-end)
		 (if (number? cur-sync) (set! (sync snd-output) cur-sync)))))
	 output-1))

     (lambda () 
       (set! *clm-verbose* old-verbose)
       (set! *clm-notehook* old-notehook)
       (set! (auto-update-interval) old-auto-update-interval)
       (if *reverb*
	   (begin
	     (if thread-reverb
		 (if (not (= (mus-safety (thread-reverb)) 1)) (mus-close (thread-reverb)))
		 (mus-close *reverb*))
	     (set! *reverb* old-*reverb*)))
       (if *output*
	   (begin
	     (if thread-output
		 (if (not (= (mus-safety (thread-output)) 1)) (mus-close (thread-output)))
		 (if (mus-output? *output*)
		     (mus-close *output*)))
	     (set! *output* old-*output*)))
       (set! (mus-srate) old-srate)))))


(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))


;;; -------- with-full-sound --------

(defmacro with-full-sound (args . body)
  ;; with-sound but display full sound in Snd window
  `(let ((snd (with-sound-helper (lambda () ,@body) ,@args)))
     (set! (x-bounds *snd-opened-sound*) (list 0.0 (/ (frames *snd-opened-sound*) (srate *snd-opened-sound*))))
     (let ((mx (apply max (maxamp *snd-opened-sound* #t))))
       (if (> mx 1.0)
	 (set! (y-bounds *snd-opened-sound*) (list (- mx) mx))))
     snd))


;;; -------- with-threaded-sound --------

(defmacro with-threaded-sound (args . body)
  (if (and (provided? 'snd-threads)
	   (provided? 's7)
	   (not (= (optimization) 0)))
      (let ((split 
	     (lambda (l n k)
	       (define (split-1 s l n i)
		 (if (null? l)
		     (reverse s)
		     (if (= i n)
			 (split-1 (cons (car l) s) (cdr l) n 1)
			 (split-1 s (cdr l) n (+ i 1)))))
	       (split-1 '() l n (- n k))))

	    (remove-output-and-reverb-args
	     (lambda (lst)
	       (let ((badarg #f)
		     (new-args '()))
		 (for-each 
		  (lambda (arg)
		    (if badarg
			(set! badarg #f)
			(if (not (member arg (list :output :reverb :revfile :reverb-data :reverb-channels)))
			    (set! new-args (cons arg new-args))
			    (set! badarg #t))))
		  lst)
		 (reverse new-args)))))

	(let ((lists '()))
	  (do ((i 0 (+ 1 i)))
	      ((= i *clm-threads*))
	    (set! lists (cons (split body *clm-threads* i) lists)))

	  (let ((new-args (remove-output-and-reverb-args args)))

	    `(with-sound-helper 
	      (lambda ()
		(let ((threads '())
		      (thread-output (make-thread-variable))
		      (thread-reverb (and *reverb* (make-thread-variable)))
		      (mix-lock (make-lock))
		      (main-output *output*)
		      (main-reverb *reverb*))

		  (set! *output* thread-output)
		  (if thread-reverb (set! *reverb* thread-reverb))
		  
		  ,@(map
		     (lambda (expr)
		       `(set! threads (cons (make-thread 
					     (lambda ()
					       (let* ((reverb-tmp (and *reverb* (snd-tempnam)))
						      (tmp (with-sound-helper 
							    (lambda ()
							      ,@expr
							      #f)
							    :thread-output thread-output
							    :thread-reverb thread-reverb
							    :output (snd-tempnam)
							    :revfile reverb-tmp
							    :to-snd #f
							    ,@new-args
							    )))
						 (grab-lock mix-lock)
						 (display (format #f "mix ~S [~D]~%" tmp (mus-safety main-output)))
						 (if (= (mus-safety main-output) 1)
						     (begin
						       (sample->file+ main-output (thread-output))
						       (mus-close (thread-output)))
						     (mus-mix main-output tmp))
						 (if *reverb*
						     (if (= (mus-safety main-output) 1)
							 (begin
							   (sample->file+ main-reverb (thread-reverb))
							   (mus-close (thread-reverb)))
							 (mus-mix main-reverb reverb-tmp)))
						 (release-lock mix-lock)
						 (delete-file tmp))))
					    threads)))
		     lists)
		  
		  (for-each 
		   (lambda (thread) 
		     (join-thread thread))
		   threads)
		  
		  (if main-reverb (set! *reverb* main-reverb))
		  (set! *output* main-output)))
	      
	      ,@args))))
      
      `(with-sound-helper
	(lambda ()
	  ,@body)
	,@args)))




;;; -------- with-temp-sound --------

(defmacro with-temp-sound (args . body)
  `(let ((old-file-name *clm-file-name*)
	 (old-to-snd *to-snd*))
     ;; with-sound but using tempnam for output (can be over-ridden by explicit :output) and does not open result in Snd
     (dynamic-wind
	 (lambda () 
	   (set! *clm-file-name* (snd-tempnam))
	   (set! *to-snd* #f))
	 (lambda ()
	   (with-sound-helper (lambda () ,@body) ,@args)) ; dynamic-wind returns this as its result
	 (lambda ()
	   (set! *to-snd* old-to-snd)
	   (set! *clm-file-name* old-file-name)))))


;;; -------- clm-load --------
;;;
;;; CM wants this to be a function so that it can use apply

(define (clm-load file . args) 
  "(clm-load file . args) loads 'file' in the context of with-sound"
  (apply with-sound-helper (lambda () (load file)) args))


;;; -------- with-mixed-sound --------

(define (with-mixed-sound-mix-info id snd)

  (define (find-if pred l)
    (cond ((null? l) #f)
	  ((pred (car l)) (car l))
	  (else (find-if pred (cdr l)))))

  (let ((all-info (sound-property 'with-mixed-sound-info snd)))
    ;; each entry is '(mx-id beg chans note)
    (find-if (lambda (info)
	       (and (>= id (car info))
		    (< id (+ (car info) (caddr info)))))
	     all-info)))

(defmacro with-mixed-sound (args . body)
  `(let* ((output (with-sound-helper (lambda () #f) ,@args :to-snd #t)) ; pick up args for output
	  (outsnd (find-sound output)))

     (if (sound? outsnd)
	 (let ((mix-info '())
	       (old-sync (sync outsnd)))

	   ;; if multichannel output, make sure cross-chan mixes move together 
	   (if (> (channels outsnd) 1)
	       (begin
		 (reset-hook! mix-release-hook)
		 (add-hook! mix-release-hook
			    (lambda (id samps-moved)
			      (let ((new-pos (+ samps-moved (mix-position id)))
				    (base (sync id)))
				(do ((mx (integer->mix base) (integer->mix (+ (mix->integer mx) 1))))
				    ((or (not (mix? mx))
					 (not (= (sync mx) base))))
				  (set! (mix-position mx) new-pos))
				#t)))))

	   ;; click shows the original note list entry
	   (reset-hook! mix-click-hook)
	   (add-hook! mix-click-hook
		      (lambda (id)
			(let ((info (with-mixed-sound-mix-info id outsnd)))
			  (report-in-minibuffer (format #f "mix ~A: ~A" 
						      id (or (and info
								  (cadddr info))
							     (exact->inexact (/ (mix-position id) (srate outsnd))))))
			  #t))) ; #t -> don't print the mix id in the minibuffer

	   (dynamic-wind
	       (lambda ()
		 (set! (sync outsnd) 0)
		 (do ((chan 0 (+ 1 chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #t)))

	       (lambda ()
		 (for-each
		  (lambda (note)
		    (let* ((snd (with-temp-sound (,@args :ignore-output #t :clipped #f) (eval (append (list (car note) 0.0) (cddr note)) (current-module))))
			   ;; I can't immediately find a way around the "eval" 
			   (beg (floor (* (srate outsnd) (cadr note))))
			   ;; can't use seconds->samples here because the global mus-srate value might not match the local one
			   (mx (car (mix snd beg #t outsnd #f #t #t)))     ; all chans mixed, current output sound, with mixes, with auto-delete
			   (chans (mus-sound-chans snd)))
		      (set! (mix-name mx) (format #f "(~A ~A)" (car note) (cadr note)))
		      (do ((chan 0 (+ 1 chan)))
			  ((= chan chans))
			(set! (sync (integer->mix (+ (mix->integer mx) chan))) (mix->integer mx)))
		      (set! mix-info (cons (list mx beg chans note) mix-info))))
		  ',body)
		 (set! (sound-property 'with-mixed-sound-info outsnd) (reverse mix-info)))

	       (lambda ()
		 (set! (sync outsnd) old-sync)
		 (do ((chan 0 (+ 1 chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #f)
		   ;; fixup y bounds to show waveform unclipped
		   (let ((mx (ceiling (maxamp outsnd chan))))
		     (set! (y-bounds outsnd chan) (list (- mx) mx)))
		   ;; fixup mix tags so they overlap less often
		   (let ((mxs (mixes outsnd chan)))
		     (let ((hgt 0))
		       (for-each
			(lambda (m)
			  (set! (mix-tag-y m) hgt)
			  (set! hgt (modulo (+ hgt 30) 100)))
			mxs)))
		   (update-time-graph outsnd chan))))))
     output))

;(with-mixed-sound () (fm-violin 0 .1 440 .1) (fm-violin 1 .1 660 .1))
;(with-mixed-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (fm-violin 1 .1 660 .1 :degree 45))


(define* (with-mixed-sound->notelist :optional (output-file "temp.scm") snd)
  (let* ((outsnd (or snd (selected-sound) (car (sounds))))
	 (mix-info (sound-property 'with-mixed-sound-info outsnd)))
    (if (not mix-info)
	(throw 'no-such-mixed-sound (list "with-mixed-sound->notelist" outsnd))
	(let ((cur-mixes (mixes outsnd 0)) ; for now assume each mix is multichannel
	      (oput (open-output-file output-file)))
	  (display (format #f "(with-sound (:channels ~D)~%" (channels snd)) oput)
	  (for-each
	   (lambda (id)
	     (let ((info (with-mixed-sound-mix-info id snd)))
	       (if info
		   (let ((call (cadddr info)))
		     (if (not (= (cadr info) (mix-position id)))
			 (display (format #f "  (~A ~,3F~{ ~A~})~%"
					  (car call) 
					  (exact->inexact (/ (mix-position id) (srate snd)))
					  (cddr call)) 
				  oput)
			 (display (format #f "  ~A~%" call) oput)))
		   (report-in-minibuffer "can't find note associated with mix ~A" id))))
	   cur-mixes)
	  (display (format #f ")~%") oput)
	  (close-output-port oput)))))



;;; -------- with-marked-sound --------

(defmacro with-marked-sound (args . body)
  `(let ((old-notehook *clm-notehook*)
	 (mark-list '()))
     (dynamic-wind
	 (lambda ()
	   (set! *clm-notehook* (lambda (name . args)
				  (set! mark-list (cons (append (list name) args) mark-list)))))

	 (lambda ()
	   (let* ((result (with-sound-helper (lambda () ,@body) ,@args))
		  (snd (find-sound result))
		  (old-update (squelch-update snd 0)))
	     (dynamic-wind
		 (lambda ()
		   (set! (squelch-update snd 0) #t))
		 (lambda ()
		   (for-each
		    (lambda (descr)
		      (let ((m (add-mark (floor (* (srate snd) (cadr descr))) snd)))
			(set! (mark-name m) (format #f "~A ~A ~A" (car descr) (cadr descr) (caddr descr)))))
		    mark-list))
		 (lambda ()
		   (set! (squelch-update snd 0) old-update)))
	     result))
		  
	 (lambda ()
	   (set! *clm-notehook* old-notehook)))))

;;; (with-marked-sound () (do ((i 0 (+ 1 i))) ((= i 5)) (fm-violin i .1 440 .1)))



;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(defmacro sound-let (snds . body) 
  `(let ((temp-files '())
	 (old-hook-list (hook->list new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (begin
       (reset-hook! new-sound-hook)
       (add-hook! new-sound-hook (lambda (file)       ; save current sound-let temp file list
				   (if (string? file) ; try to ignore vcts and sound-data objects
				       (set! temp-files (cons file temp-files)))))
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				                      ; if with-sound, embed with-temp-sound
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))              ; else use direct (normal var in the let)
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (and (string? file)          ; is it a file? (might be a vct or sound-data object)
			      (file-exists? file))
			 (delete-file file)))
		   temp-files)
	 (reset-hook! new-sound-hook)                 ; restore old new-sound-hook (should this happen before ,@body?)
	 (if (not (null? old-hook-list))
	     (for-each (lambda (proc)
			 (add-hook! new-sound-hook proc))
		       old-hook-list))
	 val))))                                      ; return body result



;;; -------- Common Music --------

(define* (init-with-sound
	  :key (srate *clm-srate*) 
	  (output *clm-file-name*) 
	  (channels *clm-channels*)
	  (header-type *clm-header-type*)
	  (data-format *clm-data-format*)
	  (comment #f)
	  ;(verbose *clm-verbose*) ; why is this commented out?
	  (reverb *clm-reverb*)
	  (revfile "test.rev")
	  (reverb-data *clm-reverb-data*)
	  (reverb-channels *clm-reverb-channels*)
	  (continue-old-file #f)
	  (statistics *clm-statistics*)
	  (scaled-to #f)
	  (play *clm-play*)
	  (to-snd *to-snd*)
	  (scaled-by #f))
  "(init-with-sound . args) is the first half of with-sound; it sets up the CLM output choices, reverb, etc. Use \
finish-with-sound to complete the process."
  (let ((old-srate (mus-srate))
	(start (if statistics (get-internal-real-time)))
	(output-to-file (string? output))
	(reverb-to-file (and reverb (string? revfile))))
    (if output-to-file
	(if continue-old-file
	    (begin
	      (set! *output* (continue-sample->file output))
	      (set! (mus-srate) (mus-sound-srate output))
	      (let ((ind (find-sound output)))
		(if (sound? ind)
		    (close-sound ind))))
	    (begin
	      (if (file-exists? output) 
		  (delete-file output))
	      (set! *output* (make-sample->file output channels data-format header-type comment))))
	(begin
	  (if (not continue-old-file)
	      (if (vct? output)
		  (vct-fill! output 0.0)
		  (sound-data-fill! output 0.0)))
	  (set! *output* output)))

    (if reverb
	(if reverb-to-file
	    (if continue-old-file
		(set! *reverb* (continue-sample->file revfile))
		(begin
		  (if (file-exists? revfile) 
		      (delete-file revfile))
		  (set! *reverb* (make-sample->file revfile reverb-channels data-format header-type))))
	    (begin
	      (if (not continue-old-file)
		  (if (vct? revfile)
		      (vct-fill! revfile 0.0)
		      (sound-data-fill! revfile 0.0)))
	      (set! *reverb* revfile))))

    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  to-snd
	  scaled-to
	  scaled-by
	  play
	  reverb-data
	  start)))

(define (finish-with-sound wsd)
  "(finish-with-sound wsd) closes the notelist process started by init-with-sound"
  (if (eq? (car wsd) 'with-sound-data)
      (let ((cycles 0)
	    (output (list-ref wsd 1))
	    (reverb (list-ref wsd 2))
	    (revfile (list-ref wsd 3))
	    (old-srate (list-ref wsd 4))
	    (statistics (list-ref wsd 5))
	    (to-snd (list-ref wsd 6))
	    (scaled-to (list-ref wsd 7))
	    (scaled-by (list-ref wsd 8))
	    (play (list-ref wsd 9))
	    (reverb-data (list-ref wsd 10))
	    (start (list-ref wsd 11)))

	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (if (string? revfile)
		  (set! *reverb* (make-file->sample revfile))
		  (set! *reverb* revfile))
	      (apply reverb reverb-data)
	      (mus-close *reverb*)))
	(if (mus-output? *output*)
	    (mus-close *output*))

	(if statistics
	    (set! cycles (/ (- (get-internal-real-time) start) 100)))
	(if (and to-snd (string? output))
	    (let ((snd-output (open-sound output)))
	      (set! (sync snd-output) #t)
	      (if statistics
		  (snd-print 
		   (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			   output
			   (maxamp snd-output #t)
			   cycles)))
		 (if (or scaled-to scaled-by)
		     (begin
		       (if scaled-to
			   (scale-to scaled-to snd-output)
			   (if scaled-by
			       (scale-by scaled-by snd-output)))
		       (save-sound snd-output)))
	      (if play (play-and-wait 0 snd-output))
	      (update-time-graph snd-output)))
	(set! (mus-srate) old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))


(define wsdat-play ; for cm
  (make-procedure-with-setter
   (lambda (w)
     "accessor for play field of init-with-sound struct"
     (list-ref w 9))
   (lambda (w val)
     (list-set! w 9 val))))


;;; -------- with-sound save state --------

(define (ws-save-state filename)
  "(ws-save-state filename) is an after-save-state-hook function that saves the current with-sound global settings"

  (define (open-appending filename)
    (if (provided? 'snd-guile)
	(open filename (logior O_RDWR O_APPEND))
	(open-output-file filename "a")))

  (define (close-appending fd)
    (if (provided? 'snd-guile)
	(close fd)
	(close-output-port fd)))

  (let ((fd (open-appending filename)))
    ;; open in Guile throws 'system-error (I think) if anything goes wrong
    ;; fd is a Scheme port at this point (not an integer), so we can use format etc
    ;; should the save-state file load this file if it hasn't been loaded? (what path?)
    (format fd "~%~%;;; from ws.scm~%")
    (format fd "(if (defined? '*clm-srate*)~%")
    (format fd "    (begin~%")
    (format fd "      (set! *clm-srate* ~A)~%" *clm-srate*)
    (format fd "      (set! *clm-file-name* ~S)~%" *clm-file-name*)
    (format fd "      (set! *clm-channels* ~A)~%" *clm-channels*)
    (format fd "      (set! *clm-data-format* ~A)~%" (mus-data-format->string *clm-data-format*))
    (format fd "      (set! *clm-header-type* ~A)))~%" (mus-header-type->string *clm-header-type*))
    (close-appending fd)))

(add-hook! after-save-state-hook ws-save-state)


;;; -------- with-mix --------
;;;
;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body
;;;
;;; (with-sound () 
;;;   (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
;;;                              (fm-violin 1 2 660 .1))
;;; ...)

(define (with-mix-find-file-with-extensions file extensions)
  "(with-mix-find-file-with-extensions file extensions) helps the with-mix macro find checkpoint files"
  (if (file-exists? file)
      file
      (call-with-exit
       (lambda (found-one)
	 (for-each
	  (lambda (ext)
	    (let ((new-file (string-append file "." ext)))
	      (if (file-exists? new-file)
		  (found-one new-file))))
	  extensions)
	 #f))))

(define (with-mix-file-extension file default)
  "(with-mix-file-extension file default) is a helper function for the with-mix macro"
  (let ((len (string-length file)))
    (call-with-exit
     (lambda (ok)
       (do ((i (- len 1) (- i 1)))
	   ((= i 0))
	 (if (char=? (string-ref file i) #\.)
	     (ok (substring file (+ 1 i) len))))
       default))))

(defmacro with-mix (options ur-chkpt-file ur-beg . body)
  `(let ((chkpt-file ,ur-chkpt-file)
	 (beg-1 ,ur-beg))
     (if (not (list? ',options))
	 (throw 'with-sound-interrupt (format #f "with-mix options list (arg 1) is ~A?~%;" ',options))
	 (if (not (string? chkpt-file))
	     (throw 'with-sound-interrupt (format #f "with-mix file (arg 2) is ~A?~%;" ,ur-chkpt-file))
	     (if (not (number? beg-1))
		 (throw 'with-sound-interrupt (format #f "with-mix begin time (arg 3) for ~S = ~A?~%;" chkpt-file beg-1))
		 (let ((beg (round (* (mus-srate) beg-1))))
		   (if (null? ',body)
		       (mus-mix *output* chkpt-file beg)
		       (let* ((call-str (object->string ',body))
			      (option-str (object->string ',options))
			      (sndf (with-mix-find-file-with-extensions chkpt-file (list (with-mix-file-extension *clm-file-name* "snd") "snd")))
			      (revf (and sndf *reverb* (with-mix-find-file-with-extensions chkpt-file (list "rev"))))
			      (mix-values (and sndf
					       (or (not *reverb*)
						   revf)
					       (let ((comment (mus-sound-comment sndf)))
						 (and (string? comment)
						      (catch #t
							     (lambda ()
							       (eval-string comment))
							     (lambda args #f))))))) ; any error means we lost
			 (if (and sndf
				  (or (not *reverb*)
				      revf)
				  (list? mix-values)
				  (= (length mix-values) 2)
				  (string? (car mix-values))
				  (string? (cadr mix-values))
				  (string=? (car mix-values) option-str)
				  (string=? (cadr mix-values) call-str))
			     (begin
			       (if *clm-verbose* (snd-print (format #f "mix ~S at ~,3F~%" sndf beg)))
			       (mus-mix *output* sndf beg)
			       (if revf (mus-mix *reverb* revf beg)))
			     ;; else recompute
			     (let ((old-to-snd *to-snd*))
			       (set! *to-snd* #f)
			       (if *clm-verbose* (snd-print (format #f "remake ~S at ~,3F~%" chkpt-file beg)))
			       (let ((new-sound 
				      (apply with-sound-helper 
					     (lambda () ,@body) 
					     (append (list :output 
							   (string-append chkpt-file "." (with-mix-file-extension *clm-file-name* "snd")))
						     (list :comment 
							   (format #f "(begin~%;; written ~A (Snd: ~A)~%(list ~S ~S))~%"
								   (strftime "%a %d-%b-%Y %H:%M %Z" (localtime (current-time)))
								   (snd-version)
								   option-str
								   call-str))
						     (if (and (> (channels *output*) 1)
							      (not (member :channels ',options)))
							 (list :channels (channels *output*))
							 '())
						     ',options))))
				 (set! *to-snd* old-to-snd)
				 (mus-mix *output* new-sound beg)
				 (if revf (mus-mix *reverb* revf beg)))))))))))))


(defmacro def-optkey-instrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names '()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names))))
  `(begin 
     (def-optkey-fun (,name ,@targs)
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       ((lambda () ; for inner defines, if any
	  ,@body)))
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))


(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 (/ 57 12)))) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                             ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0)))
    (lambda* (pitch :optional pythagorean)          ; pitch can be pitch name or actual frequency
      "(->frequency pitch :optional pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."
      (if (symbol? pitch)
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (base-char (string-ref name 0))
		 (sign-char (and (> (string-length name) 1)
				 (not (char-numeric? (string-ref name 1)))
				 (not (char=? (string-ref name 1) #\n))
				 (string-ref name 1)))
		 (octave-char (if (and (> (string-length name) 1)
				       (char-numeric? (string-ref name 1))) 
				  (string-ref name 1)
				  (if (and (> (string-length name) 2) 
					   (char-numeric? (string-ref name 2)))
				      (string-ref name 2)
				      #f)))
		 (base (modulo (+ 5 (- (char->integer base-char) (char->integer #\a))) 7)) ; c-based (diatonic) octaves
		 (sign (if (not sign-char) 0 (if (char=? sign-char #\f) -1 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (+ sign (case base ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (if pythagorean
		(* main-pitch (expt 2 octave) (vector-ref ratios base-pitch))
		(* main-pitch (expt 2.0 (/ et-pitch 12)))))
	  pitch))))

(define (->sample beg)
  "(->sample time-in-seconds) -> time-in-samples"
  (round (* (if (not (null? (sounds))) (srate) (mus-srate)) beg)))



;;; -------- def-clm-struct --------

;;;  :(def-clm-struct (osc :make-wrapper (lambda (gen) (set! (osc-freq gen) (hz->radians (osc-freq gen))) gen)) freq phase)
;;;  #<unspecified>
;;;  :(define hi (make-osc 440.0 0.0))
;;;  #<unspecified>
;;;  :hi
;;;  (osc 0.125378749798983 0.0)

;;; besides setting up the list accessors, the make function, and the type predicate, this
;;;   calls add-clm-field to tell run the type of each list element (only actually needed if
;;;   there are different types in use)
;;;
;;; see defgenerator in generators.scm for an extension that adds various methods such as mus-describe

(defmacro def-clm-struct (struct-name . fields)
  (let* ((name (if (list? struct-name) (car struct-name) struct-name))
	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (list-ref struct-name 1) :make-wrapper)
				    (list-ref struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (list-ref struct-name 3) :make-wrapper)
				    (list-ref struct-name 4))))
		      (lambda (gen) gen)))
	 (methods (and (list? struct-name)
		       (or (and (> (length struct-name) 2)
				(equal? (list-ref struct-name 1) :methods)
				(list-ref struct-name 2))
			   (and (= (length struct-name) 5)
				(equal? (list-ref struct-name 3) :methods)
				(list-ref struct-name 4)))))
	 (sname (if (string? name) name (symbol->string name)))
	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))
	 (field-types (map (lambda (n)
			     (if (and (list? n) (cadr n) (eq? (cadr n) :type)) 
				 (snd-error (format #f ":type indication for def-clm-struct (~A) field (~A) should be after the default value" name n)))
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (list-ref n 2) :type))
				 (list-ref n 3)
				 (if (and (list? n)
					  (= (length n) 2))
				     (if (number? (cadr n))
					 (if (exact? (cadr n))
					     'int
					     'float)
					 (if (string? (cadr n))
					     'string
					     (if (char? (cadr n))
						 'char
						 (if (or (equal? (cadr n) #t)
							 (equal? (cadr n) #f))
						     'boolean
						     'float))))
				     'float)))
			   fields)))
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   "clm struct type check"
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))
       (define ,(string->symbol (string-append sname "-methods"))
	 (lambda ()
	   "clm struct local method list accessor"
	   ,methods))
       (def-optkey-fun (,(string->symbol (string-append "make-" sname))
		        ,@(map (lambda (n)
				(if (and (list? n)
					 (>= (length n) 2))
				    (list (car n) (cadr n))
				    (list n 0.0)))
			      fields))
	 (,wrapper (if (list? ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)
			     ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)))))
       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   "clm struct field accessor"
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field sname (string-append sname "-" n) ctr type)
		    (set! ctr (+ 1 ctr))
		    val)))
	      field-names field-types))))


;;; ----------------
;;;
;;; display all the globals that might affect with-sound unexpectedly

(define (clm-display-globals)

  (format #f ";CLM globals:~%;  *clm-srate*: ~A (default: ~A, mus-srate: ~A)~%;  *clm-file-name*: ~A~%;  *clm-channels: ~A (default: ~A)~%;  *clm-data-format*: ~A (default: ~A)~%;  *clm-header-type*: ~A (default: ~A)~%;  *clm-reverb-channels*: ~A, *clm-reverb-data*: ~A~%;  *clm-table-size*: ~A~%;  *clm-file-buffer-size*: ~A (~A)~%;  *clm-locsig-type*: ~A~%;  *clm-array-print-length*: ~A (~A)~%;  *clm-notehook*: ~A~%;  *clm-default-frequency*: ~A~%;  *clm-clipped*: ~A, mus-clipping: ~A, mus-prescaler: ~A~%;  *clm-threads*: ~A~%;  *clm-output-safety*: ~A~%~%"

	  *clm-srate* (default-output-srate) (mus-srate)
	  *clm-file-name*
	  *clm-channels* (default-output-chans)
	  (mus-data-format->string *clm-data-format*) (mus-data-format->string (default-output-data-format))
	  (mus-header-type->string *clm-header-type*) (mus-header-type->string (default-output-header-type))
	  *clm-reverb-channels* *clm-reverb-data*
	  *clm-table-size*
	  *clm-file-buffer-size* (mus-file-buffer-size)
	  *clm-locsig-type*
	  *clm-array-print-length* (print-length)
	  *clm-notehook*
	  *clm-default-frequency*
	  *clm-clipped* (mus-clipping)
	  (mus-prescaler)
	  *clm-threads*
	  *clm-output-safety*))




;;; -------- with-threaded-channels

(define (with-threaded-channels snd func)
  (let ((chns (channels snd)))
    (if (and (provided? 'snd-threads)
	     (provided? 's7)
	     (> chns 1))
	
	(dynamic-wind

	 (lambda ()
	   (do ((chn 0 (+ 1 chn)))
	       ((= chn chns))
	     (set! (squelch-update snd chn) #t)))
	   
	 (lambda ()
	   (let ((threads '()))
	     (do ((chn 0 (+ 1 chn)))
		 ((= chn chns))
	       (let ((lchn chn))
		 (set! threads (cons (make-thread (lambda () 
						    (func snd lchn))) 
				     threads))))
	     (for-each 
	      (lambda (expr) 
		(join-thread expr))
	      threads)))

	 (lambda ()
	   (do ((chn 0 (+ 1 chn)))
	       ((= chn chns))
	     (set! (squelch-update snd chn) #f))))

	;; else do it without threads
	(do ((chn 0 (+ 1 chn)))
	    ((= chn chns))
	  (func snd chn)))))

