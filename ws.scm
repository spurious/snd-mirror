;;; with-sound and friends

(provide 'snd-ws.scm)

;;; -------- with-sound defaults --------

(define *clm-srate*             (default-output-srate))
(define *clm-file-name*         "test.snd")
(define *clm-channels*          (default-output-chans))
(define *clm-data-format*       (default-output-data-format))
(define *clm-header-type*       (default-output-header-type))
(define *clm-verbose*           #f)
(define *clm-play*              #f)
(define *clm-statistics*        #f)
(define *clm-reverb*            #f)
(define *clm-reverb-channels*   1)
(define *clm-reverb-data*       ())
(define *clm-table-size*        512)
(define *clm-file-buffer-size*  65536)
(define *clm-locsig-type*       mus-interp-linear)
(define *clm-clipped*           #t)
(define *clm-array-print-length* (print-length))
(define *clm-player*            #f)
(define *clm-notehook*          #f)
(define *clm-with-sound-depth*  0)           ; for CM, not otherwise used
(define *clm-default-frequency* 0.0)
(define *clm-delete-reverb*     #f)          ; should with-sound clean up reverb stream
(define *clm-threads*           4)           ; obsolete, unused

(define *to-snd*                #t)
(define *default-player* (lambda (s) (play s :wait #t))) ; we need to preserve "play" because it is used as a keyword argument in with-sound


(define (times->samples beg dur) 
  "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"
  (list (seconds->samples beg) (seconds->samples (+ beg dur))))


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05

(define *definstrument-hook* #f) ; for CM

(defmacro definstrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names ()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a)) ; redundant :optional or something
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names))))
  `(begin 
     (define* (,name ,@targs)
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       ,@body)
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))


;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    (srate *clm-srate*) 
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
			    (ignore-output #f))
  "with-sound-helper is the business portion of the with-sound macro"
  (let ((old-srate (mus-srate))
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
		   (set! *output* (make-sample->file output-1 channels data-format header-type comment)))))
	   (begin
	     (if (not continue-old-file)
		 (if (or (vct? output-1)
			 (sound-data? output-1)
			 (vector? output-1))
		     (fill! output-1 0.0)))
	     (set! *output* output-1)))
       
       (if reverb
	   (if reverb-to-file
	       (begin
		 (if continue-old-file
		     (set! *reverb* (continue-sample->file reverb-1))
		     (begin
		       (if (file-exists? reverb-1) 
			   (delete-file reverb-1))
		       (set! *reverb* (make-sample->file reverb-1 
							 reverb-channels 
							 (if (mus-header-writable header-type mus-ldouble)
							     mus-ldouble
							     data-format)
							 header-type)))))
	       (begin
		 (if (not continue-old-file)
		     (if (or (vct? reverb-1)
			     (sound-data? reverb-1)
			     (vector? reverb-1))
			 (fill! reverb-1 0.0)))
		 (set! *reverb* reverb-1))))

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
		  (format #t ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args))

		  ;; now try to get something to listener, since there may be no stdout
		  (snd-print (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))
		  (set! flush-reverb #t)))
		  
	 (if (and reverb 
		  (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	     (begin
	       (if reverb-to-file
		   (mus-close *reverb*))
	       (if statistics 
		   (if (or reverb-to-file
			   (vct? reverb-1)
			   (sound-data? reverb-1)
			   (vector? reverb-1))
		       (set! revmax (maxamp reverb-1))))
	       (if reverb-to-file
		   (set! *reverb* (make-file->sample reverb-1)))
	       (apply reverb reverb-data)                                   ; here is the reverb call(!)
	       (if reverb-to-file
		   (mus-close *reverb*))
	       (if (and reverb-to-file *clm-delete-reverb*)
		   (delete-file reverb-1))))

	 (if output-to-file
	     (mus-close *output*))

	 (let ((snd-output #f)
	       (cur-sync #f))
	   (if statistics
	       (set! cycles (/ (* 1.0 (- (get-internal-real-time) start)) internal-time-units-per-second)))

	   (if (and to-snd output-to-file)
	       (let ((cur (find-sound output-1)))
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
					(if (vector? output-1) "vector"
					    "flush")))))
			(if (or scaled-to scaled-by) 
			    " (before scaling)" 
			    "")
			(if output-to-file
			    (if to-snd
				(maxamp snd-output #t) ; this is a list of chan maxs '(.1 .2)
				(let ((lst (mus-sound-maxamp output-1)))
				  (do ((i 0 (+ i 2)))
				      ((>= i (length lst)))
				    (set! (lst i) (/ (lst i) (mus-srate))))
				  lst))
			    (if (vct? output-1)
				(list (vct-peak output-1))
				(if (sound-data? output-1)
				    (sound-data-maxamp output-1)
				    (if (vector? output-1)
					(list (maxamp output-1))
					0.0))))
			(if revmax (format #f "  rev max: ~,4F~%" revmax) "")
			cycles)))

	   (if (or scaled-to scaled-by)
	       (if output-to-file
		   (let* ((scale-output (or snd-output (open-sound output-1)))
			  (old-sync (sync scale-output)))
		     (set! (sync scale-output) 0)          ; make sure scaling doesn't follow sync
		     (if scaled-to
			 (scale-to scaled-to scale-output)
			 (scale-by scaled-by scale-output))
		     (set! (sync scale-output) old-sync)
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
			       (sound-data-scale! output-1 scaled-by))
			   (if (vector? output-1)
			       (if scaled-to
				   (let ((pk (maxamp output-1)))
				     (if (> pk 0.0)
					 (let ((scl (/ scaled-to pk)))
					   (do ((i 0 (+ i 1)))
					       ((= i (length output-1)))
					     (set! (output-1 i) (* scl (output-1 i)))))))
				   (do ((i 0 (+ i 1)))
				       ((= i (length output-1)))
				     (set! (output-1 i) (* scaled-by (output-1 i))))))))))

	   (if (and play output-to-file)
	       (if to-snd
		   (if *clm-player*
		       (*clm-player* snd-output)
		       (*default-player* snd-output))
		   (*default-player* output-1))) ; this was (play output-1) which could not have worked?!

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
	     (mus-close *reverb*)
	     (set! *reverb* old-*reverb*)))
       (if *output*
	   (begin
	     (if (mus-output? *output*)
		 (mus-close *output*))
	     (set! *output* old-*output*)))
       (set! (mus-srate) old-srate)))))


(define-macro (with-sound args . body)
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
	 (let ((mix-info ())
	       (old-sync (sync outsnd)))

	   ;; if multichannel output, make sure cross-chan mixes move together 
	   (if (> (channels outsnd) 1)
	       (begin
		 (set! (hook-functions mix-release-hook) ())
		 (hook-push mix-release-hook
			    (lambda (hook)
			      (let ((id (hook 'id))
				    (samps-moved (hook 'samples)))
				(let ((new-pos (+ samps-moved (mix-position id)))
				      (base (sync id)))
				  (do ((mx (integer->mix base) (integer->mix (+ (mix->integer mx) 1))))
				      ((or (not (mix? mx))
					   (not (= (sync mx) base))))
				    (set! (mix-position mx) new-pos))
				  (set! (hook 'result) #t)))))))

	   ;; click shows the original note list entry
	   (set! (hook-functions mix-click-hook) ())
	   (hook-push mix-click-hook
		      (lambda (hook)
			(let ((info (with-mixed-sound-mix-info (hook 'id) outsnd)))
			  (status-report (format #f "mix ~A: ~A" 
						 (hook 'id) (or (and info
							     (cadddr info))
							(/ (mix-position id) (* 1.0 (srate outsnd))))))
			  (set! (hook 'result) #t)))) ; #t -> don't print the mix id in the status area

	   (dynamic-wind
	       (lambda ()
		 (set! (sync outsnd) 0)
		 (do ((chan 0 (+ 1 chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #t)))

	       (lambda ()
		 (for-each
		  (lambda (note)
		    (let* ((snd (with-temp-sound (,@args :ignore-output #t :clipped #f) (eval (append (list (car note) 0.0) (cddr note)) (current-environment))))
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


(define* (with-mixed-sound->notelist (output-file "temp.scm") snd)
  (let* ((outsnd (or snd (selected-sound) (car (sounds))))
	 (mix-info (sound-property 'with-mixed-sound-info outsnd)))
    (if (not mix-info)
	(error 'no-such-mixed-sound (list "with-mixed-sound->notelist" outsnd))
	(let ((cur-mixes (mixes outsnd 0)) ; for now assume each mix is multichannel
	      (oput (open-output-file output-file)))
	  (format oput "(with-sound (:channels ~D)~%" (channels snd))
	  (for-each
	   (lambda (id)
	     (let ((info (with-mixed-sound-mix-info id snd)))
	       (if info
		   (let ((call (cadddr info)))
		     (if (not (= (cadr info) (mix-position id)))
			 (format oput "  (~A ~,3F~{ ~A~})~%"
				 (car call) 
				 (/ (mix-position id) (* 1.0 (srate snd)))
				 (cddr call))
			 (format oput "  ~A~%" call)))
		   (status-report "can't find note associated with mix ~A" id))))
	   cur-mixes)
	  (format oput ")~%")
	  (close-output-port oput)))))



;;; -------- with-marked-sound --------

(defmacro with-marked-sound (args . body)
  `(let ((old-notehook *clm-notehook*)
	 (mark-list ()))
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
  `(let ((temp-files ())
	 (old-hook-list (hook-functions new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (set! (hook-functions new-sound-hook)
	   (list (lambda (hook)       ; save current sound-let temp file list
		   (let ((file (hook 'name)))
		     (if (string? file) ; try to ignore vcts and sound-data objects
			 (set! temp-files (cons file temp-files)))))))
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
       (set! (hook-functions new-sound-hook) old-hook-list)
       val)))



;;; -------- Common Music --------

(define* (init-with-sound
	  (srate *clm-srate*) 
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
	      (if (or (vct? output)
		      (sound-data? output)
		      (vector? output))
		  (fill! output 0.0)))
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
		  (if (or (vct? revfile)
			  (sound-data? revfile)
			  (vector? revfile))
		      (fill! revfile 0.0)))
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
	    (output (wsd 1))
	    (reverb (wsd 2))
	    (revfile (wsd 3))
	    (old-srate (wsd 4))
	    (statistics (wsd 5))
	    (to-snd (wsd 6))
	    (scaled-to (wsd 7))
	    (scaled-by (wsd 8))
	    (play (wsd 9))
	    (reverb-data (wsd 10))
	    (start (wsd 11)))

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
	      (if play (*default-player* snd-output))
	      (update-time-graph snd-output)))
	(set! (mus-srate) old-srate)
	output)
      (error 'wrong-type-arg
	     (list "finish-with-sound" wsd))))


(define wsdat-play ; for cm
  (make-procedure-with-setter
   (lambda (w)
     "accessor for play field of init-with-sound struct"
     (w 9))
   (lambda (w val)
     (set! (w 9) val))))


;;; -------- with-sound save state --------

(define (ws-save-state hook)
  "(ws-save-state filename) is an after-save-state-hook function that saves the current with-sound global settings"
  (let ((filename (hook 'name)))

    (define (open-appending filename)
      (open-output-file filename "a"))
    
    (define close-appending close-output-port)
    
    (let ((fd (open-appending filename)))
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
      (close-appending fd))))

(hook-push after-save-state-hook ws-save-state)


;;; -------- ->frequency --------

(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 (/ 57 12)))) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                             ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0)))

    (define (string-downcase str) 
      (apply string (map char-downcase str)))

    (lambda* (pitch pythagorean)          ; pitch can be pitch name or actual frequency
      "(->frequency pitch pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."
      (if (symbol? pitch)
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (base-char (name 0))
		 (sign-char (and (> (string-length name) 1)
				 (not (char-numeric? (name 1)))
				 (not (char=? (name 1) #\n))
				 (name 1)))
		 (octave-char (if (and (> (string-length name) 1)
				       (char-numeric? (name 1))) 
				  (name 1)
				  (if (and (> (string-length name) 2) 
					   (char-numeric? (name 2)))
				      (name 2)
				      #f)))
		 (base (modulo (+ 5 (- (char->integer base-char) (char->integer #\a))) 7)) ; c-based (diatonic) octaves
		 (sign (if (not sign-char) 0 (if (char=? sign-char #\f) -1 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (+ sign (case base ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (if pythagorean
		(* main-pitch (expt 2 octave) (ratios base-pitch))
		(* main-pitch (expt 2.0 (/ et-pitch 12)))))
	  pitch))))


;;; -------- ->sample --------

(define (->sample beg)
  "(->sample time-in-seconds) -> time-in-samples"
  (round (* (if (not (null? (sounds))) (srate) (mus-srate)) beg)))



;;; -------- defgenerator --------

;;; (defgenerator osc a b)
;;; (defgenerator (osc :methods (list (cons 'mus-frequency (lambda (obj) 100.0)))) a b)

(define-macro (defgenerator struct-name . fields)

  (define (list->bindings lst)
    (if (null? lst)
	()
	(cons (if (pair? (car lst))
		  (list 'cons (list 'quote (caar lst)) (caar lst))
		  (list 'cons (list 'quote (car lst)) (car lst)))
	      (list->bindings (cdr lst)))))

  (let* ((name (if (list? struct-name) 
		   (car struct-name) 
		   struct-name))
	 (sname (if (string? name) 
		    name 
		    (symbol->string name)))
	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (struct-name 1) :make-wrapper)
				    (struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (struct-name 3) :make-wrapper)
				    (struct-name 4))))
		      (lambda (gen) gen)))
	 (methods (and (list? struct-name)
		       (or (and (> (length struct-name) 2)
				(equal? (struct-name 1) :methods)
				(struct-name 2))
			   (and (= (length struct-name) 5)
				(equal? (struct-name 3) :methods)
				(struct-name 4))))))
    `(begin 
       (define ,(string->symbol (string-append sname "?")) #f)
       (define ,(string->symbol (string-append "make-" sname)) #f)

       (let ((gen-type ',(string->symbol (string-append (string (integer->char 27)) sname (string (integer->char 12))))))
	 
	 (set! ,(string->symbol (string-append sname "?"))
	       (lambda (obj)
		 (and (environment? obj)
		      (eq? (obj 'mus-generator-type) gen-type))))

	 (set! ,(string->symbol (string-append "make-" sname))
	       (lambda* ,(map (lambda (n) 
				(if (list? n) n (list n 0.0)))
			      fields)
  	         (,wrapper 
		  (open-environment
		   ,(if methods
		       `(augment-environment 
			   (apply environment ,methods)
			 (environment (cons 'mus-generator-type gen-type) ,@(list->bindings (reverse fields))))
		       `(environment (cons 'mus-generator-type gen-type) ,@(list->bindings (reverse fields))))))))))))


;;; -------- clm-display-globals --------
;;;
;;; display all the globals that might affect with-sound unexpectedly

(define (clm-display-globals)

  (format #f ";CLM globals:~%;  *clm-srate*: ~A (default: ~A, mus-srate: ~A)~%;  *clm-file-name*: ~A~%;  *clm-channels: ~A (default: ~A)~%;  *clm-data-format*: ~A (default: ~A)~%;  *clm-header-type*: ~A (default: ~A)~%;  *clm-reverb-channels*: ~A, *clm-reverb-data*: ~A~%;  *clm-table-size*: ~A~%;  *clm-file-buffer-size*: ~A (~A)~%;  *clm-locsig-type*: ~A~%;  *clm-array-print-length*: ~A (~A)~%;  *clm-notehook*: ~A~%;  *clm-default-frequency*: ~A~%;  *clm-clipped*: ~A, mus-clipping: ~A, mus-prescaler: ~A~%~%"

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
	  (mus-prescaler)))




;;; -------- *clm-search-list*

(define *clm-search-list* (list "."))

(define (clm-find-file name)
  (if (file-exists? name)
      name
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (path)
	    (let ((len (string-length path)))
	      (if (> len 0)
		  (let ((new-name (string-append path (if (not (char=? (path (- len 1)) #\/)) "/" "") name)))
		    (if (file-exists? new-name)
			(return new-name))))))
	  *clm-search-list*)
	 #f))))

#|
(with-sound ()
  (let ((fd (make-file->sample "oboe.snd"))
	(len (mus-sound-frames "oboe.snd")))
    (do ((i 0 (+ i 1)))
	((= i len))
      (outa i (* .5 (file->sample fd i 0))))))
|#


(define (mix-notelists . notelists)
  ;; assume the 2nd parameter is the begin time in seconds (the 1st is the instrument name)
  (sort! 
   (apply append notelists)
   (lambda (note1 note2)
     (< (cadr note1) (cadr note2)))))

#|
(mix-notelists '((fm-violin 0 1 440 .1)
		 (fm-violin 1 1 550 .1))
	       '((bird 0 .1 )
		 (bird .2 .1)
		 (bird 1.2 .3)
		 (bird .5 .5)))
((bird 0 0.1) (fm-violin 0 1 440 0.1) (bird 0.2 0.1) (bird 0.5 0.5) (fm-violin 1 1 550 0.1) (bird 1.2 0.3))
|#


(define* (with-simple-sound-helper thunk (output "test.snd") (channels 1) (srate 44100) (data-format mus-lfloat) (header-type mus-next))
  (let ((old-output *output*))
    (dynamic-wind
	(lambda ()
	  (set! *output* (make-sample->file output channels data-format header-type "with-simple-sound output")))
	(lambda ()
	  (thunk)
	  output)
	(lambda ()
	  (mus-close *output*)
	  (set! *output* old-output)))))

(defmacro with-simple-sound (args . body)
  `(with-simple-sound-helper (lambda () ,@body) ,@args))
