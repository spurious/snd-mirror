;;; various generally useful Snd extensions

;;; channel-property, sound-property
;;; delete selected portion and smooth the splice
;;; eval over selection
;;; mix with result at original peak amp
;;; mix with envelope
;;; map-sound-files, for-each-sound-file, match-sound-files, directory->list
;;; selection-members
;;; make-selection
;;; check-for-unsaved-edits
;;; remember-sound-state
;;; mix-channel, insert-channel
;;; redo-channel, undo-channel
;;; sine-ramp, sine-env-channel, blackman4-ramp, blackman4-env-channel
;;; ramp-squared, env-squared-channel
;;; ramp-expt, env-expt-channel
;;; offset-channel
;;; channels-equal
;;; mono->stereo, mono-files->stereo, stereo->mono


(use-modules (ice-9 common-list) (ice-9 optargs) (ice-9 format))
(provide 'snd-extensions.scm)

;;; -------- channel-property

(define channel-property
  (make-procedure-with-setter
       
   (lambda (key snd chn)
     "(channel-property key snd chn) returns the value associated with 'key' in the given channel's property list, or #f"
     (let ((data (assoc key (channel-properties snd chn))))
       (if data
	   (cdr data)
           #f)))

   (lambda (key snd chn new-val)
     (let ((old-val (assoc key (channel-properties snd chn))))
       (if old-val
	   (set-cdr! old-val new-val)
	   (set! (channel-properties snd chn) (cons (cons key new-val) (channel-properties snd chn))))
       new-val))))

;;; -------- sound-property

(define sound-property
  (make-procedure-with-setter
       
   (lambda (key snd)
     "(sound-property key snd) returns the value associated with 'key' in the given sound's property list, or #f"
     (let ((data (assoc key (sound-properties snd))))
       (if data
	   (cdr data)
           #f)))

   (lambda (key snd new-val)
     (let ((old-val (assoc key (sound-properties snd))))
       (if old-val
	   (set-cdr! old-val new-val)
	   (set! (sound-properties snd) (cons (cons key new-val) (sound-properties snd))))
       new-val))))


(define channel-sync
  (make-procedure-with-setter
   (lambda (snd chn) (channel-property 'sync snd chn))
   (lambda (snd chn val) (set! (channel-property 'sync snd chn) val))))


(if (not (defined? 'all-chans))
    (define (all-chans)
      "(all-chans) -> two parallel lists, the first snd indices, the second channel numbers.  If we have
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((0 1 1) (0 0 1))"
      (let ((sndlist '())
	    (chnlist '()))
	(for-each (lambda (snd)
		    (do ((i (1- (channels snd)) (1- i)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))



;;; -------- mix with result at original peak amp

(define (normalized-mix filename beg in-chan snd chn)
  "(normalized-mix filename beg in-chan snd chn) is like mix but the mix result has same peak amp as unmixed snd/chn (returns scaler)"
  (let ((original-maxamp (maxamp snd chn)))
    (mix filename beg in-chan snd chn)
    (let ((new-maxamp (maxamp snd chn)))
      (if (not (= original-maxamp new-maxamp))
	  (let ((scaler (/ original-maxamp new-maxamp))
		(old-sync (sync snd)))
	    (set! (sync snd) 0)
	    (scale-by scaler snd chn)
	    (set! (sync snd) old-sync)
	    scaler)
	  1.0))))


;;;-------- mix with envelope on mixed-in file
;;;
;;; there are lots of ways to do this; this version uses functions from Snd, CLM, and Sndlib.

(define (enveloped-mix filename beg env)
  "(enveloped-mix filename beg env) mixes filename starting at beg with amplitude envelope env. (enveloped-mix \"pistol.snd\" 0 '(0 0 1 1 2 0))"
  (let* ((len (mus-sound-frames filename))
	 (tmp-name (string-append (if (and (string? (temp-dir))
					   (> (string-length (temp-dir)) 0))
				      (string-append (temp-dir) "/")
				      "")
				  "tmp.snd"))
	 (tmpfil (mus-sound-open-output tmp-name 22050 1 mus-bshort mus-next ""))
	 (mx (make-mixer 1 1.0))
	 (envs (make-vector 1))
	 (inenvs (make-vector 1)))
    (mus-sound-close-output tmpfil 0)
    (vector-set! inenvs 0 (make-env env :end len))
    (vector-set! envs 0 inenvs)
    (mus-mix tmp-name filename 0 len 0 mx envs)
    (mix tmp-name beg)
    (delete-file tmp-name)))

;;; another way:
; 
; (define (enveloped-mix-1 filename beg env)
;   "(enveloped-mix-1 filename beg env) mixes filename starting at beg with amplitude envelope env. (enveloped-mix-1 \"pistol.snd\" 0 '(0 0 1 1 2 0))"
;   (as-one-edit
;    (lambda ()
;      (let* ((mix-id (mix filename beg))
; 	    (inchans (mix-chans mix-id)))
;        (do ((i 0 (1+ i)))
; 	   ((= i inchans))
; 	 (set! (mix-amp-env mix-id i) env))))))


;;; -------- map-sound-files, match-sound-files
;;;
;;; apply a function to each sound in dir
;;;
;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0) (snd-print n))))

(define map-sound-files
  (lambda args
    "(map-sound-files func #:optional dir) applies func to each sound file in dir"
    (map (car args) 
	 (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))))

(define for-each-sound-file
  (lambda args
    "(for-each-sound-file func #:optional dir) applies func to each sound file in dir"
    (for-each 
     (car args) 
     (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))))

#!
 (for-each-sound-file
  (lambda (n) 
    (catch #t
           (lambda ()
 	      (if (not (null? (mus-sound-loop-info (string-append "/home/bil/sf/" n)))) 
 		  (snd-print (format #f "~%~A" n))))
           (lambda args #f)))
  "/home/bil/sf")
!#

(define match-sound-files
  (lambda args
    "(match-sound-files func #:optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
    (let* ((func (car args))
	   (matches '()))
      (for-each
       (lambda (file)
	 (if (func file)
	     (set! matches (cons file matches))))
       (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
      matches)))
  
;;; we can use Guile's regexp support here to search for all .snd and .wav files:
#!
(let ((reg (make-regexp "\\.(wav|snd)$")))
  (match-sound-files (lambda (file) (regexp-exec reg file))))

;;; this argument to make-regexp is looking for *.wav and *.snd
;;; a prettier version might use a function written by Dirk Herrmann:

(define (filter-list pred? objects)
  (let loop ((objs objects)
	     (result '()))
    (cond ((null? objs) (reverse! result))
	  ((pred? (car objs)) (loop (cdr objs) (cons (car objs) result)))
	  (else (loop (cdr objs) result)))))

(define match-sound-files-1
  (lambda args
    (filter-list 
     (car args) 
     (sound-files-in-directory 
      (if (null? (cdr args)) 
	  "." 
	  (cadr args))))))

;;; in fact, we could use regexp's in place of Snd's sound-files-in-directory,
;;; using two more of Dirk Herrman's procedures:

(define (grep rx strings)
  (let ((r (make-regexp rx)))
    (filter-list (lambda (x) (regexp-exec r x)) strings)))

(define (directory->list dir)
  (let ((dport (opendir dir)))
    (let loop ((entry (readdir dport))
	       (files '()))
      (if (not (eof-object? entry))
	  (loop (readdir dport) (cons entry files))
	  (begin
	    (closedir dport)
	    (reverse! files))))))

;(sort (grep "^[^.]" (directory->list ".")) string<?)


(define sound-file-extensions (list "snd" "aiff" "aif" "wav" "au" "aifc" "voc" "wve"))

(define (add-sound-file-extension-1 ext) 
  (set! sound-file-extensions (cons ext sound-file-extensions)))

(define* (sound-files-in-directory-1 #:optional (dir "."))
  (sort (grep
	 (format #f "\\.(~{~A~^|~})$" sound-file-extensions)
	 (directory->list dir))
	string<?))
!#

    
;;; -------- selection-members
;;;
;;; returns a list of lists of (snd chn): channels in current selection

(define (selection-members)
  "(selection-members) -> list of lists of (snd chn) indicating the channels participating in the current selection."
  (let ((sndlist '()))
    (if (selection?)
	(map (lambda (snd)
	       (do ((i (1- (channels snd)) (1- i)))
		   ((< i 0))
		 (if (selection-member? snd i)
		     (set! sndlist (cons (list snd i) sndlist)))))
	     (sounds)))
    sndlist))


;;; -------- make-selection

;;; the regularized form of this would use dur not end

(define* (make-selection #:optional beg end snd chn)
  "(make-selection #:optional beg end snd chn) makes a selection like make-region but without creating a region.
make-selection follows snd's sync field, and applies to all snd's channels if chn is not specified. end defaults
to end of channel, beg defaults to 0, snd defaults to the currently selected sound."
  (let ((current-sound (or snd (selected-sound) (car (sounds)))))
    (define (add-chan-to-selection s0 s1 s c)
      (set! (selection-member? s c) #t)
      (set! (selection-position s c) (or s0 0))
      (set! (selection-frames s c) (- (or (and (number? s1) (1+ s1)) (frames s c)) (or s0 0))))
    (if (not (sound? current-sound))
	(throw 'no-such-sound (list "make-selection" beg end snd chn)))
    (let ((current-sync (sync current-sound)))
      (if (selection?) ; clear selection
	  (for-each
	   (lambda (s)
	     (do ((i 0 (1+ i)))
		 ((= i (chans s)))
	       (let ((need-update (selection-member? s i)))
		 (set! (selection-member? s i) #f)
		 (if need-update (update-time-graph s i)))))
	   (sounds)))
      (if (number? chn)
	  (add-chan-to-selection beg end snd chn)
	  (for-each
	   (lambda (s)
	     (if (or (eq? snd #t)
		     (= s current-sound)
		     (and (not (= current-sync 0))
			  (= current-sync (sync s))))
		 (do ((i 0 (1+ i)))
		     ((= i (chans s)))
		   (add-chan-to-selection beg end s i))))
	   (sounds))))))

;;; -------- delete selected portion and smooth the splice

(define (delete-selection-and-smooth)
  "(delete-selection-and-smooth) deletes the current selection and smooths the splice"
  (if (selection?)
      (let ((beg (selection-position))
	    (len (selection-frames)))
	(apply map (lambda (snd chn)
		     (if (selection-member? snd chn)
			 (let ((smooth-beg (max 0 (- beg 16))))
			   (delete-samples beg len snd chn)
			   (smooth-sound smooth-beg 32 snd chn))))
	       (all-chans)))))


;;; -------- eval over selection, replacing current samples, mapped to "C-x x" key using prompt-in-minibuffer
;;;
;;; when the user types C-x x (without modifiers) and there is a current selection,
;;;   the minibuffer prompts "selection eval:".  Eventually the user responds,
;;;   hopefully with a function of one argument, the current selection sample
;;;   the value returned by the function becomes the new selection value.

(bind-key (char->integer #\x) 0
	  (lambda () "eval over selection"
	    (if (selection?)
		(prompt-in-minibuffer "selection eval:" eval-over-selection)
		(report-in-minibuffer "no selection")))
	  #t)

(define eval-over-selection 
  (lambda (func)
    "(eval-over-selection func) evaluates func on each sample in the current selection"
    (if (and (procedure? func) 
	     (selection?))
	(let* ((beg (selection-position))
	       (len (selection-frames)))
	  (apply map (lambda (snd chn)
		       (if (selection-member? snd chn)
			   (let ((new-data (make-vct len))
				 (old-data (channel->vct beg len snd chn)))
			     (do ((k 0 (1+ k))) ;here we're applying our function to each sample in the currently selected portion
				 ((= k len) (vct->channel new-data beg len snd chn))
			       (vct-set! new-data k (func (vct-ref old-data k)))))))
		 (all-chans))))))

;;; the same idea can be used to apply a function between two marks (see eval-between-marks in marks.scm)



;;; -------- check-for-unsaved-edits
;;;
;;; (check-for-unsaved-edits #:optional (on #t)): if 'on', add a function to the close-hook and exit-hook
;;;    that asks the user for confirmation before closing a sound if there are unsaved
;;;    edits on that sound.  if 'on' is #f, remove those hooks.

(define* (check-for-unsaved-edits #:optional (check #t))
  "(check-for-unsaved-edits #:optional (check #t)) -> sets up hooks to check for and ask about unsaved edits when a sound is closed.
If 'check' is #f, the hooks are removed."
  (let ((dummy #f)) ; make new guile happy
    (define (unsaved-edits-at-close? ind)
      (letrec ((unsaved-edits-in-chan? 
		(lambda (chan)
		  (if (>= chan (channels ind))
		      #f
		      (let ((eds (edits ind chan)))
			(if (> (car eds) 0)
			    (not (yes-or-no? ;that is, "yes" => exit
				  (format #f "~A has ~D unsaved edit~P in channel ~D, exit anyway? " 
					  (short-file-name ind) 
					  (car eds)
					  (car eds)
					  chan)))
			    (unsaved-edits-in-chan? (1+ chan))))))))
	(unsaved-edits-in-chan? 0)))
    
    (define (unsaved-edits-at-exit?)
      (letrec ((unsaved-edits-at-exit-1?
		(lambda (snds)
		  (and (not (null? snds))
		       (or (unsaved-edits-at-close? (car snds))
			   (unsaved-edits-at-exit-1? (cdr snds)))))))
	(unsaved-edits-at-exit-1? (sounds))))
    
    (if check
	(begin
	  (if (not (member unsaved-edits-at-exit? (hook->list exit-hook)))
	      (add-hook! exit-hook unsaved-edits-at-exit?))
	  (if (not (member unsaved-edits-at-close? (hook->list close-hook)))
	      (add-hook! close-hook unsaved-edits-at-close?)))
	(begin
	  (remove-hook! exit-hook unsaved-edits-at-exit?)
	  (remove-hook! close-hook unsaved-edits-at-close?)))))


;;; -------- remember-sound-state

(define (remember-sound-state)
  "(remember-sound-state) remembers the state of a sound when it is closed, and if it is subsquently re-opened, restores that state"
  (let ((states '())
	(sound-funcs (list sync cursor-follows-play selected-channel show-controls read-only
			   contrast-control? expand-control? reverb-control? filter-control?
			   amp-control amp-control-bounds
			   contrast-control contrast-control-amp contrast-control-bounds
			   expand-control expand-control-bounds expand-control-hop expand-control-jitter expand-control-length expand-control-ramp 
			   filter-control-envelope filter-control-in-dB filter-control-in-hz filter-control-order 
			   reverb-control-decay reverb-control-feedback reverb-control-length reverb-control-length-bounds
			   reverb-control-lowpass reverb-control-scale reverb-control-scale-bounds
			   speed-control speed-control-bounds speed-control-style speed-control-tones))
	(channel-funcs (list time-graph? transform-graph? lisp-graph? x-bounds y-bounds cursor cursor-size
			     cursor-style show-marks show-y-zero show-grid wavo-hop wavo-trace max-transform-peaks
			     show-transform-peaks fft-log-frequency fft-log-magnitude verbose-cursor zero-pad
			     wavelet-type min-dB transform-size transform-graph-type time-graph-type fft-window
			     transform-type transform-normalization time-graph-style show-mix-waveforms dot-size
			     x-axis-style show-axes graphs-horizontal lisp-graph-style transform-graph-style)))
    (define saved-state
      (make-procedure-with-setter
       (lambda (snd)
	 (find-if (lambda (n) 
		    (string=? (car n) (file-name snd))) 
		  states))
       (lambda (snd new-state)
	 (set! states (cons new-state
			    (remove-if
			     (lambda (n)
			       (string=? (car n) (file-name snd)))
			     states))))))

    (add-hook! close-hook (lambda (snd)
			    ;; save current state
			    (set! (saved-state snd)
				  (list (file-name snd)
					(file-write-date (file-name snd))
					(map (lambda (f) 
					       (f snd)) 
					     sound-funcs)
					(map (lambda (sc)
					       (map (lambda (f)
						      (f (car sc) (cadr sc)))
						    channel-funcs))
					     (let ((scs '()))
					       (do ((i 0 (1+ i)))
						   ((= i (chans snd)))
						 (set! scs (cons (list snd i) scs)))
					       (reverse scs)))))
			    #f))
    (add-hook! after-open-hook (lambda (snd)
				 ;; restore previous state, if any
				 (let ((state (saved-state snd)))
				   (if state
				       (if (= (file-write-date (file-name snd))
					      (cadr state))
					   ;; otherwise all bets are off (anything could have changed)
					   (begin
					     (for-each (lambda (f val)
							 (set! (f snd) val))
						       sound-funcs
						       (caddr state))
					     (do ((chn 0 (1+ chn)))
						 ((= chn (chans snd)))
					       (set! (squelch-update snd chn) #t)
					       (for-each (lambda (f val)
							   (set! (f snd chn) val))
							 channel-funcs
							 (list-ref (cadddr state) chn))
					       (set! (squelch-update snd chn) #f)
					       (if (time-graph? snd chn) (update-time-graph snd chn))
					       (if (transform-graph? snd chn) (update-transform-graph snd chn))
					       )))))))
    ;; next hooks save the current states info in the saved state file
    (add-hook! open-hook (lambda (filename)
			   (if (and (null? states)
				    (defined? '-saved-remember-sound-states-states-))
			       (set! states -saved-remember-sound-states-states-))
			   #f))
    (add-hook! after-save-state-hook (lambda (filename)
				       (let ((fd (open filename (logior O_RDWR O_APPEND))))
					 (format fd "~%~%;;; from remember-sound-state in extensions.scm~%")
					 (format fd "(define -saved-remember-sound-states-states- '~A)~%" states)
					 (close fd))))
    'remembering!))


;;; -------- mix-channel, insert-channel, c-channel

(define* (mix-channel file-data #:optional beg dur snd chn edpos)
  "(mix-channel file #:optional beg dur snd chn edpos) mixes in file. file can be the file name or a list (file-name [beg [channel]])"
  ;; should this create and return a mix instead?
  (let* ((file-name (if (string? file-data) file-data (car file-data)))
	 (file-beg (if (or (string? file-data) 
			   (< (length file-data) 2)) 
		       0 
		       (cadr file-data)))
	 (file-channel (if (or (string? file-data) 
			       (< (length file-data) 3))
			   0 
			   (caddr file-data)))
	 (len (or dur (- (mus-sound-frames file-name) file-beg)))
	 (start (or beg 0)))
    (if (< start 0) (throw 'no-such-sample (list "mix-channel" beg)))
    (if (> len 0)
	(let ((reader (make-sample-reader file-beg file-name file-channel)))
	  (map-channel (lambda (val)
			 (+ val (next-sample reader)))
		       start len snd chn edpos
		       (if (string? file-data)
			   (format #f "mix-channel ~S ~A ~A" file-data beg dur)
			   (format #f "mix-channel '~A ~A ~A" file-data beg dur)))))))

(define* (insert-channel file-data #:optional beg dur snd chn edpos)
  "(insert-channel file #:optional beg dur snd chn edpos) inserts the file. file can be the file name or a list (file-name [beg [channel]])"
  (let* ((file-name (if (string? file-data) file-data (car file-data)))
	 (file-beg (if (or (string? file-data) 
			   (< (length file-data) 2)) 
		       0 
		       (cadr file-data)))
	 (file-channel (if (or (string? file-data) 
			       (< (length file-data) 3))
			   0 
			   (caddr file-data)))
	 (len (or dur (- (mus-sound-frames file-name) file-beg)))
	 (start (or beg 0)))
    (if (< start 0) (throw 'no-such-sample (list "insert-channel" beg)))
    (if (> len 0)
	(let ((reader (make-sample-reader file-beg file-name file-channel))
	      (data (make-vct len)))
	  (vct-map! data (lambda () (next-sample reader)))
	  (insert-samples start len data snd chn edpos #f 
			  (if (string? file-data)
			      (format #f "insert-channel ~S ~A ~A" file-data beg dur)
			      (format #f "insert-channel '~A ~A ~A" file-data beg dur)))))))


;;; -------- redo-channel, undo-channel

(define* (redo-channel #:optional (edits 1) snd chn)
  "(redo-channel (edits 1) snd chn) is the regularized version of redo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (+ (edit-position snd chn) edits))
      (redo edits snd)))

(define* (undo-channel #:optional (edits 1) snd chn)
  "(undo-channel (edits 1) snd chn) is the regularized version of undo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (max 0 (- (edit-position snd chn) edits)))
      (undo edits snd)))


;;; -------- any-env-channel

(define* (any-env-channel env func #:optional (beg 0) dur snd chn edpos origin)
  ;; take breakpoints in env, connect with func, apply as envelope to channel
  ;; handled as a sequence of funcs and scales
  (if (not (null? env))
      (let ((pts (/ (length env) 2)))
	(if (= pts 1)
	    (scale-channel (car env) beg dur snd chn edpos)
	    (let ((x0 0)
		  (y0 0)
		  (x1 (car env))
		  (y1 (cadr env))
		  (xrange (- (list-ref env (- (length env) 2)) (car env)))
		  (ramp-beg beg)
		  (ramp-dur 0))
	      (if (not (number? dur)) (set! dur (frames snd chn)))
	      (as-one-edit 
	       (lambda ()
		 (do ((i 1 (1+ i))
		      (j 2 (+ j 2)))
		     ((= i pts))
		   (set! x0 x1)
		   (set! y0 y1)
		   (set! x1 (list-ref env j))
		   (set! y1 (list-ref env (1+ j)))
		   (set! ramp-dur (inexact->exact (round (* dur (/ (- x1 x0) xrange)))))
		   (if (= y0 y1)
		       (scale-channel y0 ramp-beg ramp-dur snd chn edpos)
		       (func y0 y1 ramp-beg ramp-dur snd chn edpos))
		   (set! ramp-beg (+ ramp-beg ramp-dur))))
	       origin))))))

;;; -------- sine-ramp sine-env-channel 

(if (not (defined? 'pi)) (define pi 3.141592653589793))

(define* (sine-ramp rmp0 rmp1 #:optional (beg 0) dur snd chn edpos)
  "(sine-ramp rmp0 rmp1 #:optional (beg 0) dur snd chn edpos) produces a sinsusoidal connection from rmp0 to rmp1"
  ;; vct: angle incr off scl
  (ptree-channel
   (lambda (y data forward)
     (declare (y real) (data vct) (forward boolean))
     (let* ((angle (vct-ref data 0))
	    (incr (vct-ref data 1))
	    (val (* y (+ (vct-ref data 2) (* (vct-ref data 3) (+ 0.5 (* 0.5 (cos angle))))))))
       ;; this could be optimized into offset=off+scl/2 and scl=scl/2, then (* y (+ off (* scl cos)))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ pi frag-dur)))
       (vct (+ (* -1.0 pi) (* frag-beg incr))
	    incr
	    rmp0
	    (- rmp1 rmp0))))
   #t (format #f "sine-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur)))

(define* (sine-env-channel env #:optional (beg 0) dur snd chn edpos)
  "(sine-env-channel env #:optional (beg 0) dur snd chn edpos) connects env's dots with sinusoids"
  (any-env-channel env sine-ramp beg dur snd chn edpos (format #f "sine-env-channel '~A ~A ~A" env beg dur)))

;;; (sine-env-channel '(0 0 1 1 2 -.5 3 1))

;;; an obvious extension of this idea is to use the blackman fft window formulas
;;;   to get sharper sinusoids (i.e. use the sum of n cosines, rather than just 1)

;;; -------- blackman4-ramp, blackman4-env-channel

(define* (blackman4-ramp rmp0 rmp1 #:optional (beg 0) dur snd chn edpos)
  ;; vct: angle incr off scl
  (ptree-channel
   (lambda (y data forward)
     (declare (y real) (data vct) (forward boolean))
     (let* ((angle (vct-ref data 0))
	    (incr (vct-ref data 1))
	    (cx (cos angle))
	    (val (* y (+ (vct-ref data 2) 
			 (* (vct-ref data 3) 
			    (+ .084037 (* cx (+ -.29145 (* cx (+ .375696 (* cx (+ -.20762 (* cx .041194)))))))))))))
       ;; blackman2 would be: (+ .34401 (* cx (+ -.49755 (* cx .15844))))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ pi frag-dur)))
       (vct (* frag-beg incr)
	    incr
	    rmp0
	    (- rmp1 rmp0))))
   #t (format #f "blackman4-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur)))

(define* (blackman4-env-channel env #:optional (beg 0) dur snd chn edpos)
  (any-env-channel env blackman4-ramp beg dur snd chn edpos (format #f "blackman4-env-channel '~A ~A ~A" env beg dur)))

;;; any curve can be used as the connecting line between envelope breakpoints in the
;;;   same manner -- set up each ramp to take the current position and increment,
;;;   then return the value in ptree-channel.  A simple one would have a table of
;;;   values and use array-interp.


;;; -------- ramp-squared, env-squared-channel

(define* (ramp-squared rmp0 rmp1 #:optional (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-squared rmp0 rmp1 #:optional (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^2 curve"
  ;; vct: start incr off scl
  (ptree-channel
   (lambda (y data forward)
     (declare (y real) (data vct) (forward boolean))
     (let* ((angle (vct-ref data 0))
	    (incr (vct-ref data 1))
	    (val (* y (+ (vct-ref data 2) (* angle angle (vct-ref data 3))))))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ 1.0 frag-dur)))
       (if (and symmetric
		(< rmp1 rmp0))
	   (vct (* (- frag-dur frag-beg) incr) (- incr) rmp1 (- rmp0 rmp1))
	   (vct (* frag-beg incr) incr rmp0 (- rmp1 rmp0)))))
   #t (format #f "ramp-squared ~A ~A ~A ~A ~A" rmp0 rmp1 symmetric beg dur)))

(define* (env-squared-channel env #:optional (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-squared-channel env #:optional (symmetric #t) (beg 0) dur snd chn edpos) connects env's dots with x^2 curves"
  (any-env-channel env 
		   (lambda (r0 r1 b d s c e)
		     (ramp-squared r0 r1 symmetric b d s c e))
		   beg dur snd chn edpos
		   (format #f "env-squared-channel '~A ~A ~A ~A" env symmetric beg dur)))

;;; (env-squared-channel '(0 0 1 1 2 -.5 3 1))

;;; -------- ramp-expt, env-expt-channel

(define* (ramp-expt rmp0 rmp1 exponent #:optional (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-expt rmp0 rmp1 exponent #:optional (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^exponent curve"
  ;; vct: start incr off scl exponent
  ;; a^x = exp(x * log(a))
  (ptree-channel
   (lambda (y data forward)
     (declare (y real) (data vct) (forward boolean))
     (let* ((angle (vct-ref data 0))
	    (incr (vct-ref data 1))
	    (val (* y (+ (vct-ref data 2) (* (exp (* (log angle) (vct-ref data 4))) (vct-ref data 3))))))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ 1.0 frag-dur)))
       (if (and symmetric
		(< rmp1 rmp0))
	   (vct (* (- frag-dur frag-beg) incr) (- incr) rmp1 (- rmp0 rmp1) exponent)
	   (vct (* frag-beg incr) incr rmp0 (- rmp1 rmp0) exponent))))
   #t (format #f "ramp-expt ~A ~A ~A ~A ~A ~A" rmp0 rmp1 exponent symmetric beg dur)))

(define* (env-expt-channel env exponent #:optional (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-expt-channel env exponent #:optional (symmetric #t) (beg 0) dur snd chn edpos) connects env'e dots with x^exponent curves"
  (if (= exponent 1.0)
      (env-channel env beg dur snd chn edpos)
      (any-env-channel env 
		       (lambda (r0 r1 b d s c e)
			 (ramp-expt r0 r1 exponent symmetric b d s c e))
		       beg dur snd chn edpos
		       (format #f "env-expt-channel '~A ~A ~A ~A ~A" env exponent symmetric beg dur))))

;;; -------- offset-channel 

(define* (offset-channel amount #:optional (beg 0) dur snd chn edpos)
  "(offset-channel amount #:optional (beg 0) dur snd chn edpos) adds amount to each sample"
  (let ((dc amount))
    (ptree-channel (lambda (y) (+ y dc)) beg dur snd chn edpos #t #f #t 
		   (format #f "offset-channel ~A ~A ~A" amount beg dur))))


;;; -------- dither-channel

(define* (dither-channel #:optional (amount .00006) (beg 0) dur snd chn edpos)
  "(dither-channel #:optional (amount .00006) (beg 0) dur snd chn edpos) adds amount dither to each sample"
  (let ((dither (* .5 amount)))
    (ptree-channel (lambda (y) (+ y (mus-random dither) (mus-random dither))) beg dur snd chn edpos #t #f #t
		   (format #f "dither-channel ~,8F ~A ~A" amount beg dur))))


;;; -------- contrast-channel

(define* (contrast-channel index #:optional (beg 0) dur snd chn edpos)
  "(contrast-channel index #:optional (beg 0) dur snd chn edpos) applies contrast enhancement to the sound"
  (let ((ind index))
    (ptree-channel
     (lambda (y)
       (sin (+ (* y 0.5 pi) (* ind (sin (* y 2.0 pi))))))
     beg dur snd chn edpos #f #f #t 
     (format #f "contrast-channel ~A ~A ~A" index beg dur))))

#!
;;; -------- delay-channel 
;;;
;;; this is ok going forward (I think), but not in reverse

(define* (delay-channel amount #:optional (beg 0) dur snd chn edpos)
  (let ((dly amount)
	(cur-edpos (if (or (not edpos)
			   (= edpos current-edit-position))
		       (edit-position snd chn)
		       edpos)))
    (ptree-channel (lambda (y data dir)
		     (declare (y real) (data vct) (dir boolean))
		     (let* ((pos (inexact->exact (floor (vct-ref data 0))))
			    (len (inexact->exact (floor (vct-ref data 1))))
			    (val (vct-ref data (+ pos 2))))
		       (vct-set! data (+ pos 2) y)
		       (set! pos (1+ pos))
		       (if (>= pos len) (vct-set! data 0 0) (vct-set! data 0 pos))
		       val))
		   beg dur snd chn edpos #f
		   (lambda (fpos fdur)
		     (let ((data (make-vct (+ dly 2))))
		       (vct-set! data 0 0.0)
		       (vct-set! data 1 dly)
		       (if (= fpos 0)
			   data
			   (let* ((reader (make-sample-reader (1- fpos) snd chn -1 cur-edpos)))
			     (do ((i (1- dly) (1- i)))
				 ((< i 0))
			       (vct-set! data (+ i 2) (reader)))
			     data)))))))
!#

;;; -------- channels-equal

(define* (channels=? snd1 chn1 snd2 chn2 #:optional (allowable-difference 0.0))
  "(channels=? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff) modulo trailing 0's"
  (if (and (= snd1 snd2)
	   (= chn1 chn2))
      #t
      (let ((mx1 (maxamp snd1 chn1))
	    (mx2 (maxamp snd1 chn1)))
	(if (> (abs (- mx1 mx2)) allowable-difference)
	    #f
	    (let* ((len1 (frames snd1 chn1))
		   (len2 (frames snd2 chn2))
		   (first-longer (>= len1 len2))
		   (len (if first-longer len1 len2))
		   (s1 (if first-longer snd1 snd2))
		   (s2 (if first-longer snd2 snd1))
		   (c1 (if first-longer chn1 chn2))
		   (c2 (if first-longer chn2 chn1))
		   (read2 (make-sample-reader 0 s2 c2))
		   (diff allowable-difference))
	      (not (scan-channel (lambda (y)
				   (let ((val (read-sample read2)))
				     (> (abs (- val y)) diff)))
				 0 len s1 c1)))))))

(define* (channels-equal? snd1 chn1 snd2 chn2 #:optional (allowable-difference 0.0))
  "(channels-equal? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff)"
  (let ((len1 (frames snd1 chn1))
	(len2 (frames snd2 chn2)))
    (if (not (= len1 len2))
	#f
	(channels=? snd1 chn1 snd2 chn2 allowable-difference))))


;;; -------- mono->stereo, mono-files->stereo

(define (mono->stereo new-name snd1 chn1 snd2 chn2)
  ;; (mono->stereo "test.snd" 0 0 1 0)
  (let ((old-ed1 (edit-position snd1 chn1))
	(old-ed2 (edit-position snd2 chn2))
	(ind (new-sound new-name :channels 2 :srate (srate snd1))))
    (swap-channels ind 0 snd1 chn1)
    (swap-channels ind 1 snd2 chn2)
    (set! (edit-position snd1 chn1) old-ed1)
    (set! (edit-position snd2 chn2) old-ed2)
    ind))

(define (mono-files->stereo new-name chan1-name chan2-name)
  ;; (mono-files->stereo "test.snd" "oboe.snd" "pistol.snd")
  (let* ((ind1 (open-sound chan1-name))
	 (ind2 (open-sound chan2-name))
	 (ind3 (mono->stereo new-name ind1 0 ind2 0)))
    (close-sound ind1)
    (close-sound ind2)
    ind3))

(define (stereo->mono orig-snd chan1-name chan2-name)
  ;; (stereo->mono 0 "hi1.snd" "hi2.snd")
  (let ((old-ed0 (edit-position orig-snd 0))
	(old-ed1 (edit-position orig-snd 1))
	(chan1 (new-sound chan1-name :srate (srate orig-snd)))	
	(chan2 (new-sound chan2-name :srate (srate orig-snd))))
    (swap-channels orig-snd 0 chan1 0)
    (swap-channels orig-snd 1 chan2 0)
    (set! (edit-position orig-snd 0) old-ed0)
    (set! (edit-position orig-snd 1) old-ed1)
    (list chan1 chan2)))
