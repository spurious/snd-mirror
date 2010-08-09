;;; various generally useful Snd extensions

;;; delete selected portion and smooth the splice
;;; mix then scale result to original peak amp
;;; mix with envelope
;;; map-sound-files, for-each-sound-file, match-sound-files, directory->list
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


(provide 'snd-extensions.scm)

(define (remove-if pred l)
  "(remove-if func lst) removes any element from 'lst' that 'func' likes"
  (let loop ((l l) (result '()))
    (cond ((null? l) (reverse! result))
	  ((pred (car l)) (loop (cdr l) result))
	  (else (loop (cdr l) (cons (car l) result))))))

(if (not (defined? 'all-chans))
    (define (all-chans)
      "(all-chans) -> two parallel lists, the first sound objects, the second channel numbers.  If we have
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((#<sound 0> #<sound 1> #<sound 1>) (0 0 1))"
      (let ((sndlist '())
	    (chnlist '()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))


(define channel-sync
  (make-procedure-with-setter
   (lambda (snd chn) (channel-property 'sync snd chn))
   (lambda (snd chn val) (set! (channel-property 'sync snd chn) val))))



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
  (let* ((len (frames filename))
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
    (set! (inenvs 0) (make-env env :length len))
    (set! (envs 0) inenvs)
    (mus-mix tmp-name filename 0 len 0 mx envs)
    (mix tmp-name beg)
    (delete-file tmp-name)))


;;; -------- map-sound-files, match-sound-files
;;;
;;; apply a function to each sound in dir
;;;
;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0) (snd-print n))))

(define* (map-sound-files func dir)
  "(map-sound-files func dir) applies func to each sound file in dir"
  (map func (sound-files-in-directory (or dir "."))))


(define* (for-each-sound-file func dir)
  "(for-each-sound-file func dir) applies func to each sound file in dir"
  (for-each func (sound-files-in-directory (or dir "."))))

#|
 (for-each-sound-file
  (lambda (n) 
    (catch #t
           (lambda ()
 	      (if (not (null? (mus-sound-loop-info (string-append "/home/bil/sf/" n)))) 
 		  (snd-print (format #f "~%~A" n))))
           (lambda args #f)))
  "/home/bil/sf")
|#


(define* (match-sound-files func dir)
  "(match-sound-files func dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
  (let* ((matches '()))
    (for-each
     (lambda (file)
       (if (func file)
	   (set! matches (cons file matches))))
     (sound-files-in-directory (or dir ".")))
    matches))
  

    
;;; -------- check-for-unsaved-edits
;;;
;;; (check-for-unsaved-edits (on #t)): if 'on', add a function to before-close-hook and before-exit-hook
;;;    that asks the user for confirmation before closing a sound if there are unsaved
;;;    edits on that sound.  if 'on' is #f, remove those hooks.

(define checking-for-unsaved-edits #f) ; for prefs

(define* (check-for-unsaved-edits (check #t))
  "(check-for-unsaved-edits (check #t)) -> sets up hooks to check for and ask about unsaved edits when a sound is closed.
If 'check' is #f, the hooks are removed."

  (define* (yes-or-no? question action-if-yes action-if-no snd)
    ;; we are replacing the caller's requested action with this prompt, so the action won't take place
    ;;   until we get a response.
    (clear-minibuffer snd)
    (prompt-in-minibuffer question
			  (lambda (response)
			    (clear-minibuffer snd)
			    (if (or (string=? response "yes")
				    (string=? response "y"))
				(action-if-yes snd)
				(action-if-no snd)))
			  snd #t))

  (define (ignore-unsaved-edits-at-close? ind exiting)
    (let ((eds 0))
      (do ((i 0 (+ 1 i)))
	  ((= i (channels ind)))
	(set! eds (+ eds (car (edits ind i)))))
      (if (> eds 0)
	  (begin
	    ;; there are unsaved edits; cancel requested action (return #f -> #t) and wait for response
	    (yes-or-no?
	     (format #f "~A has unsaved edits.  ~A anyway? " 
		     (short-file-name ind)
		     (if exiting "Exit" "Close"))
	     (lambda (snd)
	       ;;"Yes close anyway"
	       (revert-sound ind) ; to make sure this hook doesn't activate
	       (close-sound ind)
	       (if exiting (exit)))
	     (lambda (snd)
	       ;;"no don't close"
	       #f) ; this return value is just a placeholder to make Scheme happy
	     ind)
	    #f) ; (not #f) -> #t means cancel request pending response
	  #t))) ; (not #t) -> #f means go ahead, there are no unsaved edits
  
  (define (ignore-unsaved-edits-at-exit?)
    (letrec ((ignore-unsaved-edits-at-exit-1?
	      (lambda (snds)
		(or (null? snds)
		    (and (ignore-unsaved-edits-at-close? (car snds) #t)
			 (ignore-unsaved-edits-at-exit-1? (cdr snds)))))))
      (ignore-unsaved-edits-at-exit-1? (sounds))))
  
  (define (unsaved-edits-at-exit?) (not (ignore-unsaved-edits-at-exit?)))
  (define (unsaved-edits-at-close? snd) (not (ignore-unsaved-edits-at-close? snd #f)))
    
  (set! checking-for-unsaved-edits check)
  (if check
      (begin
	(if (not (member unsaved-edits-at-exit? (hook->list before-exit-hook)))
	    (add-hook! before-exit-hook unsaved-edits-at-exit?))
	(if (not (member unsaved-edits-at-close? (hook->list before-close-hook)))
	    (add-hook! before-close-hook unsaved-edits-at-close?)))
      (begin
	(remove-hook! before-exit-hook unsaved-edits-at-exit?)
	(remove-hook! before-close-hook unsaved-edits-at-close?))))


;;; -------- remember-sound-state

(define remembering-sound-state 0) ; for prefs
(define remember-sound-filename ".snd-remember-sound") ; should this be in the home directory?

(define* (remember-sound-state (choice 3))
  "(remember-sound-state) remembers the state of a sound when it is closed, and if it is subsquently re-opened, restores that state"

  (let ((states '())
	(sound-funcs (list sync with-tracking-cursor selected-channel show-controls read-only
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
			     show-transform-peaks fft-log-frequency fft-log-magnitude with-verbose-cursor zero-pad
			     wavelet-type min-dB transform-size transform-graph-type time-graph-type fft-window
			     transform-type transform-normalization time-graph-style show-mix-waveforms dot-size
			     x-axis-style show-axes graphs-horizontal lisp-graph-style transform-graph-style
			     grid-density tracking-cursor-style
			     )))

    (define (print-readably fd field depth first)
      (if (not first) (format fd " "))
      (if (string? field)
	  (if (= (string-length (format #f "~S" "1")) 3)
	      (format fd "~S" field)
	      (format fd "\"~S\"" field)) ; sometimes format omits the double quotes!
	  (if (number? field)
	      (if (rational? field) ; get these out of our way before float stuff
		  (format fd "~A" field)
		  (format fd "~,4F" field))
	      (if (procedure? field)
		  (format fd "~A" (procedure-source field))
		  (if (list? field)
		      (begin
			(if (or (= depth 1)
				(> (length field) 12))
			    (begin
			      (format fd "~%")
			      (do ((i 0 (+ 1 i)))
				  ((= i depth))
				(format fd "  "))))
			(format fd "(")
			(let ((fst #t))
			  (for-each 
			   (lambda (val)
			     (print-readably fd val (+ 1 depth) fst)
			     (set! fst #f))
			   field))
			(format fd ")"))
		      (format fd "~A" field))))))

    (define (find-if pred l)
      "(find-if func lst) scans 'lst' for any element that 'func' likes"
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l)))))

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

    (define (remember-sound-at-close snd)
      ;; save current state in list (name write-date (snd props) (chan props))
      (set! (saved-state snd)
	    (list (file-name snd)
		  (file-write-date (file-name snd))
		  (map (lambda (f) 
			 (f snd)) 
		       sound-funcs)
		  (map (lambda (sc)
			 (map (lambda (f)
				(if (equal? f transform-type) 
				    (transform->integer (f (car sc) (cadr sc))) 
				    (f (car sc) (cadr sc))))
			      channel-funcs))
		       (let ((scs '()))
			 (do ((i 0 (+ 1 i)))
			     ((= i (channels snd)))
			   (set! scs (cons (list snd i) scs)))
			 (reverse scs)))))
      #f)

    (define (remember-sound-at-open snd)
      ;; restore previous state, if any
      (let ((state (saved-state snd))) ; removes old state from current list
	(if (and state
		 (= (file-write-date (file-name snd)) (cadr state))
		 (= (channels snd) (length (cadddr state)))
		 (not (= choice 2)))
	    ;; we need the chans check because auto-test files seem to have confused write dates
	    (begin
	      (for-each (lambda (f val)
			  (set! (f snd) val))
			sound-funcs
			(caddr state))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn (channels snd)))
		(dynamic-wind
		    (lambda () (set! (squelch-update snd chn) #t))
		    (lambda ()
		      (for-each (lambda (f val)
				  (if (and (list? val)
					   (not (null? val))
					   (eq? (car val) 'lambda))
				      (set! (f snd chn) (eval val (current-environment)))
				      (if (equal? f transform-type) 
					  (set! (f snd chn) (integer->transform val)) 
					  (set! (f snd chn) val))))
				channel-funcs
				(list-ref (cadddr state) chn)))
		    (lambda () (set! (squelch-update snd chn) #f)))
		(if (time-graph? snd chn) (update-time-graph snd chn))
		(if (transform-graph? snd chn) (update-transform-graph snd chn)))))))

    (define (remember-sound-at-start filename)
      (if (and (null? states)
	       (file-exists? remember-sound-filename))
	  (begin
	    (load remember-sound-filename)
	    (set! states -saved-remember-sound-states-states-)))
      #f)

    (define (remember-sound-at-exit)
      (if (not (null? states))
	  (call-with-output-file remember-sound-filename
	    (lambda (fd)
	      (format fd "~%~%;;; from remember-sound-state in extensions.scm~%")
	      (format fd "(define -saved-remember-sound-states-states-~%  '")
	      (print-readably fd states 0 #t)
	      (format fd ")~%"))))
      #f)
      
    (if (or (= choice 0)  ; no remembering
	    (= choice 1)) ; just within-run remembering
	(begin
	  (if (= choice 0)
	      (begin
		(remove-hook! close-hook remember-sound-at-close)
		(remove-hook! after-open-hook remember-sound-at-open)))
	  (remove-hook! open-hook remember-sound-at-start)
	  (remove-hook! before-exit-hook remember-sound-at-exit)
	  (if (file-exists? remember-sound-filename)
	      (delete-file remember-sound-filename))))

    (if (not (= choice 0))
	(begin
	  (add-hook! close-hook remember-sound-at-close)
	  (add-hook! after-open-hook remember-sound-at-open)
	  (if (not (= choice 1))
	      (begin
		(add-hook! open-hook remember-sound-at-start)
		(add-hook! before-exit-hook remember-sound-at-exit)))))

    (set! remembering-sound-state choice)
    'remembering!))


;;; -------- mix-channel, insert-channel, c-channel

(define* (mix-channel input-data (beg 0) dur snd (chn 0) edpos with-tag)

  "(mix-channel file beg dur snd chn edpos with-tag) mixes in file. file can be the file name, a sound object, or \
a list (file-name-or-sound-object [beg [channel]])."

  (define (channel->mix input-snd input-chn input-beg input-len output-snd output-chn output-beg)
    (if (< input-len 1000000)
	(mix-vct (channel->vct input-beg input-len input-snd input-chn) output-beg output-snd output-chn #t)
	(let* ((output-name (snd-tempnam))
	       (output (new-sound output-name :size input-len))
	       (reader (make-sampler input-beg input-snd input-chn)))
	  (map-channel (lambda (val) 
			 (next-sample reader)) 
		       0 input-len output 0)
	  (save-sound output)
	  (close-sound output)
	  (mix output-name output-beg 0 output-snd output-chn #t #t))))

  (let* ((input (if (not (list? input-data)) 
		    input-data 
		    (car input-data)))
	 (input-beg (if (or (not (list? input-data))
			   (< (length input-data) 2)) 
		       0 
		       (cadr input-data)))
	 (input-channel (if (or (not (list? input-data))
			       (< (length input-data) 3))
			   0 
			   (caddr input-data)))
	 (len (or dur (- (if (string? input)
			     (frames input) 
			     (frames input input-channel))
			 input-beg)))
	 (start (or beg 0)))
    (if (< start 0) 
	(throw 'no-such-sample (list "mix-channel" beg))
	(if (> len 0)
	    (if (not with-tag)

		;; not a virtual mix
		(let ((reader (make-sampler input-beg input input-channel)))
		  (map-channel (lambda (val)
				 (+ val (next-sample reader)))
			       start len snd chn edpos
			       (if (string? input-data)
				   (format #f "mix-channel ~S ~A ~A" input-data beg dur)
				   (format #f "mix-channel '~A ~A ~A" input-data beg dur))))

		;; a virtual mix -- use simplest method available
		(if (sound? input)

		    ;; sound object case
		    (channel->mix input input-channel input-beg len snd chn start)

		    ;; file input
		    (if (and (= start 0) 
			     (= len (frames input)))

			;; mixing entire file
			(mix input start 0 snd chn #t #f) ; don't delete it!

			;; mixing part of file
			(let* ((output-name (snd-tempnam))
			       (output (new-sound output-name :size len))
			       (reader (make-sampler input-beg input input-channel)))
			  (map-channel (lambda (val) (next-sample reader)) 0 len output 0)
			  (save-sound output)
			  (close-sound output)
			  (mix output-name start 0 snd chn #t #t)))))))))


(define* (insert-channel file-data beg dur snd chn edpos)
  "(insert-channel file beg dur snd chn edpos) inserts the file. file can be the file name or a list (file-name [beg [channel]])"
  (let* ((file-name (if (string? file-data) file-data (car file-data)))
	 (file-beg (if (or (string? file-data) 
			   (< (length file-data) 2)) 
		       0 
		       (cadr file-data)))
	 (file-channel (if (or (string? file-data) 
			       (< (length file-data) 3))
			   0 
			   (caddr file-data)))
	 (len (or dur (- (frames file-name) file-beg)))
	 (start (or beg 0)))
    (if (< start 0) (throw 'no-such-sample (list "insert-channel" beg)))
    (if (> len 0)
	(let ((reader (make-sampler file-beg file-name file-channel))
	      (data (make-vct len)))
	  (vct-map! data (lambda () (next-sample reader)))
	  (insert-samples start len data snd chn edpos #f 
			  (if (string? file-data)
			      (format #f "insert-channel ~S ~A ~A" file-data beg dur)
			      (format #f "insert-channel '~A ~A ~A" file-data beg dur)))))))


;;; -------- redo-channel, undo-channel

(define* (redo-channel (edits 1) snd chn)
  "(redo-channel (edits 1) snd chn) is the regularized version of redo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (+ (edit-position snd chn) edits))
      (redo edits snd)))


(define* (undo-channel (edits 1) snd chn)
  "(undo-channel (edits 1) snd chn) is the regularized version of undo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (max 0 (- (edit-position snd chn) edits)))
      (undo edits snd)))


;;; -------- any-env-channel

(define* (any-env-channel env func (beg 0) dur snd chn edpos origin)
  "(any-env-channel env func (beg 0) dur snd chn edpos origin) takes breakpoints in 'env', \
connects them with 'func', and applies the result as an amplitude envelope to the given channel"
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
		 (do ((i 1 (+ 1 i))
		      (j 2 (+ j 2)))
		     ((= i pts))
		   (set! x0 x1)
		   (set! y0 y1)
		   (set! x1 (list-ref env j))
		   (set! y1 (list-ref env (+ 1 j)))
		   (set! ramp-dur (round (* dur (/ (- x1 x0) xrange))))
		   (if (= y0 y1)
		       (scale-channel y0 ramp-beg ramp-dur snd chn edpos)
		       (func y0 y1 ramp-beg ramp-dur snd chn edpos))
		   (set! ramp-beg (+ ramp-beg ramp-dur))))
	       origin))))))

;;; -------- sine-ramp sine-env-channel 

(define* (sine-ramp rmp0 rmp1 (beg 0) dur snd chn edpos)
  "(sine-ramp rmp0 rmp1 (beg 0) dur snd chn edpos) produces a sinsusoidal connection from rmp0 to rmp1"
  ;; vct: angle incr off scl
  (ptree-channel
   (lambda (y data forward)
     (let* ((angle (data 0))
	    (incr (data 1))
	    (val (* y (+ (data 2) (* (data 3) (+ 0.5 (* 0.5 (cos angle))))))))
       ;; this could be optimized into offset=off+scl/2 and scl=scl/2, then (* y (+ off (* scl cos)))
       (if forward
	   (set! (data 0) (+ angle incr))
	   (set! (data 0) (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ pi frag-dur)))
       (vct (+ (* -1.0 pi) (* frag-beg incr))
	    incr
	    rmp0
	    (- rmp1 rmp0))))
   (format #f "sine-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur)))


(define* (sine-env-channel env (beg 0) dur snd chn edpos)
  "(sine-env-channel env (beg 0) dur snd chn edpos) connects env's dots with sinusoids"
  (any-env-channel env sine-ramp beg dur snd chn edpos (format #f "sine-env-channel '~A ~A ~A" env beg dur)))

;;; (sine-env-channel '(0 0 1 1 2 -.5 3 1))

;;; an obvious extension of this idea is to use the blackman fft window formulas
;;;   to get sharper sinusoids (i.e. use the sum of n cosines, rather than just 1)


;;; -------- blackman4-ramp, blackman4-env-channel

(define* (blackman4-ramp rmp0 rmp1 (beg 0) dur snd chn edpos)
  "(blackman4-ramp rmp0 rmp1 (beg 0) dur snd chn edpos) produces a blackman4-shaped envelope"
  ;; vct: angle incr off scl
  (ptree-channel
   (lambda (y data forward)
     (let* ((angle (data 0))
	    (incr (data 1))
	    (cx (cos angle))
	    (val (* y (+ (data 2) 
			 (* (data 3) 
			    (+ .084037 (* cx (+ -.29145 (* cx (+ .375696 (* cx (+ -.20762 (* cx .041194)))))))))))))
       ;; blackman2 would be: (+ .34401 (* cx (+ -.49755 (* cx .15844))))
       (if forward
	   (set! (data 0) (+ angle incr))
	   (set! (data 0) (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ pi frag-dur)))
       (vct (* frag-beg incr)
	    incr
	    rmp0
	    (- rmp1 rmp0))))
   (format #f "blackman4-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur)))


(define* (blackman4-env-channel env (beg 0) dur snd chn edpos)
  "(blackman4-env-channel env (beg 0) dur snd chn edpos) uses the blackman4 window to connect the dots in 'env'"
  (any-env-channel env blackman4-ramp beg dur snd chn edpos (format #f "blackman4-env-channel '~A ~A ~A" env beg dur)))

;;; any curve can be used as the connecting line between envelope breakpoints in the
;;;   same manner -- set up each ramp to take the current position and increment,
;;;   then return the value in ptree-channel.  A simple one would have a table of
;;;   values and use array-interp.


;;; -------- ramp-squared, env-squared-channel

(define* (ramp-squared rmp0 rmp1 (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-squared rmp0 rmp1 (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^2 curve"
  ;; vct: start incr off scl
  (ptree-channel
   (lambda (y data forward)
     (let* ((angle (data 0))
	    (incr (data 1))
	    (val (* y (+ (data 2) (* angle angle (data 3))))))
       (if forward
	   (set! (data 0) (+ angle incr))
	   (set! (data 0) (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ 1.0 frag-dur)))
       (if (and symmetric
		(< rmp1 rmp0))
	   (vct (* (- frag-dur frag-beg) incr) (- incr) rmp1 (- rmp0 rmp1))
	   (vct (* frag-beg incr) incr rmp0 (- rmp1 rmp0)))))
   (format #f "ramp-squared ~A ~A ~A ~A ~A" rmp0 rmp1 symmetric beg dur)))


(define* (env-squared-channel env (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-squared-channel env (symmetric #t) (beg 0) dur snd chn edpos) connects env's dots with x^2 curves"
  (any-env-channel env 
		   (lambda (r0 r1 b d s c e)
		     (ramp-squared r0 r1 symmetric b d s c e))
		   beg dur snd chn edpos
		   (format #f "env-squared-channel '~A ~A ~A ~A" env symmetric beg dur)))

;;; (env-squared-channel '(0 0 1 1 2 -.5 3 1))


;;; -------- ramp-expt, env-expt-channel

(define* (ramp-expt rmp0 rmp1 exponent (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-expt rmp0 rmp1 exponent (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^exponent curve"
  ;; vct: start incr off scl exponent
  ;; a^x = exp(x * log(a))
  (ptree-channel
   (lambda (y data forward)
     (let* ((angle (data 0))
	    (incr (data 1))
	    (val (* y (+ (data 2) (* (exp (* (log angle) (data 4))) (data 3))))))
       (if forward
	   (set! (data 0) (+ angle incr))
	   (set! (data 0) (- angle incr)))
       val))
   beg dur snd chn edpos #t
   (lambda (frag-beg frag-dur)
     (let ((incr (/ 1.0 frag-dur)))
       (if (and symmetric
		(< rmp1 rmp0))
	   (vct (* (- frag-dur frag-beg) incr) (- incr) rmp1 (- rmp0 rmp1) exponent)
	   (vct (* frag-beg incr) incr rmp0 (- rmp1 rmp0) exponent))))
   (format #f "ramp-expt ~A ~A ~A ~A ~A ~A" rmp0 rmp1 exponent symmetric beg dur)))


(define* (env-expt-channel env exponent (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-expt-channel env exponent (symmetric #t) (beg 0) dur snd chn edpos) connects env's dots with x^exponent curves"
  (if (= exponent 1.0)
      (env-channel env beg dur snd chn edpos)
      (any-env-channel env 
		       (lambda (r0 r1 b d s c e)
			 (ramp-expt r0 r1 exponent symmetric b d s c e))
		       beg dur snd chn edpos
		       (format #f "env-expt-channel '~A ~A ~A ~A ~A" env exponent symmetric beg dur))))


;;; -------- offset-channel 

(define* (offset-channel dc (beg 0) dur snd chn edpos)
  "(offset-channel amount (beg 0) dur snd chn edpos) adds amount to each sample"
  (ptree-channel (lambda (y) (+ y dc)) beg dur snd chn edpos #t #f
		 (format #f "offset-channel ~A ~A ~A" dc beg dur)))


(define* (offset-sound off (beg 0) dur snd)
  "(offset-sound off beg dur snd) adds 'off' to every sample in 'snd'"
  ;; the pretty but slow way:
  ;; (map-sound (lambda (fr) (frame+ fr off)) beg dur snd)
  ;;
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (offset-channel off beg dur index chn)))
	(throw 'no-such-sound (list "offset-sound" snd)))))


;;; -------- pad-sound

(define* (pad-sound beg dur snd) 
  "(pad-sound beg dur snd) places a block of 'dur' zeros in every channel of 'snd' starting at 'beg'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (pad-channel beg dur index chn)))
	(throw 'no-such-sound (list "pad-sound" snd)))))


;;; -------- dither-channel

(define* (dither-channel (amount .00006) (beg 0) dur snd chn edpos)
  "(dither-channel (amount .00006) (beg 0) dur snd chn edpos) adds amount dither to each sample"
  (let ((dither (* .5 amount)))
    (ptree-channel (lambda (y) (+ y (mus-random dither) (mus-random dither))) beg dur snd chn edpos #t #f
		   (format #f "dither-channel ~,8F ~A ~A" amount beg dur))))


(define* (dither-sound (amount .00006) (beg 0) dur snd)
  "(dither-sound (amount .00006) beg dur snd) adds dithering to every channel of 'snd'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (dither-channel amount beg dur index chn)))
	(throw 'no-such-sound (list "dither-sound" snd)))))


;;; -------- contrast-channel

(define* (contrast-channel index (beg 0) dur snd chn edpos)
  "(contrast-channel index (beg 0) dur snd chn edpos) applies contrast enhancement to the sound"
  (ptree-channel
   (lambda (y)
     (sin (+ (* y 0.5 pi) (* index (sin (* y 2.0 pi))))))
   beg dur snd chn edpos #f #f
   (format #f "contrast-channel ~A ~A ~A" index beg dur)))


(define* (contrast-sound index (beg 0) dur snd)
  "(contrast-sound index beg dur snd) applies contrast-enhancement to every channel of 'snd'"
  (let ((ind (or snd (selected-sound) (car (sounds)))))
    (if (sound? ind)
	(let* ((out-chans (channels ind)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (contrast-channel index beg dur ind chn)))
	(throw 'no-such-sound (list "contrast-sound" snd)))))


;;; -------- scale-sound

(define* (scale-sound scl (beg 0) dur snd)
  "(scale-sound scl beg dur snd) multiplies every sample in 'snd' by 'scl'"
  ;; the slow way:
  ;; (map-sound (lambda (fr) (frame* fr scl)) beg dur snd))
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (scale-channel scl beg dur index chn)))
	(throw 'no-such-sound (list "scale-sound" snd)))))


;;; -------- normalize-sound

(define* (normalize-sound amp (beg 0) dur snd)
  "(normalize-sound amp beg dur snd) scales 'snd' to peak amplitude 'amp'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index))
	       (mx (apply max (maxamp index #t))))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (scale-channel (/ amp mx) beg dur index chn)))
	(throw 'no-such-sound (list "normalize-sound" snd)))))



#|
;;; -------- delay-channel 
;;;
;;; this is ok going forward (I think), but not in reverse

(define* (delay-channel dly (beg 0) dur snd chn edpos)
  "(delay-channel amount (beg 0) dur snd chn edpos) implements a delay using virtual edits"
  (let ((cur-edpos (if (or (not edpos)
			   (= edpos current-edit-position))
		       (edit-position snd chn)
		       edpos)))
    (ptree-channel (lambda (y data dir)
		     (let* ((pos (floor (data 0)))
			    (len (floor (data 1)))
			    (val (data (+ pos 2))))
		       (set! (data (+ pos 2)) y)
		       (set! pos (+ 1 pos))
		       (if (>= pos len) (set! (data 0) 0) (set! (data 0) pos))
		       val))
		   beg dur snd chn edpos #f
		   (lambda (fpos fdur)
		     (let ((data (make-vct (+ dly 2))))
		       (set! (data 0) 0.0)
		       (set! (data 1) dly)
		       (if (= fpos 0)
			   data
			   (let* ((reader (make-sampler (- fpos 1) snd chn -1 cur-edpos)))
			     (do ((i (- dly 1) (- i 1)))
				 ((< i 0))
			       (set! (data (+ i 2)) (reader)))
			     data)))))))
|#

;;; -------- channels-equal

(define* (channels=? snd1 chn1 snd2 chn2 (allowable-difference 0.0))
  "(channels=? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff) modulo trailing 0's"
  (if (and (equal? snd1 snd2)
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
		   (read2 (make-sampler 0 s2 c2)))
	      (not (scan-channel (lambda (y)
				   (let ((val (read-sample read2)))
				     (> (abs (- val y)) allowable-difference)))
				 0 len s1 c1)))))))


(define* (channels-equal? snd1 chn1 snd2 chn2 (allowable-difference 0.0))
  "(channels-equal? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff)"
  (let ((len1 (frames snd1 chn1))
	(len2 (frames snd2 chn2)))
    (if (not (= len1 len2))
	#f
	(channels=? snd1 chn1 snd2 chn2 allowable-difference))))


;;; -------- mono->stereo, mono-files->stereo

(define (mono->stereo new-name snd1 chn1 snd2 chn2)
  "(mono->stereo new-name snd1 chn1 snd2 chn2) takes the two channels and combines them into a stereo sound 'new-name'"
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
  "(mono-files->stereo new-name file1 file2) combines two mono files into the stereo file 'new-name'"
  ;; (mono-files->stereo "test.snd" "oboe.snd" "pistol.snd")
  (let* ((ind1 (open-sound chan1-name))
	 (ind2 (open-sound chan2-name))
	 (ind3 (mono->stereo new-name ind1 0 ind2 0)))
    (close-sound ind1)
    (close-sound ind2)
    ind3))


(define (stereo->mono orig-snd chan1-name chan2-name)
  "(stereo->mono stereo-sound new-chan1 new-chan2) splits a stereo sound into two mono sounds named 'new-chan1' and 'new-chan2'"
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




;;; -------- initial bounds 

(define prefs-show-full-duration #f) ; prefs dialog
(define prefs-initial-beg 0.0)
(define prefs-initial-dur 0.1)

(define (prefs-initial-bounds snd chn dur)
  "(prefs-initial-bounds snd chn dur) returns the current preferences dialog initial graph bounds"
  (list prefs-initial-beg
	(if prefs-show-full-duration
	    dur
	    (min prefs-initial-dur dur))))

(define (prefs-activate-initial-bounds beg dur full)
  "(prefs-activate-initial-bounds beg dur full) activates the preferences dialog initial graph bounds settings"
  (set! prefs-initial-beg beg)
  (set! prefs-initial-dur dur)
  (set! prefs-show-full-duration full)
  (add-hook! initial-graph-hook prefs-initial-bounds))

(define (prefs-deactivate-initial-bounds)
  "(prefs-deactivate-initial-bounds) deactivates the preferences dialog initial graph bounds settings"
  (set! prefs-initial-beg 0.0)
  (set! prefs-initial-dur 0.1)
  (set! prefs-show-full-duration #f)
  (remove-hook! initial-graph-hook prefs-initial-bounds))


;;; -------- reopen menu

(define including-reopen-menu #f) ; for prefs

(define (with-reopen-menu)
  (if (not including-reopen-menu)
      (let ((reopen-menu (add-to-main-menu "Reopen"))
	    (reopen-names '()))

	(define (add-to-reopen-menu snd)
	  (let ((brief-name (short-file-name snd))
		(long-name (file-name snd))
		(reopen-max-length 16)) ; sets max length of menu
	    (if (not (member brief-name reopen-names))
		(begin
		  (add-to-menu reopen-menu 
			       brief-name
			       (lambda () 
				 (remove-from-menu reopen-menu brief-name)
				 (open-sound long-name))
			       0) ; add to top
		  (set! reopen-names (append reopen-names (list brief-name)))
		  (if (> (length reopen-names) reopen-max-length)
		      (let ((goner (car reopen-names)))
			(set! reopen-names (cdr reopen-names))
			(remove-from-menu reopen-menu goner)))))))
	
	(define (check-reopen-menu filename)
	  (define (just-filename name)
	    (let ((last-slash -1)
		  (len (string-length name)))
	      (do ((i 0 (+ 1 i)))
		  ((= i len) (substring name (+ 1 last-slash)))
		(if (char=? (string-ref name i) #\/)
		    (set! last-slash i)))))
	  (let ((brief-name (just-filename filename)))
	    (if (member brief-name reopen-names)
		(set! reopen-names (remove-if (lambda (n) 
						(let ((val (string=? n brief-name)))
						  (if val (remove-from-menu reopen-menu brief-name))
						  val))
					      reopen-names))))
	  #f)
	
	(set! including-reopen-menu #t)
	(add-hook! close-hook add-to-reopen-menu)
	(add-hook! open-hook check-reopen-menu))))


;;; -------- global-sync (for prefs dialog)

(define global-sync-choice 0) ; global var so that we can reflect the current setting in prefs dialog
;; 0 = no sync, 1 = all synced, 2 = sync within sound

(define (global-sync-func snd)
  "(global-sync-func snd) is an after-open-hook function used by the preferences dialog"
  (if (= global-sync-choice 1)
      (set! (sync snd) 1)
      (if (= global-sync-choice 2)
	  (set! (sync snd) (+ 1 (sync-max))))))

(define (set-global-sync choice)
  "(set-global-sync choice) sets the preferences dialog global-sync choice"
  (set! global-sync-choice choice)
  (if (and (not (= choice 0))
	   (not (member global-sync-func (hook->list after-open-hook))))
      (add-hook! after-open-hook global-sync-func)))



;;; -------- with-threaded-channels
;;;
;;; experimental!

(define (with-threaded-channels func)
  (let ((chns (chans)))
    (if (and (provided? 'snd-threads)
	     (not (provided? 'gmp))
	     (provided? 'snd-nogui))
	
	;; use threads (s7 only)
	(let ((threads '()))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn chns))
	    (let ((lchn chn))
	      (set! threads (cons (make-thread 
				   (lambda () 
				     (func lchn))) 
				  threads))))
	  (for-each 
	   (lambda (expr) 
	     (join-thread expr))
	   threads))

	;; else do it the normal way
	(do ((chn 0 (+ 1 chn)))
	    ((= chn chns))
	  (func chn)))))
      

;;; -------- profiling

(define* (profile (file "sort.data"))
  ;; find all functions, write out each one's number of calls, sorted first by calls, then alphabetically 

  (if (provided? 'profiling)
      (let ((st (symbol-table))
	    (calls (make-vector 50000 #f))
	    (call 0))
	(do ((i 0 (+ i 1))) 
	    ((= i (length st)))
	  (let ((lst (st i)))
	    (for-each
	     (lambda (sym)
	       (if (and (defined? sym) 
			(procedure? (symbol->value sym)))
		   (begin
		     (set! (calls call) (list sym (symbol-calls sym)))
		     (set! call (+ call 1)))))
	     lst)))
	(let ((new-calls (make-vector call)))
	  (do ((i 0 (+ i 1)))
	      ((= i call))
	    (set! (new-calls i) (calls i)))
	  (let ((sorted-calls (sort! new-calls 
				     (lambda (a b) 
				       (or (> (cadr a) (cadr b))
					   (and (= (cadr a) (cadr b))
						(string<? (symbol->string (car a)) 
							  (symbol->string (car b)))))))))
	    (with-output-to-file file
	      (lambda ()
		(do ((i 0 (+ i 1)))
		    ((= i call))
		  (let ((c (sorted-calls i)))
		    (format #t "~A:~40T~A~%" (car c) (cadr c)))))))))))
