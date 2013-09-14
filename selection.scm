;;; selection.scm -- selection-related functions
;;;
;;;   swap-selection-channels
;;;   replace-with-selection
;;;   selection-members
;;;   make-selection
;;;   filter-selection-and-smooth
;;;   with-temporary-selection
;;;
;;;
;;; see also make-selection-frame-reader (frame.scm)
;;;          selection->sound-data (frame.scm)
;;;          selection-rms (examp.scm)
;;;          fit-selection-between-marks (marks.scm)
;;;          define-selection-via-marks (marks.scm)
;;;          pan-mix-selection (mix.scm)

(provide 'snd-selection.scm)

(if (not (defined? 'all-chans))
    (define (all-chans)
      (let ((sndlist ())
	    (chnlist ()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))



;;; -------- swap selection chans

(define (swap-selection-channels)
  "(swap-selection-channels) swaps the currently selected data's channels"

  (define find-selection-sound 
    (lambda (not-this)
      (let ((scs (all-chans)))
	(call-with-exit
	 (lambda (return)
	   (map 
	    (lambda (snd chn)
	      (if (and (selection-member? snd chn)
		       (or (null? not-this)
			   (not (equal? snd (car not-this)))
			   (not (= chn (cadr not-this)))))
		  (return (list snd chn))))
	    (car scs)
	    (cadr scs)))))))

  (if (selection?)
      (if (= (selection-chans) 2)
	  (let* ((beg (selection-position))
		 (len (selection-frames))
		 (snd-chn0 (find-selection-sound ()))
		 (snd-chn1 (find-selection-sound snd-chn0)))
	    (if snd-chn1
		(swap-channels (car snd-chn0) (cadr snd-chn0) (car snd-chn1) (cadr snd-chn1) beg len)
		(error 'wrong-number-of-channels "swap-selection-channels needs two channels to swap")))
	  (error 'wrong-number-of-channels "swap-selection-channels needs a stereo selection"))
      (error 'no-active-selection "swap-selection-channels needs a selection")))



;;; -------- replace-with-selection

(define (replace-with-selection)
  "(replace-with-selection) replaces the samples from the cursor with the current selection"
  (let ((beg (cursor))
	(len (selection-frames)))
    (insert-selection beg) ; put in the selection before deletion, since delete-samples can deactivate the selection
    (delete-samples (+ beg len) len)))



;;; -------- selection-members
;;;
;;; returns a list of lists of (snd chn): channels in current selection

(define (selection-members)
  "(selection-members) -> list of lists of (snd chn) indicating the channels participating in the current selection."
  (let ((sndlist ()))
    (if (selection?)
	(map (lambda (snd)
	       (do ((i (- (channels snd) 1) (- i 1)))
		   ((< i 0))
		 (if (selection-member? snd i)
		     (set! sndlist (cons (list snd i) sndlist)))))
	     (sounds)))
    sndlist))


;;; -------- make-selection

;;; the regularized form of this would use dur not end

(define* (make-selection beg end snd chn)
  "(make-selection beg end snd chn) makes a selection like make-region but without creating a region.
make-selection follows snd's sync field, and applies to all snd's channels if chn is not specified. end defaults
to end of channel, beg defaults to 0, snd defaults to the currently selected sound."

  (let ((current-sound (or snd (selected-sound) (car (sounds)))))

    (define (add-chan-to-selection s0 s1 s c)
      (set! (selection-member? s c) #t)
      (set! (selection-position s c) (or s0 0))
      (set! (selection-frames s c) (- (or (and (number? s1) (+ 1 s1)) (frames s c)) (or s0 0))))

    (if (not (sound? current-sound))
	(error 'no-such-sound "make-selection can't find sound"))

    (let ((current-sync (sync current-sound)))
      (unselect-all)
      (if (number? chn)
	  (add-chan-to-selection beg end snd chn)
	  (for-each
	   (lambda (s)
	     (if (or (eq? snd #t)
		     (equal? s current-sound)
		     (and (not (= current-sync 0))
			  (= current-sync (sync s))))
		 (do ((i 0 (+ 1 i)))
		     ((= i (channels s)))
		   (add-chan-to-selection beg end s i))))
	   (sounds))))))



;;; -------- with-temporary-selection

(define (with-temporary-selection thunk beg dur snd chn)

  "(with-temporary-selection thunk beg dur snd chn) saves the current selection placement, makes a new selection \
of the data from sample beg to beg + dur in the given channel.  It then calls thunk, and
restores the previous selection (if any).  It returns whatever 'thunk' returned."

  (let ((seldata (and (selection?) 
		      (car (selection-members)))))
    (if (selection?)
	(set! seldata (append seldata (list (selection-position) (selection-frames)))))
    (make-selection beg (- (+ beg dur) 1) snd chn)
    (let ((result (thunk)))
      (if seldata
	  (make-selection (caddr seldata) 
			  (- (+ (caddr seldata) (cadddr seldata)) 1)
			  (car seldata)
			  (cadr seldata))
	  (unselect-all))
      result)))



;;; -------- filter-selection-and-smooth

(define* (filter-selection-and-smooth ramp-dur flt order)
  "(filter-selection-and-smooth ramp-dur flt order) applies 'flt' (via filter-sound) to \
the selection, the smooths the edges with a ramp whose duration is 'ramp-dur' (in seconds)"
  (let ((temp-file (snd-tempnam)))
    (save-selection temp-file)
    (let ((selsnd (open-sound temp-file)))
      (filter-sound flt (or order (length flt)) selsnd)
      (let ((tmp-dur (samples->seconds (frames selsnd))))
	(set! (sync selsnd) (+ 1 (sync-max))) ; make sure env-sound hits all chans
	(env-sound (list 0 0  ramp-dur 1  (- tmp-dur ramp-dur) 1  tmp-dur 0) 0 #f 1.0 selsnd)
	(save-sound selsnd)
	(close-sound selsnd)
	(env-selection (list 0 1  ramp-dur 0  (- tmp-dur ramp-dur) 0  tmp-dur 1))))
    (mix temp-file (selection-position) #t #f #f #f #f)))

;;; (filter-selection-and-smooth .01 (vct .25 .5 .5 .5 .25))
