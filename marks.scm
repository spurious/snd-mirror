;;; examples of mark-related functions

(use-modules (ice-9 format) (ice-9 common-list))
(provide 'snd-marks.scm)

(if (not (defined? 'find-if))
    (define (find-if pred l)
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l))))))

;;; Contents:
;;;     mark-name->id is a global version of find-mark
;;;     move-syncd-marks moves all syncd marks together
;;;     describe-mark shows mark history
;;;     click marks between start-sync and stop-sync to sync them together
;;;     synchronize sounds at a given mark
;;;     fit selection between marks, expanding via granulate (this needs some tweaking...)
;;;     pad-marks inserts silence before each in a list of marks
;;;     play-syncd-marks and play-between-marks
;;;     report-mark-names causes mark names to be posted in the minibuffer as a sound is played
;;;     eval-between-marks evaluates func between two marks
;;;     snap-marks places marks at current selection boundaries
;;;     define-selection-via-marks selects the portion between two marks
;;;     snap-mark-to-beat forces dragged mark to end up on a beat
;;;     mark-explode splits a sound into a bunch of sounds based on mark placements
;;;     mark-property implements property lists for marks
;;;     save-mark-properties sets up an after-save-state-hook function to save any mark-properties
;;;     mark-click-info is a mark-click-hook function that describes a mark and its properties


;;; -------- mark-name->id is a global version of find-mark

(define (mark-name->id name)
  "(mark-name->id name) is like find-mark but searches all currently accessible channels"
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (channels snd)))
	     (let ((mark (find-mark name snd chn)))
	       (if (mark? mark) 
		   (return mark)))))
      (sounds))
     'no-such-mark)))


;;; -------- move-syncd-marks moves all syncd marks together

(define (move-syncd-marks sync diff)
  "(move-syncd-marks sync diff) moves all marks sharing sync by diff samples"
  (for-each (lambda (m)
	      (set! (mark-sample m) (+ (mark-sample m) diff)))
	    (syncd-marks sync)))


;;; -------- describe-mark shows mark history

(define (describe-mark id)
  "(describe-mark id) returns a description of the movements of mark id over the channel's edit history"
  (let ((mark-setting (catch 'no-such-mark (lambda () (mark-home id)) (lambda args #f))))
    (if (not mark-setting)
        ;; not an active mark, so go scrounging for it
        ;;   we're looking at every edit of every channel
	(set! mark-setting (call-with-current-continuation
			    (lambda (return)
			      (for-each
			       (lambda (snd)
				 (do ((chn 0 (1+ chn)))
				     ((= chn (channels snd)))
				   (let* ((max-edits (apply + (edits snd chn))))
				     (do ((ed 0 (1+ ed)))
					 ((> ed max-edits))
				       (if (member id (marks snd chn ed))
					   (return (list snd chn)))))))
			       (sounds))
			      #f))))
    (if (list? mark-setting)
        (let* ((snd (car mark-setting))
               (chn (cadr mark-setting))
               (max-edits (apply + (edits snd chn)))
               (descr '())
               (header (list 'mark id 'sound snd (short-file-name snd) 'channel chn)))
          (do ((i max-edits (1- i)))
              ((< i 0) descr)
            (if (member id (marks snd chn i))
                (set! descr (cons (mark-sample id i) descr))
                (set! descr (cons #f descr))))
          (cons header descr))
        (throw 'no-such-mark (list "describe-mark" id)))))


;;; -------- click marks between start-sync and stop-sync to sync them together
;;;  (easier than calling mark-sync over and over by hand)

(define mark-sync-number 0)
(define (start-sync) (set! mark-sync-number (+ (mark-sync-max) 1)))
(define (stop-sync) (set! mark-sync-number 0))
(define (click-to-sync id) (set! (mark-sync id) mark-sync-number) #f)
;(add-hook! mark-click-hook click-to-sync)


;;; -------- syncronize sounds at a given mark

(define syncup
  (lambda ids
    "(syncup ids) pads the channels with zeros so that all the marks in ids list occur at the same time"
    (let* ((samps (map mark-sample ids))
	   (max-samp (apply max samps)))
      (define (pad-to-sync lst-ids lst-samps)
	(if (not (null? lst-ids))
	    (begin
	      (if (< (car lst-samps) max-samp)
		  (let ((nsamps (- max-samp (car lst-samps)))
			(snd-chn (mark-home (car lst-ids))))
		    (insert-samples 0 nsamps (make-vct nsamps) (car snd-chn) (cadr snd-chn))))
	      (pad-to-sync (cdr lst-ids) (cdr lst-samps)))))
      (pad-to-sync ids samps))))


;;; -------- fit selection between marks, expanding via granulate (this needs some tweaking...)

(define (fit-selection-between-marks m1 m2)
  "(fit-selection-between-marks m1 m2) fits (and mixes) the current selection (via granulate) between the given marks"
  (let* ((m1-samp (mark-sample m1))
	 (m2-samp (mark-sample m2))
	 (m1-home (mark-home m1))
	 (m2-home (mark-home m2)))
    (if (not (selection?))
	(throw 'no-active-selection))
    (if (not (equal? m1-home m2-home))
	(snd-print (format #f "mark ~A is in ~A[~A] but mark ~A is in ~A[~A]?" 
			   m1 (car m1-home) (cadr m1-home)
			   m2 (car m2-home) (cadr m2-home)))
	(let* ((mark-samps (- m2-samp m1-samp))
	       (selection-samps (selection-frames))
	       (reg-data (let ((data (make-vct selection-samps))
			       (rd (make-sample-reader (selection-position))))
			   (do ((i 0 (1+ i)))
			       ((= i selection-samps))
			     (vct-set! data i (rd)))
			   data))
	       (inctr 0))
	  (if (= mark-samps selection-samps)
	      (map-channel (lambda (y) 
			     (let ((val (+ y (vct-ref reg-data inctr))))
			       (set! inctr (1+ inctr))
			       val))
			   m1-samp mark-samps (car m1-home) (cadr m1-home))
	      (let* ((gr (make-granulate :expansion (/ mark-samps selection-samps))))
		(map-channel (lambda (y)
			       (+ y (granulate gr (lambda (dir)
						    (if (or (>= inctr selection-samps)
							    (< inctr 0))
							0.0
							(let ((val (vct-ref reg-data inctr)))
							  (set! inctr (+ inctr dir))
							  val))))))
			     m1-samp mark-samps (car m1-home) (cadr m1-home))))))))


;;; -------- pad-marks inserts silence before each in a list of marks

(define (pad-marks ids secs)
  "(pad-marks ids secs) inserts secs seconds of silence before each mark in ids"
  (let* ((silence-length (inexact->exact (floor (* secs (srate)))))
	 (silence-samps (make-vct silence-length)))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (n)
	  (let ((samp (max 0 (- (mark-sample n) 1)))
		(home (mark-home n)))
	    (insert-samples samp silence-length silence-samps (car home) (cadr home))))
	ids)))))


;;; -------- play-syncd-marks

(define (play-syncd-marks sync)
  "(play-syncd-marks sync) starts playing from all marks sharing sync"
  (let ((chans 1)
	(rate 22050))
    (for-each
     (lambda (m)
       (let* ((sound (car (mark-home m)))
	      (channel (cadr (mark-home m)))
	      (new-player (make-player sound channel)))
	 (add-player new-player (mark-sample m))
	 (set! chans (max chans (1+ channel)))
	 (set! rate (max rate (srate sound)))))
     (syncd-marks sync))
    (start-playing chans rate)))

(define play-between-marks
  (lambda args
    "(play-between-marks ...) plays the portion between the marks (searching for plausible default marks)"
    (let* ((snd (or (selected-sound) (car (sounds))))
	   (chn (or (selected-channel) 0))
	   (m1 (if (> (length args) 0)
		   (car args)
		   (let find-mark ((ms (marks snd chn)))
		     (if (null? ms)
			 (begin
			   (snd-print ";no marks in current window?")
			   #f)
			 (if (>= (mark-sample (car ms)) (left-sample snd chn))
			     (car ms)
			     (find-mark (cdr ms)))))))
	   (m2 (and (mark? m1)
		    (if (> (length args) 1)
			(cadr args)
			(let find-another-mark ((ms (marks snd chn)))
			  (if (null? ms)
			      (begin
				(snd-print ";no second mark?")
				#f)
			      (if (> (mark-sample (car ms)) (mark-sample m1))
				  (car ms)
				  (find-another-mark (cdr ms)))))))))
      (if (and (mark? m1)
	       (mark? m2))
	  (let* ((pos1 (mark-sample m1))
		 (pos2 (mark-sample m2))
		 (beg (min pos1 pos2))
		 (end (max pos1 pos2)))
	    (play beg
		  (car (mark-home m1)) 
		  (cadr (mark-home m1)) 
		  #f 
		  end))))))


;;; -------- report-mark-names causes mark names to be posted in the minibuffer as a sound is played

(if (not (provided? 'snd-hooks.scm)) (load-from-path "hooks.scm"))

(define (report-mark-names)
  "(report-mark-names) causes mark names to be printed as they are passed while playing"
  (add-hook! start-playing-hook 
	     (lambda (snd)
	       (let* ((marklist (marks snd 0))
		      (samplist (map mark-sample marklist))
		      (samp 0))
		 (define (report-mark-names-play-hook size)
		   (set! samp (+ samp size))
		   (if (and (not (null? samplist))
			    (>= samp (car samplist)))
		       (begin
			 (report-in-minibuffer (mark-name (car marklist)) snd)
			 (set! marklist (cdr marklist))
			 (set! samplist (cdr samplist)))))
		 (define (report-mark-names-stop-playing-hook snd)
		   (report-in-minibuffer "" snd)
		   (remove-local-hook! play-hook report-mark-names-play-hook)
		   (remove-local-hook! stop-playing-hook report-mark-names-stop-playing-hook))
		 
		 (add-hook! stop-playing-hook report-mark-names-stop-playing-hook)
		 (add-hook! play-hook report-mark-names-play-hook)
		 #f))))


;;; -------- eval-between-marks

(define+ (eval-between-marks func)
  "(eval-between-marks func) evaluates func between the leftmost marks; func takes one arg, the original sample"
  (define (find-if pred l) ; this is from guile/ice-9/common-list.scm but returns l not car l
    (cond ((null? l) #f)
	  ((pred (car l)) l)
	  (else (find-if pred (cdr l)))))
  (if (procedure? func)
      ;; find leftmost two marks in selected chn
      (let ((chan (selected-channel))
	    (snd (selected-sound)))
	(if (< chan 0) (set! chan 0)) ;perhaps not current sound, so no selected channel in it
	(let ((mlist (marks snd chan)))
	  (if (< (length mlist) 2)
	      (report-in-minibuffer "need 2 marks")
	      (let* ((left-samp (left-sample snd chan))
		     (winl (find-if (lambda (n) (> (mark-sample n) left-samp)) mlist)))
		(if (and winl (> (length winl) 1))
		    (let* ((beg (mark-sample (car winl)))
			   (len (- (mark-sample (cadr winl)) beg))
			   (new-data (make-vct len))
			   (old-data (channel->vct beg len snd chan)))
		      (do ((k 0 (1+ k)))
			  ((= k len) (vct->channel new-data beg len snd chan))
			(vct-set! new-data k (func (vct-ref old-data k))))))))))))

;(bind-key #\m 0 (lambda () "eval between marks" (prompt-in-minibuffer "mark eval:" eval-between-marks)))


;;; -------- snap-marks

(define (snap-marks)
  "snap-marks places marks at current selection boundaries"
  (let ((ms (list)))
    (if (selection?)
	(for-each 
	 (lambda (select)
	   (let ((pos (apply selection-position select))
		 (len (apply selection-frames select)))
	     (set! ms (cons (apply add-mark pos select) ms))
	     (set! ms (cons (apply add-mark (+ pos len) select) ms))))
	(selection-members)))
    (reverse ms)))


;;; -------- define-selection-via-marks

(define (define-selection-via-marks m1 m2)
  "(define-selection-via-marks m1 m2) defines the current selection to lie between the marks given"
  (let ((m1sc (mark-home m1))
	(m2sc (mark-home m2)))
    (if (not (equal? m1sc m2sc))
	(snd-error "define-selection-via-marks assumes the marks are in the same channel")
	(let ((beg (min (mark-sample m1) (mark-sample m2)))
	      (end (max (mark-sample m1) (mark-sample m2)))
	      (snd (car m1sc))
	      (chn (cadr m1sc)))
	  (if (selection?)
	      (set! (selection-member? #t) #f)) ; clear entire current selection, if any
	  (set! (selection-member? snd chn) #t)
	  (set! (selection-position snd chn) beg)
	  (set! (selection-frames snd chn) (1+ (- end beg)))))))


;;; -------- snap-mark-to-beat

(define (snap-mark-to-beat)
  "(snap-mark-to-beat) ensures that when a mark is dragged, its released position is always on a beat"
  (let ((mark-release 4))
    (add-hook! mark-hook 
	       (lambda (mrk snd chn reason)
		 (if (= reason mark-release)
		     (let* ((samp (mark-sample mrk))
			    (bps (/ (beats-per-minute snd chn) 60.0))
			    (sr (srate snd))
			    (beat (floor (/ (* samp bps) sr)))
			    (lower (inexact->exact (floor (/ (* beat sr) bps))))
			    (higher (inexact->exact (floor (/ (* (1+ beat) sr) bps)))))
		       (set! (mark-sample mrk)
			     (if (< (- samp lower) (- higher samp))
				 lower
				 higher))))))))

;;; -------- mark-explode
;;;
;;; write out each section of a file between marks as a separate file

(define* (mark-explode :optional (htype mus-next) (dformat mus-bfloat))
  "(mark-explode :optional header-type data-format) splits a sound into a bunch of sounds based on mark placements"
  (let ((start 0)
	(file-ctr 0)
	(snd (or (selected-sound) (car (sounds)))))
    (for-each
     (lambda (mark)
       (let ((end (mark-sample mark)))
	 (if (> end start)
	     (let ((filename (format #f "mark-~D.snd" file-ctr)))
	       (set! file-ctr (1+ file-ctr))
	       (do ((i 0 (1+ i)))
		   ((= i (chans snd)))
		 (set! (selection-member? snd i) #t)
		 (set! (selection-position snd i) start)
		 (set! (selection-frames snd i) (- end start)))
	       (save-selection filename :header-type htype :data-format dformat :srate (srate snd))
	       (do ((i 0 (1+ i)))
		   ((= i (chans snd)))
		 (set! (selection-member? snd i) #f))))
	 (set! start end)))
     (car (marks snd)))
    (update-time-graph snd)))


;;; -------- mark property lists

(define all-mark-properties '())

(define mark-properties
  (make-procedure-with-setter
   (lambda (id)
     "(mark-properties id) accesses the properties of mark 'id'"
     (let ((data (assoc id all-mark-properties)))
       (if data
	   (cdr data)
           '())))
   (lambda (id new-val)
     (let ((old-val (assoc id all-mark-properties)))
       (if old-val
	   (set-cdr! old-val new-val)
	   (set! all-mark-properties (cons (cons id new-val) all-mark-properties)))
       new-val))))
     
(define mark-property
  (make-procedure-with-setter
   (lambda (key id)
     "(mark-property key id) returns the value associated with 'key' in the given mark's property list, or #f"
     (if (mark? id)
	 (let ((data (assoc key (mark-properties id))))
	   (if data
	       (cdr data)
	       #f))
	 (throw 'no-such-mark (list "mark-property" id))))
   (lambda (key id new-val)
     (if (mark? id)
	 (let ((old-val (assoc key (mark-properties id))))
	   (if old-val
	       (set-cdr! old-val new-val)
	       (set! (mark-properties id) (cons (cons key new-val) (mark-properties id))))
	   new-val)
	 (throw 'no-such-mark (list "set! mark-property" id))))))

(add-hook! close-hook
	   (lambda (snd)
	     (if (not (null? all-mark-properties))
		 ;; prune out inactive mark properties
		 (set! all-mark-properties (remove-if (lambda (val)
						       (not (mark? (car val))))
						     all-mark-properties)))))

(if (not (defined? 'open-appending))
    (define (open-appending filename)
      (if (provided? 'snd-guile)
	  (open filename (logior O_RDWR O_APPEND))
	  (open-output-file filename :if-exists :append :if-does-not-exist :create))))

(if (not (defined? 'close-appending))
    (define (close-appending fd)
      (if (provided? 'snd-guile)
	  (close fd)
	  (close-output-port fd))))

(define (save-mark-properties)
  "(save-mark-properties) sets up an after-save-state-hook function to save any mark-properties"
  (if (defined? 'open)
  (add-hook! after-save-state-hook 
    (lambda (filename)
      (let ((fd (open-appending filename)))
	(format fd "~%~%;;; from remember-mark-properties in marks.scm~%")
	(format fd "(if (not (defined? 'mark-property)) (load-from-path \"marks.scm\"))~%")
	(for-each 
	 (lambda (snd-m)
	   (for-each 
	    (lambda (chn-m)
	      (for-each
	       (lambda (m)
		 (let ((mp (mark-properties m)))
		   (if (and mp
			    (list? mp)
			    (not (null? mp)))
		       (let ((mhome (mark-home m))
			     (msamp (mark-sample m)))
			 (format fd "(let ((s (find-sound ~S)))~%" (file-name (car mhome)))
			 (format fd "  (if (sound? s)~%")
			 (format fd "      (let ((m (find-mark ~A s ~A)))~%" msamp (cadr mhome))
			 (format fd "        (if (mark? m)~%")
			 (format fd "            (set! (mark-properties m) '~A)))))~%" mp)))))
	       chn-m))
	    snd-m))
	 (marks))
	(close-appending fd))))))


(define (mark-click-info n)
  "(mark-click-info n) is a mark-click-hook function that describes a mark and its properties"
  (help-dialog "Mark Help"
	       (format #f "Mark ~D~A:~%  sample: ~D = ~,3F secs~A~A"
		       n 
		       (let ((name (mark-name n)))
			 (if (> (string-length name) 0)
			     (format #f " (~S)" name)
			     ""))
		       (mark-sample n)
		       (/ (mark-sample n) (srate (car (mark-home n))))
		       (if (not (= (mark-sync n) 0))
			   (format #f "~%  sync: ~A" (mark-sync n))
			   "")
		       (let ((props (mark-properties n)))
			 (if (and (list? props)
				  (not (null? props)))
			     (format #f "~%  properties: '~A" props)
			     ""))))
  #t)


#|
;;; this code saves mark info in the sound file header, and reads it back in when the sound is later reopened

(define (eval-header sndf)
  (and (string? (comment sndf))
       (catch #t
	      (lambda ()
		(eval-string (comment sndf)))
	      (lambda args #f))))

(define (marks->string sndf)
  (let ((str (format #f "(if (not (provided? 'snd-marks.scm)) (load-from-path \"marks.scm\"))~%(let ((m #f))~%"))
	(chan 0))
    (for-each
     (lambda (chan-marks)
       (for-each 
	(lambda (m)
	  (set! str 
		(string-append str 
			       (format #f
				       "  (set! m (add-mark ~A #f ~D ~A ~D))~%" 
				       (mark-sample m)
				       chan
				       (if (and (string? (mark-name m))
						(> (string-length (mark-name m)) 0))
					   (format #f "~S" (mark-name m))
					   #f)
				       (mark-sync m))))
	  (if (not (null? (mark-properties m)))
	      (set! str
		    (string-append str 
				   (format #f
					   "  (set! (mark-properties m) '~A)~%"
					   (mark-properties m))))))
	  chan-marks)
       (set! chan (1+ chan)))
     (marks sndf))
    (string-append str (format #f "  m)~%"))))
		   
(add-hook! output-comment-hook (lambda (str) (marks->string (selected-sound))))
(add-hook! after-open-hook (lambda (snd) (eval-header snd)))
|#



