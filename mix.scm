;;; various mix related functions
;;;
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat) forces dragged mix to end up on a beat
;;; (silence-all-mixes) sets all mix amps to 0.0
;;; (find-mix sample snd chn) returns the id of the mix at the given sample, or #f
;;; (save-mix mix filename) saves mix data in file filename
;;; (mix-maxamp id) maxamp of mix
;;;
;;; mix-property associates a property list with a mix
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value

(use-modules (ice-9 common-list))
(provide 'snd-mix.scm)


(define (tree-for-each func tree)
  "(tree-for-each func tree) applies func to every leaf of 'tree'"
  (cond ((null? tree) '())
	((not (pair? tree)) (func tree))
	(else (tree-for-each func (car tree))
	      (tree-for-each func (cdr tree)))))

(define+ (tree-for-each-reversed func tree)
  "(tree-for-each-reversed func tree) applies func to every leaf of 'tree' moving in reverse through all the lists"
  (define (flatten lst)
    ;; there's probably a more elegant way to do this
    (define (list-p val)
      (and (list? val)
	   (not (null? val))))
    (cond ((null? lst) '())
	  ((list-p lst)
	   (if (list-p (car lst))
	       (append (flatten (car lst)) (flatten (cdr lst)))
	       (cons (car lst) (flatten (cdr lst)))))
	  (#t lst)))
  (for-each func (reverse (flatten tree))))

(define (mix-sound file start)
  "(mix-sound file start) mixes file (all chans) at start in the currently selected sound."
  (mix file start #t))

(define (silence-all-mixes)
  "(silence-all-mixes) sets all mix amps to 0"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
          (set! (mix-amp id) 0.0))
        (mixes)))))


(define* (find-mix sample :optional snd chn)
  "(find-mix sample :optional snd chn) returns the id of the mix at the given sample, or #f"
  (let ((mix-list (mixes (or snd (selected-sound) (car (sounds))) (or chn (selected-channel snd) 0))))
    (call-with-current-continuation
     (lambda (found-it)
       (for-each
	(lambda (n)
	  (if (= (mix-position n) sample)
	      (found-it n)))
	mix-list)
       #f))))


(define (mix->vct id)
  "(mix->vct id) returns mix's data in vct"
  (if (mix? id)
      (let* ((len (mix-length id))
	     (v (make-vct len))
	     (reader (make-mix-sample-reader id)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (vct-set! v i (read-mix-sample reader)))
	(free-sample-reader reader)
	v)
      (throw 'no-such-mix (list "mix->vct" id))))

;;; TODO: save-mix too large for vct */

(define (save-mix id filename)
  "(save-mix id filename) saves mix data (as floats) in file filename"
  (let ((v (mix->vct id))
	(fd (mus-sound-open-output filename (srate) 1 #f #f "")))
    (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
    (mus-sound-close-output fd (* 4 (vct-length v)))))

;;; TODO: mix-maxamp could easily be built-in (internal peak-env calc)

(define (mix-maxamp id)
  "(mix-maxamp id) returns the max amp in the given mix"
  (if (mix? id)
      (let* ((len (mix-length id))
	     (peak 0.0)
	     (reader (make-mix-sample-reader id)))
	(set! peak (abs (read-mix-sample reader)))
	(do ((i 1 (1+ i)))
	    ((= i len))
	  (let ((val (abs (read-mix-sample reader))))
	    (if (> val peak)
		(set! peak val))))
	(free-sample-reader reader)
	peak)
      (throw 'no-such-mix (list "mix-maxamp" id))))
	  

(define* (snap-mix-to-beat)
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  reset mix-release-hook to cancel"
  (add-hook! mix-release-hook
	     (lambda (id samps-moved)
	       (let* ((samp (+ samps-moved (mix-position id)))
		      (snd (car (mix-home id)))
		      (chn (cadr (mix-home id)))
		      (bps (/ (beats-per-minute snd chn) 60.0))
		      (sr (srate snd))
		      (beat (floor (/ (* samp bps) sr)))
		      (lower (inexact->exact (floor (/ (* beat sr) bps))))
		      (higher (inexact->exact (floor (/ (* (1+ beat) sr) bps)))))
		 (set! (mix-position id)
		       (if (< (- samp lower) (- higher samp))
			   (max 0 lower)
			   higher))
		 #t))))


;;; --------- mix-property

(define mix-property
  (make-procedure-with-setter
   (lambda (key id)
     "(mix-property key id) returns the value associated with 'key' in the given mix's property list, or #f"
     (if (mix? id)
	 (let ((data (assoc key (mix-properties id))))
	   (if data
	       (cdr data)
	       #f))
	 (throw 'no-such-mix (list "mix-property" id))))
   (lambda (key id new-val)
     (if (mix? id)
	 (let ((old-val (assoc key (mix-properties id))))
	   (if old-val
	       (set-cdr! old-val new-val)
	       (set! (mix-properties id) (cons (cons key new-val) (mix-properties id))))
	   new-val)
	 (throw 'no-such-mix (list "set! mix-property" id))))))


;;; -------- mix-click-sets-amp

(define (mix-click-sets-amp)
  (add-hook! mix-click-hook 
	     (lambda (n)
	       (let ((zeroed (mix-property :zero n)))
		 (if (not zeroed)
		     (begin
		       (set! (mix-property :amp n) (mix-amp n))
		       (set! (mix-amp n) 0.0)
		       (set! (mix-property :zero n) #t))
		     (begin
		       (set! (mix-amp n) (mix-property :amp n))
		       (set! (mix-property :zero n) #f)))
		 #t))))


;;; ---------- mix-click-info

(define (mix-click-info n)
  "(mix-click-info n) is a mix-click-hook function that describes a mix and its properties"
  (help-dialog "Mix Help"
	       (format #f "Mix ~A:~%  position: ~D = ~,3F secs~%  length: ~D (~,3F secs)
~%  in: ~A[~D]~%  scaler: ~A~%  speed: ~A~%  env: ~A~A"
		       (if (mix-name n)
			   (format #f "~S (~D)" (mix-name n) n)
			   (format #f "~D" n))
		       (mix-position n)
		       (exact->inexact (/ (mix-position n) (srate (car (mix-home n)))))
		       (mix-length n)
		       (exact->inexact (/ (mix-length n) (srate (car (mix-home n)))))
		       (short-file-name (car (mix-home n))) 
		       (cadr (mix-home n))
		       (mix-amp n)
		       (mix-speed n)
		       (mix-amp-env n)
		       (let ((props (mix-properties n)))
			 (if (and (list? props)
				  (not (null? props)))
			     (format #f "~%  properties: '~A" props)
			     ""))))
  #t)


;;; -------- mix-name->id

(define (mix-name->id name)
  "(mix-name->id name) returns the mix id associated with 'name'"
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (channels snd)))
	  (for-each
	   (lambda (m)
	     (if (and (string? (mix-name m))
		      (string=? (mix-name m) name))
		 (return m)))
	   (mixes snd chn))))
      (sounds))
     'no-such-mix)))


;;; ---------------- backwards compatibilty

(define (delete-mix id) 
  (set! (mix-amp id) 0.0))



;;; -------- mix lists (used to be "tracks"

(define (scale-mixes mix-list scl)
  (as-one-edit
   (lambda ()
     (for-each
      (lambda (m)
	(set! (mix-amp m) (* scl (mix-amp m))))
      mix-list))))

(define (silence-mixes mix-list)
  (scale-mixes mix-list 0.0))

(define (move-mixes mix-list samps)
  (as-one-edit
   (lambda ()
     (for-each
      (lambda (m)
	(set! (mix-position m) (+ (mix-position m) samps)))
      mix-list))))

(define (src-mixes mix-list sr)
  (as-one-edit
   (lambda ()
     (for-each
      (lambda (m)
	(set! (mix-speed m) (* (mix-speed m) sr)))
      mix-list))))

(define (transpose-mixes mix-list semitones)
  "(transpose-mixes mix-list semitones) transposes each mix in mix-list by semitones"
  (src-mixes mix-list (expt 2.0 (/ semitones 12.0))))

(define (color-mixes mix-list col)
  (for-each
   (lambda (m)
     (set! (mix-color m) col))
   mix-list))

(define (set-mixes-tag-y mix-list new-y)
  (for-each
   (lambda (m)
     (set! (mix-tag-y m) new-y))
   mix-list))
  
(define (mixes-maxamp mix-list)
  (let ((mx 0.0))
    (for-each
     (lambda (m)
       (set! mx (max mx (mix-maxamp m))))
     mix-list)
    mx))

(define (scale-tempo mix-list tempo-scl)
  (let* ((first-beg (mix-position (car mix-list)))
	 (last-beg first-beg))
    (for-each
     (lambda (m)
       (let ((pos (mix-position m)))
	 (set! first-beg (min first-beg pos))
	 (set! last-beg (max last-beg pos))))
     (cdr mix-list))
    (as-one-edit
     (lambda ()
       (for-each
	(lambda (m)
	  (let ((diff (inexact->exact (round (* tempo-scl (- (mix-position m) first-beg))))))
	    (if (not (= diff 0))
		(set! (mix-position m) (+ first-beg diff)))))
	mix-list)))))

;;; reverse-mix-list is (scale-tempo mix-list -1.0)


(define (mixes-length mix-list)
  (1+ (- (apply max (map (lambda (m) 
			   (+ (mix-position m) (mix-length m))) 
			 mix-list))
	 (apply min (map mix-position mix-list)))))

  
(define (save-mixes mix-list filename)
  (let* ((len (mixes-length mix-list))
	 (beg (apply min (map mix-position mix-list)))
	 (data (make-vct len)))
    (for-each
     (lambda (m)
       (vct-add! data (mix->vct m) (- (mix-position m) beg)))
     mix-list)
    (let ((fd (mus-sound-open-output filename (srate) 1 #f #f "")))
      (mus-sound-write fd 0 (1- len) 1 (vct->sound-data data))
      (mus-sound-close-output fd (* 4 (vct-length data))))))


;;; env-mixes, play-mixes
