;;; a first look at using goops in Snd

(use-modules (oop goops))
(use-modules (ice-9 debug))
(use-modules (ice-9 session))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(debug-enable 'debug 'backtrace)
(read-enable 'positions)
(read-set! keywords 'prefix)


;;; beyond reducing names (i.e. mark-name, mix-name, file-name, etc)
;;;  is there a real use for this?
;;;    adds a level of customizability: new display funcs
;;;    provides possibility of extending built-in ops to these data types:
;;;      (+ vct vct) -> add-vct! (but needs to decide about allocations)
;;;      (+ snd snd) -> mix (but is this a new sound?)
;;;      (length vct) -> vct-length (frames, string-length)
;;;      (map func chn) -> map-chan?
;;;      but these all fall under name reduction
;;;    in clm, could finally use set! for (set! (mus-frequency osc) 440)


;;; -------- marks --------

(define-class mark ()
  (id :init-value -1 :accessor id))

(define-method initialize ((obj mark) initargs)
  ;; (make mark :sample :group :name :sound :channel) -- not sure about the latter two
  (next-method)
  (let* ((sample (get-keyword :sample initargs 0))
	 (snd (get-keyword :sound initargs #f))
	 (chn (get-keyword :channel initargs #f))
	 (name (get-keyword :name initargs ""))
	 (grp (get-keyword :group initargs 0))
	 (mid (add-mark sample snd chn)))
    (if (number? mid)
	(begin
	  (set! (id obj) mid)
	  (set-mark-name mid name)
	  (set-mark-sync mid grp)))
    obj))

(define-method write ((obj mark) port)
  (let ((mid (id obj)))
    (if (mark? mid)
	(display (format #f "<mark ~A: ~A~A~A>" 
			 mid 
			 (if (> (string-length (mark-name mid)) 0)
			     (format #f "~A, " (mark-name mid))
			     "")
			 (mark-sample mid)
			 (if (not (= (mark-sync mid) 0))
			     (format #f ", sync: ~A" (mark-sync mid))
			     ""))
		 port)
	(display (format #f "<mark ~S: inactive>" (id obj)) port))))

(define %sample sample)
(define-generic sample)
(define-method sample ((obj mark)) (mark-sample (id obj)))
;; or should this be the value of the data at the mark and use position for "mark-sample"?

(define-method name ((obj mark)) (mark-name (id obj)))
(define-method group ((obj mark)) (mark-sync (id obj)))
;; can't use sync unless we protect sync (guile built-in) somehow

(define %mark? mark?)
(define-generic mark?)
(define-method mark? ((obj mark)) #t)
(define-method mark? ((obj <integer>)) (%mark? obj))
(define-method mark? ((obj <top>)) #f)

;;; now (make mark :sample 123) places a mark at sample 123 returning a new mark object with id as returned by add-mark


;;; play


;;; -------- channel --------

;;; then sound is id plus list of channel objs?
;;; or chan includes sound obj?

;;; is it better to have these be unique (checking unique id# etc) or simply a reference to a position in the sound lists?


;;; -------- sounds --------

(define-class sound ()
  (id :init-value -1 :accessor id :init-keyword :id))

(define-method initialize ((obj sound) initargs)
  ;; (make sound :name) opens name
  (next-method)
  (let ((name (get-keyword :name initargs #f)))
    (if name
	(set! (id obj) (open-sound name)))
    obj))

(define-method name ((obj sound)) (file-name (id obj)))
(define-method group ((obj sound)) (syncing (id obj)))

;(define-method length ((obj sound)) (frames (id obj)))

(define-method sample ((obj sound) samp) (%sample samp (id obj)))

;; need a way to map from UI actions to associated (or newly created?) objects
;; (id->sound n) -> object associated with snd n? and id->mark etc?

;;; sound+channel as arg to all snd chn funcs

;;; need equivalent of looping through sounds, marks, chans, etc
;;; for-each? or (sounds) -> list of active sound as object?


;;; -------- regions --------

(define-class region ()
  (id :init-value -1))


;;; -------- mixes (tracks?) --------
;;; -------- transforms? --------
;;; -------- sample readers vcts? clm gens? envs? sound-data objs? --------
;;; -------- colors? widgets? (grouped as dialogs? (amp recorder-dialog...) --------

;;; sample obj
;;; make reader obj :position :direction?

(define-class reader ()
  (cursor))

(define-method initialize ((obj reader) initargs)
  (next-method)
  ;; :read? :position :direction
  obj)

(define-method sample ((obj reader))
  ;; now need to know which type of reader? -- subclasses of reader?
  )



;(define-method length ((obj <string>)) (string-length obj))
;; can't specialize length (yet?) -- a "simple procedure"


;;; name group position 
;;; play src filter env 
;;; amp maxamp move? length
;;; + sound sound -> mix? 


