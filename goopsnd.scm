;;; a first look at using goops in Snd

(use-modules (oop goops))
(use-modules (ice-9 debug))
(use-modules (ice-9 session))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(debug-enable 'debug 'backtrace)
(read-enable 'positions)


;;; beyond reducing names (i.e. mark-name, mix-name, file-name, etc)
;;;  is there a real use for this?
;;;    adds a level of customizability: new display funcs
;;;    provides possibility of extending built-in ops to these data types:
;;;      (+ vct vct) -> add-vct! (but needs to decide about allocations)
;;;      (+ snd snd) -> mix (but is this a new sound?)
;;;      (length vct) -> vct-length (frames, string-length)
;;;      (map func chn) -> map-chan?
;;;      but these all fall under name reduction
;;;  mus-copy


;;; -------- marks --------

(define-class mark ()
  (id :init-value -1 :accessor id))

(define-method (initialize (obj mark) initargs)
  ;; (make mark :sample :group :name :sound :channel) -- not sure about the latter two
  (next-method)
  (let* ((sample (get-keyword :sample initargs 0))
	 (snd (get-keyword :sound initargs #f))
	 (chn (get-keyword :channel initargs #f))
	 (name (get-keyword :name initargs ""))
	 (grp (get-keyword :group initargs 0))
	 (mid (add-mark sample snd chn)))
    (set! (id obj) mid)
    (set! (mark-name mid) name)
    (set! (mark-sync mid) grp)
    obj))

(define-method (write (obj mark) port)
  (let ((mid (id obj)))
    (display (format #f "<mark ~A: ~A~A~A>" 
		     mid 
		     (if (> (string-length (mark-name mid)) 0)
			 (format #f "~A, " (mark-name mid))
			 "")
		     (mark-sample mid)
		     (if (not (= (mark-sync mid) 0))
			 (format #f ", sync: ~A" (mark-sync mid))
			 ""))
	     port)))

