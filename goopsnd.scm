;;; a first look at using goops in Snd

(use-modules (oop goops))

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

;;; mus-copy?

(define-class fcmb ()
  (dly :accessor fcomb-delay)
  (flt :accessor fcomb-filter)
  (fdb :accessor fcomb-feedback))

(define (fcomb gen input)
  ((fcomb-delay gen) 
   (+ input (* ((fcomb-filter gen) 
		(tap (fcomb-delay gen)))
	       (fcomb-feedback gen)))))

(define-method (initialize (obj fcmb) initargs)
  (next-method)
  (let* ((len (get-keyword :length initargs 0))
	 (feedback (get-keyword :feedback initargs 0.5))
	 (a0 (get-keyword :a0 initargs 0.5))
	 (a1 (get-keyword :a1 initargs 0.5)))
    (set! (fcomb-delay obj) (make-delay len))
    (set! (fcomb-filter obj) (make-one-zero a0 a1))
    (set! (fcomb-feedback obj) feedback)
    obj))

(define-method (write (obj fcmb) port)
  (display (format #f "#<fcomb: delay: ~A, filter: ~A, feedback: ~A>"
		   (fcomb-delay obj)
		   (fcomb-filter obj)
		   (fcomb-feedback obj))
	   port))

;;; (define f0 (make fcmb :length 12 :feedback 0.7))
;;; (fcomb f0 1.0)


;;; TODO: goopsify clm2scm.c
