;;; backwards compatibility for snd 11


;;; old Guile-style hook functions
(define (hook-empty? hook) 
  (null? (hook-functions hook)))

(define (reset-hook! hook) 
  (set! (hook-functions hook) ()))

(define (run-hook . args) 
  (hook-apply (car args) (cdr args)))

(define hook->list hook-functions)

(define* (add-hook! hook func (at-end #f))
  (set! (hook-functions hook)
	(if (not at-end)
	    (cons func (hook-functions hook))
	    (append (hook-functions hook) (list func)))))

(define (remove-hook! hook func)
  (set! (hook-functions hook)
	(let loop ((l (hook-functions hook))
		   (result ()))
	  (cond ((null? l) (reverse! result))
		((eq? func (car l)) (loop (cdr l) result))
		(else (loop (cdr l) (cons (car l) result)))))))


;;; these are carried forward from snd10.scm

(define sine-summation nrxysin)
(define sine-summation? nrxysin?)

(define sum-of-sines nsin)
(define sum-of-sines? nsin?)

(define sum-of-cosines ncos)
(define sum-of-cosines? ncos?)

(define* (make-sum-of-sines (sines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-nsin :frequency frequency :n sines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sum-of-cosines (cosines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-ncos :frequency frequency :n cosines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sine-summation (frequency 0.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0))
  (let ((gen (make-nrxysin :frequency frequency :ratio ratio :n n :r a)))
    (set! (mus-phase gen) initial-phase)
    gen))

(if (not (defined? 'in-hz)) (define in-hz hz->radians))

(define copy-sample-reader copy-sampler)
(define free-sample-reader free-sampler)
(define make-mix-sample-reader make-mix-sampler)
(define make-region-sample-reader make-region-sampler)
(define make-sample-reader make-sampler)
(define mix-sample-reader? mix-sampler?)
(define region-sample-reader? region-sampler?)
(define sample-reader-at-end? sampler-at-end?)
(define sample-reader-home sampler-home)
(define sample-reader? sampler?)
(define sample-reader-position sampler-position)



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
	 (if (char=? (file i) #\.)
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
							 ())
						     ',options))))
				 (set! *to-snd* old-to-snd)
				 (mus-mix *output* new-sound beg)
				 (if revf (mus-mix *reverb* revf beg)))))))))))))


#|
;;; this is the with-mix documentation:

<br>
<table border=0 bordercolor="lightgreen" width=50% cellpadding=1 cellspacing=0><tr><td bgcolor="lightgreen">
<table width=100% border=0><tr><td bgcolor="#EEFDEE" valign="middle"><h4>with-mix</h4></td></tr></table>
</td></tr></table>

<p><a name="with-mix">with-mix</a> is a "checkpointing" version of with-sound, more useful in the bad old days when computers
were incredibly slow, but lingering on...
It is a macro, callable within <a href="#wsdoc">with-sound</a> or <a href="#clmload">clm-load</a>,
which saves the computation in its body in a separate file, and
then upon a subsequent recomputation, tries to tell (via a string comparison) when that file's data is up to date
and does not need to be recomputed.
</p>

<table border=0 hspace=40 cellpadding=8 cellspacing=3><tr><td>
<pre>
(<a class=quiet href="#wsdoc" onmouseout="UnTip()" onmouseover="Tip(sndscm_wsdoc_tip)">with-sound</a> () 
  (fm-violin 0 .1 440 .1)
  (<em class=red>with-mix</em> () "sec1" .5 
    (fm-violin 0 .1 550 .1)
    (fm-violin .1 .1 660 .1))
  (<em class=red>with-mix</em> (:reverb jc-reverb) "sec2" 1.0
    (fm-violin 0 .1 880 .1 :reverb-amount .2)
    (fm-violin .1 .1 1320 .1 :reverb-amount .2))
  (fm-violin 2 .1 220 .1)
  (<em class=red>mix</em> "/zap/slow.snd"))
</pre>
</td></tr></table>

<p>Now, if we change just the first note in the with-mix call, the
second with-mix section will not be recomputed, but will be mixed in from the
saved file "sec2.snd".
In the old days, when notes took hours to compute, this was a big deal,
but not anymore.
</p>


;;; and these are the regression tests

  (define (check-with-mix num dur total-dur amp opts calls old-date chkmx)
    (let ((ind (find-sound "test.snd")))
      (if (not (sound? ind)) (snd-display #__line__ ";with-mix (~A) init: no test.snd?" num))
      (if (and chkmx (fneq (maxamp ind) amp)) (snd-display #__line__ ";with-mix (~A) maxamp: ~A (~A)" num (maxamp ind) amp))
      (if (not (file-exists? "with-mix.snd")) (snd-display #__line__ ";with-mix (~A) output doesn't exist" num))
      (let ((mx (mus-sound-maxamp "with-mix.snd"))
	    (date (mus-sound-write-date "with-mix.snd"))
	    (duration (mus-sound-duration "with-mix.snd")))
	(if (fneq duration dur) (snd-display #__line__ ";with-mix (~A) dur: ~A ~A" num dur duration))
	(if (fneq total-dur (/ (frames ind) (srate ind))) 
	    (snd-display #__line__ ";with-mix (~A) total dur: ~A ~A" num total-dur (/ (frames ind) (srate ind))))
	(if (and old-date
		 (> (- date old-date) 1)) ; these can be off by some amount in Linux
	    (snd-display #__line__ ";with-mix (~A) rewrote output?: ~A ~A ~A" num (- date old-date)
			 (strftime "%d-%b-%g %H:%M:%S" (localtime old-date))
			 (strftime "%d-%b-%g %H:%M:%S" (localtime date))))
	(if (and chkmx (or (not mx) (fneq (cadr mx) amp))) (snd-display #__line__ ";with-mix sndf (~A) maxamp: ~A (~A)" num mx amp))
	(let ((header-str (mus-sound-comment "with-mix.snd")))
	  (if (not (string? header-str)) (snd-display #__line__ ";with-mix (~A) comment unwritten?: ~A" num (mus-sound-comment "with-mix.snd")))
	  (let ((header (eval-string header-str)))
	    (if (not (list? header)) (snd-display #__line__ ";with-mix (~A) comment: ~A -> ~A" num header-str header))
	    (if (or (not (string=? (car header) opts))
		    (not (string=? (cadr header) calls)))
		(snd-display #__line__ ";with-mix (~A) header values: ~A" num header))))
	(close-sound ind)
	date)))
  
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (with-mix () "with-mix" 0 (fm-violin 0 .1 440 .1)))
    (let ((old-date (check-with-mix 1 .1 .1 .1 "()" "((fm-violin 0 0.1 440 0.1))" #f #t)))
      (with-sound () (with-mix () "with-mix" 0 (fm-violin 0 .1 440 .1)))
      (check-with-mix 1 .1 .1 .1 "()" "((fm-violin 0 0.1 440 0.1))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1)))
    (let ((old-date (check-with-mix 2 .1 .2 .1 "()" "((fm-violin 0 0.1 440 0.1))" #f #t)))
      (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1)))
      (check-with-mix 2 .1 .2 .1 "()" "((fm-violin 0 0.1 440 0.1))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1) (fm-violin .1 .1 660 .2)))
    (let ((old-date (check-with-mix 3 .2 .3 .2 "()" "((fm-violin 0 0.1 440 0.1) (fm-violin 0.1 0.1 660 0.2))" #f #t)))
      (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1) (fm-violin .1 .1 660 .2)))
      (check-with-mix 3 .2 .3 .2 "()" "((fm-violin 0 0.1 440 0.1) (fm-violin 0.1 0.1 660 0.2))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound ()
		(with-mix () "with-mix" 0
			  (sound-let ((tmp () (fm-violin 0 1 440 .1))) (mus-mix *output* tmp 0))))
    (let ((old-date (check-with-mix 4 1 1 .1 "()" "((sound-let ((tmp () (fm-violin 0 1 440 0.1))) (mus-mix *output* tmp 0)))" #f #t)))
      (with-sound ()
		  (with-mix () "with-mix" 0
			    (sound-let ((tmp () (fm-violin 0 1 440 .1))) (mus-mix *output* tmp 0))))
      (check-with-mix 4 1 1 .1 "()" "((sound-let ((tmp () (fm-violin 0 1 440 0.1))) (mus-mix *output* tmp 0)))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3 :degree 90)))
    (let ((ind (find-sound "test.snd")))
      (if (or (fneq (maxamp ind 0) .1)
	      (fneq (maxamp ind 1) .3))
	  (snd-display #__line__ ";with-mix stereo: ~A" (maxamp ind #t)))
      (if (not (= (mus-sound-chans "with-mix.snd") 2)) (snd-display #__line__ ";with-mix stereo out: ~A" (mus-sound-chans "with-mix.snd"))))
    (let ((old-date (mus-sound-write-date "with-mix.snd")))
      (with-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3 :degree 90)))
      (if (not (= (mus-sound-write-date "with-mix.snd") old-date)) 
	  (snd-display #__line__ ";stereo with-mix dates: ~A ~A" old-date (mus-sound-write-date "with-mix.snd"))))
    (let ((ind (find-sound "test.snd")))
      (close-sound ind))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound (:reverb jc-reverb) (fm-violin 0 .1 440 .1) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3)))
    (let ((old-date (check-with-mix 6 .1 1.1 .398 "()" "((fm-violin 0 0.1 550 0.3))" #f #f)))
      (with-sound (:reverb jc-reverb) (fm-violin 0 .1 440 .1) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3)))
      (check-with-mix 6 .1 1.1 .398 "()" "((fm-violin 0 0.1 550 0.3))" old-date #f))

|#

;;; --------------------------------------------------------------------------------


(define (focus-follows-mouse) (set! (with-pointer-focus) #t))
(define (make-current-window-display) (set! (with-inset-graph) #t))
(define load-from-path load)
;(define def-optkey-fun define*)
;(define def-optkey-instrument definstrument)

(define spectro-cutoff spectrum-start)
(define spectro-end spectrum-end)



(define* (play-region reg wait stop-func)
  (play (if (integer? reg) (integer->region reg) reg) :wait wait :stop stop-func))

(define* (play-selection wait stop-func)
  (play (selection) :wait wait :stop stop-func))

(define* (play-mix id (beg 0))
  (play (if (integer? id) (integer->mix id) id) beg))

(define* (play-and-wait (start 0) snd chn syncd end (pos -1) stop-proc)
  (if (string? start)
      (play start (or snd 0) :end (or chn -1) :wait #t) 
      (play (if (integer? snd) (integer->sound snd)
		(if (sound? snd) snd
		    (or (selected-sound) (car (sounds)))))
	    :channel (or chn -1) :wait #t :with-sync syncd :start start :end (or end -1) 
	    :stop stop-proc :edit-position pos)))

(define* (old-play (start 0) snd chn syncd end (pos -1) stop-proc (out-chan -1))
  (play (if (integer? snd) (integer->sound snd)
	    (if (sound? snd) snd
		(or (selected-sound) (car (sounds)))))
	:channel (or chn -1) :with-sync syncd :start start :end (or end -1) 
	:stop stop-proc :out-channel out-chan :edit-position pos))

(define* (play-channel (beg 0) dur snd chn (pos -1) stop-proc (out-chan -1))
  (play (if (integer? snd) (integer->sound snd)
	    (if (sound? snd) snd
		(or (selected-sound) (car (sounds)))))
	:channel (or chn -1) :with-sync #f :start beg :end (if dur (+ beg dur) -1) 
	:stop stop-proc :out-channel out-chan :edit-position pos))


(define quit-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define reset-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define help-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define do-it-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define do-it-again-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define pushed-button-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))


;;; --------------------------------------------------------------------------------

(define (add-watcher func)
  (hook-push effects-hook func))

(define (delete-watcher func)
  (hook-remove effects-hook func))


(define clear-selection unselect-all)



(define cursor-follows-play (make-procedure-with-setter
			     (lambda (snd)
			       (with-tracking-cursor))
			     (lambda (snd val)
			       (set! (with-tracking-cursor) val))))

