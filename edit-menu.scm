;;; add some useful options to the Edit menu
;;;
;;; these used to be in the effects menu


(use-modules (ice-9 format))

(if (not (defined? 'all-chans))
    (define (all-chans)
      (let ((sndlist '())
	    (chnlist '()))
	(for-each (lambda (snd)
		    (do ((i (1- (channels snd)) (1- i)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))

(define edit-menu 1)

;(add-to-menu edit-menu #f #f)


;;; -------- selection -> new file

(define selctr 0)

(define (selection->new)
  (if (selection?)
      (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	(set! selctr (+ selctr 1))
	(save-selection new-file-name)
	(open-sound new-file-name))))

(add-to-menu edit-menu "Selection->new" selection->new 8) ;pos=8 puts this in the selection section in the Edit menu


;;; -------- cut selection -> new file

(define (cut-selection->new)
  (if (selection?)
      (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	(set! selctr (+ selctr 1))
	(save-selection new-file-name)
	(delete-selection)
	(open-sound new-file-name))))

(add-to-menu edit-menu "Cut selection->new" cut-selection->new 9)

;;; -------- append sound (and append selection for lafs)

(define (append-sound name)
  ;; appends sound file
  (insert-sound name (frames)))

(define (append-selection)
  (if (selection?)
      (insert-selection (frames))))

(add-to-menu edit-menu "Append selection" append-selection 10)


(add-to-menu edit-menu #f #f)

;;; -------- trim front and back (goes by first or last mark)
(define (trim-front)
  "trim-front finds the first mark in each of the syncd channels and removes all samples before it"
  (let ((snc (sync)))
    (define (trim-front-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-front needs a mark" snd)
	  (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (trim-front-one-channel snd chn)))
	       (all-chans))
	(trim-front-one-channel (selected-sound) (selected-channel)))))

(add-to-menu edit-menu "Trim front" trim-front)

(define (trim-back)
  "trim-back finds the last mark in each of the syncd channels and removes all samples after it"
  (let ((snc (sync)))
    (define (trim-back-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-back needs a mark" snd)
	  (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	    (delete-samples (+ endpt 1) (- (frames snd chn) endpt)))))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (trim-back-one-channel snd chn)))
	       (all-chans))
	(trim-back-one-channel (selected-sound) (selected-channel)))))

(add-to-menu edit-menu "Trim back" trim-back)


;;; -------- crop (trims front and back)
(define (crop)
  "crop finds the first and last marks in each of the syncd channels and removes all samples outside them"
  (let ((snc (sync)))
    (define (crop-one-channel snd chn)
      (if (< (length (marks snd chn)) 2)
	  (report-in-minibuffer "crop needs start and end marks" snd)
	  (as-one-edit
	   (lambda ()
	     (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
	     (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	       (delete-samples (+ endpt 1) (- (frames snd chn) endpt))))
	   "crop")))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (crop-one-channel snd chn)))
	       (all-chans))
	(crop-one-channel (selected-sound) (selected-channel)))))

(add-to-menu edit-menu "Crop" crop)

(if (provided? 'xm)
    (let* ((edit-cascade (|Widget (list-ref (menu-widgets) 2)))
	   (edit-menu (cadr (|XtGetValues edit-cascade (list |XmNsubMenuId 0)))))

      (define (for-each-child w func)
	(func w)
	(if (|XtIsComposite w)
	    (for-each 
	     (lambda (n)
	       (for-each-child n func))
	     (cadr (|XtGetValues w (list |XmNchildren 0) 1)))))
      
      (|XtAddCallback edit-cascade |XmNcascadingCallback 
	(lambda (w c i)
	  (for-each-child 
	   edit-menu
	   (lambda (child)
	     (if (or (string=? (|XtName child) "Selection->new")
		     (string=? (|XtName child) "Cut selection->new")
		     (string=? (|XtName child) "Append selection"))
		 (|XtSetSensitive child (selection?))
		 (if (or (string=? (|XtName child) "Crop")
			 (string=? (|XtName child) "Trim front")
			 (string=? (|XtName child) "Trim back"))
		     (|XtSetSensitive child (> (length (marks (selected-sound) (selected-channel))) 1))))))))))


