;;; add some useful options to the Edit menu
;;;
;;; these used to be in the effects menu

(use-modules (ice-9 format))
(provide 'snd-edit-menu.scm)

(define edit-menu 1)


;;; -------- selection -> new file

(define (selection->new)
  "(selection-<new) saves the selection in a new file, then opens that file"
  (if (selection?)
      (let ((new-file-name (snd-tempnam)))
	(save-selection new-file-name)
	(open-sound new-file-name))
      #f))

(add-to-menu edit-menu "Selection->new" selection->new 8) ;pos=8 puts this in the selection section in the Edit menu


;;; -------- cut selection -> new file

(define (cut-selection->new)
  "(cut-selection->new) saves the selection, deletes it, then opens the saved file"
  (if (selection?)
      (let ((new-file-name (snd-tempnam)))
	(save-selection new-file-name)
	(delete-selection)
	(open-sound new-file-name))
      #f))

(add-to-menu edit-menu "Cut selection->new" cut-selection->new 9)


;;; -------- append selection

(define (append-selection)
  "(append-selection) appends the current selection"
  (if (selection?)
      (insert-selection (frames))))

(add-to-menu edit-menu "Append selection" append-selection 10)


;;; -------- make-stereofile
(define (make-stereofile)
  (let* ((ofile-name (file-name))
	 (old-sound (selected-sound))
	 (nsnd (new-sound (string-append ofile-name ".stereo") (header-type) (data-format) (srate) 2)))
    (if (not nsnd)
	(begin
	  (display "Could not make new sound.")(newline))
	(begin
	  (insert-sound ofile-name 0 0 nsnd 0)
	  (insert-sound ofile-name 0 (if (> 0 (channels old-sound)) 1 0) nsnd 1)))))

(add-to-menu edit-menu "Make Stereofile" make-stereofile)

;;; --------


(add-to-menu edit-menu #f #f)

;;; -------- trim front and back (goes by first or last mark)

(define (trim-front)
  "(trim-front) finds the first mark in each of the syncd channels and removes all samples before it"
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
	(trim-front-one-channel 
	 (or (selected-sound) (car (sounds))) 
	 (or (selected-channel) 0)))))

(add-to-menu edit-menu "Trim front" trim-front)

(define (trim-back)
  "(trim-back) finds the last mark in each of the syncd channels and removes all samples after it"
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
	(trim-back-one-channel 
	 (or (selected-sound) (car (sounds))) 
	 (or (selected-channel) 0)))))

(add-to-menu edit-menu "Trim back" trim-back)


;;; -------- crop (trims front and back)

(define* (crop-one-channel :optional snd chn)
  (if (< (length (marks snd chn)) 2)
      (report-in-minibuffer "crop needs start and end marks" snd)
      (as-one-edit
       (lambda ()
	 (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
	 (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	   (delete-samples (+ endpt 1) (- (frames snd chn) endpt))))
       "crop-one-channel")))

(define (crop)
  "(crop) finds the first and last marks in each of the syncd channels and removes all samples outside them"
  (let ((snc (sync)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (crop-one-channel snd chn)))
	       (all-chans))
	(crop-one-channel 
	 (or (selected-sound) (car (sounds)))
	 (or (selected-channel) 0)))))

(add-to-menu edit-menu "Crop" crop)


;;; -------- add these to the Edit menu, if possible

(if (and (not (provided? 'snd-gtk))
	 (provided? 'xm))
    (let* ((edit-cascade (list-ref (menu-widgets) 2))
	   (edit-menu (cadr (XtGetValues edit-cascade (list XmNsubMenuId 0)))))

      (XtAddCallback edit-cascade XmNcascadingCallback 
	(lambda (w c i)
	  (for-each-child 
	   edit-menu
	   (lambda (child)
	     (if (or (string=? (XtName child) "Selection->new")
		     (string=? (XtName child) "Cut selection->new")
		     (string=? (XtName child) "Append selection"))
		 (XtSetSensitive child (selection?))
		 (if (string=? (XtName child) "Crop")
		     (XtSetSensitive child (and (selected-sound)
						(> (length (marks (selected-sound) (selected-channel))) 1)))
		     (if (or (string=? (XtName child) "Trim front")
			     (string=? (XtName child) "Trim back"))
			 (XtSetSensitive child (and (selected-sound)
						    (>= (length (marks (selected-sound) (selected-channel))) 1))))))))))))


