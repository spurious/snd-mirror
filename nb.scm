;;; provide pop-up help in the Files viewer
;;;   if use-gdbm is #t, any data associated with the file in the gdbm database will also be posted
;;;   the database name is defined by nb-database
;;;   the function (nb file note) adds note to the info currently associated with file
;;;   to remove a file's info, (forget file)

(define use-gdbm #f)
(define nb-database "nb.db")

(if use-gdbm (use-modules (gdbm gdbm)))

(define (nb file note)
  (let ((ptr (gdbm-open nb-database 'create)))
    (if (gdbm? ptr)
	(let ((current-note (and (gdbm-exists? ptr file)
				 (gdbm-fetch ptr file))))
	  (gdbm-store! ptr 
		       file 
		       (if (string? current-note)
			   (string-append note "\n" current-note)
			   note)
		       'replace)
	  (gdbm-close! ptr)))))

(define (forget file)
  (let ((ptr (gdbm-open nb-database 'write)))
    (if (gdbm? ptr)
	(begin
	  (gdbm-delete! ptr file)
	  (gdbm-close! ptr)))))

(define curent-file-viewer 0)
(define previous-file-viewer 1)
(define region-viewer 2)
(define (help-widget) (list-ref (dialog-widgets) 14))
(define alert-color (make-color 1.0 1.0 .94))

(define file-info
  (lambda (file)
    "(file-info file) -> description (as a string) of file"
    (format #f "~A:  ~%  chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F~%  written: ~A~A~A"
	    file
	    (mus-sound-chans file)
	    (mus-sound-srate file)
	    (mus-header-type-name (mus-sound-header-type file))
	    (mus-data-format-name (mus-sound-data-format file))
	    (/ (mus-sound-samples file)
	       (* (mus-sound-chans file) (mus-sound-srate file)))
	    (strftime "%d-%b %H:%M %Z" (localtime (mus-sound-write-date file)))
	    (let ((comment (mus-sound-comment file)))
	      (if (and (string? comment)
		       (> (string-length comment) 0))
		  (format #f "~%  comment: ~A" comment)
		  ""))
	    (if (and use-gdbm
		     (file-exists? nb-database))
		(let* ((ptr (gdbm-open nb-database 'read))
		       (note (gdbm-fetch ptr file)))
		  (gdbm-close! ptr)
		  (if (string? note)
		      (format #f "~%~A" note)
		      ""))
		""))))

(define (files-popup-info type position name)
  (if (not (= type region-viewer))
      (let ((help-exists (help-widget)))
	(help-dialog name (file-info name))
	(let ((widget (help-widget)))
	  (if widget
	      (begin
		(if (not help-exists) ; keep the help dialog from overlapping the files dialog
		    (let* ((files-dialog (list-ref (dialog-widgets) 8))
			   (files-position (widget-position files-dialog))
			   (files-size (widget-size files-dialog)))
		      (set! (widget-position widget) (list (+ (car files-position) (car files-size) 10)
							   (cadr files-position)))))
		(recolor-widget widget alert-color)))))))

(define (files-popup-quit type position name)
  (let ((widget (help-widget)))
    (if widget
	(recolor-widget widget (basic-color)))))

(add-hook! mouse-enter-label-hook files-popup-info)
(add-hook! mouse-leave-label-hook files-popup-quit)


