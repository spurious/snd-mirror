;;; provide pop-up help in the Files viewer
;;;   if use-gdbm is #t, any data associated with the file in the gdbm database will also be posted
;;;   the database name is defined by nb-database
;;;   the function (nb file note) adds note to the info currently associated with file
;;;   to remove a file's info, (unb file)
;;;   to clean non-existent file references out of the database, (prune-db)

(use-modules (ice-9 format))

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

(define (unb file)
  (let ((ptr (gdbm-open nb-database 'write)))
    (if (gdbm? ptr)
	(begin
	  (gdbm-delete! ptr file)
	  (gdbm-close! ptr)))))

(define (prune-db)
  (define (collect-files ptr key files)
    (if key
	(collect-files ptr (gdbm-next-key ptr key) (cons key files))
	files))
  (define (prune-file ptr files)
    (if (not (null? files))
	(begin
	  (if (not (file-exists? (car files)))
	      (begin
		(snd-print (format #f "pruning ~A" (car files)))
		(gdbm-delete! ptr (car files))))
	  (prune-file ptr (cdr files)))))
  (let ((ptr (gdbm-open nb-database 'read)))
    (if (gdbm? ptr)
	(let ((files (collect-files ptr (gdbm-first-key ptr) '())))
	  (gdbm-close! ptr)
	  (if (not (null? files))
	      (let ((ptr (gdbm-open nb-database 'write)))
		(prune-file ptr files)
		(gdbm-close! ptr)))))))


(define (files-popup-info type position name)

    (define file-info
      (lambda (file)
	"(file-info file) -> description (as a string) of file"
	(format #f "~A:  ~%  chans: ~D, srate: ~D, len: ~A~%  ~A ~A~A~%  written: ~A~A~A"
		file
		(mus-sound-chans file)
		(mus-sound-srate file)
		(let ((den (* (mus-sound-chans file) (mus-sound-srate file))))
		  (if (> den 0)
		      (format #f "~1,3F" (/ (mus-sound-samples file) den))
		      "unknown"))
		(mus-header-type-name (mus-sound-header-type file))
		(mus-data-format-name (mus-sound-data-format file))
		(if (mus-sound-max-amp-exists? file)
		    (format #f "~%  maxamp: ~A" (mus-sound-max-amp file))
		    "")
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

  (let ((alert-color (make-color 1.0 1.0 .94))
	(current-file-viewer 0)
	(previous-file-viewer 1)
	(region-viewer 2))
    (if (not (= type region-viewer))
	(let ((help-exists (list-ref (dialog-widgets) 14)))
	  (help-dialog name (file-info name))
	  (let ((help-widget (list-ref (dialog-widgets) 14)))
	    (if help-widget
		(begin
		  (if (not help-exists) ; keep the help dialog from overlapping the files dialog
		      (let* ((files-dialog (list-ref (dialog-widgets) 8))
			     (files-position (widget-position files-dialog))
			     (files-size (widget-size files-dialog)))
			(set! (widget-position help-widget) (list (+ (car files-position) (car files-size) 10)
								  (+ (cadr files-position) 10)))))
		  (recolor-widget help-widget alert-color))))))))

(define (files-popup-quit type position name)
  (let ((widget (list-ref (dialog-widgets) 14)))
    (if widget
	(recolor-widget widget (basic-color)))))

(add-hook! mouse-enter-label-hook files-popup-info)
(add-hook! mouse-leave-label-hook files-popup-quit)


