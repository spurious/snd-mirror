;;; provide pop-up help in the Files viewer

;(use-modules (gdbm gdbm))

;; TODO: move help dialog out of the way
;; TODO: use gdbm module!

(define curent-file-viewer 0)
(define previous-file-viewer 1)
(define region-viewer 2)
(define (help-widget) (list-ref (dialog-widgets) 14))
(define alert-color (make-color 1.0 1.0 .94))

(define file-info
  (lambda (file)
    "(file-info file) -> description (as a string) of file"
    (format #f "~A:  ~%  chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F~%  written: ~A~A"
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
		  "")))))

(define (files-popup-info type position name)
  (if (not (= type region-viewer))
      (begin
	(help-dialog name (file-info name))
	(let ((widget (help-widget)))
	  (if widget
	      (recolor-widget widget alert-color))))))

(define (files-popup-quit type position name)
  (let ((widget (help-widget)))
    (if widget
	(recolor-widget widget (basic-color)))))

(add-hook! mouse-enter-label-hook files-popup-info)
(add-hook! mouse-leave-label-hook files-popup-quit)


