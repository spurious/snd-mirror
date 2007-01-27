#!/usr/bin/guile -s
!#

;;; checkxg.scm checks the gtk2/gdk/pango/glib bindings in the snd sources, flagging any that are not gtk-2.0 compatible

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))
(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define ignore (list "gdk_pixmap_new" "gdk_pixmap_create_from_xpm" "gdk_pixmap_create_from_xpm_d"
		     "gtk_notebook_append_page" 
		     "g_timeout_add_full" "g_timeout_add" "g_idle_add" "g_idle_add_full" "g_source_remove"
;		     "GTK_FILE_CHOOSER_DIALOG" "GTK_FILE_FILTER"
;		     "GTK_FILE_CHOOSER" "GTK_FILE_FILTER_FILENAME" "GTK_FILE_FILTER_DISPLAY_NAME"
;		     "GTK_FILE_CHOOSER_ACTION_OPEN" "GTK_FILE_CHOOSER_ACTION_SAVE" "gtk_file_chooser_dialog_new"
;		     "gtk_file_filter_new" "gtk_file_filter_set_name" "gtk_file_filter_add_pattern"
;		     "gtk_file_filter_add_custom" "gtk_file_chooser_set_select_multiple"
;		     "gtk_file_chooser_get_filename" "gtk_file_chooser_set_extra_widget"
;		     "gtk_file_chooser_add_filter" "gtk_file_chooser_set_filter"
		     ))

(define (shell cmd)
  (with-output-to-string
    (lambda ()
      (let ((in-port (open-input-pipe cmd)))
	(let loop ((line (read-line in-port 'concat)))
	  (or (eof-object? line)
	      (begin
		(display line)
		(loop (read-line in-port 'concat)))))))))

(define (fgrep id num)
  (if (not (member id ignore))
      (let ((str (shell (format #f "fgrep ~A snd-*.[ch] --line-number" id))))
	(if (> (string-length str) 1)
	    (begin
	      (display (format #f "-------------------------------- ~A from ~A --------------------------------~%" id num))
	      (display str)
	      (display (format #f "~%~%")))))))

(define (cadr-str data)
  (let ((sp1 -1)
	(len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) (substring data sp1))
	 (if (char=? (string-ref data i) #\space)
	     (if (= sp1 -1)
		 (set! sp1 i)
		 (return (substring data (1+ sp1) i)))))))))

(define (paren-str data)
  (let ((len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) data)
	 (if (char=? (string-ref data i) #\()
	     (return (substring data 0 i))))))))


(define* (CFNC data #:optional spec spec-data) ; 'const -> const for arg cast, 'etc for ... args, 'free -> must free C val before return
  #f)

(define (CFNC-PA data min-len max-len types)
  #f)

(define* (CFNC-21 data #:optional spec)
  (fgrep (cadr-str data) 21))

(define* (CFNC-23 data #:optional spec spec-data)
  (fgrep (cadr-str data) 23))

(define (CFNC-23-PA data min-len max-len types)
  (fgrep (cadr-str data) 23))

(define* (CFNC-236 data)
  (fgrep (cadr-str data) 236))

(define* (CFNC-250 data #:optional spec)
  (fgrep (cadr-str data) 250))

(define* (CFNC-256 data #:optional spec)
  (fgrep (cadr-str data) 256))

(define* (CFNC-260 data #:optional spec)
  (fgrep (cadr-str data) 260))

(define* (CFNC-270 data #:optional spec)
  (fgrep (cadr-str data) 270))

(define* (CFNC-273 data #:optional spec)
  (fgrep (cadr-str data) 273))

(define* (CFNC-290 data #:optional spec)
  (fgrep (cadr-str data) 290))

(define* (CFNC-22 data)
  (fgrep (cadr-str data) 22))

(define* (CFNC-210 data #:optional spec)
  (fgrep (cadr-str data) 210))

(define (CATOM name)
  #f)

(define (CSTR name)
  #f)

(define (CSTR-236 name)
  (fgrep name 236))

(define (CSTR-250 name)
  (fgrep name 250))

(define (CSTR-273 name)
  (fgrep name 273))

(define (CSTR-290 name)
  (fgrep name 290))

(define (CSTR-29x name)
  (fgrep name 291))

(define (CSTR-210 name)
  (fgrep name 210))

(define (CDBL name)
  #f)

(define* (CLNG name #:optional type spec-name)
  #f)

(define* (CLNG-21 name #:optional type spec-name)
  (fgrep name 21))

(define* (CLNG-23 name #:optional type spec-name)
  (fgrep name 23))

(define* (CLNG-250 name #:optional type spec-name)
  (fgrep name 250))

(define* (CLNG-256 name #:optional type spec-name)
  (fgrep name 256))

(define* (CLNG-290 name #:optional type spec-name)
  (fgrep name 290))

(define* (CINT name #:optional type)
  #f)

(define* (CINT-22 name #:optional type)
  (fgrep name 22))

(define* (CINT-23 name #:optional type)
  (fgrep name 23))

(define* (CINT-236 name #:optional type)
  (fgrep name 236))

(define* (CINT-250 name #:optional type)
  (fgrep name 250))

(define* (CINT-256 name #:optional type)
  (fgrep name 256))

(define* (CINT-260 name #:optional type)
  (fgrep name 260))

(define* (CINT-270 name #:optional type)
  (fgrep name 270))

(define* (CINT-273 name #:optional type)
  (fgrep name 273))

(define* (CINT-290 name #:optional type)
  (fgrep name 290))

(define* (CINT-29x name #:optional type)
  (fgrep name 291))

(define* (CINT-210 name #:optional type)
  (fgrep name 210))

(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  #f)

(define (CCAST-21 name type)
  (fgrep (paren-str name) 21))

(define (CCAST-23 name type)
  (fgrep (paren-str name) 23))

(define (CCAST-236 name type)
  (fgrep (paren-str name) 236))

(define (CCAST-250 name type)
  (fgrep (paren-str name) 250))

(define (CCAST-256 name type)
  (fgrep (paren-str name) 256))

(define (CCAST-290 name type)
  (fgrep (paren-str name) 290))

(define (CCAST-210 name type)
  (fgrep (paren-str name) 210))

(define (CCHK name type)
  #f)

(define (CCHK-21 name type)
  (fgrep (paren-str name) 21))

(define (CCHK-23 name type)
  (fgrep (paren-str name) 23))

(define (CCHK-236 name type)
  (fgrep (paren-str name) 236))

(define (CCHK-250 name type)
  (fgrep (paren-str name) 250))

(define (CCHK-256 name type)
  (fgrep (paren-str name) 256))

(define (CCHK-290 name type)
  (fgrep (paren-str name) 290))

(define (CCHK-210 name type)
  (fgrep (paren-str name) 216))

(define (STRUCT data)
  #f)

(define (STRUCT-make data)
  #f)

;;; ---------------------------------------- read data ---------------------------------------- 
(load "xgdata.scm")



