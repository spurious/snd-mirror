(init-xm)

(define match-sound-files
  (lambda args
    "(match-sound-files func &optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
    (let* ((func (car args))
	   (matches '()))
      (for-each
       (lambda (file)
	 (if (func file)
	     (set! matches (cons file matches))))
       (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
      matches)))

(define (install-searcher proc)
  (define (XmString->string str)
    (cadr (|XmStringGetLtoR str |XmFONTLIST_DEFAULT_TAG)))
  (define (XmStringTable->list st len)
    (|XmStringTableUnparse st len #f |XmCHARSET_TEXT |XmCHARSET_TEXT #f 0 |XmOUTPUT_ALL))
  (define (list->XmStringTable strs)
    (|XmStringTableParseStringArray strs (length strs) #f |XmCHARSET_TEXT #f 0 #f))
  (|XtSetValues (let ((m (open-file-dialog #f)))
		  (|Widget (list-ref (dialog-widgets) 6)))
		(list |XmNfileSearchProc
		       (lambda (widget info)
			 (let* ((dir (XmString->string (|dir info)))
				(files (match-sound-files proc dir))
				(fileTable (list->XmStringTable (map (lambda (n) (string-append dir n)) files))))
			   (|XtSetValues widget
					 (list |XmNfileListItems fileTable
						|XmNfileListItemCount (length files)
						 |XmNlistUpdated #t))
			   (|XmStringTableFree fileTable (length files)))))))

;(install-searcher (lambda (file) (= (mus-sound-chans file) 4)))
;(install-searcher (lambda (file) (= (mus-sound-chans file) 4)))



(define* (install-y-position func #:optional snd chn)
  (let ((sy (|Widget (list-ref (channel-widgets snd chn) 4))))
    (|XtRemoveAllCallbacks sy |XmNdragCallback)
    (|XtRemoveAllCallbacks sy |XmNvalueChangedCallback)
    (|XtSetValues sy (list |XmNvalueChangedCallback
			    (list (lambda (widget context info)
				    (func (|value info) snd chn))
				  #f)))))


