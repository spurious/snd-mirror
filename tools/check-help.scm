;;; check procedure help strings

(if (not (defined? 'symbol->value))
(define (symbol->value sym)
  (if (defined? 'module-ref)
      (module-ref (current-module) sym) ; symbol-binding is deprecated
      (symbol-binding #f sym))))


(let ((names (snd-urls)))
  (for-each
   (lambda (biname)
     (let ((name (string->symbol(car biname))))
       (if (and (defined? name)
		(procedure? (symbol->value name)))
	   (let* ((help (snd-help name))
		  (arity (procedure-property (symbol->value name) 'arity)))
	     (if (and (string? help)
		      (char=? (string-ref help 0) #\()
		      (not (caddr arity))) ; rest args
		 (let ((args (+ (car arity) (cadr arity)))
		       (counted-args 0)
		       (len (string-length help))
		       (got-args #f)
		       (got-name #f)
		       (cur-start -1)
		       (paren-ctr 0))
		   (do ((i 1 (1+ i)))
		       ((or got-args 
			    (>= i len)))
		     (let ((chr (string-ref help i)))
		       (if (char=? chr #\))
			   (begin
			     (set! paren-ctr (1- paren-ctr))
			     (if (and (= paren-ctr 0)
				      (> cur-start 0))
				 (begin
				   (set! counted-args (1+ counted-args))
				   (set! cur-start -1)))
			     (if (< paren-ctr 0)
				 (begin
				   (if (and (not got-name)
					    (> cur-start 0))
				       (begin
					 (set! got-name (substring help cur-start i))
					 (set! cur-start -1)))
				   (if (and got-name 
					    (> cur-start 0))
				       (set! counted-args (1+ counted-args)))
				   (let ((str-name (symbol->string name)))
				     (if (and (not (= counted-args args))
					      (not (member str-name (list "save-region")))
					      (or (< (string-length str-name) 6)
						  (not (and (string=? (substring str-name 0 5) "make-")
							    (= args (* 2 counted-args ))))))
					 (display (format #f ";~A: ~A doc but ~A help" name args counted-args)))
				     (if (and (not (member str-name (list "undo-edit" "redo-edit" "verbose-cursor" "cursor-follows-play" "chans" "read-region-sample")))
					      (or (not (string? got-name))
						  (not (string=? str-name got-name))))
					 (display (format #f ";~A: help name: ~A" name got-name))))
				   (set! got-args #t))))
			   (if (char=? chr #\()
			       (set! paren-ctr (1+ paren-ctr))
			       (if (or (char-alphabetic? chr)
				       (char-numeric? chr)
				       (char=? chr #\-)
				       (char=? chr #\!)
				       (char=? chr #\?)
				       (char=? chr #\:)
				       (char=? chr #\+)
				       (char=? chr #\*)
				       (char=? chr #\>))
				   (if (= cur-start -1)
				       (set! cur-start i))
				   (begin
				     (if (> cur-start 0)
					 (if (not got-name)
					     (set! got-name (substring help cur-start i))
					     (if (= paren-ctr 0)
						 (let ((arg (substring help cur-start i)))
						   (if (and (not (string=? arg ":optional"))
							    (not (string=? arg ":rest"))
							    (not (string=? arg ":optional-key"))
							    (not (string=? arg ":key")))
						       (set! counted-args (1+ counted-args)))))))
				     (if (= paren-ctr 0)
					 (set! cur-start -1))))))))))))))
   names))
   
(exit)
