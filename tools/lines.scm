#!/usr/bin/guile -s
!#

(use-modules (ice-9 format))
(if (not (defined? 'read-line)) (use-modules (ice-9 rdelim)))


(let ((cur-name #f)
      (cur-count 1)
      (total 0))
  (call-with-input-file
      "gpound"
    (lambda (file)
      (let loop ((line (read-line file 'concat)))
	(let ((name #f)
	      (pos 0)
	      (len 0))
	  (if (eof-object? line)
	      (begin
		(display (format #f "~A: ~A~%" cur-name cur-count))
		(display (format #f "total lines: ~A~%" (+ total cur-count))))
	      (begin
		(call-with-current-continuation
		 (lambda (break)
		   (set! len (string-length line))
		   (do ((i 0 (1+ i)))
		       ((= i len))
		     (if (char=? (string-ref line i) #\:)
			 (begin
			   (set! pos i)
			   (break))))))
		(if (> pos 0)
		    (begin
		      (set! name (substring line 0 pos))
		      (if (not (string? cur-name))
			  (set! cur-name name)
			  (if (string=? name cur-name)
			      (set! cur-count (1+ cur-count))
			      (begin
				(display (format #f "~A: ~A~%" cur-name cur-count))
				(set! total (+ total cur-count))
				(set! cur-count 1)
				(set! cur-name name))))))
		(loop (read-line file 'concat)))))))))


(define num-funcs 1400)
(define funcs (make-vector num-funcs))
(define func-ctr 0)

(let ((state 0)
      (current-func ""))
  (call-with-input-file "gcall"
    (lambda (file)
      (let loop ((line (read-line file 'concat)))
	(or (eof-object? line)
	    (let ((len (string-length line))
		  (ok #f))

	      (if (= state 1)
		  (do ((i 0 (1+ i)))
		      ((or ok (= i len))
		       (let ()
			 (set! state 0)
			 state))
		    (let ((ch (string-ref line i)))
		      (if (char=? ch #\-) ; ignore -- not relevent to this version
			  (begin
			    (set! state 0)
			    (set! ok #t))
			  (if (char=? ch #\#)
			      (begin
				(vector-set! funcs func-ctr (list current-func 0))
				(set! func-ctr (1+ func-ctr))
				(set! state 0)
				(set! ok #t))
			      (if (char-numeric? ch)
				  (let ((k (+ i 1)))
				    (do ()
					((or (= k len) 
					     (not (char-numeric? (string-ref line k)))))
				      (set! k (1+ k)))
				    (vector-set! funcs func-ctr (list current-func (string->number (substring line i k))))
				    (set! func-ctr (1+ func-ctr))
				    (set! state 0)
				    (set! ok #t)))))))

		  ;; state = 0
		  (do ((i 0 (1+ i)))
		      ((or ok (>= i (- len 14))))
		    (let ((ch (string-ref line i)))
		      (if (and (char=? ch #\s)
			       (string=? (substring line i (+ i 13)) "static XEN g_"))
			  (begin
			    (do ((k (+ i 14) (1+ k)))
				((or ok (= k len)))
			      (let ((ch (string-ref line k)))
				(if (or (char=? ch #\()
					(char=? ch #\space))
				    (begin
				      (set! ok #t)
				      (set! current-func (substring line (+ i 11) k))))))
			    (set! state 1))))))
	      (loop (read-line file 'concat))))))))

(define nfuncs (make-vector func-ctr))
(do ((i 0 (1+ i)))
    ((= i func-ctr))
  (vector-set! nfuncs i (vector-ref funcs i)))
(set! nfuncs (sort nfuncs (lambda (a b) (< (cadr a) (cadr b)))))

(call-with-output-file "g-calls"
  (lambda (fd)
    (format fd "~D functions found~%" func-ctr)
    (do ((i 0 (1+ i)))
	((= i func-ctr))
      (let ((name (car (vector-ref nfuncs i)))
	    (calls (cadr (vector-ref nfuncs i))))
	(format fd "  ~A: ~A~%" name calls)))))

