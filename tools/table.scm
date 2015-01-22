(define (no-dashes-or-cr str)
  (let ((len (length str))
	(newstr (make-string 0))
	(last-ch #\-))
    (do ((i 0 (1+ i)))
	((= i (1- len)))
      (let ((ch (string-ref str i)))
	(if (and (or (not (char=? ch #\-))
		     (char-alphabetic? last-ch))
		 (not (char=? ch #\newline)))
	    (set! newstr (string-append newstr (make-string 1 ch))))
	(set! last-ch ch)))
    newstr))

(let ((ctr 0))
  (call-with-input-file 
      "snd-test.scm"
    (lambda (file)
      (let loop ((line (read-line file #t)))
	(set! ctr (1+ ctr))
	(or (eof-object? line)
	    (let ((len (length line)))
	      (if (and (> len 30)
		       (string=? ";;; ---------------- test "
				 (substring line 0 26)))
		  (format #t "~A ~48,1T[~D]~%" (no-dashes-or-cr line) ctr))
	      (loop (read-line file #t))))))))

(exit)
