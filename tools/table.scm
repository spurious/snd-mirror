#!/usr/bin/guile -s
!#

(use-modules (ice-9 format))
(if (not (defined? 'read-line)) (use-modules (ice-9 rdelim)))

(define (no-dashes-or-cr str)
  (let* ((len (string-length str))
	 (newstr (make-string 0))
	 (colons 0)
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
	  (let loop ((line (read-line file 'concat)))
	    (set! ctr (1+ ctr))
	    (or (eof-object? line)
		(let ((len (string-length line)))
		(if (and (> len 30)
			 (string=? ";;; ---------------- test "
				   (substring line 0 26)))
		    (display (format #f "~A ~48,1T[~D]~%" (no-dashes-or-cr line) ctr)))
		(loop (read-line file 'concat))))))))



