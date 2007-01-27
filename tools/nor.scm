#!/usr/bin/guile -s
!#

(use-modules (ice-9 format))
(if (not (defined? 'read-line)) (use-modules (ice-9 rdelim)))

(with-output-to-file
    "test.scm"
  (lambda ()
    (call-with-input-file 
	"ladspa.scm"
      (lambda (file)
	(let loop ((line (read-line file 'concat)))
	  (or (eof-object? line)
	      (let ((len (string-length line)))
		(do ((i 0 (1+ i)))
		    ((= i len))
		  (let ((ch (string-ref line i)))
		    (if (not (char=? ch (integer->char #o015)))
			(display (format #f "~C" ch)))))
		(loop (read-line file 'concat)))))))))


