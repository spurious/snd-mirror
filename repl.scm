;;; a repl

(provide 'repl.scm)
(require libc.scm)

(when (not (defined? '*repl*))
  (define *repl*         ; environment that holds the REPL functions
    (let ((*prompt* #f)  ; function to get/set prompt
	  (*keymap* #f)  ; function to get/set keymap entries
	  (*history* #f) ; function to get/set history buffer entries
	  (*run* #f)     ; function that fires up a REPL
	  (*keymap-let*  ; environment for keymap functions to access all the REPL innards (cursor-position etc)
      
      (with-let (sublet *libc*)
	
	(define cursor-home (format #f "~C[H" #\escape))
	(define cursor-right (format #f "~C[C" #\escape))
	(define cursor-left (format #f "~C[D" #\escape))
	(define erase-line (format #f "~C[2K" #\escape))
	(define erase-to-eol (format #f "~C[0K" #\escape))
	
	(define (bold text) (format #f "~C[1m~A~C[21m" #\escape text #\escape)) ; or is it 22??
	(define (underline text) (format #f "~C[4m~A~C[24m" #\escape text #\escape))
	(define (blink text) (format #f "~C[5m~A~C[25m" #\escape text #\escape))
	(define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape)) ; black=30, green=32, yellow=33, blue=34
	(define (foreground text color) (format *stderr* "~C[38;5;~Dm~A~C[0m" #\escape color text #\escape)) ; color 0..255
	
	(define (cursor-forward n) (format #f "~C[~DC" #\escape n))
	(define (cursor-backward n) (format #f "~C[~DD" #\escape n))

	(define keymap-let #f)
	
	(define (symbol-completion text)
	  (let ((st (symbol-table))
		(len (length text))
		(short-match #f)
		(short-len 0))
	    (call-with-exit
	     (lambda (return)
	       (for-each
		(lambda (symbol)
		  (let* ((name (symbol->string symbol))
			 (name-len (length name)))
		    (when (>= name-len len)
		      (when (string=? text (substring name 0 len))
			;; symbol matches text, need to return shortest such match
			(if (not short-match)
			    (begin
			      (set! short-match name)
			      (set! short-len name-len))
			    (if (< (length name) short-len)
				(if (= name-len len)
				    (return text)
				    (begin
				      (set! short-match name)
				      (set! short-len name-len)))))))))
		st)
	       (or short-match text)))))
	
	(define (filename-completion text)
	  (let ((g (glob.make)))
	    (glob (string-append text "*")
		  (logior (if (and (defined? 'GLOB_TILDE)
				   (char=? (text 0) #\~))
			      GLOB_TILDE
			      0)
			  GLOB_MARK)
		  g)
	    (let ((files (glob.gl_pathv g)))
	      (globfree g) 
	      (if (null? files)
		  text
		  (if (null? (cdr files))
		      (car files)
		      (if (< (length (car files)) (length (cadr files)))
			  (car files)
			  (cadr files)))))))
	
	(let ((history-buffer (make-vector 10 #f))
	      (current-history-size 100)
	      (history-position 0)
	      (history-index 0))
	  
	  (define history-size (dilambda
				(lambda ()
				  current-history-size)
				(lambda (new-size)
				  (set! history-buffer (copy history-buffer (make-vector new-size #f)))
				  (set! current-history-size new-size))))
	  
	  (define history (dilambda 
			   (lambda (back)
			     (let ((i (+ history-position back)))
			       (if (< i 0)
				   (history-buffer (+ current-history-size i))
				   (if (>= i current-history-size)
				       (history-buffer (- i current-history-size))
				       (history-buffer i)))))
			   (lambda (new-line)
			     (set! (history-buffer history-position) new-line)
			     (set! history-position (+ history-position 1))
			     (if (= history-position current-history-size)
				 (set! history-position 0)))))
	  
	  (define* (save-history (file "repl-history.scm"))
	    (call-with-output-file file
	      (lambda (port)
		(format port "(set! history-position ~D)~%" history-position)
		(format port "(set! current-history-size ~D)~%" current-history-size)
		(format port "(set! history-buffer ~A)~%" (object->string history-buffer :readable)))))
	  
	  (define* (restore-history (file "repl-history.scm"))
	    (load file (curlet)))
	  
	  (let* ((prompt-string "> ")
		 (prompt-length 2)
		 (current-line "")
		 (previous-line #f)
		 (cursor-position 0)
		 (c (string #\null #\null))
		 (cc (string->c-pointer c))
		 (fn (fileno stdin))
		 (saved #f))
	    
	    (define prompt (dilambda
			    (lambda ()
			      prompt-string)
			    (lambda (new-prompt)
			      (set! prompt-length (length new-prompt))
			      (set! prompt-string new-prompt))))
	    
	    (define keymap-functions (make-vector 256 #f))
	    (define keymap (dilambda
			    (lambda (c)
			      (keymap-functions (char->integer c)))
			    (lambda (c f)
			      (set! (*keymap (char->integer c)) f))))
	    
	    (define C-a 1) ; #\x01 etc
	    (define C-b 2)
	    (define C-d 4)
	    (define C-e 5)
	    (define C-f 6)
	    (define C-h 8)
	    (define C-k 11)
	    (define C-m 13) ; #\return
	    (define C-n 14)
	    (define C-p 16)
	    (define C-r 18)
	    (define C-t 20)
	    (define C-_ 31)
	    (define Tab 9)
	    (define Enter 10) ; #\linefeed
	    (define Backspace 127)
	    (define Escape (char->integer #\escape))
	    
	    (let ((main-keyfunc (lambda (c)
				  ;; (format *stderr* "~D" (char->integer c))
				  (set! current-line (string-append current-line (string c)))
				  (set! cursor-position (+ cursor-position 1))
				  (set! history-index 0)
				  (format *stderr* "~C" c))))
	      (do ((i 0 (+ i 1)))
		  ((= i 256))
		(set! (keymap-functions i) main-keyfunc)))
	    
	    (set! (keymap-functions C-a) (lambda (c)
					   (format *stderr* "~C~A" #\return (cursor-forward prompt-length))
					   (set! cursor-position 0)))
	    
	    (set! (keymap-functions C-b) (lambda (c)
					   (when (> cursor-position 0)
					     (format *stderr* cursor-left)
					     (set! cursor-position (- cursor-position 1)))))
	    
	    (set! (keymap-functions C-d) (lambda (c)
					   (let ((len (length current-line)))
					     (when (< cursor-position len)
					       (set! previous-line (copy current-line))
					       (do ((i cursor-position (+ i 1)))
						   ((>= i (- len 1)))
						 (set! (current-line i) (current-line (+ i 1))))
					       (set! current-line (substring current-line 0 (- len 1)))
					       (format *stderr* "~A~A~C" erase-to-eol (substring current-line cursor-position) #\return)
					       (format *stderr* (cursor-forward (+ prompt-length cursor-position)))))))
	    
	    (set! (keymap-functions C-e) (lambda (c)
					   (let ((len (length current-line)))
					     (when (< cursor-position len)
					       (format *stderr* (cursor-forward (- len cursor-position)))
					       (set! cursor-position len)))))
	    
	    (set! (keymap-functions C-f) (lambda (c)
					   (when (< cursor-position (length current-line))
					     (format *stderr* cursor-right)
					     (set! cursor-position (+ cursor-position 1)))))
	    
	    (set! (keymap-functions C-k) (lambda (c)
					   (when (< cursor-position (length current-line))
					     (set! previous-line (copy current-line))
					     (format *stderr* erase-to-eol)
					     (set! current-line (substring current-line 0 cursor-position)))))
	    
	    (set! (keymap-functions C-t) (lambda (c)
					   (when (and (> (length current-line) 1)
						      (> cursor-position 0))
					     (set! previous-line (copy current-line))
					     (let ((cur (if (= cursor-position (length current-line))
							    (- cursor-position 1)
							    cursor-position)))
					       (let ((tmp-c (current-line (- cur 1))))
						 (set! (current-line (- cur 1)) (current-line cur))
						 (set! (current-line cur) tmp-c)
						 (format *stderr* "~C~A~A~A" #\return erase-to-eol prompt-string current-line)
						 (set! cursor-position (+ cur 1))
						 (format *stderr* "~C~A" #\return (cursor-forward (+ prompt-length cursor-position))))))))
	    
	    (set! (keymap-functions C-_) (lambda (c)
					   (when previous-line
					     (set! current-line previous-line)
					     (set! previous-line #f)
					     (format *stderr* "~C~A~A~A" #\return erase-to-eol prompt-string current-line)
					     (set! cursor-position (min cursor-position (length current-line)))
					     (format *stderr* "~C~A" #\return (cursor-forward (+ prompt-length cursor-position))))))
	    
	    (set! (keymap-functions Enter) (lambda (c)
					     (set! (history) current-line)
					     (catch #t
					       (lambda ()
						 (format *stderr* "~%~S~%" (eval-string current-line keymap-let)))
					       (lambda (type info)
						 (apply format *stderr* info)
						 (newline *stderr*)))
					     (set! current-line "")
					     (set! cursor-position 0)
					     (format *stderr* prompt-string)
					     (format *stderr* "~C~A" #\return (cursor-forward prompt-length))))
	    
	    (set! (keymap-functions C-h) (lambda (c)
					   (when (> cursor-position 0)
					     (set! previous-line (copy current-line))
					     (let ((len (length current-line)))
					       (set! cursor-position (- cursor-position 1))
					       (format *stderr* cursor-left)
					       (do ((i cursor-position (+ i 1)))
						   ((>= i (- len 1)))
						 (set! (current-line i) (current-line (+ i 1))))
					       (set! current-line (substring current-line 0 (- len 1)))
					       (format *stderr* "~A~A~C" erase-to-eol (substring current-line cursor-position) #\return)
					       (format *stderr* (cursor-forward (+ prompt-length cursor-position)))))))
	    
	    (set! (keymap-functions Backspace) (keymap-functions C-h))
	    
	    (set! (keymap-functions C-p) (lambda (c)
					   (when (and (zero? history-index) (> (length current-line) 0))
					     (set! (history) current-line)
					     (set! history-index -1))
					   (set! history-index (max (- history-index 1) (- current-history-size)))
					   (when (history history-index)
					     (set! current-line (history history-index))
					     (format *stderr* "~C~A~A~A" #\return erase-line prompt-string current-line)
					     (set! cursor-position (length current-line)))))
	    
	    (set! (keymap-functions C-n) (lambda (c)
					   (set! history-index (min 0 (+ history-index 1)))
					   (when (history history-index)
					     (set! current-line (history history-index))
					     (format *stderr* "~C~A~A~A" #\return erase-line prompt-string current-line)
					     (set! cursor-position (length current-line)))))
	    
	    (set! (keymap-functions Escape) (lambda (esc)
					      ;; arrow-keys etc
					      (read fn cc 1)
					      (when (char=? (c 0) #\[)
						(read fn cc 1)
						(case (c 0)
						  ((#\A) (format *stderr* "up arrow")) ; esc n A?
						  ((#\B) (format *stderr* "down arrow")) ; esc n B?
						  
						  ((#\C) ((keymap-functions C-f) C-f))
						  ((#\D) ((keymap-functions C-b) C-b))
						  
						  ((#\1) 
						   (read fn cc 1) ; random -- good up to F5 anyway?
						   (format *stderr* "F~D?" (- (char->integer (c 0)) (char->integer #\0)))
						   (read fn cc 1)) ; tilde (126)
						  (else (format *stderr* "got ~C" (c 0)))
						  ))))
	    
	    (set! (keymap-functions Tab) (lambda (c)
					   (let ((len (length current-line))
						 (completion #f))
					     (when (and (positive? len)
							(= cursor-position len))
					       (let ((loc (do ((i (- len 1) (- i 1)))
							      ((or (< i 0)
								   (char-whitespace? (current-line i))
								   (memv (current-line i) '(#\( #\' #\" #\))))
							       i))))
						 (if (< loc 0)
						     (set! completion (symbol-completion current-line))
						     (if (char=? (current-line loc) #\")
							 (set! completion (filename-completion (substring current-line (+ loc 1))))
							 (set! completion (symbol-completion (substring current-line (+ loc 1))))))
						 (when (and completion
							    (> (length completion) (- len loc 1)))
						   (set! current-line (string-append (substring current-line 0 (+ loc 1)) completion))
						   (format *stderr* "~C~A~A~A" #\return erase-to-eol prompt-string current-line)
						   (set! cursor-position (length current-line))
						   (format *stderr* "~C~A" #\return (cursor-forward (+ prompt-length cursor-position)))
						   ))))))
	    (define (tty-reset no)
	      (tcsetattr fn TCSAFLUSH saved)
	      (#_exit))
		
	    (define (exit)
	      (newline *stderr*)
	      (tty-reset 0))
		
	    (set! keymap-let (curlet))

	    (define (run)
	      (set! saved (termios.make))
		
	      (if (or (equal? (signal SIGINT tty-reset) SIG_ERR)
		      (equal? (signal SIGQUIT tty-reset) SIG_ERR)
		      (equal? (signal SIGTERM tty-reset) SIG_ERR)
		      (negative? (tcgetattr fn saved)))
		  (exit))
	      
	      (let ((buf (termios.make)))
		(tcgetattr fn buf)
		(termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
		(termios.set_c_cc buf VMIN 1)
		(termios.set_c_cc buf VTIME 0)
		(when (negative? (tcsetattr fn TCSAFLUSH buf))
		  (tty-reset fn))
		
		(format *stderr* prompt-string)
		(format *stderr* "~C~A" #\return (cursor-forward prompt-length))
		;; cursor-position is the logical position (index into current-line)
		;; cursor's true position is cursor-position + prompt-length
		
		(do ((i (read fn cc 1) (read fn cc 1)))
		    ((not (= i 1))
		     (tty-reset fn))
		  ((keymap-functions (char->integer (c 0))) (c 0)))))))

      keymap-let)))

      (set! *keymap* (*keymap-let* 'keymap))
      (set! *history* (*keymap-let* 'history))
      (set! *prompt* (*keymap-let* 'prompt))
      (set! *run* (*keymap-let* 'run))

      (curlet))))
  
((*repl* '*run*))

;; need prompt handling (multi), paren matches, multiline edits
;; function/arrow keys I guess etc
;; doc repl.scm
;; obj->str readable print-length if func, readable should look for cycles, and print more cleanly if none

#|
to add/change keymap entry:

(set! (*keymap* loc)
      (with-let (*repl* '*keymap-let*)
	(lambda (c)
	  ;; access here to all internal repl vars and funcs
	  )))
|#
