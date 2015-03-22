;;; a repl
;;;
;;; (load "repl.scm") ((*repl* 'run))

(provide 'repl.scm)
(require libc.scm)

(unless (defined? '*repl*)
  (define *repl*               ; environment that holds the REPL functions
    (let ((prompt #f)          ; function to get/set prompt
	  (keymap #f)          ; function to get/set keymap entries
	  (history #f)         ; function to get/set history buffer entries
	  (save-history #f)    ; function to save the current history buffer entries in a file
	  (restore-history #f) ; function to restore history buffer entries from previous *save-history*
	  (run #f)             ; function that fires up a REPL
	  (repl-let            ; environment for keymap functions to access all the REPL innards (cursor-position etc)
      
      (with-let (sublet *libc*)
	
	(define cursor-right (format #f "~C[C" #\escape))
	(define cursor-left (format #f "~C[D" #\escape))
	(define erase-line (format #f "~C[2K" #\escape))
	(define erase-to-eol (format #f "~C[0K" #\escape))
	
	(define (bold text) (format #f "~C[1m~A~C[21m" #\escape text #\escape)) 
	(define (underline text) (format #f "~C[4m~A~C[24m" #\escape text #\escape))
	(define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape))                                ; black=30, green=32, yellow=33, blue=34
	;(define (foreground text color) (format *stderr* "~C[38;5;~Dm~A~C[0m" #\escape color text #\escape)) ; color 0..255 if TERM=xterm-256color
	
	(define (cursor-forward n) (format #f "~C[~DC" #\escape n))
	(define (cursor-backward n) (format #f "~C[~DD" #\escape n))
	(define (word-wrap on) (format *stderr* "~C[7~A" #\escape (if on "h" "l")) on)

	;; not sure this is useful -- maybe in multiline case
	(define (cursor-coords)
	  (let* ((c (string #\null #\null))
		 (cc (string->c-pointer c))
		 (fn (fileno stdin)))
	    (format *stderr* "~C[6n" #\escape)
	    (do ((b (read fn cc 1) (read fn cc 1)))
		((char=? (c 0) #\escape)))
	    (read fn cc 1) 
	    (and (char=? (c 0) #\[)
		 (let ((y 0)
		       (x 0))
		   (do ((b (read fn cc 1) (read fn cc 1)))
		       ((not (char-numeric? (c 0))))
		     (set! y (+ (* 10 y) (- (char->integer (c 0)) (char->integer #\0)))))
		   (and (char=? (c 0) #\;)
			(do ((b (read fn cc 1) (read fn cc 1)))
			    ((not (char-numeric? (c 0)))
			     (and (char=? (c 0) #\R)
				  (cons x y)))
			  (set! x (+ (* 10 x) (- (char->integer (c 0)) (char->integer #\0))))))))))

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
	
	(let ((history-buffer (make-vector 100 #f))
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
		(let ((pl (*s7* 'print-length)))
		  (set! (*s7* 'print-length) 1024)
		  (format port "(set! history-buffer ~A)~%" (object->string history-buffer))
		  (set! (*s7* 'print-length) pl)))))
	  
	  (define* (restore-history (file "repl-history.scm"))
	    (load file (curlet)))
	  
	  (let* ((prompt-string "> ")
		 (prompt-length 2)
		 (current-line "")
		 (previous-line #f)         ; for undo via C-_
		 (selection #f)             ; for C-y
		 (cursor-position 0)        ; cursor-position is the logical position (index into current-line)
		                            ;   cursor's true position is cursor-position + prompt-length
		 (red-par-pos #f)           ; paren match position
		 (c (string #\null #\null))
		 (cc (string->c-pointer c))
		 (fn (fileno stdin))
		 (saved #f))

	    (define-macro (with-repl-let expr)
	      ;; this is tricky.  We want the user's actions in the repl to appear to take place
	      ;;   in the rootlet, but we also want local repl functions and variables to be accessible
	      ;;   without the verbose ((*repl* 'repl-let) 'cursor-position) business.  So, the eval-string
	      ;;   of user-type-in uses (rootlet) and binds for that call *unbound-variable-hook* to 
	      ;;   check repl-let.  Maybe this is a bad idea -- since repl-let is chained to libc,
	      ;;   we might get confusing shadowing.  Here (not in the repl) for example, read is
	      ;;   libc's read, not s7's.
	      `(let ((old-hook (hook-functions *unbound-variable-hook*)))
		 (set! (hook-functions *unbound-variable-hook*)
		       (cons (lambda (h)
			       (set! (h 'result) ((*repl* 'repl-let) (h 'variable))))
			     old-hook))
		 (let ((result ,expr))
		   (set! (hook-functions *unbound-variable-hook*) old-hook)
		   result)))
	    
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
			      (set! (keymap-functions (char->integer c)) f))))
	    
	    (define C-a 1)     ; #\x01 etc
	    (define C-b 2)
	    (define C-d 4)
	    (define C-e 5)
	    (define C-f 6)
	    (define C-h 8)
	    (define C-k 11)
	    (define C-m 13)    ; #\return
	    (define C-n 14)
	    (define C-p 16)
	    (define C-r 18)
	    (define C-t 20)
	    (define C-y 25)
	    (define C-_ 31)
	    (define Tab 9)
	    (define Enter 10)  ; #\linefeed
	    (define Backspace 127)
	    (define Escape 27) ; #\escape
	    
	    (define (display-line)
	      (format *stderr* "~C~A~A~A" #\return erase-to-eol prompt-string current-line)
	      (format *stderr* "~C~A" #\return (cursor-forward (+ prompt-length cursor-position))))

	    (define (display-line-with-red-paren pos)
	      (format *stderr* "~C~A~A" #\return erase-to-eol prompt-string)
	      (if (zero? pos)
		  (format *stderr* "~A~A" (bold (red "(")) (substring current-line 1))
		  (format *stderr* "~A~A~A" (substring current-line 0 pos) (bold (red "(")) (substring current-line (+ pos 1))))
	      (format *stderr* "~C~A" #\return (cursor-forward (+ prompt-length cursor-position))))
	    
	    (let ((main-keyfunc (lambda (c)
				  (if (= cursor-position (length current-line))
				      (set! current-line (string-append current-line (string c)))
				      (if (= cursor-position 0)
					  (set! current-line (string-append (string c) current-line))
					  (set! current-line (string-append 
							      (substring current-line 0 cursor-position) 
							      (string c) 
							      (substring current-line cursor-position)))))
				  (set! cursor-position (+ cursor-position 1))
				  (set! history-index 0)
				  (display-line)))
		  (no-op-keyfunc (lambda (c) #f)))
	      (do ((i 0 (+ i 1)))
		  ((= i 32))
		(set! (keymap-functions i) no-op-keyfunc))
	      (do ((i 32 (+ i 1)))
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
					     (set! selection (substring current-line cursor-position))
					     (set! current-line (substring current-line 0 cursor-position)))))
	    
	    (set! (keymap-functions C-y) (lambda (c)
					   (when selection
					     (set! previous-line (copy current-line))
					     (if (zero? cursor-position)
						 (set! current-line (string-append selection current-line))
						 (if (>= cursor-position (length current-line))
						     (set! current-line (string-append current-line selection))
						     (set! current-line (string-append (substring current-line 0 cursor-position)
										       selection
										       (substring current-line cursor-position)))))
					     (display-line))))

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
						 (set! cursor-position (+ cur 1))
						 (display-line))))))
	    
	    (set! (keymap-functions C-_) (lambda (c)
					   (when previous-line
					     (set! current-line previous-line)
					     (set! previous-line #f)
					     (set! cursor-position (min cursor-position (length current-line)))
					     (display-line))))
	    
	    (set! (keymap-functions Enter) (lambda (c)
					     (set! (history) current-line)
					     (display-line) ; cancel possible red open paren
					     (set! red-par-pos #f)
					     (catch #t
					       (lambda ()
						 (format *stderr* "~%~S~%" (with-repl-let (eval-string current-line (rootlet)))))
					       (lambda (type info)
						 (format *stderr* "~%~A: " (red "error"))
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
					   (when (and (zero? history-index) 
						      (> (length current-line) 0))
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
	    
	    (define (word-break pos) ; assume we're at the start of a word
	      (let ((len (length current-line)))
		(let loop ((i pos))
		  (if (or (>= i len)
			  (not (or (char-alphabetic? (current-line i))
				   (char-numeric? (current-line i)))))
		      i
		      (loop (+ i 1))))))
	      
	    (set! (keymap-functions Escape) (lambda (esc)
					      ;; arrow-keys etc
					      (read fn cc 1)
					      
					      ;; do we collide with UTF-8 if we treat this as a meta bit?
					      ;;  that is, if (c 0) is #\c, did user type M-c?

					      (case (c 0)
						((#\[)
						 (read fn cc 1)
						 (case (c 0)
						   ((#\A) ((keymap-functions C-p) C-p))
						   ((#\B) ((keymap-functions C-n) C-n))
						   ((#\C) ((keymap-functions C-f) C-f))
						   ((#\D) ((keymap-functions C-b) C-b))
						   
						   ;;((#\1) 
						   ;; (read fn cc 1) ; random -- good up to F5 anyway?
						   ;; (format *stderr* "F~D?" (- (char->integer (c 0)) (char->integer #\0)))
						   ;; (read fn cc 1)) ; tilde (126)
						   ;; (else (format *stderr* "got ~C" (c 0)))
						   ))
						
						((#\<)
						 (format *stderr* "~C~A" #\return (cursor-forward prompt-length))
						 (set! cursor-position 0))

						((#\>)
						 (let ((len (length current-line)))
						   (when (< cursor-position len)
						     (format *stderr* (cursor-forward (- len cursor-position)))
						     (set! cursor-position len))))

						((#\c) ; M-c
						 (let loop ((i cursor-position))
						   (if (< i (length current-line))
						       (if (char-alphabetic? (current-line i))
							   (begin
							     (set! previous-line (copy current-line))
							     (set! (current-line i) (char-upcase (current-line i)))
							     (set! cursor-position (word-break i))
							     (display-line))
							   (loop (+ i 1))))))
						)))
	    
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
						   (set! cursor-position (length current-line))
						   (display-line)))))))

	    (define (char-constant? pos)
	      (and (> pos 2)
		   (char=? (current-line (- pos 1)) #\\)
		   (char=? (current-line (- pos 2)) #\#)))

	    (define (check-parens)
	      (if (and (> cursor-position 1)
		       (char=? (current-line (- cursor-position 1)) #\)) ; ")" on left of cursor
		       (not (char-constant? (- cursor-position 1))))     ; it's not "#\)"
		  (let ((oparens 1)
			(dquotes 0)
			(new-red-pos #f))
		    (do ((i (- cursor-position 2) (- i 1)))
			((or new-red-pos (< i 0)))
		      (when (not (char-constant? i))
			(case (current-line i)
			  ((#\)) 
			   (set! oparens (+ oparens 1)))
			  ((#\()
			   (set! oparens (- oparens 1))
			   (if (and (zero? dquotes)
				    (zero? oparens))
			       (set! new-red-pos i)))
			  ((#\") 
			   (when (or (= i 0)
				     (= dquotes 0)
				     (not (char=? (current-line (- i 1)) #\\)))
			     (set! dquotes (modulo (+ dquotes 1) 2))))
			  ((#\;) 
			   (if (zero? dquotes)
			       (set! new-red-pos #t))))))
		    (when (not (equal? new-red-pos red-par-pos))
		      (if (number? new-red-pos)
			  (begin
			    (set! red-par-pos new-red-pos)
			    (display-line-with-red-paren red-par-pos))
			  (begin
			    (set! red-par-pos #f)
			    (display-line)))))
		  (if (number? red-par-pos)
		      (begin
			(set! red-par-pos #f)
			(display-line)))))

	    ;; we're in libc here, so exit is libc's exit!
	    (define (tty-reset no)
	      (tcsetattr fn TCSAFLUSH saved)
	      (#_exit))
		
	    (set! ((rootlet) 'exit) ; we'd like this to happen when user types "(exit)"
		  (lambda ()
		    (newline *stderr*)
		    (tty-reset 0)))

	    (define (run)
	      ;; check for dumb terminal
	      (let ((terminal (getenv "TERM")))
		(if (string=? terminal "dumb")  ; just read direct -- emacs shell for example
		    (let ((buf (c-pointer->string (calloc 512 1) 512)))
		      (format *stderr* "> ")
		      (do ((b (fgets buf 512 stdin) (fgets buf 512 stdin)))
			  ((zero? (length b))
			   (#_exit))
			(let ((len (strlen buf)))
			  (when (positive? len)
			    (do ((i 0 (+ i 1)))
				((or (not (char-whitespace? (buf i)))
				     (= i len))
				 (when (< i len)
				   (catch #t
				     (lambda ()
				       (format *stderr* "~S~%" (with-repl-let (eval-string (substring buf 0 (- (strlen buf) 1)) (rootlet)))))
				     (lambda (type info)
				       (format *stderr* "error: ")
				       (apply format *stderr* info)
				       (newline *stderr*)))
				   (format *stderr* "> "))))))))))

	      ;; not a dumb terminal -- hopefully all others accept vt100 codes
	      (set! saved (termios.make))
	      (if (or (equal? (signal SIGINT tty-reset) SIG_ERR)
		      (equal? (signal SIGQUIT tty-reset) SIG_ERR)
		      (equal? (signal SIGTERM tty-reset) SIG_ERR)
		      (negative? (tcgetattr fn saved)))
		  (#_exit))
	      (let ((buf (termios.make)))
		(tcgetattr fn buf)
		(termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
		(termios.set_c_cc buf VMIN 1)
		(termios.set_c_cc buf VTIME 0)
		(when (negative? (tcsetattr fn TCSAFLUSH buf))
		  (tty-reset fn))
		
		(format *stderr* prompt-string)
		(format *stderr* "~C~A" #\return (cursor-forward prompt-length))
		
		(do ((i (read fn cc 1) (read fn cc 1)))
		    ((not (= i 1))
		     (tty-reset fn))
		  ((keymap-functions (char->integer (c 0))) (c 0))
		  (check-parens)
		  )))

	    (curlet))))))

      (set! keymap (repl-let 'keymap))
      (set! history (repl-let 'history))
      (set! save-history (repl-let 'save-history))
      (set! restore-history (repl-let 'restore-history))
      (set! prompt (repl-let 'prompt))
      (set! run (repl-let 'run))

      (curlet))))
  
;((*repl* 'run))

#|
to add/change keymap entry:

(set! (*keymap* loc)
      (with-let (*repl* 'repl-let)
	(lambda (c)
	  ;; access here to all internal repl vars and funcs
	  )))
|#

;; need multiline edits
;; if cursor back not a complete expr, put cr[prompt-spaces] in string and edit 
;;   need a way to move between lines, and keep cursor tracked by last newline
;; M-u M-l maybe M-d and M-b or whatever
;; need some sort of auto-test