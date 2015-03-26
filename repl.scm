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
	  (restore-history #f) ; function to restore history buffer entries from a file
	  (helpers ())         ; list of functions displaying help strings 
	  (run #f)             ; function that fires up a REPL
	  (repl-let            ; environment for keymap functions to access all the REPL innards (cursor-position etc)
      
      (with-let (sublet *libc*)

	;; -------- completion --------
	(define (symbol-completion text)
	  (let ((st (symbol-table))
		(text-len (length text))
		(match #f))
	    (call-with-exit
	     (lambda (return)
	       (for-each
		(lambda (symbol)
		  (let* ((sym (symbol->string symbol))
			 (sym-len (length sym)))
		    (when (and (>= sym-len text-len)
			       (string=? text (substring sym 0 text-len)))
		      (if match
			  (return text)
			  (set! match sym)))))
		st)
	       (or match text)))))
	
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
	      (if (or (null? files)
		      (not (null? (cdr files))))
		  text
		  (car files)))))


	;; -------- history --------
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

	  (define (pop-history) ; throw away most recent addition
	    (set! history-position (- history-position 1))
	    (if (negative? history-position)
		(set! history-position (- current-history-size 1))))

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

	  
	  ;; -------- evaluation ---------
	  (define (badexpr h)               ; *missing-close-paren-hook* function for Enter command
	    (set! (h 'result) 'incomplete-expr))
	  
	  (define-macro (with-repl-let expr)
	    ;; for multiline edits, we will use *missing-close-paren-hook* rather than try to parse the input ourselves.
	    `(let ((old-badexpr-hook (hook-functions *missing-close-paren-hook*)))
	       (dynamic-wind
		   (lambda ()
		     (set! (hook-functions *missing-close-paren-hook*) (cons badexpr old-badexpr-hook)))
		   (lambda ()
		     ,expr)
		   (lambda ()
		     (set! (hook-functions *missing-close-paren-hook*) old-badexpr-hook)))))

	  
	  (let ((prompt-string "> ")
		(prompt-length 2)
		(current-line "")          ; current expression might be a better name
		(previous-line ())         ; for undo via C-_
		(selection #f)             ; for C-y
		(cursor-position 0)        ; cursor-position is the logical position (index into current-line)
		(current-row 0)            ; catch window scrolling
		(red-par-pos #f)           ; paren match position (index into current-line)
		(prompt-row 0)             ; need to tie everything to prompt position (it can move in media res if window scrolls)
		(prompt-col 0)
		(last-row 0)               ; these hold the window bounds when we start
		(last-col 0)
		(input-fd (fileno stdin))  ; either tty input or file input
		(next-char #f)
		(chars 0))                 ; (sigh) a kludge to try to distinguish tab-as-space from tab-as-completion/indentation

		 
	    ;; -------- match parens --------
	    (define (char-constant? pos)
	      (and (> pos 2)
		   (char=? (current-line (- pos 1)) #\\)
		   (char=? (current-line (- pos 2)) #\#)))
	    
	    (define (check-parens)
	      (let ((endpos (- cursor-position 1)))
	      (if (and (> cursor-position 1)
		       (char=? (current-line endpos) #\))              ; ")" on left of cursor
		       (not (char-constant? endpos)))                  ; it's not "#\)"
		  (let ((oparens ())
			(new-red-pos #f))
		    (do ((i 0 (+ i 1)))
			((>= i (- cursor-position 1)))
		      (case (current-line i)
			((#\()
			 (set! oparens (cons i oparens)))
			((#\))
			 (if (pair? oparens)
			     (set! oparens (cdr oparens))))
			((#\;)
			 (do ((k (+ i 1) (+ k 1)))
			     ((or (>= k endpos)
				  (char=? (current-line k) #\newline))
			      (set! i k))))
			((#\")
			 (do ((k (+ i 1) (+ k 1)))
			     ((or (>= k endpos)
				  (and (char=? (current-line k) #\")
				       (not (char=? (current-line (- k 1)) #\\))))
			      (set! i k))))
			((#\#)
			 (if (char=? (current-line (+ i 1)) #\|)
			     (do ((k (+ i 1) (+ k 1)))
				 ((or (>= k endpos)
				      (and (char=? (current-line k) #\|)
					   (char=? (current-line (+ k 1)) #\#)))
				  (set! i (+ k 1))))))))
		    
		    (if (pair? oparens)
			(set! new-red-pos (car oparens)))
		    (unless (equal? new-red-pos red-par-pos)
		      (if (number? new-red-pos)
			  (set! red-par-pos new-red-pos)
			  (set! red-par-pos #f))))
		  (if (number? red-par-pos)
		      (set! red-par-pos #f)))))
	    
	    
	    ;; -------- indentation --------
	    (define (indent pos)
	      (let ((old-red red-par-pos)
		    (old-line (copy current-line))
		    (old-cursor cursor-position))
		(set! current-line (string-append (substring current-line 0 cursor-position) ")"))
		(set! cursor-position (length current-line))
		(check-parens)
		(set! current-line old-line)
		(set! cursor-position old-cursor)
		(let ((new-red red-par-pos))
		  (set! red-par-pos old-red)
		  (let ((col 0))
		    (do ((i 0 (+ i 1)))
			((= i new-red))
		      (if (char=? (current-line i) #\newline)
			  (set! col 0)
			  (set! col (+ col 1))))
		    (let ((sym (do ((i (+ new-red 1) (+ i 1)))
				   ((not (char-alphabetic? (current-line i)))
				    (substring current-line (+ new-red 1) i)))))

		      (let ((spaces (+ col (if (member sym '("or" "and" "cond" "if"))
					       (+ (length sym) 2)
					       2))))
			(if (= cursor-position (length current-line))
			    (begin
			      (set! current-line (format #f "~A~NC" current-line spaces #\space))
			      (set! cursor-position (length current-line)))
			    (begin
			      (set! current-line (format #f "~A~NC~A" 
							 (substring current-line 0 cursor-position)
							 spaces #\space
							 (substring current-line (+ cursor-position 1))))
			      (set! cursor-position (+ cursor-position spaces))))))))))


	    ;; -------- prompt --------
	    (define prompt (dilambda
			    (lambda ()
			      prompt-string)
			    (lambda (new-prompt)
			      (set! prompt-length (length new-prompt))
			      (set! prompt-string new-prompt))))
	    

	    ;; -------- vt100 --------
	    (define (bold text) (format #f "~C[1m~A~C[21m" #\escape text #\escape)) 
	    (define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape))  ; black=30, green=32, yellow=33, blue=34
	    
	    (define* (rgb text (r 0) (g 0) (b 0) all-colors)
	      (if all-colors
		  (format #f "~C[38;5;~Dm~A~C[0m" #\escape (+ 16 (* 36 (round (* r 5))) (* 6 (round (* g 5))) (round (* b 5))) text #\escape)
		  (format #f "~C[~Dm~A~C[0m" #\escape (+ 30 (ash (round b) 2) (ash (round g) 1) (round r)) text #\escape)))

	    (define (cursor-coords)
	      (let* ((c (string #\null #\null))
		     (cc (string->c-pointer c))
		     (terminal-fd (fileno stdin)))
		(format *stderr* "~C[6n" #\escape)
		(do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
		    ((char=? (c 0) #\escape)))
		(read terminal-fd cc 1) 
		(and (char=? (c 0) #\[)
		     (let ((y 0)
			   (x 0))
		       (do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
			   ((not (char-numeric? (c 0))))
			 (set! y (+ (* 10 y) (- (char->integer (c 0)) (char->integer #\0)))))
		       (and (char=? (c 0) #\;)
			    (do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
				((not (char-numeric? (c 0)))
				 (and (char=? (c 0) #\R)
				      (cons x y)))
			      (set! x (+ (* 10 x) (- (char->integer (c 0)) (char->integer #\0))))))))))

	    (define (cursor-bounds)
	      (let ((coords (cursor-coords)))
		(set! prompt-col (car coords))
		(set! prompt-row (cdr coords))
		(format *stderr* "~C[~D;~DH" #\escape 4000 4000)
		(let ((bounds (cursor-coords)))
		  (format *stderr* "~C[~D;~DH" #\escape (cdr coords) (car coords))
		  (set! last-col (car bounds))
		  (set! last-row (cdr bounds)))))
	    

	    ;; -------- display --------
	    (define (display-prompt)
	      (format *stderr* "~A" prompt-string))
	    
	    (define (new-prompt)
	      (set! current-line "")
	      (set! cursor-position 0)
	      (set! current-row 0)
	      (set! previous-line ())
	      (display-prompt)
	      (cursor-bounds))
	    
	    (define (display-line start end)
	      (if (and red-par-pos
		       (<= start red-par-pos)
		       (< red-par-pos end))
		  (string-append
		   (if (zero? start)
		       (format #f "~A" prompt-string)
		       (format #f "~NC" prompt-length #\space))
		   (if (= start red-par-pos)
		       (format #f "~A~A"
			       (bold (red "(")) 
			       (substring current-line (+ start 1) end))
		       (format #f "~A~A~A"
			       (substring current-line start red-par-pos) 
			       (bold (red "(")) 
			       (substring current-line (+ red-par-pos 1) end))))
		  (if (zero? start)
		      (format #f "~A~A" prompt-string (substring current-line 0 end))
		      (format #f "~NC~A" prompt-length #\space (substring current-line start end)))))
	    
	    (define (display-lines)
	      (format *stderr* "~C[~D;0H~C[J" #\escape prompt-row #\escape) ; set cursor back to prompt, erase down from there
	      
	      (let ((len (length current-line))
		    (new-line ""))
		(let ((line-end 0))
		  (do ((i 0 (+ line-end 2)))
		      ((> i len))
		    (set! line-end (end-of-line i))
		    (set! new-line (string-append new-line (display-line i (min (+ line-end 2) len))))))
		(format *stderr* "~A" new-line)
		(let ((row 0)
		      (start 0))
		  (do ((i 0 (+ i 1)))
		      ((or (= i len)
			   (= i cursor-position))
		       (format *stderr* "~C[~D;~DH" #\escape (+ prompt-row row) (+ prompt-col (- cursor-position start))))
		    (when (char=? (current-line i) #\newline)
		      (set! row (+ row 1))
		      (set! start (+ i 1)))))))
	    
	    
	    ;; -------- help/debugging --------
	    (define (one-line text)
	      (if (string? text)
		  (let ((ntext (copy text)))
		    (do ((i 0 (+ i 1)))
			((= i (length ntext))
			 ntext)
		      (if (char=? (ntext i) #\newline) (set! (ntext i) #\|))))
		  text))
	    
	    (define (help c)
	      (when (pair? (*repl* 'helpers))
		(let ((coords (cursor-coords))
		      (col (floor (/ last-col 2)))
		      (row (floor (/ last-row 2))))
		  (format *stderr* "~C[~D;~DH" #\escape row col)
		  (format *stderr* "+~NC" (- col 2) #\-)

		  (do ((i 1 (+ i 1))
		       (lst (*repl* 'helpers) (cdr lst)))
		      ((null? lst))
		    (let ((str ((car lst) c)))
		      (format *stderr* "~C[~D;~DH" #\escape (+ row i) col)
		      (format *stderr* "~C[K| ~A"  #\escape (if (> (length str) col) (substring str 0 (- col 1)) str))))

		  (format *stderr* "~C[~D;~DH" #\escape (+ row 1 (length (*repl* 'helpers))) col)
		  (format *stderr* "+~NC" (- col 2) #\-)
		  (format *stderr* "~C[~D;~DH"   #\escape (cdr coords) (car coords)))))
		  
	    (define (debug-help)
	      (set! (*repl* 'helpers)
		    (list
		     (lambda (c) 
		       (format #f "cursor: ~A, line: ~S"
			       cursor-position 
			       (one-line current-line)))
		     (lambda (c)
		       (format #f "len: ~D, selection: ~S, previous: ~S" 
			       (length current-line) 
			       (one-line selection) 
			       (if (pair? previous-line) (one-line (cdar previous-line)) ())))
		     (lambda (c)
		       (format #f "current-row: ~A, prompt-row: ~A" 
			       current-row 
			       prompt-row))
		     (lambda (c)
		       (format #f "c: ~S ~D, start: ~A, end: ~A" 
			       (object->string c #t) 
			       (char->integer c)	
			       (start-of-line cursor-position)	
			       (end-of-line cursor-position))))))
		 
	    
	    ;; -------- keymap(s) --------
	    (define meta-keymap-functions (make-vector 256))
	    (define keymap-functions (make-vector 256 #f))

	    (define keymap (dilambda
			    (lambda (c)
			      (cond ((char? c) (keymap-functions (char->integer c)))
				    ((integer? c) (keymap-functions c))
				    ((string? c)
				     (if (= (length c) 1)
					 (keymap-functions (char->integer (c 0)))
					 (if (and (= (length c) 2)
						  (char=? (c 0) #\escape))
					     (meta-keymap-functions (char->integer (c 1)))
					     (lambda (c) #t))))
				    (else (error 'wrong-type-arg "keymap takes a character or string argument"))))
			    (lambda (c f)
			      (cond ((char? c) (set! (keymap-functions (char->integer c)) f))
				    ((integer? c) (set! (keymap-functions c) f))
				    ((string? c)
				     (if (= (length c) 1)
					 (set! (keymap-functions (char->integer (c 0))) f)
					 (if (and (= (length c) 2)
						  (char=? (c 0) #\escape))
					     (set! (meta-keymap-functions (char->integer (c 1))) f))))
				    (else (error 'wrong-type-arg "set! keymap takes a character or string first argument"))))))
	    
	    (define C-a 1)     ; #\x01 etc
	    (define C-b 2)
	    (define C-d 4)
	    (define C-e 5)
	    (define C-f 6)
	    (define C-h 8)
	    (define C-k 11)
	    (define C-l 12)
					;(define C-m 13)    ; #\return -- Enter handles this case (crlf?)
	    (define C-n 14)
	    (define C-p 16)
					;(define C-r 18)
	    (define C-t 20)
	    (define C-y 25)
	    (define C-_ 31)
	    (define Tab 9)
	    (define Enter 10)           ; #\linefeed
	    (define Backspace 127)
	    (define Escape 27)          ; #\escape
	    
	    (define (end-of-line pos)             ; prompt or #\newline mark the line boundary
	      (let ((len (length current-line)))
		(do ((i (max 0 (min pos len)) (+ i 1)))
		    ((or (>= i len)
			 (char=? (current-line i) #\newline))
		     (if (>= i len) len (max 0 (- i 1)))))))
	    
	    (define (start-of-line pos)
	      (if (<= pos 0)
		  0
		  (do ((i (min pos (- (length current-line) 1)) (- i 1)))
		      ((or (zero? i)
			   (char=? (current-line i) #\newline))
		       (if (zero? i) 0 (+ i 1))))))
	    
	    (define (count-newlines line)
	      (let ((len (length line))
		    (newlines 0))
		(do ((i 0 (+ i 1)))
		    ((= i len) newlines)
		  (if (char=? (current-line i) #\newline)
		      (set! newlines (+ newlines 1))))))
	    
	    (define (append-newline)
	      (set! current-line (string-append current-line (string #\space #\newline)))
	      (set! cursor-position (length current-line))
	      (when (= last-row (+ prompt-row current-row))
		(format *stderr* "~%")
		(set! prompt-row (- prompt-row 1)))
	      (set! current-row (+ current-row 1)))
	    
	    (define (word-break pos) ; assume we're at the start of a word
	      (let ((len (length current-line)))
		(let loop ((i pos))
		  (if (or (>= i len)
			  (not (or (char-alphabetic? (current-line i))
				   (char-numeric? (current-line i)))))
		      i
		      (loop (+ i 1))))))

	    (define (save-line)
	      (set! previous-line (cons (cons cursor-position (copy current-line)) previous-line)))
	    
	    (let ((main-keyfunc (lambda (c)
				  (if (<= chars 1) (save-line))
				  (if (= cursor-position (length current-line))
				      (set! current-line (string-append current-line (string c)))
				      (if (= cursor-position 0)
					  (set! current-line (string-append (string c) current-line))
					  (set! current-line (string-append 
							      (substring current-line 0 cursor-position) 
							      (string c) 
							      (substring current-line cursor-position)))))
				  (set! cursor-position (+ cursor-position 1))
				  (set! history-index 0)))
		  (no-op-keyfunc (lambda (c) #t)))

	      (do ((i 0 (+ i 1)))
		  ((= i 32))
		(set! (keymap-functions i) no-op-keyfunc))

	      (do ((i 32 (+ i 1)))
		  ((= i 256))
		(set! (keymap-functions i) main-keyfunc))

	      (do ((i 0 (+ i 1)))
		  ((= i 256))
		(set! (meta-keymap-functions i) no-op-keyfunc)))


	    ;; -------- cursor movement 
	    (set! (keymap-functions C-a) 
		  (lambda (c)
		    (set! cursor-position (start-of-line cursor-position))))
	    
	    (set! (keymap-functions C-e) 
		  (lambda (c)
		    (set! cursor-position (end-of-line cursor-position))))
	    
	    (set! (keymap-functions C-b) 
		  (lambda (c)
		    (when (> cursor-position 0)
		      (set! cursor-position (- cursor-position 1))
		      (if (char=? (current-line cursor-position) #\newline)
			  (set! cursor-position (- cursor-position 1))))))
	    
	    (set! (keymap-functions C-f) 
		  (lambda (c)
		    (let ((len (length current-line)))
		      (when (< cursor-position len)
			(set! cursor-position (+ cursor-position 1))
			(if (and (< cursor-position len)
				 (char=? (current-line cursor-position) #\newline))
			    (set! cursor-position (+ cursor-position 1)))))))
	    
	    (set! (keymap-functions C-p) 
		  (lambda (c)
		    ;; try to find corresponding column in previous line
		    (let ((start (start-of-line cursor-position)))
		      (when (positive? start)
			(let ((upstart (start-of-line (- start 2)))
			      (upend (end-of-line (- start 2)))
			      (line-pos (- cursor-position start)))
			  (set! cursor-position (min (+ upstart line-pos) upend)))))))
	    
	    (set! (keymap-functions C-n) 
		  (lambda (c)
		    ;; try to find corresponding column in next line
		    (let ((start (start-of-line cursor-position))
			  (len (length current-line))
			  (next-start (+ (end-of-line cursor-position) 1)))      ; should be at #\newline (if any)
		      (if (> len next-start)                                     ; not already at last line
			  (let ((next-end (end-of-line (+ next-start 1)))
				(line-pos (- cursor-position start)))
			    (set! cursor-position (min (+ next-start 1 line-pos) next-end)))))))
	    
	    (set! (keymap-functions C-l) 
		  (lambda (c)
		    (format *stderr* "~C[H~C[J" #\escape #\escape)
		    (new-prompt)))

	    ;; -------- deletion
	    (set! (keymap-functions C-d) 
		  (lambda (c)
		    (let ((len (length current-line)))
		      (when (< cursor-position len)
			(save-line)
			(do ((i cursor-position (+ i 1)))
			    ((>= i (- len 1)))
			  (set! (current-line i) (current-line (+ i 1))))
			(set! current-line (substring current-line 0 (- len 1)))))))
	    
	    (set! (keymap-functions C-h) 
		  (lambda (c)
		    (when (positive? cursor-position)
		      (save-line)
		      (let ((len (length current-line)))
			(set! cursor-position (- cursor-position 1))
			(do ((i cursor-position (+ i 1)))
			    ((>= i (- len 1)))
			  (set! (current-line i) (current-line (+ i 1))))
			(set! current-line (substring current-line 0 (- len 1)))))))
	    
	    (set! (keymap-functions Backspace) 
		  (keymap-functions C-h))
	    
	    (set! (keymap-functions C-k) 
		  (lambda (c)
		    (let ((len (length current-line)))
		      (when (< cursor-position len)
			(save-line)
			(let ((end (end-of-line cursor-position)))
			  (if (= end len)
			      (begin
				(set! selection (substring current-line cursor-position))
				(set! current-line (substring current-line 0 cursor-position)))
			      (if (= cursor-position end) ; delete next newline
				  (set! current-line (string-append (substring current-line 0 (+ end 1))
								    (substring current-line (+ end 2))))
				  (begin
				    (set! selection (substring current-line cursor-position (+ end 1)))
				    (set! current-line (string-append (substring current-line 0 cursor-position)
								      (substring current-line (+ end 1))))
				    (if (positive? cursor-position)
					(set! cursor-position (- cursor-position 1)))
				    ))))))))

	    ;; -------- undo/selection
	    (set! (keymap-functions C-y) 
		  (lambda (c)
		    (when selection
		      (save-line)
		      (if (zero? cursor-position)
			  (set! current-line (string-append selection current-line))
			  (if (>= cursor-position (length current-line))
			      (set! current-line (string-append current-line selection))
			      (set! current-line (string-append (substring current-line 0 cursor-position)
								selection
								(substring current-line cursor-position)))))
		      (set! cursor-position (+ cursor-position (length selection))))))
	    
	    (set! (keymap-functions C-_) 
		  (lambda (c)
		    (when (pair? previous-line)
		      (set! current-line (cdar previous-line))
		      (set! cursor-position (caar previous-line))
		      (set! previous-line (cdr previous-line))
		      #f)))
	    
	    ;; -------- transpose
	    (set! (keymap-functions C-t) 
		  (lambda (c)
		    (let ((start (start-of-line cursor-position))
			  (end (end-of-line cursor-position))
			  (len (length current-line)))
		      (when (and (> end start)
				 (> cursor-position start))
			(save-line)
			(let ((cur (if (or (= cursor-position end)
					   (< end len))
				       (- cursor-position 1)
				       cursor-position)))
			  (let ((tmp-c (current-line (- cur 1))))
			    (set! (current-line (- cur 1)) (current-line cur))
			    (set! (current-line cur) tmp-c)
			    (set! cursor-position (+ cur 1))))))))
	    
	    ;; -------- indentation/completion
	    (set! (keymap-functions Tab) 
		  (lambda (c)
		    ;; if user pastes in a selection, it may have embedded tabs which are just spacing, 
		    ;;   not requests for filename completion!  We'll try to catch this case by assuming
		    ;;   that a selection will not be the single character #\tab
		    (if (> chars 1)
			(begin
			  (set! current-line (string-append current-line "    "))
			  (set! cursor-position (+ cursor-position 4)))
			(let ((start (start-of-line cursor-position))
			      (end (end-of-line cursor-position))
			      (completion #f))
			  (if (and (positive? start)
				   (= end start))
			      (indent start)
			      (if (= cursor-position end)
				  (let ((loc (do ((i (- end 1) (- i 1)))
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
					       (> (length completion) (- end loc 1)))
				      (save-line)
				      (if (= end (length current-line))
					  (set! current-line (string-append (substring current-line 0 (+ loc 1)) completion))
					  (set! current-line (string-append (substring current-line 0 (+ loc 1)) 
									    completion
									    (substring current-line (+ end 1)))))
				      (set! cursor-position (end-of-line cursor-position))))))))))
	    
	    ;; -------- evaluation/multiline
	    (set! (keymap-functions Enter) 
		  (lambda (c)
		    
		    (call-with-exit
		     (lambda (return)
		       (let ((len (length current-line)))
			 
			 (do ((i 0 (+ i 1))) ; check for just whitespace
			     ((or (= i len)
				  (not (char-whitespace? (current-line i))))
			      (when (= i len)
				(append-newline)
				(return))))
			 
			 (set! red-par-pos #f)
			 (display-lines)
			 
			 (with-repl-let
			  (catch #t
			    (lambda ()
			      
			      ;; we want to add current-line (copied) to the history buffer
			      ;;   unless it is an on-going edit (missing close paren)
			      (catch 'incomplete-expr
				(lambda ()
				  (set! cursor-position len)
				  (display-lines)
				  (set! (history) (copy current-line))
				  ;(append-newline) ; need newline now if evaluation prints something
				  (format *stderr* "~%~S~%" (eval-string current-line (rootlet))))
				(lambda args
				  (pop-history)    ; remove last history entry
				  (append-newline)
				  (return))))
			    
			    (lambda (type info)
			      (format *stderr* "~A: " (red "error"))
			      (apply format *stderr* info)
			      (newline *stderr*))))
			 
			 (new-prompt))))))
	    
	    ;; -------- escaped (Meta/Alt/arrow) keys
	    (set! (keymap-functions Escape) 
		  (lambda (esc)
		    (let ((chr (next-char)))
		      ((meta-keymap-functions (char->integer chr)) chr))))
		    
	    (set! (meta-keymap-functions (char->integer #\[))
		  (lambda (c)
		    ;; arrow-keys etc
		    (let ((chr (next-char)))
		      (case chr
			((#\A) ((keymap-functions C-p) C-p))
			((#\B) ((keymap-functions C-n) C-n))
			((#\C) ((keymap-functions C-f) C-f))
			((#\D) ((keymap-functions C-b) C-b))))))
	    ;; (#\1) here might be F1?
			
	    (set! (meta-keymap-functions (char->integer #\<))
		  (lambda (c) 
		    (set! cursor-position 0)))

	    (set! (meta-keymap-functions (char->integer #\>))
		  (lambda (c) 
		    (set! cursor-position (length current-line))))
			
	    ;; ((#\x02) ; C-M-b
			
	    (set! (meta-keymap-functions (char->integer #\c))
		  (lambda (c) 
		    (let ((len (length current-line)))
		      (let loop ((i cursor-position))
			(if (< i len)
			    (if (char-alphabetic? (current-line i))
				(begin
				  (save-line)
				  (set! (current-line i) (char-upcase (current-line i)))
				  (set! cursor-position (word-break i)))
				(loop (+ i 1))))))))

	    (set! (meta-keymap-functions (char->integer #\p))
		  (lambda (c) 
		    (let ((old-index history-index))
		      (when (and (zero? history-index) 
				 (> (length current-line) 0))
			(set! (history) current-line)
			(set! history-index -1))
		      (set! history-index (max (- history-index 1) (- current-history-size)))
		      (if (history history-index)
			  (begin
			    (set! current-line (history history-index))
			    (set! cursor-position (length current-line))
			    (let ((newlines (count-newlines current-line)))
			      (when (< last-row (+ prompt-row newlines))
				(format *stderr* "~NC" (- (+ prompt-row newlines) last-row) #\newline)
				(set! prompt-row (- prompt-row newlines)))
			      (set! current-row newlines))
			    #f)
			  (set! history-index old-index)))))
			
	    (set! (meta-keymap-functions (char->integer #\n))
		  (lambda (c) 
		    (set! history-index (min 0 (+ history-index 1)))
		    (when (history history-index)
		      (set! current-line (history history-index))
		      (set! cursor-position (length current-line))
		      (let ((newlines (count-newlines current-line)))
			(when (< last-row (+ prompt-row newlines))
			  (format *stderr* "~NC" (- (+ prompt-row newlines) last-row) #\newline)
			  (set! prompt-row (- prompt-row newlines)))
			(set! current-row newlines)))
		    #f))
		  
	    (set! (meta-keymap-functions (char->integer #\u))
		  (lambda (c) 
		    (let ((len (length current-line)))
		      (do ((i cursor-position (+ i 1)))
			  ((or (= i len)
			       (char-alphabetic? (current-line i)))
			   (when (< i len)
			     (save-line)
			     (do ((k i (+ k 1)))
				 ((or (= k len)
				      (not (char-alphabetic? (current-line k))))
				  (set! cursor-position k))
			       (set! (current-line k) (char-upcase (current-line k))))))))))
			
	    (set! (meta-keymap-functions (char->integer #\l))
		  (lambda (c) 
		    (let ((len (length current-line)))
		      (do ((i cursor-position (+ i 1)))
			  ((or (= i len)
			       (char-alphabetic? (current-line i)))
			   (when (< i len)
			     (save-line)
			     (do ((k i (+ k 1)))
				 ((or (= k len)
				      (not (char-alphabetic? (current-line k))))
				  (set! cursor-position k))
			       (set! (current-line k) (char-downcase (current-line k))))))))))
	    
	    
	    ;; -------- terminal setup --------
	    (let ((saved #f)
		  (terminal-fd (fileno stdin))
		  (tty #t))
	      
	      ;; we're in libc here, so exit is libc's exit!
	      (define (tty-reset no)
		(if tty (tcsetattr terminal-fd TCSAFLUSH saved))
		(if (not (equal? input-fd terminal-fd)) (close input-fd))
		(#_exit))
	      
	      (set! ((rootlet) 'exit) ; we'd like this to happen when user types "(exit)"
		    (lambda ()
		      (newline *stderr*)
		      (tty-reset 0)))
	      
	      (define* (run file)
		;; check for dumb terminal
		(if (or (zero? (isatty terminal-fd))        ; not a terminal -- input from pipe probably
			(string=? (getenv "TERM") "dumb"))  ; no vt100 codes -- emacs shell for example
		    (let ((buf (c-pointer->string (calloc 512 1) 512)))
		      (set! tty #f)
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
				   (with-repl-let
				    (catch #t
				      (lambda ()
					(format *stderr* "~S~%" (eval-string (substring buf 0 (- (strlen buf) 1)) (rootlet))))
				      (lambda (type info)
					(format *stderr* "error: ")
					(apply format *stderr* info)
					(newline *stderr*))))
				   (format *stderr* "> ")))))))))
	      
		;; not a pipe or a dumb terminal -- hopefully all others accept vt100 codes

		;; (if (or (equal? (signal SIGINT tty-reset) SIG_ERR)  ; are these needed? as far as I can see, no.
		;; 	   (equal? (signal SIGQUIT tty-reset) SIG_ERR)
		;;	   (equal? (signal SIGTERM tty-reset) SIG_ERR)
		;;	   (negative? (tcgetattr terminal-fd saved)))
		;;      (#_exit))
		
		(let ((buf (termios.make))
		      (read-size 128))

		  (set! next-char                                     ; this indirection is needed if user pastes the selection into the repl
			(let* ((c (make-string read-size #\null)) 
			       (cc (string->c-pointer c))
			       (csize read-size)
			       (ctr 0))
			  (lambda ()
			    (when (>= ctr chars)
			      (set! ctr 0)
			      (set! chars (read input-fd cc read-size))
			      (if (= chars 0)
				  (tty-reset terminal-fd))

			      (when (= chars read-size)
				;; concatenate buffers until we get the entire selection
				(let ((str (substring c 0 read-size)))
				  (let reading ((num (read input-fd cc read-size)))
				    (set! str (string-append str (substring c 0 num)))
				    (set! chars (+ chars num))
				    (if (= num read-size)
					(reading (read input-fd cc read-size))))

				  ;; avoid time-consuming redisplays
				  (catch #t
				    (lambda ()
				      (do ((i 0 (+ i 1)))
					  ((= i (- chars 1)))
					((keymap-functions (char->integer (str i))) (str i))))
				    (lambda (type info)
				      (format *stderr* "~C[~D;~DH" #\escape prompt-row prompt-col)
				      (format *stderr* "internal error: ")
				      (apply format *stderr* info)
				      (newline *stderr*)
				      (format *stderr* "line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
				      (new-prompt)
				      (set! (str (- chars 1)) #\null)))

				  ;; handle last char normally
				  (set! (c 0) (str (- chars 1)))
				  (set! chars 1)
				  (set! ctr 0))))

			    (let ((result (c ctr)))
			      (set! ctr (+ ctr 1))
			      result))))

		  (set! input-fd (if (not file) 
				terminal-fd
				(open file O_RDONLY 0)))
		  (set! saved (termios.make))

		  (tcgetattr terminal-fd buf)
		  (termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
		  (termios.set_c_cc buf VMIN 1)
		  (termios.set_c_cc buf VTIME 0)
		  (when (negative? (tcsetattr terminal-fd TCSAFLUSH buf))
		    (tty-reset terminal-fd))
		  
		  
		  ;; -------- the repl --------
		  (display-prompt)
		  (cursor-bounds)
		  (if (string=? (getenv "HOME") "/home/bil") (debug-help))

		  (do () ()
		    (catch #t
		      (lambda ()
			(let ((chr (next-char)))
			  ((keymap-functions (char->integer chr)) chr)
			  (check-parens)
			  (display-lines)
			  (help chr)))
		      
		      (lambda (type info)
			(format *stderr* "~C[~D;~DH" #\escape prompt-row prompt-col)
			(format *stderr* "internal error: ")
			(apply format *stderr* info)
			(newline *stderr*)
			(format *stderr* "line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
			(new-prompt))))))
	      
	      (curlet)))))))
      
      (set! keymap (repl-let 'keymap))
      (set! history (repl-let 'history))
      (set! save-history (repl-let 'save-history))
      (set! restore-history (repl-let 'restore-history))
      (set! prompt (repl-let 'prompt))
      (set! run (repl-let 'run))
      
      (curlet))))

;; ((*repl* 'run))


#|
to add/change keymap entry:
(set! ((*repl* 'keymap) (integer->char 12)) ; C-l will expand to "(lambda " at the cursor
      (lambda (c)
	(with-let (*repl* 'repl-let)
	   (if (zero? cursor-position)
	       (set! current-line (string-append "(lambda " current-line))
	       (if (>= cursor-position (length current-line))
		   (set! current-line (string-append current-line "(lambda "))
		   (set! current-line (string-append (substring current-line 0 cursor-position)
						     "(lambda "
						     (substring current-line cursor-position)))))
	   (set! cursor-position (+ cursor-position (length "(lambda "))))))

change the prompt:
(set! ((*repl* 'prompt)) "scheme> ")

red lambda prompt: 
(with-let (*repl* 'repl-let)
  (set! prompt-string (bold (red (string #\xce #\xbb #\> #\space))))
  (set! prompt-length 3)) ; until we get unicode length calc

to post a help string (kinda tedious, but the helper list is aimed more at posting variable values):
(set! (*repl* 'helpers)
      (list (lambda (c)
	      (let ((sym (with-let (*repl* 'repl-let)
			   (let ((len (length current-line)))
			     (let loop ((i (min (- len 1) cursor-position)))
			       (and (not (negative? i))
				    (if (char=? (current-line i) #\()
					(let loop1 ((k (+ i 1)))
					  (and (< k len)
					       (if (not (char-alphabetic? (current-line k)))
						   (and (> k (+ i 1))
							(string->symbol (substring current-line (+ i 1) k)))
						   (loop1 (+ k 1)))))
					(loop (- i 1)))))))))
		(if sym
		    (let ((str (help sym)))
		      (if str
			  (substring (help sym) 0 (min (length str) 40))
			  ""))
		    "")))))

;; "function keys":
(set! ((*repl* 'keymap) (string #\escape #\[))
      (lambda (c)
	(with-let (*repl* 'repl-let)
	  (let ((next (next-char)))
	    (case next
	      ((#\A) ((keymap-functions C-p) C-p))
	      ((#\B) ((keymap-functions C-n) C-n))
	      ((#\C) ((keymap-functions C-f) C-f))
	      ((#\D) ((keymap-functions C-b) C-b))
	      ((#\1) ; on my system F1 is esc [ 1 1 ~, F2 esc [ 1 2 ~, etc (but they skip F6?)
	       (let ((n (- (char->integer (next-char)) (char->integer #\0))))
		 (next-char) ; throw away the tilde
		 (set! current-line (string-append current-line (format #f "--F~D!--" n))))))))))

;; this actually works but doesn't echo the filename correctly and the cursor is off during the filename processing
(set! ((*repl* 'keymap) (integer->char 24)) ; C-x
      (lambda (c)
	(with-let (*repl* 'repl-let)
	  (let ((next (next-char)))
	    (when (char=? next (integer->char 6)) ; C-f
	      (format *stderr* "~%load: ")
	      ;; now recursive call: prompt="load: "
	      (let ((old-prompt-string prompt-string)
		    (old-prompt-length prompt-length)
		    (old-current-line current-line)
		    (old-cursor-position cursor-position)
		    (old-enter-func (keymap-functions 10))
		    (filename #f))
		(set! prompt-string "load: ")
		(set! prompt-length 6)
		(set! current-line "")
		(set! cursor-position 0)
		(set! (keymap-functions 10) (lambda (c)
					      (set! filename current-line)
					      (set! prompt-string old-prompt-string)
					      (set! prompt-length old-prompt-length)
					      (set! current-line old-current-line)
					      (set! cursor-position old-cursor-position)
					      (set! (keymap-functions 10) old-enter-func)))
		(do ((c (next-char) (next-char)))
		    ((string? filename))
		  ((keymap-functions (char->integer c)) c)
		  (display-lines))
		(load filename)))))))
|#



;; need some sort of auto-test: repl.input
;; possibly notice bad input?
;; unicode someday
;; C-t might be uncanonical, C-k is messed up

