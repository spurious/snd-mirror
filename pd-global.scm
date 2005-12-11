

;; These functions are global functions available for all guile scripts loaded into PD.
;; Kjetil S. Matheussen, 2004.

;;/* This program is free software; you can redistribute it and/or                */
;;/* modify it under the terms of the GNU General Public License                  */
;;/* as published by the Free Software Foundation; either version 2               */
;;/* of the License, or (at your option) any later version.                       */
;;/*                                                                              */
;;/* This program is distributed in the hope that it will be useful,              */
;;/* but WITHOUT ANY WARRANTY; without even the implied warranty of               */
;;/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                */
;;/* GNU General Public License for more details.                                 */
;;/*                                                                              */
;;/* You should have received a copy of the GNU General Public License            */
;;/* along with this program; if not, write to the Free Software                  */
;;/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */
;;/*                                                                              */


(provide 'snd-pd-global.scm)

(debug-enable 'debug)
(debug-enable 'trace)
(debug-enable 'backtrace)

(use-modules (ice-9 stack-catch))


(set! (show-backtrace) #t)
(debug-enable 'debug)
(if #t
    (begin
      (read-enable 'positions)
      (debug-enable 'debug)
      (debug-enable 'backtrace)
      (debug-set! frames 8)
      (debug-set! depth 50)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. functions
;;
;;
(define (pd-load-if-exists filename)
  (if (access? filename F_OK)
      (load filename)))

(define (pd-display . args)
  (if (not (null? args))
      (begin
	(display (car args))
	(apply pd-display (cdr args)))
      (newline)))

(define (pd-filter proc list)
  (if (null? list)
      '()
      (if (proc (car list))
	  (cons (car list) (pd-filter proc (cdr list)))
	  (pd-filter proc (cdr list)))))

(define (pd-for init pred least add proc)
  (if (pred init least)
      (begin
	(proc init)
	(pd-for (+ add init) pred least add proc))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument checking
;;
;;
(define (pd-check-number number message)
  (if (number? number)
      #t
      (begin
	(pd-display message ": " number " is not a number")
	#f)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;
;;
(define pd-global-bindings '())

(define (pd-bind-do symbol func bindings)
  (if (or (not (symbol? symbol))
	  (not (procedure? func)))
      (begin
	(pd-display "Wrong arguments for pd-bind")
	bindings)
      (cons (list symbol 
		  func
		  (pd-c-bind symbol func))
	    bindings)))

(define (pd-unbind-do symbol bindings)
  (if (not (symbol? symbol))
      (begin
	(pd-display "Wrong arguments for pd-unbind")
	bindings)
      (let ((binding (assq symbol bindings)))
	(pd-c-unbind (caddr binding) symbol)
	(pd-filter (lambda (x) (not (eq? symbol (car x))))
		   bindings))))

(define (pd-bind symbol func)
  (set! pd-global-bindings (pd-bind-do symbol func pd-global-bindings)))

(define (pd-unbind symbol)
  (set! pd-global-bindings (pd-unbind-do symbol pd-global-bindings)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending
;;
;;
(define (pd-send symbol firstarg . args)
  (if (or (symbol? symbol)
	  (number? symbol))
      (cond ((> (length args) 0) (pd-c-send-list symbol (cons firstarg args)))
	    ((list? firstarg) (pd-c-send-list symbol firstarg))
	    ((number? firstarg) (pd-c-send-number symbol firstarg))
	    ((string? firstarg) (pd-c-send-string symbol firstarg))
	    ((eq? 'bang firstarg) (pd-c-send-bang symbol))
	    ((symbol? firstarg) (pd-c-send-symbol symbol firstarg))
	    (else
	     (pd-display "Unknown argument to pd-outlet-or-send:" firstarg)))))

(define (pd-get-symbol sym)
  (if (not (symbol? sym))
      (pd-display sym " is not a scheme symbol")
      (pd-c-get-symbol sym)))


#!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate strings from stdin
;; (incomplete, used snd_eval_stdin_str instead)
;;
(define pd-eval-strings
  (let ((to-be-evaluated "")
	(insidestring #f)
	(inside-commented-block 0)
	(inside-commented-line #f)
	(balance 0)) ;; parantheses
    (define (eval-it!)
      ;(pd-display "about to evaluate -" to-be-evaluated "-")
      (if (not (string=? "" to-be-evaluated))
	  (pd-display (eval-string to-be-evaluated)))
      (display "snd-pd> ")
      (set! to-be-evaluated ""))
    (define (add-char! c)
      (set! to-be-evaluated (string-append/shared to-be-evaluated (string c))))
    (define (left-par!)
      (add-char! #\()
      (set! balance (1+ balance)))
    (define (right-par!)
      (if (> balance 0)
	  (begin
	    (add-char! #\))
	    (set! balance (1- balance))
	    (if (= 0 balance)
		(eval-it!)))))
    (lambda (string)
      ;(pd-display "debug" to-be-evaluated balance)
      (for-each (lambda (c)
		  ;(pd-display "checking" c "-" to-be-evaluated "-")
		  (if (and (= -1 inside-commented-block)
			   (char=? c #\!))
		      (set! inside-commented-block 2)
		      (begin
			(if (= -1 inside-commented-block)
			    (set! inside-commented-block 0))
			(cond (insidestring
			       (if (char=? c #\")
				   (set! insidestring #f))
			       (add-char! c))
			      ((= inside-commented-block 2)
			       (if (char=? c #\!)
				   (set! inside-commented-block 1)))
			      ((= inside-commented-block 1)
			       (if (char=? c #\#)
				   (set! inside-commented-block 0)
				   (set! inside-commented-block 2)))
			      (else
			       (cond ((char=? c #\() (left-par!))
				     ((char=? c #\)) (right-par!))
				     ((and (or (char=? c #\ )
					       (char=? c #\newline))
					   (= 0 balance))
				      (eval-it!))
				     ((char=? c #\")
				      (set! insidestring #t)
				      (add-char! c))
				     ((char=? c #\#)
				      (set! inside-commented-block -1))
				     (else
				      (add-char! c))))))))
		(string->list string)))))
  
!#


(pd-load-if-exists "/etc/.k_guile.scm")
(pd-load-if-exists (string-append (getenv "HOME") "/.k_guile.scm"))




