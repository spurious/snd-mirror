#!/usr/bin/env guile \
-e main -s
!#
;;; agn.scm -- Bill Schottstaedt's agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)

;; Translator/Author: Michael Scholz <scholz-micha@gmx.de>
;; Created: Tue Jun 24 19:05:06 CEST 2003
;; Last: Mon May 10 19:10:02 CEST 2004

;; This file is part of Sndins.

;; Type (do-agn)
;; or start the script in a shell.

(use-modules (ice-9 format) (ice-9 optargs))

(if (not (provided? "sndlib")) (load-extension "libsndlib" "init_sndlib"))
(if (not (provided? "sndins")) (load-extension "libsndins" "init_sndins"))

(if (not (defined? 'with-sound))
    (if (defined? 'open-sound)		;calling from Snd or from a shell?
	(load-from-path "ws")
	(load "ws_s.scm")))

(define *clm-play* #t)
(define *clm-statistics* #t)
(define *clm-verbose* #t)
(define *clm-srate* 44100)
(define *clm-channels* 2)
(define *clm-reverb* freeverb)
(define *clm-reverb-data* '(#:room-decay 0.9))
(define *clm-reverb-channels* 2)
(define *clm-delete-reverb* #t)
(define *clm-locsig-type* mus-interp-sinusoidal)
;; mus-interp-linear or mus-interp-sinusoidal

(define (main args)
  (do-agn (if (= 2 (length args)) (cadr args) "agn.clm")))

(define* (do-agn #:optional (file "agn.clm"))
  (let ((sndfile (format #f "~A.snd" (basename file ".clm"))))
    (snd-msg ";; Writing ~S~%" file)
    (agn file)
    (with-sound (#:output sndfile)
		(snd-msg ";; Loading ~S~%" file)
		(load file))))

(if (not (defined? 'envelope-interp))
    (define* (envelope-interp #:rest args)
      (let ((x (car args))
	    (env (cadr args))
	    (base (if (null? (cddr args)) #f (caddr args))))
	(cond ((null? env) 0.0)
	      ((or (<= x (car env))
		   (null? (cddr env)))
	       (cadr env))
	      ((> (caddr env) x)
	       (if (or (= (cadr env) (cadddr env))
		       (and base (= base 0.0)))
		   (cadr env)
		   (if (or (not base) (= base 1.0))
		       (+ (cadr env)
			  (* (- x (car env))
			     (/ (- (cadddr env) (cadr env))
				(- (caddr env) (car env)))))
		       (+ (cadr env)
			  (* (/ (- (cadddr env) (cadr env))
				(- base 1.0))
			     (- (expt base (/ (- x (car env))
					      (- (caddr env) (car env))))
				1.0))))))
	      (else (envelope-interp x (cddr env)))))))

(define (snd-msg frm . args)
  (let ((str (apply format (append (list #f frm) args))))
    (if (and (defined? 'open-sound)
	     (not (getenv "EMACS")))
	(snd-print str)
	(display str))))

(define lim 256)
(define time 60)
(define mode (list->array 1 '(0 0 2 4 11 11 5 6 7 0 0 0 0)))
(define rats (list->array 1 '(1.0 256/243 9/8 32/27 81/64 4/3 1024/729
				  3/2 128/81 27/16 16/9 243/128 2.0)))

(define bell '(0 0 10 0.25 90 1.0 100 1.0))

(define octs (make-array 0 (1+ lim)))
(define pits (make-array 0 (1+ lim)))
(define rhys (make-array 0 (1+ lim)))
(define amps (make-array 0 (1+ lim)))

(define (tune x)
  (let* ((pit (modulo x 12))
	 (oct (inexact->exact (floor (/ x 12))))
	 (base (array-ref rats pit)))
    (* base (expt 2 oct))))

(define (rbell x)
  (envelope-interp (* x 100) bell))

(define* (glog r #:optional b)
  (if (<= r 0) (error "r must be > 0"))
  (if (and b (<= b 0)) (error "b must be > 0"))
  (if b (/ (log r) (log b)) (log r)))

(define (agn file)
  (let ((wins (list->array 1 '((0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			       (0 0 60 0.1 80 0.2 90 0.4 95 1 100 0)
			       (0 0 10 1 16 0 32 0.1 50 1 56 0 60 0 90 0.3 100 0)
			       (0 0 30 1 56 0 60 0 90 0.3 100 0)
			       (0 0 50 1 80 0.3 100 0)
			       (0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			       (0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			       (0 0 10 1 32 0.1 50 1 90 0.3 100 0)
			       (0 0 60 0.1 80 0.3 95 1 100 0)
			       (0 0 80 0.1 90 1 100 0)))))
    (do ((i 0 (1+ i)))
	((= i (+ lim 1)))
      (array-set! octs (inexact->exact (floor (+ 4 (* 2 (rbell (random 1.0)))))) i)
      (array-set! pits (array-ref mode (inexact->exact (floor (* 12 (random 1.0))))) i)
      (array-set! rhys (inexact->exact (floor (+ 4 (* 6 (random 1.0))))) i)
      (array-set! amps (inexact->exact (floor (+ 1 (* 8 (rbell (random 1.0)))))) i))
    (call-with-output-file file
      (lambda (out-port)
	(format out-port ";; from agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)~%~%")
	(do ((i 1 (1+ i)))
	    ((> i 3))
	  (let ((cellbeg 0)
		(cellsiz 4)
		(cellctr 0)
		(whichway 1)
		(base i)
		(mi (- i 1))
		(winnum 0)
		(mytempo 0.2)
		(nextbeg 0.0)
		(revamt 0.0)
		(ranamt 0.0)
		(beg 0.0)
		(dur 0.0)
		(freq 0.0)
		(ampl 0.0)
		(ind 0.0))
	    (while (and (< beg time) (< cellctr lim))
		   (set! beg (+ beg nextbeg))
		   (set! nextbeg (max 0.25 (* mytempo (+ 0.9 (* 0.2 (random 0.1)))
					      (array-ref rhys cellctr))))
		   (set! freq (* (/ 16.352 (expt 2 mi)) (tune (array-ref pits cellctr))
				 (expt 2 (array-ref octs cellctr))))
		   (set! dur nextbeg)
		   (if (< freq 100) (set! dur (+ dur dur)))
		   (set! ampl (max 0.003 (* (array-ref amps cellctr) (/ (* 60 base)))))
		   (set! ind (* (random 1.0) 2 base))
		   (set! cellctr (1+ cellctr))
		   (set! revamt (* base 0.1))
		   (set! winnum (inexact->exact (floor (* 10 (- beg (floor beg))))))
		   (set! ranamt (* 0.00001 (expt (- (glog freq 2.0) 4) 4)))
		   (format out-port
			   (string-append "(fm-violin ~,2F ~,2F ~,3F ~,3F "
					  "#:fm-index ~,2F #:reverb-amount ~,2F "
					  "#:noise-amount ~,2F #:amp-env '~S)~%")
			   beg dur freq ampl ind revamt ranamt (array-ref wins winnum))
		   (set! cellctr (1+ cellctr))
		   (if (> cellctr (+ cellsiz cellbeg))
		       (begin
			 (set! cellbeg (1+ cellbeg))
			 (if (> (random 1.0) 0.5) (set! cellsiz (+ cellsiz whichway)))
			 (if (and (> cellsiz 16) (> (random 1.0) 0.99))
			     (begin
			       (set! whichway -2)
			       (if (and (> cellsiz 12) (> (random 1.0) 0.999))
				   (begin
				     (set! whichway -1)
				     (if (< cellsiz 4)
					 (set! whichway 1))))))
			 (set! cellbeg (+ cellbeg 3))
			 (set! cellctr cellbeg)))))))))
  file)

;; agn.scm ends here
