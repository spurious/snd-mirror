;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 92, 93, 94, 98, 99, 2000, 2001 Fernando Lopez Lezcano. 
;;; All rights reserved.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted and may be copied as long as 
;;; no fees or compensation are charged for use, copying, or accessing
;;; this software and all copies of this software include this copyright
;;; notice. Suggestions, comments and bug reports are welcome. Please 
;;; address email to: nando@ccrma.stanford.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic multichannel three-dimentional signal locator
;;; (wow that sound good! :-)
;;;
;;; by Fernando Lopez Lezcano
;;;    CCRMA, Stanford University
;;;    nando@ccrma.stanford.edu
;;;
;;; Thanks to Juan Pampin for help in the initial coding of the new version
;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
;;; for insights into the Ambisonics coding and decoding process. 
;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

;;; CHANGES:
;;; 04/26/2010: add delay hack to remove artifacts in delay output, fix other bugs (Nando)
;;;             added proper doppler src conversion thanks to Bill's code in dsp.scm
;;;             merged in code for higher order ambisonics (up to 2nd order h/v)
;;; 06/28/2009: remove class/method stuff for s7 (Bill)
;;; 01/08/2007: make a few functions local etc (Bill)
;;; 07/05/2006: translate to scheme, use move-sound generator (Bill)
;;; 04/29/2002: fixed reverb envelopes for no reverb under clisp
;;; 01/14/2001: added multichannel reverb output with local and global control
;;;             in the reverberator (the hrtf code is currently not merged into
;;;             this version). Reverb-amount can now be an envelope. Ambisonics
;;;             now outputs signal to the reverb stream. 
;;; 02/05/2000: . don't compile as part of the clm package, import symbols
;;;             . switched over to clm-2, otherwise convolve HAS to operate
;;;               on a file, we want convolve to process the output of the 
;;;               doppler delay line (for the hrtf code)
;;; 02/03/2000: started working on hrtf's
;;; 01/15/2000: rewrote transform-path code to account for 3d paths
;;; 01/13/2000: . changed order of b-format output file to W:X:Y:Z from X:Y:Z:W
;;;             . plot method would bomb with paths that had constant velocity,
;;;               fixed norm function
;;;             . added make-literal-path and friends to enable to create
;;;               paths with user supplied coordinates
;;;             . decoded-ambisonics was rotating sounds in the wrong direction
;;;             . in change-direction: only check for change if both points are
;;;               different (intersect-inside-radius can create a redundant point)
;;; 11/28/1999: decoded-ambisonics is now working for N channels
;;;             includes reverberation send
;;; 11/27/1999: set minimum segment distance for rendering, otherwise for long
;;;             lines the amplitude envelope does not reflect power curve. 
;;; 11/26/1999: added code to check for intersection with inner radius
;;;             fixed nearest-point to handle trivial cases

;;; 01/21/2001: fix envelope generated for mono reverberation stream.
;;;             change input and output to use frames and mixers
;;;             fix supersonic movement warning code
;;; > add warnings when object goes outside of area covered by speakers
;;; > fix one common vertice case of 3 speaker group transitions
;;; > redo the old code for multiple images (reflections in a rectangular room)
;;;     a bit of a pain, would have to add z (ceiling and floor reflections)
;;;     would be better to find general purpose code for non-rectangular rooms
;;; > we really need a good N-channel reverb [fixed with freeverb]
;;; > change reverb to be multichannel, add local and global reverb
;;;   11/24/1999: should use a waveguide reverb like pph@ccrma project
;;;               would be a good idea to inject the signal through a center
;;;               injection point that moves inside the virtual cube, more like
;;;               a physical model of what actually happens in a room
;;; | add ambisonics back-end
;;;   11/24/1999: code to b-format sort of working
;;;                 how to deal with the inner space and 0:0:0?
;;;               decoded format not working if we incorporate distance att
;;;                 formulas are wrong...
;;; > add hrtf back-end
;;; > extract a supath from an existing path
;;; > recode the transformation functions
;;; > add arcs of circles and other basic geometric paths
;;;     make it so that you can concatenate them...
;;; | 11/25/1999 fix the "diagonal case" (sounds go through the head of the listener)

(provide 'snd-dlocsig.scm)


(define* (envelope-interp x env base)   ;env is list of x y breakpoint pairs, interpolate at x returning y
  "(envelope-interp x env (base 1.0)) -> value of env at x; base controls connecting segment 
type: (envelope-interp .3 '(0 0 .5 1 1 0) -> .6"
  (cond ((null? env) 0.0)		;no data -- return 0.0
	((or (<= x (car env))	        ;we're sitting on x val (or if < we blew it)
	     (null? (cddr env)))	;or we're at the end of the list
	 (cadr env))		        ;so return current y value
	((> (caddr env) x)		;x <= next env x axis value
	 (if (or (= (cadr env) (cadddr env))
		 (and base (= base 0.0)))
	     (cadr env)		;y1=y0, so just return y0 (avoid endless calculations below)
	     (if (or (not base) (= base 1.0))
		 (+ (cadr env)	;y0+(x-x0)*(y1-y0)/(x1-x0)
		    (* (- x (car env))
		       (/ (- (cadddr env) (cadr env))
			  (- (caddr env) (car env)))))
		 (+ (cadr env) ; this does not exactly match xramp-channel
		    (* (/ (- (cadddr env) (cadr env))
			  (- base 1.0))
		       (- (expt base (/ (- x (car env))
					(- (caddr env) (car env))))
			  1.0))))))
	(else (envelope-interp x (cddr env) base)))) ;go on looking for x segment

(define (x-norm env xmax)
  "(x-norm env xmax) changes 'env' x axis values so that they run to 'xmax'"
  (let ((scl (/ xmax (list-ref env (- (length env) 2))))
	(val '())
	(len (length env)))
    (do ((i 0 (+ i 2)))
	((>= i len))
      (set! val (cons (* (list-ref env i) scl) val))
      (set! val (cons (list-ref env (+ i 1)) val)))
    (reverse val)))



;;;;;;;;;;;;;;;;;;;;;
;;; Global Parameters

;;; Define the base in which all angles are expressed
(define dlocsig-one-turn 360)

(define (one-turn-is unit)
  "(one-turn-is unit) sets dlocsig's angle unit (degrees=360, the default or radians=2*pi)"
  (set! dlocsig-one-turn unit)
  unit)

(define (angles-in-degree)
  "(angles-in-degree) sets dlocsig's unit to degrees (the default)"
  (one-turn-is 360))

(define (angles-in-radians)
  "(angles-in-radians) sets dlocsig's unit to radians (default is degrees)"
  (one-turn-is (* 2 pi)))

(define (angles-in-turns)
  "(angles-in-turns) sets dlocsig's angle unit to turns"
  (one-turn-is 1))

;; speed of sound in air, in meters per second under normal conditions
(define dlocsig-speed-of-sound 344)

(define (distances-in-meters)
  "(distances-in-meters) sets dlocsig's distances in meters (the default)"
  (set! dlocsig-speed-of-sound 344)
  344)

(define (distances-in-feet)
  "(distances-in-feet) sets dlocsig's distances in feet (default is meters)"
  (set! dlocsig-speed-of-sound 1128)
  1128)

;; default for whether to use two or three-dimensional speaker configurations
(define dlocsig-3d #f)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speaker Configuration

(define* (make-group (id 0) (size 0) vertices speakers matrix)
  (list 'group id size vertices speakers matrix))

(define group-id (make-procedure-with-setter (lambda (a) (list-ref a 1)) (lambda (a b) (list-set! a 1 b))))
(define group-size (make-procedure-with-setter (lambda (a) (list-ref a 2)) (lambda (a b) (list-set! a 2 b))))
(define group-vertices (make-procedure-with-setter (lambda (a) (list-ref a 3)) (lambda (a b) (list-set! a 3 b))))
(define group-speakers (make-procedure-with-setter (lambda (a) (list-ref a 4)) (lambda (a b) (list-set! a 4 b))))
(define group-matrix (make-procedure-with-setter (lambda (a) (list-ref a 5)) (lambda (a b) (list-set! a 5 b))))


(define* (make-speaker-config number dimension coords groups delays omap)
  (list 'speaker-config number dimension coords groups delays omap))

(define speaker-config-number (make-procedure-with-setter (lambda (a) (list-ref a 1)) (lambda (a b) (list-set! a 1 b))))
(define speaker-config-dimension (make-procedure-with-setter (lambda (a) (list-ref a 2)) (lambda (a b) (list-set! a 2 b))))
(define speaker-config-coords (make-procedure-with-setter (lambda (a) (list-ref a 3)) (lambda (a b) (list-set! a 3 b))))
(define speaker-config-groups (make-procedure-with-setter (lambda (a) (list-ref a 4)) (lambda (a b) (list-set! a 4 b))))
(define speaker-config-delays (make-procedure-with-setter (lambda (a) (list-ref a 5)) (lambda (a b) (list-set! a 5 b))))
(define speaker-config-map (make-procedure-with-setter (lambda (a) (list-ref a 6)) (lambda (a b) (list-set! a 6 b))))


;;; Create a speaker configuration structure based on a list of speakers
;;;
;;; speakers:  list of angles of speakers with respect to 0
;;; delays:    list of delays for each speaker, zero if nil
;;; distances: list relative speaker distances, 
;;;            (instead of delays)
;;; omap:      mapping of speakers to output channels
;;;            content should be output channel number, zero based

(define (cis a)
  "(cis a) returns e^(ia)"
  (exp (* 0.0+1.0i a)))

(defmacro when (test . forms)
  `(if ,test (begin ,@forms)))

(define (third a) 
  "(third lst) returns the 3rd element of 'lst'"
  (if (>= (length a) 3) (list-ref a 2) #f))

(define (fourth a) 
  "(fourth lst) returns the 4th element of 'lst'"
  (if (>= (length a) 4) (list-ref a 3) #f))

(define* (last a n) 
  "(last lst n) returns the last 'n' elements of 'lst' as a list"
  (if (null? a)
      #f
      (if (not n)
	  (list (list-ref a (- (length a) 1)))
	  (let ((res '()))
	    (do ((i 0 (+ 1 i)))
		((= i n))
	      (set! res (cons (list-ref a (- (length a) (+ i 1))) res)))
	    res))))

(define (listp a) 
  "(listp lst) is #t is 'lst' is a non-null list"
  (and (list? a) (not (null? a))))

(define (make-list-1 n val)
  (let ((lst '()))
    (do ((i 0 (+ i 1)))
	((= i n))
      (set! lst (cons val lst)))
    lst))


(define* (arrange-speakers (speakers '())
			   (groups '())
			   (delays '())
			   (distances '())
			   (channel-map '()))
  ;; sanity checking of configuration

  (define (has-duplicates? lst)
    ;; from ice-9/common-list.scm
    (cond ((null? lst) #f)
	  ((member (car lst) (cdr lst)) #t)
	  (else (has-duplicates? (cdr lst)))))

  (define (invert3x3 mat) ; invert a 3x3 matrix using cofactors
    (let ((m (make-mixer 3))
	   (det 0.0)
	   (invdet 0.0))
      (do ((i 0 (+ 1 i)))
	  ((= i 3))
	(do ((j 0 (+ 1 j)))
	    ((= j 3))
	  (set! (mixer-ref m i j) (mixer-ref mat i j))))
      (set! (mixer-ref mat 0 0) (- (* (mixer-ref m 1 1) (mixer-ref m 2 2)) (* (mixer-ref m 1 2) (mixer-ref m 2 1))))
      (set! (mixer-ref mat 0 1) (- (* (mixer-ref m 0 2) (mixer-ref m 2 1)) (* (mixer-ref m 0 1) (mixer-ref m 2 2))))
      (set! (mixer-ref mat 0 2) (- (* (mixer-ref m 0 1) (mixer-ref m 1 2)) (* (mixer-ref m 0 2) (mixer-ref m 1 1))))
      (set! (mixer-ref mat 1 0) (- (* (mixer-ref m 1 2) (mixer-ref m 2 0)) (* (mixer-ref m 1 0) (mixer-ref m 2 2))))
      (set! (mixer-ref mat 1 1) (- (* (mixer-ref m 0 0) (mixer-ref m 2 2)) (* (mixer-ref m 0 2) (mixer-ref m 2 0))))
      (set! (mixer-ref mat 1 2) (- (* (mixer-ref m 0 2) (mixer-ref m 1 0)) (* (mixer-ref m 0 0) (mixer-ref m 1 2))))
      (set! (mixer-ref mat 2 0) (- (* (mixer-ref m 1 0) (mixer-ref m 2 1)) (* (mixer-ref m 1 1) (mixer-ref m 2 0))))
      (set! (mixer-ref mat 2 1) (- (* (mixer-ref m 0 1) (mixer-ref m 2 0)) (* (mixer-ref m 0 0) (mixer-ref m 2 1))))
      (set! (mixer-ref mat 2 2) (- (* (mixer-ref m 0 0) (mixer-ref m 1 1)) (* (mixer-ref m 0 1) (mixer-ref m 1 0))))
      (set! det (+ (* (mixer-ref m 0 0) (mixer-ref mat 0 0))
		   (* (mixer-ref m 0 1) (mixer-ref mat 1 0))
		   (* (mixer-ref m 0 2) (mixer-ref mat 2 0))))
      (if (<= (abs det) 1e-06)
	  #f
	(begin
	 (set! invdet (/ 1.0 det))
	 (do ((row 0 (+ 1 row)))
	     ((= row 3))
	   (do ((col 0 (+ 1 col)))
	       ((= col 3))
	     (set! (mixer-ref mat row col) (* (mixer-ref mat row col) invdet))))
	 mat))))
	 
  (define (invert2x2 mat) ; invert a 2x2 matrix
    (let* ((m (make-mixer 2))
	   (det (- (* (mixer-ref mat 0 0) (mixer-ref mat 1 1))
		   (* (mixer-ref mat 1 0) (mixer-ref mat 0 1)))))
      (if (<= (abs det) 1e-06)
	  #f
	(begin
	 (set! (mixer-ref m 0 0) (/ (mixer-ref mat 1 1) det))
	 (set! (mixer-ref m 1 1) (/ (mixer-ref mat 0 0) det))
	 (set! (mixer-ref m 0 1) (- (/ (mixer-ref mat 0 1) det)))
	 (set! (mixer-ref m 1 0) (- (/ (mixer-ref mat 1 0) det)))
	 m))))

  (if (null? speakers)
      (error 'mus-error "ERROR: a speaker configuration must have at least one speaker!~%"))

  (if (not (null? groups))
      (let ((first-len (length (car groups))))
	(for-each
	 (lambda (group)
	   (if (not (= (length group) first-len))
	       (error 'mus-error "ERROR: all groups must be of the same length! (~A)~%" first-len)))
	 groups))

    ;; if the speakers are defined with only azimuth angles (no elevation)
    (if (not (list? (car speakers)))
	;; then create the groups ourselves because it is a 2d configuration;
	;; we could create the 3d configuration groups but the algorithm for
	;; doing that in the generic case is not trivial

	(let ((len (length speakers)))
	  (if (= len 1)
	      (set! groups (list (list 0)))
	    (begin
	     (do ((i 0 (+ 1 i))
		  (j 1 (+ 1 j)))
		 ((= i len))
	       (set! groups (cons (list i (modulo j len)) groups)))
	     (set! groups (reverse groups)))))))

  (if (null? groups)
      (error 'mus-error "ERROR: no groups specified, speakers must be arranged in groups~%"))

  (if (and (not (null? delays))
	   (not (null? distances)))
      (error 'mus-error "ERROR: please specify delays or distances but not both~%"))

  (if (not (null? delays))
      (if (> (length speakers) (length delays))
	  (error 'mus-error "ERROR: all speaker delays have to be specified, only ~A supplied [~A]~%" (length delays) delays)
	(if (< (length speakers) (length delays))
	    (error 'mus-error "ERROR: more speaker delays than speakers, ~A supplied instead of ~A [~A]~%" (length delays) (length speakers) delays))))

  (if (not (null? delays))
      (for-each
       (lambda (delay)
	 (if (< delay 0.0) (error 'mus-error "ERROR: delays must be all positive, ~A is negative~%" delay)))
       delays))

  (if (not (null? distances))
      (if (> (length speakers) (length distances))
	  (error 'mus-error "ERROR: all speaker distances have to be specified, only ~A supplied [~A]~%" (length distances) distances)
	(if (< (length speakers) (length distances))
	    (error 'mus-error "ERROR: more speaker distances than speakers, ~A supplied instead of ~A [~A]~%" (length distances) (length speakers) distances))))

  (if (not (null? distances))
      (for-each
       (lambda (delay)
	 (if (< delay 0.0) (error 'mus-error "ERROR: distances must be all positive, ~A is negative~%" delay)))
       distances))

  (if (not (null? channel-map))
      (if (> (length speakers) (length channel-map))
	  (error 'mus-error "ERROR: must map all speakers to output channels, only ~A mapped [~A]~%" (length channel-map) channel-map)
	(if (< (length speakers) (length channel-map))
	    (error 'mus-error "ERROR: trying to map more channels than there are speakers, ~A supplied instead of ~A [~A]~%" 
		    (length channel-map) (length speakers) channel-map))))

  ;; collect unit vectors describing the speaker positions
  (let* ((coords
	  (let ((val '()))
	    (for-each
	     (lambda (s) ; speakers
	       (let* ((a (if (list? s) (car s) s))
		      (e (if (list? s) (or (cadr s) 0.0) 0.0))
		      (evec (cis (* (/ e dlocsig-one-turn) 2 pi)))
		      (dxy (real-part evec))
		      (avec (cis (* (/ a dlocsig-one-turn) 2 pi)))
		      (x (* dxy (imag-part avec)))
		      (y (* dxy (real-part avec)))
		      (z (imag-part evec))
		      (mag (sqrt (+ (* x x) (* y y) (* z z)))))
		 (set! val (cons (list (/ x mag) (/ y mag) (/ z mag)) val))))
	     speakers)
	    (reverse val)))

	   ;; minimum distance
	   (min-dist (if (not (null? distances))
			 (let ((mind (car distances)))
			   (for-each 
			    (lambda (d)
			      (if (< d mind) (set! mind d)))
			    distances)
			   mind)
		       0.0))

	   ;; find delay times from specified distances or delays
	   (times (let ((v (make-vct (length speakers))))
		    (do ((i 0 (+ 1 i)))
			((= i (length speakers)))
		      (vct-set! v i (let ((distance (and (not (null? distances)) (list-ref distances i)))
					  (delay (and (not (null? delays)) (list-ref delays i))))
				      (or delay
					  (and distance 
					       (/ (- distance min-dist) dlocsig-speed-of-sound))
					  0.0))))
		    v))

	   ;; create the group structures
	   (groups (let* ((vals '())
			  (id 0))
		     (for-each 
		      (lambda (group)
			(let* ((size (length group))
			       (vertices (map (lambda (n)
						(list-ref coords n))
					      group))
			       (matrix (if (= size 3)
					   (let* ((m (make-mixer 3)))
					     (do ((i 0 (+ 1 i)))
						 ((= i 3))
					       (do ((j 0 (+ 1 j)))
						   ((= j 3))
						 (mixer-set! m i j (list-ref (list-ref vertices i) j))))
					     (invert3x3 m))
					 (if (= size 2)
					     (let* ((m (make-mixer 2)))
					       (do ((i 0 (+ 1 i)))
						   ((= i 2))
						 (do ((j 0 (+ 1 j)))
						     ((= j 2))
						   (mixer-set! m i j (list-ref (list-ref vertices i) j))))
					       (invert2x2 m))
					   #f))))
			  (set! vals (cons (make-group :id id
						       :size size
						       :speakers group
						       :vertices vertices
						       :matrix matrix)
					  vals))
			  (set! id (+ 1 id))))
		      groups)
		     (reverse vals))))
    
      ;; check validity of map entries
    (if channel-map
	(let ((entries (length channel-map)))
	  (for-each
	   (lambda (entry)
	     (if (>= entry entries)
		 (error 'mus-error "ERROR: channel ~A in map ~A is out of range (max=~A)~%" entry channel-map entries)))
	   channel-map)
	  (if (has-duplicates? channel-map)
	      (error 'mus-error "ERROR: there are duplicate channels in channel-map ~A~%" channel-map))))

    ;; create the speaker configuration structure

    (make-speaker-config :number (length speakers)
			 :dimension (group-size (car groups))
			 :coords coords
			 :groups groups
			 :delays times
			 :omap (let ((v (make-vector (length speakers))))
				 (do ((chan 0 (+ 1 chan)))
				     ((= chan (length speakers)))
				   (vector-set! v chan (or (and (not (null? channel-map)) (list-ref channel-map chan))
							   chan)))
				 v))))

;;; Default speaker configurations

(define dlocsig-speaker-configs
  ;; by default up to eight channels, 2-d and 3-d configurations
  (list 
   (list
    ;;
    ;; 2-D speaker configurations
    ;; no channels: impossible
    #f
    ;; mono
    (arrange-speakers :speakers '(0))
    ;; stereo
    (arrange-speakers :speakers '(-60 60))
    ;; three channels
    (arrange-speakers :speakers '(-45 45 180))
    ;; four channels
    (arrange-speakers :speakers '(-45 45 135 225))
    ;; five channels (5.1 arrangement)
    (arrange-speakers :speakers '(-45 0 45 135 -135))
    ;; six channels
    (arrange-speakers :speakers '(-60 0 60 120 180 240))
    ;; seven channels
    (arrange-speakers :speakers '(-45 0 45 100 140 -140 -100))
    ;; eight speakers
    (arrange-speakers :speakers '(-22.5 22.5 67.5 112.5 157.5 202.5 247.5 292.5)))
   ;;
   ;; 3-D speaker configurations
   ;;
   (list
    ;; no channels: impossible
    #f
    ;; mono
    #f
    ;; stereo
    #f
    ;; three channels
    #f
    ;; four channels 3d
    (arrange-speakers :speakers '((-60 0) (60 0) (180 0)
				  (0 90))
		      :groups '((0 1 3) (1 2 3) (2 0 3)
				;; floor
				(0 1 2)))
    ;; five channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (0 90))
		      :groups '((0 1 4) (1 2 4) (2 3 4) (3 0 4)
				;; floor
				(0 1 2) (2 3 0)))
    ;; six channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (-90 60) (90 60))
		      :groups '((0 1 4) (1 4 5) (1 2 5) (2 3 5) (3 4 5) (3 0 4)
				;; floor
				(0 1 2) (2 3 0)))
    ;; seven channels 3d
    (arrange-speakers :speakers '((-45 0) (45 0) (135 0) (-135 0)
				  (-60 60) (60 60) (180 60))
		      :groups '((0 1 4) (1 4 5) (1 2 5) (2 6 5) (2 3 6) (3 4 6) (3 0 4) (4 5 6)
				;; floor
				(0 1 2) (2 3 0)))
    ;; eight speakers 3d
    (arrange-speakers :speakers '((-45 -10) (45 -10) (135 -10) (225 -10)
				  (-45 45) (45 45) (135 45) (225 45))
		      :groups '((0 4 5) (0 5 1) (5 1 2) (2 6 5) (6 7 2) (2 3 7)
				(3 7 4) (3 0 4)
				;; ceiling
				(4 7 6) (6 5 4)
				;; floor
				(0 1 2) (2 3 0))))))


;;; Set a particular speaker configuration

(define* (set-speaker-configuration config (configs dlocsig-speaker-configs))
  "(set-speaker-configuration config (configs dlocsig-speaker-configs)) sets a dlocsig speaker configuration"
  (let ((lst (if (< (speaker-config-dimension config) 3)
		 (car configs)
	       (cadr configs)))
	(num (speaker-config-number config)))
    (list-set! lst num config)))


;;; Get the speaker configuration for a given number of output channels

(define* (get-speaker-configuration channels (3d dlocsig-3d) (configs dlocsig-speaker-configs))
  "(get-speaker-configuration channels (3d dlocsig-3d) (configs dlocsig-speaker-configs)) returns a dlocsig speaker configuration"
  (let* ((config (if 3d (list-ref (cadr configs) channels) (list-ref (car configs) channels))))
    (if (null? config)
	(error 'mus-error "ERROR: no speaker configuration exists for ~A ~A output channel~A~%~%" 
		(if 3d "tridimensional" "bidimensional")
		channels (if (= channels 1) "s" "")))
    config))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dlocsig unit generator
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; global dlocsig parameters

(define dlocsig-path '())
(define dlocsig-scaler 1.0)
(define dlocsig-direct-power 1.5)
(define dlocsig-inside-direct-power 1.5)
(define dlocsig-reverb-power 0.5)
(define dlocsig-inside-reverb-power 0.5)
(define dlocsig-initial-delay #f)
(define dlocsig-unity-gain-distance #f)
(define dlocsig-reverb-amount 0.04)
(define dlocsig-inside-radius 1.0)
(define dlocsig-minimum-segment-length 1.0)

;; render using:

(define amplitude-panning 1)
(define ambisonics 2)
(define decoded-ambisonics 3)
;(define stereo-hrtf 4)

; for backwards compatibility
(define b-format-ambisonics ambisonics)

; a reasonable default

(define dlocsig-render-using amplitude-panning)

;; ambisonics horizontal and vertical order for encoding
;; the default is first order b-format WXYZ

(define dlocsig-ambisonics-h-order 1)
(define dlocsig-ambisonics-v-order 1)

;; globals for ambisonics

(define point707 (cos (/ (* pi 2) 8.0)))
(define dlocsig-ambisonics-scaler point707)
(define dlocsig-ambisonics-ho-rev-scaler 0.05)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get number of channels needed by ambisonics

(define (ambisonics-channels h-order v-order)
  (let* ((count 0))
    (if (>= h-order 0)
	(begin
	  (if (>= h-order 1)
	      ;; W X Y
	      (set! count (+ count 3)))
	  (if (>= v-order 1)
	      ;; Z
	      (set! count (+ count 1)))
	  (if (>= v-order 2)
	      ;; R S T
	      (set! count (+ count 3)))
	  (if (>= h-order 2)
	      ;; U V
	      (set! count (+ count 2)))
	  count)
	;; error: we need at least horizontal order 1!
	0)))

;;;;;;;;;
;;; Paths
;;;;;;;;;

;;; Generic path class

;;; path is a list (type rx ry rz rv rt tx ty tz tt ...)

(define path-rx (make-procedure-with-setter (lambda (p) (list-ref p 1)) (lambda (p val) (list-set! p 1 val))))
(define path-ry (make-procedure-with-setter (lambda (p) (list-ref p 2)) (lambda (p val) (list-set! p 2 val))))
(define path-rz (make-procedure-with-setter (lambda (p) (list-ref p 3)) (lambda (p val) (list-set! p 3 val))))
(define path-rv (make-procedure-with-setter (lambda (p) (list-ref p 4)) (lambda (p val) (list-set! p 4 val))))
(define path-rt (make-procedure-with-setter (lambda (p) (list-ref p 5)) (lambda (p val) (list-set! p 5 val))))
(define path-tx (make-procedure-with-setter (lambda (p) (list-ref p 6)) (lambda (p val) (list-set! p 6 val))))
(define path-ty (make-procedure-with-setter (lambda (p) (list-ref p 7)) (lambda (p val) (list-set! p 7 val))))
(define path-tz (make-procedure-with-setter (lambda (p) (list-ref p 8)) (lambda (p val) (list-set! p 8 val))))
(define path-tt (make-procedure-with-setter (lambda (p) (list-ref p 9)) (lambda (p val) (list-set! p 9 val))))

;(define (make-path) (list 'path '() '() '() '() '() '() '() '() '()))

(define (describe path)
  (cond ((or (eq? (car path) 'bezier-path)
	     (eq? (car path) 'open-bezier-path))
	 (format #f "<bezier-path>:~%  rx: ~A~%  ry: ~A~%  rz: ~A~%  rv: ~A~%  rt: ~A~%  tx: ~A~%  ty: ~A~%  tz: ~A~%  tt: ~A~%  ~
                         x: ~A~%  y: ~A~%  z: ~A~%  v: ~A~%  bx: ~A~%  by: ~A~%  bz: ~A~%  error: ~A~%  curvature: ~A~%"
		 (path-rx path) (path-ry path) (path-rz path) (path-rv path) (path-rt path) (path-tx path) (path-ty path) (path-tz path) (path-tt path)
		 (bezier-x path) (bezier-y path) (bezier-z path) (bezier-v path) (bezier-bx path) (bezier-by path) (bezier-bz path) (bezier-error path) (bezier-curvature path)))
	(else
	 (format #f "<path>:~%  rx: ~A~%  ry: ~A~%  rz: ~A~%  rv: ~A~%  rt: ~A~%  tx: ~A~%  ty: ~A~%  tz: ~A~%  tt: ~A~%"
		 (path-rx path) (path-ry path) (path-rz path) (path-rv path) (path-rt path) (path-tx path) (path-ty path) (path-tz path) (path-tt path)))))


;;; Inquiries into the state of the path

(define (not-rendered path)
  (null? (path-rx path)))

(define (not-transformed path)
  (null? (path-tx path)))

;;; Reset any transformations on the originally rendered path

(define (reset-transformation path)
  (set! (path-tt path) '())
  (set! (path-tx path) '())
  (set! (path-ty path) '())
  (set! (path-tz path) '())
  path)

;;; Reset the rendered path (and any transformations)

(define (reset-rendering path)
  (set! (path-rt path) '())
  (set! (path-rv path) '())
  (set! (path-rx path) '())
  (set! (path-ry path) '())
  (set! (path-rz path) '())
  (reset-transformation path))

;;; Return the best possible set of coordinates

(define (list?? a) 
  "list?? returns a if it is a list"
  (and (listp a) a))

(define (path-x path)
  (or (list?? (path-tx path))
      (list?? (path-rx path))
      (path-rx (render-path path))))

(define (path-y path)
  (or (list?? (path-ty path))
      (list?? (path-ry path))
      (path-ry (render-path path))))

(define (path-z path)
  (or (list?? (path-tz path))
      (list?? (path-rz path))
      (path-rz (render-path path))))

(define (path-time path)
  (or (list?? (path-tt path))
      (list?? (path-rt path))
      (path-rt (render-path path))))


;;;;;;;;;;;;;;;;
;;; Bezier paths
;;;;;;;;;;;;;;;;

;;; Parse a path as two or three-dimensional paths

(define path-3d #f)

;;; Path class for bezier rendered paths

;;; bezier-path is path + path 3d polar x y z v bx by bz error curvature


(define bezier-path      (make-procedure-with-setter (lambda (p) (list-ref p 10)) (lambda (p val) (list-set! p 10 val))))
(define bezier-3d        (make-procedure-with-setter (lambda (p) (list-ref p 11)) (lambda (p val) (list-set! p 11 val))))
(define bezier-polar     (make-procedure-with-setter (lambda (p) (list-ref p 12)) (lambda (p val) (list-set! p 12 val))))
(define bezier-x         (make-procedure-with-setter (lambda (p) (list-ref p 13)) (lambda (p val) (list-set! p 13 val))))
(define bezier-y         (make-procedure-with-setter (lambda (p) (list-ref p 14)) (lambda (p val) (list-set! p 14 val))))
(define bezier-z         (make-procedure-with-setter (lambda (p) (list-ref p 15)) (lambda (p val) (list-set! p 15 val))))
(define bezier-v         (make-procedure-with-setter (lambda (p) (list-ref p 16)) (lambda (p val) (list-set! p 16 val))))
(define bezier-bx        (make-procedure-with-setter (lambda (p) (list-ref p 17)) (lambda (p val) (list-set! p 17 val))))
(define bezier-by        (make-procedure-with-setter (lambda (p) (list-ref p 18)) (lambda (p val) (list-set! p 18 val))))
(define bezier-bz        (make-procedure-with-setter (lambda (p) (list-ref p 19)) (lambda (p val) (list-set! p 19 val))))
(define bezier-error     (make-procedure-with-setter (lambda (p) (list-ref p 20)) (lambda (p val) (list-set! p 20 val))))
(define bezier-curvature (make-procedure-with-setter (lambda (p) (list-ref p 21)) (lambda (p val) (list-set! p 21 val))))

(define* (make-bezier-path (path '()) (3d #t) (polar #f) (error 0.01) (curvature #f))
  (list 'bezier-path '() '() '() '() '() '() '() '() '() path 3d polar '() '() '() '() '() '() '() error curvature))


;;; Path class for open bezier paths

(define initial-direction (make-procedure-with-setter (lambda (p) (list-ref p 22)) (lambda (p val) (list-set! p 22 val))))
(define final-direction   (make-procedure-with-setter (lambda (p) (list-ref p 23)) (lambda (p val) (list-set! p 23 val))))

(define* (make-open-bezier-path (path '()) (3d #t) (polar #f) (error 0.01) (curvature #f) 
				       (initial-direction '(0.0 0.0 0.0)) (final-direction '(0.0 0.0 0.0)))
  (list 'open-bezier-path '() '() '() '() '() '() '() '() '() path 3d polar '() '() '() '() '() '() '() error curvature initial-direction final-direction))



;;;
;;; Generic defining function (for open, closed, polar and cartesian paths)
;;;

(define* (make-path path
		    (3d path-3d)
		    polar
		    closed
		    curvature
		    (error 0.01)
		    ;; only for open paths
		    initial-direction
		    final-direction)
  ;; some sanity checks
  (if (null? path)
      (error 'mus-error "ERROR: Can't define a path with no points in it~%"))
  (if (and closed initial-direction)
      (error 'mus-error "ERROR: Can't specify initial direction ~A for a closed path ~A~%" initial-direction path))
  (if (and closed final-direction)
      (error 'mus-error "ERROR: Can't specify final direction ~A for a closed path ~A~%" final-direction path))

  (if (and closed
	   (not (if (list? (car path))
		    (let* ((start (car path))
			   (end (car (last path))))
		      (and (= (car start) (car end))
			   (= (cadr start) (cadr end))
			   (if path-3d
			       (= (third start) (third end)) #t)))
		  (let* ((end (last path (if path-3d 3 2))))
		    (and (= (car path) (car end))
			 (= (cadr path) (cadr end))
			 (if path-3d
			     (= (third path) (third end)) #t))))))
      (error 'mus-error "ERROR: Closed path ~A is not closed~%" path))

  ;; create the path structure
  (if closed
      (make-bezier-path
		     :path path
		     :3d 3d
		     :polar polar
		     :curvature curvature
		     :error error)
    (make-open-bezier-path
		   :path path
		   :3d 3d
		   :polar polar
		   :curvature curvature
		   :error error
		   :initial-direction initial-direction
		   :final-direction final-direction)))


;;; Some convenient abbreviations

(define* (make-polar-path path
			  (3d path-3d)
			  closed
			  curvature
			  (error 0.01)
			  ;; only for open paths
			  initial-direction
			  final-direction)
  (if closed
      (make-path :path path
		 :3d 3d
		 :polar #t
		 :closed closed
		 :curvature curvature
		 :error error)
    (make-path :path path
	       :3d 3d
	       :polar #t
	       :closed closed
	       :curvature curvature
	       :error error
	       :initial-direction initial-direction
	       :final-direction final-direction)))

(define* (make-closed-path path
			   (3d path-3d)
			   polar
			   curvature
			   (error 0.01))
  (make-path :path path
	     :3d 3d
	     :polar polar
	     :closed #t
	     :curvature curvature
	     :error error))



;;;
;;; Parse a path and transform it into cartesian coordinates
;;;

(define (not-parsed path)
  (null? (bezier-x path)))


;;; Parse a set of 2d or 3d points into the separate coordinates

(define (parse-cartesian-coordinates points 3d)
  "(parse-cartesian-coordinates points 3d) parses a set of 2d or 3d points into the separate coordinates"
  (if (list? (car points))
      ;; decode a list of lists into x:y:z:v components
      ;; 3d -> t [default]
      ;;   '((x0 y0 z0 v0) (x1 y1 z1 v1)...(xn yn zn vn))
      ;;   '((x0 y0 z0) (x1 y1 z1)...(xn yn zn))
      ;;   '((x0 y0) (x1 y1)...(xn yn)) 
      ;;      v: relative velocity
      ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
      ;; 3d -> nil
      ;;   '((x0 y0 v0) (x1 y1 v1)...(xn yn vn))
      ;;   '((x0 y0) (x1 y1)...(xn yn))
      ;;      v: relative velocity
      ;;      x, y, z: coordinates of source [missing z's assumed 0.0]
      (let* ((v '())
	     (x '())
	     (y '())
	     (z '()))
	(for-each
	 (lambda (p)
	   (set! x (cons (car p) x))
	   (set! y (cons (cadr p) y))
	   (set! z (cons (if 3d (or (third p) 0.0) 0.0) z))
	   (set! v (cons (if 3d 
			     (fourth p)
			     (third p))
			 v)))
	 points)
	(list (reverse x) (reverse y) (reverse z) (reverse v)))

    ;; decode a plain list
    (if 3d
	;; it's a three dimensional list
	;; '(x0 y0 z0 x1 y1 z1 ... xn yn zn)
	;;     x, y, z: coordinates of source
	(let ((px '())
	      (py '())
	      (pz '())
	      (len (length points)))
	  (do ((i 0 (+ i 3)))
	      ((>= i len))
	    (set! px (cons (list-ref points i) px))
	    (set! py (cons (list-ref points (+ i 1)) py))
	    (set! pz (cons (list-ref points (+ i 2)) pz)))
	  (list (reverse px) (reverse py) (reverse pz) (make-list-1 (length px) #f)))

      ;; it's a two dimensional list
      ;; '(x0 y0 x1 y1 ... xn yn)
      ;;     x, y, z: coordinates of source [missing z's assumed 0.0]
      (let ((px '())
	    (py '())
	    (len (length points)))
	(do ((i 0 (+ i 2)))
	    ((>= i len))
	  (set! px (cons (list-ref points i) px))
	  (set! py (cons (list-ref points (+ i 1)) py)))
	(list (reverse px) (reverse py) (make-list-1 (length px) 0.0) (make-list-1 (length px) #f))))))

;;; Parse a set of 2d or 3d polar points into the separate coordinates

(define (parse-polar-coordinates points 3d)
  "(parse-polar-coordinates points 3d) parses a polar path"
  (if (list? (car points))
      ;; decode a list of lists of d:a:e:v into x:y:z:v components
      ;; 3d --> t [default]
      ;;   '((d0 a0 e0 v0) (d1 a1 e1 v1)...(dn an en vn))
      ;;   '((d0 a0 e0) (d1 a1 e1)...(dn an en))
      ;;   '((d0 a0) (d1 a1)...(dn an))  
      ;; 3d --> nil
      ;;   '((d0 a0 v0) (d1 a1 v1)...(dn an vn))
      ;;   '((d0 a0) (d1 a1)...(dn an))
      ;;      v: velocity
      ;;      d: distance
      ;;      a: azimut angle
      ;;      e: elevarion angle [missing elevations assumed 0.0]
      (let ((x '())
	    (y '())
	    (z '())
	    (v '()))
	(for-each
	 (lambda (p)
	   (let* ((d (car p))
		  (a (cadr p))
		  (e (if 3d (if (not (null? (cddr p))) (caddr p) 0.0) 0.0))
		  (evec (cis (* (/ e dlocsig-one-turn) 2 pi)))
		  (dxy (* d (real-part evec)))
		  (avec (cis (* (/ a dlocsig-one-turn) 2 pi))))
	     (set! x (cons (* dxy (imag-part avec)) x))
	     (set! y (cons (* dxy (real-part avec)) y))
	     (set! z (cons (* d (imag-part evec)) z))
	     (set! v (cons (if 3d (fourth p) (third p)) v))))
	 points)
	(list (reverse x) (reverse y) (reverse z) (reverse v)))

    ;; decode a list of d:a:e components
    (if 3d
	;; decode a three dimensional list
	;;   '(d0 a0 e0 d1 a1 e1 ... dn an en)
	;;      d: distance
	;;      a: azimut angle
	;;      e: elevarion angle [missing elevations assumed 0.0]
	(let ((x '())
	      (y '())
	      (z '())
	      (len (length points)))
	  (do ((i 0 (+ i 3)))
	      ((>= i len))
	    (let* ((d (list-ref points i))
		   (a (list-ref points (+ i 1)))
		   (e (list-ref points (+ i 2)))
		   (evec (cis (* (/ e dlocsig-one-turn) 2 pi)))
		   (dxy (* d (real-part evec)))
		   (avec (cis (* (/ a dlocsig-one-turn) 2 pi))))
	      (set! x (cons (* dxy (imag-part avec)) x))
	     (set! y (cons (* dxy (real-part avec)) y))
	     (set! z (cons (* d (imag-part evec)) z))))
	  (list (reverse x) (reverse y) (reverse z) (make-list-1 (length x) #f)))

      ;; decode a two dimensional list
      ;;   '(d0 a0 d1 a1 ... dn an)
      ;;      d: distance
      ;;      a: azimut angle
      ;;      e: elevarion angle [missing elevations assumed 0.0]
      (let* ((x '())
	     (y '())
	     (len (length points)))
	(do ((i 0 (+ i 2)))
	    ((>= i len))
	  (let* ((d (list-ref points i))
		 (a (list-ref points (+ i 1)))
		 (avec (cis (* (/ a dlocsig-one-turn) 2 pi))))
	    (set! x (cons (* d (imag-part avec)) x))
	    (set! y (cons (* d (real-part avec)) y))))
	(list (reverse x) (reverse y) (make-list-1 (length x) 0.0) (make-list-1 (length x) #f))))))


(define (xparse-path xpath)
  (let* ((polar (bezier-polar xpath))
	 (points (bezier-path xpath))
	 (3d (bezier-3d xpath)))
    (if polar
	;; parse a polar path
	(let ((vals (parse-polar-coordinates points 3d)))
	  (set! (bezier-x xpath) (car vals))
	  (set! (bezier-y xpath) (cadr vals))
	  (set! (bezier-z xpath) (caddr vals))
	  (set! (bezier-v xpath) (cadddr vals)))
      (let ((vals (parse-cartesian-coordinates points 3d)))
      ;; parse a cartesian path
	(set! (bezier-x xpath) (car vals))
	(set! (bezier-y xpath) (cadr vals))
	(set! (bezier-z xpath) (caddr vals))
	(set! (bezier-v xpath) (cadddr vals)))))
  (for-each
   (lambda (v)
     (if (and (number? v) 
	      (< v 0))
	 (error 'mus-error "ERROR: velocities for path ~A must be all positive~%" (bezier-path xpath))))
   (bezier-v xpath))
  (reset-fit xpath))


;;;
;;; Bezier curve fitting auxiliary functions
;;;

;;; Pythagoras

(define (distance x y z)
  "(distance x y z) returns the euclidean distance of (x y z) from the origin"
  (sqrt (+ (* x x) (* y y) (* z z))))

;;; Nearest point in a line

(define (nearest-point x0 y0 z0 x1 y1 z1 px py pz)

  (define (vmag a b c) 
    (sqrt (+ (* a a) (* b b) (* c c))))

  (define (vcos a0 b0 c0 a1 b1 c1)
    (/ (+ (* a0 a1) (* b0 b1) (* c0 c1))
       (* (vmag a0 b0 c0) (vmag a1 b1 c1))))

  (define (same a0 b0 c0 a1 b1 c1)
    (and (= a0 a1) (= b0 b1) (= c0 c1)))

  (if (same x0 y0 z0 px py pz)
      (list x0 y0 z0)
    (if (same x1 y1 z1 px py pz)
	(list x1 y1 z1)
      (if (same x0 y0 z0 x1 y1 z1)
	  (list x0 y0 z0)
	(let* ((xm0 (- x1 x0))
	       (ym0 (- y1 y0))
	       (zm0 (- z1 z0))
	       (xm1 (- px x0))
	       (ym1 (- py y0))
	       (zm1 (- pz z0))
	       (p (* (vmag xm1 ym1 zm1) (vcos xm0 ym0 zm0 xm1 ym1 zm1)))
	       (l (vmag xm0 ym0 zm0))
	       (ratio (/ p l)))
	  (list (+ x0 (* xm0 ratio))
		(+ y0 (* ym0 ratio))
		(+ z0 (* zm0 ratio))))))))

;;; Bezier curve fitting auxilliary functions

(define path-ak-even #f)
(define path-ak-odd #f)
(define path-maxcoeff 8)
(define path-gtab #f)

(define (make-a-even)

  (define (g m)
    (if (not path-gtab)
	(begin
	 (set! path-gtab (make-vector path-maxcoeff))
	 (vector-set! path-gtab 0 1)
	 (vector-set! path-gtab 1 -4)
	 (do ((i 2 (+ i 1)))
	     ((= i path-maxcoeff))
	   (vector-set! path-gtab i (- (* -4 (vector-ref path-gtab (- i 1)))
				       (vector-ref path-gtab (- i 2)))))))
    (vector-ref path-gtab m))

  (set! path-ak-even (make-vector (- path-maxcoeff 1)))
  (do ((m 1 (+ 1 m)))
      ((= m path-maxcoeff))
    (vector-set! path-ak-even (- m 1) (make-vector m))
    (do ((k 1 (+ 1 k)))
	((> k m))
      (vector-set! (vector-ref path-ak-even (- m 1)) (- k 1) (exact->inexact (/ (- (g (- m k))) (g m)))))))

(define path-ftab #f)

(define (make-a-odd)

  (define (f m)
    (if (not path-ftab)
	(begin
	 (set! path-ftab (make-vector path-maxcoeff))
	 (vector-set! path-ftab 0 1)
	 (vector-set! path-ftab 1 -3)
	 (do ((i 2 (+ i 1)))
	     ((= i path-maxcoeff))
	   (vector-set! path-ftab i (- (* -4 (vector-ref path-ftab (- i 1)))
				       (vector-ref path-ftab (- i 2)))))))
    (vector-ref path-ftab m))

  (set! path-ak-odd (make-vector (- path-maxcoeff 1)))
  (do ((m 1 (+ 1 m)))
      ((= m path-maxcoeff))
    (vector-set! path-ak-odd (- m 1) (make-vector m))
    (do ((k 1 (+ 1 k)))
	((> k m))
      (vector-set! (vector-ref path-ak-odd (- m 1)) (- k 1) (exact->inexact (/ (- (f (- m k))) (f m)))))))

;;; Calculate bezier difference vectors for the given path
;;; (path-x (make-path '((-10 10)(0 5)(10 10))))

(define (calculate-fit path)
  (cond ((not (eq? (car path) 'open-bezier-path))
	 (let* ((n (- (length (bezier-x path )) 1))
		(m (/ (- n (if (odd? n) 3 4)) 2))
		;; data points P(i)
		(p (vector (list->vector (bezier-x path))
			   (list->vector (bezier-y path))
			   (list->vector (bezier-z path))))
		;; control points D(i)
		(d (vector (make-vector n 0.0)
			   (make-vector n 0.0)
			   (make-vector n 0.0))))

	   (define (a-1 k n)
	     (if (odd? (min (+ (* path-maxcoeff 2) 1) n))
		 (begin
		   (if (not path-ak-odd) (make-a-odd))
		   (vector-ref (vector-ref path-ak-odd (/ (- n 3) 2)) (- k 1)))
		 (begin
		   (if (not path-ak-even) (make-a-even))
		   (vector-ref (vector-ref path-ak-even (/ (- n 4) 2)) (- k 1)))))

	   (define (xvector-ref z j i)
	     (if (> i (- n 1))
		 (vector-ref (vector-ref z j) (- i n))
		 (if (< i 0) 
		     (vector-ref (vector-ref z j) (+ i n))
		     (vector-ref (vector-ref z j) i))))

	   (do ((i 0 (+ 1 i)))
	       ((= i n))
	     (do ((k 1 (+ 1 k)))
		 ((> k m))
	       (do ((a 0 (+ 1 a)))
		   ((> a 2))
		 (vector-set! (vector-ref d a) i 
			      (+ (vector-ref (vector-ref d a) i)
				 (* (a-1 k n)
				    (- (xvector-ref p a (+ i k))
				       (xvector-ref p a (- i k)))))))))
	   (if (bezier-curvature path)
	       (do ((i 0 (+ 1 i)))
		   ((= i n))
		 (vector-set! (vector-ref d 0) i (* (vector-ref (vector-ref d 0) i) (bezier-curvature path)))
		 (vector-set! (vector-ref d 1) i (* (vector-ref (vector-ref d 1) i) (bezier-curvature path)))
		 (vector-set! (vector-ref d 2) i (* (vector-ref (vector-ref d 2) i) (bezier-curvature path)))))
	   (list (- n 1) p d)))
	(else
	 (let* ((n (- (length (bezier-x path)) 1))
		(m (- n 1))
		;; data points P(i)
		(p (vector (list->vector (bezier-x path))
			   (list->vector (bezier-y path))
			   (list->vector (bezier-z path))))
		;; control points D(i)
		(d (vector (make-vector (+ n 1) 0.0) 
			   (make-vector (+ n 1) 0.0) 
			   (make-vector (+ n 1) 0.0))))
	   
	   (define (ac k n)
	     (let ((un (min n path-maxcoeff)))
	       (if (not path-ak-even) (make-a-even))
	       (vector-ref (vector-ref path-ak-even (- un 2)) (- k 1))))
	   
	   (define (ref z j i)
	     (if (> i n) 
		 (vector-ref (vector-ref z j) (- i n))
		 (if (< i 0) 
		     (vector-ref (vector-ref z j) (+ i n))
		     (if (= i n) 
			 (- (vector-ref (vector-ref z j) n) 
			    (vector-ref (vector-ref d j) n))
			 (if (= i 0) 
			     (+ (vector-ref (vector-ref z j) 0) 
				(vector-ref (vector-ref d j) 0))
			     (vector-ref (vector-ref z j) i))))))
	   
	   ;; forced initial direction
	   (if (initial-direction path)
	       (begin
		 (vector-set! (vector-ref d 0) 0 (car (initial-direction path)))
		 (vector-set! (vector-ref d 1) 0 (cadr (initial-direction path)))
		 (vector-set! (vector-ref d 2) 0 (third (initial-direction path))))
	       (begin
		 (vector-set! (vector-ref d 0) 0 0.0)
		 (vector-set! (vector-ref d 1) 0 0.0)
		 (vector-set! (vector-ref d 2) 0 0.0)))
	   
	   ;; forced final direction
	   (if (final-direction path)
	       (begin
		 (vector-set! (vector-ref d 0) n (car (final-direction path)))
		 (vector-set! (vector-ref d 1) n (cadr (final-direction path)))
		 (vector-set! (vector-ref d 2) n (caddr (final-direction path))))
	       (begin
		 (vector-set! (vector-ref d 0) n 0.0)
		 (vector-set! (vector-ref d 1) n 0.0)
		 (vector-set! (vector-ref d 2) n 0.0)))
	   
	   ;; calculate fit
	   (do ((i 1 (+ 1 i)))
	       ((= i n))
	     (do ((k 1 (+ 1 k)))
		 ((> k (min m (- path-maxcoeff 1))))
	       (let ((d0 (vector-ref (vector-ref d 0) i))
		     (d1 (vector-ref (vector-ref d 1) i))
		     (d2 (vector-ref (vector-ref d 2) i)))
		 (vector-set! (vector-ref d 0) i (+ d0 
						    (* (ac k n)
						       (- (ref p 0 (+ i k))
							  (ref p 0 (- i k))))))
		 (vector-set! (vector-ref d 1) i (+ d1
						    (* (ac k n)
						       (- (ref p 1 (+ i k))
							  (ref p 1 (- i k))))))
		 (vector-set! (vector-ref d 2) i (+ d2
						    (* (ac k n)
						       (- (ref p 2 (+ i k))
							  (ref p 2 (- i k)))))))))
	   (list n p d)))))

;;; Calculate bezier control points for the given open path

(define (not-fitted path)
  (null? (bezier-bx path)))

(define (reset-fit path)
  (set! (bezier-bx path) '())
  (set! (bezier-by path) '())
  (set! (bezier-bz path) '())
  (reset-rendering path))

(define (fit-path path)
  (cond ((eq? (car path) 'open-bezier-path)
	 (if (not-parsed path)
	     (xparse-path path))
	 
	 (let ((points (length (bezier-x path))))
	   (if (> points 2)
	       (let* ((vals (calculate-fit path))
		      (n (car vals))
		      (p (cadr vals))
		      (d (caddr vals)))
		 (let* ((c (bezier-curvature path))
			(cs (make-vector n)))
		   ;; setup the curvatures array
		   (if (or (not c) (null? c))                          ; no curvature specified, default is 1.0
		       (do ((i 0 (+ 1 i)))
			   ((= i n))
			 (vector-set! cs i (list 1.0 1.0)))
		       (if (number? c)                    ; same curvature for all segments
			   (do ((i 0 (+ 1 i)))
			       ((= i n))
			     (vector-set! cs i (list c c)))
			   (if (and (list? c) (= n (length c)))   ; list of curvatures
			       (let ((i 0))
				 (for-each
				  (lambda (ci)
				    (vector-set! cs i (if (list? ci) 
							  (if (not (= (length ci) 2))
							      (error 'mus-error "ERROR: curvature sublist must have two elements ~A~%" ci)
							      ci)
							  (list ci ci)))
				    (set! i (+ 1 i)))
				  c))
			       (error 'mus-error "ERROR: bad curvature argument ~A to path, need ~A elements~%" c n))))

		   ;; calculate control points
		   (let ((xc '())
			 (yc '())
			 (zc '()))
		     (do ((i 0 (+ 1 i)))
			 ((= i n))
		       
		       (set! xc (cons (list (vector-ref (vector-ref p 0) i)
					    (+ (vector-ref (vector-ref p 0) i) (* (vector-ref (vector-ref d 0) i) (car (vector-ref cs i))))
					    (- (vector-ref (vector-ref p 0) (+ i 1)) (* (vector-ref (vector-ref d 0) (+ i 1)) (cadr (vector-ref cs i))))
					    (vector-ref (vector-ref p 0) (+ i 1))) xc))
		       (set! yc (cons (list (vector-ref (vector-ref p 1) i)
					    (+ (vector-ref (vector-ref p 1) i) (* (vector-ref (vector-ref d 1) i) (car (vector-ref cs i))))
					    (- (vector-ref (vector-ref p 1) (+ i 1)) (* (vector-ref (vector-ref d 1) (+ i 1)) (cadr (vector-ref cs i))))
					    (vector-ref (vector-ref p 1) (+ i 1))) yc))
		       (set! zc (cons (list (vector-ref (vector-ref p 2) i)
					    (+ (vector-ref (vector-ref p 2) i) (* (vector-ref (vector-ref d 2) i) (car (vector-ref cs i))))
					    (- (vector-ref (vector-ref p 2) (+ i 1)) (* (vector-ref (vector-ref d 2) (+ i 1)) (cadr (vector-ref cs i))))
					    (vector-ref (vector-ref p 2) (+ i 1))) zc)))
		     (set! (bezier-bx path) (reverse xc))
		     (set! (bezier-by path) (reverse yc))
		     (set! (bezier-bz path) (reverse zc)))))
	       
	       (if (= points 2)
		   ;; just a line, stays a line
		   (let* ((x1 (car (bezier-x path)))
			  (x2 (cadr (bezier-x path)))
			  (y1 (car (bezier-y path)))
			  (y2 (cadr (bezier-y path)))
			  (z1 (car (bezier-z path)))
			  (z2 (cadr (bezier-z path))))
		     (set! (bezier-bx path) (list (list x1 x1 x2 x2)))
		     (set! (bezier-by path) (list (list y1 y1 y2 y2)))
		     (set! (bezier-bz path) (list (list z1 z1 z2 z2))))
		   (if (= points 1)
		       ;; just one point, bezier won't do much here
		       (begin
			 (set! (bezier-bx path) '())
			 (set! (bezier-by path) '())
			 (set! (bezier-bz path) '())))))
	   (reset-rendering path)))
	
	(else
	 (if (not-parsed path)
	     (xparse-path path))
			 
	 (if (> (length (bezier-x path)) 4)
	     (let* ((vals (calculate-fit path))
		    (n (car vals))
		    (p (cadr vals))
		    (d (caddr vals)))
	       ;; enough points, fit path
	       (let ((xc '())
		     (yc '())
		     (zc '()))
		 (do ((i 0 (+ 1 i)))
		     ((= i n))
		   (set! xc (cons (list (vector-ref (vector-ref p 0) i)
					(+ (vector-ref (vector-ref p 0) i) (vector-ref (vector-ref d 0) i))
					(- (vector-ref (vector-ref p 0) (+ i 1)) (vector-ref (vector-ref d 0) (+ i 1)))
					(vector-ref (vector-ref p 0) (+ i 1))) xc))
		   (set! yc (cons (list (vector-ref (vector-ref p 1) i)
					(+ (vector-ref (vector-ref p 1) i) (vector-ref (vector-ref d 1) i))
					(- (vector-ref (vector-ref p 1) (+ i 1)) (vector-ref (vector-ref d 1) (+ i 1)))
					(vector-ref (vector-ref p 1) (+ i 1))) yc))
		   (set! zc (cons (list (vector-ref (vector-ref p 2) i)
					(+ (vector-ref (vector-ref p 2) i) (vector-ref (vector-ref d 2) i))
					(- (vector-ref (vector-ref p 2) (+ i 1)) (vector-ref (vector-ref d 2) (+ i 1)))
					(vector-ref (vector-ref p 2) (+ i 1))) zc)))
		 (set! (bezier-bx path) (append (reverse xc) (list (list (vector-ref (vector-ref p 0) n)
								  (+ (vector-ref (vector-ref p 0) n) (vector-ref (vector-ref d 0) n))
								  (- (vector-ref (vector-ref p 0) 0) (vector-ref (vector-ref d 0) 0))
								  (vector-ref (vector-ref p 0) 0)))))
		 (set! (bezier-by path) (append (reverse yc) (list (list (vector-ref (vector-ref p 1) n)
								  (+ (vector-ref (vector-ref p 1) n) (vector-ref (vector-ref d 1) n))
								  (- (vector-ref (vector-ref p 1) 0) (vector-ref (vector-ref d 1) 0))
								  (vector-ref (vector-ref p 1) 0)))))
		 (set! (bezier-bz path) (append (reverse zc) (list (list (vector-ref (vector-ref p 2) n)
								  (+ (vector-ref (vector-ref p 2) n) (vector-ref (vector-ref d 2) n))
								  (- (vector-ref (vector-ref p 2) 0) (vector-ref (vector-ref d 2) 0))
								  (vector-ref (vector-ref p 2) 0)))))))
	     
	     ;; not enough points to fit a closed path
	     (let ((xc '())
		   (yc '())
		   (zc '())
		   (len (min (length (bezier-x path)) (length (bezier-y path)) (length (bezier-z path)))))
	       (do ((i 0 (+ 1 i)))
		   ((>= i len))
		 (let ((x1 (list-ref (bezier-x path) i))
		       (x2 (list-ref (bezier-x path) (+ i 1)))
		       (y1 (list-ref (bezier-y path) i))
		       (y2 (list-ref (bezier-y path) (+ i 1)))
		       (z1 (list-ref (bezier-z path) i))
		       (z2 (list-ref (bezier-z path) (+ i 1))))
		   (set! xc (cons (list x1 x1 x2 x2) xc))
		   (set! yc (cons (list y1 y1 y2 y2) yc))
		   (set! zc (cons (list z1 z1 z2 z2) zc))))
	       (warn "[fit-path:closed-path] not enough points to do bezier fit (~A points)" len)
	       (set! (bezier-bx path) (reverse xc))
	       (set! (bezier-by path) (reverse yc))
	       (set! (bezier-bz path) (reverse zc))))
	 (reset-rendering path))))



;;;;;;;;;;;;;;;;;
;;; Literal paths
;;;;;;;;;;;;;;;;;


(define literal-points (make-procedure-with-setter (lambda (p) (list-ref p 10)) (lambda (p val) (list-set! p 10 val))))
(define literal-3d     (make-procedure-with-setter (lambda (p) (list-ref p 11)) (lambda (p val) (list-set! p 11 val))))
(define literal-polar  (make-procedure-with-setter (lambda (p) (list-ref p 12)) (lambda (p val) (list-set! p 12 val))))

;;; Generic literal path creation function
(define* (make-literal-path (points '()) (3d path-3d) polar)
  (list 'literal-path '() '() '() '() '() '() '() '() '() points 3d polar))

;;; Specific polar literal path creation function
(define* (make-literal-polar-path (points '()) (3d path-3d))
  (make-literal-path points 3d #t))


;;;;;;;;;;;
;;; Spirals
;;;;;;;;;;;

(define spiral-start-angle (make-procedure-with-setter (lambda (p) (list-ref p 13)) (lambda (p val) (list-set! p 13 val))))
(define spiral-total-angle (make-procedure-with-setter (lambda (p) (list-ref p 14)) (lambda (p val) (list-set! p 14 val))))
(define spiral-step-angle  (make-procedure-with-setter (lambda (p) (list-ref p 15)) (lambda (p val) (list-set! p 15 val))))
(define spiral-turns       (make-procedure-with-setter (lambda (p) (list-ref p 16)) (lambda (p val) (list-set! p 16 val))))
(define spiral-distance    (make-procedure-with-setter (lambda (p) (list-ref p 17)) (lambda (p val) (list-set! p 17 val))))
(define spiral-height      (make-procedure-with-setter (lambda (p) (list-ref p 18)) (lambda (p val) (list-set! p 18 val))))
(define spiral-velocity    (make-procedure-with-setter (lambda (p) (list-ref p 19)) (lambda (p val) (list-set! p 19 val))))

(define* (make-spiral-path (start-angle 0.0)
			   total-angle
			   step-angle
			   (turns '())
			   (distance '(0 10 1 10))
			   (height '(0 0 1 0))
			   (velocity '(0 1 1 1)))
  (if (and total-angle (not (null? turns)))
      (error 'mus-error "ERROR: can't specify total-angle [~A] and turns [~A] at the same time for the spiral path~%" total-angle turns))
  
  (list 'spiral-path '() '() '() '() '() '() '() '() '() '() path-3d #f 
	start-angle total-angle 
	(or step-angle (/ dlocsig-one-turn 100))
	turns distance height velocity))



;;; Transform a Bezier control point fit to a linear segment approximation

(define (bezier-render path)
  (if (not-fitted path)
      (fit-path path))
  (let ((xrx '()) (xry '()) (xrz '()) (xrv '()))
    
    (define (bezier-point u c)
      ;; Evaluate a point at parameter u in bezier segment
      (let* ((u1 (- 1 u))
	     (cr (vector (make-vector 3 0.0) (make-vector 3 0.0) (make-vector 3 0.0))))
	(do ((j 0 (+ 1 j)))
	    ((= j 3))
	  (vector-set! (vector-ref cr 0) j (+ (* u1 (vector-ref (vector-ref c 0) j)) (* u (vector-ref (vector-ref c 0) (+ j 1)))))
	  (vector-set! (vector-ref cr 1) j (+ (* u1 (vector-ref (vector-ref c 1) j)) (* u (vector-ref (vector-ref c 1) (+ j 1)))))
	  (vector-set! (vector-ref cr 2) j (+ (* u1 (vector-ref (vector-ref c 2) j)) (* u (vector-ref (vector-ref c 2) (+ j 1))))))
	(do ((i 1 (- i 1)))
	    ((< i 0))
	  (do ((j 0 (+ 1 j)))
	      ((> j i))
	    
	    (vector-set! (vector-ref cr 0) j (+ (* u1 (vector-ref (vector-ref cr 0) j)) (* u (vector-ref (vector-ref cr 0) (+ j 1)))))
	    (vector-set! (vector-ref cr 1) j (+ (* u1 (vector-ref (vector-ref cr 1) j)) (* u (vector-ref (vector-ref cr 1) (+ j 1)))))
	    (vector-set! (vector-ref cr 2) j (+ (* u1 (vector-ref (vector-ref cr 2) j)) (* u (vector-ref (vector-ref cr 2) (+ j 1)))))
	    ))
	(list (vector-ref (vector-ref cr 0) 0)
	      (vector-ref (vector-ref cr 1) 0)
	      (vector-ref (vector-ref cr 2) 0))))
    
    (define (berny xl yl zl xh yh zh ul u uh c err)
      ;; Create a linear segment rendering of a bezier segment
      (let* ((vals (bezier-point u c))
	     (x (car vals))
	     (y (cadr vals))
	     (z (caddr vals)))
	(let* ((val1 (nearest-point xl yl zl xh yh zh x y z))
	       (xn (car val1))
	       (yn (cadr val1))
	       (zn (caddr val1)))
	  (if (> (distance (- xn x) (- yn y) (- zn z)) err)
	      (let* ((val2 (berny xl yl zl x y z ul (/ (+ ul u) 2) u c err))
		     (xi (car val2))
		     (yi (cadr val2))
		     (zi (caddr val2)))
		(let* ((val3 (berny x y z xh yh zh u (/ (+ u uh) 2) uh c err))
		       (xj (car val3))
		       (yj (cadr val3))
		       (zj (caddr val3)))
		  (list (append xi (list x) xj)
			(append yi (list y) yj)
			(append zi (list z) zj))))
	      (list '() '() '())))))
    
    ;; Create linear segment approximations of the bezier segments
    ;; make sure there are initial and final velocity values
    (if (not (listp (bezier-v path)))
	(set! (bezier-v path) (list 1 1))
	(if (not (car (bezier-v path)))
	    (begin
	      (list-set! (bezier-v path) 0 1)
	      (list-set! (bezier-v path) (- (length (bezier-v path)) 1) 1))))
    
    ;; only one point means no movement, static source
    (if (= (length (bezier-x path)) 1)
	(begin
	  (set! (path-rx path) (bezier-x path))
	  (set! (path-ry path) (bezier-y path))
	  (set! (path-rz path) (bezier-z path))
	  (set! (path-rt path) (list 0.0))
	  (reset-transformation path)) ; after?
	(begin
	  (let ((len (length (bezier-bx path))))
					;(path-x (make-path '((-10 10)(0 5)(10 10))))
	    ;; render the path only if it has at least two points
	    (do ((i 0 (+ 1 i)))
		((= i len))
	      (let* ((x-bz (list-ref (bezier-bx path) i))
		     (y-bz (list-ref (bezier-by path) i))
		     (z-bz (list-ref (bezier-bz path) i))
		     (vi-bz (list-ref (bezier-v path) i))
		     (vf-bz (list-ref (bezier-v path) (+ i 1)))
		     (xi-bz (car x-bz))
		     (xf-bz (list-ref x-bz (- (length x-bz) 1)))
		     (yi-bz (car y-bz))
		     (yf-bz (list-ref y-bz (- (length y-bz) 1)))
		     (zi-bz (car z-bz))
		     (zf-bz (list-ref z-bz (- (length z-bz) 1))))
		(let* ((vals (berny xi-bz yi-bz zi-bz xf-bz yf-bz zf-bz 0.0 0.5 1.0 
				    (vector (list->vector x-bz)
					    (list->vector y-bz)
					    (list->vector z-bz))
				    (bezier-error path)))
		       (xs (car vals))
		       (ys (cadr vals))
		       (zs (caddr vals)))
		  
		  ;; approximate the bezier curve with linear segments
		  (set! xrx (append xrx (list xi-bz) xs))
		  (set! xry (append xry (list yi-bz) ys))
		  (set! xrz (append xrz (list zi-bz) zs))
		  
		  ;; accumulate intermediate unknown velocities as nils
		  (set! xrv (append xrv (list vi-bz) (make-list-1 (length xs) #f)))
		  (if (= i (- len 1))
		      (begin
			;; add the last point
			(set! xrx (append xrx (list xf-bz)))
			(set! xry (append xry (list yf-bz)))
			(set! xrz (append xrz (list zf-bz)))
			(set! xrv (append xrv (list vf-bz)))
			))))))
	  
	  ;; calculate times for each velocity segment
	  (let ((len (- (length xrx) 1))
		(ti 0)
		(times (list 0))
		(xseg (list (list-ref xrx 0)))
		(yseg (list (list-ref xry 0)))
		(zseg (list (list-ref xrz 0)))
		(vseg (list (list-ref xrv 0)))
		(vi (list-ref xrv 0)))
	    (do ((i 0 (+ 1 i)))
		((= i len))
	      (let* ((x (list-ref xrx (+ i 1)))
		     (y (list-ref xry (+ i 1)))
		     (z (list-ref xrz (+ i 1)))
		     (v (list-ref xrv (+ i 1))))
		(set! xseg (append xseg (list x)))
		(set! yseg (append yseg (list y)))
		(set! zseg (append zseg (list z)))
		(set! vseg (append vseg (list v)))
		
		(if v
		    (let* ((dseg (list))
			   (sum 0.0)
			   (len (- (length xseg) 1)))
		      
		      (do ((i 0 (+ 1 i)))
			  ((= i len))
			(let* ((xsi (list-ref xseg i))
			       (ysi (list-ref yseg i))
			       (zsi (list-ref zseg i))
			       (xsf (list-ref xseg (+ i 1)))
			       (ysf (list-ref yseg (+ i 1)))
			       (zsf (list-ref zseg (+ i 1))))
			  
			  (set! sum (+ sum (distance (- xsf xsi) (- ysf ysi) (- zsf zsi))))
			  (set! dseg (cons sum dseg))))
		      
		      (let* ((df (car dseg)))
			(set! dseg (reverse dseg))
			(let* ((tseg '())
			       (vf v)
			       (a (/ (* (- vf vi) (+ vf vi)) df 4)))
			  (if (= vi 0.0) (set! vi 1))
			  (for-each
			   (lambda (d)
			     (set! tseg (cons (+ ti (if (= vf vi)
							(/ d vi)
							(/ (- (sqrt (+ (* vi vi) (* 4 a d))) vi) (* 2 a))))
					      tseg)))
			   dseg)
			  (set! ti (car tseg))
			  (set! tseg (reverse tseg))
			  
			  (set! times (append times tseg))
			  (set! xseg (list x))
			  (set! yseg (list y))
			  (set! zseg (list z))
			  (set! vseg (list v))
			  (set! vi v)))))
		))
	    
	    (set! (path-rx path) xrx)
	    (set! (path-ry path) xry)
	    (set! (path-rz path) xrz)
	    (set! (path-rt path) 
		  (let* ((tf (list-ref times (- (length times) 1)))
			 (val '()))
		    (for-each
		     (lambda (ti)
		       (set! val (cons (/ ti tf) val)))
		     times)
		    (reverse val)))
	    (reset-transformation path))))))

;; (set! p (make-path '((-10 10 0 0) (0 5 0 1) (10 10 0 0)) :error 0.01))
;; (set! p (make-path '((-10 10 0 1) (-7 7 0 0.9) (0 5 0 0) (7 7 0 0.2) (10 10 0 1)) :error 0.001))
;; (with-sound(:channels 4 :play #f) (sinewave 0 2 880 0.5 :path p))


(define (literal-render path)
  
  ;; Render a user-defined literal path from the data points
  
  ;; decode the points into coordinates
  (let* ((points (literal-points path))
	 (3d (literal-3d path))
	 (polar (literal-polar path)))
    (let ((vals (if polar (parse-polar-coordinates points 3d) (parse-cartesian-coordinates points 3d))))
      (set! (path-rx path) (car vals))
      (set! (path-ry path) (cadr vals))
      (set! (path-rz path) (caddr vals))
      (set! (path-rv path) (cadddr vals)))
    
    ;; make sure there are initial and final velocity values
    (if (not (car (path-rv path)))
	(begin
	  (list-set! (path-rv path) 0 1)
	  (list-set! (path-rv path) (- (length (path-rv path)) 1) 1)))
    
    ;; only one point means no movement, static source
    (if (= (length (path-rx path)) 1)
	(begin
	  (set! (path-rt path) (list 0.0))
	  (reset-transformation path))
	(let* ((rx (path-rx path))
	       (ry (path-ry path))
	       (rz (path-rz path))
	       (rv (path-rv path))
	       (xseg (list (car rx)))
	       (yseg (list (car ry)))
	       (zseg (list (car rz)))
	       (vseg (list (car rv)))
	       (vi (car rv))
	       (len (length rx))
	       (ti 0)
	       (times (list ti)))
	  (do ((i 1 (+ 1 i)))
	      ((= i len))
	    (let ((x (list-ref rx i))
		  (y (list-ref ry i))
		  (z (list-ref rz i))
		  (v (list-ref rv i)))
	      (set! xseg (append xseg (list x)))
	      (set! yseg (append yseg (list y)))
	      (set! zseg (append zseg (list z)))
	      (set! vseg (append vseg (list v)))
	      
	      (if (number? v) ; when v
		  (let* ((sofar 0.0)
			 (dseg '())
			 (len (- (length xseg) 1)))
		    (do ((i 0 (+ 1 i)))
			((= i len))
		      (let* ((xsi (list-ref xseg i))
			     (ysi (list-ref yseg i))
			     (zsi (list-ref zseg i))
			     (xsf (list-ref xseg (+ i 1)))
			     (ysf (list-ref yseg (+ i 1)))
			     (zsf (list-ref zseg (+ i 1))))
			(set! sofar (+ sofar (distance (- xsf xsi) (- ysf ysi) (- zsf zsi))))
			(set! dseg (cons sofar dseg))))
		    (let* ((df (car dseg)))
		      (set! dseg (reverse dseg))
		      (let* ((tseg '())
			     (vf v)
			     (a (/ (* (- vf vi) (+ vf vi)) df 4)))
			(for-each
			 (lambda (d)
			   (set! tseg (cons (+ ti (if (= vf vi)
						      (/ d vi)
						      (/ (- (sqrt (+ (* vi vi) (* 4 a d))) vi) (* 2 a))))
					    tseg)))
			 dseg)
			(set! ti (car tseg))
			(set! tseg (reverse tseg))
			(set! times (append times tseg))
			(set! xseg (list x))
			(set! yseg (list y))
			(set! zseg (list z))
			(set! vseg (list v))
			(set! vi v)))))))
	  
	  (set! (path-rt path) (let ((val '())
				(tf (list-ref times (- (length times) 1))))
			    (for-each
			     (lambda (ti)
			       (set! val (cons (/ ti tf) val)))
			     times)
			    (reverse val)))
	  (reset-transformation path)))))

(define (spiral-render path)
  ;; Render a spiral path from the object data
  
  (let* ((start (* (/ (spiral-start-angle path) dlocsig-one-turn) 2 pi))
	 (total (if (spiral-total-angle path)
		    (* (/ (spiral-total-angle path) dlocsig-one-turn) 2 pi)
		    (if (spiral-turns path)
			(* (spiral-turns path) 2 pi)
			(error 'mus-error "ERROR: a spiral-path needs either a total-angle or turns, none specified~%"))))
	 (steps (abs (/ total (* (/ (spiral-step-angle path) dlocsig-one-turn) 2 pi))))
	 (step (/ total (ceiling steps)
		  (if (< (spiral-step-angle path) 0) -1 1)))
	 (xdistance (x-norm (spiral-distance path) total))
	 (height (x-norm (spiral-height path) total)))
    (let* ((x '())
	   (y '())
	   (z '())
	   (segments (inexact->exact (round (abs (/ total step)))))
	   (len (+ 1 segments)))
      (do ((i 0 (+ 1 i))
	   (angle start (+ angle step)))
	  ((>= i len))
	(let* ((xy (cis angle))
	       (d (envelope-interp angle xdistance)))
	  (set! x (cons (* d (imag-part xy)) x))
	  (set! y (cons (* d (real-part xy)) y))
	  (set! z (cons (envelope-interp angle height) z))))
      
      (set! x (reverse x))
      (set! y (reverse y))
      (set! z (reverse z))
      
      (let* ((dp '())
	     (len (- (length x) 1))
	     (sofar 0.0))
	(do ((i 0 (+ 1 i)))
	    ((>= i len))
	  (let* ((xi (list-ref x i))
		 (xf (list-ref x (+ i 1)))
		 (yi (list-ref y i))
		 (yf (list-ref y (+ i 1)))
		 (zi (list-ref z i))
		 (zf (list-ref z (+ i 1))))
	    (set! sofar (+ sofar (distance (- xf xi) (- yf yi) (- zf zi))))
	    (set! dp (cons sofar dp))))
	(let ()
	  (set! dp (reverse dp))
	  (let* ((tp '())
		 (td 0)
		 (len (- (length dp) 1)))
	    (do ((i 0 (+ 1 i)))
		((>= i len))
	      (let* ((di (list-ref dp i))
		     (df (list-ref dp (+ i 1)))
		     (vp (x-norm (spiral-velocity path) df))
		     (vi (envelope-interp di vp))
		     (vf (envelope-interp df vp)))
		(set! tp (cons td tp))
		(set! td (+ td (/ (- df di) (+ vi vf) 2)))))
	    (let ((tf (car tp)))
	      (set! tp (reverse tp))
	      (set! (path-rx path) x)
	      (set! (path-ry path) y)
	      (set! (path-rz path) z)
	      (let ((val '()))
		(for-each
		 (lambda (ti)
		   (set! val (cons (/ ti tf) val)))
		 tp)
		(set! (path-rt path) (reverse val))))))))
    
    (reset-transformation path)))


(define (render-path path)
  (cond ((or (eq? (car path) 'bezier-path)
	     (eq? (car path) 'open-bezier-path))
	 (bezier-render path))
	((eq? (car path) 'literal-path)
	 (literal-render path))
	(#t (spiral-render path))))




;;;;;;;;;;;;;;;;;;;
;;; Transformations
;;;;;;;;;;;;;;;;;;;

;;; Transform a rendered path using scaling, translation and rotation 

;;; Transform a path (scaling + translation + rotation)

(define* (transform-path path
			 scaling
			 translation
			 rotation
			 rotation-center
			 (rotation-axis '(0.0 0.0 1.0)))
  
  ;; Derive a rotation matrix from an axis vector and an angle

  (define (rotation-matrix x y z angle)
    ;; translated from C routine by David Eberly
    ;; (http://www.magic-software.com/)
    
    (define (normalize a b c)
      (let* ((mag (sqrt (+ (* a a) (* b b) (* c c)))))
	(list (/ a mag) (/ b mag) (/ c mag))))
    
    (let* ((vals (normalize x y z))
	   (dx (car vals))
	   (dy (cadr vals))
	   (dz (caddr vals))
	   (rotate (vector (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0)))
	   (I (vector (vector 1.0 0.0 0.0) (vector 0.0 1.0 0.0) (vector 0.0 0.0 1.0)))
	   (A (vector (vector 0.0 dz (- dy)) (vector (- dz) 0.0 dx) (vector dy (- dx) 0.0)))
	   (AA (vector (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0) (vector 0.0 0.0 0.0)))
	   (sn (sin (- angle)))
	   (omcs (- 1 (cos (- angle)))))
      
      (do ((row 0 (+ 1 row)))
	  ((= row 3))
	(do ((col 0 (+ 1 col)))
	    ((= col 3))
	  (vector-set! (vector-ref AA row) col 0.0)
	  (do ((mid 0 (+ 1 mid)))
	      ((= mid 3))
	    (vector-set! (vector-ref AA row) col
			 (+ (vector-ref (vector-ref AA row) col)
			    (* (vector-ref (vector-ref A row) mid) 
			       (vector-ref (vector-ref A mid) col)))))))
      
      ;; rotation matrix is I+sin(angle)*A+[1-cos(angle)]*A*A 
      (do ((row 0 (+ 1 row)))
	  ((= row 3))
	(do ((col 0 (+ 1 col)))
	    ((= col 3))
	  (vector-set! (vector-ref rotate row) col
		       (+ (vector-ref (vector-ref I row) col)
			  (* sn (vector-ref (vector-ref A row) col))
			  (* omcs (vector-ref (vector-ref AA row) col))))))
      rotate))
  
  
  (if (not-rendered path)
      (render-path path))
  (if (or scaling translation rotation)
      ;; there's at least one transformation to execute
      (let* ((rotation (if rotation (* 2 pi (/ rotation dlocsig-one-turn)) #f))
	     (matrix (if rotation (rotation-matrix (car rotation-axis)
						   (cadr rotation-axis)
						   (third rotation-axis)
						   rotation)
			 #f))
	     (xc (path-x path))
	     (yc (path-y path))
	     (zc (path-z path)))
	(if (and rotation-center (not (= (length rotation-center) 3)))
	    (error 'mus-error "ERROR: rotation center has to have all three coordinates~%"))
	(if (and rotation-axis (not (= (length rotation-axis) 3)))
	    (error 'mus-error "ERROR: rotation axis has to have all three coordinates~%"))
	(let ((len (length xc))
	      (xtr '())
	      (ytr '())
	      (ztr '()))
	  (do ((i 0 (+ 1 i)))
	      ((= i len))
	    (let* ((x (list-ref xc i))
		   (y (list-ref yc i))
		   (z (list-ref zc i))
		   (xw x)
		   (yw y)
		   (zw z))
	      ;; rotating around non-triple zero? translate first
	      (if (and rotation-center rotation)
		  (begin
		    (set! xw (- xw (car rotation-center)))
		    (set! yw (- yw (cadr rotation-center)))
		    (set! zw (- zw (third rotation-center)))))
	      ;; rotation
	      (if rotation
		  (let* ((xr (+ (* (vector-ref (vector-ref matrix 0) 0) xw)
				(* (vector-ref (vector-ref matrix 1) 0) yw)
				(* (vector-ref (vector-ref matrix 2) 0) zw)))
			 (yr (+ (* (vector-ref (vector-ref matrix 0) 1) xw)
				(* (vector-ref (vector-ref matrix 1) 1) yw)
				(* (vector-ref (vector-ref matrix 2) 1) zw)))
			 (zr (+ (* (vector-ref (vector-ref matrix 0) 2) xw)
				(* (vector-ref (vector-ref matrix 1) 2) yw)
				(* (vector-ref (vector-ref matrix 2) 2) zw))))
		    (set! xw xr)
		    (set! yw yr)
		    (set! zw zr)))
	      ;; rotating around non-triple zero? untranslate
	      (if (and rotation-center rotation)
		  (begin
		    (set! xw (+ xw (car rotation-center)))
		    (set! yw (+ yw (cadr rotation-center)))
		    (set! zw (+ zw (third rotation-center)))))
	      ;; scaling
	      (if scaling
		  (begin
		    (set! xw (* xw (car scaling)))
		    (if (cadr scaling)
			(set! yw (* yw (cadr scaling))))
		    (if (third scaling)
			(set! zw (* zw (third scaling))))))
	      ;; translating
	      (if translation
		  (begin
		    (set! xw (+ xw (car translation)))
		    (if (cadr translation)
			(set! yw (+ yw (cadr translation))))
		    (if (third translation)
			(set! zw (+ zw (third translation))))))
	      ;; collect the points
	      (set! xtr (cons xw xtr))
	      (set! ytr (cons yw ytr))
	      (set! ztr (cons zw ztr))))

	  (set! (path-tx path) (reverse xtr))
	  (set! (path-ty path) (reverse ytr))
	  (set! (path-tz path) (reverse ztr))))
      (begin
	;; if there's no transformation just copy the rendered path
	(set! (path-tt path) (copy (path-rt path)))
	(set! (path-tx path) (copy (path-rx path)))
	(set! (path-ty path) (copy (path-ry path)))
	(set! (path-tz path) (copy (path-rz path)))))
  path)

;;; Scale a path

(define (scale-path path scaling)
  (transform-path path :scaling scaling))

;;; Translate a path

(define (translate-path path translation)
  (transform-path path :translation translation))

;;; Rotate a path

(define* (rotate-path path rotation
			:key
			rotation-center
			(rotation-axis '(0.0 0.0 1.0)))
  "rotate-path is a dlocsig function that rotates a dlocsig path"
  (transform-path path 
		  :rotation rotation 
		  :rotation-center rotation-center
		  :rotation-axis rotation-axis))

;;; Mirror a path around an axis

(define* (mirror-path path (axis 'y) (around 0))
  (if (not-transformed path)
      (transform-path path))
  (if (equal axis 'y)
      (let ((val '()))
	(for-each
	 (lambda (x)
	   (set! val (cons (- around x) val)))
	 (path-tx path))
	(set! (path-tx path) (reverse val)))
      (let ((val '()))
	(for-each
	 (lambda (y)
	   (set! val (cons (- around y) val)))
	 (path-ty path))
	(set! (path-ty path) (reverse val))))
  path)

;;; Change the times of the rendered envelope so that the velocity is constant

(define (constant-velocity path)
  "constant-velocity is a dlocsig function that changes the times of the rendered envelope so that the velocity is constant"
  (if (not (path-rx path))
      (render-path path))
  (reset-transformation path)
  (let* ((xcoords (path-x path))
	 (ycoords (path-y path))
	 (zcoords (path-z path))
	 (tcoords (path-time path))
	 (total-distance 
	  (let* ((sum 0.0)
		 (len (length xcoords)))
	    (do ((i 0 (+ 1 i)))
		((= i len))
	      (let ((x1 (list-ref xcoords i))
		    (x2 (list-ref xcoords (+ i 1)))
		    (y1 (list-ref ycoords i))
		    (y2 (list-ref ycoords (+ i 1)))
		    (z1 (list-ref zcoords i))
		    (z2 (list-ref zcoords (+ i 1))))
		(set! sum (+ sum (distance (- x2 x1) (- y2 y1) (- z2 z1))))))
	    sum))
	 (start-time (car tcoords))
	 (end-time (list-ref tcoords (- (length tcoords) 1)))
	 (total-time (- end-time start-time))
	 (velocity (/ total-distance total-time)))
    (let ((len (length xcoords))
	  (now '())
	  (dist 0.0))
      (do ((i 0 (+ 1 i)))
	  ((= i len))
	(let* ((xp (list-ref xcoords i))
	       (x (list-ref xcoords (+ i 1)))
	       (yp (list-ref ycoords i))
	       (y (list-ref ycoords (+ i 1)))
	       (zp (list-ref zcoords i))
	       (z (list-ref zcoords (+ i 1))))
	  (set! dist (+ dist (distance (- x xp) (- y yp) (- z zp))))
	  (set! now (cons (/ dist velocity) now))))
      (set! now (reverse now))
      (set! (path-rt path) (append (list start-time) now))
      (set! (path-tx path) (copy (path-rx path)))
      (set! (path-ty path) (copy (path-ry path)))
      (set! (path-tz path) (copy (path-rz path)))))
  path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a new dlocsig structure

(define* (make-dlocsig start-time
		       duration
		       (path dlocsig-path)
		       (scaler dlocsig-scaler)
		       (direct-power dlocsig-direct-power)
		       (inside-direct-power dlocsig-inside-direct-power)
		       (reverb-power dlocsig-reverb-power)
		       (inside-reverb-power dlocsig-inside-reverb-power)
		       (reverb-amount dlocsig-reverb-amount)
		       (initial-delay dlocsig-initial-delay)
		       (unity-gain-dist dlocsig-unity-gain-distance)
		       (inside-radius dlocsig-inside-radius)
		       (minimum-segment-length dlocsig-minimum-segment-length)
		       (render-using dlocsig-render-using)
		       (ambisonics-h-order dlocsig-ambisonics-h-order)
		       (ambisonics-v-order dlocsig-ambisonics-v-order)
		       out-channels
		       rev-channels)

  (if (null? start-time)
      (error 'mus-error "ERROR: a start time is required in make-dlocsig~%"))
  (if (null? duration)
      (error 'mus-error "ERROR: a duration has to be specified in make-dlocsig~%"))

  ;; check to see if we have the right number of channels for b-format ambisonics
  (if (= render-using ambisonics)
      (begin
	(if (or (> ambisonics-h-order 2)
		(> ambisonics-v-order 2))
	    (error 'mus-error "ERROR: ambisonics encoding is currently limited to second order components~%"))
	(let* ((channels (ambisonics-channels ambisonics-h-order ambisonics-v-order)))
	  (if (< (or out-channels (mus-channels *output*)) channels)
	      (error 'mus-error "ERROR: ambisonics number of channels is wrong, dlocsig needs ~A output channels for h:~A, v:~A order (current number is ~A)~%"
		      channels ambisonics-h-order ambisonics-v-order (or out-channels (mus-channels *output*)))))))

  (if (not out-channels)
      (if *output*
	  (set! out-channels (channels *output*))
	  (begin
	    (format #t "WARNING: no *output*?  Will set out-channels to 2~%~%")
	    (set! out-channels 2))))
  (if (not rev-channels)
      (set! rev-channels (if *reverb* (channels *reverb*) 0)))

  (let* (;; speaker configuration for current number of channels
	 (speakers (if (= render-using ambisonics)
		       #f
		       (get-speaker-configuration out-channels)))

	 ;; array of gains -- envelopes
	 (channel-gains (make-vector out-channels '()))
	 (channel-rev-gains (make-vector out-channels '()))

	 ;; speaker output delays
	 (max-out-delay 0.0)
	 (out-delays (make-vector out-channels))

	 ;; coordinates of rendered path
	 (xpoints (path-x path))
	 (ypoints (path-y path))
	 (zpoints (path-z path))
	 (tpoints (path-time path))

	 ;; speed of sound expressed in terms of path time coordinates
	 (speed-limit (/ (* dlocsig-speed-of-sound 
			    (- (car (last tpoints)) (car tpoints)))
			 duration))
	 (start 0)
	 ;(end 0)
	 (delay '())
	 (doppler '())
	 (real-dur 0)
	 (prev-time #f)
	 (prev-dist #f)
	 (prev-group #f)
	 (prev-x #f)
	 (prev-y #f)
	 (prev-z #f)
	 (first-dist #f)
	 (last-dist #f)
	 (min-dist #f)
	 (max-dist #f)
	 (min-delay #f)
	 ;; without this the delay apparently stomps over something else in the structure
	 ;; and we get artifacts in the output, probably a off-by-one error somewhere
	 (delay-hack 1)
	 (min-dist-unity #f)
	 (unity-gain 1.0)
	 (unity-rev-gain 1.0)
	 (run-beg #f)
	 (run-end #f)
	 ;; channel offsets in output stream for ambisonics
	 ;; (depends on horizontal and vertical order, default is h=1,v=1)
	 (w-offset 0)
	 (x-offset 1)
	 (y-offset 2)
	 (z-offset #f)
	 (r-offset #f)
	 (s-offset #f)
	 (t-offset #f)
	 (u-offset #f)
	 (v-offset #f))

    (if (= render-using ambisonics)
	;; calculate output channel offsets for ambisonics rendering
	(let* ((offset 3))
	  ;; the default is at least a horizontal order of 1
	  (if (>= ambisonics-v-order 1)
	      (begin
		;; add Z
		(set! z-offset offset)
		(set! offset (+ offset 1))))
	  (if (>= ambisonics-v-order 2)
	      (begin
		;; add R S T
		(set! r-offset offset)
		(set! s-offset (+ offset 1))
		(set! t-offset (+ offset 2))
		(set! offset (+ offset 3))))
	  (if (>= ambisonics-h-order 2)
	      (begin
		;; add U V
		(set! u-offset offset)
		(set! v-offset (+ offset 1))
		(set! offset (+ offset 2))))))

    (define (equalp-intersection l1 l2)
      (if (null? l2) 
	  l2
	  (let loop1 ((l1 l1) 
		      (result '()))
	    (cond ((null? l1) 
		   (reverse! result))
		  ((member (car l1) l2) 
		   (loop1 (cdr l1) 
			  (cons (car l1) 
				result)))
		  (else (loop1 (cdr l1) 
			       result))))))

    (define (dist->samples d) (inexact->exact (round (* d (/ (mus-srate) dlocsig-speed-of-sound)))))
    (define (dist->seconds d) (/ d dlocsig-speed-of-sound))
    (define (time->samples time) (inexact->exact (round (* time (mus-srate)))))

    (define (transition-point-3 vert-a vert-b xa ya za xb yb zb) 
      (define (cross v1 v2)
	(list (- (* (cadr v1) (third v2))
		 (* (third v1) (cadr v2)))
	      (- (* (third v1) (car v2))
		 (* (car v1) (third v2)))
	      (- (* (car v1) (cadr v2))
		 (* (cadr v1) (car v2)))))
      (define (dot v1 v2)
	(+ (* (car v1) (car v2))
	    (* (cadr v1) (cadr v2))
	    (* (third v1) (third v2))))
      (define (sub v1 v2)
	(list (- (car v1) (car v2))
	      (- (cadr v1) (cadr v2))
	      (- (third v1) (third v2))))
      (define (add v1 v2)
	(list (+ (car v1) (car v2))
	       (+ (cadr v1) (cadr v2))
	       (+ (third v1) (third v2))))
      (define (scale v1 c)
	(list (* (car v1) c)
	       (* (cadr v1) c)
	       (* (third v1) c)))

      (let* ((tolerance 1.0e-6)
	     (line-b (list xa ya za))
	     (line-m (sub (list xb yb zb) line-b))
	     (normal (cross vert-a vert-b))
	     (denominator (dot normal line-m)))
	(if (<= (abs denominator) tolerance)
	    #f
	    (add line-b (scale line-m (/ (- (dot normal line-b)) denominator))))))

    ;; calculate transition point between two adjacent two-speaker groups
    ;; original line intersection code from Graphic Gems III
    (define (transition-point-2 vert xa ya xb yb)
      (let* ((Ax (car vert))
	     (Bx (- xa xb))
	     (Ay (cadr vert))
	     (By (- ya yb))
	     (Cx (- xa))
	     (Cy (- ya))
	     (d (- (* By Cx) (* Bx Cy)))
	     (f (- (* Ay Bx) (* Ax By))))
	(if (= f 0)
	    #f
	    (list (/ (* d Ax) f)
		  (/ (* d Ay) f)))))

    ;; calculate speaker gains for group
    (define (calculate-gains x y z group)
      (let* ((zero-coord 1.0e-10)
	     (zero-gain 1.0e-10)
	     (size (group-size group))
	     (mat (group-matrix group))) ; returns mixer
	(if (and (< (abs x) zero-coord)
		 (< (abs y) zero-coord)
		 (< (abs z) zero-coord))
	    (list #t (list 1.0 1.0 1.0))

	    (if (= size 3)
		(let* ((gain-a (+ (* (mixer-ref mat 0 0) x)
				  (* (mixer-ref mat 1 0) y)
				  (* (mixer-ref mat 2 0) z)))
		       (gain-b (+ (* (mixer-ref mat 0 1) x)
				  (* (mixer-ref mat 1 1) y)
				  (* (mixer-ref mat 2 1) z)))
		       (gain-c (+ (* (mixer-ref mat 0 2) x)
				  (* (mixer-ref mat 1 2) y)
				  (* (mixer-ref mat 2 2) z)))
		       (mag (sqrt (+ (* gain-a gain-a)
				     (* gain-b gain-b)
				     (* gain-c gain-c)))))
		  ;; truncate to zero roundoff errors
		  (if (< (abs gain-a) zero-gain)
		      (set! gain-a 0.0))
		  (if (< (abs gain-b) zero-gain)
		      (set! gain-b 0.0))
		  (if (< (abs gain-c) zero-gain)
		      (set! gain-c 0.0))
		  (list (and (>= gain-a 0) (>= gain-b 0) (>= gain-c 0))
			(list (/ gain-a mag) (/ gain-b mag) (/ gain-c mag))))

		(if (= size 2)
		    (let* ((gain-a (+ (* (mixer-ref mat 0 0) x)
				      (* (mixer-ref mat 1 0) y)))
			   (gain-b (+ (* (mixer-ref mat 0 1) x)
				      (* (mixer-ref mat 1 1) y)))
			   (mag (sqrt (+ (* gain-a gain-a)
					 (* gain-b gain-b)))))
		      ;; truncate to zero roundoff errors
		      (if (< (abs gain-a) zero-gain)
			  (set! gain-a 0.0))
		      (if (< (abs gain-b) zero-gain)
			  (set! gain-b 0.0))
		      (list (and (>= gain-a 0) (>= gain-b 0))
			    (list (/ gain-a mag) (/ gain-b mag))))

		    (if (= size 1)
			(list #t (list 1.0))))))))

    ;; find the speaker group that contains a point
    (define (find-group x y z)
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (group)
	    (let* ((vals (calculate-gains x y z group))
		   (inside (car vals))
		   (gains (cadr vals)))
	      (if inside
		  (return (list group gains)))))
	  (speaker-config-groups speakers))
	 (list #f #f))))

    ;; push zero gains on all channels
    (define (push-zero-gains time)
      (let ((len (speaker-config-number speakers)))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (vector-set! channel-gains i (cons time (vector-ref channel-gains i)))
	  (vector-set! channel-gains i (cons 0.0 (vector-ref channel-gains i)))))
      (let ((len rev-channels))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (vector-set! channel-rev-gains i (cons time (vector-ref channel-rev-gains i)))
	  (vector-set! channel-rev-gains i (cons 0.0 (vector-ref channel-rev-gains i))))))

    (define (position val lst)
      (define (position-1 val lst pos)
	(call-with-exit
	 (lambda (return)
	   (if (null? lst)
	       #f
	       (if (= val (car lst))
		   (return pos)
		   (position-1 val (cdr lst) (+ 1 pos)))))))
      (position-1 val lst 0))

    ;; push gain and time into envelopes
    (define (push-gains group gains dist time num)
      (let* ((outputs (make-vector out-channels 0.0))
	     (rev-outputs (make-vector rev-channels 0.0))
	     ;; attenuation with distance of direct signal
	     (att (if (>= dist inside-radius)
		      (/ (expt dist direct-power))
		      (- 1.0 (expt (/ dist inside-radius) (/ inside-direct-power)))))
	     ;; attenuation with distance of reverberated signal
	     (ratt (if (>= dist inside-radius)
		       (/ (expt dist reverb-power))
		       (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))))
	(if (>= dist inside-radius)
	    ;; outside the inner sphere, signal is sent to group
	    (let ((len (length gains)))
	      (do ((i 0 (+ 1 i)))
		  ((= i len))
		(let ((speaker (list-ref (group-speakers group) i))
		      (gain (list-ref gains i)))
		  (vector-set! outputs speaker (* gain att))
		  (if (and (> rev-channels 1)
			   (< speaker (length rev-outputs)))
		      (vector-set! rev-outputs speaker (* gain ratt))))))

	    (let ((gain 0.0)
		  (len (speaker-config-number speakers)))
	      (do ((speaker 0 (+ 1 speaker)))
		  ((= speaker len))
		;; inside the inner sphere, signal is sent to all speakers
		(let ((found (position speaker (group-speakers group))))
		  (if found
		      ;; speaker belongs to group, add to existing gain
		      (begin
			(set! gain (list-ref gains found))
			(vector-set! outputs speaker (+ gain (* (- 1.0 gain) att)))
			(if (> rev-channels 1) (vector-set! rev-outputs speaker (+ gain (* (- 1.0 gain) ratt)))))
		      ;; speaker outside of group
		      (begin
			(vector-set! outputs speaker att)
			(if (> rev-channels 1) (vector-set! rev-outputs speaker ratt))))))))

	;; push all channel gains into envelopes
	(let ((len (speaker-config-number speakers)))
	  (do ((i 0 (+ 1 i)))
	      ((= i len))
	    (if (or (null? (vector-ref channel-gains i))
		    (> time (cadr (vector-ref channel-gains i))))
		(begin
		  (vector-set! channel-gains i (cons time (vector-ref channel-gains i)))
		  (vector-set! channel-gains i (cons (vector-ref outputs i) (vector-ref channel-gains i)))))))

	(if (> rev-channels 1)
	    (do ((i 0 (+ 1 i)))
		((= i rev-channels))
	      (if (or (null? (vector-ref channel-rev-gains i))
		      (> time (cadr (vector-ref channel-rev-gains i))))
		  (begin
		    (vector-set! channel-rev-gains i (cons time (vector-ref channel-rev-gains i)))
		    (vector-set! channel-rev-gains i (cons (vector-ref rev-outputs i) (vector-ref channel-rev-gains i)))))))

	;; push reverb gain into envelope for mono reverb
	(if (= rev-channels 1)
	    (begin
	      (if (or (null? (vector-ref channel-rev-gains 0))
		      (> time (cadr (vector-ref channel-rev-gains 0))))
		  (begin
		    (vector-set! channel-rev-gains 0 (cons time (vector-ref channel-rev-gains 0)))
		    (vector-set! channel-rev-gains 0 (cons ratt (vector-ref channel-rev-gains 0)))))))))

    ;; Render a trajectory breakpoint through amplitude panning
    (define (famplitude-panning x y z dist time q)
      ;; output gains for current point
      (if prev-group
	  (let* ((vals (calculate-gains x y z prev-group))
		 (inside (car vals))
		 (gains (cadr vals)))
	    ;; check that the source is not moving faster than sound
	    (if (not (= time prev-time))
		(let* ((speed (/ (- dist prev-dist) (- time prev-time))))
		  (if (> speed speed-limit)
		      (format #t "WARNING: supersonic radial movement at [~F,~F,~F, ~F], speed=~F~%~%" x y z time speed))))
	    (if inside
		;; still in the same group
		(begin
		  (push-gains prev-group gains dist time 1)
		  (set! prev-x x)
		  (set! prev-y y)
		  (set! prev-z z))
		;; left the group
		(let* ((vals (find-group x y z))
		       (group (car vals))
		       (gains (cadr vals)))
		  (if group
		      ;; we have to interpolate a new point that lies on the shared
		      ;; edge of the adjacent groups so that the speakers opposite
		      ;; the edge have zero gain when the trajectory switches groups
		      (let* ((edge (equalp-intersection (group-vertices group)
							(group-vertices prev-group))))
			(if (= (length edge) 2)
			    ;; the groups have two shared points (ie: share an edge)
			    ;; this must be a three speaker groups transition
			    (let* ((pint (transition-point-3 (car edge) (cadr edge) x y z prev-x prev-y prev-z)))
			      (if pint
				  (let* ((xi (car pint))
					 (yi (cadr pint))
					 (zi (third pint))
					 (di (distance xi yi zi))
					 (ti (+ prev-time (max .00001 (* (/ (distance (- xi prev-x)
										      (- yi prev-y)
										      (- zi prev-z))
									    (distance (- x prev-x)
										      (- y prev-y)
										      (- z prev-z)))
									 (- time prev-time))))))
				    ;; see if we are inside the previous group
				    ;; we can be on either side due to roundoff errors
				    (let* ((vals (calculate-gains xi yi zi prev-group))
					   (inside (car vals))
					   (gains (cadr vals)))
				      (if inside
					  (push-gains prev-group gains di ti 2)
					  (let* ((val1 (calculate-gains xi yi zi group))
						 (inside (car val1))
						 (gains (cadr val1)))
					    (if inside
						(push-gains group gains di ti 3)
						;; how did we get here?
						(error 'mus-error "ERROR: Outside of both adjacent groups [~A:~A:~A @~A]~%~%" xi yi zi ti))))))))

			    (if (and (= (length edge) 1) (= (group-size group) 2))
				;; two two-speaker groups share one point
				;; z coordinates are silently ignored
				(let* ((pint (transition-point-2 (car edge) x y prev-x prev-y)))
				  (if pint
				      (let* ((xi (car pint))
					     (yi (cadr pint))
					     (di (distance xi yi 0.0))
					     (ti (+ prev-time (max .00001 (* (/ (distance (- xi prev-x)
											  (- yi prev-y)
											  0.0)
										(distance (- x prev-x)
											  (- y prev-y)
											  0.0))
									     (- time prev-time))))))
					;; see if we are inside the previous group
					;; we can be on either side due to roundoff errors
					(let* ((vals (calculate-gains xi yi 0.0 prev-group))
					       (inside (car vals))
					       (gains (cadr vals)))
					  (if inside 
					      (push-gains prev-group gains di ti 4)
					      (let* ((val1 (calculate-gains xi yi 0.0 group))
						     (inside (car val1))
						     (gains (cadr val1)))
						(if inside
						    (push-gains group gains di ti 5)
						    ;; how did we get here?
						    (format #t "Outside of both adjacent groups [~A:~A @~A]~%~%" xi yi ti))))))))
				(if (= (length edge) 1)
				    ;; groups share only one point... for now a warning
				    ;; we should calculate two additional interpolated
				    ;; points as the trajectory must be crossing a third
				    ;; group
				      (begin
					(for-each
					 (lambda (int-group)
					   (if (and (member (car edge) (group-vertices int-group))
						    (not (equal? int-group group))
						    (not (equal? int-group prev-group)))
					       (let* ((edge1 (equal-intersection (group-vertices int-group)
										 (group-vertices prev-group)))
						      (edge2 (equal-intersection (group-vertices int-group)
										 (group-vertices group))))
						 (format #t "e1=~A; e2=~A~%~%" edge1 edge2))))
					 (speaker-config-groups speakers))
					(format #t "WARNING: crossing between groups with only one point in common~%  prev=~A~%  curr=~A~%~%" prev-group group))

				      ;; groups don't share points... how did we get here?
				      (if (= (length edge) 0)
					  (format #t "WARNING: crossing between groups with no common points, ~A~A to ~A~A~%~%"
						  (group-id prev-group) (group-speakers prev-group)
						  (group-id group) (group-speakers group))))))

			;; finally push gains for current group
			(push-gains group gains dist time 6)
			(set! prev-group group)
			(set! prev-x x)
			(set! prev-y y)
			(set! prev-z z))
		      ;; current point is outside all defined groups
		      ;; we should send a warning at this point...
		      (begin
			(push-zero-gains time)
			(set! prev-group #f))))))
	  ;; first time around
	  (let* ((vals (find-group x y z))
		 (group (car vals))
		 (gains (cadr vals)))
	    (if group
		(begin
		  (push-gains group gains dist time 7)
		  (set! prev-group group)
		  (set! prev-x x)
		  (set! prev-y y)
		  (set! prev-z z))
		(begin
		  (push-zero-gains time)
		  (set! prev-group #f))))))

    ;; Render a trajectory breakpoint for ambisonics b-format coding
    ;; http://www.york.ac.uk/inst/mustech/3d_audio/ambis2.htm
    ;;
    ;; Ambisonics b-format has four discrete channels encoded as follows:
    ;; W     0.707107             0.707107
    ;; X     cos(A)cos(E)         x
    ;; Y     sin(A)cos(E)         y
    ;; R     1.5sin(E)sin(E)-0.5  1.5zz-0.5
    ;; S     cos(A)sin(2E)        2zx
    ;; T     sin(A)sin(2E)        2yz
    ;; U     cos(2A)cos(E)cos(E)  xx-yy
    ;; V     sin(2A)cos(E)cos(E)  2xy
    ;;
    ;; where:
    ;; A: counter-clockwise angle of rotation from the front center
    ;; E: the angle of elevation above the horizontal plane
    ;; 
    ;; in our coordinate system (normalizing the vectors):
    ;; xy: (* dist (cos E))
    ;; (cos A): (/ y xy)
    ;; (sin A): (/ -x xy)
    ;; (cos E): (/ xy dist)
    ;; (sin E): (/ z dist)
    ;; so:
    ;; W: (* signal 0.707)
    ;; X: (* signal (/ y dist))
    ;; Y: (* signal (/ -x dist))
    ;; Z: (* signal (/ z dist))
    ;;
    ;; R: (* signal (- (* 1.5 z z 1/dist 1/dist) 0.5))
    ;; S: (* signal 2 z (- x) 1/dist 1/dist)
    ;; T: (* signal 2 z y 1/dist 1/dist)
    ;; U: (* signal (- (* x x 1/dist 1/dist) (* y y 1/dist 1/dist)))
    ;; V: (* signal 2 (- x) y 1/dist 1/dist)
    ;;
    ;; see also: http://wiki.xiph.org/index.php/Ambisonics
    ;; for mixed order systems
    ;;
    (define (render-ambisonics x y z dist time)
      (let* ((att (if (> dist inside-radius)
		      (expt (/ inside-radius dist) direct-power)
		      (expt (/ dist inside-radius) (/ inside-direct-power))))
	     (attW (if (> dist inside-radius)
		       (* point707 att)
		       (- 1 (* (- 1 point707) (expt (/ dist inside-radius) direct-power)))))
	     (ratt (if (> dist inside-radius)
		       (expt (/ inside-radius dist) reverb-power)
		       (expt (/ dist inside-radius) (/ inside-reverb-power))))
	     (rattW (if (> dist inside-radius)
			(* point707 ratt)
			(- 1 (* (- 1 point707) (expt (/ dist inside-radius) reverb-power))))))
	;; output encoding gains for point
	;; W: 0.707
	(vector-set! channel-gains w-offset (cons time (vector-ref channel-gains w-offset)))
	(vector-set! channel-gains w-offset (cons attW (vector-ref channel-gains w-offset)))
	;; X: (* (cos A) (cos E))
	(vector-set! channel-gains x-offset (cons time (vector-ref channel-gains x-offset)))
	(vector-set! channel-gains x-offset (cons (* (if (zero? dist) 0 (/ y dist)) att) (vector-ref channel-gains x-offset)))
	;; Y: (* (sin A) (cos E))
	(vector-set! channel-gains y-offset (cons time (vector-ref channel-gains y-offset)))
	(vector-set! channel-gains y-offset (cons (* (if (zero? dist) 0 (/ (- x) dist)) att) (vector-ref channel-gains y-offset)))
	(if (>= ambisonics-v-order 1)
	    (begin
	      ;; Z: (sin E)
	      (vector-set! channel-gains z-offset (cons time (vector-ref channel-gains z-offset)))
	      (vector-set! channel-gains z-offset (cons (* (if (zero? dist) 0 (/ z dist)) att) (vector-ref channel-gains z-offset)))))
	(if (>= ambisonics-v-order 2)
	    (begin
	      ;; R
	      (vector-set! channel-gains r-offset (cons time (vector-ref channel-gains r-offset)))
	      (vector-set! channel-gains r-offset (cons (* (if (zero? dist) 0 (- (* 1.5 z z (if (zerop dist) 1 (/ 1 (* dist dist)))) 0.5)) att)
							   (vector-ref channel-gains r-offset)))
	      ;; S
	      (vector-set! channel-gains s-offset (cons time (vector-ref channel-gains s-offset)))
	      (vector-set! channel-gains s-offset (cons (* (if (zero? dist) 0 2) z (- x) (if (zero? dist) 1 (/ 1 (* dist dist))) att)
							(vector-ref channel-gains s-offset)))
	      ;; T
	      (vector-set! channel-gains t-offset (cons time (vector-ref channel-gains t-offset)))
	      (vector-set! channel-gains t-offset (cons (* (if (zero? dist) 0 2) z y (if (zero? dist) 1 (/ 1 (* dist dist))) att)
							(vector-ref channel-gains t-offset)))))
	(if (>= ambisonics-h-order 2)
	    (begin
	      ;; U
	      (vector-set! channel-gains u-offset (cons time (vector-ref channel-gains u-offset)))
	      (vector-set! channel-gains u-offset (cons (* (if (zero? dist) 0 1) (- (* x x (if (zero? dist) 1 (/ 1 (* dist dist))))
										    (* y y (if (zero? dist) 1 (/ 1 (* dist dist))))) att)
							(vector-ref channel-gains u-offset)))
	      ;; V
	      (vector-set! channel-gains v-offset (cons time (vector-ref channel-gains v-offset)))
	      (vector-set! channel-gains v-offset (cons (* (if (zero? dist) 0 2) (- x) y (if (zero? dist) 1 (/ 1 (* dist dist))) att)
							(vector-ref channel-gains v-offset)))))
	;; push reverb gain into envelope
	(if (= rev-channels 1)
	    (begin
	      ;; mono reverb output
	      (vector-set! channel-rev-gains 0 (cons time (vector-ref channel-rev-gains 0)))
	      (vector-set! channel-rev-gains 0 (cons (if (>= dist inside-radius)
							 (/ (expt dist reverb-power))
							 (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))
						     (vector-ref channel-rev-gains 0)))))
	(if (> rev-channels 1)
	    (let ((ho-ratt dlocsig-ambisonics-ho-rev-scaler))
	      ;; multichannel reverb, send ambisonics components
	      ;; W: 0.707
	      (vector-set! channel-rev-gains w-offset (cons time (vector-ref channel-rev-gains w-offset)))
	      (vector-set! channel-rev-gains w-offset (cons rattW (vector-ref channel-rev-gains w-offset)))
	      ;; X: (* (cos A)(cos E))
	      (vector-set! channel-rev-gains x-offset (cons time (vector-ref channel-rev-gains x-offset)))
	      (vector-set! channel-rev-gains x-offset (cons (* (if (zero? dist) 0 1) y (if (zerop dist) 1 (/ dist)) ratt)(vector-ref channel-rev-gains x-offset)))
	      ;; Y: (* (sin A)(cos E))
	      (vector-set! channel-rev-gains y-offset (cons time (vector-ref channel-rev-gains y-offset)))
	      (vector-set! channel-rev-gains y-offset (cons (* (if (zero? dist) 0 1) (- x) (if (zerop dist) 1 (/ dist)) ratt)
							    (vector-ref channel-rev-gains y-offset)))
	      (if (>= ambisonics-v-order 1)
		  (begin
		    ;; Z: (sin E)
		    (vector-set! channel-rev-gains z-offset (cons time (vector-ref channel-rev-gains z-offset)))
		    (vector-set! channel-rev-gains z-offset (cons (* (if (zero? dist) 0 1) z (if (zerop dist) 1 (/ dist)) ratt)
								  (vector-ref channel-rev-gains z-offset)))))
	      (if (>= ambisonics-v-order 2)
		  (begin
		    ;; R
		    (vector-set! channel-rev-gains r-offset (cons time (vector-ref channel-rev-gains r-offset)))
		    (vector-set! channel-rev-gains r-offset (cons (* (if (zero? dist) 0 (- (* 1.5 z z (if (zero? dist) 1 (/ 1 (* dist dist)))) 0.5)) ho-ratt ratt)
								  (vector-ref channel-rev-gains r-offset)))
		    ;; S
		    (vector-set! channel-rev-gains s-offset (cons time (vector-ref channel-rev-gains s-offset)))
		    (vector-set! channel-rev-gains s-offset (cons (* (if (zero? dist) 0 2) z (- x) (if (zero? dist) 1 (/ 1 (* dist dist))) ho-ratt ratt)
								  (vector-ref channel-rev-gains s-offset)))
		    ;; T
		    (vector-set! channel-rev-gains t-offset (cons time (vector-ref channel-rev-gains t-offset)))
		    (vector-set! channel-rev-gains t-offset (cons (* (if (zero? dist) 0 2) z y (if (zero? dist) 1 (/ 1 (* dist dist))) ho-ratt ratt)
								  (vector-ref channel-rev-gains t-offset)))))
	      (if (>= ambisonics-h-order 2)
		  (begin
		    ;; U
		    (vector-set! channel-rev-gains u-offset (cons time (vector-ref channel-rev-gains u-offset)))
		    (vector-set! channel-rev-gains u-offset (cons (* (if (zero? dist) 0 (- (* x x (if (zero? dist) 1 (/ 1 (* dist dist))))
											   (* y y (if (zero? dist) 1 (/ 1 (* dist dist)))))) ho-ratt ratt)
								  (vector-ref channel-rev-gains u-offset)))
		    ;; V
		    (vector-set! channel-rev-gains v-offset (cons time (vector-ref channel-rev-gains v-offset)))
		    (vector-set! channel-rev-gains v-offset (cons (* (if (zero? dist) 0 2) (- x) y (if (zero? dist) 1 (/ 1 (* dist dist))) ho-ratt ratt)
								  (vector-ref channel-rev-gains v-offset)))))))))

    ;; Render a trajectory breakpoint to a room for decoded ambisonics
    ;;
    ;; for a given speaker located in 3d space in polar coordinates:
    ;; az: azimut angle, increments clockwise
    ;; el: elevation angle
    ;;
    ;; S: (+ W (* X (cos az) (cos el))
    ;;         (* Y (sin az) (cos el))
    ;;         (* Z (sin el)))
    ;; 
    (define (fdecoded-ambisonics x y z dist time)
      (let* ((att (if (> dist inside-radius)
		      (expt (/ inside-radius dist) direct-power)
		      (expt (/ dist inside-radius) (/ inside-direct-power))))
	     (attW (if (> dist inside-radius)
		       (* point707 att)
		       (- 1 (* (- 1 point707) (expt (/ dist inside-radius) direct-power)))))
	     (ratt (if (> dist inside-radius)
		       (expt (/ inside-radius dist) reverb-power)
		       (expt (/ dist inside-radius) (/ inside-reverb-power))))
	     (rattW (if (> dist inside-radius)
			(* point707 ratt)
			(- 1 (* (- 1 point707) (expt (/ dist inside-radius) reverb-power))))))
	;; output decoded gains for point
	(let ((len (speaker-config-number speakers))
	      (spkrs (speaker-config-coords speakers)))
	  (do ((i 0 (+ 1 i)))
	      ((= i len))
	    (let* ((s (list-ref spkrs i))
		   (signal (* dlocsig-ambisonics-scaler
			      (+ 
			       ;; W
			       (* attW point707)
			       ;; (* X (cos az) (cos el))
			       (* att (if (= dist 0) 0 (/ y dist)) (cadr s))
			       ;; (* Y (sin az) (cos el))
			       (* att (if (= dist 0) 0 (/ x dist)) (car s))
			       ;; (* Z (sin el)
			       (* att (if (= dist 0) 0 (/ z dist)) (third s))))))
	      (vector-set! channel-gains i (cons time (vector-ref channel-gains i)))
	      (vector-set! channel-gains i (cons signal (vector-ref channel-gains i))))))

	;; push reverb gain into envelope
	(if (= rev-channels 1)
	    (begin
	      ;; mono reverberation
	      (vector-set! channel-rev-gains 0 (cons time (vector-ref channel-rev-gains 0)))
	      (vector-set! channel-rev-gains 0 (cons (if (>= dist inside-radius)
							 (/ (expt dist reverb-power))
							 (- 1.0 (expt (/ dist inside-radius) (/ inside-reverb-power))))
						     (vector-ref channel-rev-gains 0))))
	    ;; multichannel reverb
	    (do ((i 0 (+ 1 i)))
		((= i rev-channels))
	      (let* ((s (list-ref (speaker-config-coords speakers) i))
		     (signal (* dlocsig-ambisonics-scaler
				(+ 
				 ;; W
				 (* rattW point707)
				 ;; (* X (cos az) (cos el))
				 (* ratt (if (zero? dist) 0 (/ y dist)) (cadr s))
				 ;; (* Y (sin az) (cos el))
				 (* ratt (if (zero? dist) 0 (/ x dist)) (car s))
				 ;; (* Z (sin el)
				 (* ratt (if (zero? dist) 0 (/ z dist)) (third s))))))
		(vector-set! channel-rev-gains i (cons time (vector-ref channel-rev-gains i)))
		(vector-set! channel-rev-gains i (cons signal (vector-ref channel-rev-gains i))))))))

    ;; Loop through all virtual rooms for one breakpoint in the trajectory
    (define (walk-all-rooms x y z time num)
      (let ((room 0)
	    (dist (distance x y z)))
	;; remember first and last distances
	(if (not first-dist) ; set to #f (far) above
	    (set! first-dist dist))
	(set! last-dist dist)
	;; remember maximum and minimum distances
	(if (or (not min-dist) (< dist min-dist))
	    (set! min-dist dist))
	(if (or (not max-dist) (> dist max-dist))
	    (set! max-dist dist))
	;; push delay for current point (for doppler)
	(if (or (null? delay)
		(> time (cadr delay)))
	    (begin
	      (set! delay (cons time delay))
	      (set! delay (cons (dist->samples dist) delay))
	      ;; doppler should be easy, yeah right. We use "relativistic" correction
	      ;; as the sound object can be travelling close to the speed of sound. 
	      ;; http://www.mathpages.com/rr/s2-04/2-04.htm, 
	      ;; va = 0 (stationary listener)
	      ;; ve = moving object
	      ;; va = (* ve (/ 1 (+ 1 (/ ve c))) (sqrt (- 1 (* (/ ve c) (/ ve c)))))
	      (if prev-time
		  (let* ((ratio (/ (- dist prev-dist)
				   (* duration (- time prev-time) dlocsig-speed-of-sound))))
		    (set! doppler (cons (/ (+ prev-time time) 2) doppler))
		    (set! doppler (cons (* (/ 1 (+ 1 ratio)) (sqrt (- 1 (* ratio ratio)))) doppler))))))
	;; do the rendering of the point
	(if (= render-using amplitude-panning)
	    ;; amplitude panning
	    (famplitude-panning x y z dist time 1)
	    (if (= render-using ambisonics)
		;; ambisonics b format
		(render-ambisonics x y z dist time)
		(if (= render-using decoded-ambisonics)
		    ;; ambisonics decoded
		    (fdecoded-ambisonics x y z dist time))))

	(set! room (+ 1 room))
	;; remember current time and distance for next point
	(set! prev-time time)
	(set! prev-dist dist)
	;; return number of rooms processed
	room))

    ;; Check to see if a segment changes radial direction:
    ;;   a change in radial direction implies a change in 
    ;;   doppler shift that has to be reflected as a new
    ;;   point in the rendered envelopes
    (define (change-direction xa ya za ta xb yb zb tb num)
      (walk-all-rooms xa ya za ta 1)
      (if (or (not (= xa xb))
	      (not (= ya yb))
	      (not (= za zb))
	      (not (= ta tb)))
	  (let* ((vals (nearest-point xa ya za xb yb zb 0 0 0))
		 (xi (car vals))
		 (yi (cadr vals))
		 (zi (caddr vals)))
	    (if (and (if (< xa xb) (<= xa xi xb) (<= xb xi xa))
		     (if (< ya yb) (<= ya yi yb) (<= yb yi ya))
		     (if (< za zb) (<= za zi zb) (<= zb zi za)))
		(walk-all-rooms xi yi zi
				(+ tb (* (- ta tb)
					 (/ (distance (- xb xi) (- yb yi) (- zb zi))
					    (distance (- xb xa) (- yb ya) (- zb za)))))
				2
				)))))

    ;; Check to see if a segment intersects the inner sphere:
    ;;   points inside are rendered differently so we need to
    ;;   create additional envelope points in the boundaries
    (define (intersects-inside-radius xa ya za ta xb yb zb tb)
      (let* ((mag (distance (- xb xa) (- yb ya) (- zb za)))
	     (vx (/ (- xb xa) mag))
	     (vy (/ (- yb ya) mag))
	     (vz (/ (- zb za) mag))
	     (bsq (+ (* xa vx) (* ya vy) (* za vz)))
	     (u (- (+ (* xa xa) (* ya ya) (* za za))
		   (* inside-radius inside-radius)))
	     (disc (- (* bsq bsq) u))
	     (hit (>= disc 0.0)))
	(if hit
	    ;; ray defined by two points hits sphere
	    (let* ((root (sqrt disc))
		   (rin  (- (- bsq) root))
		   (rout (+ (- bsq) root))
		   (xi #f) (yi #f) (zi #f) (ti #f) (xo #f) (yo #f) (zo #f) (to #f))
	      (if (and (> rin 0) (< rin mag))
		  ;; intersects entering sphere
		  (begin
		    (set! xi (+ xa (* vx rin)))
		    (set! yi (+ ya (* vy rin)))
		    (set! zi (+ za (* vz rin)))
		    (set! ti (+ tb (* (- ta tb)
				      (/ (distance (- xb xi) (- yb yi) (- zb zi))
					 (distance (- xb xa) (- yb ya) (- zb za))))))))
	      (if (and (> rout 0) (< (abs rout) mag))
		  ;; intersects leaving sphere
		  (begin
		    (set! xo (+ xa (* vx rout)))
		    (set! yo (+ ya (* vy rout)))
		    (set! zo (+ za (* vz rout)))
		    (set! to (+ tb (* (- ta tb)
				      (/ (distance (- xb xo) (- yb yo) (- zb zo))
					 (distance (- xb xa) (- yb ya) (- zb za))))))))
	      (if xi
		  (begin
		    (change-direction xa ya za ta xi yi zi ti 1)
		    (if xo
			(begin
			  (change-direction xi yi zi ti xo yo zo to 2)
			  (change-direction xo yo zo to xb yb zb tb 3))
			(change-direction xi yi zi ti xb yb zb tb 4)))
		  (if xo
		      (begin
			(change-direction xa ya za ta xo yo zo to 5)
			(change-direction xo yo zo to xb yb zb tb 6))
		      (change-direction xa ya za ta xb yb zb tb 7))))
	    (change-direction xa ya za ta xb yb zb tb 8))))

    ;; Recursively split segment if longer than minimum rendering distance:
    ;;   otherwise long line segments that have changes in distance render 
    ;;   the amplitude envelope as a linear function that does not reflect
    ;;   the chosen power function (1/d^n)
    (define (fminimum-segment-length xa ya za ta xb yb zb tb)
      (let* ((dist (distance (- xb xa) (- yb ya) (- zb za))))
	(if (< dist minimum-segment-length)
	    (intersects-inside-radius xa ya za ta xb yb zb tb)
	    ;; interpolate a new point half way thorugh the segment
	    (let* ((xi (/ (+ xa xb) 2))
		   (yi (/ (+ ya yb) 2))
		   (zi (/ (+ za zb) 2))
		   (ti (+ tb (* (- ta tb)
				(/ (distance (- xb xi) (- yb yi) (- zb zi))
				   (distance (- xb xa) (- yb ya) (- zb za)))))))
	      (fminimum-segment-length xa ya za ta xi yi zi ti)
	      (fminimum-segment-length xi yi zi ti xb yb zb tb)))))


    ;; Loop for each pair of points in the position envelope and render them
    (if (= (length xpoints) 1)
	;; static source (we should check if this is inside the inner radius?)
	(walk-all-rooms (car xpoints) (car ypoints) (car zpoints) (car tpoints) 3)

	;; moving source
	(let ((len (- (min (length xpoints) (length ypoints) (length zpoints) (length tpoints)) 1)))
	  (do ((i 0 (+ 1 i)))
	      ((>= i len))
	    (let* ((xa (list-ref xpoints i))
		   (ya (list-ref ypoints i))
		   (za (list-ref zpoints i))
		   (ta (list-ref tpoints i))
		   (xb (list-ref xpoints (+ i 1)))
		   (yb (list-ref ypoints (+ i 1)))
		   (zb (list-ref zpoints (+ i 1)))
		   (tb (list-ref tpoints (+ i 1))))
	      (fminimum-segment-length xa ya za ta xb yb zb tb)
	      (if (= i len)
		  (walk-all-rooms xb yb zb tb 4))))))

    ;; returns the new duration of a sound after using an envelope for time-varying sampling-rate conversion
    ;; (from Bill's dsp.scm)
    (define (src-duration e)
      (let* ((len (length e))
	     (ex0 (car e))
	     (ex1 (list-ref e (- len 2)))
	     (all-x (- ex1 ex0))
	     (dur 0.0))
	(do ((i 0 (+ i 2)))
	    ((>= i (- len 2)) dur)
	  (let* ((x0 (list-ref e i))
		 (x1 (list-ref e (+ i 2)))
		 (y0 (list-ref e (+ i 1))) ; 1/x x points
		 (y1 (list-ref e (+ i 3)))
		 (area (if (< (abs (- y0 y1)) .0001)
			   (/ (- x1 x0) (* y0 all-x))
			   (* (/ (- (log y1) (log y0)) 
				 (- y1 y0)) 
			      (/ (- x1 x0) all-x)))))
	    (set! dur (+ dur (abs area)))))))

    ;; create delay lines for output channels that need them
    (if speakers
	(let* ((delays (speaker-config-delays speakers))
	       (len (length delays)))
	  (do ((channel 0 (+ 1 channel)))
	      ((= channel len))
	    (let ((delayo (vct-ref delays channel)))
	      (vector-set! out-delays channel (if (not (= delayo 0.0))
						  (make-delay (time->samples delayo))
						  #f))
	      (set! max-out-delay (max max-out-delay delayo))))))

    ;; delay from the minimum distance to the listener
    (set! min-delay (dist->samples min-dist))
    ;; duration of sound at listener's position after doppler src
    ;;
    ;; this does not work quite right but the error leads to a longer
    ;; run with zeroed samples at the end so it should be fine
    (set! real-dur (* duration (src-duration (reverse doppler)))) 
    ;; end of the run according to the duration of the note
    ;; (set! end (time->samples duration))
    ;; start and end of the run loop in samples
    (set! run-beg (time->samples start-time))
    (set! run-end (inexact->exact (floor (- (+ (time->samples (+ start-time (max duration real-dur)))
					       (dist->samples last-dist)
					       (time->samples max-out-delay))
					    (if initial-delay 0.0 min-delay)))))
    ;; sample at which signal first arrives to the listener
    (set! start (+ run-beg (dist->samples (- first-dist (if initial-delay 0.0 min-dist)))))
    ;; minimum distance for unity gain calculation
    (set! min-dist-unity (if (< min-dist inside-radius)
			     inside-radius 
			     min-dist))
    ;; unity-gain gain scalers
    (set! unity-gain (* scaler
			(if (number? unity-gain-dist)
			    (expt unity-gain-dist direct-power)
			    (if (not unity-gain-dist)
				(expt min-dist-unity direct-power)
				1.0))))
    (set! unity-rev-gain (* scaler
			    (if (number? unity-gain-dist)
				(expt unity-gain-dist reverb-power)
				(if (not unity-gain-dist) ; defaults to #f above
				    (expt min-dist-unity reverb-power)
				    1.0))))

    (list 
     (make-move-sound
      (list
       ;; :start 
       start
       ;; :end 
       (time->samples (+ start-time (max duration real-dur)))
       ;; :out-channels 
       (if speakers (speaker-config-number speakers) out-channels)
       ;; :rev-channels 
       rev-channels
       ;; :path 
       (make-delay delay-hack :max-size (max 1 (+ (ceiling (dist->samples max-dist)) delay-hack)))
       ;; :delay 
       (make-env (reverse delay)
		 :offset (if initial-delay 0.0 (- min-delay))
		 :duration real-dur)
       ;; :rev 
       (make-env (if (number? reverb-amount) ; as opposed to an envelope I guess
		     (list 0 reverb-amount 1 reverb-amount)
		     reverb-amount)
		 :duration real-dur)
       ;; :out-delays 
       out-delays
       ;; :gains 
       (let ((v (make-vector out-channels)))
	 (do ((i 0 (+ 1 i)))
	     ((= i out-channels))
	   (vector-set! v i (make-env (reverse (vector-ref channel-gains i))
				      :scaler (if (= render-using ambisonics) 1.0 unity-gain)
				      :duration real-dur)))
	 v)
       ;; :rev-gains 
       (if (> rev-channels 0)
	   (let ((v (make-vector rev-channels)))
	     (do ((i 0 (+ 1 i)))
		 ((= i rev-channels))
	       (vector-set! v i (make-env (reverse (vector-ref channel-rev-gains i))
					  :scaler (if (= render-using ambisonics) 1.0 unity-rev-gain)
					  :duration real-dur)))
	     v)
	   #f)
       ;; :out-map 
       (if speakers 
	   (speaker-config-map speakers) 
	   (let ((v (make-vector out-channels)))
	     (do ((i 0 (+ i 1)))
		 ((= i out-channels))
	       (vector-set! v i i))
	     v)))
      *output*
      *reverb*)
     ;; return start and end samples for the run loop
     run-beg
     run-end)))

;(with-sound(:channels 6 :play #f :statistics #t) (sinewave 0 10 440 0.5 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :error 0.001)))
;
;(with-sound(:statistics #t :channels 4 :reverb-channels 4 :reverb freeverb :decay-time 3)
;  (move 0 "/usr/ccrma/snd/nando/sounds/kitchen/bowl/small-medium-large-1.snd"
;	:paths (list (make-spiral-path :start-angle 0 :turns 2.5)
;		     (make-spiral-path :start-angle 180 :turns 3.5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run macro to localize samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dlocsig a b c) 
  "(dlocsig a b c) is the same as move-sound"
  (move-sound a b c)) ; use this form for run's benefit

#|

;(define hi (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f :error 0.001))
;(make-dlocsig 0 1.0 :out-channels 2 :rev-channels 0 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(define* (sinewave start-time duration freq amp 
		   (amp-env '(0 1 1 1))
		   (path (make-path :path '(-10 10 0 5 10 10))))
  (let* ((vals (make-dlocsig :start-time start-time
			     :duration duration
			     :path path))
	 (dloc (car vals))
	 (beg (cadr vals))
	 (end (caddr vals)))
    (let* ((osc (make-oscil :frequency freq))
	   (aenv (make-env :envelope amp-env :scaler amp :duration duration)))
      (run
       (do ((i beg (+ 1 i)))
	   ((= i end))
	 (dlocsig dloc i (* (env aenv) (oscil osc))))))))

(with-sound (:channels 2) (sinewave 0 1.0 440 .5 :path (make-path '((-10 10) (0.5 0.5) (10 10)) :3d #f)))

|#


