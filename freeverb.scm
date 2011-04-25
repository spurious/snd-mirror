;;; freeverb.scm -- CLM -> Snd/Scheme translation of freeverb.ins

;; Translator/Author: Michael Scholz <scholz-micha@gmx.de>
;; Last: Thu Apr 24 01:32:15 CEST 2003
;; Version: $Revision: 1.2 $

;;; Original notes of Fernando Lopez-Lezcano

;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
;;
;; Written by Jezar at Dreampoint, June 2000
;; http://www.dreampoint.co.uk
;;
;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
;; Version 1.0 for clm-2 released in January 2001
;; http://ccrma.stanford.edu/~nando/clm/freeverb/
;;
;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
;; - the clm version can now work with a mono input or an n-channel input
;;   stream (in the latter case the number of channels of the input and output
;;   streams must match.
;; - the "wet" parameter has been eliminated as it does not apply to the model
;;   that clm uses to generate reverberation
;; - the "width" parameter name has been changed to :global. It now controls the
;;   coefficients of an NxN matrix that specifies how the output of the reverbs
;;   is mixed into the output stream.
;; - predelays for the input channels have been added.
;; - damping can be controlled individually for each channel.

;; For more information see clm-2/freeverb/index.html [MS]

;;; changed to accommodate run and mono output, bill 11-Jun-06
;;;            use the filtered-comb gen, bill 29-Jun-06

;;; Code:

(provide 'snd-freeverb.scm)
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(definstrument (freeverb
		   (room-decay 0.5)
		   (damping 0.5)
		   (global 0.3)
		   (predelay 0.03)
		   (output-gain 1.0)
		   (output-mixer #f)
		   (scale-room-decay 0.28)
		   (offset-room-decay 0.7)
		   (combtuning '(1116 1188 1277 1356 1422 1491 1557 1617))
		   (allpasstuning '(556 441 341 225))
		   (scale-damping 0.4)
		   (stereo-spread 23)
		   (verbose #f))
  (let* ((startime 0.0)
	 (dur (+ 1.0 (mus-sound-duration (mus-file-name *reverb*))))
	 (beg (seconds->samples startime))
	 (end (cadr (times->samples startime dur)))
	 (out-chans (channels *output*))
	 (out-mix (if (mixer? output-mixer) output-mixer
		      (make-mixer out-chans 0.0)))
	 (out-buf (make-frame out-chans 0.0))
	 (out-gain output-gain)
	 (f-out (make-frame out-chans 0.0))
	 (in-chans (channels *reverb*))
	 (f-in (make-frame in-chans 0.0))
	 (predelays (make-vector in-chans))
	 (local-gain (if (= out-chans 1)
			 global
			 (+ (/ (- 1.0 global) (- 1 (/ 1.0 out-chans)))
			    (/ 1.0 out-chans))))
	 (global-gain (if (= out-chans 1)
			  local-gain
			  (/ (- out-chans (* local-gain out-chans))
			     (- (* out-chans out-chans) out-chans))))
	 (srate-scale (/ *clm-srate* 44100.0))
	 (room-decay-val (+ (* room-decay scale-room-decay)
			    offset-room-decay))
	 (numcombs (length combtuning))
	 (numallpasses (length allpasstuning))
	 (fcombs (make-vector (* out-chans numcombs)))
	 (allpasses (make-vector (* out-chans numallpasses))))
    (if verbose
	(format #t ";;; freeverb: ~d input channels, ~d output channels~%" in-chans out-chans))
    (if (and (> in-chans 1)
	     (not (= in-chans out-chans)))
	(error "input must be mono or input channels must equal output channels"))
    (if (not (mixer? output-mixer))
	(if (vector? output-mixer)
	    (do ((i 0 (+ 1 i)))
		((= i out-chans))
	      (do ((j 0 (+ 1 j)))
		  ((= j out-chans))
		(set! (out-mix i j) (vector-ref output-mixer (+ (* i out-chans) j)))))
	    (do ((i 0 (+ 1 i)))
		((= i out-chans))
	      (do ((j 0 (+ 1 j)))
		  ((= j out-chans))
		(set! (out-mix i j)
		      (/ (* out-gain (if (= i j)
					 local-gain
					 global-gain))
			 out-chans))))))
    (do ((c 0 (+ 1 c)))
	((= c in-chans))
      (vector-set! predelays
		   c
		   (make-delay :size (round (* *clm-srate*
					       (if (vector? predelay)
						   (vector-ref predelay c)
						   (if (list? predelay)
						       (list-ref predelay c)
						       predelay)))))))
    (do ((c 0 (+ 1 c)))
	((= c out-chans))
      (do ((i 0 (+ 1 i)))
	  ((= i numcombs))
	(let* ((tuning (list-ref combtuning i))
	       (len (floor (* srate-scale tuning)))
	       (dmp (* scale-damping
		       (if (vector? damping)
			   (vector-ref damping i)
			   (if (list? damping)
			       (list-ref damping i)
			       damping)))))
	  (if (odd? c)
	      (set! len (+ len (floor (* srate-scale stereo-spread)))))
	  (vector-set! fcombs (+ (* c numcombs) i)
		       (make-filtered-comb :size len 
					   :scaler room-decay-val 
					   :filter (make-one-zero :a0 (- 1.0 dmp) :a1 dmp))))))
    (do ((c 0 (+ 1 c)))
	((= c out-chans))
      (do ((i 0 (+ 1 i)))
	  ((= i numallpasses))
	(let* ((tuning (list-ref allpasstuning i))
	       (len (floor (* srate-scale tuning))))
	  (if (odd? c)
	      (set! len (+ len (floor (* srate-scale stereo-spread)))))
	  (vector-set! allpasses (+ (* c numallpasses) i)
		       (make-all-pass :size len :feedforward -1 :feedback 0.5)))))
    (run
     (do ((i beg (+ 1 i)))
	 ((= i end))
       (declare (predelays clm-vector) (allpasses clm-vector) (fcombs clm-vector))
       (file->frame *reverb* i f-in)
       (if (> in-chans 1)
	   (do ((c 0 (+ 1 c)))
	       ((= c out-chans))
	     (frame-set! f-in c (delay (vector-ref predelays c) (f-in c)))
	     (frame-set! f-out c 0.0)
	     (do ((j 0 (+ 1 j)))
		 ((= j numcombs))
	       (let ((ctr (+ (* c numcombs) j)))
		 (frame-set! f-out c (+ (f-out c)
					(filtered-comb (vector-ref fcombs ctr) (f-in 0)))))))
	   (begin
	     (frame-set! f-in 0 (delay (vector-ref predelays 0) (f-in 0)))
	     (do ((c 0 (+ 1 c)))
		 ((= c out-chans))
	       (frame-set! f-out c 0.0)
	       (do ((j 0 (+ 1 j)))
		   ((= j numcombs))
		 (let ((ctr (+ (* c numcombs) j)))
		   (frame-set! f-out c (+ (f-out c)
					  (filtered-comb (vector-ref fcombs ctr) (f-in 0)))))))))
       (do ((c 0 (+ 1 c)))
	   ((= c out-chans))
	 (do ((j 0 (+ 1 j)))
	     ((= j numallpasses))
	   (frame-set! f-out c (all-pass (vector-ref allpasses (+ (* c numallpasses) j))
					 (f-out c)))))
       (frame->file *output* i (frame->frame f-out out-mix out-buf))))))

;; freeverb.scm ends here
