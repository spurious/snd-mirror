;;; libSMS translations: 
;;;    tools/smsPrint
;;;    tools/smsSynth


;;; -------------------------------- smsPrint --------------------------------

(define* (smsPrint file (choice 1) (beg 0.0) (end #f) (firsttrack 0) (lasttrack -1))
  ;; tools/smsPrint.c
  ;;   choice: 1=all, 2=deterministic, 3=stochastic, 4=header
  ;;   (smsPrint "/home/bil/test/libsms-1.02/examples/ocarina.sms")

  "(smsPrint filename (choice 1) begin-time end-time firsttrack lasttrack) prints out the contents of an SMS analysis file (*.sms)"
  
  (let ((vals (sms_getHeader file))) ; vals = '(err hdr fp)
    (if (not (= (car vals) SMS_OK))
	(format #t "error in sms_getHeader: ~S~%" (sms_errorString (car vals)))

	(let* ((smsHeader (cadr vals))
	       (smsFile (caddr vals))
	       (smsData (sms_make_SMS_Data))
	       (err (sms_allocFrameH smsHeader smsData)))
	  (if (not (= err SMS_OK))
	      (format #t "error in sms_allocFrameH: ~S~%" (sms_errorString err))

	      (let ((PRINT_ALL 1)
		    (PRINT_DET 2)
		    (PRINT_STOC 3)
		    (PRINT_HDR 4))
		(format #t "~%HEADER INFORMATION:~%")
		(format #t "Number of frames = ~D~%" (.nFrames smsHeader))
		(format #t "Frame rate (Hz) = ~A~%" (.iFrameRate smsHeader))
		(format #t "Number of tracks = ~A~%" (.nTracks smsHeader))
		(format #t "Number of stochastic coefficients = ~D~%" (.nStochasticCoeff smsHeader))
		(let ((f (.iFormat smsHeader)))
		  (format #t "Format = ~A~%" (if (= f SMS_FORMAT_H) "harmonic"
						 (if (= f SMS_FORMAT_IH) "inharmonic"
						     (if (= f SMS_FORMAT_HP) "harmonic with phase"
							 "inharmonic with phase")))))
		(let ((t (.iStochasticType smsHeader)))
		  (format #t "Stochastic type = ~A~%" (if (= t SMS_STOC_IFFT) "IFFT"
							  (if (= t SMS_STOC_APPROX) "spectrum approximation IFFT"
							      "none"))))
		(format #t "Analysis Signal Sampling Rate = ~D~%" (.iSamplingRate smsHeader))
		(if (> (.nTextCharacters smsHeader) 0)
		    (format #t "~%Header Text String:~%~S~%" (.pChTextCharacters smsHeader)))
		
		(if (not (= choice PRINT_HDR))
		    (let ((iFirstFrame (min (- (.nFrames smsHeader) 1)
					    (inexact->exact (floor (* beg (.iFrameRate smsHeader))))))
			  (iLastFrame (if end 
					  (min (.nFrames smsHeader)
					       (inexact->exact (floor (* end (.iFrameRate smsHeader)))))
					  (.nFrames smsHeader)))
			  (iFirstTrack (min firsttrack (.nTracks smsHeader)))
			  (iLastTrack (if (>= lasttrack 0)
					  (min lasttrack (.nTracks smsHeader))
					  (.nTracks smsHeader))))
		      (do ((i iFirstFrame (+ i 1)))
			  ((= i iLastFrame))
			(sms_getFrame smsFile smsHeader i smsData)
			(format #t "~%Frame #~D {~1,3F}: " i (exact->inexact (/ i (.iFrameRate smsHeader))))
			(if (not (= choice PRINT_STOC))
			    (begin
			      (format #t "~%    det:~%")
			      (do ((j iFirstTrack (+ j 1)))
				  ((= j iLastTrack))
				(if (or (= (.iFormat smsHeader) SMS_FORMAT_H)
					(= (.iFormat smsHeader) SMS_FORMAT_IH))
				    (format #t "~5,2F[~2,4F]  " 
					    (sms_floats_ref (.pFSinFreq smsData) j)
					    (sms_floats_ref (.pFSinAmp smsData) j))
				    (format #t "~5,2F[~2,4F, ~2,4F]  "
					    (sms_floats_ref (.pFSinFreq smsData) j)
					    (sms_floats_ref (.pFSinAmp smsData) j)
					    (sms_floats_ref (.pFSinPha smsData) j)))))
			    (if (and (not (= choice PRINT_DET))
				     (= (.iStochasticType smsHeader) SMS_STOC_APPROX))
				(begin
				  (format #t "~%    stoc_gain: ~F~%" 
					  (sms_floats_ref (.pFStocGain smsData) 0))
					  ;; surely this is a bug?!? 
				  (format #t "stoc_coefficients: ")
				  (do ((j 0 (+ j 1)))
				      ((= j (.nCoeff smsData)))
				    (format #t "~1,3F " (sms_floats_ref (.pFStocCoeff smsData) j))))))
			(newline))))))))))



;;; -------------------------------- smsSynth --------------------------------

