;;; GL spectrograph Snd hook
;;;
;;; file of data has: srate scaler cutoff slices bins followed by data[slices][bins] all float
;;; data file is "glfft.data" written first, then "glfft.lock" is created.
;;;   reader waits for glfft.lock, then reads glfft.data and deletes both
;;;   if writer sees lock file, returns without writing
;;;   if reader doesn't see lock file, returns without reading

(define data-file "glfft.data")
(define lock-file "glfft.lock")

(define write-fft-data 
  (lambda (snd chn scaler)
    (let ((selsnd (selected-sound))
	  (selchn (selected-channel snd)))
      (if (and (= (fft-style) spectrogram)
	       (or (= selsnd -1) (= selsnd snd))
	       (or (= selchn -1) (= selchn chn))
	       (not (file-exists? lock-file)))
	  (let ((data (transform-samples->vct snd chn))
		(sizes (cons (srate) (cons scaler (transform-size snd chn))))
		(fd (mus-sound-open-output data-file 1 1 mus-bfloat mus-raw "")))
	    (if (not (= fd -1))
		(let ((desc (list->vct sizes)))
		  (vct->sound-file fd desc 5)
		  (vct->sound-file fd data (vct-length data))
		  (mus-sound-close-output fd 0)
		  (set! fd (mus-sound-open-output lock-file 1 1 mus-bfloat mus-raw ""))
		  (mus-sound-close-output fd 0)))))
      #f)))

(define start-gl
  (lambda ()
    (add-hook! fft-hook write-fft-data)))

(define stop-gl 
  (lambda ()
    (remove-hook! fft-hook write-fft-data)))

(define cleanup-gl 
  (lambda ()
    (stop-gl)
    (if (file-exists? lock-file) (delete-file lock-file))
    (if (file-exists? data-file) (delete-file data-file))))

		
