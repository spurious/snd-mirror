;;; Snd-4 compatibility stuff

(use-modules (ice-9 format) (ice-9 optargs))
(provide 'snd-snd4.scm)

;;; Snd-4 map/scan/temp functions

(define* (scan-sound-chans proc :optional (beg 0) end snd edpos)
  "(scan-sound-chans proc :optional (beg 0) end snd edpos) applies scan-chan with proc to each channel in a sound"
  (let ((result #f))
    (do ((i 0 (1+ i)))
	((or (= i (chans snd))
	     result)
	 result)
      (let ((val (scan-chan proc beg end snd i edpos)))
	(if val
	    (set! result (append val (list (or snd (selected-sound)) i))))))))

(define* (map-sound-chans proc :optional (beg 0) end edname snd edpos)
  "(map-sound-chans proc :optional (beg 0) end edname snd edpos) applies map-chan with proc to each channel in a sound"
  (do ((i 0 (1+ i)))
      ((= i (chans snd)))
    (map-chan proc beg end edname snd i edpos)))


(define* (scan-all-chans proc :optional (beg 0) end edpos)
  "(scan-all-chans proc :optional (beg 0) end snd edpos) applies scan-chan with proc to all channels (all sounds)"
  (catch 'done
	 (lambda ()
	   (apply for-each 
		  (lambda (snd chn)
		    (let ((result (scan-chan proc beg end snd chn edpos)))
		      (if result (throw 'done (append result (list snd chn))))))
		  (all-chans)))
	 (lambda args (cadr args))))

(define* (map-all-chans proc :optional (beg 0) end edname edpos)
  "(map-all-chans proc :optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels (all sounds)"
  (apply for-each 
	 (lambda (snd chn)
	   (map-chan proc beg end edname snd chn edpos))
	 (all-chans)))


(define* (scan-chans proc :optional (beg 0) end edpos)
  "(scan-chans proc :optional (beg 0) end snd edpos) applies scan-chan with proc to all channels sharing current sound's sync"
  (let ((current-sync (sync (selected-sound))))
    (define (check-one-chan proc beg end snd chn edpos)
      (let ((val (scan-chan proc beg end snd chn edpos)))
	(if val
	    (append val (list snd chn))
	    #f)))
    (call-with-current-continuation
     (lambda (return)
       (for-each 
	(lambda (snd)
	  (if (= (sync snd) current-sync)
	      (do ((i 0 (1+ i)))
		  ((= i (chans snd)))
		(let ((val (check-one-chan proc beg end snd i edpos)))
		  (if val
		      (return val))))))
	(sounds))
       #f))))

(define* (map-chans proc :optional (beg 0) end edname edpos)
  "(map-chans proc :optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels sharing current sound's sync"
  (let ((current-sync (sync (selected-sound))))
    (for-each 
     (lambda (snd)
       (if (= (sync snd) current-sync)
	   (do ((i 0 (1+ i)))
	       ((= i (chans snd)))
	     (map-chan proc beg end edname snd i edpos))))
     (sounds))))


(define* (map-across-all-chans proc :optional (beg 0) end edname snd edpos)
  "(map-across-all-chans proc :optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels in parallel"
  (let* ((chans (all-chans))
	 (chan-num (length (car chans)))
	 (maxlen (apply max (apply map frames chans)))
	 (len (if (number? end)
		  (- (min end maxlen) beg)
		  (- maxlen beg)))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num))
	 (filenames (make-vector chan-num))
	 (outsamp 0)
	 (outgs (make-vector chan-num)))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg (car s) (car c) 1 edpos))
      (vector-set! filenames j (snd-tempnam))
      (vector-set! outgs j (make-sample->file (vector-ref filenames j) 1 mus-out-format mus-next)))
    (do ((i 0 (1+ i)))
	((= i len))
      (do ((j 0 (1+ j)))
	  ((= j chan-num))
	(vector-set! data j (next-sample (vector-ref fds j))))
      (let ((newdata (proc data chan-num)))
	(if newdata
	    (begin
	      (do ((j 0 (1+ j)))
		  ((= j chan-num))
		(out-any outsamp (vector-ref newdata j) 0 (vector-ref outgs j)))
	      (set! outsamp (1+ outsamp))))))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (mus-close (vector-ref outgs j))
      (free-sample-reader (vector-ref fds j))
      (if (> outsamp 0)
	  (begin
	    (if (not (= outsamp len))
		(delete-samples beg len (car s) (car c)))
	    (set! (samples beg outsamp (car s) (car c) #t edname) (vector-ref filenames j))
	    (delete-file (vector-ref filenames j)))))))

(define* (scan-across-all-chans proc :optional (beg 0) end snd edpos)
  "(scan-across-all-chans proc :optional (beg 0) end edname snd edpos) applies scan-chan with proc to all channels in parallel"
  (let* ((chans (all-chans))
	 (chan-num (length (car chans)))
	 (maxlen (apply max (apply map frames chans)))
	 (len (if (number? end)
		  (- (min end maxlen) beg)
		  (- maxlen beg)))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num)))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg (car s) (car c) 1 edpos)))
    (catch 'done
	   (lambda ()
	     (do ((i 0 (1+ i)))
		 ((= i len))
	       (do ((j 0 (1+ j)))
		   ((= j chan-num))
		 (vector-set! data j (next-sample (vector-ref fds j))))
	       (let ((newdata (proc data chan-num)))
		 (if newdata
		     (throw 'done (list newdata (+ i beg)))))))
	   (lambda args (cadr args)))))

(define* (map-across-sound-chans proc :optional (beg 0) end edname snd edpos)
  "(map-across-sound-chans proc :optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels or sound in parallel"
  (let* ((chan-num (chans snd))
	 (len (- (min end (frames snd 0)) beg))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num))
	 (filenames (make-vector chan-num))
	 (outsamp 0)
	 (outgs (make-vector chan-num)))
    (do ((j 0 (1+ j)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg snd j 1 edpos))
      (vector-set! filenames j (snd-tempnam))
      (vector-set! outgs j (make-sample->file (vector-ref filenames j) 1 mus-out-format mus-next)))
    (do ((i 0 (1+ i)))
	((= i len))
      (do ((j 0 (1+ j)))
	  ((= j chan-num))
	(vector-set! data j (next-sample (vector-ref fds j))))
      (let ((newdata (proc data chan-num)))
	(if newdata
	    (begin
	      (do ((j 0 (1+ j)))
		  ((= j chan-num))
		(out-any outsamp (vector-ref newdata j) 0 (vector-ref outgs j)))
	      (set! outsamp (1+ outsamp))))))
    (do ((j 0 (1+ j)))
	((= j chan-num))
      (mus-close (vector-ref outgs j))
      (free-sample-reader (vector-ref fds j))
      (if (> outsamp 0)
	  (begin
	    (if (not (= outsamp len))
		(delete-samples beg len snd j))
	    (set! (samples beg outsamp snd j #t edname) (vector-ref filenames j))
	    (delete-file (vector-ref filenames j)))))))


;;; Snd-4 external program support stuff

(define* (selection-to-temp :optional (type mus-next) (format mus-out-format))
  "(selection-to-temp :optional (type mus-next) (format mus-out-format)) writes selection data as (multichannel) file (for external program)"
  (let ((data (make-vector 1)))
    (vector-set! data 0 (snd-tempnam))
    (save-selection (vector-ref data 0) type format)
    data))

(define (syncd-sounds val)
  "(syncd-sounds val) returns how many sounds share the sync value 'val'"
  (let ((ctr 0))
    (for-each 
     (lambda (n)
       (if (= (sync n) val)
	   (set! ctr (+ ctr 1))))
     (sounds))
    ctr))

(define* (sound-to-temp :optional (type mus-next) (format mus-out-format) edpos)
  "(sound-to-temp :optional (type mus-next) (format mus-out-format) edpos) writes sound data as (multichannel) file (for external program)"
  (let* ((cursnd (selected-sound))
	 (cursync (sync cursnd)))
    (if (or (= cursync 0)
	    (= (syncd-sounds cursync) 1))
	(let ((data (make-vector 1)))
	  (vector-set! data 0 (snd-tempnam))
	  (save-sound-as (vector-ref data 0) (selected-sound) type format :edit-position edpos)
	  data)
	(snd-error "re-implemented sound-to-temp doesn't handle sync bit correctly yet."))))

(define* (selection-to-temps :optional (type mus-next) (format mus-out-format))
  "(selection-to-temps :optional (type mus-next) (format mus-out-format)) writes selection data as mono files (for external program)"
  (let* ((chns (selection-chans))
	 (data (make-vector chns)))
    (do ((i 0 (1+ i))) 
	((= i chns)) 
      (vector-set! data i (snd-tempnam))
      (save-selection (vector-ref data i) type format :channel i))
    data))

  
(define* (sound-to-temps :optional (type mus-next) (format mus-out-format) edpos)
  "(sound-to-temps :optional (type mus-next) (format mus-out-format) edpos) writes sound data as mono files (for external program)"
  (let* ((cursnd (selected-sound))
	 (cursync (sync cursnd)))
    (if (or (= cursync 0)
	    (= (syncd-sounds cursync) 1))
	(let* ((chns (chans cursnd))
	       (data (make-vector chns)))
	  (do ((i 0 (1+ i)))
	      ((= i chns))
	    (vector-set! data i (snd-tempnam))
	    (save-sound-as (vector-ref data i) (selected-sound) type format :channel i :edit-position edpos))
	  data)
	(snd-error "re-implemented sound-to-temps doesn't handle sync bit correctly yet."))))

(define (temp-filenames data) 
  "(temp-filenames data) returns 'data'"
  data)

(define* (temp-to-sound data filename :optional origin)
  "(temp-to-sound data filename :optional origin) reads (multichannel) file as new sound data (from external program)"
  (let ((cursnd (selected-sound)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file filename)))
	    (delete-file temp-file))))
    (do ((i 0 (1+ i)))
	((= i (chans cursnd)))
      (set! (samples 0 (mus-sound-frames filename)  cursnd i #t origin i) filename))))

(define* (temps-to-sound data filenames :optional origin)
  "(temps-to-sound data filenames :optional origin) reads mono files as new sound data (from external program)"
  (let ((cursnd (selected-sound)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file (vector-ref filenames i))))
	    (delete-file temp-file))))
    (do ((i 0 (1+ i)))
	((= i (chans cursnd)))
      (set! (samples 0 (mus-sound-frames (vector-ref filenames i)) cursnd i #t origin) (vector-ref filenames i)))))

(define* (temp-to-selection data filename :optional origin)
  "(temp-to-selection data filename :optional origin) sets selection from (multichannel) file (from external program)"
  (let ((chan 0)
	(len (mus-sound-frames filename)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file filename)))
	    (delete-file temp-file))))
    (for-each
     (lambda (n)
       (do ((i 0 (1+ i)))
	   ((= i (chans n)))
	 (if (selection-member? n i)
	     (begin
	       (set! (samples (selection-position n i) len n i #t origin chan) filename)
	       (set! chan (+ chan 1))))))
     (sounds))))

(define* (temps-to-selection data filenames :optional origin)
  "(temps-to-selection data filenames :optional origin) sets selection from mono files (from external program)"
  (let ((chan 0))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file (vector-ref filenames i))))
	    (delete-file temp-file))))
    (for-each
     (lambda (n)
       (do ((i 0 (1+ i)))
	   ((= i (chans n)))
	 (if (selection-member? n i)
	     (let ((len (mus-sound-frames (vector-ref filenames chan))))
	       (set! (samples (selection-position n i) len n i #t origin) (vector-ref filenames chan))
	       (set! chan (+ chan 1))))))
     (sounds))))
