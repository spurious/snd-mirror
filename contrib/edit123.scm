; easy edit
;
;version 0.6
;keybindings and functions to make sound editing as easy and convenient
;as pressing 1 2 3
;1 set mark
;2 move mark backward
;3 move mark backward by a fraction
;C-2 move mark forward
;C-3 move mark forward by a fraction
;p set mark fixed and play (important: you have to use this key to finalize the setting of the  start mark )
;f fade
;c crop delete the part before and after the marks
;C-1 toggles start and end status 
; keys have multiple actions assigned depending on the status of the editing process
; there are some more keybindings, that i commented out.
; you can suit them to your taste.
; 
; tom@tomroth.de

;channels have to be synced (how i wish there would be mode specific settings)

;(add-hook! after-open-hook
;  (lambda (snd)
;      (if (> (channels snd) 1)
;        (begin
;        (set! (sync snd) (1+ snd)) ; 0 = #f
;         (set! (channel-style snd) channels-combined)
;))

(define status 0)
(define curpos 0)
(define is-playing 0)


(add-hook! after-open-hook
  (lambda (snd)
  (set! status 0)
))

(add-hook! start-playing-hook 
               (lambda (snd)
               (set! is-playing 1)))


 (add-hook! stop-playing-hook 
               (lambda (snd)
               (set! is-playing 0)))

(define (eos )
(set! (cursor) (+ (selection-position) (selection-frames))))

(define (mark-named name)
(set! (mark-name (add-mark (cursor) )) name)
)

(define (goto-named-mark name)
(set! (cursor) (mark-sample (find-mark name))))

(define (delete-named-mark name)
(delete-mark (find-mark name)))

(define (my-play-selection pos1 pos2)
(stop-playing)
 (make-selection pos1 pos2)
;(set! (x-bounds) 
;    (list (/ (selection-position) (srate))
;          (/ (+ (selection-position) (selection-length)) (srate))))
(play-selection))

(define (test-mark-forw name  length)
(stop-playing)
(goto-named-mark name)
(make-selection (cursor)  (+ (cursor) length))
(play-selection))

(define (test-mark-backw name  length)
(stop-playing)
(goto-named-mark name)
(make-selection (- (cursor) length) (cursor)  )
(play-selection))



(define (move-start dif length)
(stop-playing) 
(goto-named-mark "start")
(delete-named-mark "start")
(set! (cursor) (+ (cursor) dif))
(mark-named "start")
(make-selection (cursor)  (+ (cursor) length))
(play-selection))

(define (move-end dif length)
(stop-playing) 
(goto-named-mark "end")
(delete-named-mark "end")
(set! (cursor) (+ (cursor) dif))
(mark-named "end")
(make-selection (- (cursor) length) (cursor))
(play-selection))



(define (my-play-selection-forw dif length)
(stop-playing)
(set! (cursor) (+ (cursor) dif))
(make-selection (cursor)  (+ (cursor) length))
;(set! (x-bounds) 
;    (list (/ (selection-position) (srate))
;          (/ (+ (selection-position) (selection-length)) (srate))))
(play-selection))



(define (my-play-selection-backw dif length)
(stop-playing)
(set! (cursor) (- (cursor) dif))
(make-selection (cursor)  (+ (cursor) length))
(play-selection))



(define (forward-selection)
  (if (not (selection?)) (make-selection (cursor)  (+ (cursor) 20000)))
 (if (< (selection-frames) 2000) (make-selection  (- (cursor) 2000) (cursor)))
 ; is cursor inside of selection ?
 (if  (or (< (cursor) (selection-position)) (> (cursor) (+ (selection-position) (selection-frames))))
      (begin
	(make-selection (cursor)  (+ (cursor) (selection-frames)))
	(stop-playing)
        (eos)))
 (if  (and (>= (cursor) (selection-position)) (<= (cursor) (+ (selection-position) (selection-frames))))  
      (begin  
	(stop-playing)
	(eos)
	(make-selection (cursor)  (+ (cursor) (selection-frames)))))
 (play-selection))


(define (backward-selection)
  (if (not (selection?)) (make-selection  (- (cursor) 20000) (cursor)))
  (if (< (selection-frames) 2000) (make-selection  (- (cursor) 2000) (cursor)))
 (if  (or (< (cursor) (selection-position)) (> (cursor) (+ (selection-position) (selection-frames))))
      (begin
	(make-selection (cursor)  (- (cursor) (selection-frames)))
	(stop-playing)
	(set! (cursor) (selection-position))))

 (if  (and (>= (cursor) (selection-position)) (<= (cursor) (+ (selection-position) (selection-frames))))  
      (begin  
	(stop-playing)
	(set! (cursor) (selection-position))))
 (make-selection  (- (cursor) (selection-frames)) (cursor) )
 (play-selection))


(define (mark-start  length)
(if (find-mark "start")
    (delete-named-mark "start")
)
(mark-named "start")
(stop-playing)
(goto-named-mark "start")
(set! status 1)
(make-selection (cursor)  (+ (cursor) length))
(stop-playing)
(key (char->integer #\t) 4)
(play-selection))

(define (mark-end  length)
(if (find-mark "end")
    (delete-named-mark "end")
)
(mark-named "end")
(stop-playing)
(goto-named-mark "end")
(set! status 3)
(make-selection  (- (cursor) length) (cursor) )
(key (char->integer #\t) 4)
(play-selection))

(define (stop-song)
(set! curpos (cursor))
(stop-playing)
(set! (cursor) curpos))

(define (play-song)
(stop-playing)
(select-channel 0)
;(if (eq? status 1)(set! status 2))
;(if (eq? status 3)(set! status 0))
(let ((old-tracking (cursor-follows-play)))
    (set! (cursor-follows-play) #t)
    (add-hook! stop-playing-hook 
               (lambda (snd)
                 (set! (cursor-follows-play) old-tracking))))

(let ((old-channel-style(channel-style (selected-sound))))
(lambda (snd)
    
        (set! (channel-style snd) channels-superimposed)))
    (play (cursor)))

(define (play-end)
(key (char->integer #\>) 1)
(stop-playing)
(key (char->integer #\t) 4)
(set! status 2)
(set! (cursor) (- (cursor) 100000))
(play (cursor))
)

(define (toggle-play)
(lambda () (case is-playing
((1) (stop-playing))
((0) (play))
)))

;double-selection
(define (double-selection)
(set! (cursor) (selection-position))
(make-selection (selection-position) (+ (selection-position)(*  (selection-frames) 2)))
)


;half-selection
(define (half-selection)
(set! (cursor) (selection-position))
(make-selection (selection-position) (+ (selection-position)(/  (selection-frames) 2)))
)

;play-selection



; to lazy to code ;-)

(define (my_crop)
(if (find-mark "start")
(begin
  (key (char->integer #\<) 4)
  (key (char->integer #\ ) 4)
  (key (char->integer #\j) 4)
  (key (char->integer #\d) 0)
))
(if (find-mark "end")
(begin
  (key (char->integer #\j) 4)
  (key (char->integer #\ ) 4)
  (key (char->integer #\>) 5)
  (key (char->integer #\d) 0)
))
)

(bind-key (char->integer #\d) 0 (lambda ()(delete-selection-and-smooth)))
(bind-key (char->integer #\s) 0  (lambda () (
(define-selection-via-marks (find-mark "start") (find-mark "end"))
(prompt "file:" save-region))))

(bind-key (char->integer #\ ) 8  (lambda () (stop-song)))
(bind-key (char->integer #\c) 0  (lambda () (my_crop)))
(bind-key (char->integer #\x) 0  (lambda () (half-selection)))
(bind-key (char->integer #\X) 1  (lambda () (double-selection)))
(bind-key (char->integer #\y) 0  (lambda () (double-selection)));german version
(bind-key (char->integer #\z) 0  (lambda () (double-selection)));english version
(bind-key (char->integer #\^) 0 (lambda () (my-play-selection-forw 50000 50000)))
(bind-key (char->integer #\^) 4 (lambda () (my-play-selection-backw 50000 50000)))
(bind-key (char->integer #\t) 8 (lambda () (play-selection)))
(bind-key (char->integer #\p) 0 (lambda () (play-song)))
(bind-key (char->integer #\P) 1 (lambda () (play-end)))
(bind-key (char->integer #\p) 8 (lambda () (toggle-play)))
(bind-key (char->integer #\3) 8 (lambda () (forward-selection)))
(bind-key (char->integer #\2) 8 (lambda () (backward-selection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; status 
;;;;             0 = new
;;;              1 = start is set
;;;              2 = playing
;;;              3 = end is set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key #xFFBE 0 (lambda () (case status
				((0) (mark-start 50000))
				((1) (mark-start 50000))
				((2) (mark-end   50000))
				((3) (mark-end   40000))
				(else (set! status 0))
)))


(bind-key (char->integer #\1) 0 (lambda () (case status
				((0) (mark-start 50000))
				((1) (mark-start 50000))
				((2) (mark-end   50000))
				((3) (mark-end   40000))
				(else (set! status 0))
)))

(bind-key (char->integer #\1) 4 (lambda () (case status
				((0) (mark-end 50000))
				((1) (set! status 3))
				((2) (mark-start   50000))
				((3) (set! status 1))
				(else (set! status 0))
)))

(bind-key (char->integer #\1) 8 (lambda () (case status
				((0) (set! status 2))
				((1) (set! status 3))
				((2) (set! status 0))
				((3) (set! status 1))
				(else (set! status 0))
)))

(bind-key (char->integer #\2) 4 (lambda () (case status
				((0) (mark-start 30000))
				((1) (move-start 2000 50000))
				((2) (mark-end   50000))
				((3) (move-end 2000 50000))
				(else (set! status 0))
)))

(bind-key (char->integer #\2) 0 (lambda ()(case status
				((0) (mark-start 30000))
				((1) (move-start -2000 50000))
				((2) (mark-end   50000))
				((3) (move-end -2000 50000))
				(else (set! status 0))
)))

(bind-key (char->integer #\3) 4 (lambda () (case status
				((0) (mark-start 20000))
				((1) (move-start 300 20000))
				((2) (mark-end   20000))
				((3) (move-end 300 20000))
				(else (set! status 0))
)))

(bind-key (char->integer #\3) 0 (lambda () (case status
				((0) (mark-start 20000))
				((1) (move-start -300 20000))
				((2) (mark-end   20000))
				((3) (move-end -300 20000))
				(else (set! status 0))
)))

(bind-key (char->integer #\p) 4 (lambda () (toggle-play)))
(bind-key (char->integer #\p) 0 (lambda () (play-song)))
(bind-key (char->integer #\P) 1 (lambda () (play-end)))

; f fade in or out  selection
(bind-key (char->integer #\f) 0  (lambda()  (case status
				((0) (mark-start 50000))
				((1) (env-selection '(0 0  2 1)))
				((2) (mark-end   50000))
				((3) (env-selection '(0 1  2 0)))
				(else (set! status 0))
))) 

(bind-key (char->integer #\t) 0 (lambda () (case status

				((1) (goto-named-mark "start") (my-play-selection (cursor) (+ (cursor) 100000)))
                                
				
				((3)  (goto-named-mark "end") (my-play-selection (- (cursor) 100000) (cursor)))
;				(else (report-in-minibuffer "status:" (status) ) )
				)
			)
)
;  change status



; + zoom in 
(bind-key (char->integer #\+) 0 (lambda () (set! (x-bounds) 
	                                                           (list (/ (selection-position) (srate))
                                                                              (/ (+ (selection-position) (selection-length)) (srate))) 
								                                                              (cursor-on-left)
                                                                         )
			)
)

; - zoom out
(bind-key (char->integer #\-) 0 (lambda () (let* ((curs (cursor)))
                                                               (set! (x-bounds) (list 0 5))
                                                               (set! (cursor) curs))
			)
)

