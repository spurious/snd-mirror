; easy edit
;
;version 0.3
;author tom roth <rawdlite@yahoo.com>
;objective:
;keybindings to make sound editing as easy and convenient
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




(define status 0)

(add-hook! after-open-hook
  (lambda (snd)
  (set! status 0)))

(define (eos )
(set! (cursor) (+ (selection-position) (selection-length))))

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
(set! (x-bounds) 
    (list (/ (selection-position) (srate))
          (/ (+ (selection-position) (selection-length)) (srate))))
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
(eos)
(set! (cursor) (+ (+ (selection-position) (selection-length)) dif))
(make-selection (- (cursor) length )  (cursor))
;(set! (x-bounds) 
;    (list (/ (selection-position) (srate))
;          (/ (+ (selection-position) (selection-length)) (srate))))
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
(play-selection))

(define (play-song)
(if (eq? status 1)(set! status 2))
(play (cursor) (selected-sound) (selected-channel)))

(bind-key (char->integer #\d) 0 (lambda ()(delete-selection-and-smooth)))


; to lazy to code ;-)

(define (my_crop)
  (key (char->integer #\<) 4)
  (key (char->integer #\ ) 4)
  (key (char->integer #\j) 4)
  (key (char->integer #\d) 0)
  (key (char->integer #\j) 4)
  (key (char->integer #\ ) 4)
  (key (char->integer #\>) 5)
  (key (char->integer #\d) 0)
)

(bind-key (char->integer #\c) 0  (lambda () (my_crop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; status 
;;;;             0 = new
;;;              1 = start is set
;;;              2 = playing
;;;              3 = end is set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(bind-key (char->integer #\p) 0 (lambda () (play-song)))



; f fade in or out  selection
(bind-key (char->integer #\f) 0  (lambda()  (case status
                                ((0) (mark-start 50000))
                                ((1) (env-selection '(0 0  2 1)))
                                ((2) (mark-end   50000))
                                ((3) (env-selection '(0 1  2 0)))
                                (else (set! status 0))
))) 




(bind-key (char->integer #\^) 0 (lambda () (forward-sample (selection-length))))



;(bind-key (char->integer #\1) 8 (lambda () (my-play-selection-forw 50000 50000)))
;(bind-key  (char->integer #\!) 9  (lambda () (my-play-selection-backw -50000 50000)))

;; + zoom in 
;(bind-key (char->integer #\+) 0 (lambda () (set! (x-bounds) 
;                                             (list (/ (selection-position) (srate))
;                                                   (/ (+ (selection-position) (selection-length)) (srate)))
;                                                   )
;                       )
;)

;; - zoom out
;(bind-key (char->integer #\-) 0 (lambda () (let* ((curs (cursor)))
;                                             (set! (x-bounds) (list 0 5))
;                                             (set! (cursor) curs))
;                       )
;)

;(bind-key (char->integer #\t) 0 (lambda () (case status
;                               ((1) (my-play-selection (cursor) (+ (cursor) 100000)))
;                               ((3) (eos)
;                                     (my-play-selection (- (cursor) 100000)  (cursor) ))
;                               (else (report-in-minibuffer "status:" (status) ) )
;                               )
;                       )
;)
