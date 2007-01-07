\ -*- snd-forth -*-
\ extensions.fs -- extensions.scm -> extensions.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sun Dec 18 19:21:00 CET 2005
\ Changed: Thu Jan 04 18:54:24 CET 2007

\ Commentary:
\ 
\ Snd-7 compatibility
\ 
\ color-map constants
\ mus-a0, set-mus-a0, etc
\ back-or-forth-graph 		  ( count -- lst )
\ forward-graph       		  ( count -- lst )
\ backward-graph      		  ( count -- lst )
\ back-or-forth-mix   		  ( count snd chn -- mx )
\ forward-mix         		  ( count snd chn -- mx )
\ backward-mix        		  ( count snd chn -- mx )
\ back-or-forth-mark  		  ( count snd chn -- mk )
\ forward-mark        		  ( count snd chn -- mk )
\ backward-mark       		  ( count snd chn -- mk )
\ mus-bank            		  ( gens amps in1 in2 -- val )
\ oscil-bank          		  ( amps gens in1 in2 -- val )
\
\ sound-property       		  ( key snd -- value|#f )
\ set-sound-property   		  ( key val snd -- )
\ set-sound-property-save-state-ignore   ( key snd -- )
\ channel-property     		  ( key snd chn -- value|#f )
\ set-channel-property 		  ( key val snd chn -- )
\ set-channel-property-save-state-ignore ( key snd chn -- )
\
\ selection-members               ( -- array of lists )
\
\ yes-or-no?                      ( question action-if-yes action-if-no snd -- )
\ check-for-unsaved-edits         ( check -- )
\ remember-sound-state            ( choice -- )
\
\ mix-channel                     ( file-data :optional beg dur snd chn edpos -- val )
\ insert-channel                  ( file-data :optional beg 0 dur snd chn edpos -- val )
\ any-env-channel                 ( en func :optional beg dur snd chn edpos origin -- val )
\ sine-ramp                       ( rmp0 rmp1 :optional beg dur snd chn edpos -- val )
\ sine-env-channel                ( en :optional beg dur snd chn edpos -- val )
\ blackman4-ramp                  ( rmp0 rmp1 :optional beg dur snd chn edpos -- val )
\ blackman4-env-channel           ( en :optional beg dur snd chn edpos -- val )
\ ramp-squared                    ( rmp0 rmp1 :optional symmetric beg dur snd chn edpos -- val )
\ env-squared-channel             ( en :optional symmetric beg dur snd chn edpos -- val )
\ ramp-expt                       ( rmp0 rmp1 exponent
\                                   :optional symmetric beg dur snd chn edpos -- val )
\ env-expt-channel                ( en exponent
\                                   :optional symmetric beg dur snd chn edpos -- val )
\ offset-channel                  ( amount :optional beg dur snd chn edpos -- val )
\ offset-sound                    ( offset :optional beg dur snd -- )
\ dither-channel                  ( :optional amount beg dur snd chn edpos -- val )
\ dither-sound                    ( :optional amount beg dur snd -- )
\ contrast-channel                ( index :optional beg dur snd chn edpos -- val )
\ contrast-sound                  ( index :optional beg dur snd -- )
\
\ focus-follows-mouse             ( -- )
\ prefs-activate-initial-bounds   ( beg dur full -- )
\ prefs-deactivate-initial-bounds ( -- )
\ with-reopen-menu     		  ( -- )
\ with-buffers-menu    		  ( -- )
\ set-global-sync                 ( choice -- )
\ show-selection                  ( -- )

\ Code:

require clm
require examp

\ === Snd-7 compatibility stuff ===
: mus-a0     ( gen     -- val ) 0          mus-xcoeff ;
: set-mus-a0 ( gen val -- val ) 0 swap set-mus-xcoeff ;
: mus-a1     ( gen     -- val ) 1          mus-xcoeff ;
: set-mus-a1 ( gen val -- val ) 1 swap set-mus-xcoeff ;
: mus-a2     ( gen     -- val ) 2          mus-xcoeff ;
: set-mus-a2 ( gen val -- val ) 2 swap set-mus-xcoeff ;
: mus-b1     ( gen     -- val ) 1          mus-ycoeff ;
: set-mus-b1 ( gen val -- val ) 1 swap set-mus-ycoeff ;
: mus-b2     ( gen     -- val ) 2          mus-ycoeff ;
: set-mus-b2 ( gen val -- val ) 2 swap set-mus-ycoeff ;

: back-or-forth-graph ( count -- lst )
  { count }
  sounds if
    #f snd-snd { cursnd }
    #f snd-chn { curchn }
    0 { curpos }
    0 { pos }
    #() { sndlst }
    sounds each { snd }
      snd channels 0 ?do
	cursnd snd =
	curchn i = && if pos to curpos then
	pos 1+ to pos
	sndlst '( snd i ) array-push drop
      loop
    end-each
    curpos count + sndlst object-length mod { newpos }
    sndlst newpos array-ref { vals }
    vals car     set-selected-sound   drop
    vals cadr #f set-selected-channel drop
    '( selected-sound #f selected-channel )
  else
    nil
  then
;
: forward-graph  ( count -- lst )        back-or-forth-graph ;
: backward-graph ( count -- lst ) negate back-or-forth-graph ;

hide
: sort-mix-pos ( a b -- n )
  { a b }
  a mix-position { am }
  b mix-position { bm }
  am bm < if
    1
  else am bm > if
      -1
    else
      0
    then
  then
;
set-current
: back-or-forth-mix <{ count :optional snd #f chn #f -- mx|#f }>
  snd chn mixes { mx }
  count 0<> mx null? not && if
    mx length 1 = if
      mx car mix-position snd chn #f set-cursor	drop
      mx car				\ retval
    else
      mx ['] sort-mix-pos object-sort array->list { sorted-mx }
      snd chn #f cursor { pos }
      count 0> if -1 else 0 then { curpos }
      pos sorted-mx car mix-position >= if
	sorted-mx each { m }
	  count 0>
	  pos m mix-position < &&
	  count 0<
	  pos m mix-position <= && || if
	    leave
	  else
	    curpos 1+ to curpos
	  then
	end-each
      then
      curpos count + mx length mod to curpos
      sorted-mx curpos object-ref { val }
      val mix-position snd chn #f set-cursor drop
      val				\ retval
    then
  else
    #f
  then
;
previous
: forward-mix  <{ count :optional snd #f chn #f -- mx|#f }>
  count        snd snd-snd chn snd-chn back-or-forth-mix
;
: backward-mix <{ count :optional snd #f chn #f -- mx|#f }>
  count negate snd snd-snd chn snd-chn back-or-forth-mix
;

hide
: sort-mark-sample ( a b -- n )
  { a b }
  a #f mark-sample { am }
  b #f mark-sample { bm }
  am bm < if
    1
  else am bm > if
      -1
    else
      0
    then
  then
;
set-current
: back-or-forth-mark <{ count :optional snd #f chn #f -- mk|#f }>
  snd chn #f marks { mk }
  count 0<>
  mk empty? not && if
    mk length 1 = if
      mk car #f mark-sample snd chn #f set-cursor drop
      mk car				\ retval
    else
      mk ['] sort-mark-sample object-sort array->list { sorted-mk }
      snd chn #f cursor { pos }
      count 0> if -1 else 0 then { curpos }
      pos sorted-mk car #f mark-sample >= if
	sorted-mk each { m }
	  count 0>
	  pos m #f mark-sample < &&
	  count 0<
	  pos m #f mark-sample <= && || if
	    leave
	  else
	    curpos 1+ to curpos
	  then
	end-each
      then
      curpos count + mk length mod to curpos
      sorted-mk curpos object-ref { val }
      val mark-sample snd chn #f set-cursor
      val				\ retval
    then
  else
    #f
  then
;
previous
: forward-mark  <{ count :optional snd #f chn #f -- mk|#f }>
  count        snd snd-snd chn snd-chn back-or-forth-mark
;
: backward-mark <{ count :optional snd #f chn #f -- mk|#f }>
  count negate snd snd-snd chn snd-chn back-or-forth-mark
;

: mus-bank ( gens amps in1 in2 -- sum )
  { gens amps in1 in2 }
  0.0					\ sum
  gens each ( gen ) in1 i object-ref in2 i object-ref mus-run amps i vct-ref f* ( sum ) f+ end-each
;
: oscil-bank ( amps gens in1 in2 -- sum )
  { amps gens in1 in2 }
  0.0					\ sum
  gens each ( gen ) in1 i object-ref in2 i object-ref oscil amps i vct-ref f* ( sum ) f+ end-each
;

\ === PROPERTIES ===

: sound-property <{ key :optional snd #f -- val }>
  doc" Returns the value associated with KEY in the given sound's property list, or #f."
  snd sound-properties key list-assoc-ref
;

: set-sound-property <{ key val :optional snd #f -- alist }>
  doc" Sets key-value pair in the given sound's property list.  \
If KEY exists, VAL overwrites the old value."
  snd sound-properties key val list-assoc-set! snd set-sound-properties
;

: set-sound-property-save-state-ignore <{ key :optional snd #f -- alist }>
  'save-state-ignore
  key
  'save-state-ignore snd sound-property dup false? if
    drop
    'save-state-ignore '() cons
  then cons
  snd set-sound-property
;

: channel-property <{ key :optional snd #f chn #f -- val }>
  doc" Returns the value associated with KEY in the given channel's property list, or #f."
  snd chn channel-properties key list-assoc-ref
;

: set-channel-property <{ key val :optional snd #f chn #f -- alist }>
  doc" Sets key-value pair in the given channel's property list.  \
If KEY exists, VAL overwrites the old value."
  snd chn channel-properties key val list-assoc-set! snd chn set-channel-properties
;

: set-channel-property-save-state-ignore <{ key :optional snd #f chn #f -- alist }>
  'save-state-ignore
  key
  'save-state-ignore snd chn channel-property dup false? if
    drop
    'save-state-ignore '() cons
  then cons
  snd chn set-channel-property
;

: selection-members ( -- array-of-lists )
  doc" Array of lists of '( snd chn ) indicating the channels \
participating in the current selection."
  #() { sndlist }
  selection? if
    sounds each { snd }
      snd channels 0 ?do
	snd i selection-member? if sndlist '( snd i ) array-push drop then
      loop
    end-each
  then
  sndlist
;

\ --- check-for-unsaved-edits ---

hide
: response-cb { snd action-if-yes action-if-no -- proc; response self -- val }
  1 proc-create action-if-no , action-if-yes , snd ,
 does> { response self -- val }
  self @ { action-if-no }
  self cell+ @ { action-if-yes }
  self 2 cells + @ { snd }
  snd clear-minibuffer drop
  response 0 string-ref [char] y = if
    action-if-yes snd run-proc
  else
    action-if-no snd run-proc
  then
;
set-current
: yes-or-no? ( question action-if-yes action-if-no snd -- )
  { question action-if-yes action-if-no snd }
  snd clear-minibuffer drop
  question
  snd action-if-yes action-if-no response-cb
  snd
  #t
  prompt-in-minibuffer drop
;
previous

hide
: yes-cb ( exiting -- prc; snd self -- val )
  1 proc-create swap ,
 does> { snd self -- val }
  snd revert-sound drop
  self @ ( exiting ) if 0 snd-exit then
  #t
;
lambda: <{ snd -- val }> #f ; value no-cb

: ignore-unsaved-edits-at-close? ( snd exiting -- f )
  { snd exiting }
  #t
  snd channels 0 ?do
    snd i edits car 0> if
      $" %s[%d] has unsaved edits.  Close (y/n)? " _ '( snd short-file-name i ) string-format
      exiting yes-cb no-cb snd yes-or-no?
      not
    then
  loop
;

: unsaved-edits-at-close <{ snd -- f }> snd #f ignore-unsaved-edits-at-close? not ;
: unsaved-edits-at-exit <{ -- f }>
  #f sounds each #t ignore-unsaved-edits-at-close? unless not leave then end-each
;
set-current

#f value checking-for-unsaved-edits

: check-for-unsaved-edits ( check -- )
  doc" Sets up hooks to check for and ask about unsaved edits when a sound is closed.  \
If CHECK is #f, the hooks are removed."
  ( check ) dup to checking-for-unsaved-edits if
    before-close-hook ['] unsaved-edits-at-close add-hook!
    before-exit-hook  ['] unsaved-edits-at-exit  add-hook!
  else
    before-close-hook ['] unsaved-edits-at-close remove-hook! drop
    before-exit-hook  ['] unsaved-edits-at-exit  remove-hook! drop
  then
;
previous

\ --- remember-sound-state ---

".snd-remember-sound" value remember-sound-filename
#() value -saved-remember-sound-states-states-

hide
#( ' sync ' cursor-follows-play ' selected-channel ' show-controls ' read-only
   ' contrast-control? ' expand-control? ' reverb-control? ' filter-control?
   ' amp-control-bounds
   ' contrast-control ' contrast-control-amp ' contrast-control-bounds
   ' expand-control ' expand-control-bounds ' expand-control-hop ' expand-control-jitter
   ' expand-control-length ' expand-control-ramp
   ' filter-control-envelope ' filter-control-in-dB ' filter-control-in-hz ' filter-control-order 
   ' reverb-control-decay ' reverb-control-feedback ' reverb-control-length
   ' reverb-control-length-bounds ' reverb-control-lowpass ' reverb-control-scale
   ' reverb-control-scale-bounds
   ' speed-control ' speed-control-bounds ' speed-control-style
   ' speed-control-tones ) value sound-funcs
#( ' time-graph? ' transform-graph? ' lisp-graph? ' x-bounds ' y-bounds ' cursor ' cursor-size
   ' amp-control
   ' cursor-style ' show-marks ' show-y-zero ' show-grid ' wavo-hop ' wavo-trace
   ' max-transform-peaks
   ' show-transform-peaks ' fft-log-frequency ' fft-log-magnitude ' verbose-cursor ' zero-pad
   ' wavelet-type ' min-dB ' transform-size ' transform-graph-type ' time-graph-type ' fft-window
   ' transform-type ' transform-normalization ' time-graph-style ' show-mix-waveforms ' dot-size
   ' x-axis-style ' show-axes ' graphs-horizontal ' lisp-graph-style ' transform-graph-style
   ' grid-density ) value channel-funcs
#() value remember-states
0 value remember-choice

: saved-state ( snd -- ary|#f )
  { snd }
  snd file-name { fname }
  #f					\ flag
  remember-states each { ary }
    ary 0 array-ref fname string= if drop ary leave then
  end-each
;

: set-saved-state ( snd new-state -- )
  { snd new-state }
  snd file-name { fname }
  #t					\ flag
  remember-states each { ary }
    ary 0 array-ref fname string= if
      not				\ toggle flag
      remember-states i new-state array-set!
      leave
    then
  end-each ( flag ) if
    remember-states new-state array-push drop
  then
;

: remember-sound-at-close <{ snd -- #f }>
  4 nil make-array { new-state }
  new-state 0 snd file-name array-set!
  new-state 1 snd file-name file-write-date array-set!
  new-state 2 sound-funcs map snd *key* execute end-map array-set!
  new-state 3 snd channels nil make-array map!
    channel-funcs map *key* { fnc }
      fnc xt->name "cursor" string= if
	snd j ( chn ) undef ( edpos ) cursor \ three arguments!
      else
	snd j ( chn ) fnc execute
      then
    end-map
  end-map array-set!
  snd new-state set-saved-state
  #f
;

: remember-sound-at-open <{ snd -- #f }>
  snd saved-state { state }
  remember-choice 2 <>
  state length && if
    snd file-name file-write-date state 1 array-ref equal?
    snd channels state 3 array-ref length = && if
      state 2 array-ref each { val }
	sound-funcs i array-ref { fnc }
	fnc xt->name "selected-channel" string= if
	  snd val set-selected-channel drop \ arguments swaped!
	else
	  val snd fnc set-execute drop
	then
      end-each
      snd channels 0 ?do
	#t snd i ( chn ) set-squelch-update drop
	state 3 array-ref i ( chn) array-ref each { val } \ channel-funcs values
	  channel-funcs i array-ref { fnc }
	  fnc xt->name "cursor" string= if
	    val snd j ( chn ) undef ( edpos ) set-cursor drop \ four arguments!
	  else
	    val snd j ( chn ) fnc set-execute drop
	  then
	end-each
	#f snd i ( chn ) set-squelch-update drop
	snd i ( chn ) time-graph?      if snd i update-time-graph      drop then
	snd i ( chn ) transform-graph? if snd i update-transform-graph drop then
      loop
    then
  then
  #f
;

: remember-sound-at-start <{ filename -- #f }>
  remember-states empty?
  remember-sound-filename file-exists? && if
    remember-sound-filename file-eval
    -saved-remember-sound-states-states- to remember-states
  then
  #f
;

: remember-sound-at-exit <{ -- #f }>
  \ The local variable IO must be defined here, not in the else
  \ branch.  ficlVmExecuteXT()/ficlVmInnerLoop() try to kill a local
  \ stack but no local stack exists if remember-states is empty and IO
  \ is defined in the else branch.  (I haven't seen this bug before.)
  nil { io }
  remember-states empty? if
    remember-sound-filename file-delete
  else
    remember-sound-filename io-open-write to io
    io $" \\ -*- snd-forth -*-\n" io-write
    io $" \\ from remember-sound-state in %s\n" _ '( *filename* ) io-write-format
    io $" \\ written: %s\n\n" _ '( date ) io-write-format
    io $" %s to -saved-remember-sound-states-states-\n\n"
    '( remember-states object-dump ) io-write-format
    io $" \\ %s ends here\n" _ '( remember-sound-filename #f file-basename ) io-write-format
    io io-close
  then
  #f
;
set-current

0 value remembering-sound-state

: remember-sound-state ( choice -- )
  doc" Remembers the state of a sound when it is closed, \
and if it is subsquently re-opened, restores that state."
  { choice }
  choice 0=				\ no remembering
  choice 1 = ||	if			\ just within-run remembering
    choice 0= if
      close-hook      ['] remember-sound-at-close remove-hook! drop
      after-open-hook ['] remember-sound-at-open  remove-hook! drop
    then
    open-hook        ['] remember-sound-at-start remove-hook! drop
    before-exit-hook ['] remember-sound-at-exit  remove-hook! drop
    remember-sound-filename file-delete
  then
  choice 0<> if
    close-hook      ['] remember-sound-at-close add-hook!
    after-open-hook ['] remember-sound-at-open  add-hook!
    choice 1 <> if
      open-hook        ['] remember-sound-at-start add-hook!
      before-exit-hook ['] remember-sound-at-exit  add-hook!
    then
  then
  choice to remember-choice
  choice to remembering-sound-state
;
previous

\ ;;; -------- mix-channel, insert-channel, c-channel

hide
: mc-cb { rd -- prc; y self -- val }
  1 proc-create rd , ( prc )
 does> { y self -- val }
  self @ ( rd ) next-sample y f+
;
set-current
: mix-channel <{ file-data :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Mixes in FILE-DATA.  \
FILE-DATA can be the file name or a list '( file-name [beg [channel]] )"
  file-data string? if file-data else file-data car then { file-name }
  file-name find-file to file-name
  file-name false? if 'no-such-file '( get-func-name file-name ) fth-throw then
  file-data string? file-data length 2 < || if 0 else file-data cadr  then { file-beg }
  file-data string? file-data length 3 < || if 0 else file-data caddr then { file-channel }
  dur file-name mus-sound-frames file-beg - || { len }
  beg 0< if 'no-such-sample '( get-func-name beg ) fth-throw then
  len 0> if
    file-beg file-name file-channel 1 #f make-sample-reader { reader }
    $" %S %s %s %s" '( file-data beg dur get-func-name ) string-format { origin }
    reader mc-cb beg len snd chn edpos origin map-channel
  else
    #f
  then
;
previous

: insert-channel <{ file-data :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Inserts the FILE-DATA.  \
FILE-DATA can be the file name or a list '( file-name [beg [channel]] )"
  file-data string? if file-data else file-data car then { file-name }
  file-name find-file to file-name
  file-name false? if 'no-such-file '( get-func-name file-name ) fth-throw then
  file-data string? file-data length 2 < || if 0 else file-data cadr  then { file-beg }
  file-data string? file-data length 3 < || if 0 else file-data caddr then { file-channel }
  dur file-name mus-sound-frames file-beg - || { len }
  beg 0< if 'no-such-sample '( get-func-name beg ) fth-throw then
  len 0> if
    file-beg file-name file-channel 1 #f make-sample-reader { reader }
    len 0.0 make-vct map! reader next-sample end-map { data }
    reader free-sample-reader drop
    $" %S %s %s %s" '( file-data beg dur get-func-name ) string-format { origin }
    beg len data snd chn edpos #f origin insert-samples
  else
    #f
  then
;

\ ;;; -------- any-env-channel

hide
: aec-cb { en func beg dur snd chn edpos -- prc; self -- val }
  0 proc-create en , func , beg , dur , snd , chn , edpos , ( prc )
 does> { self -- val }
  self           @ { en }
  self   cell+   @ { func }
  self 2 cells + @ { beg }
  self 3 cells + @ { dur }
  self 4 cells + @ { snd }
  self 5 cells + @ { chn }
  self 6 cells + @ { edpos }
  0.0 0.0 { x0 y0 }
  en car  { x1 }
  en cadr { y1 }
  en envelope-last-x en car f- { xrange }
  beg { ramp-beg }
  0 { ramp-dur }
  en length 1- 2 ?do
    x1 to x0
    y1 to y0
    en i    list-ref to x1
    en i 1+ list-ref to y1
    x1 x0 f- xrange f/ dur f* fround->s to ramp-dur
    y0 y1 f= if
      y0 ramp-beg ramp-dur snd chn edpos scale-channel
    else
      func '( y0 y1 ramp-beg ramp-dur snd chn edpos ) run-proc
    then
    ramp-dur +to ramp-beg
  2 +loop
;
set-current
: any-env-channel <{ en func :optional beg 0 dur #f snd #f chn #f edpos #f origin #f -- val }>
  en null? if
    #f
  else
    en envelope-length ( pts ) 1 = if
      en car beg dur snd chn edpos scale-channel
    else
      dur integer? unless snd chn #f frames to dur then
      en func beg dur snd chn edpos aec-cb origin as-one-edit
    then
  then
;
previous

\ ;;; -------- sine-ramp sine-env-channel 

hide
: sr3-cb <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  angle fcos f2/ 0.5 f+ data 3 vct-ref f* data 2 vct-ref f+ y f* ( val )
  data 0  forward angle incr if f+ else f- then  vct-set! drop
  ( val )
;
: sr2-cb { rmp0 rmp1 -- prc; frag-beg frag-dur self -- vct }
  2 proc-create rmp0 , rmp1 ,
 does> { frag-beg frag-dur self -- vct }
  self @ { rmp0 }
  self cell+ @ { rmp1 }
  pi frag-dur f/ { incr }
  vct( pi fnegate frag-beg incr f* f+
     incr
     rmp0
     rmp1 rmp0 f- )
;
set-current
: sine-ramp <{ rmp0 rmp1 :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Produces a sinsusoidal connection from RMP0 to RMP1."
  \ ;; vct: angle incr off scl
  $" %s %s %s %s %s" '( rmp0 rmp1 beg dur get-func-name ) string-format { origin }
  ['] sr3-cb beg dur snd chn edpos #t  rmp0 rmp1 sr2-cb  origin ptree-channel
;
previous

: sine-env-channel <{ en :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Connects ENV's dots with sinusoids."
  $" %s %s %s %s" '( en beg dur get-func-name ) string-format { origin }
  en ['] sine-ramp beg dur snd chn edpos origin any-env-channel
;
\ '( 0 0 1 1 2 -0.5 3 1 ) sine-env-channel

\ ;;; an obvious extension of this idea is to use the blackman fft window formulas
\ ;;;   to get sharper sinusoids (i.e. use the sum of n cosines, rather than just 1)
\ 
\ ;;; -------- blackman4-ramp, blackman4-env-channel

hide
: b4r3-cb <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  angle fcos { cx }
  cx 0.041194 f* -0.20762 f+ cx f* 0.375696 f+ cx f* -0.29145 f+ cx f* 0.084037 f+
  data 3 vct-ref f* data 2 vct-ref f+ y f* ( val )
  data 0  forward angle incr if f+ else f- then  vct-set! drop
  ( val )
;
: b4r2-cb { rmp0 rmp1 -- prc; frag-beg frag-dur self -- vct }
  2 proc-create rmp0 , rmp1 , ( prc )
 does> { frag-beg frag-dur self -- vct }
  self       @ { rmp0 }
  self cell+ @ { rmp1 }
  pi frag-dur f/ { incr }
  vct( frag-beg incr f*
     incr
     rmp0
     rmp1 rmp0 f- )
;
set-current
: blackman4-ramp <{ rmp0 rmp1 :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  $" %s %s %s %s %s" '( rmp0 rmp1 beg dur get-func-name ) string-format { origin }
  ['] b4r3-cb beg dur snd chn edpos #f rmp0 rmp1 b4r2-cb origin ptree-channel
;
previous

: blackman4-env-channel <{ en :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  $" %s %s %s %s" '( en beg dur get-func-name ) string-format { origin }
  en ['] blackman4-ramp beg dur snd chn edpos origin any-env-channel
;

\ ;;; any curve can be used as the connecting line between envelope breakpoints in the
\ ;;;   same manner -- set up each ramp to take the current position and increment,
\ ;;;   then return the value in ptree-channel.  A simple one would have a table of
\ ;;;   values and use array-interp.
\ 
\ ;;; -------- ramp-squared, env-squared-channel

hide
: rsq3-cb <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  angle dup f* data 3 vct-ref f* data 2 vct-ref f+ y f* ( val )
  data 0  forward angle incr if f+ else f- then  vct-set! drop
  ( val )
;
: rsq2-cb { rmp0 rmp1 symmetric -- prc; frag-beg frag-dur self -- vct }
  2 proc-create rmp0 , rmp1 , symmetric , ( prc )
 does> { frag-beg frag-dur self -- vct }
  self           @ { rmp0 }
  self cell+     @ { rmp1 }
  self 2 cells + @ { symmetric }
  frag-dur 1/f { incr }
  symmetric rmp1 rmp0 f< && if
    vct( frag-dur frag-beg f- incr f*  incr fnegate  rmp1  rmp0 rmp1 f- )
  else
    vct( frag-beg incr f*  incr  rmp0  rmp1 rmp0 f- )
  then
;
set-current
: ramp-squared <{ rmp0 rmp1 :optional symmetric #t beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Connects RMP0 and RMP1 with an x^2 curve."
  $" %s %s %s %s %s %s" '( rmp0 rmp1 symmetric beg dur get-func-name ) string-format { origin }
  ['] rsq3-cb beg dur snd chn edpos #t  rmp0 rmp1 symmetric rsq2-cb  origin ptree-channel
;
previous

hide
: esqc-cb { symmetric -- prc; r0 r1 b d s c e self -- val }
  7 proc-create symmetric , ( prc )
 does> { r0 r1 b d s c e self -- val }
  r0 r1 self @ ( symmetric ) b d s c e ramp-squared
;
set-current
: env-squared-channel <{ en :optional symmetric #t beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Connects ENV's dots with x^2 curves."
  $" %s %s %s %s %s" '( en symmetric beg dur get-func-name ) string-format { origin }
  en  symmetric esqc-cb beg dur snd chn edpos origin any-env-channel
;
previous
\ '( 0 0 1 1 2 -0.5 3 1 ) env-squared-channel

\ ;;; -------- ramp-expt, env-expt-channel


hide
: rex3-cb <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  data 4 vct-ref angle flog f* fexp data 3 vct-ref f* data 2 vct-ref f+ y f* ( val )
  data 0  forward angle incr if f+ else f- then  vct-set! drop
  ( val )
;
: rex2-cb { rmp0 rmp1 symmetric exponent -- prc; frag-beg frag-dur self -- vct }
  2 proc-create rmp0 , rmp1 , symmetric , exponent , ( prc )
 does> { frag-beg frag-dur self -- vct }
  self           @ { rmp0 }
  self cell+     @ { rmp1 }
  self 2 cells + @ { symmetric }
  self 3 cells + @ { exponent }
  frag-dur 1/f { incr }
  symmetric rmp1 rmp0 f< && if
    vct( frag-dur frag-beg f- incr f*  incr fnegate  rmp1  rmp0 rmp1 f-  exponent )
  else
    vct( frag-beg incr f*  incr  rmp0  rmp1 rmp0 f-  exponent )
  then
;
set-current
: ramp-expt <{ rmp0 rmp1 exponent
     :optional symmetric #t beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Connects RMP0 and RMP1 with an x^exponent curve."
  \ ;; vct: start incr off scl exponent
  \ ;; a^x = exp(x * log(a))
  $" %s %s %s %s %s %s %s"
  '( rmp0 rmp1 exponent symmetric beg dur get-func-name ) string-format { origin }
  ['] rex3-cb beg dur snd chn edpos #t  rmp0 rmp1 symmetric exponent rex2-cb origin ptree-channel
;
previous

hide
: expc-cb { symmetric exponent -- prc; r0 r1 b d s c e self -- val }
  7 proc-create symmetric , exponent , ( prc )
 does> { r0 r1 b d s c e self -- val }
  r0 r1 self cell+ @ ( exponent ) self @ ( symmetric ) b d s c e ramp-expt
;
set-current
: env-expt-channel <{ en exponent
     :optional symmetric #t beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Connects ENV's dots with x^exponent curves."
  $" %s %s %s %s %s %s" '( en exponent symmetric beg dur get-func-name ) string-format { origin }
  en  symmetric exponent expc-cb beg dur snd chn edpos origin any-env-channel
;
previous

\ ;;; -------- offset-channel

hide
: offc-cb { dc -- prc; y self -- val }
  1 proc-create dc , ( prc )
 does> { y self -- val }
  self @ ( dc ) y f+
;
set-current
: offset-channel <{ amount :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Adds AMOUNT to each sample."
  $" %s %s %s %s" '( amount beg dur get-func-name ) string-format { origin }
  amount offc-cb beg dur snd chn edpos #t #f origin ptree-channel
;
previous

: offset-sound <{ offset :optional beg 0 dur #f snd #f -- }>
  doc" Adds OFFSET to every sample in SND."
  snd snd-snd to snd
  snd sound? if
    snd channels 0 ?do offset beg dur snd i ( chn ) #f offset-channel drop loop
  else
    'no-such-sound '( get-func-name snd ) fth-throw
  then
;

\ ;;; -------- dither-channel

hide
: dith-cb { dither -- prc; y self -- val }
  1 proc-create dither , ( prc )
 does> { y self -- val }
  self @ ( dither ) dup mus-random swap mus-random f+ y f+
;
set-current
: dither-channel <{ :optional amount 0.00006 beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Adds AMOUNT dither to each sample."
  $" %s %s %s %s" '( amount beg dur get-func-name ) string-format { origin }
  amount f2/ dith-cb beg dur snd chn edpos #t #f origin ptree-channel
;

: dither-sound <{ :optional amount 0.00006 beg 0 dur #f snd #f -- }>
  doc" Adds dithering to every sample of SND."
  snd snd-snd to snd
  snd sound? if
    snd channels 0 ?do amount beg dur snd i ( chn ) #f dither-channel drop loop
  else
    'no-such-sound '( get-func-name snd ) fth-throw
  then
;

\ ;;; -------- contrast-channel

hide
: cntr-cb { index -- prc; y self -- val }
  1 proc-create index , ( prc )
 does> { y self -- val }
  y two-pi f* fsin self @ ( index ) f*  y half-pi f*  f+ fsin
;
set-current
: contrast-channel <{ index :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Applies contrast enhancement to the sound."
  $" %s %s %s %s" '( index beg dur get-func-name ) string-format { origin }
  index cntr-cb beg dur snd chn edpos #f #f origin ptree-channel
;

: contrast-sound <{ index :optional beg 0 dur #f snd #f -- }>
  doc" Applies contrast-enhancement to every channel of SND."
  snd snd-snd to snd
  snd sound? if
    snd channels 0 ?do index beg dur snd i ( chn ) #f contrast-channel drop loop
  else
    'no-such-sound '( get-func-name snd ) fth-throw
  then
;

\ === PREFERENCES DIALOG ===

\ --- focus-follows-mouse ---

hide
: channel-xt ( snd chn -- xt; self -- val )
  swap lambda-create , , latestxt
 does> ( self -- val )
  { self }
  self @ ( snd )
  self cell+ @ ( chn ) channel-widgets
;

: graph-hook-cb <{ snd chn -- val }>
  snd sound? if
    snd chn channel-xt 'no-such-channel nil fth-catch false? if car focus-widget then
  else
    #f
  then
;
set-current

#f value focus-is-following-mouse	\ for prefs

: focus-follows-mouse ( -- )
  focus-is-following-mouse unless
    mouse-enter-graph-hook    ['] graph-hook-cb add-hook!
    mouse-enter-listener-hook ['] focus-widget  add-hook!
    mouse-enter-text-hook     ['] focus-widget  add-hook!
    #t to focus-is-following-mouse
  then
;
previous

\ --- initial bounds ---

#f  value prefs-show-full-duration	\ for prefs
0.0 value prefs-initial-beg
0.1 value prefs-initial-dur

hide
: bounds-cb <{ snd chn dur -- lst }>
  '( prefs-initial-beg prefs-show-full-duration if dur else prefs-initial-dur dur fmin then )
;
set-current
: prefs-activate-initial-bounds ( beg dur full -- )
  { beg dur full }
  beg to prefs-initial-beg
  dur to prefs-initial-dur
  full to prefs-show-full-duration
  initial-graph-hook ['] bounds-cb add-hook!
;

: prefs-deactivate-initial-bounds ( -- )
  0.0 to prefs-initial-beg
  0.1 to prefs-initial-dur
  #f  to prefs-show-full-duration
  initial-graph-hook ['] bounds-cb remove-hook! drop
;
previous

\ --- reopen menu ---

hide
"empty" _ value reopen-empty
#() value reopen-names
#f  value reopen-menu
16  value reopen-max-length
' noop 0 make-proc constant extensions-noop

: reopen-select-cb { brief-name long-name -- proc; self -- }
  0 proc-create long-name , brief-name ,
 does> ( self -- )
  { self }
  self @ { long-name }
  self cell+ @ { brief-name }
  reopen-menu brief-name remove-from-menu drop
  long-name file-exists? if long-name open-sound drop then
;

: add-to-reopen-menu <{ snd -- #f }>
  snd short-file-name { brief-name }
  snd file-name { long-name }
  reopen-names brief-name array-member? unless
    reopen-names reopen-empty array-member? if
      reopen-menu reopen-empty remove-from-menu drop
      #() to reopen-names
    then
    reopen-menu brief-name brief-name long-name reopen-select-cb 0 add-to-menu drop
    reopen-names brief-name array-push drop
    reopen-names length reopen-max-length > if
      reopen-menu reopen-names array-shift remove-from-menu drop
    then
  then
  #f
;

: check-reopen-menu <{ file -- }>
  file #f file-basename { brief-name }
  reopen-names brief-name array-member? if
    reopen-menu brief-name remove-from-menu drop
    reopen-names   reopen-names brief-name array-index   array-delete! drop
  then
  reopen-names empty? if
    reopen-menu reopen-empty extensions-noop undef add-to-menu drop
    reopen-names reopen-empty array-push drop
  then
;
set-current

#f value including-reopen-menu		\ for prefs

: with-reopen-menu ( -- )
  including-reopen-menu unless
    #() to reopen-names
    reopen-menu false? if "Reopen" _ extensions-noop add-to-main-menu to reopen-menu then
    reopen-menu reopen-empty extensions-noop 0 add-to-menu drop
    reopen-names reopen-empty array-push drop
    #t to including-reopen-menu
    close-hook ['] add-to-reopen-menu add-hook!
    open-hook  ['] check-reopen-menu  add-hook!
  then
;
previous

\ --- buffers menu ---

hide
"empty" _ value buffer-empty
#() value buffer-names
#f  value buffer-menu

: buffer-select-cb ( file -- proc ; self -- )
  0 proc-create swap ,
 does> ( self -- )
  { self }
  self @ ( file ) 0 find-sound dup sound? if select-sound then drop
;

: open-buffer <{ file -- }>
  buffer-names buffer-empty array-member? if
    buffer-menu buffer-empty remove-from-menu drop
    #() to buffer-names
  then
  buffer-menu file file buffer-select-cb -1 add-to-menu drop
  buffer-names file array-push drop
;

: close-buffer <{ snd -- #f }>
  buffer-menu snd file-name remove-from-menu drop
  buffer-names   buffer-names snd file-name array-index   array-delete! drop
  buffer-names empty? if
    buffer-menu buffer-empty extensions-noop 0 add-to-menu drop
    buffer-names buffer-empty array-push drop
  then
  #f
;
set-current

#f value including-buffers-menu		\ for prefs

: with-buffers-menu ( -- )
  including-buffers-menu unless
    #() to buffer-names
    buffer-menu false? if "Buffers" _ extensions-noop add-to-main-menu to buffer-menu then
    buffer-menu buffer-empty extensions-noop 0 add-to-menu drop
    buffer-names buffer-empty array-push drop
    #t to including-buffers-menu
    open-hook  ['] open-buffer  add-hook!
    close-hook ['] close-buffer add-hook!
  then
;
previous

\ --- global sync (for prefs dialog) ---
0 value global-sync-choice		\ global var so that we can reflect
					\ the current setting in prefs dialog
: global-sync-cb <{ snd -- val }>
  global-sync-choice 1 = if
    1 snd set-sync
  else
    global-sync-choice 2 = if
      sync-max 1+ snd set-sync
    else
      #f
    then
  then
;

: set-global-sync ( choice -- )
  { choice }
  choice to global-sync-choice
  choice 0<> if
    after-open-hook ['] global-sync-cb hook-member? unless
      after-open-hook ['] global-sync-cb add-hook!
    then
  then
;

\ ;;; -------- show-selection

: show-selection <{ -- }>
  selection? if
    #f #f { beg end }
    sounds each { snd }
      snd channels 0 ?do
	snd i ( chn ) selection-member? if
	  snd i selection-position snd srate f/ { pos }
	  snd i selection-frames   snd srate f/ { len }
	  beg not  pos        beg f< || if pos        to beg then
	  end not  pos len f+ end f> || if pos len f+ to end then
	then
      loop
    end-each
    sounds each { snd }
      snd channels 0 ?do
	'( beg end ) snd i set-x-bounds drop
      loop
    end-each
  then
;

\ extensions.fs ends here
