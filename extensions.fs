\ -*- snd-forth -*-
\ extensions.fs -- extensions.scm -> extensions.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sun Dec 18 19:21:00 CET 2005
\ Changed: Mon Dec 11 23:07:15 CET 2006

\ Commentary:
\ 
\ Snd-7 compatibility
\ 
\ color-map constants
\ mus-a0, set-mus-a0, etc
\ back-or-forth-graph 			 ( count -- lst )
\ forward-graph       			 ( count -- lst )
\ backward-graph      			 ( count -- lst )
\ back-or-forth-mix   			 ( count snd chn -- mx )
\ forward-mix         			 ( count snd chn -- mx )
\ backward-mix        			 ( count snd chn -- mx )
\ back-or-forth-mark  			 ( count snd chn -- mk )
\ forward-mark        			 ( count snd chn -- mk )
\ backward-mark       			 ( count snd chn -- mk )
\ mus-bank            			 ( gens amps in1 in2 -- val )
\ oscil-bank          			 ( amps gens in1 in2 -- val )
\
\ sound-property       		  	 ( key snd -- value|#f )
\ set-sound-property   		  	 ( key val snd -- )
\ set-sound-property-save-state-ignore   ( key snd -- )
\ channel-property     		  	 ( key snd chn -- value|#f )
\ set-channel-property 		  	 ( key val snd chn -- )
\ set-channel-property-save-state-ignore ( key snd chn -- )
\
\ selection-members                      ( -- array of lists )
\
\ yes-or-no?                      	 ( question action-if-yes action-if-no snd -- )
\ check-for-unsaved-edits         	 ( check -- )
\ remember-sound-state            	 ( choice -- )
\
\ focus-follows-mouse                    ( -- )
\ prefs-activate-initial-bounds   	 ( beg dur full -- )
\ prefs-deactivate-initial-bounds 	 ( -- )
\ with-reopen-menu     		  	 ( -- )
\ with-buffers-menu    		  	 ( -- )
\ set-global-sync                 	 ( choice -- )

\ Code:

require clm
require examp

\ === Snd-7 compatibility stuff ===
: mus-a0     ( gen     -- val ) 0 mus-xcoeff ;
: set-mus-a0 ( gen val -- val ) 0 swap set-mus-xcoeff ;
: mus-a1     ( gen     -- val ) 1 mus-xcoeff ;
: set-mus-a1 ( gen val -- val ) 1 swap set-mus-xcoeff ;
: mus-a2     ( gen     -- val ) 2 mus-xcoeff ;
: set-mus-a2 ( gen val -- val ) 2 swap set-mus-xcoeff ;
: mus-b1     ( gen     -- val ) 1 mus-ycoeff ;
: set-mus-b1 ( gen val -- val ) 1 swap set-mus-ycoeff ;
: mus-b2     ( gen     -- val ) 2 mus-ycoeff ;
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
: back-or-forth-mix ( count snd chn -- mx|#f )
  { count snd chn }
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
: forward-mix ( count snd chn -- mx|#f )
  { count snd chn }
  count snd snd-snd chn snd-chn back-or-forth-mix
;
: backward-mix ( count snd chn -- mx|#f )
  { count snd chn }
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
: back-or-forth-mark ( count snd chn -- mk|#f )
  { count snd chn }
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
: forward-mark ( count snd chn -- mk|#f )
  { count snd chn }
  count snd snd-snd chn snd-chn back-or-forth-mark
;
: backward-mark ( count snd chn -- mk|#f )
  { count snd chn }
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

: sound-property { key snd -- val }
  doc" ( key snd -- value|#f )  \
Returns the value associated with KEY in the given sound's property list, or #f."
  snd sound-properties key list-assoc-ref
;

: set-sound-property { key val snd -- alist }
  doc" ( key val snd -- alist )  Sets key-value pair in the given sound's property list.  \
If KEY exists, VAL overwrites the old value."
  snd sound-properties key val list-assoc-set! snd set-sound-properties
;

: set-sound-property-save-state-ignore ( key snd -- alist )
  { key snd }
  'save-state-ignore
  key
  'save-state-ignore snd sound-property dup false? if
    drop
    'save-state-ignore '() cons
  then cons
  snd set-sound-property
;

: channel-property { key snd chn -- val }
  doc" ( key snd chn -- value|#f )  \
Returns the value associated with KEY in the given channel's property list, or #f."
  snd chn channel-properties key list-assoc-ref
;

: set-channel-property { key val snd chn -- alist }
  doc" ( key val snd chn -- alist )  Sets key-value pair in the given channel's property list.  \
If KEY exists, VAL overwrites the old value."
  snd chn channel-properties key val list-assoc-set! snd chn set-channel-properties
;

: set-channel-property-save-state-ignore ( key snd chn -- alist )
  { key snd chn }
  'save-state-ignore
  key
  'save-state-ignore snd chn channel-property dup false? if
    drop
    'save-state-ignore '() cons
  then cons
  snd chn set-channel-property
;

: selection-members ( -- array-of-lists )
  doc" ( -- array-of-lists )  Array of lists of '( snd chn ) indicating the channels \
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
: response-cb ( snd action-if-yes action-if-no -- proc; response self -- val )
  lambda-create , , , latestxt 1 make-proc
 does> ( response self -- val )
  { response self }
  self @ { action-if-no }
  self cell+ @ { action-if-yes }
  self 2 cells + @ { snd }
  snd clear-minibuffer drop
  response 0 string-ref [char] y = if
    snd action-if-yes execute
  else
    snd action-if-no execute
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
: yes-xt ( exiting -- xt; snd self -- val )
  lambda-create , latestxt
 does> ( snd self -- val )
  { snd self }
  snd revert-sound drop
  self @ ( exiting ) if 0 snd-exit then
  #t
;
lambda: ( snd -- val ) drop #f ; value no-xt

: ignore-unsaved-edits-at-close? ( snd exiting -- f )
  { snd exiting }
  #t
  snd channels 0 ?do
    snd i edits car 0> if
      $" %s[%d] has unsaved edits.  Close (y/n)? " _ '( snd short-file-name i ) string-format
      exiting yes-xt no-xt snd yes-or-no?
      not
    then
  loop
;

: unsaved-edits-at-close ( snd -- f ) #f ignore-unsaved-edits-at-close? not ;
: unsaved-edits-at-exit ( -- f )
  #f sounds each #t ignore-unsaved-edits-at-close? unless not leave then end-each
;
set-current

#f value checking-for-unsaved-edits

: check-for-unsaved-edits { check -- }
  doc" ( check -- )  \
Sets up hooks to check for and ask about unsaved edits when a sound is closed.  \
If CHECK is #f, the hooks are removed."
  check if
    before-close-hook ['] unsaved-edits-at-close 1 make-proc add-hook!
    before-exit-hook  ['] unsaved-edits-at-exit  0 make-proc add-hook!
  else
    before-close-hook ['] unsaved-edits-at-close remove-hook! drop
    before-exit-hook  ['] unsaved-edits-at-exit  remove-hook! drop
  then
  check to checking-for-unsaved-edits
;
previous

\ --- remember-sound-state ---

$" .snd-remember-sound" value remember-sound-filename
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

: remember-sound-at-close ( snd -- #f )
  { snd }
  4 nil make-array { new-state }
  new-state 0 snd file-name array-set!
  new-state 1 snd file-name file-write-date array-set!
  new-state 2 sound-funcs map snd *key* execute end-map array-set!
  new-state 3 snd channels nil make-array map!
    channel-funcs map *key* { fnc }
      fnc xt->name $" cursor" string= if
	snd j ( chn ) undef ( edpos ) cursor \ three arguments!
      else
	snd j ( chn ) fnc execute
      then
    end-map
  end-map array-set!
  snd new-state set-saved-state
  #f
;

: remember-sound-at-open ( snd -- #f )
  { snd }
  snd saved-state { state }
  remember-choice 2 <>
  state length && if
    snd file-name file-write-date state 1 array-ref equal?
    snd channels state 3 array-ref length = && if
      state 2 array-ref each { val }
	sound-funcs i array-ref { fnc }
	fnc xt->name $" selected-channel" string= if
	  snd val set-selected-channel drop \ arguments swaped!
	else
	  val snd fnc set-execute drop
	then
      end-each
      snd channels 0 ?do
	#t snd i ( chn ) set-squelch-update drop
	state 3 array-ref i ( chn) array-ref each { val } \ channel-funcs values
	  channel-funcs i array-ref { fnc }
	  fnc xt->name $" cursor" string= if
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

: remember-sound-at-start ( filename -- #f )
  { filename }
  remember-states empty?
  remember-sound-filename file-exists? && if
    remember-sound-filename file-eval
    -saved-remember-sound-states-states- to remember-states
  then
  #f
;

: remember-sound-at-exit ( -- #f )
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

: remember-sound-state { choice -- }
  doc" ( choice -- )  Remembers the state of a sound when it is closed, \
and if it is subsquently re-opened, restores that state."
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
    close-hook      ['] remember-sound-at-close 1 make-proc add-hook!
    after-open-hook ['] remember-sound-at-open  1 make-proc add-hook!
    choice 1 <> if
      open-hook        ['] remember-sound-at-start 1 make-proc add-hook!
      before-exit-hook ['] remember-sound-at-exit  0 make-proc add-hook!
    then
  then
  choice to remember-choice
  choice to remembering-sound-state
;
previous

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

: graph-hook-cb ( snd chn -- val )
  { snd chn }
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
    mouse-enter-graph-hook    ['] graph-hook-cb 2 make-proc add-hook!
    mouse-enter-listener-hook ['] focus-widget  1 make-proc add-hook!
    mouse-enter-text-hook     ['] focus-widget  1 make-proc add-hook!
    #t to focus-is-following-mouse
  then
;
previous

\ --- initial bounds ---

#f  value prefs-show-full-duration	\ for prefs
0.0 value prefs-initial-beg
0.1 value prefs-initial-dur

hide
: bounds-cb ( snd chn dur -- lst )
  { snd chn dur }
  '( prefs-initial-beg prefs-show-full-duration if dur else prefs-initial-dur dur fmin then )
;
set-current
: prefs-activate-initial-bounds ( beg dur full -- )
  { beg dur full }
  beg to prefs-initial-beg
  dur to prefs-initial-dur
  full to prefs-show-full-duration
  initial-graph-hook ['] bounds-cb 3 make-proc add-hook!
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
$" empty" _ value reopen-empty
#() value reopen-names
#f  value reopen-menu
16  value reopen-max-length

: reopen-select-cb ( brief-name long-name -- proc; self -- )
  lambda-create , , latestxt 0 make-proc
 does> ( self -- )
  { self }
  self @ { long-name }
  self cell+ @ { brief-name }
  reopen-menu brief-name remove-from-menu drop
  long-name file-exists? if long-name open-sound drop then
;

: add-to-reopen-menu ( snd -- #f )
  { snd }
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

: check-reopen-menu ( file -- )
  { file }
  file #f file-basename { brief-name }
  reopen-names brief-name array-member? if
    reopen-menu brief-name remove-from-menu drop
    reopen-names   reopen-names brief-name array-index   array-delete! drop
  then
  reopen-names empty? if
    reopen-menu reopen-empty ['] noop 0 make-proc 0 add-to-menu drop
    reopen-names reopen-empty array-push drop
  then
;
set-current

#f value including-reopen-menu		\ for prefs

: with-reopen-menu ( -- )
  including-reopen-menu unless
    #() to reopen-names
    reopen-menu false? if $" Reopen" _ ['] noop 0 make-proc add-to-main-menu to reopen-menu then
    reopen-menu reopen-empty ['] noop 0 make-proc 0 add-to-menu drop
    reopen-names reopen-empty array-push drop
    #t to including-reopen-menu
    close-hook ['] add-to-reopen-menu 1 make-proc add-hook!
    open-hook  ['] check-reopen-menu  1 make-proc add-hook!
  then
;
previous

\ --- buffers menu ---

hide
$" empty" _ value buffer-empty
#() value buffer-names
#f  value buffer-menu

: buffer-select-cb ( file -- proc ; self -- )
  lambda-create , latestxt 0 make-proc
 does> ( self -- )
  { self }
  self @ ( file ) 0 find-sound dup sound? if select-sound then drop
;

: open-buffer ( file -- )
  { file }
  buffer-names buffer-empty array-member? if
    buffer-menu buffer-empty remove-from-menu drop
    #() to buffer-names
  then
  buffer-menu file file buffer-select-cb -1 add-to-menu drop
  buffer-names file array-push drop
;

: close-buffer ( snd -- #f )
  { snd }
  buffer-menu snd file-name remove-from-menu drop
  buffer-names   buffer-names snd file-name array-index   array-delete! drop
  buffer-names empty? if
    buffer-menu buffer-empty ['] noop 0 make-proc 0 add-to-menu drop
    buffer-names buffer-empty array-push drop
  then
  #f
;
set-current

#f value including-buffers-menu		\ for prefs

: with-buffers-menu ( -- )
  including-buffers-menu unless
    #() to buffer-names
    buffer-menu false? if $" Buffers" _ ['] noop 0 make-proc add-to-main-menu to buffer-menu then
    buffer-menu buffer-empty ['] noop 0 make-proc 0 add-to-menu drop
    buffer-names buffer-empty array-push drop
    #t to including-buffers-menu
    open-hook  ['] open-buffer  1 make-proc add-hook!
    close-hook ['] close-buffer 1 make-proc add-hook!
  then
;
previous

\ --- global sync (for prefs dialog) ---
0 value global-sync-choice		\ global var so that we can reflect
					\ the current setting in prefs dialog
: global-sync-cb ( snd -- val )
  { snd }
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
      after-open-hook ['] global-sync-cb 1 make-proc add-hook!
    then
  then
;

\ extensions.fs ends here
