\ snd-test.fs -- snd-test.scm|rb tests -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sat Aug 05 00:09:28 CEST 2006
\ Changed: Fri Jan 05 22:52:12 CET 2007

\ Commentary:
\
\ snd-forth -noinit -load snd-test.fs
\
\ test -1: general
\ test 10: marks
\ test 15: chan-local vars
\ test 19: save and restore
\ test 23: with-sound

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]
'snd-gtk   provided? 'xg provided? not && [if] dl-load libxg Init_libxg [then]

require clm
require clm-ins
require examp
require hooks
require marks
require extensions
require env
require peak-env
require mix
require dsp
'snd-motif provided? [if]
  require snd-xm
  require effects
[then]

#t to *fth-verbose*
#f to *fth-debug*
#t to *clm-verbose*
#t to *clm-debug*

file-pwd "/peaks" $+ to save-peak-env-info-directory
save-peak-env-info-directory file-directory? [unless]
  save-peak-env-info-directory 0o755 file-mkdir
[then]
*clm-search-list* file-pwd array-push drop

save-dir "/zap/snd" || value original-save-dir
temp-dir "/zap/tmp" || value original-temp-dir
listener-prompt        value original-prompt
300                    value sample-reader-tests
1024 8 *               value default-file-buffer-size
"HOME" getenv          value home-dir
"/home/bil/sf1/"       value sf-dir
#t  		       value with-exit

\ SND-INFO:    Wraps \ and \n around text.
\ SND-DISPLAY: Wraps text like snd-info but prepends text with line number.
'snd-nogui provided? [if]
  #t set-with-mix-tags drop
  ' noop alias hide-widget
  : <snd-info> { fmt args -- msg }
    $" \\ %s\n" '( fmt args string-format ) string-format
  ;
  : <snd-display> { fmt args lineno -- msg }
    $" \\ [%d] %s\n" '( lineno fmt args string-format ) string-format
  ;
  : snd-info      ( fmt args -- )        <snd-info>    .stdout ;
  : (snd-display) ( fmt args lineno -- ) <snd-display> .stdout ;
[else]
  : <snd-info> { fmt args -- msg }
    $" \n\\ %s " '( fmt args string-format ) string-format
  ;
  : <snd-display> { fmt args lineno -- msg }
    $" \n\\ [%d] %s " '( lineno fmt args string-format ) string-format
  ;
  : snd-info      ( fmt args -- )        <snd-info>    dup ( msg ) .string ( msg ) .stdout ;
  : (snd-display) ( fmt args lineno -- ) <snd-display> dup ( msg ) .string ( msg ) .stdout ;
[then]
: snd-display ( --; fmt args -- ) postpone *lineno* postpone (snd-display) ; immediate

: mus-audio-playback-amp ( -- val )
  32 0.0 make-vct { vals }
  mus-audio-default mus-audio-amp 0 vals mus-audio-mixer-read drop
  vals 0 vct-ref
;
: set-mus-audio-playback-amp ( val -- )
  { val }
  32 0.0 make-vct { vals }
  vals 0 val vct-set! drop
  mus-audio-default mus-audio-amp 0 vals mus-audio-mixer-write drop
;
0.0                    value audio-amp-zero
mus-audio-playback-amp value original-audio-amp

: fneq-err ( r1 r2 err -- f ) -rot f- fabs f<= ;
: cneq-err ( c1 c2 err -- f )
  { c1 c2 err }
  c1 real-ref  c2 real-ref  err fneq-err
  c1 image-ref c2 image-ref err fneq-err ||
;
: fneq   ( a b -- f ) 0.001 fneq-err ;
: ffneq  ( a b -- f ) 0.01  fneq-err ;
: fffneq ( a b -- f ) 0.1   fneq-err ;
: cneq   ( a b -- f ) 0.001 cneq-err ;
: any->vct ( obj )
  { obj }
  obj vct? if
    obj
  else obj array? if
      obj vector->vct
    else obj list? if
	obj list->vct
      else
	#f
      then
    then
  then
;
: vequal-err ( v0 v1 err -- f )
  { val0 val1 err }
  val0 any->vct { v0 }
  val1 any->vct { v1 }
  v0 v1 && if
    v0 vct-copy v1 vct-subtract! vct-peak err f<=
  else
    #f
  then
;
: vequal    ( v0 v1 -- f ) 0.001   vequal-err ;
: vvequal   ( v0 v1 -- f ) 0.00002 vequal-err ;
: vfequal   ( v0 v1 -- f ) 0.01    vequal-err ;
: vffequal  ( v0 v1 -- f ) 0.1     vequal-err ;
: vfffequal ( v0 v1 -- f ) 0.5     vequal-err ;
: feql-err ( obj0 obj1 err -- f )
  { obj0 obj1 err }
  obj0 object-length obj1 object-length = if
    #t ( f ) obj0 each ( r0 ) obj1 i object-ref ( r1 ) err fneq-err if not leave then end-each
  else
    #f
  then
;
: feql   ( obj0 obj1 -- f ) 0.001 feql-err ;
: ffeql  ( obj0 obj1 -- f ) 0.01  feql-err ;
: fffeql ( obj0 obj1 -- f ) 0.1   feql-err ;
: fveql ( v1 v2 idx -- f)
  { v1 v2 idx }
  #t v1 length v2 length min idx ?do
    v1 i object-ref v2 i object-ref fneq if not leave then
  loop
;
: arity-ok ( proc args -- f )
  { proc args }
  proc proc-arity { args-lst }
  args-lst car args-lst cadr + { rargs }
  rargs args =
  rargs args < args-lst caddr && ||
;
: set-arity-ok ( proc args -- f )
  { proc args }
  proc proc-arity { args-lst }
  args-lst car args-lst cadr + 1+ { rargs }
  rargs args =
  rargs args < args-lst caddr && ||
;
: safe-divide ( a b -- c )
  { a b }
  b zero? b fzero? || if a else a b b/ then
;
: start-snd-test ( -- )
  ".sndtest-forth-rc" load-init-file
  'snd-motif provided? if
    "snd-motif"
  else
    'snd-gtk provided? if
      "snd-gtk"
    else
      'snd-nogui provided? if
	"snd-nogui"
      else
	"snd-unknown"
      then
    then
  then { kind }
  $" === Snd version: %s (%s)" '( snd-version kind ) snd-info
  $" === Fth version: %s"      '( fth-version )      snd-info
  ""   '() snd-info
  date '() snd-info
  ""   '() snd-info
;
: finish-snd-test ( -- )
  stack-reset
  #t show-listener drop
  sounds if stop-playing drop then
  0 { file-count }
  #( original-save-dir original-temp-dir "/tmp" ) each { path }
    path file-directory? if
      path file-dir each { file }
	/snd_/ file regexp-match if
	  file file-delete
	  1 +to file-count
	then
      end-each
    then
  end-each
  "" '() snd-info
  $" %d files deleted" '( file-count ) snd-info
  "" '() snd-info
  #( "aaa.eps"
     "envs.save"
     "fmv.snd"
     "fmv.wav"
     "fmv0.snd"
     "fmv1.snd"
     "fmv2.snd"
     "fmv3.snd"
     "fmv4.reverb"
     "fmv4.snd"
     "hiho.marks"
     "hiho.snd"
     "hiho.snd"
     "hiho.tmp"
     "hiho.wave"
     "ho"
     "new.snd"
     "oboe.marks"
     "obtest.snd.stereo"
     "snd.eps"
     "test-1.snd"
     "test-2.snd"
     "test-macros.scm"
     "test.aiff"
     "test.data"
     "test.rev"
     "test.reverb"
     "test.snd"
     "test.snd.snd"
     "test.wav"
     "test.xpm"
     "test2.snd"
     "test3.snd"
     "tmp.snd"
     "with-mix.snd"
     "1"
     "gtk-errors"
     "accelmap" ) each ( file ) file-delete end-each
  #( "mus10.snd.snd"
     "ieee-text-16.snd.snd"
     "trumps22.adp.snd"
     "oki.wav.snd"
     "nasahal.avi.snd"
     "hcom-16.snd.snd"
     "ce-c3.w02.snd"
     "oboe.g723_24.snd"
     "oboe.g723_40.snd"
     "oboe.g721.snd"
     "wood.sds.snd"
     "o2_dvi.wave.snd"
     "nist-shortpack.wav.snd"
     "bad_data_format.snd.snd" ) each ( file ) sf-dir swap $+ file-delete end-each
  "test-forth.output" save-listener drop
  original-audio-amp set-mus-audio-playback-amp
  original-prompt set-listener-prompt drop
;

#() value timer-values
#() value test-numbers
: run-fth-test ( xt -- )
  { xt }
  xt xt->name { name }
  test-numbers  name 0 1 string-substring string->number  array-member? if
    stack-reset
    make-timer { tm }
    tm start-timer
    xt execute
    tm stop-timer
    stack-reset
    timer-values '( name tm ) array-push drop
    sounds if
      $" open sounds: %s" '( #t short-file-name ) snd-info
      sounds each ( snd ) close-sound drop end-each
    then
    $" %s done\n\\ " '( name ) snd-info
  then
;
: fth-test-timer-inspect ( name tm -- ) 2 >list $" %21s: %s" swap snd-info ;

'complex provided? [if]
  : complex-test ( -- )
    \ edot-product (test008)
    0.0 vct( 1.0 ) edot-product dup 1.0 fneq if
      1 >list $" edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 vct( 0.0 ) edot-product dup 0.0 fneq if
      1 >list $" edot 0.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 1.0 ) edot-product dup 1.0 fneq if
      1 >list $" edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 0+1i ) edot-product dup 0+1i cneq if
      1 >list $" edot i: %s?" swap snd-display
    else
      drop
    then
    0.25 two-pi f* vct( 1.0 1.0 1.0 1.0 ) edot-product
    0.00 two-pi f* fexp
    0.25 two-pi f* fexp f+
    0.50 two-pi f* fexp f+
    0.75 two-pi f* fexp f+ over over fneq if
      2 >list $" edot 4: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1.0 2.0 3.0 4.0 ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1 c* 
    0.25 two-pi f* 0-1i c* cexp 2 c* c+
    0.50 two-pi f* 0-1i c* cexp 3 c* c+
    0.75 two-pi f* 0-1i c* cexp 4 c* c+ over over cneq if
      2 >list $" edot 4 -i: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1+1i 2+1i 3+1i 4+1i ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1+1i c* 
    0.25 two-pi f* 0-1i c* cexp 2+1i c* c+
    0.50 two-pi f* 0-1i c* cexp 3+1i c* c+
    0.75 two-pi f* 0-1i c* cexp 4+1i c* c+ over over cneq if
      2 >list $" edot 4 -i * i: %s %s?" swap snd-display
    else
      2drop
    then
  ;
[else]
  ' noop alias complex-test
[then]

: print-and-check ( gen name desc -- )
  { gen name desc }
  gen mus-name name string<> if $" mus-name %s: %s?" '( name gen mus-name ) snd-display then
  gen mus-describe desc string<> if $" mus-describe %s: %s?" '( name gen ) snd-display then
  gen { egen }
  gen egen object-equal? unless $" equal? %s: %s %s?" '( name gen egen ) snd-display then
;

\ === TEST -1 (a selection from snd-test.scm) ===

: test-gen-equal ( g0 g1 g2 -- )
  { g0 g1 g2 }
  \ g0 g1 =
  \ g0 g2 <> at start
  g0 { g3 }
  2 make-frame { gad }
  g0 g3 object-equal? unless $" let %s: %s equal? %s?" '( g0 mus-name g0 g3 ) snd-display then
  g0 g1 object-equal? unless $" %s: %s equal? %s?"     '( g0 mus-name g0 g1 ) snd-display then
  g0 g2 object-equal?     if $" %s: %s equal? %s?"     '( g0 mus-name g0 g2 ) snd-display then
  g0 gad object-equal?    if $" %s/frame: %s equal? %s?" '( g0 mus-name g0 gad ) snd-display then
  g0 0.0 0.0 mus-apply drop
  g3 '( 0.0 0.0 ) object-apply drop
  g3 0.0 0.0 mus-apply drop
  g0 g3 object-equal? unless $" run let %s: %s equal? %s?" '( g0 mus-name g0 g3 ) snd-display then
  g0 g1 object-equal?     if $" run %s: %s equal? %s?"     '( g0 mus-name g0 g1 ) snd-display then
  g0 g2 object-equal?     if $" run %s: %s equal? %s?"     '( g0 mus-name g0 g2 ) snd-display then
;

\ bind-key xt
: C-xC-c <{ -- f }> 0 snd-exit #f ;

\ hooks
: my-test1-proc <{ fname -- f }> #f ;
: my-test2-proc <{ fname -- f }> #f ;
: my-test3-proc <{ fname -- f }> #f ;
: my-test4-proc <{ fname -- f }> #f ;
: my-test5-proc <{ fname -- f }> #f ;
: my-local-thunk <{ -- }>
  open-hook object-length 3 <> if
    $" add-hook! local length: %d?" '( open-hook object-length ) snd-display
  then
  open-hook $" my-test3-proc" hook-member? unless
    $" local3 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook $" my-test4-proc" hook-member? unless
    $" local4 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook $" my-test5-proc" hook-member? unless
    $" local5 add-hook!: %s" '( open-hook ) snd-display
  then
;

event: 00-selection-from-snd ( -- )
  \ hooks
  open-hook reset-hook!
  open-hook ['] my-test1-proc add-hook!
  open-hook ['] my-test2-proc add-hook!
  open-hook object-length 2 <> if
    $" add-hook! global length: %d?" '( open-hook object-length ) snd-display
  then
  open-hook "my-test1-proc" hook-member? unless
    $" global1 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook ['] my-test2-proc hook-member? unless
    $" global2 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook
  '( ['] my-test3-proc ['] my-test4-proc ['] my-test5-proc )
  ['] my-local-thunk with-local-hook
  open-hook object-length 2 <> if
    $" add-hook! reset length: %d?" '( open-hook object-length ) snd-display
  then
  open-hook ['] my-test1-proc hook-member? unless
    $" reset1 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook "my-test2-proc" hook-member? unless
    $" reset2 add-hook!: %s" '( open-hook ) snd-display
  then
  \ set window
  window-x { x }			\ set to 600, x says 606
  window-y { y }			\ set to  10, y says 35
  'snd-motif provided? if
    x 600 6 + <> if $" window-x[600]: %s?" '( x ) snd-display then
    y 10 25 + <> if $" window-y[10]: %s?"  '( y ) snd-display then
  then
  .stack
  \ bind-key
  ['] C-xC-c { prc }
  [char] c 4 #t key-binding { old-prc }
  [char] c 4 prc #t bind-key { prc1 }
  [char] c 4 #t key-binding { prc2 }
  prc prc1 proc= unless $" bind-key: %s %s?" '( prc prc1 ) snd-display then
  prc prc2 proc= unless $" key-binding: %s %s?" '( prc prc2 ) snd-display then
  old-prc proc? if
    [char] c 4 old-prc #t bind-key drop
  else
    [char] c 4 #t unbind-key drop
  then
  .stack
  \ new-sound
  "fmv.snd" mus-next mus-bshort 22050 1 $" set-samples test" 100 new-sound { ind }
  10  3  3 0.1 make-vct set-samples drop
  0 20 ind 0 channel->vct { res }
  res vct( 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 ) vequal unless
    $" 1 set samples 0 for 0.1: %s?" '( res ) snd-display
  then
  ind close-sound drop
  .stack
  \ edot-product (test008)
  complex-test
  .stack
  \ delay (test008)
  3 make-delay { gen }
  3 make-delay { gen2 }
  4 :initial-contents '( 1.0 0.5 0.25 0.0 ) make-delay { gen1 }
  4 :initial-contents vct( 1.0 0.5 0.25 0.0 ) make-delay { gen3 }
  gen "delay" $" delay: line[3, step]: [0.000 0.000 0.000]" print-and-check
  10 0.0 make-vct map gen i 0.0 delay end-map { v0 }
  10 0.0 make-vct map gen2 delay? if gen2 i 0.0 delay else -1.0 then end-map { v1 }
  v0 v1 vequal unless $" map delay: %s %s?" '( v0 v1 ) snd-display then
  gen delay? unless $" %s not a delay?" '( gen ) snd-display then
  gen mus-length 3 <> if $" delay length: %d?" '( gen mus-length ) snd-display then
  v0 1 vct-ref 0.0 fneq
  v0 4 vct-ref 1.0 fneq ||
  v0 8 vct-ref 5.0 fneq || if $" delay output: %s?" '( v0 ) snd-display then
  gen1 0.0 0.0 delay 1.0  fneq
  gen1 0.0 0.0 delay 0.5  fneq ||
  gen1 0.0 0.0 delay 0.25 fneq ||
  gen1 0.0 0.0 delay 0.0  fneq ||
  gen1 0.0 0.0 delay 0.0  fneq || if
    $" delay with list initial-contents confused" '() snd-display
  then
  gen3 0.0 0.0 delay 1.0  fneq
  gen3 0.0 0.0 delay 0.5  fneq ||
  gen3 0.0 0.0 delay 0.25 fneq ||
  gen3 0.0 0.0 delay 0.0  fneq ||
  gen3 0.0 0.0 delay 0.0  fneq || if
    $" delay with vct initial-contents confused" '() snd-display
  then
  :size #f ['] make-delay #t nil fth-catch to res
  stack-reset
  res car 'wrong-type-arg object-equal? unless
    $" make-delay bad size false: %s" '( res ) snd-display
  then
  make-oscil { osc }
  3 :initial-element osc ['] make-delay #t nil fth-catch to res
  stack-reset
  res car 'wrong-type-arg object-equal? unless
    $" make-delay bad initial element: %s" '( res ) snd-display
  then
  -3 ['] make-delay #t nil fth-catch to res
  stack-reset
  res car 'out-of-range object-equal? unless
    $" make-delay bad size: %s" '( res ) snd-display
  then
  .stack
  3 make-delay { d1 }
  3 make-delay { d2 }
  4 make-delay { d3 }
  d1 1.0 0.0 delay drop
  d2 1.0 0.0 delay drop
  d3 1.0 0.0 delay drop
  d1 d2 d3 test-gen-equal
  3 :initial-element 1.0 make-delay to d1
  3 :initial-element 1.0 make-delay to d2
  3 :initial-element 0.5 make-delay to d3
  d1 d2 d3 test-gen-equal
  3 :initial-contents '( 1.0 0.0 0.0 ) make-delay to d1
  3 :initial-contents '( 1.0 0.0 0.0 ) make-delay to d2
  3 :initial-contents '( 1.0 1.0 1.0 ) make-delay to d3
  d1 d2 d3 test-gen-equal
  \ mix, track (test009)
  "hiho.wave" mus-next mus-bshort 22050 1 new-sound { new-index }
  new-index select-sound drop
  0 new-index 0 find-mix to res
  res if $" found non-existent mix: %s?" '( res ) snd-display then
  "pistol.snd" 100 mix { mix-id }
  mix-id mix? unless $" %s not mix?" '( mix-id ) snd-display then
  view-files-dialog { wid }
  mix-id mix-position { pos }
  mix-id mix-frames { len }
  mix-id mix-locked? { loc }
  mix-id mix-inverted? { inv }
  mix-id mix-tag-position { anc }
  mix-id mix-speed { spd }
  mix-id mix-speed-style { spdstyle }
  mix-id mix-track { trk }
  mix-id mix-home { home-lst }
  home-lst car { snd }
  home-lst cadr { chn }
  mix-id mix-chans { chns }
  mix-id 0 mix-amp { amp }
  mix-id make-mix-sample-reader { mr }
  mr mix-sample-reader? unless $" %s is not mix-sample-reader?"  '( mr ) snd-display then
  mr track-sample-reader?   if $" mix-sample-reader: track %s?"  '( mr ) snd-display then
  mr region-sample-reader?  if $" mix-sample-reader: region %s?" '( mr ) snd-display then
  mr sample-reader?         if $" mix-sample-reader: normal %s?" '( mr ) snd-display then
  mr sample-reader-position to res
  res 0<> if $" mix sample-reader-position: %d?" '( res ) snd-display then
  mr sample-reader-at-end? if $" mix sample-reader-at-end: %s?" '( mr ) snd-display then
  mr sample-reader-home to res
  mix-id res <> if $" mix sample-reader-home: %d %s?" '( res mr ) snd-display then
  mr object->string 0 21 string-substring to res
  res $" #<mix-sample-reader mi" string<> if
    $" mix sample-reader actually got: [%s]?" '( res ) snd-display
  then
  mix-id 1234 ['] mix-amp #t nil fth-catch to res
  stack-reset
  res car 'no-such-channel object-equal? unless
    $" mix-amp bad chan: %s" '( res ) snd-display
  then
  mix-id 1234 0.1 ['] set-mix-amp #t nil fth-catch to res
  stack-reset
  res car 'no-such-channel object-equal? unless
    $" set-mix-amp bad chan: %s" '( res ) snd-display
  then
  mix-id 1234 '( 0 0 1 1 ) ['] set-mix-amp-env #t nil fth-catch to res
  stack-reset
  res car 'no-such-channel object-equal? unless
    $" set-mix-amp-env bad chan: %s" '( res ) snd-display
  then
  0.0 0.0 { mx sx }
  99 0 do
    i odd? if mr read-mix-sample else mr read-mix-sample then to mx
    100 i + sample to sx
    mx sx fneq if $" read-mix-sample: %s %s?" '( mx sx ) snd-display then
  loop
  \ Scheme: (mr)
  \ Ruby:   mr.call
  \ Forth:  mr '() apply
  mr '() object-apply to mx
  199 sample to sx
  mx sx fneq if $" read-mix-sample 100: %s %s?" '( mx sx ) snd-display then
  mr free-sample-reader drop
  \
  100 pos <>   if $" mix-position: %d?"     '( pos ) snd-display then
  41623 len <> if $" mix-frames: %d?"       '( len ) snd-display then
  loc          if $" mix-locked?: %s?"      '( loc ) snd-display then
  inv          if $" mix-inverted?: %s?"    '( inv ) snd-display then
  anc      0<> if $" mix-tag-position: %d?" '( anc ) snd-display then
  trk      0<> if $" mix-track: %s?"        '( trk ) snd-display then
  snd new-index <> if $" snd mix-home: %d?" '( snd ) snd-display then
  chn      0<> if $" chn mix-home: %d?"     '( chn ) snd-display then
  chns 1    <> if $" mix-chans: %d?"        '( chns ) snd-display then
  amp 1.0 fneq if $" mix-amp: %s?"          '( amp ) snd-display then
  spd 1.0 fneq if $" mix-speed: %s?"        '( spd ) snd-display then
  spdstyle new-index speed-control-style <> if
    $" mix-speed-style: %s %s?" '( spdstyle new-index speed-control-style ) snd-display
  then
  mix-id ['] play-mix 'mus-error nil fth-catch to res
  stack-reset
  res car exception? if $" can't play mix: %s" '( res ) snd-display then
  mix-id -1 ['] set-mix-track #t nil fth-catch to res
  stack-reset
  res car 'out-of-range object-equal? unless
    $" set-mix-track -1: %s" '( res ) snd-display
  then
  mix-id 200 set-mix-position drop
  mix-id 0 0.5 set-mix-amp drop
  mix-id 2.0 set-mix-speed drop
  mix-id speed-control-as-ratio set-mix-speed-style drop
  mix-id mix-speed-style to res
  res speed-control-as-ratio <> if $" set-mix-speed-style: %s?" '( res ) snd-display then
  mix-id 123123 ['] set-mix-speed-style #t nil fth-catch to res
  stack-reset
  res car 'out-of-range object-equal? unless
    $" set-mix-speed-style bad arg: %s" '( res ) snd-display
  then
  \ 
  mix-id make-track to trk
  123123 ['] play-track #t nil fth-catch to res
  stack-reset
  res car 'no-such-track object-equal? unless
    $" play-track bad track: %s" '( res ) snd-display
  then
  123123 #t ['] play-track #t nil fth-catch to res
  stack-reset
  res car 'no-such-track object-equal? unless
    $" play-track bad track #t: %s" '( res ) snd-display
  then
  123123 0 ['] play-track #t nil fth-catch to res
  stack-reset
  res car 'no-such-track object-equal? unless
    $" play-track bad track index: %s" '( res ) snd-display
  then
  "oboe.snd" 0 0 sounds car 0 #f #f 123123 ['] mix #t nil fth-catch to res
  stack-reset
  res car 'no-such-track object-equal? unless
    $" mix bad track index: %s" '( res ) snd-display
  then
  3 0.1 make-vct 0 sounds car 0 #t $" bad mix-vct" 123123 ['] mix-vct #t nil fth-catch to res
  stack-reset
  res car 'no-such-track object-equal? unless
    $" mix-vct bad track index: %s" '( res ) snd-display
  then
  trk 123 ['] track #t nil fth-catch to res
  stack-reset
  res car 'no-such-channel object-equal? unless
    $" mix-vct bad track index: %s" '( res ) snd-display
  then
  trk play-track drop
  mix-id 30 set-mix-tag-position drop
  mix-id 0 '( 0 0 1 1 ) set-mix-amp-env drop
  mix-id 0 mix-amp-env to res
  mix-id 0  mix-id 0 mix-amp-env  set-mix-amp-env drop
  mix-id 0 mix-amp-env { res1 }
  res res1 vequal unless $" set-mix-amp-env to self: %s %s?" '( res res1 ) snd-display then
  mix-id 20 set-mix-tag-y drop
  mix-id mix-position to pos
  mix-id mix-speed to spd
  mix-id mix-track to trk
  mix-id 0 mix-amp to amp
  mix-id mix-tag-y { my }
  mix-id mix-tag-position to anc
  200 pos <>   if $" set-mix-position: %d?"     '( pos ) snd-display then
  spd 2.0 fneq if $" set-mix-speed: %s?"        '( spd ) snd-display then
  trk track? unless $" set-mix-track: %s?"      '( trk ) snd-display then
  my  20    <> if $" set-mix-tag-y: %d?"        '( my ) snd-display then
  amp 0.5 fneq if $" set-mix-amp: %s?"          '( amp ) snd-display then
  anc 30    <> if $" set-mix-tag-position: %d?" '( anc ) snd-display then
  mix-id 0 mix-amp-env to res
  res '( 0.0 0.0 1.0 1.0 ) list= unless $" set-mix-amp-env: %s?" '( res ) snd-display then
  \
  3 0.1 make-vct 100 mix-vct drop
  0 set-cursor drop
  1 #f #f forward-mix { nid }
  nid mix? if
    cursor { curs }
    curs nid mix-position <> if
      $" 1 forward-mix: %s %s %s?" '( nid curs nid mix-position ) snd-display
    then
  else
    $" 1 forward-mix: not a mix %s?" '( nid ) snd-display
  then
  2 #f #f forward-mix { nid1 }
  nid1 mix? if
    cursor { curs }
    curs nid1 mix-position <> if
      $" 2 forward-mix: m0 %s m1 %s c %s p %s?" '( nid nid1 curs nid mix-position ) snd-display
    then
  else
    $" 2 forward-mix: not a mix %s?" '( nid1 ) snd-display
  then
  1 #f #f backward-mix to nid1
  nid1 mix? if
    cursor { curs }
    curs nid1 mix-position <> if
      $" 1 backward-mix: m0 %s m1 %s c %s p %s?" '( nid nid1 curs nid mix-position ) snd-display
    then
  else
    $" 1 backward-mix: not a mix %s?" '( nid1 ) snd-display
  then
  100 #f #f find-mix to nid
  nid mix? if
    nid mix-position 100 <> if
      new-index 0 mixes map *key* mix-position end-map { mx-pos }
      $" 100 find-mix: %s %s %s?" '( nid dup mix-position mx-pos ) snd-display
    then
  else
    $" 100 find-mix: not a mix %s?" '( nid ) snd-display
  then
  200 #f #f find-mix to nid
  nid mix? if
    nid mix-position 200 <> if
      new-index 0 mixes map *key* mix-position end-map { mx-pos }
      $" 200 find-mix: %s %s %s?" '( nid dup mix-position mx-pos ) snd-display
    then
  else
    $" 200 find-mix: not a mix %s?" '( nid ) snd-display
  then
  \
  "oboe.snd" 100 mix to mix-id
  40 set-mix-waveform-height drop
  mix-id 'hiho 123 set-mix-property
  mix-id 'hiho mix-property to res
  res 123 <> if $" mix-property: %s?" '( res ) snd-display then
  mix-id 'not-here mix-property to res
  res if $" mix-property not-here: %s?" '( res ) snd-display then
  #f #f update-time-graph drop
  20 set-mix-waveform-height drop
  new-index revert-sound drop
  new-index close-sound drop
  wid hide-widget drop
  .stack
  \ envelopes (lists, vcts, arrays) (test015)
  1.0 vct( 0.0 0.0 2.0 1.0 )           1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  1.0 '( 0.0 0.0 1.0 1.0 2.0 0.0 )     1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  2.0 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  0.0 #( 1.0 0.5 2.0 0.0 )             1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  0.0 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )   1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0; %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )  1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 0.0 fneq if
    $" envelope-interp 0.0: %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -0.5 fneq if
    $" envelope-interp -0.5: %s?" 1 >list snd-display
  else
    drop
  then
  -1.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -1.0 fneq if
    $" envelope-interp -1.0: %s?" 1 >list snd-display
  else
    drop
  then
  1.5 #( -1.0 -1.0 1.0 1.0 )           1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 0.1 fneq if
    $" envelope-interp 0.1: %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )            32.0 envelope-interp dup 0.01336172 fneq if
    $" envelope-interp (exp 32): %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )           0.012 envelope-interp dup 0.36177473 fneq if
    $" envelope-interp (exp 0.012): %s?" 1 >list snd-display
  else
    drop
  then
  0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 )     1.0 envelope-interp dup 0.6 fneq if
    $" envelope-interp 0.6: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.5 ) { v0 }
  #( 0.0 0.0 2.0 0.5 ) #( 0.0 0.0 1.0 2.0 2.0 1.0 ) multiply-envelopes dup v0 0 fveql unless
    $" multiply-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) #( 0.0 0.0 1.0 1.0 2.0 0.0 ) multiply-envelopes dup v0 0 fveql unless
    $" multiply-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) max-envelope dup 3.0 fneq if
    $" 0 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 ) max-envelope dup 1.0 fneq if
    $" 1 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) max-envelope dup 2.0 fneq if
    $" 2 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) max-envelope dup -1.0 fneq if
    $" 3 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) max-envelope dup -1.0 fneq if
    $" 4 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) min-envelope dup 0.0 fneq if
    $" 0 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 ) min-envelope dup 1.0 fneq if
    $" 1 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) min-envelope dup 1.0 fneq if
    $" 2 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) min-envelope dup -2.0 fneq if
    $" 3 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) min-envelope dup -2.0 fneq if
    $" 4 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 0.1 0.2 #f #f stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope att: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope dec: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 1.5 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) #( 0.0 0.0 1.0 1.0 ) add-envelopes dup v0 0 fveql unless
    $" add-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 2.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 2.0 scale-envelope dup v0 0 fveql unless
    $" scale-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 0.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  .stack
;event

\ === TEST 10 ===

event: 10-marks ( -- )
  "oboe.snd" open-sound { ind }
  123 add-mark drop
  234 ind 0 "hiho"     1 add-mark drop
  345 ind 0 #f         1 add-mark drop
  456 ind 0 $" a mark" 2 add-mark drop
  567 ind 0 #f         1 add-mark drop
  ind "oboe.marks" save-marks drop
  ind close-sound drop
  "oboe.snd" open-sound to ind
  1 ind 0 $" new mark" 1 add-mark drop
  "oboe.marks" file-eval
  123 ind 0 find-mark { m }
  m mark? if
    m mark-name length zero? unless
      $" saved mark 123 name: %S?" '( m mark-name ) snd-display
    then
    m mark-sync zero? unless
      $" saved mark 123 sync: %S?" '( m mark-sync ) snd-display
    then
  else
    $" saved marks missed 123: %S?" '( m ) snd-display
  then
  234 ind 0 find-mark to m
  m mark? if
    m mark-name $" hiho" string<> if
      $" saved mark 234 name: %S?" '( m mark-name ) snd-display
    then
    m mark-sync { m2sync }
    m2sync 0= m2sync 1 = || if
      $" saved mark 234 sync: %S?" '( m mark-sync ) snd-display
    then
    m mark-sync
  else
    $" saved marks missed 234: %S?" '( m ) snd-display
    0
  then { m1-sync }
  345 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      $" saved mark 345 name: %S?" '( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      $" saved mark 345 sync: %S %S?" '( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 345: %S?" '( m ) snd-display
  then
  456 ind 0 find-mark to m
  m mark? if
    m mark-name $" a mark" string<> if
      $" saved mark 456 name: %S?" '( m mark-name ) snd-display
    then
    m mark-sync { m4sync }
    m4sync m1-sync = m4sync 0= || m4sync 1 = || if
      $" saved mark 456 sync: %S %S?" '( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 456: %S?" '( m ) snd-display
  then
  567 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      $" saved mark 567 name: %S?" '( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      $" saved mark 567 sync: %S %S?" '( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 567: %S?" '( m ) snd-display
  then
  ind close-sound drop
;event

\ === TEST 15 ===

: interpolated-peak-offset ( r1 r2 r3 -- r4 )
  { la ca ra }
  la ca fmax ra fmax 0.001 f+ { pk }
  la 0.0000001 fmax pk f/ flog 10 flog f/ { logla }
  ca 0.0000001 fmax pk f/ flog 10 flog f/ { logca }
  ra 0.0000001 fmax pk f/ flog 10 flog f/ { logra }
  logla logra f- f2/
  logla logra f+
  logca f2*  f-  f/
;

: freq-peak { beg ind size -- lst }
  beg size ind 0 channel->vct { data }
  data blackman2-window size snd-spectrum { spectr }
  0.0 { peak0 }
  0 { pk0loc }
  size 2/ 0 ?do
    spectr i vct-ref peak0 f> if
      spectr i vct-ref to peak0
      i to pk0loc
    then
  loop
  pk0loc 0> if
    spectr pk0loc 1- vct-ref  spectr pk0loc vct-ref  spectr pk0loc 1+ vct-ref
    interpolated-peak-offset
  else
    0.0
  then pk0loc f+  #f srate f*  size f/  peak0 2 >list
;
: src-test15-cb ( os -- proc; y self -- val )
  1 proc-create swap ,
 does> { y self -- val }
  self @ ( os ) 0.0 0.0 oscil f2/
;

: f3neq ( a b -- f ) f- fabs 10.0 f> ;
: f4neq ( a b -- f ) f- fabs  1.0 f> ;
: f5neq ( a b -- f ) { a b } a b f- fabs 10.0  a b fmax 0.05 f*  f> ;

event: 15-chan-local-vars ( -- )
  \ env.fs
  \ envelope-interp
  0.1 '( 0 0 1 1 ) 1.0 envelope-interp dup 0.1 fneq if
    $" envelope-interp 0.1: %s?" swap 1 >list snd-display
  else
    drop
  then
  0.1 '( 0 0 1 1 ) 32.0 envelope-interp dup 0.01336172 fneq if
    $" envelope-interp 0.013: %s?" swap 1 >list snd-display
  else
    drop
  then
  0.1 '( 0 0 1 1 ) 0.012 envelope-interp dup 0.36177473 fneq if
    $" envelope-interp 0.361: %s?" swap 1 >list snd-display
  else
    drop
  then
  0.3 '( 0 0 0.5 1 1 0 ) 1.0 envelope-interp dup 0.6 fneq if
    $" envelope-interp 0.3 '( 0 0 0.5 1 1 0 ): %s?" swap 1 >list snd-display
  else
    drop
  then
  \ window-envelope
  1.0 3.0 '( 0.0 0.0 5.0 1.0 ) window-envelope dup '( 1.0 0.2 3.0 0.6 ) feql if
    drop
  else
    $" window-envelope: %s?" swap 1 >list snd-display
  then
  \ multiply-envelopes
  '( 0 0 1 1 ) '( 0 0 1 1 2 0 ) multiply-envelopes dup '( 0 0 0.5 0.5 1 0 ) feql if
    drop
  else
    $" multiply-envelopes: %s?" swap 1 >list snd-display
  then
  \ max-envelope
  '( 0 0 1 1 2 3 4 0 ) max-envelope dup 3.0 fneq if
    $" max-envelopes (0): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 1 ) max-envelope dup 1.0 fneq if
    $" max-envelopes (1): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 1 1 1 2 2 ) max-envelope dup 2.0 fneq if
    $" max-envelopes (2): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 -1 1 -2 ) max-envelope dup -1.0 fneq if
    $" max-envelopes (3): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 -2 1 -1 ) max-envelope dup -1.0 fneq if
    $" max-envelopes (4): %s?" swap 1 >list snd-display
  else
    drop
  then
  \ min-envelope
  '( 0 0 1 1 2 3 4 0 ) min-envelope dup 0.0 fneq if
    $" min-envelopes (0): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 1 ) min-envelope dup 1.0 fneq if
    $" min-envelopes (1): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 1 1 1 2 2 ) min-envelope dup 1.0 fneq if
    $" min-envelopes (2): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 -1 1 -2 ) min-envelope dup -2.0 fneq if
    $" min-envelopes (3): %s?" swap 1 >list snd-display
  else
    drop
  then
  '( 0 -2 1 -1 ) min-envelope dup -2.0 fneq if
    $" min-envelopes (4): %s?" swap 1 >list snd-display
  else
    drop
  then
  \ integrate-envelope
  '(  0 0 1 1 ) integrate-envelope dup 0.5 fneq if
    $" integrate-envelopes (0): %s?" swap 1 >list snd-display
  else
    drop
  then
  '(  0 1 1 1 ) integrate-envelope dup 1.0 fneq if
    $" integrate-envelopes (1): %s?" swap 1 >list snd-display
  else
    drop
  then
  '(  0 0 1 1 2 0.5 ) integrate-envelope dup 1.25 fneq if
    $" integrate-envelopes (2): %s?" swap 1 >list snd-display
  else
    drop
  then
  \ stretch-envelope
  '(  0 0 1 1 ) 0.1 0.2 #f #f stretch-envelope dup '( 0 0 0.2 0.1 1.0 1 ) feql if
    drop
  else
    $" stretch-envelope att: %s?" swap 1 >list snd-display
  then
  '( 0 0 1 1 2 0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup '( 0 0 0.2 0.1 1.1 1 1.6 0.5 2 0 ) feql if
    drop
  else
    $" stretch-envelope dec: %s?" swap 1 >list snd-display
  then
  \ add-envelopes
  '( 0 0 1 1 2 0 ) '( 0 0 1 1 ) add-envelopes dup '( 0 0 0.5 1.5 1 1 ) feql if
    drop
  else
    $" add-envelopes: %s?" swap 1 >list snd-display
  then
  \ scale-envelope
  '( 0 0 1 1 ) 2 0 scale-envelope dup '( 0 0 1 2 ) feql if
    drop
  else
    $" scale-envelope: %s?" swap 1 >list snd-display
  then
  '( 0 0 1 1 ) 2 1 scale-envelope dup '( 0 1 1 3 ) feql if
    drop
  else
    $" scale-envelope off: %s?" swap 1 >list snd-display
  then
  \ reverse-envelope
  '( 0 0 1 1 ) reverse-envelope dup '( 0 1 1 0 ) feql if
    drop
  else
    $" reverse-envelope ramp: %s?" swap 1 >list snd-display
  then
  '( 0 0 0.5 1 2 0 ) reverse-envelope dup '( 0 0 1.5 1 2 0 ) feql if
    drop
  else
    $" reverse-envelope ramp 2: %s?" swap 1 >list snd-display
  then
  '( 0 0 0.5 1 2 1 ) reverse-envelope dup '( 0 1 1.5 1 2 0 ) feql if
    drop
  else
    $" reverse-envelope ramp 2: %s?" swap 1 >list snd-display
  then
  \ concatenate-envelopes (from snd/env.scm)
  '( 0 0 1 1 ) '( 0 1 1 0 ) 2 concatenate-envelopes dup '( 0.0 0 1.0 1 2.0 0 ) feql if
    drop
  else
    $" concatenate-envelopes (0): %s?" swap 1 >list snd-display
  then
  '( 0 0 1 1.5 ) '( 0 1 1 0 ) 2 concatenate-envelopes dup '( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    $" concatenate-envelopes (1): %s?" swap 1 >list snd-display
  then
  \ envelope-concatenate (from clm/env.lisp)
  '( 0 0 1 1 ) '( 0 1 1 0 ) 2 envelope-concatenate dup '( 0.0 0 1.0 1 1.01 1 2.01 0 ) feql if
    drop
  else
    $" envelope-concatenate (0): %s?" swap 1 >list snd-display
  then
  '( 0 0 1 1.5 ) '( 0 1 1 0 ) 2 envelope-concatenate dup '( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    $" envelope-concatenate (1): %s?" swap 1 >list snd-display
  then
  \ repeat-envelope
  '( 0 0 1 100 ) 2 repeat-envelope dup '( 0 0 1 100 1.01 0 2.01 100 ) feql if
    drop
  else
    $" repeat-envelope (0): %s?" swap 1 >list snd-display
  then
  '( 0 0 1.5 1 2 0 ) 2 repeat-envelope dup '( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 ) feql if
    drop
  else
    $" repeat-envelope (1): %s?" swap 1 >list snd-display
  then
  '( 0 0 1.5 1 2 0 ) 2 #f #t repeat-envelope dup '( 0.0 0 0.75 1 1.0 0 1.75 1 2.0 0 ) feql if
    drop
  else
    $" repeat-envelope (2): %s?" swap 1 >list snd-display
  then
  '( 0 0 1.5 1 2 0 ) 2 #t repeat-envelope dup '( 0 0 1.5 1 2.0 0 2.5 1 4.0 0 ) feql if
    drop
  else
    $" repeat-envelope (3): %s?" swap 1 >list snd-display
  then
  '( 0 0 1.5 1 2 0 ) 3 repeat-envelope dup '( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 5.5 1 6.0 0 ) feql if
    drop
  else
    $" repeat-envelope (4): %s?" swap 1 >list snd-display
  then
  \ normalize-envelope
  '( 0 0 1 1.5 2.0 1.0 ) normalize-envelope dup '( 0 0.0 1 1.0 2.0 0.667 ) feql if
    drop
  else
    $" normalize-envelope (0): %s?" swap 1 >list snd-display
  then
  '( 0 0 1 0.5 2 -0.8 ) normalize-envelope dup '( 0 0.0 1 0.625 2 -1.0 ) feql if
    drop
  else
    $" normalize-envelope (1): %s?" swap 1 >list snd-display
  then
  \ envelope-exp
  '( 0 0 1 1 ) 2.0 10 envelope-exp dup
  '( 0 0 0.1 0.01 0.2 0.04 0.3 0.09 0.4 0.16 0.5 0.25 0.6 0.36 0.7 0.49 0.8 0.64 0.9 0.81 1 1 )
  feql if
    drop
  else
    $" envelope-exp (0): %s?" swap 1 >list snd-display
  then
  '( 0 0 1 1 2 0 ) 1.0 10 envelope-exp dup
  '( 0 0 0.2 0.2 0.4 0.4 0.6 0.6 0.8 0.8 1 1 1.2 0.8 1.4 0.6 1.6 0.4 1.8 0.2 2 0 )
  feql if
    drop
  else
    $" envelope-exp (1): %s?" swap 1 >list snd-display
  then
  \ dsp.fs
  "test.snd" mus-next mus-bfloat 22050 1 $" src-* tests" 10000 new-sound { ind }
  \ src-duration tests
  '( 0 1 1 2 )      src-duration { d1 }
  '( 0 2 1 1 )      src-duration { d2 }
  '( 0 1 0.5 2 )    src-duration { d3 }
  '( 0.5 1 0.75 2 ) src-duration { d4 }
  d1 0.693147180559945 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    $" src-duration test1: %f %f %f %f" '( d1 d2 d3 d4 ) snd-display
  then
  '( 0 1 1 0.5 )      src-duration to d1
  '( 0 0.5 1 1 )      src-duration to d2
  '( 0 1 0.5 0.5 )    src-duration to d3
  '( 0.5 1 0.75 0.5 ) src-duration to d4
  d1 1.38629436111989 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    $" src-duration test2: %f %f %f %f" '( d1 d2 d3 d4 ) snd-display
  then
  '( 0 1 1 1 ) src-duration to d1
  '( 0 2 1 2 ) src-duration to d2
  d1 1.0 fneq
  d2 0.5 fneq || if
    $" src-duration test3: %f %f" '( d1 d2 ) snd-display
  then
  '( 0 0.5 0.5 3 0.6 1 0.7 0.1 0.8 1.5 1 1 ) src-duration to d1
  d1 1.02474349685432 fneq if
    $" src-duration test4: %f" '( d1 ) snd-display
  then
  '( 0 1 1 2 2 1 ) src-duration to d1
  d1 0.693147180559945 fneq if
    $" src-duration test5: %f" '( d1 ) snd-display
  then
  500.0 0.0 make-oscil src-test15-cb map-channel drop
  0 ind 8192 freq-peak { vals }
  500.0 vals car f4neq
  1.0   vals cadr fneq || if
    $" src no-test: %s" '( vals ) snd-display
  then
  ind close-sound drop
  .stack
  \
  16 1.0 dolph { val1 }
  dolph-chebyshev-window 16 1.0 make-fft-window { val2 }
  val1 val2 vequal unless
    $" dolph/dolph 1: %s %s" '( val1 val2 ) snd-display
  then
  16 1.0 dolph-1 to val1
  dolph-chebyshev-window 16 1.0 make-fft-window to val2
  val1 val2 vequal unless
    $" dolph-1/dolph 1: %s %s" '( val1 val2 ) snd-display
  then
;event

\ === TEST 19 ===

: clm-channel-test <{ :optional snd #f chn #f -- gen }>
  1 -1 make-two-zero 0 #f snd chn #f #f get-func-name clm-channel
;

: random-pi-func <{ x -- y }> pi random ;
#( '( lambda: <{ -- val }> vct( 1.0 0.5 ) 0 2 #f #f #f insert-vct ;
      $" lambda: <{ snd chn -- val }> vct( 1.000 0.500 ) 0 2 snd chn insert-vct drop ;" )
   '( lambda: <{ -- val }> #f #f clm-channel-test ;
      $" lambda: <{ snd chn -- val }>  snd chn clm-channel-test drop ;" )
   ( examp.fs )
   '( lambda: <{ -- val }> 1000 3000 #f #f fft-edit ;
      $" lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-edit drop ;" )
   '( lambda: <{ -- val }> 0.01 #f #f fft-squelch ;
      $" lambda: <{ snd chn -- val }> 0.01 snd chn fft-squelch drop ;" )
   '( lambda: <{ -- val }> 1000 3000 #f #f fft-cancel ;
      $" lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-cancel drop ;" )
   '( lambda: <{ -- val }> #f #f squelch-vowels ;
      $" lambda: <{ snd chn -- val }>  snd chn squelch-vowels drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 0 ) #f #f fft-env-edit ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 0 ) snd chn fft-env-edit drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 0 ) '( 0 1 1 0 2 0 ) '( 0 0 1 1 ) #f #f fft-env-interp ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 0 ) '( 0 1 1 0 2 0 ) '( 0 0 1 1 ) snd chn fft-env-interp drop ;" )
   '( lambda: <{ -- val }> 10 0.1 #f #f hello-dentist ;
      $" lambda: <{ snd chn -- val }> 10 0.1 snd chn hello-dentist drop ;" )
   '( lambda: <{ -- val }> 1 0.3 20 #f #f fp ;
      $" lambda: <{ snd chn -- val }> 1 0.3 20 snd chn fp drop ;" )
   '( lambda: <{ -- val }> '( 0 1 1 2 ) #f #f expsnd ;
      $" lambda: <{ snd chn -- val }> '( 0 1 1 2 ) snd chn expsnd drop ;" )
   '( lambda: <{ -- val }> 1 256 2 2 #f #f voiced->unvoiced ;
      $" lambda: <{ snd chn -- val }> 1 256 2 2 snd chn voiced->unvoiced drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 0 ) 2 #f #f env-sound-interp ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 0 ) 2 snd chn env-sound-interp drop ;" )
   '( lambda: <{ -- val }> '( '( "1a.snd" ) '( "pistol.snd" 1 2 ) ) #f #f add-notes ;
      $" lambda: <{ snd chn -- val }> '( '( \"1a.snd\" ) '( \"pistol.snd\" 1 2 ) ) snd chn add-notes drop ;" )
   '( lambda: <{ -- val }> 0 #f #f #f compand-channel ;
      $" lambda: <{ snd chn -- val }> 0 #f snd chn compand-channel drop ;" )
   '( lambda: <{ -- val }> 0 #f #f #f #f smooth-channel-via-ptree ;
      $" lambda: <{ snd chn -- val }> 0 #f snd chn smooth-channel-via-ptree drop ;" )
   '( lambda: <{ -- val }> 300 0 #f #f #f #f ring-modulate-channel ;
      $" lambda: <{ snd chn -- val }> 300 0 #f snd chn ring-modulate-channel drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 0 ) #f #f filtered-env ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 0 ) snd chn filtered-env drop ;" )
   '( lambda: <{ -- val }> 0.1 #f #f reverse-by-blocks ;
      $" lambda: <{ snd chn -- val }> 0.1 snd chn reverse-by-blocks drop ;" )
   '( lambda: <{ -- val }> 0.1 #f #f reverse-within-blocks ;
      $" lambda: <{ snd chn -- val }> 0.1 snd chn reverse-within-blocks drop ;" )
   ( extensions.fs )
   '( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f mix-channel ;
      $" lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn mix-channel drop ;" )
   '( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f insert-channel ;
      $" lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn insert-channel drop ;" )
   '( lambda: <{ -- val }> "1a.snd" 0.5 0.9 0 #f #f #f #f sine-ramp ;
      $" lambda: <{ snd chn -- val }> 0.5 0.9 0 #f snd chn sine-ramp drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f sine-env-channel ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn sine-env-channel drop ;" )
   '( lambda: <{ -- val }> 0 1 0 #f #f #f #f blackman4-ramp ;
      $" lambda: <{ snd chn -- val }> 0 1 0 #f snd chn blackman4-ramp drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f blackman4-env-channel ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn blackman4-env-channel drop ;" )
   '( lambda: <{ -- val }> 0.2 0.8 #t 0 #f #f #f #f ramp-squared ;
      $" lambda: <{ snd chn -- val }> 0.2 0.8 #t 0 #f snd chn ramp-squared drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 ) #t 0 #f #f #f #f env-squared-channel ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 ) #t 0 #f snd chn env-squared-channel drop ;" )
   '( lambda: <{ -- val }> 0.2 0.8 32 #t 0 #f #f #f #f ramp-expt ;
      $" lambda: <{ snd chn -- val }> 0.2 0.8 32 #t 0 #f snd chn ramp-expt drop ;" )
   '( lambda: <{ -- val }> '( 0 0 1 1 ) 32 #t 0 #f #f #f #f env-expt-channel ;
      $" lambda: <{ snd chn -- val }> '( 0 0 1 1 ) 32 #t 0 #f snd chn env-expt-channel drop ;" )
   '( lambda: <{ -- val }> 0.1 0 #f #f #f #f offset-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn offset-channel drop ;" )
     '( lambda: <{ -- val }> 0.1 0 #f #f #f #f dither-channel ;
        $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn dither-channel drop ;" )
   '( lambda: <{ -- val }> 0.1 0 #f #f #f #f contrast-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn contrast-channel drop ;" )
   ( dsp.fs )
   '( lambda: <{ -- val }> 550 600 10 40 50 0 #f #f #f #f ssb-bank ;
      $" lambda: <{ snd chn -- val }> 550 600 10 40 50 0 #f snd chn ssb-bank drop ;" )
   '( lambda: <{ -- val }> 550 600 '( 0 1 1 2 ) 10 40 50 0 #f #f #f #f ssb-bank-env ;
      $" lambda: <{ snd chn -- val }> 550 600 '( 0 1 1 2 ) 10 40 50 0 #f snd chn ssb-bank-env drop ;" )
   '( lambda: <{ -- val }> 1 #f #f down-oct ;
      $" lambda: <{ snd chn -- val }> 1 snd chn down-oct drop ;" )
   '( lambda: <{ -- val }> 8 #f #f freqdiv ;
      $" lambda: <{ snd chn -- val }> 8 snd chn freqdiv drop ;" )
   '( lambda: <{ -- val }> 8 0 #f #f #f adsat ;
      $" lambda: <{ snd chn -- val }> 8 0 #f snd chn adsat drop ;" )
   '( lambda: <{ -- val }> #f #f spike ;
      $" lambda: <{ snd chn -- val }>  snd chn spike drop ;" )
   '( lambda: <{ -- val }> #f #f zero-phase ;
      $" lambda: <{ snd chn -- val }>  snd chn zero-phase drop ;" )
   '( lambda: <{ -- val }> ['] random-pi-func #f #f rotate-phase ;
      $" lambda: <{ snd chn -- val }> ['] random-pi-func snd chn rotate-phase drop ;" )
   '( lambda: <{ -- val }> 0.5 #f #f brighten-slightly ;
      $" lambda: <{ snd chn -- val }> 0.5 snd chn brighten-slightly drop ;" )
   '( lambda: <{ -- val }> 100 40 0 #f #f #f #f shift-channel-pitch ;
      $" lambda: <{ snd chn -- val }> 100 40 0 #f snd chn shift-channel-pitch drop ;" )
   '( lambda: <{ -- val }> vct( 0.0 0.5 ) #f #f channel-polynomial ;
      $" lambda: <{ snd chn -- val }> vct( 0.000 0.500 ) snd chn channel-polynomial drop ;" )
   '( lambda: <{ -- val }> vct( 0.0 1.0 ) #f #f spectral-polynomial ;
      $" lambda: <{ snd chn -- val }> vct( 0.000 1.000 ) snd chn spectral-polynomial drop ;" )
   '( lambda: <{ -- val }> '( 60.0 120.0 240.0 ) #f 0 #f #f #f #f #t 2 notch-channel ;
      $" lambda: <{ snd chn -- val }> '( 60.0 120.0 240.0 ) #f 0 #f snd chn notch-channel drop ;" )
'snd-motif provided? [if]
   ( effects.fs )
   '( lambda: <{ -- val }> 0.1 128 effects-squelch-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 128 snd chn effects-squelch-channel drop ;" )
   '( lambda: <{ -- val }> #f 0.5 0.1 0 #f #f #f effects-echo ;
      $" lambda: <{ snd chn -- val }> #f 0.5 0.1 0 #f snd chn effects-echo drop ;" )
   '( lambda: <{ -- val }> 0.5 0.1 #f 0 #f #f #f effects-flecho ;
      $" lambda: <{ snd chn -- val }> 0.5 0.1 #f 0 #f snd chn effects-flecho drop ;" )
   '( lambda: <{ -- val }> 0.75 0.75 6.0 10.0 #f 0 #f #f #f effects-zecho ;
      $" lambda: <{ snd chn -- val }> 0.75 0.75 6.0 10.0 #f 0 #f snd chn effects-zecho drop ;" )
   '( lambda: <{ -- val }> 0.1 50 0 #f #f #f effects-comb-filter ;
      $" lambda: <{ snd chn -- val }> 0.1 50 0 #f snd chn effects-comb-filter drop ;" )
   '( lambda: <{ -- val }> 10000 0.5 0 #f #f #f effects-moog ;
      $" lambda: <{ snd chn -- val }> 10000 0.5 0 #f snd chn effects-moog drop ;" )
   '( lambda: <{ -- val }> #f #f effects-remove-dc ;
      $" lambda: <{ snd chn -- val }>  snd chn effects-remove-dc drop ;" )
   '( lambda: <{ -- val }> #f #f effects-compand ;
      $" lambda: <{ snd chn -- val }>  snd chn effects-compand drop ;" )
   '( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-am ;
      $" lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-am drop ;" )
   '( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-rm ;
      $" lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-rm drop ;" )
   '( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbp ;
      $" lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbp drop ;" )
   '( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbr ;
      $" lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbr drop ;" )
   '( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-bhp ;
      $" lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-bhp drop ;" )
   '( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-blp ;
      $" lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-blp drop ;" )
   '( lambda: <{ -- val }> 50.0 0.5 0 #f #f #f effects-hello-dentist ;
      $" lambda: <{ snd chn -- val }> 50.0 0.5 0 #f snd chn effects-hello-dentist drop ;" )
   '( lambda: <{ -- val }> 1.0 0.3 20.0 0 #f #f #f effects-fp ;
      $" lambda: <{ snd chn -- val }> 1.0 0.3 20.0 0 #f snd chn effects-fp drop ;" )
   '( lambda: <{ -- val }> 5.0 2.0 0.001 0 #f #f #f effects-flange ;
      $" lambda: <{ snd chn -- val }> 5.0 2.0 0.001 0 #f snd chn effects-flange drop ;" )
   '( lambda: <{ -- val }> 0.1 0 #f #f #f effects-jc-reverb-1 ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn effects-jc-reverb-1 drop ;" )
[then]
) value test19-*.fs

event: 19-save/restore ( -- )
  \
  "oboe.snd" open-sound { ind }
  'xm provided? 'xg provided? || if
    .stack
    test19-*.fs each { vals }
      vals car  { func1 }
      vals cadr { descr }
      func1 '() run-proc drop
      ind #f undef undef edit-list->function { func }
      func proc-source-ref descr string<> if
	$" edit-list->function %d: %s?" '( i func proc-source-ref ) snd-display
      then
      ind revert-sound drop
      func '( ind 0 ) run-proc drop
      ind revert-sound drop
    end-each
  then
  ind close-sound drop
;event

\ === TEST 23 ===

: test23-notehook ( inst start dur -- )
  { inst start dur }
  $" %14s[%d]: %5.2f  %5.2f" '( inst stack-level start dur ) snd-info
;
: test23-clm-ins ( -- )
  0.0 0.3 ['] clm-ins-test			\ from clm-ins.fs
  :notehook ['] test23-notehook
  :statistics #t
  :play #t
  :channels 2
  :reverb ['] nrev
  :reverb-data '( :lp-coeff 0.5 :lp-out-coeff 0.8 )
  :delete-reverb #t  with-sound drop
;
: test23-balance ( -- )
  make-rmsgain { rg }
  40 make-rmsgain { rg1 }
  2  make-rmsgain { rg2 }
  '( 0 0 1 1 2 0 )      :end 10000 make-env { e }
  '( 0 0 1 1 )          :end 10000 make-env { e1 }
  '( 0 0 1 1 2 0 10 0 ) :end 10000 make-env { e2 }
  440.0 make-oscil { o }
  10000 0 do
    e env { sig }
    i  rg  sig                    e2 env rmsgain-balance  *output*  outa drop
    i  rg1 sig                    e1 env rmsgain-balance  *output*  outb drop
    i  rg2 o 0.0 0.0 oscil 0.1 f* e2 env rmsgain-balance  *output*  outc drop
  loop
  rg rmsgain-gain-avg 0.98402 ffneq if
    $" rmsgain gain-avg: %f (%f)?" '( rg rmsgain-gain-avg 0.98402 ) snd-display
  then
  rg1 rmsgain-balance-avg 19380.2848 fneq
  rg1 rmsgain-balance-avg 19378.7850 fneq && if \ the resulte here
    $" rmsgain balance-avg: %f (%f)?" '( rg1 rmsgain-balance-avg 19380.2848 ) snd-display
  then
  rg2 :rmsg-avgc hash-ref 10000 <> if
    $" rmsgain count: %d (%d)?" '( rg2 :rmsg-avgc hash-ref 10000 ) snd-display
  then
;
: test23-ssb-fm ( gen mg -- proc; y self -- val )
  1 proc-create { prc } ( mg ) , ( gen ) ,
 does> { y self -- val }
  self       @ { mg }
  self cell+ @ { gen }
  gen   mg 0.0 0.0 oscil 0.02 f*  ssb-fm
;

include bird.fsm

event: 23-with-sound ( -- )
  0 { ind }
  ws-bird-test
  test23-clm-ins
  .stack
  ['] test23-balance :channels 3 with-sound ( ws ) :output hash-ref 0 find-sound to ind
  .stack
  ind sound? if ind close-sound drop else $" with-sound balance: %S?" '( ind ) snd-display then
  100.0 make-oscil { mg }
  1000 make-ssb-fm { gen }
  "tmp.snd" mus-next mus-bfloat 22050 1 new-sound to ind
  0 1000 ind 0 pad-channel drop
  gen mg test23-ssb-fm ['] map-channel #t nil fth-catch stack-reset
  ind close-sound drop
  .stack
;event

SIGINT lambda: ( sig -- )
  drop
  "\n" fth-print
  $" Interrupt received.  Finish %s" '( *filename* ) snd-info
  "" '() snd-info
  finish-snd-test
  with-exit if 2 (bye) then
; signal value original-sig-handler

let: ( -- )
  default-file-buffer-size set-mus-file-buffer-size drop
  #f  set-with-background-processes drop
  audio-amp-zero set-mus-audio-playback-amp
  #f  set-trap-segfault  drop
  #t  set-show-backtrace drop
  600 set-window-x       drop
  10  set-window-y       drop
  #t  show-listener      drop
  script-arg positive? if
    script-args length script-arg 1+ ?do
      test-numbers  script-args i list-ref string->number  array-push drop
    loop
  then
  test-numbers empty? if
    29 -1 do test-numbers i array-push drop loop
  then
  start-snd-test
  make-timer { tm }
  stack-reset
  tm start-timer
  ['] 00-selection-from-snd run-fth-test
  ['] 10-marks              run-fth-test
  ['] 15-chan-local-vars    run-fth-test
  ['] 19-save/restore       run-fth-test
  ['] 23-with-sound         run-fth-test
  tm stop-timer
  $" all done!" '() snd-info
  "" '() snd-info
  .stack
  timer-values each ( lst ) dup car swap cadr fth-test-timer-inspect end-each
  "summary" tm fth-test-timer-inspect
  finish-snd-test
  with-exit if bye then
;let

\ snd-test.fs ends here
