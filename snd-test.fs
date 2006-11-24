\ snd-test.fs -- snd-test.scm|rb tests -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sat Aug 05 00:09:28 CEST 2006
\ Changed: Sun Aug 27 05:20:26 CEST 2006

\ Commentary:
\
\ snd-forth -noinit -load snd-test.fs
\
\ test -1: general
\ 
\ Code:

#t value stack-verbose

require clm
require examp
require hooks
require env
require mix

char c 4 #t unbind-key drop

save-dir $" /zap/snd" || value original-save-dir
temp-dir $" /zap/tmp" || value original-temp-dir
listener-prompt          value original-prompt
300                      value sample-reader-tests
65536                    value default-file-buffer-size
$" HOME" getenv          value home-dir
0.0                	 value audio-amp-zero
#f                 	 value with-backtrace
$" /home/bil/sf1/" 	 value sf-dir
#t  		   	 value with-exit

: snd-display ( fmt args-lst -- )
  { fmt args-lst }
  ." \ " fmt args-lst fth-print cr
  $" \\ " fmt $+ $" \n" $+ args-lst string-format 1 >list .stderr
;

hide
: print-stack ( fmt args-lst -- )
  { fmt args-lst }
  $" #<stack" fmt $+ $" >" $+ args-lst snd-display
;
set-current

: show-stack ( ?? -- ?? )
  save-stack { s }
  $"  depth: %d" '( s length ) print-stack
  s each { obj } $" [%d]: %s" '( i obj ) print-stack end-each
  s restore-stack
;
previous

\ -2 value test-number
\ 
\ script-arg positive? [if]
\   /^-l.*/ script-args script-arg 1 - list-ref regexp-match [if]
\     $" script-args[%d]: %s %s"
\     '( script-arg 1 -
\        script-args script-arg 1 - list-ref
\        script-args ) snd-display
\   [then]
\   /snd-test/ script-args script-arg list-ref regexp-match [if]
\     $" script-args[%d]: %s %s"
\     '( script-arg
\        script-args script-arg list-ref
\        script-args ) snd-display
\   [then]
\   script-args script-arg list-ref string->number dup number? [if] to test-number [else] drop [then]
\ [then]

: fneq-err ( r1 r2 err -- f ) -rot f- fabs f<= ;
: cneq-err ( c1 c2 err -- f )
  { c1 c2 err }
  c1 real-ref  c2 real-ref  err fneq-err
  c1 image-ref c2 image-ref err fneq-err and
;
: fneq ( a b -- f ) 0.001 fneq-err ;
: cneq ( a b -- f ) 0.001 cneq-err ;

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
  b zero? b fzero? or if a else a b b/ then
;

: .stack ( -- )
  stack-verbose if
    depth 0> if
      "" '() snd-display
      show-stack
      "" '() snd-display
    then
  then
;

: start-snd-test ( date -- )
  { date }
  $" .sndtest-forth-rc" load-init-file
  $" === Snd version: %s" '( snd-version ) snd-display
  $" === Fth version: %s\n\\ " '( fth-date ) snd-display
  $" %s\n\\ " '( date ) snd-display
;

: finish-snd-test ( -- )
  stack-reset
  #t show-listener drop
  sounds if stop-playing drop then
  0 { file-count }
  #( original-save-dir original-temp-dir $" /tmp" ) { paths }
  paths each { path }
    path file-directory? if
      $" echo %s/snd_*" '( path ) string-format shell { str }
      str length 0> if str $"  \n" string-split else #() then { files }
      files length file-count + to file-count
      files each ( file ) file-delete end-each
    then
  end-each
  "" '() snd-display
  $" %d files deleted" '( file-count ) snd-display
  "" '() snd-display
  #( $" aaa.eps"
     $" envs.save"
     $" fmv.snd"
     $" fmv.wav"
     $" fmv0.snd"
     $" fmv1.snd"
     $" fmv2.snd"
     $" fmv3.snd"
     $" fmv4.reverb"
     $" fmv4.snd"
     $" hiho.marks"
     $" hiho.snd"
     $" hiho.snd"
     $" hiho.tmp"
     $" hiho.wave"
     $" ho"
     $" new.snd"
     $" oboe.marks"
     $" obtest.snd.stereo"
     $" snd.eps"
     $" test-1.snd"
     $" test-2.snd"
     $" test-macros.scm"
     $" test.aiff"
     $" test.data"
     $" test.rev"
     $" test.reverb"
     $" test.snd"
     $" test.snd.snd"
     $" test.wav"
     $" test.xpm"
     $" test2.snd"
     $" test3.snd"
     $" tmp.snd"
     $" with-mix.snd"
     $" 1"
     $" gtk-errors"
     $" accelmap" ) each ( file ) file-delete end-each
  #( $" mus10.snd.snd"
     $" ieee-text-16.snd.snd"
     $" trumps22.adp.snd"
     $" oki.wav.snd"
     $" nasahal.avi.snd"
     $" hcom-16.snd.snd"
     $" ce-c3.w02.snd"
     $" oboe.g723_24.snd"
     $" oboe.g723_40.snd"
     $" oboe.g721.snd"
     $" wood.sds.snd"
     $" o2_dvi.wave.snd"
     $" nist-shortpack.wav.snd"
     $" bad_data_format.snd.snd" ) each ( file ) sf-dir swap $+ file-delete end-each
  $" test-forth.output" save-listener drop
;

#() value timer-values

: run-fth-test ( xt -- )
  { xt }
  xt xt->name { name }
  $" test %s" '( name ) snd-display
  #f set-show-backtrace drop
  make-timer { tm }
  tm start-timer
  xt #t nil fth-catch ?dup-if ( res ) cadr '() snd-display then
  tm stop-timer
  stack-reset
  timer-values '( name tm ) array-push drop
  sounds if
    $" open sounds: %s" '( #t short-file-name ) snd-display
    sounds each ( snd ) close-sound drop end-each
  then
  $" test %s done\n\\ " '( name ) snd-display
;

: fth-test-timer-inspect ( name tm -- )
  { name tm }
  $" %s: %s" '( name tm ) snd-display
;

'complex provided? [if]
  : complex-test ( -- )
    \ edot-product (test008)
    1 1.0 make-vct { vals }
    0+0i vals edot-product dup 1.0 cneq if $" edot 1.0: %s?" swap 1 >list snd-display else drop then
    vals 0 0.0 vct-set! drop
    0+0i vals edot-product dup 0.0 cneq if $" edot 0.0: %s?" swap 1 >list snd-display else drop then
    #( 1.0 ) to vals
    0+0i vals edot-product dup 1.0 cneq if $" edot 1.0: %s?" swap 1 >list snd-display else drop then
    vals 0 0+0i array-set!
    0+0i vals edot-product dup 0.0 cneq if $" edot i: %s?"   swap 1 >list snd-display else drop then
    4 1.0 make-vct to vals
    0.25 two-pi f* >c vals edot-product
    0.00 two-pi f* fexp
    0.25 two-pi f* fexp f+
    0.50 two-pi f* fexp f+
    0.75 two-pi f* fexp f+ over over cneq if
      $" edot 4 i: %s %s?" 2 >list snd-display
    else
      2drop
    then
    #( 1.0 2.0 3.0 4.0 ) to vals
    0.25 two-pi f* 0+0i c* >c vals edot-product
    0.00 two-pi f* 0+0i c* cexp 1.0 c* 
    0.25 two-pi f* 0+0i c* cexp 2.0 c* c+
    0.50 two-pi f* 0+0i c* cexp 3.0 c* c+
    0.75 two-pi f* 0+0i c* cexp 4.0 c* c+ over over cneq if
      $" edot 4 -i: %s %s?" 2 >list snd-display
    else
      2drop
    then
    #( 0.0 1+0i c+ 1.0 1+0i c+ 2.0 1+0i c+ 3.0 1+0i c+ ) to vals
    0.25 two-pi f* 0-1i c* vals edot-product
    0.00 two-pi f* 0-1i c* cexp 1+0i c* 
    0.25 two-pi f* 0-1i c* cexp 2+0i c* c+
    0.50 two-pi f* 0-1i c* cexp 3+0i c* c+
    0.75 two-pi f* 0-1i c* cexp 4+0i c* c+ over over cneq if
      $" edot 4 -i * i: %s %s?" 2 >list snd-display
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
: C-xC-c ( -- f ) 0 snd-exit #f ;

\ hooks
: my-test1-proc ( -- proc; self fname -- f )
  lambda-create latestxt 1 make-proc
 does> { self fname -- f }
  #f
;
: my-test2-proc ( -- proc; self fname -- f )
  lambda-create latestxt 1 make-proc
 does> { self fname -- f }
  #f
;
: my-test3-proc ( -- proc; self fname -- f )
  lambda-create latestxt 1 make-proc
 does> { self fname -- f }
  #f
;
: my-test4-proc ( -- proc; self fname -- f )
  lambda-create latestxt 1 make-proc
 does> { self fname -- f }
  #f
;
: my-test5-proc ( -- proc; self fname -- f )
  lambda-create latestxt 1 make-proc
 does> { self fname -- f }
  #f
;
: my-local-thunk ( -- )
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

: test0-1 ( -- )
  \ hooks
  open-hook reset-hook!
  open-hook my-test1-proc add-hook!
  open-hook my-test2-proc add-hook!
  open-hook object-length 2 <> if
    $" add-hook! global length: %d?" '( open-hook object-length ) snd-display
  then
  open-hook $" my-test1-proc" hook-member? unless
    $" global1 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook $" my-test2-proc" hook-member? unless
    $" global2 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook '( my-test3-proc my-test4-proc my-test5-proc ) ['] my-local-thunk with-local-hook
  open-hook object-length 2 <> if
    $" add-hook! reset length: %d?" '( open-hook object-length ) snd-display
  then
  open-hook $" my-test1-proc" hook-member? unless
    $" reset1 add-hook!: %s" '( open-hook ) snd-display
  then
  open-hook $" my-test2-proc" hook-member? unless
    $" reset2 add-hook!: %s" '( open-hook ) snd-display
  then
  .stack
  \ set window
  window-x { x }			\ set to 600, x says 606
  window-y { y }			\ set to  10, y says 35
  x 600 6 + <> if $" window-x[600]: %s?" '( x ) snd-display then
  y 10 25 + <> if $" window-y[10]: %s?"  '( y ) snd-display then
  .stack 
  \ bind-key
  ['] C-xC-c 0 make-proc { prc }
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
  $" fmv.snd" mus-next mus-bshort 22050 1 $" set-samples test" 100 new-sound { ind }
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
  gen $" delay" $" delay: line[3, step]: [0.000 0.000 0.000]" print-and-check
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
  res if 2drop ( args :size #f ) else drop ( result ) then
  res car 'wrong-type-arg object-equal? unless
    $" make-delay bad size false: %s" '( res ) snd-display
  then
  make-oscil { osc }
  3 :initial-element osc ['] make-delay #t nil fth-catch to res
  res if 2drop drop ( args ) else drop ( result ) then
  res car 'wrong-type-arg object-equal? unless
    $" make-delay bad initial element: %s" '( res ) snd-display
  then
  -3 ['] make-delay #t nil fth-catch to res drop
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
  $" hiho.wave" mus-next mus-bshort 22050 1 new-sound { new-index }
  new-index select-sound drop
  0 new-index 0 find-mix to res
  res if $" found non-existent mix: %s?" '( res ) snd-display then
  $" pistol.snd" 100 mix { mix-id }
  mix-id mix? unless $" %s not mix?" '() snd-display then
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
  mr object->string 0 22 string-substring to res
  res $" #<mix-sample-reader mi" string<> if
    $" mix sample-reader actually got: [%s]?" '( res ) snd-display
  then
  mix-id 1234 ['] mix-amp #t nil fth-catch to res
  res if 2drop else drop then
  res car 'no-such-channel object-equal? unless
    $" mix-amp bad chan: %s" '( res ) snd-display
  then
  mix-id 1234 0.1 ['] set-mix-amp #t nil fth-catch to res
  res if 2drop drop else drop then
  res car 'no-such-channel object-equal? unless
    $" set-mix-amp bad chan: %s" '( res ) snd-display
  then
  mix-id 1234 '( 0 0 1 1 ) ['] set-mix-amp-env #t nil fth-catch to res
  res if 2drop drop else drop then
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
  mix-id ['] play-mix 'mus-error nil fth-catch to res drop
  res car exception? if
    $" can't play mix: %s" '( res ) snd-display
  then
  mix-id -1 ['] set-mix-track #t nil fth-catch to res
  res if 2drop else drop then
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
  res if 2drop else drop then
  res car 'out-of-range object-equal? unless
    $" set-mix-speed-style bad arg: %s" '( res ) snd-display
  then
  \ 
  mix-id make-track to trk
  123123 ['] play-track #t nil fth-catch to res drop
  res car 'no-such-track object-equal? unless
    $" play-track bad track: %s" '( res ) snd-display
  then
  123123 #t ['] play-track #t nil fth-catch to res
  res if 2drop else drop then
  res car 'no-such-track object-equal? unless
    $" play-track bad track #t: %s" '( res ) snd-display
  then
  123123 0 ['] play-track #t nil fth-catch to res
  res if 2drop else drop then
  res car 'no-such-track object-equal? unless
    $" play-track bad track index: %s" '( res ) snd-display
  then
  $" oboe.snd" 0 0 sounds car 0 #f #f 123123 ['] mix #t nil fth-catch to res
  res if 2drop 2drop 2drop 2drop else drop then
  res car 'no-such-track object-equal? unless
    $" mix bad track index: %s" '( res ) snd-display
  then
  3 0.1 make-vct 0 sounds car 0 #t $" bad mix-vct" 123123 ['] mix-vct #t nil fth-catch to res
  res if 2drop 2drop 2drop drop else drop then
  res car 'no-such-track object-equal? unless
    $" mix-vct bad track index: %s" '( res ) snd-display
  then
  trk 123 ['] track #t nil fth-catch to res
  res if 2drop else drop then
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
  $" oboe.snd" 100 mix to mix-id
  40 set-mix-waveform-height drop
  'hiho 123 mix-id set-mix-property
  'hiho mix-id mix-property to res
  res 123 <> if $" mix-property: %s?" '( res ) snd-display then
  'not-here mix-id mix-property to res
  res if $" mix-property not-here: %s?" '( res ) snd-display then
  #f #f update-time-graph drop
  20 set-mix-waveform-height drop
  new-index revert-sound drop
  new-index close-sound drop
  wid hide-widget drop
  .stack
  \ envelopes (lists, vcts, arrays) (test015)
  1.0 vct( 0.0 0.0 2.0 1.0 )           1.0 envelope-interp dup 0.5 fneq if
    $" \ envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  1.0 '( 0.0 0.0 1.0 1.0 2.0 0.0 )     1.0 envelope-interp dup 1.0 fneq if
    $" \ envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  2.0 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 1.0 fneq if
    $" \ envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  0.0 #( 1.0 0.5 2.0 0.0 )             1.0 envelope-interp dup 0.5 fneq if
    $" \ envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  0.0 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )   1.0 envelope-interp dup 1.0 fneq if
    $" \ envelope-interp 1.0; %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )  1.0 envelope-interp dup 0.5 fneq if
    $" \ envelope-interp 0.5: %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 0.0 fneq if
    $" \ envelope-interp 0.0: %s?" 1 >list snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -0.5 fneq if
    $" \ envelope-interp -0.5: %s?" 1 >list snd-display
  else
    drop
  then
  -1.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -1.0 fneq if
    $" \ envelope-interp -1.0: %s?" 1 >list snd-display
  else
    drop
  then
  1.5 #( -1.0 -1.0 1.0 1.0 )           1.0 envelope-interp dup 1.0 fneq if
    $" \ envelope-interp 1.0: %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 0.1 fneq if
    $" \ envelope-interp 0.1: %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )            32.0 envelope-interp dup 0.01336172 fneq if
    $" \ envelope-interp (exp 32): %s?" 1 >list snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )           0.012 envelope-interp dup 0.36177473 fneq if
    $" \ envelope-interp (exp 0.012): %s?" 1 >list snd-display
  else
    drop
  then
  0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 )     1.0 envelope-interp dup 0.6 fneq if
    $" \ envelope-interp 0.6: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.5 ) { v0 }
  #( 0.0 0.0 2.0 0.5 ) #( 0.0 0.0 1.0 2.0 2.0 1.0 ) multiply-envelopes dup v0 0 fveql unless
    $" \ multiply-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) #( 0.0 0.0 1.0 1.0 2.0 0.0 ) multiply-envelopes dup v0 0 fveql unless
    $" \ multiply-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) max-envelope dup 3.0 fneq if
    $" \ 0 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 ) max-envelope dup 1.0 fneq if
    $" \ 1 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) max-envelope dup 2.0 fneq if
    $" \ 2 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) max-envelope dup -1.0 fneq if
    $" \ 3 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) max-envelope dup -1.0 fneq if
    $" \ 4 max-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) min-envelope dup 0.0 fneq if
    $" \ 0 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 ) min-envelope dup 1.0 fneq if
    $" \ 1 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) min-envelope dup 1.0 fneq if
    $" \ 2 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) min-envelope dup -2.0 fneq if
    $" \ 3 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) min-envelope dup -2.0 fneq if
    $" \ 4 min-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 0.1 0.2 #f #f stretch-envelope dup v0 0 fveql unless
    $" \ stretch-envelope att: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" \ stretch-envelope dec: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" \ stretch-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 1.5 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) #( 0.0 0.0 1.0 1.0 ) add-envelopes dup v0 0 fveql unless
    $" \ add-envelopes: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 2.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 2.0 scale-envelope dup v0 0 fveql unless
    $" \ scale-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" \ reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 0.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 0.0 ) reverse-envelope dup v0 0 fveql unless
    $" \ reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  #( 0.0 1.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" \ reverse-envelope: %s?" 1 >list snd-display
  else
    drop
  then
  .stack
;

'snd-nogui provided? [if] #t set-with-mix-tag drop [then]
default-file-buffer-size set-mus-file-buffer-size drop
#f set-with-background-processes drop
600 set-window-x drop
10  set-window-y drop
#t show-listener drop
date start-snd-test
make-timer value test-tm
stack-reset

test-tm start-timer
' test0-1 run-fth-test
test-tm stop-timer

$" all done!\n\\ " '() snd-display
depth 0> [if] show-stack [then]
timer-values [each] ( lst ) dup car swap cadr fth-test-timer-inspect [end-each]
$" summary" test-tm fth-test-timer-inspect
finish-snd-test
with-exit [if] bye [then]

\ snd-test.fs ends here
