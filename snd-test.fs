\ snd-test.fs -- snd-test.scm|rb tests -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sat Aug 05 00:09:28 CEST 2006
\ Changed: Wed Jun 09 23:58:15 CEST 2010

\ Commentary:
\
\ You may use an init file name `pwd`/.sndtest.fs or ~/.sndtest.fs to set
\ global variables or define other special functions, hooks, etc.
\
\ snd-forth -noinit -load snd-test.fs        \ all tests
\ snd-forth -noinit -load snd-test.fs 3 7 20 \ test 3 7 20
\ snd-forth -noinit -load snd-test.fs -23    \ all tests but 23
\
\ test -1: general
\ test 10: marks
\ test 15: chan-local vars
\ test 19: save and restore
\ test 23: with-sound
\ test 28: errors

#f value under-valgrind
24 set-object-print-length

'snd-nogui provided? [unless]
  \ Prints to Snd's listener and stdout/stderr.
  :port-name "sndout"
  :write-line lambda: <{ line -- }> line snd-print .stdout ;
  make-soft-port set-*stdout* value stdout-io
  :port-name "snderr"
  :write-line lambda: <{ line -- }> line snd-print .stderr ;
  make-soft-port set-*stderr* value stderr-io
[then]

\ Output words: not clm-print here if we want xterm output.  That's
\ why no clm-message which uses clm-print.

\ SND-TEST-MESSAGE: Puts a comment sign before output and terminates with a carriage return
: snd-test-message ( fmt args -- ) ." \ " fth-print cr ;

\ SND-DISPLAY: Wraps text like snd-test-message and prepends text with
\ current line number ("\ [0010] text\n").
hide
: (snd-display) { fmt args lno -- } $" \\ [%04d] %s\n" #( lno fmt args string-format ) fth-print ;
set-current
: snd-display ( --; fmt args -- ) postpone *lineno* postpone (snd-display) ; immediate
previous

\ lambda: <{ -- }> cr gc-stats cr .memory cr cr ; at-exit
\ before-load-hook lambda: <{ fname -- f }> $" loading " fname $+ #f snd-test-message #t ; add-hook!

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]
'snd-gtk   provided? 'xg provided? not && [if] dl-load libxg Init_libxg [then]

'snd-nogui provided? [if]
  : x-bounds <{ :optional snd 0 chn 0 axis 0 -- }> #f ;
  : y-bounds <{ :optional snd 0 chn 0 axis 0 -- }> #f ;
  : set-x-bounds <{ bounds :optional snd 0 chn 0 axis 0 -- }> bounds ;
  : set-y-bounds <{ bounds :optional snd 0 chn 0 axis 0 -- }> bounds ;
[then]

require clm
require clm-ins
require examp
require hooks
require marks
require extensions
require env
require mix
require dsp
'snd-motif provided? [if]
  require snd-xm
  under-valgrind [unless]
    require effects
  [then]
[then]

reset-all-hooks

nil value *arg1*
nil value *arg2*
nil value *prc*
nil value *tag*

\ If #t, prints additionals, e.g. function names in test-19 or proc-array lengths in test-28
#t value *snd-test-verbose*

\ You may set them in .sndtest.fs.
#f value my-snd-error-hook
#f value my-mus-error-hook

save-dir "/home/bil/zap/snd" ||   value original-save-dir
temp-dir "/home/bil/zap/tmp" ||   value original-temp-dir
listener-prompt        		  value original-prompt
1024 8 *               		  value default-file-buffer-size
"HOME" getenv          		  value home-dir
"/home/bil/sf1/"       		  value sf-dir
#t  		       		  value with-exit
#f                     		  value all-args
"/home/bil/zap/sounds/bigger.snd" value bigger-snd

#f to *fth-verbose*
#f to *fth-debug*
#f to *clm-verbose*
#f to *clm-debug*

\ let: ( -- )
\   file-pwd "/peaks" $+ { dir }
\   dir file-directory? unless dir 0o755 file-mkdir then
\   dir set-peak-env-dir drop
\ ;let

*clm-search-list* file-pwd array-push to *clm-search-list*

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
    else
      #f
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
: arity-ok <{ proc args -- f }>
  proc proc? if
    \ draw-axes 0/0/#t but it means 3/6/#f
    proc proc-name "draw-axes" string= if
      args 3 >=
      args 9 <= &&
    else
      proc proc-arity { args-lst }
      args args-lst 0 array-ref >=
      args args-lst 0 array-ref args-lst 1 array-ref + <= &&
    then
  else
    #f
  then
;
: set-arity-ok <{ proc args -- f }> proc set-xt args arity-ok ; 
: make-color-with-catch ( c1 c2 c3 -- color )
  <'> make-color 'no-such-color #t fth-catch if stack-reset 1 0 0 make-color then
;
: reset-almost-all-hooks ( -- )
  reset-all-hooks
  my-snd-error-hook proc? if snd-error-hook my-snd-error-hook add-hook! then
  my-mus-error-hook proc? if mus-error-hook my-mus-error-hook add-hook! then
;
'xm provided? [if]
  : dismiss-all-dialogs ( -- )
    nil nil { dialog d }
    dialog-widgets each to dialog
      dialog if
	dialog 0 array-ref symbol? if
	  dialog FXtIsManaged if dialog FXtUnmanageChild drop then
	else
	  dialog each to d
	    d 0 array-ref symbol? if
	      d FXtIsManaged if d FXtUnmanageChild drop then
	    then
	  end-each
	then
      then
    end-each
  ;
[else]
  'xg provided? [if]
    : dismiss-all-dialogs ( -- )
      nil nil { dialog d }
      dialog-widgets each to dialog
	dialog if
	  dialog xmobj? if
	    dialog Fgtk_widget_hide drop
	  else
	    dialog each to d
	      d xmobj? if
		d Fgtk_widget_hide drop
	      then
	    end-each
	  then
	then
      end-each
    ;
  [then]
  <'> noop alias dismiss-all-dialogs
[then]

#f  value overall-start-time
#() value test-numbers
: run-fth-test ( xt -- )
  { xt }
  xt xt->name { name }
  test-numbers  name 0 2 string-substring string->number  array-member? if
    name #f snd-test-message
    stack-reset
    gc-run
    make-timer { tm }
    xt execute
    tm stop-timer
    stack-reset
    sounds if
      $" open sounds: %s" #( #t short-file-name ) snd-test-message
      sounds each ( snd ) close-sound drop end-each
    then
    $" %s: %s\n\\ " #( name tm ) snd-test-message
  then
;
: start-snd-test ( -- )
  \ Global variables may be overridden in `pwd`/.sndtest.fs or ~/.sndtest.fs
  ".sndtest.fs" load-init-file
  'snd-motif provided? if
    "motif"
  else
    'snd-gtk provided? if
      "gtk"
    else
      'snd-nogui provided? if
	#t set-with-mix-tags drop
	"nogui"
      else
	"unknown"
      then
    then
  then { kind }
  stack-reset
  "test.snd" file-exists? if "test.snd" 0o644 file-chmod then
  $" === Snd version: %s (snd-%s)" #( snd-version kind ) snd-test-message
  $" === Fth version: %s"          #( fth-version )      snd-test-message
  ""   #f snd-test-message
  date #f snd-test-message
  ""   #f snd-test-message
  default-file-buffer-size set-mus-file-buffer-size to *clm-file-buffer-size*
  #f  set-with-background-processes drop
  #f  set-trap-segfault  	    drop
  600 set-window-x       	    drop
  10  set-window-y       	    drop
  #t  set-show-listener      	    drop
  reset-almost-all-hooks
  22050 set-mus-srate f>s to *clm-srate*
  stack-reset
  make-timer to overall-start-time
;
: finish-snd-test ( -- )
  overall-start-time stop-timer
  .stack
  stack-reset
  regions each ( r ) forget-region drop end-each
  0 set-view-files-sort drop
  clear-sincs drop
  sounds if stop-playing drop then
  reset-almost-all-hooks
  $" all done!" #f snd-test-message
  "" #f snd-test-message
  $" summary: %s" #( overall-start-time ) snd-test-message
  0 nil nil { file-count path file }
  #( original-save-dir original-temp-dir "/tmp" ) each to path
    path file-directory? if
      path file-dir each to file
	/snd_/ file regexp-match if
	  file file-delete
	  1 +to file-count
	then
      end-each
    then
  end-each
  "" #f snd-test-message
  $" %d files deleted" #( file-count ) snd-test-message
  "" #f snd-test-message
  "test.snd" file-exists? if "test.snd" 0o644 file-chmod then
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
  #t  set-show-listener      	    drop
  "test-forth.output" save-listener drop
  original-prompt set-listener-prompt drop
;

'complex provided? [if]
  : complex-test ( -- )
    \ edot-product (test008)
    0.0 vct( 1.0 ) edot-product dup 1.0 fneq if
      $" edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 vct( 0.0 ) edot-product dup 0.0 fneq if
      $" edot 0.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 1.0 ) edot-product dup 1.0 fneq if
      $" edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 0+1i ) edot-product dup 0+1i cneq if
      $" edot i: %s?" swap snd-display
    else
      drop
    then
    0.25 two-pi f* vct( 1.0 1.0 1.0 1.0 ) edot-product
    0.00 two-pi f* fexp
    0.25 two-pi f* fexp f+
    0.50 two-pi f* fexp f+
    0.75 two-pi f* fexp f+ over over fneq if
      2 >array $" edot 4: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1.0 2.0 3.0 4.0 ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1 c* 
    0.25 two-pi f* 0-1i c* cexp 2 c* c+
    0.50 two-pi f* 0-1i c* cexp 3 c* c+
    0.75 two-pi f* 0-1i c* cexp 4 c* c+ over over cneq if
      2 >array $" edot 4 -i: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1+1i 2+1i 3+1i 4+1i ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1+1i c* 
    0.25 two-pi f* 0-1i c* cexp 2+1i c* c+
    0.50 two-pi f* 0-1i c* cexp 3+1i c* c+
    0.75 two-pi f* 0-1i c* cexp 4+1i c* c+ over over cneq if
      2 >array $" edot 4 -i * i: %s %s?" swap snd-display
    else
      2drop
    then
  ;
[else]
  <'> noop alias complex-test
[then]

: print-and-check ( gen name desc -- )
  { gen name desc }
  gen mus-name name string<> if $" mus-name %s: %s?" #( name gen mus-name ) snd-display then
  gen mus-describe desc string<> if $" mus-describe %s: %s?" #( name gen ) snd-display then
  gen { egen }
  gen egen object-equal? unless $" equal? %s: %s %s?" #( name gen egen ) snd-display then
;

\ ====== test -1: general
: test-gen-equal ( g0 g1 g2 -- )
  { g0 g1 g2 }
  \ g0 g1 =
  \ g0 g2 <> at start
  g0 { g3 }
  2 make-frame { gad }
  g0 g3 object-equal? unless $" let %s: %s equal? %s?" #( g0 mus-name g0 g3 ) snd-display then
  g0 g1 object-equal? unless $" %s: %s equal? %s?"     #( g0 mus-name g0 g1 ) snd-display then
  g0 g2 object-equal?     if $" %s: %s equal? %s?"     #( g0 mus-name g0 g2 ) snd-display then
  g0 gad object-equal?    if $" %s/frame: %s equal? %s?" #( g0 mus-name g0 gad ) snd-display then
  g0 0.0 0.0 mus-apply drop
  g3 #( 0.0 0.0 ) object-apply drop
  g3 0.0 0.0 mus-apply drop
  g0 g3 object-equal? unless $" run let %s: %s equal? %s?" #( g0 mus-name g0 g3 ) snd-display then
  g0 g1 object-equal?     if $" run %s: %s equal? %s?"     #( g0 mus-name g0 g1 ) snd-display then
  g0 g2 object-equal?     if $" run %s: %s equal? %s?"     #( g0 mus-name g0 g2 ) snd-display then
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
    $" add-hook! local length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook $" my-test3-proc" hook-member? unless
    $" local3 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook $" my-test4-proc" hook-member? unless
    $" local4 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook $" my-test5-proc" hook-member? unless
    $" local5 add-hook!: %s" #( open-hook ) snd-display
  then
;

: 00-sel-from-snd ( -- )
  \ hooks
  open-hook reset-hook!
  open-hook <'> my-test1-proc add-hook!
  open-hook <'> my-test2-proc add-hook!
  open-hook object-length 2 <> if
    $" add-hook! global length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook "my-test1-proc" hook-member? unless
    $" global1 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook <'> my-test2-proc hook-member? unless
    $" global2 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook
  #( <'> my-test3-proc <'> my-test4-proc <'> my-test5-proc )
  <'> my-local-thunk with-local-hook
  open-hook object-length 2 <> if
    $" add-hook! reset length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook <'> my-test1-proc hook-member? unless
    $" reset1 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook "my-test2-proc" hook-member? unless
    $" reset2 add-hook!: %s" #( open-hook ) snd-display
  then
  \ set window
  window-x { x }			\ set to 600, x says 606
  window-y { y }			\ set to  10, y says 35
  'snd-motif provided? if
    x 600 6 + <> if $" window-x[600]: %s?" #( x ) snd-display then
    y 10 25 + <> if $" window-y[10]: %s?"  #( y ) snd-display then
  then
  \ bind-key
  'snd-nogui provided? unless
    <'> C-xC-c { prc }
    "c" 4 #t key-binding { old-prc }
    "c" 4 prc #t bind-key { prc1 }
    "c" 4 #t key-binding { prc2 }
    prc prc1 = unless $" bind-key: %s %s?" #( prc prc1 ) snd-display then
    prc prc2 = unless $" key-binding: %s %s?" #( prc prc2 ) snd-display then
    old-prc proc? if
      "c" 4 old-prc #t bind-key drop
    else
      "c" 4 #t unbind-key drop
    then
  then
  \ new-sound
  "fmv.snd" mus-next mus-bshort 22050 1 $" set-samples test" 100 new-sound { ind }
  10  3  3 0.1 make-vct set-samples drop
  0 20 ind 0 channel->vct { res }
  res vct( 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 ) vequal unless
    $" 1 set samples 0 for 0.1: %s?" #( res ) snd-display
  then
  ind close-sound drop
  \ edot-product (test008)
  complex-test
  \ delay (test008)
  3 make-delay { gen }
  3 make-delay { gen2 }
  4 :initial-contents #( 1.0 0.5 0.25 0.0 ) make-delay { gen1 }
  4 :initial-contents vct( 1.0 0.5 0.25 0.0 ) make-delay { gen3 }
  gen "delay" $" delay line[3, step]: [0.000 0.000 0.000]" print-and-check
  10 0.0 make-vct map gen i 0.0 delay end-map { v0 }
  10 0.0 make-vct map gen2 delay? if gen2 i 0.0 delay else -1.0 then end-map { v1 }
  v0 v1 vequal unless $" map delay: %s %s?" #( v0 v1 ) snd-display then
  gen delay? unless $" %s not a delay?" #( gen ) snd-display then
  gen mus-length 3 <> if $" delay length: %d?" #( gen mus-length ) snd-display then
  v0 1 vct-ref 0.0 fneq
  v0 4 vct-ref 1.0 fneq ||
  v0 8 vct-ref 5.0 fneq || if $" delay output: %s?" #( v0 ) snd-display then
  gen1 0.0 0.0 delay 1.0  fneq
  gen1 0.0 0.0 delay 0.5  fneq ||
  gen1 0.0 0.0 delay 0.25 fneq ||
  gen1 0.0 0.0 delay 0.0  fneq ||
  gen1 0.0 0.0 delay 0.0  fneq || if
    $" delay with list initial-contents confused" #f snd-display
  then
  gen3 0.0 0.0 delay 1.0  fneq
  gen3 0.0 0.0 delay 0.5  fneq ||
  gen3 0.0 0.0 delay 0.25 fneq ||
  gen3 0.0 0.0 delay 0.0  fneq ||
  gen3 0.0 0.0 delay 0.0  fneq || if
    $" delay with vct initial-contents confused" #f snd-display
  then
  :size #f <'> make-delay #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'wrong-type-arg object-equal? unless
    $" make-delay bad size false: %s" #( res ) snd-display
  then
  make-oscil { osc }
  3 :initial-element osc <'> make-delay #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'wrong-type-arg object-equal? unless
    $" make-delay bad initial element: %s" #( res ) snd-display
  then
  -3 <'> make-delay #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'out-of-range object-equal? unless
    $" make-delay bad size: %s" #( res ) snd-display
  then
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
  3 :initial-contents #( 1.0 0.0 0.0 ) make-delay to d1
  3 :initial-contents #( 1.0 0.0 0.0 ) make-delay to d2
  3 :initial-contents #( 1.0 1.0 1.0 ) make-delay to d3
  d1 d2 d3 test-gen-equal
  \ mix (test009)
  "hiho.wave" mus-next mus-bshort 22050 1 new-sound { new-index }
  new-index select-sound drop
  0 new-index 0 find-mix to res
  res if $" found non-existent mix: %s?" #( res ) snd-display then
  "pistol.snd" 100 mix car { mix-id }
  mix-id mix? unless $" %s not mix?" #( mix-id ) snd-display then
  view-mixes-dialog    { wid }
  mix-id mix-position  { pos }
  mix-id mix-length    { len }
  mix-id mix-speed     { spd }
  mix-id mix-home      { home-lst }
  home-lst 0 array-ref { snd }
  home-lst 1 array-ref { chn }
  mix-id mix-amp       { amp }
  mix-id make-mix-sampler { mr }
  mr mix-sampler? unless $" %s is not mix-sampler?"  #( mr ) snd-display then
  mr region-sampler?  if $" mix-sampler: region %s?" #( mr ) snd-display then
  mr sampler-position to res
  res 0<> if $" mix sampler-position: %d?" #( res ) snd-display then
  mr sampler-at-end? if $" mix sampler-at-end: %s?" #( mr ) snd-display then
  mr sampler-home to res
  mix-id res object-equal? unless $" mix sampler-home: %d %s?" #( res mr ) snd-display then
  mr object->string 0 16 string-substring to res
  res $" #<mix-sampler mi" string<> if
    $" mix sampler actually got: [%s]?" #( res ) snd-display
  then
  1234 integer->mix <'> mix-amp #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'no-such-mix object-equal? unless
    $" mix-amp bad id: %s" #( res ) snd-display
  then
  1234 integer->mix 0.1 <'> set-mix-amp #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'no-such-mix object-equal? unless
    $" set-mix-amp bad id: %s" #( res ) snd-display
  then
  1234 integer->mix #( 0 0 1 1 ) <'> set-mix-amp-env #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'no-such-mix object-equal? unless
    $" set-mix-amp-env bad id: %s" #( res ) snd-display
  then
  0.0 0.0 { mx sx }
  99 0 do
    i odd? if mr read-mix-sample else mr read-mix-sample then to mx
    100 i + sample to sx
    mx sx fneq if $" read-mix-sample: %s %s?" #( mx sx ) snd-display then
  loop
  \ Scheme: (mr)
  \ Ruby:   mr.call
  \ Forth:  mr #() apply
  mr #() object-apply to mx
  199 sample to sx
  mx sx fneq if $" read-mix-sample 100: %s %s?" #( mx sx ) snd-display then
  mr free-sampler drop
  \
  100 pos <>   if $" mix-position: %d?"     #( pos ) snd-display then
  41623 len <> if $" mix-length: %d?"       #( len ) snd-display then
  snd new-index object-equal? unless $" snd mix-home: %s?" #( snd ) snd-display then
  chn      0<> if $" chn mix-home: %d?"     #( chn ) snd-display then
  amp 1.0 fneq if $" mix-amp: %s?"          #( amp ) snd-display then
  spd 1.0 fneq if $" mix-speed: %s?"        #( spd ) snd-display then
  mix-id <'> play 'mus-error nil fth-catch to res
  stack-reset
  res false? not if $" can't play mix: %s" #( res ) snd-display then
  mix-id 200 set-mix-position drop
  mix-id 0.5 set-mix-amp drop
  mix-id 2.0 set-mix-speed drop
  \ 
  mix-id #( 0 0 1 1 ) set-mix-amp-env drop
  mix-id mix-amp-env to res
  mix-id res set-mix-amp-env drop
  mix-id mix-amp-env { res1 }
  res res1 vequal unless $" set-mix-amp-env to self: %s %s?" #( res res1 ) snd-display then
  mix-id 20 set-mix-tag-y drop
  mix-id mix-position to pos
  mix-id mix-speed    to spd
  mix-id mix-amp      to amp
  mix-id mix-tag-y { my }
  200 pos <>   if $" set-mix-position: %d?" #( pos ) snd-display then
  spd 2.0 fneq if $" set-mix-speed: %s?"    #( spd ) snd-display then
  my  20    <> if $" set-mix-tag-y: %d?"    #( my )  snd-display then
  amp 0.5 fneq if $" set-mix-amp: %s?"      #( amp ) snd-display then
  mix-id mix-amp-env to res
  res #( 0.0 0.0 1.0 1.0 ) array= unless $" set-mix-amp-env: %s?" #( res ) snd-display then
  \
  3 0.1 make-vct 100 #f #f #t "" mix-vct drop
  0 set-cursor drop
  100 #f #f find-mix { nid }
  nid mix? false? unless
    nid mix-position 100 <> if
      new-index 0 mixes map *key* mix-position end-map { mx-pos }
      $" 100 find-mix: %s %s %s?" #( nid dup mix-position mx-pos ) snd-display
    then
  else
    $" 100 find-mix: not a mix %s?" #( nid ) snd-display
  then
  200 #f #f find-mix to nid
  nid mix? false? unless
    nid mix-position 200 <> if
      new-index 0 mixes map *key* mix-position end-map { mx-pos }
      $" 200 find-mix: %s %s %s?" #( nid dup mix-position mx-pos ) snd-display
    then
  else
    $" 200 find-mix: not a mix %s?" #( nid ) snd-display
  then
  \
  "oboe.snd" 100 mix car to mix-id
  40 set-mix-waveform-height drop
  'hiho mix-id 123 set-mix-property
  'hiho mix-id mix-property to res
  res 123 <> if $" mix-property: %s?" #( res ) snd-display then
  'not-here mix-id mix-property to res
  res if $" mix-property not-here: %s?" #( res ) snd-display then
  #f #f update-time-graph drop
  20 set-mix-waveform-height drop
  new-index revert-sound drop
  new-index close-sound drop
  wid hide-widget drop
  \ envelopes (lists, vcts, arrays) (test015)
  1.0 vct( 0.0 0.0 2.0 1.0 )           1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  1.0 #( 0.0 0.0 1.0 1.0 2.0 0.0 )     1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  2.0 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  0.0 #( 1.0 0.5 2.0 0.0 )             1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  0.0 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )   1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0; %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 0.0 0.0 1.0 1.0 -1.0 )  1.0 envelope-interp dup 0.5 fneq if
    $" envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 0.0 fneq if
    $" envelope-interp 0.0: %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -0.5 fneq if
    $" envelope-interp -0.5: %s?" swap snd-display
  else
    drop
  then
  -1.5 #( -1.0 -1.0 1.0 1.0 )          1.0 envelope-interp dup -1.0 fneq if
    $" envelope-interp -1.0: %s?" swap snd-display
  else
    drop
  then
  1.5 #( -1.0 -1.0 1.0 1.0 )           1.0 envelope-interp dup 1.0 fneq if
    $" envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )             1.0 envelope-interp dup 0.1 fneq if
    $" envelope-interp 0.1: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )            32.0 envelope-interp dup 0.01336172 fneq if
    $" envelope-interp (exp 32): %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 )           0.012 envelope-interp dup 0.36177473 fneq if
    $" envelope-interp (exp 0.012): %s?" swap snd-display
  else
    drop
  then
  0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 )     1.0 envelope-interp dup 0.6 fneq if
    $" envelope-interp 0.6: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.5 ) { v0 }
  #( 0.0 0.0 2.0 0.5 ) #( 0.0 0.0 1.0 2.0 2.0 1.0 ) multiply-envelopes dup v0 0 fveql unless
    $" multiply-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) #( 0.0 0.0 1.0 1.0 2.0 0.0 ) multiply-envelopes dup v0 0 fveql unless
    $" multiply-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) max-envelope dup 3.0 fneq if
    $" 0 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 ) max-envelope dup 1.0 fneq if
    $" 1 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) max-envelope dup 2.0 fneq if
    $" 2 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) max-envelope dup -1.0 fneq if
    $" 3 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) max-envelope dup -1.0 fneq if
    $" 4 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) min-envelope dup 0.0 fneq if
    $" 0 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 ) min-envelope dup 1.0 fneq if
    $" 1 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) min-envelope dup 1.0 fneq if
    $" 2 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) min-envelope dup -2.0 fneq if
    $" 3 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) min-envelope dup -2.0 fneq if
    $" 4 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 0.1 0.2 #f #f stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope att: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope dec: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup v0 0 fveql unless
    $" stretch-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 1.5 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) #( 0.0 0.0 1.0 1.0 ) add-envelopes dup v0 0 fveql unless
    $" add-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 2.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 2.0 scale-envelope dup v0 0 fveql unless
    $" scale-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 0.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    $" reverse-envelope: %s?" swap snd-display
  else
    drop
  then
;

\ ====== test 10: marks
: 10-marks ( -- )
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
      $" saved mark 123 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync zero? unless
      $" saved mark 123 sync: %s?" #( m mark-sync ) snd-display
    then
  else
    $" saved marks missed 123: %s?" #( m ) snd-display
  then
  234 ind 0 find-mark to m
  m mark? if
    m mark-name $" hiho" string<> if
      $" saved mark 234 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync { m2sync }
    m2sync 0= m2sync 1 = || if
      $" saved mark 234 sync: %s?" #( m mark-sync ) snd-display
    then
    m mark-sync
  else
    $" saved marks missed 234: %s?" #( m ) snd-display
    0
  then { m1-sync }
  345 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      $" saved mark 345 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      $" saved mark 345 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 345: %s?" #( m ) snd-display
  then
  456 ind 0 find-mark to m
  m mark? if
    m mark-name $" a mark" string<> if
      $" saved mark 456 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync { m4sync }
    m4sync m1-sync = m4sync 0= || m4sync 1 = || if
      $" saved mark 456 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 456: %s?" #( m ) snd-display
  then
  567 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      $" saved mark 567 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      $" saved mark 567 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    $" saved marks missed 567: %s?" #( m ) snd-display
  then
  ind close-sound drop
;

\ ====== test 15: chan-local vars
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
  then pk0loc f+  #f srate f*  size f/  peak0 2 >array
;
: src-test15-cb ( os -- proc; y self -- val )
  1 proc-create swap ,
 does> { y self -- val }
  self @ ( os ) 0.0 0.0 oscil f2/
;

: f3neq ( a b -- f ) f- fabs 10.0 f> ;
: f4neq ( a b -- f ) f- fabs  1.0 f> ;
: f5neq ( a b -- f ) { a b } a b f- fabs 10.0  a b fmax 0.05 f*  f> ;

'complex provided? [if]
  \ dolph/dolph-1 are only defined if complex numbers available
  : dolph-test ( -- )
    16 1.0 dolph { val1 }
    dolph-chebyshev-window 16 1.0 make-fft-window { val2 }
    val1 val2 vequal unless
      $" dolph/dolph 1: %s %s" #( val1 val2 ) snd-display
    then
    16 1.0 dolph-1 to val1
    val1 val2 vequal unless
      $" dolph-1/dolph 1: %s %s" #( val1 val2 ) snd-display
    then
  ;
[else]
  <'> noop alias dolph-test
[then]

: 15-chan-local-vars ( -- )
  \ dsp.fs
  "test.snd" mus-next mus-bfloat 22050 1 $" src-* tests" 10000 new-sound { ind }
  \ src-duration tests
  #( 0 1 1 2 )      src-duration { d1 }
  #( 0 2 1 1 )      src-duration { d2 }
  #( 0 1 0.5 2 )    src-duration { d3 }
  #( 0.5 1 0.75 2 ) src-duration { d4 }
  d1 0.693147180559945 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    $" src-duration test1: %f %f %f %f" #( d1 d2 d3 d4 ) snd-display
  then
  #( 0 1 1 0.5 )      src-duration to d1
  #( 0 0.5 1 1 )      src-duration to d2
  #( 0 1 0.5 0.5 )    src-duration to d3
  #( 0.5 1 0.75 0.5 ) src-duration to d4
  d1 1.38629436111989 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    $" src-duration test2: %f %f %f %f" #( d1 d2 d3 d4 ) snd-display
  then
  #( 0 1 1 1 ) src-duration to d1
  #( 0 2 1 2 ) src-duration to d2
  d1 1.0 fneq
  d2 0.5 fneq || if
    $" src-duration test3: %f %f" #( d1 d2 ) snd-display
  then
  #( 0 0.5 0.5 3 0.6 1 0.7 0.1 0.8 1.5 1 1 ) src-duration to d1
  d1 1.02474349685432 fneq if
    $" src-duration test4: %f" #( d1 ) snd-display
  then
  #( 0 1 1 2 2 1 ) src-duration to d1
  d1 0.693147180559945 fneq if
    $" src-duration test5: %f" #( d1 ) snd-display
  then
  500.0 0.0 make-oscil src-test15-cb map-channel drop
  0 ind 8192 freq-peak { vals }
  500.0 vals 0 array-ref f4neq
  1.0   vals 1 array-ref fneq || if
    $" src no-test: %s" #( vals ) snd-display
  then
  ind close-sound drop
  \
  dolph-test
  \ env.fs
  \ envelope-interp
  0.1 #( 0 0 1 1 ) 1.0 envelope-interp dup 0.1 fneq if
    $" envelope-interp 0.1: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0 0 1 1 ) 32.0 envelope-interp dup 0.01336172 fneq if
    $" envelope-interp 0.013: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0 0 1 1 ) 0.012 envelope-interp dup 0.36177473 fneq if
    $" envelope-interp 0.361: %s?" swap snd-display
  else
    drop
  then
  0.3 #( 0 0 0.5 1 1 0 ) 1.0 envelope-interp dup 0.6 fneq if
    $" envelope-interp 0.3 #( 0 0 0.5 1 1 0 ): %s?" swap snd-display
  else
    drop
  then
  \ window-envelope
  1.0 3.0 #( 0.0 0.0 5.0 1.0 ) window-envelope dup #( 1.0 0.2 3.0 0.6 ) feql if
    drop
  else
    1 >array $" window-envelope: %s?" swap snd-display
  then
  \ multiply-envelopes
  #( 0 0 1 1 ) #( 0 0 1 1 2 0 ) multiply-envelopes dup #( 0 0 0.5 0.5 1 0 ) feql if
    drop
  else
    1 >array $" multiply-envelopes: %s?" swap snd-display
  then
  \ max-envelope
  #( 0 0 1 1 2 3 4 0 ) max-envelope dup 3.0 fneq if
    $" max-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 ) max-envelope dup 1.0 fneq if
    $" max-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 1 1 2 2 ) max-envelope dup 2.0 fneq if
    $" max-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  #( 0 -1 1 -2 ) max-envelope dup -1.0 fneq if
    $" max-envelopes (3): %s?" swap snd-display
  else
    drop
  then
  #( 0 -2 1 -1 ) max-envelope dup -1.0 fneq if
    $" max-envelopes (4): %s?" swap snd-display
  else
    drop
  then
  \ min-envelope
  #( 0 0 1 1 2 3 4 0 ) min-envelope dup 0.0 fneq if
    $" min-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 ) min-envelope dup 1.0 fneq if
    $" min-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 1 1 2 2 ) min-envelope dup 1.0 fneq if
    $" min-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  #( 0 -1 1 -2 ) min-envelope dup -2.0 fneq if
    $" min-envelopes (3): %s?" swap snd-display
  else
    drop
  then
  #( 0 -2 1 -1 ) min-envelope dup -2.0 fneq if
    $" min-envelopes (4): %s?" swap snd-display
  else
    drop
  then
  \ integrate-envelope
  #(  0 0 1 1 ) integrate-envelope dup 0.5 fneq if
    $" integrate-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #(  0 1 1 1 ) integrate-envelope dup 1.0 fneq if
    $" integrate-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #(  0 0 1 1 2 0.5 ) integrate-envelope dup 1.25 fneq if
    $" integrate-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  \ stretch-envelope
  #(  0 0 1 1 ) 0.1 0.2 #f #f stretch-envelope dup #( 0 0 0.2 0.1 1.0 1 ) feql if
    drop
  else
    1 >array $" stretch-envelope att: %s?" swap snd-display
  then
  #( 0 0 1 1 2 0 ) 0.1 0.2 1.5 1.6 stretch-envelope dup #( 0 0 0.2 0.1 1.1 1 1.6 0.5 2 0 ) feql if
    drop
  else
    1 >array $" stretch-envelope dec: %s?" swap snd-display
  then
  \ add-envelopes
  #( 0 0 1 1 2 0 ) #( 0 0 1 1 ) add-envelopes dup #( 0 0 0.5 1.5 1 1 ) feql if
    drop
  else
    1 >array $" add-envelopes: %s?" swap snd-display
  then
  \ scale-envelope
  #( 0 0 1 1 ) 2 0 scale-envelope dup #( 0 0 1 2 ) feql if
    drop
  else
    1 >array $" scale-envelope: %s?" swap snd-display
  then
  #( 0 0 1 1 ) 2 1 scale-envelope dup #( 0 1 1 3 ) feql if
    drop
  else
    1 >array $" scale-envelope off: %s?" swap snd-display
  then
  \ reverse-envelope
  #( 0 0 1 1 ) reverse-envelope dup #( 0 1 1 0 ) feql if
    drop
  else
    1 >array $" reverse-envelope ramp: %s?" swap snd-display
  then
  #( 0 0 0.5 1 2 0 ) reverse-envelope dup #( 0 0 1.5 1 2 0 ) feql if
    drop
  else
    1 >array $" reverse-envelope ramp 2: %s?" swap snd-display
  then
  #( 0 0 0.5 1 2 1 ) reverse-envelope dup #( 0 1 1.5 1 2 0 ) feql if
    drop
  else
    1 >array $" reverse-envelope ramp 2: %s?" swap snd-display
  then
  \ concatenate-envelopes (from snd/env.scm)
  #( 0 0 1 1 ) #( 0 1 1 0 ) 2 concatenate-envelopes dup #( 0.0 0 1.0 1 2.0 0 ) feql if
    drop
  else
    1 >array $" concatenate-envelopes (0): %s?" swap snd-display
  then
  #( 0 0 1 1.5 ) #( 0 1 1 0 ) 2 concatenate-envelopes dup #( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array $" concatenate-envelopes (1): %s?" swap snd-display
  then
  \ envelope-concatenate (from clm/env.lisp)
  #( 0 0 1 1 ) #( 0 1 1 0 ) 2 envelope-concatenate dup #( 0.0 0 1.0 1 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array $" envelope-concatenate (0): %s?" swap snd-display
  then
  #( 0 0 1 1.5 ) #( 0 1 1 0 ) 2 envelope-concatenate dup #( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array $" envelope-concatenate (1): %s?" swap snd-display
  then
  \ repeat-envelope
  #( 0 0 1 100 ) 2 #f #f repeat-envelope dup #( 0 0 1 100 1.01 0 2.01 100 ) feql if
    drop
  else
    1 >array $" repeat-envelope (0): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #f #f repeat-envelope dup #( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 ) feql if
    drop
  else
    1 >array $" repeat-envelope (1): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #f #t repeat-envelope dup #( 0.0 0 0.75 1 1.0 0 1.75 1 2.0 0 ) feql if
    drop
  else
    1 >array $" repeat-envelope (2): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #t #f repeat-envelope dup #( 0 0 1.5 1 2.0 0 2.5 1 4.0 0 ) feql if
    drop
  else
    1 >array $" repeat-envelope (3): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 3 #f #f repeat-envelope dup
  #( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 5.5 1 6.0 0 ) feql if
    drop
  else
    1 >array $" repeat-envelope (4): %s?" swap snd-display
  then
  \ normalize-envelope
  #( 0 0 1 1.5 2.0 1.0 ) normalize-envelope dup #( 0 0.0 1 1.0 2.0 0.667 ) feql if
    drop
  else
    1 >array $" normalize-envelope (0): %s?" swap snd-display
  then
  #( 0 0 1 0.5 2 -0.8 ) normalize-envelope dup #( 0 0.0 1 0.625 2 -1.0 ) feql if
    drop
  else
    1 >array $" normalize-envelope (1): %s?" swap snd-display
  then
  \ envelope-exp
  #( 0 0 1 1 ) 2.0 10 envelope-exp dup
  #( 0 0 0.1 0.01 0.2 0.04 0.3 0.09 0.4 0.16 0.5 0.25 0.6 0.36 0.7 0.49 0.8 0.64 0.9 0.81 1 1 )
  feql if
    drop
  else
    1 >array $" envelope-exp (0): %s?" swap snd-display
  then
  #( 0 0 1 1 2 0 ) 1.0 10 envelope-exp dup
  #( 0 0 0.2 0.2 0.4 0.4 0.6 0.6 0.8 0.8 1 1 1.2 0.8 1.4 0.6 1.6 0.4 1.8 0.2 2 0 )
  feql if
    drop
  else
    1 >array $" envelope-exp (1): %s?" swap snd-display
  then
;

\ ====== test 19: save and restore
: clm-channel-test <{ :optional snd #f chn #f -- gen }>
  1 -1 make-two-zero 0 #f snd chn #f #f get-func-name clm-channel
;

: random-pi-func <{ x -- y }> pi random ;
lambda: <{ x -- y }> pi random ; value random-pi-addr

#( #( lambda: <{ -- val }> vct( 1.0 0.5 ) 0 2 #f #f #f insert-vct ;
      $" lambda: <{ snd chn -- val }> vct( 1.000 0.500 ) 0 2 snd chn insert-vct drop ;"
      "insert-vct" )
   #( lambda: <{ -- val }> #f #f clm-channel-test ;
      $" lambda: <{ snd chn -- val }>  snd chn clm-channel-test drop ;"
      "clm-channel-test" )
   ( examp.fs )
   #( lambda: <{ -- val }> 1000 3000 #f #f fft-edit ;
      $" lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-edit drop ;"
      "fft-edit" )
   #( lambda: <{ -- val }> 0.01 #f #f fft-squelch ;
      $" lambda: <{ snd chn -- val }> 0.01 snd chn fft-squelch drop ;"
      "fft-sqelch" )
   #( lambda: <{ -- val }> 1000 3000 #f #f fft-cancel ;
      $" lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-cancel drop ;"
      "fft-cancel" )
   #( lambda: <{ -- val }> #f #f squelch-vowels ;
      $" lambda: <{ snd chn -- val }>  snd chn squelch-vowels drop ;"
      "squelch-vowels" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) #f #f fft-env-edit ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) snd chn fft-env-edit drop ;"
      "fft-env-edit" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) #( 0 1 1 0 2 0 ) #( 0 0 1 1 ) #f #f fft-env-interp ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) #( 0 1 1 0 2 0 ) #( 0 0 1 1 ) snd chn fft-env-interp drop ;"
      "fft-env-interp" )
   #( lambda: <{ -- val }> 10 0.1 #f #f hello-dentist ;
      $" lambda: <{ snd chn -- val }> 10 0.1 snd chn hello-dentist drop ;"
      "hello-dentist" )
   #( lambda: <{ -- val }> 1 0.3 20 #f #f fp ;
      $" lambda: <{ snd chn -- val }> 1 0.3 20 snd chn fp drop ;"
      "fp" )
   #( lambda: <{ -- val }> #( 0 1 1 2 ) #f #f expsnd ;
      $" lambda: <{ snd chn -- val }> #( 0 1 1 2 ) snd chn expsnd drop ;"
      "expsnd" )
   #( lambda: <{ -- val }> 1 256 2 2 #f #f voiced->unvoiced ;
      $" lambda: <{ snd chn -- val }> 1 256 2 2 snd chn voiced->unvoiced drop ;"
      "voiced->unvoided" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) 2 #f #f env-sound-interp ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) 2 snd chn env-sound-interp drop ;"
      "env-sound-interp" )
   #( lambda: <{ -- val }> #( #( "1a.snd" ) #( "pistol.snd" 1 2 ) ) #f #f add-notes ;
      $" lambda: <{ snd chn -- val }> #( #( \"1a.snd\" ) #( \"pistol.snd\" 1 2 ) ) snd chn add-notes drop ;"
      "add-notes" )
   #( lambda: <{ -- val }> 0 #f #f #f compand-channel ;
      $" lambda: <{ snd chn -- val }> 0 #f snd chn compand-channel drop ;"
      "compand-channel" )
   #( lambda: <{ -- val }> 0 #f #f #f #f smooth-channel-via-ptree ;
      $" lambda: <{ snd chn -- val }> 0 #f snd chn smooth-channel-via-ptree drop ;"
      "smooth-channel-via-ptree" )
   #( lambda: <{ -- val }> 300 0 #f #f #f #f ring-modulate-channel ;
      $" lambda: <{ snd chn -- val }> 300 0 #f snd chn ring-modulate-channel drop ;"
      "ring-modulate-channel" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) #f #f filtered-env ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) snd chn filtered-env drop ;"
      "filtered-env" )
   #( lambda: <{ -- val }> 0.1 #f #f reverse-by-blocks ;
      $" lambda: <{ snd chn -- val }> 0.1 snd chn reverse-by-blocks drop ;"
      "reverse-by-blocks" )
   #( lambda: <{ -- val }> 0.1 #f #f reverse-within-blocks ;
      $" lambda: <{ snd chn -- val }> 0.1 snd chn reverse-within-blocks drop ;"
      "reverse-within-blocks" )
   ( extensions.fs )
   #( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f mix-channel ;
      $" lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn mix-channel drop ;"
      "mix-channel" )
   #( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f insert-channel ;
      $" lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn insert-channel drop ;"
      "insert-channel" )
   #( lambda: <{ -- val }> "1a.snd" 0.5 0.9 0 #f #f #f #f sine-ramp ;
      $" lambda: <{ snd chn -- val }> 0.5 0.9 0 #f snd chn sine-ramp drop ;"
      "sine-ramp" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f sine-env-channel ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn sine-env-channel drop ;"
      "sine-env-channel" )
   #( lambda: <{ -- val }> 0 1 0 #f #f #f #f blackman4-ramp ;
      $" lambda: <{ snd chn -- val }> 0 1 0 #f snd chn blackman4-ramp drop ;"
      "blackman4-ramp" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f blackman4-env-channel ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn blackman4-env-channel drop ;"
      "blackman4-env-channel" )
   #( lambda: <{ -- val }> 0.2 0.8 #t 0 #f #f #f #f ramp-squared ;
      $" lambda: <{ snd chn -- val }> 0.2 0.8 #t 0 #f snd chn ramp-squared drop ;"
      "ramp-squared" )
   #( lambda: <{ -- val }> #( 0 0 1 1 ) #t 0 #f #f #f #f env-squared-channel ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 ) #t 0 #f snd chn env-squared-channel drop ;"
      "env-squared-channel" )
   #( lambda: <{ -- val }> 0.2 0.8 32 #t 0 #f #f #f #f ramp-expt ;
      $" lambda: <{ snd chn -- val }> 0.2 0.8 32 #t 0 #f snd chn ramp-expt drop ;"
      "ramp-expt" )
   #( lambda: <{ -- val }> #( 0 0 1 1 ) 32 #t 0 #f #f #f #f env-expt-channel ;
      $" lambda: <{ snd chn -- val }> #( 0 0 1 1 ) 32 #t 0 #f snd chn env-expt-channel drop ;"
      "env-expt-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f offset-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn offset-channel drop ;"
      "offset-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f dither-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn dither-channel drop ;"
      "dither-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f contrast-channel ;
      $" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn contrast-channel drop ;"
      "contrast-channel" )
   ( dsp.fs )
   #( lambda: <{ -- val }> 550 600 10 40 50 0 #f #f #f #f ssb-bank ;
      $" lambda: <{ snd chn -- val }> 550 600 10 40 50 0 #f snd chn ssb-bank drop ;"
      "ssb-bank" )
   #( lambda: <{ -- val }> 550 600 #( 0 1 1 2 ) 10 40 50 0 #f #f #f #f ssb-bank-env ;
      $" lambda: <{ snd chn -- val }> 550 600 #( 0 1 1 2 ) 10 40 50 0 #f snd chn ssb-bank-env drop ;"
      "ssb-bank-env" )
   #( lambda: <{ -- val }> 1 #f #f down-oct ;
      $" lambda: <{ snd chn -- val }> 1 snd chn down-oct drop ;"
      "donw-oct" )
   #( lambda: <{ -- val }> 8 #f #f freqdiv ;
      $" lambda: <{ snd chn -- val }> 8 snd chn freqdiv drop ;"
      "freqdiv" )
   #( lambda: <{ -- val }> 8 0 #f #f #f adsat ;
      $" lambda: <{ snd chn -- val }> 8 0 #f snd chn adsat drop ;"
      "adsat" )
   #( lambda: <{ -- val }> #f #f spike ;
      $" lambda: <{ snd chn -- val }>  snd chn spike drop ;"
      "spike" )
   #( lambda: <{ -- val }> #f #f zero-phase ;
      $" lambda: <{ snd chn -- val }>  snd chn zero-phase drop ;"
      "zero-phase" )
   #( lambda: <{ -- val }> <'> random-pi-func #f #f rotate-phase ;
      $" lambda: <{ snd chn -- val }> <'> random-pi-func snd chn rotate-phase drop ;"
      "rotate-phase-proc" )
   #( lambda: <{ -- val }> random-pi-addr #f #f rotate-phase ;
      $" lambda: <{ snd chn -- val }> <'> %s snd chn rotate-phase drop ;" #( random-pi-addr ) format
      "rotate-phase-lambda" )
   #( lambda: <{ -- val }> 0.5 #f #f brighten-slightly ;
      $" lambda: <{ snd chn -- val }> 0.5 snd chn brighten-slightly drop ;"
      "brighten-slightly" )
   #( lambda: <{ -- val }> 100 40 0 #f #f #f #f shift-channel-pitch ;
      $" lambda: <{ snd chn -- val }> 100 40 0 #f snd chn shift-channel-pitch drop ;"
      "shift-channel-pitch" )
   #( lambda: <{ -- val }> vct( 0.0 0.5 ) #f #f channel-polynomial ;
      $" lambda: <{ snd chn -- val }> vct( 0.000 0.500 ) snd chn channel-polynomial drop ;"
      "channel-polynomial" )
   #( lambda: <{ -- val }> vct( 0.0 1.0 ) #f #f spectral-polynomial ;
      $" lambda: <{ snd chn -- val }> vct( 0.000 1.000 ) snd chn spectral-polynomial drop ;"
      "spectral-polynomial" )
   #( lambda: <{ -- val }> #( 60.0 120.0 240.0 ) #f 0 #f #f #f #f #t 2 notch-channel ;
      $" lambda: <{ snd chn -- val }> #( 60.0 120.0 240.0 ) #f 0 #f snd chn notch-channel drop ;"
      "notch-channel" )
   'snd-motif provided? under-valgrind not && [if]
     ( effects.fs )
     #( lambda: <{ -- val }> 0.1 128 effects-squelch-channel ;
	$" lambda: <{ snd chn -- val }> 0.1 128 snd chn effects-squelch-channel drop ;"
	"effects-sqelch-channel" )
     #( lambda: <{ -- val }> #f 0.5 0.1 0 #f #f #f effects-echo ;
	$" lambda: <{ snd chn -- val }> #f 0.5 0.1 0 #f snd chn effects-echo drop ;"
	"effects-echo" )
     #( lambda: <{ -- val }> 0.5 0.1 #f 0 #f #f #f effects-flecho ;
	$" lambda: <{ snd chn -- val }> 0.5 0.1 #f 0 #f snd chn effects-flecho drop ;"
	"effects-flecho" )
     #( lambda: <{ -- val }> 0.75 0.75 6.0 10.0 #f 0 #f #f #f effects-zecho ;
	$" lambda: <{ snd chn -- val }> 0.75 0.75 6.0 10.0 #f 0 #f snd chn effects-zecho drop ;"
	"effects-zecho" )
     #( lambda: <{ -- val }> 0.1 50 0 #f #f #f effects-comb-filter ;
	$" lambda: <{ snd chn -- val }> 0.1 50 0 #f snd chn effects-comb-filter drop ;"
	"effects-comb-filter" )
     #( lambda: <{ -- val }> 10000 0.5 0 #f #f #f effects-moog ;
	$" lambda: <{ snd chn -- val }> 10000 0.5 0 #f snd chn effects-moog drop ;"
	"effects-moog" )
     #( lambda: <{ -- val }> #f #f effects-remove-dc ;
	$" lambda: <{ snd chn -- val }>  snd chn effects-remove-dc drop ;"
	"effects-remove-dc" )
     #( lambda: <{ -- val }> #f #f effects-compand ;
	$" lambda: <{ snd chn -- val }>  snd chn effects-compand drop ;"
	"effects-compand" )
     #( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-am ;
	$" lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-am drop ;"
	"effects-am" )
     #( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-rm ;
	$" lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-rm drop ;"
	"effects-rm" )
     #( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbp ;
	$" lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbp drop ;"
	"effects-bbp" )
     #( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbr ;
	$" lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbr drop ;"
	"effects-bbr" )
     #( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-bhp ;
	$" lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-bhp drop ;"
	"effects-bhp" )
     #( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-blp ;
	$" lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-blp drop ;"
	"effects-blp" )
     #( lambda: <{ -- val }> 50.0 0.5 0 #f #f #f effects-hello-dentist ;
	$" lambda: <{ snd chn -- val }> 50.0 0.5 0 #f snd chn effects-hello-dentist drop ;"
	"effects-hello-dentist" )
     #( lambda: <{ -- val }> 1.0 0.3 20.0 0 #f #f #f effects-fp ;
	$" lambda: <{ snd chn -- val }> 1.0 0.3 20.0 0 #f snd chn effects-fp drop ;"
	"effects-fp" )
     #( lambda: <{ -- val }> 5.0 2.0 0.001 0 #f #f #f effects-flange ;
	$" lambda: <{ snd chn -- val }> 5.0 2.0 0.001 0 #f snd chn effects-flange drop ;"
	"effects-flange" )
     #( lambda: <{ -- val }> 0.1 0 #f #f #f effects-jc-reverb-1 ;
	$" lambda: <{ snd chn -- val }> 0.1 0 #f snd chn effects-jc-reverb-1 drop ;"
	"effects-jc-reverb-1" )
   [then]
) value test19-*.fs

: 19-save/restore ( -- )
  \
  "oboe.snd" open-sound { ind }
  'xm provided? 'xg provided? || if
    nil nil nil nil nil { vals func1 descr name func }
    test19-*.fs each to vals
      vals 0 array-ref to func1
      vals 1 array-ref to descr
      vals 2 array-ref to name
      *clm-debug* if name #f snd-test-message then
      func1 #() run-proc drop
      ind #f undef undef edit-list->function to func
      func proc-source-ref descr string<> if
      	$" edit-list->function %d: %s?" #( i func proc-source-ref ) snd-display
      then
      ind revert-sound drop
      func #( ind 0 ) run-proc drop
      ind revert-sound drop
    end-each
  then
  ind close-sound drop
;

\ ====== test 23: with-sound
: test23-notehook { ins start dur -- } $" %14s: %5.2f  %5.2f" #( ins start dur ) snd-test-message ;
: test23-balance ( -- )
  make-rmsgain { rg }
  40 make-rmsgain { rg1 }
  2  make-rmsgain { rg2 }
  #( 0 0 1 1 2 0 )      :length 10000 make-env { e }
  #( 0 0 1 1 )          :length 10000 make-env { e1 }
  #( 0 0 1 1 2 0 10 0 ) :length 10000 make-env { e2 }
  440.0 make-oscil { o }
  10000 0 do
    e env { sig }
    i  rg  sig                    e2 env rmsgain-balance  *output*  outa drop
    i  rg1 sig                    e1 env rmsgain-balance  *output*  outb drop
    i  rg2 o 0.0 0.0 oscil 0.1 f* e2 env rmsgain-balance  *output*  outc drop
  loop
  rg rmsgain-gain-avg 0.98402 fneq if
    $" rmsgain gain-avg: %f (0.98402)?" #( rg rmsgain-gain-avg ) snd-display
  then
  rg2 :rmsg-avgc array-assoc-ref 10000 <> if
    $" rmsgain count: %d (10000)?" #( rg2 :rmsg-avgc array-assoc-ref ) snd-display
  then
;
: test23-ssb-fm ( gen mg -- proc; y self -- val )
  1 proc-create { prc } ( mg ) , ( gen ) , prc
 does> { y self -- val }
  self       @ { mg }
  self cell+ @ { gen }
  gen   mg 0.0 0.0 oscil 0.02 f*  ssb-fm
;

\ examples from sndclm.html
: sndclm-oscil-test ( -- )
  440.0 make-oscil { gen }
  44100 0 do
    i  gen 0 0 oscil  f2/ *output* outa drop
  loop
;
: sndclm-env-test ( -- )
  440.0 make-oscil { gen }
  '( 0 0 0.01 1 0.25 0.1 0.5 0.01 1 0 )
  :scaler 0.5 :length 44100 make-env { ampf }
  44100 0 do
    i  gen 0 0 oscil  ampf env  f* *output*  outa drop
  loop
;
: sndclm-table-lookup-test ( -- )
  440.0 :wave '( 1 0.5  2 0.5 ) #f #f partials->wave make-table-lookup { gen }
  44100 0 do
    i  gen 0 table-lookup  f2/ *output* outa drop
  loop
;
: sndclm-polywave-test ( -- )
  440.0 :partials '( 1 0.5 2 0.5 ) make-polywave { gen }
  44100 0 do
    i  gen 0 polywave  f2/ *output* outa drop
  loop
;
: sndclm-triangle-wave-test ( -- )
  440.0 make-triangle-wave { gen }
  44100 0 do
    i  gen 0 triangle-wave  f2/ *output* outa drop
  loop
;
: sndclm-ncos-test ( -- )
  440.0 10 make-ncos { gen }
  44100 0 do
    i  gen 0 ncos  f2/ *output* outa drop
  loop
;
: sndclm-nrxycos-test ( -- )
  440.0 :n 10 make-nrxycos { gen }
  44100 0 ?do
    i  gen 0 nrxycos  f2/ *output* outa drop
  loop
;
: sndclm-ssb-am-test ( -- )
  440.0 20 make-ssb-am { shifter }
  440.0 make-oscil { osc }
  44100 0 ?do
    i  shifter  osc 0 0 oscil  0 ssb-am f2/ *output* outa drop
  loop
;
: sndclm-wave-train-test ( -- )
  400 10 make-ncos { g }
  g -0.5 pi f* set-mus-phase drop
  64 make-vct map! g 0 ncos end-map { v }
  440.0 :wave v make-wave-train { gen }
  44100 0 do
    i  gen 0 wave-train  f2/ *output* outa drop
  loop
;
: sndclm-rand-test ( -- )
  5.0 220.0 hz->radians make-rand { ran1 }
  5.0 330.0 hz->radians make-rand-interp { ran2 }
   440.0 make-oscil { osc1 }
  1320.0 make-oscil { osc2 }
  88200 0 do
    i  osc1  ran1 0 rand         0 oscil  f2/ *output* outa drop
    i  osc2  ran2 0 rand-interp  0 oscil  f2/ *output* outb drop
  loop
;
: sndclm-two-pole-test ( -- )
  1000.0 0.999 make-two-pole { flt }
  10000.0 0.002 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  two-pole  f2/ *output* outa drop
  loop
;
: sndclm-firmant-test ( -- )
  1000.0 0.999 make-firmant { flt }
  10000.0 5.0 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  #f firmant  f2/ *output* outa drop
  loop
;
: sndclm-iir-filter-test ( -- )
  3 vct( 0.0 -1.978 0.998 ) make-iir-filter { flt }
  10000.0 0.002 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  iir-filter  f2/ *output* outa drop
  loop
;
: sndclm-delay-test ( -- )
  0.5 seconds->samples make-delay { dly }
  440.0 make-oscil { osc1 }
  660.0 make-oscil { osc2 }
  44100 0 do
    i
    osc1 0 0 oscil
    dly  osc2 0 0 oscil  0 delay f+
    f2/ *output* outa drop
  loop
;
: sndclm-comb-test ( -- )
  0.4 0.4 seconds->samples make-comb { cmb }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  88200 0 do
    i
    cmb ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )
    comb f2/ *output* outa drop
  loop
;
: sndclm-all-pass-test ( -- )
  -0.4 0.4 0.4 seconds->samples make-all-pass { alp }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  88200 0 do
    i
    alp ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )
    all-pass f2/ *output* outa drop
  loop
;
: sndclm-moving-average-test ( -- )
  4410 make-moving-average { avg }
  440.0 make-oscil { osc }
  44100 4410 - { stop }
  0.0 { val }
  stop 0 do
    osc 0 0 oscil to val
    i  avg val fabs moving-average  val f* *output* outa drop
  loop
  44100 stop do
    i  avg 0.0 moving-average  osc 0 0 oscil f*  *output* outa drop
  loop
;
: sndclm-src1-test ( -- )
  "oboe.snd" make-readin { rd }
  rd 0.5 make-src { sr }
  "oboe.snd" mus-sound-frames 2* ( len ) 0 do
    i  sr 0 #f src  *output* outa drop
  loop
;
: make-src-proc { osc -- prc; dir self -- val }
  1 proc-create osc , ( prc )
 does> { dir self -- val }
  self @ ( osc ) 0 0 oscil
;
: sndclm-src2-test ( -- )
  440.0 make-oscil { osc }
  osc make-src-proc { prc }
  :srate 2.0 make-src { sr }
  44100 0 do
    i  sr 0 prc src  *output* outa drop
  loop
;
: sndclm-convolve1-test ( -- )
  "pistol.snd" make-readin ( rd )
  "oboe.snd" file->vct ( v ) make-convolve { cnv }
  88200 0 do
    i  cnv #f convolve  0.25 f* *output* outa drop
  loop
;
: sndclm-convolve2-test ( -- )
  "oboe.snd" "pistol.snd" 0.5 "convolved.snd" convolve-files { tempfile }
  tempfile make-readin { reader }
  tempfile mus-sound-frames ( len ) 0 do
    i  reader readin  *output* outa drop
  loop
  tempfile file-delete
;
: sndclm-granulate1-test ( -- )
  "oboe.snd" make-readin 2.0 make-granulate { grn }
  44100 0 do
    i  grn #f #f granulate  *output* outa drop
  loop
;
: make-granulate-proc { osc sweep -- prc; dir self -- val }
  1 proc-create osc , sweep , ( prc )
 does> { dir self -- val }
  self @ ( osc )  self cell+ @ ( sweep ) env  0 oscil  0.2 f*
;
: sndclm-granulate2-test ( -- )
  440.0 make-oscil { osc }
  '( 0 0 1 1 ) :scaler 440.0 hz->radians :length 44100 make-env { sweep }
  osc sweep make-granulate-proc :expansion 2.0 :length 0.5 make-granulate { grn }
  88200 0 do
    i  grn #f #f granulate  *output* outa drop
  loop
;
: sndclm-phase-vocoder1-test ( -- )
  "oboe.snd" make-readin :pitch 2.0 make-phase-vocoder { pv }
  44100 0 do
    i  pv #f #f #f #f phase-vocoder  *output* outa drop
  loop
;
: sndclm-phase-vocoder2-test ( -- )
  "oboe.snd" make-readin :interp 256 make-phase-vocoder { pv }
  "oboe.snd" mus-sound-frames 2* ( samps ) 0 do
    i  pv #f #f #f #f phase-vocoder  *output* outa drop
  loop
;
: sndclm-asymmetric-fm-test ( -- )
  440.0 0.0 0.9 0.5 make-asymmetric-fm { fm }
  44100 0 do
    i  fm 1.0 0 asymmetric-fm  f2/ *output* outa drop
  loop
;
: sndclm-file->frame->file-test ( -- )
  "stereo.snd" make-file->frame { input }
  2 make-frame { frm }
  "stereo.snd" mus-sound-frames ( len ) 0 do
    input i frm file->frame ( frm ) 1 frame-ref ( val1 )
    frm 0 frame-ref ( val0 ) frm 1 rot frame-set! drop
    ( val1 ) frm 0 rot frame-set! drop
    *output* i frm frame->file drop
  loop
;
: sndclm-readin-test ( -- )
  "oboe.snd" make-readin { reader }
  44100 0 do
    i  reader readin  f2/ *output* outa drop
  loop
;
: sndclm-in-out-any-test ( -- )
  "oboe.snd" make-file->sample { infile }
  44100 0 do
    i  i 0 infile in-any  0 *output* out-any drop
  loop
;
: sndclm-locsig-test ( -- )
  60.0 make-locsig { loc }
  440.0 make-oscil { osc }
  44100 0 do
    loc i  osc 0 0 oscil f2/  locsig drop
  loop
;
: sndclm-amplitude-modulate-test ( -- )
  440.0 make-oscil { osc1 }
  220.0 make-oscil { osc2 }
  44100 0 do
    i
    0.3            ( car )
    osc1 0 0 oscil ( in1 )
    osc2 0 0 oscil ( in2 ) amplitude-modulate  f2/ *output* outa drop
  loop
;

include bird.fsm

: ws-close-sound ( ws -- )
   ( ws ) :output array-assoc-ref 0 find-sound { ind }
  ind sound? if ind close-sound drop then
;
: 23-with-sound ( -- )
  1024 1024 * to *clm-file-buffer-size*
  <'> bird-test				\ from bird.fsm
  :statistics *clm-verbose*
  :verbose    *clm-verbose*
  :srate      44100
  :scaled-to  0.8 with-sound ( ws ) ws-close-sound
  \ FIXME: Here a mysterious sound "test.snd" remains open [ms].
  sounds if sounds each ( snd ) close-sound drop end-each then
  0.0 0.3 <'> clm-ins-test		\ from clm-ins.fs
  :notehook   *clm-verbose* if <'> test23-notehook else #f then
  :statistics *clm-verbose*
  :verbose    *clm-verbose*
  :channels 2 with-sound ( ws ) ws-close-sound
  <'> test23-balance :channels 3 with-sound ( ws ) ws-close-sound
  100.0 make-oscil { mg }
  1000 make-ssb-fm { gen }
  "tmp.snd" mus-next mus-bfloat 22050 1 new-sound { ind }
  0 1000 ind 0 pad-channel drop
  gen mg test23-ssb-fm <'> map-channel #t nil fth-catch stack-reset
  ind close-sound drop
  \ examples from sndclm.html
  <'> sndclm-oscil-test              :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-env-test                :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-table-lookup-test       :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-polywave-test           :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-triangle-wave-test      :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-ncos-test               :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-nrxycos-test            :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-ssb-am-test             :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-wave-train-test         :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-rand-test   :channels 2 :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-two-pole-test           :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-firmant-test            :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-iir-filter-test         :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-delay-test              :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-comb-test               :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-all-pass-test           :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-moving-average-test     :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-src1-test  :srate 22050 :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-src2-test               :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-convolve1-test          :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-convolve2-test          :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-granulate1-test         :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-granulate2-test         :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-phase-vocoder1-test     :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-phase-vocoder2-test :srate 22050 :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-asymmetric-fm-test      :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-file->frame->file-test :channels 2 :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-readin-test             :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-in-out-any-test         :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-locsig-test :channels 2 :play *clm-verbose* with-sound ( ws ) ws-close-sound
  <'> sndclm-amplitude-modulate-test :play *clm-verbose* with-sound ( ws ) ws-close-sound
;

\ ====== test 28: errors
: check-error-tag { xt expected-tag -- }
  xt #t nil fth-catch { tag }
  stack-reset
  tag if				\ we ignore #f
    tag car expected-tag = unless
      $" %s: expected %s from %s, got %s?" #( get-func-name expected-tag xt tag ) snd-display
    then
  then
;
'snd-motif provided? [if]
  : snd-motif-error-checks ( -- )
    #( 'Widget 0 ) 	      <'> widget-position     'no-such-widget check-error-tag
    #( 'Widget 0 ) 	      <'> widget-size         'no-such-widget check-error-tag
    #( 'Widget 0 ) 	      <'> widget-text         'no-such-widget check-error-tag
    #( 'Widget 0 ) #( 0 0 )   <'> set-widget-position 'no-such-widget check-error-tag
    #( 'Widget 0 ) #( 10 10 ) <'> set-widget-size     'no-such-widget check-error-tag
    #( 'Widget 0 ) "text"     <'> set-widget-text     'no-such-widget check-error-tag
    #( 'Widget 0 ) 	      <'> hide-widget         'no-such-widget check-error-tag
    #( 'Widget 0 ) 	      <'> show-widget         'no-such-widget check-error-tag
    #( 'Widget 0 ) 	      <'> focus-widget        'no-such-widget check-error-tag
  ;
[else]
  <'> noop alias snd-motif-error-checks
[then]
[ifundef] mus-audio-reinitialize
  : mus-audio-reinitialize ( -- n ) 0 ;
[then]

: pt-test-1 <{ a -- f }>     #f ;
: pt-test-2 <{ a b -- f }>   #f ;
: pt-test-3 <{ a b c -- f }> #f ;

3 :initial-element 1 make-array constant color-95
0 make-array        constant vector-0
3 0.0 make-vct      constant vct-3
5 0.0 make-vct      constant vct-5
2 3 make-sound-data constant sound-data-23
2 make-hook         constant a-hook
#( 0 0 1 1 ) value env3
'snd-nogui provided? [if]
  '( 'XtAppContext 0x54acad81 ) constant car-main
  '( 'Widget 0x554ab501 )       constant cadr-main
[else]
  main-widgets 0 array-ref constant car-main
  main-widgets 1 array-ref constant cadr-main
[then]

: make-identity-mixer <{ chans -- mx }>
  #f { mx }
  chans 256 < if
    chans make-mixer to mx
    mx mixer? if chans 0 do mx i i 1.0 mixer-set! drop loop else #f to mx then
  then
  mx
;

#( <'> make-all-pass <'> make-asymmetric-fm <'> make-snd->sample <'> make-moving-average
   <'> make-comb <'> make-filtered-comb <'> make-convolve <'> make-delay <'> make-env
   <'> make-fft-window <'> make-file->frame <'> make-file->sample <'> make-filter
   <'> make-fir-filter <'> make-formant <'> make-firmant <'> make-frame
   <'> make-frame->file <'> make-granulate <'> make-iir-filter <'> make-locsig
   <'> make-mixer <'> make-notch <'> make-one-pole <'> make-one-zero <'> make-oscil
   <'> make-pulse-train <'> make-rand <'> make-rand-interp <'> make-readin
   <'> make-sample->file <'> make-sawtooth-wave <'> make-nrxysin <'> make-nrxycos
   <'> make-square-wave <'> make-src <'> make-ncos <'> make-nsin <'> make-table-lookup
   <'> make-triangle-wave <'> make-two-pole <'> make-two-zero <'> make-wave-train
   <'> make-phase-vocoder <'> make-ssb-am <'> make-polyshape <'> make-polywave
   <'> make-player <'> make-region <'> make-scalar-mixer ) constant make-procs

#( :frequency :initial-phase :wave :cosines :amplitude :ratio :size :a0 :a1 :a2 :b1 :b2 :input 
   :srate :file :channel :start :initial-contents :initial-element :scaler :feedforward :feedback 
   :max-size :radius :gain :partials :r :a :n :fill-time :order :xcoeffs :ycoeffs :envelope 
   :base :duration :offset :end :direction :degree :distance :reverb :output :fft-size :expansion 
   :length :hop :ramp :jitter :type :format :comment :channels :filter :revout :width :edit 
   :synthesize :analyze :interp :overlap :pitch :distribution :sines :dur ) constant keyargs

#( <'> add-mark <'> add-sound-file-extension <'> add-source-file-extension
   <'> sound-file-extensions <'> sound-file? <'> add-to-main-menu <'> add-to-menu <'> add-transform
   <'> amp-control <'> as-one-edit <'> ask-before-overwrite <'> audio-input-device
   <'> audio-output-device <'> auto-resize <'> auto-update <'> autocorrelate
   <'> axis-info <'> c-g? <'> apply-controls <'> change-samples-with-origin
   <'> channel-style <'> channels <'> chans <'> close-sound
   <'> comment <'> contrast-control <'> contrast-control-amp <'> contrast-control?
   <'> convolve-selection-with <'> convolve-with <'> channel-properties <'> channel-property
   <'> amp-control-bounds
   <'> speed-control-bounds <'> expand-control-bounds <'> contrast-control-bounds
   <'> reverb-control-length-bounds
   <'> reverb-control-scale-bounds <'> cursor-update-interval <'> cursor-location-offset
   <'> auto-update-interval <'> count-matches <'> cursor
   <'> with-tracking-cursor <'> cursor-size <'> cursor-style <'> tracking-cursor-style
   <'> dac-combines-channels <'> dac-size <'> clipping
   <'> data-format <'> data-location <'> data-size <'> default-output-chans
   <'> default-output-data-format <'> default-output-srate <'> default-output-header-type
   <'> define-envelope <'> delete-mark <'> delete-marks <'> forget-region <'> delete-sample
   <'> delete-samples <'> delete-selection <'> display-edits
   <'> edit-fragment <'> edit-position <'> edit-tree <'> edits <'> env-selection
   <'> env-sound <'> enved-envelope <'> enved-base <'> enved-clip?
   <'> enved-in-dB <'> enved-style <'> enved-power <'> enved-target <'> enved-wave? <'> eps-file
   <'> eps-left-margin <'> eps-bottom-margin <'> eps-size <'> expand-control
   <'> expand-control-hop <'> expand-control-jitter <'> expand-control-length
   <'> expand-control-ramp <'> expand-control? <'> fft <'> fft-window-alpha <'> fft-window-beta
   <'> fft-log-frequency <'> fft-log-magnitude <'> transform-size <'> disk-kspace
   <'> transform-graph-type <'> fft-window <'> transform-graph? <'> file-name
   <'> filter-sound <'> filter-control-in-dB <'> filter-control-envelope <'> enved-filter-order
   <'> enved-filter <'> filter-control-in-hz <'> filter-control-order <'> filter-selection
   <'> filter-channel <'> filter-control? <'> find-channel
   <'> find-mark <'> find-sound <'> finish-progress-report <'> frames <'> free-sampler
   <'> graph <'> transform? <'> delete-transform <'> add-watcher <'> delete-watcher
   <'> graph-cursor <'> graph->ps
   <'> gl-graph->ps <'> graph-style <'> lisp-graph? <'>  graphs-horizontal <'> header-type
   <'> in <'> insert-region <'> insert-sample <'> insert-samples
   <'> insert-samples-with-origin <'> insert-selection <'> insert-silence <'> insert-sound
   <'> just-sounds <'> left-sample <'> listener-prompt
   <'> make-mix-sampler <'> make-player <'> make-region <'> make-region-sampler
   <'> make-sampler <'> map-chan
   <'> mark-name <'> mark-properties <'> mark-property
   <'> mark-sample <'> mark-sync <'> mark-sync-max
   <'> mark-home <'> marks <'> mark? <'>  max-transform-peaks
   <'> max-regions <'> maxamp <'> maxamp-position
   <'> minibuffer-history-length <'> min-dB <'> log-freq-start <'> mix
   <'> mixes <'> mix-amp <'> mix-amp-env <'> mix-length
   <'> mix? <'> mix-position <'> mix-properties <'> mix-property
   <'> mix-name <'> mix-region <'> mix-sampler?
   <'> mix-selection <'> mix-sound <'> mix-home <'> mix-speed
   <'> mix-tag-height <'> mix-tag-width <'> mark-tag-height <'> mark-tag-width
   <'> mix-tag-y <'> mix-vct <'> mix-waveform-height <'> time-graph-style
   <'> lisp-graph-style <'> transform-graph-style <'> read-mix-sample
   <'> next-sample <'> transform-normalization <'> open-raw-sound
   <'> open-sound <'> previous-sample <'> peaks <'> player? <'> players
   <'> add-directory-to-view-files-list <'> add-file-to-view-files-list
   <'> view-files-sort <'> view-files-amp <'> view-files-speed <'> view-files-files
   <'> view-files-selected-files <'> view-files-speed-style <'> view-files-amp-env
   <'> print-length <'> progress-report <'> prompt-in-minibuffer <'> read-only
   <'> redo <'> region-chans <'> region-home
   <'> region-graph-style <'> region-frames <'> region-position <'> region-maxamp
   <'> region-maxamp-position <'> selection-maxamp <'> selection-maxamp-position
   <'> region-sample <'> region->vct <'> clear-minibuffer <'> region-srate <'> regions
   <'> region? <'>  remove-from-menu <'> report-in-minibuffer <'> reset-controls
   <'> restore-controls <'> restore-region <'> reverb-control-decay <'> reverb-control-feedback
   <'> reverb-control-length <'> reverb-control-lowpass <'> reverb-control-scale
   <'> reverb-control? <'>  reverse-sound <'> reverse-selection <'> revert-sound
   <'> right-sample <'> sample <'> sampler-at-end? <'>  sampler?
   <'> samples <'> sampler-position <'> save-controls
   <'> ladspa-dir <'> peak-env-dir <'> save-dir <'> save-edit-history <'> save-envelopes
   <'> save-listener <'> save-marks <'> save-region <'> save-selection
   <'> save-sound <'> save-sound-as <'> save-state <'> save-state-file
   <'> scale-by <'> scale-selection-by <'> scale-selection-to <'> scale-to
   <'> scan-chan <'> search-procedure <'> select-all <'> select-channel
   <'> select-sound <'> selected-channel
   <'> selected-sound <'> selection-position <'> selection-creates-region
   <'> selection-frames <'> selection-member? <'> selection? <'> short-file-name
   <'> show-axes <'> show-controls <'> show-transform-peaks
   <'> show-indices <'> show-marks <'> show-mix-waveforms
   <'> show-selection-transform <'> show-y-zero <'> sinc-width <'> show-grid
   <'> show-sonogram-cursor <'> grid-density <'> smooth-sound <'> smooth-selection
   <'> snd-print <'> snd-spectrum <'> snd-tempnam <'> snd-version
   <'> sound-files-in-directory <'> sound-loop-info
   <'> sound? <'> sounds <'> spectrum-end <'> spectro-hop
   <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale <'> spectro-y-angle
   <'> spectro-y-scale <'> spectro-z-angle <'> spectro-z-scale <'> speed-control
   <'> speed-control-style <'> speed-control-tones <'> squelch-update <'> srate
   <'> src-sound <'> src-selection <'> start-progress-report <'> stop-player
   <'> stop-playing <'> swap-channels <'> syncd-marks <'> sync
   <'> sync-max <'> sound-properties <'> sound-property <'> temp-dir <'>  region-sampler?
   <'> transform-sample <'> transform->vct <'> transform-frames <'> transform-type
   <'> trap-segfault <'> with-file-monitor <'> optimization
   <'> undo <'> update-transform-graph <'> update-time-graph <'> update-lisp-graph
   <'> update-sound <'> clm-table-size <'> with-verbose-cursor <'> view-sound <'> wavelet-type
   <'> time-graph? <'>  time-graph-type <'> wavo-hop <'> wavo-trace
   <'> window-height <'> window-width <'> window-x <'> window-y
   <'> with-mix-tags <'> with-relative-panes <'> with-gl
   <'> x-axis-style <'> beats-per-measure <'> beats-per-minute <'> x-bounds
   <'> x-position-slider <'> x-zoom-slider <'> mus-header-type->string
   <'> mus-data-format->string <'> y-bounds <'> y-position-slider
   <'> y-zoom-slider <'> zero-pad <'> zoom-focus-style
   <'> mus-sound-samples <'> mus-sound-frames <'> mus-sound-duration <'> mus-sound-datum-size
   <'> mus-sound-data-location <'> data-size <'> mus-sound-chans <'> mus-sound-srate
   <'> mus-sound-header-type <'> mus-sound-data-format <'> mus-sound-length
   <'> mus-sound-type-specifier
   <'> mus-header-type-name <'> mus-data-format-name <'> mus-sound-comment
   <'> mus-sound-write-date
   <'> mus-bytes-per-sample <'> mus-sound-loop-info
   'snd-nogui [unless] <'> mus-audio-describe [then]
   <'> mus-alsa-squelch-warning <'> mus-sound-maxamp
   <'> mus-sound-maxamp-exists?
   <'> mus-file-prescaler <'> mus-prescaler <'> mus-clipping <'> mus-file-clipping
   <'> mus-header-raw-defaults <'> moving-average <'> moving-average? <'> make-moving-average
   <'> mus-expand-filename <'> make-sound-data <'> sound-data-ref <'> sound-data-set!
   <'> sound-data-scale! <'> sound-data-fill! <'> sound-data? <'> sound-data-length
   <'> sound-data-multiply! <'> sound-data-add! <'> sound-data-offset! <'> sound-data*
   <'> sound-data+ <'> sound-data-copy <'> sound-data-reverse! <'> sound-data-maxamp
   <'> sound-data-chans <'> sound-data->vct <'> vct->sound-data <'> sound-data-peak
   <'> all-pass <'> all-pass? <'> amplitude-modulate <'> array->file
   <'> array-interp <'> mus-interpolate <'> asymmetric-fm <'> asymmetric-fm?
   <'> sound-data->sound-data <'> clear-array <'> comb <'> comb?
   <'> filtered-comb <'> filtered-comb? <'> contrast-enhancement <'> convolution
   <'> convolve <'> convolve? <'> db->linear <'> degrees->radians
   <'> delay <'> delay? <'> dot-product <'> env
   <'> env-interp <'> env? <'> file->array <'> file->frame
   <'> file->frame? <'>  file->sample <'> file->sample? <'> filter
   <'> filter? <'> fir-filter <'> fir-filter? <'> formant
   <'> formant-bank <'> formant? <'> frame* <'> frame+
   <'> frame->file <'> frame->file? <'> frame->frame <'> frame->list
   <'> frame->sample <'> frame-ref <'> frame-set! <'> frame?
   <'> granulate <'> granulate? <'> hz->radians <'> iir-filter
   <'> iir-filter? <'>  in-any <'> ina <'> inb
   <'> linear->db <'> locsig <'> locsig-ref <'> locsig-reverb-ref
   <'> locsig-reverb-set! <'> locsig-set! <'>  locsig? <'> make-all-pass
   <'> make-asymmetric-fm <'> make-comb <'> make-filtered-comb <'> make-convolve
   <'> make-delay <'> make-env <'> make-fft-window <'> make-file->frame
   <'> make-file->sample <'> make-filter <'> make-fir-filter <'> make-formant
   <'> make-frame <'> make-frame->file <'> make-granulate <'> make-iir-filter
   <'> make-locsig <'> move-locsig <'> make-mixer <'> make-notch
   <'> make-one-pole <'> make-one-zero <'> make-oscil <'> make-pulse-train
   <'> make-rand <'> make-rand-interp <'> make-readin <'> make-sample->file
   <'> make-sawtooth-wave <'> make-square-wave <'> make-src
   <'> make-ssb-am <'> make-table-lookup
   <'> make-triangle-wave <'> make-two-pole <'> make-two-zero <'> make-wave-train
   <'> mixer* <'> mixer-ref <'> mixer-set!
   <'> mixer? <'> mixer+ <'> move-sound <'> make-move-sound
   <'> move-sound? <'> mus-float-equal-fudge-factor <'> multiply-arrays
   <'> mus-array-print-length
   <'> mus-channel <'> mus-channels <'> make-polyshape <'> polyshape?
   <'> mus-close <'> mus-data <'> mus-feedback
   <'> mus-feedforward <'> mus-fft <'> mus-frequency
   <'> mus-hop <'> mus-increment <'> mus-input? <'> mus-file-name
   <'> mus-length <'> mus-location <'> mus-mix <'> mus-order
   <'> mus-output? <'>  mus-phase <'> mus-ramp <'> mus-random
   <'> mus-scaler <'> mus-srate <'> mus-xcoeffs <'> mus-ycoeffs
   <'> notch <'> notch? <'> one-pole <'> one-pole?
   <'> one-zero <'> one-zero? <'> oscil <'> oscil?
   <'> out-any <'> outa <'> outb <'> outc
   <'> outd <'> partials->polynomial <'> partials->wave
   <'> phase-partials->wave <'> polynomial <'> pulse-train <'> pulse-train?
   <'> radians->degrees <'> radians->hz <'> rand <'> rand-interp
   <'> rand-interp? <'>  rand? <'> readin <'> readin?
   <'> rectangular->polar <'> rectangular->magnitudes
   <'> ring-modulate <'> sample->file <'> sample->file?
   <'> sample->frame <'> sawtooth-wave <'> sawtooth-wave?
   <'> spectrum <'> square-wave <'> square-wave?
   <'> src <'> src? <'> ssb-am <'> ssb-am?
   <'> table-lookup <'> table-lookup? <'> tap <'> triangle-wave
   <'> triangle-wave? <'> two-pole <'> two-pole? <'> two-zero
   <'> two-zero? <'> wave-train <'> wave-train?
   <'> make-vct <'> vct-add! <'> vct-subtract!
   <'> vct-copy <'> vct-length <'> vct-multiply! <'> vct-offset!
   <'> vct-ref <'> vct-scale! <'> vct-fill! <'> vct-set!
   <'> vct-peak <'> vct? <'> list->vct
   <'> vct->list <'> vector->vct <'> vct->vector <'> vct-move!
   <'> vct-reverse! <'> vct-subseq <'> vct <'> little-endian?
   <'> vct->string <'> clm-channel <'> env-channel <'> map-channel
   <'> scan-channel <'> reverse-channel <'> seconds->samples
   <'> samples->seconds <'> smooth-channel <'> vct->channel <'> channel->vct
   <'> src-channel <'> scale-channel <'> ramp-channel <'> pad-channel
   <'> normalize-channel <'> cursor-position <'> show-listener <'> mus-sound-prune
   <'> mus-sound-forget <'> xramp-channel <'> ptree-channel <'> snd->sample
   <'> snd->sample? <'> make-snd->sample <'> make-scalar-mixer <'> beats-per-minute
   <'> beats-per-measure <'> channel-amp-envs <'> convolve-files <'> filter-control-coeffs
   <'> locsig-type <'> make-phase-vocoder <'> mus-describe
   <'> mus-error-type->string <'> mus-file-buffer-size <'> mus-name <'> mus-offset
   <'> mus-out-format <'> mus-reset <'> mus-rand-seed <'> mus-width
   <'> phase-vocoder? <'> polar->rectangular <'> phase-vocoder-amp-increments
   <'> phase-vocoder-amps
   <'> phase-vocoder-freqs <'> phase-vocoder-phase-increments
   <'> phase-vocoder-phases
   <'> mus-generator? <'> read-sample <'> reset-listener-cursor <'> goto-listener-end
   <'> sampler-home <'> selection-chans <'> selection-srate <'> snd-warning
   <'> channel-data <'> x-axis-label <'> variable-graph? <'> y-axis-label
   <'> snd-url <'> snd-urls <'> free-player <'> delete-mix <'> delay-tick <'> playing
   <'> pausing <'> copy-sampler <'> html-dir <'> html-program
   <'> make-fir-coeffs <'> make-identity-mixer <'> mus-interp-type <'> mus-run
   <'> phase-vocoder <'> player-home <'> redo-edit <'> undo-edit ) constant procs

#( <'> amp-control <'> ask-before-overwrite <'> audio-input-device <'> audio-output-device
   <'> auto-update <'> channel-style <'> sound-file-extensions
   <'> contrast-control <'> contrast-control-amp
   <'> amp-control-bounds <'> speed-control-bounds <'> expand-control-bounds
   <'> contrast-control-bounds
   <'> reverb-control-length-bounds <'> reverb-control-scale-bounds <'> cursor-update-interval
   <'> cursor-location-offset
   <'> contrast-control? <'> auto-update-interval <'> cursor
   <'> channel-properties <'> channel-property <'> with-tracking-cursor <'> cursor-size
   <'> cursor-style <'> tracking-cursor-style <'> dac-combines-channels <'> dac-size
   <'> clipping <'> default-output-chans <'> default-output-data-format
   <'> default-output-srate <'> default-output-header-type <'> dot-size <'> enved-envelope
   <'> enved-base <'> enved-clip? <'> enved-in-dB <'> enved-style
   <'> enved-power <'> enved-target <'> enved-wave?
   <'> eps-file <'> eps-left-margin <'> eps-bottom-margin <'> eps-size
   <'> expand-control <'> expand-control-hop <'> expand-control-jitter <'> expand-control-length
   <'> expand-control-ramp <'> expand-control? <'> fft-window-alpha <'> fft-window-beta
   <'> fft-log-frequency <'> fft-log-magnitude <'> transform-size <'> transform-graph-type
   <'> fft-window <'> transform-graph? <'> filter-control-in-dB <'> filter-control-envelope
   <'> enved-filter-order <'> enved-filter <'> filter-control-in-hz <'> filter-control-order
   <'> filter-control? <'> graph-cursor <'> graph-style <'> lisp-graph? <'> graphs-horizontal
   <'> just-sounds <'> left-sample <'> listener-prompt
   <'> mark-name <'> mark-properties <'> mark-property
   <'> mark-sample <'> mark-sync <'> max-transform-peaks
   <'> min-dB <'> log-freq-start <'> mix-amp
   <'> mix-amp-env <'> mix-name <'> mix-position <'> mix-properties <'> mix-property
   <'> mix-speed <'> mix-tag-height <'> mix-tag-width
   <'> mix-tag-y <'> mark-tag-width <'> mark-tag-height <'> mix-waveform-height
   <'> transform-normalization
   <'> view-files-sort <'> print-length <'> view-files-amp
   <'> view-files-speed <'> view-files-speed-style <'> view-files-amp-env <'> view-files-files
   <'> view-files-selected-files <'> region-graph-style <'> reverb-control-decay
   <'> reverb-control-feedback <'> reverb-control-length
   <'> reverb-control-lowpass <'> reverb-control-scale <'> time-graph-style <'> lisp-graph-style
   <'> transform-graph-style <'> reverb-control? <'> ladspa-dir <'> peak-env-dir
   <'> save-dir <'> save-state-file
   <'> selection-creates-region <'> show-axes
   <'> show-controls <'> show-transform-peaks <'> show-indices <'> show-marks
   <'> show-mix-waveforms <'> show-selection-transform <'> show-y-zero
   <'> show-grid <'> show-sonogram-cursor <'> sinc-width <'> spectrum-end
   <'> spectro-hop <'> spectrum-start <'> spectro-x-angle <'>  grid-density
   <'> spectro-x-scale <'> spectro-y-angle <'> spectro-y-scale <'> spectro-z-angle
   <'> spectro-z-scale <'> speed-control <'> speed-control-style <'> speed-control-tones
   <'> squelch-update <'> sync <'> sound-properties <'> sound-property <'> temp-dir
   <'> y-bounds <'> transform-type
   <'> trap-segfault <'> with-file-monitor <'> optimization <'> with-verbose-cursor
   <'> wavelet-type <'> x-bounds
   <'> time-graph? <'> wavo-hop <'> wavo-trace <'> with-gl
   <'> with-mix-tags <'> x-axis-style <'> beats-per-minute <'> zero-pad
   <'> zoom-focus-style <'> with-relative-panes <'>  window-x
   <'> window-y <'> window-width <'> window-height
   <'> beats-per-measure <'> channels <'> chans
   <'> comment <'> data-format <'> data-location
   <'> data-size <'> edit-position <'> frames <'> header-type
   <'> maxamp <'> minibuffer-history-length <'> read-only <'> right-sample
   <'> sample <'> samples <'> selected-channel
   <'> selected-sound <'> selection-position <'> selection-frames
   <'> selection-member? <'> sound-loop-info <'> srate <'> time-graph-type
   <'> x-position-slider <'> x-zoom-slider <'> y-position-slider
   <'> y-zoom-slider <'> sound-data-ref <'> mus-array-print-length <'> mus-float-equal-fudge-factor
   <'> mus-data <'> mus-feedback <'> mus-feedforward
   <'> mus-frequency <'> mus-hop <'> mus-increment
   <'> mus-length <'> mus-location <'> mus-phase <'> mus-ramp
   <'> mus-scaler <'> vct-ref <'> x-axis-label <'> filter-control-coeffs
   <'> locsig-type <'> mus-file-buffer-size <'> mus-rand-seed <'> mus-width
   <'> clm-table-size <'> mus-offset <'> mus-reset
   <'> phase-vocoder-amp-increments <'> phase-vocoder-amps <'> phase-vocoder-freqs
   <'> phase-vocoder-phase-increments <'> phase-vocoder-phases
   <'> html-dir <'> html-program <'> mus-interp-type
   <'> mixer-ref <'> frame-ref <'> locsig-ref <'> locsig-reverb-ref
   <'> mus-file-prescaler <'> mus-prescaler <'> mus-clipping <'> mus-file-clipping
   <'> mus-header-raw-defaults ) constant set-procs

: arity-not-ok     <{ prc args -- f }> prc args     arity-ok not ;
: set-arity-not-ok <{ prc args -- f }> prc args set-arity-ok not ;
procs <'> arity-not-ok  0 array-reject constant procs00
procs <'> arity-not-ok  1 array-reject constant procs01
procs <'> arity-not-ok  2 array-reject constant procs02
procs <'> arity-not-ok  3 array-reject constant procs03
procs <'> arity-not-ok  4 array-reject constant procs04
procs <'> arity-not-ok  5 array-reject constant procs05
procs <'> arity-not-ok  6 array-reject constant procs06
procs <'> arity-not-ok  7 array-reject constant procs07
procs <'> arity-not-ok  8 array-reject constant procs08
procs <'> arity-not-ok 10 array-reject constant procs10
set-procs <'> set-arity-not-ok 1 array-reject constant set-procs00
set-procs <'> set-arity-not-ok 2 array-reject constant set-procs01
set-procs <'> set-arity-not-ok 3 array-reject constant set-procs02
set-procs <'> set-arity-not-ok 4 array-reject constant set-procs03
set-procs <'> set-arity-not-ok 5 array-reject constant set-procs04

: close-sound-mc-cb { -- prc; y self -- val }
  1 proc-create "oboe.snd" open-sound , ( prc )
 does> { y self -- val }
  self @ { ind }
  ind sound? if ind close-sound drop then
  0.0
;
: close-sound-aoe-1-cb { -- prc; self -- val }
  0 proc-create "oboe.snd" open-sound , "pistol.snd" open-sound ,
 does> { self -- val }
  self       @ ( ind1 ) close-sound drop
  self cell+ @ ( ind2 ) close-sound
;
: close-sound-aoe-2b-cb { ind1 ind2 -- prc; self -- val }
  0 proc-create ind1 , ind2 , ( prc )
 does> { self -- val }
  self       @ ( ind1 ) close-sound drop
  self cell+ @ ( ind2 ) close-sound
;
: close-sound-aoe-2a-cb { -- prc; self -- val }
  0 proc-create "oboe.snd" open-sound , "pistol.snd" open-sound ,
 does> { self -- val }
  self       @ { ind1 }
  self cell+ @ { ind2 }
  100 0.1 ind1 0 set-sample drop
  100 0.1 ind2 0 set-sample drop
  ind1 ind2 close-sound-aoe-2b-cb "inner-edit" as-one-edit
;
: close-sound-fc-cb { -- prc; y self -- f }
  1 proc-create "oboe.snd" open-sound , ( prc )
 does> { y self -- f }
  self @ { ind }
  ind sound? if ind close-sound drop then
  #f
;
: sc-1-cb { mx -- prc; x self -- f }
  1 proc-create mx , ( prc )
 does> { x self -- f }
  x fabs self @ @ ( mx ) fmax self @ !
  #f
;
: mc-1-cb { scl -- prc; y self -- val }
  1 proc-create scl , 0.0 ( mx ) , ( prc )
 does> { y self -- val }
  y 0.4 f> if
    0.0 self cell+ !
    self cell+ ( addr-of-mx ) sc-1-cb scan-channel drop
    self cell+ @ 1/f self ! ( scl = 1/mx )
  then
  self @ ( scl ) y f*
;
: mc-2-cb { ind -- prc; y self -- val }
  1 proc-create ind , ( prc )
 does> { y self -- val }
  y 0.4 f> if 1 self @ ( ind ) 0 set-frames drop then
  y
;
'complex provided? [if]
  : mc-3-cb <{ y -- val }> y 0.0+1.0i c* ;
[else]
  noop alias mc-3-cb
[then]
: edpos-1-cb { ind -- prc; self -- edpos }
  0 proc-create ind , ( prc )
 does> { self -- edpos }
  self @ ( ind ) close-sound drop
  current-edit-position
;
: edpos-2-cb <{ snd chn -- edpos }>
  snd close-sound drop
  current-edit-position
;

: 28-errors ( -- )
  #t set-with-background-processes drop
  reset-all-hooks
  nil nil { prc tag }
  #( <'> amp-control <'> apply-controls <'> close-sound <'> comment <'> contrast-control
     <'> amp-control-bounds <'> speed-control-bounds <'> expand-control-bounds
     <'> contrast-control-bounds <'> reverb-control-length-bounds <'> reverb-control-scale-bounds
     <'> contrast-control-amp <'> contrast-control? <'> data-format <'> data-location
     <'> data-size <'> expand-control <'> expand-control-hop <'> expand-control-jitter
     <'> expand-control-length <'> expand-control-ramp <'> expand-control? <'> file-name
     <'> filter-control-in-dB <'> filter-control-in-hz <'> filter-control-envelope
     <'> filter-control-order <'> filter-control? <'> finish-progress-report <'> frames
     <'> header-type <'> read-only <'> reset-controls <'> restore-controls
     <'> reverb-control-decay <'> reverb-control-feedback <'> reverb-control-length
     <'> reverb-control-lowpass <'> reverb-control-scale <'> reverb-control? <'> save-controls
     <'> select-sound <'> short-file-name <'> sound-loop-info <'> speed-control
     <'> speed-control-style <'> speed-control-tones <'> srate <'> channel-style
     <'> start-progress-report <'> sync <'> sound-properties <'> sound-property
     <'> swap-channels ) { prcs-1 }
  prcs-1 each to prc
    123 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-sound = unless
	$" snd no-such-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( vct-5 -1.0 csqrt 1.5 "hiho" ) { args-1 }
  nil { arg }
  args-1 each to arg
    prcs-1 each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'no-such-sound  =
	tag car 'wrong-type-arg = ||
	tag car 'mus-error      = || unless
	  $" snd wrong-type-arg %s: %s (%s)" #( prc tag arg ) snd-display
	then
      then
    end-each
  end-each
  "obtest.snd" open-sound { ind }
  args-1 each to arg
    prcs-1 each to prc
      \ INFO: [ms]
      \ snd/chn before value
      \ g_set_channels(snd, val)           arg 0
      \ snd/chn after value
      \ g_set_amp_control(val, snd, chn)   0 arg
      prc <'> data-format   =
      prc <'> data-location = ||
      prc <'> data-size     = ||
      prc <'> header-type   = ||
      prc <'> srate         = ||
      prc <'> comment       = || if arg 0 else 0 arg then prc set-xt #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'wrong-type-arg = unless
	  $" snd wrong-type-arg set-%s [%s]: %s" #( prc arg tag ) snd-display
	then
      then
    end-each
  end-each
  #( <'> amp-control <'> contrast-control <'> contrast-control-amp <'> contrast-control?
     <'> expand-control <'> amp-control-bounds <'> speed-control-bounds <'> expand-control-bounds
     <'> contrast-control-bounds <'> reverb-control-length-bounds <'> reverb-control-scale-bounds
     <'> expand-control-hop <'> expand-control-jitter <'> expand-control-length
     <'> expand-control-ramp <'> expand-control? <'> filter-control-in-dB
     <'> filter-control-in-hz <'> filter-control-envelope <'> filter-control-order
     <'> filter-control? <'> reverb-control-decay <'> reverb-control-feedback
     <'> reverb-control-length <'> reverb-control-lowpass <'> reverb-control-scale
     <'> reverb-control? <'> speed-control <'> speed-control-style <'> speed-control-tones
     <'> channel-style <'> sync ) { prcs-2 }
  args-1 each to arg
    prcs-2 each to prc
      arg ind prc set-xt #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'wrong-type-arg = unless
	  $" snd safe wrong-type-arg set-%s [%s]: %s" #( prc arg tag ) snd-display
	then
      then
    end-each
  end-each
  ind close-sound drop
  #( 1 make-array "hiho" -1.0 csqrt 1.5 #( 1 0 ) #( 0 1 ) ) { args-2 }
  args-2 each to arg
    #( <'> make-vct <'> vct-copy <'> vct-length <'> vct->list <'> vct-peak ) each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-type-arg = unless
	$" vct 0 wrong-type-arg %s [%s]: %s" #( prc arg tag ) snd-display
      then
    end-each
  end-each
  #( <'> vct-add! <'> vct-subtract! <'> vct-multiply!
     <'> vct-ref <'> vct-scale! <'> vct-fill! ) { vct-prcs-2 }
  nil nil { arg1 arg2 }
  args-2 each to arg1
    args-1 each to arg2
      vct-prcs-2 each to prc
	arg1 arg2 prc #t nil fth-catch to tag
	stack-reset
	tag car 'wrong-type-arg       =
	tag car 'wrong-number-of-args = ||
	tag car 'mus-error            = || unless
	  $" vct 1 wrong-whatever %s [%s %s]: %s" #( prc arg1 arg2 tag ) snd-display
	then
      end-each
    end-each
  end-each
  args-2 each to arg
    vct-prcs-2 each to prc
      arg vct-3 prc #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-type-arg = unless
	$" vct 1 wrong-whatever %s [%s]: %s" #( prc arg tag ) snd-display
      then
    end-each
  end-each
  -23 <'> make-vct #t nil fth-catch to tag
  tag car 'out-of-range = unless $" make-vct -23: %s" #( tag ) snd-display then
  vct-3 { v }
  v 12 <'> vct-ref #t nil fth-catch to tag
  tag car 'out-of-range = unless $"  vct[12]: %s" #( tag ) snd-display then
  #( <'> all-pass? <'> asymmetric-fm? <'> comb? <'> filtered-comb?
     <'> convolve? <'> delay? <'> env? <'> file->frame?
     <'> file->sample? <'> snd->sample? <'> filter? <'> fir-filter?
     <'> formant? <'> frame->file? <'> frame? <'> granulate?
     <'> iir-filter? <'> locsig? <'> mixer? <'> move-sound?
     <'> mus-input? <'> mus-output? <'> notch? <'> one-pole?
     <'> one-zero? <'> oscil? <'> phase-vocoder? <'> pulse-train?
     <'> rand-interp? <'> rand? <'> readin? <'> sample->file?
     <'> sawtooth-wave? <'> square-wave? <'> src? <'> table-lookup? <'> triangle-wave?
     <'> two-pole? <'> two-zero? <'> wave-train?
     <'> mix-sampler? <'> moving-average? <'> ssb-am?
     <'> sampler? <'> region-sampler? <'> vct? ) { ?prcs }
  args-1 each to arg
    ?prcs each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'wrong-type-arg = unless
	  $" ?proc %s [%s]: %s" #( prc arg tag ) snd-display
	then
      then
    end-each
  end-each
  ?prcs each to prc
    440 make-oscil prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag 'wrong-type-arg = unless
	$" ?proc %s [440 make-oscil]: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> reverse-selection <'> selection-position <'> selection-frames <'> smooth-selection
     <'> scale-selection-to <'> insert-selection <'> delete-selection
     <'> mix-selection ) each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-active-selection = unless
      $" [0] selection %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> src-selection <'> filter-selection <'> env-selection ) each to prc
    0.0 prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-active-selection = unless
      $" [1] selection %s: %s" #( prc tag ) snd-display
    then
  end-each

  #( <'> all-pass <'> asymmetric-fm <'> clear-array <'> comb <'> filtered-comb
     <'> convolve <'> db->linear <'> moving-average <'> degrees->radians <'> delay
     <'> env <'> formant <'> firmant <'> frame->list <'> granulate <'> hz->radians
     <'> linear->db <'> make-all-pass <'> make-asymmetric-fm <'> make-comb
     <'> make-filtered-comb <'> make-convolve <'> make-delay <'> make-env
     <'> make-file->frame <'> make-file->sample <'> make-filter <'> make-fir-filter
     <'> make-formant <'> make-firmant <'> make-frame <'> make-granulate <'> make-iir-filter
     <'> make-locsig <'> make-notch <'> make-one-pole <'> make-one-zero <'> make-oscil
     <'> make-pulse-train <'> make-rand <'> make-rand-interp <'> make-readin
     <'> make-sawtooth-wave <'> make-nrxysin <'> make-nrxycos
     <'> make-square-wave <'> make-src <'> make-ncos
     <'> make-nsin <'> make-table-lookup <'> make-triangle-wave
     <'> make-two-pole <'> make-two-zero <'> make-wave-train <'> make-ssb-am
     <'> mus-channel <'> mus-channels <'> make-polyshape <'> make-polywave
     <'> mus-data <'> mus-feedback <'> mus-feedforward <'> mus-frequency <'> mus-hop
     <'> mus-increment <'> mus-length <'> mus-file-name <'> mus-location <'> mus-name
     <'> mus-order <'> mus-phase <'> mus-ramp <'> mus-random <'> mus-run <'> mus-scaler
     <'> mus-xcoeffs <'> mus-ycoeffs <'> notch <'> one-pole <'> one-zero <'> make-moving-average
     <'> seconds->samples <'> samples->seconds <'> oscil <'> partials->polynomial
     <'> partials->wave <'> phase-partials->wave <'> phase-vocoder <'> pulse-train
     <'> radians->degrees <'> radians->hz <'> rand <'> rand-interp <'> readin
     <'> sawtooth-wave <'> nrxysin <'> nrxycos <'> square-wave <'> src
     <'> ncos <'> nsin <'> table-lookup <'> tap
     <'> triangle-wave <'> two-pole <'> two-zero <'> wave-train <'> ssb-am ) { clm-prcs-1 }
  \ FIXME [ms]
  \ 'bad-type added for      #( 1 1 1 ) partials->wave
  \ 'out-of-range added for  -1.0 csqrt make-frame
  \                          -1.0 csqrt make-moving-average
  #( 1 make-array color-95 -1.0 csqrt 1 1.0 make-vct ) each to arg
    clm-prcs-1 each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'wrong-type-arg =
	tag car 'no-data        = ||
	tag car 'no-such-method = ||
	tag car 'error          = ||
	tag car 'arg-error      = ||
	tag car 'bad-type       = ||
	tag car 'out-of-range   = || unless
	  $" clm %s [%s]: %s" #( prc arg tag ) snd-display
	then
      then
    end-each
  end-each
  clm-prcs-1 each to prc
    make-oscil vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg =
      tag car 'bad-arity      = ||
      tag car 'mus-error      = || unless
	$" clm 1 %s [make-oscil]: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-channel <'> mus-channels <'> mus-data
     <'> mus-feedback <'> mus-feedforward <'> mus-frequency
     <'> mus-hop <'> mus-increment <'> mus-length <'> mus-location
     <'> mus-mix <'> mus-name <'> mus-order <'> mus-phase <'> mus-ramp
     <'> mus-random <'> mus-run <'> mus-scaler <'> mus-xcoeffs <'> mus-ycoeffs ) each to prc
    \ FIXME: 'out-of-range added [ms].
    make-oscil vector-0 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg =
      tag car 'out-of-range   = ||
      tag car 'error          = || unless
	$" mus-gen %s [make-oscil]: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-sound-samples <'> mus-sound-frames <'> mus-sound-duration <'> mus-sound-datum-size
     <'> mus-sound-data-location <'> mus-sound-chans <'> mus-sound-srate <'> mus-sound-header-type
     <'> mus-sound-data-format <'> mus-sound-length <'> mus-sound-type-specifier
     <'> mus-header-type-name
     <'> mus-data-format-name <'> mus-sound-comment <'> mus-sound-write-date
     <'> mus-bytes-per-sample
     <'> mus-sound-loop-info <'> mus-sound-maxamp <'> mus-sound-maxamp-exists?
     <'> mus-header-type->string
     <'> mus-data-format->string ) { mus-snd-prcs-1 }
  mus-snd-prcs-1 each to prc
    vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-type-arg = unless
      $" mus-sound %s: %s" #( prc tag ) snd-display
    then
  end-each
  mus-snd-prcs-1 each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-number-of-args = unless
      $" no arg mus-sound %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> mus-sound-samples <'> mus-sound-frames <'> mus-sound-duration <'> mus-sound-datum-size
     <'> mus-sound-data-location <'> mus-sound-chans <'> mus-sound-srate <'> mus-sound-header-type
     <'> mus-sound-data-format <'> mus-sound-length <'> mus-sound-type-specifier
     <'> mus-sound-comment
     <'> mus-sound-write-date <'> mus-sound-maxamp <'> mus-sound-maxamp-exists? ) each to prc
    "/bad/baddy" prc #t nil fth-catch to tag
    stack-reset
    tag car 'mus-error = unless
      $" bad file mus-sound %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> count-matches <'> cursor <'> channel-properties <'> channel-property
     <'> with-tracking-cursor <'> cursor-position <'> cursor-size <'> cursor-style
     <'> tracking-cursor-style <'> delete-sample <'> display-edits <'> dot-size
     <'> edit-fragment <'> edit-position
     <'> edit-tree <'> edits <'> fft-window-alpha <'> fft-window-beta
     <'> fft-log-frequency <'> fft-log-magnitude <'> transform-size <'> transform-graph-type
     <'> fft-window <'> transform-graph? <'> find-channel <'> graph
     <'> graph-style <'> lisp-graph? <'> insert-sound
     <'> time-graph-style <'> lisp-graph-style <'> transform-graph-style <'> left-sample
     <'> map-chan <'> max-transform-peaks <'> maxamp-position <'> min-dB <'> mix-region
     <'> transform-normalization <'> peaks <'> reverse-sound
     <'> revert-sound <'> right-sample <'> sample <'> save-sound <'> save-sound-as
     <'> scan-chan <'> select-channel <'> show-axes <'> show-transform-peaks
     <'> show-marks <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'> spectrum-end <'> spectro-hop <'> spectrum-start
     <'> spectro-x-angle <'> spectro-x-scale <'> spectro-y-angle <'>  grid-density
     <'> spectro-y-scale <'> spectro-z-angle <'> spectro-z-scale <'> squelch-update
     <'> transform-sample <'> transform->vct <'> transform-frames <'> transform-type
     <'> update-transform-graph <'> update-time-graph <'> update-lisp-graph <'> update-sound
     <'> wavelet-type <'> time-graph? <'> time-graph-type <'> wavo-hop
     <'> wavo-trace <'> x-bounds <'> x-position-slider <'> x-zoom-slider
     <'> x-axis-label <'> y-axis-label <'> y-bounds <'> y-position-slider
     <'> y-zoom-slider <'> zero-pad ) { chn-prcs }
  chn-prcs each to prc
    vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg =
      tag car 'no-such-sound  = || unless
	$" chn (no snd) procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  chn-prcs each to prc
    0 vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-type-arg = unless
      $" chn (no chn) procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> cursor <'> with-tracking-cursor <'> channel-properties <'> channel-property
     <'> cursor-position <'> cursor-size <'> cursor-style <'> tracking-cursor-style
     <'> delete-sample <'> display-edits <'> dot-size <'> edit-fragment
     <'> edit-position <'> edit-tree <'> edits <'> env-sound
     <'> fft-window-alpha <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
     <'> transform-size <'> transform-graph-type <'> fft-window <'> transform-graph?
     <'> filter-sound <'> graph-style <'> lisp-graph?
     <'> left-sample <'> time-graph-style <'> lisp-graph-style
     <'> transform-graph-style <'> max-transform-peaks <'> maxamp
     <'> maxamp-position <'> min-dB <'> transform-normalization
     <'> redo <'> reverse-sound <'> revert-sound <'> right-sample
     <'> sample <'> save-sound <'> scale-by <'> scale-to
     <'> show-axes <'> show-transform-peaks <'> show-marks <'> show-mix-waveforms
     <'> show-y-zero <'> show-grid <'> show-sonogram-cursor <'> spectrum-end
     <'> spectro-hop <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale
     <'> spectro-y-angle <'> spectro-y-scale <'> spectro-z-angle <'> spectro-z-scale
     <'> squelch-update <'>  grid-density <'> src-sound <'> transform-sample
     <'> transform->vct <'> transform-frames <'> transform-type <'> undo
     <'> update-transform-graph <'> update-time-graph <'> update-lisp-graph <'> update-sound
     <'> wavelet-type <'> time-graph? <'> time-graph-type <'> wavo-hop
     <'> wavo-trace <'> x-bounds <'> x-position-slider <'> normalize-channel
     <'> x-zoom-slider <'> y-bounds <'> y-position-slider
     <'> x-axis-label <'> y-axis-label <'> y-zoom-slider
     <'> zero-pad <'> scale-channel ) each to prc
    1234 prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-such-sound = unless
      $" chn procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> delete-sample <'> edit-fragment <'> graph-style
     <'> redo <'> time-graph-style <'> lisp-graph-style
     <'> transform-graph-style <'> scale-by <'> scale-to <'> undo <'> x-axis-label ) { prcs-3 }
  prcs-3 each to prc
    0 1234 prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-such-sound = unless
      $" snd(1) chn procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  "oboe.snd" open-sound to ind
  prcs-3 each to prc
    prc proc-name "x-axis-label" string= unless
      0 ind 1234 prc #t nil fth-catch to tag
      stack-reset
      tag car 'no-such-channel = unless
	$" snd(1 1234) chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" open-sound to ind
  #( <'> cursor <'> cursor-position <'> cursor-size
     <'> cursor-style <'> tracking-cursor-style <'> display-edits <'> dot-size
     <'> edit-position <'> edit-tree <'> edits <'> fft-window-alpha
     <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude <'> transform-size
     <'> transform-graph-type <'> fft-window <'> transform-graph? <'> graph-style
     <'> lisp-graph? <'> left-sample <'> time-graph-style <'> lisp-graph-style
     <'> transform-graph-style <'> max-transform-peaks <'> maxamp
     <'> maxamp-position <'> min-dB <'> transform-normalization
     <'> reverse-sound <'> right-sample <'> show-axes <'> show-transform-peaks
     <'> show-marks <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'>  grid-density <'> spectrum-end <'> spectro-hop
     <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale <'> spectro-y-angle
     <'> spectro-y-scale <'> spectro-z-angle <'> spectro-z-scale <'> squelch-update
     <'> transform->vct <'> transform-frames <'> transform-type <'> update-transform-graph
     <'> update-time-graph <'> update-lisp-graph <'> wavelet-type <'> time-graph?
     <'> time-graph-type <'> wavo-hop <'> wavo-trace <'> x-bounds
     <'> x-position-slider <'> x-axis-label <'> x-zoom-slider <'> y-bounds
     <'> y-position-slider <'> y-zoom-slider <'> zero-pad
     <'> channel-properties <'> channel-property ) each to prc
    ind 1234 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-sound   =
      tag car 'no-such-channel = || unless
	$" chn (2) procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" open-sound to ind
  chn-prcs each to prc
    vct-5 ind 0 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-sound  =
      tag car 'wrong-type-arg = || unless
	$" set chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  #( <'> mix-amp <'> mix-amp-env <'> mix-length <'> mix-name
     <'> mix-position <'> mix-home <'> mix-speed <'> mix-tag-y ) { mix-prcs }
  mix-prcs each to prc
    vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-type-arg = unless
      $" [0] mix procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  mix-prcs each to prc
    1234 integer->mix prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-such-mix = unless
      $" [1] mix procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> mix-name <'> mix-position <'> mix-home <'> mix-speed <'> mix-tag-y ) { mix-set-prcs }
  mix-set-prcs each to prc
    1234 integer->mix vct-5 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg =
      tag car 'no-such-mix    = || unless
	$" [2] set mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  "oboe.snd" 10 mix-sound { id }
  mix-set-prcs each to prc
    id vct-5 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg =
      tag car 'no-such-mix    = || unless
	$" [3] set mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  #( <'> add-mark <'> mark-name <'> mark-sample <'> mark-sync
     <'> mark-home <'> delete-mark <'> delete-marks <'> find-mark ) each to prc
    vct-5 prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-type-arg = unless
      $" mark procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> mark-name <'> mark-sample <'> mark-sync <'> mark-home <'> delete-mark ) each to prc
    1234 integer->mark prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-such-mark = unless
      $" no mark procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  "oboe.snd" open-sound to ind
  0 ind 0 add-mark to id
  #( <'> mark-name <'> mark-sample <'> mark-sync ) each to prc
    id vct-5 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-type-arg = unless
      $" set mark procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  ind close-sound drop
  \ region-sample, initially here, requires two args
  #( <'> region-chans <'> region-home <'> region-frames
     <'> region-position <'> region-maxamp <'> region-maxamp-position
     <'> region-srate <'> forget-region ) { reg-prcs-1 }
  #( vct-5 #( 0 1 ) #() "hiho" #( 0 1 ) ) each to arg
    reg-prcs-1 each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-type-arg = unless
	$" region procs %s [%s]: %s" #( prc arg tag ) snd-display
      then
    end-each
  end-each
  reg-prcs-1 each to prc
    1234 integer->region prc #t nil fth-catch to tag
    stack-reset
    tag car 'no-such-region = unless
      $" (no) region procs %s: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> enved-filter-order <'> enved-filter
     <'> ask-before-overwrite <'> auto-resize <'> auto-update <'> channel-style
     <'> dac-combines-channels <'> dac-size <'> clipping
     <'> default-output-chans <'> default-output-data-format <'> default-output-srate
     <'> default-output-header-type
     <'> enved-envelope <'> enved-base <'> enved-clip? <'> enved-in-dB
     <'> enved-dialog <'> enved-style <'>  enved-power <'> enved-target
     <'> enved-wave? <'> eps-file <'> eps-left-margin
     <'> eps-bottom-margin <'> eps-size
     <'> graph-cursor <'> listener-prompt
     <'> max-regions <'> minibuffer-history-length <'> mix-waveform-height <'> region-graph-style
     <'> time-graph-style <'> lisp-graph-style <'> transform-graph-style
     <'> view-files-sort <'> print-length
     <'> ladspa-dir <'> peak-env-dir <'> save-dir
     <'> save-state-file <'> selected-channel
     <'> selected-sound <'> selection-creates-region <'> show-controls
     <'> show-indices <'> show-selection-transform <'> sinc-width
     <'> temp-dir <'> trap-segfault
     <'> with-file-monitor <'> optimization <'> with-verbose-cursor
     <'> window-height <'> beats-per-measure
     <'> window-width <'> window-x <'> window-y <'> with-gl
     <'> with-mix-tags <'> x-axis-style <'> beats-per-minute
     <'> mix-tag-height <'> mix-tag-width <'> with-relative-panes
     <'> clm-table-size <'> mark-tag-width <'> mark-tag-height ) each to prc
    vct-5 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg = unless
	$" misc procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  nil { hook }
  snd-hooks each to hook
    hook <'> noop 0 make-proc <'> add-hook! #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'bad-arity      =	\ FTH special 'bad-arity (add-hook!) [ms]
      tag car 'wrong-type-arg = || unless
	$" [0] hooks %s: %s" #( hook hook-name tag ) snd-display
      then
    then
  end-each
  #( <'> exit-hook <'> stop-dac-hook <'> stop-playing-selection-hook <'> color-hook
     <'> orientation-hook <'> start-playing-selection-hook ) each to hook
    hook <'> noop 3 make-proc <'> add-hook! #t nil fth-catch to tag
    stack-reset
    tag car 'bad-arity      =	\ FTH special 'bad-arity (add-hook!) [ms]
    tag car 'wrong-type-arg = || unless
      $" [1] hooks %s: %s" #( hook hook-name tag ) snd-display
    then
  end-each
  "not-an-env" 	       	     <'> set-enved-envelope  	'no-such-envelope check-error-tag
  "/bad/baddy" 	       	     <'> save-envelopes      	'cannot-save      check-error-tag
  "/bad/baddy" 	       	     <'> save-macros         	'cannot-save      check-error-tag
  "/bad/baddy" 	       	     <'> mus-sound-report-cache 'cannot-save      check-error-tag
  <'> noop 3 make-proc       <'> set-search-procedure   'bad-arity        check-error-tag
  1234 <'> noop 1 make-proc  <'> set-search-procedure   'no-such-sound    check-error-tag
  0 "oboe.snd" 1             <'> make-sampler     	'no-such-channel  check-error-tag
  0 "oboe.snd" -1            <'> make-sampler     	'no-such-channel  check-error-tag
  <'> noop 1 make-proc       <'> set-zoom-focus-style   'bad-arity        check-error-tag
  1 1.0 make-mixer { mx }
  "oboe.snd" "pistol.snd" 0 12 0 mx $" a string" <'> mus-mix 'wrong-type-arg check-error-tag
  :file "test.snd" new-sound to ind
  "test.snd" sf-dir "bad_chans.aifc" $+ <'> mus-mix     'bad-header       check-error-tag
  "test.snd" sf-dir "bad_length.aifc" $+ <'> mus-mix    'mus-error        check-error-tag
  ind close-sound drop
  "test.snd" file-delete
  sf-dir "bad_chans.aifc" $+ "oboe.snd" <'> mus-mix     'bad-header       check-error-tag
  123 #( 0 0 1 1 )           <'> set-sound-loop-info    'no-such-sound    check-error-tag
  "fmv.snd" mus-nist mus-bfloat 22050 2 "comment" <'> new-sound 'bad-header check-error-tag
  123                        <'> player-home            'wrong-type-arg   check-error-tag
  "/hiho"                    <'> set-temp-dir           'no-such-file     check-error-tag
  "/hiho"                    <'> set-save-dir           'no-such-file     check-error-tag
  20 integer->transform 4 0.0 make-vct <'> snd-transform 'out-of-range    check-error-tag
  sf-dir "bad_chans.aifc" $+ <'> mus-sound-maxamp       'bad-header       check-error-tag
  sf-dir "bad_chans.snd" $+ #( 0.0 0.0 ) <'> set-mus-sound-maxamp 'bad-header check-error-tag
  :order 32 :ycoeffs 4 0.0 make-vct <'> make-iir-filter 'mus-error        check-error-tag
  :coeffs 4 0 make-vct :ycoeffs 4 0 make-vct <'> make-iir-filter 'mus-error check-error-tag
  :coeffs 4 0 make-vct :xcoeffs 4 0 make-vct <'> make-fir-filter 'mus-error check-error-tag
  :size 123456789            <'> make-table-lookup      'out-of-range     check-error-tag
  :ramp -0.5                 <'> make-granulate         'out-of-range     check-error-tag
  :ramp 1.5                  <'> make-granulate         'out-of-range     check-error-tag
  :expansion 32000.0         <'> make-granulate         'mus-error        check-error-tag
  "test.snd" :channels 0     <'> new-sound              'out-of-range     check-error-tag
  "test.snd" :srate 0        <'> new-sound              'out-of-range     check-error-tag
  "test.snd" :size -1        <'> new-sound              'out-of-range     check-error-tag
  "test.snd" :size 0         <'> make-readin            'out-of-range     check-error-tag
  "test.snd" :size -1        <'> make-readin            'out-of-range     check-error-tag
  "oboe.snd" 0               <'> make-file->sample      'out-of-range     check-error-tag
  "oboe.snd" -1              <'> make-file->sample      'out-of-range     check-error-tag
  "oboe.snd" 0               <'> make-file->frame       'out-of-range     check-error-tag
  "oboe.snd" -1              <'> make-file->frame       'out-of-range     check-error-tag
  -1                         <'> set-default-output-data-format 'out-of-range check-error-tag
  mus-soundfont              <'> set-default-output-header-type 'out-of-range check-error-tag
  sf-dir "bad_location.nist" $+ <'> mus-sound-chans     'mus-error        check-error-tag
  sf-dir "bad_field.nist" $+ <'> mus-sound-chans        'mus-error        check-error-tag
  snd-motif-error-checks
  -1                         <'> main-menu              'no-such-menu     check-error-tag
  111                        <'> main-menu              'no-such-menu     check-error-tag
  "hiho" 123                 <'> new-sound              'out-of-range     check-error-tag
  "hiho" mus-nist 123        <'> new-sound              'out-of-range     check-error-tag
  "hiho" mus-nist mus-bfloat <'> new-sound              'bad-header       check-error-tag
  0 1                        <'> make-sound-data        'out-of-range     check-error-tag
  -2 1                       <'> make-sound-data        'out-of-range     check-error-tag
  1 -1                       <'> make-sound-data        'out-of-range     check-error-tag
  1 0                        <'> make-sound-data        'out-of-range     check-error-tag
  0 1                        <'> mus-sound-close-output 'out-of-range     check-error-tag
  1 1                        <'> mus-sound-close-output 'out-of-range     check-error-tag
  2 1                        <'> mus-sound-close-output 'out-of-range     check-error-tag
  0                          <'> mus-sound-close-input  'out-of-range     check-error-tag
  1                          <'> mus-sound-close-input  'out-of-range     check-error-tag
  2                          <'> mus-sound-close-input  'out-of-range     check-error-tag
  -1                         <'> set-mus-array-print-length 'out-of-range check-error-tag
  -1                         <'> set-print-length       'out-of-range     check-error-tag
  12                         <'> set-enved-style        'out-of-range     check-error-tag
  1.5 0.0 0.0                <'> make-color             'out-of-range     check-error-tag
  -0.5 0.0 0.0               <'> make-color             'out-of-range     check-error-tag
  #f                         <'> make-variable-graph    'wrong-type-arg   check-error-tag
  'snd-nogui unless
    cadr-main                <'> make-variable-graph    'arg-error        check-error-tag
  then
  <'> graph->ps              'cannot-print     check-error-tag
  "oboe.snd" open-sound to ind
  #t set-selection-creates-region drop
  select-all drop
  "sel0.snd" :not-a-key 3    <'> save-selection         'mus-error        check-error-tag
  #( ind )                   <'> read-only              'wrong-type-arg   check-error-tag
  ind #( 0 )                 <'> frames                 'wrong-type-arg   check-error-tag
  0 -10                      <'> smooth-sound           'wrong-type-arg   check-error-tag
  0 ind 123                  <'> mix-selection          'no-such-channel  check-error-tag
  0 ind 123                  <'> insert-selection       'no-such-channel  check-error-tag
  ind 0                      <'> set-channels           'out-of-range     check-error-tag
  ind -1                     <'> set-channels           'out-of-range     check-error-tag
  ind 12340                  <'> set-channels           'out-of-range     check-error-tag
  ind 12340                  <'> set-data-format        'out-of-range     check-error-tag
  ind 12340                  <'> set-header-type        'out-of-range     check-error-tag
  ind 0                      <'> set-srate              'out-of-range     check-error-tag
  ind -1                     <'> set-data-location      'out-of-range     check-error-tag
  ind -1                     <'> set-data-size          'out-of-range     check-error-tag
  -1 -1                      <'> set-sample             'no-such-sample   check-error-tag
  -1                         <'> sample                 'no-such-sample   check-error-tag
  -10                        <'> set-frames             'out-of-range     check-error-tag
  0.0                        <'> set-min-dB             'out-of-range     check-error-tag
  0.0 ind 0                  <'> set-min-dB             'out-of-range     check-error-tag
  1 -22                      <'> start-playing          'out-of-range     check-error-tag
  1 0                        <'> start-playing          'out-of-range     check-error-tag
  #( 0.0 1.0 0.1 -0.1 1.0 0.0 ) ind <'> set-filter-control-envelope 'out-of-range check-error-tag
  #( 0.0 1.0 0.1 1.1 1.0 0.0 ) ind  <'> set-filter-control-envelope 'out-of-range check-error-tag
  #( 0 0 .1 .1 .05 .1 1 1 ) 32 <'> filter-sound         'env-error        check-error-tag
  ind 123                    <'> apply-controls         'out-of-range     check-error-tag
  #( 0.0 2.0 )               <'> set-speed-control-bounds  'out-of-range  check-error-tag
  #( 0.0 2.0 )               <'> set-expand-control-bounds 'out-of-range  check-error-tag
  #( 2.0 0.0 )               <'> set-speed-control-bounds  'out-of-range  check-error-tag
  #( 2.0 0.0 )               <'> set-expand-control-bounds 'out-of-range  check-error-tag
  sf-dir "bad_chans.snd" $+  <'> insert-sound           'bad-header       check-error-tag
  sf-dir "bad_chans.snd" $+  <'> convolve-with          'IO-error         check-error-tag
  "hiho.snd" ind -12         <'> save-sound-as          'cannot-save      check-error-tag
  "hiho.snd" ind mus-next -12 <'> save-sound-as         'cannot-save      check-error-tag
  "test.snd" ind mus-nist mus-bdouble <'> save-sound-as 'cannot-save      check-error-tag
  "test.snd" ind mus-aifc mus-lfloat  <'> save-sound-as 'cannot-save      check-error-tag
  "test.snd" ind mus-riff mus-bshort  <'> save-sound-as 'cannot-save      check-error-tag
  "test.snd" ind mus-voc  mus-bshort  <'> save-sound-as 'cannot-save      check-error-tag
  "test.snd" mus-riff mus-bshort <'> save-selection     'cannot-save      check-error-tag
  "test.snd" mus-voc  mus-bshort <'> save-selection     'cannot-save      check-error-tag
  #( 0 0 1 1 )  :length 11 make-env <'> src-channel     'out-of-range     check-error-tag
  #( 0 1 1 0 )  :length 11 make-env <'> src-channel     'out-of-range     check-error-tag
  #( 0 1 1 -1 ) :length 11 make-env <'> src-channel     'out-of-range     check-error-tag
  #( 0 -1 1 1 ) :length 11 make-env <'> src-channel     'out-of-range     check-error-tag
  #( 0 0 1 1 )  :length 11 make-env <'> src-sound       'out-of-range     check-error-tag
  #( 0 1 1 0 )  :length 11 make-env <'> src-sound       'out-of-range     check-error-tag
  #( 0 1 1 -1 ) :length 11 make-env <'> src-sound       'out-of-range     check-error-tag
  #( 0 -1 1 1 ) :length 11 make-env <'> src-sound       'out-of-range     check-error-tag
  0.0 0.0 0.0 0.0 0.0 0.0 0.0 <'> make-readin           'mus-error        check-error-tag
  vct-3 32                   <'> filter-sound           'out-of-range     check-error-tag
  #( 0 0 1 1 ) 0             <'> filter-sound           'out-of-range     check-error-tag
  ind 0 12345 0              <'> swap-channels          'no-such-sound    check-error-tag
  vct( 0.1 0.2 0.3 ) -1 ind 0 #t "" <'> mix-vct         'no-such-sample   check-error-tag
  8 0.0 make-vct 0 -123      <'> snd-spectrum           'out-of-range     check-error-tag
  8 0.0 make-vct 0 0         <'> snd-spectrum           'out-of-range     check-error-tag
  "/baddy/hiho"              <'> play                   'no-such-file     check-error-tag
  sf-dir "nist-shortpack.wav" $+ <'> play               'bad-format       check-error-tag
  ind 123                    <'> make-player            'no-such-channel  check-error-tag
  "/baddy/hiho"              <'> mix                    'no-such-file     check-error-tag
  "oboe.snd" 0 2             <'> mix                    'no-such-channel  check-error-tag
  "/baddy/hiho" 0            <'> mix-sound              'no-such-file     check-error-tag
  "/baddy/hiho.snd"          <'> insert-sound           'no-such-file     check-error-tag
  0 10 "/baddy/hiho.snd"     <'> insert-samples         'no-such-file     check-error-tag
  #() ind                    <'> set-filter-control-envelope 'no-data     check-error-tag
  ind 123                    <'> set-data-format        'out-of-range     check-error-tag
  ind 123                    <'> set-header-type        'out-of-range     check-error-tag
  ind 123                    <'> set-selected-channel   'no-such-channel  check-error-tag
  ind <'> noop 3 make-proc   <'> set-search-procedure   'bad-arity        check-error-tag
  <'> noop 3 make-proc       <'> map-chan               'bad-arity        check-error-tag
  <'> noop 3 make-proc       <'> scan-chan              'bad-arity        check-error-tag
  <'> noop 1 make-proc ind 0 <'> set-cursor-style       'bad-arity        check-error-tag
  <'> noop 0 make-proc       <'> find-channel           'bad-arity        check-error-tag
  <'> noop 0 make-proc       <'> count-matches          'bad-arity        check-error-tag
  ind 0 1234                 <'> axis-info              'no-such-axis     check-error-tag
  'snd-nogui unless
    ind 1234                 <'> axis-info              'no-such-channel  check-error-tag
    1234                     <'> axis-info              'no-such-sound    check-error-tag
  then
  graph-once set-time-graph-type drop
  #( 0.1 -0.1 )              <'> set-x-bounds           'out-of-range     check-error-tag
  100 0                      <'> make-region            'out-of-range     check-error-tag
  -1                         <'> delete-sample          'no-such-sample   check-error-tag
  ind frames 2*              <'> delete-sample          'no-such-sample   check-error-tag
  "/bad/baddy.snd"           <'> play                   'no-such-file     check-error-tag
  1234 0                     <'> play                   'no-such-sound    check-error-tag
  regions empty? if 0 100 make-region then
  regions 0 array-ref 0 1234 <'> region-sample          'no-such-channel  check-error-tag
  regions 0 array-ref 1234   <'> region-frames          'no-such-channel  check-error-tag
  regions 0 array-ref 1234   <'> region-position        'no-such-channel  check-error-tag
  regions 0 array-ref 0 1 1234 <'> region->vct          'no-such-channel  check-error-tag
  "/bad/baddy.snd"           <'> save-sound-as          'cannot-save      check-error-tag
  0 1 1234                   <'> transform-sample       'no-such-sound    check-error-tag
  0 1 ind 1234               <'> transform-sample       'no-such-channel  check-error-tag
  vct( 0 1 ) "hi" 0 1 0 1 1234     <'> graph            'no-such-sound    check-error-tag
  vct( 0 1 ) "hi" 0 1 0 1 ind 1234 <'> graph            'no-such-channel  check-error-tag
  #f #t set-selection-member? drop
  vct( 0 0 1 1 ) 4           <'> filter-selection       'no-active-selection check-error-tag
  "/bad/baddy.snd"           <'> save-selection         'no-active-selection check-error-tag
  #( 0 0 1 1 )               <'> env-selection          'no-active-selection check-error-tag
  1234 integer->region "/bad/baddy.snd" <'> save-region 'no-such-region   check-error-tag
  0 100 ind 0 make-region drop
  "/bad/baddy.snd"           <'> save-selection         'cannot-save      check-error-tag
  regions 0 array-ref "/bad/baddy.snd" <'> save-region  'cannot-save      check-error-tag
  1234 integer->mix          <'> make-mix-sampler       'no-such-mix      check-error-tag
  0 12 1234 #t               <'> make-region            'no-such-sound    check-error-tag
  #t ind set-read-only drop
  ind #( 0 0 1 1 )           <'> set-sound-loop-info    'cannot-save      check-error-tag
  0 ind 0 123                <'> make-sampler     	'no-such-direction check-error-tag
  0 ind 0 0                  <'> make-sampler     	'no-such-direction check-error-tag
  0 ind 0 -2                 <'> make-sampler     	'no-such-direction check-error-tag
  #()                        <'> scale-by               'no-data          check-error-tag
  #()                        <'> scale-to               'no-data          check-error-tag
  "hi" <'> noop 2 make-proc  <'> prompt-in-minibuffer   'bad-arity        check-error-tag
  -999 ind 0                 <'> set-selection-position 'no-such-sample   check-error-tag
  -999 ind 0                 <'> set-selection-frames   'wrong-type-arg   check-error-tag
  0 ind 0                    <'> set-selection-frames   'wrong-type-arg   check-error-tag
  -1                         <'> edit-fragment          'no-such-edit     check-error-tag
  101 ind 0                  <'> edit-fragment          'no-such-edit     check-error-tag
  ind 0 -2                   <'> edit-tree              'no-such-edit     check-error-tag
  ind 0 101                  <'> edit-tree              'no-such-edit     check-error-tag
  -1                         <'> add-mark               'no-such-sample   check-error-tag
  frames 2*                  <'> add-mark               'no-such-sample   check-error-tag
  "/bad/baddy"               <'> convolve-with          'no-such-file     check-error-tag
  "/bad/baddy"               <'> mix                    'no-such-file     check-error-tag
  ind 0 123                  <'> swap-channels          'no-such-sound    check-error-tag
  123 ind 0                  <'> set-show-axes          'out-of-range     check-error-tag
  -123 ind 0                 <'> set-show-axes          'out-of-range     check-error-tag
  123 ind 0                  <'> set-x-axis-style       'out-of-range     check-error-tag
  -123 ind 0                 <'> set-x-axis-style       'out-of-range     check-error-tag
  123 ind 0                  <'> set-graph-style        'out-of-range     check-error-tag
  -123 ind 0                 <'> set-graph-style        'out-of-range     check-error-tag
  #( 0 0 1 1 ) 0 #f -1.5     <'> env-sound              'out-of-range     check-error-tag
  0.0 1.0 -1.6               <'> xramp-channel          'out-of-range     check-error-tag
  0 2 -1                     <'> set-samples            'wrong-type-arg   check-error-tag
  #( 0 )                     <'> left-sample            'wrong-type-arg   check-error-tag
  #( 0 )                     <'> amp-control            'wrong-type-arg   check-error-tag
  #( 0 )                     <'> sound-loop-info        'wrong-type-arg   check-error-tag
  123 #( 0 )                 <'> add-mark               'wrong-type-arg   check-error-tag
  #( 0 0 1 1 ) 100 #f #f 1234 0 <'> filter-channel      'no-such-sound    check-error-tag
  #( 0 0 1 1 ) 100 #f #f ind 1  <'> filter-channel      'no-such-channel  check-error-tag
  vct( 0 0 1 1 ) 4 #f #f ind 1  <'> filter-channel      'no-such-channel  check-error-tag
  vct( 0 0 1 1 ) 0           <'> filter-sound           'out-of-range     check-error-tag
  vct( 0 0 1 1 ) 10          <'> filter-sound           'out-of-range     check-error-tag
  #( 0.1 0.01 ) ind          <'> set-reverb-control-length-bounds 'out-of-range check-error-tag
  #( 0.1 0.01 ) ind          <'> set-reverb-control-scale-bounds  'out-of-range check-error-tag
  #f                         <'> scale-by               'wrong-type-arg   check-error-tag
  2 0.1 0.1 0.2 0.2 make-mixer <'> scale-by             'wrong-type-arg   check-error-tag
  3.0 1.0 #t                 <'> src-sound              'wrong-type-arg   check-error-tag
  3.0 1.0 ind #t             <'> src-sound              'wrong-type-arg   check-error-tag
  ind 0 123                  <'> display-edits          'no-such-edit     check-error-tag
  ind close-sound drop
  "hiho" "time" 0 1 <'> noop 0 make-proc <'> add-transform 'bad-arity     check-error-tag
  "/bad/baddy"               <'> save-state             'cannot-save      check-error-tag
  1234 "hi" <'> noop 0 make-proc <'> add-to-menu        'no-such-menu     check-error-tag
  "hi" <'> noop 2 make-proc  <'> add-to-main-menu       'bad-arity        check-error-tag
  1 "hi" <'> noop 2 make-proc  <'> add-to-menu          'bad-arity        check-error-tag
  -1 integer->transform      <'> set-transform-type     'wrong-type-arg   check-error-tag
  123 integer->transform     <'> set-transform-type     'out-of-range     check-error-tag
  #( 0 1 ) "hiho"            <'> help-dialog            'wrong-type-arg   check-error-tag
  #( 0 1 ) "hiho"            <'> info-dialog            'wrong-type-arg   check-error-tag
  1234                       <'> edit-header-dialog     'no-such-sound    check-error-tag
  "/bad/baddy.snd"           <'> open-sound             'no-such-file     check-error-tag
  "/bad/baddy.snd" 1 22050 mus-lshort <'> open-raw-sound 'no-such-file    check-error-tag
  "/bad/baddy.snd"           <'> view-sound             'no-such-file     check-error-tag
  0 "/bad/baddy.snd"         <'> make-sampler           'no-such-file     check-error-tag
  1234567 integer->region 0  <'> make-region-sampler    'no-such-region   check-error-tag
  sf-dir "bad_chans.snd" $+ 0 0 123 234 0.0 make-vct <'> file->array 'bad-header check-error-tag
  sf-dir "bad_chans.snd" $+  <'> make-readin            'bad-header       check-error-tag
  30 3 0 make-vct            <'> make-iir-filter        'mus-error        check-error-tag
  :size 2 30 f** f>s         <'> make-wave-train        'out-of-range     check-error-tag
  0.0                        <'> set-mus-srate          'out-of-range     check-error-tag
  -1000                      <'> set-mus-srate          'out-of-range     check-error-tag
  3 0 make-vct 3 0 make-vct -1 <'> dot-product          'out-of-range     check-error-tag
  3 0 make-vct 3 0 make-vct -1 <'> multiply-arrays      'out-of-range     check-error-tag
  3 :initial-element 0.0 :initial-contents vct( 0.1 0.2 0.3 )
  <'> make-delay 'out-of-range check-error-tag
  3 :max-size 100 :initial-contents vct( 0.1 0.2 0.3 )
  <'> make-delay 'out-of-range check-error-tag
  :size 100 :wave 3 0 make-vct <'> make-table-lookup    'out-of-range     check-error-tag
  :size 100 :wave 3 0 make-vct <'> make-wave-train      'out-of-range     check-error-tag
  100 12345678               <'> make-ssb-am            'out-of-range     check-error-tag
  :envelope #( 0 0 1 1 ) :distribution 10 0 make-vct <'> make-rand 'mus-error check-error-tag
  :envelope #( 0 0 1 )       <'> make-rand              'mus-error        check-error-tag
  :envelope #( 0 0 1 1 ) :size -2 <'> make-rand         'out-of-range     check-error-tag
  :envelope #( 0 0 1 1 ) :size 1234567890 <'> make-rand 'out-of-range     check-error-tag
  make-granulate #f           <'> noop 3 make-proc <'> granulate     'bad-arity check-error-tag
  make-phase-vocoder #f       <'> noop 0 make-proc <'> phase-vocoder 'bad-arity check-error-tag
  make-phase-vocoder #f #f    <'> noop 0 make-proc <'> phase-vocoder 'bad-arity check-error-tag
  make-phase-vocoder #f #f #f <'> noop 0 make-proc <'> phase-vocoder 'bad-arity check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 <'> mus-xcoeff 'mus-error check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 <'> mus-ycoeff 'mus-error check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 1.0 <'> set-mus-xcoeff 'mus-error check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 1.0 <'> set-mus-ycoeff 'mus-error check-error-tag
  :ycoeffs 4 0 make-vct :order 12 <'> make-filter       'mus-error        check-error-tag
  make-oscil 1               <'> set-mus-offset         'mus-error        check-error-tag
  :channels 2 30 f** f>s     <'> make-locsig            'out-of-range     check-error-tag
  :width 3000                <'> make-src               'out-of-range     check-error-tag
  -1                         <'> make-frame             'out-of-range     check-error-tag
  2 0.1 0.2 make-frame 3     <'> frame-ref              'mus-error        check-error-tag
  0 0.1                      <'> make-scalar-mixer      'out-of-range     check-error-tag
  2 make-mixer 3 4           <'> mixer-ref              'mus-error        check-error-tag
  :input <'> noop 1 make-proc make-src 2000000.0 <'> src 'out-of-range    check-error-tag
  #( 1 1 ) -1                <'> partials->polynomial   'out-of-range     check-error-tag
  #( 1 1 ) 3                 <'> partials->polynomial   'out-of-range     check-error-tag
  :partials #( 1 1 ) :kind -1 <'> make-polyshape        'out-of-range     check-error-tag
  :partials #( 1 1 ) :kind 3 <'> make-polyshape         'out-of-range     check-error-tag
  1234                       <'> set-mus-header-raw-defaults 'wrong-type-arg check-error-tag
  #( 44100 2.123 "hi" )      <'> set-mus-header-raw-defaults 'wrong-type-arg check-error-tag
  mus-audio-reinitialize drop
  10 set-window-y drop
  dismiss-all-dialogs
  "test.snd" file-exists? if
    "test.snd" 0o644 file-chmod
    "test.snd" file-delete
  then
  $" cp -f oboe.snd test.snd" file-system drop
  "test.snd" open-sound to ind
  "test.snd" file-delete
  ind <'> update-sound #t nil fth-catch to tag
  stack-reset
  tag car 'cant-update-file = unless
    $" update-sound after deletion: %s" #( tag ) snd-display
  then
  ind <'> save-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'cannot-save = unless
      $" save file deleted: %s" #( tag ) snd-display
    then
  then
  ind close-sound drop
  \ 
  $" cp -f oboe.snd test.snd" file-system drop
  "test.snd" open-sound to ind
  select-all drop
  "test.snd" file-delete
  view-regions-dialog drop
  ind close-sound drop
  dismiss-all-dialogs
  \ 
  $" cp -f oboe.snd test.snd" file-system drop
  "test.snd" open-sound to ind
  "test.snd" 0o400 file-chmod
  10 delete-sample drop
  ind <'> save-sound #t nil fth-catch to tag
  stack-reset
  tag car 'cannot-save = unless
    $" save protected sound msg: %s" #( tag ) snd-display
  then
  ind close-sound drop
  \ 
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  $" cp -f oboe.snd test.snd" file-system drop
  "test.snd" 0o200 file-chmod
  "test.snd" <'> open-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-file =
    tag car 'mus-error    = || unless
      $" open read-protected sound worked!: %s" #( tag ) snd-display
    then
  then
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  ind sound? if ind close-sound drop then
  \ 
  $" cp -f oboe.snd test.snd" file-system drop
  "test.snd" 0o400 file-chmod
  "test.snd" open-sound to ind
  10 delete-sample drop
  "test.snd" <'> save-sound-as #t nil fth-catch to tag
  stack-reset
  tag car 'cannot-save = unless
    $" save-as write-protected sound msg: %s" #( tag ) snd-display
  then
  ind close-sound drop
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  \ 
  close-sound-mc-cb <'> map-channel #t nil fth-catch to tag
  stack-reset
  tag car 'no-such-channel = unless
    $" map-channel closing own chan: %s" #( tag ) snd-display
  then
  \ 
  close-sound-aoe-1-cb  "snd-test"   as-one-edit drop
  close-sound-aoe-2a-cb "outer-edit" as-one-edit drop
  close-sound-fc-cb find-channel drop
  \ 
  "oboe.snd" open-sound to ind
  0 make-sampler { rd }
  ind close-sound drop
  10 0 do rd read-sample drop loop
  rd sampler-home { home }
  home array-length 0> if
    home 0 array-ref sound? if
      $" reader-home of closed sound: %s %s?" #( home sounds ) snd-display
    then
  then
  rd sampler-position { loc }
  loc 0<> if $" closed reader position: %s?" #( loc ) snd-display then
  rd sampler-at-end? { at-end }
  at-end false? if $" closed sampler at end: %s?" #( at-end ) snd-display then
  \ 
  "oboe.snd" open-sound to ind
  vct( 0.1 0.2 0.3 ) mix-vct { mx }
  mx make-mix-sampler to rd
  ind close-sound drop
  10 0 do rd read-mix-sample drop loop
  \
  8 max-regions max set-max-regions drop
  "oboe.snd" open-sound to ind
  0 100 ind 0 make-region { reg }
  reg 0 make-region-sampler to rd
  ind close-sound drop
  reg forget-region drop
  10 0 do rd read-sample drop loop
  \
  "oboe.snd" open-sound to ind
  1.0 { scl }
  100 0.5 ind 0 set-sample drop
  scl mc-1-cb map-channel drop
  100 ind 0 sample { s100 }
  s100 1.0 fneq if $" scan + map 100: %s" #( s100 ) snd-display then
  ind revert-sound drop
  \ 
  100 0.5 ind 0 set-sample drop
  ind mc-2-cb map-channel drop
  100 ind 0 sample to s100
  s100 0.5 fneq if $" map + reset frames: %s" #( s100 ) snd-display then
  ind 0 frames { frms }
  frms 50828 <> if $" map + reset frames, frames: %s" #( frms ) snd-display then
  1 ind 0 undo drop
  ind 0 frames to frms
  frms 1 <> if $" map + reset frames, undo frames: %s" #( frms ) snd-display then
  ind revert-sound drop
  \ 
  100 0.5 ind 0 set-sample drop
  \ INFO:
  \ Doesn't work like expected with FTH.  If more values on stack
  \ than needed, no exception can be raised.  No one knows who will
  \ take and need them.  So we have 'no-such-channel as first
  \ exception.
  \ frames ( snd chn edpos -- frms )
  \ set-frames ( frms snd chn -- val )
  1 ind 0 <'> set-frames #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'wrong-number-of-args = unless
      $" set frames + edpos: %s" #( tag ) snd-display
    then
  then
  ind revert-sound drop
  'complex provided? if
    <'> mc-3-cb <'> map-channel #t nil fth-catch to tag
  then
  stack-reset
  tag if
    tag car 'bad-type = unless
      $" map-channel rtn complex: %s" #( tag ) snd-display
    then
  then
  0 make-sampler to rd
  10 0 do rd #() apply drop loop
  rd copy-sampler { crd }
  ind close-sound drop
  10 0 do crd read-sample drop loop
  crd sampler-home to home
  home array-length 0> if
    home 0 array-ref sound? if
      $" copy reader-home of closed sound: %s %s?" #( home sounds ) snd-display
    then
  then
  crd sampler-position to loc
  loc 0<> if $" closed copy reader position: %s?" #( loc ) snd-display then
  crd sampler-at-end? to at-end
  at-end false? if $" closed copy sampler at end: %s?" #( at-end ) snd-display then
  \
  ind <'> revert-sound #t nil fth-catch to tag
  stack-reset
  tag car 'no-such-sound = unless
    $" revert-sound of closed sound: %s" #( tag ) snd-display
  then
  \
  "oboe.snd" open-sound to ind
  100 0.5 ind 0 set-sample drop
  0.5 0 100 ind 0 ind edpos-1-cb <'> scale-channel #t nil fth-catch to tag
  stack-reset
  tag car 'bad-arity = unless
    $" edpos proc bad args: %s" #( tag ) snd-display
  then
  ind sound? unless $" edpos bad arity proc clobbers chan??: %s" #( ind ) snd-display then
  \ 
  0.5 0 100 ind 0 <'> edpos-2-cb <'> scale-channel #t nil fth-catch to tag
  stack-reset
  tag car 'no-such-channel = unless
    $" edpos clobbers channel: %s" #( tag ) snd-display
  then
  ind sound? if $" edpos proc clobbers chan??: %s" #( ind ) snd-display then
  *snd-test-verbose* if
    $" procs   prcs/set-prcs" #f snd-test-message
    $" =====================" #f snd-test-message
    $" procs00: %3d/%3d" #( procs00 length set-procs00 length ) snd-test-message
    $" procs01: %3d/%3d" #( procs01 length set-procs01 length ) snd-test-message
    $" procs02: %3d/%3d" #( procs02 length set-procs02 length ) snd-test-message
    $" procs03: %3d/%3d" #( procs03 length set-procs03 length ) snd-test-message
    $" procs04: %3d/%3d" #( procs04 length set-procs04 length ) snd-test-message
    $" procs05: %3d"     #( procs05 length )                    snd-test-message
    $" procs06: %3d"     #( procs06 length )                    snd-test-message
    $" procs07: %3d"     #( procs07 length )                    snd-test-message
    $" procs08: %3d"     #( procs08 length )                    snd-test-message
    $" procs10: %3d"     #( procs10 length )                    snd-test-message
  then
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 :wave -1 0 1 #f #t '() vector-0 12345678901234567890 ) { vals }
  nil nil nil nil nil nil nil { arg1 arg2 arg3 arg4 tm prc tag }
  "keyargs-2-args" #f snd-test-message
  keyargs each to arg1
    vals each to arg2
      make-procs each to prc
  	arg1 arg2 prc #t nil fth-catch stack-reset
      end-each
    end-each
  end-each
  all-args if
    "keyargs-3-args" #f snd-test-message
    vals each to arg1
      keyargs each to arg2
	vals each to arg3
	  make-procs each to prc
	    arg1 arg2 arg3 prc #t nil fth-catch stack-reset
	  end-each
	end-each
      end-each
    end-each
    "keyargs-4-args" #f snd-test-message
    keyargs each to arg1
      vals each to arg2
	keyargs each to arg3
	  vals each to arg4
	    make-procs each to prc
	      arg1 arg2 arg3 arg4 prc #t nil fth-catch stack-reset
	    end-each
	  end-each
	end-each
      end-each
    end-each
  then
  \ 0 args
  "0-args" #f snd-test-message
  procs00 each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-number-of-args = if
      $" procs00: %s %s" #( prc tag ) snd-display
    then
  end-each
  dismiss-all-dialogs
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 color-95  #( 0 1 ) 3/4 'mus-error -1.0 csqrt
     <'> noop 0 make-proc vct-5 sound-data-23 :order 0 1 -1 a-hook #f #t <char> c 0.0 1.0 -1.0 
     '() '3 2 8 64 -64 vector-0 2.0 21.5 f** 2.0 -18.0 f** car-main cadr-main 
     12345678901234567890 <'> noop 1 make-proc ) { main-args }
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 color-95 #( 0 1 ) 3/4 -1.0
     -1.0 csqrt :feedback -1 0 1 3 64 -64 #f #t '() vector-0 12345678901234567890 ) { few-args }
  #( "/hiho" 1234 vct-3 -1.0 -1.0 csqrt
     -1 0 1 #f #t #() 12345678901234567890 ) { fewer-args }
  all-args if main-args else few-args then { less-args }
  \ 1 arg
  "1-arg" #f snd-test-message
  nil { arg }
  main-args each to arg
    procs01 each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-number-of-args = if
	$" procs01 wna: (%s) %s %s" #( arg prc tag ) snd-display
      then
    end-each
  end-each
  cr					\ CR here because string "/hiho" appears on stdout
					\ (or all args with snd-nogui)
  \ 2 args
  "2-args" #f snd-test-message
  main-args each to arg1
    main-args each to arg2
      procs02 each to prc
	arg1 arg2 prc #t nil fth-catch to tag
	stack-reset
	tag car 'wrong-number-of-args = if
	  $" procs02: (%s %s) %s %s" #( arg1 arg2 prc tag ) snd-display
	then
      end-each
    end-each
  end-each
  \ set! no args
  "set-no-args" #f snd-test-message
  main-args each to arg
    set-procs00 each to prc
      arg prc set-xt #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-number-of-args = if
	$" set-procs00: (%s) %s %s" #( arg prc tag ) snd-display
      then
    end-each
  end-each
  dismiss-all-dialogs
  \ set! 1 arg
  "set-1-arg" #f snd-test-message
  main-args each to arg1
    main-args each to arg2
      set-procs01 each to prc
	prc proc-name "widget-size" string= unless
	  arg1 arg2 prc set-xt #t nil fth-catch to tag
	  stack-reset
	  tag car 'wrong-number-of-args = if
	    $" set-procs01: (%s %s) %s %s" #( arg1 arg2 prc tag ) snd-display
	  then
	then
      end-each
    end-each
  end-each
  all-args if
    \ set! 2 args
    "set-2-args" #f snd-test-message
    less-args each to arg1
      less-args each to arg2
	less-args each to arg3
	  set-procs02 each to prc
	    arg1 arg2 arg3 prc set-xt #t nil fth-catch to tag
	    stack-reset
	    tag car 'wrong-number-of-args = if
	      $" set-procs02: (%s %s %s) %s %s" #( arg1 arg2 arg3 prc tag ) snd-display
	    then
	  end-each
	end-each
      end-each
    end-each
    nil nil nil nil nil nil { arg5 arg6 arg7 arg8 arg9 arg0 }
    \ 3 args
    "3-args" #f snd-test-message
    make-timer to tm
    less-args each to arg1
      less-args each to arg2
	less-args each to arg3
	  procs03 each to prc
	    arg1 arg2 arg3 prc #t nil fth-catch to tag
	    stack-reset
	    tag car 'wrong-number-of-args = if
	      $" procs03: (%s %s %s) %s %s" #( arg1 arg2 arg3 prc tag ) snd-display
	    then
	  end-each
	end-each
      end-each
    end-each
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ set! 3 args
    "set!-3-args" #f snd-test-message
    tm start-timer
    less-args each to arg1
      less-args each to arg2
	less-args each to arg3
	  less-args each to arg4
	    set-procs03 each to prc
	      arg1 arg2 arg3 arg4 prc #t nil fth-catch to tag
	      stack-reset
	      tag car 'wrong-number-of-args = if
		$" set-procs03: (%s %s %s %s) %s %s" #( arg1 arg2 arg3 arg4 prc tag ) snd-display
	      then
	    end-each
	  end-each
	end-each
      end-each
    end-each
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ 4 args
    "4-args" #f snd-test-message
    tm start-timer
    few-args each to arg1
      few-args each to arg2
	few-args each to arg3
	  few-args each to arg4
	    procs04 each to prc
	      arg1 arg2 arg3 arg4 prc #t nil fth-catch to tag
	      stack-reset
	      tag car 'wrong-number-of-args = if
		$" procs04: (%s %s %s %s) %s %s" #( arg1 arg2 arg3 arg4 prc tag ) snd-display
	      then
	    end-each
	  end-each
	end-each
      end-each
    end-each
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ set! 4 args
    "set!-4-args" #f snd-test-message
    tm start-timer
    few-args each to arg1
      few-args each to arg2
	few-args each to arg3
	  few-args each to arg4
	    few-args each to arg5
	      set-procs04 each to prc
		arg1 arg2 arg3 arg4 arg5 prc #t nil fth-catch to tag
		stack-reset
		tag car 'wrong-number-of-args = if
		  $" set-procs04: (%s %s %s %s %s) %s %s"
		  #( arg1 arg2 arg3 arg4 arg5 prc tag ) snd-display
		then
	      end-each
	    end-each
	  end-each
	end-each
      end-each
    end-each
    clear-sincs drop
    stop-playing drop
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ 5 args
    "5-args" #f snd-test-message
    tm start-timer
    fewer-args each to arg1
      fewer-args each to arg2
	fewer-args each to arg3
	  fewer-args each to arg4
	    fewer-args each to arg5
	      procs05 each to prc
		arg1 arg2 arg3 arg4 arg5 prc #t nil fth-catch to tag
		stack-reset
		tag car 'wrong-number-of-args = if
		  $" procs05: (%s %s %s %s %s) %s %s"
		  #( arg1 arg2 arg3 arg4 arg5 prc tag ) snd-display
		then
	      end-each
	    end-each
	  end-each
	end-each
      end-each
    end-each
    clear-sincs drop
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ 6 args
    "6-args" #f snd-test-message
    tm start-timer
    #( 1.5 "/hiho" -1234 #f #t vct-5 ) each to arg1
      #( 1.5 -1234 vct-3 vct-5 -1 0 #f #t ) each to arg2
	#( 1.5 "/hiho" -1234 0 vct-3 -1 #f #t ) each to arg3
	  #( 1.5 "/hiho" -1234 0 vct-3 #f #t ) each to arg4
	    #( 1.5 "/hiho" -1234 0 vct-5 #f #t ) each to arg5
	      #( 1.5 "/hiho" -1234 -1 0 #f #t #() vct-3 ) each to arg6
		procs06 each to prc
		  arg1 arg2 arg3 arg4 arg5 arg6 prc #t nil fth-catch to tag
		  stack-reset
		  tag car 'wrong-number-of-args = if
		    $" procs06: (%s %s %s %s %s %s) %s %s"
		    #( arg1 arg2 arg3 arg4 arg5 arg6 prc tag ) snd-display
		  then
		end-each
	      end-each
	    end-each
	  end-each
	end-each
      end-each
    end-each
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ 8 args
    "8-args" #f snd-test-message
    tm start-timer
    #( 1.5 -1 1234 #f #() ) each to arg1
      #( "/hiho" -1 1234 #() vct-5 ) each to arg2
	#( #t #f -1 1234 #() vct-3 ) each to arg3
	  #( -1.0 csqrt  1234 0 -1 #() ) each to arg4
	    #( 1.5 -1 #f 1 234 vct-3 #() ) each to arg5
	      #( 2 #f #t 1234 vct-5 -1 ) each to arg6
		#( #f #t -1 1234 vct-3 ) each to arg7
		  #( 1.5 -1 #() 1234 "/hiho" ) each to arg8
		    procs08 each to prc
		      arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 prc #t nil fth-catch to tag
		      stack-reset
		      tag car 'wrong-number-of-args = if
			$" procs08: (%s %s %s %s %s %s %s %s) %s %s"
			#( arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 prc tag ) snd-display
		      then
		    end-each
		  end-each
		end-each
	      end-each
	    end-each
	  end-each
	end-each
      end-each
    end-each
    tm stop-timer
    "%s" #( tm ) snd-test-message
    \ 10 args
    "10-args" #f snd-test-message
    tm start-timer
    #( 1.5 -1 #f 1234 ) each to arg1
      #( "/hiho" -1 1234 ) each to arg2
	#( #t #f vct-3 1234 ) each to arg3
	  #( -1.0 csqrt #f -1 vct-5 ) each to arg4
	    #( 1.5 #f -1 1234 #() ) each to arg5
	      #( -2 #f 1234 vct-3 ) each to arg6
		#( #f #t #() 1234 vct-5 ) each to arg7
		  #( 1.5 -1 "/hiho" #() ) each to arg8
		    #( 1.5 -1 #() ) each to arg9
		      #( #f -1 1234 ) each to arg0
			procs10 each to prc
			  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg0 prc #t nil fth-catch
			  to tag
			  stack-reset
			  tag car 'wrong-number-of-args = if
			    $" procs10: (%s %s %s %s %s %s %s %s %s %s) %s %s"
			    #( arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg0 prc tag )
			    snd-display
			  then
			end-each
		      end-each
		    end-each
		  end-each
		end-each
	      end-each
	    end-each
	  end-each
	end-each
      end-each
    end-each
    clear-sincs drop
    tm stop-timer
    "%s" #( tm ) snd-test-message
  then
;

: 30-test
  32 make-delay { delay-32 }
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 :wave -1 0 1 #f #t
     '() vector-0 1234567890 ) { vals }
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 color-95  #( 0 1 ) 3/4 'mus-error -1.0 csqrt ( delay-32 )
     <'> noop 0 make-proc vct-5 sound-data-23 :order 0 1 -1 a-hook #f #t <char> c 0.0 1.0 -1.0 
     '() '3 2 8 64 -64 vector-0 2.0 21.5 f** 2.0 -18.0 f** car-main cadr-main 
     1234567890 <'> noop 1 make-proc ) { main-args }
  #( 1.5 "/hiho" #( 0 1 ) 1234 vct-3 color-95 #( 0 1 ) 3/4 -1.0
     -1.0 csqrt ( delay-32 ) :feedback -1 0 1 3 64 -64 #f #t '() vector-0 1234567890 ) { few-args }
  #( "/hiho" 1234 vct-3 -1.0 -1.0 csqrt ( delay-32 )
     -1 0 1 #f #t #() 1234567890 ) { fewer-args }
  fewer-args { less-args }
  fewer-args to vals
  fewer-args to few-args
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  { ind prc tag arg arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg0 tm }
  make-timer { tm }
  clear-sincs drop
  tm stop-timer
  "%s" #( tm ) snd-test-message
;

SIGSEGV lambda: { sig -- }
  stack-reset
  backtrace
  "" #f snd-test-message
  $" Segmentation fault (signal no %d)" #( sig ) snd-test-message
  "" #f snd-test-message
  finish-snd-test
  2 snd-exit drop
; signal drop
SIGILL lambda: { sig -- }
  stack-reset
  backtrace
  "" #f snd-test-message
  $" Illegal instruction (signal no %d)" #( sig ) snd-test-message
  "" #f snd-test-message
  finish-snd-test
  2 snd-exit drop
; signal drop
SIGINT lambda: { sig -- }
  stack-reset
  backtrace
  "" #f snd-test-message
  $" Interrupt received.  Clean up %S." #( *filename* #f file-basename ) snd-test-message
  "" #f snd-test-message
  finish-snd-test
  0 snd-exit drop
; signal drop

let: ( -- )
  #() { numbs }
  script-arg 0> if
    script-args length script-arg 1+ ?do
      script-args i list-ref string->number { n }
      script-arg 1+ set-script-arg drop
      n 0< if
	numbs        n array-push to numbs \ negative number means exclude this test
      else
	test-numbers n array-push to test-numbers
      then
    loop
  then
  test-numbers empty? if
    29 -1 do test-numbers i array-push to test-numbers loop
  then
  numbs each abs { n } test-numbers test-numbers n array-index array-delete! drop end-each
  .stack
  start-snd-test
  <'> 00-sel-from-snd    run-fth-test
  <'> 10-marks           run-fth-test
  <'> 15-chan-local-vars run-fth-test
  <'> 19-save/restore    run-fth-test
  <'> 23-with-sound      run-fth-test
  <'> 28-errors          run-fth-test
  <'> 30-test            run-fth-test	\ local fragment test
  finish-snd-test
  with-exit if 0 snd-exit drop then
;let

\ snd-test.fs ends here
