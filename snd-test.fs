\ snd-test.fs -- snd-test.scm|rb tests -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sat Aug 05 00:09:28 CEST 2006
\ Changed: Wed Nov 03 11:55:21 CET 2010

\ Commentary:
\
\ You may use an init file name `pwd`/.sndtest.fs or ~/.sndtest.fs to set
\ global variables or define other special functions, hooks, etc.
\
\ snd-forth -noinit -load snd-test.fs          \ all tests
\ snd-forth -noinit -load snd-test.fs 10 15 19 \ test 10 15 19
\ snd-forth -noinit -load snd-test.fs -23      \ all tests but 23
\
\ test  0: general
\ test 10: marks
\ test 15: chan-local vars
\ test 19: save and restore
\ test 23: with-sound
\ test 26: Gtk
\ test 28: errors

24 set-object-print-length

'snd-nogui provided? [if]
  #f value stdout-io
  #f value stderr-io
[else]
  \ Prints to Snd's listener and stdout/stderr.

  \ The original CLM-PRINT utilizes SND-PRINT only but we want output
  \ to stdout/stderr too.
  : clm-print ( fmt :optional args -- ) fth-format ( str ) snd-print ( str ) .stdout ;

  :port-name "sndout"
  :write-line lambda: <{ line -- }> line snd-print ( line ) .stdout ;
  make-soft-port set-*stdout* value stdout-io

  :port-name "snderr"
  :write-line lambda: <{ line -- }> line snd-print ( line ) .stderr ;
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
: snd-display ( -- ; fmt args -- ) postpone *lineno* postpone (snd-display) ; immediate
previous

\ *SND-TEST-VERBOSE*: progress information in long tests
\
\ test 19-save/restore: function names
\ test       28-errors: prints proc-array length and progress information
#f value *snd-test-verbose*

\ WITH-SOUND control
\
\ test   23-with-sound: :play
\ test   23-with-sound: :statistics
\ test   23-with-sound: :verbose (event (bird) and instrument names)
#f value *snd-test-ws-play*
#f value *snd-test-ws-statistics*
#f value *snd-test-ws-verbose*

\ You may set them in .sndtest.fs.
#f value my-snd-error-hook
#f value my-mus-error-hook

save-dir "/home/bil/zap/snd" ||   value original-save-dir
temp-dir "/home/bil/zap/tmp" ||   value original-temp-dir
listener-prompt        		  value original-prompt
1024 8 *               		  value default-file-buffer-size
"HOME" getenv          		  value home-dir
"/home/bil/sf1/"       		  value sf-dir
#f                     		  value all-args
"/home/bil/zap/sounds/bigger.snd" value bigger-snd

\ let: ( -- )
\   file-pwd "/peaks" $+ { dir }
\   dir file-directory? unless dir 0o755 file-mkdir then
\   dir set-peak-env-dir drop
\ ;let

\ Global variables may be overridden in `pwd`/.sndtest.fs or ~/.sndtest.fs
".sndtest.fs" load-init-file

\
\ lambda: <{ -- }> cr gc-stats cr .memory cr cr ; at-exit
\
\ before-load-hook lambda: <{ file -- f }> $" loading " file $+ #f snd-test-message #t ; add-hook!
\

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
require snd-xm
require effects
require bird.fsm

*clm-search-list* file-pwd array-push to *clm-search-list*

reset-all-hooks

'snd-motif provided? [if]
  lambda: <{ dpy e -- }>
    dpy e Ferror_code   nil 1024 FXGetErrorText { res }
    $" Xlib error_code[%s]: %s"   res fth-warning
    dpy e Frequest_code nil 1024 FXGetErrorText to res
    $" Xlib request_code[%s]: %s" res fth-warning
    dpy e Fminor_code   nil 1024 FXGetErrorText to res
    $" Xlib minor_code[%s]: %s"   res fth-warning
  ; FXSetErrorHandler drop
  lambda: <{ dpy -- }>
    $" Xlib IO Error dpy: %S" #( dpy ) fth-error
  ; FXSetIOErrorHandler drop
[then]

: fneq-err ( r1 r2 err -- f ) -rot f- fabs f<= ;

: cneq-err ( c1 c2 err -- f )
  { c1 c2 err }
  c1 real-ref  c2 real-ref  err fneq-err
  c1 image-ref c2 image-ref err fneq-err ||
;

: fneq   ( a b -- f ) 0.001 fneq-err ;
: ffneq  ( a b -- f ) 0.010 fneq-err ;
: fffneq ( a b -- f ) 0.100 fneq-err ;
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

: fveql  ( v1 v2 idx -- f)
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

'snd-nogui provided? [if]
  <'> noop alias dismiss-all-dialogs
[else]
  : dismiss-all-dialogs ( -- )
    nil nil { dialog d }
    dialog-widgets each to dialog
      dialog if
	dialog 0 array-ref symbol? if
	  dialog is-managed? if dialog hide-widget drop then
	else
	  dialog each to d
	    d 0 array-ref symbol? if
	      d is-managed? if d hide-widget drop then
	    then
	  end-each
	then
      then
    end-each
  ;
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
  \ set-mus-srate returns old srate
  22050 dup set-mus-srate drop to *clm-srate*
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
     "saved-snd.fs"
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
  #t set-show-listener drop
  "test-forth.output" save-listener drop
  original-prompt set-listener-prompt drop
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

\ ====== test 0: general
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

\ bind-key proc
: C-xC-c <{ -- }> 0 snd-exit ;

\ hooks
: my-test1-proc <{ fname -- f }> #f ;
<'> my-test1-proc alias my-test2-proc
<'> my-test1-proc alias my-test3-proc
<'> my-test1-proc alias my-test4-proc
<'> my-test1-proc alias my-test5-proc

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

: map-10-times-cb <{ y -- y' }> y 10.0 f* ;

: map-diff-plus-cb { mx -- proc; y self -- y' }
  1 proc-create 1.001 mx f- , ( proc )
 does> { y self -- y' }
  self @ y f+
;

: scan-less-than-zero-cb <{ y -- y' }> y f0< ;

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
  #( <'> my-test3-proc
     <'> my-test4-proc
     <'> my-test5-proc ) <'> my-local-thunk with-local-hook
  open-hook object-length 2 <> if
    $" add-hook! reset length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook <'> my-test1-proc hook-member? unless
    $" reset1 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook "my-test2-proc" hook-member? unless
    $" reset2 add-hook!: %s" #( open-hook ) snd-display
  then
  \ bind-key
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
  \ new-sound
  "fmv.snd" mus-next mus-bshort 22050 1 $" set-samples test" 100 new-sound { ind }
  10  3  3 0.1 make-vct set-samples drop
  0 20 ind 0 channel->vct { res }
  res vct( 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 ) vequal unless
    $" 1 set samples 0 for 0.1: %s?" #( res ) snd-display
  then
  ind close-sound drop
  \ check clipping choices (test004)
  "oboe.snd" view-sound to ind
  #f set-clipping drop
  <'> map-10-times-cb 0 ind undef undef frames ind 0 map-channel drop
  "test.snd" ind mus-next mus-bfloat save-sound-as drop
  1 ind 0 undo drop
  "test.snd" open-sound { ind1 }
  ind1 0 undef maxamp { res1 }
  ind  0 undef maxamp { ind-mx }
  res1 ind-mx 10.0 f*  fneq if $" clipping 0: %s %s?" #( res1 ind-mx ) snd-display then
  ind1 close-sound drop
  "test.snd" file-delete
  \ 
  #t set-clipping drop
  <'> map-10-times-cb 0 ind undef undef frames ind 0 map-channel drop
  "test.snd" ind mus-next mus-bfloat save-sound-as drop
  1 ind 0 undo drop
  "test.snd" open-sound to ind1
  ind1 0 undef maxamp to res1
  \ ind-mx == ind 0 maxamp from above
  res1 1.0 fneq if $" clipping 1: %s %s?" #( res1 ind-mx ) snd-display then
  ind1 close-sound drop
  "test.snd" file-delete
  \ 
  #f set-clipping drop
  ind-mx map-diff-plus-cb 0 ind undef undef frames ind 0 map-channel drop
  "test.snd" ind mus-next mus-bshort save-sound-as drop
  "test.snd" open-sound to ind1
  <'> scan-less-than-zero-cb scan-channel to res1
  res1 unless $" clipping 2: %s?" #( res1 ) snd-display then
  ind1 close-sound drop
  "test.snd" file-delete
  \ 
  #t set-clipping drop
  "test.snd" ind mus-next mus-bshort save-sound-as drop
  "test.snd" open-sound to ind1
  <'> scan-less-than-zero-cb scan-channel to res1
  res1 if $" clipping 3: %s?" #( res1 ) snd-display then
  ind1 close-sound drop
  "test.snd" file-delete
  #f set-clipping drop
  ind close-sound drop
  \
  #f set-clipping drop
  "test.snd" :data-format mus-lshort new-sound { snd }
  0 10 pad-channel drop
  1  1.0    set-sample drop
  2 -1.0    set-sample drop
  3  0.9999 set-sample drop
  4  2.0    set-sample drop
  5 -2.0    set-sample drop
  6  1.3    set-sample drop
  7 -1.3    set-sample drop
  8  1.8    set-sample drop
  9 -1.8    set-sample drop
  snd save-sound drop
  snd close-sound drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  \ FIXME
  \ clipping(#f)
  \ data-format mus-lshort:
  \ GCC   vct( 0.0 1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 )
  \ Clang vct( 0.0 1.0 -1.0 1.0  0.0  0.0 -0.7  0.7 -0.2  0.2 )
  res vct( 0.0 1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 ) vequal unless
    res vct( 0.0 1.0 -1.0 1.0 0.0 0.0 -0.7 0.7 -0.2 0.2 ) vequal if
      $" clipping(#f): clang" #() snd-display
    else
      $" clipping(#f):: %s?" #( res ) snd-display
    then
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \ 
  #t set-clipping drop
  "test.snd" :data-format mus-lshort new-sound { snd }
  0 10 pad-channel drop
  1  1.0    set-sample drop
  2 -1.0    set-sample drop
  3  0.9999 set-sample drop
  4  2.0    set-sample drop
  5 -2.0    set-sample drop
  6  1.3    set-sample drop
  7 -1.3    set-sample drop
  8  1.8    set-sample drop
  9 -1.8    set-sample drop
  snd save-sound drop
  snd close-sound drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  res vct( 0.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 -1.0 ) vequal unless
    $" clipping(#t): %s?" #( res ) snd-display
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \ 
  vct( 0.0 1.0 -1.0 0.9999 2.0 -2.0 1.3 -1.3 1.8 -1.8 ) { data }
  data vct->sound-data { sdata }
  "test.snd" 22050 1 mus-lshort mus-riff $" a comment" mus-sound-open-output to snd
  snd #f set-mus-file-clipping drop
  snd 0 9 1 sdata mus-sound-write drop
  snd 40 mus-sound-close-output drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  \ FIXME
  \ mus-file-clipping(#f)
  \ data-format mus-lshort:
  \ GCC   vct( 0.0 -1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 )
  \ Clang vct( 0.0 -1.0 -1.0 1.0  0.0  0.0 -0.7  0.7 -0.2  0.2 )
  res vct( 0.0 -1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 ) vequal unless
    res vct( 0.0 -1.0 -1.0 1.0 0.0 0.0 -0.7 0.7 -0.2 0.2 ) vequal if
      $" mus-file-clipping(#f): clang" #() snd-display
    else
      $" mus-file-clipping(#f): %s?" #( res ) snd-display
    then
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \ 
  vct( 0.0 1.0 -1.0 0.9999 2.0 -2.0 1.3 -1.3 1.8 -1.8 ) { data }
  data vct->sound-data { sdata }
  "test.snd" 22050 1 mus-lshort mus-riff $" a comment" mus-sound-open-output to snd
  snd #t set-mus-file-clipping drop
  snd 0 9 1 sdata mus-sound-write drop
  snd #f set-mus-file-clipping drop
  snd 40 mus-sound-close-output drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  res vct( 0.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 -1.0 ) vequal unless
    $" mus-file-clipping(#t): %s?" #( res ) snd-display
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \
  #f set-mus-clipping drop
  vct( 0.0 1.0 -1.0 0.9999 2.0 -2.0 1.3 -1.3 1.8 -1.8 ) { data }
  data vct->sound-data { sdata }
  "test.snd" 22050 1 mus-lshort mus-riff $" a comment" mus-sound-open-output to snd
  snd 0 9 1 sdata mus-sound-write drop
  snd 40 mus-sound-close-output drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  \ FIXME
  \ mus-clipping(#f)
  \ data-format mus-lshort:
  \ GCC   vct( 0.0 -1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 )
  \ Clang vct( 0.0 -1.0 -1.0 1.0  0.0  0.0 -0.7  0.7 -0.2  0.2 )
  res vct( 0.0 -1.0 -1.0 1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 ) vequal unless
    res vct( 0.0 -1.0 -1.0 1.0 0.0 0.0 -0.7 0.7 -0.2 0.2 ) vequal if
      $" mus-clipping(#f): clang" #() snd-display
    else
      $" mus-clipping(#f): %s?" #( res ) snd-display
    then
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \ 
  #t set-mus-clipping drop
  vct( 0.0 1.0 -1.0 0.9999 2.0 -2.0 1.3 -1.3 1.8 -1.8 ) { data }
  data vct->sound-data { sdata }
  "test.snd" 22050 1 mus-lshort mus-riff $" a comment" mus-sound-open-output to snd
  snd 0 9 1 sdata mus-sound-write drop
  snd 40 mus-sound-close-output drop
  "test.snd" open-sound to snd
  0 10 channel->vct to res
  res vct( 0.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 -1.0 ) vequal unless
    $" mus-clipping(#t): %s?" #( res ) snd-display
  then
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \
  #t set-mus-clipping drop
  vct( 0.0 1.0 -1.0 0.9999 2.0 -2.0 1.3 -1.3 1.8 -1.8 ) { data }
  data vct->sound-data { sdata }
  "test.snd" 22050 1 mus-lshort mus-riff $" a comment" mus-sound-open-output to snd
  snd 0 10 1 sdata <'> mus-sound-write #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'out-of-range <> if
    $" mus-sound-write too many bytes: %s" #( res ) snd-display
  then
  snd 0 10 1 sdata <'> mus-sound-read #t nil fth-catch to res
  stack-reset
  res 0 array-ref 'out-of-range <> if
    $" mus-sound-read too many bytes: %s" #( res ) snd-display
  then
  snd 0 mus-sound-close-output drop
  "test.snd" file-delete
  "test.snd" mus-sound-forget drop
  #f set-mus-clipping drop 		\ default
  #f set-clipping drop
  \ x-axis-label (test005)
  'snd-nogui provided? unless
    "oboe.snd" open-sound to ind
    #t set-transform-graph? drop
    #t set-time-graph? drop
    x-axis-label to res
    res "time" string<> if $" get time x-axis-label: %s?" #( res ) snd-display then
    "hiho1" ind 0 time-graph set-x-axis-label drop
    x-axis-label to res
    res "hiho1" string<> if $" set time x-axis-label: %s?" #( res ) snd-display then
    update-transform-graph drop
    ind 0 transform-graph x-axis-label to res
    res "frequency" string<> if $" get fft x-axis-label: %s?" #( res ) snd-display then
    "hiho2" ind 0 transform-graph set-x-axis-label drop
    update-transform-graph drop
    ind 0 transform-graph x-axis-label to res
    res "hiho2" string<> if $" set fft x-axis-label: %s?" #( res ) snd-display then
    "frequency" ind 0 transform-graph set-x-axis-label drop
    '( 0 0 1 1 2 0 ) "lisp" graph drop
    update-lisp-graph drop
    ind 0 lisp-graph x-axis-label to res
    res "lisp" string<> if $" get lisp x-axis-label: %s?" #( res ) snd-display then
    "hiho3" ind 0 lisp-graph set-x-axis-label drop
    ind 0 lisp-graph x-axis-label to res
    res "hiho3" string<> if $" set lisp x-axis-label: %s?" #( res ) snd-display then
    "hiho4" ind 0 time-graph set-y-axis-label drop
    y-axis-label to res
    res "hiho4" string<> if $" set time y-axis-label: %s?" #( res ) snd-display then
    "hiho5" ind 0 lisp-graph set-y-axis-label drop
    ind 0 lisp-graph y-axis-label to res
    res "hiho5" string<> if $" set lisp y-axis-label: %s?" #( res ) snd-display then
    #f set-y-axis-label drop
    "hiho6" ind 0 set-y-axis-label drop
    ind 0 y-axis-label to res
    res "hiho6" string<> if $" set time y-axis-label (time): %s?" #( res ) snd-display then
    #f set-y-axis-label drop
    ind close-sound drop
  then
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
      "effects-jc-reverb-1" ) ) value test19-*.fs

: 19-save/restore ( -- )
  "oboe.snd" open-sound { ind }
  nil nil nil nil nil { vals func1 descr name func }
  test19-*.fs each to vals
    vals 0 array-ref to func1
    vals 1 array-ref to descr
    vals 2 array-ref to name
    *snd-test-verbose* if name #f snd-test-message then
    func1 #() run-proc drop
    ind #f undef undef edit-list->function to func
    func proc-source-ref descr string<> if
      $" edit-list->function %d: %s?" #( i func proc-source-ref ) snd-display
    then
    ind revert-sound drop
    func #( ind 0 ) run-proc drop
    ind revert-sound drop
  end-each
  ind close-sound drop
;

\ ====== test 23: with-sound
: test23-notehook { ins start dur -- } $" %14s: %5.2f  %5.2f" #( ins start dur ) snd-test-message ;

: test23-balance ( -- )
  make-rmsgain    { rg }
  40 make-rmsgain { rg1 }
  2  make-rmsgain { rg2 }
  #( 0 0 1 1 2 0 )      :length 10000 make-env { e }
  #( 0 0 1 1 )          :length 10000 make-env { e1 }
  #( 0 0 1 1 2 0 10 0 ) :length 10000 make-env { e2 }
  440.0 make-oscil { o }
  nil { sig }
  10000 0 do
    e env to sig
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

: check-maxamp { fname name -- }
  fname mus-sound-maxamp each { val }
    i 2 mod 0<> if
      val 1.1 f> if $" %s: maxamp chn %s > 1.0: %s" '( name i 1- 2/ val ) snd-display then
    then
  end-each
;

: ws-close-sound { ws -- }
  ws ws-output 0 find-sound dup sound? if close-sound then drop
  ws :statistics ws-ref unless ws ws-output ws :comment ws-ref check-maxamp then
;

: 23-with-sound ( -- )
  1024 1024 * to *clm-file-buffer-size*
  *clm-play*       { old-play }
  *clm-statistics* { old-stats }
  *snd-test-ws-play*       to *clm-play*
  *snd-test-ws-statistics* to *clm-statistics*
  \ from bird.fsm
  <'> bird-test
  :comment  over object->string
  :verbose  *snd-test-ws-verbose*
  :channels 2 with-sound ws-close-sound
  \ from clm-ins.fs
  0.0 0.3 <'> clm-ins-test
  :comment  over object->string
  :notehook *snd-test-ws-verbose* if <'> test23-notehook else #f then
  :channels 2 with-sound ws-close-sound
  <'> test23-balance
  :comment  over object->string
  :channels 3 with-sound ws-output 0 find-sound { ind }
  ind sound? if ind close-sound drop else $" with-sound balance?" snd-display then
  "test.snd" "test23-balance" check-maxamp
  "tmp.snd" mus-next mus-bfloat 22050 1 new-sound to ind
  0 1000 ind 0 pad-channel drop
  100.0 make-oscil { mg }
  1000 make-ssb-fm { gen }
  gen mg test23-ssb-fm <'> map-channel #t nil fth-catch stack-reset
  ind close-sound drop
  \ examples from sndclm.html
  <'> sndclm-oscil-test          :comment over object->string with-sound ws-close-sound
  <'> sndclm-env-test            :comment over object->string with-sound ws-close-sound
  <'> sndclm-table-lookup-test   :comment over object->string with-sound ws-close-sound
  <'> sndclm-polywave-test       :comment over object->string with-sound ws-close-sound
  <'> sndclm-triangle-wave-test  :comment over object->string with-sound ws-close-sound
  <'> sndclm-ncos-test           :comment over object->string with-sound ws-close-sound
  <'> sndclm-nrxycos-test        :comment over object->string with-sound ws-close-sound
  <'> sndclm-ssb-am-test         :comment over object->string with-sound ws-close-sound
  <'> sndclm-wave-train-test     :comment over object->string with-sound ws-close-sound
  <'> sndclm-rand-test :comment over object->string :channels 2 with-sound ws-close-sound
  <'> sndclm-two-pole-test       :comment over object->string with-sound ws-close-sound
  <'> sndclm-firmant-test        :comment over object->string with-sound ws-close-sound
  <'> sndclm-iir-filter-test     :comment over object->string with-sound ws-close-sound
  <'> sndclm-delay-test          :comment over object->string with-sound ws-close-sound
  <'> sndclm-comb-test           :comment over object->string with-sound ws-close-sound
  <'> sndclm-all-pass-test       :comment over object->string with-sound ws-close-sound
  <'> sndclm-moving-average-test :comment over object->string with-sound ws-close-sound
  <'> sndclm-src1-test :comment over object->string :srate 22050 with-sound ws-close-sound
  <'> sndclm-src2-test           :comment over object->string with-sound ws-close-sound
  <'> sndclm-convolve1-test      :comment over object->string with-sound ws-close-sound
  <'> sndclm-convolve2-test      :comment over object->string with-sound ws-close-sound
  <'> sndclm-granulate1-test     :comment over object->string with-sound ws-close-sound
  <'> sndclm-granulate2-test     :comment over object->string with-sound ws-close-sound
  <'> sndclm-phase-vocoder1-test :comment over object->string with-sound ws-close-sound
  <'> sndclm-phase-vocoder2-test :comment over object->string :srate 22050 with-sound ws-close-sound
  <'> sndclm-asymmetric-fm-test  :comment over object->string with-sound ws-close-sound
  <'> sndclm-file->frame->file-test :comment over object->string :channels 2 with-sound ws-close-sound
  <'> sndclm-readin-test         :comment over object->string with-sound ws-close-sound
  <'> sndclm-in-out-any-test     :comment over object->string with-sound ws-close-sound
  <'> sndclm-locsig-test :comment over object->string :channels 2 with-sound ws-close-sound
  <'> sndclm-amplitude-modulate-test :comment over object->string with-sound ws-close-sound
  old-play  to *clm-play*
  old-stats to *clm-statistics*
;

\ ====== test 26: Gtk
'snd-gtk provided? [if]
  #( <'> Fg_free <'> Fg_signal_lookup <'> Fg_source_remove <'> Fg_type_from_name
     <'> Fg_type_is_a <'> Fg_type_name <'> Fg_type_parent <'> Fg_type_qname <'> Fgdk_atom_intern
     <'> Fgdk_init <'> Fgdk_init_check <'> Fgdk_set_show_events <'> Fgdk_threads_enter
     <'> Fgdk_threads_init <'> Fgdk_threads_leave <'> Fgdk_utf8_to_string_target
     <'> Fgtk_accel_map_load <'> Fgtk_accel_map_load_fd <'> Fgtk_accel_map_lookup_entry
     <'> Fgtk_accel_map_save <'> Fgtk_accel_map_save_fd <'> Fgtk_button_new_with_label
     <'> Fgtk_check_button_new_with_label <'> Fgtk_check_menu_item_new_with_label
     <'> Fgtk_color_selection_palette_from_string <'> Fgtk_disable_setlocale
     <'> Fgtk_icon_size_from_name <'> Fgtk_image_menu_item_new_with_label <'> Fgtk_init
     <'> Fgtk_init_check <'> Fgtk_key_snooper_install <'> Fgtk_key_snooper_remove
     <'> Fgtk_main <'> Fgtk_main_do_event <'> Fgtk_main_iteration <'> Fgtk_main_iteration_do
     <'> Fgtk_main_level <'> Fgtk_main_quit <'> Fgtk_menu_item_new_with_label
     <'> Fgtk_radio_button_new_with_label <'> Fgtk_radio_menu_item_new_with_label
     <'> Fgtk_rc_find_module_in_path <'> Fgtk_toggle_button_new_with_label
     <'> Fpango_coverage_from_bytes <'> Fpango_find_paragraph_boundary
     <'> Fpango_language_from_string <'> Fpango_script_iter_new ) constant breakable-gtk-procs

  #( <'> FGDK_DEVICE <'> FGDK_DISPLAY_OBJECT <'> FGDK_DRAG_CONTEXT
     <'> FGDK_DRAWABLE <'> FGDK_EVENT_ANY <'> FGDK_EVENT_BUTTON
     <'> FGDK_EVENT_CONFIGURE <'> FGDK_EVENT_CROSSING <'> FGDK_EVENT_DND
     <'> FGDK_EVENT_EXPOSE <'> FGDK_EVENT_FOCUS <'> FGDK_EVENT_KEY
     <'> FGDK_EVENT_MOTION <'> FGDK_EVENT_NOEXPOSE <'> FGDK_EVENT_PROPERTY
     <'> FGDK_EVENT_PROXIMITY <'> FGDK_EVENT_SCROLL <'> FGDK_EVENT_SELECTION
     <'> FGDK_EVENT_SETTING <'> FGDK_EVENT_VISIBILITY <'> FGDK_EVENT_WINDOWSTATE
     <'> FGDK_IS_DEVICE <'> FGDK_IS_DISPLAY <'> FGDK_IS_DRAG_CONTEXT
     <'> FGDK_IS_DRAWABLE <'> FGDK_IS_KEYMAP <'> FGDK_IS_SCREEN
     <'> FGDK_IS_VISUAL <'> FGDK_IS_WINDOW <'> FGDK_KEYMAP <'> FGDK_SCREEN
     <'> FGDK_VISUAL <'> FGDK_WINDOW <'> FGPOINTER <'> FGTK_ABOUT_DIALOG
     <'> FGTK_ACCEL_GROUP <'> FGTK_ACCEL_LABEL <'> FGTK_ACCEL_MAP
     <'> FGTK_ACCESSIBLE <'> FGTK_ACTION <'> FGTK_ACTION_GROUP <'> FGTK_ADJUSTMENT
     <'> FGTK_ALIGNMENT <'> FGTK_ARROW <'> FGTK_ASPECT_FRAME <'> FGTK_BIN
     <'> FGTK_BOX <'> FGTK_BUTTON <'> FGTK_BUTTON_BOX <'> FGTK_CALENDAR
     <'> FGTK_CELL_EDITABLE <'> FGTK_CELL_LAYOUT <'> FGTK_CELL_RENDERER
     <'> FGTK_CELL_RENDERER_COMBO <'> FGTK_CELL_RENDERER_PIXBUF
     <'> FGTK_CELL_RENDERER_PROGRESS <'> FGTK_CELL_RENDERER_TEXT
     <'> FGTK_CELL_RENDERER_TOGGLE <'> FGTK_CELL_VIEW <'> FGTK_CHECK_BUTTON
     <'> FGTK_CHECK_MENU_ITEM <'> FGTK_CLIPBOARD <'> FGTK_COLOR_BUTTON
     <'> FGTK_COLOR_SELECTION <'> FGTK_COLOR_SELECTION_DIALOG <'> FGTK_COMBO_BOX
     <'> FGTK_CONTAINER <'> FGTK_DIALOG
     <'> FGTK_DRAWING_AREA <'> FGTK_EDITABLE <'> FGTK_ENTRY
     <'> FGTK_ENTRY_COMPLETION <'> FGTK_EVENT_BOX <'> FGTK_EXPANDER
     <'> FGTK_FILE_CHOOSER <'> FGTK_FILE_CHOOSER_BUTTON
     <'> FGTK_FILE_CHOOSER_DIALOG <'> FGTK_FILE_CHOOSER_WIDGET
     <'> FGTK_FILE_FILTER <'> FGTK_FIXED <'> FGTK_FONT_BUTTON
     <'> FGTK_FONT_SELECTION <'> FGTK_FONT_SELECTION_DIALOG
     <'> FGTK_FRAME <'> FGTK_HANDLE_BOX <'> FGTK_HBOX <'> FGTK_HBUTTON_BOX
     <'> FGTK_HPANED <'> FGTK_HRULER <'> FGTK_HSCALE <'> FGTK_HSCROLLBAR
     <'> FGTK_HSEPARATOR <'> FGTK_ICON_FACTORY <'> FGTK_ICON_THEME
     <'> FGTK_ICON_VIEW <'> FGTK_IMAGE <'> FGTK_IMAGE_MENU_ITEM
     <'> FGTK_IM_CONTEXT <'> FGTK_IM_CONTEXT_SIMPLE <'> FGTK_IM_MULTICONTEXT
     <'> FGTK_INVISIBLE <'> FGTK_IS_ABOUT_DIALOG <'> FGTK_IS_ACCEL_GROUP
     <'> FGTK_IS_ACCEL_LABEL <'> FGTK_IS_ACCEL_MAP <'> FGTK_IS_ACCESSIBLE
     <'> FGTK_IS_ACTION <'> FGTK_IS_ACTION_GROUP <'> FGTK_IS_ADJUSTMENT
     <'> FGTK_IS_ALIGNMENT <'> FGTK_IS_ARROW <'> FGTK_IS_ASPECT_FRAME
     <'> FGTK_IS_BIN <'> FGTK_IS_BOX <'> FGTK_IS_BUTTON <'> FGTK_IS_BUTTON_BOX
     <'> FGTK_IS_CALENDAR <'> FGTK_IS_CELL_EDITABLE <'> FGTK_IS_CELL_LAYOUT
     <'> FGTK_IS_CELL_RENDERER <'> FGTK_IS_CELL_RENDERER_COMBO
     <'> FGTK_IS_CELL_RENDERER_PIXBUF <'> FGTK_IS_CELL_RENDERER_PROGRESS
     <'> FGTK_IS_CELL_RENDERER_TEXT <'> FGTK_IS_CELL_RENDERER_TOGGLE
     <'> FGTK_IS_CELL_VIEW <'> FGTK_IS_CHECK_BUTTON
     <'> FGTK_IS_CHECK_MENU_ITEM <'> FGTK_IS_CLIPBOARD
     <'> FGTK_IS_COLOR_BUTTON <'> FGTK_IS_COLOR_SELECTION
     <'> FGTK_IS_COLOR_SELECTION_DIALOG <'> FGTK_IS_COMBO_BOX
     <'> FGTK_IS_CONTAINER
     <'> FGTK_IS_DIALOG <'> FGTK_IS_DRAWING_AREA <'> FGTK_IS_EDITABLE
     <'> FGTK_IS_ENTRY <'> FGTK_IS_ENTRY_COMPLETION <'> FGTK_IS_EVENT_BOX
     <'> FGTK_IS_EXPANDER <'> FGTK_IS_FILE_CHOOSER <'> FGTK_IS_FILE_CHOOSER_BUTTON
     <'> FGTK_IS_FILE_CHOOSER_DIALOG <'> FGTK_IS_FILE_CHOOSER_WIDGET
     <'> FGTK_IS_FILE_FILTER <'> FGTK_IS_FIXED <'> FGTK_IS_FONT_BUTTON
     <'> FGTK_IS_FONT_SELECTION <'> FGTK_IS_FONT_SELECTION_DIALOG
     <'> FGTK_IS_FRAME <'> FGTK_IS_HANDLE_BOX <'> FGTK_IS_HBOX
     <'> FGTK_IS_HBUTTON_BOX <'> FGTK_IS_HPANED <'> FGTK_IS_HRULER
     <'> FGTK_IS_HSCALE <'> FGTK_IS_HSCROLLBAR <'> FGTK_IS_HSEPARATOR
     <'> FGTK_IS_ICON_FACTORY <'> FGTK_IS_ICON_THEME <'> FGTK_IS_ICON_VIEW
     <'> FGTK_IS_IMAGE <'> FGTK_IS_IMAGE_MENU_ITEM <'> FGTK_IS_IM_CONTEXT
     <'> FGTK_IS_IM_CONTEXT_SIMPLE <'> FGTK_IS_IM_MULTICONTEXT
     <'> FGTK_IS_INVISIBLE <'> FGTK_IS_LABEL <'> FGTK_IS_LAYOUT
     <'> FGTK_IS_LIST_STORE <'> FGTK_IS_MENU <'> FGTK_IS_MENU_BAR
     <'> FGTK_IS_MENU_ITEM <'> FGTK_IS_MENU_SHELL <'> FGTK_IS_MENU_TOOL_BUTTON
     <'> FGTK_IS_MISC <'> FGTK_IS_NOTEBOOK <'> FGTK_IS_OBJECT <'> FGTK_IS_PANED
     <'> FGTK_IS_PLUG <'> FGTK_IS_PROGRESS_BAR <'> FGTK_IS_RADIO_ACTION
     <'> FGTK_IS_RADIO_BUTTON <'> FGTK_IS_RADIO_MENU_ITEM
     <'> FGTK_IS_RADIO_TOOL_BUTTON <'> FGTK_IS_RANGE <'> FGTK_IS_RC_STYLE
     <'> FGTK_IS_RULER <'> FGTK_IS_SCALE <'> FGTK_IS_SCROLLBAR
     <'> FGTK_IS_SCROLLED_WINDOW <'> FGTK_IS_SEPARATOR
     <'> FGTK_IS_SEPARATOR_MENU_ITEM <'> FGTK_IS_SEPARATOR_TOOL_ITEM
     <'> FGTK_IS_SIZE_GROUP <'> FGTK_IS_SOCKET <'> FGTK_IS_SPIN_BUTTON
     <'> FGTK_IS_STATUSBAR <'> FGTK_IS_STYLE <'> FGTK_IS_TABLE
     <'> FGTK_IS_TEAROFF_MENU_ITEM <'> FGTK_IS_TEXT_BUFFER
     <'> FGTK_IS_TEXT_CHILD_ANCHOR <'> FGTK_IS_TEXT_MARK <'> FGTK_IS_TEXT_TAG
     <'> FGTK_IS_TEXT_TAG_TABLE <'> FGTK_IS_TEXT_VIEW
     <'> FGTK_IS_TOGGLE_ACTION <'> FGTK_IS_TOGGLE_BUTTON
     <'> FGTK_IS_TOGGLE_TOOL_BUTTON <'> FGTK_IS_TOOLBAR <'> FGTK_IS_TOOL_BUTTON
     <'> FGTK_IS_TOOL_ITEM <'> FGTK_IS_TREE_DRAG_DEST
     <'> FGTK_IS_TREE_DRAG_SOURCE <'> FGTK_IS_TREE_MODEL
     <'> FGTK_IS_TREE_MODEL_FILTER <'> FGTK_IS_TREE_MODEL_SORT
     <'> FGTK_IS_TREE_SELECTION <'> FGTK_IS_TREE_SORTABLE
     <'> FGTK_IS_TREE_STORE <'> FGTK_IS_TREE_VIEW
     <'> FGTK_IS_TREE_VIEW_COLUMN <'> FGTK_IS_UI_MANAGER <'> FGTK_IS_VBOX
     <'> FGTK_IS_VBUTTON_BOX <'> FGTK_IS_VIEWPORT <'> FGTK_IS_VPANED
     <'> FGTK_IS_VRULER <'> FGTK_IS_VSCALE <'> FGTK_IS_VSCROLLBAR
     <'> FGTK_IS_VSEPARATOR <'> FGTK_IS_WIDGET <'> FGTK_IS_WINDOW <'> FGTK_LABEL
     <'> FGTK_LAYOUT <'> FGTK_LIST_STORE <'> FGTK_MENU <'> FGTK_MENU_BAR
     <'> FGTK_MENU_ITEM <'> FGTK_MENU_SHELL <'> FGTK_MENU_TOOL_BUTTON
     <'> FGTK_MISC <'> FGTK_NOTEBOOK <'> FGTK_PANED <'> FGTK_PLUG
     <'> FGTK_PROGRESS_BAR <'> FGTK_RADIO_ACTION <'> FGTK_RADIO_BUTTON
     <'> FGTK_RADIO_MENU_ITEM <'> FGTK_RADIO_TOOL_BUTTON
     <'> FGTK_RANGE <'> FGTK_RULER <'> FGTK_SCALE <'> FGTK_SCROLLBAR
     <'> FGTK_SCROLLED_WINDOW <'> FGTK_SEPARATOR <'> FGTK_SEPARATOR_MENU_ITEM
     <'> FGTK_SEPARATOR_TOOL_ITEM <'> FGTK_SIZE_GROUP <'> FGTK_SOCKET
     <'> FGTK_SPIN_BUTTON <'> FGTK_STATUSBAR <'> FGTK_STYLE <'> FGTK_TABLE
     <'> FGTK_TEAROFF_MENU_ITEM <'> FGTK_TEXT_BUFFER <'> FGTK_TEXT_CHILD_ANCHOR
     <'> FGTK_TEXT_MARK <'> FGTK_TEXT_TAG <'> FGTK_TEXT_TAG_TABLE
     <'> FGTK_TEXT_VIEW <'> FGTK_TOGGLE_ACTION <'> FGTK_TOGGLE_BUTTON
     <'> FGTK_TOGGLE_TOOL_BUTTON <'> FGTK_TOOLBAR <'> FGTK_TOOL_BUTTON
     <'> FGTK_TOOL_ITEM <'> FGTK_TREE_DRAG_DEST <'> FGTK_TREE_DRAG_SOURCE
     <'> FGTK_TREE_MODEL <'> FGTK_TREE_MODEL_FILTER
     <'> FGTK_TREE_MODEL_SORT <'> FGTK_TREE_SELECTION
     <'> FGTK_TREE_SORTABLE <'> FGTK_TREE_STORE <'> FGTK_TREE_VIEW
     <'> FGTK_TREE_VIEW_COLUMN <'> FGTK_UI_MANAGER <'> FGTK_VBOX
     <'> FGTK_VBUTTON_BOX <'> FGTK_VIEWPORT <'> FGTK_VPANED <'> FGTK_VRULER
     <'> FGTK_VSCALE <'> FGTK_VSCROLLBAR <'> FGTK_VSEPARATOR <'> FGTK_WIDGET
     <'> FG_IS_OBJECT <'> FG_OBJECT <'> FPANGO_CONTEXT <'> FPANGO_FONT
     <'> FPANGO_FONT_FACE <'> FPANGO_FONT_FAMILY <'> FPANGO_FONT_MAP
     <'> FPANGO_IS_CONTEXT <'> FPANGO_IS_FONT <'> FPANGO_IS_FONT_FACE
     <'> FPANGO_IS_FONT_FAMILY <'> FPANGO_IS_FONT_MAP <'> FPANGO_IS_LAYOUT
     <'> FPANGO_LAYOUT <'> Fg_cclosure_new <'> Fg_idle_add
     <'> Fg_idle_add_full <'> Fg_idle_remove_by_data <'> Fg_list_copy
     <'> Fg_list_first <'> Fg_list_free <'> Fg_list_last <'> Fg_list_length
     <'> Fg_list_nth_data <'> Fg_list_remove_link <'> Fg_list_reverse
     <'> Fg_object_get_data <'> Fg_object_ref <'> Fg_object_set_data
     <'> Fg_object_unref <'> Fg_quark_from_string <'> Fg_quark_to_string
     <'> Fg_signal_add_emission_hook <'> Fg_signal_connect_closure
     <'> Fg_signal_connect_closure_by_id <'> Fg_signal_connect_data
     <'> Fg_signal_get_invocation_hint <'> Fg_signal_handler_block
     <'> Fg_signal_handler_disconnect <'> Fg_signal_handler_find
     <'> Fg_signal_handler_is_connected <'> Fg_signal_handler_unblock
     <'> Fg_signal_handlers_block_matched <'> Fg_signal_handlers_destroy
     <'> Fg_signal_handlers_disconnect_matched
     <'> Fg_signal_handlers_unblock_matched <'> Fg_signal_has_handler_pending
     <'> Fg_signal_list_ids <'> Fg_signal_name <'> Fg_signal_newv
     <'> Fg_signal_parse_name <'> Fg_signal_query
     <'> Fg_signal_remove_emission_hook <'> Fg_signal_stop_emission
     <'> Fg_signal_stop_emission_by_name <'> Fg_timeout_add
     <'> Fg_timeout_add_full <'> Fgdk_add_client_message_filter
     <'> Fgdk_atom_name <'> Fgdk_beep <'> Fgdk_color_copy <'> Fgdk_color_equal
     <'> Fgdk_color_free <'> Fgdk_color_hash <'> Fgdk_color_parse
     <'> Fgdk_display_add_client_message_filter <'> Fgdk_display_beep
     <'> Fgdk_display_close <'> Fgdk_display_flush <'> Fgdk_display_get_default
     <'> Fgdk_display_get_default_cursor_size <'> Fgdk_display_get_default_group
     <'> Fgdk_display_get_default_screen <'> Fgdk_display_get_event
     <'> Fgdk_display_get_maximal_cursor_size <'> Fgdk_display_get_n_screens
     <'> Fgdk_display_get_name <'> Fgdk_display_get_pointer
     <'> Fgdk_display_get_screen <'> Fgdk_display_get_window_at_pointer
     <'> Fgdk_display_keyboard_ungrab <'> Fgdk_display_open
     <'> Fgdk_display_peek_event <'> Fgdk_display_pointer_is_grabbed
     <'> Fgdk_display_pointer_ungrab <'> Fgdk_display_put_event
     <'> Fgdk_display_set_double_click_distance <'> Fgdk_display_set_double_click_time
     <'> Fgdk_display_supports_clipboard_persistence <'> Fgdk_display_supports_cursor_alpha
     <'> Fgdk_display_supports_cursor_color <'> Fgdk_display_sync
     <'> Fgdk_drag_abort <'> Fgdk_drag_begin <'> Fgdk_drag_context_new
     <'> Fgdk_drag_drop <'> Fgdk_drag_drop_succeeded <'> Fgdk_drag_find_window
     <'> Fgdk_drag_get_protocol <'> Fgdk_drag_get_selection <'> Fgdk_drag_motion
     <'> Fgdk_drag_status <'> Fgdk_drop_finish <'> Fgdk_drop_reply
     <'> Fgdk_error_trap_pop <'> Fgdk_error_trap_push <'> Fgdk_event_copy
     <'> Fgdk_event_free <'> Fgdk_event_get <'> Fgdk_event_get_coords
     <'> Fgdk_event_get_root_coords <'> Fgdk_event_get_state
     <'> Fgdk_event_get_time <'> Fgdk_event_handler_set <'> Fgdk_event_peek
     <'> Fgdk_event_put <'> Fgdk_event_send_client_message
     <'> Fgdk_event_send_clientmessage_toall <'> Fgdk_events_pending
     <'> Fgdk_flush <'> Fgdk_get_default_root_window <'> Fgdk_get_display
     <'> Fgdk_get_display_arg_name <'> Fgdk_get_program_class
     <'> Fgdk_get_show_events <'> Fgdk_keyboard_grab <'> Fgdk_keyboard_ungrab
     <'> Fgdk_keymap_get_default <'> Fgdk_keymap_get_direction
     <'> Fgdk_keymap_get_entries_for_keycode <'> Fgdk_keymap_get_entries_for_keyval
     <'> Fgdk_keymap_lookup_key <'> Fgdk_keyval_convert_case
     <'> Fgdk_keyval_from_name <'> Fgdk_keyval_is_lower <'> Fgdk_keyval_is_upper
     <'> Fgdk_keyval_name <'> Fgdk_keyval_to_lower <'> Fgdk_keyval_to_unicode
     <'> Fgdk_keyval_to_upper <'> Fgdk_list_visuals
     <'> Fgdk_notify_startup_complete <'> Fgdk_pango_context_get
     <'> Fgdk_pixbuf_add_alpha <'> Fgdk_pixbuf_animation_get_height
     <'> Fgdk_pixbuf_animation_get_iter <'> Fgdk_pixbuf_animation_get_static_image
     <'> Fgdk_pixbuf_animation_get_width <'> Fgdk_pixbuf_animation_is_static_image
     <'> Fgdk_pixbuf_animation_iter_advance <'> Fgdk_pixbuf_animation_iter_get_delay_time
     <'> Fgdk_pixbuf_animation_iter_get_pixbuf
     <'> Fgdk_pixbuf_animation_iter_on_currently_loading_frame
     <'> Fgdk_pixbuf_animation_new_from_file <'> Fgdk_pixbuf_composite
     <'> Fgdk_pixbuf_composite_color <'> Fgdk_pixbuf_composite_color_simple
     <'> Fgdk_pixbuf_copy <'> Fgdk_pixbuf_copy_area <'> Fgdk_pixbuf_error_quark
     <'> Fgdk_pixbuf_fill <'> Fgdk_pixbuf_get_bits_per_sample
     <'> Fgdk_pixbuf_get_colorspace <'> Fgdk_pixbuf_get_has_alpha
     <'> Fgdk_pixbuf_get_height <'> Fgdk_pixbuf_get_n_channels
     <'> Fgdk_pixbuf_get_option <'> Fgdk_pixbuf_get_pixels
     <'> Fgdk_pixbuf_get_rowstride <'> Fgdk_pixbuf_get_width
     <'> Fgdk_pixbuf_new_from_data <'> Fgdk_pixbuf_new_from_file
     <'> Fgdk_pixbuf_new_from_inline <'> Fgdk_pixbuf_new_from_xpm_data
     <'> Fgdk_pixbuf_new_subpixbuf <'> Fgdk_pixbuf_saturate_and_pixelate
     <'> Fgdk_pixbuf_savev <'> Fgdk_pixbuf_scale <'> Fgdk_pixbuf_scale_simple
     <'> Fgdk_pointer_grab <'> Fgdk_pointer_is_grabbed <'> Fgdk_pointer_ungrab
     <'> Fgdk_property_change <'> Fgdk_property_delete <'> Fgdk_property_get
     <'> Fgdk_query_depths <'> Fgdk_query_visual_types
     <'> Fgdk_rectangle_intersect <'> Fgdk_rectangle_union
     <'> Fgdk_screen_broadcast_client_message <'> Fgdk_screen_get_default
     <'> Fgdk_screen_get_display <'> Fgdk_screen_get_height
     <'> Fgdk_screen_get_height_mm <'> Fgdk_screen_get_monitor_at_point
     <'> Fgdk_screen_get_monitor_at_window <'> Fgdk_screen_get_monitor_geometry
     <'> Fgdk_screen_get_n_monitors <'> Fgdk_screen_get_number
     <'> Fgdk_screen_get_root_window <'> Fgdk_screen_get_system_visual
     <'> Fgdk_screen_get_toplevel_windows <'> Fgdk_screen_get_width
     <'> Fgdk_screen_get_width_mm <'> Fgdk_screen_height
     <'> Fgdk_screen_height_mm <'> Fgdk_screen_list_visuals
     <'> Fgdk_screen_make_display_name <'> Fgdk_screen_width
     <'> Fgdk_screen_width_mm <'> Fgdk_selection_convert
     <'> Fgdk_selection_owner_get <'> Fgdk_selection_owner_set
     <'> Fgdk_selection_property_get <'> Fgdk_selection_send_notify
     <'> Fgdk_set_double_click_time <'> Fgdk_set_locale
     <'> Fgdk_set_program_class <'> Fgdk_set_sm_client_id
     <'> Fgdk_unicode_to_keyval <'> Fgdk_visual_get_best
     <'> Fgdk_visual_get_best_depth <'> Fgdk_visual_get_best_type
     <'> Fgdk_visual_get_best_with_both <'> Fgdk_visual_get_best_with_depth
     <'> Fgdk_visual_get_best_with_type <'> Fgdk_visual_get_system
     <'> Fgdk_window_add_filter <'> Fgdk_window_at_pointer
     <'> Fgdk_window_begin_move_drag <'> Fgdk_window_begin_paint_rect
     <'> Fgdk_window_begin_resize_drag <'> Fgdk_window_configure_finished
     <'> Fgdk_window_constrain_size <'> Fgdk_window_deiconify
     <'> Fgdk_window_destroy <'> Fgdk_window_enable_synchronized_configure
     <'> Fgdk_window_end_paint <'> Fgdk_window_focus
     <'> Fgdk_window_foreign_new <'> Fgdk_window_freeze_updates
     <'> Fgdk_window_get_children <'> Fgdk_window_get_decorations
     <'> Fgdk_window_get_events <'> Fgdk_window_get_frame_extents
     <'> Fgdk_window_get_geometry <'> Fgdk_window_get_group
     <'> Fgdk_window_get_origin <'> Fgdk_window_get_parent
     <'> Fgdk_window_get_pointer <'> Fgdk_window_get_position
     <'> Fgdk_window_get_root_origin <'> Fgdk_window_get_state
     <'> Fgdk_window_get_toplevel <'> Fgdk_window_get_user_data
     <'> Fgdk_window_get_window_type <'> Fgdk_window_hide
     <'> Fgdk_window_iconify <'> Fgdk_window_invalidate_rect
     <'> Fgdk_window_is_viewable <'> Fgdk_window_is_visible
     <'> Fgdk_window_lookup <'> Fgdk_window_lower
     <'> Fgdk_window_maximize <'> Fgdk_window_merge_child_shapes
     <'> Fgdk_window_move <'> Fgdk_window_move_resize <'> Fgdk_window_new
     <'> Fgdk_window_peek_children <'> Fgdk_window_process_all_updates
     <'> Fgdk_window_process_updates <'> Fgdk_window_raise
     <'> Fgdk_window_register_dnd <'> Fgdk_window_remove_filter
     <'> Fgdk_window_reparent <'> Fgdk_window_resize <'> Fgdk_window_scroll
     <'> Fgdk_window_set_background <'> Fgdk_window_set_child_shapes
     <'> Fgdk_window_set_cursor <'> Fgdk_window_set_debug_updates
     <'> Fgdk_window_set_decorations <'> Fgdk_window_set_events
     <'> Fgdk_window_set_functions <'> Fgdk_window_set_geometry_hints
     <'> Fgdk_window_set_group <'> Fgdk_window_set_icon_list
     <'> Fgdk_window_set_icon_name <'> Fgdk_window_set_keep_above
     <'> Fgdk_window_set_keep_below <'> Fgdk_window_set_modal_hint
     <'> Fgdk_window_set_override_redirect <'> Fgdk_window_set_role
     <'> Fgdk_window_set_static_gravities <'> Fgdk_window_set_title
     <'> Fgdk_window_set_transient_for <'> Fgdk_window_set_type_hint
     <'> Fgdk_window_set_user_data <'> Fgdk_window_show
     <'> Fgdk_window_show_unraised <'> Fgdk_window_stick
     <'> Fgdk_window_thaw_updates <'> Fgdk_window_unmaximize
     <'> Fgdk_window_unstick <'> Fgdk_window_withdraw
     <'> Fgtk_about_dialog_get_artists <'> Fgtk_about_dialog_get_authors
     <'> Fgtk_about_dialog_get_comments <'> Fgtk_about_dialog_get_copyright
     <'> Fgtk_about_dialog_get_documenters <'> Fgtk_about_dialog_get_license
     <'> Fgtk_about_dialog_get_logo <'> Fgtk_about_dialog_get_logo_icon_name
     <'> Fgtk_about_dialog_get_translator_credits
     <'> Fgtk_about_dialog_get_version <'> Fgtk_about_dialog_get_website
     <'> Fgtk_about_dialog_get_website_label <'> Fgtk_about_dialog_new
     <'> Fgtk_about_dialog_set_artists <'> Fgtk_about_dialog_set_authors
     <'> Fgtk_about_dialog_set_comments <'> Fgtk_about_dialog_set_copyright
     <'> Fgtk_about_dialog_set_documenters <'> Fgtk_about_dialog_set_license
     <'> Fgtk_about_dialog_set_logo <'> Fgtk_about_dialog_set_logo_icon_name
     <'> Fgtk_about_dialog_set_translator_credits
     <'> Fgtk_about_dialog_set_version <'> Fgtk_about_dialog_set_website
     <'> Fgtk_about_dialog_set_website_label
     <'> Fgtk_accel_group_activate <'> Fgtk_accel_group_connect
     <'> Fgtk_accel_group_connect_by_path <'> Fgtk_accel_group_disconnect
     <'> Fgtk_accel_group_disconnect_key <'> Fgtk_accel_group_find
     <'> Fgtk_accel_group_from_accel_closure <'> Fgtk_accel_group_lock
     <'> Fgtk_accel_group_new <'> Fgtk_accel_group_query
     <'> Fgtk_accel_group_unlock <'> Fgtk_accel_groups_activate
     <'> Fgtk_accel_groups_from_object <'> Fgtk_accel_label_get_accel_widget
     <'> Fgtk_accel_label_get_accel_width <'> Fgtk_accel_label_new
     <'> Fgtk_accel_label_refetch <'> Fgtk_accel_label_set_accel_closure
     <'> Fgtk_accel_label_set_accel_widget <'> Fgtk_accel_map_add_entry
     <'> Fgtk_accel_map_add_filter <'> Fgtk_accel_map_change_entry
     <'> Fgtk_accel_map_foreach <'> Fgtk_accel_map_foreach_unfiltered
     <'> Fgtk_accel_map_get
     <'> Fgtk_accelerator_get_label <'> Fgtk_accelerator_name
     <'> Fgtk_accelerator_parse <'> Fgtk_accelerator_set_default_mod_mask
     <'> Fgtk_accelerator_valid <'> Fgtk_accessible_connect_widget_destroyed
     <'> Fgtk_action_activate <'> Fgtk_action_connect_accelerator
     <'> Fgtk_action_create_icon <'> Fgtk_action_create_menu_item
     <'> Fgtk_action_create_tool_item <'> Fgtk_action_disconnect_accelerator
     <'> Fgtk_action_get_name <'> Fgtk_action_get_proxies
     <'> Fgtk_action_get_sensitive <'> Fgtk_action_get_visible
     <'> Fgtk_action_group_add_action <'> Fgtk_action_group_add_action_with_accel
     <'> Fgtk_action_group_add_actions <'> Fgtk_action_group_add_toggle_actions
     <'> Fgtk_action_group_add_toggle_actions_full
     <'> Fgtk_action_group_get_action <'> Fgtk_action_group_get_name
     <'> Fgtk_action_group_get_sensitive <'> Fgtk_action_group_get_visible
     <'> Fgtk_action_group_list_actions <'> Fgtk_action_group_new
     <'> Fgtk_action_group_remove_action <'> Fgtk_action_group_set_sensitive
     <'> Fgtk_action_group_set_translation_domain
     <'> Fgtk_action_group_set_visible <'> Fgtk_action_is_sensitive
     <'> Fgtk_action_is_visible <'> Fgtk_action_new <'> Fgtk_action_set_sensitive
     <'> Fgtk_action_set_visible <'> Fgtk_adjustment_changed
     <'> Fgtk_adjustment_clamp_page <'> Fgtk_adjustment_get_value
     <'> Fgtk_adjustment_new <'> Fgtk_adjustment_set_value
     <'> Fgtk_adjustment_value_changed <'> Fgtk_alignment_get_padding
     <'> Fgtk_alignment_new <'> Fgtk_alignment_set <'> Fgtk_alignment_set_padding
     <'> Fgtk_alternative_dialog_button_order <'> Fgtk_arrow_new
     <'> Fgtk_arrow_set <'> Fgtk_aspect_frame_new <'> Fgtk_aspect_frame_set
     <'> Fgtk_bin_get_child <'> Fgtk_binding_entry_remove
     <'> Fgtk_binding_set_add_path <'> Fgtk_binding_set_by_class
     <'> Fgtk_binding_set_find <'> Fgtk_binding_set_new <'> Fgtk_border_copy
     <'> Fgtk_border_free <'> Fgtk_box_get_homogeneous <'> Fgtk_box_get_spacing
     <'> Fgtk_box_pack_end <'> Fgtk_box_pack_start
     <'> Fgtk_box_query_child_packing <'> Fgtk_box_reorder_child
     <'> Fgtk_box_set_child_packing <'> Fgtk_box_set_homogeneous
     <'> Fgtk_box_set_spacing <'> Fgtk_button_box_get_child_secondary
     <'> Fgtk_button_box_get_layout <'> Fgtk_button_box_set_child_secondary
     <'> Fgtk_button_box_set_layout <'> Fgtk_button_get_alignment
     <'> Fgtk_button_get_focus_on_click <'> Fgtk_button_get_image
     <'> Fgtk_button_get_label <'> Fgtk_button_get_relief
     <'> Fgtk_button_get_use_stock <'> Fgtk_button_get_use_underline
     <'> Fgtk_button_new <'> Fgtk_button_new_from_stock
     <'> Fgtk_button_new_with_mnemonic <'> Fgtk_button_set_alignment
     <'> Fgtk_button_set_focus_on_click <'> Fgtk_button_set_image
     <'> Fgtk_button_set_label <'> Fgtk_button_set_relief
     <'> Fgtk_button_set_use_stock <'> Fgtk_button_set_use_underline
     <'> Fgtk_calendar_clear_marks <'> Fgtk_calendar_get_date
     <'> Fgtk_calendar_get_display_options <'> Fgtk_cell_editable_editing_done
     <'> Fgtk_cell_editable_remove_widget <'> Fgtk_cell_editable_start_editing
     <'> Fgtk_cell_layout_add_attribute <'> Fgtk_cell_layout_clear
     <'> Fgtk_cell_layout_clear_attributes <'> Fgtk_cell_layout_pack_end
     <'> Fgtk_cell_layout_pack_start <'> Fgtk_cell_layout_reorder
     <'> Fgtk_cell_layout_set_attributes <'> Fgtk_cell_layout_set_cell_data_func
     <'> Fgtk_cell_renderer_activate <'> Fgtk_cell_renderer_combo_new
     <'> Fgtk_cell_renderer_get_fixed_size <'> Fgtk_cell_renderer_get_size
     <'> Fgtk_cell_renderer_pixbuf_new <'> Fgtk_cell_renderer_progress_new
     <'> Fgtk_cell_renderer_set_fixed_size <'> Fgtk_cell_renderer_start_editing
     <'> Fgtk_cell_renderer_text_new <'> Fgtk_cell_renderer_text_set_fixed_height_from_font
     <'> Fgtk_cell_renderer_toggle_get_active <'> Fgtk_cell_renderer_toggle_get_radio
     <'> Fgtk_cell_renderer_toggle_new <'> Fgtk_cell_renderer_toggle_set_active
     <'> Fgtk_cell_renderer_toggle_set_radio <'> Fgtk_cell_view_get_displayed_row
     <'> Fgtk_cell_view_new <'> Fgtk_cell_view_new_with_markup
     <'> Fgtk_cell_view_new_with_pixbuf <'> Fgtk_cell_view_new_with_text
     <'> Fgtk_cell_view_set_background_color <'> Fgtk_cell_view_set_displayed_row
     <'> Fgtk_cell_view_set_model <'> Fgtk_check_button_new
     <'> Fgtk_check_button_new_with_mnemonic <'> Fgtk_check_menu_item_get_active
     <'> Fgtk_check_menu_item_get_draw_as_radio <'> Fgtk_check_menu_item_get_inconsistent
     <'> Fgtk_check_menu_item_new <'> Fgtk_check_menu_item_new_with_mnemonic
     <'> Fgtk_check_menu_item_set_active <'> Fgtk_check_menu_item_set_draw_as_radio
     <'> Fgtk_check_menu_item_set_inconsistent <'> Fgtk_check_menu_item_toggled
     <'> Fgtk_check_version <'> Fgtk_clipboard_clear <'> Fgtk_clipboard_get
     <'> Fgtk_clipboard_get_display <'> Fgtk_clipboard_get_for_display
     <'> Fgtk_clipboard_get_owner <'> Fgtk_clipboard_request_contents
     <'> Fgtk_clipboard_request_image <'> Fgtk_clipboard_request_targets
     <'> Fgtk_clipboard_request_text <'> Fgtk_clipboard_set_can_store
     <'> Fgtk_clipboard_set_image <'> Fgtk_clipboard_set_text
     <'> Fgtk_clipboard_set_with_data <'> Fgtk_clipboard_store
     <'> Fgtk_clipboard_wait_for_contents <'> Fgtk_clipboard_wait_for_image
     <'> Fgtk_clipboard_wait_for_targets <'> Fgtk_clipboard_wait_for_text
     <'> Fgtk_clipboard_wait_is_image_available <'> Fgtk_clipboard_wait_is_target_available
     <'> Fgtk_clipboard_wait_is_text_available
     <'> Fgtk_color_button_get_alpha <'> Fgtk_color_button_get_color
     <'> Fgtk_color_button_get_title <'> Fgtk_color_button_get_use_alpha
     <'> Fgtk_color_button_new <'> Fgtk_color_button_new_with_color
     <'> Fgtk_color_button_set_alpha <'> Fgtk_color_button_set_color
     <'> Fgtk_color_button_set_title <'> Fgtk_color_button_set_use_alpha
     <'> Fgtk_color_selection_dialog_new <'> Fgtk_color_selection_get_current_alpha
     <'> Fgtk_color_selection_get_current_color <'> Fgtk_color_selection_get_has_opacity_control
     <'> Fgtk_color_selection_get_has_palette <'> Fgtk_color_selection_get_previous_alpha
     <'> Fgtk_color_selection_get_previous_color <'> Fgtk_color_selection_is_adjusting
     <'> Fgtk_color_selection_new <'> Fgtk_color_selection_palette_to_string
     <'> Fgtk_color_selection_set_current_alpha <'> Fgtk_color_selection_set_current_color
     <'> Fgtk_color_selection_set_has_opacity_control <'> Fgtk_color_selection_set_has_palette
     <'> Fgtk_color_selection_set_previous_alpha <'> Fgtk_color_selection_set_previous_color
     <'> Fgtk_combo_box_get_active <'> Fgtk_combo_box_get_active_iter
     <'> Fgtk_combo_box_get_add_tearoffs
     <'> Fgtk_combo_box_get_column_span_column <'> Fgtk_combo_box_get_focus_on_click
     <'> Fgtk_combo_box_get_model <'> Fgtk_combo_box_get_row_span_column
     <'> Fgtk_combo_box_get_wrap_width
     <'> Fgtk_combo_box_new
     <'> Fgtk_combo_box_new_with_model
     <'> Fgtk_combo_box_popdown <'> Fgtk_combo_box_popup
     <'> Fgtk_combo_box_set_active
     <'> Fgtk_combo_box_set_active_iter
     <'> Fgtk_combo_box_set_add_tearoffs <'> Fgtk_combo_box_set_column_span_column
     <'> Fgtk_combo_box_set_focus_on_click <'> Fgtk_combo_box_set_model
     <'> Fgtk_combo_box_set_row_separator_func <'> Fgtk_combo_box_set_row_span_column
     <'> Fgtk_combo_box_set_wrap_width <'> Fgtk_container_add <'> Fgtk_container_check_resize
     <'> Fgtk_container_foreach <'> Fgtk_container_get_border_width <'> Fgtk_container_get_children
     <'> Fgtk_container_get_resize_mode <'> Fgtk_container_remove
     <'> Fgtk_container_set_border_width
     <'> Fgtk_container_set_resize_mode <'> Fgtk_dialog_add_action_widget
     <'> Fgtk_dialog_add_button <'> Fgtk_dialog_add_buttons <'> Fgtk_dialog_new
     <'> Fgtk_dialog_new_with_buttons <'> Fgtk_dialog_response <'> Fgtk_dialog_run
     <'> Fgtk_dialog_set_alternative_button_order_from_array <'> Fgtk_dialog_set_default_response
     <'> Fgtk_dialog_set_response_sensitive <'> Fgtk_drag_begin <'> Fgtk_drag_check_threshold
     <'> Fgtk_drag_dest_add_image_targets <'> Fgtk_drag_dest_add_text_targets
     <'> Fgtk_drag_dest_add_uri_targets <'> Fgtk_drag_dest_find_target
     <'> Fgtk_drag_dest_get_target_list <'> Fgtk_drag_dest_set <'> Fgtk_drag_dest_set_proxy
     <'> Fgtk_drag_dest_set_target_list <'> Fgtk_drag_dest_unset <'> Fgtk_drag_finish
     <'> Fgtk_drag_get_data <'> Fgtk_drag_get_source_widget <'> Fgtk_drag_highlight
     <'> Fgtk_drag_set_icon_default <'> Fgtk_drag_set_icon_pixbuf <'> Fgtk_drag_set_icon_stock
     <'> Fgtk_drag_set_icon_widget <'> Fgtk_drag_source_add_image_targets
     <'> Fgtk_drag_source_add_text_targets <'> Fgtk_drag_source_add_uri_targets
     <'> Fgtk_drag_source_get_target_list <'> Fgtk_drag_source_set
     <'> Fgtk_drag_source_set_icon_pixbuf <'> Fgtk_drag_source_set_icon_stock
     <'> Fgtk_drag_source_set_target_list <'> Fgtk_drag_source_unset
     <'> Fgtk_drag_unhighlight <'> Fgtk_drawing_area_new <'> Fgtk_editable_copy_clipboard
     <'> Fgtk_editable_cut_clipboard <'> Fgtk_editable_delete_selection
     <'> Fgtk_editable_delete_text
     <'> Fgtk_editable_get_chars <'> Fgtk_editable_get_editable
     <'> Fgtk_editable_get_position <'> Fgtk_editable_get_selection_bounds
     <'> Fgtk_editable_insert_text <'> Fgtk_editable_paste_clipboard
     <'> Fgtk_editable_set_editable <'> Fgtk_editable_set_position
     <'> Fgtk_entry_completion_complete <'> Fgtk_entry_completion_delete_action
     <'> Fgtk_entry_completion_get_entry <'> Fgtk_entry_completion_get_inline_completion
     <'> Fgtk_entry_completion_get_minimum_key_length <'> Fgtk_entry_completion_get_model
     <'> Fgtk_entry_completion_get_popup_completion <'> Fgtk_entry_completion_get_text_column
     <'> Fgtk_entry_completion_insert_action_markup <'> Fgtk_entry_completion_insert_action_text
     <'> Fgtk_entry_completion_insert_prefix <'> Fgtk_entry_completion_new
     <'> Fgtk_entry_completion_set_inline_completion <'> Fgtk_entry_completion_set_match_func
     <'> Fgtk_entry_completion_set_minimum_key_length <'> Fgtk_entry_completion_set_model
     <'> Fgtk_entry_completion_set_popup_completion <'> Fgtk_entry_completion_set_text_column
     <'> Fgtk_entry_get_activates_default <'> Fgtk_entry_get_alignment
     <'> Fgtk_entry_get_completion
     <'> Fgtk_entry_get_has_frame <'> Fgtk_entry_get_invisible_char <'> Fgtk_entry_get_layout
     <'> Fgtk_entry_get_max_length <'> Fgtk_entry_get_text <'> Fgtk_entry_get_visibility
     <'> Fgtk_entry_get_width_chars <'> Fgtk_entry_layout_index_to_text_index <'> Fgtk_entry_new
     <'> Fgtk_entry_set_activates_default <'> Fgtk_entry_set_alignment
     <'> Fgtk_entry_set_completion <'> Fgtk_entry_set_has_frame <'> Fgtk_entry_set_invisible_char
     <'> Fgtk_entry_set_max_length <'> Fgtk_entry_set_text <'> Fgtk_entry_set_visibility
     <'> Fgtk_entry_set_width_chars <'> Fgtk_entry_text_index_to_layout_index
     <'> Fgtk_event_box_get_above_child <'> Fgtk_event_box_get_visible_window
     <'> Fgtk_event_box_new <'> Fgtk_event_box_set_above_child
     <'> Fgtk_event_box_set_visible_window <'> Fgtk_events_pending <'> Fgtk_expander_get_expanded
     <'> Fgtk_expander_get_label <'> Fgtk_expander_get_label_widget
     <'> Fgtk_expander_get_spacing <'> Fgtk_expander_get_use_markup
     <'> Fgtk_expander_get_use_underline <'> Fgtk_expander_new <'> Fgtk_expander_new_with_mnemonic
     <'> Fgtk_expander_set_expanded <'> Fgtk_expander_set_label <'> Fgtk_expander_set_label_widget
     <'> Fgtk_expander_set_spacing <'> Fgtk_expander_set_use_markup
     <'> Fgtk_expander_set_use_underline <'> Fgtk_false ) constant gtk-procs-1

  #( <'> Fgtk_file_chooser_add_filter <'> Fgtk_file_chooser_add_shortcut_folder
     <'> Fgtk_file_chooser_add_shortcut_folder_uri <'> Fgtk_file_chooser_button_get_title
     <'> Fgtk_file_chooser_button_get_width_chars <'> Fgtk_file_chooser_button_set_title
     <'> Fgtk_file_chooser_button_set_width_chars <'> Fgtk_file_chooser_dialog_new
     <'> Fgtk_file_chooser_get_action <'> Fgtk_file_chooser_get_current_folder
     <'> Fgtk_file_chooser_get_current_folder_uri <'> Fgtk_file_chooser_get_extra_widget
     <'> Fgtk_file_chooser_get_filename <'> Fgtk_file_chooser_get_filenames
     <'> Fgtk_file_chooser_get_filter <'> Fgtk_file_chooser_get_local_only
     <'> Fgtk_file_chooser_get_preview_filename <'> Fgtk_file_chooser_get_preview_uri
     <'> Fgtk_file_chooser_get_preview_widget <'> Fgtk_file_chooser_get_preview_widget_active
     <'> Fgtk_file_chooser_get_select_multiple <'> Fgtk_file_chooser_get_show_hidden 
     <'> Fgtk_file_chooser_get_uri <'> Fgtk_file_chooser_get_uris 
     <'> Fgtk_file_chooser_get_use_preview_label <'> Fgtk_file_chooser_list_filters
     <'> Fgtk_file_chooser_list_shortcut_folder_uris <'> Fgtk_file_chooser_list_shortcut_folders
     <'> Fgtk_file_chooser_remove_filter <'> Fgtk_file_chooser_remove_shortcut_folder
     <'> Fgtk_file_chooser_remove_shortcut_folder_uri <'> Fgtk_file_chooser_select_all 
     <'> Fgtk_file_chooser_select_filename <'> Fgtk_file_chooser_select_uri 
     <'> Fgtk_file_chooser_set_action <'> Fgtk_file_chooser_set_current_folder
     <'> Fgtk_file_chooser_set_current_folder_uri <'> Fgtk_file_chooser_set_current_name 
     <'> Fgtk_file_chooser_set_extra_widget <'> Fgtk_file_chooser_set_filename 
     <'> Fgtk_file_chooser_set_filter <'> Fgtk_file_chooser_set_local_only 
     <'> Fgtk_file_chooser_set_preview_widget <'> Fgtk_file_chooser_set_preview_widget_active
     <'> Fgtk_file_chooser_set_select_multiple <'> Fgtk_file_chooser_set_show_hidden 
     <'> Fgtk_file_chooser_set_uri <'> Fgtk_file_chooser_set_use_preview_label
     <'> Fgtk_file_chooser_unselect_all <'> Fgtk_file_chooser_unselect_filename
     <'> Fgtk_file_chooser_unselect_uri <'> Fgtk_file_filter_add_pattern
     <'> Fgtk_file_filter_add_pixbuf_formats <'> Fgtk_file_filter_filter
     <'> Fgtk_file_filter_get_name <'> Fgtk_file_filter_get_needed
     <'> Fgtk_file_filter_new <'> Fgtk_file_filter_set_name
     <'> Fgtk_fixed_move <'> Fgtk_fixed_new <'> Fgtk_fixed_put
     <'> Fgtk_font_button_get_font_name <'> Fgtk_font_button_get_show_size
     <'> Fgtk_font_button_get_show_style <'> Fgtk_font_button_get_title
     <'> Fgtk_font_button_get_use_font <'> Fgtk_font_button_get_use_size
     <'> Fgtk_font_button_new <'> Fgtk_font_button_new_with_font
     <'> Fgtk_font_button_set_font_name <'> Fgtk_font_button_set_show_size
     <'> Fgtk_font_button_set_show_style <'> Fgtk_font_button_set_title
     <'> Fgtk_font_button_set_use_font <'> Fgtk_font_button_set_use_size
     <'> Fgtk_font_selection_dialog_get_font_name <'> Fgtk_font_selection_dialog_get_preview_text
     <'> Fgtk_font_selection_dialog_new <'> Fgtk_font_selection_dialog_set_font_name
     <'> Fgtk_font_selection_dialog_set_preview_text <'> Fgtk_font_selection_get_font_name
     <'> Fgtk_font_selection_get_preview_text <'> Fgtk_font_selection_new
     <'> Fgtk_font_selection_set_preview_text <'> Fgtk_frame_get_label
     <'> Fgtk_frame_get_label_align <'> Fgtk_frame_get_label_widget
     <'> Fgtk_frame_get_shadow_type <'> Fgtk_frame_new
     <'> Fgtk_frame_set_label <'> Fgtk_frame_set_label_align
     <'> Fgtk_frame_set_label_widget <'> Fgtk_frame_set_shadow_type
     <'> Fgtk_get_current_event <'> Fgtk_get_current_event_state
     <'> Fgtk_get_current_event_time <'> Fgtk_get_default_language
     <'> Fgtk_get_event_widget <'> Fgtk_grab_add <'> Fgtk_grab_get_current
     <'> Fgtk_grab_remove <'> Fgtk_handle_box_get_handle_position
     <'> Fgtk_handle_box_get_shadow_type <'> Fgtk_handle_box_get_snap_edge
     <'> Fgtk_handle_box_new <'> Fgtk_handle_box_set_handle_position
     <'> Fgtk_handle_box_set_shadow_type <'> Fgtk_handle_box_set_snap_edge
     <'> Fgtk_hbox_new <'> Fgtk_hbutton_box_new <'> Fgtk_hpaned_new
     <'> Fgtk_hruler_new <'> Fgtk_hscale_new <'> Fgtk_hscale_new_with_range
     <'> Fgtk_hscrollbar_new <'> Fgtk_hseparator_new <'> Fgtk_icon_factory_add
     <'> Fgtk_icon_factory_add_default <'> Fgtk_icon_factory_lookup
     <'> Fgtk_icon_factory_lookup_default <'> Fgtk_icon_factory_new
     <'> Fgtk_icon_factory_remove_default <'> Fgtk_icon_info_copy
     <'> Fgtk_icon_info_free <'> Fgtk_icon_info_get_base_size
     <'> Fgtk_icon_info_get_builtin_pixbuf <'> Fgtk_icon_info_get_display_name
     <'> Fgtk_icon_info_get_embedded_rect <'> Fgtk_icon_info_get_filename
     <'> Fgtk_icon_info_load_icon <'> Fgtk_icon_info_set_raw_coordinates
     <'> Fgtk_icon_set_add_source <'> Fgtk_icon_set_copy
     <'> Fgtk_icon_set_get_sizes <'> Fgtk_icon_set_new
     <'> Fgtk_icon_set_new_from_pixbuf <'> Fgtk_icon_set_ref
     <'> Fgtk_icon_set_render_icon <'> Fgtk_icon_set_unref
     <'> Fgtk_icon_size_get_name <'> Fgtk_icon_size_lookup
     <'> Fgtk_icon_size_register <'> Fgtk_icon_size_register_alias
     <'> Fgtk_icon_source_copy <'> Fgtk_icon_source_free
     <'> Fgtk_icon_source_get_direction <'> Fgtk_icon_source_get_direction_wildcarded
     <'> Fgtk_icon_source_get_filename
     <'> Fgtk_icon_source_get_icon_name <'> Fgtk_icon_source_get_pixbuf
     <'> Fgtk_icon_source_get_size <'> Fgtk_icon_source_get_size_wildcarded
     <'> Fgtk_icon_source_get_state <'> Fgtk_icon_source_get_state_wildcarded
     <'> Fgtk_icon_source_new <'> Fgtk_icon_source_set_direction
     <'> Fgtk_icon_source_set_direction_wildcarded
     <'> Fgtk_icon_source_set_filename <'> Fgtk_icon_source_set_pixbuf
     <'> Fgtk_icon_source_set_size <'> Fgtk_icon_source_set_size_wildcarded
     <'> Fgtk_icon_source_set_state <'> Fgtk_icon_source_set_state_wildcarded
     <'> Fgtk_icon_theme_add_builtin_icon <'> Fgtk_icon_theme_append_search_path
     <'> Fgtk_icon_theme_get_default <'> Fgtk_icon_theme_get_example_icon_name
     <'> Fgtk_icon_theme_get_for_screen <'> Fgtk_icon_theme_get_icon_sizes
     <'> Fgtk_icon_theme_get_search_path <'> Fgtk_icon_theme_has_icon
     <'> Fgtk_icon_theme_list_icons <'> Fgtk_icon_theme_load_icon
     <'> Fgtk_icon_theme_lookup_icon <'> Fgtk_icon_theme_new
     <'> Fgtk_icon_theme_prepend_search_path <'> Fgtk_icon_theme_rescan_if_needed
     <'> Fgtk_icon_theme_set_custom_theme <'> Fgtk_icon_theme_set_screen
     <'> Fgtk_icon_view_get_markup_column <'> Fgtk_icon_view_get_model
     <'> Fgtk_icon_view_get_path_at_pos <'> Fgtk_icon_view_get_pixbuf_column
     <'> Fgtk_icon_view_get_selected_items <'> Fgtk_icon_view_get_selection_mode
     <'> Fgtk_icon_view_get_text_column <'> Fgtk_icon_view_item_activated
     <'> Fgtk_icon_view_new <'> Fgtk_icon_view_new_with_model
     <'> Fgtk_icon_view_path_is_selected <'> Fgtk_icon_view_select_all
     <'> Fgtk_icon_view_select_path <'> Fgtk_icon_view_selected_foreach
     <'> Fgtk_icon_view_set_markup_column <'> Fgtk_icon_view_set_model
     <'> Fgtk_icon_view_set_pixbuf_column <'> Fgtk_icon_view_set_selection_mode
     <'> Fgtk_icon_view_set_text_column <'> Fgtk_icon_view_unselect_all
     <'> Fgtk_icon_view_unselect_path <'> Fgtk_im_context_delete_surrounding
     <'> Fgtk_im_context_filter_keypress <'> Fgtk_im_context_focus_in
     <'> Fgtk_im_context_focus_out <'> Fgtk_im_context_get_preedit_string
     <'> Fgtk_im_context_get_surrounding <'> Fgtk_im_context_reset
     <'> Fgtk_im_context_set_client_window <'> Fgtk_im_context_set_cursor_location
     <'> Fgtk_im_context_set_surrounding <'> Fgtk_im_context_set_use_preedit
     <'> Fgtk_im_context_simple_add_table <'> Fgtk_im_context_simple_new
     <'> Fgtk_im_multicontext_append_menuitems <'> Fgtk_im_multicontext_new
     <'> Fgtk_label_get_attributes <'> Fgtk_label_get_ellipsize
     <'> Fgtk_label_get_justify <'> Fgtk_label_get_label
     <'> Fgtk_label_get_layout <'> Fgtk_label_get_layout_offsets
     <'> Fgtk_label_get_line_wrap <'> Fgtk_label_get_mnemonic_keyval
     <'> Fgtk_label_get_mnemonic_widget <'> Fgtk_label_get_selectable
     <'> Fgtk_label_get_selection_bounds <'> Fgtk_label_get_single_line_mode
     <'> Fgtk_label_get_text <'> Fgtk_label_get_use_markup
     <'> Fgtk_label_get_use_underline <'> Fgtk_label_get_width_chars
     <'> Fgtk_label_new <'> Fgtk_label_new_with_mnemonic <'> Fgtk_label_set_angle
     <'> Fgtk_label_set_attributes <'> Fgtk_label_set_ellipsize
     <'> Fgtk_label_set_justify <'> Fgtk_label_set_label
     <'> Fgtk_label_set_line_wrap <'> Fgtk_label_set_markup
     <'> Fgtk_label_set_markup_with_mnemonic <'> Fgtk_label_set_mnemonic_widget
     <'> Fgtk_label_set_pattern <'> Fgtk_label_set_selectable
     <'> Fgtk_label_set_single_line_mode <'> Fgtk_label_set_text
     <'> Fgtk_label_set_text_with_mnemonic <'> Fgtk_label_set_use_markup
     <'> Fgtk_label_set_use_underline <'> Fgtk_label_set_width_chars
     <'> Fgtk_layout_get_size
     <'> Fgtk_layout_move <'> Fgtk_layout_new
     <'> Fgtk_layout_set_size
     <'> Fgtk_list_store_append
     <'> Fgtk_list_store_clear <'> Fgtk_list_store_insert
     <'> Fgtk_list_store_insert_after <'> Fgtk_list_store_insert_before
     <'> Fgtk_list_store_move_after <'> Fgtk_list_store_move_before
     <'> Fgtk_list_store_new <'> Fgtk_list_store_newv <'> Fgtk_list_store_prepend
     <'> Fgtk_list_store_remove <'> Fgtk_list_store_reorder
     <'> Fgtk_list_store_set <'> Fgtk_list_store_set_column_types
     <'> Fgtk_list_store_swap <'> Fgtk_menu_attach <'> Fgtk_menu_bar_new
     <'> Fgtk_menu_detach <'> Fgtk_menu_get_accel_group <'> Fgtk_menu_get_active
     <'> Fgtk_menu_get_attach_widget <'> Fgtk_menu_get_for_attach_widget
     <'> Fgtk_menu_get_tearoff_state <'> Fgtk_menu_get_title
     <'> Fgtk_menu_item_activate <'> Fgtk_menu_item_deselect
     <'> Fgtk_menu_item_get_right_justified <'> Fgtk_menu_item_get_submenu
     <'> Fgtk_menu_item_new <'> Fgtk_menu_item_new_with_mnemonic
     <'> Fgtk_menu_item_select <'> Fgtk_menu_item_set_accel_path
     <'> Fgtk_menu_item_set_right_justified <'> Fgtk_menu_item_set_submenu
     <'> Fgtk_menu_item_toggle_size_allocate
     <'> Fgtk_menu_item_toggle_size_request <'> Fgtk_menu_new
     <'> Fgtk_menu_popdown <'> Fgtk_menu_popup <'> Fgtk_menu_reorder_child
     <'> Fgtk_menu_reposition <'> Fgtk_menu_set_accel_group
     <'> Fgtk_menu_set_accel_path <'> Fgtk_menu_set_active
     <'> Fgtk_menu_set_monitor <'> Fgtk_menu_set_screen
     <'> Fgtk_menu_set_tearoff_state <'> Fgtk_menu_set_title
     <'> Fgtk_menu_shell_activate_item <'> Fgtk_menu_shell_append
     <'> Fgtk_menu_shell_cancel <'> Fgtk_menu_shell_deactivate
     <'> Fgtk_menu_shell_deselect <'> Fgtk_menu_shell_insert
     <'> Fgtk_menu_shell_prepend <'> Fgtk_menu_shell_select_first
     <'> Fgtk_menu_shell_select_item <'> Fgtk_menu_tool_button_get_menu
     <'> Fgtk_menu_tool_button_new <'> Fgtk_menu_tool_button_new_from_stock
     <'> Fgtk_menu_tool_button_set_menu <'> Fgtk_misc_get_alignment
     <'> Fgtk_misc_get_padding <'> Fgtk_misc_set_alignment
     <'> Fgtk_misc_set_padding <'> Fgtk_notebook_append_page
     <'> Fgtk_notebook_append_page_menu <'> Fgtk_notebook_get_current_page
     <'> Fgtk_notebook_get_menu_label <'> Fgtk_notebook_get_menu_label_text
     <'> Fgtk_notebook_get_n_pages <'> Fgtk_notebook_get_nth_page
     <'> Fgtk_notebook_get_scrollable <'> Fgtk_notebook_get_show_border
     <'> Fgtk_notebook_get_show_tabs <'> Fgtk_notebook_get_tab_label
     <'> Fgtk_notebook_get_tab_label_text <'> Fgtk_notebook_get_tab_pos
     <'> Fgtk_notebook_insert_page <'> Fgtk_notebook_insert_page_menu
     <'> Fgtk_notebook_new <'> Fgtk_notebook_next_page <'> Fgtk_notebook_page_num
     <'> Fgtk_notebook_popup_disable <'> Fgtk_notebook_popup_enable
     <'> Fgtk_notebook_prepend_page <'> Fgtk_notebook_prepend_page_menu
     <'> Fgtk_notebook_prev_page <'> Fgtk_notebook_remove_page
     <'> Fgtk_notebook_reorder_child <'> Fgtk_notebook_set_current_page
     <'> Fgtk_notebook_set_menu_label <'> Fgtk_notebook_set_menu_label_text
     <'> Fgtk_notebook_set_scrollable <'> Fgtk_notebook_set_show_border
     <'> Fgtk_notebook_set_show_tabs <'> Fgtk_notebook_set_tab_label
     <'> Fgtk_notebook_set_tab_label_text <'> Fgtk_notebook_set_tab_pos
     <'> Fgtk_paned_add1 <'> Fgtk_paned_add2 <'> Fgtk_paned_get_child1
     <'> Fgtk_paned_get_child2 <'> Fgtk_paned_get_position <'> Fgtk_paned_pack1
     <'> Fgtk_paned_pack2 <'> Fgtk_paned_set_position <'> Fgtk_plug_construct
     <'> Fgtk_plug_get_id <'> Fgtk_plug_new <'> Fgtk_progress_bar_get_ellipsize
     <'> Fgtk_progress_bar_get_fraction <'> Fgtk_progress_bar_get_pulse_step
     <'> Fgtk_progress_bar_get_text <'> Fgtk_progress_bar_new
     <'> Fgtk_progress_bar_pulse <'> Fgtk_progress_bar_set_ellipsize
     <'> Fgtk_progress_bar_set_fraction <'> Fgtk_progress_bar_set_pulse_step
     <'> Fgtk_progress_bar_set_text <'> Fgtk_propagate_event
     <'> Fgtk_radio_action_get_current_value <'> Fgtk_radio_action_get_group
     <'> Fgtk_radio_action_new <'> Fgtk_radio_action_set_group
     <'> Fgtk_radio_button_get_group <'> Fgtk_radio_button_new
     <'> Fgtk_radio_button_new_from_widget <'> Fgtk_radio_button_new_with_label_from_widget
     <'> Fgtk_radio_button_new_with_mnemonic <'> Fgtk_radio_button_new_with_mnemonic_from_widget
     <'> Fgtk_radio_button_set_group <'> Fgtk_radio_menu_item_get_group
     <'> Fgtk_radio_menu_item_new <'> Fgtk_radio_menu_item_new_from_widget
     <'> Fgtk_radio_menu_item_new_with_label_from_widget
     <'> Fgtk_radio_menu_item_new_with_mnemonic
     <'> Fgtk_radio_menu_item_new_with_mnemonic_from_widget
     <'> Fgtk_radio_menu_item_set_group <'> Fgtk_radio_tool_button_get_group
     <'> Fgtk_radio_tool_button_new <'> Fgtk_radio_tool_button_new_from_stock
     <'> Fgtk_radio_tool_button_new_from_widget
     <'> Fgtk_radio_tool_button_new_with_stock_from_widget
     <'> Fgtk_radio_tool_button_set_group <'> Fgtk_range_get_adjustment
     <'> Fgtk_range_get_inverted <'> Fgtk_range_get_update_policy
     <'> Fgtk_range_get_value <'> Fgtk_range_set_adjustment
     <'> Fgtk_range_set_increments <'> Fgtk_range_set_inverted
     <'> Fgtk_range_set_range <'> Fgtk_range_set_update_policy
     <'> Fgtk_range_set_value <'> Fgtk_rc_add_default_file
     <'> Fgtk_rc_get_default_files <'> Fgtk_rc_get_im_module_file
     <'> Fgtk_rc_get_im_module_path <'> Fgtk_rc_get_module_dir
     <'> Fgtk_rc_get_style <'> Fgtk_rc_get_theme_dir <'> Fgtk_rc_parse
     <'> Fgtk_rc_reparse_all <'> Fgtk_rc_set_default_files <'> Fgtk_rc_style_copy
     <'> Fgtk_rc_style_new <'> Fgtk_ruler_get_metric <'> Fgtk_ruler_get_range
     <'> Fgtk_ruler_set_metric <'> Fgtk_ruler_set_range <'> Fgtk_scale_get_digits
     <'> Fgtk_scale_get_draw_value <'> Fgtk_scale_get_layout
     <'> Fgtk_scale_get_layout_offsets <'> Fgtk_scale_get_value_pos
     <'> Fgtk_scale_set_digits <'> Fgtk_scale_set_draw_value
     <'> Fgtk_scale_set_value_pos <'> Fgtk_scrolled_window_add_with_viewport
     <'> Fgtk_scrolled_window_get_hadjustment <'> Fgtk_scrolled_window_get_placement
     <'> Fgtk_scrolled_window_get_policy <'> Fgtk_scrolled_window_get_shadow_type
     <'> Fgtk_scrolled_window_get_vadjustment <'> Fgtk_scrolled_window_new
     <'> Fgtk_scrolled_window_set_hadjustment <'> Fgtk_scrolled_window_set_placement
     <'> Fgtk_scrolled_window_set_policy <'> Fgtk_scrolled_window_set_shadow_type
     <'> Fgtk_scrolled_window_set_vadjustment <'> Fgtk_selection_add_target
     <'> Fgtk_selection_add_targets <'> Fgtk_selection_clear_targets
     <'> Fgtk_selection_convert <'> Fgtk_selection_data_copy
     <'> Fgtk_selection_data_free <'> Fgtk_selection_data_get_pixbuf
     <'> Fgtk_selection_data_get_targets <'> Fgtk_selection_data_get_text
     <'> Fgtk_selection_data_get_uris <'> Fgtk_selection_data_set
     <'> Fgtk_selection_data_set_pixbuf <'> Fgtk_selection_data_set_text
     <'> Fgtk_selection_data_set_uris <'> Fgtk_selection_data_targets_include_image
     <'> Fgtk_selection_data_targets_include_text <'> Fgtk_selection_owner_set
     <'> Fgtk_selection_remove_all <'> Fgtk_separator_menu_item_new
     <'> Fgtk_separator_tool_item_get_draw <'> Fgtk_separator_tool_item_new
     <'> Fgtk_separator_tool_item_set_draw <'> Fgtk_set_locale
     <'> Fgtk_size_group_add_widget <'> Fgtk_size_group_get_mode
     <'> Fgtk_size_group_new <'> Fgtk_size_group_remove_widget
     <'> Fgtk_size_group_set_mode <'> Fgtk_socket_add_id
     <'> Fgtk_socket_get_id <'> Fgtk_socket_new <'> Fgtk_spin_button_configure
     <'> Fgtk_spin_button_get_adjustment <'> Fgtk_spin_button_get_digits
     <'> Fgtk_spin_button_get_increments <'> Fgtk_spin_button_get_numeric
     <'> Fgtk_spin_button_get_range <'> Fgtk_spin_button_get_snap_to_ticks
     <'> Fgtk_spin_button_get_update_policy <'> Fgtk_spin_button_get_value
     <'> Fgtk_spin_button_get_value_as_int <'> Fgtk_spin_button_get_wrap
     <'> Fgtk_spin_button_new <'> Fgtk_spin_button_new_with_range
     <'> Fgtk_spin_button_set_adjustment <'> Fgtk_spin_button_set_digits
     <'> Fgtk_spin_button_set_increments <'> Fgtk_spin_button_set_numeric
     <'> Fgtk_spin_button_set_range <'> Fgtk_spin_button_set_snap_to_ticks
     <'> Fgtk_spin_button_set_update_policy <'> Fgtk_spin_button_set_value
     <'> Fgtk_spin_button_set_wrap <'> Fgtk_spin_button_spin
     <'> Fgtk_spin_button_update <'> Fgtk_statusbar_get_context_id
     <'> Fgtk_statusbar_new
     <'> Fgtk_statusbar_pop <'> Fgtk_statusbar_push
     <'> Fgtk_statusbar_remove
     <'> Fgtk_stock_add <'> Fgtk_stock_add_static <'> Fgtk_stock_item_copy
     <'> Fgtk_stock_item_free <'> Fgtk_stock_list_ids <'> Fgtk_stock_lookup
     <'> Fgtk_style_attach <'> Fgtk_style_copy <'> Fgtk_style_detach
     <'> Fgtk_style_lookup_icon_set <'> Fgtk_style_new <'> Fgtk_style_render_icon
     <'> Fgtk_style_set_background <'> Fgtk_table_attach
     <'> Fgtk_table_attach_defaults <'> Fgtk_table_get_col_spacing
     <'> Fgtk_table_get_default_col_spacing <'> Fgtk_table_get_default_row_spacing
     <'> Fgtk_table_get_homogeneous <'> Fgtk_table_get_row_spacing
     <'> Fgtk_table_new <'> Fgtk_table_resize <'> Fgtk_table_set_col_spacing
     <'> Fgtk_table_set_col_spacings <'> Fgtk_table_set_homogeneous
     <'> Fgtk_table_set_row_spacing <'> Fgtk_table_set_row_spacings
     <'> Fgtk_target_list_add <'> Fgtk_target_list_add_image_targets
     <'> Fgtk_target_list_add_table <'> Fgtk_target_list_add_text_targets
     <'> Fgtk_target_list_add_uri_targets <'> Fgtk_target_list_find
     <'> Fgtk_target_list_remove <'> Fgtk_target_list_unref
     <'> Fgtk_tearoff_menu_item_new <'> Fgtk_text_attributes_copy
     <'> Fgtk_text_attributes_copy_values <'> Fgtk_text_attributes_new
     <'> Fgtk_text_attributes_unref <'> Fgtk_text_buffer_add_selection_clipboard
     <'> Fgtk_text_buffer_apply_tag <'> Fgtk_text_buffer_apply_tag_by_name
     <'> Fgtk_text_buffer_backspace <'> Fgtk_text_buffer_begin_user_action
     <'> Fgtk_text_buffer_copy_clipboard <'> Fgtk_text_buffer_create_child_anchor
     <'> Fgtk_text_buffer_create_mark <'> Fgtk_text_buffer_create_tag
     <'> Fgtk_text_buffer_cut_clipboard <'> Fgtk_text_buffer_delete
     <'> Fgtk_text_buffer_delete_interactive <'> Fgtk_text_buffer_delete_mark
     <'> Fgtk_text_buffer_delete_mark_by_name
     <'> Fgtk_text_buffer_delete_selection <'> Fgtk_text_buffer_end_user_action
     <'> Fgtk_text_buffer_get_bounds <'> Fgtk_text_buffer_get_char_count
     <'> Fgtk_text_buffer_get_end_iter <'> Fgtk_text_buffer_get_insert
     <'> Fgtk_text_buffer_get_iter_at_child_anchor <'> Fgtk_text_buffer_get_iter_at_line
     <'> Fgtk_text_buffer_get_iter_at_line_index <'> Fgtk_text_buffer_get_iter_at_line_offset
     <'> Fgtk_text_buffer_get_iter_at_mark <'> Fgtk_text_buffer_get_iter_at_offset
     <'> Fgtk_text_buffer_get_line_count <'> Fgtk_text_buffer_get_mark
     <'> Fgtk_text_buffer_get_modified <'> Fgtk_text_buffer_get_selection_bound
     <'> Fgtk_text_buffer_get_selection_bounds <'> Fgtk_text_buffer_get_slice
     <'> Fgtk_text_buffer_get_start_iter <'> Fgtk_text_buffer_get_tag_table
     <'> Fgtk_text_buffer_get_text <'> Fgtk_text_buffer_insert
     <'> Fgtk_text_buffer_insert_at_cursor <'> Fgtk_text_buffer_insert_child_anchor
     <'> Fgtk_text_buffer_insert_interactive <'> Fgtk_text_buffer_insert_interactive_at_cursor
     <'> Fgtk_text_buffer_insert_pixbuf <'> Fgtk_text_buffer_insert_range
     <'> Fgtk_text_buffer_insert_range_interactive <'> Fgtk_text_buffer_insert_with_tags
     <'> Fgtk_text_buffer_insert_with_tags_by_name <'> Fgtk_text_buffer_move_mark
     <'> Fgtk_text_buffer_move_mark_by_name <'> Fgtk_text_buffer_new
     <'> Fgtk_text_buffer_paste_clipboard <'> Fgtk_text_buffer_place_cursor
     <'> Fgtk_text_buffer_remove_all_tags <'> Fgtk_text_buffer_remove_selection_clipboard
     <'> Fgtk_text_buffer_remove_tag <'> Fgtk_text_buffer_remove_tag_by_name
     <'> Fgtk_text_buffer_select_range <'> Fgtk_text_buffer_set_modified
     <'> Fgtk_text_buffer_set_text <'> Fgtk_text_child_anchor_get_deleted
     <'> Fgtk_text_child_anchor_get_widgets <'> Fgtk_text_child_anchor_new
     <'> Fgtk_text_iter_backward_char <'> Fgtk_text_iter_backward_chars
     <'> Fgtk_text_iter_backward_cursor_position <'> Fgtk_text_iter_backward_cursor_positions
     <'> Fgtk_text_iter_backward_find_char <'> Fgtk_text_iter_backward_line
     <'> Fgtk_text_iter_backward_lines <'> Fgtk_text_iter_backward_search
     <'> Fgtk_text_iter_backward_sentence_start <'> Fgtk_text_iter_backward_sentence_starts
     <'> Fgtk_text_iter_backward_to_tag_toggle <'> Fgtk_text_iter_backward_word_start
     <'> Fgtk_text_iter_backward_word_starts <'> Fgtk_text_iter_begins_tag
     <'> Fgtk_text_iter_can_insert <'> Fgtk_text_iter_compare
     <'> Fgtk_text_iter_copy <'> Fgtk_text_iter_editable
     <'> Fgtk_text_iter_ends_line <'> Fgtk_text_iter_ends_sentence
     <'> Fgtk_text_iter_ends_tag <'> Fgtk_text_iter_ends_word
     <'> Fgtk_text_iter_equal <'> Fgtk_text_iter_forward_char
     <'> Fgtk_text_iter_forward_chars <'> Fgtk_text_iter_forward_cursor_position
     <'> Fgtk_text_iter_forward_cursor_positions <'> Fgtk_text_iter_forward_find_char 
     <'> Fgtk_text_iter_forward_line <'> Fgtk_text_iter_forward_lines 
     <'> Fgtk_text_iter_forward_search <'> Fgtk_text_iter_forward_sentence_end
     <'> Fgtk_text_iter_forward_sentence_ends <'> Fgtk_text_iter_forward_to_end
     <'> Fgtk_text_iter_forward_to_line_end
     <'> Fgtk_text_iter_forward_to_tag_toggle <'> Fgtk_text_iter_forward_word_end
     <'> Fgtk_text_iter_forward_word_ends <'> Fgtk_text_iter_free
     <'> Fgtk_text_iter_get_attributes <'> Fgtk_text_iter_get_buffer
     <'> Fgtk_text_iter_get_bytes_in_line <'> Fgtk_text_iter_get_char
     <'> Fgtk_text_iter_get_chars_in_line <'> Fgtk_text_iter_get_child_anchor
     <'> Fgtk_text_iter_get_language <'> Fgtk_text_iter_get_line
     <'> Fgtk_text_iter_get_line_index <'> Fgtk_text_iter_get_line_offset
     <'> Fgtk_text_iter_get_marks <'> Fgtk_text_iter_get_offset
     <'> Fgtk_text_iter_get_pixbuf <'> Fgtk_text_iter_get_slice
     <'> Fgtk_text_iter_get_tags <'> Fgtk_text_iter_get_text
     <'> Fgtk_text_iter_get_toggled_tags <'> Fgtk_text_iter_get_visible_line_index
     <'> Fgtk_text_iter_get_visible_line_offset
     <'> Fgtk_text_iter_get_visible_slice <'> Fgtk_text_iter_get_visible_text
     <'> Fgtk_text_iter_has_tag <'> Fgtk_text_iter_in_range
     <'> Fgtk_text_iter_inside_sentence <'> Fgtk_text_iter_inside_word
     <'> Fgtk_text_iter_is_cursor_position <'> Fgtk_text_iter_is_end
     <'> Fgtk_text_iter_is_start <'> Fgtk_text_iter_order
     <'> Fgtk_text_iter_set_line <'> Fgtk_text_iter_set_line_index
     <'> Fgtk_text_iter_set_line_offset <'> Fgtk_text_iter_set_offset
     <'> Fgtk_text_iter_set_visible_line_index
     <'> Fgtk_text_iter_set_visible_line_offset <'> Fgtk_text_iter_starts_line
     <'> Fgtk_text_iter_starts_sentence <'> Fgtk_text_iter_starts_word
     <'> Fgtk_text_iter_toggles_tag <'> Fgtk_text_mark_get_buffer
     <'> Fgtk_text_mark_get_deleted <'> Fgtk_text_mark_get_left_gravity
     <'> Fgtk_text_mark_get_name <'> Fgtk_text_mark_get_visible
     <'> Fgtk_text_mark_set_visible <'> Fgtk_text_tag_event
     <'> Fgtk_text_tag_get_priority <'> Fgtk_text_tag_new
     <'> Fgtk_text_tag_set_priority <'> Fgtk_text_tag_table_add
     <'> Fgtk_text_tag_table_foreach <'> Fgtk_text_tag_table_get_size
     <'> Fgtk_text_tag_table_lookup <'> Fgtk_text_tag_table_new
     <'> Fgtk_text_tag_table_remove <'> Fgtk_text_view_add_child_at_anchor
     <'> Fgtk_text_view_add_child_in_window <'> Fgtk_text_view_backward_display_line
     <'> Fgtk_text_view_backward_display_line_start <'> Fgtk_text_view_buffer_to_window_coords
     <'> Fgtk_text_view_forward_display_line <'> Fgtk_text_view_forward_display_line_end
     <'> Fgtk_text_view_get_accepts_tab <'> Fgtk_text_view_get_border_window_size
     <'> Fgtk_text_view_get_buffer <'> Fgtk_text_view_get_cursor_visible
     <'> Fgtk_text_view_get_default_attributes <'> Fgtk_text_view_get_editable
     <'> Fgtk_text_view_get_indent <'> Fgtk_text_view_get_iter_at_location
     <'> Fgtk_text_view_get_iter_location <'> Fgtk_text_view_get_justification
     <'> Fgtk_text_view_get_left_margin <'> Fgtk_text_view_get_line_at_y
     <'> Fgtk_text_view_get_line_yrange <'> Fgtk_text_view_get_overwrite
     <'> Fgtk_text_view_get_pixels_above_lines <'> Fgtk_text_view_get_pixels_below_lines
     <'> Fgtk_text_view_get_pixels_inside_wrap <'> Fgtk_text_view_get_right_margin
     <'> Fgtk_text_view_get_tabs <'> Fgtk_text_view_get_visible_rect
     <'> Fgtk_text_view_get_window <'> Fgtk_text_view_get_window_type
     <'> Fgtk_text_view_get_wrap_mode <'> Fgtk_text_view_move_child
     <'> Fgtk_text_view_move_mark_onscreen <'> Fgtk_text_view_move_visually
     <'> Fgtk_text_view_new <'> Fgtk_text_view_new_with_buffer
     <'> Fgtk_text_view_place_cursor_onscreen
     <'> Fgtk_text_view_scroll_mark_onscreen <'> Fgtk_text_view_scroll_to_iter
     <'> Fgtk_text_view_scroll_to_mark <'> Fgtk_text_view_set_accepts_tab
     <'> Fgtk_text_view_set_border_window_size <'> Fgtk_text_view_set_buffer
     <'> Fgtk_text_view_set_cursor_visible <'> Fgtk_text_view_set_editable
     <'> Fgtk_text_view_set_indent <'> Fgtk_text_view_set_justification
     <'> Fgtk_text_view_set_left_margin <'> Fgtk_text_view_set_overwrite
     <'> Fgtk_text_view_set_pixels_above_lines <'> Fgtk_text_view_set_pixels_below_lines
     <'> Fgtk_text_view_set_pixels_inside_wrap <'> Fgtk_text_view_set_right_margin
     <'> Fgtk_text_view_set_tabs <'> Fgtk_text_view_set_wrap_mode
     <'> Fgtk_text_view_starts_display_line
     <'> Fgtk_text_view_window_to_buffer_coords <'> Fgtk_toggle_action_get_active
     <'> Fgtk_toggle_action_get_draw_as_radio <'> Fgtk_toggle_action_new
     <'> Fgtk_toggle_action_set_active <'> Fgtk_toggle_action_set_draw_as_radio
     <'> Fgtk_toggle_action_toggled <'> Fgtk_toggle_button_get_active
     <'> Fgtk_toggle_button_get_inconsistent <'> Fgtk_toggle_button_get_mode
     <'> Fgtk_toggle_button_new <'> Fgtk_toggle_button_new_with_mnemonic
     <'> Fgtk_toggle_button_set_active <'> Fgtk_toggle_button_set_inconsistent
     <'> Fgtk_toggle_button_set_mode <'> Fgtk_toggle_button_toggled
     <'> Fgtk_toggle_tool_button_get_active <'> Fgtk_toggle_tool_button_new
     <'> Fgtk_toggle_tool_button_new_from_stock
     <'> Fgtk_toggle_tool_button_set_active <'> Fgtk_tool_button_get_icon_widget
     <'> Fgtk_tool_button_get_label <'> Fgtk_tool_button_get_label_widget
     <'> Fgtk_tool_button_get_stock_id <'> Fgtk_tool_button_get_use_underline
     <'> Fgtk_tool_button_new <'> Fgtk_tool_button_new_from_stock
     <'> Fgtk_tool_button_set_icon_widget <'> Fgtk_tool_button_set_label
     <'> Fgtk_tool_button_set_label_widget <'> Fgtk_tool_button_set_stock_id
     <'> Fgtk_tool_button_set_use_underline <'> Fgtk_tool_item_get_expand
     <'> Fgtk_tool_item_get_homogeneous <'> Fgtk_tool_item_get_icon_size
     <'> Fgtk_tool_item_get_is_important <'> Fgtk_tool_item_get_proxy_menu_item
     <'> Fgtk_tool_item_get_relief_style <'> Fgtk_tool_item_get_toolbar_style
     <'> Fgtk_tool_item_get_use_drag_window <'> Fgtk_tool_item_get_visible_horizontal
     <'> Fgtk_tool_item_get_visible_vertical <'> Fgtk_tool_item_new
     <'> Fgtk_tool_item_rebuild_menu <'> Fgtk_tool_item_retrieve_proxy_menu_item
     <'> Fgtk_tool_item_set_expand <'> Fgtk_tool_item_set_homogeneous
     <'> Fgtk_tool_item_set_is_important <'> Fgtk_tool_item_set_proxy_menu_item
     <'> Fgtk_tool_item_set_visible_horizontal
     <'> Fgtk_tool_item_set_visible_vertical <'> Fgtk_toolbar_get_drop_index
     <'> Fgtk_toolbar_get_icon_size <'> Fgtk_toolbar_get_item_index
     <'> Fgtk_toolbar_get_n_items <'> Fgtk_toolbar_get_nth_item
     <'> Fgtk_toolbar_get_relief_style <'> Fgtk_toolbar_get_show_arrow
     <'> Fgtk_toolbar_get_style <'> Fgtk_toolbar_insert <'> Fgtk_toolbar_new
     <'> Fgtk_toolbar_set_show_arrow <'> Fgtk_toolbar_set_style
     <'> Fgtk_toolbar_unset_style <'> Fgtk_tree_drag_dest_drag_data_received
     <'> Fgtk_tree_drag_dest_row_drop_possible <'> Fgtk_tree_drag_source_drag_data_delete
     <'> Fgtk_tree_drag_source_drag_data_get <'> Fgtk_tree_drag_source_row_draggable 
     <'> Fgtk_tree_get_row_drag_data <'> Fgtk_tree_iter_copy <'> Fgtk_tree_iter_free
     <'> Fgtk_tree_model_filter_clear_cache <'> Fgtk_tree_model_filter_convert_child_path_to_path
     <'> Fgtk_tree_model_filter_convert_iter_to_child_iter
     <'> Fgtk_tree_model_filter_convert_path_to_child_path
     <'> Fgtk_tree_model_filter_get_model <'> Fgtk_tree_model_filter_new
     <'> Fgtk_tree_model_filter_refilter
     <'> Fgtk_tree_model_filter_set_visible_column <'> Fgtk_tree_model_foreach
     <'> Fgtk_tree_model_get_column_type <'> Fgtk_tree_model_get_flags
     <'> Fgtk_tree_model_get_iter <'> Fgtk_tree_model_get_iter_first
     <'> Fgtk_tree_model_get_iter_from_string <'> Fgtk_tree_model_get_n_columns
     <'> Fgtk_tree_model_get_path <'> Fgtk_tree_model_get_string_from_iter
     <'> Fgtk_tree_model_iter_children <'> Fgtk_tree_model_iter_has_child
     <'> Fgtk_tree_model_iter_n_children <'> Fgtk_tree_model_iter_next
     <'> Fgtk_tree_model_iter_nth_child <'> Fgtk_tree_model_iter_parent
     <'> Fgtk_tree_model_ref_node <'> Fgtk_tree_model_row_changed
     <'> Fgtk_tree_model_row_deleted <'> Fgtk_tree_model_row_has_child_toggled
     <'> Fgtk_tree_model_row_inserted <'> Fgtk_tree_model_rows_reordered
     <'> Fgtk_tree_model_sort_clear_cache <'> Fgtk_tree_model_sort_convert_child_iter_to_iter
     <'> Fgtk_tree_model_sort_convert_child_path_to_path
     <'> Fgtk_tree_model_sort_convert_iter_to_child_iter
     <'> Fgtk_tree_model_sort_convert_path_to_child_path
     <'> Fgtk_tree_model_sort_get_model <'> Fgtk_tree_model_sort_iter_is_valid
     <'> Fgtk_tree_model_sort_new_with_model <'> Fgtk_tree_model_sort_reset_default_sort_func
     <'> Fgtk_tree_model_unref_node <'> Fgtk_tree_path_append_index
     <'> Fgtk_tree_path_compare <'> Fgtk_tree_path_copy <'> Fgtk_tree_path_down
     <'> Fgtk_tree_path_free <'> Fgtk_tree_path_get_depth
     <'> Fgtk_tree_path_get_indices <'> Fgtk_tree_path_is_ancestor
     <'> Fgtk_tree_path_is_descendant <'> Fgtk_tree_path_new
     <'> Fgtk_tree_path_new_first <'> Fgtk_tree_path_new_from_string
     <'> Fgtk_tree_path_next <'> Fgtk_tree_path_prepend_index
     <'> Fgtk_tree_path_prev <'> Fgtk_tree_path_to_string <'> Fgtk_tree_path_up
     <'> Fgtk_tree_row_reference_deleted <'> Fgtk_tree_row_reference_free
     <'> Fgtk_tree_row_reference_get_path <'> Fgtk_tree_row_reference_inserted
     <'> Fgtk_tree_row_reference_new <'> Fgtk_tree_row_reference_new_proxy
     <'> Fgtk_tree_row_reference_reordered <'> Fgtk_tree_row_reference_valid
     <'> Fgtk_tree_selection_count_selected_rows <'> Fgtk_tree_selection_get_mode
     <'> Fgtk_tree_selection_get_selected <'> Fgtk_tree_selection_get_selected_rows
     <'> Fgtk_tree_selection_get_tree_view <'> Fgtk_tree_selection_get_user_data
     <'> Fgtk_tree_selection_iter_is_selected <'> Fgtk_tree_selection_path_is_selected
     <'> Fgtk_tree_selection_select_all <'> Fgtk_tree_selection_select_iter
     <'> Fgtk_tree_selection_select_path <'> Fgtk_tree_selection_select_range
     <'> Fgtk_tree_selection_selected_foreach <'> Fgtk_tree_selection_set_mode
     <'> Fgtk_tree_selection_set_select_function <'> Fgtk_tree_selection_unselect_all 
     <'> Fgtk_tree_selection_unselect_iter <'> Fgtk_tree_selection_unselect_path 
     <'> Fgtk_tree_set_row_drag_data <'> Fgtk_tree_sortable_get_sort_column_id
     <'> Fgtk_tree_sortable_has_default_sort_func <'> Fgtk_tree_sortable_set_default_sort_func
     <'> Fgtk_tree_sortable_set_sort_column_id <'> Fgtk_tree_sortable_set_sort_func
     <'> Fgtk_tree_sortable_sort_column_changed <'> Fgtk_tree_store_append
     <'> Fgtk_tree_store_clear <'> Fgtk_tree_store_insert
     <'> Fgtk_tree_store_insert_after <'> Fgtk_tree_store_insert_before
     <'> Fgtk_tree_store_is_ancestor <'> Fgtk_tree_store_iter_depth
     <'> Fgtk_tree_store_new <'> Fgtk_tree_store_newv <'> Fgtk_tree_store_prepend
     <'> Fgtk_tree_store_remove <'> Fgtk_tree_store_reorder
     <'> Fgtk_tree_store_set <'> Fgtk_tree_store_set_column_types
     <'> Fgtk_tree_store_swap <'> Fgtk_tree_view_append_column
     <'> Fgtk_tree_view_collapse_all <'> Fgtk_tree_view_collapse_row
     <'> Fgtk_tree_view_column_add_attribute <'> Fgtk_tree_view_column_cell_get_position
     <'> Fgtk_tree_view_column_cell_get_size <'> Fgtk_tree_view_column_cell_is_visible
     <'> Fgtk_tree_view_column_cell_set_cell_data <'> Fgtk_tree_view_column_clear
     <'> Fgtk_tree_view_column_clear_attributes <'> Fgtk_tree_view_column_clicked
     <'> Fgtk_tree_view_column_get_alignment <'> Fgtk_tree_view_column_get_clickable 
     <'> Fgtk_tree_view_column_get_expand <'> Fgtk_tree_view_column_get_fixed_width
     <'> Fgtk_tree_view_column_get_max_width <'> Fgtk_tree_view_column_get_min_width
     <'> Fgtk_tree_view_column_get_reorderable <'> Fgtk_tree_view_column_get_resizable
     <'> Fgtk_tree_view_column_get_sizing <'> Fgtk_tree_view_column_get_sort_column_id
     <'> Fgtk_tree_view_column_get_sort_indicator <'> Fgtk_tree_view_column_get_sort_order
     <'> Fgtk_tree_view_column_get_spacing <'> Fgtk_tree_view_column_get_title
     <'> Fgtk_tree_view_column_get_visible <'> Fgtk_tree_view_column_get_widget
     <'> Fgtk_tree_view_column_get_width <'> Fgtk_tree_view_column_new
     <'> Fgtk_tree_view_column_new_with_attributes <'> Fgtk_tree_view_column_pack_end 
     <'> Fgtk_tree_view_column_pack_start <'> Fgtk_tree_view_column_set_alignment
     <'> Fgtk_tree_view_column_set_attributes <'> Fgtk_tree_view_column_set_cell_data_func
     <'> Fgtk_tree_view_column_set_clickable <'> Fgtk_tree_view_column_set_expand
     <'> Fgtk_tree_view_column_set_fixed_width <'> Fgtk_tree_view_column_set_max_width
     <'> Fgtk_tree_view_column_set_min_width <'> Fgtk_tree_view_column_set_reorderable
     <'> Fgtk_tree_view_column_set_resizable <'> Fgtk_tree_view_column_set_sizing
     <'> Fgtk_tree_view_column_set_sort_column_id <'> Fgtk_tree_view_column_set_sort_indicator
     <'> Fgtk_tree_view_column_set_sort_order
     <'> Fgtk_tree_view_column_set_spacing <'> Fgtk_tree_view_column_set_title
     <'> Fgtk_tree_view_column_set_visible <'> Fgtk_tree_view_column_set_widget
     <'> Fgtk_tree_view_columns_autosize <'> Fgtk_tree_view_enable_model_drag_dest
     <'> Fgtk_tree_view_enable_model_drag_source <'> Fgtk_tree_view_expand_all
     <'> Fgtk_tree_view_expand_row <'> Fgtk_tree_view_expand_to_path
     <'> Fgtk_tree_view_get_background_area <'> Fgtk_tree_view_get_bin_window
     <'> Fgtk_tree_view_get_cell_area <'> Fgtk_tree_view_get_column
     <'> Fgtk_tree_view_get_columns <'> Fgtk_tree_view_get_cursor
     <'> Fgtk_tree_view_get_dest_row_at_pos <'> Fgtk_tree_view_get_drag_dest_row
     <'> Fgtk_tree_view_get_enable_search <'> Fgtk_tree_view_get_expander_column
     <'> Fgtk_tree_view_get_fixed_height_mode
     <'> Fgtk_tree_view_get_headers_visible <'> Fgtk_tree_view_get_hover_expand
     <'> Fgtk_tree_view_get_hover_selection <'> Fgtk_tree_view_get_model
     <'> Fgtk_tree_view_get_path_at_pos <'> Fgtk_tree_view_get_reorderable ) constant gtk-procs-2

  #( <'> Fgtk_tree_view_get_rules_hint <'> Fgtk_tree_view_get_search_column
     <'> Fgtk_tree_view_get_search_equal_func <'> Fgtk_tree_view_get_selection
     <'> Fgtk_tree_view_get_visible_rect
     <'> Fgtk_tree_view_insert_column <'> Fgtk_tree_view_insert_column_with_attributes
     <'> Fgtk_tree_view_insert_column_with_data_func <'> Fgtk_tree_view_map_expanded_rows
     <'> Fgtk_tree_view_move_column_after <'> Fgtk_tree_view_new <'> Fgtk_tree_view_new_with_model
     <'> Fgtk_tree_view_remove_column <'> Fgtk_tree_view_row_activated
     <'> Fgtk_tree_view_row_expanded <'> Fgtk_tree_view_scroll_to_cell
     <'> Fgtk_tree_view_scroll_to_point <'> Fgtk_tree_view_set_column_drag_function
     <'> Fgtk_tree_view_set_cursor <'> Fgtk_tree_view_set_drag_dest_row
     <'> Fgtk_tree_view_set_enable_search <'> Fgtk_tree_view_set_expander_column
     <'> Fgtk_tree_view_set_fixed_height_mode
     <'> Fgtk_tree_view_set_headers_clickable <'> Fgtk_tree_view_set_headers_visible
     <'> Fgtk_tree_view_set_hover_expand <'> Fgtk_tree_view_set_hover_selection
     <'> Fgtk_tree_view_set_model <'> Fgtk_tree_view_set_reorderable
     <'> Fgtk_tree_view_set_row_separator_func <'> Fgtk_tree_view_set_rules_hint
     <'> Fgtk_tree_view_set_search_column <'> Fgtk_tree_view_set_search_equal_func
     <'> Fgtk_tree_view_unset_rows_drag_dest
     <'> Fgtk_tree_view_unset_rows_drag_source <'> Fgtk_true <'> Fgtk_ui_manager_add_ui
     <'> Fgtk_ui_manager_add_ui_from_file <'> Fgtk_ui_manager_add_ui_from_string
     <'> Fgtk_ui_manager_ensure_update <'> Fgtk_ui_manager_get_accel_group
     <'> Fgtk_ui_manager_get_action <'> Fgtk_ui_manager_get_action_groups
     <'> Fgtk_ui_manager_get_add_tearoffs <'> Fgtk_ui_manager_get_ui
     <'> Fgtk_ui_manager_get_widget <'> Fgtk_ui_manager_insert_action_group
     <'> Fgtk_ui_manager_new <'> Fgtk_ui_manager_new_merge_id
     <'> Fgtk_ui_manager_remove_action_group <'> Fgtk_ui_manager_remove_ui
     <'> Fgtk_ui_manager_set_add_tearoffs <'> Fgtk_vbox_new <'> Fgtk_vbutton_box_new
     <'> Fgtk_viewport_get_shadow_type
     <'> Fgtk_viewport_new
     <'> Fgtk_viewport_set_shadow_type
     <'> Fgtk_vpaned_new
     <'> Fgtk_vruler_new <'> Fgtk_vscale_new <'> Fgtk_vscale_new_with_range <'> Fgtk_vscrollbar_new
     <'> Fgtk_vseparator_new <'> Fgtk_widget_activate <'> Fgtk_widget_add_accelerator
     <'> Fgtk_widget_add_events <'> Fgtk_widget_add_mnemonic_label
     <'> Fgtk_widget_can_activate_accel <'> Fgtk_widget_child_focus <'> Fgtk_widget_child_notify
     <'> Fgtk_widget_class_path <'> Fgtk_widget_create_pango_context
     <'> Fgtk_widget_create_pango_layout <'> Fgtk_widget_destroy <'> Fgtk_widget_destroyed
     <'> Fgtk_widget_ensure_style <'> Fgtk_widget_event <'> Fgtk_widget_freeze_child_notify
     <'> Fgtk_widget_get_accessible <'> Fgtk_widget_get_ancestor
     <'> Fgtk_widget_get_child_visible <'> Fgtk_widget_get_clipboard
     <'> Fgtk_widget_get_composite_name <'> Fgtk_widget_get_default_direction
     <'> Fgtk_widget_get_default_style <'> Fgtk_widget_get_direction <'> Fgtk_widget_get_display
     <'> Fgtk_widget_get_events <'> Fgtk_widget_get_modifier_style <'> Fgtk_widget_get_name
     <'> Fgtk_widget_get_no_show_all <'> Fgtk_widget_get_pango_context <'> Fgtk_widget_get_parent
     <'> Fgtk_widget_get_parent_window <'> Fgtk_widget_get_pointer <'> Fgtk_widget_get_root_window
     <'> Fgtk_widget_get_screen <'> Fgtk_widget_get_size_request <'> Fgtk_widget_get_style
     <'> Fgtk_widget_get_toplevel <'> Fgtk_widget_get_visual <'> Fgtk_widget_grab_default
     <'> Fgtk_widget_grab_focus <'> Fgtk_widget_has_screen <'> Fgtk_widget_hide
     <'> Fgtk_widget_hide_all <'> Fgtk_widget_hide_on_delete <'> Fgtk_widget_intersect
     <'> Fgtk_widget_is_ancestor <'> Fgtk_widget_is_focus <'> Fgtk_widget_list_accel_closures
     <'> Fgtk_widget_list_mnemonic_labels <'> Fgtk_widget_map <'> Fgtk_widget_mnemonic_activate
     <'> Fgtk_widget_modify_base <'> Fgtk_widget_modify_bg <'> Fgtk_widget_modify_fg
     <'> Fgtk_widget_modify_font <'> Fgtk_widget_modify_style <'> Fgtk_widget_modify_text
     <'> Fgtk_widget_path <'> Fgtk_widget_pop_composite_child <'> Fgtk_widget_push_composite_child
     <'> Fgtk_widget_queue_draw <'> Fgtk_widget_queue_draw_area <'> Fgtk_widget_queue_resize
     <'> Fgtk_widget_queue_resize_no_redraw <'> Fgtk_widget_realize
     <'> Fgtk_widget_remove_accelerator <'> Fgtk_widget_remove_mnemonic_label
     <'> Fgtk_widget_render_icon <'> Fgtk_widget_reparent <'> Fgtk_widget_reset_rc_styles
     <'> Fgtk_widget_reset_shapes <'> Fgtk_widget_send_expose <'> Fgtk_widget_set_accel_path
     <'> Fgtk_widget_set_app_paintable <'> Fgtk_widget_set_child_visible
     <'> Fgtk_widget_set_composite_name <'> Fgtk_widget_set_default_direction
     <'> Fgtk_widget_set_direction <'> Fgtk_widget_set_double_buffered <'> Fgtk_widget_set_events
     <'> Fgtk_widget_set_name <'> Fgtk_widget_set_no_show_all <'> Fgtk_widget_set_parent
     <'> Fgtk_widget_set_parent_window <'> Fgtk_widget_set_redraw_on_allocate
     <'> Fgtk_widget_set_sensitive
     <'> Fgtk_widget_set_size_request <'> Fgtk_widget_set_state <'> Fgtk_widget_set_style
     <'> Fgtk_widget_show <'> Fgtk_widget_show_all <'> Fgtk_widget_show_now
     <'> Fgtk_widget_size_allocate <'> Fgtk_widget_thaw_child_notify
     <'> Fgtk_widget_translate_coordinates <'> Fgtk_widget_unmap <'> Fgtk_widget_unparent
     <'> Fgtk_widget_unrealize <'> Fgtk_window_activate_default <'> Fgtk_window_activate_focus
     <'> Fgtk_window_activate_key <'> Fgtk_window_add_accel_group <'> Fgtk_window_add_embedded_xid
     <'> Fgtk_window_add_mnemonic <'> Fgtk_window_begin_move_drag <'> Fgtk_window_begin_resize_drag
     <'> Fgtk_window_deiconify <'> Fgtk_window_get_accept_focus <'> Fgtk_window_get_decorated
     <'> Fgtk_window_get_default_icon_list <'> Fgtk_window_get_default_size
     <'> Fgtk_window_get_destroy_with_parent <'> Fgtk_window_get_focus
     <'> Fgtk_window_get_focus_on_map <'> Fgtk_window_get_frame_dimensions
     <'> Fgtk_window_get_gravity <'> Fgtk_window_get_has_frame <'> Fgtk_window_get_icon 
     <'> Fgtk_window_get_icon_list <'> Fgtk_window_get_icon_name
     <'> Fgtk_window_get_mnemonic_modifier
     <'> Fgtk_window_get_modal <'> Fgtk_window_get_position <'> Fgtk_window_get_resizable
     <'> Fgtk_window_get_role <'> Fgtk_window_get_size <'> Fgtk_window_get_title
     <'> Fgtk_window_get_transient_for <'> Fgtk_window_has_toplevel_focus <'> Fgtk_window_iconify
     <'> Fgtk_window_is_active <'> Fgtk_window_list_toplevels <'> Fgtk_window_maximize
     <'> Fgtk_window_mnemonic_activate <'> Fgtk_window_move <'> Fgtk_window_new
     <'> Fgtk_window_parse_geometry <'> Fgtk_window_present <'> Fgtk_window_propagate_key_event
     <'> Fgtk_window_remove_accel_group <'> Fgtk_window_remove_embedded_xid
     <'> Fgtk_window_remove_mnemonic <'> Fgtk_window_reshow_with_initial_size
     <'> Fgtk_window_resize <'> Fgtk_window_set_accept_focus
     <'> Fgtk_window_set_auto_startup_notification <'> Fgtk_window_set_decorated
     <'> Fgtk_window_set_default <'> Fgtk_window_set_default_icon
     <'> Fgtk_window_set_default_icon_list <'> Fgtk_window_set_default_icon_name
     <'> Fgtk_window_set_default_size <'> Fgtk_window_set_destroy_with_parent
     <'> Fgtk_window_set_focus <'> Fgtk_window_set_focus_on_map
     <'> Fgtk_window_set_frame_dimensions <'> Fgtk_window_set_geometry_hints
     <'> Fgtk_window_set_gravity <'> Fgtk_window_set_has_frame <'> Fgtk_window_set_icon
     <'> Fgtk_window_set_icon_list <'> Fgtk_window_set_icon_name <'> Fgtk_window_set_keep_above
     <'> Fgtk_window_set_keep_below <'> Fgtk_window_set_mnemonic_modifier
     <'> Fgtk_window_set_modal <'> Fgtk_window_set_position <'> Fgtk_window_set_resizable
     <'> Fgtk_window_set_role <'> Fgtk_window_set_title <'> Fgtk_window_set_transient_for
     <'> Fgtk_window_set_type_hint <'> Fgtk_window_set_wmclass <'> Fgtk_window_stick
     <'> Fgtk_window_unmaximize <'> Fgtk_window_unstick <'> Fpango_attr_background_new
     <'> Fpango_attr_fallback_new <'> Fpango_attr_family_new <'> Fpango_attr_font_desc_new
     <'> Fpango_attr_foreground_new <'> Fpango_attr_iterator_copy <'> Fpango_attr_iterator_destroy
     <'> Fpango_attr_iterator_get <'> Fpango_attr_iterator_get_attrs
     <'> Fpango_attr_iterator_get_font
     <'> Fpango_attr_iterator_next <'> Fpango_attr_iterator_range <'> Fpango_attr_language_new
     <'> Fpango_attr_letter_spacing_new <'> Fpango_attr_list_change <'> Fpango_attr_list_copy
     <'> Fpango_attr_list_filter <'> Fpango_attr_list_get_iterator <'> Fpango_attr_list_insert
     <'> Fpango_attr_list_insert_before <'> Fpango_attr_list_new <'> Fpango_attr_list_splice
     <'> Fpango_attr_list_unref <'> Fpango_attr_rise_new <'> Fpango_attr_scale_new
     <'> Fpango_attr_shape_new <'> Fpango_attr_size_new <'> Fpango_attr_stretch_new
     <'> Fpango_attr_strikethrough_color_new <'> Fpango_attr_strikethrough_new
     <'> Fpango_attr_style_new <'> Fpango_attr_type_register <'> Fpango_attr_underline_color_new
     <'> Fpango_attr_underline_new <'> Fpango_attr_variant_new <'> Fpango_attr_weight_new
     <'> Fpango_attribute_copy <'> Fpango_attribute_destroy <'> Fpango_attribute_equal
     <'> Fpango_break <'> Fpango_color_copy <'> Fpango_color_free <'> Fpango_color_parse
     <'> Fpango_context_get_base_dir <'> Fpango_context_get_font_description
     <'> Fpango_context_get_language <'> Fpango_context_get_metrics
     <'> Fpango_context_list_families <'> Fpango_context_load_font <'> Fpango_context_load_fontset
     <'> Fpango_context_set_base_dir <'> Fpango_context_set_font_description
     <'> Fpango_context_set_language <'> Fpango_coverage_copy <'> Fpango_coverage_get
     <'> Fpango_coverage_max <'> Fpango_coverage_new <'> Fpango_coverage_ref
     <'> Fpango_coverage_set
     <'> Fpango_coverage_to_bytes <'> Fpango_coverage_unref <'> Fpango_font_describe
     <'> Fpango_font_description_better_match <'> Fpango_font_description_copy
     <'> Fpango_font_description_copy_static <'> Fpango_font_description_equal
     <'> Fpango_font_description_free <'> Fpango_font_description_from_string
     <'> Fpango_font_description_get_family <'> Fpango_font_description_get_set_fields
     <'> Fpango_font_description_get_size <'> Fpango_font_description_get_stretch
     <'> Fpango_font_description_get_style <'> Fpango_font_description_get_variant
     <'> Fpango_font_description_get_weight <'> Fpango_font_description_hash
     <'> Fpango_font_description_merge <'> Fpango_font_description_merge_static
     <'> Fpango_font_description_new <'> Fpango_font_description_set_family
     <'> Fpango_font_description_set_family_static <'> Fpango_font_description_set_size
     <'> Fpango_font_description_set_stretch <'> Fpango_font_description_set_style
     <'> Fpango_font_description_set_variant <'> Fpango_font_description_set_weight
     <'> Fpango_font_description_to_filename <'> Fpango_font_description_to_string
     <'> Fpango_font_description_unset_fields <'> Fpango_font_descriptions_free
     <'> Fpango_font_face_describe <'> Fpango_font_face_get_face_name
     <'> Fpango_font_face_list_sizes <'> Fpango_font_family_get_name
     <'> Fpango_font_family_is_monospace <'> Fpango_font_family_list_faces
     <'> Fpango_font_get_coverage <'> Fpango_font_get_glyph_extents
     <'> Fpango_font_get_metrics <'> Fpango_font_map_list_families
     <'> Fpango_font_map_load_font <'> Fpango_font_map_load_fontset
     <'> Fpango_font_metrics_get_approximate_char_width
     <'> Fpango_font_metrics_get_approximate_digit_width
     <'> Fpango_font_metrics_get_ascent <'> Fpango_font_metrics_get_descent
     <'> Fpango_font_metrics_get_strikethrough_position
     <'> Fpango_font_metrics_get_strikethrough_thickness
     <'> Fpango_font_metrics_get_underline_position
     <'> Fpango_font_metrics_get_underline_thickness <'> Fpango_font_metrics_ref
     <'> Fpango_font_metrics_unref <'> Fpango_get_log_attrs <'> Fpango_glyph_string_copy
     <'> Fpango_glyph_string_extents <'> Fpango_glyph_string_extents_range
     <'> Fpango_glyph_string_free <'> Fpango_glyph_string_get_logical_widths
     <'> Fpango_glyph_string_index_to_x <'> Fpango_glyph_string_new
     <'> Fpango_glyph_string_set_size <'> Fpango_glyph_string_x_to_index
     <'> Fpango_item_copy <'> Fpango_item_free <'> Fpango_item_new <'> Fpango_item_split
     <'> Fpango_itemize <'> Fpango_language_matches <'> Fpango_layout_context_changed
     <'> Fpango_layout_copy <'> Fpango_layout_get_alignment <'> Fpango_layout_get_attributes
     <'> Fpango_layout_get_auto_dir <'> Fpango_layout_get_context <'> Fpango_layout_get_cursor_pos
     <'> Fpango_layout_get_extents <'> Fpango_layout_get_indent <'> Fpango_layout_get_iter
     <'> Fpango_layout_get_justify <'> Fpango_layout_get_line <'> Fpango_layout_get_line_count
     <'> Fpango_layout_get_lines <'> Fpango_layout_get_log_attrs
     <'> Fpango_layout_get_pixel_extents
     <'> Fpango_layout_get_pixel_size <'> Fpango_layout_get_single_paragraph_mode
     <'> Fpango_layout_get_size <'> Fpango_layout_get_spacing <'> Fpango_layout_get_tabs
     <'> Fpango_layout_get_text <'> Fpango_layout_get_width <'> Fpango_layout_get_wrap
     <'> Fpango_layout_index_to_pos <'> Fpango_layout_iter_at_last_line <'> Fpango_layout_iter_free
     <'> Fpango_layout_iter_get_baseline <'> Fpango_layout_iter_get_char_extents
     <'> Fpango_layout_iter_get_cluster_extents <'> Fpango_layout_iter_get_index
     <'> Fpango_layout_iter_get_layout_extents <'> Fpango_layout_iter_get_line
     <'> Fpango_layout_iter_get_line_extents <'> Fpango_layout_iter_get_line_yrange
     <'> Fpango_layout_iter_get_run <'> Fpango_layout_iter_get_run_extents
     <'> Fpango_layout_iter_next_char <'> Fpango_layout_iter_next_cluster
     <'> Fpango_layout_iter_next_line <'> Fpango_layout_iter_next_run
     <'> Fpango_layout_line_get_extents <'> Fpango_layout_line_get_pixel_extents
     <'> Fpango_layout_line_get_x_ranges <'> Fpango_layout_line_index_to_x
     <'> Fpango_layout_line_x_to_index <'> Fpango_layout_move_cursor_visually
     <'> Fpango_layout_new <'> Fpango_layout_set_alignment <'> Fpango_layout_set_attributes
     <'> Fpango_layout_set_auto_dir <'> Fpango_layout_set_font_description
     <'> Fpango_layout_set_indent <'> Fpango_layout_set_justify <'> Fpango_layout_set_markup
     <'> Fpango_layout_set_markup_with_accel <'> Fpango_layout_set_single_paragraph_mode
     <'> Fpango_layout_set_spacing <'> Fpango_layout_set_tabs <'> Fpango_layout_set_text
     <'> Fpango_layout_set_width <'> Fpango_layout_set_wrap <'> Fpango_layout_xy_to_index
     <'> Fpango_parse_markup <'> Fpango_renderer_deactivate
     <'> Fpango_renderer_draw_error_underline
     <'> Fpango_script_iter_free <'> Fpango_script_iter_get_range <'> Fpango_script_iter_next
     <'> FGDK_COLORMAP <'> FGDK_IS_COLORMAP <'> FG_OBJECT_TYPE <'> Fgdk_colormap_alloc_color
     <'> Fgdk_colormap_alloc_colors <'> Fgdk_colormap_get_system <'> Fgdk_colormap_get_visual
     <'> Fgdk_colormap_new <'> Fgdk_drawable_get_depth <'> Fgdk_drawable_get_size
     <'> Fgdk_drawable_get_visual <'> Fgdk_drawable_set_colormap <'> Fgdk_pixbuf_get_from_drawable
     <'> Fgdk_pixbuf_render_pixmap_and_mask <'> Fgdk_pixbuf_render_pixmap_and_mask_for_colormap
     <'> Fgdk_pixbuf_render_threshold_alpha <'> Fgdk_screen_get_default_colormap
     <'> Fgdk_screen_get_system_colormap <'> Fgdk_screen_set_default_colormap
     <'> Fgdk_window_clear <'> Fgdk_window_clear_area <'> Fgdk_window_clear_area_e
     <'> Fgdk_window_get_internal_paint_info <'> Fgdk_window_set_back_pixmap
     <'> Fgdk_window_set_icon
     <'> Fgdk_window_shape_combine_mask <'> Fgtk_binding_set_activate <'> Fgtk_bindings_activate
     <'> Fgtk_cell_renderer_render <'> Fgtk_cell_view_get_size_of_row <'> Fgtk_drag_set_icon_pixmap
     <'> Fgtk_drag_source_set_icon <'> Fgtk_paint_arrow <'> Fgtk_paint_box <'> Fgtk_paint_box_gap
     <'> Fgtk_paint_check <'> Fgtk_paint_diamond <'> Fgtk_paint_extension <'> Fgtk_paint_flat_box
     <'> Fgtk_paint_focus <'> Fgtk_paint_handle <'> Fgtk_paint_hline <'> Fgtk_paint_layout
     <'> Fgtk_paint_option <'> Fgtk_paint_resize_grip <'> Fgtk_paint_shadow
     <'> Fgtk_paint_shadow_gap
     <'> Fgtk_paint_slider <'> Fgtk_paint_tab <'> Fgtk_paint_vline <'> Fgtk_requisition_copy
     <'> Fgtk_requisition_free <'> Fgtk_ruler_draw_pos <'> Fgtk_ruler_draw_ticks
     <'> Fgtk_style_apply_default_background <'> Fgtk_tree_view_create_row_drag_icon
     <'> Fgtk_widget_get_child_requisition <'> Fgtk_widget_get_colormap
     <'> Fgtk_widget_get_default_colormap <'> Fgtk_widget_get_default_visual
     <'> Fgtk_widget_pop_colormap <'> Fgtk_widget_push_colormap <'> Fgtk_widget_set_colormap
     <'> Fgtk_widget_set_default_colormap <'> Fgtk_widget_shape_combine_mask
     <'> Fgtk_widget_size_request <'> Fgdk_drawable_get_colormap
     <'> Fpango_shape ) constant gtk-procs-3

  \ FIXME missing functions

  \ <'> FGTK_COMBO_BOX_ENTRY
  \ <'> FGTK_IS_COMBO_BOX_ENTRY
  \ <'> Fgtk_accel_map_load_scanner
  \ <'> Fgtk_accelerator_get_default_mod_mask
  \ <'> Fgtk_combo_box_append_text
  \ <'> Fgtk_combo_box_entry_get_text_column
  \ <'> Fgtk_combo_box_entry_new
  \ <'> Fgtk_combo_box_entry_new_text
  \ <'> Fgtk_combo_box_entry_new_with_model
  \ <'> Fgtk_combo_box_entry_set_text_column
  \ <'> Fgtk_combo_box_get_active_text
  \ <'> Fgtk_combo_box_insert_text
  \ <'> Fgtk_combo_box_new_text
  \ <'> Fgtk_combo_box_prepend_text
  \ <'> Fgtk_combo_box_remove_text
  \ <'> Fgtk_init_add
  \ <'> Fgtk_paint_expander ( special )
  \ <'> Fgtk_quit_add
  \ <'> Fgtk_quit_add_destroy
  \ <'> Fgtk_quit_remove
  \ <'> Fgtk_quit_remove_by_data
  \ <'> Fgtk_statusbar_get_has_resize_grip
  \ <'> Fgtk_statusbar_set_has_resize_grip

  \ FIXME Wed Oct 27 22:08:17 CEST 2010
  
  \ <'> Fgtk_layout_get_hadjustment
  \ <'> Fgtk_layout_get_vadjustment
  \ <'> Fgtk_layout_put
  \ <'> Fgtk_layout_set_hadjustment
  \ <'> Fgtk_layout_set_vadjustment
  \ <'> Fgtk_tree_view_get_hadjustment
  \ <'> Fgtk_tree_view_get_vadjustment
  \ <'> Fgtk_tree_view_set_hadjustment
  \ <'> Fgtk_tree_view_set_vadjustment
  \ <'> Fgtk_viewport_get_hadjustment
  \ <'> Fgtk_viewport_get_vadjustment
  \ <'> Fgtk_viewport_set_hadjustment
  \ <'> Fgtk_viewport_set_vadjustment
  \ <'> Fgtk_widget_set_scroll_adjustments

  : 26-gtk ( -- )
    $" breakable gtk procs: %d" #( breakable-gtk-procs length ) snd-test-message
    $"           gtk procs: %d" #(
       gtk-procs-1 length gtk-procs-2 length + gtk-procs-3 length + ) snd-test-message
  ; 
[else]
  <'> noop alias 26-gtk
[then]

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

#( 1 2 3 )     constant color-95
440 make-oscil constant delay-32
0 make-array   constant vector-0
3 0.0 make-vct constant vct-3
5 0.0 make-vct constant vct-5

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
   <'> snd-spectrum <'> snd-tempnam <'> snd-version
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

: check-args-progress-info { msg -- } *snd-test-verbose* if msg #f snd-test-message then ;

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
      tag if
	tag car 'wrong-type-arg = unless
	  $" vct 0 wrong-type-arg %s [%s]: %s" #( prc arg tag ) snd-display
	then
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
	tag if
	  tag car 'wrong-type-arg       =
	  tag car 'wrong-number-of-args = ||
	  tag car 'mus-error            = || unless
	    $" vct 1 wrong-whatever %s [%s %s]: %s" #( prc arg1 arg2 tag ) snd-display
	  then
	then
      end-each
    end-each
  end-each
  args-2 each to arg
    vct-prcs-2 each to prc
      arg vct-3 prc #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'wrong-type-arg = unless
	  $" vct 1 wrong-whatever %s [%s]: %s" #( prc arg tag ) snd-display
	then
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
    tag if
      tag car 'no-active-selection = unless
	$" [0] selection %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> src-selection <'> filter-selection <'> env-selection ) each to prc
    0.0 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-active-selection = unless
	$" [1] selection %s: %s" #( prc tag ) snd-display
      then
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
    tag if
      tag car 'wrong-type-arg = unless
	$" mus-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  mus-snd-prcs-1 each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-number-of-args = unless
	$" no arg mus-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-sound-samples <'> mus-sound-frames <'> mus-sound-duration <'> mus-sound-datum-size
     <'> mus-sound-data-location <'> mus-sound-chans <'> mus-sound-srate <'> mus-sound-header-type
     <'> mus-sound-data-format <'> mus-sound-length <'> mus-sound-type-specifier
     <'> mus-sound-comment
     <'> mus-sound-write-date <'> mus-sound-maxamp <'> mus-sound-maxamp-exists? ) each to prc
    "/bad/baddy" prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'mus-error = unless
	$" bad file mus-sound %s: %s" #( prc tag ) snd-display
      then
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
    tag if
      tag car 'wrong-type-arg = unless
	$" chn (no chn) procs %s: %s" #( prc tag ) snd-display
      then
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
    tag if
      tag car 'no-such-sound = unless
	$" chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> delete-sample <'> edit-fragment <'> graph-style
     <'> redo <'> time-graph-style <'> lisp-graph-style
     <'> transform-graph-style <'> scale-by <'> scale-to <'> undo <'> x-axis-label ) { prcs-3 }
  prcs-3 each to prc
    0 1234 prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-sound = unless
	$" snd(1) chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  prcs-3 each to prc
    prc proc-name "x-axis-label" string= unless
      0 ind 1234 prc #t nil fth-catch to tag
      stack-reset
      tag if
	tag car 'no-such-channel = unless
	  $" snd(1 1234) chn procs %s: %s" #( prc tag ) snd-display
	then
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
    tag if
      tag car 'wrong-type-arg = unless
	$" [0] mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  mix-prcs each to prc
    1234 integer->mix prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-mix = unless
	$" [1] mix procs %s: %s" #( prc tag ) snd-display
      then
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
    tag if
      tag car 'wrong-type-arg = unless
	$" mark procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mark-name <'> mark-sample <'> mark-sync <'> mark-home <'> delete-mark ) each to prc
    1234 integer->mark prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-mark = unless
	$" no mark procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  0 ind 0 add-mark to id
  #( <'> mark-name <'> mark-sample <'> mark-sync ) each to prc
    id vct-5 prc set-xt #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'wrong-type-arg = unless
	$" set mark procs %s: %s" #( prc tag ) snd-display
      then
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
      tag if
	tag car 'wrong-type-arg = unless
	  $" region procs %s [%s]: %s" #( prc arg tag ) snd-display
	then
      then
    end-each
  end-each
  reg-prcs-1 each to prc
    1234 integer->region prc #t nil fth-catch to tag
    stack-reset
    tag if
      tag car 'no-such-region = unless
	$" (no) region procs %s: %s" #( prc tag ) snd-display
      then
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
    tag if
      tag car 'bad-arity      =	\ FTH special 'bad-arity (add-hook!) [ms]
      tag car 'wrong-type-arg = || unless
	$" [1] hooks %s: %s" #( hook hook-name tag ) snd-display
      then
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
  3 vct( 0.1 0.2 0.3 ) 0.0   <'> make-delay             'out-of-range     check-error-tag
  3 vct( 0.1 0.2 0.3 ) :max-size 100 <'> make-delay     'out-of-range     check-error-tag
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
  tag if
    tag car 'cant-update-file = unless
      $" update-sound after deletion: %s" #( tag ) snd-display
    then
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
  tag if
    tag car 'cannot-save = unless
      $" save protected sound msg: %s" #( tag ) snd-display
    then
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
  tag if
    tag car 'cannot-save = unless
      $" save-as write-protected sound msg: %s" #( tag ) snd-display
    then
  then
  ind close-sound drop
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  \ 
  close-sound-mc-cb <'> map-channel #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-channel = unless
      $" map-channel closing own chan: %s" #( tag ) snd-display
    then
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
  tag if
    tag car 'no-such-sound = unless
      $" revert-sound of closed sound: %s" #( tag ) snd-display
    then
  then
  \
  "oboe.snd" open-sound to ind
  100 0.5 ind 0 set-sample drop
  0.5 0 100 ind 0 ind edpos-1-cb <'> scale-channel #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'bad-arity = unless
      $" edpos proc bad args: %s" #( tag ) snd-display
    then
  then
  ind sound? unless $" edpos bad arity proc clobbers chan??: %s" #( ind ) snd-display then
  \ 
  0.5 0 100 ind 0 <'> edpos-2-cb <'> scale-channel #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-channel = unless
      $" edpos clobbers channel: %s" #( tag ) snd-display
    then
  then
  ind sound? if $" edpos proc clobbers chan??: %s" #( ind ) snd-display then
  set-procs04 length 2 = if
    $" (%s %s)" set-procs04
  else
    "%s" #( set-procs04 )
  then string-format { set04fncs }
  procs10 length 2 = if
    $" (%s %s)" procs10
  else
    "%s" #( procs10 )
  then string-format { 10fncs }
  *snd-test-verbose* if
    $" procs   prcs/set-prcs" #f snd-test-message
    $" =====================" #f snd-test-message
    $" procs00: %3d/%3d" #( procs00 length set-procs00 length ) snd-test-message
    $" procs01: %3d/%3d" #( procs01 length set-procs01 length ) snd-test-message
    $" procs02: %3d/%3d" #( procs02 length set-procs02 length ) snd-test-message
    $" procs03: %3d/%3d" #( procs03 length set-procs03 length ) snd-test-message
    $" procs04: %3d/%3d %s" #( procs04 length set-procs04 length set04fncs ) snd-test-message
    $" procs05: %3d"     #( procs05 length )                    snd-test-message
    $" procs06: %3d"     #( procs06 length )                    snd-test-message
    $" procs07: %3d"     #( procs07 length )                    snd-test-message
    $" procs08: %3d"     #( procs08 length )                    snd-test-message
    $" procs10: %3d %s"  #( procs10 length 10fncs )             snd-test-message
  then
  #( 1.5 #( 0 1 ) 1234 #t )                     { random-args }
  #( 1.5 #( 0 1 ) 1234 vct-3 color-95 -1.0 csqrt delay-32 :feedback #f ) { main-args }
  #( 1.5 #( 0 1 ) 1234 -1.0 csqrt delay-32 #t ) { few-args }
  #( 1.5 vct-3 -1.0 csqrt )                     { fewer-args }
  all-args if main-args else few-args then      { less-args }
  nil nil nil nil nil nil nil { arg1 arg2 arg3 arg4 tm prc tag }
  gc-run
  "keyargs-2-args" check-args-progress-info
  keyargs each to arg1
    random-args each to arg2
      make-procs each to prc
	arg1 arg2 prc #t nil fth-catch stack-reset
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  all-args if
    "keyargs-3-args" check-args-progress-info
    random-args each to arg1
      keyargs each to arg2
	random-args each to arg3
	  make-procs each to prc
	    arg1 arg2 arg3 prc #t nil fth-catch stack-reset
	  end-each
	end-each
      end-each
    end-each
    dismiss-all-dialogs
    gc-run
    "keyargs-4-args" check-args-progress-info
    keyargs each to arg1
      random-args each to arg2
	keyargs each to arg3
	  random-args each to arg4
	    make-procs each to prc
	      arg1 arg2 arg3 arg4 prc #t nil fth-catch stack-reset
	    end-each
	  end-each
	end-each
      end-each
    end-each
    dismiss-all-dialogs
    gc-run
  then
  "no-args" check-args-progress-info
  procs00 each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-number-of-args = if
      $" procs00: %s %s" #( prc tag ) snd-display
    then
  end-each
  dismiss-all-dialogs
  gc-run
  "set-no-args" check-args-progress-info
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
  gc-run
  "1-arg" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "set-1-arg" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "2-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "set-2-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  nil nil nil nil nil nil { arg5 arg6 arg7 arg8 arg9 arg0 }
  "3-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "set-3-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "4-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "set-4-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "5-args" check-args-progress-info
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
  dismiss-all-dialogs
  gc-run
  "6-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
	fewer-args each to arg4
	  fewer-args each to arg5
	    fewer-args each to arg6
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
  dismiss-all-dialogs
  gc-run
  "8-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
	fewer-args each to arg4
	  fewer-args each to arg5
	    fewer-args each to arg6
	      fewer-args each to arg7
		fewer-args each to arg8
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
  dismiss-all-dialogs
  gc-run
  "10-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
	fewer-args each to arg4
	  fewer-args each to arg5
	    fewer-args each to arg6
	      fewer-args each to arg7
		fewer-args each to arg8
		  fewer-args each to arg9
		    fewer-args each to arg0
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
  dismiss-all-dialogs
  gc-run
;

: 30-test
  #( 1.5 #( 0 1 ) 1234 #t )                     { random-args }
  #( 1.5 #( 0 1 ) 1234 vct-3 color-95 -1.0 csqrt delay-32 :feedback #f ) { main-args }
  #( 1.5 #( 0 1 ) 1234 -1.0 csqrt delay-32 #t ) { few-args }
  #( 1.5 vct-3 -1.0 csqrt )                     { fewer-args }
  fewer-args { less-args }
  fewer-args to random-args
  fewer-args to few-args
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  { ind prc tag arg arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg0 tm }
  make-timer to tm
  0.0 0.3 <'> anoi-test \ clm-ins-test
  \ <'> sndclm-src2-test
  :comment over object->string
  :statistics #t
  :verbose    #f
  :play       #t
  :srate      22050
  :channels 2 with-sound ws-close-sound
  tm stop-timer
  \ "%s" #( tm ) snd-test-message
;

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
  <'> 26-gtk             run-fth-test
  <'> 28-errors          run-fth-test
  <'> 30-test            run-fth-test	\ local fragment test
  finish-snd-test
  0 snd-exit drop
;let

\ snd-test.fs ends here
