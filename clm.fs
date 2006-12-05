\ clm.fs -- clm related base words -*- snd-forth -*-

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Mon Mar 15 19:25:58 CET 2004
\ Changed: Tue Dec 05 01:19:00 CET 2006

\ Commentary:
\
\ clm-print            ( fmt args -- )
\ clm-message          ( fmt args -- )
\ 
\ tempnam              ( -- name )
\ 
\ now@   	       ( -- secs )
\ now!   	       ( secs -- )
\ tempo@ 	       ( -- secs )
\ tempo! 	       ( secs -- )
\
\ interval->hertz      ( n -- r )
\ keynum->hertz        ( n -- r )
\ hertz->keynum        ( n -- r )
\ bpm->seconds         ( bpm -- secs )
\ rhythm->seconds      ( rhy -- secs )
\
\ make-default-comment ( -- str )
\ times->samples       ( start dur -- len beg )
\ normalize-partials   ( parts -- parts' )
\ run                  ( start dur -- )
\ run-instrument       ( start dur args -- )
\ end-run              ( value -- )
\ reverb-info          ( caller in-chans out-chans -- )
\ instrument:          ( ?? -- )
\ ;instrument          ( -- )
\ event:               ( ?? -- )
\ ;event               ( -- )
\
\ get-args             ( key val1 -- val2 )
\ find-file            ( file -- fname|#f )
\ snd-info             ( fname revname chans srate frames scl? tm -- )
\ play-sound           ( file -- )
\ record-sound         ( file dur -- )
\
\ with-sound           ( body-xt keyword-args -- ws )
\ clm-load             ( keyword-args fname -- ws )
\ scaled-to            ( body-xt keyword-args scl -- )
\ scaled-by            ( body-xt keyword-args scl -- )
\ with-offset          ( body-xt keyword-args secs -- )
\ with-mix             ( body-str args fname beg -- )
\ sound-let            ( ws-xt-lst body-xt -- )

\ Code:

\ defined in snd/snd-xen.c
[undefined] clm-print [if] ' fth-print alias clm-print [then]

[undefined] clm-message [if]
  'snd provided? not 'snd-nogui provided? || [if]
    \ Prints to stdout through snd-print.
    : clm-message ( fmt args -- ) $" \\ " rot ( fmt ) $+ $" \n" $+ swap ( args ) clm-print ;
  [else]
    \ Prints to Snd's listener (snd-print) and to stdout if $EMACS is
    \ set or $TERM matches /^xterm/.
    : clm-message ( fmt args -- )
      $" \n\\ " rot ( fmt ) $+ swap ( args ) string-format { msg }
      msg '() clm-print
      $" EMACS" getenv
      /^xterm/ $" TERM" getenv 1 >string re= || if msg .stdout then
    ;
  [then]
[then]

dl-load sndlib Init_sndlib

'snd provided? [unless]
  ' noop alias sound?
  ' noop alias open-sound
  ' noop alias find-sound
  ' noop alias update-sound
  ' noop alias save-sound
  ' noop alias close-sound
  ' noop alias channels
  ' noop alias play
  ' noop alias play-and-wait
  ' noop alias maxamp
  ' noop alias frames
  ' noop alias scale-channel
  ' noop alias snd-tempnam
  ' noop alias snd-version
[then]

hide
user *fth-file-number*
set-current
: tempnam ( -- name )
  doc" ( -- name )  Looks for environment variables TMP, TEMP, or TMPDIR, otherwise \
uses /tmp as temporary path and produces something like:\n\
/tmp/fth-12345-1.snd\n\
/tmp/fth-12345-2.snd\n\
/tmp/fth-12345-3.snd\n\
[...]"
  1 *fth-file-number* +!
  $" %s/fth-%d-%d.snd"
  $" TMP" getenv dup unless
    drop
    $" TEMP" getenv dup unless
      drop
      $" TMPDIR" getenv dup unless
	drop
	$" /tmp"
      then
    then
  then ( tmp ) getpid *fth-file-number* @  3 >list string-format
;
previous

: fth-tempnam ( -- fname )
  'snd provided? if
    snd-tempnam
  else
    tempnam
  then
;

\ === Notelist ===
hide
0.00 value *clm-current-time*
60.0 value *clm-tempo*
0.25 value *clm-beat*
set-current
: now@   ( -- secs ) *clm-current-time* ;
: now!   ( secs -- ) to *clm-current-time* ;
: step   ( secs -- ) now@ f+ now! ;
: tempo@ ( -- secs ) *clm-tempo* ;
: tempo! ( secs -- ) to *clm-tempo* ;
previous

\ --- Pitches ---
6.875 constant lowest-freq

: interval->hertz ( n -- r ) { n } 2.0 12.0 n 3.0 f+ f+ 12.0 f/ f** lowest-freq f* ;
: keynum->hertz   ( n -- r ) { n } 2.0 n 3.0 f+ 12.0 f/ f** lowest-freq f* ;
: hertz->keynum   ( r -- n ) lowest-freq f/ 2.0 flogn 12.0 f* 3.0 f- f>s ;

hide
: pitch ( interval octave "name" --; self -- freq )
  { interval octave }
  2.0 octave 1.0 f+ 12.0 f* interval 3.0 f+ f+ 12.0 f/ f** lowest-freq f*
  create ,
 does> ( self -- freq )
  @
;
set-current
 0 0 pitch |C0    1 0 pitch |Cs0    1 0 pitch |Df0
 2 0 pitch |D0    3 0 pitch |Ds0    3 0 pitch |Ef0
 4 0 pitch |E0    4 0 pitch |Ff0    5 0 pitch |Es0
 5 0 pitch |F0    6 0 pitch |Fs0    6 0 pitch |Gf0
 7 0 pitch |G0    8 0 pitch |Gs0    8 0 pitch |Af0
 9 0 pitch |A0   10 0 pitch |As0   10 0 pitch |Bf0
11 0 pitch |B0   11 0 pitch |Cf0   12 0 pitch |Bs0

 0 1 pitch |C1    1 1 pitch |Cs1    1 1 pitch |Df1
 2 1 pitch |D1    3 1 pitch |Ds1    3 1 pitch |Ef1
 4 1 pitch |E1    4 1 pitch |Ff1    5 1 pitch |Es1
 5 1 pitch |F1    6 1 pitch |Fs1    6 1 pitch |Gf1
 7 1 pitch |G1    8 1 pitch |Gs1    8 1 pitch |Af1
 9 1 pitch |A1   10 1 pitch |As1   10 1 pitch |Bf1
11 1 pitch |B1   11 1 pitch |Cf1   12 1 pitch |Bs1

 0 2 pitch |C2    1 2 pitch |Cs2    1 2 pitch |Df2
 2 2 pitch |D2    3 2 pitch |Ds2    3 2 pitch |Ef2
 4 2 pitch |E2    4 2 pitch |Ff2    5 2 pitch |Es2
 5 2 pitch |F2    6 2 pitch |Fs2    6 2 pitch |Gf2
 7 2 pitch |G2    8 2 pitch |Gs2    8 2 pitch |Af2
 9 2 pitch |A2   10 2 pitch |As2   10 2 pitch |Bf2
11 2 pitch |B2   11 2 pitch |Cf2   12 2 pitch |Bs2

 0 3 pitch |C3    1 3 pitch |Cs3    1 3 pitch |Df3
 2 3 pitch |D3    3 3 pitch |Ds3    3 3 pitch |Ef3
 4 3 pitch |E3    4 3 pitch |Ff3    5 3 pitch |Es3
 5 3 pitch |F3    6 3 pitch |Fs3    6 3 pitch |Gf3
 7 3 pitch |G3    8 3 pitch |Gs3    8 3 pitch |Af3
 9 3 pitch |A3   10 3 pitch |As3   10 3 pitch |Bf3
11 3 pitch |B3   11 3 pitch |Cf3   12 3 pitch |Bs3

 0 4 pitch |C4    1 4 pitch |Cs4    1 4 pitch |Df4
 2 4 pitch |D4    3 4 pitch |Ds4    3 4 pitch |Ef4
 4 4 pitch |E4    4 4 pitch |Ff4    5 4 pitch |Es4
 5 4 pitch |F4    6 4 pitch |Fs4    6 4 pitch |Gf4
 7 4 pitch |G4    8 4 pitch |Gs4    8 4 pitch |Af4
 9 4 pitch |A4   10 4 pitch |As4   10 4 pitch |Bf4
11 4 pitch |B4   11 4 pitch |Cf4   12 4 pitch |Bs4

 0 5 pitch |C5    1 5 pitch |Cs5    1 5 pitch |Df5
 2 5 pitch |D5    3 5 pitch |Ds5    3 5 pitch |Ef5
 4 5 pitch |E5    4 5 pitch |Ff5    5 5 pitch |Es5
 5 5 pitch |F5    6 5 pitch |Fs5    6 5 pitch |Gf5
 7 5 pitch |G5    8 5 pitch |Gs5    8 5 pitch |Af5
 9 5 pitch |A5   10 5 pitch |As5   10 5 pitch |Bf5
11 5 pitch |B5   11 5 pitch |Cf5   12 5 pitch |Bs5

 0 6 pitch |C6    1 6 pitch |Cs6    1 6 pitch |Df6
 2 6 pitch |D6    3 6 pitch |Ds6    3 6 pitch |Ef6
 4 6 pitch |E6    4 6 pitch |Ff6    5 6 pitch |Es6
 5 6 pitch |F6    6 6 pitch |Fs6    6 6 pitch |Gf6
 7 6 pitch |G6    8 6 pitch |Gs6    8 6 pitch |Af6
 9 6 pitch |A6   10 6 pitch |As6   10 6 pitch |Bf6
11 6 pitch |B6   11 6 pitch |Cf6   12 6 pitch |Bs6

 0 7 pitch |C7    1 7 pitch |Cs7    1 7 pitch |Df7
 2 7 pitch |D7    3 7 pitch |Ds7    3 7 pitch |Ef7
 4 7 pitch |E7    4 7 pitch |Ff7    5 7 pitch |Es7
 5 7 pitch |F7    6 7 pitch |Fs7    6 7 pitch |Gf7
 7 7 pitch |G7    8 7 pitch |Gs7    8 7 pitch |Af7
 9 7 pitch |A7   10 7 pitch |As7   10 7 pitch |Bf7
11 7 pitch |B7   11 7 pitch |Cf7   12 7 pitch |Bs7

 0 8 pitch |C8    1 8 pitch |Cs8    1 8 pitch |Df8
 2 8 pitch |D8    3 8 pitch |Ds8    3 8 pitch |Ef8
 4 8 pitch |E8    4 8 pitch |Ff8    5 8 pitch |Es8
 5 8 pitch |F8    6 8 pitch |Fs8    6 8 pitch |Gf8
 7 8 pitch |G8    8 8 pitch |Gs8    8 8 pitch |Af8
 9 8 pitch |A8   10 8 pitch |As8   10 8 pitch |Bf8
11 8 pitch |B8   11 8 pitch |Cf8   12 8 pitch |Bs8
previous

\ --- Note length ---
: bpm->seconds    ( bpm -- secs ) 60.0 swap f/ ;
: rhythm->seconds ( rhy -- secs ) 4.0 tempo@ bpm->seconds f* f* ;

: notelength ( scale "name" --; self -- r )
  rhythm->seconds create ,
 does> ( self -- r )
  @
;

 1.0     notelength |W			\ whole
 2.0 1/f notelength |H			\ half
 4.0 1/f notelength |Q			\ quarter
 8.0 1/f notelength |A			\ eighth
16.0 1/f notelength |S			\ sixteenth
32.0 1/f notelength |T			\ thirty-second
 1.0      2.0 1/f f+ notelength |W.
 2.0 1/f  4.0 1/f f+ notelength |H.
 4.0 1/f  8.0 1/f f+ notelength |Q.
 8.0 1/f 16.0 1/f f+ notelength |A.
16.0 1/f 32.0 1/f f+ notelength |S.

\ === Global User Variables (settable in ~/.snd_forth or ~/.fthrc) ===
$" fth 5-Dec-2006"  value *clm-version*
#f 		    value *output*
#f 		    value *reverb*
#f 		    value *locsig*
8                   value *clm-array-print-length*
mus-lshort          value *clm-audio-format*
#f                  value *clm-clipped*
#f                  value *clm-comment*
1.0                 value *clm-decay-time*
#f  		    value *clm-delete-reverb*
1024 64 *           value *clm-file-buffer-size*
$" test.snd"        value *clm-file-name*
#f 		    value *clm-notehook*
#f  		    value *clm-play*
#f                  value *clm-player*           
#f 		    value *clm-reverb*
1     		    value *clm-reverb-channels*
'()                 value *clm-reverb-data*
$" test.reverb"     value *clm-reverb-file-name*
#f  		    value *clm-statistics*
512   		    value *clm-table-size*
#f  		    value *clm-verbose*
#()                 value *clm-search-list* \ array of sound directories

'snd provided? [unless]
  1                 constant default-output-chans
  mus-lfloat        constant default-output-data-format
  mus-next          constant default-output-header-type
  mus-interp-linear constant locsig-type
  mus-audio-default constant audio-output-device
  512               constant dac-size
  22050             constant default-output-srate
[then]

default-output-chans       value *clm-channels*
default-output-data-format value *clm-data-format*
default-output-header-type value *clm-header-type*
locsig-type                value *clm-locsig-type*
audio-output-device        value *clm-output-device*
dac-size                   value *clm-rt-bufsize*
default-output-srate       value *clm-srate*

\ internal global variables
*clm-channels*      value *channels*
*clm-locsig-type*   value *locsig-type*
*clm-verbose*       value *verbose*
*clm-notehook*      value *notehook*

: make-default-comment ( -- str )
  $" Written %s by %s at %s using clm (%s)" _
  '( $" %a %d-%b-%y %H:%M %Z" current-time strftime
     getlogin
     gethostname
     *clm-version* ) string-format
;

: times->samples ( start dur -- len beg )
  { start dur }
  start seconds->samples { beg }
  dur seconds->samples { len }
  beg len b+ beg
;

: normalize-partials ( parts -- parts' )
  { parts }
  0.0
  parts length 1 ?do parts i object-ref fabs f+ 2 +loop
  dup f0= if
    $" all parts have 0.0 amplitude: %s" '( parts ) string-format warning
    drop
  else
    1/f { scl }
    parts length 1 ?do parts i scl object-set*! 2 +loop
  then
  parts
;

\ === With-Sound Run-Instrument ===
: run ( start dur -- ) postpone times->samples postpone ?do ; immediate

hide
: (make-locsig) ( start dur args -- )
  { start dur args }
  args hash? unless #{} to args then
  save-stack { s }
  :degree    args :degree   hash-ref            0.0 ||
  :distance  args :distance hash-ref            1.0 ||
  :reverb    args :reverb   hash-ref           0.05 ||
  :channels  args :channels hash-ref     *channels* ||
  :output    args :output   hash-ref       *output* ||
  :revout    args :revout   hash-ref       *reverb* ||
  :type      args :type     hash-ref  *locsig-type* || make-locsig to *locsig*
  s restore-stack
  \ we set channel 3/4 if any to 0.5 * channel 1/2
  *output* mus-output? if
    *output* mus-channels 2 > if
      *locsig* 2  *locsig* 0 locsig-ref f2/  locsig-set! drop
      *output* mus-channels 3 > if
	*locsig* 3  *locsig* 1 locsig-ref f2/  locsig-set! drop
      then
    then
  then
  *reverb* mus-output? if
    *reverb* mus-channels 2 > if
      *locsig* 2  *locsig* 0 locsig-reverb-ref f2/  locsig-reverb-set! drop
      *reverb* mus-channels 3 > if
	*locsig* 3  *locsig* 1 locsig-reverb-ref f2/  locsig-reverb-set! drop
      then
    then
  then
  start dur times->samples
;
set-current
: run-instrument ( start dur locsig-args -- )
  \ postpone (make-locsig) postpone times->samples postpone ?do
  postpone (make-locsig) postpone ?do
; immediate
: end-run ( value -- )
  postpone *locsig* postpone r@ postpone rot postpone locsig postpone drop postpone loop
; immediate
previous

: reverb-info ( caller in-chans out-chans -- )
  { caller in-chans out-chans }
  $" %s on %d in and %d out channels" _ '( caller in-chans out-chans ) clm-message
;

\ === Helper functions for instruments ===
hide
: ins-info ( ins-name -- ) *notehook* if *notehook* execute else drop then ;
: event-info ( ev-name -- )
  { ev-name }
  *verbose* if $" \tevent: %s" '( ev-name ) clm-message then
;
set-current
: instrument: ( ?? -- )
  >in @ parse-word $>string { ins-name } >in ! :
  ins-name postpone literal ['] ins-info compile,
;
: ;instrument ( -- ) postpone ; ; immediate
: event: ( ?? -- )
  >in @ parse-word $>string { ev-name } >in ! :
  ev-name postpone literal ['] event-info compile,
;
: ;event ( -- ) postpone ; ; immediate
previous

\ === Argument Scanning ===
: get-args ( key def -- val2 )
  swap { key }
  depth 0 ?do
    i 1+ pick keyword? if
      i 1+ pick key = if
	i roll drop		\ drop keyword
	i 1- roll		\ value on top of stack
	nip			\ drop default value
	leave
      then
    then
  loop
;

\ === Playing and Recording Sound Files ===
: find-file ( file -- fname|#f )
  { file }
  file file-exists? if
    file
  else
    #f					\ flag
    *clm-search-list* each { dir }
      dir $" /" $+ file $+ { fname }
      fname file-exists? if drop ( flag ) fname leave then
    end-each
  then
;

hide
: .maxamps ( fname name sr scl? -- )
  { fname name sr scl? }
  fname mus-sound-maxamp { vals }
  vals length 0 ?do
    $" %*s %c: %.3f (near %.3f secs)%s"
    '( 6 name
       [char] A i 2/ +
       vals i 1+ list-ref
       vals i list-ref sr f/
       scl? if $"  (before scaling)" else "" then ) clm-message
  2 +loop
;
: .timer ( obj -- )
  { obj }
  $" %*s: %.3f  (utime %.3f, stime %.3f)"
  '( 8 $" real" obj real-time@ obj user-time@ obj system-time@ ) clm-message
;
: .timer-ratio ( srate frames obj -- )
  { sr frms obj }
  frms 0> if
    sr frms f/ { m }
    $" %*s: %.2f  (uratio %.2f)"
    '( 8 $" ratio" obj real-time@ m f* obj user-time@ m f* ) clm-message
  else
    $" %*s: no ratio" '( 8 $" ratio" ) clm-message
  then
;
set-current

: snd-info ( output revfile chans sr frms scl? tm -- )
  { output revfile chans sr frms scl? tm }
  output mus-sound-duration { dur }
  $" filename: %S"                '( output )       clm-message
  $"    chans: %d, srate: %d"     '( chans sr f>s ) clm-message
  $"   format: %s [%s]"
  '( output mus-sound-data-format mus-data-format-name
     output mus-sound-header-type mus-header-type-name ) clm-message
  $"   length: %.3f  (%d frames)" '( dur frms )     clm-message
  tm timer? if
    tm .timer
    sr frms tm .timer-ratio
  then
  output $" maxamp" sr scl? .maxamps
  revfile ?dup-if $" revamp" sr scl? .maxamps then
  output mus-sound-comment { comm }
  comm empty? unless $"  comment: %s" '( comm ) clm-message then
;
previous

\ === Playing and Recording one or two Channel Sounds ===
: play-sound ( file -- )
  find-file { fname }
  fname unless 'no-such-file '( get-func-name fname ) fth-throw then
  fname mus-sound-frames { frms }
  fname mus-sound-srate { sr }
  fname mus-sound-chans { chans }
  chans 2 > if
    $" %s: we can only handle 2 chans, not %d" _ '( get-func-name chans ) string-format warning
    2 to chans
  then
  *clm-verbose* if fname #f chans sr frms #f #f snd-info then
  *clm-rt-bufsize* frms min { bufsize }
  bufsize 0> if
    chans bufsize make-sound-data { data }
    fname mus-sound-open-input { snd-fd }
    snd-fd 0< if 'forth-error '( get-func-name $" cannot open %s" _ fname ) fth-throw then
    mus-audio-default sr chans 2 min *clm-audio-format* bufsize mus-audio-open-output { dac-fd }
    dac-fd 0< if 'forth-error '( get-func-name $" cannot open dac" _ ) fth-throw then
    frms 0 ?do
      i bufsize + frms > if frms i - to bufsize then
      snd-fd 0 bufsize 1- chans data mus-sound-read drop
      dac-fd data bufsize mus-audio-write drop
    bufsize +loop
    snd-fd mus-sound-close-input drop
    dac-fd mus-audio-close drop
  else
    $" nothing to play for %S (%d frames)" '( fname bufsize ) string-format warning
  then
;

: record-sound ( fname dur -- )
  { fname dur }
  dur seconds->samples { frms }
  *clm-output-device* { device }
  *clm-rt-bufsize* frms min { bufsize }
  *clm-channels* 2 min { chans }
  *clm-srate* { sr }
  mus-srate { old-srate }
  sr set-mus-srate drop
  *clm-audio-format* { afmt }
  *clm-data-format* { dfmt }
  *clm-header-type* { htype }
  chans bufsize make-sound-data { data }
  chans 0.25 make-vct { vals }
  vals each drop mus-audio-mixer mus-audio-reclev i vals mus-audio-mixer-write drop end-each
  vals 0.75 vct-fill! drop
  vals each drop device  mus-audio-amp i vals mus-audio-mixer-write drop end-each
  $" written %s by %s" _ '( date get-func-name ) string-format { descr }
  fname sr chans dfmt htype descr mus-sound-open-output { snd-fd }
  snd-fd 0< if 'forth-error '( get-func-name $" cannot open %s" _ fname ) fth-throw then
  device sr chans 2 min afmt bufsize mus-audio-open-input { dac-fd }
  dac-fd 0< if 'forth-error '( get-func-name $" cannot open dac" _ ) fth-throw then
  *clm-verbose* if
    $" filename: %s"                '( fname )                     clm-message
    $"   device: %d"                '( device )                    clm-message
    $"    chans: %d, srate: %d"     '( chans sr )                  clm-message
    $" r format: %s"                '( afmt mus-data-format-name ) clm-message
    $" w format: %s [%s]" '( dfmt mus-data-format-name htype mus-header-type-name ) clm-message
    $"   length: %.3f  (%d frames)" '( dur frms )                  clm-message
    $"  comment: %s"                '( descr )                     clm-message
  then
  frms 0 ?do
    i bufsize + frms > if frms i - to bufsize then
    dac-fd data bufsize mus-audio-read drop
    snd-fd 0 bufsize 1- chans data mus-sound-write drop
  bufsize +loop
  dac-fd mus-audio-close drop
  snd-fd frms chans * dfmt mus-bytes-per-sample * mus-sound-close-output drop
  old-srate set-mus-srate drop
;

$" with-sound error" create-exception with-sound-error
#() value *ws-args*

: clm-mix ( keyword-args -- )
  :output      #f get-args { outfile }
  :filename    #f get-args { infile }
  :output-frame 0 get-args { outloc }
  :frames      #f get-args { frames }
  :input-frame  0 get-args { inloc }
  :scaler     1.0 get-args { scaler }
  0  { chans }
  #f { mx }
  *output* mus-output? { outgen }
  outfile false? if
    outgen if
      *output* mus-channels  to chans
      *output* mus-file-name to outfile
    else
      'with-sound-error '( get-func-name $" *output* gen or :output required" ) fth-throw
    then
  then
  frames infile mus-sound-frames || to frames
  outgen if *output* mus-close drop then
  chans 0>
  scaler f0<> and
  scaler 1.0 f<> and if
    save-stack { s }
    chans  chans dup * 0 ?do scaler loop make-mixer to mx
    s restore-stack
  then
  outfile ( outfile )
  infile  ( infile )
  outloc  ( outloc )
  frames  ( frames )
  inloc   ( inloc )
  mx      ( mixer )
  #f      ( envs )    mus-mix drop
  outgen if outfile continue-sample->file to *output* then
;

hide
: ws-get-snd ( ws -- snd )
  { ws }
  'snd provided? if
    ws :output hash-ref find-file { fname }
    fname 0 find-sound { snd }
    snd sound? 'snd-nogui provided? not && if
      snd update-sound channels 0 ?do drop loop
    else
      snd update-sound drop
    then
    snd
  else
    #f
  then
;
: ws-scaled-to ( ws -- )
  { ws }
  ws :scale-to hash-ref { scale }
  'snd provided? if
    ws ws-get-snd { snd }
    0.0  snd #t #f maxamp each fmax end-each { mx }
    mx f0<> if
      scale mx f/ { scl }
      snd #f #f frames { len }
      *channels* 0 ?do scl 0 len snd i ( chn ) #f scale-channel drop loop
    then
    snd save-sound drop
  else
    ws :output hash-ref mus-sound-maxamp { smax }
    0.0 smax length 1 ?do smax i list-ref fabs fmax 2 +loop { mx }
    mx f0<> if
      :filename ws :output hash-ref :scaler scale mx f/ clm-mix
    then
  then
;
: ws-scaled-by ( ws -- )
  { ws }
  ws :scale-to hash-ref { scale }
  'snd provided? if
    ws ws-get-snd { snd }
    snd #f #f frames { len }
    *channels* 0 ?do scale 0 len snd i ( chn ) #f scale-channel drop loop
    snd save-sound drop
  else
    :filename ws :output hash-ref :scaler scale clm-mix
  then
;
: ws-before-output ( ws -- )
  { ws }
  ws :continue-old-file hash-ref if
    ws :output hash-ref continue-sample->file
  else
    ws :comment     hash-ref unless ws :comment make-default-comment hash-set! then
    ws :output      hash-ref
    ws :channels    hash-ref
    ws :data-format hash-ref
    ws :header-type hash-ref
    ws :comment     hash-ref  make-sample->file
  then to *output*
  *output* sample->file? unless
    'with-sound-error '( get-func-name $" cannot open sample->file" _ ) fth-throw
  then
  ws :continue-old-file hash-ref if
    ws :output hash-ref mus-sound-srate set-mus-srate drop
    'snd provided? if ws :output hash-ref 0 find-sound dup sound? if close-sound then drop then
  then
  ws :reverb-file-name hash-ref if
    ws :continue-old-file hash-ref if
      ws :reverb-file-name hash-ref continue-sample->file
    else
      ws :reverb-file-name hash-ref
      ws :reverb-channels  hash-ref
      ws :data-format      hash-ref
      ws :header-type      hash-ref
      $" with-sound temporary reverb file" make-sample->file
    then to *reverb*
    *reverb* sample->file? unless
      'with-sound-error '( get-func-name $" cannot open reverb sample->file" _ ) fth-throw
    then
  then
  *clm-table-size*         set-clm-table-size         drop
  *clm-file-buffer-size*   set-mus-file-buffer-size   drop
  *clm-array-print-length* set-mus-array-print-length drop
  *clm-clipped* boolean? if *clm-clipped* else #f then set-mus-clipping drop
;
: ws-after-output ( ws -- ws )
  { ws }
  *output* mus-output? if *output* mus-close drop then
  *reverb* mus-output? *reverb* mus-input? || if *reverb* mus-close drop then
  ws :old-*output*    hash-ref to *output*
  ws :old-*reverb*    hash-ref to *reverb*
  ws :old-verbose     hash-ref to *verbose*
  ws :old-channels    hash-ref to *channels*
  ws :old-srate       hash-ref set-mus-srate drop
  ws :old-locsig-type hash-ref to *locsig-type*
  ws :old-notehook    hash-ref to *notehook*
  ws :old-decay-time  hash-ref to *clm-decay-time*
  *ws-args* array-pop
;
\ player can be one of proc, string, or #f.
\ 
\     proc: player '( output ) run-proc
\   string: "player output" system
\ else snd: output play-and-wait
\      clm: output play-sound
: ws-play-it ( ws -- )
  { ws }
  ws :output hash-ref { output }
  ws :player hash-ref { player }
  player proc? if
    player '( output ) run-proc drop
  else
    player string? if
      $" %s %s" '( player output ) string-format file-shell drop
    else
      'snd provided? if
	output find-file play-and-wait drop
      else
	*clm-verbose* { old-verbose }
	#f to *clm-verbose*
	output play-sound
	old-verbose to *clm-verbose*
      then
    then
  then
;
set-current

: set-args ( key def ws -- )
  { key def ws }
  key def get-args ws key rot hash-set!
;
: with-sound-default-args ( keyword-args -- ws )
  make-hash { ws }
  *ws-args* ws array-push drop
  :play              *clm-play*             ws set-args
  :statistics        *clm-statistics*       ws set-args
  :verbose           *clm-verbose*          ws set-args
  :continue-old-file #f                     ws set-args
  :output            *clm-file-name*        ws set-args
  :channels          *clm-channels*         ws set-args
  :srate             *clm-srate*            ws set-args
  :locsig-type       *clm-locsig-type*      ws set-args
  :header-type       *clm-header-type*      ws set-args
  :data-format       *clm-data-format*      ws set-args
  :comment           *clm-comment*          ws set-args
  :notehook          *clm-notehook*         ws set-args
  :scaled-to         #f                     ws set-args
  :scaled-by         #f                     ws set-args
  :delete-reverb     *clm-delete-reverb*    ws set-args
  :reverb            *clm-reverb*           ws set-args
  :reverb-data       *clm-reverb-data*      ws set-args
  :reverb-channels   *clm-reverb-channels*  ws set-args
  :reverb-file-name  *clm-reverb-file-name* ws set-args
  :player            *clm-player*           ws set-args
  :decay-time        *clm-decay-time*       ws set-args
  ws
;  

: with-sound-args ( keyword-args -- ws )
  make-hash { ws }
  *ws-args* -1 array-ref { ws1 }
  *ws-args* ws array-push drop
  :play              #f                        ws set-args
  :statistics        #f                        ws set-args
  :verbose           ws1 :verbose     hash-ref ws set-args
  :continue-old-file #f                        ws set-args
  :output            ws1 :output      hash-ref ws set-args
  :channels          ws1 :channels    hash-ref ws set-args
  :srate             ws1 :srate       hash-ref ws set-args
  :locsig-type       ws1 :locsig-type hash-ref ws set-args
  :header-type       ws1 :header-type hash-ref ws set-args
  :data-format       ws1 :data-format hash-ref ws set-args
  :comment $" with-sound level %d" '( *ws-args* length ) string-format ws set-args
  :notehook          ws1 :notehook    hash-ref ws set-args
  :scaled-to         #f                        ws set-args
  :scaled-by         #f                        ws set-args
  :delete-reverb     #f                        ws set-args
  :reverb            #f                        ws set-args
  :reverb-data       #f                        ws set-args
  :reverb-channels   0                         ws set-args
  :reverb-file-name  #f                        ws set-args
  :player            #f                        ws set-args
  :decay-time        ws1 :decay-time  hash-ref ws set-args
  ws
;

: with-sound-main ( body-xt ws -- ws )
  { body-xt ws }
  ws :old-*output*    *output*         hash-set!
  ws :old-*reverb*    *reverb*         hash-set!
  ws :old-verbose     *verbose*        hash-set! 
  ws :verbose                          hash-ref  to *verbose*
  ws :old-channels    *channels*       hash-set!
  ws :channels                         hash-ref  to *channels*
  ws :old-srate       mus-srate        hash-set!
  ws :srate                            hash-ref  set-mus-srate drop
  ws :old-locsig-type *locsig-type*    hash-set!
  ws :locsig-type                      hash-ref  to *locsig-type*
  ws :old-notehook    *notehook*       hash-set!
  ws :notehook                         hash-ref  to *notehook*
  ws :reverb hash-ref unless ws :reverb-file-name #f hash-set! then
  ws :old-decay-time  *clm-decay-time* hash-set!
  ws :decay-time                       hash-ref  to *clm-decay-time*
  ws ws-before-output
  make-timer { tm }
  tm start-timer
  \ compute ws body
  body-xt #t nil fth-catch if
    ws ws-after-output ( ws )
    #f #f #f fth-raise			\ re-raises last (catched) exception again
  then
  ws :reverb-file-name hash-ref if
    *reverb* mus-close drop
    ws :reverb-file-name hash-ref undef make-file->sample to *reverb*
    *reverb* file->sample? unless
      'with-sound-error '( get-func-name $" cannot open file->sample" _ ) fth-throw
    then
    \ compute ws reverb
    ws :reverb-data hash-ref each end-each ( push reverb arguments on stack )
    ws :reverb hash-ref #t nil fth-catch if
      ws ws-after-output ( ws )
      #f #f #f fth-raise		\ re-raises last (catched) exception again
    then
  then
  tm stop-timer
  ws :statistics hash-ref if
    ws :output   hash-ref
    ws :reverb-file-name  hash-ref
    ws :channels hash-ref
    ws :srate    hash-ref
    ws :output   hash-ref mus-sound-frames
    ws :scaled-to hash-ref ws :scaled-by hash-ref ||
    tm snd-info then
  ws :delete-reverb hash-ref ws :reverb-file-name hash-ref && if
    ws :reverb-file-name hash-ref file-delete
  then
  ws ws-get-snd drop
  ws :scaled-to hash-ref if ws ws-scaled-to then
  ws :scaled-by hash-ref if ws ws-scaled-by then
  ws :play      hash-ref if ws ws-play-it   then
  ws ws-after-output ( ws )
;
previous

\ Usage: ' resflt-test with-sound drop
\        ' resflt-test :play #f :channels 2 with-sound .g
\        lambda: resflt-test ; :output "resflt.snd" with-sound drop
: with-sound ( body-xt keyword-args -- ws )
  doc" ( body-xt keyword-args -- )  Evals CLM instruments.\n\
\\ keywords and default values:\n\
:play              *clm-play*             #f\n\
:statistics        *clm-statistics*       #f\n\
:verbose           *clm-verbose*          #f\n\
:continue-old-file                        #f\n\
:output            *clm-file-name*        \"test.snd\"\n\
:channels          *clm-channels*         1\n\
:srate             *clm-srate*            22050\n\
:locsig-type       *clm-locsig-type*      mus-interp-linear\n\
:header-type       *clm-header-type*      mus-next\n\
:data-format       *clm-data-format*      mus-lfloat\n\
:comment           *clm-comment*          #f\n\
:notehook          *clm-notehook*         #f\n\
:scaled-to                                #f\n\  
:scaled-by                                #f\n\  
:delete-reverb     *clm-delete-reverb*    #f\n\
:reverb            *clm-reverb*           #f\n\
:reverb-data       *clm-reverb-data*      '()\n\
:reverb-channels   *clm-reverb-channels*  1\n\
:reverb-file-name  *clm-reverb-file-name* \"test.reverb\"\n\
:player            *clm-player*           #f\n\
:decay-time        *clm-decay-time*       1.0\n\
' resflt-test with-sound .g cr\n\
' resflt-test :play #t :channels 2 :srate 44100 with-sound drop"
  *ws-args* length 0= if
    with-sound-default-args
  else
    with-sound-args
  then ( ws )
  with-sound-main ( ws )
;

: clm-load ( keyword-args output -- ws )
  doc" ( keyword-args output -- )  Loads and evals CLM files.  \
See with-sound for a full keyword list.\n\
:play #t :player \"sndplay\" \"test.fsm\" clm-load drop"
  { output }
  output file-exists? if
    :verbose *clm-verbose* get-args { verbose }
    verbose if $" loading %S" _ '( output ) clm-message then
    output ['] file-eval :verbose verbose with-sound ( ws )
  else
    'no-such-file '( get-func-name output ) fth-throw
  then
;

hide
: with-current-sound ( body-xt keyword-args -- )
  :output     fth-tempnam get-args { infile }
  :offset     0.0         get-args { offset }
  :scaled-to  #f          get-args { scl-to }
  :scaled-by  #f          get-args { scl-by }
  :output    infile
  :scaled-to scl-to
  :scaled-by scl-by with-sound drop
  :filename infile :output-frame offset seconds->samples clm-mix
  infile file-delete
;
set-current
: scaled-to   ( body-xt keyword-args scl  -- ) { scl }  :scaled-to scl with-current-sound ;
: scaled-by   ( body-xt keyword-args scl  -- ) { scl }  :scaled-by scl with-current-sound ;
: with-offset ( body-xt keyword-args secs -- ) { secs } :offset   secs with-current-sound ;
previous

: with-mix ( body-str args fname start -- )
  doc" ( body-str args fname start -- )  \
BODY-STR is a string with with-sound commands, \
ARGS are with-sound arguments, \
FNAME is the temporary mix file name \
and START is the begin time for mix in.\n\
lambda: ( -- )\n\
  0.0 0.1 440 0.1 fm-violin\n\
  $\"\n\
  0.0 0.1 550 0.1 fm-violin\n\
  0.1 0.1 660 0.1 fm-violin\n\
  \" '() $\" sec1\" 0.5 with-mix\n\
  $\"\n\
  0.0 0.1  880 0.1 :reverb-amount 0.2 fm-violin\n\
  0.1 0.1 1320 0.1 :reverb-amount 0.2 fm-violin\n\
  \" '( :reverb ['] jc-reverb ) $\" sec2\" 1.0 with-mix\n\
  2.0 0.1 220 0.1 fm-violin\n\
  ; with-sound drop"
  { body-str args fname start }
  fname $" .snd" $+ { snd-file }
  fname $" .fsm" $+ { mix-file }
  snd-file file-exists? if
    snd-file file-mtime
  else
    #f
  then { snd-time }
  mix-file file-exists? if
    mix-file readlines "" array-join
  else
    ""
  then { old-body }
  old-body body-str string= if
    mix-file file-mtime
  else
    mix-file #( body-str ) writelines
    #f
  then { mix-time }
  snd-time false?
  mix-time false? ||
  snd-time mix-time b< || if
    mix-file ['] file-eval args each end-each :output snd-file with-sound drop
  then
  :filename snd-file :output-frame start seconds->samples clm-mix
;

: sound-let ( ws-xt-lst body-xt -- )
  doc" ( ws-xt-lst body-xt -- )  \ 
Requires a list of lists WS-XT-LST with with-sound args and xts, and a BODY-XT.  \
The BODY-XT must take WS-XT-LST length arguments which are tempfile names.  \
with-sound will be feed with ws-args und ws-xts from WS-XT-LST.  \
:output is set to tempnam which will be on stack before executing BODY-XT.  \
These temporary files will be deleted after execution of BODY-XT.\n\
'( '( '( :reverb ' jc-reverb ) 0.0 1 220 0.2 ' fm-violin )\n\
   '( '()                      0.5 1 440 0.3 ' fm-violin ) ) ( ws-xt-lst )\n\
lambda: { tmp1 tmp2 }\n\
  :output tmp1 :filename tmp2 clm-mix\n\
  :filename tmp1 clm-mix\n\
; ( body-xt ) ' sound-let with-sound drop"
  { ws-xt-lst body-xt }
  #() { outfiles }
  ws-xt-lst each { arg }
    fth-tempnam { oname }
    arg cdr     ( ws-xts )  each end-each
    arg car     ( ws-args ) each end-each :output oname with-sound drop
    outfiles oname array-push drop
  end-each
  outfiles each end-each body-xt execute
  outfiles each file-delete end-each
;

\ === example instruments, more in clm-ins.fs ===

instrument: simp ( start dur freq amp -- )
  { start dur freq amp }
  :frequency freq make-oscil { os }
  :envelope '( 0e 0e 25e 1e 75e 1e 100e 0e ) :duration dur :scaler amp make-env { en }
  start dur run
    i  os 0.0 0.0 oscil en env f*  *output* outa drop
  loop
;instrument

: run-test ( -- ) 0.0 1.0 330.0 0.5 simp ;

: input-fn ( gen -- proc; dir self -- r )
  lambda-create , latestxt 1 make-proc
 does> ( dir self -- r )
  nip @ readin
;

instrument: src-simp ( start dur amp sr sr-env fname -- )
  { start dur amp sr sr-env fname }
  :file fname find-file make-readin { f }
  :input f input-fn :srate sr make-src { sc }
  :envelope sr-env :duration dur make-env { en }
  start dur run
    i  sc en env #f src amp f*  *output* outa drop
  loop
  f mus-close drop
;instrument

instrument: hello-dentist ( start dur freq amp fname -- )
  { start dur freq amp fname }
  :file fname find-file make-readin { f }
  :frequency freq :amplitude amp make-rand-interp { rn }
  :input f input-fn :width 5 make-src { sr }
  start dur run
    i sr rn 0.0 rand-interp #f src amp f* *output* outa drop
  loop
  f mus-close drop
;instrument

instrument: conv-simp ( start dur filt fname amp -- )
  { start dur filt fname amp }
  :file fname find-file make-readin { f }
  filt string? if
    8192 0.0 make-vct { v }
    filt find-file 0 0 v length v file->array
  else
    filt
  then { data }
  :input f input-fn :filter data make-convolve { cv }
  start dur run
    i cv #f convolve  amp f*  *output* outa drop
  loop
  f mus-close drop
;instrument

\ ' src-test with-sound drop
event: src-test ( -- )
  0.0 1.0 1.0 0.2 '( 0e 0e 50e 1e 100e 0e ) $" oboe.snd" src-simp
;event

\ ' dent-test with-sound drop
event: dent-test ( -- )
  0.0 1.0 40.0 1.0 $" oboe.snd" hello-dentist
;event

\ ' conv1-test with-sound drop
event: conv1-test ( -- )
  0.0 1.0 vct( 0.5 0.2 0.1 0.05 0e 0e 0e 0e ) $" fyow.snd" 1.0 conv-simp
;event

\ ' conc2-test with-sound drop
event: conv2-test ( -- )
  0.0 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ ' inst-test with-sound drop
event: inst-test ( -- )
  0.0 1.0 1.0 0.2 '( 0 0 50 1 100 0 ) $" oboe.snd" src-simp
  1.2 1.0 40.0 1.0 $" oboe.snd" hello-dentist
  2.4 1.0 vct( 0.5 0.2 0.1 0.05 0 0 0 0 ) $" fyow.snd" 1.0 conv-simp
  3.6 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ clm.fs ends here
