\ clm.fs -- clm related base words -*- snd-forth -*-

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Mar 15 19:25:58 CET 2004
\ Changed: Sun Apr 02 23:34:54 CEST 2006

\ Commentary:
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
\ run                  ( start dur -- )
\ run-instrument       ( start dur args -- )
\ end-run              ( value -- )
\ reverb-info          ( in-chans out-chans rev-name -- )
\ srate-ref            ( -- n )
\ srate-set!           ( n -- )
\ instrument:          ( ?? -- )
\ ;instrument          ( -- )
\ event:               ( ?? -- )
\ ;event               ( -- )
\
\ get-args             ( key val1 -- val2 )
\ find-file            ( file -- fname|#f )
\ snd-info             ( fname revname chans srate frames tm -- )
\ play-sound           ( file -- )
\ record-sound         ( file dur -- )
\ with-sound           ( body-xt keyword-args -- )
\ clm-load             ( keyword-args fname -- )
\ 

\ Code:

$" snd-fth" textdomain drop

dl-load sndlib Init_sndlib

'snd provided? [unless]
  : sound?       ( snd -- f )         drop #f ;
  : open-sound   ( name -- snd )      drop #f ;
  : update-sound ( snd )              drop #f ;
  : find-sound   ( name nth -- snd ) 2drop #f ;
  : channels     ( snd -- chns )      drop 0 ;
  : play         ( start snd chn syncd end pos stop-proc outchan -- f ) stack-reset #f ;
  ' play alias play-and-wait
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
  $" TMP" getenv dup empty? if
    drop
    $" TEMP" getenv dup empty? if
      drop
      $" TMPDIR" getenv dup empty? if
	drop
	$" /tmp"
      then
    then
  then getpid *fth-file-number* @ 3 >list string-format
;
previous

\ === Notelist ===
hide
0.00 value *clm-current-time*
60.0 value *clm-tempo*
0.25 value *clm-beat*
set-current
: now@   ( -- secs ) *clm-current-time* ;
: now!   ( secs -- ) to *clm-current-time* ;
: wait   ( secs -- ) now@ f+ now! ;
: tempo@ ( -- secs ) *clm-tempo* ;
: tempo! ( secs -- ) to *clm-tempo* ;
previous

\ --- Pitches ---
6.875 constant lowest-freq

: interval->hertz ( n -- r ) { n } 2.0 12.0 n 3.0 f+ f+ 12.0 f/ f** lowest-freq f* ;
: keynum->hertz   ( n -- r ) { n } 2.0 n 3.0 f+ 12.0 f/ f** lowest-freq f* ;
: hertz->keynum   ( r -- n ) lowest-freq f/ 2.0 flogn 12.0 f* 3.0 f- f>s ;
: f,              ( r -- ) here 1 floats allot f! ;

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

\ === Global User Variables (settable in ~/.snd or ~/.fthrc) ===
#t  		  value *clm-play*
#t  		  value *clm-statistics*
#t  		  value *clm-verbose*
#t  		  value *clm-delete-reverb*
#f 		  value *clm-reverb*
'()               value *clm-reverb-data*
1     		  value *clm-channels*
1     		  value *clm-reverb-channels*
22050             value *clm-srate*
8192              value *clm-rt-bufsize*
512   		  value *clm-table-size*
mus-interp-linear value *clm-locsig-type*
mus-next          value *clm-header-type*
mus-lfloat        value *clm-data-format*
#f 		  value *clm-notehook*
$" test.snd"      value *clm-file-name*
$" test.reverb"   value *clm-reverb-file-name*
'intern           value *clm-player*           
#f                value *clm-comment*
0.05  		  value *clm-reverb-amount*
1.00  		  value *clm-decay-time*
mus-lshort        value *clm-audio-format*
mus-audio-default value *clm-device*
1024 64 *         value *clm-file-buffer-size*
#()               value *clm-search-list* \ array of sound directories

#f value *output*
#f value *reverb*
#f value *locsig*

\ internal global variables
*clm-channels*      value *channels*
*clm-srate*         value *srate*
*clm-table-size*    value *table-size*
*clm-rt-bufsize*    value *rt-bufsize*
*clm-locsig-type*   value *locsig-type*
*clm-verbose*       value *verbose*
*clm-audio-format*  value *audio-format*
*clm-reverb-amount* value *reverb-amount*
*clm-notehook*      value *notehook*

: make-default-comment ( -- str )
  $" Written on %s by %s at %s using clm (fth) of %s" _
  '( date $" USER" getenv hostname fth-date ) string-format
;
: times->samples ( start dur -- len beg )
  { start dur }
  start seconds->samples { beg }
  dur seconds->samples { len }
  beg len + beg
;
: run ( start dur -- ) postpone times->samples postpone ?do ; immediate

\ === With-Sound Run-Instrument ===
hide
: (set-locsig) ( args -- )
  { args }
  args hash? unless #{} to args then
  save-stack { s }
  :degree   args :degree   hash-ref             0.0 ||
  :distance args :distance hash-ref             1.0 ||
  :reverb   args :reverb   hash-ref *reverb-amount* ||
  :channels args :channels hash-ref      *channels* ||
  :output   args :output   hash-ref        *output* ||
  :revout   args :revout   hash-ref        *reverb* ||
  :type     args :type     hash-ref   *locsig-type* || make-locsig to *locsig*
  s restore-stack
  \ we set channel 3/4, if any, to 0.5 * channel 1/2
  *output* mus-output? if
    *output* mus-channels 2 > if
      *locsig* dup 0 locsig-ref f2/ 2 locsig-set!
      *output* mus-channels 3 > if
	*locsig* dup 1 locsig-ref f2/ 3 locsig-set!
      then
    then
  then
  *reverb* mus-output? if
    *reverb* mus-channels 2 > if
      *locsig* dup 0 locsig-reverb-ref f2/ 2 locsig-reverb-set!
      *reverb* mus-channels 3 > if
	*locsig* dup 1 locsig-reverb-ref f2/ 3 locsig-reverb-set!
      then
    then
  then
;
set-current
: run-instrument ( start dur locsig-args -- )
  postpone (set-locsig) postpone times->samples postpone ?do
; immediate

: end-run ( value -- )
  postpone *locsig* postpone r@ postpone rot postpone locsig postpone drop postpone loop
; immediate
previous

: reverb-info ( in-chans out-chans rev-name -- )
  { in-chans out-chans rev-name }
  $" \\ %s on %d in and %d out channels\n" _ '( rev-name in-chans out-chans ) fth-print
;

: srate-ref  ( -- n ) mus-srate f>s ;
: srate-set! ( n -- ) set-mus-srate f>s to *srate* ;

\ === Helper functions for instruments ===
hide
: ins-info ( ins-name -- ) *notehook* if *notehook* execute else drop then ;
: event-info ( ev-name -- )
  *verbose* if ." \    event: " .string cr else drop then
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
: .maxamps ( fname name srate -- )
  { fname name sr }
  fname mus-sound-maxamp { vals }
  vals length 0 do
    $" \\ %*s %c: %.3f (near %.3f secs)\n"
    '( 6 name
       [char] A i 2/ +
       vals i 1+ list-ref
       vals i list-ref sr f/ ) fth-print
  2 +loop
;
set-current

: snd-info ( fname revname chans srate frames tm -- )
  { fname revname chans sr frms tm }
  fname mus-sound-duration { dur }
  $" \\ filename: %s\n"                '( fname )       fth-print
  $" \\    chans: %d, srate: %d\n"     '( chans sr )    fth-print
  $" \\   format: %s [%s]\n"
  '( fname mus-sound-data-format mus-data-format-name
     fname mus-sound-header-type mus-header-type-name ) fth-print
  $" \\   length: %.3f  (%d frames)\n" '( dur frms )    fth-print
  tm timer? if
    tm .timer
    sr frms tm .timer-ratio
  then
  fname $" maxamp" sr .maxamps
  revname ?dup-if $" revamp" sr .maxamps then
  fname mus-sound-comment { comm }
  comm empty? unless $" \\  comment: %s\n" comm fth-print then
;
previous

\ playing and recording one or two channel sounds
: play-sound ( file -- )
  find-file { fname }
  fname unless 'no-such-file '( get-func-name fname ) fth-throw then
  fname mus-sound-frames { frms }
  fname mus-sound-srate { srate }
  fname mus-sound-chans { chans }
  chans 2 > if
    $" %s: we handle only 2 chans, not %d" _ '( get-func-name chans ) string-format warning
  then
  *clm-verbose* if fname #f chans srate frms #f snd-info then
  *clm-rt-bufsize* frms min { bufsize }
  chans bufsize make-sound-data { data }
  fname mus-sound-open-input { snd-fd }
  snd-fd 0< if 'forth-error '( get-func-name $" cannot open %s" _ fname ) fth-throw then
  mus-audio-default srate chans 2 min *clm-audio-format* bufsize mus-audio-open-output { dac-fd }
  dac-fd 0< if 'forth-error '( get-func-name $" cannot open dac" _ ) fth-throw then
  frms 0 ?do
    i bufsize + frms > if frms i - to bufsize then
    snd-fd 0 bufsize 1- chans data mus-sound-read drop
    dac-fd data bufsize mus-audio-write drop
  bufsize +loop
  snd-fd mus-sound-close-input drop
  dac-fd mus-audio-close drop
;

: record-sound ( file dur -- )
  { fname dur }
  dur seconds->samples { frms }
  *clm-device* { device }
  *clm-rt-bufsize* frms min { bufsize }
  *clm-channels* 2 min { chans }
  *clm-srate* { srate }
  srate srate-set!
  *clm-audio-format* { fmt }
  *clm-header-type* { htype }
  chans bufsize make-sound-data { data }
  chans 0.25 make-vct { vals }
  vals each drop mus-audio-mixer mus-audio-reclev i vals mus-audio-mixer-write drop end-each
  vals 0.75 vct-fill! drop
  vals each drop device  mus-audio-amp i vals mus-audio-mixer-write drop end-each
  $" written by %s at %s" _ '( get-func-name date ) string-format { descr }
  fname srate chans fmt htype descr mus-sound-open-output { snd-fd }
  snd-fd 0< if 'forth-error '( get-func-name $" cannot open %s" _ fname ) fth-throw then
  device srate chans 2 min fmt bufsize mus-audio-open-input { dac-fd }
  dac-fd 0< if 'forth-error '( get-func-name $" cannot open dac" _ ) fth-throw then
  *clm-verbose* if
    $" \\ filename: %s\n"                '( fname )                    fth-print
    $" \\   device: %d\n"                '( device )                   fth-print
    $" \\    chans: %d, srate: %d\n"     '( chans srate )              fth-print
    $" \\ r format: %s\n"                '( fmt mus-data-format-name ) fth-print
    $" \\ w format: %s [%s]\n" '( fmt mus-data-format-name htype mus-header-type-name ) fth-print
    $" \\   length: %.3f  (%d frames)\n" '( dur frms )                 fth-print
    $" \\  comment: %s\n"                '( descr )                    fth-print
  then
  frms 0 ?do
    i bufsize + frms > if frms i - to bufsize then
    dac-fd data bufsize mus-audio-read drop
    snd-fd 0 bufsize 1- chans data mus-sound-write drop
  bufsize +loop
  dac-fd mus-audio-close drop
  snd-fd frms chans * fmt mus-bytes-per-sample * mus-sound-close-output drop
;

\ === With-Sound Keywords ===
\ 
\ (It's actually not necessary to define them, a keyword will be
\ created if the interpreter finds a word prepended by `:'.)
\
create-keyword play
create-keyword statistics
create-keyword verbose
create-keyword continue-old-file
create-keyword delete-reverb
create-keyword reverb
create-keyword reverb-data
create-keyword channels
create-keyword reverb-channels
create-keyword srate
create-keyword rt-buffer-size
create-keyword table-size
create-keyword locsig-type
create-keyword header-type
create-keyword data-format
create-keyword notehook
create-keyword output
create-keyword reverb-file-name
create-keyword player
create-keyword comment
create-keyword reverb-amount
create-keyword decay-time
create-keyword audio-format
create-keyword device

hide
#f value *ws-verbose*
#f value *ws-channels*
#f value *ws-srate*
#f value *ws-rt-bufsize*
#f value *ws-table-size*
#f value *ws-locsig-type*
#f value *ws-audio-format*
#f value *ws-reverb-amount*
#f value *ws-notehook*
#f value *ws-output*
#f value *ws-reverb*
: ws-preserve ( -- )
  *verbose*       to *ws-verbose*
  *channels*      to *ws-channels*
  *srate*         to *ws-srate*
  *rt-bufsize*    to *ws-rt-bufsize*
  *table-size*    to *ws-table-size*
  *locsig-type*   to *ws-locsig-type*
  *audio-format*  to *ws-audio-format*
  *notehook*      to *ws-notehook*
  *reverb-amount* to *ws-reverb-amount*
  *output*        to *ws-output*
  *reverb*        to *ws-reverb*
;
: ws-reset ( -- )
  *ws-verbose*       to *verbose*
  *ws-channels*      to *channels*
  *ws-srate*         srate-set!
  *ws-rt-bufsize*    to *rt-bufsize*
  *ws-table-size*    to *table-size*
  *ws-locsig-type*   to *locsig-type*
  *ws-audio-format*  to *audio-format*
  *ws-reverb-amount* to *reverb-amount*
  *ws-notehook*      to *notehook*
  *ws-output*        to *output*
  *ws-reverb*        to *reverb*
;
set-current

\ Usage: ' resflt-test with-sound
\        ' resflt-test :play #f :channels 2 with-sound
\        lambda: resflt-test ; :output "resflt.snd" with-sound
: with-sound ( body-xt keyword-args -- )
  doc" ( body-xt keyword-args -- )  has the following keyword arguments:\n\
:play              *clm-play*             #t\n\
:statistics        *clm-statistics*       #t\n\
:verbose           *clm-verbose*          #t\n\
:continue-old-file                        #f\n\
:delete-reverb     *clm-delete-reverb*    #t\n\
:reverb            *clm-reverb*           #f\n\
:reverb-data       *clm-reverb-data*      '()\n\
:channels          *clm-channels*         1\n\
:reverb-channels   *clm-reverb-channels*  1\n\
:srate             *clm-srate*            22050\n\
:rt-buffer-size    *clm-rt-bufsize*       8192\n\
:table-size        *clm-table-size*       512\n\
:locsig-type       *clm-locsig-type*      mus-interp-linear\n\
:header-type       *clm-header-type*      mus-next\n\
:data-format       *clm-data-format*      mus-lfloat\n\
:notehook          *clm-notehook*         #f\n\
:output            *clm-file-name*        \"test.snd\"\n\
:reverb-file-name  *clm-reverb-file-name* \"test.reverb\"\n\
:player            *clm-player*           'intern\n\
:comment           *clm-comment*          #f\n\
:decay-time        *clm-decay-time*       1.0\n\
:audio-format      *clm-audio-format*     mus-lshort\n\
:device            *clm-device*           mus-audio-default\n\
' resflt-test with-sound\n\
' resflt-test :play #f :channels 2 :srate 44100 with-sound"
  ws-preserve
  :play              *clm-play*             get-args { play }
  :statistics        *clm-statistics*       get-args { statistics }
  :verbose           *clm-verbose*          get-args to *verbose*
  :continue-old-file #f                     get-args { continue-old-file }
  :delete-reverb     *clm-delete-reverb*    get-args { delete-reverb }
  :reverb            *clm-reverb*           get-args { reverb-xt }
  :reverb-data       *clm-reverb-data*      get-args { reverb-data }
  :channels          *clm-channels*         get-args to *channels*
  :reverb-channels   *clm-reverb-channels*  get-args { rev-chans }
  :srate             *clm-srate*            get-args srate-set!
  :rt-buffer-size    *clm-rt-bufsize*       get-args to *rt-bufsize*
  :table-size        *clm-table-size*       get-args to *table-size*
  :locsig-type       *clm-locsig-type*      get-args to *locsig-type*
  :header-type       *clm-header-type*      get-args { header-type }
  :data-format       *clm-data-format*      get-args { data-format }
  :notehook          *clm-notehook*         get-args to *notehook*
  :output            *clm-file-name*        get-args { fname }
  :reverb-file-name  *clm-reverb-file-name* get-args { rev-name }
  :player            *clm-player*           get-args { player }
  :comment           *clm-comment*          get-args { comm }
  :reverb-amount     *clm-reverb-amount*    get-args to *reverb-amount*
  :decay-time        *clm-decay-time*       get-args { decay-time }
  :audio-format      *clm-audio-format*     get-args to *audio-format*
  { body-xt }
  comm unless make-default-comment to comm then
  $" with-sound: temporary reverb file" _ { rev-comment }
  continue-old-file if
    fname continue-sample->file
  else
    fname *channels* data-format header-type comm make-sample->file
  then to *output*
  *output* sample->file? unless
    'forth-error '( get-func-name $" cannot open sample->file" _ fname ) fth-throw
  then
  reverb-xt if
    rev-name rev-chans data-format header-type rev-comment make-sample->file to *reverb*
    *reverb* sample->file? unless
      'forth-error '( get-func-name $" cannot open sample->file" _ rev-name ) fth-throw
    then
  then
  make-timer { tm }
  tm start-timer
  body-xt #t nil fth-catch { ret }	\ #f or '( exception args ... )
  ret list? if
    *output* mus-close drop
    *reverb* if *reverb* mus-close drop then
    ws-reset
    ret object->string error exit
  then
  reverb-xt if
    *reverb* mus-close drop
    rev-name mus-sound-duration { dur }
    rev-name make-file->sample to *reverb*
    *reverb* file->sample? unless
      'forth-error '( get-func-name $" cannot open file->sample" _ rev-name ) fth-throw
    then
    0.0 dur decay-time f+ reverb-data reverb-xt #t nil fth-catch to ret
    ret list? if
      *reverb* mus-close drop
      *output* mus-close drop
      ws-reset
      ret object->string error exit
    then
    *reverb* mus-close drop
  then
  tm stop-timer
  *output* mus-close drop
  statistics if
    fname reverb-xt if rev-name else #f then *channels* srate-ref fname mus-sound-frames tm snd-info
  then
  delete-reverb if rev-name file-delete then
  'snd provided? if
    fname find-file to fname
    fname 0 find-sound { snd }
    snd sound? if
      snd update-sound
      'snd-nogui provided? if
	drop
      else
	\ update-sound returns a value for every channel (?)
	channels 0 ?do drop loop
      then
    else
      fname open-sound to snd
    then
    play if 0 snd #f #f #f -1 undef undef play-and-wait drop then
  else
    play if
      player 'intern = if
	*clm-verbose* { old-verbose }
	#f to *clm-verbose*
	fname play-sound
	old-verbose to *clm-verbose*
      else
	player $"  " $+ fname $+ shell drop
      then
    then
  then
  ws-reset
;
previous

: clm-load ( keyword-args fname -- )
  { fname }
  fname file-exists? if
    $" \\ loading \"%s\"\n" _ '( fname ) fth-print
    fname ['] file-eval with-sound
  else
    'no-such-file '( get-func-name fname ) fth-throw
  then
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

\ ' src-test with-sound
event: src-test ( -- )
  0.0 1.0 1.0 0.2 '( 0e 0e 50e 1e 100e 0e ) $" oboe.snd" src-simp
;event

\ ' dent-test with-sound
event: dent-test ( -- )
  0.0 1.0 40.0 1.0 $" oboe.snd" hello-dentist
;event

\ ' conv1-test with-sound
event: conv1-test ( -- )
  0.0 1.0 vct( 0.5 0.2 0.1 0.05 0e 0e 0e 0e ) $" fyow.snd" 1.0 conv-simp
;event

\ ' conc2-test with-sound
event: conv2-test ( -- )
  0.0 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ ' inst-test with-sound
event: inst-test ( -- )
  0.0 1.0 1.0 0.2 '( 0 0 50 1 100 0 ) $" oboe.snd" src-simp
  1.2 1.0 40.0 1.0 $" oboe.snd" hello-dentist
  2.4 1.0 vct( 0.5 0.2 0.1 0.05 0 0 0 0 ) $" fyow.snd" 1.0 conv-simp
  3.6 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ clm.fs ends here
