\ gfm.fs -- GFM load file -*- forth -*-

\ Copyright (C) 2003--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Thu Aug 28 00:54:28 CEST 2003
\ Last: Fri Jan 14 00:18:09 CET 2005
\ Ident: $Id: gfm.fs,v 1.261 2005/01/13 23:18:29 mike Exp $

\ This file is part of GFM Gforth Music.

\ This program is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License as
\ published by the Free Software Foundation; either version 2 of
\ the License, or (at your option) any later version.

\ This program is distributed in the hope that it will be
\ useful, but WITHOUT ANY WARRANTY; without even the implied
\ warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
\ PURPOSE.  See the GNU General Public License for more details.

\ You should have received a copy of the GNU General Public
\ License along with this program; if not, write to the Free
\ Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
\ MA 02111-1307 USA

\ Commentary:
\
\ Below the constants from clm.h and sndlib.h you will find global
\ variables which you can set to fit your needs, e.g. in ~/.gfmrc.
\ Using fsndlib.fs *clm-header-type* is meaningless (we use mus-next
\ only).

\ Code:

s" 14-01-2005" 2constant gfm-version

s" gforth" environment? [if] 2dup
    s" 0.6.0" compare 0< [if] s" Gforth 0.6.0 or higher required" type cr 1 (bye) [then]
    s" 0.6.2" compare 0< [if] ' lastxt alias latestxt [then]
[else]
    s" Gforth required" type cr 1 (bye)
[then]

require gfmpath.fs
require utils.fs
only forth also definitions
also Utils

: str-banner ( -- )
    ." Gforth " version-string type ." , Copyright (C) 1995--2003 Free Software Foundation, Inc." cr
    ." CLM/Sndlib, Copyright (C) 1996--2005 Bill Schottstaedt" cr
    ." GFM Gforth Music of " gfm-version type ." , Copyright (C) 2003--2005 Michael Scholz" cr
;
: str-version ( -- )
    str-banner cr
    .\" GFM Gforth Music comes with ABSOLUTELY NO WARRANTY.\n"
    .\" You may redistribute copies of GFM Gforth Music\n"
    .\" under the terms of the GNU General Public License.\n"
    .\" For more information about these matters, see the file named COPYING.\n"
;

\ STR-HELP is defered in utils.fs and can be changed after loading
\ utils.fs and before loading gfm.fs.  See sndtest.gfm for an example.

[ifundef] *clm-use-csndlib*
    [ifdef] av-call-int true [else] false [then] value *clm-use-csndlib*
[then]
[ifundef] *clm-dac-output* false value *clm-dac-output* [then]

array[ $" -c" $" --csndlib" ] opt-bool getopts [if]  true to *clm-use-csndlib* [then]
array[ $" -f" $" --fsndlib" ] opt-bool getopts [if] false to *clm-use-csndlib* [then]
array[ $" -d" $" --dac" ]     opt-bool getopts [if]  true to *clm-dac-output*  [then]
array[ $" -V" $" --version" ] opt-bool getopts [if] str-version bye            [then]
array[ $" -h" $" --help"    ] opt-bool getopts [if] str-help    bye            [then]

only forth also definitions
vocabulary GFMusic
also GFMusic definitions
also Utils

\ === mus_interp_t (clm.h) ===
-1 constant mus-interp-unknown
00 constant mus-interp-none
01 constant mus-interp-linear
02 constant mus-interp-sinusoidal
03 constant mus-interp-all-pass
04 constant mus-interp-lagrange
05 constant mus-interp-bezier
06 constant mus-interp-hermite

\ === mus_fft_window_t (clm.h) ===
00 constant mus-rectangular-window
01 constant mus-hann-window
02 constant mus-welch-window
03 constant mus-parzen-window
04 constant mus-bartlett-window
05 constant mus-hamming-window
06 constant mus-blackman2-window
07 constant mus-blackman3-window
08 constant mus-blackman4-window
09 constant mus-exponential-window
10 constant mus-riemann-window
11 constant mus-kaiser-window
12 constant mus-cauchy-window
13 constant mus-poisson-window
14 constant mus-gaussian-window
15 constant mus-tukey-window
16 constant mus-dolph-chebyshev-window
17 constant mus-hann-poisson-window
18 constant mus-connes-window

\ === sndlib.h ===
00 constant mus-unsupported
01 constant mus-next
02 constant mus-aifc
03 constant mus-riff
04 constant mus-bicsf
05 constant mus-nist
06 constant mus-inrs
07 constant mus-esps
08 constant mus-svx
09 constant mus-voc
10 constant mus-sndt
11 constant mus-raw
12 constant mus-smp
13 constant mus-avr
14 constant mus-ircam
15 constant mus-sd1
16 constant mus-sppack
17 constant mus-mus10
18 constant mus-hcom
19 constant mus-psion
20 constant mus-maud
21 constant mus-ieee
22 constant mus-matlab
23 constant mus-adc
24 constant mus-midi
25 constant mus-soundfont
26 constant mus-gravis
27 constant mus-comdisco
28 constant mus-goldwave
29 constant mus-srfs
30 constant mus-midi-sample-dump
31 constant mus-diamondware
32 constant mus-adf
33 constant mus-sbstudioii
34 constant mus-delusion
35 constant mus-farandole
36 constant mus-sample-dump
37 constant mus-ultratracker
38 constant mus-yamaha-sy85
39 constant mus-yamaha-tx16w
40 constant mus-digiplayer
41 constant mus-covox
42 constant mus-avi
43 constant mus-omf
44 constant mus-quicktime
45 constant mus-asf
46 constant mus-yamaha-sy99
47 constant mus-kurzweil-2000
48 constant mus-aiff
49 constant mus-paf
50 constant mus-csl
51 constant mus-file-samp
52 constant mus-pvf
53 constant mus-soundforge
54 constant mus-twinvq
55 constant mus-akai4
56 constant mus-impulsetracker
57 constant mus-korg
58 constant mus-maui
59 constant mus-sdif

00 constant mus-unknown
01 constant mus-bshort
02 constant mus-mulaw
03 constant mus-byte
04 constant mus-bfloat
05 constant mus-bint
06 constant mus-alaw
07 constant mus-ubyte
08 constant mus-b24int
09 constant mus-bdouble
10 constant mus-lshort
11 constant mus-lint
12 constant mus-lfloat
13 constant mus-ldouble
14 constant mus-ubshort
15 constant mus-ulshort
16 constant mus-l24int
17 constant mus-bintn
18 constant mus-lintn
19 constant mus-bfloat-unscaled
20 constant mus-lfloat-unscaled
21 constant mus-bdouble-unscaled
22 constant mus-ldouble-unscaled

00 constant mus-audio-default
01 constant mus-audio-duplex-default
02 constant mus-audio-adat-in
03 constant mus-audio-aes-in
04 constant mus-audio-line-out
05 constant mus-audio-line-in
06 constant mus-audio-microphone
07 constant mus-audio-speakers
08 constant mus-audio-digital-in
09 constant mus-audio-digital-out
10 constant mus-audio-dac-out
11 constant mus-audio-adat-out
12 constant mus-audio-aes-out
13 constant mus-audio-dac-filter
14 constant mus-audio-mixer
15 constant mus-audio-line1
16 constant mus-audio-line2
17 constant mus-audio-line3
18 constant mus-audio-aux-input
19 constant mus-audio-cd
20 constant mus-audio-aux-output
21 constant mus-audio-spdif-in
22 constant mus-audio-spdif-out
23 constant mus-audio-amp
24 constant mus-audio-srate
25 constant mus-audio-channel
26 constant mus-audio-format
27 constant mus-audio-imix
28 constant mus-audio-igain
29 constant mus-audio-reclev
30 constant mus-audio-pcm
31 constant mus-audio-pcm2
32 constant mus-audio-ogain
33 constant mus-audio-line
34 constant mus-audio-synth
35 constant mus-audio-bass
36 constant mus-audio-treble
37 constant mus-audio-port
38 constant mus-audio-samples-per-channel
39 constant mus-audio-direction
40 constant mus-audio-record-device

\ === Global User Variables (settable in ~/.gfmrc) ===
true  		  value *clm-play*
true  		  value *clm-statistics*
true  		  value *clm-verbose*
true  		  value *clm-delete-reverb*
nil 		  value *clm-reverb*
nil               value *clm-reverb-data*
1     		  value *clm-channels*
1     		  value *clm-reverb-channels*
22050             value *clm-srate*
1024 8 *          value *clm-rt-bufsize*
mus-interp-linear value *clm-locsig-type*
mus-next          value *clm-header-type*
mus-lfloat        value *clm-data-format*
mus-lshort        value *clm-audio-format*
mus-audio-default value *clm-device*
nil 		  value *clm-notehook*
$" test.snd"      value *clm-file-name*
$" test.reverb"   value *clm-reverb-file-name*
$" intern"        value *clm-player*           
nil               value *clm-comment*
0.05e 		 fvalue *clm-reverb-amount*
1.00e 		 fvalue *clm-decay-time*
512   		  value *clm-table-size*

\ Don't use SOUND-MAXAMP (slow) in PLAY-SOUND and WITH-SOUND if sound
\ duration is longer than *clm-maxamp-time* in seconds.
30e              fvalue *clm-maxamp-time*

\ Contains only one path, not a list of paths.
nil               value *clm-search-list*
nil               value *clm-process-id*

\ Instrument-name array of Strings which will be filled by
\ INSTRUMENT:...;INSTRUMENT.  Try SHOW-INSTRUMENTS.
0 make-array      value *clm-instruments*

\ internal global variables
*clm-channels*     value *channels*
*clm-srate*        value *srate*
*clm-table-size*   value *table-size*
*clm-rt-bufsize*   value *rt-bufsize*
*clm-locsig-type*  value *locsig-type*
*clm-verbose*      value *verbose*
*clm-audio-format* value *audio-format*
*clm-notehook*     value *notehook*

nil value *output*
nil value *reverb*
nil value *locsig*

pi f2* fconstant two-pi
pi f2/ fconstant half-pi

\ === Error Strings ===
\ --- fsndlib.fs and csndlib.fs ---
$" cannot open DAC"           constant str-audio-open-dac
$" cannot close audio device" constant str-audio-close
$" cannot open mixer device"  constant str-audio-open-mixer
$" cannot close mixer device" constant str-audio-close-mixer
$" audio-mixer-read"          constant str-audio-mixer-read
$" audio-mixer-write"         constant str-audio-mixer-write
\ --- gfm.fs and fsndlib.fs ---
$" missing filename"          constant str-missing-filename

\ takes info from environment variable $USER
: make-default-comment { -- str }
    $" Written " date>string $+
    $"  by " $+ $" USER" $getenv $+
    $"  at " $+ $" hostname" shell $+
    [ environment-wordlist >order ]
    $"  using gforth " $+
    gforth $>string $+
    $"  [" $+ os-class $>string $+
    [ previous ]
    $" ], GFM Gforth Music of " $+
    gfm-version $>string $+
;
: $(.interp-type) ( tp -- str )
    case
	mus-interp-none       of $" step"       endof
	mus-interp-linear     of $" linear"     endof
	mus-interp-sinusoidal of $" sinusoidal" endof
	mus-interp-all-pass   of $" all-pass"   endof
	mus-interp-lagrange   of $" lagrange"   endof
	mus-interp-bezier     of $" bezier"     endof
	mus-interp-hermite    of $" hermite"    endof
	$" unknown type" swap
    endcase
;
: .interp-type ( tp -- ) $(.interp-type) .string ;

: $(.device) ( dev -- str )
    case
	mus-audio-microphone of $" microphone" endof
	mus-audio-speakers   of $" speakers"   endof
	mus-audio-line       of $" line"       endof
	mus-audio-line-in    of $" line-in"    endof
	mus-audio-line1      of $" line1"      endof
	mus-audio-line2      of $" line2"      endof
	mus-audio-line3      of $" line3"      endof
	mus-audio-dac-out    of $" dac-out"    endof
	mus-audio-default    of $" default"    endof
	mus-audio-cd         of $" cd"         endof
	mus-audio-pcm        of $" pcm"        endof
	mus-audio-reclev     of $" reclev"     endof
	$" unknown device" swap
    endcase
;
: .device ( dev -- ) $(.device) .string ;
: yes-no-string     ( f -- str ) if $" yes"  else $" no"    then ;
: true-false-string ( f -- str ) if $" true" else $" false" then ;

: .sndversion ( -- )
    *verbose* if
	script? if ." \  " else cr then
	." using " *clm-use-csndlib* if ." csndlib" else ." fsndlib" then
	script-cr
    then
;

lambda: ( -- )
    str-banner
    *verbose* if
	." order: " order cr
	." stack: float " f.s ."  data " .s
	.sndversion
    then
; is bootmessage

\ === VCT, vct.h (defined in gfm-defs.fs and f|csndlib.fs) ===
struct
    cell% field c-vct-len
    cell% field c-vct-data
    cell% field c-vct-dont-free
end-struct c-vct%

obj%
    cell% field v-orig
    cell% field v-buf-len
end-struct vct%
$" vct" constant str-vct
str-vct make-?obj ?vct

: vct-fetch      ( idx v -- r )   obj-data @ swap sfloats + sf@ ;
: vct-store      ( val idx v -- ) obj-data @ swap sfloats + sf! ;
: vct-plus-store ( val idx v -- ) 2dup vct-fetch f+ vct-store ;

\ === Sound-Data, sndlib2xen.h (defined in gfm-defs.fs and f/csndlib.fs) ===
\ sndlib.so compiled with --with-float-samples
struct
    cell% field c-sd-len
    cell% field c-sd-chans
    cell% field c-sd-data
    cell% field c-sd-wrapped
end-struct c-sd%

obj%
    cell% field sd-chans
    cell% field sd-orig
end-struct sd%

$" sound-data" constant str-sound-data
str-sound-data make-?obj ?sound-data

\ === Src, Convolve, Granulate and Phase-Vocoder Callbacks ===
struct
    cell% field input-xt		\ ( dir -- r )
    cell% field edit-xt			\ ( gen -- n )
    cell% field analyze-xt		\ ( input-xt gen -- f )
    cell% field synthesize-xt		\ ( gen -- r )
    cell% field mus-gen
end-struct closure%

: make-closure { input edit analyze synthesize gen -- closure }
    closure% %alloc { closure }
    input      	closure input-xt !
    edit       	closure edit-xt !
    analyze    	closure analyze-xt !
    synthesize 	closure synthesize-xt !
    gen        	closure mus-gen !
    closure
;

: f-input-cb { closure dir -- r }
    closure input-xt @ ?dup-if dir swap execute else 0e then
;
: f-edit-cb { closure -- n }
    closure edit-xt @ ?dup-if closure mus-gen @ swap execute else 0 then
;
: f-analyze-cb { closure input -- f }
    closure analyze-xt @ ?dup-if closure input-xt @ closure mus-gen @ rot execute else true then
;
: f-synthesize-cb { closure -- r }
    closure synthesize-xt @ ?dup-if closure mus-gen @ swap execute else 0e then
;

\ MUS-AUDIO-DEFAULT AUDIO-PACK-SYSTEM card# OR VALUE dev
\ dev AUDIO-SYSTEM .       ==> card#
\ dev AUDIO-DEVICE .DEVICE ==> device name
: audio-pack-system ( n1 -- n2 ) 16 lshift ;
: audio-system      ( n1 -- n2 ) 16 rshift $ffff and ;
: audio-device      ( n1 -- n2 ) $ffff and ;

\ === Load CLM Library ===
*clm-use-csndlib* [if]
    so-lib-exists? sndlib libsndlib.so [if]
	require csndlib.fs
    [else]
	$" libsndlib.so not found, try fsndlib.fs instead\n" .string
	false to *clm-use-csndlib*
	require fsndlib.fs
    [then]
[else]
    require fsndlib.fs
[then]

only forth
also GFMusic definitions
also Utils
also CLM-Sndlib

' mus-close alias close-input
' mus-close alias close-output
: open-input    ( fname -- gen ) 0 0 1 clm-make-readin ;
: open-output   ( fname chans fmt typ -- gen ) nil make-sample>file ;
: times>samples ( start dur -- len beg ) seconds>samples seconds>samples tuck + swap ;
: run           ( start dur -- ) postpone times>samples postpone ?do ; immediate
: frame+! { f: val chn fr -- } chn fr frame@ f+ chn fr frame! ;

\ Next definitions use RANDOM/IRANDOM which is not known until
\ f|csndlib.fs is loaded.

\ === Notelist ===
\ v   rnd-pick f.
\ ary rnd-pick  .
: rnd-pick ( obj -- val ) dup length irandom swap object@ ;
: ?chance  ( r -- f ) 1e random f> ;
' ?chance alias ?odds
: between  ( nmin nmax -- n ) over - dup 0> if irandom + else 2drop 0 then ;
: fbetween ( rmin rmax -- r ) fover f- fdup f0> if random f+ else fdrop fdrop 0e then ;
\ 60 ( key ) 6 ( width ) 30 90 drunk .
: drunk { n1 w nmin nmax -- n2 }
    0 ( n2 ) begin dup ( n2 ) nmin nmax within 0= while drop n1 w - n1 w + between repeat
;

\ === Rnd ===
obj%
    cell% field rnd-pattern
end-struct rnd%
$" Rnd" constant str-rnd

str-rnd make-?obj ?rnd

: free-rnd { obj -- }
    obj ?rnd if
	obj rnd-pattern @ free-vct
	nil obj gfm-base-name !
	obj free throw
    then
;
: rnd-init { v1 -- v2 }
    0 make-vct { v2 }
    0e v1 length 1- 0 do i v1 vct@ f+ 2 +loop { f: flen }
    100e flen f/ { f: scl }
    0e 0e { f: wei f: wei2 }
    v1 length 1- 0 do
	i v1 vct@ wei f+ to wei
	i 1+ v1 vct@ hertz>keynum s>f { f: val }
	wei2 { f: wei1 }
	wei scl f* to wei2
	wei1 0.01e f+ v2 vct-push!
	val v2 vct-push!
	wei2 v2 vct-push!
	val v2 vct-push!
    2 +loop
    0e 0 v2 vct!
    v2
;
: $(.rnd) { obj -- str }
    $" #<" str-rnd $+
    $"  pattern: " $+ obj rnd-pattern @ gen>string $+
    $" >" $+
;
' $(.rnd) make-inspect .rnd
\ vct[ weight1 val1 ... weightn valn ] make-rnd
: make-rnd { v -- rnd }
    rnd% %alloc { rnd }
    v rnd-init   rnd rnd-pattern !
    ['] free-rnd rnd gfm-base-free !
    ['] .rnd     rnd gfm-base-inspect !
    ['] $(.rnd)  rnd gfm-base-to-string !
    str-rnd      rnd gfm-base-name !
    rnd
;
: rnd-randomize { rnd -- f: val } 100e random rnd rnd-pattern @ 1e envelope-interp ;

\ === Array/Vct-Rand ===
\ shuffles elements of arrays or vcts
: array-rand { ary1 -- ary2 }
    ary1 array-copy { ary2 }
    ary1 length { len }
    ary2 each { val }
	len irandom { ridx }
	ridx ary2 array@ i ary2 array!
	val ridx ary2 array!
    end-each
    ary2
;
: array-rand! { ary -- }
    ary length { len }
    ary each { val }
	len irandom { ridx }
	ridx ary array@ i ary array!
	val ridx ary array!
    end-each
;
: vct-rand { v1 -- v2 }
    v1 vct-copy { v2 }
    v1 length { len }
    v2 each { f: val }
	len irandom { ridx }
	ridx v2 vct@ i v2 vct!
	val ridx v2 vct!
    end-each
    v2
;
: vct-rand! { v -- }
    v length { len }
    v each { f: val }
	len irandom { ridx }
	ridx v vct@ i v vct!
	val ridx v vct!
    end-each
;

user *clm-file-number*

\ Looks for environment variables TMP, TEMP, or TMPDIR, otherwise
\ uses /tmp as temporary path and produces something like
\ /tmp/gfm-12345-1.snd
\ /tmp/gfm-12345-2.snd
\ /tmp/gfm-12345-3.snd
\ [...]

: tempnam ( -- name )
    1 *clm-file-number* +!
    $" TMP" $getenv ?dup-0=-if
	$" TEMP" $getenv ?dup-0=-if
	    $" TMPDIR" $getenv ?dup-0=-if
		$" /tmp"
	    then
	then
    then
    $" /gfm-" $+ *clm-process-id* $(.) $+ $" -" $+ *clm-file-number* @ $(.) $+ $" .snd" $+
;

: vct>sound-file { fd v frames --  }
    assert1( frames v length <= )
    1 frames make-sound-data { sd }
    0 frames v vct-subseq 0 sd vct>sound-data
    fd 0 frames 1- 1 sd sound-write
    sd sound-data-free
;

\ === Playing and Recording Sound Files ===
: .maxamps { fname name srate -- }
    fname sound-maxamp { vals times }
    times each
	cr ." \ " name .string space [char] A i + emit ." : "
	i vals vct@ 3 f.r ( i times array@ ) s>f srate s>f f/ ." (near " 3 f.r ." secs)"
    end-each
;
$" maxamp" constant str-maxamp
$" revamp" constant str-revamp
: play-info { fname revname chans srate frames tm -- }
    fname sound-duration { f: dur }
    !script-cr
    ." \ filename: " fname .gen cr
    ." \    chans: " chans (.) type ." , srate: " srate . cr
    ." \   format: " fname sound-data-format .data-format
    ."  [" fname sound-header-type .header-type ." ]" cr
    ." \   length: " dur 3 f.r ." (" frames . ." frames)"
    tm ?timer if
	cr
	." \    " tm .timer cr
	." \    " srate frames tm .timer-ratio
    then
    dur *clm-maxamp-time* f< if
	fname str-maxamp srate .maxamps
	revname if revname str-revamp srate .maxamps then
    then
    fname sound-comment ?dup-if cr ." \  comment: " .string then
    script-cr
;

$" file has too much channels" constant str-too-much-chans
: play-sound { fname -- }
    fname ?string unless str-missing-filename error then
    *clm-audio-format* { dac-fmt }
    fname sound-frames { frames }
    fname sound-srate  { srate }
    fname sound-chans  { chans }
    chans 2 > { quad }
    chans audio-systems 2* > if str-too-much-chans error then
    *clm-verbose* if fname nil chans srate frames nil play-info then
    *clm-rt-bufsize* frames min { bufsize }
    chans bufsize make-sound-data { sd }
    fname sound-open-input { snd-fd }
    0 audio-pack-system mus-audio-default or srate chans 2 min dac-fmt bufsize
    audio-open-output { fd0 }
    quad unless
	frames 0 do
	    i bufsize + frames > if frames i - to bufsize then
	    snd-fd 0 bufsize 1- chans sd sound-read drop
	    fd0 sd bufsize audio-write drop
	bufsize +loop
    else
	1 audio-pack-system mus-audio-default or srate chans 2 - 2 min dac-fmt bufsize
	audio-open-output { fd1 }
	2 bufsize make-sound-data { sd0 }
	chans 4 = if 2 else 1 then bufsize make-sound-data { sd1 }
	frames 0 do
	    i bufsize + frames > if frames i - to bufsize then
	    snd-fd 0 bufsize 1- chans sd sound-read drop
	    0 sd 0 sd0 sound-data>sound-data
	    1 sd 1 sd0 sound-data>sound-data
	    2 sd 0 sd1 sound-data>sound-data
	    chans 4 = if 3 sd 1 sd1 sound-data>sound-data then
	    fd0 sd0 bufsize audio-write drop
	    fd1 sd1 bufsize audio-write drop
	bufsize +loop
	fd1 audio-close
	sd0 sound-data-free
	sd1 sound-data-free
    then
    fd0 audio-close
    snd-fd sound-close-input
    sd sound-data-free
;

: audio-record! ( dev -- name ) mus-audio-record-device 0 0e audio-mixer-write fdrop array-first ;
: audio-reclev! { chans f: val -- }
    chans 1+ 1 ?do mus-audio-mixer mus-audio-reclev i val audio-mixer-write fdrop drop loop
;
: audio-srate   ( -- sr ) nil mus-audio-srate 0 audio-mixer-read fdrop array-first ;
: audio-chans   ( dev -- chns ) mus-audio-mixer mus-audio-format 40 audio-mixer-read fdrop array@ ;

$" input from mus-audio-" constant str-record-comment
: record-sound { fname device f: dur -- }
    *clm-srate* srate!
    device audio-record! { dev-name }
    *clm-audio-format*  { dac-fmt }
    dur seconds>samples { frames }
    *clm-channels* { chans }
    chans 2 > { quad }
    chans 0.25e audio-reclev!
    *clm-verbose* if
	!script-cr
	." \ filename: " fname .gen cr
	." \   device: mus-audio-" dev-name .device cr
	." \    chans: " chans (.) type ." , srate: " audio-srate . cr
	." \   format: " dac-fmt .data-format cr
	." \   length: " dur 3 f.r ." (" frames . ." frames)"
	script-cr
    then
    *clm-rt-bufsize* frames min { bufsize }
    chans 2 min bufsize make-sound-data { sd }
    fname srate@ chans *clm-data-format* *clm-header-type*
    str-record-comment dev-name $(.device) $+ sound-open-output { sfd }
    0 audio-pack-system device or srate@ chans 2 min dac-fmt bufsize audio-open-input { fd }
    0 ( samps )
    quad unless
	frames 0 do
	    i bufsize + frames > if frames i - to bufsize then
	    fd sd bufsize audio-read + ( samps++ )
	    sfd 0 bufsize 1- chans sd sound-write
	bufsize +loop
    else
	chans bufsize make-sound-data { sd1 }
	frames 0 do
	    i bufsize + frames > if frames i - to bufsize then
	    fd sd bufsize audio-read + ( samps++ )
	    0 sd 0 sd1 sound-data>sound-data
	    1 sd 1 sd1 sound-data>sound-data
	    0.5e sd sound-data-scale!
	    0 sd 2 sd1 sound-data>sound-data
	    chans 4 = if 1 sd 3 sd1 sound-data>sound-data then
	    sfd 0 bufsize 1- chans sd1 sound-write
	bufsize +loop
	sd1 sound-data-free
    then
    sfd *clm-data-format* rot ( samps ) chans * samples>bytes sound-close-output
    fd audio-close
    sd sound-data-free
;

\ === General Definitions ===
: oscil-bank { amps oscils inputs -- r }
    assert1( amps length oscils length = amps length inputs length = and )
    0e oscils each i amps vct@ i inputs vct@ 0e oscil f* f+ end-each
;
: ssb-bank { ssbs fir-flts f: input -- f: sum }
    assert1( ssbs length fir-flts length = )
    0e ssbs each input i fir-flts array@ fir-filter 0e ssb-am f+ end-each
;

\ === CLM-Generator Keywords ===
$" wave"       	      constant :wave
$" type"       	      constant :type
$" partials"   	      constant :partials
$" envelope"   	      constant :envelope
$" start"      	      constant :start
$" end"        	      constant :end
$" n"          	      constant :n
$" cosines"    	      constant :cosines
$" sines"      	      constant :sines
$" order"      	      constant :order
$" xcoeffs"    	      constant :xcoeffs
$" ycoeffs"    	      constant :ycoeffs
$" initial-contents"  constant :initial-contents
$" size"       	      constant :size
$" direction"  	      constant :direction
$" channel"    	      constant :channel
$" file"       	      constant :file
$" revout"     	      constant :revout
$" output"     	      constant :output
$" channels"   	      constant :channels
$" input"      	      constant :input
$" width"      	      constant :width
$" filter"     	      constant :filter
$" fft-size"   	      constant :fft-size
$" max-size"          constant :max-size
$" edit"       	      constant :edit
$" analyze"    	      constant :analyze
$" synthesize" 	      constant :synthesize
$" interp"     	      constant :interp
$" overlap"    	      constant :overlap
$" infile"            constant :infile
$" outfile"           constant :outfile
$" outloc"            constant :outloc
$" inloc"             constant :inloc
$" frames"            constant :frames
$" mixer"             constant :mixer
$" envs"              constant :envs
$" time"              constant :time
$" amp-env"           constant :amp-env

make-hash value *clm-float-keyword*
: float-keyword! ( key -- )     true *clm-float-keyword* hash! ;
: ?float-keyword-member ( key -- f ) *clm-float-keyword* hash@ ;

\ keywords for float arguments
$" offset"     	      dup constant :offset          float-keyword!
$" frequency"  	      dup constant :frequency       float-keyword!
$" initial-phase"     dup constant :initial-phase   float-keyword!
$" base"       	      dup constant :base	    float-keyword!
$" duration"   	      dup constant :duration	    float-keyword!
$" scaler"     	      dup constant :scaler	    float-keyword!
$" a"          	      dup constant :a	            float-keyword!
$" r"          	      dup constant :r	            float-keyword!
$" ratio"      	      dup constant :ratio	    float-keyword!
$" radius"     	      dup constant :radius	    float-keyword!
$" gain"       	      dup constant :gain	    float-keyword!
$" feedback"   	      dup constant :feedback	    float-keyword!
$" feedforward"       dup constant :feedforward     float-keyword!
$" degree"            dup constant :degree	    float-keyword!
$" distance"          dup constant :distance	    float-keyword!
$" jitter"            dup constant :jitter	    float-keyword!
$" initial-element"   dup constant :initial-element float-keyword!
$" hop"      	      dup constant :hop	            float-keyword!
$" a0"      	      dup constant :a0	            float-keyword!
$" a1"       	      dup constant :a1	            float-keyword!
$" ramp"     	      dup constant :ramp	    float-keyword!
$" reverb-amount"     dup constant :reverb-amount   float-keyword!
$" base"       	      dup constant :length	    float-keyword!
$" expansion"  	      dup constant :expansion       float-keyword!
$" pitch"     	      dup constant :pitch	    float-keyword!
$" srate"             dup constant :srate           float-keyword!
$" amplitude"         dup constant :amplitude       float-keyword!
$" begin"             dup constant :begin           float-keyword!
:begin                constant :beg
:duration             constant :dur
:frequency            constant :freq
:amplitude            constant :amp

\ === With-Sound Keywords ===
$" play"              constant :play
$" statistics"        constant :statistics
$" verbose"           constant :verbose
$" continue-old-file" constant :continue-old-file
$" delete-reverb"     constant :delete-reverb
$" reverb"            constant :reverb
$" reverb-data"       constant :reverb-data
$" reverb-channels"   constant :reverb-channels
$" clm-srate"         constant :clm-srate
$" rt-buffer-size"    constant :rt-buffer-size
$" table-size"        constant :table-size
$" locsig-type"       constant :locsig-type
$" header-type"       constant :header-type
$" data-format"       constant :data-format
$" audio-format"      constant :audio-format
$" device"            constant :device
$" notehook"          constant :notehook
$" reverb-file-name"  constant :reverb-file-name
$" player"            constant :player
$" comment"           constant :comment
$" decay-time"        dup constant :decay-time float-keyword!

$" intern"            constant str-intern

: get-args ( val1 tp -- val2 )
    { tp }
    depth 0 ?do
	i pick ?string if
	    i pick tp = if
		i roll drop		\ drop keyword
		i 1- roll		\ value on top of stack
		nip			\ drop default value
		leave
	    then
	then
    loop
;
: get-fargs ( val1 type -- val2 )
    0 { tp fidx }
    depth 0 ?do
	i pick ?string if
	    i pick ?float-keyword-member if fidx 1+ to fidx then
	    i pick tp = if
		i roll drop		\ drop keyword
		fidx froll		\ value on top of float stack
		fnip			\ drop default value
		leave
	    then
	then
    loop
;

\ see gfm.fs (make-with-sound-args) etc. or clm-ins.fs (fm-violin-args) and others
\ make-hash { args }
\ 1e    :fm-index  args set-fargs
\ nil   :amp-env   args set-args 
: set-args  { val key hash -- }    val key get-args key swap hash hash! ;
: set-fargs { f: val key hash -- } val key get-fargs key hash fhash! ;

\ === Generator Keyword Args ===
: make-oscil ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    0e   :initial-phase get-fargs { f: phase }
    freq phase clm-make-oscil
;
: make-env ( keyword-args -- gen )
    nil   :envelope get-args  { v }
    1e 	  :scaler   get-fargs { f: scl }
    0e 	  :duration get-fargs { f: dur }
    0e 	  :offset   get-fargs { f: offset }
    1e 	  :base     get-fargs { f: base }
    0  	  :start    get-args  { start }
    0  	  :end      get-args  { end }
    v ?vct if v vct-copy else vct[ 0e 0e 1e 0e ] then
    scl dur offset base start end clm-make-env
;
: make-table-lookup ( keyword-args -- gen )
    440e  	      :frequency     get-fargs { f: freq }
    0e    	      :initial-phase get-fargs { f: phase }
    nil               :wave          get-args  { v }
    mus-interp-linear :type          get-args  { tp }
    v ?vct if v vct-copy else vct[ 1e 1e ] false partials>wave then { wave }
    freq phase wave tp clm-make-table-lookup
;
: make-waveshape ( keyword-args -- gen ) 
    440e  	      :frequency get-fargs { f: freq }
    nil               :wave      get-args  { wv }
    nil               :partials  get-args  { pv }
    pv ?vct if
	pv *table-size* partials>waveshape
    else
	wv ?vct if
	    wv
	else
	    vct[ 1e 1e ] *table-size* partials>waveshape
	then
    then freq clm-make-waveshape
;
: make-triangle-wave ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    1e   :amplitude     get-fargs { f: amp }
    0e   :initial-phase get-fargs { f: phase }
    freq amp phase clm-make-triangle-wave
;
: make-square-wave ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    1e   :amplitude     get-fargs { f: amp }
    0e   :initial-phase get-fargs { f: phase }
    freq amp phase clm-make-square-wave
;
: make-sawtooth-wave ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    1e   :amplitude     get-fargs { f: amp }
    pi   :initial-phase get-fargs { f: phase }
    freq amp phase clm-make-sawtooth-wave
;
: make-pulse-train ( keyword-args -- gen )
    440e   :frequency     get-fargs { f: freq }
    1e     :amplitude     get-fargs { f: amp }
    two-pi :initial-phase get-fargs { f: phase }
    freq amp phase clm-make-pulse-train
;
: make-sum-of-cosines ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    0e   :initial-phase get-fargs { f: phase }
    1    :cosines       get-args 1 max { cosines }
    freq phase cosines clm-make-sum-of-cosines
;
: make-sum-of-sines ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    0e   :initial-phase get-fargs { f: phase }
    1    :sines         get-args 1 max { sines }
    freq phase sines clm-make-sum-of-sines
;
: make-sine-summation ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    0e   :initial-phase get-fargs { f: phase }
    1    :n             get-args  { n }
    0.5e :a             get-fargs { f: a }
    1e   :ratio         get-fargs { f: ratio }
    freq phase n a ratio clm-make-sine-summation
;
: make-asymmetric-fm ( keyword-args -- gen )
    440e :frequency     get-fargs { f: freq }
    0e   :initial-phase get-fargs { f: phase }
    1e   :r             get-fargs { f: r }
    1e   :ratio         get-fargs { f: ratio }
    freq phase r ratio clm-make-asymmetric-fm
;
: make-rand ( keyword-args -- gen )
    440e  :frequency get-fargs { f: freq }
    1e    :amplitude get-fargs { f: amp }
    nil   :envelope  get-args  { v }
    freq amp v clm-make-rand
;
: make-rand-interp ( keyword-args -- gen )
    440e  :frequency get-fargs { f: freq }
    1e    :amplitude get-fargs { f: amp }
    nil   :envelope  get-args  { v }
    freq amp v clm-make-rand-interp
;
: make-formant ( keyword-args -- gen )
    1e   :radius    get-fargs { f: radius }
    440e :frequency get-fargs { f: freq }
    1e   :gain      get-fargs { f: gain }
    radius freq gain clm-make-formant
;
$" xcoeffs-vct or ycoeffs-vct missing" constant str-coeffs-missing
: make-filter ( keyword-args -- gen )
    1     :order   get-args 1 max { ord }
    nil   :xcoeffs get-args { xcofs }
    nil   :ycoeffs get-args { ycofs }
    xcofs ?vct ycofs ?vct and unless str-coeffs-missing error then
    ord xcofs ycofs clm-make-filter
;
: make-fir-filter ( keyword-args -- gen )
    1     :order   get-args 1 max { ord }
    nil :xcoeffs get-args { xcofs }
    xcofs ?vct unless str-coeffs-missing error then
    ord xcofs clm-make-fir-filter
;
: make-iir-filter ( keyword-args -- gen )
    1     :order   get-args 1 max { ord }
    nil   :ycoeffs get-args { ycofs }
    ycofs ?vct unless str-coeffs-missing error then
    ord ycofs clm-make-iir-filter
;
: (init-delays) { v f: init-els size max-size tp -- line typ }
    v ?vct if
	v length to max-size
	v vct-copy
    else
	max-size make-vct
	init-els f0<> if init-els vct-fill then
    then ( line )
    tp dup mus-interp-unknown = if
	drop max-size size = if mus-interp-none else mus-interp-linear then
    then ( typ )
;
: make-delay ( keyword-args -- gen )
    *table-size*       :size             get-args  { size }
    nil                :initial-contents get-args  { v }
    0e                 :initial-element  get-fargs { f: init-els }
    size               :max-size         get-args  { max-size }
    mus-interp-unknown :type             get-args  { tp }
    v init-els size max-size tp (init-delays) { line typ }
    size line typ clm-make-delay
;
: make-comb ( keyword-args -- gen )
    1e                 :scaler           get-fargs { f: scl }
    *table-size*       :size             get-args  { size }
    nil                :initial-contents get-args  { v }
    0e                 :initial-element  get-fargs { f: init-els }
    size               :max-size         get-args  { max-size }
    mus-interp-unknown :type             get-args  { tp }
    v init-els size max-size tp (init-delays) { line typ }
    scl size line typ clm-make-comb
;
: make-notch ( keyword-args -- gen )
    1e                 :scaler           get-fargs { f: scl }
    *table-size*       :size             get-args  { size }
    nil                :initial-contents get-args  { v }
    0e                 :initial-element  get-fargs { f: init-els }
    size               :max-size         get-args  { max-size }
    mus-interp-unknown :type             get-args  { tp }
    v init-els size max-size tp (init-delays) { line typ }
    scl size line typ clm-make-notch
;
: make-all-pass ( keyword-args -- gen )
    1e                 :feedback         get-fargs { f: feedback }
    1e                 :feedforward      get-fargs { f: feedforward }
    *table-size*       :size             get-args  { size }
    nil                :initial-contents get-args  { v }
    0e                 :initial-element  get-fargs { f: init-els }
    size               :max-size         get-args  { max-size }
    mus-interp-unknown :type             get-args  { tp }
    v init-els size max-size tp (init-delays) { line typ }
    feedback feedforward size line typ clm-make-all-pass
;
: make-average ( keyword-args -- gen )
    *table-size* :size get-args { size }
    nil          :initial-contents get-args { v }
    0e           :initial-element  get-fargs { f: init-els }
    v ?vct if
	v vct-copy
    else
	size make-vct
	init-els f0<> if init-els vct-fill then
    then clm-make-average
;
: make-wave-train ( keyword-args -- gen )
    440e              :frequency     get-fargs { f: freq }
    0e                :initial-phase get-fargs { f: phase }
    nil               :wave          get-args { wave }
    mus-interp-linear :type          get-args { tp }
    wave ?vct unless *table-size* make-vct to wave then
    freq phase wave tp clm-make-wave-train
;
: make-ssb-am ( keyword-args -- gen )
    440e :frequency   get-fargs { f: freq }
    40   :order 1 max get-args  { ord }
    freq ord clm-make-ssb-am
;
: make-readin ( keyword-args -- gen )
    $" test.snd" :file      get-args { fname }
    0 	  	 :channel   get-args 0 max { chan }
    0 	  	 :start     get-args 0 max { start }
    1 	  	 :direction get-args { dir }
    fname chan start dir clm-make-readin
;
: make-locsig ( keyword-args -- gen )
    0e 		      :degree        get-fargs { f: degr }
    1e 		      :distance      get-fargs { f: dist }
    0e 		      :reverb-amount get-fargs { f: reverb }
    1                 :channels      get-args 1 max { chans }
    nil               :output        get-args  { output }
    nil               :revout        get-args  { revout }
    mus-interp-linear :type          get-args  { tp }
    degr dist reverb chans output revout tp clm-make-locsig
;

$" need either a filname or a file>frame generator" constant str-gen-or-name-missing
: mus-mix ( keyword-args -- outfile )
    nil   :infile  get-args { infile }
    nil   :outfile get-args { outfile }
    0     :outloc  get-args { outloc }
    0     :frames  get-args { frames }
    0     :inloc   get-args { inloc }
    nil   :mixer   get-args { mixer }
    nil   :envs    get-args { envs }
    infile ?string infile ?file>frame or unless str-gen-or-name-missing error then
    nil nil nil nil { del-in del-out ingen outgen }
    infile ?string if
	infile make-file>frame to ingen
	true to del-in
    else
        infile to ingen
    then
    outfile ?string if
	outfile continue-frame>file to outgen
	true to del-out
    else
        outfile to outgen
    then
    outgen unless
	*output* ?output if
	    *output* to outgen
	else
	    *clm-file-name*
	    ingen channels@
	    *clm-data-format*
	    *clm-header-type* make-frame>file to outgen
	    true to del-out
	then
    then
    frames unless ingen length@ to frames then
    outgen ingen outloc frames inloc mixer envs clm-mix
    outgen file-name@			\ returns outfile-string
    del-in if ingen mus-free then
    del-out if outgen mus-free then
;

$" srate f0<"         constant str-srate
$" 0 <= width < 2000" constant str-width
: make-src ( keyword-args -- gen )
    nil   :input get-args  { input }
    1e    :srate get-fargs { f: srate }
    10    :width get-args  { width }
    srate f0<              if str-srate error then
    width 0 2000 within unless str-width error then
    input srate width clm-make-src
;
: src ( keyword-args pm gen -- r )
    nil :input get-args { input }
    { gen f: sr-change }
    input sr-change gen clm-src
;
: src-20 ( keyword-args gen -- r )
    nil :input get-args { input }
    { gen }
    input gen clm-src-20
;
: src-05 ( keyword-args gen -- r )
    nil :input get-args { input }
    { gen }
    input gen clm-src-05
;

$" impulse filter missing" constant str-filter-missing
: make-convolve ( keyword-args -- gen )
    nil   :input    get-args { input }
    nil   :filter   get-args { filter }
    16    :fft-size get-args { fftsize }
    filter ?vct unless str-filter-missing error then
    input filter fftsize clm-make-convolve
;
: convolve ( keyword-args gen -- r )
    nil   :input    get-args { input }
    { gen }
    input gen clm-convolve
;

$" extension f0<=" constant str-extension
$" length f0<="    constant str-length
$" scaler f0="     constant str-scaler
$" hop f0<"        constant str-hop
$" ramp f0<"       constant str-ramp
: make-granulate ( keyword-args -- gen )
    nil   :input     get-args  { input }
    1e    :expansion get-fargs { f: expansion }
    0.15e :length    get-fargs { f: length }
    0.6e  :scaler    get-fargs { f: scaler }
    0.05e :hop       get-fargs { f: hop }
    0.4e  :ramp      get-fargs { f: ramp }
    1e    :jitter    get-fargs { f: jitter }
    0     :max-size  get-args  { max-size }
    nil   :edit      get-args  { edit }
    expansion f0<= if str-extension error then
    length f0<=    if str-length    error then
    scaler f0=     if str-scaler    error then
    hop f0<        if str-hop       error then
    ramp f0<       if str-ramp      error then
    input expansion length scaler hop ramp jitter max-size edit clm-make-granulate
;
: granulate ( keyword-args gen -- r )
    nil   :input     get-args  { input }
    nil   :edit      get-args  { edit }
    { gen }
    input edit gen clm-granulate
;

$" overlap 0<=" constant str-overlap
$" interp 0<="  constant str-interp
: make-phase-vocoder ( keyword-args -- gen )
    nil   :input      get-args  { input }
    512   :fft-size   get-args  { fftsize }
    4     :overlap    get-args  { overlap }
    128   :interp     get-args  { interp }
    1e    :pitch      get-fargs { f: pitch }
    nil   :analyze    get-args  { analyze }
    nil   :edit       get-args  { edit }
    nil   :synthesize get-args  { synth }
    overlap 0<= if str-overlap error then
    interp  0<= if str-interp  error then
    input fftsize overlap interp pitch analyze edit synth clm-make-phase-vocoder
;
: phase-vocoder ( keyword-args gen -- r )
    nil   :input      get-args  { input }
    nil   :analyze    get-args  { analyze }
    nil   :edit       get-args  { edit }
    nil   :synthesize get-args  { synth }
    { gen }
    input analyze edit synth gen clm-phase-vocoder
;
' pv-amp-increments@ 	  alias phase-vocoder-amp-increments@
' pv-amp-increments! 	  alias phase-vocoder-amp-increments!
' pv-amp-increments>vct   alias phase-vocoder-amp-increments>vct
' pv-amps@ 		  alias phase-vocoder-amps@
' pv-amps! 		  alias phase-vocoder-amps! 
' pv-amps>vct             alias phase-vocoder-amps>vct
' pv-freqs@ 		  alias phase-vocoder-freqs@
' pv-freqs! 		  alias phase-vocoder-freqs!
' pv-freqs>vct            alias phase-vocoder-freqs>vct
' pv-phases@ 		  alias phase-vocoder-phases@
' pv-phases! 		  alias phase-vocoder-phases!
' pv-phases>vct           alias phase-vocoder-phases>vct
' pv-phase-increments@    alias phase-vocoder-phase-increments@
' pv-phase-increments!    alias phase-vocoder-phase-increments!
' pv-phase-increments>vct alias phase-vocoder-phase-increments>vct
' pv-outctr@ 		  alias phase-vocoder-outctr@
' pv-outctr! 		  alias phase-vocoder-outctr!

: make-fcomb ( keyword-args -- gen )
    1e           :scaler get-fargs { f: scl }
    *table-size* :size   get-args  { size }
    0e           :a0     get-fargs { f: a0 }
    0e           :a1     get-fargs { f: a1 }
    scl size a0 a1 clm-make-fcomb
;
: make-buffer ( keyword-args -- gen )
    *table-size* :size get-args { size }
    0            :time get-args { ftime }
    size ftime clm-make-buffer
;

\ === With-Sound (with-snd, with-dac, with-reverb, sound-let, with-psound) ===
nil value *ws-verbose*
nil value *ws-channels*
nil value *ws-srate*
nil value *ws-rt-bufsize*
nil value *ws-table-size*
nil value *ws-locsig-type*
nil value *ws-audio-format*
nil value *ws-notehook*
nil value *ws-output*
nil value *ws-reverb*

: ws-preserve ( -- )
    *verbose*      to *ws-verbose*
    *channels*     to *ws-channels*
    *srate*        to *ws-srate*
    *rt-bufsize*   to *ws-rt-bufsize*
    *table-size*   to *ws-table-size*
    *locsig-type*  to *ws-locsig-type*
    *audio-format* to *ws-audio-format*
    *notehook*     to *ws-notehook*
    *output*       to *ws-output*
    *reverb*       to *ws-reverb*
;
: ws-reset ( -- )
    *ws-verbose*      to *verbose*
    *ws-channels*     to *channels*
    *ws-srate*        srate!
    *ws-rt-bufsize*   to *rt-bufsize*
    *ws-table-size*   to *table-size*
    *ws-locsig-type*  to *locsig-type*
    *ws-audio-format* to *audio-format*
    *ws-notehook*     to *notehook*
    *ws-output*       to *output*
    *ws-reverb*       to *reverb*
;

: make-with-sound-args ( keyword-args -- ws )
    make-hash { args }
    *clm-play*             :play              args set-args
    *clm-statistics*       :statistics        args set-args
    *clm-verbose*          :verbose           args set-args
    false                  :continue-old-file args set-args
    *clm-delete-reverb*    :delete-reverb     args set-args
    *clm-reverb*           :reverb            args set-args
    *clm-reverb-data*      :reverb-data       args set-args
    *clm-channels*         :channels          args set-args
    *clm-reverb-channels*  :reverb-channels   args set-args
    *clm-srate*            :clm-srate         args set-args
    *clm-rt-bufsize*       :rt-buffer-size    args set-args
    *clm-table-size*       :table-size        args set-args
    *clm-locsig-type*      :locsig-type       args set-args
    *clm-header-type*      :header-type       args set-args
    *clm-data-format*      :data-format       args set-args
    *clm-audio-format*     :audio-format      args set-args
    *clm-notehook*         :notehook          args set-args
    *clm-file-name*        :output            args set-args
    *clm-reverb-file-name* :reverb-file-name  args set-args
    *clm-player*           :player            args set-args
    *clm-comment*          :comment           args set-args
    *clm-device*           :device            args set-args
    *clm-decay-time*       :decay-time        args set-fargs
    :verbose        args hash@ to *verbose*
    :channels       args hash@ to *channels*
    :clm-srate      args hash@ srate!
    :rt-buffer-size args hash@ to *rt-bufsize*
    :table-size     args hash@ to *table-size*
    :locsig-type    args hash@ to *locsig-type*
    :audio-format   args hash@ to *audio-format*
    :notehook       args hash@ to *notehook*
    args
;

: ws-error ( n str -- ) str-error swap $+ .string throw ;

$" with-sound: cannot open file"  constant str-open-file
$" with-sound: body-xt aborted"   constant str-body-xt
$" with-sound: reverb-xt aborted" constant str-reverb-xt
$" temporary reverb file"         constant str-comment

: with-snd-with-args { body-xt args -- }
    :output args hash@           { fname }
    :header-type args hash@      { hdr }
    :data-format args hash@      { fmt }
    :statistics args hash@       { statistics }
    :reverb args hash@           { reverb-xt }
    reverb-xt if :reverb-file-name args hash@ else nil then { rev-name }
    :continue-old-file args hash@ if
	fname continue-sample>file
    else
	fname *channels* fmt hdr :comment args hash@ make-sample>file
    then to *output*
    *output* ?sample>file unless str-open-file error then
    reverb-xt if
	rev-name :reverb-channels args hash@ fmt hdr str-comment make-sample>file to *reverb*
	*reverb* ?sample>file unless str-open-file error then
    then
    make-timer { tm }
    statistics if tm start-timer then
    try
	body-xt execute
    recover ( err )
	*output* mus-close
	*output* gen-free
	*reverb* if
	    *reverb* mus-close
	    *reverb* gen-free
	then
	ws-reset
	tm gen-free
	args gen-free
	str-body-xt ws-error
    endtry
    reverb-xt if
	*reverb* mus-close
	*reverb* gen-free
	rev-name sound-duration { f: dur }
	rev-name make-file>sample to *reverb*
	*reverb* ?file>sample unless str-open-file error then
	try
	    0e dur :decay-time args fhash@ f+ :reverb-data args hash@ reverb-xt execute
	recover ( err )
	    *reverb* mus-close
	    *reverb* gen-free
	    *output* mus-close
	    *output* gen-free
	    ws-reset
	    tm gen-free
	    args gen-free
	    str-reverb-xt ws-error
	endtry
	*reverb* mus-close
	*reverb* gen-free
    then
    statistics if tm stop-timer then
    *output* mus-close
    *output* gen-free
    statistics if fname rev-name *channels* srate@ fname sound-frames tm play-info then
    rev-name :delete-reverb args hash@ and if rev-name file-delete then
    :play args hash@ if
	:player args hash@ str-intern string= if
	    *clm-verbose* { old-verbose }
	    false to *clm-verbose*
	    fname play-sound
	    old-verbose to *clm-verbose*
	else
	    :player args hash@ $"  " $+ fname $+ string>$ system
	then
    then
    tm gen-free
;

\ Usage: ' resflt-test with-sound
\        ' resflt-test :play false :channels 2 with-sound
\        lambda: resflt-test ; :output $" resflt.snd" with-sound
: with-snd ( body-xt keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    { body-xt }
    body-xt args with-snd-with-args
    args gen-free
    ws-reset
;

\ === With-Reverb ===
: with-reverb-body-xt { infile chans rev-chans -- }
    infile make-file>frame { fil-in }
    chans make-frame { in-fr }
    rev-chans make-frame { out-fr }
    chans rev-chans max make-mixer { mx }
    mx channels@ 0 do mx channels@ 0 do *clm-reverb-amount* 0.05e fmax j i mx mixer! loop loop
    0e infile sound-duration run
	in-fr i fil-in file>frame i *output* frame>file
	in-fr out-fr mx frame>frame i *reverb* frame>file
    loop
    fil-in mus-close
    fil-in gen-free
    in-fr gen-free
    out-fr gen-free
    mx gen-free
;
: with-reverb ( infile keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    { infile }
    :reverb args hash@ unless $" with-reverb needs a reverb-xt" error then
    infile *channels* :reverb-channels args hash@ ['] with-reverb-body-xt args with-snd-with-args
    args gen-free
    ws-reset
;

\ === Sound-Let ===
$" sound-let needs an array of array: [ [ ws-xt ws-args ] ... ]" constant str-sound-let-usage
: sound-let { ws-ary body-xt -- ?? }
    ws-ary ?array unless str-sound-let-usage error then
    ws-ary length make-array { outfiles }
    ws-ary each array>stack :output tempnam dup i outfiles array! with-snd end-each
    outfiles array>stack body-xt execute
    outfiles each file-delete end-each
    outfiles gen-free
;

\ === CLM-Load ===
$" clm-load: no such file " constant str-clm-load-failed
: clm-load ( fname keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    { fname }
    fname ?file-exist if
	!script-cr ." \  loading " fname .gen script-cr
	fname string>$ ['] included args with-snd-with-args
    else
	str-clm-load-failed fname gen>string $+ error
    then
    args gen-free
    ws-reset
;

nil value +dac-vct+
nil value +dac-data+
nil value +bufsize+
0e fvalue +start-time+

$" /dev/dsp" constant str-dac-device
\ similar like with-snd
: with-dac ( body-xt keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    { body-xt }
    str-dac-device           { fname }
    :audio-format args hash@ { fmt }
    :statistics args hash@   { statistics }
    :device args hash@ *srate* *channels* fmt *rt-bufsize* *channels* * shorts audio-open-output
    to *output*
    *output* 0< if str-audio-open-dac error then
    nil to *reverb*
    statistics if
        !script-cr
	." \ filename: " fname .gen cr
	." \    chans: " *channels* . bs ." , srate: " *srate* . cr
	." \   format: " fmt .data-format
        script-cr
    then
    \ new stuff
    *rt-bufsize* make-vct to +dac-vct+
    *channels* *rt-bufsize* make-sound-data to +dac-data+
    
    \ end new stuff
    make-timer { tm }
    statistics if tm start-timer then
    frtime fto +start-time+
    try
	body-xt execute
    recover ( err )
	*output* audio-close
	ws-reset
	tm gen-free
	args gen-free
	str-body-xt ws-error
    endtry
    *output* audio-close
    statistics if !script-cr tm stop-timer ." \    " tm .timer script-cr then
    ws-reset
    tm gen-free
    args gen-free
;

\ === Locsig Keyword Args ===
struct
    sfloat% field loc-degree
    sfloat% field loc-distance
    sfloat% field loc-rev-amount
end-struct clm::locsig%

user *clm::locsig*
clm::locsig% %allot *clm::locsig* !

: locsig-reset ( -- )
    0.00e               *clm::locsig* @ loc-degree sf!
    1.00e               *clm::locsig* @ loc-distance sf!
    *clm-reverb-amount* *clm::locsig* @ loc-rev-amount sf!
;
locsig-reset

: :locsig-degree@        ( -- degree )        *clm::locsig* @ loc-degree sf@ ;
: :locsig-degree         ( degree -- )        *clm::locsig* @ loc-degree sf! ;
: :locsig-distance@      ( -- distance )      *clm::locsig* @ loc-distance sf@ ;
: :locsig-distance       ( distance -- )      *clm::locsig* @ loc-distance sf! ;
: :locsig-reverb-amount@ ( -- reverb-amount ) *clm::locsig* @ loc-rev-amount sf@ ;
: :locsig-reverb-amount  ( reverb-amount -- ) *clm::locsig* @ loc-rev-amount sf! ;

\ === Parallel Instruments === 
nil value *instruments*

: psound-times>samples ( start dur -- beg len ) seconds>samples seconds>samples tuck + ;
\ array[ array[ xt beg end ] ... ]
: start-dac ( xt start dur -- ) psound-times>samples 3 >array *instruments* array-push! ;
    
\ A parallel instrument must return the next sample, the
\ LAMBDA-CREATE-DOES construct is one solution.  The body-xt for
\ WITH-PSOUND receives the current time and must return a flag, true
\ for continue and false for finishing WITH-PSOUND.  The body-xt calls
\ the instrument(s); instruments must call START-DAC to install them
\ in the queue.  Below is a general example with all elements.

\ simple parallel instrument:
: make-p-simp { f: start f: dur f: freq f: amp -- xt; self -- r }
    :frequency freq make-oscil { os }
    :envelope vct[ 0e 0e 50e 1e 100e 0e ] :duration dur :scaler amp make-env { en }
    lambda-create os , en ,
    latestxt start dur start-dac
  does> { self -- r }
    self @ { os }
    self cell+ @ { en }
    os oscil-0  en env  f*
;

\ body-xt for with-psound:
: run-p-test { f: cur-time -- f }
    now@ 0.0e f+ 0.5e 220e 0.3e make-p-simp
    now@ 0.2e f+ 0.5e 440e 0.3e make-p-simp
    now@ 0.4e f+ 0.5e 660e 0.3e make-p-simp
    cur-time wait			\ sets now@ to cur-time
    cur-time 10e f<			\ plays 10 seconds
;

\ calling:
\ ' run-p-test with-psound

: with-psound ( body-xt keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    { body-xt }
    str-dac-device                 { fname }
    *clm-reverb-amount* 0.05e fmax { f: rev-scl }
    :audio-format args hash@       { fmt }
    :statistics args hash@         { statistics }
    :reverb args hash@             { reverb-xt }
    :device args hash@ *srate* *channels* fmt *rt-bufsize* *channels* * shorts audio-open-output
    to *output*
    *output* 0< if str-audio-open-dac error then
    nil to *reverb*
    *rt-bufsize* make-vct { dac-vct }
    *rt-bufsize* make-vct { rev-vct }
    *channels* *rt-bufsize* make-sound-data { dac-data }
    0 make-array { ins-idx }
    0 { w^ dac-index }
    0 make-array to *instruments*
    :locsig-degree@ :locsig-distance@ :locsig-reverb-amount@ *channels* *output* *reverb*
    *locsig-type* clm-make-locsig { loc }
    statistics if
        !script-cr
	." \ filename: " fname .gen cr
	." \    chans: " *channels* . bs ." , srate: " *srate* . cr
	." \   format: " fmt .data-format
        script-cr
    then
    make-timer { tm }
    statistics if tm start-timer then
    try
	begin
	    tm stop-timer tm real-time@ body-xt execute
	while
		*rt-bufsize* 0 ?do
		    i dac-index @ + { idx }
		    *instruments* each array>stack { xt beg end }
			idx beg >= if
			    idx end < if
				xt execute j dac-vct vct+!
			    else
				\ index 0 means false in begin-while-repeat below
				i 1+ ins-idx array-push!
			    then
			then
		    end-each
		    begin
			ins-idx array-pop! dup
		    while
			    1- *instruments* array-delete! gen-free
		    repeat drop
		loop
		*rt-bufsize* dac-index +!
		*channels* 0 ?do i { chn }
		    chn loc locsig@ dac-vct vct-scale { out-vct }
		    reverb-xt if
			out-vct each rev-scl f* chn reverb-xt execute i out-vct vct+! end-each
		    then
		    out-vct chn dac-data vct>sound-data
		    out-vct gen-free
		loop
		*output* dac-data *rt-bufsize* audio-write drop
		dac-vct vct-clear
	repeat
    recover ( err )
	*output* audio-close
	ws-reset
	tm gen-free
	args gen-free
	str-body-xt ws-error
    endtry
    *output* audio-close
    statistics if !script-cr tm stop-timer ." \    " tm .timer script-cr then
    dac-vct free-vct
    rev-vct free-vct
    dac-data sound-data-free
    ins-idx free-array
    *instruments* gen-free
    ws-reset
    tm gen-free
    args gen-free
;

\ === With-Snd Run-Instrument ===
: (run-snd-make-locsig) ( -- )
    :locsig-degree@
    :locsig-distance@
    :locsig-reverb-amount@
    *channels*
    *output*
    *reverb*
    *locsig-type* clm-make-locsig to *locsig*
    \ we set channel 3/4, if any, to 0.5 * channel 1/2
    *output* ?output if
	*output* channels@ 2 > if
	    0 *locsig* locsig@ 0.5e f* 2 *locsig* locsig!
	    *output* channels@ 3 > if
		1 *locsig* locsig@ 0.5e f* 3 *locsig* locsig!
	    then
	then
    then
    *reverb* ?output if
	*reverb* channels@ 2 > if
	    0 *locsig* locsig-reverb@ 0.5e f* 2 *locsig* locsig-reverb!
	    *reverb* channels@ 3 > if
		1 *locsig* locsig-reverb@ 0.5e f* 3 *locsig* locsig-reverb!
	    then
	then
    then
;

: (run-snd) ( start dur -- )
    postpone (run-snd-make-locsig) postpone times>samples postpone ?do
; immediate

: (finish-snd-run) *locsig* mus-free locsig-reset ;

: (end-run-snd) ( value -- )
    postpone r@ postpone *locsig* postpone locsig
    postpone loop
    postpone (finish-snd-run)
; immediate

\ === With-Dac Run-Instrument ===
: (run-dac-make-buffer) ( start dur -- start dur )
    (run-snd-make-locsig)
    fover { f: start }
    fdup seconds>samples *rt-bufsize* min to +bufsize+
    +bufsize+ make-vct to +dac-vct+
    *channels* +bufsize+ make-sound-data to +dac-data+
    begin frtime +start-time+ f- start f>= until
;

: (run-dac) ( start dur -- )
    postpone (run-dac-make-buffer)
    postpone times>samples postpone ?do
    postpone +bufsize+ 0 postpone literal postpone ?do
; immediate

: (write-to-dac)
    *channels* 0 ?do i *locsig* locsig@ +dac-vct+ dup vct-scale! i +dac-data+ vct>sound-data loop
    *output* +dac-data+ +bufsize+ audio-write drop
;
: (finish-dac-run)
    +dac-data+ sound-data-free
    +dac-vct+ free-vct
    *locsig* mus-free
    locsig-reset
;
: (end-run-dac) ( value -- )
    postpone r@ postpone +dac-vct+ postpone vct! postpone loop
    postpone (write-to-dac)
    postpone +bufsize+ postpone +loop
    postpone (finish-dac-run)
; immediate

defer run-instrument immediate ( start dur -- )
defer end-run immediate ( value -- )

[ifdef] *clm-dac-output*
    *clm-dac-output* [if]
	' with-dac alias with-sound
	' (run-dac) is run-instrument
	' (end-run-dac) is end-run
    [else]
	' with-snd alias with-sound
	' (run-snd) is run-instrument
	' (end-run-snd) is end-run
    [then]
[else]
    ' with-snd is with-sound
    ' (run-snd) is run-instrument
    ' (end-run-snd) is end-run
[endif]

require clm-ins.fs
require dlocsig.fs

only forth also definitions
also Utils
also GFMusic
also CLM-Sndlib

: clm-init ( -- )
    \ Takes value of environment variable $SFDIR or current directory
    \ if $SFDIR isn't defined.
    $" SFDIR" $getenv dup unless drop file-pwd then $" /" $+ to *clm-search-list*

    \ The temporary shell process id but it's arbitrary enough, I hope.
    $" echo $$" shell string>$ evaluate to *clm-process-id*

    make-default-comment to *clm-comment*
    1024 1024 * file-buffer-size!
    8 array-print-length!

    \ load .gfmrc if .gfmrc exists in current or $HOME dir
    $" .gfmrc" load-init-file

    *clm-channels*    to *channels*
    *clm-table-size*  to *table-size*
    *clm-rt-bufsize*  to *rt-bufsize*
    *clm-locsig-type* to *locsig-type*
    *clm-verbose*     to *verbose*
    *clm-notehook*    to *notehook*
    *clm-srate* srate!
;

clm-init

\ === Online Help ===
\   Usage: help function-name
\ Example: help make-oscil

' help $" help ( \"name\" -- )\n"
$" \\ Returns a short help, at least the stack effect,\n" $+
$" \\ if one exists, or \"no description available\".\n" $+
$" \\ If NAME doesn't exists, it returns \"unknown word\".\n" $+
$" help make-oscil" $+ help!
' help! $" help! ( xt str -- )\n"
$" \\ stores a help string to XT for reading with HELP\n" $+
$" ' oscil $\" oscil ( f: fm f: pm gen -- r )\" help!" $+ help!

' fvalue $" fvalue ( f: val \"name\" --; -- f: val )\n"
$" \\ creates a global variable much like VALUE but holds floating point numbers\n" $+
$" 0.3e fvalue foo  foo f." $+ help!
' fto $" fto ( f: val \"name\" -- )\n"
$" \\ stores floating point value in FVALUE variables much like TO and VALUE\n" $+
$" 0.0e fvalue foo\n" $+
$" 0.3e fto foo  foo f." $+ help!
' f+! $" f+! ( f: val addr -- ) \\ adds double float VAL to value on place ADDR" help!
' sf+! $" sf+! ( f: val addr -- ) \\ adds float VAL to value on place ADDR" help!
' df+! $" df+! ( f: val addr -- ) \\ adds double float VAL to value on place ADDR" help!

' play-sound $" play-sound ( fname -- )\n"
$" \\ plays sound files with 1 to 4 channels (if two sound cards are available)" $+ help!
' record-sound $" record-sound ( fname device f: duration -- )\n"
$" \\ Records from DEVICE for DURATION (in seconds) to sound file FNAME.\n" $+
$" \\ Data format, header type, channels and srate are taken from\n" $+
$" \\ *clm-data-format*, *clm-header-type*, *clm-channels*, and *clm-srate*,\n" $+
$" \\ format for audio-read is *clm-audio-format* (record-sound can be found in gfm.fs).\n" $+
$" \\ It can record from a device of 1 or 2 channels\n" $+
$" \\ and can write a sound file of 1 to 4 channels (if two sound cards are available).\n" $+
$" \\ In the latter case the 3rd and 4th channels are simply scaled by 0.5." $+ help!
' with-sound $" with-sound ( body-xt keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :play              -- *clm-play*\n" $+
$" \\ :statistics        -- *clm-statistics*\n" $+
$" \\ :verbose           -- *clm-verbose*\n" $+
$" \\ :continue-old-file -- false\n" $+
$" \\ :delete-reverb     -- *clm-delete-reverb*\n" $+
$" \\ :reverb            -- *clm-reverb*\n" $+
$" \\ :channels          -- *clm-channels*\n" $+
$" \\ :reverb-channels   -- *clm-reverb-channels*\n" $+
$" \\ :clm-srate         -- *clm-srate*\n" $+
$" \\ :rt-buffer-size    -- *clm-rt-bufsize*\n" $+
$" \\ :table-size        -- *clm-table-size*\n" $+
$" \\ :locsig-type       -- *clm-locsig-type*\n" $+
$" \\ :header-type       -- *clm-header-type*\n" $+
$" \\ :data-format       -- *clm-data-format*\n" $+
$" \\ :audio-format      -- *clm-audio-format*\n" $+
$" \\ :notehook          -- *clm-notehook*\n" $+
$" \\ :output            -- *clm-file-name*\n" $+
$" \\ :reverb-file-name  -- *clm-reverb-file-name*\n" $+
$" \\ :player            -- *clm-player*\n" $+
$" \\ :comment           -- *clm-comment*\n" $+
$" \\ :decay-time        -- *clm-decay-time*\n" $+
$" \\ :device            -- *clm-device* (dac only)\n" $+
$" 0e 2e 330e 0.2e ' fm-violin :clm-srate 44100 with-sound\n" $+
$" 0e 2e 220e 0.1e ' fm-violin with-sound" $+ help!
' clm-load $" clm-load ( source-file keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :play              -- *clm-play*\n" $+
$" \\ :statistics        -- *clm-statistics*\n" $+
$" \\ :verbose           -- *clm-verbose*\n" $+
$" \\ :continue-old-file -- false\n" $+
$" \\ :delete-reverb     -- *clm-delete-reverb*\n" $+
$" \\ :reverb            -- *clm-reverb*\n" $+
$" \\ :channels          -- *clm-channels*\n" $+
$" \\ :reverb-channels   -- *clm-reverb-channels*\n" $+
$" \\ :clm-srate         -- *clm-srate*\n" $+
$" \\ :rt-buffer-size    -- *clm-rt-bufsize*\n" $+
$" \\ :table-size        -- *clm-table-size*\n" $+
$" \\ :locsig-type       -- *clm-locsig-type*\n" $+
$" \\ :header-type       -- *clm-header-type*\n" $+
$" \\ :data-format       -- *clm-data-format*\n" $+
$" \\ :audio-format      -- *clm-audio-format*\n" $+
$" \\ :notehook          -- *clm-notehook*\n" $+
$" \\ :output            -- *clm-file-name*\n" $+
$" \\ :reverb-file-name  -- *clm-reverb-file-name*\n" $+
$" \\ :player            -- *clm-player*\n" $+
$" \\ :comment           -- *clm-comment*\n" $+
$" \\ :decay-time        -- *clm-decay-time*\n" $+
$" \\ :device            -- *clm-device* (dac only)\n" $+
$" $\" test.clm\" :clm-srate 44100 clm-load" $+ help!
' with-reverb $" with-reverb ( infile keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :play              -- *clm-play*\n" $+
$" \\ :statistics        -- *clm-statistics*\n" $+
$" \\ :verbose           -- *clm-verbose*\n" $+
$" \\ :delete-reverb     -- *clm-delete-reverb*\n" $+
$" \\ :reverb            -- *clm-reverb* (required)\n" $+
$" \\ :channels          -- *clm-channels*\n" $+
$" \\ :reverb-channels   -- *clm-reverb-channels*\n" $+
$" \\ :clm-srate         -- *clm-srate*\n" $+
$" \\ :rt-buffer-size    -- *clm-rt-bufsize*\n" $+
$" \\ :table-size        -- *clm-table-size*\n" $+
$" \\ :locsig-type       -- *clm-locsig-type*\n" $+
$" \\ :header-type       -- *clm-header-type*\n" $+
$" \\ :data-format       -- *clm-data-format*\n" $+
$" \\ :audio-format      -- *clm-audio-format*\n" $+
$" \\ :notehook          -- *clm-notehook*\n" $+
$" \\ :output            -- *clm-file-name*\n" $+
$" \\ :reverb-file-name  -- *clm-reverb-file-name*\n" $+
$" \\ :player            -- *clm-player*\n" $+
$" \\ :comment           -- *clm-comment*\n" $+
$" \\ :decay-time        -- *clm-decay-time*\n" $+
$" \\ Takes INFILE and use the given reverberator instrument to compute a new outfile.\n" $+
$" \\ If no reverberator instrument was given, an exception will be raised." $+ help!
' sound-let $" sound-let ( ws-array body-xt -- ?? )\n"
$" \\ WS-ARRAY -- array of array with WITH-SOUND-body-xt and WITH-SOUND arguments\n" $+
$" \\ BODY-XT  -- ( outfile(s) -- ?? )\n" $+
$" \\ Every element of WS-ARRAY will be given to with-sound\n" $+
$" \\ and the resulting outfiles are given to sound-let's BODY-XT.\n" $+
$" \\ Return values of sound-let are return values of BODY-XT\n" $+
$" \\ See move-sound in dlocsig.fs for an example." $+ help!
' with-move-sound $" with-move-sound ( infile path keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :play              -- *clm-play*\n" $+
$" \\ :statistics        -- *clm-statistics*\n" $+
$" \\ :verbose           -- *clm-verbose*\n" $+
$" \\ :delete-reverb     -- *clm-delete-reverb*\n" $+
$" \\ :reverb            -- *clm-reverb*\n" $+
$" \\ :channels          -- *clm-channels*\n" $+
$" \\ :reverb-channels   -- *clm-reverb-channels*\n" $+
$" \\ :clm-srate         -- *clm-srate*\n" $+
$" \\ :rt-buffer-size    -- *clm-rt-bufsize*\n" $+
$" \\ :table-size        -- *clm-table-size*\n" $+
$" \\ :locsig-type       -- *clm-locsig-type*\n" $+
$" \\ :header-type       -- *clm-header-type*\n" $+
$" \\ :data-format       -- *clm-data-format*\n" $+
$" \\ :audio-format      -- *clm-audio-format*\n" $+
$" \\ :notehook          -- *clm-notehook*\n" $+
$" \\ :output            -- *clm-file-name*\n" $+
$" \\ :reverb-file-name  -- *clm-reverb-file-name*\n" $+
$" \\ :player            -- *clm-player*\n" $+
$" \\ :comment           -- *clm-comment*\n" $+
$" \\ :decay-time        -- *clm-decay-time*\n" $+
$" \\ Moves INFILE through PATH with dlocsig's DL-MOVE\n" $+
$" $\" bell.snd\" make-spiral-path :channels 4 with-move-sound" $+ help!
' with-psound $" with-psound ( body-xt keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :play              -- *clm-play*\n" $+
$" \\ :statistics        -- *clm-statistics*\n" $+
$" \\ :verbose           -- *clm-verbose*\n" $+
$" \\ :reverb            -- *clm-reverb*\n" $+
$" \\ :channels          -- *clm-channels*\n" $+
$" \\ :reverb-channels   -- *clm-reverb-channels*\n" $+
$" \\ :clm-srate         -- *clm-srate*\n" $+
$" \\ :rt-buffer-size    -- *clm-rt-bufsize*\n" $+
$" \\ :table-size        -- *clm-table-size*\n" $+
$" \\ :locsig-type       -- *clm-locsig-type*\n" $+
$" \\ :audio-format      -- *clm-audio-format*\n" $+
$" \\ :notehook          -- *clm-notehook*\n" $+
$" \\ :decay-time        -- *clm-decay-time*\n" $+
$" \\ :device            -- *clm-device*\n" $+
$" ' run-p-test with-psound" $+ help!

' each $" each ( obj -- )\n"
$" \\ Compilation state EACH (in functions), for interpreter state see [EACH].\n" $+
$" \\ EACH works with String, Array, Vct and Hash and must be terminated by END-EACH.\n" $+
$" \\ On every iteration it leaves the next element of object on stack.\n" $+
$" v   each  f.          end-each\n" $+
$" str each  emit space  end-each\n" $+
$" ary each  .           end-each" $+ help!
' end-each $" end-each ( val -- ) \\ see EACH" help!
' map $" map ( obj -- )\n"
$" \\ Compilation state MAP (in functions), for interpreter state see [MAP].\n" $+
$" \\ MAP works with String, Array, Vct and Hash and must be terminated by END-MAP.\n" $+
$" \\ On every iteration it stores the last value of body to object\n" $+
$" \\ and on termination it returns the object.\n" $+
$" \\ So it's possible to create and initialize Vcts or Arrays in one run.\n" $+
$" 10 make-vct    map  i s>f   end-map { v }\n" $+
$" 10 make-string map  i 65 +  end-map { str }\n" $+
$" 10 make-array  map  i       end-map { ary }" $+ help!
' end-map $" end-map ( val -- obj ) \\ see MAP" help!
' [each] $" [each] ( obj -- )\n"
$" \\ Interpreter state [EACH] (outside functions), for compiler state see EACH.\n" $+
$" v   [each]  f.         [end-each]\n" $+
$" str [each] emit space  [end-each]\n" $+
$" ary [each]  .          [end-each]" $+ help!
' [map] $" [map] ( obj -- obj' )\n"
$" \\ Interpreter state [MAP] (outside functions), for compiler state see MAP.\n" $+
$" 10 make-vct    [map]  [i] s>f   [end-map] value v\n" $+
$" 10 make-string [map]  [i] 65 +  [end-map] value str\n" $+
$" 10 make-array  [map]  [i]       [end-map] value ary" $+ help!
' length $" length ( obj -- n ) \\ returns number of elements of OBJ" help!
' data-ptr $" data-ptr ( obj -- ptr ) \\ returns data pointer of OBJ" help!
' cycle $" cycle ( obj -- val )\n"
$" \\ returns next value of OBJ, after last element continues with first again" $+ help!
' ?range $" ?range ( idx obj -- f) \\ returns true if IDX is in range of OBJ" help!
' ?empty $" ?empty ( obj -- f ) \\ returns true if OBJ is empty" help!

' make-vct $" make-vct ( size -- vct )\n"
$" \\ returns a Vct object of SIZE elements filled with 0e" $+ help!
' ?vct $" ?vct ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Vct" $+ help!
' >vct $" >vct ( u-float-values u -- vct )\n"
$" \\ collects U floats and returns a new Vct object\n" $+
$" 0e 1e 1e 1e  4  >vct value v" $+ help!
' vct[ $" vct[ ( values -- vct )\n"
$" \\ Collects values between vct[ and ] or end of line and returns a new Vct object.\n" $+
$" \\ It works only for one line, not for multiple lines.  For greater Vcts use >vct.\n" $+
$" vct[ 0e 1e 2e ] value v" $+ help!
' free-vct $" free-vct ( vct -- ) \\ frees allocated memory of VCT object" help!
' vct-copy $" vct-copy ( vct1 -- vct2 ) \\ copies contents of VCT1 and returns a new Vct" help!
' vct= $" vct= ( vct1 vct2 -- f ) \\ returns true if VCT1 and VCT2 have equal contents" help!
' .vct $" .vct ( vct -- ) \\ prints Vct object to stdout" help!
' vct@ $" vct@ ( idx vct -- f: value ) \\ 13 v vct@ f. -- gets IDX' VALUE of VCT" help!
' vct! $" vct! ( f: value idx vct -- ) \\ 0.7e 13 v vct! -- sets ARY's IDX to VALUE" help!
' vct+! $" vct+! ( f: value idx vct -- )\n"
$" \\ adds VALUE to value on IDX and stores it\n" $+
$" 0.3e 10 v vct+!\n" $+
$" \\ or\n" $+
$" 10 v vct@ 0.3e f+ 10 v vct!" $+ help!
' vct-first $" vct-first ( vct -- f: value ) \\ returns first element of VCT" help!
' vct-second $" vct-second ( vct -- f: value ) \\ returns second element of VCT" help!
' vct-third $" vct-third ( vct -- f: value ) \\ returns third element of VCT" help!
' vct-last $" vct-last ( vct -- f: value ) \\ returns last element of VCT" help!
' vct-map! $" vct-map! ( xt vct -- )\n"
$" \\ takes an XT of stack effect ( -- f: value )\n" $+
$" \\ and adds resulting VALUE to elements of VCT\n" $+
$" \\ lambda: ( -- f: value ) 0.5e ;  v  vct-map!  v .vct" $+ help!
' vct-each! $" vct-each! ( xt vct -- )\n"
$" \\ takes an XT of stack effect ( f: value -- f: result )\n" $+
$" \\ and adds RESULT to elements of VCT;\n" $+
$" \\ VALUE is the current element of VCT\n" $+
$" lambda: ( f: r1 -- f: r2 ) 0.5e f+ ;  v  vct-each!" $+ help!
' vct-each-with-index! $" vct-each-with-index! ( xt vct -- )\n"
$" \\ takes an XT of stack effect ( f: value idx -- f: result )\n" $+
$" \\ and adds RESULT to elements of VCT;\n" $+
$" \\ VALUE is the current element of VCT and IDX the current index\n" $+
$" lambda: ( f: r1 idx -- f: r2 ) s>f f* ;  v  vct-each-with-index!" $+ help!
' vct-scale! $" vct-scale! ( f: scale vct -- )\n"
$" \\ adds SCALE to all elements of VCT\n" $+
$" 0.5e v vct-scale! v .vct" $+ help!
' vct-scale $" vct-scale ( f: scale vct1 -- vct2 )\n"
$" \\ adds SCALE to all elements of a copy of VCT1 and returns the new VCT2\n" $+
$" 0.5e v vct-scale! v .vct" $+ help!
' vct-offset! $" vct-offset! ( f: value v -- )\n"
$" \\ adds OFFSET to all elements of VCT\n" $+
$" 0.5e v vct-offset! v .vct" $+ help!
' vct-fill! $" vct-fill! ( f: value vct -- ) \\ sets all items of VCT to VALUE" help!
' vct-fill $" vct-fill ( f: value vct1 -- vct2 )\n"
$" \\ copies VCT1 to VCT2 sets all items of VCT2 to VALUE and returns it" $+ help!
' ?vct-member $" ?vct-member ( f: value vct -- f )\n"
$" \\ returns true if VALUE is member of VCT, otherwise false" $+ help!
' vct-clear $" vct-clear ( v -- ) \\ sets all elements to 0e" help!
' vct-add! $" vct-add! ( vct1 vct2 -- ) \\ result of element-wise addition in VCT1" help!
' vct-add $" vct-add ( vct1 vct2 -- vct3 ) \\ result of element-wise addition in VCT3" help!
' vct-subtract! $" vct-subtract! ( vct1 vct2 -- )\n"
$" \\ result of element-wise subtraction in VCT1" $+ help!
' vct-subtract $" vct-subtract ( vct1 vct2 -- vct3 )\n"
$" \\ result of element-wise subtraction in VCT3" $+ help!
' vct-multiply! $" vct-multiply! ( vct1 vct2 -- )\n"
$" \\ result of element-wise multiplication in VCT1" $+ help!
' vct-multiply $" vct-multiply ( vct1 vct2 -- vct3 )\n"
$" \\ result of element-wise multiplication in VCT3" $+ help!
' vct-peak $" vct-peak ( vct -- r ) \\ returns fabs fmax value of VCT" help!
' vct-max $" vct-max ( vct -- r ) \\ returns fmax value of VCT" help!
' vct-min $" vct-min ( vct -- r ) \\ returns fmin value of VCT" help!
' vct-sort! $" vct-sort! ( cmp-xt vct -- )\n"
$" \\ takes CMP-XT of stack effect ( r1 r2 -- f ) and sorts VCT inplace\n" $+
$" ' f< v vct-sort!  v .vct" $+ help!
' vct-sort $" vct-sort ( cmp-xt vct1 -- vct2 )\n"
$" \\ takes CMP-XT of stack effect ( r1 r2 -- f ) and returns a new sorted Vct\n" $+
$" ' f< v vct-sort .vct" $+ help!
' vct-sort-<! $" vct-sort-<! ( vct -- ) \\ sorts VCT in increasing order" help!
' vct-sort-< $" vct-sort-< ( vct1 -- vct2 )\n"
$" \\ sorts VCT in increasing order and returns a new sorted Vct" $+ help!
' vct-sort->! $" vct-sort->! ( vct -- ) \\ sorts VCT in decreasing order" help!
' vct-sort-> $" vct-sort-> ( vct1 -- vct2 )\n"
$" \\ sorts VCT in decreasing order and returns a new sorted Vct" $+ help!
' vct-move! $" vct-move! ( to from back? vct -- )\n"
$" \\ moves items of VCT FROM TO (back?: true backwards, false forward)" $+ help!
' vct-reverse! $" vct-reverse! ( vct -- )\n" 
$" \\ reverses elements of VCT\n" $+
$" vct[ 0e 1e 2e 3e ] vct-reverse! \\ #<vct: 3.0 2.0 1.0 0.0>" $+ help!
' vct-push! $" vct-push! ( f: value vct -- )\n" 
$" \\ appends VALUE to VCT\n" $+
$" vct[ 0e 1e 2e 3e ] 0.5e vct-push! \\ #<vct: 0.0 1.0 2.0 3.0 0.5>" $+ help!
' vct-pop! $" vct-pop! ( vct -- f: value )\n" 
$" \\ removes last element from VCT and returns it\n" $+
$" vct[ 0e 1e 2e 3e ] vct-pop! f. \\ 3.0; #<vct: 0.0 1.0 2.0>" $+ help!
' vct-unshift! $" vct-unshift! ( f: value v -- )\n" 
$" \\ prepends VALUE to VCT\n" $+
$" vct[ 0e 1e 2e 3e ] 0.3e vct-unshift! \\ #<vct: 0.3 0.0 1.0 2.0 3.0>" $+ help!
' vct-shift! $" vct-shift! ( v -- f: value )\n" 
$" \\ removes first element from VCT and returns it\n" $+
$" vct[ 0e 1e 2e 3e ] vct-shift! f. \\ 0.0; #<vct: 1.0 2.0 3.0>" $+ help!
' vct-uniq! $" vct-uniq! ( vct -- )\n"
$" \\ removes duplicates from VCT" $+ help!
' vct-uniq $" vct-uniq ( vct1 -- vct2 )\n"
$" \\ removes duplicates from VCT1 and returns new Vct" $+ help!
' vct-delete! $" vct-delete! ( idx v -- f: value )\n"
$" \\ deletes IDX element of V and returns it" $+ help!
' vct-insert! $" vct-insert! ( f: val idx v -- ) \\ inserts VAL on IDX's place of V" help!
' vct-rand! $" vct-rand! ( vct -- ) \\ rearranges randomly all elements of VCT" help!
' vct-rand $" vct-rand ( vct1 -- vct2 ) \\ returns new VCT2 with rearranged elements of VCT1" help!
' vct-index $" vct-index ( f: val v -- idx|-1 )\n"
$" \\ if VAL was found returns IDX, otherwise -1" $+ help!
' vct-index-bs $" vct-index-bs ( f: val v -- idx|-1 ) \\ index or -1 from sorted vct" help!
' ?vct-member $" ?vct-member ( f: val v -- f ) \\ if VAL member of V true otherwise false" help!
' ?vct-member-bs $" ?vct-member-bs ( f: val v -- f ) \\ find on sorted vct" help!
' vct-and $" vct-and ( v1 v2 -- v3 )\n"
$" \\ returns new vct v3 of elements common to both v1 and v2\n" $+
$" vct[ 1e 3e 5e ] vct[ 1e 2e 3e ] vct-and .vct ==> vct[ 1e 3e ]" $+ help!
' vct>stack $" vct>stack ( v -- values ) \\ pushs all elements on float stack" help!
' vct-subseq $" vct-subseq ( start size vct1 -- vct2 )\n"
$" returns a new vct with SIZE values of VCT1 starting from START" $+ help!
' vct>sound-file $" vct>sound-file ( fd vct frames -- )\n"
$" \\ writes FRAMES elements from VCT to file descriptor FD\n" $+
$" \\ which should be opened with sound-open-output" $+ help!
' c-float-array@ $" c-float-array@ ( idx ptr -- r )\n"
$" \\ returns IDX' element of the C float pointer PTR\n" $+
$" \\ (see src, convolve, granulate and phase-vocoder in csndlib.fs)" $+ help!
' c-float-array! $" c-float-array! ( f: value idx ptr -- )\n"
$" \\ stores VALUE to C float pointer PTR on IDX\n" $+
$" \\ (see src, convolve, granulate and phase-vocoder in csndlib.fs)" $+ help!
' c-float-array+! $" c-float-array+! ( f: value idx ptr -- )\n"
$" \\ adds VALUE to C float pointer PTR on IDX\n" $+
$" \\ (see src, convolve, granulate and phase-vocoder in csndlib.fs)" $+ help!

' make-sound-data $" make-sound-data ( chans frames -- sd )\n" 
$" \\ returns a Sound-data object filled with 0.0e\n" $+
$" \\ 1 128 make-sound-data value sd" $+ help!
' ?sound-data $" ?sound-data ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sound-data" $+ help!
' sound-data-free $" sound-data-free ( sd -- ) \\ frees allocated memory of SD object" help!
' sound-data= $" sound-data= ( sd1 sd2 -- f )\n"
$" \\ returns true if SD1 and SD2 have equal contents" $+ help!
' .sound-data $" .sound-data ( sd -- ) \\ prints Sound-data object SD to stdout" help!
' sound-data-length $" sound-data-length ( sd -- n ) \\ returns frames of SD" help!
' sound-data-chans $" sound-data-chans ( sd -- n ) \\ return channels of SD" help!
' sound-data@ $" sound-data@ ( frame chan sd -- f: value )\n"
$" \\ returns element on CHAN and FRAME of SD\n" $+
$" 17 0 sd sound-data@ f." $+ help!
' sound-data! $" sound-data! ( f: value frame chan sd -- )\n"
$" \\ sets element on CHAN and FRAME of SD to VALUE\n" $+
$" 0.5e 17 0 sd sound-data!" $+ help!
' vct>sound-data $" vct>sound-data ( vct chan sd -- )\n"
$" \\ copies contents from VCT to SD's CHAN\n" $+
$" v 0 sd vct>sound-data" $+ help!
' sound-data>vct $" sound-data>vct ( vct chan sd -- )\n"
$" \\ copies contents from SD's CHAN to VCT\n" $+
$" v 0 sd sound-data>vct" $+ help!
' sound-data>sound-data $" sound-data>sound-data ( chn1 sd1 chn2 sd2 -- )\n"
$" \\ copies from SD1's CHN1 to SD2's CHN2 (see record-sound)" $+ help!
' sound-data-scale! $" sound-data-scale! ( f: scl sd -- ) \\ scales the entire object SD" help!
' sound-data-clear $" sound-data-clear ( sd -- ) \\ sets all elements of SD to 0.0e" help!
' sound-data-maxamp $" sound-data-maxamp ( sd -- vct )\n"
$" \\ returns a Vct of length sound-data-chans with fabs fmax values for each channel" $+ help!
' sound-maxamp $" sound-maxamp ( fname -- vals times )\n"
$" \\ Returns a Vct VALS with fabs fmax values and an Array TIMES with sample numbers,\n" $+
$" \\ each of length chans of FNAME.\n" $+
$" \\ The sample number is the location of VALS' maxamp in the corresponding channel.\n" $+
$" \\ See .MAXAMP (gfm.fs) for usage." $+ help!

' ?envelope $" ?envelope ( env-vct -- f )\n"
$" \\ returns true if x values increase in sequence and at least two breakpoints exist" $+ help!
' max-envelope $" max-envelope ( env-vct -- r ) \\ returns max y value" help!
' min-envelope $" min-envelope ( env-vct -- r ) \\ returns min y value" help!
' scale-envelope $" scale-envelope ( f: scale f: optional-offset env-vct1 -- env-vct2 )\n"
$" \\ OPTIONAL-OFFSET is 0e if no value was given\n" $+
$" \\ returns a new vct ENV-VCT2 with all y values scaled by SCALE\n" $+
$" \\ and OPTIONAL-OFFSET added" $+ help!
' envelope-last-x $" envelope-last-x ( env-vct -- r ) \\ returns last x value of envelope" help!
' envelope-length $" envelope-length ( env-vct -- n ) \\ returns number of breakpoints" help!
' normalize-envelope $" normalize-envelope ( env-vct1 -- env-vct2 )\n"
$" \\ returns a new normalized envelope" $+ help!
' envelope-interp $" envelope-interp ( f: x envel-vct f: optional-base -- r )\n"
$" \\ OPTIONAL-BASE is 1e if no value was given\n" $+
$" 0.3e vct[ 0e 0e 0.5e 1e 1e 0e ] envelope-interp f. ==> 0.6" $+ help!
' stretch-envelope $" stretch-envelope ( fn1 f: oatt f: natt f: odec f: ndec -- fn2 )\n"
$" vct[ 0e 0e 1e 1e ] 0.1e 0.2e 0e 0e stretch-envelope .vct\n" $+
$" \\ ==> #<vct: 0.000 0.000 0.200 0.100 1.000 1.000>\n" $+
$" vct[ 0e 0e 1e 1e 2e 0e ] 0.1e 0.2e 1.5e 1.6e stretch-envelope .vct\n" $+
$" \\ ==> #<vct: 0.000 0.000 0.200 0.100 1.100 1.000 1.600 0.500 2.000 0.000>" $+ help!
' reverse-envelope $" reverse-envelope ( env-vct1 -- env-vct2 )\n"
$" \\ returns envelope in reversed order" $+ help!
' map-envelopes $" map-envelopes ( xt env-vct1 env-vct2 -- env-vct2 )\n"
$" \\ merges envelopes ENV-VCT1 ENV-VCT2 to a new created vct" $+ help!
' add-envelopes $" add-envelopes ( env-vct1 env-vct2 -- env-vct2 ) \\ alias envelopes+\n"
$" \\ adds y values of envelopes and returns result in a new vct" $+ help!
' multiply-envelopes $" multiply-envelopes ( env-vct1 env-vct2 -- env-vct3 )\n"
$" \\ multiplies y values of envelopes and returns result in a new vct" $+ help!

' make-hook $" make-hook ( name arity-n arity-r return-n return-r help -- hook )" help!
' ?hook $" ?hook ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Hook" $+ help!
' .hook $" .hook ( hook -- ) \\ prints Hook object HOOK to stdout" help!
' free-hook $" free-hook ( hook -- ) \\ frees allocated memory of HOOK object" help!
' hook-add! $" hook-add! ( xt name hook -- ) \\ adds new hook with NAME and action XT" help!
' hook-remove! $" hook-remove! ( name hook -- ) \\ removes hook named NAME" help!
' ?hook-empty $" ?hook-empty ( hook -- f )" help!
' ?hook-not-empty $" ?hook-not-empty ( hook -- f )" help!
' hook-reset! $" hook-reset! ( hook -- ) \\ clears all hook entries" help!
' hook-run $" hook-run ( ?? hook -- ?? ) \\ executes the hook(s)" help!
' hook-help@ $" hook-help@ ( hook -- str ) \\ returns help strings of hook" help!
' hook-help! $" hook-help! ( str hook -- ) \\ sets new help strings of hook" help!
' .hook-help $" .hook-help ( hook -- ) \\ prints HOOK's help strings" help!

' make-matrix $" make-matrix ( row col -- matrix )\n"
$" \\ returns a matrix of ROWs and COLumns" $+ help!
' make-matrix-diagonal $" make-matrix-diagonal ( vct-vals -- matrix )\n"
$" \\ returns a new squared matrix with VALS as diagonal values" $+ help!
' make-matrix-scalar $" make-matrix-scalar ( f: val rows -- matrix )\n"
$" \\ returns a ROW times ROW matrix with diagonal value VAL" $+ help!
' make-matrix-identity $" make-matrix-identity ( rows -- matrix )\n"
$" \\ returns a ROW times ROW matrix with diagonal value 1e" $+ help!
' make-matrix-zero $" make-matrix-zero ( rows -- matrix )\n"
$" \\ returns a ROW times ROW empty matrix" $+ help!
' ?matrix $" ?matrix ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Matrix" $+ help!
' ?matrix-square $" ?matrix-square ( matrix -- f )\n"
$" \\ returns true if rows and columns are even" $+ help!
' .matrix $" .matrix ( matrix -- ) \\ prints matrix object" help!
' free-matrix $" free-matrix ( matrix -- ) \\ frees allocated memory of MATRIX object" help!
' matrix@ $" matrix@ ( row col matrix -- f: value ) \\ returns VALUE on ROW/COL of MATRIX" help!
' matrix! $" matrix! ( f: value row col matrix -- ) \\ sets VALUE to ROW/COL of MATRIX" help!
' matrix+! $" matrix+! ( f: value row col matrix -- ) \\ adds VALUE to ROW/COL of MATRIX" help!
' matrix-row $" matrix-row ( row matrix -- vct )\n"
$" \\ returns a vct with ROW's values" $+ help!
' matrix-column $" matrix-column ( col matrix -- vct )\n"
$" \\ returns a vct with COL's values" $+ help!
' array>matrix $" array>matrix ( ary row -- matrix )\n"
$" \\ converts ARY to a matrix of ROWs and ROW columns" $+ help!
' matrix-scale! $" matrix-scale! ( f: scl matrix -- )\n"
$" \\ scales the whole matrix by SCL" $+ help!
' matrix-inverse $" matrix-inverse ( matrix1 -- matrix2 )\n"
$" \\ returns an inverse MATRIX2 or nil if MATRIX1 is not regular" $+ help!

' make-array $" make-array ( size -- ary )\n"
$" \\ returns an Array object of SIZE elements filled with 0" $+ help!
' ?array $" ?array ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Array" $+ help!
' >array $" >array ( u-values u -- ary )\n"
$" \\ collects U values and returns an Array object\n" $+
$" 0 1 1 1  4  >array value ary" $+ help!
' array[ $" array[ ( values -- ary )\n"
$" \\ Collects values between array[ and ] or end of line returning a new Array object.\n" $+
$" \\ It works only for one line, not for multiple lines.  For greater Arrays use >array.\n" $+
$" array[ 0 1 2 ] value ary" $+ help!
' free-array $" free-array ( ary -- ) \\ frees allocated memory of ARY object" help!
' array-copy $" array-copy ( a1 -- a2 ) \\ copies contents of A1 and returns a new Array" help!
' array= $" array= ( a1 a2 -- f ) \\ returns true if A1 and A2 have equal contents" help!
' .array $" .array ( ary -- ) \\ prints Array object ARY to stdout" help!
' array@ $" array@ ( idx ary -- n ) \\ 13 ary array@ . -- gets IDX' VALUE of ARY" help!
' array! $" array! ( value idx ary -- ) \\ 7 13 ary array! -- sets ARY's IDX to VALUE" help!
' array+! $" array+! ( value idx ary -- )\n"
$" \\ adds VALUE to value on IDX and stores it there\n" $+
$" 3 10 ary array+! \\ or 10 ary array@ 3 + 10 ary array!" $+ help!
' array-first $" array-first ( ary -- value ) \\ returns first element of Array or nil" help!
' array-second $" array-second ( ary -- value ) \\ returns second element of Array or nil" help!
' array-third $" array-third ( ary -- value ) \\ returns third element of Array or nil" help!
' array-last $" array-last ( ary -- value ) \\ returns last element of Array" help!
' array-index $" array-index ( val ary -- idx|-1 )\n"
$" \\ returns index if VAL was found in ARY, otherwise -1" $+ help!
' array-index-bs $" array-index-bs ( val ary -- idx|-1 )\n"
$" \\ returns index if VAL was found in ARY, otherwise -1, on sorted array" $+ help!
' array-map! $" array-map! ( xt ary -- )\n"
$" \\ takes an XT of stack effect ( -- value )\n" $+
$" \\ and adds resulting VALUE to elements of ARY\n" $+
$" \\ lambda: ( -- value ) make-oscil ;  ary array-map!  ary .array" $+ help!
' array-each! $" array-each! ( xt ary -- )\n"
$" \\ takes an XT of stack effect ( value -- result )\n" $+
$" \\ and adds RESULT to elements of ARY;\n" $+
$" \\ VALUE is the current element of ARY\n" $+
$" lambda: ( n1 -- n2 ) 5 + ;  ary  array-each!" $+ help!
' array-each-with-index! $" array-each-with-index! ( xt ary -- )\n"
$" \\ takes an XT of stack effect ( value idx -- result )\n" $+
$" \\ and adds RESULT to elements of ARY;\n" $+
$" \\ VALUE is the current element of ARY and IDX the current index\n" $+
$" lambda: ( n1 idx -- n2 ) * ;  ary  array-each-with-index!" $+ help!
' array-fill! $" array-fill! ( value ary -- ) \\ sets all items of ARY to VALUE" help!
' ?array-member $" ?array-member ( value ary -- f )\n"
$" \\ returns true if VALUE is member of ARY, otherwise false" $+ help!
' ?array-member-bs $" ?array-member-bs ( value ary -- f )\n"
$" \\ returns true if VALUE is member of ARY, otherwise false, on sorted array" $+ help!
' array-clear $" array-clear ( ary -- ) \\ sets all elements to 0" help!
' array-max $" array-max ( ary -- n ) \\ returns max value of ARY" help!
' array-min $" array-min ( ary -- n ) \\ returns min value of ARY" help!
' array-sort! $" array-sort! ( cmp-xt ary -- )\n"
$" \\ takes CMP-XT of stack effect ( n1 n2 -- f ) and sorts ARY inplace\n" $+
$" ' < ary array-sort!  ary .array" $+ help!
' array-sort $" array-sort ( cmp-xt ary1 -- ary2 )\n"
$" \\ takes CMP-XT of stack effect ( n1 n2 -- f ) and returns a new sorted Array\n" $+
$" ' < ary array-sort .array" $+ help!
' array-sort-<! $" array-sort-<! ( ary -- ) \\ sorts ARY in increasing order" help!
' array-sort-< $" array-sort-< ( ary1 -- ary2 )\n"
$" \\ sorts ARY in increasing order and returns a new sorted Array" $+ help!
' array-sort->! $" array-sort->! ( ary -- ) \\ sorts ARY in decreasing order" help!
' array-sort-> $" array-sort-> ( ary1 -- ary2 )\n"
$" \\ sorts ARY in decreasing order and returns a new sorted Array" $+ help!
' array-sort-$<! $" array-sort-$<! ( ary -- ) \\ sorts string Array in increasing order" help!
' array-sort-$< $" array-sort-$< ( ary1 -- ary2 )\n"
$" \\ sorts string Array in increasing order and returns a new sorted Array" $+ help!
' array-sort-$>! $" array-sort-$>! ( ary -- ) \\ sorts string Array in decreasing order" help!
' array-sort-$> $" array-sort-$> ( ary1 -- ary2 )\n"
$" \\ sorts string Array in decreasing order and returns a new sorted Array" $+ help!
' array-reverse! $" array-reverse! ( ary -- )\n" 
$" \\ reverses elements of ARY\n" $+
$" array[ 0 1 2 3 ] array-reverse! \\ #<array: 3 2 1 0>" $+ help!
' array-delete! $" array-delete! ( idx ary -- val )\n"
$" \\ deletes IDX element of ARY and returns it" $+ help!
' array-insert! $" array-insert! ( val idx ary -- ) \\ inserts VAL on IDX's place of ARY" help!
' array-push! $" array-push! ( value ary -- )\n" 
$" \\ appends VALUE to ARY\n" $+
$" array[ 0 1 2 3 ] 5 array-push! \\ #<array: 0 1 2 3 5>" $+ help!
' array-pop! $" array-pop! ( ary -- value )\n" 
$" \\ removes last element from ARY and returns it\n" $+
$" array[ 0 1 2 3 ] array-pop! . \\ 3; #<array: 0 1 2>" $+ help!
' array-unshift! $" array-unshift! ( value ary -- )\n" 
$" \\ prepends VALUE to ARY\n" $+
$" array[ 0 1 2 3 ] 3 array-unshift! \\ #<array: 3 0 1 2 3>" $+ help!
' array-shift! $" array-shift! ( ary -- value )\n" 
$" \\ removes first element from ARY and returns it\n" $+
$" array[ 0 1 2 3 ] array-shift! . \\ 0; #<array: 1 2 3>" $+ help!
' array-uniq! $" array-uniq! ( ary -- )\n"
$" \\ removes duplicates from ARY" $+ help!
' array-uniq $" array-uniq ( ary1 -- ary2 )\n"
$" \\ removes duplicates from ARY1 and returns new Array" $+ help!
' array-rand! $" array-rand! ( ary -- ) \\ rearranges randomly all elements of ARY" help!
' array-rand $" array-rand ( a1 -- a2 ) \\ returns new A2 with rearranged elements of A1" help!
' c-array>array $" c-array>array ( addr size -- ary )\n"
$" \\ converts a pointer to an integer array, e.g. a C pointer,\n" $+
$" \\ to an object of kind array of length SIZE" $+ help!
' array-and $" array-and ( ary1 ary2 -- ary3 )\n"
$" \\ returns new array ARY3 of elements common to both ARY1 and ARY2\n" $+
$" array[ 1 3 5 ] array[ 1 2 3 ] array-and .array ==> array[ 1 3 ]" $+ help!
' array>stack $" array>stack ( ary -- values ) \\ pushs all elements on stack" help!

' make-string $" make-string ( size -- str )\n"
$" \\ returns a String of SIZE characters filled with $20 (spaces)" $+ help!
' ?string $" ?string ( obj -- f ) \\ alias ?$\n"
$" \\ returns true if OBJ is of kind String" $+ help!
' free-string $" free-string ( str -- ) \\ frees allocated memory of STR object" help!
\ ' .STRING doesn't work as help xt, it's a defered word in gforth-0.6.2/see.fs
' .$ $" .$ ( str -- ) \\ alias .string\n"
$" \\ prints String object STR to stdout" $+ help!
' string@ $" string@ ( idx str -- c ) \\ returns char on IDX from STR" help!
' string! $" string! ( val idx str -- ) \\ sets char VAL on IDX to STR" help!
' string= $" string= ( str1 str2 -- f ) \\ returns true if strings are equal" help!
' string< $" string< ( str1 str2 -- f )\n"
$" \\ returns true if STR1 is alphabetical less than STR2" $+ help!
' string> $" string> ( str1 str2 -- f )\n"
$" \\ returns true if STR1 is alphabetical greather than STR2" $+ help!
' string-search $" string-search ( s1 str -- s2|false )\n"
$" \\ Searchs in STR for S1. If match was found, returns S2 beginning at match\n" $+
$" \\ and of remaining length of STR, if no match was found, returns FASLE" $+ help!
' string>$ $" string>$ ( str -- addr len ) alias $>\n"
$" \\ returns addr and length of STR ready e.g. for TYPE" $+ help!
' $>string $" $>string ( addr len -- str ) \\ alias >$\n"
$" \\ converts Forth string to object of kind String" $+ help!
' string>c$ $" string>c$ ( str -- ptr ) \\ converts String to C-string" help!
' $c>string $" $c>string ( ptr -- str ) \\ converts C-string to String" help!
' string+ $" string+ ( str1 str2 -- str3 ) \\ alias $+\n"
$" \\ concatenats STR2 to STR1 and returns result in new created STR3" $+ help!

' make-float $" make-float ( f: val -- sfl ) \\ alias float>float\n"
$" \\ The separate stacks for integers and floats are sometimes problematic.\n" $+
$" \\ Make-float returns an pointer which can be set to Arrays and Hashs." $+ help!
' ?float $" ?float ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Float" $+ help!
' float@ $" float@ ( sfl -- f: val ) \\ returns float value of SFL" help!
' float! $" float! ( f: val sfl -- ) \\ sets VAL to SFL" help!
' float+! $" float+! ( f: val sfl -- ) \\ adds VAL to SFL" help!
' .float $" .float ( sfl -- ) \\ prints sfl object" help!

' make-complex $" make-complex ( f: re f: im -- cp ) \\ returns new complex number CP" help!
' make-complex-polar $" make-complex-polar ( f: r -- f: theta -- cp )" help!
' ?complex $" ?complex ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Complex" $+ help!
' .complex $" .complex ( cp -- ) \\ prints object CP" help!
' real@ $" real@ ( cp -- r ) \\ returns real part of complex number CP" help!
' real! $" real! ( f: val cp -- ) \\ sets real part of complex number CP" help!
' image@ $" image@ ( cp -- r ) \\ returns image part of complex number CP" help!
' image! $" image! ( f: val cp -- ) \\ sets image part of complex number CP" help!
' complex-conjugate $" complex-conjugate ( cp1 -- cp2 )" help!
' complex-abs2 $" complex-abs2 ( cp -- r )" help!
' complex+ $" complex+ ( cp1 cp2 -- cp3 ) \\ CP1 may be of kind Float or Complex" help!
' complex- $" complex- ( cp1 cp2 -- cp3 ) \\ CP1 may be of kind Float or Complex" help!
' complex* $" complex* ( cp1 cp2 -- cp3 ) \\ CP1 may be of kind Float or Complex" help!
' complex/ $" complex/ ( cp1 cp2 -- cp3 ) \\ CP2 may be of kind Float or Complex" help!

' make-hash $" make-hash ( -- hash )\n"
$" \\ creates a bare hash object\n" $+
$" make-hash value str-hash\n" $+
$" $\" foo\" $\" foo-value\" str-hash hash!\n" $+
$" $\" foo\" str-hash hash@ .gen\n" $+
$" str-hash .hash\n" $+
$" str-hash free-hash\n" $+ help!
' make-hash-with-size $" make-hash-with-size ( size -- hash )\n"
$" \\ This is intended for use with MAP END-MAP, not for normal use.\n" $+
$" 10 make-hash-with-size  map  $\" val-\" i $(.) $+  i  end-map  value  hs\n" $+
$" \\ creates 10 entries with keys val-0, val-1, val-2, ... and values 0, 1, 2, ..." $+ help!
' ?hash $" ?hash ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Hash" $+ help!
' free-hash $" free-hash ( hash -- ) \\ frees hash object" help!
' .hash $" .hash ( hash -- ) \\ prints hash object" help!
' hash@ $" hash@ ( key hash -- value ) \\ returns VALUE of KEY or nil" help!
' fhash@ $" fhash@ ( key hash -- f: value ) \\ returns float-VALUE of KEY or nil" help!
' hash! $" hash! ( key value hash -- ) \\ inserts KEY/VALUE in HASH" help!
' fhash! $" fhash! ( key f: value hash -- ) \\ inserts KEY/float-VALUE in HASH" help!
' hash-keys $" hash-keys ( hash -- keys-ary ) \\ returns array with all HASH-keys" help!
' hash-values $" hash-values ( hash -- values-ary ) \\ returns array with all HASH-values" help!
' hash-delete $" hash-delete ( key hash -- ) \\ deletes KEY's entry" help!

' getopts $" getopts ( data kind -- ?? f )\n"
$" \\ KIND: opt-bool, opt-string, opt-number\n" $+
$" \\ DATA: String or Array of Strings\n" $+
$" array[ $\" -p\" $\" --play\" ] opt-string getopts IF play-sound THEN\n" $+
$" $\" -n\" opt-number getopts if . cr then\n" $+
$" $\" -r\" opt-number getopts if f. cr then\n" $+
$" ./test.gfm -n 17; ./test.gfm -r 17e\n" $+
$" \\ If opt-string or opt-number have no argument an exception will be thrown" $+ help!

' file-pwd $" file-pwd ( -- path ) \\ returns current working directory as a String" help!
' file-chdir $" file-chdir ( path -- ) \\ sets PATH to current working directory" help!
' file-split $" file-split ( fname -- path file ) \\ splits FNAME in PATH and FILE" help!
' file-basename $" file-basename ( fname optional-extension -- basename )\n"
$" \\ Returns filename without path;\n" $+
$" \\ if an extension was given this part will be removed from the end of filename.\n" $+
$" $\" ./oboe.snd\" file-basename ==> oboe.snd\n" $+
$" $\" ./oboe.snd\" $\" .snd\" file-basename ==> oboe" $+ help!
' file-pathname $" file-pathname ( fname -- pathname ) \\ returns pathname part of fname" help!
' file-fullname $" file-fullname ( fname -- pathname ) \\ returns full pathname of fname" help!
' file-delete $" file-delete ( fname -- ) \\ unlinks FNAME from system" help!
' ?file-exist $" ?file-exist ( fname -- f ) \\ returns true if FNAME exists" help!

' file-buffer-size@ $" file-buffer-size@ ( -- size ) \\ default 8192" help!
' file-buffer-size! $" file-buffer-size! ( size -- ) \\ default 8192" help!
' radians>hz $" radians>hz ( f: rads -- f: hz ) \\ 10e radians>hz f." help!
' hz>radians $" hz>radians ( f: hz -- f: rads ) \\ 10e hz>radians f." help!
' degrees>radians $" degrees>radians ( f: degrees -- f: rads ) \\ 10e degrees>radians f." help!
' radians>degrees $" radians>degrees ( f: rads -- f: degrees ) \\ 10e radians>degrees f." help!
' db>linear $" db>linear ( f: rads -- f: degrees ) \\ 10e db>linear f." help!
' linear>db $" linear>db ( f: degrees -- f: rads ) \\ 10e linear>db f." help!
' mus-srate@ $" mus-srate@ ( -- f: sr ) \\ mus-srate@ f." help!
' mus-srate! $" mus-srate! ( f: sr -- ) \\ 44100e mus-srate!" help!
' srate@ $" srate@ ( -- n ) \\ srate@ ." help!
' srate! $" srate! ( n -- ) \\ 44100 srate!" help!
' seconds>samples $" seconds>samples ( f: secs -- samps ) \\ 0.3e seconds>samples ." help!
' samples>seconds $" samples>seconds ( samps -- f: secs ) \\ 10000 samples>seconds f." help!
' times>samples $" times>samples ( f: start f: dur -- limit beg )\n" 
$" 0.3e 2.1e times>samples do i . loop" $+ help!
' array-print-length@ $" array-print-length@ ( -- len ) \\ array-print-length@ ." help!
' array-print-length! $" array-print-length! ( len -- ) \\ 8 array-print-length!" help!
' sine-bank $" sine-bank ( vct-amps vct-phases --  f: value )" help!
' ring-modulate $" ring-modulate ( f: s1 f: s2 -- f: s3 )\n"
$" \\ S1 S2 f*" $+ help!
' amplitude-modulate $" amplitude-modulate ( f: carrier f: in1 f: in2 -- f: value )\n"
$" \\ CARRIER IN2 f+ IN1 f* " $+ help!
' contrast-enhancement $" contrast-enhancement ( f: sig f: index -- f: value )\n"
$" \\ fsin(SIG * half-pi + (fsin(SIG * two-pi) * INDEX))" $+ help!
' dot-product $" dot-product ( vct1 vct2 -- f: value )" help!
' clear-array $" clear-array ( vct --  ) \\ alias vct-clear" help!
' polynomial $" polynomial ( vct f: x -- f: value ) \\ evaluate a polynomial at x" help!
' multiply-arrays $" multiply-arrays ( vct-data vct-window -- ) \\ result in VCT-DATA" help!
' rectangular>polar $" rectangular>polar ( vct-rl vct-im size -- ) \\ result in VCT-RL" help!
' polar>rectangular $" polar>rectangular ( vct-rl vct-im size -- ) \\ result in VCT-RL" help!
' array-interp $" array-interp ( f: phase vct-wave -- f: value )" help!
' spectrum $" spectrum ( rl-vct im-vct window-vct type -- )\n"
$" \\ result in RL-VCT\n" $+
$" \\ 0 = data in dB,\n" $+
$" \\ 1 = linear and normalized\n" $+
$" \\ 2 = linear and un-normalized" $+ help!
' fft $" fft ( rl-vct im-vct size dir -- )\n"
$" \\ result in RL-VCT\n" $+
$" \\ SIZE should be a power of 2\n" $+
$" \\ DIR = 1 for fft, -1 for inverse-fft" $+ help!
' make-fft-window $" make-fft-window ( type size f: beta -- w-vct )\n" 
$" \\ returns fft data window\n" $+
$" \\ TYPE is one of the sndlib fft window (e.g. kaiser-window)\n" $+
$" \\ BETA is the window parameter\n" $+
$" mus-hamming-window 256 0e make-fft-window value v" $+ help!
' normalize-partials $" normalize-partials ( parts -- parts' )" help!
' inverse-integrate $" inverse-integrate ( dist-vct -- data-vct )" help!
' spectrum>coeffs $" spectrum>coeffs ( order spectr -- coeffs )" help!
' phase@ $" phase@ ( gen -- f: phase ) \\ returns phase of GEN" help!
' phase! $" phase! ( f: phase gen -- ) \\ sets phase of GEN" help!
' data>vct $" data>vct ( gen -- vct-data ) \\ returns data of GEN as vct" help!
' vct>data $" vct>data ( vct-data gen -- ) \\ sets data of GEN with vct" help!
' data@ $" data@ ( idx gen -- f: value ) \\ returns IDX' VALUE of GEN's data" help!
' data! $" data! ( f: value idx gen -- ) \\ sets VALUE on IDX of GEN's data" help!
' name@ $" name@ ( gen -- str )\n"
$" \\ returns name of GEN as String\n" $+
$" make-oscil value os  os name@ .string \\ oscil" $+ help!
' offset@ $" offset@ ( gen -- f: offset ) \\ returns offset of GEN" help!
' offset! $" offset! ( f: offset gen -- ) \\ sets offset of GEN" help!
' width@ $" width@ ( gen -- f: width ) \\ returns width of GEN" help!
' width! $" width! ( f: width gen -- ) \\ set width of GEN" help!
' file-name@ $" file-name@ ( gen -- str )\n"
$" \\ returns filename of GEN as String\n" $+
$" $\" test.snd\" make-readin value rd  rd file-name@ .string \\ test.snd" $+ help!
' interp-type@ $" interp-type@ ( gen -- n ) \\ returns interpolation type as integer" help!
' cosines@ $" cosines@ ( gen -- n ) \\ returns number of GEN's cosines" help!
' cosines! $" cosines! ( n gen -- ) \\ sets number of GEN's cosines" help!
' feedback@ $" feedback@ ( gen -- r ) \\ returns feedback of GEN" help!
' feedback! $" feedback! ( f: value gen -- ) \\ sets feedback of GEN" help!
' feedforward@ $" feedforward@ ( gen -- r ) \\ returns feedforward of GEN" help!
' feedforward! $" feedforward! ( f: value gen -- ) \\ sets feedforward of GEN" help!
' channels@ $" channels@ ( gen -- chns ) \\ returns number of GEN's channels" help!
' channel@ $" channel@ ( gen -- chn ) \\ returns channel of GEN (readin)" help!
' location@ $" location@ ( gen -- loc ) \\ returns location of GEN" help!
' location! $" location! ( loc gen -- ) \\ sets location of GEN" help!
' length@ $" length@ ( gen -- length ) \\ returns length of GEN" help!
' length! $" length! ( length gen -- ) \\ sets length of GEN" help!
' frequency@ $" frequency@ ( gen -- f: frequency ) \\ returns frequency of GEN" help!
' frequency! $" frequency! ( f: frequency gen -- ) \\ sets frequency of GEN" help!
' scaler@ $" scaler@ ( gen -- f: scaler ) \\ returns scaler of GEN" help!
' scaler! $" scaler! ( f: scaler gen -- ) \\ sets scaler of GEN" help!
' increment@ $" increment@ ( gen -- f: inc ) \\ returns increment of GEN" help!
' increment! $" increment! ( f: inc gen -- ) \\ sets increment of GEN" help!
' hop@ $" hop@ ( gen -- hop ) \\ returns hop of GEN" help!
' hop! $" hop! ( hop gen -- ) \\ sets hop of GEN" help!

' oscil-bank $" oscil-bank ( amps-vct oscils-array inputs-vct -- f: value )\n" 
$" 440e :frequency 1e :initial-phase make-oscil value os1\n" $+
$" 220e :frequency 1e :initial-phase make-oscil value os2\n" $+
$" 660e :frequency 1e :initial-phase make-oscil value os3\n" $+
$" array[ os1 os2 os3 ] value oss\n" $+
$" vct[ 0.5e 0.3e 0.2e ] value amps\n" $+
$" vct[ 0.5e 0.3e 0.2e ] value ins\n" $+
$" amps oss ins oscil-bank f." $+ help!
' ssb-bank $" ssb-bank ( ssbs fir-flts f: input -- f: sum )\n" 
$" \\ SSBS and FIR-FLTS are arrays of ssb- and fir-filter-generators,\n" $+
$" \\ INPUT is fir-filter's INPUT and SUM is the sum of\n" $+
$" \\ input fir-gen fir-filter 0e ssb-gen ssb-am sum +!" $+ help!

' .gen $" .gen ( gen -- )\n"
$" \\ prints objects of Vct, Array, String, Sound-data and all CLM-Generators\n" $+
$" make-oscil .gen  $\" foo-bar\" .gen  10 make-vct .gen" $+ help!
' gen>string $" gen>string ( obj -- str )\n"
$" \\ Returns the string representation of OBJ and works with all classes.\n" $+
$" 0.32e make-float gen>string .string\n" $+
$" make-oscil gen>string .string" $+ help!
' gen= $" gen= ( gen1 gen2 -- f )\n"
$" \\ compares objects of Vct, Array, String, Sound-data and all CLM-Generators\n" $+
$" os1 os2 gen= .  vct1 vct2 gen= .  str1 str2 gen= ." $+ help!
' gen-free $" gen-free ( obj -- )\n"
$" \\ frees allocated memory of all objects, both generators and forth classes" $+ help!
' ?gen $" ?gen ( obj -- f )\n"
$" \\ returns true if OBJ is of kind CLM-Generator" $+ help!
' mus-run $" mus-run ( f: val1 f: val2 gen -- r )\n" 
$" make-oscil value os\n" $+
$" \\ all CLM-generators can be replaced by mus-run\n" $+
$" 0e 0e os oscil f.\n" $+
$" 0e 0e os mus-run f." $+ help!
' mus-free $" mus-free ( gen -- ) \\ frees allocated memory of GEN" help!

' make-oscil $" make-oscil ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" make-oscil value os\n" $+
$" :frequency 330e make-oscil value os" $+ help!
' ?oscil $" ?oscil ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Oscil" $+ help!
' oscil $" oscil ( f: fm f: pm gen -- f: value ) \\ 0e 0e os oscil f." help!

' make-sum-of-cosines $" make-sum-of-cosines ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :cosines       -- 1\n" $+
$" :cosines 10 make-sum-of-cosines value os" $+ help!
' ?sum-of-cosines $" ?sum-of-cosines ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sum-of-cosines" $+ help!
' sum-of-cosines $" sum-of-cosines ( f: fm gen -- f: value ) \\ 0e os sum-of-cosines f." help!

' make-sum-of-sines $" make-sum-of-sines ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :sines         -- 1\n" $+
$" :sines 8 make-sum-of-sines value os" $+ help!
' ?sum-of-sines $" ?sum-of-sines ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sum-of-cosines" $+ help!
' sum-of-sines $" sum-of-sines ( f: fm gen -- f: value ) \\ 0e os sum-of-sines f." help!

' make-delay $" make-delay ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :size             -- *table-size*\n" $+
$" \\ :initial-contents -- nil (vct)\n" $+
$" \\ :initial-element  -- 0e\n" $+
$" \\ :max-size         -- size\n" $+
$" \\ :type             -- mus-interp-none\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-all-pass\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-bezier\n" $+
$" \\                      mus-interp-hermite\n" $+
$" :size 10 make-delay value os\n" $+
$" :size  4 :initial-contents vct[ 1e 0.5e 0.25e 0e ] make-delay value os" $+ help!
' ?delay $" ?delay ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Delay" $+ help!
' delay $" delay ( f: input f: pm gen -- f: value ) \\ 0e 0e os delay f." help!
' tap $" tap ( f: loc gen -- f: value ) \\ 0e os tap f." help!
' delay-tick $" delay-tick ( f: input gen -- ) \\ 0e os delay-tick" help!

' make-comb $" make-comb ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :scaler           -- 0e\n" $+
$" \\ :size             -- *table-size*\n" $+
$" \\ :initial-contents -- nil (vct)\n" $+
$" \\ :initial-element  -- 0e\n" $+
$" \\ :max-size         -- size\n" $+
$" \\ :type             -- mus-interp-none\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-all-pass\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-bezier\n" $+
$" \\                      mus-interp-hermite\n" $+
$" :size 10 make-comb value os\n" $+
$" :scaler 0.3e :size 4 :initial-contents vct[ 1e 0.5e 0.25e 0e ] make-comb value os" $+ help!
' ?comb $" ?comb ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Comb" $+ help!
' comb $" comb ( f: input f: pm gen -- f: value ) \\ 0e 0e gen comb f." help!

' make-notch $" make-notch ( keywords-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :scaler           -- 0e\n" $+
$" \\ :size             -- *table-size*\n" $+
$" \\ :initial-contents -- nil (vct)\n" $+
$" \\ :initial-element  -- 0e\n" $+
$" \\ :max-size         -- size\n" $+
$" \\ :type             -- mus-interp-none\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-all-pass\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-bezier\n" $+
$" \\                      mus-interp-hermite\n" $+
$" :size 10 make-notch value os\n" $+
$" :scaler 0.3e :size 4 :initial-contents vct[ 1e 0.5e 0.25e 0e ] make-notch value os" $+ help!
' ?notch $" ?notch ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Notch" $+ help!
' notch $" notch ( f: input f: pm gen -- f: value ) \\ gen 0e 0e notch f." help!

' make-all-pass $" make-all-pass ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :feedback         -- 0e\n" $+
$" \\ :feedforward      -- 0e\n" $+
$" \\ :size             -- *table-size*\n" $+
$" \\ :initial-contents -- nil (vct)\n" $+
$" \\ :initial-element  -- 0e\n" $+
$" \\ :max-size         -- size\n" $+
$" \\ :type             -- mus-interp-none\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-all-pass\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-bezier\n" $+
$" \\                      mus-interp-hermite\n" $+
$" :size 10 make-all-pass value os\n" $+
$" :feedback 0.3e\n" $+
$" :feedforward -0.7e\n" $+
$" :size 4\n" $+
$" :initial-contents vct[ 1e 0.5e 0.25e 0e ] make-all-pass value os" $+ help!
' ?all-pass $" ?all-pass ( obj -- f )\n"
$" \\ returns true if OBJ is of kind All-pass" $+ help!
' all-pass $" all-pass ( f: input f: pm gen -- f: value ) \\ gen 0e 0e all-pass f." help!

' make-average $" make-average ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :size             -- *table-size*\n" $+
$" \\ :initial-contents -- nil (vct)\n" $+
$" \\ :initial-element  -- 0e\n" $+
$" :size 10 make-average value os\n" $+
$" :size 4 :initial-contents vct[ 1e 0.5e 0.25e 0e ] make-average value os" $+ help!
' ?average $" ?average ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Average" $+ help!
' average $" average ( f: input gen -- f: value ) \\ moving window average" help!

' make-table-lookup $" make-table-lookup ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :wave          -- vct[ 1e 1e ] false partials>wave\n" $+
$" \\ :type          -- mus-interp-linear\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-hermite\n" $+
$" make-table-lookup value gen\n" $+
$" :type mus-interp-lagrange  make-table-lookup value gen\n" $+
$" :wave vct[ 1e 2e ] false partials>wave make-table-lookup value gen" $+ help!
' ?table-lookup $" ?table-lookup ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Table-lookup" $+ help!
' table-lookup $" table-lookup ( f: fm gen -- f: value ) \\ gen 0e table-lookup f." help!
' partials>wave $" partials>wave ( vct-parts normalize -- vct-wave )\n" 
$" vct[ 1e 1e 2e 0.5e ] true partials>wave value wv\n" $+
$" \\ if NORMALIZE is true, the resulting waveform goes between -1.0 and 1.0.\n" $+
$" :wave vct[ 1e 1e 2e 0.5e ] false partials>wave make-table-lookup value gen" $+ help!
' phase-partials>wave $" phase-partials>wave ( vct-parts normalize -- vct-wave )\n" 
$" vct[ 1e 0.75e 0e 2e 0.25e pi 0.5e f* ] false phase-partials>wave value wv\n" $+
$" \\ take a list of partials (harmonic number, amplitude, initial phase)\n" $+
$" \\ if NORMALIZE is true, the resulting waveform goes between -1.0 and 1.0.\n" $+
$" vct[ 1e 0.75e 0e 2e 0.25e pi 0.5e f* ] false phase-partials>wave value wv\n" $+
$" :wave wv make-table-lookup value tb" $+ help!

' make-sawtooth-wave $" make-sawtooth-wave ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :amplitude     -- 1e\n" $+
$" \\ :initial-phase -- pi\n" $+
$" make-sawtooth-wave value os" $+ help!
' ?sawtooth-wave $" ?sawtooth-wave ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sawtooth-wave" $+ help!
' sawtooth-wave $" sawtooth-wave ( f: fm gen -- f: value ) \\ 0e os sawtooth-wave f." help!

' make-square-wave $" make-square-wave ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :amplitude     -- 1e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" make-square-wave value os" $+ help!
' ?square-wave $" ?square-wave ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Square-wave" $+ help!
' square-wave $" square-wave ( f: fm gen -- f: value ) \\ 0e os square-wave f." help!

' make-triangle-wave $" make-triangle-wave ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :amplitude     -- 1e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" make-triangle-wave value os" $+ help!
' ?triangle-wave $" ?triangle-wave ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Triangle-wave" $+ help!
' triangle-wave $" triangle-wave ( f: fm gen -- f: value ) \\ 0e os triangle-wave f." help!

' make-pulse-train $" make-pulse-train ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :amplitude     -- 1e\n" $+
$" \\ :initial-phase -- two-pi\n" $+
$" make-pulse-train value os" $+ help!
' ?pulse-train $" ?pulse-train ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Pulse-train" $+ help!
' pulse-train $" pulse-train ( f: fm gen -- f: value ) \\ 0e os pulse-train f." help!

' rand-seed@ $" rand-seed@ ( -- u ) \\ rand-seed@ u." help!
' rand-seed! $" rand-seed! ( u -- ) \\ 12345 rand-seed!" help!
' mus-random $" mus-random ( f: amp -- f: value ) \\ 0.5e mus-random f." help!
' random $" random ( f: amp -- f: value ) \\ 0.5e random f." help!
' irandom $" irandom ( amp -- value ) \\ 1 irandom ." help!

' make-rand $" make-rand ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency -- 440e\n" $+
$" \\ :amplitude -- 1e\n" $+
$" \\ :envelope  -- nil\n" $+
$" make-rand value os \\ without distribution\n" $+
$" :envelope vct[ -1e 1e 1e 1e ] make-rand value os \\ with distribution" $+ help!
' ?rand $" ?rand ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Rand" $+ help!
' rand $" rand ( f: fm gen -- f: value ) \\ 0e os rand f." help!

' make-rand-interp $" make-rand-interp ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency -- 440e\n" $+
$" \\ :amplitude -- 1e\n" $+
$" \\ :envelope  -- nil\n" $+
$" make-rand-interp value os \\ without distribution\n" $+
$" :envelope vct[ 0e 1e 1e 1e ] make-rand-interp value os \\ with distribution" $+ help!
' ?rand-interp $" ?rand-interp ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Rand-interp" $+ help!
' rand-interp $" rand-interp ( f: fm gen -- f: value ) \\ 0e os rand-interp f." help!

' make-asymmetric-fm $" make-asymmetric-fm ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :r             -- 1e\n" $+
$" \\ :ratio         -- 1e\n" $+
$" make-asymmetric-fm value os" $+ help!
' ?asymmetric-fm $" ?asymmetric-fm ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Asymmetric-fm" $+ help!
' asymmetric-fm $" asymmetric-fm ( f: index f: fm gen -- f: value )\n" 
$" 0e 0e os asymmetric-fm f." $+ help!

' make-one-zero $" make-one-zero ( f: a0 f: a1 -- gen )" help!
' ?one-zero $" ?one-zero ( obj -- f )\n"
$" \\ returns true if OBJ is of kind One-zero" $+ help!
' one-zero $" one-zero ( f: input gen -- f: value )" help!

' make-one-pole $" make-one-pole ( f: a0 f: b1 -- gen )" help!
' ?one-pole $" ?one-pole ( obj -- f )\n"
$" \\ returns true if OBJ is of kind One-pole" $+ help!
' one-pole $" one-pole ( f: input gen -- f: value )" help!

' make-two-zero $" make-two-zero ( f: a0 f: a1 f: a2 -- gen )" help!
' make-zpolar $" make-zpolar ( f: radius f: freq -- gen )" help!
' ?two-zero $" ?two-zero ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Two-zero" $+ help!
' two-zero $" two-zero ( f: input gen -- f: value )" help!

' make-two-pole $" make-two-pole ( f: a0 f: b1 f: b2 -- gen )" help!
' make-ppolar $" make-ppolar ( f: radius f: freq -- gen )" help!
' ?two-pole $" ?two-pole ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Two-zero" $+ help!
' two-pole $" two-pole ( f: input gen -- f: value )" help!

' make-formant $" make-formant ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :radius    -- 1e\n" $+
$" \\ :frequency -- 440e\n" $+
$" \\ :gain      -- 1e\n" $+
$" make-formant value gen" $+ help!
' ?formant $" ?formant ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Formant" $+ help!
' formant $" formant ( f: input obj -- f: value ) \\ 0e os formant f." help!
' formant-bank $" formant-bank ( amps-vct formants-array f: input -- f: value )" help!

' make-sine-summation $" make-sine-summation ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :n             -- 1\n" $+
$" \\ :a             -- 0.5e\n" $+
$" \\ :ratio         -- 1e\n" $+
$" make-sine-summation value os" $+ help!
' ?sine-summation $" ?sine-summation ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sine-summation" $+ help!
' sine-summation $" sine-summation ( f: fm gen -- f: value ) \\ 0e os sine-summation f." help!

' make-filter $" make-filter ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :order   -- 1\n" $+
$" \\ :xcoeffs -- nil (vct)\n" $+
$" \\ :ycoeffs -- nil (vct)\n" $+
$" :xcoeffs x-coeff-vct :ycoeffs y-coeff-vct make-filter value os" $+ help!
' ?filter $" ?filter ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Filter" $+ help!
' filter $" filter ( f: input gen -- f: value ) \\ 0e os filter f." help!

' make-fir-filter $" make-fir-filter ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :order   -- 1\n" $+
$" \\ :xcoeffs -- nil (vct)\n" $+
$" :xcoeffs x-coeff-vct make-fir-filter value os" $+ help!
' ?fir-filter $" ?fir-filter ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Fir-filter" $+ help!
' fir-filter $" fir-filter ( f: input gen -- f: value ) \\ 0e os fir-filter f." help!

' make-iir-filter $" make-iir-filter ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :order   -- 1\n" $+
$" \\ :ycoeffs -- nil (vct)\n" $+
$" :ycoeffs y-coeff-vct make-iir-filter value os" $+ help!
' ?iir-filter $" ?iir-filter ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Iir-filter" $+ help!
' iir-filter $" iir-filter ( f: input gen -- f: value ) \\ 0e os iir-filter f." help!
' make-fir-coeffs $" make-fir-coeffs ( v -- gen )\n" 
$" \\ turns spectral envelope in Vct V into coeffs for FIR filter" $+ help!
' xcoeffs@ $" xcoeffs@ ( gen -- xcoeffs-vct )" help!
' ycoeffs@ $" ycoeffs@ ( gen -- ycoeffs-vct )" help!
' xcoeff@ $" xcoeff@ ( idx gen -- f: val )" help!
' xcoeff! $" xcoeff! ( idx gen f: val -- )" help!
' ycoeff@ $" ycoeff@ ( idx gen -- f: val )" help!
' ycoeff! $" ycoeff! ( idx gen f: val -- )" help!

' make-wave-train $" make-wave-train ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency     -- 440e\n" $+
$" \\ :initial-phase -- 0e\n" $+
$" \\ :wave          -- *table-size* make-vct\n" $+
$" \\ :type          -- mus-interp-linear\n" $+
$" \\ interpolation types: mus-interp-none\n" $+
$" \\                      mus-interp-linear\n" $+
$" \\                      mus-interp-lagrange\n" $+
$" \\                      mus-interp-hermite\n" $+
$" make-wave-train value os" $+ help!
' ?wave-train $" ?wave-train ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Wave-train" $+ help!
' wave-train $" wave-train ( f: fm gen -- f: value ) \\ 0e os wave-train f." help!

' make-waveshape $" make-waveshape ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency -- 440e\n" $+
$" \\ :wave      -- vct[ 1e 1e ] *table-size* partials>waveshape\n" $+
$" \\ :partials  -- nil\n" $+
$" make-waveshape value gen\n" $+
$" :frequency 330e :partials vct[ 1e 1e 2e 0.5e ] make-waveshape value gen" $+ help!
' ?waveshape $" ?waveshape ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Waveshape" $+ help!
' waveshape $" waveshape ( f: index f: fm gen -- f: value ) \\ 1e 0e os waveshape f." help!
' partials>waveshape $" partials>waveshape ( vct-partials size -- vct-table )\n" 
$" vct[ 1e 0.5e 2e 0.3e 3e 0.2e ] 512 partials>waveshape value table" $+ help!
' partials>polynomial $" partials>polynomial ( vct-partials kind -- vct-table )\n" 
$" vct[ 1e 0.5e 2e 0.3e 3e 0.2e ] 1 partials>polynomial value table\n" $+
$" vct[ 1e 1e 2e 1e ] partials>polynomial value v0\n" $+
$" make-oscil value os\n" $+
$" os 0e 0e oscil v0 polynomial f." $+ help!

' make-env $" make-env ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :envelope -- vct[ 0e 0e 1e 0e ]\n" $+
$" \\ :scaler   -- 1e\n" $+
$" \\ :duration -- 0e\n" $+
$" \\ :offset   -- 0e\n" $+
$" \\ :base     -- 1e (base 0e: step; base 1e: seg; otherwise: exp\n" $+
$" \\ :start    -- 0\n" $+
$" \\ :end      -- 0\n" $+
$" make-env value en \\ seg\n" $+
$" :envelope vct[ 0e 1e 1e 1e ] :duration 3.4e :offset 0.3e make-env value seg-en\n" $+
$" :envelope vct[ 0e 1e 1e 1e ] :scaler 0.3e :base 0.5e :end 10 make-env value exp-en\n" $+
$" :envelope vct[ 0e 1e 1e 1e ] :end 20 :offset 0.5e :base 0e make-env value step-en" $+ help!
' ?env $" ?env ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Env" $+ help!
' env $" env ( gen -- f: value ) \\ en env f." help!
' restart-env $" restart-env ( gen -- ) \\ sets all values to start values" help!
' env-interp $" env-interp ( f: x gen -- f: value ) \\ 0.7e en env-interp f." help!

' make-frame $" make-frame ( chans -- fr ) \\ 2 make-frame value fr" help!
' ?frame $" ?frame ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Frame" $+ help!
' >frame $" >frame ( chans-values chans -- fr ) \\ 0.5e 0.3e 2 >frame value fr" help!
' frame+ $" frame+ ( fr1 fr2 -- fr3 ) \\ adds FR1 to FR2 and returns a new Frame FR3" help!
' frame* $" frame* ( fr1 fr2 -- fr3 ) \\ multiplies FR1 to FR2 and returns new Frame FR3" help!
' frame@ $" frame@ ( chn fr -- f: value ) \\ returns value of FR's channel CHN" help!
' frame! $" frame! ( f: val chn fr -- ) \\ sets VAL to FR's channel CHN" help!
' frame>vct $" frame>vct ( fr -- vct-vals ) \\ returns a pointer to FR's data Vct" help!
' vct>frame $" vct>frame ( vct-vals -- fr ) \\ sets Vct to FR's data" help!

' make-mixer $" make-mixer ( chans -- mx ) \\ 2 make-mixer value mx" help!
' ?mixer $" ?mixer ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Mixer" $+ help!
' >mixer $" >mixer ( chans-in-out-values chans -- mx )\n" 
$" 0.5e 0.25e 0.125e 1e 2 >mixer value mx\n" $+
$" \\ chan 0 in 0.5 out 0.25\n" $+
$" \\ chan 1 in 0.125 out 1" $+ help!
' make-identity-mixer $" make-identity-mixer ( chans -- mx )\n"
$" \\ returns a mixer with identity matrix\n" $+
$" 2 make-identity-mixer value mx" $+ help!
' mixer+ $" mixer+ ( mx1 mx2 res -- res ) \\ adds MX1 to MX2 and returns a new Mixer" help!
' mixer* $" mixer* ( mx1 mx2 res -- res ) \\ multiplies MX1 to MX2 and returns new Mixer" help!
' mixer@ $" mixer@ ( in-chn out-chn mx -- f: value ) \\ returns value of IN- and OUT-CHAN" help!
' mixer! $" mixer! ( f: val in-chn out-chn mx -- ) \\ sets value to IN- and OUT-CHAN" help!
' mixer-scale $" mixer-scale ( f: scl mx res -- res ) \\ scales MX by SCL" help!
' frame>frame $" frame>frame ( fin fout mx -- fout )\n"
$" \\ returns a new Frame through Mixer MX" $+ help!
' sample>frame $" sample>frame ( f: val fr mx -- fr )\n"
$" \\ returns a new Frame through Mixer MX" $+ help!
' frame>sample $" frame>sample ( fr mx -- f: value ) \\ returns value through Mixer MX" help!

' make-file>sample $" make-file>sample ( fname -- gen )\n" 
$" $\" test.reverb\" make-file>sample value *reverb*" $+ help!
' ?file>sample $" ?file>sample ( obj -- f )\n"
$" \\ returns true if OBJ is of kind File>sample" $+ help!
' file>sample $" file>sample ( samp chn gen -- f: value ) \\ 10 0 gen file>sample f." help!

' make-readin $" make-readin ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :file      -- $\" test.snd\"\n" $+
$" \\ :channel   -- 0\n" $+
$" \\ :start     -- 0\n" $+
$" \\ :direction -- 1\n" $+
$" :file $\" foo.snd\" make-readin value rd" $+ help!
' ?readin $" ?readin ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Readin" $+ help!
' readin $" readin ( gen -- f: value ) \\ rd readin f." help!

' ?output $" ?output ( obj -- f )\n"
$" \\ returns true if OBJ is of kind IO-Output" $+ help!
' ?input $" ?input ( obj -- f )\n"
$" \\ returns true if OBJ is of kind IO-Input" $+ help!
' in-any $" in-any ( samp chn gen -- f: value ) \\ returns sample on SAMP of channel CHN" help!

' make-file>frame $" make-file>frame ( fname -- gen )\n" 
$" $\" test.reverb\" make-file>frame value *reverb*" $+ help!
' ?file>frame $" ?file>frame ( obj -- f )\n"
$" \\ returns true if OBJ is of kind File>frame" $+ help!
' file>frame $" file>frame ( fr samp gen -- fr ) \\ 10 gen file>frame value fr" help!

' make-sample>file $" make-sample>file ( fname chans format type comment -- gen )\n" 
$" $\" test.snd\" 1 mus-lshort mus-next $\" test\" make-sample>file value *output*" $+ help!
' ?sample>file $" ?sample>file ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Sample>file" $+ help!
' sample>file $" sample>file ( f: val samp chn gen -- ) \\ 0.3e 10 0 *output* sample>file" help!
' mus-close $" mus-close ( gen -- ) \\ *output* mus-close" help!
' continue-sample>file $" continue-sample>file ( fname -- gen )\n" 
$" $\" test.snd\" continue-sample>file value *output*" $+ help!
' out-any $" out-any ( f: value samp chn gen -- ) \\ sets sample on SAMP of channel CHN" help!

' make-frame>file $" make-frame>file ( fname chans format type -- gen )\n" 
$" $\" test.snd\" 1 mus-lshort mus-next make-frame>file value *output*" $+ help!
' ?frame>file $" ?frame>file ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Frame>file" $+ help!
' frame>file $" frame>file ( fr samp gen -- ) \\ fr 10 *output* frame>file" help!
' continue-frame>file $" continue-frame>file ( fname -- gen )\n" 
$" $\" test.snd\" continue-frame>file value *output*" $+ help!

' make-locsig $" make-locsig ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :degree        -- 0e\n" $+
$" \\ :distance      -- 1e\n" $+
$" \\ :reverb-amount -- 0e\n" $+
$" \\ :channels      -- 1\n" $+
$" \\ :output        -- nil\n" $+
$" \\ :revout        -- nil\n" $+
$" \\ :type          -- mus-interp-linear\n" $+
$" \\ interpolation types: mus-interp-linear\n" $+
$" \\                      mus-interp-sinusoidal\n" $+
$" :output *output* make-locsig value loc" $+ help!
' ?locsig $" ?locsig ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Locsig" $+ help!
' locsig $" locsig ( f: value samp gen -- ) \\ 0.35e 10 loc locsig" help!
' locsig@ $" locsig@ ( chn gen -- f: value ) \\ returns value on channel CHN of GEN" help!
' locsig! $" locsig! ( f: val chn gen -- ) \\ sets value on channel CHN of GEN" help!
' locsig-reverb@ $" locsig-reverb@ ( chn gen -- r )\n"
$" \\ returns value on channel CHN of GEN" $+ help!
' locsig-reverb! $" locsig-reverb! ( f: val chn gen -- )\n"
$" \\ sets value on channel CHN of GEN" $+ help!
' move-locsig $" move-locsig ( f: degree f: distance gen -- )\n"
$" \\ changes DEGREE and DISTANCE of GEN" $+ help!
' fill-locsig $" fill-locsig ( vct-data chans f: degree f: scaler type -- )" help!

' file>array $" file>array ( fname chan start samps -- vct )\n" 
$" \\ reads channel CHAN from START of file FNAME and returns a new Vct\n" $+
$" $\" test.snd\" 1 0 1000 file>array value v" $+ help!
' array>file $" array>file ( fname vct samps srate chans -- )\n" 
$" \\ writes SAMPS values of VCT to FNAME opened with SRATE and CHANS\n" $+
$" $\" test.snd\" v 1000 22050 2 array>file" $+ help!

' mus-mix $" mus-mix ( keyword-args -- outfile )\n" 
$" \\ === Mus-Mix ===\n" $+
$" \\ keywords and default values\n" $+
$" \\ :infile  -- nil (filename or file>frame generator required)\n" $+
$" \\ :outfile -- *clm-file-name* (filename or frame>file generator)\n" $+
$" \\ :outloc  -- 0 (start frame in outfile)\n" $+
$" \\ :frames  -- infile frames\n" $+
$" \\ :inloc   -- 0 (start frame in infile)\n" $+
$" \\ :mixer   -- nil (mixer generator)\n" $+
$" \\ :envs    -- nil (array of array of envelope generators)\n" $+
$" \\ INFILE and OUTFILE can be a filename as String or a CLM-Generator\n" $+
$" :infile $\" oboe.snd\" mus-mix play-sound \\ mixes oboe.snd to test.snd\n" $+
$" 0.725e 1 >mixer value mx\n" $+
$" :infile $\" oboe.snd\" :mixer mx mus-mix play-sound \\ through mixer MX\n" $+
$" $\" oboe.snd\" make-file>frame value in-gen\n" $+
$" :infile in-gen :frames 10000 :mixer mx mus-mix play-sound\n" $+
$" array[ array[ :envelope vct[ 0e 0e 1e 1e ] :end 10000 make-env ] ] value envs\n" $+
$" 0.5e 1 >mixer value mx\n" $+
$" :infile $\" oboe.snd\" :frames 10000 :mixer mx :envs envs mus-mix play-sound" $+ help!

' make-ssb-am $" make-ssb-am ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :frequency -- 440e\n" $+
$" \\ :order     -- 40\n" $+
$" make-ssb-am value gen" $+ help!
' ?ssb-am $" ?ssb-am ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Ssb-am" $+ help!
' ssb-am $" ssb-am ( f: insig f: fm gen -- f: result ) \\ 0e 0e gen ssb-am f." help!

' make-src $" make-src ( keyword-args -- gen )\n" 
$" \\ === Sampling-Rate Conversion ===\n" $+
$" \\ keywords and default values\n" $+
$" \\ :input -- nil (xt)\n" $+
$" \\ :srate -- 1e\n" $+
$" \\ :width -- 10\n" $+
$" \\ INPUT can be a readin generator or an XT with stack effect ( dir -- f: value )\n" $+
$" \\ To convert a readin generator in an XT one can use make-readin-cb.\n" $+
$" \\ The run function SRC can only take an XT.\n" $+
$" :file $\"pistol.snd\" make-readin value rd\n" $+
$" :input rd make-src value sr   0e sr src f.\n" $+
$" \\ or\n" $+
$" rd make-readin-cb value cb\n" $+
$" make-src value sr\n" $+
$" :input cb 0e sr src f." $+ help!
' ?src $" ?src ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Src" $+ help!
' src $" src ( keyword-args f: pm gen -- f: value )\n" 
$" \\ keywords and default values\n" $+
$" \\ :input -- nil (xt)\n" $+
$" \\ INPUT is an XT with stack effect ( dir -- f: value )\n" $+
$" 0e sr src f.\n" $+
$" :input lambda: ( dir -- r ) drop rd readin ; 0e sr src f." $+ help!

' make-convolve $" make-convolve ( keyword-args -- gen )\n" 
$" \\ === Convolve ===\n" $+
$" \\ keywords and default values\n" $+
$" \\ :input    -- nil (xt)\n" $+
$" \\ :filter   -- nil (vct, required)\n" $+
$" \\ :fft-size -- 16\n" $+
$" \\ INPUT can be a readin generator or an XT with stack effect ( dir -- f: value )\n" $+
$" \\ To convert a readin generator in an XT one can use make-readin-cb.\n" $+
$" \\ The run function CONVOLVE can only take an XT.\n" $+
$" :file $\" pistol.snd\" make-readin value rd\n" $+
$" vct[ 0.5e 0.2e 0.1e 0.05e 0e 0e 0e 0e ] value filt\n" $+
$" :input rd :filter filt make-convolve value cv" $+ help!
' ?convolve $" ?convolve ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Convolve" $+ help!
' convolve $" convolve ( input-xt gen -- f: value )\n" 
$" \\ keywords and default values\n" $+
$" \\ :input -- nil (xt)\n" $+
$" \\ INPUT is an XT with stack effect ( dir -- f: value )\n" $+
$" cv convolve f.\n" $+
$" :input lambda: drop rd readin ; cv convolve f." $+ help!
' convolution $" convolution ( rl1-vct rl2-vct size -- ) \\ result in RL1-VCT" help!
' convolve-files $" convolve-files ( fname1 fname2 f: maxamp fname3 -- )\n"
$" $\" oboe.snd\" $\" fyow.snd\" 0.5e $\" test.snd\" convolve-files" $+ help!

' make-granulate $" make-granulate ( keyword-args -- gen )\n" 
$" \\ === Granular Synthesis ===\n" $+
$" \\ keywords and default values\n" $+
$" \\ :input     -- nil (xt)\n" $+
$" \\ :expansion -- 1e\n" $+
$" \\ :length    -- 0.15e\n" $+
$" \\ :scaler    -- 0.6e\n" $+
$" \\ :hop       -- 0.05e\n" $+
$" \\ :ramp      -- 0.4e\n" $+
$" \\ :jitter    -- 1e\n" $+
$" \\ :max-size  -- 0\n" $+
$" \\ :edit      -- nil (xt)\n" $+
$" \\ INPUT can be a readin generator or an XT with stack effect ( dir -- f: value )\n" $+
$" \\ To convert a readin generator in an XT one can use make-readin-cb.\n" $+
$" \\ The run function GRANULATE can only take an XT.\n" $+
$" \\ INPUT  XT ( dir -- f: value )\n" $+
$" \\ EDIT   XT ( gen -- samp-len|0 )\n" $+
$" :file $\" pistol.snd\" make-readin value rd\n" $+
$" rd make-granulate value gr" $+ help!
' ?granulate $" ?granulate ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Granulate" $+ help!
' granulate $" granulate ( keyword-args gen -- f: value )\n" 
$" \\ keywords and default values\n" $+
$" \\ :input -- nil (xt)\n" $+
$" \\ :edit  -- nil (xt)\n" $+
$" \\ INPUT XT ( dir -- f: value )\n" $+
$" \\ EDIT  XT ( gen -- samp-len|0 )\n" $+
$" gr granulate f.\n" $+
$" :input lambda: ( dir -- r ) drop rd readin ; gr granulate f." $+ help!

' make-phase-vocoder $" make-phase-vocoder ( keyword-args -- gen )\n" 
$" \\ === Phase Vocoder === \n" $+
$" \\ keywords and default values\n" $+
$" \\ :input      -- nil (xt)\n" $+
$" \\ :fft-size   -- 512\n" $+
$" \\ :overlap    -- 4\n" $+
$" \\ :interp     -- 128\n" $+
$" \\ :pitch      -- 1e\n" $+
$" \\ :analyze    -- nil (xt)\n" $+
$" \\ :edit       -- nil (xt)\n" $+
$" \\ :synthesize -- nil (xt)\n" $+
$" \\ INPUT can be a readin generator or an XT with stack effect ( dir -- f: value )\n" $+
$" \\ To convert a readin generator in an XT one can use make-readin-cb.\n" $+
$" \\ The run function PHASE-VOCODER can only take an XT.\n" $+
$" \\ INPUT      XT ( dir -- f: value )\n" $+
$" \\ ANALYZE    XT ( input-xt gen -- f )\n" $+
$" \\ EDIT       XT ( gen -- n )\n" $+
$" \\ SYNTHESIZE XT ( gen -- r )\n" $+
$" \\ If ANALYZE returns true use internal computation,\n" $+
$" \\ otherwise skip internal computation.\n" $+
$" make-phase-vocoder value pv\n" $+
$" :file $\" test.snd\" make-readin value rd\n" $+
$" :input rd make-phase-vocoder value pv" $+ help!
' ?phase-vocoder $" ?phase-vocoder ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Phase-vocoder" $+ help!
' phase-vocoder $" phase-vocoder ( keyword-args gen -- f: value )\n" 
$" \\ keywords and default values\n" $+
$" \\ :input      -- nil (xt)\n" $+
$" \\ :analyze    -- nil (xt)\n" $+
$" \\ :edit       -- nil (xt)\n" $+
$" \\ :synthesize -- nil (xt)\n" $+
$" \\ INPUT      XT ( dir -- f: value )\n" $+
$" \\ ANALYZE    XT ( input-xt gen -- f )\n" $+
$" \\ EDIT       XT ( gen -- n )\n" $+
$" \\ SYNTHESIZE XT ( gen -- r )\n" $+
$" \\ If ANALYZE returns true use internal computation,\n" $+
$" \\ otherwise skip internal computation.\n" $+
$" ph phase-vocoder f.\n" $+
$" :input lambda: ( dir -- r ) drop rd readin ; ph phase-vocoder f." $+ help!
' pv-amp-increments@ $" pv-amp-increments@ ( idx gen -- f: value )" help!
' pv-amp-increments! $" pv-amp-increments! ( f: value idx gen -- )" help!
' pv-amp-increments>vct $" pv-amp-increments>vct ( gen -- vct )" help!
' pv-amps@ $" pv-amps@ ( idx gen -- f: value )" help!
' pv-amps! $" pv-amps! ( f: value idx gen -- )" help!
' pv-amps>vct $" pv-amps>vct ( gen -- vct )" help!
' pv-freqs@ $" pv-freqs@ ( idx gen -- f: value )" help!
' pv-freqs! $" pv-freqs! ( f: value idx gen -- )" help!
' pv-freqs>vct $" pv-freqs>vct ( gen -- vct )" help!
' pv-phases@ $" pv-phases@ ( idx gen -- f: value )" help!
' pv-phases! $" pv-phases! ( f: value idx gen -- )" help!
' pv-phases>vct $" pv-phases>vct ( gen -- vct )" help!
' pv-phase-increments@ $" pv-phase-increments@ ( idx gen -- f: value )" help!
' pv-phase-increments! $" pv-phase-increments! ( f: value idx gen -- )" help!
' pv-phase-increments>vct $" pv-phase-increments>vct ( gen -- vct )" help!
' pv-outctr@ $" pv-outctr@ ( gen -- out-field )" help!
' pv-outctr! $" pv-outctr! ( field gen -- )" help!
' environ@ $" environ@ ( gen -- closure )\n"
$" \\ Src, Convolve, Granulate and Phase-vocoder only!\n" $+
$" \\ returns a Closure struct of GEN with the following entries:\n" $+
$" \\                    XT stack effect\n" $+
$" \\ input-xt      -- ( dir          -- r )\n" $+
$" \\ edit-xt       -- ( gen          -- n )\n" $+
$" \\ analyze-xt    -- ( input-xt gen -- f )\n" $+
$" \\ synthesize-xt -- ( gen          -- r )\n" $+
$" \\ mus-gen       -- current Granulate or Phase-vocoder generator\n" $+ help!

' make-fcomb $" make-fcomb ( keyword-args -- gen )\n" 
$" \\ keywords and default values\n" $+
$" \\ :scaler -- 1e\n" $+
$" \\ :size   -- *table-size*\n" $+
$" \\ :a0     -- 0e\n" $+
$" \\ :a1     -- 0e\n" $+
$" make-fcomb value gen" $+ help!
' ?fcomb $" ?fcomb ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Fcomb" $+ help!
' fcomb $" fcomb ( f: input gen -- f: value ) \\ 0e gen fcomb f. cr" help!

' make-buffer $" make-buffer ( keyword-args -- gen )\n"
$" \\ keywords and default values\n" $+
$" \\ :size -- *table-size*\n" $+
$" \\ :time -- 0\n" $+
$" make-buffer value gen\n" $+
$" \\ BUFFER is no longer in libsndlib.so" $+ help!
' ?buffer $" ?buffer ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Buffer" $+ help!
' buffer>sample $" buffer>sample ( gen -- f: value ) \\ returns next sample of Buffer" help!
' sample>buffer $" sample>buffer ( f: value gen -- ) \\ stores VALUE in Buffer" help!

' sound-samples $" sound-samples ( str -- u ) \\ returns samples of sound file" help!
' sound-frames $" sound-frames ( str -- u ) \\ returns frames of sound file" help!
' sound-chans $" sound-chans ( str -- n ) \\ returns channels of sound file" help!
' sound-srate $" sound-srate ( str -- n ) \\ returns sampling rate of sound file" help!
' sound-header-type $" sound-header-type ( str -- n )\n" 
$" \\ returns number of header type of sound file\n" $+
$" $\" test.snd\" sound-header-type .header-type" $+ help!
' sound-data-format $" sound-data-format ( str -- n )\n" 
$" \\ returns number of header format of sound file\n" $+
$" $\" test.snd\" sound-data-format .data-format" $+ help!
' sound-length $" sound-length ( str -- u ) \\ returns raw length of sound file" help!
' .header-type $" .header-type ( n -- ) \\ prints header type as string" help!
' .data-format $" .data-format ( n -- ) \\ prints data format as string" help!
' sound-comment $" sound-comment ( str -- str|f ) \\ returns comment as String" help!
' sound-duration $" sound-duration ( str -- f: dur ) \\ returns duration in seconds" help!
' sound-open-input $" sound-open-input ( str -- fd )\n"
$" \\ returns new file descriptor for use with sound-read" $+ help!
' sound-open-output $" sound-open-output ( fname srate chans format header comment -- fd )\n"
$" \\ returns new file descriptor for use with sound-write" $+ help!
' sound-close-input $" sound-close-input ( fd -- ) \\ close descriptor's file" help!
' sound-close-output $" sound-close-output ( fd bytes -- ) \\ close descriptor's file" help!
' sound-read $" sound-read ( fd beg end chans sd -- bytes )\n"
$" \\ reads file from BEG to END with CHANS channels to Sound-data object SD\n" $+
$" fd 0 128 1 sd sound-read" $+ help!
' sound-write $" sound-write ( fd beg end chans sd -- )\n"
$" \\ writes file from BEG to END with CHANS channels from Sound-data object SD\n" $+
$" fd 0 128 1 sd sound-write" $+ help!

' audio-open-input $" audio-open-input ( dev srate chans format size -- fd )\n" 
$" \\ opens audio device DEV with SRATE, CHANS, FORMAT and SIZE for reading\n" $+
$" \\ and returns file descriptor FD for use with audio-read\n" $+
$" mus-audio-default 22050 1 mus-lshort 1024 audio-open-input value fd" $+ help!
' audio-open-output $" audio-open-output ( dev srate chans format size -- fd )\n" 
$" \\ opens audio device DEV with SRATE, CHANS, FORMAT and SIZE for writing\n" $+
$" \\ and returns file descriptor FD for use with audio-write\n" $+
$" \\ returns file descriptor from device DEV with SRATE, FORMAT and SIZE for audio-write\n" $+
$" mus-audio-default 22050 1 mus-lshort 1024 audio-open-output value fd" $+ help!
' audio-close $" audio-close ( line -- ) \\ close device connected to LINE (i.e. fd)" help!
' audio-read $" audio-read ( line sd frames -- n )\n"
$" \\ read from LINE FRAMES to Sound-data object SD, returns read frames\n" $+
$" dac-fd sd 128 audio-read . cr" $+ help!
' audio-write $" audio-write ( line sd frames -- n )\n"
$" \\ write to DAC-FD FRAMES from Sound-data object SD, returns written frames\n" $+
$" dac-fd sd 128 audio-write . cr" $+ help!
' audio-mixer-read $" audio-mixer-read ( device field chan -- f: val vals-array )" help!
' audio-mixer-write $" audio-mixer-write ( device field chan f: val -- f: res vals-ary )" help!

' audio-record! $" audio-record! ( device -- n ) \\ sets DEVICE to recording device" help!
' audio-reclev! $" audio-reclev! ( chans f: value -- ) \\ sets recordlevel for all CHANS" help!
' audio-srate $" audio-srate ( -- srate ) \\ returns current srate of 1st sound card" help!
' audio-chans $" audio-chans ( device -- chans ) returns DEVICE's channels" help!

' make-dlocsig $" make-dlocsig ( f: start f: dur path keyword-args -- gen )\n"
$" \\ keywords and default values\n" $+
$" \\ :scaler        -- 1e\n" $+
$" \\ :reverb-amount -- 0.05e\n" $+
$" \\ :output-power  -- 1.5e\n" $+
$" \\ :reverb-power  -- 0.5e\n" $+
$" \\ :output        -- *output*\n" $+
$" \\ :revout        -- *reverb*\n" $+
$" \\ :render-using  -- amplitude-panning\n" $+
$" 0e 2e path make-dlocsig" $+ help!
' make-dlocsig-args $" make-dlocsig-args ( keyword-args -- args )\n"
$" \\ keywords and default values\n" $+
$" \\ :scaler        -- 1e\n" $+
$" \\ :reverb-amount -- 0.05e\n" $+
$" \\ :output-power  -- 1.5e\n" $+
$" \\ :reverb-power  -- 0.5e\n" $+
$" \\ :output        -- *output*\n" $+
$" \\ :revout        -- *reverb*\n" $+
$" \\ :render-using  -- amplitude-panning\n" $+
$" \\ intended for use with make-dlocsig-with-args" $+ help!
' make-dlocsig-with-args $" make-dlocsig-with-args ( f: start f: dur path args -- gen )\n"
$" \\ ARGS is a hash created e.g. by make-dlocsig-args" $+ help!
' ?dlocsig $" ?dlocsig ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Dlocsig" $+ help!
' dlocsig $" dlocsig ( f: input samp gen -- ) \\ writes INPUT to SAMP of GEN" help!
' dl-move $" dl-move ( f: start file path f: scl f: reverb f: opow f: rpow rend -- )" help!
' dl-move-with-args $" dl-move-with-args ( f: start file path args -- )\n"
$" \\ ARGS is a hash created e.g. by make-dlocsig-args" $+ help!
' move-sound $" move-sound ( body-xt keyword-args -- outfile )\n"
$" \\ keywords and default values\n" $+
$" \\ move-sound args\n" $+
$" \\ :begin         -- 0e\n" $+
$" \\ :channels      -- 1\n" $+
$" \\ :path          -- vct[ -10e 10e 0e 1e ] vct[ 0e 5e 1e 1e ] vct[ 10e 10e 0e 1e ]\n" $+
$" \\                       3 >array make-path\n" $+
$" \\ dlocsig args\n" $+
$" \\ :scaler        -- 1e\n" $+
$" \\ :reverb-amount -- 0.05e\n" $+
$" \\ :output-power  -- 1.5e\n" $+
$" \\ :reverb-power  -- 0.5e\n" $+
$" \\ :render-using  -- amplitude-panning\n" $+
$" \\ move-sound should be part of with-sound's body-xt:\n" $+
$" lambda:\n" $+
$"   make-spiral-path { path }\n" $+
$"   0e 2e 440e 0.3e ['] fm-violin :path path :channels 4 move-sound drop\n" $+
$"   path gen-free\n" $+
$" ; :play true :statistics true :channels 4 with-sound\n" $+ help!

' make-path $" make-path ( array keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :3d                -- true\n" $+
$" \\ :polar             -- false\n" $+
$" \\ :error             -- 0.01e\n" $+
$" \\ :initial-direction -- vct[ 0e 0e 0e ]\n" $+
$" \\ :final-direction   -- vct[ 0e 0e 0e ]\n" $+
$" \\ ARRAY should be an array of vcts or a vct\n" $+
$" vct[ -10e 10e 0e 5e 10e 10e ] make-path\n" $+
$" vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array make-path" $+ help!
' make-polar-path $" make-polar-path ( array keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :3d                -- true\n" $+
$" \\ :error             -- 0.01e\n" $+
$" \\ :initial-direction -- vct[ 0e 0e 0e ]\n" $+
$" \\ :final-direction   -- vct[ 0e 0e 0e ]\n" $+
$" \\ ARRAY should be an array of vcts or a vct\n" $+
$" vct[ -10e 10e 0e 5e 10e 10e ] make-polar-path\n" $+
$" vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array make-polar-path" $+ help!
' make-closed-path $" make-closed-path ( array keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :3d    -- true\n" $+
$" \\ :polar -- false\n" $+
$" \\ :error -- 0.01e\n" $+
$" \\ ARRAY should be an array of vcts or a vct\n" $+
$" vct[ -10e 10e 0e 5e 10e 10e ] make-closed-path\n" $+
$" vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array make-closed-path" $+ help!
' make-literal-path $" make-literal-path ( array keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :3d    -- true\n" $+
$" \\ :polar -- false\n" $+
$" \\ ARRAY should be an array of vcts or a vct\n" $+
$" vct[ -10e 10e 0e 5e 10e 10e ] make-literal-path\n" $+
$" vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array make-literal-path" $+ help!
' make-literal-polar-path $" make-literal-polar-path ( array keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :3d -- true\n" $+
$" \\ ARRAY should be an array of vcts or a vct\n" $+
$" vct[ -10e 10e 0e 5e 10e 10e ] make-literal-polar-path\n" $+
$" vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array make-literal-polar-path" $+ help!
' make-spiral-path $" make-spiral-path ( keyword-args -- path )\n"
$" \\ keywords and default values\n" $+
$" \\ :start-angle -- 0e\n" $+
$" \\ :turns       -- 2e\n" $+
$" :start-angle 270e :turns 5.5e make-spiral-path" $+ help!

' make-rnd $" make-rnd ( vct -- rnd )\n"
$" \\ returns a weighted number object where VCT is vct[ weight1 val1 ... weightn valn ]\n" $+
$" \\ This is an example from cm-examp.gfm\n" $+
$" \\ : cage { offset -- }\n" $+
$" \\     vct[ |S |A |T |T ]    { rhy-ary }\n" $+
$" \\     vct[ 0e 0.5e 90e 4e ] { ary-interp }\n" $+
$" \\     0e { f: weight }\n" $+
$" \\     100 0 do\n" $+
$" \\         rhy-ary rnd-pick vct@ { f: rhy }\n" $+
$" \\         weight |G3\n" $+
$" \\         weight |A3\n" $+
$" \\         1e     |Bf3\n" $+
$" \\         weight |C3\n" $+
$" \\         1e     |D4\n" $+
$" \\         weight |E4\n" $+
$" \\         1e     |F4\n" $+
$" \\         weight |G4 16 >vct MAKE-RND { rn }\n" $+
$" \\         i s>f ary-interp interp to weight\n" $+
$" \\         now@ rhy RN RND-RANDOMIZE f>s offset + keynum>hertz 0.3e 1e cm-inst\n" $+
$" \\         rn gen-free\n" $+
$" \\         rhy wait\n" $+
$" \\     loop\n" $+
$" \\     rhy-ary free-vct\n" $+
$" \\     ary-interp free-vct\n" $+
$" \\ ;\n" $+
$" \\ RND-RANDOMIZE returns a weighted number." $+ help!
' ?rnd $" ?rnd ( obj -- f )\n"
$" \\ returns true if OBJ is of kind Rnd" $+ help!
' rnd-randomize $" rnd-randomize ( rnd -- f: val ) returns a weighted number of RND" help!

\ gfm.fs ends here
