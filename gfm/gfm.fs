\ gfm.fs -- GFM load file -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Thu Aug 28 00:54:28 CEST 2003
\ Last: Sat Oct 09 01:45:36 CEST 2004
\ Ident: $Id: gfm.fs,v 1.85 2004/10/08 23:46:36 mike Exp $

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

\ Below the constants from clm.h and sndlib.h you will find global
\ variables which you can set to fit your needs.  Using fsndlib.fs
\ some variables are meaningless, e.g. *clm-header-type* (mus-next) or
\ *clm-data-format* (mus-lshort); fsndlib.fs reads and writes only
\ NeXT/Sun files (RIFF files can be played but not readin by
\ make-readin and friends); csndlib.fs has no such restriction.

\ Code:

s" 09-10-2004" 2constant gfm-version

s" gforth" environment? [if] 2dup
    s" 0.6.0" compare 0< [if] s" sorry, we need at least gforth 0.6.0 or higher" type cr bye [then]
    s" 0.6.2" compare 0< [if] ' lastxt alias latestxt [then]
[else]
    s" sorry, we need gforth" type cr bye
[then]

[ifdef] *clm-use-csndlib*
    *clm-use-csndlib* [if]
	[ifdef] av-call-int
	    true to *clm-use-csndlib*
	[else]
	    false to *clm-use-csndlib*
	[then]
    [then]
[else]
    [ifdef] av-call-int
	true value *clm-use-csndlib*
    [else]
	false value *clm-use-csndlib*
    [then]
[then]

require utils.fs

only forth also definitions
vocabulary GFMusic GFMusic definitions

\ === clm.h ===
00 constant mus-interp-none
01 constant mus-interp-linear
02 constant mus-interp-sinusoidal
03 constant mus-interp-all-pass
04 constant mus-interp-lagrange
05 constant mus-interp-bezier
06 constant mus-interp-hermite

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

\ === Global User Variables (settable in ~/.gfmrc) ===
true  		  value *clm-play*
true  		  value *clm-statistics*
true  		  value *clm-verbose*
true  		  value *clm-delete-reverb*
false 		  value *clm-reverb*
1     		  value *clm-channels*
1     		  value *clm-reverb-channels*
22050             value *clm-srate*
1024 8 *          value *clm-rt-bufsize*
mus-interp-linear value *clm-locsig-type*
mus-next          value *clm-header-type*
mus-lshort        value *clm-data-format*
mus-audio-default value *clm-device*
false 		  value *clm-notehook*
s" test.snd"     $value *clm-file-name*
s" test.reverb"  $value *clm-reverb-file-name*
s" intern"       $value *clm-player*           
s" "             $value *clm-comment*
0.05e 		 fvalue *clm-reverb-amount*
1.00e 		 fvalue *clm-decay-time*
512   		  value *clm-table-size*

\ Don't use SOUND-MAXAMP (very slow) in PLAY-SOUND and WITH-SOUND if
\ sound duration is longer than *clm-maxamp-time* in seconds.
30e              fvalue *clm-maxamp-time*
\ Contains only one path, not a list.
s" "             $value *clm-search-list*
\ Array of Strings of instrument names which are created with
\ INSTRUMENT: ;INSTRUMENT (filled automatically).
0 make-array      value *clm-instruments*

\ internal global variables
*clm-channels*        value *channels*
*clm-srate*           value *srate*
*clm-reverb-channels* value *reverb-channels*
*clm-table-size*      value *table-size*
*clm-rt-bufsize*      value *rt-bufsize*
*clm-locsig-type*     value *locsig-type*
*clm-verbose*         value *verbose*	\ read in notelist.fs (event-info) and clm-ins.fs (reverbs)
*clm-notehook*        value *notehook*	\ read in notelist.fs (ins-info)

false value *output*
false value *reverb*
false value *input*
false value *locsig*

pi f2* fconstant two-pi
pi f2/ fconstant half-pi
1024 1024 20 * * constant max-table-size

\ Takes value of environment variable $SFDIR or current directory if
\ $SFDIR isn't defined.
s" SFDIR" getenv dup 0= [if] 2drop s" pwd" shell [then] s" /" $+ $to *clm-search-list*

\ takes info from environment variable $USER
: make-default-comment { -- c-addr u }
    s" Written " >$ s" date" shell $+ s"  by " $+ s" USER" getenv $+
    s"  at " $+ s" hostname" shell $+
    [ environment-wordlist >order ]
    s"  using gforth " $+ gforth $+ s"  [" $+ os-class $+
    [ previous ]
    s" ], GFM Gforth Music of " $+ gfm-version $+
;
: .sndversion ( -- )
    *verbose* if
	script? if ." \  " then
	." using " *clm-use-csndlib* if ." csndlib" else ." fsndlib" then
	script? if cr then
    then
;

\ === Generall Music-V Struct ===
\ used in fsndlib.fs as well as in gfm-defs.fs and gfm-gens.fs
struct
    cell% field location
    cell% field data-buffer
    cell% field buffer-length
    cell% field buffer-order
    cell% field cosines
    cell% field interpolation-type
    cell% field get-inspect
    cell% field get-gen-free
    cell% field get-freq
    cell% field set-freq
    cell% field get-scaler
    cell% field set-scaler
    cell% field get-xcoeffs
    cell% field get-mus-run
    double% field gen-name
    float% field scaler
    float% field current-value
    float% field fill-time
end-struct music5%

\ === VCT, vct.h ===
struct
    cell% field v-len
    cell% field v-data
    cell% field v-orig
    double% field v-name
end-struct vct%

: vct-data { v -- c-ary } v v-data @ ;
: vct-length { v -- u } v v-len @ ;
: vct@ { idx v -- r } assert1( idx 0 v v-len @ within ) v v-data @ idx sfloats + sf@ ;
: vct! { f: val idx v -- } assert1( idx 0 v v-len @ within ) val v v-data @ idx sfloats + sf! ;
: vct-scale! { f: scl v -- } v vct-length 0 do i v vct@ scl f* i v vct! loop ;
: vct-fill! { f: val v -- } v vct-length 0 do val i v vct! loop ;
: vct-map! { xt v -- } v vct-length 0 do xt execute i v vct! loop ;
: vct-clear { v -- } v vct-length 0 do 0e i v vct! loop ;
: vct+ { v1 v2 -- } v1 vct-length v2 vct-length min 0 do i v1 vct@ i v2 vct@ f+ i v1 vct! loop ;
: vct- { v1 v2 -- } v1 vct-length v2 vct-length min 0 do i v1 vct@ i v2 vct@ f- i v1 vct! loop ;
: vct* { v1 v2 -- } v1 vct-length v2 vct-length min 0 do i v1 vct@ i v2 vct@ f* i v1 vct! loop ;
: vct-peak { v -- r } 0e v vct-length 0 do i v vct@ fabs fmax loop ;
: vct-offset! { f: val v -- } v vct-length 0 do i v vct@ val f+ i v vct! loop ;
: vct-each ( v -- ) postpone vct-length 0 postpone literal postpone do ; immediate
: ?vct-empty ( v -- f ) v-len 0= ;
: ?vct { obj -- f } try obj v-name 2@ s" vct" str= recover drop false endtry ;
: correct-vct-data { data len v -- }
    v v-data @ free throw
    len v v-orig @ v-len !
    data v v-orig @ v-data !
    len v v-len !
    data v v-data !
;
: vct-push! { f: val v -- }
    assert1( v ?vct )
    v v-len @ 1+ { len }
    len sfloats allocate throw { data }
    v v-data @ data len 1- sfloats move
    val data len 1- sfloats + sf!
    data len v correct-vct-data
;
: vct-pop! { v -- f: val }
    assert1( v ?vct )
    v v-len @ 0= if
	0e
    else
	v v-len @ 1- { len }
	len v vct@			\ result
	len sfloats allocate throw { data }
	v v-data @ data len sfloats move
	data len v correct-vct-data
    then
;
: vct-unshift! { f: val v -- }
    assert1( v ?vct )
    v v-len @ 1+ { len }
    len sfloats allocate throw { data }
    val data sf!
    v v-data @ data 1 sfloats + len 1- sfloats move
    data len v correct-vct-data
;
: vct-shift! { v -- f: val }
    assert1( v ?vct )
    v v-len @ 0= if
	0e
    else
	v v-len @ 1- { len }
	len sfloats allocate throw { data }
	0 v vct@			\ result
	v v-data @ 1 sfloats + data len sfloats move
	data len v correct-vct-data
    then
;
: vct-reverse! { v -- }
    assert1( v ?vct )
    v v-len @ { len }
    len 1- { idx }
    len sfloats allocate throw { data }
    len 0 u+do v v-data @ i sfloats + sf@  data idx sfloats + sf!  idx 1- to idx loop
    data len v correct-vct-data
;

' vct-length s" vct-length ( vct -- n ) \ v vct-length ." help!
' vct@ s" vct@ ( idx vct -- f: value ) \ 13 v vct@ f." help!
' vct! s" vct! ( f: value idx vct -- ) \ 0.7e 13 v vct!" help!
' vct-scale! s" vct-scale! ( f: scale vct -- ) \ 0.5e v vct-scale! v .vct" help!
' vct-fill! s" vct-fill! ( f: value vct -- ) \ 0.5e v vct-fill! v .vct" help!
' vct-map! s" vct-map! ( xt vct -- ) \ lambda: ( -- f: value ) 0.5e ; v vct-map! v .vct" help!
' vct-clear s" vct-clear ( v -- ) \ all elements reset to 0e" help!
' vct+ s" vct+ ( v1 v2 -- ) \ result of element-wise addition in V1" help!
' vct- s" vct- ( v1 v2 -- ) \ result of element-wise subtraction in V1" help!
' vct* s" vct* ( v1 v2 -- ) \ result of element-wise multiplication in V1" help!
' vct-peak s" vct-peak ( v -- r ) \ returns fabs fmax value of V" help!
' vct-offset! s" vct-offset! ( f: value v -- ) \ adds VALUE to all elements of V" help!
' vct-each s" ( vct -- ) \ v vct-each  i v vct@ f.  loop" help!
' ?vct-empty s" ?vct-empty ( v -- f )" help!
' ?vct s" ?vct ( obj -- f ) \ v ?vct ." help!
' vct-push! s\" vct-push! ( f: value v -- )\n" >$
s\" 0e 1e 2e 3e 4 >vct value v\n" $+
s\" 0.5e v vct-push! ==> #<vct: 0.0 1.0 2.0 3.0 0.5>" $+ help!
' vct-pop! s\" vct-pop! ( v -- f: value )\n" >$
s\" 0e 1e 2e 3e 4 >vct value v\n" $+
s\" v vct-pop! f. ==> 3.0; #<vct: 0.0 1.0 2.0>" $+ help!
' vct-unshift! s\" vct-unshift! ( f: value v -- )\n" >$
s\" 0e 1e 2e 3e 4 >vct value v\n" $+
s\" 0.3e v vct-unshift! ==> #<vct: 0.3 0.0 1.0 2.0 3.0>" $+ help!
' vct-shift! s\" vct-shift! ( v -- f: value )\n" >$
s\" 0e 1e 2e 3e 4 >vct value v\n" $+
s\" v vct-shift! f. ==> 0.0; #<vct: 1.0 2.0 3.0>" $+ help!
' vct-reverse! s\" vct-reverse! ( vct -- )\n" >$
s\" 0e 1e 2e 3e 4 >vct value v\n" $+
s\" v vct-reverse! ==> #<vct: 3.0 2.0 1.0 0.0>" $+ help!

\ === Sound-Data, sndlib2xen.h ===
\ sndlib.so compiled with --with-float-samples
struct
    cell% field sd-len
    cell% field sd-chans
    cell% field sd-data
    cell% field sd-orig
    double% field sd-name
end-struct sd%

: sound-data-length { sd -- n } sd sd-len @ ;
: sound-data-chans { sd -- n } sd sd-chans @ ;
: sound-data@ { frm chn sd -- r }
    assert1( frm 0 sd sd-len within chn 0 sd sd-chans within and )
    sd sd-data @ chn cells + @ frm sfloats + sf@
;
: sound-data! { f: val frm chn sd -- }
    assert1( frm 0 sd sd-len within chn 0 sd sd-chans within and )
    val sd sd-data @ chn cells + @ frm sfloats + sf!
;
: vct>sound-data { v chn sd -- }
    assert1( chn 0 sd sd-chans @ within )
    v v-len @ sd sd-len @ min 0 do
	v v-data @ i sfloats + sf@   sd sd-data @ chn cells + @ i sfloats + sf!
    loop
;
: sound-data>vct { v chn sd -- }
    assert1( chn 0 sd sd-chans @ within )
    v v-len @ sd sd-len @ min 0 do
	sd sd-data @ chn cells + @ i sfloats + sf@   v v-data @ i sfloats + sf!
    loop
;
: sound-data-clear { sd -- }
    sd sd-chans @ 0 do sd sd-len @ 0 do 0e sd sd-data @ j cells + @ i sfloats + sf! loop loop
;
: ?sound-data { obj -- f } try obj sd-name 2@ s" sound-data" str= recover drop false endtry ;

' sound-data-length s" sound-data-length ( sd -- )" help!
' sound-data-chans s" sound-data-chans ( sd -- n )" help!
' sound-data@ s" sound-data@ ( frame chan sd -- f: value ) \ 17 0 sd sound-data@ f." help!
' sound-data! s" sound-data! ( f: value frame chan sd -- ) \ 0.5e 17 0 sd sound-data!" help!
' vct>sound-data s" vct>sound-data ( vct chan sd -- ) \ v 0 sd vct>sound-data" help!
' sound-data>vct s" sound-data>vct ( chan sd -- vct ) \ 0 sd sound-data>vct value v" help!
' sound-data-clear s" sound-data-clear ( sd -- ) \ resets all values to 0e" help!
' ?sound-data s" ?sound-data ( obj -- f ) \ sd ?sound-data ." help!

\ === Load CLM Library ===
*clm-use-csndlib* [if] require csndlib.fs [else] require fsndlib.fs [then]
only forth also GFMusic definitions also CLM-Sndlib

1024 1024 * file-buffer-size!
8 array-print-length!
make-default-comment $to *clm-comment*
*srate* srate!

\ === Load .gfmrc if .gfmrc exists in current or $HOME dir ===
s" .gfmrc" load-init-file

\ internal global variables
*clm-channels*        to *channels*
*clm-srate*           to *srate*
*clm-reverb-channels* to *reverb-channels*
*clm-table-size*      to *table-size*
*clm-rt-bufsize*      to *rt-bufsize*
*clm-locsig-type*     to *locsig-type*
*clm-verbose*         to *verbose*
*clm-notehook*        to *notehook*

' mus-close alias close-input
' mus-close alias close-output
: open-input ( d: fname -- gen ) 0 0 1 make-readin ;
: open-output ( d: fname chans fmt typ -- gen ) 0. make-sample>file ;
: times>samples ( f: start f: dur -- len beg ) seconds>samples seconds>samples tuck + swap ;

: sound-data-maxamp { sd -- maxamps-vct }
    sd sd-chans @ make-vct { maxamps-v }
    sd sd-chans @ 0 do
	0e
	sd sd-len @ 0 do
	    sd sd-data @ j cells + @ i sfloats + sf@  fabs fmax
	loop
	i maxamps-v vct!
    loop
    maxamps-v
;
' sound-data-maxamp s\" sound-data-maxamp ( sd -- maxamps-vct )\n" >$
s\" \\ returns a sound-data-length vct with fabs fmax values for each channel" $+ help!

: sound-maxamp { d: fname -- maxamps-vct }
    fname sound-chans { chans }
    fname sound-frames { frames }
    *clm-rt-bufsize* frames min { bufsize }
    chans bufsize make-sound-data { sd }
    fname sound-open-input { snd-fd }
    chans make-vct { maxamps-v }
    frames 0 do
	snd-fd 0 bufsize 1- chans sd sound-read drop
	sd sound-data-maxamp { v }
	chans 0 do i v vct@ i maxamps-v vct@ fmax i maxamps-v vct! loop
	v free-vct
    bufsize +loop
    snd-fd sound-close-input
    sd sound-data-free
    maxamps-v
;
' sound-maxamp s\" sound-maxamp ( d: fname -- maxamps-vct )\n" >$
s\" \\ returns a vct with fabs fmax values for each channel of FNAME (slow)" $+ help!


\ === Playing sound files ===
: play-sound { d: fname -- }
    fname sound-chans { chans }
    fname sound-frames { frames }
    fname sound-srate { srate }
    *clm-verbose* if
	fname sound-duration { f: dur }
	script? 0= if cr then
	.\" \\ filename: \"" fname type .\" \"" cr
	." \    chans: " chans . bs ." , srate: " srate . cr
	." \   format: " fname sound-data-format .data-format
	."  [" fname sound-header-type .header-type ." ]" cr
	." \   length: " dur 3 f.r ." (" frames . ." frames)"
	dur *clm-maxamp-time* f< if	\ sound-maxamp is very slow
	    cr ." \   maxamp: [" fname sound-maxamp { v } v vct-each i v vct@ 3 f.r loop bs ." ]"
	    v free-vct
	then
	fname sound-comment ?dup-if cr ." \  comment: " type then
	script? if cr then
    then
    *clm-rt-bufsize* frames min { bufsize }
    chans bufsize make-sound-data { sd }
    fname sound-open-input { snd-fd }
    *clm-device* srate chans *clm-data-format* bufsize chans * shorts audio-open-output { dac-fd }
    frames 0 do
	snd-fd 0 bufsize 1- chans sd sound-read >r
	dac-fd sd r> audio-write drop
    bufsize +loop
    dac-fd audio-close
    snd-fd sound-close-input
    sd sound-data-free
;

\ --- keyword args used by with-sound (i.e. with-snd, with-dac) ---
struct
    cell% field args-play
    cell% field args-statistics
    cell% field args-verbose
    cell% field args-continue-old-file
    cell% field args-delete-reverb
    cell% field args-reverb
    cell% field args-channels
    cell% field args-reverb-channels
    cell% field args-srate
    cell% field args-rt-buffer-size
    cell% field args-table-size
    cell% field args-locsig-type
    cell% field args-header-type
    cell% field args-data-format
    cell% field args-device
    cell% field args-notehook
    double% field args-name
    double% field args-file-name
    double% field args-reverb-file-name
    double% field args-player
    double% field args-comment
    float% field args-reverb-amount
    float% field args-decay-time
end-struct ws-args%
: ws-args-init { ws -- ws }
    s" ws-args"            ws args-name 2!
    *clm-play*             ws args-play !
    *clm-statistics*       ws args-statistics !
    *clm-verbose*          ws args-verbose !
    false                  ws args-continue-old-file !
    *clm-delete-reverb*    ws args-delete-reverb !
    *clm-reverb*           ws args-reverb !
    *clm-channels*         ws args-channels !
    *clm-reverb-channels*  ws args-reverb-channels !
    *clm-srate*            ws args-srate !
    *clm-rt-bufsize* 	   ws args-rt-buffer-size !
    *clm-table-size* 	   ws args-table-size !
    *clm-locsig-type* 	   ws args-locsig-type !
    *clm-header-type* 	   ws args-header-type !
    *clm-data-format* 	   ws args-data-format !
    *clm-device*           ws args-device !
    *clm-notehook*         ws args-notehook !
    *clm-file-name*        ws args-file-name 2!
    *clm-reverb-file-name* ws args-reverb-file-name 2!
    *clm-player*           ws args-player 2!
    *clm-comment*          ws args-comment 2!
    *clm-reverb-amount*    ws args-reverb-amount f!
    *clm-decay-time*       ws args-decay-time f!
    ws
;
: ws-reset { ws -- }
    *clm-verbose*     	  to *verbose*
    *clm-channels*    	  to *channels*
    *clm-srate*       	  to *srate*
    *clm-reverb-channels* to *reverb-channels*
    *clm-rt-bufsize*  	  to *rt-bufsize*
    *clm-table-size*  	  to *table-size*
    *clm-locsig-type* 	  to *locsig-type*
    *clm-notehook*    	  to *notehook*
    ws free throw
;
: :ws-args { -- ws } ws-args% %alloc ws-args-init ;
: ?ws-args { ws -- f } try ws args-name 2@ s" ws-args" str= recover drop false endtry ;
: make-ws-args ( ?? -- ws )
    depth 0= if
	:ws-args
    else
	dup ?ws-args 0= if :ws-args then
    then
;
: :play { val -- ws }             make-ws-args { ws } val ws args-play ! ws ;
: :statistics { val -- ws }       make-ws-args { ws } val ws args-statistics ! ws ;
: :verbose { val -- ws }          make-ws-args { ws } val ws args-verbose ! ws ;
: :continue-old-file { val -- ws } make-ws-args { ws } val ws args-continue-old-file ! ws ;
: :delete-reverb { val -- ws }    make-ws-args { ws } val ws args-delete-reverb ! ws ;
: :reverb { val -- ws }           make-ws-args { ws } val ws args-reverb ! ws ;
: :channels { val -- ws }         make-ws-args { ws } val ws args-channels ! ws ;
: :reverb-channels { val -- ws }  make-ws-args { ws } val ws args-reverb-channels ! ws ;
: :srate { val -- ws }       	  make-ws-args { ws } val ws args-srate ! ws ;
: :rt-bufsize { val -- ws }  	  make-ws-args { ws } val ws args-rt-buffer-size ! ws ;
: :table-size { val -- ws }  	  make-ws-args { ws } val ws args-table-size ! ws ;
: :locsig-type { val -- ws } 	  make-ws-args { ws } val ws args-locsig-type ! ws ;
: :header-type { val -- ws } 	  make-ws-args { ws } val ws args-header-type ! ws ;
: :data-format { val -- ws } 	  make-ws-args { ws } val ws args-data-format ! ws ;
: :device { val -- ws }      	  make-ws-args { ws } val ws args-device ! ws ;
: :notehook { val -- ws }    	  make-ws-args { ws } val ws args-notehook ! ws ;
: :output { d: val -- ws }   	  make-ws-args { ws } val ws args-file-name 2! ws ;
: :rev-file-name { d: val -- ws } make-ws-args { ws } val ws args-reverb-file-name 2! ws ;
: :player { d: val -- ws }        make-ws-args { ws } val ws args-player 2! ws ;
: :comment { d: val -- ws }       make-ws-args { ws } val ws args-comment 2! ws ;
: :reverb-amount { f: val -- ws } make-ws-args { ws } val ws args-reverb-amount f! ws ;
: :decay-time { f: val -- ws }    make-ws-args { ws } val ws args-decay-time f! ws ;

: :play@ { ws -- f }              ws args-play @ ;
: :statistics@ { ws -- f }        ws args-statistics @ ;
: :verbose@ { ws -- f }           ws args-verbose @ ;
: :continue-old-file@ { ws -- f } ws args-continue-old-file @ ;
: :delete-reverb@ { ws -- f }     ws args-delete-reverb @ ;
: :reverb@ { ws -- w }            ws args-reverb @ ;
: :channels@ { ws -- n }          ws args-channels @ ;
: :reverb-channels@ { ws -- n }   ws args-reverb-channels @ ;
: :srate@ { ws -- n }             ws args-srate @ ;
: :rt-bufsize@ { ws -- n }        ws args-rt-buffer-size @ ;
: :table-size@ { ws -- n }        ws args-table-size @ ;
: :locsig-type@ { ws -- n }       ws args-locsig-type @ ;
: :header-type@ { ws -- n }       ws args-header-type @ ;
: :data-format@ { ws -- n }       ws args-data-format @ ;
: :device@ { ws -- n }            ws args-device @ ;
: :notehook@ { ws -- n }          ws args-notehook @ ;
: :output@ { ws -- d }            ws args-file-name 2@ ;
: :rev-file-name@ { ws -- d }     ws args-reverb-file-name 2@ ;
: :player@ { ws -- d }            ws args-player 2@ ;
: :comment@ { ws -- d }           ws args-comment 2@ ;
: :reverb-amount@ { ws -- r }     ws args-reverb-amount f@ ;
: :decay-time@ { ws -- r }        ws args-decay-time f@ ;

\ --- Locsig keyword args ---
struct
    float% field degree
    float% field distance
    float% field rev-amount
end-struct clm::locsig%

: clm::locsig-init { self -- }
    0.00e self degree f!
    1.00e self distance f!
    *clm-reverb-amount* self rev-amount f!
;
: .clm::locsig { self -- }
    ." #<clm::locsig degree: " self degree f@ 3 f.r bs
    ." , distance: " self distance f@ 3 f.r bs
    ." , reverb-amount: " self rev-amount f@ 3 f.r bs ." >" cr
;
clm::locsig% %alloc value *clm::locsig*
*clm::locsig* clm::locsig-init

: locsig-reset ( -- ) *clm::locsig* clm::locsig-init ;

: :locsig-degree@ ( -- f: degree ) *clm::locsig* degree f@ ;
: :locsig-degree ( f: degree -- ) *clm::locsig* degree f! ;
: :locsig-distance@ ( -- f: distance ) *clm::locsig* distance f@ ;
: :locsig-distance ( f: distance -- ) *clm::locsig* distance f! ;
: :locsig-reverb-amount@ ( -- f: reverb-amount ) *clm::locsig* rev-amount f@ ;
: :locsig-reverb-amount ( f: reverb-amount -- ) *clm::locsig* rev-amount f! ;

\ --- With-Snd Run-Instrument ---

: (run-snd-make-locsig) ( -- )
    :locsig-degree@
    :locsig-distance@
    :locsig-reverb-amount@
    *channels*
    *output*
    *reverb*
    *locsig-type* make-locsig to *locsig*
;

: (run-snd) ( f: start f: dur -- )
    postpone (run-snd-make-locsig) postpone times>samples postpone do
; immediate

: (finish-snd-run) *locsig* mus-free locsig-reset ;

: (end-run-snd) ( f: value -- )
    postpone r@ postpone *locsig* postpone locsig
    postpone loop
    postpone (finish-snd-run)
; immediate

\ --- With-Dac Run-Instrument ---
false value +dac-vct+
false value +dac-data+
false value +bufsize+
fvariable +start-time+ 0e +start-time+ f!

: time ( -- r ) utime d>f ;

: (run-dac-make-buffer) ( f: start f: dur -- f: start f: dur )
    (run-snd-make-locsig)
    fover { f: start }
    fdup seconds>samples *rt-bufsize* min to +bufsize+
    +bufsize+ make-vct to +dac-vct+
    *channels* +bufsize+ make-sound-data to +dac-data+
    begin time +start-time+ f@ f- start f>= until
;

: (run-dac) ( f: start f: dur -- )
    postpone (run-dac-make-buffer)
    postpone times>samples postpone do
    postpone +bufsize+ 0 postpone literal postpone do
; immediate

: (write-to-dac)
    *channels* 0 do
	i *locsig* locsig@ +dac-vct+ vct-scale! +dac-vct+ i +dac-data+ vct>sound-data
    loop
    *output* +dac-data+ +bufsize+ audio-write drop
;
: (finish-dac-run)
    +dac-data+ sound-data-free
    +dac-vct+ free-vct
    *locsig* mus-free
    locsig-reset
;
: (end-run-dac) ( f: value -- )
    postpone r@ postpone +dac-vct+ postpone vct! postpone loop
    postpone (write-to-dac)
    postpone +bufsize+ postpone +loop
    postpone (finish-dac-run)
; immediate

\ Usage: ' resflt-test with-sound
\        ' resflt-test false :play 2 :channels with-sound
\        lambda: resflt-test ; s" resflt.snd" :output with-sound
: with-snd ( xt ?? -- )
    :ws-args { ws }
    depth 0= abort" with-sound: need a body to play"
    depth 1 > if
	dup ?ws-args if
	    to ws
	else
	    true abort" with-sound: Usage: ' body-word [ :ws-args ... ] with-sound"
	then
    then
    { body-xt }
    ws :verbose@ to *verbose*
    ws :channels@ to *channels*
    ws :srate@ to *srate*
    ws :reverb-channels@ to *reverb-channels*
    ws :rt-bufsize@ to *rt-bufsize*
    ws :table-size@ to *table-size*
    ws :locsig-type@ to *locsig-type*
    ws :notehook@ to *notehook*
    ws :output@ { d: fname }
    ws :rev-file-name@ { d: rname }
    ws :data-format@ { fmt }
    ws :header-type@ { hdr }
    *output* { old-output }
    *reverb* { old-reverb }
    ws :continue-old-file@ if
	fname continue-sample>file
    else
	fname *channels* fmt hdr ws :comment@ make-sample>file
    then to *output*
    *output* ?sample>file 0= abort" with-sound: can't create *output*"
    ws :reverb@ if
	rname *reverb-channels* fmt hdr s" reverb file" make-sample>file to *reverb*
	*reverb* ?sample>file 0= abort" with-sound: can't create *reverb*"
    then
    start-timer
    try
	body-xt execute
    recover
	cr ." with-sound: can't execute body"
	*output* mus-close
	*reverb* if *reverb* mus-close then
	throw
    endtry
    ws :reverb@ if
	*reverb* mus-close
	rname sound-duration { f: dur }
	rname make-file>sample to *reverb*
	*reverb* ?file>sample 0= abort" with-sound: can't open *reverb*"
	try
	    0e dur ws :decay-time@ f+ ws :reverb@ execute
	recover
	    cr ." with-sound: can't execute reverb"
	    *reverb* mus-close
	    *output* mus-close
	    throw
	endtry
	*reverb* mus-close
	ws :delete-reverb@ if rname delete-file throw then
    then
    stop-timer
    *output* mus-close
    ws :statistics@ if
	fname sound-frames { frames }
	fname sound-duration { f: dur }
	script? 0= if cr then
	.\" \\ filename: \"" fname type .\" \"" cr
	." \    chans: " *channels* . bs ." , srate: " srate@ . cr
	." \   format: " fname sound-data-format .data-format
	."  [" fname sound-header-type .header-type ." ]" cr
	." \   length: " dur 3 f.r ." (" frames . ." frames)" cr
	." \     " .timer cr
	." \    " srate@ frames .timer-ratio
	dur *clm-maxamp-time* f< if	\ sound-maxamp is very slow
	    cr ." \   maxamp: [" fname sound-maxamp { v } v vct-each i v vct@ 3 f.r loop bs ." ]"
	    v free-vct
	then
	fname sound-comment ?dup-if cr ." \  comment: " type then
	script? if cr then
    then
    ws :play@ if
	ws :player@ s" intern" str= if
	    *clm-verbose* { old-verbose }
	    false to *clm-verbose*
	    fname play-sound
	    old-verbose to *clm-verbose*
	else
	    ws :player@ s"  " $+ fname $+ system
	then
    then
    old-reverb to *reverb*
    old-output to *output*
    ws ws-reset
;

\ similar like with-snd
: with-dac ( xt -- )
    :ws-args { ws }
    depth 0= abort" with-sound: need a body to play"
    depth 1 > if
	dup ?ws-args if
	    to ws
	else
	    true abort" with-sound: Usage: ' body-word [ :ws-args ... ] with-sound"
	then
    then
    { body-xt }
    ws :verbose@ to *verbose*
    ws :channels@ to *channels*
    ws :srate@ to *srate*
    ws :rt-bufsize@ to *rt-bufsize*
    ws :table-size@ to *table-size*
    ws :locsig-type@ to *locsig-type*
    ws :notehook@ to *notehook*
    s" dac " { d: fname }
    ws :data-format@ { fmt }
    *output* { old-output }
    *reverb* { old-reverb }
    ws :device@ *srate* *channels* fmt ws :rt-bufsize@ *channels* * shorts
    audio-open-output to *output*
    false to *reverb*
    ws :statistics@ if
	." \ filename: " fname type cr
	." \    chans: " *channels* . bs ." , srate: " *srate* . cr
	." \   format: " fmt .data-format cr
    then
    start-timer
    time +start-time+ f!
    try
	body-xt execute
    recover
	." with-sound: can't execute body" cr
	*output* audio-close
	throw
    endtry
    stop-timer
    *output* audio-close
    ws :statistics@ if ." \     " .timer cr then
    old-reverb to *reverb*
    old-output to *output*
    ws ws-reset
;

defer run-instrument immediate ( f: start f: dur -- )
defer end-run immediate ( f: value -- )
defer with-sound ( xt ?? -- )

[ifdef] *clm-dac-output*
    *clm-dac-output* [if]
	' with-dac is with-sound
	' (run-dac) is run-instrument
	' (end-run-dac) is end-run
    [else]
	' with-snd is with-sound
	' (run-snd) is run-instrument
	' (end-run-snd) is end-run
    [then]
[else]
    ' with-snd is with-sound
    ' (run-snd) is run-instrument
    ' (end-run-snd) is end-run
[endif]

require notelist.fs
require clm-ins.fs

only forth also definitions
GFMusic also
CLM-Sndlib also

\ simple online help;
\ usage: help make-oscil

' make-vct s" make-vct ( len -- vct ) \ 100 make-vct value v" help!
' >vct s" >vct ( u-float-values u -- vct ) \ 0e 1e 1e 1e 4 >vct value v" help!
' free-vct s" free-vct ( vct -- ) \ v free-vct" help!
' vct-copy s" vct-copy ( vct1 -- vct2 ) \ v1 vct-copy value v2" help!
' vct= s" vct= ( v1 v2 -- f ) \ v1 v2 vct= ." help!
' .vct s" .vct ( vct -- ) \ v .vct" help!

' make-sound-data s\" make-sound-data ( chans frames -- sd )\n" >$
s\" \\ 1 128 make-sound-data value sd" $+ help!
' sound-data-free s" sound-data-free ( sd -- ) \ sd sound-data-free" help!
' sound-data= s" sound-data= ( sd1 sd2 -- f ) \ sd1 sd2 sound-data= ." help!
' .sound-data s" .sound-data ( sd -- ) \ sd .sound-data" help!

' file-buffer-size@ s" file-buffer-size@ ( -- size ) \ default 8192" help!
' file-buffer-size! s" file-buffer-size! ( size -- ) \ default 8192" help!
' radians>hz s" radians>hz ( f: rads -- f: hz ) \ 10e radians>hz f." help!
' hz>radians s" hz>radians ( f: hz -- f: rads ) \ 10e hz>radians f." help!
' degrees>radians s" degrees>radians ( f: degrees -- f: rads ) \ 10e degrees>radians f." help!
' radians>degrees s" radians>degrees ( f: rads -- f: degrees ) \ 10e radians>degrees f." help!
' db>linear s" db>linear ( f: rads -- f: degrees ) \ 10e db>linear f." help!
' linear>db s" linear>db ( f: degrees -- f: rads ) \ 10e linear>db f." help!
' mus-srate@ s" mus-srate@ ( -- f: sr ) \ mus-srate@ f." help!
' mus-srate! s" mus-srate! ( f: sr -- ) \ 44100e mus-srate!" help!
' srate@ s" srate@ ( -- n ) \ srate@ ." help!
' srate! s" srate! ( n -- ) \ 44100 srate!" help!
' seconds>samples s" seconds>samples ( f: secs -- samps ) \ 0.3e seconds>samples ." help!
' samples>seconds s" samples>seconds ( samps -- f: secs ) \ 10000 samples>seconds f." help!
' times>samples s\" times>samples ( f: start f: dur -- limit beg )\n" >$
s\" 0.3e 2.1e times>samples do i . loop" $+ help!
' array-print-length@ s" array-print-length@ ( -- len ) \ array-print-length@ ." help!
' array-print-length! s" array-print-length! ( len -- ) \ 8 array-print-length!" help!
' sine-bank s\" sine-bank ( vct-amps vct-phases --  f: value )\n" >$
s\" sine-bank ( vct-amps vct-phases --  f: value )" $+ help!
' ring-modulate s" ring-modulate ( f: s1 f: s2 -- f: s3 ) \ s1 s2 f*" help!
' amplitude-modulate s" amplitude-modulate ( f: carrier f: in1 f: in2 -- f: value )" help!
' contrast-enhancement s" contrast-enhancement ( f: sig f: index -- f: value )" help!
' dot-product s" dot-product ( vct1 vct2 -- f: value )" help!
' clear-array s" clear-array ( vct --  ) \ alias vct-clear" help!
' polynomial s" polynomial ( vct f: x -- f: value ) \ evaluate a polynomial at x" help!
' multiply-arrays s" multiply-arrays ( vct-data vct-window -- ) \ result in vct-data" help!
' rectangular>polar s" rectangular>polar ( vct-rl vct-im -- ) \ result in vct-rl" help!
' polar>rectangular s" polar>rectangular ( vct-rl vct-im -- ) \ result in vct-rl" help!
' array-interp s" array-interp ( vct-wave f: phase -- f: value )" help!
' spectrum s\" spectrum ( rl-vct im-vct window-vct type -- ) \\ result in rl-vct\n" >$
s\" \\ real and imaginary data in RL-VCT and IM-VCT, returns (in RL-VCT)\n" $+
s\" \\ the spectrum thereof;\n" $+
s\" \\ WINDOW is the data window (a vct as returned by make-fft-window),\n" $+
s\" \\ and TYPE determines how the spectral data is scaled:\n" $+
s\" \\   0 = data in dB,\n" $+
s\" \\   1 = linear and normalized\n" $+
s\" \\   2 = linear and un-normalized." $+ help!
' fft s\" fft ( rl-vct im-vct len dir -- ) \\ result in rl-vct\n" >$
s\" \\ return the fft of RL-VCT and IM-VCT the real and imaginary parts of the\n" $+
s\" \\ data, len should be a power of 2, DIR = 1 for fft, -1 for inverse-fft" $+ help!
' make-fft-window s\" make-fft-window ( type size f: beta -- w-vct )\n" >$
s\" \\ return fft data window.  TYPE is one of the sndlib fft window\n" $+
s\" \\ identifiers such as kaiser-window, BETA is the window parameter, if\n" $+
s\" \\ any:\n" $+
s\" \\ mus-hamming-window 256 0e make-fft-window value v" $+ help!
' phase@ s" phase@ ( gen -- f: phase ) \ os phase@ f." help!
' phase! s" phase! ( f: phase gen -- ) \ os 10e phase!" help!
' data>vct s" data>vct ( gen -- vct-data ) \ os data>vct .vct" help!
' vct>data s" vct>data ( vct-data gen -- ) \ v os vct>data" help!
' data@ s" data@ ( idx gen -- f: value ) \ 128 os data@ f." help!
' data! s" data! ( f: value idx gen -- ) \ 0.5e 128 os data!" help!
' name@ s" name@ ( gen -- addr u ) \ os name@ type cr" help!
' offset@ s" offset@ ( gen -- f: offset ) \ os offset@ f." help!
' offset! s" offset! ( f: offset gen -- ) \ 10e os offset!" help!
' width@ s" width@ ( gen -- f: width ) \ os width@ f." help!
' width! s" width! ( f: width gen -- ) \ 10e os width!" help!
' file-name@ s" file-name@ ( gen -- addr u ) \ f file-name@ type cr" help!
' interp-type@ s" interp-type@ ( gen -- n )" help!
' cosines@ s" cosines@ ( gen -- n )" help!
' cosines! s" cosines! ( n gen -- )" help!
' feedback@ s" feedback@ ( gen -- r )" help!
' feedback! s" feedback! ( f: value gen -- )" help!
' feedforward@ s" feedforward@ ( gen -- r )" help!
' feedforward! s" feedforward! ( f: value gen -- )" help!
' channels@ s" channels@ ( gen -- chns ) \ *output* channels@ ." help!
' channel@ s" channel@ ( gen -- chn ) \ rd channel@ ." help!
' location@ s" location@ ( gen -- loc ) \ rd location@ ." help!
' location! s" location! ( loc gen -- ) \ 10 rd location!" help!

' make-oscil s" make-oscil ( f: freq f: phase -- gen ) \ 440e 0e make-oscil value os" help!
' ?oscil s" ?oscil ( obj -- f ) \ os ?oscil ." help!
' oscil s" oscil ( f: fm f: pm gen -- f: value ) \ 0e 0e os oscil f." help!

' make-sum-of-cosines s\" make-sum-of-cosines ( f: freq f: phase cosines -- gen )\n" >$
s\" 440e 0e 1 make-sum-of-cosines value os" $+ help!
' ?sum-of-cosines s" ?sum-of-cosines ( obj -- f ) \ os ?sum-of-cosines ." help!
' sum-of-cosines s" sum-of-cosines ( f: fm gen -- f: value ) \ 0e os sum-of-cosines f." help!

' make-sum-of-sines s\" make-sum-of-sines ( f: freq f: phase sines -- gen )\n" >$
s\" 440e 0e 1 make-sum-of-sines value os" $+ help!
' ?sum-of-sines s" ?sum-of-sines ( obj -- f ) \ os ?sum-of-sines ." help!
' sum-of-sines s" sum-of-sines ( f: fm gen -- f: value ) \ 0e os sum-of-sines f." help!

' make-delay s" make-delay ( max-size -- gen ) \ 10 make-delay value os" help!
' ?delay s" ?delay ( obj -- f ) \ os ?delay ." help!
' delay s" delay ( f: input f: pm gen -- f: value ) \ 0e 0e os delay f." help!
' tap s" tap ( f: loc gen -- f: value ) \ 0e os tap f." help!
' delay-tick s" delay-tick ( f: input gen -- ) \ 0e os delay-tick" help!

' make-comb s" make-comb ( f: scaler max-size -- gen ) \ 0e 10 make-comb value os" help!
' ?comb s" ?comb ( obj -- f ) \ gen ?comb ." help!
' comb s" comb ( f: input f: pm gen -- f: value ) \ 0e 0e gen comb f." help!

' make-notch s" make-notch ( f: scaler max-size -- gen ) \ 0e 10 make-notch value os" help!
' ?notch s" ?notch ( obj -- f ) \ gen ?notch ." help!
' notch s" notch ( f: input f: pm gen -- f: value ) \ gen 0e 0e notch f." help!

' make-all-pass s\" make-all-pass ( f: backward f: forward max-size -- gen )\n" >$
s\" 0e 0e 10 make-all-pass value os" $+ help!
' ?all-pass s" ?all-pass ( obj -- f ) \ gen ?all-pass ." help!
' all-pass s" all-pass ( f: input f: pm gen -- f: value ) \ gen 0e 0e all-pass f." help!

' make-average s" make-average ( max-size -- gen )" help!
' ?average s" ?average ( obj -- f )" help!
' average s" average ( f: input gen -- f: value ) \ moving window average" help!

' make-table-lookup s\" make-table-lookup ( f: freq f: phase vct-wave type -- gen )\n" >$
s\" 440e 0e 1e 1e 2 >vct false partials>wave mus-interp-linear make-table-lookup value gen" $+ help!
' ?table-lookup s" ?table-lookup ( obj -- f ) \ gen ?table-lookup ." help!
' table-lookup s" table-lookup ( f: fm gen -- f: value ) \ gen 0e table-lookup f." help!
' partials>wave s\" partials>wave ( vct-parts normalize -- vct-wave )\n" >$
s\" 1e 1e 2e 0.5e 4 >vct true partials>wave value wv\n" $+
s\" \\ take a list of partials (harmonic number and associated amplitude)\n" $+
s\" \\ and produces a waveform for use in table-lookup.\n" $+
s\" \\ If NORMALIZE is true, the resulting waveform goes between -1.0 and 1.0.\n" $+
s\" 1e 1e 2e 0.5e 4 >vct false partials>wave 440e 0e make-table-lookup value gen" $+ help!
' phase-partials>wave s\" phase-partials>wave ( vct-parts normalize -- vct-wave )\n" >$
s\" 1e 0.75e 0e 2e 0.25e pi 0.5e f* 6 >vct false phase-partials>wave value wv\n" $+
s\" \\ take a list of partials (harmonic number, amplitude, initial phase)\n" $+
s\" \\ and produce a waveform for use in table-lookup.\n" $+
s\" \\ If NORMALIZE is true, the resulting waveform goes between -1.0 and 1.0.\n" $+
s\" 1e 0.75e 0e 2e 0.25e pi 0.5e f* 6 >vct false\n" $+
s\" phase-partials>wave 440e 0e make-table-lookup value tb" $+ help!

' make-sawtooth-wave s\" make-sawtooth-wave ( f: freq f: amp f: phase -- gen )\n" >$
s\" 440e 1e pi make-sawtooth-wave value os" $+ help!
' ?sawtooth-wave s" ?sawtooth-wave ( obj -- f ) \ os ?sawtooth-wave ." help!
' sawtooth-wave s" sawtooth-wave ( f: fm gen -- f: value ) \ 0e os sawtooth-wave f." help!

' make-square-wave s\" make-square-wave ( f: freq f: amp f: phase -- gen )\n" >$
s\" 440e 1e 0e make-square-wave value os" $+ help!
' ?square-wave s" ?square-wave ( obj -- f ) \ os ?square-wave ." help!
' square-wave s" square-wave ( f: fm gen -- f: value ) \ 0e os square-wave f." help!

' make-triangle-wave s\" make-triangle-wave ( f: freq f: amp f: phase -- gen )\n" >$
s\" 440e 1e 0e make-triangle-wave value os" $+ help!
' ?triangle-wave s" ?triangle-wave ( obj -- f ) \ os ?triangle-wave ." help!
' triangle-wave s" triangle-wave ( f: fm gen -- f: value ) \ 0e os triangle-wave f." help!

' make-pulse-train s\" make-pulse-train ( f: freq f: amp f: phase -- gen )\n" >$
s\" 440e 1e two-pi make-pulse-train value os" $+ help!
' ?pulse-train s" ?pulse-train ( obj -- f ) \ os ?pulse-train ." help!
' pulse-train s" pulse-train ( f: fm gen -- f: value ) \ 0e os pulse-train f." help!

' rand-seed! s" rand-seed! ( u -- ) \ 12345 rand-seed!" help!
' rand-seed@ s" rand-seed@ ( -- u ) \ rand-seed@ u." help!
' mus-random s" mus-random ( f: amp -- f: value ) \ 0.5e mus-random f." help!
' random s" random ( f: amp -- f: value ) \ 0.5e random f." help!
' irandom s" irandom ( amp -- value ) \ 1 irandom ." help!

' make-rand s" make-rand ( f: freq f: amp -- gen ) \ 440e 1e make-rand value os" help!
' make-rand-dist s\" make-rand-dist ( f: freq f: amp vct-env -- gen )\n" >$
s\" 440e 1e -1e 1e 1e 1e 4 >vct make-rand-dist value os" $+ help!
' ?rand s" ?rand ( obj -- f ) \ os ?rand ." help!
' rand s" rand ( f: fm gen -- f: value ) \ 0e os rand f." help!

' make-rand-interp s\" make-rand-interp ( f: freq f: amp -- gen )\n" >$
s\" 440e 1e make-rand-interp value os" $+ help!
' make-rand-interp-dist s\" make-rand-interp-dist ( f: freq f: amp vct-env -- gen )\n" >$
s\" 440e 1e 0e 1e 1e 1e 4 >vct make-rand-interp-dist value os" $+ help!
' ?rand-interp s" ?rand-interp ( obj -- f ) \ os ?rand-interp ." help!
' rand-interp s" rand-interp ( f: fm gen -- f: value ) \ 0e os rand-interp f." help!

' make-asymmetric-fm s\" make-asymmetric-fm ( f: freq f: phase f: r f: ratio -- gen )\n" >$
s\" 440e 0e 1e 1e make-asymmetric-fm value os" $+ help!
' ?asymmetric-fm s" ?asymmetric-fm ( obj -- f ) \ os ?asymmetric-fm ." help!
' asymmetric-fm s\" asymmetric-fm ( f: index f: fm gen -- f: value )" >$
s\" \\ 0e 0e os asymmetric-fm f." $+ help!

' make-one-zero s" make-one-zero ( f: a0 f: a1 -- gen )" help!
' ?one-zero s" ?one-zero ( obj -- f )" help!
' one-zero s" one-zero ( f: input gen -- f: value )" help!

' make-one-pole s" make-one-pole ( f: a0 f: b1 -- gen )" help!
' ?one-pole s" ?one-pole ( obj -- f )" help!
' one-pole s" one-pole ( f: input gen -- f: value )" help!

' make-two-zero s" make-two-zero ( f: a0 f: a1 f: a2 -- gen )" help!
' make-zpolar s" make-zpolar ( f: radius f: freq -- gen )" help!
' ?two-zero s" ?two-zero ( obj -- f )" help!
' two-zero s" two-zero ( f: input gen -- f: value )" help!

' make-two-pole s" make-two-pole ( f: a0 f: b1 f: b2 -- gen )" help!
' make-ppolar s" make-ppolar ( f: radius f: freq -- gen )" help!
' ?two-pole s" ?two-pole ( obj -- f )" help!
' two-pole s" two-pole ( f: input gen -- f: value )" help!
' b2@ s" b2@ ( gen -- f: value )" help!
' b2! s" b2! ( f: value gen -- )" help!

' make-formant s\" make-formant ( f: radius f: freq f:  gain -- gen )\n" >$
s\" 0e 0e 1e make-formant value gen" $+ help!
' ?formant s" ?formant ( obj -- f ) \ os ?formant ." help!
' formant s" formant ( f: input obj -- f: value ) \ 0e os formant f." help!
' formant-bank s" formant-bank ( amps-vct formants-array f: input -- f: value )" help!


' make-sine-summation s\" make-sine-summation ( f: freq f: phase n f: a f: b-ratio -- gen )\n" >$
s\" 440e 0e 1 0.5e 1e make-sine-summation value os" $+ help!
' ?sine-summation s" ?sine-summation ( obj -- f ) \ os ?sine-summation ." help!
' sine-summation s" sine-summation ( f: fm gen -- f: value ) \ 0e os sine-summation f." help!

' make-filter s" make-filter ( order xcoeffs-vct ycoeffs-vct -- gen )" help!
' ?filter s" ?filter ( obj -- f ) \ os ?filter ." help!
' filter s" filter ( f: input gen -- f: value ) \ 0e os filter f." help!

' make-fir-filter s" make-fir-filter ( order xcoeffs-vct -- gen )" help!
' ?fir-filter s" ?fir-filter ( obj -- f ) \ os ?fir-filter ." help!
' fir-filter s" fir-filter ( f: input gen -- f: value ) \ 0e os fir-filter f." help!

' make-iir-filter s" make-iir-filter ( order ycoeffs-vct -- gen )" help!
' ?iir-filter s" ?iir-filter ( obj -- f ) \ os ?iir-filter ." help!
' iir-filter s" iir-filter ( f: input gen -- f: value ) \ 0e os iir-filter f." help!
' make-fir-filter s\" make-fir-coeffs ( v -- gen )\n" >$
s\" \\ turns spectral envelope in vct V into coeffs for FIR filter" $+ help!
' xcoeffs@ s" xcoeffs@ ( gen -- xcoeffs-vct )" help!
' ycoeffs@ s" ycoeffs@ ( gen -- ycoeffs-vct )" help!
' xcoeff@ s" xcoeff@ ( gen idx -- f: val )" help!
' xcoeff! s" xcoeff! ( gen idx f: val -- )" help!
' ycoeff@ s" ycoeff@ ( gen idx -- f: val )" help!
' ycoeff! s" ycoeff! ( gen idx f: val -- )" help!

' make-wave-train s\" make-wave-train ( f: freq f: phase vct-wave type -- gen )\n" >$
s\" 440e 0e vct-wave mus-interp-linear make-wave-train value os\n" $+
s\" \\ return a new wave-train generator (an extension of pulse-train).\n" $+
s\" \\ Frequency is the repetition rate of the wave found in wave.\n" $+
s\" \\ Successive waves can overlap." $+ help!
' ?wave-train s" ?wave-train ( obj -- f ) \ os ?wave-train ." help!
' wave-train s" wave-train ( f: fm gen -- f: value ) \ 0e os wave-train f." help!

' make-waveshape s\" make-waveshape ( f: freq vct-parts -- gen )\n" >$
s\" 440e 1e 1e 2 >vct make-waveshape value gen" $+ help!
' ?waveshape s" ?waveshape ( obj -- f ) \ os ?waveshape ." help!
' waveshape s" waveshape ( f: index f: fm gen -- f: value ) \ 1e 0e os waveshape f." help!
' partials>waveshape s\" partials>waveshape ( vct-partials size -- vct-table )\n" >$
s\" 1e 0.5e 2e 0.3e 3e 0.2e 6 >vct 512 partials>waveshape value table\n" $+
s\" \\ produce a waveshaping lookup table (suitable for the waveshape generator)\n" $+
s\" \\ that will produce the harmonic spectrum given by the partials argument" $+ help!
' partials>polynomial s\" partials>polynomial ( vct-partials kind -- vct-table )\n" >$
s\" 1e 0.5e 2e 0.3e 3e 0.2e 6 >vct 1 partials>polynomial value table\n" $+
s\" \\ produce a Chebyshev polynomial suitable for use with the polynomial\n" $+
s\" \\ generator.  To create (via waveshaping) the harmonic spectrum\n" $+
s\" \\ described by the partials argument:\n" $+
s\" 1e 1e 2e 1e 4 >vct partials>polynomial value v0\n" $+
s\" 440e 0e make-oscil value os\n" $+
s\" os 0e 0e oscil v0 polynomial f." $+ help!

' make-env s\" make-env ( envelope-vct f: scaler f: duration -- gen )\n" >$
s\" 0e 1e 1e 1e 4 >vct 1e 0e make-env value en" $+ help!
' make-envelope s\" make-envelope ( envelope-vct f: scaler f: duration f: offset f: base start end -- gen )\n" >$
s\" 0e 1e 1e 1e 4 >vct 1e 0e 0e 1e 0 0 make-envelope value en" $+ help!
' ?env s" ?env ( obj -- f ) \ en ?env ." help!
' env s" env ( gen -- f: value ) \ en env f." help!
' restart-env s" restart-env ( gen -- ) \ en restart-env" help!
' env-interp s" env-interp ( f: x gen -- f: value ) \ 0.7e en env-interp f." help!

' make-frame s" make-frame ( chans -- fr ) \ 2 make-frame value fr" help!
' ?frame s" ?frame ( obj -- f ) \ gen ?frame ." help!
' >frame s" >frame ( chans-values chans -- fr ) \ 0.5e 0.3e 2 >frame value fr" help!
' frame+ s" frame+ ( fr1 fr2 -- fr3 )" help!
' frame* s" frame* ( fr1 fr2 -- fr3 )" help!
' frame@ s" frame@ ( chn fr -- f: value ) \ 0 fr frame@ f." help!
' frame! s" frame! ( f: val chn fr -- ) \ fr 0 0.5e frame!" help!
' frame>vct s" frame>vct ( fr -- vct-vals ) \ fr frame>vct value v" help!
' vct>frame s" vct>frame ( vct-vals -- fr ) \ v vct>frame value fr" help!

' make-mixer s" make-mixer ( chans -- mx ) \ 2 make-mixer value mx" help!
' ?mixer s" ?mixer ( obj -- f ) \ gen ?mixer ." help!
' >mixer s\" >mixer ( chans-in-out-values chans -- mx )\n" >$
s\" 0.5e 0.25e 0.125e 1e 2 >mixer value mx\n" $+
s\" \\ chan 0 in 0.5 out 0.25\n" $+
s\" \\ chan 1 in 0.125 out 1" $+ help!
' make-identity-mixer s" make-identity-mixer ( chans -- mx ) \ 2 make-identity-mixer value mx" help!
' mixer+ s" mixer+ ( mx1 mx2 -- mx3 )" help!
' mixer* s" mixer* ( mx1 mx2 -- mx3 )" help!
' mixer@ s" mixer@ ( in-chn out-chn mx -- f: value ) \ 0 0 mx mixer@ f." help!
' mixer! s" mixer! ( f: val in-chn out-chn mx -- ) \ 0.5e 0 0 mx mixer!" help!
' mixer-scale s" mixer-scale ( f: scl mx -- gen )" help!
' frame>frame s" frame>frame ( fr1 mx -- fr2 )" help!
' sample>frame s" sample>frame ( f: val mx -- fr )" help!
' frame>sample s" frame>sample ( fr mx -- f: value )" help!

' make-file>sample s\" make-file>sample ( addr u -- gen )\n" >$
s\" s\" test.reverb\" make-file>sample value *reverb*" $+ help!
' ?file>sample s" ?file>sample ( obj -- f ) \ gen ?file>sample ." help!
' file>sample s" file>sample ( samp chn gen -- f: value ) \ 10 0 gen file>sample f." help!

' make-readin s\" make-readin ( d: fname chn start dir -- gen )\n" >$
s\" s\" test.snd\" 0 0 1 make-readin value rd" $+ help!
' ?readin s" ?readin ( obj -- f ) \ gen ?readin ." help!
' readin s" readin ( gen -- f: value ) \ rd readin f." help!

' ?output s" ?output ( obj -- f ) \ gen ?output ." help!
' ?input s" ?input ( obj -- f ) \ gen ?input ." help!
' in-any s" in-any ( samp chn gen -- f: value ) \ 10 0 gen in-any f." help!

' make-file>frame s\" make-file>frame ( addr u -- gen )\n" >$
s\" s\" test.reverb\" make-file>frame value *reverb*" $+ help!
' ?file>frame s" ?file>frame ( obj -- f ) \ gen ?file>frame ." help!
' file>frame s" file>frame ( samp gen -- fr ) \ 10 gen file>frame value fr" help!

' make-sample>file s\" make-sample>file ( d: fname chans format type d: comment -- gen )\n" >$
s\" s\" test.snd\" 1 mus-lshort mus-next s\" test file\" make-sample>file value *output*" $+ help!
' ?sample>file s" ?sample>file ( obj -- f ) \ *output* ?sample>file ." help!
' sample>file s" sample>file ( f: value samp chan gen -- ) \ 0.35e 10 0 *output* sample>file" help!
' mus-close s" mus-close ( gen -- ) \ *output* mus-close" help!
' continue-sample>file s\" continue-sample>file ( addr u -- gen )\n" >$
s\" s\" test.snd\" continue-sample>file value *output*" $+ help!
' out-any s" out-any ( f: value samp chn gen -- ) \ 0.5e 10 0 *output* out-any" help!

' make-frame>file s\" make-frame>file ( d: fname chans format type -- gen )\n" >$
s\" s\" test.snd\" 1 mus-lshort mus-next make-frame>file value *output*" $+ help!
' ?frame>file s" ?frame>file ( obj -- f ) \ *output* ?frame>file ." help!
' frame>file s" frame>file ( fr samp gen -- ) \ fr 10 *output* frame>file" help!
' continue-frame>file s\" continue-frame>file ( addr u -- gen )\n" >$
s\" s\" test.snd\" continue-frame>file value *output*" $+ help!

' make-locsig s\" make-locsig ( f: degree f: distance f: reverb chans output revput type -- gen )\n" >$
s\" 0e 1e 0e 1 *output* false 0 make-locsig value loc" $+ help!
' ?locsig s" ?locsig ( obj -- f ) \ loc ?locsig ." help!
' locsig s" locsig ( f: value samp gen -- ) \ 0.35e 10 loc locsig" help!
' locsig@ s" locsig@ ( chn gen -- f: value )" help!
' locsig! s" locsig! ( f: val chn gen -- )" help!
' locsig-reverb@ s" locsig-reverb@ ( chn gen -- f: value )" help!
' locsig-reverb! s" locsig-reverb! ( f: val chn gen -- )" help!
' move-locsig s" move-locsig ( f: degree f: distance gen -- )" help!
' fill-locsig s" fill-locsig ( vct-data chans f: degree f: scaler type -- )" help!

' array>file s\" array>file ( vct samps srate chans d: fname -- )\n" >$
s\" \\ write VCT of interleaved samples to the sound file FNAME set up to\n" $+
s\" \\ have the given SRATE and CHANS.  SAMPS samples are written." $+ help!
' file>array s\" file>array ( d: fname chan start samps -- vct )\n" >$
s\" \\ read the sound file FNAME placing samples from channel CHAN into the\n" $+
s\" \\ vct VCT starting in the file at frame START and reading SAMPS samples\n" $+
s\" \\ altogether." $+ help!

' make-ssb-am s" make-ssb-am ( f: freq order -- gen ) \ 440e 40 make-ssb-am value gen" help!
' ?ssb-am s" ?ssb-am ( obj -- f ) \ gen ?ssb-am ." help!
' ssb-am s" ssb-am ( f: insig f: fm gen -- f: result ) \ 0e 0e gen ssb-am f." help!

' sound-samples s\" sound-samples ( addr u -- u ) \\ s\" test.snd\" sound-samples u." help!
' sound-frames s\" sound-frames ( addr u -- u ) \\ s\" test.snd\" sound-frames u." help!
' sound-chans s\" sound-chans ( addr u -- n ) \\ s\" test.snd\" sound-chans ." help!
' sound-srate s\" sound-srate ( addr u -- n ) \\ s\" test.snd\" sound-srate ." help!
' sound-header-type s\" sound-header-type ( addr u -- n )\n" >$
s\" \\ s\" test.snd\" sound-header-type .header-type" $+ help!
' sound-data-format s\" sound-data-format ( addr u -- n )\n" >$
s\" \\ s\" test.snd\" sound-data-format .data-format" $+ help!
' sound-length s\" sound-length ( addr u -- u ) \\ s\" test.snd\" sound-length u." help!
' .header-type s" .header-type ( n -- ) \ 10 .header-type" help!
' .data-format s" .data-format ( n -- ) \ 10 .data-format" help!
' sound-comment s\" sound-comment ( addr1 u1 -- addr2 u2|f ) \\ s\" test.snd\" sound-comment type cr" help!
' sound-duration s\" sound-duration ( addr u -- f: dur )\n" >$
s\" \\ s\" test.snd\" sound-duration f." $+ help!
' sound-open-input s\" sound-open-input ( addr u -- n )\n" >$
s\" \\ s\" test.snd\" sound-open-input value fd" $+ help!
' sound-open-output s\" sound-open-output ( d: fname srate chans data-format header-type d: comment -- n )" help!
' sound-close-input s" sound-close-input ( fd -- ) \ fd sound-close-input" help!
' sound-close-output s" sound-close-output ( fd bytes -- )" help!
' sound-read s" sound-read ( fd beg end chans sd -- ) \ fd 0 128 1 sd sound-read" help!
' sound-write s" sound-write ( fd beg end chans sd -- )" help!

' audio-open-input s\" audio-open-input ( dev srate chans format size -- fd )\n" >$
s\" mus-audio-default 22050 1 mus-lshort 1024 audio-open-input value fd" $+ help!
' audio-open-output s\" audio-open-output ( dev srate chans format size -- fd )\n" >$
s\" mus-audio-default 22050 1 mus-lshort 1024 audio-open-output value fd" $+ help!
' audio-read s" audio-read ( line sd frames -- ) \ dac-fd sd 128 audio-read" help!
' audio-write s" audio-write ( line sd frames -- ) \ dac-fd sd 128 audio-write" help!
' audio-close s" audio-close ( line -- ) \ dac-fd audio-close" help!

\ gfm.fs ends here
