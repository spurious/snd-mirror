\ fsndlib.fs -- generators -*- forth -*-

\ Copyright (C) 2003--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Jul 21 22:19:43 CEST 2003
\ Last: Thu Jan 13 17:21:18 CET 2005
\ Ident: $Id: fsndlib.fs,v 1.211 2005/01/13 16:23:45 mike Exp $

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
\ All generators have the following functions:
\
\ make-generator ( ... -- gen )
\ generator ( ... gen -- r )
\ .generator ( gen -- )
\ ?generator ( gen -- f )
\ generator= ( g1 g2 -- f )
\
\ e.g.
\ make-oscil ( freq phase -- gen )
\ oscil ( fm pm gen -- r )
\ .oscil ( gen -- )
\ ?oscil ( gen -- f )
\ oscil= ( g1 g2 -- f )
\
\ It exists a general print function .GEN which prints all generators,
\ vcts, arrays, and other objects.
\
\ 440e 0e make-oscil value os
\ os .oscil <=> os .gen
\ 
\ 10 make-vct value v
\ v .vct <=> v .gen
\ 
\ 10 make-array value ary
\ ary .array <=> ary .gen
\ 
\ 1 128 make-sound-data value sd
\ sd .sound-data <=> sd .gen
\
\ Instrument Example:
\
\ : simp { f: start f: dur f: freq f: amp -- }
\     :frequency freq make-oscil { os }
\     start dur times>samples do   amp  0e 0e os oscil  f*  i *output* outa   loop
\     os mus-free
\ ;
\
\ The same result can be achieved by
\
\ instrument: simp { f: start f: dur f: freq f: amp -- }
\     :frequency freq make-oscil { os }
\     start dur run-instrument   amp  0e 0e os oscil  f*   end-run
\     os mus-free
\ ;instrument

\ Code:

require utils.fs
only forth also definitions
vocabulary CLM-Sndlib
also CLM-Sndlib definitions
also Utils
also GFMusic

\ === Vct ===
: free-vct { v -- }
    v ?vct if
	v obj-data @ free throw
	v v-orig @ free throw
	nil v gfm-base-name !
	v free throw
    then
;
: vct= { v1 v2 -- f }
    v1 ?vct v2 ?vct and if
	v1 v2 = if
	    true
	else
	    v1 obj-len @ v2 obj-len @ = if
		true
		v1 obj-data @ v1 obj-len @ sfloats v2 obj-data @ v2 obj-len @ sfloats compare 0= and
	    else
		false
	    then
	then
    else
	false
    then
;
: $(.vct) { v -- str }
    v ?vct if
	$" vct[ " v obj-len @ 0 ?do v obj-data @ i sfloats + sf@ $(f.) $+ $" e " $+ loop $" ]" $+
    else
	$" nil"
    then
;
: .vct { v -- str }
    ." #<" v ?vct if
	*array-print-length* { len }
	v obj-len @ { vlen }
	str-vct .string ." [" vlen . bs ." ]"
	vlen if
	    len vlen min 0 do space v obj-data @ i sfloats + sf@ 3 f.r bs loop
	    vlen len > if ."  ..." then
	then
    else
	." null"
    then ." >"
;
: make-vct { len -- v }
    assert1( len 0>= )
    len *clm-array-buffer-length* / 1+ *clm-array-buffer-length* * { buf-len }
    vct% %alloc { v }
    len                 v obj-len !
    buf-len sfloats allocate throw dup buf-len sfloats erase v obj-data !
    0                  v obj-cycle !
    ['] vct-fetch      v obj-fetch !
    ['] vct-store      v obj-store !
    ['] vct-plus-store v obj-plus-store !
    buf-len            v v-buf-len !
    c-vct% %alloc      v v-orig !
    ['] .vct           v gfm-base-inspect !
    ['] $(.vct)        v gfm-base-to-string !
    ['] vct=           v gfm-base-equal !
    ['] free-vct       v gfm-base-free !
    str-vct            v gfm-base-name !
    v
;
: >vct ( u-float-values u -- v ) { len }
    len make-vct { v }
    len if v obj-data @ 1 sfloats - len sfloats bounds swap u-do i sf! 1 sfloats -loop then
    v
;
: vct-copy { v1 -- v2 }
    v1 obj-len @ { len }
    len make-vct { v2 }
    v1 obj-data @ v2 obj-data @ len sfloats move
    v2
;

\ === Sound-Data ===
: sound-data-free { sd -- }
    sd ?sound-data if
	sd sd-chans @ 0 do sd obj-data @ i cells + @ free throw loop
	sd obj-data @ free throw
	sd sd-orig @ free throw
	nil sd gfm-base-name !
	sd free throw
    then
;
: sound-data= { sd1 sd2 -- f }
    sd1 ?sound-data sd2 ?sound-data and if
	sd1 sd2 = if
	    true
	else
	    sd1 sd-chans @ sd2 sd-chans @ = sd1 obj-len @ sd2 obj-len @ = and if
		true
		sd1 sd-chans @ 0 do
		    sd1 obj-data @ i cells + @ sd1 obj-len @ sfloats
		    sd2 obj-data @ i cells + @ sd2 obj-len @ sfloats compare 0= and
		loop
	    else
		false
	    then
	then
    else
	false
    then
;
: $(.sound-data) { sd -- str }
    $" #<" sd ?sound-data if
	str-sound-data $+ $"  chans: " $+ sd sd-chans @ $(.) $+
	$" , frames: " $+ sd obj-len @ $(.) $+
    else
	$" null" $+
    then $" >" $+
;
' $(.sound-data) make-inspect .sound-data
: make-sound-data { chns frms -- sd }
    assert1( chns 0> frms 0> and )
    sd% %alloc { sd }
    chns cells allocate throw { data }
    chns 0 do frms sfloats allocate throw dup frms sfloats erase data i cells + ! loop
    data                sd obj-data !
    chns                sd sd-chans !
    frms                sd obj-len !
    c-sd% %alloc        sd sd-orig !
    ['] .sound-data     sd gfm-base-inspect !
    ['] $(.sound-data)  sd gfm-base-to-string !
    ['] sound-data=     sd gfm-base-equal !
    ['] sound-data-free sd gfm-base-free !
    str-sound-data      sd gfm-base-name !
    sd
;

require gfm-defs.fs

\ === Generall Music-V Struct ===
gfm-base%
    cell%   field location
    cell%   field vct-buffer
    cell%   field buffer-length
    cell%   field cosines
    cell%   field interpolation-type
    cell%   field get-freq
    cell%   field set-freq
    cell%   field get-scaler
    cell%   field set-scaler
    cell%   field get-increment
    cell%   field set-increment
    cell%   field set-location
    cell%   field gfm-generator
    sfloat% field scaler
    sfloat% field current-value
    sfloat% field fill-time
end-struct music5%

0 value *io-buffer-size*

: srate@            ( -- n ) *srate* ;
: srate!            ( val -- ) to *srate* ;
: mus-srate@        ( -- r ) *srate* s>f ;
: mus-srate!        ( val -- ) f>s to *srate* ;
: seconds>samples   ( val -- u ) *srate* s>f f* f>s ;
: samples>seconds   ( val -- r ) s>f *srate* s>f f/ ;
: file-buffer-size@ ( -- n ) *io-buffer-size* ;
: file-buffer-size! ( val -- ) to *io-buffer-size* ;
' gen-free alias mus-free
' obj-run alias mus-run
: array-print-length@ ( -- n ) *array-print-length* ;
: array-print-length! ( val -- ) to *array-print-length* ;

\ === Music5 ===
: radians>hz         ( val -- r ) mus-srate@ f* two-pi f/ ;
: hz>radians         ( val -- r ) two-pi f* mus-srate@ f/ ;
: degrees>radians    ( val -- r ) 360e f/ two-pi f* ;
: radians>degrees    ( val -- r ) 360e two-pi f/ f* ;
: db>linear          ( val -- r ) 20e f/ falog ;
: linear>db          ( val -- r ) 0.00001e fmax flog 20e f* ;
: ring-modulate      ( in1 in2 -- r ) f* ;
: amplitude-modulate ( car in1 in2 -- r ) frot f+ f* ;

: dot-product { in1 in2 -- r } 0e in1 length 0 ?do i in1 vct@ i in2 vct@  f*  f+ loop ;
: sine-bank { amps phases -- r }
    0e amps length 0 ?do i amps vct@ i phases vct@ fsin  f*  f+ loop
;
' vct* alias multiply-arrays
' vct-clear alias clear-array
: sqr ( val -- r*r ) fdup f* ;
\ r = sqrt(x^2 + y^2)
\ tan(phi) = y/x
\ rdat[i] = rdat[i] * rdat[i] + idat[i] * idat[i]
\ idat[i] = -atan2(idat[i], rdat[i])
: rectangular>polar { rdat idat size -- }
    size 0 do
	i idat vct@ fdup fdup f* i rdat vct@ fdup fdup f* frot f+ fsqrt i rdat vct!
	fatan2 fnegate i idat vct!
    loop
;
\ x = r * cos(phi)
\ y = r * sin(phi)
\ rdat[i] = rdat[i] * fcos(-idat[i])
\ idat[i] = rdat[i] * fsin(-idat[i])
: polar>rectangular { rdat idat size -- }
    size 0 do
	i rdat vct@ i idat vct@ fnegate fover fover fsin f* i idat vct! fcos f* i rdat vct!
    loop
;
\ val 1e contrast-enhancement fvalue res
: contrast-enhancement { f: val f: fm -- r } val half-pi f* val two-pi f* fsin fm f* f+ fsin ;

\ rl im rl length 1 fft
: fft { xdata ydata n isign -- }
    0 0 { mmax jj }
    n s>f 2e flogn fround f>s { ipow }
    0 0 { pow prev }
    0e 0e 0e 0e 0e 0e { f: wtmp f: wr f: wpr f: wpi f: wi f: theta }
    0e 0e 0e 0e { f: tmpr f: tmpi f: wrs f: wis }
    n 0 do
	jj i > if
	    jj xdata vct@ to tmpr
	    jj ydata vct@ to tmpi
	    i xdata vct@ jj xdata vct!
	    i ydata vct@ jj ydata vct!
	    tmpr i xdata vct!
	    tmpi i ydata vct!
	then
	n 2/ { m }
	begin m 2 < jj m < or 0= while jj m - to jj m 2/ to m repeat
	jj m + to jj
    loop
    1 to prev 2 to mmax n 2/ to pow
    two-pi isign s>f f* 0.5e f* to theta
    ipow 0 do
	theta fcos to wpr
	theta fsin to wpi
	1e to wr
	0e to wi
	prev 0 do
	    wr to wrs wi to wis
	    i { idx } i prev + { jdx }
	    pow 0 do
		wrs jdx xdata vct@ f* wis jdx ydata vct@ f* f- to tmpr
		wrs jdx ydata vct@ f* wis jdx xdata vct@ f* f+ to tmpi
		idx xdata vct@ tmpr f- jdx xdata vct!
		idx ydata vct@ tmpi f- jdx ydata vct!
		idx xdata vct@ tmpr f+ idx xdata vct!
		idx ydata vct@ tmpi f+ idx ydata vct!
		idx mmax + to idx
		jdx mmax + to jdx
	    loop
	    wr to wtmp
	    wr wpr f* wi wpi f* f- to wr
	    wi wpr f* wtmp wpi f* f+ to wi
	loop
	pow 2/ to pow mmax to prev
	theta f2/ to theta mmax 2* to mmax
    loop
;
: bess-i0 { f: x -- r }
    x fabs 3.75e f< if
	x 3.75e f/ 2e f** { f: y }
	1e y 3.5156229e y 3.0899424e y 1.2067492e y 0.2659732e y 0.360768e-1 y 0.45813e-2
	f* f+ f* f+ f* f+ f* f+ f* f+ f* f+
    else
	x fabs { f: ax }
	3.75e ax f/ { f: y }
	ax fexp ax fsqrt f/ 0.39894228e y 0.1328592e-1 y 0.225319e-2 y -0.157565e-2 y 0.916281e-2
	y -0.2057706e-1 y 0.2635537e-1 y -0.1647633e-1 y 0.392377e-2
	f* f+ f* f+ f* f+ f* f+ f* f+ f* f+ f* f+ f* f+ f*
    then
;
\ clm-3/mus.lisp
: dolph-chebyshev { window f: gamma -- }
    window length dup s>f { size f: fsize }
    10e gamma f** facosh fsize f/ fcosh { f: alpha }
    alpha facosh fsize f* { f: den }
    pi fsize f/ { f: freq }
    size make-vct { rl }
    size make-vct { im }
    0e { f: phase }
    rl length 0 ?do
	phase fcos alpha f* facos fsize f* fcos den f* i rl vct!
	phase freq f+ to phase
    loop
    rl im size -1 fft
    rl vct-peak { f: pk }
    pk f0> pk 1e f<> and if pk 1/f rl vct-scale! then
    size 2/ { idx }
    window length 0 ?do
	idx rl vct@ i window vct!
	idx 1+ dup size = if drop 0 then to idx
    loop
    rl free-vct
    im free-vct
;
\ type size 0e make-fft-window value v
: make-fft-window { tp size f: beta -- w }
    size make-vct { window }
    size 2/ { midn }
    size 1+ 2/ { midp1 }
    two-pi size s>f f/ { f: freq }
    midn s>f 1/f { f: rate }
    freq { f: sr }
    0e { f: angle }
    1e 2e fln midn s>f f/ f+ { f: expn }
    1e { f: expsum }
    0e { f: val }
    tp case
	mus-rectangular-window of size 0 do 1e i window vct! loop endof
	mus-bartlett-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		angle i window vct! angle jj window vct!
		angle rate f+ to angle
		jj 1- to jj
	    loop
	endof
	mus-parzen-window of
	    size 1- { jj }
	    midn 1+ 0 do
		1e i midn - s>f midp1 s>f f/ fabs f- to val
		val i window vct! val jj window vct!
		jj 1- to jj
	    loop
	endof
	mus-welch-window of
	    size 1- { jj }
	    midn 1+ 0 do
		1e i midn - s>f midp1 s>f f/ fsqrt f- to val
		val i window vct! val jj window vct!
		jj 1- to jj
	    loop
	endof
	mus-exponential-window of
	    size 1- { jj }
	    midn 1+ 0 do
		expsum 1e f- to val
		val i window vct! val jj window vct!
		jj 1- to jj
		expsum expn f* to expsum
	    loop
	endof
	mus-kaiser-window of
	    beta bess-i0 { f: IObeta }
	    1e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		beta 1e angle sqr f- fsqrt f* bess-i0 IObeta f/ to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle rate f- to angle
	    loop
	endof
	mus-gaussian-window of
	    1e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		-0.5e beta angle f* sqr f* fexp to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle rate f- to angle
	    loop
	endof
	mus-poisson-window of
	    1e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		beta fnegate angle f* fexp to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle rate f- to angle
	    loop
	endof
	mus-riemann-window of
	    size 1- { jj }
	    midn 1+ 0 do
		midn 1 = if
		    1e
		else
		    sr midn i - s>f f* fsin sr midn i - s>f f* f/
		then to val
		val i window vct! val jj window vct!
		jj 1- to jj
	    loop
	endof
	mus-cauchy-window of
	    1e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		1e beta angle f* sqr f+ 1/f to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle rate f- to angle
	    loop
	endof
	mus-tukey-window of
	    midn s>f 1e beta f- f* { f: cx }
	    size 1- { jj }
	    midn 1+ 0 do
		i s>f cx f>= if
		    1e
		else
		    0.5e 1e pi i s>f f* cx f/ fcos f- f*
		then to val
		val i window vct! val jj window vct!
		jj 1- to jj
	    loop
	endof
	mus-hamming-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		0.54e 0.46e angle fcos f* f- to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-hann-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		0.5e 0.5e angle fcos f* f- to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-blackman2-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		angle fcos { f: cx }
		0.34401e cx -0.49755e cx 0.15844e f* f+ f* f+ to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-blackman3-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		angle fcos { f: cx }
		0.21747e cx -0.45325e cx 0.28256e cx 0.04672e f* f- f* f+ f* f+ to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-blackman4-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		angle fcos { f: cx }
		0.08403e cx -0.29145e cx 0.37569e cx -0.20762e cx 0.041194e
		f* f+ f* f+ f* f+ f* f+ to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-dolph-chebyshev-window of
	    window beta dolph-chebyshev
	endof
	mus-hann-poisson-window of
	    0e to angle
	    size 1- { jj }
	    midn 1+ 0 do
		angle fcos { f: cx }
		0.5e cx f2/ f-  beta fnegate angle f* fexp  f*  to val
		val i window vct! val jj window vct!
		jj 1- to jj
		angle freq f+ to angle
	    loop
	endof
	mus-connes-window of
	    size 1- { jj }
	    midn 1+ 0 do
		1e i midn - s>f midp1 s>f f/ fsqrt f- fsqrt to val
		val i window vct! val jj window vct!
		jj 1- to jj
	    loop
	endof
    endcase
    window
;
\ rdat idat window 1 spectrum
: spectrum { rdat idat window tp -- }
    rdat length { len }
    len 1- len and if 2e len s>f 2e flogn fround f** f>s to len then
    window if rdat window multiply-arrays then
    idat vct-clear
    rdat idat len 1 fft
    len 2/ to len
    0e { f: maxa }
    0.000001e { f: lowest }
    rdat length 0 ?do
	i rdat vct@ sqr i idat vct@ sqr f+ { f: val }
	val lowest f< if 0.001e else val fsqrt then fdup i rdat vct!
	fabs maxa fmax to maxa
    loop
    maxa f0> if
	maxa 1/f to maxa
	tp unless
	    20e 10e fln f/ { f: to-dB }
	    rdat length 0 ?do i rdat vct@ maxa f* fln to-dB f* i rdat vct! loop
	else
	    tp 1 = if maxa rdat vct-scale! then
	then
    then
;

\ --- polynomial, partials>wave(shape) ---
: normalize-array { ary -- ary' }
    ary vct-peak { f: maxval }
    maxval f0<> maxval 1e f<> and if ary length 0 ?do i ary vct@ maxval f/ i ary vct! loop then
    ary
;
: signify { harm-amps -- harm-amps' }
    harm-amps length { len } 2 { idx } 1 { di }
    begin idx len < while
	    idx harm-amps vct@ fnegate idx harm-amps vct!
	    idx di + to idx 4 di - to di
    repeat
    harm-amps
;
: vct>partials ( v -- parts ) normalize-partials array>partials signify ;
: partials>waveshape { v size -- wave }
    v vct>partials { parts }
    size 0<= if *table-size* to size then
    size make-vct { wave }
    2e size 1- s>f f/ { f: max-i-2 }
    -1e { f: x }
    size 0 do
	0e { f: sum }
	1e { f: tn }
	x { f: tn-1 }
	parts length 0 ?do
	    i parts vct@ tn f* sum f+ to sum
	    tn-1
	    tn-1 f2* x f* tn f- to tn-1
	    to tn			\ tn-1 from fstack
	loop
	sum i wave vct!
	x max-i-2 f+ to x
    loop
    wave normalize-array
;
: partial-amp { f: n parts -- r }
    fdepth
    parts length 0 ?do i parts vct@ n f= if i 1+ parts vct@ leave then 2 +loop
    fdepth = if 0e then

;
\ parts 1 partials>polynomial value v
: partials>polynomial { parts kind -- w }
    0e parts length 0 ?do i parts vct@ fmax 2 +loop f>s { top }
    top 1+ { size }
    size make-vct { t0 }
    size make-vct { t1 }
    size make-vct { tn }
    size make-vct { cc1 }
    0e { f: amp }
    kind s>f 0 t0 vct! 1e 1 t1 vct!
    top 1+ 1 do
	i s>f parts partial-amp to amp
	amp f0<> if
	    kind if
		i 1+ 0 do i cc1 vct@ amp i t1 vct@ f* f+ i cc1 vct! loop
	    else
		i 1+ 1 do i 1- cc1 vct@ amp i t1 vct@ f* f+ i 1- cc1 vct! loop
	    then
	then
	i top <> if
	    0 i 1+ -do i 1- t1 vct@ f2* i t0 vct@ f- i tn vct! 1 -loop
	    0 t0 vct@ fnegate 0 tn vct!
	    -1 i 1+ -do
		i t1 vct@ i t0 vct!
		i tn vct@ i t1 vct!
	    1 -loop
	then
    loop
    cc1 vct-copy			\ result
    t0 free-vct
    t1 free-vct
    tn free-vct
    cc1 free-vct
;

: load-one-sine-wave { f: partial f: part-amp table f: phase -- }
    part-amp f0<> if
	partial two-pi table length s>f f/ f* { f: frq }
	phase
	table length 0 do fdup fsin part-amp f* i table vct+! frq f+ loop
	fdrop
    then
;
: partials>wave { data norm -- w }
    *table-size* make-vct { table }
    data length 0 ?do
	i data vct@ i 1+ data vct@  table  0e  load-one-sine-wave
    2 +loop
    norm if table normalize-array else table then
;
: phase-partials>wave { data norm -- w }
    *table-size* make-vct { table }
    data length 0 ?do
	i data vct@ i 1+ data vct@  table  i 2 + data vct@  load-one-sine-wave
    3 +loop
    norm if table normalize-array else table then
;

: polynomial { coeffs f: x -- sum }
    coeffs ?empty if
	x
    else
	-1 coeffs vct@ ( sum )
	-1 coeffs length 2 - -do x f* i coeffs vct@ f+ 1 -loop
    then
;
: (corr-x) { f: x size -- f: x' }
    size s>f { f: fsize }
    x f0< x fsize f> or if
	x fsize fmod fdup f0< if fsize f+ then
    else
	x
    then
;
: array-interp { f: x wave -- r }
    wave length { size }
    x size (corr-x) to x
    x truncate dup size = if drop 0 then { int-part f: frac-part }
    frac-part f0= if
	int-part wave vct@
    else
	int-part 1+ dup size >= if drop 0 then { inx }
	int-part wave vct@
	inx wave vct@ int-part wave vct@ f- frac-part f* f+
    then
;
\ snd-7/clm.c
: array-all-pass-interp { f: x f: val wave -- r }
    wave length { size }
    x size (corr-x) to x
    x truncate dup size = if drop 0 then { int-part f: fract-part }
    int-part 1+ dup size >= if size - then { inx }
    fract-part f0= if
	int-part wave vct@  inx wave vct@  f+  val  f-
    else
	int-part wave vct@
	1e fract-part f-  1e fract-part f+  f/
	inx wave vct@ val f-  f*  f+
    then
;
\ snd-7/clm.c
: array-lagrange-interp { f: x wave -- r }
    wave length { size }
    x size (corr-x) to x
    x floor f>s { x0 }
    x x0 s>f f- { f: p }
    x0 size >= if x0 size - to x0 then
    p f0= if
	x0 wave vct@
    else
	x0 1+ dup size >= if size - then { xp1 }
	x0 1- dup 0< if drop size 1- then { xm1 }
	p p f* { f: pp }
	xm1 wave vct@ f2/ pp p f- f*
	x0 wave vct@     1e pp f- f*  f+
	xp1 wave vct@ f2/ p pp f+ f*  f+
    then
;
\ snd-7/clm.c
: array-hermite-interp { f: x wave -- r }
    wave length { size }
    x size (corr-x) to x
    x floor f>s { x1 }
    x x1 s>f f- { f: p }
    x1 size = if 0 to x1 then
    p f0= if
	x1 wave vct@
    else
	x1 1+ dup size = if drop 0 then { x2 }
	x2 1+ dup size = if drop 0 then { x3 }
	x1 1- dup 0< if drop size 1- then { x0 }
	x0 wave vct@ { f: y0 }
	x1 wave vct@ { f: y1 }
	x2 wave vct@ { f: y2 }
	x3 wave vct@ { f: y3 }
	y1 { f: c0 }
	y2 y0 f- f2/ { f: c1 }
	1.5e y1 y2 f- f*  y3 y0 f- f2/ f+ { f: c3 }
	y0 y1 f- c1 c3 f- f+ { f: c2 }
	c3 p f* c2 f+  p f* c1 f+  p f* c0 f+
    then
;

\ === Generator Definitions ===
$" oscil"          constant str-oscil
$" env"            constant str-env
$" table-lookup"   constant str-table-lookup
$" waveshape"      constant str-waveshape
$" triangle-wave"  constant str-triangle-wave
$" square-wave"    constant str-square-wave
$" sawtooth-wave"  constant str-sawtooth-wave
$" pulse-train"    constant str-pulse-train
$" sum-of-cosines" constant str-sum-of-cosines
$" sum-of-sines"   constant str-sum-of-sines
$" sine-summation" constant str-sine-summation
$" asymmetric-fm"  constant str-asymmetric-fm
$" rand"           constant str-rand
$" rand-interp"    constant str-rand-interp
$" one-pole"       constant str-one-pole
$" one-zero"       constant str-one-zero
$" two-pole"       constant str-two-pole
$" two-zero"       constant str-two-zero
$" formant"        constant str-formant
$" filter"         constant str-filter
$" fir-filter"     constant str-fir-filter
$" iir-filter"     constant str-iir-filter
$" delay"          constant str-delay
$" comb"           constant str-comb
$" notch"          constant str-notch
$" all-pass"       constant str-all-pass
$" average"        constant str-average
$" wave-train"     constant str-wave-train
$" ssb-am"         constant str-ssb-am
$" locsig"         constant str-locsig
$" sample>file"    constant str-sample>file
$" readin"         constant str-readin
$" file>sample"    constant str-file>sample
$" frame"          constant str-frame
$" mixer"          constant str-mixer
$" file>frame"     constant str-file>frame
$" frame>file"     constant str-frame>file
$" src"            constant str-src
$" convolve"       constant str-convolve
$" granulate"      constant str-granulate
$" phase-vocoder"  constant str-phase-vocoder
$" fcomb"          constant str-fcomb
$" buffer"         constant str-buffer

$" mus-gen"        constant str-mus-gen
$" io-gen"         constant str-io-gen
$" io-input"       constant str-io-input
$" io-output"      constant str-io-output

: ?gen ( obj -- f ) try gfm-generator @ str-mus-gen = recover 2drop false endtry ;
: generator-free { gen -- }
    gen ?gen if
	nil gen gfm-base-name !
	nil gen gfm-generator !
	gen free throw
    then
;
: vct-buffer-free { gen -- }
    gen ?gen if
	gen vct-buffer @ free-vct
	gen generator-free
    then
;

\ === Oscil ===
\ Usage: make-oscil value os
\        0e 0e os oscil 3 f.r  --> 0.000
\        os frequency@ 3 f.r   --> 440.000
\        330e os frequency!
\        os phase@ 3 f.r       --> 0.125
\        0.5e os phase!
\        os .oscil             --> #<oscil: frequency: 0.094 (330 Hz), phase: 0.500 (0 degrees)>
music5%
    cell%   field get-phase
    cell%   field set-phase
    sfloat% field freq
    sfloat% field phase
end-struct oscil%

: name@          ( gen -- addr len ) gfm-base-name @ ;
: gfm-frequency@ ( gen -- r ) freq sf@ radians>hz ;
: gfm-frequency! ( val gen -- ) hz>radians freq sf! ;
: gfm-phase@     ( gen -- r ) phase sf@ two-pi fmod ;
: gfm-phase!     ( val gen -- ) phase sf! ;
: frequency@     ( gen -- r ) dup get-freq @ execute ;
: frequency!     ( val gen -- ) dup set-freq @ execute ;
: phase@         ( gen -- r ) dup get-phase @ execute ;
: phase!         ( ph gen -- ) dup set-phase @ execute ;

: $(.pp-print) { gen -- str }
    $"  freq: " gen gfm-frequency@ 3 $(f.r) $+
    $" Hz, phase: " $+ gen gfm-phase@ 3 $(f.r) $+
;
: oscil-equal { g1 g2 -- f }
    g1 g2 =
    g1 freq sf@ g2 freq sf@ f=
    g1 phase sf@ g2 phase sf@ f= and or
;

str-oscil make-?obj ?oscil
: oscil= { g1 g2 -- f } g1 ?oscil g2 ?oscil and if g1 g2 oscil-equal else false then ;
: $(.oscil) { gen -- str } $" #<" str-oscil $+ gen $(.pp-print) $+ $" >" $+ ;
' $(.oscil) make-inspect .oscil
: oscil { f: fm f: pm gen -- r }
    gen phase sf@ pm f+ fsin		\ result
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ fdup 1000e f> fdup -1000e f< or if two-pi fmod gen phase sf! else fdrop then
;
: oscil-0 { gen -- r }
    gen phase sf@ fsin
    gen freq sf@ gen phase sf+!
    gen phase sf@ fdup 1000e f> fdup -1000e f< or if two-pi fmod gen phase sf! else fdrop then
;
: oscil-1 { f: fm gen -- r }
    gen phase sf@ fsin
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ fdup 1000e f> fdup -1000e f< or if two-pi fmod gen phase sf! else fdrop then
;

: clm-make-oscil { f: frq f: ph -- gen }
    oscil% %alloc { gen }
    frq hz>radians     gen freq sf!
    ph                 gen phase sf!
    1                  gen cosines !
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@     gen get-phase !
    ['] gfm-phase!     gen set-phase !
    ['] oscil=         gen gfm-base-equal !
    ['] .oscil         gen gfm-base-inspect !
    ['] $(.oscil)      gen gfm-base-to-string !
    ['] oscil          gen gfm-base-run !
    ['] generator-free gen gfm-base-free !
    str-oscil          gen gfm-base-name !
    str-mus-gen        gen gfm-generator !
    gen
;

\ === Env ===
\ Usage: vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] 1e 1e 0e 1e 0 0 clm-make-env value en
\        en env 3 f.r --> 0.000
\        en .gen      --> #<env: rate: 0.000, pass: 16539, value: 0.000,
\                            scaler: 1.000, offset: 0.000, length: 22050,
\                            data: #<vct[8]: [0.000 0.000 25.000 1.000 75.000 1.000 100.000 0.000]>>
music5%
    cell%   field env-location
    cell%   field env-type
    cell%   field env-buffer
    sfloat% field orig-scaler
    sfloat% field orig-offset
    sfloat% field orig-pass
    sfloat% field orig-rate
    sfloat% field pass
    sfloat% field parts
    sfloat% field rate
    sfloat% field power
    sfloat% field env-offset
end-struct env%

0 constant env-seg
1 constant env-exp
2 constant env-step

: data@          ( idx gen -- r ) vct-buffer @ vct@ ;
: data!          ( val idx gen -- ) vct-buffer @ vct! ;
: data>vct       ( gen -- v ) vct-buffer @ ;
: vct>data       ( v gen -- ) vct-buffer ! ;
: length@        ( gen -- n ) buffer-length @ ;
: length!        ( val gen - ) buffer-length ! ;
: offset@        ( gen -- r ) env-offset sf@ ;
: offset!        ( val gen -- ) env-offset sf! ;
: scaler@        ( gen -- r ) dup get-scaler @ execute ;
: scaler!        ( val gen -- ) dup set-scaler @ execute ;
: gfm-scaler@    ( gen -- r ) scaler sf@ ;
: gfm-scaler!    ( val gen -- ) scaler sf! ;
: gfm-increment@ ( gen -- r ) fill-time sf@ ;
: gfm-increment! ( val gen -- ) fill-time sf! ;
: increment@     ( gen -- r ) dup get-increment @ execute ;
: increment!     ( val gen -- ) dup set-increment @ execute ;
: position@      ( gen -- n ) dup buffer-length @ swap pass sf@ f>s - ;
: location@      ( gen -- n ) location @ ;
: location!      ( val gen -- ) dup set-location @ execute ;
: gfm-location!  ( val gen -- ) location ! ;

: fixup-exp { gen data -- exp-data }
    gen scaler sf@ gen env-offset sf@ data scale-envelope { exp-data }
    exp-data min-envelope { f: min-y }
    exp-data max-envelope { f: max-y }
    gen fill-time sf@ 1e f- { f: b-1 }
    min-y max-y f<> { not-flat }
    not-flat if max-y min-y f- 1/f else 1e then { f: val }
      exp-data length 1 u+do
	not-flat if
	    i exp-data vct@ min-y f- val f*
	else
	    val
	then b-1 f* flnp1 i exp-data vct!
    2 +loop
    max-y min-y f- b-1 f/ gen scaler sf!
    min-y gen env-offset sf!
    exp-data
;
: passes { gen -- r }
    gen env-location @ 2 + gen env-buffer @ vct@ ( x1 )
    gen env-location @     gen env-buffer @ vct@ ( x0 )
    f- gen parts sf@ f* 0.5e f+ floor 1e fmax fdup gen pass sf!
;
: rates-seg { gen -- }
    gen env-location @ 3 + gen env-buffer @ length < if
	gen env-location @ 1+  gen env-buffer @ vct@ { f: y0 }
	gen env-location @ 3 + gen env-buffer @ vct@ { f: y1 }
	gen passes { f: pas }
	y0 y1 f= pas f0= or if 0e else y1 y0 f- pas f/ gen scaler sf@ f* then
    else
	0e
    then gen rate sf!
;
: rates-exp { gen -- }
    gen env-location @ 3 + gen env-buffer @ length < if
	gen env-location @ 1+  gen env-buffer @ vct@ { f: y0 }
	gen env-location @ 3 + gen env-buffer @ vct@ { f: y1 }
	gen passes { f: pas }
	y0 y1 f= pas f0= or if 0e else y1 y0 f- pas f/ then
    else
	0e
    then gen rate sf!
;
: rates-step { gen -- }
    gen env-location @ 3 + gen env-buffer @ length < if
	gen passes fdrop
	gen env-location @ 1+ gen env-buffer @ vct@ gen scaler sf@ f* gen env-offset sf@ f+
	gen rate sf!
    then
;

str-env make-?obj ?env
: ?env-linear { gen -- f }
    gen ?env if
	gen env-type @ env-seg =
    else
	false
    then
;
: env-free { gen -- }
    gen ?env if
	gen env-buffer @ free-vct
	gen vct-buffer-free
    then
;
: env= { g1 g2 -- f }
    g1 ?env g2 ?env and if
	g1 g2 =
	g1 buffer-length @ g2 buffer-length @ =
	g1 env-buffer @ g2 env-buffer @ vct= and
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 env-offset sf@ g2 env-offset sf@ f= and
	g1 fill-time sf@ g2 fill-time sf@ f= and
	g1 location @ g2 location @ = and
	g1 env-location @ g2 env-location @ = and
	g1 pass sf@ g2 pass sf@ f= and
	g1 rate sf@ g2 rate sf@ f= and or
    else
	false
    then
;
: restart-env { gen -- }
    0 gen location !
    0 gen env-location !
    1 gen vct-buffer @ vct@ gen orig-scaler sf@ f* gen orig-offset sf@ f+ gen current-value sf!
    1 gen env-buffer @ vct@ gen power sf!
    gen orig-pass sf@ gen pass sf!
    gen orig-rate sf@ gen rate sf!
;
: $(.env) { gen -- str }
    $" #<" str-env $+
    gen env-type @ case
	env-seg  of $"  linear"      endof
	env-exp  of $"  exponential" endof
	env-step of $"  step"        endof
	$" unknown" swap
    endcase $+
    $" , pass: " $+ gen pass sf@ f>s $(.) $+
    $"  (dur: " $+ gen length@ 1+ $(.) $+
    $" ), index: " $+ gen location@ $(.) $+
    $" , scaler: " $+ gen orig-scaler sf@ 3 $(f.r) $+
    $" , offset: " $+ gen orig-offset sf@ 3 $(f.r) $+
    $" , data: " $+ gen data>vct $(.vct) $+
    $" >" $+
;
' $(.env) make-inspect .env
: env { gen -- r }
    gen location @ gen buffer-length @ 1+ >= if
	gen env-type @ env-step = if gen rate sf@ else 0e then
    else
	gen current-value sf@
    then
    1 gen location +!
    -1e gen pass sf+!
    gen env-type @ case
	env-seg  of
	    gen pass sf@ f0< if 2 gen env-location +! gen rates-seg then
	    gen rate sf@ gen current-value sf+!
	endof
	env-exp  of
	    gen pass sf@ f0< if 2 gen env-location +! gen rates-exp then
	    gen rate sf@ f0<> if
		gen rate sf@ gen power sf+!
		gen power sf@ fexp gen scaler sf@ f* gen env-offset sf@ f+ gen current-value sf!
	    then
	endof
	env-step of
	    gen pass sf@ f0< if 2 gen env-location +! gen rates-step then
	    gen rate sf@ gen current-value sf!
	endof
    endcase
;
: env-linear { gen -- r }
    gen current-value sf@
    1 gen location +!
    -1e gen pass sf+!
    gen pass sf@ f0< if 2 gen env-location +! gen rates-seg then
    gen rate sf@ gen current-value sf+!
;
: env-location! ( val gen -- ) dup restart-env swap 0 ?do dup env fdrop loop drop ;
: env-interp { f: x gen -- r }
    x gen buffer-length @ 1+ s>f f*
    gen vct-buffer @ length 2 - gen vct-buffer @ vct@ f/ f>s gen env-location!
    gen current-value sf@
;
: env-run ( r1 r2 gen -- r3 ) fdrop fdrop env ;
: clm-make-env { data f: scale f: dur f: offset f: base start end -- gen }
    assert1( data ?envelope )
    env% %alloc { gen }
    0                              gen location !
    0                              gen env-location !
    scale                          gen scaler sf!
    scale                          gen orig-scaler sf!
    offset                         gen env-offset sf!
    offset                         gen orig-offset sf!
    1 data vct@ scale f* offset f+ gen current-value sf!
    dur f0<> if
	dur seconds>samples 1-
    else
	end start -
    then                            gen buffer-length !
    0 data vct@ { f: x0 }
    data envelope-last-x { f: xlast }
    x0 xlast f<> if gen buffer-length @ s>f xlast x0 f- f/ else 1e then gen parts sf!
    \ 0e base: step
    \ 1e base: seg
    \    else: exp
    base gen fill-time sf!
    base f0= if
	env-step
    else
	base 1e f= if
	    env-seg
	else
	    env-exp
	then
    then dup                        gen env-type !
    case
	env-seg of
	    0e                      gen power sf!
	    data vct-copy           gen env-buffer !	
	    gen rates-seg
	endof
	env-exp of
	    gen data fixup-exp { exp-data }
	    exp-data                gen env-buffer !
	    1 exp-data vct@         gen power sf!
	    gen scaler sf@ fnegate  gen env-offset sf+!
	    gen rates-exp
	endof
	env-step of
	    0e                      gen power sf!
	    0e                      gen pass sf!
	    data vct-copy           gen env-buffer !
	    gen rates-step
	endof
    endcase
    gen pass sf@                    gen orig-pass sf!
    gen rate sf@                    gen orig-rate sf!
    data vct-copy                   gen vct-buffer !
    \ make-env (gfm.fs) calls clm-make-env with copied data
    data free-vct
    ['] gfm-scaler@                 gen get-scaler !
    ['] gfm-scaler!                 gen set-scaler !
    ['] gfm-increment@              gen get-increment !
    ['] gfm-increment!              gen set-increment !
    ['] env-location!               gen set-location !
    ['] env=                        gen gfm-base-equal !
    ['] .env                        gen gfm-base-inspect !
    ['] $(.env)                     gen gfm-base-to-string !
    ['] env-run                     gen gfm-base-run !
    ['] env-free                    gen gfm-base-free !
    str-env                         gen gfm-base-name !
    str-mus-gen                     gen gfm-generator !
    gen
;

\ === Table-Lookup ===
oscil%
end-struct table-lookup%

: interp-type@ ( gen -- n ) interpolation-type @ ;

: tb-frequency@ ( gen -- r ) dup freq sf@ mus-srate@ f* buffer-length @ s>f f/ ;
: tb-frequency! ( val gen -- ) dup buffer-length @ s>f f* mus-srate@ f/ freq sf! ;
: tb-phase@     ( gen -- r ) dup phase sf@ two-pi f* buffer-length @ s>f f/ two-pi fmod ;
: tb-phase!     ( val gen -- ) dup buffer-length @ s>f f* two-pi f/ phase sf! ;
: tb-lookup-interp { f: x wave tp -- r }
    wave length { size }
    tp case
	mus-interp-none     of x 0.001e f+ f>s size mod dup 0< if size + then wave vct@ endof
	mus-interp-lagrange of x wave array-lagrange-interp endof
	mus-interp-hermite  of x wave array-hermite-interp  endof
	mus-interp-linear   of x wave array-interp          endof
    endcase
;

: $(.tb-pp-print) { gen -- str }
    $"  freq: " gen tb-frequency@ 3 $(f.r) $+
    $" Hz, phase: " $+ gen tb-phase@ 3 $(f.r) $+
;

str-table-lookup make-?obj ?table-lookup
: table-lookup= { g1 g2 -- f }
    g1 ?table-lookup g2 ?table-lookup and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 interpolation-type @ g2 interpolation-type @ = and
	g1 buffer-length @ g2 buffer-length @ = and
	g1 vct-buffer @ g2 vct-buffer @ vct= and
    else
	false
    then
;
: $(.table-lookup) { gen -- str }
    $" #<" str-table-lookup $+
    gen $(.tb-pp-print) $+
    $" , size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+
    $" >" $+
;
' $(.table-lookup) make-inspect .table-lookup
: table-lookup { f: fm gen -- r }
    gen phase sf@ gen vct-buffer @ gen interpolation-type @ tb-lookup-interp \ result
    gen freq sf@ fm gen scaler sf@ f* f+ gen phase sf+!
    gen buffer-length @ s>f { f: flen }
    gen phase sf@ fdup flen f>= fdup f0< or if
	flen fmod fdup f0< if flen f+ then gen phase sf!
    else
	fdrop
    then
;
: table-lookup-1 { gen -- r }
    gen phase sf@ gen vct-buffer @ gen interpolation-type @ tb-lookup-interp \ result
    gen freq sf@ gen phase sf+!
    gen buffer-length @ s>f { f: flen }
    gen phase sf@ fdup flen f>= fdup f0< or if
	flen fmod fdup f0< if flen f+ then gen phase sf!
    else
	fdrop
    then
;
: table-lookup-run ( r1 r2 gen -- r3 ) fdrop table-lookup ;
\ 440e  0e  *table-size* make-vct  mus-interp-linear  clm-make-table-lookup value gen
: clm-make-table-lookup { f: frq f: ph wave tp -- gen )
    wave length dup s>f { len f: flen }
    table-lookup% %alloc { gen }
    wave                      gen vct-buffer !
    len                       gen buffer-length !
    frq flen f* mus-srate@ f/ gen freq sf!
    ph flen f* two-pi f/      gen phase sf!
    flen two-pi f/            gen scaler sf!
    tp                        gen interpolation-type !
    ['] tb-frequency@         gen get-freq !
    ['] tb-frequency!         gen set-freq !
    ['] tb-phase@             gen get-phase !
    ['] tb-phase!             gen set-phase !
    ['] table-lookup=         gen gfm-base-equal !
    ['] .table-lookup         gen gfm-base-inspect !
    ['] $(.table-lookup)      gen gfm-base-to-string !
    ['] table-lookup-run      gen gfm-base-run !
    ['] vct-buffer-free       gen gfm-base-free !
    str-table-lookup          gen gfm-base-name !
    str-mus-gen               gen gfm-generator !
    gen
;

\ === Waveshape ===
oscil%
    cell%   field ws-osc
    sfloat% field ws-offset
end-struct waveshape%

: ws-frequency@ ( gen -- r ) ws-osc @ gfm-frequency@ ;
: ws-frequency! ( val gen -- r ) ws-osc @ gfm-frequency! ;
: ws-phase@     ( gen -- r ) ws-osc @ gfm-phase@ ;
: ws-phase!     ( val gen -- r ) ws-osc @ gfm-phase! ;

str-waveshape make-?obj ?waveshape
: waveshape= { g1 g2 -- }
    g1 ?waveshape g2 ?waveshape and if
	g1 g2 oscil-equal
	g1 buffer-length @ g2 buffer-length @ = and
	g1 vct-buffer @ g2 vct-buffer @ vct= and
    else
	false
    then
;
: $(.waveshape) { gen -- str }
    $" #<" str-waveshape $+
    gen ws-osc @ $(.pp-print) $+
    $" , size: " $+ gen data>vct length $(.) $+
    $" >" $+
;
' $(.waveshape) make-inspect .waveshape
: waveshape { f: index f: fm gen -- r }
    gen ws-offset sf@ 1e fm gen ws-osc @ oscil-1 index f* f+ f* gen vct-buffer @ array-interp
;
: waveshape-1 { f: index gen -- r }
    gen ws-offset sf@ 1e gen ws-osc @ oscil-0 index f* f+ f* gen vct-buffer @ array-interp
;
\ 440e 1e 1e 2 >vct make-waveshape value gen
: clm-make-waveshape { f: frq parts -- gen }
    waveshape% %alloc { gen }
    frq 0e clm-make-oscil    gen ws-osc !
    parts                    gen vct-buffer !
    *table-size*             gen buffer-length !
    *table-size* 1- s>f f2/  gen ws-offset sf!
    ['] ws-frequency@        gen get-freq !
    ['] ws-frequency!        gen set-freq !
    ['] ws-phase@            gen get-phase !
    ['] ws-phase!            gen set-phase !
    ['] waveshape=           gen gfm-base-equal !
    ['] .waveshape           gen gfm-base-inspect !
    ['] $(.waveshape)        gen gfm-base-to-string !
    ['] waveshape            gen gfm-base-run !
    ['] vct-buffer-free      gen gfm-base-free !
    str-waveshape            gen gfm-base-name !
    str-mus-gen              gen gfm-generator !
    gen
;

\ === Triangle-Wave ===
3e half-pi f* fconstant three-half-pi
2e pi f/ fconstant two/pi

: triangle-scaler@ ( gen -- r ) scaler sf@ half-pi f* ;
: triangle-scaler! ( val gen -- ) half-pi f* scaler sf! ;

str-triangle-wave make-?obj ?triangle-wave
: triangle-wave= { g1 g2 -- f }
    g1 ?triangle-wave g2 ?triangle-wave and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
    else
	false
    then
;
: $(.triangle-wave) { gen -- str }
    $" #<" str-triangle-wave $+
    gen $(.pp-print) $+
    $" , scaler: " $+ gen triangle-scaler@ 3 $(f.r) $+
    $" >" $+
;
' $(.triangle-wave) make-inspect .triangle-wave
: triangle-wave { f: fm gen -- r }
    gen current-value sf@		\ result
    gen freq sf@ fm f+ gen phase sf+!

    gen phase sf@ fdup two-pi f>= fdup f0< or if
	two-pi fmod fdup f0< if two-pi f+ then gen phase sf!
    else
	fdrop
    then

    gen phase sf@ half-pi f< if
	gen scaler sf@ gen phase sf@ f*
    else
	gen phase sf@ three-half-pi f< if
	    gen scaler sf@ pi gen phase sf@ f- f*
	else
	    gen scaler sf@ gen phase sf@ two-pi f- f*
	then
    then gen current-value sf!
;
: triangle-wave-run ( r1 r2 gen -- r3 ) fdrop triangle-wave ;
\ 440e 1e 0e clm-make-triangle-wave value gen
: clm-make-triangle-wave { f: frq f: amp f: ph -- gen }
    frq ph clm-make-oscil { gen }
    amp f2* pi f/         gen scaler sf!
    ph half-pi f< if
	amp ph f*
    else
	ph three-half-pi f< if
	    amp pi ph f- f*
	else
	    amp ph two-pi f- f*
	then
    then                  gen current-value sf!
    ['] triangle-scaler@  gen get-scaler !
    ['] triangle-scaler!  gen set-scaler !
    ['] triangle-wave=    gen gfm-base-equal !
    ['] .triangle-wave    gen gfm-base-inspect !
    ['] $(.triangle-wave) gen gfm-base-to-string !
    ['] triangle-wave-run gen gfm-base-run !
    ['] generator-free    gen gfm-base-free !
    str-triangle-wave     gen gfm-base-name !
    str-mus-gen           gen gfm-generator !
    gen
;

\ === Square-Wave ===
oscil%
    sfloat% field width
end-struct square-wave%

: width@ ( gen -- r ) width sf@ two-pi f/ ;
: width! ( val gen -- ) two-pi f* width sf! ;

str-square-wave make-?obj ?square-wave
: square-wave= { g1 g2 -- f }
    g1 ?square-wave g2 ?square-wave and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
    else
	false
    then
;
: $(.square-wave) { gen -- str }
    $" #<" str-square-wave $+
    gen $(.pp-print) $+
    $" , scaler: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" >" $+
;
' $(.square-wave) make-inspect .square-wave
: square-wave { f: fm gen -- r }
    gen current-value sf@		\ result
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ fdup two-pi f>= fdup f0< or if
	two-pi fmod fdup f0< if two-pi f+ then gen phase sf!
    else
	fdrop
    then
    gen phase sf@ gen width sf@ f< if gen scaler sf@ else 0e then gen current-value sf!
;
: square-wave-run ( r1 r2 gen -- r3 ) fdrop square-wave ;
\ 440e 1e 0e make-square-wave value gen
: clm-make-square-wave { f: frq f: amp f: ph -- gen }
    square-wave% %alloc { gen }
    ph pi f< if amp else 0e then gen current-value sf!
    frq hz>radians      gen freq sf!
    ph                  gen phase sf!
    amp                 gen scaler sf!
    pi                  gen width sf!
    ['] gfm-frequency@  gen get-freq !
    ['] gfm-frequency!  gen set-freq !
    ['] gfm-phase@      gen get-phase !
    ['] gfm-phase!      gen set-phase !
    ['] gfm-scaler@     gen get-scaler !
    ['] gfm-scaler!     gen set-scaler !
    ['] square-wave=    gen gfm-base-equal !
    ['] .square-wave    gen gfm-base-inspect !
    ['] $(.square-wave) gen gfm-base-to-string !
    ['] square-wave-run gen gfm-base-run !
    ['] generator-free  gen gfm-base-free !
    str-square-wave     gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === Sawtooth-Wave ===
: sawtooth-scaler@ ( gen -- r ) scaler sf@ pi f* ;
: sawtooth-scaler! ( val gen -- ) pi f/ scaler sf! ;

str-sawtooth-wave make-?obj ?sawtooth-wave
: sawtooth-wave= { g1 g2 -- f }
    g1 ?sawtooth-wave g2 ?sawtooth-wave and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
    else
	false
    then
;
: $(.sawtooth-wave) { gen -- str }
    $" #<" str-sawtooth-wave $+
    gen $(.pp-print) $+
    $" , scaler: " $+ gen sawtooth-scaler@ 3 $(f.r) $+
    $" >" $+
;
' $(.sawtooth-wave) make-inspect .sawtooth-wave
: sawtooth-wave { f: fm gen -- r }
    gen current-value sf@		\ result
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ fdup two-pi f>= fdup f0< or if
	two-pi fmod fdup f0< if two-pi f+ then gen phase sf!
    else
	fdrop
    then
    gen scaler sf@ gen phase sf@ pi f- f* gen current-value sf!
;
: sawtooth-wave-run ( r1 r2 gen -- r3 ) fdrop sawtooth-wave ;
\ 440e 1e pi clm-make-sawtooth-wave value gen
: clm-make-sawtooth-wave { f: frq f: amp f: ph -- gen }
    frq ph clm-make-oscil { gen }
    amp ph pi f- pi f/ f* gen current-value sf!
    amp pi f/             gen scaler sf!
    ['] sawtooth-scaler@  gen get-scaler !
    ['] sawtooth-scaler!  gen set-scaler !
    ['] sawtooth-wave=    gen gfm-base-equal !
    ['] .sawtooth-wave    gen gfm-base-inspect !
    ['] $(.sawtooth-wave) gen gfm-base-to-string !
    ['] sawtooth-wave-run gen gfm-base-run !
    ['] generator-free    gen gfm-base-free !
    str-sawtooth-wave     gen gfm-base-name !
    str-mus-gen           gen gfm-generator !
    gen
;

\ === Pulse-Train ===
str-pulse-train make-?obj ?pulse-train
: pulse-train= { g1 g2 -- f }
    g1 ?pulse-train g2 ?pulse-train and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
    else
	false
    then
;
: $(.pulse-train) { gen -- str }
    $" #<" str-pulse-train $+
    gen $(.pp-print) $+
    $" , scaler: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" >" $+
;
' $(.pulse-train) make-inspect .pulse-train
: pulse-train { f: fm gen -- r }
    gen phase sf@ fdup two-pi f>= fdup f0< or if
	two-pi fmod fdup f0< if two-pi f+ then gen phase sf!
	gen scaler sf@
    else
	fdrop
	0e
    then				\ return
    gen freq sf@ fm f+ gen phase sf+!
;
: pulse-train-run ( r1 r2 gen -- r3 ) fdrop pulse-train ;
\ 440e 1e two-pi clm-make-pulse-train value gen
: clm-make-pulse-train { f: frq f: amp f: ph -- gen }
    frq ph clm-make-oscil { gen }
    amp                 gen scaler sf!
    ['] gfm-scaler@     gen get-scaler !
    ['] gfm-scaler!     gen set-scaler !
    ['] pulse-train=    gen gfm-base-equal !
    ['] .pulse-train    gen gfm-base-inspect !
    ['] $(.pulse-train) gen gfm-base-to-string !
    ['] pulse-train-run gen gfm-base-run !
    ['] generator-free  gen gfm-base-free !
    str-pulse-train     gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === Sum-Of-Cosines ===
oscil%
    sfloat% field cos5
end-struct sum-of-cosines%

: cosines@ ( gen -- n ) cosines @ ;
: cosines! { val gen -- }
    val gen cosines !
    val s>f { f: fval }
    fval 0.5e f+ gen cos5 sf!
    fval 1/f gen scaler sf!
;

str-sum-of-cosines make-?obj ?sum-of-cosines
: sum-of-cosines= { g1 g2 -- f }
    g1 ?sum-of-cosines g2 ?sum-of-cosines and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 cosines @ g2 cosines @ = and
    else
	false
    then
;
: $(.sum-of-cosines) { gen -- str }
    $" #<" str-sum-of-cosines $+
    gen $(.pp-print) $+
    $" , cosines: " $+ gen cosines@ $(.) $+
    $" >"  $+
;
' $(.sum-of-cosines) make-inspect .sum-of-cosines
: sum-of-cosines { f: fm gen -- r }
    gen phase sf@ f2/ fsin fdup f0= if
	fdrop 1e
    else
	gen scaler sf@  gen phase sf@ gen cos5 sf@ f* fsin  frot f2*  f/  0.5e f-  f*  1e fmin
    then
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ two-pi f> if two-pi fnegate gen phase sf+! then
    gen phase sf@ two-pi fnegate f< if two-pi gen phase sf+! then
;
: sum-of-cosines-run ( r1 r2 gen -- r3 ) fdrop sum-of-cosines ;
\ 440e 0e 1 clm-make-sum-of-cosines value gen
: clm-make-sum-of-cosines { f: frq f: ph cosin -- gen }
    sum-of-cosines% %alloc { gen }
    frq hz>radians         gen freq sf!
    ph                     gen phase sf!
    cosin                  gen cosines !
    cosin                  gen buffer-length !
    cosin s>f 1/f          gen scaler sf!
    cosin s>f 0.5e f+      gen cos5 sf!
    ['] gfm-frequency@     gen get-freq !
    ['] gfm-frequency!     gen set-freq !
    ['] gfm-phase@         gen get-phase !
    ['] gfm-phase!         gen set-phase !
    ['] gfm-scaler@        gen get-scaler !
    ['] gfm-scaler!        gen set-scaler !
    ['] sum-of-cosines=    gen gfm-base-equal !
    ['] .sum-of-cosines    gen gfm-base-inspect !
    ['] $(.sum-of-cosines) gen gfm-base-to-string !
    ['] sum-of-cosines-run gen gfm-base-run !
    ['] generator-free     gen gfm-base-free !
    str-sum-of-cosines     gen gfm-base-name !
    str-mus-gen            gen gfm-generator !
    gen
;

\ === Sum-Of-Sines ===
1.0e 1.0e 1.761e 2.5e 3.24e 3.97e 4.7e 5.42e 6.15e 6.88e 7.6e 8.33e 9.05e 9.78e
10.51e 11.23e 11.96e 12.68e 13.41e 14.13e  20 >vct value fsndlib-scl-ary

: sum-of-sines-scaler { sines -- r }
    sines 20 < if
	sines fsndlib-scl-ary vct@ 1/f
    else
	sines 50 < if 0.743e else 0.733e then sines s>f f* 1/f
    then
;

: sines@ ( gen -- n ) cosines @ ;
: sines! { val gen -- }
    val gen cosines !
    val 1+ s>f gen cos5 sf!
    val sum-of-sines-scaler gen scaler sf!
;

str-sum-of-sines make-?obj ?sum-of-sines
: sum-of-sines= { g1 g2 -- f }
    g1 ?sum-of-sines g2 ?sum-of-sines and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 cosines @ g2 cosines @ = and
    else
	false
    then
;
: $(.sum-of-sines) { gen -- str }
    $" #<" str-sum-of-sines $+
    gen $(.pp-print) $+
    $" , sines: " $+ gen sines@ $(.) $+
    $" >" $+
;
' $(.sum-of-sines) make-inspect .sum-of-sines
: sum-of-sines { f: fm gen -- r }
    gen phase sf@ f2/ { f: a2 }
    a2 fsin fdup f0<> if ( f: den )
	gen scaler sf@  gen cosines @ s>f a2 f* fsin  a2 gen cos5 sf@ f* fsin  f* f* fswap f/
    then
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ two-pi f> if two-pi fnegate gen phase sf+! then
    gen phase sf@ two-pi fnegate f< if two-pi gen phase sf+! then
;
: sum-of-sines-run ( r1 r2 gen -- r3 ) fdrop sum-of-sines ;
\ 440e 0e 1 clm-make-sum-of-sines value gen
: clm-make-sum-of-sines { f: frq f: ph cosin -- gen }
    frq ph cosin clm-make-sum-of-cosines { gen }
    cosin sum-of-sines-scaler gen scaler sf!
    cosin 1+ s>f              gen cos5 sf!
    ['] sum-of-sines=         gen gfm-base-equal !
    ['] .sum-of-sines         gen gfm-base-inspect !
    ['] $(.sum-of-sines)      gen gfm-base-to-string !
    ['] sum-of-sines-run      gen gfm-base-run !
    str-sum-of-sines          gen gfm-base-name !
    gen
;

\ === Sine-Summation ===
oscil%
    sfloat% field ss-b
    sfloat% field ss-n
    sfloat% field ss-an
    sfloat% field ss-a2
end-struct sine-summation%

str-sine-summation make-?obj ?sine-summation
: sine-summation= { g1 g2 -- f }
    g1 ?sine-summation g2 ?sine-summation and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 ss-n sf@ g2 ss-n sf@ f= and
	g1 ss-b sf@ g2 ss-b sf@ f= and
	g1 ss-a2 sf@ g2 ss-a2 sf@ f= and
    else
	false
    then
;
: $(.sine-summation) { gen -- str }
    $" #<" str-sine-summation $+
    gen $(.pp-print) $+
    $" , n: " $+ gen ss-n sf@ f>s $(.) $+
    $" , a: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" , ratio: " $+ gen ss-b sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.sine-summation) make-inspect .sine-summation
: sine-summation { f: fm gen -- r }
    gen phase sf@ { f: th }
    gen scaler sf@ { f: a }
    gen ss-n sf@ { f: N }
    gen ss-b sf@ th f* { f: B }
    th B f- { f: thB }
    gen ss-a2 sf@ a B fcos f* f2* f- { f: divis }
    divis f0= if
	0e
    else
	th fsin thB fsin a f* f-
	gen ss-an sf@ th B N 1e f+ f* f+ fsin a th B N f* f+ fsin f* f- f* f-
	divis f/
    then
    gen freq sf@ fm f+ gen phase sf+!
;
: sine-summation-run ( r1 r2 gen -- r3 ) fdrop sine-summation ;
\ 440e 0e 1 0.5e 1e clm-make-sine-summation value gen
: clm-make-sine-summation { f: frq f: ph w: n f: a f: ratio -- gen }
    sine-summation% %alloc { gen }
    frq hz>radians         gen freq sf!
    ph                     gen phase sf!
    a                      gen scaler sf!
    n s>f                  gen ss-n sf!
    ratio                  gen ss-b sf!
    1e a a f* f+           gen ss-a2 sf!
    a n s>f 1e f+ f**      gen ss-an sf!
    ['] gfm-frequency@     gen get-freq !
    ['] gfm-frequency!     gen set-freq !
    ['] gfm-phase@         gen get-phase !
    ['] gfm-phase!         gen set-phase !
    ['] gfm-scaler@        gen get-scaler !
    ['] gfm-scaler!        gen set-scaler !
    ['] sine-summation=    gen gfm-base-equal !
    ['] .sine-summation    gen gfm-base-inspect !
    ['] $(.sine-summation) gen gfm-base-to-string !
    ['] sine-summation-run gen gfm-base-run !
    ['] generator-free     gen gfm-base-free !
    str-sine-summation     gen gfm-base-name !
    str-mus-gen            gen gfm-generator !
    gen
;

\ === Asymmetric-FM ===
oscil%
    sfloat% field ratio
    sfloat% field cosr
    sfloat% field sinr
end-struct asymmetric-fm%

: asymmetric-scaler! { f: val gen -- }
    val gen scaler !
    val f0<> if
	val gen ratio sf!
	0.5e val val 1/f f- f* gen cosr sf!
	0.5e val val 1/f f+ f* gen sinr sf!
    then
;

str-asymmetric-fm make-?obj ?asymmetric-fm
: asymmetric-fm= { g1 g2 -- f }
    g1 ?asymmetric-fm g2 ?asymmetric-fm and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 ratio sf@ g2 ratio sf@ f= and
	g1 sinr sf@ g2 sinr sf@ f= and
    else
	false
    then
;
: $(.asymmetric-fm) { gen -- str }
    $" #<" str-asymmetric-fm $+
    gen $(.pp-print) $+
    $" , ratio: "  $+ gen ratio sf@ 3 $(f.r) $+
    $" , r: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" >" $+
;
' $(.asymmetric-fm) make-inspect .asymmetric-fm
: asymmetric-fm { f: index f: fm gen -- r }
    gen ratio sf@ gen phase sf@ f* { f: mth }
    index gen cosr sf@ mth fcos f* f* fexp
    gen phase sf@ gen sinr sf@ index mth fsin f* f* f+ fsin f* \ result
    gen freq sf@ fm f+ gen phase sf+!
    gen phase sf@ fdup 1000e f> -1000e f< or if gen phase sf@ two-pi fmod gen phase sf! then
;
: asymmetric-fm-1 { f: index gen -- r }
    gen ratio sf@ gen phase sf@ f* { f: mth }
    index gen cosr sf@ mth fcos f* f* fexp
    gen phase sf@ gen sinr sf@ index mth fsin f* f* f+ fsin f* \ result
    gen freq sf@ gen phase sf+!
    gen phase sf@ fdup 1000e f> -1000e f< or if gen phase sf@ two-pi fmod gen phase sf! then
;
\ 440e 0e 1e 1e clm-make-asymmetric-fm value gen
: clm-make-asymmetric-fm { f: frq f: ph f: r f: rat -- gen }
    asymmetric-fm% %alloc { gen }
    frq hz>radians         gen freq sf!
    ph                     gen phase sf!
    rat                    gen ratio sf!
    r                      gen scaler sf!
    0.5e r r 1/f f- f*     gen cosr sf!
    0.5e r r 1/f f+ f*     gen sinr sf!
    ['] gfm-frequency@     gen get-freq !
    ['] gfm-frequency!     gen set-freq !
    ['] gfm-phase@         gen get-phase !
    ['] gfm-phase!         gen set-phase !
    ['] gfm-scaler@        gen get-scaler !
    ['] asymmetric-scaler! gen set-scaler !
    ['] asymmetric-fm=     gen gfm-base-equal !
    ['] .asymmetric-fm     gen gfm-base-inspect !
    ['] $(.asymmetric-fm)  gen gfm-base-to-string !
    ['] asymmetric-fm      gen gfm-base-run !
    ['] generator-free     gen gfm-base-free !
    str-asymmetric-fm      gen gfm-base-name !
    str-mus-gen            gen gfm-generator !
    gen
;

\ === Rand ===
\ from clm-2/cmus.c
1 value random-seed
0.0000610351563e fconstant INVERSE-MAX-RAND
0.0000305175790e fconstant INVERSE-MAX-RAND2

: rand-seed!  ( val -- ) to random-seed ;
: rand-seed@  ( -- u ) random-seed ;
: next-random ( -- r )
    random-seed 1103515245 * 12345 + dup to random-seed
    16 rshift 32767 and s>f
;
: mus-random ( val -- r ) next-random INVERSE-MAX-RAND f* 1e f- f* ;
: random     ( val -- r ) next-random INVERSE-MAX-RAND2 f* f* ;
: irandom    ( val -- r ) next-random INVERSE-MAX-RAND2 f* s>f f* f>s ;

oscil%
    sfloat% field output
end-struct rand%

: random-any { gen -- r }
    gen vct-buffer @ ?vct if
	gen scaler sf@ gen buffer-length @ s>f random gen vct-buffer @ array-interp f*
    else
	gen scaler sf@ f0= if 0e else gen scaler sf@ f2* random gen scaler sf@ f- then
    then
;

str-rand make-?obj ?rand
: rand= { g1 g2 -- f }
    g1 ?rand g2 ?rand and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 output sf@ g2 output sf@ f= and
	g1 vct-buffer @ ?vct if
	    g1 buffer-length @ g2 buffer-length @ = and
	    g1 vct-buffer @ g2 vct-buffer @ vct= and
	then
    else
	false
    then
;
: $(.rand) { gen -- str }
    $" #<" str-rand $+
    gen $(.pp-print) $+
    $" , amp: " $+ gen gfm-scaler@ 3 $(f.r) $+
    gen data>vct ?vct if $" , with dist" $+ then
    $" >" $+
;
' $(.rand) make-inspect .rand
: rand { f: fm gen -- r }
    gen phase sf@ fdup two-pi f>= fdup f0< or if
	two-pi fmod fdup f0< if two-pi f+ then gen phase sf!
	gen random-any gen output sf!
    else
	fdrop
    then
    gen freq sf@ fm f+ gen phase sf+!
    gen output sf@
;
: rand-run ( r1 r2 gen -- r3 ) fdrop rand ;
\ 440e 1e false clm-make-rand value gen
: clm-make-rand { f: frq f: amp dist -- gen }
    rand% %alloc { gen }
    frq hz>radians     gen freq sf!
    0e                 gen phase sf!
    amp                gen scaler sf!
    0e                 gen output sf!
    dist ?vct if
	dist inverse-integrate { data }
	data                gen vct-buffer !
	data length         gen buffer-length !
	['] vct-buffer-free gen gfm-base-free !
    else
	false               gen vct-buffer !
	0                   gen buffer-length !
	['] generator-free  gen gfm-base-free !
    then
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@     gen get-phase !
    ['] gfm-phase!     gen set-phase !
    ['] gfm-scaler@    gen get-scaler !
    ['] gfm-scaler!    gen set-scaler !
    ['] rand=          gen gfm-base-equal !
    ['] .rand          gen gfm-base-inspect !
    ['] $(.rand)       gen gfm-base-to-string !
    ['] rand-run       gen gfm-base-run !
    str-rand           gen gfm-base-name !
    str-mus-gen        gen gfm-generator !
    gen
;

\ === Rand-Interp ===
rand%
    sfloat% field incr
end-struct rand-interp%

str-rand-interp make-?obj ?rand-interp
: rand-interp= { g1 g2 -- f }
    g1 ?rand-interp g2 ?rand-interp and if
	g1 g2 oscil-equal
	g1 scaler sf@ g2 scaler sf@ f= and
	g1 current-value sf@ g2 current-value sf@ f= and
	g1 vct-buffer @ ?vct if
	    g1 buffer-length @ g2 buffer-length @ = and
	    g1 vct-buffer @ g2 vct-buffer @ vct= and
	then
    else
	false
    then
;
: $(.rand-interp) { gen -- str }
    $" #<" str-rand-interp $+
    gen $(.pp-print) $+
    $" , amp: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" , incr: " $+ gen incr sf@ 3 $(f.r) $+
    $" , curval: " $+ gen current-value sf@ 3 $(f.r) $+
    gen data>vct ?vct if $" , with dist" $+ then
    $" >" $+
;
' $(.rand-interp) make-inspect .rand-interp
: rand-interp { f: fm gen -- r }
    gen incr sf@ gen current-value sf+!
    gen phase sf@ two-pi f>= if
	begin gen phase sf@ two-pi f>= while two-pi fnegate gen phase sf+! repeat
	gen random-any gen current-value sf@ f- gen freq sf@ fm f+ two-pi f/ f* gen incr sf!
    then
    gen freq sf@ fm f+ gen phase sf+!
    begin gen phase sf@ f0< while two-pi gen phase sf+! repeat
    gen current-value sf@
;
: rand-interp-run ( r1 r2 gen -- r3 ) fdrop rand-interp ;
\ 440e 1e make-rand-interp value gen
: clm-make-rand-interp { f: frq f: amp dist -- gen }
    rand-interp% %alloc { gen }
    frq hz>radians      gen freq sf!
    0e                  gen phase sf!
    amp                 gen scaler sf!
    0e                  gen current-value sf!
    amp f0= if 0e else frq mus-srate@ f/ amp mus-random f* then gen incr sf!
    dist ?vct if
	dist inverse-integrate { data }
	data                gen vct-buffer !
	data length         gen buffer-length !
	['] vct-buffer-free gen gfm-base-free !
    else
	false               gen vct-buffer !
	0                   gen buffer-length !
	['] generator-free  gen gfm-base-free !
    then
    ['] gfm-frequency@  gen get-freq !
    ['] gfm-frequency!  gen set-freq !
    ['] gfm-phase@      gen get-phase !
    ['] gfm-phase!      gen set-phase !
    ['] gfm-scaler@     gen get-scaler !
    ['] gfm-scaler!     gen set-scaler !
    ['] rand-interp=    gen gfm-base-equal !
    ['] .rand-interp    gen gfm-base-inspect !
    ['] $(.rand-interp) gen gfm-base-to-string !
    ['] rand-interp-run gen gfm-base-run !
    str-rand-interp     gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === One-Pole, One-Zero, Two-Pole, Two-Zero ===
music5%
    cell%   field get-xcoeff
    sfloat% field flt-a0
    sfloat% field flt-b1
    sfloat% field flt-b2
    sfloat% field flt-y1
    sfloat% field flt-y2
    sfloat% field flt-a1
    sfloat% field flt-a2
    sfloat% field flt-x1
    sfloat% field flt-x2
end-struct simp-flt%

' length@ alias order@
: xcoeff@ ( gen -- r ) dup get-xcoeff @ execute ;
: a0@ 	  ( gen -- r ) flt-a0 sf@ ;
: a0! 	  ( val gen -- ) flt-a0 sf! ;
: a1@ 	  ( gen -- r ) flt-a1 sf@ ;
: a1! 	  ( val gen -- ) flt-a1 sf! ;
: a2@ 	  ( gen -- r ) flt-a2 sf@ ;
: a2! 	  ( val gen -- ) flt-a2 sf! ;
: b1@ 	  ( gen -- r ) flt-b1 sf@ ;
: b1! 	  ( val gen -- ) flt-b1 sf! ;
: b2@ 	  ( gen -- r ) flt-b2 sf@ ;
: b2! 	  ( val gen -- ) flt-b2 sf! ;
: simp-flt-xcoeff@ ( idx gen -- r )
    swap case
	0 of a0@ endof
	1 of a1@ endof
	2 of a2@ endof
	0e
    endcase
;
: xcoeff! ( val idx gen -- )
    swap case
	0 of a0! endof
	1 of a1! endof
	2 of a2! endof
	fdrop
    endcase
;
: ycoeff@ ( idx gen -- r )
    swap case
	1 of b1@ endof
	2 of b2@ endof
	0e
    endcase
;
: ycoeff! ( val idx gen -- )
    swap case
	1 of b1! endof
	2 of b2! endof
	fdrop
    endcase
;

\ === One-Pole ===
str-one-pole make-?obj ?one-pole
: one-pole= { g1 g2 -- f }
    g1 ?one-pole g2 ?one-pole and if
	g1 g2 =
	g1 flt-b1 sf@ g2 flt-b1 sf@ f=
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-y1 sf@ g2 flt-y1 sf@ f= and or
    else
	false
    then
;
: $(.one-pole) { gen -- str }
    $" #<" str-one-pole $+
    $"  a0: " $+ gen a0@ 3 $(f.r) $+
    $" , b1: " $+ gen b1@ 3 $(f.r) $+
    $" , y1: " $+ gen flt-y1 sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.one-pole) make-inspect .one-pole
: one-pole { f: input gen -- r }
    gen flt-a0 sf@ input f* gen flt-b1 sf@ gen flt-y1 sf@ f* f- fdup
    gen flt-y1 sf!

;
: one-pole-run ( r1 r2 gen -- r3 ) fdrop one-pole ;
: make-one-pole { f: a0 f: b1 -- gen }
    simp-flt% %alloc { gen }
    a0                   gen flt-a0 sf!
    0e                   gen flt-a1 sf!
    0e                   gen flt-a2 sf!
    b1                   gen flt-b1 sf!
    0e                   gen flt-b2 sf!
    0e                   gen flt-y1 sf!
    1                    gen buffer-length !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] one-pole=        gen gfm-base-equal !
    ['] .one-pole        gen gfm-base-inspect !
    ['] $(.one-pole)     gen gfm-base-to-string !
    ['] one-pole-run     gen gfm-base-run !
    ['] generator-free   gen gfm-base-free !
    str-one-pole         gen gfm-base-name !
    str-mus-gen          gen gfm-generator !
    gen
;

\ === One-Zero ===
str-one-zero make-?obj ?one-zero
: one-zero= { g1 g2 -- f }
    g1 ?one-zero g2 ?one-zero and if
	g1 g2 =
	g1 flt-a1 sf@ g2 flt-a1 sf@ f=
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-x1 sf@ g2 flt-x1 sf@ f= and or
    else
	false
    then
;
: $(.one-zero) { gen -- str }
    $" #<" str-one-zero $+
    $"  a0: " $+ gen a0@ 3 $(f.r) $+
    $" , a1: " $+ gen a1@ 3 $(f.r) $+
    $" , x1: " $+ gen flt-x1 sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.one-zero) make-inspect .one-zero
: one-zero { f: input gen -- r }
    gen flt-a0 sf@ input f* gen flt-a1 sf@ gen flt-x1 sf@ f* f+
    input gen flt-x1 sf!
;
: one-zero-run ( r1 r2 gen -- r3 ) fdrop one-zero ;
: make-one-zero { f: a0 f: a1 -- gen }
    simp-flt% %alloc { gen }
    a0                   gen flt-a0 sf!
    a1                   gen flt-a1 sf!
    0e                   gen flt-a2 sf!
    0e                   gen flt-x1 sf!
    1                    gen buffer-length !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] one-zero=        gen gfm-base-equal !
    ['] .one-zero        gen gfm-base-inspect !
    ['] $(.one-zero)     gen gfm-base-to-string !
    ['] one-zero-run     gen gfm-base-run !
    ['] generator-free   gen gfm-base-free !
    str-one-zero         gen gfm-base-name !
    str-mus-gen          gen gfm-generator !
    gen
;

\ === Two-Pole ===
str-two-pole make-?obj ?two-pole
: two-pole= { g1 g2 -- f }
    g1 ?two-pole g2 ?two-pole and if
	g1 g2 =
	g1 flt-b2 sf@ g2 flt-b2 sf@ f=
	g1 flt-b1 sf@ g2 flt-b1 sf@ f= and
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-y1 sf@ g2 flt-y1 sf@ f= and
	g1 flt-y2 sf@ g2 flt-y2 sf@ f= and or
    else
	false
    then
;
: $(.two-pole) { gen -- str }
    $" #<" str-two-pole $+
    $"  a0: " $+ gen a0@ 3 $(f.r) $+
    $" , b1: " $+ gen b1@ 3 $(f.r) $+
    $" , b2: " $+ gen b2@ 3 $(f.r) $+
    $" , y1: " $+ gen flt-y1 sf@ 3 $(f.r) $+
    $" , y2: " $+ gen flt-y2 sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.two-pole) make-inspect .two-pole
: two-pole { f: input gen -- r }
    gen flt-a0 sf@ input f* gen flt-b1 sf@ gen flt-y1 sf@ f* f- gen flt-b2 sf@ gen flt-y2 sf@ f* f-
    gen flt-y1 sf@ gen flt-y2 sf!
    fdup gen flt-y1 sf!
;
: two-pole-run ( r1 r2 gen -- r3 ) fdrop two-pole ;
: make-two-pole { f: a0 f: b1 f: b2 -- gen }
    simp-flt% %alloc { gen }
    b1                   gen flt-b1 sf!
    b2                   gen flt-b2 sf!
    a0                   gen flt-a0 sf!
    0e                   gen flt-a1 sf!
    0e                   gen flt-a2 sf!
    0e                   gen flt-y1 sf!
    0e                   gen flt-y2 sf!
    2                    gen buffer-length !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] two-pole=        gen gfm-base-equal !
    ['] .two-pole        gen gfm-base-inspect !
    ['] $(.two-pole)     gen gfm-base-to-string !
    ['] two-pole-run     gen gfm-base-run !
    ['] generator-free   gen gfm-base-free !
    str-two-pole         gen gfm-base-name !
    str-mus-gen          gen gfm-generator !
    gen
;
: make-ppolar { f: rad f: frq -- gen }
    1e frq hz>radians fcos rad f* f2* fnegate rad rad f* make-two-pole
;

\ === Two-Zero ===
str-two-zero make-?obj ?two-zero
: two-zero= { g1 g2 -- f }
    g1 ?two-zero g2 ?two-zero and if
	g1 g2 =
	g1 flt-a2 sf@ g2 flt-a2 sf@ f=
	g1 flt-a1 sf@ g2 flt-a1 sf@ f= and
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-x1 sf@ g2 flt-x1 sf@ f= and
	g1 flt-x2 sf@ g2 flt-x2 sf@ f= and or
    else
	false
    then
;
: $(.two-zero) { gen -- str }
    $" #<" str-two-zero $+
    $"  a0: " $+ gen a0@ 3 $(f.r) $+
    $" , a1: " $+ gen a1@ 3 $(f.r) $+
    $" , a2: " $+ gen a2@ 3 $(f.r) $+
    $" , x1: " $+ gen flt-x1 sf@ 3 $(f.r) $+
    $" , x2: " $+ gen flt-x2 sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.two-zero) make-inspect .two-zero
: two-zero { f: input gen -- r }
    gen flt-a0 sf@ input f* gen flt-a1 sf@ gen flt-x1 sf@ f* f+ gen flt-a2 sf@ gen flt-x2 sf@ f* f+
    gen flt-x1 sf@ gen flt-x2 sf!
    input gen flt-x1 sf!
;
: two-zero-run ( r1 r2 gen -- r3 ) fdrop two-zero ;
: make-two-zero { f: a0 f: a1 f: a2 -- gen }
    simp-flt% %alloc { gen }
    a0                   gen flt-a0 sf!
    a1                   gen flt-a1 sf!
    a2                   gen flt-a2 sf!
    0e                   gen flt-x1 sf!
    0e                   gen flt-x2 sf!
    2                    gen buffer-length !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] two-zero=        gen gfm-base-equal !
    ['] .two-zero        gen gfm-base-inspect !
    ['] $(.two-zero)     gen gfm-base-to-string !
    ['] two-zero-run     gen gfm-base-run !
    ['] generator-free   gen gfm-base-free !
    str-two-zero         gen gfm-base-name !
    str-mus-gen          gen gfm-generator !
    gen
;
: make-zpolar { f: rad f: frq -- gen }
    1e frq hz>radians fcos rad f* f2* fnegate rad rad f* make-two-zero
;

\ === Formant ===
simp-flt%
    sfloat% field flt-gain
    sfloat% field flt-radius
    sfloat% field flt-freq
end-struct formant%

: formant-radius@ ( gen -- r ) flt-radius sf@ ;
: formant-radius! { f: R gen -- }
    gen flt-freq sf@ hz>radians { f: fw }
    R gen flt-radius sf!
    gen flt-gain sf@ fw fsin f* 1e R R f* f- f* gen flt-a0 sf!
    R fnegate gen flt-a2 sf!
    R f2* fw fcos f* fnegate gen flt-b1 sf!
    R R f* gen flt-b2 sf!
;
: formant-frequency@ ( gen -- r ) flt-freq sf@ ;
: formant-frequency! { f: val gen -- }
    val hz>radians { f: fw }
    gen flt-radius sf@ { f: R }
    val gen flt-freq sf!
    gen flt-gain sf@ fw fsin f* 1e R R f* f- f* gen flt-a0 sf!
    R f2* fw fcos f* fnegate gen flt-b1 sf!
;

str-formant make-?obj ?formant
: formant= { g1 g2 -- f }
    g1 ?formant g2 ?formant and if
	g1 g2 =
	g1 flt-b2 sf@ g2 flt-b2 sf@ f=
	g1 flt-b1 sf@ g2 flt-b1 sf@ f= and
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-a1 sf@ g2 flt-a1 sf@ f= and
	g1 flt-x1 sf@ g2 flt-x1 sf@ f= and
	g1 flt-x2 sf@ g2 flt-x2 sf@ f= and
	g1 flt-y1 sf@ g2 flt-y1 sf@ f= and
	g1 flt-y2 sf@ g2 flt-y2 sf@ f= and or
    else
	false
    then
;
: $(.formant) { gen -- str }
    $" #<" str-formant $+
    $"  radius: " $+ gen flt-radius sf@ 3 $(f.r) $+
    $" , freq: " $+ gen flt-freq sf@ 3 $(f.r) $+
    $" , gain: " $+ gen flt-gain sf@ 3 $(f.r) $+
    $" >" $+
;
' $(.formant) make-inspect .formant
: formant { f: val gen -- r }
    gen flt-a0 sf@ val f* { f: inval }
    inval gen flt-a2 sf@ gen flt-x2 sf@ f* f+ { f: tpinval }
    tpinval gen flt-b1 sf@ gen flt-y1 sf@ f* f- gen flt-b2 sf@ gen flt-y2 sf@ f* f- \ return
    gen flt-y1 sf@ gen flt-y2 sf!
    fdup gen flt-y1 sf!
    gen flt-x1 sf@ gen flt-x2 sf!
    inval gen flt-x1 sf!
;
: formant-run ( r1 r2 gen -- r3 ) fdrop formant ;
\ 1e 440e 1e clm-make-formant value gen
: clm-make-formant { f: rad f: frq f: gain -- gen }
    frq hz>radians { f: fw }
    formant% %alloc { gen }
    0e                                  gen flt-x1 sf!
    0e                                  gen flt-x2 sf!
    0e                                  gen flt-y1 sf!
    0e                                  gen flt-y2 sf!
    gain                                gen flt-gain sf!
    rad                                 gen flt-radius sf!
    frq                                 gen flt-freq sf!
    gain fw fsin f* 1e rad rad f* f- f* gen flt-a0 sf!
    rad fnegate                         gen flt-a2 sf!
    rad f2* fw fcos f* fnegate          gen flt-b1 sf!
    rad rad f*                          gen flt-b2 sf!
    2                                   gen buffer-length !
    ['] formant-frequency@              gen get-freq !
    ['] formant-frequency!              gen set-freq !
    ['] formant=                        gen gfm-base-equal !
    ['] .formant                        gen gfm-base-inspect !
    ['] $(.formant)                     gen gfm-base-to-string !
    ['] formant-run                     gen gfm-base-run !
    ['] generator-free                  gen gfm-base-free !
    str-formant                         gen gfm-base-name !
    str-mus-gen                         gen gfm-generator !
    gen
;
: formant-bank { amps formants f: inval -- r }
    0e
    amps length formants length = if
	amps length 0 ?do i amps vct@ inval i formants array@ formant f* f+ loop
    then
;

\ === Filter ===
simp-flt%
    cell% field flt-xcoeffs
    cell% field flt-ycoeffs
    cell% field flt-state
end-struct filter%

: xcoeffs@ ( gen -- w ) flt-xcoeffs @ ;
: ycoeffs@ ( gen -- w ) flt-ycoeffs @ ;

str-filter make-?obj ?filter
: filter-free { gen -- }
    gen ?filter if
	gen flt-state @ free-vct
	gen flt-xcoeffs @ free-vct
	gen flt-ycoeffs @ free-vct
	gen generator-free
    then
;
: filter= { g1 g2 -- f }
    g1 ?filter g2 ?filter and if
	g1 g2 =
	g1 buffer-length @ g2 buffer-length @ =
	g1 flt-state @ g2 flt-state @ vct= and
	g1 flt-xcoeffs @ g2 flt-xcoeffs @ vct= and
	g1 flt-ycoeffs @ g2 flt-ycoeffs @ vct= and or
    else
	false
    then
;
: $(.filter) { gen -- str } $" #<" str-filter $+ $"  order: " $+ gen order@ $(.) $+ $" >" $+ ;
' $(.filter) make-inspect .filter
: filter { f: input gen -- r }
    0e
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-length @ 1- -do
	i fstate vct@  i gen flt-xcoeffs @ vct@  f*  f+	\ result += ...
	i gen flt-ycoeffs @ vct@  i fstate vct@  f* fnegate 0 fstate vct+!
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@  0 gen flt-xcoeffs @ vct@  f*  f+
;
: filter-run ( r1 r2 gen -- r3 ) fdrop filter ;
\ 1 2 make-vct 2 make-vct clm-make-filter value gen
: clm-make-filter { ord xcoeffs ycoeffs -- gen }
    filter% %alloc { gen }
    ord                 gen buffer-length !
    xcoeffs length ycoeffs length max make-vct gen flt-state !
    xcoeffs vct-copy    gen flt-xcoeffs !
    ycoeffs vct-copy    gen flt-ycoeffs !
    ['] filter=         gen gfm-base-equal !
    ['] .filter         gen gfm-base-inspect !
    ['] $(.filter)      gen gfm-base-to-string !
    ['] filter-run      gen gfm-base-run !
    ['] filter-free     gen gfm-base-free !
    str-filter          gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === FIR-Filter ===
str-fir-filter make-?obj ?fir-filter
: fir-filter-free { gen -- }
    gen ?fir-filter if
	gen flt-state @ free-vct
	gen flt-xcoeffs @ free-vct
	gen generator-free
    then
;
: fir-filter= { g1 g2 -- f }
    g1 ?fir-filter g2 ?fir-filter and if
	g1 g2 =
	g1 buffer-length @ g2 buffer-length @ =
	g1 flt-state @ g2 flt-state @ vct= and
	g1 flt-xcoeffs @ g2 flt-xcoeffs @ vct= and or
    else
	false
    then
;
: $(.fir-filter) { gen -- str }
    $" #<" str-fir-filter $+ $"  order: " $+ gen order@ $(.) $+ $" >" $+
;
' $(.fir-filter) make-inspect .fir-filter
: fir-filter { f: input gen -- r }
    0e					\ result
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-length @ 1- -do
	i fstate vct@  i gen flt-xcoeffs @ vct@  f*  f+
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@  0 gen flt-xcoeffs @ vct@  f*  f+
;
: fir-filter-run ( r1 r2 gen -- r3 ) fdrop fir-filter ;
: clm-make-fir-filter { ord xcoeffs -- gen }
    filter% %alloc { gen }
    ord                 gen buffer-length !
    xcoeffs length make-vct gen flt-state !
    xcoeffs vct-copy    gen flt-xcoeffs !
    ['] fir-filter=     gen gfm-base-equal !
    ['] .fir-filter     gen gfm-base-inspect !
    ['] $(.fir-filter)  gen gfm-base-to-string !
    ['] fir-filter-run  gen gfm-base-run !
    ['] fir-filter-free gen gfm-base-free !
    str-fir-filter      gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === IIR-Filter ===
str-iir-filter make-?obj ?iir-filter
: iir-filter-free { gen -- }
    gen ?iir-filter if
	gen flt-state @ free-vct
	gen flt-ycoeffs @ free-vct
	gen generator-free
    then
;
: iir-filter= { g1 g2 -- f }
    g1 ?iir-filter g2 ?iir-filter and if
	g1 g2 =
	g1 buffer-length @ g2 buffer-length @ =
	g1 flt-state @ g2 flt-state @ vct= and
	g1 flt-ycoeffs @ g2 flt-ycoeffs @ vct= and or
    else
	false
    then
;
: $(.iir-filter) { gen -- str }
    $" #<" str-iir-filter $+ $"  order: " $+ gen order@ $(.) $+ $" >" $+
;
' $(.iir-filter) make-inspect .iir-filter
: iir-filter { f: input gen -- r }
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-length @ 1- -do
	i gen flt-ycoeffs @ vct@  i fstate vct@  f*  fnegate 0 fstate vct+!
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@
;
: iir-filter-run ( r1 r2 gen -- r3 ) fdrop iir-filter ;
: clm-make-iir-filter { ord ycoeffs -- gen }
    filter% %alloc { gen }
    ord                 gen buffer-length !
    ycoeffs length make-vct gen flt-state !
    ycoeffs vct-copy    gen flt-ycoeffs !
    ['] iir-filter=     gen gfm-base-equal !
    ['] .iir-filter     gen gfm-base-inspect !
    ['] $(.iir-filter)  gen gfm-base-to-string !
    ['] iir-filter-run  gen gfm-base-run !
    ['] iir-filter-free gen gfm-base-free !
    str-iir-filter      gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;
' spectrum>coeffs alias make-fir-coeffs

\ === Delay ===
simp-flt%
    cell%   field vsize
    cell%   field vdly
    sfloat% field vloc
    sfloat% field xscale
    sfloat% field yscale
end-struct delay%

: feedback@    ( gen -- r ) yscale sf@ ;
: feedback!    ( val gen -- ) yscale sf! ;
: feedforward@ ( gen -- r ) xscale sf@ ;
: feedforward! ( val gen -- ) xscale sf! ;

: delay-equal { g1 g2 -- f }
    g1 g2 =
    g1 buffer-length @ g2 buffer-length @ =
    g1 vct-buffer @ g2 vct-buffer @ vct= and
    g1 location @ g2 location @ = and
    g1 interpolation-type @ g2 interpolation-type @ = and or
;

str-delay make-?obj ?delay
: delay= { g1 g2 -- f } g1 ?delay g2 ?delay and if g1 g2 delay-equal else false then ;
: $(.delay) { gen -- str }
    $" #<" str-delay $+
    $"  size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+
    $" , line: " $+ gen data>vct $(.vct) $+
    $" >" $+
;
' $(.delay) make-inspect .delay
: tap { f: loc gen -- r }
    gen vdly @ if
	gen interpolation-type @ case
	    mus-interp-all-pass of
		gen vloc sf@ loc f-  gen current-value sf@  gen vct-buffer @
		array-all-pass-interp  fdup gen current-value sf!
	    endof
	    mus-interp-none of
		gen location @ loc f>s - gen buffer-length @ mod 0 max gen vct-buffer @ vct@
	    endof
	    mus-interp-lagrange of
		gen vloc sf@ loc f-  gen vct-buffer @  array-lagrange-interp
	    endof
	    mus-interp-hermite of
		gen vloc sf@ loc f-  gen vct-buffer @  array-hermite-interp
	    endof
	    mus-interp-linear of
		loc f0= if
		    gen vloc sf@ f>s gen vct-buffer @ vct@
		else
		    gen vloc sf@ loc f-  gen vct-buffer @  array-interp
		then
	    endof
	endcase
    else
	loc f0= if gen location @ else gen location @ loc f>s - gen buffer-length @ mod 0 max then
	gen vct-buffer @ vct@
    then
;
: tap-1 { gen -- r } gen location @ gen vct-buffer @ vct@ ;
: delay-tick { f: input gen -- }
    input gen location @ gen vct-buffer @ vct!
    1 gen location +!
    gen vdly @ if
	gen location @ gen vsize @ >= if 0 gen location ! then
	1e gen vloc sf+!
	gen vloc sf@ f>s gen vsize @ >= if 0e gen vloc sf! then
    else
	gen location @ gen buffer-length @ >= if 0 gen location ! then
    then
;
: delay { f: input f: pm gen -- r } pm gen tap ( result ) input gen delay-tick ;
: delay-1 { f: input gen -- r }
    gen vdly @ if gen vloc sf@ f>s else gen location @ then gen vct-buffer @ vct@ \ result
    input gen delay-tick
;
: clm-make-delay { size v tp -- gen }
    delay% %alloc { gen }
    assert1( v ?vct )
    v                   gen vct-buffer !
    v length dup    gen vsize !
    dup size - min s>f  gen vloc sf!
    tp mus-interp-none <> size gen vsize @ <> or gen vdly !
    size                gen buffer-length !
    0                   gen location !
    0e                  gen current-value sf!
    tp                  gen interpolation-type !
    ['] gfm-location!   gen set-location !
    ['] delay=          gen gfm-base-equal !
    ['] .delay          gen gfm-base-inspect !
    ['] $(.delay)       gen gfm-base-to-string !
    ['] delay           gen gfm-base-run !
    ['] vct-buffer-free gen gfm-base-free !
    str-delay           gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === Comb ===
str-comb make-?obj ?comb
: comb= { g1 g2 -- f }
    g1 ?comb g2 ?comb and if
	g1 g2 delay-equal
	g1 yscale sf@ g2 yscale sf@ f= and
    else
	false
    then
;
: $(.comb) { gen -- str }
    $" #<" str-comb $+
    $"  size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+
    $" , feedback: " $+ gen feedback@ 3 $(f.r) $+
    $" , line: " $+ gen data>vct $(.vct) $+
    $" >" $+
;    
' $(.comb) make-inspect .comb
: comb { f: input f: pm gen -- r }
    gen yscale sf@ gen vdly @ if
	pm gen tap f* input f+ pm
    else
	gen location @ gen vct-buffer @ vct@ f* input f+ 0e
    then gen delay
;
: comb-1 { f: input gen -- r }
    gen location @ gen vct-buffer @ vct@ gen yscale sf@ f* input f+ gen delay-1
;
: clm-make-comb { f: scal size v tp -- gen }
    size v tp clm-make-delay { gen }
    scal         gen yscale sf!
    ['] comb=    gen gfm-base-equal !
    ['] .comb    gen gfm-base-inspect !
    ['] $(.comb) gen gfm-base-to-string !
    ['] comb     gen gfm-base-run !
    str-comb     gen gfm-base-name !
    str-mus-gen  gen gfm-generator !
    gen
;

\ === Notch ===
str-notch make-?obj ?notch
: notch= { g1 g2 -- f }
    g1 ?notch g2 ?notch and if
	g1 g2 delay-equal
	g1 xscale sf@ g2 xscale sf@ f= and
    else
	false
    then
;
: $(.notch) { gen -- str }
    $" #<" str-notch $+
    $"  size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+ $" >" $+
    $" , feedforward: " $+ gen feedforward@ 3 $(f.r) $+
    $" , line: " $+ gen data>vct $(.vct) $+
;    
' $(.notch) make-inspect .notch
: notch { f: input f: pm gen -- r } gen xscale sf@ input f* input pm gen delay f+ ;
: notch-1 { f: input gen -- r } gen xscale sf@ input f* input gen delay-1 f+ ;
: clm-make-notch { f: scal size v tp -- gen }
    size v tp clm-make-delay { gen }
    scal          gen xscale sf!
    ['] notch=    gen gfm-base-equal !
    ['] .notch    gen gfm-base-inspect !
    ['] $(.notch) gen gfm-base-to-string !
    ['] notch     gen gfm-base-run !
    str-notch     gen gfm-base-name !
    str-mus-gen   gen gfm-generator !
    gen
;

\ === All-Pass ==
str-all-pass make-?obj ?all-pass
: all-pass= { g1 g2 -- f }
    g1 ?all-pass g2 ?all-pass and if
	g1 g2 delay-equal
	g1 xscale sf@ g2 xscale sf@ f= and
	g1 yscale sf@ g2 yscale sf@ f= and
    else
	false
    then
;
: $(.all-pass) { gen -- str }
    $" #<" str-all-pass $+
    $"  size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+
    $" , feedback: " $+ gen feedback@ 3 $(f.r) $+
    $" , feedforward: " $+ gen feedforward@ 3 $(f.r) $+
    $" , line: " $+ gen data>vct $(.vct) $+
    $" >" $+
;
' $(.all-pass) make-inspect .all-pass
: all-pass { f: input f: pm gen -- r }
    gen yscale sf@
    gen vdly @ if pm gen tap else gen location @ gen vct-buffer @ vct@ then
    f* input f+ { f: d-in }
    d-in pm gen delay gen xscale sf@ d-in f* f+
;
: all-pass-1 { f: input gen -- r }
    gen yscale sf@ gen location @ gen vct-buffer @ vct@ f* input f+ { f: d-in }
    d-in gen delay-1 gen xscale sf@ d-in f* f+
;
: clm-make-all-pass { f: fbck f: ffw size v tp -- gen )
    size v tp clm-make-delay { gen }
    ffw              gen xscale sf!
    fbck             gen yscale sf!
    ['] all-pass=    gen gfm-base-equal !
    ['] .all-pass    gen gfm-base-inspect !
    ['] $(.all-pass) gen gfm-base-to-string !
    ['] all-pass     gen gfm-base-run !
    str-all-pass     gen gfm-base-name !
    str-mus-gen      gen gfm-generator !
    gen
;

\ === Average ===
str-average make-?obj ?average
: average= { g1 g2 -- f }
    g1 ?average g2 ?average and if
	g1 g2 delay-equal
	g1 xscale sf@ g2 xscale sf@ f= and
	g1 yscale sf@ g2 yscale sf@ f= and
    else
	false
    then
;
: $(.average) { gen -- str }
    $" #<" str-average $+
    $"  size: " $+ gen length@ $(.) $+
    $" , line: " $+ gen data>vct $(.vct) $+
    $" >" $+
;
' $(.average) make-inspect .average
: average { f: input gen -- r }
    input  input gen delay-1  f-  gen xscale sf+!
    gen xscale sf@ gen yscale sf@ f*
;
: average-run ( r1 r2 gen -- r3 ) fdrop average ;
: clm-make-average { v -- gen }
    v length { size }
    size v mus-interp-none clm-make-delay { gen }
    0e              gen xscale sf!
    size 0 do i gen vct-buffer @ vct@ gen xscale sf+! loop
    size s>f 1/f    gen yscale sf!
    ['] average=    gen gfm-base-equal !
    ['] .average    gen gfm-base-inspect !
    ['] $(.average) gen gfm-base-to-string !
    ['] average-run gen gfm-base-run !
    str-average     gen gfm-base-name !
    str-mus-gen     gen gfm-generator !
    gen
;

\ === Wave-Train ===
oscil%
    cell%   field out-data
    cell%   field out-pos
    cell%   field first-time
    sfloat% field next-time
end-struct wave-train%

: wt-frequency@ ( gen -- r ) freq sf@ ;
: wt-frequency! ( val gen -- ) freq sf! ;
' tb-phase@ alias wt-phase@
' tb-phase! alias wt-phase!

str-wave-train make-?obj ?wave-train
: wt-free { gen -- }
    gen ?wave-train if
	gen out-data @ free-vct
	gen vct-buffer-free
    then
;
: wave-train= { g1 g2 -- f }
    g1 ?wave-train g2 ?wave-train and if
	g1 g2 oscil-equal
	g1 vct-buffer @ g2 vct-buffer @ vct= and
	g1 out-data @ g2 out-data @ vct= and
	g1 interpolation-type @ g2 interpolation-type @ = and
    else
	false
    then
;
: $(.wave-train) { gen -- str }
    $" #<" str-wave-train $+
    $"  freq: " $+ gen wt-frequency@ 3 $(f.r) $+
    $" Hz, phase: " $+ gen wt-phase@ 3 $(f.r) $+
    $" , size: " $+ gen length@ $(.) $+
    $" , interp: " $+ gen interpolation-type @ $(.interp-type) $+
    $" >" $+
;
' $(.wave-train) make-inspect .wave-train
: wave-train { f: fm gen -- r }
    gen out-data @ { data }
    data length { size }
    fm two-pi mus-srate@ f/ f/ to fm
    gen out-pos @ size < if gen out-pos @ data vct@ else 0e then \ result
    1 gen out-pos +!
    gen out-pos @ s>f gen next-time sf@ f>= if
	gen out-pos @ size < if
	    gen out-pos @ { pos }
	    size pos - { samps }
	    data data-ptr pos sfloats +  data data-ptr  samps sfloats  move
	    data data-ptr samps sfloats + pos sfloats erase
	else
	    data data-ptr size sfloats erase
	then
	gen buffer-length @ 0 do
	    i data vct@  gen phase sf@ i s>f f+  gen vct-buffer @  gen interpolation-type @
	    tb-lookup-interp  f+  i data vct!
	loop
	gen first-time @ if
	    false gen first-time !
	    gen phase sf@ f>s gen out-pos !
	    fdrop			\ old result
	    gen out-pos @ data vct@	\ result
	    1 gen out-pos +!
	    mus-srate@  gen freq sf@ fm f+  f/  gen next-time sf!
	else
	    gen next-time sf@  mus-srate@  gen freq sf@ fm f+  f/  gen out-pos @ s>f  f-  f+
	    gen next-time sf!
	    0 gen out-pos !
	then
    then
;
: wave-train-1 { gen -- r }
    gen out-data @ { data }
    data length { size }
    gen out-pos @ size < if gen out-pos @ data vct@ else 0e then \ result
    1 gen out-pos +!
    gen out-pos @ s>f gen next-time sf@ f>= if
	gen out-pos @ size < if
	    gen out-pos @ { pos }
	    size pos - { samps }
	    data data-ptr pos sfloats +  data data-ptr  samps sfloats  move
	    data data-ptr samps sfloats + pos sfloats erase
	else
	    data data-ptr size sfloats erase
	then
	gen buffer-length @ 0 do
	    i data vct@  gen phase sf@ i s>f f+  gen vct-buffer @  gen interpolation-type @
	    tb-lookup-interp  f+  i data vct!
	loop
	gen first-time @ if
	    false gen first-time !
	    gen phase sf@ f>s gen out-pos !
	    fdrop			\ old result
	    gen out-pos @ data vct@	\ result
	    1 gen out-pos +!
	    mus-srate@  gen freq sf@ f/  gen next-time sf!
	else
	    gen next-time sf@  mus-srate@  gen freq sf@ f/  gen out-pos @ s>f  f-  f+
	    gen next-time sf!
	    0 gen out-pos !
	then
    then
;
: wave-train-run ( r1 r2 gen -- r3 ) fdrop wave-train ;
\ 440e  0e  *table-size* make-vct  mus-interp-linear clm-make-wave-train value gen
: clm-make-wave-train { f: frq f: ph wav tp -- gen }
    wav length dup s>f { len f: flen }
    wave-train% %alloc { gen }
    wav vct-copy         gen vct-buffer !
    len                  gen buffer-length !
    frq                  gen freq sf!
    flen ph f* two-pi f/ gen phase sf!
    tp                   gen interpolation-type !
    len 2 + make-vct     gen out-data !
    len 2 +              gen out-pos !
    true                 gen first-time !
    0e                   gen next-time sf!
    ['] wt-frequency@    gen get-freq !
    ['] wt-frequency!    gen set-freq !
    ['] wt-phase@        gen get-phase !
    ['] wt-phase!        gen set-phase !
    ['] wave-train=      gen gfm-base-equal !
    ['] .wave-train      gen gfm-base-inspect !
    ['] $(.wave-train)   gen gfm-base-to-string !
    ['] wave-train-run   gen gfm-base-run !
    ['] wt-free          gen gfm-base-free !
    str-wave-train       gen gfm-base-name !
    str-mus-gen          gen gfm-generator !
    gen
;

\ === SSB-AM ===
simp-flt%
    cell% field shift-up
    cell% field sin-osc
    cell% field cos-osc
    cell% field ssb-dly
    cell% field ssb-hilbert
end-struct ssb-am%

: ssb-am-frequency@ ( gen -- r ) sin-osc @ gfm-frequency@ ;
: ssb-am-frequency! ( val gen -- ) fdup dup sin-osc @ gfm-frequency! cos-osc @ gfm-frequency! ;
: ssb-am-phase@ ( gen -- r ) sin-osc @ gfm-phase@ ;
: ssb-am-phase! ( val gen -- ) fdup dup sin-osc @ gfm-phase! half-pi f+ cos-osc @ gfm-phase! ;

str-ssb-am make-?obj ?ssb-am
: ssb-am-free { gen -- }
    gen ?ssb-am if
	gen sin-osc @ mus-free
	gen cos-osc @ mus-free
	gen ssb-dly @ mus-free
	gen ssb-hilbert @ mus-free
	gen generator-free
    then
;
: ssb-am= { g1 g2 -- f }
    g1 ?ssb-am g2 ?ssb-am and if
	g1 g2 =
	g1 sin-osc @ g2 sin-osc @ oscil=
	g1 cos-osc @ g2 cos-osc @ oscil= and
	g1 ssb-dly @ g2 ssb-dly @ delay= and
	g1 ssb-hilbert @ g2 ssb-hilbert @ fir-filter= and
	g1 shift-up @ g2 shift-up @ = and or
    else
	false
    then
;
: $(.ssb-am) { gen -- str }
    $" #<" str-ssb-am $+
    gen sin-osc @ $(.pp-print) $+
    $" , order: " $+ gen order@ $(.) $+
    $" >" $+
;
' $(.ssb-am) make-inspect .ssb-am
: ssb-am { f: insig f: fm gen -- r }
    gen shift-up @ if
	fm gen cos-osc @ oscil-1  insig gen ssb-dly @ delay-1  f*
	fm gen sin-osc @ oscil-1  insig gen ssb-hilbert @ fir-filter  f*  f-
    else
	fm gen cos-osc @ oscil-1  insig gen ssb-dly @ delay-1  f*
	fm gen sin-osc @ oscil-1  insig gen ssb-hilbert @ fir-filter  f*  f+
    then
;
: ssb-am-1 { f: insig gen -- r }
    gen shift-up @ if
	gen cos-osc @ oscil-0  insig gen ssb-dly @ delay-1  f*
	gen sin-osc @ oscil-0  insig gen ssb-hilbert @ fir-filter  f*  f-
    else
	gen cos-osc @ oscil-0  insig gen ssb-dly @ delay-1  f*
	gen sin-osc @ oscil-0  insig gen ssb-hilbert @ fir-filter  f*  f+
    then
;
\ 440e 40 clm-make-ssb-am value gen
: clm-make-ssb-am { f: frq ord -- gen }
    ord 2* { len }
    len make-vct { xcofs }
    0 { idx }
    0e 0e { f: denom f: num }
    ord ord negate do
	i s>f pi f* to denom
	1e denom fcos f- to num
	i unless
	    0e
	else
	    num denom f/  0.54e  0.46e  denom ord s>f f/ fcos  f*  f+  f*
	then idx xcofs vct!  idx 1+ to idx
    loop
    ssb-am% %alloc { gen }
    frq f0>                         gen shift-up !
    frq fabs      0e clm-make-oscil gen sin-osc !
    frq fabs half-pi clm-make-oscil gen cos-osc !
    ord ord make-vct mus-interp-none clm-make-delay gen ssb-dly !
    len xcofs clm-make-fir-filter   gen ssb-hilbert !
    xcofs free-vct
    gen ssb-dly @ vct-buffer @      gen vct-buffer !
    gen ssb-dly @ interpolation-type @ gen interpolation-type !
    ord                             gen buffer-length !
    1                               gen cosines !
    ['] ssb-am-frequency@           gen get-freq !
    ['] ssb-am-frequency!           gen set-freq !
    ['] ssb-am-phase@               gen get-phase !
    ['] ssb-am-phase!               gen set-phase !
    ['] ssb-am=                     gen gfm-base-equal !
    ['] .ssb-am                     gen gfm-base-inspect !
    ['] $(.ssb-am)                  gen gfm-base-to-string !
    ['] ssb-am                      gen gfm-base-run !
    ['] ssb-am-free                 gen gfm-base-free !
    str-ssb-am                      gen gfm-base-name !
    str-mus-gen                     gen gfm-generator !
    gen
;

\ === Snd-IO ===
\ Sun/NeXT
08 constant size-loc
12 constant type-loc
16 constant srate-loc
20 constant chans-loc
28 constant sun-comment-loc-begin

01 constant gfm-raw-mulaw
02 constant gfm-raw-byte
03 constant gfm-raw-bshort
04 constant gfm-raw-b24int
05 constant gfm-raw-bint
06 constant gfm-raw-bfloat
07 constant gfm-raw-bdouble
27 constant gfm-raw-alaw
30 constant gfm-raw-lint
31 constant gfm-raw-lfloat
32 constant gfm-raw-bintn
33 constant gfm-raw-lintn
34 constant gfm-raw-ldouble
35 constant gfm-raw-ulshort
36 constant gfm-raw-ubshort
37 constant gfm-raw-lfloat-unscaled
38 constant gfm-raw-bfloat-unscaled
39 constant gfm-raw-ldouble-unscaled
40 constant gfm-raw-bdouble-unscaled
41 constant gfm-raw-lshort
42 constant gfm-raw-l24int
43 constant gfm-raw-ubyte

music5%
    cell% field io-file-id
    cell% field io-chans
    cell% field io-srate
    cell% field io-sample
    cell% field io-frame
    cell% field io-header-type
    cell% field io-data-format
    cell% field io-snd-data-loc
    cell% field io-current-index
    cell% field io-last-index
    cell% field buf-beg
    cell% field buf-end
    cell% field io-file-buffer
    cell% field io-buffer-length
    cell% field ?io-gen
    cell% field ?io-input
    cell% field ?io-output
    cell% field io-file-name
    cell% field io-comment
    cell% field io-reader
    cell% field io-writer
    cell% field io-writer+
end-struct snd-io%

: ?iogen  ( obj -- f ) try ?io-gen @ str-io-gen string= recover 2drop false endtry ;
: ?input  ( obj -- f ) try ?io-input @ str-io-input string= recover 2drop false endtry ;
: ?output ( obj -- f ) try ?io-output @ str-io-output string= recover 2drop false endtry ;

: file-name@ ( gen -- fname ) io-file-name @ ;
: channels@  ( gen -- n ) io-chans @ ;

-32124 -31100 -30076 -29052 -28028 -27004 -25980 -24956 -23932 -22908 -21884 -20860 
-19836 -18812 -17788 -16764 -15996 -15484 -14972 -14460 -13948 -13436 -12924 -12412 
-11900 -11388 -10876 -10364 -9852 -9340 -8828 -8316 -7932 -7676 -7420 -7164 -6908 
-6652 -6396 -6140 -5884 -5628 -5372 -5116 -4860 -4604 -4348 -4092 -3900 -3772 -3644 
-3516 -3388 -3260 -3132 -3004 -2876 -2748 -2620 -2492 -2364 -2236 -2108 -1980 -1884 
-1820 -1756 -1692 -1628 -1564 -1500 -1436 -1372 -1308 -1244 -1180 -1116 -1052 -988 
-924 -876 -844 -812 -780 -748 -716 -684 -652 -620 -588 -556 -524 -492 -460 -428 
-396 -372 -356 -340 -324 -308 -292 -276 -260 -244 -228 -212 -196 -180 -164 -148 
-132 -120 -112 -104 -96 -88 -80 -72 -64 -56 -48 -40 -32 -24 -16 -8 0 32124 31100 
30076 29052 28028 27004 25980 24956 23932 22908 21884 20860 19836 18812 17788 16764 
15996 15484 14972 14460 13948 13436 12924 12412 11900 11388 10876 10364 9852 9340 
8828 8316 7932 7676 7420 7164 6908 6652 6396 6140 5884 5628 5372 5116 4860 4604 
4348 4092 3900 3772 3644 3516 3388 3260 3132 3004 2876 2748 2620 2492 2364 2236 
2108 1980 1884 1820 1756 1692 1628 1564 1500 1436 1372 1308 1244 1180 1116 1052 
988 924 876 844 812 780 748 716 684 652 620 588 556 524 492 460 428 396 372 
356 340 324 308 292 276 260 244 228 212 196 180 164 148 132 120 112 104 96 
88 80 72 64 56 48 40 32 24 16 8 0
256 >array value int-mulaw
256 make-vct constant mulaw
int-mulaw [each] s>f 32768e f/ [i] mulaw vct! [end-each]
int-mulaw free-array nil to int-mulaw

-5504 -5248 -6016 -5760 -4480 -4224 -4992 -4736 -7552 -7296 -8064 -7808 -6528 -6272 -7040 -6784 
-2752 -2624 -3008 -2880 -2240 -2112 -2496 -2368 -3776 -3648 -4032 -3904 -3264 -3136 -3520 -3392 
-22016 -20992 -24064 -23040 -17920 -16896 -19968 -18944 -30208 -29184 -32256 -31232 -26112 -25088
-28160 -27136 -11008 -10496 -12032 -11520 -8960 -8448 -9984 -9472 -15104 -14592 -16128 -15616
-13056 -12544 -14080 -13568
-344 -328 -376 -360 -280 -264 -312 -296 -472 -456 -504 -488 -408 -392 -440 -424 
-88 -72 -120 -104 -24 -8 -56 -40 -216 -200 -248 -232 -152 -136 -184 -168 
-1376 -1312 -1504 -1440 -1120 -1056 -1248 -1184 -1888 -1824 -2016 -1952 -1632 -1568 -1760 -1696 
-688 -656 -752 -720 -560 -528 -624 -592 -944 -912 -1008 -976 -816 -784 -880 -848 
5504 5248 6016 5760 4480 4224 4992 4736 7552 7296 8064 7808 6528 6272 7040 6784 
2752 2624 3008 2880 2240 2112 2496 2368 3776 3648 4032 3904 3264 3136 3520 3392 
22016 20992 24064 23040 17920 16896 19968 18944 30208 29184 32256 31232 26112 25088 28160 27136 
11008 10496 12032 11520 8960 8448 9984 9472 15104 14592 16128 15616 13056 12544 14080 13568 
344 328 376 360 280 264 312 296 472 456 504 488 408 392 440 424 
88 72 120 104 24 8 56 40 216 200 248 232 152 136 184 168 
1376 1312 1504 1440 1120 1056 1248 1184 1888 1824 2016 1952 1632 1568 1760 1696 
688 656 752 720 560 528 624 592 944 912 1008 976 816 784 880 848
256 >array value int-alaw
256 make-vct constant alaw
int-alaw [each] s>f 32768e f/ [i] alaw vct! [end-each]
int-alaw free-array nil to int-alaw

\ headers writing functions
: (int>4bytes) ( val -- c1 c2 c3 c4 )
    dup >r $ff and r@ 8 rshift $ff and r@ 16 rshift $ff and r> 24 rshift $ff and
;
: write-bint ( val fd -- ) { fd } (int>4bytes) 4 0 do fd emit-file throw loop ;
: bshorts>int ( addr -- u )
    dup c@ 24 lshift over 1+ c@ 16 lshift or over 2 + c@ 8 lshift or swap 3 + c@ or
;

\ Read and write functions assume little endian machine!

1  7 lshift s>f fconstant float<=>byte
1 15 lshift s>f fconstant float<=>short
1 23 lshift s>f fconstant float<=>int
1 23 lshift s>f fconstant float<=>int24
1 15 lshift s>f fconstant sample-unscaled
1  7 lshift constant byte-max
1  8 lshift constant byte-minus
1 15 lshift constant short-max
1 16 lshift constant short-minus
1 23 lshift constant int-max
1 24 lshift constant int-minus

: (correct-byte)   ( c -- c' ) dup byte-max  >= if byte-minus -  then ;
: (correct-ubyte)  ( c -- c' ) dup byte-max  >= if byte-max -    then ;
: (correct-short)  ( s -- s' ) dup short-max >= if short-minus - then ;
: (correct-ushort) ( s -- s' ) dup short-max >= if short-max -   then ;
: (correct-int)    ( n -- n' ) dup int-max   >= if int-minus -   then ;
: float->byte   ( r -- c ) float<=>byte f* f>s ;
: byte->float   ( c -- r ) s>f float<=>byte f/ ;
: float->short  ( r -- s ) float<=>short f* f>s ;
: short->float  ( s -- r ) s>f float<=>short f/ ;
: float->int    ( r -- n ) float<=>int f* f>s ;
: int->float    ( n -- r ) s>f float<=>int f/ ;
: float->int24  ( r -- n ) float<=>int24 f* f>s ;
: int24->float  ( n -- r ) s>f float<=>int24 f/ ;

\ MUS-MULAW, MUS-ALAW (read only)
: mulaw>float@  ( addr -- r ) c@ mulaw vct@ ;
: alaw>float@   ( addr -- r ) c@ alaw vct@ ;
\ MUS-BYTE
: byte>float@   ( addr -- r )   c@ (correct-byte) byte->float ;
: float>byte!   ( val addr -- ) float->byte swap c! ;
: float>byte+!  ( val addr -- ) dup byte>float@ f+ float>byte! ;
\ MUS-UBYTE
: ubyte>float@  ( addr -- r )   c@ byte-max - byte->float ;
: float>ubyte!  ( val addr -- ) float->byte byte-max + swap c! ;
: float>ubyte+! ( val addr -- ) dup c@ (correct-ubyte) float->byte byte-max + + swap c! ;
\ MUS-LSHORT
: lshort>float@ { addr -- r }
    0 { w^ result }
    addr result short move
    result @ (correct-short) short->float
;
: float>lshort! { f: val addr -- }
    val float->short { w^ samp }
    samp addr short move
;
: float>lshort+! ( val addr -- ) dup lshort>float@ f+ float>lshort! ;
\ MUS-BSHORT
: bshort>float@ { addr -- r }
    0 { w^ result }
    addr 1 chars + c@ result           c!
    addr           c@ result 1 chars + c!
    result @ (correct-short) short->float
;
: float>bshort! { f: val addr -- }
    val float->short { w^ samp }
    samp 1 chars + c@ addr           c!
    samp           c@ addr 1 chars + c!
;
: float>bshort+! ( val addr -- ) dup bshort>float@ f+ float>bshort! ;
\ MUS-ULSHORT
: ulshort>float@ { addr -- r }
    0 { w^ result }
    addr result short move
    result @ short-max - short->float
;
: float>ulshort! { f: val addr -- }
    val float->short short-max + { w^ samp }
    samp addr short move
;
: float>ulshort+! { f: val addr -- }
    0 { w^ result }
    addr result short move result @ (correct-ushort) val float->short short-max + + { w^ samp }
    samp addr short move
;
\ MUS-UBSHORT
: ubshort>float@ { addr -- r }
    0 { w^ result }
    addr 1 chars + c@ result           c!
    addr           c@ result 1 chars + c!
    result @ short-max - short->float
;
: float>ubshort! { f: val addr -- }
    val float->short short-max + { w^ samp }
    samp 1 chars + c@ addr           c!
    samp           c@ addr 1 chars + c!
;
: float>ubshort+! { f: val addr -- }
    0 { w^ result }
    addr 1 chars + c@ result           c!
    addr           c@ result 1 chars + c!
    result @ (correct-ushort) val float->short short-max + + { w^ samp }
    samp 1 chars + c@ addr           c!
    samp           c@ addr 1 chars + c!
;
\ MUS-LINT
: lint>float@ ( addr -- r ) @ int->float ;
: float>lint! ( val addr -- ) float->int swap ! ;
: float>lint+! ( val addr -- ) dup lint>float@ f+ float>lint! ;
\ MUS-BINT
: bint>float@ { addr -- r }
    0 { w^ result }
    addr 3 chars + c@ result           c!
    addr 2 chars + c@ result 1 chars + c!
    addr 1 chars + c@ result 2 chars + c!
    addr           c@ result 3 chars + c!
    result @ int->float
;
: float>bint! { f: val addr -- }
    val float->int { w^ result }
    result 3 chars + c@ addr           c!
    result 2 chars + c@ addr 1 chars + c!
    result 1 chars + c@ addr 2 chars + c!
    result           c@ addr 3 chars + c!
;
: float>bint+! ( val addr -- ) dup bint>float@ f+ float>bint! ;
\ MUS-LINTN
: lintn>float@ ( addr -- r ) @ 8 rshift (correct-int) int->float ;
: float>lintn! ( val addr -- ) float->int 8 lshift swap ! ;
: float>lintn+! ( val addr -- ) dup lintn>float@ f+ float>lintn! ;
\ MUS-BINTN
: bintn>float@ { addr -- r }
    0 { w^ result }
    addr 3 chars + c@ result           c!
    addr 2 chars + c@ result 1 chars + c!
    addr 1 chars + c@ result 2 chars + c!
    addr           c@ result 3 chars + c!
    result @ 8 rshift (correct-int) int->float
;
: float>bintn! { f: val addr -- }
    val float->int 8 lshift { w^ result }
    result 3 chars + c@ addr           c!
    result 2 chars + c@ addr 1 chars + c!
    result 1 chars + c@ addr 2 chars + c!
    result           c@ addr 3 chars + c!
;
: float>bintn+! ( val addr -- ) dup bintn>float@ f+ float>bintn! ;
\ MUS-L24INT
: l24int>float@ { addr -- r }
    addr 2 chars + c@ 24 lshift
    addr 1 chars + c@ 16 lshift +
    addr           c@  8 lshift +
    8 rshift (correct-int) int24->float
;
: float>l24int! { f: val addr -- }
    val float->int24 { result }
    result 16 rshift addr 2 chars + c!
    result  8 rshift addr 1 chars + c!
    result $ff and   addr           c!
;
: float>l24int+! ( f: val addr -- ) dup l24int>float@ f+ float>l24int! ;
\ MUS-B24INT
: b24int>float@ { addr -- r }
    addr           c@ 24 lshift
    addr 1 chars + c@ 16 lshift +
    addr 2 chars + c@  8 lshift +
    8 rshift (correct-int) int24->float
;
: float>b24int! { f: val addr -- }
    val float->int24 { result }
    result 16 rshift addr           c!
    result  8 rshift addr 1 chars + c!
    result $ff and   addr 2 chars + c!
;
: float>b24int+! ( f: val addr -- ) dup b24int>float@ f+ float>b24int! ;
\ MUS-LFLOAT
: lfloat>float@ ( addr -- r ) sf@ ;
: float>lfloat! { f: val addr -- }
    0e { f^ result }
    val result sf!
    result addr 1 sfloats move
;
: float>lfloat+! ( f: val addr -- ) dup lfloat>float@ f+ float>lfloat! ;
\ MUS-BFLOAT
: bfloat>float@ { addr -- r }
    0e { f^ result }
    addr 3 chars + c@ result           c!
    addr 2 chars + c@ result 1 chars + c!
    addr 1 chars + c@ result 2 chars + c!
    addr           c@ result 3 chars + c!
    result sf@
;
: float>bfloat! { f: val addr -- }
    0e { f^ result }
    val result sf!
    result 3 chars + c@ addr           c!
    result 2 chars + c@ addr 1 chars + c!
    result 1 chars + c@ addr 2 chars + c!
    result           c@ addr 3 chars + c!
;
: float>bfloat+! ( f: val addr -- ) dup bfloat>float@ f+ float>bfloat! ;
\ MUS-LFLOAT-UNSCALED
: lfloat-unscaled>float@ ( addr -- r ) lfloat>float@ sample-unscaled f/ ;
: float>lfloat-unscaled! { f: val addr -- }
    0e { f^ result }
    val sample-unscaled f* result sf!
    result addr 1 sfloats move
;
: float>lfloat-unscaled+! ( f: val addr -- ) dup lfloat-unscaled>float@ f+ float>lfloat-unscaled! ;
\ MUS-BFLOAT-UNSCALED
: bfloat-unscaled>float@ ( addr -- r ) bfloat>float@ sample-unscaled f/ ;
: float>bfloat-unscaled! { f: val addr -- }
    0e { f^ result }
    val sample-unscaled f* result sf!
    result 3 chars + c@ addr           c!
    result 2 chars + c@ addr 1 chars + c!
    result 1 chars + c@ addr 2 chars + c!
    result           c@ addr 3 chars + c!
;
: float>bfloat-unscaled+! ( f: val addr -- ) dup bfloat-unscaled>float@ f+ float>bfloat-unscaled! ;
\ MUS-LDOUBLE
: ldouble>float@ ( addr -- r ) df@ ;
: float>ldouble! { f^ val addr -- } val 2@ addr 2! ;
: float>ldouble+! ( val addr -- ) dup ldouble>float@ f+ float>ldouble! ;
\ MUS-BDOUBLE
: bdouble>float@ { addr -- r }
    0e { f^ result }
    addr 7 chars + c@ result           c!
    addr 6 chars + c@ result 1 chars + c!
    addr 5 chars + c@ result 2 chars + c!
    addr 4 chars + c@ result 3 chars + c!
    addr 3 chars + c@ result 4 chars + c!
    addr 2 chars + c@ result 5 chars + c!
    addr 1 chars + c@ result 6 chars + c!
    addr           c@ result 7 chars + c!
    result df@
;
: float>bdouble! { f^ val addr -- }
    val 7 chars + c@ addr           c!
    val 6 chars + c@ addr 1 chars + c!
    val 5 chars + c@ addr 2 chars + c!
    val 4 chars + c@ addr 3 chars + c!
    val 3 chars + c@ addr 4 chars + c!
    val 2 chars + c@ addr 5 chars + c!
    val 1 chars + c@ addr 6 chars + c!
    val           c@ addr 7 chars + c!
;
: float>bdouble+! ( f: val addr -- ) dup bdouble>float@ f+ float>bdouble! ;
\ MUS-LDOUBLE-UNSCALED
: ldouble-unscaled>float@ ( addr -- r ) ldouble>float@ sample-unscaled f/ ;
: float>ldouble-unscaled! { f^ val addr -- } val df@ sample-unscaled f* val df! val 2@ addr 2! ;
: float>ldouble-unscaled+! ( val addr -- ) dup ldouble-unscaled>float@ f+ float>ldouble-unscaled! ;
\ MUS-BDOUBLE-UNSCALED
: bdouble-unscaled>float@ ( addr -- r ) bdouble>float@ sample-unscaled f/ ;
: float>bdouble-unscaled! { f^ val addr -- }
    val df@ sample-unscaled f* val df!
    val 7 chars + c@ addr           c!
    val 6 chars + c@ addr 1 chars + c!
    val 5 chars + c@ addr 2 chars + c!
    val 4 chars + c@ addr 3 chars + c!
    val 3 chars + c@ addr 4 chars + c!
    val 2 chars + c@ addr 5 chars + c!
    val 1 chars + c@ addr 6 chars + c!
    val           c@ addr 7 chars + c!
;
: float>bdouble-unscaled+! ( val addr -- ) dup bdouble-unscaled>float@ f+ float>bdouble-unscaled! ;

$" data format unknown" constant str-unknown-data-format
: io-bytes-per-sample { fmt -- bytes }
    fmt mus-ubyte =
    fmt mus-byte = or
    fmt mus-mulaw = or
    fmt mus-alaw = or if 1 chars else
	fmt mus-lshort =
	fmt mus-bshort = or
	fmt mus-ulshort = or
	fmt mus-ubshort = or if short else
	    fmt mus-b24int =
	    fmt mus-l24int = or if 3 chars else
		fmt mus-lint =
		fmt mus-bint = or
		fmt mus-bintn = or
		fmt mus-lintn = or
		fmt mus-lfloat = or
		fmt mus-bfloat = or
		fmt mus-lfloat-unscaled = or
		fmt mus-bfloat-unscaled = or if cell else
		    fmt mus-ldouble =
		    fmt mus-bdouble = or
		    fmt mus-ldouble-unscaled = or
		    fmt mus-bdouble-unscaled = or if 1 dfloats else
			str-unknown-data-format error
		    then
		then
	    then
	then
    then
;
: samples>bytes ( fmt size -- u ) swap io-bytes-per-sample * ;
: bytes>samples ( fmt size -- u ) swap io-bytes-per-sample / ;
: io-raw>mus-format ( fmt -- fmt' )
    case
	gfm-raw-mulaw  		 of mus-mulaw   	 endof
	gfm-raw-byte    	 of mus-byte    	 endof
	gfm-raw-bshort 		 of mus-bshort  	 endof
	gfm-raw-b24int 		 of mus-b24int  	 endof
	gfm-raw-bint 		 of mus-bint    	 endof
	gfm-raw-bfloat 		 of mus-bfloat  	 endof
	gfm-raw-bdouble 	 of mus-bdouble 	 endof
	gfm-raw-alaw   		 of mus-alaw    	 endof
	gfm-raw-lint 		 of mus-lint    	 endof
	gfm-raw-lfloat 		 of mus-lfloat  	 endof
	gfm-raw-bintn 		 of mus-bintn   	 endof
	gfm-raw-lintn 		 of mus-lintn   	 endof
	gfm-raw-ldouble 	 of mus-ldouble 	 endof
	gfm-raw-ulshort 	 of mus-ulshort 	 endof
	gfm-raw-ubshort 	 of mus-ubshort 	 endof
	gfm-raw-lfloat-unscaled  of mus-lfloat-unscaled  endof
	gfm-raw-bfloat-unscaled  of mus-bfloat-unscaled  endof
	gfm-raw-ldouble-unscaled of mus-ldouble-unscaled endof
	gfm-raw-bdouble-unscaled of mus-bdouble-unscaled endof
	gfm-raw-lshort 		 of mus-lshort  	 endof
	gfm-raw-l24int 		 of mus-l24int  	 endof
	gfm-raw-ubyte   	 of mus-ubyte   	 endof
	str-unknown-data-format error 0 swap
    endcase
;

: io-mus>raw-format ( fmt -- fmt' )
    case
	mus-mulaw   	     of gfm-raw-mulaw  		 endof
	mus-byte    	     of gfm-raw-byte    	 endof
	mus-bshort  	     of gfm-raw-bshort 		 endof
	mus-b24int  	     of gfm-raw-b24int 		 endof
	mus-bint    	     of gfm-raw-bint 		 endof
	mus-bfloat  	     of gfm-raw-bfloat 		 endof
	mus-bdouble 	     of gfm-raw-bdouble 	 endof
	mus-alaw    	     of gfm-raw-alaw   	 	 endof
	mus-lint    	     of gfm-raw-lint 	 	 endof
	mus-lfloat  	     of gfm-raw-lfloat 	 	 endof
	mus-bintn   	     of gfm-raw-bintn 	 	 endof
	mus-lintn   	     of gfm-raw-lintn 	 	 endof
	mus-ldouble 	     of gfm-raw-ldouble 	 endof
	mus-ulshort 	     of gfm-raw-ulshort 	 endof
	mus-ubshort 	     of gfm-raw-ubshort 	 endof
	mus-lfloat-unscaled  of gfm-raw-lfloat-unscaled  endof
	mus-bfloat-unscaled  of gfm-raw-bfloat-unscaled  endof
	mus-ldouble-unscaled of gfm-raw-ldouble-unscaled endof
	mus-bdouble-unscaled of gfm-raw-bdouble-unscaled endof
	mus-lshort  	     of gfm-raw-lshort 		 endof
	mus-l24int  	     of gfm-raw-l24int 		 endof
	mus-ubyte   	     of gfm-raw-ubyte   	 endof
	str-unknown-data-format error 0 swap
    endcase
;

: read-next-header { gen -- }
    gen io-file-id @ { fd }
    mus-next gen io-header-type !
    8192 allocate throw { buf }
    buf 8192 erase
    buf 4 fd read-file throw drop
    buf bshorts>int gen io-snd-data-loc !
    buf 4 fd read-file throw drop
    buf bshorts>int gen buffer-length !
    buf 4 fd read-file throw drop
    buf bshorts>int io-raw>mus-format gen io-data-format !
    buf 4 fd read-file throw drop
    buf bshorts>int gen io-srate !
    buf 4 fd read-file throw drop
    buf bshorts>int gen io-chans !
    buf 4 erase
    buf gen io-snd-data-loc @ cell+ sun-comment-loc-begin - fd read-file throw drop
    buf c@ if buf $c>string else nil then gen io-comment !
    buf free throw
;
: read-wave-header { gen -- }
    gen io-file-id @ { fd }
    mus-riff gen io-header-type !
    false gen io-comment !
    100 allocate throw { buf }
    buf 100 erase
    buf 100 fd read-file throw drop 0 { loc }
    100 0 do buf i + 4 s" fmt " str= if i to loc leave then 4 +loop
    buf loc + 10 + c@ gen io-chans !
    buf loc + 12 + @ gen io-srate !
    buf loc + 22 + c@ 8 = if 0 else 41 then io-raw>mus-format gen io-data-format !
    100 loc do buf i + 4 s" data" str= if i to loc leave then 4 +loop
    buf free throw
    loc 4 + gen io-snd-data-loc !
;
$" header type unknown" constant str-unknown-header-type
: read-header { gen -- }
    128 allocate throw { buf }
    buf 128 erase
    buf 4 gen io-file-id @ read-file throw drop
    buf 4 s" .snd" str= if
	gen read-next-header
    else
	buf 4 s" RIFF" str= if gen read-wave-header else str-unknown-header-type error then
    then
    buf free throw
    gen io-data-format @ io-bytes-per-sample gen io-sample !
    gen buffer-length @ unless
	gen io-file-id @ file-size throw d>s gen io-snd-data-loc @ -
	gen io-chans @ gen io-sample @ * / gen buffer-length !
    then
;

: write-next-header { gen -- }
    gen io-comment @ { comm }
    comm if comm length else 0 then sun-comment-loc-begin + cell - gen io-snd-data-loc !
    gen io-file-id @ { fd }
    s" .snd" fd write-file throw
    gen io-snd-data-loc @ fd write-bint
    0 fd write-bint
    gen io-data-format @ io-mus>raw-format fd write-bint
    gen io-srate @ fd write-bint
    gen io-chans @ fd write-bint
    comm ?string if comm string>$ fd write-file throw then
    fd flush-file throw
;

: write-header { gen -- )
    gen io-header-type @ case
	mus-next of gen write-next-header endof
	str-unknown-header-type error 0 swap
    endcase
;

\ in-any/out-any functions
: io-set-file-location { gen -- }
    gen location @ gen io-frame @ * gen io-snd-data-loc @ + s>d
    gen io-file-id @ reposition-file throw
;
: io-reread-file { gen -- }
    gen io-set-file-location
    gen io-file-buffer @ gen io-buffer-length @ erase
    gen io-file-buffer @ gen io-buffer-length @ gen io-file-id @ read-file throw drop
    gen io-set-file-location
;
: io-flush-buffer { gen -- }
    gen io-file-buffer @ gen io-last-index @ gen io-frame @ + gen io-file-id @ write-file throw
;

\ === Mus-Close/Mus-Free ===
\ mus-free function for IO objects
: io-close-and-free { gen -- }
    gen ?iogen if
	gen io-file-id @ { fd }
	gen ?output if
	    gen io-flush-buffer
	    size-loc s>d fd reposition-file throw
	    gen io-file-id @ file-size throw d>s gen io-snd-data-loc @ - gen io-frame @ /
	    fd write-bint
	then
	fd close-file throw
	gen io-file-buffer @ free throw
	false gen ?io-gen !
	false gen ?io-input !
	false gen ?io-output !
	gen generator-free
    then
;
: mus-close ( gen -- ) dup ?iogen if io-close-and-free else drop then ;

\ generic open-output
: io-open-output { fname srate chans data-format header-type comment flag -- }
    assert1( chans 0> data-format 0> and )
    file-buffer-size@ { len }
    len allocate throw dup len erase { buf }
    snd-io% %alloc { gen }
    fname                   gen io-file-name !
    chans                   gen io-chans !
    srate                   gen io-srate !
    data-format             gen io-data-format !
    header-type             gen io-header-type !
    flag if comment gen io-comment ! then
    0                       gen io-current-index !
    0                       gen io-last-index !
    len                     gen io-buffer-length !
    buf                     gen io-file-buffer !
    0                       gen buffer-length !
    fname string>$ flag if r/w create-file else r/w open-file then throw gen io-file-id !
    gen flag if write-header else read-header then
    data-format io-bytes-per-sample gen io-sample !
    chans gen io-sample @ * gen io-frame !
    0                       gen location !
    0                       gen buf-beg !
    len 1- gen io-frame @ / gen buf-end !
    nil                     gen io-reader !
    data-format case
	mus-byte    	     of ['] float>byte+!      	     ['] float>byte!             endof
	mus-ubyte   	     of ['] float>ubyte+!      	     ['] float>ubyte!            endof
	mus-lint    	     of ['] float>lint+!       	     ['] float>lint!             endof
	mus-bint    	     of ['] float>bint+!       	     ['] float>bint!             endof
	mus-lintn   	     of ['] float>lintn+!      	     ['] float>lintn!            endof
	mus-bintn   	     of ['] float>bintn+!      	     ['] float>bintn!            endof
	mus-l24int    	     of ['] float>l24int+!     	     ['] float>l24int!           endof
	mus-b24int    	     of ['] float>b24int+!     	     ['] float>b24int!           endof
	mus-lshort  	     of ['] float>lshort+!    	     ['] float>lshort!           endof
	mus-bshort  	     of ['] float>bshort+!     	     ['] float>bshort!           endof
	mus-ulshort  	     of ['] float>ulshort+!    	     ['] float>ulshort!          endof
	mus-ubshort  	     of ['] float>ubshort+!    	     ['] float>ubshort!          endof
	mus-lfloat  	     of ['] float>lfloat+!     	     ['] float>lfloat!           endof
	mus-bfloat  	     of ['] float>bfloat+!     	     ['] float>bfloat!           endof
	mus-lfloat-unscaled  of ['] float>lfloat-unscaled+!  ['] float>lfloat-unscaled!  endof
	mus-bfloat-unscaled  of ['] float>bfloat-unscaled+!  ['] float>bfloat-unscaled!  endof
	mus-ldouble 	     of ['] float>ldouble+!          ['] float>ldouble!          endof
	mus-bdouble 	     of ['] float>bdouble+!          ['] float>bdouble!          endof
	mus-ldouble-unscaled of ['] float>ldouble-unscaled+! ['] float>ldouble-unscaled! endof
	mus-bdouble-unscaled of ['] float>bdouble-unscaled+! ['] float>bdouble-unscaled! endof
	str-unknown-data-format error 0 swap
    endcase  gen io-writer !  gen io-writer+ !
    ['] gfm-location!       gen set-location !
    ['] io-close-and-free   gen gfm-base-free !
    str-io-gen              gen ?io-gen !
    false                   gen ?io-input !
    str-io-output           gen ?io-output !
    str-mus-gen             gen gfm-generator !
    gen
;

\ generic open-input
: io-open-input { fname -- }
    assert1( fname ?string )
    file-buffer-size@ { len }
    snd-io% %alloc { gen }
    fname                              gen io-file-name !
    fname string>$ r/o open-file throw gen io-file-id !
    short                              gen io-sample !
    0                                  gen io-current-index !
    gen read-header
    gen io-chans @ gen io-sample @ *   gen io-frame !
    0                                  gen location !
    0                                  gen buf-beg !
    len 1- gen io-frame @ /            gen buf-end !
    len                                gen io-buffer-length !
    len allocate throw                 gen io-file-buffer !
    gen io-reread-file
    gen io-data-format @ case
	mus-byte    	     of ['] byte>float@             endof
	mus-ubyte   	     of ['] ubyte>float@            endof
	mus-mulaw  	     of ['] mulaw>float@            endof
	mus-alaw   	     of ['] alaw>float@             endof
	mus-lshort 	     of ['] lshort>float@           endof
	mus-bshort 	     of ['] bshort>float@           endof
	mus-ulshort	     of ['] ulshort>float@          endof
	mus-ubshort	     of ['] ubshort>float@          endof
	mus-lint    	     of ['] lint>float@             endof
	mus-bint    	     of ['] bint>float@             endof
	mus-lintn   	     of ['] lintn>float@            endof
	mus-bintn   	     of ['] bintn>float@            endof
	mus-l24int    	     of ['] l24int>float@           endof
	mus-b24int    	     of ['] b24int>float@           endof
	mus-lfloat 	     of ['] lfloat>float@           endof
	mus-bfloat 	     of ['] bfloat>float@           endof
	mus-lfloat-unscaled  of ['] lfloat-unscaled>float@  endof
	mus-bfloat-unscaled  of ['] bfloat-unscaled>float@  endof
	mus-ldouble 	     of ['] ldouble>float@          endof
	mus-bdouble 	     of ['] bdouble>float@          endof
	mus-ldouble-unscaled of ['] ldouble-unscaled>float@ endof
	mus-bdouble-unscaled of ['] bdouble-unscaled>float@ endof
	str-unknown-data-format error 0 swap
    endcase                            gen io-reader !
    nil                                gen io-writer !
    nil                                gen io-writer+ !
    ['] gfm-location!                  gen set-location !
    ['] io-close-and-free              gen gfm-base-free !
    str-io-gen                         gen ?io-gen !
    str-io-input                       gen ?io-input !
    false                              gen ?io-output !
    str-mus-gen                        gen gfm-generator !
    gen
;

\ === Out-Any ===
: out-any { f: val samp chan gen -- }
    gen if
	samp gen location !
	gen io-frame @ samp * { loc }
	loc gen buf-beg @ gen buf-end @ within if
	    loc gen buf-beg @ - dup gen io-current-index !
	    gen io-last-index @ max gen io-last-index !
	else
	    gen io-flush-buffer
	    0 gen io-current-index !
	    0 gen io-last-index !
	    loc gen buf-beg !
	    loc gen io-buffer-length @ 1- gen io-frame @ / + gen buf-end !
	    gen io-reread-file
	then
	val
	gen io-file-buffer @ gen io-current-index @ + chan gen io-sample @ * +
	gen io-writer+ @ execute
    then
;
: outa ( val samp gen -- ) 0 swap out-any ;
: outb ( val samp gen -- ) 1 swap out-any ;
: outc ( val samp gen -- ) 2 swap out-any ;
: outd ( val samp gen -- ) 3 swap out-any ;

\ === Sample>File ===
str-sample>file make-?obj ?sample>file
: sample>file= { g1 g2 -- f } g1 ?sample>file g2 ?sample>file and g1 g2 = and ;
: $(.sample>file) { gen -- str }
    $" #<" gen name@ $+
    $"  file: " $+ gen file-name@ $+
    $" , channels: " $+ gen channels@ $(.) $+
    $" , srate: " $+ gen io-srate @ $(.) $+ $" >" $+
;
' $(.sample>file) make-inspect .sample>file
' out-any alias sample>file
: make-sample>file { fname chans fmt hdr commt -- gen }
    fname srate@ chans fmt hdr commt true io-open-output { gen }
    ['] sample>file=    gen gfm-base-equal !
    ['] .sample>file    gen gfm-base-inspect !
    ['] $(.sample>file) gen gfm-base-to-string !
    str-sample>file     gen gfm-base-name !
    gen
;

\ === In-Any ===
: in-any { samp chan gen -- r }
    gen if
	samp gen location !
	samp 0 gen buffer-length @ within if
	    gen io-frame @ samp * { loc }
	    loc gen buf-beg @ gen buf-end @ within if
		loc gen buf-beg @ - gen io-current-index !
	    else
		0 gen io-current-index !
		loc gen buf-beg !
		loc gen io-buffer-length @ 1- gen io-frame @ / + gen buf-end !
		gen io-reread-file
	    then
	    gen io-file-buffer @ gen io-current-index @ + chan gen io-sample @ * +
	    gen io-reader @ execute
	else
	    0e
	then
    then
;
: ina ( samp gen -- r ) 0 swap in-any ;
: inb ( samp gen -- r ) 1 swap in-any ;

\ === Readin ===
snd-io%
    cell% field rd-fill-time
    cell% field rd-channel
end-struct readin%

: rd-increment@ ( gen -- r ) rd-fill-time @ s>f ;
: rd-increment! ( val gen -- ) f>s swap rd-fill-time ! ;
: channel@      ( gen -- n ) rd-channel @ ;

str-readin make-?obj ?readin
: readin= { g1 g2 -- f }
    g1 ?readin g2 ?readin and if
	g1 g2 =
	g1 io-file-name @ g2 io-file-name @ string=
	g1 location @ g2 location @ = and
	g1 rd-channel @ g2 rd-channel @ = and
	g1 rd-fill-time @ g2 rd-fill-time @ = and or
    else
	false
    then
;
: $(.readin) { gen -- str }
    $" #<" str-readin $+
    $"  file: " $+ gen file-name@ $+
    $" , channel: " $+ gen channel@ $(.) $+
    $" , location: " $+ gen location@ $(.) $+
    $" , direction: " $+ gen rd-fill-time @ $(.) $+ $" >" $+
;
' $(.readin) make-inspect .readin
: readin { gen -- r }
    gen location @ gen rd-channel @ gen in-any
    gen rd-fill-time @ gen location +!
;
\ fname 0 0 1 clm-make-readin value gen
: clm-make-readin { fname chan start dir -- gen }
    fname io-open-input { gen }
    start             gen location !
    chan              gen rd-channel !
    dir               gen rd-fill-time !
    ['] rd-increment@ gen get-increment !
    ['] rd-increment! gen set-increment !
    ['] readin=       gen gfm-base-equal !
    ['] .readin       gen gfm-base-inspect !
    ['] $(.readin)    gen gfm-base-to-string !
    str-readin        gen gfm-base-name !
    gen 
;

\ === File>Sample ===
str-file>sample make-?obj ?file>sample
: file>sample= { g1 g2 -- f }
    g1 ?file>sample g2 ?file>sample and if
	g1 g2 =
	g1 io-file-name @ g2 io-file-name @ string= or
    else
	false
    then
;
: $(.file>sample) { gen -- str }
    $" #<" gen name@ $+
    $"  file: " $+  gen file-name@ $+
    $" , channels: " $+ gen channels@ $(.) $+
    $" , srate: " $+ gen io-srate @ $(.) $+ $" >" $+
;
' $(.file>sample) make-inspect .file>sample
' in-any alias file>sample ( samp chan gen -- r )
: file>sample-run { f: samp f: chan gen -- r3 } samp f>s chan f>s gen file>sample ;
: make-file>sample { fname -- gen }
    fname 0 0 1 clm-make-readin { gen }
    ['] file>sample=    gen gfm-base-equal !
    ['] .file>sample    gen gfm-base-inspect !
    ['] $(.file>sample) gen gfm-base-to-string !
    ['] file>sample-run gen gfm-base-run !
    str-file>sample  gen gfm-base-name !
    gen
;

\ === Frame ===
: frame@  ( idx gen -- r )   vct-buffer @ vct@ ;
: frame!  ( val idx gen -- ) vct-buffer @ vct! ;
: frame+! ( val idx gen -- ) vct-buffer @ vct+! ;

str-frame make-?obj ?frame
: frame= { g1 g2 -- f }
    g1 ?frame g2 ?frame and if
	g1 g2 =
	g1 vct-buffer @ g2 vct-buffer @ vct= or
    else
	false
    then
;
: $(.frame) { gen -- str }
    $" #<" str-frame $+
    $"  chans: " $+ gen channels@ $(.) $+
    $" , vals: " $+ gen data>vct $(.vct) $+ $" >" $+
;
' $(.frame) make-inspect .frame
: make-simple-frame { chns -- gen }
    assert1( chns 0> )
    snd-io% %alloc { gen }
    chns                gen buffer-length !
    chns                gen io-chans !
    ['] frame=          gen gfm-base-equal !
    ['] .frame          gen gfm-base-inspect !
    ['] $(.frame)       gen gfm-base-to-string !
    ['] vct-buffer-free gen gfm-base-free !
    str-frame           gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;
: make-frame { chns -- gen }
    chns make-simple-frame { gen }
    chns make-vct gen vct-buffer !
    gen
;
: >frame ( chans-values chans -- gen ) { chns }
    chns make-simple-frame { gen }
    chns >vct gen vct-buffer !
    gen
;
' data>vct alias frame>vct
' vct>data alias vct>frame
: frame+ { fr1 fr2 res -- res }
    res unless fr1 channels@ fr2 channels@ min make-frame to res then
    fr1 frame>vct vct-copy res vct>frame
    res frame>vct fr2 frame>vct vct-add!
    res
;
: frame* { fr1 fr2 res -- res }
    res unless fr1 channels@ fr2 channels@ min make-frame to res then
    fr1 frame>vct vct-copy res vct>frame
    res frame>vct fr2 frame>vct vct-multiply!
    res
;

\ === Mixer ===
: mixer@  ( in out gen -- r )   >r swap r> ( out in gen ) vct-buffer @ array@ vct@ ;
: mixer!  ( val in out gen -- ) >r swap r> ( out in gen ) vct-buffer @ array@ vct! ;
: mixer+! ( val in out gen -- ) >r swap r> ( out in gen ) vct-buffer @ array@ vct+! ;

str-mixer make-?obj ?mixer
: mixer-free { gen -- }
    gen ?mixer if
	gen vct-buffer @ free-array
	gen generator-free
    then
;
: $(.mixer-matrix) { gen -- str }
    gen channels@ { chns }
    0 make-string
    chns 0 do
	$" [" $+ chns 0 do j i gen mixer@ 3 $(f.r) $+ $space $+ loop $" \b]" $+
    loop
;
: mixer= { g1 g2 -- f }
    g1 ?mixer g2 ?mixer and if
	g1 g2 =
	g1 buffer-length @ g2 buffer-length @ =
	g1 vct-buffer @ each i g2 vct-buffer @ array@ vct= and end-each or
    else
	false
    then
;
: $(.mixer) { gen -- str }
    $" #<" str-mixer $+
    $"  chans: " $+ gen channels@ $(.) $+
    $" , vals: [" $+ gen $(.mixer-matrix) $+ $" ]>" $+
;
' $(.mixer) make-inspect .mixer
: make-simple-mixer { chns -- gen }
    assert1( chns 0> )
    snd-io% %alloc { gen }
    chns           gen buffer-length !
    chns           gen io-chans !
    ['] mixer=     gen gfm-base-equal !
    ['] .mixer     gen gfm-base-inspect !
    ['] $(.mixer)  gen gfm-base-to-string !
    ['] mixer-free gen gfm-base-free !
    str-mixer      gen gfm-base-name !
    str-mus-gen    gen gfm-generator !
    gen
;
: make-mixer { chns -- mx }
    chns make-simple-mixer { mx }
    chns 0 do chns make-vct loop chns >array mx vct-buffer !
    mx
;
: make-identity-mixer { chns -- mx }
    chns make-mixer { mx }
    chns 0 do 1e i i mx mixer! loop
    mx
;
: >mixer ( chans-in-out-values chans -- mx ) { chns }
    chns make-mixer { mx }
    -1 chns 1- -do -1 chns 1- -do j i mx mixer! 1 -loop 1 -loop
    mx
;
: mixer+ { mx1 mx2 res -- res }
    mx1 channels@ mx2 channels@ min { chns }
    res unless chns make-mixer to res then
    res channels@ chns min to chns
    chns 0 do chns 0 do j i mx1 mixer@ j i mx2 mixer@ f+ j i res mixer! loop loop
    res
;
: mixer* { mx1 mx2 res -- res }
    mx1 channels@ mx2 channels@ min { chns }
    res unless chns make-mixer to res then
    res channels@ chns min to chns
    chns 0 do
	chns 0 do 0e chns 0 do k i mx1 mixer@ i j mx2 mixer@ f* f+ loop j i res mixer! loop
    loop
    res
;
: mixer-scale { f: scl mx res -- res }
    res if
	mx channels@ res channels@ min
    else
	mx channels@ dup make-mixer to res
    then { chns }
    chns 0 do chns 0 do j i mx mixer@ scl f* j i res mixer! loop loop
    res
;

: frame>frame { fr-in fr-out mx -- fr-out }
    mx channels@ fr-in channels@ min { in-chans }
    fr-out if
	mx channels@ fr-out channels@ min
    else
	mx channels@ dup make-frame to fr-out
    then 0 do 0e in-chans 0 do i fr-in frame@ i j mx mixer@ f* f+ loop i fr-out frame! loop
    fr-out
;
: sample>frame { f: val fr-out gen -- fr-out }
    gen ?frame if
	fr-out if
	    fr-out channels@ gen channels@ min
	else
	    gen channels@ dup make-frame to fr-out
	then 0 do
	    i gen frame@ val f* i fr-out frame!
	loop
    else
	gen ?mixer if
	    fr-out if
		fr-out channels@ gen channels@ min
	    else
		gen channels@ dup make-frame to fr-out
	    then 0 do
		0 i gen mixer@ val f* i fr-out frame!
	    loop
	else
	    true abort" sample>frame: GEN neither frame nor mixer"
	then
    then
    fr-out
;
: frame>sample { fr gen -- r }
    gen ?frame if
	0e gen channels@ fr channels@ min 0 do i fr frame@ i gen frame@ f* f+ loop
    else
	gen ?mixer if
	    0e gen channels@ fr channels@ min 0 do i fr frame@ i 0 gen mixer@ f* f+ loop
	else
	    true abort" frame>sample: GEN neither frame nor mixer"
	then
    then
;

\ === File>Frame ===
str-file>frame make-?obj ?file>frame
: file>frame= { g1 g2 -- f }
    g1 ?file>frame g2 ?file>frame and if
	g1 g2 =
	g1 io-file-name @ g2 io-file-name @ string= or
    else
	false
    then
;
' $(.file>sample) alias $(.file>frame)
' $(.file>frame) make-inspect .file>frame
: file>frame { fr samp gen -- fr }
    fr unless gen channels@ make-frame to fr then
    gen channels@ 0 do samp i gen file>sample i fr frame! loop
    fr
;
: make-file>frame { fname -- gen }
    fname 0 0 1 clm-make-readin { gen }
    ['] file>frame=    gen gfm-base-equal !
    ['] .file>frame    gen gfm-base-inspect !
    ['] $(.file>frame) gen gfm-base-to-string !
    str-file>frame     gen gfm-base-name !
    gen
;

\ === Frame>File ===
str-frame>file make-?obj ?frame>file
: frame>file= { g1 g2 -- f } g1 ?frame>file g2 ?frame>file and if g1 g2 = else false then ;
' $(.sample>file) alias $(.frame>file)
' $(.frame>file) make-inspect .frame>file
: frame>file { fr samp gen -- } fr channels@ 0 do i fr frame@ samp i gen sample>file loop ;
: make-frame>file { fname chans fmt hdr -- gen }
    fname chans fmt hdr false make-sample>file { gen }
    ['] frame>file=    gen gfm-base-equal !
    ['] .frame>file    gen gfm-base-inspect !
    ['] $(.frame>file) gen gfm-base-to-string !
    str-frame>file     gen gfm-base-name !
    gen
;

\ === Mus-Mix ===
: clm-mix { outf inf out-start out-frames in-start mx envs -- }
    inf  channels@ { in-chans }
    outf channels@ { out-chans }
    in-chans  make-frame { fr-in }
    out-chans make-frame { fr-out }
    envs ?array if
	mx ?mixer if nil else out-chans in-chans max make-identity-mixer to mx true then { del-mx }
	out-frames 0 do
	    in-chans 0 do
		out-chans 0 do
		    i j envs array@ array@ dup ?env if env j i mx mixer! else drop then
		    fr-in k in-start + inf file>frame
		    fr-out mx frame>frame k out-start + outf frame>file
		loop
	    loop
	loop
	del-mx if mx mus-free then
    else
	mx ?mixer if
	    out-frames 0 do
		fr-in i in-start + inf file>frame
		fr-out mx frame>frame i out-start + outf frame>file
	    loop
	else
	    out-frames 0 do
		fr-in i in-start + inf file>frame i out-start + outf frame>file
	    loop
	then
    then
    fr-in  mus-free
    fr-out mus-free
;

\ === File>Array>, Array>File ===
: file>array { fname chan start samps -- data }
    fname make-file>sample { gen }
    samps make-vct { data }
    samps 0 do i start + chan gen file>sample i data vct! loop
    gen mus-close
    data
;
: array>file { fname data len srate chans -- }
    fname srate chans *clm-data-format* mus-next $" array>file" true io-open-output { gen }
    len chans / 0 do
	chans 0 do j chans * i + data vct@ j i gen sample>file loop
    chans +loop
    gen mus-close
;

\ === Locsig ===
snd-io%
    cell%   field loc-frame
    cell%   field rev-frame
    cell%   field loc-output
    cell%   field loc-reverb
    cell%   field rev-chans
    cell%   field loc-type
    sfloat% field loc-dist
    sfloat% field loc-rev-amount
end-struct locsig%

: locsig@        ( chan gen -- r ) loc-frame @ vct@ ;
: locsig!        ( val chan gen -- ) loc-frame @ vct! ;
: locsig-reverb@ ( chan gen -- r ) rev-frame @ vct@ ;
: locsig-reverb! ( val chan gen -- r ) rev-frame @ vct! ;

str-locsig make-?obj ?locsig
: locsig-free { gen -- }
    gen ?locsig if
	gen loc-frame @ free-vct
	gen rev-frame @ free-vct
	gen generator-free
    then
;
: locsig= { g1 g2 -- f }
    g1 ?locsig g2 ?locsig and if
	g1 g2 =
	g1 loc-frame @ g2 loc-frame @ vct=
	g1 loc-type @ g2 loc-type @ = and
	g1 io-chans @ g2 io-chans @ = and
	g1 rev-chans @ g2 rev-chans @ = and
	g1 rev-frame @ g2 rev-frame @ vct= and or
    else
	false
    then
;
: $(.locsig) { gen -- str }
    $" #<" str-locsig $+
    $"  chans: " $+ gen channels@ $(.) $+
    $" , frame: " $+ gen loc-frame @ $(.vct) $+
    gen loc-reverb @ if
	$" , rev-chans: " $+ gen rev-chans @ $(.) $+
	$" , rev-frame: " $+ gen rev-frame @ $(.vct) $+
    then
    $" , interp: " $+ gen loc-type @ $(.interp-type) $+ $" >" $+
;
' $(.locsig) make-inspect .locsig
: locsig { f: val loc gen -- }
    gen loc-output @ if
	gen io-chans @  0 do val i gen loc-frame @ vct@ f* loc i gen loc-output @ out-any loop
    then
    gen loc-reverb @ if
	gen rev-chans @ 0 do val i gen rev-frame @ vct@ f* loc i gen loc-reverb @ out-any loop
    then
;
: fill-locsig { v chans f: degree f: scaler tp -- v' }
    chans 1 = if
	scaler 0 v vct!
    else degree f0< if degree -360e f/ fceil 360e f* degree f+ to degree then
	0e 0e { f: deg f: degs_per_chan }
	chans 2 = if
	    degree 90e f> if 90e else degree f0< if 0e else degree then then to deg
	    90e to degs_per_chan
	else
	    degree 360e fmod to deg
	    360e chans s>f f/ to degs_per_chan
	then
	deg degs_per_chan f/ { f: pos }
	pos floor f>s { left }
	left 1+ { right }
	right chans = if 0 to right then
	pos left s>f f- { f: frac }
	tp mus-interp-linear = if
	    scaler 1e frac f- f* left v vct!
	    scaler frac f* right v vct!
	else
	    half-pi 0.5e frac f- f* { f: ldeg }
	    scaler 2e fsqrt f* f2/ to scaler
	    ldeg fcos { f: cc }
	    ldeg fsin { f: ss }
	    scaler cc ss f+ f* left v vct!
	    scaler cc ss f- f* right v vct!
	then
    then
    v
;
: locsig-run { f: val f: loc gen -- r3 } val loc f>s gen locsig ;
: clm-make-locsig { f: degr f: dist f: reverb chns output revput tp -- gen }
    dist 1e f> if dist 1/f else 1e then to dist
    chns to *channels*
    locsig% %alloc { gen }
    dist            gen loc-dist sf!
    chns            gen io-chans !
    chns            gen buffer-length !
    output          gen loc-output !
    tp              gen loc-type !
    chns make-vct chns degr dist tp fill-locsig gen loc-frame !
    revput if
	revput io-chans @ { rchns }
	rchns       gen rev-chans !
	revput      gen loc-reverb !
	reverb      gen loc-rev-amount sf!
	rchns make-vct rchns degr reverb dist fsqrt f* tp fill-locsig gen rev-frame !
    else
	false       gen rev-chans !
	false       gen loc-reverb !
	false       gen rev-frame !
	0e          gen loc-rev-amount sf!
    then
    ['] locsig=     gen gfm-base-equal !
    ['] .locsig     gen gfm-base-inspect !
    ['] $(.locsig)  gen gfm-base-to-string !
    ['] locsig-run  gen gfm-base-run !
    ['] locsig-free gen gfm-base-free !
    str-locsig      gen gfm-base-name !
    str-mus-gen     gen gfm-generator !
    gen
;
: move-locsig { f: degr f: dist gen -- }
    dist 1e f> if dist 1/f else 1e then to dist
    gen loc-reverb @ if
	gen rev-frame @ gen rev-chans @ degr gen loc-rev-amount sf@ dist fsqrt f* gen loc-type @
	fill-locsig drop
    then
    gen loc-frame @ gen io-chans @ degr dist gen loc-type @ fill-locsig drop
;

\ === Src, Convolve, Granulate and Phase-Vocoder ===
filter%
    cell% field cb-closure
    cell% field cb-hop
end-struct environ%

: environ@ ( gen -- closure ) cb-closure @ ;
: wrapper@ ( gen -- wrapper ) cb-closure @ ;

: make-readin-cb ( rd -- xt; dir self -- r )
    lambda-create , latestxt
  does> ( dir self -- r )
    nip @ readin
;

\ === SRC ===
environ%
    cell%   field src-lim
    cell%   field src-sinc
    cell%   field src-len
    sfloat% field src-x
    sfloat% field src-incr
end-struct src%

false value previous-sinc-table
false value previous-sinc-table-size
1000e fconstant f-sinc-density
1000   constant i-sinc-density

: fill-sinc-table { len -- w }
    previous-sinc-table-size len = if
	previous-sinc-table vct-copy
    else
	pi len s>f f/ { f: win-freq }
	pi f-sinc-density f/ { f: sinc-freq }
	sinc-freq { f: sf }
	win-freq { f: wf }
	len 1+ make-vct { sinc-table }
	1e 0 sinc-table vct!
	0e len sinc-table vct!
	len 1 do
	    0.5e wf fcos f2/ f+ sf fsin f* sf f/ i sinc-table vct!
	    sf sinc-freq f+ to sf
	    wf win-freq f+ to wf
	loop
	previous-sinc-table ?vct if previous-sinc-table free-vct then
	sinc-table vct-copy to previous-sinc-table
	len to previous-sinc-table-size
	sinc-table
    then
;

str-src make-?obj ?src
: src-free { gen -- }
    gen ?src if
	gen cb-closure @ free throw
	gen vct-buffer-free
    then
;
: src= { g1 g2 -- f } g1 ?src g2 ?src and if g1 g2 = else false then ;
: $(.src) { gen -- str }
    $" #<" str-src $+
    $"  width: " $+ gen length@ $(.) $+
    $" , x: " $+ gen src-x sf@ 3 $(f.r) $+
    $" , incr: " $+ gen gfm-increment@ 3 $(f.r) $+
    $" , sinc table len: " $+ gen src-len @ $(.) $+ $" >" $+
;
' $(.src) make-inspect .src
: clm-src { input f: sr-change gen -- r }
    gen fill-time sf@ sr-change f+ { f: srx }
    gen src-lim @ { lim }
    gen vct-buffer @ { data }
    gen src-x sf@ 1e f>= if
	gen cb-closure @ { closure }
	input ?dup-if closure input-xt ! then
	gen src-x sf@ floor fdup f>s { fsx }
	fnegate gen src-x sf+!
	fsx lim > if
	    fsx lim do closure srx f0>= if 1 else -1 then f-input-cb fdrop loop
	    lim to fsx
	then
	lim fsx - { loc }
	loc 0> if data data-ptr fsx sfloats + data data-ptr loc sfloats move then
	lim loc do closure srx f0>= if 1 else -1 then f-input-cb i data vct! loop
    then
    srx f0< if srx fnegate to srx then
    1e { f: factor }
    srx 1e f<= if f-sinc-density else srx 1/f fdup to factor f-sinc-density f* then { f: zf }
    gen src-sinc @ { sinc }
    1e gen src-x sf@ f- gen buffer-length @ s>f f- zf f* { f: x }
    0e ( sum )
    lim 0 do
	x fabs truncate { kk f: frac }
	kk gen src-len @ < if
	    kk 1+ sinc vct@ kk sinc vct@ f- frac f* kk sinc vct@ f+ i data vct@ f* f+
	then
	x zf f+ to x
    loop
    srx gen src-x sf+!
    factor f* ( factor * sum )
;
: clm-src-20 { input gen -- r }
    gen src-lim @ { lim }
    gen vct-buffer @ { data }
    gen buffer-length @ { width }
    gen src-x sf@ f0> if
	gen cb-closure @ { closure }
	input ?dup-if closure input-xt ! then
	lim 2 - { loc }
	data data-ptr 2 sfloats + data data-ptr loc sfloats move
	lim loc do closure 1 f-input-cb i data vct! loop
    else
	2e gen src-x sf!
    then
    1 width - i-sinc-density 2/ * ( xs )
    width 1- data vct@ ( sum )
    lim 0 do
	i data vct@ dup ( xs ) abs gen src-sinc @ vct@ f* f+ ( sum += ... )
	i-sinc-density + ( xs += i-sinc-density )
    2 +loop
    drop ( xs )
    f2/ ( sum / 2.0 )
;
: clm-src-05 { input gen -- r }
    gen src-lim @ { lim }
    gen vct-buffer @ { data }
    gen buffer-length @ { width }
    gen src-x sf@ 1e f>= if
	gen cb-closure @ { closure }
	input ?dup-if closure input-xt ! then
	lim 1- { loc }
	data data-ptr 1 sfloats + data data-ptr loc sfloats move
	lim loc do closure 1 f-input-cb i data vct! loop
	0e gen src-x sf!
    then
    gen src-x sf@ f0= if
	0.5e gen src-x sf!
	width 1- data vct@		\ result (if)
    else
	f-sinc-density 1 width - s>f 0.5e f- f* f>s ( xs )
	0e ( sum )
	lim 0 do
	    i data vct@ dup ( xs ) abs gen src-sinc @ vct@ f* f+ ( sum += ... )
	    i-sinc-density + ( xs += i-sinc-density )
	loop
	drop ( xs )
	0.5e gen src-x sf+!
	( sum )				\ result (else)
    then
;
: src-run ( r1 r2 gen -- r3 ) fdrop false swap clm-src ;
\ false 1e 10 clm-make-src value gen
: clm-make-src { input f: srate width -- gen }
    width srate fabs fceil f2* f>s max { wid }
    wid i-sinc-density * { len }
    src% %alloc { gen }
    input ?readin if input make-readin-cb to input then
    input false false false gen make-closure { closure }
    closure             gen cb-closure !
    0e                  gen src-x sf!
    srate               gen fill-time sf!
    wid                 gen buffer-length !
    wid 2*              gen src-lim !
    len fill-sinc-table gen src-sinc !
    len                 gen src-len !
    wid 2* 1+ make-vct  gen vct-buffer !
    ['] gfm-increment@  gen get-increment !
    ['] gfm-increment!  gen set-increment !
    ['] src=            gen gfm-base-equal !
    ['] .src            gen gfm-base-inspect !
    ['] $(.src)         gen gfm-base-to-string !
    ['] src-run         gen gfm-base-run !
    ['] src-free        gen gfm-base-free !
    str-src             gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    input if wid 2* wid 1- do closure 1 f-input-cb i gen vct-buffer @ vct! loop then
    gen
;

\ === Convolve ===
environ%
    cell% field cv-hop
    cell% field cv-ctr
    cell% field cv-datar
    cell% field cv-datai
end-struct convolve%

: convolution { rl im size -- }
    size s>f { f: fsize }
    rl im size 1 fft
    0.25e fsize f/ { f: invn }
    0 rl vct@ 0 im vct@ f* fsize f/ 0 rl vct!
    0e 0 im vct!
    size 2/ 1+ 1 do
	size i - { nn2 }
	i rl vct@ nn2 rl vct@ f+ { f: rep }
	i rl vct@ nn2 rl vct@ f- { f: rem }
	i im vct@ nn2 im vct@ f+ { f: aip }
	i im vct@ nn2 im vct@ f- { f: aim }
	rep aip f* aim rem f* f+ invn f* i rl vct!
	i rl vct@ nn2 rl vct!
	aim aip f* rep rem f* f- invn f* i im vct!
	i im vct@ fnegate nn2 im vct!
    loop
    rl im size -1 fft
;

str-convolve make-?obj ?convolve
: convolve-free { gen -- }
    gen ?convolve if
	gen cv-datar @ free-vct
	gen cv-datai @ free-vct
	gen flt-xcoeffs @ free-vct
	gen vct-buffer @ free-vct
	gen cb-closure @ free throw
	gen generator-free
    then
;
: convolve= { g1 g2 -- f } g1 ?convolve g2 ?convolve and if g1 g2 = else false then ;
: $(.convolve) { gen -- str } $" #<" str-convolve $+ $"  size: " $+ gen length@ $(.) $+ $" >" $+ ;
' $(.convolve) make-inspect .convolve
: clm-convolve { input gen -- r }
    gen cv-hop @ { hop }
    gen vct-buffer @ { data }
    gen cv-ctr @ hop >= if
	gen cb-closure @ { closure }
	input ?dup-if closure input-xt ! then
	gen cv-datar @ { rl }
	gen cv-datai @ { im }
	gen flt-xcoeffs @ { flt }
	hop { idx }
	hop 0 do
	    idx data vct@ i data vct!
	    0e idx data vct!
	    closure 1 f-input-cb i rl vct!
	    0e idx rl vct!
	    0e i im vct!
	    0e idx im vct!
	    idx 1+ to idx
	loop
	flt data-ptr im data-ptr flt length sfloats move
	rl im rl length convolution
	hop to idx
	hop 0 do
	    i rl vct@ i data vct+!
	    idx rl vct@ idx data vct!
	    idx 1+ to idx
	loop
	0 gen cv-ctr !
    then
    gen cv-ctr @ data vct@		\ result
    1 gen cv-ctr +!
;
: convolve-run ( r1 r2 gen -- r3 ) fdrop fdrop false swap clm-convolve ;
: clm-make-convolve { input filt fftsize -- gen }
    filt length { len }
    len 1- len and unless len 2* else 2e len s>f 2e flogn fceil f** f>s then fftsize max { fft1 }
    fft1 2/ { fft2 }
    convolve% %alloc { gen }
    input ?readin if input make-readin-cb to input then
    input false false false gen make-closure gen cb-closure !
    filt vct-copy         gen flt-xcoeffs !
    fft1 make-vct         gen vct-buffer !
    fft1                  gen buffer-length !
    fft1 make-vct         gen cv-datar !
    fft1 make-vct         gen cv-datai !
    fft2                  gen cv-hop !
    fft2                  gen cv-ctr !
    ['] convolve=         gen gfm-base-equal !
    ['] .convolve         gen gfm-base-inspect !
    ['] $(.convolve)      gen gfm-base-to-string !
    ['] convolve-run      gen gfm-base-run !
    ['] convolve-free     gen gfm-base-free !
    str-convolve          gen gfm-base-name !
    str-mus-gen           gen gfm-generator !
    gen
;

\ === Granulate ===
environ%
    cell% field gran-ramp
    cell% field gran-hop-in
    cell% field gran-cur-out
    cell% field gran-s20
    cell% field gran-s50
    cell% field gran-data-in
    cell% field gran-data-in-len
    cell% field gran-data-out
    cell% field gran-data-out-len
    cell% field gran-edit
    cell% field gran-first-samp
end-struct granulate%

: ramp@ ( gen -- n ) gran-ramp @ ;
: ramp! ( val gen -- ) 2dup buffer-length @ 2/ < if gran-ramp ! else 2drop then ;
: hop@  ( gen -- n ) cb-hop @ ;
: hop!  ( val gen -- ) cb-hop ! ;

: gr-frequency@ ( gen -- r ) cb-hop @ samples>seconds ;
: gr-frequency! ( val gen - ) seconds>samples swap cb-hop ! ;
: gr-increment@ ( gen -- r ) dup cb-hop @ s>f gran-hop-in @ s>f f/ ;
: gr-increment! ( val gen -- ) dup cb-hop @ s>f fswap f/ floor f>s swap gran-hop-in ! ;

str-granulate make-?obj ?granulate
: granulate-free { gen -- }
    gen ?granulate if
	gen gran-data-in @ free-vct
	gen gran-data-out @ free-vct
	gen cb-closure @ free throw
	gen vct-buffer-free
    then
;
: granulate= { g1 g2 -- f } g1 ?granulate g2 ?granulate and if g1 g2 = else false then ;
: $(.granulate) { gen -- str }
    $" #<" str-granulate $+
    $"  expansion: " $+ gen cb-hop @ s>f gen gran-hop-in @ s>f f/ 3 $(f.r) $+
    $"  (" $+ gen gran-hop-in @ $(.) $+
    $" /" $+ gen cb-hop @ $(.) $+
    $" ), scaler: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" , length: " $+ gen length@ samples>seconds 3 $(f.r) $+
    $"  secs (" $+ gen length@ $(.) $+ $"  samps)" $+
    $" , ramp: " $+ gen ramp@ samples>seconds 3 $(f.r) $+ $" >" $+
;
' $(.granulate) make-inspect .granulate
: clm-granulate { input edit gen -- r }
    \ return value
    gen location @ gen gran-data-out-len @ < if gen location @ gen gran-data-out @ vct@ else 0e then
    1 gen location +!
    gen location @ gen gran-cur-out @ >= if
	gen cb-closure @ { closure }
	input ?dup-if closure input-xt ! then
	edit  ?dup-if closure edit-xt !  then
	gen gran-data-out @     { out-data }
	gen gran-data-out-len @ { out-len }
	gen gran-data-in @      { in-data }
	gen gran-data-in-len @  { in-len }
	gen gran-cur-out @      { cur-out }
	gen gran-first-samp @ if
	    in-data length 0 ?do closure 1 f-input-cb i in-data vct! loop
	else
	    cur-out out-len >= if
		out-data vct-clear
	    else
		out-len cur-out - { good-samps }
		out-data data-ptr cur-out sfloats + out-data data-ptr good-samps sfloats move
		out-data data-ptr good-samps sfloats + cur-out sfloats erase
	    then
	    gen gran-hop-in @ { in-hop }
	    in-hop in-len > if
		in-hop in-len do closure 1 f-input-cb fdrop loop
		in-data length 0 ?do closure 1 f-input-cb i in-data vct! loop
	    else
		in-len in-hop - { good-samps }
		in-data data-ptr in-hop sfloats + in-data data-ptr good-samps sfloats move
		in-len good-samps do closure 1 f-input-cb i in-data vct! loop
	    then
	then
	gen gran-s20 @ irandom { curstart }
	gen buffer-length @ dup { grain-len } in-len curstart - min { lim }
	gen vct-buffer @ { grain-buf }
	grain-buf data-ptr gen buffer-length @ sfloats erase
	gen gran-ramp @ { rmp }
	rmp 0> if
	    0e { f: amp }
	    grain-len rmp - { steady-end }
	    gen scaler sf@ rmp s>f f/ { f: incr }
	    curstart { idx }
	    lim 0 do
		idx in-data vct@ amp f* i grain-buf vct!
		i rmp < if amp incr f+ to amp else i steady-end >= if amp incr f- to amp then then
		idx 1+ to idx
	    loop
	else
	    gen scaler sf@ 1e f= if
		in-data data-ptr curstart sfloats + grain-buf data-ptr lim sfloats move
	    else
		curstart { idx }
		lim 0 do idx in-data vct@ gen scaler sf@ f* i grain-buf vct! loop
		idx 1+ to idx
	    then
	then
	closure edit-xt @ if
	    closure f-edit-cb dup 0<= if drop grain-len else out-len min then
	else
	    grain-len
	then { new-len }
	new-len 0 do i grain-buf vct@ i out-data vct+! loop
	0 gen location !
	gen gran-s50 @ irandom gen gran-s50 @ 2/ - gen cb-hop @ + 0 max gen gran-cur-out !
	gen gran-first-samp @ if
	    false gen gran-first-samp !
	    1 gen location !
	    fdrop out-data vct-first	\ new return value
	then
    then
;
: granulate-run ( r1 r2 gen -- r3 ) fdrop fdrop false false rot clm-granulate ;
\ input-xt 1e 0.15e 0.6e 0.05e 0.4e 1e 0 edit-xt make-convolve
: clm-make-granulate { input f: expan f: len f: scl f: hop f: ramp f: jitter max-size edit -- gen }
    hop len f+ seconds>samples max-size max { out-len }
    assert1( out-len 0> )
    granulate% %alloc { gen }
    input ?readin if input make-readin-cb to input then
    input edit false false gen make-closure gen cb-closure !
    0                                gen gran-cur-out !
    0                                gen location !
    len mus-srate@ f* fceil fdup f>s gen buffer-length !
    ramp f* floor f>s                gen gran-ramp !
    scl                              gen scaler sf!
    hop seconds>samples dup          gen cb-hop !
    s>f expan f/ floor f>s           gen gran-hop-in !
    jitter hop f* mus-srate@ f* floor f>s         gen gran-s20 !
    jitter hop f* mus-srate@ f* 0.4e f* floor f>s gen gran-s50 !
    out-len gen gran-s20 @ + 1+ { in-len }
    out-len                          gen gran-data-out-len !
    out-len make-vct                 gen gran-data-out !
    in-len                           gen gran-data-in-len !
    in-len make-vct                  gen gran-data-in !
    in-len make-vct                  gen vct-buffer !
    true                             gen gran-first-samp !
    ['] gfm-scaler@                  gen get-scaler !
    ['] gfm-scaler!                  gen set-scaler !
    ['] gr-frequency@                gen get-freq !
    ['] gr-frequency!                gen set-freq !
    ['] gr-increment@                gen get-increment !
    ['] gr-increment!                gen set-increment !
    ['] gfm-location!                gen set-location !
    ['] granulate=                   gen gfm-base-equal !
    ['] .granulate                   gen gfm-base-inspect !
    ['] $(.granulate)                gen gfm-base-to-string !
    ['] granulate-run                gen gfm-base-run !
    ['] granulate-free               gen gfm-base-free !
    str-granulate                    gen gfm-base-name !
    str-mus-gen                      gen gfm-generator !
    gen
;

\ === Phase-Vocoder ===
environ%
    cell%   field pv-outctr
    cell%   field pv-filptr
    cell%   field pv-first-time
    cell%   field pv-win
    cell%   field pv-ampinc
    cell%   field pv-amps
    cell%   field pv-freqs
    cell%   field pv-phases
    cell%   field pv-phaseinc
    cell%   field pv-lastphase
    cell%   field pv-analyze
    cell%   field pv-edit
    cell%   field pv-synthesize
    sfloat% field pv-pitch
end-struct phase-vocoder%

: pv-frequency@ 	  ( gen -- r ) pv-pitch sf@ ;
: pv-frequency! 	  ( val gen -- ) pv-pitch sf! ;
: pv-outctr@ 		  ( gen -- n ) pv-outctr @ ;
: pv-outctr! 		  ( val gen -- ) pv-outctr ! ;
: pv-amp-increments@      ( idx gen -- r ) pv-ampinc @ vct@ ;
: pv-amp-increments!      ( val idx gen -- ) pv-ampinc @ vct! ;
: pv-amp-increments>vct   ( gen -- v ) pv-ampinc @ ;
: pv-amps@ 		  ( idx gen -- r ) pv-amps @ vct@ ;
: pv-amps! 		  ( val idx gen -- ) pv-amps @ vct! ;
: pv-amps>vct             ( gen -- v ) pv-amps @ ;
: pv-freqs@ 		  ( idx gen -- r ) pv-freqs @ vct@ ;
: pv-freqs! 		  ( val idx gen -- ) pv-freqs @ vct! ;
: pv-freqs>vct            ( gen -- v ) pv-freqs @ ;
: pv-phases@ 		  ( idx gen -- r ) pv-phases @ vct@ ;
: pv-phases! 		  ( val idx gen -- ) pv-phases @ vct! ;
: pv-phases>vct           ( gen -- v ) pv-phases @ ;
: pv-phase-increments@    ( idx gen -- r ) pv-phaseinc @ vct@ ;
: pv-phase-increments!    ( val idx gen -- ) pv-phaseinc @ vct! ;
: pv-phase-increments>vct ( gen -- v ) pv-phaseinc @ ;

str-phase-vocoder make-?obj ?phase-vocoder
: phase-vocoder-free { gen -- }
    gen ?phase-vocoder if
	gen pv-win @ free-vct
	gen pv-ampinc @ free-vct
	gen pv-freqs @ free-vct
	gen pv-amps @ free-vct
	gen pv-phases @ free-vct
	gen pv-lastphase @ free-vct
	gen pv-phaseinc @ free-vct
	gen cb-closure @ free throw
	gen vct-buffer-free
    then
;
: phase-vocoder= { g1 g2 -- f } g1 ?phase-vocoder g2 ?phase-vocoder and if g1 g2 = else false then ;
: $(.phase-vocoder) { gen -- str }
    $" #<" str-phase-vocoder $+
    $"  outctr: " $+ gen pv-outctr @ $(.) $+
    $" , interp: " $+ gen gfm-increment@ f>s $(.) $+
    $" , filptr: " $+ gen pv-filptr @ $(.) $+
    $" , N: " $+ gen length@ $(.) $+
    $" , D: " $+ gen cb-hop @ $(.) $+
    $" , in-data: " $+ gen data>vct $(.vct) $+ $" >" $+
;
' $(.phase-vocoder) make-inspect .phase-vocoder
: clm-phase-vocoder { input analyze edit synthesize gen -- r }
    gen cb-hop @ { D }
    gen cb-closure @ { closure }
    gen pv-outctr @ gen fill-time sf@ f>s >= if
	0 gen pv-outctr !
	gen buffer-length @ { N }
	gen pv-filptr @ { filptr }
	input ?dup-if closure input-xt ! then
	\ analyze: if no analyze function was given or it returns true
	analyze ?dup-if closure analyze-xt ! then
	closure analyze-xt @ if closure dup input-xt @ f-analyze-cb else true then if
	    gen pv-freqs @ vct-clear
	    0 gen pv-outctr !
	    gen vct-buffer @ { indat }
	    gen pv-first-time @ if
		indat length 0 ?do closure 1 f-input-cb i indat vct! loop
		false gen pv-first-time !
	    else
		N D ?do i indat vct@ i D - indat vct! loop
		N N D - ?do closure 1 f-input-cb i indat vct! loop
	    then
	    filptr N mod ( buf )
	    gen pv-win @ length 0 ?do
		i gen pv-win @ vct@ i indat vct@ f* dup gen pv-ampinc @ vct!
		1+ dup N >= if drop 0 then
	    loop
	    drop
	    D gen pv-filptr +!
	    gen pv-ampinc @ gen pv-freqs @ N 1 fft
	    gen pv-ampinc @ gen pv-freqs @ N 2/ rectangular>polar
	then
	\ edit: if no edit function was given or it returns true
	edit ?dup-if closure edit-xt ! then
	closure edit-xt @ if closure f-edit-cb else true then if
	    D s>f 1/f       { f: pscl }
	    two-pi N s>f f/ { f: kscl }
	    0e ( ks )
	    gen pv-lastphase @ length 0 ?do
		i gen pv-freqs @ vct@ i gen pv-lastphase @ vct@ f- ( phasediff )
		i gen pv-freqs @ vct@ i gen pv-lastphase @ vct!
		begin fdup pi f> while two-pi f- repeat
		begin fdup pi fnegate f< while two-pi f+ repeat
		pscl f* fover f+ gen pv-pitch sf@ f* i gen pv-freqs @ vct!
		kscl f+
	    loop
	    fdrop
	then
	\ general
	gen fill-time sf@ 1/f { f: scl }
	gen pv-amps @ length 0 ?do
	    i gen pv-ampinc @ vct@ i gen pv-amps @     vct@ f- scl f* i gen pv-ampinc @ vct!
	    i gen pv-freqs @  vct@ i gen pv-phaseinc @ vct@ f- scl f* i gen pv-freqs @  vct!
	loop
    then
    1 gen pv-outctr +!
    \ synthesize
    synthesize ?dup-if closure synthesize-xt ! then
    closure synthesize-xt @ if
	closure f-synthesize-cb
    else
	gen pv-amps @ length 0 ?do
	    i gen pv-ampinc   @ vct@ i gen pv-amps     @ vct+!
	    i gen pv-freqs    @ vct@ i gen pv-phaseinc @ vct+!
	    i gen pv-phaseinc @ vct@ i gen pv-phases   @ vct+!
	loop
	gen pv-amps @ gen pv-phases @ sine-bank
    then
;
: phase-vocoder-run ( r1 r2 gen -- r3 ) fdrop fdrop >r nil nil nil nil r> clm-phase-vocoder ;
\ false 512 4 128 1e false false false clm-make-phase-vocoder value gen
: clm-make-phase-vocoder { input fftsize overlap interp f: pitch analyze edit synthesize -- gen }
    fftsize 2 max to fftsize
    overlap 1 max to overlap
    interp  1 max to interp
    fftsize 2/ { N2 }
    mus-hamming-window fftsize 0e make-fft-window { win }
    2e 0.54e fftsize s>f f* f/ win vct-scale!
    phase-vocoder% %alloc { gen }
    input ?readin if input make-readin-cb to input then
    input edit analyze synthesize gen make-closure gen cb-closure !
    fftsize                gen buffer-length !
    interp s>f             gen fill-time sf!
    interp                 gen pv-outctr !
    fftsize overlap /      gen cb-hop !
    pitch                  gen pv-pitch sf!
    0                      gen pv-filptr !
    win                    gen pv-win !
    fftsize make-vct       gen vct-buffer !
    fftsize make-vct       gen pv-ampinc !
    fftsize make-vct       gen pv-freqs !
    N2 make-vct            gen pv-amps !
    N2 make-vct            gen pv-phases !
    N2 make-vct            gen pv-lastphase !
    N2 make-vct            gen pv-phaseinc !
    true                   gen pv-first-time !
    ['] gfm-increment@     gen get-increment !
    ['] gfm-increment!     gen set-increment !
    ['] pv-frequency@      gen get-freq !
    ['] pv-frequency!      gen set-freq !
    ['] phase-vocoder=     gen gfm-base-equal !
    ['] .phase-vocoder     gen gfm-base-inspect !
    ['] $(.phase-vocoder)  gen gfm-base-to-string !
    ['] phase-vocoder-run  gen gfm-base-run !
    ['] phase-vocoder-free gen gfm-base-free !
    str-phase-vocoder      gen gfm-base-name !
    str-mus-gen            gen gfm-generator !
    gen
;

\ === Fcomb ===
simp-flt%
    cell% field fc-delay
    cell% field fc-filter
end-struct fcomb%

str-fcomb make-?obj ?fcomb
: fcomb= { g1 g2 -- f }
    g1 ?fcomb g2 ?fcomb and if
	g1 g2 =
	g1 scaler sf@ g2 scaler sf@ f=
	g1 flt-a0 sf@ g2 flt-a0 sf@ f= and
	g1 flt-a1 sf@ g2 flt-a1 sf@ f= and or
    else
	false
    then
;
: $(.fcomb) { gen -- str }
    $" #<" str-fcomb $+
    $"  scaler: " $+ gen gfm-scaler@ 3 $(f.r) $+
    $" , a0: " $+ gen a0@ 3 $(f.r) $+
    $" , a1: " $+ gen a1@ 3 $(f.r) $+
    $" , line: " $+ gen vct-buffer @ $(.vct) $+
    $" >" $+
;
' $(.fcomb) make-inspect .fcomb
: fcomb { f: input gen -- r }
    gen location @ gen vct-buffer @ vct@ { f: tap-result }
    gen flt-a0 sf@ tap-result f* gen flt-a1 sf@ gen flt-x1 sf@ f* f+ { f: filter-result }
    tap-result gen flt-x1 sf!
    filter-result gen scaler sf@ f* input f+ gen location @ gen vct-buffer @ vct!
    1 gen location +!
    gen location @ gen buffer-length @ >= if 0 gen location ! then
    tap-result
;
: fcomb-run ( r1 r2 gen -- r3 ) fdrop fcomb ;
\ 0e 1 0e 0e clm-make-fcomb value gen
: clm-make-fcomb { f: scl size f: a0 f: a1 -- gen }
    fcomb% %alloc { gen }
    0                   gen location !
    size                gen buffer-length !
    size make-vct       gen vct-buffer !
    scl                 gen scaler sf!
    a0                  gen flt-a0 sf!
    a1                  gen flt-a1 sf!
    0e                  gen flt-x1 sf!
    ['] gfm-scaler@     gen get-scaler !
    ['] gfm-scaler!     gen set-scaler !
    ['] fcomb=          gen gfm-base-equal !
    ['] .fcomb          gen gfm-base-inspect !
    ['] $(.fcomb)       gen gfm-base-to-string !
    ['] fcomb-run       gen gfm-base-run !
    ['] vct-buffer-free gen gfm-base-free !
    str-fcomb           gen gfm-base-name !
    str-mus-gen         gen gfm-generator !
    gen
;

\ === Buffer ===
music5%
    cell% field buf-fill-time
    cell% field buf-empty
end-struct buffer%

: buffer-increment@ ( gen -- r ) buf-fill-time @ s>f ;
: buffer-increment! ( val gen -- ) f>s swap 2dup buf-fill-time ! swap 0= swap buf-empty ! ;

: ?buffer-empty ( gen -- f ) buf-empty @ ;
: ?buffer-full  ( gen -- f ) dup buf-fill-time @ over buffer-length @ >= swap location @ 0= and ;
str-buffer make-?obj ?buffer
: buffer= { g1 g2 -- f } g1 ?buffer g2 ?buffer and g1 g2 = and ;
: $(.buffer) { gen -- str }
    $" #<" str-buffer $+
    $"  loc: " $+ gen location@ $(.) $+
    $" , fill-time: " $+ gen buf-fill-time @ $(.) $+
    $" , size: " $+ gen length@ $(.) $+
    $" , empty: " $+ gen ?buffer-empty true-false-string $+
    $" >" $+
;
' $(.buffer) make-inspect .buffer
: buffer>sample { gen -- r }
    gen location @ gen buffer-length @ < if
	gen location @ gen vct-buffer @ vct@
    else
	0e
    then				\ result
    1 gen location +!
    gen location @ { loc }
    gen buf-empty @ 0= loc gen buf-fill-time @ >= and if
	gen vct-buffer @ data-ptr { data }
	gen buffer-length @ { size }
	loc size < if
	    data loc sfloats + data size loc - sfloats move
	    data loc sfloats + size loc - sfloats erase
	else
	    data size sfloats erase
	then
	gen location @ negate gen buf-fill-time +!
	0 gen location ! true gen buf-empty !
    then
;
: sample>buffer { f: val gen -- }
    gen buf-fill-time @ gen buffer-length @ >= if
	gen location @ { loc }
	assert1( loc 0> )
	gen vct-buffer @ data-ptr { data }
	gen buffer-length @ loc - sfloats { size }
	data loc sfloats + data size move
	data loc sfloats + size erase
	loc negate gen buf-fill-time +!
	0 gen location !
    then
    val gen buf-fill-time @ gen vct-buffer @ vct!
    1 gen buf-fill-time +!
;
: buffer-run ( r1 r2 gen -- r3 ) fdrop fdrop buffer>sample ;
\ *table-size* 0e make-buffer value gen
: clm-make-buffer { size ftime -- gen }
    buffer% %alloc { gen }
    ftime                 gen buf-fill-time !
    size                  gen buffer-length !
    size make-vct         gen vct-buffer !
    0                     gen location !
    ftime 0=              gen buf-empty !
    ['] buffer-increment@ gen get-increment !
    ['] buffer-increment! gen set-increment !
    ['] gfm-location!     gen set-location !
    ['] buffer=           gen gfm-base-equal !
    ['] .buffer           gen gfm-base-inspect !
    ['] $(.buffer)        gen gfm-base-to-string !
    ['] buffer-run        gen gfm-base-run !
    ['] vct-buffer-free   gen gfm-base-free !
    str-buffer            gen gfm-base-name !
    str-mus-gen           gen gfm-generator !
    gen
;

\ === Sound ===
gfm-base%
    cell%   field sf-io-gen
    cell%   field sf-file-id
    cell%   field sf-fd
    cell%   field sf-file-name
    cell%   field sf-reader
    cell%   field sf-writer
    cell%   field sf-chans
    cell%   field sf-srate
    cell%   field sf-frames
    cell%   field sf-samples
    cell%   field sf-sample
    cell%   field sf-header-type
    cell%   field sf-data-format
    cell%   field sf-length
    cell%   field sf-comment
    cell%   field sf-buffer
    cell%   field sf-maxamp
    sfloat% field sf-duration
end-struct sound-file%

: $(.data-format) ( n -- str )
    case
	mus-ubyte            of $" unsigned byte (8 bits)"                   endof
	mus-mulaw   	     of $" mulaw (8 bits)"                           endof
	mus-byte    	     of $" signed byte (8 bits)"                     endof
	mus-bshort  	     of $" big endian short (16 bits)"               endof
	mus-b24int           of $" big endian int (24 bits)"                 endof
	mus-bint             of $" big endian int (32 bits)"                 endof
	mus-bfloat  	     of $" big endian float (32 bits)"               endof
	mus-bdouble          of $" big endian double (64 bits)"              endof
	mus-alaw             of $" alaw (8 bits)"                            endof
	mus-lint             of $" little endian int (32 bits)"              endof
	mus-lfloat           of $" little endian float (32 bits)"            endof
	mus-bintn            of $" normalized big endian int (32 bits)"      endof
	mus-lintn            of $" normalized little endian int (32 bits)"   endof
	mus-ldouble          of $" little endian double (64 bits)"           endof
	mus-ulshort          of $" unsigned little endian short (16 bits)"   endof
	mus-ubshort          of $" unsigned big endian short (16 bits)"      endof
	mus-lfloat-unscaled  of $" little endian float (32 bits, unscaled)"  endof
	mus-bfloat-unscaled  of $" big endian float (32 bits, unscaled)"     endof
	mus-ldouble-unscaled of $" little endian double (64 bits, unscaled)" endof
	mus-bdouble-unscaled of $" big endian double (64 bits, unscaled)"    endof
	mus-lshort           of $" little endian short (16 bits)"            endof
	mus-l24int           of $" little endian int (24 bits)"              endof
	$" unknown data format" swap
    endcase
;
: .data-format ( n -- ) $(.data-format) .string ;
: $(.short-data-format) ( n -- str )
    case
	mus-ubyte            of $" mus-ubyte"            endof
	mus-mulaw   	     of $" mus-mulaw"            endof
	mus-byte    	     of $" mus-byte"             endof
	mus-bshort  	     of $" mus-bshort"           endof
	mus-b24int           of $" mus-b24int"           endof
	mus-bint             of $" mus-bint"             endof
	mus-bfloat  	     of $" mus-bfloat"           endof
	mus-bdouble          of $" mus-bdouble"          endof
	mus-alaw             of $" mus-alaw"             endof
	mus-lint             of $" mus-lint"             endof
	mus-lfloat           of $" mus-lfloat"           endof
	mus-bintn            of $" mus-bintn"            endof
	mus-lintn            of $" mus-lintn"            endof
	mus-ldouble          of $" mus-ldouble"          endof
	mus-ulshort          of $" mus-ulshort"          endof
	mus-ubshort          of $" mus-ubshort"          endof
	mus-lfloat-unscaled  of $" mus-lfloat-unscaled"  endof
	mus-bfloat-unscaled  of $" mus-bfloat-unscaled"  endof
	mus-ldouble-unscaled of $" mus-ldouble-unscaled" endof
	mus-bdouble-unscaled of $" mus-bdouble-unscaled" endof
	mus-lshort           of $" mus-lshort"           endof
	mus-l24int           of $" mus-l24int"           endof
	$" unknown" swap
    endcase
;
: .short-data-format ( n -- ) $(.short-data-format) .string ;
: $(.header-type) ( n -- str )
    case
	mus-next of $" Sun"  endof
	mus-riff of $" RIFF" endof
	$" unknown" swap
    endcase
;
: .header-type ( n -- ) $(.header-type) .string ;

2 chars 2 chars 2constant short%
$" sound-file" constant str-sound-file

\ /usr/include/stdio.h
struct
    cell%  field file-p
    cell%  field file-r
    cell%  field file-w
    short% field file-flags
    short% field file-fd		\ file descriptor
    \ ...
end-struct FILE%

: fileno@ ( wio -- fd ) 0 { w^ fd } file-fd fd 2 move fd @ ;

128 make-array value +clm-sound-files+

: sound-free ( gen -- ) dup io-file-buffer @ free throw free throw ;
$" invalid file descriptor" constant str-invalid-fd
: fd>sound-file { fd -- sf }
    fd +clm-sound-files+ ?range if
	fd +clm-sound-files+ array@
    else
	str-invalid-fd error
    then
;
str-sound-file make-?obj ?sound-file
: free-sound-file { fd -- }
    fd +clm-sound-files+ ?range if
	fd fd>sound-file { sf }
	sf ?sound-file if
	    sf sf-buffer @ dup ?array if free-array else drop then
	    sf sf-maxamp @ dup ?array if gen-free else drop then
	    nil sf gfm-base-name !
	    sf free throw
	then
	nil fd +clm-sound-files+ array!
    then
;
: $(.sound-file) { sf -- str }
    $" #<" str-sound-file $+
    $"  " $+ sf sf-file-name @ $+
    $" , fd: " $+ sf sf-fd @ $(.) $+
    $" , chans: " $+ sf sf-chans @ $(.) $+
    $" , srate: " $+ sf sf-srate @ $(.) $+
    $" , data format: " $+ sf sf-data-format @ $(.short-data-format) $+
    $" , bytes-per-sample: " $+ sf sf-sample @ $(.) $+ $" >" $+
;
' $(.sound-file) make-inspect .sound-file
: make-sound-file { gen -- sf }
    sound-file% %alloc { sf }
    gen io-file-id @                         sf sf-file-id !
    gen io-file-id @ fileno@                 sf sf-fd !
    gen io-file-name @                       sf sf-file-name !
    gen io-reader @                          sf sf-reader !
    gen io-writer @                          sf sf-writer !
    gen io-chans @                           sf sf-chans !
    gen io-srate @                           sf sf-srate !
    gen io-file-id @ file-size throw d>s gen io-snd-data-loc @ - gen io-frame @ / sf sf-frames !
    sf sf-frames @ sf sf-chans @ *           sf sf-samples !
    gen io-sample @                          sf sf-sample !
    gen io-header-type @                     sf sf-header-type !
    gen io-data-format @                     sf sf-data-format !
    gen io-file-id @ file-size throw d>s     sf sf-length !
    gen io-comment @                         sf sf-comment !
    nil                                      sf sf-maxamp !
    nil                                      sf sf-buffer !
    sf sf-frames @ s>f sf sf-srate @ s>f f/  sf sf-duration sf!
    ['] .sound-file                          sf gfm-base-inspect !
    ['] $(.sound-file)                       sf gfm-base-to-string !
    ['] free-sound-file                      sf gfm-base-free !
    str-sound-file                           sf gfm-base-name !
    gen sound-free
    sf
;
: gen>sound-file { gen -- sf }
    gen make-sound-file { sf }
    sf sf sf-fd @ +clm-sound-files+ array!
    sf
;
: sf>sound-file { gen sf -- sf }
    sf sf-fd @ { old-fd }
    gen io-file-id @ sf sf-file-id !
    gen io-file-id @ fileno@ sf sf-fd !
    sf sf-fd @ { new-fd }
    gen sound-free
    old-fd new-fd <> if
	sf new-fd +clm-sound-files+ array!
	old-fd +clm-sound-files+ array@ free throw
	nil old-fd +clm-sound-files+ array!
    then
    sf
;
: file-length { fname -- n }
    fname string>$ r/o open-file throw { id }
    id file-size throw d>s
    id close-file throw
;
: name>sound-file { fname -- sf }
    nil
    +clm-sound-files+ length 0 ?do
	i fd>sound-file { sf }
	sf ?sound-file if
	    sf sf-file-name @ fname string= if
		sf sf-length @ fname file-length = if
		    drop sf
		    leave
		then
	    then
	then
    loop { sf }
    sf unless
	fname make-file>sample gen>sound-file
    else
	sf sf-file-id @ unless fname make-file>sample sf sf>sound-file else sf then
    then
;
: sound-prune ( -- )
    +clm-sound-files+ each dup ?sound-file if sf-fd @ free-sound-file else drop then end-each
;

: sound-chans       ( fname -- n )   name>sound-file sf-chans @ ;
: sound-srate       ( fname -- n )   name>sound-file sf-srate @ ;
: sound-frames      ( fname -- n )   name>sound-file sf-frames @ ;
: sound-samples     ( fname -- n )   name>sound-file sf-samples @ ;
: sound-header-type ( fname -- u )   name>sound-file sf-header-type @ ;
: sound-data-format ( fname -- n )   name>sound-file sf-data-format @ ;
: sound-length      ( fname -- u )   name>sound-file sf-length @ ;
: sound-comment     ( fname -- str ) name>sound-file sf-comment @ ;
: sound-duration    ( fname -- r )   name>sound-file sf-duration sf@ ;

: sound-open-input ( fname -- fd ) name>sound-file sf-fd @ ;
: sound-open-output { fname srate chans format header comm -- fd }
    fname srate chans format header comm true io-open-output gen>sound-file sf-fd @
;

: sound-close-input { fd -- }
    fd fd>sound-file { sf }
    sf sf-file-id @ close-file throw
    nil sf sf-file-id !
;
: sound-close-output { fd bytes -- }
    fd fd>sound-file sf-file-id @ { file-id }
    size-loc s>d file-id reposition-file throw
    bytes file-id write-bint
    fd sound-close-input
;

: sound-file-read { sf buf chns len chn sd -- }
    sf sf-sample @ { samp }
    sf sf-reader @ { reader }
    chns samp *    { stp }
    chn samp * len 0 ?do dup buf + reader execute i chn sd sound-data! stp + loop drop
;
: sound-file-write { sf buf chns len chn sd -- }
    sf sf-sample @ { samp }
    sf sf-writer @ { writer }
    chns samp *    { stp }
    chn samp * len 0 ?do dup buf + i chn sd sound-data@ writer execute stp + loop drop
;

: samples>sound-data { sf buf frames sd -- }
    sd sound-data-chans { chns }
    chns 0 ?do sf buf chns frames i sd sound-file-read loop
;
: sound-data>samples { sf buf frames sd -- }
    sd sound-data-chans { chns }
    chns 0 ?do sf buf chns frames i sd sound-file-write loop
;

: sound-read { fd beg end chns sd -- n }
    fd fd>sound-file { sf }
    sf sf-sample @ { samp }
    end beg - 1+ chns samp * * { bytes }
    bytes allocate throw dup bytes erase { buf }
    buf bytes sf sf-file-id @ read-file throw
    sd sound-data-clear
    sf buf 2 pick chns samp * / sd samples>sound-data
    buf free throw
;
: sound-write { fd beg end chns sd -- }
    fd fd>sound-file { sf }
    end beg - 1+ chns sf sf-sample @ * * { bytes }
    bytes allocate throw dup bytes erase { buf }
    sf buf sd sound-data-length sd sound-data>samples
    buf bytes sf sf-file-id @ write-file throw
    sd sound-data-clear
    buf free throw
;
: sound-maxamp { fname -- vals times }
    fname ?string unless str-missing-filename error then
    fname name>sound-file { sf }
    sf sf-maxamp @ unless
	fname sound-chans { chans }
	fname sound-frames { frames }
	*clm-rt-bufsize* frames min { bufsize }
	chans bufsize make-sound-data { sd }
	fname sound-open-input { snd-fd }
	chans make-vct { vals }
	chans make-array { times }
	frames 0 do
	    snd-fd 0 bufsize 1- chans sd sound-read drop
	    chans 0 do
		bufsize 0 do
		    i j sd sound-data@ fabs j vals vct@ f> if
			i j sd sound-data@ fabs j vals vct!
			i k + j times array!
		    then
		loop
	    loop
	bufsize +loop
	snd-fd sound-close-input
	sd sound-data-free
	vals times 2 >array sf sf-maxamp !
    then
    0 sf sf-maxamp @ array@ ( vals-vct )
    1 sf sf-maxamp @ array@ ( times-array)
;

: continue-sample>file { fname -- gen }
    fname
    fname sound-srate
    fname sound-chans
    fname sound-data-format
    fname sound-header-type
    false false io-open-output { gen }
    fname sound-frames gen location !
    gen io-set-file-location
    ['] .sample>file    gen gfm-base-inspect !
    ['] $(.sample>file) gen gfm-base-to-string !
    str-sample>file     gen gfm-base-name !
    gen
;
: continue-frame>file ( fname -- gen )
    continue-sample>file { gen }
    ['] .frame>file    gen gfm-base-inspect !
    ['] $(.frame>file) gen gfm-base-to-string !
    str-frame>file     gen gfm-base-name !
    gen
;
: convolve-one-channel { file1 file2 chan1 chan2 len1 len2 fftlen -- data }
    file1 chan1 0 len1 file>array { dat1 }
    file2 chan2 0 len2 file>array { dat2 }
    fftlen make-vct { d1 }
    fftlen make-vct { d2 }
    dat1 data-ptr d1 data-ptr len1 sfloats move
    dat2 data-ptr d2 data-ptr len2 sfloats move
    dat1 free-vct
    dat2 free-vct
    d1 d2 fftlen convolution
    d2 free-vct
    d1
;

: convolve-files { fname1 fname2 f: max-amp out-file -- }
    fname1 sound-frames { len1 }
    fname2 sound-frames { len2 }
    fname1 sound-chans  { chans1 }
    fname2 sound-chans  { chans2 }
    chans1 chans2 max   { o-chns }
    2e len1 len2 + s>f 2e flogn fceil f** f>s { fftlen }
    len1 len2 + 1+ { o-len }
    o-chns 1 = if
	fname1 fname2 0 0 len1 len2 fftlen convolve-one-channel { o-dat }
	o-dat vct-peak fdup f0> if max-amp fswap f/ o-dat vct-scale! else fdrop then
	out-file o-dat o-len fname1 sound-srate 1 array>file
	o-dat free-vct
    else
	o-len o-chns * { tot-len }
	tot-len make-vct { o-dat }
	chans1 1- { chn1 }
	chans2 1- { chn2 }
	o-chns 0 do
	    fname1 fname2 chn1 i min chn2 i min len1 len2 fftlen convolve-one-channel { cur-dat }
	    0 tot-len i do dup cur-dat vct@ i o-dat vct! 1+ o-chns +loop drop
	    cur-dat free-vct
	loop
	o-dat vct-peak fdup f0> if max-amp fswap f/ o-dat vct-scale! else fdrop then
	out-file o-dat tot-len fname1 sound-srate o-chns array>file
	o-dat free-vct
    then
;

\ === Audio ===
\ The audio section of fsndlib.fs isn't very flexible and
\ reflects only my system (FreeBSD, OSS).

\ /usr/include/stdio.h
$0000 constant O_RDONLY
$0001 constant O_WRONLY
$0002 constant O_RDWR
$0004 constant O_NONBLOCK

\ The following constants are taken from my soundcard.h and you may
\ probably adjust their values.

\ /usr/include/sys/soundcard.h (version 3.0.1)
301     constant BSD_SOUND_VERSION
\ /usr/local/lib/oss/include/sys/soundcard.h (3.99.99)
$039999 constant SOUND_VERSION

$20005000 constant SNDCTL_DSP_RESET
$c0045002 constant SNDCTL_DSP_SPEED
$c0045003 constant SNDCTL_DSP_STEREO
$c0045005 constant SNDCTL_DSP_SETFMT
$c0045006 constant SNDCTL_DSP_CHANNELS
$c004500a constant SNDCTL_DSP_SETFRAGMENT
$4004500f constant SNDCTL_DSP_GETCAPS

$4004500b constant SOUND_PCM_GETFMTS
$00000100 constant DSP_CAP_DUPLEX

$00000000 constant AFMT_QUERY 
$00000001 constant AFMT_MU_LAW
$00000002 constant AFMT_A_LAW 
$00000004 constant AFMT_IMA_ADPCM
$00000008 constant AFMT_U8    
$00000010 constant AFMT_S16_LE
$00000020 constant AFMT_S16_BE
$00000040 constant AFMT_S8
$00000080 constant AFMT_U16_LE
$00000100 constant AFMT_U16_BE
$00000200 constant AFMT_MPEG
$00000400 constant AFMT_AC3

0   constant SOUND_MIXER_VOLUME
1   constant SOUND_MIXER_BASS
2   constant SOUND_MIXER_TREBLE
3   constant SOUND_MIXER_SYNTH
4   constant SOUND_MIXER_PCM
5   constant SOUND_MIXER_SPEAKER
6   constant SOUND_MIXER_LINE
7   constant SOUND_MIXER_MIC
8   constant SOUND_MIXER_CD
9   constant SOUND_MIXER_IMIX
10  constant SOUND_MIXER_ALTPCM
11  constant SOUND_MIXER_RECLEV
12  constant SOUND_MIXER_IGAIN
13  constant SOUND_MIXER_OGAIN
14  constant SOUND_MIXER_LINE1
15  constant SOUND_MIXER_LINE2
16  constant SOUND_MIXER_LINE3
17  constant SOUND_MIXER_DIGITAL1
18  constant SOUND_MIXER_DIGITAL2
19  constant SOUND_MIXER_DIGITAL3
20  constant SOUND_MIXER_PHONEIN
21  constant SOUND_MIXER_PHONEOUT
22  constant SOUND_MIXER_VIDEO
23  constant SOUND_MIXER_RADIO
24  constant SOUND_MIXER_MONITOR
25  constant SOUND_MIXER_NRDEVICES

$ff constant SOUND_MIXER_RECSRC
$fe constant SOUND_MIXER_DEVMASK
$fd constant SOUND_MIXER_RECMASK
$fc constant SOUND_MIXER_CAPS
1   constant SOUND_CAP_EXCL_INPUT
$fb constant SOUND_MIXER_STEREODEVS

1 SOUND_MIXER_VOLUME   lshift constant SOUND_MASK_VOLUME
1 SOUND_MIXER_BASS     lshift constant SOUND_MASK_BASS
1 SOUND_MIXER_TREBLE   lshift constant SOUND_MASK_TREBLE
1 SOUND_MIXER_SYNTH    lshift constant SOUND_MASK_SYNTH
1 SOUND_MIXER_PCM      lshift constant SOUND_MASK_PCM
1 SOUND_MIXER_SPEAKER  lshift constant SOUND_MASK_SPEAKER
1 SOUND_MIXER_LINE     lshift constant SOUND_MASK_LINE
1 SOUND_MIXER_MIC      lshift constant SOUND_MASK_MIC
1 SOUND_MIXER_CD       lshift constant SOUND_MASK_CD
1 SOUND_MIXER_IMIX     lshift constant SOUND_MASK_IMIX
1 SOUND_MIXER_ALTPCM   lshift constant SOUND_MASK_ALTPCM
1 SOUND_MIXER_RECLEV   lshift constant SOUND_MASK_RECLEV
1 SOUND_MIXER_IGAIN    lshift constant SOUND_MASK_IGAIN
1 SOUND_MIXER_OGAIN    lshift constant SOUND_MASK_OGAIN
1 SOUND_MIXER_LINE1    lshift constant SOUND_MASK_LINE1
1 SOUND_MIXER_LINE2    lshift constant SOUND_MASK_LINE2
1 SOUND_MIXER_LINE3    lshift constant SOUND_MASK_LINE3
1 SOUND_MIXER_DIGITAL1 lshift constant SOUND_MASK_DIGITAL1
1 SOUND_MIXER_DIGITAL2 lshift constant SOUND_MASK_DIGITAL2
1 SOUND_MIXER_DIGITAL3 lshift constant SOUND_MASK_DIGITAL3
1 SOUND_MIXER_PHONEIN  lshift constant SOUND_MASK_PHONEIN
1 SOUND_MIXER_PHONEOUT lshift constant SOUND_MASK_PHONEOUT
1 SOUND_MIXER_VIDEO    lshift constant SOUND_MASK_VIDEO
1 SOUND_MIXER_RADIO    lshift constant SOUND_MASK_RADIO
1 SOUND_MIXER_MONITOR  lshift constant SOUND_MASK_MONITOR

$40044d00 constant SOUND_MIXER_READ_VOLUME
$40044d01 constant SOUND_MIXER_READ_BASS
$40044d02 constant SOUND_MIXER_READ_TREBLE
$40044d03 constant SOUND_MIXER_READ_SYNTH
$40044d04 constant SOUND_MIXER_READ_PCM
$40044d05 constant SOUND_MIXER_READ_SPEAKER
$40044d06 constant SOUND_MIXER_READ_LINE
$40044d07 constant SOUND_MIXER_READ_MIC
$40044d08 constant SOUND_MIXER_READ_CD
$40044d09 constant SOUND_MIXER_READ_IMIX
$40044d0a constant SOUND_MIXER_READ_ALTPCM
$40044d0b constant SOUND_MIXER_READ_RECLEV
$40044d0c constant SOUND_MIXER_READ_IGAIN
$40044d0d constant SOUND_MIXER_READ_OGAIN
$40044d0e constant SOUND_MIXER_READ_LINE1
$40044d0f constant SOUND_MIXER_READ_LINE2
$40044d10 constant SOUND_MIXER_READ_LINE3
$40044d1c constant SOUND_MIXER_READ_MUTE
$40044d1d constant SOUND_MIXER_READ_ENHANCE
$40044d1e constant SOUND_MIXER_READ_LOUD
$40044dff constant SOUND_MIXER_READ_RECSRC
$40044dfe constant SOUND_MIXER_READ_DEVMASK
$40044dfd constant SOUND_MIXER_READ_RECMASK
$40044dfb constant SOUND_MIXER_READ_STEREODEVS
$40044dfc constant SOUND_MIXER_READ_CAPS

$c0044d00 constant SOUND_MIXER_WRITE_VOLUME
$c0044d01 constant SOUND_MIXER_WRITE_BASS
$c0044d02 constant SOUND_MIXER_WRITE_TREBLE
$c0044d03 constant SOUND_MIXER_WRITE_SYNTH
$c0044d04 constant SOUND_MIXER_WRITE_PCM
$c0044d05 constant SOUND_MIXER_WRITE_SPEAKER
$c0044d06 constant SOUND_MIXER_WRITE_LINE
$c0044d07 constant SOUND_MIXER_WRITE_MIC
$c0044d08 constant SOUND_MIXER_WRITE_CD
$c0044d09 constant SOUND_MIXER_WRITE_IMIX
$c0044d0a constant SOUND_MIXER_WRITE_ALTPCM
$c0044d0b constant SOUND_MIXER_WRITE_RECLEV
$c0044d0c constant SOUND_MIXER_WRITE_IGAIN
$c0044d0d constant SOUND_MIXER_WRITE_OGAIN
$c0044d0e constant SOUND_MIXER_WRITE_LINE1
$c0044d0f constant SOUND_MIXER_WRITE_LINE2
$c0044d10 constant SOUND_MIXER_WRITE_LINE3
$c0044d1c constant SOUND_MIXER_WRITE_MUTE
$c0044d1d constant SOUND_MIXER_WRITE_ENHANCE
$c0044d1e constant SOUND_MIXER_WRITE_LOUD
$c0044dff constant SOUND_MIXER_WRITE_RECSRC

require lib.fs
library libc libc.so
3 (int) libc c-open  open
1 (int) libc c-close close
3 (int) libc c-read  read
3 (int) libc c-write write
3 (int) libc c-ioctl ioctl

s" /dev/dsp0"   c-string s" /dev/dsp1"   c-string 2 >array constant audio-devices
s" /dev/mixer0" c-string s" /dev/mixer1" c-string 2 >array constant audio-mixers

: $(.audio-inspect) ( fd -- str ) fd>sound-file $(.sound-file) ;
: .audio-inspect ( fd -- ) $(.audio-inspect) .string ;

: make-audio-info { fd reader writer srate chans format frames -- }
    sound-file% %alloc { sf }
    fd      	    	       sf sf-fd !
    reader  	    	       sf sf-reader !
    writer  	    	       sf sf-writer !
    chans   	    	       sf sf-chans !
    srate   	    	       sf sf-srate !
    format          	       sf sf-data-format !
    format io-bytes-per-sample sf sf-sample !
    frames sf sf-sample @ * chans * make-array sf sf-buffer !
    ['] .audio-inspect         sf gfm-base-inspect !
    ['] $(.audio-inspect)      sf gfm-base-to-string !
    ['] free-sound-file        sf gfm-base-free !
    str-sound-file  	       sf gfm-base-name !
    sf fd +clm-sound-files+ array!
;

: .audio         ( -- ) s" audinfo" system ;
: audio-systems  ( -- n )
    0 audio-devices each O_RDONLY 0 c-open { fd } fd 0> if 1+ then fd c-close drop end-each
;
: $(.audio-system)  ( sys -- str ) audio-systems < if $" OSS" else $" no system" then ;
: .audio-system ( sys -- ) $(.audio-system) .string ; 
: $(.audio-moniker) ( -- str )
    $" OSS "
    SOUND_VERSION 361 < if
	SOUND_VERSION 0 <<# # [char] . hold # [char] . hold # #> #>> $>string $+
    else
	SOUND_VERSION 16 rshift $ff and hex $(.) $+ decimal $" ." $+
	SOUND_VERSION  8 rshift $ff and hex $(.) $+ decimal $" ." $+
	SOUND_VERSION           $ff and hex $(.) $+ decimal
    then
;
: .audio-moniker ( -- ) $(.audio-moniker) .string ;
: io-audio-mus>raw-format ( fmt -- raw-fmt )
    case
	mus-byte    of AFMT_S8     endof
	mus-ubyte   of AFMT_U8     endof
	mus-mulaw   of AFMT_MU_LAW endof
	mus-alaw    of AFMT_A_LAW  endof
	mus-lshort  of AFMT_S16_LE endof
	mus-bshort  of AFMT_S16_BE endof
	mus-ulshort of AFMT_U16_LE endof
	mus-ubshort of AFMT_U16_BE endof
	$" io-audio-mus>raw-format found unknown format" error 0 swap
    endcase	      
;
: io-audio-raw>mus-format ( raw-fmt -- fmt )
    case
	AFMT_S8     of mus-byte    endof
	AFMT_U8     of mus-ubyte   endof
	AFMT_MU_LAW of mus-mulaw   endof
	AFMT_A_LAW  of mus-alaw    endof
	AFMT_S16_LE of mus-lshort  endof
	AFMT_S16_BE of mus-bshort  endof
	AFMT_U16_LE of mus-ulshort endof
	AFMT_U16_BE of mus-ubshort endof
	$" io-audio-raw>mus-format found unknown format" error 0 swap
    endcase	      
;

: audio-open-read-device { dev -- fd }
    dev audio-system audio-devices array@ { in-dev }
    in-dev O_RDONLY 0 c-open { fd }
    fd 0< if
	in-dev O_RDONLY O_NONBLOCK or 0 c-open to fd
	fd 0< if str-audio-open-dac error then
    then
    fd
;
: audio-open-write-device { dev -- fd }
    dev audio-system audio-devices array@ { out-dev }
    out-dev O_WRONLY 0 c-open { fd }
    fd 0< if
	out-dev O_WRONLY O_NONBLOCK or 0 c-open to fd
	fd 0< if str-audio-open-dac error then
    then
    fd
;
$" cannot set FORMAT "   constant str-cant-set-format
$" cannot set STEREO "   constant str-cant-set-stereo
$" cannot set CHANNELS " constant str-cant-set-chans
$" cannot set SRATE "    constant str-cant-set-srate
$" cannot reset DEVICE"  constant str-audio-reset-device
: io-audio-open  { fd srate chns fmt -- }
    0 { w^ retval }
    fd SNDCTL_DSP_RESET retval c-ioctl if str-audio-reset-device error then
    fmt io-audio-mus>raw-format retval !
    fd SNDCTL_DSP_SETFMT retval c-ioctl if
	fd c-close drop str-cant-set-format fmt $(.device) $+ error
    then
    SOUND_VERSION 361 < if
	chns 2 = if 1 else 0 then retval !
	fd SNDCTL_DSP_STEREO retval c-ioctl if
	    fd c-close drop
	    str-cant-set-stereo chns $(.) $+ error
	then
    else
	chns retval !
	fd SNDCTL_DSP_CHANNELS retval c-ioctl if
	    fd c-close drop
	    str-cant-set-chans chns $(.) $+ error
	then
    then
    srate retval !
    fd SNDCTL_DSP_SPEED retval c-ioctl if
	fd c-close drop
	str-cant-set-srate srate $(.) $+ error
    then
;
: audio-open-input { dev srate chans fmt frames -- fd }
    assert1( srate 0> chans 0> and )
    dev audio-open-read-device { fd }
    chans 2 min to chans
    fd srate chans fmt io-audio-open
    fmt case
	mus-byte    of ['] byte>float@    endof
	mus-ubyte   of ['] ubyte>float@   endof
	mus-mulaw   of ['] mulaw>float@   endof
	mus-alaw    of ['] alaw>float@    endof
	mus-lshort  of ['] lshort>float@  endof
	mus-bshort  of ['] bshort>float@  endof
	mus-ulshort of ['] ulshort>float@ endof
	mus-ubshort of ['] ubshort>float@ endof
	str-unknown-data-format error 0 swap
    endcase
    fd swap nil srate chans fmt frames make-audio-info
    fd
;
: audio-open-output { dev srate chans fmt frames -- fd }
    assert1( srate 0> chans 0> and )
    dev audio-open-write-device { fd }
    chans 2 min to chans
    fd srate chans fmt io-audio-open
    fmt case
	mus-byte    of ['] float>byte!    endof
	mus-ubyte   of ['] float>ubyte!   endof
	mus-lshort  of ['] float>lshort!  endof
	mus-bshort  of ['] float>bshort!  endof
	mus-ulshort of ['] float>ulshort! endof
	mus-ubshort of ['] float>ubshort! endof
	str-unknown-data-format error 0 swap
    endcase
    fd nil rot srate chans fmt frames make-audio-info
    fd
;
: audio-close { fd -- }
    fd c-close 0< if str-audio-close error then
;
$" audio-read" constant str-audio-read
: audio-read { fd sd out-frames -- n }
    out-frames 0> if
	fd fd>sound-file { sf }
	sf sf-buffer @ data-ptr { buf }
	sf sf-sample @ sf sf-chans @ * { frm }
	out-frames frm * { bufsize }
	fd buf bufsize c-read dup 0< if str-audio-read error else frm / then
	sf buf 2 pick sd samples>sound-data
	buf bufsize erase
    else
	0
    then
;
$" audio-write" constant str-audio-write
: audio-write { fd sd in-frames -- n }
    in-frames 0> if
	fd fd>sound-file { sf }
	sf sf-buffer @ data-ptr { buf }
	sf buf in-frames sd sound-data>samples
	sf sf-sample @ sf sf-chans @ * { frm }
	in-frames frm * { bufsize }
	fd buf bufsize c-write dup 0< if str-audio-write error else frm / then
	buf bufsize erase
    else
	0
    then
;
: audio-open-mixer-read ( -- fd )
    0 { fd }
    audio-mixers each { dev }
	dev O_RDONLY 0 c-open to fd
	fd 0< if dev O_RDONLY O_NONBLOCK or 0 c-open to fd then
	fd 0> if leave then
    end-each
    fd 0< if str-audio-open-mixer error then
    fd
;
: audio-open-mixer-write ( -- fd )
    0 { fd }
    audio-mixers each { dev }
	dev O_WRONLY 0 c-open to fd
	fd 0< if dev O_WRONLY O_NONBLOCK or 0 c-open to fd then
	fd 0> if leave then
    end-each
    fd 0< if str-audio-open-mixer error then
    fd
;
: audio-mixer-close ( fd -- ) c-close 0< if str-audio-close-mixer error then ;

mus-byte mus-bshort mus-ubyte mus-mulaw mus-alaw mus-lshort mus-ubshort mus-ulshort
8 >array constant mus-formats
$" unknown device"        constant str-audio-mixer-device
$" mus-audio-amp failure" constant str-audio-mixer-amp
: audio-mixer-read { dev fld chan -- r vals }
    0 { w^ retval }
    audio-open-mixer-read { fd }
    mus-audio-direction make-array { vals }
    fd SOUND_MIXER_READ_DEVMASK retval c-ioctl if
	fd audio-mixer-close
	str-audio-mixer-read error
    then
    dev mus-audio-mixer = dev mus-audio-dac-filter = or if
	fd SOUND_MIXER_READ_DEVMASK retval c-ioctl drop
	0 { w^ amp }
	fld case
	    mus-audio-imix of
		SOUND_MASK_IMIX   retval @ and if fd SOUND_MIXER_READ_IMIX   amp c-ioctl drop then
	    endof
	    mus-audio-igain of
		SOUND_MASK_IGAIN  retval @ and if fd SOUND_MIXER_READ_IGAIN  amp c-ioctl drop then
	    endof
	    mus-audio-reclev of
		SOUND_MASK_RECLEV retval @ and if fd SOUND_MIXER_READ_RECLEV amp c-ioctl drop then
	    endof
	    mus-audio-pcm of
		SOUND_MASK_PCM    retval @ and if fd SOUND_MIXER_READ_PCM    amp c-ioctl drop then
	    endof
	    mus-audio-pcm2 of
		SOUND_MASK_ALTPCM retval @ and if fd SOUND_MIXER_READ_ALTPCM amp c-ioctl drop then
	    endof
	    mus-audio-ogain of
		SOUND_MASK_OGAIN  retval @ and if fd SOUND_MIXER_READ_OGAIN  amp c-ioctl drop then
	    endof
	    mus-audio-line of
		SOUND_MASK_LINE   retval @ and if fd SOUND_MIXER_READ_LINE   amp c-ioctl drop then
	    endof
	    mus-audio-microphone of
		SOUND_MASK_MIC    retval @ and if fd SOUND_MIXER_READ_MIC    amp c-ioctl drop then
	    endof
	    mus-audio-line1 of
		SOUND_MASK_LINE1  retval @ and if fd SOUND_MIXER_READ_LINE1  amp c-ioctl drop then
	    endof
	    mus-audio-line2 of
		SOUND_MASK_LINE2  retval @ and if fd SOUND_MIXER_READ_LINE2  amp c-ioctl drop then
	    endof
	    mus-audio-line3 of
		SOUND_MASK_LINE3  retval @ and if fd SOUND_MIXER_READ_LINE3  amp c-ioctl drop then
	    endof
	    mus-audio-synth of
		SOUND_MASK_SYNTH  retval @ and if fd SOUND_MIXER_READ_SYNTH  amp c-ioctl drop then
	    endof
	    mus-audio-bass of
		SOUND_MASK_BASS   retval @ and if fd SOUND_MIXER_READ_BASS   amp c-ioctl drop then
	    endof
	    mus-audio-treble of
		SOUND_MASK_TREBLE retval @ and if fd SOUND_MIXER_READ_TREBLE amp c-ioctl drop then
	    endof
	    mus-audio-cd of
		SOUND_MASK_CD     retval @ and if fd SOUND_MIXER_READ_CD     amp c-ioctl drop then
	    endof
	    mus-audio-channel of
		fd SOUND_MIXER_READ_STEREODEVS retval c-ioctl drop
		dev mus-audio-mixer = if
		    0
		    SOUND_MASK_IMIX    retval @ and if 2 + else 1+ then
		    SOUND_MASK_IGAIN   retval @ and if 2 + else 1+ then
		    SOUND_MASK_RECLEV  retval @ and if 2 + else 1+ then
		    SOUND_MASK_PCM     retval @ and if 2 + else 1+ then
		    SOUND_MASK_ALTPCM  retval @ and if 2 + else 1+ then
		    SOUND_MASK_OGAIN   retval @ and if 2 + else 1+ then
		    SOUND_MASK_LINE    retval @ and if 2 + else 1+ then
		    SOUND_MASK_MIC     retval @ and if 2 + else 1+ then
		    SOUND_MASK_LINE1   retval @ and if 2 + else 1+ then
		    SOUND_MASK_LINE2   retval @ and if 2 + else 1+ then
		    SOUND_MASK_LINE3   retval @ and if 2 + else 1+ then
		    SOUND_MASK_SYNTH   retval @ and if 2 + else 1+ then
		    SOUND_MASK_CD      retval @ and if 2 + else 1+ then
		else
		    SOUND_MASK_TREBLE  retval @ and if 2   else 0  then
		then
	    endof
	    mus-audio-format of
		fd SOUND_MIXER_READ_STEREODEVS retval c-ioctl drop
		SOUND_MASK_IMIX    retval @ and if 2 else 1 then mus-audio-imix       vals array!
		SOUND_MASK_IGAIN   retval @ and if 2 else 1 then mus-audio-igain      vals array!
		SOUND_MASK_RECLEV  retval @ and if 2 else 1 then mus-audio-reclev     vals array!
		SOUND_MASK_PCM     retval @ and if 2 else 1 then mus-audio-pcm        vals array!
		SOUND_MASK_ALTPCM  retval @ and if 2 else 1 then mus-audio-pcm2       vals array!
		SOUND_MASK_OGAIN   retval @ and if 2 else 1 then mus-audio-ogain      vals array!
		SOUND_MASK_LINE    retval @ and if 2 else 1 then mus-audio-line       vals array!
		SOUND_MASK_MIC     retval @ and if 2 else 1 then mus-audio-microphone vals array!
		SOUND_MASK_LINE1   retval @ and if 2 else 1 then mus-audio-line1      vals array!
		SOUND_MASK_LINE2   retval @ and if 2 else 1 then mus-audio-line2      vals array!
		SOUND_MASK_LINE3   retval @ and if 2 else 1 then mus-audio-line3      vals array!
		SOUND_MASK_SYNTH   retval @ and if 2 else 1 then mus-audio-synth      vals array!
		SOUND_MASK_CD      retval @ and if 2 else 1 then mus-audio-cd         vals array!
		vals length
	    endof
	endcase
	amp @ if
	    amp @ 0 vals array!
	    chan unless amp @ $ff and else amp @ $ff00 and 8 rshift then s>f 0.01e f*
	else
	    dup 0 vals array! s>f ( channels, format )
	then
    else
	fld case
	    mus-audio-port of
		1
		mus-audio-mixer over vals array!
		SOUND_MASK_MIC SOUND_MASK_LINE or SOUND_MASK_CD or retval @ and if
		    1+ mus-audio-line-in over vals array!
		then
		SOUND_MASK_SPEAKER retval @ and if 1+ mus-audio-speakers   over vals array! then
		SOUND_MASK_VOLUME  retval @ and if 1+ mus-audio-dac-out    over vals array! then
		SOUND_MASK_TREBLE  retval @ and if 1+ mus-audio-dac-filter over vals array! then
		dup 0 vals array! s>f
	    endof
	    mus-audio-format of
		fd SOUND_PCM_GETFMTS retval c-ioctl if
		    fd audio-mixer-close
		    0 audio-open-read-device to fd
		    fd SOUND_PCM_GETFMTS retval c-ioctl drop
		then
		0
		mus-formats each
		    retval @ and if 1+ i mus-formats array@ over vals array! then
		end-each dup 0 vals array! s>f
	    endof
	    mus-audio-channel of
		fd SOUND_MIXER_READ_STEREODEVS retval c-ioctl drop
		dev case
		    mus-audio-microphone of SOUND_MASK_MIC     retval @ and if 2 else 1 then endof
		    mus-audio-speakers   of SOUND_MASK_SPEAKER retval @ and if 2 else 1 then endof
		    mus-audio-line       of SOUND_MASK_LINE    retval @ and if 2 else 1 then endof
		    mus-audio-line-in    of SOUND_MASK_LINE    retval @ and if 2 else 1 then endof
		    mus-audio-line1      of SOUND_MASK_LINE1   retval @ and if 2 else 1 then endof
		    mus-audio-line2      of SOUND_MASK_LINE2   retval @ and if 2 else 1 then endof
		    mus-audio-line3      of SOUND_MASK_LINE3   retval @ and if 2 else 1 then endof
		    mus-audio-dac-out    of SOUND_MASK_VOLUME  retval @ and if 2 else 1 then endof
		    mus-audio-default    of SOUND_MASK_VOLUME  retval @ and if 2 else 1 then endof
		    mus-audio-cd         of SOUND_MASK_CD      retval @ and if 2 else 1 then endof
		    mus-audio-pcm        of SOUND_MASK_PCM     retval @ and if 2 else 1 then endof
		    mus-audio-duplex-default of
			fd SNDCTL_DSP_GETCAPS retval c-ioctl if
			    0
			else
			    retval @ DSP_CAP_DUPLEX and
			then
		    endof
		endcase dup 0 vals array! s>f
	    endof
	    mus-audio-amp of
		fd SOUND_MIXER_READ_DEVMASK retval c-ioctl drop
		dev case
		    mus-audio-microphone of
			SOUND_MASK_MIC retval @ and     if SOUND_MIXER_READ_MIC     else false then
		    endof
		    mus-audio-speakers of
			SOUND_MASK_SPEAKER retval @ and if SOUND_MIXER_READ_SPEAKER else false then
		    endof
		    mus-audio-line-in of
			SOUND_MASK_LINE retval @ and    if SOUND_MIXER_READ_LINE    else false then
		    endof
		    mus-audio-line1 of
			SOUND_MASK_LINE1 retval @ and   if SOUND_MIXER_READ_LINE1   else false then
		    endof
		    mus-audio-line2 of
			SOUND_MASK_LINE2 retval @ and   if SOUND_MIXER_READ_LINE2   else false then
		    endof
		    mus-audio-line3 of
			SOUND_MASK_LINE3 retval @ and   if SOUND_MIXER_READ_LINE3   else false then
		    endof
		    mus-audio-dac-out of
			SOUND_MASK_VOLUME retval @ and  if SOUND_MIXER_READ_VOLUME  else false then
		    endof
		    mus-audio-default of
			SOUND_MASK_VOLUME retval @ and  if SOUND_MIXER_READ_VOLUME  else false then
		    endof
		    mus-audio-cd of
			SOUND_MASK_CD retval @ and      if SOUND_MIXER_READ_CD      else false then
		    endof
		    str-audio-mixer-device warn false swap
		endcase
		?dup-if fd swap retval c-ioctl if str-audio-mixer-amp warn then then
		retval @ 0 vals array!
		chan unless retval @ $ff and else retval @ $ff00 and 8 rshift then s>f 0.01e f*
	    endof
	    mus-audio-srate of
		fd SNDCTL_DSP_SPEED retval c-ioctl if
		    fd audio-mixer-close
		    0 audio-open-read-device to fd
		then
		fd SNDCTL_DSP_SPEED retval c-ioctl drop
		retval @ dup 0 vals array! s>f
	    endof
	    mus-audio-direction of
		dev mus-audio-digital-out =
		dev mus-audio-line-out = or
		dev mus-audio-default = or
		dev mus-audio-adat-out = or
		dev mus-audio-aes-out = or
		dev mus-audio-spdif-out = or
		dev mus-audio-speakers = or
		dev mus-audio-mixer = or
		dev mus-audio-dac-filter = or
		dev mus-audio-aux-output = or
		dev mus-audio-dac-out = or if 0e 0 else 1e 1 then 0 vals array!
	    endof
	    mus-audio-record-device of
		fd SOUND_MIXER_READ_RECSRC retval c-ioctl drop
		retval @ case
		    SOUND_MASK_MIC     of mus-audio-microphone endof
		    SOUND_MASK_SPEAKER of mus-audio-speakers   endof
		    SOUND_MASK_LINE    of mus-audio-line       endof
		    SOUND_MASK_LINE1   of mus-audio-line1      endof
		    SOUND_MASK_LINE2   of mus-audio-line2      endof
		    SOUND_MASK_LINE3   of mus-audio-line3      endof
		    SOUND_MASK_VOLUME  of mus-audio-dac-out    endof
		    SOUND_MASK_VOLUME  of mus-audio-default    endof
		    SOUND_MASK_CD      of mus-audio-cd         endof
		    str-audio-mixer-device warn 0 swap
		endcase dup 0 vals array! s>f
	    endof
	    -1e ( default return value )
	endcase
    then
    fd audio-mixer-close
    vals
;
: audio-mixer-write { dev fld chan f: val -- r vals }
    audio-open-mixer-write { fd }
    0 { w^ retval }
    fd SOUND_MIXER_READ_DEVMASK retval c-ioctl if
	fd audio-mixer-close
	str-audio-mixer-write error
    then
    dev mus-audio-mixer = dev mus-audio-dac-filter = or if
	dev fld chan 0> if 0 else 1 then audio-mixer-read drop { f: vol }
	val 1e fmin to val
	val 0e fmax to val
	vol 1e fmin to vol
	0 { w^ amp }
	chan unless
	    vol 100e f* f>s 8 lshift val 100e f* f>s +
	else
	    val 100e f* f>s 8 lshift vol 100e f* f>s +
	then amp !
	fd SOUND_MIXER_READ_DEVMASK retval c-ioctl drop
	fld case
	    mus-audio-imix of
		SOUND_MASK_IMIX   retval @ and if SOUND_MIXER_WRITE_IMIX    else false then
	    endof
	    mus-audio-igain of
		SOUND_MASK_IGAIN  retval @ and if SOUND_MIXER_WRITE_IGAIN   else false then
	    endof
	    mus-audio-reclev of
		SOUND_MASK_RECLEV retval @ and if SOUND_MIXER_WRITE_RECLEV  else false then
	    endof
	    mus-audio-pcm of
		SOUND_MASK_PCM    retval @ and if SOUND_MIXER_WRITE_PCM     else false then
	    endof
	    mus-audio-pcm2 of
		SOUND_MASK_ALTPCM retval @ and if SOUND_MIXER_WRITE_ALTPCM  else false then
	    endof
	    mus-audio-ogain of
		SOUND_MASK_OGAIN  retval @ and if SOUND_MIXER_WRITE_OGAIN   else false then
	    endof
	    mus-audio-line of
		SOUND_MASK_LINE   retval @ and if SOUND_MIXER_WRITE_LINE    else false then
	    endof
	    mus-audio-microphone of
		SOUND_MASK_MIC    retval @ and if SOUND_MIXER_WRITE_MIC     else false then
	    endof
	    mus-audio-line1 of
		SOUND_MASK_LINE1  retval @ and if SOUND_MIXER_WRITE_LINE1   else false then
	    endof
	    mus-audio-line2 of
		SOUND_MASK_LINE2  retval @ and if SOUND_MIXER_WRITE_LINE2   else false then
	    endof
	    mus-audio-line3 of
		SOUND_MASK_LINE3  retval @ and if SOUND_MIXER_WRITE_LINE3   else false then
	    endof
	    mus-audio-synth of
		SOUND_MASK_SYNTH  retval @ and if SOUND_MIXER_WRITE_SYNTH   else false then
	    endof
	    mus-audio-bass of
		SOUND_MASK_BASS   retval @ and if SOUND_MIXER_WRITE_BASS    else false then
	    endof
	    mus-audio-treble of
		SOUND_MASK_TREBLE retval @ and if SOUND_MIXER_WRITE_TREBLE  else false then
	    endof
	    mus-audio-cd of
		SOUND_MASK_CD     retval @ and if SOUND_MIXER_WRITE_CD      else false then
	    endof
	    str-audio-mixer-device warn 0 swap
	endcase
	?dup-if fd swap amp c-ioctl if str-audio-mixer-amp warn then then
    else
	fld case
	    mus-audio-amp of
		dev fld chan if 0 else 1 then audio-mixer-read drop { f: vol }
		val 1e fmin to val
		val 0e fmax to val
		vol 1e fmin to vol
		0 { w^ amp }
		chan unless
		    vol 100e f* f>s 8 lshift val 100e f* f>s +
		else
		    val 100e f* f>s 8 lshift vol 100e f* f>s +
		then amp !
		fd SOUND_MIXER_READ_DEVMASK retval c-ioctl drop
		dev case
		    mus-audio-microphone of
			SOUND_MASK_MIC retval @ and     if SOUND_MIXER_WRITE_MIC     else false then
		    endof
		    mus-audio-speakers of
			SOUND_MASK_SPEAKER retval @ and if SOUND_MIXER_WRITE_SPEAKER else false then
		    endof
		    mus-audio-line-in of
			SOUND_MASK_LINE retval @ and    if SOUND_MIXER_WRITE_LINE    else false then
		    endof
		    mus-audio-line1 of
			SOUND_MASK_LINE1 retval @ and   if SOUND_MIXER_WRITE_LINE1   else false then
		    endof
		    mus-audio-line2 of
			SOUND_MASK_LINE2 retval @ and   if SOUND_MIXER_WRITE_LINE2   else false then
		    endof
		    mus-audio-line3 of
			SOUND_MASK_LINE3 retval @ and   if SOUND_MIXER_WRITE_LINE3   else false then
		    endof
		    mus-audio-dac-out of
			SOUND_MASK_VOLUME retval @ and  if SOUND_MIXER_WRITE_VOLUME  else false then
		    endof
		    mus-audio-default of
			SOUND_MASK_VOLUME retval @ and  if SOUND_MIXER_WRITE_VOLUME  else false then
		    endof
		    mus-audio-cd of
			SOUND_MASK_CD retval @ and      if SOUND_MIXER_WRITE_CD      else false then
		    endof
		    str-audio-mixer-device warn 0 swap
		endcase
		?dup-if fd swap amp c-ioctl if str-audio-mixer-amp warn then then
	    endof
	    mus-audio-srate of
		val f>s retval !
		fd SNDCTL_DSP_SPEED retval c-ioctl if
		    fd audio-mixer-close
		    0 audio-open-write-device to fd
		    fd SNDCTL_DSP_SPEED retval c-ioctl drop
		then
	    endof
	    mus-audio-record-device of
		dev case
		    mus-audio-microphone of SOUND_MASK_MIC     endof
		    mus-audio-speakers   of SOUND_MASK_SPEAKER endof
		    mus-audio-line       of SOUND_MASK_LINE    endof
		    mus-audio-line-in    of SOUND_MASK_LINE    endof
		    mus-audio-line1      of SOUND_MASK_LINE1   endof
		    mus-audio-line2      of SOUND_MASK_LINE2   endof
		    mus-audio-line3      of SOUND_MASK_LINE3   endof
		    mus-audio-dac-out    of SOUND_MASK_VOLUME  endof
		    mus-audio-default    of SOUND_MASK_VOLUME  endof
		    mus-audio-cd         of SOUND_MASK_CD      endof
		    str-audio-mixer-device warn 0 swap
		endcase retval !
		fd SOUND_MIXER_WRITE_RECSRC retval c-ioctl drop
	    endof
	endcase
    then
    fd audio-mixer-close
    dev fld chan audio-mixer-read
;

\ fsndlib.fs ends here
