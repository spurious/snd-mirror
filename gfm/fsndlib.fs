\ fsndlib.fs -- generators -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Jul 21 22:19:43 CEST 2003
\ Last: Sat Oct 09 00:10:06 CEST 2004
\ Ident: $Id: fsndlib.fs,v 1.94 2004/10/08 23:15:13 mike Exp $

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
\
\ e.g.
\ make-oscil ( freq phase -- gen )
\ oscil ( fm pm gen -- r )
\ .oscil ( gen -- )
\ ?oscil ( gen -- f )
\
\ generator print-functions like .oscil can be replaced by the
\ generall `.gen' function.
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
\ Example:
\
\ : simp { f: start f: dur f: freq f: amp -- }
\     freq 0e make-oscil { os }
\     start dur times>samples do   amp  0e 0e os oscil  f*  i *output* outa   loop
\     os mus-free
\ ;
\
\ The same result can be achieved by
\
\ instrument: simp { f: start f: dur f: freq f: amp -- }
\     freq 0e make-oscil { os }
\     start dur run-instrument   amp  0e 0e os oscil  f*   end-run
\     os mus-free
\ ;instrument

\ Code:

require utils.fs
only forth also definitions also GFMusic
vocabulary CLM-Sndlib CLM-Sndlib definitions also GFMusic

\ === Vct ===
: make-simple-vct { len -- v }
    assert1( len 0>= )
    vct% %alloc { v }
    len sfloats allocate throw { data }
    len v v-len !
    data v v-data !
    vct% %alloc v v-orig !
    s" vct" v v-name 2!
    v
;
: make-vct { len -- v }
    len make-simple-vct { v }
    len if v v-data @ len sfloats bounds u+do 0e i sf! 1 sfloats +loop then
    v
;
: >vct ( u-float-values u -- v ) { len }
    len make-vct { v }
    len if v v-data @ 1 sfloats - len sfloats bounds swap u-do i sf! 1 sfloats -loop then
    v
;
: free-vct { v -- } v v-data @ free throw v free throw ;
: vct-copy { v1 -- v2 }
    v1 v-len @ { len }
    len make-simple-vct { v2 }
    v1 v-data @ v2 v-data @ len sfloats move
    v2
;
: vct= { v1 v2 -- f }
    v1 v2 = if
	true
    else
	v1 vct-length v2 vct-length = if
	    true
	    v1 vct-length 0 do i v1 vct@ i v2 vct@ f= 0= if drop false leave then loop
	else
	    false
	then
    then
;

8 value *array-print-length*
: array-print-length@ ( -- n ) *array-print-length* ;
: array-print-length! ( val -- ) to *array-print-length* ;
: .vct { v -- }
    array-print-length@ { len }
    v vct-length { vlen }
    ." #<vct[" vlen . bs ." ]:"
    vlen if
	."  [" len vlen min 0 do i v vct@ 3 f.r loop
	vlen len > if ." ..." else bs then ." ]"
    then
    ." >"
; 

\ === Sound-Data ===
: make-sound-data { chns frms -- sd }
    assert1( chns 0> frms 0> and )
    sd% %alloc { sd }
    chns sd sd-chans !
    frms sd sd-len !
    chns cells allocate throw { data }
    chns 0 do
	frms sfloats allocate throw dup frms sfloats bounds do 0e i sf! 1 sfloats +loop
	data i cells + !
    loop
    data sd sd-data !
    false sd sd-orig !
    s" sound-data" sd sd-name 2!
    sd
;
: sound-data-free { sd -- }
    sd sd-chans @ 0 do sd sd-data @ i cells + @ free throw loop
    sd sd-data @ free throw
    sd free throw
;
: sound-data= { sd1 sd2 -- f }
    sd1 sd2 = if
	true
    else sd1 sd-chans @ sd2 sd-chans @ = sd1 sd-len @ sd2 sd-len @ = and if
	    true
	    sd1 sd-chans @ 0 do
		sd1 sd-data @ i cells + @ sd1 sd-len @ sfloats
		sd2 sd-data @ i cells + @ sd2 sd-len @ sfloats
		compare if drop false leave then
	    loop
	else
	    false
	then
    then
;
: .sound-data { sd -- }
    ." #<sound-data: " sd sd-chans @ . ." chans, " sd sd-len @ . ." frames>"
;

require gfm-defs.fs

0 value *io-buffer-size*

: srate@ ( -- n ) *srate* ;
: srate! ( val -- ) to *srate* ;
: mus-srate@ ( -- r ) *srate* s>f ;
: mus-srate! ( f: val -- ) f>s to *srate* ;
: seconds>samples ( f: val -- u ) *srate* s>f f* f>s ;
: samples>seconds ( val -- r ) s>f *srate* s>f f/ ;
: file-buffer-size@ ( -- n ) *io-buffer-size* ;
: file-buffer-size! ( val -- ) to *io-buffer-size* ;
: gfm-mus-free ( gen -- ) dup get-gen-free perform ;
: .gfm-gen ( gen -- ) dup get-inspect perform ;
: gfm-gen= ( gen1 gen2 -- f ) 2drop true ;

\ === Music5 ===
: radians>hz ( f: val -- r ) mus-srate@ f* two-pi f/ ;
: hz>radians ( f: val -- r ) two-pi f* mus-srate@ f/ ;
: degrees>radians ( f: val -- r ) 360e f/ two-pi f* ;
: radians>degrees ( f: val -- r ) 360e two-pi f/ f* ;
: db>linear ( f: val -- r ) 20e f/ falog ;	    \ r falog --> 10^r
: linear>db ( f: val -- r ) 0.00001e fmax flog 20e f* ;
: ring-modulate ( f: in1 f: in2 -- r ) f* ;
: amplitude-modulate { f: car f: in1 f: in2 -- r } in1 car in2 f+ f* ;
: dot-product { in1 in2 -- r } 0e in1 vct-each i in1 vct@  i in2 vct@  f*  f+ loop ;
: sine-bank { amps phases -- r } 0e amps vct-each i amps vct@  i phases vct@ fsin  f*  f+ loop ;
' vct* alias multiply-arrays
' vct-clear alias clear-array
: sqr ( f: val -- r*r ) fdup f* ;
: rectangular>polar { rdat idat -- }
    rdat vct-each
	i rdat vct@ sqr i idat vct@ sqr f+ fsqrt { f: tmp }
	i idat vct@ i rdat vct@ fatan2 fnegate i idat vct!
	tmp i rdat vct!
    loop
;
: polar>rectangular { rdat idat -- }
    rdat vct-each
	i rdat vct@ { f: mag }
	i idat vct@ fnegate { f: ang }
	mag ang fsin f* { f: tmp }
	mag ang fcos f* i rdat vct!
	tmp i idat vct!
    loop
;
\ val 1e contrast-enhancement fvalue res
: contrast-enhancement { f: val f: fm -- r } val half-pi f* val two-pi f* fsin fm f* f+ fsin ;

\ rl im rl vct-length 1 fft
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
	    midn s>f 1e beta f- f* { cx }
	    size 1- { jj }
	    midn 1+ 0 do
		i cx >= if
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
    endcase
    window
;
\ rdat idat window 1 spectrum
: spectrum { rdat idat window tp -- }
    rdat vct-length { len }
    len 1- len and if 2e len s>f 2e flogn fround f** f>s to len then
    rdat window multiply-arrays
    idat vct-clear
    rdat idat len 1 fft
    len 2/ to len
    0e { f: maxa }
    0.000001e { f: lowest }
    rdat vct-each
	i rdat vct@ sqr i idat vct@ sqr f+ { f: val }
	val lowest f< if 0.001e else val fsqrt then fdup i rdat vct!
	fabs maxa fmax to maxa
    loop
    maxa f0> if
	maxa 1/f to maxa
	tp 0= if
	    20e 10e fln f/ { f: to-dB }
	    rdat vct-each to-dB i rdat vct@ maxa f* fln f* i rdat vct! loop
	else
	    tp 1 = if rdat vct-each i rdat vct@ maxa f* i rdat vct! loop then
	then
    then
;

\ --- polynomial, partials>wave(shape) ---
: normalize-array { ary -- ary' }
    ary vct-length { len }
    0e { f: maxval }
    ary vct-each i ary vct@ fabs maxval fmax to maxval loop
    maxval f0<> maxval 1e f<> and if ary vct-each i ary vct@ maxval f/ i ary vct! loop then
    ary
;
: partials>waveshape { parts size -- parts' }
    size make-vct { ary }
    2e size s>f f/ { f: max-i-2 }
    -1e { f: x }
    size 0 do
	0e { f: sum }
	0e { f: temp }
	1e { f: tn }
	x { f: tn-1 }
	parts vct-each
	    i parts vct@ f0<> if tn i parts vct@ f* sum f+ to sum then
	    tn-1 to temp
	    tn-1 f2* x f* tn f- to tn-1
	    temp to tn
	loop
	sum i ary vct!
	x max-i-2 f+ to x
    loop
    ary normalize-array
;
: partial-amp { f: n parts -- r }
    fdepth
    parts vct-each i parts vct@ n f= if i 1+ parts vct@ leave then 2 +loop
    fdepth = if 0e then

;
\ parts 1 partials>polynomial value v
: partials>polynomial { parts kind -- w }
    0e parts vct-each i parts vct@ fmax 2 +loop f>s { top }
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
    size 0 do i cc1 vct@ loop size >vct
;

: load-one-sine-wave { f: partial f: part-amp table f: phase -- }
    part-amp f0<> if
	partial two-pi table vct-length s>f f/ f* { f: frq }
	phase { f: angle }
	table vct-each
	    i table vct@ part-amp angle fsin f* f+ i table vct!
	    angle frq f+ to angle
	loop
    then
;
: partials>wave { data norm -- w }
    *table-size* make-vct { table }
    data vct-each
	i data vct@  i 1+ data vct@  table  0e  load-one-sine-wave
    2 +loop
    norm if table normalize-array else table then
;
: phase-partials>wave { data norm -- w }
    *table-size* make-vct { table }
    data vct-each
	i data vct@  i 1+ data vct@  table  i 2 + data vct@  load-one-sine-wave
    3 +loop
    norm if table normalize-array else table then
;

: polynomial { coeffs f: x -- sum }
    coeffs vct-length 1- { top }
    top coeffs vct@ { f: sum }
    -1 top 1- -do x sum f* i coeffs vct@ f+ to sum 1 -loop
    sum
;
: array-interp { ary f: x -- r }
    ary vct-length { len }
    len s>f { f: flen }
    x f0< x flen f> or if
	x flen fmod to x
	x f0< if x flen f+ to x then
    then
    x truncate { int-part f: frac-part }
    int-part len = if 0 to int-part then
    frac-part f0= if
	int-part ary vct@
    else
	int-part 1+ { inx }
	inx len >= if 0 to inx then
	int-part ary vct@ frac-part inx ary vct@ int-part ary vct@ f- f* f+
    then
;
: array-interp-size { ary f: x len -- r }
    x f0< if x len s>f f+ to x then
    x truncate { int-part f: frac-part }
    int-part len >= if int-part len mod to int-part then
    frac-part f0= if
	int-part ary vct@
    else
	int-part ary vct@
	frac-part int-part 1+ len < if int-part 1+ else 0 then ary vct@
	int-part ary vct@ f- f* f+
    then
;
: signify { harm-amps -- harm-amps' }
    harm-amps vct-length { len } 2 { idx } 1 { di }
    begin idx len < while
	    idx harm-amps vct@ fnegate idx harm-amps vct!
	    idx di + to idx 4 di - to di
    repeat
    harm-amps
;

: ?gen { d: gname obj -- f } try obj gen-name 2@ gname str= recover drop false endtry ;
: generator-free ( gen -- ) free throw ;
: data-buffer-free ( gen -- ) dup data-buffer @ free-vct free throw ;

\ === Oscil ===
\ Usage: 440e 0e make-oscil value os
\        0e 0e os oscil 3 f.r  --> 0.000
\        os frequency@ 3 f.r   --> 440.000
\        330e os frequency!
\        os phase@ 3 f.r       --> 0.125
\        0.5e os phase!
\        os .oscil             --> #<oscil: frequency: 0.094 (330 Hz), phase: 0.500 (0 degrees)>
music5%
    cell% field get-phase
    cell% field set-phase
    float% field freq
    float% field phase
end-struct oscil%

: gfm-mus-run ( f: val1 f: val2 gen -- r ) dup get-mus-run perform ;
: name@ ( gen -- addr len ) gen-name 2@ ;
: gfm-frequency@ ( gen -- r ) freq f@ radians>hz ;
: gfm-frequency! ( f: val gen -- ) hz>radians freq f! ;
: phase@ ( gen -- r ) dup get-phase perform ;
: phase! ( f: ph gen -- ) dup set-phase perform ;
: gfm-phase@ ( gen -- r ) phase f@ two-pi fmod ;
: gfm-phase! ( f: val gen -- ) phase f! ;

: .pp-print { gen -- }
    ." frequency: " gen freq f@ 3 f.r ." (" gen freq f@ radians>hz fround f>s . ." Hz), "
    ." phase: " gen phase f@ 3 f.r ." (" gen phase f@ two-pi fmod fround f>s . ." degrees)"
;

: .oscil { gen -- } ." #<oscil: " gen .pp-print ." >" ;
: ?oscil { obj -- f } s" oscil" obj ?gen ;
: oscil { f: fm f: pm gen -- r }
    gen phase f@ pm f+ fsin		\ result
    gen phase f@ gen freq f@ fm f+ f+ { f: ph }
    ph 100e f> ph -100e f< or if ph two-pi fmod else ph then gen phase f!
;
: make-oscil { f: frq f: ph -- gen }
    oscil% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    1 gen cosines !
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] oscil gen get-mus-run !
    ['] .oscil gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" oscil" gen gen-name 2!
    gen
;

\ === Env ===
\ Usage: 0e 0e 25e 1e 75e 1e 100e 0e 8 >vct 1e 1e make-env value en
\        en env 3 f.r --> 0.000
\        en .gen      --> #<env: rate: 0.000, pass: 16539, value: 0.000,
\                            scaler: 1.000, offset: 0.000, length: 22050,
\                            data: #<vct[8]: [0.000 0.000 25.000 1.000 75.000 1.000 100.000 0.000]>>
music5%
    cell% field data-len
    cell% field pass
    float% field parts
    float% field rate
    float% field env-offset
    float% field env-base
end-struct env%

: data@ ( idx gen -- r ) data-buffer @ vct@ ;
: data! ( f: val idx gen -- ) data-buffer @ vct! ;
: data>vct ( gen -- v ) data-buffer @ vct-copy ;
: vct>data ( v gen -- ) >r vct-copy r> data-buffer ! ;
: offset@ ( gen -- r ) env-offset f@ ;
: offset! ( f: val gen -- ) env-offset f! ;
: position@ { gen -- n } gen buffer-length @ gen pass @ - ;
: gfm-length@ ( gen -- n ) buffer-length @ ;
: gfm-length! ( val gen -- ) buffer-length ! ;
: gfm-scaler@ ( gen -- r ) scaler f@ ;
: gfm-scaler! ( f: val gen -- ) scaler f! ;

: passes { gen -- u }
    gen location @ 2 + gen data-len @ >= if
	assert-level @ 1 >= if
	    cr ." \ === FIXME: in ENV-PASSES location " gen location @ . bs ." , reset to 0 ===" cr
	then
	0 gen location !
    then
    gen location @     gen data-buffer @ vct@ { f: x0 }
    gen location @ 2 + gen data-buffer @ vct@ { f: x1 }
    x1 x0 f- gen parts f@ f/ gen buffer-length @ s>f f* f>s
;
: rates { gen -- r }
    gen location @ 1+  gen data-buffer @ vct@ { f: y0 }
    gen location @ 3 + gen data-buffer @ vct@ { f: y1 }
    y0 y1 f= if 0e else y1 y0 f- gen pass @ s>f f/ then
;

: .env { gen -- }
    ." #<env: rate: " gen rate f@ 3 f.r bs
    ." , pass: " gen pass @ . bs
    ." , value: " gen current-value f@ 3 f.r bs
    ." , scaler: " gen gfm-scaler@ 3 f.r bs
    ." , offset: " gen offset@ 3 f.r bs
    ." , length: " gen gfm-length@ . bs
    ." , data: " gen data-buffer @ .vct ." >"
;
: ?env { obj -- f } s" env" obj ?gen ;
: env { gen -- r }
    gen current-value f@ gen scaler f@ f* gen env-offset f@ f+
    -1 gen pass +!
    gen pass @ 0< if 2 gen location +! gen passes gen pass ! gen rates gen rate f! then
    gen rate f@ gen current-value f@ f+ gen current-value f!
;
: env-run ( r1 r2 gen -- r ) fdrop fdrop env ;
: make-envelope { data f: scale f: dur f: offset f: base start end -- gen }
    assert1( data ?vct start 0>= end 0>= and and )
    dur f0= if end start - s>f mus-srate@ f/ to dur then
    env% %alloc { gen }
    0 gen location !
    dur seconds>samples gen buffer-length !
    data vct-length 2 - data vct@ gen parts f!
    1 data vct@ scale f* offset f+ gen current-value f!
    scale gen scaler f!
    data gen data-buffer !
    data vct-length gen data-len !
    gen passes gen pass !
    gen rates gen rate f!
    offset gen env-offset f!
    base gen env-base f!		\ base; not used (only seg)
					\ 0e base: step
					\ 1e base: seg or exp
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] env-run gen get-mus-run !
    ['] .env gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" env" gen gen-name 2!
    gen
;
: make-env { data f: scale f: dur -- gen } data scale dur 0e 1e 0 0 make-envelope ;
: restart-env { gen -- }
    0 gen location ! 1 gen data-buffer @ vct@ gen current-value f!
    gen passes gen pass ! gen rates gen rate f!
;
: env-interp { f: x gen -- r } gen data-buffer @ x envelope-interp gen scaler f@ f* ;
: location@ ( gen -- n ) dup ?env if pass else location then @ ;
: location! { val gen -- }
    gen ?env if
	gen restart-env val 0 u+do gen env fdrop loop
    else
	val gen location !
    then
;

\ === Table-Lookup ===
oscil%
end-struct table-lookup%

: tb-frequency@ { gen -- r } gen freq f@ mus-srate@ f* gen buffer-length @ s>f f/ ;
: tb-frequency! { f: val gen -- } val gen buffer-length @ s>f f* mus-srate@ f/ gen freq f! ;
: tb-phase@ { gen -- r } gen phase f@ two-pi f* gen buffer-length @ s>f f/ two-pi fmod ;
: tb-phase! { f: val gen -- } val gen buffer-length @ s>f f* two-pi f/ gen phase f! ;
: interp-type@ ( gen -- n ) interpolation-type @ ;

: .tb-pp-print { gen -- }
    ." frequency: " gen freq f@ 3 f.r ." (" gen tb-frequency@ fround f>s . ." Hz), "
    ." phase: " gen phase f@ 3 f.r ." (" gen tb-phase@ fround f>s . ." degrees)"
;

: .table-lookup { gen -- }
    ." #<table-lookup: " gen .tb-pp-print
    ." , size: " gen gfm-length@ . bs
    ." , wave: " gen data-buffer @ .vct ." >"
;
: ?table-lookup { obj -- f } s" table-lookup" obj ?gen ;
: table-lookup { f: fm gen -- r }
    gen data-buffer @ gen phase f@ array-interp { f: val }
    gen buffer-length @ s>f { f: len }
    gen phase f@ gen freq f@ len two-pi f/ fm f* f+ f+ { f: ph }
    ph len f>= ph f0< or if
	ph len fmod { f: res }
	res f0< if res len f+ else res then
    else
	ph
    then gen phase f! 
    val
;
: tb-run ( r1 r2 gen -- r ) fdrop table-lookup ;
\ 440e  0e  *table-size* make-vct  mus-interp-linear  make-table-lookup value gen
: make-table-lookup { f: frq f: ph wav tp -- gen )
    assert1( wav ?vct )
    table-lookup% %alloc { gen }
    wav vct-length s>f { f: len }
    wav gen data-buffer !
    len f>s gen buffer-length !
    frq len f* mus-srate@ f/ gen freq f!
    ph len f* two-pi f/ gen phase f!
    tp gen interpolation-type !
    ['] tb-frequency@ gen get-freq !
    ['] tb-frequency! gen set-freq !
    ['] tb-phase@ gen get-phase !
    ['] tb-phase! gen set-phase !
    ['] tb-run gen get-mus-run !
    ['] .table-lookup gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" table-lookup" gen gen-name 2!
    gen
;

\ === Waveshape ===
oscil%
end-struct waveshape%

: .waveshape { gen -- }
    ." #<waveshape: " gen .pp-print
    ." , size: " gen data-buffer @ vct-length . bs
    ." , wave: " gen data-buffer @ .vct ." >"
;
: ?waveshape { obj -- f } s" waveshape" obj ?gen ;
: waveshape { f: index f: fm gen -- r }
    gen phase f@ fsin { f: oscval }
    gen data-buffer @ dup vct-length s>f f2/ floor 1e index oscval f* f+ f*
    array-interp { f: wsval }
    gen phase f@ gen freq f@ fm f+ f+ { f: ph }
    ph 100e f> ph -100e f< or if ph two-pi fmod else ph then gen phase f!
    wsval
;
\ 440e 1e 1e 2 >vct make-waveshape value gen
: make-waveshape { f: frq parts -- gen }
    assert1( parts ?vct )
    waveshape% %alloc { gen }
    frq hz>radians gen freq f!
    0e gen phase f!
    parts normalize-partials array>partials signify *table-size* partials>waveshape { ary }
    ary gen data-buffer !
    *table-size* gen buffer-length !
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] waveshape gen get-mus-run !
    ['] .waveshape gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" waveshape" gen gen-name 2!
    gen
;

\ === Triangle-Wave ===
oscil%
end-struct triangle-wave%

: triangle-scaler@ ( gen -- r ) scaler f@ half-pi f* ;
: triangle-scaler! ( f: val gen -- ) half-pi f* scaler f! ;

3e half-pi f* fconstant three-half-pi
2e pi f/ fconstant two/pi

: .triangle-wave { gen -- }
    ." #<triangle-wave: " gen .pp-print
    ." , scaler: " gen triangle-scaler@ 3 f.r bs
    ." , value: " gen current-value f@ 3 f.r bs ." >"
;
: ?triangle-wave { obj -- f } s" triangle-wave" obj ?gen ;
: triangle-wave { f: fm gen -- r }
    gen current-value f@ { f: val }
    gen phase f@ gen freq f@ fm f+ f+ gen phase f!
    begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
    begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
    gen phase f@ half-pi f< if
	gen scaler f@ gen phase f@ f*
    else
	gen phase f@ three-half-pi f< if
	    gen scaler f@ pi gen phase f@ f- f*
	else
	    gen scaler f@ gen phase f@ two-pi f- f*
	then
    then gen current-value f!
    val
;
: triangle-run ( r1 r2 gen -- r ) fdrop triangle-wave ;
\ 440e 1e 0e make-triangle-wave value gen
: make-triangle-wave { f: frq f: amp f: ph -- gen }
    triangle-wave% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    amp f2* pi f/ gen scaler f!
    ph half-pi f< if
	amp ph f*
    else
	ph three-half-pi f< if
	    amp pi ph f- f*
	else
	    amp ph two-pi f- f*
	then
    then gen current-value f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] triangle-scaler@ gen get-scaler !
    ['] triangle-scaler! gen set-scaler !
    ['] triangle-run gen get-mus-run !
    ['] .triangle-wave gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" triangle-wave" gen gen-name 2!
    gen
;

\ === Square-Wave ===
triangle-wave%
    float% field width
end-struct square-wave%

: width@ ( gen -- r ) width f@ two-pi f/ ;
: width! ( f: val gen -- ) two-pi f* width f! ;

: .square-wave { gen -- }
    ." #<square-wave: " gen .pp-print
    ." , scaler: " gen gfm-scaler@ 3 f.r bs
    ." , value: " gen current-value f@ 3 f.r bs ." >"
;
: ?square-wave { obj -- f } s" square-wave" obj ?gen ;
: square-wave { f: fm gen -- r }
    gen current-value f@ { f: val }
    gen phase f@ gen freq f@ fm f+ f+ gen phase f!
    begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
    begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
    gen phase f@ gen width f@ f< if gen scaler f@ else 0e then gen current-value f!
    val
;
: square-run ( r1 r2 gen -- r ) fdrop square-wave ;
\ 440e 1e 0e make-square-wave value gen
: make-square-wave { f: frq f: amp f: ph -- gen }
    square-wave% %alloc { gen }
    \ clm.c says the following but mus.lisp f>
    ph pi f< if amp else 0e then gen current-value f!
    frq hz>radians gen freq f!
    ph gen phase f!
    amp gen scaler f!
    pi gen width f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] square-run gen get-mus-run !
    ['] .square-wave gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" square-wave" gen gen-name 2!
    gen
;

\ === Sawtooth-Wave ===
triangle-wave%
end-struct sawtooth-wave%

: sawtooth-scaler@ { gen -- r } gen scaler f@ pi f* ;
: sawtooth-scaler! { f: val gen -- } val pi f/ gen scaler f! ;

: .sawtooth-wave { gen -- }
    ." #<sawtooth-wave: " gen .pp-print
    ." , scaler: " gen sawtooth-scaler@ 3 f.r bs
    ." , value: " gen current-value f@ 3 f.r bs ." >"
;
: ?sawtooth-wave { obj -- f } s" sawtooth-wave" obj ?gen ;
: sawtooth-wave { f: fm gen -- r }
    gen current-value f@ { f: val }
    gen phase f@ gen freq f@ fm f+ f+ fdup { f: ph } gen phase f!
    begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
    begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
    gen scaler f@ gen phase f@ pi f- f* gen current-value f!
    val
;
: sawtooth-run ( r1 r2 gen -- r ) fdrop sawtooth-wave ;
\ 440e 1e pi make-sawtooth-wave value gen
: make-sawtooth-wave { f: frq f: amp f: ph -- gen }
    sawtooth-wave% %alloc { gen }
    amp ph pi f- pi f/ f* gen current-value f!
    frq hz>radians gen freq f!
    ph gen phase f!
    amp pi f/ gen scaler f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] sawtooth-scaler@ gen get-scaler !
    ['] sawtooth-scaler! gen set-scaler !
    ['] sawtooth-run gen get-mus-run !
    ['] .sawtooth-wave gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sawtooth-wave" gen gen-name 2!
    gen
;

\ === Pulse-Train ===
triangle-wave%
end-struct pulse-train%

: .pulse-train { gen -- }
    ." #<pulse-train: " gen .pp-print
    ." , scaler: " gen gfm-scaler@ 3 f.r bs
    ." , value: " gen current-value f@ 3 f.r bs ." >"
;
: ?pulse-train { obj -- f } s" pulse-train" obj ?gen ;
: pulse-train { f: fm gen -- r }
    gen phase f@ fabs two-pi f>= if
	begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
	begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
	gen scaler f@
    else
	0e
    then { f: val }
    val gen current-value f!
    gen phase f@ gen freq f@ fm f+ f+ gen phase f!
    val
;
: pulse-run ( r1 r2 gen -- r ) fdrop pulse-train ;
\ 440e 1e two-pi make-pulse-train value gen
: make-pulse-train { f: frq f: amp f: ph -- gen }
    pulse-train% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    amp gen scaler f!
    0e gen current-value f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] pulse-run gen get-mus-run !
    ['] .pulse-train gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" pulse-train" gen gen-name 2!
    gen
;

\ === Sum-Of-Cosines ===
oscil%
    float% field cos5
end-struct sum-of-cosines%

: cosines@ ( gen -- n ) cosines @ ;
: cosines! ( val gen -- ) cosines ! ;

: .sum-of-cosines { gen -- }
    ." #<sum-of-cosines: " gen .pp-print
    ." , cosines: " gen cosines@ . bs
    ." , scaler: " gen gfm-scaler@ 3 f.r bs ." >" 
;
: ?sum-of-cosines { obj -- f } s" sum-of-cosines" obj ?gen ;
: sum-of-cosines { f: fm gen -- r }
    gen phase f@ f2/ fsin { f: den }
    den f0= if
	1e
    else
	gen scaler f@  gen phase f@ gen cos5 f@ f* fsin  den f2*  f/  0.5e f-  f*  1e fmin
    then
    gen phase f@ gen freq f@ fm f+ f+ gen phase f!
;
: soc-run ( r1 r2 gen -- r ) fdrop sum-of-cosines ;
\ 440e 0e 1 make-sum-of-cosines value gen
: make-sum-of-cosines { f: frq f: ph cosin -- gen }
    assert1( cosin 0> )
    sum-of-cosines% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    cosin gen cosines !
    cosin gen buffer-length !
    cosin s>f 1/f gen scaler f!
    cosin s>f 0.5e f+ gen cos5 f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] soc-run gen get-mus-run !
    ['] .sum-of-cosines gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sum-of-cosines" gen gen-name 2!
    gen
;

\ === Sum-Of-Sines ===
sum-of-cosines%
end-struct sum-of-sines%

1.0e 1.0e 1.761e 2.5e 3.24e 3.97e 4.7e 5.42e 6.15e 6.88e 7.6e 8.33e 9.05e 9.78e
10.51e 11.23e 11.96e 12.68e 13.41e 14.13e 20 >vct value fsndlib-scl-ary

: sum-of-sines-scaler { sines -- r }
    sines 20 < if
	sines fsndlib-scl-ary vct@ 1/f
    else
	sines s>f
	sines 50 < if
	     0.743e
	else
	     0.733e
	then
	f* 1/f
    then
;

: sines@ ( gen -- n ) cosines @ ;
: sines! { val gen -- }
    val gen cosines !
    val sum-of-sines-scaler gen scaler f!
;

: .sum-of-sines { gen -- }
    ." #<sum-of-sines: " gen .pp-print
    ." , sines: " gen sines@ . bs
    ." , scaler: " gen gfm-scaler@ 3 f.r bs ." >"
;
: ?sum-of-sines { obj -- f } s" sum-of-sines" obj ?gen ;
: sum-of-sines { f: fm gen -- r }
    gen phase f@ f2/ { f: a2 }
    a2 fsin { f: den }
    den f0= if
	0e
    else
	gen cosines @ s>f { f: cosin }
	gen scaler f@  cosin a2 f* fsin  a2 gen cos5 f@ f* fsin  f* f* den f/
    then
    gen phase f@ gen freq f@ fm f+ f+ gen phase f!
;
: sos-run ( r1 r2 gen -- r ) fdrop sum-of-sines ;
\ 440e 0e 1 make-sum-of-sines value gen
: make-sum-of-sines { f: frq f: ph cosin -- gen }
    assert1( cosin 0> )
    sum-of-sines% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    cosin gen cosines !
    cosin gen buffer-length !
    cosin sum-of-sines-scaler gen scaler f!
    cosin s>f 1e f+ gen cos5 f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] sos-run gen get-mus-run !
    ['] .sum-of-sines gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sum-of-sines" gen gen-name 2!
    gen
;

\ === Sine-Summation ===
oscil%
    float% field ss-b
    float% field ss-n
    float% field ss-an
    float% field ss-a2
end-struct sine-summation%

: .sine-summation { gen -- }
    ." #<sine-summation: " gen .pp-print
    ." , a: " gen gfm-scaler@ 1 f.r bs
    ." , n: " gen ss-n f@ f>s . bs
    ." , b: " gen ss-b f@ 1 f.r bs
    ." , an: " gen ss-an f@ 1 f.r bs
    ." , a2: " gen ss-a2 f@ 1 f.r bs ." >"
;
: ?sine-summation { obj -- f } s" sine-summation" obj ?gen ;
: sine-summation { f: fm gen -- r }
    gen phase f@ { f: th }
    gen scaler f@ { f: a }
    gen ss-n f@ { f: N }
    gen ss-b f@ th f* { f: B }
    th B f- { f: thB }
    gen ss-a2 f@ a f2* B fcos f* f- { f: divis }
    th fsin a thB fsin f* f- gen ss-an f@ th N 1e f+ B f* f+ fsin a th N B f* f+ fsin f* f- f* f-
    divis f/
    th gen freq f@ fm f+ f+ two-pi fmod gen phase f!
;
: sine-sum-run ( r1 r2 gen -- r ) fdrop sine-summation ;
\ 440e 0e 1 0.5e 1e make-sine-summation value gen
: make-sine-summation { f: frq f: ph w: n f: a f: ratio -- gen }
    sine-summation% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    a gen scaler f!
    n s>f gen ss-n f!
    ratio gen ss-b f!
    1e a a f* f+ gen ss-a2 f!
    a n s>f 1e f+ f** gen ss-an f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] sine-sum-run gen get-mus-run !
    ['] .sine-summation gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sine-summation" gen gen-name 2!
    gen
;

\ === Asymmetric-FM ===
oscil%
    float% field ratio
    float% field cosr
    float% field sinr
end-struct asymmetric-fm%

: asymmetric-scaler! { f: val gen -- }
    val gen scaler !
    val f0<> if
	val gen ratio f!
	0.5e val val 1/f f- f* gen cosr f!
	0.5e val val 1/f f+ f* gen sinr f!
    then
;

: .asymmetric-fm { gen -- }
    ." #<asymmetric-fm: " gen .pp-print
    ." , ratio: " gen ratio f@ 1 f.r bs
    ." , cosr: " gen cosr f@ 1 f.r bs
    ." , sinr: " gen sinr f@ 1 f.r bs ." >"
;
: ?asymmetric-fm { obj -- f } s" asymmetric-fm" obj ?gen ;
: asymmetric-fm { f: index f: fm gen -- r }
    gen phase f@ { f: th }
    gen ratio f@ th f* { f: mth }
    gen cosr f@ { f: cr }
    gen sinr f@ { f: sr }
    index cr mth fcos f* f* fexp th sr index mth fsin f* f* f+ fsin f*
    gen phase f@ gen freq f@ fm f+ f+ fdup gen phase f! { f: ph }
    ph 100e f> ph -100e f< or if ph two-pi fmod gen phase f! then
;
\ 440e 0e 1e 1e make-asymmetric-fm value gen
: make-asymmetric-fm { f: frq f: ph f: r f: rat -- gen }
    asymmetric-fm% %alloc { gen }
    frq hz>radians gen freq f!
    ph gen phase f!
    rat gen ratio f!
    r gen scaler f!
    0.5e r r 1/f f- f* gen cosr f!
    0.5e r r 1/f f+ f* gen sinr f!
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] asymmetric-scaler! gen set-scaler !
    ['] asymmetric-fm gen get-mus-run !
    ['] .asymmetric-fm gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" asymmetric-fm" gen gen-name 2!
    gen
;

\ === Rand ===
\ from clm-2/cmus.c
1 value random-seed
0.0000610351563e fconstant INVERSE-MAX-RAND
0.0000305175790e fconstant INVERSE-MAX-RAND2

: rand-seed! { val -- } val to random-seed ;
: rand-seed@ { -- u } random-seed ;
: next-random ( -- r )
    random-seed 1103515245 * 12345 + dup to random-seed
    16 rshift 32767 and s>f
;
: mus-random { f: val -- r } next-random INVERSE-MAX-RAND f* 1e f- val f* ;
: random { f: val -- r } next-random INVERSE-MAX-RAND2 f* val f* ;
: irandom { val -- r } next-random INVERSE-MAX-RAND2 f* val s>f f* f>s ;

oscil%
    float% field output
end-struct rand%

: random-any { gen -- r }
    gen data-buffer @ if
	gen scaler f@  gen data-buffer @ gen buffer-length @ s>f random array-interp f*
    else
	gen scaler f@ f0= if 0e else gen scaler f@ f2* random gen scaler f@ f- then
    then
;

: .rand { gen -- }
    ." #<rand: " gen .pp-print
    ." , scaler: " gen gfm-scaler@ 3 f.r bs
    gen data-buffer @ if ." , dist: " gen data-buffer @ .vct else ." , no dist" then ." >"
;
: ?rand { obj -- f } s" rand" obj ?gen ;
: rand { f: sweep gen -- r }
    gen phase f@ two-pi f>= if
	begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
	gen random-any gen output f!
    then
    gen phase f@ gen freq f@ sweep f+ f+ gen phase f!
    begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
    gen output f@
;
: rand-run ( r1 r2 gen -- r ) fdrop rand ;
\ 440e 1e make-rand value gen
: make-rand { f: frq f: amp -- gen }
    rand% %alloc { gen }
    frq hz>radians gen freq f!
    0e gen phase f!
    amp gen scaler f!
    0e gen output f!
    false gen data-buffer !
    0 gen buffer-length !
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] rand-run gen get-mus-run !
    ['] .rand gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" rand" gen gen-name 2!
    gen
;
: make-rand-dist { f: frq f: amp dist -- gen }
    assert1( dist vct-length 1 max-table-size within )
    frq amp make-rand { gen }
    dist inverse-integrate { data }
    data gen data-buffer !
    data vct-length gen buffer-length !
    ['] data-buffer-free gen get-gen-free !
    gen
;

\ === Rand-Interp ===
rand%
    float% field incr
end-struct rand-interp%

: .rand-interp { gen -- }
    ." #<rand-interp: " gen .pp-print
    ." , scaler: " gen gfm-scaler@ 3 f.r bs
    ." , incr: " gen incr f@ 3 f.r bs
    gen data-buffer @ if ." , dist: " gen data-buffer @ .vct else ." , no dist" then ." >"
;
: ?rand-interp { obj -- f } s" rand-interp" obj ?gen ;
: rand-interp { f: sweep gen -- r }
    gen output f@ gen incr f@ f+ gen output f!
    gen phase f@ two-pi f>= if
	begin gen phase f@ two-pi f>= while gen phase f@ two-pi f- gen phase f! repeat
	gen random-any gen output f@ f- gen freq f@ sweep f+ two-pi f/ f* gen incr f!
    then
    gen phase f@ gen freq f@ f+ sweep f+ gen phase f!
    begin gen phase f@ f0< while gen phase f@ two-pi f+ gen phase f! repeat
    gen output f@
;
: rand-interp-run ( r1 r2 gen -- r ) fdrop rand-interp ;
\ 440e 1e make-rand-interp value gen
: make-rand-interp { f: frq f: amp -- gen }
    rand-interp% %alloc { gen }
    frq hz>radians gen freq f!
    0e gen phase f!
    amp gen scaler f!
    0e gen output f!
    amp f0= if 0e else frq mus-srate@ f/ amp random f* then gen incr f!
    false gen data-buffer !
    0 gen buffer-length !
    ['] gfm-frequency@ gen get-freq !
    ['] gfm-frequency! gen set-freq !
    ['] gfm-phase@ gen get-phase !
    ['] gfm-phase! gen set-phase !
    ['] gfm-scaler@ gen get-scaler !
    ['] gfm-scaler! gen set-scaler !
    ['] rand-interp-run gen get-mus-run !
    ['] .rand-interp gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" rand-interp" gen gen-name 2!
    gen
;
: make-rand-interp-dist { f: frq f: amp dist -- gen }
    assert1( dist vct-length 1 max-table-size within )
    frq amp make-rand-interp { gen }
    dist inverse-integrate { data }
    data gen data-buffer !
    data vct-length gen buffer-length !
    ['] data-buffer-free gen get-gen-free !
    gen
;

\ === One-Pole, One-Zero, Two-Pole, Two-Zero ===
music5%
    cell% field get-xcoeff
    float% field flt-a0
    float% field flt-b1
    float% field flt-b2
    float% field flt-y1
    float% field flt-y2
    float% field flt-a1
    float% field flt-a2
    float% field flt-x1
    float% field flt-x2
end-struct simp-flt%

: a0@ ( gen -- r ) flt-a0 f@ ;
: a0! ( f: val gen -- ) flt-a0 f! ;
: a1@ ( gen -- r ) flt-a1 f@ ;
: a1! ( f: val gen -- ) flt-a1 f! ;
: a2@ ( gen -- r ) flt-a2 f@ ;
: a2! ( f: val gen -- ) flt-a2 f! ;
: b1@ ( gen -- r ) flt-b1 f@ ;
: b1! ( f: val gen -- ) flt-b1 f! ;
: b2@ ( gen -- r ) flt-b2 f@ ;
: b2! ( f: val gen -- ) flt-b2 f! ;
: x1@ ( gen -- r ) flt-x1 f@ ;
: x1! ( f: val gen -- ) flt-x1 f! ;
: x2@ ( gen -- r ) flt-x2 f@ ;
: x2! ( f: val gen -- ) flt-x2 f! ;
: y1@ ( gen -- r ) flt-y1 f@ ;
: y1! ( f: val gen -- ) flt-y1 f! ;
: y2@ ( gen -- r ) flt-y2 f@ ;
: y2! ( f: val gen -- ) flt-y2 f! ;
: xcoeff@ ( gen -- r ) dup get-xcoeff perform ;
: simp-flt-xcoeff@ { idx gen -- r }
    idx case
	0 of gen a0@ endof
	1 of gen a1@ endof
	2 of gen a2@ endof
    endcase
;
: xcoeff! { f: val idx gen -- }
    idx case
	0 of val gen a0! endof
	1 of val gen a1! endof
	2 of val gen a2! endof
    endcase
;
: ycoeff@ { idx gen -- r }
    idx case
	1 of gen b1@ endof
	2 of gen b2@ endof
    endcase
;
: ycoeff! { f: val idx gen -- }
    idx case
	1 of val gen b1! endof
	2 of val gen b2! endof
    endcase
;

simp-flt%
end-struct one-pole%

: order@ ( gen -- n ) buffer-order @ ;

: .one-pole { gen -- }
    ." #<one-pole: a0: " gen a0@ 3 f.r bs
    ." , b1: " gen b1@ 3 f.r bs
    ." , y1: " gen y1@ 3 f.r bs ." >"
;
: ?one-pole { obj -- f } s" one-pole" obj ?gen ;
: one-pole { f: input gen -- r }
    gen flt-a0 f@ input f* gen flt-b1 f@ gen flt-y1 f@ f* f- fdup gen flt-y1 f!
;
: op-run ( r1 r2 gen -- r ) fdrop one-pole ;
: make-one-pole { f: a0 f: b1 -- gen }
    one-pole% %alloc { gen }
    b1 gen flt-b1 f!
    a0 gen flt-a0 f!
    0e gen flt-y1 f!
    1 gen buffer-order !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] op-run gen get-mus-run !
    ['] .one-pole gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" one-pole" gen gen-name 2!
    gen
;

simp-flt%
end-struct one-zero%

: .one-zero { gen -- }
    ." #<one-zero: a0: " gen a0@ 3 f.r bs
    ." , a1: " gen a1@ 3 f.r bs
    ." , x1: " gen x1@ 3 f.r bs ." >"
;
: ?one-zero { obj -- f } s" one-zero" obj ?gen ;
: one-zero { f: input gen -- r }
    gen flt-a0 f@ input f* gen flt-a1 f@ gen flt-x1 f@ f* f+ input gen flt-x1 f!
;
: oz-run ( r1 r2 gen -- r ) fdrop one-zero ;
: make-one-zero { f: a0 f: a1 -- gen }
    one-zero% %alloc { gen }
    a1 gen flt-a1 f!
    a0 gen flt-a0 f!
    0e gen flt-x1 f!
    1 gen buffer-order !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] oz-run gen get-mus-run !
    ['] .one-zero gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" one-zero" gen gen-name 2!
    gen
;

simp-flt%
end-struct two-pole%

: .two-pole { gen -- }
    ." #<two-pole: a0: " gen a0@ 3 f.r bs
    ." , b1: " gen b1@ 3 f.r bs
    ." , b2: " gen b2@ 3 f.r bs
    ." , y1: " gen y1@ 3 f.r bs
    ." , y2: " gen y2@ 3 f.r bs ." >"
;
: ?two-pole { obj -- f } s" two-pole" obj ?gen ;
: two-pole { f: input gen -- r }
    gen flt-a0 f@ input f* gen flt-b1 f@ gen flt-y1 f@ f* f- gen flt-b2 f@ gen flt-y2 f@ f* f-
    gen flt-y1 f@ gen flt-y2 f! fdup gen flt-y1 f!
;
: tp-run ( r1 r2 gen -- r ) fdrop two-pole ;
: make-two-pole { f: a0 f: b1 f: b2 -- gen }
    two-pole% %alloc { gen }
    b2 gen flt-b2 f!
    b1 gen flt-b1 f!
    a0 gen flt-a0 f!
    0e gen flt-y1 f!
    0e gen flt-y2 f!
    2 gen buffer-order !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] tp-run gen get-mus-run !
    ['] .two-pole gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" two-pole" gen gen-name 2!
    gen
;
: make-ppolar { f: rad f: frq -- gen }
    1e frq hz>radians fcos rad f* f2* fnegate rad rad f* make-two-pole
;

simp-flt%
end-struct two-zero%

: .two-zero { gen -- }
    ." #<two-zero: a0: " gen a0@ 3 f.r bs
    ." , a1: " gen a1@ 3 f.r bs
    ." , a2: " gen a2@ 3 f.r bs
    ." , x1: " gen x1@ 3 f.r bs
    ." , x2: " gen x2@ 3 f.r bs ." >"
;
: ?two-zero { obj -- f } s" two-zero" obj ?gen ;
: two-zero { f: input gen -- r }
    gen flt-a0 f@ input f* gen flt-a1 f@ gen flt-x1 f@ f* f+ gen flt-a2 f@ gen flt-x2 f@ f* f+
    gen flt-x1 f@ gen flt-x2 f! input gen flt-x1 f!
;
: tz-run ( r1 r2 gen -- r ) fdrop two-zero ;
: make-two-zero { f: a0 f: a1 f: a2 -- gen }
    two-zero% %alloc { gen }
    a2 gen flt-a2 f!
    a1 gen flt-a1 f!
    a0 gen flt-a0 f!
    0e gen flt-x1 f!
    0e gen flt-x2 f!
    2 gen buffer-order !
    ['] simp-flt-xcoeff@ gen get-xcoeff !
    ['] tz-run gen get-mus-run !
    ['] .two-zero gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" two-zero" gen gen-name 2!
    gen
;
: make-zpolar { f: rad f: frq -- gen }
    1e frq hz>radians fcos rad f* f2* fnegate rad rad f* make-two-zero
;

\ === Formant ===
simp-flt%
    float% field flt-gain
    float% field flt-radius
    float% field flt-freq
end-struct formant%

: formant-radius@ ( gen -- r ) flt-radius f@ ;
: formant-radius! { f: R gen -- }
    gen flt-freq f@ hz>radians { f: fw }
    R gen flt-radius f!
    gen flt-gain f@ fw fsin f* 1e R R f* f- f* gen flt-a0 f!
    R fnegate gen flt-a2 f!
    R f2* fw fcos f* fnegate gen flt-b1 f!
    R R f* gen flt-b2 f!
;
: formant-frequency@ ( gen -- r ) flt-freq f@ ;
: formant-frequency! { f: val gen -- }
    val hz>radians { f: fw }
    gen flt-radius f@ { f: R }
    val gen flt-freq f!
    gen flt-gain f@ fw fsin f* 1e R R f* f- f* gen flt-a0 f!
    R f2* fw fcos f* fnegate gen flt-b1 f!
;

: .formant { gen -- }
    ." #<formant: a0: " gen flt-a0 f@ 1 f.r bs
    ." , a2: " gen a2@ 1 f.r bs
    ." , b1: " gen b1@ 1 f.r bs
    ." , b2: " gen b2@ 1 f.r bs
    ." , x1: " gen x1@ 1 f.r bs
    ." , x2: " gen x2@ 1 f.r bs
    ." , y1: " gen y1@ 1 f.r bs
    ." , y2: " gen y2@ 1 f.r bs ." >"
;
: ?formant { obj -- f } s" formant" obj ?gen ;
: formant { f: val gen -- r }
    gen flt-a0 f@ val f* { f: inval }
    inval gen flt-a2 f@ gen flt-x2 f@ f* f+ { f: tpinval }
    tpinval gen flt-b1 f@ gen flt-y1 f@ f* f- gen flt-b2 f@ gen flt-y2 f@ f* f- { f: output }
    gen flt-y1 f@ gen flt-y2 f!
    output gen flt-y1 f!
    gen flt-x1 f@ gen flt-x2 f!
    inval gen flt-x1 f!
    output
;
: formant-run ( r1 r2 gen -- r ) fdrop formant ;
\ 1e 440e 1e make-formant value gen
: make-formant { f: rad f: frq f: gain -- gen }
    formant% %alloc { gen }
    0e gen flt-x1 f!
    0e gen flt-x2 f!
    0e gen flt-y1 f!
    0e gen flt-y2 f!
    gain gen flt-gain f!
    rad gen flt-radius f!
    frq gen flt-freq f!
    frq hz>radians { f: fw }
    gain fw fsin f* 1e rad rad f* f- f* gen flt-a0 f!
    rad fnegate gen flt-a2 f!
    rad f2* fw fcos f* fnegate gen flt-b1 f!
    rad rad f* gen flt-b2 f!
    2 gen buffer-order !
    ['] formant-frequency@ gen get-freq !
    ['] formant-frequency! gen set-freq !
    ['] formant-run gen get-mus-run !
    ['] .formant gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" formant" gen gen-name 2!
    gen
;
: formant-bank { amps formants f: inval -- r }
    0e
    amps vct-length formants array-length = if
	amps vct-each i amps vct@ inval i formants array@ formant f* f+ loop
    then
;

\ === Filter ===
music5%
    cell% field flt-xcoeffs
    cell% field flt-ycoeffs
    cell% field flt-state
end-struct filter%

: gfm-xcoeffs@ ( gen -- w ) dup get-xcoeffs perform ;
: filter-xcoeffs@ ( gen -- w ) flt-xcoeffs @ ;
: ycoeffs@ ( gen -- w ) flt-ycoeffs @ ;

: filter-free { gen -- } gen flt-state @ free-vct gen free throw ;

: .filter { gen -- }
    ." #<filter: order: " gen order@ . bs
    ." , xcoeffs: " gen filter-xcoeffs@ .vct
    ." , ycoeffs: " gen ycoeffs@ .vct
    ." , state: " gen flt-state @ .vct ." >"
;
: ?filter { obj -- f } s" filter" obj ?gen ;
: filter { f: input gen -- r }
    0e
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-order @ 1- -do
	i fstate vct@  i gen flt-xcoeffs @ vct@  f*  f+
	0 fstate vct@  i gen flt-ycoeffs @ vct@  i fstate vct@  f*  f-
	0 fstate vct!
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@  0 gen flt-xcoeffs @ vct@  f*  f+
;
: filter-run ( r1 r2 gen -- r ) fdrop filter ;
\ 1 2 make-vct 2 make-vct make-filter value gen
: make-filter { ord xcoeffs ycoeffs -- gen }
    assert1( ord 0> xcoeffs ?vct ycoeffs ?vct and and )
    filter% %alloc { gen }
    ord gen buffer-order !
    ord gen buffer-length !
    xcoeffs vct-length ycoeffs vct-length max make-vct gen flt-state !
    xcoeffs gen flt-xcoeffs !
    ycoeffs gen flt-ycoeffs !
    ['] filter-xcoeffs@ gen get-xcoeffs !
    ['] filter-run gen get-mus-run !
    ['] .filter gen get-inspect !
    ['] filter-free gen get-gen-free !
    s" filter" gen gen-name 2!
    gen
;

\ === FIR-Filter ===
filter%
end-struct fir-filter%

: .fir-filter { gen -- }
    ." #<fir-filter: order: " gen order@ . bs
    ." , xcoeffs: " gen filter-xcoeffs@ .vct
    ." , state: " gen flt-state @ .vct ." >"
;
: ?fir-filter { obj -- f } s" fir-filter" obj ?gen ;
: fir-filter { f: input gen -- r }
    0e					\ result
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-order @ 1- -do
	i fstate vct@  i gen flt-xcoeffs @ vct@  f*  f+
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@  0 gen flt-xcoeffs @ vct@  f*  f+
;
: fir-run ( r1 r2 gen -- r ) fdrop fir-filter ;
: make-fir-filter { ord xcoeffs -- gen }
    assert1( ord 0> xcoeffs ?vct and )
    fir-filter% %alloc { gen }
    ord gen buffer-order !
    ord gen buffer-length !
    xcoeffs gen flt-xcoeffs !
    xcoeffs vct-length make-vct gen flt-state !
    ['] filter-xcoeffs@ gen get-xcoeffs !
    ['] fir-run gen get-mus-run !
    ['] .fir-filter gen get-inspect !
    ['] filter-free gen get-gen-free !
    s" fir-filter" gen gen-name 2!
    gen
;

\ === IIR-Filter ===
filter%
end-struct iir-filter%

: .iir-filter { gen -- }
    ." #<iir-filter: order: " gen order@ . bs
    ." , ycoeffs: " gen ycoeffs@ .vct
    ." , state: " gen flt-state @ .vct ." >"
;
: ?iir-filter { obj -- f } s" iir-filter" obj ?gen ;
: iir-filter { f: input gen -- r }
    gen flt-state @ { fstate }
    input 0 fstate vct!
    0 gen buffer-order @ 1- -do
	0 fstate vct@  i gen flt-ycoeffs @ vct@  i fstate vct@  f*  f-  0 fstate vct!
	i 1- fstate vct@  i fstate vct!
    1 -loop
    0 fstate vct@
;
: iir-run ( r1 r2 gen -- r ) fdrop iir-filter ;
: make-iir-filter { ord ycoeffs -- gen }
    assert1( ord 0> ycoeffs ?vct and )
    iir-filter% %alloc { gen }
    ord gen buffer-order !
    ord gen buffer-length !
    ycoeffs gen flt-ycoeffs !
    ycoeffs vct-length make-vct gen flt-state !
    ['] filter-xcoeffs@ gen get-xcoeffs !
    ['] iir-run gen get-mus-run !
    ['] .iir-filter gen get-inspect !
    ['] filter-free gen get-gen-free !
    s" iir-filter" gen gen-name 2!
    gen
;
' spectrum>coeffs alias make-fir-coeffs

\ === Delay ===
music5%
    float% field xscale
    float% field yscale
end-struct delay%

: feedback@ ( gen -- r ) yscale f@ ;
: feedback! ( f: val gen -- ) yscale f! ;
: feedforward@ ( gen -- r ) xscale f@ ;
: feedforward! ( f: val gen -- ) xscale f! ;

: .delay { gen -- }
    ." #<delay: size: " gen gfm-length@ . bs
    ." , loc: " gen location @ . bs
    ." , line: " gen data-buffer @ .vct ." >"
;
: ?delay { obj -- f } s" delay" obj ?gen ;
: tap { f: loc gen -- r }
    loc f0= if gen location @ else gen location @ loc f>s - gen buffer-length @ mod 0 max then
    gen data-buffer @ vct@
;
: delay-tick { f: input gen -- r }
    input gen location @ gen data-buffer @ vct!
    1 gen location +!
    gen buffer-length @ gen location @ <= if 0 gen location ! then
;
: delay { f: input f: pm gen -- r } pm gen tap ( result ) input gen delay-tick ;
: make-delay { size -- gen }
    assert1( size 0 max-table-size within )
    delay% %alloc { gen }
    size gen buffer-length !
    size make-vct gen data-buffer !
    0 gen location !
    mus-interp-none gen interpolation-type !
    ['] delay gen get-mus-run !
    ['] .delay gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" delay" gen gen-name 2!
    gen
;

\ === Comb ===
delay%
end-struct comb%

: .comb { gen -- }
    ." #<comb: size: " gen gfm-length@ . bs
    ." , loc: " gen location @ . bs
    ." , feedback: " gen feedback@ 3 f.r bs
    ." , line: " gen data-buffer @ .vct ." >"
;    
: ?comb { obj -- f } s" comb" obj ?gen ;
: comb { f: input f: pm gen -- r } gen yscale f@ pm gen tap f* input f+ 0e gen delay ;
: make-comb { f: scal size -- gen }
    assert1( size 0 max-table-size within )
    comb% %alloc { gen }
    size gen buffer-length !
    size gen buffer-order !
    size make-vct gen data-buffer !
    0 gen location !
    scal gen yscale f!
    mus-interp-none gen interpolation-type !
    ['] comb gen get-mus-run !
    ['] .comb gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" comb" gen gen-name 2!
    gen
;

\ === Notch ===
delay%
end-struct notch%

: .notch { gen -- }
    ." #<notch: size: " gen gfm-length@ . bs
    ." , loc: " gen location @ . bs
    ." , feedforward: " gen feedforward@ 3 f.r bs
    ." , line: " gen data-buffer @ .vct ." >"
;    
: ?notch { obj -- f } s" notch" obj ?gen ;
: notch { f: input f: pm gen -- r } gen xscale f@ input f* input pm gen delay f+ ;
: make-notch { f: scal size -- gen }
    assert1( size 0 max-table-size within )
    notch% %alloc { gen }
    size gen buffer-length !
    size gen buffer-order !
    size make-vct gen data-buffer !
    0 gen location !
    scal gen xscale f!
    mus-interp-none gen interpolation-type !
    ['] notch gen get-mus-run !
    ['] .notch gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" notch" gen gen-name 2!
    gen
;

\ === All-Pass ==
delay%
end-struct all-pass%

: .all-pass { gen -- }
    ." #<all-pass: size: " gen gfm-length@ . bs
    ." , loc: " gen location @ . bs
    ." , feedback: " gen feedback@ 3 f.r bs
    ." , feedforward: " gen feedforward@ 3 f.r bs
    ." , line: " gen data-buffer @ .vct ." >"
;
: ?all-pass { obj -- f } s" all-pass" obj ?gen ;
: all-pass { f: input f: pm gen -- r }
    gen yscale f@ pm gen tap f* input f+ { f: d-in }
    d-in pm gen delay gen xscale f@ d-in f* f+
;
: make-all-pass { f: fbck f: ffw size -- gen )
    assert1( size 0 max-table-size within )
    all-pass% %alloc { gen }
    size gen buffer-length !
    size gen buffer-order !
    size make-vct gen data-buffer !
    0 gen location !
    ffw gen xscale f!
    fbck gen yscale f!
    mus-interp-none gen interpolation-type !
    ['] all-pass gen get-mus-run !
    ['] .all-pass gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" all-pass" gen gen-name 2!
    gen
;

\ === Average ===
delay%
end-struct average%

: .average { gen -- }
    ." #<average: size: " gen gfm-length@ . bs
    ." , loc: " gen location @ . bs
    ." , line: " gen data-buffer @ .vct ." >"
;
: ?average { obj -- f } s" average" obj ?gen ;
: average { f: input gen -- r }
    input  input 0e gen delay  f-  gen xscale f@  f+  gen xscale f!
    gen xscale f@ gen yscale f@ f*
;
: average-run ( r1 r2 gen -- r ) fdrop average ;
: make-average { size -- gen }
    assert1( size 0 max-table-size within )
    average% %alloc { gen }
    0 gen location !
    0e gen xscale f!
    size s>f 1/f gen yscale f!
    size make-vct gen data-buffer !
    size gen buffer-length !
    size gen buffer-order !
    ['] average-run gen get-mus-run !
    ['] .average gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" average" gen gen-name 2!
    gen
;

\ === Wave-Train ===
oscil%
    cell% field wave-buffer
end-struct wave-train%

: wt-frequency@ ( gen -- r ) freq f@ ;
: wt-frequency! ( f: val gen -- ) freq f! ;
: wt-phase@ { gen -- r } gen phase f@ two-pi f* gen buffer-length @ s>f f/ two-pi fmod ;
: wt-phase! { f: val gen -- } val gen buffer-length @ s>f f* two-pi f/ gen phase f! ;
: gfm-increment@ ( gen -- r ) fill-time f@ ;
: gfm-increment! ( f: val gen -- ) fill-time f! ;

: wave-train-free { gen -- } gen wave-buffer @ gfm-mus-free ;

: .wave-train { gen -- }
    ." #<wave-train: freq: " gen wt-frequency@ 3 f.r bs
    ." , phase: " gen wt-phase@ 3 f.r bs
    ." , size: " gen gfm-length@ . bs
    ." , wave: " gen data-buffer @ .vct
    ." , buf: " gen wave-buffer @ data-buffer @ .vct ." >"
;
: ?wave-train { obj -- f } s" wave-train" obj ?gen ;
: wave-train { f: fm gen -- r }
    gen wave-buffer @ { b }
    b ?buffer-empty if
	b data-buffer @ { buf }
	gen buffer-length @ 0 do
	    i buf vct@ gen data-buffer @ gen phase f@ i s>f f+ array-interp f+ i buf vct!
	loop
	b fill-time f@ mus-srate@ gen freq f@ fm 1e radians>hz f* f+ f/ f+ b fill-time f!
	false b buf-empty !
    then
    b buffer>sample
;
: wt-run ( r1 r2 gen -- r ) fdrop wave-train ;
\ 440e  0e  *table-size* make-vct  mus-interp-linear make-wave-train value gen
: make-wave-train { f: frq f: ph wav tp -- gen }
    assert1( wav ?vct )
    wave-train% %alloc { gen } wav vct-length { len }
    wav gen data-buffer !
    len gen buffer-length !
    len 0e make-buffer gen wave-buffer !
    frq gen freq f!
    ph f0= if 0e else ph two-pi f/ gen buffer-length @ s>f f* then gen phase f!
    tp gen interpolation-type !
    ['] wt-frequency@ gen get-freq !
    ['] wt-frequency! gen set-freq !
    ['] wt-phase@ gen get-phase !
    ['] wt-phase! gen set-phase !
    ['] wt-run gen get-mus-run !
    ['] .wave-train gen get-inspect !
    ['] wave-train-free gen get-gen-free !
    s" wave-train" gen gen-name 2!
    gen
;

\ === SSB-AM ===
filter%
    cell% field shift-up
    cell% field sin-osc
    cell% field cos-osc
    cell% field ssb-dly
    cell% field ssb-hilbert
end-struct ssb-am%

: ssb-am-frequency@ ( gen -- r ) sin-osc @ freq f@ radians>hz ;
: ssb-am-frequency! { f: val gen -- }
    val gen sin-osc @ hz>radians freq f!
    val hz>radians gen cos-osc @ hz>radians freq f!
;
: ssb-am-phase@ ( gen -- r ) sin-osc @ phase f@ two-pi fmod ;
: ssb-am-phase! { f: val gen -- }
    val gen sin-osc @ phase f!
    val half-pi f+ gen cos-osc @ phase f!
;
: ssb-am-xcoeffs@ ( gen -- w ) ssb-hilbert @ flt-xcoeffs @ ;
: ssb-am-xcoeff@ { idx gen -- r }
    gen ssb-hilbert @ to gen
    idx case
	0 of gen a0@ endof
	1 of gen a1@ endof
	2 of gen a2@ endof
    endcase
;
: ssb-am-free { gen -- }
    gen sin-osc @ gfm-mus-free
    gen cos-osc @ gfm-mus-free
    gen ssb-dly @ gfm-mus-free
    gen ssb-hilbert @ gfm-mus-free
    gen free throw
;

: .ssb-am { gen -- }
    ." #<ssb-am: " gen sin-osc @ .pp-print
    ." , order: " gen order@ . bs  ." >"
;
: ?ssb-am { obj -- f } s" ssb-am" obj ?gen ;
: ssb-am { f: insig f: fm gen -- r }
    gen shift-up @ if
	fm 0e gen cos-osc @ oscil  insig 0e gen ssb-dly @ delay  f*
	fm 0e gen sin-osc @ oscil  insig gen ssb-hilbert @ fir-filter  f*  f-
    else
	fm 0e gen cos-osc @ oscil  insig 0e gen ssb-dly @ delay  f*
	fm 0e gen sin-osc @ oscil  insig gen ssb-hilbert @ fir-filter  f*  f+
    then
;
\ 440e 40 make-ssb-am value gen
: make-ssb-am { f: frq ord -- gen }
    assert1( ord 0> )
    ssb-am% %alloc { gen }
    frq f0> gen shift-up !
    frq fabs      0e make-oscil gen sin-osc !
    frq fabs half-pi make-oscil gen cos-osc !
    ord make-delay gen ssb-dly !
    ord 2* { len }
    len make-vct { xcofs }
    0 { idx }
    0e 0e { f: denom f: num }
    ord ord negate do
	i s>f pi f* to denom
	1e denom fcos f- to num
	i 0= if
	    0e
	else
	    num denom f/  0.54e  0.46e  denom ord s>f f/ fcos  f*  f+  f*
	then idx xcofs vct!  idx 1+ to idx
    loop
    len xcofs make-fir-filter gen ssb-hilbert !
    gen ssb-dly @ data-buffer @ gen data-buffer !
    ord gen buffer-length !
    ord gen buffer-order !
    1 gen cosines !
    mus-interp-none gen interpolation-type !
    ['] ssb-am-frequency@ gen get-freq !
    ['] ssb-am-frequency! gen set-freq !
    ['] ssb-am-phase@ gen get-phase !
    ['] ssb-am-phase! gen set-phase !
    ['] ssb-am-xcoeffs@ gen get-xcoeffs !
    ['] ssb-am gen get-mus-run !
    ['] .ssb-am gen get-inspect !
    ['] ssb-am-free gen get-gen-free !
    s" ssb-am" gen gen-name 2!
    gen
;

\ === Snd-IO ===
\ Sun/NeXT
08 constant size-loc
12 constant type-loc
16 constant srate-loc
20 constant chans-loc
28 constant sun-comment-loc-begin
00 constant gfm-format-ubyte
03 constant gfm-format-bshort
41 constant gfm-format-lshort

variable retval retval off

buffer%
    cell% field io-fd
    cell% field io-chans
    cell% field io-srate
    cell% field io-sample
    cell% field io-frame
    cell% field io-header-type
    cell% field io-data-format
    cell% field io-file-length
    cell% field io-snd-data-loc
    cell% field io-buffer-index
    cell% field buf-beg
    cell% field buf-end
    double% field ?io-input
    double% field ?io-output
    double% field io-file-name
    double% field io-comment
end-struct snd-io%

: file-name@ ( gen -- d: fname ) io-file-name 2@ ;
: channels@ ( gen -- n ) io-chans @ ;

: ?input { obj -- f } try obj ?io-input 2@ s" io-input" str= recover drop false endtry ;
: ?output { obj -- f } try obj ?io-output 2@ s" io-output" str= recover drop false endtry ;

: int>4bytes { val -- c1 c2 c3 c4 }
    val $ff and val 8 rshift $ff and val 16 rshift $ff and val 24 rshift $ff and
;
: write-bint { val fd -- } val int>4bytes 4 0 do fd emit-file throw loop ;
: bshorts>int { addr -- u }
    addr c@ 24 lshift addr 1+ c@ 16 lshift or addr 2 + c@ 8 lshift or addr 3 + c@ or
;
: short>float ( s -- r ) s>f 32768e f/ ;
: float>short ( r -- s ) 32768e f* f>s dup $ff and 8 lshift swap 8 rshift $ff and or ;
: float>dac-short ( r -- s ) 1 15 lshift s>f f* f>s ;

: make-next-header { gen -- }
    gen io-comment 2@ { d: comm }
    comm nip sun-comment-loc-begin + cell - gen io-snd-data-loc !
    gen io-fd @ { fd }
    s" .snd" fd write-file throw
    gen io-snd-data-loc @ fd write-bint
    0 fd write-bint
    gen io-data-format @ fd write-bint
    gen io-srate @ fd write-bint
    gen io-chans @ fd write-bint
    comm fd write-file throw
    fd flush-file throw
;
: read-next-header { gen -- }
    gen io-fd @ { fd }
    gfm-format-lshort gen io-data-format !
    mus-next gen io-header-type !
    0 0 gen io-comment 2!
    0 retval !
    retval 4 fd read-file throw drop
    retval bshorts>int gen io-snd-data-loc !
    retval 4 fd read-file throw drop
    retval bshorts>int gen io-file-length !
    retval 4 fd read-file throw drop
    retval bshorts>int gen io-data-format !
    retval 4 fd read-file throw drop
    retval bshorts>int gen io-srate !
    retval 4 fd read-file throw drop
    retval bshorts>int gen io-chans !
    retval gen io-snd-data-loc @ cell+ sun-comment-loc-begin - fd read-file throw
    retval swap gen io-comment 2!
;
: read-wave-header { gen -- }
    gen io-fd @ { fd }
    gfm-format-lshort gen io-data-format !
    mus-riff gen io-header-type !
    0 0 gen io-comment 2!
    0 retval !
    retval 100 fd read-file throw drop 0 { loc }
    100 0 ?do retval i + 4 s" fmt " str= if i to loc leave then 4 +loop
    retval loc + 10 + c@ gen io-chans !
    retval loc + 12 + @ gen io-srate !
    retval loc + 22 + c@ dup 8 = if 0 else 41 then gen io-data-format !
    100 loc - 0 do retval i + 4 s" data" str= if i to loc leave then 4 +loop
    loc 4 + gen io-snd-data-loc !
;
: read-header { gen -- )
    0 retval !
    retval 4 gen io-fd @ read-file throw drop
    retval 4 s" .snd" str= if
	gen read-next-header
    else
	retval 4 s" RIFF" str= if
	    gen read-wave-header
	else
	    true abort" read-header: sorry, fsndlib can only read simple Sun/NeXT and RIFF headers"
	then
    then
;

: io-flush-buffer { gen -- }
    gen io-file-length @ gen io-chans @ + gen io-sample @ * { len }
    gen buffer-length @  len  > if
	len
    else
	gen io-buffer-index @ 1+
    then
    gen data-buffer @ swap gen io-fd @ write-file throw
;
: io-set-file-location { gen -- }
    gen location @ gen io-sample @ * gen io-snd-data-loc @ + s>d gen io-fd @ reposition-file throw
;
: io-reread-file { gen -- }
    gen io-set-file-location
    gen data-buffer @ gen buffer-length @ erase
    gen data-buffer @ gen buffer-length @ gen io-fd @ read-file throw drop
    gen io-set-file-location
;

: io-open-output { d: fname chans data-format header-type d: comment flag gen -- }
    assert1( chans 0> )
    fname gen io-file-name 2!
    chans gen io-chans !
    srate@ gen io-srate !
    gfm-format-lshort gen io-data-format !	\ ignore data format
    mus-next gen io-header-type !		\ ignore header type
    flag if comment gen io-comment 2! then
    short gen io-sample !
    file-buffer-size@ { len }
    0 gen io-buffer-index !
    len gen buffer-length !
    len allocate throw { buf }
    buf len erase
    buf gen data-buffer !
    fname flag if r/w create-file else r/w open-file then throw gen io-fd !
    gen flag if make-next-header else read-header then
    chans short * gen io-frame !
    0 gen location !
    0 gen buf-beg !
    len 1- short / gen buf-end !
    1 gen io-file-length !
    0 0 gen ?io-input 2!
    s" io-output" gen ?io-output 2!
;
: io-open-input { d: fname gen -- }
    fname gen io-file-name 2!
    fname r/o open-file throw gen io-fd !
    short gen io-sample !
    file-buffer-size@ { len }
    0 gen io-buffer-index !
    gen read-header
    gen io-chans @ shorts gen io-frame !
    0 gen location !
    0 gen buf-beg !
    len 1- short / gen buf-end !
    len gen buffer-length !
    len allocate throw gen data-buffer !
    gen io-reread-file
    s" io-input" gen ?io-input 2!
    0 0 gen ?io-output 2!
;

\ === Mus-Close ===
: mus-close { gen -- }
    gen io-fd @ { fd }
    gen ?output if
	gen io-flush-buffer
	size-loc s>d fd reposition-file throw
	gen io-file-length @ fd write-bint
    then
    gen data-buffer @ free throw
    fd close-file throw
;

\ === Out-Any ===
: out-any { f: val samp chan gen -- }
    gen io-chans @ samp * { loc }
    loc gen location !
    loc gen io-file-length @ max gen io-file-length !
    loc gen buf-beg @ gen buf-end @ within if
	loc gen buf-beg @ - gen io-sample @ * gen io-buffer-index !
    else
	gen io-flush-buffer
	0 gen io-buffer-index !
	loc gen buf-beg !
	loc gen buffer-length @ 1- gen io-sample @ / + gen buf-end !
	gen io-reread-file
    then
    val float>short gen data-buffer @ gen io-buffer-index @ + chan shorts + s+!
;
: outa { f: val loc gen -- } val loc 0 gen out-any ;
: outb { f: val loc gen -- } val loc 1 gen out-any ;
: outc { f: val loc gen -- } val loc 2 gen out-any ;
: outd { f: val loc gen -- } val loc 3 gen out-any ;

\ === Sample>File ===
snd-io%
end-struct sample>file%

: .sample>file { gen -- }
    ." #<sample>file: " gen file-name@ type
    ." , channels: " gen channels@ . bs
    ." , srate: " gen io-srate @ . bs
    ." , buffer-size: " gen gfm-length@ . bs ." >"
;
: ?sample>file { obj -- f } s" sample>file" obj ?gen ;
' out-any alias sample>file
: make-sample>file { d: fname chans fmt tp d: commt -- gen }
    sample>file% %alloc { gen }
    fname chans fmt tp commt true gen io-open-output
    ['] .sample>file gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sample>file" gen gen-name 2!
    gen
;

\ === In-Any ===
: in-any { samp chan gen -- r }
    samp 0 gen io-file-length @ gen io-chans @ shorts + within if
	gen io-chans @ samp * { loc }
	loc gen location !
	loc gen buf-beg @ gen buf-end @ within if
	    loc gen buf-beg @ - gen io-sample @ * gen io-buffer-index !
	else
	    0 gen io-buffer-index !
	    loc gen buf-beg !
	    loc gen buffer-length @ 1- gen io-sample @ / + gen buf-end !
	    gen io-reread-file
	then
	gen data-buffer @ gen io-buffer-index @ + chan shorts + s@ short>float
    else
	0e
    then
;
: ina { loc gen -- r } loc 0 gen in-any ;
: inb { loc gen -- r } loc 1 gen in-any ;

\ === Readin ===
snd-io%
    cell% field rd-channel
end-struct readin%

: channel@ ( gen -- n ) rd-channel @ ;

: .readin { gen -- }
    ." #<readin: " gen file-name@ type
    ." , channel: " gen channel@ . bs
    ." , location: " gen location @ . bs
    ." , direction: " gen gfm-increment@ f>s . bs ." >"
;
: ?readin { obj -- f } s" readin" obj ?gen ;
: readin { gen -- r }
    gen location @ 0 max gen rd-channel @ gen in-any
    gen fill-time f@ f>s gen location +!
;
\ fname 0 0 1 make-readin value gen
: make-readin { d: fname chan start dir -- gen }
    assert1( chan 0>= )
    readin% %alloc { gen }
    fname gen io-open-input
    start gen location !
    chan gen rd-channel !
    dir s>f gen fill-time f!
    start gen io-sample @ * gen buf-end @ min gen io-buffer-index !
    ['] .readin gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" readin" gen gen-name 2!
    gen 
;

\ === File>Sample ===
snd-io%
end-struct file>sample%

: .file>sample { gen -- }
    ." #<file>sample: " gen file-name@ type
    ." , channels: " gen channels@ . bs
    ." , srate: " gen io-srate @ . bs ." >"
;
: ?file>sample { obj -- f } s" file>sample" obj ?gen ;
' in-any alias file>sample
: make-file>sample { d: fname -- gen }
    file>sample% %alloc { gen }
    fname gen io-open-input
    ['] .file>sample gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" file>sample" gen gen-name 2!
    gen
;

\ === Frame ===
snd-io%
end-struct frame%

: frame@ ( idx gen -- r ) data-buffer @ vct@ ;
: frame! ( f: val idx gen -- ) data-buffer @ vct! ;

: .frame { gen -- }
    ." #<frame: chans: " gen gfm-length@ . bs
    ." , vals: " gen data-buffer @ .vct ." >"
;
: ?frame { obj -- f } s" frame" obj ?gen ;
: make-simple-frame { chns -- gen }
    assert1( chns 0> )
    frame% %alloc { gen }
    chns gen buffer-length !
    chns gen io-chans !
    ['] .frame gen get-inspect !
    ['] data-buffer-free gen get-gen-free !
    s" frame" gen gen-name 2!
    gen
;
: make-frame { chns -- gen }
    chns make-simple-frame { gen }
    chns make-vct gen data-buffer !
    gen
;
: >frame ( chans-values chans -- gen ) { chns }
    chns make-simple-frame { gen }
    chns >vct gen data-buffer !
    gen
;
: frame>vct ( gen -- v ) data-buffer @ vct-copy ;
: vct>frame { v -- gen }
    v vct-length make-frame { gen }
    v vct-copy gen data-buffer !
    gen
;
: frame+ { fr1 fr2 -- gen }
    fr1 frame>vct fr2 frame>vct { v1 v2 }
    v1 v2 vct+
    v1 vct>frame			\ result
    v1 free-vct v2 free-vct
;
: frame* { fr1 fr2 -- gen }
    fr1 frame>vct fr2 frame>vct { v1 v2 }
    v1 v2 vct*
    v1 vct>frame			\ result
    v1 free-vct v2 free-vct
;

\ === Mixer ===
snd-io%
end-struct mixer%

: mixer@ ( in out gen -- r ) >r swap r> ( out in gen ) data-buffer @ array@ vct@ ;
: mixer! ( f: val in out gen -- ) >r swap r> ( out in gen ) data-buffer @ array@ vct! ;

: mixer-free { gen -- }
    gen buffer-length @ 0 do i gen data-buffer @ array@ free-vct loop
    gen data-buffer @ free-array
    gen free throw
;
: .mixer-matrix { gen -- }
    gen gfm-length@ { chns }
    chns 0 do
	." [" chns 0 do i j gen mixer@ 3 f.r loop bs ." ] "
    loop bs
;
: .mixer { gen -- }
    ." #<mixer: chans: " gen gfm-length@ . bs
    ." , vals: [" gen .mixer-matrix ." ]>"
;
: ?mixer { obj -- f } s" mixer" obj ?gen ;
: make-simple-mixer { chns -- gen }
    assert1( chns 0> )
    mixer% %alloc { gen }
    chns gen buffer-length !
    chns gen io-chans !
    ['] .mixer gen get-inspect !
    ['] mixer-free gen get-gen-free !
    s" mixer" gen gen-name 2!
    gen
;
: make-mixer { chns -- gen }
    chns make-simple-mixer { gen }
    chns 0 do chns make-vct loop chns >array gen data-buffer !
    gen
;
: make-identity-mixer { chns -- gen }
    chns make-mixer { mx }
    chns 0 do 1e i i mx mixer! loop
    mx
;
: >mixer ( chans-in-out-values chans -- gen ) { chns }
    chns make-simple-mixer { gen }
    chns make-array { mx }
    -1 chns 1- -do chns >vct i mx array! 1 -loop
    mx gen data-buffer !
    gen
;
: mixer+ { mx1 mx2 -- gen }
    mx1 gfm-length@ mx2 gfm-length@ min { chns }
    chns 0 do
	chns 0 do i j mx1 mixer@  i j mx2 mixer@  f+ loop
    loop chns >mixer
;
: mixer* { mx1 mx2 -- gen }
    mx1 gfm-length@ mx2 gfm-length@ min { chns }
    chns make-mixer { mx3 }
    chns 0 do
	chns 0 do
	    chns 0 do
		i j mx3 mixer@  i k mx1 mixer@   k j mx2 mixer@   f*  f+  i j mx3 mixer!
	    loop
	loop
    loop
    mx3
;
: mixer-scale { f: scl mx -- gen }
    mx gfm-length@ { chns }
    chns 0 do
	chns 0 do i j mx mixer@ scl f* loop
    loop chns >mixer
;

: frame>frame { fr1 mx -- fr2 }
    fr1 gfm-length@ mx gfm-length@ min make-vct { v }
    v vct-each
	fr1 gfm-length@ 0 do j v vct@  i fr1 frame@  i j mx mixer@  f* f+  j v vct! loop
    loop
    v vct>frame				\ result
    v free-vct
;
: sample>frame { f: val mx -- fr }
    mx gfm-length@ make-vct { v }
    v vct-each  0 i mx mixer@  val f*  i v vct! loop
    v vct>frame				\ result
    v free-vct
;
: frame>sample { fr mx -- r }
    0e fr gfm-length@ mx gfm-length@ min 0 do i fr frame@  0 i mx mixer@  f*  f+ loop
;

\ === File>Frame ===
snd-io%
end-struct file>frame%

: .file>frame { gen -- }
    ." #<file>frame " gen file-name@ type
    ." , channels: " gen channels@ . bs
    ." , srate: " gen io-srate @ . bs ." >"
;
: ?file>frame { obj -- f } s" file>frame" obj ?gen ;
: file>frame { samp gen -- fr }
    gen channels@ make-vct { v }
    v vct-each  samp i gen file>sample  i v vct!  loop
    v vct>frame
    v free-vct
;
: make-file>frame { d: fname -- gen }
    file>frame% %alloc { gen }
    fname gen io-open-input
    ['] .file>frame gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" file>frame" gen gen-name 2!
    gen
;

\ === Frame>File ===
snd-io%
end-struct frame>file%

: .frame>file { gen -- }
    ." #<frame>file: " gen file-name@ type
    ." , channels: " gen channels@ . bs
    ." , srate: " gen io-srate @ . bs
    ." , buffer-size: " gen gfm-length@ . bs ." >"
;
: ?frame>file { obj -- f } s" frame>file" obj ?gen ;
: frame>file { fr samp gen -- }
    fr frame>vct { v }
    v vct-each  i v vct@  samp i gen sample>file  loop
    v free-vct
;
: make-frame>file { d: fname chans fmt tp -- gen }
    frame>file% %alloc { gen }
    fname chans fmt tp 0 0 true gen io-open-output
    ['] .frame>file gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" frame>file" gen gen-name 2!
    gen
;

\ === Locsig ===
snd-io%
    cell% field loc-frame
    cell% field rev-frame
    cell% field loc-output
    cell% field loc-reverb
    cell% field rev-chans
    cell% field loc-type
    float% field loc-dist
    float% field loc-rev-amount
end-struct locsig%

: locsig@ ( chan gen -- r ) loc-frame @ vct@ ;
: locsig! ( f: val chan gen -- ) loc-frame @ vct! ;
: locsig-reverb@ ( chan gen -- r ) rev-frame @ vct@ ;
: locsig-reverb! ( f: val chan gen -- r ) rev-frame @ vct! ;

: locsig-free { gen -- }
    gen loc-frame @ free-vct
    gen loc-reverb @ if gen rev-frame @ free-vct then
    gen free throw
;

: .locsig { gen -- }
    ." #<locsig: chans: " gen channels@ . bs
    ." , frame: " gen loc-frame @ .vct
    gen loc-reverb @ if
	." , rchans: " gen rev-chans @ . bs
	." , rframe: " gen rev-frame @ .vct
    then ." >"
;
: locsig { f: val loc gen -- }
    gen loc-output @ if
	gen io-chans @ 0 do val i gen loc-frame @ vct@ f* loc i gen loc-output @ out-any loop
    then
    gen loc-reverb @ if
	gen rev-chans @ 0 do val i gen rev-frame @ vct@ f* loc i gen loc-reverb @ out-any loop
    then
;
: ?locsig { obj -- f } s" locsig" obj ?gen ;
: fill-locsig { arr chans f: degree f: scaler tp -- arr' }
    chans 1 = if
	scaler 0 arr vct!
    else degree f0< if degree -360e f/ fround 360e f* degree f+ to degree then
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
	    scaler 1e frac f- f* left arr vct!
	    scaler frac f* right arr vct!
	else
	    half-pi 0.5e frac f- f* { f: ldeg }
	    scaler 2e fsqrt f* f2/ to scaler
	    ldeg fcos { f: cc }
	    ldeg fsin { f: ss }
	    scaler cc ss f+ f* left arr vct!
	    scaler cc ss f- f* right arr vct!
	then
    then
    arr
;
: make-locsig { f: degr f: dist f: reverb chns output revput tp -- gen }
    assert1( chns 0> )
    dist 1e f> if dist 1/f else 1e then to dist
    chns to *channels*
    locsig% %alloc { gen }
    dist gen loc-dist f!
    chns gen io-chans !
    chns gen buffer-length !
    output gen loc-output !
    tp gen loc-type !
    chns make-vct chns degr dist tp fill-locsig gen loc-frame !
    revput if
	revput io-chans @ { rchns }
	rchns to *reverb-channels*
	rchns gen rev-chans !
	revput gen loc-reverb !
	reverb gen loc-rev-amount f!
	rchns make-vct rchns degr reverb dist fsqrt f* tp fill-locsig gen rev-frame !
    else
	false gen loc-reverb !
    then
    ['] .locsig gen get-inspect !
    ['] locsig-free gen get-gen-free !
    s" locsig" gen gen-name 2!
    gen
;
: move-locsig { f: degr f: dist gen -- }
    dist 1e f> if dist 1/f else 1e then to dist
    gen loc-reverb @ if
	gen rev-frame @ gen rev-chans @ degr gen loc-rev-amount f@ dist fsqrt f* gen loc-type @
	fill-locsig drop
    then
    gen loc-frame @ gen io-chans @ degr dist gen loc-type @ fill-locsig drop
;

\ === File>Array>, Array>File ===
: file>array { d: fname chan start samps -- data }
    fname chan start 1 make-readin { gen }
    samps make-vct { data }
    samps 0 do gen readin i data vct! loop
    gen mus-close
    data
;
: array>file { data len srate chans d: fname -- }
    srate srate!
    fname chans *clm-data-format* *clm-header-type* s" array>file" make-sample>file { gen }
    len 0 do chans 0 do j i + data vct@ j i gen out-any loop chans +loop
    gen mus-close
;

\ === Sound ===
: sound-chans ( d: fname -- n )
    make-file>sample { gen }
    gen channels@
    gen mus-close
;
: sound-srate ( d: fname -- n )
    make-file>sample { gen }
    gen io-srate @
    gen mus-close
;
: sound-samples ( d: fname -- n )
    make-file>sample { gen }
    gen io-fd @ file-size throw d>s gen io-snd-data-loc @ - gen io-sample @ /
    gen mus-close
;
: sound-frames { d: fname -- n } fname sound-samples fname sound-chans / ;
: sound-duration { d: fname -- r } fname sound-frames s>f fname sound-srate s>f f/ ;
: .data-format ( n -- )
    case
	gfm-format-bshort of ." big endian short (16 bits)" endof
	mus-bshort of ." big endian short (16 bits)" endof
	gfm-format-lshort of ." little endian short (16 bits)" endof
	mus-lshort of ." little endian short (16 bits)" endof
	gfm-format-ubyte  of ." unsigned byte (8 bits)" endof
	mus-ubyte  of ." unsigned byte (8 bits)" endof
	." unknown format"
    endcase
;
: sound-data-format ( d: fname -- n )
    make-file>sample { gen }
    gen io-data-format @
    gen mus-close
;
: .header-type ( n -- )
    case
	mus-next of ." Sun" endof
	mus-riff of ." RIFF" endof
	." unknown"
    endcase
;
: sound-header-type ( d: fname -- u )
    make-file>sample { gen }
    gen io-header-type @
    gen mus-close
;
: sound-length ( d: fname -- u )
    make-file>sample { gen }
    gen io-fd @ file-size throw d>s
    gen mus-close
;
: sound-comment ( d: fname -- addr u|f )
    make-file>sample { gen }
    gen io-comment 2@ dup 0= if 2drop false then
    gen mus-close
;

: continue-sample>file { d: fname -- gen }
    sample>file% %alloc { gen }
    fname
    fname sound-chans
    fname sound-data-format
    fname sound-header-type
    0 0 false gen io-open-output
    fname sound-frames gen location !
    gen io-set-file-location
    ['] .sample>file gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" sample>file" gen gen-name 2!
    gen
;
: continue-frame>file { d: fname -- gen }
    frame>file% %alloc { gen }
    fname
    fname sound-chans
    fname sound-data-format
    fname sound-header-type
    0 0 false gen io-open-output
    fname sound-frames gen location !
    gen io-set-file-location
    ['] .frame>file gen get-inspect !
    ['] generator-free gen get-gen-free !
    s" frame>file" gen gen-name 2!
    gen
;

: sound-open-input ( d: fname -- n )
    make-file>sample { gen }
    gen io-fd @
    gen gfm-mus-free
;
: sound-open-output { d: fname srate chans fmt d: comm -- n }
    fname chans fmt *clm-header-type* comm make-sample>file { gen }
    gen io-fd @
    gen gfm-mus-free
;
: sound-close-input ( fd -- ) close-file throw ;
: sound-close-output ( fd bytes -- ) drop close-file throw ;

\ sound-data helper functions for sound/audio-read/write
: samples>sound-data { buf samps sd -- }
    sd sound-data-chans { chns }
    chns 0 do
	i shorts { idx }
	sd sound-data-length samps min 0 do
	    buf idx + s@ short>float  i j sd sound-data!
	    idx chns shorts + to idx
	loop
    loop
;
: sound-data>samples { buf samps sd -- }
    sd sound-data-chans { chns }
    chns 0 do
	i shorts { idx }
	sd sound-data-length samps min 0 do
	    i j sd sound-data@  float>dac-short buf idx + s!
	    idx chns shorts + to idx
	loop
    loop
;

: sound-read { fd beg end chns sd -- }
    end beg - 1+ { samps }
    samps chns * shorts { buflen }
    buflen allocate throw dup buflen erase { buf }
    buf buflen fd read-file throw dup >r
    buf r> sd samples>sound-data
    buf free throw
;
: sound-write { fd beg end chns sd -- }
    end beg - 1+ { samps }
    samps chns * shorts { buflen }
    buflen allocate throw { buf }
    buf samps sd sound-data>samples
    buf buflen fd write-file throw
    buf free throw
;

\ === Audio ===
\ The whole audio section of fsndlib.fs isn't very flexible and
\ reflects only my installation.

\ /usr/include/stdio.h
$0000 constant O_RDONLY
$0001 constant O_WRONLY
$0002 constant O_RDWR
$0004 constant O_NONBLOCK

\ The constants in this section are mostly take from my soundcard.h
\ and you probably have to adjust their contents.

\ /usr/local/lib/oss/include/sys/soundcard.h
$20005000 constant SNDCTL_DSP_RESET
$c0045002 constant SNDCTL_DSP_SPEED
$c0045003 constant SNDCTL_DSP_STEREO	  \ not used
$c0045005 constant SNDCTL_DSP_SETFMT
$c0045006 constant SNDCTL_DSP_CHANNELS
$c004500a constant SNDCTL_DSP_SETFRAGMENT \ not used
$00000008 constant AFMT_U8
$00000010 constant AFMT_S16_LE

\ Hardcoded dac device /dev/dsp; audio-open-output tries /dev/dsp and
\ /dev/dsp2 and gives up.  This may be changed by the user.

2variable snd-device s" /dev/dsp" snd-device 2!
AFMT_S16_LE value dac-format

require lib.fs
library libc /usr/lib/libc.so
2 (int) libc c-open  open
1 (int) libc c-close close
3 (int) libc c-read  read
3 (int) libc c-write write
3 (int) libc c-ioctl ioctl

: audio-open-input { tp srate chns frmt bytes -- fd } ." audio-open-input not written yet" ;
: audio-open-output { tp srate chns frmt bytes -- fd }
    assert1( srate 0> chns 0> and )
    snd-device 2@ c-string O_WRONLY c-open { fd }
    fd 0< if
	s" /dev/dsp2" snd-device 2!
	snd-device 2@ c-string O_WRONLY c-open to fd
	fd 0< abort" audio-open-output: can't open DAC"
    then
    0 retval !
    fd SNDCTL_DSP_RESET retval c-ioctl 0< if ." can't reset device" cr then
    retval @ if ." not reset, retval is " retval @ . cr then
    dac-format retval !
    fd SNDCTL_DSP_SETFMT retval c-ioctl 0< if ." can't set FORMAT" cr then
    dac-format retval @ <> if ." format not set, retval is " retval @ . cr then
    chns retval !
    fd SNDCTL_DSP_CHANNELS retval c-ioctl 0< if ." can't set CHANNELS" cr then
    chns retval @ <> if ." channels not set, retval is " retval @ . cr then
    srate retval !
    fd SNDCTL_DSP_SPEED retval c-ioctl 0< if ." can't set SRATE" cr then
    srate retval @ <> if ." srate not set, retval is " retval @ . cr then
    fd
;
: audio-read { line sd samps -- n }
    samps 0> if
	samps sd sound-data-chans * shorts { buflen }
	buflen allocate throw dup buflen erase { buf }
	line buf samps c-read dup >r
	buf r> sd samples>sound-data
	buf free throw
    else
	0
    then
;
: audio-write { line sd samps -- n }
    samps 0> if
	samps sd sound-data-chans * shorts { buflen }
	buflen allocate throw { buf }
	buf samps sd sound-data>samples
	line buf samps c-write
	buf free throw
    else
	0
    then
;
: audio-close ( line -- ) c-close 0< abort" audio-close: can't close audio device" ;

\ src, granulate, convolve, phase-vocoder, etc.
require gfm-gens.fs

\ fsndlib.fs ends here
