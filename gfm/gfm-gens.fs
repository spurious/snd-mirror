\ gfm-gens.fs -- generators not found in fsndlib/csndlib -*- forth -*-

\ Copyright (C) 2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Sep 27 13:39:11 CEST 2004
\ Last: Sat Oct 09 01:20:14 CEST 2004
\ Ident: $Id: gfm-gens.fs,v 1.19 2004/10/08 23:21:06 mike Exp $

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

\ Code:

require utils.fs

\ === SRC ===
music5%
    cell% field src-width
    cell% field src-rd
    cell% field src-sinc
    float% field src-x
    float% field src-incr
end-struct src%

: src-increment@ ( gen -- r ) fill-time f@ ;
: src-increment! ( f: val gen -- r ) fill-time f! ;
\ use READIN's channel and location
\ SRC has no direct object from readin
: src-channel@ ( gen -- n ) drop 0 ;
: src-channel! ( val gen -- ) 2drop ;
: src-location@ ( gen -- n ) drop 0 ;
: src-location! ( val gen -- ) 2drop ;

0 value previous-sinc-table
5 value *clm-src-width*
20 value sinc-density
-1 value previous-sinc-table-size

: fill-sinc-table { len -- w }
    previous-sinc-table-size len = if
	previous-sinc-table
    else
	pi len s>f f/ { f: win-freq }
	pi sinc-density s>f f/ { f: sinc-freq }
	sinc-freq { f: sf } win-freq { f: wf }
	1e
	len 1 do 0.5e 0.5e wf fcos f* f+ sf fsin f* sf f/
	    sf sinc-freq f+ to sf
	    wf win-freq f+ to wf
	loop
	0e len 1+ >vct { sinc-table }
	sinc-table to previous-sinc-table
	len to previous-sinc-table-size
	sinc-table
    then
;
: src-free { gen -- } gen data-buffer @ free-vct gen free throw ;

: .src { gen -- }
    ." #<src : x: " gen src-x f@ 1 f.r bs
    ." , incr: " gen fill-time f@ 1 f.r bs
    ." , width: " gen src-width @ . bs
    ." , len: " gen buffer-length . bs
    ." , sinc: " gen src-sinc f@ 1 f.r bs
    ." , data: " gen data-buffer @ .vct ." >"
;
: ?src { obj -- f } try obj gen-name 2@ s" src" str= recover drop false endtry ;
: src { input f: sr-change gen -- r }
    gen fill-time f@ sr-change f+ { f: srx } 0 { loc } 0e ( sum )
    gen src-width @ 2* { lim } gen src-x f@ floor f>s { fsx }
    input 0= if gen src-rd @ to input then
    gen data-buffer @ { dat }
    fsx 0> if
	lim fsx do i dat vct@ loc dat vct! loc 1+ to loc loop
	input if lim loc do srx f0> if 1 else -1 then gen input execute i dat vct! loop then
	gen src-x f@ fsx s>f f- gen src-x f!
    then
    srx f0< if srx fnegate to srx then
    srx 1e f<= if 1e else srx 1/f then { f: factor }
    factor sinc-density s>f f* { f: zf }
    zf 1e gen src-x f@ f- gen src-width @ s>f f- f* { f: x }
    lim 0 do
	x fabs truncate { k f: frac }
	k gen buffer-length @ < if
	    i dat vct@
	    k gen src-sinc @ vct@
	    frac k 1+ gen src-sinc @ vct@ k gen src-sinc @ vct@ f- f* f+ f* f+ ( sum += ... )
	then
	x zf f+ to x
    loop
    gen src-x f@ srx f+ gen src-x f!
    factor f* ( factor * sum )
;
: src-run ( r1 r2 gen -- r ) fdrop false swap src ;
\ false 1e *clm-src-width* make-src value gen
: make-src { input f: srate width -- gen }
    width srate f>s 2* max { wid }
    wid sinc-density * { len }
    src% %alloc { gen }
    input gen src-rd !
    0e gen src-x f!
    srate gen fill-time f!
    wid gen src-width !
    len fill-sinc-table gen src-sinc !
    len gen buffer-length !
    wid 2* 1+ make-vct gen data-buffer !
    ['] src-run gen get-mus-run !
    ['] .src gen get-inspect !
    ['] src-free gen get-gen-free !
    s" src" gen gen-name 2!
    gen
;
' make-src s\" make-src ( input-xt f: srate width -- gen )\n" >$
s\" s\"pistol.snd\" 0 0 1 make-readin value rd\n" $+
s\" lambda: drop rd readin ; 1e 10 make-src value sr false 0e sr src f.\n" $+
s\" \\ or\n" $+
s\" false 1e 10 make-src value sr\n" $+
s\" lambda: drop rd readin ; 0e sr src f." $+ help!
' ?src s" ?src ( obj -- f ) \ obj ?src ." help!
' src s\" src ( input-xt f: pm gen -- f: value )\n" >$
s\" false 0e sr src f.\n" $+
s\" lambda: drop rd readin ; 0e sr src f." $+ help!

\ === Convolve ===
music5%
    cell% field conv-filter
    cell% field conv-length
    cell% field conv-hop
    cell% field conv-rd
    cell% field conv-datar
    cell% field conv-datai
end-struct convolve%

: conv-xcoeffs@ ( gen -- w ) conv-filter @ ;

: basic-convolve { rdat idat -- }
    rdat vct-length { len }
    rdat idat len 1 fft
    len 2/ { len2 }
    0.25e len s>f f/ { f: invn }
    0 rdat vct@ 0 idat vct@ f* len s>f f/ 0 rdat vct!
    0e 0 idat vct!
    len2 1+ 1 do
	len i - { nn2 }
	i rdat vct@ nn2 rdat vct@ f+ { f: rep }
	i rdat vct@ nn2 rdat vct@ f- { f: rem }
	i idat vct@ nn2 idat vct@ f+ { f: aip }
	i idat vct@ nn2 idat vct@ f- { f: aim }
	invn rep aip f* aim rem f* f+ f* i rdat vct!
	i rdat vct@ nn2 rdat vct!
	invn aim aip f* rep rem f* f- f* i idat vct!
	i idat vct@ fnegate nn2 idat vct!
    loop
    rdat idat len -1 fft
;
*clm-use-csndlib* 0= [if] ' basic-convolve alias convolution [then]

: conv-free { gen -- }
    gen conv-datar @ free-vct
    gen conv-datai @ free-vct
    gen free throw
;

: .convolve { gen -- }
    ." #<convolve: size: " gen buffer-length @ . bs
    ." , hop: " gen conv-hop @ . bs
    ." , datar: " gen conv-datar @ .vct
    ." , datai: " gen conv-datai @ .vct
    ." , filter: " gen conv-filter @ .vct
    ." , fltlen: " gen conv-length @ . bs ." >"
;
: ?convolve { obj -- f } try obj gen-name 2@ s" convolve" str= recover drop false endtry ;
: convolve { input gen -- r }
    gen conv-hop @ { da }
    gen data-buffer @ { buf }
    gen conv-datar @ { rdat }
    gen conv-datai @ { idat }
    gen conv-filter @ { flt }
    input 0= if gen conv-rd @ to input then
    buf ?buffer-empty if
	input if da 0 do 1 gen input execute i rdat vct! loop then
	gen buffer-length @ da do 0e i rdat vct! loop
	gen conv-length @ 0 do i flt vct@ i idat vct! loop
	gen buffer-length @ gen conv-length @ do 0e i idat vct! loop
	rdat idat convolution
	da 0 do i buf data-buffer @ vct@ i rdat vct@ f+ i buf data-buffer @ vct! loop
	da 2* da do i rdat vct@ i buf data-buffer @ vct! loop
	da s>f fdup buf fill-time f! f0= buf buf-empty !
    then
    buf buffer>sample
;
: conv-run ( r1 r2 gen -- r ) fdrop fdrop false swap convolve ;
: make-convolve { input filt -- gen }
    16 { fftsize }
    filt vct-length { len }
    len 1- len and 0= if
	len 2*
    else
	2e len s>f flnp1 2e fln f/ fround 1e f+ f** f>s
    then fftsize max { fft1 }
    fft1 2/ { fft2 }
    convolve% %alloc { gen }
    filt gen conv-filter !
    len gen conv-length !
    fft1 gen buffer-length !
    fft2 gen conv-hop !
    fft1 make-vct gen conv-datar !
    fft1 make-vct gen conv-datai !
    fft1 0e make-buffer gen data-buffer !
    input gen conv-rd !
    ['] conv-xcoeffs@ gen get-xcoeffs !
    ['] conv-run gen get-mus-run !
    ['] .convolve gen get-inspect !
    ['] conv-free gen get-gen-free !
    s" convolve" gen gen-name 2!
    gen
;
*clm-use-csndlib* 0= [if]
    : convolve-one-channel { d: fil1 d: fil2 chan-f1 chan-f2 len-f1 len-f2 -- data }
	fil1 chan-f1 0 len-f1 file>array { dat1 }
	fil2 chan-f2 0 len-f2 file>array { dat2 }
	dat1 dat2 convolution
	dat2 free-vct
	dat1
    ;

    : convolve-files { d: fnam1 d: fnam2 f: maxamp d: ofile -- }
	fnam1 sound-frames { len-f1 }
	fnam2 sound-frames { len-f2 }
	fnam1 sound-chans { chns-f1 }
	fnam2 sound-chans { chns-f2 }
	chns-f1 chns-f2 max { o-chns }
	2e len-f1 len-f2 + s>f flnp1 2e fln f/ fround 1e f+ f** f>s { fftlen }
	len-f1 len-f2 + 1+ { o-len }
	o-chns 1 = if
	    fnam1 fnam2 0 0 len-f1 len-f2 convolve-one-channel { o-dat }
	    0 o-dat vct@ fabs o-len 0 do i o-dat vct@ fabs fmax loop { f: maxval }
	    maxval f0<> if
		maxamp maxval f/ { f: maxv }
		o-len 0 do i o-dat vct@ maxv f* i o-dat vct! loop
	    then
	    o-dat o-len fnam1 sound-srate 1 ofile array>file
	    o-dat free-vct
	else
	    o-len o-chns * { tot-len }
	    tot-len make-vct { o-dat }
	    0 0 { c1 c2 }
	    o-chns 0 do
		fnam1 fnam2 c1 c2 len-f1 len-f2 convolve-one-channel { cur-dat }
		0 { kk }
		tot-len i do k cur-dat vct@ j o-dat vct! kk 1+ to kk o-chns +loop
		c1 1+ to c1 c1 chns-f1 >= if 0 to c1 then
		c2 1+ to c2 c2 chns-f2 >= if 0 to c2 then
		cur-dat free-vct
	    loop
	    0 o-dat vct@ fabs tot-len 0 do i o-dat vct@ fabs fmax loop { f: maxval }
	    maxval f0<> if
		maxamp maxval f/ { f: maxv }
		tot-len 0 do i o-dat vct@ maxv f* i o-dat vct! loop
	    then
	    o-dat tot-len fnam1 sound-srate o-chns ofile array>file
	    o-dat free-vct
	then
    ;
[then]
' make-convolve s\" make-convolve ( input-xt filter-vct -- gen )\n" >$
s\" s\" pistol.snd\" 0 0 1 make-readin value rd\n" $+
s\" 0.5e 0.2e 0.1e 0.05e 0e 0e 0e 0e 8 >vct value filt\n" $+
s\" lambda: drop rd readin ; filt make-convolve value cv" $+ help!
' ?convolve s" ?convolve ( obj -- f ) \ obj ?convolve ." help!
' convolve s\" convolve ( input-xt gen -- f: value )\n" >$
s\" false cv convolve f.\n" $+
s\" lambda: drop rd readin ; cv convolve f." $+ help!
' convolution s" convolution ( rl1-vct rl2-vct -- ) \ result in rl1-vct" help!
' convolve-files s" convolve-files ( d: fname1 d: fname2 f: maxamp d: fname3 -- )" help!

\ === Granulate ===
music5%
    cell% field gran-rd
    cell% field gran-ramp
    cell% field gran-hop-in
    cell% field gran-hop-out
    cell% field gran-cur-in
    cell% field gran-cur-out
    cell% field gran-s20
    cell% field gran-s50
    cell% field gran-block-len
    cell% field gran-data-in
    cell% field gran-data-in-start
    cell% field gran-data-in-len
end-struct granulate%

: ramp@ ( gen -- n ) gran-ramp @ ;
: ramp! ( val gen -- ) gran-ramp ! ;
: gr-frequency@ ( gen -- r ) gran-hop-out @ s>f mus-srate@ f/ ;
: gr-frequency! ( f: val gen - ) seconds>samples swap gran-hop-out ! ;
: gr-scaler@ ( gen -- r ) scaler f@ ;
: gr-scaler! ( f: val gen -- n ) scaler f! ;
: gr-hop@ ( gen -- n ) gran-hop-out @ ;
: gr-hop! ( val gen -- ) gran-hop-out ! ;
: gr-increment@ ( gen -- r ) dup gran-hop-out @ s>f gran-hop-in @ s>f f/ ;
: gr-increment! ( f: val gen -- ) dup gran-hop-out @ s>f fswap f/ f>s swap gran-hop-in ! ;

: gran-free { gen -- }
    gen data-buffer @ free-vct
    gen gran-data-in @ free-vct
    gen free throw
;

: .granulate { gen -- }
    ." #<granulate: amp: " gen scaler f@ 1 f.r bs
    ." , len: " gen buffer-length @ . ." (" gen gran-block-len @ . bs ." )"
    ." , ramp: " gen ramp@ . bs
    ." , hop-in: " gen gran-hop-in @ . bs
    ." , hop-out: " gen gr-hop@ . bs
    ." , cur-in: " gen gran-cur-in @ . bs
    ." , cur-out: " gen gran-cur-out @ . bs
    ." , s20: " gen gran-s20 @ . bs
    ." , s50: " gen gran-s50 @ . bs
    ." , ctr: " gen location @ . bs
    ." , data: " gen data-buffer @ .vct
    ." , in-data: " gen gran-data-in @ .vct ." >"
;
: ?granulate { obj -- f } try obj gen-name 2@ s" granulate" str= recover drop false endtry ;
: granulate { input gen -- r }
    gen data-buffer @ { buf }
    gen location @ buf vct@ { f: val }
    1 gen location +!
    gen location @ gen gran-cur-out @ >= if
	gen buffer-length @ { dlen }
	gen gran-cur-out @ { start }
	0 dlen start - max { end }
	end 0> if end 0 do i start + buf vct@ i buf vct! loop then
	gen gran-block-len @ end do 0e i buf vct! loop
	gen gran-data-in-start @ to start
	gen gran-data-in-len @ to end
	input 0= if gen gran-rd @ to input then
	gen gran-data-in @ { in-buf }
	input if start end > if start end - 0 do 1 gen input execute loop end to start then then
	start end < if end start do i in-buf vct@ i start - in-buf vct! loop then
	input if end end start - do 1 gen input execute i in-buf vct! loop then
	gen gran-hop-in @ gen gran-data-in-start !
	gen gran-ramp @ { rmp }
	0e { f: amp }
	gen scaler f@ rmp s>f f/ { f: incr }
	dlen rmp - { steady-end }
	gen gran-s20 @ irandom { cur-start }
	dlen 0 do
	    i buf vct@ i cur-start + in-buf vct@ amp f* f+ i buf vct!
	    i rmp < if
		amp incr f+ to amp
	    else
		i steady-end > if amp incr f- to amp then
	    then
	loop
	gen gran-cur-out @ negate gen location +!
	0 gen gran-hop-out @ gen gran-s50 @ irandom gen gran-s50 @ 2/ - + max gen gran-cur-out !
    then
    val
;
: gr-run ( r1 r2 gen -- r ) fdrop fdrop false swap granulate ;
: make-granulate { input f: expan edit-fn -- gen }
    0.15e { f: len }
    0.05e { f: hop }
    1e { f: jitter }
    granulate% %alloc { gen }
    0 gen gran-cur-in !
    0 gen gran-cur-out !
    0 gen location !
    input gen gran-rd !
    len mus-srate@ f* 1e f+ { f: flen } flen f>s gen buffer-length !
    flen 0.4e f* floor f>s gen gran-ramp !
    0.6e gen scaler f!
    hop mus-srate@ f* expan f/ floor f>s gen gran-hop-in !
    hop seconds>samples gen gran-hop-out !
    jitter mus-srate@ 20e f/ f* floor f>s gen gran-s20 !
    jitter mus-srate@ 50e f/ f* floor f>s gen gran-s50 !
    hop len f+ seconds>samples { blen }
    blen 0<= if 1 to blen then
    blen gen gran-block-len !
    blen gen gran-s20 @ + 1+ { iblen }
    iblen gen gran-data-in-len !
    iblen gen gran-data-in-start !
    blen make-vct gen data-buffer !
    iblen make-vct gen gran-data-in !
    ['] gr-frequency@ gen get-freq !
    ['] gr-frequency! gen set-freq !
    ['] gr-scaler@ gen get-scaler !
    ['] gr-scaler! gen set-scaler !
    ['] gr-run gen get-mus-run !
    ['] .granulate gen get-inspect !
    ['] gran-free gen get-gen-free !
    s" granulate" gen gen-name 2!
    gen
;
' make-granulate s\" make-granulate ( input-xt f: expansion edit-xt -- gen )\n" >$
s\" lambda: drop rd readin ; 1e false make-granulate value gr\n" $+
s\" \\ default values can be changed via\n" $+
s\" 0.15e gr length!\n" $+
s\" 0.6e  gr scaler!\n" $+
s\" 0.05e gr hop!\n" $+
s\" 0.4e  gr ramp!\n\n" $+
s\" \\ return a new granular synthesis generator.  LENGTH is the grain\n" $+
s\" \\ length (seconds), EXPANSION is the ratio in timing between the new and\n" $+
s\" \\ old (expansion > 1.0 slows things down), SCALER scales the grains\n" $+
s\" \\ to avoid overflows, HOP is the spacing (seconds) between successive\n" $+
s\" \\ grains upon output, INPUT-XT's stack effect is XT ( dir -- f: value ).\n" $+
s\" \\ The current grain is accessible via DATA@/DATA>VCT." $+ help!
' ?granulate s" ?granulate ( obj -- f ) \ obj ?granulate ." help!
' granulate s\" granulate ( input-xt gen -- f: value )\n" >$
s\" false gr granulate f.\n" $+
s\" lambda: drop rd readin ; gr granulate f." $+ help!

\ === Phase-Vocoder ===
music5%
    cell% field pv-outctr
    cell% field pv-interp
    cell% field pv-filptr
    cell% field pv-N
    cell% field pv-D
    cell% field pv-in-data
    cell% field pv-win
    cell% field pv-ampinc
    cell% field pv-amps
    cell% field pv-freqs
    cell% field pv-phases
    cell% field pv-phaseinc
    cell% field pv-lastphase
    cell% field pv-input
    cell% field pv-analyze
    cell% field pv-edit
    cell% field pv-synthesize
    float% field pv-pitch
end-struct phase-vocoder%

: pv-sum-of-sines { amps phs len -- r } 0e len 0 do i amps vct@ i phs vct@ fsin f* f+ loop ;

: pv-hop@ ( gen -- n ) pv-D @ ;
: pv-hop! ( val gen -- ) pv-D ! ;
: pv-length@ ( gen -- n ) pv-N @ ;
: pv-length! ( val gen -- ) pv-N ! ;
: pv-increment@ ( gen -- r ) pv-interp @ s>f ;
: pv-increment! ( f: val gen -- ) f>s swap pv-interp ! ;
: pv-frequency@ ( gen -- r ) pv-pitch f@ ;
: pv-frequency! ( f: val gen -- ) pv-pitch f! ;
: pv-outctr@ ( gen -- n ) pv-outctr @ ;
: pv-outctr! ( val gen -- ) pv-outctr ! ;
: pv-data@ ( idx gen -- r ) pv-in-data @ vct@ ;
: pv-data! ( f: val idx gen -- ) pv-in-data @ vct! ;
: pv-amp-increments@ ( idx gen -- r ) pv-ampinc @ vct@ ;
: pv-amp-increments! ( f: val idx gen -- ) pv-ampinc @ vct! ;
: pv-amps@ ( idx gen -- r ) pv-amps @ vct@ ;
: pv-amps! ( f: val idx gen -- ) pv-amps @ vct! ;
: pv-freqs@ ( idx gen -- r ) pv-freqs @ vct@ ;
: pv-freqs! ( f: val idx gen -- ) pv-freqs @ vct! ;
: pv-phases@ ( idx gen -- r ) pv-phases @ vct@ ;
: pv-phases! ( f: val idx gen -- ) pv-phases @ vct! ;
: pv-phase-increments@ ( idx gen -- r ) pv-phaseinc @ vct@ ;
: pv-phase-increments! ( f: val idx gen -- ) pv-phaseinc @ vct! ;
: pv-lastphase@ ( idx gen -- r ) pv-lastphase @ vct@ ;
: pv-lastphase! ( f: val idx gen -- ) pv-lastphase @ vct! ;

: pv-free { gen -- }
    gen pv-ampinc @ free-vct
    gen pv-freqs @ free-vct
    gen pv-amps @ free-vct
    gen pv-phases @ free-vct
    gen pv-lastphase @ free-vct
    gen pv-phaseinc @ free-vct
    gen free throw
;

: .phase-vocoder { gen -- }
    ." #<phase-vocoder: N: " gen pv-N @ . bs
    ." , D: " gen pv-D @ . bs
    ." , interp: " gen pv-interp @ . bs
    ." , output: " gen pv-outctr @ . bs
    ." , filptr: " gen pv-filptr @ . bs
    ." , in-data: " gen pv-in-data @ .vct ." >"
;
: ?phase-vocoder { obj -- f }
    try obj gen-name 2@ s" phase-vocoder" str= recover drop false endtry
;
: phase-vocoder { input gen -- r }
    gen pv-ampinc @ { amps }
    gen pv-amps @ { ph-amps }
    gen pv-phaseinc @ { ph-phase-inc }
    gen pv-freqs @ { freqs }
    gen pv-N @ { N }
    N 2/ { N2 }
    gen pv-D @ { D }
    gen pv-outctr @ gen pv-interp @ >= if
	input 0= if gen pv-input @ to input then
	gen pv-filptr @ { filptr }
	\ analyze: if no analyze definition was given or the definition returns true
	gen pv-analyze @ if input gen gen pv-analyze perform else true then if
	    gen pv-ampinc @ to amps
	    gen pv-freqs @ to freqs
	    N 0 do 0e i freqs vct! loop
	    0 gen pv-outctr !
	    gen pv-in-data @ 0= if
		N make-vct { indat }
		indat gen pv-in-data !
		input if N 0 do 1 gen input execute i indat vct! loop then
	    else
		gen pv-in-data @ { indat }
		N D do i indat vct@ i D - indat vct! loop
		input if N N D - do 1 gen input execute i indat vct! loop then
	    then
	    gen pv-in-data @ { indat }
	    gen pv-win @ { win }
	    filptr N mod { buf }
	    N 0 do
		i win vct@ i indat vct@ f* buf amps vct!
		buf 1+ to buf
		buf N >= if 0 to buf then
	    loop
	    D gen pv-filptr +!
	    amps freqs N 1 fft
	    amps freqs rectangular>polar
	then
	\ edit: if no edit definition was given or the definition returns true
	gen pv-edit @ if gen gen pv-edit perform else true then if
	    gen pv-freqs @ to freqs
	    D s>f 1/f { f: pscl }
	    two-pi N s>f f/ { f: kscl }
	    gen pv-lastphase @ { prev-phases }
	    gen pv-pitch f@ { f: pt }
	    N2 0 do
		i freqs vct@ i prev-phases vct@ f- { f: phasediff }
		i freqs vct@ i prev-phases vct!
		begin phasediff pi f> while phasediff two-pi f- to phasediff repeat
		begin phasediff pi fnegate f< while phasediff two-pi f+ to phasediff repeat
		pt phasediff pscl f* i s>f kscl f* f+ f* i freqs vct!
	    loop
	then
	\ general
	gen pv-ampinc @ to amps
	gen pv-amps @ to ph-amps
	gen pv-phaseinc @ to ph-phase-inc
	gen pv-freqs @ to freqs
	gen pv-interp @ s>f 1/f { f: scl }
	N2 0 do
	    scl i amps vct@ i ph-amps vct@ f- f* i amps vct!
	    scl i freqs vct@ i ph-phase-inc vct@ f- f* i freqs vct!
	loop
    then
    1 gen pv-outctr +!
    \ synthesize
    gen pv-synthesize @ if
	gen gen pv-synthesize perform
    else
	gen pv-ampinc @ to amps
	gen pv-amps @ to ph-amps
	gen pv-phaseinc @ to ph-phase-inc
	gen pv-freqs @ to freqs
	gen pv-phases @ { ph-phases }
	N2 0 do
	    i amps vct@ i ph-amps vct@ f+ i ph-amps vct!
	    i freqs vct@ i ph-phase-inc vct@ f+ i ph-phase-inc vct!
	    i ph-phase-inc vct@ i ph-phases vct@ f+ i ph-phases vct!
	loop
	ph-amps ph-phases N2 pv-sum-of-sines
    then
;
: pv-run ( r1 r2 gen -- r ) fdrop fdrop false swap phase-vocoder ;
\ false 512 4 128 1e false false false make-phase-vocoder value gen
: make-phase-vocoder { input fftsize overlap interp f: pitch analyze edit synthesize -- gen }
    fftsize 1 <= if 2 to fftsize then
    overlap 0<= if 1 to overlap then
    interp 0<= if 1 to interp then
    fftsize 2/ { N2 }
    phase-vocoder% %alloc { gen }
    fftsize gen pv-N !
    interp gen pv-interp !
    interp gen pv-outctr !
    fftsize overlap / gen pv-D !
    pitch gen pv-pitch f!
    0 gen pv-filptr !
    mus-hamming-window fftsize 0e make-fft-window { win }
    2e 0.54e fftsize s>f f* f/ { f: scl }
    fftsize 0 do i win vct@ scl f* i win vct! loop
    win gen pv-win !
    fftsize make-vct gen pv-ampinc !
    fftsize make-vct gen pv-freqs !
    N2 make-vct gen pv-amps !
    N2 make-vct gen pv-phases !
    N2 make-vct gen pv-lastphase !
    N2 make-vct gen pv-phaseinc !
    false gen pv-in-data !
    input gen pv-input !
    analyze gen pv-analyze !
    edit gen pv-edit !
    synthesize gen pv-synthesize !
    ['] pv-frequency@ gen get-freq !
    ['] pv-frequency! gen set-freq !
    ['] pv-run gen get-mus-run !
    ['] .phase-vocoder gen get-inspect !
    ['] pv-free gen get-gen-free !
    s" phase-vocoder" gen gen-name 2!
    gen
;
' make-phase-vocoder s\" make-phase-vocoder ( input-xt fft-size overlap interp f: pitch analyze-xt edit-xt syn-xt -- gen )\n" >$
s\" \\ (fft-size = 512, overlap = 4, interp = 128)\n" $+
s\" \\ return a new phase-vocoder generator; INPUT-XT is the input function,\n" $+
s\" \\ ANALYZE-XT, EDIT-XT, and SYN-XT are either false or functions that\n" $+
s\" \\ replace the default innards of the generator, FFT-SIZE, OVERLAP and\n" $+
s\" \\ INTERP set the fftsize, the amount of overlap between ffts, and the\n" $+
s\" \\ time between new analysis calls.  ANALYZE-XT, if given, takes 2 args,\n" $+
s\" \\ the generator and the input function; if it returns true, the default\n" $+
s\" \\ analysis code is also called.  EDIT-XT, if given, takes 1 arg, the\n" $+
s\" \\ generator; if it returns true, the default edit code is run.\n" $+
s\" \\ SYNTHESIZE-XT is a function of 1 arg, the generator; it is called to\n" $+
s\" \\ get the current vocoder output.\n\n" $+
s\" false 512 4 128 1e false false false make-phase-vocoder value pv\n" $+
s\" s\" test.snd\" 0 0 1 make-readin value rd\n" $+
s\" lambda: drop rd readin ; 512 4 128 1e false false false make-phase-vocoder value pv" $+ help!
' ?phase-vocoder s" ?phase-vocoder ( obj -- f ) \ obj ?phase-vocoder ." help!
' phase-vocoder s\" phase-vocoder ( input-xt gen -- f: value )\n" >$
s\" false ph phase-vocoder f.\n" $+
s\" lambda: drop rd readin ; ph phase-vocoder f." $+ help!
' pv-amp-increments@ s" pv-amp-increments@ ( idx gen -- f: value )" help!
' pv-amp-increments! s" pv-amp-increments! ( f: value idx gen -- )" help!
' pv-amps@ s" pv-amps@ ( idx gen -- f: value )" help!
' pv-amps! s" pv-amps! ( f: value idx gen -- )" help!
' pv-freqs@ s" pv-freqs@ ( idx gen -- f: value )" help!
' pv-freqs! s" pv-freqs! ( f: value idx gen -- )" help!
' pv-phases@ s" pv-phases@ ( idx gen -- f: value )" help!
' pv-phases! s" pv-phases! ( f: value idx gen -- )" help!
' pv-phase-increments@ s" pv-phase-increments@ ( idx gen -- f: value )" help!
' pv-phase-increments! s" pv-phase-increments! ( f: value idx gen -- )" help!
' pv-outctr@ s" pv-outctr@ ( gen -- out-field )" help!
' pv-outctr! s" pv-outctr! ( field gen -- )" help!

\ === Fcomb ===
music5%
    cell% field fc-delay
    cell% field fc-filter
end-struct fcomb%

: fc-scaler@ ( gen -- r ) scaler f@ ;
: fc-scaler! ( f: val gen -- r ) scaler f! ;

: fcomb-free { gen -- }
    gen fc-delay @ gfm-mus-free
    gen fc-filter @ gfm-mus-free
    gen free throw
;

: .fcomb { gen -- }
    ." #<fcomb: scaler: " gen scaler f@ 3 f.r bs
    ." , a0: " gen fc-filter @ a0@ 3 f.r bs
    ." , a1: " gen fc-filter @ a1@ 3 f.r bs
    ." , line: " gen fc-delay @ data>vct { v } v .vct v free-vct ." >"
;
: ?fcomb { obj -- f } try obj gen-name 2@ s" fcomb" str= recover drop false endtry ;
: fcomb { f: input gen -- r }
    gen scaler f@
    0e gen fc-delay @ tap  gen fc-filter @  one-zero  f*
    input f+
    0e gen fc-delay @ delay
;
: fc-run ( r1 r2 gen -- r ) fdrop fcomb ;
\ 0e 1 0e 0e make-fcomb value gen
: make-fcomb { f: scl size f: a0 f: a1 -- gen }
    fcomb% %alloc { gen }
    scl gen scaler f!
    size make-delay gen fc-delay !
    a0 a1 make-one-zero gen fc-filter !
    gen fc-delay @ buffer-length @ gen buffer-length !
    ['] fc-scaler@ gen get-scaler !
    ['] fc-scaler! gen set-scaler !
    ['] fc-run gen get-mus-run !
    ['] .fcomb gen get-inspect !
    ['] fcomb-free gen get-gen-free !
    s" fcomb" gen gen-name 2!
    gen
;
' make-fcomb s\" make-fcomb ( f: scaler size f: a0 f: a1 -- gen )\n" >$
s\" 0e 1 0e 0e make-fcomb value gen" $+ help!
' ?fcomb s" ?fcomb ( obj -- f )" help!
' fcomb s" fcomb ( f: input gen -- f: value ) \ 0e gen fcomb f. cr" help!

\ === General Definitions ===
: oscil-bank { amps oscils inputs -- r }
    0e
    oscils array-length 0 do
	i amps vct@   i inputs vct@  0e  i oscils array@ oscil   f*   f+
    loop
;
' oscil-bank s\" oscil-bank ( amps-vct oscils-array inputs-vct -- f: value )\n" >$
s\" 440e 1e make-oscil value os1\n" $+
s\" 220e 1e make-oscil value os2\n" $+
s\" 660e 1e make-oscil value os3\n\n" $+
s\" os1 os2 os3 3 >array value a1\n\n" $+
s\" 0.5e 0.3e 0.2e 3 >vct value v1\n" $+
s\" 0.5e 0.3e 0.2e 3 >vct value v2\n\n" $+
s\" v1 a1 v2 oscil-bank f." $+ help!

: ssb-bank { ssbs fir-flts f: input -- f: sum }
    0e					\ sum
    ssbs array-length fir-flts array-length min 0 do
	input i fir-flts array@ fir-filter 0e i ssbs array@ ssb-am f+
    loop
;
' ssb-bank s\" ssb-bank ( ssbs fir-flts f: input -- f: sum )\n" >$
s\" \\ SSBS and FIR-FLTS are arrays of ssb- and fir-filter-generators,\n" $+
s\" \\ INPUT is fir-filter's INPUT and SUM is the sum of\n" $+
s\" \\ input fir-gen fir-filter 0e ssb-gen ssb-am sum +!" $+ help!

: .gen { gen -- }
    gen ?src
    gen ?convolve or
    gen ?granulate or
    gen ?phase-vocoder or
    gen ?buffer or
    gen ?fcomb or if
	gen dup get-inspect perform
    else
	gen ?vct if
	    gen .vct
	else
	    gen ?array if
		gen .array
	    else
		gen ?sound-data if
		    gen .sound-data
		else
		    gen .gfm-gen
		then
	    then
	then
    then
;
' .gen s\" .gen ( gen -- )\n" >$
s\" 440e 0e make-oscil .gen\n" $+
s\" \\ works with vct, array, sound-data and all generators" $+ help!

: gen= { gen1 gen2 -- f }
    gen1 ?src gen2 ?src or
    gen1 ?convolve gen2 ?convolve or
    gen1 ?granulate gen2 ?granulate or
    gen1 ?phase-vocoder gen2 ?phase-vocoder or
    gen1 ?buffer gen2 ?buffer or
    gen1 ?fcomb gen2 ?fcomb or
    or or or or or if
	true
    else
	gen1 ?vct gen2 ?vct and if
	    gen1 gen2 vct=
	else
	    gen1 ?array gen2 ?array and if
		gen1 gen2 array=
	    else
		gen1 ?sound-data gen2 ?sound-data and if
		    gen1 gen2 sound-data=
		else
		    gen1 gen2 gfm-gen=
		then
	    then
	then
    then
;
' gen= s\" gen= ( gen1 gen2 -- f )\n" >$
s\" os1 os2 gen= .\n" $+
s\" \\ works with vct, array, sound-data and all csndlib generators" $+ help!

: xcoeffs@ ( gen -- w ) dup ?convolve if dup get-xcoeffs perform else gfm-xcoeffs@ then ;
: length@ { gen -- n } 
    gen ?phase-vocoder if
	gen pv-length@
    else
	*clm-use-csndlib* if
	    gen ?convolve gen ?granulate or gen ?fcomb or if
		gen buffer-length @
	    else
		gen gfm-length@
	    then
	else
	    gen gfm-length@
	then
    then
;
' length@ s" length@ ( gen -- length ) \ os length@ ." help!

: length! { val gen -- }
    val gen ?phase-vocoder if
	gen pv-length! 
    else
	*clm-use-csndlib* if
	    gen ?granulate if
		gen buffer-length !
	    else
		gen gfm-length!
	    then
	else
	    gen gfm-length!
	then
    then
;
' length! s" length! ( length gen -- ) \ 10 os length!" help!

: frequency@ { gen -- r }
    *clm-use-csndlib* if
	gen ?granulate gen ?phase-vocoder or if gen dup get-freq perform else
	    gen gfm-frequency@
	then
    else
	gen dup get-freq perform
    then
;
' frequency@ s" frequency@ ( gen -- f: frequency ) \ os frequency@ f." help!

: frequency! { f: val gen -- }
    val *clm-use-csndlib* if
	gen ?granulate gen ?phase-vocoder or if gen dup set-freq perform else
	    gen gfm-frequency!
	then
    else
	gen dup set-freq perform
    then
;
' frequency! s" frequency! ( f: frequency gen -- ) \ os 10e frequency!" help!

: scaler@ { gen -- r }
    *clm-use-csndlib* if
	gen ?granulate gen ?fcomb or if gen dup get-scaler perform else
	    gen gfm-scaler@
	then
    else
	gen dup get-scaler perform
    then
;
' scaler@ s" scaler@ ( gen -- f: scaler ) \ os scaler@ f." help!

: scaler! { f: val gen -- }
    val *clm-use-csndlib* if
	gen ?granulate gen ?fcomb or if gen dup set-scaler perform else
	    gen gfm-scaler!
	then
    else
	gen dup set-scaler perform
    then
;
' scaler! s" scaler! ( f: scaler gen -- ) \ 10e os scaler!" help!

: increment@ { gen -- r }
    gen ?granulate if gen gr-increment@ else
	gen ?phase-vocoder if gen pv-increment@ else
	    gen ?src if gen src-increment@ else
		gen gfm-increment@
	    then
	then
    then
;
' increment@ s" increment@ ( gen -- f: inc ) \ os increment@ f." help!

: increment! { f: val gen -- }
    val gen ?granulate if gen gr-increment! else
	gen ?phase-vocoder if gen pv-increment! else
	    gen ?src if gen src-increment@ else
		gen ?buffer if gen buf-increment! else
		    gen gfm-increment!
		then
	    then
	then
    then
;
' increment! s" increment! ( f: inc gen -- ) \ 10e os increment!" help!

: hop@ { gen -- n } gen ?granulate if gen gr-hop@ else gen pv-hop@ then ;
' hop@ s" hop@ ( gen -- hop ) \ rd hop@ ." help!

: hop! { val gen -- } val gen ?granulate if gen gr-hop! else gen pv-hop! then ;
' hop! s" hop! ( hop gen -- ) \ 10 rd hop!" help!

: mus-run { f: val1 f: val2 gen -- r }
    val1 val2 *clm-use-csndlib* if
	gen ?buffer
	gen ?src or
	gen ?convolve or
	gen ?granulate or
	gen ?phase-vocoder or
	gen ?fcomb or if
	    gen dup get-mus-run perform
	else
	    gen gfm-mus-run
	then
    else
	gen gfm-mus-run
    then
;
' mus-run s\" mus-run ( f: val1 f: val2 gen -- r )\n" >$
s\" 440e 0e make-oscil value os\n" $+
s\" \\ all generators can be replaced by mus-run\n" $+
s\" 0e 0e os oscil f.\n" $+
s\" 0e 0e os mus-run f." $+ help!

: mus-free { gen -- }
    *clm-use-csndlib* if
	gen ?src
	gen ?convolve or
	gen ?granulate or
	gen ?phase-vocoder or
	gen ?buffer or
	gen ?fcomb or if
	    gen dup get-gen-free perform
	else
	    gen gfm-mus-free
	then
    else
	gen gfm-mus-free
    then
;
' mus-free s" mus-free ( gen -- ) \ os mus-free" help!

\ gfm-gens.fs ends here
