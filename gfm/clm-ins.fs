\ clm-ins.fs -- instrument definitions for GFM -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Tue Aug 12 16:25:33 CEST 2003
\ Last: Fri Oct 08 23:16:59 CEST 2004
\ Ident: $Id: clm-ins.fs,v 1.59 2004/10/08 21:17:52 mike Exp $

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

\ Instruments:
\
\ jc-reverb
\ nrev
\ fm-violin
\ cascade
\ bird
\ bigbird
\ resflt-cos
\ resflt-noise
\ fofins
\ cross-synthesis
\ touch-tone
\ fm-insect
\ fm-noise
\ pqw
\ anoi
\ pluck
\ poltergeist
\
\ Instrument tests:
\
\ fm-violin-test
\ resflt-test
\ fofins-test
\ formant-test
\ touch-tone-test
\ fm-insect-test
\ fm-noise-test
\ pqw-test
\ anoi-test
\ pluck-test
\ polter-test

\ Code:

require gfm.fs
only forth also definitions also GFMusic also CLM-Sndlib

\ clm-2/jcrev.ins
instrument: jc-reverb { f: start f: dur }
    -0.7e 0.7e 1051 make-all-pass { allpass1 }
    -0.7e 0.7e 337 make-all-pass { allpass2 }
    -0.7e 0.7e 113 make-all-pass { allpass3 }
    0.742e 4799 make-comb { comb1 }
    0.733e 4999 make-comb { comb2 }
    0.715e 5399 make-comb { comb3 }
    0.697e 5801 make-comb { comb4 }
    13 srate@ 1000 */ make-delay { outdel1 }
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    chans 2 = if 11 srate@ 1000 */ make-delay else false then { outdel2 }
    *verbose* if ." \  jc-reverb on " rev-chans . ." in and " chans . ." out channels" cr then
    0e { f: allpass-sum }
    0e { f: all-sums }
    start dur run
	0e rev-chans 0 do j i *reverb* in-any f+ loop
	0e allpass1 all-pass  0e allpass2 all-pass  0e allpass3 all-pass to allpass-sum
	allpass-sum 0e comb1 comb  allpass-sum 0e comb2 comb  f+
	allpass-sum 0e comb3 comb  f+  allpass-sum 0e comb4 comb  f+ to all-sums
	all-sums 0e outdel1 delay  i *output*  outa
	chans 2 = if all-sums 0e outdel2 delay  i  *output*  outb then
    loop
    allpass1 mus-free
    allpass2 mus-free
    allpass3 mus-free
    comb1 mus-free
    comb2 mus-free
    comb3 mus-free
    comb4 mus-free
    outdel1 mus-free
    outdel2 ?dup-if mus-free then
;instrument

\ clm-2/nrev.ins
instrument: nrev { f: start f: dur }
    1.09e { f: reverb-factor }
    0.7e { f: lp-coeff }
    mus-srate@ 25641e f/ { f: srscale }
    1433 1601 1867 2053 2251 2399 347 113 37 59 53 43 37 29 19 15 >array { dly-len }
    dly-len array-length 0 do
	i dly-len array@ s>f srscale f* floor f>s { val }
	val 2 mod 0= if val 1+ to val then
	begin val prime 0= while val 2 + to val repeat
	val i dly-len array!
    loop
    0.822e reverb-factor f* 0 dly-len array@ make-comb { comb1 }
    0.802e reverb-factor f* 1 dly-len array@ make-comb { comb2 }
    0.773e reverb-factor f* 2 dly-len array@ make-comb { comb3 }
    0.753e reverb-factor f* 3 dly-len array@ make-comb { comb4 }
    0.753e reverb-factor f* 4 dly-len array@ make-comb { comb5 }
    0.733e reverb-factor f* 5 dly-len array@ make-comb { comb6 }
    lp-coeff lp-coeff 1e f- make-one-pole { low }
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    chans 1 > { chan2 }
    chans 4 = { chan4 }
    -0.7e 0.7e 6 dly-len array@ make-all-pass { allpass1 }
    -0.7e 0.7e 7 dly-len array@ make-all-pass { allpass2 }
    -0.7e 0.7e 8 dly-len array@ make-all-pass { allpass3 }
    -0.7e 0.7e 9 dly-len array@ make-all-pass { allpass4 }
    -0.7e 0.7e 11 dly-len array@ make-all-pass { allpass5 }
    -0.7e 0.7e 12 dly-len array@ make-all-pass { allpass6 }
    -0.7e 0.7e 13 dly-len array@ make-all-pass { allpass7 }
    -0.7e 0.7e 14 dly-len array@ make-all-pass { allpass8 }
    *verbose* if ." \  nrev on " rev-chans . ." in and " chans . ." out channels" cr then
    0e { f: rev }
    0e { f: outrev }
    start dur run
	0e rev-chans 0 do j i *reverb* in-any f+ loop to rev
	rev 0e comb1 comb rev 0e comb2 comb f+ rev 0e comb3 comb f+
	rev 0e comb4 comb f+ rev 0e comb5 comb f+ rev 0e comb6 comb f+
	0e allpass1 all-pass 0e allpass2 all-pass 0e allpass3 all-pass
	low one-pole 0e allpass4 all-pass to outrev
	outrev 0e allpass5 all-pass i *output* outa
	chan2 if outrev 0e allpass6 all-pass i *output* outb then
	chan4 if
	    outrev 0e allpass7 all-pass i *output* outc
	    outrev 0e allpass8 all-pass i *output* outd
	then
    loop
    allpass1 mus-free
    allpass2 mus-free
    allpass3 mus-free
    allpass4 mus-free
    allpass5 mus-free
    allpass6 mus-free
    allpass7 mus-free
    allpass8 mus-free
    comb1 mus-free
    comb2 mus-free
    comb3 mus-free
    comb4 mus-free
    comb5 mus-free
    comb6 mus-free
;instrument

\ clm-2/fm.html
instrument: fm-violin { f: start f: dur f: freq f: amp f: fm-index ind-env }
    freq hz>radians { f: frq-scl }
    frq-scl fm-index f* { f: maxdev }
    5e freq flog f/ maxdev f* { f: index1 }
    8.5e freq flog f- 3e freq 1000e f/ f+ f/ maxdev 3e f* f* { f: index2 }
    4e freq fsqrt f/ maxdev f* { f: index3 }
    freq 0e make-oscil { carrier }
    freq 0e make-oscil { fmosc1 }
    freq 3e f* 0e make-oscil { fmosc2 }
    freq 4e f* 0e make-oscil { fmosc3 }
    ind-env amp dur make-env { ampf }
    0e 1e 25e 0.4e 75e 0.6e 100e 0e 8 >vct { index-env }
    index-env index1 dur make-env { indf1 }
    index-env index2 dur make-env { indf2 }
    index-env index3 dur make-env { indf3 }
    5e 0.0025e frq-scl f* 0e make-triangle-wave { pervib }
    16e 0.005e frq-scl f* make-rand-interp { ranvib }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	0e pervib triangle-wave ranvib 0e rand-interp f+ { f: vib }
	vib
	   vib    0e fmosc1 oscil indf1 env f* f+
	3e vib f* 0e fmosc2 oscil indf2 env f* f+
	4e vib f* 0e fmosc3 oscil indf3 env f* f+ 0e carrier oscil ampf env f*
    end-run
    carrier mus-free
    fmosc1 mus-free
    fmosc2 mus-free
    fmosc3 mus-free
    ampf mus-free
    indf1 mus-free
    indf2 mus-free
    indf3 mus-free
    pervib mus-free
    ranvib mus-free
    index-env free-vct
;instrument
event: fm-violin-test ( -- )
    0e 0e 25e 1e 75e 1e 100e 0e 8 >vct { ienv }
    0e 2e 440e 0.5e 1e ienv fm-violin
;event

\ clm-2/fm.html
instrument: cascade
    { f: start f: dur f: freq f: amp f: modrat f: modind f: casrat f: casind f: caspha }
    freq 0e make-oscil { cr }
    freq modrat f* 0e make-oscil { md }
    freq casrat f* caspha make-oscil { ca }
    modind modrat f* freq f* hz>radians { f: fm-ind0 }
    casind casrat modrat f/ f* freq f* hz>radians { f: fm-ind1 }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	0e 0e ca oscil fm-ind1 f* 0e md oscil fm-ind0 f* 0e cr oscil amp f*
    end-run
    cr mus-free
    md mus-free
    ca mus-free
;instrument

\ clm-2/bird.ins
instrument: bird { f: start f: dur f: freq f: freq-skew f: amp freq-env amp-envel }
    freq 0e make-oscil { os }
    amp-envel amp dur make-env { amp-env }
    freq-env freq-skew hz>radians dur make-env { gls-env }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument  amp-env env  gls-env env 0e os oscil  f*  end-run
    os mus-free
    amp-env mus-free
    gls-env mus-free
;instrument

\ clm-2/bigbird.ins
instrument: bigbird { f: start f: dur f: freq f: freq-skew f: amp freq-env amp-envel parts }
    freq 0e make-oscil { os }
    amp-envel amp dur make-env { amp-env }
    freq-env freq-skew hz>radians dur make-env { gls-env }
    parts normalize-partials 1 partials>polynomial { coeffs }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument amp-env env  coeffs  gls-env env 0e os oscil  polynomial  f* end-run
    os mus-free
    amp-env mus-free
    gls-env mus-free
    coeffs free-vct
;instrument

\ clm-2/resflt.ins
instrument: resflt-cos { f: start f: dur f: cosamp f: cosfreq1 f: cosfreq0 w: cosnum w: ampcosfun w: freqcosfun f: freq1 f: r1 f: g1 f: freq2 f: r2 f: g2 f: freq3 f: r3 f: g3 }
    r1 freq1 make-ppolar { f1 }
    r2 freq2 make-ppolar { f2 }
    r3 freq3 make-ppolar { f3 }
    freqcosfun cosfreq1 cosfreq0 f- hz>radians dur make-env { frqf }
    ampcosfun cosamp dur make-env { ampf }
    cosfreq0 0e cosnum make-sum-of-cosines { cn }
    0e { f: input }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	ampf env frqf env cn sum-of-cosines f* to input
	input g1 f* f1 two-pole
	input g2 f* f2 two-pole f+
	input g3 f* f3 two-pole f+
    end-run
    f1 mus-free
    f2 mus-free
    f3 mus-free
    frqf mus-free
    ampf mus-free
    cn mus-free
;instrument
instrument: resflt-noise { f: start f: dur f: ranfreq f: noiamp w: noifun f: freq1 f: r1 f: g1 f: freq2 f: r2 f: g2 f: freq3 f: r3 f: g3 }
    r1 freq1 make-ppolar { f1 }
    r2 freq2 make-ppolar { f2 }
    r3 freq3 make-ppolar { f3 }
    noifun noiamp dur make-env { ampf }
    ranfreq 0e make-rand { rn }
    0e { f: input }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	ampf env 0e rn rand f* to input
	input g1 f* f1 two-pole
	input g2 f* f2 two-pole f+
	input g3 f* f3 two-pole f+
    end-run
    f1 mus-free
    f2 mus-free
    f3 mus-free
    ampf mus-free
    rn mus-free
;instrument
event: resflt-test ( -- )
    0e 1e 0.1e 200e 230e 10
    0e 0e 50e 1e 100e 0e 6 >vct 0e 0e 100e 1e 4 >vct
    500e 0.995e 0.1e
    1000e 0.995e 0.1e
    2000e 0.995e 0.1e resflt-cos
    0.5e 1e 10000e 0.01e
    0e 0e 50e 1e 100e 0e 6 >vct
    500e 0.995e 0.1e
    1000e 0.995e 0.1e
    2000e 0.995e 0.1e resflt-noise
;event

\ clm-2/clm.html, section wave-train
instrument: fofins { f: start f: dur f: freq f: amp f: vib f: f0 f: a0 f: f1 f: a1 f: f2 f: a2 ve ae }
    ae amp dur make-env { ampf }
    6e 0e make-oscil { vibr }
    ve vib dur make-env { vibenv }
    f0 hz>radians { f: frq0 }
    f1 hz>radians { f: frq1 }
    f2 hz>radians { f: frq2 }
    srate@ 22050 = if 100 else 200 then { foflen }
    two-pi foflen s>f f/ { f: win-freq }
    foflen make-vct { foftab }
    foflen 0 do
	i s>f { f: idx }
	a0 idx frq0 f* fsin f*
	a1 idx frq1 f* fsin f* f+
	a2 idx frq2 f* fsin f* f+
	0.5e f*
	1e idx win-freq f* fcos f- f*
	i foftab vct!
    loop
    freq 0e foftab mus-interp-linear make-wave-train { wt0 }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument ampf env vibenv env 0e 0e vibr oscil f* wt0 wave-train f* end-run
    ampf mus-free
    vibr mus-free
    vibenv mus-free
    foftab free-vct
;instrument
event: fofins-test ( -- )
    0e 1e 100e 1e 4 >vct { ve }
    0e 0e 25e 1e 75e 1e 100e 0e 8 >vct { ae }
    0e 1e 270e 0.2e 0.001e 730e 0.6e 1090e 0.3e 2440e 0.1e ve ae fofins

    0e 0e 40e 0e 75e 0.2e 100e 1e 8 >vct to ve
    0e 0e 0.5e 1e 3e 0.5e 10e 0.2e 20e 0.1e 50e 0.1e 60e 0.2e 85e 1e 100e 0e 18 >vct to ae
    1.2e 4e 270e 0.2e 0.005e 730e 0.6e 1090e 0.3e 2440e 0.1e ve ae fofins

    0e 0e 0.5e 0.5e 3e .25e 6e 0.1e 10e .1e 50e .1e 60e 0.2e 85e 1e 100e 0e 18 >vct to ae
    1.2e 4e 6e 5e f/ 540e f* 0.2e 0.005e 730e 0.6e 1090e 0.3e 2440e 0.1e ve ae fofins

    0e 0e 1e 3e 3e 1e 6e 0.2e 10e 0.1e 50e 0.1e 60e 0.2e 85e 1e 100e 0e 18 >vct to ae
    1.2e 4e 135e 0.2e 0.005e 730e 0.6e 1090e 0.3e 2440e 0.1e ve ae fofins
;event

\ clm-2/clm.html, section formant
instrument: cross-synthesis { f: start f: dur d: file1 d: file2 f: amp }
    128 { fftsize } two-pi { f: r } 2 { lo } 0 { hi }
    file1 open-input { fil1 }
    file2 open-input { fil2 }
    fftsize 2/ { freq-inc }
    fftsize make-vct { fdr }
    fftsize make-vct { fdi }
    freq-inc make-vct { diffs }
    fftsize make-vct { spectr }
    0 { filptr }
    freq-inc { ctr }
    1e r fftsize s>f f/ f- { f: radius }
    srate@ fftsize / s>f { f: bin }
    fftsize make-array { fs }
    hi 0= if freq-inc to hi then
    hi lo do radius i s>f bin f* 1e make-formant i fs array! loop
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	ctr freq-inc = if
	    fftsize 0 do filptr fil2 ina i fdr vct! filptr 1+ to filptr loop
	    fdi vct-clear
	    filptr freq-inc - to filptr
	    fdr fdi fdr vct-length 1 fft
	    fdr fdi rectangular>polar
	    freq-inc 0 do i fdr vct@ i spectr vct@ f- freq-inc s>f f/ i diffs vct! loop
	    0 to ctr
	then
	ctr 1+ to ctr
	freq-inc 0 do i spectr vct@ i diffs vct@ f+ i spectr vct! loop
	i fil1 ina { f: inval }
	0e hi lo do i spectr vct@ inval i fs array@ formant f* f+ loop
	amp f*
    end-run
    fil1 close-input
    fil2 close-input
    fs array-length 0 do i fs array@ ?dup-if mus-free then loop
    fs free-array
    fdr free-vct
    fdi free-vct
    diffs free-vct
    spectr free-vct
;instrument
event: formant-test ( -- ) 0e 1e s" oboe.snd" s" fyow.snd" 0.1e cross-synthesis ;event

\ clm-2/ugex.ins
0e 697e 697e 697e 770e 770e 770e 852e 852e 852e 941e 941e 941e 13 >vct value tt1
0e 1209e 1336e 1477e 1209e 1336e 1477e 1209e 1336e 1477e 1209e 1336e 1477e 13 >vct value tt2
instrument: touch-tone { numbers }
    90e random :locsig-degree 1e :locsig-distance
    numbers array-length 0 do
	i numbers array@ ?dup-if else 11 then { idx }
	idx tt1 vct@ 0e make-oscil { frq1 }
	idx tt2 vct@ 0e make-oscil { frq2 }
	i s>f 0.3e f* 0.2e run-instrument 0.25e 0e 0e frq1 oscil 0e 0e frq2 oscil f+ f* end-run
	frq1 mus-free
	frq2 mus-free
    loop
;instrument
event: touch-tone-test ( -- ) 8 5 7 7 5 8 6 >array touch-tone ;event

\ clm-2/insect.ins
instrument: fm-insect { f: start f: dur f: freq f: amp w: amp-env f: mod-freq f: mod-skew w: mod-freq-env f: mod-index w: mod-index-env f: fm-index f: fm-ratio }
    freq 0e make-oscil { carrier }
    mod-freq 0e make-oscil { fm1-osc }
    fm-ratio freq f* 0e make-oscil { fm2-osc }
    amp-env amp dur make-env { ampf }
    mod-index-env mod-index hz>radians dur make-env { indf }
    mod-freq-env mod-skew hz>radians dur make-env { modfrqf }
    fm-index fm-ratio f* freq f* hz>radians { f: fm2-amp }
    0e 0e { f: garble-in f: garble-out }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	indf env   modfrqf env 0e fm1-osc oscil   f* to garble-in
	fm2-amp   garble-in 0e fm2-osc oscil   f* to garble-out
	ampf env   garble-out garble-in f+ 0e carrier oscil   f*
    end-run
    carrier mus-free
    fm1-osc mus-free
    fm2-osc mus-free
    ampf mus-free
    indf mus-free
    modfrqf mus-free
;instrument    
event: fm-insect-test ( -- )
    0e 0e 40e 1e 95e 1e 100e 0.5e 8 >vct { locust }
    0e 1e 25e 0.7e 75e 0.78e 100e 1e 8 >vct { bug-hi }
    0e 0e 25e 1e 75e 0.7e 100e 0e 8 >vct { amp }
    0.000e 1.699e 4142.627e 0.015e amp 60e -16.707e locust 500.866e bug-hi 0.346e 0.5e fm-insect
    0.195e 0.233e 4126.284e 0.030e amp 60e -12.142e locust 649.490e bug-hi 0.407e 0.5e fm-insect
    0.217e 2.057e 3930.258e 0.045e amp 60e  -3.011e locust 562.087e bug-hi 0.591e 0.5e fm-insect
    2.100e 1.500e  900.627e 0.060e amp 40e -16.707e locust 300.866e bug-hi 0.346e 0.5e fm-insect
    3.000e 1.500e  900.627e 0.060e amp 40e -16.707e locust 300.866e bug-hi 0.046e 0.5e fm-insect
    3.450e 1.500e  900.627e 0.090e amp 40e -16.707e locust 300.866e bug-hi 0.006e 0.5e fm-insect
    3.950e 1.500e  900.627e 0.120e amp 40e -10.707e locust 300.866e bug-hi 0.346e 0.5e fm-insect
    4.300e 1.500e  900.627e 0.090e amp 40e -20.707e locust 300.866e bug-hi 0.246e 0.5e fm-insect
;event

\ clm-2/noise.ins
: attack-point { f: dur f: att f: dec }
    100e att f0= if dec f0= if dur else dur dec f- then 4e f/ else att then dur f/ f*
;
instrument: fm-noise { f: start f: dur f: freq0 f: amp w: ampfun f: ampat f: ampdc f: freq1 w: glissfun f: freqat f: freqdc f: rfreq0 f: rfreq1 w: rfreqfun f: rfreqat f: rfreqdc f: dev0 f: dev1 w: devfun f: devat f: devdc }
    freq0 0e make-oscil { car }
    rfreq0 1e make-rand { modul }
    dev0 hz>radians { f: dev-0 }
    devfun 25e dur devat devdc attack-point 75e 100e dur devdc devat attack-point f-
    stretch-envelope dev1 dev0 f- hz>radians dur make-env { devf }
    ampfun 25e dur ampat ampdc attack-point 75e 100e dur ampdc ampat attack-point f-
    stretch-envelope amp dur make-env { ampf }
    glissfun 25e dur freqat freqdc attack-point 75e 100e dur freqdc freqat attack-point f-
    stretch-envelope freq1 freq0 f- hz>radians dur make-env { freqf }
    rfreqfun 25e dur rfreqat rfreqdc attack-point 75e 100e dur rfreqdc rfreqat attack-point f-
    stretch-envelope rfreq1 rfreq0 f- hz>radians dur make-env { rfrqf }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	ampf env freqf env dev-0 devf env f+ rfrqf env modul rand f* f+ 0e car oscil f*
    end-run
    car mus-free
    modul mus-free
    devf mus-free
    ampf mus-free
    freqf mus-free
    rfrqf mus-free
;instrument
event: fm-noise-test ( -- )
    0e 0e 100e 1e 4 >vct { up-env }
    0e 1e 100e 0e 4 >vct { down-env }
    0e 0e 25e 1e 75e 1e 100e 0e 8 >vct { amp-env }

    0e 2e 500e 0.25e amp-env 0.1e 0.1e
    1000e up-env 0.1e 0.1e
    10e 1000e up-env 0e 0e
    100e 500e up-env 0e 0e fm-noise
;event

\ clm-2/pqw.ins
instrument: pqw { f: start f: dur f: sfreq f: cfreq f: amp ampfun indexfun parts }
    parts normalize-partials { nparts }
    sfreq half-pi make-oscil { sp-cos }
    sfreq 0e make-oscil { sp-sin }
    cfreq half-pi make-oscil { c-cos }
    cfreq 0e make-oscil { c-sin }
    nparts 0 partials>polynomial { sin-coeffs }
    nparts 1 partials>polynomial { cos-coeffs }
    ampfun amp dur make-env { amp-env }
    indexfun 1e dur make-env { ind-env }
    90e random :locsig-degree 1e :locsig-distance
    0e 0e 0e 0e { f: vib f: ax f: fax f: yfax }
    cfreq sfreq f/ { f: r }
    5e 0.005e sfreq f* hz>radians 0e make-triangle-wave { tr }
    12e 0.005e sfreq f* hz>radians make-rand-interp { rn }
    start dur run-instrument
	tr 0e triangle-wave rn 0e rand-interp f+ to vib
	1e ind-env env fmin vib 0e sp-cos oscil f* to ax
	cos-coeffs ax polynomial to fax
	vib 0e sp-sin oscil sin-coeffs ax polynomial f* to yfax
	amp-env env
	vib r f* 0e c-sin oscil yfax f*
	vib r f* 0e c-cos oscil fax f* f- f*
    end-run
    sp-cos mus-free
    sp-sin mus-free
    c-cos mus-free
    c-sin mus-free
    amp-env mus-free
    ind-env mus-free
    sin-coeffs free-vct
    cos-coeffs free-vct
    nparts free-vct
;instrument
event: pqw-test ( -- )
    0e 5e 200e 1000e 0.2e
    0e 0e 25e 1e 100e 0e 6 >vct
    0e 1e 100e 0e 4 >vct
    2e 0.1e 3e 0.3e 6e 0.5e 6 >vct pqw
;event

\ clm-2/anoi.ins
instrument: anoi { d: fname f: start f: dur fftsize f: amp-scaler f: R }
    fftsize 2/ { freq-inc }
    fftsize make-vct { fdr }
    fftsize make-vct { fdi }
    freq-inc 0 do 1e loop freq-inc >vct { spectr }
    freq-inc 0 do 1e loop freq-inc >vct { scales }
    freq-inc make-vct { diffs }
    mus-blackman2-window fftsize 0e make-fft-window { win }
    0 0 0e { kk samp f: amp }
    amp-scaler 4e f* mus-srate@ f/ { f: incr }
    fname open-input { fil }
    1e R fftsize s>f f/ f- { f: radius }
    mus-srate@ fftsize s>f f/ { f: bin }
    freq-inc 0 do radius i s>f bin f* 1e make-formant loop freq-inc >array { fs }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	samp 0 fil file>sample { f: inval } samp 1+ to samp
	inval kk fdr vct! kk 1+ to kk
	amp amp-scaler f< if amp incr f+ to amp then
	kk fftsize >= if
	    0 to kk
	    fdr fdi win 1 spectrum
	    freq-inc 0 do
		0.9e i spectr vct@ f* 0.1e i fdr vct@ f* f+ i spectr vct!
		i spectr vct@ i fdr vct@ f>= if
		    i scales vct@ fftsize negate s>f f/
		else
		    i fdr vct@ i spectr vct@ f- i fdr vct@ f/ i scales vct@ f-
		    fftsize s>f f/
		then i diffs vct!
	    loop
	then
	0e { f: outval }
	freq-inc 1 do
	    i scales vct@ { f: curscl }
	    outval curscl inval i fs array@ formant f* f+ to outval
	    curscl i diffs vct@ f+ i scales vct!
	loop
	amp outval f*
    end-run
    fil close-input
    fs array-length 0 do i fs array@ ?dup-if mus-free then loop
    fs free-array
    fdr free-vct
    fdi free-vct
    spectr free-vct
    scales free-vct
    diffs free-vct
    win free-vct
;instrument
event: anoi-test ( -- ) s" fyow.snd" 0e 1e 128 1e two-pi anoi ;event

: get-optimum-c { f: s f: o f: p -- t c }
    o 1/f s o fsin f* 1e s f- s o fcos f* f+ fatan2 f* { f: pa }
    p pa f- f>s { tmp_int } tmp_int 0= if 1 to tmp_int then
    p pa f- tmp_int s>f f- { f: pc }
    begin pc 0.1e f< while tmp_int 1 - to tmp_int pc 1e f+ to pc repeat
    tmp_int
    o fsin o pc f* fsin f- o o pc f* f+ fsin f/
;
: tune-it { f: f f: s1 -- s c t }
    mus-srate@ f f/ { f: p }
    s1 f0= if 0.5e else s1 then { f: s }
    f hz>radians { f: o }
    s o p get-optimum-c { t1 f: c1 }
    1e s f- o p get-optimum-c { t2 f: c2 }
    s 0.5e f<> c1 fabs c2 fabs f< and if 1e s f- c1 t1 else s c2 t2 then
;
instrument: pluck { f: start f: dur f: freq f: amp f: weighting f: lossfact }
    freq weighting tune-it { f: wt0 f: c dlen }
    lossfact f0= if 1e else 1e lossfact fmin then { f: lf }
    wt0 f0= if 0.5e else 1e wt0 fmin then { f: wt }
    lf 1e wt f- f* lf wt f* make-one-zero { allp }
    c 1e make-one-zero { feedb }
    dlen 0 do 1e 2e mus-random f- loop dlen >vct { tab }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	i dlen mod tab vct@ { f: val }
	1e c f- val allp one-zero feedb one-zero f* i dlen mod tab vct!
	amp val f*
    end-run
    allp mus-free
    feedb mus-free
    tab free-vct
;instrument
event: pluck-test ( -- ) 0.05e 0.1e 330e 0.1e 0.95e 0.95e pluck ;event

\ snd-7/snd-test.scm (courtesy of Anders Vinjar)
instrument: poltergeist { f: start d: fname f: freq f: amp f: radius f: gain freq-env rad-env }
    fname open-input { rd }
    fname sound-duration { f: dur }
    fname sound-frames { frms }
    radius freq gain make-formant { filt }
    freq-env 1e 0e freq 1e 0 frms make-envelope { fe }
    rad-env 1e 0e radius 1e 0 frms make-envelope { re }
    90e random :locsig-degree 1e :locsig-distance
    start dur run-instrument
	rd readin amp f* filt formant ( f: result )
	re env filt formant-radius!
	fe env filt frequency!
    end-run
    rd close-input
    rd mus-free
    filt mus-free
    fe mus-free
    re mus-free
;instrument
event: polter-test ( -- )
    0e s" oboe.snd" 300e 0.5e 0e 30e
    0e 100e 1e 4000e 4 >vct 0e 0.99e 1e 0.9e 4 >vct
    poltergeist
;event

\ clm-ins.fs ends here
