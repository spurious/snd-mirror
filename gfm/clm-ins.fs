\ clm-ins.fs -- instrument definitions for GFM -*- forth -*-

\ Copyright (C) 2003--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Tue Aug 12 16:25:33 CEST 2003
\ Last: Thu Jan 13 17:04:47 CET 2005
\ Ident: $Id: clm-ins.fs,v 1.139 2005/01/13 16:06:17 mike Exp $

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

\ Instruments:                Tests:
\
\ jc-reverb     jc-reverb-args
\ freeverb      freeverb-args
\ violin                      violin-test
\ fm-violin                   fm-violin-test
\ cascade
\ bird
\ bigbird
\ cross-synthesis             formant-test
\ fm-noise                    fm-noise-test
\ poltergeist                 polter-test
\
\ clm-ins.(rb|scm)
\
\ pluck                       pluck-test
\ vox                         vox-test
\ fofins                      fofins-test
\ bes-fm                      bess-test
\ fm-trumpet                  fm-trumpet-test
\ pqw-vox                     pqw-vox-test
\ stereo-flute                flute-test
\ fm-bell                     fm-bell-test
\ fm-insect                   fm-insect-test
\ fm-drum                     fm-drum-test
\ gong                        gong-test
\ attract                     attract-test
\ pqw                         pqw-test
\ tubebell                    tubebell-test
\ wurley                      wurley-test
\ rhodey                      rhodey-test
\ hammondoid                  hammondoid-test
\ metal                       metal-test
\ drone
\ canter                      drone/canter-test
\ nrev          nrev-args
\ reson                       reson-test
\ cellon                      cellon-test
\ jl-reverb
\ gran-synth                  gran-synth-test
\ touch-tone                  touch-tone-test
\ spectra                     spectra-test
\ two-tab                     two-tab-test
\ lbj-piano                   lbj-piano-test
\ resflt                      resflt-test
\ scratch-ins                 scratch-test
\ pins                        pins-test
\ zc                          zc-test
\ zn                          zn-test
\ za                          za-test
\ exp-snd                     exp-snd-test
\ expfil                      expfil-test
\ graph-eq                    graph-eq-test
\ anoi                        anoi-test
\ fullmix                     fullmix-test

\ Code:

require spectr.fs
only forth also definitions
also Utils
also GFMusic
also CLM-Sndlib

: reverb-info { in-chans out-chans rev-name -- }
    !script-cr
    ." \  " rev-name .string ."  on " in-chans . ." in and " out-chans . ." out channels"
    script-cr
;

\ === JC-Reverb ===
$" volume"   dup constant :volume float-keyword!
$" delay1"   dup constant :delay1 float-keyword!
$" delay2"   dup constant :delay2 float-keyword!
$" delay3"   dup constant :delay3 float-keyword!
$" delay4"   dup constant :delay4 float-keyword!
$" low-pass" constant :low-pass
$" doubled"  constant :doubled

: jc-reverb-args ( keyword-args -- args )
    make-hash { args }
    1e     :volume   args set-fargs
    0.013e :delay1   args set-fargs
    0.011e :delay2   args set-fargs
    0.015e :delay3   args set-fargs
    0.017e :delay4   args set-fargs
    false  :low-pass args set-args 
    false  :doubled  args set-args 
    nil    :amp-env  args set-args 
    args
;

so-lib-exists? sndins-jc libsndins.so [if]
    \ sndind/sndins.c
    sndins-jc clm-jc-reverb sf sf sf int int sf sf sf sf ptr int ptr ptr (llong) ins_jc_reverb
    : main-jc-reverb  { f: start f: dur chans rev-chans args -- }
	start dur
	:volume   args fhash@ 
	:low-pass args hash@
	:doubled  args hash@
	:delay1   args fhash@ 
	:delay2   args fhash@ 
	:delay3   args fhash@ 
	:delay4   args fhash@ 
	:amp-env  args hash@ { amp-env }
	amp-env if amp-env data-ptr amp-env length else nil 0 then
	*output*
	*reverb* clm-jc-reverb 2drop
    ;
[else]
\ clm-3/jcrev.ins
    : main-jc-reverb { f: start f: dur chans rev-chans args -- }
	:feedback -0.7e :feedforward 0.7e :size 1051 make-all-pass { allpass1 }
	:feedback -0.7e :feedforward 0.7e :size  337 make-all-pass { allpass2 }
	:feedback -0.7e :feedforward 0.7e :size  113 make-all-pass { allpass3 }
	:scaler 0.742e :size 4799 make-comb { comb1 }
	:scaler 0.733e :size 4999 make-comb { comb2 }
	:scaler 0.715e :size 5399 make-comb { comb3 }
	:scaler 0.697e :size 5801 make-comb { comb4 }
	chans 1 > { chan2 }
	chans 4 = { chan4 }
	:size :delay1 args fhash@ seconds>samples make-delay { outdel1 }
	chan2 if
	    :size :delay2 args fhash@ seconds>samples make-delay
	else
	    nil
	then { outdel2 }
	:doubled args hash@ { doubled }
	doubled chan4 or if
	    :size :delay3 args fhash@ seconds>samples make-delay
	else
	    nil
	then { outdel3 }
	chan4 doubled chan2 and or if
	    :size :delay4 args fhash@ seconds>samples make-delay
	else
	    nil
	then { outdel4 }
	:volume args fhash@ { f: vol }
	:amp-env args hash@ if
	    :envelope :amp-env args hash@ :scaler vol :duration dur make-env 
	else
	    nil
	then { env-a }
	doubled chan4 and if $" jc-reverb is not set up for doubled reverb in quad" error then
	0e 0e 0e 0e 0e { f: allpass-sum f: all-sums f: comb-sum f: comb-sum-1 f: comb-sum-2 }
	0e 0e { f: del-a f: del-b }
	:low-pass args hash@ { low-pass }
	start dur run
	    0e rev-chans 0 do j i *reverb* in-any f+ loop ( in )
	    allpass1 all-pass-1  allpass2 all-pass-1  allpass3 all-pass-1 to allpass-sum
	    comb-sum-1 to comb-sum-2
	    comb-sum   to comb-sum-1
	    allpass-sum comb1 comb-1
	    allpass-sum comb2 comb-1 f+
	    allpass-sum comb3 comb-1 f+
	    allpass-sum comb4 comb-1 f+ to comb-sum
	    low-pass if
		comb-sum comb-sum-2 f+ 0.25e f* comb-sum-1 f2/ f+
	    else
		comb-sum
	    then to all-sums
	    all-sums outdel1 delay-1 to del-a
	    doubled if all-sums outdel3 delay-1 del-a f+ to del-a then
	    env-a if env-a env to vol then
	    del-a vol f* i *output* outa
	    chan2 if
		all-sums outdel2 delay-1 to del-b
		doubled if all-sums outdel4 delay-1 del-b f+ to del-b then
		del-b vol f* i *output* outb
	    then
	    chan4 if
		all-sums outdel3 delay-1 vol f* i *output* outc
		all-sums outdel4 delay-1 vol f* i *output* outd
	    then
	loop
	allpass1 gen-free
	allpass2 gen-free
	allpass3 gen-free
	comb1 gen-free
	comb2 gen-free
	comb3 gen-free
	comb4 gen-free
	outdel1 gen-free
	outdel2 ?delay if outdel2 gen-free then
	outdel3 ?delay if outdel3 gen-free then
	outdel4 ?delay if outdel4 gen-free then
	env-a ?env if env-a gen-free then
    ;
[then]
$" jc-reverb" constant str-jc-reverb
instrument: jc-reverb  { f: start f: dur args -- }
    args unless jc-reverb-args to args then
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    *verbose* if rev-chans chans str-jc-reverb reverb-info then
    start dur chans rev-chans args main-jc-reverb
;instrument
' jc-reverb $" jc-reverb ( f: start f: dur args -- )\n"
$" \\ JC-REVERB-ARGS (see there) takes this keywords\n" $+
$" \\ :volume   -- 1e\n" $+
$" \\ :delay1   -- 0.013e\n" $+
$" \\ :delay2   -- 0.011e\n" $+
$" \\ :delay3   -- 0.015e\n" $+
$" \\ :delay4   -- 0.017e\n" $+
$" \\ :low-pass -- false\n" $+
$" \\ :doubled  -- false\n" $+
$" \\ :amp-env  -- nil (vct)\n" $+
$" ' fm-violin-test :reverb ' jc-reverb with-sound" $+ help!
' jc-reverb-args $" jc-reverb-args ( keyword-args -- args )\n"
$" \\ keywords and default values\n" $+
$" \\ :volume   -- 1e\n" $+
$" \\ :delay1   -- 0.013e\n" $+
$" \\ :delay2   -- 0.011e\n" $+
$" \\ :delay3   -- 0.015e\n" $+
$" \\ :delay4   -- 0.017e\n" $+
$" \\ :low-pass -- false\n" $+
$" \\ :doubled  -- false\n" $+
$" \\ :amp-env  -- nil (vct)\n" $+
$" ' fm-violin-test\n" $+
$" :reverb-data :low-pass true jc-reverb-args\n" $+
$" :reverb ' jc-reverb :channels 2 with-sound" $+ help!

\ === Freeverb ===
$" room-decay"        dup constant :room-decay        float-keyword!
$" damping"           dup constant :damping           float-keyword!
$" global"            dup constant :global	      float-keyword!
$" predelay"          dup constant :predelay	      float-keyword!
$" output-gain"       dup constant :output-gain	      float-keyword!
$" scale-room-decay"  dup constant :scale-room-decay  float-keyword!
$" offset-room-decay" dup constant :offset-room-decay float-keyword!
$" scale-damping"     dup constant :scale-damping     float-keyword!
$" stereo-spread"     dup constant :stereo-spread     float-keyword!
$" output-mixer"      constant :output-mixer
$" combtuning"        constant :combtuning
$" allpasstuning"     constant :allpasstuning
$" del-comb"          constant :del-comb
$" del-allpass"       constant :del-allpass

: freeverb-args ( keyword-args -- args )
    make-hash { args }
    0.50e     :room-decay        args set-fargs
    0.50e     :damping           args set-fargs
    0.30e     :global            args set-fargs
    0.03e     :predelay          args set-fargs
    1.00e     :output-gain       args set-fargs
    0.28e     :scale-room-decay  args set-fargs
    0.70e     :offset-room-decay args set-fargs
    0.40e     :scale-damping     args set-fargs
    23.0e     :stereo-spread     args set-fargs
    nil       :output-mixer      args set-args 
    nil       :combtuning        args set-args 
    nil       :allpasstuning     args set-args 
    args
;

so-lib-exists? sndins-fv libsndins.so [if]
    \ sndins/sndins.c
    sndins-fv clm-freeverb sf sf sf sf sf sf sf sf sf sf sf ptr int ptr int ptr ptr ptr (llong) ins_freeverb
    : main-freeverb { f: start f: dur out-chans in-chans args -- }
	start dur
	:room-decay	   args fhash@
	:damping	   args fhash@
	:global		   args fhash@
	:predelay	   args fhash@
	:output-gain	   args fhash@
	:scale-room-decay  args fhash@
	:offset-room-decay args fhash@
	:scale-damping	   args fhash@
	:stereo-spread     args fhash@
	:combtuning        args hash@ { combtuning }
	combtuning ?array if
	    false
	else
	    array[ 1116 1188 1277 1356 1422 1491 1557 1617 ] to combtuning
	    true
	then { del-comb }
	combtuning data-ptr
	combtuning length
	:allpasstuning args hash@ { allpasstuning }
	allpasstuning ?array if
	    false
	else
	    array[ 556 441 341 225 ] to allpasstuning
	    true
	then { del-allpass }
	allpasstuning data-ptr
	allpasstuning length
	:output-mixer args hash@
	*output*
	*reverb* clm-freeverb 2drop
	del-comb if combtuning gen-free then
	del-allpass if allpasstuning gen-free then
    ;
[else]
    \ clm-3/freeverb/freeverb.ins
    : main-freeverb { f: start f: dur out-chans in-chans args -- }
	in-chans 1 > in-chans out-chans <> and if
	    $" freeverb: input must be mono or input channels must equal output channels" error
	then
	:output-mixer  args hash@ { out-mix }
	:combtuning    args hash@ { combtuning }
	:allpasstuning args hash@ { allpasstuning }
	combtuning ?array if
	    false
	else
	    array[ 1116 1188 1277 1356 1422 1491 1557 1617 ] to combtuning
	    true
	then { del-comb }
	allpasstuning ?array if
	    false
	else
	    array[ 556 441 341 225 ] to allpasstuning
	    true
	then { del-allpass }
	out-chans s>f { f: fchans }
	1e :global args fhash@ f- 1e fchans 1/f f- f* fchans 1/f f+ { f: local-gain }
	fchans local-gain fchans f* f- fchans fchans f* fchans f- 1e fmax f/ { f: global-gain }
	out-mix ?mixer if
	    false
	else
	    out-chans make-mixer to out-mix
	    out-chans 0 do
		out-chans 0 do
		    :output-gain args fhash@ i j = if
			local-gain
		    else
			global-gain
		    then f* fchans f/
		    j i out-mix mixer!
		loop
	    loop
	    true
	then { del-mix }
	mus-srate@ 44100e f/ { f: srate-scale }
	:room-decay args fhash@ :scale-room-decay args fhash@ f*
	:offset-room-decay args fhash@ f+ { f: room-decay-val }
	in-chans make-array map
	    :size mus-srate@ :predelay args fhash@ f* f>s make-delay
	end-map { predelays }
	nil { ary }
	out-chans make-array map
	    combtuning length make-array to ary
	    combtuning each
		s>f srate-scale f* ( size )
		:scale-damping args fhash@ :damping args fhash@ f* ( dmp )
		fswap
		j 2 mod if srate-scale :stereo-spread args fhash@ f* f+ then ( size++ )
		:scaler room-decay-val :size fswap f>s :a0 fover 1e fswap f- :a1 frot make-fcomb
		i ary array!
	    end-each
	    ary
	end-map { combs }
	out-chans make-array map
	    allpasstuning length make-array to ary
	    allpasstuning each
		s>f srate-scale f* ( size )
		j 2 mod if srate-scale :stereo-spread args fhash@ f* f+ then ( size++ )
		:size f>s :feedforward -1e :feedback 0.5e make-all-pass i ary array!
	    end-each
	    ary
	end-map { allpasses }
	in-chans make-frame { f-in }
	out-chans make-frame { f-out }
	out-chans make-frame { out-buf }
	in-chans 1 > if
	    start dur run
		f-in i *reverb* file>frame drop
		predelays each
		    i f-in frame@ delay-1 i f-in frame!
		    0e i f-out frame!
		    i combs array@ each j f-in frame@ fcomb j f-out frame+! end-each
		end-each
		allpasses each
		    each j f-out frame@ all-pass-1 j f-out frame! end-each
		end-each
		f-out out-buf out-mix frame>frame i *output* frame>file
	    loop
	else
	    start dur run
		f-in i *reverb* file>frame drop
		0 f-in frame@ 0 predelays array@ delay-1 0 f-in frame!
		combs each
		    0e i f-out frame!
		    each 0 f-in frame@ fcomb j f-out frame+! end-each
		end-each
		allpasses each
		    each j f-out frame@ all-pass-1 j f-out frame! end-each
		end-each
		f-out out-buf out-mix frame>frame i *output* frame>file
	    loop
	then
	predelays gen-free
	combs gen-free
	allpasses gen-free
	del-mix if out-mix gen-free then
	f-out gen-free
	f-in gen-free
	del-comb if combtuning gen-free then
	del-allpass if allpasstuning gen-free then
    ;
[then]
$" freeverb" constant str-freeverb
instrument: freeverb { f: start f: dur args -- }
    args unless freeverb-args to args then
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    *verbose* if rev-chans chans str-freeverb reverb-info then
    start dur chans rev-chans args main-freeverb
;instrument
' freeverb $" freeverb ( f: start f: dur args -- )\n"
$" \\ FREEVERB-ARGS (see there) takes this keywords\n" $+
$" \\ :room-decay        -- 0.5e\n" $+
$" \\ :damping           -- 0.5e\n" $+
$" \\ :global            -- 0.3e\n" $+
$" \\ :predelay          -- 0.03e\n" $+
$" \\ :output-gain       -- 1e\n" $+
$" \\ :scale-room-decay  -- 0.28e\n" $+
$" \\ :offset-room-decay -- 0.7e\n" $+
$" \\ :scale-damping     -- 0.4e\n" $+
$" \\ :stereo-spread     -- 23e\n" $+
$" \\ :output-mixer      -- nil\n" $+
$" \\ :combtuning        -- array[ 1116 1188 1277 1356 1422 1491 1557 1617 ]\n" $+
$" \\ :allpasstuning     -- array[ 556 441 341 225 ]\n" $+
$" \\ Reverb channels must be mono or must equal output channels!\n" $+
$" ' fm-violin-test :reverb ' freeverb with-sound" $+ help!
' freeverb-args $" freeverb-args ( keyword-args -- args )\n"
$" \\ keywords and default values\n" $+
$" \\ :room-decay        -- 0.5e\n" $+
$" \\ :damping           -- 0.5e\n" $+
$" \\ :global            -- 0.3e\n" $+
$" \\ :predelay          -- 0.03e\n" $+
$" \\ :output-gain       -- 1e\n" $+
$" \\ :scale-room-decay  -- 0.28e\n" $+
$" \\ :offset-room-decay -- 0.7e\n" $+
$" \\ :scale-damping     -- 0.4e\n" $+
$" \\ :stereo-spread     -- 23e\n" $+
$" \\ :output-mixer      -- nil\n" $+
$" \\ :combtuning        -- array[ 1116 1188 1277 1356 1422 1491 1557 1617 ]\n" $+
$" \\ :allpasstuning     -- array[ 556 441 341 225 ]\n" $+
$" ' fm-violin-test\n" $+
$" :reverb-data :room-decay 0.8e freeverb-args\n" $+
$" :reverb ' freeverb :channels 2 with-sound" $+ help!

\ === Instrument Keywords ===
$" fm-index"   dup constant :fm-index float-keyword!
$" index-env"  constant :index-env

\ snd-7/fm.html
instrument: violin ( start dur freq amp keyword-args -- )
    1e         :fm-index      get-fargs { f: fm-index }
    false      :amp-env       get-args  { amp-env }
    false      :index-env     get-args  { index-env }
    90e random :degree        get-fargs :locsig-degree
    1e         :distance      get-fargs :locsig-distance
    0.01e      :reverb-amount get-fargs :locsig-reverb-amount
    { f: start f: dur f: freq f: amp }
    amp-env ?envelope if
	false
    else
	vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] to amp-env true
    then { amp-del }
    index-env ?envelope if
	false
    else
	vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ] to index-env
	true
    then { index-del }
    freq hz>radians { f: frq-scl }
    frq-scl fm-index f* { f: maxdev }
    5e freq flog f/ maxdev f* { f: index1 }
    8.5e freq flog f- 3e freq 1000e f/ f+ f/ maxdev 3e f* f* { f: index2 }
    4e freq fsqrt f/ maxdev f* { f: index3 }
    :frequency freq make-oscil { carrier }
    :frequency freq make-oscil { fmosc1 }
    :frequency freq 3e f* make-oscil { fmosc2 }
    :frequency freq 4e f* make-oscil { fmosc3 }
    :envelope amp-env :scaler amp :duration dur make-env { ampf }
    :envelope index-env :scaler index1 :duration dur make-env { indf1 }
    :envelope index-env :scaler index2 :duration dur make-env { indf2 }
    :envelope index-env :scaler index3 :duration dur make-env { indf3 }
    :frequency 5e :amplitude 0.0025e frq-scl f* make-triangle-wave { pervib }
    :frequency 16e :amplitude 0.005e frq-scl f* make-rand-interp { ranvib }
    start dur run-instrument
	0e pervib triangle-wave ranvib 0e rand-interp f+ { f: vib }
	vib
	   vib    fmosc1 oscil-1 indf1 env f* f+
	3e vib f* fmosc2 oscil-1 indf2 env f* f+
	4e vib f* fmosc3 oscil-1 indf3 env f* f+ carrier oscil-1 ampf env f*
    end-run
    carrier gen-free
    fmosc1 gen-free
    fmosc2 gen-free
    fmosc3 gen-free
    ampf gen-free
    indf1 gen-free
    indf2 gen-free
    indf3 gen-free
    pervib gen-free
    ranvib gen-free
    amp-del if amp-env gen-free then
    index-del if index-env gen-free then
;instrument
' violin $" violin ( f: start f: dur f: freq f: amp keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :fm-index      -- 1e\n" $+
$" \\ :amp-env       -- vct[ 0e 0e 25e 1e 75e 1e 100e 0e ]\n" $+
$" \\ :index-env     -- vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ]\n" $+
$" \\ :degree        -- 90e random (locsig-degree)\n" $+
$" \\ :distance      -- 1e (locsig-distance)\n" $+
$" \\ :reverb-amount -- 0.01e (locsig-reverb-amount)\n" $+
$" 0e 3e 440e 0.5e :fm-index 0.5e ' violin with-sound" $+ help!

event: violin-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.5e violin
    dur wait
;event

\ === FM-Violin (clm-3/v.ins, snd-7/v.scm|rb) ===
$" periodic-vibrato-rate" dup constant :periodic-vibrato-rate float-keyword!
$" periodic-vibrato-amp"  dup constant :periodic-vibrato-amp  float-keyword!
$" random-vibrato-rate"   dup constant :random-vibrato-rate   float-keyword!
$" random-vibrato-amp"    dup constant :random-vibrato-amp    float-keyword!
$" noise-freq"            dup constant :noise-freq	      float-keyword!
$" noise-amount"          dup constant :noise-amount	      float-keyword!
$" ind-noise-amount"      dup constant :ind-noise-amount      float-keyword!
$" ind-noise-freq"        dup constant :ind-noise-freq	      float-keyword!
$" amp-noise-amount"      dup constant :amp-noise-amount      float-keyword!
$" amp-noise-freq"        dup constant :amp-noise-freq	      float-keyword!
$" gliss-env"             constant :gliss-env
$" gliss-amount"          dup constant :gliss-amount	      float-keyword!
$" fm1-env"               constant :fm1-env
$" fm2-env"               constant :fm2-env
$" fm3-env"               constant :fm3-env
$" fm1-rat"               dup constant :fm1-rat		      float-keyword!
$" fm2-rat"               dup constant :fm2-rat		      float-keyword!
$" fm3-rat"               dup constant :fm3-rat		      float-keyword!
$" fm1-index"             dup constant :fm1-index	      float-keyword!
$" fm2-index"             dup constant :fm2-index	      float-keyword!
$" fm3-index"             dup constant :fm3-index             float-keyword!
$" index-type"            constant :index-type
$" no-waveshaping"        constant :no-waveshaping

$" violin"                constant :violin
$" cello"                 constant :cello

: fm-violin-args ( keyword-args -- args )
    make-hash { args }
    1e         :fm-index              args set-fargs
    nil        :amp-env               args set-args 
    5e         :periodic-vibrato-rate args set-fargs
    0.0025e    :periodic-vibrato-amp  args set-fargs
    16e        :random-vibrato-rate   args set-fargs
    0.005e     :random-vibrato-amp    args set-fargs
    1000e      :noise-freq            args set-fargs
    0e         :noise-amount          args set-fargs
    10e        :ind-noise-freq        args set-fargs
    0e         :ind-noise-amount      args set-fargs
    20e        :amp-noise-freq        args set-fargs
    0e         :amp-noise-amount      args set-fargs
    nil        :gliss-env             args set-args 
    0e         :gliss-amount          args set-fargs
    nil        :fm1-env               args set-args 
    nil        :fm2-env               args set-args 
    nil        :fm3-env               args set-args 
    1e         :fm1-rat               args set-fargs
    3e         :fm2-rat               args set-fargs
    4e         :fm3-rat               args set-fargs
    0e         :fm1-index             args set-fargs
    0e         :fm2-index             args set-fargs
    0e         :fm3-index             args set-fargs
    1e         :base                  args set-fargs
    90e random :degree                args set-fargs
    1e         :distance              args set-fargs
    0.01e      :reverb-amount         args set-fargs
    :violin    :index-type            args set-args 
    false      :no-waveshaping        args set-args 
    args
;

so-lib-exists? sndins-v libsndins.so [if]
    sndins-v clm-fm-violin sf sf sf sf sf ptr int sf sf sf sf sf sf sf sf sf sf ptr int sf ptr int ptr int ptr int sf sf sf sf sf sf sf sf sf sf int int ptr ptr int (llong) ins_fm_violin

    struct
	cell% field dac-out-chans
	cell% field dac-audio-format
	cell% field dac-buffer-size
    end-struct dac-violin%
    
    : main-fm-violin { f: start f: dur f: freq f: amp args -- )
	nil { dac }
	*clm-dac-output* if
	    dac-violin% %alloc to dac
	    *channels*         dac dac-out-chans !
	    *audio-format*     dac dac-audio-format !
	    *rt-bufsize*       dac dac-buffer-size !
	    dac to *reverb*
	    begin frtime +start-time+ f- start f>= until
	then
	start dur freq amp
	:fm-index              args fhash@
	:amp-env               args  hash@ { amp-env }
	amp-env data-ptr
	amp-env length
	:periodic-vibrato-rate args fhash@
	:periodic-vibrato-amp  args fhash@
	:random-vibrato-rate   args fhash@
	:random-vibrato-amp    args fhash@
	:noise-freq            args fhash@
	:noise-amount          args fhash@
	:ind-noise-freq        args fhash@
	:ind-noise-amount      args fhash@
	:amp-noise-freq        args fhash@
	:amp-noise-amount      args fhash@
	:gliss-env             args  hash@ { gls-env }
	gls-env data-ptr
	gls-env length
	:gliss-amount          args fhash@
	:fm1-env               args  hash@ { fm1-env }
	fm1-env data-ptr
	fm1-env length
	:fm2-env               args  hash@ { fm2-env }   	  
	fm2-env data-ptr
	fm2-env length
	:fm3-env               args  hash@ { fm3-env }   	  
	fm3-env data-ptr
	fm3-env length
	:fm1-rat               args fhash@
	:fm2-rat               args fhash@
	:fm3-rat               args fhash@
	:fm1-index             args fhash@
	:fm2-index             args fhash@
	:fm3-index             args fhash@
	:base                  args fhash@
	:degree                args fhash@
	:distance              args fhash@
	:reverb-amount         args fhash@
	:index-type            args  hash@
	:no-waveshaping        args  hash@
	*output*
	*reverb*
	*clm-locsig-type*
	clm-fm-violin 2drop
    ;
[else]
    : main-fm-violin { f: start f: dur f: freq f: amp args -- }
	freq hz>radians { f: frq-scl }
	:fm-index args fhash@ f0<> { modulate }
	frq-scl :fm-index args fhash@ f* { f: maxdev }
	:index-type args hash@ :violin = { vln }
	:fm1-index args fhash@ f0= if
	    maxdev vln if 5e else 7.5e then freq fln f/ f*
	    pi fmin :fm-index float>float args hash!
	then
	:fm2-index args fhash@ f0= if
	    maxdev 3e f* vln if
		8.5e freq fln f- freq 0.001e f* 3e f+ f/
	    else
		15e freq fsqrt f/
	    then f* pi fmin :fm2-index float>float args hash!
	then
	:fm3-index args fhash@ f0= if
	    maxdev vln if 4e else 8e then freq fsqrt f/ f*
	    pi fmin :fm3-index float>float args hash!
	then
	:fm1-rat args fhash@ { f: fm1-rat }
	:fm2-rat args fhash@ { f: fm2-rat }
	:fm3-rat args fhash@ { f: fm3-rat }
	:noise-amount args fhash@ f0= :no-waveshaping args hash@ 0= and
	:fm1-env args hash@ :fm2-env args hash@ gen= and
	:fm1-env args hash@ :fm3-env args hash@ gen= and
	fm1-rat fm1-rat floor f= and
	fm2-rat fm2-rat floor f= and
	fm3-rat fm3-rat floor f= and { easy-case }
	easy-case modulate and if
	    fm1-rat floor :fm1-index args fhash@
	    fm2-rat fm1-rat f/ floor :fm2-index args fhash@
	    fm3-rat fm1-rat f/ floor :fm3-index args fhash@
	    6 >vct 1 partials>polynomial
	    1e
	else
	    nil
	    :fm1-index args fhash@
	then { coeffs f: norm }
	:frequency freq make-oscil { carrier }
	nil nil nil { fmosc1 fmosc2 fmosc3 }
	modulate if
	    :frequency freq fm1-rat f* make-oscil to fmosc1
	    easy-case unless
		:frequency freq fm2-rat f* make-oscil to fmosc2
		:frequency freq fm3-rat f* make-oscil to fmosc3
	    then
	then
	:envelope :amp-env args hash@ :scaler amp :duration dur :base :base args fhash@
	make-env { ampf }
	nil nil nil { indf1 indf2 indf3 }
	modulate if
	    :envelope :fm1-env args hash@ :scaler norm :duration dur make-env to indf1
	    easy-case unless
		:envelope :fm2-env args hash@ :scaler :fm2-index args fhash@ :duration dur
		make-env to indf2
		:envelope :fm3-env args hash@ :scaler :fm3-index args fhash@ :duration dur
		make-env to indf3
	    then 
	then
	:envelope :gliss-env args hash@ :scaler :gliss-amount args fhash@ frq-scl f* :duration dur
	make-env { frqf }
	:frequency :periodic-vibrato-rate args fhash@
	:amplitude :periodic-vibrato-amp args fhash@ frq-scl f* make-triangle-wave { pv }
	:frequency :random-vibrato-rate args fhash@
	:amplitude :random-vibrato-amp args fhash@  frq-scl f* make-rand-interp { rv }
	:noise-amount args fhash@ f0<> if
	    :frequency :noise-freq args fhash@
	    :amplitude :noise-amount args fhash@ pi f* make-rand
	else
	    nil
	then { fm-noi }
	:ind-noise-freq args fhash@ f0<> :ind-noise-amount args fhash@ f0<> and if
	    :frequency :ind-noise-freq args fhash@
	    :amplitude :ind-noise-amount args fhash@ make-rand-interp
	else
	    nil
	then { ind-noi }
	:amp-noise-freq args fhash@ f0<> :amp-noise-amount args fhash@ f0<> and if
	    :frequency :amp-noise-freq args fhash@
	    :amplitude :amp-noise-amount args fhash@ make-rand-interp
	else
	    nil
	then { amp-noi }
	:degree        args fhash@ :locsig-degree
	:distance      args fhash@ :locsig-distance
	:reverb-amount args fhash@ :locsig-reverb-amount
	start dur run-instrument
	    fm-noi if 0e fm-noi rand else 0e then { f: fuzz }
	    frqf env 0e pv triangle-wave rv f+ 0e rand-interp f+ { f: vib }
	    ind-noi if 0e ind-noi rand-interp 1e f+ else 1e then { f: ind-fuzz }
	    amp-noi if 0e amp-noi rand-interp 1e f+ else 1e then { f: amp-fuzz }
	    modulate if
		coeffs if
		    indf1 env coeffs vib fmosc1 oscil-1 polynomial f*
		else
		    fm1-rat vib f* fuzz f+ fmosc1 oscil-1 indf1 env f*
		    fm2-rat vib f* fuzz f+ fmosc2 oscil-1 indf2 env f* f+
		    fm3-rat vib f* fuzz f+ fmosc3 oscil-1 indf3 env f* f+
		then
	    else
		1e
	    then ind-fuzz f* vib f+ carrier oscil-1 amp-fuzz f* ampf env f*
	end-run
	carrier gen-free
	ampf gen-free
	frqf gen-free
	pv gen-free
	rv gen-free
	fmosc1 if fmosc1 gen-free then
	fmosc2 if fmosc2 gen-free then
	fmosc3 if fmosc3 gen-free then
	indf1 if indf1 gen-free then
	indf2 if indf2 gen-free then
	indf3 if indf3 gen-free then
	fm-noi  if fm-noi  gen-free then
	ind-noi if ind-noi gen-free then
	amp-noi if amp-noi gen-free then
	coeffs if coeffs gen-free then
    ;
[then]

\ snd-7/v.scm|rb
instrument: fm-violin ( start dur freq amp keyword-args -- )
    fm-violin-args { args }
    { f: start f: dur f: freq f: amp }
    :amp-env args hash@ if
	false	
    else
	:amp-env vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] args hash!
	true
    then { amp-del }
    :gliss-env args hash@ if
	false
    else
	:gliss-env vct[ 0e 0e 100e 0e ] args hash!
	true
    then { gls-del }
    :fm1-env args hash@ if
	false
    else
	:fm1-env vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ] args hash!
	true
    then { fm1-del }
    :fm2-env args hash@ if
	false
    else
	:fm2-env vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ] args hash!
	true
    then { fm2-del }
    :fm3-env args hash@ if
	false
    else
	:fm3-env vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ] args hash!
	true
    then { fm3-del }
    start dur freq amp args main-fm-violin
    amp-del if :amp-env args hash@ gen-free then
    gls-del if :gliss-env args hash@ gen-free then
    fm1-del if :fm1-env args hash@ gen-free then
    fm2-del if :fm2-env args hash@ gen-free then
    fm3-del if :fm3-env args hash@ gen-free then
;instrument
' fm-violin $" fm-violin ( f: start f: dur f: freq f: amp keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :fm-index              -- 1e\n" $+
$" \\ :amp-env               -- vct[ 0e 0e 25e 1e 75e 1e 100e 0e ]\n" $+
$" \\ :periodic-vibrato-rate -- 5e\n" $+
$" \\ :periodic-vibrato-amp  -- 0.0025e\n" $+
$" \\ :random-vibrato-rate   -- 16e\n" $+
$" \\ :random-vibrato-amp    -- 0.005e\n" $+
$" \\ :noise-freq            -- 1000e\n" $+
$" \\ :noise-amount          -- 0e\n" $+
$" \\ :ind-noise-freq        -- 10e\n" $+
$" \\ :ind-noise-amount      -- 0e\n" $+
$" \\ :amp-noise-freq        -- 20e\n" $+
$" \\ :amp-noise-amount      -- 0e\n" $+
$" \\ :gliss-env             -- vct[ 0e 0e 100e 0e ]\n" $+
$" \\ :gliss-amount          -- 0e\n" $+
$" \\ :fm1-env               -- vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ]\n" $+
$" \\ :fm2-env               -- vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ]\n" $+
$" \\ :fm3-env               -- vct[ 0e 1e 25e 0.4e 75e 0.6e 100e 0e ]\n" $+
$" \\ :fm1-rat               -- 1e\n" $+
$" \\ :fm2-rat               -- 3e\n" $+
$" \\ :fm3-rat               -- 4e\n" $+
$" \\ :fm1-index             -- 0e\n" $+
$" \\ :fm2-index             -- 0e\n" $+
$" \\ :fm3-index             -- 0e\n" $+
$" \\ :base                  -- 1e\n" $+
$" \\ :degree                -- 90e random\n" $+
$" \\ :distance              -- 1e\n" $+
$" \\ :reverb-amount         -- 0.01e\n" $+
$" \\ :index-type            -- :violin (or :cello)\n" $+
$" \\ :no-waveshaping        -- false\n" $+
$" 0e 3e 440e 0.5e :fm-index 0.5e ' fm-violin with-sound" $+ help!

event: fm-violin-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.5e fm-violin
    dur wait
;event

\ snd-7/fm.html
instrument: cascade
    { f: start f: dur f: freq f: amp f: modrat f: modind f: casrat f: casind f: caspha -- }
    :frequency freq make-oscil { cr }
    :frequency freq modrat f* make-oscil { md }
    :frequency freq casrat f* :initial-phase caspha make-oscil { ca }
    modind modrat f* freq f* hz>radians { f: fm-ind0 }
    casind casrat modrat f/ f* freq f* hz>radians { f: fm-ind1 }
    90e random :locsig-degree
    start dur run-instrument
	ca oscil-0 fm-ind1 f* md oscil-1 fm-ind0 f* cr oscil-1 amp f*
    end-run
    cr gen-free
    md gen-free
    ca gen-free
;instrument
' cascade
$" cascade ( f: start f: dur f: freq f: amp\n"
$"           f: modrat f: modind\n" $+
$"           f: casrat f: casind f: caspha -- )\n" $+
$" \\ see snd-7/fm.html" $+ help!

\ clm-3/bird.ins
instrument: bird { f: start f: dur f: freq f: freq-skew f: amp freqenv ampenv -- }
    :frequency freq make-oscil { os }
    :envelope ampenv :scaler amp :duration dur make-env { ampf }
    :envelope freqenv :scaler freq-skew hz>radians :duration dur make-env { gls-env }
    90e random :locsig-degree
    start dur run-instrument  ampf env  gls-env env os oscil-1  f*  end-run
    os gen-free
    ampf gen-free
    gls-env gen-free
;instrument
' bird $" bird ( f: start f: dur f: freq f: freq-skew f: amp freqenv ampenv -- )\n"
$" \\ see clm-3/bird.ins and bird.gfm" $+ help!

\ clm-3/bigbird.ins
instrument: bigbird { f: start f: dur f: freq f: freq-skew f: amp freqenv ampenv parts -- }
    :frequency freq make-oscil { os }
    :envelope ampenv :scaler amp :duration dur make-env { ampf }
    :envelope freqenv :scaler freq-skew hz>radians :duration dur make-env { gls-env }
    parts normalize-partials 1 partials>polynomial { coeffs }
    90e random :locsig-degree
    start dur run-instrument ampf env  coeffs  gls-env env os oscil-1  polynomial  f* end-run
    os gen-free
    ampf gen-free
    gls-env gen-free
    coeffs gen-free
;instrument
' bigbird $" bigbird ( f: start f: dur f: freq f: freq-skew f: amp freqenv ampenv parts -- )\n"
$" \\ see clm-3/bigbird.ins and bird.gfm" $+ help!

\ snd-7/clm.html, section formant
instrument: cross-synthesis { f: start f: dur file1 file2 f: amp -- }
    128 128e two-pi { fftsize f: f-fftsize f: r }
    file1 open-input { fil1 }
    file2 open-input { fil2 }
    fftsize 2/ { freq-inc }
    f-fftsize f2/ { f: f-freq-inc }
    fftsize make-vct { fdr }
    fftsize make-vct { fdi }
    freq-inc make-vct { spectr }
    0 { filptr }
    1e r f-fftsize f/ f- { f: radius }
    mus-srate@ f-fftsize f/ { f: bin }
    freq-inc make-array map :radius radius :frequency i s>f bin f* make-formant end-map { fs }
    90e random :locsig-degree
    start seconds>samples { beg }
    start dur run-instrument
	i beg - freq-inc mod unless
	    fdr map j beg - i filptr + + fil2 ina end-map drop
	    fdr length filptr + to filptr
	    fdi vct-clear
	    fdr fdi false 2 spectrum
	    fdr spectr vct-subtract!
	    f-freq-inc 1/f fdr vct-scale!
	then
	spectr fdr vct-add!
	i beg - fil1 ina { f: inval }
	spectr fs inval formant-bank amp f*
    end-run
    fil1 close-input
    fil2 close-input
    fs gen-free
    fdr gen-free
    fdi gen-free
    spectr gen-free
;instrument
' cross-synthesis $" cross-synthesis ( f: start f: dur file1 file2 f: amp -- )\n"
$" \\ see snd-7/clm.html" $+ help!

event: formant-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur $" fyow.snd" $" oboe.snd" 0.5e cross-synthesis
    dur wait
;event

\ clm-3/noise.ins
: attack-point { f: dur f: att f: dec -- r }
    100e att f0= if dec f0= if dur else dur dec f- then 4e f/ else att then dur f/ f*
;
instrument: fm-noise { f: start f: dur f: freq0 f: amp w: ampfun f: ampat f: ampdc f: freq1 w: glissfun f: freqat f: freqdc f: rfreq0 f: rfreq1 w: rfreqfun f: rfreqat f: rfreqdc f: dev0 f: dev1 w: devfun f: devat f: devdc -- }
    :frequency freq0 make-oscil { car }
    :frequency rfreq0 make-rand { modul }
    dev0 hz>radians { f: dev-0 }
    :envelope
    devfun 25e dur devat devdc attack-point 75e 100e dur devdc devat attack-point f-
    stretch-envelope
    :scaler dev1 dev0 f- hz>radians :duration dur make-env { devf }
    :envelope
    ampfun 25e dur ampat ampdc attack-point 75e 100e dur ampdc ampat attack-point f-
    stretch-envelope
    :scaler amp :duration dur make-env { ampf }
    :envelope
    glissfun 25e dur freqat freqdc attack-point 75e 100e dur freqdc freqat attack-point f-
    stretch-envelope
    :scaler freq1 freq0 f- hz>radians :duration dur make-env { freqf }
    :envelope
    rfreqfun 25e dur rfreqat rfreqdc attack-point 75e 100e dur rfreqdc rfreqat attack-point f-
    stretch-envelope
    :scaler rfreq1 rfreq0 f- hz>radians :duration dur make-env { rfrqf }
    90e random :locsig-degree
    start dur run-instrument
	ampf env freqf env dev-0 devf env f+ rfrqf env modul rand f* f+ car oscil-1 f*
    end-run
    car gen-free
    modul gen-free
    devf gen-free
    ampf gen-free
    freqf gen-free
    rfrqf gen-free
;instrument
' fm-noise
$" fm-noise ( f: start f: dur f: freq0 f: amp w: ampfun f: ampat f: ampdc\n"
$"            f: freq1 w: glissfun f: freqat f: freqdc\n" $+
$"            f: rfreq0 f: rfreq1 w: rfreqfun f: rfreqat f: rfreqdc\n" $+
$"            f: dev0 f: dev1 w: devfun f: devat f: devdc )\n" $+
$" see clm-3/noise.ins and fm-noise-test in clm-ins.fs" $+ help!

event: fm-noise-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 100e 1e ] { up-env }
    vct[ 0e 1e 100e 0e ] { down-env }
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] { amp-env }

    start dur 500e 0.25e amp-env 0.1e 0.1e
    1000e up-env 0.1e 0.1e
    10e 1000e up-env 0e 0e
    100e 500e up-env 0e 0e fm-noise
    up-env gen-free
    down-env gen-free
    amp-env gen-free
    dur wait
;event

\ snd-7/snd-test.scm (courtesy of Anders Vinjar)
instrument: poltergeist { f: start fname f: freq f: amp f: radius f: gain freq-env rad-env -- }
    fname open-input { rd }
    fname sound-duration { f: dur }
    fname sound-frames { frms }
    :radius radius :frequency freq :gain gain make-formant { filt }
    :envelope freq-env :offset freq :end frms make-env { fe }
    :envelope rad-env :offset radius :end frms make-env { re }
    90e random :locsig-degree
    start dur run-instrument
	rd readin amp f* filt formant ( f: result )
	re env filt formant-radius!
	fe env filt frequency!
    end-run
    rd close-input
    rd gen-free
    filt gen-free
    fe gen-free
    re gen-free
;instrument
' poltergeist
$" poltergeist ( f: start fname f: freq f: amp f: radius f: gain freq-env rad-env )\n"
$" see snd-7/snd-test.scm (courtesy of Anders Vinjar)" $+ help!

event: polter-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }

    start $" oboe.snd" 300e 0.5e 0e 30e vct[ 0e 100e 1e 4000e ] vct[ 0e 0.99e 1e 0.9e ] poltergeist
    $" oboe.snd" sound-duration wait
;event

\ clm-ins.(rb|scm)
\ with original comments from clm-ins.scm

: get-optimum-c { f: s f: o f: p -- t c }
    o 1/f s o fsin f* 1e s f- s o fcos f* f+ fatan2 f* { f: pa }
    p pa f- f>s { tmp_int } tmp_int unless 1 to tmp_int then
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
\ PLUCK
\
\ The Karplus-Strong algorithm as extended by David Jaffe and Julius
\ Smith -- see Jaffe and Smith, "Extensions of the Karplus-Strong
\ Plucked-String Algorithm" CMJ vol 7 no 2 Summer 1983, reprinted in
\ "The Music Machine".  translated from CLM's pluck.ins
instrument: pluck { f: start f: dur f: freq f: amp f: weighting f: lossfact -- }
    freq weighting tune-it { f: wt0 f: c dlen }
    lossfact f0= if 1e else 1e lossfact fmin then { f: lf }
    wt0 f0= if 0.5e else 1e wt0 fmin then { f: wt }
    lf 1e wt f- f* lf wt f* make-one-zero { allp }
    c 1e make-one-zero { feedb }
    dlen make-vct map 1e 2e mus-random f- end-map { tab }
    90e random :locsig-degree
    start dur run-instrument
	tab cycle { f: val }
	1e c f- val allp one-zero feedb one-zero f* i dlen mod tab vct!
	amp val f*
    end-run
    allp gen-free
    feedb gen-free
    tab gen-free
;instrument
' pluck $" pluck ( f: start f: dur f: freq f: amp f: weighting f: lossfact -- )" help!

event: pluck-test ( keyword-args -- )
    0e   :beg get-fargs { f: start }
    0.1e :dur get-fargs { f: dur }
    start 0.1e 330e 0.1e 0.95e 0.95e pluck
    dur 0.1e f+ wait
;event

$" I00"  constant :I:   $" E12"   constant :E:   $" AE24" constant :AE:
$" UH01" constant :UH:  $" A13"   constant :A:   $" OW25" constant :OW:
$" U02"  constant :U:   $" OO14"  constant :OO:  $" ER26" constant :ER:
$" W03"  constant :W:   $" LL15"  constant :LL:  $" R27"  constant :R:
$" Y04"  constant :Y:   $" EE16"  constant :EE:  $" LH28" constant :LH:
$" L05"  constant :L:   $" I217"  constant :I2:  $" B29"  constant :B:
$" D06"  constant :D:   $" G18"   constant :G:   $" M30"  constant :M:
$" N07"  constant :N:   $" NG19"  constant :NG:  $" P31"  constant :P:
$" T08"  constant :T:   $" K20"   constant :K:   $" F32"  constant :F:
$" TH09" constant :TH:  $" S21"   constant :S:   $" SH33" constant :SH:
$" V10"  constant :V:   $" THE22" constant :THE: $" Z34"  constant :Z:
$" ZH11" constant :ZH:  $" ZZ23"  constant :ZZ:  $" VV35" constant :VV:

make-hash value clm-ins-formants

\ formant center frequencies for a male speaker (vox and pqw-vox)
:I:   vct[ 390e 1990e 2550e ] clm-ins-formants hash!
:UH:  vct[ 520e 1190e 2390e ] clm-ins-formants hash!
:U:   vct[ 440e 1020e 2240e ] clm-ins-formants hash!
:W:   vct[ 300e  610e 2200e ] clm-ins-formants hash!
:Y:   vct[ 300e 2200e 3065e ] clm-ins-formants hash!
:L:   vct[ 300e 1300e 3000e ] clm-ins-formants hash!
:D:   vct[ 300e 1700e 2600e ] clm-ins-formants hash!
:N:   vct[ 280e 1700e 2600e ] clm-ins-formants hash!
:T:   vct[ 200e 1700e 2600e ] clm-ins-formants hash!
:TH:  vct[ 200e 1400e 2200e ] clm-ins-formants hash!
:V:   vct[ 175e 1100e 2400e ] clm-ins-formants hash!
:ZH:  vct[ 175e 1800e 2000e ] clm-ins-formants hash!
:E:   vct[ 530e 1840e 2480e ] clm-ins-formants hash!
:A:   vct[ 730e 1090e 2440e ] clm-ins-formants hash!
:OO:  vct[ 300e  870e 2240e ] clm-ins-formants hash!
:LL:  vct[ 380e  880e 2575e ] clm-ins-formants hash!
:EE:  vct[ 260e 3500e 3800e ] clm-ins-formants hash!
:I2:  vct[ 350e 2300e 3340e ] clm-ins-formants hash!
:G:   vct[ 250e 1350e 2000e ] clm-ins-formants hash!
:NG:  vct[ 280e 2300e 2750e ] clm-ins-formants hash!
:K:   vct[ 350e 1350e 2000e ] clm-ins-formants hash!
:S:   vct[ 200e 1300e 2500e ] clm-ins-formants hash!
:THE: vct[ 200e 1600e 2200e ] clm-ins-formants hash!
:ZZ:  vct[ 900e 2400e 3800e ] clm-ins-formants hash!
:AE:  vct[ 660e 1720e 2410e ] clm-ins-formants hash!
:OW:  vct[ 570e  840e 2410e ] clm-ins-formants hash!
:ER:  vct[ 490e 1350e 1690e ] clm-ins-formants hash!
:R:   vct[ 420e 1300e 1600e ] clm-ins-formants hash!
:LH:  vct[ 280e 1450e 1600e ] clm-ins-formants hash!
:B:   vct[ 200e  800e 1750e ] clm-ins-formants hash!
:M:   vct[ 280e  900e 2200e ] clm-ins-formants hash!
:P:   vct[ 300e  800e 1750e ] clm-ins-formants hash!
:F:   vct[ 175e  900e 4400e ] clm-ins-formants hash!
:SH:  vct[ 200e 1800e 2000e ] clm-ins-formants hash!
:Z:   vct[ 200e 1300e 2500e ] clm-ins-formants hash!
:VV:  vct[ 565e 1045e 2400e ] clm-ins-formants hash!

\ MLBVOI
\ 
\ translation from MUS10 of Marc LeBrun's waveshaping voice instrument
\ (using FM here) this version translated (and simplified slightly)
\ from CLM's mlbvoi.ins
instrument: vox
    { f: start f: dur f: freq f: amp ampfun freqfun f: freqscl voxfun f: index f: vibscl -- }
    voxfun length { size }
    size make-vct { f1 }
    size make-vct { f2 }
    size make-vct { f3 }
    size 1- 0 do
	i    voxfun array@ s>f
	i 1+ voxfun array@ clm-ins-formants hash@ { phon }
	fdup        i    f1 vct!
	0 phon vct@ i 1+ f1 vct!
	fdup        i    f2 vct!
	1 phon vct@ i 1+ f2 vct!
	            i    f3 vct!
	2 phon vct@ i 1+ f3 vct!
    2 +loop
    :frequency 0e make-oscil { car-os }
    6 make-array map :frequency 0e make-oscil end-map { ofs }
    :envelope ampfun :scaler amp :duration dur make-env { ampf }
    :envelope f1 :duration dur make-env { frmf1 }
    :envelope f2 :duration dur make-env { frmf2 }
    :envelope f3 :duration dur make-env { frmf3 }
    :envelope freqfun :duration dur :scaler freqscl freq f* :offset freq make-env { freqf }
    :frequency 6e :amplitude freq vibscl f* make-triangle-wave { per-vib }
    :frequency 20e :amplitude freq 0.01e f* make-rand-interp { ran-vib }
    6 make-vct { freqs }
    6 make-vct { amps }
    90e random :locsig-degree
    start dur run-instrument
	freqf env 0e per-vib triangle-wave f+ 0e ran-vib rand-interp f+ { f: frq }
 	frmf1 env    { f: frm }
	frm frq f/   { f: frm0 }
	frm0 floor fdup f>s { f: frm-fint frm-int }
	frm-int 2 mod unless
	    frm-fint frq f* hz>radians       0 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 1 freqs vct!
	    frm0 frm-fint f-                 1 amps  vct!
	    1e 1 amps vct@ f-                0 amps  vct!
	else
	    frm-fint frq f* hz>radians       1 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 0 freqs vct!
	    frm0 frm-fint f-                 0 amps  vct!
	    1e 0 amps vct@ f-                1 amps  vct!
	then
	frmf2 env    to frm
	frm frq f/   to frm0
	frm0 floor   to frm-fint
	frm-fint f>s to frm-int
	frm-int 2 mod unless
	    frm-fint frq f* hz>radians       2 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 3 freqs vct!
	    frm0 frm-fint f-                 3 amps  vct!
	    1e 3 amps vct@ f-                2 amps  vct!
	else
	    frm-fint frq f* hz>radians       3 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 2 freqs vct!
	    frm0 frm-fint f-                 2 amps  vct!
	    1e 2 amps vct@ f-                3 amps  vct!
	then
	frmf3 env    to frm
	frm frq f/   to frm0
	frm0 floor   to frm-fint
	frm-fint f>s to frm-int
	frm-int 2 mod unless
	    frm-fint frq f* hz>radians       4 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 5 freqs vct!
	    frm0 frm-fint f-                 5 amps  vct!
	    1e 5 amps vct@ f-                4 amps  vct!
	else
	    frm-fint frq f* hz>radians       5 freqs vct!
	    frm-fint 1e f+ frq f* hz>radians 4 freqs vct!
	    frm0 frm-fint f-                 4 amps  vct!
	    1e 4 amps vct@ f-                5 amps  vct!
	then
	frq hz>radians car-os oscil-1 index f* { f: car }
	car 0.2e f* 0 freqs vct@ f+ 0 ofs array@ oscil-1 0 amps vct@ f*
	car 0.2e f* 1 freqs vct@ f+ 1 ofs array@ oscil-1 1 amps vct@ f* f+ 0.80e f*
	car 0.5e f* 2 freqs vct@ f+ 2 ofs array@ oscil-1 2 amps vct@ f*
	car 0.5e f* 3 freqs vct@ f+ 3 ofs array@ oscil-1 3 amps vct@ f* f+ 0.15e f* f+
	car         4 freqs vct@ f+ 4 ofs array@ oscil-1 4 amps vct@ f*
	car         5 freqs vct@ f+ 5 ofs array@ oscil-1 5 amps vct@ f* f+ 0.05e f* f+ ampf env f*
    end-run
    f1 gen-free
    f2 gen-free
    f3 gen-free
    freqs gen-free
    amps gen-free
    car-os gen-free
    ofs gen-free
    ampf gen-free
    frmf1 gen-free
    frmf2 gen-free
    frmf3 gen-free
    freqf gen-free
    per-vib gen-free
    ran-vib gen-free
;instrument
' vox $" vox ( f: start f: dur f: freq f: amp ampfun freqfun f: freqscl voxfun f: index f: vibscl -- )" help!

event: vox-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] { amp-env }
    vct[ 0e 0e 5e 0.5e 10e 0e 100e 1e ] { frq-env }
    array[ 0 :E: 25 :AE: 35 :ER: 65 :ER: 75 :I: 100 :UH: ] { examp1 }
    array[ 0 :I: 5 :OW: 10 :I: 50 :AE: 100 :OO: ] { examp2 }

    start dur 170e 0.4e amp-env frq-env 0.1e examp1 0.05e 0.1e vox
    start dur 0.2e f+ f+ to start
    start dur 300e 0.4e amp-env frq-env 0.1e examp2 0.02e 0.1e vox
    start dur 0.2e f+ f+ to start
    start 5e 600e 0.4e amp-env frq-env 0.1e examp2 0.01e 0.1e vox

    amp-env gen-free
    frq-env gen-free
    examp1 free-array		  \ NOT GEN-FREE because of preventing freeing hash entries
    examp2 free-array
    dur f2* 0.4e f+ 5e f+ wait
;event

$" vibrato" dup constant :vibrato float-keyword!
$" f0"      dup constant :f0	  float-keyword!
$" f1"      dup constant :f1	  float-keyword!
$" f2"      dup constant :f2	  float-keyword!
$" a2"      dup constant :a2      float-keyword!
$" vib-env" constant :vib-env

\ FOF example
\
\ snd-7/clm.html, section wave-train
instrument: fofins ( start dur keyword-args -- )
    270e       :frequency get-fargs { f: freq }
    0.2e       :amplitude get-fargs { f: amp }
    0.001e     :vibrato   get-fargs { f: vib }
    730e       :f0        get-fargs { f: f0 }
    0.6e       :a0        get-fargs { f: a0 }
    1090e      :f1        get-fargs { f: f1 }
    0.3e       :a1        get-fargs { f: a1 }
    2440e      :f2        get-fargs { f: f2 }
    0.1e       :a2        get-fargs { f: a2 }
    false      :vib-env   get-args  { ve }
    false      :amp-env   get-args  { ae }
    90e random :degree    get-fargs { f: loc-degr }
    1e         :distance  get-fargs { f: loc-dist }
    { f: start f: dur }
    ve ?envelope if false else vct[ 0e 1e 100e 1e ]               to ve true then { vib-del }
    ae ?envelope if false else vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] to ae true then { amp-del }
    :envelope ae :scaler amp :duration dur make-env { ampf }
    :frequency 6e make-oscil { vibr }
    :envelope ve :scaler vib :duration dur make-env { vibenv }
    f0 hz>radians { f: frq0 }
    f1 hz>radians { f: frq1 }
    f2 hz>radians { f: frq2 }
    srate@ 22050 = if 100 else 200 then { foflen }
    two-pi foflen s>f f/ { f: win-freq }
    foflen make-vct map
	i s>f { f: idx }
	a0 idx frq0 f* fsin f*
	a1 idx frq1 f* fsin f* f+
	a2 idx frq2 f* fsin f* f+ f2/
	1e idx win-freq f* fcos f- f*
    end-map { foftab }
    :frequency freq :wave foftab make-wave-train { wt0 }
    loc-degr :locsig-degree
    loc-dist :locsig-distance
    start dur run-instrument ampf env vibenv env vibr oscil-0 f* wt0 wave-train f* end-run
    wt0 gen-free
    ampf gen-free
    vibr gen-free
    vibenv gen-free
    foftab gen-free
    vib-del if ve gen-free then
    amp-del if ae gen-free then
;instrument
' fofins $" fofins ( f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :freq     -- 270e\n" $+
$" \\ :amp      -- 0.2e\n" $+
$" \\ :vibrato  -- 0.001e\n" $+
$" \\ :f0       -- 730e\n" $+
$" \\ :a0       -- 0.6e\n" $+
$" \\ :f1       -- 1090e\n" $+
$" \\ :a1       -- 0.3e\n" $+
$" \\ :f2       -- 2440e\n" $+
$" \\ :a2       -- 0.1e\n" $+
$" \\ :vib-env  -- vct[ 0e 1e 100e 1e ]\n" $+
$" \\ :amp-env  -- vct[ 0e 0e 25e 1e 75e 1e 100e 0e ]\n" $+
$" \\ :degree   -- 90e random (locsig-degree)\n" $+
$" \\ :distance -- 1e (locsig-distance)\n" $+
$" ' fofins with-sound\n\n" $+
$" vct[ 0e 0e 40e 0e 75e 0.2e 100e 1e ] value ve\n" $+
$" vct[ 0e 0e 0.5e 1e 3e 0.5e 10e 0.2e 20e 0.1e 50e 0.1e 60e 0.2e 85e 1e 100e 0e ] value ae\n" $+
$" 1.2e :beg 4e :dur 0.005e :vibrato ve :vib-env ae :amp-env ' fofins with-sound" $+ help!

event: fofins-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }

    start dur fofins

    vct[ 0e 0e 40e 0e 75e 0.2e 100e 1e ] { ve }
    vct[ 0e 0e 0.5e 1e 3e 0.5e 10e 0.2e 20e 0.1e 50e 0.1e 60e 0.2e 85e 1e 100e 0e ] { ae }
    start dur 0.2e f+ f+ 4e :vibrato 0.005e :vib-env ve :amp-env ae fofins
    ae gen-free

    vct[ 0e 0e 0.5e 0.5e 3e .25e 6e 0.1e 10e .1e 50e .1e 60e 0.2e 85e 1e 100e 0e ] to ae
    start dur 0.2e f+ f+ 4e :freq 6e 5e f/ 540e f* :vibrato 0.005e :vib-env ve :amp-env ae fofins
    ae gen-free

    vct[ 0e 0e 1e 3e 3e 1e 6e 0.2e 10e 0.1e 50e 0.1e 60e 0.2e 85e 1e 100e 0e ] to ae
    start dur 0.2e f+ f+ 4e :freq 135e :vibrato 0.005e :vib-env ve :amp-env ae fofins
    ve gen-free
    ae gen-free
    dur 0.2e f+ 4e f+ wait
;event

: j0 { f: x -- r }
    x fabs 8e f< if
	x x f* { f: y }
	57568490574.0e y
	-13362590354.0e y
	651619640.7e y
	-11214424.18e y
	77392.33017e y
	-184.9052456e f* f+ f* f+ f* f+ f* f+ f* f+
	57568490411.0e y
	1029532985.0e y
	9494680.718e y
	59272.64853e y
	267.8532712e y f+ f* f+ f* f+ f* f+ f* f+
	f/
    else
	x fabs { f: ax }
	8e ax f/ { f: z }
	z z f* { f: y }
	ax 0.785398164e f- { f: xx }
	1e y
	-0.1098628627e-2 y
	0.2734510407e-4 y
	-0.2073370639e-5 y
	0.2093887211e-6 f* f+ f* f+ f* f+ f* f+ { f: ans1 }
	-0.1562499995e-1 y
	0.1430488765e-3 y
	-0.6911147651e-5 y
	0.7621095161e-6 y
	-0.934945152e-7 f* f+ f* f+ f* f+ f* f+ { f: ans2 }
	0.636619772e ax f/ fsqrt xx fcos f* ans1 f* z xx fsin f* ans2 f* f-
    then
;
\ BES-FM
instrument: bes-fm { f: start f: dur f: freq f: amp f: ratio f: index -- }
    freq hz>radians { f: car-incr }
    ratio car-incr f* { f: mod-incr }
    :envelope vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] :scaler amp :duration dur make-env { ampenv }
    90e random :locsig-degree
    0e ( car-ph ) 0e ( mod-ph )
    start dur run-instrument
	\ car-ph  mod-ph result
	fover ( car-ph ) j0 ampenv env f* \ result
	\ mod-ph result car-ph
	frot ( car-ph ) car-incr f+ 2 fpick ( mod-ph ) j0 index f* f+ ( car-ph )
	frot ( mod-ph ) mod-incr f+ ( mod-ph ) frot
    end-run
    fdrop fdrop
    ampenv gen-free
;instrument
\    0e 0e { f: mod-ph f: car-ph }
\    start dur run-instrument
\	ampenv env car-ph j0 f*		\ result
\	car-ph car-incr f+ index mod-ph j0 f* f+ to car-ph
\	mod-ph mod-incr f+ to mod-ph
\    end-run
' bes-fm $" bes-fm ( f: start f: dur f: freq f: amp f: ratio f: index -- )\n"
$" 0e 2e 440e 5e 1e 8e ' bes-fm with-sound" $+ help!

event: bess-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 5e 1e 8e bes-fm
    dur wait
;event

\ FM TRUMPET
\
\ Dexter Morrill's FM-trumpet: from CMJ feb 77 p51
$" frq1"     dup constant :frq1     float-keyword!
$" frq2"     dup constant :frq2	    float-keyword!
$" amp1"     dup constant :amp1	    float-keyword!
$" amp2"     dup constant :amp2	    float-keyword!
$" ampatt1"  dup constant :ampatt1  float-keyword!
$" ampdec1"  dup constant :ampdec1  float-keyword!
$" ampatt2"  dup constant :ampatt2  float-keyword!
$" ampdec2"  dup constant :ampdec2  float-keyword!
$" modfrq1"  dup constant :modfrq1  float-keyword!
$" modind11" dup constant :modind11 float-keyword!
$" modind12" dup constant :modind12 float-keyword!
$" modfrq2"  dup constant :modfrq2  float-keyword!
$" modind21" dup constant :modind21 float-keyword!
$" modind22" dup constant :modind22 float-keyword!
$" rvibamp"  dup constant :rvibamp  float-keyword!
$" rvibfrq"  dup constant :rvibfrq  float-keyword!
$" vibamp"   dup constant :vibamp   float-keyword!
$" vibfrq"   dup constant :vibfrq   float-keyword!
$" vibatt"   dup constant :vibatt   float-keyword!
$" vibdec"   dup constant :vibdec   float-keyword!
$" frqskw"   dup constant :frqskw   float-keyword!
$" frqatt"   dup constant :frqatt   float-keyword!
$" ampenv1"  constant :ampenv1
$" ampenv2"  constant :ampenv2
$" indenv1"  constant :indenv1
$" indenv2"  constant :indenv2

instrument: fm-trumpet ( start dur keyword-args -- )
    make-hash { args }
    250e   :frq1     args set-fargs
    1500e  :frq2     args set-fargs
    0.5e   :amp1     args set-fargs
    0.1e   :amp2     args set-fargs
    0.03e  :ampatt1  args set-fargs
    0.35e  :ampdec1  args set-fargs
    0.03e  :ampatt2  args set-fargs
    0.3e   :ampdec2  args set-fargs
    250e   :modfrq1  args set-fargs
    0e     :modind11 args set-fargs
    2.66e  :modind12 args set-fargs
    250e   :modfrq2  args set-fargs
    0e     :modind21 args set-fargs
    1.8e   :modind22 args set-fargs
    0.007e :rvibamp  args set-fargs
    125e   :rvibfrq  args set-fargs
    0.007e :vibamp   args set-fargs
    7e     :vibfrq   args set-fargs
    0.6e   :vibatt   args set-fargs
    0.2e   :vibdec   args set-fargs
    0.03e  :frqskw   args set-fargs
    0.06e  :frqatt   args set-fargs
    nil    :ampenv1  get-args { ampenv1 }
    nil    :ampenv2  get-args { ampenv2 }
    nil    :indenv1  get-args { indenv1 }
    nil    :indenv2  get-args { indenv2 }
    { f: start f: dur }
    ampenv1 if
	false
    else
	vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ] to ampenv1
	true
    then { del-amp1 }
    ampenv2 if
	false
    else
	vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ] to ampenv2
	true
    then { del-amp2 }
    indenv1 if
	false
    else
	vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ] to indenv1
	true
    then { del-ind1 }
    indenv2 if
	false
    else
	vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ] to indenv2
	true
    then { del-ind2 }
    :envelope
    vct[ 0e 1e 25e 0.1e 75e 0e 100e 0e ]
    25e
    100e :vibatt args fhash@ dur f/ f* 45e fmin
    75e
    100e 1e :vibdec args fhash@ dur f/ f- f* 55e fmax
    stretch-envelope
    :scaler :vibamp args fhash@ :duration dur make-env { per-vib-f }
    :frequency :rvibfrq args fhash@ :amplitude :rvibamp args fhash@ make-rand-interp { ran-vib }
    :frequency :vibfrq args fhash@ make-oscil { per-vib }
    75e 100e 1e 0.01e dur f/ f- f* fmax { f: dec-01 }
    :envelope
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ]
    25e 25e 100e :frqatt args fhash@ dur f/ f* fmin 75e dec-01 stretch-envelope
    :scaler :frqskw args fhash@ :duration dur make-env { frq-f }
    25e 100e :ampatt1 args fhash@ dur f/ f* fmin { f: ampattpt1 }
    75e 100e 1e :ampdec1 args fhash@ dur f/ f- f* fmax { f: ampdecpt1 }
    25e 100e :ampatt2 args fhash@ dur f/ f* fmin { f: ampattpt2 }
    75e 100e 1e :ampdec2 args fhash@ dur f/ f- f* fmax { f: ampdecpt2 }
    :envelope indenv1 25e ampattpt1 75e dec-01 stretch-envelope
    :scaler :modfrq1 args fhash@ :modind12 args fhash@ :modind11 args fhash@ f- f*
    :duration dur make-env { mod1-f }
    :frequency 0e make-oscil { mod1 }
    :frequency 0e make-oscil { car1 }
    :envelope ampenv1 25e ampattpt1 75e ampdecpt1 stretch-envelope
    :scaler :amp1 args fhash@ :duration dur make-env { car1-f }
    :envelope indenv2 25e ampattpt2 75e dec-01 stretch-envelope
    :scaler :modfrq2 args fhash@ :modind22 args fhash@ :modind21 args fhash@ f- f*
    :duration dur make-env { mod2-f }
    :frequency 0e make-oscil { mod2 }
    :frequency 0e make-oscil { car2 }
    :envelope ampenv2 25e ampattpt2 75e ampdecpt2 stretch-envelope
    :scaler :amp2 args fhash@ :duration dur make-env { car2-f }
    :frq1 args fhash@ { f: frq1 }
    :frq2 args fhash@ { f: frq2 }
    :modfrq1 args fhash@ { f: modfrq1 }
    :modfrq2 args fhash@ { f: modfrq2 }
    90e random :locsig-degree
    start dur run-instrument
	1e 0e ran-vib rand-interp f+
	1e per-vib-f env per-vib oscil-0 f* f+ f*
	1e frq-f env f+ f* hz>radians { f: frq-change }
	car1-f env
	modfrq1 frq-change f* mod1 oscil-1 mod1-f env f* frq1 f+ frq-change f*
	car1 oscil-1 f*
	car2-f env
	modfrq2 frq-change f* mod2 oscil-1 mod2-f env f* frq2 f+ frq-change f*
	car2 oscil-1 f* f+
    end-run
    del-amp1 if ampenv1 gen-free then
    del-amp2 if ampenv2 gen-free then
    del-ind1 if indenv1 gen-free then
    del-ind2 if indenv2 gen-free then
    per-vib-f gen-free
    ran-vib   gen-free
    per-vib   gen-free
    frq-f     gen-free
    mod1-f    gen-free
    mod1      gen-free
    car1      gen-free
    car1-f    gen-free
    mod2-f    gen-free
    mod2      gen-free
    car2      gen-free
    car2-f    gen-free
    args      gen-free
;instrument
' fm-trumpet $" fm-trumpet ( f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :frq1     -- 250e\n" $+
$" \\ :frq2     -- 1500e\n" $+
$" \\ :amp1     -- 0.5e\n" $+
$" \\ :amp2     -- 0.1e\n" $+
$" \\ :ampatt1  -- 0.03e\n" $+
$" \\ :ampdec1  -- 0.35e\n" $+
$" \\ :ampatt2  -- 0.03e\n" $+
$" \\ :ampdec2  -- 0.3e\n" $+
$" \\ :modfrq1  -- 250e\n" $+
$" \\ :modind11 -- 0e\n" $+
$" \\ :modind12 -- 2.66e\n" $+
$" \\ :modfrq2  -- 250e\n" $+
$" \\ :modind21 -- 0e\n" $+
$" \\ :modind22 -- 1.8e\n" $+
$" \\ :rvibamp  -- 0.007e\n" $+
$" \\ :rvibfrq  -- 125e\n" $+
$" \\ :vibamp   -- 0.007e\n" $+
$" \\ :vibfrq   -- 7e\n" $+
$" \\ :vibatt   -- 0.6e\n" $+
$" \\ :vibdec   -- 0.2e\n" $+
$" \\ :frqskw   -- 0.03e\n" $+
$" \\ :frqatt   -- 0.06e\n" $+
$" \\ :ampenv1  -- vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ]\n" $+
$" \\ :ampenv2  -- vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ]\n" $+
$" \\ :indenv1  -- vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ]\n" $+
$" \\ :indenv2  -- vct[ 0e 0e 25e 1e 75e 0.9e 100e 0e ]\n" $+
$" 0e 2e ' fm-trumpet with-sound" $+ help!

event: fm-trumpet-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur fm-trumpet
    dur wait
;event

struct
    cell%   field sin-evens
    cell%   field cos-evens
    cell%   field sin-odds
    cell%   field cos-odds
    cell%   field frmfs
    cell%   field sin-coeffs
    cell%   field cos-coeffs
    sfloat% field amps
end-struct pqw-vox%

\ PQWVOX
\
\ translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's
\ waveshaping voice instrument (using phase quadrature waveshaping))
instrument: pqw-vox { f: start f: dur f: freq f: spacing-freq f: amp ampfun freqfun f: freqscl phonemes formant-amps formant-shapes -- }
    :frequency 0e make-oscil { car-sin }
    :frequency 0e :initial-phase half-pi make-oscil { car-cos }
    :envelope ampfun :scaler amp :duration dur make-env { ampf }
    :envelope freqfun :scaler freqscl freq f* :duration dur :offset freq make-env { freqf }
    :frequency 6e :amplitude freq 0.1e f* make-triangle-wave { per-vib }
    :frequency 20e :amplitude freq 0.05e f* make-rand-interp { ran-vib }
    phonemes length { plen }
    plen make-vct { phone1 }
    plen make-vct { phone2 }
    plen make-vct { phone3 }
    plen 1- 0 do
	i    phonemes array@ s>f fdup i phone1 vct! fdup i phone2 vct! i phone3 vct!
	i 1+ phonemes array@ clm-ins-formants hash@ { ary }
	0 ary vct@ i 1+ phone1 vct!
	1 ary vct@ i 1+ phone2 vct!
	2 ary vct@ i 1+ phone3 vct!
    2 +loop
    phone1 phone2 phone3 3 >array { phones }
    nil { pv }
    formant-amps length make-array map
	pqw-vox% %alloc to pv
	:frequency 0e make-oscil pv sin-evens !
	:frequency 0e make-oscil pv sin-odds !
	:frequency 0e :initial-phase half-pi make-oscil pv cos-evens !
	:frequency 0e :initial-phase half-pi make-oscil pv cos-odds !
	i formant-shapes array@ normalize-partials { shape }
	shape 1 partials>polynomial pv cos-coeffs !
	shape 0 partials>polynomial pv sin-coeffs !
	:envelope i phones array@ :duration dur make-env pv frmfs !
	i formant-amps vct@ pv amps sf!
	pv
    end-map { values }
    4 make-vct { vals }
    90e random :locsig-degree
    start dur run-instrument
	freqf env 0e per-vib triangle-wave f+ 0e ran-vib rand-interp f+ { f: frq }
	frq spacing-freq freq f/ f* hz>radians
	fdup car-sin oscil-1 { f: carsin }
	car-cos oscil-1 { f: carcos }
	0e ( sum )
	values each
	    to pv
	    pv frmfs @ env frq f/ { f: frm0 }
	    frm0 floor            { f: frm-fint }
	    frm-fint f>s 2 mod unless
		frm-fint frq f* hz>radians       0 vals vct! ( even-freq )
		frm-fint 1e f+ frq f* hz>radians 1 vals vct! ( odd-freq )
		frm0 frm-fint f-                 3 vals vct! ( odd-amp )
		1e 3 vals vct@ f-                2 vals vct! ( even-amp )
	    else
		frm-fint frq f* hz>radians       1 vals vct! ( odd-freq )
		frm-fint 1e f+ frq f* hz>radians 0 vals vct! ( even-freq )
		frm0 frm-fint f-                 2 vals vct! ( even-amp )
		1e 2 vals vct@ f-                3 vals vct! ( odd-amp )
	    then
	    pv cos-coeffs @ carcos polynomial { f: fax }
	    pv sin-coeffs @ carcos polynomial carsin f* { f: yfax }
	    0 vals vct@ pv sin-evens @ oscil-1 yfax f*
	    0 vals vct@ pv cos-evens @ oscil-1 fax f* f- 2 vals vct@ f*
	    1 vals vct@ pv sin-odds @  oscil-1 yfax f*
	    1 vals vct@ pv cos-odds @  oscil-1 fax f* f- 3 vals vct@ f* f+ pv amps sf@ f* f+
	end-each
	ampf env f*
    end-run
    car-sin gen-free
    car-cos gen-free
    ampf gen-free
    freqf gen-free
    per-vib gen-free
    ran-vib gen-free
    phone1 gen-free
    phone2 gen-free
    phone3 gen-free
    phones gen-free
    values gen-free
    vals gen-free
;instrument
' pqw-vox $" pqw-vox ( f: start f: dur f: freq f: spacing-freq f: amp\n"
$"            ampfun freqfun f: freqscl phonemes formant-amps formant-shapes -- )" $+ help!

event: pqw-vox-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 50e 1e 100e 0e ] { ampfun }
    vct[ 0e 0e 100e 0e ] { freqfun }
    vct[ 0e 0e 100e 1e ] { freqramp }
    vct[ 1e 1e 2e 0.5e ] vct[ 1e 0.5e 2e 0.5e 3e 1e ] vct[ 1e 1e 4e 0.5e ] 3 >array { shapes1 }
    vct[ 1e 1e 2e 0.5e ] vct[ 1e 1e 2e 0.5e 3e 0.2e 4e 0.1e ] vct[ 1e 1e 3e 0.1e 4e 0.5e ]
    3 >array { shapes2 }
    vct[ 1e 1e 2e 0.5e ] vct[ 1e 1e 4e 0.1e ] vct[ 1e 1e 2e 0.1e 4e 0.05e ] 3 >array { shapes3 }
    vct[ 1e 1e 2e 0.5e 3e 0.1e 4e 0.01e ] vct[ 1e 1e 4e 0.1e ] vct[ 1e 1e 2e 0.1e 4e 0.05e ]
    3 >array { shapes4 }

    start dur 300e 300e 0.5e ampfun freqfun 0e array[ 0 :L: 100 :L: ] vct[ 0.33e 0.33e 0.33e ]
    shapes1 pqw-vox
    start dur 0.2e f+ f+ to start
    start dur 200e 200e 0.5e ampfun freqramp 0.1e array[ 0 :UH: 100 :ER: ] vct[ 0.8e 0.15e 0.05e ]
    shapes2 pqw-vox
    start dur 0.2e f+ f+ to start
    start dur 100e 314e 0.5e ampfun freqramp 0.1e array[ 0 :UH: 100 :ER: ] vct[ 0.8e 0.15e 0.05e ]
    shapes2 pqw-vox
    start dur 0.2e f+ f+ to start
    start dur 200e 314e 0.5e ampfun freqramp 0.01e array[ 0 :UH: 100 :ER: ] vct[ 0.8e 0.15e 0.05e ]
    shapes3 pqw-vox
    start dur 0.2e f+ f+ to start
    start dur 100e 414e 0.5e ampfun freqramp 0.01e array[ 0 :OW: 50 :E: 100 :ER: ]
    vct[ 0.8e 0.15e 0.05e ] shapes4 pqw-vox
    ampfun gen-free
    freqfun gen-free
    freqramp gen-free
    shapes1 gen-free
    shapes2 gen-free
    shapes3 gen-free
    shapes4 gen-free
    dur 5e f* wait
;event

\ STEREO-FLUTE
\ slightly simplified [MS]
$" noise"      dup constant :noise      float-keyword!
$" decay"      dup constant :decay	float-keyword!
$" mp-size"    dup constant :mp-size	float-keyword!
$" fbk-scl1"   dup constant :fbk-scl1	float-keyword!
$" fbk-scl2"   dup constant :fbk-scl2	float-keyword!
$" out-scl"    dup constant :out-scl	float-keyword!
$" vib-rate"   dup constant :vib-rate	float-keyword!
$" vib-amount" dup constant :vib-amount	float-keyword!
$" ran-rate"   dup constant :ran-rate	float-keyword!
$" ran-amount" dup constant :ran-amount	float-keyword!
$" b1"         dup constant :b1         float-keyword!
$" flow-env"   constant :flow-env

instrument: stereo-flute ( start dur keyword-args -- )
    make-hash { args }
    440e       :frequency  args set-fargs
    0.5e       :amplitude  args set-fargs
    false      :flow-env   get-args  { flow-env }
    0.01e      :decay      args set-fargs
    0.0356e    :noise      get-fargs { f: noise }
    0.5e       :mp-size    args set-fargs \ mouthpiece
    0.5e       :fbk-scl1   get-fargs { f: fbk-scl1 }
    0.55e      :fbk-scl2   get-fargs { f: fbk-scl2 }
    1e         :out-scl    get-fargs { f: out-scl }
    0.7e       :a0         args set-fargs
    -0.3e      :b1         args set-fargs
    5e         :vib-rate   args set-fargs
    0.03e      :vib-amount get-fargs { f: vib-amount }
    5e         :ran-rate   args set-fargs
    0.03e      :ran-amount get-fargs { f: ran-amount }
    { f: start f: dur }
    flow-env if false else vct[ 0e 1e 100e 1e ] to flow-env true then { flow-del }
    :envelope flow-env :scaler :amplitude args fhash@
    :start start seconds>samples
    :end dur :decay args fhash@ f- seconds>samples start seconds>samples + make-env { flowf }
    :frequency :vib-rate args fhash@ make-oscil { p-vib }
    :frequency :ran-rate args fhash@ make-rand-interp { ran-vib }
    :frequency mus-srate@ f2/ :amplitude 1e make-rand { breath }
    :size :mp-size args fhash@ mus-srate@ :frequency args fhash@ f/ f* f>s make-delay { mp }
    :size mus-srate@ :frequency args fhash@ f/ f>s make-delay { bore }
    :a0 args fhash@ :b1 args fhash@ make-one-pole { rlf }
    0e 0e 0e 0e { f: out-sig f: cur-diff f: prev-out-sig f: prev-dc-blocked }
    90e random :locsig-degree
    start dur run-instrument
	out-sig bore delay-1 { f: delay-sig }
	cur-diff mp delay-1 { f: mp-sig }
	p-vib oscil-0 vib-amount f*
	0e ran-vib rand-interp ran-amount f* f+ flowf env f+ { f: cur-flow }
	0e breath rand cur-flow f* noise f* cur-flow f+ fbk-scl1 delay-sig f* f+ to cur-diff
	mp-sig mp-sig mp-sig f* mp-sig f* f- fbk-scl2 delay-sig f* f+ rlf one-pole to out-sig
	out-sig prev-out-sig f- 0.995e prev-dc-blocked f* f+ { f: dc-blocked }
	out-sig to prev-out-sig
	dc-blocked to prev-dc-blocked
	out-scl dc-blocked f*
    end-run
    rlf gen-free
    bore gen-free
    mp gen-free
    breath gen-free
    ran-vib gen-free
    p-vib gen-free
    flowf gen-free
    flow-del if flow-env gen-free then
;instrument
' stereo-flute $" stereo-flute ( f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :frequency  -- 440e\n" $+
$" \\ :amplitude  -- 0.5e\n" $+
$" \\ :flow-env   -- vct[ 0e 1e 100e 1e ]\n" $+
$" \\ :decay      -- 0.01e\n" $+
$" \\ :noise      -- 0.0356e\n" $+
$" \\ :mp-size    -- 0.5e\n" $+
$" \\ :fbk-scl1   -- 0.5e\n" $+
$" \\ :fbk-scl2   -- 0.55e\n" $+
$" \\ :out-scl    -- 1e\n" $+
$" \\ :a0         -- 0.7e\n" $+
$" \\ :b1         -- -0.3e\n" $+
$" \\ :vib-rate   -- 5e\n" $+
$" \\ :vib-amount -- 0.03e\n" $+
$" \\ :ran-rate   -- 5e\n" $+
$" \\ :ran-amount -- 0.03e\n" $+
$" 0e 2e :flow-env vct[ 0e 0e 1e 1e 2e 1e 3e 0e ] ' stereo-flute with-sound" $+ help!

event: flute-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur :flow-env vct[ 0e 0e 1e 1e 2e 1e 3e 0e ] stereo-flute
    dur wait
;event

\ FM-BELL
instrument: fm-bell ( keyword-args -- )
    0e         :beg       get-fargs { f: start }
    1e         :dur       get-fargs { f: dur }
    440e       :frequency get-fargs { f: freq }
    0.5e       :amplitude get-fargs { f: amp }
    1e         :fm-index  get-fargs { f: index }
    false      :amp-env   get-args  { amp-env }
    false      :index-env get-args  { index-env }
    amp-env ?envelope if
	false
    else
	vct[ 0e 0e 0.1e 1e 10e 0.6e 25e 0.3e 50e 0.15e 90e 0.1e 100e 0e ] to amp-env
	true
    then { del-amp }
    index-env ?envelope if
	false
    else
	vct[ 0e 1e 2e 1.1e 25e 0.75e 75e 0.5e 100e 0.2e ] to index-env
	true
    then { del-ind }
    freq 32e f* hz>radians { f: fm-ind1 }
    8e freq 50e f/ f- 4e f* hz>radians { f: fm-ind2 }
    1.4e freq 250e f/ f- 0.705e f* fm-ind2 f* { f: fm-ind3 }
    20e freq 20e f/ f- 32e f* hz>radians { f: fm-ind4 }
    :frequency freq f2* make-oscil { mod1 }
    :frequency freq 1.41e f* make-oscil { mod2 }
    :frequency freq 2.82e f* make-oscil { mod3 }
    :frequency freq 2.4e f* make-oscil { mod4 }
    :frequency freq make-oscil { car1 }
    :frequency freq make-oscil { car2 }
    :frequency freq 2.4e f* make-oscil { car3 }
    :envelope amp-env :scaler amp :duration dur make-env { ampf }
    :envelope index-env :scaler index :duration dur make-env { indf }
    90e random :locsig-degree
    start dur run-instrument
	indf env { f: fmenv }
	fmenv fm-ind1 f* mod1 oscil-0 f* car1 oscil-1
	fmenv fm-ind2 mod2 oscil-0 f* fm-ind3 mod3 oscil-0 f* f+ f* car2 oscil-1 .15e f* f+
	fmenv fm-ind4 f* mod4 oscil-0 f* car3 oscil-1 0.15e f* f+ ampf env f*
    end-run
    mod1 gen-free
    mod2 gen-free
    mod3 gen-free
    mod4 gen-free
    car1 gen-free
    car2 gen-free
    car3 gen-free
    ampf gen-free
    indf gen-free
    del-amp if amp-env gen-free then
    del-ind if index-env gen-free then
;instrument
' fm-bell $" fm-bell ( keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :beg       -- 0e (secs)\n" $+
$" \\ :dur       -- 1e (secs)\n" $+
$" \\ :freq      -- 440e\n" $+
$" \\ :amp       -- 0.5e\n" $+
$" \\ :fm-index  -- 1e\n" $+
$" \\ :amp-env   -- vct[ 0e 0e 0.1e 1e 10e 0.6e 25e 0.3e 50e 0.15e 90e 0.1e 100e 0e ]\n" $+
$" \\ :index-env -- vct[ 0e 1e 2e 1.1e 25e 0.75e 75e 0.5e 100e 0.2e ]\n" $+
$" 220e :freq ' fm-bell with-sound" $+ help!

event: fm-bell-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start :beg dur :dur fm-bell
    dur wait
;event

\ FM-INSECT
\ clm-3/insect.ins
instrument: fm-insect { f: start f: dur f: freq f: amp w: amp-env f: mod-freq f: mod-skew w: mod-freq-env f: mod-index w: mod-index-env f: fm-index f: fm-ratio -- }
    :frequency freq make-oscil { carrier }
    :frequency mod-freq make-oscil { fm1-osc }
    :frequency fm-ratio freq f* make-oscil { fm2-osc }
    :envelope amp-env :scaler amp :duration dur make-env { ampf }
    :envelope mod-index-env :scaler mod-index hz>radians :duration dur make-env { indf }
    :envelope mod-freq-env :scaler mod-skew hz>radians :duration dur make-env { modfrqf }
    fm-index fm-ratio f* freq f* hz>radians { f: fm2-amp }
    90e random :locsig-degree
    start dur run-instrument
	indf env   modfrqf env fm1-osc oscil-1   f* { f: garble-in }
	fm2-amp    garble-in   fm2-osc oscil-1   f* { f: garble-out }
	ampf env   garble-out garble-in f+ carrier oscil-1   f*
    end-run
    carrier gen-free
    fm1-osc gen-free
    fm2-osc gen-free
    ampf gen-free
    indf gen-free
    modfrqf gen-free
;instrument    
' fm-insect
$" fm-insect ( f: start f: dur f: freq f: amp w: amp-env\n"
$"             f: mod-freq f: mod-skew w: mod-freq-env f: mod-index w: mod-index-env\n" $+
$"             f: fm-index f: fm-ratio )\n" $+
$" see clm-3/insect.ins and fm-insect-test in clm-ins.fs" $+ help!


event: fm-insect-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 40e 1e 95e 1e 100e 0.5e ] { locust }
    vct[ 0e 1e 25e 0.7e 75e 0.78e 100e 1e ] { bug-hi }
    vct[ 0e 0e 25e 1e 75e 0.7e 100e 0e ] { amp }

    start 0.000e f+ 1.699e 4142.627e 0.015e amp 60e -16.707e locust 500.866e
    bug-hi 0.346e 0.5e fm-insect
    start 0.195e f+ 0.233e 4126.284e 0.030e amp 60e -12.142e locust 649.490e
    bug-hi 0.407e 0.5e fm-insect
    start 0.217e f+ 2.057e 3930.258e 0.045e amp 60e  -3.011e locust 562.087e
    bug-hi 0.591e 0.5e fm-insect
    start 2.100e f+ 1.500e  900.627e 0.060e amp 40e -16.707e locust 300.866e
    bug-hi 0.346e 0.5e fm-insect
    start 3.000e f+ 1.500e  900.627e 0.060e amp 40e -16.707e locust 300.866e
    bug-hi 0.046e 0.5e fm-insect
    start 3.450e f+ 1.500e  900.627e 0.090e amp 40e -16.707e locust 300.866e
    bug-hi 0.006e 0.5e fm-insect
    start 3.950e f+ 1.500e  900.627e 0.120e amp 40e -10.707e locust 300.866e
    bug-hi 0.346e 0.5e fm-insect
    start 4.300e f+ 1.500e  900.627e 0.090e amp 40e -20.707e locust 300.866e
    bug-hi 0.246e 0.5e fm-insect
    locust gen-free
    bug-hi gen-free
    amp gen-free
    5.8e wait
;event

\ FM-DRUM
\
\ Jan Mattox's fm drum:
instrument: fm-drum { f: start f: dur f: freq f: amp f: index high -- }
    high if 3.414e 8.525e else 1.414e 3.515e then { f: casrat f: fmrat }
    :envelope vct[ 0e 0e 25e 0e 75e 1e 100e 1e ]
    :scaler high if 66e hz>radians else 0e then
    :duration dur make-env { glsf }
    0e 0e 3e 0.05e 5e 0.2e 7e 0.8e 8e 0.95e 10e 1.0e 12e 0.95e 20e 0.3e 30e 0.1e 100e 0e
    20 >vct { ampfun }
    100e high if 0.01e else 0.015e then f* dur f/ { f: atdrpt }
    :envelope
    ampfun 10e atdrpt 15e atdrpt 1e f+ 100e 100e dur 0.2e f- dur f/ f* f- fmax stretch-envelope
    :scaler amp :duration dur make-env { ampf }
    0e 0e 5e 0.014e 10e 0.033e 15e 0.061e 20e 0.099e
    25e 0.153e 30e 0.228e 35e 0.332e 40e 0.477e 45e 0.681e
    50e 0.964e 55e 0.681e 60e 0.478e 65e 0.332e 70e 0.228e
    75e 0.153e 80e 0.099e 85e 0.061e 90e 0.033e 95e 0.0141e 100e 0e 42 >vct { indxfun }
    100e 100e dur 0.1e f- dur f/ f* f- { f: indxpt }
    indxfun 50e atdrpt 65e indxpt stretch-envelope { divindxf }
    :envelope divindxf :duration dur
    :scaler index fmrat freq f* f* hz>radians pi fmin make-env { indxf }
    :envelope divindxf :duration dur
    :scaler index casrat freq f* f* hz>radians pi fmin make-env { mindxf }
    :envelope
    ampfun 10e atdrpt 90e atdrpt 1e f+ 100e 100e dur 0.05e f- dur f/ f* f- fmax stretch-envelope
    :duration dur :scaler 7000e hz>radians pi fmin make-env { devf }
    :frequency 7000e :amplitude 1e make-rand { rn }
    :frequency freq make-oscil { car }
    :frequency freq fmrat f* make-oscil { fmosc }
    :frequency freq casrat f* make-oscil { cc }
    90e random :locsig-degree
    start dur run-instrument
	glsf env { f: gls }
	devf env 0e rn rand f* gls casrat f* f+ cc oscil-1
	mindxf env f* gls fmrat f* f+ fmosc oscil-1
	indxf env f* gls f+ car oscil-1 ampf env f*
    end-run
    glsf gen-free
    ampfun gen-free
    ampf gen-free
    indxfun gen-free
    divindxf gen-free
    indxf gen-free
    mindxf gen-free
    devf gen-free
    rn gen-free
    car gen-free
    fmosc gen-free
    cc gen-free
;instrument
' fm-drum $" fm-drum ( f: start f: dur f: freq f: amp f: index high -- )" help!

event: fm-drum-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }

    start dur 55e 0.3e 5e false fm-drum
    start dur 0.2e f+ f+ to start
    start dur 66e 0.3e 4e true  fm-drum
    dur f2* 0.4e f+ wait
;event

\ FM-GONG
\
\ Paul Weineke's gong.
instrument: gong { f: start f: dur f: freq f: amp -- }
    0.01e 1.160e freq f* f* hz>radians { f: indx01 }
    0.30e 1.160e freq f* f* hz>radians { f: indx11 }
    0.01e 3.140e freq f* f* hz>radians { f: indx02 }
    0.38e 3.140e freq f* f* hz>radians { f: indx12 }
    0.01e 1.005e freq f* f* hz>radians { f: indx03 }
    0.50e 1.005e freq f* f* hz>radians { f: indx13 }
    5e { f: atpt }
    100e 0.002e dur f/ f* { f: atdur }
    0e 0e 3e 1e 15e 0.5e 27e 0.25e 50e 0.1e 100e 0e  12 >vct { expf }
    0e 0e 15e 0.3e 30e 1.0e 75e 0.5e 100e 0e  10 >vct { rise }
    0e 0e 75e 1.0e 98e 1.0e 100e 0e  8 >vct { fmup }
    0e 0e 2e 1.0e 100e 0e  6 >vct { fmdwn }
    :envelope expf atpt atdur 0e 0e stretch-envelope :scaler amp :duration dur make-env { ampfun }
    :envelope fmup  :scaler indx11 indx01 f- :duration dur :offset indx01 make-env {  indxfun1 }
    :envelope fmdwn :scaler indx12 indx02 f- :duration dur :offset indx02 make-env { indxfun2 }
    :envelope rise  :scaler indx13 indx03 f- :duration dur :offset indx03 make-env { indxfun3 }
    :frequency freq make-oscil { car }
    :frequency freq 1.160e f* make-oscil { mod1 }
    :frequency freq 3.140e f* make-oscil { mod2 }
    :frequency freq 1.005e f* make-oscil { mod3 }
    90e random :locsig-degree
    start dur run-instrument
	mod3 oscil-0 indxfun3 env f*
	mod2 oscil-0 indxfun2 env f* f+
	mod1 oscil-0 indxfun1 env f* f+ car oscil-1 ampfun env f*
    end-run
    expf gen-free
    rise gen-free
    fmup gen-free
    fmdwn gen-free
    ampfun gen-free
    indxfun1 gen-free
    indxfun2 gen-free
    indxfun3 gen-free
    car gen-free
    mod1 gen-free
    mod2 gen-free
    mod3 gen-free
;instrument
' gong $" gong ( f: start f: dur f: freq f: amp -- )" help!

event: gong-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 261.61e 0.6e gong
    dur wait
;event

\ ATTRACT
\
\ by James McCartney, from CMJ vol 21 no 3 p 6
instrument: attract { f: start f: dur f: amp f: c -- }
    0.2e fdup { f: a f: b }
    0.04e { f: dt }
    amp f2/ c f/ { f: scale }
    -1e { f: x }
    0e 0e { f: y f: z }
    90e random :locsig-degree
    start dur run-instrument
	x dt y z f+ f* f- { f: x1 }
	dt x a y f* f+ f* y f+ to y
	dt b x z f* f+ c z f* f- f* z f+ to z
	x1 to x
	scale x f*
    end-run
;instrument
' attract $" attract ( f: start f: dur f: amp f: c -- )" help!

event: attract-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 0.5e 2e attract
    dur wait
;event

\ PQW
\
\ phase-quadrature waveshaping used to create asymmetric (i.e. single
\ side-band) spectra.  The basic idea here is a variant of sin x sin y
\ - cos x cos y = cos (x + y)
\ 
\ clm-3/pqw.ins
instrument: pqw { f: start f: dur f: sfreq f: cfreq f: amp ampfun indexfun parts -- }
    parts normalize-partials { nparts }
    :frequency sfreq :initial-phase half-pi make-oscil { sp-cos }
    :frequency sfreq make-oscil { sp-sin }
    :frequency cfreq :initial-phase half-pi make-oscil { c-cos }
    :frequency cfreq make-oscil { c-sin }
    nparts 0 partials>polynomial { sin-coeffs }
    nparts 1 partials>polynomial { cos-coeffs }
    :envelope ampfun :scaler amp :duration dur make-env { amp-env }
    :envelope indexfun :duration dur make-env { ind-env }
    0e 0e 0e 0e { f: vib f: ax f: fax f: yfax }
    cfreq sfreq f/ { f: r }
    :frequency 5e :amplitude 0.005e sfreq f* hz>radians make-triangle-wave { tr }
    :frequency 12e :amplitude 0.005e sfreq f* hz>radians make-rand-interp { rn }
    90e random :locsig-degree
    start dur run-instrument
	tr 0e triangle-wave rn 0e rand-interp f+ to vib
	1e ind-env env fmin vib sp-cos oscil-1 f* to ax
	cos-coeffs ax polynomial to fax
	vib 0e sp-sin oscil sin-coeffs ax polynomial f* to yfax
	amp-env env
	vib r f* c-sin oscil-1 yfax f*
	vib r f* c-cos oscil-1 fax f* f- f*
    end-run
    sp-cos gen-free
    sp-sin gen-free
    c-cos gen-free
    c-sin gen-free
    amp-env gen-free
    ind-env gen-free
    sin-coeffs gen-free
    cos-coeffs gen-free
    nparts gen-free
;instrument
' pqw $" pqw ( f: start f: dur f: sfreq f: cfreq f: amp ampfun indexfun parts )\n"
$" see clm-3/pqw.ins and pqw-test in clm-ins.fs" $+ help!

event: pqw-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }

    start 5e 200e 1000e 0.2e
    vct[ 0e 0e 25e 1e 100e 0e ] vct[ 0e 1e 100e 0e ] vct[ 2e 0.1e 3e 0.3e 6e 0.5e ] pqw
    5e wait
;event

\ taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
\ in a bit of a hurry and may not have made slavishly accurate
\ translations.  Please let me (bil@ccrma.stanford.edu) know of any
\ serious (non-envelope) errors.
\
\ from Perry Cook's TubeBell.cpp
instrument: tubebell { f: start f: dur f: freq f: amp f: base -- }
    :frequency freq 0.995e f* make-oscil { osc0 }
    :frequency freq 0.995e 1.414e f* f* make-oscil { osc1 }
    :frequency freq 1.005e f* make-oscil { osc2 }
    :frequency freq 1.414e f* make-oscil { osc3 }
    :envelope 0e 0e 0.005e 1e dur 0e  6 >vct :base base :duration dur make-env { ampenv1 }
    :envelope 0e 0e 0.001e 1e dur 0e  6 >vct :base base f2* :duration dur make-env { ampenv2 }
    :frequency 2e make-oscil { ampmod }
    90e random :locsig-degree
    start dur run-instrument
	ampmod oscil-0 0.007e f* 0.993e f+
	osc1 oscil-0 0.203e f* osc0 oscil-1 ampenv1 env f* amp f2/ 0.707e f* f*
	osc3 oscil-0 0.144e f* osc2 oscil-1 ampenv2 env f* amp f2/ f* f+ f*
    end-run
    osc0 gen-free
    osc1 gen-free
    osc2 gen-free
    osc3 gen-free
    ampenv1 gen-free
    ampenv2 gen-free
    ampmod gen-free
;instrument
' tubebell $" tubebell ( f: start f: dur f: amp f: base -- )" help!

event: tubebell-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.2e 32e tubebell
    dur wait
;event

\ from Perry Cook's Wurley.cpp
instrument: wurley { f: start f: dur f: freq f: amp -- }
    :frequency freq make-oscil { osc0 }
    :frequency freq 4e f* make-oscil { osc1 }
    :frequency 510e make-oscil { osc2 }
    :frequency 510e make-oscil { osc3 }
    :frequency 8e make-oscil { ampmod }
    :envelope 0e 0e 1e 1e 9e 1e 10e 0e  8 >vct :duration dur make-env { ampenv }
    :envelope 0e 0e 0.001e 1e 0.15e 0e dur 0e  8 >vct :duration dur make-env { indenv }
    :envelope 0e 0e 0.001e 1e 0.25e 0e dur 0e  8 >vct :duration dur make-env { resenv }
    90e random :locsig-degree
    start dur run-instrument
	ampenv env
	ampmod oscil-0 0.007e f* 1e f+ f*
	osc1 oscil-0 0.307e f* osc0 oscil-1 amp f2/ f*
	osc3 oscil-0 indenv env f* 0.117e f* osc2 oscil-1 amp f2/ 0.307e f* f*
	resenv env f* f+ f*
    end-run
    osc0 gen-free
    osc1 gen-free
    osc2 gen-free
    osc3 gen-free
    ampmod gen-free
    ampenv gen-free
    indenv gen-free
    resenv gen-free
;instrument
' wurley $" wurley ( f: start f: dur f: freq f: amp -- )" help!

event: wurley-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.2e wurley
    dur wait
;event

\ from Perry Cook's Rhodey.cpp
instrument: rhodey { f: start f: dur f: freq f: amp f: base -- }
    :frequency freq make-oscil { osc0 }
    :frequency freq make-oscil { osc1 }
    :frequency freq make-oscil { osc2 }
    :frequency freq make-oscil { osc3 }
    :envelope 0e 0e 0.005e 1e dur 0e  6 >vct :base base :duration dur make-env { ampenv1 }
    :envelope 0e 0e 0.001e 1e dur 0e  6 >vct :base base 1.5e f* :duration dur make-env { ampenv2 }
    :envelope 0e 0e 0.001e 1e 0.25e 0e  6 >vct :base base 4e f* :duration dur make-env { ampenv3 }
    90e random :locsig-degree
    start dur run-instrument
	osc1 oscil-0 0.535e f* osc0 oscil-1 ampenv1 env f* amp f2/ f*
	osc3 oscil-0 0.109e f* ampenv3 env f* osc2 oscil-1 ampenv2 env f* amp f2/ f* f+
    end-run
    osc0 gen-free
    osc1 gen-free
    osc2 gen-free
    osc3 gen-free
    ampenv1 gen-free
    ampenv2 gen-free
    ampenv3 gen-free
;instrument
' rhodey $" rhodey ( f: start f: dur f: freq f: amp f: base -- )" help!

event: rhodey-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.2e 0.5e rhodey
    dur wait
;event

\ from Perry Cook's BeeThree.cpp
instrument: hammondoid { f: start f: dur f: freq f: amp -- }
    :frequency freq 0.999e f* make-oscil { osc0 }
    :frequency freq 1.997e f* make-oscil { osc1 }
    :frequency freq 3.006e f* make-oscil { osc2 }
    :frequency freq 6.009e f* make-oscil { osc3 }
    :envelope 0e 0e 0.005e 1e dur 0.008e f- 1e dur 0e  8 >vct :duration dur make-env { ampenv1 }
    :envelope 0e 0e 0.005e 1e dur 0e  6 >vct :duration dur make-env { ampenv2 }
    90e random :locsig-degree
    start dur run-instrument
	ampenv1 env
	osc0 oscil-0 0.1875e amp f* f*
	osc1 oscil-0 0.1875e amp f* f* f+
	osc2 oscil-0 amp f2/ f* f+ f*
	ampenv2 env
	osc3 oscil-0 0.375e amp f* f* f* f+
    end-run
    osc0 gen-free
    osc1 gen-free
    osc2 gen-free
    osc3 gen-free
    ampenv1 gen-free
    ampenv2 gen-free
;instrument
' hammondoid $" hammondoid ( f: start f: dur f: freq f: amp -- )" help!

event: hammondoid-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.2e hammondoid
    dur wait
;event

\ from Perry Cook's HeavyMtl.cpp
instrument: metal { f: start f: dur f: freq f: amp -- }
    :frequency freq make-oscil { osc0 }
    :frequency freq 4e f* 0.999e f* make-oscil { osc1 }
    :frequency freq 3e f* 1.001e f* make-oscil { osc2 }
    :frequency freq 0.5e f* 1.002e f* make-oscil { osc3 }
    :envelope 0e 0e 0.001e 1e dur 0.002e f- 1e dur 0e  8 >vct :duration dur make-env { ampenv0 }
    :envelope 0e 0e 0.001e 1e dur 0.011e f- 1e dur 0e  8 >vct :duration dur make-env { ampenv1 }
    :envelope 0e 0e 0.010e 1e dur 0.015e f- 1e dur 0e  8 >vct :duration dur make-env { ampenv2 }
    :envelope 0e 0e 0.030e 1e dur 0.040e f- 1e dur 0e  8 >vct :duration dur make-env { ampenv3 }
    90e random :locsig-degree
    start dur run-instrument
	osc3 oscil-0 ampenv3 env f* 0.116e f*
	osc2 oscil-0 ampenv2 env f* 0.574e f* osc1 oscil-1 ampenv1 env f* 0.202e f* f+
	osc0 oscil-1 ampenv0 env f* 0.615e amp f* f*
    end-run
    osc0 gen-free
    osc1 gen-free
    osc2 gen-free
    osc3 gen-free
    ampenv0 gen-free
    ampenv1 gen-free
    ampenv2 gen-free
    ampenv3 gen-free
;instrument
' metal $" metal ( f: start f: dur f: freq f: amp -- )" help!

event: metal-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.2e metal
    dur wait
;event

\ DRONE
instrument: drone
    { f: start f: dur f: freq f: amp ampfun synth f: ampat f: ampdc f: rvibamt f: rvibfreq -- }
    :frequency freq :wave synth false partials>wave make-table-lookup { s }
    :envelope ampfun 25e 100e ampat dur f/ f* 75e 100e 100e ampdc dur f/ f* f- stretch-envelope
    :scaler amp 0.25e f* :duration dur make-env { ampenv }
    :frequency rvibfreq :amplitude rvibamt freq f* hz>radians make-rand { ranvib }
    90e random :locsig-degree
    start dur run-instrument 0e ranvib rand fabs s table-lookup ampenv env f* end-run
    s gen-free
    ampenv gen-free
    ranvib gen-free
;instrument
' drone $" drone ( f: start f: dur f: freq f: amp\n"
$"         ampfun synth f: ampat f: ampdc f: rvibamt f: rvibfreq -- )" $+ help!

struct
    sfloat% field atpt
    sfloat% field dcpt
    sfloat% field lfmt1
    sfloat% field harm1
    sfloat% field dev11
    sfloat% field dev01
    sfloat% field lamp1
    sfloat% field lfmt2
    sfloat% field harm2
    sfloat% field dev12
    sfloat% field dev02
    sfloat% field lamp2
    sfloat% field lfmt3
    sfloat% field harm3
    sfloat% field dev13
    sfloat% field dev03
    sfloat% field lamp3
    sfloat% field lfmt4
    sfloat% field harm4
    sfloat% field dev14
    sfloat% field dev04
    sfloat% field lamp4
    cell%   field tampfun
    cell%   field tskwfun
    cell%   field tranfun
    cell%   field tidxfun
    cell%   field modgen
    cell%   field gen1
    cell%   field gen2
    cell%   field gen3
    cell%   field gen4
    cell%   field ranvib
end-struct canter%

\ CANTER
instrument: canter { f: start f: dur f: pitch f: amp ampfun ranfun skewfun f: skewpc f: ranpc f: ranfreq indexfun f: atdr f: dcdr ampfun1 indfun1 fmtfun1 ampfun2 indfun2 fmtfun2 ampfun3 indfun3 fmtfun3 ampfun4 indfun4 fmtfun4 -- }
    canter% %alloc { ct }
    pitch 400e f/ fln 910e 400e f/ fln f/ 100e f* floor { f: k }
    100e atdr dur f/ f*                           ct atpt  sf!
    100e 100e dcdr dur f/ f* f-                   ct dcpt  sf!
    k fmtfun1 envelope-interp                     ct lfmt1 sf!
    0.5e ct lfmt1 sf@ pitch f/ f+ floor           ct harm1 sf!
    k indfun1 envelope-interp pitch f* hz>radians ct dev11 sf!
    ct dev11 sf@ f2/                              ct dev01 sf!
    k ampfun1 envelope-interp amp f* 1e ct harm1 sf@ ct lfmt1 sf@ pitch f/ f- fabs f- f*
    ct lamp1 sf!
    k fmtfun2 envelope-interp                     ct lfmt2 sf!
    0.5e ct lfmt2 sf@ pitch f/ f+ floor           ct harm2 sf!
    k indfun2 envelope-interp pitch f* hz>radians ct dev12 sf!
    ct dev12 sf@ f2/                              ct dev02 sf!
    k ampfun2 envelope-interp amp f* 1e ct harm2 sf@ ct lfmt2 sf@ pitch f/ f- fabs f- f*
    ct lamp2 sf!
    
    k fmtfun3 envelope-interp                     ct lfmt3 sf!
    0.5e ct lfmt3 sf@ pitch f/ f+ floor           ct harm3 sf!
    k indfun3 envelope-interp pitch f* hz>radians ct dev13 sf!
    ct dev13 sf@ f2/                              ct dev03 sf!
    k ampfun3 envelope-interp amp f* 1e ct harm3 sf@ ct lfmt3 sf@ pitch f/ f- fabs f- f*
    ct lamp3 sf!
    k fmtfun4 envelope-interp                     ct lfmt4 sf!
    0.5e ct lfmt4 sf@ pitch f/ f+ floor           ct harm4 sf!
    k indfun4 envelope-interp pitch f* hz>radians ct dev14 sf!
    ct dev14 sf@ f2/                              ct dev04 sf!
    k ampfun4 envelope-interp amp f* 1e ct harm4 sf@ ct lfmt4 sf@ pitch f/ f- fabs f- f*
    ct lamp4 sf!
    :envelope ampfun 25e ct atpt sf@ 75e ct dcpt sf@ stretch-envelope
    :duration dur make-env                        ct tampfun !
    :envelope skewfun 25e ct atpt sf@ 75e ct dcpt sf@ stretch-envelope :duration dur
    :scaler pitch skewpc f* hz>radians make-env   ct tskwfun !
    :envelope ranfun 25e ct atpt sf@ 75e ct dcpt sf@ stretch-envelope
    :duration dur make-env                        ct tranfun !
    :envelope indexfun 25e ct atpt sf@ 75e ct dcpt sf@ stretch-envelope
    :duration dur make-env                        ct tidxfun !
    :frequency pitch make-oscil                   ct modgen !
    :frequency pitch ct harm1 sf@ f* make-oscil   ct gen1 !
    :frequency pitch ct harm2 sf@ f* make-oscil   ct gen2 !
    :frequency pitch ct harm3 sf@ f* make-oscil   ct gen3 !
    :frequency pitch ct harm4 sf@ f* make-oscil   ct gen4 !
    :frequency ranfreq :amplitude ranpc pitch f* hz>radians make-rand  ct ranvib !
    90e random :locsig-degree
    start dur run-instrument
	ct tskwfun @ env ct tranfun @ env 0e ct ranvib @ rand f* f+ { f: frqval }
	frqval ct modgen @ oscil-1 { f: modval }
	ct tampfun @ env { f: ampval }
	ct tidxfun @ env { f: indval }
	ct dev01 sf@ indval ct dev11 sf@ f* f+ modval f* frqval f+ ct harm1 sf@ f*
	ct gen1 @ oscil-1 ct lamp1 sf@ ampval f* f*
	ct dev02 sf@ indval ct dev12 sf@ f* f+ modval f* frqval f+ ct harm2 sf@ f*
	ct gen2 @ oscil-1 ct lamp3 sf@ ampval f* f* f+
	ct dev03 sf@ indval ct dev13 sf@ f* f+ modval f* frqval f+ ct harm3 sf@ f*
	ct gen3 @ oscil-1 ct lamp3 sf@ ampval f* f* f+
	ct dev04 sf@ indval ct dev14 sf@ f* f+ modval f* frqval f+ ct harm4 sf@ f*
	ct gen4 @ oscil-1 ct lamp4 sf@ ampval f* f* f+
    end-run
    ct tampfun @ gen-free
    ct tskwfun @ gen-free
    ct tranfun @ gen-free
    ct tidxfun @ gen-free
    ct modgen @ gen-free
    ct gen1 @ gen-free
    ct gen2 @ gen-free
    ct gen3 @ gen-free
    ct gen4 @ gen-free
    ct ranvib @ gen-free
    ct free throw
;instrument
' canter $" canter ( f: start f: dur f: pitch f: amp\n"
$"          ampfun ranfun skewfun f: skewpc f: ranpc f: ranfreq indexfun f: atdr f: dcdr\n" $+
$"          ampfun1 indfun1 fmtfun1 ampfun2 indfun2 fmtfun2\n" $+
$"          ampfun3 indfun3 fmtfun3 ampfun4 indfun4 fmtfun4 -- )" $+ help!

event: drone/canter-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 1200e 100e 1000e ]     { fmt1 }
    vct[ 0e 2250e 100e 1800e ]	   { fmt2 }
    vct[ 0e 4500e 100e 4500e ]	   { fmt3 }
    vct[ 0e 6750e 100e 8100e ]	   { fmt4 }
    vct[ 0e 0.67e 100e 0.70e ]	   { amp1 }
    vct[ 0e 0.95e 100e 0.95e ]	   { amp2 }
    vct[ 0e 0.28e 100e 0.33e ]	   { amp3 }
    vct[ 0e 0.14e 100e 0.15e ]	   { amp4 }
    vct[ 0e 0.75e 100e 0.65e ]	   { ind1 }
    vct[ 0e 0.75e 100e 0.75e ]	   { ind2 }
    vct[ 0e 1e 100e 1e ]	   { ind3 }
    vct[ 0e 1e 100e 1e ]	   { ind4 }
    vct[ 0e 0e 100e 0e ]	   { skwf }
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] { ampf }
    vct[ 0e 0.5e 100e 0.5e ]	   { ranf }
    vct[ 0e 1e 100e 1e ]           { index }
    vct[ 0e 0e 5e 1e 95e 1e 100e 0e ] { solid }
    0.5e 0.06e 1e 0.62e 1.5e 0.07e 2e 0.6e 2.5e 0.08e 3e 0.56e 4e 0.24e 5e 0.98e 6e 0.53e
    7e 0.16e 8e 0.33e 9e 0.62e 10e 0.12e 12e 0.14e 14e 0.86e 16e 0.12e 23e 0.14e 24e 0.17e
    36 >vct { bassdr2 }
    0.3e 0.04e 1e 0.81e 2e 0.27e 3e 0.2e 4e 0.21e 5e 0.18e 6e 0.35e 7e 0.03e
    8e 0.07e 9e 0.02e 10e 0.025e 11e 0.035e  24 >vct { tenordr }

    start 4e 115.0e 0.125e solid bassdr2 0.1e 0.5e 0.01e 10e drone
    start 4e 229.0e 0.125e solid tenordr 0.1e 0.5e 0.01e 11e drone
    start 4e 229.5e 0.125e solid tenordr 0.1e 0.5e 0.01e 09e drone
    start 2.100e 918.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 2.100e f+ 0.300e 688.500e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 2.400e f+ 0.040e 826.200e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 2.440e f+ 0.560e 459.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.000e f+ 0.040e 408.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.040e f+ 0.040e 619.650e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.080e f+ 0.040e 408.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.120e f+ 0.040e 688.500e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.160e f+ 0.290e 459.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.450e f+ 0.150e 516.375e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.600e f+ 0.040e 826.200e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.640e f+ 0.040e 573.750e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.680e f+ 0.040e 619.650e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.720e f+ 0.180e 573.750e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.900e f+ 0.040e 688.500e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
    start 3.940e f+ 0.260e 459.000e 0.175e ampf ranf skwf 0.050e 0.01e 10e index 0.005e 0.005e
    amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter

    fmt1 gen-free
    fmt2 gen-free
    fmt3 gen-free
    fmt4 gen-free
    amp1 gen-free
    amp2 gen-free
    amp3 gen-free
    amp4 gen-free
    ind1 gen-free
    ind2 gen-free
    ind3 gen-free
    ind4 gen-free
    skwf gen-free
    ampf gen-free
    ranf gen-free
    index gen-free
    solid gen-free
    bassdr2 gen-free
    tenordr gen-free
    5e wait
;event

\ NREV (the most popular Samson box reverb)
$" reverb-factor" dup constant :reverb-factor float-keyword!
$" lp-coeff"      dup constant :lp-coeff      float-keyword!
$" lp-out-coeff"  dup constant :lp-out-coeff  float-keyword!
$" output-scale"  dup constant :output-scale  float-keyword!

\ REVERB-FACTOR controls the length of the decay -- it should not
\ exceed (/ 1.0 .823), LP-COEFF controls the strength of the low pass
\ filter inserted in the feedback loop, VOLUME can be used to boost the
\ reverb output.
: nrev-args ( keyword-args -- args )
    make-hash { args }
    1.09e :reverb-factor args set-fargs
    0.70e :lp-coeff      args set-fargs
    0.85e :lp-out-coeff  args set-fargs
    1.00e :output-scale  args set-fargs
    1.00e :volume        args set-fargs
    nil   :amp-env       args set-args 
    args
;

array[ 1433 1601 1867 2053 2251 2399 347 113 37 59 43 37 29 19 ] constant dly-len
so-lib-exists? sndins-nr libsndins.so [if]
    \ sndins/sndins.c
    sndins-nr clm-nrev sf sf sf sf sf sf sf ptr int ptr ptr (llong) ins_nrev
    : main-nrev { f: start f: dur chans rev-chans args -- }
	start dur
	:reverb-factor args fhash@
	:lp-coeff      args fhash@
	:lp-out-coeff  args fhash@
	:output-scale  args fhash@
	:volume        args fhash@
	:amp-env       args hash@ { amp-env }
	amp-env if
	    false
	else
	    vct[ 0e 1e 1e 1e ] to amp-env
	    true
	then { amp-del }
	amp-env data-ptr
	amp-env length
	*output*
	*reverb* clm-nrev 2drop
	amp-del if amp-env gen-free then
    ;
[else]
    \ clm-3/nrev.ins
    : main-nrev { f: start f: dur chans rev-chans args -- }
	dly-len each
	    s>f mus-srate@ 25641e f/ f* floor f>s { val }
	    val 2 mod unless val 1+ to val then
	    begin val prime 0= while val 2 + to val repeat
	    val i dly-len array!
	end-each
	:reverb-factor args fhash@ { f: rf }
	:scaler 0.822e rf f* :size 0 dly-len array@ make-comb
	:scaler 0.802e rf f* :size 1 dly-len array@ make-comb
	:scaler 0.773e rf f* :size 2 dly-len array@ make-comb
	:scaler 0.753e rf f* :size 3 dly-len array@ make-comb
	:scaler 0.753e rf f* :size 4 dly-len array@ make-comb
	:scaler 0.733e rf f* :size 5 dly-len array@ make-comb 6 >array { combs }
	:feedback -0.7e :feedforward 0.7e :size 6  dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 7  dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 8  dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 9  dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 10 dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 11 dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 12 dly-len array@ make-all-pass
	:feedback -0.7e :feedforward 0.7e :size 13 dly-len array@ make-all-pass 8 >array { apass }
	:lp-coeff args fhash@ { f: lpc }
	:lp-out-coeff args fhash@ { f: lpoc }
	lpc lpc 1e f- make-one-pole { low }
	lpoc lpc 1e f- make-one-pole { low-a }
	lpoc lpc 1e f- make-one-pole { low-b }
	lpoc lpc 1e f- make-one-pole { low-c }
	lpoc lpc 1e f- make-one-pole { low-d }
	:output-scale args fhash@ { f: out-scale }
	:amp-env args hash@ { amp-env }
	amp-env if
	    false
	else
	    vct[ 0e 1e 1e 1e ] to amp-env
	    true
	then { amp-del }
	:envelope amp-env :scaler out-scale :duration dur make-env { ampf }
	:volume args fhash@ { f: volume }
	start dur run
	    0e rev-chans 0 do j i *reverb* in-any f+ loop volume f* ampf env f* { f: rev }
	    0e combs each rev comb-1 f+ end-each
	    0 apass array@ all-pass-1
	    1 apass array@ all-pass-1
	    2 apass array@ all-pass-1
	    low one-pole
	    3 apass array@ all-pass-1 { f: outrev }
	    outrev 4 apass array@ all-pass-1 low-a one-pole out-scale f* { f: sample-a }
	    outrev 5 apass array@ all-pass-1 low-a one-pole out-scale f* { f: sample-b }
	    outrev 6 apass array@ all-pass-1 low-a one-pole out-scale f* { f: sample-c }
	    outrev 7 apass array@ all-pass-1 low-a one-pole out-scale f* { f: sample-d }
	    chans 2 = if
		sample-a sample-d f+ f2/ i *output* outa
	    else
		sample-a i *output* outa
	    then
	    chans 2 = chans 4 = or if
		chans 2 = if
		    sample-b sample-c f+ f2/ i *output* outb
		else
		    sample-b i *output* outb
		then
	    then
	    chans 4 = if
		sample-c i *output* outc
		sample-d i *output* outd
	    then
	loop
	apass gen-free
	combs gen-free
	low gen-free
	low-a gen-free
	low-b gen-free
	low-c gen-free
	low-d gen-free
	ampf gen-free
	amp-del if amp-env gen-free then
    ;
[then]
$" nrev" constant str-nrev
instrument: nrev { f: start f: dur args -- }
    args unless nrev-args to args then
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    *verbose* if rev-chans chans str-nrev reverb-info then
    start dur chans rev-chans args main-nrev
;instrument
' nrev $" nrev ( f: start f: dur args -- )"
$" \\ NREV-ARGS (see there) takes this keywords\n" $+
$" \\ :reverb-factor -- 1.09e\n" $+
$" \\ :lp-coeff      -- 0.7e\n" $+
$" \\ :lp-out-coeff  -- 0.85e\n" $+
$" \\ :output-scale  -- 1e\n" $+
$" \\ :volume        -- 1e\n" $+
$" \\ :amp-env       -- vct[ 0e 1e 1e 1e ]\n" $+
$" ' fm-violin-test :reverb ' nrev with-sound" $+ help!
' nrev-args $" nrev-args ( keyword-args -- args )\n"
$" \\ keywords and default values\n" $+
$" \\ :reverb-factor -- 1.09e\n" $+
$" \\ :lp-coeff      -- 0.7e\n" $+
$" \\ :lp-out-coeff  -- 0.85e\n" $+
$" \\ :output-scale  -- 1e\n" $+
$" \\ :volume        -- 1e\n" $+
$" \\ :amp-env       -- vct[ 0e 1e 1e 1e ]\n" $+
$" ' fm-violin-test\n" $+
$" :reverb-data :volume 0.8e nrev-args\n" $+
$" :reverb ' nrev :channels 2 with-sound" $+ help!

struct
    cell%   field carriers
    cell%   field ampfs
    cell%   field indfs
    sfloat% field c-rats
end-struct reson%

\ RESON
instrument: reson { f: start f: dur f: pitch f: amp indxfun skewfun f: pcskew f: skewat  f: skewdc f: vibfreq f: vibpc f: ranvibfreq f: ranvibpc data -- }
    :frequency pitch make-oscil { mod }
    :envelope skewfun 25e 100e skewat dur f/ f* 75e 100e 100e skewdc dur f/ f* f- stretch-envelope
    :scaler pcskew pitch f* hz>radians :duration dur make-env { frqf }
    :frequency vibfreq :amplitude vibpc pitch f* hz>radians make-triangle-wave { pervib }
    :frequency ranvibfreq :amplitude ranvibpc pitch f* hz>radians make-rand-interp { ranvib }
    0e data each 1 tuck swap array@ vct@ f+ end-each { f: totalamp }
    nil { rs }
    data length make-array map
	reson% %alloc to rs
	i data array@ { frmdat }
	1 frmdat array@ { vals }
	:envelope indxfun 25e 100e 6 vals vct@ dur f/ f* 75e 100e 100e 7 vals vct@ dur f/ f* f-
	stretch-envelope
	:scaler 5 vals vct@ 0 vals vct@ f* hz>radians 4 vals vct@ 0 vals vct@ f* hz>radians f-
	:offset 4 vals vct@ 0 vals vct@ f* hz>radians :duration dur make-env   rs indfs !
	:envelope
	0 frmdat array@ 25e 100e 2 vals vct@ dur f/ f* 75e 100e 100e 3 vals vct@ dur f/ f* f-
	stretch-envelope
	:scaler 1e 0 vals vct@ pitch f/ fround 0 vals vct@ pitch f/ f- f- fabs amp f*
	1 vals vct@ totalamp f/ f*
	:duration dur make-env   rs ampfs !
	0 vals vct@ pitch f/ fround   rs c-rats sf!
	:frequency pitch 0 vals vct@ pitch f/ fround f* make-oscil   rs carriers !
	rs
    end-map { values }
    90e random :locsig-degree
    start dur run-instrument
	0e pervib triangle-wave 0e ranvib rand-interp f+ frqf env f+ { f: vib }
	vib mod oscil-1 { f: modsig }
	0e
	values each to rs
	    rs ampfs @ env
	    vib rs c-rats sf@ f* rs indfs @ env modsig f* f+ rs carriers @ oscil-1 f* f+
	end-each
    end-run
    mod gen-free
    frqf gen-free
    pervib gen-free
    ranvib gen-free
    values gen-free
;instrument
' reson $" reson ( f: start f: dur f: pitch f: amp indxfun skewfun\n"
$"         f: pcskew f: skewat  f: skewdc\n" $+
$"         f: vibfreq f: vibpc f: ranvibfreq f: ranvibpc data -- )" $+ help!

event: reson-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 100e 1e ] vct[ 1200e 0.5e 0.1e 0.1e 0e 1.0e 0.1e 0.1e ] 2 >array
    vct[ 0e 1e 100e 0e ] vct[ 2400e 0.5e 0.1e 0.1e 0e 1.0e 0.1e 0.1e ] 2 >array 2 >array { data }

    start dur 440e 0.5e vct[ 0e 0e 100e 1e ] vct[ 0e 0e 100e 1e ]
    0.1e 0.1e 0.1e 5e 0.01e 5e 0.01e data reson
    data gen-free
    dur wait
;event

struct
    cell%   field car
    cell%   field low
    cell%   field fmosc
    cell%   field pvib
    cell%   field rvib
    cell%   field pvibenv
    cell%   field rvibenv
    cell%   field glisenv
    cell%   field amplenv
    cell%   field betaenv
    sfloat% field ampap
    sfloat% field ampdp
    sfloat% field glsap
    sfloat% field glsdp
    sfloat% field betap
    sfloat% field betdp
    sfloat% field pvbap
    sfloat% field pvbdp
end-struct cellon%

\ STK's feedback-fm instrument named CelloN in Sambox-land
instrument: cellon { f: start f: dur f: pitch0 f: amp ampfun betafun f: beta0 f: beta1 f: betaat f: betadc f: ampat f: ampdc f: pitch1 glissfun f: glissat f: glissdc f: pvibfreq f: pvibpc pvibfun f: pvibat f: pvibdc f: rvibfreq f: rvibpc rvibfun -- }
    cellon% %alloc { cl }
    pitch1 f0= if pitch0 else pitch1 then { f: pit1 }
    :frequency pitch0 make-oscil              cl car !
    0.5e -0.5e make-one-zero                  cl low !
    :frequency pitch0 make-oscil              cl fmosc !
    :frequency pvibfreq make-triangle-wave    cl pvib !
    :frequency rvibfreq make-rand-interp      cl rvib !
    ampat f0> if 100e ampat dur f/ f* else 25e then           cl ampap sf!
    ampdc f0> if 100e 1e ampdc dur f/ f- f* else 75e then     cl ampdp sf!
    glissat f0> if 100e glissat dur f/ f* else 25e then       cl glsap sf!
    glissdc f0> if 100e 1e glissdc dur f/ f- f* else 75e then cl glsdp sf!
    betaat f0> if 100e betaat dur f/ f* else 25e then         cl betap sf!
    betadc f0> if 100e 1e betadc dur f/ f- f* else 75e then   cl betdp sf!
    pvibat f0> if 100e pvibat dur f/ f* else 25e then         cl pvbap sf!
    pvibdc f0> if 100e 1e pvibdc dur f/ f- f* else 75e then   cl pvbdp sf!
    :envelope pvibfun 25e cl pvbap sf@ 75e cl pvbdp sf@ stretch-envelope
    :scaler pvibpc pitch0 f* hz>radians :duration dur make-env   cl pvibenv !
    :envelope rvibfun 0e 0e 0e 0e stretch-envelope :duration dur
    :scaler rvibpc pitch0 f* hz>radians make-env                 cl rvibenv !
    :envelope glissfun 25e cl glsap sf@ 75e cl glsdp sf@ stretch-envelope
    :scaler pit1 pitch0 f- :duration dur make-env                cl glisenv !
    :envelope ampfun 25e cl ampap sf@ 75e cl ampdp sf@ stretch-envelope
    :scaler amp :duration dur make-env                           cl amplenv !
    :envelope betafun 25e cl betap sf@ 75e cl betdp sf@ stretch-envelope
    :scaler beta1 beta0 f- :offset beta0 :duration dur make-env  cl betaenv !
    90e random :locsig-degree
    0e { f: fm }
    start dur run-instrument
	cl pvibenv @ env 0e cl pvib @ triangle-wave f*
	cl rvibenv @ env 0e cl rvib @ rand-interp f* f+ cl glisenv @ env f+ ( vib )
	cl betaenv @ env fover fm f+ cl fmosc @ oscil-1 f* cl low @ one-zero to fm
	cl amplenv @ env fswap fm f+ cl car @ oscil-1 f*
    end-run
    cl car @ gen-free
    cl low @ gen-free
    cl fmosc @ gen-free
    cl pvib @ gen-free
    cl rvib @ gen-free
    cl pvibenv @ gen-free
    cl rvibenv @ gen-free
    cl glisenv @ gen-free
    cl amplenv @ gen-free
    cl betaenv @ gen-free
    cl free throw
;instrument
' cellon $" cellon ( f: start f: dur f: pitch0 f: amp\n"
$"          ampfun betafun f: beta0 f: beta1 f: betaat f: betadc\n" $+
$"          f: ampat f: ampdc f: pitch1 glissfun f: glissat f: glissdc\n" $+
$"          f: pvibfreq f: pvibpc pvibfun f: pvibat f: pvibdc f: rvibfreq\n" $+
$"          f: pvibpc rvibfun -- )" $+ help!

event: cellon-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] { env1 }
    vct[ 0e 0e 100e 0e ] { env2 }
    
    start dur 220e 0.5e env1 env1 0.75e 1e 0e 0e 0e 0e 220e env1 0e 0e 0e 0e env2 0e 0e 0e 0e env2
    cellon
    env1 gen-free
    env2 gen-free
    dur wait
;event

\ JL-REVERB
$" jl-reverb" constant str-jl-reverb
: jl-reverb-args ( keyword-args -- args ) make-hash ;
instrument: jl-reverb { f: start f: dur args -- }
    :feedback -0.7e :feedforward 0.7e :size 2111 make-all-pass { allpass1 }
    :feedback -0.7e :feedforward 0.7e :size  673 make-all-pass { allpass2 }
    :feedback -0.7e :feedforward 0.7e :size  223 make-all-pass { allpass3 }
    :scaler 0.742e :size  9601 make-comb { comb1 }
    :scaler 0.733e :size 10007 make-comb { comb2 }
    :scaler 0.715e :size 10799 make-comb { comb3 }
    :scaler 0.697e :size 11597 make-comb { comb4 }
    *output* channels@ { chans }
    *reverb* channels@ { rev-chans }
    :size 0.013e seconds>samples make-delay { outdel1 }
    chans 1 > if :size 0.011e seconds>samples make-delay else nil then { outdel2 }
    chans 2 > if :size 0.015e seconds>samples make-delay else nil then { outdel3 }
    chans 3 > if :size 0.017e seconds>samples make-delay else nil then { outdel4 }
    *verbose* if rev-chans chans str-jl-reverb reverb-info then
    0e { f: allpass-sum }
    0e { f: all-sums }
    start dur run
	0e rev-chans 0 do j i *reverb* in-any f+ loop
	allpass1 all-pass-1  allpass2 all-pass-1  allpass3 all-pass-1 to allpass-sum
	allpass-sum comb1 comb-1
	allpass-sum comb2 comb-1 f+
	allpass-sum comb3 comb-1 f+
	allpass-sum comb4 comb-1 f+ to all-sums
	all-sums outdel1 delay-1 i *output*  outa
	outdel2 if all-sums outdel2 delay-1 i *output* outb then
	outdel3 if all-sums outdel3 delay-1 i *output* outc then
	outdel4 if all-sums outdel4 delay-1 i *output* outd then
    loop
    allpass1 gen-free
    allpass2 gen-free
    allpass3 gen-free
    comb1 gen-free
    comb2 gen-free
    comb3 gen-free
    comb4 gen-free
    outdel1 gen-free
    outdel2 ?dup-if gen-free then
    outdel3 ?dup-if gen-free then
    outdel4 ?dup-if gen-free then
;instrument
' jl-reverb $" jl-reverb ( f: start f: dur args -- ) \\ reverberator instrument" help!

\ GRAN-SYNTH
instrument: gran-synth { f: start f: dur f: freq f: grain-dur f: interval f: amp -- }
    vct[ 0e 0e 25e 1e 75e 1e 100e 0e ] { env1 }
    :envelope env1 :duration grain-dur make-env { grain-env }
    :frequency freq make-oscil { car }
    grain-dur interval fmax mus-srate@ f* fceil f>s make-vct map
	grain-env env 0e 0e car oscil f*
    end-map { data }
    grain-env gen-free
    car gen-free
    :frequency interval 1/f :wave data make-wave-train { grains }
    :envelope env1 :duration dur :scaler amp make-env { ampf }
    90e random :locsig-degree
    start dur run-instrument grains wave-train-1 ampf env f* end-run
    ampf gen-free
    grains gen-free
    env1 gen-free
;instrument
' gran-synth $" gran-synth ( f: start f: dur f: freq f: grain-dur f: interval f: amp -- )" help!

event: gran-synth-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 220e 0.0189e 0.02e 0.4e gran-synth
    dur wait
;event

\ TOUCH-TONE
\ 
\ clm-3/ugex.ins
instrument: touch-tone ( numbers keyword-args -- )
    0e :beg get-fargs { f: start }
    { numbers }
    vct[ 0e 697e 697e 697e 770e 770e 770e 852e 852e 852e 941e 941e 941e ] { tt1 }
    vct[ 0e 1209e 1336e 1477e 1209e 1336e 1477e 1209e 1336e 1477e 1209e 1336e 1477e ] { tt2 }
    90e random :locsig-degree
    numbers each
	?dup-if else 11 then { idx }
	:frequency idx tt1 vct@ make-oscil { frq1 }
	:frequency idx tt2 vct@ make-oscil { frq2 }
	90e random :locsig-degree
	i s>f 0.3e f* start f+ 0.2e run-instrument 0.25e frq1 oscil-0 frq2 oscil-0 f+ f* end-run
	frq1 gen-free
	frq2 gen-free
    end-each
    tt1 gen-free
    tt2 gen-free
;instrument
' touch-tone $" touch-tone ( numbers keyword-arg -- ) (see clm-3/ugex.ins)\n"
$" \\ keyword and default value\n" $+
$" \\ :beg -- 0e\n" $+
$" \\ NUMBERS is an Array with phone numbers\n" $+
$" array[ 8 5 7 7 5 8 ] ' touch-tone with-sound" $+ help!

event: touch-tone-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    array[ 8 5 7 7 5 8 ] :beg start touch-tone
    dur wait
;event

\ SPECTRA
instrument: spectra { f: start f: dur f: freq f: amp parts ampenv f: vibamp f: vibfrq -- }
    :frequency freq :wave parts false partials>wave make-table-lookup { s }
    :envelope ampenv :scaler amp :duration dur make-env { ampf }
    freq hz>radians vibamp f* { f: vamp }
    :frequency vibfrq :amplitude vamp make-triangle-wave { pervib }
    :frequency vibfrq 1e f+ :amplitude vamp make-rand-interp { ranvib }
    90e random :locsig-degree
    start dur run-instrument
	0e pervib triangle-wave 0e ranvib rand-interp f+ s table-lookup ampf env f*
    end-run
    s gen-free
    ampf gen-free
    pervib gen-free
    ranvib gen-free
;instrument
' spectra $" spectra ( f: start f: dur f: freq f: amp parts ampenv f: vibamp f: vibfrq -- )" help!

event: spectra-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    vct[ 0e 0e 1e 1e 5e 0.9e 12e 0.5e 25e 0.25e 100e 0e ] { ampenv }
    
    start dur 440e 2e p-a4 ampenv 0.005e 5e spectra
    ampenv gen-free
    dur wait
;event

\ TWO-TAB
\
\ interpolate between two waveforms (this could be extended to
\ implement all the various wavetable-based synthesis techniques).
instrument: two-tab { f: start f: dur f: freq f: amp part1 part2 ampenv interpenv f: vibamp f: vibfrq -- }
    :frequency freq :wave part1 false partials>wave make-table-lookup { s1 }
    :frequency freq :wave part2 false partials>wave make-table-lookup { s2 }
    :envelope ampenv :scaler amp :duration dur make-env { ampf }
    :envelope interpenv :duration dur make-env { interpf }
    freq hz>radians vibamp f* { f: vamp }
    :frequency vibfrq :amplitude vamp make-triangle-wave { pervib }
    :frequency vibfrq 1e f+ :amplitude vamp make-rand-interp { ranvib }
    90e random :locsig-degree
    start dur run-instrument
	0e pervib triangle-wave 0e ranvib rand-interp f+ { f: vib }
	interpf env { f: intrp }
	vib s1 table-lookup intrp f* vib s2 table-lookup 1e intrp f- f* f+ ampf env f*
    end-run
    s1 gen-free
    s2 gen-free
    ampf gen-free
    interpf gen-free
    pervib gen-free
    ranvib gen-free
;instrument
' two-tab $" two-tab ( f: start f: dur f: freq f: amp\n"
$"           part1 part2 ampenv interpfun f: vibamp f: vibfrq -- )" $+ help!

event: two-tab-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.5e vct[ 1e 1e 2e 0.5e ] vct[ 1e 0e 3e 1e ] vct[ 0e 0e 50e 1e 100e 0e ]
    vct[ 0e 1e 100e 0e ] 0.005e 5e two-tab
    dur wait
;event

\ LBJ-PIANO
0.04e fvalue *clm-piano-attack-duration*
0.2e  fvalue *clm-piano-realease-duration*
-10e  fvalue *clm-db-drop-per-second*

vct[ 1.97e 0.0326e 2.99e 0.0086e 3.95e 0.0163e 4.97e 0.0178e 5.98e 0.0177e 6.95e 0.0315e 8.02e 0.0001e 8.94e 0.0076e  9.96e 0.0134e 10.99e 0.0284e 11.98e 0.0229e 13.02e 0.0229e 13.89e 0.0010e 15.06e 0.0090e 16.00e 0.0003e 17.08e 0.0078e 18.16e 0.0064e 19.18e 0.0129e 20.21e 0.0085e 21.27e 0.0225e 22.32e 0.0061e 23.41e 0.0102e 24.48e 0.0005e 25.56e 0.0016e 26.64e 0.0018e 27.70e 0.0113e 28.80e 0.0111e 29.91e 0.0158e 31.06e 0.0093e 32.17e 0.0017e 33.32e 0.0002e 34.42e 0.0018e 35.59e 0.0027e 36.74e 0.0055e 37.90e 0.0037e 39.06e 0.0064e 40.25e 0.0033e 41.47e 0.0014e 42.53e 0.0004e 43.89e 0.0010e 45.12e 0.0039e 46.33e 0.0039e 47.64e 0.0009e 48.88e 0.0016e 50.13e 0.0006e 51.37e 0.0010e 52.70e 0.0002e 54.00e 0.0004e 55.30e 0.0008e 56.60e 0.0025e 57.96e 0.0010e 59.30e 0.0012e 60.67e 0.0011e 61.99e 0.0003e 62.86e 0.0001e 64.36e 0.0005e 64.86e 0.0001e 66.26e 0.0004e 67.70e 0.0006e 68.94e 0.0002e 70.10e 0.0001e 70.58e 0.0002e 72.01e 0.0007e 73.53e 0.0006e 75.00e 0.0002e 77.03e 0.0005e 78.00e 0.0002e 79.57e 0.0006e 81.16e 0.0005e 82.70e 0.0005e 84.22e 0.0003e 85.41e 0.0002e 87.46e 0.0001e 90.30e 0.0001e 94.02e 0.0001e 95.26e 0.0002e 109.39e 0.0003e ]
vct[ 1.98e 0.0194e 2.99e 0.0210e 3.97e 0.0276e 4.96e 0.0297e 5.96e 0.0158e 6.99e 0.0207e 8.01e 0.0009e 9.00e 0.0101e 10.00e 0.0297e 11.01e 0.0289e 12.02e 0.0211e 13.04e 0.0127e 14.07e 0.0061e 15.08e 0.0174e 16.13e 0.0009e 17.12e 0.0093e 18.16e 0.0117e 19.21e 0.0122e 20.29e 0.0108e 21.30e 0.0077e 22.38e 0.0132e 23.46e 0.0073e 24.14e 0.0002e 25.58e 0.0026e 26.69e 0.0035e 27.77e 0.0053e 28.88e 0.0024e 30.08e 0.0027e 31.13e 0.0075e 32.24e 0.0027e 33.36e 0.0004e 34.42e 0.0004e 35.64e 0.0019e 36.78e 0.0037e 38.10e 0.0009e 39.11e 0.0027e 40.32e 0.0010e 41.51e 0.0013e 42.66e 0.0019e 43.87e 0.0007e 45.13e 0.0017e 46.35e 0.0019e 47.65e 0.0021e 48.89e 0.0014e 50.18e 0.0023e 51.42e 0.0015e 52.73e 0.0002e 54.00e 0.0005e 55.34e 0.0006e 56.60e 0.0010e 57.96e 0.0016e 58.86e 0.0005e 59.30e 0.0004e 60.75e 0.0005e 62.22e 0.0003e 63.55e 0.0005e 64.82e 0.0003e 66.24e 0.0003e 67.63e 0.0011e 69.09e 0.0007e 70.52e 0.0004e 72.00e 0.0005e 73.50e 0.0008e 74.95e 0.0003e 77.13e 0.0013e 78.02e 0.0002e 79.48e 0.0004e 82.59e 0.0004e 84.10e 0.0003e ]
vct[ 2.00e 0.0313e 2.99e 0.0109e 4.00e 0.0215e 5.00e 0.0242e 5.98e 0.0355e 7.01e 0.0132e 8.01e 0.0009e 9.01e 0.0071e 10.00e 0.0258e 11.03e 0.0221e 12.02e 0.0056e 13.06e 0.0196e 14.05e 0.0160e 15.11e 0.0107e 16.11e 0.0003e 17.14e 0.0111e 18.21e 0.0085e 19.23e 0.0010e 20.28e 0.0048e 21.31e 0.0128e 22.36e 0.0051e 23.41e 0.0041e 24.05e 0.0006e 25.54e 0.0019e 26.62e 0.0028e 27.72e 0.0034e 28.82e 0.0062e 29.89e 0.0039e 30.98e 0.0058e 32.08e 0.0011e 33.21e 0.0002e 34.37e 0.0008e 35.46e 0.0018e 36.62e 0.0036e 37.77e 0.0018e 38.92e 0.0042e 40.07e 0.0037e 41.23e 0.0011e 42.67e 0.0003e 43.65e 0.0018e 44.68e 0.0025e 45.99e 0.0044e 47.21e 0.0051e 48.40e 0.0044e 49.67e 0.0005e 50.88e 0.0019e 52.15e 0.0003e 53.42e 0.0008e 54.69e 0.0010e 55.98e 0.0005e 57.26e 0.0013e 58.53e 0.0027e 59.83e 0.0011e 61.21e 0.0027e 62.54e 0.0003e 63.78e 0.0003e 65.20e 0.0001e 66.60e 0.0006e 67.98e 0.0008e 69.37e 0.0019e 70.73e 0.0007e 72.14e 0.0004e 73.62e 0.0002e 74.40e 0.0003e 76.52e 0.0006e 77.97e 0.0002e 79.49e 0.0004e 80.77e 0.0003e 81.00e 0.0001e 82.47e 0.0005e 83.97e 0.0001e 87.27e 0.0002e ]
vct[ 2.00e 0.0257e 2.99e 0.0142e 3.97e 0.0202e 4.95e 0.0148e 5.95e 0.0420e 6.95e 0.0037e 7.94e 0.0004e 8.94e 0.0172e 9.95e 0.0191e 10.96e 0.0115e 11.97e 0.0059e 12.98e 0.0140e 14.00e 0.0178e 15.03e 0.0121e 16.09e 0.0002e 17.07e 0.0066e 18.08e 0.0033e 19.15e 0.0022e 20.18e 0.0057e 21.22e 0.0077e 22.29e 0.0037e 23.33e 0.0066e 24.97e 0.0002e 25.49e 0.0019e 26.55e 0.0042e 27.61e 0.0043e 28.73e 0.0038e 29.81e 0.0084e 30.91e 0.0040e 32.03e 0.0025e 33.14e 0.0005e 34.26e 0.0003e 35.38e 0.0019e 36.56e 0.0037e 37.68e 0.0049e 38.86e 0.0036e 40.11e 0.0011e 41.28e 0.0008e 42.50e 0.0004e 43.60e 0.0002e 44.74e 0.0022e 45.99e 0.0050e 47.20e 0.0009e 48.40e 0.0036e 49.68e 0.0004e 50.92e 0.0009e 52.17e 0.0005e 53.46e 0.0007e 54.76e 0.0006e 56.06e 0.0005e 57.34e 0.0011e 58.67e 0.0005e 59.95e 0.0015e 61.37e 0.0008e 62.72e 0.0004e 65.42e 0.0009e 66.96e 0.0003e 68.18e 0.0003e 69.78e 0.0003e 71.21e 0.0004e 72.45e 0.0002e 74.22e 0.0003e 75.44e 0.0001e 76.53e 0.0003e 78.31e 0.0004e 79.83e 0.0003e 80.16e 0.0001e 81.33e 0.0003e 82.44e 0.0001e 83.17e 0.0002e 84.81e 0.0003e 85.97e 0.0003e 89.08e 0.0001e 90.70e 0.0002e 92.30e 0.0002e 95.59e 0.0002e 97.22e 0.0003e 98.86e 0.0001e 108.37e 0.0001e 125.54e 0.0001e ]
vct[ 1.99e 0.0650e 3.03e 0.0040e 4.03e 0.0059e 5.02e 0.0090e 5.97e 0.0227e 6.98e 0.0050e 8.04e 0.0020e 9.00e 0.0082e 9.96e 0.0078e 11.01e 0.0056e 12.01e 0.0095e 13.02e 0.0050e 14.04e 0.0093e 15.08e 0.0064e 16.14e 0.0017e 17.06e 0.0020e 18.10e 0.0025e 19.14e 0.0023e 20.18e 0.0015e 21.24e 0.0032e 22.29e 0.0029e 23.32e 0.0014e 24.37e 0.0005e 25.43e 0.0030e 26.50e 0.0022e 27.60e 0.0027e 28.64e 0.0024e 29.76e 0.0035e 30.81e 0.0136e 31.96e 0.0025e 33.02e 0.0003e 34.13e 0.0005e 35.25e 0.0007e 36.40e 0.0014e 37.51e 0.0020e 38.64e 0.0012e 39.80e 0.0019e 40.97e 0.0004e 42.09e 0.0003e 43.24e 0.0003e 44.48e 0.0002e 45.65e 0.0024e 46.86e 0.0005e 48.07e 0.0013e 49.27e 0.0008e 50.49e 0.0006e 52.95e 0.0001e 54.23e 0.0005e 55.45e 0.0004e 56.73e 0.0001e 58.03e 0.0003e 59.29e 0.0002e 60.59e 0.0003e 62.04e 0.0002e 65.89e 0.0002e 67.23e 0.0002e 68.61e 0.0002e 69.97e 0.0004e 71.36e 0.0005e 85.42e 0.0001e ]
vct[ 1.98e 0.0256e 2.96e 0.0158e 3.95e 0.0310e 4.94e 0.0411e 5.95e 0.0238e 6.94e 0.0152e 7.93e 0.0011e 8.95e 0.0185e 9.92e 0.0166e 10.93e 0.0306e 11.94e 0.0258e 12.96e 0.0202e 13.97e 0.0403e 14.95e 0.0228e 15.93e 0.0005e 17.01e 0.0072e 18.02e 0.0034e 19.06e 0.0028e 20.08e 0.0124e 21.13e 0.0137e 22.16e 0.0102e 23.19e 0.0058e 23.90e 0.0013e 25.30e 0.0039e 26.36e 0.0039e 27.41e 0.0025e 28.47e 0.0071e 29.64e 0.0031e 30.60e 0.0027e 31.71e 0.0021e 32.84e 0.0003e 33.82e 0.0002e 35.07e 0.0019e 36.09e 0.0054e 37.20e 0.0038e 38.33e 0.0024e 39.47e 0.0055e 40.55e 0.0016e 41.77e 0.0006e 42.95e 0.0002e 43.27e 0.0018e 44.03e 0.0006e 45.25e 0.0019e 46.36e 0.0033e 47.50e 0.0024e 48.87e 0.0012e 50.03e 0.0016e 51.09e 0.0004e 53.52e 0.0017e 54.74e 0.0012e 56.17e 0.0003e 57.40e 0.0011e 58.42e 0.0020e 59.70e 0.0007e 61.29e 0.0008e 62.56e 0.0003e 63.48e 0.0002e 64.83e 0.0002e 66.12e 0.0012e 67.46e 0.0017e 68.81e 0.0003e 69.13e 0.0003e 70.53e 0.0002e 71.84e 0.0001e 73.28e 0.0002e 75.52e 0.0010e 76.96e 0.0005e 77.93e 0.0003e 78.32e 0.0003e 79.73e 0.0003e 81.69e 0.0002e 82.52e 0.0001e 84.01e 0.0001e 84.61e 0.0002e 86.88e 0.0001e 88.36e 0.0002e 89.85e 0.0002e 91.35e 0.0003e 92.86e 0.0002e 93.40e 0.0001e 105.28e 0.0002e 106.22e 0.0002e 107.45e 0.0001e 108.70e 0.0003e 122.08e 0.0002e ]
vct[ 1.97e 0.0264e 2.97e 0.0211e 3.98e 0.0234e 4.98e 0.0307e 5.96e 0.0085e 6.94e 0.0140e 7.93e 0.0005e 8.96e 0.0112e 9.96e 0.0209e 10.98e 0.0194e 11.98e 0.0154e 12.99e 0.0274e 13.99e 0.0127e 15.01e 0.0101e 15.99e 0.0002e 17.04e 0.0011e 18.08e 0.0032e 19.14e 0.0028e 20.12e 0.0054e 21.20e 0.0053e 22.13e 0.0028e 23.22e 0.0030e 24.32e 0.0006e 25.24e 0.0004e 26.43e 0.0028e 27.53e 0.0048e 28.52e 0.0039e 29.54e 0.0047e 30.73e 0.0044e 31.82e 0.0007e 32.94e 0.0008e 34.04e 0.0012e 35.13e 0.0018e 36.29e 0.0007e 37.35e 0.0075e 38.51e 0.0045e 39.66e 0.0014e 40.90e 0.0004e 41.90e 0.0002e 43.08e 0.0002e 44.24e 0.0017e 45.36e 0.0013e 46.68e 0.0020e 47.79e 0.0015e 48.98e 0.0010e 50.21e 0.0012e 51.34e 0.0001e 53.82e 0.0003e 55.09e 0.0004e 56.23e 0.0005e 57.53e 0.0004e 58.79e 0.0005e 59.30e 0.0002e 60.03e 0.0002e 61.40e 0.0003e 62.84e 0.0001e 66.64e 0.0001e 67.97e 0.0001e 69.33e 0.0001e 70.68e 0.0001e 73.57e 0.0002e 75.76e 0.0002e 76.45e 0.0001e 79.27e 0.0001e 80.44e 0.0002e 81.87e 0.0002e ]
vct[ 2.00e 0.0311e 2.99e 0.0086e 3.99e 0.0266e 4.97e 0.0123e 5.98e 0.0235e 6.97e 0.0161e 7.97e 0.0008e 8.96e 0.0088e 9.96e 0.0621e 10.99e 0.0080e 11.99e 0.0034e 12.99e 0.0300e 14.03e 0.0228e 15.04e 0.0105e 16.03e 0.0004e 17.06e 0.0036e 18.09e 0.0094e 18.95e 0.0009e 20.17e 0.0071e 21.21e 0.0161e 22.25e 0.0106e 23.28e 0.0104e 24.33e 0.0008e 25.38e 0.0030e 26.46e 0.0035e 27.50e 0.0026e 28.59e 0.0028e 29.66e 0.0128e 30.75e 0.0139e 31.81e 0.0038e 32.93e 0.0006e 34.04e 0.0004e 35.16e 0.0005e 36.25e 0.0023e 37.35e 0.0012e 38.46e 0.0021e 39.59e 0.0035e 40.71e 0.0006e 41.86e 0.0007e 42.42e 0.0001e 43.46e 0.0003e 44.17e 0.0032e 45.29e 0.0013e 46.57e 0.0004e 47.72e 0.0011e 48.79e 0.0005e 50.11e 0.0005e 51.29e 0.0003e 52.47e 0.0002e 53.68e 0.0004e 55.02e 0.0005e 56.18e 0.0003e 57.41e 0.0003e 58.75e 0.0007e 59.33e 0.0009e 60.00e 0.0004e 61.34e 0.0001e 64.97e 0.0003e 65.20e 0.0002e 66.48e 0.0002e 67.83e 0.0002e 68.90e 0.0003e 70.25e 0.0003e 71.59e 0.0002e 73.68e 0.0001e 75.92e 0.0001e 77.08e 0.0002e 78.45e 0.0002e 81.56e 0.0002e 82.99e 0.0001e 88.39e 0.0001e ]
vct[ 0.97e 0.0059e 1.98e 0.0212e 2.99e 0.0153e 3.99e 0.0227e 4.96e 0.0215e 5.97e 0.0153e 6.98e 0.0085e 7.98e 0.0007e 8.97e 0.0179e 9.98e 0.0512e 10.98e 0.0322e 12.00e 0.0098e 13.02e 0.0186e 14.00e 0.0099e 15.05e 0.0109e 15.88e 0.0011e 17.07e 0.0076e 18.11e 0.0071e 19.12e 0.0045e 20.16e 0.0038e 21.23e 0.0213e 22.27e 0.0332e 23.34e 0.0082e 24.34e 0.0014e 25.42e 0.0024e 26.47e 0.0012e 27.54e 0.0014e 28.60e 0.0024e 29.72e 0.0026e 30.10e 0.0008e 31.91e 0.0021e 32.13e 0.0011e 33.02e 0.0007e 34.09e 0.0014e 35.17e 0.0007e 36.27e 0.0024e 37.39e 0.0029e 38.58e 0.0014e 39.65e 0.0017e 40.95e 0.0012e 41.97e 0.0004e 42.43e 0.0002e 43.49e 0.0001e 44.31e 0.0012e 45.42e 0.0031e 46.62e 0.0017e 47.82e 0.0013e 49.14e 0.0013e 50.18e 0.0010e 51.54e 0.0003e 53.90e 0.0006e 55.06e 0.0010e 56.31e 0.0003e 57.63e 0.0001e 59.02e 0.0003e 60.09e 0.0004e 60.35e 0.0004e 61.62e 0.0009e 63.97e 0.0001e 65.19e 0.0001e 65.54e 0.0002e 66.92e 0.0002e 67.94e 0.0002e 69.17e 0.0003e 69.60e 0.0004e 70.88e 0.0002e 72.24e 0.0002e 76.12e 0.0001e 78.94e 0.0001e 81.75e 0.0001e 82.06e 0.0001e 83.53e 0.0001e 90.29e 0.0002e 91.75e 0.0001e 92.09e 0.0002e 93.28e 0.0001e 97.07e 0.0001e ]
vct[ 1.98e 0.0159e 2.98e 0.1008e 3.98e 0.0365e 4.98e 0.0133e 5.97e 0.0101e 6.97e 0.0115e 7.97e 0.0007e 8.99e 0.0349e 10.01e 0.0342e 11.01e 0.0236e 12.00e 0.0041e 13.02e 0.0114e 14.05e 0.0137e 15.06e 0.0100e 16.05e 0.0007e 17.04e 0.0009e 18.12e 0.0077e 19.15e 0.0023e 20.12e 0.0017e 21.24e 0.0113e 22.26e 0.0126e 23.30e 0.0093e 24.36e 0.0007e 25.43e 0.0007e 26.47e 0.0009e 27.55e 0.0013e 28.59e 0.0025e 29.61e 0.0010e 30.77e 0.0021e 31.86e 0.0023e 32.96e 0.0003e 34.03e 0.0007e 35.06e 0.0005e 36.20e 0.0006e 37.34e 0.0006e 38.36e 0.0009e 39.60e 0.0016e 40.69e 0.0005e 41.77e 0.0002e 42.92e 0.0002e 44.02e 0.0003e 45.24e 0.0006e 46.33e 0.0004e 47.50e 0.0007e 48.71e 0.0007e 49.87e 0.0002e 51.27e 0.0002e 53.42e 0.0003e 55.88e 0.0003e 57.10e 0.0004e 58.34e 0.0002e 59.86e 0.0003e 61.13e 0.0003e 67.18e 0.0001e 68.50e 0.0001e 71.17e 0.0001e 83.91e 0.0001e 90.55e 0.0001e ]
vct[ 0.98e 0.0099e 2.00e 0.0181e 2.99e 0.0353e 3.98e 0.0285e 4.97e 0.0514e 5.96e 0.0402e 6.96e 0.0015e 7.98e 0.0012e 8.98e 0.0175e 9.98e 0.0264e 10.98e 0.0392e 11.98e 0.0236e 13.00e 0.0153e 14.04e 0.0049e 15.00e 0.0089e 16.01e 0.0001e 17.03e 0.0106e 18.03e 0.0028e 19.05e 0.0024e 20.08e 0.0040e 21.11e 0.0103e 22.12e 0.0104e 23.20e 0.0017e 24.19e 0.0008e 25.20e 0.0007e 26.24e 0.0011e 27.36e 0.0009e 27.97e 0.0030e 29.40e 0.0044e 30.37e 0.0019e 31.59e 0.0017e 32.65e 0.0008e 33.59e 0.0005e 34.79e 0.0009e 35.75e 0.0027e 36.88e 0.0035e 37.93e 0.0039e 39.00e 0.0031e 40.08e 0.0025e 41.16e 0.0010e 43.25e 0.0004e 44.52e 0.0012e 45.62e 0.0023e 45.85e 0.0012e 47.00e 0.0006e 47.87e 0.0008e 48.99e 0.0003e 50.48e 0.0003e 51.62e 0.0001e 52.43e 0.0001e 53.56e 0.0002e 54.76e 0.0002e 56.04e 0.0002e 56.68e 0.0006e 57.10e 0.0003e 58.28e 0.0005e 59.47e 0.0003e 59.96e 0.0002e 60.67e 0.0001e 63.08e 0.0002e 64.29e 0.0002e 66.72e 0.0001e 67.97e 0.0001e 68.65e 0.0001e 70.43e 0.0001e 79.38e 0.0001e 80.39e 0.0001e 82.39e 0.0001e ]
vct[ 1.00e 0.0765e 1.99e 0.0151e 2.99e 0.0500e 3.99e 0.0197e 5.00e 0.0260e 6.00e 0.0145e 6.98e 0.0128e 7.97e 0.0004e 8.98e 0.0158e 9.99e 0.0265e 11.02e 0.0290e 12.02e 0.0053e 13.03e 0.0242e 14.03e 0.0103e 15.06e 0.0054e 16.04e 0.0006e 17.08e 0.0008e 18.10e 0.0058e 19.16e 0.0011e 20.16e 0.0055e 21.18e 0.0040e 22.20e 0.0019e 23.22e 0.0014e 24.05e 0.0005e 25.31e 0.0019e 26.38e 0.0018e 27.44e 0.0022e 28.45e 0.0024e 29.57e 0.0073e 30.58e 0.0032e 31.66e 0.0071e 32.73e 0.0015e 33.85e 0.0005e 34.96e 0.0003e 36.00e 0.0020e 37.11e 0.0018e 38.18e 0.0055e 39.23e 0.0006e 40.33e 0.0004e 41.52e 0.0003e 43.41e 0.0028e 45.05e 0.0003e 45.99e 0.0002e 47.07e 0.0003e 48.52e 0.0002e 49.48e 0.0003e 50.63e 0.0003e 51.81e 0.0002e 54.05e 0.0002e 55.24e 0.0001e 56.62e 0.0001e 57.81e 0.0004e 59.16e 0.0013e 60.23e 0.0003e 66.44e 0.0001e 68.99e 0.0004e 75.49e 0.0001e 87.56e 0.0004e ]
vct[ 0.98e 0.0629e 1.99e 0.0232e 2.98e 0.0217e 4.00e 0.0396e 4.98e 0.0171e 5.97e 0.0098e 6.99e 0.0167e 7.99e 0.0003e 8.98e 0.0192e 9.98e 0.0266e 10.99e 0.0256e 12.01e 0.0061e 13.02e 0.0135e 14.02e 0.0062e 15.05e 0.0158e 16.06e 0.0018e 17.08e 0.0101e 18.09e 0.0053e 19.11e 0.0074e 20.13e 0.0020e 21.17e 0.0052e 22.22e 0.0077e 23.24e 0.0035e 24.00e 0.0009e 25.32e 0.0016e 26.40e 0.0022e 27.43e 0.0005e 28.55e 0.0026e 29.60e 0.0026e 30.65e 0.0010e 31.67e 0.0019e 32.77e 0.0008e 33.81e 0.0003e 34.91e 0.0003e 36.01e 0.0005e 37.11e 0.0010e 38.20e 0.0014e 39.29e 0.0039e 40.43e 0.0012e 41.50e 0.0006e 43.38e 0.0017e 43.75e 0.0002e 44.94e 0.0005e 46.13e 0.0002e 47.11e 0.0003e 48.28e 0.0005e 48.42e 0.0005e 49.44e 0.0003e 50.76e 0.0004e 51.93e 0.0002e 54.15e 0.0003e 55.31e 0.0005e 55.50e 0.0003e 56.98e 0.0003e 57.90e 0.0004e 60.33e 0.0002e 61.39e 0.0001e 61.59e 0.0001e 65.09e 0.0002e 66.34e 0.0001e 68.85e 0.0001e 70.42e 0.0002e 71.72e 0.0001e 73.05e 0.0003e 79.65e 0.0001e 85.28e 0.0002e 93.52e 0.0001e ]
vct[ 1.02e 0.0185e 1.99e 0.0525e 2.98e 0.0613e 3.99e 0.0415e 4.98e 0.0109e 5.97e 0.0248e 6.99e 0.0102e 7.98e 0.0005e 8.98e 0.0124e 9.99e 0.0103e 10.99e 0.0124e 12.00e 0.0016e 13.01e 0.0029e 14.03e 0.0211e 15.04e 0.0128e 16.07e 0.0021e 17.09e 0.0009e 18.09e 0.0043e 19.14e 0.0022e 20.13e 0.0016e 21.20e 0.0045e 22.21e 0.0088e 23.26e 0.0046e 24.29e 0.0013e 25.35e 0.0009e 26.39e 0.0028e 27.49e 0.0009e 28.51e 0.0006e 29.58e 0.0012e 30.70e 0.0010e 31.74e 0.0019e 32.75e 0.0002e 33.85e 0.0001e 34.95e 0.0005e 36.02e 0.0003e 37.16e 0.0009e 38.25e 0.0018e 39.35e 0.0008e 40.54e 0.0004e 41.61e 0.0002e 43.40e 0.0004e 43.74e 0.0003e 45.05e 0.0001e 46.11e 0.0003e 47.40e 0.0002e 48.36e 0.0004e 49.55e 0.0004e 50.72e 0.0002e 52.00e 0.0001e 55.58e 0.0002e 57.02e 0.0001e 57.98e 0.0002e 59.13e 0.0003e 61.56e 0.0001e 66.56e 0.0001e 87.65e 0.0002e ]
vct[ 1.00e 0.0473e 1.99e 0.0506e 2.99e 0.0982e 3.99e 0.0654e 5.00e 0.0196e 5.99e 0.0094e 6.99e 0.0118e 7.93e 0.0001e 8.99e 0.0057e 10.01e 0.0285e 11.01e 0.0142e 12.03e 0.0032e 13.03e 0.0056e 14.06e 0.0064e 15.06e 0.0059e 16.11e 0.0005e 17.09e 0.0033e 18.14e 0.0027e 19.15e 0.0014e 20.17e 0.0010e 21.21e 0.0059e 22.26e 0.0043e 23.31e 0.0031e 24.31e 0.0018e 25.33e 0.0009e 26.41e 0.0005e 27.47e 0.0015e 28.53e 0.0015e 29.58e 0.0041e 30.65e 0.0025e 31.73e 0.0011e 32.83e 0.0010e 34.98e 0.0003e 36.07e 0.0009e 37.23e 0.0001e 38.26e 0.0020e 39.41e 0.0014e 40.53e 0.0005e 41.40e 0.0003e 42.80e 0.0002e 43.48e 0.0028e 43.93e 0.0001e 45.03e 0.0003e 46.18e 0.0007e 47.41e 0.0001e 48.57e 0.0002e 49.67e 0.0001e 50.83e 0.0002e 54.39e 0.0001e 55.58e 0.0002e 57.97e 0.0005e 58.11e 0.0002e 59.21e 0.0001e 60.42e 0.0002e 61.66e 0.0001e ]
vct[ 1.00e 0.0503e 2.00e 0.0963e 2.99e 0.1304e 3.99e 0.0218e 4.98e 0.0041e 5.98e 0.0292e 6.98e 0.0482e 7.99e 0.0005e 8.99e 0.0280e 10.00e 0.0237e 11.00e 0.0152e 12.02e 0.0036e 12.95e 0.0022e 14.06e 0.0111e 15.07e 0.0196e 16.08e 0.0016e 17.11e 0.0044e 18.13e 0.0073e 19.17e 0.0055e 20.19e 0.0028e 21.20e 0.0012e 22.27e 0.0068e 23.30e 0.0036e 24.35e 0.0012e 25.35e 0.0002e 26.46e 0.0005e 27.47e 0.0005e 28.59e 0.0009e 29.65e 0.0021e 30.70e 0.0020e 31.78e 0.0012e 32.89e 0.0010e 35.06e 0.0005e 36.16e 0.0008e 37.27e 0.0010e 38.36e 0.0010e 39.47e 0.0014e 40.58e 0.0004e 41.43e 0.0007e 41.82e 0.0003e 43.48e 0.0008e 44.53e 0.0001e 45.25e 0.0003e 46.43e 0.0002e 47.46e 0.0002e 48.76e 0.0005e 49.95e 0.0004e 50.96e 0.0002e 51.12e 0.0002e 52.33e 0.0001e 54.75e 0.0001e 55.75e 0.0002e 56.90e 0.0002e 58.17e 0.0002e 59.40e 0.0004e 60.62e 0.0002e 65.65e 0.0001e 66.91e 0.0002e 69.91e 0.0001e 71.25e 0.0002e ]
vct[ 1.00e 0.1243e 1.98e 0.1611e 3.00e 0.0698e 3.98e 0.0390e 5.00e 0.0138e 5.99e 0.0154e 7.01e 0.0287e 8.01e 0.0014e 9.01e 0.0049e 10.00e 0.0144e 11.01e 0.0055e 12.05e 0.0052e 13.01e 0.0011e 14.05e 0.0118e 15.07e 0.0154e 16.12e 0.0028e 17.14e 0.0061e 18.25e 0.0007e 19.22e 0.0020e 20.24e 0.0011e 21.27e 0.0029e 22.30e 0.0046e 23.34e 0.0049e 24.35e 0.0004e 25.45e 0.0003e 26.47e 0.0007e 27.59e 0.0008e 28.16e 0.0009e 29.12e 0.0002e 29.81e 0.0006e 30.81e 0.0009e 31.95e 0.0004e 33.00e 0.0011e 34.12e 0.0005e 35.18e 0.0003e 36.30e 0.0008e 37.38e 0.0003e 38.55e 0.0003e 39.64e 0.0006e 40.77e 0.0007e 41.52e 0.0006e 41.89e 0.0006e 43.04e 0.0011e 43.60e 0.0009e 44.31e 0.0002e 45.68e 0.0002e 46.56e 0.0003e 47.60e 0.0001e 48.83e 0.0006e 50.01e 0.0003e 51.27e 0.0003e 56.04e 0.0005e 57.21e 0.0003e 58.56e 0.0004e 59.83e 0.0003e 61.05e 0.0001e 62.20e 0.0001e 67.37e 0.0002e 76.53e 0.0001e ]
vct[ 0.99e 0.0222e 1.99e 0.0678e 2.99e 0.0683e 4.00e 0.0191e 5.00e 0.0119e 6.01e 0.0232e 6.98e 0.0336e 7.99e 0.0082e 9.01e 0.0201e 10.01e 0.0189e 11.01e 0.0041e 12.01e 0.0053e 13.05e 0.0154e 14.04e 0.0159e 15.06e 0.0092e 16.11e 0.0038e 17.12e 0.0014e 18.15e 0.0091e 19.16e 0.0006e 20.30e 0.0012e 21.25e 0.0061e 22.28e 0.0099e 23.34e 0.0028e 24.38e 0.0012e 25.43e 0.0016e 26.49e 0.0048e 27.55e 0.0025e 28.62e 0.0015e 29.71e 0.0032e 30.78e 0.0077e 31.88e 0.0011e 32.97e 0.0007e 34.08e 0.0006e 35.16e 0.0008e 36.28e 0.0004e 37.41e 0.0006e 38.54e 0.0005e 39.62e 0.0002e 40.80e 0.0003e 41.93e 0.0001e 43.06e 0.0002e 44.21e 0.0003e 45.38e 0.0002e 46.54e 0.0007e 47.78e 0.0003e 48.95e 0.0004e 50.10e 0.0003e 51.37e 0.0002e 53.79e 0.0003e 56.20e 0.0001e 58.71e 0.0002e 66.47e 0.0003e ]
vct[ 1.01e 0.0241e 1.99e 0.1011e 2.98e 0.0938e 3.98e 0.0081e 4.99e 0.0062e 5.99e 0.0291e 6.99e 0.0676e 7.59e 0.0004e 8.98e 0.0127e 9.99e 0.0112e 10.99e 0.0142e 12.00e 0.0029e 13.02e 0.0071e 14.02e 0.0184e 15.03e 0.0064e 16.07e 0.0010e 17.09e 0.0011e 18.11e 0.0010e 19.15e 0.0060e 20.19e 0.0019e 21.24e 0.0025e 22.29e 0.0013e 23.31e 0.0050e 25.41e 0.0030e 26.50e 0.0018e 27.53e 0.0006e 28.63e 0.0012e 29.66e 0.0013e 30.77e 0.0020e 31.84e 0.0006e 34.04e 0.0001e 35.14e 0.0001e 36.32e 0.0004e 37.41e 0.0007e 38.53e 0.0007e 39.67e 0.0009e 40.85e 0.0003e 45.49e 0.0002e 46.65e 0.0001e 47.81e 0.0004e 49.01e 0.0002e 53.91e 0.0002e 55.14e 0.0002e 57.69e 0.0002e ]
vct[ 1.00e 0.0326e 2.00e 0.1066e 2.99e 0.1015e 4.00e 0.0210e 4.97e 0.0170e 5.99e 0.0813e 6.98e 0.0820e 7.96e 0.0011e 8.99e 0.0248e 10.03e 0.0107e 11.01e 0.0126e 12.01e 0.0027e 13.01e 0.0233e 14.04e 0.0151e 15.05e 0.0071e 16.04e 0.0002e 17.10e 0.0061e 18.12e 0.0059e 19.15e 0.0087e 20.23e 0.0005e 21.25e 0.0040e 22.30e 0.0032e 23.35e 0.0004e 24.40e 0.0001e 25.45e 0.0030e 26.54e 0.0022e 27.60e 0.0003e 28.70e 0.0009e 29.80e 0.0029e 30.85e 0.0006e 31.97e 0.0006e 34.19e 0.0004e 35.30e 0.0003e 36.43e 0.0007e 37.56e 0.0005e 38.68e 0.0019e 39.88e 0.0013e 41.00e 0.0003e 43.35e 0.0003e 44.51e 0.0002e 45.68e 0.0006e 46.93e 0.0010e 48.11e 0.0006e 49.29e 0.0003e 55.58e 0.0002e ]
vct[ 0.98e 0.0113e 1.99e 0.0967e 3.00e 0.0719e 3.98e 0.0345e 4.98e 0.0121e 6.00e 0.0621e 7.00e 0.0137e 7.98e 0.0006e 9.01e 0.0314e 10.01e 0.0171e 11.02e 0.0060e 12.03e 0.0024e 13.05e 0.0077e 14.07e 0.0040e 15.12e 0.0032e 16.13e 0.0004e 17.15e 0.0011e 18.20e 0.0028e 19.18e 0.0003e 20.26e 0.0003e 21.31e 0.0025e 22.35e 0.0021e 23.39e 0.0005e 25.55e 0.0002e 26.62e 0.0014e 27.70e 0.0003e 28.78e 0.0005e 29.90e 0.0030e 31.01e 0.0011e 32.12e 0.0005e 34.31e 0.0001e 35.50e 0.0002e 36.62e 0.0002e 37.76e 0.0005e 38.85e 0.0002e 40.09e 0.0004e 43.60e 0.0001e 44.73e 0.0002e 46.02e 0.0002e 47.25e 0.0004e 48.44e 0.0004e ]
vct[ 0.99e 0.0156e 1.98e 0.0846e 2.98e 0.0178e 3.98e 0.0367e 4.98e 0.0448e 5.98e 0.0113e 6.99e 0.0189e 8.00e 0.0011e 9.01e 0.0247e 10.02e 0.0089e 11.01e 0.0184e 12.03e 0.0105e 13.00e 0.0039e 14.07e 0.0116e 15.09e 0.0078e 16.13e 0.0008e 17.14e 0.0064e 18.19e 0.0029e 19.22e 0.0028e 20.25e 0.0017e 21.32e 0.0043e 22.37e 0.0055e 23.42e 0.0034e 24.48e 0.0004e 25.54e 0.0002e 26.61e 0.0017e 27.70e 0.0011e 28.80e 0.0002e 29.89e 0.0019e 30.97e 0.0028e 32.09e 0.0007e 34.30e 0.0002e 35.44e 0.0003e 36.55e 0.0001e 37.69e 0.0004e 38.93e 0.0002e 40.05e 0.0005e 41.20e 0.0005e 42.37e 0.0002e 43.54e 0.0003e 44.73e 0.0001e 45.95e 0.0002e 47.16e 0.0001e 48.43e 0.0005e 49.65e 0.0004e 55.90e 0.0002e 59.81e 0.0004e ]
vct[ 1.01e 0.0280e 2.00e 0.0708e 2.99e 0.0182e 3.99e 0.0248e 4.98e 0.0245e 5.98e 0.0279e 6.98e 0.0437e 7.99e 0.0065e 8.99e 0.0299e 10.00e 0.0073e 10.99e 0.0011e 12.03e 0.0122e 13.03e 0.0028e 14.08e 0.0044e 15.11e 0.0097e 16.15e 0.0010e 17.17e 0.0025e 18.19e 0.0017e 19.24e 0.0008e 20.28e 0.0040e 21.32e 0.0024e 22.38e 0.0008e 23.46e 0.0032e 24.52e 0.0010e 25.59e 0.0008e 26.68e 0.0009e 27.76e 0.0012e 28.88e 0.0003e 29.95e 0.0005e 31.05e 0.0017e 32.14e 0.0002e 33.29e 0.0003e 37.88e 0.0002e 39.03e 0.0002e 40.19e 0.0004e 41.37e 0.0003e 43.74e 0.0002e 46.20e 0.0001e 48.68e 0.0001e 49.93e 0.0001e 51.19e 0.0002e ]
vct[ 1.00e 0.0225e 1.99e 0.0921e 2.98e 0.0933e 3.99e 0.0365e 4.99e 0.0100e 5.98e 0.0213e 6.98e 0.0049e 7.98e 0.0041e 8.98e 0.0090e 9.99e 0.0068e 11.01e 0.0040e 12.03e 0.0086e 13.02e 0.0015e 14.04e 0.0071e 15.09e 0.0082e 16.14e 0.0011e 17.15e 0.0014e 18.18e 0.0010e 19.26e 0.0013e 20.26e 0.0005e 21.33e 0.0006e 22.36e 0.0011e 23.46e 0.0016e 24.52e 0.0004e 25.59e 0.0002e 26.70e 0.0006e 27.78e 0.0007e 28.87e 0.0002e 30.03e 0.0008e 31.14e 0.0010e 32.24e 0.0006e 33.37e 0.0002e 35.67e 0.0003e 37.99e 0.0004e 39.17e 0.0004e 40.35e 0.0005e 41.53e 0.0001e 46.42e 0.0001e ]
vct[ 1.00e 0.0465e 1.99e 0.0976e 2.98e 0.0678e 4.00e 0.0727e 4.99e 0.0305e 5.98e 0.0210e 6.98e 0.0227e 8.00e 0.0085e 9.01e 0.0183e 10.02e 0.0258e 11.05e 0.0003e 12.06e 0.0061e 13.05e 0.0021e 14.10e 0.0089e 15.12e 0.0077e 16.16e 0.0016e 17.21e 0.0061e 18.23e 0.0011e 19.29e 0.0031e 20.36e 0.0031e 21.41e 0.0007e 22.48e 0.0013e 23.55e 0.0020e 24.64e 0.0004e 25.74e 0.0005e 26.81e 0.0006e 27.95e 0.0006e 29.03e 0.0001e 30.22e 0.0010e 31.30e 0.0004e 32.48e 0.0001e 33.60e 0.0002e 38.30e 0.0003e ]
vct[ 1.00e 0.0674e 1.99e 0.0841e 2.98e 0.0920e 3.99e 0.0328e 4.99e 0.0368e 5.98e 0.0206e 6.99e 0.0246e 8.01e 0.0048e 9.01e 0.0218e 10.03e 0.0155e 11.05e 0.0048e 12.06e 0.0077e 13.00e 0.0020e 14.10e 0.0083e 15.15e 0.0084e 16.18e 0.0015e 17.22e 0.0039e 18.27e 0.0032e 19.34e 0.0026e 20.40e 0.0012e 21.47e 0.0009e 22.54e 0.0008e 23.62e 0.0016e 24.71e 0.0005e 25.82e 0.0004e 26.91e 0.0002e 28.03e 0.0008e 29.17e 0.0002e 30.32e 0.0028e 31.45e 0.0004e 32.61e 0.0005e 33.77e 0.0001e 36.14e 0.0003e 37.32e 0.0002e 38.54e 0.0005e 39.75e 0.0002e 42.23e 0.0002e 48.65e 0.0001e ]
vct[ 1.01e 0.0423e 1.99e 0.0240e 2.98e 0.0517e 4.00e 0.0493e 5.00e 0.0324e 6.00e 0.0094e 6.99e 0.0449e 7.99e 0.0050e 9.00e 0.0197e 10.03e 0.0132e 11.03e 0.0009e 12.07e 0.0017e 13.08e 0.0023e 14.12e 0.0094e 15.16e 0.0071e 16.21e 0.0020e 17.25e 0.0005e 18.30e 0.0027e 19.04e 0.0004e 20.43e 0.0022e 21.51e 0.0002e 22.59e 0.0006e 23.72e 0.0018e 24.80e 0.0002e 25.88e 0.0002e 27.03e 0.0002e 28.09e 0.0006e 29.31e 0.0002e 30.46e 0.0004e 31.61e 0.0007e 32.78e 0.0005e 33.95e 0.0001e 36.34e 0.0002e 37.56e 0.0001e 38.80e 0.0001e 40.02e 0.0001e 44.14e 0.0001e ]
vct[ 1.00e 0.0669e 1.99e 0.0909e 2.99e 0.0410e 3.98e 0.0292e 4.98e 0.0259e 5.98e 0.0148e 6.98e 0.0319e 7.99e 0.0076e 9.01e 0.0056e 10.02e 0.0206e 11.04e 0.0032e 12.05e 0.0085e 13.08e 0.0040e 14.12e 0.0037e 15.16e 0.0030e 16.20e 0.0013e 17.24e 0.0021e 18.30e 0.0010e 19.36e 0.0015e 20.44e 0.0013e 21.50e 0.0009e 22.60e 0.0015e 23.69e 0.0014e 24.80e 0.0006e 25.87e 0.0002e 27.02e 0.0006e 28.12e 0.0002e 29.28e 0.0003e 30.43e 0.0002e 31.59e 0.0007e 32.79e 0.0001e 35.14e 0.0001e 37.57e 0.0001e 40.03e 0.0002e 41.28e 0.0004e 44.10e 0.0001e ]
vct[ 0.99e 0.0421e 1.99e 0.1541e 2.98e 0.0596e 3.98e 0.0309e 4.98e 0.0301e 5.99e 0.0103e 7.00e 0.0240e 8.01e 0.0073e 9.01e 0.0222e 10.04e 0.0140e 11.05e 0.0033e 12.08e 0.0045e 13.13e 0.0009e 14.13e 0.0015e 15.21e 0.0026e 16.24e 0.0003e 17.30e 0.0004e 18.35e 0.0010e 19.39e 0.0003e 20.50e 0.0015e 21.57e 0.0003e 22.68e 0.0011e 23.80e 0.0005e 24.90e 0.0008e 26.02e 0.0002e 27.16e 0.0001e 28.30e 0.0006e 29.48e 0.0002e 31.81e 0.0005e 33.00e 0.0003e 34.21e 0.0001e 37.89e 0.0001e ]
vct[ 0.99e 0.0389e 2.00e 0.2095e 3.00e 0.0835e 3.99e 0.0289e 5.00e 0.0578e 5.99e 0.0363e 7.01e 0.0387e 8.01e 0.0056e 9.04e 0.0173e 10.05e 0.0175e 11.08e 0.0053e 12.10e 0.0056e 13.15e 0.0064e 14.19e 0.0036e 15.22e 0.0019e 16.29e 0.0010e 17.36e 0.0017e 18.43e 0.0018e 19.51e 0.0004e 20.60e 0.0011e 21.70e 0.0003e 22.82e 0.0003e 23.95e 0.0001e 25.05e 0.0004e 26.17e 0.0001e 28.50e 0.0003e 29.68e 0.0001e 32.07e 0.0003e 33.28e 0.0004e 34.52e 0.0001e ]
vct[ 1.00e 0.1238e 1.99e 0.2270e 3.00e 0.0102e 3.99e 0.0181e 4.98e 0.0415e 6.00e 0.0165e 7.01e 0.0314e 8.02e 0.0148e 9.04e 0.0203e 10.05e 0.0088e 11.07e 0.0062e 12.11e 0.0070e 13.14e 0.0054e 14.19e 0.0028e 15.24e 0.0044e 16.30e 0.0029e 17.38e 0.0009e 18.45e 0.0026e 19.56e 0.0003e 20.65e 0.0025e 21.74e 0.0014e 22.87e 0.0013e 23.99e 0.0007e 25.15e 0.0002e 27.46e 0.0004e 28.39e 0.0006e 28.65e 0.0004e 29.85e 0.0001e 31.05e 0.0002e 32.27e 0.0003e 33.52e 0.0002e 34.76e 0.0003e ]
vct[ 1.00e 0.1054e 2.00e 0.2598e 2.99e 0.0369e 3.98e 0.0523e 4.99e 0.0020e 5.99e 0.0051e 7.00e 0.0268e 8.01e 0.0027e 9.04e 0.0029e 10.05e 0.0081e 11.08e 0.0047e 12.12e 0.0051e 13.16e 0.0091e 14.19e 0.0015e 15.27e 0.0030e 16.34e 0.0017e 17.42e 0.0006e 18.51e 0.0003e 19.61e 0.0007e 20.72e 0.0003e 21.84e 0.0001e 22.99e 0.0010e 24.13e 0.0001e 28.44e 0.0001e 30.09e 0.0001e ]
vct[ 0.99e 0.0919e 2.00e 0.0418e 2.99e 0.0498e 3.99e 0.0135e 4.99e 0.0026e 6.00e 0.0155e 7.01e 0.0340e 8.02e 0.0033e 9.04e 0.0218e 10.08e 0.0084e 11.11e 0.0057e 12.15e 0.0051e 13.21e 0.0043e 14.25e 0.0015e 15.31e 0.0023e 16.40e 0.0008e 17.48e 0.0004e 18.59e 0.0016e 19.71e 0.0010e 20.84e 0.0018e 21.98e 0.0002e 23.11e 0.0013e 24.26e 0.0003e 26.67e 0.0002e 29.12e 0.0002e 30.37e 0.0002e 31.62e 0.0003e 32.92e 0.0001e ]
vct[ 0.99e 0.1174e 1.99e 0.1126e 2.99e 0.0370e 3.99e 0.0159e 5.01e 0.0472e 6.01e 0.0091e 7.03e 0.0211e 8.05e 0.0015e 9.07e 0.0098e 10.11e 0.0038e 11.15e 0.0042e 12.20e 0.0018e 13.24e 0.0041e 14.32e 0.0033e 15.41e 0.0052e 16.49e 0.0001e 17.61e 0.0004e 18.71e 0.0004e 19.84e 0.0004e 20.99e 0.0002e 22.14e 0.0006e 23.31e 0.0006e 24.50e 0.0004e 25.70e 0.0002e 28.09e 0.0002e 28.66e 0.0002e 32.00e 0.0001e ]
vct[ 1.00e 0.1085e 2.00e 0.1400e 2.99e 0.0173e 3.99e 0.0229e 5.00e 0.0272e 6.02e 0.0077e 7.03e 0.0069e 8.04e 0.0017e 9.08e 0.0045e 10.10e 0.0030e 11.15e 0.0040e 12.20e 0.0007e 13.25e 0.0019e 14.32e 0.0008e 15.42e 0.0024e 16.50e 0.0002e 17.59e 0.0005e 18.71e 0.0003e 19.83e 0.0002e 20.98e 0.0005e 23.29e 0.0008e ]
vct[ 1.00e 0.0985e 2.00e 0.1440e 2.99e 0.0364e 3.99e 0.0425e 5.00e 0.0190e 6.01e 0.0089e 7.03e 0.0278e 8.04e 0.0006e 9.07e 0.0083e 10.10e 0.0021e 11.14e 0.0050e 12.18e 0.0005e 13.26e 0.0036e 14.33e 0.0005e 15.41e 0.0026e 17.62e 0.0004e 18.75e 0.0004e 19.89e 0.0003e 21.04e 0.0012e 22.21e 0.0002e 23.38e 0.0004e 27.04e 0.0001e ]
vct[ 0.99e 0.1273e 2.00e 0.1311e 2.99e 0.0120e 4.00e 0.0099e 5.00e 0.0235e 6.02e 0.0068e 7.03e 0.0162e 8.06e 0.0009e 9.08e 0.0083e 10.12e 0.0014e 11.17e 0.0050e 12.24e 0.0010e 13.29e 0.0013e 14.39e 0.0022e 15.48e 0.0011e 16.59e 0.0002e 17.70e 0.0003e 18.84e 0.0010e 20.00e 0.0003e 21.17e 0.0003e 23.56e 0.0004e 28.79e 0.0003e ]
vct[ 1.00e 0.1018e 2.00e 0.1486e 3.00e 0.0165e 4.00e 0.0186e 5.01e 0.0194e 6.02e 0.0045e 7.04e 0.0083e 8.06e 0.0012e 9.10e 0.0066e 10.15e 0.0009e 11.19e 0.0008e 12.26e 0.0011e 13.34e 0.0028e 14.45e 0.0006e 15.53e 0.0009e 16.66e 0.0002e 17.79e 0.0006e 18.94e 0.0005e 20.11e 0.0003e 21.29e 0.0005e 22.49e 0.0003e 23.73e 0.0005e 26.22e 0.0001e 27.52e 0.0001e 28.88e 0.0002e ]
vct[ 1.00e 0.1889e 1.99e 0.1822e 3.00e 0.0363e 4.00e 0.0047e 5.01e 0.0202e 6.03e 0.0053e 7.05e 0.0114e 8.01e 0.0002e 9.13e 0.0048e 10.17e 0.0010e 11.23e 0.0033e 12.30e 0.0010e 13.38e 0.0006e 14.50e 0.0002e 15.62e 0.0010e 20.27e 0.0001e 21.47e 0.0001e ]
vct[ 1.00e 0.0522e 1.99e 0.0763e 2.99e 0.0404e 4.00e 0.0139e 5.01e 0.0185e 6.01e 0.0021e 7.06e 0.0045e 8.09e 0.0002e 9.11e 0.0003e 10.17e 0.0006e 11.25e 0.0004e 12.32e 0.0005e 13.40e 0.0003e 14.53e 0.0003e 15.65e 0.0007e 16.80e 0.0001e 17.95e 0.0002e 19.14e 0.0006e 20.34e 0.0002e 21.56e 0.0003e ]
vct[ 0.99e 0.1821e 1.99e 0.0773e 3.00e 0.0125e 4.01e 0.0065e 5.01e 0.0202e 6.03e 0.0071e 7.05e 0.0090e 8.08e 0.0006e 9.13e 0.0008e 10.18e 0.0013e 11.25e 0.0010e 12.33e 0.0012e 13.42e 0.0006e 14.54e 0.0005e 15.65e 0.0004e 17.97e 0.0002e 19.15e 0.0001e ]
vct[ 1.00e 0.1868e 2.00e 0.0951e 3.00e 0.0147e 4.01e 0.0134e 5.02e 0.0184e 6.04e 0.0132e 7.06e 0.0011e 8.11e 0.0008e 9.15e 0.0010e 10.22e 0.0012e 11.30e 0.0011e 12.40e 0.0003e 13.11e 0.0004e 13.49e 0.0002e 14.62e 0.0003e 15.77e 0.0001e ]
vct[ 1.00e 0.1933e 2.00e 0.0714e 3.00e 0.0373e 4.00e 0.0108e 5.02e 0.0094e 6.02e 0.0010e 7.07e 0.0022e 8.11e 0.0002e 9.16e 0.0065e 10.23e 0.0015e 11.31e 0.0023e 12.40e 0.0003e 13.53e 0.0014e 14.66e 0.0002e 15.81e 0.0011e 18.20e 0.0002e 19.41e 0.0001e ]
vct[ 0.99e 0.2113e 1.99e 0.0877e 3.00e 0.0492e 4.01e 0.0094e 5.02e 0.0144e 6.04e 0.0103e 7.07e 0.0117e 8.12e 0.0006e 9.19e 0.0019e 10.25e 0.0007e 11.35e 0.0017e 12.45e 0.0010e 13.58e 0.0003e 14.74e 0.0003e 15.91e 0.0003e 19.57e 0.0002e ]
vct[ 0.99e 0.2455e 1.99e 0.0161e 3.00e 0.0215e 4.01e 0.0036e 5.03e 0.0049e 6.04e 0.0012e 7.09e 0.0036e 8.14e 0.0011e 9.21e 0.0009e 10.30e 0.0001e 11.40e 0.0012e 12.50e 0.0001e 13.66e 0.0005e 14.84e 0.0001e ]
vct[ 1.00e 0.1132e 2.00e 0.0252e 3.00e 0.0292e 4.01e 0.0136e 5.03e 0.0045e 6.06e 0.0022e 7.11e 0.0101e 8.17e 0.0004e 9.23e 0.0010e 10.33e 0.0012e 11.44e 0.0013e 12.58e 0.0011e 13.75e 0.0002e 14.93e 0.0005e 16.14e 0.0002e ]
vct[ 1.00e 0.1655e 2.00e 0.0445e 3.00e 0.0120e 4.00e 0.0038e 5.02e 0.0015e 6.07e 0.0038e 7.11e 0.0003e 8.19e 0.0002e 9.25e 0.0010e 10.36e 0.0011e 11.48e 0.0005e 12.63e 0.0002e 13.79e 0.0003e 16.24e 0.0002e ]
vct[ 0.99e 0.3637e 1.99e 0.0259e 3.01e 0.0038e 4.01e 0.0057e 5.03e 0.0040e 6.07e 0.0067e 7.12e 0.0014e 8.19e 0.0004e 9.27e 0.0003e 10.38e 0.0002e 12.67e 0.0001e ]
vct[ 1.00e 0.1193e 2.00e 0.0230e 3.00e 0.0104e 4.01e 0.0084e 5.04e 0.0047e 6.08e 0.0035e 7.13e 0.0041e 8.20e 0.0002e 9.29e 0.0005e 10.40e 0.0005e 11.53e 0.0003e 12.70e 0.0002e 13.91e 0.0002e ]
vct[ 1.00e 0.0752e 2.00e 0.0497e 3.00e 0.0074e 4.02e 0.0076e 5.05e 0.0053e 6.09e 0.0043e 7.15e 0.0024e 8.22e 0.0001e 9.32e 0.0006e 10.45e 0.0002e 11.58e 0.0001e 12.78e 0.0001e 15.22e 0.0001e ]
vct[ 1.00e 0.2388e 2.00e 0.0629e 3.01e 0.0159e 4.04e 0.0063e 5.07e 0.0051e 6.12e 0.0045e 7.19e 0.0026e 8.29e 0.0015e 9.43e 0.0001e 11.75e 0.0002e ]
vct[ 1.00e 0.1919e 2.01e 0.0116e 3.01e 0.0031e 4.03e 0.0090e 5.07e 0.0061e 6.13e 0.0036e 7.19e 0.0013e 8.30e 0.0016e 9.13e 0.0001e 10.59e 0.0002e 11.78e 0.0002e ]
vct[ 1.00e 0.1296e 2.00e 0.0135e 3.01e 0.0041e 4.04e 0.0045e 5.09e 0.0028e 6.14e 0.0046e 7.23e 0.0007e 8.32e 0.0007e 9.50e 0.0001e ]
vct[ 1.00e 0.0692e 2.00e 0.0209e 3.02e 0.0025e 4.05e 0.0030e 5.09e 0.0047e 6.17e 0.0022e 7.25e 0.0015e 8.36e 0.0015e 9.53e 0.0010e 10.69e 0.0001e 13.40e 0.0001e ]
vct[ 1.00e 0.1715e 2.00e 0.0142e 3.01e 0.0024e 4.03e 0.0015e 5.07e 0.0017e 6.13e 0.0018e 7.22e 0.0009e 8.33e 0.0014e 9.51e 0.0007e 10.69e 0.0002e ]
vct[ 1.00e 0.1555e 2.01e 0.0148e 3.02e 0.0007e 4.06e 0.0006e 5.10e 0.0005e 6.16e 0.0008e 7.26e 0.0009e 8.39e 0.0008e 9.58e 0.0002e ]
vct[ 1.00e 0.1357e 2.00e 0.0116e 3.02e 0.0026e 4.04e 0.0009e 5.09e 0.0004e 6.17e 0.0005e 7.27e 0.0002e 8.40e 0.0001e ]
vct[ 1.00e 0.2185e 2.01e 0.0087e 3.03e 0.0018e 4.06e 0.0025e 5.11e 0.0020e 6.20e 0.0012e 7.32e 0.0005e 8.46e 0.0001e 9.66e 0.0003e ]
vct[ 1.00e 0.2735e 2.00e 0.0038e 3.02e 0.0008e 4.06e 0.0012e 5.12e 0.0008e 6.22e 0.0011e 7.35e 0.0003e 8.50e 0.0002e ]
vct[ 1.00e 0.1441e 1.99e 0.0062e 3.01e 0.0023e 4.05e 0.0011e 5.11e 0.0012e 6.20e 0.0003e 7.33e 0.0004e 8.50e 0.0001e ]
vct[ 1.00e 0.0726e 2.01e 0.0293e 3.03e 0.0022e 5.14e 0.0005e 6.26e 0.0011e 7.41e 0.0002e 8.63e 0.0002e ]
vct[ 1.00e 0.0516e 2.00e 0.0104e 3.02e 0.0029e 5.15e 0.0002e 6.27e 0.0001e ]
vct[ 1.00e 0.0329e 2.00e 0.0033e 3.03e 0.0013e 4.10e 0.0005e 5.19e 0.0004e 6.32e 0.0002e ]
vct[ 1.00e 0.0179e 1.99e 0.0012e 3.04e 0.0005e 4.10e 0.0017e 5.20e 0.0005e 6.35e 0.0001e ]
vct[ 1.00e 0.0334e 2.01e 0.0033e 3.04e 0.0011e 4.13e 0.0003e 5.22e 0.0003e ]
vct[ 0.99e 0.0161e 2.01e 0.0100e 3.04e 0.0020e 4.13e 0.0003e ]
vct[ 1.00e 0.0475e 1.99e 0.0045e 3.03e 0.0035e 4.12e 0.0011e ]
vct[ 1.00e 0.0593e 2.00e 0.0014e 4.17e 0.0002e ]
vct[ 1.00e 0.0249e 2.01e 0.0016e ]
vct[ 1.00e 0.0242e 2.00e 0.0038e 4.19e 0.0002e ]
vct[ 1.00e 0.0170e 2.02e 0.0030e ]
vct[ 1.00e 0.0381e 2.00e 0.0017e 3.09e 0.0002e ]
vct[ 1.00e 0.0141e 2.03e 0.0005e 3.11e 0.0003e 4.26e 0.0001e ]
vct[ 1.00e 0.0122e 2.03e 0.0024e ]
vct[ 1.00e 0.0107e 2.07e 0.0007e 3.12e 0.0004e ]
vct[ 1.00e 0.0250e 2.02e 0.0026e 3.15e 0.0002e ]
vct[ 1.01e 0.0092e ]
vct[ 1.01e 0.0102e 2.09e 0.0005e ]
vct[ 1.00e 0.0080e 2.00e 0.0005e 3.19e 0.0001e ]
vct[ 1.01e 0.0298e 2.01e 0.0005e ] 80 >array constant piano-spectra

\ This thing sounds pretty good down low, below middle c or so.
\ Unfortunately, there are some tens of partials down there and we're
\ using exponential envelopes.  You're going to wait for a long long
\ time just to hear a single low note.  The high notes sound pretty
\ rotten--they just don't sparkle; I have a feeling that this is due
\ to the low amplitude of the original data, and the lack of
\ mechanical noise.
\
\ The only thing you can do to alter the sound of a piano note is to
\ set the pfreq parameter.  Pfreq is used to look up the partials.  By
\ default, it's set to the requested frequency.  Setting it to a
\ neighboring freq is useful when you're repeating notes.  Note that
\ there's no nyquist detection; a high freq with a low pfreq, will
\ give you fold over (hmmm...maybe I can get those high notes to
\ sparkle after all).
instrument: lbj-piano { f: start f: dur f: freq f: amp -- }
    12e freq 32.703e f/ fln 2e fln f/ f* f>s piano-spectra array@ normalize-partials { parts }
    dur *clm-piano-attack-duration* *clm-piano-realease-duration* f+ f+ to dur
    dur *clm-piano-realease-duration* f- { f: env1dur }
    env1dur mus-srate@ f* floor f>s { env1samples }
    parts length 2/ make-vct { alist }
    0e 0e
    *clm-piano-attack-duration* 100e f* env1dur f/ 4e f/ 1e
    *clm-piano-attack-duration* 100e f* env1dur f/ 1e
    100e *clm-db-drop-per-second* env1dur f* db>linear
    8 >vct { ampfun1 }
    :envelope ampfun1 :scaler amp :duration env1dur :base 10000e make-env { ampenv1 }
    :envelope vct[ 0e 1e 100e 0e ] :scaler amp ampfun1 vct-last f*
    :duration env1dur :base 1e make-env { ampenv2 }
    parts length 2/ make-array map
	i 2* 1+ parts vct@ i alist vct!
	:frequency i 2* parts vct@ freq f* make-oscil
    end-map { oscils }
    90e random :locsig-degree
    start dur run-instrument
	0e oscils each oscil-0 i alist vct@ f* f+ end-each
	i env1samples > if ampenv2 else ampenv1 then env f*
    end-run
    ampenv1 gen-free
    ampenv2 gen-free
    oscils gen-free
    parts gen-free
    ampfun1 gen-free
;instrument
' lbj-piano $" lbj-piano ( f: start f: dur f: freq f: amp -- )" help!

event: lbj-piano-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 440e 0.5e lbj-piano
    dur wait
;event

$" cosamp"     dup constant :cosamp   float-keyword!
$" cosfreq1"   dup constant :cosfreq1 float-keyword!
$" cosfreq0"   dup constant :cosfreq0 float-keyword!
$" ranfreq"    dup constant :ranfreq  float-keyword!
$" noiamp"     dup constant :noiamp   float-keyword!
$" freq1"      dup constant :freq1    float-keyword!
$" r1"         dup constant :r1	      float-keyword!
$" g1"         dup constant :g1	      float-keyword!
$" freq2"      dup constant :freq2    float-keyword!
$" r2"         dup constant :r2	      float-keyword!
$" g2"         dup constant :g2	      float-keyword!
$" freq3"      dup constant :freq3    float-keyword!
$" r3"         dup constant :r3	      float-keyword!
$" g3"         dup constant :g3       float-keyword!
$" cosnum"     constant :cosnum
$" ampcosfun"  constant :ampcosfun
$" freqcosfun" constant :freqcosfun
$" noifun"     constant :noifun
$" driver"     constant :driver

\ RESFLT
\ clm-3/resflt.ins
instrument: resflt ( keyword-args -- )
    0e         :beg        get-fargs { f: start }
    1e         :dur        get-fargs { f: dur }
    false      :driver     get-args  { driver }
    10000e     :ranfreq    get-fargs { f: ranfreq }
    0.01e      :noiamp     get-fargs { f: noiamp }
    false      :noifun     get-args  { noif }
    0.1e       :cosamp     get-fargs { f: cosamp }
    200e       :cosfreq1   get-fargs { f: cosfreq1 }
    230e       :cosfreq0   get-fargs { f: cosfreq0 }
    10         :cosnum     get-args  { cosnum }
    false      :ampcosfun  get-args  { ampcosf }
    false      :freqcosfun get-args  { freqcosf }
    550e       :freq1      get-fargs { f: freq1 }
    0.995e     :r1         get-fargs { f: r1 }
    0.1e       :g1         get-fargs { f: g1 }
    1000e      :freq2      get-fargs { f: freq2 }
    0.995e     :r2         get-fargs { f: r2 }
    0.1e       :g2         get-fargs { f: g2 }
    2000e      :freq3      get-fargs { f: freq3 }
    0.995e     :r3         get-fargs { f: r3 }
    0.1e       :g3         get-fargs { f: g3 }
    90e random :degree     get-fargs { f: loc-degr }
    1e         :distance   get-fargs { f: loc-dist }
    noif     ?envelope if false else vct[ 0e 0e 50e 1e 100e 0e ] to noif true     then { noi-del }
    ampcosf  ?envelope if false else vct[ 0e 0e 50e 1e 100e 0e ] to ampcosf true  then { amp-del }
    freqcosf ?envelope if false else vct[ 0e 0e 100e 1e ]        to freqcosf true then { frq-del }
    r1 freq1 make-ppolar { f1 }
    r2 freq2 make-ppolar { f2 }
    r3 freq3 make-ppolar { f3 }
    driver if
	false
    else
	:envelope freqcosf :scaler cosfreq1 cosfreq0 f- hz>radians :duration dur make-env
    then { frqf }
    driver if
	:envelope noif :scaler noiamp :duration dur make-env
    else
	:envelope ampcosf :scaler cosamp :duration dur make-env
    then { ampf }
    driver if
	:frequency ranfreq make-rand
    else
	:frequency cosfreq0 :cosines cosnum make-sum-of-cosines
    then { gen }
    loc-degr :locsig-degree
    loc-dist :locsig-distance
    start dur run-instrument
	driver if
	    0e 0e ( rand )
	else
	    frqf env 0e ( sum-of-cosines )
	then gen mus-run ampf env f* { f: input }
	input g1 f* f1 two-pole
	input g2 f* f2 two-pole f+
	input g3 f* f3 two-pole f+
    end-run
    f1 gen-free
    f2 gen-free
    f3 gen-free
    frqf ?env if frqf gen-free then
    ampf gen-free
    gen gen-free
    amp-del if ampcosf gen-free then
    frq-del if freqcosf gen-free then
    noi-del if noif gen-free then
;instrument
' resflt $" resflt ( keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :beg        -- 0e (secs)\n" $+
$" \\ :dur        -- 1e (secs)\n" $+
$" \\ :driver     -- false (true: sum of cosines, false: white noise)" $+
$" \\ :ranfreq    -- 10000e\n" $+
$" \\ :noiamp     -- 0.01e\n" $+
$" \\ :noifun     -- vct[ 0e 0e 50e 1e 100e 0e ]\n" $+
$" \\ :cosamp     -- 0.1e\n" $+
$" \\ :cosfreq1   -- 200e\n" $+
$" \\ :cosfreq0   -- 230e\n" $+
$" \\ :cosnum     -- 10\n" $+
$" \\ :ampcosfun  -- vct[ 0e 0e 50e 1e 100e 0e ]\n" $+
$" \\ :freqcosfun -- vct[ 0e 0e 100e 1e ]\n" $+
$" \\ :freq1      -- 550e\n" $+
$" \\ :r1         -- 0.995e\n" $+
$" \\ :g1         -- 0.1e\n" $+
$" \\ :freq2      -- 1000e\n" $+
$" \\ :r2         -- 0.995e\n" $+
$" \\ :g2         -- 0.1e\n" $+
$" \\ :freq3      -- 2000e\n" $+
$" \\ :r3         -- 0.995e\n" $+
$" \\ :g3         -- 0.1e\n" $+
$" \\ :degree     -- 90e random (locsig-degree)\n" $+
$" \\ :distance   -- 1e (locsig-distance)\n" $+
$" ' resflt with-sound" $+ help!

event: resflt-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start :beg dur :dur false :driver resflt
    start 0.5e f+ :beg dur :dur true :driver resflt
    dur 0.5e f+ wait
;event

: scratch-input-cb { rd samp -- xt; dir self -- r }
    lambda-create rd , samp , latestxt
  does> { dir self -- r }
    self @ { f }
    self cell+ @ @ { w^ samp }
    samp @ 0 f file>sample
    samp @ dir + self cell+ @ !
;

\ SCRATCH-INS
instrument: scratch-ins { f: start file f: src-ratio turntable -- }
    file sound-duration { f: dur }
    file open-input { f }
    turntable vct-first seconds>samples { w^ cur-samp }
    turntable vct-second seconds>samples { turn-samp }
    :input f cur-samp scratch-input-cb :srate src-ratio make-src { rd }
    src-ratio f0> { forwards }
    forwards turn-samp cur-samp @ < and if src-ratio fnegate rd increment! then
    1 { turn-i }
    0 { turning }
    0e 0e { f: last-val1 f: last-val2 }
    90e random :locsig-degree
    start dur run-instrument
	turn-i turntable length >= if leave then
	0e rd src { f: val }
	turning unless
	    forwards cur-samp @ turn-samp >= and if
		1
	    else
		forwards 0= cur-samp @ turn-samp <= and
		if
		    -1
		else
		    turning
		then
	    then to turning
	else
	    last-val2 last-val1 f<= last-val1 val f>= and
	    last-val2 last-val1 f>= last-val1 val f<= and or if
		turn-i 1+ to turn-i
		turn-i turntable length < if
		    turn-i turntable vct@ seconds>samples to turn-samp
		    forwards negate to forwards
		    rd increment@ fnegate rd increment!
		then
		0 to turning
	    then
	then
	last-val1 to last-val2
	val to last-val1
	val
    end-run
    f close-input
    rd gen-free
;instrument
' scratch-ins $" scratch-ins ( f: start file :src-ratio turntable -- )" help!

event: scratch-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start $" fyow.snd" 1.5e vct[ 0e 0.5e 0.25e 1e ] scratch-ins
    $" fyow.snd" sound-duration wait
;event

struct
    cell%   field fdr
    cell%   field fdi
    cell%   field win
    cell%   field fftamps
    cell%   field current-peak-freqs
    cell%   field last-peak-freqs
    cell%   field current-peak-amps
    cell%   field last-peak-amps
    cell%   field peak-amps
    cell%   field peak-freqs
    cell%   field resynth-oscils
    cell%   field ampls
    cell%   field rates
    cell%   field freqs
    cell%   field sweeps
    cell%   field hop
    cell%   field outhop
    cell%   field cur-oscils
    cell%   field ramped
    cell%   field splice-attack
    cell%   field ramp-ind
    cell%   field ramped-attack
    cell%   field trigger
    sfloat% field ifreq
    sfloat% field ihifreq
    sfloat% field fft-mag
    sfloat% field attack-size
end-struct pins%

$" transposition" dup constant :transposition float-keyword!
$" time-scaler"   dup constant :time-scaler   float-keyword!
$" fftsize"       constant :fftsize
$" highest-bin"   constant :highest-bin
$" max-peaks"     constant :max-peaks
$" attack"        constant :attack

\ PINS
\
\ spectral modeling (SMS)
instrument: pins ( file start dur keyword-args -- )
    0.5e :amplitude     get-fargs { f: amp }
    1e   :transposition get-fargs { f: transposition }
    1e   :time-scaler   get-fargs { f: time-scaler }
    256  :fftsize       get-args  { fftsize }
    128  :highest-bin   get-args  { highest-bin }
    16   :max-peaks     get-args  { max-peaks }
    nil  :attack        get-args  { attack }
    { f: start f: dur file }
    dur time-scaler f/ file sound-duration f> if
	file $"  is " $+ file sound-duration 3 $(f.r) $+ $"  seconds long, but we'll need " $+
	dur time-scaler f/ 3 $(f.r) $+ $"  seconds of data for this note" $+ error
    then
    pins% %alloc { ps }
    file open-input { fil }
    fftsize make-vct                                ps fdr !
    fftsize make-vct                                ps fdi !
    mus-blackman2-window fftsize 0e make-fft-window ps win !
    fftsize make-vct                                ps fftamps !
    max-peaks 2* { max-oscils }
    max-oscils make-vct                             ps current-peak-freqs !
    max-oscils make-vct                             ps last-peak-freqs !
    max-oscils make-vct                             ps current-peak-amps !
    max-oscils make-vct                             ps last-peak-amps !
    max-peaks  make-vct                             ps peak-amps !
    max-peaks  make-vct                             ps peak-freqs !
    max-oscils make-array map :frequency 0e make-oscil end-map ps resynth-oscils !
    max-oscils make-vct                             ps ampls !
    max-oscils make-vct                             ps rates !
    max-oscils make-vct                             ps freqs !
    max-oscils make-vct                             ps sweeps !
    fftsize s>f 4e f/ floor f>s                     ps hop !
    time-scaler ps hop @ s>f f* floor f>s           ps outhop !
    ps outhop @ s>f 1/f                             ps ifreq sf!
    ps ifreq sf@ hz>radians                         ps ihifreq sf!
    mus-srate@ fftsize s>f f/                       ps fft-mag sf!
    max-oscils                                      ps cur-oscils !
    attack if attack else 0 then                    ps ramped !
    attack if true else false then                  ps splice-attack !
    attack if attack else 1 then                    ps attack-size !
    0                                               ps ramp-ind !
    ps attack-size @ make-vct                       ps ramped-attack !
    ps outhop @                                     ps trigger !
    fftsize s>f 0.42323e f* 1/f ps win @ vct-scale!
    0 { filptr }
    90e random :locsig-degree
    start dur run-instrument
	ps splice-attack @ if
	    ps attack-size @ s>f 1/f { f: ramp }
	    filptr 0 fil file>sample amp f* ( outval )
	    filptr 1+ to filptr
	    filptr ps attack-size @ > if
		1e { f: mult }
		ps ramped-attack @ each
		    fdrop
		    filptr i + 0 fil file>sample mult f* i ps ramped-attack @ vct!
		    mult ramp f- to mult
		end-each
		false ps splice-attack !
	    then
	    ( outval )
	else
	    ps trigger @ ps outhop @ >= if
		0 { peaks }
		0 ps trigger !
		ps fdr @ map filptr i + 0 fil file>sample i ps win @ vct@ f* end-map drop
		filptr ps fdr @ length + to filptr
		ps fdi @ vct-clear
		filptr fftsize ps hop @ - - to filptr
		ps fdr @ ps fdi @ fftsize 1 fft
		highest-bin 0 ?do
		    i ps fdr @ vct@ fdup f* i ps fdi @ vct@ fdup f* f+ fsqrt f2* i ps fftamps @ vct!
		loop
		ps current-peak-freqs @ data-ptr ps last-peak-freqs @ data-ptr
		max-oscils sfloats move
		ps current-peak-amps @ data-ptr ps last-peak-amps @ data-ptr
		max-oscils sfloats move
		ps current-peak-amps @ vct-clear
		ps peak-amps @ vct-clear
		ps fftamps @ vct-first { f: ra }
		0e 0e { f: la f: ca }
		highest-bin 0 ?do
		    ca to la
		    ra to ca
		    i ps fftamps @ vct@ to ra
		    ca 0.001e f> ca ra f> and ca la f> and if
			la flog ra flog f- f2/ la flog -2e ca flog f* f+ ra flog f+ f/ { f: offset }
			10e ca flog 0.25e la flog ra flog f- f* offset f* f- f** { f: amp-1 }
			ps fft-mag sf@ i s>f offset 1e f- f+ f* { f: freq }
			peaks max-peaks = if
			    0 { minp }
			    ps peak-amps @ vct-first { f: minpeak }
			    max-peaks 1 ?do
				i ps peak-amps @ vct@ minpeak f< if
				    i to minp
				    i ps peak-amps @ vct@ to minpeak
				then
			    loop
			    amp-1 minpeak f> if
				freq minp ps peak-freqs @ vct!
				amp-1 minp ps peak-amps @ vct!
			    then
			else
			    freq peaks ps peak-freqs @ vct!
			    amp-1 peaks ps peak-amps @ vct!
			    peaks 1+ to peaks
			then
		    then
		loop
		peaks 0 ?do
		    0 { maxp }
		    ps peak-amps @ vct-first ( maxpk )
		    max-peaks 1 ?do
			i ps peak-amps @ vct@ fover f> if
			    i to maxp
			    fdrop ( maxpk )
			    i ps peak-amps @ vct@ ( maxpk )
			then
		    loop
		    ( maxpk ) f0> if
			-1 { closestp }
			10e { f: closestamp }
			maxp ps peak-freqs @ vct@ { f: cur-freq }
			cur-freq 1/f { f: icf }
			max-peaks 0 ?do
			    i ps last-peak-amps @ vct@ f0> if
				icf i ps last-peak-freqs @ vct@ cur-freq f- fabs f* { f: closeness }
				closeness closestamp f< if
				    closeness to closestamp
				    i to closestp
				then
			    then
			loop
			closestamp 0.1e f< if
			    maxp ps peak-amps @ vct@ closestp ps current-peak-amps @ vct!
			    0e maxp ps peak-amps @ vct!
			    cur-freq closestp ps current-peak-freqs @ vct!
			then
		    then
		loop
		max-peaks 0 ?do
		    i ps peak-amps @ vct@ f0> if
			-1 { new-place }
			max-oscils 0 ?do
			    i ps last-peak-amps @ vct@ f0= i ps current-peak-amps @ vct@ f0= and if
				i to new-place
				leave
			    then
			loop
			i ps peak-amps @ vct@ new-place ps current-peak-amps @ vct!
			0e i ps peak-amps @ vct!
			i ps peak-freqs @ vct@ new-place ps current-peak-freqs @ vct!
			i ps peak-freqs @ vct@ new-place ps last-peak-freqs @ vct!
			transposition i ps peak-freqs @ vct@ f* new-place ps resynth-oscils @ array@
			frequency!
		    then
		loop
		0 ps cur-oscils !
		max-oscils 0 ?do
		    i ps current-peak-amps @ vct@ i ps last-peak-amps @ vct@ f- ps ifreq sf@ f*
		    i ps rates @ vct!
		    i ps current-peak-amps @ vct@ f0<> i ps last-peak-amps @ vct@ f0<> or if
			i ps cur-oscils !
		    then
		    i ps current-peak-freqs @ vct@ i ps last-peak-freqs @ vct@ f- transposition f*
		    ps ihifreq sf@ f* i ps sweeps @ vct!
		loop
		1 ps cur-oscils +!
	    then
	    1 ps trigger +!
	    ps ramped @ unless
		0e ( sum )
	    else
		ps ramp-ind @ ps ramped-attack @ vct@ ( sum )
		1 ps ramp-ind +!
		ps ramp-ind @ ps ramped @ = if 0 ps ramp-ind ! then
	    then ( sum )
	    ps cur-oscils @ 0 ?do
		i ps ampls @ vct@ f0<> i ps rates @ vct@ f0<> or if
		    i ps freqs @ vct@ 0e i ps resynth-oscils @ array@ oscil
		    i ps ampls @ vct@ f* f+ ( sum += ... )
		    i ps rates @ vct@ i ps ampls @ vct+!
		    i ps sweeps @ vct@ i ps freqs @ vct+!
		then
	    loop
	    amp ( sum ) f*
	then
    end-run
    ps fdr @ gen-free
    ps fdi @ gen-free
    ps win @ gen-free
    ps fftamps @ gen-free
    ps current-peak-freqs @ gen-free
    ps last-peak-freqs @ gen-free
    ps current-peak-amps @ gen-free
    ps last-peak-amps @ gen-free
    ps peak-freqs @ gen-free
    ps peak-amps @ gen-free
    ps resynth-oscils @ gen-free
    ps ampls @ gen-free
    ps rates @ gen-free
    ps freqs @ gen-free
    ps sweeps @ gen-free
    ps ramped-attack @ gen-free
    ps free throw
;instrument
' pins $" pins ( file f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :amplitude     -- 0.5e\n" $+
$" \\ :transposition -- 1e\n" $+
$" \\ :time-scaler   -- 1e\n" $+
$" \\ :fftsize       -- 256\n" $+
$" \\ :highest-bin   -- 128\n" $+
$" \\ :max-peaks     -- 16\n" $+
$" \\ :attack        -- nil\n" $+
$" $\" fyow.snd\" start dur :amplitude 1e :time-scaler 2e pins" $+ help!

event: pins-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    $" fyow.snd" start dur :amplitude 1e :time-scaler 2e pins
    dur wait
;event

\ ZC
instrument: zc { f: start f: dur f: freq f: amp len1 len2 f: feedback -- }
    :frequency freq make-pulse-train { s }
    :size len1 :scaler feedback :max-size len1 len2 max 1+ make-comb { d0 }
    :envelope vct[ 0e 0e 1e 1e ] :scaler len2 len1 - s>f :duration dur make-env { zenv }
    90e random :locsig-degree
    start dur run-instrument 0e s pulse-train amp f*  zenv env  d0  comb end-run
    s gen-free
    d0 gen-free
    zenv gen-free
;instrument
' zc $" zc ( f: start f: dur f: freq f: amp len1 len2 f: feedback -- )" help!

event: zc-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start                dur 100e 0.4e 20 100 0.95e zc
    start dur 0.5e f+ f+ dur 100e 0.4e 100 20 0.95e zc
    dur f2* 0.5e f+ wait
;event

\ ZN
\
\ notches are spaced at srate/len, feedforward sets depth thereof so
\ sweep of len from 20 to 100 sweeps the notches down from 1000 Hz to
\ ca 200 Hz so we hear our downward glissando beneath the pulses.
instrument: zn { f: start f: dur f: freq f: amp len1 len2 f: feedforward -- }
    :frequency freq make-pulse-train { s }
    :size len1 :scaler feedforward :max-size len1 len2 max 1+ make-notch { d0 }
    :envelope vct[ 0e 0e 1e 1e ] :scaler len2 len1 - s>f :duration dur make-env { zenv }
    90e random :locsig-degree
    start dur run-instrument 0e s pulse-train amp f*  zenv env  d0  notch end-run
    s gen-free
    d0 gen-free
    zenv gen-free
;instrument
' zn $" zn ( f: start f: dur f: freq f: amp len1 len2 f: feedforward -- )" help!

event: zn-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start                dur 100e 0.5e 20 100 0.95e zn
    start dur 0.5e f+ f+ dur 100e 0.5e 100 20 0.95e zn
    dur f2* 0.5e f+ wait
;event

\ ZA
instrument: za { f: start f: dur f: freq f: amp len1 len2 f: fb f: ffw -- }
    :frequency freq make-pulse-train { s }
    :size len1 :feedback fb :feedforward ffw :max-size len1 len2 max 1+ make-all-pass { d0 }
    :envelope vct[ 0e 0e 1e 1e ] :scaler len2 len1 - s>f :duration dur make-env { zenv }
    90e random :locsig-degree
    start dur run-instrument 0e s pulse-train amp f*  zenv env  d0  all-pass end-run
    s gen-free
    d0 gen-free
    zenv gen-free
;instrument
' za $" za ( f: start f: dur f: freq f: amp len1 len2 f: feedback f: feedforward -- )" help!

event: za-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start                dur 100e 0.3e 20 100 0.95e 0.95e za
    start dur 0.5e f+ f+ dur 100e 0.3e 100 20 0.95e 0.95e za
    dur f2* 0.5e f+ wait
;event

$" exp-env"  constant :exp-env
$" seg-env"  constant :seg-env
$" sr-env"   constant :sr-env
$" ramp-env" constant :ramp-env
$" hop-env"  constant :hop-env

\ EXP-SND
\
\ granulate with envelopes on the expansion amount, segment envelope
\ shape, segment length, hop length, and input file resampling rate
instrument: exp-snd ( file start dur keyword-args -- )
    1e    :amplitude get-fargs { f: amp }
    nil   :exp-env   get-args  { exp-env }
    nil   :ramp-env  get-args  { ramp-env }
    nil   :seg-env   get-args  { seg-env }
    nil   :sr-env    get-args  { sr-env }
    nil   :hop-env   get-args  { hop-env }
    nil   :amp-env   get-args  { amp-env }
    { file f: start f: dur }
    exp-env  ?envelope if false else true vct[ 0e 1e 1e 1e ]         to exp-env  then { del-exp }
    ramp-env ?envelope if false else true vct[ 0e 0.4e 1e 0.4e ]     to ramp-env then { del-ramp }
    seg-env  ?envelope if false else true vct[ 0e 0.15e 1e 0.15e ]   to seg-env  then { del-seg }
    sr-env   ?envelope if false else true vct[ 0e 1e 1e 1e ]         to sr-env   then { del-sr }
    hop-env  ?envelope if false else true vct[ 0e 0.05e 1e 0.05e ]   to hop-env  then { del-hop }
    amp-env  ?envelope if false else true vct[ 0e 0e 0.5e 1e 1e 0e ] to amp-env  then { del-amp }
    ramp-env min-envelope f0<= ramp-env max-envelope 0.5e f>= or if
	$" ramp argument to expand must always be between 0.0e and 0.5e: "
	ramp-env min-envelope 3 $(f.r) $+ $" --" $+ ramp-env max-envelope 3 $(f.r) $+ error
    then
    :envelope exp-env  :duration dur make-env { expenv }
    :envelope seg-env  :duration dur make-env { lenenv }
    :envelope sr-env   :duration dur make-env { srenv }
    :envelope ramp-env :duration dur make-env { rampenv }
    :envelope hop-env  :duration dur make-env { hopenv }
    :envelope amp-env  :duration dur :scaler amp make-env { ampf }
    seg-env max-envelope { f: max-seg-len }
    hop-env max-envelope { f: max-out-hop }
    max-out-hop exp-env min-envelope f/ { f: max-in-hop }
    file open-input { fil }
    :input fil
    :expansion exp-env vct-second
    :ramp ramp-env vct-second
    :hop hop-env vct-second
    :max-size mus-srate@ max-out-hop max-in-hop fmax max-seg-len f+ f* fceil f>s
    :scaler max-seg-len 0.15e f> if 0.6e 0.15e f* max-seg-len f/ else 0.6e then
    :length seg-env vct-second make-granulate { ex-a }
    0e 0e { f: ex-samp f: next-samp }
    ampf env { f: vol }
    ex-a granulate vol f* { f: val-a0 }
    ex-a granulate vol f* { f: val-a1 }
    90e random :locsig-degree
    start dur run-instrument
	lenenv env mus-srate@ f* floor { f: sl }
	ampf env to vol
	sl f>s ex-a length!
	rampenv env sl f* floor f>s ex-a ramp!
	hopenv env ex-a frequency!
	expenv env ex-a increment!
	next-samp srenv env f+ to next-samp
	next-samp ex-samp 1e f+ f> if
	    next-samp ex-samp f- floor f>s 0 ?do
		val-a1 to val-a0
		ex-a granulate vol f* to val-a1
		ex-samp 1e f+ to ex-samp
	    loop
	then
	next-samp ex-samp f= if val-a0 else next-samp ex-samp f- val-a1 val-a0 f- f* val-a0 f+ then
    end-run
    fil     close-input
    ex-a    gen-free
    expenv  gen-free
    lenenv  gen-free
    srenv   gen-free
    rampenv gen-free
    hopenv  gen-free
    ampf    gen-free
    del-exp  if exp-env gen-free  then
    del-seg  if seg-env gen-free  then
    del-sr   if sr-env gen-free   then
    del-ramp if ramp-env gen-free then
    del-hop  if hop-env gen-free  then
;instrument
' exp-snd $" exp-snd ( file f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :amplitude -- 1e\n" $+
$" \\ :exp-env   -- vct[ 0e 1e 1e 1e ]\n" $+
$" \\ :ramp-env  -- vct[ 0e 0.4e 1e 0.4e ]\n" $+
$" \\ :seg-env   -- vct[ 0e 0.15e 1e 0.15e ]\n" $+
$" \\ :sr-env    -- vct[ 0e 1e 1e 1e ]\n" $+
$" \\ :hop-env   -- vct[ 0e 0.05e 1e 0.05e ]\n" $+
$" $\" fyow.snd\" 0e 2e :exp-env vct[ 0e 1e 1e 3e ] :sr-env vct[ 0e 2e 1e 0.5e ] exp-snd" $+ help!

event: exp-snd-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    $" fyow.snd" start dur :exp-env vct[ 0e 1e 1e 3e ] :sr-env vct[ 0e 2e 1e 0.5e ] exp-snd
    start dur 0.2e f+ f+ to start
    $" oboe.snd" start dur
    :exp-env vct[ 0e 1e 1e 3e ] :sr-env vct[ 0e 2e 1e 0.5e ] :hop-env vct[ 0e 0.2e 1e 0.2e ] exp-snd
    dur f2* 0.2e f+ wait
;event

struct
    sfloat% field exp-rampval
    sfloat% field exp-rampinc
    cell%   field exp-loc
    cell%   field exp-segctr
    cell%   field exp-whichseg
    cell%   field exp-ramplen
    cell%   field exp-steadylen
    cell%   field exp-trigger
end-struct grn%

\ EXPFIL
instrument: expfil { f: start f: dur f: hopsecs f: rampsecs f: steadysecs file1 file2 -- }
    rampsecs seconds>samples { ramplen }
    grn% %alloc { grn1 }
    grn% %alloc { grn2 }
    0e 0e                          grn1 exp-rampval sf! grn2 exp-rampval sf!
    ramplen s>f 1/f fdup           grn1 exp-rampinc sf! grn2 exp-rampinc sf!
    0 0                            grn1 exp-loc !       grn2 exp-loc !
    0 0                            grn1 exp-segctr !    grn2 exp-segctr !
    0 0                            grn1 exp-whichseg !  grn2 exp-whichseg !
    ramplen dup                    grn1 exp-ramplen !   grn2 exp-ramplen !
    steadysecs seconds>samples dup grn1 exp-steadylen ! grn2 exp-steadylen !
    0 0                            grn1 exp-trigger !   grn2 exp-trigger !
    hopsecs seconds>samples { hop }
    start seconds>samples { out1 }
    hop out1 + { out2 }
    file1 open-input { fil1 }
    file2 open-input { fil2 }
    90e random :locsig-degree
    start dur run-instrument
	0e ( val )
	i out1 = if
	    grn1 exp-loc @ 0 fil1 file>sample { f: inval }
	    1 grn1 exp-loc +!
	    grn1 exp-whichseg @ case
		0 of
		    grn1 exp-rampval sf@ inval f* to inval
		    grn1 exp-rampinc sf@ grn1 exp-rampval sf+!
		    1 grn1 exp-segctr +!
		    grn1 exp-segctr @ grn1 exp-ramplen @ = if
			0 grn1 exp-segctr !
			1 grn1 exp-whichseg +!
		    then
		endof
		1 of
		    1 grn1 exp-segctr +!
		    grn1 exp-segctr @ grn1 exp-steadylen @ = if
			0 grn1 exp-segctr !
			1 grn1 exp-whichseg +!
		    then
		endof
		grn1 exp-rampval sf@ inval f* to inval
		1 grn1 exp-segctr +!
		grn1 exp-rampinc sf@ fnegate grn1 exp-rampval sf+!
		grn1 exp-segctr @ grn1 exp-ramplen @ = if
		    0 grn1 exp-segctr !
		    1 grn1 exp-trigger !
		    0 grn1 exp-whichseg !
		    0e grn1 exp-rampval sf!
		then
	    endcase
	    inval f+ ( val )
	    out1 1+ to out1
	    grn1 exp-trigger @ 1 = if
		0 grn1 exp-trigger !
		hop out1 + to out1
	    then
	then
	i out2 = if
	    grn2 exp-loc @ 0 fil2 file>sample { f: inval }
	    1 grn2 exp-loc +!
	    grn2 exp-whichseg @ case
		0 of
		    grn2 exp-rampval sf@ inval f* to inval
		    grn2 exp-rampinc sf@ grn2 exp-rampval sf+!
		    1 grn2 exp-segctr +!
		    grn2 exp-segctr @ grn2 exp-ramplen @ = if
			0 grn2 exp-segctr !
			1 grn2 exp-whichseg +!
		    then
		endof
		1 of
		    1 grn2 exp-segctr +!
		    grn2 exp-segctr @ grn2 exp-steadylen @ = if
			0 grn2 exp-segctr !
			1 grn2 exp-whichseg +!
		    then
		endof
		grn2 exp-rampval sf@ inval f* to inval
		1 grn2 exp-segctr +!
		grn2 exp-rampinc sf@ fnegate grn2 exp-rampval sf+!
		grn2 exp-segctr @ grn2 exp-ramplen @ = if
		    0 grn2 exp-segctr !
		    1 grn2 exp-trigger !
		    0 grn2 exp-whichseg !
		    0e grn2 exp-rampval sf!
		then
	    endcase
	    inval f+ ( val )
	    out2 1+ to out2
	    grn2 exp-trigger @ 1 = if
		0 grn2 exp-trigger !
		hop out2 + to out2
	    then
	then
    end-run
    fil1 close-input
    fil2 close-input
    grn1 free throw
    grn2 free throw
;instrument
' expfil $" expfil ( f: start f: dur f: hopsecs f: rampsecs f: steadysecs file1 file2 -- )" help!

event: expfil-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    start dur 0.2e 0.01e 0.1e $" oboe.snd" $" fyow.snd" expfil
    dur wait
;event

\ GRAPH-EQ
\
\ From: Marco Trevisani <marco@ccrma.Stanford.EDU>
\ 
\ This should work like a Graphic Equalizer....
\ Very easy to use. Just some note:
\ 
\ "amp" & "amp-env" apply an enveloppe to the final result of the
\ filtering.  
\ 
\ "dur" as ""standard"" in my instruments, when dur = 0 it will take the length of the
\ sndfile input, otherwise the duration in seconds.
\ 
\ "gain-freq-list" is a list of gains and frequencies to
\ filter --in this order gain and frequencies--. There is no limit to
\ the size of the list. Gain can be a number or an
\ envelope. Unfortunatelly in this version they cant alternate, one
\ should chose, all envelopes or all numbers i.e.: 
\ case 1 -> '( .1 440.0 .3 1500.0 .2 330.0 ...etc) or 
\ case 2 -> '((0 .1 1 .5) 440.0 (0 1 1 .01) 1500 (0 .3 1 .5) 330.0 ...etc) 
\ '( .1 440.0 (0 1 1 .01) 1500 ..etc) <<< again, this is not allowed ..
\ 
\ "offset-gain" This apply to all the gains if case 1. It adds or
\ subtracts an offset to all the gains in the list. This number can be positive or
\ negative. In case the result is a negative number --let's say offset =
\ -.4 and, like in case 1, the first gain is .1, the result would be
\ -.3 -- the instrument will pass a gain equal to 0.  
\ 
\ "filt-gain-scale" & "filt-gain-base" will apply to the elements of the
\ envelopes if we are in case 2, gains are envelopes.
\ 
\ "stats" if #t --default-- prints the number of seconds processed, if
\ nil doesnt print anything, which will speed up a bit the process.

$" file-start"      dup constant :file-start      float-keyword!
$" filt-gain-scale" dup constant :filt-gain-scale float-keyword!
$" filt-gain-base"  dup constant :filt-gain-base  float-keyword!
$" gain-freq-list"  constant :gain-freq-list

instrument: graph-eq ( file start dur keyword-args -- )
    0e    :file-start  	   get-fargs { f: or-beg }
    1e    :amplitude   	   get-fargs { f: amp }
    nil   :amp-env     	   get-args  { amp-env }
    1e    :base        	   get-fargs { f: base }
    nil   :gain-freq-list  get-args  { gain-freq-list }
    1e    :filt-gain-scale get-fargs { f: filt-gain-scale }
    1e    :filt-gain-base  get-fargs { f: filt-gain-base }
    0.99e :a1              get-fargs { f: a1 }
    { file f: start f: dur }
    amp-env ?envelope if false else true vct[ 0e 1e 0.8e 1e 1e 0e ] to amp-env then { del-amp }
    gain-freq-list ?array if
	false
    else
	true
	vct[ 0e 1e 1e 0e ] 440 vct[ 0e 0e 1e 1e ] 660  4 >array to gain-freq-list
    then { del-gain }
    :file file :start file sound-srate s>f or-beg f* fround f>s make-readin { rd }
    :envelope amp-env :scaler amp :duration dur :base base make-env { ampf }
    gain-freq-list length 2/ { len }
    len make-array { gainl }
    len make-vct { freql }
    0 { idx }
    gain-freq-list length 1- 0 do
	i gain-freq-list array@ idx gainl array!
	i 1+ gain-freq-list array@ s>f idx freql vct!
	idx 1+ to idx
    2 +loop
    len make-array map
	:envelope i gainl array@ :scaler filt-gain-scale :duration dur :base filt-gain-base make-env
    end-map { env-size }
    len make-array map :radius a1 :frequency i freql vct@ make-formant end-map { frm-size }
    90e random :locsig-degree
    start dur run-instrument
	rd readin ( inval )
	0e ( outval )
	env-size each
	    env 1e a1 f- f* i frm-size array@ ( w: fmt ) 0 over xcoeff!
	    fover ( f: inval ) ( w: fmt ) formant f+ ( f: outval )
	end-each
	fnip
	ampf env f* ( outval )
    end-run
    rd mus-close
    del-amp if amp-env gen-free then
    del-gain if gain-freq-list gen-free then
    gainl gen-free
    freql gen-free
    env-size gen-free
    frm-size gen-free
    ampf gen-free
;instrument
' graph-eq $" graph-eq ( file f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :file-start      -- 0e (secs)\n" $+
$" \\ :amplitude       -- 1e\n" $+
$" \\ :amp-env         -- vct[ 0e 1e 0.8e 1e 1e 0e ]\n" $+
$" \\ :base            -- 1e\n" $+
$" \\ :gain-freq-list  -- array[ vct[ 0e 1e 1e 0e ] 440 vct[ 0e 0e 1e 1e ] 660 ]\n" $+
$" \\ :filt-gain-scale -- 1e\n" $+
$" \\ :filt-gain-base  -- 1e\n" $+
$" \\ :a1              -- 0.99e\n" $+
$" $\" oboe.snd\" 0e 2e graph-eq" $+ help!

event: graph-eq-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    $" oboe.snd" start dur :amplitude 50e graph-eq
    dur wait
;event

\ ANOI
\ 
\ a kind of noise reduction -- on-going average spectrum is squelched
\ to some extent obviously aimed at intermittent signal in background
\ noise
\ this is based on Perry Cook's Scrubber.m
\ 
\ clm-3/anoi.ins
instrument: anoi { fname f: start f: dur fftsize f: amp-scaler f: R -- }
    fftsize s>f { f: f-fftsize }
    fftsize 2/ { freq-inc }
    fftsize make-vct { fdr }
    fftsize make-vct { fdi }
    freq-inc make-vct { spectr }
    freq-inc make-vct { scales }
    freq-inc make-vct { diffs }
    spectr 1e vct-fill!
    scales 1e vct-fill!
    mus-blackman2-window fftsize 0e make-fft-window { win }
    0e { f: amp }
    amp-scaler 4e f* mus-srate@ f/ { f: incr }
    fname open-input { fil }
    1e R f-fftsize f/ f- { f: radius }
    mus-srate@ f-fftsize f/ { f: bin }
    freq-inc make-array map :radius radius :frequency i s>f bin f* make-formant end-map { fs }
    start seconds>samples { beg }
    90e random :locsig-degree
    start dur run-instrument
	i beg - 0 fil file>sample { f: inval }
	inval i fftsize mod fdr vct!
	amp amp-scaler f< if amp incr f+ to amp then
	i beg - fftsize mod unless
	    fdr fdi win 1 spectrum
	    spectr each
		0.9e f* i fdr vct@ 0.1e f* f+ fdup i spectr vct!
		i fdr vct@ f>= if
		    i scales vct@ f-fftsize fnegate f/
		else
		    i fdr vct@ fdup i spectr vct@ f- fswap f/ i scales vct@ f- f-fftsize f/
		then i diffs vct!
	    end-each
	then
	0e ( outval )
	fs each
	    i scales vct@ { f: curscl }
	    inval formant curscl f* f+ ( outval += ... )
	    i diffs vct@ curscl f+ i scales vct!
	end-each
	amp f* ( outval )
    end-run
    fil close-input
    fs gen-free
    fdr gen-free
    fdi gen-free
    spectr gen-free
    scales gen-free
    diffs gen-free
    win gen-free
;instrument
' anoi $" anoi ( fname f: start f: dur fftsize f: amp-scaler f: R ) \\ see clm-3/anoi.ins" help!

event: anoi-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    $" fyow.snd" start dur 128 2e two-pi anoi
    dur wait
;event

\ Date: Fri, 25 Sep 1998 09:56:41 +0300
\ From: Matti Koskinen <mjkoskin@sci.fi>
\ To: linux-audio-dev@ginette.musique.umontreal.ca
\ Subject: [linux-audio-dev] Announce: alpha version of denoising
\ [...]
\ 	I wrote a simple denoiser called anoi after it's parent
\ 	clm-instrument anoi.ins.
\ 
\ 	anoi tries to remove white noise like tape hiss from wav-
\ 	files. Removing of noise succeeds ok, but depending of the
\ 	original sound, some distortion can be audible.
\ 
\ 	If someone is interested, http://www.sci.fi/~mjkoskin
\ 	contains tarred and gzipped file.
\ 
\ 	Now only monophonic wav-files can be denoised, but adding
\ 	others isn't too difficult. 
\ 
\ -matti
\ mjkoskin@sci.fi

$" matrix" constant :matrix

\ FULLMIX
\ 
\ Matrix can be a simple amplitude (0.5e float>float) or an array of arrays.
\ Each inner array represents one input channel's amps
\ into one output channel.  Each element of the list can be an amplitude (float>float),
\ an envelop (vct) or an env generator.
\ Float values must be wrapped with float>float (0.8e float>float).
: fullmix-mus-close ( gen -- n ) mus-close nil ;
instrument: fullmix ( file start dur keyword-args -- )
    0e  :beg           get-fargs { f: beg }
    nil :matrix        get-args  { matrix }
    0e  :srate         get-fargs { f: sr }
    0e  :reverb-amount get-fargs { f: rev-amount }
    { file f: start f: dur }
    file sound-chans { in-chans }
    *output* channels@ { out-chans }
    beg file sound-srate s>f f* fround f>s { inloc }
    matrix sr f0<> or if in-chans out-chans max make-mixer else nil then { mx }
    nil { rev-mx }
    *reverb* 0<> rev-amount f0> and if
	in-chans make-mixer to rev-mx
	in-chans 0 do rev-amount i 0 rev-mx mixer! loop
    then
    nil { envs }
    matrix if
	matrix ?array if
	    in-chans matrix length min 0 ?do
		i matrix array@ { inlist }
		out-chans inlist length min 0 ?do
		    i inlist array@ { outn }
		    envs unless
			in-chans make-array map out-chans make-array end-map to envs
		    then
		    outn ?env if
			outn i j envs array@ array!
		    else
			outn ?envelope if
			    :envelope outn :duration dur make-env i j envs array@ array!
			else
			    outn ?float if
				outn float@ j i mx mixer!
			    else
				$" fullmix: unknown element in matrix: " outn gen>string $+ error
			    then
			then
		    then
		loop
	    loop
	else
	    matrix ?float if
		mx channels@ 0 ?do matrix float@ i i mx mixer! loop
	    then
	then
    then
    sr f0= if
	start seconds>samples { fbeg }
	dur seconds>samples { samps }
	:infile file :outfile *output*
	:outloc fbeg :frames samps :inloc inloc :mixer mx :envs envs mus-mix drop
	rev-mx if
	    :infile file :outfile *reverb*
	    :outloc fbeg :frames samps :inloc inloc :mixer rev-mx mus-mix drop
	then
    else
	in-chans make-frame { inframe }
	*output* channels@ make-frame { outframe }
	in-chans make-array { files }
	in-chans make-array map
	    :file file :start inloc :channel i make-readin dup i files array!
	    :input swap :srate sr make-src
	end-map { srcs }
	envs ?array if
	    start dur run
		envs each
		    each dup ?env if env j i mx mixer! else drop then end-each
		end-each
		in-chans 0 do 0e i srcs array@ src i inframe frame! loop
		inframe outframe mx frame>frame i *output* frame>file
		rev-mx if inframe outframe rev-mx frame>frame i *reverb* frame>file then
	    loop
	else
	    start dur run
		in-chans 0 do 0e i srcs array@ src i inframe frame! loop
		inframe outframe mx frame>frame i *output* frame>file
		rev-mx if inframe outframe rev-mx frame>frame i *reverb* frame>file then
	    loop
	then
	inframe gen-free
	outframe gen-free
	mx if mx gen-free then
	rev-mx if rev-mx gen-free then
	['] fullmix-mus-close files array-each!
	files free-array
	srcs gen-free
    then
;instrument
' fullmix $" fullmix ( file f: start f: dur keyword-args -- )\n"
$" \\ keywords and default values\n" $+
$" \\ :beg           -- 0e (secs)\n" $+
$" \\ :matrix        -- nil\n" $+
$" \\ :srate         -- 1e\n" $+
$" \\ :reverb-amount -- 0e\n" $+
$" \\ Matrix can be a simple amplitude (0.5e float>float) or an array of arrays.\n" $+
$" \\ Each inner array represents one input channel's amps\n" $+
$" \\ into one output channel.  Each element of the list can be an amplitude (float>float)\n" $+
$" \\ an envelop (vct) or an env generator.\n" $+
$" \\ Float values must be wrapped with float>float (0.8e float>float).\n" $+
$" $\" pistol.snd\" 0e 1e fullmix\n" $+
$" 0.8e float>float\n" $+
$" :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env\n" $+
$" 2 >array 1 >array value mx\n" $+
$" $\" oboe.snd\" 0e 2e :matrix mx :srate 2e fullmix" $+ help!

event: fullmix-test ( keyword-args -- )
    0e :beg get-fargs { f: start }
    1e :dur get-fargs { f: dur }
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    8 >array
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    8 >array
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    8 >array
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    0.8e float>float
    :envelope vct[ 0e 0e 1e 1e ] :duration dur :scaler 0.5e make-env
    8 >array 4 >array { mx }

    $" pistol.snd" start dur :matrix 1.2e float>float :reverb-amount 0.04e fullmix
    start dur 0.2e f+ f+ to start
    $" oboe.snd" start dur :matrix mx :srate 0.5e :reverb-amount 0.02e fullmix

    mx gen-free
    dur f2* 0.2e f+ wait
;event

\ clm-ins.fs ends here
