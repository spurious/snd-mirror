\ csndlib.fs -- ffi to libsndlib.so -*- forth -*-

\ Copyright (C) 2003--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Wed Aug 27 22:52:21 CEST 2003
\ Last: Wed Jan 12 03:28:22 CET 2005
\ Ident: $Id: csndlib.fs,v 1.150 2005/01/12 02:28:25 mike Exp $

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

\ works with sndlib.so (clm-3)

\ FFI between Forth and C-library libsndlib.so.  This file assumes
\ that libsndlib.so was configured with float as Float (not double)
\ and --with-float-samples.

\ Code:

require utils.fs

only forth also definitions
vocabulary CLM-Sndlib
also CLM-Sndlib definitions
also Utils
also GFMusic

\ === vct.h ===
sndlib mus-set-vct-print-length int (void) set_vct_print_length ( len -- )
sndlib vct-print ptr (ptr) vct_to_string ( v -- c-str )
sndlib c-make-vct int (ptr) c_make_vct ( len -- v )
sndlib v-free-vct ptr (ptr) c_free_vct ( v -- n )
sndlib c-vct-copy ptr (ptr) c_vct_copy ( v1 -- v2 )
sndlib c-vct-equalp ptr ptr (ptr) vct_equalp ( v1 v2 -- f )

: free-vct { v -- }
    v ?vct if
	v v-orig @ v-free-vct drop
	nil v gfm-base-name !
	v free throw
    then
;
: vct= ( v1 v2 -- f ) v-orig @ swap v-orig @ c-vct-equalp ;
: $(.vct) { v -- str }
    v ?vct if
	$" vct[ " v obj-len @ 0 ?do v obj-data @ i sfloats + sf@ $(f.) $+ $" e " $+ loop $" ]" $+
    else
	$" nil"
    then
;
: .vct ( v -- ) dup ?vct if v-orig @ vct-print .cstring else drop ." #<null>" then ;
: c>forth-vct { c-vct -- v }
    vct% %alloc { v }
    c-vct              v v-orig !
    c-vct c-vct-len  @ v obj-len !
    c-vct c-vct-data @ v obj-data !
    0                  v obj-cycle !
    ['] vct-fetch      v obj-fetch !
    ['] vct-store      v obj-store !
    ['] vct-plus-store v obj-plus-store !
    c-vct c-vct-len  @ v v-buf-len !
    ['] .vct           v gfm-base-inspect !
    ['] $(.vct)        v gfm-base-to-string !
    ['] vct=           v gfm-base-equal !
    ['] free-vct       v gfm-base-free !
    str-vct            v gfm-base-name !
    v
;
: make-vct { len -- v }
    assert1( len 0>= )
    len *clm-array-buffer-length* / 1+ *clm-array-buffer-length* * c-make-vct c>forth-vct { v }
    len v obj-len !
    len v v-orig @ c-vct-len !
    v
;
: >vct ( u-float-values u -- v ) { len }
    len make-vct { v }
    len if v obj-data @ 1 sfloats - len sfloats bounds swap u-do i sf! 1 sfloats -loop then
    v obj-data @ v v-orig @ c-vct-data !
    v
;
: vct-copy ( v1 -- v2 ) v-orig @ c-vct-copy c>forth-vct ;
: c-array>vct { addr len -- v }
    0 c-make-vct { c-vct }
    len  c-vct c-vct-len !
    addr c-vct c-vct-data !
    true c-vct c-vct-dont-free !
    c-vct c>forth-vct
;

\ === sndlib2xen.h ===
sndlib sound-data>string ptr (ptr) sound_data_to_string ( sd -- c-addr )
sndlib c-sound-data-free ptr (void) sound_data_free ( sd -- )
sndlib c-sound-data-equalp ptr ptr (int) sound_data_equalp ( sd1 sd2 -- f )
sndlib c-make-sound-data int int (ptr) c_make_sound_data ( chans frames -- sd )

: sound-data-free { sd -- }
    sd ?sound-data if
	sd sd-orig @ c-sound-data-free
	nil sd gfm-base-name !
	sd free throw
    then
;
: sound-data= ( sd1 sd2 -- f ) sd-orig @ swap sd-orig @ c-sound-data-equalp ;
: $(.sound-data) ( sd -- str )
    dup ?sound-data if sd-orig @ sound-data>string $c>string else drop $" #<null>" then
;
' $(.sound-data) make-inspect .sound-data 
: make-sound-data { chns frms -- sd }
    assert1( chns 0> frms 0> and )
    chns frms c-make-sound-data { s }
    sd% %alloc { sd }
    s                   sd sd-orig !
    s c-sd-data @       sd obj-data !
    chns                sd sd-chans !
    frms                sd obj-len !
    ['] .sound-data     sd gfm-base-inspect !
    ['] $(.sound-data)  sd gfm-base-to-string !
    ['] sound-data=     sd gfm-base-equal !
    ['] sound-data-free sd gfm-base-free !
    str-sound-data      sd gfm-base-name !
    sd
;

require gfm-defs.fs

\ === clm.h ===
struct
    cell% field mus-any-type
    cell% field mus-any-name
    cell% field mus-any-release
    cell% field mus-any-describe
    cell% field mus-any-equalp
    cell% field mus-any-data
    cell% field mus-any-set-data
    cell% field mus-any-length
    cell% field mus-any-set-length
    cell% field mus-any-frequency
    cell% field mus-any-set-frequency
    cell% field mus-any-phase
    cell% field mus-any-set-phase
    cell% field mus-any-scaler
    cell% field mus-any-set-scaler
    cell% field mus-any-increment
    cell% field mus-any-set-increment
    cell% field mus-any-run
    cell% field mus-any-extended-type
    cell% field mus-any-closure
    cell% field mus-any-channels
    cell% field mus-any-offset
    cell% field mus-any-set-offset
    cell% field mus-any-width
    cell% field mus-any-set-width
    cell% field mus-any-xcoeff
    cell% field mus-any-set-xcoeff
    cell% field mus-any-hop
    cell% field mus-any-set-hop
    cell% field mus-any-ramp
    cell% field mus-any-set-ramp
    cell% field mus-any-read-sample
    cell% field mus-any-write-sample
    cell% field mus-any-file-name
    cell% field mus-any-end
    cell% field mus-any-location
    cell% field mus-any-set-location
    cell% field mus-any-channel
    cell% field mus-any-ycoeff
    cell% field mus-any-set-ycoeff
    cell% field mus-any-xcoeffs
    cell% field mus-any-ycoeffs
    cell% field mus-any-wrapper
end-struct mus-any-class%

struct
    mus-any-class% field mus-any-core
end-struct mus-any%

\ === mus_clm_extended_t (clm.h) ===
00 constant mus-not-special
01 constant mus-simple-filter
02 constant mus-full-filter
03 constant mus-output
04 constant mus-input
05 constant mus-delay-line

\ === mys_any_type (clm.c) ===
00 constant mus-oscil
01 constant mus-sum-of-cosines
02 constant mus-delay
03 constant mus-comb
04 constant mus-notch
05 constant mus-all-pass
06 constant mus-table-lookup
07 constant mus-square-wave
08 constant mus-sawtooth-wave
09 constant mus-triangle-wave
10 constant mus-pulse-train
11 constant mus-rand
12 constant mus-rand-interp
13 constant mus-asymmetric-fm
14 constant mus-one-zero
15 constant mus-one-pole
16 constant mus-two-zero
17 constant mus-two-pole
18 constant mus-formant
19 constant mus-waveshape
20 constant mus-src
21 constant mus-granulate
22 constant mus-sine-summation
23 constant mus-wave-train
24 constant mus-filter
25 constant mus-fir-filter
26 constant mus-iir-filter
27 constant mus-convolve
28 constant mus-env
29 constant mus-locsig
30 constant mus-frame
31 constant mus-readin
32 constant mus-file-to-sample
33 constant mus-file-to-frame
34 constant mus-sample-to-file
35 constant mus-frame-to-file
36 constant mus-mixer
37 constant mus-phase-vocoder
38 constant mus-average
39 constant mus-sum-of-sines
40 constant mus-ssb-am
41 constant mus-last

\ Fcomb and Buffer increment MY-MUS-LAST for correct use in ?GEN
mus-last value my-mus-last

: ?gen ( obj -- f )
    try mus-any-core @ mus-any-type @ 0 my-mus-last within recover 2drop false endtry
;

\ === clm2xen.h ===
\ WRAPPER@ returns this struct (GFM doesn't use it)
struct
  cell% field mus-xen-gen
  cell% field mus-xen-vcts
  cell% field mus-xen-nvcts
  cell% field mus-xen-dont-free-gen
  cell% field mus-xen-input-ptree
  cell% field mus-xen-edit-ptree
  cell% field mus-xen-analyze-ptree
  cell% field mus-xen-synthesize-ptree
end-struct mus-xen%

\     C-true:  1
\ Forth-true: -1
\ that's why NEGATE after EXECUTE
: make-?function ( xt "name" --; gen self -- f )
    create ,
  does> ( gen self -- f )
    try @ execute negate recover 2drop drop false endtry
;

sndlib init-mus-module (void) init_mus_module ( -- )

sndlib mus-make-class-tag (int) mus_make_class_tag ( -- tag )
sndlib radians>hz sf (sf) mus_radians_to_hz ( rads -- r )
sndlib hz>radians sf (sf) mus_hz_to_radians ( hz -- r )
sndlib degrees>radians sf (sf) mus_degrees_to_radians ( degree -- r )
sndlib radians>degrees sf (sf) mus_radians_to_degrees ( rads -- r )
sndlib db>linear sf (sf) mus_db_to_linear ( x -- r )
sndlib linear>db sf (sf) mus_linear_to_db ( x -- r )

sndlib mus-srate@ (sf) mus_srate ( -- r )
: srate@ ( -- n ) mus-srate@ f>s ;
sndlib gfm-srate! sf (sf) mus_set_srate ( val -- r )
: mus-srate! ( val -- ) fdup f>s to *srate* gfm-srate! fdrop ;
: srate! ( n -- ) dup to *srate* s>f gfm-srate! fdrop ;
sndlib mus-seconds>samples sf (llong) mus_seconds_to_samples ( secs -- d )
: seconds>samples ( secs -- n ) mus-seconds>samples d>s ;
sndlib mus-samples>seconds llong (sf) mus_samples_to_seconds ( samps -- r )
: samples>seconds ( samps -- r ) s>d mus-samples>seconds ;
sndlib array-print-length@ (int) mus_array_print_length ( -- n )
sndlib mus-array-print-length! int (int) mus_set_array_print_length ( val -- n )
: array-print-length! ( val -- )
    dup mus-array-print-length! drop
    dup mus-set-vct-print-length
    to *array-print-length*		\ .array (utils.fs)
;
sndlib mus-sine-bank ptr ptr int (sf) mus_sine_bank ( amps-ary phases-ary size -- r )
: sine-bank { amps phases -- r }
    amps data-ptr phases data-ptr amps length phases length min mus-sine-bank
;

sndlib ring-modulate sf sf (sf) mus_ring_modulate ( s1 s2 -- r )
sndlib amplitude-modulate sf sf sf (sf) mus_amplitude_modulate ( s1 s2 s3 -- r )
sndlib contrast-enhancement sf sf (sf) mus_contrast_enhancement ( sig index -- r )
sndlib mus-dot-product ptr ptr int (sf) mus_dot_product ( data1 data2 size -- r )
: dot-product { data1 data2 -- r }
    data1 data-ptr data2 data-ptr data1 length data2 length min mus-dot-product
;
sndlib mus-clear-array ptr int (void) mus_clear_array ( arr size -- )
: clear-array { ary -- } ary data-ptr ary length mus-clear-array ;
sndlib mus-polynomial ptr sf int (sf) mus_polynomial ( coeffs x ncoeffs -- r )
: polynomial ( coeffs x -- r ) dup data-ptr swap length mus-polynomial ;
sndlib mus-multiply-arrays ptr ptr int (void) mus_multiply_arrays ( data window len -- )
: multiply-arrays { rdat window -- }
    rdat data-ptr window data-ptr rdat length window length min mus-multiply-arrays
;
sndlib mus-rectangular>polar ptr ptr int (void) mus_rectangular_to_polar ( rl im size -- )
: rectangular>polar { rl im size -- }
    rl data-ptr im data-ptr rl length size min mus-rectangular>polar
;
sndlib mus-polar>rectangular ptr ptr int (void) mus_polar_to_rectangular ( rl im size -- )
: polar>rectangular { rl im size -- }
    rl data-ptr im data-ptr rl length size min mus-polar>rectangular
;
sndlib mus-array-interp ptr sf int (sf) mus_array_interp ( wave phase size -- r )
: array-interp { wave f: ph -- r } wave data-ptr ph wave length mus-array-interp ;

sndlib clm-mus-free ptr (int) mus_free ( gen -- f )
sndlib describe ptr (ptr) mus_describe ( gen -- c-addr )
: $(.clm-gen) ( gen -- str ) $" #<" swap describe $c>string $+ $" >" $+ ;
: .clm-gen ( gen -- ) $(.clm-gen) .string ;
sndlib clm-gen= ptr ptr (int) mus_equalp ( gen1 gen2 -- f )
sndlib phase@ ptr (sf) mus_phase ( gen -- r )
sndlib mus-phase! ptr sf (sf) mus_set_phase ( val gen -- r )
: phase! ( val gen -- ) mus-phase! fdrop ;
sndlib frequency@ ptr (sf) mus_frequency ( gen -- r )
sndlib mus-frequency! ptr sf (sf) mus_set_frequency ( val gen -- r )
: frequency! ( val gen -- ) mus-frequency! fdrop ;
sndlib mus-run ptr sf sf (sf) mus_run ( gen arg1 arg2 -- r )
sndlib mus-length@ ptr (llong) mus_length ( gen -- d )
: length@ ( gen -- n ) mus-length@ d>s ;
sndlib mus-length! ptr llong (llong) mus_set_length ( gen d -- d )
: length! ( n gen -- ) swap s>d mus-length! 2drop ;
sndlib mus-data@ ptr (ptr) mus_data ( gen -- w )
: data@ ( idx gen -- r ) mus-data@ c-float-array@ ;
: data>vct ( gen -- v ) dup mus-data@ swap length@ c-array>vct ;
sndlib mus-data! ptr ptr (ptr) mus_set_data ( gen data -- w )
: data! ( val idx gen -- ) mus-data@ c-float-array! ;
: vct>data ( v gen -- ) swap data-ptr mus-data! drop ;
sndlib mus-name@ ptr (ptr) mus_name ( gen -- c-addr )
: name@ ( gen -- addr len ) mus-name@ $c>string ;
sndlib scaler@ ptr (sf) mus_scaler ( gen -- r )
sndlib mus-scaler! ptr sf (sf) mus_set_scaler ( val gen -- r )
: scaler! ( val gen -- ) mus-scaler! fdrop ;
sndlib offset@ ptr (sf) mus_offset ( gen -- r )
sndlib mus-offset! ptr sf (sf) mus_set_offset ( val gen -- r )
: offset! ( val gen -- ) mus-offset! fdrop ;
sndlib width@ ptr (sf) mus_width ( gen -- r )
sndlib mus-width! ptr sf (sf) mus_set_width ( val gen -- r )
: width! ( val gen -- ) mus-width! fdrop ;
sndlib mus-file-name@ ptr (ptr) mus_file_name ( gen -- c-addr )
: file-name@ ( gen -- str ) mus-file-name@ $c>string ;
sndlib interp-type@ ptr (int) mus_channels ( gen -- n )

sndlib clm-make-oscil sf sf (ptr) mus_make_oscil ( freq phase -- gen )
sndlib ?mus-oscil ptr (int) mus_oscil_p ( gen -- f )
' ?mus-oscil make-?function ?oscil
sndlib oscil ptr sf sf (sf) mus_oscil ( fm pm gen -- r )
sndlib oscil-0 ptr (sf) mus_oscil_0 ( gen -- r )
sndlib oscil-1 ptr sf (sf) mus_oscil_1 ( fm gen -- r )

sndlib clm-make-sum-of-cosines int sf sf (ptr) mus_make_sum_of_cosines ( cosines freq phase -- gen )
sndlib ?mus-sum-of-cosines ptr (int) mus_sum_of_cosines_p ( gen -- f )
' ?mus-sum-of-cosines make-?function ?sum-of-cosines
sndlib sum-of-cosines ptr sf (sf) mus_sum_of_cosines ( fm gen -- r )
sndlib mus-cosines@ ptr (llong) mus_hop ( gen -- d )
: cosines@ ( gen -- n ) mus-cosines@ d>s ;
sndlib mus-cosines! ptr llong (llong) mus_set_hop ( val gen -- d )
: cosines! ( val gen -- ) s>d mus-cosines! 2drop ;

sndlib clm-make-sum-of-sines int sf sf (ptr) mus_make_sum_of_sines ( sines freq phase -- gen )
sndlib ?mus-sum-of-sines ptr (int) mus_sum_of_sines_p ( gen -- f )
' ?mus-sum-of-sines make-?function ?sum-of-sines
sndlib sum-of-sines ptr sf (sf) mus_sum_of_sines ( fm gen -- r )

sndlib mus-make-delay int ptr int int (ptr) mus_make_delay ( size line line-size type -- gen )
: clm-make-delay { size v tp -- gen }
    assert1( v ?vct )
    assert1( size v length <= )
    size v data-ptr v length tp mus-make-delay
;
sndlib tap ptr sf (sf) mus_tap ( loc gen -- r )
sndlib tap-1 ptr (sf) mus_tap_1 ( gen -- r )
sndlib mus-delay-tick ptr sf (sf) mus_delay_tick ( input gen -- r )
: delay-tick ( input gen -- ) mus-delay-tick fdrop ;
sndlib ?mus-delay ptr (int) mus_delay_p ( gen -- f )
' ?mus-delay make-?function ?delay
sndlib delay ptr sf sf (sf) mus_delay ( input pm gen -- r )
sndlib delay-1 ptr sf (sf) mus_delay_1 ( input gen -- r )

( scaler size line line-size type -- gen )
sndlib mus-make-comb sf int ptr int int (ptr) mus_make_comb
: clm-make-comb { f: scl size v tp -- gen }
    assert1( v ?vct )
    assert1( size v length <= )
    scl size v data-ptr v length tp mus-make-comb
;
sndlib ?mus-comb ptr (int) mus_comb_p ( gen -- f )
' ?mus-comb make-?function ?comb
sndlib comb ptr sf sf (sf) mus_comb ( input pm gen -- r )
sndlib comb-1 ptr sf (sf) mus_comb_1 ( input gen -- r )

( scaler size line line-size type -- gen )
sndlib mus-make-notch sf int ptr int int (ptr) mus_make_notch
: clm-make-notch { f: scl size v tp -- gen }
    assert1( v ?vct )
    assert1( size v length <= )
    scl size v data-ptr v length tp mus-make-notch
;
sndlib ?mus-notch ptr (int) mus_notch_p ( gen -- f )
' ?mus-notch make-?function ?notch
sndlib notch ptr sf sf (sf) mus_notch ( input pm gen -- r )
sndlib notch-1 ptr sf (sf) mus_notch_1 ( input gen -- r )

\ ( backward forward size line line-size type -- gen )
sndlib mus-make-all-pass sf sf int ptr int int (ptr) mus_make_all_pass
: clm-make-all-pass { f: back f: ffw size v tp -- gen }
    assert1( v ?vct )
    assert1( size v length <= )
    back ffw size v data-ptr v length tp mus-make-all-pass
;
sndlib ?mus-all-pass ptr (int) mus_all_pass_p ( gen -- f )
' ?mus-all-pass make-?function ?all-pass
sndlib all-pass ptr sf sf (sf) mus_all_pass ( input pm gen -- r )
sndlib all-pass-1 ptr sf (sf) mus_all_pass_1 ( input gen -- r )

sndlib mus-make-average int ptr (ptr) mus_make_average ( size line -- gen )
: clm-make-average { v -- gen }
    assert1( v ?vct )
    v length v data-ptr mus-make-average
;
sndlib ?mus-average ptr (int) mus_average_p ( gen -- f )
' ?mus-average make-?function ?average
sndlib average ptr sf (sf) mus_average ( gen input -- f )

sndlib feedforward@ ptr (sf) mus_scaler ( gen -- r )
sndlib mus-feedforward! ptr sf (sf) mus_set_scaler ( val gen -- r )
: feedforward! ( val gen -- ) mus-feedforward! fdrop ;
sndlib feedback@ ptr (sf) mus_increment ( gen -- r )
sndlib mus-feedback! ptr sf (sf) mus_set_increment ( val gen -- r )
: feedback! ( val gen -- ) mus-feedback! fdrop ;

\ ( freq phase wave wave-size type -- gen )
sndlib mus-make-table-lookup sf sf ptr int int (ptr) mus_make_table_lookup
: clm-make-table-lookup { f: frq f: ph wave tp -- gen }
    frq ph wave data-ptr wave length tp mus-make-table-lookup
;
sndlib ?mus-table-lookup ptr (int) mus_table_lookup_p ( gen -- f )
' ?mus-table-lookup make-?function ?table-lookup
sndlib table-lookup ptr sf (sf) mus_table_lookup ( fm gen -- r )
sndlib table-lookup-1 ptr (sf) mus_table_lookup_1 ( gen -- r )

\ ( partials-data partials table table-size normalize -- w )
sndlib mus-partials>wave ptr int ptr int int (ptr) mus_partials_to_wave
: partials>wave { parts norm -- v }
    parts data-ptr parts length 2/
    *table-size* make-vct { table }
    table data-ptr *table-size*
    norm mus-partials>wave drop table
;
\ ( partials-data partials table table-size normalize -- w )
sndlib mus-phasepartials>wave ptr int ptr int int (ptr) mus_phase_partials_to_wave
: phase-partials>wave { parts norm -- v }
    parts data-ptr parts length 3 /
    *table-size* make-vct { table }
    table data-ptr *table-size*
    norm mus-phasepartials>wave drop table
;

sndlib clm-make-sawtooth-wave sf sf sf (ptr) mus_make_sawtooth_wave ( freq amp phase -- gen )
sndlib ?mus-sawtooth-wave ptr (int) mus_sawtooth_wave_p ( gen -- f )
' ?mus-sawtooth-wave make-?function ?sawtooth-wave
sndlib sawtooth-wave ptr sf (sf) mus_sawtooth_wave ( fm gen -- r )

sndlib clm-make-square-wave sf sf sf (ptr) mus_make_square_wave ( freq amp phase -- gen )
sndlib ?mus-square-wave ptr (int) mus_square_wave_p ( gen -- f )
' ?mus-square-wave make-?function ?square-wave
sndlib square-wave ptr sf (sf) mus_square_wave ( fm gen -- r )

sndlib clm-make-triangle-wave sf sf sf (ptr) mus_make_triangle_wave ( freq amp phase -- gen )
sndlib ?mus-triangle-wave ptr (int) mus_triangle_wave_p ( gen -- f )
' ?mus-triangle-wave make-?function ?triangle-wave
sndlib triangle-wave ptr sf (sf) mus_triangle_wave ( fm gen -- r )

sndlib clm-make-pulse-train sf sf sf (ptr) mus_make_pulse_train ( freq amp phase -- gen )
sndlib ?mus-pulse-train ptr (int) mus_pulse_train_p ( gen -- f )
' ?mus-pulse-train make-?function ?pulse-train
sndlib pulse-train ptr sf (sf) mus_pulse_train ( fm gen -- r )

sndlib rand-seed@ (int) mus_rand_seed ( -- u )
sndlib rand-seed! int (void) mus_set_rand_seed ( u -- )
sndlib mus-random sf (sf) mus_random ( amp -- r )
sndlib random sf (sf) mus_frandom ( amp -- r )
sndlib irandom int (int) mus_irandom ( amp -- n )

sndlib mus-make-rand sf sf (ptr) mus_make_rand ( freq base -- gen )
( freq base dist size -- gen )
sndlib mus-make-rand-dist sf sf ptr int (ptr) mus_make_rand_with_distribution
: clm-make-rand { f: frq f: amp dist -- gen }
    frq amp dist ?vct if
	dist inverse-integrate data-ptr 512 mus-make-rand-dist
    else
	mus-make-rand
    then
;
sndlib ?mus-rand ptr (int) mus_rand_p ( gen -- f )
' ?mus-rand make-?function ?rand
sndlib rand ptr sf (sf) mus_rand ( fm gen -- r )

sndlib mus-make-rand-interp sf sf (ptr) mus_make_rand_interp ( freq base -- gen )
( freq base dist size -- gen )
sndlib mus-make-rand-interp-dist sf sf ptr int (ptr) mus_make_rand_interp_with_distribution
: clm-make-rand-interp { f: frq f: amp dist -- gen }
    frq amp dist ?vct if
	dist inverse-integrate data-ptr 512 mus-make-rand-interp-dist
    else
	mus-make-rand-interp
    then
;
sndlib ?mus-rand-interp ptr (int) mus_rand_interp_p ( gen -- f )
' ?mus-rand-interp make-?function ?rand-interp
sndlib rand-interp ptr sf (sf) mus_rand_interp ( fm gen -- r )

sndlib clm-make-asymmetric-fm sf sf sf sf (ptr) mus_make_asymmetric_fm ( freq phase r ratio -- gen )
sndlib ?mus-asymmetric-fm ptr (int) mus_asymmetric_fm_p ( gen -- f )
' ?mus-asymmetric-fm make-?function ?asymmetric-fm
sndlib asymmetric-fm ptr sf sf (sf) mus_asymmetric_fm ( index fm gen -- r )
sndlib asymmetric-fm-1 ptr sf (sf) mus_asymmetric_fm_1 ( index gen -- r )

sndlib make-one-zero sf sf (ptr) mus_make_one_zero ( a0 a1 -- gen )
sndlib ?mus-one-zero ptr (int) mus_one_zero_p ( gen -- f )
' ?mus-one-zero make-?function ?one-zero
sndlib one-zero ptr sf (sf) mus_one_zero ( input gen -- r )

sndlib make-one-pole sf sf (ptr) mus_make_one_pole ( a0 b1 -- gen )
sndlib ?mus-one-pole ptr (int) mus_one_pole_p ( gen -- f )
' ?mus-one-pole make-?function ?one-pole
sndlib one-pole ptr sf (sf) mus_one_pole ( input gen -- r )

sndlib make-two-zero sf sf sf (ptr) mus_make_two_zero ( a0 a1 a2 -- gen )
sndlib make-zpolar sf sf (ptr) mus_make_zpolar ( radius freq -- gen )
sndlib ?mus-two-zero ptr (int) mus_two_zero_p ( gen -- f )
' ?mus-two-zero make-?function ?two-zero
sndlib two-zero ptr sf (sf) mus_two_zero ( input gen -- r )

sndlib make-two-pole sf sf sf (ptr) mus_make_two_pole ( a0 b1 b2 -- gen )
sndlib make-ppolar sf sf (ptr) mus_make_ppolar ( radius freq -- gen )
sndlib ?mus-two-pole ptr (int) mus_two_pole_p ( gen -- f )
' ?mus-two-pole make-?function ?two-pole
sndlib two-pole ptr sf (sf) mus_two_pole ( input gen -- r )

sndlib clm-make-formant sf sf sf (ptr) mus_make_formant ( radius freq gain -- gen )
sndlib ?mus-formant ptr (int) mus_formant_p ( gen -- f )
' ?mus-formant make-?function ?formant
sndlib formant ptr sf (sf) mus_formant ( input gen -- r )
sndlib mus-formant-bank ptr ptr sf int (sf) mus_formant_bank ( amps formants inval size -- r )
: formant-bank { amps formants f: inval -- r }
    amps data-ptr formants data-ptr inval amps length mus-formant-bank
;
sndlib formant-radius@ ptr (sf) mus_phase ( gen -- r )
sndlib mus-formant-radius! ptr sf (sf) mus_set_phase ( val gen -- r )
: formant-radius! ( val gen -- r ) mus-formant-radius! fdrop ;

\ ( freq phase n a b_ratio -- gen )
sndlib clm-make-sine-summation sf sf int sf sf (ptr) mus_make_sine_summation
sndlib ?mus-sine-summation ptr (int) mus_sine_summation_p ( gen -- f )
' ?mus-sine-summation make-?function ?sine-summation
sndlib sine-summation ptr sf (sf) mus_sine_summation ( input gen -- r )

sndlib mus-make-filter int ptr ptr ptr (ptr) mus_make_filter ( order xcoeffs ycoeffs state -- gen )
: clm-make-filter { order xcoeffs ycoeffs -- gen }
    xcoeffs length ycoeffs length max { len }
    order xcoeffs data-ptr ycoeffs data-ptr len make-vct data-ptr
    mus-make-filter
;
sndlib ?mus-filter ptr (int) mus_filter_p ( gen -- f )
' ?mus-filter make-?function ?filter
sndlib filter ptr sf (sf) mus_filter ( input gen -- r )

sndlib mus-make-fir-filter int ptr ptr (ptr) mus_make_fir_filter ( order xcoeffs state -- gen )
: clm-make-fir-filter { order xcoeffs -- gen }
    order xcoeffs data-ptr xcoeffs length make-vct data-ptr mus-make-fir-filter
;
sndlib ?mus-fir-filter ptr (int) mus_fir_filter_p ( gen -- f )
' ?mus-fir-filter make-?function ?fir-filter
sndlib fir-filter ptr sf (sf) mus_fir_filter ( input gen -- r )

sndlib mus-make-iir-filter int ptr ptr (ptr) mus_make_iir_filter ( order ycoeffs state -- gen )
: clm-make-iir-filter { order ycoeffs -- gen }
    order ycoeffs data-ptr ycoeffs length make-vct data-ptr mus-make-iir-filter
;
sndlib ?mus-iir-filter ptr (int) mus_iir_filter_p ( gen -- f )
' ?mus-iir-filter make-?function ?iir-filter
sndlib iir-filter ptr sf (sf) mus_iir_filter ( input gen -- r )
sndlib mus-make-fir-coeffs int ptr ptr (ptr) mus_make_fir_coeffs ( order env aa -- float-array )
: make-fir-coeffs { ord v1 -- v2 } ord v1 data-ptr nil mus-make-fir-coeffs ord c-array>vct ;

sndlib mus-xcoeffs@ ptr (ptr) mus_xcoeffs ( gen -- w )
: xcoeffs@ ( gen -- w ) dup mus-xcoeffs@ swap length@ c-array>vct ;
sndlib mus-ycoeffs@ ptr (ptr) mus_ycoeffs ( gen -- w )
: ycoeffs@ ( gen -- w ) dup mus-ycoeffs@ swap length@ c-array>vct ;
sndlib mus-xcoeff@ ptr int (sf) mus_xcoeff ( gen index -- r )
: xcoeff@ ( idx gen -- r ) swap mus-xcoeff@ ;
sndlib mus-xcoeff! ptr int sf (sf) mus_set_xcoeff ( gen index val -- r )
: xcoeff! ( val idx gen -- ) swap mus-xcoeff! fdrop ;
sndlib mus-ycoeff@ ptr int (sf) mus_ycoeff ( gen index -- r )
: ycoeff@ ( idx gen -- r ) swap mus-ycoeff@ ;
sndlib mus-ycoeff! ptr int sf (sf) mus_set_ycoeff ( gen index val -- r )
: ycoeff! ( val idx gen -- ) swap mus-ycoeff! fdrop ;
sndlib order@ ptr (int) mus_length ( gen -- n )
: a0@ ( gen -- r )   0 mus-xcoeff@ ;
: a0! ( val gen -- ) 0 mus-xcoeff! fdrop ;
: a1@ ( gen -- r )   1 mus-xcoeff@ ;
: a1! ( val gen -- ) 1 mus-xcoeff! fdrop ;
: a2@ ( gen -- r )   2 mus-xcoeff@ ;
: a2! ( val gen -- ) 2 mus-xcoeff! fdrop ;
: b1@ ( gen -- r )   1 mus-ycoeff@ ;
: b1! ( val gen -- ) 1 mus-ycoeff! fdrop ;
: b2@ ( gen -- r )   2 mus-ycoeff@ ;
: b2! ( val gen -- ) 2 mus-ycoeff! fdrop ;

( freq phase wave size type -- gen )
sndlib mus-make-wave-train sf sf ptr int int (ptr) mus_make_wave_train
: clm-make-wave-train { f: frq f: ph wave tp -- gen }
    frq ph wave data-ptr wave length tp mus-make-wave-train
;
sndlib ?mus-wave-train ptr (int) mus_wave_train_p ( gen -- f )
' ?mus-wave-train make-?function ?wave-train
sndlib wave-train ptr sf (sf) mus_wave_train ( fm gen -- r )
sndlib wave-train-1 ptr (sf) mus_wave_train_1 ( gen -- r )

\ ( npartials partials kind -- w )
sndlib mus-partials>polynomial int ptr int (ptr) mus_partials_to_polynomial
: partials>polynomial { parts kind -- v }
    parts array>partials { part }
    part length part data-ptr kind mus-partials>polynomial part length c-array>vct
;
\ ( npartials partials size table -- w )
sndlib mus-partials>waveshape int ptr int ptr (ptr) mus_partials_to_waveshape
: partials>waveshape { parts size -- v }
    parts array>partials { part }
    part length part data-ptr size nil mus-partials>waveshape *table-size* c-array>vct
;

sndlib mus-make-waveshape sf sf ptr int (ptr) mus_make_waveshape ( freq phase table size -- gen )
: clm-make-waveshape { f: freq parts -- gen }
    freq 0e parts data-ptr parts length mus-make-waveshape
;
sndlib ?mus-waveshape ptr (int) mus_waveshape_p ( gen -- f )
' ?mus-waveshape make-?function ?waveshape
sndlib waveshape ptr sf sf (sf) mus_waveshape ( index fm gen -- r )
sndlib waveshape-1 ptr sf (sf) mus_waveshape_1 ( index gen -- r )

\ ( brkpts pts scaler offset base duration start end odata -- gen )
sndlib mus-make-env ptr int sf sf sf sf llong llong ptr (ptr) mus_make_env
: clm-make-env { data f: scl f: dur f: offset f: base start end -- gen }
    data data-ptr data length 2/ scl offset base dur start s>d end s>d nil mus-make-env
;
sndlib ?mus-env ptr (int) mus_env_p ( gen -- f )
' ?mus-env make-?function ?env
sndlib ?env-linear ptr (int) mus_env_linear_p ( gen -- f )
sndlib env ptr (sf) mus_env ( gen -- r )
sndlib env-linear ptr (sf) mus_env_linear ( gen -- r )
sndlib restart-env ptr (void) mus_restart_env ( gen -- )
sndlib env-interp sf ptr (sf) mus_env_interp ( x gen -- r )
sndlib position@ ptr (int) mus_channels ( gen -- n )

sndlib mus-make-frame int (ptr) mus_make_empty_frame ( chans -- gen )
: make-frame ( chns -- gen ) assert1( dup 0> ) mus-make-frame ;
sndlib ?mus-frame ptr (int) mus_frame_p ( gen -- f )
' ?mus-frame make-?function ?frame
sndlib frame+ ptr ptr ptr (ptr) mus_frame_add ( fr1 fr2 res -- res )
sndlib frame* ptr ptr ptr (ptr) mus_frame_multiply ( fr1 fr2 res -- res )
sndlib mus-frame@ ptr int (sf) mus_frame_ref ( gen chan -- r )
: frame@ ( chan gen -- r ) swap mus-frame@ ;
sndlib mus-frame! ptr int sf (sf) mus_frame_set ( gen chan val -- r )
: frame! ( val chan gen -- ) swap mus-frame! fdrop ;
sndlib mus-frame-data ptr (ptr) mus_frame_data ( gen -- w )
: frame>vct ( gen -- v ) dup mus-frame-data swap length@ c-array>vct ;
: vct>frame ( v gen -- ) swap data-ptr swap mus-frame-data ! ;
: >frame ( chans-values chans -- gen )
    { chns }
    chns make-frame { gen }
    -1 chns 1- -do i gen frame! 1 -loop
    gen
;

sndlib mus-make-mixer int (ptr) mus_make_empty_mixer ( chans -- gen )
: make-mixer ( chns -- gen ) assert1( dup 0> ) mus-make-mixer ;
sndlib mus-make-identity-mixer int (ptr) mus_make_identity_mixer ( chans -- gen )
: make-identity-mixer ( chns -- gen ) assert1( dup 0> ) mus-make-identity-mixer ;
sndlib ?mus-mixer ptr (int) mus_mixer_p ( gen -- f )
' ?mus-mixer make-?function ?mixer
sndlib mus-mixer@ ptr int int (sf) mus_mixer_ref ( gen in out -- r )
: mixer@ ( in out gen -- r ) -rot mus-mixer@ ;
sndlib mus-mixer! ptr int int sf (sf) mus_mixer_set ( gen in out val -- r )
: mixer! ( val in out gen -- ) -rot mus-mixer! fdrop ;
sndlib mus-frame>frame ptr ptr ptr (ptr) mus_frame_to_frame ( gen1 in out -- out )
: frame>frame ( fr-in fr-out mx -- fr-out ) -rot mus-frame>frame ;
sndlib mus-sample>frame ptr sf ptr (ptr) mus_sample_to_frame ( gen1 in out -- out )
: sample>frame ( val fr-out mx -- fr ) swap mus-sample>frame ;
sndlib mus-frame>sample ptr ptr (sf) mus_frame_to_sample ( gen in -- r )
: frame>sample ( fr mx -- r ) swap mus-frame>sample ;
sndlib mixer* ptr ptr ptr (ptr) mus_mixer_multiply ( mx1 mx2 res -- res )
sndlib mixer+ ptr ptr ptr (ptr) mus_mixer_add ( mx1 mx2 res -- res )
sndlib mixer-scale ptr sf ptr (ptr) mus_mixer_scale ( mx scl res -- res )
: >mixer ( chans-in-out-values chans -- gen )
    { chns }
    chns make-mixer { gen }
    -1 chns 1- -do -1 chns 1- -do j i gen mixer! 1 -loop 1 -loop
    gen
;

sndlib mus-make-file>sample ptr (ptr) mus_make_file_to_sample ( filename -- gen )
: make-file>sample ( fname -- gen ) string>c$ mus-make-file>sample ;
sndlib ?mus-file>sample ptr (int) mus_file_to_sample_p ( gen -- f )
' ?mus-file>sample make-?function ?file>sample
sndlib mus-file>sample ptr llong int (sf) mus_file_to_sample ( gen samp chan -- r )
: file>sample { samp chan gen -- r } gen samp s>d chan mus-file>sample ;

sndlib mus-make-readin ptr int llong int (ptr) mus_make_readin ( filename chan start dir -- gen )
: clm-make-readin { fname chan beg dir -- gen }
    fname string>c$ chan beg s>d dir mus-make-readin
;
sndlib ?mus-readin ptr (int) mus_readin_p ( gen -- f )
' ?mus-readin make-?function ?readin
sndlib readin ptr (sf) mus_readin ( gen -- r )
sndlib increment@ ptr (sf) mus_increment ( gen -- r )
sndlib mus-increment! ptr sf (sf) mus_set_increment ( gen val -- r )
: increment! ( val gen -- ) mus-increment! fdrop ;
: increment+! ( val gen -- ) dup increment@ f+ increment! ;
sndlib mus-location@ ptr (llong) mus_location ( gen -- d )
: location@ ( gen -- u ) mus-location@ d>s ;
sndlib mus-location! ptr llong (llong) mus_set_location ( gen val -- d )
: location! ( val gen -- ) swap s>d mus-location! 2drop ;
sndlib channel@ ptr (int) mus_channel ( gen -- n )

sndlib ?mus-output ptr (int) mus_output_p ( gen -- f )
' ?mus-output make-?function ?output
sndlib ?mus-input ptr (int) mus_input_p ( gen -- f )
' ?mus-input make-?function ?input
sndlib mus-in-any llong int ptr (sf) mus_in_any ( frame chan IO -- r )
: in-any ( samp chan gen -- r ) 0 -rot mus-in-any ;
: ina ( samp gen -- r ) 0 0 rot mus-in-any ;
: inb ( samp gen -- r ) 0 1 rot mus-in-any ;

sndlib mus-make-file>frame ptr (ptr) mus_make_file_to_frame ( filename -- gen )
: make-file>frame ( fname -- gen ) string>c$ mus-make-file>frame ;
sndlib ?mus-file>frame ptr (int) mus_file_to_frame_p ( gen -- f )
' ?mus-file>frame make-?function ?file>frame
sndlib mus-file>frame ptr llong ptr (ptr) mus_file_to_frame ( gen1 samp f -- gen2 )
: file>frame { fr samp gen -- fr } gen samp s>d fr mus-file>frame ;

$" cannot close file" constant str-close-file
\ ( filename chans format type comment -- gen )
sndlib mus-make-sample>file ptr int int int ptr (ptr) mus_make_sample_to_file_with_comment
: make-sample>file { fname chans fmt typ com -- gen }
    assert1( chans 0> )
    fname string>c$ chans fmt typ com string>c$ mus-make-sample>file
;
sndlib ?mus-sample>file ptr (int) mus_sample_to_file_p ( gen -- f )
' ?mus-sample>file make-?function ?sample>file
sndlib mus-sample>file ptr llong int sf (sf) mus_sample_to_file ( gen samp chan val -- r )
: sample>file { f: val samp chan gen -- } gen samp s>d chan val mus-sample>file fdrop ;
sndlib mus-continue-sample>file ptr (ptr) mus_continue_sample_to_file ( filename -- gen )
: continue-sample>file ( fname -- gen ) string>c$ mus-continue-sample>file ;
sndlib mus-close-file ptr (int) mus_close_file ( gen -- n )
: mus-close ( gen -- ) mus-close-file 0< if str-close-file error then ;

sndlib mus-out-any llong sf int ptr (sf) mus_out_any ( frame val chan IO -- r )
: out-any ( frame val chan gen -- ) 0 -rot mus-out-any fdrop ;
: outa ( samp val gen -- ) 0 0 rot mus-out-any fdrop ;
: outb ( samp val gen -- ) 0 1 rot mus-out-any fdrop ;
: outc ( samp val gen -- ) 0 2 rot mus-out-any fdrop ;
: outd ( samp val gen -- ) 0 3 rot mus-out-any fdrop ;

\ ( filename chans format type -- gen )
sndlib mus-make-frame>file ptr int int int (ptr) mus_make_frame_to_file
: make-frame>file { fname chns fmt tp -- gen }
    assert1( chns 0> )
    fname string>c$ chns fmt tp mus-make-frame>file
;
sndlib ?mus-frame>file ptr (int) mus_frame_to_file_p ( gen -- f )
' ?mus-frame>file make-?function ?frame>file
sndlib mus-frame>file ptr llong ptr (ptr) mus_frame_to_file ( gen1 samp data -- gen2 )
: frame>file { fr samp gen -- } gen samp s>d fr mus-frame>file drop ;
sndlib mus-continue-frame>file ptr (ptr) mus_continue_frame_to_file ( filename -- gen )
: continue-frame>file ( fname -- gen ) string>c$ mus-continue-frame>file ;

\ ( degree distance reverb chans output revout type -- gen )
sndlib clm-make-locsig sf sf sf int ptr ptr int (ptr) mus_make_locsig
sndlib ?mus-locsig ptr (int) mus_locsig_p ( gen -- f )
' ?mus-locsig make-?function ?locsig
sndlib clm-locsig ptr llong sf (ptr) mus_locsig ( gen loc val -- gen )
: locsig ( val offset gen -- ) swap s>d clm-locsig drop ;
sndlib channels@ ptr (int) mus_channels ( gen -- n )
sndlib mus-locsig@ ptr int (sf) mus_locsig_ref ( gen chan -- r )
: locsig@ ( chan gen -- r ) swap mus-locsig@ ;
sndlib mus-locsig! ptr int sf (sf) mus_locsig_set ( gen chan val -- r )
: locsig! ( val chan gen -- ) swap mus-locsig! fdrop ;
sndlib mus-locsig-reverb@ ptr int (sf) mus_locsig_reverb_ref ( gen chan -- r )
: locsig-reverb@ ( chan gen -- r ) swap mus-locsig-reverb@ ;
sndlib mus-locsig-reverb! ptr int sf (sf) mus_locsig_reverb_set ( gen chan val -- r )
: locsig-reverb! ( val chan gen -- ) swap mus-locsig-reverb! fdrop ;
sndlib move-locsig ptr sf sf (void) mus_move_locsig ( gen degree distance -- )
sndlib fill-locsig ptr int sf sf int (void) mus_fill_locsig ( arr chans degree scaler type -- )

\ === Src, Convolve, Granulate and Phase-Vocoder Callbacks ===

\ Don't allocate memory in callback functions if you need it later
\ (i.e. NO make-vct)!  Provide allocated objects to CBs (see
\ PV-ANALYZE-CB in sndtest.gfm).  Test stack effects carefully with
\ F.S and .S in CBs.  After returning from CBs the stacks are in the
\ same state as before calling CBs, CB-stack mistakes can't be seen
\ outside CBs.
\
\ Before using callbacks you have to change function engine_callback()
\ in gforth-0.6.2/engine/main.c:
\
\ Original
\ 
\ void engine_callback(Xt* fcall, void * alist)
\ {
\   clist = (va_alist)alist;
\   engine(fcall, SP, RP, FP, LP);
\ }
\
\ Patched
\ 
\ void engine_callback(Xt* fcall, void * alist)
\ {
\   /* save global valiables */
\   Cell *rp = RP;
\   Cell *sp = SP;
\   Float *fp = FP;
\   Address lp = LP;
\ 
\   clist = (va_alist)alist;
\   engine(fcall, sp, rp, fp, lp);
\   
\   /* restore global variables */
\   RP = rp;
\   SP = sp;
\   FP = fp;
\   LP = lp;
\ }

callback make-input-cb         (sf)  ptr int callback;  \ ( closure dir      -- r )
callback make-edit-cb          (int) ptr     callback;  \ ( closure          -- n )
callback make-pv-analyze-cb    (int) ptr ptr callback;  \ ( closure input-xt -- f )
callback make-pv-synthesize-cb (sf)  ptr     callback;  \ ( closure          -- r )

                                                       ( Forth XT stack effect )
' f-input-cb      make-input-cb         c-input-cb      \ ( dir          -- r )
' f-edit-cb       make-edit-cb          c-edit-cb       \ ( gen          -- n )
' f-analyze-cb    make-pv-analyze-cb    c-analyze-cb    \ ( input-xt gen -- f )
' f-synthesize-cb make-pv-synthesize-cb c-synthesize-cb	\ ( gen          -- r )

\ Test on Forth side of C input function.
\ usage: gen environ@ 1 test-input-cb f.
: test-input-cb ( closure dir -- r ) 
    c-input-cb av-start-float >r >r av-ptr-r av-int-r av-call-float
;

sndlib environ@ ptr (ptr) mus_environ ( gen -- closure )
sndlib wrapper@ ptr (ptr) mus_wrapper ( gen -- wrapper )

: make-readin-cb ( rd -- xt; dir self -- r )
    lambda-create , latestxt
  does> ( dir self -- r )
    nip @ readin
;

sndlib mus-make-src ptr sf int ptr (ptr) mus_make_src ( input srate width closure -- gen )
: clm-make-src { input f: srate width -- gen }
    input ?readin if input make-readin-cb to input then
    input false false false false make-closure { closure }
    c-input-cb srate width closure mus-make-src
;
sndlib ?mus-src ptr (int) mus_src_p ( gen -- f )
' ?mus-src make-?function ?src
sndlib clm-mus-src ptr sf ptr (sf) mus_src ( gen sr-change input -- r )
: clm-src { input f: sr-change gen -- r }
    input ?dup-if gen environ@ input-xt ! then
    gen sr-change nil clm-mus-src
;
sndlib clm-mus-src-20 ptr ptr (sf) mus_src_20 ( gen input -- r )
: clm-src-20 { input gen -- r }
    input ?dup-if gen environ@ input-xt ! then
    gen nil clm-mus-src-20
;
sndlib clm-mus-src-05 ptr ptr (sf) mus_src_05 ( gen input -- r )
: clm-src-05 { input gen -- r }
    input ?dup-if gen environ@ input-xt ! then
    gen nil clm-mus-src-05
;

( input filter fftsize filt-size closure -- gen )
sndlib mus-make-convolve ptr ptr int int ptr (ptr) mus_make_convolve
: clm-make-convolve { input filt fftsize -- gen }
    input ?readin if input make-readin-cb to input then
    input false false false false make-closure { closure }
    filt length { len }
    len 1- len and unless
	len 2*
    else
	2e len s>f 2e flogn fceil f** f>s
    then fftsize max to fftsize
    c-input-cb filt data-ptr fftsize filt length closure mus-make-convolve
;
sndlib ?mus-convolve ptr (int) mus_convolve_p ( gen -- f )
' ?mus-convolve make-?function ?convolve
sndlib clm-mus-convolve ptr ptr (sf) mus_convolve ( gen input -- r )
: clm-convolve { input gen -- r }
    input ?dup-if gen environ@ input-xt ! then
    gen nil clm-mus-convolve
;

sndlib mus-spectrum ptr ptr ptr int int (ptr) mus_spectrum ( rdat idat window n type -- rdat' )
: spectrum { rdat idat window tp -- }
    rdat data-ptr
    idat data-ptr
    window ?vct if window data-ptr else false then
    rdat length tp mus-spectrum drop
;
sndlib mus-fft ptr ptr int int (void) mus_fft ( rl im n is -- )
: fft { rl im len isign -- } rl data-ptr im data-ptr len isign mus-fft ;
sndlib mus-make-fft-window int int sf (ptr) mus_make_fft_window ( type size beta -- w )
: make-fft-window { type size f: beta -- w } type size beta mus-make-fft-window size c-array>vct ;
\ ( type size beta window -- w )
sndlib mus-convolution ptr ptr int (ptr) mus_convolution ( rl1 rl2 n -- w )
: convolution { rdat idat size -- } rdat data-ptr idat data-ptr size mus-convolution drop ;
( file1 file2 maxamp outfile -- )
sndlib mus-convolve-files ptr ptr sf ptr (void) mus_convolve_files
: convolve-files { fname1 fname2 f: max-amp out-file -- }
    fname1 string>c$ fname2 string>c$ max-amp out-file string>c$ mus-convolve-files
;
    
( input expansion length scaler hop ramp jitter max-size edit closure -- gen )
sndlib mus-make-granulate ptr sf sf sf sf sf sf int ptr ptr (ptr) mus_make_granulate
: clm-make-granulate { input f: expan f: len f: scl f: hop f: ramp f: jitter max-size edit -- gen }
    input ?readin if input make-readin-cb to input then
    input edit false false false make-closure { closure }
    edit if
	c-input-cb expan len scl hop ramp jitter max-size c-edit-cb closure mus-make-granulate
	dup closure mus-gen !
    else
	c-input-cb expan len scl hop ramp jitter max-size nil closure mus-make-granulate
    then
;
sndlib ?mus-granulate ptr (int) mus_granulate_p ( gen -- f )
' ?mus-granulate make-?function ?granulate
sndlib mus-granulate-edit-function! ptr ptr (void) mus_granulate_set_edit_function ( gen edit -- )
sndlib clm-mus-granulate ptr ptr ptr (sf) mus_granulate_with_editor ( gen input edit -- r )
: clm-granulate { input edit gen -- r }
    input ?dup-if gen environ@ input-xt ! then
    edit ?dup-if
	gen environ@ edit-xt !
	gen c-edit-cb mus-granulate-edit-function!
	c-edit-cb
    else
	nil
    then { ed-cb }
    gen nil ed-cb clm-mus-granulate
;

sndlib mus-ramp@ ptr (llong) mus_ramp ( gen -- d )
: ramp@ ( gen -- n ) mus-ramp@ d>s ;
sndlib mus-ramp! ptr llong (llong) mus_set_ramp ( gen val -- d )
: ramp! ( val gen --  ) swap s>d mus-ramp! 2drop ;
sndlib mus-hop@ ptr (llong) mus_hop ( gen -- d )
: hop@ ( gen -- n ) mus-hop@ d>s ;
sndlib mus-hop! ptr llong (llong) mus_set_hop ( gen val -- d )
: hop! ( val gen --  ) swap s>d mus-hop! 2drop ;

( input fftsize overlap interp pitch analyze edit synthesize closure -- gen )
sndlib mus-make-phase-vocoder ptr int int int sf ptr ptr ptr ptr (ptr) mus_make_phase_vocoder
: clm-make-phase-vocoder { input fftsize overlap interp f: pitch analyze edit synth -- gen }
    input ?readin if input make-readin-cb to input then
    input edit analyze synth false make-closure { closure }
    analyze if c-analyze-cb    else nil then { a-cb }
    edit    if c-edit-cb       else nil then { e-cb }
    synth   if c-synthesize-cb else nil then { s-cb }
    c-input-cb fftsize overlap interp pitch a-cb e-cb s-cb closure mus-make-phase-vocoder
    dup closure mus-gen ! 
;
sndlib ?mus-phase-vocoder ptr (int) mus_phase_vocoder_p ( gen -- f )
' ?mus-phase-vocoder make-?function ?phase-vocoder
( gen input analyze edit synthesize -- r )
sndlib clm-mus-phase-vocoder ptr ptr ptr ptr ptr (sf) mus_phase_vocoder_with_editors
: clm-phase-vocoder { input analyze edit synthesize gen -- r }
    input      ?dup-if gen environ@ input-xt ! then
    analyze    ?dup-if gen environ@ analyze-xt !    c-analyze-cb    else nil then { a-cb }
    edit       ?dup-if gen environ@ edit-xt !       c-edit-cb       else nil then { e-cb }
    synthesize ?dup-if gen environ@ synthesize-xt ! c-synthesize-cb else nil then { s-cb }
    gen nil a-cb e-cb s-cb clm-mus-phase-vocoder
;

sndlib mus-pv-amp-increments ptr (ptr) mus_phase_vocoder_amp_increments
: pv-amp-increments@ 	  ( idx gen -- r ) mus-pv-amp-increments c-float-array@ ;
: pv-amp-increments! 	  ( val idx gen -- ) mus-pv-amp-increments c-float-array! ;
: pv-amp-increments>vct   ( gen -- v ) dup mus-pv-amp-increments swap length@ c-array>vct ;
sndlib mus-pv-amps ptr (ptr) mus_phase_vocoder_amps
: pv-amps@ 		  ( idx gen -- r ) mus-pv-amps c-float-array@ ;
: pv-amps! 		  ( val idx gen -- ) mus-pv-amps c-float-array! ;
: pv-amps>vct             ( gen -- v ) dup mus-pv-amps swap length@ 2/ c-array>vct ;
sndlib mus-pv-freqs ptr (ptr) mus_phase_vocoder_freqs
: pv-freqs@ 		  ( idx gen -- r ) mus-pv-freqs c-float-array@ ;
: pv-freqs! 		  ( val idx gen -- ) mus-pv-freqs c-float-array! ;
: pv-freqs>vct            ( gen -- v ) dup mus-pv-freqs swap length@ c-array>vct ;
sndlib mus-pv-phases ptr (ptr) mus_phase_vocoder_phases
: pv-phases@ 		  ( idx gen -- r ) mus-pv-phases c-float-array@ ;
: pv-phases! 		  ( val idx gen -- ) mus-pv-phases c-float-array! ;
: pv-phases>vct           ( gen -- v ) dup mus-pv-phases swap length@ 2/ c-array>vct ;
sndlib mus-pv-phase-increments ptr (ptr) mus_phase_vocoder_phase_increments
: pv-phase-increments@    ( idx gen -- r ) mus-pv-phase-increments c-float-array@ ;
: pv-phase-increments!    ( val idx gen -- ) mus-pv-phase-increments c-float-array! ;
: pv-phase-increments>vct ( gen -- v ) dup mus-pv-phase-increments swap length@ 2/ c-array>vct ;
sndlib pv-outctr@ ptr (int) mus_phase_vocoder_outctr ( ptr -- n )
sndlib mus-pv-outctr! ptr int (int) mus_phase_vocoder_set_outctr
: pv-outctr! ( val gen -- ) swap mus-pv-outctr! drop ;

sndlib file-buffer-size@ (int) mus_file_buffer_size ( -- n )
sndlib mus-file-buffer-size! int (int) mus_set_file_buffer_size ( size -- n )
: file-buffer-size! ( size -- ) mus-file-buffer-size! drop ;

: array-of-array>c-array { ary -- c-ary }
    ary length cells allocate throw { data }
    ary each data-ptr data i cells + ! end-each
    data
;

( outf inf out-start out-frames in-start mx env -- )
sndlib mus-mix-w/rw ptr ptr llong llong llong ptr ptr (void) mus_mix_with_reader_and_writer
: clm-mix { outf inf out-start out-frames in-start mx envs -- }
    envs ?array if envs array-of-array>c-array else false then { c-envs }
    outf inf out-start s>d out-frames s>d in-start s>d mx c-envs mus-mix-w/rw
    envs ?array if c-envs free throw then
;
( outfile infile out-start out-samps in-start mx envs -- )
sndlib gfm-mix ptr llong llong llong ptr ptr (void) mus_mix
: clm-mix-file { outfile infile out-start out-samps in-start mx envs -- }
    envs ?array if envs array-of-array>c-array else false then { c-envs }
    outfile string>c$ infile string>c$ out-start s>d out-samps s>d in-start s>d mx c-envs gfm-mix
    envs ?array if c-envs free throw then
;

( file chan st samps ary -- n )
sndlib mus-file>float-array ptr int llong int ptr (int) mus_file_to_float_array
: file>array { fname chan start samps -- data }
    samps make-vct { v }
    fname string>c$ chan start s>d samps v data-ptr mus-file>float-array drop
    v
;
( file data len srate chans -- n )
sndlib mus-float-array>file ptr ptr int int int (int) mus_float_array_to_file
: array>file { fname data len srate chans -- }
    fname string>c$ data data-ptr len srate chans mus-float-array>file drop
;

sndlib clm-make-ssb-am sf int (ptr) mus_make_ssb_am ( freq order -- gen )
sndlib ?mus-ssb-am ptr (int) mus_ssb_am_p ( gen -- f )
' ?mus-ssb-am make-?function ?ssb-am
sndlib ssb-am ptr sf sf (sf) mus_ssb_am ( gen insig fm -- r )
sndlib ssb-am-1 ptr sf (sf) mus_ssb_am_1 ( gen insig -- r )

init-mus-module

\ === sndlib.h ===
\ --- sound.c ---
$" cannot open file" constant str-open-file
sndlib mus-sound-chans ptr (int) mus_sound_chans ( filename -- n )
: sound-chans ( fname -- n ) string>c$ mus-sound-chans ;
sndlib mus-sound-srate ptr (int) mus_sound_srate ( filename -- n )
: sound-srate ( fname -- n ) string>c$ mus-sound-srate ;
sndlib mus-sound-frames ptr (llong) mus_sound_frames ( filename -- d )
: sound-frames ( fname -- n ) string>c$ mus-sound-frames d>s ;
sndlib mus-sound-samples ptr (llong) mus_sound_samples ( filename -- d )
: sound-samples ( fname -- n ) string>c$ mus-sound-samples d>s ;
sndlib mus-sound-duration ptr (sf) mus_sound_duration ( filename -- r )
: sound-duration ( fname -- r ) string>c$ mus-sound-duration ;
sndlib mus-data-format-name int (ptr) mus_data_format_name ( format -- c-str )
: .data-format ( format -- ) mus-data-format-name .cstring ;
sndlib mus-short-data-format-name int (ptr) mus_short_data_format_name ( format -- c-str )
: .short-data-format ( format -- ) mus-short-data-format-name .cstring ;
sndlib mus-sound-data-format ptr (int) mus_sound_data_format ( filename -- n )
: sound-data-format ( fname -- n ) string>c$ mus-sound-data-format ;
sndlib mus-header-type-name int (ptr) mus_header_type_name ( type -- c-str )
: .header-type ( type -- ) mus-header-type-name .cstring ;
sndlib mus-sound-header-type ptr (int) mus_sound_header_type ( filename -- n )
: sound-header-type ( fname -- n ) string>c$ mus-sound-header-type ;
sndlib mus-sound-length ptr (llong) mus_sound_length ( filename -- u )
: sound-length ( fname -- u ) string>c$ mus-sound-length d>s ;
sndlib mus-sound-comment ptr (ptr) mus_sound_comment ( filename -- c-str )
: sound-comment ( fname -- str|f )
    string>c$ mus-sound-comment dup if $c>string else drop false then
;
sndlib mus-sound-open-input ptr (int) mus_sound_open_input ( filename -- fd )
: sound-open-input ( fname -- n )
    string>c$ mus-sound-open-input dup 0< if str-open-file error then
;
\ ( filename srate chans format type comment -- fd )
sndlib mus-sound-open-output ptr int int int int ptr (int) mus_sound_open_output
: sound-open-output { fname srate chans fmt hdr comm -- n }
    fname string>c$ srate chans fmt hdr comm string>c$ mus-sound-open-output
    dup 0< if str-open-file error then
;
sndlib mus-sound-close-input int (int) mus_sound_close_input ( fd -- n )
: sound-close-input ( fd -- ) mus-sound-close-input 0< if str-close-file error then ;
sndlib mus-sound-close-output int llong (int) mus_sound_close_output ( fd bytes-of-data -- n )
: sound-close-output ( fd bytes -- ) s>d mus-sound-close-output 0< if str-close-file error then ;
sndlib mus-sound-read int int int int ptr (int) mus_sound_read ( fd beg end chans bufs -- n )
: sound-read ( fd beg end chns sd -- n ) obj-data @ mus-sound-read ;
sndlib mus-sound-write int int int int ptr (int) mus_sound_write ( fd beg end chans bufs -- n )
: sound-write ( fd beg end chns sd -- ) obj-data @ mus-sound-write throw ;
sndlib mus-sound-maxamps ptr int ptr ptr (llong) mus_sound_maxamps ( fname chans vals times -- d )
: sound-maxamp { fname -- vals times }
    fname sound-chans { chans }
    chans make-vct { vals }		\ mus_sample_t * (i.e. Float *)
    chans make-2array { dtimes }	\ off_t *
    fname string>c$ chans vals data-ptr dtimes data-ptr mus-sound-maxamps 2drop
    chans make-array map i dtimes 2array@ nip end-map { times }
    dtimes free-array
    vals times
;
sndlib mus-sound-prune (int) mus_sound_prune ( -- n )
: sound-prune ( -- ) mus-sound-prune drop ;

\ --- headers.c ---
sndlib mus-samples-to-bytes int llong (llong) mus_samples_to_bytes ( format size -- d )
sndlib mus-bytes-to-samples int llong (llong) mus_bytes_to_samples ( format size -- d )
: samples>bytes ( fmt size -- u ) s>d mus-samples-to-bytes d>s ;
: bytes>samples ( fmt size -- u ) s>d mus-bytes-to-samples d>s ;

\ --- audio.c ---
sndlib mus-audio-initialize (int) mus_audio_initialize ( -- n )
: audio-initialize ( -- ) mus-audio-initialize 0< abort" can't initialize audio module" ;

sndlib .audio (void) mus_audio_describe ( -- )
sndlib audio-systems (int) mus_audio_systems ( -- n )
sndlib mus-audio-system-name int (ptr) mus_audio_system_name ( sys -- c-str )
: .audio-system ( sys -- ) mus-audio-system-name .cstring ;
sndlib mus-audio-moniker (ptr) mus_audio_moniker ( -- c-str )
: .audio-moniker ( -- ) mus-audio-moniker .cstring ;
\ ( dev srate chans format size -- fd )
sndlib mus-audio-open-input int int int int int (int) mus_audio_open_input
: audio-open-input { dev sr chns fmt size -- fd }
    fmt to *audio-format*
    dev sr chns fmt size mus-audio-open-input dup 0< if str-audio-open-dac error then
;
\ ( dev srate chans format size -- fd )
sndlib mus-audio-open-output int int int int int (int) mus_audio_open_output
: audio-open-output { dev sr chns fmt size -- fd }
    fmt to *audio-format*
    dev sr chns fmt size mus-audio-open-output dup 0< if str-audio-open-dac error then
;
sndlib mus-audio-close int (int) mus_audio_close ( line -- n )
: audio-close ( line -- )
    mus-audio-close 0< if str-audio-close error then
    0 to *audio-format*
;
\ ( line frames chans bufs format -- n )
sndlib audio-read-buffers int int int ptr int (int) mus_audio_read_buffers
: audio-read { line sd bytes -- n }
    line bytes sd sound-data-chans sd obj-data @ *audio-format* audio-read-buffers
;
\ ( line frames chans bufs format clipped -- n )
sndlib audio-write-buffers int int int ptr int int (int) mus_audio_write_buffers
: audio-write { line sd bytes -- n }
    line bytes sd sound-data-chans sd obj-data @ *audio-format* true audio-write-buffers
;

\ === AUDIO-MIXER: in addition get and set recorder device ===
[ifundef] libc library libc libc.so [then]
libc c-open  int int int (int) open
libc c-close int (int) close
libc c-ioctl int int int (int) ioctl

\ constants taken from fsndlib.fs
$0000 constant O_RDONLY
$0001 constant O_WRONLY
$0002 constant O_RDWR
$0004 constant O_NONBLOCK

0   constant SOUND_MIXER_VOLUME
5   constant SOUND_MIXER_SPEAKER
6   constant SOUND_MIXER_LINE
7   constant SOUND_MIXER_MIC
8   constant SOUND_MIXER_CD
14  constant SOUND_MIXER_LINE1
15  constant SOUND_MIXER_LINE2
16  constant SOUND_MIXER_LINE3

1 SOUND_MIXER_VOLUME   lshift constant SOUND_MASK_VOLUME
1 SOUND_MIXER_SPEAKER  lshift constant SOUND_MASK_SPEAKER
1 SOUND_MIXER_MIC      lshift constant SOUND_MASK_MIC
1 SOUND_MIXER_CD       lshift constant SOUND_MASK_CD
1 SOUND_MIXER_LINE     lshift constant SOUND_MASK_LINE
1 SOUND_MIXER_LINE1    lshift constant SOUND_MASK_LINE1
1 SOUND_MIXER_LINE2    lshift constant SOUND_MASK_LINE2
1 SOUND_MIXER_LINE3    lshift constant SOUND_MASK_LINE3

$40044dfe constant SOUND_MIXER_READ_DEVMASK
$40044dff constant SOUND_MIXER_READ_RECSRC
$c0044dff constant SOUND_MIXER_WRITE_RECSRC

s" /dev/mixer"  c-string constant snd-mixer-1
s" /dev/mixer2" c-string constant snd-mixer-2

: audio-mixer-close ( fd -- ) c-close 0< if str-audio-close-mixer error then ;
: audio-open-mixer-read ( -- fd )
    snd-mixer-1 O_RDONLY 0 c-open { fd }
    fd 0< if
	snd-mixer-1 O_RDONLY O_NONBLOCK or 0 c-open to fd
	fd 0< if
	    snd-mixer-2 O_RDONLY 0 c-open to fd
	    fd 0< if
		snd-mixer-2 O_RDONLY O_NONBLOCK or 0 c-open to fd
		fd 0< if str-audio-open-mixer error then
	    then
	then
    then
    fd
;
: audio-open-mixer-write ( -- fd )
    snd-mixer-1 O_WRONLY 0 c-open { fd }
    fd 0< if
	snd-mixer-1 O_WRONLY O_NONBLOCK or 0 c-open to fd
	fd 0< if
	    snd-mixer-2 O_WRONLY 0 c-open to fd
	    fd 0< if
		snd-mixer-2 O_WRONLY O_NONBLOCK or 0 c-open to fd
		fd 0< if str-audio-open-mixer error then
	    then
	then
    then
    fd
;
$" unknown device" constant str-audio-mixer-device

sndlib mus-audio-mixer-read int int int ptr (int) mus_audio_mixer_read ( dev fld chan vals -- n )
: audio-mixer-read { dev fld chan -- r vals }
    mus-audio-direction make-vct { fvals }
    fld mus-audio-record-device = if
	fvals length make-array { vals }
	audio-open-mixer-read { fd }
	0 { w^ retval }
	fd SOUND_MIXER_READ_DEVMASK retval c-ioctl if
	    fd audio-mixer-close str-audio-mixer-read error
	then
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
	fd audio-mixer-close
	vals
    else
	dev fld chan fvals data-ptr mus-audio-mixer-read 0< if str-audio-mixer-read error then
	fvals length make-array map i fvals vct@ f>s end-map { vals }
	fvals vct-first
	fvals free-vct
	vals
    then
;
sndlib mus-audio-mixer-write int int int ptr (int) mus_audio_mixer_write ( dev fld chan vals -- n )
: audio-mixer-write { dev fld chan f: val -- r vals }
    fld mus-audio-record-device = if
	0 { w^ retval }
	audio-open-mixer-write { fd }
	fd SOUND_MIXER_READ_DEVMASK retval c-ioctl if
	    fd audio-mixer-close
	    str-audio-mixer-write error
	then
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
	fd audio-mixer-close
	dev fld chan audio-mixer-read
    else
	mus-audio-direction make-vct { fvals }
	val 0 fvals vct!
	dev fld chan fvals data-ptr mus-audio-mixer-write 0< if str-audio-mixer-write error then
	fvals length make-array map i fvals vct@ f>s end-map { vals }
	fvals vct-first
	fvals free-vct
	vals
    then
;

audio-initialize

\ === Fcomb (Forth or libsndins.so if lib exists) ===

\ callbacks for CLM-getter and -setter (Fcomb- and Buffer-Class)
callback (int)-ptr      (int)   ptr       callback;
callback (ptr)-ptr      (ptr)   ptr       callback;
callback (int)-ptr-ptr  (int)   ptr ptr   callback;
callback (llong)-ptr    (llong) ptr       callback;
callback (sf)-ptr       (sf)    ptr       callback;
callback (sf)-ptr-sf    (sf)    ptr sf    callback;
callback (sf)-ptr-sf-sf (sf)    ptr sf sf callback;

so-lib-exists? sndins libsndins.so [if]
    42 constant mus-fcomb
    sndins clm-make-fcomb sf int sf sf (ptr) mus_make_fcomb ( scaler size a0 a1 -- gen )
    sndins ?mus-fcomb ptr (int) mus_fcomb_p ( gen -- f )
    ' ?mus-fcomb make-?function ?fcomb
    sndins clm-fcomb ptr sf sf (sf) mus_fcomb ( gen input ignored -- r )
    : fcomb ( input gen -- r ) 0e clm-fcomb ;
[else]
    struct
	cell%  	field fcomb-core
	cell%  	field fcomb-loc
	cell%  	field fcomb-size
	cell%  	field fcomb-line
	sfloat% field fcomb-xscl
	sfloat% field fcomb-a0
	sfloat% field fcomb-a1
	sfloat% field fcomb-x1
    end-struct fcomb%

    mus-make-class-tag constant mus-fcomb
    mus-fcomb 1+ to my-mus-last
    
    : ?fcomb ( gen -- f )
	try
	    ?dup-if fcomb-core @ mus-any-type @ mus-fcomb = else false then
	recover
	    2drop false
	endtry
    ;
    : fcomb { f: input gen -- r }
	gen fcomb-loc @ gen fcomb-line @ vct@ { f: tap-result }
	gen fcomb-a0 sf@ tap-result f* gen fcomb-a1 sf@ gen fcomb-x1 sf@ f* f+ { f: filter-result }
	tap-result gen fcomb-x1 sf!
	filter-result gen fcomb-xscl sf@ f* input f+ gen fcomb-loc @ gen fcomb-line @ vct!
	1 gen fcomb-loc +!
	gen fcomb-loc @ gen fcomb-size @ >= if 0 gen fcomb-loc ! then
	tap-result
    ;

    :noname { gen -- 0 }
	gen ?fcomb if
	    gen fcomb-line @ ?dup-if
		dup obj-data @ ?dup-if free throw then
		free throw
	    then
	then 0
    ;                                            (int)-ptr      mus-fcomb-free
    :noname ( gen -- c-str )
	{ gen }
	$" fcomb scaler: " gen fcomb-xscl sf@ 3 $(f.r) $+
	$" , a0: " $+ gen fcomb-a0 sf@ 3 $(f.r) $+
	$" , a1: " $+ gen fcomb-a1 sf@ 3 $(f.r) $+
	$" , line: " $+ gen fcomb-line @ $(.vct) $+
	string>c$
    ;                                            (ptr)-ptr      mus-fcomb-.
    :noname ( gen1 gen2 -- f )
	{ g1 g2 } g1 g2 =
	g1 fcomb-xscl sf@ g2 fcomb-xscl sf@ f=
	g1 fcomb-a0 sf@ g2 fcomb-a0 sf@ f= and
	g1 fcomb-a1 sf@ g2 fcomb-a1 sf@ f= and
	g1 fcomb-size @ g2 fcomb-size @ = and or
    ;                                            (int)-ptr-ptr  mus-fcomb-=
    :noname ( gen -- w ) fcomb-line @ data-ptr ; (ptr)-ptr      mus-fcomb-data
    :noname ( gen -- d ) fcomb-size @ s>d ;      (llong)-ptr    mus-fcomb-length@
    :noname ( gen -- r ) fcomb-xscl sf@ ;        (sf)-ptr       mus-fcomb-scaler@
    :noname ( gen r -- r ) fdup fcomb-xscl sf! ; (sf)-ptr-sf    mus-fcomb-scaler!
    :noname ( gen r1 r2 -- r3 ) fdrop fcomb ;    (sf)-ptr-sf-sf mus-fcomb-run

    mus-any-class% %allot value fcomb-class
    mus-fcomb           fcomb-class mus-any-type !
    $" fcomb" string>c$ fcomb-class mus-any-name !
    mus-fcomb-free      fcomb-class mus-any-release !
    mus-fcomb-.         fcomb-class mus-any-describe !
    mus-fcomb-=         fcomb-class mus-any-equalp !
    mus-fcomb-data      fcomb-class mus-any-data !
    mus-fcomb-length@   fcomb-class mus-any-length !
    mus-fcomb-scaler@   fcomb-class mus-any-scaler !
    mus-fcomb-scaler!   fcomb-class mus-any-set-scaler !
    mus-fcomb-run       fcomb-class mus-any-run !
    mus-not-special     fcomb-class mus-any-extended-type !

    \ 1e *table-size* 0e 0e clm-make-fcomb value gen
    : clm-make-fcomb { f: scl size f: a0 f: a1 -- gen }
	fcomb% %alloc { gen }
	fcomb-class   gen fcomb-core !
	0             gen fcomb-loc !
	size          gen fcomb-size !
	size make-vct gen fcomb-line !
	scl           gen fcomb-xscl sf!
	a0            gen fcomb-a0 sf!
	a1            gen fcomb-a1 sf!
	0e            gen fcomb-x1 sf!
	gen
    ;
[then]

\ === Buffer ===
struct
    cell% field buf-core
    cell% field buf-buffer
    cell% field buf-size
    cell% field buf-loc
    cell% field buf-fill-time
    cell% field buf-empty
end-struct buffer%

mus-make-class-tag constant mus-buffer
mus-buffer mus-fcomb max 1+ to my-mus-last

: ?buffer-empty ( gen -- f ) buf-empty @ ;
: ?buffer-full  ( gen -- f ) dup buf-fill-time @ over buf-size @ >= swap buf-loc @ 0= and ;
: ?buffer ( obj -- f )
    try
	?dup-if buf-core @ mus-any-type @ mus-buffer = else false then
    recover
	2drop false
    endtry
;

: buffer>sample { gen -- r }
    gen buf-loc @ gen buf-size @ < if
	gen buf-loc @ gen buf-buffer @ vct@
    else
	0e
    then				\ result
    1 gen buf-loc +!
    gen buf-loc @ { loc }
    gen buf-empty @ 0= loc gen buf-fill-time @ >= and if
	gen buf-buffer @ data-ptr { data }
	gen buf-size @ { size }
	loc size < if
	    data loc sfloats + data size loc - sfloats move
	    data loc sfloats + size loc - sfloats erase
	else
	    data size sfloats erase
	then
	gen buf-loc @ negate gen buf-fill-time +!
	0 gen buf-loc !
	true gen buf-empty !
    then
;
: sample>buffer { f: val gen -- }
    gen buf-fill-time @ gen buf-size @ >= if
	gen buf-loc @ { loc }
	assert1( loc 0> )
	gen buf-buffer @ data-ptr { data }
	gen buf-size @ loc - sfloats { size }
	data loc sfloats + data size move
	data loc sfloats + size erase
	loc negate gen buf-fill-time +!
	0 gen buf-loc !
    then
    val gen buf-fill-time @ gen buf-buffer @ vct!
    1 gen buf-fill-time +!
;

:noname { gen -- 0 }
    gen ?buffer if
	gen buf-buffer @ ?dup-if
	    obj-data @ ?dup-if free throw then
	    free throw
	then
    then 0
;                                                       (int)-ptr      mus-buffer-free
:noname ( gen -- c-str )
    { gen }
    $" buffer loc: " gen buf-loc @ $(.) $+
    $" , fill-time: " $+ gen buf-fill-time @ $(.) $+
    $" , size: " $+ gen buf-size @ $(.) $+
    $" , empty: " $+ gen ?buffer-empty true-false-string $+
    string>c$
;                                                       (ptr)-ptr      mus-buffer-.
:noname ( gen1 gen2 -- f ) = ;                          (int)-ptr-ptr  mus-buffer-=
:noname ( gen -- w ) buf-buffer @ data-ptr ;            (ptr)-ptr      mus-buffer-data
:noname ( gen -- d ) buf-size @ s>d ;                   (llong)-ptr    mus-buffer-length@
:noname ( gen -- r ) buf-fill-time @ s>f ;              (sf)-ptr       mus-buffer-increment@
:noname ( gen r -- r )
    fdup f>s { f: fval gen val -- r }
    val gen buf-fill-time !
    val 0= gen buf-empty !
    fval
;                                                       (sf)-ptr-sf    mus-buffer-increment!
:noname ( gen r1 r2 -- r3 ) fdrop fdrop buffer>sample ; (sf)-ptr-sf-sf mus-buffer-run

mus-any-class% %allot value buffer-class
mus-buffer            buffer-class mus-any-type !
$" buffer" string>c$  buffer-class mus-any-name !
mus-buffer-free       buffer-class mus-any-release !
mus-buffer-.          buffer-class mus-any-describe !
mus-buffer-=          buffer-class mus-any-equalp !
mus-buffer-data       buffer-class mus-any-data !
mus-buffer-length@    buffer-class mus-any-length !
mus-buffer-increment@ buffer-class mus-any-increment !
mus-buffer-increment! buffer-class mus-any-set-increment !
mus-buffer-run        buffer-class mus-any-run !
mus-not-special       buffer-class mus-any-extended-type !

\ *table-size* 0e make-buffer value gen
: clm-make-buffer { size ftime -- gen }
    buffer% %alloc { gen }
    buffer-class  gen buf-core !
    size make-vct gen buf-buffer !
    size          gen buf-size !
    0             gen buf-loc !
    ftime         gen buf-fill-time !
    ftime 0=      gen buf-empty !
    gen
;

\ Generalized inspect, equal, and free functions.
\ We know Vct, Array, Hash, Hook, String, Complex, Sound-data and CLM-gens.
lambda: ( obj -- )
    depth 1 >= if
	dup ?gen if .clm-gen else .inspect then
    else
	fdepth 1 >= if
	    make-float { sfl } sfl dup gfm-base-inspect @ execute sfl free-float
	else
	    $" missing object" warn
	then
    then
; is .gen
lambda: ( obj -- )
    depth 1 >= if
	dup ?gen if $(.clm-gen) else $(.obj>string) then
    else
	fdepth 1 >= if
	    make-float { sfl } sfl dup gfm-base-to-string @ execute sfl free-float
	else
	    $" WARNING: missing object"
	then
    then
; is gen>string
lambda: ( gen1 gen2 -- f ) dup ?gen if clm-gen= else obj-equal then ; is gen=
lambda: ( gen -- ) dup ?gen if clm-mus-free drop else free-obj then ; is gen-free
' gen-free alias mus-free

\ csndlib.fs ends here
