\ csndlib.fs -- ffi to libsndlib.so -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Wed Aug 27 22:52:21 CEST 2003
\ Last: Fri Oct 08 02:27:58 CEST 2004
\ Ident: $Id: csndlib.fs,v 1.65 2004/10/08 01:51:49 mike Exp $

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

\ FFI interface to libsndlib.so.  This file assumes that libsndlib.so
\ was configured with float as Float (not double) and
\ --with-float-samples.

\ Code:

only forth also definitions
require gfm.fs
require fflib.fs
only forth also definitions also GFMusic
vocabulary CLM-Sndlib CLM-Sndlib definitions also GFMusic

\ If you have no $LD_LIBRARY_PATH, you may replace the following line
\ to reflect your path to libsndlib.so's location, e.g.
\
\ library sndlib /usr/local/lib/libsndlib.so

library sndlib libsndlib.so

\ === vct.h ===
sndlib c-make-vct int (ptr) c_make_vct ( len -- v )
: c>forth-vct { c-vct -- v }
    vct% %alloc { v }
    c-vct v v-orig !
    c-vct v-data @ v v-data !
    c-vct v-len @ v v-len !
    s" vct" v v-name 2!
    v
;
: make-vct ( len -- v ) assert1( dup 0>= ) c-make-vct c>forth-vct ;
: >vct ( u-float-values u -- v ) { len }
    len make-vct { v }
    len if v v-data @ 1 sfloats - len sfloats bounds swap u-do i sf! 1 sfloats -loop then
    v
;
sndlib v-free-vct ptr (ptr) c_free_vct ( v -- v' )
: free-vct ( v -- ) dup v-orig @ v-free-vct drop free throw ;
sndlib c-vct-copy ptr (ptr) c_vct_copy ( v1 -- v2 )
: vct-copy ( v1 -- v2 ) v-orig @ c-vct-copy c>forth-vct ;
sndlib c-vct-equalp ptr ptr (ptr) vct_equalp ( v1 v2 -- f )
: vct= ( v1 v2 -- f ) v-orig @ swap v-orig @ c-vct-equalp ;
sndlib vct-print ptr (ptr) vct_to_string ( v -- c-str )
: .vct ( v -- ) vct-print .cstring ;
: c-array>vct { addr len -- v } len make-vct addr over v-data ! ;

\ === sndlib2xen.h ===
sndlib c-make-sound-data int int (ptr) c_make_sound_data ( chans frames -- sd )
: make-sound-data { chns frms -- sd }
    assert1( chns 0> frms 0> and )
    chns frms c-make-sound-data { s }
    sd% %alloc { sd }
    s sd sd-orig !
    s sd-data @ sd sd-data !
    chns sd sd-chans !
    frms sd sd-len !
    s" sound-data" sd sd-name 2!
    sd
;
sndlib c-sound-data-free ptr (void) sound_data_free ( sd -- )
: sound-data-free ( sd -- ) dup sd-orig @ c-sound-data-free free throw ;
sndlib c-sound-data-equalp ptr ptr (int) sound_data_equalp ( sd1 sd2 -- f )
: sound-data= ( sd1 sd2 -- f ) sd-orig @ swap sd-orig @ c-sound-data-equalp ;
sndlib sound-data>string ptr (ptr) sound_data_to_string ( sd -- c-addr )
: .sound-data ( sd -- ) sound-data>string .cstring ;

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
    cell% field mus-any-environ
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
end-struct mus-any-class%

struct
    mus-any-class% field mus-any-core
end-struct mus-any%

sndlib init-module (void) init_mus_module ( -- )

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
: array-print-length! ( val -- ) mus-array-print-length! drop ;
sndlib mus-sine-bank ptr ptr int (sf) mus_sine_bank ( amps-ary phases-ary size -- r )
: sine-bank { amps phases -- r }
    amps vct-data phases vct-data amps vct-length phases vct-length min mus-sine-bank
;

sndlib ring-modulate sf sf (sf) mus_ring_modulate ( s1 s2 -- r )
sndlib amplitude-modulate sf sf sf (sf) mus_amplitude_modulate ( s1 s2 s3 -- r )
sndlib contrast-enhancement sf sf (sf) mus_contrast_enhancement ( sig index -- r )
sndlib mus-dot-product ptr ptr int (sf) mus_dot_product ( data1 data2 size -- r )
: dot-product { data1 data2 -- r }
    data1 vct-data data2 vct-data data1 vct-length data2 vct-length min mus-dot-product
;
sndlib mus-clear-array ptr int (void) mus_clear_array ( arr size -- )
: clear-array { ary -- } ary vct-data ary vct-length mus-clear-array ;
sndlib mus-polynomial ptr sf int (sf) mus_polynomial ( coeffs x ncoeffs -- r )
: polynomial ( coeffs x -- r ) dup vct-data swap vct-length mus-polynomial ;
sndlib mus-multiply-arrays ptr ptr int (void) mus_multiply_arrays ( data window len -- )
: multiply-arrays { rdat window -- }
    rdat vct-data window vct-data rdat vct-length window vct-length min mus-multiply-arrays
;
sndlib mus-rectangular>polar ptr ptr int (void) mus_rectangular_to_polar ( rl im size -- )
: rectangular>polar { rl im -- }
    rl vct-data im vct-data rl vct-length im vct-length min mus-rectangular>polar
;
sndlib mus-polar>rectangular ptr ptr int (void) mus_polar_to_rectangular ( rl im size -- )
: polar>rectangular { rl im -- }
    rl vct-data im vct-data rl vct-length im vct-length min mus-polar>rectangular
;
sndlib mus-array-interp ptr sf int (sf) mus_array_interp ( wave phase size -- r )
: array-interp { wave f: ph -- r } wave vct-data ph wave vct-length mus-array-interp ;

sndlib clm-mus-free ptr (int) mus_free ( gen -- f )
: gfm-mus-free ( gen -- ) clm-mus-free drop ;
sndlib describe ptr (ptr) mus_describe ( gen -- c-addr )
: .gfm-gen ( gen -- ) describe ." #<" .cstring ." >" ;
sndlib gfm-gen= ptr ptr (int) mus_equalp ( gen1 gen2 -- f )
sndlib phase@ ptr (sf) mus_phase ( gen -- r )
sndlib mus-phase! ptr sf (sf) mus_set_phase ( val gen -- r )
: phase! ( val gen -- ) mus-phase! fdrop ;
sndlib gfm-frequency@ ptr (sf) mus_frequency ( gen -- r )
sndlib mus-frequency! ptr sf (sf) mus_set_frequency ( val gen -- r )
: gfm-frequency! ( val gen -- ) mus-frequency! fdrop ;
sndlib gfm-mus-run ptr sf sf (sf) mus_run ( gen f: arg1 f: arg2 -- r )
sndlib mus-length@ ptr (llong) mus_length ( gen -- d )
: gfm-length@ ( gen -- n ) mus-length@ d>s ;
sndlib mus-length! ptr llong (llong) mus_set_length ( gen d -- d )
: gfm-length! ( n gen -- ) r> s>d >r mus-length! 2drop ;
sndlib mus-data@ ptr (ptr) mus_data ( gen -- w )
: data@ ( idx gen -- r ) mus-data@ swap sfloats + sf@ ;
: data>vct ( gen -- v ) dup mus-data@ swap mus-length@ d>s c-array>vct ;
sndlib mus-data! ptr ptr (ptr) mus_set_data ( gen data -- w )
: data! ( f: val idx gen -- ) mus-data@ swap sfloats + sf! ;
: vct>data ( v gen -- ) vct-data mus-data! ;
sndlib mus-name@ ptr (ptr) mus_name ( gen -- c-addr )
: name@ ( gen -- addr len ) mus-name@ cstring>sstring ;
sndlib gfm-scaler@ ptr (sf) mus_scaler ( gen -- r )
sndlib mus-scaler! ptr sf (sf) mus_set_scaler ( val gen -- r )
: gfm-scaler! ( val gen -- ) mus-scaler! fdrop ;
sndlib offset@ ptr (sf) mus_offset ( gen -- r )
sndlib mus-offset! ptr sf (sf) mus_set_offset ( val gen -- r )
: offset! ( val gen -- ) mus-offset! fdrop ;
sndlib width@ ptr (sf) mus_width ( gen -- r )
sndlib mus-width! ptr sf (sf) mus_set_width ( val gen -- r )
: width! ( val gen -- ) mus-width! fdrop ;
sndlib mus-file-name@ ptr (ptr) mus_file_name ( gen -- c-addr )
: file-name@ ( gen -- addr len ) mus-file-name@ cstring>sstring ;
sndlib interp-type@ ptr (int) mus_channels ( gen -- n )

sndlib make-oscil sf sf (ptr) mus_make_oscil ( freq phase -- gen )
sndlib ?oscil ptr (int) mus_oscil_p ( gen -- f )
sndlib oscil ptr sf sf (sf) mus_oscil ( fm pm gen -- r )

sndlib mus-make-sum-of-cosines int sf sf (ptr) mus_make_sum_of_cosines ( cosines freq phase -- gen )
: make-sum-of-cosines ( f: frq f: ph cosin -- gen ) assert1( dup 0> ) mus-make-sum-of-cosines ;
sndlib ?sum-of-cosines ptr (int) mus_sum_of_cosines_p ( gen -- f )
sndlib sum-of-cosines ptr sf (sf) mus_sum_of_cosines ( fm gen -- r )
sndlib mus-cosines@ ptr (llong) mus_hop ( gen -- d )
: cosines@ ( gen -- n ) mus-cosines@ d>s ;
sndlib mus-cosines! ptr llong (llong) mus_set_hop ( val gen -- d )
: cosines! ( val gen -- ) s>d mus-cosines! 2drop ;

sndlib mus-make-sum-of-sines int sf sf (ptr) mus_make_sum_of_sines ( sines freq phase -- gen )
: make-sum-of-sines ( f: frq f: ph cosin -- gen ) assert1( dup 0> ) mus-make-sum-of-sines ;
sndlib ?sum-of-sines ptr (int) mus_sum_of_sines_p ( gen -- f )
sndlib sum-of-sines ptr sf (sf) mus_sum_of_sines ( fm gen -- r )

sndlib mus-make-delay int ptr int int (ptr) mus_make_delay ( size line line-size type -- gen )
: make-delay ( size -- gen )
    assert1( dup 0 max-table-size within )
    dup nil swap mus-interp-none mus-make-delay
;
sndlib tap ptr sf (sf) mus_tap ( loc gen -- r )
sndlib delay-tick ptr sf (sf) mus_delay_tick ( input gen -- r )
sndlib ?delay ptr (int) mus_delay_p ( gen -- f )
sndlib delay ptr sf sf (sf) mus_delay ( input pm gen -- r )

( scaler size line line-size type -- gen )
sndlib mus-make-comb sf int ptr int int (ptr) mus_make_comb
: make-comb ( f: scl size -- gen )
    assert1( dup 0 max-table-size within )
    dup nil swap mus-interp-none mus-make-comb
;
sndlib ?comb ptr (int) mus_comb_p ( gen -- f )
sndlib comb ptr sf sf (sf) mus_comb ( input pm gen -- r )

( scaler size line line-size type -- gen )
sndlib mus-make-notch sf int ptr int int (ptr) mus_make_notch
: make-notch ( f: scl size -- gen )
    assert1( dup 0 max-table-size within )
    dup nil swap mus-interp-none mus-make-notch
;
sndlib ?notch ptr (int) mus_notch_p ( gen -- f )
sndlib notch ptr sf sf (sf) mus_notch ( input pm gen -- r )

\ ( backward forward size line line-size type -- gen )
sndlib mus-make-all-pass sf sf int ptr int int (ptr) mus_make_all_pass
: make-all-pass ( f: back f: ffw size -- gen )
    assert1( dup 0 max-table-size within )
    dup nil swap mus-interp-none mus-make-all-pass
;
sndlib ?all-pass ptr (int) mus_all_pass_p ( gen -- f )
sndlib all-pass ptr sf sf (sf) mus_all_pass ( input pm gen -- r )

sndlib mus-make-average int ptr (ptr) mus_make_average ( size line -- gen )
: make-average ( size -- gen ) assert1( dup 0 max-table-size within ) nil mus-make-average ;
sndlib ?average ptr (int) mus_average_p ( gen -- f )
sndlib average ptr sf (sf) mus_average ( gen input -- f )

sndlib feedforward@ ptr (sf) mus_scaler ( gen -- r )
sndlib mus-feedforward! ptr sf (sf) mus_set_scaler ( val gen -- r )
: feedforward! ( val gen -- ) mus-feedforward! fdrop ;
sndlib feedback@ ptr (sf) mus_increment ( gen -- r )
sndlib mus-feedback! ptr sf (sf) mus_set_increment ( val gen -- r )
: feedback! ( val gen -- ) mus-feedback! fdrop ;

\ ( freq phase wave wave-size type -- gen )
sndlib mus-make-table-lookup sf sf ptr int int (ptr) mus_make_table_lookup
: make-table-lookup { f: frq f: ph wav tp -- gen }
    assert1( wav ?vct )
    frq ph wav vct-data wav vct-length tp mus-make-table-lookup
;
sndlib ?table-lookup ptr (int) mus_table_lookup_p ( gen -- f )
sndlib table-lookup ptr sf (sf) mus_table_lookup ( fm gen -- r )

\ ( partials-data partials table table-size normalize -- w )
sndlib mus-partials>wave ptr int ptr int int (ptr) mus_partials_to_wave
: partials>wave { parts norm -- w' }
    parts vct-data parts vct-length 2/
    *table-size* make-vct { table }
    table vct-data *table-size*
    norm mus-partials>wave drop table
;
\ ( partials-data partials table table-size normalize -- w )
sndlib mus-phasepartials>wave ptr int ptr int int (ptr) mus_phase_partials_to_wave
: phase-partials>wave { parts norm -- w' }
    parts vct-data parts vct-length 3 /
    *table-size* make-vct { table }
    table vct-data *table-size*
    norm mus-phasepartials>wave drop table
;

sndlib make-sawtooth-wave sf sf sf (ptr) mus_make_sawtooth_wave ( freq amp phase -- gen )
sndlib ?sawtooth-wave ptr (int) mus_sawtooth_wave_p ( gen -- f )
sndlib sawtooth-wave ptr sf (sf) mus_sawtooth_wave ( fm gen -- r )

sndlib make-square-wave sf sf sf (ptr) mus_make_square_wave ( freq amp phase -- gen )
sndlib ?square-wave ptr (int) mus_square_wave_p ( gen -- f )
sndlib square-wave ptr sf (sf) mus_square_wave ( fm gen -- r )

sndlib make-triangle-wave sf sf sf (ptr) mus_make_triangle_wave ( freq amp phase -- gen )
sndlib ?triangle-wave ptr (int) mus_triangle_wave_p ( gen -- f )
sndlib triangle-wave ptr sf (sf) mus_triangle_wave ( fm gen -- r )

sndlib make-pulse-train sf sf sf (ptr) mus_make_pulse_train ( freq amp phase -- gen )
sndlib ?pulse-train ptr (int) mus_pulse_train_p ( gen -- f )
sndlib pulse-train ptr sf (sf) mus_pulse_train ( fm gen -- r )

sndlib rand-seed@ (int) mus_rand_seed ( -- u )
sndlib rand-seed! int (void) mus_set_rand_seed ( u -- )
sndlib mus-random sf (sf) mus_random ( amp -- r )
sndlib random sf (sf) mus_frandom ( amp -- r )
sndlib irandom int (int) mus_irandom ( amp -- n )

sndlib make-rand sf sf (ptr) mus_make_rand ( freq base -- gen )
( freq base dist size -- gen )
sndlib mus-make-rand-dist sf sf ptr int (ptr) mus_make_rand_with_distribution
: make-rand-dist ( f: frq f: amp dist -- gen )
    assert1( dup vct-length 1 max-table-size within )
    inverse-integrate vct-data 512 mus-make-rand-dist
;
sndlib ?rand ptr (int) mus_rand_p ( gen -- f )
sndlib rand ptr sf (sf) mus_rand ( fm gen -- r )

sndlib make-rand-interp sf sf (ptr) mus_make_rand_interp ( freq base -- gen )
( freq base dist size -- gen )
sndlib mus-make-rand-interp-dist sf sf ptr int (ptr) mus_make_rand_interp_with_distribution
: make-rand-interp-dist ( f: frq f: amp dist -- gen )
    assert1( dup vct-length 1 max-table-size within )
    inverse-integrate vct-data 512 mus-make-rand-interp-dist
;
sndlib ?rand-interp ptr (int) mus_rand_interp_p ( gen -- f )
sndlib rand-interp ptr sf (sf) mus_rand_interp ( fm gen -- r )

( f: freq f: phase f: r f: ratio -- gen )
sndlib make-asymmetric-fm sf sf sf sf (ptr) mus_make_asymmetric_fm
sndlib ?asymmetric-fm ptr (int) mus_asymmetric_fm_p ( gen -- f )
sndlib asymmetric-fm ptr sf sf (sf) mus_asymmetric_fm ( index fm gen -- r )

sndlib make-one-zero sf sf (ptr) mus_make_one_zero ( a0 a1 -- gen )
sndlib ?one-zero ptr (int) mus_one_zero_p ( gen -- f )
sndlib one-zero ptr sf (sf) mus_one_zero ( input gen -- r )

sndlib make-one-pole sf sf (ptr) mus_make_one_pole ( a0 b1 -- gen )
sndlib ?one-pole ptr (int) mus_one_pole_p ( gen -- f )
sndlib one-pole ptr sf (sf) mus_one_pole ( input gen -- r )

sndlib make-two-zero sf sf sf (ptr) mus_make_two_zero ( a0 a1 a2 -- gen )
sndlib make-zpolar sf sf (ptr) mus_make_zpolar ( radius freq -- gen )
sndlib ?two-zero ptr (int) mus_two_zero_p ( gen -- f )
sndlib two-zero ptr sf (sf) mus_two_zero ( input gen -- r )

sndlib make-two-pole sf sf sf (ptr) mus_make_two_pole ( a0 b1 b2 -- gen )
sndlib make-ppolar sf sf (ptr) mus_make_ppolar ( radius freq -- gen )
sndlib ?two-pole ptr (int) mus_two_pole_p ( gen -- f )
sndlib two-pole ptr sf (sf) mus_two_pole ( input gen -- r )

sndlib make-formant sf sf sf (ptr) mus_make_formant ( radius freq gain -- gen )
sndlib ?formant ptr (int) mus_formant_p ( gen -- f )
sndlib formant ptr sf (sf) mus_formant ( input gen -- r )
sndlib mus-formant-bank ptr ptr sf int (sf) mus_formant_bank ( amps formants inval size -- r )
: formant-bank { amps formants f: inval -- r }
    amps vct-data formants vct-data inval amps vct-length mus-formant-bank
;
sndlib formant-radius@ ptr (sf) mus_phase ( gen -- r )
sndlib mus-formant-radius! ptr sf (sf) mus_set_phase ( val gen -- r )
: formant-radius! ( val gen -- r ) mus-formant-radius! fdrop ;

\ ( f: freq f: phase n f: a f: b_ratio -- gen )
sndlib make-sine-summation sf sf int sf sf (ptr) mus_make_sine_summation
sndlib ?sine-summation ptr (int) mus_sine_summation_p ( gen -- f )
sndlib sine-summation ptr sf (sf) mus_sine_summation ( input gen -- r )

sndlib mus-make-filter int ptr ptr ptr (ptr) mus_make_filter ( order xcoeffs ycoeffs state -- gen )
: make-filter { order xcoeffs ycoeffs -- gen }
    assert1( order 0> xcoeffs ?vct ycoeffs ?vct and and )
    xcoeffs vct-length ycoeffs vct-length max { len }
    order xcoeffs vct-data ycoeffs vct-data len make-vct vct-data
    mus-make-filter
;
sndlib ?filter ptr (int) mus_filter_p ( gen -- f )
sndlib filter ptr sf (sf) mus_filter ( input gen -- r )

sndlib mus-make-fir-filter int ptr ptr (ptr) mus_make_fir_filter ( order xcoeffs state -- gen )
: make-fir-filter { order xcoeffs -- gen }
    assert1( order 0> xcoeffs ?vct and )
    order xcoeffs vct-data xcoeffs vct-length make-vct vct-data mus-make-fir-filter
;
sndlib ?fir-filter ptr (int) mus_fir_filter_p ( gen -- f )
sndlib fir-filter ptr sf (sf) mus_fir_filter ( input gen -- r )

sndlib mus-make-iir-filter int ptr ptr (ptr) mus_make_iir_filter ( order ycoeffs state -- gen )
: make-iir-filter { order ycoeffs -- gen }
    assert1( order 0> ycoeffs ?vct and )
    order ycoeffs vct-data ycoeffs vct-length make-vct vct-data mus-make-iir-filter
;
sndlib ?iir-filter ptr (int) mus_iir_filter_p ( gen -- f )
sndlib iir-filter ptr sf (sf) mus_iir_filter ( input gen -- r )
sndlib mus-make-fir-coeffs int ptr ptr (ptr) mus_make_fir_coeffs ( order env aa -- float-array )
: make-fir-coeffs { ord v1 -- v2 } ord v1 vct-data nil mus-make-fir-coeffs ord c-array>vct ;

sndlib mus-xcoeffs@ ptr (ptr) mus_xcoeffs ( gen -- w )
: gfm-xcoeffs@ ( gen -- w ) dup mus-xcoeffs@ swap gfm-length@ c-array>vct ;
sndlib mus-ycoeffs@ ptr (ptr) mus_ycoeffs ( gen -- w )
: ycoeffs@ ( gen -- w ) dup mus-ycoeffs@ swap gfm-length@ c-array>vct ;
sndlib xcoeff@ ptr int (sf) mus_xcoeff ( gen index -- r )
sndlib mus-xcoeff! ptr int sf (sf) mus_set_xcoeff ( gen index f: val -- r )
: xcoeff! ( gen index f: val -- ) mus-xcoeff! fdrop ;
sndlib ycoeff@ ptr int (sf) mus_ycoeff ( gen index -- r )
sndlib mus-ycoeff! ptr int sf (sf) mus_set_ycoeff ( gen index f: val -- r )
: ycoeff! ( gen index f: val -- ) mus-ycoeff! fdrop ;
sndlib order@ ptr (int) mus_length ( gen -- n )
: a0@ ( gen -- r ) 0 xcoeff@ ;
: a0! ( f: val gen -- ) 0 xcoeff! ;
: a1@ ( gen -- r ) 1 xcoeff@ ;
: a1! ( f: val gen -- ) 1 xcoeff! ;
: a2@ ( gen -- r ) 2 xcoeff@ ;
: a2! ( f: val gen -- ) 2 xcoeff! ;
: b1@ ( gen -- r ) 1 ycoeff@ ;
: b1! ( f: val gen -- ) 1 ycoeff! ;
: b2@ ( gen -- r ) 2 ycoeff@ ;
: b2! ( f: val gen -- ) 2 ycoeff! ;

( freq phase wave size type -- gen )
sndlib mus-make-wave-train sf sf ptr int int (ptr) mus_make_wave_train
: make-wave-train { f: frq f: ph wave tp -- gen }
    assert1( wave ?vct )
    frq ph wave vct-data wave vct-length tp mus-make-wave-train
;
sndlib ?wave-train ptr (int) mus_wave_train_p ( gen -- f )
sndlib wave-train ptr sf (sf) mus_wave_train ( fm gen -- r )

\ ( npartials partials kind -- w )
sndlib mus-partials>polynomial int ptr int (ptr) mus_partials_to_polynomial
: partials>polynomial { parts kind -- w }
    parts array>partials { part }
    part vct-length { len }
    len part vct-data kind mus-partials>polynomial len c-array>vct
;
\ ( npartials partials size table -- w )
sndlib mus-partials>waveshape int ptr int ptr (ptr) mus_partials_to_waveshape
: partials>waveshape { parts size -- w }
    parts array>partials { part }
    part vct-length { len }
    len part vct-data size nil mus-partials>waveshape *table-size* c-array>vct
;

sndlib mus-make-waveshape sf sf ptr int (ptr) mus_make_waveshape ( freq phase table size -- gen )
: make-waveshape { f: freq parts -- gen }
    assert1( parts ?vct )
    parts *table-size* partials>waveshape { table }
    freq 0e table vct-data table vct-length mus-make-waveshape
;
sndlib ?waveshape ptr (int) mus_waveshape_p ( gen -- f )
sndlib waveshape ptr sf sf (sf) mus_waveshape ( index fm gen -- r )

\ ( brkpts pts scaler offset base duration start end odata -- gen )
sndlib mus-make-env ptr int sf sf sf sf llong llong ptr (ptr) mus_make_env
: make-env { ary f: scl f: dur -- gen }
    ary vct-data ary vct-length 2/ scl 0e 1e dur 0. 0. nil mus-make-env
;
: make-envelope { ary f: scl f: dur f: offset f: base start end -- gen }
    assert1( ary ?vct start 0>= end 0>= and and )
    ary vct-data ary vct-length 2/ scl offset base dur start s>d end s>d nil mus-make-env
;
sndlib ?env ptr (int) mus_env_p ( gen -- f )
sndlib env ptr (sf) mus_env ( gen -- r )
sndlib restart-env ptr (void) mus_restart_env ( gen -- )
sndlib env-interp sf ptr (sf) mus_env_interp ( x gen -- r )
sndlib position@ ptr (int) mus_channels ( gen -- n )

sndlib mus-make-frame int (ptr) mus_make_empty_frame ( chans -- gen )
: make-frame ( chns -- gen ) assert1( dup 0> ) mus-make-frame ;
sndlib ?frame ptr (int) mus_frame_p ( gen -- f )
sndlib mus-frame+ ptr ptr ptr (ptr) mus_frame_add ( fr1 fr2 res -- gen )
: frame+ ( fr1 fr2 -- gen ) nil mus-frame+ ;
sndlib mus-frame* ptr ptr ptr (ptr) mus_frame_multiply ( fr1 fr2 res -- gen )
: frame* ( fr1 fr2 -- gen ) nil mus-frame* ;
sndlib mus-frame@ ptr int (sf) mus_frame_ref ( gen chan -- r )
: frame@ ( chan gen -- r ) swap mus-frame@ ;
sndlib mus-frame! ptr int sf (sf) mus_frame_set ( gen chan val -- r )
: frame! ( f: val chan gen -- ) swap mus-frame! fdrop ;
sndlib mus-frame-data ptr (ptr) mus_frame_data ( gen -- w )
: frame>vct ( gen -- v ) dup mus-frame-data swap gfm-length@ c-array>vct ;
: vct>frame { v -- gen }
    v vct-length { chns }
    chns make-frame { gen }
    chns 0 do i v vct@ i gen frame! loop
    gen
;
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
sndlib ?mixer ptr (int) mus_mixer_p ( gen -- f )
sndlib mus-mixer@ ptr int int (sf) mus_mixer_ref ( gen in out -- r )
: mixer@ ( in out gen -- r ) -rot mus-mixer@ ;
sndlib mus-mixer! ptr int int sf (sf) mus_mixer_set ( gen in out val -- r )
: mixer! ( f: val in out gen -- ) -rot mus-mixer! fdrop ;
sndlib mus-frame>frame ptr ptr ptr (ptr) mus_frame_to_frame ( gen1 in out -- gen2 )
: frame>frame ( fr1 mx -- fr2 ) swap nil mus-frame>frame ;
sndlib mus-sample>frame ptr sf ptr (ptr) mus_sample_to_frame ( gen1 in out -- gen2 )
: sample>frame ( f: val mx -- fr ) nil mus-sample>frame ;
sndlib mus-frame>sample ptr ptr (sf) mus_frame_to_sample ( gen in -- r )
: frame>sample ( fr mx -- r ) swap mus-frame>sample ;
sndlib mus-mixer* ptr ptr ptr (ptr) mus_mixer_multiply ( mx1 mx2 res -- gen )
: mixer* ( mx1 mx2 -- gen ) nil mus-mixer* ;
sndlib mus-mixer+ ptr ptr ptr (ptr) mus_mixer_add ( mx1 mx2 res -- gen )
: mixer+ ( mx1 mx2 -- gen ) nil mus-mixer+ ;
sndlib mus-mixer-scale ptr sf ptr (ptr) mus_mixer_scale ( mx f: scl res -- gen )
: mixer-scale ( f: scl mx -- gen ) nil mus-mixer-scale ;
: >mixer ( chans-in-out-values chans -- gen )
    { chns }
    chns make-mixer { gen }
    -1 chns 1- -do -1 chns 1- -do j i gen mixer! 1 -loop 1 -loop
    gen
;

sndlib mus-make-file>sample ptr (ptr) mus_make_file_to_sample ( filename -- gen )
: make-file>sample ( d: fname -- gen ) c-string mus-make-file>sample ;
sndlib ?file>sample ptr (int) mus_file_to_sample_p ( gen -- f )
sndlib mus-file>sample ptr llong int (sf) mus_file_to_sample ( gen samp chan -- r )
: file>sample { samp chan gen -- r } gen samp s>d chan mus-file>sample ;

sndlib mus-make-readin ptr int llong int (ptr) mus_make_readin ( filename chan start dir -- gen )
: make-readin { d: fname chan beg dir -- gen }
    assert1( chan 0>= )
    fname c-string chan beg s>d dir mus-make-readin
;
sndlib ?readin ptr (int) mus_readin_p ( gen -- f )
sndlib readin ptr (sf) mus_readin ( gen -- r )
sndlib gfm-increment@ ptr (sf) mus_increment ( gen -- r )
sndlib mus-increment! ptr sf (sf) mus_set_increment ( val gen -- r )
: gfm-increment! ( val gen -- ) mus-increment! fdrop ;
: increment+! ( val gen -- ) dup gfm-increment@ f+ gfm-increment! ;
sndlib mus-location@ ptr (llong) mus_location ( gen -- d )
: location@ ( gen -- u ) mus-location@ d>s ;
sndlib mus-location! ptr llong (llong) mus_set_location ( gen val -- d )
: location! ( val gen -- ) swap s>d mus-location! 2drop ;
sndlib channel@ ptr (int) mus_channel ( gen -- n )

sndlib ?output ptr (int) mus_output_p ( gen -- f )
sndlib ?input ptr (int) mus_input_p ( gen -- f )
sndlib mus-in-any llong int ptr (sf) mus_in_any ( frame chan IO -- r )
: in-any { frame chan gen -- r } frame s>d chan gen mus-in-any ;
: ina { frame gen -- r } frame s>d 0 gen mus-in-any ;
: inb { frame gen -- r } frame s>d 1 gen mus-in-any ;

sndlib mus-make-file>frame ptr (ptr) mus_make_file_to_frame ( filename -- gen )
: make-file>frame ( d: fname -- gen ) c-string mus-make-file>frame ;
sndlib ?file>frame ptr (int) mus_file_to_frame_p ( gen -- f )
sndlib mus-file>frame ptr llong ptr (ptr) mus_file_to_frame ( gen1 samp f -- gen2 )
: file>frame ( samp gen -- fr ) swap s>d nil mus-file>frame ;

\ ( filename chans format type comment -- gen )
sndlib mus-make-sample>file ptr int int int ptr (ptr) mus_make_sample_to_file_with_comment
: make-sample>file { d: fname chans fmt typ d: com -- gen }
    assert1( chans 0> )
    fname c-string chans fmt typ com c-string mus-make-sample>file
;
sndlib ?sample>file ptr (int) mus_sample_to_file_p ( gen -- f )
sndlib mus-sample>file ptr llong int sf (sf) mus_sample_to_file ( gen samp chan val -- r )
: sample>file { f: val samp chan gen -- } gen samp s>d chan val mus-sample>file fdrop ;
sndlib mus-continue-sample>file ptr (ptr) mus_continue_sample_to_file ( filename -- gen )
: continue-sample>file ( d: fname -- gen ) c-string mus-continue-sample>file ;
sndlib mus-close-file ptr (int) mus_close_file ( gen -- n )
: mus-close ( gen -- ) mus-close-file 0< abort" can't close file" ;

sndlib mus-out-any llong sf int ptr (sf) mus_out_any ( frame val chan IO -- r )
: out-any { frame f: val chan gen -- } frame s>d val chan gen mus-out-any fdrop ;
: outa { frame f: val gen -- } frame s>d val 0 gen mus-out-any fdrop ;
: outb { frame f: val gen -- } frame s>d val 1 gen mus-out-any fdrop ;
: outc { frame f: val gen -- } frame s>d val 2 gen mus-out-any fdrop ;
: outd { frame f: val gen -- } frame s>d val 3 gen mus-out-any fdrop ;

\ ( filename chans format type -- gen )
sndlib mus-make-frame>file ptr int int int (ptr) mus_make_frame_to_file
: make-frame>file { d: fname chns fmt tp -- gen }
    assert1( chns 0> )
    fname c-string chns fmt tp mus-make-frame>file
;
sndlib ?frame>file ptr (int) mus_frame_to_file_p ( gen -- f )
sndlib mus-frame>file ptr llong ptr (ptr) mus_frame_to_file ( gen1 samp data -- gen2 )
: frame>file { fr samp gen -- } gen samp s>d fr mus-frame>file drop ;
sndlib mus-continue-frame>file ptr (ptr) mus_continue_frame_to_file ( filename -- gen )
: continue-frame>file ( d: fname -- gen ) c-string mus-continue-frame>file ;

\ ( degree distance reverb chans output revout type -- gen )
sndlib mus-make-locsig sf sf sf int ptr ptr int (ptr) mus_make_locsig
: make-locsig { f: degr f: dist f: reverb chns output revput tp -- gen }
    assert1( chns 0> )
    degr dist reverb chns output revput tp mus-make-locsig
;
sndlib ?locsig ptr (int) mus_locsig_p ( gen -- f )
sndlib mus-locsig ptr llong sf (ptr) mus_locsig ( gen loc val -- gen )
: locsig ( val offset gen -- ) swap s>d mus-locsig drop ;
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

sndlib mus-spectrum ptr ptr ptr int int (ptr) mus_spectrum ( rdat idat window n type -- rdat' )
: spectrum { rdat idat window type -- }
    rdat vct-data idat vct-data window vct-data rdat vct-length type mus-spectrum drop
;
sndlib mus-fft ptr ptr int int (void) mus_fft ( rl im n is -- )
: fft { rl im len isign -- } rl vct-data im vct-data len isign mus-fft ;
sndlib mus-make-fft-window int int sf (ptr) mus_make_fft_window ( type size beta -- w )
: make-fft-window { type size f: beta -- w } type size beta mus-make-fft-window size c-array>vct ;
\ ( type size beta window -- w )
sndlib mus-convolution ptr ptr int (ptr) mus_convolution ( rl1 rl2 n -- w )
: convolution { rdat idat -- } rdat vct-data idat vct-data rdat vct-length mus-convolution drop ;
( file1 file2 maxamp outfile -- )
sndlib mus-convolve-files ptr ptr sf ptr (void) mus_convolve_files
: convolve-files { d: fnam1 d: fnam2 f: mamp d: ofile -- }
    fnam1 c-string fnam2 c-string mamp ofile c-string mus-convolve-files
;

sndlib file-buffer-size@ (int) mus_file_buffer_size ( -- n )
sndlib mus-file-buffer-size! int (int) mus_set_file_buffer_size ( size -- n )
: file-buffer-size! ( size -- ) mus-file-buffer-size! drop ;

( file chan st samps ary -- n )
sndlib mus-file>float-array ptr int llong int ptr (int) mus_file_to_float_array
: file>array { d: fname chan st samps -- data }
    samps make-vct { ary }
    fname c-string chan st s>d samps ary vct-data mus-file>float-array drop
    ary
;
( file data len srate chans -- n )
sndlib mus-float-array>file ptr ptr int int int (int) mus_float_array_to_file
: array>file { data len srate chans d: fname -- }
    fname c-string data vct-data len srate chans mus-float-array>file drop
;

sndlib mus-make-ssb-am sf int (ptr) mus_make_ssb_am ( freq order -- gen )
: make-ssb-am ( f: frq ord -- gen ) assert1( dup 0> ) mus-make-ssb-am ;
sndlib ?ssb-am ptr (int) mus_ssb_am_p ( gen -- f )
sndlib ssb-am ptr sf sf (sf) mus_ssb_am ( gen insig fm -- r )

\ === sndlib.h ===
\ --- sound.c ---
sndlib mus-sound-samples ptr (llong) mus_sound_samples ( filename -- d )
: sound-samples ( d: fname -- n ) c-string mus-sound-samples d>s ;
sndlib mus-sound-frames ptr (llong) mus_sound_frames ( filename -- d )
: sound-frames ( d: fname -- n ) c-string mus-sound-frames d>s ;
sndlib mus-sound-chans ptr (int) mus_sound_chans ( filename -- n )
: sound-chans ( d: fname -- n ) c-string mus-sound-chans ;
sndlib mus-sound-srate ptr (int) mus_sound_srate ( filename -- n )
: sound-srate ( d: fname -- n ) c-string mus-sound-srate ;
sndlib mus-sound-header-type ptr (int) mus_sound_header_type ( filename -- n )
: sound-header-type ( d: fname -- n ) c-string mus-sound-header-type ;
sndlib mus-sound-data-format ptr (int) mus_sound_data_format ( filename -- n )
: sound-data-format ( d: fname -- n ) c-string mus-sound-data-format ;
sndlib mus-sound-length ptr (llong) mus_sound_length ( filename -- u )
: sound-length ( d: fname -- u ) c-string mus-sound-length d>s ;
sndlib mus-header-type-name int (ptr) mus_header_type_name ( type -- c-str )
: .header-type ( type -- ) mus-header-type-name .cstring ;
sndlib mus-data-format-name int (ptr) mus_data_format_name ( format -- c-str )
: .data-format ( format -- ) mus-data-format-name .cstring ;
sndlib mus-sound-comment ptr (ptr) mus_sound_comment ( filename -- c-str )
: sound-comment ( filename -- addr u|f )
    c-string mus-sound-comment ?dup-if cstring>sstring else nil then
;
sndlib mus-sound-duration ptr (sf) mus_sound_duration ( filename -- r )
: sound-duration ( d: fname -- r ) c-string mus-sound-duration ;
sndlib mus-sound-open-input ptr (int) mus_sound_open_input ( filename -- fd )
: sound-open-input ( d: fname -- n )
    c-string mus-sound-open-input dup 0< abort" sound-open-input: can't open sound input"
;
\ ( filename srate chans format type comment -- fd )
sndlib mus-sound-open-output ptr int int int int ptr (int) mus_sound_open_output
: sound-open-output { d: fname srate chans fmt type d: com -- n }
    fname c-string srate chans fmt type com c-string mus-sound-open-output
    dup 0< abort" sound-open-output: can't open sound output"
;
sndlib mus-sound-close-input int (int) mus_sound_close_input ( fd -- n )
: sound-close-input ( fd -- ) mus-sound-close-input 0< abort" can't close sound input" ;
sndlib mus-sound-close-output int llong (int) mus_sound_close_output ( fd bytes-of-data -- n )
: sound-close-output ( fd bytes -- ) s>d mus-sound-close-output drop ;
sndlib mus-sound-read int int int int ptr (int) mus_sound_read ( fd beg end chans bufs -- n )
: sound-read ( fd beg end chns sd -- n ) sd-data @ mus-sound-read ;
sndlib mus-sound-write int int int int ptr (int) mus_sound_write ( fd beg end chans bufs -- n )
: sound-write ( fd beg end chns sd -- n ) sd-data @ mus-sound-write ;

\ --- audio.c ---
0 value *data-format*

sndlib mus-audio-initialize (int) mus_audio_initialize ( -- n )
: audio-initialize ( -- ) mus-audio-initialize 0< abort" can't initialize audio module" ;

sndlib audio-describe (void) mus_audio_describe ( -- )
sndlib mus-audio-report (ptr) mus_audio_report ( -- c-str )
: .audio ( -- ) mus-audio-report .cstring ;
\ ( dev srate chans format size -- fd )
sndlib mus-audio-open-input int int int int int (int) mus_audio_open_input
: audio-open-input { dev sr chns frmt size -- fd }
    assert1( sr 0> chns 0> and )
    frmt to *data-format*
    dev sr chns frmt size mus-audio-open-input
    dup 0< abort" audio-open-input: can't open input device"
;
\ ( dev srate chans format size -- fd )
sndlib mus-audio-open-output int int int int int (int) mus_audio_open_output
: audio-open-output { dev sr chns frmt size -- fd }
    assert1( sr 0> chns 0> and )
    frmt to *data-format*
    dev sr chns frmt size mus-audio-open-output
    dup 0< abort" audio-open-output: can't open output device"
;
sndlib mus-audio-close int (int) mus_audio_close ( line -- n )
: audio-close ( line -- )
    mus-audio-close 0< abort" audio-close: can't close device"
    0 to *data-format*
;
\ ( line frames chans bufs format -- n )
sndlib audio-read-buffers int int int ptr int (int) mus_audio_read_buffers
: audio-read { line sd bytes -- n }
    line bytes sd sound-data-chans sd sd-data @ *data-format* audio-read-buffers
;
\ ( line frames chans bufs format clipped -- n )
sndlib audio-write-buffers int int int ptr int int (int) mus_audio_write_buffers
: audio-write { line sd bytes -- n }
    line bytes sd sound-data-chans sd sd-data @ *data-format* true audio-write-buffers
;
sndlib audio-systems (int) mus_audio_systems ( -- n )
sndlib mus-audio-system-name int (ptr) mus_audio_system_name ( sys -- c-str )
: .audio-system ( sys -- ) mus-audio-system-name .cstring ;
sndlib mus-audio-moniker (ptr) mus_audio_moniker ( -- c-str )
: .audio-moniker ( -- ) mus-audio-moniker .cstring ;

init-module
audio-initialize

require gfm-gens.fs

\ csndlib.fs ends here
