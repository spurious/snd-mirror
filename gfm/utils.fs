\ utils.fs -- utility definitions -*- forth -*-

\ Copyright (C) 2003--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Thu Aug 28 00:28:20 CEST 2003
\ Last: Thu Jan 13 17:30:58 CET 2005
\ Ident: $Id: utils.fs,v 1.155 2005/01/13 16:34:24 mike Exp $

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

\ General purpose classes and functions which may be useful in other
\ Forth programs too.
\ 
\ Object (scattered around the file)
\ String
\ Array
\ Float
\ Complex
\ Hash
\ getopts
\ help
\ float additions
\ short
\ Timer
\ Pathname
\ several...

\ Code:

require lib.fs

only forth also definitions
vocabulary Utils
also Utils definitions

\ === Object ===
\
\ This is the base struct.  Vct, Sound-data, Array, Hash, String and
\ others derive their structs from this.  So we have a common inspect
\ function for all objects as well as common free and equal functions.
\ Fsndlib.fs includes this struct in his music5% struct and therefore
\ all CLM-functions can use the same inspect, equal and free function
\ too.  Csndlib.fs has a similar integration.

struct
    cell% field gfm-base-inspect
    cell% field gfm-base-to-string
    cell% field gfm-base-equal
    cell% field gfm-base-free
    cell% field gfm-base-run
    cell% field gfm-base-name
end-struct gfm-base%

gfm-base%
    cell% field obj-len
    cell% field obj-data
    cell% field obj-cycle
    cell% field obj-fetch
    cell% field obj-store
    cell% field obj-plus-store
end-struct obj%

: length ( obj -- n ) obj-len @ ;
: data-ptr ( obj -- n ) obj-data @ ;
: cycle  { obj -- val }
    obj obj-cycle @ obj dup obj-fetch @ execute	\ result
    obj obj-cycle @ 1+ obj obj-len @ < if 1 obj obj-cycle +! else 0 obj obj-cycle ! then
;
: ?range ( idx obj -- f ) 0 swap obj-len @ within ;
: ?empty ( obj -- f ) obj-len @ 0= ;
: (object-index) ( idx obj -- idx' obj )
    assert1( depth 2 >= ) over 0< if dup obj-len @ rot + swap then ;
: object@  ( idx obj -- val ) (object-index) assert1( 2dup ?range ) dup obj-fetch @ execute ;
: object!  ( val idx obj -- ) (object-index) assert1( 2dup ?range ) dup obj-store @ execute ;
: object+! ( val idx obj -- ) (object-index) assert1( 2dup ?range ) dup obj-plus-store @ execute ;

\ EACH END-EACH; MAP END-MAP are defined below Array

defer .gen
defer gen>string ( GEN>STRING )
defer gen=       ( GEN= )
defer gen-free   ( GEN-FREE )

: make-?obj  ( str "name" --; obj self -- f )
    create ,
  does> ( obj self -- f )
    try @ swap gfm-base-name @ = recover 2drop drop false endtry
;

\ === QSort (see ARRAY-SORT! (utils.fs) and VCT-SORT! (gfm-defs.fs) ===
: qsort recursive { obj left right comp-xt swap-xt -- }
    left right < if
	left { last }
	obj left left right + 2/ swap-xt execute
	right 1+ left 1+ ?do
	    obj i left comp-xt execute if
		last 1+ to last
		obj last i swap-xt execute
	    then
	loop
	obj left last swap-xt execute
	obj left last 1- comp-xt swap-xt qsort
	obj last 1+ right comp-xt swap-xt qsort
    then
;
\ may work with String and Array, all sorted
\ returns index or -1
: binary-search { read-xt val obj len -- idx|-1 }
    0 { left }
    len { right }
    -1
    begin
	right left >=
    while
	    left right + 2/ { middle }
	    val middle obj read-xt execute = if drop middle exit then
	    val middle obj read-xt execute < if middle 1- to right else middle 1+ to left then
    repeat
;

: unless postpone 0= postpone if ; immediate
: [unless] 0= postpone [if] ;
: lambda-create noname create ;
' :noname alias lambda:

\ === String ===
obj%
end-struct string%

' object@  alias string@
' object!  alias string!
\ EACH and MAP are defined below Array

: string-fetch  ( idx str -- c ) obj-data @ swap chars + c@ ;
: string-store  ( c idx str -- ) obj-data @ swap chars + c! ;
: string-first  ( str -- c ) assert( dup obj-len @ 0> )  0 swap string-fetch ;
: string-second ( str -- c ) assert( dup obj-len @ 1 > ) 1 swap string-fetch ;
: string-third  ( str -- c ) assert( dup obj-len @ 2 > ) 2 swap string-fetch ;
: string-last   ( str -- c ) dup obj-len @ 1- swap string-fetch ;
: string>$      ( str -- addr len ) dup obj-data @ swap obj-len @ ;
: (strings>$)   ( str1 str2 -- addr1 u1 addr2 u2 ) swap string>$ rot string>$ ;

: (make-simple-string) { addr len -- str }
    string% %alloc { str }
    len chars allocate throw dup len chars blank str obj-data !
    addr str obj-data @ len chars move
    len str obj-len !
    str
;

s" String" (make-simple-string) constant str-string
str-string str-string gfm-base-name !
str-string make-?obj ?string

: string= { str1 str2 -- f }
    str1 ?string str2 ?string and if
	str1 str2 (strings>$) str=
    else
	false
    then
;
: string< { str1 str2 -- f }
    str1 ?string str2 ?string and if
	str1 str2 (strings>$) str<
    else
	false
    then
;
: str> ( addr1 len1 addr2 len2 -- f ) compare 0> ;
: string> { str1 str2 -- f }
    str1 ?string str2 ?string and if
	str1 str2 (strings>$) str>
    else
	false
    then
;
: free-string { str -- }
    str ?string if
	str obj-data @ free throw
	nil str gfm-base-name !
	str free throw
    then
;
: make-simple-string ( addr len -- str )
    (make-simple-string) { str }
    ['] string-fetch str obj-fetch !
    ['] string-store str obj-store !
    ['] string=      str gfm-base-equal !
    ['] free-string  str gfm-base-free !
    str-string       str gfm-base-name !
    str
;
\ .STRING is defered and defined as (.STRING) in gforth-0.6.2/see.fs
:noname ( str -- )
    dup ?string if
	dup obj-data @ swap obj-len @ type
    else
	depth 3 >= if
	    ( c-addr u n -- )
	    (.string)			\ defined in see.fs
	else
	    drop ." #<null>"
	then
    then
; is .string
: .$ ( str -- ) dup ?string if dup obj-data @ swap obj-len @ else drop s\" \"\"" then type ;
: $(.string) { str -- str2 }
    str ?string if
	str obj-len @ 2 + dup allocate throw swap make-simple-string { str2 }
	[char] " 0 str2 string!
	str obj-len @ 0 ?do i str string@ i 1+ str2 string! loop
	[char] " str2 obj-len @ 1- str2 string!
	str2
    else
	s\" \"\"" make-simple-string
    then
;
: .string-inspect ( str -- ) $(.string) .$ ;
: make-string { len -- str }
    assert1( len 0>= )
    string% %alloc { str }
    len                 str obj-len !
    len chars allocate throw dup len chars blank str obj-data !
    0                   str obj-cycle !
    ['] string-fetch    str obj-fetch !
    ['] string-store    str obj-store !
    ['] .string-inspect str gfm-base-inspect !
    ['] $(.string)      str gfm-base-to-string !
    ['] string=         str gfm-base-equal !
    ['] free-string     str gfm-base-free !
    str-string          str gfm-base-name !
    str
;
: $>string { addr len -- str }
    len make-string { str }
    addr str obj-data @ len chars move
    str
;
: string-search { s1 str -- s2|false }
    str s1 (strings>$) search if $>string else 2drop false then
;
\ $" foo\nbar" value str
:noname \"-parse $>string ;
:noname postpone s\" postpone $>string ;
interpret/compile: $"

: string>c$ { str -- addr }
    str obj-len @ { len }
    len 1+ chars allocate throw { addr }
    addr len 1+ chars erase
    str obj-data @ addr len chars move
    addr
;
: $c>string ( addr -- str ) cstring>sstring $>string ;
: string+ { str1 str2 -- str3 }
    str1 obj-len @ { len1 }
    str2 obj-len @ { len2 }
    len1 len2 + make-string { str3 }
    str1 obj-data @ str3 obj-data @ len1 chars move
    str2 obj-data @ str3 obj-data @ len1 chars + len2 chars move
    str3
;

' ?string  alias ?$
' string>$ alias $>
' $>string alias >$
' string=  alias $=
' string+  alias $+

: bs ( -- ) #bs emit ;
: $nil ( -- str ) 0 make-string ;

$" \n" constant $cr
$" \b" constant $bs
$"  "  constant $space

\ simple debug help;
\ usage: obj1 obj2 2 .debug
\        prints string representations or values of obj1 and obj2
\        3.2e 1 .debug ==> 3.2000
\        make-oscil make-env 2 .debug ==> #<env ...> #<oscil ... >

: script-cr  ( -- ) script?     if cr then ;
: !script-cr ( -- ) script? unless cr then ;
: .debug ( ?? n -- )
    !script-cr
    depth 1 >= if
	." \ === DEBUG[" dup . bs ." ]: " 0 ?do gen>string .$ space loop ." ==="
    else
	." \   Usage: .DEBUG ( ?? n -- )" cr
	." \          At least one argument is required!" cr
	." \ Example: 0 .DEBUG" cr
	." \          make-oscil  10 make-vct  2  .DEBUG"
    then
    script-cr
;
\ defered in gforth-0.6.2/debugs.fs for debugging function `~~'
: (gfm-printdebugdata) f.s .s ;
' (gfm-printdebugdata) is printdebugdata
\ stolen from gforth-0.6.2/source.fs
: gfm-.sourcepos ( file line -- )
    swap loadfilename#>str type ." ["
    base @ decimal swap 0 .r ." ]" base !
;
: (gfm-.debugline)
    !script-cr ." \ === " gfm-.sourcepos ." : float " f.s ." data " .s ." ===" script-cr
;
' (gfm-.debugline) is .debugline

\ other object functions are defined below Hash
: ?obj      ( obj -- f ) try gfm-base-name @ ?string recover 2drop false endtry ;
: make-inspect ( fct "name" --; obj self -- )
    create ,
  does> { obj self -- }
    obj ?obj if obj self @ execute .$ else ." #<null>" then
;

$" WARNING: " constant str-warning
$" ERROR: "   constant str-error
$" DIE: "     constant str-die
: warn ( str -- )  !script-cr str-warning swap $+ .string script-cr ;
: error ( str -- ) !script-cr str-error   swap $+ string>$ exception throw ;
: die ( str -- )   !script-cr str-die     swap $+ .string script-cr 1 (bye) ;

\ === Floats ===
: fvalue ( val "name" --; addr -- val )
    create f,
  does> ( addr -- val )
    f@
;
:noname ' >body f! ;
:noname ' >body postpone aliteral postpone f! ;
interpret/compile: fto ( value "name" -- )

\ usage: so-lib-exists? sndlib libsndlib.so [if] require csndlib.fs [else] require fsndlib.fs [then]
: so-lib-exists? ( "name" "library" -- f ) try true library recover drop false endtry ;

: (u.)  ( u -- addr len ) 0 <# #s #> ;
: (.)   ( n -- addr len ) s>d swap over dabs <<# #s rot sign #> #>> ;
: $(u.) ( u -- str ) (u.) $>string ;
: $(.)  ( n -- str ) (.) $>string ;
: s>f   ( n -- r ) s>d d>f ;
: f>s   ( r -- n ) f>d d>s ;
\ F.R is similar to D.R and friends
\ usage: 17e 3 f.r --> 17.000
: (f.r) { f: val u -- }
    val f0< { s }
    val fabs to val
    u s>f { f: dig }
    dig 2e f+ fnegate falog 5e f* val f+ to val
    2e dig falog f* 1/f val f+ dig falog f* f>d	\ (1 / (2 * 10^u) + r) * 10^u
    <# u 0 ?do # loop [char] . hold #s s sign #>
;
: $(f.r) ( val u -- ) (f.r) $>string ;
\ stolen from share/gforth/0.6.2/float.fs
: $zeros ( n -- str ) $nil swap 0 max 0 ?do $" 0" $+ loop ;
: my-f$ ( r -- str n )
    $nil
    scratch represent unless 2drop scratch 3 min $>string $+ dup length rdrop exit then
    if >r $" -" $+ r> then
;
: $(f.)  ( val -- str )
    my-f$ dup >r 0<= if
	$" 0" $+
    else
	scratch r@ min $>string $+ r@ precision - $zeros $+
    then $" ." $+ r@ negate $zeros $+
    scratch r> 0 max /string 0 max -zeros $>string $+
;
: (f.) ( val -- addr u ) $(f.) string>$ ;
: f.r    ( val u -- ) (f.r) type space ;
: uf.r   ( val len-all len-after-comma -- ) 0 f.rdp ;
: f+!    ( val addr -- ) dup f@ f+ f! ;
: sf+!   ( val addr -- ) dup sf@ f+ sf! ;
: df+!   ( val addr -- ) dup df@ f+ df! ;
: flogn { f: r f: b -- r }
    r f0<= if s" flogn: r must be > 0" exception throw then
    b f0<= if s" flogn: b must be > 0" exception throw then
    r fln b fln f/
;
: fceil ( r1 -- r2 ) 1e f+ floor ;
: truncate { f: r1 -- n r2 } r1 f>s { n } r1 n s>f f- n ;
: prime { val -- f }
    val 2 =
    val 2 mod
    true val s>f fsqrt f>s 1+ 3 do val i mod unless drop false leave then 2 +loop and or
;
: froll ( r0 r1 ... rn n -- r1 ... rn r0 )
    dup fpick { f: val } floats fp@ float+ dup 1 floats - swap rot move fdrop val
;

\ x = a / b; result = x * b - floor(x) * b
: my-fmod ( f: a f: b ) fswap fover f/ fover fover f* frot frot floor f* f- ;

[ifdef] av-call-int
    so-lib-exists? libm libm.so [if]
	libm fmod sf sf (sf) fmodf
    [else]
	' my-fmod alias fmod
    [then]
[else]
    ' my-fmod alias fmod
[then]

\ === Array ===
\ Usage: array[ 0 0 100 1 ] value ary
\        ary .array     --> #<array[4]: 0 0 100 1>
\        33 0 ary vct!
\        0 ary array@ . --> 33
\        ary .array     --> #<array[4]: 33 0 100 1>

obj%
    cell% field ary-buf-len
end-struct array%

$" Array" constant str-array
8 value *array-print-length*
128 value *clm-array-buffer-length*

str-array make-?obj ?array

' object@  alias array@
' object!  alias array!
' object+! alias array+!

: array-fetch      ( idx ary -- n )   obj-data @ swap cells + @ ;
: array-store      ( val idx ary -- ) obj-data @ swap cells + ! ;
: array-plus-store ( val idx ary -- ) 2dup 2>r array-fetch + 2r> array-store ;
: array-first  ( ary -- n ) dup obj-len @ 0>  if 0 swap array-fetch else drop nil then ;
: array-second ( ary -- n ) dup obj-len @ 1 > if 1 swap array-fetch else drop nil then ;
: array-third  ( ary -- n ) dup obj-len @ 2 > if 2 swap array-fetch else drop nil then ;
: array-last   ( ary -- n ) dup obj-len @ 1- swap array-fetch ;
: array-index  { val ary -- idx|-1 }
    -1 ary obj-len @ 0 ?do i ary array-fetch val = if drop i leave then loop
;
: array-index-bs ( val ary -- idx|-1 ) ['] array@ -rot dup length 1- binary-search ;
\ xt ( -- result )
: array-map! { xt ary -- } ary obj-len @ 0 ?do xt execute i ary array-store loop ;
\ xt ( val -- result )
: array-each! { xt ary -- }
    ary obj-len @ 0 ?do i ary array-fetch xt execute i ary array-store loop
;
\ xt ( val idx -- result )
: array-each-with-index! { xt ary -- }
    ary obj-len @ 0 do i ary array-fetch i xt execute i ary array-store loop
;
: array-fill! { val ary -- } ary obj-len @ 0 ?do val i ary array-store loop ;
: ?array-member ( val ary -- f ) array-index -1 <> ;
: ?array-member-bs ( val ary -- f ) array-index-bs -1 <> ;
: array-clear { ary -- } ary obj-data @ ary obj-len @ cells erase ;
: array-max   { ary -- n } 0 ary obj-len @ 0 ?do i ary array-fetch max loop ;
: array-min   { ary -- n } 0 ary obj-len @ 0 ?do i ary array-fetch min loop ;
: $(.array) { ary -- str }
    $" #<" ary ?array if
	*array-print-length* { len }
	ary obj-len @ { alen }
	str-array $+ $" [" $+ alen $(.) $+ $" ]" $+
	alen if
	    len alen min 0 do $space $+ ary obj-data @ i cells + @ gen>string $+ loop
	    alen len > if $"  ..." $+ then
	then
    else
	$" null" $+
    then $" >" $+
; 
' $(.array) make-inspect .array
\ user function
: free-array { ary -- }
    ary ?array if
	ary obj-data @ free throw
	nil ary gfm-base-name !
	ary free throw
    then
;
\ used by gen-free
: array-free { ary -- }
    ary ?array if
	ary obj-len @ 0 ?do i ary array-fetch gen-free loop
	ary obj-data @ free throw
	nil ary gfm-base-name !
	ary free throw
    then
;
: array=       { ary1 ary2 -- f }
    ary1 ary2 = if
	true
    else
	ary1 obj-len @ ary2 obj-len @ = if
	    true
	    ary1 obj-len @ 0 ?do
		i ary1 array-fetch i ary2 array-fetch <> if drop false leave then
	    loop
	else
	    false
	then
    then
;
: make-simple-array { len -- ary }
    assert1( len 0>= )
    array% %alloc { ary }
    len *clm-array-buffer-length* / 1+ *clm-array-buffer-length* * { buf-len }
    buf-len cells allocate throw { data }
    buf-len   	         ary ary-buf-len !
    len       	         ary obj-len !
    data      	         ary obj-data !
    0                    ary obj-cycle !
    ['] array-fetch      ary obj-fetch !
    ['] array-store      ary obj-store !
    ['] array-plus-store ary obj-plus-store !
    ['] .array           ary gfm-base-inspect !
    ['] $(.array)        ary gfm-base-to-string !
    ['] array=           ary gfm-base-equal !
    ['] array-free       ary gfm-base-free !
    str-array            ary gfm-base-name !
    ary
;
: make-array   { len -- w }
    len make-simple-array { ary }
    len if ary obj-data @ len cells erase then
    ary
;
: >array       ( data u -- w ) { len }
    len make-simple-array { ary }
    len if ary obj-data @ cell - len cells bounds swap u-do i ! cell -loop then
    ary
;
\ array[ 0 1 2 3 ] value ary
: (set-array)  ( addr u -- ary ) depth 2 - >r evaluate depth r> - >array ;
:noname [char] ] parse (set-array) ;
:noname [char] ] parse postpone sliteral postpone (set-array) ;
interpret/compile: array[ immediate

: array-copy   { ary1 -- ary2 }
    ary1 obj-len @ { len }
    len make-simple-array { ary2 }
    ary1 obj-data @ ary2 obj-data @ len cells move
    ary2
;
: c-array>array { addr len -- ary }
    len make-simple-array { ary }
    addr ary obj-data @ len cells move
    ary
;
: array-push!  { val ary -- }
    1 ary obj-len +!
    ary obj-len @ ary ary-buf-len @ > if
	ary ary-buf-len @ *clm-array-buffer-length* + { buf-len }
	ary obj-data @ buf-len cells resize throw ary obj-data !
	buf-len ary ary-buf-len !
    then
    val ary obj-data @ ary obj-len @ 1- cells + !
;
: array-pop!   { ary -- val }
    ary obj-len @ unless
	0
    else
	-1 ary obj-len +!
	ary obj-data @ ary obj-len @ cells + @
    then
;
: array-unshift! { val ary -- }
    1 ary obj-len +!
    ary obj-len @ ary ary-buf-len @ > if
	ary ary-buf-len @ *clm-array-buffer-length* + { buf-len }
	ary obj-data @ buf-len cells resize throw ary obj-data !
	buf-len ary ary-buf-len !
    then
    ary obj-data @ ary obj-data @ cell+ ary obj-len @ 1- cells move
    val ary obj-data @ !
;
: array-shift! { ary -- val }
    ary obj-len @ unless
	0
    else
	-1 ary obj-len +!
	ary obj-data @ @
	ary obj-data @ cell+ ary obj-data @ ary obj-len @ cells move
    then
;
: array-reverse! { ary -- }
    ary obj-len @ { len }
    len 1 > if
	len 1- { idx }
	len cells allocate throw { data }
	len 0 u+do ary obj-data @ i cells + @  data idx cells + !  idx 1- to idx loop
	ary obj-data @ free throw
	data ary obj-data !
    then
;
: array-delete! { idx ary -- val }
    idx 0 ary obj-len @ within if
	ary obj-data @ idx cells + @	\ value
	ary obj-data @ idx 1+ cells + ary obj-data @ idx cells + ary obj-len @ idx - cells move
	-1 ary obj-len +!
    else
	nil
    then
;
: array-insert! { val idx ary -- }
    idx 0 ary obj-len @ within if
	1 ary obj-len +!
	ary obj-len @ ary ary-buf-len @ > if
	    ary ary-buf-len @ *clm-array-buffer-length* + { buf-len }
	    ary obj-data @ buf-len cells resize throw ary obj-data !
	    buf-len ary ary-buf-len !
	then
	ary obj-data @ idx cells + ary obj-data @ idx 1+ cells + ary obj-len @ idx - cells move
	val ary obj-data @ idx cells + !
    then
;
: array-and { ary1 ary2 -- ary3 }
    0 make-array { ary3 }
    ary1 length ary2 length >= if
	ary1 obj-len @ 0 ?do i ary1 array-fetch ary2 array-index 0>=
	    if
		i ary1 array@ ary3 array-push!
	    then
	loop
    else
	ary2 obj-len @ 0 ?do i ary2 array-fetch ary1 array-index 0>= if
		i ary2 array@ ary3 array-push!
	    then
	loop
    then
    ary3
;
: array>stack { ary -- values } ary obj-len @ 0 ?do i ary array-fetch loop ;
: (array-swap) { ary i-1 i-2 -- }
    i-1 ary array-fetch i-2 ary array-fetch i-1 ary array-store i-2 ary array-store
;
: (array-compare) { cmp-xt -- xt; ary i-1 i-2 addr -- f }
    lambda-create cmp-xt , latestxt
  does> { ary i-1 i-2 addr -- f }
    i-1 ary array-fetch i-2 ary array-fetch addr @ execute 
;
\ array[ -3 4 100 -80 ] ' < array-sort!
: array-sort!  ( cmp-xt ary -- )
    assert1( dup ?array ) dup 0 swap obj-len @ 1- 3 roll (array-compare) ['] (array-swap) qsort
;
: array-sort     ( cmp-xt ary1 -- ary2 ) array-copy swap over array-sort! ;
: array-sort-<!  ( ary -- )       ['] < swap array-sort! ;
: array-sort-<   ( ary1 -- ary2 ) ['] < swap array-sort ;
: array-sort->!  ( ary -- )       ['] > swap array-sort! ;
: array-sort->   ( ary1 -- ary2 ) ['] > swap array-sort ;
: array-sort-$<! ( ary -- )       ['] string< swap array-sort! ;
: array-sort-$<  ( ary1 -- ary2 ) ['] string< swap array-sort ;
: array-sort-$>! ( ary -- )       ['] string> swap array-sort! ;
: array-sort-$>  ( ary1 -- ary2 ) ['] string> swap array-sort ;
: array-uniq   { ary1 -- ary2 }
    ary1 ?empty if
	0 make-array
    else
	0 ary1 array@ 1 >array { ary2 }
	ary1 obj-len @ 1 do
	    i ary1 array@ dup ary2 ?array-member if drop else ary2 array-push! then
	loop
	ary2
    then
;
: array-uniq!  { ary -- }
    ary array-uniq { ary2 }
    ary2 obj-len @ ary obj-len !
    ary2 obj-data @ ary obj-data @ ary2 obj-len @ cells move
    ary2 free-array
;
: make-2array   { len -- w }
    len 2* make-simple-array { ary }
    len if ary obj-data @ len 2* cells erase then
    ary
;
: 2array@ ( idx ary -- d )   assert1( 2dup ?range ) obj-data @ swap 2* cells + 2@ ;
: 2array! ( val idx ary -- ) assert1( 2dup ?range ) obj-data @ swap 2* cells + 2! ;

\ === Each/Map (String, Array, Hash, Vct) ===
user *map-array*
0 make-array *map-array* !

: object-set-loop   ( obj -- len ) dup *map-array* @ array-push! obj-len @ ;
: object-reset-each ( -- )         *map-array* @ array-pop! drop ;
: object-reset-map  ( -- )         *map-array* @ array-pop! ;
: object-fetch      ( idx -- )     *map-array* @ array-last object@ ;
: object-store      ( val idx -- ) *map-array* @ array-last object! ;

: each ( obj -- )
    postpone object-set-loop 0 postpone literal postpone ?do
    postpone r@ postpone object-fetch
; immediate
: end-each ( -- ) postpone loop postpone object-reset-each ; immediate

: map ( ary -- ) postpone object-set-loop 0 postpone literal postpone ?do ; immediate
: end-map ( val -- obj )
    postpone r@ postpone object-store postpone loop
    postpone object-reset-map
; immediate

\ === Float ===
gfm-base%
    sfloat% field sfl-value
end-struct sfl%
$" Float" constant str-float

: float@ ( sfl -- r ) sfl-value sf@ ;
: float! ( r sfl -- ) sfl-value sf! ;
: float+! ( r sfl -- ) dup sfl-value sf@ f+ sfl-value sf! ;

str-float make-?obj ?float

: free-float ( sfl -- ) dup ?float if free throw else drop then ;
: $(.float) ( sfl -- str ) sfl-value sf@ 4 $(f.r) ;
: .float ( sfl -- ) ." #<" str-float .string space $(.float) .string ." >" ;

: make-float { f: val -- sfl }
    sfl% %alloc { sfl }
    val            sfl sfl-value sf!
    ['] free-float sfl gfm-base-free !
    ['] f=         sfl gfm-base-equal !
    ['] $(.float)  sfl gfm-base-to-string !
    ['] .float     sfl gfm-base-inspect !
    str-float      sfl gfm-base-name !
    sfl
;
\ >FLOAT exists
' make-float alias float>float

\ === Complex ===
gfm-base%
    sfloat% field cp-real
    sfloat% field cp-image
end-struct complex%
$" Complex" constant str-complex

: real@  ( cp -- r )   cp-real sf@ ;
: image@ ( cp -- r )   cp-image sf@ ;
: real!  ( val cp -- ) cp-real sf! ;
: image! ( val cp -- ) cp-image sf! ;

str-complex make-?obj ?complex
: complex-free ( cp -- ) free throw ;
: complex= { cp1 cp2 -- f }
    cp1 ?complex cp2 ?complex and if
	cp1 cp2 =
	cp1 real@ cp2 real@ f=
	cp2 image@ cp2 image@ f= and or
    else
	false
    then
;
: $(.complex) { cp -- str }
    cp cp-real sf@ 3 $(f.r) $" +(" $+ cp cp-image sf@ 3 $(f.r) $+ $" )i" $+
;
: .complex { cp -- }
    ." #<" str-complex .string
    ."  real: " cp cp-real sf@ 3 f.r bs
    ." , image: " cp cp-image sf@ 3 f.r bs ." >"
;
: make-complex { f: re f: im -- cp }
    complex% %alloc { cp }
    re               cp cp-real sf!
    im               cp cp-image sf!
    ['] complex-free cp gfm-base-free !
    ['] complex=     cp gfm-base-equal !
    ['] .complex     cp gfm-base-inspect !
    ['] $(.complex)  cp gfm-base-to-string !
    str-complex      cp gfm-base-name !
    cp
;
: make-complex-polar { f: r f: theta -- cp }
    r theta make-complex { cp }
    r theta fcos f* cp cp-real sf!
    r theta fsin f* cp cp-image sf!
    cp
;
: complex-conjugate { cp1 -- cp2 }
    cp1 cp-real sf@ cp1 cp-image sf@ fnegate make-complex
;
: complex-abs2 { cp -- f: val }
    cp cp-real sf@ fdup f* cp cp-image sf@ fdup f* f+
;
: complex+ { cp1 cp2 -- cp3 }
    assert1( cp1 ?complex cp1 ?float or cp2 ?complex and )
    cp1 ?complex if
	cp1 cp-real sf@ cp2 cp-real sf@ f+
	cp1 cp-image sf@ cp2 cp-image sf@ f+ make-complex
    else
	cp1 float@ cp2 cp-real sf@ f+ cp2 cp-image sf@ make-complex
    then
;
: complex- { cp1 cp2 -- cp3 }
    assert1( cp1 ?complex cp1 ?float or cp2 ?complex and )
    cp1 ?complex if
	cp1 cp-real sf@ cp2 cp-real sf@ f-
	cp1 cp-image sf@ cp2 cp-image sf@ f- make-complex
    else
	cp1 float@ cp2 cp-real sf@ f- cp2 cp-image sf@ make-complex
    then
;
: complex* { cp1 cp2 -- cp3 }
    assert1( cp1 ?complex cp1 ?float or cp2 ?complex and )
    cp1 ?complex if
	cp1 cp-real sf@ cp2 cp-real sf@ f* cp1 cp-image sf@ cp2 cp-image sf@ f* f-
	cp1 cp-real sf@ cp2 cp-image sf@ f* cp1 cp-image sf@ cp2 cp-real sf@ f* f+ make-complex
    else
	cp1 float@ cp2 cp-real sf@ f* cp1 float@ cp2 cp-image sf@ f* make-complex
    then
;
: complex/ { cp1 cp2 -- cp3 }
    assert1( cp2 ?complex cp2 ?float or cp1 ?complex and )
    cp2 ?complex if
	cp1 cp2 complex-conjugate complex* { cp3 }
	cp2 complex-abs2 { f: val }
	cp3 cp-real sf@ val f/ cp3 cp-real sf!
	cp3 cp-image sf@ val f/ cp3 cp-image sf!
	cp3
    else
	cp1 cp-real sf@ cp2 float@ f/ cp1 cp-image sf@ cp2 float@ f/ make-complex
    then
;

\ === Hash ===
\ Usage: MAKE-HASH  value hs
\        $" osc"  make-oscil make-oscil make-oscil 3 >array  hs  HASH!
\        $" osc"  hs HASH@  each  0e 0e oscil f. cr  end-each
\        hs FREE-HASH
obj%
    cell% field hash-keys-array
    cell% field hash-values-array
    cell% field hash-has-changed
end-struct hash%

gfm-base%
    cell% field hash-entry-key
    cell% field hash-entry-value
    cell% field hash-entry-next
end-struct hash-entry%

$" Hash"       constant str-hash
$" Hash-entry" constant str-hash-entry
101 constant *utils-hash-size*

' length   alias hash-length
' data-ptr alias hash-data
' ?empty   alias ?hash-empty

str-hash-entry make-?obj ?hash-entry
: hash-entry-free { gen -- f } gen ?hash-entry if nil gen gfm-base-name ! true else false then ;

str-hash make-?obj ?hash
: free-hash { hash -- }
    hash ?hash if
	hash obj-data @ { ary }
	ary each { h } begin h hash-entry-free while h hash-entry-next @ to h repeat end-each
	ary free-array
	hash hash-keys-array @ free-array
	hash hash-values-array @ free-array
	nil hash gfm-base-name !
	hash free throw
    then
;
: $(.hash) { hash -- str }
    $" #<" str-hash $+
    $" [" $+ hash obj-len @ gen>string $+ $" ]" $+
    hash obj-len @ if
	hash obj-data @ each { h }
	    h ?hash-entry if
		begin
		    h ?hash-entry if
			$space $+ h hash-entry-key @ gen>string $+ $"  ==> " $+
			h hash-entry-value @ gen>string $+ $" ," $+
		    then
		    h hash-entry-next @ to h
		    h ?hash-entry 0=
		until
	    then
	end-each
	$bs $+
    then
    $" >" $+
;
' $(.hash) make-inspect .hash
: hash= { h1 h2 -- f }
    h1 ?hash h2 ?hash and if
	h1 h2 =
    else
	false
    then
;
\ while (s++)
\     hval = *s + 64 * hval;
\ hval % hash_size;
: calculate-hash ( key -- hval )
    0 swap gen>string each swap 64 * + end-each s>d *utils-hash-size* um/mod drop
;
: make-hash-entry { key val hash -- }
    hash-entry% %alloc { h }
    key                 h hash-entry-key !
    val                 h hash-entry-value !
    ['] hash-entry-free h gfm-base-free !
    str-hash-entry      h gfm-base-name !
    key calculate-hash { hval }
    hval hash obj-data @ array@ h hash-entry-next !
    h hval hash obj-data @ array!
    true hash hash-has-changed !
;
: hash! { key val hash -- }
    assert1( hash ?hash )
    key val hash make-hash-entry
    1 hash obj-len +!
;
: hash@ { key hash -- val }
    assert1( hash ?hash )
    key calculate-hash hash obj-data @ array@ { h }
    h ?hash-entry if
	nil
	begin
	    h hash-entry-key @ key gen= if
		drop
		h hash-entry-value @
		true
	    else
		h hash-entry-next @ to h
		h ?hash-entry 0=
	    then
	until
    else
	nil
    then
;
: fhash@ ( key hash -- val ) hash@ float@ ;
: fhash! { key f: val hash -- } key val float>float hash hash! ;
: hash-delete { key hash -- }
    assert1( hash ?hash )
    key calculate-hash { hval }
    hash obj-data @ { ary }
    hval ary array@ dup { h prev }
    h ?hash-entry if
	begin
	    h if
		h hash-entry-key @ key gen= if
		    h hash-entry-free drop
		    h prev = if
			h hash-entry-next @ hval ary array!
		    else
			h hash-entry-next @ prev hash-entry-next !
		    then
		    -1 hash obj-len +!
		    true hash hash-has-changed !
		    true
		else
		    h to prev
		    h hash-entry-next @ to h
		    h ?hash-entry 0=
		then
	    else
		true
	    then
	until
    then
;

: hash-set-keys-values { hash -- }
    hash obj-len @ if
	hash obj-len @ make-array { keys }
	hash obj-len @ make-array { values }
	0 { idx }
	hash obj-data @ each { h }
	    h ?hash-entry if
		begin
		    h ?hash-entry if
			h hash-entry-key   @ idx keys array!
			h hash-entry-value @ idx values array!
			idx 1+ to idx
		    then
		    h hash-entry-next @ to h
		    h ?hash-entry 0=
		until
	    then
	end-each
	keys   hash hash-keys-array !
	values hash hash-values-array !
    then
    false hash hash-has-changed !
;
: hash-keys { hash -- keys-ary }
    hash hash-has-changed @ if hash hash-set-keys-values then
    hash hash-keys-array @
;
: hash-values { hash -- values-ary }
    hash hash-has-changed @ if hash hash-set-keys-values then
    hash hash-values-array @
;
: hash-fetch { idx hash -- key value }
    idx hash hash-keys   array@
    idx hash hash-values array@
;
: hash-store ( key value idx hash -- ) nip make-hash-entry ;

: make-hash { -- hash }
    hash% %alloc { hash }
    0                 hash obj-len !
    *utils-hash-size* make-array hash obj-data !
    ['] hash-fetch    hash obj-fetch !
    ['] hash-store    hash obj-store !
    nil               hash hash-keys-array !
    nil               hash hash-values-array !
    true              hash hash-has-changed !
    ['] .hash         hash gfm-base-inspect !
    ['] $(.hash)      hash gfm-base-to-string !
    ['] hash=         hash gfm-base-equal !
    ['] free-hash     hash gfm-base-free !
    str-hash          hash gfm-base-name !
    hash
;
\ for MAP END-MAP
\ 10 MAKE-HASH-WITH-SIZE { hs }
\ hs MAP ... key value END-MAP
\ hs .gen cr
: make-hash-with-size ( n -- hash )
    make-hash { hash }
    hash obj-len !
    hash
;

\ === Object ===
$" missing object" constant str-missing-object
: .inspect  ( obj -- )
    depth 1 >= if
	dup ?obj if dup gfm-base-inspect @ execute else . bs then
    else
	fdepth 1 >= if
	    make-float { sfl } sfl dup gfm-base-inspect @ execute sfl free-float
	else
	    str-missing-object warn
	then
    then
;
: obj-equal ( obj1 obj2 -- f ) try over gfm-base-equal @ execute recover drop = endtry ;
: free-obj  ( obj -- )   dup ?obj if dup gfm-base-free @ execute else drop then ;
: obj-run   ( r1 r2 gen -- r3 )
    dup ?obj if dup gfm-base-run @ execute else fdrop fdrop drop 0e then
;
: $(.obj>string) ( obj -- str )
    depth 1 >= if
	try dup gfm-base-to-string @ execute recover drop $(.) endtry
    else
	fdepth 1 >= if
	    make-float { sfl } sfl dup gfm-base-to-string @ execute sfl free-float
	else
	    str-warning str-missing-object $+
	then
    then
;

' .inspect       is .gen
' $(.obj>string) is gen>string
' obj-equal      is gen=
' free-obj       is gen-free

: [each] { obj -- }
    >in @
    obj length 0 ?do
	i (i) ! dup >r >in !
	(i) @ obj object@
	interpret
    r> swap +loop
    drop
; immediate
' [loop] alias [end-each]
: [map] { obj -- obj' }
    >in @
    obj length 0 ?do
	i (i) ! dup >r >in !
	interpret
	obj ?hash if
	    -rot
	else
	    depth 2 >= if swap then
	then
	(i) @ obj object!
    r> swap +loop
    drop
    obj
; immediate
' [loop] alias [end-map]

\ === Getopts ===
variable *clm-argc* argc @ *clm-argc* !
variable *clm-argv*
:noname ( -- )
    argc @ cells allocate throw { ary }
    argc @ 0 do argv @ i cells + @ ary i cells + ! loop
    ary *clm-argv* !
; execute

: remove-arg { n -- }
    *clm-argv* @ n 1+ cells +   *clm-argv* @ n cells +   *clm-argc* @ n - cells move
    2 allocate throw dup 2 erase *clm-argv* @ *clm-argc* @ cells + !
    -1 *clm-argc* +!
;

: get-bool-opts { opt -- f }
    false
    *clm-argc* @ 2 >= if
	*clm-argc* @ 2 ?do
	    opt *clm-argv* @ i cells + @ $c>string string= if
		i remove-arg
		drop true leave
	    then
	loop
    then
;

: get-string-opts { opt -- str2 f }
    false { flag }
    *clm-argc* @ 2 >= if
	*clm-argc* @ 2 ?do
	    opt *clm-argv* @ i cells + @ $c>string string= if
		i 1+ *clm-argc* @ < if
		    *clm-argv* @ i 1+ cells + @ $c>string true to flag
		    i remove-arg
		    i remove-arg
		    leave
		else
		    $" getopts: option " opt gen>string $+ $"  needs an argument" $+ error
		then
	    then
	loop
    then
    flag
;

: get-number-opts ( opt -- n|r f ) get-string-opts dup if >r string>$ evaluate r> then ;

$" opt-bool"   constant opt-bool
$" opt-string" constant opt-string
$" opt-number" constant opt-number

: getopts { data kind -- ?? f }
    data ?array if
	false { flag }
	data each
	    kind case
		opt-bool   of get-bool-opts   to flag endof
		opt-string of get-string-opts to flag endof
		opt-number of get-number-opts to flag endof
		false swap
	    endcase
	    flag ?leave
	end-each
	flag
    else
	data
	kind case
	    opt-bool   of get-bool-opts   endof
	    opt-string of get-string-opts endof
	    opt-number of get-number-opts endof
	    false swap
	endcase
    then
;

\ === Online Help ===
make-hash value *clm-online-help*
$" no description available " constant str-help-no-description
$" unknown word "             constant str-help-unknown-word
: help! ( xt online-help -- ) *clm-online-help* hash! ;
: help@ { xt -- str }
    depth { dep }
    xt *clm-online-help* hash@ ?dup-if $cr swap $+ else str-help-no-description then { str }
    depth dep - 0 ?do drop loop		\ xts from csndlib (create-does words) disturb the stack
    str
;
: help ( "name" -- )
    parse-word find-name ?dup-if name>int help@ else str-help-unknown-word then .string
;

\ === Data Type Short ===
2 constant short
: shorts ( n1 -- n2 ) short * ;
: short+ ( addr1 -- addr2 ) 1 shorts + ;

\ === Timer ===
gfm-base%
    dfloat% field timer-real
    dfloat% field timer-user
    dfloat% field timer-system
    dfloat% field timer-result-real
    dfloat% field timer-result-user
    dfloat% field timer-result-system
end-struct timer%
$" Timer" constant str-timer

: real-time@   ( tm -- r ) timer-result-real df@ ;
: user-time@   ( tm -- r ) timer-result-user df@ ;
: system-time@ ( tm -- r ) timer-result-system df@ ;

str-timer make-?obj ?timer
: timer-free { tm -- } tm ?timer if tm free throw then ;
: $(.timer) { tm -- }
    $" #<" str-timer $+
    $"  real: " $+ tm timer-result-real df@ 3 $(f.r) $+
    $" , user: " $+ tm timer-result-user df@ 3 $(f.r) $+
    $" , system: " $+ tm timer-result-system df@ 3 $(f.r) $+
    $" >" $+
;

: .timer { tm -- }
    ."  real: " tm real-time@ 3 f.r bs
    ."   (utime " tm user-time@ 3 f.r bs
    ." , stime " tm system-time@ 3 f.r bs ." )"
;
: .timer-ratio { srate frames tm -- }
    frames 0> if
	srate s>f frames s>f f/
    else
	1e
    then { f: m }
    ." ratio: " tm real-time@ m f* 2 f.r bs
    ."   (uratio " tm user-time@ m f* 2 f.r bs ." )"
;
' $(.timer) make-inspect .timer-inspect

: dtime>ftime d>f 1000000e f/ ;
: frtime  ( -- r ) utime dtime>ftime ;
: futime  ( -- r ) cputime 2drop dtime>ftime ;
: init-timer { tm -- }
    utime { d: rt }
    cputime { d: ut d: st }
    rt dtime>ftime tm timer-real df!
    ut dtime>ftime tm timer-user df!
    st dtime>ftime tm timer-system df!
;
: make-timer ( -- tm )
    timer% %alloc { tm }
    tm init-timer
    0e 		       tm timer-result-real df!
    0e 		       tm timer-result-user df!
    0e 		       tm timer-result-system df!
    ['] timer-free     tm gfm-base-free !
    ['] .timer-inspect tm gfm-base-inspect !
    ['] $(.timer)      tm gfm-base-to-string !
    str-timer          tm gfm-base-name !
    tm
;
: start-timer { tm -- }
    tm init-timer
;
: stop-timer { tm -- }
    utime { d: rt }
    cputime { d: ut d: st }
    rt dtime>ftime tm timer-real   df@ f- tm timer-result-real   df!
    ut dtime>ftime tm timer-user   df@ f- tm timer-result-user   df!
    st dtime>ftime tm timer-system df@ f- tm timer-result-system df!
;

\ === Others ===
: str>upper       ( c-addr u -- ) bounds do i c@ toupper emit 1 chars +loop ;
: str>capitalize  ( c-addr u -- ) over c@ toupper emit bounds char+ do i c@ emit 1 chars +loop ;
: get-rcs-version ( addr u1 -- addr u2 ) 11 /string 2dup s"  $" search drop blank -trailing ;
\ Sun Dec 12 22:27:57 CET 2004
nil $" Jan" $" Feb" $" Mar" $" Apr" $" May" $" Jun"
$" Jul" $" Aug" $" Sep" $" Oct" $" Nov" $" Dec" 13 >array constant *utils-month-names*
: date>string ( -- str )
    time&date { nsec nmin nhour nday nmonth nyear }
    nmonth *utils-month-names* array@ $space $+
    nday   0 <# # #s #> $>string $+ $space $+
    nyear  0 <# #s #> $>string $+ $space $+
    nhour  0 <# # #s #> $>string $+ $" :" $+
    nmin   0 <# # #s #> $>string $+ $" :" $+
    nsec   0 <# # #s #> $>string $+
;
: .cstring ( w -- ) cstring>sstring type ;
\ convert forth string (addr len) to null terminated c-string (addr)
: c-string { addr1 len -- adr' }
    len 1+ allocate throw { addr2 }
    addr2 len 1+ erase
    addr1 addr2 len chars move
    addr2
;

\ Returns a forth string containing the output of the command `cmd'.
\ $" pwd" shell .$ --> /home/mike
1024 cells constant shell-buffer-size
: shell { cmd -- output }
    cmd string>$ r/o open-pipe throw { fd }
    shell-buffer-size allocate throw { output }
    output shell-buffer-size erase
    output shell-buffer-size fd read-file throw { len }
    fd close-pipe throw drop
    output len 1- $>string		\ remove trailing `\n'
    output free throw
;

: $getenv ( name -- str|false ) string>$ getenv dup if $>string else 2drop false then ;
\ Try to load given file in current or $HOME dir;
\ if file doesn't exist, do nothing.

: load-init-file { fname -- }
    try
	$" pwd" shell $" /" $+ fname $+ string>$ included
    recover
	drop try
	    $" HOME" $getenv $" /" $+ fname $+ string>$ included
	recover
	    drop
	endtry
    endtry
;

\ === File ===
1024 constant maxpathlength

so-lib-exists? libc-utils libc.so [if]
    libc-utils c-getcwd ptr int (ptr) getcwd \ char *getcwd(char *buf, size_t size);
    : file-pwd ( -- path )
	maxpathlength make-string { path }
	path data-ptr maxpathlength c-getcwd $c>string
	path free-string
    ;
    libc-utils c-chdir ptr (int) chdir \ int chdir(const char *path)
    : file-chdir ( path -- ) string>c$ c-chdir throw ;
[else]
    : file-pwd ( -- path ) $" pwd" shell ;
    : file-chdir ( path -- ) .obj ."  not changed" cr ;
[then]

: file-split { fname -- path file }
    fname length dup 1- 0 max { len idx }
    $" /" fname string-search if
	begin idx fname string@ [char] / <> while idx 1- to idx repeat
	idx make-string map i fname string@ end-map ( path )
	idx 1+ to idx
	len idx - make-string map idx i + fname string@ end-map ( file )
    else
	$" ./" ( path)
	fname ( file )
    then
;

: file-basename ( fname optional-extension -- basename )
    depth 2 >= if over ?string over ?string and unless nil then else nil then { fname ext }
    fname file-split swap free-string
    ext if
	{ base }
	base length ext length - { idx }
	true ext each idx i + base string@ <> if drop false leave then end-each
	if idx make-string map i base string@ end-map base free-string else base then
    then
;
: file-pathname ( fname -- pathname ) file-split free-string ;
: file-fullname { fname -- pathname }
    fname string-first [char] / = if
	fname
    else
	file-pwd $" /" $+ fname $nil file-basename $+
    then
;
: file-delete ( fname -- ) string>$ delete-file throw ;
\ file-status ( addr u -- wfam wior )
: ?file-exist ( fname -- f ) string>$ file-status nip 0= ;

defer str-help

: (str-help) ( -- )
    argv @ cell+ @ $c>string $nil file-basename { fn }
    ." Usage: " fn .string ."  [ options ]" cr
    ."	   -c, --csndlib	       use csndlib.fs (default)" cr
    ."	   -f, --fsndlib	       use fsndlib.fs" cr
    ."	   -d, --dac		       write to dac" cr
    cr
    ."	   -V, --version	       display version information and exit" cr
    ."	   -h, --help		       display this help message and exit" cr
;

' (str-help) is str-help

\ utils.fs ends here
