\ -*- snd-forth -*-
\ enved.fs -- enved object type

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sun Nov 13 13:59:42 CET 2005
\ Changed: Tue Oct 19 23:01:15 CEST 2010

\ Commentary:

\ This is an example of an Object type written in Forth.
\
\ ENVED
\  enved-inspect  	( obj -- str )
\  enved->string  	( obj -- str )
\  enved-dump  	        ( obj -- str )
\  enved->array         ( obj -- ary )
\  enved-copy           ( obj1 -- obj2 )
\  enved-ref            ( obj index -- point )
\  enved-set!           ( obj index point -- )
\  enved-equal?   	( obj1 obj2 -- f )
\  enved-length         ( obj -- len )
\  enved-free     	( obj -- )
\ 
\ enved?     	  	( obj -- f )
\ make-enved 	  	( lst -- enved )
\ enved-index           ( obj x -- index|-1 )
\ enved-insert!         ( obj index point -- )
\ enved-delete!         ( obj index -- )

\ === ENVED OBJECT TYPE ===

hide
\ The name enved-envelope is in use!
struct
  cell% field enved-fs-envelope
end-struct enved%
set-current

: envelope@ ( obj -- lst ) instance-gen-ref enved-fs-envelope @ ;
: envelope! ( lst obj -- ) instance-gen-ref enved-fs-envelope ! ;

"enved" make-object-type constant fth-enved
fth-enved make-?obj enved?

: make-enved ( envelope -- enved )
  { envelope }
  envelope array? envelope 1 $" an array" assert-type
  enved% %alloc { enved }
  enved unless 'system-error #( get-func-name $" cannot create enved" ) fth-throw then
  envelope enved enved-fs-envelope !
  enved fth-enved make-instance
;  
previous

: enved-length ( obj -- len ) envelope@ array-length 2/ ;

: enved-inspect { obj -- str }
  $" #<%s[%d]: %S>" #( obj object-name obj enved-length obj envelope@ ) string-format
;

: enved->string ( obj -- str )  envelope@ object->string ;
: enved-dump    ( obj -- str )  enved->string $"  make-enved" $+ ;
: enved->array  ( obj -- ary )  envelope@ array->array ;
: enved-copy    ( obj -- obj2 ) envelope@ array-copy make-enved ;

: enved-ref { obj index -- point }
  obj enved? obj 1 $" an enved object" assert-type
  index 0< if index obj enved-length + to index then
  obj index object-range? if
    index 2* to index
    #( obj envelope@ index array-ref obj envelope@ index 1+ array-ref )
  else
    'out-of-range
    #( get-func-name $" index %s, enved length %s" #( index obj enved-length ) )
    fth-throw
  then
;

: enved-set! { obj index point -- }
  obj enved? obj 1 $" an enved object" assert-type
  index 0< if index obj enved-length + to index then
  obj index object-range? if
    index 2* to index
    obj envelope@ index    point 0 array-ref array-set!
    obj envelope@ index 1+ point 1 array-ref array-set!
  else
    'out-of-range
    #( get-func-name $" index %s, enved length %s" #( index obj enved-length ) )
    fth-throw
  then
;

: enved-equal? { obj1 obj2 -- f }
  obj1 enved? obj2 enved? && if
    obj1 envelope@ obj2 envelope@ object-equal?
  else
    #f
  then
;

\ frees %alloc-ed struct enved%
: enved-free ( obj -- ) instance-gen-ref free throw ;

\ Init enved
<'> enved-inspect  fth-enved set-object-inspect	\ en .inspect
<'> enved->string  fth-enved set-object->string	\ en object->string
<'> enved-dump     fth-enved set-object-dump	\ en object-dump
<'> enved->array   fth-enved set-object->array	\ en object->array
<'> enved-copy     fth-enved set-object-copy	\ en object-copy
<'> enved-ref      fth-enved set-object-value-ref \ en index          object-ref => #( x y )
<'> enved-set!     fth-enved set-object-value-set \ en index #( x y ) object-set!
<'> enved-equal?   fth-enved set-object-equal-p	\ obj1 obj2 equal?
<'> enved-length   fth-enved set-object-length  \ en object-length => number of points (lstlen/2)
<'> enved-free     fth-enved set-object-free	\ for gc
<'> enved-ref      fth-enved 1 set-object-apply	\ en index apply => #( x y )

\ ENVED-INDEX, ENVED-INSERT!, ENVED-DELETE!
: enved-index { obj x -- index|-1 }
  obj enved? obj 1 $" an enved object" assert-type
  -1 obj each 0 array-ref x f= if drop i leave then end-each
;

: enved-insert! { obj index point -- }
  obj enved? obj 1 $" an enved object" assert-type
  point array? point object-length 2 = &&  point 3 $" a point array #( x y )" assert-type
  obj enved-length 0= if
    point
  else
    index 0< if index obj enved-length + to index then
    index 0>= index obj enved-length <= || if
      index 2* to index
      obj envelope@ index point array-insert
    else
      'out-of-range
      #( get-func-name $" index %s, enved length %s" #( index obj enved-length ) )
      fth-throw
    then
  then obj envelope!
;

: enved-delete! { obj index -- }
  obj enved? obj 1 $" an enved object" assert-type
  index 0< if index obj enved-length + to index then
  obj index object-range? if
    index 2* to index
    obj envelope@ index array-delete! drop
    obj envelope@ index array-delete! drop
  else
    'out-of-range
    #( get-func-name $" index %s, enved length %s" #( index obj enved-length ) )
    fth-throw
  then
;

\ enved.fs ends here
