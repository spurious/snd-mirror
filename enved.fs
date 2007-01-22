\ -*- snd-forth -*-
\ enved.fs -- enved object type

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sun Nov 13 13:59:42 CET 2005
\ Changed: Sat Jan 20 01:08:01 CET 2007

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
\  enved-mark     	( obj -- )
\  enved-free     	( obj -- )
\ 
\ enved?     	  	( obj -- f )
\ make-enved 	  	( lst -- enved )
\ enved-index           ( obj x -- index|-1 )
\ enved-insert!         ( obj index point -- )
\ enved-delete!         ( obj index -- )

\ Code:

\ === ENVED OBJECT TYPE ===
hide
\ The name enved-envelope is in use!
struct
  cell% field enved-fs-envelope
end-struct enved%
set-current

: envelope@ ( obj -- lst ) instance-gen-ref enved-fs-envelope @ ;
: envelope! ( lst obj -- ) instance-gen-ref enved-fs-envelope ! ;

$" enved" make-object-type constant fth-enved
fth-enved make-?obj enved?

: make-enved ( envelope -- enved )
  { envelope }
  envelope list? envelope 1 $" a list" _ assert-type
  enved% %alloc { enved }
  enved unless 'system-error '( get-func-name $" cannot create enved" _ ) fth-throw then
  envelope enved enved-fs-envelope !
  enved fth-enved make-instance
;  
previous

: enved-length ( obj -- len ) envelope@ object-length 2/ ;
: enved-inspect ( obj -- str )
  { obj }
  $" #<%s[%d]: %s>" '( obj object-name obj enved-length obj envelope@ ) string-format
;
: enved->string ( obj -- str ) envelope@ object->string ;
: enved-dump ( obj -- str ) $" %s make-enved"  swap envelope@ object-dump string-format ;
: enved->array ( obj -- ary ) envelope@ list->array ;
: enved-copy ( obj1 -- obj2 ) envelope@ list-copy make-enved ;
: enved-ref ( obj index -- point )
  { obj index }
  obj enved? obj 1 $" an enved object" _ assert-type
  index 0< if index obj enved-length + to index then
  obj index range? if
    index 2* to index
    '( obj envelope@ index list-ref obj envelope@ index 1+ list-ref )
  else
    'out-of-range
    '( get-func-name $" index %s, enved length %s" _ '( index obj enved-length ) )
    fth-throw
  then
;
: enved-set! ( obj index point -- )
  { obj index point }
  obj enved? obj 1 $" an enved object" _ assert-type
  index 0< if index obj enved-length + to index then
  obj index range? if
    index 2* to index
    obj envelope@ index    point car  list-set!
    obj envelope@ index 1+ point cadr list-set!
  else
    'out-of-range
    '( get-func-name $" index %s, enved length %s" _ '( index obj enved-length ) )
    fth-throw
  then
;
: enved-equal? ( obj1 obj2 -- f )
  { obj1 obj2 }
  obj1 enved? obj2 enved? && if
    obj1 envelope@ obj2 envelope@ equal?
  else
    #f
  then
;
: enved-mark ( obj -- ) envelope@ object-mark ;
: enved-free ( obj -- ) instance-gen-ref free throw ;

\ Init enved
' enved-inspect  fth-enved set-object-inspect 	\ enved .inspect
' enved->string  fth-enved set-object->string 	\ enved object->string
' enved-dump     fth-enved set-object-dump    	\ enved object-dump
' enved->array   fth-enved set-object->array  	\ enved object->array
' enved-copy     fth-enved set-object-copy    	\ enved object-copy
' enved-ref      fth-enved set-object-value-ref	\ enved index        object-ref => '( x y )
' enved-set!     fth-enved set-object-value-set \ enved index '( x y ) object-set!
' enved-equal?   fth-enved set-object-equal-p 	\ obj1 obj2 equal?
' enved-length   fth-enved set-object-length  	\ enved object-length => number of points (lstlen/2)
' enved-mark     fth-enved set-object-mark    	\ enved object-mark and for gc
' enved-free     fth-enved set-object-free    	\ for gc
' enved-ref      fth-enved 1 set-object-apply 	\ enved index apply => '( x y )

\ ENVED-INDEX, ENVED-INSERT!, ENVED-DELETE!
: enved-index ( obj x -- index|-1 )
  { obj x }
  obj enved? obj 1 $" an enved object" _ assert-type
  -1 obj each car x f= if drop i leave then end-each
;
: enved-insert! ( obj index point -- )
  { obj index point }
  obj enved? obj 1 $" an enved object" _ assert-type
  point list? point object-length 2 = &&  point 3 $" a point list '(x y)" _ assert-type
  index 0< if index obj enved-length + to index then
  obj index range? if
    index 2* to index
    obj envelope@ index point list-insert obj envelope!
  else
    'out-of-range
    '( get-func-name $" index %s, enved length %s" _ '( index obj enved-length ) )
    fth-throw
  then
;
: enved-delete! ( obj index -- )
  { obj index }
  obj enved? obj 1 $" an enved object" _ assert-type
  index 0< if index obj enved-length + to index then
  obj index range? if
    index 2* to index
    obj envelope@ index 2 list-slice obj envelope!
  else
    'out-of-range
    '( get-func-name $" index %s, enved length %s" _ '( index obj enved-length ) )
    fth-throw
  then
;

\ enved.fs ends here
