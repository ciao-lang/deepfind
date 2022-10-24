% assrt_db(# args, # conds, # test assrts).

assrt_db(1, 1, { :- pred p/1 : term. } ).
assrt_db(1, 2, { :- pred p/1 : term => int. }).
assrt_db(1, 3, { :- pred p/1 : term => int. :- pred p/1 => list. }).
assrt_db(1, 4, { :- pred p/1 : term => int. :- pred p/1 : stream => stream. :- pred p/1 : atom => atom. }).
assrt_db(1, 5, { :- pred p/1 : term => int. :- pred p/1 => list. :- pred p/1 : atom => atom. :- pred p/1 : stream => stream. }).

assrt_db(2, 1, { :- pred p/2 : list * term. } ).
assrt_db(2, 2, {
    :- pred p/2 : list * num. 
    :- pred p/2 : term * num => list * num. } ).
assrt_db(2, 3, {
    :- pred p/2 : term * list. 
    :- pred p/2 : term * term => list * num.
    :- pred p/2 : stream * term => stream * list. } ).
assrt_db(2, 4, {
    :- pred p/2 : term * list. 
    :- pred p/2 : list * var => list * num.
    :- pred p/2 : var * atom => list * atom.
    :- pred p/2 : stream * term => stream * list. } ).

assrt_db(3, 1, { :- pred p/3 : list * list * term. } ).
assrt_db(3, 2, {
    :- pred p/3 : list * list * term. 
    :- pred p/3 : list * list * var => list * list * list. } ).
assrt_db(3, 3, {
    :- pred p/3 : list * list * term. 
    :- pred p/3 : list * list * var => list * list * list.  
    :- pred p/3 : atom * atom * var => atom * atom * stream. } ).
assrt_db(3, 4, {
    :- pred p/3 : list * list * term. 
    :- pred p/3 : list * list * var => list * list * list.  
    :- pred p/3 : atom * atom * var => atom * atom * stream.
    :- pred p/3 : var * var * term => list * list * int. } ).


assrt_db(4, 1, { :- pred p/4 : list * list * var * var. } ).
assrt_db(4, 2, {
    :- pred p/4 : list * list * var * var.
    :- pred p/4 : list * list * var * var => list * list * int * atom. } ).
assrt_db(4, 3, {
    :- pred p/4 : list * list * var * var.
    :- pred p/4 : list * list * var * var => list * list * int * atom.
    :- pred p/4 : atom * atom * var * var => atom * atom * stream * list. } ).

assrt_db(4, 4, {
    :- pred p/4 : list * list * var * var.
    :- pred p/4 : list * list * var * var => list * list * int * atom.
    :- pred p/4 : atom * atom * var * var => atom * atom * stream * list.
    :- pred p/4 : list * int * var * var => list * int * stream * list. } ).
