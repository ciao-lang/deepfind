:- module(search_examples, [search_demo/8], [assertions]).

:- use_module(engine(stream_basic)).
:- use_package(deepfind(find_syntax)).
:- use_package(regtypes).
:- use_package(hiord).

:- pred search_demo(ID, Arity, Loc, Reanalyze, Terms, String, Description, Explanation) #"Searching examples for findp/4 or graphic interface".

search_demo(
    strings, 1, lib, off,
    { :- pred P/1 => string. },
      ":- pred P/1 => string.",
    "Predicates that display strings",    
    "We use a success assertion instead of a calls assertion
     because code could lack of assertions and the condition would
     not be checked. If the query requires this condition on exit,
     we avoid this problem").

search_demo(
    transform_list, 2, lib, off,
    { :- pred P(A,B) : list(A) => (list(A), list(B)). },
    ":- pred P(A,B) : list(A) => (list(A), list(B)).",
    "Predicates that transform lists", _).

search_demo(
    regex_write, _, lib, off,
    { :- apropos(P, '.*write.*'). },
    ":- apropos(P, '.*write.*').",
    "Predicates that contain the word 'write'", _).
      
search_demo(
    write_stream, 2, lib, off,
    { :- pred P(X,Y) : stream(X). :- apropos(P, '.*write.*'). },
":- pred P(X,Y): stream(X).
:- apropos(P, '.*write.*').",
      "Predicates to write to stream", _).

search_demo(
    insert_list, 3, lib, off,
    { :- pred P(A,_,B) : list(A) => (list(A), list(B)). },
    ":- pred P(A,_,B) : list(A) => (list(A), list(B)).",
    "Predicates that insert elements in lists", _).

search_demo(
    op_list_num, 2, lib, off,
    { :- pred P(A,B) : list(num, A) => num(B). },
     ":- pred P(A,B) : list(num, A) => num(B).",
    "Predicates that gather a list of numbers: sum, etc ...",
    "This search can be more general if the calls condition is moved 
     to the success conditions").

search_demo(sort, 2, lib, off,
    { :- pred P(A,B) : list(A) => list(B).
      :- pred P(A,B) : list(B) => list(A). },
":- pred P(A,B) : list(A) => list(B).
:- pred P(A,B) : list(B) => list(A).",
    "Sorting predicate", _).

search_demo(
    graph_structure, 1, examples, on,
    {
      :- use_package(regtypes).

      :- regtype math_graph/1.

      :- pred P/2 => term * math_graph.

      math_graph(graph(Vertices,Edges)) :-
      list(Vertices),
      list(pair, Edges).

      :- regtype pair/1.
      pair((_,_)).
},
":- use_package(regtypes).
:- pred P/2 => term * math_graph.
:- regtype math_graph/1.
math_graph(graph(Vertices,Edges)) :-
    list(Vertices),
    list(pair, Edges).
:- regtype pair/1.
pair((_,_)).",
    "Looking for graph structures",    
    "We look for success because there may not exist calls. We expect
     that a graph is a structure with a list of vertex (V) and a list
     of edges (E). We dont specify the shape of the edges or vertex
     because we do not know how they are represented in the code."
).

search_demo(
    op_graph, 3, examples, on,
    {
      :- use_package(regtypes).

      :- pred P(A,B,C) : (my_ugraph(A),list(B),var(C))
                     => my_ugraph(C).

      :- regtype my_graph_elem/1.
      my_graph_elem(_Vertex-_Neighbors) :- list(_Neighbors).

      :- regtype my_ugraph(_).
      my_ugraph(A) :-
      list(my_graph_elem,A).
},
":- use_package(regtypes).
:- pred P(A,B,C) : (my_ugraph(A),list(B),var(C))
    => my_ugraph(C).
:- regtype my_graph_elem/1.
my_graph_elem(_Vertex-_Neighbors) :- list(_Neighbors).
:- regtype my_ugraph(_).
my_ugraph(A) :-
    list(my_graph_elem,A).",
    "Looking for predicates for editing graphs",
    "Given the ugraph A, we want to add B (list of edges or vertex)
     and C will be the new constructed graph. We require therefore
     that C is a free variable.  This query does not mark the calls
     condition as checked because the code does not have any calls
     written. However, even the assertion were written, it would not
     be checked because it does not work with combined domains."
).

search_demo(
    op_graph2, 3, examples, on,
    {
      :- use_package(regtypes).

      :- pred P(A,B,C) :
    ((nonvar(A), nonvar(B), var(C)) ; (my_ugraph(A), list(B)))
    => my_ugraph(C).

      :- regtype my_graph_elem/1.
      my_graph_elem(Vertex-Neighbors) :- list(Neighbors).

      :- regtype my_ugraph(_).
      my_ugraph(A) :- list(my_graph_elem,A).
},
":- use_package(regtypes).
:- pred P(A,B,C) :
    ((nonvar(A), nonvar(B), var(C)) ; (my_ugraph(A), list(B)))
    => my_ugraph(C).
:- regtype my_graph_elem/1.
my_graph_elem(Vertex-Neighbors) :- list(Neighbors).
:- regtype my_ugraph(_).
my_ugraph(A) :- list(my_graph_elem,A).",
    "Predicates for editing graphs",
    "This query is the same as in op_graph but with calls splited in
     domains so once calls assertions are written they can be checked"
).
