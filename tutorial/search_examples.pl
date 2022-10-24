:- module(search_examples, [search_demo/5], [assertions]).

:- use_package(deepfind(find_syntax)).
:- use_package(regtypes).
:- use_package(hiord).

search_demo(
    strings, 1,
    { :- pred P/1 => string. },
    "Predicates that display strings",    
    "We use a success assertion instead of a calls assertion because the code
     could lack calls assertions and the condition would not be checked. If the
     query requires this condition on success, we avoid this problem").

search_demo(
    transform_list, 2,
    { :- pred P(A,B) : list(A) => (list(A), list(B)). },
    "Predicates that transform lists", _).

search_demo(
    insert_list, 3,
    { :- pred P(A,_,B) : list(A) => (list(A), list(B)). },
    "Predicates that insert elements in lists", _).

search_demo(
    op_list_num, 2,
    { :- pred P(A,B) : list(num, A) => num(B). },
    "Predicates that gather a list of numbers: sum, etc ...",
    "This search can be more general if the calls condition is moved 
     to the success conditions").

search_demo(sort, 2,
    { :- pred P(A,B) : list(A) => list(B).
      :- pred P(A,B) : list(B) => list(A). },
    "Sorting predicate", _).

search_demo(
    graph_structure, 1,
    {
      :- use_package(regtypes).

      :- regtype math_graph/1.

      :- pred P/2 => math_graph * term.

      math_graph(graph(Vertices,Edges)) :-
      list(Vertices),
      list(pair, Edges).

      :- regtype pair/1.
      pair((_,_)).
    },
    "Looking for graph structures",    
    "We look for success because there may not exist calls. We expect
     that a graph is a structure with a list of vertex (V) and a list
     of edges (E). We dont specify the shape of the edges or vertex
     because we do not know how they are represented in the code."
).

search_demo(
    op_graph, 3,
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
    "Looking for predicates for editing graphs",
    "Given the ugraph A, we want to add B (list of edges or vertex)
     and C will be the new constructed graph. We require therefore
     that C is a free variable.  This query does not mark the calls
     condition as checked because the code does not have any calls
     written. However, even the assertion were written, it would not
     be checked because it does not work with combined domains."
).

search_demo(
    op_graph2, 3,
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
    "Predicates for editing graphs",
    "This query is the same as in op_graph but with calls splited in
     domains so once calls assertions are written they can be checked"
).
