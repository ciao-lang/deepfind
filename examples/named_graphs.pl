:- module(named_graphs,
    [complete_graph/2,
     al_complete_graph/2,
     cycle_graph/2,
     al_cycle_graph/2],
    [assertions, regtypes]).

:- use_module(library(lists), [append/3]).

%%%%%% COMPLETE GRAPH %%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred complete_graph(N, G) : int(N)  #"A graph of @var{N} vertices".
complete_graph(N, graph(V,E)) :-
    count(N, V),
    generate_complete_edges(V, E).

generate_complete_edges(V, E) :-
    generate_complete_edges_(V, V, E).

generate_complete_edges_([], _, []).
generate_complete_edges_([V|Vs], AllV, E) :- 
    generate_complete_edges_for_vertex(V, AllV, E1),
    append(E1, RestE, E),
    generate_complete_edges_(Vs, AllV, RestE).

:- pred generate_complete_edges/2 #"Generate all posible combinations
    of 2 elements of a list".
generate_complete_edges_for_vertex(_, [], []) :- !.
generate_complete_edges_for_vertex(V, [V|Vs], E) :- !,
    generate_complete_edges_for_vertex(V, Vs, E).
generate_complete_edges_for_vertex(V, [V1|Vs], [(V, V1)|E]) :-  
    generate_complete_edges_for_vertex(V, Vs, E).
    

%% adjacency list
:- pred al_complete_graph(N, G) : int(N) #"A graph of @var{N} vertices".
al_complete_graph(N, GraphV) :-
    count(N, V),
    generate_al_complete_vertices(V, V, GraphV).

generate_al_complete_vertices([], _, []).
generate_al_complete_vertices([V|Vs], AllV, [ALV|Vertices]) :-
    remove_elem(AllV, V, AL),
    ALV = (V-AL),
    generate_al_complete_vertices(Vs, AllV, Vertices).

% remove elem from list (that elem is unique in the list).
remove_elem([Ele|Lst], Ele1, Lst) :- Ele = Ele1, !.
remove_elem([Elem|L1], Ele, [Elem|L2]):- remove_elem(L1, Ele, L2).


%%%%%%%%%%%%%%% CYCLE GRAPH %%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred cycle_graph(N, G) : int(N).
cycle_graph(N, graph(V,E)) :-
    N = 2, !,
    V = [1,2],
    E = [(1,2),(2,1)].
cycle_graph(N, graph(V,E)) :-
    N > 1,
    count(N, V),
    generate_cycle_edges(V, E).

generate_cycle_edges([V1], [(V1, 1)]) :- !.
generate_cycle_edges([V1, V2|Vs], [(V1, V2)|Edges]) :-
    generate_cycle_edges([V2|Vs], Edges).

%% adjacency list
:- pred al_cycle_graph(N, G) : int(N).
al_cycle_graph(2, [1-[2],2-[1]]) :- !.
%       G = [1-[2],2-[1]].
al_cycle_graph(N, Vertices) :-
    count(N, V),
    append(V, [1], V1),
    generate_al_vertices([N|V1], Vertices).

generate_al_vertices([_,_], []).
generate_al_vertices([VPrev, V, VNext|Vs], [V-[VPrev, VNext]|Vertices]) :-
%       CurrV =  V-[VPrev, VNext],
    generate_al_vertices([V, VNext|Vs], Vertices).

count(N, Lst) :-
    count_(1, N, Lst).

count_(I, N, []) :-
    I > N, !.
count_(I, N, [I|L]) :-
    I1 is I+1,
    count_(I1, N, L).

:- trust pred append(A, B, C) : list(pair, A) => list(pair, C).
:- trust pred append(A, B, C) : list(al_graph_elem, A) => list(al_graph_elem, C).
:- regtype al_graph_elem/1.
al_graph_elem(_Vertex-Neighbors) :-
    list(Neighbors).

:- regtype pair/1.
pair((_,_)).
