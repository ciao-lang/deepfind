:- module(_, [run_tests/0], [deepfind(deepfind)]).

:- use_module(library(lists), [length/2]).
:- use_module(library(messages)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(streams)).
%:- use_module(library(compiler/c_itf)).

%FAILS if we use the cache
%  why?
%  perhaps ugraphs is used? or is it confused with preds in the cache?
%  debug and the ndebug the new branch

run_tests :-
%    cleanup_itf_cache,
    note_message("searching in deepfind/examples"),
    bundle_path(deepfind, 'examples', ExDir),
    find_add_dir(ExDir),
    set_find_flag(reanalyze, on),
 %   findall(P, findp({ :- pred P(A,B) : list(A) => (list(A),list(B)). },P,_,_), LP1),
    findall((P,P1,P2), findp({ :- pred P(A,B) : list(A) => (list(A),list(B)). },P,P1,P2), LP1),
    length(LP1,N1),
    displayq((LP1,N1)), nl,
    ( N1 == 7 -> true
    ; error_message("deepfind test failed"),
      halt(1)
    ),
%    findall(P, findp({ :- pred P(A,_,B) : list(A) => (list(A),list(B)). },P,_,checked), LP2),
    findall((P,P1), findp({ :- pred P(A,_,B) : list(A) => (list(A),list(B)). },P,P1,checked), LP2),
    length(LP2,N2),
    displayq((LP2,N2)), nl,
    ( N2 == 5 -> true
    ; error_message("deepfind test failed"),
      halt(1)
    ).
