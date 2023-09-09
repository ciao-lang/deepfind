:- module(_, [run_tests/0], [deepfind(deepfind)]).

:- use_module(library(lists), [length/2]).
:- use_module(library(messages)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
%:- use_module(library(compiler/c_itf), [cleanup_itf_cache/0]).

run_tests :-
    % cleanup_itf_cache, % (use to reset location of modules)
    note_message("searching in deepfind/examples"),
    bundle_path(deepfind, 'examples', ExDir),
    find_add_dir(ExDir),
    set_find_flag(reanalyze, on),
    findall(P, findp({ :- pred P(A,B) : list(A) => (list(A),list(B)). },P,_,_), LP1),
    length(LP1,N1),
    ( N1 == 7 -> true
    ; error_message("deepfind test failed"),
      halt(1)
    ),
    findall(P, findp({ :- pred P(A,_,B) : list(A) => (list(A),list(B)). },P,_,checked), LP2),
    length(LP2,N2),
    ( N2 == 5 -> true
    ; error_message("deepfind test failed"),
      halt(1)
    ).
