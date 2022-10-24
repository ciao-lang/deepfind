:- package(deepfind).

:- use_package(assertions).
:- set_prolog_flag(read_curly_blocks, on).
:- set_prolog_flag(read_postfix_blocks, on).
:- op(50, yf, ({})).

:- use_package(hiord). % for allowing variable in pred queries.

:- use_module(deepfind(find)).
