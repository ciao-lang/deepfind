:- module(assrt_to_string, [], [assertions, argnames, isomodes, regtypes]).

:- doc(title, "Write assertions to strings").
:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "This module implements predicates to pretty print
   assertions as strings.").

:- doc(bug, "This should be implemented without writing on a file").

:- use_package(ciaopp(p_unit/p_unit_argnames)).

:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(write), [portray_clause/2]).
:- use_module(ciaopp(p_unit), [assertion_set_status/3, assertion_set_head/3]).
:- use_module(ciaopp(p_unit/p_printer), [print_assrt/2]).

:- export(write_assertion_to_string/2).
:- pred write_assertion_to_string(A, AString) => string(AString)
    #" Writes @var{A} to @var{AString}.  *Warning* The status of
    the assertion is not shown by changing it to ' ' and the name
    of the head is changed to ''.".
write_assertion_to_string(A, AString) :-
    A = as${ head => Head },
    Head =.. [_|Args],
    NewHead =.. [''|Args],
    assertion_set_status(A, '', A1),
    assertion_set_head(A1, NewHead, NewA),
    %
    mktemp_in_tmp('asXXXXXX', File),
    open(File, write, S),
    print_assrt(S, NewA),
    close(S),
    file_to_string(File, AString),
    del_file_nofail(File).

:- export(write_clauses_to_string/2).
write_clauses_to_string(Clauses, String) :-
    mktemp_in_tmp('clausesXXXXXX', FileBase),
    open(FileBase, write, S),
    write_clauses_to_string_(S, Clauses),
    close(S),
    file_to_string(FileBase, String),
    del_file_nofail(FileBase).

write_clauses_to_string_(_, []) :- !.
write_clauses_to_string_(S, [C|Cs]) :-
    portray_clause(S, C),
    write_clauses_to_string_(S, Cs).
