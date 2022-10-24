:- module(assertion_spec, [process_query/2], [doccomments, isomodes, datafacts]).

:- doc(title, "Normalizing queries").
:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "

Save set of assertions (specifications) in a query as temporary modules.

Asserts data of predicate name requirements made by @tt{:-
apropos(Predicate, Name).} directive.

Example:

@begin{verbatim}
    ?- use_package(deepfind(find_syntax)).
    ?- process_query({ :- pred P(X) => int(X). :- apropos(P, length). }, M).

    M = '/tmp/modROKkyL' ?
@end{verbatim}

The generated file is @tt{/tmp/modROKkyL.pl}:

@begin{verbatim}

:-module(_,['\006\pvar'/1],[assertions]).
:- pred '\006\pvar'(_A)
    => int(_A).

:-impl_defined('\006\pvar'/1).

@end{verbatim}

").

:- doc(bug, "We do not know the name of the variable in the query assertion").

% ---------------------------------------------------------------------------

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(assertions/assrt_lib), [normalize_assertion/9]).
:- use_module(engine(stream_basic)).
:- use_module(library(write), [portray_clause/2]).
:- use_module(library(dec10_io)).
:- use_module(library(assertions/assrt_write), [write_assertion/7]).
:- use_module(library(assertions/assrt_lib_extra), [assrt_replace_pd/4]).

:- use_module(library(modblobs)).

:- export(keyword_spec/1).
:- pred keyword_spec(Expression)
    #"@var{Expression} is a regexp or a word that the found predicate name 
      has to meet.".
:- data keyword_spec/1.

:- export(no_query_as/0).
:- data no_query_as/0.

query_mod(query).

search_assertion_name('\6\pvar'). % Default name for predicate

:- pred process_query(Curly, File) : (nonvar(Curly), var(File)) => atm(File)
    #"Saves a set of assertions (curly block) into a temporary @var{File}.".
process_query('\006\curly_block'(X), File) :- !,
    process_query_(X, File).

:- pred process_query_(Ass, ModName) : list * var => list * atm
    #"Saves assertions of ONE predicate to a temporary file (creating a module).".
process_query_(Clauses, ModPath) :-
    retractall_fact(keyword_spec(_)),
    set_fact(no_query_as),
    % create tmp file
    query_mod(ModName),
    new_modblob([(:- module(_, [], [assertions]))], [], ModName, ModBlob),
    modblob_path(ModBlob, ModPath),
    open(ModPath, append, S),
    process_clauses(S, Clauses),
    close(S),
    ( no_query_as, \+ keyword_spec(_) ->
        message(error, ['At least one query assertion or apropos directive must be specified.']),
        delete_modblob(ModBlob),
        fail
    ;
        true
    ).

:- pred process_clauses(A, B) : list * atm.
process_clauses(S, [Cl|Cls]) :-
    process_clause(S, Cl), !,
    process_clauses(S, Cls).
process_clauses(_, []).

process_clause(_, Cl) :-
    apropos_directive(Cl), !.
process_clause(S, Cl) :-
    search_assertion_name(Name),
    change_and_write_assertion(S, Cl, Name), !.
process_clause(S, Cl) :-
    portray_clause(S, Cl).

change_and_write_assertion(S, Sentence, Name):-
    Sentence = sentence(As, _Dict, _, _, _),
    change_assrt_name(As, Name, NewAs),
    normalize_assertion(test, NewAs, Head, Status, Type, Body, _D, _E, _F),
    ( no_query_as ->
        functor(Head, Name, Arity),
        portray_clause(S, (:- export(Name/Arity))),
        portray_clause(S, (:- impl_defined(Name/Arity))),
        retract_fact(no_query_as)
    ;
        true
    ),
    write_assertion(S, Head, Status, Type, Body, [], nostatus).

:- export(change_assrt_name/3).
change_assrt_name(As, Name, NewAs):-
    As = ':-'(Decl),
    assrt_replace_pd(Decl, NewPD, NewAs, OldPD),
    (
        (OldPD =.. [call,PVar|Args], var(PVar)) -> % form P(X)
         NewPD =.. [Name|Args]
    ;
        OldPD = PVar/Ar,                           % form P/1
        var(PVar),
        NewPD = Name/Ar
    ).

:- doc(section, "Specifications outside query assertions").

:- pred apropos_directive/1
    #"Checks that a clause is an apropos directive and processes it.".
apropos_directive((:- apropos(_P, Spec))) :-
    ( keyword_spec(_) ->
        message(error, ['Only one apropos directive is allowed'])
    ;
        assertz_fact(keyword_spec(Spec))
    ).
