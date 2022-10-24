:- module(find_test, [main_test/0],[assertions, hiord, datafacts]).

:- doc(title, "Benchmarking DeepFind").
:- doc(author, "Isabel Garcia-Contreras").
:- doc(module, "Module for testing time execution of queries with DeepFind").

% ---------------------------------------------------------------------------

:- use_package(deepfind(find_syntax)).

:- use_module(deepfind(find)).

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(format)).

:- include(test_db).

% ---------------------------------------

:- data status_counter/2.
:- data assrt_db/3.

% --------------------------------------

inc_status_counter(Status) :-
    \+  status_counter(Status, _), !,
    assertz_fact(status_counter(Status, 1)).
inc_status_counter(Status) :-
    status_counter(Status, Count), !,
    NewCount is Count + 1,
    retract_fact(status_counter(Status, _)),
    assertz_fact(status_counter(Status, NewCount)).
process_status_pred(_P, Status) :-
%       format(' ~w ~w~n', [P, Status]),
    inc_status_counter(Status).

clear_status_counter :-
    retractall_fact(status_counter(_,_)).

:- doc(doinclude, summarize_search/1).
:- pred summarize_search(As) : nonvar
    #"Collects run-time data of the execution of the 
      query given the assertion query @var{As}".
summarize_search(As) :-
    clear_status_counter,
    set_find_flag(reanalyze, off),
    ( foreach(findp(As, P, _, Status),
              process_status_pred(P, Status)) ->
        true
    ; throw(bug('findp/4 failed'))
    ),
    display_search_stats,
    display_counter_info,
    dump_size(Size),
    format('Total dump size ~w B~n', [Size]),
    restore_time(RTime),
    format('Time spent restoring ~w msec.~n', [RTime]),
    check_time(CTime),
    format('Time spent checking ~w msec.~n', [CTime]).

:- meta_predicate foreach(goal, goal).
% Execute Do for each solution of Gen
% Fail if any call to Do fails.
foreach(Gen, Do) :-
    \+ (Gen, \+ Do).

counter(false).
counter(check).
counter(checked).

display_counter_info :-
    format('Matches:', []),
    counter(S),
    ( status_counter(S, C) -> true ; C = 0 ),
    format(' ~w=~w', [S, C]),
    fail.
display_counter_info :-
    nl.

ini_status_counter :-
    counter(X),
    asserta_fact(status_counter(X, 0)),
    fail.
ini_status_counter.

tex_file('checking_stats.tex').

:- pred main_test/0
    #"Executes a set of queries defined in test_db.pl and generates 
     a table in tex format with total and avg times of execution".
main_test :-
    % Select the context of the search
    find_add_bundle(core),
    find_add_bundle(contrib), % TODO: use other available bundles?
    % Do search, generate and print statistics
    ini_status_counter,
    tex_file(F),
    open(F, write, S),
    print_test_header(S),
    print_tab_header(S),
    generate_table_info(S, 4,4),
    print_tab_foot(S),
    close(S).

generate_table_info(S, MaxAssrts, MaxArgs) :-
    generate_table_info_(S, 1, MaxAssrts, MaxArgs).

generate_table_info_(_S, CurrArgs, _MaxAssrts, MaxArgs) :-
    CurrArgs > MaxArgs, !.

generate_table_info_(S, CurrArgs, MaxAssrts, MaxArgs) :-
    generate_row_info(S, CurrArgs, MaxAssrts),
    NewCurrArgs is CurrArgs + 1,
    generate_table_info_(S, NewCurrArgs, MaxAssrts, MaxArgs).

generate_row_info(S, Args, MaxAssrts) :-
    generate_row_info_(Args, 1, MaxAssrts, Info),
    print_row_info(S, Args, Info).

generate_row_info_(_Args, CurrAssrt, MaxAssrts, []) :-
    CurrAssrt > MaxAssrts, !.

generate_row_info_(Args, CurrAssrt, MaxAssrts, RowInfo) :-
    generate_cell_info(Args, CurrAssrt, [T, AVG]),
    RowInfo = [T, AVG | RestRowInfo],
    NewCurrAssrt is CurrAssrt + 1,
    generate_row_info_(Args, NewCurrAssrt, MaxAssrts, RestRowInfo).

generate_cell_info(Args, CurrAssrt, Info) :-
    assrt_db(Args, CurrAssrt, As),
    summarize_search(As),
    check_time(CTime),
    total_predicates(T),
    AVG is CTime / T,
    Info = [CTime, AVG].

total_predicates(T) :-
    ( status_counter(check, N1) ->
        true
    ;
        N1 = 0
    ),
    ( status_counter(checked, N2) ->
        true
    ;
        N2 = 0
    ),
    Sum is N1 + N2,
    ( status_counter(false, N3) ->
        true
    ;
        N3 = 0
    ),
    T is Sum + N3.

print_test_header(S) :-
    format(S, '\\begin{longtable}{||r|r|r|r|r|r|r|r|r||}~n', []),
    format(S, '\\caption{Assertion Checking times ($\mu$s).\\label{tab:ass_check_time}}~n', []),
    format(S, '\\\\ \\hline\\hline~n'),
    format(S, '\\textbf{Ar\\textbackslash Cnds}  & \\textbf{1} & \\textbf{1 (AVG)}& \\textbf{2}& \\textbf{2 (AVG)} & \\textbf{3}& \\textbf{3 (AVG)} & \\textbf{4} & \\textbf{4 (AVG)} \\\\ \\hline\\hline~n', []).

print_tab_header(S) :-
    format(S, '# Args $\\setminus$ & \multicolumn{2}{l}{1} & \multicolumn{2}{l}{2} & \multicolumn{2}{l}{3} & \multicolumn{2}{l}{4} &  \multicolumn{2}{l}{AVG} \\\\ \\hline \\hline~n', []).

print_row_info(S, NArgs, Info) :-
    format(S, '~w & ~w & ~w & ~w & ~w & ~w & ~w & ~w & ~w\\\\ \\hline~n', [NArgs | Info ]).

print_tab_foot(S) :-
    format(S, '\\end{longtable}~n\\end{small}~n', []).






/*  Running examples

    summarize_search({ :- pred p/1 => list. }).

This is useful for finding properties, for example, it finds
    qsort:sorted_num_list/1
 dict_types:varnamesl/1
 assertions_props:dictionary/1
 ttyout:ttydisplay_string/1
 sort:keylist/1
 miscprops:formatstring/1
 stream_utils:write_string/1

 This search discards a lot of unuseful predicates (39).
 
Matches: false=39 check=39 checked=7
Total dump size 3514256 B
Time spent restoring 26228.0 msec.
Time spent checking 64.0 msec.

Trying to find  qsort:qsort/2

    its assertion is:
    :- calls qsort(A, B) : list(num, A).
so it cannot be found with

    summarize_search({ :- pred p/2 : list * var  => list * list. }).
(no predicates are checked)

    but with this, the only predicate checked is qsort :)
summarize_search({ :- pred p/2 : list * term  => list * list. }).
Matches: false=35 check=38 checked=1
Total dump size 3514256 B
Time spent restoring 26104.0 msec.
Time spent checking 132.0 msec.

---------------------------------------------------------------


    */
