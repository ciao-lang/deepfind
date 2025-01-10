:- module(find,
    [findp/4,
     find_add_bundle/1,
     find_rm_bundle/1,
     find_add_dir/1,
     find_rm_dir/1,
     find_add_mod/1,
     find_rm_mod/1,
     analyze_search_mods/1,
     dump_size/1,
     restore_time/1,
     check_time/1,
     clean_search_opts/0,
     display_search_stats/0],
    [assertions, argnames, modes, regexp, hiord, regtypes, datafacts]).

:- doc(title, "Finding code").
:- doc(author,"Isabel Garcia-Contreras").

:- doc(stability, devel).

:- doc(module, "
This module contains the main functionality of @apl{DeepFind}.

@pred{findp/4} is the predicate used for finding code in some
determined modules. This modules can be specified with
@pred{find_add_mod/1}, @pred{find_add_dir/1}, and @pred{find_add_bundle/1}.

@bf{Phases of} @pred{findp/4}:

@begin{itemize}
@item @bf{Process query}. The information in the query is extracted:
@begin{itemize}
@item Query assertions.
@item Keyword requirements (specified via @tt{:- apropos(String).} directives).
@item User-defined regular types.
@end{itemize}
@item @bf{Check conditions against code}, for each module:
@begin{itemize}
@item Get predicates with the specified arity.
@item Filter predicates with keyword requirements.
@item Restore analysis/ make analysis.
@item Check conditions for the filtered predicates.
@end{itemize}

@end{itemize}

During this whole process, time and memory statistics are collected,
and can be accessed with @tt{dump_size/1}, @tt{restore_time/1} and
@tt{check_time/1}.

 ").

% ---------------------------------------------------------------------------

:- use_package(library(compiler/p_unit/p_unit_argnames)).

:- use_module(engine(runtime_control), [module_split/3]).
:- use_module(engine(internals), [module_concat/3]).
:- use_module(engine(stream_basic)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(pathnames), [path_basename/2, path_split/3, path_concat/3, path_splitext/3]).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(library(system), [file_property/2, file_exists/1]).
:- use_module(library(fuzzy_search), [damerau_lev_dist/3]).
:- use_module(library(source_tree/bundle_source_tree), [bundle_contents/3]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(assertions/assertions_props), [assrt_status/1]).
% display
:- use_module(library(format), [format/2]).
:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic), [message/2]).

:- use_module(library(compiler/p_unit), [
    get_assertion/2,
    assertion_set_head/3,
    assertion_set_calls/3,
    assertion_set_success/3]).
:- use_module(library(compiler/p_unit/p_unit_db), [current_itf/3]).
:- use_module(ciaopp(p_dump), [restore/1]).

:- use_module(ciaopp(frontend_driver), [module/1]).
:- use_module(ciaopp(analyze_driver), [analyze/1, clean_analysis_info/0]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).

:- use_module(ciaopp_batch(ciaopp_batch_aux), [dump_file/4, get_mod_name_from_file/2]).
:- use_module(ciaopp_batch(ciaopp_batch), [analysis_start/2]).

:- use_module(library(modblobs), [delete_modblob/1]).
:- use_module(deepfind(get_exports)).
:- use_module(deepfind(assertion_spec), [process_query/2, keyword_spec/1, no_query_as/0]).

% ---------------------------------------------------------------------------
% Analysis database
:- use_module(ciaopp_batch(db_analysis),
    [update_tasks_analysis_status/0, update_task_status/0, task_status/5]).

% ---------------------------------------------------------------------------
:- use_module(ciaopp(ctchecks/ctchecks_common), [abs_exec_one_assertion/6]).
:- use_module(ciaopp(ctchecks/ctchecks_pred), [decide_get_info/4]).
:- use_module(library(compiler/p_unit/program_keys), [predkey_from_sg/2]).

:- include(ciaopp_batch(analyze_opts)).

% ------------------ SEARCH PARAMETERS ------------------------------- %

:- data preds/1.
:- data pred_class/1.

:- data aux_module/1. % Stores the location of the search module
:- data search_assertion/1.
:- data search_arity/1.

:- export(find_loc/2).
:- data find_loc/2.
:- pred find_loc(LocType, LocPath) #"Data structure for saving
    user-defined search locations".

:- data search_module/2.
:- pred search_module(ModPath, ModName) #"Data structure for
    saving modules to search in.".

% -------------------------------------------------------------------------
:- doc(section, "Flags").

:- data config_flag_/2.
:- pred config_flag(Flag, Value) : (config_opt(Flag), atm(Value))
    #"
@tt{allow_not_preanalyzed}

 - @tt{on}: analyzes modules although they might have not been analyzed before
 - @tt{off}: if a module wasn't marked as safe it is not analyzed
 This flag is useful if reanalysis is on, if not, preanalysis is
 needed for loading dumps.

@tt{reanalyze}:

 - @tt{on}: modules are analyzed before searching.
SLOW but user defined types allowed.
- @tt{off}: information from dumps is loaded search in the module.
QUICK

@tt{mult_dump_load}:
 - @tt{on}: load all dumps at the same time (saves time when module location do not vary.
 - @tt{off}: load dump information for each module independently for each query.
".
config_flag(A,B) :- config_flag_(A,B).

:- export(config_opt/1).
:- regtype config_opt/1.
config_opt(X) :-
    deepfind_flag(X, _).

deepfind_flag(reanalyze, [on,off]).
deepfind_flag(allow_not_preanalyzed, [on,off]).
deepfind_flag(mult_dump_load, [on,off]).

:- export(set_find_flag/2).
:- pred set_find_flag(Flag, Status) : atm * atm
    #"sets a search option: allow_not_preanalyzed or reanalyze. 
      @var{Status} can be on/off".
set_find_flag(Flag, Status) :-
    deepfind_flag(Flag, Opts),
    member(Status, Opts),
    ( config_flag(Flag, _) ->  
        ( retract_fact(config_flag_(Flag, _)) -> true ; fail ),
        assertz_fact(config_flag_(Flag,Status))
    ;
        assertz_fact(config_flag_(Flag,Status))
    ).

%available only for analysis search
:- export(set_pred_class/1).
:- pred set_pred_class(C) : atm(C)
    # "Select predicate class @var{C} for searching, i.e., exported, 
      defined... (exported by default).".
set_pred_class(C) :-
    set_fact(pred_class(C)).

:- pred set_search_opts(Opts) : list(Opts) #"Sets a list of search @var{Opts}.".
set_search_opts([]).
set_search_opts([Opt|Opts]) :-
    set_search_opt(Opt),
    set_search_opts(Opts).

%set_search_opt(domain(D)) :- add_domain(D).
set_search_opt(mod(M)) :- find_add_mod(M).

% TODO: use set_search_opts?
:- export(find_use_doms/1).
:- dynamic(find_use_doms/1).
find_use_doms([shfr, eterms]). % TODO: hardwired

:- pred find_add_bundle(BundleName) : atm(BundleName) #"Adds a bundle to the search".
find_add_bundle(Name) :-
    ( find_loc(bundle, Name) ->
        true
    ;
        ( bundle_path(Name, '.', _) ->
            assertz_fact(find_loc(bundle, Name)),
            set_fact(internal_flag(updated_locs, yes))
        ;
            message(error, ['Bundle can not be located'])
        )
    ).
:- pred find_rm_bundle(BundleName) : atm(Mod)
    #"Removes a @var{Mod} from the search.".
find_rm_bundle(Name) :-
    ( find_loc(bundle, Name) ->
        ( retract_fact(find_loc(bundle, Name)) -> true ; fail ),
        set_fact(internal_flag(updated_locs, yes))
    ;
        message(error, ['This bundle was not present'])
    ).

:- pred find_add_dir(Path) : atm(Path)
    #"Sets a directory path within which code will be found.".
find_add_dir(Path0) :-
    fixed_absolute_file_name(Path0, '.', Path),
    ( find_loc(dir, Path) ->
        true
    ;
        ( file_exists(Path) ->
            assertz_fact(find_loc(dir, Path)),
            set_fact(internal_flag(updated_locs, yes))
        ;
            message(error, [~~(Path), ': does not exist'])
        )
    ).
:- pred find_rm_dir(Path) : atm(Path)
    #"Removes a @var{Mod} from the search.".
find_rm_dir(Path0) :-
    fixed_absolute_file_name(Path0, '.', Path),
    ( find_loc(dir, Path) ->
        ( retract_fact(find_loc(dir, Path)) -> true ; fail ),
        set_fact(internal_flag(updated_locs, yes))
    ;
        message(error, ['This directory was not present'])
    ).

:- pred find_add_mod(ModPath) : atm(ModPath)
    #"Adds to the module search set within which code will be checked.".
find_add_mod(ModPath0) :-
    absolute_file_name(ModPath0, ModPath),
    ( find_loc(mod, ModPath) ->
        true
    ;
        ( file_exists(ModPath) ->
            get_mod_name_from_file(ModPath, ModName),
            assertz_fact(find_loc(mod, pl(ModName, ModPath))),
            set_fact(internal_flag(updated_locs, yes))
        ;
            message(error, [~~(ModPath), ': does not exist'])
        )
    ).

:- pred find_rm_mod(ModPath0) : atm(ModPath0)
    #"Removes a @var{Mod} from the search.".
find_rm_mod(ModPath0) :-
    absolute_file_name(ModPath0, ModPath),
    ( find_loc(mod, pl(ModPath, _)) ->
        ( retract_fact(find_loc(mod, (pl(ModPath, _)))) -> true ; fail )
    ;
        message(error, ['This module was not present'])
    ).

:- export(list_locs/2).
:- pred list_locs/2 #"".
list_locs(bundle, Bs) :-
    findall(X, find_loc(bundle, X), Bs).
list_locs(dir, Bs) :-
    findall(X, find_loc(dir, X), Bs).
list_locs(mod, Bs) :-
    findall(X, find_loc(mod, X), Bs).

:- export(reset_search_locs/0).
:- pred reset_search_locs/0 #"Removes all search locations".
reset_search_locs :-
    retractall_fact(find_loc(_, _)).

:- export(list_flags/1).
:- pred list_flags(-Flags) => list(Flags)
    #"Returns all current flags of @apl{DeepFind} and their values.".
list_flags(Flags) :-
    findall(flag(X,Opts, Curr), (deepfind_flag(X,Opts), config_flag(X, Curr)), Flags).

%%%%%%%%%%%% INTERNAL FLAGS %%%%%%%%%%%%%%%%%%

:- data internal_flag/2.
% updated_locs

:- initialization(set_defaults).

:- pred clean_search_opts/0 # "Reset search options".
clean_search_opts :-
    retractall_fact(find_loc(_,_)),
    set_defaults.

set_defaults :-
    asserta_fact(search_arity(0)),
    asserta_fact(pred_class(defined_pred)),
    retractall_fact(config_flag_(_,_)),
    asserta_fact(config_flag_(allow_not_preanalyzed, on)),
    asserta_fact(config_flag_(reanalyze, off)),
    asserta_fact(config_flag_(mult_dump_load, off)),
    retractall_fact(internal_flag(_,_)),
    asserta_fact(internal_flag(updated_locs, yes)),
    asserta_fact(internal_flag(all_mods_loaded, no)).

my_module(X) :-
    find_set_pp_flags, !,
    module(X), !. % TODO: module/1 leaves choicepoints

:- doc(section, "Analysis search predicates").

analyze_domains :- % TODO: use analyze and diagnose from ciaopp_master?
    % domain(D), % TODO: set automatically when calling my_module
    find_use_doms(Ds), member(D, Ds),
    ( analyze(D) -> true ; fail),
    fail.
analyze_domains.

% % TODO: fix, this is done to avoid adding the fake predicate used to search to the pred list.
% get_module_predicate(Class, Pred, Mod, SearchMod) :-
%     current_itf(Class, Pred, Mod),
%     ( Mod = SearchMod ->
%         fail
%     ;
%         true
%     ).

% TODO: replace atm(Status) for assertion status regular type
:- pred findp(+Assertions, -Pred, -Residue, Status) => atm(Status)
    #"Finds a predicate matching @var{Assertions}, @var{Pred} is
    the predicate that matches, @var{Residue} is how well it suits
    the assertions and @var{Status} is a summary of the residue".
findp(Ass, Pred, Residue, Status) :-
    set_fact(dump_size(0)),
    set_fact(restore_time_(0)),
    set_fact(check_time_(0)),
    findp_(Ass, ResidueInfo),
    ( no_query_as ->
        Pred = ResidueInfo,
        Residue = [],
        Status = checked
    ;
        ResidueInfo = matching(P, Residue),
        search_arity(A),
        module_split(P, Mod, PredName),
        Pred = Mod:PredName/A,
        summarize_predicate(Residue, [checked,true], TState),
        ( TState = proven ->
            Status = checked
        ;
            summarize_false_predicate(Residue, FState),
                ( FState = proven ->
                    Status = false
                ;
                    Status = check
                )
        )
    ).

:- pred summarize_predicate(Conditions, States, Status): list * list * var
    #"Reports in @var{Status} if a predicate was proven to hold a
     list of assertion @var{States} (checked, true, false...) in
     some @var{Conditions}".
% One condition is proven if it can be proven in any abstract domain.
summarize_predicate([Cond|Conds], States, Status) :-
    Cond = cond_info(_Ass, DomsInfo),
    summarize_cond(DomsInfo, States, Matches),
    ( Matches = proven ->
      summarize_predicate(Conds, States, Status)
    ; Status = not_proven
    ).
summarize_predicate([], _, proven).

% If one condition is false, we are not interested in the predicate
summarize_false_predicate([Cond|Conds], Status) :-
    Cond = cond_info(_Ass, DomsInfo),
    summarize_cond(DomsInfo, [false], Matches),
    ( Matches = proven ->
      Status = proven
    ;
      summarize_false_predicate(Conds, Status)
    ).
summarize_false_predicate([], not_proven).

:- pred summarize_cond(Domains, MatchStates, Matches) : list(atm) * list * var
    #"Checks if in any domain in @var{Domains} any of
     @var{MatchStates} is proven".
summarize_cond([Dom|Doms], MatchStates, Matches) :-
    Dom = dom_info(_Dom, Status, _),
    ( member(Status, MatchStates) ->
       Matches = proven
    ;
        summarize_cond(Doms, MatchStates, Matches)
    ).
summarize_cond([], _, not_proven).

findp_(_, _) :-
    clean_stats,
    \+ find_loc(_, _), !,
    message(error, ['No search modules specified. Type help(deepfind(find)). to see how to add them.']),
    fail.
findp_(As, P) :-
    process_search_parameters(As),
    ( no_query_as ->   % search with apropos only
        apropos_search(P)
    ;
        ( ( config_flag(reanalyze,off)
          ; config_flag(reanalyze,on), config_flag(allow_not_preanalyzed,off)
          ) ->
            Status = safe_module
        ;
            Status = loadable_module
        ),
        overall_search(Status, LP),
        member(P, LP)
    ).

process_search_parameters(Ass) :-
    process_query(Ass, SaveMod),
    path_splitext(SaveMod, BaseMod, '.pl'),
    path_basename(BaseMod, NameMod),
    store_search_data(SaveMod, NameMod),
    set_fact(aux_module(SaveMod)).

:- meta_predicate overall_search(pred(2), ?).
:- pred overall_search(ModStatus, P) : var(P) => list(P)
    #"Search for predicates analyzing the modules each time".
overall_search(ModStatus, LP) :-
    update_tasks_analysis_status,
    find_use_doms(AllDoms),
    analyze_search_mods([timeout(10),analysis(AllDoms)]),
    update_task_status,
    extract_info_from_locs,
    mod_list(ModList, ModStatus),
    ( config_flag(mult_dump_load, on), config_flag(reanalyze,off) ->
        restore_all_analyses(ModList)
    ;
        %retract_fact(internal_flag(all_mods_loaded, _)),
        % TODO: uncomment when loading multiple modules is allowed
        %asserta_fact(internal_flag(all_mods_loaded, no))
        true
    ),
    % reset updated_locs flag
    ( retract_fact(internal_flag(updated_locs, _)) -> true ; fail ),
    asserta_fact(internal_flag(updated_locs, no)),
    ( ModList = [] ->
        display('No analyzable modules available'),
        fail
    ;
        get_stored_assertions(As),
        check_global_predicates(As, ModList, LP),
        aux_module(SearchMod),
        delete_modblob(SearchMod)
    ).

:- pred store_search_data(SearchModPath, ModName) : atm * atm
    #"Stores the info relating the search (assertions specified by
     the user and their arity). It uses different data structures
     than ciaopp because it removes them when restoring analysis".
store_search_data(_,_) :-
    no_query_as, !.
store_search_data(SearchModPath, Mod) :-
    retractall_fact(search_assertion(_)),
    my_module(SearchModPath),
    get_initial_assertions(As, Mod),
    As = [A|_],
    A = as${head => Head},
    functor(Head, _, Arity),
    set_fact(search_arity(Arity)),
    assert_search_assertions(As).

assert_search_assertions([A|As]) :-
    asserta_fact(search_assertion(A)),
    assert_search_assertions(As).
assert_search_assertions([]).   

get_initial_assertion(A, Mod) :-
    A = as${status=>check, head => Head},
    get_assertion(_,A),
    functor(Head, F, _),
    current_itf(exports, Head2, Mod),
    functor(Head2, F, _).

get_initial_assertions(As, Mod) :-
    findall(X, get_initial_assertion(X, Mod), As).

get_stored_assertions(As) :-
    findall(X, search_assertion(X), As).

:- pred create_assertion(+PrevAssrt, +NewGoalName, -NewAssrt) : atm(NewGoalName)
    #"Creates an assertion @var{NewAssrt} based on @var{PrevAssrt} 
      with @var{NewGoalName}".
create_assertion(PrevAssrt, NewGoalName, NewAssrt) :-
    PrevAssrt = as${ status=>check, head=>Goal },
    replace_name_goal(Goal, NewGoalName, NewGoal),
    assertion_set_head(PrevAssrt, NewGoal, NewAssrt).

:- pred replace_name_goal/3 #"Replaces the name of a goal keeping its terms".
replace_name_goal(Goal, NewName, NewGoal) :-
    Goal =.. [_|Args],
    NewGoal =.. [NewName|Args].

:- pred check_assertion_for_predicate_(+A, +Head, +Domain, -Status, -SimplA)
    #"Checks an assertion @var{A} in a @var{Domain}, outputs the @var{Status}
        of the assertion and the simplified assertion @var{SimplA}".
check_assertion_for_predicate_(A, Head, Domain, Status, SimplA):-
    functor(Head, Name, _),
    A = as${ status => check },
    create_assertion(A, Name, NewA),
    predkey_from_sg(Head, Key),
    decide_get_info(Domain, Key, Head, Cmpls),
    abs_exec_one_assertion(Domain, Cmpls, NewA, Name, SimplA, Status).

:- pred check_assertions_for_predicate(+Assertions, +Head, -OutInfo)
    #"Checks several @var{Assertions} for the same predicate and 
      computes an AND between them".
check_assertions_for_predicate([A|As], Head, OutInfo) :-
    findall(X, domain(X), Domains),
    OutInfo = [cond_info(A, DomInfo)|CondInfo],
    check_domains_for_assertion_pred(Domains, A, Head, DomInfo),
    check_assertions_for_predicate(As, Head, CondInfo).
check_assertions_for_predicate([], _, []).

% TODO: Commented for demo, uncomment?
check_domains_for_assertion_pred([Dom|Domains], A, Head, Res) :- 
%       Res = [dom_info(Dom,St)|Res0],
%       ( check_assertion_for_predicate_(A, Head, Dom, St) -> true
%       ; throw(bug(check_assertion_for_predicate_(A, Head, Dom, St), failed))
%       ),
    ( check_assertion_for_predicate_(A, Head, Dom, St, SimplA) ->
        Res = [dom_info(Dom,St, SimplA)|Res0]
    ; %format('BUG: ~w failed!~n', [check_assertion_for_predicate_(A, Head, Dom, St)]),
      % IG: commented for demo
      Res = Res0
    ),
    check_domains_for_assertion_pred(Domains, A, Head, Res0).
check_domains_for_assertion_pred([], _A, _Head, []).

:- pred check_predicates/3
    #"Checks assertion for a list of loaded predicates".
check_predicates(As, [PredName|LNames], NewFPreds) :- !,
    search_arity(Arity),
    functor(Head, PredName, Arity),
    check_assertions_for_predicate(As, Head, OutInfo),
    NewFPreds = [matching(PredName, OutInfo)|FPreds],
    check_predicates(As, LNames, FPreds).
check_predicates(_, [], []).

valid_pred(valid, FPreds, Name, [Name|FPreds]):- !.
valid_pred(_, FPreds, _, FPreds).

get_predicates_with_arity(Arity, []) :-
    Arity < 0, !.
get_predicates_with_arity(Arity, ValidPreds) :-
    preds(Preds),
    get_predicates_with_arity_(Preds, Arity, ValidPreds).

get_predicates_with_arity_([Pred|Preds], Arity, ListValid) :- !,
    ( functor(Pred, _, Arity)  ->
      ListValid = [Pred|ValidPreds]
    ; ListValid = ValidPreds
    ),
    get_predicates_with_arity_(Preds, Arity, ValidPreds).
get_predicates_with_arity_([], _, []).

% TODO: fix modes of referencing
:- pred check_global_predicates(+Assertions, +Mods, -FoundPredicates)
    #"@var{FoundPredicates} are the predicates from @var{Mods} 
      that check @var{Assertions}".
check_global_predicates(Assrts, [pl(ModPath, ModName)|ModList], FoundPreds) :-
    !,
    check_predicates_in_module(Assrts, ModPath, ModName, Preds),
    append(Preds, RestPreds, FoundPreds),   
    check_global_predicates(Assrts, ModList, RestPreds).
check_global_predicates(_, [], []).

check_predicates_in_module(_Assertions, _ModPath, ModName, []) :-
    config_flag(allow_not_preanalyzed,off),
    Flags = [], % Flags not used at the moment
    \+ safe_module(ModName, Flags),
    !.
check_predicates_in_module(Assertions, ModPath, ModName, CheckedPreds) :-
    search_arity(Arity),
    get_filtered_predicates(ModName, Arity, Preds),
    ( Preds = [] ->
       CheckedPreds = Preds
    ;
        ( config_flag(reanalyze,on) ->
            stat(ModName, analysis, make_analysis(ModPath, ModName))
        ;
            stat(ModName, restore, restore_analysis(ModPath, ModName))
        ),
        stat(ModName, check, check_predicates(Assertions, Preds, CheckedPreds))
    ).

:- data stat_runtime/3.
% stat_runtime(Mod, What, Time)

:- pred display_search_stats/0
    #"Displays the list of individual modules within which code is to be found".
display_search_stats :-
    mod_list(Mods, any_module),
    member(pl(_, Mod), Mods),
    display_mod_dump_stats(Mod),
    fail.
display_search_stats.

display_mod_dump_stats(Mod) :-
    ( config_flag(reanalyze,on) ->
        stat_runtime(Mod, analysis, TA)
    ;
        stat_runtime(Mod, restore, TA)
    ),
    %stat_runtime(Mod, check, TC),
    mod_list(ModList, any_module),
    member(pl(FilePath, Mod), ModList),
    % TODO: hardwired domains
    find_use_doms(AllDoms),
    display_mod_dump_absint_stats(AllDoms, Mod, FilePath, TA).

display_mod_dump_absint_stats([], _Mod, _FilePath, _TA).
display_mod_dump_absint_stats([AbsInt|AbsInts], Mod, FilePath, TA) :-
    dump_file(FilePath, Mod, AbsInt, FileDump),
    file_property(FileDump, size(Size)),
    add_size(Size),
    format('~w & ~w & ~w & ~w\\\\ \\hline~n', [Mod, AbsInt, Size, TA]),
    display_mod_dump_absint_stats(AbsInts, Mod, FilePath, TA).

:- data dump_size/1.
:- pred dump_size(S) => num(S) #"@var{S} is the amount of memory (in B) needed to store an analysis dump.".
add_size(Size) :-
    dump_size(CurrSize),
    NewSize is CurrSize + Size,
    set_fact(dump_size(NewSize)).

:- data restore_time_/1.
add_restore_time(Time) :-
    restore_time_(CurrTime),
    NewTime is CurrTime + Time,
    set_fact(restore_time_(NewTime)).

:- pred restore_time(T) => num(T) #"@var{T} is the time spent restoring modules.".
restore_time(Time) :-
    %statistics(ckfreq,Clockfreq_result),
    restore_time_(T),
    Time = T.
    %Time is (T / Clockfreq_result) * 1000.

:- data check_time_/1.
add_check_time(Time) :-
    check_time_(CurrTime),
    NewTime is CurrTime + Time,
    set_fact(check_time_(NewTime)).

:- pred check_time(T) => num(T)
    #"@var{T} is the time (in msecs) spent checking conditions.".
check_time(Time) :-
    %statistics(wallclockfreq,Clockfreq_result),
    check_time_(T),
    Time = T.
    %Time is (T / Clockfreq_result) * 1000.

clean_stats :-
    retractall_fact(stat_runtime(_, _, _)).

clean_analysis :-
    clean_analysis_info,
    ( internal_flag(all_mods_loaded, _) ->
        retractall_fact(internal_flag(all_mods_loaded, _))
    ;
        true
    ),
    asserta_fact(internal_flag(all_mods_loaded, no)).

:- meta_predicate stat(?, ?, goal).
stat(Mod, What, Goal) :-
    %statistics(walltick, [T0|_]),
    statistics(runtime, [T0, _]),
    Goal,
    %statistics(walltick, [T|_]),
    statistics(runtime,[T, _]),
    Time is T - T0,
    retractall_fact(stat_runtime(Mod, What, _)),
    assertz_fact(stat_runtime(Mod, What, Time)),
    ( What = check ->
        add_check_time(Time)
    ;   
        ( What = restore ->
            add_restore_time(Time)
        ;   true
        )
    ).

:- pred get_filtered_predicates(+ModName, +Arity, -Preds)
    #"@var{Preds} is a list of predicates in a @var{ModName} of 
       arity @var{Arity}".
get_filtered_predicates(ModName, Arity, Preds) :-
    findall(X, exported_pred(ModName, Arity, X), Preds).

:- pred meets_keyword_spec(+Word, +Spec).
% TODO: switch order if one is more efficient than the other
meets_keyword_spec(Word, Spec) :- % fuzzy spec
    damerau_lev_dist(Word, Spec, N),
    N =< 1, !.
meets_keyword_spec(Word, Spec) :- % regexp spec
    atom_codes(Word, WCodes),
    atom_codes(Spec, SCodes),
    match_posix(SCodes,WCodes).

:- pred exported_pred(+ModName, +Arity, -Pred)
    #"@var{Pred} is a exported predicate in module @var{ModName} of arity 
        @var{Arity} that matches keyword specification (if any)".
exported_pred(ModName, Arity, CompPred) :-
    exports(ModName, P, Arity),
    ( keyword_spec(Spec) ->
        meets_keyword_spec(P, Spec),
        Pred = P
    ;
        Pred = P
    ),
    module_concat(ModName, Pred, CompPred).
    
:- pred restore_analysis(+FilePath, +Mod) # "Restores dump of a module 
    that was previously analyzed".
%restore_analysis(_,_) :-
%       config_flag(mult_dump_load, on), !.
restore_analysis(FilePath, Mod) :-
    find_use_doms(AllDoms),
    dump_file_all_doms(AllDoms, FilePath, Mod, Dumps),
    clean_analysis,
    find_set_pp_flags,
    restore_all(Dumps).

dump_file_all_doms([], _, _, []).
dump_file_all_doms([AbsInt|AbsInts], FilePath, Mod, [Dump|Dumps]) :-
    dump_file(FilePath, Mod, AbsInt, Dump),
    dump_file_all_doms(AbsInts, FilePath, Mod, Dumps).

restore_all([]).
restore_all([F|Fs]) :- restore(F), restore_all(Fs).

% TODO: uncomment when module does not delete analysis info
:- pred restore_all_analyses(ModPaths) : list(ModPaths)
    #"Restores a list of analysis dumps, previously saved with ciaopp_flag '' ".
restore_all_analyses(Mods) :-
    %( ( internal_flag(updated_locs, yes) ; internal_flag(all_mods_loaded, no)) ->
    clean_analysis,
    restore_all_analyses_(Mods).
%       ;
%           true
%       ).

restore_all_analyses_([]).
restore_all_analyses_([pl(ModPath, ModName)|ModPaths]) :-
    find_use_doms(AllDoms),
    dump_file_all_doms(AllDoms, ModPath, ModName, Dumps),
    restore_all(Dumps),
    restore_all_analyses_(ModPaths).

:- pred make_analysis/2 # "Loads and analyzes a module and dumps the info to disk".
make_analysis(FilePath, Mod) :-
    clean_analysis,
    path_split(FilePath, Path, _),
    path_concat(Path, Mod, ModPath),
    aux_module(SearchMod),
    my_module([SearchMod, ModPath]),
    analyze_domains.

% ------------------------------------------------------------------------
:- doc(section, "Apropos search").

apropos_search(M:P/A) :-
    extract_info_from_locs,
    keyword_spec(Spec),
    exports(M,P,A),
    meets_keyword_spec(P, Spec).

:- export(extract_info_from_locs/0).
:- pred extract_info_from_locs/0 #"extract_info_from_locs".
extract_info_from_locs :-
%       internal_flag(updated_locs, yes), !,
    retractall_fact(search_module(_,_)),
    extract_info_from_mods,
    extract_info_from_dirs,
    extract_info_from_bundles,
    findall(ModPath, search_module(_, ModPath), ModPaths),
    get_exports(ModPaths).
%extract_info_from_locs.

extract_info_from_mods :-
    find_loc(mod, pl(ModName, ModPath)),
    ( search_module(ModName, _) ->
        true
    ;
        asserta_fact(search_module(ModName, ModPath))
    ),
    fail.
extract_info_from_mods.

extract_info_from_dirs :-
    find_loc(dir, Path),
    current_file_find([proj(compilable), srctype(module)], Path, ModPath),
    get_mod_name_from_file(ModPath, ModName),
    ( search_module(ModName, _) -> 
      % already added module 
        true
    ; loadable_module(ModName, []) ->
        asserta_fact(search_module(ModName, ModPath))
    ; true
    ),
    fail.
extract_info_from_dirs.

extract_info_from_bundles :-
    find_loc(bundle, Bundle),
    bundle_contents(Bundle, [srctype(module), proj(compilable)], ModPath),
    get_mod_name_from_file(ModPath, ModName),
    ( search_module(ModName, _) -> 
      % already added module 
      true
    ;
        asserta_fact(search_module(ModName, ModPath))
    ),
    fail.
extract_info_from_bundles.

%:- export(mod_list/2).
:- meta_predicate mod_list(?, pred(2)).
:- pred mod_list(L, ModSt) => list(L) #"@var{L} is the list of modules to 
      analyze that hold the property @var{ModSt}.".
mod_list(L, ModStatus) :-
    findall(pl(Path, Mod), valid_module(Mod, Path, ModStatus), L).

:- export(mod_path/2).
:- pred mod_path(Mod, Path) #"@var{Path} is the location of @var{Mod}".
mod_path(Mod, Path) :-
    search_module(Mod, Path).

% TODO: discard modules that timeout?
:- meta_predicate valid_module(?, ?, pred(2)).
valid_module(Mod, Path, ModStatus) :-
    search_module(Mod, Path),
    ModStatus(Mod, []).

:- pred safe_module(Mod, Flags)
    #"A @var{Mod} is safe if it could be loaded and analyzed.".
safe_module(Mod, Flags) :-
    task_status(Mod, _, module, Flags, ok),
    find_use_doms(AllDoms),
    safe_module_(AllDoms, Mod, Flags).

safe_module_([], _Mod, _Flags).
safe_module_([AbsInt|AbsInts], Mod, Flags) :-
    task_status(Mod, _, AbsInt, Flags, ok),
    safe_module_(AbsInts, Mod, Flags).

:- pred loadable_module(Mod, Flags)
    #"A @var{Mod} is loadable if it could be loaded without errors".
loadable_module(Mod, Flags) :-
    task_status(Mod, _, module, Flags, ok).

:- pred any_module(Mod, Flags) #"@var{Mod} is a prolog module.".
any_module(_,_).

% -------------------------------------------------------------------------
:- doc(section, "Predicates to update analysis").

:- pred analyze_search_mods(Opts) : list(Opts)
    #"Analyzes modules specified for searching".
analyze_search_mods(Opts) :-
    findall(Path, find_loc(mod, pl(_ModName, Path)), MPaths),
    findall(Path, find_loc(dir, Path), DPaths),
    findall(Path, (find_loc(bundle, BundleName), bundle_path(BundleName, '.', Path)), BPaths),
    append(MPaths, DPaths, Paths1),
    append(Paths1, BPaths, Paths2),
    analysis_start(Paths2, Opts).

% --------------------------------------------------------------------------
:- doc(section, "Future Use (fuzzy search)").

% This will be useful if we implement fuzzy predicate search
% This is thought to work for only one variable
replace_call_assertion(PrevAssrt, Prop, NewAssrt) :-
    PrevAssrt = as${ call=>Calls },
    replace_prop(Calls, Prop, NewCalls),
    assertion_set_calls(PrevAssrt, NewCalls, NewAssrt).

replace_success_assertion(PrevAssrt, CallProp, SuccProp, NewAssrt) :-
    PrevAssrt = as${ call=>Calls, succ=>Success },
    replace_prop(Calls, CallProp, NewCalls),
    replace_prop(Success, SuccProp, NewSuccess),
    assertion_set_calls(PrevAssrt, NewCalls, Assrt0),
    assertion_set_success(Assrt0, NewSuccess, NewAssrt).

replace_prop([PrevProp], NewName, [NewProp]) :-
    PrevProp =.. [_|X],
    NewProp =.. [NewName|X].

:- export(find_assrt_status/1).
:- prop find_assrt_status(Status) => assrt_status
    #"Result of the proof of the whole set of conditions
@begin{itemize}
@item @var{Status} = (true ; checked). The conditions could be proved to hold.
@item @var{Status} = false. Conditions were proved to be false.
@item @var{Status} = check. Neither false nor true could be proved.
@end{itemize}
".
:- regtype find_assrt_status/1.
find_assrt_status(checked).
find_assrt_status(check).
find_assrt_status(false).
