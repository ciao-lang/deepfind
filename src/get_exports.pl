:- module(get_exports,[],[assertions,hiord,datafacts]).

:- doc(title, "A simple module to extract exported predicates of a module").
:- doc(author, "Jose F. Morales").

:- use_module(engine(stream_basic), [absolute_file_name/2]).
:- use_module(library(compiler/c_itf), [process_files_from/7, defines_module/2, exports/5, false/1]).
:- use_module(library(errhandle), [error_protect/2]).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).

:- export(exports/3).
:- data exports/3.

:- export(get_exports/1).

get_exports(Fs) :-
    retractall_fact(exports(_,_,_)),
    get_exports_(Fs).

get_exports_([]).
get_exports_([F|Fs]) :- get_exports_mod(F), get_exports_(Fs).

get_exports_mod(F) :-
    absolute_file_name(F, Fb),
    error_protect(ctrlc_clean(
        process_files_from(F, asr, any, 
                   treat(Fb), notsame(Fb),
                    c_itf:false,
                    always)
                 ), fail).

always(_).

treat(Fb,Base) :-
    absolute_file_name(Base, Fb0),
    Fb = Fb0,
    defines_module(Base, Module),
    exports(Base, F, A, _DefType, _Meta),
    assertz_fact(exports(Module, F, A)),
    fail.
treat(_Fb,_Base).

notsame(Fb, Base) :-
    absolute_file_name(Base, Fb0),
    \+ Fb0 = Fb.
