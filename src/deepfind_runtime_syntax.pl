:- module(deepfind_runtime_syntax, [enable_deepfind_syntax/0], []).

:- use_module(library(operators)).

% ---------------------------------------------------------------------------
% Define syntax for deepfind at runtime
% TODO: cannot deactivate syntax! reuse code from compiler or define a
%   "read term with context"
% TODO: do not use use_package(runtime_ops) unless there is a way to activate/deactivate them

:- use_module(engine(runtime_control), [set_prolog_flag/2]).

enable_deepfind_syntax :-
    % flags for hiord
    set_prolog_flag(read_hiord, on),
    % operators for assertions
    op(975, xfx,(=>)),
    op(978, xfx,(::)),
    op(1150, fx,(decl)),
    op(1150,xfx,(decl)),
    op(1150, fx,(pred)),
    op(1150,xfx,(pred)),
    op(1150, fx,(func)),
    op(1150,xfx,(func)),
    op(1150, fx,(prop)),
    op(1150,xfx,(prop)),
    op(1150, fx,(modedef)),
    op(1150,xfx,(modedef)),
    op(1150, fx,(calls)),
    op(1150,xfx,(calls)),
    op(1150, fx,(success)),
    op(1150,xfx,(success)),
    op(1150, fx,(test)),
    op(1150,xfx,(test)),
    op(1150, fx,(texec)),
    op(1150,xfx,(texec)),
    op(1150, fx,(comp)),
    op(1150,xfx,(comp)),
    op(1150, fx,(entry)),
    op(1150,xfx,(entry)),
    op(1150, fx,(exit)),
    op(1150,xfx,(exit)),
    % operators for regtypes
    op(1150, fx,(regtype)),
    op(1150,xfx,(regtype)).


% ---------------------------------------------------------------------------
