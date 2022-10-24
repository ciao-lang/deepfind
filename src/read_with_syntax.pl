:- module(read_with_syntax, [], [assertions, regtypes, hiord]).

% Find syntax is not needed because the user is not going to write the
% set of query assertions in {}
:- use_module(deepfind(deepfind_runtime_syntax)).

:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(terms_io), [file_to_terms/2]).

:- export(string_read_with_syntax/2).
% TODO: this is an ad hoc solution for transforming in *our context*
string_read_with_syntax(String, Terms) :-
    enable_deepfind_syntax, % TODO: syntax not disabled!!!
    mktemp_in_tmp('stringXXXXXX', F),
    string_to_file(String, F),
    file_to_terms(F, Terms),
    del_file_nofail(F).
