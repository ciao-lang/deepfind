:- module(_, [], [ciaobld(bundlehooks)]).

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

'$builder_hook'(test) :- !,
    do_deepfind_tests.

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [invoke_ciaosh_batch/1]).

do_deepfind_tests :-
    working_directory(ThisDir, ~bundle_path(deepfind, 'tests')),
    invoke_ciaosh_batch([
      use_module(run_tests, [run_tests/0]),
      run_tests:run_tests
    ]),
    working_directory(_, ThisDir).
