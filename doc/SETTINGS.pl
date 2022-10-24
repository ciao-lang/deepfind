:- module(_, [], [doccfg]).

%! \title Configuration for DeepFind manual
%  \author Jose F. Morales
%  \author Isabel Garcia-Contreras

:- include(core_docsrc(docpaths)).

filepath := '../src'.
filepath := '../tutorial'.
filepath := '.'.

filepath := ~ciaofilepath_common.

output_name := deepfind.

doc_structure :=
    'deepfind_man'-[
        'users_part'-[
            'deepfind_usage',
            'librowser/librowser'
        ],
        'reference_part'-[
            'bundle_structure',
            'find'-[
                'assertion_spec'
            ],
            'find_test'
        ]
    ].

%papertype := afourthesis.

%index := lib, pred.

doc_mainopts := no_biblio | no_bugs | no_patches.
doc_compopts := no_biblio | no_bugs | no_patches.
