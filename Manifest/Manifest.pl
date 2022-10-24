:- bundle(deepfind).

depends([
    core,
    ciaopp
]).

alias_paths([
    deepfind = 'src'
]).

lib('src').

manual('deepfind', [main='doc/SETTINGS.pl']).

