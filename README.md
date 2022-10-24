# Code search based on abstract interpretation

Deepfind is a bundle for the Ciao System which allows finding code in
set of modules defined by the user by specifying abstract semantic
characteristics of the arguments of the predicates.

See the manual for more details.

Note: this is work in progress (see TODO.md file)

## Build and installation

You can automatically fetch, build, and install this bundle using:

```sh
ciao get deepfind
```

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call a binary without specifying its full
path it is recommended to include this directory in your `PATH`:

```sh
export PATH=$CIAOPATH/build/bin:$PATH
# or export PATH=~/.ciao/build/bin:$PATH
```

**For developing** this bundle it is recommended to define `CIAOPATH`
(E.g., `~/ciao`) and clone this repository in your workspace. Remember to 
update registered bundles after cloning (E.g., `ciao rescan-bundles ~/ciao`).

## Usage

Usage:
```ciao
?- use_package(deepfind(deepfind)).
```

Selection of the search context:
```ciao
find_add_mod(<<path>>).% Add one module to the search
find_add_dir(<<path>>). % (or) add one directory for search
find_add_bundle(<<bundle name>>). % (or) add one bundle for search
```

To pre-analize the selected modules you may use:
```ciao
?- analyze_search_modules([]).
```
where the first argument are the options:
     * `no_incremental`: redo analysis for all modules
       (otherwise only those whose modification data has changed are
        reanalized -- note that this heuristic is incomplete)

Then:
```ciao
?- findp(Asrts, P, Residue, Status). % To enumerate predicates
```

Example:
```ciao
?- use_package(deepfind(deepfind)).
?- analyze_search_modules([]). 
?- findp({ :- pred P(A) => atm(A). }, Pred, Residue, Status). 
?- findp({ :- apropos('.*'). }, Pred, Residue, Status).
```
