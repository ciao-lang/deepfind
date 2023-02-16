:- use_package(assertions).

:- doc(filetype, documentation).

%:- doc(author, "Isabel Garcia-Contreras").

:- doc(title, "DeepFind usage").

:- doc(module, "

@section{Load bundle modules}

@begin{verbatim}
?- use_package(deepfind(find_syntax)).
?- use_module(deepfind(find)).
@end{verbatim}

@section{Set code location}

By default, DeepFind will look in your ciao installation directory 'core/lib'. 
This can be changed with predicates for:

@begin{itemize}
@item Adding individual modules to search in:

@begin{verbatim}
?- find_add_mod('~/ciao-devel/core/lib/llists.pl').
@end{verbatim}

@item Setting a directory to search in recursively:

@begin{verbatim}
?- find_add_dir('~/ciao-devel/core/lib').
@end{verbatim}

@end{itemize}

@section{Run a query}

Find predicates that performs operations with lists 

    ?- findp(@{ :- pred P(A,_,B) : list(A) => (list(A),list(B)). @},P,R,S).

Some results:

    P = idlists:substract/3,
    Sum = checked,
    Res = Explanation... ? ;

    P = stream_utils:read_to_end/3,
    Sum = false,
    Res = Explanation... ? ;

    P = set:ord_intersection/3,
    Sum = check,
    Res = Explanation... ? ; 


If you want only the predicates that meet the conditions to be
displayed, you can set the last argument to checked:

    ?- findp(@{ :- pred P(A,_,B) : list(A) => ((list(A),list(B)). @},P,R,S),
        S = checked.

@section{Combining with keyword search}

Restrictions over predicate names can be made with @tt{apropos} directive


    ?- findp(@{ As. :- apropos(Spec)@}, P, Res, S). 

Spec can be a regular expression or a keyword. Checking if the
predicate name meets the requirements is made as in @pred{apropos/1}
in the Ciao System. 

Example:


    ?- findp(@{ :- pred '_'/2 => term * string. :- apropos(write)@}, P, Res, S). 


@section{Search options}

There are two search options that can be chaged via flags:

@begin{itemize}
@item @bf{allow_not_preanalyzed}

@begin{itemize}
@item @bf{on}: analyzes modules although they might have not been analyzed before

@item @bf{off}: if a module wasn't marked as safe it is not analyzed
@end{itemize}
 This flag is useful if reanalysis is on, if not, preanalysis is
 needed for loading dumps.

@item @bf{reanalyze}

@begin{itemize}
@item @bf{on}: modules are analyzed before searching.
SLOW but allows user-defined types.

@item @bf{off}: information from dumps is loaded search in the module.
QUICK but less flexible. Useful for roughly discard predicates.

@end{itemize}
@end{itemize}


@section{Some examples}


Examples can be found in
@file{search_examples.pl}. This module can be loaded and used directly
for using @apl{DeepFind}.

For example:

@begin{verbatim}
?- search_demo(ID, _, Query, Comment, Explanation).

Comment = \"Predicates that display strings\",
Explanation = \"We use a success assertion instead of a calls assertion
because the code could lack calls assertions and the condition would not be
checked. If the query requires this condition on success, we avoid this
problem\",
ID = strings,
Query = {:-pred P/1=>string} ? 

@end{verbatim}

Each example has:
@begin{itemize}
    @item ID to differentiate it, all of them should be different.
    @item The arity of the query assertion.
    @item Query assertion.
    @item A short comment describing what kind of predicates the query is 
          thought to gind.
    @item [Optional] An explanation of why this query is useful to find the
          code described in the comment.
@end{itemize}

Here are some examples:

@includecode{search_examples}


").
