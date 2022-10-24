:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(title,"Bundle structure").

:- doc(summary,"summary").

%:- doc(author,"Isabel Garcia-Contreras").

:- doc(module,"
The essential modules for finding code are:

@image{deepfind_structure}{210}{280}

@begin{itemize} 

@item @bf{Batch Analyzer}: This module performs the static analysis of a
  list of modules or the modules located in a user-specified
  path. Modules are analyzed individually and the analysis trusts the
  assertions for exported and imported predicates. The analysis
  results are cached on disk (as CiaoPP @tt{dump} files) and reused
  for each search. Also, a module report can be generated with
  analysis memory and time data, and timeout information.

@item @bf{Query Processor}: Preprocesses user's queries, i.e.,
  normalizes the anonymous assertions, extracts the assertion
  conditions and stores them so they can be checked later. It 
  extracts also predicate name requirements.
  
@item @bf{Condition Checker}: Its task is to prove that the
  conditions in the query hold or not for all predicates that
  potentially could meet the requirementes, i.e., those which have the same
  arity as specified in the query. 

@end{itemize}

Other modules have been implemented that complement the previous ones:

@begin{itemize}

@item @apl{DeepFind} @bf{pretty printer}: Transform the prolog output
of the query to html to improve results visualization.

@item @apl{DeepFind} @bf{benchmark}: Generates statistics about time
spent checking conditions.

@item @bf{Statistics Collecter}: gathers the output from the Batch
Analyzer and summarizes it.

@end{itemize}
").
