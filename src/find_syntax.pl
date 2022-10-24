:- package(find_syntax).

:- use_package(assertions).
:- set_prolog_flag(read_curly_blocks, on).
:- set_prolog_flag(read_postfix_blocks, on).
:- op(50, yf, ({})).

% TODO: *bug?*
%   
%   I wrote X = {
%       X([]). accidentally and deleted that line but for ciao it seems
%           that it was already stored because it raised a syntax error:        
%   X = {
%      x([]).
%      x([_]).
%      },  X = '\006\curly_block'(Clauses), new_modblob(Clauses, [x/1], File).
%   {SYNTAX ERROR: (lns 162-166) }, ., or operator expected
%   X = { X
%   ** here **
%   ( [ ] ) . x( [ ] ) . x( [ _ ] ) . } , X = curly_block( Clauses ) , new_modblob( Clauses , [ x / 1 ] , File ) .
%   }
