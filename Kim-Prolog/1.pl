%% AdventOfCode day 1 1.pl
%% swi-prolog
%% compile: ['1.pl']
%%
%% calc_floor(+,-)
%%
%% calc_floor(Input,Output)
%% Output is the floor level of Input.

calc_floor([],0).
    
calc_floor([40|T],R1):-
    calc_floor(T,R2),
    R1 is R2 + 1.

calc_floor([41|T],R1):-
    calc_floor(T,R2),
    R1 is R2 - 1.

test:-
    calc_floor("(())",0),
    calc_floor("()()",0),
    calc_floor("(((",3),
    calc_floor("(()(()(",3),
    calc_floor("())",-1),
    calc_floor("))(",-1),
    calc_floor(")))",-3),
    calc_floor(")())())",-3).
