%% AdventOfCode day 2 2.pl
%% swi-prolog
%% compile: ['2.pl']
%%
%% paper(+,+,+,-)
%%
%% paper(Length,Width,Height,Output)
%% Output is the required wrapping paper for a gift of size Length,Width,Height.

paper(L,W,H,R):-
    R is 2*L*W + 2*W*H + 2*H*L + min(min(L*W,L*H),W*H).

test:-
    paper(2,3,4,58),
    paper(1,1,10,43).
