%% AdventOfCode day 5 5.pl
%% swi-prolog
%% compile: ['5.pl']
%%
%% count_is_nice(+,-)
%%
%% count_is_nice(Input,Output)
%% Output is number of "nice" strings of Input. Input is comma-separated strings.

count_is_nice(Input,Result):-
    count_is_nice_traverse(Input,0,Result).

count_is_nice_traverse([X|Xs],I,Result):-
    X =\= 44,
    count_is_nice_traverse(Xs,[X],I,Result).

count_is_nice_traverse([X|Xs],String,I,Result):-
    X =\= 44,
    append(String,[X],String1),
    count_is_nice_traverse(Xs,String1,I,Result).

count_is_nice_traverse([44|Xs],String,I,Result):-
    is_nice(String),
    I1 is I + 1,
    count_is_nice_traverse(Xs,[],I1,Result).

count_is_nice_traverse([44|Xs],String,I,Result):-
    \+ is_nice(String),
    count_is_nice_traverse(Xs,[],I,Result).

count_is_nice_traverse([],String,I,Result):-
    is_nice(String),
    Result is I + 1.

count_is_nice_traverse([],String,I,I):-
    \+ is_nice(String).

is_nice(Input):-
    three_vowels(Input),
    twice_in_a_row(Input),
    \+ contain_ab_cd_pq_xy(Input).

three_vowels(Input):-
    three_vowels(Input,0,3).

three_vowels([97|Xs],I,R):-
    I1 is I+1,
    three_vowels(Xs,I1,R).

three_vowels([101|Xs],I,R):-
    I1 is I+1,
    three_vowels(Xs,I1,R).

three_vowels([105|Xs],I,R):-
    I1 is I+1,
    three_vowels(Xs,I1,R).

three_vowels([111|Xs],I,R):-
    I1 is I+1,
    three_vowels(Xs,I1,R).

three_vowels([117|Xs],I,R):-
    I1 is I+1,
    three_vowels(Xs,I1,R).

three_vowels([X|Xs],I,R):-
    \+ X=97,
    \+ X=101,
    \+ X=105,
    \+ X=111,
    \+ X=117,
    three_vowels(Xs,I,R).

three_vowels([],I,I).

twice_in_a_row([X|Xs]):-
    twice_in_a_row(Xs,X).

twice_in_a_row([X|Xs],X).

twice_in_a_row([X|Xs],Y):-
    twice_in_a_row(Xs,X).

twice_in_a_row([],_):-
    fail.

contain_ab_cd_pq_xy([97,98|_]).

contain_ab_cd_pq_xy([99,100|_]).

contain_ab_cd_pq_xy([120,121|_]).

contain_ab_cd_pq_xy([X|Xs]):-
    contain_ab_cd_pq_xy(Xs).

contain_ab_cd_pq_xy([]):-
    fail.

test:-
    is_nice("ugknbfddgicrmopn"),
    is_nice("aaa"),
    \+ is_nice("jchzalrnumimnmhp"),
    \+ is_nice("haegwjzuvuyypxyu"),
    \+ is_nice("dvszwmarrgswjxmb"),
    count_is_nice("aaa,ugknbfddgicrmopn,jchzalrnumimnmhp",2),
    count_is_nice("aaa,ugknbfddgicrmopn,jchzalrnumimnmhp,aaa,aaa",4).
