%% AdventOfCode day 3 3.pl
%% swi-prolog
%% compile: ['3.pl']
%%
%% santa_route(+,-)
%%
%% santa_route(Input,Output)
%% Output is number of presents for Inputmoves.

:- dynamic visited/2.

undo:- retractall(visited(X,Y)).

santa_route(Route,R):-
    undo,
    asserta(visited(0,0)),
    santa_route(Route,0,0,1,R).

santa_route([],X,Y,I,I).

santa_route([60|T],X,Y,I,N):-
    X1 is X-1,
    (visited(X1,Y) 
    ->
    santa_route(T,X1,Y,I,N);    
    (asserta(visited(X1,Y)),
    I1 is I +1,
    santa_route(T,X1,Y,I1,N))).

santa_route([62|T],X,Y,I,N):-
    X1 is X+1,
    (visited(X1,Y) 
    ->
    santa_route(T,X1,Y,I,N);    
    (asserta(visited(X1,Y)),
    I1 is I +1,
    santa_route(T,X1,Y,I1,N))).

santa_route([94|T],X,Y,I,N):-
    Y1 is Y+1,
    (visited(X,Y1) 
    ->
    santa_route(T,X,Y1,I,N);    
    (asserta(visited(X,Y1)),
    I1 is I +1,
    santa_route(T,X,Y1,I1,N))).

santa_route([118|T],X,Y,I,N):-
    Y1 is Y-1,
    (visited(X,Y1) 
    ->
    santa_route(T,X,Y1,I,N);    
    (asserta(visited(X,Y1)),
    I1 is I +1,
    santa_route(T,X,Y1,I1,N))).

test:-
    santa_route(">",2),
    santa_route("^>v<",4),
    santa_route("^v^v^v^v^v",2).
