:- [tests].
:- use_module(library(clpfd)). % Import the module
:- set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometime

snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
, checkRowClues(Solution,RowClues)
, checkColClues(Solution,ColClues)
, countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
, checkTouching(Solution)
, solutionCoords(Solution, 0, Coords)
, flatten(Coords, [First | Rest])
, snakeConnected([First], Rest) % snake must be connected
.

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[X|S]) :- (X = 0; X = 2), copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- Clue \= -1, copyRow(R,S).

%First row
check_neighbors_rows([],[B1,B2],[_,C2]) :- check_neighbors_pattern(B2,0,0,C2,B1).
check_neighbors_rows([],[W,M,E|RowB],[_,S,C3|RowC]) :-
	check_neighbors_pattern(M,0,E,S,W),
	check_neighbors_rows([],[M,E|RowB],[S,C3|RowC]).

%Last row
check_neighbors_rows([_,A2],[B1,B2],[]) :- 
	%touchCheck(A1, A2, B1, B2),
	check_neighbors_pattern(B2,A2,0,0,B1).

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[]) :-
	%touchCheck(A1, N, W, M), 
	check_neighbors_pattern(M,N,E,0,W),
	check_neighbors_rows([N,A3|RowA],[M,E|RowB],[]).

%Regular Case
check_neighbors_rows([_,A2],[B1,B2],[_,C2]) :- 
	%touchCheck(A1, A2, B1, B2), 
	check_neighbors_pattern(B2,A2,0,C2,B1).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
	%touchCheck(A1, N, W, M),
	check_neighbors_pattern(M,N,E,S,W),
	check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).


:- dynamic check_neighbors_pattern/5.
check_neighbors_pattern(0,_,_,_,_) :- !.
check_neighbors_pattern(Piece,N,E,S,W) :- 1 #=< Piece,
	count_cell(N,X1),
	count_cell(E,X2),
	count_cell(S,X3),
	count_cell(W,X4),
	Piece #= X1+X2+X3+X4, asserta(check_neighbors_pattern(Piece,N,E,S,W) :- !).

count_cell(0,0) :- !.
count_cell(1,1) :- !.
count_cell(2,1) :- !.
count_cell(_,1).

countNeighbors([]).
countNeighbors([RowA, RowB | Sol]) :- check_neighbors_rows([],[0 | RowA], [0 | RowB]), countNeighbors([RowA, RowB |Sol], notfirst).
countNeighbors([RowA, RowB], notfirst) :- check_neighbors_rows([0 | RowA], [0 | RowB], []).
countNeighbors([RowA, RowB, RowC | Sol], notfirst) :- check_neighbors_rows([0 | RowA],[0 | RowB], [0 | RowC]), countNeighbors([RowB, RowC | Sol], notfirst).

checkRowClues([],[]) :- !.
checkRowClues([_|Solution], [-1|RowClues]) :- checkRowClues(Solution, RowClues).
checkRowClues([Row|Solution], [Clue|RowClues]) :- 
    countRow(Row, Clue, 0),
    %checkSum(Clue, Row),
    checkRowClues(Solution, RowClues).

countRow([], Clue, Clue) :- !.
countRow([Cell|Row], Clue, X) :- count_cell(Cell, X1), Y is X + X1, countRow(Row, Clue, Y).

checkSum(Clue, Row) :- renumberAll(Row, Renumbered), sum_list(Renumbered, Clue).
renumberAll([0|L],[0|R]) :- renumberAll(L,R).
renumberAll([1|L],[1|R]) :- renumberAll(L,R).
renumberAll([2|L],[1|R]) :- renumberAll(L,R).
renumberAll([],[]).

checkColClues([], []) :- !.
checkColClues(Solution, ColClues) :- transpose(Solution, X), checkRowClues(X, ColClues).

checkTouching([]) :- !.
checkTouching([_]).
checkTouching([R1, R2 | Rows]) :- checkTouchingRows(R1, R2), checkTouching(Rows).

checkTouchingRows([_],[_]) :- !. 
checkTouchingRows([],[]) :- !.
checkTouchingRows([A,B|R1],[C,D|R2]) :- touchCheck(A,B,C,D), checkTouchingRows(R1,R2).

:- dynamic touchCheck/4. 
touchCheck(X1, X2, X3, X4) :-
	count_cell(X1,A),
	count_cell(X2,B),
	count_cell(X3,C),
	count_cell(X4,D),
	not((A=D, B=C, A+B+C+D #\= 0)),
	asserta(touchCheck(X1,X2,X3,X4) :- !),
	asserta(touchCheck(X2,X1,X4,X3) :- !). %Check to see if the opposite corners are equal.

neighbors([[X1,Y1]|_], [X2,Y2]) :- (abs(X2-X1) + abs(Y2-Y1)) #= 1, !.
neighbors([_|List],Coords) :- neighbors(List,Coords), !.

solutionCoords([], _, []).
%solutionCoords([Row|Solution], 0, List) :- findCoords(Row, 0, 0, List), solutionCoords(Solution, 1, List), !.
solutionCoords([Row|Solution], X, [Y|List]) :- X1 is X+1, findCoords(Row, X1, 0, Y), solutionCoords(Solution, X1, List).
 
findCoords([], _, _, [] ).
findCoords([0|Row], X, Y, List) :- Y1 is Y+1, findCoords(Row, X, Y1, List).
findCoords([1|Row], X, Y, [[X1,Y1]|List]) :- X1 = X, Y1 = Y,Y2 is Y+1, findCoords(Row, X, Y2, List).
findCoords([2|Row], X, Y, [[X1,Y1]|List]) :- X1 = X, Y1 = Y,Y2 is Y+1, findCoords(Row, X, Y2, List).
 
flatten([], []).
flatten([A|B],L) :- is_list(A), flatten(B,B1), !, append(A,B1,L).
flatten([A|B],[A|B1]) :- flatten(B,B1).

snakeConnected(_, []) :- !.
snakeConnected(Region, Other) :-
    select(B, Other, O2),
    neighbors(Region, B),
    snakeConnected([B|Region],O2), !.