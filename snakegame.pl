:- use_module(library(clpfd)). % Import the module
:- set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometime

snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
, checkRowClues(Solution,RowClues)
, checkColClues(Solution,ColClues)
, countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
%, snakeConnected(Solution) % snake must be connected
.

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[X|S]) :- (X = 0; X = 2), copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- Clue \= -1, copyRow(R,S).


copyGrid2([],[],[]).
copyGrid2([Row|G],[RowS|S],X,CoordList) :- copyRow2(Row,X,0,RowS, CoordList), 
										   X1 is X + 1, copyGrid2(G,S,X1,CoordList).

copyRow2([],[],_,[]).
copyRow2([-1|R],X,Y,[Z|S], CoordList) :- (Z = 0; Z = 2),Y1 is Y + 1, (Z = 2 -> copyRow2(R,S,X,Y1,[[X,Y]|CoordList])) ; copyRow2(R,S,X,Y1,CoordList).
copyRow2([Clue|R],X,Y,[Clue|S], CoordList) :- Clue \= -1, Y1 is Y + 1, copyRow2(R,S,X,Y1,[[X,Y]|CoordList]).

%First row
check_neighbors_rows([],[B1,B2],[_,C2]) :- check_neighbors_pattern(B2,0,0,C2,B1).
check_neighbors_rows([],[W,M,E|RowB],[_,S,C3|RowC]) :-
	check_neighbors_pattern(M,0,E,S,W),
	check_neighbors_rows([],[M,E|RowB],[S,C3|RowC]).

%Last row
check_neighbors_rows([A1,A2],[B1,B2],[]) :- 
	touchCheck(A1, A2, B1, B2),
	check_neighbors_pattern(B2,A2,0,0,B1).

check_neighbors_rows([A1,N,A3|RowA],[W,M,E|RowB],[]) :-
	touchCheck(A1, N, W, M), 
	check_neighbors_pattern(M,N,E,0,W),
	check_neighbors_rows([N,A3|RowA],[M,E|RowB],[]).

%Regular Case
check_neighbors_rows([A1,A2],[B1,B2],[_,C2]) :- 
	touchCheck(A1, A2, B1, B2), 
	check_neighbors_pattern(B2,A2,0,C2,B1).
check_neighbors_rows([A1,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
	touchCheck(A1, N, W, M),
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
count_cell(_,1).

countNeighbors([]).
countNeighbors([RowA, RowB | Sol]) :- check_neighbors_rows([],[0 | RowA], [0 | RowB]), countNeighbors([RowA, RowB |Sol], notfirst).
countNeighbors([RowA, RowB], notfirst) :- check_neighbors_rows([0 | RowA], [0 | RowB], []).
countNeighbors([RowA, RowB, RowC | Sol], notfirst) :- check_neighbors_rows([0 | RowA],[0 | RowB], [0 | RowC]), countNeighbors([RowB, RowC | Sol], notfirst).

checkRowClues([],[]).
checkRowClues([_|Solution], [-1|RowClues]) :- checkRowClues(Solution, RowClues).
checkRowClues([Row|Solution], [Clue|RowClues]) :- 
    countRow(Row, Clue, 0),
    checkRowClues(Solution, RowClues).

countRow([], Clue, Clue) :- !.
countRow([Cell|Row], Clue, X) :- count_cell(Cell, X1), Y is X + X1, countRow(Row, Clue, Y).

checkColClues([], []) :- !.
checkColClues(Solution, ColClues) :- transpose(Solution, X), checkRowClues(X, ColClues).

:- dynamic touchCheck/4. 
touchCheck(X1, X2, X3, X4) :-
	count_cell(X1,A),
	count_cell(X2,B),
	count_cell(X3,C),
	count_cell(X4,D),
	not((A=D, B=C, A+B+C+D #\= 0)),
	asserta(touchCheck(X1,X2,X3,X4) :- !). %Check to see if the opposite corners are equal.