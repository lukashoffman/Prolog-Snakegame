:- [tests].
:- use_module(library(clpfd)). % Import the module
:- set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometime

snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
, checkRowClues(RowClues,Solution)
, checkColClues(Solution,ColClues)
, countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
, checkTouching(Solution)
, solutionCoords(Solution, 0, Coords)
, flatten(Coords, [First | Rest])
, snakeConnected([First], Rest) % snake must be connected
.

%copyGrid is mostly the same as the standard, but the row copier has
%extra clauses to make copying more efficient, and to remove invalid substitutions. 

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[X|S]) :- (X = 0; X = 2), copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- Clue \= -1, copyRow(R,S).

%%% CHECK NEIGHBORS %%%

% First row
check_neighbors_rows([],[B1,B2],[_,C2]) :-  %End of the row case
	check_neighbors_pattern(B2,0,0,C2,B1).
check_neighbors_rows([],[W,M,E|RowB],[_,S,C3|RowC]) :- %Rest of the row case
	check_neighbors_pattern(M,0,E,S,W),
	check_neighbor_rows([],[M,E|RowB],[S,C3|RowC]).

%Last row
check_neighbors_rows([_,A2],[B1,B2],[]) :- %End of the row case
	check_neighbors_pattern(B2,A2,0,0,B1).

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[]) :- %Rest of the row case
	check_neighbors_pattern(M,N,E,0,W),
	check_neighbors_rows([N,A3|RowA],[M,E|RowB],[]).

% %Regular Case
check_neighbors_rows([_,A2],[B1,B2],[_,C2]) :- %End of the row case
	check_neighbors_pattern(B2,A2,0,C2,B1).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :- %Rest of the row case
	check_neighbors_pattern(M,N,E,S,W),
	check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

%Standard check_neighbors_pattern with some meta-programming for efficiency.
:- dynamic check_neighbors_pattern/5.
check_neighbors_pattern(0,_,_,_,_) :- !.
check_neighbors_pattern(Piece,N,E,S,W) :- !, 1 #=< Piece,
	count_cell(N,X1),
	count_cell(E,X2),
	count_cell(S,X3),
	count_cell(W,X4),
	Piece #= X1+X2+X3+X4, asserta(check_neighbors_pattern(Piece,N,E,S,W) :- !).


% A few specific cases for speed, and a general one for exceptional cases.
count_cell(0,0) :- !.
count_cell(1,1) :- !.
count_cell(2,1) :- !.
count_cell(_,1).

countNeighbors([]).
%For checking the first row, we pass it in as the middle, with an empty list on top.
countNeighbors([RowA, RowB | Sol]) :- check_neighbors_rows([],[0 | RowA], [0 | RowB]), countNeighbors([RowA, RowB |Sol], notfirst).
%If there are only two rows and we have checked the first, we pass in an empty list for the last row.
countNeighbors([RowA, RowB], notfirst) :- check_neighbors_rows([0 | RowA], [0 | RowB], []).
%Pass in 3 rows for the normal case.
countNeighbors([RowA, RowB, RowC | Sol], notfirst) :- check_neighbors_rows([0 | RowA],[0 | RowB], [0 | RowC]), countNeighbors([RowB, RowC | Sol], notfirst).

%Standard check row clues. Check col clues transposes the grid, then calls check row clues.

checkRowClues([-1|T], [_|GridRemainder]) :- checkRowClues(T, GridRemainder).
checkRowClues([H|T], [R|GridRemainder]) :- H >= 0, checkSum(H,R), checkRowClues(T, GridRemainder).
checkRowClues([], []).

countRow([], Clue, Clue) :- !.
countRow([Cell|Row], Clue, X) :- count_cell(Cell, X1), Y is X + X1, countRow(Row, Clue, Y).

checkSum(Clue, Row) :- renumberAll(Row, Renumbered), sum_list(Renumbered, Clue).
renumberAll([0|L],[0|R]) :- renumberAll(L,R).
renumberAll([1|L],[1|R]) :- renumberAll(L,R).
renumberAll([2|L],[1|R]) :- renumberAll(L,R).
renumberAll([],[]).

checkColClues([], []) :- !.
checkColClues(Solution, ColClues) :- transpose(Solution, X), checkRowClues(ColClues, X).

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


%%% CONNECTEDNESS %%%
% We found the connectedness suggestions in the assignment to be relatively difficult
% to implement, so we devised a different solution that is still fairly runtime efficient.
% We put the coordinates of each point in our grid into a list, then using the first in the list,
% attempt to select remaining points to build a "region," and if there are still leftover 
% elements that cannot be put into the region, we know there is a disconnect.
% In our tests, this takes roughly 0.001 seconds to evaluate a correct solution,
% and in finding a disconnected cycle, somewhere around 0.004 seconds.

% This function finds coordinates of each element in our grid.
solutionCoords([], _, []).
solutionCoords([Row|Solution], X, [Y|List]) :- X1 is X+1, findCoords(Row, X1, 0, Y), solutionCoords(Solution, X1, List).

%Helper function to assemble the list of coordinates
findCoords([], _, _, [] ).
findCoords([0|Row], X, Y, List) :- Y1 is Y+1, findCoords(Row, X, Y1, List).
findCoords([1|Row], X, Y, [[X1,Y1]|List]) :- X1 = X, Y1 = Y,Y2 is Y+1, findCoords(Row, X, Y2, List).
findCoords([2|Row], X, Y, [[X1,Y1]|List]) :- X1 = X, Y1 = Y,Y2 is Y+1, findCoords(Row, X, Y2, List).

%Flatten takes list for each row and returns a single list of only coordinates,
%which are still stored as lists of two numbers.
flatten([], []).
flatten([A|B],L) :- is_list(A), flatten(B,B1), !, append(A,B1,L).
flatten([A|B],[A|B1]) :- flatten(B,B1).

%This sees if a coordinate neighbors any in a list, returning true if so.
neighbors([[X1,Y1]|_], [X2,Y2]) :- (abs(X2-X1) + abs(Y2-Y1)) #= 1, !.
neighbors([_|List],Coords) :- neighbors(List,Coords), !.

% This function attempts to remove elements from other and add them to region
% if they neighbor each other. If all elements have been removed from other, it returns true.
snakeConnected(_, []) :- !.
snakeConnected(Region, Other) :-
    select(B, Other, O2),
    neighbors(Region, B),
    snakeConnected([B|Region],O2), !.