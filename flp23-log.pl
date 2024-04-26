/**
 * Author: Matus Gazdik (xgazdi04)
 * Implementation of the puzzle game Babylon Tower in Prolog using iterative deepening search
*/


/** Reads Lines from standard input */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** Checks if character is end of line or end of file */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


/** Reads lines from standard input */
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** splits the line into list of characters */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% splits into lines
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


% prints the tower
print_matrix([]).
print_matrix([Row|Rows]) :-
    print_row(Row),
    nl,
    print_matrix(Rows).

% prints the solution
print_solution([Matrix]) :- print_matrix(Matrix).
print_solution([Matrix|Matrixes]) :-
    print_matrix(Matrix),
    nl,
    print_solution(Matrixes).


print_touple([X,Y]) :- write(X), write(Y).

% Define the predicate to print a single row.
print_row([]).
print_row([Element|Elements]) :-
    print_touple(Element),
    write(' '),
    print_row(Elements).



% predicate to replace element at given index
replace_at_index([_|T], 0, X, [X|T]).
replace_at_index([H|T], Index, X, [H|R]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_at_index(T, Index1, X, R).


% predicate to rotate given list by 1 to the left
rotate_left_row([First|Rest], Rotated) :-
    append(Rest, [First], Rotated).

% predicate to rotate given row by 1 to the left
rotate_left_once(Tower, Index, R) :- 
    nth0(Index, Tower, Row),
    rotate_left_row(Row, RowNew),
    replace_at_index(Tower, Index, RowNew, R).
    

% predicate to rotate given list by 1 to the right
rotate_right_row(List, R):- rotate_right_row(List, R1, H), R = [H|R1].
rotate_right_row([H], [], H).
rotate_right_row([H|T], L, R) :- rotate_right_row(T, T1, R), L = [H|T1]. 

% predicate to rotate given row by 1 to the right
rotate_right_once(Tower, Index, R) :- 
    nth0(Index, Tower, Row),
    rotate_right_row(Row, RowNew),
    replace_at_index(Tower, Index, RowNew, R).

% predicate to move the free space up by 1
move_up(Tower, NewNewTower) :-
    get_empty_position(Tower, Row, Col),
    Row > 0, % Make sure there's a row above
    RowAbove is Row - 1,
    nth0(RowAbove, Tower, RowAboveList),
    nth0(Col, RowAboveList, Element),
    replace_at_index(RowAboveList, Col, [*,*], NewRowAboveList),
    replace_at_index(Tower, RowAbove, NewRowAboveList, NewTower),
    nth0(Row, Tower, RowList),
    replace_at_index(RowList, Col, Element, NewRowList),
    replace_at_index(NewTower, Row, NewRowList, NewNewTower).

% predicate to move the free space down by 1
move_down(Tower,NewNewTower) :- 
    get_empty_position(Tower, Row, Col),
    length(Tower, Rows),
    Row < Rows, % Make sure there's a row below
    RowBelow is Row + 1,
    nth0(RowBelow, Tower, RowBelowList),
    nth0(Col, RowBelowList, Element),
    replace_at_index(RowBelowList, Col, [*,*], NewRowBelowList),
    replace_at_index(Tower, RowBelow, NewRowBelowList, NewTower),
    nth0(Row, Tower, RowList),
    replace_at_index(RowList, Col, Element, NewRowList),
    replace_at_index(NewTower, Row, NewRowList, NewNewTower).

% move does all the possible moves with the tower
% moves all the possible rotations of the tower
% moves the free space up and down if possible
move(Tower, NewTower) :- 
    height(Rows),
    between(0, Rows, R),
    rotate_left_once(Tower, R, NewTower).

move(Tower, NewTower) :- 
    height(Rows),
    between(0, Rows, R),
    rotate_right_once(Tower, R, NewTower).

move(Tower, NewTower) :- 
    move_up(Tower, NewTower).

move(Tower, NewTower) :- 
    move_down(Tower, NewTower).

% get dimensions of the tower
get_dimensions(Tower, Rows, Columns) :- 
    length(Tower, Rows),
    nth0(0, Tower, Row),
    length(Row, Columns).

% generate the final matrix
generate_matrix(ColSize, RowSize, NewMatrix) :-
    generate_matrix(RowSize, ColSize, Matrix, 'A', 1),
    IndexRow is RowSize-1,
    nth0(IndexRow,Matrix,Row), 
    replace_at_index(Row,0, ['*','*'],NEWROW), 
    replace_at_index(Matrix,IndexRow,NEWROW,NewMatrix).
generate_matrix(0, _, [], _, _) :- !.
generate_matrix(RowSize, ColSize, [Row|Rest], Char, Num) :-
    RowSize > 0,
    generate_row(ColSize, Row, Char, Num),
    NextNum is Num + 1,
    NewRowSize is RowSize - 1,
    generate_matrix(NewRowSize, ColSize, Rest, Char, NextNum).

% generate a row
generate_row(0, [], _, _) :- !.
generate_row(ColSize, [[Char,NumChar]|Rest], Char, Num) :-
    ColSize > 0,
    atom_number(NumChar, Num),
    char_increment(Char, NextChar),
    NewColSize is ColSize - 1,
    generate_row(NewColSize, Rest, NextChar, Num).

% increment a character
char_increment('Z', 'A') :- !.
char_increment(Char, NextChar) :-
    char_code(Char, CharCode),
    NextCharCode is CharCode + 1,
    char_code(NextChar, NextCharCode).

% get the position of the empty space
get_empty_position(Tower, Row, Col) :- 
    nth0(Row, Tower, RowList),
    nth0(Col, RowList, ['*','*']).

% compare two towers
compare_towers(Tower1, Tower2) :- Tower1 == Tower2.

% check if the tower is final
is_final(Tower) :-
    final(Final),
    compare_towers(Tower, Final).




% gets tower, number of moves, history, and list of moves and returns the list of moves when the tower is final
% uses depth first search with given maximum depth
get_moves(Tower, 0, _, [Tower]) :- is_final(Tower).
get_moves(_, 0, _, _) :- !, fail.
get_moves(Tower, N, History, [Tower|Moves]) :-
    N > 0,
    N1 is N - 1,
    move(Tower, NewTower),
    not(member(NewTower, History)),
    get_moves(NewTower, N1, [Tower|History], Moves).


% solve the tower
% using iterative deepening up to 30 moves
solve_ids(Tower, Moves) :-
    between(0, 30, N),
    get_moves(Tower, N, [], Moves).


% main predicate
start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
        get_dimensions(S, Rows, Cols),!, % get the dimensions of the tower
        generate_matrix(Cols, Rows, Final), % generate the final matrix
        assert(width(Cols)), % assert the width of the tower
        assert(height(Rows)), % assert the height of the tower
        assert(final(Final)), % assert the final matrix
        solve_ids(S, WayList), % solve the tower
        print_solution(WayList), % print the solution
		halt.
