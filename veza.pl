/** FLP 2020
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz
preklad: swipl -q -g start -o flp19-log -c input2.pl
**/


test2([[['A',1],['B',1],['C',1]],[['*','*'],['B',2],['C',2]],[['A',2],['B',3],['C',3]],[['A',3],['B',4],['C',4]]]).

tower([[['A',1],['B',1],['C',1]],[['A',2],['B',2],['C',2]],[['A',3],['B',3],['C',3]],[['*','*'],['B',4],['C',4]]]).

tower2([[['A','1'],['B','1']],[['*','*'],['B','2']]]).


/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% prints the tower
print_matrix([]).
print_matrix([Row|Rows]) :-
    print_row(Row),
    nl,
    print_matrix(Rows).

print_solution([]).
print_solution([Row|Rows]) :-
    print_matrix(Row),
    nl,
    print_solution(Rows).


print_touple([X,Y]) :- write(X), write(Y).

% Define the predicate to print a single row.
print_row([]).
print_row([Element|Elements]) :-
    print_touple(Element),
    write(' '),
    print_row(Elements).

/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).





replace_at_index([_|T], 0, X, [X|T]).
replace_at_index([H|T], Index, X, [H|R]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_at_index(T, Index1, X, R).


rotate_left_row([First|Rest], Rotated) :-
    append(Rest, [First], Rotated).

% predicate to rotate given row by 1 to the left
% gets the tower and the index of the row to rotate
rotate_left_once(Tower, Index, R) :- 
    nth0(Index, Tower, Row),
    rotate_left_row(Row, RowNew),
    replace_at_index(Tower, Index, RowNew, R).
    
rotate_left(Tower, _ , 0, Tower).
rotate_left(Tower, Index, N, R) :- 
    N > 0,
    rotate_left_once(Tower, Index, R1),
    N1 is N - 1,
    rotate_left(R1, Index, N1, R).


rotate_right_row(List, R):- rotate_right_row(List, R1, H), R = [H|R1].
rotate_right_row([H], [], H).
rotate_right_row([H|T], L, R) :- rotate_right_row(T, T1, R), L = [H|T1]. 

rotate_right_once(Tower, Index, R) :- 
    nth0(Index, Tower, Row),
    rotate_right_row(Row, RowNew),
    replace_at_index(Tower, Index, RowNew, R).

rotate_right(Tower, _, 0, Tower).
rotate_right(Tower, Index, N, R) :- 
    N > 0,
    rotate_right_once(Tower, Index, R1),
    N1 is N - 1,
    rotate_right(R1, Index, N1, R).


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

/**
move(Tower, NewTower) :- 
    height(Rows),
    width(Cols),
    Col1 is Cols-1,
    between(0, Rows, R),
    between(1, Col1, N),
    rotate_left(Tower, R, N, NewTower).
**/


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

generate_row(0, [], _, _) :- !.

generate_row(ColSize, [[Char,NumChar]|Rest], Char, Num) :-
    ColSize > 0,
    atom_number(NumChar, Num),
    char_increment(Char, NextChar),
    NewColSize is ColSize - 1,
    generate_row(NewColSize, Rest, NextChar, Num).

char_increment('Z', 'A') :- !.
char_increment(Char, NextChar) :-
    char_code(Char, CharCode),
    NextCharCode is CharCode + 1,
    char_code(NextChar, NextCharCode).

get_empty_position(Tower, Row, Col) :- 
    nth0(Row, Tower, RowList),
    nth0(Col, RowList, ['*','*']).


compare_towers(Tower1, Tower2) :- Tower1 == Tower2.

reverse_list([], []).
reverse_list([X|Xs], Reversed) :-
    reverse_list(Xs, ReversedTail),
    append(ReversedTail, [X], Reversed).

is_final(Tower) :-
    final(Final),
    compare_towers(Tower, Final).

% get has tower, number of moves, and list of moves
get_moves(X, 0, [X]).
get_moves(X, N, [X|C]) :-
    N > 0,
    N1 is N-1,
    move(X,X1),
    get_moves(X1, N1, C).


solve(Tower, Moves) :-
    solve_ids(Tower, Moves).
    %solve_bfs([[Tower]], [], Moves).

solve_bfs([[Tower|PrevMoves]|_], _, [Tower|PrevMoves]) :-
    is_final(Tower).

solve_bfs([[Tower|PrevMoves]|RestPaths], Visited, Moves) :-
    findall([NextTower,Tower|PrevMoves], (move(Tower, NextTower), \+ member(NextTower, Visited)), NextPaths),
    append(RestPaths, NextPaths, NewPaths),
    solve_bfs(NewPaths, [Tower|Visited], Moves).


solve_ids(Tower, Seq) :-
    between(0, 30, N),
    get_moves(Tower, N, Seq),
    append(_,[L], Seq),
    is_final(L).

    



/**
solve_bfs([[Tower|PrevMoves]|_], _, [Tower|PrevMoves]) :-
    is_final(Tower).

solve_bfs([[Tower|PrevMoves]|RestPaths], Visited, Moves) :-
    findall([NextTower,Tower|PrevMoves], (move(Tower, NextTower), \+ member(NextTower, Visited)), NextPaths),
    append(RestPaths, NextPaths, NewPaths),
    solve_bfs(NewPaths, [Tower|Visited], Moves).
**/

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
        get_dimensions(S, Rows, Cols),!,
        generate_matrix(Cols, Rows, Final),
        assert(width(Cols)),
        assert(height(Rows)),
        assert(final(Final)),
        solve(S, WayList),
        reverse_list(WayList, WayListReversed),
        print_solution(WayListReversed),
		halt.

