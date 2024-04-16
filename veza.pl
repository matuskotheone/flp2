/** FLP 2020
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz
preklad: swipl -q -g start -o flp19-log -c input2.pl
**/


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
    write('Row: '), write(Row), nl,
    rotate_left_row(Row, RowNew),
    write('Rotated row: '), write(RowNew), nl,
    replace_at_index(Tower, Index, RowNew, R).
    
rotate_left(Tower, Index, 0, Tower).
rotate_left(Tower, Index, N, R) :- 
    N > 0,
    rotate_left_once(Tower, Index, R1),
    N1 is N - 1,
    rotate_left(R1, Index, N1, R).







start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
        print_matrix(S),
        rotate_left(S, 0, 2, R),
        print_matrix(R),
		halt.


/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).


/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
cislo(N,X) :- cislo(N,0,X).
cislo([],F,F).
cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
cislo([],F,F,_).
cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).


/** existuje knihovni predikat number_chars(?Number, ?CharList) */
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).
