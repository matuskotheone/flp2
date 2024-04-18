% Predicate to rotate rings of the tower
rotate([], []).
rotate([Ring|T], [RotatedRing|RotatedT]) :-
    rotate_ring(Ring, RotatedRing),
    rotate(T, RotatedT).

% Predicate to rotate a single ring
rotate_ring([Last, SecondLast|Rest], [SecondLast, Last|RotatedRest]) :-
    rotate_ring([SecondLast|Rest], RotatedRest).
rotate_ring([Last], [Last]).
rotate_ring([], []).

% Predicate to shift balls vertically between rings
shift_vertical([Ring|T], [RotatedRing|T]) :-
    rotate_ring(Ring, RotatedRing).

% Predicate to shift balls horizontally within a ring
shift_horizontal([Ring1, Ring2|T], [RotatedRing1, RotatedRing2|T]) :-
    select(LastBall, Ring1, RestRing1),
    select(FirstBall, Ring2, RestRing2),
    append(RestRing1, [FirstBall], TempRing1),
    append(RestRing2, [LastBall], TempRing2),
    RotatedRing1 = TempRing1,
    RotatedRing2 = TempRing2.

% Base case: no more moves to make
solve_tower(Tower, Tower) :-
    format("~n~nTower Solved:~n"),
    print_tower(Tower).

% Recursive case: apply moves until tower is solved
solve_tower(Tower, SolvedTower) :-
    format("~n~nTower State:~n"),
    print_tower(Tower),
    move(Tower, MovedTower),
    solve_tower(MovedTower, SolvedTower).

% Predicate to perform a move on the tower
move(Tower, MovedTower) :-
    (   shift_horizontal(Tower, MovedTower)
    ;   shift_vertical(Tower, MovedTower)
    ;   rotate(Tower, MovedTower)
    ).

% Predicate to print the tower
print_tower([]).
print_tower([Ring|T]) :-
    print_ring(Ring),
    print_tower(T).

% Predicate to print a single ring
print_ring([]) :- nl.
print_ring([Ball|Rest]) :-
    write(Ball),
    write(' '),
    print_ring(Rest).

% Example input
tower([[a1, b1, c1, d1, e1],
       [a2, b2, c2, d2, e2],
       [b3, c3, d3, e3, e4],
       [a3, b4, c4, d4, '**']]).

solve :-
    tower(Tower),
    solve_tower(Tower, _).
