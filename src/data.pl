%creation of the different boards

% board size 8x8 

board8x8([
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _]
]).

% board size 12x12
board12x12([
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _, _, _]
]).

% Define a predicate to print an 8x8 matrix.
print_matrix(Matrix) :-
    print_rows(Matrix).

% Base case: Printing an empty matrix.
print_rows([]).

% Recursive case: Print the current row and then the rest of the rows.
print_rows([Row | Rest]) :-
    print_row(Row),
    nl,  % Move to the next line
    print_rows(Rest).

% Print a single row.
print_row([]).
print_row([X | Rest]) :-
    write(X), write(' '),  % Print the element and add a space
    print_row(Rest).       % Print the rest of the row