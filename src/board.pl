% Define the game board as an 8x8 grid.
board([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

% Define the possible player pieces.
player_piece(player1, 'X').
player_piece(player2, 'O').

columns([0, 1, 2, 3, 4, 5, 6, 7]).

% Predicate to display the 8x8 board.
display_board(Board) :-
    display_board_column_labels,
    display_board_rows(Board, 0).

display_board_rows([Row], N) :-
    write(N), write(' '),
    display_row(Row),
    nl,
    write('   ------------------------------------------------\n').
display_board_rows([Row | Rest], N) :-
    write(N), write(' '),
    display_row(Row),
    nl,
    write('   ------------------------------------------------\n'),
    NextN is N + 1,
    display_board_rows(Rest, NextN).

display_row([]).
display_row([Cell | Rest]) :-
    write(' | '), display_cell(Cell),
    display_row(Rest).

display_cell(empty) :- write('   ').
display_cell(Cell) :- write(' '), write(Cell), write(' ').

display_columns(Cols) :-
    write('      '),     % Initial spacing
    display_columns_with_spacing(Cols).

display_columns_with_spacing([]).
display_columns_with_spacing([Col | Rest]) :-
    write(Col),       % Display the column label
    write('     '),      % Add two spaces for spacing
    display_columns_with_spacing(Rest).

% predicate to display the column labels.
display_column_labels :-
    columns(Cols),
    write('     '),
    display_columns(Cols),
    nl.