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
    display_column_labels,
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

% Predicate to display the column labels.
display_columns(Cols) :-
    write(' '),     % Initial spacing
    display_columns_with_spacing(Cols).

% Predicate to display the column labels with spacing.
display_columns_with_spacing([]).
display_columns_with_spacing([Col | Rest]) :-
    write(Col),       % Display the column label
    write('     '),      % Add two spaces for spacing
    display_columns_with_spacing(Rest).

% Predicate to display the column labels.
display_column_labels :-
    columns(Cols),
    write('     '),
    display_columns(Cols),
    nl.

% Makes a move on the board.
make_move(Board, Row, Col, Piece, NewBoard) :-
    (
        is_valid_move(Board, Row, Col),
        get_row(Board, Row, OldRow),
        replace(OldRow, Col, Piece, NewRow),
        replace_row(Board, Row, NewRow, NewBoard)
    ;
        write('Invalid move. Please try again.'), nl,
        NewBoard = Board
    ).

% Predicate to replace an element in a list.
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

% Predicate to check if a move is valid.
is_valid_move(Board, Row, Col) :-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 0, Row < NumRows,
    Col >= 0, Col < NumCols.

% Custom predicate to retrieve a specific row from a list of lists.
get_row([Row | _], 0, Row).
get_row([_ | Rest], Index, Row) :-
    Index > 0,
    NextIndex is Index - 1,
    get_row(Rest, NextIndex, Row).

% Custom predicate to replace a row in a list of lists.
replace_row([_ | Rest], 0, NewRow, [NewRow | Rest]).
replace_row([Row | Rest], Index, NewRow, [Row | UpdatedRest]) :-
    Index > 0,
    NextIndex is Index - 1,
    replace_row(Rest, NextIndex, NewRow, UpdatedRest).

% Predicate to count the size of a group of a given player starting from a given position (Row, Col).
count_group_size(_, _, _, _, _, _, [], GroupSize).
count_group_size(Board, Visited, Row, Col, Player, Piece, [(NewRow, NewCol) | Rest], GroupSize) :-
    % Check if the (NewRow, NewCol) position is a valid move and belongs to the same player.
    is_valid_move(Board, NewRow, NewCol),
    get_row(Board, NewRow, NewBoardRow),
    nth0(NewCol, NewBoardRow, NewBoardCell),
    NewBoardCell == Piece,
    \+ member((NewRow, NewCol), Visited), % Check if it hasnt been visited before.
    % Mark it as visited.
    append(Visited, [(NewRow, NewCol)], NewVisited),
    % Recursively count the group size starting from (NewRow, NewCol).
    find_adjacent_positions(NewRow, NewCol, Positions),
    append(Rest, Positions, NewRest),
    NewGroupSize is GroupSize + 1,
    count_group_size(Board, NewVisited, NewRow, NewCol, Player, Piece, NewRest, NewGroupSize).
count_group_size(Board, Visited, Row, Col, Player, Piece, [(NewRow, NewCol) | Rest], GroupSize) :-
    % If its not a valid move or doesnt belong to the player, skip it.
    find_adjacent_positions(Row, Col, Positions),
    append(Rest, Positions, NewRest),
    count_group_size(Board, Visited, Row, Col, Player, Piece, NewRest, GroupSize).

% Predicate to find the group size for a player.
group_size(Board, Player, Row, Col, GroupSize) :-
    player_piece(Player, Piece),
    find_adjacent_positions(Row, Col, Positions),
    count_group_size(Board, [], Row, Col, Player, Piece, Positions, 1, GroupSize).

% Predicates to find adjacent positions.
find_adjacent_positions(Row, Col, [(NewRow, NewCol) | Rest]) :-
    NewRow is Row - 1,
    NewCol is Col,
    NewRow >= 0,
    find_adjacent_positions(Row, Col, Rest).
find_adjacent_positions(Row, Col, [(NewRow, NewCol) | Rest]) :-
    NewRow is Row + 1,
    NewCol is Col,
    NewRow < 8,
    find_adjacent_positions(Row, Col, Rest).
find_adjacent_positions(Row, Col, [(NewRow, NewCol) | Rest]) :-
    NewRow is Row,
    NewCol is Col - 1,
    NewCol >= 0,
    find_adjacent_positions(Row, Col, Rest).
find_adjacent_positions(Row, Col, [(NewRow, NewCol) | Rest]) :-
    NewRow is Row,
    NewCol is Col + 1,
    NewCol < 8,
    find_adjacent_positions(Row, Col, Rest).
find_adjacent_positions(_, _, []).


% This assumes the game alternates between player1 and player2.
play(Player, Board, NewBoard) :-
    display_board(Board),
    write(Player), write('\'s turn.'), nl,
    get_valid_move(Board, Row, Col),
    player_piece(Player, Piece),
    make_move(Board, Row, Col, Piece, NewBoard).

% Predicate to get a valid move from the player
get_valid_move(Board, Row, Col) :-
    repeat,
    write('Enter the row (0-7): '),
    read(RowTerm),
    write('Enter the column (0-7): '),
    read(ColTerm),
    (
        integer(RowTerm), integer(ColTerm),
        RowTerm >= 0, RowTerm < 8,
        ColTerm >= 0, ColTerm < 8,
        Row is RowTerm,
        Col is ColTerm,
        is_valid_move(Board, Row, Col) ->
        true; % Valid move, exit the loop
        write('Invalid move. Please use valid row and column inputs.'), nl,
        fail % Retry the loop
    ).

% Initalize the game.
play_game :-
    board(Board),
    play_loop(player1, Board).

% Predicates to loop through the game.
play_loop(_, Board) :-
    game_over(Board, Winner),
    display_board(Board),
    write('Game over! Winner: '), write(Winner), nl.

play_loop(Player, Board) :-
    \+ game_over(Board, _), % Check if the game is not over
    play(Player, Board, NewBoard),
    switch_player(Player, NextPlayer),
    play_loop(NextPlayer, NewBoard).

% Predicate to switch players.
switch_player(Player, NextPlayer) :-
    Player == player1 -> NextPlayer = player2;
    Player == player2 -> NextPlayer = player1.


% Predicate to determine the winner.
game_over(Board, Winner) :-
    \+ (member(Row, Board), member(empty, Row)),
    % If no empty positions are left, determine the winner.
    group_size(Board, player1, 0, 0, GroupSize1),
    group_size(Board, player2, 0, 0, GroupSize2),
    compare_winner(GroupSize1, GroupSize2, Winner).

% Predicate to compare group sizes and determine the winner.
compare_winner(GroupSize1, GroupSize2, Winner) :-
    (GroupSize1 > GroupSize2 -> Winner = player1;
    GroupSize2 > GroupSize1 -> Winner = player2).

