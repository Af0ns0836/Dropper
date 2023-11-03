:- consult('board.pl').

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

% count_group_size(Board, [], Row, Col, Player, Piece, Positions, GroupSize).

% Predicate to count the size of a group of a given player starting from a given position (Row, Col).
count_group_size(_, _, _, _, _, _, [], GroupSize).
count_group_size(Board, Visited, Row, Col, Player, Piece, [(NewRow, NewCol) | Rest], GroupSize) :-
    write('Checking position2 ('), write(NewRow), write(', '), write(NewCol), write(')...'), nl,
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

% Predicates to find adjacent positions.
% mesma coluna, linha acima
find_adjacent_positions(Row, Col, [(NewRow, NewCol) | Rest]) :-
    write('Finding adjacent positions for position1 ('), write(Row), write(', '), write(Col), write(')...'), nl,
    NewRow1 is Row - 1,
    NewCol1 is Col,
    NewRow1 >= 0,
    find_adjacent_positions2(NewRow1, NewCol1, Rest1),
    append([(NewRow1, NewCol1)], Rest1, Rest).
% mesma coluna, linha abaixo
find_adjacent_positions2(Row, Col, [(NewRow, NewCol) | Rest]) :-
    write('Finding adjacent positions for position2 ('), write(Row), write(', '), write(Col), write(')...'), nl,
    NewRow2 is Row + 1,
    NewCol2 is Col,
    NewRow2 < 8,
    find_adjacent_positions3(NewRow2, NewCol2, Rest2),
    append([(NewRow2, NewCol2)], Rest2, Rest).
% mesma linha, coluna a esquerda
find_adjacent_positions3(Row, Col, [(NewRow, NewCol) | Rest]) :-
    write('Finding adjacent positions for position3 ('), write(Row), write(', '), write(Col), write(')...'), nl,
    NewRow3 is Row,
    NewCol3 is Col - 1,
    NewCol3 >= 0,
    find_adjacent_positions4(NewRow3, NewCol3, Rest3),
    append([(NewRow3, NewCol3)], Rest3, Rest).
% mesma linha, coluna a direita
find_adjacent_positions4(Row, Col, [(NewRow, NewCol) | Rest]) :-
    write('Finding adjacent positions for position4 ('), write(Row), write(', '), write(Col), write(')...'), nl,
    NewRow4 is Row,
    NewCol4 is Col + 1,
    NewCol4 < 8,
    find_adjacent_positions(NewRow4, NewCol4, Rest4),
    append([(NewRow4, NewCol4)], Rest4, Rest).
%caso base
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


game_over(Board, Winner) :-
    write('Checking if game is over...'), nl,
    % Check if there are no empty positions left
    \+ (member(Row, Board), member(empty, Row)),
    % Calculate group sizes for both players
    group_size(Board, player1, 0, 0, GroupSize1),
    group_size(Board, player2, 0, 0, GroupSize2),
    write('Group sizes for player1: '), write(GroupSize1), nl,
    write('Group sizes for player2: '), write(GroupSize2), nl,
    compare_winner(GroupSize1, GroupSize2, Winner).


% Predicate to find the group size for a player.
group_size(Board, Player, Row, Col, GroupSize) :-
    write('Checking group size for player '), write(Player), write(' at position ('), write(Row), write(', '), write(Col), write(')...'), nl,
    player_piece(Player, Piece),
    find_adjacent_positions(Row, Col, Positions),
    count_group_size(Board, [], Row, Col, Player, Piece, Positions, GroupSize).


% compare_winner(+GroupSize1, +GroupSize2, -Winner).
% Predicate to compare group sizes and determine the winner.
compare_winner(GroupSize1, GroupSize2, Winner) :-
    write('Comparing group sizes...'), nl,
    (GroupSize1 > GroupSize2 -> Winner = player1;
    GroupSize2 > GroupSize1 -> Winner = player2;
    GroupSize1 =:= GroupSize2 -> Winner = tie).

