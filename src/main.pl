:- consult('board.pl').
:- consult('endgame.pl').
:- use_module(library(lists)).

% predicate that returns the piece in a certain position, first selects the row, and then the piece itself
% arguments, Board, the row and the column and the piece will be returned in the piece variable
get_piece(Board, Row, Column, Piece) :-
    element_at(Row, Board, RowList),         
    element_at(Column, RowList,Piece).


% Predicate to make a move based on the move type.
make_move(Board, Row, Col, Piece, NewBoard, 'free') :-
    write('free move'),nl,
    get_valid_move(Board, Row, Col),
    free_move(Board, Piece, [Row, Col], NewBoard).

make_move(Board, Row, Col, 'X', NewBoard, 'drop') :-
    write('drop move'), nl,
    write('Piece: X'), nl,
    write('Choose the opponents piece you want to drop on:'),nl,
    correct_drop_move(Board, Row, Col, 'X'),
    drop_move(Board, 'X', 'O', [Row, Col], NewBoard).

make_move(Board, Row, Col, 'O', NewBoard, 'drop') :-
    write('drop move'), nl,
    write('Piece: O'), nl,
    correct_drop_move(Board, Row, Col, 'O',OpponentPieceCol,OpponentPieceRow),
    drop_move(Board, 'O', 'X', [OpponentPieceRow, OpponentPieceCol], NewBoard).

correct_drop_move(Board, Row, Col, 'O',OpponentPieceCol,OpponentPieceRow) :-
    repeat,
    write('Choose the piece you want to drop on:'), nl,
    write('Row:'), read(OpponentPieceRow),
    write('Col:'), read(OpponentPieceCol),
    (
        integer(OpponentPieceRow), integer(OpponentPieceCol),
        OpponentPieceRow >= 0, OpponentPieceRow < 8,
        OpponentPieceCol >= 0, OpponentPieceCol < 8,
        get_piece(Board, OpponentPieceRow, OpponentPieceCol, Piece),
        write('Piece found: '), write(Piece), nl,
        Piece == 'X' -> write('Valid move.'), nl,
        true; % Valid move, exit the loop
        write('Invalid move. Please use valid row and column inputs.'), nl,
        fail % Retry the loop
    ).

correct_drop_move(Board, Row, Col, 'X',OpponentPieceCol,OpponentPieceRow) :-
    repeat,
    write('Choose the piece you want to drop on:'), nl,
    write('Row:'), read(OpponentPieceRow),
    write('Col:'), read(OpponentPieceCol),
    (
        integer(OpponentPieceRow), integer(OpponentPieceCol),
        OpponentPieceRow >= 0, OpponentPieceRow < 8,
        OpponentPieceCol >= 0, OpponentPieceCol < 8,
        get_piece(Board, OpponentPieceRow, OpponentPieceCol, Piece),
        write('Piece found: '), write(Piece), nl,
        Piece == 'O' -> write('Valid move.'), nl,
        true; % Valid move, exit the loop
        write('Invalid move. Please use valid row and column inputs.'), nl,
        fail % Retry the loop
    ).


% Predicate to replace an element in a matrix.
replace_in_matrix([H|T], 0, Col, X, [R|T]) :-
    replace(H, Col, X, R).
replace_in_matrix([H|T], Row, Col, X, [H|R]) :-
    Row > 0,
    Row1 is Row - 1,
    replace_in_matrix(T, Row1, Col, X, R).

% Predicate to find a free adjacent square.
find_free_adjacent(Board, Row, Col, [NewRow, NewCol]) :-
    member([DeltaRow, DeltaCol], [[-1, 0], [1, 0], [0, -1], [0, 1], [-1, -1], [-1, 1], [1, -1], [1, 1]]),
    NewRow is Row + DeltaRow,
    NewCol is Col + DeltaCol,
    is_valid_move(Board, NewRow, NewCol).


% Predicate for a drop move.
drop_move(Board, PlayerPiece, OpponentPiece, [OpponentPieceRow, OpponentPieceCol], NewBoard) :-
    write('drop_move'), nl,
    display_board(Board),
    write('Player piece '), write(PlayerPiece), write(' found at '), write(OpponentPieceRow), write(', '), write(OpponentPieceCol), nl,
    write('Opponent piece '), write(OpponentPiece), write(' found at '), write(OpponentPieceRow), write(', '), write(OpponentPieceCol), nl,
    write('Where do you want to place it? (left, right, up, down, up-right, up-left, down-left, down-right): '), nl,
    read(Direction),
    
    % Get the direction offsets based on the chosen direction.
    direction_offset(Direction, DeltaX, DeltaY),

    % Calculate the new position.
    NewCol is OpponentPieceCol + DeltaX,
    NewRow is OpponentPieceRow + DeltaY,

    write('New position: '), write(NewRow), write(', '), write(NewCol), nl,
    % Ensure the new position is a valid move.
    %is_valid_move(Board, NewRow, NewCol) -> write('Valid move.'), nl,
    % Replace the OpponentPieceRow and OpponentPieceCol with the player's piece in NewBoard.
    display_board(Board),
    replace_in_matrix(Board, OpponentPieceRow, OpponentPieceCol, PlayerPiece, NewBoard),
    replace_in_matrix(Board, NewRow, NewCol, OpponentPiece, NewBoard),
    display_board(NewBoard).
    % Replace the target position with the opponent's piece in NewBoard.
    %replace_in_matrix(Board, NewRow, NewCol, PlayerPiece, NewBoard).


 % Define the direction offsets.
direction_offset(left, DeltaX, DeltaY) :- DeltaX is -1, DeltaY is 0.
direction_offset(right, DeltaX, DeltaY) :- DeltaX is 1, DeltaY is 0.
direction_offset(up, DeltaX, DeltaY) :- DeltaX is 0, DeltaY is -1.
direction_offset(down, DeltaX, DeltaY) :- DeltaX is 0, DeltaY is 1.
direction_offset(up-right, DeltaX, DeltaY) :- DeltaX is 1, DeltaY is -1.
direction_offset(up-left, DeltaX, DeltaY) :- DeltaX is -1, DeltaY is -1.
direction_offset(down-left, DeltaX, DeltaY) :- DeltaX is -1, DeltaY is 1.
direction_offset(down-right, DeltaX, DeltaY) :- DeltaX is 1, DeltaY is 1.


% Predicate for a free move.
free_move(Board, PlayerPiece, [Row, Col], NewBoard) :-
    is_valid_move(Board, Row, Col),
    replace_in_matrix(Board, Row, Col, PlayerPiece, NewBoard).


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
    Col >= 0, Col < NumCols,
    get_row(Board, Row, RowList), % Get the row
    nth0(Col, RowList, 'empty'),
    \+has_adjacent_pieces(Board, Row, Col).

% Predicate to check if there are adjacent pieces to a given position.
has_adjacent_pieces(Board, Row, Col) :-
    adjacent_positions(Row, Col, Adjacent),
    member([X, Y], Adjacent),
    get_row(Board, X, XRow),
    nth0(Y, XRow, Piece),
    Piece \== 'empty'.

% Predicate to get positions adjacent to a given position.
adjacent_positions(Row, Col, Adjacent) :-
    RowPlus1 is Row + 1,
    RowMinus1 is Row - 1,
    ColPlus1 is Col + 1,
    ColMinus1 is Col - 1,
    Adjacent = [[RowMinus1, Col], [RowPlus1, Col], [Row, ColMinus1], [Row, ColPlus1],
                [RowMinus1, ColMinus1], [RowPlus1, ColPlus1], [RowMinus1, ColPlus1], [RowPlus1, ColMinus1]],
    include(is_valid_position, Adjacent, ValidAdjacent).

% Predicate to check if a position is valid (within the board boundaries).
is_valid_position([Row, Col]) :-
    Row >= 0, Row < 8, Col >= 0, Col < 8.


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

% Predicate to get a valid move from the player . Free move
% get_valid_move(+Board, -Row, -Col).
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
        is_valid_move(Board, Row, Col) -> write('Valid move.'), nl,
        true; % Valid move, exit the loop
        write('Invalid move. Please use valid row and column inputs.'), nl,
        fail % Retry the loop
    ).

play(Player, Board, NewBoard, MoveType, NextMoveType) :-
    display_board(Board),
    write('Move type: '), write(MoveType), nl, 
    write(Player), write('\'s turn.'), nl,
    %get_valid_move(Board, Row, Col),
    player_piece(Player, Piece),
    make_move(Board, Row, Col, Piece, NewBoard, MoveType),
    next_move_type(MoveType, NextMoveType).


% Predicate to get the next move type.
next_move_type('free', 'drop').
next_move_type('drop', 'free').

% Initalize the game.
play_game :-
    board(Board),
    write('Welcome to the game of Dropper!'), nl,
    write('Player 1 is represented by X and Player 2 is represented by O.'), nl,
    write('1.PLAYER VS PLAYER'), nl, write('2.PLAYER VS COMPUTER'), nl,
    read(Option),
    (
        Option == 1 -> write('Player vs Player selected.'), nl;
        Option == 2 -> write('Player vs Computer selected.'), nl
    ),
    play_loop(player1, Board, Option, 'free').

% Predicates to loop through the game.
play_loop(_, Board, _) :-
    game_over(Board, Winner),
    display_board(Board),
    write('Game over! Winner: '), write(Winner), nl.

play_loop(Player, Board,Option, MoveType) :-
    \+ game_over(Board, _), % Check if the game is not over
    play(Player, Board, NewBoard, MoveType, NextMoveType),
    switch_player(Player, NextPlayer, Option),
    play_loop(NextPlayer, NewBoard,_,NextMoveType).

% Predicate to switch players.
switch_player(Player, NextPlayer, Option) :-
    Option == 1 ->(
        Player == player1 -> NextPlayer = player2;
        Player == player2 -> NextPlayer = player1
    );
    Option == 2 ->(
        Player == player1 -> NextPlayer = computer;
        Player == computer -> NextPlayer = player1
    ).



% game_over(+Board, -Winner).
game_over(Board, Winner) :-
    % Check if there are no empty positions left
    \+ (member(Row, Board), member(empty, Row)),
    write('No empty positions left.'), nl,
    check_group_size(Board, 'X', BlackGroups),
    write('Black group count: '), write(BlackGroups), nl,
    check_group_size(Board, 'O', WhiteGroups),
    write('White group count: '), write(WhiteGroups), nl,
    compare_winner(BlackGroups, WhiteGroups, Winner).

