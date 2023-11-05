:- consult('board.pl').
:- use_module(library(lists)).


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
    %free_move_possible(Board, Row, Col).

free_move_possible(Board) :-
    (member(Row, Board), member(empty, Row)).

% Predicate to check if a piece is adjacent to another piece diagonally and ortoganally.
adjacent([X,Y], [X1,Y1]) :-
    (X1 is X+1, Y1 is Y);  % right
    (X1 is X-1, Y1 is Y);  % left
    (X1 is X, Y1 is Y+1);  % up
    (X1 is X, Y1 is Y-1);  % down
    (X1 is X+1, Y1 is Y-1);  % diagonal up right
    (X1 is X-1, Y1 is Y+1);  % diagonal up left
    (X1 is X+1, Y1 is Y+1);  % diagonal down right
    (X1 is X-1, Y1 is Y-1).  % diagonal down left

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

% This assumes the game alternates between player1 and player2.
play(Player, Board, NewBoard) :-
    display_board(Board),
    write(Player), write('\'s turn.'), nl,
    free_move(Board, Row, Col),
    player_piece(Player, Piece),
    make_move(Board, Row, Col, Piece, NewBoard).

% Drop move predicate
% drop_move(+Board, +).
drop_move(Board, Row, Col):-
    get_row(Board, Row, OldRow),
    replace(OldRow, Col, 'X', NewRow),
    replace_row(Board, Row, NewRow, NewBoard).

% Predicate to get a valid move from the player . Free move
% get_valid_move(+Board, -Row, -Col).
free_move(Board, Row, Col) :-
    %free_move_possible(Board),
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
    write('Welcome to the game of Dropper!'), nl,
    write('Player 1 is represented by X and Player 2 is represented by O.'), nl,
    write('1.PLAYER VS PLAYER'), nl, write('2.PLAYER VS COMPUTER'), nl,
    read(Option),
    (
        Option == 1 -> write('Player vs Player selected.'), nl;
        Option == 2 -> write('Player vs Computer selected.'), nl
    ),
    play_loop(player1, Board,Option).

% Predicates to loop through the game.
play_loop(_, Board, _) :-
    game_over(Board, Winner),
    display_board(Board),
    write('Game over! Winner: '), write(Winner), nl.

play_loop(Player, Board,Option) :-
    \+ game_over(Board, _), % Check if the game is not over
    play(Player, Board, NewBoard),
    switch_player(Player, NextPlayer, Option),
    play_loop(NextPlayer, NewBoard).

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

% predicate to check the size of the group of pieces of a player
check_group_size(Board, Player, SortedDescendingSizes) :-
    write('checking group size...'), nl,
    findall([X, Y], (member(X, [0,1,2,3,4,5,6,7]), member(Y, [0,1,2,3,4,5,6,7]), get_piece(Board, X, Y, Player)), List),
    largest_connected_component(List, SortedDescendingSizes).

element_at(0, [Elem|_], Elem).
element_at(N, [_|Rest], Elem) :-
    N > 0,
    N1 is N - 1,
    element_at(N1, Rest, Elem).

% predicate that returns the piece in a certain position, first selects the row, and then the piece itself
% arguments, Board, the row and the column and the piece will be returned in the piece variable
get_piece(Board, Row, Column, Piece) :-
    element_at(Row, Board, RowList),         
    element_at(Column, RowList,Piece).

:- dynamic visited/1.
% Predicate to find the size of the largest connected component
largest_connected_component(Points, SortedDescendingSizes) :-
    findall(Size, (
        member(Point, Points),
        \+ visited(Point),  % Ensure we dont start BFS from visited points
        bfs_from_point(Point, Points, [Point], Size),
        asserta(visited(Point))  % Mark the starting point as visited after BFS
    ), Sizes),
    sort(Sizes, SortedSizes),
    reverse(SortedSizes,SortedDescendingSizes),
    retractall(visited(_)). % Clean up visited facts after computation

% Predicate for BFS traversal from a given point
bfs_from_point(Point, Points, Visited, Size) :-
    bfs([Point], Points, Visited, 1, Size).

% Helper predicate for BFS traversal
bfs([], _, _, Size, Size). % Base case: no more points to traverse
bfs([P|RestQueue], Points, Visited, CurrentSize, Size) :-
    find_connected(P, Points, Visited, ConnectedPoints),
    length(ConnectedPoints, NumConnected),
    NewSize is CurrentSize + NumConnected,
    append(RestQueue, ConnectedPoints, NewQueue),
    append(Visited, ConnectedPoints, NewVisited),
    bfs(NewQueue, Points, NewVisited, NewSize, Size).

% Predicate to find all points connected to a given point that havent been visited
find_connected(Point, Points, Visited, ConnectedPoints) :-
    include(is_connected(Point, Visited), Points, ConnectedPoints).

% Predicate to check if a point is connected and not visited
is_connected([X,Y], Visited, [X1,Y1]) :-
    adjacent([X,Y], [X1,Y1]),
    \+ memberchk([X1,Y1], Visited).

% Predicate to check if two points are adjacent and potentially connected
adjacent([X,Y], [X1,Y1]) :-
    (X1 is X+1, Y1 is Y);  % right
    (X1 is X-1, Y1 is Y);  % left
    (X1 is X, Y1 is Y+1);  % up
    (X1 is X, Y1 is Y-1).  % down


% Remove all instances of elements in the second list from the first list
remove_elements(List, [], List).
remove_elements(List, [H|T], Result) :-
    delete(List, H, TempResult), % delete/3 is a built-in predicate
    remove_elements(TempResult,T,Result).

% compare_winner(+GroupSize1, +GroupSize2, -Winner).
% Predicate to compare group sizes and determine the winner.
compare_winner(BlackGroups, WhiteGroups, Winner) :-
    write('Comparing group sizes...'), nl,
    compare_lists(BlackGroups, WhiteGroups, Winner).


% Compare two lists element by element
compare_lists([], [], Winner) :-
   Winner = 'Draw'. % Both lists are equal.

% compare_list(+List1, +List2, -Winner).
compare_lists([X|Xs], [Y|Ys], Winner) :-
    (write('Comparing '),write(X), write('>'),write(Y),nl ,X > Y -> Winner = 'Black';  % Check if X is greater than Y
    X < Y -> Winner = 'White';  % Check if Y is greater than X
    compare_lists(Xs, Ys, Winner)).  % Recurse to compare the rest of the lists
