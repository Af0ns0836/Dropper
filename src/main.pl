:- consult(board).

% display_game(+GameState)
% Prints the board

display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).
