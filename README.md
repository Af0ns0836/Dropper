# Dropper

Group: Dropper_2

Team members:
| Name                         | UP            | Contribution |
| ------------                 | ------------  |------------  |
| Afonso Gouveia Dias| [up202006721] |50%           |
| Ricardo Ribeiro Vieira     | [up202005091] |50%           |

## Installation and Execution

To execute the game you simple need to download the zip file, navigate to the src directory and consult the main.pl file inside the comand line or the UI in SICStus Prolog version 4.8(this was the version the game was made on). 
The game works both on Windows and Linux. To start the game just run:
``` play. ```

## Description of the game

Dropper was designed in Abril 2023 by Andrea Chia.

"Dropper" is an engrossing and strategic board game that combines tactical placement, clever maneuvers, and an intense quest for territorial supremacy. Players will face off on an 8x8 board, although variations of the game can be played on boards of different sizes, making it adaptable to your preferred challenge level.

### Objective:
The goal of "Dropper" is to strategically place your pieces on the board and form the most extensive groups of pieces compared to your opponent. A "group" in the game consists of a single piece or a set of stones of the same type adjacent orthogonally.

### Gameplay:
The game begins with the first player placing a single stone of their choice on the empty board, setting the stage for the strategic battle that's about to unfold. The second player, in response, deploys a pair of stones using a two-step protocol, namely "1Drop - 1Free." Here's how this protocol works:

### 1Drop:
In the first part of the protocol, the second player places one of their stones in the location of one of their opponent's stones. This move forces the opponent's stone to be moved to a free square that is adjacent either diagonally or orthogonally. This strategic maneuver can disrupt your opponent's planned formations.

### 1Free:
In the second part of the protocol, the second player places their own stone on an empty square that has no stones adjacent to it in any direction, diagonally or orthogonally. This move allows the player to assert control over the board and secure positions that are strategically advantageous.

Players continue to take turns, following the "1Drop - 1Free" protocol until it is no longer possible to make "Free" moves. Once this point is reached, players proceed with "Drop" moves only. In the "Drop" moves, players attempt to strategically place their stones in a manner that maximizes the size of their stone groups.

### Victory:
The game's outcome is determined by the size of the stone groups that each player has formed when placement possibilities are exhausted. The player with the largest group is declared the winner. In the event of a tie, the size of the second-largest group is counted, and so on until a victor is determined. Strategy and foresight are crucial in ensuring that your stone groups are the largest on the board.

### Official Game Rules: 
The oficial game rules are in the *https://andreachia.wordpress.com/2023/04/01/dropper/* website.

## Game Logic

### Internal Game State Representation
The game state is represented by a list of lists with the current board situation.

Initial State:
```
board([
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty']
]).
```
![empty_board](https://github.com/Af0ns0836/Dropper/assets/82235745/26dc38f0-d8c1-497b-930d-5c436de33d49)

Example of middle state:
```
    [['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'X', 'O', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'O', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty'],
    ['empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty', 'empty']]
```
![drop_move](https://github.com/Af0ns0836/Dropper/assets/82235745/f0fc6aba-85e3-4e3e-9f25-71ee192a629a)

Example of final state:
```
    [['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'O', 'X', 'O', 'O', 'O', 'X', 'X'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O'],
    ['X', 'X', 'X', 'O', 'O', 'O', 'X', 'O']]
```
![Captura de ecrã 2023-11-06 012722](https://github.com/Af0ns0836/Dropper/assets/114420282/ee0da053-2ff8-4e71-9d2e-87a04e81573b)


The game reach the final state because there are no more moves to make.

### Game State Visualization
Before the game, the user(s) is(are) asked to choose the game mode between PLAYER v PLAYER or PLAYER v COMPUTER

![Captura de ecrã 2023-11-06 012124](https://github.com/Af0ns0836/Dropper/assets/114420282/d3cc93e1-2717-407a-abb6-a3bcbd97db20)
![Captura de ecrã 2023-11-06 012200](https://github.com/Af0ns0836/Dropper/assets/114420282/01a6932a-545a-4256-b9b6-b0d3c0417f70)



This choice is guaranteed this way:
```Prolog
play_game :-
    board(Board),
    write('Welcome to the game of Dropper!'), nl,
    write('Player 1 is represented by X and Player 2 is represented by O.'), nl,
    write('1. PLAYER VS PLAYER'), nl,
    write('2. PLAYER VS COMPUTER'), nl,
    read(Option),
    (
        Option == 1 -> write('Player vs Player selected.'), nl;
        Option == 2 -> write('Player vs Computer selected.'), nl
    ),
    play_loop(player1, Board, Option).

play_loop(Player, Board, Option) :-
    \+ game_over(Board, _),
    (Option == 1 ->   % Player vs Player
        % display_board(Board),
        play(Player, Board, NewBoard);
        Option == 2 ->   % Player vs Computer
        (
            Player == player1 -> play(player1, Board, NewBoard);  % Humans turn
            Player == computer -> computer_play(Board, NewBoard)  % Computers turn
        )
    ),
    switch_player(Player, NextPlayer, Option),
    play_loop(NextPlayer, NewBoard, Option).
```
### Move Validation and Execution

Each move is done by inputing the row and column of the place we want to place our piece

![Captura de ecrã 2023-11-06 012829](https://github.com/Af0ns0836/Dropper/assets/114420282/95232e88-94fa-49ab-99f4-021a7250eafd)

The input is validated by:

```Prolog
% Predicate to check if a move is valid.
is_valid_move(Board, Row, Col) :-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 0, Row < NumRows,
    Col >= 0, Col < NumCols,
    get_row(Board, Row, RowList), % Get the row
    nth0(Col, RowList, 'empty'),
    \+has_adjacent_pieces(Board, Row, Col).
```

### List of Valid Moves

There isn't a predicate that lists the valid moves to the console, the valid moves are searched internally. We use this predicate:
```Prolog
valid_empty_positions(Board, ValidPositions) :-
    findall([Row, Col], (
        between(0, 7, Row), % Iterate over rows
        between(0, 7, Col), % Iterate over columns
        is_valid_move(Board, Row, Col)
    ), ValidPositions).
```
...
### End of Game
The game ends when there are no more empty spaces in the board to place a piece.It uses more predicates than these ones but these are the main ones.

```Prolog
% game_over(+Board, -Winner).
game_over(Board, Winner) :-
    % Check if there are no empty positions left
    no_empty_spaces(Board),
    check_group_size(Board, 'X', BlackGroups),
    check_group_size(Board, 'O', WhiteGroups),
    compare_winner(BlackGroups, WhiteGroups, Winner).

% compare_winner(+GroupSize1, +GroupSize2, -Winner).
% Predicate to compare group sizes and determine the winner.
compare_winner(BlackGroups, WhiteGroups, Winner) :-
    compare_lists(BlackGroups, WhiteGroups, Winner).

% Compare two lists element by element
compare_lists([], [], Winner) :-
   Winner = 'Draw'. % Both lists are equal.

% compare_list(+List1, +List2, -Winner).
compare_lists([X|Xs], [Y|Ys], Winner) :-
    (X > Y -> Winner = 'Black';  % Check if X is greater than Y
    X < Y -> Winner = 'White';  % Check if Y is greater than X
    compare_lists(Xs, Ys, Winner)).  % Recurse to compare the rest of the lists

```
### Game State Evaluation

The game state evaluation is done at the end of the game, where we find all the connected components and find the largest one. We can think of the board as a graph or tree to perform a BFS(Breadth-First Search). If there are two components from different pieces that have the same size we proceed to the next largest component. We didn't use the given predicate `value(+GameState, +Player, -Value).` because we tought it didn't fit our game structure.

### Computer Plays
The Computer plays are chosen randomly by `computer_play` that are called by `computer_move`. (this stopped working properly after drop moves were finally implemented)

## Conclusions
The work was very well distributed between the two members of the group. We both worked on the implementation of the game, and we both worked on the report. We had some problems with the implementation of the game, mainly the 1Drop move and the search for groups at the end of the game, as well as all the verifications needed from the user's input. Which culminated in having problems to implement the AI.

### Known Issues

- Missing a logical AI.
- AI stopped working well after the latest improvement to the game as it used to only have free moves before.
- Some user's input moves are not well filtered, in the case of the 1Drop move.

### Future Improvements

- Implement a better AI, that has reasons to make each play instead of just playing randomly.
- Implement a better visualization of the game, with a graphical interface.

## Bibliography

This section includes websites that were used to gather information about the project

https://boardgamegeek.com/boardgame/384171/dropper

https://andreachia.wordpress.com/2023/04/01/dropper/

https://dagazproject.github.io/go/dropper-10x10-board.htm
