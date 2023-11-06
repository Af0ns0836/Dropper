% predicate to check the size of the group of pieces of a player
check_group_size(Board, Player, SortedDescendingSizes) :-
    findall([X, Y], (member(X, [0,1,2,3,4,5,6,7]), member(Y, [0,1,2,3,4,5,6,7]), get_piece(Board, X, Y, Player)), List),
    largest_connected_component(List, SortedDescendingSizes).

element_at(0, [Elem|_], Elem).
element_at(N, [_|Rest], Elem) :-
    N > 0,
    N1 is N - 1,
    element_at(N1, Rest, Elem).

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
    compare_lists(BlackGroups, WhiteGroups, Winner).


% Compare two lists element by element
compare_lists([], [], Winner) :-
   Winner = 'Draw'. % Both lists are equal.

% compare_list(+List1, +List2, -Winner).
compare_lists([X|Xs], [Y|Ys], Winner) :-
    (X > Y -> Winner = 'Black';  % Check if X is greater than Y
    X < Y -> Winner = 'White';  % Check if Y is greater than X
    compare_lists(Xs, Ys, Winner)).  % Recurse to compare the rest of the lists