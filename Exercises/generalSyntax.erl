
-module(exercises).
-export([reverseList/1, find/2, delete/2, flatten/1, square1/1, square2/1, square3/1, filter/2]).

reverseList([]) -> [];
reverseList([H|T]) -> reverseList(T) ++ [H].

% My solution to the "find" exercise.
find(_, []) -> 'not_found';
find(VAL, [H|T]) -> if H =:= VAL -> {'found',VAL};
                      true -> find(VAL,T)
                    end.

% Teachers solution to the "find" exercise.
%find(_, []) -> not_found;
%find(X, [X|_]) -> {found, X};
%find(X, [_|Ys]) -> find(X, Ys).

% My solution to the "delete" exercise.
delete(_, []) -> [];
delete(X, [X|T]) -> T;
delete(X, [H|T]) -> [H] ++ delete(X, T).

% Teachers solution to the "delete" exercise.
% delete(_, []) -> [];
% delete(X, [X|Ys]) -> Ys;
% delete(X, [Y|Ys]) -> [Y|delete(X, Ys)].

% My solution to the "flatten" exercise.
flatten([]) -> [];
flatten([H|T]) -> H ++ flatten(T).

% Teachers solution to the "flatten" exercise.
% flatten([]) -> [];
% flatten([Xs|Ys]) -> Xs ++ flatten(Ys).

% My solution to the "square" exercise using tail recursion
square1([]) -> [];
square1([H|T]) -> [H*H] ++ square1(T).

% My solution to the "square" exercise using list comprehension
% Apparently pattern matching for [] is not needed
square2(N) -> [P*P || P <- N].

% My solution to the "square" exercise using map
% Apparently pattern matching for [] is not needed
square3(N) -> lists:map(fun(X) -> X * X end,N).

% My solution to the "filter" exercise
filter(_,[]) -> [];
filter(F,[H|T]) -> Result = F(H),                     % Note that you apparently can't use the Result directly in the if-case
                   if Result -> [H] ++ filter(F, T);
                      true -> filter(F, T)
                   end.
