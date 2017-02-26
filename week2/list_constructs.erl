
-module(list_constructs).
-export([all/0,double/1,evens/1,median/1,mode/1]).
-export([test_double/1,test_evens/1,test_qsort/1,test_median/1,test_mode/1]).


double(Xs) -> double(Xs, []).

double([], Acc) -> Acc;
double([X|Xs], Acc) -> double(Xs, [ 2*X | Acc ]).


evens(Xs) -> evens(Xs, []).

evens([], Acc) -> Acc;
evens([X|Xs], Acc) when X rem 2 == 0 -> evens(Xs, [ X | Acc ]);
evens([_|Xs], Acc) -> evens(Xs, Acc).


%% I could just use `lists:sort` but that felt like cheating.
%% Feeling pretty good since my implementation is close:
%% http://erlangexamples.com/tag/quicksort/
qsort([]) -> [];
qsort([X]) -> [X]; % sorted single element list is just the list (identity).
qsort([Pivot|Rest]) ->
	{ L, G } = qsort(Pivot, Rest),
	qsort(L) ++ [Pivot] ++ qsort(G).

qsort(Pivot, Xs) -> qsort(Pivot, [], [], Xs).

qsort(_, L, G, []) -> { L, G };
qsort(Pivot, L, G, [X|Xs]) when X < Pivot -> qsort(Pivot, [X|L], G, Xs);
qsort(Pivot, L, G, [X|Xs]) when x >= Pivot -> qsort(Pivot, L, [X|G], Xs).


take(_, []) -> [];
take(N, L) when length(L) =< N -> L;
take(N, L) -> take(N, L, []).

take(0, _, Acc) -> Acc;
take(N, [X|Xs], Acc) when N > 0 -> take(N-1, Xs, [ X | Acc ]).


drop(0, L) -> L;
drop(_, []) -> [];
drop(N, L) when length(L) =< N -> [];
drop(N, [_|Xs]) -> drop(N-1, Xs).


sum(L) -> sum(L, 0).

sum([], Acc) -> Acc;
sum([X|Xs], Acc) -> sum(Xs, X + Acc).


average([X]) -> X;
average(L) -> sum(L) / length(L).


median([X]) -> X;
median(List) ->
	S = length(List),
	L = qsort(List),

	case S rem 2 == 0 of
		true -> average(take(2, drop(S div 2 - 1, L)));
		false -> hd(drop(S div 2, L))
	end.
	

takeWhile(Fn, L) -> takeWhile(Fn, L, []).

takeWhile(_, [], Acc) -> Acc;
takeWhile(Fn, [X|Xs], Acc) ->
	case Fn(X) of
		true -> takeWhile(Fn, Xs, [X | Acc]);
		false -> Acc
  end.


dropWhile(_,[]) -> [];
dropWhile(Fn, [X|Xs] = List) ->
	case Fn(X) of
		true -> dropWhile(Fn, Xs);
		false -> List
  end.


group([]) -> [];
group([X]) -> [[X]];
group(L) -> group(qsort(L), []).

group([], Acc) -> Acc;
group([X|Xs], Acc) ->
	G = [ X | takeWhile(fun (Y) -> Y == X end, Xs) ],
	R = drop(length(G)-1, Xs),
	group(R, [G | Acc]).


%% Mode
%% - can be multi-modal, N+1 elements are repeated Y times.
%% - if all elements are repeated the same number of times, then I consider
%%   the list to be a multi-modal list.
%% - if all the elements in the list are unique, then the list does not have
%%   a mode.
mode([X]) -> [X];
mode(L) ->
	case dropWhile(fun(X) -> length(X) < 2 end, group(L)) of
		[] -> [];
		[G|Gs] -> mode(Gs, length(G), [hd(G)])
  end.

mode([], _, Acc) -> Acc;
mode([G|Gs], S, Acc) ->
	N  = length(G),
	if
		N>S -> mode(Gs, N, [hd(G)]);
		N==S -> mode(Gs, S, [ hd(G) | Acc ]);
		true ->  mode(Gs, S, Acc)
  end.


% Common Test - Test Cases
all() -> [test_double,test_evens,test_qsort,test_median,test_mode].


run(_, []) -> ok;
run(Fn, [{ Label, Input, Expected }|Rest]) ->
	case Fn(Input) of
		Expected -> run(Fn, Rest);
		Output -> ct:fail({ Label, Input, Expected, Output })
  end.


test_double(_Config) -> run(fun double/1, [
	{ to_five, [ 1, 2, 3, 4, 5], [ 10, 8, 6, 4 , 2 ] },
	{ six_ten, [ 6, 7, 8, 9, 10], [ 20, 18, 16, 14, 12 ] },
	{ zero, [ 55, 44, 33, 0, 22, 11], [ 22, 44, 0, 66, 88, 110 ] },
	{ empty, [], [] }
]).


test_evens(_Config) -> run(fun evens/1, [
	{ to_five, [ 1, 2, 3, 4, 5], [ 4, 2 ] },
	{ six_ten, [ 6, 7, 8, 9, 10], [ 10, 8, 6 ] },
	{ zero, [ 55, 44, 33, 0, 22, 11], [ 22, 0, 44 ] },
	{ empty, [], [] },
	{ no_evens, [ 1, 3, 5, 7, 9, 11 ], [] }
]).


test_qsort(_Config) -> run(fun qsort/1, [
	{ to_five, [ 3, 5, 1, 2, 4 ], [ 1, 2, 3, 4, 5 ] },
	{ six_ten, [ 6, 7, 10, 9, 8 ], [ 6, 7, 8, 9, 10 ] },
	{ zero, [ 33, 11, 0, 22, 44, 55 ], [ 0, 11, 22, 33, 44, 55 ] },
	{ empty, [], [] },
	{ reversed, [ 100, 99, 98, 97, 96, 95 ], [ 95, 96, 97, 98, 99, 100 ] }
]).


test_median(_Config) -> run(fun median/1, [
	{ to_five, [ 3, 5, 1, 2, 4 ], 3 },
	{ six_ten, [ 6, 7, 10, 9, 8 ], 8 },
	{ zero, [ 33, 11, 0, 22, 44, 55 ], 27.5 },
	{ reversed, [ 100, 99, 98, 97, 96, 95 ], 97.5 }
]).


%% Examples: http://www.purplemath.com/modules/meanmode.htm
%% Wikipedia Example: https://en.wikipedia.org/wiki/Mode_(statistics) 
test_mode(_Config) -> run(fun mode/1, [
  { case1, [ 13, 18, 13, 14, 13, 16, 14, 21, 13 ], [13] },
	{ case2, [ 1, 2, 4, 7 ], [] },
	{ case3, [ 8, 9, 10, 10, 10, 11, 11, 11, 12, 13 ], [ 10, 11 ] },
	{ case4, [ 1, 2, 2, 3, 4, 7, 9 ], [2] }
]).
