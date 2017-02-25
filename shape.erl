%% FutureLearn.com Assignment 1.24
-module(shape).
-export([area/1,bits/1,perimeter/1]).
%% Export the tests.
-export([all/0,check_distance/1]).
-export([check_bits/1,check_area/1,check_perimeter/1]).


%% I'm choosing to define a general shape as a regular polygon.
%% My representation for a regular polygon is a tuple containing:
%% - name atom
%% - a list of vertices given as a list of points which correspond to
%%   segements traced in a clockwise fashion.


%% not the best name... but it's just the square of the distance between
%% two points on a line.
segment(X, Y) -> math:pow(Y-X, 2).


%% distance between to coordinate points.
distance({ _, X1, Y1}, {_, X2, Y2}) ->
	math:sqrt( segment(X1,X2) + segment(Y1,Y2) ).


%% get a list of segment lengths from a list of vertices.
%% It's a funky signature, but since it's an internal function,
%% I think it's ok.
find_segment_lengths(P1, P2, [], L) -> [ distance(P1,P2) | L ];
find_segment_lengths(P1, P2, [P3|T], L) ->
  find_segment_lengths(P2,P3,T,[ distance(P1,P2) | L ]).


%% Given any regular polygon definition, return a list of side lengths.
segment_lengths({ _, [P1,P2|T] }) ->
  %% find the distance between the first and last, then the rest.
  find_segment_lengths(P1, P2, T, [ distance(P1, lists:last(T)) ]).


%% find the perimeter of the given regular polygon.
perimeter(P) -> lists:sum( segment_lengths(P) ).


%% support function for the public area function.
segment_areas([_], L) -> L;
segment_areas([ P1,P2|T ], L) ->
	{_,X1,Y1} = P1,
	{_,X2,Y2} = P2,
  segment_areas([P2|T], [ ( (X1+X2) * (Y2-Y1) ) | L ]).


%% Calculate the area of a given polygon.
%% We push the last element onto the head because the initial P1 value
%% should be the last vertice.
area({ _, V}) ->
	L = [ lists:last(V) | V ],
	A = segment_areas(L, []),
	abs( lists:sum(A) / 2).


%% enclose/1 umm... yeah, no.  Geometrical analysis, while working 12 hour
%% days, too much.


%% Using the algorigthm found on Wikipedia:
%% https://en.wikipedia.org/wiki/Digit_sum
sum_digits(N, B) -> sum_digits(N, B, 0).

sum_digits(0, _, S) -> S;
sum_digits(N, B, S) when N < B -> S + N;
sum_digits(N, B, S) -> sum_digits(N div B, B, S + (N rem B)).


bits(N) -> sum_digits(N, 2).


%% Erlang Common Test, Test Suite
%% Use `ct_run -suite recurse` from your OS command line or;
%% `ct:run_test([{suite, "./recurse"}]).` from the Erlang shell.
all() -> [check_distance,check_perimeter,check_area,check_bits].


point({X,Y}) -> { point, X, Y }.


to_points(V) -> lists:map(fun point/1, V).


run(Fn) -> fun ({L, V, E}) -> Fn({ L, to_points(V) }) == E end.


check_cases(_,_,[]) ->ok;
check_cases(Fn, Check, [Case|Rest]) ->
	case Check(Case) of
		false -> ct:fail({ Fn, Case });
		_ -> check_cases(Fn, Check, Rest)
  end.


check_distance(_Config) ->
	Fn = fun ({ _, P1, P2, E }) -> distance( point(P1), point(P2) ) == E end,

  check_cases(distance, Fn, [
		{ one, {-4,-8}, {-4,6}, 14.0 },
		{ two, {-4,6}, {4,6}, 8.0 },
		{ three, {4,6}, {4,-4}, 10.0 },
		{ four, {4,-4}, {8,-4}, 4.0 },
		{ five, {8,-4}, {8,-8}, 4.0 },
		{ six, {8,-8}, {-4,-8}, 12.0 }
	]).


check_perimeter(_Config) -> check_cases(perimeter, run(fun perimeter/1), [
	{ square, [ {0,0}, {0,2}, {2,2}, {2,0} ], 8.0 },
	{ triangle345, [ {0,0}, {0,4}, {3,0} ], 12.0 },
	%% http://www.mathopenref.com/coordpolygonarea2.html
	{ l_polygon, [ {-4,-8}, {-4,6}, {4,6}, {4,-4}, {8,-4}, {8,-8} ], 52.0 }
]).


check_area(_Config) -> check_cases(area, run(fun area/1), [
  { square, [ {0,0}, {0,2}, {2,2}, {2,0} ], 4.0 },
	{ triangle345, [ {0,0}, {0,4}, {3,0} ], 12.0 },
	%% http://www.mathopenref.com/coordpolygonarea2.html
	{ l_polygon, [ {-4,-8}, {-4,6}, {4,6}, {4,-4}, {8,-4}, {8,-8} ], 128.0 }
]).


check_bits(_Config) ->
  Fn = fun({ _, C, E}) -> bits(C) == E end,

	check_cases(bits, Fn, [
    { five, 5, 2 },
		{ seven, 7, 3 },
		{ eight, 8, 1 },
		{ fifty, 50, 3 },
		{ nine_thousand, 9000, 5 }
  ]).
