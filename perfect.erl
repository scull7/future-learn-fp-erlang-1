-module(perfect).
-export([perfect/1]).
-export([all/0, perfect_6/1]).
-export([perfect_test_true/1, perfect_test_false/1]).

perfect(N) when N>5 ->
	perfect_tail(N, N div 2, 1);
perfect(_) -> false.


perfect_tail(N, C, T) when C<2 -> N == T;
perfect_tail(N, C, T) when N rem C == 0 -> 
	perfect_tail(N, C-1, C+T);
perfect_tail(N, C, T) ->
	perfect_tail(N, C-1, T).
	

%% Common Test, Test Suite
%% List of perfect numbers: https://oeis.org/A000396
all() -> [perfect_6, perfect_test_true, perfect_test_false].


perfect_test_case_true(N) ->
	case perfect(N) of
		true -> ok;
		false -> ct:fail({perfect_fail_false, N})
	end.


perfect_test_case_false(N) ->
	case perfect(N) of
		true -> ct:fail({perfect_fail_true, N});
		false -> ok
	end.


perfect_test_true(_Config) ->
	% A sufficient list in my probably terrible opinion.
	A = [6, 28, 496, 8128],
	lists:map(fun perfect_test_case_true/1, A).


perfect_test_false(_Config) ->
	A = [8, 30, 500, 8192],
	lists:map(fun perfect_test_case_false/1, A).


perfect_6(_Config) ->
	case perfect(6) of
		true -> ok;
		false -> ct:fail({perfect_6_fail, false})
	end.


