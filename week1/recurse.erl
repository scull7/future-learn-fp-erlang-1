-module(recurse).
-export([fib/1, fib_list/1, pieces/1]).
-export([all/0,check_fib_list/1, check_pieces_3/1, check_pieces_6/1]).


fib(0) -> 0;
fib(1) -> 1;
fib(2) -> 1;
fib(N) when N>2 ->
	fib(N-1)+fib(N-2).

%% Generate a list of the fibonacci numbers using the "extended sequence"
%% starting at 0.
%% @see https://en.wikipedia.org/wiki/Fibonacci_number
fib_list(0) -> [0];
fib_list(1) -> [0, 1];
fib_list(N) when N>1 -> fib_list_tail(N, 1, 1, [0]).


%% Private tail recursive function so we don't explode the stack.
fib_list_tail(0, _, _, FibList) -> lists:reverse(FibList);
fib_list_tail(N, Current, Next, FibList) ->
	fib_list_tail(N-1, Next, Current + Next, [Current|FibList]).


pieces(N) when N>0 -> pieces_tail(N, 1).

pieces_tail(0, T) -> T;
pieces_tail(N, T) ->
	pieces_tail(N-1, N+T).


%% Erlang Common Test, Test Suite
%% Use `ct_run -suite recurse` from your OS command line or;
%% `ct:run_test([{suite, "./recurse"}]).` from the Erlang shell.
all() -> [check_fib_list, check_pieces_3, check_pieces_6].

check_fib_list(_Config) ->
	case fib_list(4) of
		[0,1,1,2,3] -> ok;
		X -> ct:fail({fib_list_4_failed,X})
	end.

check_pieces_3(_Config) ->
	case pieces(3) of
		7 -> ok;
		N -> ct:fail({check_pieces_3_failed,N})
	end.

check_pieces_6(_Config) ->
	case pieces(6) of
		22 -> ok;
		N -> ct:fail({check_pieces_6_failed,N})
	end.
