
-module(funs_over_list).
-export([all/0,maximum/1,product/1]).
-export([test_maximum/1,test_product/1]).

product(Xs) -> product(Xs, 1).

product([], P) -> P;
product([X|Xs], P) -> product(Xs, X*P).


maximum(Xs) -> maximum(Xs, 0).

maximum([], M) -> M;
maximum([X|Xs], M) when X > M -> maximum(Xs, X);
maximum([_|Xs], M) -> maximum(Xs, M).


%% Common Test - Test Cases

all() -> [test_product,test_maximum].


run(_, []) -> ok;
run(Fn, [{ Label, Input, Expected }|Rest]) ->
	case Fn(Input) of
		Expected -> run(Fn, Rest);
		Output -> ct:fail({ Label, Input, Expected, Output })
  end.


test_product(_Config) -> run(fun product/1, [
	{ to_five, [ 1, 2, 3, 4, 5], 120 },
	{ six_ten, [ 6, 7, 8, 9, 10], 30240 },
	{ zero, [ 55, 44, 33, 0, 22, 11], 0 }
]).


test_maximum(_Config) -> run(fun maximum/1, [
	{ to_five, [1, 2, 3, 4, 5], 5 },
	{ fifty, [1, 25, 10, 50, 49], 50 },
	{ twenty, [20, 19, 18, 17, 16], 20 }
]).
