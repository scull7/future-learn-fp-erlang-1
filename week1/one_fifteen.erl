-module(one_fifteen).
-export([xor1/2,xor2/2,xor3/2,maxThree/3,howManyEqual/3]).
-export([all/0,xor2_true_false/1,xor2_false_false/1]).

xor1(X,Y) ->
	X =/= Y.

xor2(X,Y) ->
	(X or Y) and (not (X and Y)).

xor3(X,Y) ->
	not X == Y.


maxThree(X,X,X) ->
	X;
maxThree(X,X,Y) ->
	max(X,Y);
maxThree(X,Y,Y) ->
	max(X,Y);
maxThree(X,Y,Z) ->
	max(max(X,Y),Z).


howManyEqual(X,X,X) ->
	3;
howManyEqual(X,X,_) ->
	2;
howManyEqual(_,Y,Y) ->
	2;
howManyEqual(X,_,X) ->
	2;
howManyEqual(_,_,_) ->
	0.


%% Common Test, Test Suite

all() -> [xor2_true_false,xor2_false_false].


xor2_true_false(_Config) ->
	case xor2(true,false) of
		true -> ok;
		X -> ct:fail({xor2_true_false_failed, X})
	end.


xor2_false_false(_Config) ->
	case xor2(false,false) of
		false -> ok;
		X -> ct:fail({xor2_false_false_failed, X})
	end.

