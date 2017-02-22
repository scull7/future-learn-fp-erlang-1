-module(second).
-export([hypotenuse/2, perimeter/2]).


square(X) ->
	X*X.


hypotenuse(A,B) ->
	math:sqrt(square(A) + square(B)).


perimeter(A,B) ->
	hypotenuse(A,B) + A + B.
