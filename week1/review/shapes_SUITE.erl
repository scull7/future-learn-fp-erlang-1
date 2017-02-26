-module(shapes_SUITE).
-export([all/0,test_perimeter/1,test_area/1,test_enclose/1]).


all() -> [test_perimeter,test_area,test_enclose].


run(_, []) -> ok;
run(Fn, [{Input, Expected}|Rest]) ->
	case Fn(Input) == Expected of
		false -> ct:fail({ Fn, Input, Expected, Fn(Input) });
		_ -> run(Fn, Rest)
  end.


test_perimeter(_Config) -> run(fun shapes:perimeter/1, [
	{ { rectangle, 2, 2 }, 8 },
	{ { triangle, 3, 4, 5 }, 12 },
	{ { circle, 1 }, 6.283185307179586 }
]).
	

test_area(_Config) -> run(fun shapes:area/1, [
	{ { rectangle, 2, 2 }, 4 },
	{ { triangle, 3, 4, 5 }, 6 },
	{ { circle, 1 }, 3.141592653589793 }
]).

%% It doesn't seem to work for my 3-4-5 triangle.
%% I believe this is because the height is incorrectly
%% calculated to be 2.4 instead of 3.
test_enclose(_Config) -> run(fun shapes:enclose/1, [
	{ { rectangle, 2, 2 }, { rectangle, 2, 2 } },
	{ { circle, 1 }, { rectangle, 2, 2 } },
	{ { triangle, 3, 4, 5 }, { rectangle, 3, 4 } }
]).
