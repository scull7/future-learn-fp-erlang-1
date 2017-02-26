-module(bits_SUITE).
-export([all/0, check_bits/1]).


all() -> [check_bits].


check_cases([]) -> ok;
check_cases([{L, C, E}|Rest]) ->
	case bits:bits(C) == E of
		false -> ct:fail({ bits_fail, L });
		_ -> check_cases(Rest)
  end.


check_bits(_Config) -> check_cases([
    { five, 5, 2 },
		{ seven, 7, 3 },
		{ eight, 8, 1 },
		{ fifty, 50, 3 },
		{ nine_thousand, 9000, 5 }
]).
