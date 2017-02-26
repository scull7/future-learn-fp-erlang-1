% Summing the bits
%
% Define a function bits/1 that takes a positive integer N and returns the sum of the bits in the binary representation. For example bits(7) is 3 and bits(8) is 1.

-module(bits).
-export([bits/1]).

bits(N) when N>0 ->
  bits(list_to_binary(integer_to_list(N, 2)), 0).

bits(<<>>, Acc) ->
  Acc;

bits(<<Bit:1/binary, Rest/bitstring>>, Acc) when Bit == <<"1">> ->
  bits(Rest, Acc + 1);

bits(<<Bit:1/binary, Rest/bitstring>>, Acc) when Bit == <<"0">> ->
bits(Rest, Acc).
