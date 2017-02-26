% Shapes
% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
%
% Choose a suitable representation of triangles, and augment area/1 and perimeter/1 to handle this case too.
%
% Define a function enclose/1 that takes a shape an returns the smallest enclosing rectangle of the shape.

-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

% perimeter/1
perimeter({rectangle, Width, Height}) ->
  2 * (Width + Height);

perimeter({triangle, A, B, C}) ->
  A + B + C;

perimeter({circle, Radius}) ->
  2 * math:pi() * Radius.


% area/1
area({rectangle, Width, Height}) ->
  Width * Height;

area(T = {triangle, A, B, C}) ->
  S = perimeter(T) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C));

area({circle, Radius}) ->
  math:pi() * Radius * Radius.


% enclose/1
enclose({rectangle, Width, Height}) ->
  {rectangle, Width, Height};

enclose(T = {triangle, A, B, C}) ->
  Area = area(T),
  Base = max(max(A, B), max(B, C)),
  Height = (Area * 2) / Base,
  {rectangle, Base, Height};

enclose({circle, Radius}) ->
{rectangle, Radius * 2, Radius * 2}.

