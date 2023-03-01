-module (pi).
-export ([pi/0]).

% ============================ %
%  Calls the main pi function. %
% ============================ %

pi() -> calc(1,3,-1).

% =================================================== %
% A - current value in brackets (1 - 1/3 + 1/5 - ...)
% B - denominator
% C - sign(+/-)
%
% Recursively calculates pi, stops when pi is 5 decimals accurate.
calc(A, _, _) when abs(4*A - 3.14159) < 0.001 -> 4*A;
calc(A,B,C) -> calc(A+(C/B), B+2, C*-1).
