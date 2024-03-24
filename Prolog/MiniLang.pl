/* Interpreter */ 
calculate(numeral(N),numeral(N)) :- integer(N).
calculate(mybool(B), mybool(B)) :- member(B, [true, false]), !.
calculate(plus(E1,E2), numeral(N)) :- 
 calculate(E1, numeral(N1)), 
 calculate(E2, numeral(N2)), 
 N is N1+N2. 
calculate(times(E1,E2), numeral(N)) :- 
 calculate(E1, numeral(N1)), 
 calculate(E2, numeral(N2)), 
 N is N1*N2.

logical_or(false, false, false) :- !.
logical_or(_, _, true) :- !.

logical_and(true, true, true) :- !.
logical_and(_, _, false) :- !.

logical_not(true, false) :- !.
logical_not(false, true) :- !.

calculate(or(E1, E2), mybool(B)) :- calculate(E1, mybool(B1)), calculate(E2, mybool(B2)), logical_or(B1, B2, B).
calculate(and(E1, E2), mybool(B)) :- calculate(E1, mybool(B1)), calculate(E2, mybool(B2)), logical_and(B1, B2, B).
calculate(not(E1), mybool(B)) :- calculate(E1, mybool(B1)), logical_not(B1, B).

logical_eq(N1, N2, true) :- N1 =:= N2, !.
logical_eq(_, _, false).

logical_gt(N1, N2, true) :- N1 > N2, !.
logical_gt(_, _, false).

calculate(eq(E1, E2), mybool(B)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), logical_eq(N1, N2, B).
calculate(gt(E1, E2), mybool(B)) :- calculate(E1, numeral(N1)), calculate(E2, numeral(N2)), logical_gt(N1, N2, B).


/* Compilers */
append([], L, L). 
append([X|R], L, [X|Z]):- append(R, L, Z).

compile(numeral(N),[ldop(N)]) :- integer(N). 
compile(plus(E1,E2),C ) :- 
 compile(E1, C1), 
 compile(E2, C2), 
 append(C1, C2, C3), 
 append(C3, [plusop], C). 
compile(times(E1,E2), C) :- 
 compile(E1, C1),
 compile(E2, C2), 
 append(C1, C2, C3), 
 append(C3, [timesop], C).

stackmc( [A|S1], [ ], A). 
stackmc( S, [ldop(N)|C], A) :- 
 stackmc([N|S], C, A). 
stackmc( [N2|[N1|S]], [plusop|C], A ) :- 
 N is N1+N2, stackmc([N|S], C, A). 
stackmc( [N2|[N1|S]], [timesop|C], A ) :- 
 N is N1*N2, stackmc([N|S], C, A).