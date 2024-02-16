/* Interpreter */ 
calculate(numeral(N),numeral(N)) :- integer(N). 
calculate(plus(E1,E2), numeral(N)) :- 
 calculate(E1, numeral(N1)), 
 calculate(E2, numeral(N2)), 
 N is N1+N2. 
calculate(times(E1,E2), numeral(N)) :- 
 calculate(E1, numeral(N1)), 
 calculate(E2, numeral(N2)), 
 N is N1*N2. 

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