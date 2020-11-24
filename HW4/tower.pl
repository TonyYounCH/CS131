revail2([], M, M).
revtail2([X|L], M, RXM) :- revtail2(L, [X|M], RXM).
reverse2(L, R) :- revtail2(L, [], R).


% % % % % % % % % %
% 	 Problem 1    %
% % % % % % % % % %

check_each_length([],_).
check_each_length([H|T], N) :-
	length(H, N),
	check_each_length(T, N).

check_unique(T, N) :-
	check_unique_element(T, N),
	transpose(T, TTran),
	check_unique_element(TTran, N).

check_unique_element([],_).
check_unique_element([H|T],N) :-
	fd_domain(H, 1, N),
	fd_all_different(H),
	check_unique_element(T, N).

% Transpose Matrix 
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% Checks for constraints
check_constraints([], [], []).
check_constraints([H|T], [HCons1|TCons1], [HCons2|Tcons2]) :-
	visible(H, 0, 0, HCons1),
	reverse(H, Hrev),
	visible(Hrev, 0, 0, HCons2),
	check_constraints(T, TCons1, Tcons2).
	
visible([],_,Count, Num) :-
	Num is Count.

visible([H|T], Curr, Count, Num) :-
	H > Curr,
	C is Count + 1,
	visible(T, H, C, Num).
visible([H|T], Curr, Count, Num) :-
	H =< Curr,
	visible(T, Curr, Count, Num).


tower(N, T, C) :-
	length(T, N),
	check_each_length(T, N),
	check_unique(T, N),
	maplist(fd_labeling, T),
	counts(Top, Bottom, Left, Right) = C,
	check_constraints(T, Left, Right),
	transpose(T, TTran),
	check_constraints(TTran, Top, Bottom).


% % % % % % % % % %
% 	 Problem 2    %
% % % % % % % % % %

unique_list(List, N) :-
	length(List, N),
	elements_between(List, 1, N),
	all_unique(List).

all_unique([ ]). 
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

elements_between(List, Min, Max) :- maplist(between(Min, Max), List).

plain_check_constraints(N, [], [], []).
plain_check_constraints(N, [H|T], [HCons1|TCons1], [HCons2|Tcons2]) :-
	unique_list(H, N),
	visible(H, 0, 0, HCons1),
	reverse(H, Hrev),
	visible(Hrev, 0, 0, HCons2),
	plain_check_constraints(N, T, TCons1, Tcons2).

plain_tower(N, T, C) :-
	length(T, N),	
	counts(Top, Bottom, Left, Right) = C,
	plain_check_constraints(N, T, Left, Right),
	transpose(T, TTran),
	plain_check_constraints(N, TTran, Top, Bottom).
	

% % % % % % % % % %
% 	 Statistics   %
% % % % % % % % % %


tower_test(Tt) :- 
	statistics(cpu_time, [Start|_]),
	tower(5, T,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [Stop|_]),
	Tt is Stop - Start.


plain_tower_test(Ptt) :-
	statistics(cpu_time, [Start|_]),
	plain_tower(5, T,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [Stop|_]),
	Ptt is Stop - Start.

speedup(Ratio) :-
	tower_test(Tt),
	plain_tower_test(Ptt),
	Ratio is Ptt / Tt.

% % % % % % % % % %
% 	 Problem 3    %
% % % % % % % % % %


ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.