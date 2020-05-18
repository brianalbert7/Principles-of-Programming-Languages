/* YOUR CODE HERE (Prolem 1, delete the following line) */
range(S,E,M) :- M>=S, M=<E.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Prolem 2, delete the following line) */
reverseL([], []).
reverseL([Head|Tail], X) :- reverseL(Tail, RevX), append(RevX, [Head], X).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Prolem 3, delete the following line) */
memberL(X,[X|_]).
memberL(X,[_|Tail]) :- memberL(X,Tail).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Prolem 4, delete the following line) */
zip([], [], []).
zip([_|_], [], []).
zip([], [_|_], []).
zip([H1|T1], [H2|T2], [X-Y|Zs]) :- zip(Xs, Ys, Zs).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Prolem 5, delete the following line) */
insert(X, [], [X]). 
insert(X, [Ys | T], [X,Ys | T]) :- X < Ys.
insert(X, [Ys | T1], [Ys | T2]) :- insert(X, T1, T2).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Prolem 6, delete the following line) */
remove_duplicates([], []).
remove_duplicates([Head|Tail], T2) :- memberL(Head, Tail), remove_duplicates(Tail, T2).
remove_duplicates([Head|Tail], [Head|T2]) :- not(memberL(Head, Tail)), remove_duplicates(Tail, T2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Prolem 7, delete the following line) */
intersectionL([],L,[]).
intersectionL([H|T],L,L2) :- not(memberL(H,L)), intersectionL(T,L,L2).
intersectionL([H|T],L,[H2|T2]) :- memberL(H,L), intersectionL(T,L,T2).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Prolem 8, delete the following line) */
prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

partition(_,[],[]).
partition(L,L,[]).
partition(L,P,[H|T]) :- prefix(P,L), suffix(S,L), partition(L,S,T).       

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Prolem 9, delete the following line) */
merge([],[],[]).
merge([L1],[],[L1]).
merge([],[L2],[L2]).
merge([H1|T1],[H2|T2],[H1|T]) :- H1 =< H2, merge(T1,[H2|T2],T).
merge([H1|T1],[H2|T2],[H2|T]) :- merge([H1|T1],T2,T).


?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Prolem 10, delete the following line) */
mergesort([],[]).
mergesort([L],[L]).
mergesort(L,SL) :- half(L, L1, L2), mergesort(L1, L3), mergesort(L2, L4), merge(L3, L4, SL).

half([],[],[]).
half([L],[L],[]).
half([H1,T1|T],[H1|T2],[T1|T3]) :- half(T,T2,T3).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
