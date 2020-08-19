/* holds if the integer M is within the range S to E including S and E. */
range(S,E,M) :- M>=S, M=<E.

?- range(1,2,2).
?- not(range(1,2,3)).

/* RevX is the reverse of the list X. */
reverseL([], []).
reverseL([Head|Tail], X) :- reverseL(Tail, RevX), append(RevX, [Head], X).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* holds if  X∈L . Addtionally, if  L=∅ , return false. */
memberL(X,[X|_]).
memberL(X,[_|Tail]) :- memberL(X,Tail).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* elates three lists, where the third list XYs contains pairs whose elements are the elements at the same indices of the first two lists Xs and Ys. 
    We will use A-B to represent a pair of element A and B. For example, 1-2 is like a pair (1,2) */
zip([], [], []).
zip([_|_], [], []).
zip([], [_|_], []).
zip([H1|T1], [H2|T2], [X-Y|Zs]) :- zip(Xs, Ys, Zs).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* a relation between an integer X , a sorted list of integers Ys, and a second sorted list of integers Zs that contains the first 
  argument X and all the elements of the second argument Ys. */
insert(X, [], [X]). 
insert(X, [Ys | T], [X,Ys | T]) :- X < Ys.
insert(X, [Ys | T1], [Ys | T2]) :- insert(X, T1, T2).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* true if L2 is equal to the result of removing all duplicate elements from L1. 
  In the result, the order of the elements must be the same as the order in which the (first occurences of the) elements appear in L1. */
remove_duplicates([], []).
remove_duplicates([Head|Tail], T2) :- memberL(Head, Tail), remove_duplicates(Tail, T2).
remove_duplicates([Head|Tail], [Head|T2]) :- not(memberL(Head, Tail)), remove_duplicates(Tail, T2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* true if L3 is equal to the list containing intersection of the elements in L1 and L2 without any duplicates. 
  In other words, L3 should contain the elements that both in L1 and in L2. 
  As for union the predicate must be true for some order of elements of the intersection (but not necessarily all). */
intersectionL([],L,[]).
intersectionL([H|T],L,L2) :- not(memberL(H,L)), intersectionL(T,L,L2).
intersectionL([H|T],L,[H2|T2]) :- memberL(H,L), intersectionL(T,L,T2).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* P is the prefix of L and
  S is the suffix of L and
  append(P,S,L) holds
  If L is [], then P and S are [].
  If L is [H], then P is [H] and S is [].
  Otherwise,
  let length of L be N. Then length of P is div(N,2). Use Prolog's built-in integer division.
  length of S is N - div(N,2). */
prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

partition(_,[],[]).
partition(L,L,[]).
partition(L,P,[H|T]) :- prefix(P,L), suffix(S,L), partition(L,S,T).       

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* X and Y are sorted, and Z contains the same elements as U where append(X,Y,U) but Z is also additionally sorted. */
merge([],[],[]).
merge([L1],[],[L1]).
merge([],[L2],[L2]).
merge([H1|T1],[H2|T2],[H1|T]) :- H1 =< H2, merge(T1,[H2|T2],T).
merge([H1|T1],[H2|T2],[H2|T]) :- merge([H1|T1],T2,T).


?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* SL is the sorted version of L. Use the predicate to partition the list L into two, sort each on separately (using mergesort) and 
  combine the individual sorted list using merge. */
mergesort([],[]).
mergesort([L],[L]).
mergesort(L,SL) :- half(L, L1, L2), mergesort(L1, L3), mergesort(L2, L4), merge(L3, L4, SL).

half([],[],[]).
half([L],[L],[]).
half([H1,T1|T],[H1|T2],[T1|T3]) :- half(T,T2,T3).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
