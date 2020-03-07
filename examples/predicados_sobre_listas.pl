:- module(_,_).

my_list([]).
my_list([X|Xs]) :-
	my_list(Xs).

my_member(X,[X|Xs]).
my_member(X,[Y|Ys]) :-
	my_member(X,Ys).

my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]) :-
	my_append(Xs,Ys,Zs).

prefix([],Ys).
prefix([X|Xs],[X|Ys]) :-
	prefix(Xs,Ys).

suffix(Xs,Xs).
suffix(Xs,[Y|Ys]) :-
	suffix(Xs,Ys).
	
sublist_recursive(Xs,Ys) :-
	prefix(Xs,Ys).
sublist_recursive(Xs,[Y|Ys]) :-
	sublist_recursive(Xs,Ys).

sublist_suffix_of_a_prefix1(Xs,Ys) :-
	prefix(Ps,Ys),
	suffix(Xs,Ps).

sublist_prefix_of_a_suffix1(Xs,Ys) :-
	prefix(Xs,Ss),
	suffix(Ss,Ys).

sublist_suffix_of_a_prefix2(Xs,AsXsBs) :-
	my_append(AsXs,Bs,AsXsBs),
	my_append(As,Xs,AsXs). % AsXs is a prefix of AsXsBs

sublist_prefix_of_a_suffix2(Xs,AsXsBs) :-
	my_append(As,XsBs,AsXsBs),
	my_append(Xs,Bs,XsBs). % Xs is a suffix of AsXsBs

naive_reverse([],[]).
naive_reverse([X|Xs],Zs) :-
	naive_reverse(Xs,Ys),
	my_append(Ys,[X],Zs).

reverse_accumulate(Xs,Ys) :-
	reverse_accumulate(Xs,[],Ys).

reverse_accumulate([],Ys,Ys).
reverse_accumulate([X|Xs],Acc,Ys) :-
	reverse_accumulate(Xs,[X|Acc],Ys).

my_length([],0).
my_length([X|Xs],s(N)) :-
	my_length(Xs,N).

my_delete([],X,[]).
my_delete([X|Xs],X,Ys) :-
	my_delete(Xs,X,Ys).
my_delete([X|Xs],Z,[X|Ys]) :-
	X \= Z,
	my_delete(Xs,Z,Ys).

my_select([X|Xs],X,Xs).
my_select([Y|Ys],X,[Y|Zs]) :-
	my_select(Ys,X,Zs).

my_select_n(Xs,N,Zs) :-
	nat(N),
	nat_geq(N,0),
	my_length(Xs,M),
	nat_geq(M,N),
	my_select_n(Xs,N,[],Zs).

my_select_n(Xs,0,Acc,Acc).
my_select_n(Xs,s(N),Acc,Zs) :-
	my_select(Xs,X,Ys),
	my_select_n(Ys,N,[X|Acc],Zs).

my_insert(X,Ys,Zs) :-
	my_select(Zs,X,Ys).

my_permutation([],[]).
my_permutation(Xs,[Y|Ys]) :-
	my_select(Xs,Y,Zs),
	my_permutation(Zs,Ys).

ordered([]).
ordered([X]) :-
	nat(X).
ordered([X,Y|Ys]) :-
	nat(X),
	nat(Y),
	nat_leq(X,Y),
	ordered([Y|Ys]).

insert_into_sorted_list(X,[],[X]) :-
	nat(X).
insert_into_sorted_list(X,[Y|Ys],[X,Y|Ys]) :-
	nat(X),
	nat(Y),
	nat_leq(X,Y).
insert_into_sorted_list(X,[Y|Ys],[Y|Zs]) :-
	nat(X),
	nat(Y),
	nat_gt(X,Y),
	insert_into_sorted_list(X,Ys,Zs).

insertion_sort([],[]).
insertion_sort([X|Xs],Ys) :-
	insertion_sort(Xs,Zs),
	insert_into_sorted_list(X,Zs,Ys).

quicksort([],[]).
quicksort([X|Xs],Ys) :-
	partition(Xs,X,Littles,Bigs),
	quicksort(Littles,LS),
	quicksort(Bigs,BS),
	append(LS,[X|BS],Ys).

partition([],Y,[],[]) :-
	nat(Y).
partition([X|Xs],Y,[X|LS],BS) :-
	nat_leq(X,Y),
	partition(Xs,Y,LS,BS).
partition([X|Xs],Y,LS,[X|BS]) :-
	nat_gt(X,Y),
	partition(Xs,Y,LS,BS).

% EXAMPLE: DISTRIBUTING STICKERS AMONG KIDS

distribute([],[],Ds).
distribute(Ks,Ss,[K#S|Ds]) :-
	my_select(Ks,K,RKs),
	my_select(Ss,S,RSs),
	distribute(RKs,RSs,Ds).	

distribute2([K1,K2,K3,K4],[S1,S2,S3,S4],[K1#SA,K2#SB,K3#SC,K4#SD]) :-
	my_permutation([S1,S2,S3,S4],[SA,SB,SC,SD]).

% PREDICATES FOR PEANO REPRESENTATION

nat(0).
nat(s(N)) :-
	nat(N).

nat_geq(0,0).
nat_geq(N,0) :-
	nat(N),
	N \= 0.
nat_geq(s(N),s(M)) :-
	nat_geq(N,M).

nat_leq(0,0).
nat_leq(0,N) :-
	nat(N),
	N \= 0.
nat_leq(s(N),s(M)) :-
	nat_leq(N,M).

nat_lt(0,N) :-
	nat(N),
	N \= 0.
nat_lt(s(N),s(M)) :-
	nat_lt(N,M).

nat_gt(N,0) :-
	nat(N),
	N \= 0.
nat_gt(s(N),s(M)) :-
	nat_gt(N,M).