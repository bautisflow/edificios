:- module(_,_).

% AUTOR: Jaime Bautista Salinero - 53940280-J, 150103
alumno_prode('Salinero', 'Bautista', 'Jaime', 'X150103').

% Tipos
% TEST: Probar distintas combinaciones de edificicos
% FALLA: si al menos todos los elementos de una lista son naturales devuelve verdadero cuando tendría que ser falso
% Lo que tengo que hacer es 'deconstruir' la lista de listas
basic_building(X) :- % N. Al menos un nivel y cada nivel una vivienda
        %list_members(Y,X),    % Será una lista (X) de listas (Ys). Los elementos de cada Y serán números naturales. 
    is_list_list_members_nat(X),
    has_level_one_home(X).    % Si cada nivel tiene que tener al menos una vivienda no podrá haber listas vacias.


building(X) :- % N basic building donde todos los niveles tienen el mismo número de viviendas.
    basic_building(X), % Igual puedo contar y sumar el primer nivel y comparar con el resto
    all_levels_equal(X).

% Predicates for Peano representation

% natural type
% TEST: probar si es natural por peano
nat(0).
nat(s(N)) :-
    nat(N).

% Is it greater or equal to some number
% ¿IS IT PL? No es PL, no se puede usar
nat_geq(0,0).
nat_geq(N,0) :-
	nat(N),
	N \= 0.
nat_geq(s(N),s(M)) :-
	nat_geq(N,M).

% Predicates for List representation

% Definición de lista
% TEST: probar si es una lista
list([]).
list([X|Xs]) :-
	list(Xs).

% Recorre cada miembro de una lista
% TEST: probar por los elementos de una lista
% TEST de INTEGRACIÓN: Probar si los elementos de una lista son naturales
list_members(X,[X|Xs]).
list_members(X,[Y|Ys]) :-
	list_members(X,Ys).


% BASIC! Comprueba si los miembros de una lista son naturales
% TEST: probar con los elementos de una lista
is_list_members_nat([]).
is_list_members_nat([X|Xs]) :-
    nat(X),
    is_list_members_nat(Xs).

% BASIC! Recorre cada lista del edificio para comprobar que todos los numeros de
% las mismas son naturales
is_list_list_members_nat([]).
is_list_list_members_nat([X|Xs]) :-
    is_list_members_nat(X),
    is_list_list_members_nat(Xs).

% BASIC! Recorre cada nivel del edificio para comprobar que al menos tiene una vivienda
has_level_one_home([]).
has_level_one_home([X|Xs]) :-
    list_has_one(X),
    has_level_one_home(Xs).

% BASIC! Comprueba que una lista tiene al menos un numero > 0
list_has_one([s(X)|_]).
list_has_one([0|Xs]) :-
    list_has_one(Xs).
    
% NON-BASIC! Cuenta las viviendas de un nivel. Suma todos los elementos de una lista
% Not working it there are 0 in the middle
count_homes_level([],0).
count_homes_level([0|Xs],Y) :-
    count_homes_level(Xs,Y).
count_homes_level([s(X)|Xs],s(Y)) :-
    count_homes_level([X|Xs],Y).

% Add two natural numbers. NOT USED AT THE MOMENT
add_nat(0,X,X).
add_nat(s(X),Y,s(Z)) :-
    add_nat(X,Y,Z).

% Checks if two natural numbers are equal
nat_eq(0,0).
nat_eq(s(X),s(Y)) :-
    nat_eq(X,Y).

% Comprueba si todos los niveles tienes las mismas viviendas
all_levels_equal([X|Xs]) :-
        count_homes_level(X,Y),
        first_level_homes(Xs,Y). 

% Cuenta cada nivel y lo compara con el resto de niveles. Devuelve yes or no
first_level_homes([],_).
first_level_homes([X|Xs],Y) :-
    first_level_homes(Xs,Y),
    count_homes_level(X,Z),
    nat_eq(Y,Z).

% Lista las viviendas de un nivel C
level(X,N,C) :-
    level_aux(X,N,C).

% Esto lo que hace es sacar el elemento N de una lista, ya sea lista de lista = lista, o lista de numeros = numero
level_aux([C|_],s(0),C).
level_aux([X|Xs],s(N),C) :-
    level_aux(Xs,N,C).

% Lista formada por las columnas de un nivel concreto
column(X,N,C) :-
    column_aux(X,N,C).

column_aux([],_,[]).
column_aux([X|Xs],N,[C|Cs]) :-
    level_aux(X,N,C),
    column_aux(Xs,N,Cs).

% Devuelve una lista donde cada elemento es una lista correspondiente a cada columna
columns(X,C) :-
    building_columns(X,N),
    columns_aux(X,N,Y),
    list_reverse(Y,C).

% Me devuelve la lista pero al reves. por lo que tendrá que pasar por un reverse list después
columns_aux(_,0,[]).
columns_aux(X,s(N),[C|Cs]) :-
    column(X,s(N),C),
    columns_aux(X,N,Cs).

building_columns([X|Xs],N) :-
    list_length(X,N).

% Numero total de personas
total_people(X,T) :-
    lists_to_list(X,Y), % Aplana la lista
    count_homes_level(Y,T).


% LISTA DE LISTAS A LISTA DE ELEMENTOS
% Aplana una lista
lists_to_list([],[]).
lists_to_list([X|Xs],Y) :-
    lists_to_list(X,Ys1),
    lists_to_list(Xs,Ys2),
    list_append(Ys1,Ys2,Y).
lists_to_list(X,[X]) :-
    nat(X).


% Comprueba si dos listas son iguales. Devuelve yes or no
list_same_length(Ls,Lt) :-
    list_length(Ls, Ts),
    list_length(Lt, Tt),
    nat_eq(Ts,Tt).

% Devuelve el elemento indicado por el parámetro del medio
% de una lista. ESTA FUNCIÖN ES IGUAL QUE level_aux
% Ahora es distinto de el otro porque devuelve un elemento metido en una lista
sublevel_column([X|_],s(0),[X]).
sublevel_column([X|Xs],s(N),C) :-
    sublevel_column(Xs,N,C).

list_append([],Ys,Ys).
list_append([X|Xs],Ys,[X|Zs]) :-
	list_append(Xs,Ys,Zs).

list_length([],0).
list_length([X|Xs],s(N)) :-
	list_length(Xs,N).

list_reverse(Xs,Ys) :-
    list_reverse(Xs,[],Ys).
list_reverse([],Ys,Ys).
list_reverse([X|Xs],Acc,Ys) :-
	list_reverse(Xs,[X|Acc],Ys).
