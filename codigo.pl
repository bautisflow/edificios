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
    basic_building(X). % Igual puedo contar y sumar el primer nivel y comparar con el resto

% Predicates for Peano representation

% natural type
% TEST: probar si es natural por peano
nat(0).
nat(s(N)) :-
    nat(N).

% Is it greater or equal to some number
% ¿IS IT PL?
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
    
% NON-BASIC! Cuenta las viviendas de un nivel
count_homes_level([],0).
count_homes_level([s(X)|Xs],s(Y)) :-
    count_homes_level(Xs,Y).

% Add two natural numbers. NOT USED AT THE MOMENT
add_nat(0,X,X).
add_nat(s(X),Y,s(Z)) :-
    add_nat(X,Y,Z).

% Checks if two natural numbers are equal
nat_eq(0,0).
nat_eq(s(X),s(Y)) :-
    nat_eq(X,Y).

%all_levels_equal([X|Xs]) :-
    
    
