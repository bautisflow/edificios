:- module(_,_, [assertions]).

:- doc(title, "Primera práctica - edificios").
:- doc(author, "Jaime Bautista Salinero; 150103").
:- doc(module, "Este módulo se representa un edificio de viviendas.

Un edificio está representado por una lista, que a su vez contiene sublistas que
representarán las distintas plantas del edificio. Los elementos de cada sublista
serán las viviendas y su valor el número de habitantes para dicha vivienda.

El número de habitantes estará representado por números en notación de Peano:
@includedef{nat/1}

@section{Generación de la documentación}

Esta documentación ha sido generada automáticamente con la herramienta
@href{http://ciao-lang.org/ciao/build/doc/lpdoc.html/}{@bf{lpdoc}}. Para generarla,
desde una línea de comandos ubicada en el directorio donde se encuentra el fichero de código,
se ha ejecutado:
~$ lpdoc -t pdf codigo.pl").

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
nat_eq(0,0).
nat_eq(s(N),s(M)) :-
    nat_eq(N,M).

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
nat_add(0,X,X).
nat_add(s(X),Y,s(Z)) :-
    nat_add(X,Y,Z).

% Checks if two natural numbers are equal
nat_geq(0,0).
nat_geq(s(X),0) :-
    nat(X).
nat_geq(s(X),s(Y)) :-
    nat_geq(X,Y).

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

% Devuelve el numero de habitantes en H y el numero de casas en V
total_people_homes(X,H,V) :-
    lists_to_list(X,Y), % Aplana la lista
    count_homes_level(Y,H),
    list_length(Y,V).


% LISTA DE LISTAS A LISTA DE ELEMENTOS
% Aplana una lista
lists_to_list([],[]).
lists_to_list([X|Xs],Y) :-
    lists_to_list(X,Ys1),
    lists_to_list(Xs,Ys2),
    list_append(Ys1,Ys2,Y).
lists_to_list(X,[X]) :-
    nat(X).

% habitantes / viviendas -> total_people / list_length
average(X,s(0)) :-
    total_people_homes(X,H,V),
    nat_eq(V,H). % Si las viviendas son mayor que los habitantes
average(X,A) :-
    total_people_homes(X,H,V),
    nat_gt(V,H), % Si las viviendas son mayor que los habitantes
    round(s(0),H,A).
average(X,A) :-
    total_people_homes(X,H,V),
    nat_gt(H,V), % Que los habitantes sean mayor que las viviendas
    nat_mod(H,V,R), % Resto a R
    nat_add(Hh,R,H), % A habitantes le quito el resto para una división justa
    nat_prod(P,V,Hh), % Multiplicación usada como división. Que pasa si hay más viviendas que habitantes?
    round(P,R,A).

% Siguiendo el redondeo del IEEE 754 de 2008, se redondea al más cercano.
% En el caso del .5 se redondea al par más cercano
% De esta manera, en la mitad de los casos el redondeo será hacia el superior
% y en la otra mitad hacia el inferior
round(P,R,s(P)) :-
    nat_prod(R,s(s(0)),Pp),
    nat_gt(Pp,P).
round(P,R,P) :-
    nat_prod(R,s(s(0)),Pp),
    nat_gt(P,Pp).
round(P,R,s(P)) :-
    nat_prod(R,s(s(0)),Pp),
    nat_eq(Pp,P),
    nat_odd(P).
round(P,R,P) :-
    nat_prod(R,s(s(0)),Pp),
    nat_eq(Pp,P),
    nat_even(P).

nat_even(0).
nat_even(s(s(X))) :-
    nat_even(X).

nat_odd(s(0)).
nat_odd(s(s(X))) :-
    nat_odd(X).
    
    
% X = dividendo; Y = divisor, C = cociente
nat_division(_,0,_).
nat_division(0,Y,0) :-
    nat_gt(Y,0).
%nat_division(X,Y,C) :-

nat_gt(s(X),0) :-
    nat(X).
nat_gt(s(X),s(Y)) :-
    nat_gt(X,Y).

% Modulo: resto de la division
nat_mod(X,Y,X) :-
    nat_gt(Y,X).
nat_mod(X,Y,R) :-
    nat_add(Z,Y,X),
    nat_mod(Z,Y,R).


% Si puedo prescindir de esto mejor
nat_prod(0,_,0).
%nat_prod(s(0),X,X).
nat_prod(s(X),Y,Z) :-
    nat_add(Y,T,Z),
    nat_prod(X,Y,T).

% Comprueba si dos listas son iguales. Devuelve yes or no
% Seguro que se puede hacer más bonito y recursivo
% ADEMÁS, HACE uso de nat_eq y no puedo usar el existente
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
