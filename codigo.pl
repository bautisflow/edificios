:- module(_,_, [assertions]).

:- doc(title, "Primera pr@'{a}ctica - edificios").
:- doc(subtitle, "Programaci@'{o}n Declarativa: L@'{o}gica y Restricciones").
:- doc(author, "Jaime Bautista Salinero; 150103").
:- doc(date, "@today").
:- doc(module, "Este m@'{o}dulo se representa un edificio de viviendas.

Un edificio est@'{a} representado por una lista, que a su vez contiene sublistas que
representar@'{a}n las distintas plantas del edificio. Los elementos de cada sublista
ser@'{a}n las viviendas y su valor el n@'{u}mero de habitantes para dicha vivienda.

El n@'{u}mero de habitantes estar@'{a} representado por n@'{u}meros en notaci@'{o}n de Peano,
por lo que deber@'{a}n ser de la siguiente forma:
@includedef{nat/1}

@section{Aclaraci@'{o}n sobre redondeo}

En cuanto al redondeo realizado en el predicado @pred{average}, en el que se ped@'{i}a el redondeo al n@'{u}mero natural m@'{a}s cercano, este redondeo se ha implementado siguiendo las directrices del est@'{a}ndar IEEE 754-2008, @href{https://standards.ieee.org/standard/754-2008.html}, mediante el cual, en caso de empate,que el decimal sea .5, se redondear@'{a} al n@'{u}mero natural par de entre las dos posibilidades existentes. De esta manera, en la mitad de las ocasiones se redondear@'{a} al elemento superior y en la otra mitad al elemento inferior.

@section{Estructura de la documentaci@'{o}n}

Los predicados explicados en la secci@'{o}n 'Documentation on exports' est@'{a}n ordenados de la siguiente manera para facilitar su estructura:
@begin{enumerate}
@item Predicados pedidos en el ejercicio y a continuaci@'{o}n sus predicados auxiliares.
@item Predicados empleados para la representaci@'{o}n y tratamiento de n@'{u}meros naturales en notaci@'{o}n de Peano.
@item Predicados sobre listas.
@end{enumerate}

@section{Consultas de comprobaci@'{o}n}

A continuaci@'{o}n, se muestras las peticiones realizadas al programa para validar los resultados y a su derecha el resultado esperado:

@subsection{basic_building}
@noindent
@tt{basic_building([0,0]). -> no} @p
@noindent
@tt{basic_building([s(0),0]). -> no} @p
@noindent
@tt{basic_building([[0,0,0],[],[0,0,0]]). -> no} @p
@noindent
@tt{basic_building([[0],[0]]). -> yes} @p
@noindent
@tt{basic_building([[0,0],[0,0]]). -> yes} @p
@noindent
@tt{basic_building([[0,0,0],[0,0,0],[0,0,0]]). -> yes} @p
@noindent
@tt{basic_building([[s(0),0,0],[0,s(0),0],[0,0,s(0)]]). -> yes} @p
@noindent
@tt{basic_building([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]]). -> yes} @p
@noindent
@tt{basic_building([[s(0),s(s(0))],[0,s(s(s(0))),0],[s(0),s(0),s(0),0]]). -> yes} @p

@subsection{building}
@noindent
@tt{building([[0,0],[0,0,0],[0,0,0]]). -> no} @p
@noindent
@tt{building([[s(0),s(s(0))],[0,s(s(s(0))),0],[s(0),s(0),s(0),0]]). -> no} @p
@noindent
@tt{building([[0],[0]]). -> yes} @p
@noindent
@tt{building([[0,0],[0,0]]). -> yes} @p
@noindent
@tt{building([[0,0,0],[0,0,0],[0,0,0]]). -> yes} @p
@noindent
@tt{building([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]]). -> yes} @p
@noindent
@tt{building([[s(0),0,0],[0,s(0),0],[0,0,s(0)]]). -> yes} @p

@subsection{level}
@noindent
@tt{level([s(0),0,s(s(s(0)))],s(s(s(0))),C). -> no} @p
@noindent
@tt{level([[0,0],[0,0]],s(s(s(0))),C). -> no} @p
@noindent
@tt{level([[0],[0]],s(0),C). -> yes + C = [0]} @p
@noindent
@tt{level([[0,0],[0,0]],s(s(0)),C). -> yes + C = [0,0]} @p
@noindent
@tt{level([[0,0,0],[0,0,0],[0,0,0]],s(s(s(0))),C). -> yes + C = [0,0,0]} @p
@noindent
@tt{level([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(0),C).} @p @tt{-> yes + C = [s(0),s(s(0)),0]} @p
@noindent
@tt{level([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(s(0)),C).} @p @tt{-> yes + C = [0,s(s(s(0))),0]} @p
@noindent
@tt{level([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(s(s(0))),C).} @p @tt{-> yes + C = [s(0),s(0),s(0)]} @p

@subsection{column}
@noindent
@tt{column([s(0),0,s(s(s(0)))],s(s(s(0))),C). -> no} @p
@noindent
@tt{column([[0],[0]],s(0),C). -> yes + C = [0,0]} @p
@noindent
@tt{column([[0,0],[0,0]],s(s(s(0))),C). -> yes + C = []} @p
@noindent
@tt{column([[0,0,s(0)],[0,0]],s(s(s(0))),C). -> yes + C = [s(0)]} @p  
@noindent
@tt{column([[0,0],[0,0]],s(s(0)),C). -> yes + C = [0,0]} @p
@noindent
@tt{column([[0,0,0],[0,0,0],[0,0,0]],s(s(s(0))),C). -> yes + C = [0,0,0]} @p
@noindent
@tt{column([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(0),C).} @p @tt{-> yes + C = [s(0),0,s(0)]} @p
@noindent
@tt{column([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(s(0)),C).} @p @tt{-> yes + C = [s(s(0)),s(s(s(0))),s(0)]} @p
@noindent
@tt{column([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],s(s(s(0))),C).} @p @tt{-> yes + C = [0,0,s(0)]} @p

@subsection{columns}
@noindent
@tt{columns([s(0),0,s(s(s(0)))],C). -> no} @p
@noindent
@tt{columns([[0],[0]],C). -> yes + C = [[0,0]]} @p
@noindent
@tt{columns([[0,0,s(0)],[0,0]],C). -> yes + C = [[0,0],[0,0],[s(0)]]} @p
@noindent
@tt{columns([[0,0],[0,0]],C). -> yes + C = [[0,0],[0,0]]} @p
@noindent
@tt{columns([[0,0,0],[0,0,0],[0,0,0]],C). -> yes + C = [[0,0,0],[0,0,0],[0,0,0]]} @p
@noindent
@tt{columns([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],C).} @p @tt{-> yes + C = [[s(0),0,s(0)],[s(s(0)),s(s(s(0))),s(0)],[0,0,s(0)]]} @p
@noindent
@tt{columns([[s(0),0,s(s(s(0))),0],[0,s(s(0)),s(s(s(s(s(0)))))],[s(0),s(0),s(0),0,0]],C).} @p @tt{-> yes + C = [[s(0),0,s(0)],[0,s(s(0)),s(0)],[s(s(s(0))),s(s(s(s(s(0))))),s(0)],[0,0],[0]]} @p

@subsection{total_people}
@noindent
@tt{total_people([s(0),0,s(s(s(0)))],C). -> no} @p
@noindent
@tt{total_people([[0],[0]],C). -> yes + C = 0} @p
@noindent
@tt{total_people([[0,0,s(0)],[0,0]],C). -> yes + C = 0} @p
@noindent
@tt{total_people([[0,0],[0,0]],C). -> yes + C = 0} @p
@noindent
@tt{total_people([[0,0,0],[0,0,0],[0,0,0]],C). -> yes +  C = 0} @p
@noindent
@tt{total_people([[s(0),s(s(s(0)))],[0,s(s(0))]],C). -> yes +  C = s(s(s(s(s(s(0))))))} @p
@noindent
@tt{total_people([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],C).} @p @tt{-> yes +  C = s(s(s(s(s(s(s(s(s(0)))))))))} @p
@noindent
@tt{total_people([[s(0),s(s(s(0))),0],[0,s(s(0)),s(s(s(s(s(0)))))],[s(0),s(0),s(0)]],C).} @p @tt{-> yes + C = s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))} @p
@noindent
@tt{total_people([[s(0),0,s(s(s(0))),0],[0,s(s(0)),s(s(s(s(s(0)))))],[s(0),s(0),s(0),0,0]],C).} @p @tt{-> yes + C = s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))} @p

@subsection{average}
@noindent
@tt{average([s(0),0,s(s(s(0)))],C). -> no} @p
@noindent
@tt{average([[0],[0]],C). -> yes + C = 0} @p
@noindent
@tt{average([[0,0,s(0)],[0,0]],C). -> yes + C = 0} @p
@noindent
@tt{average([[0,0],[0,0]],C). -> yes + C = 0} @p
@noindent
@tt{average([[0,0,0],[0,0,0],[0,0,0]],C). -> yes +  C = 0} @p
@noindent
@tt{average([[s(0),s(0),s(0)],[0,0]],C). -> yes + C = s(0)} @p
@noindent
@tt{average([[s(0),s(s(s(0)))],[0,s(s(0))]],C). -> yes + C = s(s(0))} @p
@noindent
@tt{average([[s(0),s(s(0)),0],[0,s(s(s(0))),0],[s(0),s(0),s(0)]],C). -> yes + C = s(0)} @p
@noindent
@tt{average([[s(0),s(s(s(0))),0],[0,s(s(0)),s(s(s(s(s(0)))))],[s(0),s(0),s(0)]],C). -> yes + C = s(s(0))} @p
@noindent
@tt{average([[s(0),0,s(s(s(0))),0],[0,s(s(0)),s(s(s(s(s(0)))))],[s(0),s(0),s(0),0,0]],C). -> yes + C = s(s(0))} @p

@section{Generaci@'{o}n de la documentaci@'{o}n}

Esta documentaci@'{o}n ha sido generada automaticamente con la herramienta
@href{http://ciao-lang.org/ciao/build/doc/lpdoc.html/}{@bf{lpdoc}}. Para generarla,
desde una l@'{i}nea de comandos ubicada en el directorio donde se encuentra el fichero de c@'{o}digo,
se ha ejecutado:
@begin{verbatim}
~$ lpdoc -t pdf codigo.pl
@end{verbatim}

").

% AUTOR: Jaime Bautista Salinero - 53940280-J, 150103
alumno_prode('Bautista', 'Salinero', 'Jaime', 'X150103').

%% ---------------------------------------%%
% --- Predicates for buldings exercise --- %
%% ---------------------------------------%%
%%% Types

:- prop basic_building(X) # "@var{X} es un edificio, es decir una lista de listas, donde los elementos de las sublistas son n@'{u}meros naturales en notaci@'{o}n de Peano. @includedef{basic_building/1}".
basic_building(X) :- % N. Al menos un nivel y cada nivel una vivienda
    sublist_members_nat(X),
    levels_have_home(X).    % Si cada nivel tiene que tener al menos una vivienda no podra haber listas vacias.

:- prop building(X) # "@var{X} es un edificio (basic_building) donde todos los niveles tienen el mismo número de viviendas. @includedef{building/1}".
building(X) :- % N basic building donde todos los niveles tienen el mismo número de viviendas.
    basic_building(X), % Igual puedo contar y sumar el primer nivel y comparar con el resto
    all_levels_equal(X).

%%% Predicates

:- pred level(X,N,C) # "@var{C} es la lista con todas las viviendas del @var{N}-@'{e}simo nivel de un edificio @var{X}. @includedef{level/3}".
level([C|_],s(0),C) :-
    list(C).
level([_|Xs],s(N),C) :-
    level(Xs,N,C).

:- pred column(X,N,C) # "@var{C} es la lista formada por las viviendas @var{N}-@'{e}simas de todos los niveles del edificio @var{X}. Si se pide una columna que el edificio @var{X} no tiene se devolver@'{a} una lista vac@'{i}a. @includedef{column/3}". 
column([],_,[]).
column([X|Xs],N,[C|Cs]) :-
    list_length(X,L),
    nat_geq(L,N),
    list_select(X,N,C),
    column(Xs,N,Cs).
column([X|Xs],N,Cs) :- % Para edificios irregulares en columnas
    list_length(X,L),
    nat_gt(N,L),
    column(Xs,N,Cs).

:- pred columns(X,C) # "@var{C} es la lista de las columnas de viviendas del edificio @var{X}. @includedef{columns/2}".
columns(X,C) :-
    longest_level(X,Ls),
    list_length(Ls,L),
    columns_aux(X,L,Y),
    list_reverse(Y,C).

:- pred total_people(X,T) # "Calcula el n@'{u}mero total de habitantes del edificio @var{X}, devolviendo el resultado en @var{T}. El procedimiento que sigue es aplanar la lista @var{X} y sumar todos los elementos de la lista aplanada. @includedef{total_people/2}". 
total_people(X,T) :-
    sublist_members_nat(X),
    list_flatten(X,Y),
    count_level_people(Y,T).

:- pred average(X,A) # "Calcula la media de personas, en @var{A}, que viven en cada vivienda del edificio @var{X} y redondea el valor al n@'{u}mero natural m@'{a}s cercano. @var{A} ser@'{a} el resultado de dividir habitantes / viviendas y el redondeo se realiza en base al est@'{a}ndar IEEE 754 de 2008. @includedef{average/2}". 
average(X,s(0)) :-      % Si hay el mismo numero de viviendas que de habitantes
    total_people_homes(X,H,V),
    nat_eq(V,H).
average(X,A) :-         % Si las viviendas son mayor que los habitantes
    total_people_homes(X,H,V),
    nat_gt(V,H),
    nat_prod(H,s(s(0)),Z),
    nat_gt(Z,V),
    nat_round(s(0),0,A).
average(X,A) :-         % Si las viviendas son mayor que los habitantes
    total_people_homes(X,H,V),
    nat_gt(V,H),
    nat_prod(H,s(s(0)),Z),
    nat_gt(V,Z),
    nat_round(0,0,A).
average(X,A) :-         % Si los habitantes son mayor que las viviendas
    total_people_homes(X,H,V),
    nat_gt(H,V),
    nat_mod(H,V,R),     % R = resto de la division habitantes / viviendas
    nat_add(Hh,R,H),    % Quitamos el resto para una division exacta
    nat_prod(P,V,Hh),   % Empleo de la multiplicacion para la division
    nat_round(P,R,A).

%%%% Auxiliary predicates

%:- prop level_has_home(X) # "Comprueba que un nivel @var{X} de un edificio tiene al menos una vivienda. Comprueba si la lista @var{X} tiene alguno de sus elementos mayor estricto (>) que 0. @includedef{level_has_home/1}".
%level_has_home([s(_)|_]).
%level_has_home([0|Xs]) :-
%    level_has_home(Xs).

:- prop levels_have_home(X) # "Recorre un edificio @var{X} comprobando que cada nivel tiene al menos una vivienda. Recorre la lista @var{X} de listas comprobando que todas las sublistas tienes al menos un elemento mayor estricto (>) que 0. @includedef{levels_have_home/1}".  
levels_have_home([_]).
levels_have_home([_|Xs]) :-
        %level_has_home(X),
    levels_have_home(Xs).

:- pred count_level_people(X,Y) # "Cuenta todos los habitantes de un nivel @var{X} de un edificio, devolviendo el resultado en @var{Y}. Recorre una lista @var{X} sumando todos sus elementos en @var{Y}. @includedef{count_level_people/2}".
count_level_people([],0).
count_level_people([0|Xs],Y) :-
    count_level_people(Xs,Y).
count_level_people([s(X)|Xs],s(Y)) :-
    count_level_people([X|Xs],Y).

:- prop all_levels_equal(X) # "Comprueba que todos los niveles de un edificio @var{X} tienen el mismo n@'{u}mero de viviendas. Recorre las sublistas de una lista @var{X} comprobando que la longitud de una sublista es igual que el resto de sublistas. @includedef{all_levels_equal/1}".
all_levels_equal([X|Xs]) :-
    list_length(X,Y),
    levels_equal(Xs,Y). 

:- prop levels_equal(X,Y) # "Comprueba que todos los niveles dados en el edificio @var{X} tienen el n@'{u}mero de viviendas indicado por @var{Y}. Recorre la lista @var{X} de sublistas comprobando que la lista de sus elementos es igual a @var{Y}. @includedef{levels_equal/2}".
levels_equal([],_).
levels_equal([X|Xs],Y) :-
    levels_equal(Xs,Y),
    list_length(X,Z),
    nat_eq(Y,Z).

:- pred longest_level(X,L) # "Devuelve en @var{Y} el nivel del edificio @var{X} de mayor longitud de viviendas. @includedef{longest_level/2}".
longest_level([X|Xs],L) :-
    longest_level(Xs,X,L).
:- pred longest_level(X,Y,L) # "Predicado auxiliar para el anterior predicado. @includedef{longest_level/3}".
longest_level([],L,L). 
longest_level([X|Xs],Y,L) :-
    list_length(X,Z1),
    list_length(Y,Z2),
    nat_geq(Z1,Z2),
    longest_level(Xs,X,L).
longest_level([X|Xs],Y,L) :-
    list_length(X,Z1),
    list_length(Y,Z2),
    nat_gt(Z2,Z1),
    longest_level(Xs,Y,L).

:- pred columns_aux(X,N,C) # "@var{C} es la lista, del rev@'{e}s, de las columnas del edificio @var{X}. Dado que se trata de un predicado auxiliar para colums, se le pasa en @var{N} la longitud (niveles) del edificio @var{X}. @includedef{columns_aux/3}". 
columns_aux(_,0,[]).
columns_aux(X,s(N),[C|Cs]) :-
    column(X,s(N),C),
    columns_aux(X,N,Cs).
    

:- prop total_people_homes(X,H,V) # "Calcula el n@'{u}mero total de habitantes, en @var{H}, y de viviendas, en @var{V}, de un edificio @var{X}. @includedef{total_people_homes/3}".
total_people_homes(X,H,V) :-
    sublist_members_nat(X),
    list_flatten(X,Y),
    count_level_people(Y,H),
    list_length(Y,V).

%% ----------------------------------------%%
% -- Predicates for Peano representation -- %
%% ----------------------------------------%%

:- prop nat(X) #"@var{X} es un n@'{u}mero natural en notaci@'{o}n de Peano. @includedef{nat/1}".
nat(0).
nat(s(N)) :-
  nat(N).

:- prop nat_eq(X,Y) # "@var{X} y @var{Y} son iguales la una a la otra. @includedef{nat_eq/2}".
nat_eq(0,0).
nat_eq(s(X),s(Y)) :-
    nat_eq(X,Y).

:- prop nat_gt(X,Y) # "@var{X} es mayor estricto (>) que @var{Y}. @includedef{nat_gt/2}".
nat_gt(s(X),0) :-
    nat(X).
nat_gt(s(X),s(Y)) :-
    nat_gt(X,Y).

:- prop nat_geq(X,Y) # "@var{X} es mayor o igual (>=) que @var{Y}. @includedef{nat_geq/2}".
nat_geq(0,0).
nat_geq(s(X),0) :-
    nat(X).
nat_geq(s(X),s(Y)) :-
    nat_geq(X,Y).

:- prop nat_even(X) # "@var{X} es par. @includedef{nat_even/1}".
nat_even(0).
nat_even(s(s(X))) :-
    nat_even(X).

:- prop nat_odd(X) # "@var{X} es impar. @includedef{nat_odd/1}".
nat_odd(s(0)).
nat_odd(s(s(X))) :-
    nat_odd(X).

:- pred nat_add(X,Y,S) # "@var{S} es el resultado de la suma @var{X} + @var{Y}. @includedef{nat_add/3}".
nat_add(0,X,X).
nat_add(s(X),Y,s(S)) :-
    nat_add(X,Y,S).

:- pred nat_prod(X,Y,P) # "@var{P} es el producto de multiplicar @var{X} * @var{Y}. @includedef{nat_prod/3}".
nat_prod(0,_,0).
nat_prod(s(X),Y,P) :-
    nat_add(Y,Z,P),
    nat_prod(X,Y,Z).

:- pred nat_mod(X,Y,R) # "@var{R} es el resultado de la operaci@'{o}n m@'{o}dulo, es decir, el resto de la divisi@'{o}n @var{X} / @var{Y}. @includedef{nat_mod/3}".
nat_mod(X,Y,X) :-
    nat_gt(Y,X).
nat_mod(X,Y,R) :-
    nat_add(Z,Y,X),
    nat_mod(Z,Y,R).

:- pred nat_round(X,R,Y) # "Redondea el n@'{u}mero dado por @var{X} en funci@'{o}n del resto, @var{R}, obtenido en una divisi@'{o}n anterior al n@'{u}mero entero m@'{a}s cercano, devolviendo el resultado del redondeo en @var{Y}. Para la toma de decisiones, especialmente en la mitad de dos n@'{u}meros, se toma como referencia el est@'{a}ndar IEEE 754 de 2008. @includedef{nat_round/3}".
nat_round(X,R,s(X)) :-
    nat_prod(R,s(s(0)),Y),
    nat_gt(Y,X).
nat_round(X,R,X) :-
    nat_prod(R,s(s(0)),Y),
    nat_gt(X,Y).
nat_round(X,R,s(X)) :-
    nat_prod(R,s(s(0)),Y),
    nat_eq(Y,X),
    nat_odd(X).
nat_round(X,R,X) :-
    nat_prod(R,s(s(0)),Y),
    nat_eq(Y,X),
    nat_even(X).

%% ---------------------------------------%%
% -- Predicates for List representation -- %
%% ---------------------------------------%%

:- prop list(X) # "@var{X} es una lista. @includedef{list/1}".
list([]).
list([_|Xs]) :-
     list(Xs).

:- prop list_members_nat(X) # "@var{X} es una lista compuesta por n@'{u}meros naturales en notaci@'{o}n de Peano. @includedef{list_members_nat/1}".
list_members_nat([X]) :-
    nat(X).
list_members_nat([X|Xs]) :-
    nat(X),
    list_members_nat(Xs).

:- prop sublist_members_nat(X) # "@var{X} es una lista compuesta por listas donde sus elementos son n@'{u}meros naturales en notaci@'{o}n de Peano. @includedef{sublist_members_nat/1}".
sublist_members_nat([]).
sublist_members_nat([X|Xs]) :-
    list_members_nat(X),
    sublist_members_nat(Xs).

% Seguro que se puede hacer mas bonito y recursivo
:- prop list_same_length(Xs,Ys) # "Comprueba si la lista @var{Xs} tiene la misma longitud que @var{Ys}. @includedef{list_same_length/2}".
list_same_length(Xs,Ys) :-
    list_length(Xs, Zs1),
    list_length(Ys, Zs2),
    nat_eq(Zs1,Zs2).

:- pred list_select(X,N,Y) # "Devuelve en @var{Y} el @var{N}-@'{e}simo n@'{u}mero de la lista @var{X}. @includedef{list_select/3}".
list_select([X|_],s(0),X).
list_select([_|Xs],s(N),Y) :-
    list_select(Xs,N,Y).

:- pred list_append(Xs,Ys,Zs) # "@var{Zs} ser@'{a} el resultado de introducir la lista @var{Ys} al final de la lista @var{Xs}. @includedef{list_append/3}".
list_append([],Ys,Ys).
list_append([X|Xs],Ys,[X|Zs]) :-
	list_append(Xs,Ys,Zs).

:- pred list_length(Xs,L) # "Calcula la longitud de una lista @var{Xs}, devolvi@'{e}ndolo en @var{L}. @includedef{list_length/2}".
list_length([],0).
list_length([_|Xs],s(L)) :-
	list_length(Xs,L).

:- pred list_reverse(Xs,Ys) # "@var{Ys} ser@'{a} la lista @var{Xs} del rev@'{e}s, es decir, intercambiando cada elemento 'n' de @var{Xs} por longitud(@var{Xs}) -'n' - 1. @includedef{list_reverse/2}".
list_reverse(Xs,Ys) :-
    list_reverse(Xs,[],Ys).
:- pred list_reverse(Xs,Acc,Ys) # "@var{Ys} ser@'{a} la lista @var{Xs} del rev@'{e}s. Se genera mediante el uso de un acumulador de elementos. @includedef{list_reverse/3}".
list_reverse([],Ys,Ys).
list_reverse([X|Xs],Acc,Ys) :-
	list_reverse(Xs,[X|Acc],Ys).

:- pred list_flatten(Xs,Ys) # "Aplana la lista @var{Xs}, devolviendo el resultado en @var{Ys}. El aplanado consiste en generar una lista de elementos, en este caso naturales, a partir de una lista cuyos elementos son listas. @includedef{list_flatten/2}".
list_flatten([],[]).
list_flatten([X|Xs],Y) :-
    list_flatten(X,Ys1),
    list_flatten(Xs,Ys2),
    list_append(Ys1,Ys2,Y).
list_flatten(X,[X]) :-
    nat(X).



