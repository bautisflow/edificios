:- module(_,_).

% AUTOR: Jaime Bautista Salinero - 53940280-J - 150103

% En este programa unicamente se cumple con el ejemplo propuesto en el enunciado.
% La manera correcta de resolver el ejercicio es separando elemento a elemento el numero inicial,
% metiendolo en su lista correspondiente (Parte entera o parte Decimal), tomando como punto de referencia la coma.
% Se podría ir descomponiedo la lista por la cabeza introduciendo los elementos en la lista entera hasta llegar
% a la coma y una vez pasada la coma seguir con la descomposición por la cabeza introduciendolo en la lista decimal.

%PREDICADO PEDIDO
%redondearDecimal/3: recibe el numero inicial, el tipo de redondeo y devuelve el numero final.
redondearDecimal([N,P,D], redondeoUnidad, redondeo(redondeoUnidad, numeroOriginal(P, [N], [D]), numeroRedondeado(P, N2, []))) :-
		mayor_igual(D, s(s(s(s(s(0))))), S).
		suma(N, S, N2).

% PREDICADOS AUXILIARES
% mayor_igual/3: Predicado que comprueba si un elemento es mayor o igual a otro
mayor_igual(_,s(0),s(0)). % Si X es menor devuelve 0
mayor_igual(s(0),_,0). % Si X es mayor devuelve 1
mayor_igual(s(X),s(Y), Z) :-
	mayor_igual(X,Y,Z).
	
% suma/3: Predicado que realiza una suma en notación de Peano. primerNumero, segundoNumero, Resultado.
suma(0,X,X).
suma(s(X),Y,s(Z)) :-
	suma(X,Y,Z).
	