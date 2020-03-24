:- module(_, _, [assertions]).

:- doc(title, "Ejemplo de documentacion - Calculadora").

:- doc(author, "Isabel Garcia").

:- doc(module, "Este módulo define una calculadora para números de peano.

Los números aceptados por esta calculadora tienen que ser de la forma:
@includedef{nat/1}

@subsection{Ejemplos de uso}
@begin{enumerate}
@item Sumar dos numeros:
@begin{verbatim}
?- calcular('+', 0, s(0), X).

X = s(0) ? 
yes
?- 
@end{verbatim}
@item Restar dos numeros:
@begin{verbatim}
?- calcular('-', s(s(0)), s(0), X).

X = s(0) ? 
yes
?- 
@end{verbatim}
@end{enumerate}

Las operaciones disponibles son:
@includedef{operacion/1}

@section{Generación de la documentación}

Esta documentación ha sido generada automáticamente con la herramienta
@href{http://ciao-lang.org/ciao/build/doc/lpdoc.html/}{@bf{lpdoc}}. Para generarla,
selecciona las opciones del menú @tt{LPdoc -> Generate documentation for
buffer}, para visualizarla @tt{LPdoc -> View documentation in selected format}.

@begin{alert}
Este módulo incluye algunos comandos de formato pero hay muchos
más en:
@href{http://ciao-lang.org/ciao/build/doc/lpdoc.html/comments.html#stringcommand/1}.
@end{alert}

Para documentar específicamente cada predicado se usan aserciones. Por ejemplo:
@begin{verbatim}
:- pred calcular(Op,A,B,C)
   #``@var{C} es el resultado de aplicar la operacion @var{Op} a @var{A} y @var{B}.''.
@end{verbatim}

Para más información consulta:
@href{http://ciao-lang.org/ciao/build/doc/lpdoc.html/assertions_doc.html}.



@section{Tests automáticos}
Este módulo incluye aserciones que empiezan por @tt{:- test}. Por ejemplo
@begin{verbatim}
:- test calcular(Op,A,B,C) : (Op = '+', A = 0, B = 0) => (C = 0) + not_fails #``Caso base''.
@end{verbatim}

Estas definen casos de test. Dada una aserción @tt{:- test Cabeza : Llamada =>
Salida + Comp}, @var{Cabeza} denota a qué predicado se refiere la aserción,
@var{Llamada} describe los valores de entrada para el test, @var{Salida} define
los valores de salida @bf{si el predicado tiene éxito} y @var{Comp} lo vamos a
usar para definir si el predicado tiene que tener éxito para esa llamada o no:
@begin{itemize}
@item @tt{not_fails}: significa que la llamada al predicado con la entrada @var{Llamada} siempre tendrá al menos una solución.
@item @tt{fails}: significa que la llamada al predicado con la entrada @var{Llamada} siempre fallará.
@end{itemize}

@subsection{Lanzar los test automáticamente}
Para lanzar los test, selecciona en el menú de emacs @tt{CiaoDbg -> Run tests in current module}.

 ").

:- prop operacion(Op) #"@var{Op} es una operacion aceptada por la calculadora. @includedef{operacion/1}".
operacion('+').
operacion('-').

:- test calcular(Op,A,B,C) : (Op = '+', A = 0, B = 0) => (C = 0) + not_fails #"Caso base".
:- test calcular(Op,A,B,C) : (Op = '+', A = s(s(0)), B = s(0)) => (C = s(s(s(0)))) + not_fails.
:- test calcular(Op,A,B,C) : (Op = '-', A = 0, B = s(0)) + fails #"El resultado sólo puede ser un número negativo.".

:- pred calcular(Op,A,B,C)
   #"@var{C} es el resultado de aplicar la operacion @var{Op} a @var{A} y @var{B}. @includedef{calcular/4}".
calcular('+',A,B,C) :-
    suma(A,B,C).
calcular('-',A,B,C) :-
    suma(B,C,A).

:- pred suma(A,B,C)
   #"@var{C} es la suma de @var{A} y @var{B} en formato peano. @includedef{suma/3}".
suma(0,X,X) :- nat(X).
suma(s(X),Y,s(Z)) :-
    suma(X,Y,Z).

:- prop nat/1 #"Numero natural. @includedef{nat/1}".
nat(0).
nat(s(X)) :-
    nat(X).
