:- style_check(-singleton).
:- use_module(library(clpfd)).

%size se encarga de devolver el tamaño de una lista
size([],0).
size([H|T],N):-
	size(T,N1)
	,N is N1+1.

%rotar90 se encarga de rotar la matriz 90 grados.
rotar90(L,R):-
	transpose(L,R1)
	,reverse(R1,R).

%Retorna el valor que existe en alguna posicion de la lista.
valorEnPos(R, C, Mat,F) :-
    nth0(R, Mat, OldRow, RestRows),   % get the row and the rest
    nth0(C, OldRow, _Val, NewRow),
    F = _Val.


%reemplaza un valor dentro de la lista.
replace([_|T], 0, X, [X|T]).

replace([H|T], I, X, [H|R]):-
	I > -1,
	NI is I-1,
	replace(T, NI, X, R), !.

replace(L, _, _, L).

% Busca con un indice una posicion de la lista para reemplazarla por un
% "O".
cambiarValorPorOenPos(R,C,Mat,Upd) :-
    nth0(R, Mat, OldRow, RestRows),% obtiene la fila
    replace(OldRow,C,o,P), %Cambia el valor en esa columna y le asigna a P la nueva lista.
    nth0(R, Upd,P,RestRows). %Vuelva a montar la matriz.

% Funcion encargada de buscar la primera aparicion de un "X" en la
% matriz para iniciar el proceso de resolucion.
primApX(O1,O2,[H|T],I,J):-
	H == [],
	primApX(O1+1,0,T,I,J).

primApX(O1,O2,[[H|T1]|T2],I,J):-
	H \= x,
	primApX(O1,O2+1,[T1|T2],I,J).

primApX(O1,O2,[[H|T1]|T2],I,J):-
	H == x,
	I is O1,
	J is O2.

%mover retorna la posicion del valor "X" mas proximo.
%mover(Pieza,I,J,NuevoI,NuevoJ).
mover([H|T],I,J,I2,0):-
	size(H,Tam),
	J is Tam-1,
	I2 is I+1.

mover([H|T],I,J,I,J2):-
	size(H,Tam),
	J2 is J+1,
	J2<Tam.

% posicionarPieza posiciona la pieza en la matriz de la figura, si logra
% acomodar la pieza se retornan los grados de la pieza y en la matriz de
% la figura se sustituyen las
% posiciones "X" por 0.
%posicionarPieza(Matriz,I1,J1,Pieza,I2,J2).
posicionarPieza(Matriz,I1,J1,Pieza,I2,J2,Matriz):-
	size(Pieza,Tam),
	I2 is Tam.

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2,Mat2):-
	valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I2,J2,Pieza,V2),
	V2==x,
	V1==x,
	mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,
	A > -1,
	B is J1+NuevoJ2-J2,
	B > -1,
	cambiarValorPorOenPos(I1,J1,Matriz,M2),
	posicionarPieza(M2,A,B,Pieza,NuevoI2,NuevoJ2,Mat2).

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2,Mat2):-
	valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I2,J2,Pieza,V2),
	V2==o,
	mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,
	A > -1,
	B is J1+NuevoJ2-J2,
	B > -1,
	posicionarPieza(Matriz,A,B,Pieza,NuevoI2,NuevoJ2,Mat2).



%Posiciona y rota la pieza
%pyrPieza(Matriz,Pieza,Grados,Contador).
pyrPieza(M,P,0,N,M2,I1,J1):-
	primApX(0,0,M,I1,J1),
	primApX(0,0,P,I2,J2),
	posicionarPieza(M,I1,J1,P,I2,J2,M2).

pyrPieza(M,P,G,N,M2,X,Y):-
	N<3,
	rotar90(P,P1),
	pyrPieza(M,P1,G1,N+1,M2,X,Y),
	G is G1+90.

%Envia al final el primer elemento de una lista.
enviarAlFinal([H|T],M2):-
	reverse(T,T2),
	reverse([H|T2],M2).

%Es nula comprueba si una lista y/o matriz es nula.
esNula([]).
esNula([H|T]):-
	H==[],
	esNula(T).

esNula([[H|T1]|T2]):-
	H==o,
	esNula([T1|T2]).

%El mismo proceso que pyrPieza
%pyrPiezas(Matriz,Piezas,Temp,Sol).
pyrPiezas(M,[],Temp,Temp2):-esNula(M),reverse(Temp,Temp2).
pyrPiezas(M,[[Nom,Pieza|T]|T2],Temp,Sol):-
	pyrPieza(M,Pieza,G,0,M2,X,Y),
	pyrPiezas(M2,T2,[[Nom,G,X,Y]|Temp],Sol).


%figuraAux(Matriz,Piezas,Cont,Sol).
figuraAux(M,P,C,Sol):-
	pyrPiezas(M,P,[],Sol).

figuraAux(M,P,C,Sol):-
	size(P,Tam),
	C<Tam,
	enviarAlFinal(P,P2),
	figuraAux(M,P2,C+1,Sol).


%Funcion principal de entrada del programa.
figura(M,P,S):-figuraAux(M,P,0,S).




















