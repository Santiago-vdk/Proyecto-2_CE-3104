:- style_check(-singleton).
:- use_module(library(clpfd)).


:- L=[perro,gato,raton,queso].
pertenece(E,L):-L=[E|_].
pertenece(E,[_|T]):-pertenece(E,T).


:-pertenece(E,[a,b,c,d,e]).
perros(pastor_aleman, [juli, esteban, pancho]).
perros(san_bernardo, [master, rigan, mujamad]).
perros(french_poodle, [figaro, piojo, ramiro]).

size([],0).
size([H|T],N):-size(T,N1),N is N1+1.

rotar90(L,R):-transpose(L,R1),reverse(R1,R).

comparar(M1,M2,0):-M1 == M2.
comparar(M1,M2,G):-
	rotar90(M1,R),
	comparar(R,M2,G1),
	G is G1+90,
	G<360.



valorEnPos(R, C, Mat,F) :-
    nth0(R, Mat, OldRow, RestRows),   % get the row and the rest
    nth0(C, OldRow, _Val, NewRow),
    F = _Val.



replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):-
	I > -1,
	NI is I-1,
	replace(T, NI, X, R), !.

replace(L, _, _, L).


cambiarValorPorOenPos(R,C,Mat,Upd) :-
    nth0(R, Mat, OldRow, RestRows),% obtiene la fila
    replace(OldRow,C,o,P), %Cambia el valor en esa columna y le asigna a P la nueva lista.
    nth0(R, Upd,P,RestRows). %Vuelva a montar la matriz.


indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

primeraAparicionX(I,J,M,F) :-
	M \= [].

primeraAparicionX(I,J,[H|T],F):-
	size(M,W),
	I < W,
	indexOf(H,x,F).

primeraAparicionX(I,J,[H|T],F):-
	primeraAparacion(I2,J,T,F),
	I is I2 + 1.

test1 :-
    L = [[1,2,3],
	 [4,5,6],
	 [7,8,9]],
    nth0(I,L,Loquellevo,Loquesobra),
    writeln(I),
    writeln(Loquellevo),
    indexOf(Loquellevo,2,P), %Busca el valor y asigna a P la posicion de fila
    writeln(P),
    writeln(Loquesobra).
    %nth0(J,Loquellevo,

test :-
    L = [[a,b,c,d],
         [e,r,t,y],
         [u,i,x,t]],


    valorEnPos(2,2,L,K), %Reviso que valor hay en esa posicion
    writeln(K),
    cambiarValorPorOenPos(2,2,L,X), %F tiene el valor en esa posicion, cambio el valor y devuelvo la matriz X
    writeln(X).






%mover(Pieza,I,J,NuevoI,NuevoJ).
mover([H|T],I,J,I2,0):-size(H,Tam),J is Tam-1,I2 is I+1.
mover(Pieza,I,J,I,J2):-J2 is J+1.


%posicionarPieza(Matriz,I1,J1,Pieza,I2,J2).
posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-size(Pieza,Tam),I2 is Tam.

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-
	valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I1,J1,Pieza,V2),
	V2==x,
	V1==x,
	mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,
	B is J1+NuevoJ2-J2,
	posicionarPieza(Matriz,A,B,Pieza,NuevoI2,NuevoJ2).

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-
	valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I1,J1,Pieza,V2),
	V2==o,
	mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,
	B is J1+NuevoJ2-J2,
	posicionarPieza(Matriz,A,B,Pieza,NuevoI2,NuevoJ2).




%pyrPieza(Matriz,Pieza,Grados,Contador).
pyrPieza(M,P,0,N):-
	valorEnPos(I1,J1,M,x),
	valorEnPos(I2,J2,P,x),
	posicionarPieza(M,I1,J1,P,I2,J2).

pyrPieza(M,P,G,N):-
	N<4,
	rotar(P,P1),
	pyrPieza(M,P1,G1,N+1),
	G is G1+90.

















