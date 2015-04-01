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
    nth0(C, OldRow, _Val, NewRow),    % we don't care the _Val deleted
    F = _Val.
   % nth0(R, Upd, NewRow, RestRows).   % insert updated row in rest, get Upd matrix

test :-
    L = [[a,b,c,d],
         [e,r,t,y],
         [u,i,o,t]],
    valorEnPos(5,5,L,X,F), %F tiene el valor en esa posicion, primeras
    writeln(F).






%mover(Pieza,I,J,NuevoI,NuevoJ).
mover([H|T],I,J,I2,0):-size(H,Tam),J is Tam-1,I2 is I+1.
mover(Pieza,I,J,I,J2):-J2 is J+1.


%posicionarPieza(Matriz,I1,J1,Pieza,I2,J2).
posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-size(Pieza,Tam),I2 is Tam.

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I1,J1,Pieza,V2),V2==x,V1==x,mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,B is J1+NuevoJ2-J2,writeln(A+B+V1+V2),
	posicionarPieza(Matriz,A,B,Pieza,NuevoI2,NuevoJ2).

posicionarPieza(Matriz,I1,J1,Pieza,I2,J2):-valorEnPos(I1,J1,Matriz,V1),
	valorEnPos(I1,J1,Pieza,V2),V2==o,mover(Pieza,I2,J2,NuevoI2,NuevoJ2),
	A is I1+NuevoI2-I2,B is J1+NuevoJ2-J2,writeln(A+B+V1+V2),
	posicionarPieza(Matriz,A,B,Pieza,NuevoI2,NuevoJ2).





