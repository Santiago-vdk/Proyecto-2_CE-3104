:- style_check(-singleton).


:- L=[perro,gato,raton,queso].
pertenece(E,L):-L=[E|_].
pertenece(E,[_|T]):-pertenece(E,T).


:-pertenece(E,[a,b,c,d,e]).
perros(pastor_aleman, [juli, esteban, pancho]).
perros(san_bernardo, [master, rigan, mujamad]).
perros(french_poodle, [figaro, piojo, ramiro]).

size([],0).
size([H|T],N):-size(T,N1),N is N1+1.
