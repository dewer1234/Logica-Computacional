




%Yo soy mi propio abuelo.
%Protagonista : Ruben

conyuge(ruben,maria).
conyuge(gustavo,andrea).
madre(maria,andrea).
madre(andrea,ruben).
madre(andrea,karla).
madre(maria,pedro).
padre(gustavo,ruben).
padre(ruben,andrea).





suegro(X,Y) :- conyuge(Z,Y),padre(X,Z).
suegro(X,Y) :- conyuge(Y,Z),padre(X,Z).
suegro(X,Y) :- conyuge(Z,Y),madre(X,Z).
suegro(X,Y) :- conyuge(Y,Z),madre(X,Z).

hermano(X,Y) :- padre(Z,X),padre(Z,Y).
hermano(X,Y) :- madre(Z,X),madre(Z,Y).

cun(X,Y) :- conyuge(X,Z),hermano(Z,Y).
cun(X,Y) :- conyuge(Y,Z),hermano(X,Z).

tio(X,Y) :- padre(Z,Y),hermano(X,Z).
tio(X,Y) :- padre(Z,Y),hermano(Z,X).
tio(X,Y) :- madre(Z,Y),hermano(X,Z).
tio(X,Y) :- madre(Z,Y),hermano(Z,X).


abuelo(X,Y) :- padre(X,Z),padre(Z,Y).
abuelo(X,Y) :- madre(X,Z),madre(Z,Y).
abuelo(X,Y) :- padre(X,Z),madre(Z,Y).


%Libros de Albert


nombres([donald,jordi,michael,stephen,frank]).
apellidos([miller,rosado,knuth,spivak,king]).
precios([2,3,4,5,6]).

permutacion([],[]).
permutacion([H|T],R) :- member(H,R),select(H,R,Rez),permutacion(T,Rez).

solucion(Sol):-Sol=[[Nom1,Apell1,Precio1], [Nom2,Apell2,Precio2],
               [Nom3,Apell3,Precio3],[Nom4,Apell4,Precio4],
               [Nom5,Apell5,Precio5]],
               nombres(Nom),apellidos(Apell),precios(Prec),
               permutacion(Nom,[Nom1,Nom2,Nom3,Nom4,Nom5]),
               permutacion(Apell,[Apell1,Apell2,Apell3,Apell4,Apell5]),
               permutacion(Prec,[Precio1,Precio2,Precio3,Precio4,Precio5]),
               member([stephen,king,_],Sol),%Stephen King es uno de los autores.
               member([donald,_,N],Sol),member([_,spivak,M],Sol), N is M+1,
               member([frank,_,K],Sol), M is K+1,member([_,knuth,L],Sol),member([jordi,_,O],Sol), L is O+3,
               member([michael,_,H],Sol), member([_,rosado,J],Sol), H is J*2.



 




