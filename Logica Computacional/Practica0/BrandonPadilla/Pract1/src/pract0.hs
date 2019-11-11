{-
Lógica computacional 2017-1
         Noé Salomón Hernández Sánchez
         Albert M. Orozco Camacho
         C. Moisés Vázquez Reyes
         Diego Murillo
-}

--Brandon Padilla Ruiz, 312139805
import Data.List(words)
data Nat = Cero | S Nat deriving Show

miCuenta::Int
miCuenta = 312139805

suma::Nat->Nat->Nat
suma Cero n = n
suma (S m) n = S (suma m n)

prod::Nat->Nat->Nat
prod Cero _ = Cero							
prod (S m) n = suma (prod m n) n 

mayorQue::Nat->Nat->Bool
mayorQue Cero Cero = False
mayorQue Cero _ = False
mayorQue _ Cero = True
mayorQue (S m) (S n) = mayorQue m n 

igual::Nat->Nat->Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (S m) (S n) = igual m n


power::Int->Int->Int
power _ 0 = 1	
power m 1 = m
power m n = m*(power m (n-1))


power2::Int->Int->Int
power2 _ 0 = 1
power2 n 2 = n*n
power2 n m = if ((even m) == True)
				then power2 (power2 n 2) (div m 2)
				else n*(power2 n (m-1))

reversa::[a]->[a]
reversa [] = []		
reversa (a:as) = reversa (as) ++ [a]

sumal::[Int]->Int
sumal [] = 0
sumal (a:as) = a + sumal (as)



toma::Int->[a]->[a]
toma 0 _ = []
toma _ [] = []
toma n (a:as) = [a] ++ (toma (n-1) as)



tira::Int->[a]->[a]
tira 0 (a:as) = (a:as)
tira _ [] = []
tira n (a:as) = tira (n-1) as

cuantas::Eq a=>a->[a]->Int
cuantas _ [] = 0
cuantas x (a:as) = if x == a 
					then 1 + (cuantas x as) 
					else 0 + (cuantas x as)


--Funcion que elimina el resto de las presencias de un elemento en una lista.
elimPres::Eq a=>a->[a]->[a]
elimPres _ [] = []
elimPres x (a:as) = if x == a
					then elimPres x as
					else [a] ++ elimPres x as


frec::Eq a=>[a]->[(a, Int)]
frec [] = []
frec (a:as) = [(a,1 + cuantas a as)] ++ frec (elimPres a as)

unaVez::Eq a=>[a]->[a]
unaVez [] = []
unaVez (a:as) = if (cuantas a as < 1)
				then [a] ++ unaVez as
				else unaVez (elimPres a as)

 

compress1::String->String
compress1 = error "Te toca"


compress2::String->String
compress2 = error "Te toca"


juego::(Int,Int)->(Int,Int)
juego = error "Te toca"
