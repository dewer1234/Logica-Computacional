{-
   Lógica computacional 2017-1
      Noé Salomón Hernández Sánchez
      Albert M. Orozco Camacho
      C. Moisés Vázquez Reyes
      Diego Murillo Albarrán
      José Roberto Piche Limeta

   Práctica 1
-}

--INTERPRETACIONES
--Brandon Padilla Ruiz, 312139805, brandon.padilla.r@ciencias.unam.mx
import Data.List
data Form =  Conj Form Form | Disy Form Form | Imp Form Form | DImp Form Form | Neg Form | Var String | T | F deriving (Show,Eq,Ord)
type Estado = [String]

miCuenta::Int
miCuenta = 312139805

interp::Estado->Form->Bool 
interp s T = True
interp s F = False
interp s (Neg x) = not (interp s x)
interp s (Var x) = elem x s
interp s (Conj x y) = interp s x && interp s y
interp s (Disy x y) = interp s x || interp s y
interp s (Imp x y) = interp s (Neg x) || interp s y
interp s (DImp x y) = interp s x == interp s y


vars::Form->[String]
vars T = []
vars F = []
vars (Var x) = [x]
vars (Neg x) = vars x
vars (Conj x y)  = nub((vars x) ++ (vars y))
vars (Disy x y) = nub((vars x) ++ (vars y))
vars (Imp x y) = nub((vars x) ++ (vars y))
vars (DImp x y) = nub((vars x) ++ (vars y))

potencia::[a]->[[a]]
potencia [] = [[]]
potencia (x:xs) = potencia xs ++ [x:t | t <- potencia xs]


estados::Form->[Estado]
estados T = []
estados F = []
estados (Var x) = potencia [x]
estados (Neg x) = potencia (vars(Neg x))
estados (Conj x y) = potencia (vars(Conj x y))
estados (Disy x y) = potencia (vars(Disy x y))
estados (Imp x y) = potencia (vars(Imp x y))
estados (DImp x y) = potencia (vars(DImp x y))

--Función auxiliar para aplicar interp a cada elemento de una lista.
aplicaF::[Estado]->Form->Bool
aplicaF [] x = True
aplicaF (a:as) x = interp a x && aplicaF as x
 
tautologia::Form->Bool
tautologia x = aplicaF (estados x) x


--TABLEUAX SEMÁNTICOS

data Tableaux = Void | R1 [Form] Tableaux | R2 Tableaux Tableaux | Tache | Bolita deriving Show
   

creaTableaux::[Form]->Tableaux
creaTableaux = error "Te toca"
                                                                  
cerrado::Tableaux->Bool
cerrado = error "Te toca"  
                                                            

tautologia_tableaux::Form->Bool
tautologia_tableaux = error "Te toca"

--Algunas fórmulas de prueba. 
--Puedes comprobar que f <--> f1 <--> f2
--Puedes comprobar que g <--> g1 <--> g2


f = Disy (Neg $ Imp (Var "w") (Var "e")) (Neg $ Disy (DImp (Neg $ Var "s") (Var "w")) (Conj (Var "e") (Var "s")))
f1 = Conj (Conj (Conj (Conj (Disy (Disy (Neg $ Var "e") (Var "s")) (Neg $ Var "w")) (Disy (Neg $ Var "s") (Var "w"))) (Disy (Disy (Neg $ Var "e") (Neg $ Var "s")) (Var "w"))) (Disy (Disy (Var "w") (Neg $ Var "e")) (Neg $ Var "s"))) (Disy (Neg $ Var "e") (Neg $ Var "s"))                                                             
f2 = Disy (Disy (Disy (Conj (Var "w") (Neg $ Var "e")) (Conj (Conj (Neg $ Var "w") (Neg $ Var "s")) (Neg $ Var "e"))) (Conj (Neg $ Var "w") (Neg $ Var "s"))) (Conj (Conj (Var "s") (Var "w")) (Neg $ Var "e"))

g = Disy (Neg $ Imp (Var "w") (Var "e")) (Neg $ Disy (DImp (Neg $ Var "s") (Var "w")) (Conj (Var "e") (Var "s")))
g1 = Conj (Conj (Conj (Conj (Disy (Var "w") (Neg $ Var "s")) (Disy (Disy (Neg $ Var "e") (Neg $ Var "s")) (Var "w"))) (Disy (Disy (Neg $ Var "e") (Neg $ Var "w")) (Var "s"))) (Disy (Disy (Var "w") (Neg $ Var "e")) (Neg $ Var "s"))) (Disy (Neg $ Var "e") (Neg $ Var "s"))
g2 = Disy (Disy (Disy (Conj (Var "w") (Neg $ Var "e")) (Conj (Conj (Neg $ Var "s") (Neg $ Var "w")) (Neg $ Var "e"))) (Conj (Neg $ Var "s") (Neg $ Var "w"))) (Conj (Conj (Var "w") (Var "s")) (Neg $ Var "e"))





