module Practica03 where
import Data.List (nub)

------------------------------------------------------------------------------------
-------------------------            PRACTICA 3            -------------------------
-------------------------            CIRCUITOS             -------------------------
------------------------------------------------------------------------------------

-- Definición de bits
data Bit = Cero | Uno deriving (Eq, Show)

-- Ejercicio 1: Compuerta lógica AND
compuertaAND :: Bit -> Bit -> Bit
compuertaAND Uno Uno = Uno
compuertaAND _ _ = Cero

-- Ejercicio 2: Compuerta lógica OR
compuertaOR :: Bit -> Bit -> Bit
compuertaOR Cero Cero = Cero
compuertaOR _ _ = Uno

-- Ejercicio 3: Compuerta lógica NOT
compuertaNOT :: Bit -> Bit
compuertaNOT Cero = Uno
compuertaNOT Uno = Cero

-- Ejercicio 4: Compuerta lógica NAND
compuertaNAND :: Bit -> Bit -> Bit
compuertaNAND x y = compuertaNOT (compuertaAND x y)

-- Ejercicio 5: Compuerta lógica NOR
compuertaNOR :: Bit -> Bit -> Bit
compuertaNOR x y = compuertaNOT (compuertaOR x y)

-- Ejercicio 6: Compuerta lógica XOR
compuertaXOR :: Bit -> Bit -> Bit
compuertaXOR x y = if x == y then Cero else Uno

-- Ejercicio 7: Compuerta lógica XNOR
compuertaXNOR :: Bit -> Bit -> Bit
compuertaXNOR x y = compuertaNOT (compuertaXOR x y)

-- Ejercicio 8: Half-Adder (medio sumador)
halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder x y = (compuertaXOR x y, compuertaAND x y)

-- Ejercicio 9: Full-Adder (sumador completo)
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y cin = (suma, acarreo)
  where
    (s1, c1) = halfAdder x y
    (suma, c2) = halfAdder s1 cin
    acarreo = compuertaOR c1 c2

-- Ejercicio 10: Flip-flop RS
flipFlopRS :: Bit -> Bit -> Bit -> Bit
flipFlopRS Uno Uno _ = error "Estado inválido en flip-flop RS"
flipFlopRS Uno Cero _ = Uno
flipFlopRS Cero Uno _ = Cero
flipFlopRS Cero Cero q = q

-- Ejercicio 11: Flip-flop D
flipFlopD :: Bit -> Bit -> Bit
flipFlopD _ d = d

-- Ejercicio 12: Flip-flop JK
flipFlopJK :: Bit -> Bit -> Bit -> Bit
flipFlopJK q Cero Cero = q
flipFlopJK _ Cero Uno = Cero
flipFlopJK _ Uno Cero = Uno
flipFlopJK q Uno Uno = compuertaNOT q

-- Ejercicio 13: Flip-flop T
flipFlopT :: Bit -> Bit -> Bit
flipFlopT q Uno = compuertaNOT q
flipFlopT q Cero = q

------------------------------------------------------------------------------------
-------------------------         LÓGICA DE PRIMER ORDEN       ---------------------
------------------------------------------------------------------------------------

type Nombre = String

data Term = Var Nombre | Fun Nombre [Term] deriving (Eq, Show)

data Pred
  = PTrue
  | PFalse
  | Predicado Nombre [Term]
  | Neg Pred
  | Conj Pred Pred
  | Disy Pred Pred
  | Impl Pred Pred
  | Syss Pred Pred
  | PTodo Nombre Pred
  | Existe Nombre Pred
  deriving (Eq, Show)

-- Ejercicio 14: Variables de una fórmula predicativa
variables :: Pred -> [Nombre]
variables PTrue = []
variables PFalse = []
variables (Predicado _ terms) = nub $ concatMap variablesTerm terms
  where
    variablesTerm (Var n) = [n]
    variablesTerm (Fun _ ts) = concatMap variablesTerm ts
variables (Neg p) = variables p
variables (Conj p1 p2) = nub $ variables p1 ++ variables p2
variables (Disy p1 p2) = nub $ variables p1 ++ variables p2
variables (Impl p1 p2) = nub $ variables p1 ++ variables p2
variables (Syss p1 p2) = nub $ variables p1 ++ variables p2
variables (PTodo n p) = filter (/= n) (variables p)
  variables (Existe n p) = filter (/= n) (variables p)

-- Ejercicio 15: Variables libres
variablesLibres :: Pred -> [Nombre]
variablesLibres PTrue = []
variablesLibres PFalse = []
variablesLibres (Predicado _ terms) = nub $ concatMap variablesTerm terms
  where
    variablesTerm (Var n) = [n]
    variablesTerm (Fun _ ts) = concatMap variablesTerm ts
variablesLibres (Neg p) = variablesLibres p
variablesLibres (Conj p1 p2) = nub $ variablesLibres p1 ++ variablesLibres p2
variablesLibres (Disy p1 p2) = nub $ variablesLibres p1 ++ variablesLibres p2
variablesLibres (Impl p1 p2) = nub $ variablesLibres p1 ++ variablesLibres p2
variablesLibres (Syss p1 p2) = nub $ variablesLibres p1 ++ variablesLibres p2
variablesLibres (PTodo x p) = filter (/= x) (variablesLibres p)
variablesLibres (Existe x p) = filter (/= x) (variablesLibres p)

-- Ejercicio 16: Variables ligadas
variablesLigadas :: Pred -> [Nombre]
variablesLigadas PTrue = []
variablesLigadas PFalse = []
variablesLigadas (Predicado _ _) = []
variablesLigadas (Neg p) = variablesLigadas p
variablesLigadas (Conj p1 p2) = nub $ variablesLigadas p1 ++ variablesLigadas p2
variablesLigadas (Disy p1 p2) = nub $ variablesLigadas p1 ++ variablesLigadas p2
variablesLigadas (Impl p1 p2) = nub $ variablesLigadas p1 ++ variablesLigadas p2
variablesLigadas (Syss p1 p2) = nub $ variablesLigadas p1 ++ variablesLigadas p2
variablesLigadas (PTodo x p) = nub $ x : variablesLigadas p
variablesLigadas (Existe x p) = nub $ x : variablesLigadas p

-- Ejercicio 17: Equivalencia de negación de cuantificadores
equivalenciaNeg :: Pred -> Pred
equivalenciaNeg (Neg (PTodo x p)) = Existe x (equivalenciaNeg (Neg p))
equivalenciaNeg (Neg (Existe x p)) = PTodo x (equivalenciaNeg (Neg p))
equivalenciaNeg (Neg (Neg p)) = equivalenciaNeg p
equivalenciaNeg (Neg (Conj p1 p2)) = Disy (equivalenciaNeg (Neg p1)) (equivalenciaNeg (Neg p2))
equivalenciaNeg (Neg (Disy p1 p2)) = Conj (equivalenciaNeg (Neg p1)) (equivalenciaNeg (Neg p2))
equivalenciaNeg (Neg (Impl p1 p2)) = Conj (equivalenciaNeg p1) (Neg (equivalenciaNeg p2))
equivalenciaNeg (Neg (Syss p1 p2)) = Disy (Conj (equivalenciaNeg p1) (Neg (equivalenciaNeg p2))) (Conj (Neg (equivalenciaNeg p1)) (equivalenciaNeg p2))
equivalenciaNeg (Conj p1 p2) = Conj (equivalenciaNeg p1) (equivalenciaNeg p2)
equivalenciaNeg (Disy p1 p2) = Disy (equivalenciaNeg p1) (equivalenciaNeg p2)
equivalenciaNeg (Impl p1 p2) = Impl (equivalenciaNeg p1) (equivalenciaNeg p2)
equivalenciaNeg (Syss p1 p2) = Syss (equivalenciaNeg p1) (equivalenciaNeg p2)
equivalenciaNeg p = p


-- Ejercicio 18: Contar cuantificadores
cuantificadores :: Pred -> Int
cuantificadores (PTodo _ p) = 1 + cuantificadores p
cuantificadores (Existe _ p) = 1 + cuantificadores p
cuantificadores (Neg p) = cuantificadores p
cuantificadores (Conj p1 p2) = cuantificadores p1 + cuantificadores p2
cuantificadores (Disy p1 p2) = cuantificadores p1 + cuantificadores p2
cuantificadores (Impl p1 p2) = cuantificadores p1 + cuantificadores p2
cuantificadores (Syss p1 p2) = cuantificadores p1 + cuantificadores p2
cuantificadores _ = 0

-- Ejercicio 19: Contar conectivos lógicos
conectivos :: Pred -> Int
conectivos PTrue = 0
conectivos PFalse = 0
conectivos (Predicado _ _) = 0
conectivos (Neg p) = 1 + conectivos p
conectivos (Conj p1 p2) = 1 + conectivos p1 + conectivos p2
conectivos (Disy p1 p2) = 1 + conectivos p1 + conectivos p2
conectivos (Impl p1 p2) = 1 + conectivos p1 + conectivos p2
conectivos (Syss p1 p2) = 1 + conectivos p1 + conectivos p2
conectivos (PTodo _ p) = conectivos p
conectivos (Existe _ p) = conectivos p
