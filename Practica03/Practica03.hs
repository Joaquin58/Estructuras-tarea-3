-- UNIVERSIDAD NACIONAL AUTÓNOMA DE MEXICO
-- PRÁCTICA 03: Circuitos y lógica de primer orden
-- Ramírez Mendoza Joaquín Rodrigo
-- Treviño Puebla Héctor Jerome
-- Villalobos Juárez Gontran Eliut

------------------------------------------------------------------------------------
-------------------------            PRACTICA 3            -------------------------
------------------------------------------------------------------------------------

module Practica03 where 

------------------------------------------------------------------------------------
-------------------------        CIRCUITOS LÓGICOS         -------------------------
------------------------------------------------------------------------------------

data Bit = Cero | Uno deriving Eq

instance Show Bit where
    show Cero = "0"
    show Uno = "1"

--CompuertaNOT: 
--Simula el comportamiento (Cambia el valor de la Entrada)
compuertaNOT :: Bit -> Bit 
compuertaNOT Cero = Uno
compuertaNOT Uno = Cero

--CompuertaAND:
--Simula el comportamiento (Uno si ambas entradas son Uno)
compuertaAND :: Bit -> Bit -> Bit
compuertaAND Uno Uno = Uno
compuertaAND _ _ = Cero

--CompuertaOR:
--Simula el comportamiento (Cero si ambas entradas son Cero)
compuertaOR :: Bit -> Bit -> Bit
compuertaOR Cero Cero = Cero 
compuertaOR _ _ = Uno

--CompuertaNAND:
--Simula el comportamiento (Cero si ambas entradas son Uno)
compuertaNAND :: Bit -> Bit -> Bit
compuertaNAND p q = compuertaNOT (compuertaAND p q)

--CompuertaNOR:
--Simula el comportamiento (Uno si ambas entradas son Cero)
compuertaNOR :: Bit -> Bit -> Bit
compuertaNOR p q = compuertaNOT (compuertaOR p q)

--CompuertaXOR:
--Simula el comportamiento (Cero si ambas entradas son Iguales)
compuertaXOR :: Bit -> Bit -> Bit
compuertaXOR p q = compuertaOR (compuertaAND p (compuertaNOT q))(compuertaAND q (compuertaNOT p))

--CompuertaXNOR:
--Simula el comportamiento (Uno si ambas entradas son Iguales)
compuertaXNOR :: Bit -> Bit -> Bit
compuertaXNOR p q = compuertaOR (compuertaAND p q)(compuertaAND (compuertaNOT p)(compuertaNOT q))

--Half-Adder:
--Simula el comportamiento (Genera dos Salidas, la 1ra ocupando la CompuertaXOR de las Entradas, y la 2da la CompuertaAND)
halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder x y = (compuertaXOR x y, compuertaAND x y)

--Full-Adder:
--Simula el comportamiento (Dadas 3 entradas, genera 2 Salidas de la Siguiente forma:)
--1er Salida: Se aplica la Compuerta XOR de las 3 entradas
--2da Salida: Se usa la Compuerta OR al resultado de la Compuerta AND de las primeras entradas con
--el resultado de la Compuerta AND del acarreo y la Compuerta XOR de las primeras 2 entradas 
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y z = (compuertaXOR (compuertaXOR x y) z, compuertaOR (compuertaAND x y)(compuertaAND z (compuertaXOR x y)))

--Flip-Flop D:
--Simula el comportamiento (Regresa el Bit de Entrada dada como Salida)
flipFlopD :: Bit -> Bit -> Bit
flipFlopD _ d = d

--Flip-Flop JK:
--Simula el comportamiento de (Dadas la entradas del Set, Reset y el reloj funciona así):
--Ajustado con compuertas Lógicas, genera el Bit resultante de la Compuerta OR  de la Compuerta AND de el set
--y la negación de el reloj con la Compuerta AND  de ls negación del reset con el reloj. 
flipFlopJK :: Bit -> Bit -> Bit -> Bit
flipFlopJK q j k = compuertaOR(compuertaAND j (compuertaNOT q))(compuertaAND (compuertaNOT k) q)

--Flip-Flop T:
--Simula el comportamiento (Dadas las entradas aplica la Compuerta XOR para generar la Salida)
flipFlopT :: Bit -> Bit -> Bit
flipFlopT q t = compuertaXOR q t


------------------------------------------------------------------------------------
-------------------------         LÓGICA DE PRIMER ORDEN       ---------------------
------------------------------------------------------------------------------------

data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred |
            Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred |
            Existe Nombre Pred deriving Eq

data Term = Var Nombre | Fun Nombre [Term] deriving Eq

type Nombre = String

instance Show Term where
    show (Var n) = n
    show (Fun f l) = f ++ "(" ++ show l ++ ")"


instance Show Pred where
  show PTrue = "True"                                       -- True
  show PFalse = "False"                                     -- False
  show (Predicado n t) = n ++ "(" ++ show t ++ ")"          -- P|Q|R 
  show (Neg p) = "¬" ++ show p                              -- ¬(P)
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)
  show (PTodo n p) = "∀" ++ n ++ " " ++ show p              -- ∀n (P)
  show (Existe n p) = "∃" ++ n ++ " " ++ show p             -- ∃n (P) 

--Variables:
--Dada una Fórmula Predicatva, esta regresa las varibles que hay en ella
--Se implementa la función auxiliar_repetidos para que solo la regrese una vez  
variables :: Pred -> [Nombre]
variables PTrue = []
variables PFalse = []
variables (Predicado _ term) = auxiliar_repetidos (auxiliar term)
variables (Neg x) = auxiliar_repetidos (variables x)
variables (Conj x y) = auxiliar_repetidos (variables x ++ variables y)
variables (Disy x y) = auxiliar_repetidos (variables x ++ variables y)
variables (Impl x y) = auxiliar_repetidos (variables x ++ variables y)
variables (Syss x y) = auxiliar_repetidos (variables x ++ variables y)
variables (PTodo _ p) =  auxiliar_repetidos (variables p)
variables (Existe _ p) = auxiliar_repetidos (variables p)

--Función auxiliar
--Recibe una lista de términos ([Term]) y devuelve una lista con los nombres de las variables.
auxiliar :: [Term] -> [Nombre]
auxiliar [] = []
auxiliar (Var n : ts) = n : auxiliar ts
auxiliar (Fun _ x : rs) = auxiliar x ++ auxiliar rs

--Función auxiliar_repetidos:
--Recibe una lista con nombres de las variables y elimina las que encuentra más de una vez en la lista
auxiliar_repetidos :: [Nombre] -> [Nombre]
auxiliar_repetidos [] = []
auxiliar_repetidos (x:xs) =
    if x `elem` xs
    then auxiliar_repetidos xs  
    else x : auxiliar_repetidos xs 

--Función RemoveVar
--Recibe un nombre  y una lista de nombres, y devuelve una nueva lista con todas las veces que aparecio el 
--recibido eliminadas. 
removeVar :: Nombre -> [Nombre] -> [Nombre] 
removeVar _ [] = [] 
removeVar n (x:xs) 
    | n == x = removeVar n xs 
    | otherwise = x : removeVar n xs

--Variables Libres:
--Dado un Predicado, esta función regresa una lista con las variables que no están relacionadas con algún cuantificador de 
--este mismo predicado
--Usa las funciones auxiliar, auxiliar_repetidos y removeVar
variablesLibres :: Pred -> [Nombre]
variablesLibres PTrue = []
variablesLibres PFalse = []
variablesLibres (Predicado _ t) = auxiliar t
variablesLibres (Neg x) = variablesLibres x
variablesLibres (Conj x y) = auxiliar_repetidos (variablesLibres x ++ variablesLibres y)
variablesLibres (Disy x y) = auxiliar_repetidos (variablesLibres x ++ variablesLibres y)
variablesLibres (Impl x y) = auxiliar_repetidos (variablesLibres x ++ variablesLibres y)
variablesLibres (Syss x y) = auxiliar_repetidos (variablesLibres x ++ variablesLibres y)
variablesLibres (PTodo n p) = if n `elem` variablesLibres p then removeVar n (variablesLibres p) else variablesLibres p 
variablesLibres (Existe n p) = if n `elem` variablesLibres p then removeVar n (variablesLibres p) else variablesLibres p

--Variables Ligadas:
--Dado un Predicado, esta función regresa una lista con las variables que están relacionadas con algún cuantificador de 
--este mismo predicado
--Usa las función auxiliar_repetidos
variablesLigadas :: Pred -> [Nombre]
variablesLigadas PTrue = []
variablesLigadas PFalse = []
variablesLigadas (Predicado n t) = []
variablesLigadas (Neg x) = variablesLigadas x
variablesLigadas (Conj x y) = auxiliar_repetidos (variablesLigadas x ++ variablesLigadas y)
variablesLigadas (Disy x y) = auxiliar_repetidos (variablesLigadas x ++ variablesLigadas y)
variablesLigadas (Impl x y) = auxiliar_repetidos (variablesLigadas x ++ variablesLigadas y)
variablesLigadas (Syss x y) = auxiliar_repetidos (variablesLigadas x ++ variablesLigadas y)
variablesLigadas (PTodo n p) = n : variablesLigadas p
variablesLigadas (Existe n p) =  n : variablesLigadas p

--Equivalencia de la Negación de una Predicado:
--Dado un Predicado Negado con cuantificadores, este regresa un predicado con su equivalencia
--cambiando los cuantificadores.
equivalenciaNeg :: Pred -> Pred
equivalenciaNeg PTrue = PTrue
equivalenciaNeg PFalse = PFalse
equivalenciaNeg (Predicado n x) = Predicado n x
equivalenciaNeg (Neg (PTodo x p)) = Existe x (equivalenciaNeg (Neg p))
equivalenciaNeg (Neg (Existe x p)) = PTodo x (equivalenciaNeg (Neg p))
equivalenciaNeg (Neg x) = Neg (equivalenciaNeg x)
equivalenciaNeg (Conj x y) = Conj (equivalenciaNeg x) (equivalenciaNeg y)
equivalenciaNeg (Disy x y) = Disy (equivalenciaNeg x) (equivalenciaNeg y)
equivalenciaNeg (Impl x y) = Impl (equivalenciaNeg x) (equivalenciaNeg y)
equivalenciaNeg (Syss x y) = Syss (equivalenciaNeg x) (equivalenciaNeg y)
equivalenciaNeg (PTodo x y) =(PTodo x  (equivalenciaNeg y))
equivalenciaNeg (Existe x y) = (Existe x (equivalenciaNeg y))

--Cuantificadores:
--Dado un predicado, esta función regresa el numero de cuantificadores (∀n, ∃n) que tiene el predicado
cuantificadores :: Pred -> Int
cuantificadores PTrue = 0
cuantificadores PFalse = 0
cuantificadores (Predicado x y) = sum (map aux y)
cuantificadores (Neg x) = cuantificadores x
cuantificadores (Conj x y) = cuantificadores x + cuantificadores y
cuantificadores (Disy x y) = cuantificadores x + cuantificadores y
cuantificadores (Impl x y) = cuantificadores x + cuantificadores y
cuantificadores (Syss x y) = cuantificadores x + cuantificadores y
cuantificadores (PTodo x y) = 1 + cuantificadores y
cuantificadores (Existe x y) = 1 + cuantificadores y

--Función aux:
aux :: Term -> Int
aux (Var x) = 0 
aux (Fun x y) =sum (map aux y)


--Conectivos:
--Recibe un Predicado, regresa el número de conectivos (∧, ∨, →, ↔) que hay en todo el predicado.
conectivos :: Pred -> Int
conectivos PTrue = 0
conectivos PFalse = 0
conectivos (Predicado x p) = 0
conectivos (Neg p) = 1 + conectivos p
conectivos (Conj p q) = 1 + conectivos p + conectivos q
conectivos (Disy p q) = 1 + conectivos p + conectivos q
conectivos (Impl p q) = 1 + conectivos p + conectivos q
conectivos (Syss p q) = 1 + conectivos p + conectivos q
conectivos (PTodo _ p) = conectivos p
conectivos (Existe _ p) = conectivos p


