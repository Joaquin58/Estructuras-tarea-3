module Practica03 where 

data Bit = Cero | Uno deriving Eq

instance Show Bit where
    show Cero = "0"
    show Uno = "1"

compuertaNOT :: Bit -> Bit 
compuertaNOT Cero = Uno
compuertaNOT Uno = Cero

compuertaAND :: Bit -> Bit -> Bit
compuertaAND Uno Uno = Uno
compuertaAND _ _ = Cero

compuertaOR :: Bit -> Bit -> Bit
compuertaOR Cero Cero = Cero 
compuertaOR _ _ = Uno


compuertaNAND :: Bit -> Bit -> Bit
compuertaNAND p q = compuertaNOT (compuertaAND p q)

compuertaNOR :: Bit -> Bit -> Bit
compuertaNOR p q = compuertaNOT (compuertaOR p q)

compuertaXOR :: Bit -> Bit -> Bit
compuertaXOR p q = compuertaOR (compuertaAND p (compuertaNOT q))(compuertaAND q (compuertaNOT p))

compuertaXNOR :: Bit -> Bit -> Bit
compuertaXNOR p q = compuertaOR (compuertaAND p q)(compuertaAND (compuertaNOT p)(compuertaNOT q))

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder x y = (compuertaXOR x y, compuertaAND x y)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y z = (compuertaXOR (compuertaXOR x y) z, compuertaOR (compuertaAND x y)(compuertaAND z (compuertaXOR x y)))

-- Implementación del Full-Adder utilizando Half-Adders 
-- fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit) 
-- fullAdder x y cin = (s, cout) 
--     where 
--         (s1, c1) = halfAdder x y 
--         (s, c2) = halfAdder s1 cin 
--         cout = compuertaOR c1 c2

flipFlopD :: Bit -> Bit -> Bit
flipFlopD _ d = d

flipFlopJK :: Bit -> Bit -> Bit -> Bit
flipFlopJK q j k = compuertaOR(compuertaAND j (compuertaNOT q))(compuertaAND (compuertaNOT k) q)

flipFlopT :: Bit -> Bit -> Bit
flipFlopT q t = compuertaXOR q t

data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred |
            Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred |
            Existe Nombre Pred deriving Eq

data Term = Var Nombre | Fun Nombre [Term] deriving Eq

type Nombre = String

instance Show Term where
    show (Var n) = n
    show (Fun f l) = f ++ "(" ++ show l ++ ")"


instance Show Pred where
  show PTrue = "True"                                       -- T
  show PFalse = "False"                                     -- F
  show (Predicado n t) = n ++ "(" ++ show t ++ ")"          -- P|Q|R 
  show (Neg p) = "¬" ++ show p                              -- ¬(P)
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)
  show (PTodo n p) = "∀" ++ n ++ " " ++ show p              -- ∀n (P)
  show (Existe n p) = "∃" ++ n ++ " " ++ show p             -- ∃n (P)

variables :: Pred -> [Nombre]
variables PTrue = []
variables PFalse = []
variables (Predicado _ term) = auxiliar term
variables (Neg x) = variables x
variables (Conj x y) = variables x ++ variables y
variables (Disy x y) = variables x ++ variables y
variables (Impl x y) = variables x ++ variables y
variables (Syss x y) = variables x ++ variables y
variables (PTodo _ p) = variables p
variables (Existe _ p) = variables p

auxiliar :: [Term] -> [Nombre]
auxiliar [] = []
auxiliar (Var n : ts) = n : auxiliar ts
auxiliar (Fun _ x : rs) = auxiliar x ++ auxiliar rs


auxiliar_repetidos :: [Nombre] -> [Nombre]
auxiliar_repetidos [] = []
auxiliar_repetidos (x:xs) =
    if x `elem` xs
    then auxiliar_repetidos xs  
    else x : auxiliar_repetidos xs 

removeVar :: Nombre -> [Nombre] -> [Nombre] 
removeVar _ [] = [] 
removeVar n (x:xs) 
    | n == x = removeVar n xs 
    | otherwise = x : removeVar n xs

-- collectVarsInTerms :: [Term] -> [Nombre] 
-- collectVarsInTerms = concatMap collectVarsInTerm

-- collectVarsInTerm :: Term -> [Nombre] 
-- collectVarsInTerm (Var n) = [n] 
-- collectVarsInTerm (Fun _ ts) = collectVarsInTerms ts

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

aux :: Term -> Int
aux (Var x) = 0 
aux (Fun x y) =sum (map aux y)


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
