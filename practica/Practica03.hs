data Bit = Cero | Uno deriving Eq
data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred |
            Disy Pred Pred | Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred |
            Existe Nombre Pred

data Term = Var Nombre | Fun Nombre [Term]

data Nombre = String

instance Show Bit where
    show Cero = "0"
    show Uno = "1"

instance Show Pred where
    show PTrue = "True"
    show PFalse = "False"
    -- show Predicado = Nombre

instance Show Term where
    show (Var n) = n
--     show (Fun f l) = f ++ "(" ++ show l ++ ")"
 
compuertaAND :: Bit -> Bit -> Bit
compuertaAND Uno Uno = Uno
compuertaAND _ _ = Cero

compuertaOR :: Bit -> Bit -> Bit
compuertaOR Cero Cero = Cero 
compuertaOR _ _ = Uno

compuertaNOT :: Bit -> Bit -> Bit
compuertaNOT Cero = Uno
compuertaNOT Uno = Cero

vars :: Pred -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Predicado _ term) = auxiliar term
vars (Conj x y) = vars x ++ vars y
vars (PTodo _ p) = vars p

auxiliar :: Term -> [Nombre]
auxiliar (Var x) = [x]
auxiliar (Fun _ [x]) = concatMap (auxiliar x)

auxiliar_repetidos :: [String] -> [String]
auxiliar_repetidos [] = []
auxiliar_repetidos (x:xs) =
    if x `elem` xs
    then auxiliar_repetidos xs  
    else x : auxiliar_repetidos xs 

libres :: Pred -> [Nombre]
libres PTrue = []
libres PFalse = []
libres (Predicado n t) = concatMap auxiliar t
libres (Neg x) = libres x
libres (Conj x y) = libres x ++ libres y
libres (PTodo n y) = if n `elem` (var y) then remove n y
                                        else y
equiv :: Pred -> Pred
equiv PTrue = PTrue
equiv PFalse = PFalse
equiv (Predicado n x) = Predicado n x
equiv (Neg x) = Neg (equiv x)
equiv (Neg (PTodo x p)) = Existe x (equiv (Neg p))
equiv (Neg (Existe x p)) = PTodo x (equiv (Neg p))

cuantificadores :: Pred -> Int
cuantificadores PTrue = 0
cuantificadores PFalse = 0
cuantificadores (Predicado x y) = cocat (aux y)
cuantificadores (Neg x) = cuantificadores p
cuantificadores (Conj x y) = cuantificadores x ++ cuantificadores y
cuantificadores (PTodo x y) = 1 + cuantificadores y
cuantificadores (Existe x y) = 1 + cuantificadores y

aux :: Term -> Int
aux (Var x) = 0 
aux (Fun x y) = aux y  