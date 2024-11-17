module Practica03 where

------------------------------------------------------------------------------------
-------------------------            PRACTICA 3            -------------------------
-------------------------            CIRCUITOS             -------------------------
------------------------------------------------------------------------------------

data Bit = Cero | Uno deriving Eq

instance Show Bit where
  show Cero = "0" -- Apagado
  show Uno = "1"  -- Prendido
  
{-| Ejercicio 1: simula una compuerta lógica "and".
  Ejemplos:
  compuertaAND Cero Cero
  compuertaAND Cero Uno
  compuertaAND Uno Cero
  compuertaAND Uno Uno
-}
-- compuertaAND :: Bit -> Bit -> Bit 


{-| Ejercicio 2: simula una compuerta lógica "or".
  Ejemplos:
  compuertaOR Cero Cero
  compuertaOR Cero Uno
  compuertaOR Uno Cero
  compuertaOR Uno Uno
-}
-- compuertaOR :: Bit -> Bit -> Bit 


{-| Ejercicio 3: simula una compuerta lógica "not".
  Ejemplos:
  compuertaNOT Cero
  compuertaNOT Uno
-}
-- compuertaNOT :: Bit -> Bit  


{-| Ejercicio 4: simula una compuerta lógica "nand".
  Ejemplos:
  compuertaNAND Cero Cero
  compuertaNAND Cero Uno
  compuertaNAND Uno Cero
  compuertaNAND Uno Uno
-}
-- compuertaNAND :: Bit -> Bit -> Bit 


{-| Ejercicio 5: simula una compuerta lógica "nor".
  Ejemplos:
  compuertaNOR Cero Cero
  compuertaNOR Cero Uno
  compuertaNOR Uno Cero
  compuertaNOR Uno Uno
-}
-- compuertaNOR :: Bit -> Bit -> Bit 


{-| Ejercicio 6: simula una compuerta lógica "xor".
  Ejemplos:
  compuertaXOR Cero Cero
  compuertaXOR Cero Uno
  compuertaXOR Uno Cero
  compuertaXOR Uno Uno
-}
-- compuertaXOR :: Bit -> Bit -> Bit 


{-| Ejercicio 7: simula una compuerta lógica "xnor".
  Ejemplos:
  compuertaXNOR Cero Cero
  compuertaXNOR Cero Uno
  compuertaXNOR Uno Cero
  compuertaXNOR Uno Uno
-}
-- compuertaXNOR :: Bit -> Bit -> Bit 




{-| Ejercicio 8: Función para medio sumador (Half-Adder)
  Ejemplos:
  halfAdder Cero Cero
  halfAdder Cero Uno
  halfAdder Uno Cero
  halfAdder Uno Uno
-}
--halfAdder :: Bit -> Bit -> [Bit]
-- halfAdder :: Bit -> Bit -> (Bit, Bit)


{-| Ejercicio 9: Función para sumador completo (Full-Adder)
  Ejemplos:
  fullAdder Cero Cero Cero
  fullAdder Cero Cero Uno
  fullAdder Cero Uno Cero
  fullAdder Cero Uno Uno
  fullAdder Uno Cero Cero
  fullAdder Uno Cero Uno
  fullAdder Uno Uno Cero
  fullAdder Uno Uno Uno
-}
--fullAdder :: Bit -> Bit -> Bit -> [Bit]
-- fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)




{-| Ejercicio 10: Función de Flip-flop RS
  Ejemplos:
  flipFlopRS Cero Cero Uno
  flipFlopRS Uno Cero Cero
  flipFlopRS Cero Uno Cero
  flipFlopRS Cero Cero Cero
-}
-- flipFlopRS :: Bit -> Bit -> Bit -> Bit


{-| Ejercicio 11: Función para Flip-flop D
  Ejemplos:
  flipFlopD Cero Cero
  flipFlopD Cero Uno
  flipFlopD Uno Cero
  flipFlopD Uno Uno
-}
-- flipFlopD :: Bit -> Bit -> Bit


{-| Ejercicio 12: Función para Flip-flop JK
  Ejemplos:
  flipFlopJK Cero Cero Cero
  flipFlopJK Cero Cero Uno
  flipFlopJK Cero Uno Cero
  flipFlopJK Cero Uno Uno
  flipFlopJK Uno Cero Cero
  flipFlopJK Uno Cero Uno
  flipFlopJK Uno Uno Cero
  flipFlopJK Uno Uno Uno
-}
-- flipFlopJK :: Bit -> Bit -> Bit -> Bit


{-| Ejercicio 13: Función para Flip-flop T
  Ejemplos:
  flipFlopT Cero Cero
  flipFlopT Cero Uno
  flipFlopT Uno Cero
  flipFlopT Uno Uno
-}
-- flipFlopT :: Bit -> Bit -> Bit




data Pred = PTrue | PFalse | Predicado Nombre [Term] | Neg Pred | Conj Pred Pred | Disy Pred Pred | 
            Impl Pred Pred | Syss Pred Pred | PTodo Nombre Pred | Existe Nombre Pred deriving Eq

data Term = Var Nombre | Fun Nombre [Term] deriving Eq
type Nombre = String

{-|
  Visualización de tipo de dato Pred.
  (Var "y")
  Fun "g" [Var "x"]
  Predicado "D" [Var "a", Var "z"]
  Existe "x" (PTodo "w" (Impl (Predicado "C" [Fun "f" [(Var "y"), (Var "w"), (Var "v")], Fun "g" [Var "x"]]) PTrue))
-}
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

{-|
  Ejercicio 1: instancia de la clase ’Show’ para visualizar fórmulas del tipo Pred.
  --instance Show Pred where
-}
instance Show Term where
  show (Var n) = n
  show (Fun f l) = f ++ "(" ++ show l ++ ")"


{-| Ejercicio 14: Regresa el conjunto de variables de la fórmula pasada como parámetro.
  Ejemplos:
  variables (Predicado "C" [(Var "x")])
  variables (Predicado "P" [Var "x", Var "y"])
  variables (PTodo "x" (Predicado "Q" [Var "x"]))
  variables (  Neg ( Existe "y" (   Conj PTrue (    Predicado "C" [ (Var "x"), Fun "h" [(Var "x")] ]    )   ) )  )
-}
-- variables :: Pred -> [Nombre]

-- concat: acepta una lista de listas y las concatena
-- > concat [[1,2,3], [1,2,3]]
-- >   [1,2,3,1,2,3] 
-- > [1,2,3] ++ [1,2,3]
-- >   [1,2,3,1,2,3] 

-- map: devuelve una lista construida aplicando una función (el primer argumento) 
--      a todos los elementos de una lista pasada como segundo argumento
-- > map abs [-1,-3,4,-12] 
-- >   [1,3,4,12]
-- > map reverse ["abc","cda","1234"]
-- >   ["cba","adc","4321"]
-- > map (3*) [1,2,3,4]
-- >   [3,6,9,12]

{-| Función auxiliar para obtener las variables de un término.
  Ejemplos:
  variablesTermino (Var "x")
  variablesTermino (Fun "f" [Var "y", Var "z"])
-}
-- variablesTermino :: Term -> [Nombre]

{-| Función auxiliar para eliminar los elementos repetidos de un término. -}
-- eliminaRepetidos :: (Eq a) => [a] -> [a]


{-| Ejercicio 15: Regresa el conjunto de variables libres de la fórmula pasada como parámetro.
  Ejemplos:
  variablesLibres (Neg (Predicado "R" [Var "x"]))
  variablesLibres ( Conj PTrue (    Predicado "C" [ (Var "x"), Fun "h" [(Var "x")] ]    ) )
  variablesLibres (PTodo "x" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"])))
  variablesLibres ((Impl (Predicado "D" [(Var "v")]) (Existe "w" (Neg (Predicado "C" [Var "u", Var "w"])))))
-}
-- variablesLibres :: Pred -> [Nombre]


{-| Ejercicio 16: Regresa el conjunto de variables ligadas de la fórmula pasada como parámetro.
  Ejemplos:
  variablesLigadas (Predicado "R" [Var "x"])
  variablesLigadas ( Conj PTrue (    Predicado "C" [ (Var "x"), Fun "h" [(Var "x")] ]    ) )
  variablesLigadas (PTodo "x" (Predicado "P" [Var "x", Var "y"]))
  
  variablesLigadas (PTodo "x" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"])))
  variablesLigadas (PTodo "z" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"])))
  variablesLigadas ( PTodo "x" (PTodo "y" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"]))) )
  variablesLigadas (Impl (Predicado "D" [(Var "v")]) (Existe "w" (Neg (Predicado "C" [Var "u", Var "w"]))))
-}
-- variablesLigadas :: Pred -> [Nombre]


{-| Ejercicio 17: Aplica la equivalencia de negación de cuantificadores en la fórmula pasada como parámetro.
  Ejemplos:
  equivalenciaNeg (Neg (Existe "x" (Neg (Predicado "P" [Var "x"]))))

  equivalenciaNeg (Neg (PTodo "x" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"]))))
  equivalenciaNeg ((Syss (Neg (PTodo "x" (Disy PTrue PFalse))) (Existe "x" (Neg (Predicado "E" [Var "x", Var "u"])))))
-}
-- equivalenciaNeg :: Pred -> Pred


{-| Ejercicio 18: Regresa el total de cuantificadores de la fórmula pasada como parámetro.
  Ejemplos:
  cuantificadores (Predicado "P" [Var "z"])
  cuantificadores (Existe "y" (Predicado "P" [Var "y"]))
  cuantificadores (PTodo "x" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"])))
  cuantificadores (Conj (Existe "y" (Predicado "N" [Var "x"])) (PTodo "u" (Existe "w" (Disy (Predicado "O" [Var "w", Var "z", Var "u"]) PTrue))))
-}
-- cuantificadores :: Pred -> Int

{-| Funcion auxiliar -}
-- cuantificadoresTermino :: Term -> Int


{-| Ejercicio 19: Regresa el total de conectivos lógicos de la fórmula pasada como parámetro.
  Ejemplos:
  conectivos (Predicado "P" [Var "z"])
  conectivos (Conj (Predicado "P" [Var "x"]) (Neg (Predicado "Q" [Var "y"])))
  conectivos (PTodo "x" (Disy (Predicado "R" [Var "x"]) (Predicado "Q" [Var "y"])))

  conectivos (Existe "x" (Existe "y" (Predicado "P" [Fun "h" [Var "x", Var "y"]])))
  conectivos (PTodo "v" (Syss (Neg (Predicado "Q" [Var "u"])) (Conj (Predicado "R" [Var "v"]) (Neg PFalse))))
-}
-- conectivos :: Pred -> Int
