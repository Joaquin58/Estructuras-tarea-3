{-# LANGUAGE GADTs #-} -- Error al cargar por cómo están definidos los tipos algebraicos. Solución: https://stackoverflow.com/questions/32828483/how-do-you-allow-gadts-in-haskell
import Practica03 -- Módulo con las funciones a testear.
import Data.List -- Funciones como 'sort', 'nub'...

-- Tipo de dato para pruebas con una sola respuesta.
data PruebaSimple t where PruebaS :: (Show t, Eq t) => t -> t -> PruebaSimple t
instance Show (PruebaSimple a) where 
    show (PruebaS x y) = if x == y
                         then "Pasó."
                         else "No pasó...\n  Respuesta esperada: " ++ show y ++ "\n  Respuesta dada: " ++ show x

-- Tipo de dato para pruebas con múltiples respuestas posibles.
data PruebaMultiple t where PruebaM :: (Show t, Eq t) => t -> [t] -> PruebaMultiple t
instance Show (PruebaMultiple a) where
    show (PruebaM x l)
        | elem x l = "Pasó."
        | otherwise = "No pasó...\n  Posibles respuestas: " ++ show l ++ "\n  Respuesta dada: " ++ show x

-- Variables y términos.
a = Var "a"
b = Var "b"
c = Var "c"
d = Var "d"
e = Var "e"
f = Var "f"
g = Var "g"
j = Var "j"
k = Var "k"
p = Var "p"
x = Var "x"
y = Var "y"
z = Var "z"
uno = Var "1"
cuatro = Var "4"
suma_a_b = Fun "suma" [a, b]
suma_c_b = Fun "suma" [c, b]
suma_1_1 = Fun "suma" [uno, uno]
suma_1_1_1_1 = Fun "suma" [suma_1_1, suma_1_1]
capital = Fun "capital" [p]

-- a+b = c+b => a=c.
igual_sumas = Predicado "igual" [suma_a_b, suma_c_b]
igual_ac = Predicado "igual" [a, c]
axioma = Impl igual_sumas igual_ac
-- Impl (Predicado "igual" [Fun "suma" [Var "a", Var "b"], Fun "suma" [Var "c", Var "d"]]) (Predicado "igual" [Var "a", Var "c"])
-- (1+1)+(1+1) = 4
igual_sumas_4 = Predicado "igual" [suma_1_1_1_1, cuatro]

-- Ejemplo de "Matemáticas discretas", de Favio E. Miranda y Elisa Viso G.
estudiante = Predicado "estudiante" [x]
profesor = Predicado "profesor" [y]
más_joven_xy = Predicado "más_joven" [x, y]
estudiante_profe = PTodo "x" (Impl estudiante (Existe "y" (Conj profesor más_joven_xy))) -- ∀x(estudiante(x) → ∃y(profesor(y) ∧ más_joven(x, y)))

-- (∀xP(x))→(∃yP(x))
ej00 = Impl (PTodo "x" (Predicado "P" [x])) (Existe "y" (Predicado "P" [x]))
-- (∀xP(x))∧(∃yP(z))
ej01 = Conj (PTodo "x" (Predicado "P" [x])) (Existe "y" (Predicado "P" [z]))
-- (∃xP(y,y))↔∃yP(y,z))
ej02 = Syss (PTodo "x" (Predicado "P" [y, y])) (Existe "y" (Predicado "P" [y, z]))
-- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
ej03 = Impl (Disy (Existe "x" (Predicado "P" [x, g])) (Existe "y" (Predicado "Q" [y, g]))) (Syss (Conj (Existe "z" (Predicado "R" [x])) (Neg (PTodo "f" (Predicado "S" [f])))) (Existe "g" (Predicado "T" [g, z])))
-- ¬∀x(True ∧ ¬∃yFalse)
ej04 = Neg (PTodo "x" (Conj PTrue (Neg (Existe "y" PFalse))))
-- ¬∀x∃yTrue
ej05 = Neg (PTodo "x" (Existe "y" PTrue))

-- Función principal.
main = do

  print "----- compuertaNOT -----"
  print (PruebaS (compuertaNOT Cero) Uno)
  print (PruebaS (compuertaNOT Uno) Cero)

  print "----- compuertaAND -----"
  print (PruebaS (compuertaAND Cero Cero) Cero)
  print (PruebaS (compuertaAND Cero Uno) Cero)
  print (PruebaS (compuertaAND Uno  Cero) Cero)
  print (PruebaS (compuertaAND Uno  Uno) Uno)

  print "----- compuertaOR -----"
  print (PruebaS (compuertaOR Cero Cero) Cero)
  print (PruebaS (compuertaOR Cero Uno) Uno)
  print (PruebaS (compuertaOR Uno  Cero) Uno)
  print (PruebaS (compuertaOR Uno  Uno) Uno)

  print "----- compuertaNAND -----"
  print (PruebaS (compuertaNAND Cero Cero) Uno)
  print (PruebaS (compuertaNAND Cero Uno) Uno)
  print (PruebaS (compuertaNAND Uno  Cero) Uno)
  print (PruebaS (compuertaNAND Uno  Uno) Cero)

  print "----- compuertaNOR -----"
  print (PruebaS (compuertaNOR Cero Cero) Uno)
  print (PruebaS (compuertaNOR Cero Uno) Cero)
  print (PruebaS (compuertaNOR Uno  Cero) Cero)
  print (PruebaS (compuertaNOR Uno  Uno) Cero)

  print "----- compuertaXOR -----"
  print (PruebaS (compuertaXOR Cero Cero) Cero)
  print (PruebaS (compuertaXOR Cero Uno) Uno)
  print (PruebaS (compuertaXOR Uno  Cero) Uno)
  print (PruebaS (compuertaXOR Uno  Uno) Cero)

  print "----- compuertaXNOR -----"
  print (PruebaS (compuertaXNOR Cero Cero) Uno)
  print (PruebaS (compuertaXNOR Cero Uno) Cero)
  print (PruebaS (compuertaXNOR Uno  Cero) Cero)
  print (PruebaS (compuertaXNOR Uno  Uno) Uno)

  print "----- halfAdder -----"
  print (PruebaS (halfAdder Cero Cero) (Cero, Cero))
  print (PruebaS (halfAdder Cero Uno) (Uno, Cero))
  print (PruebaS (halfAdder Uno  Cero) (Uno, Cero))
  print (PruebaS (halfAdder Uno  Uno) (Cero, Uno))

  print "----- fullAdder -----"
  print (PruebaS (fullAdder Cero Cero Cero) (Cero, Cero))
  print (PruebaS (fullAdder Cero Cero Uno) (Uno, Cero))
  print (PruebaS (fullAdder Cero Uno  Cero) (Uno, Cero))
  print (PruebaS (fullAdder Cero Uno  Uno) (Cero, Uno))
  print (PruebaS (fullAdder Uno  Cero Cero) (Uno, Cero))
  print (PruebaS (fullAdder Uno  Cero Uno) (Cero, Uno))
  print (PruebaS (fullAdder Uno  Uno  Cero) (Cero, Uno))
  print (PruebaS (fullAdder Uno  Uno  Uno) (Uno, Uno))

  print "----- flipFlopD -----"
  print (PruebaS (flipFlopD Cero Cero) Cero)
  print (PruebaS (flipFlopD Cero Uno) Uno)
  print (PruebaS (flipFlopD Uno  Cero) Cero)
  print (PruebaS (flipFlopD Uno  Uno) Uno)

  print "----- flipFlopJK -----" -- Q J K
  print (PruebaS (flipFlopJK Cero Cero Cero) Cero)
  print (PruebaS (flipFlopJK Cero Cero Uno) Cero)
  print (PruebaS (flipFlopJK Cero Uno  Cero) Uno)
  print (PruebaS (flipFlopJK Cero Uno  Uno) Uno)
  print (PruebaS (flipFlopJK Uno  Cero Cero) Uno)
  print (PruebaS (flipFlopJK Uno  Cero Uno) Cero)
  print (PruebaS (flipFlopJK Uno  Uno  Cero) Uno)
  print (PruebaS (flipFlopJK Uno  Uno  Uno) Cero)

  print "----- flipFlopT -----"
  print (PruebaS (flipFlopT Cero Cero) Cero)
  print (PruebaS (flipFlopT Cero Uno) Uno)
  print (PruebaS (flipFlopT Uno  Cero) Uno)
  print (PruebaS (flipFlopT Uno  Uno) Cero)

  print "----- variables -----"
  print (PruebaS (variables (Neg PTrue)) [])
  print (PruebaS (variables (Conj PTrue PFalse)) [])
  print (PruebaS (variables (Predicado "" [Var "hello_world"])) ["hello_world"])
  print (PruebaS (sort (variables (Predicado "" [x, Fun "g" [a, Fun "f" [b]], z]))) ["a", "b", "x", "z"])
  print (PruebaS (sort (variables igual_ac)) ["a", "c"])
  print (PruebaS (variables estudiante) ["x"])
  print (PruebaS (variables profesor) ["y"])
  print (PruebaS (sort (variables más_joven_xy)) ["x", "y"])
  print (PruebaM (sort (variables igual_sumas)) [["a", "b", "c"] , ["a", "b", "b", "c"]])
  print (PruebaM (sort (variables igual_sumas_4)) [["1", "4"] , ["1", "1", "1", "1", "4"]])
  print (PruebaM (sort (variables axioma)) [["a", "b", "c"] , ["a", "a", "b", "b", "c", "c"]])
  print (PruebaM (sort (variables estudiante_profe)) [["x", "y"] , ["x", "x", "y", "y"]])
  print (PruebaM (sort (variables ej00)) [["x"] , ["x", "x"]])
  print (PruebaS (variables ej01) ["x", "z"])
  print (PruebaM (sort (variables ej02)) [["y", "z"] , ["y", "y", "y", "z"]])
  print (PruebaM (sort (variables ej03)) [["f", "g", "x", "y", "z"] , ["f","g","g","g","x","x","y","z"]])
  print (PruebaS (variables ej04) []) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (variables ej05) []) -- ¬∀x∃yTrue
  
  print "----- variablesLibres -----"
  print (PruebaS (variablesLibres (Neg PTrue)) [])
  print (PruebaS (variablesLibres (Conj PTrue PFalse)) [])
  print (PruebaS (variablesLibres (Predicado "" [Var "hello_world"])) ["hello_world"])
  print (PruebaS (variablesLibres igual_ac) ["a", "c"])
  print (PruebaS (variablesLibres estudiante) ["x"])
  print (PruebaS (variablesLibres profesor) ["y"])
  print (PruebaS (variablesLibres más_joven_xy) ["x", "y"])
  print (PruebaM (sort (variablesLibres igual_sumas)) [["a", "b", "b", "c"], ["a", "b", "c"]])
  print (PruebaM (sort (variablesLibres igual_sumas_4)) [["1", "1", "1", "1", "4"], ["1", "4"]])
  print (PruebaM (sort (variablesLibres axioma)) [["a", "a", "b", "b", "c", "c"], ["a", "b", "c"]])
  print (PruebaS (variablesLibres estudiante_profe) [])
  print (PruebaS (variablesLibres ej00) ["x"]) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (variablesLibres ej01) ["z"]) -- (∀xP(x))∧(∃yP(z))
  print (PruebaM (sort (variablesLibres ej02)) [["y", "y", "z"], ["y", "z"]]) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaM (sort (variablesLibres ej03)) [["g", "g", "x", "z"], ["g", "x", "z"]]) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (variablesLibres ej04) []) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (variablesLibres ej05) []) -- ¬∀x∃yTrue
  
  print "----- variablesLigadas -----"
  print (PruebaS (variablesLigadas (Neg PTrue)) [])
  print (PruebaS (variablesLigadas (Conj PTrue PFalse)) [])
  print (PruebaS (variablesLigadas (Predicado "" [Var "hello_world"])) [])
  print (PruebaS (variablesLigadas igual_ac) [])
  print (PruebaS (variablesLigadas estudiante) [])
  print (PruebaS (variablesLigadas profesor) [])
  print (PruebaS (variablesLigadas más_joven_xy) [])
  print (PruebaS (variablesLigadas igual_sumas) [])
  print (PruebaS (variablesLigadas igual_sumas_4) [])
  print (PruebaS (variablesLigadas axioma) [])
  print (PruebaM (sort (variablesLigadas estudiante_profe)) [["x", "x", "x", "y", "y", "y"], ["x", "y"]])
  print (PruebaM (sort (variablesLigadas ej00)) [["x", "x", "y"], ["x", "y"]]) -- (∀xP(x))→(∃yP(x))
  print (PruebaM (sort (variablesLigadas ej01)) [["x", "x", "y"], ["x", "y"]]) -- (∀xP(x))∧(∃yP(z))
  print (PruebaM (sort (variablesLigadas ej02)) [["x", "y", "y"], ["x", "y"]]) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaM (sort (variablesLigadas ej03)) [["f", "f", "g", "g", "x", "x", "y", "y", "z"], ["f", "g", "x", "y", "z"]]) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (sort (variablesLigadas ej04)) ["x", "y"]) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (sort (variablesLigadas ej05)) ["x", "y"]) -- ¬∀x∃yTrue
  
  print "----- equivalenciaNeg -----"
  print (PruebaS (equivalenciaNeg (Neg PTrue)) (Neg PTrue))
  print (PruebaS (equivalenciaNeg (Conj PTrue PFalse)) (Conj PTrue PFalse))
  print (PruebaS (equivalenciaNeg (Predicado "" [Var "hello_world"])) (Predicado "" [Var "hello_world"]))
  print (PruebaS (equivalenciaNeg igual_ac) igual_ac)
  print (PruebaS (equivalenciaNeg estudiante) estudiante)
  print (PruebaS (equivalenciaNeg profesor) profesor)
  print (PruebaS (equivalenciaNeg más_joven_xy) más_joven_xy)
  print (PruebaS (equivalenciaNeg igual_sumas) igual_sumas)
  print (PruebaS (equivalenciaNeg igual_sumas_4) igual_sumas_4)
  print (PruebaS (equivalenciaNeg axioma) axioma)
  print (PruebaS (equivalenciaNeg estudiante_profe) estudiante_profe)
  print (PruebaS (equivalenciaNeg ej00) ej00)
  print (PruebaS (equivalenciaNeg ej01) ej01)
  print (PruebaS (equivalenciaNeg ej02) ej02)
  print (PruebaS (equivalenciaNeg ej03) (Impl (Disy (Existe "x" (Predicado "P" [x, g])) (Existe "y" (Predicado "Q" [y, g]))) (Syss (Conj (Existe "z" (Predicado "R" [x])) (Existe "f" (Neg (Predicado "S" [f])))) (Existe "g" (Predicado "T" [g, z])))))
  print (PruebaS (equivalenciaNeg ej04) (Existe "x" (Neg (Conj PTrue (PTodo "y" (Neg PFalse))))))
  print (PruebaS (equivalenciaNeg ej05) (Existe "x" (PTodo "y" (Neg PTrue))))
  
  print "----- cuantificadores -----"
  print (PruebaS (cuantificadores (Neg PTrue)) 0)
  print (PruebaS (cuantificadores (Conj PTrue PFalse)) 0)
  print (PruebaS (cuantificadores (Predicado "" [Var "hello_world"])) 0)
  print (PruebaS (cuantificadores igual_ac) 0)
  print (PruebaS (cuantificadores estudiante) 0)
  print (PruebaS (cuantificadores profesor) 0)
  print (PruebaS (cuantificadores más_joven_xy) 0)
  print (PruebaS (cuantificadores igual_sumas) 0)
  print (PruebaS (cuantificadores igual_sumas_4) 0)
  print (PruebaS (cuantificadores axioma) 0)
  print (PruebaS (cuantificadores estudiante_profe) 2)
  print (PruebaS (cuantificadores ej00) 2) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (cuantificadores ej01) 2) -- (∀xP(x))∧(∃yP(z))
  print (PruebaS (cuantificadores ej02) 2) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaS (cuantificadores ej03) 5) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (cuantificadores ej04) 2) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (cuantificadores ej05) 2) -- ¬∀x∃yTrue
  
  print "----- conectivos -----"
  print (PruebaS (conectivos (Neg PTrue)) 1)
  print (PruebaS (conectivos (Conj PTrue PFalse)) 1)
  print (PruebaS (conectivos (Predicado "" [Var "hello_world"])) 0)
  print (PruebaS (conectivos igual_ac) 0)
  print (PruebaS (conectivos estudiante) 0)
  print (PruebaS (conectivos profesor) 0)
  print (PruebaS (conectivos más_joven_xy) 0)
  print (PruebaS (conectivos igual_sumas) 0)
  print (PruebaS (conectivos igual_sumas_4) 0)
  print (PruebaS (conectivos axioma) 1)
  print (PruebaS (conectivos estudiante_profe) 2)
  print (PruebaS (conectivos ej00) 1) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (conectivos ej01) 1) -- (∀xP(x))∧(∃yP(z))
  print (PruebaS (conectivos ej02) 1) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaS (conectivos ej03) 5) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (conectivos ej04) 3) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (conectivos ej05) 1) -- ¬∀x∃yTrue
  