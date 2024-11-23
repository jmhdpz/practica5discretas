data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)
infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:
-------------------- EJERCICIO 1 --------------------
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[y | y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = variables formula
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg (Atom var)) = Atom var
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = f1 :&: negacion f2 
negacion (f1 :<=>: f2) = negacion (f1 :=>: f2) :&: negacion (f2 :=>: f1) 

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg f1) = negacion (equivalencia f1)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = negacion (equivalencia f1) :|: equivalencia f2 
equivalencia (f1 :<=>: f2) = equivalencia (f1 :=>: f2) :&: equivalencia (f2 :=>: f1)

-------------------- EJERCICIO 4 --------------------
buscar :: Var -> [(Var, Bool)] -> Bool
buscar var [] = error "No todas las variables estan definidas"
buscar var ((v, val):xs) = if var == v
                                then val
                                else buscar var xs

interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom var) asignaciones = buscar var asignaciones
interpretacion (Neg f) asignaciones = not (interpretacion f asignaciones)
interpretacion (f1 :&: f2) asignaciones = interpretacion f1 asignaciones && interpretacion f2 asignaciones
interpretacion (f1 :|: f2) asignaciones = interpretacion f1 asignaciones || interpretacion f2 asignaciones
interpretacion (f1 :=>: f2) asignaciones = not (interpretacion f1 asignaciones) || interpretacion f2 asignaciones
interpretacion (f1 :<=>: f2) asignaciones = not((not (interpretacion f1 asignaciones) && interpretacion f2 asignaciones) || (interpretacion f1 asignaciones && not (interpretacion f2 asignaciones)))

-------------------- EJERCICIO 5 --------------------
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1+longitud xs

concatenarBool :: [[Bool]] -> Bool -> [[Bool]]
concatenarBool [lista] add = [lista ++ [add]]
concatenarBool (x:xs) add = concatenarBool [x] add ++ concatenarBool xs add

posibleCombinacion :: [[Bool]] -> Int -> [[Bool]]
posibleCombinacion booleanos 0 = booleanos
posibleCombinacion booleanos n = posibleCombinacion (concatenarBool booleanos True ++ concatenarBool booleanos False)  (n-1)

crearTuplas :: [Var] -> [Bool] -> [(Var,Bool)]
crearTuplas _ [] = error "Argumentos invalidos"
crearTuplas [] _ = error "Argumentos invalidos"
crearTuplas [variable] [bool] = [(variable,bool)]
crearTuplas (x:xs) (y:ys) = crearTuplas [x] [y] ++ crearTuplas xs ys

asignarVar :: [Var] -> [[Bool]] -> [[(Var,Bool)]]
asignarVar variable [bool] = [crearTuplas variable bool]
asignarVar variable (x:xs) = crearTuplas variable x : asignarVar variable xs

combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones f = asignarVar (variables f) (posibleCombinacion [[True],[False]] (longitud (variables f)-1))

-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad f = [(asignacion, interpretacion f asignacion) | asignacion <- combinaciones f]