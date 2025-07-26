-- Ejercicio 4: la función pertenece comprueba que un elemento pertenece a una lista

-- utilizando guada
pertence:: [Integer]->Integer->Bool

pertence [] _ = False   -- caso base
pertence l ele 
            | (head l) == ele = True
            | otherwise = pertence (tail l) ele 

-- con If then else
pertenceIf:: [Integer]->Integer->Bool

pertenceIf [] _ = False
pertenceIf l ele = if ((head l)==ele) then True
                        else pertenceIf (tail l) ele

-- con case
pertenceCase:: [Integer]->Integer->Bool

pertenceCase [] _ = False
pertenceCase l ele = case (head l == ele) of
                          True -> True
                          False -> pertenceCase (tail l) ele

-- con tuplas 

pertenceTuplas::  [(a, a)] -> (a, a) -> Bool

pertenceTuplas [] _ = False
pertenceTuplas l x = if ((fst (head l) == fst x) && (snd (head l) == snd x))  then True
                     else pertenceTuplas (tail l) x




