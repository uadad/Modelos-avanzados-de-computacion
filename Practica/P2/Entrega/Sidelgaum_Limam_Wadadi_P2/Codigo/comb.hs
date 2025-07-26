-- Ejercicio 1: función comb n k que devuelve el numero de combinaciones de n elementos tomados de k en k.

-- utilizando guardas |
comb::  Integer -> Integer -> Integer

comb n k
  | (k == 0) = 1 
  | k > n = 0 
  | otherwise = div (factIf n) (factIf k * factIf (n - k))

  where
    factIf x = if (x==0) then 1 else x*factIf (x-1)

-- utilizando if then else

combIf::  Integer -> Integer -> Integer

combIf n k = if (k == 0) then 1 
    else if (k>n) then 0 
    else div (factIf n) (factIf k * factIf (n - k))

  where
    factIf x = if (x==0) then 1 else x*factIf (x-1)

-- utilizando case 

combCase::  Integer -> Integer -> Integer

combCase n k = case (k) of  
    0 -> 1
    _ -> div (factIf n) (factIf k * factIf (n - k))

  where
    factIf x = if (x==0) then 1 else x*factIf (x-1)