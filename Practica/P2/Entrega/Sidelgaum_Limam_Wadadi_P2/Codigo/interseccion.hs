-- Ejercicio 3: define la función intersección, tal que (interseccion i1 i2) es la intersección de los intervalos i1 e-- i2

-- utilizando guardas |
--interseccion :: Ord a => [a] -> [a] -> [a]

interseccion  [x, y] [w, z] 
           | y < w = []
           | x > z = []
           | otherwise = [(max x w),(min y z)]     -- [(max x w)..(min y z)]

-- utilizando if then else
--interseccionIF:: Ord a => [a] -> [a] -> [a]

interseccionIF  [x, y] [w, z] = if (y < w) then [] else if (x >z) then []
                                else  [(max x w),(min y z)]                  



