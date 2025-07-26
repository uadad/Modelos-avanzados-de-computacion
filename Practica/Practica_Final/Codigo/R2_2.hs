module R2_2 where

-- Representación del cubo
type Cara_Cubo = [Char] -- Cada cara tiene 4 pegatinas
type Cubo = [Cara_Cubo] -- Un cubo tiene 6 caras

-- definimos los movimientos que tenemos
data Movimiento = U | U'| F | F'| R | R'| L | L' | B | B'| D | D' deriving (Show, Eq)

-- Estado inicial del cubo (suponemos que esat resuelto)
estadoIni :: Cubo
estadoIni = [
    ['Y', 'Y', 'Y', 'Y'], --arriba
    ['R', 'R', 'R', 'R'], --front
    ['G', 'G', 'G', 'G'], --der
    ['O', 'O', 'O', 'O'], --izq
    ['B', 'B', 'B', 'B'], --tras
    ['W', 'W', 'W', 'W']  --abajo
    ]


-- funcion para delvolver la rotacion aplicada
devRotacion :: Movimiento -> Cubo -> Cubo
devRotacion U a  = movArriba a
devRotacion U' a = movArriba' a
devRotacion F a  = movFrontal a
devRotacion F' a = movFrontal' a
devRotacion R a  = movDer a
devRotacion R' a = movDer' a
devRotacion L a  = movIzq a
devRotacion L' a = movIzq' a
devRotacion B a  = movTrasera a
devRotacion B' a = movTrasera' a
devRotacion D a  = movAbajo a
devRotacion D' a = movAbajo' a


-- la siguienet funcion, nos ayuda a mezclar el cubo de manera que reciba un numero y genera movimientos
lista :: [(Movimiento, Cubo -> Cubo)]
lista = [
    (U, movArriba), (U', movArriba'), (F, movFrontal), (F', movFrontal'),
    (R, movDer), (R', movDer'), (L, movIzq), (L', movIzq'), (B, movTrasera), (B', movTrasera'), 
    (D, movAbajo), (D', movAbajo')]

mezclar :: Cubo -> Int -> Cubo 
mezclar a x
       | x<0 = a
       | x>=length lista = a
       | otherwise = snd (lista !! x) a


-- Comprobar si el cubo está resuelto
sol :: Cubo -> Bool
sol a = a == estadoIni



-- funciion para resolver el cubo mediante una busqueda
resolverCubo :: Cubo -> [Movimiento]
resolverCubo a = buscaSol a 

buscaSol :: Cubo -> [Movimiento]
buscaSol a = aux1 [[(a, [])]] [] 0
  where
    max = 10
    aux1 [] _ _ = []
    aux1 (n:ns) v p
      | any (\(x, _) -> x == estadoIni) n = snd ( head ( filter (\(x, _) -> x == estadoIni) n ))
      | p >= max = []
      | otherwise = aux1 (ns ++ [sigE]) (v ++ map fst n) (p + 1)
      where
        sigE = [(y, m ++ [z]) | (c, m) <- n, (z, f) <- lista, let y = f c, y `notElem` v]



-- funcion para aplicar una lista de movimiento a un cubo
aplicaMovimientoCubo :: Cubo -> [Movimiento] -> Cubo
aplicaMovimientoCubo c [] = c  -- Si no hay movimientos, el cubo no cambia
aplicaMovimientoCubo c (m:ms) = aplicaMovimientoCubo (devRotacion m c) ms



-- en continuacion, se d3efine los movimientos de rotar cada cara del cubo, para simplificar suponemos que se move en un unico sentido.
movArriba :: Cubo -> Cubo
movArriba [u, f, r, l, b, d] =
    [[u !! 1, u !! 3, u !! 0, u !! 2],  -- arriba
     [r !! 0, f !! 1, r !! 2, f !! 3],  -- frente
     [b !! 3, r !! 1, b !! 1, r !! 3],  -- derecha
     [f !! 0, l !! 1, f !! 2, l !! 3],  -- izquierda
     [b !! 0, l !! 2, b !! 2, l !! 0],  -- trasera
     d]                                 -- abajo

movArriba' :: Cubo -> Cubo
movArriba' [u, f, r, l, b, d] =
    [[u !! 2, u !! 0, u !! 3, u !! 1],  -- arriba
     [l !! 0, f !! 1, l !! 2, f !! 3],  -- frente
     [f !! 0, r !! 1, f !! 2, r !! 3],  -- derecha
     [b !! 3, l !! 1, b !! 1, l !! 3],  -- izquierda
     [b !! 0, r !! 2, b !! 2, r !! 0],  -- trasera
     d]                                 -- abajo

movFrontal :: Cubo -> Cubo
movFrontal [u, f, r, l, b, d] =
    [[u !! 0, l !! 3, u !! 2, l !! 2], -- arriba
     [f !! 1, f !! 3, f !! 0, f !! 2], -- frente
     [u !! 1, u !! 3, r !! 2, r !! 3], -- derecha
     [l !! 0, l !! 1, d !! 1, d !! 3], -- izquierda
     b,                                -- trasera
     [d !! 0, r !! 1, d !! 2, r !! 0]] -- abajo

movFrontal' :: Cubo -> Cubo
movFrontal' [u, f, r, l, b, d] =
    [[u !! 0, r !! 0, u !! 2, r !! 1], -- arriba
     [f !! 2, f !! 0, f !! 3, f !! 1], -- frente
     [d !! 3, d !! 1, r !! 2, r !! 3], -- derecha
     [l !! 0, l !! 1, u !! 3, u !! 1], -- izquierda
     b,                                -- trasera
     [d !! 0, l !! 2, d !! 2, l !! 3]] -- abajo


movDer :: Cubo -> Cubo
movDer [u, f, r, l, b, d] =
     [[u !! 0, u !! 1, f !! 2, f !! 3],  -- arriba
     [f !! 0, f !! 1, d !! 3, d !! 2],  -- frente
     [r !! 1, r !! 3, r !! 0, r !! 2],  -- derecha
     l,                                 -- izquierda
     [b !! 0, b !! 1, u !! 2, u !! 3],  -- tras
     [d !! 0, d !! 1, b !! 3, b !! 2]]  -- abajo


movDer' :: Cubo -> Cubo
movDer' [u, f, r, l, b, d] =
     [[u !! 0, u !! 1, b !! 2, b !! 3],  -- arriba
     [f !! 0, f !! 1, u !! 2, u !! 3],  -- frente
     [r !! 2, r !! 0, r !! 3, r !! 1],  -- derecha
     l,                                 -- izquierda
     [b !! 0, b !! 1, d !! 3, d !! 2],  -- tras
     [d !! 0, d !! 1, f !! 3, f !! 2]]  -- abajo


movIzq :: Cubo -> Cubo
movIzq [u, f, r, l, b, d] =
    [[b !! 0, b !! 1, u !! 2, u !! 3],  -- arriba
     [u !! 0, u !! 1, f !! 2, f !! 3],  -- frente
     r,                                 -- derecha
     [l !! 1, l !! 3, l !! 0, l !! 2],   -- izquierda
     [d !! 1, d !! 0, b !! 2, b !! 3],  -- tras
     [f !! 1, f !! 0, d !! 2, d !! 3]]  -- abajo

movIzq' :: Cubo -> Cubo
movIzq' [u, f, r, l, b, d] =
    [[f !! 0, f !! 1, u !! 2, u !! 3],  -- arriba
     [d !! 1, d !! 0, f !! 2, f !! 3],  -- frente
     r,                                 -- derecha
     [l !! 2, l !! 0, l !! 3, l !! 1],   -- izquierda
     [u !! 0, u !! 1, b !! 2, b !! 3],  -- tras
     [b !! 1, b !! 0, d !! 2, d !! 3]]  -- abajo


movTrasera :: Cubo -> Cubo
movTrasera [u, f, r, l, b, d] =
    [[r !! 2, u !! 1, r !! 3, u !! 3],  -- arriba
     f,                                 -- frente
     [r !! 0, r !! 1, d !! 2, d !! 0],  -- derecha
     [u !! 2, u !! 0, l !! 2, l !! 3],  -- izquierda
     [b !! 1, b !! 3, b !! 0, b !! 2],  -- tras
     [l !! 0, d !! 1, l !! 1, d !! 3]]  -- abajo


movTrasera' :: Cubo -> Cubo
movTrasera' [u, f, r, l, b, d] =
    [[l !! 1, u !! 1, l !! 0, u !! 3],  -- arriba
     f,                                 -- frente
     [r !! 0, r !! 1, u !! 2, u !! 0],  -- derecha
     [d !! 0, d !! 2, l !! 2, l !! 3],  -- izquierda
     [b !! 2, b !! 0, b !! 3, b !! 1],  -- tras
     [r !! 2, d !! 1, r !! 2, d !! 3]]  -- abajo


movAbajo :: Cubo -> Cubo
movAbajo [u, f, r, l, b, d] =
    [u,                                 -- arriba
     [f !! 0, r !! 1, f !! 2, r !! 3],  -- frente
     [r !! 0, b !! 2, r !! 2, b !! 0],  -- derecha
     [l !! 0, f !! 1, l !! 2, f !! 3],  -- izquierda
     [l !! 3, b !! 1, l !! 1, b !! 3],  -- tras
     [d !! 1, d !! 3, d !! 0, d !! 2]]  -- abajo

movAbajo' :: Cubo -> Cubo
movAbajo' [u, f, r, l, b, d] =
    [u,                                 -- arriba
     [f !! 0, l !! 1, f !! 2, l !! 3],  -- frente
     [r !! 0, f !! 1, r !! 2, f !! 3],  -- derecha
     [l !! 0, b !! 2, l !! 2, b !! 0],  -- izquierda
     [r !! 3, b !! 1, r !! 1, b !! 3],  -- tras
     [d !! 1, d !! 3, d !! 0, d !! 2]]  -- abajo




