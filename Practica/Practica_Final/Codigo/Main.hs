module Main where

import R2_2

-- Función principal
main :: IO ()
main = do
    putStrLn "Bienvenido al simulador del cubo de Rubik 2x2"
    menu R2_2.estadoIni

-- Bucle principal para interactuar con el usuario
menu :: Cubo -> IO ()
menu a = do
    putStrLn "-------- Opciones ----------"
    putStrLn "1. Mostrar cubo:"
    putStrLn "2. Mezclar cubo:"
    putStrLn "3. Resolver cubo:"
    putStrLn "4. Salir"
    putStr "Elige una opcion: "
    i <- getLine
    case i of
        "1" -> do
            muestraC a
            menu a
        "2" -> do
            putStrLn "\nintroduce le numero para generar un movimiento:"
            i2 <- getLine
            let x = read i2
            let a2 = R2_2.mezclar a x
            putStrLn "Cubo mezclado:"
            muestraC a2
            menu a2
        "3" -> do
           if R2_2.sol a
                then do
                    putStrLn "El cubo ya esta resuelto."
                    menu a
            else do
                let m = R2_2.resolverCubo a
                putStrLn "\nMovimientos para resolver el cubo:"
                pintarMovimientos m
                let c2 = R2_2.aplicaMovimientoCubo a m
                muestraC c2
                menu c2
        "4" -> putStrLn "Gracias por jugar."
        _ -> do
            putStrLn "Numero incorrectol, Intentelo de nuevo."
            menu a


-- Función para imprimir el estado del cubo
-- Función para mostrar el cubo
muestraC :: Cubo -> IO ()
muestraC [a, b, c, d, e, f] = do
    putStrLn "Cubo actual:"
    -- Mostrar la cara arriba
    putStrLn $ "Arriba: \n" ++ [a !! 0] ++ "\t" ++ [a !! 2] 
               ++ "\n" ++ [a !! 1] ++ "\t" ++ [a !! 3]
    -- Mostrar la cara frontal
    putStrLn $ "Frontal: \n" ++ [b !! 0] ++ "\t" ++ [b !! 2]
               ++ "\n" ++ [b !! 1] ++ "\t" ++ [b !! 3]
    -- Mostrar la cara derecha
    putStrLn $ "Derecha: \n" ++ [c !! 0] ++ "\t" ++ [c !! 2]
               ++ "\n" ++ [c !! 1] ++ "\t" ++ [c !! 3]
    -- Mostrar la cara izquierda
    putStrLn $ "Izquierda: \n" ++ [d !! 0] ++ "\t" ++ [d !! 2]
               ++ "\n" ++ [d !! 1] ++ "\t" ++ [d !! 3]
    -- Mostrar la cara trasera
    putStrLn $ "Trasera: \n" ++ [e !! 0] ++ "\t" ++ [e !! 2]
               ++ "\n" ++ [e !! 1] ++ "\t" ++ [e !! 3]
    -- Mostrar la cara abajo
    putStrLn $ "Abajo: \n" ++ [f !! 0] ++ "\t" ++ [f !! 2]
               ++ "\n" ++ [f !! 1] ++ "\t" ++ [f !! 3]


pintarMovimientos:: [Movimiento] -> IO ()
pintarMovimientos [] = return ()
pintarMovimientos (a:as) = do
                  pintaAux a
                  pintarMovimientos as

pintaAux :: Movimiento -> IO ()
pintaAux a
    | a == U = putStrLn "rotar Arriba"
    | a == U' = putStrLn "rotar Arriba Inverso"
    | a == F = putStrLn "rotar Frontal"
    | a == F' = putStrLn "rotar Frontal Inversa"
    | a == R = putStrLn "rotar Derecha"
    | a == R' = putStrLn "rotar Derecha Inverso"
    | a == L = putStrLn "rotar Izquierda"
    | a == L' = putStrLn "rotar Izquierda Inverso"
    | a == B = putStrLn "rotar Trasera"
    | a == B' = putStrLn "rotar Trasera Inversa"
    | a == D = putStrLn "rotar Abajo"
    | a == D' = putStrLn "rotar Abajo Inversa"
    | otherwise = putStrLn "movimiento desconcidp."
