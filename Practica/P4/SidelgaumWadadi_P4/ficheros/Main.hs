module Main where

import Ejercicio2 
import Ejercicio3 

-- Menú principal
main :: IO ()
main = do
    putStrLn "----- Menu Principal -----"
    putStrLn "1. Menu Ejercicio 2:"
    putStrLn "2. Menu Ejercicio 3:"
    putStrLn "3. Salir:"
    putStrLn "Elige una opcion:"
    i <- getLine
    case i of
        "1" -> do
            Ejercicio2.main1
            main
        "2" -> do
            Ejercicio3.main2
            main
        "3" -> return ()
        _   -> do
            putStrLn "Opción no válida. Inténtalo de nuevo."
            main
