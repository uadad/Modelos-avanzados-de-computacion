module Ejercicio2 where

data Fecha = Fecha {d::Int,m::Int,a::Int} deriving (Eq, Show, Ord)

data Empleado = Empleado { dni::String, nombre::String, apellido::String, fechaNac::Fecha, cargo::String, fechaAlta::Fecha } deriving (Show)

empleado_1 = Empleado { dni = "1222222W", nombre = "pepe", apellido = "gonzalwz", fechaNac = (Fecha 2 2 2008), cargo = "ingeniero", fechaAlta = (Fecha 1 1 2022) }
empleado_2 = Empleado { dni = "1333333E", nombre = "jose angel", apellido = "lopez", fechaNac = (Fecha 4 4 2008), cargo = "fisico", fechaAlta = (Fecha 15 1 2022) }
empleado_3 = Empleado { dni = "1444444R", nombre = "miguel angel", apellido = "rodriguez", fechaNac = (Fecha 3 3 2000), cargo = "matematico", fechaAlta = (Fecha 15 3 2024) }
empleado_4 = Empleado { dni = "1555555T", nombre = "fran", apellido = "fran", fechaNac = (Fecha 3 2 2002), cargo = "ingeniero", fechaAlta = (Fecha 20 3 2023) }
empleado_5 = Empleado { dni = "1666666Y", nombre = "gonzalo", apellido = "rodriguez", fechaNac = (Fecha 3 4 2003), cargo = "fisico", fechaAlta = (Fecha 15 9 2024) }
empleado_6 = Empleado { dni = "1777777I", nombre = "fran", apellido = "lopez", fechaNac = (Fecha 19 6 2003), cargo = "matematico", fechaAlta = (Fecha 3 4 2021) }

empleados = [empleado_1, empleado_2, empleado_3, empleado_4, empleado_5, empleado_6]



main1 :: IO ()
main1 = do
    putStrLn "-----Menu Principal------"
    putStrLn "1. Listar empleados:"
    putStrLn "2. Filtrar por fecha de nacimiento y fecha de alta:"
    putStrLn "3. Buscar por cargo:"
    putStrLn "4. Salir:"
    i <- getLine
    case i of
        "1" -> do
            lista empleados 
            main1
        "2" -> do
            putStrLn "Introducir una fecha de nacimiento:"
            f1 <- getLine
            let [d, m, a] = map read (words f1)
            let fechaNac = (Fecha d m a)
            putStrLn "Ahora, introducir una fecha de alta:"
            f2 <- getLine
            let [d2, m2, a2] = map read (words f2)
            let fechaAlta = (Fecha d2 m2 a2)
            lista (filfechaNacAlta empleados fechaNac fechaAlta)
            main1
        "3" -> do
            putStrLn "Introducir un cargo:"
            f1 <- getLine
            lista (bCargo empleados f1)
            main1
        "4" -> return ()
        _   -> do
            putStrLn "no valida" 
            main1


-- lista de empleados todos o filtrados
lista::[Empleado]->IO ()
lista [] = return ()
lista (e:es) = do 
                         putStrLn (show e) 
                         lista es

-- filtar las fechas Nac y ALta comprobando los años meses y dias
compFecha (Fecha d1 m1 a1) (Fecha d2 m2 a2) = 
    if a1 > a2 then True 
    else if a1 < a2 then False 
    else if m1 > m2 then True 
    else if m1 < m2 then False 
    else if d1 >= d2 then True 
    else False

filfechaNacAlta::[Empleado]->Fecha->Fecha->[Empleado]
filfechaNacAlta [] f1 f2 = []
filfechaNacAlta empleados f1 f2 =  filter (\x -> (compFecha (fechaNac x) f1) &&  (compFecha (fechaAlta x) f2)) empleados


-- buscar por cargo de emplesados
bCargo::[Empleado]->String->[Empleado]
bCargo [] c = []
bCargo empleados c = filter (\x -> cargo x == c) empleados

