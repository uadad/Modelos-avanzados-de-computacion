module Ejercicio3 where

data Fecha = Fecha {d::Int,m::Int,a::Int} deriving (Eq, Show, Ord)

data Empleado = Empleado { dni::String, nombre::String, apellido::String, fechaNac::Fecha, cargo::String, fechaAlta::Fecha } deriving (Show)

empleado_1 = Empleado { dni = "1222222W", nombre = "pepe", apellido = "gonzalwz", fechaNac = (Fecha 2 2 2008), cargo = "ingeniero", fechaAlta = (Fecha 1 1 2022) }
empleado_2 = Empleado { dni = "1333333E", nombre = "jose angel", apellido = "lopez", fechaNac = (Fecha 4 4 2008), cargo = "fisico", fechaAlta = (Fecha 15 1 2022) }
empleado_3 = Empleado { dni = "1444444R", nombre = "miguel angel", apellido = "rodriguez", fechaNac = (Fecha 3 3 2000), cargo = "matematico", fechaAlta = (Fecha 15 3 2024) }
empleado_4 = Empleado { dni = "1555555T", nombre = "fran", apellido = "fran", fechaNac = (Fecha 3 2 2002), cargo = "ingeniero", fechaAlta = (Fecha 20 3 2023) }
empleado_5 = Empleado { dni = "1666666Y", nombre = "gonzalo", apellido = "rodriguez", fechaNac = (Fecha 3 4 2003), cargo = "fisico", fechaAlta = (Fecha 15 9 2024) }
empleado_6 = Empleado { dni = "1777777I", nombre = "fran", apellido = "lopez", fechaNac = (Fecha 19 6 2003), cargo = "matematico", fechaAlta = (Fecha 3 4 2021) }

empleados = [empleado_1, empleado_2, empleado_3, empleado_4, empleado_5, empleado_6]

data Abb a = Vacio | Nodo a (Abb a) (Abb a) deriving (Show)


-- funcion de comprobar fecha segun el ano dia mes
compFecha (Fecha d1 m1 a1) (Fecha d2 m2 a2) = 
    if a1 > a2 then True 
    else if a1 < a2 then False 
    else if m1 > m2 then True 
    else if m1 < m2 then False 
    else if d1 >= d2 then True 
    else False

--funcion de insertarNodo igual que las transparencias
insertarNodo::Empleado->Abb Empleado->Abb Empleado
insertarNodo nuevo Vacio = Nodo nuevo Vacio Vacio
insertarNodo nuevo (Nodo a izq der) =
    if (compFecha (fechaNac nuevo) (fechaNac a)) then Nodo a (insertarNodo nuevo izq) der
    else Nodo a izq (insertarNodo nuevo der)

-- dsp de crear la funcion insertarNodo construyimos nuestro arbol final de empleados
arbol= foldr insertarNodo Vacio empleados


main2 :: IO ()
main2 = do
    putStrLn "-----Menu Principal------"
    putStrLn "1. Listar los empleados utilizando recorrido en profundidad:"
    putStrLn "2. Listar los empleados utilizando recorrido en anchura:"
    putStrLn "3. Buscar un empleado por dni:"
    putStrLn "4. Volver al menú principal:"
    i <- getLine
    case i of
        "1" -> do
            lista (prof arbol)
            main2
        "2" -> do
            lista (anch arbol)
            main2
        "3" -> do
            putStrLn "Introducir un dni:"
            s <- getLine
            case buscaDni s arbol of
                [a] -> lista [a]
                []  -> putStrLn "Empleado no encontrado."
            main2
        "4" -> return ()
        _   -> do
            putStrLn "no vailda."
            main2


-- lista de empleados todos o filtrados
lista::[Empleado]->IO ()
lista [] = return ()
lista (e:es) = do 
                         putStrLn (show e) 
                         lista es

--funcion en profundidad
prof::Abb Empleado->[Empleado]
prof Vacio = []
prof (Nodo actual izq der) = 
     [actual] ++ prof izq ++ prof der


anch::Abb Empleado->[Empleado]
anch Vacio = []
anch (Nodo actual izq der) = 
      anch izq ++ anch der ++ [actual]

buscaDni::String->Abb Empleado->[Empleado]
buscaDni _ Vacio = []
buscaDni s (Nodo actual izq der)
    | (dni actual) == s   = [actual]
    | otherwise = case (buscaDni s izq) of  
                              [x] -> [x]
                              []  -> buscaDni s der
