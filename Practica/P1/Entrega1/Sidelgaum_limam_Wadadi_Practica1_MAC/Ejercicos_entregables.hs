-- Ejercico 1: cambia el primer valor de la lista b por el valor de a

cambia_el_primero:: Integral a => a-> [a] -> [a]

cambia_el_primero x b = x : (tail b)

-- Ejercico 2: cambia_el_n(a,n,b): cambia el valor de la posición n de la lista b por el valor de a

cambia_el_n:: Int->Int->[Int]->[Int]

cambia_el_n x n b = take n b ++ [x] ++ drop (n+1) b 

-- Ejercico 3: get_mayor_abs(a): devuelve el mayor número en valor absoluto de la lista a

get_mayor_abs:: Integral a=>[a]->a

get_mayor_abs b =  maximum (map abs b)

-- Ejercico 4: num_veces(a,b): devuelve la cantidad de veces que aparece el valor a en la lista b

num_veces:: Int->[Int]->Int

num_veces a b = length (filter (==a) b)

-- Ejercico 5: palabras_mayores_n(n,a): devuelve una lista con las palabras mayores que n

palabras_mayores_n:: Int -> [[a]] -> [[a]]

palabras_mayores_n n b = filter (\x -> length x>n) b


------------------------------------------------------------------------------------------------------------------------------------------


-- Ejercico 1: es_palindroma palabra: comprueba si “palabra” es palíndroma

es_palindroma:: String->Bool

es_palindroma x = x == (reverse x)


-- Ejercico2: palindromas []: comprueba son palíndromas todas las palabras de una lista


palindromas:: [String] -> Bool

palindromas x = and (map (es_palindroma) x)

-- otra solucion   palindromas x = all (es_palindroma) x 


-- Ejercico 3: sumparesimp [x..z]: suma los pares y resta los impares de una lista

sumparesimp:: [Int]->Int

sumparesimp a = sum (filter even a) - sum (filter odd a)  -- si se refiere a [1,2,3,4] (4+2)-(1+3)

-- Ejercico 4: esprimo(x): Devuelve si el número introducido es primo o no

esprimo:: Int-> Bool

esprimo b = not( or (map(\x -> mod b x == 0) [2..(b `div` 2)]))


----------------------------------------------------- 5 Ejemplos --------------------------------------------------------------


-- Ejercico 1: sumparesimp_divisible(a,b): Obtener el resultado de suma los pares y resta los impares de los numeros divisibles por b de una lista a

sumparesimp_divisible:: [Int]->Int->Int

sumparesimp_divisible a b = sumparesimp (filter(\x -> mod x b == 0) a)


-- Ejercico 2: mos_busCadena(a,b) mostrar todos los valores intermedios de buscar la cadena invertida que conicida con b en una lista a 

mos_busCadena:: [String]->String->[Bool]
cad_invertida s = reverse s
mos_busCadena a b = map (\x -> x==cad_invertida b) a

-- Ejercico 3:   n_divB(a,n,b) Obtener los n primeros elementos de la lista a divisible por b

n_divB:: [Int]->Int->Int->[Int]

n_divB a n b = take n (filter (\x -> mod x b ==0) a)

-- Ejercico 4: maxComun_dosprimeros(a): Obtener el maximo comun divisor de los dos primeros pares de la lista a

maxComun_dosprimeros:: [Int]->Int

maxComun_dosprimeros a = gcd (head (filter even a)) (head(tail (filter even a)))


-- Ejercico 5: todos_primo(a): Comprobar si todos los elemntos de la lista a son primos

todos_primo:: [Int]->Bool

todos_primo a = and (map esprimo a)

