ordena_mayor:: [Int]->[Int]

ordena_mayor [] = []

ordena_mayor lista = (minimum lista : tail (filter(\x -> x == (minimum lista)) lista) ) ++ ordena_mayor(filter(\x -> x /= (minimum lista)) lista)