--      [ _______________ | x <- [1 .. 10] ________________ ]




-- ejercico 1: 

lista1 = [(+10) x | x <- [1..10] ]


-- ejercicio 2: 

lista2 = [[x] | x<-[1..10], even x]

-- ejercicio 3: 

lista3 = [[11-x] | x<-[1..10]]

-- ejercicio 4: 

lista4 = [odd x | x<-[1..10]]

-- ejercicio 5:

lista5 = [(x*3,mod 3 x*3 == 0) | x<-[1..10], x < 7] 

-- ejercicio 6:

lista6 = [(x*5,x*5==10) | x<-[1..10], x < 4 || x == 8] 

-- ejercicio 7:

lista7 = [(10+x,11+x) | x<-[1..10],odd x]


-- ejercicio 8:

lista8 = [[5..(4+3*x)] | x<-[1..10], x<4]



-- ejercicio 9:

lista9 = [21+((x-1)*(-5)) | x<-[1..10],x<6]

-- ejercicio 10:

lista10 = [[2*x, 2*x-2..4] | x<-[1..10], x>1 && x<7]

