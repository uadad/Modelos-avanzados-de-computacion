-- Ejercicio 2: función raices a b c

-- utilizando if then else

raices a b c = if (a==0) then [] 
               else if ((b*b)<(4*a*c)) then []
               else aux a b c
               where 
                 aux a b c = [(-b + sqrt ((b*b) - (4 *a*c)))/(2*a),(-b - sqrt ((b*b) - (4*a*c)))/(2*a)]

-- utilizando guardas |

raices_gua a b c 
       | a==0 = [] 
       | ((b*b)-(4*a*c))<0 = []
       | otherwise = aux a b c
       
       where 
         aux a b c = [(-b + sqrt ((b*b) - (4 *a*c)))/(2*a),(-b - sqrt ((b*b) - (4*a*c)))/(2*a)]