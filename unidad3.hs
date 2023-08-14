-- Unidad 3
--Ejercicio 2
-- 2a
absoluto x | x >= 0 = x
           | otherwise = -x


-- 2b
maximoabsoluto x y | absoluto(x) >= absoluto(y) = absoluto(x)
                   | otherwise = absoluto(y)


-- 2c
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z 


-- 2d
algunoEs0 x y | x /= 0 && y /= 0 = False
              | otherwise = True 


-- 2e
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False 


-- 2f 
mismoIntervalo x y | (3 < x && x <= 7) && (3 < y && y <= 7) = True
                   | x > 7 && y > 7 = True
                   | x <= 3 && y <= 3 = True
                   |otherwise = False


-- 2g
sumaDistintos x y z | x == y && x == z = x
                    | x == y = x + z
                    | x == z = x + y
                    | y == z = x + y
                    | otherwise = x + y + z


-- 2h
esMultiplo x y | mod x y == 0 = True
               | otherwise = False


-- 2i
digitoUnidades x | x >= 10 = mod x 10
                 | otherwise = x


-- 2j
digitoDecenas x | x >= 100 = mod x 100
                | otherwise = x


-- Ejercicio 3           
-- 3
estanRelacionados a b | a == 0 && b == 0 = False
                      | mod a b == 0 = True
                      | otherwise = False


-- Ejercicio 4
-- 4a 
prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2


-- 4b
todoMenor (x1, y1) (x2, y2) | x1 < x2 && y1 < y2 = True
                            | otherwise = False


-- 4c 
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1)**2 + (y2 - y1)**2)


-- 4d
sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (a, b, c) = a + b + c


-- 4e 
sumarSoloMultiplos (x, y, z) k | mod x k == 0 && mod y k == 0 && mod z k == 0 = x + y + z
                               | mod x k == 0 && mod y k == 0 && mod z k /= 0 = x + y 
                               | mod x k == 0 && mod y k /= 0 && mod z k == 0 = x + z
                               | mod x k /= 0 && mod y k == 0 && mod z k == 0 = y + z
                               | mod x k == 0 && mod y k /= 0 && mod z k /= 0 = x
                               | mod x k /= 0 && mod y k == 0 && mod z k /= 0 = y 
                               | mod x k /= 0 && mod y k /= 0 && mod z k == 0 = z
                               | otherwise = 0  



-- 4f
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4


-- 4g
crearPar a b = (a, b)


-- 4h
invertir (a,b) = (b,a)


-- Ejercicio 5
f :: Integer ->Integer
f j | j <= 7 = j*j
    | otherwise = (2*j) - 1

g :: Integer ->Integer
g n | mod n 2 == 0 =  div n 2
    | otherwise = 3*n + 1


todosMenores :: (Integer, Integer, Integer) ->Bool
todosMenores (x, y, z) | f(x) > g(x) && f(y) > g(y) && f(z) > g(z) = True
                       | otherwise = False


--Ejercicio 6
bisiesto x | mod x 4 /= 0 || (mod x 100 == 0 && mod x 400 /= 0) = False
           | otherwise = True


--Ejercicio 7
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = absoluto(x1 - x2) + absoluto(y1 - y2) + absoluto(z1 - z2) 


--Ejercicio 8
sumaUltimaDosDigitos x = (mod x 10) + mod (div x 10) 10

comparar a b | sumaUltimaDosDigitos(a) < sumaUltimaDosDigitos(b) = 1
             | sumaUltimaDosDigitos(a) > sumaUltimaDosDigitos(b) = -1
             | otherwise = 0






