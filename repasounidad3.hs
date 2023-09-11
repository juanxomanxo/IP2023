--UNIDAD 3

--Ejercicio 2
absoluto :: Float -> Float
absoluto x | x >= 0 = x
           | otherwise = -x

maximoAbsoluto :: Float -> Float -> Float
maximoAbsoluto x y | absoluto x > absoluto y = x
                   | absoluto x == absoluto y = x
                   | otherwise = y

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z

algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False 

ambosSon0 :: Integer -> Integer -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False

mismoIntervalo :: Integer -> Integer -> Bool
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | x > 7 && y > 8 = True
                   | (3 < x && x < 8) && (3 < y && y < 8) = True 
                   | otherwise = False

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x /= y && x /= z && z /= y = x + y + z
                    | x /= y && x == z = x + y
                    | x /= z && y == z = x + y
                    | y /= z && x == z = y + x
                    | x == y && x == z = x

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10

digitoDecenas :: Integer -> Integer
digitoDecenas x = mod (div x 10) 10


-- Ejercicio 3
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y | x == 0 = False
                      | y == 0 = False
                      | mod x y == 0 = True

--Ejercicio 4
prodInt :: (Integer, Integer) -> (Integer, Integer) -> Integer
prodInt (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

todoMenor :: (Integer, Integer) -> (Integer, Integer) -> Bool
todoMenor (x1, y1) (x2, y2) | x1 < x2 && y1 < y2 = True
                            | otherwise = False

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y, z) = x + y + z

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x, y, z) n | mod x n == 0 && mod y n == 0 && mod z n == 0 = x + y + z
                               | mod x n == 0 && mod y n == 0 && mod z n /= 0 = x + y
                               | mod x n == 0 && mod y n /= 0 && mod z n == 0 = x + z
                               | mod x n /= 0 && mod y n == 0 && mod z n == 0 = y + z
                               | mod x n == 0 && mod y n == 0 && mod z n == 0 = x
                               | mod x n /= 0 && mod y n == 0 && mod z n /= 0 = y
                               | mod x n /= 0 && mod y n /= 0 && mod z n == 0 = z
                               | mod x n /= 0 && mod y n /= 0 && mod z n /= 0 = 0

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod x 2 /= 0 && mod y 2 == 0 = 2
                       | mod x 2 /= 0 && mod y 2 /= 0 && mod z 2 == 0 = 3
                       | mod x 2 /= 0 && mod y 2 /= 0 && mod z 2 /= 0 = 4

crearPar :: Integer -> Integer -> (Integer, Integer) 
crearPar x y = (x, y)

invertir :: (Integer, Integer) -> (Integer, Integer)
invertir (x, y) = (y, x)

--Ejercicio 5
todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x, y, z) | f x > g x && f y > g y && f z > g z = True
                       | otherwise = False

f :: Integer -> Integer 
f x | x <= 7 = x^2
    | x > 7 = 2*x - 1

g :: Integer -> Integer
g x | mod x 2 == 0 = div x 2
    | otherwise = 3*x + 1


-- Ejercicio 6
bisiesto :: Integer -> Bool
bisiesto x | esMultiploDe x 4 == False || esMultiploDe x 100 == True && esMultiploDe x 400 == False = False 
           | otherwise = True

--Ejercicio 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = absoluto(x1 - x2) + absoluto(y1 - y2) + absoluto(z1 - z2)

--Ejercicio 8
comparar :: Integer -> Integer -> Integer
comparar x y | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
             | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
             | otherwise = 0

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = (mod x 10) + (mod (div x 10) 10)


--UNIDAD 4
--Ejercicio 1
fibonacci :: Integer -> Integer 
fibonacci x = fib x

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib (x-1)) + (fib (x-2))

--Ejercicio 2
parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n - 1) + 1

-- Ejercicio 3
esDivisible :: Integer -> Integer -> Bool
esDivisible x y | x == 0 = True
                | x < 0 = False
                | x > 0 = esDivisible (x-y) y

-- Ejercicio 4
sumaImpares :: Integer -> Integer
sumaImpares x | x == 1 = 1
              | x > 1 = n_esimo_impar + sumaImpares (x-1)
              where n_esimo_impar = 2*x - 1

-- Ejercicios 5
medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x-2)

-- Ejercicio 6
sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | x >= 10 = mod x 10 + sumaDigitos (div x 10)

--Ejercicio 7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x = todosDigitosIgualesBis x (mod x 10)

todosDigitosIgualesBis :: Integer -> Integer -> Bool
todosDigitosIgualesBis y z | y < 10 && (mod y 10) == z = True
                           | y < 10 && (mod y 10) /= z = False
                           | y >= 10 && (mod y 10) == z = todosDigitosIgualesBis (div y 10) z
                           | y >= 10 && (mod y 10) /= z = False

-- Ejercicio 8
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | n >= 0 && (1 <= i && i <= cantDigitos (n)) = mod (div n (10^(cantDigitos (n)-i))) 10
                 | otherwise = 00000000000

cantDigitos n | n < 10 = 1
              | n > 10 = cantDigitos (div n 10) + 1

-- Ejercicio 9 CONSULTAR
--esCapicua :: Integer -> Bool
--esCapicua x | mod (cantDigitos x) 2 == 0 = 
--            | otherwise =

-- Ejercicio 10
f1 :: Integer -> Integer
f1 0 = 1
f1 x = 2^x + f1 (x-1)

f2 :: Integer -> Integer -> Integer
f2 x 1 = 1
f2 x y = x^y + f2 x (y-1)

f3 :: Integer -> Integer -> Integer
f3 x y = f3Bis x (2*y)

f3Bis :: Integer -> Integer -> Integer
f3Bis x 1 = x
f3Bis x y = x^y + f3Bis x (y-1)

f4 :: Integer -> Integer -> Integer
f4 x y = (f4Bis x (2*y)) - (f4Tris x (y-1))

f4Bis :: Integer -> Integer -> Integer
f4Bis x 1 = x
f4Bis x y = x^y + f4Bis x (y-1)

f4Tris :: Integer -> Integer -> Integer
f4Tris x 1 = x
f4Tris x y = x^y + f4Tris x (y-1)

-- Ejercicio 11
e = eAprox 10

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = (1 / (factorial (fromIntegral n))) + eAprox (n-1) 

factorial :: Float -> Float
factorial 0 = 1
factorial x = x * factorial (x-1)

-- Ejercicio 12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox x = 1 + 1 / (denominador (fromInteger x))

denominador :: Float -> Float
denominador 1 = 2
denominador x = 2 + 1 / (denominador (x-1))  

-- Ejercicio 13
--TODO

-- Ejercicio 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n 1 = q^(n+1)
sumaPotencias q n m = sumaPotenciasBis q n m + sumaPotencias q n (m-1)

sumaPotenciasBis q 1 m = q^(1+m)
sumaPotenciasBis q n m = q^(n+m) + sumaPotenciasBis q (n-1) m 

-- Ejercicio 15
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n 1 = fromInteger n
sumaRacionales n m = sumaRacionalesBis (fromInteger n) (fromInteger m) + sumaRacionales (fromInteger n) (fromInteger(m-1))

sumaRacionalesBis 1 m = 1 / m
sumaRacionalesBis n m = n / m + sumaRacionalesBis (n-1) m 

-- Ejercicio 16
menorDivisor :: Integer -> Integer
menorDivisor x = menorDivisorBis x 2

menorDivisorBis :: Integer -> Integer -> Integer
menorDivisorBis x y | x == y = y
                    | mod x y == 0 = y
                    | otherwise = menorDivisorBis x (y+1)

esPrimo :: Integer -> Bool
esPrimo x | menorDivisor x == x = True
          | otherwise = False

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y | euclides x y == 1 = True
                | otherwise = False

euclides :: Integer -> Integer -> Integer
euclides x 0 = x
euclides x y | mod x y /= 0 = euclides y (mod x y)
             | otherwise = y

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo x = nEsimoPrimoBis x 1 2

nEsimoPrimoBis :: Integer -> Integer -> Integer -> Integer
nEsimoPrimoBis x y z | x == y && esPrimo z == True = z
                     | x == y && esPrimo z == False = nEsimoPrimoBis x y (z+1)
                     | x /= y && esPrimo z == False = nEsimoPrimoBis x y (z+1)
                     | x /= y && esPrimo z == True = nEsimoPrimoBis x (y+1) (z+1) 

-- Ejercicio 17
esFibonacci :: Integer -> Bool
esFibonacci n = intermedia n 1

intermedia :: Integer -> Integer -> Bool
intermedia n m | fibonacci m < n = intermedia n (m+1)
               | fibonacci m > n = False
               | fibonacci m == n = True

-- Ejercicio 18
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar x | modulo8 x == True = 8
                 | modulo8 x == False && modulo6 x == True = 6
                 | modulo8 x == False && modulo6 x == False && modulo4 x == True = 4
                 | modulo8 x == False && modulo6 x == False && modulo4 x == False && modulo2 x == True = 2
                 | modulo8 x == False && modulo6 x == False && modulo4 x == False && modulo2 x == False = -1

modulo8 x | x < 10 && x /= 8 = False
          | mod x 10 == 8 = True
          | mod x 10 /= 8 = modulo8 (div x 10)

modulo6 x | x < 10 && x /= 6 = False
          | mod x 10 == 6 = True
          | mod x 10 /= 6 = modulo6 (div x 10)

modulo4 x | x < 10 && x /= 4 = False
          | mod x 10 == 4 = True
          | mod x 10 /= 4 = modulo4 (div x 10)

modulo2 x | x < 10 && x /= 2 = False
          | mod x 10 == 2 = True
          | mod x 10 /= 2 = modulo2 (div x 10)

-- Ejercicio 19
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos x = esSumaInicialDePrimosBis x 1 2

esSumaInicialDePrimosBis :: Integer -> Integer -> Integer -> Bool
esSumaInicialDePrimosBis x y z | x < z = False
                               | x > z = esSumaInicialDePrimosBis x (y+1) (z + nEsimoPrimo (y+1))
                               | x == z = True

-- Ejercicio 21
pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras 0 n r = conteoPares 0 n r 0
pitagoras m n r = conteoPares m n r 0 + pitagoras (m-1) n r 

conteoPares :: Integer -> Integer -> Integer -> Integer -> Integer
conteoPares m n r w | n == 0 && m^2 > r^2 = 0
                    | n == 0 && m^2 <= r^2 = w+1
                    | m^2 + n^2 <= r^2 = conteoPares m (n-1) r (w+1)
                    | m^2 + n^2 > r^2 = conteoPares m (n-1) r w

--UNIDAD 5
-- Ejercicio 1
longitud :: [t] -> Integer
longitud [a] = 1
longitud (x:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo (x:xs) | longitud (x:xs) == 1 = x
              | otherwise = ultimo xs

principio :: [t] -> [t]
principio [t] = []
principio (x:xs) = x : principio xs

reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio 2
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | x == n = True
                   | otherwise = pertenece n xs

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [a] = True
todosIguales (x:xs) | x == (ultimo xs) = todosIguales (principio (x:xs))
                    | otherwise = False


