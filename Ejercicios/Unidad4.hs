-- Ejercicio 1
fibonacci(0) = 0
fibonacci(1) = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


--Ejercicio 2
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n - 1) + 1


-- Ejercicio 3
esDivisible a b | a == 0 = True
                | a < b = False
                | a < 0 = esDivisible (a+b) b
                | otherwise = esDivisible (a-b) b



--Ejercicio 4
sumaImpares n | n == 1 = 1
              | n > 1 = n_esimoImpar + sumaImpares(n-1)
              where n_esimoImpar = 2*n - 1



--Ejercicio 5
medioFact x | x <= 0 = 1
            | x > 0 = x * medioFact(x-2)


--Ejercicio 6
ultimoDig n = mod n 10

sumaDigitos n | n == 0 = 0
              | n > 0 = ultimoDig n + sumaDigitos(div n 10)


--Ejercicio 7
todosDigitosIguales n | n == mod n 10 = True
                      | mod (div n 10) 10 /= mod n 10 = False
                      | otherwise = todosDigitosIguales (div n 10)



--Ejercicio 8 
cantDigitos n | n == 0 = 0
              | n /= 0 = cantDigitos(div n 10) + 1

iesimoDigito n i = mod (div n (10^(cantDigitos (n) - i))) 10


-- Ejercicio 9, La primera es la de Luloide, pero esta mal
esCapicua n | mod n 10 == iesimoDigito n 1 = True
            | otherwise = False

reverso n | read (reverse (show n)) == n = True
          | otherwise = False


-- Ejercicio 10
f1 n | n == 0 = 1
     | n > 0 = 2^n + f1(n-1)

f2 n q | n == 1 = q 
       | n > 0 = q^n + f2 (n-1) q 

f3 n q | n == 1 = q 
       | n > 0 = f2 (2*n) q

f4 n q | n == 1 = q 
       | n > 0 = (f2 (2*n) q) - f1 n-1


-- Ejercicio 11
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1) 

numeroE n | n == 0 = 1
          | n > 0 = (1 / factorial n) + numeroE (n-1)

e = numeroE 10


-- Ejercicio 12
denominador n | n == 0 = 0.5
               | n >= 1 = 2 + (1 / (denominador(n-1)))

raizDe2 n = 1 + 1 / (denominador n)


-- Ejercicio 13 
primerSumatoria n m | n == 0 = 0
                    | n > 0 = segundaSumatoria n m + primerSumatoria (n-1) m

segundaSumatoria n m | m == 0 = 0 
                     | m > 0 = n^m + segundaSumatoria n (m-1)


-- Ejercicio 14 
sumaPotencias q n m = (f2 n q) * (f2 m q)

-- Ejercicio 15
sumatoriaInterna n m | m == 1 = n
                     | m > 1 = n / m + sumatoriaInterna n (m-1)

sumaRacionales n m | n == 0 = 0
                   | n > 0 = sumatoriaInterna n m + sumatoriaInterna (n-1) m
                   

-- Ejercicio 16
-- a
menorDivisor n = divisorImpar n 2 

divisorImpar n m | mod n m == 0 = m
                 | otherwise = divisorImpar n (m+1)

-- b
esPrimo n = primalidad n 2

primalidad n m | (n /= m) && (mod n m) == 0 = False
               | (n == m) && (mod n m == 0) = True
               | otherwise = primalidad n (m+1) 

-- c
sonCoprimos n m | n > m && euclides n m == 1 = True
                | m > n && euclides m n == 1 = True
                | otherwise = False

euclides a b | mod a b == 0 = b
             | otherwise = euclides b (mod a b)

-- d
nEsimoPrimo n = quilombo n 2 1

quilombo n x y | esPrimo x == True && y == n = x
               | esPrimo x == True && y < n = quilombo n (x+1) (y+1)
               | esPrimo x == False = quilombo n (x+1) y


-- Ejercicio 17
esFibonacci n = intermedia n 1

intermedia n m | fibonacci m < n = intermedia n (m+1)
               | fibonacci m > n = False
               | fibonacci m == n = True


-- Ejercicio 18
mayorDigitoPar n | moduloOcho n /= -1 = 8
                 | moduloOcho n == -1 && moduloSeis n /= -1 = 6
                 | moduloOcho n == -1 && moduloSeis n == -1 && moduloCuatro n /= -1 = 4
                 | moduloOcho n == -1 && moduloSeis n == -1 && moduloCuatro n == -1 && moduloDos n /= -1 = 2
                 | otherwise = -1

moduloOcho n | n == 0 = -1
             | mod (mod n 10) 8 == 0 = mod n 10
             | mod (mod n 10) 8 /= 0 = moduloOcho (div n 10) 

moduloSeis n | n == 0 = -1
             |mod (mod n 10) 6 == 0 = mod n 10
             | mod (mod n 10) 6 /= 0 = moduloSeis (div n 10) 

moduloCuatro n | n == 0 = -1
               | mod (mod n 10) 4 == 0 = mod n 10
               | mod (mod n 10) 4 /= 0 = moduloCuatro (div n 10) 

moduloDos n  | n == 0 = -1
             | mod (mod n 10) 2 == 0 = mod n 10
             | mod (mod n 10) 2 /= 0 = moduloDos (div n 10) 


-- Ejercicio 19
x = 123 
esSumaInicialDePrimos x = comparacion x 1 0

comparacion x y z | x > z = comparacion x (y+1) (z + nEsimoPrimo (y))
                  | x < z = False
                  | x == z = True


-- Ejercicio 20, no lo entendí
--tomaValorMax n m 

-- Ejercicio 21, este no me salió a mi xD
pitagorasUno x y z | x == 0 = pitagorasDos x y z
                   | otherwise = pitagorasDos x y z + pitagorasUno (x-1) y z

pitagorasDos x y z | y == 0 = pitagorasTres x y z
                   | otherwise = pitagorasTres x y z + pitagorasDos x (y-1) z

pitagorasTres x y z | x^2 + y^2 <= z^2 = 1
                    | otherwise = 0