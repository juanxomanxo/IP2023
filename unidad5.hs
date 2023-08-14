-- Ejercicio 1
--1. 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--2.
ultimo [a] = a
ultimo (x:xs) = ultimo xs
--3.
principio [t] = [] 
principio (x:xs) = x : principio xs
--4. 
reverso [t] = [t]
reverso (x:xs) = ultimo (x:xs) : reverso (principio (x:xs))


--Ejercicio 2
--1.
pertenece n [] = False
pertenece n (x:xs) | n == ultimo (x:xs) = True
                   | otherwise = pertenece n (principio (x:xs))

--2.
todosIguales [a] = True
todosIguales (x:xs) | x == ultimo xs = todosIguales xs
                    | otherwise = False
--3.
todosDistintos [a] = True
todosDistintos (x:xs) | barrido (x:xs) == True = todosDistintos (principio (x:xs))
                      | otherwise = False

barrido [a] = True
barrido (x:xs) | x /= ultimo (x:xs) = barrido xs
               | otherwise = False 

--4. 
hayRepetidos (x:xs) | todosDistintos (x:xs) == False && todosIguales (x:xs) == False = True
                    | otherwise = False

--5. 
quitarUno n (x:xs) | x == n = xs 
                   | otherwise = x : quitarUno n xs

--6. 
quitarTodos _ [] = []
quitarTodos n (x:xs) | x == n = quitarTodos n xs 
                     | x /= n = x : quitarTodos n xs 

--7. no estuve lejos que sacarlo yo xD
eliminarRepetidos [t] = [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

--8.
mismosElementos (x:xs) (l:ls) | mismosElementosA (eliminarRepetidos (x:xs)) (eliminarRepetidos (l:ls)) == True && mismosElementosB (eliminarRepetidos (x:xs)) (eliminarRepetidos (l:ls)) == True = True
                              | otherwise = False


mismosElementosA [] (l:ls) = True
mismosElementosA (x:xs) (l:ls) | pertenece x (l:ls) == True = mismosElementosA xs (l:ls)
                               | otherwise = False

mismosElementosB (x:xs) [] = True
mismosElementosB (x:xs) (l:ls) | pertenece l (x:xs) == True = mismosElementosB (x:xs) ls
                               | otherwise = False

--9.
capicua (x:xs) | mod (longitud (x:xs)) 2 == 0 = capicuaPar (x:xs) 
               | mod (longitud (x:xs)) 2 /= 0 = capicuaImpar (x:xs)

capicuaPar [] = True
capicuaPar (x:xs) | x == ultimo xs = capicuaPar (principio xs)
                  | otherwise = False

capicuaImpar [a] = True
capicuaImpar (x:xs) | x == ultimo xs = capicuaImpar (principio xs)
                    | otherwise = False


-- Ejercicio 3
--1.
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2.
productoria [] = 1
productoria (x:xs) = x * productoria xs

--3. 
maximo [a] = a
maximo (x:xs) = maximoDos (ultimo (x:xs)) (principio (x:xs)) 
        

maximoDos n [] = n
maximoDos n (x:xs) | n >= ultimo (x:xs) = maximoDos n (principio (x:xs))
                   | otherwise = maximoDos x xs

--4. notese que lo hice bien, pero no supe que a la parte del renglon 98, el "n+a" habia que ponerle corchetes, despues todo bien.
sumarN n [a] = [n+a]
sumarN n (x:xs) = (n+x) : sumarN n xs   

--5.
sumarElPrimero (x:xs) = sumarN x (x:xs)

--6
sumarElUltimo (x:xs) = sumarN (ultimo (x:xs)) (x:xs)

--7.
pares [] = [] 
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | mod x 2 /= 0 = pares xs

--8. 
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | mod x n /= 0 = multiplosDeN n xs

--9. TODO 
--ordenar [a] = [a]
--ordenar (x:xs) =     

minimo [a] = a  
minimo (x:xs) | x < ultimo xs = minimo (principio xs)
              | x > ultimo xs = minimo xs

--Ejercicio 4, este lo leí, xD
--1.
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:xs) | x == ' ' && head xs == ' ' = sacarBlancosRepetidos xs
                             | otherwise = x : sacarBlancosRepetidos xs

--2.
contarPalabras [] = 0
contarPalabras [a] = 1
contarPalabras (x:xs) | x /= ' ' = contarPalabras xs
                      | x == ' ' = 1 + contarPalabras xs
