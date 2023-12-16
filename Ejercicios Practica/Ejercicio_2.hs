-- 2. Desarrolle las funciones de manera recursiva:

-- Funcion suma, suma todos los elementos de una lista de numeros

suma :: (Num a) => [a] -> a -- Probar con [1,2,3,4,5] | Resultado: 15
suma [] = 0
suma [x] = x
suma (x : xs) = x + suma xs

-- Funcion alguno, si alguno de los elementos de una lista de valores booleanos es True, devuelve True, sino devuelve FalseS

alguno :: [Bool] -> Bool -- Probar con [True, False, True, False] | Resultado: True
alguno [] = False
alguno (x : xs)
  | x = True
  | otherwise = alguno xs

-- Funcion todos, si todos los elementos de una lista de valores booleanos son True, devuelve True, sino devuelve False 

todos :: [Bool] -> Bool -- Probar con [True, False, True, False] | Resultado: False
todos [] = True
todos (x : xs)
  | not x = False
  | otherwise = todos xs

-- Funcion restos, dado un numero y una lista de numeros, devuelve la lista de los restos de la division del numero dado por cada uno de los numeros de la lista

restos :: (Integral t) => t -> [t] -> [t] -- Probar con 5 [1,2,3,4,5,6,7,8,9,10] | Resultado: [1,2,3,4,0,1,2,3,4,0]
restos x [] = []
restos x [y] = [y `mod` x]
restos x (y : ys) = y `mod` x : restos x ys

-- Funcion cuadrados, dada una lista de numeros, devuelve la lista de los cuadrados de esos numeros

cuadrados :: (Num a) => [a] -> [a] -- Probar con [1,2,3,4,5,6,7,8,9,10] | Resultado: [1,4,9,16,25,36,49,64,81,100]
cuadrados [] = []
cuadrados [x] = [x * x]
cuadrados (x : xs) = x * x : cuadrados xs

-- Funcion longitudes, dada una lista de listas, devuelve la lista de las longitudes de esas listas

longitudes :: [[a]] -> [Int] -- Probar con ["Hola", "Mundo", "Haskell"] | Resultado: [4,5,7]
longitudes [] = []
longitudes [x] = [length x]
longitudes (x : xs) = length x : longitudes xs

-- Funcion orden, dada una lista de pares ordenados de numeros, devuelve la lista de aquellos pares cuya primera componente es menor que el triple de la segunda

orden :: [(Int, Int)] -> [(Int, Int)] -- Probar con [(1,2), (3,4), (5,6), (7,8), (9,10)] | Resultado: [(1,2),(3,4),(5,6)]
orden [] = []
orden ((x, y) : xs)
  | x < 3 * y = (x, y) : orden xs
  | otherwise = orden xs

-- Funcion pares, dada una lista de numeros, devuelve la lista de aquellos que son pares

pares :: (Integral a) => [a] -> [a] -- Probar con [1,2,3,4,5,6,7,8,9,10] | Resultado: [2,4,6,8,10]
pares [] = []
pares (x : xs) = if even x then x : pares xs else pares xs

-- Funcion masDe, dada una lista de listas y un numero, devuelve la lista de aquellas listas que tienen mas de ese numero de elementos

masDe :: [[a]] -> Int -> [[a]] -- Probar con ["Hola", "Mundo", "Haskell"] 4 | Resultado: ["Mundo","Haskell"]
masDe [] _ = []
masDe (x : xs) n
  | length x > n = x : masDe xs n
  | otherwise = masDe xs n
