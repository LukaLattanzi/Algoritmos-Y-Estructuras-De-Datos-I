-- Funciones para practicar haskell:

-- Funcion sumaVectores, suma dos vectores de dos dimensiones.

sumaVectores :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b) -- Probar con (1,2) (3,4) | Resultado: (4,6)
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Funcion sumaTuplas, suma los elementos de una lista de tuplas de dos dimensiones.

sumaTuplas :: [(Int, Int)] -> [Int] -- Probar con [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)] | Resultado: [4,7,6,8,11,4]
sumaTuplas array = [a + b | (a, b) <- array]

-- Funcion fibonacci, devuelve el numero de fibonacci de un numero.

fibonacci :: Int -> Int -- Probar con 10 | Resultado: 55
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)