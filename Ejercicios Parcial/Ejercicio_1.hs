-- Funcion miZip, que recibe dos listas y devuelve una lista de tuplas con los elementos de ambas listas. Si una de las listas es mas larga que la otra, los elementos sobrantes se descartan.

miZip :: [a] -> [b] -> [(a, b)] -- Probar con [1,2,3,4,5] [6,7,8,9,10] | Resultado: [(1,6),(2,7),(3,8),(4,9),(5,10)]
miZip [] _ = []
miZip _ [] = []
miZip (x : xs) (y : ys) = (x, y) : miZip xs ys

-- Funcion productoEscalar, que recibe dos listas de enteros y devuelve su producto escalar.

productoEscalar :: [Int] -> [Int] -> Int -- Probar con [1,2,3,4,5] [6,7,8,9,10] | Resultado: 130
productoEscalar xs ys = sum [x * y | (x, y) <- miZip xs ys]
