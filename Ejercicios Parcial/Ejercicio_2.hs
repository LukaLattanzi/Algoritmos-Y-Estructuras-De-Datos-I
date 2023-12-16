-- Funcion inserta, que recibe un elemento y una lista ordenada y devuelve la lista con el elemento insertado en la posicion correcta.

inserta :: (Ord t) => t -> [t] -> [t] -- Probar con 5 [1,2,3,4,6,7,8,9,10] | Resultado: [1,2,3,4,5,6,7,8,9,10]
inserta x [] = [x]
inserta x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : inserta x ys

-- Funcion particion, que recibe un elemento y una lista y devuelve un par de listas, donde la primera contiene los elementos menores o iguales al elemento dado y la segunda los mayores.

particion :: (Ord a) => a -> [a] -> ([a], [a]) -- Probar con 5 [1,2,3,4,6,7,8,9,10] | Resultado: ([1,2,3,4],[6,7,8,9,10])
particion x [] = ([], [])
particion x (y : ys)
  | x <= y = (x : xs, ys)
  | otherwise = (xs, y : ys)
  where
    (xs, ys) = particion x ys

-- Funcion quickSort, que recibe una lista y devuelve la lista ordenada.

qsort :: (Ord a) => [a] -> [a] -- Probar con [5,1,9,4,6,2,8,3,7,0] | Resultado: [0,1,2,3,4,5,6,7,8,9]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    (smaller, larger) = particion x xs