-- Funcion de Ordenamiento por Seleccion

selectionSort :: (Ord a) => [a] -> [a] -- Probar con [5,1,9,4,6,2,8,3,7,0] | Resultado: [0,1,2,3,4,5,6,7,8,9]
selectionSort [] = [] -- La lista vacia ya esta ordenada
selectionSort xs = let x = minimum xs in x : selectionSort (delete x xs)
  where
    delete _ [] = [] -- Elimina la primera ocurrencia de un elemento en la lista
    delete y (x : xs)
      | x == y = xs
      | otherwise = x : delete y xs

-- Funcion de Ordenamiento por Insercion

insertionSort :: (Ord a) => [a] -> [a] -- Probar con [5,1,9,4,6,2,8,3,7,0] | Resultado: [0,1,2,3,4,5,6,7,8,9]
insertionSort [] = [] -- La lista vacia ya esta ordenada
insertionSort (x : xs) = insert x (insertionSort xs)
  where
    insert x [] = [x] -- Inserta un elemento en la posicion correcta de la lista ordenada
    insert x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys

-- Funcion de Ordenamiento Rapido

quickSort :: (Ord a) => [a] -> [a] -- Probar con [5,1,9,4,6,2,8,3,7,0] | Resultado: [0,1,2,3,4,5,6,7,8,9]
quickSort [] = [] -- La lista vacia ya esta ordenada
quickSort (x : xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = [y | y <- xs, y <= x] -- Particiona la lista en elementos menores o iguales al pivote
    larger = [y | y <- xs, y > x] -- Particiona la lista en elementos mayores al pivote

-- Funcion de Ordenamiento por Mezcla

mergeSort :: (Ord a) => [a] -> [a] -- Probar con [5,1,9,4,6,2,8,3,7,0] | Resultado: [0,1,2,3,4,5,6,7,8,9]
mergeSort [] = [] -- La lista vacia ya esta ordenada
mergeSort [x] = [x] -- Un solo elemento ya esta ordenado
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs -- Divide la lista en dos mitades
    merge [] ys = ys -- Si la primera lista está vacia, el resultado es la segunda lista
    merge xs [] = xs -- Si la segunda lista está vacia, el resultado es la primera lista
    merge (x : xs) (y : ys)
      | x <= y = x : merge xs (y : ys) -- Agrega x al resultado y continua con las listas restantes
      | otherwise = y : merge (x : xs) ys -- Agrega y al resultado y continua con las listas restantes
