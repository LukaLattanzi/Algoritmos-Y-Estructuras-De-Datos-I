-- Definir TAD ColaPrioridad
newtype ColaPrioridad a = CP [(a, Int)] deriving (Show)

-- mkqpr, Para instanciar una nueva cola de prioridad vacía
mkqpr :: ColaPrioridad a
mkqpr = CP []

-- addqpr, Agrega un nuevo elemento a la cola de prioridad
addqpr :: (Ord a) => ColaPrioridad a -> a -> Int -> ColaPrioridad a
addqpr (CP cola) elemento clave = CP $ insertOrdenado (elemento, clave) cola
  where
    insertOrdenado :: (Ord a) => (a, Int) -> [(a, Int)] -> [(a, Int)]
    insertOrdenado x [] = [x]
    insertOrdenado x@(el, prio) (y@(el', prio') : ys)
      | prio <= prio' = x : y : ys
      | otherwise = y : insertOrdenado x ys

-- nextqpr, Devuelve el elemento con clave más baja de la cola de prioridad
nextqpr :: ColaPrioridad a -> Maybe a
nextqpr (CP []) = Nothing
nextqpr (CP ((elemento, _) : _)) = Just elemento

-- popqpr, Devuelve una cola de prioridad donde se ha quitado el nextqpr
popqpr :: ColaPrioridad a -> ColaPrioridad a
popqpr (CP []) = CP []
popqpr (CP (_ : xs)) = CP xs

-- Inputs que use para probar el codigo:

-- Instanciar cola de prioridad vacia
-- let colaPrioridad = mkqpr

-- Agregar elementos a la cola de prioridad
-- colaPrioridad' = addqpr colaPrioridad "Elemento1" 3
-- colaPrioridad'' = addqpr colaPrioridad' "Elemento2" 1
-- colaPrioridad''' = addqpr colaPrioridad'' "Elemento3" 5

-- colaPrioridad''' | Para ver el contenido de la cola de prioridad

-- Devuelve el elemento con clave mas baja de la Cola de prioridad
-- nextqpr colaPrioridad'''

-- Devuelve una cola de prioridad donde se ha quitado el nextqpr
-- popqpr colaPrioridad'''