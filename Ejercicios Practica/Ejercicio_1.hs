-- 1. Desarrollar las siguientes funciones:

-- Funcion doubleNumber, si el numero ingresado es menor o igual a 100, lo multiplica por 2, sino devuelve el mismo numero.

doubleNumber :: (Ord a, Num a) => a -> a -- Probar con 50 | Resultado: 100
doubleNumber x = if x <= 100 then x * 2 else x

-- Funcion first, toma un par ordenado, y devuelve su primer componente.

first :: (a, b) -> a -- Probar con (1,2) | Resultado: 1
first (x, y) = x

-- Funcion sign, devuelve el signo de un numero.

sign :: (Ord a1, Num a1, Num a2) => a1 -> a2 -- Probar con -5 | Resultado: -1
sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

-- Funcion xor, devuelve el xor entre dos valores booleanos.

xor :: Bool -> Bool -> Bool -- Probar con True False | Resultado: True
xor x y = (x || y) && not (x && y)

-- Funcion max3, toma tres numeros enteros y devuelve el maximo entre ellos.

max3 :: (Ord a, Num a) => a -> a -> a -> a -- Probar con 1 2 3 | Resultado: 3
max3 x y z = max x (max y z)

-- Funcion swap, toma un par ordenado y devuelve el par con sus componentes invertidas.

swap :: (b, a) -> (a, b) -- Probar con (1,2) | Resultado: (2,1)
swap (x, y) = (y, x)

-- Funcion factorial, calcula el factorial de un numero.

factorial :: (Integral a) => a -> a -- Probar con 5 | Resultado: 120
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- Funcion bisiesto, indica si un año es bisiesto.

bisiesto :: Int -> Bool -- Probar con 2020 | Resultado: True
bisiesto year
  | (year `mod` 4 == 0) && (year `mod` 100 /= 0) || year `mod` 400 == 0 = True
  | otherwise = False