module Main where

-- Consignas Omega
main = do
  putStrLn "Suma Enteros"
  print (sumaEnteros 2 3)

  putStrLn "Area Circulo"
  print (areaCirculo 2.4)

  putStrLn "Es par o impar"
  print (esPar 2)

  putStrLn "Es par o impar"
  print (esPar 5)

  putStrLn "Factorial"
  print (factorial 5)

  putStrLn "Invertir"
  print (invertir [1, 2, 3, 4, 5])

  putStrLn "Lista ordenada"
  print (listaOrdenada [1, 2, 3, 4, 5])

  putStrLn "Lista ordenada"
  print (listaOrdenada [5, 2, 6, 4, 5])

  putStrLn "Cantidad elementos de una lista"
  print (cantidadElementos [1, 2, 3, 4, 5])

  putStrLn "Cantidad elementos de una lista posiciones pares"
  print (elementosPares [5, 4, 3, 2, 1])

  putStrLn "Máximo comun divisor"
  print (mcd 10 15)

  putStrLn "Suma dígitos"
  print (sumaDigitos 1015)

  putStrLn "Minimo de una lista"
  print (minimo [5, 4, 35, 22, 7])

  putStrLn "Fibonacci"
  print (fibonacci 4)

  putStrLn "Es palindromo opcion 1"
  print (esPalindromo ["n", "e", "u", "q", "u", "e", "n"])

  putStrLn "Es palindromo opcion 2"
  print (esPalindromo2 ["n", "e", "u", "q", "u", "e", "n"])

  putStrLn "Eliminar duplicados de una lista"
  print (eliminarDuplicados [35, 4, 35, 22, 7, 4])

  putStrLn "Producto elementos de una lista"
  print (productoLista [1, 2, 3, 4, 5])

-- 1.Escribir una función que sume dos números enteros.
sumaEnteros :: Int -> Int -> Int
sumaEnteros a b = a + b

-- 2.Implementar una función que calcule el área de un círculo dado su radio.
areaCirculo :: Float -> Float
areaCirculo r = r * r * pi

-- 3.Definir una función que determine si un número es par o impar
esPar :: Int -> Bool
esPar n = mod n 2 == 0

-- 4. Escribir una función que calcule el factorial de un número.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 5.Implementar una función que invierta una lista.
invertir :: [a] -> [a]
invertir [] = []
invertir (x : xs) = invertir xs ++ [x]

-- 6. Definir una función que determine si una lista está ordenada de forma ascendente.
listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [x] = True
listaOrdenada (x : y : xs) = x < y && listaOrdenada (y : xs)

-- 7.Escribir una función que cuente la cantidad de elementos en una lista.
cantidadElementos :: [a] -> Int
cantidadElementos [] = 0
cantidadElementos (x : xs) = 1 + cantidadElementos xs

-- 8.Implementar una función que obtenga los elementos en posiciones pares de una lista.
elementosPares :: [a] -> [a]
elementosPares [] = []
elementosPares [x] = [x]
elementosPares (x : y : xs) = x : elementosPares xs

-- 9.Definir una función que calcule el máximo común divisor de dos números.
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- 10. Implementar una función que calcule la suma de los dígitos de un número entero.
sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

-- 11. Definir una función que encuentre el elemento mínimo en una lista.
menor :: Int -> Int -> Int
menor a b =
  if a < b
    then a
    else b

minimo :: [Int] -> Int
minimo [x] = x
minimo (x : xs) = menor x (minimo xs)

-- 12. Escribir una función que obtenga el enésimo número de la secuencia de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

-- 13.Implementar una función que verifique si una cadena de texto es un palíndromo.
esPalindromo :: [String] -> Bool
esPalindromo s = s == reverse s

esPalindromo2 :: [String] -> Bool
esPalindromo2 [] = True
esPalindromo2 [x] = True
esPalindromo2 (x : xs) = x == last xs && esPalindromo2 (init xs)

-- 14.Definir una función que elimine los duplicados de una lista
eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x : xs) =
  if elem x xs
    then eliminarDuplicados xs
    else x : eliminarDuplicados xs

-- 15. Implementar una función que obtenga el producto de todos los elementos de una lista

productoLista :: [Int] -> Int
productoLista [] = 1
productoLista (x : xs) = x * productoLista xs