module TratamientoListas where

-- 10. Sumatoria de los valores de una lista
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- 11. Cantidad de elementos de una lista (polimórfica)
cantidad :: [a] -> Int
cantidad [] = 0
cantidad (_:xs) = 1 + cantidad xs

-- 12. Último elemento de una lista (resuelto con patrones y guardas)
ultimo :: [a] -> a
ultimo [x] = x
ultimo (_:xs) = ultimo xs
ultimo [] = error "Lista vacía"

-- 13. Inversa de una lista
inversa :: [a] -> [a]
inversa [] = []
inversa (x:xs) = inversa xs ++ [x]

-- 14. Sacar el elemento que ocupa una posición dada (índice 0-based)
sacar :: Int -> [a] -> [a]
sacar _ [] = []
sacar 0 (_:xs) = xs
sacar n (x:xs) = x : sacar (n - 1) xs

-- 15. Eliminar todas las ocurrencias de un elemento en una lista
eliminar :: Eq a => a -> [a] -> [a]
eliminar _ [] = []
eliminar e (x:xs)
  | e == x    = eliminar e xs
  | otherwise = x : eliminar e xs

-- 16. Intersección de dos listas
intersec :: Eq a => [a] -> [a] -> [a]
intersec [] _ = []
intersec (x:xs) ys
  | elem x ys = x : intersec xs ys
  | otherwise = intersec xs ys