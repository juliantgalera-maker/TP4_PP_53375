module DefinicionesFuncionales where

-- 1. Potencia usando esquemas de patrones (sin usar operadores prohibidos)
potencia :: Int -> Int -> Int
potencia b 0 = 1
potencia b e = b * potencia b (e - 1)

-- 2. Valor absoluto
valAbsoluto :: Int -> Int
valAbsoluto x = if x < 0 then -x else x

-- 3. Sumatoria de los N primeros números naturales
sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n - 1)

-- 4 y 5. Divisor usando restas sucesivas (aplicando guardas)
divisor :: Int -> Int -> Bool
divisor a b 
  | b < a     = False
  | b == a    = True
  | otherwise = divisor a (b - a)

-- 6. Máximo Común Divisor (MCD) usando algoritmo de restos (mod)
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- 7 y 8. Funciones Lambda (para probar en consola interactiva GHCi):
-- Mayor de dos valores: (\x y -> if x > y then x else y) 5 8
-- Verificar si es par: (\x -> x `mod` 2 == 0) 4