module EstructurasDatos where

-- 23. Parejas: dadas dos listas de la misma longitud, genera la lista de duplas
parejas :: [a] -> [b] -> [(a, b)]
parejas [] _ = []
parejas _ [] = []
parejas (x:xs) (y:ys) = (x, y) : parejas xs ys

-- 25. Aplica: recibe una función y una lista, aplicando la función a cada elemento
aplica :: (a -> b) -> [a] -> [b]
aplica _ [] = []
aplica f (x:xs) = f x : aplica f xs

-- 26. Circuitos Lógicos
type Entradas = (Bool, Bool)

andLogico :: Entradas -> Bool
andLogico (x, y) = x && y

orLogico :: Entradas -> Bool
orLogico (x, y) = x || y

notLogico :: Bool -> Bool
notLogico x = not x

-- 27. Árbol Binario y funciones básicas
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

-- Cantidad de nodos de un árbol
cantNodos :: Arbol a -> Int
cantNodos Vacio = 0
cantNodos (Nodo _ izq der) = 1 + cantNodos izq + cantNodos der

-- Profundidad de un árbol
profundidad :: Arbol a -> Int
profundidad Vacio = 0
profundidad (Nodo _ izq der) = 1 + max (profundidad izq) (profundidad der)