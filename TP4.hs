{-Define uma função que retorna todos os elementos de uma lista excepto o
ultimo;-}
excultimo :: Num a => [a] -> [a]
excultimo [] = []
excultimo [x] = []
excultimo (x:xs) = [x] ++ reverse ( tail (reverse xs))

primeiros :: Num a => [a] -> [a]
primeiros [] = []
primeiros (a:as) = a: primeiros (as)


{-Define uma função que retorna todos os elementos de uma lista de inteiros
que são primos.-}

primos :: [Int] -> [Int]
primos [] = []
primos [1] = [1]
primos (x:xs) = [x] ++ primos prim
                       where prim = [n | n <- xs, primo n ]
                             primo z = divisor z == [1, z]
                             divisor n = [x | x <- [1.. n], mod n x == 0] 



{-Define uma função que verifique se todos os elementos de uma lista são
True. isTrue :: [Bool] -> Bool -}

verdadeiro :: [Bool] -> Bool
verdadeiro [] = True
verdadeiro (x:xs) = x && verdadeiro xs



 

{-Define uma função que retorna uma lista com n elementos idênticos:
identicos :: Int -> a -> [a] -}

identicos :: Int -> a -> [a]
identicos 0 _ = []
identicos n x =  x: identicos (n - 1) x


{-Define uma função que selecione um elemento de uma lista dado a posição:
selectElement :: [a] -> Int -> a -}

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n - 1) 


{-Define uma função que verifique se o elemento pertence á lista:-}

contido :: (Eq a) => a -> [a] -> Bool
contido w [] = False
contido w (x:xs) = if w == x then True else contido w xs


{-Define uma função que agrupa elementos de duas lista em uma única lista.
> agrupaLista [2,5,6] [1,3,4] -> [1,2,3,4,5,6]-}

agrupaLista :: [a] -> [a] -> [a]
agrupaLista [] _ = []
agrupaLista _ [] = []
agrupaLista (a:x) (b:y) = [b,a] ++ agrupaLista x y 


data Talvez a = Rien | Only a
               deriving (Eq, Show)
safediv :: Int -> Int -> Talvez Int
safediv _ 0 = Rien
safediv 0 _ = 0
safediv m n = m `div` n



{-
data Talvez a = Rien | Only a
               deriving (Eq, Show)

safelast :: [a] -> Talvez a
safelast [] = Rien
safelast xs = Only (last xs)
-}