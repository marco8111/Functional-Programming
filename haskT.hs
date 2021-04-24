{--comp x y = x && y

--comp :: Bool -> Bool -> Bool--
{--comp x y | x == y = x
         | otherwise = False--}
          
{--comp x y | x == y = True
         | otherwise = False--}


--1 Gerador!--
[x^2 | X <-[1..5]]

--2 Geradores--
[(x, y) | x <- [1, 2, 3], y <- [4, 5]]  --x em primeiro lugar--
[(x, y) | y <- [4, 5], x <- [1, 2, 3]]  --y em primeiro lugar--

--gerador dependente de outro(a dependencia deve ser sempre a seguir, o gerador depende do primeiro anterior a ele--)
[(x, y) | x <- [1..3], y <- [x..3]]--}


{--concatenar--
concatenar :: [[a]] -> [a]
concatenar xss  = [x | xs <- xss, x <- xs]



--guardas (teste paridade), (lista por compreensão, cria uma lista de numeros pares)--
[x | x <- [1..10], even x]--}



--guardas (divisores)--
divisor :: Int -> [Int]
divisor n = [x | x <- [1.. n], n `mod` x == 0]



--Numeros Primos--
primo :: Int -> Bool
primo n = divisor n == [1, n]

--Todos primos de 2 até um limite N--
primos :: Int -> [Int]
primos n = [x | x <- [2.. n], primo x]


--Funcão ZIP (criar lista de alunos e notas)--
zip :: [a] -> [b] -> [(a, b)]


--Função ZIP--
pares :: [a] => [(a)]
pares xs = zip xs (tail xs)


-- Funcão ordenado par a par--
ordenado :: Ord a => [a] -> Bool
ordenado xs = and [x <= y | (x, y) <- pares xs]


--Posicão de 1 elemento--
position :: Eq a => a -> [a] -> [Int]
position x xs = [i | (x´, i) <- zip xs [0..], x == x´]


--String por compreensão--

