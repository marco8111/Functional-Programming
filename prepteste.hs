
som x y | x + y == 10 = "lucky number!!"
        | otherwise = "Still trying!!"

a = b + c
     where b = 2
           c = 3
d = a * 2


k = 10 + 20 + 30 + 40 +
         50 + 60
w = product[10, 20, 30]      

--Função media com tuplos--
media :: (Double, Double) -> Double
media (m, n) = (m + n) / 2

--Funçao curried, mais flexiveis d que tuplas porque pode ser aplicada parcialmente--
soma :: Int -> (Int -> Int)
soma q l = q + l

--Expessões condicionais--
nota :: Double -> (Double -> (Double,String))
nota t i = if mdia <= 20 && mdia >= 12 then (mdia, "Aprovado") else
                    if mdia < 12 then (mdia, "Reprovado") else (mdia, "nota invalida")
                     where mdia = ((t + i) / 2)  


--programa que usa funcao dentro de função, para comparar numeros par a par--
fuc :: Int -> Int -> Int
fuc r s | r > s = r
        | s > r = s
        | otherwise = r
          

maiorxx r s f = fuc r (fuc s f)


--programa que usa let.. in..--
raizes :: Float -> Float -> Float -> [Float]
raizes c h ç | delta > 0  = let r = sqrt delta 
                            in [(-h + r) / (2 * c), (-h - r) / (2 * c)]
             | delta == 0 = [-h / (2 * c)]
             | otherwise  = []
              where delta = h ^ 2 - 4 * c * ç  

--programa com guardas usando where--

calc_imc peso altura | imc <= 18.5 = "abaixo de peso !!"
                     | imc <= 25.0 = "PEso ideal !!"
                     | imc <= 30.0 = "Acima do peso !!"
                     | otherwise = "Muito Acima do peso !!"
                      where imc = peso / altura ^ 2


--Padroes --
--lista--
{-test :: [Char] -> Bool
test [a, _, _] = True
test _  = False
tail :: [a] -> [a] -------- head ::[a] -> a
tail(_ : xs) = xs  --------head (x : _) = x -}
--Para aplicação de padroes são necessário parentesis--

small :: Int -> Bool
small n |n == 0 = True
        |n == 1 = True
        |n == -1 = True
        |otherwise = False

--Tupla--
{-fst :: (a, b) -> a
fst (x, _) -> x-}

--Lista por compreensão--
-- 1 gareador--
[x ^2 | x <- [1.. 5]]
--2 geradores--
[(x, y) | x <- [1.. 5], y <- [6.. 7]]

--Geradores dependentes de geradores anteriores--
[(x, y) | x <- [1.. 3], y <- [x.. 3]]  