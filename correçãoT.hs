import Data.Char

-- exer1 e exer2 Programa para receber dados de uma pessoa e retornar uma msg de acordo com sua idade e posse de carta --
idade :: Int
idade = 18

inform :: (String, Int, Bool) -> (String)
inform (nomex, idadex, cartax) |idadex > idade && cartax == False = "Tem idade e nao carta"
                               |idadex < idade && cartax == True = "nao idade e sim carta"
                               |idadex < idade && cartax == False = "nao idade e nao carta"
                               |otherwise = "sim idade e sim carta"

informM (nomeM, idadeM, cartaM) | idadeM >= 18 && cartaM == True = "sim idade e sim carta"
                                | otherwise = "Nao idade e nao carta"





--EXER3 Program para tomar como argumento uma lista e retornar a lista sem o ultimo elemento--
outros :: [a] -> [a]
outros xs = if null xs == False then reverse (tail (reverse xs)) else if length xs == 1 then [] else [] 


---Correspondecia por padrão--

outrosx [] = []
outrosx [x] = []
outrosx (x:xs) = xs







--EXER4 - Defina uma função que dada uma lista retorna a quantidade de letras minúsculas.--
minusculas :: [Char] -> Int
minusculas cs = length [x | x <- cs, isLower x]



{--EXER5 Programa que se comporta da mesma forma que o tail
exceto que ele mapeia a lista vazia para si em vez de produzir um erro. Utilizando tail e a
função null :: [a] -> Bool que decide se uma lista está vazia ou não--}
--Com expressão condicional--
igualtail :: [a] -> [a] 
igualtail xss = if null xss == False then tail xss else []

--com guardas--
igualtail2 xss | null xss == False = tail xss               
               | otherwise = []

--correspondencia por padrao--
igualtail3 [] = []
igualtail3 (_:xs) = xs



