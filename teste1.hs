--Exercicio 1 --
idade :: Int
idade = 18
type Nome = String
type Idadep = Int
type Carta = Bool

type Pessoa = (Nome, Idadep, Carta)
teste = ("Marco", 21, True)
pessoa(nome, idadep, carta) | idadep >= idade && carta == False = "TEM IDADE, MAS NAO TEM CARTA !! "
                            | idadep < idade && carta == True  = "NAO TEM IDADE MAS TEM CARTA !! "
                            | idadep < idade && carta == False =  "NAO TEM NEM IDADE E NEM CARTA !!"
                            | otherwise = "tem idade e carta"


resultado = pessoa(teste)


inform :: (String, Int, Bool) -> (String)
inform (nomex, idadex, cartax) |idadex >= idade && cartax == False = "Tem idade e nao carta"
                               |idadex < idade && cartax == True = "nao idade e sim carta"
                               |idadex < idade && cartax == False = "nao idade e nao carta"
                               |otherwise = "sim idade e sim carta" 




--Exercicio 3 --
outros :: [a] -> [a]
outros xs = if null xs == False then reverse (tail (reverse xs)) else if length xs == 1 then [] else []

outrosx :: [a] -> [a]
outrosx [] = []

outrosx (xs:_) = xs


-- Exercicio 4 --


--exercicio 5--
 
igualtail :: [a] -> [a] 
igualtail xss = if null xss == False then tail xss else if length xss == 1 then [] else []


