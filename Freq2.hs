import Data.Char
{-1. Utilizando uma lista por compreensão:-}


{-1.1. Dado uma lista e um valor retorna todos os elementos da lista que seja superior ou
igual ao valor.-}

superior :: Ord a => [a] -> a -> [a]
superior xs v = [x | x <- xs, x >= v]



--1.2. Dado uma lista retorna a quantidade de números pares existente na lista--

pares :: [Int] -> Int
pares xs = length [x | x <- xs, even x]



--1.3. Dado uma lista retorna a quantidade de letras existentes na lista.--
cata_letras :: [a] -> Int
cata_letras xs = length [x | x <- xs, isAlpha 'x']




{-2. Utilizando Recursão -}
{-2.1. Escreva uma que calcule a soma de todos os números de 1 a n, onde n é dado
como parâmetro.-}

soma :: Int -> Int
soma 0 = 0
soma n = n + soma (n-1)




{-2.2. Escreva uma função que encontre e retorne o elemento mínimo em uma lista, onde
a lista é dada como parâmetro.-}

{-minimo :: Eq a => [Int] -> Int
minimo 0 = 0
minimo (x:xs) = -}





{-2.3. Escreva uma função que dado uma lista de números inteiros retorna uma sequência
de números de 3 a 9 com limites exclusivos.-}

{-exclusivos :: [Int] -> [Int]
exclusivos [] = 0
exclusivos (x:xs) -}







--3. Utilizando Funções de Ordem superior--
{-3.1. Defina uma função altMap :: (a -> b) -> (a -> b) -> [a] -> [b] que alternadamente
aplique suas duas funções de argumento a elementos sucessivos em uma lista, por
sua vez sobre a ordem. Por exemplo:
> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altmap g f xs








{-3.2. Crie uma função que recebe como parâmetro uma função isApproved e uma lista
contendo o nome e a nota do aluno e deverá retornar uma lista só com alunos
aprovados.-}

isApproved :: (String, Double) -> Bool
isApproved (nome, nota) | nota <= 20 && nota >= 12 = True
                        | otherwise = False
dados :: [(String, Double)]
pauta :: (String, Double) -> (String, Double) -> [(String)]   
pauta isApproved dados | isApproved dados == True = (nome)
                       | otherwise = []                     






--4. Utilizando declaração de tipos e classes:--

{-4.1. Crie um tipo que representa o nome de uma pessoa e outro que represente o
número de telefone.-}
type Nome = String
type Numero = Int


{-4.2. Crie um tipo phonebook que será um lista contendo o nome de uma pessoa e o seu
número de telefone, utilizando os tipo declarados anteriormente.
-}

type Phonebook = [(Nome, Numero)]


{-4.3. Crie uma função inPhoneBook que recebe o tipo número de telefone e retorna o
nome da pessoa a qual pertence o número caso não existir retorna uma mensagem.
-}

inPhoneBook :: Phonebook Numero -> String
inPhoneBook num = if num == Numero then Nome else "numero nao existe"



