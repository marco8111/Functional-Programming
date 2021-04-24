import Data.Char
{--n = a `div` length xs
    where
       a = 10
       xs = [1, 2, 3, 4, 5]

--Quadrado do dobro--
quaddodobro :: Int -> Int  
quaddodobro z = formul
                  where formul = (2 * z) ^ 2     

--Lados de um triangulo--
ladostriangulo :: Int -> Int -> Int -> Bool
ladostriangulo a b c =
    a + b > c &&
    a + c > b &&
    b + c > a

--numero de escadas--
numdedegraus :: (Integral b, RealFrac a) => a -> a -> b
numdedegraus alturadegrau alturadesejada = 
    ceiling (alturadesejada / alturadegrau )


--Area de 1 circulo [A = pi * r^2]--
areac :: Floating a => a -> a
areac r = pi * r^2--}

{-1. Crie uma definição de tipo para modelar os meses do ano e outra para as
estações do ano. -}


data Meses = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
    deriving(Eq, Show, Ord)
data Estacoes = Primavera | Verao | Outono | Inverno
    deriving(Eq,Show, Ord)

{-2. Crie uma função que recebe o tipo meses declarado acima e retorna o tipo
estação.
> estacao Janeiro
Inverno -}

estacao :: Meses -> Estacoes 

                
estacao Janeiro = Inverno
estacao Fevereiro = Inverno
estacao Marco = Inverno
estacao Abril = Primavera
estacao Maio = Primavera
estacao Junho = Primavera
estacao Julho = Verao
estacao Agosto = Verao
estacao Setembro = Verao
estacao Outubro = Outono
estacao Novembro = Outono
estacao Dezembro = Inverno


{-3. Crie uma função com o nome sufixo que permite concatenar uma string
“Sra.” ou “Sr.”, dependendo do sexo da pessoa, para isso utilize a função
map do prelude
> sufixo "Sr." [(“Maria”, “f”), (“Pedro”, “m”), (“Mario”, “m”)]
["Sra.Maria", “Sr.Pedro”, Sr.Mario”]-}

tupla :: [(String, String)] -> [String]
tupla [(nome, caract)] = if caract == "m" then map ("Sr." ++ ) [nome] else 
                              if caract == "f" then map ("Sra." ++) [nome] else []

sufixo :: [(String, String)] -> [String]
sufixo [] = []
sufixo (y:ys) = tupla[y] ++ sufixo ys


{-4. Utilizando funções de ordem superior construa uma função que recebe uma
lista e retorna a lista invertida-}


listainver :: [a] -> [a]
listainver [] = []
listainver (x:xs)  = listainver xs ++ foldr (:) [x] []


{-5. Redefina as funções map f e filter p usando foldr.-}
--Função MAP--  
mapm::(a->b)->[a]->[b]
mapm f = foldr ( (:).f ) []

--Funçao FILTER--
filterm :: (a -> Bool) -> [a] -> [a]
filterm p = foldr (\ a -> if p a then (a:) else id) []


{-6. Utilizando recursão crie uma função que recebe como argumento um
numero, um caracter e uma lista. O numero deverá indicar a posição na lista
na qual deverá substituir o char existente na lista pelo char passado como
parâmetro.
> remover 2 ‘a’ “bolo”
“bola” -}
remover :: Eq a => Int -> a -> [a] -> [a]
remover d y [] = []
remover d y (x:xs)  | d == 0 = [y] ++ remover (d - 1) y (xs)
                | d > 0 = [x] ++ remover (d - 1) y (xs)  
                | otherwise = [x] ++ remover (d - 1) y (xs)


{-7. Construa uma função que valida a estrutura de uma email.
> isEmailValid “demo@mail“
> isEmailValid “demo@mail“
False
> isEmailValid “demo@mail.“
False
> isEmailValid “demomail.com“
False
> isEmailValid
“demo@mail.cv“
True-}
isEmailValid :: [Char] -> Bool
isEmailValid xs = any (== "@mail.com") [xs]




{-Construa uma função que auxilia o utilizador na escolha de uma password
segura. Para tal a senha deve ter pelo menos um símbolos, número, letra
maiúscula e o tamanho mínimo de seis caracteres.
> myPassword “mindelo“
Fraca
> myPassword “mindelo1“
Fraca
> myPassword “mindelo1$“
Media
> myPassword “Mindelo2021$“
Forte-}

