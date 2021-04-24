import Data.Char
{-1. Crie uma definição de tipo para modelar os meses do ano e outra para as
estações do ano. -}
data Mes = Janeiro | Fevereiro | Março | Abrir | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
    deriving(Eq, Show)
data Estacao = Inverno | Primavera | Verão | Outono
    deriving(Eq, Show)

{-2. Crie uma função que recebe o tipo meses declarado acima e retorna o tipo
estação.
> estacao Janeiro
Inverno -}
estacao :: Mes -> Estacao
estacao Janeiro = Inverno
estacao Fevereiro = Inverno
estacao Março = Inverno
estacao Abrir = Primavera
estacao Maio = Primavera
estacao Junho = Primavera
estacao Julho = Verão
estacao Agosto = Verão
estacao Setembro = Verão
estacao Outubro = Outono
estacao Novembro = Outono
estacao Dezembro = Inverno  

{-3. Crie uma função com o nome sufixo que permite concatenar uma string
“Sra.” ou “Sr.”, dependendo do sexo da pessoa, para isso utilize a função
map do prelude
> sufixo "Sr." [(“Maria”, “f”), (“Pedro”, “m”), (“Mario”, “m”)]
["Sra.Maria", “Sr.Pedro”, Sr.Mario”]-}
g :: [(String, String)] -> [String]
g [(x,y)] = if y == "m" then map ("Sr." ++) [x] else
          if y == "f" then map("Sra." ++) [x] else []

sufixo :: [(String, String)] -> [String]
sufixo [] = []
sufixo (y:ys) = g[y] ++ sufixo ys

{-4. Utilizando funções de ordem superior construa uma função que recebe uma
lista e retorna a lista invertida-}
lstinvr :: [a] -> [a]
lstinvr [] = []
lstinvr (x:xs)  = lstinvr xs ++ foldr (:) [x] []

{-5. Redefina as funções map f e filter p usando foldr.-}
map :: (a -> b) -> [a] -> [b]
map f =foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p foldr (\ a -> if p a then (a:) else id) []

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
isEmailValid xs = any (== ´@´) xs

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
myPassword 