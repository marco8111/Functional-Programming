import Data.Char

{--exercicio 1__Utilizando uma lista por compreensão, 
crie uma expressão que calcula a soma dos primeiros cinquentas quadrados inteiros.--}

quadrados  = sum[x^2 | x <- [1.. 50]]


{--exercicio 2__Um triplo (x, y, z) de inteiros positivos é pitagórico se ele satisfaz a equação
X^2+ y^2 = z^2. Usando uma lista por compreensão com três geradores, definir
uma função pitagora :: Int -> [(Int, Int, Int)] que retorna a lista de todos esses
triplos cujos componentes são, no máximo, um determinado limite. Por exemplo:
> pitagora 10--}
pitagora :: Int -> [(Int, Int, Int)]
pitagora num = [(x, y, z) | x <- [1 .. num], y <- [1 .. num], z <- [1 .. num], x^2 + y^2 == z^2]


{--exercicio 3__Crie uma função utilizando String por compreensão que converte uma String em
maiúsculas.--}
converter :: String -> String
converter xs = [toUpper x | x <- xs]


{--Suponha que um grade de coordenadas de tamanho m × n é dado pela lista de
todos os pares (x, y) de inteiros tal que
Usando uma lista por compreensão, define uma função grade :: Int -> Int -> [(Int,
Int)] que retorna uma grade de coordenadas de um determinado tamanho. Por
exemplo:
> grade 1 2
[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]--}
grade :: Int -> Int -> [(Int, Int)]
grade a b = [(a, b) | a <- [0.. a], b <- [0.. b]]