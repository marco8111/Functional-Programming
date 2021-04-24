import Data.Char
{--Funções que recebem 2 numeros e dá o maior, 2 funções 
que recebem funções mais do que 2 numeros 
e chama-se função dentro da função para a comparação!!
Usando GUARDAS e EXPRESSÕES CONDICIONAIS --}
maiorC :: Int -> Int -> Int
maiorC x y = if x > y then x else if y > x then y else y

maiorG :: Int -> Int -> Int
maiorG w z  | w > z = w
            | z > w = z
            | otherwise = z


juncao :: Int -> Int -> Int -> Int 
juncao a b c  = maiorC a (maiorG b c)

juncaox :: Int -> Int -> Int -> Int -> Int 
juncaox d e f g = maiorG g (juncao d e f)


--Função que recebe 1 inteiro e retorna o se nome !!--
transformar :: Int -> String
transformar q | q == 0 = "ZERO"
              | q == 1 = "UM"
              | q == 2 = "DOIS"
              | q == 3 = "TRES"
              | q == 4 = "QUATRO"
              | q == 5 = "CINCO"
              | q == 6 = "SEIS"
              | q == 7 = "SETE"
              | q == 8 = "OITO"
              | q == 9 = "NOVE"
              | otherwise = "NAO EE UM DIGITO" 

{-- função que fornecido o salário bruto de um funcionário retorna o salário
 a receber, tendo em conta que todos os funcionários com salário igual ou
superior a 11 mil escudos terão desconto de imposto de 7% e igual ou superior a
35 mil escudos será de 12%.--}

salariob :: Double -> Double
salariob h | h >= minimo = minimocalculo
           | h >= maximo = maximocalculo
           where
               minimo = 11.000
               maximo = 35.000
               minimocalculo = h - ((7 * h) / 100)
               maximocalculo = h - ((12 * h) / 100)


{--Expressao que calcula soma dos quadrados dos primeiros 50 numeros inteiros--}
quadrados = sum[x^2 | x <- [1.. 50]]

{--Usandoa logica da expressão anterior, agora para 
calcular o quadrado de numeros de 1 ate um numero "n"--}

quadradon m n = sum[x^2 | x <- [m.. n]]



{--Um triplo (x, y, z) de inteiros positivos é pitagórico se ele satisfaz a equação
X^2+ y^2 = z^2. Usando uma lista por compreensão com três geradores, definir
uma função pitagora :: Int -> [(Int, Int, Int)] que retorna a lista de todos esses
triplos cujos componentes são, no máximo, um determinado limite.--}
pitagora :: Int -> [(Int, Int, Int)]
pitagora n = [(x, y, z) | x <- [1.. n], y <- [1.. n], z <- [1.. n], x^2 + y^2 == z^2 ]


{--Crie uma função utilizando String por 
compreensão que converte uma String emmaiúsculas.--}
maiusculas :: [Char] -> [Char]
maiusculas cs = [toUpper xs | xs <- cs ]



{--Suponha que um grade de coordenadas de tamanho m × n é dado pela lista de
todos os pares (x, y) de inteiros tal que (((0 <= x <= m && 0 <= y <= n)))
Usando uma lista por compreensão, define uma função grade :: Int -> Int -> [(Int,
Int)] que retorna uma grade de coordenadas de um determinado tamanho.--}
grade :: Int -> Int -> [(Int, Int)]
grade m n= [(x, y) | x <- [0.. m], y <- [0.. n] ]

