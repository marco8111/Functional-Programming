--calcular raizes de uma expressão segundo grau--
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | delta > 0 = [(-b + sqrt delta) / (2*a), (-b - sqrt delta) / (2*a)]
             | delta == 0 = [-b / 2* a]
             | otherwise = []
             where delta = b ^ 2 - 4 * a * c

--Exemplo equaçoes com guardas usando where--
calc_imc :: Float -> Float -> String
calc_imc peso altura | imc <= 18.5 = "Abaixo do peso!"
                     | imc <= 25.0 = "Peso ideal"
                     | imc <= 30.0 = "Acima do Peso!"
                     | otherwise = "Muito Acima do peso! "
                     where imc = peso / altura ^ 2



--Exemplo de calculo de raizes com let...in..--
raizeslet :: Float -> Float -> Float -> [Float]
raizeslet a b c| delta > 0 = let r = sqrt delta
                             in [(-b + r) / (2 * a), (-b - r) / (2 * a)]
               | delta ==0 = [-b / (2* a)]
               | otherwise = []
                where delta = b ^ 2 - 4 * a * c


--Comparar numeros--
comp :: Int -> Int -> Bool
comp x y | x == y = True
         | otherwise = False




--funcionario = ("taxista", 9888090, "m", "Mindelo")-- \ == lambda 

--sem expressao lambda--
impares n = map f [0.. n - 1]
     where
         f x = x*2 + 1 

--Mais simplificado--
--expressão lambda--
imparesx n = map (\x -> x*2 + 1) [0.. n-1]


main :: IO ()
main = do
          putStrLn "Qual é o seu nome ?"
          nome <- getLine
          putStr nome 
          putStrLn ", seja bem vindo(a)!!"


strlen :: IO ()
strlen = do  
            putStr "Enter a string:"
            xs <- getLine
            putStr "The string has "
            putStr (show(length xs))
            putStrLn " characters" 



calcular_media :: Integral a => a -> a -> a
                    
calcular_media  z c = div (z + c) 2
                      

{--aprovado :: IO ()
aprovado = do
             putStrLn "Enter primeira nota :"
             z <- getLine
             
             putStrLn "enter segunda nota :"
             c <- getLine
             
             putStr (show(calcular_media z c))--}



f :: [Char] -> Bool
f ('a': _) = True
f _ = False




insert :: [Int] -> [Int]
insert xs = reverse(tail(reverse xs))




insertx :: Int -> Int -> [Int]
insertx z q = z: q: [] 


                  
             

