--Exercicio 1 --
idade :: Int
idade = 18
inform :: [(String, Int, Bool)] -> String

inform nome idadep carta | idadep > idade && carta == False = "TEM IDADE, MAS NAO TEM CARTA !! "
                         | idadep < idade && carta == True  = "NAO TEM IDADE MAS TEM CARTA !! "
                         | idadep < idade && carta == False = "NAO TEM NEM IDADE E NEM CARTA !!" 

--Exercicio 3 -Retirar ultimo elemento--
outros :: [a] -> [[a]]

outros xs = [x | x <- [xs] ]
outros xs = reverse ( tail ( reverse[xs] ) )

-- Exercicio 4 --


--exercicio 5--
igualtail :: [a] -> [a]
null :: [a] -> Bool
igualtail xss = if tail [xss] == [] then null else xss


