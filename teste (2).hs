--Exercicio 1 && 2 --
idade :: Int
idade = 18
inform :: [(String, Int, Bool)] -> String

inform [(nome, idadep, carta)] | idadep > idade && carta == False = "TEM IDADE, MAS NAO TEM CARTA !!"
                               | idadep < idade && carta == True  = "NAO TEM IDADE MAS TEM CARTA !!"
                               | idadep < idade && carta == False = "NAO TEM NEM IDADE E NEM CARTA !!"


--Exercicio 3 --

outros :: [a] -> [a]
outros xs | reverse(reverse(tail[xs]))
