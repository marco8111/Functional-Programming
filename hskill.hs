mapm :: (a -> b) -> [a] -> [b]
mapm f = foldr ((:).f) [] 



lista_alunos :: [(String, Float)] -> [(String, Float)]
lista_alunos [(nomes, notas)] | notas == bol = [(nomes, notas)]
                                   where 
                                       bol == notas > 10 

suf :: [(String, Float)] -> [(String, Float)]
suf [] = 0
suf (y:ys) = lista_alunos [y] ++ lista_alunos ys