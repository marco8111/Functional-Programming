--pauta :: [(String, Int)] -> [(String, Int, String)--
aluno1 = [("marco", 14)]
aluno2 = [("maria", 12)]--}

{--ps :: [(String, Int)] -> [(String, Int, String)]
seconds ps = [y | (_, y) <- ps]
seconds ps | seconds ps <= 20 && seconds ps >= 12 = ps ++ "Aprovado"
           | seconds ps < 12 && seconds ps >= 8 = ps ++ "Exame"
           | seconds ps < 8 = ps ++ "Reprovado"
           | otherwise = ps ++ "Nota inválida"--}

--parametros das funçoes é que devem ser usadas dentro das funções-- 
paluno :: [(String, Int)] -> [(String, Int, String)]
paluno [(m, n)] | n <= 20 && n >= 12 = [(m, n, "Aprovado")]
                | n < 12 && n >= 8 = [(m, n, "Exame")]
                | n < 8 && n >= 0 = [(m, n, "Reprovado")]
                | otherwise = [(m, n, "Nota Invalida")]


resultado = paluno aluno1