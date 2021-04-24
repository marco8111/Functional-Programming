{-- Programa de pauta, de apenas 1 aluno, com 2 vertentes; com alunos já predefinidos ou de 
alunos  insiridos --}
aluno1 = [("marco", 14)]
aluno2 = [("maria", 7)]

paluno :: [(String, Int)] -> [(String, Int, String)]
paluno [(m, n)] | n <= 20 && n >= 12 = [(m, n, "Aprovado")]
                | n < 12 && n >= 8   = [(m, n, "Exame")]
                | n < 8 && n >= 0    = [(m, n, "Reprovado")]
                | otherwise          = [(m, n, "Nota Invalida")]

resultado1 = paluno aluno1
resultado2 = paluno aluno2                