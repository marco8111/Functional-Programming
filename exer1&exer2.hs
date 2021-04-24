
nota x y z = (x + y + z) / 3
pauta nota | nota <= 20 && nota>= 12 = "APROVADO"
           | nota < 12 && nota >= 8 = "EXAME"
           | nota < 8  && nota >= 0 = "REPROVADO"
           | nota < 0 = "NOTA INVALIDA"
           | otherwise = "NOTA INVALIDA"





