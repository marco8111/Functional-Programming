--Tipos de função--
{--media :: Integral a => a -> a -> [a]
media (num1, num2) = (num1 + num2) / 2--}



--Função de media e nome alunos--
alunos :: [String]
alunos = ["joao", "paula", "zé"]

notas :: [Double]
notas = [2.3, 4.5, 5.5]


media :: [Double]
media nts = sum (nts) `div` (fromIntegral $ length nts)

resultado :: Double
resultado = media notas