--Programa para tornar inteiro em String--

num :: Int -> [Char]

num n | n == 1 = ["UM"]
      | n == 2 = ["DOIS"]
      | n == 3 = ["TRES"]
      | n == 4 = ["QUATRO"]
      | n == 5 = ["CINCO"]
      | n == 6 = ["SEIS"]
      | n == 7 = ["SETE"]
      | n == 8 = ["OITO"]
      | n == 9 = ["NOVE"]
      | n == 10 = ["DEZ"]
      | otherwise = ["ZERO"]


             


