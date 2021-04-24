{-PROGRAMA que recebem 2 valores e retorna o maior deles.
use expressões condicionais e equações com guardas, respectivamente-}

func1 :: Int -> Int -> (String, Int)
func2 :: Int -> Int -> (String, Int)

func1 x y  = if x > y then ("maior e", x) else ("o maior e", y)

func2 x y | x > y = ("o maior e", x)
          | x < y = ("o maior e", y)
          | otherwise = ("São iguais", x)
