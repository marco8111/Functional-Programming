{-PROGRAMA PARA COMPARAR 3 VALORES DIFERENTES E RETORNAR O MAIOR DELES
.USE EXPRESSÕES CONDICIONAIS, ANINHADAS E EQUAÇÕES COM GUARDAS, RESPECTIVAMENTE-}

fnc1 :: Int -> Int -> Int -> (Int)
fnc2 :: Int -> Int -> Int -> (Int)

fnc1 x y z = if x > y && x > z then (x) else
                if y > x && y > z then (y) else 
                    if z > x && z > y then (z) else x

fnc2 x y z | x > y && x > z = (x)
           | y > x && y > z = (y)
           | otherwise = (z)