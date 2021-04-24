--PROGRAMA PARA COMPARAR NUMEROS DIFERENTES--

fnc1 :: Int -> Int -> Int -> (String, Int)
fnc2 :: Int -> Int -> Int -> (String, Int)

fnc1 x y z = if x > y && x > z then ("o maior ", x) else
                if y > x && y > z then ("o maior ",  y) else ("o maior é", z)

fnc2 x y z | x > y && x > z = ("o maior ", x)
           | y > x && y > z = ("o maior  ", y)
           | otherwise = ("o maior ", z)