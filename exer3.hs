--PROGRAMA PARA COMPARAR NUMEROS DIFERENTES--
maior :: Int -> Int -> Int -> (Int)
maior x y z = if x > y && x > z then (x) else if y > x && y > z then (y) else (z)