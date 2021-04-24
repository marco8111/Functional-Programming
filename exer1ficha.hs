{-PROGRAMA que recebem 2 valores e retorna o maior deles.
use expressões condicionais e equações com guardas, respectivamente-}



funcx x y  = if x > y then (x) else (y)

fnc3 x y z | x > y && x > z = x
           | y > x && y > z = y
           | z > x && z > y = z
           | otherwise = x  || y || z

fnc4 x y z = funcx x (funcx y z)



