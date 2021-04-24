somas :: Num a => [a] -> a
somas [] = 0
somas [x] = x
somas (x:xs) = x + sum (xs)

soma :: Int -> Int
soma 0 = 0
soma 1 = 1
soma n = sum [1.. n]

duas_vezes :: (a -> a) -> a -> a
duas_vezes f x = f (f x)

