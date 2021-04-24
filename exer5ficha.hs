

dtval :: Bool -> Bool -> Bool -> Bool

ano :: Int -> Int
dia :: Int -> Bool
mes :: Int -> Bool

ano x | ano <= 0 = False
      | otherwise = True     

{--dia x | dia > 0 && dia <= 28 || 29 || 30 || 31 = True
      | otherwise = False

mes x | mes > 0 && mes <= 12 = True
      | otherwise = False--}

diamesval dia mes | dia > 0 && dia <= 28 || 29 || 30 || 31 = True
                  | dia == 31 && mes == 1 || 3 || 5 || 7 || 8 || 10 || 12 = True
                  | otherwise = False
                  | mes > 0 && mes <= 12 = True
                  | mes == 2 && dia == 30 || dia == 31 = False



bissexto ano | ano `mod` 400 == 0 || ano `mod` 4 == 0 && ano `mod` 100 /= 0 = True
             | otherwise = False






