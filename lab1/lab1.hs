{- Lab 1 -}

sqr :: Double -> Double
sqr x = x * x

{- Return the lenght of the two-dimension vector. -}
vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

{- Return the lenght of the three-dimension vector. -}
vec3DLen :: (Double, Double, Double) -> (Double)
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

{- Swap parameters. -}
swap :: (Int, Char) -> (Char, Int)
swap (x, y) = (y, x)

{- Check whether the given parameters are equal. -}
threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (y == z)

{- Functions. -}

sgn :: Int -> Int
sgn x = if x < 0 then -1
        else if x == 0 then 0
        else 1

absInt :: Int -> Int
absInt x = if x >= 0 then x
           else -x

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x <= y then x
                 else y

toLower :: Char -> Char
toLower x = if y >= 0x41 && y <= 0x5A then toEnum (y + 0x20) :: Char
            else x
            where y = fromEnum x

{- Guards -}
absInt2 :: Int -> Int
absInt2 x | x >= 0 = x
          | otherwise = -x

sgn2 :: Int -> Int
sgn2 x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) | (x <= y) && (x <= z) = x
                  | (y <= x) && (y <= z) = y
                  | (z <= x) && (z <= y) = z

{- -}

not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

printHello = putStrLn "Hello"

main = printHello

