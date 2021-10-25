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

{- Pattern matching -}

not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = False
nand' (True, False) = True
nand' (False, True) = True
nand' (False, False) = True

{- Case of -}

not'2 :: Bool -> Bool
not'2 x = case x of
            True -> False
            False -> True

absInt3 :: Int -> Int
absInt3 x = case (x >= 0) of
              True -> x
              _ -> -x

isItTheAnswer2 :: String -> Bool
isItTheAnswer2 x = case x of
                    "Love" -> True
                    _ -> False

xor' :: (Bool, Bool) -> Bool
xor' (x, y) = case (x, y) of
                (True, True) -> False
                (True, False) -> True
                (False, True) -> True
                (False, False) -> False

{- Where clause -}
roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ((-b - d) / e, (-b + d) / e)
  where d = sqrt (b * b - 4 * a * c)
        e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / l, y / l)
  where l = sqrt (x * x + y * y)

{- Let in -}
roots2 :: (Double, Double, Double) -> (Double, Double)
roots2 (a, b, c) =
  let d = sqrt (b * b - 4 * a * c)
      e = 2 * a
  in ( (-b - d) / e, (-b + d) / e)

unitVec2D' :: (Double, Double) -> (Double, Double)
unitVec2D' (x, y) =
  let l = sqrt (x * x + y * y)
  in ((x / l), (y / l))

roots3 :: (Double, Double, Double) -> (Double, Double)
roots3 (a, b, c) = ((-b - d) / e, (-b + d) / e)
  where {d = sqrt (b * b - 4 * a * c);
   e = 2 * a
        }

printHello = putStrLn "Hello"
main = printHello

