{- Lab3 -}

import Data.List
import Data.Char

f1 = \x -> x - 2

f2 = \x y -> sqrt (x^2 + y^2)

f3 = (\x y z -> sqrt (x^2 + y^2 + z^2))

t1 = (\x -> 2 * x) -- (2*)
t2 = (\x -> x * 2) -- (*2)
t3 = (\x -> x ^ 2) -- (2^)
t4 = (\x -> 2 ^ x) -- (^2)
t5 = (\x -> 2 / x) -- (2/)
t6 = (\x -> x / 3) -- (/3)
t7 = (\x -> 4 - x) -- (4-)

sqrt' = \x -> x ** (1/2)
abs' = \x -> if x >= 0 then x else -x
log' = \x -> log x
id' = \x -> x
const' = \x y -> x

-- f7 x = if x `mod` 2 == 0 then True else False
f7 = (\x -> if x `mod` 2 == 0 then True else False)

-- f8 x = let y = sqrt x in 2 * y ^ 3 * (y + 1)
f8 = (\x -> let y = sqrt x in 2 * y ^ 3 * (y + 1))

-- f9 1 = 3
-- f9 _ = 0
f9 = \x -> case x of
              1 -> 3
              _ -> 0

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum2 x = sumWith id x
sumSqr x = sumWith (^2) x
sumCube x = sumWith (^3) x
sumAbs x = sumWith abs x

listLength x = sumWith (\x -> 1) x

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod2 x = prodWith id x
prodSqr x = prodWith (^2) x
prodCube x = prodWith (^3) x
prodAbs x = prodWith abs x

funcFactory n = case n of
  1 -> id
  2 -> (^2)
  3 -> (^3)
  4 -> \x -> x^4
  5 -> intFunc
  _ -> const n
  where
    intFunc x = x^5

-- expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = \x -> (sum [((x ** i) / product [1..i]) | i <- [0..n]])

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f x) / h

funcList :: [Double -> Double]
funcList = [\x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)


sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs
-- sortDesc xs = reverse (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g (x:xs) = f x == g x && are2FunsEqAt f g xs
-- are2FunsEqAt f g xs = map f xs == map g xs

onlyEven [] = []
onlyEven (x:xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise      = onlyEven xs


onlyUpper [] = []
onlyUpper (s:ss)
  | isUpper s = s : onlyUpper ss
  | otherwise = onlyUpper ss

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs

doubleElems [] = []
doubleElems (x:xs) = x * 2 : doubleElems xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x + acc) g xs


prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x * acc) g xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z = go z f
  where
    go acc f [] = acc
    go acc f (x:xs) = go (f x acc) f xs

sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

map2 f xs = foldr (\x acc -> f x : acc) [] xs
map3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs
filter2 f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs
filter3 f xs = foldl (\acc x -> if f x then acc ++ [x] else acc) [] xs
foldl2 f z xs = foldr (flip f) z (reverse xs)
foldr2 f z xs = foldl (flip f) z (reverse xs)

isSortedAsc :: Ord a => [a] -> Bool
-- isSortedAsc xs = foldr (\(x,y) acc -> x <= y && acc) True (zip xs (tail xs))
isSortedAsc xs = all (==True) $ zipWith (<=) xs $ tail xs

everySecond :: [t] -> [t]
everySecond xs = foldr (\(x, y) acc -> if odd y then x:acc else acc) [] (zip xs [1..])

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
