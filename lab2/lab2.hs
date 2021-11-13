{- Lab2 -}

myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x, y, z) = x + y + z

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z

-- Curry

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f(x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f(x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- Sections

fiveToPower_ :: Integer -> Integer
fiveToPower_ x = (5 ^ x)

_ToPower5 :: Num a => a -> a
_ToPower5 x = (x ^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 n = 5 - n

subtr5From_ :: Num a => a -> a
subtr5From_ x = x - 5

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x

-- Lists

isPalindrome :: [Char] -> Bool
isPalindrome l = l == reverse l

getElemAtIdx :: [a] -> Int -> a
getElemAtIdx l i = head (drop i l)


-- List comprehensions

-- Napisać wyrażenie obliczające, ile jest w przedziale [1,100] trójek liczb
-- całkowitych reprezentujących długości boków trójkąta prostokątnego

triangles :: Int
triangles = length [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a ^ 2 + b ^ 2 == c ^ 2]

-- Czy poniższa definicja funkcji (sprawdzającej, czy liczba jest pierwsza) jest
-- poprawna?

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

-- isPrime 1 == True ?

-- Napisać wyrażenie obliczające, ile jest w przedziale [1,10000]
-- liczb pierwszych

primes = length [n | n <- [2..10000], isPrime n == True]

primes2 :: [Int]
primes2 = eratoSieve [2..]
  where
    eratoSieve :: [Int] -> [Int]
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]


-- Recursion

fib :: (Num a, Eq a) => a -> a
-- fib n = if n == 0 || n == 1 then n
--         else fib (n - 1) + fib (n - 2)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if x == a then True
                 else elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = x * 2 : doubleAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then x : selectEven xs
                    else selectEven xs


sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + x) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + x) xs


sumSquares :: Num a => [a] -> a
sumSquares = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + x^2) xs


prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + 1) xs


qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart xs = [ y | y <- xs, y <= x ]
    rightPart xs = [ y | y <- xs, y > x ]

qSort2 :: Ord a => [a] -> [a]
qSort2 [] = []
qSort2 (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart xs = filter (<=x) xs
    rightPart xs = filter (>x) xs


concat' :: [[a]] -> [a]
concat' x = [z | y <- x, z <- y]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

isSorted :: [Int] -> Bool
isSorted [x] = True
isSorted (x:xs) = x <= head xs && isSorted xs


reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _ = False


