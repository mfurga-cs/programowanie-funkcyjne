{- Lab5 -}

{-# LANGUAGE DeriveFunctor #-}

hello = putStr "Your name? " >>
       getLine >>=
       \x -> putStrLn $ "Hello, " ++ x

hello' = do
  putStr "Your name? "
  x <- getLine
  putStrLn $ "Hello, " ++ x


actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

doEcho3 :: IO ()
doEcho3 = do
  l1 <- getLine
  l2 <- getLine
  putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \x -> let num = read x :: Int in
           if num == 7     then putStrLn "Ah, lucky 7!"
           else if odd num then putStrLn "Odd number! That's most people's choice..."
           else                 putStrLn "Hm, even number? Unusual!"

doDialog :: IO ()
doDialog = do
  putStr "What is your happy number? "
  l <- getLine
  n <- return (read l :: Int)
  if n == 7     then putStrLn "Ah, lucky 7"
  else if odd n then putStrLn "Odd number! That's most people's choice..."
  else               putStrLn "Hm, even number? Unusual!"

twoQuestions :: IO ()
twoQuestions = putStr "What is your name? "
               >> getLine
               >>= \l1 ->
                 putStr "How old are you? "
                 >> getLine
                 >>= \l2 ->
                   print (l1, l2)

doTwoQuestions :: IO ()
doTwoQuestions = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name, age)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n' then return []
  else do
    xs <- getLine'
    return (x:xs)

nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n - 1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good night, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence' :: [IO ()] -> IO ()
sequence' [] = return ()
sequence' (a:as) = do a
                      sequence' as

-- Functor

newtype Box a = MkBox a deriving (Show, Functor)

{-
instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)
-}

data MyList a = EmptyList | Cons a (MyList a) deriving (Show, Functor)

{-
instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)
-}

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
  fmap f EmptyBT = EmptyBT
  fmap f (NodeBT x l r) = NodeBT (f x) (fmap f l) (fmap f r)

-- Applicative

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

newtype MyTriple a = MyTriple (a, a, a) deriving Show

instance Functor MyTriple where
  fmap f (MyTriple (a, b, c)) = MyTriple (f a, f b, f c)

instance Applicative MyTriple where
  pure = \x -> MyTriple (x, x, x)
  (MyTriple (f1, f2, f3)) <*> (MyTriple (a, b, c)) = MyTriple (f1 a, f2 b, f3 c)


