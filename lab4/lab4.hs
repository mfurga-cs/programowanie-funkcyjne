{- Lab4 -}

import Data.Char

-- type Name = String
-- capitalizeName :: Name -> String
-- capitalizeName = map toUpper

-- newtype FirstName = MkFirstName String

-- formatFstName :: FirstName -> String
-- formatFstName (MkFirstName s) = case s of
--   (x:xs) -> toUpper x : map toLower xs
--   [] -> []

-- {-
-- data Person = Person String String deriving (Show)

-- name :: Person -> String
-- name (Person n _) = n

-- surname :: Person -> String
-- surname (Person _ sn) = sn
-- -}

-- data Person = Person { name :: String,
--                        surname :: String
--                      } deriving (Show)

-- data Car a b c = Car
--                     { company :: a
--                     , model :: b
--                     , year :: c
--                     } deriving (Show)

-- carInfo :: (Show a) => Car String String a -> String
-- carInfo (Car {company = c, model = m, year = y}) =
--   "This " ++ c ++ " " ++ m ++ " was mode in " ++ show y

-- data Tree a = Nil |
--               Node a (Tree a) (Tree a)
--               deriving (Eq, Ord, Show, Read)

-- depth :: Tree a -> Int
-- depth Nil = 0
-- depth (Node n lt rt) = 1 + max (depth lt) (depth rt)

-- collapse :: Tree a -> [a]
-- collapse Nil = []
-- collapse (Node n lt rt) = collapse lt ++ [n] ++ collapse rt

-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree f Nil = Nil
-- mapTree f (Node n lt rt) = Node (f n) (mapTree f lt) (mapTree f rt)

polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a)
newtype PolarCoord'' a = MkPolarCoord'' (a, a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

{- Algebraic data types -}

data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec a = MkCart2DVec a a

xCoord' :: Cart2DVec a -> a
xCoord' (MkCart2DVec x _) = x

yCoord' :: Cart2DVec a -> a
yCoord' (MkCart2DVec _ y) = y

-- data Cart2DVec'' a = MkCart2DVec'' { x::a, y::a }

{-
xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' { x = xVal, y = _ }) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' { x = _, y = yVal }) = yVal
-}

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "head': the empty list has no head!"
head' (Cons x xs) = x

data TreeColors = Blue | White | Red
type ActorName = String

leadingActor :: TreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' { x :: a, y :: a, z :: a }

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error "rootValue: the empty tree has no root!"
rootValue (Node v lt rt) = v

{-
data TrafficLights = Red | Orange | Green

actionFor :: TrafficLights -> String
actionFor Red = "No entry!"
actionFor Orange = "No entry and light change!"
actionFor Green = "Entry permission!"
-}

{- Algebraic data types - recursion -}

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT v lt rt) = sumBinIntTree lt + v + sumBinIntTree rt


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT v lt rt) = sumBinTree lt + v + sumBinTree rt


data Expr a = Lit a |  -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add a b) = eval a + eval b

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add a b) = "(" ++ show' a ++ "+" ++ show' b ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT v lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT v lt rt) = flattenBT lt ++ [v] ++ flattenBT rt

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT v lt rt) = NodeBT (f v) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT b lt rt)
  | a == b = NodeBT a lt rt
  | a > b  = NodeBT b lt (insert a rt)
  | a < b  = NodeBT b (insert a lt) rt

occurs :: Eq a => a -> BinTree a -> Int
occurs a EmptyBT = 0
occurs a (NodeBT b lt rt)
  | a == b = 1 + occurs a lt + occurs a rt
  | otherwise = occurs a lt + occurs a rt

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf a EmptyBT = False
elemOf a (NodeBT b lt rt)
  | a == b = True
  | otherwise = elemOf a lt || elemOf a rt

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = list2BST' (NodeBT x EmptyBT EmptyBT) xs
  where list2BST' t [] = t
        list2BST' t (x:xs) = list2BST' (insert x t) xs

{- Instances -}

newtype MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromInteger int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

instance Eq a => Eq (BinTree a) where
  (==) (NodeBT v1 lt1 rt1) (NodeBT v2 lt2 rt2) = (v1 == v2) && (lt1 == lt2) && (rt1 == rt2)
  (==) (EmptyBT) (EmptyBT)                     = True
  (==) _ _ = False
  -- (==) (EmptyBT) (NodeBT _ _ _)                = False
  -- (==) (NodeBT _ _ _) (EmptyBT)                = False



