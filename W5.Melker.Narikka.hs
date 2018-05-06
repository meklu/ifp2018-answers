module W5 where

import System.Random
import Data.List

-- Week 5:
--  - using typeclasses
--  - implementing typeclasses
--  - forcing/laziness
--
-- Useful type classes to know:
--  - Eq
--  - Ord
--  - Show
--  - Num
--  - Functor

------------------------------------------------------------------------------
-- Ex 1: hey, did you know you can implement your own operators in
-- Haskell? Implement the operator %$ that combines two strings like
-- this:
--
-- "aa" %$ "foo" ==> "aafooaa"
--
-- and the operator *! that takes a value and a number and produces a
-- list that repeats the value that many times:
--
-- 3 *! True ==> [True,True,True]

(%$) :: String -> String -> String
x %$ y = x ++ y ++ x

(*!) :: Int -> a -> [a]
n *! val
  |    n <= 0 = []
  | otherwise = val : (n - 1) *! val

------------------------------------------------------------------------------
-- Ex 2: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--
-- allEqual [] ==> True
-- allEqual [1,2,3] ==> False
-- allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [x]      = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

------------------------------------------------------------------------------
-- Ex 3: implement the function secondSmallest that returns the second
-- smallest value in the list, or Nothing if there is no such value.
--
-- NB! If the smallest value of the list occurs multiple times, it is
-- also the second smallest. See third example.
--
-- Examples:
--
-- secondSmallest [1.0] ==>  Nothing
-- secondSmallest [1,2] ==> Just 2
-- secondSmallest [1,1,2] ==>  Just 1
-- secondSmallest [5,3,7,2,3,1]  ==>  Just 2

secondSmallest :: Ord a => [a] -> Maybe a
secondSmallest xs = case sort xs of
  (x:y:xs) -> Just y
  _        -> Nothing

------------------------------------------------------------------------------
-- Ex 4: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add a _class constraint_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey _   []           = []
incrementKey key ((k,v):pairs) = case key == k of
  True -> (k,v+1) : incrementKey key pairs
  _    -> (k,v)   : incrementKey key pairs

------------------------------------------------------------------------------
-- Ex 5: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional


average :: Fractional a => [a] -> a
average xs = foldr (+) 0 xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 6: define an Eq instance for the type Foo below.
--
-- (Do not use `deriving Eq`.)

data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
  Bar   == Bar   = True
  Quux  == Quux  = True
  Xyzzy == Xyzzy = True
  _     == _     = False

------------------------------------------------------------------------------
-- Ex 7: implement an Ord instance for Foo so that Quux < Bar < Xyzzy

instance Ord Foo where
  compare x y
    | x == y    = EQ
    | otherwise = case (x,y) of
      (Quux,_)  -> LT
      (Xyzzy,_) -> GT
      (_,_)     -> case compare y x of
        LT -> GT
        GT -> LT

------------------------------------------------------------------------------
-- Ex 8: here is a type for a 3d vector. Implement an Eq instance for it.

data Vector = Vector Integer Integer Integer
  deriving Show

instance Eq Vector where
  (Vector a b c) == (Vector x y z) = a == x && b == y && c == z

------------------------------------------------------------------------------
-- Ex 9: implementa Num instance for Vector such that all the
-- arithmetic operations work componentwise.
--
-- You should probably check the docs for which methods Num has!
--
-- Examples:
--
-- Vector 1 2 3 + Vector 0 1 1 ==> Vector 1 3 4
-- Vector 1 2 3 * Vector 0 1 2 ==> Vector 0 2 6
-- abs (Vector (-1) 2 (-3))    ==> Vector 1 2 3
-- signum (Vector (-1) 2 (-3)) ==> Vector (-1) 1 (-1)

instance Num Vector where
  (Vector a b c) + (Vector x y z) = Vector (a + x) (b + y) (c + z)
  (Vector a b c) - (Vector x y z) = Vector (a - x) (b - y) (c - z)
  (Vector a b c) * (Vector x y z) = Vector (a * x) (b * y) (c * z)

  negate (Vector a b c) = Vector (-a) (-b) (-c)

  abs    (Vector a b c) = Vector (abs a) (abs b) (abs c)
  signum (Vector a b c) = Vector (signum a) (signum b) (signum c)

  fromInteger a = Vector a a a

------------------------------------------------------------------------------
-- Ex 10: compute how many times each value in the list occurs. Return
-- the frequencies as a list of (frequency,value) pairs.
--
-- Hint! feel free to use functions from Data.List
--
-- Example:
-- freqs [False,False,False,True]
--   ==> [(3,False),(1,True)]

freqs :: (Eq a, Ord a) => [a] -> [(Int,a)]
freqs xs = go (sort xs) 0
  where
    go []  counter = []
    go [x] counter = [(counter + 1, x)]
    go (x:y:xs) counter
      | x /= y     = (counter + 1, x) : go (y:xs) 0
      | otherwise  = go (y:xs) (counter + 1)

------------------------------------------------------------------------------
-- Ex 11: implement an Eq instance for the following binary tree type

data ITree = ILeaf | INode Int ITree ITree
  deriving Show

instance Eq ITree where
  ILeaf            == ILeaf            = True
  (INode av al ar) == (INode bv bl br) = av == bv && al == bl && ar == br
  _                == _                = False

------------------------------------------------------------------------------
-- Ex 12: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq a => Eq (List a)" that compares elements
-- of the lists.

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  Empty         == Empty         = True
  (LNode av an) == (LNode bv bn) = av == bv && an == bn
  _             == _             = False

------------------------------------------------------------------------------
-- Ex 13: Implement the function incrementAll that takes a functor
-- value containing numbers and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll x = fmap (+1) x

------------------------------------------------------------------------------
-- Ex 14: below you'll find a type Result that works a bit like Maybe,
-- but there are two different types of "Nothings": one with and one
-- without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving (Show,Eq)

instance Functor Result where
  fmap _ NoResult     = NoResult
  fmap _ (Failure s)  = Failure s
  fmap f (MkResult a) = MkResult $ f a

------------------------------------------------------------------------------
-- Ex 15: Implement the instance Functor List (for the datatype List
-- from ex 12)

instance Functor List where
  fmap _ Empty       = Empty
  fmap f (LNode v l) = LNode (f v) $ fmap f l

------------------------------------------------------------------------------
-- Ex 16: Fun a is a type that wraps a function Int -> a. Implement a
-- Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of the
-- puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
  fmap f (Fun a) = Fun (\x -> f (a x))

------------------------------------------------------------------------------
-- Ex 17: Define the operator ||| that works like ||, but forces its
-- _right_ argument instead of the left one.
--
-- Examples:
--   False ||| False     ==> False
--   True ||| False      ==> True
--   undefined ||| True  ==> True
--   False ||| undefined ==> an error!
--
-- NB! Do not use any library functions in your definition. Just
-- pattern matching.

(|||) :: Bool -> Bool -> Bool
-- Judging by the exercise description these two solutions aren't allowed:
--x ||| y = y || x
--x ||| y = if y then y else x
-- So, have this instead:
_    ||| True = True
x    ||| _    = x

------------------------------------------------------------------------------
-- Ex 18: Define the function boolLength, that returns the length of a
-- list of booleans and forces all of the elements
--
-- Examples:
--   boolLength [False,True,False] ==> 3
--   boolLength [False,undefined]  ==> an error!
-- Huom! length [False,undefined] ==> 2

boolLength :: [Bool] -> Int
boolLength []         = 0
boolLength (True:xs)  = 1 + boolLength xs
boolLength (False:xs) = 1 + boolLength xs

------------------------------------------------------------------------------
-- Ex 19: this and the next exercise serve as an introduction for the
-- next week.
--
-- The module System.Random has the typeclass RandomGen that
-- represents a random generator. The class Random is for values that
-- can be randomly generated by RandomGen.
--
-- The relevant function in System.Random is
--   random :: (Random a, RandomGen g) => g -> (a, g)
-- that takes a random generator and returns a random value, and the
-- new state of the generator (remember purity!)
--
-- Implement the function threeRandom that generates three random
-- values. You don't need to return the final state of the random
-- generator (as you can see from the return type).
--
-- NB! if you use the same generator multiple times, you get the same
-- output. Remember to use the new generator returned by random.
--
-- NB! the easiest way to get a RandomGen value is the function
-- mkStdGen that takes a seed and returns a random generator.
--
-- Examples:
--  *W5> threeRandom (mkStdGen 1) :: (Int,Int,Int)
--  (7917908265643496962,-1017158127812413512,-1196564839808993555)
--  *W5> threeRandom (mkStdGen 2) :: (Bool,Bool,Bool)
--  (True,True,False)

threeRandom :: (Random a, RandomGen g) => g -> (a,a,a)
threeRandom g =
  let
    a = random g
    b = random (snd a)
    c = random (snd b)
  in
    (fst a, fst b, fst c)

------------------------------------------------------------------------------
-- Ex 20: given a Tree (same type as on Week 3), randomize the
-- contents of the tree.
--
-- That is, you get a RandomGen and a Tree, and you should return a
-- Tree with the same shape, but random values in the Nodes.
--
-- This time you should also return the final state of the RandomGen
--
-- Hint! the recursive solution is straightforward, but requires
-- careful threading of the RandomGen versions.
--
-- Examples:
--  *W5> randomizeTree (Node 0 (Node 0 Leaf Leaf) Leaf) (mkStdGen 1)  :: (Tree Char, StdGen)
--  (Node '\603808' (Node '\629073' Leaf Leaf) Leaf,1054756829 1655838864)
--  *W5> randomizeTree (Node True Leaf Leaf) (mkStdGen 2)  :: (Tree Int, StdGen)
--  (Node (-2493721835987381530) Leaf Leaf,1891679732 2103410263)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

randomizeTree :: (Random a, RandomGen g) => Tree b -> g -> (Tree a,g)
randomizeTree Leaf g                = (Leaf, g)
randomizeTree (Node _ left right) g =
  let
    gval   = random g
    gleft  = randomizeTree left $ snd gval
    gright = randomizeTree right $ snd gleft
  in
    (Node (fst gval) (fst gleft) (fst gright), snd gright)
