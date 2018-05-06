module W2 where

-- Week 2:
--
--  * lists
--  * strings
--  * library functions for them
--  * higher order functions
--  * polymorphism
--
-- Functions you will need:
--  * head, tail
--  * take, drop
--  * length
--  * null
--  * map
--  * filter
--
-- You can ask ghci for the types of these functions with the :t
-- command:
--
--  Prelude> :t head
--  head :: [a] -> a

-- Some imports you'll need.
-- Do not add any other imports! :)
import Data.List
import Data.Char

-- Ex 1: Define the constant years, that is a list of the values 1982,
-- 2004 and 2012 in this order.

years = [1982,2004,2012]

-- Ex 2: define the function measure that for an empty list returns -1
-- and for other lists returns the length of the list.

measure :: [String] -> Int
-- probably more sensible
--measure [] = -1
--measure ss = length ss

-- but look, shiny!
measure ss
  | null ss   = -1
  | otherwise = length ss

-- Ex 3: define the function takeFinal, which returns the n last
-- elements of the given list.
--
-- If the list is shorter than n, return all elements.

takeFinal :: Int -> [Int] -> [Int]
takeFinal n xs
  | length xs <= n = xs
  | otherwise      = takeFinal n $ tail xs

-- Ex 4: remove the nth element of the given list. More precisely,
-- return a list that is identical to the given list except the nth
-- element is missing.
--
-- Note! indexing starts from 0
--
-- Examples:
-- remove 0 [1,2,3]    ==>  [2,3]
-- remove 2 [4,5,6,7]  ==>  [4,5,7]
--
-- The [a] in the type signature means "a list of any type"

remove :: Int -> [a] -> [a]
remove i = go i []
  where
    go 0 left (_d:right) = left ++ right
    go i left (mv:right) = go (i - 1) (left ++ [mv]) right

-- Ex 5: substring i n s should return the length n substring of s
-- starting at index i.
--
-- Remember that strings are lists!

substring :: Int -> Int -> String -> String
substring i n s = take n $ drop i s

-- Ex 6: implement the function mymax that takes as argument a
-- measuring function (of type a -> Int) and two values (of type a).
--
-- mymax should apply the measuring function to both arguments and
-- return the argument for which the measuring function returns a
-- higher value.
--
-- Examples:
--
--  mymax (*2)   3       5      ==>  5
--  mymax length [1,2,3] [4,5]  ==>  [1,2,3]
--  mymax head   [1,2,3] [4,5]  ==>  [4,5]

mymax :: (a -> Int) -> a -> a -> a
mymax measure a b
  | measure a > measure b = a
  | otherwise             = b

-- Ex 7: countPalindromes receives a list of strings and returns a
-- count of how many of the strings are palindromes (i.e. how many
-- strings are the same when reversed)
--
-- Remember the functions length, filter and reverse

countPalindromes :: [String] -> Int
countPalindromes ss = length $ filter check ss
  where check s = s == reverse s

-- Ex 8: Implement a function funny, that
--  - takes in a list of strings
--  - returns a string
--    - that contains all input words of length over 5
--    - ... combined into one string
--    - ... separated with spaces
--    - ... and converted to upper case!
--
-- These functions will help:
--  - toUpper :: Char -> Char   from the module Data.Char
--  - intercalate               from the module Data.List
--
-- Example:
--  funny ["boing","functional","foo","haskell"] ==> "FUNCTIONAL HASKELL"

funny :: [String] -> String
funny strings = combine (filter check strings)
  where
    check      s = length s > 5
    combine list = map toUpper (go (head list) (tail list))
      where
        go s []       = s
        go s (h:rest) = go (s ++ " " ++ h) rest

-- Ex 9: powers k max should return all the powers of k that are less
-- than or equal to max. For example:
--
-- powers 2 5 ==> [1,2,4]
-- powers 3 30 ==> [1,3,9,27]
-- powers 2 2 ==> [1,2]
--
-- Hints:
--   * n^max > max
--   * the function takeWhile

powers :: Int -> Int -> [Int]
powers n max = takeWhile (<=max) $ map (n^) [0..]

-- Ex 10: implement a search function that takes an updating function,
-- a checking function and an initial value. Search should repeatedly
-- apply the updating function to the initial value until a value is
-- produced that passes the checking function. This value is then
-- returned.
--
-- Examples:
--
--   search (+1) even 0   ==>   0
--
--   search (+1) (>4) 0   ==>   5
--
--   let check [] = True
--       check ('A':xs) = True
--       check _ = False
--   in search tail check "xyzAvvt"
--     ==> Avvt

search :: (a->a) -> (a->Bool) -> a -> a
search update check initial
  | check initial = initial
  | otherwise     = search update check $ update initial

-- Ex 11: given numbers n and k, build the list of numbers n,n+1..k.
-- Use recursion and the : operator to build the list.

fromTo :: Int -> Int -> [Int]
fromTo n k
  |     n > k = []
  |    n == k = n : []
  | otherwise = n : fromTo (n+1) k

-- Ex 12: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = summarum 0 1
  where
    summarum s p
      |    p == i = s + p : []
      | otherwise = s + p : summarum (s + p) (p + 1)

-- Ex 13: using list pattern matching and recursion, define a function
-- mylast that returns the last value of the given list. For an empty
-- list, a provided default value is returned.
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def []     = def
mylast def [x]    = x
mylast def (x:xs) = mylast def xs

-- Ex 14: define a function that checks if the given list is in
-- increasing order. Use recursion and pattern matching. Don't use any
-- library list functions.

sorted :: [Int] -> Bool
sorted []     = True
sorted [x]    = True
sorted (x:nx:xs)
  | x <= nx   = sorted $ nx:xs
  | otherwise = False

-- Ex 15: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []

sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf xs = go 0 [] xs
  where
    go sum sums []     = sums
    go sum sums (x:xs) = go (sum + x) (sums ++ [sum + x]) xs

-- Ex 16: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only recursion, pattern matching and the list constructors
-- (: and []). Do not use any other list functions.
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge xs ys = go xs ys
  where
    go xs []         = xs
    go [] ys         = ys
    go (x:xs) (y:ys)
      | x < y        = x : go xs (y:ys)
      | otherwise    = y : go (x:xs) ys

-- Ex 17: using the merge function you just defined, implement mergesort
--
-- Mergesort is a sorting algorithm that splits the input list in
-- half, sorts the halves recursively using mergesort, and then merges
-- the halves back together.
--
-- You will probably need two base cases for the recursion. I've
-- filled them in for you already.
--
-- Example:
--   mergesort [4,1,3,2] ==> [1,2,3,4]

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    half = div (length xs + 1) 2
    left  = take half xs
    right = drop half xs

-- Ex 18: define the function mymaximum that takes a list and a
-- comparing function of type a -> a -> Ordering and returns the
-- maximum value of the list, according to the comparing function.
--
-- For an empty list the given default value is returned.
--
-- Examples:
--   mymaximum compare (-1) [] ==> -1
--   mymaximum compare (-1) [1,3,2] ==> 3
--   let comp 0 0 = EQ
--       comp _ 0 = LT
--       comp 0 _ = GT
--       comp x y = compare x y
--   in mymaximum comp 1 [1,4,6,100,0,3]
--     ==> 0

mymaximum :: (a -> a -> Ordering) -> a -> [a] -> a
mymaximum cmp def [] = def
mymaximum cmp def (x:xs) = go x xs
  where
    go max []     = max
    go max (x:xs)
      | cmp x max == GT = go x xs
      | otherwise       = go max xs

-- Ex 19: define a version of map that takes a two-argument function
-- and two lists. Example:
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- Use recursion and pattern matching. Do not use any library functions.
--
-- Ps. this function is in the Haskell Prelude but under a different
-- name.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f _ []  = []
map2 f [] _  = []
map2 f as bs = go [] as bs
  where
    go r [] _          = r
    go r _ []          = r
    go r (a:as) (b:bs) = go (r ++ [f a b]) as bs

-- Ex 20: in this exercise you get to implement an interpreter for a
-- simple language. You should keep track of the x and y coordinates,
-- and interpret the following commands:
--
-- up -- increment y by one
-- down -- decrement y by one
-- left -- decrement x by one
-- right -- increment x by one
-- printX -- print value of x
-- printY -- print value of y
--
-- The interpreter will be a function of type [String] -> [String].
-- Its input is a list of commands, and its output is a list of the
-- results of the print commands in the input.
--
-- Both coordinates start at 0.
--
-- Examples:
--
-- interpreter ["up","up","up","printY","down","printY"] ==> ["3","2"]
-- interpreter ["up","right","right","printY","printX"] ==> ["1","2"]
--
-- Surprise! after you've implemented the function, try running this in GHCi:
--     interact (unlines . interpreter . lines)
-- after this you can enter commands on separate lines and see the
-- responses to them
--
-- Unfortunately the surprise might not work if you've implemented
-- your interpreter correctly but weirdly :(

interpreter :: [String] -> [String]
interpreter commands = go 0 0 commands
  where
    go x y [] = []
    go x y (cmd:commands)
      | cmd == "up"
        = go x (y + 1) commands
      | cmd == "down"
        = go x (y - 1) commands
      | cmd == "left"
        = go (x - 1) y commands
      | cmd == "right"
        = go (x + 1) y commands
      | cmd == "printX"
        = show x : go x y commands
      | cmd == "printY"
        = show y : go x y commands
      | otherwise
-- makes interact behave excessively weirdly
--        = ("Unknown command " ++ cmd) : go x y commands
        = go x y commands
