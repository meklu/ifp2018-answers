module W6 where

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List

-- Week 6: Monads

------------------------------------------------------------------------------
-- Ex 1: Here's the ?> chaining operator from the lecture:

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing   -- In case of failure, propagate failure
Just x  ?> f = f x       -- In case of sucess, run the next computation

-- Your task is to help implement the function readName that given a
-- string like "Forename Surname" produces the pair ("Forename",
-- "Surname"). readName should fail (return Nothing) in the following
-- cases:
--
--   1. the input string doesn't contain a space
--   2. both the names are the same
--   3. one of the names doesn't start with a capital letter
--
-- The function readNames has already been implemented using ?>. You
-- need to define the functions split, checkNumber and checkCapitals
-- so that readNames works correctly.

-- DO NOT touch this definition!
readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkDuplicate
  ?>
  checkCapitals

-- split should split a string into two words. If the input doesn't
-- contain a space, Nothing should be returned
--
-- (NB! There are obviously other corner cases like the inputs " " and
-- "a b c", but you don't need to worry about those here)
split :: String -> Maybe (String,String)
split s =
  let
    l = takeWhile (/=' ') s
    r = drop 1 $ dropWhile (/=' ') s
  in
    case (l,r) of
      ([],_) -> Nothing
      (_,[]) -> Nothing
      _      -> Just (l,r)

-- checkDuplicate should take a pair of two strings and return Nothing
-- if they are the same. Otherwise the strings are returned.
checkDuplicate :: (String, String) -> Maybe (String, String)
checkDuplicate (for,sur)
  | for == sur = Nothing
  | otherwise  = Just (for,sur)

-- checkCapitals should take a pair of two strings and return them
-- unchanged if both start with a capital letter. Otherwise Nothing is
-- returned.
checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (for,sur) =
  let
    -- this works too :)
    --ck (c:cs) = c >= 'A' && c <= 'Z'
    ck (c:cs) = isUpper c
  in
    if ck for && ck sur then
      Just (for,sur)
    else
      Nothing

------------------------------------------------------------------------------
-- Ex 2: implement a function myDrop that works just like drop, but
--   1. the arguments are of types Maybe Int and Maybe [a]
--   2. if either of the arguments is Nothing, Nothing is returned
--   3. if the Int is larger than the length of the list or negative,
--      Nothing is returned
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- DO NOT use pattern matching for Maybe.
--
-- Examples:
--  myDrop (Just 1) (Just [5,6,7])
--    ==> Just [6,7]
--  myDrop Nothing (Just [5,6,7])
--    ==> Nothing
--  myDrop (Just 2) Nothing
--    ==> Nothing
--  myDrop (Just 4) (Just [5,6,7])
--    ==> Nothing

myDrop :: Maybe Int -> Maybe [a] -> Maybe [a]
myDrop mi ml =
  let
    ck x l =
      if x < 0 || x > length l then
        Nothing
      else
        Just x
-- just for practice's sake. it works
--in mi >>= (\i -> ml >>= (\l -> ck i l >> Just (drop i l)))
  in do
    i <- mi
    l <- ml
    ck i l
    Just $ drop i l

------------------------------------------------------------------------------
-- Ex 3: given a list of values and a list of indices, return the sum
-- of the values in the given indices. You should fail if any of the
-- indices is too large or too small.
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- DO NOT use pattern matching for Maybe.
--
-- Hint! implementa a function safeIndex :: [a] -> Int -> Maybe a
--
-- Examples:
--  selectSum [2,7,5,3,9] [0,2,4]
--    Just 16
--  selectSum [2,7,5,3,9] [0,2,5]
--    Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _             = Nothing
safeIndex l  i
  | i < 0 || i >= length l = Nothing
  | otherwise              = Just (head (drop i l))

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum _  []     = Just 0
selectSum xs (i:is) = do
  v <- safeIndex xs i
  r <- selectSum xs is
  Just $ v + r

------------------------------------------------------------------------------
-- Ex 4: below you'll find the Logger monad from the lectures.
--
-- Your task is to implement a function binom that computes binomial
-- coefficients recursively with the following formulae:
--
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--   B(n,k) = B(n-1,k-1) + B(n-1,k)
--
-- Every call to the function should be logged as "B(n,k)".
-- Invocations should be logged in execution order.
--
-- Examples:
--   binom 0 0 ==> Logger ["B(0,0)"] 1
--   binom 0 7 ==> Logger ["B(0,7)"] 0
--   binom 1 1 ==> Logger ["B(0,0)","B(0,1)","B(1,1)"] 1
--   binom 2 2 ==> Logger ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"] 1

data Logger a = Logger [String] a
  deriving Show

instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

-- Disregard this instance. In recent versions of the Haskell standard
-- library, all Monads must also be Applicative. This course doesn't
-- really cover Applicative.
instance Applicative Logger where
  pure = return
  (<*>) = ap

msg :: String -> Logger ()
msg s = Logger [s] ()

-- Implement this:
binom :: Integer -> Integer -> Logger Integer
binom n k =
  let
    lok = msg $ "B(" ++ show n ++ "," ++ show k ++ ")"
  in do
    case (n,k) of
      (n,0) -> do lok
                  return 1
      (0,k) -> do lok
                  return 0
      (n,k) -> do l <- binom (n-1) (k-1)
                  r <- binom (n-1) k
                  lok
                  return $ r + l

------------------------------------------------------------------------------
-- Ex 5: using the State monad, write the operation update that first
-- multiplies the state by 2 and then adds one to it. The state has
-- type Int.
--
-- Example:
--  runState update 3
--    ==> ((),7)

update :: State Int ()
update = do
  s <- get
  let ns = s * 2 + 1
  put ns

------------------------------------------------------------------------------
-- Ex 6: using the State monad, walk through a list and add up all the
-- elements in the state. Additionally you should return the length of
-- the list.
--
-- Do this by implementing a recursive State operation lengthAndSum.
-- DO NOT use the functions length or filter.
--
-- Example:
--  runState (lengthAndSum [1,2,3,4]) 0
--    ==> (4,10)

lengthAndSum :: [Int] -> State Int Int
lengthAndSum []     = do
  put 0
  return 0
lengthAndSum (x:xs) = do
  l <- lengthAndSum xs
  s <- get
  put    $ s + x
  return $ l + 1

------------------------------------------------------------------------------
-- Ex 7: Let's use a state of type [a] to keep track of which elements
-- we've seen an odd number of times (1, 3, 5, ...)
--
-- Implement the State operation oddUpdate that updates the state,
-- given an element. In other words, if the element is in the state,
-- remove it, and if it is not in the state, add it.
--
-- Examples:
--   runState (oddUpdate 1) [] ==> ((),[1])
--   runState (oddUpdate 1) [1,2,3] ==> ((),[2,3])
--
-- PS. Order of the list in the state doesn't matter

oddUpdate :: Eq a => a -> State [a] ()
oddUpdate x =
  let
    remove s = takeWhile (/=x) s ++ (tail (dropWhile (/=x) s))
  in do
    s <- get
    case find (==x) s of
      Nothing -> put $ s ++ [x]
      _       -> put $ remove s

------------------------------------------------------------------------------
-- Ex 8: Define the operation oddsOp, so that the function odds
-- returns all the elements of a list that occur an odd number of
-- times.
--
-- Use the oddUpdate operation you just defined.
--
-- Examples:
--   odds [] ==> []
--   odds [1,2,3] ==> [1,2,3]
--   odds [1,1,2] ==> [2]
--
-- PS. Order of the returned list doesn't matter

odds :: Eq a => [a] -> [a]
odds xs = snd (runState (oddsOp xs) [])

oddsOp :: Eq a => [a] -> State [a] ()
oddsOp [] = modify (\x -> x)
oddsOp (x:xs) = do
  oddUpdate x
  oddsOp xs

------------------------------------------------------------------------------
-- Ex 9: implement the function ifM, that takes three monadic
-- operations. If the first of the operations returns True, the second
-- operation should be run. Otherwise the third operation should be
-- run.
--
-- Examples (test is defined below):
--  runState (put 11 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('b',11)
--  runState (put 9 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('a',9)

test :: State Int Bool
test = do
  x <- get
  return (x<10)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  b <- opBool
  if b then opThen else opElse

------------------------------------------------------------------------------
-- Ex 10: the standard library function Control.Monad.mapM defines a
-- monadic map operation. Some examples of using it (safeDiv is defined
-- below):
--
-- mapM (safeDiv 10.0) [1.0,5.0,2.0]  =>  Just [10.0,2.0,5.0]
-- mapM (safeDiv 10.0) [1.0,0.0,2.0]  =>  Nothing
--
-- Your task is to implement the function mapM2 that works like mapM,
-- but there are two lists and the operation takes two arguments. If
-- the lists are of different lists, you can stop processing them once
-- the shorter one ends.
--
-- Examples:
--  mapM2 (\x y -> Just (x+y)) [1,2,3] [6,7]
--    ==> Just [7,9]
--  runState (mapM2 (\x y -> if x then modify (+y) else return () ) [True,False,True] [1,2,4]) 0
--    ==> ([(),(),()],5)

safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0.0 = Nothing
safeDiv x y = Just (x/y)

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 op [] ys         = return []
mapM2 op xs []         = return []
mapM2 op (x:xs) (y:ys) = do
  rest <- mapM2 op xs ys
  this <- op x y
  return $ this : rest

------------------------------------------------------------------------------
-- Ex 11&12: Funnykiztan has cities that are named with by integers
-- 0 .. n-1. Some cities are connected by roads. Your task is to find
-- out if you can can get from city A to city B by following the
-- roads.
--
-- The road network is given as an adjacency list, which means a list
-- of lists [[Int]] where the i'th list gives the cities to which city
-- i has a road to.
--
-- For example the road network:
--
-- 0--1
-- |\ |
-- | \|
-- 2--3
--
-- would be represented as:
--  [[1,2,3]
--  ,[0,3]
--  ,[0,3]
--  ,[0,1,2]]
--
-- Below you'll find the function routeExists that solves the task.
-- However a very important piece of the function, the helper function
-- dfs is still unimplemented.
--
-- The function dfs is intended to run a Depth-First Search. If you
-- don't know what this means, have a look in wikipedia.
--
-- Simply put, dfs uses roads to travel from city to city, using a
-- state of type [Int] to keep track of which cities have been
-- visited. This is important because the road network will have
-- cycles.
--
-- Examples:
--   routeExists example1 0 2  ==> True
--   routeExists example2 0 2  ==> True
--   routeExists example2 3 5  ==> False
--   runState (dfs example2 0) []  ==> ((),[2,3,1,0])
-- When 1 and 2 have already been visited, dfs won't proceed to city 3:
--   runState (dfs example1 0) [1,2] ==> ((),[0,1,2])
--
-- A word on tests. The tests first test the function dfs in a couple
-- of simple situations. After this they test the function routeExists
-- more extensively. The tests look at the state produced by dfs but
-- do not care in which order it is.

-- Three cities, each connected to the two others
example1 :: [[Int]]
example1 = [[1,2]
           ,[0,2]
           ,[0,1]]

-- A more two-part network:
--
-- 0 -- 1
-- |    |   4 -- 5
-- |    |
-- 2 -- 3
example2 :: [[Int]]
example2 = [[1,2]
           ,[0,3]
           ,[0,3]
           ,[1,2]
           ,[5]
           ,[4]]

routeExists :: [[Int]] -> Int -> Int -> Bool
routeExists cities i j = j `elem` execState (dfs cities i) []

dfs :: [[Int]] -> Int -> State [Int] ()
dfs cities i =
  let
    -- you could apparently just use `elem v l` :D
    exists l v = [] /= dropWhile (/=v) l
  in do
    visited <- get
    if exists visited i then do
      return ()
    else do
      put $ i : visited
      let ck = head $ drop i cities
      mapM (dfs cities) ck
      return ()

------------------------------------------------------------------------------
-- Ex 13: define the function orderedPairs that returns all pairs
-- (i,j) such that i<j and i occurs in the given list before j.
--
-- Use the list monad!
--
-- Examples:
--  orderedPairs [1,3,2,4]
--    ==> [(1,3),(1,2),(1,4),(3,4),(2,4)]
--
-- PS. once again the tests don't care about the order of results

orderedPairs :: [Int] -> [(Int,Int)]
orderedPairs xs =
  let
    -- indices' []    _ = []
    -- indices' (_:l) i = i : indices' l (i + 1)
    -- indices = indices' xs 0
    -- read i = head $ drop i xs

    -- This seems like a more sensible - or at least novel - solution
    -- At least someone else found it useful as well :)
    tuples' []    _ = []
    tuples' (v:l) i = (i,v) : tuples' l (i + 1)
    tuples = tuples' xs 0
  in do
    i <- tuples -- indices
    j <- tuples -- indices
    -- let iv = read i
    -- let jv = read j
    -- if i < j && iv < jv then [(iv,jv)] else []
    if fst i < fst j && snd i < snd j then [(snd i,snd j)] else []

------------------------------------------------------------------------------
-- Ex 14: Using the list monad, produce a list of all pairs of
-- _different_ elements in a given list.
--
-- You can assume the list contains no duplicates.
--
-- DO NOT use map, list pattern matching. Use >>= or do notation.
--
-- Examples:
--   pairs [1] ==> []
--   pairs [1,2] ==> [(1,2),(2,1)]
--   pairs [1,2,3] ==> [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
--
-- PS. the order of the returned list does not matter

pairs :: Eq a => [a] -> [(a,a)]
pairs xs = xs >>= (\i -> xs >>= (\j -> if i /= j then [(i,j)] else []))

------------------------------------------------------------------------------
-- Ex 15: the standard library defines the function
--
--   foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--
-- This function behaves like foldr, but the operation used is
-- monadic. foldM f acc xs works by running f for each element in xs,
-- giving it also the result of the previous invocation of f.
--
-- Your task is to implement the functions fsum so that the
-- function sumNotTwice works.
--
-- sumNotTwice computes the sum of a list, but ignores duplicated
-- elements. That is, only the first occurrence of a given number is
-- counted.
--
-- Examples:
--  sumNotTwice [1,1,1,1,1] ==> 1
--  sumNotTwice [5,-2,5]    ==> 3
--  sumNotTwice [1,2,-2,3]  ==> 4

sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM fsum 0 xs) []

fsum :: Int -> Int -> State [Int] Int
fsum acc x =
  let
    -- you could apparently just use `elem v xs` :D
    exists xs v = [] /= dropWhile (/=v) xs
  in do
    seen <- get
    if exists seen x then do
      return acc
    else do
      put $ x : seen
      return $ acc + x

------------------------------------------------------------------------------
-- Ex 16: here is the Result type from last week. Implement a Monad
-- Result instance that behaves roughly like the Monad Maybe instance.
--
-- That is,
--   1. MkResults behave like Just
--   2. If part of computation produces NoResult, the whole computation
--      produces NoResult (just like Nothing)
--   3. Similarly, if we get a Failure "reason" value, the whole
--      computation produces Failure "reason"
--
-- Additionally, the method "fail" of the Monad type class should
-- produce a Failure.
--
-- Examples:
--   MkResult 1 >> Failure "boom" >> MkResult 2
--     ==> Failure "boom"
--   MkResult 1 >> NoResult >> Failure "not reached"
--     ==> NoResult
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

data Result a = MkResult a | NoResult | Failure String deriving (Show,Eq)

-- A straightforward Functor instance
instance Functor Result where
  fmap f (MkResult a) = MkResult (f a)
  fmap _ NoResult = NoResult
  fmap _ (Failure s) = Failure s

-- Disregard this instance. In recent versions of the Haskell standard
-- library, all Monads must also be Applicative. These exercises don't
-- really cover Applicative.
instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return           = MkResult
  -- this wasn't requested *shrug* [editor's note: it was :O]
  fail s           = Failure s

  MkResult a >>= f = f a
  Failure s  >>= _ = Failure s
  NoResult   >>= _ = NoResult

------------------------------------------------------------------------------
-- Ex 17&18: Here is the type SL that combines the State and Logger
-- types. Implement an instance Monad SL, that behaves like the
-- combination of State and Logger. That is, state is propagated from
-- one operation to the next, and log messages are stored in the order
-- they are produced.
--
-- To simplify the type signatures, the type of the state has been set
-- to Int, instead of being a parameter like in the standard State
-- monad.
--
-- This is a tough one. Keep trying and you'll get it!
--
-- You might find it easier to start with the Functor instance
--
-- Examples:
--   runSL (fmap (+1) getSL) 13
--      ==> (14,13,[])
--   runSL (fmap (const True) (msgSL "hello")) 17
--      ==> (True,17,["hello"])
--   runSL (putSL 2 >> msgSL "hello" >> getSL) 0
--      ==> (2,2,["hello"])
--   runSL (replicateM_ 5 (modifySL (+1) >> getSL >>= \x -> msgSL ("got "++show x))) 1
--      ==> ((),6,["got 2","got 3","got 4","got 5","got 6"])

data SL a = SL (Int -> (a,Int,[String]))

-- Run an SL operation with the given starting state
runSL :: SL a -> Int -> (a,Int,[String])
runSL (SL f) state = f state

-- Write a log message
msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((),s,[msg]))

-- Fetch the state
getSL :: SL Int
getSL = SL (\s -> (s,s,[]))

-- Overwrite the state
putSL :: Int -> SL ()
putSL s' = SL (\s -> ((),s',[]))

-- Modify the state
modifySL :: (Int->Int) -> SL ()
modifySL f = SL (\s -> ((),f s,[]))

instance Functor SL where
  --                                             ":D"
  fmap f (SL s) = SL (\x -> let (a,i,l) = s x in (f a,i,l))

-- again, disregard this
instance Applicative SL where
  pure = return
  (<*>) = ap

instance Monad SL where
  return  = \a -> SL (\i -> (a,i,[]))
  s >>= f = SL res
    where
      res s0 = let (a,s1,l) = runSL s s0
                   op2      = f a
               in
                   let (na,ni,nl) = runSL op2 s1
                       fl         = l ++ nl
                   in  (na,ni,fl)
