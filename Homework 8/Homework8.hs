{-
Your Name: Isaac Kim
Partner:   Bharath Maniraj 
-}

module Homework8 where

import Control.Monad
import Control.Monad.State      -- State, runState, get, put, guard

data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)
data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

type Name = String
type FamilyTree = [(Name,Name)]


-- =============================================================================

-- given above.
-- data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)

instance Eq a => Eq (SnocList a) where
  -- base case
  (==) Lin Lin = True
  -- This will recursively call this equals as long as there are remaining snoclists
  (==) (Snoc xs x) (Snoc ys y) = (x == y) && (xs == ys)
  --if we reach this pattern, then they aren't equal, return false
  (==) _ _ = False

instance Functor SnocList where
  -- base case is empty snoclist
  fmap f Lin = Lin
  -- recursively apply function to each element of snoclist
  -- then put it in a new snoclist
  fmap f (Snoc xs x) = Snoc (fmap f xs) (f x)

snocLast :: SnocList a -> Maybe a
-- base case
snocLast Lin = Nothing
-- return "first" element of snoclist (since snoclist store last element first)
snocLast (Snoc xs x) = (Just x)

snocProduct :: (Num a) => SnocList a -> a
-- base case
snocProduct Lin = 1
-- recursively call and multiply each element
snocProduct (Snoc xs x) = x * snocProduct xs

snocMax :: (Ord a) => SnocList a -> Maybe a
-- base case
snocMax Lin = Nothing
-- call helper with extra parameter to keep track of max
snocMax (Snoc xs x) = snocMaxHelper xs x

-- helper function for snocMax
snocMaxHelper :: (Ord a) => SnocList a -> a -> Maybe a
-- base case, return y
snocMaxHelper Lin y = Just y
-- compare x element to y
snocMaxHelper (Snoc xs x) y = if(x > y)
                              -- if x greater than y, replace new "max" with x and recursively call
                              then snocMaxHelper xs x
                              -- otherwise, keep y and recursively call again
                              else snocMaxHelper xs y

longestSnocSuffix :: (Eq a) => SnocList a -> SnocList a -> SnocList a
-- base cases (empty snoclists)
longestSnocSuffix Lin _ = Lin
longestSnocSuffix _ Lin = Lin
-- if elements in snoclist are the same
longestSnocSuffix (Snoc xs x) (Snoc ys y) = if(x == y)
                                            -- create new snoclist, add x and recursively call
                                            then Snoc (longestSnocSuffix xs ys) x
                                            -- if elements are different, end snoclist
                                            else Lin

snocZip :: SnocList a -> SnocList b -> SnocList (a,b)
-- base cases (if one of them is empty, return Lin and stop recursive calls)
snocZip Lin _ = Lin
snocZip _ Lin = Lin
-- create snoclist, add tuple, recursively call until one of the lists are empty
snocZip (Snoc xs x) (Snoc ys y) = Snoc (snocZip xs ys) (x,y)

snocify :: [a] -> SnocList a
-- base case
snocify [] = Lin
-- call helper function with reversed list
snocify xs = snocifyHelper (reverse xs)

snocifyHelper :: [a] -> SnocList a
-- base case
snocifyHelper [] = Lin
-- recursively put elements into snoclist
snocifyHelper (x:xs) = Snoc (snocifyHelper xs) x


unSnocify :: SnocList a -> [a]
-- base case
unSnocify Lin = []
-- recursively add each element in the snoclist to our normal array
unSnocify (Snoc xs x) = (unSnocify xs) ++ [x]

uniques :: SnocList Int -> SnocList Int
-- base case (empty list)
uniques Lin = Lin
-- unsnocify snoclist, reverse array, re-snocify (to preserve ordering)
-- call helper function
uniques xs = snocReverse (uniquesHelper (snocify (reverse (unSnocify xs))))

uniquesHelper :: SnocList Int -> SnocList Int
-- base case (empty list)
uniquesHelper Lin = Lin
-- check if element is already in snoclist
uniquesHelper (Snoc xs x) = if(alreadyExists xs x)
                      -- if already exists in snoclist, go next
                      then uniquesHelper xs
                      -- otherwise, add unique to snoclist
                      else Snoc (uniquesHelper xs) x

--helper for checking if an snoclist already has an integer n
alreadyExists :: SnocList Int -> Int -> Bool
-- base case (return false)
alreadyExists Lin _ = False
-- otherwise, see if any of the elements match n
alreadyExists (Snoc xs x) n = if(x == n)
                       -- if they do, return true
                       then True
                       -- recursive call to check every element
                       else alreadyExists xs n

snocReverse :: SnocList a -> SnocList a
-- base case
snocReverse Lin = Lin
-- convert snoclist to array, reverse array, convert back to snoclist
snocReverse xs = snocify (reverse (unSnocify xs))

-- =============================================================================
 
-- given above.
-- data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

instance (Eq a) => Eq (Tree a) where
  -- base case
  (==) L L = True
  -- compare values
  (==) (V a) (V b) = (a == b)
  -- compare branches
  (==) (Br a n b) (Br c m d) = (n == m) && (a == c) && (b == d)
  -- if we reach here, our trees are not equal (patterns are not same in trees)
  (==) _ _ = False

instance (Ord a) => Ord (Tree a) where
  -- compare two leaves
  compare L L = EQ 
  compare L _ = LT  -- empty tree is always less than non-empty tree
  compare _ L = GT  -- non empty tree is always greater than empty tree
  -- compare V nodes
  -- base case
  compare (V x) (V y) = compare x y
  compare (V _) (Br _ _ _) = LT -- V node is less than Br node
  compare (Br _ _ _) (V _) = GT -- Br node is greater than V node
  -- compare branches
  -- compare left tree first (in order traversal)
  compare (Br a n b) (Br c m d) = if((compare a c) == EQ)   -- if right children are equal, compare middle
                                  then if ((compare n m) == EQ) -- if middle are equal
                                       then compare b d  -- compare left children
                                       else compare n m  -- otherwise, tree value depends on middle, compare middle children
                                  else compare a c  -- otherwise, tree value depends on right children, compare right

insertTree :: (Ord a) => a -> Tree a -> Tree a
-- base case (empty tree)
insertTree x L = V x
-- case of V node (convert to branch and insert accordingly)
insertTree x (V y) = if (x <= y)  -- leq because duplicates go left
                     then Br (V x) y L
                     else Br L y (V x)
-- Br case (use case here to avoid long if statements)
insertTree x (Br a y b) = case compare x y of
                                 -- EQ and LT are same bc of duplicates rule
                                 -- recursively call insertTree with respective comparison values
                                 EQ -> Br (insertTree x a) y b
                                 LT -> Br (insertTree x a) y b
                                 GT -> Br a y (insertTree x b)
                                 

inOrder :: Tree a -> [a]
-- base case (empty tree)
inOrder L = []
-- case of V node
inOrder (V x) = [x]
-- case of Br, recursively call left, append n, recursively call right (in order)
inOrder (Br a n b) = inOrder a ++ [n] ++ inOrder b

treeSort :: (Ord a) => [a] -> [a]
-- base case (empty array)
treeSort [] = []
-- call helper
treeSort xs = inOrder(treeSortHelper xs L)

--helper function for treeSort
treeSortHelper :: (Ord a) => [a] -> Tree a -> Tree a
-- base case, return tree
treeSortHelper [] t = t
-- recursively call helper with inserting element of array into tree one at a time
treeSortHelper (x:xs) t = treeSortHelper xs (insertTree x t)

treeMin :: (Ord a) => Tree a -> Maybe a
-- base case (empty tree)
treeMin L = Nothing
-- return Just (first element of sorted array of tree)
treeMin t = Just ((treeSort (inOrder t))!!0)


-- =============================================================================
-- Maybe Monads

-- useful for some testing; not actualy a required definition, and the
-- tester doesn't import this (it makes its own copy).

family = [
  ("Animal", "Object"),
  ("Cat","Animal"),
  ("Dog","Animal"),
  ("Siamese","Cat"),
  ("Calico","Cat"),
  ("Labrador","Dog"),
  ("Pug","Dog"),
  ("Book","Object"),
  ("Garbage","Can")
  ]

-- given above.
-- type Name = String
-- type FamilyTree = [(Name,Name)]


-- Maybe Monad

parent :: Name -> FamilyTree -> Maybe Name
-- base case (empty tree)
parent name [] = Nothing
-- compare name and child node name
parent name ((c,p):xs) = if(c == name)    -- if names match
                         then Just p      -- return parent of node
                         else parent name xs  -- otherwise, recursively call on rest of the tree


ancestors :: Name -> FamilyTree -> Maybe [Name]
-- do block for maybe monad
ancestors name xs = do
  -- get parent of initial child
  -- here, we use Maybe as a monad since if parent returns Nothing, ancestors will immediately return Nothing
  t <- (parent name xs)   
  -- ans = get parents of child recursively
  ans <- (case ancestors t xs of
          Nothing -> Just [t]   -- if we get Nothing, we reached the end of "lineage"
          Just other -> Just (t:other)  -- append parent to beginning of array
        ) 
  -- check for if ancestry ends at "Object"
  if(ans!!((length ans)-1) /= "Object")
  then Nothing  -- if not "Object", return Nothing
  else Just ans -- Otherwise, return Just array
         
headMaybe :: [a] -> Maybe a
-- base case (empty array)
headMaybe [] = Nothing
-- return first element
headMaybe (x:xs) = Just x

leastUpperBound :: Name -> Name -> FamilyTree -> Maybe Name
leastUpperBound n1 n2 tree = do
  -- get all ancestors of n1 and n2
  -- (uses Maybe as monad here by cal ling ancestors which calls parent which uses Maybe as a monad)
  a <- (ancestors n1 tree)
  b <- (ancestors n2 tree)
  -- return first matching pair by calling helper function on arrays
  getFirstCommon (n1:a) (n2:b)

-- helper function for leastUpperBound to return first matching pair of elements
getFirstCommon :: [Name] -> [Name] -> Maybe Name
-- base cases (empty arrays)
getFirstCommon [] _ = Nothing
getFirstCommon _ [] = Nothing
-- compare elements to each other
getFirstCommon (x:xs) ys = case getFirstCommonHelper x ys of
                           -- if we get Nothing, go next element of xs
                           Nothing -> getFirstCommon xs ys
                           -- if we get a match, return that match
                           Just other -> Just other

-- second helper function for leastUpperBound
-- acts as "inner for loop" for matching elements in array
getFirstCommonHelper :: Name -> [Name] -> Maybe Name
-- base case (empty array)
getFirstCommonHelper name [] = Nothing
-- compare element and name
getFirstCommonHelper name (x:xs) = if(name == x)  -- if they match
                                   then Just name -- return Just (element that matches)
                                   else getFirstCommonHelper name xs  -- otherwise, go next element



-- =============================================================================
-- State Monads

tribM :: Int -> State (Int,Int,Int) Int
tribM 0 = do              --0 case, return a w get
  (a, b, c) <- get
  return a
tribM n = do              --n case, increment b and c, put c into a
  (a, b, c) <- get
  put (b, c, a+b+c)     --recurse, subtracting n
  ans <- tribM (n - 1)
  return ans

trib :: Int -> Int      --here we create the driver/initial state
trib n = let initialState = (1, 1, 1) --1 1 1 is where we start the pattern
             comp          = tribM n --trib call
         in case runState comp initialState of --runstate declaration
          (answer, lastStoredState) -> answer
--------------------------------------------------------------------------------

simpleBalanced :: String -> Bool --driver
simpleBalanced s = simpleBalancedHelper s []

simpleBalancedHelper :: String -> [Char] -> Bool --this code first gets an empty stack and input string
simpleBalancedHelper "" stack = null stack --base case
simpleBalancedHelper (p:ps) stack
        | p == '(' = simpleBalancedHelper ps (p:stack)  --if it is any open bracket ( (/[/{/< ) it adds to stack
        | p == ')' && (null stack || head stack /= '(') = False --if it is a closed, it returns false if stack is empty or doesnt match (meaning its unbalanced)
        | p == ')' = simpleBalancedHelper ps (tail stack) ---if balanced we parse and move on
        | p == '[' = simpleBalancedHelper ps (p:stack) --this pattern is repeated for the other possible characters
        | p == ']' && (null stack || head stack /= '[') = False
        | p == ']' = simpleBalancedHelper ps (tail stack)
        | p == '{' = simpleBalancedHelper ps (p:stack)
        | p == '}' && (null stack || head stack /= '{') = False
        | p == '}' = simpleBalancedHelper ps (tail stack)
        | p == '<' = simpleBalancedHelper ps (p:stack)
        | p == '>' && (null stack || head stack /= '<') = False
        | p == '>' = simpleBalancedHelper ps (p:stack)
        | otherwise = simpleBalancedHelper ps (stack) --if not any of the above it is a letter so we parse over it and continue




balancedM :: String -> State [Char] Bool -- same thing as above but with state monad
balancedM []  = do 
  stack <- get -- base case, returns null stack check
  return (null stack)
balancedM ('(' :ps)  = do --if an open bracket/parens variation is found, add to stack
  stack <- get -- we have to call 'get' in each do block, stack is the background variable
  put ('(':stack) -- any put in the first four do blocks is a "push"
  balancedM ps 
balancedM ('[' :ps)  = do
  stack <- get
  put ('[':stack)
  balancedM ps 
balancedM ('{' :ps)  = do
  stack <- get
  put ('{':stack)
  balancedM ps 
balancedM ('<' :ps)  = do
  stack <- get
  put ('<':stack)
  balancedM ps  --4 opens, we now handle the closed options
balancedM (')' :ps) = do --if any closed parens variation is found, we check...
  stack <- get --call get in each do block
  if (null stack || head stack /= '(') --check if it correlates to a null stack or non-matching open parens
    then return False --if so return false, else
    else do
      put (tail stack) --here we are popping by overriding stack w the smaller one
      balancedM ps --call the helper again, and repeat this pattern for every other possible closed pattern
balancedM ('}' :ps) = do
  stack <- get
  if (null stack || head stack /= '{')
    then return False
    else  do
      put (tail stack)
      balancedM ps 
balancedM (']' :ps) = do
  stack <- get
  if (null stack || head stack /= '[')
    then return False
    else do 
      put (tail stack)
      balancedM ps 
balancedM ('>' :ps) = do
  stack <- get
  if (null stack || head stack /= '<')
    then return False
    else do
      put (tail stack)
      balancedM ps 
balancedM ( _ :ps) = do --handles chars
  balancedM ps
  
    
        

balanced :: String -> Bool
balanced s = let initialState = [] --driver and initial state declaration
                 comp          = balancedM s --initial set up, s is the input string
         in case runState comp initialState of --setting up runstate
          (answer, lastStoredState) -> answer

-- =============================================================================
-- List Monads

divisors :: Int -> [Int] 
divisors n = do
  a <- [1..n] -- a becomes list of 1 to n 
  guard $ (mod n a) == 0 -- guard to calculate mod of input number and potential factor
  return a --output of array of factors of n input

geometric :: Int -> Int -> [Int]
geometric n step = do 
  val <- [0..] --val becomes a list of 0 to infinite
  return (n*(step^val)) --transformation of every element in val w help from list monad

mersennes :: [Int]
mersennes = do
  a <- [1..] -- a becomes infinite list of 1 to infinite
  return ((2 ^a) - 1) --return the infinite li st of a element transformations 

unitTriangles :: Int -> [(Int,Int,Int)]
unitTriangles n = do
  a <- [1..n] --assign a b and c as incrementing lists from 1 to n, a to n, and b to n
  b <- [a..n] --this prevents duplicates
  c <- [b..n]
  guard $ a+b>c && a+c>b && b+c>a --guard to filter out possible combinations not adhering to triangle rule
  return (a, b, c) -- return all possible length combinatiosn
--------------------------------------------------------------------------------

