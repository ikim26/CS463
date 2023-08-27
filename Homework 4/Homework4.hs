-- Name: Isaac Kim
-- NetID: ikim26

-- please add comments to your code below as usual.

module Homework4 where

import Prelude hiding (zipWith,any)
--------------------------------------------------------------------------------

primeFactors :: Int -> [Int]
primeFactors x = primeFactorsHelper x 2 []    --initialize the divisor to 2 (smallest prime number)

 --helper for prime factors
primeFactorsHelper :: Int -> Int -> [Int] -> [Int]
--while x is greater than 1
primeFactorsHelper x divisor factors = if x>1
                                       then (if (mod x divisor) == 0
                                            --if divisor is a factor of our number, add factor to list and divide our number by factor. 
                                            --Then recursively call primeFactorsHelper again
                                             then primeFactorsHelper (div x divisor) divisor (factors ++ [divisor])

                                             --else (divisor is not factor)  increment the divisor and call primeFactorsHelper again
                                             else primeFactorsHelper x (divisor + 1) factors
                                             )
                                       --if we reach here, no more factors so return factors list
                                       else factors
--------------------------------------------------------------------------------

coprime :: Int -> Int -> Bool
--call helper function with prime factors of both inputs as parameters
coprime a b = coprimeHelper (primeFactors a) (primeFactors b)

--helper for coprime
coprimeHelper :: [Int] -> [Int] -> Bool
--if first list is empty return true (base case)
coprimeHelper [] bs = True
--call coprimehelper2 with each element of as and compare with rest of bs
coprimeHelper (a:as) bs = if(coprimeHelper2 a bs)
                          then coprimeHelper as bs  --recursive call for loop
                          else False  --if we are here, we have shared factors, so not coprime

--helper for helper, does comparison to see if any shared factors
coprimeHelper2 :: Int -> [Int] -> Bool
--no more elements to compare (base case)
coprimeHelper2 a [] = True
--if any factors are shared, return false
coprimeHelper2 a (b:bs) = if(a == b)
                          then False
                          else coprimeHelper2 a bs  --recursive call for loop

--------------------------------------------------------------------------------

trib :: Int -> Int
--call helper function with first 3 trib numbers
trib n = tribHelper n [1,1,1]

--helper for trib
tribHelper :: Int -> [Int] -> Int
--if nth trib number is more than what we have initially, loop until we get nth number
tribHelper n seq = if(n > 2)
                   --add number based on sum of left most 3 numbers in array
                   then tribHelper (n-1) ((seq!!0 + seq!!1 + seq!!2):seq)
                   --once we reach nth number, return latest addition to array
                   else seq!!0

--------------------------------------------------------------------------------

maxTwo :: [Int] -> [Int]
--call helper function with sorted array
maxTwo as = maxTwoHelper (quicksort as)

--helper for maxTwo, the input will be sorted
maxTwoHelper :: [Int] -> [Int]
--base case (empty array)
maxTwoHelper [] = []
--check length of array
--if array has more than 2 or more elements
maxTwoHelper as = if ((length as) > 1)
                  --return last two elements since array is sorted from least to greatest
                  then (as!!((length as)-1)):(as!!((length as)-2)):[]
                  --otherwise, return first element in array
                  else ([as!!0])

--quicksort algorithm taken from:
--"Learn You a Haskell for Great Good!" Chapter 5
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
--------------------------------------------------------------------------------

reversed :: [a] -> [a]
reversed as = reversedHelper as []

--helper for reversed
reversedHelper :: [a] -> [a] -> [a]
--once nothing is left, return our reversed list (base case)
reversedHelper [] bs = bs
--add each element in first list to beginning of second list
reversedHelper (a:as) bs = reversedHelper as (a:bs)

--------------------------------------------------------------------------------

clockwise :: [[Int]] -> [[Int]]
--base case for empty array
clockwise [] = []
--call helper with parameters:
--parameters: length of outer array, length of inner arrays, array, return array
clockwise (a:as) = clockwiseHelper (length as) (length a) (a:as) []

--helper function for clockwise
clockwiseHelper :: Int  -> Int -> [[Int]]  -> [[Int]] -> [[Int]]
--base case
clockwiseHelper 0 0 as r = r
--outer loop to loop through each element of each inner array
clockwiseHelper i j as r = if (j > 0)
                           --call 2nd helper to loop through each element of outer array
                           then clockwiseHelper2 0 j as [] r
                           --return array once we are done looping
                           else r

--2nd helper for clockwise
clockwiseHelper2 :: Int  -> Int -> [[Int]] -> [Int] -> [[Int]] -> [[Int]]
--loop through outer array
clockwiseHelper2 i j as temp r = if (i > (length as)-1)
                                 --we are done looping outer, add our new array to list of arrays for return
                                 then clockwiseHelper i (j-1) as (temp:r)
                                 --for each element in outer array, make new array from jth element
                                 --(j-1) is the jth element
                                 else clockwiseHelper2 (i+1) j as (((as!!i)!!(j-1)):temp) r
--------------------------------------------------------------------------------

any :: [Bool] -> Bool
--base case: empty array, return false
any [] = False
--if any of the elements are true
any (b:bs) = if(b)
             --return true
             then True
             --otherwise, keep looping
             else any bs

--------------------------------------------------------------------------------

select :: (a->Bool)-> [a] -> [a]
--call helper function to keep track of items that pass
select f xs = selectHelper f xs []

--helper function for select
selectHelper :: (a->Bool)-> [a] -> [a] -> [a]
--base case: empty array, return whatever p is (can be empty)
selectHelper f [] p = p
--loop through elements of xs, see if x passes f
selectHelper f (x:xs) p = if(f x)
                          --if x passes, add x to p
                          then selectHelper f xs (p ++ [x])
                          --otherwise, go to next element in xs
                          else selectHelper f xs p

                              
--------------------------------------------------------------------------------

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--call helper function that has array to store results
zipWith f xs ys = zipWithHelper f xs ys []

--helper function for zipWithHelper
zipWithHelper :: (a -> b -> c) -> [a] -> [b] -> [c] -> [c]
--base cases
zipWithHelper f [] [] as = as
zipWithHelper f [] ys as = as
zipWithHelper f xs [] as = as
--apply function to each of the elements in the arrays and add to our storage array
zipWithHelper f (x:xs) (y:ys) as = zipWithHelper f xs ys (as ++ [(f x y)])

--------------------------------------------------------------------------------

augdentity :: Int -> Int -> [[Int]]
--call helper with empty matrix
augdentity r c = augdentityHelper 0 r c []

--helper function for augdentity
augdentityHelper :: Int -> Int -> Int -> [[Int]] -> [[Int]]
--i is index for rows
augdentityHelper i r c arr = if(i >= r)
                             --if i is equal to number of rows, we are done, return matrix
                             then arr
                             --otherwise, fill ith column with helper2
                             else augdentityHelper2  i 0 r c arr []

--2nd helper for augdentity
augdentityHelper2 :: Int -> Int -> Int -> Int -> [[Int]] -> [Int] ->[[Int]]
--j is index for columns
augdentityHelper2 i j r c arr temp = if(j < c)
                                     --if row and column numbers match
                                     then (if (i == j) 
                                           --then fill with a 1
                                           then augdentityHelper2 i (j+1) r c arr (temp ++ [1])
                                           --otherwise, fill with a 0
                                           else augdentityHelper2 i (j+1) r c arr (temp ++ [0])
                                          )
                                     --add filled row to our matrix
                                     else augdentityHelper (i+1) r c (arr ++ [temp])

--------------------------------------------------------------------------------