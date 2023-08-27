{-

load this file and call the main function. Alternatively, run this command
to run just the expression "main", from the terminal, without going
interactive:

    ghc -e "main" Tester4H.hs

All errors and failures count against your score out of 100 points.

You can also use the testFunc function to only test a specific
function (look through the file for the exact name, but it's always
"test_" and the official name from the spec). Example:

    testFunc test_trib

You can also narrow down to a single test case via justTest, e.g. just
the third (0-indexed) test of trib:

    testOne test_trib 3

Be sure to :reload between calls! (shorthand :r works too)


-}

import Homework4
import Test.HUnit
import Control.Exception
import Control.Monad

import Prelude hiding (zipWith,any)


-- glue all test lists together to run them all.
main = runTestTT $ TestList [test_primeFactors,
                             test_coprime,
                             test_trib,
                             test_maxTwo,
                             test_reversed,
                             test_clockwise,
                             test_any,
                             test_select,
                             test_zipWith,
                             test_augdentity
                             ]

-- helper to test a single function. (Just a friendlier name)
testFunc tl = runTestTT tl

-- helper to test a single test of a single function (builds a
-- TestList with the singleton list of just the indexed position
-- requested).
justTest (TestList xs) n = TestList [xs!!n]
-- call this one.
testOne (TestList xs) n = runTestTT $ justTest (TestList xs) n



-- shorthand name for building an "assert equals" test case.
-- used below in building up lists of test cases per function.
tc s a b = TestCase $ assertEqual s a b

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- each item in the list is a test case (tc), given some string
-- description, with the expected answer, and then the expression to
-- be comparing.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- primeFactors
test_primeFactors = TestList [
  tc "primeFactors_0" [2]       $ primeFactors 2,
  tc "primeFactors_1" [2,2]     $ primeFactors 4,
  tc "primeFactors_2" [5]       $ primeFactors 5,
  tc "primeFactors_3" [2,5,5]   $ primeFactors 50,
  tc "primeFactors_4" [2,3,11]  $ primeFactors 66,
  tc "primeFactors_5" [2,2,5,5] $ primeFactors 100,
  tc "primeFactors_6" [463]     $ primeFactors 463,
  tc "primeFactors_7" [2,2,2,2,2,2,2,2,2]  $ primeFactors 512,
  tc "primeFactors_8" [3,3,13]  $ primeFactors 117,
  tc "primeFactors_9" [1117]    $ primeFactors 1117,
  tc "primeFactors_10"[2,2,3,3,5,5,7,7]    $ primeFactors 44100
  ]


-- coprime
test_coprime = TestList [
 tc "coprime_0" True  $ coprime   2    3,
 tc "coprime_1" False $ coprime   5   10,
 tc "coprime_2" False $ coprime  10   15,
 tc "coprime_3" False $ coprime  15   10,
 tc "coprime_4" True  $ coprime  50   61,
 tc "coprime_5" False $ coprime 100  200,
 tc "coprime_6" True  $ coprime  97   98,
 tc "coprime_7" False $ coprime  66  201,
 tc "coprime_8" True  $ coprime 367  463,
 tc "coprime_9" True  $ coprime 330  463,
 tc "coprime_10" True $ coprime 867 5309
 ]

-- trib
test_trib = TestList [
  tc "trib_0"         1 $ trib 0,
  tc "trib_1"         1 $ trib 2,
  tc "trib_2"         3 $ trib 3,
  tc "trib_3"         5 $ trib 4,
  tc "trib_4"         9 $ trib 5,
  tc "trib_5"        17 $ trib 6,
  tc "trib_6"       193 $ trib 10,
  tc "trib_7"       653 $ trib 12,
  tc "trib_8"      1201 $ trib 13,
  tc "trib_9"  37895489 $ trib 30,
  tc "trib_10"797691075 $ trib 35
  ]
  

-- maxTwo
test_maxTwo = TestList [
  tc "maxTwo_0"  [ 5, 4]  $ maxTwo ([1,2,3,4,5,1,2]::[Int]), -- needed a specific argument type
  tc "maxTwo_1"  [ 6, 6]  $ maxTwo [3,6,1,2,6,4],
  tc "maxTwo_2"  [-2,-3]  $ maxTwo [-3,-4,-5,-2,-10],
  tc "maxTwo_3"  [ 5]     $ maxTwo [5],
  tc "maxTwo_4"  []       $ maxTwo ([]::[Int]),
  tc "maxTwo_5"  [50,14]  $ maxTwo [-1,-2,50,-3,14,0,-10],
  tc "maxTwo_6"  [-3,-3]  $ maxTwo [-3,-4,-5,-3,-4,-5],
  tc "maxTwo_7"  [ 1, 0]  $ maxTwo [1,0,-1],
  tc "maxTwo_8"  [-(2^1000)] $ maxTwo [-(2^1000)],
  tc "maxTwo_9"  [23,15]  $ maxTwo [1,4,2,6,5,23,4,5,12,15],
  tc "maxTwo_10" [0,-1]   $ maxTwo [-1,0,-2]
 ]


  -- reversed
test_reversed = TestList [
  tc "reversed_0" [] $ reversed ([]::[Int]),  -- needed a specific argument type
  tc "reversed_1" [5] $ reversed [5],
  tc "reversed_2" [6,2] $ reversed [2,6],
  tc "reversed_3" [3,6,4] $ reversed [4,6,3],
  tc "reversed_4" [[6,4,2],[5],[1,2,3]] $ reversed [[1,2,3],[5],[6,4,2]],
  tc "reversed_5" [1000,999..0] $ reversed [0..1000],
  tc "reversed_6" [4,-2,0,3,-1] $ reversed [-1,3,0,-2,4],
  tc "reversed_7" [False, False, True] $ reversed [True, False, False],
  tc "reversed_8" "dcba" $ reversed "abcd",
  tc "reversed_9" [[6,7,8],[4,5],[1,2,3]] $ reversed [[1,2,3],[4,5],[6,7,8]],
  tc "reversed_10" [4,5,3,2,1] $ reversed [1,2,3,5,4]
  ]

-- clockwise
test_clockwise = TestList [
  tc "clockwise_0" [[3,1],[4,2]] $ clockwise [[1,2],[3,4]],
  tc "clockwise_1" [[5]]         $ clockwise [[5]],
  tc "clockwise_2" []            $ clockwise ([]::[[Int]]), -- needed a specific argument type
  tc "clockwise_3" [[4,1],[5,2],[6,3]] $ clockwise [[1,2,3],[4,5,6]],
  tc "clockwise_4" [[5,3,1],[6,4,2]] $ clockwise [[1,2],[3,4],[5,6]],
  tc "clockwise_5" [[1],[2],[3],[4],[5]] $ clockwise [[1,2,3,4,5]],
  tc "clockwise_6" [[5,4,3,2,1]] $ clockwise [[1],[2],[3],[4],[5]],
  tc "clockwise_7" [[3,2,1],[3,2,1],[3,2,1]] $ clockwise [[1,1,1],[2,2,2],[3,3,3]],
  tc "clockwise_8" [[4,4],[4,4]] $ clockwise [[4,4],[4,4]],
  tc "clockwise_9" [[1,2,3,2,1],[2,3,4,3,2],[3,4,5,4,3],[2,3,4,3,2],[1,2,3,2,1]]
                   $ clockwise [[1,2,3,2,1],[2,3,4,3,2],[3,4,5,4,3],[2,3,4,3,2],[1,2,3,2,1]]
  ]

-- any
test_any = TestList [
  tc "any_0" False $ any ([]::[Bool]),
  tc "any_1" False $ any [False],
  tc "any_2" True  $ any [True],
  tc "any_3" True  $ any [True,False],
  tc "any_4" False $ any [False,False],
  tc "any_5" True  $ any [True,False,True],
  tc "any_6" True  $ any [False,False,True],
  tc "any_7" True  $ any [True,True,True,True,True],
  tc "any_8" True  $ any [False,True,True,True,False],
  tc "any_9" False $ any [False,False,False,False,False,False]
  ]

-- select
test_select = TestList [
  tc "select_0" [2,4] $ select even [1,2,3,4,5] ,
  tc "select_1" [1,3,5] $ select odd  [1,2,3,4,5] ,
  tc "select_2" [6,7,8,9] $ select (\x-> 5<x && x<10) [1..15],
  tc "select_3" [4,6,7,8,9,11,12] $ select (coprime 5) [4..12],
  tc "select_4" [[True, False, True], [True,True],[True]] $ select any [[True, False, True], [False], [True,True],[False,False,False],[True]],
  tc "select_5" [5,5] $ select (==5) [7,6,5,4,3,4,5,6,7],
  tc "select_6" [] $ select even [1,3,5,7,9],
  tc "select_7" [] $ select even ([]::[Int]) ,
  tc "select_8" [10,100] $ select even [10,100],
  tc "select_9" [21] $ select odd [20,21,22]
  ]

-- zipWith
test_zipWith = TestList [
  tc "zipWith_0" [11,12,13,14]$ zipWith (+) [1,2,3,4] [10,10,10,10],
  tc "zipWith_1" [6,8,10,12]$ zipWith (+) [1,2,3,4] [5,6,7,8],
  tc "zipWith_2" [10,15,20] $ zipWith (*) [2,3,4] [5,5,5,5,5],
  tc "zipWith_3" [10,15,20] $ zipWith (*) [2,3,4,5,6,7,8] [5,5,5],
  tc "zipWith_4" [] $ zipWith (*) [1,2,3,4,5] ([]::[Int]),
  tc "zipWith_5" [] $ zipWith (*) ([]::[Int]) [2,3,4,5,56,6,7],
  tc "zipWith_6" [] $ zipWith (*) ([]::[Int]) ([]::[Int]),
  tc "zipWith_7" [True,False,True,False,False] $ zipWith (==) [1,2,3,4,5][1,1,3,3,3],
  tc "zipWith_8" [True,True,False] $ zipWith coprime [330,367,463][463,463,463,463],
  tc "zipWith_9" [3,4,3] $ zipWith (*) [1,2,3] [3,2,1]
  ]


test_augdentity = TestList [
  tc "augdentity_0" [[1]] $ augdentity 1 1,
  tc "augdentity_1" [[1,0],[0,1]] $ augdentity 2 2,
  tc "augdentity_2" [[1,0,0],[0,1,0],[0,0,1]] $ augdentity 3 3,
  tc "augdentity_3" [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]] $ augdentity 4 4,
  tc "augdentity_4" [[1,0,0],[0,1,0]] $ augdentity 2 3,
  tc "augdentity_5" [[1,0],[0,1],[0,0]] $ augdentity 3 2,
  tc "augdentity_6" [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0]] $ augdentity 3 5,
  tc "augdentity_7" [[1,0],[0,1],[0,0],[0,0],[0,0]] $ augdentity 5 2,
  tc "augdentity_8" [[1,0,0,0,0,0]] $ augdentity 1 6,
  tc "augdentity_9" [[1],[0],[0],[0],[0],[0]] $ augdentity 6 1
 ]

-- from http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
