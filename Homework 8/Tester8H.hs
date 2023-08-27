{-

load this file and call main. Alternatively, run this command to run
just the expression "main", from the terminal, without going interactive:

    ghc -e "main" <thisfile.hs>

All errors and failures count against your score out of 100 points.

You can also use the testFunc function to only test a specific
function (look through the file for the exact name, but it's always
"test_" and the official name from the spec). Example:

    testFunc test_trib

You can also narrow down to a single test case via justTest, e.g. just
the third (0-indexed) test of fib:

    testFunc (justTest test_trib 3)

Be sure to :reload between calls! (shorthand :r works too)


-}

-- carefully importing only the required definitions.
import Homework8(SnocList(..),snocLast,snocProduct,snocMax,longestSnocSuffix,
                 snocZip,snocify,unSnocify,uniques,snocReverse,
                 Tree(..),insertTree,inOrder,treeSort,treeMin,
                 parent,ancestors,headMaybe,leastUpperBound,
                 tribM,trib,simpleBalanced,balancedM,balanced,
                 divisors,geometric,mersennes,unitTriangles)


import Test.HUnit
import Control.Exception
import Control.Monad.State
import Data.List(sort)


-- glue all test lists together to run them all.
main = runTestTT $ TestList [
  test_SnocList,
  test_snocLast,
  test_snocProduct,
  test_snocMax,
  test_longestSnocSuffix,
  test_snocZip,
  test_snocify,
  test_unSnocify,
  test_uniques,
  test_snocReverse,
  
  test_Tree,
  test_insertTree,
  test_inOrder,
  test_treeSort,
  test_treeMin,
  
  test_parent,
  test_ancestors,
  test_headMaybe,
  test_leastUpperBound,
  
  test_trib,
  test_simpleBalanced,
  test_balanced,
  
  test_divisors,
  test_geometric,
  test_mersennes,
  test_unitTriangles
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


sampletree = [
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


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


test_SnocList = TestList [
  -- check a bunch of small things are equal all at once in a... well, a regular list.
  tc "snoclist 0" True $ (==) [Lin, Snoc Lin 1, Snoc Lin 5] [Lin, Snoc Lin 1, Snoc Lin 5],
  tc "snoclist 1" False $ (==) (Snoc (Snoc Lin 1) 2) (Snoc (Snoc Lin 2) 1),
  tc "snoclist 2" False $ (==) (Snoc (Snoc Lin 1) 2) (Snoc (Snoc (Snoc Lin 0) 1) 2),

  tc "snoclist 3" (Snoc (Snoc Lin 5)10) $ fmap (*5) (Snoc (Snoc Lin 1) 2),
  tc "snoclist 4" (Snoc (Snoc (Snoc Lin True) False) True )  $  fmap odd (Snoc (Snoc (Snoc Lin 1) 2) 3)
  ]
test_snocLast = TestList [
  tc "snocLast 0" (Just "c") $ snocLast (Snoc (Snoc (Snoc Lin "a") "b") "c"),
  tc "snocLast 1" (Just 5) $ snocLast (Snoc Lin 5),
  tc "snocLast 2" Nothing $ snocLast (Lin::SnocList Int)
  ]

test_snocProduct = TestList [
  tc "snocProduct 0"   1 $ snocProduct Lin,
  tc "snocProduct 1" 120 $ snocProduct (Snoc (Snoc (Snoc (Snoc (Snoc Lin 1)2)3)4)5),
  tc "snocProduct 2"   0 $ snocProduct (Snoc (Snoc (Snoc Lin 4) 0) 10),
  tc "snocProduct 3"  60 $ snocProduct (Snoc (Snoc (Snoc Lin (-3)) (-5)) 4)
  ]
test_snocMax = TestList [
  tc "snocMax 0" Nothing $ snocMax (Lin::SnocList String),
  tc "snocMax 1" (Just 5) $ snocMax (Snoc (Snoc (Snoc Lin 2)5)3),
  tc "snocMax 2" (Just 8) $ snocMax (Snoc (Snoc Lin 8) (-13))
  ]
test_longestSnocSuffix = TestList [
  tc "longestSnocSuffix 0" Lin $ longestSnocSuffix (Snoc (Snoc Lin 2)3) (Snoc (Snoc Lin 4)5),
  tc "longestSnocSuffix 1" (Snoc (Snoc Lin 4) 5)  $  longestSnocSuffix (Snoc(Snoc (Snoc Lin 1)4) 5)  (Snoc (Snoc (Snoc Lin 3)4)5),
  tc "longestSnocSuffix 2" (Snoc (Snoc Lin 4)12)  $  longestSnocSuffix (Snoc(Snoc (Snoc Lin 1)4)12)  (Snoc (Snoc Lin 4)12)
  ]

test_snocZip = TestList [
  tc "snocZip 0" Lin $ snocZip (Lin::SnocList Int) (Snoc (Snoc Lin 2)3),
  tc "snocZip 1" (Snoc Lin (3,"a")) $ snocZip (Snoc (Snoc (Snoc Lin 1)2)3) (Snoc Lin "a"),
  tc "snocZip 2" (Snoc (Snoc (Snoc Lin (1,10)) (2,20)) (3,30))  $ snocZip (Snoc(Snoc(Snoc Lin 1)2)3) (Snoc(Snoc(Snoc Lin 10)20)30)
  ]

test_snocify = TestList [
  tc "snocify 0" (Snoc(Snoc(Snoc(Snoc Lin 50)60)70)80) $ snocify [50,60,70,80],
  tc "snocify 1" Lin $ snocify ([]::[Int]),
  tc "snocify 2" (Snoc Lin 1000) $ snocify [1000]
  ]

test_unSnocify = TestList [
  tc "unSnocify 0" [2,4,6,8] $ unSnocify (Snoc(Snoc(Snoc(Snoc Lin 2)4)6)8),
  tc "unSnocify 1" [] $ unSnocify (Lin :: SnocList Int),
  tc "unSnocify 2" [123] $ unSnocify (Snoc Lin 123)
  ]

test_uniques = TestList [
  tc "uniques 0" (Snoc(Snoc(Snoc Lin 1)2)3) $ uniques (Snoc(Snoc(Snoc(Snoc(Snoc(Snoc Lin 1)1)2)3)2)3),
  -- remember, we keep the last occurrence of any particular value in our answer.
  tc "uniques 1" (Snoc(Snoc(Snoc(Snoc Lin 1)2)3)4) $ uniques  (Snoc(Snoc(Snoc(Snoc(Snoc(Snoc Lin 1)1)2)4)3)4)
  ]

test_snocReverse = TestList [
  tc "snocReverse 0" (Snoc(Snoc(Snoc Lin 9)8)7) $ snocReverse (Snoc(Snoc(Snoc Lin 7)8)9),
  tc "snocReverse 1" (Snoc Lin 7) $ snocReverse (Snoc Lin 7)
  ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_Tree = TestList [
  -- Eq
  tc "Tree 0"  True  $ (==) (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L)))  (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L))) ,
  -- one buried value is different.
  tc "Tree 1"  False $ (==) (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L)))  (Br (V 100) 2 (Br (V 3) 4 (Br L 5 L))) ,
  -- different trees, even though the same inorder walk exists.
  tc "Tree 2"  False $ (==) (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L)))  (Br (V 1) 2 (Br (V 3) 4 (V 5))),
  tc "Tree 3"  True $ and [L==(L::Tree Int), (V 3)==(V 3), (Br L 7 L)==(Br L 7 L)],
  
  --Ord
  tc "Tree 4"  LT $ compare (Br (V 1) 10 (V 100)) (Br (V 5) 10 (V 15)),
  tc "Tree 5"  EQ $ compare (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L))) (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L))),
  tc "Tree 6"  GT $ compare (Br (V 5) 10 (V 15))  (Br (V 1) 10 (V 100)) ,
  -- read our spec carefully for the next two cases' reasoning.
  tc "Tree 7"  LT $ compare (V 5) (Br L 5 L),
  tc "Tree 8"  GT $ compare (Br L 100 L) (V 9001)
  ]

test_insertTree = TestList [
  tc "insertTree 0" (Br (Br L 1 (V 2)) 3 (Br (V 4) 5 L))  $ insertTree 4 (Br (Br L 1 (V 2)) 3 (V 5)),
  tc "insertTree 1" (Br (Br L 1 (V 2)) 3 (Br L 5 (V 6)))  $ insertTree 6 (Br (Br L 1 (V 2)) 3 (V 5)),
  tc "insertTree 2" (Br (V 3) 4 L)  $ insertTree 3 (V 4),
  tc "insertTree 3" (V 5) $ insertTree 5 L
  ]

test_inOrder = TestList [
  tc "inOrder 0" [1,2,3,4,5] $ inOrder  (Br (V 1) 2 (Br (V 3) 4 (Br L 5 L))),
  tc "inOrder 1" [1,2,3]     $ inOrder  (Br (Br L 1 L) 2 (Br L 3 L)),
  tc "inOrder 2" [10] $ inOrder  (V 10)
  
  ]

test_treeSort = TestList [
  tc "treeSort 0" [1,2,3,4,5] $ treeSort  [1,5,3,2,4],
  tc "treeSort 1" [2,4,4,6,6] $ treeSort  [2,6,4,4,6],
  tc "treeSort 2" [] $ ([] :: [Int])
  ]

test_treeMin = TestList [
  tc "treeMin 0" (Just 2) $ treeMin (Br (V 10) 2 (Br (V 3) 4 (V 5))),
  tc "treeMin 1" (Just (-5)) $ treeMin (Br (V 1) 2 (Br (V 3) 4 (Br (V (-5)) 6 (V 7)))),
  tc "treeMin 2" (Nothing::Maybe Int) $ treeMin (L::Tree Int)
  ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_parent = TestList [
  tc "parent 0 - \"Animal\" tree" (Just "Object") $ parent "Animal"  sampletree,
  tc "parent 1 - \"Object\" tree" Nothing         $ parent "Object"  sampletree,
  tc "parent 2 - \"Cat\" tree"    (Just "Cat")    $ parent "Siamese" sampletree,
  tc "parent 3 - \"Dog\" tree"    (Just "Dog")    $ parent "Pug"     sampletree
  ]


test_ancestors = TestList [
  tc "ancestors0" (Just ["Animal", "Object"])      $ ancestors "Dog"            sampletree,
  tc "ancestors1" (Just ["Object"])                $ ancestors "Book"           sampletree,
  tc "ancestors2" Nothing                          $ ancestors "Garbage"        sampletree,
  tc "ancestors3" (Just ["Cat","Animal","Object"]) $ ancestors "Calico"         sampletree,
  tc "ancestors4" Nothing                          $ ancestors "NotEvenPresent" sampletree
  ]


test_headMaybe = TestList [
  tc "headMaybe0" Nothing  $ headMaybe ([]::[Int]),
  tc "headMaybe1" (Just 5) $ headMaybe [5],
  tc "headMaybe2" (Just 5) $ headMaybe [5,10,15]
  ]


test_leastUpperBound = TestList [
  tc "leastUpperBound0" (Just "Animal") $ leastUpperBound "Pug" "Calico" sampletree,
  tc "leastUpperBound1" (Just "Animal") $ leastUpperBound "Pug" "Animal" sampletree,
  tc "leastUpperBound2" (Just "Object") $ leastUpperBound "Pug" "Book" sampletree,
  tc "leastUpperBound3" Nothing         $ leastUpperBound "Pug" "Garbage" sampletree,
  tc "leastUpperBound4" Nothing         $ leastUpperBound "NOTFOUND" "Book" sampletree
  ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_trib = TestList [
  tc "trib 0" 1992089236780786165 $ trib 500,
  tc "trib 1" 9 $ trib 5
  ]


test_simpleBalanced = TestList [
  tc "simpleBalanced 0" True  $ simpleBalanced "(){}[]",
  tc "simpleBalanced 1" True  $ simpleBalanced "(([{}])[]{})",  tc "simpleBalanced3" True  $ simpleBalanced "(others [are] allowed)",
  tc "simpleBalanced 2" False $ simpleBalanced "(()",
  tc "simpleBalanced 3" False $ simpleBalanced "([)]",
  tc "simpleBalanced 4" True  $ simpleBalanced ""
  ]


test_balanced = TestList [

  tc "balancedM 0" True  $ fst $ runState (balancedM "(){}[]") [],
  tc "balancedM 1" True  $ fst $ runState (balancedM "(([{}])[]{})") [],
  tc "balancedM 2" True  $ fst $ runState (balancedM "(others [are] allowed)") [],
  tc "balancedM 3" False $ fst $ runState (balancedM "(()") [],
  tc "balancedM 4" False $ fst $ runState (balancedM "([)]") [],
  tc "balancedM 5" True  $ fst $ runState (balancedM "") [],
  
  tc "balanced 0" True  $ balanced "(([{}])[]{})",
  tc "balanced 1" True  $ balanced "(others [are] allowed)",
  tc "balanced 2" False $ balanced "(()",
  tc "balanced 3" False $ balanced "([)]"
  ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_divisors = TestList [
  tc "divisors #0:   12" [1,2,3,4,6,12] $ divisors 12,
  tc "divisors #1:    1" [1] $ divisors 1,
  tc "divisors #2:  100" [1,2,4,5,10,20,25,50,100] $ divisors 100,
  tc "divisors #3: 1117" [1,1117] $ divisors 1117
  ]

test_geometric = TestList [
  tc "geometric #0: 1 5" [1,5,25,125,625,3125]   $ take 6 $ geometric 1 5,
  tc "geometric #1: 2 5" [2,10,50,250,1250,6250] $ take 6 $ geometric 2 5,
  tc "geometric #2: 5 1" [5,5,5,5,5,5]           $ take 6 $ geometric 5 1,
  tc "geometric #3: 4 (-1)" [4,-4,4,-4,4,-4]     $ take 6 $ geometric 4 (-1)
  ]

-- note: four copies of the same test to give it more weight.
test_mersennes = TestList [
  tc "mersennes 0" [1,3,7,15,31,63,127,255,511,1023] $ take 10 $ mersennes ,
  tc "mersennes 1" [1,3,7,15,31,63,127,255,511,1023] $ take 10 $ mersennes ,
  tc "mersennes 3" [1,3,7,15,31,63,127,255,511,1023] $ take 10 $ mersennes ,
  tc "mersennes 4" [1,3,7,15,31,63,127,255,511,1023] $ take 10 $ mersennes
  ]

test_unitTriangles = TestList [
  tc "unitTriangles 0" []
                     $ sort (unitTriangles 0)
  ,
  tc "unitTriangles 1" [(1,1,1)]
                     $ sort (unitTriangles 1)
  ,
  tc "unitTriangles 2" [(1,1,1),(1,2,2),(2,2,2)]
                     $ sort (unitTriangles 2)
  ,
  tc "unitTriangles 3" [(1,1,1),(1,2,2),(1,3,3),(2,2,2),(2,2,3),(2,3,3),(3,3,3)]
                     $ sort (unitTriangles 3)
  ,
  tc "unitTriangles 4" [(1,1,1),(1,2,2),(1,3,3),(1,4,4),(2,2,2),(2,2,3),(2,3,3),
                        (2,3,4),(2,4,4),(3,3,3),(3,3,4),(3,4,4),(4,4,4)]
                     $ sort (unitTriangles 4)
  
  ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

  -- from http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
