module Main (main) where

import Test.HUnit hiding (Testable)
import Test.QuickCheck
import qualified Data.List as L
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.List ()
import Data.Hashable ()
import Prelude

import Structure
  ( Slots
  , initSlots
  , fromList
  , size
  , member
  , insert
  , delete
  , toList
  , mapOA
  , foldlOA
  , foldrOA
  , filterOA
  )


qcArgs :: Args
qcArgs = stdArgs { maxSuccess = 200 }

check :: Testable prop => String -> prop -> IO Bool
check name prop = do
  r <- quickCheckWithResult qcArgs (counterexample name prop)
  pure (isSuccess r)

main :: IO ()
main = do
  c <- runTestTT tests

  ok <- if errors c + failures c == 0
          then and <$> sequence
                 [ check "Monoid: left identity"  prop_monoid_leftId
                 , check "Monoid: right identity" prop_monoid_rightId
                 , check "Eq: set equivalence"    prop_eq_set_equiv
                 , check "Semigroup: associativity" prop_semigroup_assoc
                 , check "Foldable: toList -> set equivalence" prop_foldable_toSet
                 ]
          else pure False

  if ok then exitSuccess else exitFailure


tests :: Test
tests = TestList
  [ TestLabel "initSlots empty" test_initSlots
  , TestLabel "insert/member basic" test_insertMember
  , TestLabel "insert duplicate" test_insertDuplicate
  , TestLabel "delete present" test_deletePresent
  , TestLabel "delete missing" test_deleteMissing
  , TestLabel "toList/fromList set semantics" test_toListFromList
  , TestLabel "stress insert many (resize/rehash)" test_stress
  , TestLabel "map transforms elements" test_mapTransforms
  , TestLabel "filter keeps matching elements" test_filterKeepsMatching
  , TestLabel "filter removes all elements" test_filterRemovesAll
  , TestLabel "foldl sums elements" test_foldlSums
  , TestLabel "foldr sums elements" test_foldrSums
  , TestLabel "map on empty set" test_mapEmpty
  ]

prop_eq_set_equiv :: [Int] -> [Int] -> Bool
prop_eq_set_equiv xs ys =
  let a = (fromList xs :: Slots Int)
      b = fromList ys
  in (a == b) == (S.fromList xs == S.fromList ys)

prop_semigroup_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_semigroup_assoc xs ys zs =
  let a = fromList xs :: Slots Int
      b = fromList ys
      c = fromList zs
  in ((a <> b) <> c) == (a <> (b <> c))
prop_monoid_leftId :: [Int] -> Bool
prop_monoid_leftId xs =
  let a = fromList xs :: Slots Int
  in (mempty <> a) == a

prop_monoid_rightId :: [Int] -> Bool
prop_monoid_rightId xs =
  let a = fromList xs :: Slots Int
  in (a <> mempty) == a

prop_foldable_toSet :: [Int] -> Bool
prop_foldable_toSet xs =
  let a = fromList xs :: Slots Int
  in S.fromList (F.toList a) == S.fromList xs

-- qcAsHUnit :: Testable prop => String -> prop -> Test
-- qcAsHUnit name prop = TestLabel name $ TestCase $ do
--   result <- quickCheckWithResult qcArgs prop
--   assertBool (name ++ " failed: " ++ show result) (isSuccess result)


test_initSlots :: Test
test_initSlots = TestCase $ do
  let s = initSlots 8 :: Slots Int
  size s @?= 0
  member s 123 @?= False

test_insertMember :: Test
test_insertMember = TestCase $ do
  let s0 = initSlots 8 :: Slots Int
  let (s1, ok) = insert s0 42
  ok @?= True
  size s1 @?= 1
  member s1 42 @?= True

test_insertDuplicate :: Test
test_insertDuplicate = TestCase $ do
  let s0 = initSlots 8 :: Slots Int
  let (s1, ok1) = insert s0 7
  let (s2, ok2) = insert s1 7
  ok1 @?= True
  ok2 @?= False
  size s2 @?= 1

test_deletePresent :: Test
test_deletePresent = TestCase $ do
  let s0 = fromList [1,2,3,4,5] :: Slots Int
  let (s1, ok) = delete s0 3
  ok @?= True
  member s1 3 @?= False
  size s1 @?= 4

test_deleteMissing :: Test
test_deleteMissing = TestCase $ do
  let s0 = fromList [1,2,3] :: Slots Int
  let (s1, ok) = delete s0 999
  ok @?= False
  size s1 @?= 3

test_toListFromList :: Test
test_toListFromList = TestCase $ do
  let xs  = [10,10,20,30,20,40]
  let s   = fromList xs :: Slots Int
  let got = L.sort (toList s)
  let exp = L.sort (L.nub xs)
  got @?= exp
  size s @?= length exp

test_stress :: Test
test_stress = TestCase $ do
  let xs = [1..3000] <> [1..1500]
  let s  = fromList xs :: Slots Int
  size s @?= 3000
  mapM_ (\k -> assertBool ("expected member " <> show k) (member s k))
        [1,2,3,10,999,1500,2999,3000]
  mapM_ (\k -> assertBool ("expected NOT member " <> show k) (not (member s k)))
        [0,3001,4000]

test_mapTransforms :: Test
test_mapTransforms = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "3"
  let (s2, _) = insert s1 "1"
  let (s3, _) = insert s2 "2"

  let s'  = mapOA (\x -> read x + 1 :: Int) s3
  let lst = toList s'
  assertEqual "mapped elements are correct" [4, 2, 3]  lst

  let sz = size s'
  assertEqual "size remains same" 3 sz

test_filterKeepsMatching :: Test
test_filterKeepsMatching = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "apple"
  let (s2, _) = insert s1 "banana"
  let (s3, _) = insert s2 "apricot"

  let s'  = filterOA (\x -> head x == 'a') s3
  let lst = toList s'
  assertEqual "filtered elements are correct"  ["apple", "apricot"] lst

  let sz = size s'
  assertEqual "size is 2" 2 sz

test_filterRemovesAll :: Test
test_filterRemovesAll = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "apple"
  let (s2, _) = insert s1 "banana"

  let s'  = filterOA (\x -> head x == 'c') s2
  let lst = toList s'
  assertEqual "all elements removed" [] lst

  let sz = size s'
  assertEqual "size is 0" 0 sz

test_foldlSums :: Test
test_foldlSums = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 (1 :: Int)
  let (s2, _) = insert s1 2
  let (s3, _) = insert s2 3

  let sumVal = foldlOA (+) 0 s3
  assertEqual "foldl sums correctly" 6 sumVal

test_foldrSums :: Test
test_foldrSums = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 (1 :: Int)
  let (s2, _) = insert s1 2
  let (s3, _) = insert s2 3

  let sumVal = foldrOA (+) 0 s3
  assertEqual "foldr sums correctly" 6 sumVal

test_mapEmpty :: Test
test_mapEmpty = TestCase $ do
  let s  = initSlots 4
  let s' = mapOA (\x -> read x + 1 :: Int) s
  let lst = toList s'
  assertEqual "map on empty set is empty" [] lst

  let sz = size s'
  assertEqual "size is 0" 0 sz
