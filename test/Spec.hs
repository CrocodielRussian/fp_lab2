module Main (main) where

import Test.HUnit
import qualified Data.List as L
import System.Exit (exitFailure, exitSuccess)

import Structure
  ( Slots
  , initSlots
  , fromList
  , getSize
  , member
  , insert
  , delete
  , toList
  , mapOA
  , foldlOA
  , foldrOA
  , filterOA
  , powerTwo
  )

main :: IO ()
main = do
  c <- runTestTT tests
  if errors c + failures c == 0 then exitSuccess else exitFailure

tests :: Test
tests = TestList
  [ TestLabel "powerTwo edge cases" test_powerTwo
  , TestLabel "initSlots empty" test_initSlots
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
    , TestLabel "foldr concatenates strings" test_foldrConcats
    , TestLabel "foldl on empty set" test_foldlEmpty
    , TestLabel "map on empty set" test_mapEmpty
  ]

test_powerTwo :: Test
test_powerTwo = TestCase $ do
  powerTwo 0 @?= 1
  powerTwo 1 @?= 1
  powerTwo 2 @?= 2
  powerTwo 3 @?= 4
  powerTwo 4 @?= 4
  powerTwo 5 @?= 8
  powerTwo 9 @?= 16

test_initSlots :: Test
test_initSlots = TestCase $ do
  let s = initSlots 8 :: Slots Int
  getSize s @?= 0
  member s 123 @?= False

test_insertMember :: Test
test_insertMember = TestCase $ do
  let s0 = initSlots 8 :: Slots Int
  let (s1, ok) = insert s0 42
  ok @?= True
  getSize s1 @?= 1
  member s1 42 @?= True

test_insertDuplicate :: Test
test_insertDuplicate = TestCase $ do
  let s0 = initSlots 8 :: Slots Int
  let (s1, ok1) = insert s0 7
  let (s2, ok2) = insert s1 7
  ok1 @?= True
  ok2 @?= False
  getSize s2 @?= 1

test_deletePresent :: Test
test_deletePresent = TestCase $ do
  let s0 = fromList 8 [1,2,3,4,5] :: Slots Int
  let (s1, ok) = delete s0 3
  ok @?= True
  member s1 3 @?= False
  getSize s1 @?= 4

test_deleteMissing :: Test
test_deleteMissing = TestCase $ do
  let s0 = fromList 8 [1,2,3] :: Slots Int
  let (s1, ok) = delete s0 999
  ok @?= False
  getSize s1 @?= 3

test_toListFromList :: Test
test_toListFromList = TestCase $ do
  let xs  = [10,10,20,30,20,40]
  let s   = fromList 8 xs :: Slots Int
  let got = L.sort (toList s)
  let exp = L.sort (L.nub xs)
  got @?= exp
  getSize s @?= length exp

test_stress :: Test
test_stress = TestCase $ do
  let xs = [1..3000] <> [1..1500]
  let s  = fromList 2 xs :: Slots Int
  getSize s @?= 3000
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

  let sz = getSize s'
  assertEqual "getSize remains same" 3 sz

test_filterKeepsMatching :: Test
test_filterKeepsMatching = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "apple"
  let (s2, _) = insert s1 "banana"
  let (s3, _) = insert s2 "apricot"

  let s'  = filterOA (\x -> head x == 'a') s3
  let lst = toList s'
  assertEqual "filtered elements are correct"  ["apple", "apricot"] lst

  let sz = getSize s'
  assertEqual "getSize is 2" 2 sz

test_filterRemovesAll :: Test
test_filterRemovesAll = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "apple"
  let (s2, _) = insert s1 "banana"

  let s'  = filterOA (\x -> head x == 'c') s2
  let lst = toList s'
  assertEqual "all elements removed" [] lst

  let sz = getSize s'
  assertEqual "getSize is 0" 0 sz

test_foldlSums :: Test
test_foldlSums = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 (1 :: Int)
  let (s2, _) = insert s1 2
  let (s3, _) = insert s2 3

  let sumVal = foldlOA (+) 0 s3
  assertEqual "foldl sums correctly" 6 sumVal

test_foldrConcats :: Test
test_foldrConcats = TestCase $ do
  let s0 = initSlots 4
  let (s1, _) = insert s0 "a"
  let (s2, _) = insert s1 "b"
  let (s3, _) = insert s2 "c"

  let concatStr = foldrOA (++) "" s3
  assertBool
    "foldr concatenates correctly"
    (all (`elem` concatStr) "abc" && length concatStr == 3)

test_foldlEmpty :: Test
test_foldlEmpty = TestCase $ do
  let s = initSlots 4
  let sumVal = foldlOA (+) 0 s
  assertEqual "foldl on empty set is initial value" 0 sumVal

test_mapEmpty :: Test
test_mapEmpty = TestCase $ do
  let s  = initSlots 4
  let s' = mapOA (\x -> read x + 1 :: Int) s
  let lst = toList s'
  assertEqual "map on empty set is empty" [] lst

  let sz = getSize s'
  assertEqual "getSize is 0" 0 sz
