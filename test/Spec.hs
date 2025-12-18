import Test.HUnit
import Structure
import Data.List (sort)

tests :: Test
tests = TestList
    [ TestLabel "OASet basic operations" basicOps
    , TestLabel "tombstone handling and rehash" tombstoneAndRehash
    , TestLabel "stress insert/delete" stress
    , TestLabel "Higher-order functions" higherOrderOps
    ]

basicOps :: Test
basicOps =
  TestList
    [ TestLabel "insert and member" $ TestCase $ do
        s <- newOASet 4
        b1 <- insert s "hello"
        assertEqual "insert returns True" True b1
        mem <- member s "hello"
        assertEqual "member returns True" True mem

    , TestLabel "insert duplicate returns False and does not increase size" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "x"
        r <- insert s "x"
        assertEqual "duplicate insert returns False" False r
        sz <- size s
        assertEqual "size stays 1" 1 sz

    , TestLabel "delete existing and non-existing" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "a"
        ok1 <- delete s "a"
        assertEqual "delete existing returns True" True ok1
        ok2 <- delete s "a"
        assertEqual "delete non-existing returns False" False ok2
        memAfter <- member s "a"
        assertEqual "member after delete is False" False memAfter

    , TestLabel "toList returns all elements (unordered)" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "one"
        _ <- insert s "two"
        lst <- toList s
        assertEqual "toList contains all elements" (sort ["one", "two"]) (sort lst)
    ]

tombstoneAndRehash :: Test
tombstoneAndRehash =
  TestList
    [ TestLabel "inserting after delete keeps set consistent" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "a"
        _ <- insert s "b"
        _ <- delete s "a"

        sz1 <- size s
        assertEqual "size after delete" 1 sz1

        _ <- insert s "c"
        sz2 <- size s
        assertEqual "size after insert" 2 sz2

        mb <- member s "b"
        mc <- member s "c"
        assertEqual "b present" True mb
        assertEqual "c present" True mc

    , TestLabel "rehash preserves all elements when load factor exceeded" $ TestCase $ do
        s <- newOASet 2
        _ <- insert s "k1"
        _ <- insert s "k2"
        _ <- insert s "k3"

        allPresent <- mapM (member s) ["k1", "k2", "k3"]
        assertEqual "all keys present" [True, True, True] allPresent

        sz <- size s
        assertEqual "size is 3" 3 sz
    ]

stress :: Test
stress =
  TestLabel "many inserts and deletes keep invariants" $ TestCase $ do
    s <- newOASet 8
    let keys = map show ([1 .. 100] :: [Int])

    mapM_ (insert s) keys
    sz1 <- size s
    assertEqual "size after inserts" (length keys) sz1

    mapM_ (delete s . show) ([1 .. 50] :: [Int])
    sz2 <- size s
    assertEqual "size after deletes" 50 sz2

    present <- mapM (member s . show) ([51 .. 100] :: [Int])
    assertEqual "remaining keys present" (replicate 50 True) present

higherOrderOps :: Test
higherOrderOps =
  TestList
    [ TestLabel "map transforms elements" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "1"
        _ <- insert s "2"
        _ <- insert s "3"
        s' <- mapOA (\x -> read x + 1 :: Int) s
        lst <- toList s'
        assertEqual "mapped elements are correct" (sort [2, 3, 4]) (sort lst)
        sz <- size s'
        assertEqual "size remains same" 3 sz

    -- , TestLabel "map handles duplicates after mapping" $ TestCase $ do
    --     s <- newOASet 4
    --     _ <- insert s "a"
    --     _ <- insert s "b"
    --     s' <- mapOA (\_ -> "x") s
    --     lst <- toList s'
    --     assertEqual "mapped elements collapse to unique" ["x"] lst
    --     sz <- size s'
    --     assertEqual "size is 1 due to collapse" 1 sz

    , TestLabel "filter keeps matching elements" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "apple"
        _ <- insert s "banana"
        _ <- insert s "apricot"
        s' <- filterOA (\x -> head x == 'a') s
        lst <- toList s'
        assertEqual "filtered elements are correct" (sort ["apple", "apricot"]) (sort lst)
        sz <- size s'
        assertEqual "size is 2" 2 sz

    , TestLabel "filter removes all elements" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "apple"
        _ <- insert s "banana"
        s' <- filterOA (\x -> head x == 'c') s
        lst <- toList s'
        assertEqual "all elements removed" [] lst
        sz <- size s'
        assertEqual "size is 0" 0 sz

    , TestLabel "foldl sums elements" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s (1 :: Int)
        _ <- insert s 2
        _ <- insert s 3
        sumVal <- foldlOA (+) 0 s
        assertEqual "foldl sums correctly" 6 sumVal

    , TestLabel "foldr concatenates strings" $ TestCase $ do
        s <- newOASet 4
        _ <- insert s "a"
        _ <- insert s "b"
        _ <- insert s "c"

        concatStr <- foldrOA (++) "" s
        assertBool "foldr concatenates correctly" (all (`elem` concatStr) ['a', 'b', 'c'] && length concatStr == 3)

    , TestLabel "foldl on empty set" $ TestCase $ do
        s <- newOASet 4
        sumVal <- foldlOA (+) 0 s
        assertEqual "foldl on empty set is initial value" 0 sumVal

    , TestLabel "map on empty set" $ TestCase $ do
        s <- newOASet 4
        s' <- mapOA (\x -> read x + 1 :: Int) s
        lst <- toList s'
        assertEqual "map on empty set is empty" [] lst
        sz <- size s'
        assertEqual "size is 0" 0 sz
    ]

main :: IO Counts
main = runTestTT tests
