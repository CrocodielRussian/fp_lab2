import Test.HUnit
import Structure
import Data.List (sort)

tests :: Test
tests = TestList
    [ TestLabel "OASet basic operations" basicOps
    , TestLabel "tombstone handling and rehash" tombstoneAndRehash
    , TestLabel "stress insert/delete" stress
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
        _  <- insert s "a"
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

main :: IO Counts
main = runTestTT tests
