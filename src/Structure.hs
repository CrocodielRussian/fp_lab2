module Structure
  ( OASet
  , newOASet
  , fromList
  , size
  , member
  , insert
  , delete
  , toList
  , powerTwo
  ) where

import Control.Monad (forM_, forM, when)
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Vector.Mutable as MV
import Data.Vector.Mutable (IOVector)
import Prelude
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.Bits ((.&.))


data Slot k = Empty | Deleted | Occupied k
  deriving (Show, Eq)


data OASet k = OASet
  { vecRef    :: IORef (IOVector (Slot k)) 
  , sizeRef   :: IORef Int                 
  , tombRef   :: IORef Int                 
  }

loadFactorThreshold :: Double
loadFactorThreshold = 0.7

logN :: Int -> Int
logN n = ceiling (logBase 2 (fromIntegral n :: Double)) + 1

powerTwo :: Int -> Int
powerTwo n
  | n <= 1    = 1
  | otherwise = 2 ^ search 0 (logN n)
  where
    search l r
      | l == r    = l
      | otherwise =
          let mid = (l + r) `div` 2
          in if 2 ^ mid < n
               then search (mid + 1) r
               else search l mid


newOASet :: Int -> IO (OASet k)
newOASet n = do
  let cap = max 1 (powerTwo n)
  v <- MV.replicate cap Empty
  vr <- newIORef v
  sr <- newIORef 0
  tr <- newIORef 0
  return $ OASet vr sr tr

fromList :: (Eq k, Hashable k) => [k] -> IO (OASet k)
fromList xs = do
  s <- newOASet (max 16 (length xs * 2))
  forM_ xs (insert s)
  return s

size :: OASet k -> IO Int
size st = readIORef (sizeRef st)

rehash :: (Eq k, Hashable k) => OASet k -> Int -> IO ()
rehash st newCap = do
  oldV <- readIORef (vecRef st)
  let oldCap = MV.length oldV
  keys <- fmap concat $ forM [0 .. oldCap - 1] $ \i -> do
    s <- MV.read oldV i
    case s of
      Occupied k -> return [k]
      _ -> return []
  newV <- MV.replicate newCap Empty
  writeIORef (vecRef st) newV
  writeIORef (sizeRef st) 0
  writeIORef (tombRef st) 0
  forM_ keys $ \k -> do
    _ <- insert st k
    return ()

member :: (Eq k, Hashable k) => OASet k -> k -> IO Bool
member st key = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
      start = hash key .&. (cap - 1)
  probe v cap start 0
  where
    probe v cap i cnt
      | cnt >= cap = return False
      | otherwise = do
          s <- MV.read v i
          case s of
            Empty -> return False
            Occupied k' | k' == key -> return True
            _ -> probe v cap ((i + 1) .&. (cap - 1)) (cnt + 1)


probeInsert :: (Eq k, Hashable k) => IOVector (Slot k) -> Int -> Int -> Int -> Maybe Int -> k -> IO (Either Bool Int)
probeInsert v cap i cnt firstDel key
  | cnt >= cap = case firstDel of
      Just d  -> return (Right d)
      Nothing -> return (Left False)
  | otherwise = do
      s <- MV.read v i
      case s of
        Empty -> return $ Right (fromMaybe i firstDel)
        Occupied k'
          | k' == key -> return (Left True)
          | otherwise  -> probeInsert v cap ((i + 1) .&. (cap - 1)) (cnt + 1) firstDel key
        Deleted ->
          probeInsert v cap ((i + 1) .&. (cap - 1)) (cnt + 1) (firstDel <|> Just i) key

insert :: (Eq k, Hashable k) => OASet k -> k -> IO Bool
insert st key = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
      start = hash key .&. (cap - 1)
  res <- probeInsert v cap start 0 Nothing key
  case res of
    Left True -> return False
    Left False -> do
      rehash st (cap * 2)
      insert st key
    Right pos -> do
      prev <- MV.read v pos
      when (prev == Deleted) $ modifyIORef' (tombRef st) (subtract 1)
      MV.write v pos (Occupied key)
      modifyIORef' (sizeRef st) (+1)

      sz <- readIORef (sizeRef st)
      tb <- readIORef (tombRef st)
      let capD = MV.length v
          load = fromIntegral (sz + tb) / fromIntegral capD
      when (load > loadFactorThreshold) $
        rehash st (capD * 2)
      return True


delete :: (Eq k, Hashable k) => OASet k -> k -> IO Bool
delete st key = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
      start = hash key .&. (cap - 1)
  probe v cap start 0
  where
    probe v cap i cnt
      | cnt >= cap = return False
      | otherwise = do
          s <- MV.read v i
          case s of
            Empty -> return False
            (Occupied k') | k' == key -> do
              MV.write v i Deleted
              modifyIORef' (sizeRef st) (subtract 1)
              modifyIORef' (tombRef st) (+1)
              return True
            _ -> probe v cap ((i + 1) .&. (cap - 1)) (cnt + 1)


toList :: OASet k -> IO [k]
toList st = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
  fmap concat $ forM [0 .. cap - 1] $ \i -> do
    s <- MV.read v i
    case s of
      Occupied k -> return [k]
      _ -> return []