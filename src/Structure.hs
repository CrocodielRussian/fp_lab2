module Structure
  ( Slots
  , initSlots
  , fromList
  , getSize
  , member
  , insert
  , delete
  , toList
  , powerTwo
  , mapOA
  , filterOA
  , foldlOA
  , foldrOA
  ) where

import Data.Bits
import Data.Hashable
import qualified Data.Vector as V
import Data.Vector (Vector)


data State = Empty | Deleted | Occupied
  deriving (Eq, Show)

data Slot k = Slot
  { slotState :: State
  , slotKey   :: k
  } deriving (Show)


data Slots k = Slots
  { capacity   :: Int
  , size       :: Int
  , tombstones :: Int
  , slots      :: Vector (Slot k)
  } deriving (Show)

startIndex :: Hashable k => Slots k -> k -> Int
startIndex s k =
  hash k .&. (capacity s - 1) 

loadFactorThreshold :: Double
loadFactorThreshold = 0.7


resize :: (Eq k, Hashable k) => Slots k -> Int -> Slots k
resize st newCap =
  V.foldl' reinsert empty (slots st)
  where
    empty =
      Slots newCap 0 0 (V.replicate newCap emptySlot)

    reinsert acc (Slot Occupied k) =
      fst (oaInsertRaw acc k)
    reinsert acc _ =
      acc

oaBalance :: (Eq k, Hashable k) => Slots k -> Slots k
oaBalance s
  | load > loadFactorThreshold =
      resize s (capacity s * 2)

  | tombstones s > size s =
      rehash s

  | otherwise =
      s
  where
    load =
      fromIntegral (size s + tombstones s)
        / fromIntegral (capacity s)


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

emptySlot :: Slot k
emptySlot = Slot Empty undefined

initSlots :: Int -> Slots k
initSlots n = do
  Slots
    { capacity   = n
    , size       = 0
    , tombstones = 0
    , slots      = V.replicate n emptySlot
    }

getSize :: Slots k -> Int
getSize s = size s

rehash :: (Eq k, Hashable k) => Slots k -> Slots k
rehash s = do
  V.foldl' reinsert empty (slots s)
  where
    cap = capacity s

    empty =
      Slots cap 0 0 (V.replicate cap emptySlot)

    reinsert acc (Slot Occupied k) =
      fst (oaInsertRaw acc k)
    reinsert acc _ =
      acc

member :: (Eq k, Hashable k) => Slots k -> k -> Bool
member st key = go (startIndex st key) 0
  where
    cap = capacity st

    go i probe
      | probe >= cap = False
      | otherwise =
          case slots st V.! i of
            Slot Empty _ -> False
            Slot Occupied k
              | k == key -> True
              | otherwise -> next
            _ -> next
      where
        next = go ((i + 1) .&. (cap - 1)) (probe + 1)



oaInsertRaw :: Hashable k => Slots k -> k -> (Slots k, Bool)
oaInsertRaw st key = go (startIndex st key) 0
  where
    cap = capacity st

    go i probe
      | probe >= cap = (st, False)
      | otherwise =
          case slots st V.! i of
            Slot Occupied _ ->
              go ((i + 1) .&. (cap - 1)) (probe + 1)

            _ ->
              ( st { size  = size st + 1
                  , slots = slots st V.// [(i, Slot Occupied key)]
                  }
              , True
              )

insert :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insert st key = insertLoop (oaBalance st) key

insertLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insertLoop s key = go (startIndex s key) 0 Nothing
  where
    cap = capacity s

    go i probe firstDel
      | probe >= cap = (s, False)
      | otherwise =
          case slots s V.! i of
            Slot Empty _ ->
              let pos = maybe i id firstDel
               in ( s { size       = size s + 1
                      , tombstones =
                          if firstDel == Nothing
                          then tombstones s
                          else tombstones s - 1
                      , slots =
                          slots s V.// [(pos, Slot Occupied key)]
                      }
                  , True
                  )

            Slot Deleted _ ->
              go ((i + 1) .&. (cap - 1))
                 (probe + 1)
                 (case firstDel of
                    Nothing -> Just i
                    _       -> firstDel)

            Slot Occupied k
              | k == key -> (s, False)
              | otherwise ->
                  go ((i + 1) .&. (cap - 1))
                     (probe + 1)
                     firstDel


delete :: (Eq k, Hashable k) =>  Slots k -> k -> (Slots k, Bool)
delete st key = do
    case deleteLoop st key of
      (s', True)  -> (oaBalance s', True)
      result     -> result

deleteLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
deleteLoop s key = go (startIndex s key) 0
  where
    cap = capacity s

    go i probe
      | probe >= cap = (s, False)
      | otherwise =
          case slots s V.! i of
            Slot Empty _ ->
              (s, False)

            Slot Occupied k
              | k == key ->
                  ( s { size       = size s - 1
                      , tombstones = tombstones s + 1
                      , slots =
                          slots s V.// [(i, Slot Deleted undefined)]
                      }
                  , True
                  )

            _ ->
              go ((i + 1) .&. (cap - 1)) (probe + 1)

toList :: Slots k -> [k]
toList s =
  foldrOA (:) [] s

fromList :: (Eq k, Hashable k) => Int -> [k] -> Slots k
fromList cap =
  foldl (\acc k -> fst (insert acc k)) (initSlots cap)

foldlOA :: (a -> k -> a) -> a -> Slots k -> a
foldlOA f z s = 
    V.foldl' step z (slots s)
  where
    step acc (Slot Occupied k) = f acc k
    step acc _                = acc

foldrOA :: (k -> a -> a) -> a -> Slots k -> a
foldrOA f z s = 
  V.foldr step z (slots s)
  where
    step (Slot Occupied k) acc = f k acc
    step _ acc                = acc


filterOA :: (Eq k, Hashable k) => (k -> Bool) -> Slots k -> Slots k
filterOA p s =
  rehash $
    foldlOA step (initSlots (capacity s)) s
  where
    step acc k
      | p k       = fst (oaInsertRaw acc k)
      | otherwise = acc

mapOA :: (Eq k2, Hashable k2)
      => (k1 -> k2)
      -> Slots k1
      -> Slots k2
mapOA f s =
  rehash $
    foldlOA step (initSlots (capacity s)) s
  where
    step acc k =
      fst (oaInsertRaw acc (f k))

