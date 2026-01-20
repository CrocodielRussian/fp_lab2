module Structure.Internal where

import Data.Bits
import Data.Hashable
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Set as S
import Data.List (nub, foldl')
import Prelude hiding (lookup)
import Test.QuickCheck.Arbitrary

data State = Empty | Deleted | Occupied
  deriving (Eq, Show)

data Slot k = Slot
  { slotState :: State
  , slotKey   :: k
  } deriving (Show)

data Slots k = Slots
  { capacity   :: Int
  , slotsSize  :: Int
  , tombstones :: Int
  , slots      :: [Slot k]
  } deriving (Show)

index :: [a] -> Int -> a
index (x:_)  0 = x
index (_:xs) n = index xs (n - 1)
index [] _     = error "index out of bounds"

updateAt :: Int -> a -> [a] -> [a]
updateAt 0 v (_:xs) = v : xs
updateAt n v (x:xs) = x : updateAt (n - 1) v xs
updateAt _ _ []     = []


emptySlot :: Slot k
emptySlot = Slot Empty undefined

initSlots :: Int -> Slots k
initSlots n =
  Slots n 0 0 (replicate n emptySlot)

size :: Slots k -> Int
size = slotsSize

startIndex :: Hashable k => Slots k -> k -> Int
startIndex s k =
  hash k .&. (capacity s - 1)

loadFactorThreshold :: Double
loadFactorThreshold = 0.7

resize :: (Eq k, Hashable k) => Slots k -> Int -> Slots k
resize st newCap =
  foldl' reinsert empty (slots st)
  where
    empty =
      Slots newCap 0 0 (replicate newCap emptySlot)

    reinsert acc (Slot Occupied k) =
      fst (oaInsertRaw acc k)
    reinsert acc _ =
      acc

rehash :: (Eq k, Hashable k) => Slots k -> Slots k
rehash s =
  foldl' reinsert empty (slots s)
  where
    cap = capacity s

    empty =
      Slots cap 0 0 (replicate cap emptySlot)

    reinsert acc (Slot Occupied k) =
      fst (oaInsertRaw acc k)
    reinsert acc _ =
      acc

oaBalance :: (Eq k, Hashable k) => Slots k -> Slots k
oaBalance s
  | load > loadFactorThreshold =
      resize s (capacity s * 2)
  | tombstones s > slotsSize s =
      rehash s
  | otherwise =
      s
  where
    load =
      fromIntegral (slotsSize s + tombstones s)
        / fromIntegral (capacity s)

member :: (Eq k, Hashable k) => Slots k -> k -> Bool
member st key = go (startIndex st key) 0
  where
    cap = capacity st

    go i probe
      | probe >= cap = False
      | otherwise =
          case slots st `index` i of
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
          case slots st `index` i of
            Slot Occupied _ ->
              go ((i + 1) .&. (cap - 1)) (probe + 1)

            _ ->
              ( st { slotsSize = slotsSize st + 1
                   , slots = updateAt i (Slot Occupied key) (slots st)
                   }
              , True
              )

insert :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insert st = insertLoop (oaBalance st)

insertLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insertLoop s key = go (startIndex s key) 0 Nothing
  where
    cap = capacity s

    go i probe firstDel
      | probe >= cap = (s, False)
      | otherwise =
          case slots s `index` i of
            Slot Empty _ ->
              let pos = fromMaybe i firstDel
               in ( s { slotsSize = slotsSize s + 1
                      , tombstones =
                          if isNothing firstDel
                          then tombstones s
                          else tombstones s - 1
                      , slots =
                          updateAt pos (Slot Occupied key) (slots s)
                      }
                  , True
                  )

            Slot Deleted _ ->
              go ((i + 1) .&. (cap - 1))
                 (probe + 1)
                 (firstDel <|> Just i)

            Slot Occupied k
              | k == key -> (s, False)
              | otherwise ->
                  go ((i + 1) .&. (cap - 1))
                     (probe + 1)
                     firstDel

delete :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
delete st key =
  case deleteLoop st key of
    (s', True) -> (oaBalance s', True)
    result    -> result

deleteLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
deleteLoop s key = go (startIndex s key) 0
  where
    cap = capacity s

    go i probe
      | probe >= cap = (s, False)
      | otherwise =
          case slots s `index` i of
            Slot Empty _ ->
              (s, False)

            Slot Occupied k
              | k == key ->
                  ( s { slotsSize = slotsSize s - 1
                      , tombstones = tombstones s + 1
                      , slots =
                          updateAt i (Slot Deleted undefined) (slots s)
                      }
                  , True
                  )

            _ ->
              go ((i + 1) .&. (cap - 1)) (probe + 1)


instance Foldable Slots where
  foldr f z s =
    foldr step z (slots s)
    where
      step (Slot Occupied k) acc = f k acc
      step _                 acc = acc

mergeOA :: (Eq a, Hashable a) => Slots a -> Slots a -> Slots a
mergeOA lhs rhs =
  let a' = oaBalance lhs
      b' = oaBalance rhs
      total = slotsSize a' + slotsSize b'
      cap = powerTwo (max 1 (total * 2))
      base = initSlots cap
      elems = F.toList a' ++ F.toList b'
      merged = foldl' (\acc k -> fst (oaInsertRaw acc k)) base elems
  in oaBalance merged

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

fromList :: (Eq k, Hashable k) => [k] -> Slots k
fromList xs =
  let uniq = nub xs
      cap = powerTwo (max 1 (length uniq * 2))
  in foldl' (\acc k -> fst (oaInsertRaw acc k)) (initSlots cap) uniq

mapOA :: (Eq k2, Hashable k2) => (k1 -> k2) -> Slots k1 -> Slots k2
mapOA f s =
  rehash $
    foldl step (initSlots (capacity s)) s
  where
    step acc k =
      fst (oaInsertRaw acc (f k))

instance (Eq a, Hashable a) => Semigroup (Slots a) where
  (<>) = mergeOA

instance (Eq a, Hashable a) => Monoid (Slots a) where
  mempty = initSlots 10

instance (Arbitrary a, Hashable a) => Arbitrary (Slots a) where
  arbitrary = fromList . nub <$> arbitrary

instance (Eq a, Hashable a, Ord a) => Eq (Slots a) where
  s1 == s2 = S.fromList (F.toList s1) == S.fromList (F.toList s2)
