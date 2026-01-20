module Structure
  ( Slots
  , initSlots
  , fromList
  , size
  , member
  , insert
  , delete
  , toList
  , mapOA
  , filterOA
  , foldlOA
  , foldrOA
  ) where

import Data.Hashable
import qualified Data.Foldable as F
import Structure.Internal
  ( Slots
  , initSlots
  , fromList
  , size
  , member
  , insert
  , delete
  , mapOA
  , filterOA
  )


toList :: Slots k -> [k]
toList = F.toList

foldlOA :: (a -> k -> a) -> a -> Slots k -> a
foldlOA = foldl

foldrOA :: (a -> b -> b) -> b -> Slots a -> b
foldrOA = foldr
