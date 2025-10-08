module Lang.Eoc.Asm.Liveness where

import Data.Set (Set)
import qualified Data.Set as Set

-- dataflow :: (Ord a) => (b -> Set a) -> (b -> [b]) -> b -> Set a
