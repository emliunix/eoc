module Lang.Eoc.Asm.Interference where

import Lang.Eoc.Types (MyException(..), PassM)
import Lang.Eoc.Asm.Types

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

interferes :: Instr -> Set Arg -> [(Arg, Arg)]
interferes (IMv d s) lives =
  [ (d, r)
  | r <- Set.toList lives
  , r /= d && r /= s
  ]
interferes (ICall _) lives =
  [ (r1, r2)
  | r1 <- map ArgReg callerSavedRegs
  , r2 <- Set.toList lives
  , r1 /= r2
  ]
interferes i lives =
  [ (r1, r2)
  | r1 <- Set.toList $ writeLocs i
  , r2 <- Set.toList lives
  , r1 /= r2
  ]

buildFromBlock :: [Instr] -> Set Arg -> [(Arg, Arg)]
buildFromBlock instrs liveAfter =
  let lives = foldr (\i acc@(lives:_) -> (liveBefore i lives) : acc) [liveAfter] instrs
  in concat $ zipWith interferes instrs (tail lives)

buildGraph :: [(String, [Instr])] -> Map String (Set Arg) -> Map Arg (Set Arg)
buildGraph blocks livesAfterMap = error "not implemented"

buildInterferenceGraph :: Asm -> PassM Asm
buildInterferenceGraph _ = error "not implemented"
