module Lang.Eoc.Asm.RemoveJmp where

import Lang.Eoc.Types (PassM)
import Lang.Eoc.Asm.Types

removeJmp :: AsmDefs -> PassM AsmDefs
removeJmp (AsmDefsProgram info defs) = AsmDefsProgram info <$> traverse goDef defs
  where
    goDef (AsmDef defInfo name instrs) =
      return $ AsmDef defInfo name (go instrs)
    go [] = []
    go (Ilabel lbl i:is) = labelBlock lbl $ go (i:is)
    go (Ibranch lbl:cont@(i:_))
      | Just lbls <- labels i, lbl `elem` lbls = go cont  -- Remove redundant branch
    go (i:is) = i : go is
