module Lang.Eoc.Asm.PreludeConclusion where

import Control.Exception (throw)

import Lang.Eoc.Types
import Lang.Eoc.Asm.Types

mkPrelude :: Int -> [Reg] -> String -> [Instr]
mkPrelude stackSize usedSavedRegs startLbl =
  [ Iaddi (ArgReg SP) (ArgReg SP) (ArgImm (-stackSize))
  , Isd (ArgReg RA) (ArgMemRef (stackSize - 8) SP)
  , Isd (ArgReg S0) (ArgMemRef (stackSize - 16) SP)
  , Iaddi (ArgReg S0) (ArgReg SP) (ArgImm stackSize)
  , Ibranch startLbl
  ] ++ zipWith mkSt usedSavedRegs [0..]
  where
    mkSt reg idx =
      Isd (ArgReg reg) (ArgMemRef (stackSize - 16 - 8 * (idx + 1)) SP)

mkConclusion :: Int -> [Reg] -> String -> [Instr]
mkConclusion stackSize usedSavedRegs conclusionLabel =
  labelBlock conclusionLabel instrs
  where
    instrs = zipWith mkLd usedSavedRegs [0..] ++
      [ Ild (ArgReg RA) (ArgMemRef (stackSize - 8) SP)
      , Ild (ArgReg S0) (ArgMemRef (stackSize - 16) SP)
      , Iaddi (ArgReg SP) (ArgReg SP) (ArgImm stackSize)
      , Iret
      ]
    mkLd reg idx =
      Ild (ArgReg reg) (ArgMemRef (stackSize - 16 - 8 * (idx + 1)) S0)

preludeConclusion :: AsmDefs -> PassM AsmDefs
preludeConclusion (AsmDefsProgram info defs) = AsmDefsProgram info <$> traverse goDef defs
  where
    goDef (AsmDef info name instrs) =
      let
        stackSize = case aiStackSpace info of
          Just s -> s
          Nothing -> throw $ MyException
            "Stack size missing when adding prelude and conclusion"
        usedSavedRegs = case aiUsedSavedRegs info of
          Just rs -> rs
          Nothing -> throw $ MyException
            "Used saved registers missing when adding prelude and conclusion"
        startLbl = aiStartLabel info
        conclLbl = aiConclusionLabel info
        prelude = mkPrelude stackSize usedSavedRegs startLbl
        conclusion = mkConclusion stackSize usedSavedRegs conclLbl
        instrs' = prelude ++ instrs ++ conclusion
      in return $ AsmDef info name instrs'
