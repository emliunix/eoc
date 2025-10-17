module Lang.Eoc.Asm.PreludeConclusion where

import Control.Exception (throw)

import Lang.Eoc.Types
import Lang.Eoc.Asm.Types

preludeConclusion' :: Int -> ([Instr], [Instr])
preludeConclusion' stackSize = (prelude, conclusion)
  where
    prelude =
      [ Ilabel "main" $
        Ist (ArgReg RA) (ArgMemRef (-8) S0)
      , Ist (ArgReg S0) (ArgMemRef (-16) S0)
      , Iaddi (ArgReg S0) (ArgReg S0) (ArgImm (-stackSize))
      ]
    conclusion =
      [ Ilabel "conclusion" $
        Iaddi (ArgReg S0) (ArgReg S0) (ArgImm stackSize)
      , Ild (ArgReg S0) (ArgMemRef (-16) S0)
      , Ild (ArgReg RA) (ArgMemRef (-8) S0)
      , Iret
      ]

preludeConclusion :: Asm -> PassM Asm
preludeConclusion (AsmProgram info instrs) = pure $ AsmProgram info instrs'
  where
    stackSize = case aiStackSpace info of
      Just s -> s
      Nothing -> throw $ MyException "Stack size missing when adding prelude and conclusion"
    (prelude, conclusion) = preludeConclusion' stackSize
    instrs' = prelude ++ instrs ++ conclusion
