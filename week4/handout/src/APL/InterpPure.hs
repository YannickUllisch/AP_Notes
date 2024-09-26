module APL.InterpPure (runEval, stateInitial) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], Right x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' x)) = runEval' r s' x
    runEval' r s (Free (PrintOp p x)) =
      let (p', x') = runEval' r s x
        in (p : p', x')
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)

