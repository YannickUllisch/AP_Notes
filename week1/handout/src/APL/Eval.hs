{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module APL.Eval
  (
    Val(..),
    eval,
    envEmpty
  )
where

import APL.AST (Exp (..), VName)

-- Type declarations
type Error = String
type Env = [(VName, Val)]

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend n v env = (n, v) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup n env = lookup n env


eval :: Env -> Exp -> Either Error Val
eval env (CstInt a) = Right $ ValInt a
eval env (CstBool a) = Right $ ValBool a

eval env (Add a b) =
  case (eval env a, eval env b) of
    (Left err, _ ) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x + y
    (Right _, Right _) -> Left "Non Integer Operant"

eval env (Sub a b) =
  case (eval env a, eval env b) of
    (Left err, _ ) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x - y
    (Right _, Right _) -> Left "Non Integer Operant"
eval env (Mul a b) =
  case (eval env a, eval env b) of
    (Left err, _ ) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x * y
    (Right _, Right _) -> Left "Non Integer Operant"
eval env (Div a b) =
  case (eval env a, eval env b) of
    (Left err, _ ) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValInt 0)) -> Left "Cannot divide by zero"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x `div` y)
    (Right _, Right _) -> Left "Non Integer Operant"
eval env (Pow a b) =
  case (eval env a, eval env b) of
    (Left err, _ ) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValInt y)) | y < 0 -> Left "Cannot Pow with a negative number"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x ^ y)
    (Right _, Right _) -> Left "Cannot match Bool with Int"

eval env (Eql a b) =
  case (eval env a, eval env b) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Non Integer Operant"
    
eval env (If cond a b) =
  case eval env cond of
    Left err -> Left err
    (Right (ValBool True)) -> eval env a
    (Right (ValBool False)) -> eval env b
    Right _ -> Left "Conditional has to be of bool type"

eval env (Var n) = 
  case envLookup n env of
    Just a -> Right a
    Nothing -> Left "Unknown variable"

eval env (Let n a b) = 
  case eval env a of 
    Left err -> Left err
    Right v -> eval (envExtend n v env) b
