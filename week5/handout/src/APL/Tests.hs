module APL.Tests where

import APL.Eval
  ( 
    eval,
    runEval,
  )
import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen, elements, shrink, listOf, sample, oneof, sized, Arbitrary, arbitrary, quickCheck, withMaxSuccess)

instance Arbitrary Exp where
    arbitrary = sized genExp
    shrink (Add e1 e2) =
        e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
    shrink (If cond e1 e2) =
        e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
    shrink (Var x) =
        [Var x' | x' <- shrink x, not (null x')]
    shrink (Let x e1 e2) =
        e1 : [Let x' e1 e2 | x' <- shrink x, not (null x')] ++ [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
    shrink (Lambda x e) =
        e : [Lambda x' e | x' <- shrink x, not (null x')] ++ [Lambda x e' | e' <- shrink e]

genVar :: Gen VName
genVar = do
    x <- elements ['A' .. 'Z']
    y <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (x : y)


genExp :: Int -> Gen Exp
genExp size =
    if size <= 1
    then oneof 
    [
        Var <$> genVar,
        CstInt <$> arbitrary,
        CstBool <$> arbitrary
    ]
    else
        let half = (size - 1) `div` 2
        in oneof 
            [ CstInt <$> arbitrary, 
              CstBool <$> arbitrary, 
              Var <$> genVar,
              Lambda <$> genVar <*> genExp (size - 1),
              Apply <$> genExp half <*> genExp half,
              Add <$> genExp half <*> genExp half,
              Sub <$> genExp half <*> genExp half,
              Mul <$> genExp half <*> genExp half,
              Div <$> genExp half <*> genExp half,
              Pow <$> genExp half <*> genExp half,
              Eql <$> genExp half <*> genExp half,
              If <$> genExp half <*> genExp half <*> genExp half,
              Let <$> genVar <*> genExp half <*> genExp half,
              TryCatch <$> genExp half <*> genExp half
            ]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))