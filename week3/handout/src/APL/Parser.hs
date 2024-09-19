{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use <$" #-}
module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

keywords :: [String]
keywords = 
  [
    "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

-- Do not change this definition.
type Parser = Parsec Void String

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pLExp :: Parser Exp
pLExp = pAtom >>= chain
  where
    chain x =
        [
          If <$> (lString "if" <* pExp0) <*> (lString "then" <* pExp0) <*> (lString "else" <* pExp0) pAtom
        ]

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"¨
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lexeme :: Parser a -> Parser a
lexeme a = a <* space

lVName :: Parser VName
lVName = lexeme $ try $ do
    c <- satisfy isAlpha
    cs <- many $ satisfy isAlphaNum
    if c : cs `elem` keywords
      then fail "Unexpected keyword"
      else pure $ c : cs 

lBool :: Parser Bool
lBool =
  try $ lexeme $
    choice
      [
        const True <$> lKeyword "true",
        const False <$> lKeyword "false"
      ]

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)
