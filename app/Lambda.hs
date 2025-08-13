module Lambda (toLambdaNet) where

import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import INetStructures ( Connection(..), Term(..), INet(..) )
import Data.Hash (hash, asWord64)
import Data.Word

data LTerm = Function String LTerm | Application LTerm LTerm | Symbol String deriving Show

type Parser = Parsec Void String

lambdaP :: Parser LTerm
lambdaP = do
  try appP <|> enclosedTermP <|> funcP <|> symbolP


enclosedTermP :: Parser LTerm
enclosedTermP = do
  char '('
  term <- lambdaP
  char ')'
  return term


funcP :: Parser LTerm
funcP = do
  char '\\'
  h <- lowerChar
  t <- many (lowerChar <|>digitChar)
  string "->"
  body <- lambdaP
  return $ Function (h:t) body 

appP :: Parser LTerm
appP = do
  first <- firstTermP
  space1
  second <- lambdaP
  return $ Application first second

firstTermP :: Parser LTerm
firstTermP = do
  funcP <|> symbolP <|> enclosedTermP
  
symbolP :: Parser LTerm
symbolP = do
  h <- lowerChar
  t <- many (lowerChar <|> digitChar)
  return $ Symbol (h:t)

nextHash :: Int -> Int
nextHash = abs . fromInteger . toInteger . asWord64 . hash

lambdaToTerm :: Int -> LTerm -> (Term,[Connection])
lambdaToTerm _ (Symbol s) = (Wire s, [])
lambdaToTerm i (Function s t) = (term, cs)
  where
    term = Tree "Zeta" [Wire s,sonTerm]
    cs = sonCon
    (sonTerm, sonCon) = lambdaToTerm i t

lambdaToTerm i (Application t1 t2) = (term, cs)
  where
    term = Wire ("out" ++ show i)
    cs = t1Conn ++ t2Conn ++ [Connection t1Term (Tree "Zeta" [t2Term, Wire ("out" ++ show i)])]
    (t1Term,t1Conn) = lambdaToTerm (nextHash i) t1
    (t2Term,t2Conn) = lambdaToTerm (nextHash i+1) t2

toLambdaNet :: String -> INet
toLambdaNet s = case terms of
  Right (t,cs) -> INet [t] cs
  Left _ -> INet [] []
  where
    lambda = parse lambdaP "" s
    terms = lambda >>= \x -> Right (lambdaToTerm 1 x)

