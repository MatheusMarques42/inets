-- module Lambda (toLambdaNet) where
module Lambda (runLambda) where 

import Data.Hash (asWord64, hash)
import Data.Void
import Data.Word
import Data.Char
import INetStructures (Connection (..), INet (..), Rule (..), Term (..))
import NetParser (toRules, toINet)
import INets (runINet, runPrinting)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

data LTerm = Function String LTerm | Application LTerm LTerm | Symbol String

instance Show LTerm where
  show (Function s a) = "\\" ++ s ++ "->" ++ show a
  show (Symbol s) = s
  show (Application t1 (Application t2 t3)) = show t1 ++ "(" ++ show (Application t2 t3) ++ ")"   
  show (Application t1 t2) = show t1 ++ " " ++ show t2 

type Parser = Parsec Void String

binary :: String -> (LTerm -> LTerm -> LTerm) -> Operator Parser LTerm
binary  name f = InfixL  (f <$ string name)

lambdaExprP :: Parser LTerm
lambdaExprP = makeExprParser lambdaP [[binary " " Application]]

lambdaP :: Parser LTerm
lambdaP = do
  try enclosedTermP <|> funcP <|> symbolP

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
  t <- many (lowerChar <|> digitChar)
  string "->"
  body <- lambdaP
  return $ Function (h : t) body

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
  return $ Symbol (h : t)

nextHash :: Int -> Int
nextHash = abs . fromInteger . toInteger . asWord64 . hash

-- Auxiliar function to linearize
linearizeAux :: Int -> String -> LTerm -> (LTerm, Int)
linearizeAux i str (Symbol s) = if s == str then (Symbol (s ++ show i), i+1) else (Symbol s, i)
linearizeAux i str (Function s term) = (Function s newTerm, newI)
  where
    (newTerm, newI) = linearizeAux i str term
linearizeAux i str (Application t1 t2) = (Application newT1 newT2, newI)
  where
    (newT1,auxI) = linearizeAux i str t1
    (newT2,newI) = linearizeAux auxI str t2

-- Make so every occurency of a symbol get an unique id after its name and count the number of copies, used in lambdaToTerm 
linearize ::  String -> LTerm -> (LTerm,Int)
linearize = linearizeAux 0 

lambdaDuplicators :: Int -> Int -> String -> Term
lambdaDuplicators 0 _ _ = Tree "Epsilon" []
lambdaDuplicators 1 c s = Tree "Decoder" [Wire (s ++ show c)]
lambdaDuplicators n c s = Tree "Gamma" [lambdaDuplicators (n `div` 2) c s, lambdaDuplicators (n `div` 2 + n `mod` 2) (n `div` 2 + c) s]

lambdaToTerm :: Int -> LTerm -> Term
lambdaToTerm _ (Symbol s) = Wire s
lambdaToTerm i (Function s t) = term
  where
    term = Tree "Lambda" [sonTerm, lambdaDuplicators c 0 s]
    sonTerm = lambdaToTerm i linearT
    (linearT,c) = linearize s t
lambdaToTerm i (Application t1 t2) = term
  where
    term = Tree "Apply" [t1Term, t2Term]
    t1Term = lambdaToTerm (nextHash i) t1
    t2Term = lambdaToTerm (nextHash i + 1) t2
-- lambdaToTerm :: Int -> LTerm -> (Term, [Connection])
-- lambdaToTerm _ (Symbol s) = (Wire s, [])
-- lambdaToTerm i (Function s t) = (term, cs)
--   where
--     term = Tree "Zeta" [Wire s, sonTerm]
--     cs = sonCon
--     (sonTerm, sonCon) = lambdaToTerm i t

-- lambdaToTerm i (Application t1 t2) = (term, cs)
--   where
--     term = Wire ("out" ++ show i)
--     cs = t1Conn ++ t2Conn ++ [Connection t1Term (Tree "Zeta" [t2Term, Wire ("out" ++ show i)])]
--     (t1Term, t1Conn) = lambdaToTerm (nextHash i) t1
--     (t2Term, t2Conn) = lambdaToTerm (nextHash i + 1) t2

toLambdaNet :: String -> INet
toLambdaNet s = case terms of
  Right t -> INet [Wire "out"] [Connection (Tree "Decoder" [Tree "Encoder" [Wire "out"]]) t]
  Left _ -> INet [] []
  where
    lambda = parse lambdaP "" s
    terms = lambda >>= \x -> Right (lambdaToTerm 1 x)

lippiRules :: [Rule]
lippiRules = comutRules ++ erasingRules ++ decodingRules ++ encodingRules ++ applicationRule

comutRules :: [Rule]
comutRules = toRules "Delta(Gamma(a,b),Gamma(c,d)) X Gamma(Delta(d,b),Delta(c,a)), Delta(Lambda(a,b),Lambda(c,d)) X Lambda(Delta(d,b),Delta(c,a)),\
                     \Delta(Apply(a,b),Apply(c,d)) X Apply(Delta(d,b),Delta(c,a)), Delta(a,b) X Delta(a,b), Delta(Decoder(a),Decoder(b)) X Decoder(Delta(a,b))"

erasingRules :: [Rule]
erasingRules = toRules "Epsilon() X Apply(Epsilon,Epsilon), Epsilon() X Lambda(Epsilon,Epsilon), Epsilon() X Gamma(Epsilon,Epsilon), Epsilon() X Epsilon()"

decodingRules :: [Rule]
decodingRules = toRules "Decoder(a) X Apply(Decoder(DApply(b,a)),b), Decoder(DLambda(a,b)) X Lambda(Decoder(a), Decoder(b)),\
                         \ Decoder(Delta(a,b)) X Gamma(Decoder(a),Decoder(b)), Decoder(Epsilon) X Epsilon, Decoder(a) X Decoder(a)"

encodingRules :: [Rule]
encodingRules = toRules "Encoder(a) X DApply(Decoder(Encoder(b)), Encoder(Apply(a,b))), Encoder(Lambda(a,b)) X DLambda(Encoder(a),Encoder(b)),\
                        \ Encoder(Gamma(a,b)) X Delta(Encoder(a),Encoder(b)), Encoder(Epsilon) X Epsilon, Encoder(a) X Encoder(a), \
                        \ Encoder(Decoder(a)) X Decoder(Encoder(a))"

applicationRule :: [Rule]
applicationRule = toRules "DApply(a,b) X DLambda(b,a)"
-- lippiRules =
--   toRules "\
--     \ Delta(Gamma(a,b),Gamma(c,d)) X Gamma(Delta(d,b),Delta(c,a)), Delta(Apply(a,b),Apply(c,d)) X Apply(Delta(d,b),Delta(c,a)), \
--     \ Delta(DApply(a,b),DApply(c,d)) X DApply(Delta(d,b),Delta(c,a)), Delta(Lambda(a,b),Lambda(c,d)) X Lambda(Delta(d,b),Delta(c,a)),  \
--     \ Delta(DLambda(a,b),DLambda(c,d)) X DLambda(Delta(d,b),Delta(c,a)), Delta(Decoder(a),Decoder(b)) X Decoder(Delta(b,a)), \
--     \ Delta(a,b) X Delta(a,b),\
--     \ Delta(Encoder(a),Encoder(b)) X Encoder(Gamma(b,a)), Delta(Epsilon,Epsilon) X Epsilon,\
--     \ Decoder(a) X Apply(Decoder(DApply(b,a)), b), Decoder(DLambda(a,b)) X Lambda(Decoder(a),Decoder(b)),\
--     \ Decoder(Delta(a,b)) X Gamma(Decoder(a),Decoder(b)), Decoder(a) X Decoder(a), \
--     \ Decoder(Encoder (a)) X Encoder(Decoder(a)), Decoder(Epsilon) X Epsilon, \
--     \ Encoder(a) X DApply(Decoder(Encoder(b)),Encoder(Apply(a,b))), Encoder(Lambda(a,b)) X DLambda(Encoder(a), Encoder(b)), \
--     \ Encoder(Epsilon) X Epsilon, Encoder(Encoder(a)) X Encoder(Encoder(a))\
--     \ Apply(Epsilon,Epsilon) X Epsilon, DApply(Epsilon,Epsilon) X Epsilon, Lambda(Epsilon,Epsilon) X Epsilon,\
--     \ DLambda(Epsilon,Epsilon) X Epsilon, Gamma(Epsilon,Epsilon) X Epsilon,\
--     \ DApply(a,b) X DLambda(b,a)\
--     \"

runLambda :: String -> INet
runLambda = runINet lippiRules.toLambdaNet 

testLambda :: INet
testLambda = runINet lippiRules $ toLambdaNet "\\x->\\y->\\z->(x y) z"
-- lambdaSub :: LTerm -> LTerm -> LTerm -> LTerm
-- lambdaSub a b (Symbol s) = if a == Symbol s then b else Symbol s
-- lambdaSub a b (Application t1 t2) = Application (lambdaSub a b t1) (lambdaSub a b t2)
-- lambdaSub a b (Function x t) = Function x (lambdaSub a b t)

getWires :: Term -> [String]
getWires (Wire w) = [w]
getWires (Tree _ ts) = concatMap getWires ts

translateSymbol :: [([String],String)] -> String -> String
translateSymbol [] _ = "errorSymbol:0" 
translateSymbol (([],_): xs) z = translateSymbol xs z
translateSymbol ((x:xs,y):xss) z = if z==x then y else translateSymbol ((xs,y):xss) z

netToLambdaAux :: [String] -> [([String],String)] -> Term -> LTerm
netToLambdaAux _  table (Wire w) = Symbol (translateSymbol table w)
netToLambdaAux vs table (Tree s ts) = case s of
  "Apply" -> Application (netToLambdaAux vs table (head ts)) (netToLambdaAux vs table (ts!!1))
  "Lambda" -> Function (head vs) (netToLambdaAux (tail vs) newTable (head ts)) 
  _ -> Symbol "errorSymbol:1:"
  where
    newTable = (getWires $ ts!!1,head vs) : table

netToLambda :: INet -> LTerm
netToLambda (INet t _) = netToLambdaAux variableList [] (head t)
  where
    variableList = alphabet ++ [l ++ show n | n <- [0..],l <- alphabet]
    alphabet = map (\x->[chr x]) [97..122] 

-- lippiRules :: [Rule]
-- lippiRules =
--   toRules
--     "\
--     \ Delta(Apply(a,b),Apply(c,d)) X Apply(Delta(d,b),Delta(c,a)),  Delta(Lambda(a,b),Lambda(c,d)) X Lambda(Delta(d,b),Delta(c,a),\
--     \ Delta(Gamma(a,b),Gamma(c,d)) X Gamma(Delta(d,b),Delta(c,a)),  Delta(Epsilon(),Epsilon()) X Epsilon(),\
--     \"
