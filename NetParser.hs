module NetParser (toINet,toRules) where

import INetStructures
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import Text.ParserCombinators.ReadP (many1)

type Parser = Parsec Void String

-- Interaction Nets parser

netParser :: Parser INet
netParser = do
    terms <- termP  `sepBy` char ','
    char '|'
    conns <- connP `sepBy` char ','
    return $ INet terms conns

connP :: Parser Connection
connP = do
    term1 <- termP
    string "=="
    term2 <- termP
    return $ Connection term1 term2

termP :: Parser Term
termP = treeP <|> wireP

treeP :: Parser Term
treeP = do
    h <- upperChar
    t <- many lowerChar
    terms <- treeBodyP <|> emptyBodyP

    return $ Tree (h:t) terms

treeBodyP ::Parser [Term]
treeBodyP = do
    char '('
    terms <- termP `sepBy` char ','
    char ')'
    return terms

emptyBodyP :: Parser [Term]
emptyBodyP = do
    return []

wireP :: Parser Term
wireP = do
    h <- lowerChar
    t <- many (lowerChar <|>digitChar)
    return $ Wire (h:t)

toINet :: String -> INet
toINet s = case eitherNet of
    Left _ -> INet [] []
    Right net -> net
    where
        eitherNet = parse netParser "" (filter (`notElem` "\n \t") s)

-- INet Rule Parser

createRule :: Term -> Term -> Rule
createRule (Tree s1 t1) (Tree s2 t2) = Rule (s1,s2) func
    where
        func i (Tree _ ti1) (Tree _ ti2) =
            (zipWith Connection (map (indexed i) t1) ti1) ++ (zipWith Connection (map (indexed i) t2) ti2)
        indexed i (Wire w) = Wire (w++show i)
        indexed i (Tree s t) = Tree s (map (indexed i) t)

rulesP :: Parser [Rule]
rulesP = ruleP `sepBy` char ','

ruleP :: Parser Rule
ruleP = do
    t1 <- treeP
    char 'X'
    t2 <- treeP
    return $ createRule t1 t2

toRules :: String -> [Rule]
toRules s = case eitherRule of
    Left _ -> []
    Right rules -> rules
    where
        eitherRule = parse rulesP "" (filter (`notElem` "\n \t") s)


-- Standard rules for Lafont's Interaction Combinators and Mazza's Simetric Combinators
voidRule = toRules "Epsilon() X Epsilon()"
lafAnnihilation = toRules "Gamma(a,b)XGamma(b,a), Delta(a,b)XDelta(a,b)"
lafErase = toRules "Gamma(Epsilon(),Epsilon()) X Epsilon(), Delta(Epsilon(),Epsilon()) X Epsilon()"
lafCommute = toRules "Delta(Gamma(a,b),Gamma(c,d)) X Gamma(Delta(d,b),Delta(c,a))"

lafInteractionCombinatorsRules = voidRule ++ lafAnnihilation ++ lafErase ++ lafCommute

mazAnnihilation = toRules "Zeta(a,b) X Zeta(a,b), Delta(a,b)XDelta(a,b)"
mazErase = toRules "Zeta(Epsilon(),Epsilon()) X Epsilon(), Delta(Epsilon(),Epsilon()) X Epsilon()"
mazCommute = toRules "Delta(Zeta(a,b),Zeta(c,d)) X Zeta(Delta(d,b),Delta(c,a))"

mazSimetricCombinatorsRules = voidRule ++ mazAnnihilation ++ mazErase ++ mazCommute
