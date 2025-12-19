module UnaryAritmetics (factorToINet, uaRules, myFactor, termToInt) where

import INetStructures ( Connection(..), Term(..), INet(..), Rule)
import Data.Hash (hash, asWord64)
import NetParser (toRules)

data Factor = Sum Factor Factor | Mult Factor Factor | Num Int

myFactor :: Factor
myFactor = Mult (Sum (Num 4) (Num 10)) (Num 15)

plusRules :: [Rule]
plusRules = toRules "Plus(x,S(y)) X S(Plus(x,y))" ++ toRules "Plus(x,x) X Zero()"

multRules :: [Rule]
multRules = toRules "Mult(Delta(a,b),c) X S(Mult(b,Plus(a,c)))" ++ toRules "Mult(Zero(),Zero()) X Zero()"

aditionalRules :: [Rule]
aditionalRules = toRules "Delta(Zero(),Zero()) X Zero()" ++ toRules "Delta(S(x),S(y)) X S(Delta(x,y))" ++ toRules "Zero() X Zero()"

uaRules :: [Rule]
uaRules = plusRules ++ multRules ++ aditionalRules

nextHash :: Int -> Int
nextHash = abs . fromInteger . toInteger . asWord64 . hash

intToTerm :: Int -> Term
intToTerm 0 = Tree "Zero" [] 
intToTerm n = Tree "S" [intToTerm (n-1)]

termToInt :: Term -> Int
termToInt (Wire _) = 0
termToInt (Tree _ []) = 0
termToInt (Tree _ ts) = 1 + termToInt (head ts)

factorToINet :: Int -> Factor -> INet
factorToINet _ (Num n) = INet [intToTerm n] []
factorToINet i (Sum a b) = INet [outWire] ([Connection (Tree "Plus" [head ta,outWire]) (head tb)] ++ ca ++ cb)
  where
    INet ta ca = factorToINet (nextHash i) a
    INet tb cb = factorToINet (nextHash (i+1)) b
    outWire = Wire ("out" ++ show i)
factorToINet i (Mult a b) = INet [outWire] ([Connection (Tree "Mult" [head ta,outWire]) (head tb)] ++ ca ++ cb)
  where
    INet ta ca = factorToINet (nextHash i) a
    INet tb cb = factorToINet (nextHash (i+1)) b
    outWire = Wire ("out" ++ show i)
