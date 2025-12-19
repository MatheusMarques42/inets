module Main where
import INets ( runINet )
import INetStructures (INet(..))
-- import NetParser ( toINet )
-- import StdRules (lafInteractionCombinatorsRules, mazSimetricCombinatorsRules)
-- import Lambda (toLambdaNet)
import UnaryAritmetics (factorToINet, uaRules, myFactor, termToInt)

main :: IO ()
main = do
  -- input <- getLine
  -- let net = toINet input
  -- let out = runINet lafInteractionCombinatorsRules net
  -- print out
  -- input <- getLine
  -- let net = toLambdaNet input
  -- let out = runINet mazSimetricCombinatorsRules net
  -- print out
  let net = factorToINet 0 myFactor
  let (INet terms _) = runINet uaRules net
  print $ termToInt $ head terms
