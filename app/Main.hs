module Main where
import INets ( runINet )
import NetParser ( toINet )
import StdRules (lafInteractionCombinatorsRules, mazSimetricCombinatorsRules)
import Lambda (toLambdaNet)

main :: IO ()
main = do
  -- input <- getLine
  -- let net = toINet input
  -- let out = runINet lafInteractionCombinatorsRules net
  -- print out
  input <- getLine
  let net = toLambdaNet input
  let out = runINet mazSimetricCombinatorsRules net
  print out

