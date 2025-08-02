module Main where
import INets ( runINet )
import NetParser ( toINet )
import StdRules (lafInteractionCombinatorsRules)

main :: IO ()
main = do
  input <- getLine
  let net = toINet input
  let out = runINet lafInteractionCombinatorsRules net
  print out

