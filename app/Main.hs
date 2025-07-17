module Main where
import INets
import NetParser
import INetStructures

main :: IO ()
main = do
  input <- getLine
  let net = toINet input
  let out = runINet 0 lafInteractionCombinatorsRules net
  print out

