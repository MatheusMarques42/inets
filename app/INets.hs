module INets (runINet) where

import INetStructures ( Rule(..), Connection(..), Term(..), INet(..) )

-- INet functionality

termSub :: Term -> Term -> Term -> Term
termSub b a (Tree s terms) = Tree s (map (termSub b a) terms)
termSub b a w = if w == b then a else w

-- apply the context rule, that is "links" two connected wires
link :: Term -> Term -> Connection -> Connection
link b a (Connection t1 t2) = Connection (termSub b a t1) (termSub b a t2)

-- apply an interaction rule
indirection :: Rule -> Int -> Term -> Term -> [Connection]
indirection (Rule (s1,s2) f) i (Tree st1 w1) (Tree st2 w2)
  | st1 == s1 && st2 == s2 = f i (Tree st1 w1) (Tree st2 w2)
  | st1 == s2 && st2 == s1 = f i (Tree st2 w2) (Tree st1 w1)
  | otherwise = []
indirection _ _ _ _ = []

step :: Int -> [Rule] -> INet -> INet
step _ _ (INet terms []) = INet terms []
step _ _ (INet terms ((Connection (Wire w) term):cs)) =
    INet (map (termSub (Wire w) term) terms) (map (link (Wire w) term) cs)

step _ _ (INet terms ((Connection term (Wire w)):cs)) =
    INet (map (termSub (Wire w) term) terms) (map (link (Wire w) term) cs)

step i rs (INet terms ((Connection t1 t2):cs)) =
    INet terms ((concat [indirection r i t1 t2| r<-rs])++cs)

combineConn :: Connection -> INet -> INet
combineConn c (INet ts cs) = INet ts (c:cs)

deepSearch :: Term -> Term -> Bool
deepSearch (Wire a) (Wire b) = a==b
deepSearch (Tree s1 as) (Tree s2 bs) = Tree s1 as == Tree s2 bs
deepSearch (Tree s as) (Wire b) = deepSearch (Wire b) (Tree s as)
deepSearch (Wire b) (Tree t (a:as)) = case a of
    (Wire x) -> x == b
    (Tree _ xs) -> any (deepSearch (Wire b)) xs || deepSearch (Wire b) (Tree t as)
deepSearch (Wire _) (Tree _ []) = False


isCycle :: Connection -> Bool
isCycle (Connection (Wire a) (Wire b)) = a==b
isCycle (Connection (Tree _ _) (Tree _ _)) = False
isCycle (Connection (Tree s xs) (Wire y)) = isCycle (Connection (Wire y) (Tree s xs))
isCycle (Connection (Wire y) (Tree s xs)) = deepSearch (Wire y) (Tree s xs)

runINet :: Int -> [Rule] -> INet -> INet
runINet _ _ (INet t []) = INet t []
runINet i rs (INet t (c:cs)) = if isCycle c 
    then combineConn c $ runINet i rs (INet t cs)
    else runINet (i+1) rs (step i rs (INet t (c:cs)))