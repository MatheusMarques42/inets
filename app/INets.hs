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

step :: [Connection] -> Int -> [Rule] -> INet -> (INet,[Connection])
step cycles _ _ (INet ts []) = (INet ts [],cycles)
step cycles _ _ (INet ts ((Connection (Wire w) t):cs)) =(
    INet (map (termSub (Wire w) t) ts) (map (link (Wire w) t) cs),
    map (link (Wire w) t) cycles
    )
step cycles i rs (INet ts (Connection t (Wire w): cs)) = step cycles i rs (INet ts (Connection (Wire w) t: cs))
step cycles i rs (INet ts ((Connection t1 t2):cs)) =
    (INet ts ((concat [indirection r i t1 t2| r<-rs])++cs),cycles) 

auxRunINet :: [Connection] -> Int -> [Rule] -> INet -> INet
auxRunINet cycles _ _ (INet ts []) = INet ts cycles
auxRunINet cycles i rs (INet ts (c:cs)) = if isCycle c
    then auxRunINet (c:cycles) i rs (INet ts cs)
    else auxRunINet cyclesF (i+1) rs newNet
    where
        (newNet,cyclesF) = step cycles i rs (INet ts (c:cs))

runINet :: [Rule] -> INet -> INet
runINet = auxRunINet [] 0
