module StdRules (lafInteractionCombinatorsRules,mazSimetricCombinatorsRules) where

import INetStructures (Rule)
import NetParser(toRules)
-- Standard rules for Lafont's Interaction Combinators and Mazza's Simetric Combinators
voidRule :: [Rule]
voidRule = toRules "Epsilon() X Epsilon()"
lafAnnihilation :: [Rule]
lafAnnihilation = toRules "Gamma(a,b)XGamma(b,a), Delta(a,b)XDelta(a,b)"
lafErase :: [Rule]
lafErase = toRules "Gamma(Epsilon(),Epsilon()) X Epsilon(), Delta(Epsilon(),Epsilon()) X Epsilon()"
lafCommute :: [Rule]
lafCommute = toRules "Delta(Gamma(a,b),Gamma(c,d)) X Gamma(Delta(d,b),Delta(c,a))"

lafInteractionCombinatorsRules :: [Rule]
lafInteractionCombinatorsRules = voidRule ++ lafAnnihilation ++ lafErase ++ lafCommute

mazAnnihilation :: [Rule]
mazAnnihilation = toRules "Zeta(a,b) X Zeta(a,b), Delta(a,b)XDelta(a,b)"
mazErase :: [Rule]
mazErase = toRules "Zeta(Epsilon(),Epsilon()) X Epsilon(), Delta(Epsilon(),Epsilon()) X Epsilon()"
mazCommute :: [Rule]
mazCommute = toRules "Delta(Zeta(a,b),Zeta(c,d)) X Zeta(Delta(d,b),Delta(c,a))"

mazSimetricCombinatorsRules :: [Rule]
mazSimetricCombinatorsRules = voidRule ++ mazAnnihilation ++ mazErase ++ mazCommute