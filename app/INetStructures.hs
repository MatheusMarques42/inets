module INetStructures (INet(..), Term(Tree,Wire), Connection(..), Rule(..)) where

-- Datatypes 

data INet = INet [Term] [Connection]

data Term = Tree String [Term] | Wire String deriving Eq
data Connection = Connection Term Term

data Rule = Rule (String,String) (Int -> Term -> Term -> [Connection])

instance Show INet where
    show (INet terms connections) = sterms ++ "|" ++ sconnections
        where
            sterms = tail $ init $ show terms
            sconnections = tail $ init $ show connections

instance Show Term where
    show (Tree s terms) = if isNullary then s else s ++ "(" ++ sterms ++ ")"
        where
            isNullary = null terms
            sterms = tail $ init $ show terms
    show (Wire s) = s

instance Show Connection where
    show (Connection a b) = show a ++ "==" ++ show b

instance Show Rule where
    show (Rule (s1, s2) _) = s1 ++ " X " ++ s2 
