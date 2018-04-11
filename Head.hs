module Head where

data Formula = Or Formula Formula
             | And Formula Formula
             | Impl Formula Formula
             | Not Formula
             | Proposition String
             | Falsum
             deriving (Eq,Show)

data LabeledFormula = T Formula | F Formula

type Branch = [LabeledFormula]

data Tableaux = Closed Branch | Open Branch
