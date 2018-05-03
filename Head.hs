module Head where

data Formula = Or Formula Formula
             | And Formula Formula
             | Impl Formula Formula
             | Not Formula
             | Proposition String
             | Falsum
             deriving (Eq)

data LabeledFormula = T Formula | F Formula deriving (Eq,Show)

type Branch = [LabeledFormula]

data LabeledBranch = Closed Branch | Open Branch deriving (Eq,Show)

data MaybeSaturated = Unsaturated ([LabeledFormula],[LabeledFormula]) | Saturated

data Tableaux = FalseValue [(String,Bool)] Int Int | Proved Int Int

instance Show Tableaux where
    show (FalseValue v b r) = "False - " ++ show v ++ "\nNumber of branches: " ++ show b ++ "\nNumber of rules applied: " ++ show r
    show (Proved b r) = "True" ++ "\nNumber of branches: " ++ show b ++ "\nNumber of rules applied: " ++ show r

instance Show Formula where
    show (Or a b) = show a ++ " v " ++ show b
    show (And a b) = show a ++ " ^ " ++ show b
    show (Impl a b) = show a ++ " -> " ++ show b
    show (Not a) = "¬ " ++ show a
    show (Proposition a) = show a
    show (Falsum) = "Falsum"
