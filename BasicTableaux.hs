import Head
import Parser
import PrefixParser
import NotableFormulas

-- main function, proves stuff
prove a = do as <- parseFile a
             proveFile as
             return()

-- alt to main function, proves stuff given in the weird prefix form
provePrefix a = do as <- parseFile' a
                   proveFile as
                   return()

-- extract seq from parser return and calls tableaux'
proveFile (gs,p) = case p of
                      Left err -> print err
                      Right p -> case (extract gs) of
                                    Right s -> print (tableaux' (fold s) p)
                                    Left errs -> print errs

proveNotable (gs,p) = tableaux' gs p

-- b counts branches, r counts rules applied
tableaux b r (Closed fs) = Proved b r
tableaux b r (Open fs) = let a = alphas fs in case a of
                            Unsaturated (fs,as) -> tableaux b (r+1) (tryToClose (fs ++ as))
                            Saturated -> let bs = betas fs in case bs of
                                                   Unsaturated (fs,[b1,b2]) -> branch (tableaux b (r+1) (tryToClose (fs ++ [b1]))) (tableaux b 0 (tryToClose (fs ++ [b2])))
                                                   Saturated -> FalseValue (values fs) b r

tableaux' g p = let g' = map (\x -> T x) g
                    p' = F p
                in tableaux 1 0 (tryToClose (g' ++ [p']))

alphas fs = alphas' fs []
alphas' [] _ = Saturated
alphas' (f:fs) rs = if alphaf /= [] then Unsaturated (rs ++ fs,alphaf) else (alphas' fs (rs ++ [f])) where alphaf = alpha f

alpha (T (Not a)) = [F a]
alpha (F (Not a)) = [T a]
alpha (T (And a b)) = [T a,T b]
alpha (F (Or a b)) = [F a,F b]
alpha (F (Impl a b)) = [T a,F b]
alpha _ = []

betas fs = betas' fs []
betas' [] _ = Saturated
betas' (f:fs) rs = if betaf /= [] then Unsaturated (reverse (rs ++ fs),betaf) else (betas' fs (rs ++ [f])) where betaf = beta f

beta (F (And a b)) = [F a,F b]
beta (T (Or a b)) = [T a,T b]
beta (T (Impl a b)) = [F a,T b]
beta _ = []

-- checks whether a branch is closed or open
tryToClose fs = if closed fs then Closed fs else Open fs

closed [] = False
closed (T Falsum:fs) = True
closed (T f:fs) = if (filter (== (F f)) fs) /= [] then True else closed fs
closed (F f:fs) = if (filter (== (T f)) fs) /= [] then True else closed fs

values [] = []
values ((T (Proposition p)):fs) = (p,True):values fs
values ((F (Proposition p)):fs) = (p,False):values fs
values (f:fs) = values fs

-- Returns 'Proved' iff both branches have returned a proof. Otherwise, returns 'FalseValue' and the values
-- In addition, returns the counters for branches and rules applied.
branch (FalseValue v b r) _ = (FalseValue v b r) -- lazy evaluation prevents unnecessary work
branch (Proved b1 r1) (FalseValue v b2 r2) = (FalseValue v (b1+b2) (r1+r2))
branch (Proved b1 r1) (Proved b2 r2) = Proved (b1+b2) (r1+r2)


-- Ugly stuff to handle parse errors
fromRight (Right x) = x

extract ds = if (extract' ds) then Right (map fromRight ds) else Left ([extractErr ds])

extract' [] = True
extract' (d:ds) = case d of
                  Left err -> False
                  Right a -> True && (extract' ds)

extractErr (d:ds) = case d of
                     Left err -> err
                     Right a -> extractErr ds

-- foldr1 doesn't like empty lists
fold [] = []
fold (f:fs) = f ++ fold fs
