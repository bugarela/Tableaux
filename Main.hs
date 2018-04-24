import Head
import Parser
import PrefixParser

prove a = do as <- parseFile a
             proveFile as
             return()

provePrefix a = do as <- parseFile' a
                   proveFile as
                   return()

tableaux' g p = let g' = map (\x -> T x) g
                    p' = F p
                in tableaux 1 (Open (g' ++ [p']))

proveFile (gs,p) = case p of
                      Left err -> print err
                      Right p -> case (extract gs) of
                                    Right s -> print (tableaux' (fold s) p)
                                    Left errs -> print errs

tableaux n (Closed fs) = Proved n
tableaux n (Open fs) = let a = alphas fs in case a of
                         Unsaturated (fs,as) -> tableaux n (tryToClose (fs ++ as))
                         Saturated -> let b = betas (reverse fs) in case b of
                                                Unsaturated (fs,[b1,b2]) -> branch (tableaux (n+1) (tryToClose (fs ++ [b1]))) (tableaux (n+1) (tryToClose (fs ++ [b2])))
                                                Saturated -> FalseValue (values fs) n

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

tryToClose fs = if closed fs then Closed fs else Open fs

closed [] = False
closed (T f:fs) = if (filter (== (F f)) fs) /= [] then True else closed fs
closed (F f:fs) = if (filter (== (T f)) fs) /= [] then True else closed fs

values [] = []
values ((T (Proposition p)):fs) = (p,True):values fs
values ((F (Proposition p)):fs) = (p,False):values fs
values (f:fs) = values fs

branch (FalseValue a n) _ = (FalseValue a n) -- lazy ftw
branch (Proved m) (FalseValue a n) = (FalseValue a (n+m))
branch (Proved n) (Proved m) = Proved (n+m)


-- ugly stuff down here
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
