import Head
import Parser
import PrefixParser

prove' n f = do (g,p) <- f n
                let g' = map (\x -> T x) g
                let p' = F p
                return (tableaux 1 Open (g' ++ [p']))

tableaux = undefined

alphas [] = []
alphas ((T (Not a)):fs) = ((F a):fs)
alphas ((F (Not a)):fs) = ((T a):fs)
alphas ((T (Or a b)):fs) = [T a,T b] ++ fs
alphas ((F (Or a b)):fs) = [F a,F b] ++ fs
alphas ((F (Impl a b)):fs) = [T a,F b] ++ fs
alphas ((a):fs) = alphas fs
