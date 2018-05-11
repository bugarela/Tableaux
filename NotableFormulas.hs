module NotableFormulas where
import Head

gamma n = (reverse (gamma' n),(Or (Proposition ("a" ++ show (n+1))) (Proposition ("b" ++ show (n+1)))))

gamma' 0 = [Or (Proposition "a1") (Proposition "b1")]
gamma' n = [Impl (Proposition ("b" ++ show n)) r] ++ [Impl (Proposition ("a" ++ show n)) r] ++ gamma' (n-1)
                  where r = (Or (Proposition ("a" ++ show (n+1))) (Proposition ("b" ++ show (n+1))))

statman n = (reverse (statman' (n)),(Or (Proposition ("p" ++ show (n))) (Proposition ("q" ++ show (n)))))

statman' 1 = [Or (Proposition "p1") (Proposition "q1")]
statman' n = (Or (Impl a (Proposition ("p" ++ show n))) (Impl a (Proposition ("q" ++ show n)))):statman' (n-1) where a = statman'' n

statman'' 2 = Or (Proposition "p1") (Proposition "q1")
statman'' n = And (statman'' (n-1)) (Or (Proposition ("p" ++ show (n-1))) (Proposition ("q" ++ show (n-1))))
