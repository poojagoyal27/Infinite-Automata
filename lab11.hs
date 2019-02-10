---lab11
---implementation of the FSM minimization algorithm 
---Pooja Goyal---Student ID---109896598----


import Data.List (nub,sort,tails)

-- Sigma = [a,b] for testing, but must work for any finite list
sigma :: [Char]
sigma = "ab"



-- Finite state machines 
-- M = FSM {states=qs, start=s, finals=fs, delta=d}
data FSM a = FSM {
  states :: [a],
  start  :: a,
  finals :: [a],
  delta  :: [(a,Char,a)]
  }

instance Show a => Show (FSM a) where
  show m = "("   ++ show (states m) ++
           ", "  ++ show (start m)  ++
           ", "  ++ show (finals m) ++
           ", [" ++ steps (delta m) ++ "])" where
    steps [] = []
    steps [t] = step t
    steps (t:ts) = step t ++ "," ++ steps ts
    step (q,c,q') = show q ++ "/" ++ [c] ++ ">" ++ show q'

-- Cartesian product 
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]   

-- Remove adjacent duplicates
rmdups [] = []
rmdups [x] = [x]
rmdups (x:ys) | x == head ys = rmdups ys
              | otherwise = x : rmdups ys

-- function for given  list of pairs
ap2 ((x,y):rs) a = if x == a then y else ap2 rs a


-- Closure function
closure :: Ord a => [a] -> (a -> [a]) -> [a]
closure start step = stable $ iterate close (start,[]) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where  
      xs' = fr ++ xs
      fr' = nub $ filter (`notElem` xs') (concatMap step fr)


-- Minimization function
minimize :: Ord a => FSM a -> FSM a
minimize (FSM {states=qs, start=s, finals=fs, delta=ts}) = FSM {
  states = reps,
  start  = rep s,
  finals = [q | q <- reps, elem q fs],
  delta  = [(q,a,rep q') | (q,a,q') <- ts, elem q reps]
  } where
    qs' = sort qs
    lt  = [(q1,q2) | (q1:rest) <- tails qs', q2 <- rest]    
    leq = [(q1,q2) | (q1:rest) <- tails qs', q2 <- q1:rest] 
    dinv q a = [q1 | (q1,b,q2) <- ts, q2 == q, b == a]     
    dist = closure init step                                
    init = [p | p <- lt, elem (fst p) fs /= elem (snd p) fs]
    step (q1,q2) = concatMap (\a -> dinv q1 a >< dinv q2 a) sigma
    eq  = [p | p <- leq, notElem p dist]                             
    eq' = [p | (p:rs) <- tails eq, null rs || fst p < fst (head rs)] 
    reps = rmdups $ map snd eq'                             
    rep = ap2 eq'                                          

