-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a 

-- Generalised to R pegs
-- Define p1 as the source peg and p2 as the destination
-- p3... are temporary storage
-- Frame-Stewart:
-- Base case 1 disk: then move it to it's destination
-- 1: for some k, 1 <= k < n, transfer top k disks to a single other peg (p3).
-- 2: move remaining pegs (n-k) to dest p2 without using p3 (contains top k).
-- 3: Move the k pegs from storage p3 to dest p2
hanoiR :: Integer -> [Peg] -> [Move]
hanoiR 0 _ = []
hanoiR 1 (p1 : p2 : rest) = [(p1,p2)]
hanoiR n (p1 : p2 : p3 : rest) = 
    hanoiR k (p1 : p3 : p2 : rest) ++
    hanoiR (n - k) (p1 : p2 : rest) ++
    hanoiR k (p3 : p2 : p1 : rest)
    where k 
           | null rest = n - 1     -- 3 peg case
           | otherwise = n - floor (sqrt (fromIntegral (2*n)) + 0.5)
          -- See Corollary 3.3 from:
          -- https://www2.bc.edu/~grigsbyj/Rand_Final.pdf
          -- Old: 
          -- | otherwise = n `div` 2 -- heuristice for choice of k

-- TODO: optimise choice of k (breadth first search)
