{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

-- Exercise 1

-- Add an Employee to the GuestList (update the cached fun score appropriately)
-- At this stage ignore special cases such as direct subordinates contained in 
-- list etc
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

-- A Monoid instance for GuestList
instance Monoid GuestList where
    mempty = GL [] 0
    mappend gl1 (GL es _) = foldr glCons gl1 es

-- Return the guest list which is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

-- Exercise 2
-- treeFold' :: Tree Employee -> (GuestList, GuestList)
-- treeFold' (Node e []) = nextLevel e [(mempty,mempty)]
-- treeFold' (Node e ts) = nextLevel e (map treeFold' ts)

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node x []) = f x [z]
treeFold f z (Node x ts) = f x (map (treeFold f z) ts)

-- Exercise 3
-- Takes boss and list of results for subtree's under boss
-- Returns pair of GuestLists: (Best with boss, Best without boss)
-- (Including boss gives all subtree bosses 0 fun)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (bestWithBoss, bestWithoutBoss)
  where
    bestWithBoss    = glCons boss (foldr ((<>) . snd) mempty gls)
    bestWithoutBoss = foldr ((<>) . fst) mempty gls

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = bestFromPair . rootBestGLs
  where
    rootBestGLs = treeFold nextLevel (mempty,mempty)
    bestFromPair (gl1,gl2) = moreFun gl1 gl2

-- Exercise 5
main :: IO ()
main = do
    inpStr <- readFile "company.txt"
    let guestList = maxFun . buildTree $ inpStr
    putStrLn ("Total fun: " ++ show (getFun guestList))
    mapM_ putStrLn $ getNames guestList

buildTree :: String -> Tree Employee
buildTree = read

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getNames :: GuestList -> [Name]
getNames (GL es _) = map empName es







