{- |
Module      :  Euler.Data.SkewHeap
Description :  Top-down skew heaps
Copyright   :  (c) Kamil Kurkiewicz
License     :  NONE

Maintainer  :  Kamil Kurkiewicz <k-kurkiewicz@outlook.com>
Stability   :  experimental
Portability :  portable

A common type of basic* minimum-oriented /top-down/ skew heaps
for types implementing 'Ord'. Efficient in only an amortized sense.

\*Does not implement /delete/ (see [1 s4])

Sources used and further reading:

    1. Sleator DD, Tarjan RE. /Self-Adjusting Heaps/.
       SIAM J Comput [Internet]. 1986 Feb;15(1):52-69.
       doi:<https://doi.org/10.1137/0215004>.
    2. Tarjan RE. /Data Structures and Network Algorithms/.
       [Philadelphia (PA)]: Society for Industrial and Applied Mathematics; 1983. p. 131.
    3. Jones DW. /A Note on Bottom-Up Skew Heaps/.
       SIAM J Comput [Internet]. 1987 Feb;16(1):108-110.
       doi:<https://doi.org/10.1137/0216009>.
    4. Jones DW. /An Empirical Comparison of Priority-Queue and Event-Set Implementations/.
       Commun ACM. [Internet]. 1986 Apr;29(4):300-311.
       doi:<https://doi.org/10.1145/5684.5686>.
    
    5. Okasaki C. /EdisonAPI: A library of efficient, purely-functional data structures (API)/. [cited 2021 May 30].
       In: Hackage: The Haskell Package Repository [Internet]. [place unknown: publisher unknown]. c2008 -   .
       Available from: <https://hackage.haskell.org/package/EdisonAPI-1.3.1>.
    6. Canou B, Darrasse A. /Fast and Sound Random Generation for Automated Testing and Benchmarking in Objective Caml/.
       In: Proceedings of the 2009 ACM SIGPLAN Workshop on ML; 2009 Aug 30; Edinburgh, Scotland.
       New York: Association for Computing Machinery; c2009. p. 61-270.
       doi:<https://doi.org/10.1145/1596627.1596637>.

-}
module Euler.Data.SkewHeap (
   -- *SkewHeap type
   SkewHeap (),
   
   -- *Construction
   empty,
   singleton,
   fromList,
   
   -- *Insertion
   insert,
   insertAll,
   
   -- *Deletion\/Update
   deleteMin,
   deleteMinE,
   purge,
   
   -- *Query
   findAll,
   findMin,
   findMinE,
   isEmpty,
   isSingleton,
   size,
   
   -- *Combine
   meld,
   
   -- *Conversion
   toList,
   unSingleton,
   
   -- *Debugging
   showTree
 ) where


import           Control.Monad
import qualified Data.List as List
import           Euler.Data.PriorityQueue (PriorityQueue)
import qualified Euler.Data.PriorityQueue as PriorityQueue
import qualified Euler.Data.Queue as Queue
import           Test.QuickCheck as QC


------------------------------------------------------------


-- |The structure
-- 
-- INVARIANT If \(p(x)\) is the parent of \(x\), then \(p(x) \le x\)
-- 
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a) deriving (Eq, Show)


-- |Simple Arbitrary-SkewHeap instance for internal use
-- by the test suite /..\testsuite\tests\Euler\Data\SkewHeap.hs/ [5]
-- 
-- CAUTION Distribution of the generated test data not known [6]
-- 
instance (Arbitrary a, Ord a) => Arbitrary (SkewHeap a) where

    arbitrary = QC.sized arbitrarySkewHeap
      where
        arbitrarySkewHeap 0 = return Empty
        arbitrarySkewHeap n =
                QC.frequency [
                    (1, return Empty),
                    (4, liftM3 sift arbitrary (arbitrarySkewHeap (div n 2)) (arbitrarySkewHeap (div n 4)))
                 ]
        
        sift x0 h1 h2
            | Empty <- h1, Node x l r <- h2, x < x0 = Node x Empty (sift x0 l r)
            | Node x l r <- h1, Empty <- h2, x < x0 = Node x (sift x0 l r) Empty
            | Node x1 l1 r1 <- h1, Node x2 l2 r2 <- h2, x1 < x0 && x1 <= x2 = Node x1 (sift x0 l1 r1) h2
            | Node x1 l1 r1 <- h1, Node x2 l2 r2 <- h2, x2 < x0 = Node x2 h1 (sift x0 l2 r2)
            | otherwise = Node x0 h1 h2


-- |Makes 'SkewHeap' an instance of class 'PriorityQueue'
-- 
-- NOTE Requires @FlexibleInstances@
-- 
instance Ord a => PriorityQueue (SkewHeap a) a where

    empty      = empty
    isEmpty    = isEmpty
    insert     = insert
    findMin    = findMin
    findMinE   = findMinE
    deleteMin  = deleteMin
    deleteMinE = deleteMinE
    meld       = meld


-- |Creates an empty heap (time complexity: \(O(1)\))
empty :: SkewHeap a
empty = Empty


-- |True if the passed heap is empty (\(O(1)\))
isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False


-- |A heap of one element (\(O(1)\))
singleton :: a -> SkewHeap a
singleton x = Node x Empty Empty


-- |Indicates whether a heap contains exactly one element (\(O(1)\))
isSingleton :: SkewHeap a -> Bool
isSingleton (Node x Empty Empty) = True
isSingleton _                    = False


-- |Extract the only element of a singleton heap
unSingleton :: SkewHeap a -> a
unSingleton (Node x Empty Empty) = x
unSingleton _                    = error "Euler.Data.SkewHeap.unSingleton: not a singleton"


-- |Insert item @x@ in heap @h@, not previously containing it (logarithmic)
insert :: Ord a => SkewHeap a -> a -> SkewHeap a
insert h x = meld (singleton x) h


-- |Inserts all elements of the specified list (\(O(n log n)\))
insertAll :: Ord a => SkewHeap a -> [a] -> SkewHeap a
insertAll = List.foldl' insert


-- |Return the minimum element (time complexity: \(O(1)\))
findMin :: SkewHeap a -> Maybe a
findMin Empty        = Nothing
findMin (Node x _ _) = Just x


-- |
findMinE :: SkewHeap a -> a
findMinE Empty        = error "Euler.Data.SkewHeap.findMinE: empty heap"
findMinE (Node x _ _) = x


-- |Delete the minimum element and return it (time complexity: \(O(log n)\))
deleteMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
deleteMin Empty        = Nothing
deleteMin (Node x l r) = Just (x, meld l r)


-- |
deleteMinE :: Ord a => SkewHeap a -> (a, SkewHeap a)
deleteMinE Empty        = error "Euler.Data.SkewHeap.deleteMinE: empty heap" 
deleteMinE (Node x l r) = (x, meld l r)


-- |Implements the shortcut version of the top-down skew heap union operation
-- (logarithmic; for details, refer to [1 p58])
meld :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
meld Empty h2    = h2
meld h1    Empty = h1
meld h1@(Node x1 l1 r1) h2@(Node x2 l2 r2)
    | x1 <= x2  = Node x1 (meld r1 h2) l1
    | otherwise = Node x2 (meld r2 h1) l2


-- |Return a heap whose elements are the elements of @xs@ (linear)
fromList :: Ord a => [a] -> SkewHeap a
fromList xs = heapify $ map singleton xs


-- |Return the list of all items in @h@ less than or equal to @x0@ [1]
-- 
-- TODO Allow specifying the traversal method or provide a dedicated sequentialisation function
-- 
findAll :: Ord a => SkewHeap a -> a -> [a]
findAll h x0 = findAll' h []
  where
    findAll' Empty        acc = acc
    findAll' (Node x l r) acc
        | x <= x0   = x : findAll' l (findAll' r acc)
        | otherwise = findAll' l (findAll' r acc)    -- Continue


-- |Assuming that each item in heap @h@ is marked as \"good\" or \"bad\",
-- delete enough bad items so that the minimum item left in @h@ is good [1]
purge
        :: Ord a
        => (a -> Bool)    -- ^True if the passed element is /bad/
        -> SkewHeap a     -- ^The skew heap to be purged
        -> SkewHeap a
purge p h = heapify $ select h []
  where
    select Empty             acc = acc
    select root@(Node x l r) acc
        | p x       = select l (select r acc)    -- Continue
        | otherwise = root : acc


-- |Return a heap constructed by repeated
-- pairwise melding of the heaps contained in the passed list [1,2]
heapify :: Ord a => [SkewHeap a] -> SkewHeap a
heapify []    = Empty
heapify heaps = withQueue (Queue.fromList heaps)
  where
    withQueue q
        | Queue.isSingleton q = fst $ Queue.removeE q
        | otherwise =
                  let
                      (h1, q')  = Queue.removeE q
                      (h2, q'') = Queue.removeE q'
                  in
                      withQueue (Queue.insert q'' (meld h1 h2))


-- |Total number of elements in the passed heap
-- 
-- NOTE Was: @length . toList@
-- 
size :: SkewHeap a -> Int
size h = size' h 0
  where
    size' Empty        acc = acc
    size' (Node x l r) acc = size' l (size' r acc)  +  1


-- |Unfolds the argument skew heap into a list
toList :: Ord a => SkewHeap a -> [a]
toList = List.unfoldr deleteMin


-- |Like 'show', but, when printed, the produced string is
-- shown as an actual tree in the standard sidebar folder tree-like format
-- 
-- TODO Don't require the element type to implement 'Show'
-- (see 'Data.Map.Internal.Debug.showTree')
-- 
showTree :: Show a => SkewHeap a -> String
showTree h = unlines (lines h)
  where
    lines Empty        = ["Empty"]    -- ListUtils.singleton "Empty"?
    lines (Node x l r) = show x : children l r
      where
        children h1 h2 =
                withPadding "├── " "|   " (lines h1)
                        ++ withPadding "└── " "    " (lines h2)


-- |
-- 
-- TODO Rename
-- 
withPadding :: String -> String -> [String] -> [String]
withPadding leading other = zipWith (++) (leading : repeat other)

