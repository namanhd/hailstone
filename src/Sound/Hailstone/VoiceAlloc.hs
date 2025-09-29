{-# LANGUAGE BangPatterns #-}
-- | An excuse to implement a priority queue / min heap
module Sound.Hailstone.VoiceAlloc
(voiceAlloc)
where

import Data.List (sortOn, groupBy)

-- | Binary min-heap with height information, using @v@ as the key for items of type @a@.
data MinBinHeap v a = Bin !Int !v !a !(MinBinHeap v a) !(MinBinHeap v a) | Empt
  deriving (Show)

-- | Gets the height, where `Empt` has height 0 and a singleton tree has height 1. /O(1)/
height :: MinBinHeap v a -> Int
height Empt = 0
height (Bin h _ _ _ _) = h

-- | Inserts an entry with a priority value into the heap. /O(log n)/ worst case, but /O(1)/
-- on average. (This does insertion into the last level and the percolate-up all in one go.)
insert :: Ord v => v -> a -> MinBinHeap v a -> MinBinHeap v a
insert value entry heap = case heap of
  Empt -> Bin 1 value entry Empt Empt
  Bin h v a l r -> case compare (height l) (height r) of
    GT -> let inductive@(Bin h' v' a' l' r') = insert value entry r
      in if v' >= v then Bin h v a l inductive else Bin h v' a' l (Bin h' v a l' r')
    leq -> let inductive@(Bin h' v' a' l' r') = insert value entry l; bump = case leq of EQ -> succ h; _ -> h
      in if v' >= v then Bin bump v a inductive r else Bin bump v' a' (Bin h' v a l' r') r

grabLast :: MinBinHeap v a -> (MinBinHeap v a, MinBinHeap v a)
grabLast heap = case heap of
  Empt -> (Empt, Empt)
  Bin _ _ _ Empt Empt -> (heap, Empt)
  Bin h v a l Empt -> (l, Bin (pred h) v a Empt Empt)
  Bin h v a Empt r -> (r, Bin (pred h) v a Empt Empt)
  Bin _ v a l r -> let hl = height l; hr = height r in if hl > hr
    then let (grabbed, new_l) = grabLast l in (grabbed, Bin (succ $ max (height new_l) hr) v a new_l r)
    else let (grabbed, new_r) = grabLast r in (grabbed, Bin (succ $ max hl (height new_r)) v a l new_r)

percolateDown :: Ord v => MinBinHeap v a -> MinBinHeap v a
percolateDown heap = case heap of
  Empt -> Empt
  Bin _ _ _ Empt Empt -> heap
  Bin h v a (Bin h' v' a' l' r') Empt -> if v <= v' then heap else Bin h v' a' (percolateDown (Bin h' v a l' r')) Empt
  Bin h v a Empt (Bin h' v' a' l' r') -> if v <= v' then heap else Bin h v' a' Empt (percolateDown (Bin h' v a l' r'))
  Bin h v a l@(Bin h1 v1 a1 l1 r1) r@(Bin h2 v2 a2 l2 r2) -> let
    pickLeft = v1 < v2
    v' = if pickLeft then v1 else v2
    in  if v <= v'
        then heap
        else  if pickLeft
              then Bin h v1 a1 (percolateDown $ Bin h1 v a l1 r1) r
              else Bin h v2 a2 l (percolateDown $ Bin h2 v a l2 r2)

-- | This does not return the head/min, it just deletes it, restores the heap property by
-- percolating down, and returns the resulting heap. /O(log n)/.
deleteMin :: Ord v => MinBinHeap v a -> MinBinHeap v a
deleteMin heap = let
  -- pull the "last" item up to replace the head, and then percolate down
  (grabbed, after) = grabLast heap
  in case grabbed of
    Empt -> Empt
    Bin _ v' a' _ _ -> case after of
      Empt -> Empt
      Bin h _ _ l r -> percolateDown $ Bin h v' a' l r

-- | If the heap is not empty, get the minimum value and its associated entry.
peekMin :: MinBinHeap v a -> Maybe (v, a)
peekMin Empt = Nothing
peekMin (Bin _ v a _ _) = Just (v, a)

-- | Sort using a min-heap, since we might as well just write this. /O(n log n)/
heapsort :: (Foldable f, Ord v) => f v -> [v]
heapsort vs = go $ foldl' (\heap v -> insert v v heap) Empt vs
  where
    go Empt = []
    go h@(Bin _ v _ _ _) = v : (go $ deleteMin h)

-- | Given items on a timeline, each @(a, start, duration)@, separate them into sublists
-- where each sublist has only non-overlapping items on the timeline. /O(n log n)/, though I
-- ideally shouldn't have to sort a second time just to work with `groupBy`...
voiceAlloc :: (Num t, Ord t) => [(a, t, t)] -> [[(a, t, t)]]
voiceAlloc [] = []
voiceAlloc ls = fmap (fmap snd) . groupBy (\(i1, _) (i2, _) -> i1 == i2) . sortOn fst $ assignments
  where
    assignments = assign 0 Empt $ sortOn (\(_, start, _) -> start) ls
    assign :: (Num t, Ord t) => Int -> MinBinHeap t Int -> [(a, t, t)] -> [(Int, (a, t, t))]
    assign _ _ [] = []
    assign v heap (item@(_, !start, !duration):rest) = let
      !myEnd = start + duration
      in case peekMin heap of
        -- nothing playing, assign to the current voice and keep going
        Nothing -> (v, item) : assign v (insert myEnd v heap) rest
        Just (earliestEnd, earliestEnderV) -> if earliestEnd <= start
          -- this item starts after the earliest-ending item, can take its voice
          then (earliestEnderV, item) : assign v (insert myEnd earliestEnderV $ deleteMin heap) rest
          -- the earliest-ending voice is still occupied when this starts, make a new voice
          else let vv = v + 1 in (vv, item) : assign vv (insert myEnd vv heap) rest
