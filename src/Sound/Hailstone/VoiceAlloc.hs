{-# LANGUAGE BangPatterns #-}
module Sound.Hailstone.VoiceAlloc
(voiceAlloc)
where

import Data.List (sortOn, groupBy)

-- | Binary min-heap with height information, using @v@ as the key for items of type @a@.
-- This is Okasaki's leftist min heap implementation.
data MinBinHeap v a = Bin !Int !v !a !(MinBinHeap v a) !(MinBinHeap v a) | Empt
  deriving (Show)

height :: MinBinHeap v a -> Int
height Empt = 0
height (Bin h _ _ _ _) = h

makeT :: v -> a -> MinBinHeap v a -> MinBinHeap v a -> MinBinHeap v a
makeT v a l r = let hl = height l; hr = height r in
  if hl >= hr then Bin (hr + 1) v a l r else Bin (hl + 1) v a r l

merge :: Ord v => MinBinHeap v a -> MinBinHeap v a -> MinBinHeap v a
merge h Empt = h
merge Empt h = h
merge heap1@(Bin _ v1 a1 l1 r1) heap2@(Bin _ v2 a2 l2 r2) =
  if v1 <= v2 then makeT v1 a1 l1 (merge r1 heap2) else makeT v2 a2 l2 (merge heap1 r2)

insert :: Ord v => v -> a -> MinBinHeap v a -> MinBinHeap v a
insert v a = merge (Bin 1 v a Empt Empt)

deleteMin :: Ord v => MinBinHeap v a -> MinBinHeap v a
deleteMin Empt = Empt
deleteMin (Bin _ _ _ l r) = merge l r

-- | If the heap is not empty, get the minimum value and its associated entry.
peekMin :: MinBinHeap v a -> Maybe (v, a)
peekMin Empt = Nothing
peekMin (Bin _ v a _ _) = Just (v, a)

-- | Sort using a min-heap, since we might as well just write this. /O(n log n)/
-- heapsort :: (Foldable f, Ord v) => f v -> [v]
-- heapsort vs = go $ foldl' (\heap v -> insert v v heap) Empt vs
--   where
--     go Empt = []
--     go h@(Bin _ v _ _ _) = v : (go $ deleteMin h)

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
