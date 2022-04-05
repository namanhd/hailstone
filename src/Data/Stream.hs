module Data.Stream 
(
  Stream
, cons
, head, tail, headtail
, splitAt
, zipWith, zipWith3, zipWith4
, repeat, fromInfList
, interleave
)
where 

import Prelude hiding (splitAt, head, tail, zipWith, zipWith3, repeat)

-- interesting read on performance problems with trying to do realtime audio;
-- the Stream Fusion thing (not my Stream, a different Stream) might not work
-- out actually. This is a tough nut to crack in Haskell....
-- https://haskell-cafe.haskell.narkive.com/MM57Tz5H/lazy-cons-stream-fusion-style

-- | Infinite list. 
-- Definitions are selections from this module
-- https://hackage.haskell.org/package/Stream-0.4.7.2/docs/src/Data-Stream.html
data Stream a = Cons a (Stream a)
  deriving (Eq)

-- | The Cons constructor as a regular function (to make it easy to write 
-- drop-in replacement modules for Stream)
cons :: a -> Stream a -> Stream a
cons = Cons
{-# INLINE[0] cons #-}

-- | Maps a function across all values of the stream
instance Functor Stream where
  fmap = smap
  {-# INLINE[0] fmap #-}

smap :: (a -> b)  -> Stream a -> Stream b
smap f ~(Cons x xs) = Cons (f x) (smap f xs)
{-# INLINE[0] smap #-}
{-# RULES "smap/smap" forall f g s. smap f (smap g s) = smap (\x -> f (g x)) s #-}
--  this rule does actually fire

-- | This will allow us to build synthesis functions out of `<$>` and `<*>`.
-- However, try to avoid <*> because it is very slow and wastes a lot of space/
-- time allocating intermediate lists containing functions. 
-- When possible, use @zipWith*@ functions.
instance Applicative Stream where
  pure = repeat
  {-# INLINE pure #-}
  (<*>) ~(Cons f fs) ~(Cons x xs) = Cons (f x) (fs <*> xs)
  -- (<*>) = zipWith ($)
  {-# INLINE (<*>)#-}

-- TODO monad instance (follow Conduit's explanation); 
-- though Functor and Applicative are all that is needed
-- for most of the modulation functionality, so this is going to be left at this
-- most likely

-- | Extracts a list of values out of the front of the stream and also returns
-- the remainder of the stream
splitAt :: Int -> Stream a -> ([a], Stream a)
splitAt n xs
  | n == 0 = ([], xs)
  | n > 0 = let (prefix, rest) = splitAt (n - 1) (tail xs)
            in  (head xs : prefix, rest)
  | otherwise = error "splitAt negative argument"

-- | Get the head of the stream.
head :: Stream a -> a 
head ~(Cons x _) = x
{-# INLINE[0] head #-}

-- | Get the stream but without its head.
tail :: Stream a -> Stream a
tail ~(Cons _ xs) = xs
{-# INLINE[0] tail #-}

headtail :: Stream a -> (a, Stream a)
headtail ~(Cons x xs) = (x, xs)
{-# INLINE[0] headtail #-}

-- | Combine two streams into one using a pointwise function.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- | Combine 3 streams into one using a pointwise function.
zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f ~(Cons x xs) ~(Cons y ys) ~(Cons z zs) = Cons (f x y z) (zipWith3 f xs ys zs)

-- | Combine 4 streams into one using a pointwise function.
zipWith4 :: (a -> b -> c -> d -> e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
zipWith4 f ~(Cons x xs) ~(Cons y ys) ~(Cons z zs) ~(Cons w ws) = Cons (f x y z w) (zipWith4 f xs ys zs ws)

-- | Build a stream that contains the same value repeated infinitely.
repeat :: a -> Stream a
repeat x = Cons x (repeat x)
{-# INLINE[1] repeat #-}
{-# RULES "fmaprepeat" forall f x. smap f (repeat x) = repeat (f x) #-}


-- | Build a stream from a bog-standard Prelude infinite list. This is to
-- leverage the built-in syntax sugar for infinite lists (i.e. the 
-- @[n..]@ syntax)
fromInfList :: [a] -> Stream a
fromInfList (x:xs) = Cons x (fromInfList xs)
fromInfList [] = error "invalid to turn a finite list into a stream!"
{-# INLINE[1] fromInfList #-}
-- To take advantage of list fusion
{-# RULES "fmapinf" forall f s. smap f (fromInfList s) = fromInfList (fmap f s) #-}

-- | Interleave two streams.
interleave :: Stream a -> Stream a -> Stream a
interleave ~(Cons a as) ~(Cons b bs) = (Cons a (Cons b (interleave as bs)))