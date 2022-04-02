module Data.Stream where 

import Prelude hiding (splitAt, head, tail, zipWith, repeat)

-- | Infinite list. 
-- Definitions are selections from this module
-- https://hackage.haskell.org/package/Stream-0.4.7.2/docs/src/Data-Stream.html
data Stream a = Cons a (Stream a)
  deriving (Eq)

-- | Maps a function across all values of the stream
instance Functor Stream where
  fmap f ~(Cons x xs) = Cons (f x) (fmap f xs)

-- | This will allow us to build synthesis functions out of `<$>` and `<*>`
instance Applicative Stream where
  pure = repeat
  (<*>) ~(Cons f fs) ~(Cons x xs) = Cons (f x) (fs <*> xs)

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

-- | Get the head of the stream
head :: Stream a -> a 
head (Cons x _) = x

-- | Get the stream but without its head
tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

-- | Combine two streams into one using a pointwise function
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- | Build a stream that contains the same value repeated infinitely
repeat :: a -> Stream a
repeat x = Cons x (repeat x)

-- | Build a stream from a bog-standard Prelude infinite list. This is to
-- leverage the built-in syntax sugar for infinite lists (i.e. the 
-- @[n..]@ syntax)
fromInfList :: [a] -> Stream a
fromInfList (x:xs) = Cons x (fromInfList xs)
fromInfList [] = error "invalid to turn a finite list into a stream!"
