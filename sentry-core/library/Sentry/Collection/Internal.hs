-- | Shared evaluation policy for collection builders.
--
-- /This module's API is unstable!/
module Sentry.Collection.Internal (mapWHNF) where

-- | Map over a finite list, forcing every result to WHNF before returning.
--
-- The @foldr seq ys ys@ idiom evaluates the result spine and its elements,
-- then returns that same list.
--
-- Fields inside each element remain lazy.
mapWHNF :: (a -> b) -> [a] -> [b]
mapWHNF f xs =
  let ys = map f xs
   in foldr seq ys ys
