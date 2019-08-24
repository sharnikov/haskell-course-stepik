module Zzzzz where

import Data.List

revRange :: (Char,Char) -> [Char]
revRange (first, last) = unfoldr g last
  where g = (\x -> if x >= first then Just (x, pred x) else Nothing)