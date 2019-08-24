module Zyyy where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Maybe

l = [(1, "a"), (2, "b"), (3, "c")]

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

insertL :: Ord k => k -> v -> [(k,v)] -> [(k,v)]
insertL k v [] = [(k, v)]
insertL k v lm = if ((fst $ head lm) == k) then (k, v) : (tail lm) else (head lm) : (insertL k v (tail lm))

deleteL :: Ord k => k -> [(k,v)] -> [(k,v)]  
deleteL k [] = []
deleteL k lm = if ((fst $ head lm) == k) then tail lm else (head lm) : (deleteL k (tail lm))

mm :: Maybe (k,v) -> Maybe v
mm m = if isNothing m then Nothing else Just (snd (fromJust m))

instance MapLike ListMap where
    empty = ListMap []
    lookup e (ListMap lm) = mm $ L.find (\(f, s) -> f == e) lm 
    insert k v (ListMap lm) = ListMap( insertL k v lm)
    delete k (ListMap lm) = ListMap ( deleteL k lm)
    fromList x = ListMap x