module T where

t = [1,3,5,6,4,2,9,8,2]

tr = [3,5,6,4,2,9,8]

ll = [2,2]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort x = qsort (filter (< head x) (tail x)) ++ (head x) : qsort (filter (>= head x) (tail x))  