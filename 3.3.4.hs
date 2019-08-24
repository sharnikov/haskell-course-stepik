module Zzz where

{-# LANGUAGE NoMonomorphismRestriction #-}

coins :: Num a => [a]
coins = [1, 2, 3, 5, 7]

dCoins :: Num a => [[a]]
dCoins = [[1,2], [3,4], [5,6]]

change :: (Ord a, Num a) => a -> [[a]]
change money = [list | list <- (ch money), (sum list) == money]

ch :: (Ord a, Num a) => a -> [[a]]
ch amount | amount <= 0 = [[]]
ch amount = concat [map (goodCoin :) (ch (amount - goodCoin)) | goodCoin <- [coin | coin <- coins, amount >= coin]]
