module Utils (hasDuplicates) where

import Data.Set (empty, insert, member, Set )

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates =  hasDuplicates' empty

hasDuplicates' :: (Ord a) => Set a -> [a] -> Bool
hasDuplicates' _ [] = False
hasDuplicates' present (x:xs)
    | member x present = True
    | otherwise = hasDuplicates' (insert x present) xs