import Data.List (nub, sort, take)

variant :: Int
variant = 2

answer :: [Int] -> [Int]
answer xs = take 3 (sort (nub xs))