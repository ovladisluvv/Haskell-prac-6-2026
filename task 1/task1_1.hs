variant :: Int
variant = 2

gradeScore :: Int -> String
gradeScore x
  | x >= 90 = "A"
  | x >= 80 = "B"
  | x >= 70 = "C"
  | x >= 60 = "D"
  | otherwise = "E"
  
answer :: [Int] -> [String]
answer xs = map gradeScore xs
