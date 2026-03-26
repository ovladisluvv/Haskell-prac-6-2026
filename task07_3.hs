variant :: Int
variant = 20

data Parity = Even | Odd deriving (Eq, Show)

data Entry = Entry { name :: String,
                     parity :: Parity,
                     value :: Int
                   } deriving (Eq, Show)
                   
data FTree = Empty | Node Entry FTree FTree deriving (Show)

isEven :: Int -> Bool
isEven x = (x `mod` 2) == 0

getParity :: Int -> Parity
getParity x
    | isEven x = Even
    | otherwise = Odd

buildFromList :: [Int] -> FTree
buildFromList [] = Empty
buildFromList (x:xs) = Node (Entry (show x) (getParity x) x) (buildFromList xs) Empty

swapParity :: Parity -> Parity
swapParity Even = Odd
swapParity Odd = Even

changeEntry :: Entry -> Entry
changeEntry (Entry n p v) = Entry n (swapParity p) (v + 1)

transform :: FTree -> FTree
transform Empty = Empty
transform (Node e l r) = Node (changeEntry e) (transform l) (transform r) 

process :: IO ()
process = do
    nums <- readLn :: IO [Int]
    print (buildFromList nums)
    putStrLn "We got:"
    print (transform (buildFromList nums))
