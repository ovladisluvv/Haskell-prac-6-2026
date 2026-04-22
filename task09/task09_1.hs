variant :: Int
variant = 20

data Tree = Empty | Node WordInfo Tree Tree Tree deriving (Show)

data VowelStatus = HasVowels | NoVowels deriving (Show, Read, Eq)

data WordInfo = WordInfo { word :: String,
                           vowelStatus :: VowelStatus 
                         } deriving (Show)

toWordInfoList :: [(String, VowelStatus)] -> [WordInfo]
toWordInfoList [] = []
toWordInfoList ((w, vs):xs) = WordInfo w vs : toWordInfoList xs

buildTree :: [WordInfo] -> Tree
buildTree [] = Empty
buildTree xs = insertTreeNode xs 0

insertTreeNode :: [WordInfo] -> Int -> Tree
insertTreeNode xs ind
    | ind >= length xs = Empty
    | otherwise = Node (xs !! ind) (insertTreeNode xs (3 * ind + 1)) (insertTreeNode xs (3 * ind + 2)) (insertTreeNode xs (3 * ind + 3))

treeAnalyzer :: [WordInfo] -> Int
treeAnalyzer xs = countHasVowels (buildTree xs)

countHasVowels :: Tree -> Int
countHasVowels Empty = 0
countHasVowels (Node info l m r) = curCount + countHasVowels l + countHasVowels m + countHasVowels r
    where
        curCount = case vowelStatus info of
            HasVowels -> 1
            NoVowels -> 0

process :: IO ()
process = do
    input <- readLn :: IO [(String, VowelStatus)]
    print (treeAnalyzer $ toWordInfoList input)
