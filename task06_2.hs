variant :: Int
variant = 20

data BST a = Empty | Node a (BST a) (BST a) 
    deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node val left right)
    | x < val = Node val (insert x left) right
    | x > val = Node val left (insert x right)
    | otherwise = Node val left right

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Node val left right)
    | x < val = search x left
    | x > val = search x right
    | otherwise = True

fromList :: Ord a => [a] -> BST a
fromList = foldl (\acc x -> insert x acc) Empty

run :: IO ()
run = do
    list1 <- readLn :: IO [Bool]
    x1 <- readLn :: IO Bool
    print (search x1 (fromList list1))

    list2 <- readLn :: IO [Char]
    x2 <- readLn :: IO Char
    print (search x2 (fromList list2))

    list3 <- readLn :: IO [Double]
    x3 <- readLn :: IO Double
    print (search x3 (fromList list3))

    list4 <- readLn :: IO [Int]
    x4 <- readLn :: IO Int
    print (search x4 (fromList list4))
