variant :: Int
variant = 20

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

checkFib :: Int -> Int -> Int -> Bool
checkFib a b x
    | x < 0 = False
    | a == x = True
    | a > x = False
    | otherwise = checkFib b (a + b) x
    
wrapFib :: Int -> Either String Int
wrapFib x
    | checkFib 0 1 x = Right x
    | otherwise = Left "not fibonacci"
    
recheck :: Either String Int -> Either String Int
recheck (Right x)
    | checkFib 0 1 x = Right x
    | otherwise = Left "not fibonacci"
recheck (Left s) = Left s

readNums :: Int -> IO [Int]
readNums 0 = return []
readNums n = do
    x <- readLn :: IO Int
    xs <- readNums (n - 1)
    return (x:xs)

process :: IO ()
process = do
    n <- readLn :: IO Int
    nums <- readNums n
    print (map recheck (fmap (fmap fib) (map wrapFib nums)))
