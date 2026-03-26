import System.Random (randomR, StdGen, mkStdGen)

variant :: Int
variant = 20

newtype File = File (String, [Bool])

class PrettyPrint a where
    prettyPrint :: a -> String

instance PrettyPrint File where
    prettyPrint (File (name, perms)) = name ++ " : " ++ show perms ++ "\n"
    
makeFile :: [String] -> Int -> [Bool] -> File
makeFile names ids perms = File (head (drop ids names), perms)

generateFiles :: Int -> [String] -> StdGen -> ([File], StdGen)
generateFiles 0 _ gen = ([], gen)
generateFiles n names gen = (makeFile names ids [b1, b2, b3] : rest, gen5)
    where
        (ids, gen1) = randomR (0, length names - 1) gen
        (b1, gen2) = randomR (False, True) gen1
        (b2, gen3) = randomR (False, True) gen2
        (b3, gen4) = randomR (False, True) gen3
        (rest, gen5) = generateFiles (n - 1) names gen4
    
buildFiles :: [String] -> [File]
buildFiles [] = []
buildFiles names = fst (generateFiles 5 names (mkStdGen 38))
    
process :: IO ()
process = do
    names <- readLn :: IO [String]
    putStrLn (foldr (++) "" (map prettyPrint (buildFiles names)))
