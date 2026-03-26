variant :: Int
variant = 2

data Person = Person { energy :: Int, 
                       skill :: Int,
                       experienceDays :: Int,
                       favoriteIngredient :: String
                     } deriving (Show, Read)

breakfast :: Person -> Person
breakfast p = p { energy = energy p + 4 }

prepareDish :: [(String, Int)] -> Person -> Person
prepareDish recipes p = 
    case lookup (favoriteIngredient p) recipes of
        Just minEnergy | energy p > minEnergy && experienceDays p > 30 -> p { skill = skill p + 8,
                                                                              energy = energy p - 2 
                                                                            }
        _ -> p

gainExperience :: Person -> Person
gainExperience p = p { experienceDays = experienceDays p + 1 }

processPerson :: Person -> [(String, Int)] -> Person
processPerson p recipes = gainExperience (prepareDish recipes (breakfast p))

process :: [Person] -> [(String, Int)] -> [Person]
process people recipes = map (`processPerson` recipes) people

calculate :: [Person] -> [(String, Int)] -> Int
calculate people recipes = length (filter ((> 20) . energy) (process people recipes))

run :: IO ()
run = do 
  people <- readLn :: IO [Person]
  requirements <- readLn :: IO [(String, Int)]
  print (calculate people requirements)