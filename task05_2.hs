variant :: Int
variant = 20

data Action = Sing | Speak | Trade deriving (Show)

readAction :: String -> Action
readAction "sing" = Sing
readAction "speak" = Speak
readAction "trade" = Trade

data Character = Character { name :: String,
                             ocean :: String,
                             voice :: Int 
                           } deriving (Show)

processing :: Character -> Action -> Character
processing ch Sing = ch { voice = voice ch + 5 }
processing ch Speak = ch { voice = voice ch - 2 }
processing ch Trade = ch { voice = voice ch - 3 }

simulation :: Character -> [Action] -> Character
simulation = foldl processing

runSimulation :: IO ()
runSimulation = do
    name <- getLine
    ocean <- getLine
    voice <- getLine
    actions <- getLine
    print (simulation (Character name ocean (read voice)) (map readAction (words actions)))