variant :: Int
variant = 20

data VehicleType = Car | Motorcycle | Truck deriving (Show, Read)

data Vehicle = Vehicle { make :: String,
                         model :: String,
                         vehicleType  :: VehicleType
                       } deriving (Show)

data Dealership = Dealership { dealershipName :: String,
                               vehicles :: [Vehicle]
                             } deriving (Show)
                               
runSimulation :: IO ()
runSimulation = do
    name <- getLine
    brand1 <- getLine
    model1 <- getLine
    type1 <- getLine
    brand2 <- getLine
    model2 <- getLine
    type2 <- getLine
    print (Dealership name [Vehicle brand1 model1 (read type1), Vehicle brand2 model2 (read type2)])