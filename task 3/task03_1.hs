variant :: Int
variant = 20

data TestType = TestType { department :: String,
                           position :: String,
                           experience :: Int 
                         } deriving (Show, Read)

instance Eq TestType where
    a == b = department a == department b && experience a == experience b


-- answer :: String -> Bool
-- answer inp =
--     let
--         [[p11, p12, p13],[p21, p22, p23]] = map (splitOn ",") $ splitOn ";" inp
--         v1 = TestType p11 p12 (read p13 :: Int)
--         v2 = TestType p21 p22 (read p23 :: Int)
--     in v1 == v2