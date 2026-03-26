variant :: Int
variant = 20

data TestType = Fire String Int Int
              | Ice  String Int Int
              | Air  String Int Int
                 deriving (Show, Read)

class PrettyPrint a where
    toString :: a -> String 

instance PrettyPrint TestType where
    toString (Fire n p r) = "Магия огня: " ++ n ++ ", мощь " ++ show p ++ ", охват " ++ show r ++ " м"
    toString (Ice n p f) = "Магия льда: " ++ n ++ ", мощь " ++ show p ++ ", морозит " ++ show f ++ " сек"
    toString (Air n p d) = "Магия воздуха: " ++ n ++ ", мощь " ++ show p ++ ", дистанция " ++ show d ++ " м"

answer :: TestType -> String
answer inp = toString inp