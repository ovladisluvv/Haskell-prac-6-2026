import Data.List (group, sort)

variant :: Int
variant = 20

data OS = Android | IOS | HarmonyOS | WindowsPhone | Tizen deriving (Eq, Ord, Show, Read) 

data Manufacturer = Manufacturer 
    { name :: String,
      country :: String
    } deriving (Eq, Ord, Show)

data Smartphone = Smartphone 
    { model :: String,
      storageSpace :: Int,
      os :: OS,
      manufacturer :: Manufacturer
    } deriving (Eq, Ord, Show)

split :: Char -> String -> [String]
split _ "" = []
split separator str = takeWhile (/= separator) str : split separator (drop 1 (dropWhile (/= separator) str))

toOS :: String -> OS
toOS "Android" = Android
toOS "iOS" = IOS
toOS "HarmonyOS" = HarmonyOS
toOS "Windows Phone" = WindowsPhone
toOS "Tizen" = Tizen

parse :: String -> [Smartphone]
parse input = map 
    ((\[m, s, o, mn, mc] -> Smartphone m (read s) (toOS o) (Manufacturer mn mc)) . map trim . split ',') (split ';' input)
    where
        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

getMaxSimilar :: [Smartphone] -> Int
getMaxSimilar [] = 0
getMaxSimilar list = maximum (map length (group (sort (map (\s -> (storageSpace s, os s, country (manufacturer s))) list))))