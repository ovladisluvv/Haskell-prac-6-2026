variant :: Int
variant = 20

data Color = Red | Green | Blue deriving (Show, Read, Eq)

data Edge a = Edge String a deriving (Show, Read, Eq)

data EdgeArray a = Null | Spectrum a [Edge a] deriving (Show, Read, Eq)

instance Functor Edge where
    fmap f (Edge n x) = Edge n (f x)

instance Foldable Edge where
    foldMap f (Edge _ x) = f x

instance Functor EdgeArray where
    fmap _ Null = Null
    fmap f (Spectrum a xs) = Spectrum (f a) (fmap (fmap f) xs)

instance Foldable EdgeArray where
    foldMap _ Null = mempty
    foldMap f (Spectrum a xs) = f a <> foldMap (foldMap f) xs

process :: IO ()
process = do
    colors <- readLn :: IO (EdgeArray Color)
    putStrLn $ foldMap ((++ " | ") . (\x -> "Цвет #" ++ show x)) colors
