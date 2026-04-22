variant :: Int
variant = 20

data MusicGenre = Rock | Pop | Jazz | Classical | HipHop deriving (Show, Read, Eq)

newtype MyList a = MyList [a] deriving (Show, Read, Eq)

data MyPlaylist a = Empty | Page String Int (MyList a) (MyPlaylist a) deriving (Show, Read, Eq)

instance Functor MyList where
    fmap f (MyList xs) = MyList (map f xs)

instance Foldable MyList where
    foldMap f (MyList xs) = foldMap f xs

instance Functor MyPlaylist where
    fmap f Empty = Empty
    fmap f (Page n y xs next) = Page n y (fmap f xs) (fmap f next)

instance Foldable MyPlaylist where
    foldMap f Empty = mempty
    foldMap f (Page _ _ xs next) = foldMap f xs <> foldMap f next

process :: IO ()
process = do
    playlist <- readLn :: IO (MyPlaylist MusicGenre)
    putStrLn $ foldMap (( ++ " | ") . (\x -> "Genre: " ++ show x ++ ".")) playlist
