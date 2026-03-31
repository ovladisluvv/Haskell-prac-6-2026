variant :: Int
variant = 20

data FilmRating = G | PG | PG13 | R deriving (Show, Read, Eq, Enum)

data Film = Film { title :: String,
                   director :: String,
                   revenue :: Int 
                 } deriving (Show, Read, Eq)

data Cinema = Cinema { cinemaName :: String,
                       films :: [Film]
                    } deriving (Show, Read, Eq)

data CinemaStats = CinemaStats { countCinemas :: Int,
                                 countFilms :: Int,
                                 averageRevenue :: Double
                               } deriving (Show, Read, Eq)

instance Semigroup CinemaStats where
    CinemaStats c1 f1 r1 <> CinemaStats c2 f2 r2 = CinemaStats (c1 + c2) totalFilms totalAvgRevenue
        where 
            totalFilms = f1 + f2

            totalAvgRevenue 
                | totalFilms == 0 = 0
                | otherwise = (r1 * fromIntegral f1 + r2 * fromIntegral f2) / fromIntegral totalFilms

instance Monoid CinemaStats where
    mempty = CinemaStats 0 0 0

filmToStats :: Film -> CinemaStats
filmToStats (Film _ _ revenue) = CinemaStats 0 1 (fromIntegral revenue)

cinemaToStats :: Cinema -> CinemaStats
cinemaToStats (Cinema _ films) = CinemaStats 1 0 0 <> foldMap filmToStats films

analyzeCinemas :: [Cinema] -> CinemaStats
analyzeCinemas = foldMap cinemaToStats

statsToTuple :: CinemaStats -> (Int, Int, Double)
statsToTuple (CinemaStats c f r) = (c, f, r)

process :: IO ()
process = do
    cinemasData <- readLn
    print (statsToTuple (analyzeCinemas cinemasData))
