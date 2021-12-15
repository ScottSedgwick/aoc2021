module Utils ( dijkstra, getLines, rotate, timeMe, toDec, options, printGrid, printPlot, Options(..) ) where

import Control.Monad ( forM_ )
import Data.Char ( digitToInt )
import Data.List ( foldl' )
import qualified Data.Set as Set
import Options.Applicative
import System.Clock ( diffTimeSpec, getTime, toNanoSecs, Clock(Monotonic) )

data Options = Options 
  { infile :: String
  , stage :: String
  } deriving stock Show

rotate :: Eq a => [[a]] -> [[a]]
rotate [] = []
rotate xs = if null ys then [] else ys : rotate zs
  where
    ys = concatMap (take 1) xs
    zs = map (drop 1) xs

timeMe :: IO() -> IO()
timeMe f = do
  t0 <- getTime Monotonic
  f 
  t1 <- getTime Monotonic
  let t = toNanoSecs $ diffTimeSpec t1 t0
  let t' = fromIntegral t / 1000000000 :: Double
  putStrLn $ "Time (secs): " <> show t'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

optParser :: Parser Options
optParser = Options
  <$> strOption (long "inputfile" <> short 'i' <> help "Input data file to use")
  <*> strOption (long "stage" <> short 's' <> help "Stage to run (1 or 2)")


options :: ParserInfo Options
options = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

getLines :: Options -> IO [String]
getLines opts = lines <$> readFile (infile opts)

printGrid :: [[Int]] -> IO()
printGrid = mapM_ printLine

printLine :: [Int] -> IO()
printLine = putStrLn . concatMap show

printPlot :: [(Int, Int)] -> IO()
printPlot ds = do
  let xs = [0..maximum (map fst ds)]
  let ys = [0..maximum (map snd ds)]
  forM_ ys $ \y -> do
    let l = map (\x -> if (x,y) `elem` ds then '#' else '.') xs
    print l

dijkstra
  :: (Ord cost , Ord node)
  => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
  -> node                               -- ^ Where we want to get to
  -> (cost , node)                      -- ^ The start position
  -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (Set.singleton start)
  where
    search visited toBeVisited = 
      case Set.minView toBeVisited of
        Nothing -> Nothing
        Just ((cost , vertex) , withoutVertex)
          | vertex == target            -> Just (cost , vertex)
          | vertex `Set.member` visited -> search visited withoutVertex
          | otherwise                   -> search visitedWithNode withNext
          where
            visitedWithNode = Set.insert vertex visited
            withNext = foldr Set.insert withoutVertex $ next (cost , vertex)