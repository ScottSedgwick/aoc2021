module Utils ( getLines, rotate, timeMe, toDec, options, Options(..) ) where

import Data.Char ( digitToInt )
import Data.List ( foldl' )
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