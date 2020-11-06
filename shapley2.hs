import Text.CSV (parseCSV, Record)
import qualified Data.Set as S
import Data.List.Split
import Data.List
import qualified Data.Map

data Path = Path { chain :: [String]
                 , transfer :: Maybe Float
                 , card :: Maybe Float
                 } deriving Show

parseItem :: Record -> Path
parseItem record =
  Path { chain    = unqCH $ splitChain chainStr
       , transfer = case reads(transferStr) :: [(Float,String)] of
         [(c, "")] -> Just c
         _ -> Nothing

       , card = Just(fromIntegral (length . unqCH $ splitChain chainStr) :: Float)
       }
  where chainStr    = record !! 0
        transferStr = record !! 1

parseToTuple :: [String] -> Path
parseToTuple record = item
  where item = parseItem record 

main :: IO ()
main = do
  let fileName = "test_shp.csv"
  input <-  readFile fileName
  let csv =  parseCSV fileName input
  either handleError doWork csv

handleError = print

doWork :: [Record] -> IO ()
doWork csv =  print $
              Data.Map.fromListWith (+) $ take 9 $ concat $ map calcWeight $ filterPath $
              map parseToTuple $ tail $  filter (not . null) csv

splitChain :: String -> [String]
splitChain "" = []
splitChain str = splitOn ">" str


toll :: [String] -> [String]
toll = take 150 

unqCH :: [String] -> [String]
unqCH = nub . toll

calcWeight :: Path -> [(String,Float)]
calcWeight elem = map (\x -> (x , safeDiv transfers cardinality)) $ chain elem
  where transfers = transfer elem
        cardinality = card elem

safeDiv :: Maybe Float -> Maybe Float -> Float
safeDiv _ Nothing = 0.0
safeDiv Nothing _ = 0.0
safeDiv (Just a) (Just b) = a / b


filterPath :: [Path] -> [Path]
filterPath x = filter (not . null . chain) $ filter ((/= Nothing) .transfer) x
