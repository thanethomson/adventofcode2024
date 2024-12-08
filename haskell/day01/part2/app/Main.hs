module Main where
import System.Environment
import qualified Data.Map
import qualified Data.Set

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let locationIDs = map readInt . words $ contents
  let col1 = getEvenEntries locationIDs
  let uniqCol1 = Data.Set.fromList col1
  let relevantCol2 = filter (`Data.Set.member` uniqCol1) (getOddEntries locationIDs)
  let relevantCounts = Data.Map.fromListWith (+) (zip relevantCol2 relevantCol2)
  let counts = map (\v -> (v, Data.Map.findWithDefault (0 :: Int) v relevantCounts)) col1
  let total = sum (map snd counts)
  print total

readInt :: String -> Int
readInt = read

getOddEntries :: [Int] -> [Int]
getOddEntries = map snd . filter (odd . fst) . zip [(0 :: Int)..]

getEvenEntries :: [Int] -> [Int]
getEvenEntries = map snd . filter (even . fst) . zip [(0 :: Int)..]
