import Data.List

main :: IO()
main = do
  contents <- readFile "input.txt"
  let locationIDs = map readInt . words $ contents
  let column1 = sort (getEvenEntries locationIDs)
  let column2 = sort (getOddEntries locationIDs)
  let diffs = map abs (zipWith (-) column1 column2)
  print (sum diffs)

readInt :: String -> Int
readInt = read

getOddEntries :: [Int] -> [Int]
getOddEntries = map snd . filter (odd . fst) . zip [0..]

getEvenEntries :: [Int] -> [Int]
getEvenEntries = map snd . filter (even . fst) . zip [0..]
