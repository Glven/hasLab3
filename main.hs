-- import System.IO
-- import Data.Time.Clock.POSIX

-- countGroups :: [Int] -> Int
-- countGroups = go [] 0
--   where
--     go :: [Int] -> Int -> [Int] -> Int
--     go list answer [] = answer
--     go list answer (value:values) =
--       let newList = list ++ [value]
--       in if length newList == 5
--            then let maxValue = maximum newList
--                 in if newList !! 2 == maxValue
--                      then go (tail newList) (answer + 1) values
--                      else go (tail newList) answer values
--            else go newList answer values

-- main :: IO ()
-- main = do
--   start <- getPOSIXTime
--   contents <- readFile "D:\\Study\\Third Course\\FP\\statistic.txt"
--   let values = map read (lines contents) :: [Int]
--       answer = countGroups values - 1
--   end <- getPOSIXTime
--   let elapsed = round $ (end - start) * 1000 :: Integer
--   putStrLn $ "Answer is = " ++ show answer ++ ", spend = " ++ show elapsed ++ " ms."
import System.IO
import Control.Monad
import Data.List.Split (chunksOf)

countGroups :: [[Int]] -> Int
countGroups = length . filter (\group -> length group == 5 && (group !! 2) > maximum (take 2 group) && (group !! 2) > maximum (drop 3 group))

main :: IO ()
main = do
  handle <- openFile "D:/Study/Third Course/FP/statistic.txt" ReadMode
  groups <- fmap (chunksOf 5 . map read . lines) $ hGetContents handle
  let result = countGroups groups
  putStrLn $ "Number of groups with the third number greater than the others: " ++ show result
  hClose handle