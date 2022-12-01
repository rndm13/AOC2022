import Control.Monad
import Data.List
import Data.List.Split

getInput :: IO ([[Int]])
getInput = do
  input <- getContents
  return . map (map (read :: String -> Int)) . (splitOn [""]) . lines $ input

solveP1 :: [[Int]] -> Int
solveP1 = maximum . (map (foldr (+) 0))

solveP2 :: [[Int]] -> Int
solveP2 = foldr (+) 0 . (take 3) . reverse . sort . (map (foldr (+) 0))

main :: IO ()
main = do
  input <- getInput
  print . solveP1 $ input
  print . solveP2 $ input
