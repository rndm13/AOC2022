import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Extra as LE
import qualified Data.Maybe      as M

getPriority :: Char -> Int
getPriority c
  | c <= 'Z' = (C.ord c) - (C.ord 'A') + 27
  | c >= 'a' = (C.ord c) - (C.ord 'a') + 1

getComparments :: String -> (String, String)
getComparments str = splitAt ((length str) `div` 2) str

findMatching :: Ord a => ([a], [a]) -> [a]
findMatching (arr1, arr2) = filter (\v -> M.isJust . L.find (==v) $ arr1) arr2

solveP1 :: [String] -> [Int]
solveP1 = map (priorityFirst . matches)
  where
    matches = findMatching . getComparments

priorityFirst :: String -> Int
priorityFirst v =
  if null v
    then 0
    else (getPriority . head $ v)

solveP2 :: [String] -> [Int]
solveP2 = (map (priorityFirst . matches)) . (filter ((3==). length)) . LE.chunksOf 3
  where
    matches = \[a,b,c] -> findMatching (a, findMatching (b,c))

main :: IO ()
main = do
  input <- getContents
  print . sum . solveP1 . lines $ input
  print . sum . solveP2 . lines $ input
