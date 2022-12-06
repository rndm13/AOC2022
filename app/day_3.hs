import qualified Data.Char       as C
import qualified Data.Maybe      as M
import qualified Data.List.Extra as LE

getPriority :: Char -> Int
getPriority c = if c <= 'Z' then ic - 38 else ic - 96
  where ic = C.ord c

getCompartments :: String -> (String, String)
getCompartments str = splitAt (length str `div` 2) str

intersection :: Ord a => [a] -> [a] -> [a]
intersection arr1 = filter (`elem` arr1) 

solveP1 :: [String] -> [Int]
solveP1 = map (getPriorityFromFirst . uncurry (intersection) . getCompartments)

getPriorityFromFirst :: String -> Int
getPriorityFromFirst = maybe 0 (getPriority) . M.listToMaybe

solveP2 :: [String] -> [Int]
solveP2 = (map (getPriorityFromFirst . matches)) . (filter ((3==). length)) . LE.chunksOf 3
  where matches = \(a:arr) -> foldr intersection a arr

main :: IO ()
main = do
  input <- (lines <$> getContents)
  print . sum . solveP1 $ input
  print . sum . solveP2 $ input
