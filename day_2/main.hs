import Data.List.Extra
import Data.List
import Data.Maybe

data Hand = Rock
          | Paper
          | Scissors
          deriving (Show, Eq)

hands = [Rock, Paper, Scissors]

getHand :: Char -> Hand
getHand 'A' = Rock
getHand 'B' = Paper
getHand 'C' = Scissors
getHand 'X' = Rock
getHand 'Y' = Paper
getHand 'Z' = Scissors

pairs :: [a] -> [(a,a)]
pairs = (map ((,) <$> head <*> last)) . (chunksOf 2)

getInputP1 :: String -> [(Hand, Hand)]
getInputP1 = pairs . (map (getHand . head)) . words

getFromCondition :: (Char, Char) -> (Hand, Hand)
getFromCondition (a, cond)
  | cond == 'X' = (h, (cycle hands) !! ((fromJust $ elemIndex h hands) + 2))
  | cond == 'Y' = (h, h)
  | cond == 'Z' = (h, (cycle hands) !! ((fromJust $ elemIndex h hands) + 1))
  where
    h = getHand a

getInputP2 :: String -> [(Hand, Hand)]
getInputP2 = (map getFromCondition) . pairs . (map head) . words

getResult :: (Hand, Hand) -> Int
getResult (a, b) 
  | diff == 0 = 3
  | diff == 1 || diff == -2 = 6
  | otherwise = 0
  where 
    aind = (fromJust $ elemIndex a hands)
    bind = (fromJust $ elemIndex b hands)
    diff = bind - aind

getScore :: (Hand, Hand) -> Int
getScore (a, b) = (getResult (a, b)) + (fromJust $ elemIndex b hands) + 1

solve :: [(Hand, Hand)] -> Int
solve = foldr (+) 0 . (map getScore) 

main :: IO ()
main = do
   input <- getContents
   print . solve . getInputP1 $ input 
   print . solve . getInputP2 $ input 
