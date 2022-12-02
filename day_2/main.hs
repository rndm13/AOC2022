import Data.List.Extra
import Data.List
import Data.Maybe
import Data.Char

data Hand = Rock
          | Paper
          | Scissors
          deriving (Show, Eq)

getHand :: Char -> Hand
getHand c
  | c <= 'C'  = hands !! ((ord c) - (ord 'A'))
  | otherwise = hands !! ((ord c) - (ord 'X'))

hands = Rock:Paper:Scissors:hands

pairs :: [a] -> [(a,a)]
pairs = (map ((,) <$> head <*> last)) . (chunksOf 2)

getInput :: String -> [(Char, Char)]
getInput = pairs . (map head) . words

getHandScore :: Hand -> Int
getHandScore = fromJust . (flip elemIndex hands)

getScoreP2 :: (Char, Char) -> Int
getScoreP2 (a, cond)
  | cond == 'X' = ((score + 2) `mod` 3 + 1)
  | cond == 'Y' = (score + 4)
  | cond == 'Z' = ((score + 1) `mod` 3 + 7)
  where
    score = getHandScore . getHand $ a

getResult :: Hand -> Hand -> Int
getResult Rock Paper = 6 
getResult Paper Scissors = 6 
getResult Scissors Rock = 6 
getResult a b
  | a == b = 3
  | otherwise = 0

getScoreP1 :: (Char, Char) -> Int
getScoreP1 (a, b) = (getResult ha hb) + (getHandScore hb) + 1
  where
    ha = getHand a
    hb = getHand b

main :: IO ()
main = do
   input <- getContents
   print . sum . (map getScoreP1) . getInput $ input 
   print . sum . (map getScoreP2) . getInput $ input 
