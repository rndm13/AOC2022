import           Control.Arrow
import qualified Control.Monad    as M
import qualified Data.List.Split  as LS
import qualified Data.Tuple.Extra as E

readRangePair :: String -> [[Int]]
readRangePair str = (read <$>) <$> ranges
  where ranges = LS.splitWhen ('-' ==) <$> LS.splitWhen (',' ==) str

contains :: [[Int]] -> Bool
contains [[al, ar], [bl, br]] =
  (al <= bl && br <= ar) || (bl <= al && ar <= br)

overlaps :: [[Int]] -> Bool
overlaps [[al, ar], [bl, br]]
  | bl <= ar && ar <= br = True
  | al <= br && br <= ar = True
  | otherwise = contains [[al, ar], [bl, br]]

count :: (a -> Bool) -> [a] -> Int
count f arr = foldr (\x s -> if (f x) then (s + 1) else s) 0 arr

toBoth :: Arrow a => a c d -> a b (c, c) -> a b (d, d)
toBoth toAdd orig = orig >>> (toAdd *** toAdd)

main :: IO ()
main = do
  input <- (readRangePair <$>) <$> lines <$> getContents
  print . ((E.both sum) . unzip . (toBoth fromEnum (contains &&& overlaps) <$>)) $ input -- Part 1 and Part 2
