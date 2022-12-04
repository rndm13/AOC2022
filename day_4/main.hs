import qualified Control.Monad as M
import qualified Data.List.Split as LS

toTuple :: [a] -> (a,a)
toTuple = (,) <$> head <*> last

readRangePair :: String -> ((Int, Int), (Int, Int))
readRangePair str = toTuple $ toTuple <$> ((read :: String -> Int) <$>) <$> ranges
  where
    ranges = LS.splitWhen ('-' ==) <$> LS.splitWhen (',' ==) str

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (al, ar) (bl, br) =
  (al <= bl && br <= ar) || (bl <= al && ar <= br)

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (al, ar) (bl, br)
  | bl <= ar && ar <= br = True
  | al <= br && br <= ar = True
  | otherwise = contains (al, ar) (bl, br)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  input <- (readRangePair <$>) <$> lines <$> getContents 
  print . count (uncurry contains) $ input -- Part 1
  print . count (uncurry overlaps) $ input -- Part 2
