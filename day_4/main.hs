import           Control.Arrow
import qualified Data.Monoid      as Mi
import qualified Control.Monad    as M
import qualified Data.List.Split  as LS
import qualified Data.Tuple.Extra as E

readRangePair :: String -> [[Int]]
readRangePair str = (read <$>) <$> ranges
  where ranges = LS.splitWhen ('-' ==) <$> LS.splitWhen (',' ==) str

contains :: [[Int]] -> Mi.Any
contains [[al, ar], [bl, br]] = foldMap Mi.Any
  [ al <= bl && br <= ar
  , bl <= al && ar <= br ]

overlaps :: [[Int]] -> Mi.Any 
overlaps [[al, ar], [bl, br]] = foldMap Mi.Any
  [ bl <= ar && ar <= br
  , al <= br && br <= ar
  ] <> contains [[al, ar], [bl, br]]

count :: (a -> Bool) -> [a] -> Int
count f arr = foldr (\x s -> if (f x) then (s + 1) else s) 0 arr

toBoth :: Arrow a => a c d -> a b (c, c) -> a b (d, d)
toBoth toAdd orig = orig >>> (toAdd *** toAdd)

main :: IO ()
main = do
  input <- (readRangePair <$>) <$> lines <$> getContents
  print . ((E.both sum) . unzip . (toBoth (fromEnum . Mi.getAny) (contains &&& overlaps) <$>)) $ input -- Part 1 and Part 2
