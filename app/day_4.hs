import           Control.Arrow
import qualified Data.Monoid      as Mi
import qualified Control.Monad    as M
import qualified Data.List.Split  as LS
import qualified Data.Tuple.Extra as E
import Utils

readRangePair :: String -> [Int]
readRangePair str = read <$> ranges
  where ranges = LS.splitWhen (uncurry (||) <<< ('-' ==) &&& (',' ==) ) str

contains :: [Int] -> Mi.Any
contains [al, ar, bl, br] = foldMap Mi.Any
  [ al <= bl && br <= ar
  , bl <= al && ar <= br ]

overlaps :: [Int] -> Mi.Any 
overlaps [al, ar, bl, br] = foldMap Mi.Any
  [ bl <= ar && ar <= br
  , al <= br && br <= ar
  ] <> contains [al, ar, bl, br]

main :: IO ()
main = do
  input <- ((readRangePair <$>) . lines) <$> getContents
  print <<< ((E.both sum) <<< unzip <<< (toBoth (Mi.getAny >>> fromEnum) (contains &&& overlaps) <$>)) $ input -- Part 1 and Part 2
