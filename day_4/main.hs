import qualified Control.Monad as M

-- I changed input file a little bit with magic of vim so that
-- every number appears on a new line and removed ',' and '-'
getRange :: IO (Int, Int)
getRange = (,) <$> readLn <*> readLn 

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
  input <- M.replicateM 1000 ((,) <$> getRange <*> getRange) -- I counted that input is 1000 ranges :)
  print . count (uncurry contains) $ input -- Part 1
  print . count (uncurry overlaps) $ input -- Part 2
