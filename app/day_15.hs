{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

import           Control.Arrow
import           Data.Char     (isDigit)
import           Data.Maybe    (fromJust, isNothing)
import qualified Data.Set      as S
import           Utils

data Sensor = Sensor
  { _xSensor :: Int
  , _ySensor :: Int
  , _xBeacon :: Int
  , _yBeacon :: Int
  } deriving stock Show

getNumbers :: String -> [Int]
getNumbers ""  = []
getNumbers str = (read num):(getNumbers rest)
  where (num, rest) = span digitOrMinus . dropWhile (not . digitOrMinus) $ str
        digitOrMinus c = isDigit c || c == '-'

rangeAtY :: Int -> Sensor -> S.Set Int
rangeAtY y s@(Sensor xs ys _ _)
  | disp < 0  = S.empty
  | otherwise = S.fromList [xs-disp..xs+disp]
  where dist = sensorDist s
        disp = dist - abs (y - ys)

--          x1     y1     x2     y2     dist
manhDist :: Int -> Int -> Int -> Int -> Int
manhDist x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

sensorDist :: Sensor -> Int
sensorDist (Sensor xs ys xb yb) = manhDist xs ys xb yb

yP1 :: Int
yP1 = 2000000

solveP1 :: [Sensor] -> Int
solveP1 input = S.size .  S.difference emptyCov $ beacons
    where beacons = (S.fromList . map (_xBeacon) . filter ((yP1 ==) . _yBeacon) $ input)
          emptyCov = S.unions . map (rangeAtY yP1) $ input

bounds :: Int
bounds = 4000001 

isAffected :: (Int, Int) -> Sensor -> Bool
isAffected (x, y) sens = manhDist x y (_xSensor sens) (_ySensor sens) <= sensorDist sens

moveNextOpening :: [Sensor] -> (Int, Int) -> Maybe (Int, Int)
moveNextOpening sensors (x, y)
  | isNothing disp = Nothing
  | newX == x      = Nothing
  | otherwise      = Just (if newX >= bounds then 0 else newX, y + (newX `div` bounds))
  where disp = getDisp sensors (x,y) 
        newX = (snd . fromJust $ disp) + (_xSensor . fst . fromJust $ disp) + 1

getDisp :: [Sensor] -> (Int, Int) -> Maybe (Sensor, Int)
getDisp sensors (x, y) = 
  safeHead .
  zip affecting .
  map (\sens -> (sensorDist sens) - abs (y - (_ySensor sens))) $
  affecting
    where affecting = filter (isAffected (x, y)) sensors

tunFreq :: (Int, Int) -> Int
tunFreq (x,y) = x * 4000000 + y

solveP2 :: [Sensor] -> Int
solveP2 sensors =
  tunFreq .
  fromJust .
  iterateM' (moveNextOpening sensors) $ (0,0)

main :: IO ()
main = do
  input <- fmap ($(makeListBinder 4) Sensor . getNumbers) . lines <$> getContents
  print . (solveP1 &&& solveP2) $ input
