{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

import qualified Data.Monoid     as Mi
import qualified Data.Set        as S
import           Control.Lens
import           Control.Arrow

data Position = Position 
  { _x :: Int
  , _y :: Int 
  } deriving stock (Eq, Ord)
makeLenses ''Position

instance Semigroup Position where
  p1 <> p2 = over x (+ (_x p2)) .
             over y (+ (_y p2)) $ p1

instance Mi.Monoid Position where
  mempty = Position 0 0

data Move = Move 
  { _dir   :: Position
  , _count :: Int 
  }

instance Read Move where
  readsPrec _ i = [(Move dirP count, rest)]
      where dir   = head i
            dirP  = case dir of 
                      'R' -> Position 1    0
                      'L' -> Position (-1) 0
                      'U' -> Position 0    1  
                      'D' -> Position 0  (-1) 
            count = read . takeWhile (/=' ') . drop 2 $ i
            rest  = dropWhile (/=' ') . drop 2 $ i

moveToPositions :: Move -> [Position]
moveToPositions (Move p count) = replicate count p

doMove :: Position -> Position -> Position
doMove next cur = cur <> (getMove cur next)

getMove :: Position -> Position -> Position
getMove (Position tx ty) (Position hx hy) 
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = mempty
  | otherwise = Position moveX moveY
    where moveX = signum (hx - tx)
          moveY = signum (hy - ty)

moveKnot :: [Position] -> Position -> [Position]
moveKnot [] _ = []
moveKnot (h:t) p = scanl1 (doMove) ((h <> p):t)

solve :: Int -> [Move] -> Int
solve n = 
  S.size .
  S.fromList .
  (last <$>) .
  scanl (moveKnot) (replicate n (mempty)) . 
  (moveToPositions =<<)

main :: IO ()
main = do
  input <- fmap (read :: String -> Move) . lines <$> getContents
  print $ (solve 2 &&& solve 10) $ input
