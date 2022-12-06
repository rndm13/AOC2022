{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where
import           Control.Arrow
import           Control.Lens
import           Data.Char       (isAlpha)
import qualified Data.List       as L
import qualified Data.List.Split as LS
import           Utils

data Crate = Crate Char
           | EmptySpace
           deriving stock (Eq)

instance Read Crate where
  readsPrec _ input =
    if cur == "   "
      then [(EmptySpace, rest)]
      else [(Crate (cur !! 1), rest)]
        where
          cur  = take 3 input
          rest = drop 3 input

instance Show Crate where
  show (Crate c)  = [c]
  show EmptySpace = " "

data Move = Move (Int, Int, Int)

doMoveWith :: ([Crate] -> [Crate]) -> Move -> [[Crate]] -> [[Crate]]
doMoveWith func (Move (count, fromInd, toInd)) crates =
  (over (ix (toInd - 1)) (func toMove ++)) . (over (ix (fromInd - 1)) (drop count)) $ crates
    where toMove = take count (crates !! (fromInd - 1))

solveWith :: (Move -> [[Crate]] -> [[Crate]]) -> [[Crate]] -> [Move] -> [[Crate]]
solveWith mover crates moves = foldr (mover) crates (reverse moves)

solveWithMoves :: ([Crate] -> [Crate]) -> [[Crate]] -> [Move] -> String
solveWithMoves f crates moves = getTops . solveWith (doMoveWith f) crates $ moves

getTops :: [[Crate]] -> String
getTops = concat . (show . maybe EmptySpace id . safeHead <$>)

main :: IO ()
main = do
  input <- ((,) --- Parsing the crate stacks
            <$> (reverse
             .  (map (L.dropWhile (==EmptySpace)))
             .  L.transpose
             .  (readAsList
             .  reverse
             .  LS.chunksOf 4 <$>)
             .  dropr 1
             .  head)
            --- Parsing moves
            <*> (Move
             .  $(makeArrayToTuple 3)
             .  readAsList
             .  filter (not . null)
             .  LS.splitWhen (==' ')
             .  filter (not . isAlpha) <$>)
             .  last)
            --- Reading input -> Splitting to lines -> Splitting at the empty line
                . LS.splitWhen null
                . lines
               <$> getContents
            --- In the end we get a tuple (Crate stacks, Move)
  print . (uncurry (solveWithMoves reverse) &&& uncurry (solveWithMoves id)) $ input
