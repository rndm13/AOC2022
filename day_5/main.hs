{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main where
import           Control.Lens
import           Data.Char       (isAlpha)
import qualified Data.List       as L
import qualified Data.List.Split as LS
import           Text.Printf

data Crate = Crate Char
           | EmptySpace
           deriving (Eq)

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

dropr :: Int -> [a] -> [a]
dropr n arr = take (length arr - n) arr

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead arr = Just (head arr)

doMoveWith :: ([Crate] -> [Crate]) -> [Int] -> [[Crate]] -> [[Crate]]
doMoveWith func [count, fromInd, toInd] crates =
  (over (ix (toInd - 1)) (func toMove ++))
  . (over (ix (fromInd - 1)) (drop count)) $ crates
    where toMove = take count (crates !! (fromInd - 1))

solveWith :: ([Int] -> [[Crate]] -> [[Crate]]) -> [[Crate]] -> [[Int]] -> [[Crate]]
solveWith mover crates moves = foldr (mover) crates (reverse moves)

printTops :: [[Crate]] -> IO ()
printTops = putStrLn . concat
          . (show . maybe EmptySpace id . safeHead <$>)

main :: IO ()
main = do
  input <- ((,) --- Parsing the crate stacks
                <$> (reverse . (map (L.dropWhile (==EmptySpace))) . L.transpose
                 .  (map (read 
                 .  printf "[%s]" . (L.intercalate "," . reverse . LS.chunksOf 4)))
                 .  dropr 1 . head)
                --- Parsing moves
                <*> (map (read
                 .  printf "[%s]" . L.intercalate "," 
                 .  filter (not . null) . LS.splitWhen (==' ')
                 .  filter (not . isAlpha))) . last)
                --- Reading input -> Splitting to lines -> Splitting at the empty line
                      . LS.splitWhen null . lines <$> getContents
                --- In the end we get a tuple (Crate stacks, Moves)
  printTops . (uncurry (solveWith $ doMoveWith reverse)) $ input
  printTops . (uncurry (solveWith $ doMoveWith id)) $ input
