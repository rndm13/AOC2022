{-# LANGUAGE DerivingStrategies #-}

import Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as LS

data Instruction = Noop 
                 | Addx Int 
                 deriving stock (Show)

instance Read Instruction where
  readsPrec _ i = 
    case insName of
      "noop" -> [(Noop, restNoop)]
      "addx" -> [(Addx cnt, restAddx)]
    where insName  = take 4 i
          cnt      = read . takeWhile (/=' ') . drop 5 $ i
          restNoop = drop 4 i
          restAddx = dropWhile (/=' ') . drop 5 $ i

doInstruction :: [(Int, Int)] -> Int -> Instruction -> [(Int, Int)]
doInstruction cyc _    Noop    = cyc
doInstruction cyc ind (Addx x) = rest <> ((id *** (+x)) <$> cur)
  where (rest, cur) = L.partition ((<ind) . fst) cyc

instructionTime :: Instruction -> Int
instructionTime Noop = 1
instructionTime (Addx _) = 2

indexInstructions :: [Instruction] -> [(Int, Instruction)]
indexInstructions instr = zip (tail . scanl (\s x -> s + (instructionTime x)) 1 $ instr) instr

completeInstructions :: [Instruction] -> [Int] -> [(Int, Int)]
completeInstructions instr ind = foldr (\i cyc -> (uncurry $ doInstruction cyc) i) cycles . indexInstructions $ instr
  where cycles = ((,) <$> id <*> (const 1)) <$> ind

p1Cycles :: [Int]
p1Cycles = [20,60,100,140,180,220]

solveP1 :: [Instruction] -> Int
solveP1 instr = sum . ((uncurry (*)) <$>) . completeInstructions instr $ p1Cycles

allCycles :: [Int]
allCycles = [1..240]

drawPixel :: (Int,Int) -> Char
drawPixel (a, b) 
  | abs (((a - 1) `rem` 40) - b) <= 1 = '#'
  | otherwise = '.'

solveP2 :: [Instruction] -> [String]
solveP2 instr = LS.chunksOf 40 . (drawPixel <$>) . completeInstructions instr $ allCycles

main :: IO ()
main = do
  input <- (((map read) . lines) <$> getContents) :: IO [Instruction]
  print . solveP1 $ input
  (mapM_ putStrLn) . solveP2 $ input
