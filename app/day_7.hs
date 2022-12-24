{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

import qualified Data.Map.Strict as Ma
import qualified Data.List.Split as LS
import qualified Data.List       as L
import Control.Lens
import Utils
import Control.Arrow

data File = Dir  {_files :: Ma.Map String File, _name :: String, _size :: Int}
          | Data {_size  :: Int, _name :: String}
          deriving stock Show

data Command = Cd String
             | Ls [File]
             deriving stock Show

parseLine :: String -> Command
parseLine str
  | first4 == "$ cd" = Cd . drop 1 $ rest
  | first4 == "$ ls" = Ls $ []
  | first4 == "dir " = Ls . L.singleton . Dir (mempty :: Ma.Map String File) rest $ 0
  | otherwise        = Ls . L.singleton . (uncurry Data) . (read *** id) . $(makeListToTuple 2) . LS.splitWhen (==' ') $ str
  where first4 = take 4 str
        rest   = drop 4 str

groupLs :: [Command] -> [Command]
groupLs [] = []
groupLs ((Ls a):(Ls b):rest) = groupLs ((Ls (a <> b)):rest)
groupLs (a:b) = a:groupLs b

execCd :: String -> [File] -> [File]
execCd "/"  fsMem = L.singleton . head $ fsMem
execCd ".." fsMem = tail $ fsMem
execCd str  (cur:rest) = (newFile:cur:rest)
  where 
    newFile = maybe cur id . Ma.lookup str . _files $ cur

adjustAll :: [File] -> [File]
adjustAll = scanl1 (\next prev -> Dir (Ma.adjust (const next) (_name next) (_files prev)) (_name prev) (_size prev))

execLs :: [File] -> [File] -> [File]
execLs newFiles (cur:rest) = adjustAll(newCur:rest)
  where addFiles s nF  = foldr (\a b -> b & at (_name a) ?~ a) (_files s) nF
        newCur         = Dir (addFiles cur newFiles) (_name cur) (_size cur)

makeFileSystem :: [Command] -> [File] -> [File]
makeFileSystem [] fsMem = fsMem
makeFileSystem ((Ls a):rest) fsMem = (makeFileSystem rest newFsMem)
  where newFsMem = execLs a fsMem

makeFileSystem ((Cd a):rest) fsMem = (makeFileSystem rest newFsMem)
  where newFsMem = execCd a fsMem

getSize :: File -> Int
getSize (Data x  _  ) = x
getSize (Dir  fs _ _) = foldr (\a b -> (getSize a) + b) 0 fs

getDirs :: File -> [File]
getDirs (Data _ _)   = []
getDirs d = d:(concat . (Ma.map getDirs) . _files $ d)

solveP1 :: File -> Int
solveP1 root = sum . filter (<= 100000) $ sizes
  where 
    sizes = getSize <$> (getDirs root)

solveP2 :: File -> Int
solveP2 root = minimum . filter (>=rootSize - 40000000) $ sizes
  where
    sizes = getSize <$> (getDirs root)
    rootSize = head sizes

main :: IO ()
main = 
  last . (flip makeFileSystem) [Dir (Ma.empty) "/" 0] . groupLs . (parseLine <$>) . lines <$> getContents >>=
  print . (solveP1 &&& solveP2) 
