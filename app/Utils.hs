{-# LANGUAGE TemplateHaskell #-}

module Utils( makeUncurry
            , makeListBinder
            , makeListToTuple
            , dropr
            , safeHead
            , readAsList
            , count
            , toBoth
            , iterateM
            , iterateM') where

import           Control.Arrow              ((***), Arrow, (>>>))
import qualified Control.Monad              as M
import           Data.Maybe                 (isJust, isNothing, fromJust)
import qualified Data.List                  as L
import           Text.Printf
import           Language.Haskell.TH

makeUncurry :: Int -> Q Exp
makeUncurry n = do
  funcName <- newName "f"
  names    <- genNames n
  tupNames <- tupP $ varP <$> names
  appNames <- (foldl (appE) (varE funcName) $ varE <$> names)
  return $ LamE [VarP funcName] (LamE [tupNames] appNames)

makeListBinder :: Int -> Q Exp
makeListBinder n = do
  funcName  <- newName "f"
  names     <- genNames n
  listNames <- listP $ varP <$> names
  appNames  <- (foldl (appE) (varE funcName) $ varE <$> names)
  return $ LamE [VarP funcName] (LamE [listNames] appNames)

makeListToTuple :: Int -> Q Exp
makeListToTuple n = do
  names     <- genNames n
  listNames <- listP $ varP <$> names
  tupNames  <- tupE $ varE <$> names
  return $ LamE [listNames] tupNames

genNames :: (Quote m) => Int -> m [Name]
genNames n = M.replicateM n (newName "t")

dropr :: Int -> [a] -> [a]
dropr n arr = take (length arr - n) arr

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead arr = Just (head arr)

readAsList :: Read r => [String] -> r
readAsList = read . printf "[%s]" . L.intercalate ","

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count f arr = foldr (\x s -> if (f x) then (s + 1) else s) 0 arr

toBoth :: Arrow a => a c d -> a b (c, c) -> a b (d, d)
toBoth toAdd orig = orig >>> (toAdd *** toAdd) 


iterateM' :: (a -> Maybe a) -> a -> Maybe a
iterateM' func input 
  | isJust next = next >>= iterateM' func 
  | otherwise = cur
  where cur = func input
        next = cur >>= func
        
iterateM :: (a -> Maybe a) -> a -> [a]
iterateM func input
  | isNothing . func $ input = []
  | otherwise = (fromJust . func $ input):(iterateM func (fromJust . func $ input))
