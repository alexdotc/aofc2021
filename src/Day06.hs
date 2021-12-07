module Main where

import ALC (strings)
import qualified Data.Map as M

mkFish :: [String] -> [Int]
mkFish = map (\x -> read x :: Int) . strings ',' . head

reproduce :: Int -> M.Map Int Int -> M.Map Int Int
reproduce 0 m = m
reproduce n m = reproduce (n-1) $ M.mapWithKey discern m
  where discern k _
          | k == 8    = M.findWithDefault 0 0 m
          | k == 6    = M.findWithDefault 0 0 m + M.findWithDefault 0 7 m
          | otherwise = M.findWithDefault 0 (k+1) m

mkInit :: M.Map Int Int -> [Int] -> M.Map Int Int
mkInit m [] = m
mkInit m (d:ds) = mkInit (M.insertWith (+) d 1 m) ds
  
getData :: IO [String]
getData = do
  d <- readFile "data/d06.in"
  return $ lines d
  
main :: IO ()
main = do
  d <- getData
  let fs = mkFish d
  let start = M.fromList[(a,0) | a <- [0..8]]
  let fish = mkInit start fs
  print $ M.foldr (+) 0 $ reproduce  80 fish
  print $ M.foldr (+) 0 $ reproduce 256 fish
