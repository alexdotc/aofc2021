module Main where

import ALC (strings)
import Data.Bool
import qualified Data.List as L

getFuel :: Int -> [Int] -> Int
getFuel m = foldr ((+) . abs . (flip (-) m)) 0

getRealFuel :: Int -> [Int] -> Int
getRealFuel m = 
  foldr ((+) . round . sumn . fromIntegral . abs . (flip (-) m)) 0
    where sumn n = (n^2 + n) / 2

parseData :: [String] -> [Int]
parseData = map (\x -> read x :: Int) . strings ',' . head

getData :: IO [String]
getData = do
  d <- readFile "data/d07.in"
  return $ lines d

main :: IO ()
main = do
  d <- getData
  let cs = L.sort $ parseData d
  let lcs = length cs
  let len = if odd (lcs) then (lcs-1) else lcs
  let med = cs !! (len `div` 2)
  let mean = fromIntegral (sum cs) / fromIntegral lcs
  print $ getFuel med cs
  print $ min (getRealFuel (ceiling mean) cs) (getRealFuel (floor mean) cs)
