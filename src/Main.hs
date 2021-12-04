module Main where

getData :: IO [String]
getData = do
  d <- readFile "data/d05.in"
  return $ (lines d)
  
main :: IO ()
main = do
  d <- getData
  print "hello"
