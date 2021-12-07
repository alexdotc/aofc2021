module Main where
  
getData :: IO [String]
getData = do
  d <- readFile "data/sample.in"
  return $ lines d
  
main :: IO ()
main = do
  d <- getData
  print $ "Hello"
