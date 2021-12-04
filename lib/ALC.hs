module ALC where

strings :: Char -> String -> [String]
strings _ [] = []
strings sep s
  | length y == 0 = takeWhile (/= sep) s : (strings sep y)
  | otherwise = takeWhile (/= sep) s : (strings sep $ tail y)
  where y = dropWhile (/= sep) s

splitEvery :: Int -> [b] -> [[b]]
splitEvery _ [] = []
splitEvery n xs = (take n xs) : (splitEvery n $ drop n xs)
