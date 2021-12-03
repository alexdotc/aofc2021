module Day1 where

nIncreases :: [Integer] -> Integer
nIncreases [] = 0
nIncreases (x:[]) = 0
nIncreases (x:xs) = if x < head xs then 1 + next else next
  where next = nIncreases xs

n3Increases :: [Integer] -> Integer
n3Increases l =  if length l < 4 then 0 else go l
  where go (w:x:y:z:zs) = if w+x+y < x+y+z then 1 + next else next
          where next = n3Increases (x:y:z:zs)

getDistances :: IO [Integer]
getDistances = do
  distances <- readFile "d1.in"
  return $ (map (\x -> read x :: Integer) . lines $ distances)
  
main :: IO ()
main = do
  distances <- getDistances
  print $ nIncreases distances; print $ n3Increases distances
