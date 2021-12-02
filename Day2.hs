module Day2 where

type Horiz = Integer
type Depth = Integer

calculatePositions :: [(String, Integer)] -> ([Horiz],[Depth])
calculatePositions l = ([goHoriz x | x <- l], [goDepth x | x <- l])
  where goHoriz (a,n) = case a of
          "forward"  -> n
          _          -> 0
        goDepth (a,n) = case a of
          "up"   -> negate n
          "down" -> n
          _      -> 0

calculatePosition :: [(String, Integer)] -> (Horiz, Depth)
calculatePosition movements = (sum(fst cp), sum(snd cp))
  where cp = calculatePositions movements

getMovements :: IO [(String, Integer)]
getMovements = do
  movements <- readFile "movements.txt"
  return $ (map f . lines $ movements)
    where f x = (head (words x), (read $ last $ words x) :: Integer)
  
main :: IO ()
main = do
  movements <- getMovements
  let position = calculatePosition movements
  print $ fst position * snd position
