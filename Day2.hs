module Day2 where

type Horiz = Integer
type Depth = Integer
type Aim = Integer

calculatePositions :: [(String, Integer)] -> ([Horiz],[Depth])
calculatePositions l = ([goHoriz x | x <- l], [goDepth x | x <- l])
  where goHoriz (a,n) = case a of
          "forward"  -> n
          _          -> 0
        goDepth (a,n) = case a of
          "up"   -> negate n
          "down" -> n
          _      -> 0

calculatePositionsWithAim :: [Horiz] -> [Depth] -> Aim -> Depth
calculatePositionsWithAim [] _ _ = 0
calculatePositionsWithAim (h:hs) (d:ds) aim = 
    h*aim + calculatePositionsWithAim hs ds (aim+d)

getMovements :: IO [(String, Integer)]
getMovements = do
  movements <- readFile "d2.in"
  return $ (map f . lines $ movements)
    where f x = (head (words x), (read $ last $ words x) :: Integer)
  
main :: IO ()
main = do
  movements <- getMovements
  let positions = calculatePositions movements
  print $ (sum $ fst positions) * (sum $ snd positions)
  let positionsA = calculatePositionsWithAim (fst positions) (snd positions) 0
  print $ (sum $ fst positions) * positionsA
