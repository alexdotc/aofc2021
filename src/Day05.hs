module Main where

import ALC (strings)

data Line  = Line Point Point deriving (Eq, Show)
data Point = Point Int Int deriving (Eq, Show)
data PointVal = PV Int Point deriving (Eq, Show)
data CountDiag = NoDiag | Diag
type Grid = [PointVal]
type Size = Int

gridDimension :: Size
gridDimension = 1000

constructGrid :: Size -> Point -> Grid -- NxN grid
constructGrid n (Point x y)
  | y == n = []
  | otherwise = if x == n then incY else incX
  where incY = constructGrid n (Point 0 $ y+1)
        incX = (PV 0 $ Point x y) : constructGrid n (Point (x+1) y)

mkPoint :: String -> Point
mkPoint s = Point x y where
  x = read (head p) :: Int
  y = read (last p) :: Int
  p = strings ',' s

mkLine :: [Point] -> Line
mkLine lp = Line (head lp) (last lp)

mkLines :: [String] -> [Line]
mkLines = map mkLine . map (map mkPoint) . map (filter (/= "->")) . map words

isHoriz :: Line -> Bool
isHoriz (Line (Point x1 y1) (Point x2 y2))
  | y1 == y2 = True
  | otherwise = False

isVert :: Line -> Bool
isVert (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = True
  | otherwise = False

drawLines :: Grid -> CountDiag -> [Line] -> Grid
drawLines g d [] = g
drawLines g d (l:ls)
  | isHoriz l = drawHoriz l $ drawLines g d ls
  | isVert l  = drawVert  l $ drawLines g d ls
  | otherwise = case d of 
                  Diag -> drawDiag l $ drawLines g d ls
                  _    -> drawLines g d ls

drawHoriz :: Line -> Grid -> Grid
drawHoriz (Line (Point a b) (Point c d)) g = markPoints (points a c d) g
  where points x1 x2 y
          | x1 == x2 = [Point x1 y]
          | otherwise = if x1 < x2 then (Point x1 y) : points (x1+1) x2 y
                                   else (Point x2 y) : points x1 (x2+1) y

drawVert :: Line -> Grid -> Grid
drawVert (Line (Point a b) (Point c d)) g = markPoints (points a b d) g
  where points x y1 y2
          | y1 == y2 = [Point x y1]
          | otherwise = if y1 < y2 then (Point x y1) : points x (y1+1) y2
                                   else (Point x y2) : points x y1 (y2+1)

drawDiag :: Line -> Grid -> Grid
drawDiag (Line (Point a b) (Point c d)) g = markPoints (orderDiag a b c d) g
  where orderDiag x1 y1 x2 y2 = if (y2 < y1) then points x2 y2 x1 y1
                                             else points x1 y1 x2 y2
        points x1 y1 x2 y2
          | x1 == x2 = [Point x2 y2]
          | otherwise = (Point x1 y1) : points (x1+i) (y1+j) x2 y2
              where
                i = if x1 < x2 then 1 else negate 1
                j = if y1 < y2 then 1 else negate 1

markPoints :: [Point] -> Grid -> Grid
markPoints [] g = g
markPoints _ [] = [] -- shouldn't get here unless oob point
markPoints (z:zs) ((PV v p):gs)
  | z == p = (PV (v+1) p) : markPoints zs gs
  | otherwise = (PV v p) : markPoints (z:zs) gs

countIntersections :: Grid -> Int
countIntersections g = foldr ((+) . f) 0 g
  where f (PV v p) = if v > 1 then 1 else 0

getData :: IO [String]
getData = do
  d <- readFile "data/d05.in"
  return $ (lines d)
  
main :: IO ()
main = do
  d <- getData
  let grid = constructGrid gridDimension $ Point 0 0
  let l = mkLines d
  let vhres = countIntersections . drawLines grid NoDiag $ l
  let allres = countIntersections . drawLines grid Diag $ l
  print vhres
  print allres
