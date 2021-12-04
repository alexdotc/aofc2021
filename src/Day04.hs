module Day04 where

import ALC (strings, splitEvery)
import Data.List (intercalate, transpose)
import Data.Either (isLeft)

type Board = [Val]
type Val = Either Int Int

mkBoard :: [String] -> Board
mkBoard = map $ Left . (\x -> (read x :: Int))

mkBoards :: [String] -> [Board]
mkBoards [] = []
mkBoards lns = (mkBoard $ words . intercalate " " . take 5 $ lns) : (mkBoards $ drop 5 lns)

makeCalls :: [Int] -> [Board] -> [(Int, Board)]
makeCalls [] _ = []
makeCalls (c:cs) bs = case checkBoards nbs of
  Just b  -> (c, b) : makeCalls cs (filter ((/= True) . checkBoard) nbs)
  Nothing -> makeCalls cs nbs
  where nbs = markBoards c bs

markBoards :: Int -> [Board] -> [Board]
markBoards c = map $ map $ markVal c

markVal :: Int -> Val -> Val
markVal c i = case i of
  Left v -> if c == v then Right v else Left v
  _      -> i

checkBoards :: [Board] -> Maybe Board
checkBoards [] = Nothing
checkBoards (b:bs) = if checkBoard b then Just b else checkBoards bs

checkBoard :: Board -> Bool
checkBoard b = checkRows b || (checkRows $ concat . transpose . splitEvery 5 $ b)

checkRows :: Board -> Bool
checkRows [] = False
checkRows b  = (not . any isLeft . take 5 $ b) || (checkRows $ drop 5 b)

sumUnmarked :: Board -> Int
sumUnmarked [] = 0
sumUnmarked (b:bs) = case b of
  Left v -> v + sumUnmarked bs
  _      -> sumUnmarked bs

getData :: IO [String]
getData = do
  d <- readFile "data/d04.in"
  return $ (lines d)
  
main :: IO ()
main = do
  d <- getData
  let calls = map (\x -> read x :: Int) $ strings ',' $ head d
  let boards = mkBoards $ filter (/= "") $ tail d
  let winners = makeCalls calls boards
  let winner =  head $ winners
  let score = fst winner * (sumUnmarked $ snd winner)
  let loser = last $ winners
  let score2 = fst loser * (sumUnmarked $ snd loser)
  print $ score
  print $ score2
