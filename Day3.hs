module Day3 where

import Data.Word
import Data.Bits
import Data.List

type GammaRate = Word32
type EpsilonRate = Word32
type LSRating = Word32
type BitCriteria = [Word32] -> Int -> Word32

strToWord32 :: String -> Word32
strToWord32 ""     = 0
strToWord32 (x:xs) = 
  case x of
    '0' -> strToWord32 xs
    '1' -> 2^(length xs) + strToWord32 xs

mostCommonBitAtPos :: BitCriteria
mostCommonBitAtPos xs n
  | s > t = 1
  | s < t = 0
  | s == t = 1
  where s = fromIntegral . sum $ map (bit n .&.) xs
        t = 2^n * (genericLength xs / 2)

leastCommonBitAtPos :: BitCriteria
leastCommonBitAtPos xs n = if mostCommonBitAtPos xs n == 0 then 1 else 0

getGamma :: [Word32] -> Int -> GammaRate
getGamma xs (-1) = 0
getGamma xs n = 
  case mostCommonBitAtPos xs n of
    0 -> next
    1 -> (shift 1 n) + next
  where next = getGamma xs (n-1)
  
gammaToEpsilon :: GammaRate -> Word32 -> EpsilonRate
gammaToEpsilon w n = complement w .&. (2^n - 1)

getLSRating :: [Word32] -> Int -> BitCriteria -> LSRating
getLSRating xs n bc = if length xs == 1 then head xs else
                          getLSRating (filter nthBitMatch xs) (n-1) bc
  where nthBitMatch = (== shift (bc xs n) n) . (bit n .&.)

getData :: IO [Word32]
getData = do
  d <- readFile "d3.in"
  return $ (map strToWord32 . lines $ d)
  
main :: IO ()
main = do
  d <- getData
  let gamma = getGamma d 11
  let epsilon = gammaToEpsilon gamma 12
  print $ gamma * epsilon
  let o2 = getLSRating d 11 mostCommonBitAtPos
  let co2 = getLSRating d 11 leastCommonBitAtPos
  print $ o2 * co2
