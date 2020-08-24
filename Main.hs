module Main where

import Prelude
import Control.Monad
import Data.List
import Data.Maybe

prime :: Integral n => n -> Bool
prime n = not . (0 `elem`) $ (n `mod`) <$> [2..flrt n]
  where
    flrt = floor . sqrt . fromIntegral

-- Very naive trial division
factors :: Integral n => n -> [n]
factors 1 = []
factors n =
  let f = firstFactor n in
  f : factors (n `div` f)
  where
    firstFactor n = until (\a -> n `mod` a == 0 && prime a) (+1) 2

nextTrunc :: Integral n => n -> [n] -> [n]
nextTrunc b ps = [ n | p <- ps, 
                     d <- [n | n <- [1..b], not $ n `elem` factors b], 
                     let n = b * p + d, 
                     prime n ]

main :: IO ()
main = do
  let b = 10 
  let primes = [ n | n <- [2..b], prime n ]
  print $ truncs b primes 
  where
    truncs :: Integral n => n -> [n] -> [n]
    truncs _ [] = []
    truncs b ps = ps <> (truncs b $ nextTrunc b ps)

