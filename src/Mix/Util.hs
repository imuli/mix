module Mix.Util where

import           Data.List

split :: Eq a => a -> [a] -> [[a]]
split sep = fmap dropIt . groupBy (const (sep /=))
  where
    dropIt [] = []
    dropIt (x:xs) | x == sep = xs
                  | otherwise = x:xs
