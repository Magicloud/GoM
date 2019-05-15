module GoM.Helper where

import Data.List

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn as a = filter ([a] /=) $ groupBy (\y x -> y /= a && x /= a) as
