module GoM.Helper where

import qualified Data.ByteString.Char8 as C8
import           Data.List

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn as a = filter ([a] /=) $ groupBy (\y x -> y /= a && x /= a) as

readIntegerAll :: C8.ByteString -> Maybe Integer
readIntegerAll s = do
  (i, s') <- C8.readInteger s
  if s' == C8.empty
    then return i
    else fail ""
