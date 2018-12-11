import Control.Arrow
import qualified Data.Set as Set

import Data.Maybe (catMaybes)

parseLine ('+':cs) = read cs
parseLine ('-':cs) = (-1) * read cs

solution = lines >>> map parseLine >>> cycle >>> freqs >>> firstRepeat >>> show

freqs = scanl (+) 0

firstRepeat :: [Int] -> Int
firstRepeat = head . catMaybes . map fst . scanl foo (Nothing, Set.empty)

foo (_, previous) x = (duplicates', previous') where
  duplicates' = if x `Set.member` previous then Just x else Nothing
  previous' = Set.insert x previous


main = getContents >>= (solution >>> putStrLn)



