import Data.Char
import Control.Arrow
import Data.Function
import Data.List

sameType = (==) `on` toLower
oppositePolarity = (/=) `on` isLower

reactable x1 x2 = x1 `sameType` x2 && x1 `oppositePolarity` x2

reactTwo x1 (x2:xs) = if reactable x1 x2 then xs else x1:x2:xs
reactTwo x1 []      = [x1]

reactAll = foldr reactTwo ""

lengthPolymer = length . reactAll

example = "dabAcCaCBAcCcaDA"

-- remove all units of type t
removeType t = filter (not . sameType t)

-- given a polymer create a list of polymers each one create by removing all units of a single type
variants xs = map (flip removeType xs) types
  where types = (nub . map toLower) xs

stuff xs = p1 ++ "\n" ++ p2 where
  p1 = "Part 1: " ++ (show . lengthPolymer $ xs)
  p2 = "Part 2: " ++ (show . minimum . map lengthPolymer . variants $ xs)

main = getLine >>= (stuff >>> putStrLn)

