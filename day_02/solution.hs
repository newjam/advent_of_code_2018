import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe (catMaybes)

letterFrequency :: String -> Map.Map Char Int
letterFrequency = foldr (\k -> Map.insertWith (+) k 1) Map.empty

elements = foldr Set.insert Set.empty


bar n = length . filter ( letterFrequency >>> elements >>> Set.member n)


distance x1 = sum . map (\b -> if b then 0 else 1) . zipWith (==) x1


answer_part1 xs = bar 2 xs * bar 3 xs

answer_part2 xs = [common x1 x2 | x1 <- xs, x2 <- xs, distance x1 x2 == 1]

common x1 = catMaybes . zipWith (\c1 c2 -> if c1 == c2 then Just c1 else Nothing) x1

main = getContents >>= (lines >>> answer_part2 >>> show >>> putStrLn)

