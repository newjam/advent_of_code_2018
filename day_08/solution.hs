import Data.Tree
import Control.Arrow
import Text.Parsec
import Control.Monad (replicateM)

type Parser = Parsec [Int] ()

tree :: Parser (Tree [Int])
tree = do
  numChildren <- anyToken
  numMetadata <- anyToken
  children <- replicateM numChildren tree
  metadata <- replicateM numMetadata anyToken
  return (Node metadata children)

parseTree = runParser tree () "stdin"

part1 :: Tree [Int] -> Int
part1 = sum . fmap sum

part2 :: Tree [Int] -> Int
part2 (Node metadata children) =
  if null children then
    sum metadata
  else
    (sum . map valueAt) metadata
  where
    valueAt index = if index <= length children && index >= 1 then part2 (children !! (index - 1)) else 0

bothParts t = (part1 t, part2 t)

example :: [Int]
example = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

main = getContents >>= (words >>> map read >>> parseTree >>> fmap bothParts >>> show >>> putStrLn)

