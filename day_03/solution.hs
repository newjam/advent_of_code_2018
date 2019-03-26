import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Arrow

data Claim = Claim Integer (Integer, Integer) (Integer, Integer)
  deriving (Eq, Show, Ord)


space = char ' '
natural = read <$> many1 (satisfy isDigit)


-- #1 @ 906,735: 28x17

claimParser = do
  char '#'
  id <- natural
  string " @ "
  offset_x <- natural
  char ','
  offset_y <- natural
  string ": "
  width <- natural
  char 'x'
  height <- natural
  return (Claim id (offset_x, offset_y) (width, height))

safeHead (x:_) = Just x
safeHead _     = Nothing

runParser parser = fmap fst . safeHead . filter (null . snd) . readP_to_S parser

parseClaim :: String -> Maybe Claim
parseClaim = runParser claimParser

points (Claim _ (i, j) (m, n)) = [(x, y) | x <- [i .. i+m-1], y <- [j .. j + n-1]]

frequency :: (Ord a, Foldable t) => t a -> Map.Map a Integer
frequency = foldr (flip (Map.insertWith (+)) 1) Map.empty

-- How many square inches are used by 2 or more claims
answer_p1 :: Foldable t => t Claim -> Int
answer_p1 = length . filter (>=2) . Map.elems . frequency . concatMap points

type Point = (Integer, Integer)

foo :: Map.Map Point Integer -> Claim -> Bool
foo freq =  all (\k -> freq Map.! k == 1) . points

answer_p2 claims = filter (foo freqs) claims
  where freqs = (frequency . concatMap points) claims

main = getContents >>= (lines >>> map parseClaim >>> sequence >>> fmap answer_p2 >>> show >>> putStrLn)

