import qualified Data.Set as Set
import Types

import qualified Parse
import Text.Parsec
import Text.Parsec.String

-- patternParser = (,,,,) <$> 

--ruleParser = do


alive = flip Set.member

dead state = not . alive state

neighborSet x = Set.fromList [x - 2 .. x + 2]

activePots = Set.foldr Set.union Set.empty . Set.map neighborSet

neighbors x = (x - 2, x -1 , x, x + 1, x + 2)

pattern xs (x1, x2, x3, x4, x5) = (alive' x1, alive' x2, alive' x3, alive' x4, alive' x5) where alive' = alive xs

lives :: Set.Set Pattern -> Pattern -> Bool
lives ps p = p `Set.member` ps

next ps xs = Set.filter (lives ps . pattern xs . neighbors) (activePots xs)

drawPot p = if p then '#' else '.'

drawPots xs = map (drawPot . alive xs) [Set.findMin xs .. Set.findMax xs]

part1 n xs ps = sum generation
  where
    generations = iterate (next ps) xs
    generation  = generations !! n

-- Part 2 requires finding the answer for the 50000000000th generation.
-- This is intractible to simulate, so some analysis is in order.
-- Mustering my first quarter of undergraduate math, we can find the first difference, and second difference
-- given our generations,
-- let sums = map sum generations
-- let diff1 = zipWith (-) (drop 1 sums) sums
-- let diff2 = zipWith (-) (drop 1 diff1) diff1
-- Then we can see that diff2 !! n == 0 for all n >= 108.
-- Also, sums !! 108 == 7976
-- So the answer is 65 * (50000000000 - 108) + 7976, ie 3250000000956!

draw nth xs ps = drawPots $ (iterate (next ps) xs) !! nth

handle parseResult = case parseResult of
  Right (xs, ps) -> show (part1 50000000000 xs ps)
  Left error     -> show error

expect (Right x) = x
expect (Left e) = error . show $ e

table n xs ps = sequence_ . take n .  map (putStrLn . ) $ [0..] generations
  where generations = iterate (next ps) xs
        sums = map sum generations
        diff1s = zipWith (-1) sums (drop 1 sums)
        row (i, generation) = show i ++ " " ++ (show . sum $ generation)

stuff fn = runParser Parse.setup () ""  <$> readFile fn
