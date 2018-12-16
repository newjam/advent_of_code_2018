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

draw nth xs ps = drawPots $ (iterate (next ps) xs) !! nth

handle parseResult = case parseResult of
  Right (xs, ps) -> show (part1 50000000000 xs ps)
  Left error     -> show error

expect (Right x) = x
expect (Left e) = error . show $ e

table n xs ps = sequence_ . take n .  map (putStrLn . show . sum) $ generations
  where generations = iterate (next ps) xs



stuff fn = runParser Parse.setup () ""  <$> readFile fn
