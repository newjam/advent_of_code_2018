import Text.Parsec
import Text.Parsec.String
import Control.Arrow
import Data.List (intercalate)
import System.Environment

natural :: Parser Integer
natural = read <$> many1 digit

sign :: Parser Integer
sign = option 1 (char '-' *> pure (-1))

integer :: Parser Integer
integer = (*) <$> sign <*> natural

vector :: Parser (Integer, Integer)
vector = do
  char '<'
  spaces
  v1 <- integer
  char ','
  spaces
  v2 <- integer
  char '>'
  return (v1, v2)

start :: Parser ((Integer, Integer), (Integer, Integer))
start = do
  string "position="
  position <- vector
  string " velocity="
  velocity <- vector
  return (position, velocity)

initialConditions = many1 (start <* string "\n")

parseInitialConditions = runParser initialConditions () "stdin"

atTime time ((init_x, init_y), (vel_x, vel_y)) = (init_x + time * vel_x, init_y + time * vel_y)

bounds points = ((min_x, min_y), (max_x, max_y)) where
  min_x = (minimum . map fst) points
  max_x = (maximum . map fst) points
  min_y = (minimum . map snd) points
  max_y = (maximum . map snd) points

draw points = intercalate "\n" [[if (x, y) `elem` points then '#' else ' ' | x <- xs]  | y <- ys]
  where
    ((min_x, min_y), (max_x, max_y)) = bounds points
    xs = [min_x .. max_x]
    ys = [min_y .. max_y]

dimensions ((min_x, min_y), (max_x, max_y)) = (max_x - min_x, max_y - min_y)

--stuff time (Right points) = show . dimensions . bounds . map (atTime time) $ points
stuff time (Right points) =  draw . map (atTime time) $ points
stuff _ (Left error) = show error

getTime :: IO Integer
getTime = (read . head) <$> getArgs

getInitialConditions = parseInitialConditions <$> getContents

main = (stuff <$> getTime <*> getInitialConditions) >>= putStrLn

