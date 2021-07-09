import Data.Tree
import Control.Arrow
import Text.Parsec

{--

data Parser b a = Parser (b -> (a, b))

instance Functor (Parser  b) where
  fmap f (Parser p) = Parser p' where
    p' cs = case p cs of (x, cs') -> (f x, cs')

instance Applicative (Parser b) where
  pure x = Parser (\cs -> (x, cs))
  Parser p1 <*> Parser p2 = Parser p3 where
    p3 cs = case p1 cs of (f, cs') -> case p2 cs' of (x, cs'') -> (f x, cs'')

instance Monad (Parser b) where
  (Parser p) >>= f = Parser( \cs -> case p cs of
    (x, cs') -> case f x of Parser p' -> p' cs')


runParser (Parser p) = \cs -> p cs


number = let p (i:is) = (i, is) in Parser p

--}
--nat :: Parsec Integer
--nat = read <$> many1 digit


type Parser = Parsec [Integer] ()

tree :: Parser (Tree [Integer])
tree = do
  numChildren <- anyToken
  numMetadata <- anyToken
  children <- (sequence . take (fromIntegral numChildren) . repeat) tree
  metadata <- (sequence . take (fromIntegral numMetadata) . repeat) anyToken
  return (Node metadata children)


part1 :: Tree [Integer] -> Integer
part1 = sum . fmap sum

parseTree = runParser tree () "stdin"

main = getContents >>= (words >>> map read >>> parseTree >>> fmap part1 >>> show >>> putStrLn )

