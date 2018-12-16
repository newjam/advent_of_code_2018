module Parse (pot, pattern, rule, setup) where

import Types
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String

pot :: Parser Bool
pot = (char '#' *> pure True) <|> (char '.' *> pure False)

pattern = (,,,,) <$> pot <*> pot <*> pot <*> pot <*> pot

rule = (,) <$> (pattern <* string " => ") <*> pot

setup = do
  string "initial state: "
  initialState <- Set.fromList . map fst . filter snd . zip [0..] <$>  many1 pot
  string "\n\n"
  patterns <-  Set.fromList . map fst . filter snd <$>  many1 (rule <* string "\n")
  return (initialState, patterns)

