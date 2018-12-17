module Parse where

import Types
import Text.Parsec
import Text.Parsec.String



expect4 :: Parser [a] -> Parser (a, a, a, a)
expect4 p = do
  is <- p
  case is of
    [r0, r1, r2, r3] -> pure (r0, r1, r2, r3)
    _                -> unexpected "list must be 4 long"

int :: Parser Int
int = read <$> many1 digit

registers :: Parser Registers
registers = expect4 $ between (string "[") (string "]") (sepBy1 int (string ", "))


before = string "Before: " *> registers <* newline
after  = string "After:  " *> registers <* newline

instruction :: Parser Instruction
instruction = expect4 $ sepBy1 int (string " ") <* newline

sample :: Parser Sample
sample = Sample <$> before <*> instruction <*> after <* newline

input = do
  samples <- many1 sample
  newline
  newline
  instructions <- many1 instruction
  pure (samples, instructions)

