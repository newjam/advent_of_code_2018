module Types where

import Data.Set

type Neighborhood = (Int, Int, Int, Int, Int)
type Pattern = (Bool, Bool, Bool, Bool, Bool)
data Rule = Rule Pattern Bool
