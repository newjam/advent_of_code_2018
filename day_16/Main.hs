import Types
import Operations
import Parse

import Text.Parsec
import Text.Parsec.String

import qualified Data.Set as Set
import qualified Data.Map as Map

parseInput = runParser input () "bleh"

possibleOpcodes (Sample before (_, a, b, c) after) = filter testOpcode opcodes
  where testOpcode opcode = after == (operationOf opcode) a b c before

exampleSample1 = Sample (0, 3, 0, 3) (9, 0, 0, 1) (0, 0, 0, 3)

exampleSample2 = Sample (0, 0, 2, 0) (9, 0, 0, 3) (0, 0, 2, 0)

foo :: Sample -> Map.Map Int (Set.Set Opcode)
foo s@(Sample _ (o, _, _, _) _) = Map.singleton o (Set.fromList $ possibleOpcodes s)

bar = foldr (Map.unionWith Set.intersection) Map.empty . map foo

-- A lot of dangerous indexing here 8)
lookupOperation map = operationOf . (Map.!) map


answer (samples, program) = (part1 samples, part2 samples program)

part1 = length . filter (>=3) . map (length . possibleOpcodes)

runProgram lookup program = composed initial
  where
    instruction (o, a, b, c) = (lookup o) a b c
    instructions = map instruction program
    composed = foldl (.) id . reverse $ instructions
    initial = (0, 0, 0, 0)

known :: Map.Map Int (Set.Set Opcode) -> Set.Set Opcode
known = Map.foldr Set.union Set.empty . Map.filter (\s -> Set.size s == 1)

removeKnown m = Map.map (\s -> if Set.size s == 1 then s else Set.difference s (known m)) m

part2 samples program = runProgram lookup program
  where
    lookup = lookupOperation . Map.map (\s -> if Set.size s == 1 then Set.elemAt 0 s else error "") . iterateN 20 removeKnown . bar $ samples

    (r0, _, _, _) = runProgram lookup program
    iterateN n = foldr (.) id . take 20 . repeat

