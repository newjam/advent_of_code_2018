module Types where

import Control.Arrow
--import Data.Set
import Data.Bits

{--
data Register = R0 | R1 | R2 | R3
  deriving (Ord, Eq, Enum, Bounded, Show)
--}

type Register = Int
type Value = Int

type Registers = (Value, Value, Value, Value)

--opcodes :: Set Opcode

type Instruction = (Int, Int, Int, Int)

data Sample = Sample Registers Instruction Registers
  deriving (Eq, Ord, Show)

type Program = [Instruction]

type Operation = Int -> Int -> Int -> Registers -> Registers

data Opcode = ADDR | ADDI -- Addition
            | MULR | MULI -- Multiplication
            | BANR | BANI -- Bitwise And
            | BORR | BORI -- Bitwise Or
            | SETR | SETI -- Assignment
            | GTIR | GTRI | GTRR -- Greater-than testing
            | EQIR | EQRI | EQRR -- Equality testing
  deriving (Ord, Eq, Enum, Bounded, Show)

opcodes :: [Opcode]
opcodes = [minBound..maxBound]


