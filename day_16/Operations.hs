module Operations where

import Types
import Data.Bits

readr :: Int -> Registers -> Int
readr 0 (v, _, _, _) = v
readr 1 (_, v, _, _) = v
readr 2 (_, _, v, _) = v
readr 3 (_, _, _, v) = v

setr a _ c registers =  let v = readr a registers in seti v 0 c registers

seti a _ 0 (_ , r1, r2, r3) = (a , r1, r2, r3)
seti a _ 1 (r0, _ , r2, r3) = (r0, a , r2, r3)
seti a _ 2 (r0, r1, _ , r3) = (r0, r1, a , r3)
seti a _ 3 (r0, r1, r2, _ ) = (r0, r1, r2, a )

bin l r op a b c registers = seti (x `op`  y) 0 c registers where
  x = l a registers
  y = r b registers

bin_rr op a b c registers = bin readr readr op a b c registers
bin_ri op a b c registers = bin readr const op a b c registers
bin_ir op a b c registers = bin const readr op a b c registers

addr = bin_rr (+)
addi = bin_ri (+)

mulr = bin_rr (*)
muli = bin_ri (*)

banr = bin_rr (.&.)
bani = bin_ri (.&.)

borr = bin_rr (.|.)
bori = bin_ri (.|.)

gt a b = fromEnum (a > b)
eq a b = fromEnum (a == b)

gtir = bin_ir gt
gtri = bin_ri gt
gtrr = bin_rr gt

eqir = bin_ir eq
eqri = bin_ri eq
eqrr = bin_rr eq

-- OK, dynamic languages, you've got me here....
operationOf :: Opcode -> Operation
operationOf op = case op of
  ADDR -> addr
  ADDI -> addi
  MULR -> mulr
  MULI -> muli
  BANR -> banr
  BANI -> bani
  BORR -> borr
  BORI -> bori
  SETR -> setr
  SETI -> seti
  GTIR -> gtir
  GTRI -> gtri
  GTRR -> gtrr
  EQIR -> eqir
  EQRI -> eqri
  EQRR -> eqrr

