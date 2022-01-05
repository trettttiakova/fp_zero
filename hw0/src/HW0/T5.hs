module HW0.T5 where
import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz func x = x

ns :: Nat a -> Nat a
-- ((a -> a) -> a -> a) -> (a -> a) -> a -> a
-- simplified for: ns nat func x = nat func x
ns nat func x = nat func (func x)

nplus, nmult :: Nat a -> Nat a -> Nat a
-- ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
nplus n1 n2 func x = n1 func $ n2 func x
nmult n1 n2 func = n1 $ n2 func

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

nToNum :: Num a => Nat a -> a
nToNum nz = 0

