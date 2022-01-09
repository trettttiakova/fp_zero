module HW0.T5
  ( Nat
  , ns
  , nz
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  )
  where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

-- Returns a Nut corresponding to zero.
nz :: Nat a
nz func x = x

-- Returns an incremented Nat.
ns :: Nat a -> Nat a
-- ((a -> a) -> a -> a) -> (a -> a) -> a -> a
ns nat func x = func $ nat func x

-- Addition and multiplication for Nat.
nplus, nmult :: Nat a -> Nat a -> Nat a
-- ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
nplus n1 n2 func x = n1 func $ n2 func x
nmult n1 n2 func   = n1 $ n2 func

-- Retuns a Nat corresponding to a given natural number.
nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

-- Returns a natural number corresponding to a given Nat.
nToNum :: Num a => Nat a -> a
nToNum nat = nat (+1) 0
