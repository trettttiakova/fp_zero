module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  )
  where

import Data.Function
import GHC.Natural

-- behaves like Data.List.repeat
repeat' :: a -> [a]
repeat' x = fix (x:)

-- behaves like Data.List.map
map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec func ar -> if null ar then [] else func (head ar) : rec func (tail ar))

-- computes the n-th Fibonacci number
fib :: Natural -> Natural
fib = fix (\rec n -> if n <= 1 then 1 else n + rec (n - 1))

-- computes the factorial
fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
  