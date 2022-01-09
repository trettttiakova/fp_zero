module HW0.T4 where

import Data.Function
import GHC.Natural

-- behaves like Data.List.repeat
repeat' :: a -> [a]
repeat' x = fix (x:)        


-- map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map

-- computes the n-th Fibonacci number
fib :: Natural -> Natural
fib = fix (\rec n -> if n <= 1 then 1 else n + rec (n - 1))

-- computes the factorial
fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
  