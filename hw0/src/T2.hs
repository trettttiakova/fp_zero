module T2 where

import Data.Void

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)  -- a -> (a -> Void) -> Void
doubleNeg a fun = fun a

reduceTripleNeg :: Not (Not (Not a)) -> Not a  -- (((a -> Void) -> Void) -> Void) -> a -> Void
reduceTripleNeg fun a = fun (doubleNeg a) 
