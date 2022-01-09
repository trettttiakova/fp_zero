{-# LANGUAGE TypeOperators #-}

module HW0.T1 
  ( assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  )
  where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso one two
  where
    one :: Either a (Either b c) -> Either (Either a b) c
    one (Left a)          = Left $ Left a
    one (Right (Left b))  = Left $ Right b
    one (Right (Right c)) = Right c
    two :: Either (Either a b) c -> Either a (Either b c)
    two (Left (Left a))   = Left a
    two (Left (Right b))  = Right $ Left b
    two (Right c)         = Right $ Right c

