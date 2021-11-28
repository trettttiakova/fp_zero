{-# LANGUAGE TypeOperators #-}

module HW0.T1 where

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
assocEither = Iso firstFunc secondFunc
  where
    firstFunc :: Either a (Either b c) -> Either (Either a b) c
    firstFunc (Left a)          = Left $ Left a
    firstFunc (Right (Left b))  = Left $ Right b
    firstFunc (Right (Right c)) = Right c
    secondFunc :: Either (Either a b) c -> Either a (Either b c)
    secondFunc (Left (Left a)) = Left a
    secondFunc (Left (Right b))  = Right $ Left b
    secondFunc (Right c) = Right $ Right c

