-- from http://okmij.org/ftp/continuations/undelimited.html

{-# LANGUAGE RankNTypes #-}

module GenCont where

import Control.Monad (ap, liftM)


newtype Cont w a = Cont { unCont :: (a -> w) -> w }

instance Monad (Cont w) where
    return x = Cont $ \k -> k x
    m >>= f  = Cont $ \k -> unCont m (\mv -> unCont (f mv) k)

instance Applicative (Cont w) where
  pure = return
  (<*>) = ap

instance Functor (Cont w) where
  fmap = liftM


-- The type of the captured continuation
-- Once again, it is not a function in the object language;
-- it's result is NOT of the type Cont w x
type K w a = a -> w

callCC :: (K w a -> Cont w a) -> Cont w a
callCC f = Cont $ \k -> unCont (f k) k

throw :: K w a -> a -> Cont w b
throw k x = Cont $ \ _ -> k x

-- And here is the main difference from Cont: the higher-rank type of
-- runCont, ensuring that the computation should really not have looked
-- at the answer-type
runCont :: (forall w. Cont w a) -> a
runCont m = unCont m id
