-- from http://okmij.org/ftp/continuations/undelimited.html

{-# LANGUAGE RankNTypes #-}

module Cont where

import           Control.Monad             (ap, liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Functor.Identity


-- newtype ContT w m a = ContT { unContT :: (a -> m w) -> m w }
newtype Cont r a = Cont { unCont :: (a -> r) -> r }

instance Monad (Cont r) where
    return x = Cont $ \k -> k x
    m >>= f  = Cont $ \k -> unCont m (\mv -> unCont (f mv) k)

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Functor (Cont r) where
  fmap = liftM


-- The type of the captured continuation
-- Once again, it is not a function in the object language;
-- it's result is NOT of the type Cont w x
-- class K k where
--   throw :: k w a -> a ->
type K r a = a -> r

callCCT :: (K r a -> Cont r a) -> Cont r a
callCCT f = Cont $ \k -> unCont (f k) k

throwT :: K r a -> a -> Cont r b
throwT k x = Cont $ \ _ -> k x

-- And here is the main difference from Cont: the higher-rank type of
-- runCont, ensuring that the computation should really not have looked
-- at the answer-type
runContT :: Monad m => (forall w. Cont (m w) a) -> m a
runContT m = unCont m return

runContT :: Monad m => (forall w. Cont (m w) a) -> m a
runContT m = unCont m return

-- -- instance MonadTrans (ContT r) where
-- --     lift m = ContT (m >>=)

-- -- instance (MonadIO m) => MonadIO (ContT r m) where
-- --     liftIO = lift . liftIO


-- -- type Cont w = ContT w Identity

-- -- type K w a = a -> w

-- -- callCC :: (K w a -> Cont w a) -> Cont w a
-- -- callCC f = Cont $ \k -> unCont (f k) k

-- -- throw :: K w a -> a -> Cont w b
-- -- throw k x = Cont $ \ _ -> k x

-- -- type ExtContT m a = forall w . ContT w m a
