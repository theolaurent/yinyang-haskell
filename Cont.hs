-- from http://okmij.org/ftp/continuations/undelimited.html

{-# LANGUAGE RankNTypes #-}

module Cont where

import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad (ap, liftM)

type ExtContT m a = forall w . ContT w m a

newtype ContT w m a = ContT { unContT :: (a -> m w) -> m w }

instance Monad (ContT w m) where
    return x = ContT $ \k -> k x
    m >>= f  = ContT $ \k -> unContT m (\mv -> unContT (f mv) k)

instance Applicative (ContT w m) where
  pure = return
  (<*>) = ap

instance Functor (ContT w m) where
  fmap = liftM


-- The type of the captured continuation
-- Once again, it is not a function in the object language;
-- it's result is NOT of the type Cont w x
type KT w m a = a -> m w

callCCT :: (KT w m a -> ContT w m a) -> ContT w m a
callCCT f = ContT $ \k -> unContT (f k) k

throwT :: KT w m a -> a -> ContT w m b
throwT k x = ContT $ \ _ -> k x

-- And here is the main difference from Cont: the higher-rank type of
-- runCont, ensuring that the computation should really not have looked
-- at the answer-type
runContT :: Monad m => (forall w. ContT w m a) -> m a
runContT m = unContT m return

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO


-- type Cont w = ContT w Identity

-- type K w a = a -> w

-- callCC :: (K w a -> Cont w a) -> Cont w a
-- callCC f = Cont $ \k -> unCont (f k) k

-- throw :: K w a -> a -> Cont w b
-- throw k x = Cont $ \ _ -> k x
