-- from http://okmij.org/ftp/continuations/undelimited.html

{-# LANGUAGE RankNTypes #-}

module Cont where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad (ap, liftM)

import Control.Monad.Identity

newtype ContM w m a = ContM { unContM :: (a -> m w) -> m w }
type ContM' m a = forall w . ContM w m a

instance Monad (ContM w m) where
    return x = ContM $ \k -> k x
    m >>= f  = ContM $ \k -> unContM m (\mv -> unContM (f mv) k)

instance Applicative (ContM w m) where
  pure = return
  (<*>) = ap

instance Functor (ContM w m) where
  fmap = liftM


-- The type of the captured continuation
-- Once again, it is not a function in the object language;
-- it's result is NOT of the type Cont w x
type K w m a = a -> m w

callCC :: (K w m a -> ContM w m a) -> ContM w m a
callCC f = ContM $ \k -> unContM (f k) k

throw :: K w m a -> a -> ContM w m b
throw k x = ContM $ \ _ -> k x

-- And here is the main difference from Cont: the higher-rank type of
-- runCont, ensuring that the computation should really not have looked
-- at the answer-type
runContM :: Monad m => ContM' m a -> m a
runContM m = unContM m return

instance MonadTrans (ContM r) where
    lift m = ContM (m >>=)

instance (MonadIO m) => MonadIO (ContM r m) where
    liftIO = lift . liftIO

type Cont w a = ContM w Identity a
type Cont' a = forall w . Cont w a

runCont :: Cont' a -> a
runCont x = runIdentity $ runContM x
