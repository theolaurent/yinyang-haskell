-- from http://okmij.org/ftp/continuations/undelimited.html

{-# LANGUAGE RankNTypes #-}

module Cont where

import Control.Monad (ap, liftM)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Identity (Identity, runIdentity)

newtype ContM w m a = ContM { unContM :: (a -> m w) -> m w }
-- The higher-rank type ContM' ensures that the computation
-- should not look at the answer-type
type ContM' m a = forall w . ContM w m a

instance Monad (ContM w m) where
    return x = ContM $ \k -> k x
    m >>= f  = ContM $ \k -> unContM m (\mv -> unContM (f mv) k)

instance Applicative (ContM w m) where
  pure = return
  (<*>) = ap

instance Functor (ContM w m) where
  fmap = liftM


-- Type of captured continuations
-- It is not a function in the object language;
-- it's result is NOT of the type Cont w m x
type K w m a = a -> m w

callCC :: (K w m a -> ContM w m a) -> ContM w m a
callCC f = ContM $ \k -> unContM (f k) k

throw :: K w m a -> a -> ContM w m b
throw k x = ContM $ \ _ -> k x

runContM :: Monad m => ContM' m a -> m a
runContM m = unContM m return

-- Bringing IO to the Continuation Monad
instance MonadTrans (ContM r) where
    lift m = ContM (m >>=)

instance (MonadIO m) => MonadIO (ContM r m) where
    liftIO = lift . liftIO

-- Simple Continuations
type Cont w a = ContM w Identity a
type Cont' a = forall w . Cont w a

runCont :: Cont' a -> a
runCont x = runIdentity $ runContM x

-- excludedMiddle :: Cont' (Either (a -> Void) a)
-- excludedMiddle = do
--   let it :: Cont w (Either (a -> Cont w b) a)
--       it = callCC (\ k -> throw k (Left (\ x -> throw k (Right x))))
--   res <- it
--   case res of
--     Right a -> return $ Right a
--     Left  f -> undefined
-- To write this I would need the Falsum version and "dynamic check"
-- that is continuation that are not type-guaranteed not to escape their scope
-- (an alternative would be continuations that you can't run)
