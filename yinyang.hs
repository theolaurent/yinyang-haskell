{-# LANGUAGE RankNTypes #-}

-- http://okmij.org/ftp/continuations/
-- https://en.wikipedia.org/wiki/Call-with-current-continuation


-- TODO: https://hackage.haskell.org/package/CC-delcont-0.2.1.0/docs/Control-Monad-CC.html ?

-- TODO: check for recursive types in haskell
-- TODO: or try the same workaround as Y combinator with in/out?

import Cont
import Control.Monad.IO.Class (liftIO)

-- term3c :: Cont w Int -> Cont w String
-- term3c x = do
--   v1 <- callCC (\p ->
--           do
--           v1 <- x
--           v2 <- if v1 == 0 then throw p '-' else return (10 `div` v1)
--           if v2 > 5 then return '5' else return '0')
--   return $ "Result: " ++ [v1]



-- test :: (forall w . Cont w ())
-- test = do
--   _ <- callCC (\ k -> throw k (Left (\ x -> throw k (Right x))))
--   return ()


-- I am clearly relying on internals of Cont module
newtype RecK w m = RecK { unRecK :: KT w m (RecK w m) }

loopTest :: ExtContT IO ()
loopTest = do
  -- _ <- callCCT (\ k -> throwT k (Left (\ x -> throwT k (Right x))))
  k <- callCCT (\k -> return $ RecK k)
  liftIO $ print "test"
  throwT (unRecK k) k

main :: IO ()
main = runContT loopTest
