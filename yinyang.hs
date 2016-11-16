
-- http://okmij.org/ftp/continuations/
-- https://en.wikipedia.org/wiki/Call-with-current-continuation


-- TODO: https://hackage.haskell.org/package/CC-delcont-0.2.1.0/docs/Control-Monad-CC.html ?

-- TODO: check for recursive types in haskell

import Cont
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity

import Data.Void (Void)

newtype RecK w m = RecK { unRecK :: KT w m (RecK w m) }


loopTest :: ContT w IO ()
loopTest = do
  k <- callCCT (\k -> return $ RecK k)
  liftIO $ print "test"
  throwT (unRecK k) k

yinyang :: ContT w IO ()
yinyang = do
  let yin :: ContT w IO (RecK w IO)
      yin = do
        k <- callCCT (\ k -> return $ RecK k)
        liftIO $ print "@"
        return k
  -- ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c)
  -- (\ cc -> print "@" ; cc) (callCC id)
  -- a -> a                  K (K (K ...))
  let yang :: ContT w IO (RecK w IO)
      yang = do
        k <- callCCT (\ k -> return $ RecK k)
        liftIO $ print "*"
        return k
  k1 <- yin
  k2 <- yang
  throwT (unRecK k1) k2


main :: IO ()
main = runContT yinyang
