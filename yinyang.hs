
-- http://okmij.org/ftp/continuations/
-- https://en.wikipedia.org/wiki/Call-with-current-continuation


-- TODO: https://hackage.haskell.org/package/CC-delcont-0.2.1.0/docs/Control-Monad-CC.html ?

-- TODO: check for recursive types in haskell

import Cont
import Control.Monad.IO.Class (liftIO)

newtype RecK w m = RecK { unRecK :: K w m (RecK w m) }

yinyang :: ContM' IO ()
yinyang =
  let yin :: ContM w IO (RecK w IO)
      yin = do
        k <- callCC (\ k -> return $ RecK k)
        liftIO $ putChar '@'
        return k
      yang :: ContM w IO (RecK w IO)
      yang = do
        k <- callCC (\ k -> return $ RecK k)
        liftIO $ putChar '*'
        return k
  in do
    k1 <- yin
    k2 <- yang
    throw (unRecK k1) k2


main :: IO ()
main = runContM yinyang
