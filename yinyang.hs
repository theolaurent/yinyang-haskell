
-- http://okmij.org/ftp/continuations/
-- https://en.wikipedia.org/wiki/Call-with-current-continuation


-- TODO: https://hackage.haskell.org/package/CC-delcont-0.2.1.0/docs/Control-Monad-CC.html ?

-- TODO: check for recursive types in haskell

import Cont
import Control.Monad.IO.Class (liftIO)

-- import Data.Void (Void)

newtype RecK w m = RecK { unRecK :: K w m (RecK w m) }


-- excludedMiddle :: Cont' (Either (a -> Void) a)
-- excludedMiddle = do
--   let it :: Cont w (Either (a -> Cont w b) a)
--       it = callCC (\ k -> throw k (Left (\ x -> throw k (Right x))))
--   res <- it
--   case res of
--     Right a -> return $ Right a
--     Left  f -> undefined -- TODO: to write this I would need the Falsum version and "dynamic check"
--                          -- that is continuation that are not type-guaranteed not to escape their scope
--                          -- an alternative would be continuations that you can't run


yinyang :: ContM' IO ()
yinyang = do
  let yin :: ContM w IO (RecK w IO)
      yin = do
        k <- callCC (\ k -> return $ RecK k)
        liftIO $ print "@"
        return k
  -- ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c)
  -- (\ cc -> print "@" ; cc) (callCC id)
  -- a -> a                  K (K (K ...))
  let yang :: ContM w IO (RecK w IO)
      yang = do
        k <- callCC (\ k -> return $ RecK k)
        liftIO $ print "*"
        return k
  k1 <- yin
  k2 <- yang
  throw (unRecK k1) k2


main :: IO ()
main = runContM yinyang
