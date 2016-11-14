{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, EmptyDataDecls #-}

-- Various encodings of the CPS monad, of _undelimited_ continuations

module CPSs where

import Control.Monad
import Control.Monad.ST
import Data.STRef

-- ------------------------------------------------------------------------
-- First encoding, with the polymorphic answer-type
-- It doesn't quite work

newtype CPS1 a = CPS1{unCPS1:: forall w. (a -> w) -> w}

instance Monad CPS1 where
    return x = CPS1 $ \k -> k x
    m >>= f  = CPS1 $ \k -> unCPS1 m (\mv -> unCPS1 (f mv) k)


runCPS1 :: CPS1 a -> a
runCPS1 m = (unCPS1 m) id

-- A sample term
term1 = do
	x <- return 1
	y <- return 2
	return (x + y)

term1_cps1 = runCPS1 term1
-- 3

-- The type of the captured continuation
-- The type is different from (a -> Cont b):
-- continuations are not functions, in the object language
type K1 a = forall w. a -> w

-- We can write throw

throw1 :: K1 a -> a -> CPS1 b
throw1 k x = CPS1 $ \_ -> k x

-- But we can't write callCC, since K1 is _co_variant

{-
callCC1 :: (K1 a -> CPS1 a) -> CPS1 a
callCC1 f = CPS1 $ \k -> unCPS1 (f k) k

    Couldn't match expected type `w1' against inferred type `w'
      `w1' is a rigid type variable bound by
           the polymorphic type `forall w1. a -> w1'
      `w' is a rigid type variable bound by
          the polymorphic type `forall w. (a1 -> w) -> w'
    In the first argument of `f', namely `k'
    In the first argument of `unCPS1', namely `(f k)'
    In the expression: unCPS1 (f k) k
-}

-- term5_cps1 = liftM (2 +) $ callCC1 (\p -> liftM (3 +) $ throw1 p 4)


-- ------------------------------------------------------------------------
-- Second encoding, using Falsum

data Falsum				-- no constructors

newtype CPS2 a = CPS2{unCPS2:: (a -> Falsum) -> Falsum}

instance Monad CPS2 where
    return x = CPS2 $ \k -> k x
    m >>= f  = CPS2 $ \k -> unCPS2 m (\mv -> unCPS2 (f mv) k)


-- The type of the captured continuation
-- Again, continuations are not functions, _in the object language_
type K2 a = a -> Falsum

-- Now we can write callCC and throw
callCC2 :: (K2 a -> CPS2 a) -> CPS2 a
callCC2 f = CPS2 $ \k -> unCPS2 (f k) k

throw2 :: K2 a -> a -> CPS2 b
throw2 k x = CPS2 $ \ k_ignored -> k x

-- and use them
term2 :: CPS2 Int -> CPS2 String
term2 x = do
  v1 <- callCC2 (\p -> 
          do
          v1 <- x
          v2 <- if v1 == 0 then throw2 p '-' else return (10 `div` v1)
          if v2 > 5 then return '5' else return '0')
  return $ "Result: " ++ [v1]

-- Alas, we cannot write the run operation
-- We need the top level continuation a -> Falsum. Only OS can give us
-- that, or the run-time system
-- runCPS2 :: CPS2 a -> a
-- runCPS2 m = (unCPS2 m) id


-- We can cheat, using the `side-channel' to get the result
-- We introduce the monad to get the side channel
newtype CPS3 m a = CPS3{unCPS3:: (a -> m Falsum) -> m Falsum}

-- remain the same, modulo CPS2 -> CPS3 renaming
instance Monad (CPS3 m) where
    return x = CPS3 $ \k -> k x
    m >>= f  = CPS3 $ \k -> unCPS3 m (\mv -> unCPS3 (f mv) k)

-- remain the same, modulo CPS2 -> CPS3 m renaming

type K3 m a = a -> m Falsum

callCC3 :: (K3 m a -> CPS3 m a) -> CPS3 m a
callCC3 f = CPS3 $ \k -> unCPS3 (f k) k

throw3 :: K3 m a -> a -> CPS3 m b
throw3 k x = CPS3 $ \ k_ignored -> k x

term3 :: CPS3 m Int -> CPS3 m String
term3 x = do
  v1 <- callCC3 (\p -> 
          do
          v1 <- x
          v2 <- if v1 == 0 then throw3 p '-' else return (10 `div` v1)
          if v2 > 5 then return '5' else return '0')
  return $ "Result: " ++ [v1]


-- Major machinations; lots of apparent partiality
-- Instead of ST s as m we could have use Either w.
runCPS3 :: (forall m. CPS3 m a) -> a
runCPS3 m = runST (do
  res <- newSTRef (error "Continuation has escaped!")
  unCPS3 m (\v -> writeSTRef res v >> return undefined)
  readSTRef res)

term1_cps3 = runCPS3 term1
-- 3

term31_cps3 = runCPS3 (term3 term1)
-- "Result: 0"

term32_cps3 = runCPS3 (term3 (return 0))
-- "Result: -"

-- more examples
term4 = callCC3 (\p -> return (\a -> throw3 p (\x -> return True)))
-- Cannot run it! The type-checker prevents stupid things
-- term4_cps3 = runCPS3 term4

-- Here, term4 returns twice
term4' = term4 >>= \f -> f 1
term4'_cps3 = runCPS3 term4'
-- True

term5 = liftM (2 +) $ callCC3 (\p -> liftM (3 +) $ throw3 p 4)

term5_cps3 = runCPS3 term5
-- 6

-- ------------------------------------------------------------------------
-- Third encoding, fixed but unknown answer-type
-- The computation proper cannot look at it.
-- Except the run function, it is the same as the Cont monad in MTL

-- CPS3 looks suspiciously like Cont in the MTL; the side-channel
-- monad m playing the role of the answer-type w. Neither should be looked
-- upon by computations.

newtype Cont w a = Cont{unCont:: (a -> w) -> w}

-- as before, modulo (CPS3 m) to (Cont w) renaming
instance Monad (Cont w) where
    return x = Cont $ \k -> k x
    m >>= f  = Cont $ \k -> unCont m (\mv -> unCont (f mv) k)


-- The type of the captured continuation
-- Once again, it is not a function in the object language;
-- it's result is NOT of the type Cont w x
type K w a = a -> w

-- The same as callCC3
callCC :: (K w a -> Cont w a) -> Cont w a
callCC f = Cont $ \k -> unCont (f k) k

throw :: K w a -> a -> Cont w b
throw k x = Cont $ \ k_ignored -> k x

term3c :: Cont w Int -> Cont w String
term3c x = do
  v1 <- callCC (\p -> 
          do
          v1 <- x
          v2 <- if v1 == 0 then throw p '-' else return (10 `div` v1)
          if v2 > 5 then return '5' else return '0')
  return $ "Result: " ++ [v1]

-- And here is the main difference from Cont: the higher-rank type of
-- runCont, ensuring that the computation should really not have looked
-- at the answer-type
runCont :: (forall w. Cont w a) -> a
runCont m = unCont m id

term1_cont = runCont term1
-- 3

term31_cont = runCont (term3c term1)
-- "Result: 0"

term32_cont = runCont (term3c (return 0))
-- "Result: -"

-- More examples
term4c = callCC (\p -> return (\a -> throw p (\x -> return True)))
-- Cannot run it! Because we have looked at the answer-type
-- The type-checker prevents stupid things
-- term4_cont = runCont term4c

-- Here, term4 is called once but returns twice
term4'c = term4c >>= \f -> f 1
term4'_cont = runCont term4'c
-- True

term5c = liftM (2 +) $ callCC (\p -> liftM (3 +) $ throw p 4)

term5_cont = runCont term5c
-- 6

