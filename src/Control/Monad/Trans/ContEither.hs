{-# LANGUAGE RankNTypes #-}

module Control.Monad.Trans.ContEither
  ( ContEitherT(..)
  , ContEither(..)
  , left
  , right
  , liftE
  , catch
  , runContEither
  , toEither
  )
where

import           Control.Monad.Identity

-- Monad Transformer For Continuation Either.
-- Catches Errors throws exceptions.
-- Instead of simply letting them be identified by 
-- pattern matches.

newtype ContEitherT m l r = ContEitherT {
                                runContEitherT :: forall z.
                                                  (l -> m z)
                                               -> (r -> m z)
                                               -> m z
                            }

-- Execute the Left side of ContEither 
left :: l -> ContEitherT m l r
left l = ContEitherT $ \lk rk -> lk l


-- Execute the Right side of ContEither 
right :: r -> ContEitherT m l r
right r = ContEitherT $ \lk rk -> rk r

-- Lift a regular Either to a ContEither.
liftE :: Either l r -> ContEitherT m l r
liftE (Left  l) = left l
liftE (Right r) = right r

-- ContEither is a monad Transformer, it requires 
-- Instances declared for it.

-- An fmapped function operates only on the valid
-- cases, that will always exist on right side.
-- Thus f is composed with right sided function 
instance Functor (ContEitherT m l) where
  fmap f (ContEitherT cet) = ContEitherT $ \lk rk -> cet lk (rk . f)

instance Applicative (ContEitherT m l) where
  pure = right
  (ContEitherT lcet) <*> (ContEitherT rcet) =
    ContEitherT $ \lk rk -> lcet lk (\f -> rcet lk (rk . f))
  (ContEitherT lcet) *> (ContEitherT rcet) =
    ContEitherT $ \lk rk -> lcet lk (\_ -> rcet lk rk)
  (ContEitherT lcet) <* (ContEitherT rcet) =
    ContEitherT $ \lk rk -> lcet lk (\f -> rcet lk (rk . const f))


-- Any value in ContEitherT monad for successful return goes through
-- right side. (type signature matches as well)
instance Monad (ContEitherT m l) where
  return = right
  (ContEitherT cet) >>= f =
    ContEitherT $ \lk rk -> cet lk (\a -> runContEitherT (f a) lk rk)

type ContEither = ContEitherT Identity

runContEither :: ContEither l r -> (l -> z) -> (r -> z) -> z
runContEither ma lk rk =
  runIdentity $ runContEitherT ma (Identity . lk) (Identity . rk)

toEither :: ContEither l r -> Either l r
toEither ma = runContEither ma Left Right

catch :: ContEitherT m l r -> (l -> ContEitherT m l r) -> ContEitherT m l r
catch ma handler = ContEitherT
  $ \lk rk -> runContEitherT ma (\l -> runContEitherT (handler l) lk rk) rk
