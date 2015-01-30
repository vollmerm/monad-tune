
{- |
Copyright    : 2015 Mike Vollmer
License      : BSD2

Search strategies that cooperate with TunerT.
-}

module Control.Monad.Tune.Search (
  crossover,
  mutate
  ) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Tune
import qualified Data.Map             as M

crossover :: (MonadRandom m) => TunerState -> TunerState -> m TunerState
crossover s1 s2 = undefined

mutate :: (MonadRandom m, Ord a, Fractional a, Random a) =>
          a -> TunerState -> m TunerState
mutate c s = do
  let ls = M.assocs $ choices s
  ls' <- mapM (mutateVal c s) ls
  let choices' = M.fromList ls'
  return $ s { choices = choices' }

mutateVal
  :: (MonadRandom m, Random a, Ord a, Fractional a) =>
     a -> TunerState -> (Name, Decision) -> m (Name, Decision)
mutateVal c s (k,v) = do
  chance <- coinFlip c
  let d = getDomain k s
  r <- getRandomR (0, length d - 1)
  if chance && hasDependent k s then return (k, d !! r) else return (k,v)

coinFlip :: (MonadRandom m, Random a, Ord a, Fractional a) => a -> m Bool
coinFlip c = do
  c' <- getRandomR (0.0, 1.0)
  return $ c < c'

hasChoice :: Name -> TunerState -> Bool
hasChoice n s = M.member n $ choices s

hasDependent :: Name -> TunerState -> Bool
hasDependent n s = case M.lookup n $ env s of
  Nothing -> error "Choice not found in env!"
  Just c -> case parent c of
    Nothing -> True
    Just p -> hasChoice (name p) s

getDomain :: Name -> TunerState -> Domain
getDomain n s = case M.lookup n $ env s of
  Nothing -> error "Domain cannot be found!"
  Just c -> domain c
