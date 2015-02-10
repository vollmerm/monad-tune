
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
  ls1 <- mapM (mutateVal c s) ls
  s   <- foldM fixDeps s ls1
  ls2 <- mapM (\(k,v,_) -> return (k,v)) ls1
  return $ s { choices = M.fromList ls2, evaluation = Nothing }
  where mutateVal c s (k,v) = 
          -- current limitation/hack:
          -- if not a root, don't mutate
          let choice = getChoice s k
          in if hasParent choice
             then return (k,v,False)
             else do 
               chance <- coinFlip c
               let d = getDomain s k
               r <- getRandomR (0, length d - 1)
               if chance
                 then return (k, d !! r, True)
                 else return (k, v, False)
        fixDeps s (_,_,False) = return s
        fixDeps s (k,v,True)  =
          let choice = getChoice s k
          in return $ remDependents s k

coinFlip :: (MonadRandom m, Random a, Ord a, Fractional a) => a -> m Bool
coinFlip c = do
  c' <- getRandomR (0.0, 1.0)
  return $ c < c'
