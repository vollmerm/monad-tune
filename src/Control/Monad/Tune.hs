{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
Copyright    : 2015 Mike Vollmer
License      : BSD2

A first attempt at an auto-tuning monad(-transformer).

See: <https://github.com/vollmerm/monad-tune>
-}

module Control.Monad.Tune  (

  -- * The monad transformer, plus plain monad for convenience.
  Tune, TuneT,

  -- * Computation inside the monad uses makeChoice and setEval.
  makeChoice,
  setEval,

  runTuneT,
  runTune,
  addChoiceRoot,
  addChoiceDepends,
  makeTunerState,

  -- * Data structures and type aliases for using TunerT.
  TunerState(..), TunerChoice(..),
  Name, Score, Decision, Domain
  ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as M
import           System.Random

-- | These are some aliases I made to keep the types straight.
-- For this to be properly general they probably should be type parameters
-- on the functions and datastructures below, but I decided on String, Int,
-- and lists as resonable defaults.
type Name = String
type Decision = Int
type Score = Int
type Domain = [Decision]

-- | A particular potential choice has a name, a domain, and possibly a parent.
data TunerChoice = TunerChoice {
  name   :: Name ,
  domain :: Domain ,
  parent :: Maybe TunerChoice -- ^ Might depend on another choice
  } deriving (Show)

type TunerChoiceMap = Map Name TunerChoice

-- | The search state has a map of all choices made so far, a final evaluation,
-- an environment of all possible choices, and the current random number
-- generator.
data TunerState = TunerState {
  choices    :: Map Name Decision ,
  evaluation :: Maybe Score ,
  env        :: TunerChoiceMap ,
  rnd        :: StdGen
  } deriving (Show)

-- | Two functions are defined in the MonadTune typeclass. One grabs a
-- decision, either from the map of decisions already made or from the
-- random generator, and the other sets the final evaluation.
class (Monad m) => MonadTune m where
  makeChoice :: Name -> m Decision -- ^ Returns a decision
  setEval :: Score -> m Score -- ^ Sets the eval score

-- | The auto-tuning monad transformer
newtype TuneT m a = TuneT (StateT TunerState m a)
                  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Apply the above transfromer to Identity to get the plain Tune monad
newtype Tune a = Tune (TuneT Identity a)
               deriving (Functor, Applicative, Monad)

-- | The real work in the monad is done by StateT, so we lift the
-- choose and saveScore functions into TuneT.
instance (Monad m) => MonadTune (TuneT m) where
  makeChoice n = TuneT $ liftState (choose n)
  setEval i = TuneT $ liftState (saveScore i)

liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x

runTuneT :: (Monad m) => TuneT m a -> TunerState -> m (a, TunerState)
runTuneT (TuneT x) s = runStateT x s

runTune :: Tune a -> TunerState -> (a, TunerState)
runTune (Tune x) s = runIdentity (runTuneT x s)

addChoiceRoot :: TunerChoiceMap
                 -> String -> Domain -> TunerChoiceMap
addChoiceRoot m name domain =
  M.insert name
  (TunerChoice name domain Nothing) m

addChoiceDepends :: TunerChoiceMap
                    -> String -> Domain -> TunerChoice
                    -> TunerChoiceMap
addChoiceDepends m name domain parent =
  M.insert name
  (TunerChoice name domain $ Just parent) m

makeTunerState :: TunerChoiceMap -> StdGen -> TunerState
makeTunerState m r = TunerState M.empty Nothing m r

-- | This is extremely inefficient! Ideally we shouldn't have to
-- walk the domain to pick an element out of it. This should be
-- refactored to be smarter.
randElem :: (RandomGen g) => [a] -> g -> (a, g)
randElem s g = (s !! n, g')
    where (n, g') = randomR (0, length s - 1) g

choose n s =
  case (M.lookup n $ choices s) of
    -- Two cases:
    --   * n is in state: Either we already made this choice, or
    --     it was put in the state before the start by a search strategy.
    --   * n is not in state: We need to generate a random value and
    --     save it in the choices map under this name.
    Just e  -> (e, s { choices = M.insert n e $ choices s })
    Nothing -> let g = rnd s
                   se = case M.lookup n $ env s of
                     Just c -> domain c
                     Nothing -> error "Name of choice not found in map!"
                   (e,g') = randElem se g
               in (e, s { choices = M.insert n e $ choices s , rnd = g' })

saveScore i s = (i, s { evaluation = Just i })
