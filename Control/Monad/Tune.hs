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

  -- ^ The monad transformer, plus plain monad for convenience.
  Tune, TuneT,

  -- ^ Computation inside the monad uses makeChoice and setEval.
  makeChoice,
  makeChoiceDepends,
  setEval,

  -- ^ Helper functions.
  remChoice,
  remDependents,
  hasChoice,
  hasParent,
  getDomain,
  getChoice,
  addChoiceRoot,
  addChoiceDepends,
  makeTunerState,

  -- ^ Running the tuner monad.
  runTuneT,
  runTune,

  -- ^ Data structures and type aliases for using TunerT.
  TunerState(..), TunerChoice(..),
  Name, Score, Decision, Domain
  ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (isJust, fromJust)
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
  name   :: Name , -- ^ The name associated with the choice
  domain :: Domain , -- ^ Values we have to choose from for this choice
  parent :: Maybe TunerChoice -- ^ Might depend on another choice
  } deriving (Show)

-- | The search state has a map of all choices made so far, a final evaluation,
-- an environment of all possible choices, and the current random number
-- generator.

data TunerState = TunerState {
  choices    :: Map Name Decision ,
  evaluation :: Maybe Score ,
  env        :: Map Name TunerChoice ,
  rnd        :: StdGen
  } deriving (Show)

-- | Three functions are defined in the MonadTune typeclass. Two grab a
-- decision, either from the map of decisions already made or from the
-- random generator, and the other sets the final evaluation.

class (Monad m) => MonadTune m where
  makeChoice ::
    Name -> Domain -> m Decision -- ^ Returns a decision
  makeChoiceDepends ::
    Name -> Domain -> Name -> m Decision -- ^ Returns a decision
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
  makeChoice n d =
    TuneT $ liftState (choose n d Nothing)
  makeChoiceDepends n d p =
    TuneT $ liftState (choose n d $ Just p)  
  setEval i = TuneT $ liftState (saveScore i)

liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t =
  do v <- get
     let (x, v') = t v
     put v'
     return x

runTuneT :: (Monad m) => TuneT m a -> TunerState -> m (a, TunerState)
runTuneT (TuneT x) = runStateT x

runTune :: Tune a -> TunerState -> (a, TunerState)
runTune (Tune x) = runIdentity . runTuneT x

addChoiceRoot :: Map Name TunerChoice
                 -> String -> Domain -> Map Name TunerChoice
addChoiceRoot m name domain =
  M.insert name
  (TunerChoice name domain Nothing) m

addChoiceDepends :: Map Name TunerChoice
                    -> String -> Domain -> TunerChoice
                    -> Map Name TunerChoice
addChoiceDepends m name domain parent =
  M.insert name
  (TunerChoice name domain $ Just parent) m

makeTunerState :: Map Name TunerChoice -> StdGen -> TunerState
makeTunerState = TunerState M.empty Nothing

-- | This is extremely inefficient! Ideally we shouldn't have to
-- walk the domain to pick an element out of it. This should be
-- refactored to be smarter.

randElem :: (RandomGen g) => [a] -> g -> (a, g)
randElem s g = (s !! n, g')
    where (n, g') = randomR (0, length s - 1) g

-- | This is where the magic happens! Or, one of the places anyway.
-- This function makes a "choice" by returning a value corresponding
-- to an auto-tuned parameter. This may mean looking up an already
-- determined choice, or extending the search space with a new decision
-- point. Note that this function may extend the env arbitrarily!

choose :: Name -> Domain -> Maybe Name -> TunerState ->
          (Decision, TunerState)
choose n d p s =
  case M.lookup n $ choices s of
    -- Two cases:
    --   * n is in state: Either we already made this choice, or
    --     it was put in the state before the start by a search strategy.
    Just e  -> (e, s)
    --   * n is not in state: We need to generate a random value and
    --     save it in the choices map under this name.    
    Nothing -> let g = rnd s
                   -- pick a random element from the domain
                   (e,g') = randElem d g
                   -- determine its parent object
                   p' = case p of
                     Nothing -> Nothing
                     Just np -> M.lookup np $ env s
               in (e, s { choices = M.insert n e $ choices s ,
                          rnd = g' ,
                          -- do we need to extend the env?
                          env = case M.lookup n $ env s of
                            Nothing -> M.insert n
                                       (TunerChoice n d p')
                                       $ env s
                            -- for sanity's sake we should check that
                            -- the domain passed in matches the existing
                            -- one in the env
                            Just c  -> if domain c /= d
                                       then error "Domain mismatch!"
                                       else env s
                        })

-- | Save the evaluation score to the state.

saveScore :: Score -> TunerState -> (Score,TunerState)
saveScore i s = (i, s { evaluation = Just i })

-- | Helper functions

hasChoice :: TunerState -> Name -> Bool
hasChoice s n = M.member n $ choices s

hasParent :: TunerChoice -> Bool
hasParent c = isJust $ parent c

getDomain :: TunerState -> Name -> Domain
getDomain s n = domain $ getChoice s n

getChoice :: TunerState -> Name -> TunerChoice
getChoice s n = fromJust $ M.lookup n $ env s

remChoice :: TunerState -> Name -> TunerState
remChoice s n = s { choices = choices', env = env' }
  where env' = M.delete n $ env s
        choices' = M.delete n $ choices s

remDependents :: TunerState -> Name -> TunerState
remDependents s n = case parent $ getChoice s n of
  Nothing -> s
  Just p  -> remChoice s' n'
    where n' = name p
          s' = remDependents s n'
