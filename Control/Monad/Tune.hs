{-#LANGUAGE MultiParamTypeClasses, UndecidableInstances #-} 
{-#LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Control.Monad.Tune  (
  Tune, TuneT,
  makeChoice,
  setEval,
  runTuneT,
  runTune,
  addChoiceRoot,
  addChoiceDepends,
  makeTunerState,
  TunerState, TunerChoiceMap,
  Score, Decision, Domain
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import System.Random
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative

type Decision = Int
type Score = Int
type Domain = Set Decision
                     
data TunerChoice = TunerChoice {
  name :: String ,
  domain :: Domain ,
  parent :: Maybe TunerChoice
  } deriving (Show)

type TunerChoiceMap = Map String TunerChoice

data TunerState = TunerState { 
  choices :: Set (String, Decision) ,
  evaluation :: Maybe Score ,
  env :: TunerChoiceMap ,
  rnd :: StdGen
  } deriving (Show)

class (Monad m) => MonadTune m where
  makeChoice :: m a
  setEval :: Score -> m a
  
newtype TuneT m a = TuneT (StateT TunerState m a)
                  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
 
liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x

newtype Tune a = Tune (TuneT Identity a)
               deriving (Functor, Applicative, Monad, MonadTune)

instance (Monad m) => MonadTune (TuneT m) where
  makeChoice = TuneT $ liftState choose
  setEval s = TuneT $ liftState (saveScore s)

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
makeTunerState m r = TunerState S.empty Nothing m r

choose = undefined
saveScore = undefined
