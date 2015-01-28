module Control.Monad.TuneSpec where

import SpecHelper

import qualified Data.Map as M
import System.Random

spec :: Spec
spec = do
  describe "choiceMaps" $ do
    it "creates an environment with one root choice" $
      (M.size $ addChoiceRoot M.empty "a" [1]) `shouldBe` 1
    it "creates an environment with two root choices" $ 
      (M.size $ addChoiceRoot (addChoiceRoot M.empty "b" [1,2]) "a" [1]) `shouldBe` 2
    it "creates an environment with one root choice and one dependent choice" $
      let b    = TunerChoice "b" [1, 2] Nothing
          bmap = M.singleton "b" b
      in (M.size $ addChoiceDepends bmap "a" [1] b) `shouldBe` 2
  describe "tunerStates" $ do
    it "creates a tuner state with a singleton environment" $ do
      r <- getStdGen
      let m = M.singleton "a" $ TunerChoice "a" [1,2] Nothing
      (M.size $ env $ makeTunerState m r) `shouldBe` 1
      
