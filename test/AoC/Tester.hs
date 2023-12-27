module AoC.Tester where

import AoC.Core.Date (Date)
import AoC.Core.File
import Data.Maybe (fromJust, isJust)
import Test.Hspec
import Prelude

tester ::
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Date ->
  (String -> Maybe a) ->
  (a -> b, b) ->
  (a -> c, c) ->
  Spec
tester (Left s) _ _ _ = it "is an invalid year" $ expectationFailure s
tester (Right date) parse (solveA, solA) (solveB, solB) = do
  beforeAll (readInput date) $ do
    it ("Parse " <> inputName date) $ \i -> do
      parse i `shouldSatisfy` isJust
    it ("Solve A - should be " <> show solA) $ \i -> do
      solveA <$> parse i `shouldBe` Just solA
    it ("Solve B - should be " <> show solB) $ \i -> do
      solveB <$> parse i `shouldBe` Just solB

iotester ::
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Date ->
  (String -> Maybe a) ->
  (a -> IO b, b) ->
  (a -> IO c, c) ->
  Spec
iotester (Left s) _ _ _ = it "is an invalid year" $ expectationFailure s
iotester (Right date) parse (solveA, solA) (solveB, solB) = do
  beforeAll (readInput date) $ do
    it ("Parse " <> inputName date) $ \i -> do
      parse i `shouldSatisfy` isJust
    it ("Solve A - should be " <> show solA) $ \i -> do
      a <- solveA (fromJust $ parse i)
      a `shouldBe` solA
    it ("Solve B - should be " <> show solB) $ \i -> do
      b <- solveB (fromJust $ parse i)
      b `shouldBe` solB
