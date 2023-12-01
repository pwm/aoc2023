module AoC.Tester where

import AoC.Core.Date (Date)
import AoC.Core.File
import Data.Maybe (isJust)
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

testerPending ::
  forall a b c.
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Date ->
  ParseTest a ->
  RunTest a b ->
  RunTest a c ->
  Spec
testerPending (Left s) _ _ _ = it "is an invalid year" $ expectationFailure s
testerPending (Right date) p sa sb = do
  beforeAll (readInput date) $ do
    it ("Parse " <> inputName date) $ \i -> case p of
      ParsePending -> pending
      Parse parse -> parse i `shouldSatisfy` isJust
    it "Solve A" $ \i -> case sa of
      RunPending -> pending
      Run parse (solveA, solA) -> solveA <$> parse i `shouldBe` Just solA
    it "Solve B" $ \i -> case sb of
      RunPending -> pending
      Run parse (solveB, solB) -> solveB <$> parse i `shouldBe` Just solB

parsePending :: ParseTest ()
parsePending = ParsePending

runPending :: RunTest a ()
runPending = RunPending

data ParseTest a
  = ParsePending
  | Parse (String -> Maybe a)

data RunTest a b
  = RunPending
  | Run (String -> Maybe a) (a -> b, b)
