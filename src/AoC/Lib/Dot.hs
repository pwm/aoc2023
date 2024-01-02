{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC.Lib.Dot where

import AoC.Lib.Prelude
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL

data DotG v = DotG
  { vertices :: [v],
    edges :: [(v, v)]
  }
  deriving stock (Show, Eq, Ord, Generic)

data Directed = Undirected | Directed
  deriving stock (Show, Eq, Ord, Generic)

ddd :: (PrintDotRepr dg v) => String -> dg v -> IO ()
ddd path = T.writeFile path . ppd

ppd :: (PrintDotRepr dg v) => dg v -> Text
ppd = TL.toStrict . printDotGraph

toDotGraph :: (Ord v) => Directed -> DotG v -> DotGraph v
toDotGraph = toDotGraphLbl @_ @Text @Text Nothing Nothing

toDotGraphLbl ::
  forall v vl el.
  (Ord v, Labellable vl, Labellable el) =>
  Maybe (v -> vl) ->
  Maybe ((v, v) -> el) ->
  Directed ->
  DotG v ->
  DotGraph v
toDotGraphLbl lblrV lblrE dir dotG =
  graphElemsToDot
    nonClusteredParams
      { isDirected = case dir of Undirected -> False; Directed -> True,
        fmtNode = case lblrV of
          Nothing -> const []
          Just lblr -> \(v, _) -> [toLabel (lblr v)],
        fmtEdge = case lblrE of
          Nothing -> const []
          Just lblr -> \(v1, v2, _) -> [toLabel (lblr (v1, v2))]
      }
    (map (\v -> (v, ())) dotG.vertices)
    (map (\(v1, v2) -> (v1, v2, ())) dotG.edges)

instance (PrintDot a, PrintDot b) => PrintDot (a, b) where
  unqtDot :: (a, b) -> DotCode
  unqtDot (a, b) = dquotes $ text "(" <> commaDel a b <> text ")"
