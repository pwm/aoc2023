{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module AoC.Lib.Dot where

import AoC.Lib.Prelude
import Data.GraphViz
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL

data DotG a = DotG
  { vertices :: [a],
    edges :: [(a, a)]
  }
  deriving stock (Show, Eq, Ord, Generic)

data Directed = Undirected | Directed
  deriving stock (Show, Eq, Ord, Generic)

ddd :: (Ord a, PrintDot a) => String -> Directed -> DotG a -> IO ()
ddd path dir = T.writeFile path . ppd dir

ppd :: (Ord n, PrintDot n) => Directed -> DotG n -> Text
ppd dir = TL.toStrict . printDotGraph . toDotGraph dir

toDotGraph :: (Ord a) => Directed -> DotG a -> DotGraph a
toDotGraph dir g =
  graphElemsToDot
    nonClusteredParams {isDirected = case dir of Undirected -> False; Directed -> True}
    (map (\n -> (n, ())) g.vertices)
    (map (\(f, t) -> (f, t, ())) g.edges)
