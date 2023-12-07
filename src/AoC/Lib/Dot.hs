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

ddd :: (Ord a, PrintDot a) => String -> DotG a -> IO ()
ddd path = T.writeFile path . ppd

ppd :: (Ord n, PrintDot n) => DotG n -> Text
ppd = TL.toStrict . printDotGraph . toDotGraph

toDotGraph :: (Ord a) => DotG a -> DotGraph a
toDotGraph g =
  graphElemsToDot
    nonClusteredParams
    (map (\n -> (n, ())) g.vertices)
    (map (\(f, t) -> (f, t, ())) g.edges)
