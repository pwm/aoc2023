{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module AoC.Lib.Dot where

import Data.GraphViz
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Prelude

data DotG a = DotG
  { vertices :: [a],
    edges :: [(a, a)]
  }
  deriving stock (Show, Eq, Ord)

ddg :: (Ord a, PrintDot a) => String -> DotG a -> IO ()
ddg name =
  T.writeFile (".local/" <> name <> ".dot")
    . TL.toStrict
    . printDotGraph
    . toDotGraph

toDotGraph :: (Ord a) => DotG a -> DotGraph a
toDotGraph g =
  graphElemsToDot
    nonClusteredParams
    (map (\n -> (n, ())) g.vertices)
    (map (\(f, t) -> (f, t, ())) g.edges)
