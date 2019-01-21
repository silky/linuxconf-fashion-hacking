{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import System.Random
import Data.Colour.Palette.BrewerSet
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Path.Boolean


main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


d :: IO (Diagram B)
d = do
  return $ mempty


























haskellPoints :: [[(Double, Double)]]
haskellPoints = [ -- >
                  [ (0, 3), (1.2, 1.5), (0,0) ]
                  -- \
                , [ (0.8, 3), (0.8 + (2 * 1.2), 0) ]
                  -- /
                , [ (0.8, 0), (0.8 + 1.2, 1.5) ]
                  -- =
                , [ (2.2, 1.85), (4, 1.85) ]
                , [ (2.7, 1.32), (4, 1.32) ]
                ]


haskellLogo :: Diagram B
haskellLogo = Path trails
                   # expandPath 0.2
                   # union Winding
                   # stroke
                   # centerXY
    where
        verts  = (map . map) p2 haskellPoints
        trails = map fromVertices verts 

