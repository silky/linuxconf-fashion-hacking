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


bars :: Diagram B
bars = vsep 0.5 (replicate 3 (rect 10 1))
          # centerXY
          # scaleY 0.5


dropShadow :: Diagram B -> Diagram B
dropShadow d = d <> d # translateX 0.4
                      # translateY (-0.4)
                      # fc black

doTransform :: Diagram B -> IO (Diagram B)
doTransform d = do
  s <- randomRIO (0.3, 1.2)
  r <- randomRIO (0,   360)
  c <- randomRIO (0, len)

  return $ d # scale s
             # rotateBy r
             # fc (colours !! c)
  where
    colours = brewerSet Set3 12
    len = length colours - 1


d :: IO (Diagram B)
d = do
  let items = [ circle 3
              , triangle 2
              , bars
              , haskellLogo
              ]

  xs <- mapM (doTransform . dropShadow) items
  return $ hcat xs
  -- return $ hcat (map (fc blue . dropShadow) items)


























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

