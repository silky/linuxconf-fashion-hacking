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


bars :: Diagram B
bars = vsep 0.5 (replicate 3 $ rect 10 1)
        # centerXY
        # scaleY 0.5


dropShadow :: Diagram B -> Diagram B
dropShadow d = d <> d # (translateX 0.4)
                      # (translateY (-0.4))
                      # fc black


doTransform :: Diagram B -> IO (Diagram B)
doTransform d = do
  -- Scale
  s <- randomRIO (0.3, 1.2)

  -- Rotation
  r <- randomRIO (0, 360)

  -- Colour
  c <- randomRIO (0, len)


  return $ d # scale s
             # rotateBy r
             # fc (colours !! c)
             # lw none
    where
      colours = brewerSet Set3 12
      len     = length colours - 1


d :: IO (Diagram B)
d = do
  let items = [ rect 8 1
              , circle 3
              , triangle 3
              , bars
              , haskellLogo
              ]

  -- let diags = map (fc blue . dropShadow) items
  diags <- mapM (doTransform . dropShadow) items
  
  return $ hcat diags
































s = sRGB24read

p angles coords =
    polygon
        ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides angles coords
        )
        # lw none
        # lc black


-- d :: Diagram B
-- d = (left # alignT # snugR <> right # alignT # snugL)
--         # bg (s "#22274c")
--     where
--         left  = section (s "#37657a") (s "#2f6baf") (s "#5aafda") (s "#fdde4b")
--         right = section (s "#c9452e") (s "#81313a") (s "#d67357") (s "#e09ea6")
--                     # reflectY
--                     # reflectX

--         section c1 c2 c3 c4 = t1' c1 c2 c3 <> t4 c4

--         -- the green thing and the blue/blue thing together
--         t1' c1 c2 c3 = (t1 c1 <> t3' c2 c3)
--                         # alignL
--                         # snugB

--         -- bottom yellow thing
--         t4 c = p [ 90 @@ deg , 270 @@ deg , 270 @@ deg     ]
--                [ 0         , 4.6        ,  2.7         , 4 ]
--                # fc c
--                # alignL
--                # snugT

--         a1 = (atan (0.4/4) * (180 / pi))
--         d1 = (sqrt ( 4 ** 2 + 0.4 ** 2 )) / 2 

--         -- combined blue/blue thing
--         t3' c1 c2 = (t3_1 c1 <> t3_2 c2)
--                      # center
--                      # snugT

--         -- triangle part of blue/blue thing
--         t3_1 c = p [ (270 + a1) @@ deg , 0 @@ deg ]
--                    [ 6.6                 , d1 ]
--                    # fc c
--                    # alignB
--                    # snugR

--         -- polygon part of blue/blue thing
--         t3_2 c = p [ 270 @@ deg , 90 @@ deg , (90 + a1) @@ deg     ]
--                    [ 0          , 4         , 7               , d1 ]
--                    # fc c
--                    # alignB
--                    # snugL

--         -- top green-ish thing
--         t1 c = p [ 90 @@ deg, 90 @@ deg, 90 @@ deg ]
--                  [ 0.8        , 4        , 1.2     ]
--                  # fc c
--                  # snugB































-- Demo code:

