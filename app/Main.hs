module Main where

import Lib
import Control.Monad
import Control.Concurrent
import System.Console.ANSI

main :: IO ()
main = let screen = ProjectionScreen {
             xResolution = 40,
             yResolution = 40,
             screenDirection = Ray { rayOrigin=origin, rayDirection=unitY},
             toLeftEdge = scaleV unitX (-1),
             toTopEdge = unitZ
           }
           seedWorld = World [farTriangle]
           farTriangle = Triangle (makePoint (-20) 30 0.5) (makePoint (-10) 30 10) (makePoint 0 30 0.5)
           worlds = map (renderWorld screen) $ iterate updateWorld seedWorld
        in
          forM_ worlds (\w -> clearScreen >> (forM_ w putStrLn) >> threadDelay 50000)



updateWorld :: World -> World
updateWorld (World triangles) = World $ fmap go triangles
  where go :: Triangle -> Triangle
        go t = fmap (pushBack . (rotateAroundCorner t)) t
        pushBack p = translateP p (Vector 0 0.1 0)
        rotateAroundCorner (Triangle _ _ c) p = rotateAround p (Ray c (Vector 0 0 1)) (Radians 0.1)

