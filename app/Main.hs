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
             toLeftEdge = scaleV unitX (-0.5),
             toTopEdge = unitZ
           }
           seedWorld = World $ sphere
           farTriangle = Triangle (makePoint (-20) 30 0.5) (makePoint (-10) 30 10) (makePoint 0 30 0.5)
           sphere = makeSphere (makePoint 0 20 0) 5.0
           worlds = map (renderWorld screen) $ iterate updateWorld seedWorld
        in
          --forM_ worlds (\w -> clearScreen >> (forM_ w putStrLn) >> threadDelay 50000)
          forM_ (renderWorld screen seedWorld) putStrLn



updateWorld :: World -> World
updateWorld (World triangles) = World $ fmap go triangles
  where go :: Triangle -> Triangle
        go t = fmap (pushBack . (rotateAroundCorner t)) t
        pushBack p = translateP p (Vector 0 0.1 0)
        rotateAroundCorner (Triangle _ _ c) p = rotateAround p (Ray c (Vector 0 0 1)) (Radians 0.1)

makeSphere origin radius = concat [makeTriangles thetaPair phiPair | thetaPair <- thetaPairs, phiPair <- phiPairs]
  where thetaPairs = zip thetas $ tail thetas
        phiPairs = zip phis $ tail phis
        thetas = [0,0.5..pi]
        phis = [0,0.5..(2 * pi)]
        makeTriangles (thetaStart, thetaEnd) (phiStart, phiEnd) =
          [Triangle
            (getPoint thetaStart phiStart)
            (getPoint thetaStart phiEnd)
            (getPoint thetaEnd phiEnd),
           Triangle
            (getPoint thetaStart phiStart)
            (getPoint thetaStart phiEnd)
            (getPoint thetaEnd phiStart),
           Triangle
            (getPoint thetaEnd phiStart)
            (getPoint thetaEnd phiEnd)
            (getPoint thetaStart phiEnd),
           Triangle
            (getPoint thetaEnd phiStart)
            (getPoint thetaEnd phiEnd)
            (getPoint thetaStart phiStart)
           ]
        getPoint theta phi = translateP origin $ getDelta theta phi
        getDelta theta phi = Vector x y z
          where x = radius * (cos theta) * (sin phi)
                y = radius * (sin theta) * (sin phi)
                z = radius * (cos phi)


