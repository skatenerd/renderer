module Lib
    ( tests
    ) where

import Data.Function
import Data.Maybe
import Control.Monad

data GenericPoint t = Point { pX :: t, pY :: t, pZ :: t } deriving (Show)
type Point = GenericPoint Float

data GenericVector t = Vector { vX :: t, vY :: t, vZ :: t } deriving (Show)
type Vector = GenericVector Float

data GenericTriangle t = Triangle (GenericPoint t) (GenericPoint t) (GenericPoint t) deriving (Show)
type Triangle = GenericTriangle Float

data GenericRay t = Ray { rayOrigin :: Point, normal :: Vector } deriving (Show)
type Ray = GenericRay Float

type Plane = Ray

type World = [Triangle]

data Segment  = Segment Point Point deriving (Show)

data ProjectionScreen = ProjectionScreen { xResolution :: Int, yResolution :: Int, screenDirection :: Ray, toLeftEdge :: Vector, toTopEdge :: Vector }

normalTraingleFace :: Triangle -> Vector
normalTraingleFace t@(Triangle a b c) =
  crossProduct (pointDifference b a) (pointDifference c a)

pointDifference :: Point -> Point -> Vector
pointDifference p1@(Point{pX = p1X, pY = p1Y, pZ = p1Z})
                p2@(Point{pX = p2X, pY = p2Y, pZ = p2Z})  =
  Vector {
    vX = (p1X - p2X),
    vY = (p1Y - p2Y),
    vZ = (p1Z - p2Z)
  }

vecToPoint :: Vector -> Point
vecToPoint v =
  Point (vX v) (vY v) (vZ v)

pointToVec :: Point -> Vector
pointToVec p =
  Vector (pX p) (pY p) (pZ p)

vectorDifference v1 v2 = pointDifference `on` vecToPoint

crossProduct :: Vector -> Vector -> Vector
crossProduct v1@(Vector{vX = v1X, vY = v1Y, vZ = v1Z})
             v2@(Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
  Vector {
    vX = (v1Y * v2Z - v1Z * v2Y),
    vY = (v1Z * v2X - v1X * v2Z),
    vZ = (v1X * v2Y - v1Y * v2X)
  }

dotProduct :: Vector -> Vector -> Float
dotProduct v1@(Vector{vX = v1X, vY = v1Y, vZ = v1Z})
           v2@(Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
    (v1X * v2X) + (v1Y * v2Y) + (v1Z * v2Z)

triangleToPlane :: Triangle -> Plane
triangleToPlane triangle@(Triangle a b c) = Ray a (normalTraingleFace triangle)

scaleV :: Vector -> Float -> Vector
scaleV v f =
  Vector {
    vX = (vX v) * f,
    vY = (vY v) * f,
    vZ = (vZ v) * f
  }

translateP :: Point -> Vector -> Point
translateP p v =
  Point {
    pX = (vX v) + (pX p),
    pY = (vY v) + (pY p),
    pZ = (vZ v) + (pZ p)
  }

translateV :: Vector -> Vector -> Vector
translateV v1 v2 =
  Vector {
    vX = (vX v1) + (vX v2),
    vY = (vY v1) + (vY v2),
    vZ = (vZ v1) + (vZ v2)
  }

rayIntersectPlane :: Ray -> Plane -> Maybe Point
rayIntersectPlane ray plane =
  if (isInfinite t)
     then Nothing
     else Just (translateP (rayOrigin ray) (scaleV (normal ray) t))
  where t = ((dotProduct (pointToVec (rayOrigin plane)) (normal plane)) - (dotProduct (pointToVec (rayOrigin ray)) (normal plane)))/ (dotProduct (normal ray) (normal plane))

sameSideOfSegment a b s@(Segment from to) = sameishDirection crossOne crossTwo
  where crossOne = crossProduct (pointDifference a from) (pointDifference to from)
        crossTwo = crossProduct (pointDifference b from) (pointDifference to from)
        sameishDirection v1 v2 = (dotProduct v1 v2) > 0

-- assumes coplanar!
pointInTriangle p t = all (insideOfSide p) (sides t)
  where sides t@(Triangle a b c) = [(Segment a b, c), (Segment a c, b), (Segment b c, a)]
        insideOfSide p (segment, opposite) = sameSideOfSegment p opposite segment

rayTriangleIntersection r t =
  case planeHit of
    Nothing -> Nothing
    Just p -> if pointInTriangle p t
                 then Just p
                 else Nothing
  where planeHit = rayIntersectPlane r (triangleToPlane t)


scales :: Int -> [Float]
scales resolution = [index / (r / 2.0) | index <- stuff]
  where stuff :: [Float]
        stuff = fmap (* (-1)) [((-0.5) * r) .. ((0.5) * r)]
        r :: Float
        r = fromIntegral resolution

rays screen = [rayRow yScale screen | yScale <- scales (yResolution screen)]
  where rayRow yScale screen = [mkRay xScale yScale screen | xScale <- scales (xResolution screen)]
        mkRay xScale yScale screen = Ray {
          rayOrigin = (rayOrigin (screenDirection screen)),
          normal = mkVector xScale yScale screen
        }
        mkVector xScale yScale screen = translateV
                                          (screenCenter screen)
                                          offCenter
          where offCenter = translateV (scaleV (toLeftEdge screen) xScale)
                                       (scaleV (toTopEdge screen) yScale)
        screenCenter screen = pointToVec $ translateP (rayOrigin (screenDirection screen)) (normal (screenDirection screen))

render :: World -> Ray -> Char
render world r = if (any (hitsTriangle r) world)
                    then '*'
                    else ' '
                 where hitsTriangle r t = isJust (rayTriangleIntersection r t)


tests :: IO ()
tests = let unitX = Vector 1 0 0
            unitY = Vector 0 1 0
            unitZ = Vector 0 0 1
            origin = Point 0 0 0
            triangle = Triangle (Point 0 0 0) (Point 10 0 0) (Point 10 0 10)
            farTriangle = Triangle (Point (-5) 10 0) (Point 0 10 5) (Point 5 10 0)
            plane :: Plane
            plane = Ray {rayOrigin = (Point 0 0 10), normal = (Vector 0 0 (-1)) }
            upray = Ray {rayOrigin = (Point 0 0 0), normal = (Vector 0 0 1) }
            upright = Ray {rayOrigin = (Point 0 0 0), normal = (Vector 5 0 1) }
            screen = ProjectionScreen {
              xResolution = 20,
              yResolution = 20,
              screenDirection = Ray { rayOrigin=origin, normal=unitY},
              toLeftEdge = scaleV unitX (-1),
              toTopEdge = unitZ
            }
            world = [farTriangle]
            rendered = map (map (render world)) (rays screen)
        in
          --(print $ crossProduct unitX unitY) >>
          --(print $ normalTraingleFace triangle) >>
          --(print $ triangleToPlane triangle) >>
          --(print $ rayIntersectPlane upray plane) >>
          --(print $ rayIntersectPlane upright plane) >>
          --(print $ pointInTriangle (Point 1 1 0) (Triangle (Point 0 0 0) (Point 10 0 0) (Point 10 90 0))) >>
          --(print $ pointInTriangle (Point 1 (-1) 0) (Triangle (Point 0 0 0) (Point 10 0 0) (Point 10 90 0))) >>
          --(print $ scales $ yResolution screen)>>
          (forM_ rendered putStrLn)

