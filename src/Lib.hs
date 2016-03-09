{-# LANGUAGE DeriveFunctor #-}
module Lib where

import Data.Function
import Data.Maybe

data GenericVector t = Vector { vX :: t, vY :: t, vZ :: t } deriving (Show, Functor)
type Vector = GenericVector Float

newtype Point = Point Vector deriving (Show)

pX :: Point -> Float
pX (Point v) = vX v
pY :: Point -> Float
pY (Point v) = vY v
pZ :: Point -> Float
pZ (Point v) = vZ v

data GenericTriangle t = Triangle t t t deriving (Show, Functor)
type Triangle = GenericTriangle Point

data GenericRay t = Ray { rayOrigin :: Point, rayDirection :: Vector } deriving (Show)
type Ray = GenericRay Float

newtype Plane = Plane Ray deriving (Show)

pNormal :: Plane -> Vector
pNormal (Plane (Ray{rayOrigin = _, rayDirection = d})) = d

pPointOn :: Plane -> Point
pPointOn (Plane (Ray{rayOrigin = p, rayDirection = _})) = p

newtype World = World [Triangle]

data Segment  = Segment Point Point deriving (Show)

newtype Radians = Radians Float

data ProjectionScreen = ProjectionScreen { xResolution :: Int, yResolution :: Int, screenDirection :: Ray, toLeftEdge :: Vector, toTopEdge :: Vector }

makePoint :: Float -> Float -> Float -> Point
makePoint x y z = Point $ Vector x y z

normalTraingleFace :: Triangle -> Vector
normalTraingleFace (Triangle a b c) =
  crossProduct (pointDifference b a) (pointDifference c a)

vectorDifference :: Vector -> Vector -> Vector
vectorDifference (Vector{vX = v1X, vY = v1Y, vZ = v1Z})
                 (Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
  Vector {
    vX = (v1X - v2X),
    vY = (v1Y - v2Y),
    vZ = (v1Z - v2Z)
  }

pointDifference :: Point -> Point -> Vector
pointDifference = vectorDifference `on` pointToVec

vecToPoint :: Vector -> Point
vecToPoint v = makePoint (vX v) (vY v) (vZ v)

pointToVec :: Point -> Vector
pointToVec p = Vector (pX p) (pY p) (pZ p)

crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector{vX = v1X, vY = v1Y, vZ = v1Z})
             (Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
  Vector {
    vX = (v1Y * v2Z - v1Z * v2Y),
    vY = (v1Z * v2X - v1X * v2Z),
    vZ = (v1X * v2Y - v1Y * v2X)
  }

dotProduct :: Vector -> Vector -> Float
dotProduct (Vector{vX = v1X, vY = v1Y, vZ = v1Z})
           (Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
    (v1X * v2X) + (v1Y * v2Y) + (v1Z * v2Z)

triangleToPlane :: Triangle -> Plane
triangleToPlane triangle@(Triangle a _ _) = Plane $ Ray a (normalTraingleFace triangle)

scaleV :: Vector -> Float -> Vector
scaleV v f = fmap ((*) f) v

translateP :: Point -> Vector -> Point
translateP p v = vecToPoint $ translateV (pointToVec p) v

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
     else Just (translateP (rayOrigin ray) (scaleV (rayDirection ray) t))
  where t = top / bottom
        top = (dotProduct (pointToVec (pPointOn plane)) (pNormal plane)) - (dotProduct (pointToVec (rayOrigin ray)) (pNormal plane))
        bottom = dotProduct (rayDirection ray) (pNormal plane)

sameSideOfSegment :: Point -> Point -> Segment -> Bool
sameSideOfSegment a b (Segment from to) = sameishDirection crossOne crossTwo
  where crossOne = crossProduct (pointDifference a from) (pointDifference to from)
        crossTwo = crossProduct (pointDifference b from) (pointDifference to from)
        sameishDirection v1 v2 = (dotProduct v1 v2) > 0

-- assumes coplanar!
pointInTriangle :: Point -> Triangle -> Bool
pointInTriangle p t = all (insideOfSide p) (sides t)
  where sides (Triangle a b c) = [(Segment a b, c), (Segment a c, b), (Segment b c, a)]
        insideOfSide candidatePoint (segment, opposite) = sameSideOfSegment candidatePoint opposite segment

rayTriangleIntersection :: Ray -> Triangle -> Maybe Point
rayTriangleIntersection r t =
  case planeHit of
    Nothing -> Nothing
    Just p -> if pointInTriangle p t
                 then Just p
                 else Nothing
  where planeHit = rayIntersectPlane r (triangleToPlane t)

square :: (Num a) => a -> a
square x = x * x

norm :: Vector -> Float
norm v = sqrt ((square (vX v)) + (square (vY v)) + (square (vZ v)))

normalize :: Vector -> Vector
normalize v = scaleV v (1/ (norm v))

rotateAround :: Point -> Ray -> Radians -> Point
rotateAround p r (Radians theta) = makePoint outX outY outZ
  where Point (Vector x y z)  = p
        normalRayDirection = normalize (rayDirection r)
        Vector u v w = normalRayDirection
        Point (Vector a b c) = rayOrigin r
        outX = (a * (v*v + w*w) - u * (b * v + c * w - u * x - v * y - w * z )) * (1 - cos theta) + (x * cos theta) + (b * w - c * v - w * y + v * z) * (sin theta)
        outY = (b * (u*u + w*w) - v * (a * u + c * w - u * x - v * y - w * z )) * (1 - cos theta) + (y * cos theta) + (c * u - a * w + w * x - u * z ) * (sin theta)
        outZ = (c * (u*u + v*v) - w * (a * u + b * v - u * x - v * y - w * z )) * (1 - cos theta) + (z * cos theta) + (a * v - b * u - v * x + u * y) * (sin theta)

-- screen stuff
scales :: Int -> [Float]
scales resolution = [index / (r / 2.0) | index <- coefficients]
  where coefficients :: [Float]
        coefficients = fmap (* (-1)) [((-0.5) * r) .. (0.5 * r)]
        r :: Float
        r = fromIntegral resolution

rays :: ProjectionScreen -> [[Ray]]
rays screen = [rayRow yScale | yScale <- scales (yResolution screen)]
  where rayRow yScale = [mkRay xScale yScale | xScale <- scales (xResolution screen)]
        mkRay xScale yScale = Ray {
          rayOrigin = rayOrigin (screenDirection screen),
          rayDirection = mkVector xScale yScale
        }
        mkVector xScale yScale = translateV screenCenter offCenter
          where offCenter = translateV (scaleV (toLeftEdge screen) xScale)
                                       (scaleV (toTopEdge screen) yScale)
        screenCenter = pointToVec $ translateP (rayOrigin (screenDirection screen)) (rayDirection (screenDirection screen))

render :: World -> Ray -> Char
render (World triangles) r = if any (hitsTriangle r) triangles
                    then '*'
                    else ' '
                 where hitsTriangle ray t = isJust (rayTriangleIntersection ray t)

renderWorld :: ProjectionScreen -> World -> [String]
renderWorld screen w = map (map (render w)) (rays screen)

origin :: Point
origin = makePoint 0 0 0

unitX :: Vector
unitX = Vector 1 0 0
unitY :: Vector
unitY = Vector 0 1 0
unitZ :: Vector
unitZ = Vector 0 0 1

tests :: IO ()
tests = let triangle = Triangle (makePoint 0 0 0.5) (makePoint 10 0 0.5) (makePoint 10 0 10.5)
            plane :: Plane
            plane = Plane Ray {rayOrigin = makePoint 0 0 10, rayDirection = Vector 0 0 (-1)}
            upray = Ray {rayOrigin = makePoint 0 0 0, rayDirection = Vector 0 0 1 }
            upright = Ray {rayOrigin = makePoint 0 0 0, rayDirection = Vector 5 0 1 }
            screen = ProjectionScreen {
              xResolution = 3,
              yResolution = 3,
              screenDirection = Ray { rayOrigin=origin, rayDirection=unitY},
              toLeftEdge = scaleV unitX (-1),
              toTopEdge = unitZ
            }
        in
          (print $ crossProduct unitX unitY) >>
          (print $ normalTraingleFace triangle) >>
          (print $ rotateAround origin (Ray (vecToPoint unitY) unitZ) (Radians 3.14159)) >>
          (print $ triangleToPlane triangle) >>
          (print $ rayIntersectPlane upray plane) >>
          (print $ rayIntersectPlane upright plane) >>
          (print $ pointInTriangle (makePoint 1 1 0) (Triangle (makePoint 0 0 0) (makePoint 10 0 0) (makePoint 10 90 0))) >>
          (print $ pointInTriangle (makePoint 1 (-1) 0) (Triangle (makePoint 0 0 0) (makePoint 10 0 0) (makePoint 10 90 0))) >>
          (print $ scales $ yResolution screen)
