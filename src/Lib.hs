module Lib
    ( someFunc
    ) where

import Data.Function (on)

data GenericPoint t = Point { pX :: t, pY :: t, pZ :: t } deriving (Show)
type Point = GenericPoint Float

data GenericVector t = Vector { vX :: t, vY :: t, vZ :: t } deriving (Show)
type Vector = GenericVector Float

data GenericTriangle t = Triangle (GenericPoint t) (GenericPoint t) (GenericPoint t) deriving (Show)
type Triangle = GenericTriangle Float

data GenericRay t = Ray { rayOrigin :: Point, normal :: Vector } deriving (Show)
type Ray = GenericRay Float

type Plane = Ray

normalTraingleFace :: Triangle -> Vector
normalTraingleFace t@(Triangle a b c) =
  crossProduct (pointDifference b a) (pointDifference c a)

pointDifference :: Point -> Point -> Vector
pointDifference p1@(Point{pX = p1X, pY = p1Y, pZ = p1Z})
                p2@(Point{pX = p2X, pY = p2Y, pZ = p2Z})  =
  Vector {
    vX = (p2X - p1X),
    vY = (p2Y - p1Y),
    vZ = (p2Z - p1Z)
  }

vecToPoint :: Vector -> Point
vecToPoint v =
  Point (vX v) (vY v) (vZ v)

vectorDifference v1 v2 = pointDifference `on` vecToPoint

crossProduct :: Vector -> Vector -> Vector
crossProduct v1@(Vector{vX = v1X, vY = v1Y, vZ = v1Z})
             v2@(Vector{vX = v2X, vY = v2Y, vZ = v2Z})  =
  Vector {
    vX = (v1Y * v2Z - v1Z * v2Y),
    vY = (v1Z * v2X - v1X * v2Z),
    vZ = (v1X * v2Y - v1Y * v2X)
  }

triangleToPlane :: Triangle -> Plane
triangleToPlane triangle@(Triangle a b c) = Ray a (normalTraingleFace triangle)



someFunc :: IO ()
someFunc = let unitX = Vector 1 0 0
               unitY = Vector 0 1 0
               triangle = Triangle (Point 0 0 0) (Point 10 0 0) (Point 10 0 10)
               b = Point 0 0 0
               a = Point 0 0 0
           in
             (print $ crossProduct unitX unitY) >>
             (print $ normalTraingleFace triangle) >>
             (print $ triangleToPlane triangle)
