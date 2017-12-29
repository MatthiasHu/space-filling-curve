module Main where

import Data.List (nub)


type Scalar = Integer
type Vertex1d = Scalar
type Cube1d = Scalar

type Vector = [Scalar]
type Vertex = [Scalar]
type Cube = [Scalar]

isVertexOf :: Vertex -> Cube -> Bool
isVertexOf v c = all id $ zipWith isVertexOf1d v c

isVertexOf1d :: Vertex1d -> Cube1d -> Bool
isVertexOf1d v c = v `elem` [c, c+1]

isUnitVector :: Vector -> Bool
isUnitVector v = 
     length [x | x <- v, x == 0] == length v - 1
  && length [x | x <- v, abs x == 1] == 1

data PathScheme = PathScheme
  { vertices :: [Vertex]
  , cubes    :: [Cube]
  }

isValidPathScheme :: PathScheme -> Bool
isValidPathScheme (PathScheme vs cs) =
     (all isUnitVector $ zipWith (zipWith (-)) vs (tail vs))
  && length vs == length cs + 1
  && all id (zipWith isVertexOf vs cs)
  && all id (zipWith isVertexOf (tail vs) cs)

isInjectivePathScheme :: PathScheme -> Bool
isInjectivePathScheme (PathScheme vs cs) = nub cs == cs


main :: IO ()
main = putStrLn "Hello, Haskell!"
