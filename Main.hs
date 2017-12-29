module Main where

import Control.Applicative (liftA2)


type Scalar = Integer

newtype Vector = V [Scalar]
  deriving (Eq, Ord, Show)

(#+#) :: Vector -> Vector -> Vector
(V xs) #+# (V ys) = V $ zipWith (+) xs ys

(#-#) :: Vector -> Vector -> Vector
(V xs) #-# (V ys) = V $ zipWith (-) xs ys

(*#) :: Scalar -> Vector -> Vector
x *# (V ys) = V $ (x*) <$> ys

isUnitVector :: Vector -> Bool
isUnitVector (V xs) =
     length [x | x <- xs, x == 0] == length xs - 1
  && length [x | x <- xs, abs x == 1] == 1

type Rotation = [Vector] -- orthogonal Matrix, column major

rotate :: Rotation -> Vector -> Vector
rotate rot (V xs) = foldl1 (#+#) $ zipWith (*#) xs rot

rotateInCube ::Scalar -> Rotation ->  Vector -> Vector
rotateInCube size rot (V xs) = foldl1 (#+#) $
  zipWith (\x (V ys) -> V (map (f x) ys)) xs rot
  where
    f x 0 = 0
    f x 1 = x
    f x (-1) = size - 1 - x

type PathScheme = [(Vector, Rotation)]

scheme2d :: PathScheme
scheme2d =
  [ (V [0, 0], [V [0, 1], V [1, 0]])
  , (V [0, 1], [V [1, 0], V [0, 1]])
  , (V [1, 1], [V [1, 0], V [0, 1]])
  , (V [1, 0], [V [0, -1], V [-1, 0]])
  ]


type Path = [Vector]

isPath :: [Vector] -> Bool
isPath vs = all isUnitVector (zipWith (#-#) vs (tail vs))

-- Walks from [0, 0, ...] to [2^n-1, 0, ...].
approx :: PathScheme -> Int -> Path
approx _ 0 = return $ V (repeat 0)
approx scheme n = do
  (trans, rot) <- scheme
  map (((#+#) ((2^(n-1)) *# trans)) . rotateInCube (2^(n-1)) rot)
    (approx scheme (n-1))


main :: IO ()
main = putStrLn "Hello, Haskell!"
