module Main where

import Control.Applicative (liftA2)
import Data.List (intercalate)


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

scheme0d :: PathScheme
scheme0d = [ (V [], []) ]

schemeNd :: Int -> PathScheme
schemeNd 0 = scheme0d
schemeNd n = oneUp $ schemeNd (n-1)


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


-- Construct an (n+1)-dimensional path scheme from a n-dimensional one.
-- (Produces rotations with infinite columns,
-- but that does not matter for approx.)
oneUp :: PathScheme -> PathScheme
oneUp scheme = left1 ++ [left2] ++ [right1] ++ right2
  where
    left1, right2 :: [(Vector, Rotation)]
    left2, right1 :: (Vector, Rotation)
    left1  = [ (oneUpV  t, oneUpR' r) | (t, r) <- init scheme ]
    left2  = let (t, r) = last scheme in (oneUpV t, oneUpR r)
    right1 = let (t, r) = last scheme in (oneUpV' t, oneUpR r)
    right2 = [ (oneUpV' t, doubleFlipR (oneUpR' r))
             | (t, r) <- reverse (init scheme) ]
    oneUpV  (V xs) = V (0:xs)
    oneUpV' (V xs) = V (1:xs)
    oneUpR :: Rotation -> Rotation
    oneUpR  vs = e1 : map oneUpV vs
    oneUpR' vs = map oneUpV vs ++ [e1]  -- here is a choice
    -- Flip first column and first row:
    doubleFlipR (v:vs) = map flipCol $ ((-1)*#v):vs
    flipCol (V (x:xs)) = V ((-1)*x:xs)
    e1 = V (1 : repeat 0)

stretchPath :: Path -> Path
stretchPath [] = []
stretchPath [v] = [2*#v]
stretchPath (v:w:ws) = 2*#v : midway (2*#v) (2*#w) : stretchPath (w:ws)
  where
    midway (V xs) (V ys) = V $ zipWith (\x y -> (x+y) `div` 2) xs ys


-- Producing code for a-frame:

main :: IO ()
main = do
  header <- readFile "header.html"
  footer <- readFile "footer.html"
  writeFile "out.html" $
       header
    ++ spheresCode dist (dist/6) "#FF0000" path
    ++ cylindersCode dist (dist/8) "#0000FF" path
    ++ footer
  where
    dist = 0.5
    path = approx (schemeNd 3) 3

spheresCode :: Float -> Float -> String -> [Vector] -> String
spheresCode dist radius color =
  unlines . map (\v ->
    "<a-sphere position=\""++coordsString dist v++"\" radius=\""++show radius++"\" "
    ++"color=\""++color++"\" shadow></a-sphere>" )

cylindersCode :: Float -> Float -> String -> Path -> String
cylindersCode dist radius color =
  unlines . map (\(v, w) ->
    "<a-cylinder position=\""++coordsStringMidway dist v w++"\" radius=\""++show radius++"\" "
    ++"height=\""++show dist++"\" rotation=\""++unitVectorToRotationString (w#-#v)
    ++"\" color=\""++color++"\" shadow></a-cylinder>" )
  . (\vs -> zip vs (tail vs))

unitVectorToRotationString :: Vector -> String
unitVectorToRotationString (V xs) = case map abs xs of
  [1, 0, 0]  -> " 0 0 90"
  [0, 1, 0]  -> " 0 0  0"
  [0, 0, 1]  -> "90 0  0"

coordsString :: Float -> Vector -> String
coordsString dist (V xs) = intercalate " " $
  [ show (dist * fromIntegral x) | x <- xs ]

coordsStringMidway :: Float -> Vector -> Vector -> String
coordsStringMidway dist (V xs) (V ys) = intercalate " " $
  [ show (0.5*dist*(fromIntegral x + fromIntegral y))
  | (x, y) <- zip xs ys ]
