-- The program for finding the root of the equation x (x) = 0
-- in a given interval ab with an accuracy of eps by
-- the method of quadratic interpolation.
-- If successful, the function displays the root and
-- the number of iterations spent looking for it.
-- Otherwise the error code.
--
-- Compile: ghc -o a main.hs
-- Run: ./a

--module Hiker where
import Data.Maybe
maxit = 12345
e = 1e-10

readDouble:: IO Double
readDouble = read <$> getLine

main :: IO ()
main = do
  putStrLn "Input a: "
  a <- readDouble
  putStrLn "Input b: "
  b <- readDouble
  putStrLn "Input eps: "
  eps <- readDouble
  let ans = solve a b eps mf
  putStrLn $ show ans
  return ()

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

mf :: Double -> Double
mf = (subtract 4).(^2)

compare_abs a b = abs a >= abs b

inter :: Double -> Double -> Double -> Double -> Double -> Double -> Maybe Double

inter  x1 x2 x3 y1 y2 y3
 |  ( abs (x1 - x2) ) < e || ( abs (x3 - x2) ) < e || ( abs (x1 - x3) ) < e = Nothing
 | otherwise = Just $(
             - (-y1 * x2 + y2 * x1) / (x1 - x2) * x3 +
               (-y2 * x3 + y3 * x2) / (x2 - x3) * x1
             ) / (x1 - x3)

recursiveLoop :: (Double -> Double) -> Double -> Integer -> Double -> Double -> Double -> Double -> Double -> Double -> Maybe Double

recursiveLoop f eps deep y1 y2 y3 x1 x2 x3 =
    if deep > maxit
        then Nothing
        else let xn = fromJust (inter y1 y2 y3 x1 x2 x3)
                 yn = f xn
                 nextIter = recursiveLoop f eps (deep+1)
                 nextIter1 = nextIter yn y2 y3 xn x2 x3
                 nextIter2 = nextIter y1 yn y3 x1 xn x3
                 nextIter3 = nextIter y1 y2 yn x1 x2 xn
             in if abs yn < eps
              then Just xn
              else selectIter y1 y2 y3 x1 x2 x3 yn nextIter1 nextIter2 nextIter3

selectIter y1 y2 y3 x1 x2 x3 yn iter1 iter2 iter3
 |       compare_abs yn y1  &&       compare_abs yn y2  && compare_abs yn y3 = Nothing
 |       compare_abs yn y1  && (not $compare_abs yn y2) && compare_abs y2 y3 = iter2
 | (not $compare_abs yn y1) &&       compare_abs y1 y2  && compare_abs y1 y3 = iter1
 | (not $compare_abs yn y1) && (not $compare_abs y1 y2) && compare_abs y2 y3 = iter2
 | otherwise = iter3

solve :: Double -> Double -> Double -> (Double -> Double) -> Maybe Double
solve a b eps f =
    let c = (a + b) / 2.0
    in solveWithC a b eps f c
solveWithC  a b eps f c
    | (abs.f) a < eps = Just a
    | (abs.f) b < eps = Just b
    | (abs.f) c < eps = Just c
    | otherwise = recursiveLoop f eps 0 (f a) (f b) (f c) a b c


