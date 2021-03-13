
module Util where

type Vec2 = (Float, Float)
type Vec3 = (Float, Float, Float)

v2Plus :: Vec2 -> Vec2 -> Vec2
(x, y) `v2Plus` (a, b) = (x + a, y + b)

v2Minus :: Vec2 -> Vec2 -> Vec2
(x, y) `v2Minus` (a, b) = (x - a, y - b)

v2Scale :: Float -> Vec2 -> Vec2
s `v2Scale` (x, y) = (s * x, s * y)

v2Div :: Float -> Vec2 -> Vec2
d `v2Div` (x, y) = (x / d, y / d)

v2Mult :: Vec2 -> Vec2 -> Vec2
(x, y) `v2Mult` (a, b) = (x * a, y * b)

v2Len :: Vec2 -> Float
v2Len v = 
    let (x, y) = v `v2Mult` v
    in sqrt $ x + y

v3Plus :: Vec3 -> Vec3 -> Vec3
(x, y, z) `v3Plus` (a, b, c) = (x + a, y + b, z + c)

v3Minus :: Vec3 -> Vec3 -> Vec3
(x, y, z) `v3Minus` (a, b, c) = (x - a, y - b, z - c)

v3Scale :: Float -> Vec3 -> Vec3
s `v3Scale` (x, y, z) = (s * x, s * y, s * z)

v3Div :: Float -> Vec3 -> Vec3
d `v3Div` (x, y, z) = (x / d, y / d, z / d)

v3Mult :: Vec3 -> Vec3 -> Vec3
(x, y, z) `v3Mult` (a, b, c) = (x * a, y * b, z * c)

v3Len :: Vec3 -> Float
v3Len v = 
    let (x, y, z) = v `v3Mult` v
    in sqrt $ x + y + z

remove :: Integer -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove i (x:xs) = x : remove (i - 1) xs

data Updater a =
    Updater {
        updaterStep :: !Float,
        updaterFunc :: !(Float -> a -> a),
        updaterTickAccum :: !Float
    }
    
createUpdater :: Float -> (Float -> a -> a) -> Updater a
createUpdater rate func =
    Updater {
        updaterStep = 1 / rate,
        updaterFunc = func,
        updaterTickAccum = 0
    }

runUpdater :: Float -> Updater a -> a -> (Updater a, a)
runUpdater delta updater object =
    (
        updater {
            updaterTickAccum = tickAccum''
        },
        iterate (func step) object !! numUpdates
    )
    where tickAccum' = tickAccum + delta
          numUpdates = truncate $ tickAccum' / step
          tickAccum'' = tickAccum' - fromIntegral numUpdates * step
          Updater {
            updaterStep = step,
            updaterTickAccum = tickAccum,
            updaterFunc = func
          } = updater