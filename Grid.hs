
module Grid where

import Graphics.Gloss

import Util
import Universe
import Camera
import Body
import Constants

type Grid = [[Vec3]]

grid :: Float -> Float -> Float -> Vec3 -> [[Vec3]]
grid space segment width (x, _, z) =
    xs ++ zs
    where xs = [[(xx, 0, zz) | zz <- vs szs ez segment] | xx <- vs sxp ex space]
          zs = [[(xx, 0, zz) | xx <- vs sxs ex segment] | zz <- vs szp ez space]
          
          vs s e i = takeWhile (< e) $ iterate (+ i) s
    
          sx = x - hw
          sz = z - hw
          ex = x + hw
          ez = z + hw
          sxp = ceilTo space $ sx
          szp = ceilTo space $ sz
          sxs = ceilTo segment $ sx
          szs = ceilTo segment $ sz
          hw = width / 2
          
          ceilTo base val = base * (fromIntegral $ ceiling $ val / base)
          
emptyGrid :: [[Vec3]]
emptyGrid = []

createGridUpdater :: Updater (Grid, Universe, Camera)
createGridUpdater = createUpdater gridUR updateFunc
    where updateFunc dt (_, universe, camera) =
            (grid', universe, camera)
            where grid' = distort bodies $ grid u uu width $ cameraPos camera
                  width = 1 / cameraZoom camera
                  u = width / gridU
                  uu = u / gridUU
                  distort bodies grid =
                    foldl (\g b -> bodyDistort b g) grid bodies
                  bodyDistort body grid =
                    map (\l -> map (gridDepth body) l) grid
                  gridDepth body pt =
                    let pos = bodyPos body
                        (x, y, z) = pt
                        diff = pos `v2Minus` (x, z)
                        d = v2Len diff
                    in (x, y + gravityWell body d, z)
                  bodies = universeBodies universe

drawGrid :: Grid -> Camera -> Picture
drawGrid grid camera = 
    color gridCLR $ 
    gridPicture
    where transform = map $ mapCameraWorldToScreen camera
          gridLines = map transform grid
          gridPicture = pictures $ map line gridLines