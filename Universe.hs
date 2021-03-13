
module Universe where

import Graphics.Gloss

import Util
import Body
import Camera
import Constants

data Universe = 
    Universe {
        universeBodies :: ![Body]
    }
    
createUniverse :: Universe
createUniverse =
    Universe {
        universeBodies = []
    }
    
createUniverseUpdater :: Updater Universe
createUniverseUpdater = createUpdater uniUR updateFunc
    where updateFunc dt universe =
            universe {
                universeBodies = bodies'
            }
            where bodies' = 
                    map (moveBody dt) $ zipWith (sumForces dt) bodies bPerm
                  force bd1 bd2 =
                    let mass1 = bodyMass bd1
                        mass2 = bodyMass bd2
                        g = uniG
                        pos1 = bodyPos bd1
                        pos2 = bodyPos bd2
                        diff@(x, z) = pos2 `v2Minus` pos1
                        d = v2Len diff
                        fg = g * mass1 * mass2 / d ** 2
                    in fg `v2Scale` (d `v2Div` diff)
                  sumForces dt body others =
                    let Body {
                            bodyMass = mass,
                            bodyVel = vel
                        } = body
                        f = foldl v2Plus (0, 0) $ map (force body) others
                    in body {
                        bodyVel = vel `v2Plus` ((dt / mass) `v2Scale` f)
                       }
                  bPerm = zipWith remove [0..] $ repeat bodies
                  
                  Universe {
                    universeBodies = bodies
                  } = universe
                  
drawUniverse :: Universe -> Camera -> Picture
drawUniverse universe camera =
    pictures $
    map (doCircle camera) (universeBodies universe)
    where
    doCircle camera body =
        color (bodyCol body) $ translate xx yy $ scale s s $ circleSolid 1
        where (x, z) = bodyPos body
              (xx, yy) = mapCameraWorldToScreen camera $ (x, 0, z)
              (_, height) = cameraDims camera
              zoom = cameraZoom camera
              s = height * zoom * bodyRadius body