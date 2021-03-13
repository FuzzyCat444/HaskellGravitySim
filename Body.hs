
module Body where

import Graphics.Gloss

import Util

data Body =
    Body {
        bodyMass :: !Float,
        bodyRadius :: !Float,
        bodyPos :: !Vec2,
        bodyVel :: !Vec2,
        bodyCol :: !Color
    }
    
createBody :: Float -> Float -> Vec2 -> Vec2 -> Color -> Body
createBody mass radius pos vel color =
    Body {
        bodyMass = mass,
        bodyRadius = radius,
        bodyPos = pos,
        bodyVel = vel,
        bodyCol = color
    }
    
moveBody :: Float -> Body -> Body
moveBody dt body =
    body {
        bodyPos = pos `v2Plus` (dt `v2Scale` vel)
    }
    where Body {
            bodyPos = pos,
            bodyVel = vel
          } = body
          
gravityWell :: Body -> Float -> Float
gravityWell body dist =
    (3 / (4 * pi) * mass) ** (1 / 3) * (integral (dist / radius) / 1.5 - 1)
    where integral d =
            if d < 1
            then d ** 2 / 2
            else 1.5 - 1 / d
          mass = bodyMass body
          radius = bodyRadius body