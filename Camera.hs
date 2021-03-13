
module Camera where

import Util
import Constants

data Camera =
    Camera {
        cameraDims :: !Vec2,
        cameraPos :: !Vec3,
        cameraYaw :: !Float,
        cameraPitch :: !Float,
        cameraZoom :: !Float,
        
        cameraVel :: !Vec3,
        cameraYawVel :: !Float,
        cameraPitchVel :: !Float
    }
    
createCamera :: Vec2 -> Vec3 -> Float -> Float -> Float -> Camera
createCamera dims pos yaw pitch zoom = 
    Camera {
        cameraDims = dims,
        cameraPos = pos,
        cameraYaw = yaw,
        cameraPitch = pitch,
        cameraZoom = zoom,
        
        cameraVel = (0, 0, 0),
        cameraYawVel = 0,
        cameraPitchVel = 0
    }

applyCameraTranslate :: Camera -> Vec3 -> Vec3
applyCameraTranslate camera (x, y, z) =
    (x - cx, y - cy, z - cz)
    where (cx, cy, cz) = cameraPos camera

applyCameraYaw :: Camera -> Vec3 -> Vec3
applyCameraYaw camera (x, y, z) =
    (x * c - z * s, y, x * s + z * c)
    where c = cos(-yaw)
          s = sin(-yaw)
          yaw = cameraYaw camera
          
applyCameraPitch :: Camera -> Vec3 -> Vec2
applyCameraPitch camera (x, y, z) =
    (x, z * percentZ + y * percentY)
    where percentZ = sin(pitch)
          percentY = cos(pitch)
          pitch = cameraPitch camera
          
applyCameraZoom :: Camera -> Vec2 -> Vec2
applyCameraZoom camera (x, y) =
    (x * zoom, y * zoom)
    where zoom = cameraZoom camera
    
applyCameraDims :: Camera -> Vec2 -> Vec2
applyCameraDims camera (x, y) =
    (x * height, y * height)
    where (_, height) = cameraDims camera
    
mapCameraWorldToScreen :: Camera -> Vec3 -> Vec2
mapCameraWorldToScreen camera = applyCameraTranslate camera ...
                                applyCameraYaw camera ...
                                applyCameraPitch camera ...
                                applyCameraZoom camera ...
                                applyCameraDims camera
    where (...) = flip (.)

unapplyCameraDims :: Camera -> Vec2 -> Vec2
unapplyCameraDims camera (x, y) =
    (x / height, y / height)
    where (_, height) = cameraDims camera
    
unapplyCameraZoom :: Camera -> Vec2 -> Vec2
unapplyCameraZoom camera (x, y) =
    (x / zoom, y / zoom)
    where zoom = cameraZoom camera

unapplyCameraPitch :: Camera -> Vec2 -> Vec3
unapplyCameraPitch camera (x, y) =
    (x, 0, y / percentZ)
    where percentZ = sin(pitch)
          pitch = cameraPitch camera
          
unapplyCameraYaw :: Camera -> Vec3 -> Vec3
unapplyCameraYaw camera (x, y, z) =
    (x * c - z * s, y, x * s + z * c)
    where c = cos(yaw)
          s = sin(yaw)
          yaw = cameraYaw camera

unapplyCameraTranslate :: Camera -> Vec3 -> Vec3
unapplyCameraTranslate camera (x, y, z) =
    (x + cx, y + cy, z + cz)
    where (cx, cy, cz) = cameraPos camera
    
mapCameraScreenToWorld :: Camera -> Vec2 -> Vec3
mapCameraScreenToWorld camera = unapplyCameraDims camera ...
                                unapplyCameraZoom camera ...
                                unapplyCameraPitch camera ...
                                unapplyCameraYaw camera ...
                                unapplyCameraTranslate camera
    where (...) = flip (.)
    
createCameraUpdater :: Updater Camera
createCameraUpdater = createUpdater camUR updateFunc
    where updateFunc dt camera = 
            camera {
                cameraPos = pos',
                cameraYaw = yaw',
                cameraPitch = pitch'',
                cameraVel = vel',
                cameraYawVel = yawVel',
                cameraPitchVel = pitchVel''
            }
            where pos' = pos `v3Plus` (dt `v3Scale` vel)
                  vel' = (camVM ** dt) `v3Scale` vel
                  yaw' = yaw + dt * yawVel
                  yawVel' = (* camYVM ** dt) yawVel
                  pitch' = pitch + dt * pitchVel
                  pitchVel' = (* camPVM ** dt) pitchVel
                  (pitch'', pitchVel'') =
                    if pitch' < 0
                    then (0, 0)
                    else if pitch' > pi / 2
                    then (pi / 2, 0)
                    else (pitch', pitchVel')
                  Camera {
                    cameraPos = pos,
                    cameraYaw = yaw,
                    cameraPitch = pitch,
                    cameraVel = vel,
                    cameraYawVel = yawVel,
                    cameraPitchVel = pitchVel
                  } = camera