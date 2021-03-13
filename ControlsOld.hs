
module Controls where

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Util
import Universe
import Camera
import Body

data Controls =
    Controls {
        controlsWDown :: !Bool,
        controlsADown :: !Bool,
        controlsSDown :: !Bool,
        controlsDDown :: !Bool,
        controlsQDown :: !Bool,
        controlsEDown :: !Bool,
        
        controlsLMBDown :: !Bool,
        controlsLMBClicked :: !Bool,
        controlsLMBReleased :: !Bool,
        controlsClickedPoint :: !Vec2,
        controlsCurrentPoint :: !Vec2,
        controlsAnchorPoint :: !Vec3
    }
    
createControls :: Controls
createControls =
    Controls {
        controlsWDown = False,
        controlsADown = False,
        controlsSDown = False,
        controlsDDown = False,
        controlsQDown = False,
        controlsEDown = False,
        
        controlsLMBDown = False,
        controlsLMBClicked = False,
        controlsLMBReleased = False,
        controlsClickedPoint = (0, 0),
        controlsCurrentPoint = (0, 0),
        controlsAnchorPoint = (0, 0, 0)
    }
    
handleControlsEvent :: Controls -> Event -> Controls
handleControlsEvent controls event = 
    controls {
        controlsWDown = isKeyDown event 'w' wDownPrev,
        controlsADown = isKeyDown event 'a' aDownPrev,
        controlsSDown = isKeyDown event 's' sDownPrev,
        controlsDDown = isKeyDown event 'd' dDownPrev,
        controlsQDown = isKeyDown event 'q' qDownPrev,
        controlsEDown = isKeyDown event 'e' eDownPrev,
        
        controlsLMBDown = lmbDown,
        controlsLMBClicked = lmbClicked,
        controlsLMBReleased = lmbReleasedPrev || not lmbDown && lmbDownPrev,
        controlsClickedPoint = if lmbClicked
                               then mousePos event
                               else clickedPointPrev,
        controlsCurrentPoint = mousePos event
    }
    where isKeyDown event ch prev =
            case event 
            of EventKey (Char keyCh) keyState _ _ -> if keyCh == ch
                                                     then keyState == Down
                                                     else prev
               _ -> prev
          isMouseDown event b prev =
            case event
            of EventKey (MouseButton btn) keyState _ _ -> if btn == b
                                                          then keyState == Down
                                                          else prev
               _ -> prev
          mousePos event =
            case event
            of EventKey _ _ _ pos -> pos
               EventMotion pos -> pos
               _ -> (0, 0)
          
               
          wDownPrev = controlsWDown controls
          aDownPrev = controlsADown controls
          sDownPrev = controlsSDown controls
          dDownPrev = controlsDDown controls
          qDownPrev = controlsQDown controls
          eDownPrev = controlsEDown controls
          
          lmbReleasedPrev = controlsLMBReleased controls
          lmbDownPrev = controlsLMBDown controls
          lmbDown = isMouseDown event LeftButton lmbDownPrev
          lmbClicked = lmbDown && not lmbDownPrev
          clickedPointPrev = controlsClickedPoint controls
          
createControlsUpdater :: Updater (Controls, Universe, Camera)
createControlsUpdater = createUpdater 240 updateFunc
    where updateFunc dt (controls, universe, camera) = 
            (
            let pt = controlsClickedPoint controls
                anchorPoint = controlsAnchorPoint controls
            in controls {
                controlsAnchorPoint = 
                    if controlsLMBClicked controls
                    then mapCameraScreenToWorld camera pt
                    else anchorPoint,
                controlsLMBClicked = False,
                controlsLMBReleased = False
               }, 
            let bodies = universeBodies universe
                newBody = 
                    if controlsLMBReleased controls
                    then trace "Wow" [createBody 4 2 
                          (sx, sz)
                          (0, 0)]
                    else []
                (sx, _, sz) = controlsAnchorPoint controls
                (ex, _, ez) = mapCameraScreenToWorld camera $
                              controlsCurrentPoint controls
            in universe {
                universeBodies = newBody ++ bodies
               },
            let forward = unapplyCameraYaw camera $ (0, 0, magnitude)
                right = unapplyCameraYaw camera $ (magnitude, 0, 0) 
                backward = (-1) `v3Scale` forward
                left = (-1) `v3Scale` right
                accel = useAccel controlsWDown forward `v3Plus`
                        useAccel controlsSDown backward `v3Plus`
                        useAccel controlsDDown right `v3Plus`
                        useAccel controlsADown left
                useAccel p v = if p controls then v else (0, 0, 0)
                magnitude = 100
                
                yawAccel = useYawAccel controlsQDown (-yawMagnitude) +
                           useYawAccel controlsEDown yawMagnitude
                useYawAccel p a = if p controls then a else 0
                yawMagnitude = 10
            in camera {
                cameraVel = vel `v3Plus` (dt `v3Scale` accel),
                cameraYawVel = yawVel + dt * yawAccel
               }
            )
            where Camera {
                    cameraVel = vel,
                    cameraYawVel = yawVel,
                    cameraPitchVel = pitchVel
                  } = camera
            
drawControls :: Controls -> Picture
drawControls controls =
    if lmbDown
    then color red $ line [clickedPoint, currentPoint]
    else blank
    where Controls {
            controlsLMBDown = lmbDown,
            controlsClickedPoint = clickedPoint,
            controlsCurrentPoint = currentPoint
          } = controls