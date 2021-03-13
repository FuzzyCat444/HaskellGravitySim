
module Controls where

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Util
import Universe
import Camera
import Body
import Constants

data BasicEvent =
    BasicEvent {
        basicEventPressed :: !Bool,
        basicEventIsDown :: !Bool,
        basicEventReleased :: !Bool
    }
    
basicEvent :: BasicEvent
basicEvent =
    BasicEvent {
        basicEventPressed = False,
        basicEventIsDown = False,
        basicEventReleased = False
    }
    
getBasicEvent :: BasicEvent -> (BasicEvent, BasicEvent)
getBasicEvent event =
    (
    event {
        basicEventPressed = False,
        basicEventReleased = False
    },
    event
    )
    
colorsList :: [Color]
colorsList = 
    base ++
    map dim base ++
    map (dim . dim) base
    where base = [red, green, blue, yellow, cyan, magenta]

data Controls =
    Controls {
        controlsW :: !BasicEvent,
        controlsA :: !BasicEvent,
        controlsS :: !BasicEvent,
        controlsD :: !BasicEvent,
        controlsQ :: !BasicEvent,
        controlsE :: !BasicEvent,
        controlsZ :: !BasicEvent,
        controlsX :: !BasicEvent,
        controls_m :: !BasicEvent,
        controls_M :: !BasicEvent,
        controls_r :: !BasicEvent,
        controls_R :: !BasicEvent,
        controls_c :: !BasicEvent,
        controls_C :: !BasicEvent,
        
        controlsLMB :: !BasicEvent,
        controlsRMB :: !BasicEvent,
        
        controlsMousePos :: !Vec2,
        
        controlsMouseWheel :: !Int,
        
        -- Data
        controlsOldMousePos :: !Vec2,
        controlsLaunching :: !Bool,
        
        controlsZoom :: !Int,
        
        controlsMassSlider :: !Int,
        controlsRadiusSlider :: !Int,
        
        controlsColor :: !Int
    }
    
createControls :: Controls
createControls =
    Controls {
        controlsW = basicEvent,
        controlsA = basicEvent,
        controlsS = basicEvent,
        controlsD = basicEvent,
        controlsQ = basicEvent,
        controlsE = basicEvent,
        controlsZ = basicEvent,
        controlsX = basicEvent,
        controls_m = basicEvent,
        controls_M = basicEvent,
        controls_r = basicEvent,
        controls_R = basicEvent,
        controls_c = basicEvent,
        controls_C = basicEvent,
        
        controlsLMB = basicEvent,
        controlsRMB = basicEvent,
        
        controlsMousePos = (0, 0),
        
        controlsMouseWheel = 0,
        
        -- Data
        controlsOldMousePos = (0, 0),
        controlsLaunching = False,
        
        controlsZoom = 0,
        
        controlsMassSlider = 0,
        controlsRadiusSlider = 0,
        
        controlsColor = 0
    }
    
handleControlsEvent :: Controls -> Event -> Controls
handleControlsEvent controls event = 
    controls {
        controlsW = keyEvent (Char 'w') controlsW,
        controlsA = keyEvent (Char 'a') controlsA,
        controlsS = keyEvent (Char 's') controlsS,
        controlsD = keyEvent (Char 'd') controlsD,
        controlsQ = keyEvent (Char 'q') controlsQ,
        controlsE = keyEvent (Char 'e') controlsE,
        controlsZ = keyEvent (Char 'z') controlsZ,
        controlsX = keyEvent (Char 'x') controlsX,
        controls_m = keyEvent (Char 'm') controls_m,
        controls_M = keyEvent (Char 'M') controls_M,
        controls_r = keyEvent (Char 'r') controls_r,
        controls_R = keyEvent (Char 'R') controls_R,
        controls_c = keyEvent (Char 'c') controls_c,
        controls_C = keyEvent (Char 'C') controls_C,
        
        controlsLMB = mouseButtonEvent LeftButton controlsLMB,
        controlsRMB = mouseButtonEvent RightButton controlsRMB,
        
        controlsMousePos = mousePos,
        
        controlsMouseWheel = mouseWheel
    }
    where keyEvent ky =
            handleKeyState $
            case event 
            of EventKey key keyState _ _ -> if key == ky
                                            then Just keyState
                                            else Nothing
               _ -> Nothing
          mouseButtonEvent b =
            handleKeyState $
            case event
            of EventKey (MouseButton btn) keyState _ _ -> if btn == b
                                                          then Just keyState
                                                          else Nothing
               _ -> Nothing
          handleKeyState (Just state) controlVal =
            case state
            of Down -> BasicEvent {
                        basicEventPressed = True,
                        basicEventIsDown = True,
                        basicEventReleased = False
                       }
               Up -> BasicEvent {
                        basicEventPressed = False,
                        basicEventIsDown = False,
                        basicEventReleased = True
                     }
          handleKeyState Nothing controlVal = controlVal controls
          mousePos =
            case event
            of EventKey _ _ _ pos -> pos
               EventMotion pos -> pos
               _ -> controlsMousePos controls
               
          mouseWheel =
            case event
            of EventKey (MouseButton WheelUp) Up _ _ -> 1
               EventKey (MouseButton WheelDown) Up _ _ -> -1
               _ -> controlsMouseWheel controls
          
createControlsUpdater :: Updater (Controls, Universe, Camera)
createControlsUpdater = createUpdater ctrlUR updateFunc
 where 
 updateFunc dt (controls, universe, camera) =
  (
  controls',
  universe',
  camera'
  )
  where
  ev ctrl = getBasicEvent $ ctrl controls
  (w', w) = ev controlsW
  (a', a) = ev controlsA
  (s', s) = ev controlsS
  (d', d) = ev controlsD
  (q', q) = ev controlsQ
  (e', e) = ev controlsE
  (z', z) = ev controlsZ
  (x', x) = ev controlsX
  (_m', _m) = ev controls_m
  (_M', _M) = ev controls_M
  (_r', _r) = ev controls_r
  (_R', _R) = ev controls_R
  (_c', _c) = ev controls_c
  (_C', _C) = ev controls_C
  (lmb', lmb) = ev controlsLMB
  (rmb', rmb) = ev controlsRMB
  mousePos@(mx, my) = controlsMousePos controls
  mouseWheel = controlsMouseWheel controls
  controls' =
    controls {
        controlsW = w',
        controlsA = a',
        controlsS = s',
        controlsD = d',
        controlsQ = q',
        controlsE = e',
        controlsZ = z',
        controlsX = x',
        controls_m = _m',
        controls_M = _M',
        controls_r = _r',
        controls_R = _R',
        controls_c = _c',
        controls_C = _C',
        controlsLMB = lmb',
        controlsRMB = rmb',
        
        controlsMouseWheel = 0,
    
        controlsOldMousePos =
        if basicEventPressed lmb
        then mousePos
        else controlsOldMousePos controls,
        controlsLaunching = basicEventIsDown lmb,
        
        controlsZoom = controlsZoom controls + mouseWheel,
        
        controlsMassSlider = 
            controlsMassSlider controls +
            (if basicEventPressed _m then 1 else 0) +
            (if basicEventPressed _M then -1 else 0),
        controlsRadiusSlider =
            controlsRadiusSlider controls +
            (if basicEventPressed _r then 1 else 0) +
            (if basicEventPressed _R then -1 else 0),
            
        controlsColor = 
            (`mod` length colorsList) $
            controlsColor controls +
            (if basicEventPressed _c then 1 else 0) +
            (if basicEventPressed _C then -1 else 0)
            
    }
  universe' = 
    let bodies = universeBodies universe
        newBody = 
            if basicEventReleased lmb
            then [createBody 
                 (massFromExponent $ controlsMassSlider controls) 
                 (radiusFromExponent $ controlsRadiusSlider controls) 
                 (sx, sz)
                 (sx - ex, sz - ez)
                 (colorsList !! controlsColor controls)]
            else []
        (sx, _, sz) = mapCameraScreenToWorld camera $
                      controlsOldMousePos controls
        (ex, _, ez) = mapCameraScreenToWorld camera $
                      mousePos
        removeNearest bodies =
            if basicEventPressed rmb
            then remove (findNearest bodies) bodies
            else bodies
        findNearest bodies = 
            let (_, index, _) =
                 foldl
                 (\(i, j, d) b -> 
                 let dist = v2Len $ bodyPos b `v2Minus` (x, z)
                     (x, _, z) = mapCameraScreenToWorld camera mousePos
                 in if dist < d then (i + 1, i, dist) else (i + 1, j, d)) 
                 (0, 0, 1e100) bodies
            in index
    in universe {
        universeBodies = removeNearest $ newBody ++ bodies
       }
  camera' = 
    let forward = unapplyCameraYaw camera $ (0, 0, magnitude)
        right = unapplyCameraYaw camera $ (magnitude, 0, 0) 
        backward = (-1) `v3Scale` forward
        left = (-1) `v3Scale` right
        accel = useAccel (basicEventIsDown w) forward `v3Plus`
                useAccel (basicEventIsDown s) backward `v3Plus`
                useAccel (basicEventIsDown d) right `v3Plus`
                useAccel (basicEventIsDown a) left
        useAccel p v = if p then v else (0, 0, 0)
        magnitude = ctrlMA / cameraZoom camera
        
        yawAccel = useYawAccel (basicEventIsDown q) (-yawMagnitude) +
                   useYawAccel (basicEventIsDown e) yawMagnitude
        useYawAccel p a = if p then a else 0
        yawMagnitude = ctrlYA
        
        pitchAccel = usePitchAccel (basicEventIsDown z) (-pitchMagnitude) +
                     usePitchAccel (basicEventIsDown x) pitchMagnitude
        usePitchAccel p a = if p then a else 0
        pitchMagnitude = ctrlPA
    in camera {
        cameraVel = vel `v3Plus` (dt `v3Scale` accel),
        cameraYawVel = yawVel + dt * yawAccel,
        cameraPitchVel = pitchVel + dt * pitchAccel,
        cameraZoom = camSZ * camZF ** fromIntegral (controlsZoom controls)
       }
  Camera {
    cameraVel = vel,
    cameraYawVel = yawVel,
    cameraPitchVel = pitchVel
  } = camera
            
drawControls :: Controls -> Picture
drawControls controls =
    pictures
    [
    if controlsLaunching controls
    then launchLinePicture
    else blank,
    settingsPicture
    ]
    where p0 = controlsOldMousePos controls
          p1 = controlsMousePos controls
          launchLinePicture = color red $ line [p0, p1]
          
          massSlider = massFromExponent $ controlsMassSlider controls
          radiusSlider = radiusFromExponent $ controlsRadiusSlider controls
          settingsPicture =
            translate (30 - demoHSW) (demoHSH - 40) $
            scale 0.2 0.2 $
            pictures [
                color white $
                text $ "Mass: " ++ show massSlider,
                color white $
                translate 0 (-150) $ text $ "Radius: " ++ show radiusSlider,
                color (colorsList !! controlsColor controls) $
                translate 100 (-250) $
                rectangleSolid 200 100
            ]
            
massFromExponent :: Int -> Float
massFromExponent exp = 1 * ctrlMF ** fromIntegral exp

radiusFromExponent :: Int -> Float
radiusFromExponent exp = 1 * ctrlRF ** fromIntegral exp