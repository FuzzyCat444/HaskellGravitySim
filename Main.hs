
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Debug.Trace

import Util
import Camera
import Controls
import Universe
import Grid
import Constants

data State =
    State {
        stateControls :: !Controls,
        stateUniverse :: !Universe,
        stateCamera :: !Camera,
        stateGrid :: !Grid,
        
        stateControlsUpdater :: !(Updater (Controls, Universe, Camera)),
        stateUniverseUpdater :: !(Updater Universe),
        stateCameraUpdater :: !(Updater Camera),
        stateGridUpdater :: !(Updater (Grid, Universe, Camera))
    }
    
initialState :: (Int, Int) -> State
initialState (wi, hi) =
    State {
        stateControls = createControls,
        stateUniverse = createUniverse,
        stateCamera = createCamera dims (0, 0, 0) 0.5 0.5 camSZ,
        stateGrid = emptyGrid,
        
        stateControlsUpdater = createControlsUpdater,
        stateUniverseUpdater = createUniverseUpdater,
        stateCameraUpdater = createCameraUpdater,
        stateGridUpdater = createGridUpdater
    }
    where dims = (fromIntegral wi, fromIntegral hi)
    
pollEvents :: Event -> State -> State
pollEvents event state =
    state {
        stateControls = handleControlsEvent controls event
    }
    where controls = stateControls state

update :: Float -> State -> State
update delta state =
    state {
        stateControlsUpdater = controlsUpdater',
        stateUniverseUpdater = universeUpdater',
        stateCameraUpdater = cameraUpdater',
        stateGridUpdater = gridUpdater',
        
        stateControls = controls',
        stateUniverse = universe'',
        stateCamera = camera'',
        stateGrid = grid'
    }
    where (controlsUpdater', (controls', universe', camera'))
            = runUpdater delta controlsUpdater (controls, universe, camera)
          (universeUpdater', universe'')
            = runUpdater delta universeUpdater universe'
          (cameraUpdater', camera'') =
            runUpdater delta cameraUpdater camera'
          (gridUpdater', (grid', _, _)) =
            runUpdater delta gridUpdater (grid, universe'', camera'')
          State {
            stateControls = controls,
            stateUniverse = universe,
            stateCamera = camera,
            stateGrid = grid,
          
            stateControlsUpdater = controlsUpdater,
            stateUniverseUpdater = universeUpdater,
            stateCameraUpdater = cameraUpdater,
            stateGridUpdater = gridUpdater
          } = state

draw :: State -> Picture
draw state =
    pictures [
        drawGrid grid camera,
        drawUniverse universe camera,
        drawControls controls
    ]
    where camera = stateCamera state
          universe = stateUniverse state
          grid = stateGrid state
          controls = stateControls state

main :: IO ()
main = do
    (width, height) <- getScreenSize
    let res@(x, y) = (floor demoSW, floor demoSH)
    let pos = ((width - x) `div` 2, (height - y) `div` 2)
    play (InWindow "Gravity Demo" res pos) 
         black 
         demoUR
         (initialState res)
         draw 
         pollEvents 
         update
          