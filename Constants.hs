module Constants where

import Graphics.Gloss

-- Actual update rate for entire app
demoUR :: Int
demoUR = 240

-- Screen width
demoSW :: Float
demoSW = 1150

-- Screen height
demoSH :: Float
demoSH = 860

-- Half screen width
demoHSW :: Float
demoHSW = demoSW / 2

-- Half screen height
demoHSH :: Float
demoHSH = demoSH / 2

-- Camera velocity multiplier
camVM :: Float
camVM = 0.005

-- Camera yaw velocity multiplier
camYVM :: Float
camYVM = 0.003

-- Camera pitch velocity multiplier
camPVM :: Float
camPVM = 0.02

-- Camera starting zoom
camSZ :: Float
camSZ = 1 / gridU

-- Camera zoom factor
camZF :: Float
camZF = 1.2

-- Controls movement acceleration
ctrlMA :: Float
ctrlMA = 3.7

-- Controls yaw acceleration
ctrlYA :: Float
ctrlYA = 10

-- Controls pitch acceleration
ctrlPA :: Float
ctrlPA = 5

-- Controls mass scale factor
ctrlMF :: Float
ctrlMF = 1.5

-- Controls radius scale factor
ctrlRF :: Float
ctrlRF = 1.5

-- Universe update rate
uniUR :: Float
uniUR = 100

-- Universe gravity constant
uniG :: Float
uniG = 200

-- Camera update rate
camUR :: Float
camUR = 60

-- Grid update rate
gridUR :: Float
gridUR = 25

-- Controls update rate
ctrlUR :: Float
ctrlUR = 60

-- Grid color
gridCLR :: Color
gridCLR = makeColor 0.5 0.5 0.5 1.0

-- Grid width (number of grid lines to show)
gridU :: Float
gridU = 40

-- Grid resolution per unit
gridUU :: Float
gridUU = 7.5