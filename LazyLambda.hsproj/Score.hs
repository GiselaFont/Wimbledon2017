module Score where
  
import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState

--set score labels
gamescore1 :: Int -> LambdaNode
gamescore1 x = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "gameScore1"
        , nodePosition  = Point ((width/2) - 30) (height/2)
        , nodeZPosition = 100
        , labelText     = show x
        , labelFontColor = redColor
        }
        
gamescore2 :: Int -> LambdaNode
gamescore2 x = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "gameScore2"
        , nodePosition  = Point ((width/2) + 30) (height/2)
        , nodeZPosition = 100
        , labelText     = show x
        , labelFontColor = blueColor
        }
        
gamescore :: LambdaNode
gamescore = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "gameScore"
        , nodePosition  = Point ((width/2)) (height/2)
        , nodeZPosition = 100
        , labelText     = "-"
        }

        
setscoreP1 :: LambdaNode
setscoreP1 = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "setScorep1"
        , nodePosition  = Point ((width / 2)-70) (height / 50)
        , nodeZPosition = 100
        , labelText     = "Player: "
        , labelFontColor = redColor
        }
        
setscoreP2 :: LambdaNode
setscoreP2 = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "setScorep2"
        , nodePosition  = Point ((width / 2)+60) (height / 50)
        , nodeZPosition = 100
        , labelText     = "Player: "
        , labelFontColor = blueColor
        }

        
setscore1 :: Int -> LambdaNode
setscore1 x = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "setScore1"
        , nodePosition  = Point ((width / 2)-10) (height / 50)
        , nodeZPosition = 100
        , labelText     = show x
        , labelFontColor = redColor
        }
        
setscore2 :: Int -> LambdaNode
setscore2 x = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "setScore2"
        , nodePosition  = Point ((width / 2) + 120) (height / 50)
        , nodeZPosition = 100
        , labelText     = show x
        , labelFontColor = blueColor
        }

    
--score actions
--reset
reset :: LambdaScene -> LambdaScene 
reset scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore1" setScore]
    , sceneData             = sceneState{ sceneScore1 = newScore }
    }
  where
    newScore = 0
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
reset1 :: LambdaScene -> LambdaScene 
reset1 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore2" setScore]
    , sceneData             = sceneState{ sceneScore2 = newScore }
    }
  where
    newScore = 0
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node



--update scores
incScore1 :: LambdaScene -> LambdaScene 
incScore1 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore1" setScore]
    , sceneData             = sceneState{ sceneScore1 = newScore }
    }
  where
    score = sceneScore1 sceneState
    newScore | score == 0 = score + 15
             | score == 15 = score + 15
             | score == 30 = score + 10
             | score == 40 = 0
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
incScore2 :: LambdaScene -> LambdaScene 
incScore2 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore2" setScore]
    , sceneData             = sceneState{ sceneScore2 = newScore }
    }
  where
    score = sceneScore2 sceneState
    newScore | score == 0 = score + 15
             | score == 15 = score + 15
             | score == 30 = score + 10
             | score == 40 = 0
                    
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
incScore3 :: LambdaScene -> LambdaScene 
incScore3 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "setScore1" setScore]
    , sceneData             = sceneState{ gameScore1 = newScore }
    }
  where
    newScore = gameScore1 sceneState + 1
      
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
incScore4 :: LambdaScene -> LambdaScene 
incScore4 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "setScore2" setScore]
    , sceneData             = sceneState{ gameScore2 = newScore }
    }
  where
    newScore = gameScore2 sceneState + 1
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
incScore5 :: LambdaScene -> LambdaScene 
incScore5 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore1" setScore]
    , sceneData             = sceneState{ sceneScore1 = newScore }
    }
  where
    newScore = sceneScore1 sceneState + 1
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node
    
incScore6 :: LambdaScene -> LambdaScene 
incScore6 scene@Scene{ sceneData = sceneState }
    = scene
    { sceneActionDirectives = [runCustomActionOn "gameScore2" setScore]
    , sceneData             = sceneState{ sceneScore2 = newScore }
    }
  where
    newScore = sceneScore2 sceneState + 1
  
    setScore label@Label{} _dt = label{ labelText = show newScore 
    }
    setScore node          _   = node

    
