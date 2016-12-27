{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState
import Rackets
import Random
import Walls
import Score

lazyLambda :: LambdaScene
lazyLambda
  = (sceneWithSize (Size width height))
    { sceneData             = initialSceneState
    , sceneChildren         = [court, key, ball 0.6 0.22, racket1, racket2, groundPhysics, groundPhysics1, groundPhysics2, groundPhysics2', groundPhysics3, groundPhysics3', gamescore1 0, gamescore2 0, gamescore, setscoreP1, setscoreP2, setscore1 0, setscore2 0]
    , sceneUpdate           = Just update 
    , scenePhysicsWorld     = physicsWorld
                              { worldGravity = Vector 0 0
                              , worldContactDidBegin = Just contact
                               }                   
    , sceneHandleEvent      = Just handleEvent 
    }
    where
      key = (pressKey "Press space bar to start" (width/2) ((height/2)+40))
      racket1 = racket 0.6 0.1
      racket2 = racket' 0.3 0.9

    
(tennisTexture, ballWidth, ballHeight) = defineTexture "ball.png"
(courtTexture, courtWidth1, courtHeight1)                  = defineTexture "tenniscourt.jpg"

--Press key Label
pressKey :: String -> Double -> Double -> LambdaNode
pressKey x y z = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "PressKey"
        , nodePosition  = Point y z
        , nodeZPosition = 100
        , labelText     = x
        }
        
--Congratulations label
winner :: String -> Color -> LambdaNode
winner x y = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "Congrats"
        , nodePosition  = Point (width/2) ((height/2)+60)
        , nodeZPosition = 100
        , labelText     = x
        , labelFontColor    = y
        , labelFontSize     = 50
        }
        
--initialize ball and tennis court
ball :: Double -> Double -> LambdaNode
ball x y = (spriteWithTexture tennisTexture)
       { nodeName             = Just "ball"
       , nodePosition         = Point (width * x) (height * y)
       , nodeXScale           = 0.1
       , nodeYScale           = 0.1
       , nodePhysicsBody      = Just $
               (bodyWithCircleOfRadius (ballHeight * 0.1) Nothing) { bodyCategoryBitMask    = categoryBitMask [Bird]
               , bodyCollisionBitMask   = categoryBitMask [World]
               , bodyAllowsRotation = False
               , bodyContactTestBitMask = categoryBitMask [World, Score]
               }}
               
court :: LambdaNode
court = (spriteWithTexture courtTexture)
       { nodeName             = Just "court"
       , nodePosition         = Point (width / 2) (height / 2)
        }
       
--update scene
update :: LambdaScene -> TimeInterval -> LambdaScene
update scene@Scene{ sceneData = sceneState@SceneState{..}} _dt 
  = case gameState of
      Running
        | spacePressed && space == False && bstartright -> (bumpLambda True False False False) scene{ sceneData = sceneState{ spacePressed = False, space = True }, sceneChildren = children1}
        | spacePressed && space == False && bstartleft -> (bumpLambda False True False False) scene{ sceneData = sceneState{ spacePressed = False, space = True } }
        | spacePressed && space == False && rstartright -> (bumpLambda False False True False) scene{ sceneData = sceneState{ spacePressed = False, space = True } }
        | spacePressed && space == False && rstartleft -> (bumpLambda False False False True) scene{ sceneData = sceneState{ spacePressed = False, space = True } }
        | leftArrowPressed -> (bumpRacket False "racket1" (-150) 0) scene {sceneData = sceneState { leftArrowPressed = False }}
        | rightArrowPressed -> (bumpRacket False "racket1" (150) 0) scene{ sceneData = sceneState { rightArrowPressed = False }}
        | downArrowPressed -> (bumpRacket False "racket1" 0 (-150)) scene{ sceneData = sceneState { downArrowPressed = False }}
        | upArrowPressed -> (bumpRacket True "racket1" 0 150) scene{ sceneData = sceneState { upArrowPressed = False }}
        | zArrowPressed -> (bumpRacket False "racket2" (-150) 0) scene {sceneData = sceneState { zArrowPressed = False }}
        | cArrowPressed -> (bumpRacket False "racket2" 150 0) scene{ sceneData = sceneState { cArrowPressed = False }}
        | xArrowPressed -> (bumpRacket True "racket2" 0 (-150)) scene{ sceneData = sceneState { xArrowPressed = False }}
        | sArrowPressed -> (bumpRacket False "racket2" 0 150) scene{ sceneData = sceneState { sArrowPressed = False }}
        | bumpScore1 && bstartright -> incScore1 scene { sceneData = sceneState { bumpScore1 = False, space = False, bstartright = False, bstartleft = True }, sceneChildren = children2}
        | bumpScore1 && bstartleft -> incScore1 scene { sceneData = sceneState { bumpScore1 = False, space = False, bstartright = True, bstartleft = False }, sceneChildren = children1}
        | bumpScore1 && rstartright -> incScore1 scene { sceneData = sceneState { bumpScore1 = False, space = False, rstartright = False, rstartleft = True }, sceneChildren = children4}
        | bumpScore1 && rstartleft -> incScore1 scene { sceneData = sceneState { bumpScore1 = False, space = False, rstartright = True, rstartleft = False }, sceneChildren = children3}
        | bumpScore2 && bstartright -> incScore2 scene { sceneData = sceneState { bumpScore2 = False, space = False, bstartright = False, bstartleft = True }, sceneChildren = children2}
        | bumpScore2 && bstartleft -> incScore2 scene { sceneData = sceneState { bumpScore2 = False, space = False, bstartright = True, bstartleft = False }, sceneChildren = children1}
        | bumpScore2 && rstartright -> incScore2 scene { sceneData = sceneState { bumpScore2 = False, space = False, rstartright = False, rstartleft = True }, sceneChildren = children4}
        | bumpScore2 && rstartleft -> incScore2 scene { sceneData = sceneState { bumpScore2 = False, space = False, rstartright = True, rstartleft = False }, sceneChildren = children3}
        | bumpScore3 && blue -> incScore3 scene { sceneData = sceneState { bumpScore3 = False, space = False, bstartright = False, bstartleft = False, rstartright = True }, sceneChildren = children3}
        | bumpScore3 && red -> incScore3 scene { sceneData = sceneState { bumpScore3 = False, space = False, rstartright = False, rstartleft = False, bstartright = True }, sceneChildren = children1}
        | bumpScore4 && blue -> incScore4 scene { sceneData = sceneState { bumpScore4 = False, space = False, bstartright = False, bstartleft = False, rstartright = True }, sceneChildren = children3}
        | bumpScore4 && red -> incScore4 scene { sceneData = sceneState { bumpScore4 = False, space = False, rstartright = False, rstartleft = False, bstartright = True }, sceneChildren = children1}
        | bumpTieBreak && bstartright -> incScore5 scene { sceneData = sceneState { bumpTieBreak = False, space = False, bstartright = False, rstartleft = True}, sceneChildren = children4}
        | bumpTieBreak && bstartleft -> incScore5 scene { sceneData = sceneState { bumpTieBreak = False, space = False, rstartright = True, bstartleft = False}, sceneChildren = children3}
        | bumpTieBreak && rstartright -> incScore5 scene { sceneData = sceneState { bumpTieBreak = False, space = False, rstartright = False, bstartleft = True}, sceneChildren = children2}
        | bumpTieBreak && rstartleft -> incScore5 scene { sceneData = sceneState { bumpTieBreak = False, space = False, bstartright = True, rstartleft = False}, sceneChildren = children1}
        | bumpTieBreak1 && bstartright -> incScore6 scene { sceneData = sceneState {bumpTieBreak1 = False, space = False, bstartright = False, rstartleft = True }, sceneChildren = children4}
        | bumpTieBreak1 && bstartleft -> incScore6 scene { sceneData = sceneState {bumpTieBreak1 = False, space = False, rstartright = True, bstartleft = False }, sceneChildren = children3}
        | bumpTieBreak1 && rstartright -> incScore6 scene { sceneData = sceneState {bumpTieBreak1 = False, space = False, rstartright = False, bstartleft = True }, sceneChildren = children2}
        | bumpTieBreak1 && rstartleft -> incScore6 scene { sceneData = sceneState {bumpTieBreak1 = False, space = False, bstartright = True, rstartleft = False }, sceneChildren = children1}
        | resetScore -> reset scene {sceneData = sceneState { resetScore = False }}
        | resetScore1 -> reset1 scene {sceneData = sceneState { resetScore1 = False }}
        | resetScene && player1 -> scene {sceneData = sceneState { resetScene = False, player1 = False }, sceneChildren = children5}
       | resetScene && player2 -> scene {sceneData = sceneState { resetScene = False, player2 = False }, sceneChildren = children6}
        | otherwise -> emptyScene scene {sceneData = sceneState { spacePressed = False } } 
      Over
        | enterPressed -> resetl scene {sceneData = sceneState { enterPressed = False, sceneScore1 = 0, sceneScore2 = 0, gameScore1 = 0, gameScore2 = 0 } }
       
    where
         children1 = [court, ball 0.6 0.2, racket 0.6 0.1, racket' 0.3 0.9, groundPhysics, groundPhysics1, groundPhysics2, groundPhysics2', groundPhysics3, groundPhysics3', gamescore1 sceneScore1, gamescore2 sceneScore2, gamescore, setscoreP1, setscoreP2, setscore1 gameScore1, setscore2 gameScore2]
         children2 = [court, ball 0.4 0.2, racket 0.4 0.1, racket' 0.7 0.9, groundPhysics, groundPhysics1, groundPhysics2, groundPhysics2', groundPhysics3, groundPhysics3', gamescore1 sceneScore1, gamescore2 sceneScore2, gamescore, setscoreP1, setscoreP2, setscore1 gameScore1, setscore2 gameScore2]
         children3 = [court, ball 0.4 0.8, racket 0.7 0.15, racket' 0.4 0.9, groundPhysics, groundPhysics1, groundPhysics2, groundPhysics2', groundPhysics3, groundPhysics3', gamescore1 sceneScore1, gamescore2 sceneScore2, gamescore, setscoreP1, setscoreP2, setscore1 gameScore1, setscore2 gameScore2]
         children4 = [court, ball 0.6 0.8, racket 0.3 0.15, racket' 0.6 0.9, groundPhysics, groundPhysics1, groundPhysics2, groundPhysics2', groundPhysics3, groundPhysics3', gamescore1 sceneScore1, gamescore2 sceneScore2, gamescore, setscoreP1, setscoreP2, setscore1 gameScore1, setscore2 gameScore2]
         children5 = [court, winner "Player1 wins!!" redColor, pressKey "Press delete to play again" (width/2) ((height/2)+10)]
         children6 = [court, winner "Player2 wins!!" blueColor, pressKey "Press delete to play again" (width/2) ((height/2)+10)]
         red = (rstartright || rstartleft)
         blue = (bstartright || bstartleft)


emptyScene :: LambdaScene -> LambdaScene
emptyScene scene
  = scene { sceneActionDirectives = [] }

--move ball
bumpLambda :: Bool -> Bool -> Bool -> Bool -> LambdaScene -> LambdaScene
bumpLambda x y z w scene@Scene{sceneData = sceneState}
  = scene { sceneActionDirectives = [runCustomActionOn "ball" (bumpAction x y z w)] }

--move racket1
bumpRacket :: Bool -> String -> Double -> Double -> LambdaScene -> LambdaScene
bumpRacket t x y z scene
  = scene { sceneActionDirectives = [runCustomActionOn x (racketAction t y z)] }


--key press event 
handleEvent :: Event -> SceneState -> Maybe SceneState
handleEvent KeyEvent { keyEventKeyCode = 49 } state  = Just state{ spacePressed = True } 
handleEvent KeyEvent{ keyEventKeyCode = 123 } state = Just state{ leftArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode = 124 } state = Just state{ rightArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode = 125 } state = Just state{ downArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode = 126 } state = Just state{ upArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode = 1 } state = Just state{ sArrowPressed = True } 
handleEvent KeyEvent{ keyEventKeyCode = 8 } state = Just state{ cArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode = 7 } state = Just state{ xArrowPressed = True }
handleEvent KeyEvent{ keyEventKeyCode =  6} state = Just state{ zArrowPressed = True }
handleEvent KeyEvent { keyEventKeyCode = 51 } state  = Just state{ enterPressed = True, gameState = Over } 
handleEvent _ _  = Nothing 
                
--contact ball with walls to update score        
contact state@SceneState{..} PhysicsContact{..}
  | (player1 || player1' || player1'')
  = (Just state{ resetScene = True, player1 = True }, Nothing, Nothing)
  | (player2 || player2' || player2'')
  = (Just state{ resetScene = True, player2 = True }, Nothing, Nothing)
  | (tiebreak && pointp1 && sceneScore1 /= 5)
  = (Just state{ bumpTieBreak = True }, Nothing, Nothing)
  | (tiebreak && pointp2 && sceneScore2 /= 5)
  = (Just state{ bumpTieBreak1 = True }, Nothing, Nothing)
  |  pointp1 && ((sceneScore1 == 40) || (sceneScore1 == 5))
  = (Just state{ bumpScore3 = True, bumpScore1 = True, resetScore1 = True}, Nothing, Nothing)
  | pointp2 && ((sceneScore2 == 40) || (sceneScore2 == 5))
  = (Just state{ bumpScore4 = True, bumpScore2 = True, resetScore = True}, Nothing, Nothing)
  | pointp1 && gameState == Running
  = (Just state{ bumpScore1 = True }, Nothing, Nothing)
  | pointp2 && gameState == Running
  = (Just state{ bumpScore2 = True }, Nothing, Nothing)
  | otherwise
  = (Nothing, Nothing, Nothing)
  where
    ballground = ((nodeName contactBodyA == Just "ground") && (nodeName contactBodyB == Just "ball"))
    balltop = ((nodeName contactBodyA == Just "top") && (nodeName contactBodyB == Just "ball"))
    ballleftbottom = ((nodeName contactBodyA == Just "leftbottom") && (nodeName contactBodyB == Just "ball"))
    balllefttop = ((nodeName contactBodyA == Just "lefttop") && (nodeName contactBodyB == Just "ball"))
    ballrightbottom = ((nodeName contactBodyA == Just "rightbottom") && (nodeName contactBodyB == Just "ball"))
    ballrighttop = ((nodeName contactBodyA == Just "righttop") && (nodeName contactBodyB == Just "ball"))
    tiebreak = (gameScore1 == 2 && gameScore2 == 2)
    player1 = ((gameScore1 == 1 && gameScore2 == 0) && sceneScore1 == 40) && pointp1
    player2 = ((gameScore1 == 0 && gameScore2 == 1) && sceneScore2 == 40) && pointp2
    player1' = ((gameScore1 == 2 && gameScore2 == 1) && sceneScore1 == 40) && pointp1
    player2' = ((gameScore1 == 1 && gameScore2 == 2) && sceneScore2 == 40) && pointp2
    player1'' = ((gameScore1 == 2 && gameScore2 == 2) && sceneScore1 == 4) && pointp1
    player2'' = ((gameScore1 == 2 && gameScore2 == 2) && sceneScore2 == 4) && pointp2
    pointp1 = (ballground || ballleftbottom || ballrightbottom)
    pointp2 = (balltop || balllefttop || ballrighttop)
    
        
resetl :: LambdaScene -> LambdaScene 
resetl scene@Scene{ sceneData = sceneState } = lazyLambda
  
