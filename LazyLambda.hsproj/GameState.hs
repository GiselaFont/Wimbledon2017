module GameState where

import Graphics.SpriteKit

  
data NodeState = PipesState [Int]      -- unbound sequence of random numbers
               | NoState

type LambdaNode = Node NodeState

randomInt :: NodeState -> (NodeState, Int)
randomInt NoState             = (NoState, 0)
randomInt (PipesState (i:is)) = (PipesState is, i)


data GameState = Running | Crash | Over
               deriving Eq

data SceneState = SceneState 
                 { sceneScore1 :: Int
                 , sceneScore2 :: Int
                 , gameScore1 :: Int
                 , gameScore2 :: Int
                 , spacePressed :: Bool
                 , leftArrowPressed :: Bool
                 , rightArrowPressed :: Bool
                 , upArrowPressed :: Bool
                 , downArrowPressed :: Bool
                 , zArrowPressed :: Bool
                 , xArrowPressed :: Bool
                 , cArrowPressed :: Bool
                 , sArrowPressed :: Bool
                 , enterPressed :: Bool
                 , bumpScore1  :: Bool
                 , bumpScore2  :: Bool
                 , bumpScore3 :: Bool
                 , bumpScore4 :: Bool
                 , bumpTieBreak :: Bool
                 , bumpTieBreak1 :: Bool
                 , resetScore :: Bool
                 , resetScore1 :: Bool
                 , resetScene :: Bool
                 , space :: Bool
                 , bstartright :: Bool
                 , bstartleft :: Bool
                 , rstartright :: Bool
                 , rstartleft :: Bool
                 , player1 :: Bool
                 , player2 :: Bool
                 , stop :: Bool
                 , gameState  :: GameState
                 }

initialSceneState 
  = SceneState 
    { sceneScore1 = 0
    , sceneScore2 = 0
    , gameScore1 = 0
    , gameScore2 = 0
    , spacePressed = False
    , leftArrowPressed = False 
    , rightArrowPressed = False
    , upArrowPressed = False
    , downArrowPressed = False
    , zArrowPressed = False
    , xArrowPressed = False
    , cArrowPressed = False
    , sArrowPressed = False
    , enterPressed = False
    , bumpScore1  = False
    , bumpScore2 = False
    , bumpScore3 = False
    , bumpScore4 = False
    , bumpTieBreak = False
    , bumpTieBreak1 = False
    , resetScore = False
    , resetScore1 = False
    , resetScene = False
    , space = False
    , bstartright = True
    , bstartleft = False
    , rstartright = False
    , rstartleft = False
    , player1 = False
    , player2 = False
    , stop = False
    , gameState  = Running
    }
    

type LambdaScene = Scene SceneState NodeState
