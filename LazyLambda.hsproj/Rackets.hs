module Rackets where
  
import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState
import Walls


(racket1Texture, racketWidth, racketHeight)                  = defineTexture "racket1.png"
(racket2Texture, racketWidth1, racketHeight1)                  = defineTexture "racket2.png"

--initialize rackets
racket :: Double -> Double -> LambdaNode
racket y z = (spriteWithTexture racket1Texture)
       { nodeName             = Just "racket1"
       , nodePosition         = Point (width * y) (height * z)
       , nodeXScale           = 0.4
       , nodeYScale           = 0.4
       , nodeZPosition        = 0
       , nodePhysicsBody      = Just $
               (bodyWithRectangleOfSize (Size (racketWidth * 0.3) (racketHeight*0.3)) Nothing) { bodyCategoryBitMask    = categoryBitMask [World]               
        , bodyAllowsRotation = False
        , bodyMassOrDensity = Mass 1000 }
        }
               
racket' :: Double -> Double -> LambdaNode
racket' x y = (spriteWithTexture racket2Texture)
       { nodeName             = Just "racket2"
       , nodePosition         = Point (width * x) (height * y)
       , nodeXScale           = 0.4
       , nodeYScale           = 0.4
       , nodeZPosition        = 0
       , nodePhysicsBody      = Just $
               (bodyWithRectangleOfSize (Size (racketWidth1 * 0.3) (racketHeight1 * 0.3)) Nothing) { bodyCategoryBitMask    = categoryBitMask [World]               
               , bodyAllowsRotation = False
               , bodyMassOrDensity = Mass 1000}
       }     

           

