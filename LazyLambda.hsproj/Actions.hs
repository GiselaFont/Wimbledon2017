module Actions where
  
import Graphics.SpriteKit
import Data.Fixed
import System.Random

import GameState
import Random
import Constants


-- The bump used to lift Lambda on key press.
--
bumpAction :: Bool -> Bool -> Bool -> Bool -> LambdaNode -> TimeInterval -> LambdaNode
bumpAction bluer bluel redr redl sprite@Sprite{ nodePhysicsBody = Just body } _dt
   = sprite
    { nodePhysicsBody
        = Just body
               { bodyVelocity          = vectorZero
               , bodyForcesAndImpulses = [newVector]
               ,bodyRestitution = 1
               }
    }
    where
      newVector | bluer = ApplyImpulse (Vector (-6) (-10)) Nothing
                | bluel = ApplyImpulse (Vector (6) (-10)) Nothing
                | redr = ApplyImpulse (Vector (6) (10)) Nothing
                | redl = ApplyImpulse (Vector (-6) (10)) Nothing
bumpAction bluer bluel redr redl node _dt = node
         
                     
-- Move rackets
--
racketAction :: Bool -> Double -> Double -> LambdaNode -> TimeInterval -> LambdaNode
racketAction t x y sprite@Sprite{ nodeName = z, nodePosition = Point a b, nodePhysicsBody = Just body } _dt
        = sprite
          { nodePhysicsBody
            = Just body
                  {bodyVelocity  = if((t && ((b >= height/2 && z == Just "racket1") || (b <= height/2 && z == Just "racket2"))))
                                   then Vector 0 0
                                   else Vector x y
                  ,bodyAllowsRotation = False
                  ,bodyMassOrDensity = Mass 10
                  ,bodyRestitution = 0
           }

      }
racketAction t x y node _dt = node

-- Tilt Lambda in dependence on its vertical velocity vector.
--
tiltAction :: LambdaNode -> TimeInterval -> LambdaNode
tiltAction sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodeZRotation = (-1) `max` zRotation `min` 0.5 }
  where
    zRotation = dY * (if dY < 0 then 0.003 else 0.001 )
    dY        = vectorDy . bodyVelocity $ body
tiltAction node _dt = node
