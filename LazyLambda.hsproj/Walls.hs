module Walls where
  
import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState

--Edges
--ground
groundPhysics :: LambdaNode
groundPhysics = (node[])
                { nodeName = Just "ground" 
                  ,nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 0)
                                                  (Point width 0))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }
--top                
groundPhysics1 :: LambdaNode
groundPhysics1 = (node[])
                { nodeName = Just "top" 
                  ,nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 height)
                                                  (Point width height))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }
--left-bottom               
groundPhysics2 :: LambdaNode
groundPhysics2 = (node[])
                { nodeName = Just "leftbottom"
                  , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 0)
                                                  (Point 0 (height/2)))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }
                
--left-top               
groundPhysics2' :: LambdaNode
groundPhysics2' = (node[])
                { nodeName = Just "lefttop"
                  , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (height/2))
                                                  (Point 0 height))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }

               
--right-bottom               
groundPhysics3 :: LambdaNode
groundPhysics3 = (node[])
                {  nodeName = Just "rightbottom"
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point width 0)
                                                  (Point width (height/2)))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }
                
--right-top               
groundPhysics3' :: LambdaNode
groundPhysics3' = (node[])
                {  nodeName = Just "righttop"
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point width (height/2))
                                                  (Point width height))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }

