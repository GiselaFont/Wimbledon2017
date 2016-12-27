module Constants where

import Graphics.SpriteKit

import Data.Bits
import Data.Word


-- Scene dimensions
--
width, height :: GFloat
width  = 460
height = 747

-- Categorisation of the various items in the physics world.
--
data PhysicsCategory = Bird       -- Lambda
                     | World      -- Pipes & the ground
                     | Score      -- Scoring nodes
                     deriving (Enum)

categoryBitMask :: [PhysicsCategory] -> Word32
categoryBitMask = foldl setCategoryBit zeroBits
  where
    setCategoryBit bits cat = bits .|. bit (fromEnum cat)

isInCategory :: PhysicsCategory -> Node u -> Bool
isInCategory cat node
  = case nodePhysicsBody node of
      Just body -> testBit (bodyCategoryBitMask body) (fromEnum cat)
      Nothing   -> False

isWorld :: Node u -> Bool
isWorld = isInCategory World

isScore :: Node u -> Bool
isScore = isInCategory Score

