{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Gox.Turtle where

-- linear
import Linear

-- lens
import Control.Lens

data Turtle = Turtle
  { _turtleDir :: V3 Double
  , _turtlePos :: V3 Double
  , _turtleRight :: V3 Double
  }
  deriving (Show, Eq)
makeLenses ''Turtle

turnRight :: Double -> Turtle -> Turtle
turnRight angle Turtle {..} =
  let axis = normalize $ cross _turtleDir _turtleRight
      rotQuat = axisAngle axis angle
      newDir = normalize $ rotate rotQuat _turtleDir
      newRight = normalize $ rotate rotQuat _turtleRight
  in Turtle { _turtleDir = newDir, _turtleRight = newRight, .. }

turnLeft :: Double -> Turtle -> Turtle
turnLeft angle = turnRight (-angle)

pitchUp :: Double -> Turtle -> Turtle
pitchUp angle Turtle {..} =
  Turtle { _turtleDir = normalize (rotate (axisAngle _turtleRight angle) _turtleDir), .. }

pitchDown :: Double -> Turtle -> Turtle
pitchDown angle = pitchUp (-angle)

rollRight :: Double -> Turtle -> Turtle
rollRight angle Turtle {..} =
  Turtle { _turtleRight = normalize (rotate (axisAngle _turtleDir angle) _turtleRight), .. }

rollLeft :: Double -> Turtle -> Turtle
rollLeft angle = rollRight (-angle)

move :: Double -> Turtle -> Turtle
move distance Turtle {..} =
  Turtle { _turtlePos = _turtlePos + _turtleDir ^* distance, .. }
