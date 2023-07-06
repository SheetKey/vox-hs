{-# LANGUAGE RecordWildCards #-}

module Gox.Tree.Algorithm where

-- gox-hs
import Gox.Turtle
import Gox.Tree.Type

-- linear
import Linear

-- lens
import Control.Lens

-- mtl
import Control.Monad.State.Class
import Control.Monad.Reader.Class

-- vector
import qualified Data.Vector as V

-- MonadRandom
import Control.Monad.Random.Lazy

validateParameters :: Parameters -> Bool
validateParameters = undefined

stemFromDepth :: Int -> Stem
stemFromDepth _sDepth = Stem
  { _sCurve          = Curve V.empty
  , _sParent         = Nothing
  , _sOffset         = 0
  , _sRadiusLimit    = -1
  , _sChildren       = []
  , _sLength         = 0
  , _sRadius         = 0
  , _sLengthChildMax = 0
  , ..
  }

saacos :: Double -> Double
saacos fac =
  if fac <= -1 then pi
  else if fac >= 1 then 0
       else acos fac

mulQtQt :: Quaternion Double -> Quaternion Double -> Quaternion Double
mulQtQt (Quaternion q10 (V3 q11 q12 q13)) (Quaternion q20 (V3 q21 q22 q23)) =
  let t0 = q10 * q20 - q11 * q21 - q12 * q22 - q13 * q23
      t1 = q10 * q21 + q11 * q20 + q12 * q23 - q13 * q22
      t2 = q10 * q22 + q12 * q20 + q13 * q21 - q11 * q23
      t3 = q10 * q23 + q13 * q20 + q11 * q22 - q12 * q21
  in Quaternion t0 (V3 t1 t2 t3)

toTrackQuatZY :: V3 Double -> Quaternion Double
toTrackQuatZY v =
  let axis = if abs (v ^._x) + abs (v ^._y) < (10 ^ (-4))
            then V3 1 (v ^._x) 0
            else V3 (negate (v ^._y)) (v ^._x) 0
      co = (v ^._z) / 3
      q = axisAngle axis (saacos co)
      mat = fromQuaternion q
      fp = mat ^._z
      angle = -0.5 * atan2 (negate fp ^._x) (negate fp ^._y)
      co' = cos angle
      si' = (sin angle) / 3
      q2 = Quaternion co' (v ^* si')
  in mulQtQt q q2

calcHelixPoint :: RandomGen g => Double -> Double
  -> RG g (V3 Double, V3 Double, V3 Double, V3 Double)
calcHelixPoint rad pitch = do
  spinAng <- getRandomR (0, 2 * pi)
  Just dir <- preview $ treeState.turtle._Just.turtleDir
  let p0 = V3 0 (negate rad) (negate pitch / 4)
      p1 = V3 (4 * rad/ 3) (negate rad) 0
      p2 = V3 (4 * rad / 3) rad 0
      p3 = V3 0 rad (pitch / 4) 
      trf = toTrackQuatZY dir
      rotQuat = Quaternion spinAng (V3 0 0 1)
      p0' = rotate trf $ rotate rotQuat p0
      p1' = rotate trf $ rotate rotQuat p1
      p2' = rotate trf $ rotate rotQuat p2
      p3' = rotate trf $ rotate rotQuat p3
  return (p1' - p0', p2' - p0', p3' - p0', dir)

calcShapeRatio :: PShape -> Double -> R g Double
calcShapeRatio shape ratio =
  case shape of
    Spherical -> return $ 0.2 + 0.8 * sin (pi * ratio)
    Hemispherical -> return $ 0.2 + 0.8 * sin (0.5 * pi * ratio)
    Cylindrical -> return 1
    TaperedCylindrical -> return $ 0.5 + 0.5 * ratio
    Flame -> return $ if ratio <= 0.7 then ratio / 0.7 else (1 - ratio) / 0.3
    InverseConical -> return $ 1 - 0.8 * ratio
    TendFlame -> return $ if ratio <= 0.7
                          then 0.5 + 0.5 * ratio / 0.7
                          else 0.5 + 0.5 * (1 - ratio) / 0.3
    Envelope -> if ratio < 0 || ratio > 1
         then return 0
         else do Parameters {..} <- view $ treeRG.parameters
                 if ratio < 1 - _pPruneWidthPeak
                   then return $ (ratio / (1 - _pPruneWidthPeak)) ** _pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - _pPruneWidthPeak)) ** _pPrunePowerLow
    Conical -> return $ 0.2 + 0.8 * ratio
