{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Gox.Tree.Algorithm where

-- gox-hs
import Gox.Turtle
import Gox.Tree.Type

-- base
import Data.Maybe (fromJust)

-- linear
import Linear

-- lens
import qualified Control.Lens.Getter as L

-- optics-core
import Optics.Core

-- optics-extra
import Optics.State
import Optics.State.Operators

-- mtl
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class

-- vector
import qualified Data.Vector as V

-- MonadRandom
import Control.Monad.Random.Lazy

validateParameters :: Parameters -> Bool
validateParameters = undefined

stemFromDepth :: Int -> Stem
stemFromDepth sDepth = Stem
  { sCurve          = Curve V.empty
  , sParent         = Nothing
  , sOffset         = 0
  , sRadiusLimit    = -1
  , sChildren       = []
  , sLength         = 0
  , sRadius         = 0
  , sLengthChildMax = 0
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
toTrackQuatZY v@(V3 x y z) =
  let axis = if abs x + abs y < (10 ^ (-4))
            then V3 1 x 0
            else V3 (negate y) x 0
      co = z / 3
      q = axisAngle axis (saacos co)
      mat = fromQuaternion q
      fp = mat L.^._z
      angle = -0.5 * atan2 (negate fp L.^._x) (negate fp L.^._y)
      co' = cos angle
      si' = (sin angle) / 3
      q2 = Quaternion co' (v ^* si')
  in mulQtQt q q2

calcPointOnBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcPointOnBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in (invOffset ^ 3) *^ co1
          + 3 * (invOffset ^ 2) * offset *^ hr1
          + 3 * invOffset * (offset ^ 2) *^ hl2
          + (offset ^ 3) *^ co2

calcTangentToBezier :: Double -> BezierPoint -> BezierPoint -> V3 Double
calcTangentToBezier offset startPoint endPoint =
  if offset < 0 || offset > 1 then error $ "Expected '0 < offset < 1' but found 'offset = "
                                   ++ show offset ++ "'.'"
  else let co1 = bpControl startPoint
           hr1 = bpHandleRight startPoint
           co2 = bpControl endPoint
           hl2 = bpHandleLeft endPoint
           invOffset = 1 - offset 
       in 2 * (invOffset ^ 2) *^ (hr1 - co1)
          + 6 * invOffset * offset *^ (hl2 - hr1)
          + 3 * (offset ^ 2) *^ (co2 - hl2)

calcHelixPoints :: RandomGen g => Double -> Double
  -> MS g r (V3 Double, V3 Double, V3 Double, V3 Double) 
calcHelixPoints rad pitch = do
  spinAng <- getRandomR (0, 2 * pi)
  dir <- use $ #turtle % #turtleDir
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

calcShapeRatio :: PShape -> Double -> T g r Double
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
         else do Parameters {..} <- ask
                 if ratio < 1 - pPruneWidthPeak
                   then return $ (ratio / (1 - pPruneWidthPeak)) ** pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - pPruneWidthPeak)) ** pPrunePowerLow
    Conical -> return $ 0.2 + 0.8 * ratio
      
getParent :: Stem -> T g r Stem
getParent stem = do
  stems <- use #tStems
  let Just parentIdx = stem^. #sParent
  return $ stems V.! parentIdx

calcStemLength :: RandomGen g => Stem -> T g r Double
calcStemLength stem = do
  Parameters {..} <- ask
  result <- callCC $ \ break -> do
    when (stem^. #sDepth == 0) $ do
      r <- getRandomR (-1, 1)
      treeScale <- use #tTreeScale
      res <- #tTrunkLength <.= (treeScale * ((pLength V.! 0) + r * (pLengthV V.! 0)))
      break res
    when (stem^. #sDepth == 1) $ do
      parent <- getParent stem
      baseLength <- use #tBaseLength
      shapeRatio <- calcShapeRatio pShape $
        (parent^. #sLength - stem^. #sOffset) / (parent^. #sLength - baseLength)
      break $ (parent^. #sLength) * (parent^. #sLengthChildMax) * shapeRatio
    parent <- getParent stem
    return $ (parent^. #sLengthChildMax) * (parent^. #sLength - 0.7 * stem^. #sOffset)
  return $ max 0 result

calcStemRadius :: Stem -> T g r Double
calcStemRadius stem = do
  Parameters {..} <- ask
  let d = stem^. #sDepth
  if d == 0
    then return $ (stem^. #sLength) * pRatio * (pRadiusMod V.! 0)
    else do parent <- getParent stem
            return $ min (stem^. #sRadiusLimit) $ max 0.005 $
              (pRadiusMod V.! d) * (parent^. #sRadius) *
              ((stem^. #sLength / parent^. #sLength) ** pRatioPower)

calcCurveAngle :: RandomGen g => Int -> Int -> MS g r Double
calcCurveAngle depth segInd = do
  Parameters {..} <- ask
  let curve = pCurve V.! depth
      curveBack = pCurveBack V.! depth
      curveRes = fromIntegral $ pCurveRes V.! depth
      curveV = pCurveV V.! depth
  curveAngle <- callCC $ \ break -> do
    when (curveBack == 0) $ do
      break $ curve / curveRes
    when (fromIntegral segInd < curveRes / 2) $ do
      break $ curve / (curveRes / 2)
    return $ curveBack / (curveRes / 2)
  r <- getRandomR (-1, 1)
  return $ curveAngle + r * (curveV / curveRes)
