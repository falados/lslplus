module Lsl.Physics(
                   bbIntersect,
                   kin,
                   primMassApprox,
                   checkIntersections
                   )where

import Lsl.Math
import Lsl.WorldDef
import Lsl.Constants

rangeOverlap (mn0,mx0) (mn1,mx1) = mn0 >= mn1 && mn0 <= mx1 || mx0 >= mn1 && mx0 <= mx1 || mn0 <= mn1 && mx0 >= mx1
bbIntersect ((mnX0,mnY0,mnZ0),(mxX0,mxY0,mxZ0)) ((mnX1,mnY1,mnZ1),(mxX1,mxY1,mxZ1)) =
   rangeOverlap (mnX0,mxX0) (mnX1,mxX1) &&
   rangeOverlap (mnY0,mxY0) (mnY1,mxY1) &&
   rangeOverlap (mnZ0,mxZ0) (mnZ1,mxZ1)

-- really poor mass approximations, especially when tapering, shearing is involved, or non-box/cyl/spheres are used
boxMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * x * y * z * c1 * c2 * h
cylMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * pi * (x/2) * (y/2) * z * c1 * c2 * h
sphMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * (4/3) * pi * (x/2) * (y/2) * (z/2) * c1 * c2 * h
otherMassSimple :: Floating a => (a,a,a) -> a -> a -> a -> a
otherMassSimple = sphMassSimple

-- volume of a torus = (pi * r^2) * (2 * pi * R) = (x-section-area) * length
-- perimeter of an ellipse = pi * (a + b) * (1 + 3 * h / (10 + sqrt (4 - 3 * h))) where h = (a - b)^2 / (a + b)^2

primMassApprox :: Prim -> Float
primMassApprox (Prim { primScale = scale, primTypeInfo = primType }) = primMassApprox' scale primType

primMassApprox' :: (Float,Float,Float) -> PrimType -> Float
primMassApprox' scale primType =
    case primType of
       PrimTypeUnknown -> otherMassSimple scale 1 1 1
       PrimType { primTypeCode = code } ->
           case code of
              c | c == cPrimTypeBox -> boxMassSimple scale c1 c2 h
                | c == cPrimTypeCylinder -> cylMassSimple scale c1 c2 h
                | c == cPrimTypeSphere -> sphMassSimple scale c1 c2 h
                | c == cPrimTypeSculpt -> otherMassSimple scale 1 1 1
                | otherwise -> otherMassSimple scale c1 c2 h
           where c1 = let (x,y,_) = primCut primType in y - x
                 c2 = let (x,y,_) = primAdvancedCut primType in y - x
                 h = 1 - primHollow primType

kin t0 t1 t2d zoffs m p0 v0 f (i,ti) =
   let gravA = (0,0,-9.80665) -- gravitational acceleration
       accel = gravA `add3d` scale3d (1/m) f
       accel1 = scale3d (1/m) i
       belowGround (_,_,z) = z + zoffs < 0
       limit (p@(x,y,z),v@(vx,vy,vz)) = if belowGround p then ((x,y,0),(vx,vy, if vz < 0 then 0 else vz)) else (p,v)
       d = t2d $ t1 - t0
       di = max 0 (min (t2d (ti - t0)) d)
   in limit (p0 `add3d` (scale3d d v0) `add3d` (scale3d (d^2 / 2) accel) `add3d` (scale3d (di^2/2) accel1), 
             v0 `add3d` (scale3d d accel) `add3d` (scale3d di accel1))
             
checkIntersections toBB cmp objects = concat (go objects)
    where go [] = []
          go (o:os) = [ pair o o' | (True,o') <- zip (map (bbIntersect (toBB o) . toBB ) os) os] : (go os)
          pair o o' = if cmp o o' == LT then (o,o') else (o',o)