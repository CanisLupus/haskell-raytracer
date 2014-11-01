module Types where

import Control.DeepSeq.Generics
import Data.Vect.Float.Base

----------------------------------------------------------------

data Attenuation = Attenuation Float Float Float -- const, linear, quadratic
	deriving Show

data Material = Material Color Color Color Float -- ke, kd, ks, shininess
	deriving Show

----------------------------------------------------------------

data Color = Color !Float !Float !Float -- r g b

instance Num Color where
	(*) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)
	(+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
	(-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
	(negate) (Color r g b) = error "you can't negate!"
	(abs) (Color r g b) = error "you can't abs!"
	(signum) (Color r g b) = error "you can't signum!"
	(fromInteger) r = error "you can't fromInteger!"

instance Eq Color where
	(==) (Color r1 g1 b1) (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b2 == b2

instance Show Color where
	show (Color r g b) = show (floor (r*255)) ++ ' ' : show (floor (g*255)) ++ ' ' : show (floor (b*255))

instance NFData Color where
	rnf a = a `seq` ()

multNumByColor :: Float -> Color -> Color
multNumByColor n (Color r g b) = Color (r*n) (g*n) (b*n)

----------------------------------------------------------------

reflect :: Vec3 -> Vec3 -> Vec3
reflect v normal = v &- (2 * dotprod normal v) *& normal

-- cross product for Vec4 that ignores w (sets to zero)
crossprod4 :: Vec4 -> Vec4 -> Vec4
crossprod4 (Vec4 x1 y1 z1 _) (Vec4 x2 y2 z2 _) =
	Vec4 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2) 0

----------------------------------------------------------------

translationMatrix :: Vec3 -> Mat4
translationMatrix (Vec3 tx ty tz) =
	Mat4 (Vec4 1 0 0 tx)
		 (Vec4 0 1 0 ty)
		 (Vec4 0 0 1 tz)
		 (Vec4 0 0 0 1 )
--translationMatrix v = transpose $ fromProjective $ translation v

scalingMatrix :: Vec3 -> Mat4
scalingMatrix (Vec3 sx sy sz) =
	Mat4 (Vec4 sx  0  0  0)
		 (Vec4  0 sy  0  0)
		 (Vec4  0  0 sz  0)
		 (Vec4  0  0  0  1)
--scalingMatrix v = fromProjective $ scaling v

rotationMatrix :: Float -> Vec3 -> Mat4
rotationMatrix degrees v@(Vec3 x y z)
	= extendWith 1 $ (cos rads *& one) &+ ((1 - cos rads) *& m1) &+ (sin rads *& m2)
	where
		rads = degrees * pi / 180
		m1 = outer v v
		m2 = Mat3 (Vec3   0 (-z)  y )
				  (Vec3   z   0 (-x))
				  (Vec3 (-y)  x   0 )

-- converted from C++: http://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix
inverse :: Mat4 -> Mat4
inverse (Mat4 (Vec4 m0 m1 m2 m3) (Vec4 m4 m5 m6 m7) (Vec4 m8 m9 m10 m11) (Vec4 m12 m13 m14 m15))
	= det *& Mat4 (Vec4 i0 i1 i2 i3) (Vec4 i4 i5 i6 i7) (Vec4 i8 i9 i10 i11) (Vec4 i12 i13 i14 i15)
	where
		i0  =  m5 * m10 * m15 -  m5 * m11 * m14 -  m9 * m6 * m15 +  m9 * m7 * m14 +  m13 * m6 * m11 -  m13 * m7 * m10
		i4  = -m4 * m10 * m15 +  m4 * m11 * m14 +  m8 * m6 * m15 -  m8 * m7 * m14 -  m12 * m6 * m11 +  m12 * m7 * m10
		i8  =  m4 * m9 * m15 -   m4 * m11 * m13 -  m8 * m5 * m15 +  m8 * m7 * m13 +  m12 * m5 * m11 -  m12 * m7 * m9
		i12 = -m4 * m9 * m14 +   m4 * m10 * m13 +  m8 * m5 * m14 -  m8 * m6 * m13 -  m12 * m5 * m10 +  m12 * m6 * m9
		i1  = -m1 * m10 * m15 +  m1 * m11 * m14 +  m9 * m2 * m15 -  m9 * m3 * m14 -  m13 * m2 * m11 +  m13 * m3 * m10
		i5  =  m0 * m10 * m15 -  m0 * m11 * m14 -  m8 * m2 * m15 +  m8 * m3 * m14 +  m12 * m2 * m11 -  m12 * m3 * m10
		i9  = -m0 * m9 * m15 +   m0 * m11 * m13 +  m8 * m1 * m15 -  m8 * m3 * m13 -  m12 * m1 * m11 +  m12 * m3 * m9
		i13 =  m0 * m9 * m14 -   m0 * m10 * m13 -  m8 * m1 * m14 +  m8 * m2 * m13 +  m12 * m1 * m10 -  m12 * m2 * m9
		i2  =  m1 * m6 * m15 -   m1 * m7 * m14 -   m5 * m2 * m15 +  m5 * m3 * m14 +  m13 * m2 * m7 -   m13 * m3 * m6
		i6  = -m0 * m6 * m15 +   m0 * m7 * m14 +   m4 * m2 * m15 -  m4 * m3 * m14 -  m12 * m2 * m7 +   m12 * m3 * m6
		i10 =  m0 * m5 * m15 -   m0 * m7 * m13 -   m4 * m1 * m15 +  m4 * m3 * m13 +  m12 * m1 * m7 -   m12 * m3 * m5
		i14 = -m0 * m5 * m14 +   m0 * m6 * m13 +   m4 * m1 * m14 -  m4 * m2 * m13 -  m12 * m1 * m6 +   m12 * m2 * m5
		i3  = -m1 * m6 * m11 +   m1 * m7 * m10 +   m5 * m2 * m11 -  m5 * m3 * m10 -  m9 * m2 * m7 +    m9 * m3 * m6
		i7  =  m0 * m6 * m11 -   m0 * m7 * m10 -   m4 * m2 * m11 +  m4 * m3 * m10 +  m8 * m2 * m7 -    m8 * m3 * m6
		i11 = -m0 * m5 * m11 +   m0 * m7 * m9 +    m4 * m1 * m11 -  m4 * m3 * m9 -   m8 * m1 * m7 +    m8 * m3 * m5
		i15 =  m0 * m5 * m10 -   m0 * m6 * m9 -    m4 * m1 * m10 +  m4 * m2 * m9 +   m8 * m1 * m6 -    m8 * m2 * m5

		invDet = m0 * i0 + m1 * i4 + m2 * i8 + m3 * i12
		det = 1.0 / invDet

----------------------------------------------------------------
