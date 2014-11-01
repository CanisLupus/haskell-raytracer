module Surfaces where

import Data.Maybe
import Data.Vect.Float.Base hiding (inverse)

import Types

data Surface
	= Plane {pos_::Vec4, normal_::Vec4, materialOf::Material, transformOf::Mat4, inverseTransformOf::Mat4, transposedInverseTransformOf::Mat4}
	| Triangle {p1_::Vec4, p2_::Vec4, p3_::Vec4, normal_::Vec4, materialOf::Material, transformOf::Mat4, inverseTransformOf::Mat4, transposedInverseTransformOf::Mat4}
	| Sphere {pos_::Vec4, radius_::Float, materialOf::Material, transformOf::Mat4, inverseTransformOf::Mat4, transposedInverseTransformOf::Mat4}
	deriving Show

addTransforms :: Mat4 -> (Mat4 -> Mat4 -> Mat4 -> Surface) -> Surface
addTransforms transform surfacePrototype = surfacePrototype transform inverseTransform transposedInverseTransform
	where
		inverseTransform = inverse transform
		transposedInverseTransform = transpose inverseTransform

buildSphere :: Vec3 -> Float -> Material -> Mat4 -> Surface
buildSphere pos radius mat transform
	= addTransforms transform $ Sphere (extendWith 1 pos) radius mat

buildTriangle :: Vec3 -> Vec3 -> Vec3 -> Material -> Mat4 -> Surface
buildTriangle p1 p2 p3 mat transform
	= addTransforms transform $ Triangle (extendWith 1 p1) (extendWith 1 p2) (extendWith 1 p3) normal mat
	where normal = extendZero $ normalize $ crossprod (p3 &- p2) (p1 &- p2)

buildPlane :: Vec3 -> Vec3 -> Material -> Mat4 -> Surface
buildPlane pos normal mat transform
	= addTransforms transform $ Plane (extendWith 1 pos) (extendZero $ normalize normal) mat

intersect :: Vec4 -> Vec4 -> Surface -> Maybe Vec4
intersect rayOrig rayDir (Plane pos normal _ _ _ _) = intersectPlane rayOrig rayDir pos normal
intersect rayOrig rayDir (Triangle p1 p2 p3 normal _ _ _ _) = intersectConvexPolygon rayOrig rayDir [p1, p2, p3] normal
intersect o d (Sphere p r _ _ _ _)
	| det < 0 = Nothing
	| t0 > eps = Just (o &+ t0 *& d)
	| t1 > eps = Just (o &+ t1 *& d)
	| otherwise = Nothing
	where
		op = p &- o
		eps = 1e-3
		b = dotprod op d
		det = b*b - (dotprod op op) + r*r
		sdet = sqrt det
		t0 = b-sdet
		t1 = b+sdet

intersectPlane :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> Maybe Vec4
intersectPlane rayOrig rayDir pos normal
	| t <= 0 = Nothing							-- avoid collisions with objects behind ray starting point
	| otherwise = Just (rayOrig &+ t *& rayDir)		-- determine collision point
	where
		  t = dotprod normal (pos &- rayOrig) / (dotprod normal rayDir)

intersectConvexPolygon :: Vec4 -> Vec4 -> [Vec4] -> Vec4 -> Maybe Vec4
intersectConvexPolygon rayOrig rayDir vertexes normal
	| isJust point && allequal = point
	| otherwise = Nothing
	where
		point = intersectPlane rayOrig rayDir (head vertexes) normal
		vertexPairs = zip vertexes (tail $ cycle vertexes)
		results = map (\(p1, p2) -> isInside (fromJust point) rayOrig p1 p2) vertexPairs
		allequal = all (== head results) (tail results)

isInside :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> Bool
isInside point origin p1 p2 = dotprod point normal + d >= 0
	where
		normal = normalize $ crossprod4 (p2 &- origin) (p1 &- origin)
		d = dotprod (neg origin) normal

getNormalAt :: Vec4 -> Surface -> Vec4
getNormalAt _ (Plane _ n _ _ _ _) = n
getNormalAt _ (Triangle _ _ _ n _ _ _ _) = n
getNormalAt point (Sphere pos _ _ _ _ _) = normalize $ point &- pos
