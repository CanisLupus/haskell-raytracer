{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Raytrace where

import Control.Parallel.Strategies
import Data.Maybe
import Data.Vect.Float.Base
import Data.Ord (comparing)
import Data.List (minimumBy)

import Lights
import Scene
import Surfaces
import Types

raytraceImage :: SceneConfig -> [Color]
raytraceImage scene@SceneConfig { width, height }
	= [raycastFromScreen scene row col | row <- [0..height-1], col <- [0..width-1]]
		`using` parListChunk 1000 rseq

raycastFromScreen :: SceneConfig -> Int -> Int -> Color
raycastFromScreen scene@SceneConfig { width, height, camera } row col
	= raycast scene lookFrom vectorThroughPixel 0
	where
		Camera lookFrom fovy fovx axisX axisY axisZ = camera
		halfImageWidth = (fromIntegral width) / 2
		halfImageHeight = (fromIntegral height) / 2
		alpha = tan (fovx/2) * ((fromIntegral col) - halfImageWidth + 0.5)  / halfImageWidth
		beta  = tan (fovy/2) * (halfImageHeight - (fromIntegral row) - 0.5) / halfImageHeight
		vectorThroughPixel = normalize $ (alpha *& axisX) &- (beta *& axisY) &- axisZ

raycast :: SceneConfig -> Vec3 -> Vec3 -> Int -> Color
raycast scene@SceneConfig { ambient, maxDepth, surfaces, lights } rayOrig rayDir depth
	| depth > maxDepth = bgColor
	| null intersections = bgColor
	| otherwise = ambient + ke + lightingColor + reflectionColor
	where
		intersections = surfaceIntersections surfaces rayOrig rayDir
		(hitPoint, hitPoint_os, surface) = closestSurfaceIntersection intersections rayOrig

		normal_os = getNormalAt hitPoint_os surface
		normal = normalize $ trim $ (transposedInverseTransformOf surface) *. normal_os

		Material ke _ ks _ = materialOf surface
		reflectedVector = normalize $ reflect rayDir normal
		reflectionColor
			| ks == Color 0 0 0 = Color 0 0 0
			| otherwise = ks * raycast scene (hitPoint &+ eps *& reflectedVector) reflectedVector (depth+1)

		toCamera = normalize $ rayOrig &- hitPoint
		lightingColor = foldl1 (+) [getLightingColorOrShadow surfaces light surface hitPoint normal toCamera | light <- lights]

surfaceIntersections :: [Surface] -> Vec3 -> Vec3 -> [(Vec3, Vec4, Surface)]
surfaceIntersections surfaces rayOrig rayDir
	= transformIntersections [(fromJust hit, obj) | obj <- surfaces, let hit = intersectInObjectSpace rayOrig rayDir obj, isJust hit]

intersectInObjectSpace :: Vec3 -> Vec3 -> Surface -> Maybe Vec4
intersectInObjectSpace rayOrig rayDir obj
	= intersect rayOrig_os rayDir_os obj
	where
		invTransform = inverseTransformOf obj
		rayOrig_os = invTransform *. (extendWith 1 rayOrig)
		rayDir_os  = normalize $ invTransform *. (extendZero rayDir)

transformIntersections :: [(Vec4, Surface)] -> [(Vec3, Vec4, Surface)]
transformIntersections intersections_os =
	[(trim $ transformOf surface *. hitPoint_os, hitPoint_os, surface) | (hitPoint_os, surface) <- intersections_os]

closestSurfaceIntersection :: [(Vec3, Vec4, Surface)] -> Vec3 -> (Vec3, Vec4, Surface)
closestSurfaceIntersection intersections point = minimumBy (comparing $ normsqr . ((&-) point) . firstOf3) intersections
	where firstOf3 (x, _, _) = x

getLightingColorOrShadow :: [Surface] -> Light -> Surface -> Vec3 -> Vec3 -> Vec3 -> Color
getLightingColorOrShadow surfaces light surface point normal toCamera
	| isShadowed surfaces point toLight toLightSqrDistance = shadowColor
	| otherwise = getLightingColor light (materialOf surface) normal toCamera toLight toLightSqrDistance
	where
		toLight = getDirectionFrom light point
		toLightSqrDistance = getSqrDistanceFrom light point

getLightingColor :: Light -> Material -> Vec3 -> Vec3 -> Vec3 -> Float -> Color
getLightingColor light (Material _ kd ks shin) normal toCamera toLight toLightSqrDistance
	| ln > 0 = multNumByColor (1 / attenuation) $ colorOf light * (lambert + phong)
	| otherwise = Color 0 0 0
	where
		ln = dotprod toLight normal
		lambert = multNumByColor ln kd
		h = normalize $ toLight &+ toCamera
		phong = multNumByColor ((max (dotprod normal h) 0) ** shin) ks
		attenuation = getAttenuationAtDistance light (sqrt toLightSqrDistance)

isShadowed :: [Surface] -> Vec3 -> Vec3 -> Float -> Bool
isShadowed surfaces point toLight toLightSqrDistance
	= any (\(hitPoint, _, _) -> normsqr (hitPoint &- point) < toLightSqrDistance) intersections
	where
		intersections = surfaceIntersections surfaces (point &+ eps *& toLight) toLight
