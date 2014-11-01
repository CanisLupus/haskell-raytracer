module Lights where

import Data.Vect.Float.Base

import Types

data Light
	= PointLight Vec3 Color Attenuation
	| DirectionalLight Vec3 Color
	deriving Show

colorOf :: Light -> Color
colorOf (PointLight _ color _) = color
colorOf (DirectionalLight _ color) = color

buildPointLight :: Vec3 -> Color -> Attenuation -> Light
buildPointLight pos color attenuation = PointLight pos color attenuation

buildDirectionalLight :: Vec3 -> Color -> Light
buildDirectionalLight dir color = DirectionalLight (normalize dir) color

getDirectionFrom :: Light -> Vec3 -> Vec3
getDirectionFrom (PointLight pos _ _) point = normalize $ pos &- point
getDirectionFrom (DirectionalLight direction _) _ = direction	-- TODO: why negate?

getSqrDistanceFrom :: Light -> Vec3 -> Float
getSqrDistanceFrom (PointLight pos _ _) point = normsqr $ pos &- point
getSqrDistanceFrom (DirectionalLight _ _) _ = 1.0 / 0

getAttenuationAtDistance :: Light -> Float -> Float
getAttenuationAtDistance (PointLight _ _ (Attenuation const lin quad)) distance =
	const + lin * distance + quad * distance * distance
getAttenuationAtDistance (DirectionalLight _ _) _ = 1.0
