module Scene where

import Data.Vect.Float.Base
import Debug.Trace

import Lights
import Surfaces
import Types

data SceneConfig = SceneConfig
	{ height :: Int
	, width :: Int
	, maxDepth :: Int
	, output :: FilePath
	, camera :: Camera
	, ambient :: Color
	, surfaces :: [Surface]
	, lights :: [Light]
	} deriving Show

data Camera = Camera Vec3 Float Float Vec3 Vec3 Vec3
	deriving Show

buildCamera :: Vec3 -> Vec3 -> Vec3 -> Float -> Int -> Int -> Camera
buildCamera lookFrom lookAt up degrees width height
	= Camera lookFrom fovy fovx axisX axisY axisZ
	where
		fovy = degrees * pi / 180.0
		fovx = getFovx fovy width height
		axisZ = normalize $ lookFrom &- lookAt
		axisX = normalize $ crossprod (normalize up) axisZ
		axisY = crossprod axisZ axisX

updateCamera :: Camera -> Int -> Int -> Camera
updateCamera (Camera lookFrom fovy _ axisX axisY axisZ) width height
	= Camera lookFrom fovy (getFovx fovy width height) axisX axisY axisZ

getFovx :: Float -> Int -> Int -> Float
getFovx fovy width height = 2 * atan (tan (fovy/2) * (fromIntegral width) / (fromIntegral height))

eps = 0.0001::Float
shadowColor = Color 0 0 0
bgColor = Color 0 0 0
